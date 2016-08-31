#include "impala/ast.h"

#include "thorin/irbuilder.h"
#include "thorin/continuation.h"
#include "thorin/primop.h"
#include "thorin/type.h"
#include "thorin/util/array.h"
#include "thorin/util/push.h"
#include "thorin/world.h"

namespace impala {

using thorin::Continuation;
using thorin::JumpTarget;
using thorin::Location;
using thorin::Value;
using thorin::World;

class CodeGen : public thorin::IRBuilder {
public:
    CodeGen(World& world)
        : IRBuilder(world)
    {}

    const thorin::Def* frame() const { assert(cur_fn); return cur_fn->frame(); }
    /// Enter \p x and perform \p get_value to collect return value.
    const thorin::Def* converge(const Expr* expr, JumpTarget& x) {
        emit_jump(expr, x);
        if (enter(x))
            return cur_bb->get_value(1, convert(expr->type()));
        return nullptr;
    }

    void emit_jump_boolean(bool val, JumpTarget& x, const thorin::Location& loc) {
        if (is_reachable()) {
            cur_bb->set_value(1, world().literal(val, loc));
            jump(x, loc);
        }
    }

    Continuation* create_continuation(const LocalDecl* decl) {
        auto result = continuation(convert(decl->type())->as<thorin::FnType>(), decl->loc(), decl->symbol().str());
        result->param(0)->name_ = "mem";
        decl->value_ = Value::create_val(*this, result);
        return result;
    }

    void set_continuation(Continuation* continuation) {
        cur_bb = continuation;
        set_mem(continuation->param(0));
    }

    void jump_to_continuation(Continuation* continuation, const thorin::Location& loc) {
        if (is_reachable())
            cur_bb->jump(continuation, {get_mem()}, loc);
        set_continuation(continuation);
    }

    Value lemit(const Expr* expr) { return expr->lemit(*this); }
    const thorin::Def* remit(const Expr* expr) { return expr->remit(*this); }
    const thorin::Def* remit(const Expr* expr, MapExpr::State state, Location eval_loc) {
        return expr->as<MapExpr>()->remit(*this, state, eval_loc);
    }
    void emit_jump(const Expr* expr, JumpTarget& x) { if (is_reachable()) expr->emit_jump(*this, x); }
    void emit_branch(const Expr* expr, JumpTarget& t, JumpTarget& f) { expr->emit_branch(*this, t, f); }
    void emit(const Stmt* stmt) { if (is_reachable()) stmt->emit(*this); }
    void emit(const Item* item) {
        assert(!item->done_);
        item->emit_item(*this);
#ifndef NDEBUG
        item->done_ = true;
#endif
    }
    void emit(const Ptrn* ptrn, const thorin::Def* def) { ptrn->emit(*this, def); }
    Value emit(const ValueDecl* decl) {
        assert(decl->value_.tag() != thorin::Value::Empty);
        return decl->value_;
    }
    Value emit(const ValueDecl* decl, const thorin::Def* init) {
        if (!decl->value_)
            decl->value_ = decl->emit(*this, init);
        return decl->value_;
    }
    const thorin::Def* convert(const Def* type) {
        if (thorin_type(type) == nullptr)
            thorin_type(type) = convert_rec(type);
        return thorin_type(type);
    }

    Array<const thorin::Def*> convert_ops(const Def*);
    const thorin::Def* convert_rec(const Def*);

    const thorin::Def*& thorin_type(const Def* type) { return impala2thorin_[type]; }

    const Fn* cur_fn = nullptr;
    DefMap<const thorin::Def*> impala2thorin_;
};

/*
 * Type
 */

const thorin::Def* CodeGen::convert_rec(const Def* type) {
    if (auto lambda = type->isa<Lambda>()) {
        return world().lambda(world().star(), convert(lambda->body()), lambda->loc(), lambda->name());
    } else if (auto var = type->isa<Var>()) {
        return world().var(world().star(), var->depth(), var->loc(), var->name());
    } else if (auto prim_type = type->isa<PrimType>()) {
        switch (prim_type->primtype_tag()) {
#define IMPALA_TYPE(itype, ttype) \
            case PrimType_##itype: return world().type_##ttype();
#include "impala/tokenlist.h"
            default: THORIN_UNREACHABLE;
        }
    } else if (auto fn_type = type->isa<FnType>()) {
        Array<const thorin::Def*> nops(fn_type->num_ops()+1);
        nops.front() = world().mem_type();
        for (size_t i = 1, e = nops.size(); i != e; ++i)
            nops[i] = convert(fn_type->op(i-1));
        return world().fn_type(nops);
    } else if (auto sigma = type->isa<Sigma>()) {
        thorin::Sigma* thorin_sigma = nullptr;
        if (sigma->is_nominal())
            impala2thorin_[sigma] = thorin_sigma = world().sigma(sigma->num_ops(), sigma->loc(), sigma->name());

        Array<const thorin::Def*> nops(sigma->num_ops());
        for (size_t i = 0, e = nops.size(); i != e; ++i)
            nops[i] = convert(sigma->op(i));

        if (sigma->is_nominal()) {
            thorin_sigma->set(nops);
            return thorin_sigma;
        }

        return world().sigma(nops, sigma->loc(), sigma->name());
    } else if (auto ptr_type = type->isa<PtrType>()) {
        return world().ptr_type(convert(ptr_type->referenced_type()), 1, -1, thorin::AddrSpace(ptr_type->addr_space()));
    } else if (auto definite_array_type = type->isa<DefiniteArrayType>()) {
        return world().definite_array_type(convert(definite_array_type->elem_type()), definite_array_type->dim());
    } else if (auto indefinite_array_type = type->isa<IndefiniteArrayType>()) {
        return world().indefinite_array_type(convert(indefinite_array_type->elem_type()));
    } else if (auto simd_type = type->isa<SimdType>()) {
        return world().type(convert(simd_type->elem_type())->as<thorin::PrimType>()->primtype_tag(), simd_type->dim());
    }
    THORIN_UNREACHABLE;
}

/*
 * Decls and Function
 */

Value LocalDecl::emit(CodeGen& cg, const thorin::Def* init) const {
    auto thorin_type = cg.convert(type());

    if (is_mut()) {
        if (is_address_taken())
            value_ = Value::create_ptr(cg, cg.world().slot(thorin_type, cg.frame(), loc(), symbol().str()));
        else
            value_ = Value::create_mut(cg, handle(), thorin_type, symbol().str());

        if (init)
            value_.store(init, loc());
    } else
        value_ = Value::create_val(cg, init);

    return value_;
}

Continuation* Fn::emit_head(CodeGen& cg, const Location& loc) const {
    return continuation_ = cg.continuation(cg.convert(fn_type())->as<thorin::FnType>(), loc, fn_symbol().remove_quotation());
}

void Fn::emit_body(CodeGen& cg, const Location& loc) const {
    // setup function nest
    continuation()->set_parent(cg.cur_bb);
    THORIN_PUSH(cg.cur_fn, this);
    THORIN_PUSH(cg.cur_bb, continuation());

    // setup memory + frame
    size_t i = 0;
    auto mem_param = continuation()->param(i++);
    mem_param->name_ = "mem";
    cg.set_mem(mem_param);
    frame_ = cg.create_frame(loc);

    // name params and setup store locations
    for (auto param : params()) {
        auto p = continuation()->param(i++);
        p->name_ = param->symbol().str();
        cg.emit(param, p);
    }
    assert(i == continuation()->num_params());
    if (continuation()->num_params() != 0 && continuation()->params().back()->type()->isa<thorin::FnType>())
        ret_param_ = continuation()->params().back();

    // descend into body
    auto def = cg.remit(body());
    if (def) {
        auto mem = cg.get_mem();

        if (auto sigma = def->type()->isa<thorin::Sigma>()) {
            std::vector<const thorin::Def*> args;
            args.push_back(mem);
            for (size_t i = 0, e = sigma->num_ops(); i != e; ++i)
                args.push_back(cg.extract(def, i, loc));
            cg.cur_bb->jump(ret_param(), args, loc.end());
        } else
            cg.cur_bb->jump(ret_param(), {mem, def}, loc.end());
    }
}

/*
 * items
 */

void ValueItem::emit_item(CodeGen& cg) const {
    cg.emit(static_cast<const ValueDecl*>(this), nullptr);
}

void ModContents::emit(CodeGen& cg) const {
    for (auto item : items())
        cg.emit(item);
}

static bool is_primop(const Symbol& name) {
    if      (name == "select")   return true;
    else if (name == "sizeof")   return true;
    else if (name == "bitcast")  return true;
    return false;
}

Value FnDecl::emit(CodeGen& cg, const thorin::Def*) const {
    // no code is emitted for primops
    if (is_extern() && abi() == "\"thorin\"" && is_primop(symbol()))
        return value_;

    // create thorin function
    value_ = Value::create_val(cg, emit_head(cg, loc()));
    if (is_extern() && abi() == "")
        continuation_->make_external();

    // handle main function
    if (symbol() == "main") {
        continuation()->make_external();
    }

    if (body())
        emit_body(cg, loc());
    return value_;
}

void ExternBlock::emit_item(CodeGen& cg) const {
    for (auto fn : fns()) {
        cg.emit(static_cast<const ValueDecl*>(fn), nullptr); // TODO use init
        auto continuation = fn->continuation();
        if (abi() == "\"C\"")
            continuation->cc() = thorin::CC::C;
        else if (abi() == "\"device\"")
            continuation->cc() = thorin::CC::Device;
        else if (abi() == "\"thorin\"" && continuation) // no continuation for primops
            continuation->set_intrinsic();
    }
}

void ModDecl::emit_item(CodeGen&) const {
}

void ImplItem::emit_item(CodeGen& cg) const {
    if (def_)
        return;

    Array<const thorin::Def*> args(num_methods());
    for (size_t i = 0, e = args.size(); i != e; ++i) {
        cg.emit(static_cast<const ValueDecl*>(method(i)), nullptr); // TODO use init
        args[i] = method(i)->continuation();
    }

    for (size_t i = 0, e = args.size(); i != e; ++i)
        method(i)->emit_body(cg, loc());

    def_ = cg.world().tuple(args, loc());
}

Value StaticItem::emit(CodeGen& cg, const thorin::Def* init) const {
    assert(!init);
    init = !this->init() ? cg.world().bottom(cg.convert(type()), loc()) : cg.remit(this->init());
    if (!is_mut())
        return Value::create_val(cg, init);
    return Value::create_ptr(cg, cg.world().global(init, loc(), true, symbol().str()));
}

void StructDecl::emit_item(CodeGen& cg) const {
    cg.convert(type());
}

void TraitDecl::emit_item(CodeGen&) const {}
void Typedef::emit_item(CodeGen&) const {}

/*
 * expressions
 */

Value Expr::lemit(CodeGen&) const { THORIN_UNREACHABLE; }
const thorin::Def* Expr::remit(CodeGen& cg) const { return lemit(cg).load(loc()); }
void Expr::emit_jump(CodeGen& cg, JumpTarget& x) const {
    if (auto def = cg.remit(this)) {
        assert(cg.is_reachable());
        cg.cur_bb->set_value(1, def);
        cg.jump(x, loc().end());
    } else
        assert(!cg.is_reachable());
}
void Expr::emit_branch(CodeGen& cg, JumpTarget& t, JumpTarget& f) const { cg.branch(cg.remit(this), t, f, loc().end()); }
const thorin::Def* EmptyExpr::remit(CodeGen& cg) const { return cg.world().tuple({}, loc()); }

const thorin::Def* LiteralExpr::remit(CodeGen& cg) const {
    thorin::PrimTypeTag ttag;

    switch (tag()) {
#define IMPALA_LIT(itype, ttype) \
        case LIT_##itype: ttag = thorin::PrimType_##ttype; break;
#include "impala/tokenlist.h"
        case LIT_bool: ttag = thorin::PrimType_bool; break;
        default: THORIN_UNREACHABLE;
    }

    return cg.world().literal(ttag, box(), loc());
}

const thorin::Def* CharExpr::remit(CodeGen& cg) const {
    return cg.world().literal_pu8(value(), loc());
}

const thorin::Def* StrExpr::remit(CodeGen& cg) const {
    Array<const thorin::Def*> args(values_.size());
    for (size_t i = 0, e = args.size(); i != e; ++i)
        args[i] = cg.world().literal_pu8(values_[i], loc());

    return cg.world().definite_array(args, loc());;
}

const thorin::Def* CastExpr::remit(CodeGen& cg) const {
    auto def = cg.remit(lhs());
    auto thorin_type = cg.convert(type());
    return cg.world().convert(thorin_type, def, loc());
}

Value PathExpr::lemit(CodeGen& cg) const {
    return cg.emit(value_decl(), nullptr);
}

const thorin::Def* PrefixExpr::remit(CodeGen& cg) const {
    switch (tag()) {
        case INC:
        case DEC: {
            auto var = cg.lemit(rhs());
            const thorin::Def* def = var.load(loc());
            const thorin::Def* one = cg.world().one(def->type(), loc());
            const thorin::Def* ndef = cg.world().arithop(Token::to_arithop((TokenTag) tag()), def, one, loc());
            var.store(ndef, loc());
            return ndef;
        }
        case ADD: return cg.remit(rhs());
        case SUB: return cg.world().arithop_minus(cg.remit(rhs()), loc());
        case NOT: return cg.world().arithop_not(cg.remit(rhs()), loc());
        case TILDE: {
            auto def = cg.remit(rhs());
            auto ptr = cg.alloc(def->type(), rhs()->extra(), loc());
            cg.store(ptr, def, loc());
            return ptr;
        }
        case AND: {
            if (rhs()->is_lvalue()) {
                auto var = cg.lemit(rhs());
                assert(var.tag() == Value::PtrRef);
                return var.def();
            }

            auto def = cg.remit(rhs());
            if (is_const(def))
                return cg.world().global(def, loc(), false);

            auto slot = cg.world().slot(cg.convert(rhs()->type()), cg.frame(), loc());
            cg.store(slot, def, loc());
            return slot;

        }

        case RUN: return cg.remit(rhs(), MapExpr::Run, loc());
        case HLT: return cg.remit(rhs(), MapExpr::Hlt, loc());
        default:  return cg.lemit(this).load(loc());
    }
}

Value PrefixExpr::lemit(CodeGen& cg) const {
    if (tag() == MUL)
        return Value::create_ptr(cg, cg.remit(rhs()));
    THORIN_UNREACHABLE; // TODO
}

void PrefixExpr::emit_branch(CodeGen& cg, JumpTarget& t, JumpTarget& f) const {
    if (tag() == NOT && is_type_bool(cg.convert(type())))
        cg.emit_branch(rhs(), f, t);
    else
        cg.branch(cg.remit(this), t, f, loc().end());
}

void InfixExpr::emit_branch(CodeGen& cg, JumpTarget& t, JumpTarget& f) const {
    switch (tag()) {
        case ANDAND: {
            JumpTarget x(rhs()->loc().begin(), "and_extra");
            cg.emit_branch(lhs(), x, f);
            if (cg.enter(x))
                cg.emit_branch(rhs(), t, f);
            return;
        }
        case OROR: {
            JumpTarget x(rhs()->loc().begin(), "or_extra");
            cg.emit_branch(lhs(), t, x);
            if (cg.enter(x))
                cg.emit_branch(rhs(), t, f);
            return;
        }
        default:
            cg.branch(cg.remit(this), t, f, loc().end());
            return;
    }
}

const thorin::Def* InfixExpr::remit(CodeGen& cg) const {
    switch (tag()) {
        case ANDAND: {
            JumpTarget t(lhs()->loc().begin(), "and_true"), f(rhs()->loc().begin(), "and_false"), x(loc().end(), "and_exit");
            cg.emit_branch(lhs(), t, f);
            if (cg.enter(t)) cg.emit_jump(rhs(), x);
            if (cg.enter(f)) cg.emit_jump_boolean(false, x, lhs()->loc().end());
            return cg.converge(this, x);
        }
        case OROR: {
            JumpTarget t(lhs()->loc().begin(), "or_true"), f(rhs()->loc().begin(), "or_false"), x(loc().end(), "or_exit");
            cg.emit_branch(lhs(), t, f);
            if (cg.enter(t)) cg.emit_jump_boolean(true, x, rhs()->loc().end());
            if (cg.enter(f)) cg.emit_jump(rhs(), x);
            return cg.converge(this, x);
        }
        default:
            const TokenTag op = (TokenTag) tag();

            if (Token::is_assign(op)) {
                Value lvar = cg.lemit(lhs());
                const thorin::Def* rdef = cg.remit(rhs());

                if (op != Token::ASGN) {
                    TokenTag sop = Token::separate_assign(op);
                    rdef = cg.world().binop(Token::to_binop(sop), lvar.load(loc()), rdef, loc());
                }

                lvar.store(rdef, loc());
                return cg.world().tuple({}, loc());
            }

            const thorin::Def* ldef = cg.remit(lhs());
            const thorin::Def* rdef = cg.remit(rhs());
            return cg.world().binop(Token::to_binop(op), ldef, rdef, loc());
    }
}

const thorin::Def* PostfixExpr::remit(CodeGen& cg) const {
    Value var = cg.lemit(lhs());
    const thorin::Def* def = var.load(loc());
    const thorin::Def* one = cg.world().one(def->type(), loc());
    var.store(cg.world().arithop(Token::to_arithop((TokenTag) tag()), def, one, loc()), loc());
    return def;
}

const thorin::Def* DefiniteArrayExpr::remit(CodeGen& cg) const {
    Array<const thorin::Def*> thorin_args(num_args());
    for (size_t i = 0, e = num_args(); i != e; ++i)
        thorin_args[i] = cg.remit(arg(i));
    return cg.world().definite_array(cg.convert(type())->as<thorin::DefiniteArrayType>()->elem_type(), thorin_args, loc());
}

const thorin::Def* RepeatedDefiniteArrayExpr::remit(CodeGen& cg) const {
    Array<const thorin::Def*> args(count());
    std::fill_n(args.begin(), count(), cg.remit(value()));
    return cg.world().definite_array(args, loc());
}

const thorin::Def* TupleExpr::remit(CodeGen& cg) const {
    Array<const thorin::Def*> thorin_args(num_args());
    for (size_t i = 0, e = num_args(); i != e; ++i)
        thorin_args[i] = cg.remit(arg(i));
    return cg.world().tuple(thorin_args, loc());
}

const thorin::Def* IndefiniteArrayExpr::remit(CodeGen& cg) const {
    extra_ = cg.remit(dim());
    return cg.world().indefinite_array(cg.convert(type())->as<thorin::IndefiniteArrayType>()->elem_type(), extra_, loc());
}

const thorin::Def* SimdExpr::remit(CodeGen& cg) const {
    Array<const thorin::Def*> thorin_args(num_args());
    for (size_t i = 0, e = num_args(); i != e; ++i)
        thorin_args[i] = cg.remit(arg(i));
    return cg.world().vector(thorin_args, loc());
}

const thorin::Def* StructExpr::remit(CodeGen& cg) const {
    Array<const thorin::Def*> defs(num_elems());
    for (const auto& elem : elems())
        defs[elem.field_decl()->index()] = cg.remit(elem.expr());
    return cg.world().tuple(cg.convert(type())->as<thorin::Sigma>(), defs, loc());
}

Value TypeAppExpr::lemit(CodeGen&) const { THORIN_UNREACHABLE; }

const thorin::Def* TypeAppExpr::remit(CodeGen&) const {
    assert(false && "TODO");
    return nullptr;
}

Value MapExpr::lemit(CodeGen& cg) const {
    assert(lhs()->type()->isa<ArrayType>() || lhs()->type()->isa<Sigma>() || lhs()->type()->isa<SimdType>());
    auto agg = cg.lemit(lhs());
    return Value::create_agg(agg, cg.remit(arg(0)));
}

const thorin::Def* MapExpr::remit(CodeGen& cg) const { return remit(cg, None, Location()); }

const thorin::Def* MapExpr::remit(CodeGen& cg, State state, Location eval_loc) const {
    if (auto fn_type = lhs()->type()->isa<FnType>()) {
        // Handle primops here
        if (auto type_expr = lhs()->isa<TypeAppExpr>()) { // Bitcast, sizeof and select are all polymorphic
            if (auto path = type_expr->lhs()->isa<PathExpr>()) {
                if (auto fn_decl = path->value_decl()->isa<FnDecl>()) {
                    if (fn_decl->is_extern() && fn_decl->abi() == "\"thorin\"") {
                        auto name = fn_decl->symbol();
                        if (name == "bitcast") {
                            return cg.world().bitcast(cg.convert(type_expr->type_arg(0)), cg.remit(arg(0)), eval_loc);
                        } else if (name == "select") {
                            return cg.world().select(cg.remit(arg(0)), cg.remit(arg(1)), cg.remit(arg(2)), eval_loc);
                        } else if (name == "sizeof") {
                            return cg.world().size_of(cg.convert(type_expr->type_arg(0)), eval_loc);
                        }
                    }
                }
            }
        }

        auto dst = cg.remit(lhs());

        std::vector<const thorin::Def*> defs;
        defs.push_back(nullptr); // reserve for mem but set later - some other args may update the monad
        for (const auto& arg : args())
            defs.push_back(cg.remit(arg));
        defs.front() = cg.get_mem(); // now get the current memory monad

        auto ret_type = args().size() == fn_type->num_ops() ? nullptr : cg.convert(fn_type->return_type());
        auto old_bb = cg.cur_bb;
        auto ret = cg.call(dst, defs, ret_type, loc());
        if (ret_type)
            cg.set_mem(cg.cur_bb->param(0));

        if (state != None) {
            auto eval = state == Run ? &thorin::World::run : &thorin::World::hlt;
            auto cont = old_bb->args().back();
            old_bb->update_callee((cg.world().*eval)(old_bb->callee(), cont, eval_loc, ""));
        }

        return ret;
    } else if (lhs()->type()->isa<ArrayType>() || lhs()->type()->isa<Sigma>() || lhs()->type()->isa<SimdType>()) {
        auto index = cg.remit(arg(0));
        return cg.extract(cg.remit(lhs()), index, loc());
    }
    THORIN_UNREACHABLE;
}

Value FieldExpr::lemit(CodeGen& cg) const {
    return Value::create_agg(cg.lemit(lhs()), cg.world().literal_qu32(index(), loc()));
}

const thorin::Def* FieldExpr::remit(CodeGen& cg) const {
    return cg.extract(cg.remit(lhs()), index(), loc());
}

const thorin::Def* BlockExprBase::remit(CodeGen& cg) const {
    for (auto stmt : stmts())
        cg.emit(stmt);
    return cg.remit(expr());
}

const thorin::Def* RunBlockExpr::remit(CodeGen& cg) const {
    if (cg.is_reachable()) {
        World& w = cg.world();
        auto lrun = w.basicblock(loc(), "run_block");
        auto next = w.basicblock(loc(), "run_next");
        auto old_bb = cg.cur_bb;
//<<<<<<< HEAD
        //cg.cur_bb->jump(run, {cg.get_mem(), w.bottom(fn_mem, loc())}, loc());
        //cg.cur_bb = lrun;
        //cg.set_mem(cg.cur_bb->param(0));
        cg.cur_bb->jump(lrun, {}, loc());
        cg.enter(lrun);
        auto res = BlockExprBase::remit(cg);
        if (cg.is_reachable()) {
            assert(res);
//<<<<<<< HEAD
            //auto next = w.continuation(fn_mem, loc(), "run_next");
            //cg.cur_bb->jump(lrun->param(1), {cg.get_mem()}, loc());
            //old_bb->update_arg(1, next);
            //cg.cur_bb = next;
            //cg.set_mem(cg.cur_bb->param(0));
            //assert(res);
            cg.cur_bb->jump(next, {}, loc());
            cg.enter(next);
            old_bb->update_callee(w.run(lrun, next, loc()));
            return res;
        }

        old_bb->update_callee(w.run(lrun, w.bottom(w.fn_type(), loc()), loc()));
    }
    return nullptr;
}

void IfExpr::emit_jump(CodeGen& cg, JumpTarget& x) const {
    JumpTarget t(then_expr()->loc().begin(), "if_then"), f(else_expr()->loc().begin(), "if_else");
    cg.emit_branch(cond(), t, f);
    if (cg.enter(t))
        cg.emit_jump(then_expr(), x);
    if (cg.enter(f))
        cg.emit_jump(else_expr(), x);
    cg.jump(x, loc().end());
}

const thorin::Def* IfExpr::remit(CodeGen& cg) const {
    JumpTarget x(loc().end(), "next");
    return cg.converge(this, x);
}

const thorin::Def* WhileExpr::remit(CodeGen& cg) const {
    JumpTarget x(loc().end(), "next");
    auto break_continuation = cg.create_continuation(break_decl());

    cg.emit_jump(this, x);
    cg.jump_to_continuation(break_continuation, loc().end());
    return cg.world().tuple({}, loc());
}

void WhileExpr::emit_jump(CodeGen& cg, JumpTarget& exit_bb) const {
    JumpTarget head_bb(cond()->loc().begin(), "while_head"), body_bb(body()->loc().begin(), "while_body");
    auto continue_continuation = cg.create_continuation(continue_decl());

    cg.jump(head_bb, cond()->loc().end());
    cg.enter_unsealed(head_bb);
    cg.emit_branch(cond(), body_bb, exit_bb);
    if (cg.enter(body_bb)) {
        cg.remit(body());
        cg.jump_to_continuation(continue_continuation, cond()->loc().end());
    }
    cg.jump(head_bb, cond()->loc().end());
    head_bb.seal();
    cg.enter(exit_bb);
}

const thorin::Def* ForExpr::remit(CodeGen& cg) const {
    std::vector<const thorin::Def*> defs;
    defs.push_back(nullptr); // reserve for mem but set later - some other args may update the monad

    auto break_continuation = cg.create_continuation(break_decl());

    // peel off run and halt
    auto forexpr = expr();
    auto prefix = forexpr->isa<PrefixExpr>();
    if (prefix && (prefix->tag() == PrefixExpr::RUN || prefix->tag() == PrefixExpr::HLT))
        forexpr = prefix->rhs();

    // emit call
    auto map_expr = forexpr->as<MapExpr>();
    for (const auto& arg : map_expr->args())
        defs.push_back(cg.remit(arg));
    defs.push_back(cg.remit(fn_expr()));
    defs.push_back(break_continuation);
    auto fun = cg.remit(map_expr->lhs());
    if (prefix && prefix->tag() == PrefixExpr::RUN) fun = cg.world().run(fun, break_continuation, loc());
    if (prefix && prefix->tag() == PrefixExpr::HLT) fun = cg.world().hlt(fun, break_continuation, loc());

    defs.front() = cg.get_mem(); // now get the current memory monad
    cg.call(fun, defs, nullptr, map_expr->loc());

    cg.set_continuation(break_continuation);
    if (break_continuation->num_params() == 2)
        return break_continuation->param(1);
    else {
        Array<const thorin::Def*> defs(break_continuation->num_params()-1);
        for (size_t i = 0, e = defs.size(); i != e; ++i)
            defs[i] = break_continuation->param(i+1);
        return cg.world().tuple(defs, loc());
    }
}

const thorin::Def* FnExpr::remit(CodeGen& cg) const {
    auto continuation = emit_head(cg, loc());
    emit_body(cg, loc());
    return continuation;
}

/*
 * patterns
 */

void IdPtrn::emit(CodeGen& cg, const thorin::Def* init) const {
    init->name_ = local()->symbol().str();
    cg.emit(local(), init);
}

void TuplePtrn::emit(CodeGen& cg, const thorin::Def* init) const {
    for (size_t i = 0, e = num_elems(); i != e; ++i)
        cg.emit(elem(i), cg.extract(init, i, loc()));
}

/*
 * statements
 */

void ExprStmt::emit(CodeGen& cg) const {
    if (cg.is_reachable())
        cg.remit(expr());
}

void ItemStmt::emit(CodeGen& cg) const {
    cg.emit(item());
}

void LetStmt::emit(CodeGen& cg) const {
    if (cg.is_reachable())
        cg.emit(ptrn(), init() ? cg.remit(init()) : cg.world().bottom(cg.convert(ptrn()->type()), ptrn()->loc()));
}

//------------------------------------------------------------------------------

void emit(World& world, const ModContents* mod) {
    CodeGen cg(world);
    mod->emit(cg);
    clear_value_numbering_table(world);
}

//------------------------------------------------------------------------------

}
