#include "thorin/util/push.h"

#include <sstream>

#include "impala/ast.h"
#include "impala/dump.h"
#include "impala/impala.h"
#include "impala/sema/typetable.h"

#ifndef NDEBUG
#include <iostream>
#endif

namespace impala {

//------------------------------------------------------------------------------

class CheckBoundsData {
public:
    CheckBoundsData(const Location& l, Uni u, ArrayRef<Type> ts)
        : loc(l), unifiable(u), types(ts.size())
    {
        for (size_t i = 0, e = ts.size(); i != e; ++i) {
            if (auto t = ts[i])
                types[i] = t;
        }
    }

    const Location& loc;
    Uni unifiable;
    std::vector<Type> types;
};

class TypeSema : public TypeTable {
public:
    TypeSema(const bool nossa)
        : nossa_(nossa)
    {}

    void verfiy() const {
        assert(check_bounds_stash_.empty());
        TypeTable::verify();
    }

    bool nossa() const { return nossa_; }
    void push_impl(const ImplItem* i) { impls_.push_back(i); }
    void check_impls() {
        while (!impls_.empty()) {
            const ImplItem* i = impls_.back();
            impls_.pop_back();
            check_item(i);
        }
    }

    // error handling

    bool expect_lvalue(const Expr* expr) {
        if (!expr->is_lvalue()) {
            error(expr) << "lvalue required in assignment\n";
            return  false;
        }
        return true;
    }
    bool expect_int(const Expr*);
    void expect_num(const Expr*);
    Type expect_type(const Expr* expr, Type found, Type expected, const std::string& what);
    Type expect_type(const Expr* expr, Type expected, const std::string& what) { return expect_type(expr, expr->type(), expected, what); }

    TraitApp instantiate(const Location& loc, TraitAbs trait, Type self, ArrayRef<const ASTType*> args);
    Type instantiate(const Location& loc, Type type, ArrayRef<const ASTType*> args);
    Type check_call(const Location& loc, FnType fn_poly, const ASTTypes& type_args, std::vector<Type>& inferred_args, ArrayRef<const Expr*> args, Type expected);

    void stash_bound_check(const Location& loc, Uni unifiable, ArrayRef<Type> types) {
        CheckBoundsData cbs(loc, unifiable, types);
        check_bounds_stash_.push_back(cbs);
    }

    void treat_bound_check_stash() {
        for (CheckBoundsData cbs : check_bounds_stash_)
            check_bounds(cbs.loc, cbs.unifiable, cbs.types);
        check_bounds_stash_.clear();
    }

    bool check_bounds(const Location& loc, Uni unifiable, ArrayRef<Type> types);

    // check wrappers

    Type check(const TypeableDecl* decl) {
        if (!decl->checked_) { 
            decl->checked_ = true; 
            decl->type_ = decl->check(*this);
        }
        return decl->type();
    }
    Type check(const ValueDecl* decl, Type expected) {
        if (!decl->checked_) {
            decl->checked_ = true;
            decl->type_ = decl->check(*this, expected);
        }
        return decl->type();
    }
    void check_item(const Item* item) { item->check_item(*this); }
    Type check(const Expr* expr, Type expected, const std::string& what) {
        if (!expr->type_.empty())
            return expr->type_;
        return expr->type_ = expect_type(expr, expr->check(*this, expected), expected, what);
    }
    Type check(const Expr* expr, Type expected) { return check(expr, expected, ""); }
    /// a check that does not expect any type (i.e. any type is allowed)
    Type check(const Expr* expr) { return check(expr, unknown_type()); }
    Type check(const ASTType* ast_type) { return ast_type->type_ = ast_type->check(*this); }

private:
    bool nossa_;
    std::vector<const ImplItem*> impls_;
    std::vector<CheckBoundsData> check_bounds_stash_;

public:
    const BlockExpr* cur_block_expr_ = nullptr;
    const Fn* cur_fn_ = nullptr;
};

//------------------------------------------------------------------------------

// TODO factor code with expect_num
// TODO maybe have variant which also checks expr
bool TypeSema::expect_int(const Expr* expr) {
    Type t = expr->type();

    if (!t->is_error() &&
        !t->is_i8() && !t->is_i16() && !t->is_i32() && !t->is_i64() &&
        !t->is_u8() && !t->is_u16() && !t->is_u32() && !t->is_u64()) { // TODO factor this test out
        error(expr) << "expected integer type but found " << t << "\n";
        return false;
    }
    return true;
}

void TypeSema::expect_num(const Expr* expr) {
    Type t = expr->type();

    if (!t->is_error() &&
        !t->is_i8() && !t->is_i16() && !t->is_i32() && !t->is_i64() &&
        !t->is_u8() && !t->is_u16() && !t->is_u32() && !t->is_u64() &&
        !t->is_f32() && !t->is_f64()) // TODO factor this test out
        error(expr) << "expected number type but found " << t << "\n";
}

Type TypeSema::expect_type(const Expr* expr, Type found_type, Type expected, const std::string& what) {
    if (found_type == expected)
        return expected;

    // TODO: quick hack
    if (auto ptr = found_type.isa<OwnedPtrType>()) {
        if (expected.isa<PtrType>()) 
            return expected;
    }

    if (found_type->is_polymorphic()) { // try to infer instantiations for this polymorphic type
        std::vector<Type> type_args;
        Type inst = instantiate_unknown(found_type, type_args);
        if (inst == expected) {
            check_bounds(expr->loc(), *found_type, type_args);
            return expected;
        }
    }

    error(expr) << "mismatched types: expected '" << expected << "' but found '" << found_type 
        << (what.empty() ? std::string("'") : std::string("' as ") + what) << "\n";
    return expected;
}

TraitApp TypeSema::instantiate(const Location& loc, TraitAbs trait_abs, Type self, ArrayRef<const ASTType*> args) {
    if ((args.size()+1) == trait_abs->num_type_vars()) {
        std::vector<Type> type_args;
        type_args.push_back(self);
        for (auto t : args) 
            type_args.push_back(check(t));
        stash_bound_check(loc, *trait_abs, type_args);
        return trait_abs->instantiate(type_args);
    } else
        error(loc) << "wrong number of instances for bound type variables of trait '" << trait_abs << "': " << args.size() << " for " << (trait_abs->num_type_vars()-1) << "\n";

    return trait_app_error();
}

Type TypeSema::instantiate(const Location& loc, Type type, ArrayRef<const ASTType*> args) {
    if (args.size() == type->num_type_vars()) {
        std::vector<Type> type_args;
        for (auto t : args) 
            type_args.push_back(check(t));
        stash_bound_check(loc, *type, type_args);
        return type->instantiate(type_args);
    } else
        error(loc) << "wrong number of instances for bound type variables: " << args.size() << " for " << type->num_type_vars() << "\n";

    return type_error();
}

bool TypeSema::check_bounds(const Location& loc, Uni unifiable, ArrayRef<Type> type_args) {
    SpecializeMap map = specialize_map(unifiable, type_args);
    assert(map.size() == type_args.size());
    bool result = true;

    for (size_t i = 0, e = type_args.size(); i != e; ++i) {
        auto type_var = unifiable->type_var(i);
        Type arg = type_args[i];
        assert(map.contains(*type_var));
        assert(map.find(*type_var)->second == *arg);

        for (auto bound : type_var->bounds()) {
            // TODO do we need this copy?
            SpecializeMap bound_map(map); // copy the map per type var
            auto spec_bound = bound->specialize(bound_map);
            if (!arg->is_error() && !spec_bound->is_error()) {
                check_impls(); // first we need to check all implementations to be up-to-date
                if (!arg->implements(spec_bound, bound_map)) {
                    error(loc) << "'" << arg << "' (instance for '" << type_var << "') does not implement bound '" << spec_bound << "'\n";
                    result = false;
                }
            }
        }
    }

    return result;
}

//------------------------------------------------------------------------------

/*
 * AST types
 */

void TypeParamList::check_type_params(TypeSema& sema) const {
    for (auto type_param : type_params()) {
        auto type_var = type_param->type_var(sema);
        for (auto bound : type_param->bounds()) {
            if (auto type_app = bound->isa<ASTTypeApp>()) {
                type_var->add_bound(type_app->trait_app(sema, type_var));
            } else {
                sema.error(type_param) << "bounds must be trait instances, not types\n";
            }
        }
    }
}

Type TypeParam::check(TypeSema& sema) const { return sema.type_var(symbol()); }
TypeVar TypeParam::type_var(TypeSema& sema) const { return sema.check(this).as<TypeVar>(); }
Type ErrorASTType::check(TypeSema& sema) const { return sema.type_error(); }

Type PrimASTType::check(TypeSema& sema) const {
    switch (kind()) {
#define IMPALA_TYPE(itype, atype) case TYPE_##itype: return sema.type(PrimType_##itype);
#include "impala/tokenlist.h"
        default: THORIN_UNREACHABLE;
    }
}

Type PtrASTType::check(TypeSema& sema) const {
    auto type = sema.check(referenced_type());
    if (is_owned())
        return sema.owned_ptr_type(type);
    if (is_borrowed())
        return sema.borrowd_ptr_type(type);
    assert(false && "only owned and borrowed ptrs are supported");
    return Type();
}

Type IndefiniteArrayASTType::check(TypeSema& sema) const {
    return sema.indefinite_array_type(sema.check(elem_type()));
}

Type DefiniteArrayASTType::check(TypeSema& sema) const {
    return sema.definite_array_type(sema.check(elem_type()), dim());
}

Type TupleASTType::check(TypeSema& sema) const {
    std::vector<Type> types;
    for (auto arg : args())
        types.push_back(sema.check(arg));

    return sema.tuple_type(types);
}

Type FnASTType::check(TypeSema& sema) const {
    check_type_params(sema);

    std::vector<Type> params;
    for (auto arg : args())
        params.push_back(sema.check(arg));

    FnType fn_type = sema.fn_type(params);
    for (auto type_param : type_params())
        fn_type->bind(type_param->type_var(sema));

    return fn_type;
}

Type ASTTypeApp::check(TypeSema& sema) const {
    if (decl()) {
        if (auto type_decl = decl()->isa<TypeDecl>()) {
            return sema.instantiate(loc(), sema.check(type_decl), args());
        } else
            sema.error(this) << '\'' << symbol() << "' does not name a type\n";
    }

    return sema.type_error();
}

TraitApp ASTTypeApp::trait_app(TypeSema& sema, Type self) const {
    if (decl()) {
        if (auto trait_decl = decl()->isa<TraitDecl>()) {
            sema.check_item(trait_decl);
            return sema.instantiate(this->loc(), trait_decl->trait_abs(), self, args());
        } else
            sema.error(this) << '\'' << symbol() << "' does not name a trait\n";
    }
    return sema.trait_app_error();
}

//------------------------------------------------------------------------------

Type ValueDecl::check(TypeSema& sema) const { return check(sema, Type()); }

Type ValueDecl::check(TypeSema& sema, Type expected) const {
    if (auto local = this->isa<LocalDecl>())
        local->fn_ = sema.cur_fn_;

    if (ast_type()) {
        Type t = sema.check(ast_type());
        if (expected.empty() || expected == t) {
            return t;
        } else {
            sema.error(this) << "could not infer types: expected '" << expected << "' but found '" << t << "'\n";
            return sema.type_error();
        }
    } else if (expected.empty()) {
        sema.error(this) << "could not infer parameter type for " << this << "\n";
        return sema.type_error();
    } else {
        return expected;
    }
}

void Fn::check_body(TypeSema& sema, FnType fn_type) const {
    auto return_type = fn_type->return_type();
    // TODO as noret is also valid we cannot use the return type as expected type. However this is bad for type inference...
    Type body_type = sema.check(body(), sema.unknown_type());
    if (!body_type->is_noret() && !body_type->is_error())
        sema.expect_type(body(), return_type, "return type");

    for (auto param : params()) {
        if (param->is_mut() && !param->is_written())
            sema.warn(param) << "parameter '" << param->symbol() << "' declared mutable but parameter is never written to\n";
    }
}

//------------------------------------------------------------------------------

/*
 * items
 */

void TypeDeclItem::check_item(TypeSema& sema) const { sema.check(static_cast<const TypeDecl*>(this)); }
void ValueItem::check_item(TypeSema& sema) const { sema.check(static_cast<const ValueDecl*>(this)); }

Type ModDecl::check(TypeSema& sema) const {
    if (mod_contents())
        mod_contents()->check(sema);
    return Type();
}

void ModContents::check(TypeSema& sema) const {
    std::vector<const Item*> non_impls;
    for (auto item : items()) {
        if (auto impl = item->isa<const ImplItem>()) {
            sema.push_impl(impl);
        } else
            non_impls.push_back(item);
    }

    sema.check_impls();
    for (auto item : non_impls)
        sema.check_item(item);
}

void ExternBlock::check_item(TypeSema& sema) const {
    if (!abi().empty())
        if (abi() != Symbol("\"C\"") && abi() != Symbol("\"device\"") && abi() != Symbol("\"thorin\""))
            sema.error(this) << "unknown extern specification\n";  // TODO: better location
    for (auto fn : fns())
        sema.check(fn);
}

Type Typedef::check(TypeSema& sema) const {
    return Type();
}

Type EnumDecl::check(TypeSema& sema) const {
    return Type();
}

Type StructDecl::check(TypeSema& sema) const {
    check_type_params(sema);
    auto struct_type = sema.struct_abs_type(this);
    type_ = struct_type;    // break cycle to allow recursive types
    for (auto field : field_decls())
        struct_type->set(field->index(), sema.check(field));
    for (auto type_param : type_params())
        struct_type->bind(type_param->type_var(sema));

    sema.treat_bound_check_stash();

    type_.clear();          // will be set again by TypeSema's wrapper
    return struct_type;
}

Type FieldDecl::check(TypeSema& sema) const {
    return sema.check(ast_type());
}

Type FnDecl::check(TypeSema& sema) const {
    THORIN_PUSH(sema.cur_fn_, this);
    check_type_params(sema);
    std::vector<Type> types;
    for (auto param : params())
        types.push_back(sema.check(param));

    // create FnType
    FnType fn_type = sema.fn_type(types);
    for (auto tp : type_params())
        fn_type->bind(tp->type_var(sema));
    type_ = fn_type;

    sema.treat_bound_check_stash();

    if (body() != nullptr)
        check_body(sema, fn_type);

    type_.clear();          // will be set again by TypeSema's wrapper
    return fn_type;
}

Type StaticItem::check(TypeSema& sema) const {
    return Type();
}

void TraitDecl::check_item(TypeSema& sema) const {
    // did we already check this trait?
    if (!trait_abs().empty())
        return;

    TypeVar self_var = self_param()->type_var(sema);
    trait_abs_ = sema.trait_abs(this);
    trait_abs_->bind(self_var);

    check_type_params(sema);
    for (auto tp : type_params())
        trait_abs_->bind(tp->type_var(sema));

    for (auto type_app : super_traits()) {
        if (!trait_abs_->add_super_trait(type_app->trait_app(sema, self_var)))
            sema.error(type_app) << "duplicate super trait '" << type_app << "' for trait '" << symbol() << "'\n";
    }

    sema.treat_bound_check_stash();

    for (auto m : methods())
        sema.check(m);
}

void ImplItem::check_item(TypeSema& sema) const {
    check_type_params(sema);
    Type for_type = sema.check(this->type());

    TraitApp trait_app;
    if (trait() != nullptr) {
        if (auto type_app = trait()->isa<ASTTypeApp>()) {
            trait_app = type_app->trait_app(sema, for_type);
            auto impl = sema.impl(this, trait_app, for_type);
            for (auto tp : type_params())
                impl->bind(tp->type_var(sema));

            if (!for_type->is_error() && !trait_app->is_error()) {
                for_type.as<KnownType>()->add_impl(impl);
                trait_app->trait()->add_impl(impl);
            }
        } else
            sema.error(trait()) << "expected trait instance\n";
    }

    sema.treat_bound_check_stash();

    thorin::HashSet<Symbol> implemented_methods;
    for (auto fn : methods()) {
        Type fn_type = sema.check(fn);

        if (trait() != nullptr) {
            assert(!trait_app.empty());

            Symbol meth_name = fn->symbol();
            Type t = trait_app->find_method(meth_name);
            if (!t.empty()) {
                // remember name for check if all methods were implemented
                auto p = implemented_methods.insert(meth_name);
                assert(p.second && "there should be no such name in the set"); // else name analysis failed

                // check that the types match
                if (!(fn_type == t))
                    sema.error(fn) << "method '" << trait() << "." << meth_name << "' should have type '" << fn_type << "', but implementation has type '" << t << "'\n";
            }
        }
    }

    // TODO
#if 0
    // check that all methods are implemented
    if (!bound.empty()) {
        if (implemented_methods.size() != bound->num_methods()) {
            assert(implemented_methods.size() < bound->num_methods());
            for (auto p : bound->all_methods()) {
                if (!implemented_methods.contains(p.first))
                    sema.error(this) << "must implement method '" << p.first << "'\n";
            }
        }
    }
#endif
}

//------------------------------------------------------------------------------

/*
 * expressions
 */

Type EmptyExpr::check(TypeSema& sema, Type) const { return sema.unit(); }

Type BlockExpr::check(TypeSema& sema, Type expected) const {
    THORIN_PUSH(sema.cur_block_expr_, this);
    for (auto stmt : stmts())
        stmt->check(sema);

    sema.check(expr(), expected);

    for (auto local : locals_) {
        if (local->is_mut() && !local->is_written())
            sema.warn(local) << "variable '" << local->symbol() << "' declared mutable but variable is never written to\n";
    }

    return expr() ? expr()->type() : sema.unit().as<Type>();
}

Type LiteralExpr::check(TypeSema& sema, Type expected) const {
    // FEATURE we could enhance this using the expected type (e.g. 4 could be interpreted as int8 if needed)
    return sema.type(literal2type());
}

Type FnExpr::check(TypeSema& sema, Type expected) const {
    THORIN_PUSH(sema.cur_fn_, this);
    assert(type_params().empty());

    FnType fn_type;
    if (FnType exp_fn = expected.isa<FnType>()) {
        if (exp_fn->num_args() == num_params()+1) // add return param to infer type
            const_cast<FnExpr*>(this)->params_.push_back(Param::create(ret_var_handle_, "return", body()->pos1(), nullptr));
        else if (exp_fn->num_args() != num_params())
            sema.error(this) << "expected function with " << exp_fn->num_args() << " parameters, but found lambda expression with " << num_params() << " parameters\n";

        size_t i = 0;
        for (auto param : params())
            sema.check(param, exp_fn->arg(i++));

        fn_type = exp_fn;
    } else {
        std::vector<Type> par_types;
        for (auto param : params())
            par_types.push_back(sema.check(param));

        fn_type = sema.fn_type(par_types);
    }

    assert(body() != nullptr);
    check_body(sema, fn_type);

    return fn_type;
}

Type PathExpr::check(TypeSema& sema, Type expected) const {
    // FEATURE consider longer paths
    //auto* last = path()->path_args().back();
    if (value_decl()) {
        if (auto local = value_decl()->isa<LocalDecl>()) {
            // if local lies in an outer function go through memory to implement closure
            if (local->is_mut() && local->fn() != sema.cur_fn_) 
                local->take_address();
        }
        return sema.check(value_decl());
    }
    return sema.type_error();
}

Type PrefixExpr::check(TypeSema& sema, Type expected) const {
    // TODO check if operator supports the type
    auto rtype = sema.check(rhs());
    switch (kind()) {
        case AND:
            rhs()->take_address();
            return sema.borrowd_ptr_type(rtype);
        case TILDE:
            return sema.owned_ptr_type(rtype);
        case MUL:
            if (auto ptr = rtype.isa<PtrType>())
                return ptr->referenced_type();
        case INC:
        case DEC:
            sema.expect_num(rhs());
            sema.expect_lvalue(rhs());
            return rtype;
        case ADD:
        case SUB:
            sema.expect_num(rhs());
            return rtype;
        case RUN:
        case HLT:
            return sema.check(rhs());
        default:
            THORIN_UNREACHABLE;
    }

    return sema.type_error();
}

Type InfixExpr::check(TypeSema& sema, Type expected) const {
    switch (kind()) {
        case EQ:
        case NE:
            sema.check(rhs(), sema.check(lhs()));
            return sema.type_bool();
        case LT:
        case LE:
        case GT:
        case GE:
            sema.check(rhs(), sema.check(lhs()));
            sema.expect_num(lhs());
            sema.expect_num(rhs());
            return sema.type_bool();
        case OROR:
        case ANDAND:
            sema.check(lhs(), sema.type_bool(), "left-hand side of logical boolean expression");
            sema.check(rhs(), sema.type_bool(), "right-hand side of logical boolean expression");
            return sema.type_bool();
        case ADD:
        case SUB:
        case MUL:
        case DIV:
        case REM: {
            auto type = sema.check(lhs(), sema.check(rhs()));
            sema.expect_num(lhs());
            sema.expect_num(rhs());
            return type;
        }
        case SHL:
        case SHR:
        case OR:
        case XOR:
        case AND: {
            auto type = sema.check(lhs(), sema.check(rhs()));
            sema.expect_num(lhs());
            sema.expect_num(rhs());
            return type;
        }
        case ASGN:
            sema.check(rhs(), sema.check(lhs()));
            if (sema.expect_lvalue(lhs()))
                return sema.unit();
            break;
        case ADD_ASGN:
        case SUB_ASGN:
        case MUL_ASGN:
        case DIV_ASGN:
        case REM_ASGN:
        case AND_ASGN:
        case  OR_ASGN:
        case XOR_ASGN:
        case SHL_ASGN:
        case SHR_ASGN: {
            // TODO handle floats etc
            sema.check(rhs(), sema.check(lhs()));
            if (sema.expect_lvalue(lhs())) {
                sema.expect_num(lhs());
                sema.expect_num(rhs());
                return sema.unit();
            }
            break;
        }
        default: THORIN_UNREACHABLE;
    }

    return sema.type_error();
}

Type PostfixExpr::check(TypeSema& sema, Type expected) const {
    // TODO check if operator supports the type
    sema.check(lhs(), expected);
    sema.expect_lvalue(lhs());
    return lhs()->type(); 
}

Type CastExpr::check(TypeSema& sema, Type expected) const {
    // TODO check whether cast is possible at all
    sema.check(lhs());
    return sema.check(ast_type());
}

Type DefiniteArrayExpr::check(TypeSema& sema, Type expected) const {
    Type elem_type = sema.unknown_type();
    for (auto arg : args())
        sema.expect_type(arg, sema.check(arg), elem_type, "element of definite array expression");
    return sema.definite_array_type(elem_type, num_args());
}

Type RepeatedDefiniteArrayExpr::check(TypeSema& sema, Type expected) const {
    return Type(); // TODO
}

Type IndefiniteArrayExpr::check(TypeSema& sema, Type expected) const {
    sema.check(dim());
    sema.expect_int(dim());
    return sema.indefinite_array_type(sema.check(elem_type()));
}

Type TupleExpr::check(TypeSema& sema, Type expected) const {
    std::vector<Type> types;
    if (auto exp_tup = expected.isa<TupleType>()) {
        if (exp_tup->num_args() != num_args())
            sema.error(this) << "expected tuple with " << exp_tup->num_args() << " elements, but found tuple expression with " << num_args() << " elements\n";

        size_t i = 0;
        for (auto arg : args()) {
            sema.check(arg, exp_tup->arg(i++));
            types.push_back(arg->type());
        }
    } else {
        for (auto arg : args()) {
            sema.check(arg);
            types.push_back(arg->type());
        }
    }
    return sema.tuple_type(types);
}

Type StructExpr::check(TypeSema& sema, Type expected) const {
    if (auto decl = path()->decl()) {
        if (auto struct_decl = decl->isa<StructDecl>()) {
            auto struct_abs = struct_decl->type().as<StructAbsType>();
            if (num_type_args() <= struct_abs->num_type_vars()) {
                for (auto type_arg : type_args())
                    inferred_args_.push_back(sema.check(type_arg));

                for (size_t i = num_type_args(), e = struct_abs->num_type_vars(); i != e; ++i)
                    inferred_args_.push_back(sema.unknown_type());

                assert(inferred_args_.size() == struct_abs->num_type_vars());
                auto struct_app = struct_abs->instantiate(inferred_args_).as<StructAppType>();

                thorin::HashSet<const FieldDecl*> done;
                for (const auto& elem : elems()) {
                    if (auto field_decl = struct_decl->field_decl(elem.symbol())) {
                        elem.field_decl_ = field_decl;
                        if (!thorin::visit(done, field_decl)) {
                            sema.check(elem.expr());
                            std::ostringstream oss;
                            oss << "initialization type for field '" << elem.symbol() << '\'';
                            sema.expect_type(elem.expr(), elem.expr()->type(), struct_app->elem(field_decl->index()), oss.str());
                        } else
                            sema.error(elem.expr()) << "field '" << elem.symbol() << "' specified more than once\n";
                    } else
                        sema.error(elem.expr()) << "structure '" << struct_decl->symbol() << "' has no field named '" << elem.symbol() << "'\n";
                }

                if (done.size() != struct_decl->field_table().size()) {
                    for (auto p : struct_decl->field_table()) {
                        if (!done.contains(p.second))
                            sema.error(this) << "missing field '" << p.first << "'\n";
                    }
                }

                return struct_app;
            } else
                sema.error(this) << "too many type arguments to structure: " << num_type_args() << " for " << struct_abs->num_type_vars() << "\n";
        } else
            sema.error(path()) << '\'' << decl->symbol() << '\'' << " does not name a structure\n";
    }
    return sema.type_error();
}

Type TypeSema::check_call(const Location& loc, FnType fn_poly, const ASTTypes& type_args, std::vector<Type>& inferred_args, ArrayRef<const Expr*> args, Type expected) {
    size_t num_type_args = type_args.size();
    size_t num_args = args.size();

    if (num_type_args <= fn_poly->num_type_vars()) {
        for (auto type_arg : type_args)
            inferred_args.push_back(check(type_arg));

        for (size_t i = num_type_args, e = fn_poly->num_type_vars(); i != e; ++i)
            inferred_args.push_back(unknown_type());

        assert(inferred_args.size() == fn_poly->num_type_vars());
        auto fn_mono = fn_poly->instantiate(inferred_args).as<FnType>();

        bool is_contuation = num_args == fn_mono->num_args();
        if (is_contuation || num_args+1 == fn_mono->num_args()) {
            for (size_t i = 0; i != num_args; ++i)
                check(args[i], fn_mono->arg(i), "argument type");

            // note: the order is important because of the unifying side-effects of ==
            if (is_contuation || fn_mono->return_type() == expected) { // TODO this looks overly complicated
                // check if all type variables could be inferred
                bool is_known = true;
                for (size_t i = 0, e = inferred_args.size(); i != e; ++i) {
                    if (!inferred_args[i]->is_known()) {
                        is_known = false;
                        error(loc) << "could not find instance for type variable #" << i << "\n";
                    }
                }

                if (is_known) {
                    check_bounds(loc, fn_poly, inferred_args);

                    if (is_contuation) {
                        return type_noret();
                    } else
                        return expected;
                }
            } else
                error(loc) << "return type '" << fn_mono->return_type() << "' does not match expected type '" << expected << "'\n";
        } else {
            std::string rela = (num_args+1 < fn_mono->num_args()) ? "few" : "many";
            error(loc) << "too " << rela << " arguments: " << num_args << " for " << fn_mono->num_args()-1 << "\n";
        }
    } else
        error(loc) << "too many type arguments to function: " << num_type_args << " for " << fn_poly->num_type_vars() << "\n";

    return type_error();
}

Type FieldExpr::check(TypeSema& sema, Type expected) const {
    if (auto type = check_as_struct(sema, expected))
        return type;

    if (!lhs()->type()->is_error())
        sema.error(lhs()) << "attempted access of field '" << symbol() << "' on type '" << lhs()->type() << "', but no field with that name was found\n";
    return sema.type_error();
}

Type FieldExpr::check_as_struct(TypeSema& sema, Type expected) const {
    auto type = sema.check(lhs());
    if (auto struct_app = type.isa<StructAppType>()) {
        if (auto field_decl = struct_app->struct_abs_type()->struct_decl()->field_decl(symbol())) {
            index_ = field_decl->index();
            sema.expect_type(this, struct_app->elem(index_), expected, "field expression type");
            return expected;
        }
    } 

    return Type();
}

Type MapExpr::check(TypeSema& sema, Type expected) const {
    if (auto field_expr = lhs()->isa<FieldExpr>()) {
        if (auto type = field_expr->check_as_struct(sema, sema.unknown_type()))
            return check_as_map(sema, expected);
        return check_as_method_call(sema, expected);
    } 

    return check_as_map(sema, expected);
}

Type MapExpr::check_as_map(TypeSema& sema, Type expected) const {
    auto ltype = sema.check(lhs());
    if (auto ptr = ltype.isa<PtrType>()) {
        ltype.clear();
        PrefixExpr::create_deref(lhs_);
        ltype = sema.check(lhs());
    }

    if (auto fn_poly = ltype.isa<FnType>()) {
        return sema.check_call(this->loc(), fn_poly, type_args(), inferred_args_, args(), expected);
    } else if (auto array = ltype.isa<ArrayType>()) {
        if (num_args() == 1) {
            sema.check(arg(0));
            if (sema.expect_int(arg(0)))
                return array->elem_type();
            else
                sema.error(this) << "require integer as array subscript\n";
        } else
            sema.error(this) << "too many array subscripts\n";
    } else if (auto exp_tup = ltype.isa<TupleType>()) {
        if (num_args() == 1) {
            sema.check(arg(0));
            if (sema.expect_int(arg(0))) {
                if (auto lit = arg(0)->isa<LiteralExpr>())
                    return exp_tup->arg(lit->get_u64());
                else
                    sema.error(this) << "require literal as tuple subscript\n";
            } else
                sema.error(this) << "require integer as tuple subscript\n";
        } else
            sema.error(this) << "too many tuple subscripts\n";
    } else
        sema.error(this) << "incorrect type for map expression\n";

    return sema.type_error();
}

Type MapExpr::check_as_method_call(TypeSema& sema, Type expected) const {
    auto field_expr = lhs()->as<FieldExpr>();
    sema.check_impls();
    if (auto fn_method = sema.check(field_expr->lhs())->find_method(field_expr->symbol())) {
        Array<const Expr*> nargs(num_args() + 1);
        nargs[0] = field_expr->lhs();
        std::copy(args().begin(), args().end(), nargs.begin()+1);
        return sema.check_call(this->loc(), fn_method, type_args(), inferred_args_, nargs, expected);
    } else
        sema.error(this) << "no declaration for method '" << field_expr->symbol() << "' found\n";
    return sema.type_error();
}

Type ForExpr::check(TypeSema& sema, Type expected) const {
    auto forexpr = expr();
    if (auto prefix = forexpr->isa<PrefixExpr>())
        if (prefix->kind() == PrefixExpr::RUN || prefix->kind() == PrefixExpr::HLT)
            forexpr = prefix->rhs();
    if (auto map = forexpr->isa<MapExpr>()) {
        Type lhst = sema.check(map->lhs());

        if (auto fn_for = lhst.isa<FnType>()) {
            Array<const Expr*> args(map->args().size()+1);
            *std::copy(map->args().begin(), map->args().end(), args.begin()) = fn_expr();
            return sema.check_call(map->loc(), fn_for, map->type_args(), map->inferred_args_, args, expected);
        }
    } else if (auto field_expr = forexpr->isa<FieldExpr>()) {
        assert(false && field_expr && "TODO");
    }

    sema.error(expr()) << "the looping expression does not support the 'for' protocol\n";
    return sema.unit();
}

Type IfExpr::check(TypeSema& sema, Type expected) const {
    sema.check(cond(), sema.type_bool(), "condition type");
    Type then_type = sema.check(then_expr(), sema.unknown_type());
    Type else_type = sema.check(else_expr(), sema.unknown_type());

    if (then_type->is_noret() && else_type->is_noret())
        return sema.type_noret();
    if (then_type->is_noret())
        return sema.expect_type(else_expr(), else_type, expected, "if expression type");
    if (else_type->is_noret())
        return sema.expect_type(then_expr(), then_type, expected, "if expression type");
    if (then_type == else_type)
        return sema.expect_type(this, then_type, expected, "if expression type");

    sema.error(this) << "different types in arms of an if expression\n";
    sema.error(then_expr()) << "type of the consequence is '" << then_type << "'\n";
    sema.error(else_expr()) << "type of the alternative is '" << else_type << "'\n";
    return sema.type_error();
}

//------------------------------------------------------------------------------

/*
 * statements
 */

void ExprStmt::check(TypeSema& sema) const {
    if (sema.check(expr())->is_noret())
        sema.error(expr()) << "expression does not return. Rendering subsequent statements unreachable\n";
}

void ItemStmt::check(TypeSema& sema) const {
    sema.check_item(item());
}

void LetStmt::check(TypeSema& sema) const {
    sema.cur_block_expr_->add_local(local());
    Type expected = sema.check(local(), sema.unknown_type());
    if (init())
        sema.check(init(), expected, "initialization type");
}

//------------------------------------------------------------------------------

bool type_analysis(Init& init, const ModContents* mod, bool nossa) {
    auto sema = new TypeSema(nossa);
    init.typetable = sema;
    mod->check(*sema);
#ifndef NDEBUG
    sema->verify();
#endif
    return sema->result();
}

//------------------------------------------------------------------------------

}
