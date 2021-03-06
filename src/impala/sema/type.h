#ifndef IMPALA_SEMA_TYPE_H
#define IMPALA_SEMA_TYPE_H

#include "thorin/util/array.h"
#include "thorin/util/cast.h"
#include "thorin/util/hash.h"
#include "thorin/util/stream.h"

#include "impala/symbol.h"

namespace impala {

enum Tag {
#define IMPALA_TYPE(itype, atype) Tag_##itype,
#include "impala/tokenlist.h"
    Tag_app,
    Tag_borrowed_ptr,
    Tag_definite_array,
    Tag_error,
    Tag_fn,
    Tag_impl,
    Tag_infer_error,
    Tag_indefinite_array,
    Tag_lambda,
    Tag_noret,
    Tag_owned_ptr,
    Tag_pi,
    Tag_ref,
    Tag_simd,
    Tag_struct,
    Tag_enum,
    Tag_tuple,
    Tag_typedef_abs,
    Tag_unknown,
    Tag_var,
};

enum PrimTypeTag {
#define IMPALA_TYPE(itype, atype) PrimType_##itype = Tag_##itype,
#include "impala/tokenlist.h"
};

class StructDecl;
class EnumDecl;
template<class T> using ArrayRef = thorin::ArrayRef<T>;
template<class T> using Array    = thorin::Array<T>;

static const int Node_App        = impala::Tag_app;
static const int Node_Lambda     = impala::Tag_lambda;
static const int Node_Pi         = impala::Tag_pi;
static const int Node_StructType = impala::Tag_struct;
static const int Node_EnumType   = impala::Tag_enum;
static const int Node_TupleType  = impala::Tag_tuple;
static const int Node_TypeError  = impala::Tag_error;
static const int Node_Var        = impala::Tag_var;

#define HENK_STRUCT_EXTRA_NAME  struct_decl
#define HENK_STRUCT_EXTRA_TYPE  const StructDecl*

#define HENK_ENUM_EXTRA_NAME  enum_decl
#define HENK_ENUM_EXTRA_TYPE  const EnumDecl*

#define HENK_TABLE_NAME  typetable
#define HENK_TABLE_TYPE  TypeTable
#include "thorin/henk.h"

//------------------------------------------------------------------------------

/// Primitive type.
class PrimType : public Type {
private:
    PrimType(TypeTable& typetable, PrimTypeTag tag)
        : Type(typetable, (Tag) tag, {})
    {}

public:
    PrimTypeTag primtype_tag() const { return (PrimTypeTag) tag(); }

    virtual std::ostream& stream(std::ostream&) const override;

private:
    virtual const Type* vrebuild(TypeTable&, Types) const override;
    virtual const Type* vreduce(int, const Type*, Type2Type&) const override;

    friend class TypeTable;
};

bool is(const Type*, PrimTypeTag tag);
#define IMPALA_TYPE(itype, atype) inline bool is_##itype(const Type* t) { return is(t, PrimType_##itype); }
#include "impala/tokenlist.h"
inline bool is_float(const Type* t) { return             is_f16(t) || is_f32(t) || is_f64(t); }
inline bool is_int  (const Type* t) { return is_i8(t) || is_i16(t) || is_i32(t) || is_i64(t)
                                          || is_u8(t) || is_u16(t) || is_u32(t) || is_u64(t); }
bool is_void(const Type*);
bool is_subtype(const Type* dst, const Type* src);
bool is_strict_subtype(const Type* dst, const Type* src);

//------------------------------------------------------------------------------

/// Common base Type for PtrType%s and RefType.
class RefTypeBase : public Type {
protected:
    RefTypeBase(TypeTable& typetable, int tag, const Type* pointee, bool mut, int addr_space)
        : Type(typetable, tag, {pointee})
        , mut_(mut)
        , addr_space_(addr_space)
    {}

public:
    const Type* pointee() const { return op(0); }
    bool is_mut() const { return mut_; }
    int addr_space() const { return addr_space_; }

    virtual std::ostream& stream(std::ostream&) const override;
    virtual uint64_t vhash() const override;
    virtual bool equal(const Type* other) const override;
    virtual std::string prefix() const = 0;

private:
    bool mut_;
    int addr_space_;

    friend class TypeTable;
};

/// Pointer @p Type.
class PtrType : public RefTypeBase {
protected:
    PtrType(TypeTable& typetable, int tag, const Type* pointee, bool mut, int addr_space)
        : RefTypeBase(typetable, tag, pointee, mut, addr_space)
    {}

    std::ostream& stream_ptr_type(std::ostream&, std::string prefix, int addr_space, const Type* ref_type) const;

private:
    int addr_space_;

    friend class TypeTable;
};

class BorrowedPtrType : public PtrType {
public:
    BorrowedPtrType(TypeTable& typetable, const Type* pointee, bool mut, int addr_space)
        : PtrType(typetable, Tag_borrowed_ptr, pointee, mut, addr_space)
    {}

    virtual std::string prefix() const override { return is_mut() ? "&mut " : "&"; }

private:
    virtual const Type* vrebuild(TypeTable&, Types) const override;
    virtual const Type* vreduce(int, const Type*, Type2Type&) const override;
};

class OwnedPtrType : public PtrType {
public:
    OwnedPtrType(TypeTable& typetable, const Type* pointee, int addr_space)
        : PtrType(typetable, Tag_owned_ptr, pointee, true, addr_space)
    {}

    virtual std::string prefix() const override { return "~"; }

private:
    virtual const Type* vrebuild(TypeTable&, Types) const override;
    virtual const Type* vreduce(int, const Type*, Type2Type&) const override;
};

class RefType : public RefTypeBase {
protected:
    RefType(TypeTable& typetable, const Type* pointee, bool mut, int addr_space)
        : RefTypeBase(typetable, Tag_ref, pointee, mut, addr_space)
    {}

public:
    virtual std::string prefix() const override { return is_mut() ? "lvalue of " : "reference of "; }

private:
    virtual const Type* vrebuild(TypeTable&, Types) const override;
    virtual const Type* vreduce(int, const Type*, Type2Type&) const override;

    friend class TypeTable;
};

inline const RefType* is_lvalue(const Type* type) {
    if (auto ref = type->isa<RefType>()) {
        if (ref->is_mut())
            return ref;
    }
    return nullptr;
}

inline const Type* unpack_ref_type(const Type* type) {
    return type->isa<RefType>() ? type->as<RefType>()->pointee() : type;
}

inline bool is_ptr(const Type* t) {
    return t->isa<PtrType>() || (t->isa<RefType>() && t->as<RefType>()->pointee()->isa<PtrType>());
}

inline const RefType* split_ref_type(const Type*& type) {
    auto ref = type->isa<RefType>();
    type = ref ? ref->pointee() : type;
    return ref;
}

//------------------------------------------------------------------------------

class FnType : public Type {
private:
    FnType(TypeTable& typetable, Types ops)
        : Type(typetable, Tag_fn, ops)
    {
        ++order_;
    }

public:
    const Type* return_type() const;
    bool is_returning() const;
    virtual std::ostream& stream(std::ostream&) const override;

private:
    virtual const Type* vrebuild(TypeTable&, Types) const override;
    virtual const Type* vreduce(int, const Type*, Type2Type&) const override;

    friend class TypeTable;
};

class ArrayType : public Type {
protected:
    ArrayType(TypeTable& typetable, int tag, const Type* elem_type)
        : Type(typetable, tag, {elem_type})
    {}

public:
    const Type* elem_type() const { return op(0); }
};

class IndefiniteArrayType : public ArrayType {
public:
    IndefiniteArrayType(TypeTable& typetable, const Type* elem_type)
        : ArrayType(typetable, Tag_indefinite_array, elem_type)
    {}

    virtual std::ostream& stream(std::ostream&) const override;

private:
    virtual const Type* vrebuild(TypeTable&, Types) const override;
    virtual const Type* vreduce(int, const Type*, Type2Type&) const override;

    friend class TypeTable;
};

class DefiniteArrayType : public ArrayType {
public:
    DefiniteArrayType(TypeTable& typetable, const Type* elem_type, uint64_t dim)
        : ArrayType(typetable, Tag_definite_array, elem_type)
        , dim_(dim)
    {}

    uint64_t dim() const { return dim_; }
    virtual uint64_t vhash() const override { return thorin::hash_combine(Type::vhash(), dim()); }
    virtual bool equal(const Type* other) const override {
        return Type::equal(other) && this->dim() == other->as<DefiniteArrayType>()->dim();
    }

    virtual std::ostream& stream(std::ostream&) const override;

private:
    virtual const Type* vrebuild(TypeTable&, Types) const override;
    virtual const Type* vreduce(int, const Type*, Type2Type&) const override;

    uint64_t dim_;

    friend class TypeTable;
};

class SimdType : public ArrayType {
public:
    SimdType(TypeTable& typetable, const Type* elem_type, uint64_t dim)
        : ArrayType(typetable, Tag_simd, elem_type)
        , dim_(dim)
    {}

    uint64_t dim() const { return dim_; }
    virtual uint64_t vhash() const override { return thorin::hash_combine(Type::vhash(), dim()); }
    virtual bool equal(const Type* other) const override {
        return Type::equal(other) && this->dim() == other->as<SimdType>()->dim();
    }

    virtual std::ostream& stream(std::ostream&) const override;

private:
    virtual const Type* vrebuild(TypeTable&, Types) const override;
    virtual const Type* vreduce(int, const Type*, Type2Type&) const override;

    uint64_t dim_;

    friend class TypeTable;
};

class NoRetType : public Type {
private:
    NoRetType(TypeTable& typetable)
        : Type(typetable, Tag_noret, {})
    {}

public:
    virtual std::ostream& stream(std::ostream&) const override;

private:
    virtual const Type* vrebuild(TypeTable&, Types) const override;
    virtual const Type* vreduce(int, const Type*, Type2Type&) const override;

    friend class TypeTable;
};

class UnknownType : public Type {
private:
    UnknownType(TypeTable& typetable)
        : Type(typetable, Tag_unknown, {})
    {
        known_ = false;
    }

public:
    virtual std::ostream& stream(std::ostream&) const override;

private:
    virtual bool equal(const Type*) const override;
    virtual uint64_t vhash() const override { return this->gid(); }
    virtual const Type* vrebuild(TypeTable&, Types) const override;
    virtual const Type* vreduce(int, const Type*, Type2Type&) const override;

    friend class TypeTable;
};

class TypeError : public Type {
private:
    TypeError(TypeTable& table)
        : Type(table, Node_TypeError, {})
    {}

public:
    virtual std::ostream& stream(std::ostream&) const override;

private:
    virtual const Type* vrebuild(TypeTable& to, Types ops) const override;
    virtual const Type* vreduce(int, const Type*, Type2Type&) const override;

    friend class TypeTable;
};

class InferError : public Type {
    InferError(TypeTable& typetable, const Type* dst, const Type* src)
        : Type(typetable, Tag_infer_error, {dst, src})
    {}

    const Type* dst() const { return op(0); }
    const Type* src() const { return op(1); }
    virtual std::ostream& stream(std::ostream&) const override;

private:
    virtual const Type* vrebuild(TypeTable&, Types) const override;
    virtual const Type* vreduce(int, const Type*, Type2Type&) const override;

    friend class TypeTable;
};

inline bool is_no_ret_or_type_error(const Type* t) {
    return t->isa<NoRetType>() || t->isa<TypeError>();
}

inline bool is_unit(const Type* t) {
    return t->isa<TupleType>() && t->num_ops() == 0;
}

//------------------------------------------------------------------------------

}

#endif
