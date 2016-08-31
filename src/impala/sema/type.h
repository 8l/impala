#ifndef IMPALA_SEMA_TYPE_H
#define IMPALA_SEMA_TYPE_H

#include "thorin/util/array.h"
#include "thorin/util/cast.h"
#include "thorin/util/hash.h"
#include "thorin/util/stream.h"
#include "thorin/util/location.h"

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
    Tag_indefinite_array,
    Tag_lambda,
    Tag_mut_ptr,
    Tag_noret,
    Tag_owned_ptr,
    Tag_pi,
    Tag_simd,
    Tag_tuple,
    Tag_sigma,
    Tag_star,
    Tag_unknown,
    Tag_var,
};

enum PrimTypeTag {
#define IMPALA_TYPE(itype, atype) PrimType_##itype = Tag_##itype,
#include "impala/tokenlist.h"
};

class StructDecl;
template<class T> using ArrayRef = thorin::ArrayRef<T>;
template<class T> using Array    = thorin::Array<T>;

static const int Node_App        = impala::Tag_app;
static const int Node_Lambda     = impala::Tag_lambda;
static const int Node_Pi         = impala::Tag_pi;
static const int Node_Tuple      = impala::Tag_tuple;
static const int Node_Error      = impala::Tag_error;
static const int Node_Sigma      = impala::Tag_sigma;
static const int Node_Star       = impala::Tag_star;
static const int Node_Var        = impala::Tag_var;

#define HENK_TABLE_NAME  table
#define HENK_TABLE_TYPE  TypeTable
#include "thorin/henk.h"

//------------------------------------------------------------------------------

class Type : public Def {
protected:
    Type(TypeTable& table, int tag, Defs ops);
};

/// Primitive type.
class PrimType : public Type {
private:
    PrimType(TypeTable& table, PrimTypeTag tag)
        : Type(table, (Tag) tag, {})
    {}

public:
    PrimTypeTag primtype_tag() const { return (PrimTypeTag) tag(); }

    virtual std::ostream& stream(std::ostream&) const override;

private:
    virtual const Def* vrebuild(TypeTable&, Defs) const override;
    virtual const Def* vreduce(int, const Def*, Def2Def&) const override;

    friend class TypeTable;
};

bool is(const Def*, PrimTypeTag tag);
#define IMPALA_TYPE(itype, atype) inline bool is_##itype(const Def* t) { return is(t, PrimType_##itype); }
#include "impala/tokenlist.h"
inline bool is_float(const Def* t) { return             is_f16(t) || is_f32(t) || is_f64(t); }
inline bool is_int  (const Def* t) { return is_i8(t) || is_i16(t) || is_i32(t) || is_i64(t)
                                          || is_u8(t) || is_u16(t) || is_u32(t) || is_u64(t); }
bool is_void(const Def*);
bool is_subtype(const Def* dst, const Def* src);

//------------------------------------------------------------------------------

/// Pointer @p Def.
class PtrType : public Type {
protected:
    PtrType(TypeTable& table, int tag, const Def* referenced_type, int addr_space)
        : Type(table, tag, {referenced_type})
        , addr_space_(addr_space)
    {}

    std::ostream& stream_ptr_type(std::ostream&, std::string prefix, int addr_space, const Def* ref_type) const;

public:
    const Def* referenced_type() const { return op(0); }
    int addr_space() const { return addr_space_; }

    virtual std::ostream& stream(std::ostream&) const override;
    virtual uint64_t vhash() const override;
    virtual bool equal(const Def* other) const override;
    virtual std::string prefix() const = 0;

private:
    int addr_space_;

    friend class TypeTable;
};

class BorrowedPtrType : public PtrType {
public:
    BorrowedPtrType(TypeTable& table, const Def* referenced_type, int addr_space)
        : PtrType(table, Tag_borrowed_ptr, referenced_type, addr_space)
    {}

    virtual std::string prefix() const override { return "&"; }

private:
    virtual const Def* vrebuild(TypeTable&, Defs) const override;
    virtual const Def* vreduce(int, const Def*, Def2Def&) const override;
};

class MutPtrType : public PtrType {
public:
    MutPtrType(TypeTable& table, const Def* referenced_type, int addr_space)
        : PtrType(table, Tag_mut_ptr, referenced_type, addr_space)
    {}

    virtual std::string prefix() const override { return "&mut"; }

private:
    virtual const Def* vrebuild(TypeTable&, Defs) const override;
    virtual const Def* vreduce(int, const Def*, Def2Def&) const override;
};

class OwnedPtrType : public PtrType {
public:
    OwnedPtrType(TypeTable& table, const Def* referenced_type, int addr_space)
        : PtrType(table, Tag_owned_ptr, referenced_type, addr_space)
    {}

    virtual std::string prefix() const override { return "~"; }

private:
    virtual const Def* vrebuild(TypeTable&, Defs) const override;
    virtual const Def* vreduce(int, const Def*, Def2Def&) const override;
};

//------------------------------------------------------------------------------

class FnType : public Type {
private:
    FnType(TypeTable& table, Defs ops)
        : Type(table, Tag_fn, ops)
    {
        ++order_;
    }

public:
    const Def* return_type() const;
    bool is_returning() const;
    virtual std::ostream& stream(std::ostream&) const override;

private:
    virtual const Def* vrebuild(TypeTable&, Defs) const override;
    virtual const Def* vreduce(int, const Def*, Def2Def&) const override;

    friend class TypeTable;
};

class ArrayType : public Type {
protected:
    ArrayType(TypeTable& table, int tag, const Def* elem_type)
        : Type(table, tag, {elem_type})
    {}

public:
    const Def* elem_type() const { return op(0); }
};

class IndefiniteArrayType : public ArrayType {
public:
    IndefiniteArrayType(TypeTable& table, const Def* elem_type)
        : ArrayType(table, Tag_indefinite_array, elem_type)
    {}

    virtual std::ostream& stream(std::ostream&) const override;

private:
    virtual const Def* vrebuild(TypeTable&, Defs) const override;
    virtual const Def* vreduce(int, const Def*, Def2Def&) const override;

    friend class TypeTable;
};

class DefiniteArrayType : public ArrayType {
public:
    DefiniteArrayType(TypeTable& table, const Def* elem_type, uint64_t dim)
        : ArrayType(table, Tag_definite_array, elem_type)
        , dim_(dim)
    {}

    uint64_t dim() const { return dim_; }
    virtual uint64_t vhash() const override { return thorin::hash_combine(Type::vhash(), dim()); }
    virtual bool equal(const Def* other) const override {
        return Def::equal(other) && this->dim() == other->as<DefiniteArrayType>()->dim();
    }

    virtual std::ostream& stream(std::ostream&) const override;

private:
    virtual const Def* vrebuild(TypeTable&, Defs) const override;
    virtual const Def* vreduce(int, const Def*, Def2Def&) const override;

    uint64_t dim_;

    friend class TypeTable;
};

class SimdType : public ArrayType {
public:
    SimdType(TypeTable& table, const Def* elem_type, uint64_t dim)
        : ArrayType(table, Tag_simd, elem_type)
        , dim_(dim)
    {}

    uint64_t dim() const { return dim_; }
    virtual uint64_t vhash() const override { return thorin::hash_combine(Type::vhash(), dim()); }
    virtual bool equal(const Def* other) const override {
        return Def::equal(other) && this->dim() == other->as<SimdType>()->dim();
    }

    virtual std::ostream& stream(std::ostream&) const override;

private:
    virtual const Def* vrebuild(TypeTable&, Defs) const override;
    virtual const Def* vreduce(int, const Def*, Def2Def&) const override;

    uint64_t dim_;

    friend class TypeTable;
};

class NoRetType : public Type {
private:
    NoRetType(TypeTable& table)
        : Type(table, Tag_noret, {})
    {}

public:
    virtual std::ostream& stream(std::ostream&) const override;

private:
    virtual const Def* vrebuild(TypeTable&, Defs) const override;
    virtual const Def* vreduce(int, const Def*, Def2Def&) const override;

    friend class TypeTable;
};

class UnknownType : public Type {
private:
    UnknownType(TypeTable& table)
        : Type(table, Tag_unknown, {})
    {
        known_ = false;
    }

public:
    virtual std::ostream& stream(std::ostream&) const override;

private:
    virtual bool equal(const Def*) const override;
    virtual uint64_t vhash() const override { return thorin::hash_value(this->gid()); }
    virtual const Def* vrebuild(TypeTable&, Defs) const override;
    virtual const Def* vreduce(int, const Def*, Def2Def&) const override;

    friend class TypeTable;
};

//------------------------------------------------------------------------------

}

#endif
