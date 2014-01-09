#ifndef IMPALA_TYPE_H
#define IMPALA_TYPE_H

#include <iostream>
#include <unordered_set>

#include "thorin/node.h"
#include "thorin/util/hash.h"

#include "impala/token.h"

namespace thorin {
    class GenericMap;
    class Type;
    class World;
}

namespace impala {

class FnType;
class Fun;
class Generic;
class GenericRef;
class IdType;
class NoRet;
class Void;
class PrimType;
class Sema;
class TupleType;
class Type;
class TypeTable;
class TypeError;

//------------------------------------------------------------------------------

class GenericBuilder {
public:
    GenericBuilder(TypeTable& typetable)
        : typetable_(&typetable)
        , index_(0)
    {}

    size_t new_def();
    const Generic* use(size_t handle);
    const Generic* get(size_t handle) { assert(handle < index2generic_.size()); return index2generic_[handle]; }
    void pop(size_t num) { index2generic_.resize(index2generic_.size() - num); }
    TypeTable& typetable() const { return *typetable_; }

private:
    TypeTable* typetable_;
    size_t index_;
    typedef std::vector<const Generic*> Index2Generic;
    Index2Generic index2generic_;
};

//------------------------------------------------------------------------------

class GenericMap {
public:
    GenericMap() {}

    const Type*& operator [] (const Generic* generic) const;
    bool is_empty() const;
    const char* to_string() const;

private:
    mutable std::vector<const Type*> types_;
};

inline std::ostream& operator << (std::ostream& o, const GenericMap& map) { o << map.to_string(); return o; }

//------------------------------------------------------------------------------

class Type : public thorin::Node<Type> {
protected:
    Type(TypeTable& typetable, TokenKind kind, size_t size, bool is_generic, const std::string& name)
        : Node((int) kind, size, name)
        , typetable_(typetable)
        , is_generic_(is_generic)
    {}

public:
    TokenKind kind() const { return (TokenKind) thorin::Node<Type>::kind(); }
    thorin::ArrayRef<const Type*> elems() const { return ops_ref<const Type*>(); }
    const Type* elem(size_t i) const { return elems()[i]; }
    virtual const Type* refine(const Sema&) const = 0;
    virtual const Type* specialize(const GenericMap& map) const = 0;
    virtual const thorin::Type* convert(thorin::World&) const = 0;
    bool is_bool() const;
    bool is_int() const;
    bool is_float() const;
    bool is_void() const { return isa<Void>() != nullptr; }
    bool is_noret() const { return isa<NoRet>() != nullptr; }
    bool is_generic() const { return is_generic_; }
    bool check_with(const Type*) const;
    bool infer_with(GenericMap& map, const Type* type) const;
    void dump() const;
    void set(size_t i, const Type* t) { assert(!t->is_void() && !t->is_noret()); Node::set(i, t); }

protected:
    TypeTable& typetable_;
    bool is_generic_;
};

class TypeError : public Type {
private:
    TypeError(TypeTable& typetable) 
        : Type(typetable, Token::TYPE_error, 0, false, "<error>")
    {}

    virtual const Type* refine(const Sema&) const { return this; }
    virtual const Type* specialize(const GenericMap& map) const { return this; }
    virtual const thorin::Type* convert(thorin::World&) const { assert(false); return nullptr; }

    friend class TypeTable;
};

class Void : public Type {
private:
    Void(TypeTable& typetable) 
        : Type(typetable, Token::TYPE_void, 0, false, "void")
    {}

    virtual const Type* refine(const Sema&) const { return this; }
    virtual const Type* specialize(const GenericMap& map) const { return this; }
    virtual const thorin::Type* convert(thorin::World&) const { assert(false); return nullptr; }

    friend class TypeTable;
};

class NoRet : public Type {
private:
    NoRet(TypeTable& typetable) 
        : Type(typetable, Token::TYPE_noret, 0, false, "!")
    {}

    virtual const Type* refine(const Sema&) const { return this; }
    virtual const Type* specialize(const GenericMap& map) const { return this; }
    virtual const thorin::Type* convert(thorin::World&) const { assert(false); return nullptr; }

    friend class TypeTable;
};

class PrimType : public Type {
private:
    PrimType(TypeTable& typetable, TokenKind kind)
        : Type(typetable, kind, 0, false, "<primitive type>")
    {}

public:
    virtual const Type* refine(const Sema&) const { return this; }
    virtual const Type* specialize(const GenericMap&) const { return this; }
    virtual const thorin::Type* convert(thorin::World&) const;

    friend class TypeTable;
};

class ArrayType : public Type {
protected:
    ArrayType(TypeTable& typetable, TokenKind kind, const Type* elem_type)
        : Type(typetable, kind, 1, elem_type->is_generic(), "<array type>")
    {
        set(0, elem_type);
    }

public:
    const Type* elem_type() const { return elem(0); }
};

class IndefiniteArray : public ArrayType {
public:
    IndefiniteArray(TypeTable& typetable, const Type* elem_type)
        : ArrayType(typetable, Token::TYPE_definite_array, elem_type)
    {}

    virtual const Type* refine(const Sema&) const;
    virtual const Type* specialize(const GenericMap&) const;
    virtual const thorin::Type* convert(thorin::World&) const;

    friend class TypeTable;
};

class DefiniteArray : public ArrayType {
public:
    DefiniteArray(TypeTable& typetable, const Type* elem_type, thorin::u64 dim)
        : ArrayType(typetable, Token::TYPE_definite_array, elem_type)
        , dim_(dim)
    {}

    thorin::u64 dim() const { return dim_; }
    virtual const Type* refine(const Sema&) const;
    virtual const Type* specialize(const GenericMap&) const;
    virtual const thorin::Type* convert(thorin::World&) const;
    virtual size_t hash() const { return thorin::hash_combine(ArrayType::hash(), dim()); }
    virtual bool equals(const Node* other) const {
        return Type::equal(other) && this->dim() == other->as<DefiniteArray>()->dim();
    }

private:
    thorin::u64 dim_;

    friend class TypeTable;
};

class Generic : public Type {
private:
    Generic(TypeTable& typetable, size_t index)
        : Type(typetable, Token::TYPE_generic, 0, true, "<generic>")
        , index_(index)
    {}

public:
    size_t index() const { return index_; }
    static std::string to_string(size_t index);
    const GenericRef* genericref(const Fun*) const;
    virtual const Type* refine(const Sema&) const { assert(false); return this; }
    virtual const Type* specialize(const GenericMap& map) const {
        auto type = map[this]; assert(type != nullptr); return type;
    }
    virtual const thorin::Type* convert(thorin::World&) const;
    virtual size_t hash() const { return thorin::hash_combine(Type::hash(), index()); }
    virtual bool equal(const Node* other) const { 
        return Type::equal(other) ? index() == other->as<Generic>()->index() : false; 
    }

private:
    size_t index_;

    friend class TypeTable;
};

class GenericRef : public Type {
private:
    GenericRef(TypeTable& typetable, const Fun* fun, const Generic* generic)
        : Type(typetable, Token::TYPE_genericref, 1, true, "<generic>")
        , fun_(fun)
    {
        set(0, generic);
    }

    virtual const Type* refine(const Sema&) const { assert(false); return this; }
    virtual const Type* specialize(const GenericMap& map) const { return generic()->specialize(map); }
    virtual const thorin::Type* convert(thorin::World&) const;
    virtual size_t hash() const { 
        return thorin::hash_combine(thorin::hash_combine(Type::hash(), fun()), generic()); 
    }
    virtual bool equal(const Node* other) const { 
        if (Type::equal(other)) {
            auto genref = other->as<GenericRef>();
            return this->fun() == genref->fun() && this->generic() == genref->generic();
        }
        return false;
    }

public:
    const Fun* fun() const { return fun_; }
    const Generic* generic() const { return elem(0)->as<Generic>(); }
    static std::string to_string(size_t index);

private:
    const Fun* fun_;

    friend class TypeTable;
};

class CompoundType : public Type {
protected:
    CompoundType(TypeTable& typetable, TokenKind kind, thorin::ArrayRef<const Type*> elems, const std::string& name)
        : Type(typetable, kind, elems.size(), false, name)
    {
        size_t i = 0;
        for (auto elem : elems) {
            set(i++, elem);
            is_generic_ |= elem->is_generic();
        }
    }
};

class FnType : public CompoundType {
private:
    FnType(TypeTable& typetable, thorin::ArrayRef<const Type*> elems)
        : CompoundType(typetable, Token::FN, elems, "<function type>")
    {}

public:
    virtual const Type* refine(const Sema&) const;
    virtual const Type* specialize(const GenericMap& map) const;
    virtual const thorin::Type* convert(thorin::World&) const;
    const Type* return_type() const;

    friend class TypeTable;
};

class TupleType : public CompoundType {
private:
    TupleType(TypeTable& typetable, thorin::ArrayRef<const Type*> elems)
        : CompoundType(typetable, Token::TYPE_tuple, elems, "<tuple type>")
    {}

    virtual const Type* refine(const Sema& sema) const;
    virtual const Type* specialize(const GenericMap& map) const;
    virtual const thorin::Type* convert(thorin::World&) const;

    friend class TypeTable;
};

class IdType : public Type {
private:
    IdType(TypeTable& typetable, thorin::Symbol symbol)
        : Type(typetable, Token::TYPE_id, 0, false, symbol.str())
    {}
    virtual size_t hash() const { return thorin::hash_value(this); }
    virtual bool equal(const Node* other) const { return this == other; }
    virtual const Type* refine(const Sema&) const;
    virtual const Type* specialize(const GenericMap& map) const { assert(false); return nullptr; }
    virtual const thorin::Type* convert(thorin::World&) const { assert(false); return nullptr; }

    friend class TypeTable;
};

//------------------------------------------------------------------------------

struct TypeHash { size_t operator () (const Type* t) const { return t->hash(); } };
struct TypeEqual { bool operator () (const Type* t1, const Type* t2) const { return t1->equal(t2); } };
typedef std::unordered_set<const Type*, TypeHash, TypeEqual> TypeSet;

class TypeTable {
public:
    TypeTable();
    ~TypeTable();

    const TypeError* type_error() { return type_error_; }
    const NoRet* noret() { return noret_; }
    const Void* type_void() { return void_; }
    const PrimType* primtype(TokenKind kind);
    const DefiniteArray* definite_array(const Type* elem_type, thorin::u64 dim);
    const IndefiniteArray* indefinite_array(const Type* elem_type);
#define IMPALA_TYPE(itype, atype) const PrimType* type_##itype() { return itype##_; }
#include "impala/tokenlist.h"
    const FnType* fntype(thorin::ArrayRef<const Type*> elems);
    const TupleType* tupletype(thorin::ArrayRef<const Type*> elems);
    const Generic* generic(size_t index);
    const GenericRef* genericref(const Fun*, const Generic*);
    const IdType* idtype(thorin::Symbol);

private:
    const Type* unify_base(const Type* type);
    template<class T> const T* unify(const T* type) { return unify_base(type)->template as<T>(); }
    TypeTable& operator = (const TypeTable&);

    TypeSet types_;

#define IMPALA_TYPE(itype, atype) const PrimType* itype##_;
#include "impala/tokenlist.h"
    const TypeError* type_error_;
    const NoRet* noret_;
    const Void* void_;
};

//------------------------------------------------------------------------------

} // namespace impala

#endif