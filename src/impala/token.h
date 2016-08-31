#ifndef IMPALA_TOKEN_H
#define IMPALA_TOKEN_H

#include <ostream>
#include <string>

#include "thorin/enums.h"
#include "thorin/util/assert.h"
#include "thorin/util/location.h"

#include "impala/symbol.h"

namespace impala {

class Token : public thorin::HasLocation {
public:
    enum Tag {
        // !!! DO NOT CHANGE THIS ORDER !!!
        // add prefix and postfix tokens manually in order to avoid duplicates in the enum
#define IMPALA_INFIX(     tok, t_str, r, l) tok,
#define IMPALA_INFIX_ASGN(tok, t_str, r, l) tok,
#define IMPALA_KEY(       tok, t_str)       tok,
#define IMPALA_MISC(      tok, t_str)       tok,
#define IMPALA_LIT(       tok, t)           LIT_##tok,
#define IMPALA_TYPE(itype, atype)           TYPE_##itype,
#include "impala/tokenlist.h"

        // manually insert missing unary prefix/postfix types
        TILDE, NOT, INC, DEC, RUN, HLT, DOT,
        // these do ont appear in impala/tokenlist.h -- they are too special
        MUT, ID, END_OF_FILE,
        TYPE_app, TYPE_generic, TYPE_genericref, TYPE_error, TYPE_tuple, TYPE_definite_array, TYPE_indefinite_array,
        LIT_char, LIT_str,
        NUM_TOKENS
    };

    struct TagHash {
        uint64_t operator()(Tag tag) const { return thorin::hash_value((int) tag); }
    };

    Token() {}
    /// Create an operator token
    Token(const thorin::Location& loc, Tag tok);
    /// Create an identifier or a keyword (depends on \p str)
    Token(const thorin::Location& loc, const std::string& str);
    /// Create a literal
    Token(const thorin::Location& loc, Tag type, const std::string& str);

    Symbol symbol() const { return symbol_; }
    thorin::Box box() const { return box_; }
    Tag tag() const { return tag_; }
    operator Tag() const { return tag_; }

    enum Op {
        NONE    = 0,
        PREFIX  = 1,
        INFIX   = 2,
        POSTFIX = 4,
        ASGN_OP = 8
    };

    bool is_stmt_like() const { return tag() == L_BRACE || tag() == IF || tag() == FOR || tag() == WHILE || tag() == WITH; }
    bool is_prefix()    const { return is_prefix(tag_); }
    bool is_infix()     const { return is_infix(tag_); }
    bool is_postfix()   const { return is_postfix(tag_); }
    bool is_assign()    const { return is_assign(tag_); }
    bool is_op()        const { return is_op(tag_); }

    static Tag sym2lit(Symbol sym);
    static Tag sym2flit(Symbol sym);
    static bool is_prefix(Tag tag)  { return (tok2op_[tag] &  PREFIX) != 0; }
    static bool is_infix(Tag tag)   { return (tok2op_[tag] &   INFIX) != 0; }
    static bool is_postfix(Tag tag) { return (tok2op_[tag] & POSTFIX) != 0; }
    static bool is_assign(Tag tag)  { return (tok2op_[tag] & ASGN_OP) != 0; }
    static bool is_op(Tag tag)      { return is_prefix(tag) || is_infix(tag) || is_postfix(tag); }
    static bool is_rel(Tag tag);
    static Tag separate_assign(Tag tag);
    static int to_binop(Tag tag);
    static thorin::ArithOpTag to_arithop(Tag tag) { return (thorin::ArithOpTag) to_binop(tag); }
    static thorin::CmpTag     to_cmp    (Tag tag) { return (thorin::CmpTag)     to_binop(tag); }
    static const char* tok2str(Tag tag);

    bool operator==(const Token& t) const { return tag_ == t; }
    bool operator!=(const Token& t) const { return tag_ != t; }

private:
    static void init();
    static Symbol insert(Tag tok, const char* str);
    static void insert_key(Tag tok, const char* str);

    Symbol symbol_;
    Tag tag_;
    thorin::Box box_;

    static int tok2op_[NUM_TOKENS];
    static thorin::HashMap<Tag, const char*, TagHash> tok2str_;
    static thorin::HashMap<Tag, Symbol, TagHash> tok2sym_;
    static thorin::HashMap<Symbol, Tag> keywords_;
    static thorin::HashMap<Symbol, Tag> sym2lit_; ///< Table of \em all (including floating) suffixes for literals.
    static thorin::HashMap<Symbol, Tag> sym2flit_;///< Table of suffixes for \em floating point literals.

    friend void init();
    friend std::ostream& operator<<(std::ostream& os, const Token& tok);
    friend std::ostream& operator<<(std::ostream& os, const Tag&  tok);
};

typedef Token::Tag TokenTag;

//------------------------------------------------------------------------------

std::ostream& operator<<(std::ostream& os, const Token& tok);
std::ostream& operator<<(std::ostream& os, const TokenTag& tok);

//------------------------------------------------------------------------------

}

#endif
