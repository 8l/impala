#ifndef IMPALA_IMPALA_H
#define IMPALA_IMPALA_H

#include <iostream>
#include <memory>
#include <string>
#include <vector>

#include "thorin/world.h"
#include "thorin/util/stream.h"

#include "impala/token.h"
#include "impala/sema/typetable.h"

namespace impala {

bool& fancy();

class ASTNode;
class Item;
class Module;
typedef std::vector<std::unique_ptr<const Item>> Items;

void init();
void destroy();

struct Init {
    Init(std::string module_name)
        : world(std::move(module_name))
    {
        init();
    }
    ~Init() { destroy(); }

    thorin::World world;
    std::unique_ptr<TypeTable> typetable;
};

void parse(Items&, std::istream&, const char*);
void name_analysis(const Module*);
void type_inference(Init&, const Module*);
void type_analysis(const Module*, bool nossa);
//void borrow_check(const ModContents*);
void check(Init&, const Module*, bool nossa);
void emit(thorin::World&, const Module*);

enum class Prec {
    Bottom,
    Assign = Bottom,
    OrOr, AndAnd,
    Rel,
    Or, Xor, And,
    Shift, Add, Mul,
    As,
    Unary,
};

struct PrecTable {
    static Prec infix[Token::Num];
    static Prec infix_l(int tag) { return infix[tag]; }
    static Prec infix_r(int tag) { return Prec(int(infix[tag])+1); }

private:
    static void init();

    friend void init();
};

extern int global_num_warnings;
extern int global_num_errors;

int num_warnings();
int num_errors();

template<typename... Args>
std::ostream& warning(const thorin::Location& loc, const char* fmt, Args... args) {
    ++global_num_warnings;
    thorin::streamf(std::cerr, "{}: warning: ", loc);
    return thorin::streamf(std::cerr, fmt, args...) << std::endl;;
}

template<typename... Args>
std::ostream& error(const thorin::Location& loc, const char* fmt, Args... args) {
    ++global_num_errors;
    thorin::streamf(std::cerr, "{}: error: ", loc);
    return thorin::streamf(std::cerr, fmt, args...) << std::endl;;
}

}

#endif
