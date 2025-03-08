#include "hw.h"
#include "hw_dev.h"

#define Ast(x) hw_Ast##x
#define type typedef struct

enum hw_Ast {
     Ast(Number)
   , Ast(Symbol)
   , Ast(fn_args)
};

typedef struct hw_astNode hw_astNode;
struct hw_astNode {
    hw_u32 token;
    hw_u32 at;
};

/**
number = "-"? [0-9]+
symbol = ("_" | [a-zA-Z])? [a-zA-Z0-9]+

fn_arg = ("mut" | "mut*")? symbol ":" symbol
fn_args = defn_arg | "," defn_arg
fn = "fn" "(" fn_args ")" { stmt* }

callfn = symbol "(" expr | ( "," expr ) ")"

expr = number
     | symbol
     | binary_op
     | fn
     | callfn

binary_op = expr ("+" | "-" | "/" | "*" | "%" |) expr

assign = symbol (":" symbol)? "=" expr
decl_symbol = "symb" assign
decl_var = "var" assign

stmt = decl_var 
     | decl_symbol
     | assign


baseExpr = decl_symbol+
 */
