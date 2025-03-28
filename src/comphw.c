#include "dev.h"

/**
 *  Tok List
 */

typedef struct hw_TokList hw_TokList;
struct hw_TokList {
    hw_byte *tok;
    hw_u32  *pos;
    hw_u32  lenUsed;
    hw_u32  len;
};

enum hw_TokenKeyword {
    #define KEY(NAME) HW_LEXTOKENHW_KEYWORD_##NAME
    KEY(if) = HW_LEXTOKEN_TOTAL
  , KEY(for)
  , KEY(let) 
  , KEY(defn)
  , KEY(else)
  , KEY(loop)
  , KEY(symb)
  , KEY(const)
  , KEY(while)
  , KEY(return)
  , KEY(TOTAL)
  #undef KEY
};

hw_TokList *hw_TokList_new(hw_State *hw, hw_u32 len)
{
    hw_TokList *t = HW_THREAD_ALLOC(hw, sizeof(*t));
    t->len = len;
    t->lenUsed = 0;
    HW_STATIC_ASSERT(255 > HW_LEXTOKENHW_KEYWORD_TOTAL);
    t->tok = HW_THREAD_ALLOC(hw, sizeof(*t->tok) * len);
    t->pos = HW_THREAD_ALLOC(hw, sizeof(*t->pos) * len);
    return t;
}

void hw_TokList_delete(hw_State *hw, hw_TokList *t)
{
    HW_THREAD_FREE(hw, t->pos);
    HW_THREAD_FREE(hw, t->tok);
    HW_THREAD_FREE(hw, t);
}

static void hw_TokList_expand(hw_State *hw, hw_TokList *t, hw_u32 by)
{
    t->len += by;
    t->pos = HW_THREAD_REALLOC(hw, t->pos, t->len);
    t->tok = HW_THREAD_REALLOC(hw, t->tok, t->len);
}

static void hw_TokList_push(hw_State *hw, hw_TokList *t, hw_byte tok, hw_u32 pos)
{
    if(t->lenUsed >= t->len) { hw_TokList_expand(hw, t, t->len); }
    t->tok[t->lenUsed] = tok;
    t->pos[t->lenUsed] = pos;
}

static hw_byte _try_promote_symb_to_keyword(hw_LexToken lt)
{
    #define KEY(key) { .keyword = #key\
                          ,.keyword_size = sizeof(#key)-1\
                          ,.tok = HW_LEXTOKENHW_KEYWORD_##key }
    const struct {
        char const *keyword;
        hw_byte    keyword_size;
        hw_byte    tok;
    } keyword_info[] = {
        KEY(if)
      , KEY(for)
      , KEY(let) 
      , KEY(defn)
      , KEY(else)
      , KEY(loop)
      , KEY(symb)
      , KEY(const)
      , KEY(while)
      , KEY(return)
    };
    #define KEYWORDS  (sizeof(keyword_info)/sizeof(keyword_info[i]))
    #undef KEY

    for (size_t i = 0; i < KEYWORDS; i++) {
        if(hw_ptrcmp(keyword_info[i].keyword
                   , keyword_info[i].keyword_size
                   , lt.start, lt.size)) { return keyword_info[i].tok; }
    }

    return HW_LEXTOKEN_SYMBOL;
}

hw_TokList *hw_TokList_newFrom_String(hw_State *hw, hw_String const *source)
{
    hw_TokList *t = hw_TokList_new(hw, 128);
    hw_Lexer lexer;
    hw_Lexer_start(&lexer, source->data, source->lenUsed);
    do {
        hw_Lexer_next(&lexer);
        if(lexer.token.type == HW_LEXTOKEN_SYMBOL) { 
            lexer.token.type = _try_promote_symb_to_keyword(lexer.token); 
        }
        hw_TokList_push(hw, t, lexer.token.type
                             , lexer.token.start - lexer.begin);
    } while ((lexer.token.type != HW_LEXTOKEN_END_OF_SOURCE)
          && (lexer.token.type != HW_LEXTOKEN_ERROR));
    return t;
}

#define hw_Toklist_gettok(t, i) ((t)->tok[i])


/**
typedef enum JStarExprType {
    JSR_BINARY,
    JSR_UNARY,
    JSR_ASSIGN,
    JSR_NUMBER,
    JSR_BOOL,
    JSR_STRING,
    JSR_VAR,
    JSR_NULL,
    JSR_EXPR_LST,
    JSR_CALL,
    JSR_POWER,
    JSR_SUPER,
    JSR_PROPERTY_ACCESS,
    JSR_YIELD,
    JSR_LIST,
    JSR_TUPLE,
    JSR_TABLE,
    JSR_INDEX,
    JSR_TERNARY,
    JSR_COMPOUND_ASSIGN,
    JSR_FUN_LIT,
    JSR_SPREAD,
} JStarExprType;
**/
#define Ast(x) hw_ASTExprTAG_##x
enum hw_ASTExprTAG {
     Ast(BinaryOP)
   , Ast(UnaryOP)
   , Ast(Assign)
   , Ast(Number)
   , Ast(Symbol)
};
#undef Ast

#define AstDef(name) hw_ASTExpr_##name
#define Ast(name, ...)\
    typedef struct AstDef(name) AstDef(name);\
    struct AstDef(name) {__VA_ARGS__}

typedef struct hw_ASTExpr hw_ASTExpr;
typedef struct hw_ASTStmt hw_ASTStmt;
typedef struct hw_ASTDecl hw_ASTDecl;

Ast(BinaryOP, hw_byte op; hw_ASTExpr *l, *r;);
Ast(UnaryOP, hw_byte op; hw_ASTExpr *r;);
Ast(Assign, hw_ASTExpr *lval, *rval;);
Ast(StringLit, hw_u32 tok;);
Ast(Symbol, hw_u32 tok;);

struct hw_ASTExpr {
    enum hw_ASTExprTAG type;
    union {
        AstDef(BinaryOP)  bin_op;
        AstDef(UnaryOP)   uni_op;
        AstDef(Assign)    assign;
        AstDef(StringLit) string;
        AstDef(Symbol)    symbol;
    } as;
};

#undef Ast
#undef AstDef

enum hw_ASTStmtTAG {
    hw_ASTStmtTAG_declsymb,
    hw_ASTStmtTAG_declconst,
    hw_ASTStmtTAG_decldefn,
    hw_ASTStmtTAG_declvar,

    hw_ASTStmtTAG_if,
    hw_ASTStmtTAG_while,
    hw_ASTStmtTAG_block,
    hw_ASTStmtTAG_return,
    hw_ASTStmtTAG_expr,
};


#define AstDef(name) hw_ASTStmt_##name
#define Ast(name, ...)\
    typedef struct AstDef(name) AstDef(name);\
    struct AstDef(name) {__VA_ARGS__;}

Ast(if, hw_ASTExpr *cond;
            hw_ASTStmt *then_stmt, *else_stmt);

Ast(while, hw_ASTExpr *cond;
            hw_ASTStmt *body);

Ast(block, hw_ASTExpr *expr;
            hw_ASTStmt *then_stmt, *else_stmt);

Ast(return, hw_ASTExpr *expr;
            hw_ASTStmt *then_stmt, *else_stmt);

Ast(expr, hw_ASTExpr *expr;
            hw_ASTStmt *then_stmt, *else_stmt);


struct hw_ASTStmt {
    hw_byte type;
    union {
                
    }as;
};

#undef Ast
#undef AstDef


