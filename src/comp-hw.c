#include "def.h"
#include "dev.h"
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <sys/cdefs.h>

/**
 * AST
 */
#define Ast(x) hw_ASTExprTAG_##x
enum hw_ASTExprTAG {
     Ast(BinaryOP)
   , Ast(UnaryOP)
   , Ast(Assign)
   , Ast(Symbol)
   , Ast(Literal)
};
#undef Ast

#define AstDef(name) hw_ASTExpr_##name
#define Ast(name, ...)\
    typedef struct AstDef(name) AstDef(name);\
    struct AstDef(name) {__VA_ARGS__}

typedef struct hw_ASTExpr hw_ASTExpr;
typedef struct hw_ASTStmt hw_ASTStmt;
typedef struct hw_ASTDecl hw_ASTDecl;

Ast(BinaryOP, hw_u32 expr_l, expr_r; hw_byte op;);
Ast(UnaryOP, hw_u32 expr_r; hw_byte op; );
Ast(Expr, hw_u32 expr;);
Ast(Symbol, hw_u32 tok;);
Ast(Literal, hw_u32 lit;);

struct hw_ASTExpr {
    enum hw_ASTExprTAG tag;
    union {
        AstDef(BinaryOP) BinaryOP;
        AstDef(UnaryOP)  UnaryOP;
        AstDef(Symbol)   Symbol;
        AstDef(Literal)  Literal;
        AstDef(Expr)     Expr;
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
    hw_byte tag;
    union {
      hw_ASTStmt_expr *expr;
      hw_ASTStmt_block *block;
      hw_ASTStmt_if *condblock;
      hw_ASTStmt_while *whileblock;
    }as;
};

#undef Ast
#undef AstDef

/**
 *  Keyword Tokens
 */
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

/**
 * Types
 */

typedef struct hw_TokList hw_TokList;
struct hw_TokList {
    hw_byte *tok;
    hw_u32  *pos;
    hw_u32  lenUsed;
    hw_u32  len;
};

#define HW_PARSER_ERRORMSG_MAX 256
typedef struct hw_ParserHW hw_ParserHW;
struct hw_ParserHW {
    hw_TokList          *toklist;
    hw_byte const       *source;
    HW_ARR(hw_ASTExpr)  *expr_pool;
    hw_VarList          *lit_pool;
    hw_u32              source_size;
    hw_u32              tokat;
    hw_u16              emsize;
    hw_byte             error_msg[HW_PARSER_ERRORMSG_MAX];
};

typedef struct hw_CompilerHW hw_CompilerHW;
struct hw_CompilerHW {
    hw_State        *child;
    hw_State        *parent;
    hw_ParserHW      parser;
};

/************************************************************************
 ************************************************************************/

static hw_TokList *hw_TokList_new(hw_State *hw, hw_u32 len)
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
    t->lenUsed += 1;
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
        if(0 == hw_ptrcmp(keyword_info[i].keyword
                   , keyword_info[i].keyword_size
                   , lt.start, lt.size)) { return keyword_info[i].tok; }
    }

    return HW_LEXTOKEN_SYMBOL;
}

static void hw_TokList_generate(hw_State *hw, hw_TokList **tl 
                                 , hw_byte const *source, hw_u32 size) {
    hw_TokList *t = *tl;

    hw_Lexer lexer;
    hw_Lexer_start(&lexer, source, size);
    do {
        hw_Lexer_next(&lexer);
        if(lexer.token.type == HW_LEXTOKEN_SYMBOL) { 
            lexer.token.type = _try_promote_symb_to_keyword(lexer.token); 
        } else if(lexer.token.type == HW_LEXTOKEN_DOUBLE_QUOTE) {
            HW_ASSERTEX(hw_Lexer_convert_dq_string(&lexer), "%.*s"
                , (int)lexer.token.size + 1, lexer.token.start);
        }
        hw_TokList_push(hw, t, lexer.token.type
                             , lexer.token.start - lexer.begin);
    } while ((lexer.token.type != HW_LEXTOKEN_END_OF_SOURCE)
          && (lexer.token.type != HW_LEXTOKEN_ERROR));
    if(t->lenUsed) t->lenUsed -= 1; // EOF WRAP
    *tl = t;
}

#define hw_TokList_gettoktype(t, i) ((t)->tok[i])
#define hw_TokList_gettokval(tl, source, i) (source + (tl)->pos[i])
#define hw_TokList_gettokpos(tl, i) ((tl)->pos[i])
/************************************************************************
 *                     Haywire Source Compiler Private
 ************************************************************************/

hw_u32 hw_TokList_gettok_len(hw_TokList const *tl
                           , hw_u32 index, hw_u32 source_size)
{
    if(!tl->lenUsed) return 0;
    if(index < tl->lenUsed-1) return tl->pos[index+1] - tl->pos[index];
    return source_size - tl->pos[index];
}


static void _list_all_tok(hw_TokList const *t, hw_byte const *source, hw_u32 source_size)
{
    HW_LOG("Total Tokens %u", t->lenUsed);
    for (hw_u32 i = 0; i < t->lenUsed; i++) {
        hw_byte const *s = hw_TokList_gettokval(t, source, i);
        hw_u32 const s_size = hw_TokList_gettok_len(t, i, source_size);
        if(t->tok[i] < HW_LEXTOKEN_TOTAL) {
            hw_CStr tname = hw_LexToken_get_name(t->tok[i]);
            hw_logp("%3u:pos %u|> %.*s := \"%.*s\""
                , i
                , t->pos[i]
                , (int)tname.len, tname.data
                , s_size, s);
        }
        if(t->tok[i] == HW_LEXTOKEN_NUMBER) {
            hw_int val;
            hw_strto_int(&val, s, s_size);
            hw_logp(" -> parse: %lu", val);
        } else if(t->tok[i] == HW_LEXTOKEN_FLOAT) {
            hw_float val;
            hw_strto_float(&val, s, s_size);
            hw_logp(" -> parse: %lf, strtof: %lf", val, strtod((void*)s, NULL));
        }
        hw_logp("\n");
    }
}

enum hw_OPERATION_PRECIDENSE {
    hw_OP_PREC_ADD
  , hw_OP_PREC_SUB
  , hw_OP_PREC_MUL
  , hw_OP_PREC_DIV
  , hw_OP_PREC_GRP
};

static hw_bool is_operator(hw_byte tok)
{
    switch (tok) {
        break; case HW_LEXTOKEN_PLUS: return HW_TRUE;
        break; case HW_LEXTOKEN_ASTER: return HW_TRUE;
        break; case HW_LEXTOKEN_MINUS: return HW_TRUE;
        break; case HW_LEXTOKEN_SLASH: return HW_TRUE;
    }
    return HW_FALSE;
}

static hw_u32 hw_ParserHW_lit(hw_State *hw, hw_ParserHW *parser
                     , hw_Var value, hw_byte type) {
    hw_VarList_push_shallow(hw, &parser->lit_pool, value, type);
    return parser->lit_pool->lenUsed-1;
}

static hw_u32 hw_ParserHW_expr(hw_State *hw, hw_ParserHW *parser)
{
    HW_ARR_PUSHINC(hw, parser->expr_pool);
    return parser->expr_pool->lenUsed-1;
}

#define Expr(name, ...) hw_ASTExpr_as_##name(hw, parser, __VA_ARGS__)
    
#define ExprImpl(name, args, ...)\
    static hw_u32 hw_ASTExpr_as_##name(hw_State *hw, hw_ParserHW *parser, HW_MACRO_EXPAND args ) {\
      hw_u32 const id = hw_ParserHW_expr(hw, parser);                   \
      hw_ASTExpr *self = parser->expr_pool->data + id;                  \
      self->tag = hw_ASTExprTAG_##name;                                 \
      self->as.name = (hw_ASTExpr_##name){__VA_ARGS__};                 \
      return id; }

ExprImpl(Literal, (hw_Var val, hw_byte type)
    , .lit = hw_ParserHW_lit(hw, parser, val, type))
ExprImpl(BinaryOP, (hw_u32 l_expr, hw_u32 r_expr, hw_byte op), .expr_l = l_expr,
                                                               .expr_r = r_expr,
                                                               .op = op)
ExprImpl(Symbol, (hw_u32 symbol_at), .tok = symbol_at)
#undef ExprImpl

/************************************************************************
 *                            Parser Private                            *
 ************************************************************************/

/******************************** Helpers *******************************/
#define _is_whitespace(ttype) (ttype == HW_LEXTOKEN_SPACE\
                         ||ttype == HW_LEXTOKEN_NEWLINE\
                         ||ttype == HW_LEXTOKEN_TABSPACE)

#define parsefn(name) _parse_##name
#define defn_parse(name)\
  static hw_u32 parsefn(name)(hw_State *hw, hw_ParserHW *parser) {\
      hw_logp(#name ": ");\
      _prnt_parser_tok(parser);

#define parse_call(name)\
     parsefn(name)(hw, parser); { if(parser->emsize) { return (hw_u32)-1; } }

#define parse_set_fail(...) \
    { _parser_error(parser, __VA_ARGS__); return (hw_u32)(-1); }

#define parse_set_fail_on(cond, ...)\
    { if(cond) { parse_set_fail(__VA_ARGS__); }

#define _parser_softassert_err(parser) { if((parser)->emsize) return ; }
#define _currenttok() (parser->toklist->tok[parser->tokat])

static void _parser_error(hw_ParserHW *parser, const char *restrict format, ...)
__printflike(2, 3)
{
    va_list vargs;
    va_start(vargs, format);
    parser->emsize = vsnprintf((char *)parser->error_msg
        , HW_PARSER_ERRORMSG_MAX-2, format, vargs);
    parser->error_msg[parser->emsize+1] = '\0';
    va_end(vargs);
}

inline static hw_LexToken _maketok(hw_ParserHW *parser)
{
    if(parser->tokat >= parser->toklist->lenUsed) { 
        return (hw_LexToken) {
            .type = HW_LEXTOKEN_END_OF_SOURCE
          , .size = 0
          , .start = NULL
        };
    }
    
    return (hw_LexToken){
        .type = parser->toklist->tok[parser->tokat]
      , .size = hw_TokList_gettok_len(
                  parser->toklist, parser->tokat, parser->source_size)
      , .start = hw_TokList_gettokval(
                    parser->toklist, parser->source, parser->tokat)
    };   
}

static void _prnt_parser_tok(hw_ParserHW *parser)
{
    if(parser->tokat >= parser->toklist->lenUsed) {
        HW_LOG("INVALID TOK%c", ' '); return; 
    }
    hw_LexToken tk = _maketok(parser);
    hw_CStr toktname = hw_LexToken_get_name(_currenttok());
    hw_loglnp("Token(%.*s): '%.*s'"
        , (int)toktname.len, toktname.data
        , tk.size, tk.start);
}

static void _next_tok(hw_ParserHW *parser)
{
    parser->tokat += 1;
    while (parser->tokat < parser->toklist->lenUsed 
    && _is_whitespace(parser->toklist->tok[parser->tokat])) {parser->tokat++;}
}

hw_bool _next_tok_expect(hw_ParserHW *parser, hw_byte expect)
{
    _next_tok(parser);
    if(expect != _currenttok()) {
        _parser_error(parser, "unexpected token");
        return HW_FALSE;
    }
    return HW_TRUE;
}

/****************************** Parser Eval ****************************/

static hw_u32 parsefn(expr_expression)(hw_State *hw, hw_ParserHW* parser);

defn_parse(expr_factor) 
    hw_u32 node = UINT32_MAX;
    switch (_currenttok()) {
        break; case HW_LEXTOKEN_NUMBER: {
            hw_LexToken tokn = _maketok(parser);
            hw_Var value;
            hw_strto_int(&value.as_int, tokn.start, tokn.size);
            node = Expr(Literal, value, hw_TypeID_int);
            _next_tok(parser);
        }
        break; case HW_LEXTOKEN_SYMBOL: {
            node = Expr(Symbol, parser->tokat);
            _next_tok(parser);
        }
        break; case HW_LEXTOKEN_PAREN_LEFT: {
            _next_tok(parser);
            node = parse_call(expr_expression);
            if(_currenttok() != HW_LEXTOKEN_PAREN_RIGHT) {
                parse_set_fail("Expected ')'");
            }
            _next_tok(parser);
        }
        break; default: 
            parse_set_fail("Expected Literal");
            break;
    }
    return node;
}

defn_parse(expr_term) 
    hw_u32 node_left = parse_call(expr_factor);    
    while(_currenttok() == HW_LEXTOKEN_ASTER
        ||_currenttok() == HW_LEXTOKEN_SLASH) {
        char op = _currenttok();
        _next_tok(parser);
        hw_u32 node_right = parse_call(expr_factor);
        node_left = Expr(BinaryOP, node_left, node_right, op);
    }
    return node_left;
}

defn_parse(expr_expression) 
    hw_u32 node_left = parse_call(expr_term);
    while(_currenttok() == HW_LEXTOKEN_PLUS
        ||_currenttok() == HW_LEXTOKEN_MINUS) {
        char op = _currenttok();
        _next_tok(parser);
        hw_u32 node_right = parse_call(expr_term);
        node_left = Expr(BinaryOP, node_left, node_right, op);
    }
    return node_left;
}

#undef Expr
#undef defn_parse
#undef parse_call
#undef parse_fail
#undef parse_fail_on

/************************************************************************
 *                     Haywire Source Compiler Public API
 ************************************************************************/

/********************************* Parser *******************************/
/************************************************************************/

void hw_ParserHW_printerr(hw_ParserHW *parser)
{
    if(!parser->emsize) { hw_logp("Success\n"); return; }
    hw_u32 tokstart = hw_TokList_gettokpos(parser->toklist, parser->tokat);
    hw_u32 toksize = hw_TokList_gettok_len(
        parser->toklist, parser->tokat, parser->source_size);
    hw_loglnp("Error: %.*s\n", parser->emsize, parser->error_msg);
    hw_debug_print_context(parser->source, parser->source_size, tokstart
                                , toksize);
}

void hw_ParserHW_new(hw_State *hw, hw_ParserHW *self)
{
    memset(self, 0, sizeof(*self));
    self->toklist = hw_TokList_new(hw, 128);
    self->lit_pool = hw_VarList_new(hw, 32);
    HW_ARR_NEW(hw, self->expr_pool, 32);
}

void hw_ParserHW_delete(hw_State *hw, hw_ParserHW *self)
{
    hw_TokList_delete(hw, self->toklist);
    hw_VarList_delete(hw, self->lit_pool);
    HW_ARR_DELETE(hw, self->expr_pool);
}

inline void hw_debug_print_indent(hw_u16 count)
{
    for (hw_u16 i = 0; i < count; i++) {
        fputs("    ", stdout);
    }
}

#define _getexprnode(parser, id) (parser->expr_pool->data[id])
#define _getvar(parser, id)      (parser->lit_pool->data[id])
#define _getvartid(parser, id)   (parser->lit_pool->tid[id])

void hw_debug_print_astexpr(hw_State *hw, hw_ParserHW *parser, hw_u32 node, hw_u32 depth)
{
    #define prnt(fmt, ...) \
        {\
            hw_debug_print_indent(depth);\
            hw_loglnp(fmt, __VA_ARGS__);\
        }

    if(node >= parser->expr_pool->lenUsed) {
        prnt("ERROR: INVALID NODE ID %u", node);         
    }
    
    hw_ASTExpr expr = _getexprnode(parser, node);
    switch (expr.tag) {
        break; case hw_ASTExprTAG_Literal:
            prnt("Literal: (%u)", expr.as.Literal.lit);
            hw_debug_print_var(hw, _getvar(parser, expr.as.Literal.lit)
                                 , _getvartid(parser, expr.as.Literal.lit));
        break; case hw_ASTExprTAG_BinaryOP:
            prnt("BinaryOP: %s", hw_LexToken_get_name(
                        expr.as.BinaryOP.op).data);
            hw_debug_print_astexpr(hw, parser
                , expr.as.BinaryOP.expr_l, depth+1);
            hw_debug_print_astexpr(hw, parser
                , expr.as.BinaryOP.expr_r, depth+1);
        break; case hw_ASTExprTAG_Symbol: {
            hw_byte const *symb = hw_TokList_gettokval(parser->toklist
                , parser->source, expr.as.Symbol.tok);
            hw_u32 symb_sz = hw_TokList_gettok_len(parser->toklist
                , expr.as.Symbol.tok, parser->source_size);
            prnt("Symbol: %.*s", symb_sz, symb);
        }
        break; default:
            prnt("Unknown Tag %u", expr.tag);
    }
}

void hw_debug_print_parserinfo(hw_ParserHW *parser)
{
    hw_loglnp("Parser Info", "");
    hw_loglnp("Error: %.*s", parser->emsize, parser->error_msg);
    hw_loglnp("ExprNode Count: %u", parser->expr_pool->lenUsed);
    hw_loglnp("Literal  Count: %u", parser->lit_pool->lenUsed);
    hw_loglnp("Token Count %u", parser->toklist->lenUsed);
    hw_loglnp("Token At %u", parser->tokat);
    hw_loglnp("Source Size %u", parser->source_size);
    hw_loglnp("Source:\n %.*s", parser->source_size, parser->source);
}

void hw_ParserHW_generate_ast(hw_State *hw, hw_ParserHW *parser)
{
    if(_is_whitespace(_currenttok())) {
        _next_tok(parser);
    }
    hw_u32 node = _parse_expr_expression(hw, parser);
    if(parser->emsize) {
        hw_ParserHW_printerr(parser);
        return;
    }
    hw_debug_print_parserinfo(parser);
    hw_loglnp("AST: %u nodes", parser->expr_pool->lenUsed);
    hw_debug_print_astexpr(hw, parser, node, 1);
}

hw_bool hw_ParserHW_build(hw_State *hw, hw_ParserHW *parser
                            , hw_byte const *source, hw_u32 source_size) {
    parser->source = source;
    parser->source_size = source_size;
    hw_TokList_generate(hw, &parser->toklist, source, source_size);
    hw_ParserHW_generate_ast(hw, parser);
    return HW_TRUE;
}

/**************************** Haywire Compiler **************************/
/************************************************************************/
hw_CompilerHW *hw_comphw_new(hw_State *parent)
{
    hw_State *child = hw_State_new_default(parent);
    hw_CompilerHW *comp = HW_THREAD_ALLOC(child, sizeof(*comp));
    hw_ParserHW_new(child, &comp->parser);
    comp->child = child;
    comp->parent = parent;
    return comp;
}

void hw_comphw_delete(hw_CompilerHW *comp)
{
    hw_ParserHW_delete(comp->child, &comp->parser);
    hw_State *hw = comp->child;
    HW_THREAD_FREE(hw, comp);
    hw_State_delete(hw);
}

hw_bool hw_comphw_setsource(hw_CompilerHW *comp, hw_byte const *source, hw_u32 size)
{
    return hw_ParserHW_build(comp->child, &comp->parser, source, size);
}

hw_VarP hw_comphw_main(hw_State *parent, hw_byte const *source)
{
    hw_CompilerHW *comp = hw_comphw_new(parent);
    hw_comphw_setsource(comp, source, strlen((void*)source));


    hw_comphw_delete(comp);
    return HW_VARP_NIL();
}

