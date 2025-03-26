#ifndef ZAKAROUF_HAYWIRE_DEV_H
#define ZAKAROUF_HAYWIRE_DEV_H

#include "def.h"
#include <sys/cdefs.h>
#include <sys/types.h>

/**
 * Section: Pre-Processor
 */
#define CAT2(X, Y) X##_##Y
#define CAT(X, Y) CAT2(X, Y)

#ifdef HW_DEBUG_CODE_ENABLE
#define HW_DEBUG(...) __VA_ARGS__
#else
#define HW_DEBUG(...)
#endif


#if defined (HW_DEBUG_CODE_ENABLE) || defined (HW_SAFE_GAURD_CODE_ENABLE)
#define HW_SAFE_GAURD(...) __VA_ARGS__
#else
#define HW_SAFE_GAURD(...)
#endif

/**
 */
#define HW_STR(s) (void *)s, (sizeof(s)-1)

/**
 * Section: Allocator
 */
#define HW_MALLOC(X)               malloc(X)
#define HW_FREE(X)                 free(X)
#define HW_REALLOC(X, NEW_SZ)      realloc(X, NEW_SZ)

/**
 */
#define HW_MIN(x, y) (x > y? y : x)
#define HW_MAX(x, y) (x > y? x : y)

/**
 * Section: Cast
 */
#define HW_CAST(T, ...) ((T)(__VA_ARGS__))

#define HW_CAST_SET(T, ptr, offset, val) \
    { *HW_CAST(T*, ptr + offset) = val; }

/**
 * Log & Exit
 */
void hw_exit(hw_int code, const char *msg, size_t const size);
void hw_logp(const char *fmt, ...) __attribute__ ((format (printf, 1, 2)));
void hw_logstr(const char *msg, size_t const);
#define hw_loglnp(fmt, ...) hw_logp(fmt "\n", __VA_ARGS__)

#define HW_LOG(fmt, ...)\
    hw_logp("[HWLOG]: " __FILE__ ":%d: " fmt "\n", __LINE__, __VA_ARGS__)

/**
 * Section: Assert
 */
#define HW_ASSERTEX(exp, fmt, ...)\
        do {                \
            if(!(exp)) {    \
                HW_LOG("ASSERT FAIL:" fmt, __VA_ARGS__); \
                hw_exit(-1, HW_STR(#exp));\
            }                                                   \
        } while(0)

#define HW_ASSERT(exp) HW_ASSERTEX(exp, "%s", ":=")
#define HW_ASSERT_EQ(x, y) HW_ASSERTEX(x == y, "LHS != RHS, %s", ":=")
#define HW_ASSERT_NOT_EQ(x, y) HW_ASSERT(x != y)
#define HW_ASSERT_NOTNULL(exp) HW_ASSERT_NOT_EQ(exp, NULL)

#define HW_ASSERT_OP(x, op, y, fmt_x, fmt_y)\
            HW_ASSERTEX(x op y\
                    , "%"fmt_x" "#op" %"fmt_y, x, y) ;


#ifdef HW_BUILD_SINGLE_THREAD
#warning "Building Single Threaded VM"
#define HW_THREAD_ALLOC(TH, size)           HW_MALLOC(size)
#define HW_THREAD_REALLOC(TH, ptr, size)    HW_REALLOC(ptr, size)
#define HW_THREAD_FREE(TH, ptr)             HW_FREE(ptr)
#else
#define HW_THREAD_ALLOC(TH, SIZE)\
            (TH)->allocator.alloc(&(TH)->allocator, SIZE)

#define HW_THREAD_REALLOC(TH, PTR, SIZE)\
            (TH)->allocator.realloc(&(TH)->allocator, PTR, SIZE)

#define HW_THREAD_FREE(TH, PTR)\
            (TH)->allocator.free(&(TH)->allocator, PTR)
#endif

/**
 * hw_VarFn
 */
#define HW_MACRO_EXPAND(...) __VA_ARGS__
#define HW_VARFN(hw, fn, args, argtypes, ...)           \
    {                                                   \
        hw_Var __args[] = {HW_MACRO_EXPAND args};       \
        hw_byte __tid[] = {HW_MACRO_EXPAND argtypes};   \
        fn(hw, __args, __tid, sizeof(__tid));           \
        __VA_ARGS__;                                    \
    }

#define HW_VAR_CALLEX(hw, type_id, self, name, arglist, argt, ...)\
    {                                                               \
        hw_Type *__T = hw_TypeSys_get_via_id(hw->ts, type_id);      \
        hw_VarFn __fn = hw_Type_getvt(__T, name, sizeof(name)-1);   \
        HW_VARFN(hw, __fn                                           \
            , (self, HW_MACRO_EXPAND arglist)                       \
            , (type_id, HW_MACRO_EXPAND argt), __VA_ARGS__)         \
    }

#define TOKEN(x) CAT2(HW_LEXTOKEN, x)
enum hw_LexTokenType {
    /* Unknown Token */
      TOKEN(UNKNOWN)

    /* Error  */
    , TOKEN(ERROR)

    /* Defined Token */
    , TOKEN(SPACE)
    , TOKEN(TABSPACE)
    , TOKEN(NEWLINE)

    , TOKEN(PAREN_LEFT), TOKEN(PAREN_RIGHT)
    , TOKEN(BRACE_LEFT), TOKEN(BRACE_RIGHT)
    , TOKEN(SQR_BRACE_LEFT), TOKEN(SQR_BRACE_RIGHT)
                                              
    , TOKEN(COMMA), TOKEN(DOT), TOKEN(COLON), TOKEN(SEMI_COLON), TOKEN(AT)
    , TOKEN(QUOTE), TOKEN(DOUBLE_QUOTE), TOKEN(DOLLAR), TOKEN(HASH)
    , TOKEN(PIPE), TOKEN(PERCENT), TOKEN(AND), TOKEN(BACK_SLASH)
                                              
    /* Single or Multi Character Tokens */    
    , TOKEN(PLUS), TOKEN(MINUS), TOKEN(SLASH), TOKEN(ASTER)
    , TOKEN(PLUS_EQUAL), TOKEN(MINUS_EQUAL)
    , TOKEN(SLASH_EQUAL), TOKEN(ASTER_EQUAL)
                                              
    , TOKEN(BANG), TOKEN(BANG_EQUAL)
    , TOKEN(EQUAL), TOKEN(EQUAL_EQUAL)        
    , TOKEN(GREATER), TOKEN(GREATER_EQUAL)
    , TOKEN(LESS), TOKEN(LESS_EQUAL)
                                
    /* Literals */    
    , TOKEN(SYMBOL)
    , TOKEN(NUMBER), TOKEN(FLOAT)
                      
    /* Keywords */    
    /*
     * NOTE: As per the new spec, keywords are handled by the
     *       individual compiler rather than lexer. So same
     *       lexer can be used for all forms of compilers and transpiler.
    , TOKEN(RETURN)
    , TOKEN(FUNCTION)
    , TOKEN(LET)
    , TOKEN(AND, OR         
    , TOKEN(TRUE, FALSE     
    , TOKEN(IF, ELIF, ELSE  
    , TOKEN(FOR, WHILE      
    */

    /* End of source code passed */                     
    , TOKEN(END_OF_SOURCE)
                                                        
    /* Token Not Found, for internal evaluation and assertion only */ 
    , TOKEN(NOT_FOUND) 
                                                        
    /* Total number of defined tokens */                
    , TOKEN(TOTAL)
      #undef TOKEN
};

#define hw_LexToken_is(t, s) ((t).type == s)
#define hw_LexToken_is_not(t, s) (!hw_LexToken_is(t, s))

#define hw_LexToken_is_valid(t) ((t).type < HW_LEXTOKEN_TOTAL)
#define hw_LexToken_is_invalid(t) (!hw_LexToken_is_valid(t))

#define hw_LexToken_is_ws(t) ((t).type == HW_LEXTOKEN_SPACE\
                            ||(t).type == HW_LEXTOKEN_TABSPACE\
                            ||(t).type == HW_LEXTOKEN_NEWLINE)

#define hw_LexToken_is_not_ws(t) (!hw_LexToken_is_ws(t))



/**
 * Section: hw_char
 */
#define hw_char_is_ws(x) ((x) == ' ' | (x) == '\n' | (x) == '\t')
#define hw_char_is_num(x) ((x) >= '0' && (x) <= '9')
#define hw_char_is_lower(x) ((x) >= 'a' && (x) <= 'z')
#define hw_char_is_upper(x) ((x) >= 'A' && (x) <= 'Z')
#define hw_char_is_alpha(x) (hw_char_is_lower(x) || hw_char_is_upper(x))
#define hw_char_is_alnum(x) (hw_char_is_alpha(x) || hw_char_is_num(x))

#define hw_char_is_hex(ch) ( hw_char_is_num(ch) || ( ch >= 'A' && ch <= 'F' ) \
                                                || ( ch >= 'a' && ch <= 'f' ) )

#define hw_char_hex_to_num(ch)\
    ( hw_char_is_num(ch)?\
        (ch - '0'): \
     ((ch) >= 'a' && (ch) <= 'f')? \
        (ch - 'a'):\
     ((ch) >= 'A' && (ch) <= 'F')? (ch - 'A'):0 )

/**
 * Section: c-string
 * ASCII string utility
 */
hw_i32 hw_strto_uint(hw_uint *ret, hw_byte const *str, hw_u32 size);
hw_i32 hw_strto_int(hw_int *ret, hw_byte const *str, hw_u32 size);
hw_i32 hw_strto_float(hw_float *ret, hw_byte const *str, hw_u32 size);
void hw_String_trim(hw_String *str, hw_byte ch);
hw_String *hw_stripfile_path_ext(hw_State *hw, hw_byte const *file_name
                                             , hw_u32 file_namesize);
hw_byte const *hw_str_file_extension(hw_byte const *str, hw_u32 str_size, hw_u32 *size);

hw_uint hw_str_calc_linecount(hw_byte const *str, hw_uint size);
hw_uint hw_str_calc_column(hw_byte const *at, hw_byte const *str);
hw_uint hw_str_calc_lineend(hw_byte const *at, hw_byte const *end);

/**
 * Command Line & Subprocess
 */
char** hw_getenv(void);

int hw_fork(const char *command);
pid_t hw_spawn(char const *exec_path, char * const *argv);
pid_t hw_cmd(char * const cmd_nullterm);

int hw_spawn_lock(char const *exec_path, char * const *argv);
int hw_cmd_lock(char * const cmd_nullterm);

/**
 * Section: Tokens
 */
void hw_Lexer_start(hw_Lexer *lex, hw_byte const *data, hw_uint size);
void hw_Lexer_next(hw_Lexer *lex);
void hw_Lexer_next_skipws(hw_Lexer *lex);
void hw_Lexer_next_until(hw_Lexer *lex, enum hw_LexTokenType type);
hw_bool hw_Lexer_next_expect(hw_Lexer *lex, enum hw_LexTokenType type);
hw_bool hw_Lexer_tiseq(hw_Lexer *lex, char const *string, hw_uint string_size);

hw_uint hw_Lexer_line(hw_Lexer const *l);
hw_uint hw_Lexer_col(hw_Lexer const *l);
hw_byte const *hw_Lexer_line_start(hw_Lexer const *l);

/**
 * Memory Allocators 
 */
void hw_Allocator_new_gpa(hw_Allocator *self);
void hw_Allocator_gpa_delete(hw_Allocator *allocator);
void hw_Allocator_new_arena(hw_Allocator *self);
void hw_Allocator_arena_delete(hw_Allocator *self);

hw_ArenaRegion *hw_ArenaRegion_new(hw_u32 max_capacity);
hw_ptr hw_ArenaRegion_alloc(hw_ArenaRegion *region, hw_u32 size);
hw_bool hw_ArenaRegion_check(hw_ArenaRegion *r);

hw_Arena *hw_Arena_new(hw_u32 pool_capacity);
void hw_Arena_delete(hw_Arena *arena);
void *hw_Arena_alloc(hw_Arena *arena, hw_u32 size);
hw_u32 hw_Arena_total_used(hw_Arena *arena);
hw_u32 hw_Arena_total(hw_Arena *arena);
hw_bool hw_Arena_check(hw_Arena *arena);

/**
 * Section: Type Impl
 */
hw_TypeSys *hw_TypeSys_new(hw_uint type_count, hw_Allocator *allocator);
hw_TypeSys* hw_TypeSys_new_default(hw_Allocator *allocator);
void hw_TypeSys_delete(hw_TypeSys *t, hw_Allocator *allocator);
hw_Type *hw_TypeSys_set(hw_TypeSys *ts, hw_Type const *type);
hw_Type *hw_TypeSys_get(hw_TypeSys const *ts, char const *key, hw_uint key_size);
hw_Type *hw_TypeSys_get_via_id(hw_TypeSys const *ts, hw_uint typeid);
hw_VarFn hw_Type_getvt(hw_Type const *T, char const *name, hw_uint name_size);

/**
 * Utils
 */
hw_bool hw_byteArr_loadinc(
    hw_byteArr const *stream, hw_uint* index
  , hw_ptr dest, hw_uint const dest_size);
hw_uint hw_hash_string_fnv(hw_byte const *str, hw_uint len);
void *hw_loadfile(
    hw_State *state, char const path[], hw_uint unitsize, hw_uint *len);
hw_uint hw_ptrcmp(void const* lhs, hw_uint lhs_size
        , void const* rhs, hw_uint rhs_size);
hw_bool hw_writefile(char const path[], void *data, hw_u32 unitsize, hw_u32 len);
hw_byteArr *hw_byteArr_newloadfile(hw_State *hw, const char path[]);

/************************************************************************
 *                              Types
 ************************************************************************/

/*--------------------- SymTableOrd ------------------------*/
hw_SymTableOrd *hw_SymTableOrd_new(hw_State *hw, hw_u32 len);
void hw_SymTableOrd_delete(hw_State *hw, hw_SymTableOrd *table);
hw_u32 hw_SymTableOrd_get_index(hw_SymTableOrd *table, hw_byte const *key
                                                     , hw_u32 key_size);
hw_u32 hw_SymTableOrd_push(hw_State *hw, hw_SymTableOrd *table
                , hw_Var v, hw_byte vtid);
hw_bool hw_SymTableOrd_set(hw_State *hw, hw_SymTableOrd *table
                           , hw_byte const *key, hw_u32 keysize
                           , hw_Var v, hw_byte vtid);

/*--------------------- String ------------------------*/

hw_String *hw_String_new(hw_State *hw, hw_u32 _len);
hw_String *hw_String_newFrom_data(
    hw_State *hw, hw_byte const *data, hw_u32 _len);
hw_bool hw_String_expand(hw_State *hw, hw_String **selfp, hw_u32 const by);
void hw_String_append_data(
    hw_State *hw, hw_String **selfp, hw_byte const *data, hw_u32 _len);
void hw_String_push(hw_State *hw, hw_String **selfp, hw_byte ch);
void hw_String_push_hexchar(hw_State *hw, hw_String **selfp, hw_byte n1, hw_byte n2);
hw_String* hw_String_newFrom_dataRaw(
    hw_State *hw, hw_byte const *data, hw_u32 _len);
void hw_String_append_fmt(hw_State *hw, hw_String **buffer
        , char const *restrict format, ...) __printflike(3, 4);
void hw_String_append_data(
    hw_State *hw, hw_String **selfp, hw_byte const *data, hw_u32 _len);

/*--------------------- VarParse ------------------------*/
hw_uint hw_Var_parse(hw_State *state, hw_Var *result, hw_Lexer *l);

/*--------------------- SymTable ------------------------*/
hw_SymTable *hw_SymTable_new(hw_State *hw, hw_uint len);
void hw_SymTable_set(
    hw_State *hw, hw_SymTable **t
  , hw_byte const *key, hw_uint key_size
  , hw_Var value, hw_byte value_t);
hw_Var hw_SymTable_get(hw_State *hw, hw_SymTable *s, hw_CStr key);

hw_uint hw_SymTable_index(
    hw_SymTable *sym, hw_byte const *key, hw_uint key_size);

/*--------------------- Array ------------------------*/
#define HW_ARR_FOREACH(T, iterator, arr, from, upto, step)\
    for(T iterator = (arr).data + from  \
       ;iterator < (arr).data + upto    \
       ;iterator += step)               \


/*------------------------------- VarList --------------------------------*/
hw_VarList *hw_VarList_new(hw_State *hw, hw_u32 len);

/**
 * INSTS
 */
#define INST(x) CAT2(hw_Inst, x)
enum hw_Inst {
  /**/
    INST(nop) = 0
  , INST(defn)    // Point to FuncInfo
  , INST(return)  // ret with val R(Ax)
  , INST(tailret) // ret with function call
  , INST(push)    // copy push `R(Ax)` variable to top
                  // , B = deep_copy_flag(default = 0)
  , INST(pushex)  // push A variables in stack, with optional flags,
                  // B :=  0= no_copy: nil, 1= shallow_copy, 2= deep_copy
  , INST(pop)     // pop top in stack
                  // (optional) dup it to A, if flag 'B' is set
  , INST(popex)   // pop top `B` variables
                  // , (optional) dup it to `A` list if C is set

  /* Memory */
  , INST(alloc)
  , INST(allock)
  , INST(realloc)
  , INST(free)

  /* Gets */
  , INST(get_type)     // R(Ax) = @typeof(R(Bx))
  , INST(get_routine)  // R(Ax) = Thread(R(Bx) as uint)
  , INST(get_native)   // R(Ax) = x32 -> nativefn
  , INST(get_vt)       // R(Ax) = typeof(R(Bx))->R(Cx)

  /* Call */
  , INST(call)          // call function `x32` defined inside the module
  , INST(call_mod)      // R(Ax) mod:u32 fn:u32
  , INST(call_native)   // call native functions implemented for haywire
  , INST(call_c)        // call loaded c function through ffi
  , INST(call_sym)      // try to link symbol, and perform an appropiate call

  /* Variable Manupulation */
  , INST(top)  // R(x32 + A) = stack.get_top()
  , INST(dup)  // R(Ax) = R(Bx)
  , INST(dups) // [R(Ax):R(Cx)] = [R(Bx):R(Cx)]
  , INST(type) // R(Ax).type ... R(Bx).type = Cx

  , INST(reff)      // R(Ax) = index(R(Bx))
  , INST(dreff)     // R(Ax) = stack(R(Bx))

  , INST(loada32)   // R(Ax).as.u32[0] = 0;
                    //  ...     u32[0] |= x32;
  , INST(loadb32)   // R(Ax).as.u32[1] = 0;
                    // R(Ax).as.u32[1] |= x32;
  , INST(loadknst)  // R(Ax) = knst(x32)

  , INST(list)      // Make list R(Ax) with R(Bx)..R(Cx)
  , INST(unlist)    // Unroll list R(Ax) to R(Bx)..R(Cx)

  /* Jump */
  , INST(jmp)           // pc += R(Ax)
  , INST(jk)            // pc += s48
  , INST(jt)            // if(R(Ax)) pc += R(Bx)
  , INST(jtk)           // if(R(Ax)) pc += s32
  
  /* Type Comaparism */
  , INST(typeq)         // R(Ax) = (T(Bx) == T(Cx))
  , INST(tideq)         // R(Ax) = (T(Bx) == Cx)

  /* Maths (int) */
  , INST(i_add)     // R(Ax) = R(Bx) + R(Cx)
  , INST(i_sub)     // R(Ax) = R(Bx) - R(Cx)
  , INST(i_mul)     // R(Ax) = R(Bx) * R(Cx)
  , INST(i_div)     // R(Ax) = R(Bx) / R(Cx)
  , INST(i_mod)     // R(Ax) = R(Bx) % R(Cx)

  , INST(i_eq)      // R(Ax) = R(Bx) == R(Cx)
  , INST(i_lt)      // R(Ax) = R(Bx) <  R(Cx)
  , INST(i_le)      // R(Ax) = R(Bx) <= R(Cx)

  , INST(i_kadd)    // R(Ax) = R(Bx) + Cx
  , INST(i_ksub)    // R(Ax) = R(Bx) - Cx
  , INST(i_kmul)    // R(Ax) = R(Bx) * Cx
  , INST(i_kdiv)    // R(Ax) = R(Bx) / Cx
  , INST(i_kmod)    // R(Ax) = R(Bx) % Cx

  , INST(i_keq)     // R(Ax) = R(Bx) == Cx
  , INST(i_klt)     // R(Ax) = R(Bx) <  Cx
  , INST(i_kle)     // R(Ax) = R(Bx) <= Cx

  , INST(i_keqs)    // if((R(Ax) == s32) == false) pc++;
  , INST(i_klts)    // if((R(Ax) <  s32) == false) pc++;
  , INST(i_kles)    // if((R(Ax) <= s32) == false) pc++;
 
                 
  /* Maths (floats) */
  , INST(f_add)     // R(Ax) = R(Bx) + R(Cx)
  , INST(f_mul)     // R(Ax) = R(Bx) - R(Cx)
  , INST(f_lt)      // R(Ax) = R(Bx) * R(Cx)

  , INST(prnt)

  /* For testing, ignore */
  , INST(TOTAL)
};

#undef INST

/************************************************************************
 *                               Module                                 *
 ************************************************************************/
void hw_Module_get_FnInfo(hw_Module const *mod
        , hw_uint fn_id, hw_FnInfo *info);
hw_code const *hw_Module_get_fnpc(hw_Module const *m, hw_uint fn_id);
hw_bool hw_Module_get_fn(hw_Module *m, hw_byte const *name, hw_uint name_size, hw_uint *fn_id);

hw_uint hw_Module_calcsize(hw_Module *m);
hw_Module *hw_Module_newblank(
    hw_State *hw, hw_u32 fn_count, hw_u32 code_len
  , hw_u32 data_size, hw_u32 knst_count, hw_bool set_0);

void hw_Module_delete(hw_State *hw, hw_Module *m);
void hw_Module_delete_detatch_knstobj(hw_State *hw, hw_Module *m);

/*
 *
 */
hw_CModule *hw_CModule_newblank(hw_State *hw, hw_u32 const fn_count
                                            , hw_u32 const k_count);
hw_CModule *hw_CModule_newFrom_file(hw_State *hw
                                  , hw_byte const *path_nullterm);
void hw_CModule_delete(hw_State *hw, hw_CModule *cmod);
hw_VarFn hw_CModule_getfn(hw_CModule *cmod
        , hw_byte const *name, hw_u32 name_size);

/************************************************************************
 *                                  VM                                  *
 ************************************************************************/
hw_Global *hw_Global_new(hw_State *parent);
void hw_Global_delete(hw_Global *g, hw_State *parent);

inline hw_u32 hw_Global_add_anonsymb(hw_Global *g, hw_Var v, hw_byte vtid) {
    return hw_SymTableOrd_push(g->parent, g->symbols, v, vtid); }

inline hw_bool hw_Global_add_symb(hw_Global *g, hw_byte const *name
                                              , hw_u32 namesize
                                              , hw_Var v, hw_byte vtid) {
    return hw_SymTableOrd_set(g->parent, g->symbols
                              , name, namesize, v, vtid); }

inline hw_Var hw_Global_get_symb(hw_Global const *g, hw_byte const *symb_name
                                            , hw_u32 symb_name_size) {
    hw_u32 id = g->symbols->indices[
      hw_SymTableOrd_get_index(g->symbols, symb_name, symb_name_size)];
    if(id > g->symbols->vlenUsed) {
        return g->symbols->vals[id];
    }
    return (hw_Var){0}; }

inline hw_Var hw_Global_get_symb_via_id(hw_Global const *g, hw_u32 id) {
    return g->symbols->vals[id]; }

/************************************************************************
 *                              State                                   *
 ************************************************************************/
hw_State *hw_State_new_default(hw_State *parent);
void hw_State_delete(hw_State *s);

/************************************************************************
 *                          [State::FnStack]                            *
 ************************************************************************/

inline void hw_State_fstack_push(
    hw_State *s, hw_u32 const mod_id, hw_u32 const fn_id) {
    HW_ARR_PUSH(s, s->fstack, ((hw_FnState){
                                    .fn = fn_id
                                  , .mod = mod_id
                                  , .pc = 0
                                  , .var = s->vstack->lenUsed
                                })); }

inline void hw_State_fstack_pop(hw_State *hw) {
    HW_DEBUG(HW_ASSERT(hw->fstack->lenUsed));
    hw->fstack->lenUsed -= 1; }

inline hw_FnState* hw_State_fstack_top(hw_State *hw) {
    return hw->fstack->data +(hw->fstack->lenUsed-1); }

inline void hw_State_fstack_top_save(hw_State *hw, hw_code const *pc
                                                 , hw_Var const *var) {
    hw_FnState *f = hw_State_fstack_top(hw);
    hw_Module const *m = hw_Global_get_symb_via_id(hw->global, f->mod)
                            .as_module;
    f->pc = pc - m->code;
    f->var = var - hw->vstack->data; }


/************************************************************************
 *                          [Stack::VStack]                             *
 ************************************************************************/

void hw_State_vstack_reserve(hw_State *hw, hw_u32 const by);
hw_u32 hw_State_vstack_push_mult(hw_State *hw, hw_u32 const by);
hw_u32 hw_State_vstack_push(hw_State *hw, hw_Var v, hw_byte tid);
void hw_State_vstack_pop_mult_dtor(hw_State *hw, hw_u32 const by);

inline void hw_State_vstack_pop_mult(hw_State *hw, const hw_u32 by) {
    hw->vstack->lenUsed -= by; }

/**
 * VM
 */
void hw_vm(hw_State *hw);
hw_FnState* hw_vm_prepare_call(hw_State *hw, hw_uint mod_id, hw_uint fn_id);


/************************************************************************
 *                           Module Object                              *
 ************************************************************************/
hw_ModuleObj* hw_ModuleObj_new(hw_State *hw);
void hw_ModuleObj_delete(hw_State *hw, hw_ModuleObj *mobj);
void hw_ModuleObj_reset(hw_State *hw, hw_ModuleObj *mobj);
hw_Module *hw_Module_loadFromFile(hw_State *hw, char const path[]);
void hw_Module_writetofile(hw_State *hw, hw_Module *m, char const path[]);

hw_uint hw_ModuleObj_inst(hw_State *hw, hw_ModuleObj *mobj
                        , hw_code const inst);

hw_uint hw_ModuleObj_inststream(hw_State *hw, hw_ModuleObj *mobj
                              , hw_code const *insts, hw_u32 i_count);

hw_uint hw_ModuleObj_data(hw_State *hw, hw_ModuleObj *mobj
                        , void const *data, hw_uint const size);

hw_uint hw_ModuleObj_fndata(hw_State *hw, hw_ModuleObj *mobj
            , hw_byte const *name, hw_byte const *tids
            , hw_u32 name_size, hw_u32 mut_count
            , hw_u32 args_passed, hw_u32 stack_size);

hw_uint hw_ModuleObj_knst(hw_State *hw, hw_ModuleObj *mobj
                                      , hw_Var val
                                      , hw_byte val_tid);

hw_uint hw_ModuleObj_knstcopy(hw_State *hw, hw_ModuleObj *mobj
                                          , hw_Var val
                                          , hw_byte val_tid);

hw_Module *hw_Module_combine(hw_State *hw, hw_u32 mod_count
                                         , hw_Module **mods
                                         , hw_String **namespaces);

void hw_ModuleObj_addmod(hw_State *hw, hw_ModuleObj *mobj
                                     , hw_Module const *m
                                     , hw_String *namespace);

hw_Module* hw_ModuleObj_to_Module(hw_State *hw, hw_ModuleObj *mobj);


/************************************************************************
 *                          Byte Code Compiler                          *
 ************************************************************************/
hw_CompilerBC *hw_compbc_new(hw_State *parent);
void hw_compbc_set_source(hw_CompilerBC *comp, hw_byte *source, hw_uint size
                        , hw_byte *name, hw_uint name_size);
void hw_compbc_delete(hw_CompilerBC *comp);
void hw_compbc_compile_from_source(hw_CompilerBC *comp);
hw_bool hw_compbc_compile_files(hw_State *hw, hw_ModuleArr **modarr
                                            , hw_String **files
                                            , hw_u32 count);


/************************************************************************
 *                          ByteCode to C Transpiler                    *
 ************************************************************************/
hw_String* hw_compc_mod_to_c(hw_State *hw, hw_Module const *m);

/************************************************************************
 *                              Debug                                   *
 ************************************************************************/
void hw_debug_Module_disasm(hw_State *hw, hw_Module const *m);
void hw_debug_code_disasm(hw_State const *hw, hw_code code);
void hw_debug_State_trace(hw_State *hw);

void hw_debug_vm_step(
    hw_State *hw, hw_Module const *m, hw_code const *pc, hw_Var const *v);

void hw_debug_print_inst(hw_State *hw);
void hw_debug_print_fnobj(hw_CompilerBC const *comp);
void hw_debug_print_mobj(hw_CompilerBC const *comp);
void hw_debug_print_var(hw_State *hw, hw_Var v, hw_byte t);
void hw_debug_print_symtable_ord(hw_State *hw, hw_SymTableOrd *table);
void hw_debug_print_cmod(hw_State *hw, hw_CModule* cmod);

/**
 * Section: Undef
 */
#undef CAT
#undef CAT2

#endif
