#ifndef ZAKAROUF_HAYWIRE_DEV_H
#define ZAKAROUF_HAYWIRE_DEV_H

#include "hw.h"

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
 * Section: Cast
 */
#define HW_CAST(T, ...) ((T)(__VA_ARGS__))

/**
 * Log & Exit
 */
void hw_exit(hw_int code, const char *msg, size_t const size);
void hw_logp(const char *fmt, ...) __attribute__ ((format (printf, 1, 2)));
void hw_logstr(const char *msg, size_t const);

#define HW_LOG(fmt, ...)\
    hw_logp("[HWLOG]: " __FILE__ ":%d:" fmt "\n", __LINE__, __VA_ARGS__)

/**
 * Assert
 */
#define HW_ASSERTEX(exp, fmt, ...)\
        do {                \
            if(!(exp)) {    \
                HW_LOG("ASSERT FAIL:" fmt, __VA_ARGS__); \
                hw_exit(-1, HW_STR(#exp));\
            }                                                   \
        } while(0)

#define HW_ASSERT(exp) HW_ASSERTEX(exp, "%s", ":=")

/**
 * Section: Tokens
 */

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
    , TOKEN(BACK_SLASH)
                                              
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
};
#undef TOKEN

typedef struct hw_LexToken hw_LexToken;
struct hw_LexToken {
    hw_byte const   *start;
    hw_uint         size;
    hw_uint         line;
    hw_byte         type;
};

/**
 * Section: Lexer
 */

typedef struct hw_Lexer hw_Lexer;
struct hw_Lexer {
    hw_byte const   *at;
    hw_byte const   *end;
    hw_LexToken     token;
};


#define hw_LexToken_is(t, s) ((t).type == s)
#define hw_LexToken_is_not(t, s) (!hw_LexToken_is(t, s))

#define hw_LexToken_is_valid(t) ((t).type < HW_LEXTOKEN_TOTAL)
#define hw_LexToken_is_invalid(t) (!hw_LexToken_is_valid(t))

#define hw_LexToken_is_ws(t) ((t).type == HW_LEXTOKEN_SPACE\
                            ||(t).type == HW_LEXTOKEN_TABSPACE\
                            ||(t).type == HW_LEXTOKEN_NEWLINE)

#define hw_LexToken_is_not_ws(t) (!hw_LexToken_is_ws(t))

void hw_Lexer_start(hw_Lexer *lex, hw_byte const *data, hw_uint size);
void hw_Lexer_next(hw_Lexer *lex);
void hw_Lexer_next_skipws(hw_Lexer *lex);
void hw_Lexer_next_until(hw_Lexer *lex, enum hw_LexTokenType type);


/**
 * Section: Type Impl
 */
void hw_Allocator_default(hw_Allocator *self);

hw_TypeSys *hw_TypeSys_new(hw_uint type_count, hw_Allocator allocator);
hw_TypeSys* hw_TypeSys_default_with_allocator(hw_Allocator allocator);
void hw_TypeSys_delete(hw_TypeSys *t);
hw_Type *hw_TypeSys_set(hw_TypeSys *ts, hw_Type const *type);

#define HW_TYPESYS_ALLOC(TS, SIZE)\
            (TS)->allocator.alloc(&(TS)->allocator.state, SIZE)

#define HW_TYPESYS_REALLOC(TS, PTR, SIZE)\
            (TS)->allocator.realloc(&(TS)->allocator.state, PTR, SIZE)

#define HW_TYPESYS_FREE(TS, PTR)\
            (TS)->allocator.free(&(TS)->allocator.state, PTR)


/**
 * Section: Var Interface Call
 */
#define HW_VAR_CALL_CORE(tid, ts, interface, ...)               \
        { HW_DEBUG(HW_ASSERT(tid < hw_TypeID_TOTAL);)          \
        (ts)->types[tid].interface(ts, __VA_ARGS__); }

#define HW_VAR_CALL(tid, ts, interface, ...)\
    {                                                           \
        HW_DEBUG(HW_ASSERT(tid < hw_TypeID_TOTAL);)             \
        (ts)->types[tid].vt[interface](ts, __VA_ARGS__);        \
    }

/**
 * Section ARR_EXPORT
 */
#define DEFN(T, NAME)\
    hw_VarP CAT2(T, NAME) (     \
        hw_TypeSys  *ts          \
      , hw_Var      *args     \
      , hw_byte     *tid      \
      , hw_uint const count)

#define INTERFACE_EXPORT(T)\
    DEFN(T, new);               \
    DEFN(T, newFrom_data);      \
    DEFN(T, newFrom_conf);      \
    DEFN(T, newFrom_string);    \
    DEFN(T, newFrom_copy);      \
    DEFN(T, reset);             \
    DEFN(T, reset_copy);        \
    DEFN(T, delete);            \
    DEFN(T, compare_bin);       \
    DEFN(T, compare_all);       \
    DEFN(T, to_string);         \
    DEFN(T, to_data);           \
    DEFN(T, to_hash);           \

#define HW_ARR_FOREACH(T, iterator, arr, from, upto, step)\
    for(T iterator = (arr).data + from  \
       ;iterator < (arr).data + upto    \
       ;iterator += step)               \

/**
 * DECLARATION
 */
INTERFACE_EXPORT(hw_uintArr)
INTERFACE_EXPORT(hw_byteArr)

INTERFACE_EXPORT(hw_uint)
INTERFACE_EXPORT(hw_int)
INTERFACE_EXPORT(hw_ptr)

INTERFACE_EXPORT(hw_String)
    DEFN(hw_String, newFrom_cstr);
    DEFN(hw_String, newFrom_file);
    DEFN(hw_String, append_str);

INTERFACE_EXPORT(hw_VarList)
INTERFACE_EXPORT(hw_VarArr)
    DEFN(hw_VarArr, push);

#undef DEFN
#undef INTERFACE_EXPORT

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
  , INST(reserve) // reserve variables in stack
  , INST(release) // release variables in stack with (optional) delete

  /* Gets */
  , INST(get_type)     // R(Ax) = @typeof(R(Bx))
  , INST(get_routine)  // R(Ax) = Thread(R(Bx))
  , INST(get_native)   // R(Ax) = x32 -> nativefn
  , INST(get_vt)

  /* Call */
  , INST(call)      // call R(Ax) with R(Bx) args, save return val to R(Cx)
  , INST(calln)     // Call native functions implemented for haywire
                    // call R(Ax) with R(Bx) args, save return val to R(Cx)

  , INST(callc)     // Call any c function through ffi

  /* Variable Manupulation */
  , INST(dup)  // R(Ax) = R(Bx)
  , INST(dups) // [R(Ax):R(Cx)] = [R(Bx):R(Cx)]
  , INST(type) // R(Ax).type = Bx
  , INST(nil)  // R(Ax) = nil

  , INST(a32_0)  // Set the first 32bits of R(Ax)
  , INST(a32_1)  // Set the last 32bits of R(Ax)

  , INST(list) // Make list R(Ax) with R(Bx)..R(Cx)
  , INST(string) // Make string R(Ax) with data R(Bx)

  /* Jump */
  , INST(jmp) // pc += R(Ax)
  , INST(jmps32) // pc += s32
  , INST(jmpif) // if(R(Ax)) pc += R(Bx)
  , INST(jmpifs32) // if(R(Ax)) pc += s32
  
  /* Type Comaparism */
  , INST(typeq) // R(Ax) = (T(Bx) == T(Cx))
  , INST(tideq) // R(Ax) = (T(Bx) == Cx)
  , INST(iseq)

  /* Maths (number) */
  , INST(addn)
  , INST(muln)
  , INST(ltn)
  , INST(lten)

  /* Maths (floats) */
  , INST(addf)
  , INST(mulf)
  , INST(ltf)

  /* For testing, ignore */
  , INST(TOTAL)
};

#undef INST

/**
 * Threads;
 */
hw_VarP hw_Thread_init(
    hw_Thread *t, hw_State const *global, hw_uint id, const char *name
  , hw_byte name_size);
hw_VarP hw_Thread_deinit(hw_Thread *t);

/**
 * Section: Undef
 */
#undef CAT
#undef CAT2

#endif
