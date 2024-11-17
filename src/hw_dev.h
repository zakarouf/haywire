#ifndef ZAKAROUF_HAYWIRE_DEV_H
#define ZAKAROUF_HAYWIRE_DEV_H

#include "hw.h"

/**
 * Section: Pre-Processor
 */
#define CAT2(X, Y) X##_##Y
#define CAT(X, Y) CAT2(X, Y)

/**
 */
#define HW_STR(s) (void*)s, (sizeof(s)-1)

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
 * Exit
 */
void hw_exit(const char *msg, size_t size);

/**
 * Assert
 */
#define HW_ASSERT(exp)\
        (exp?\
            (void)0: \
            hw_exit(HW_STR("Assertion Failure: " #exp)))


/**
 * Section ARR_EXPORT
 */
#define ARR_EXPORT(ARRT, name, VT)\
    ARRT* CAT(name, new)(hw_uint len);          \
    ARRT* CAT(name, newFrom)(VT [static const 1], hw_uint len);\
    void CAT(name, delete)(ARRT* arr);          \
    VT* CAT(name, push)(ARRT** arr, VT value);  \
    VT* CAT(name, pushstream)(ARRT** arr, VT values[static 1], hw_uint len);\
    hw_uint CAT(name, pop)(ARRT** arr);         \

#define HW_ARR_FOREACH(T, iterator, arr, from, upto, step)\
    for(T iterator = (arr).data + from  \
       ;iterator < (arr).data + upto    \
       ;iterator += step)               \

/**
 * DECLARATION
 */
ARR_EXPORT(hw_uintArr, hw_uintArr, hw_uint)
ARR_EXPORT(hw_byteArr, hw_byteArr, hw_byte)
ARR_EXPORT(hw_codeArr, hw_codeArr, hw_code)
ARR_EXPORT(hw_String, hw_String, hw_byte)


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


typedef enum hw_LexTokenType hw_LexTokenType;
typedef struct hw_LexToken hw_LexToken;
struct hw_LexToken {
    hw_byte const   *start;
    hw_uint         size;
    hw_uint         line;
    hw_LexTokenType type;
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
void hw_Lexer_next_until(hw_Lexer *lex, hw_LexTokenType type);


/**
 * Section: Type Impl
 */

hw_TypeSys *hw_TypeSys_new(hw_uint type_count, hw_Allocator allocator);
void hw_TypeSys_delete(hw_TypeSys *t);
hw_Type *hw_TypeSys_set(hw_TypeSys *ts, hw_Type const *type);

#define HW_TYPESYS_ALLOC(TS, SIZE)\
            (TS)->allocator.alloc(&(TS)->allocator.state, SIZE)

#define HW_TYPESYS_REALLOC(TS, PTR, SIZE)\
            (TS)->allocator.realloc(&(TS)->allocator.state, PTR, SIZE)

#define HW_TYPESYS_FREE(TS, PTR)\
            (TS)->allocator.free(&(TS)->allocator.state, PTR)


/**
 * Section: Undef
 */
#undef CAT
#undef CAT2
#undef ARR_EXPORT
#undef TOKEN
#endif

