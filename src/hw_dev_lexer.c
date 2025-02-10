#include "hw.h"
#include "hw_dev.h"
#include <ctype.h>
#include <string.h>

#define TOKEN(x) [HW_LEXTOKEN_##x] = { .data = (void *)#x, .len = (sizeof(#x) -1) }
static hw_CStr const hw_TOKEN_NAMES[HW_LEXTOKEN_TOTAL+1] = {
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
    , TOKEN(PERCENT), TOKEN(QUOTE), TOKEN(DOUBLE_QUOTE), TOKEN(DOLLAR)
    , TOKEN(HASH), TOKEN(AND), TOKEN(BACK_SLASH)
                                              
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

#define CAT2(x, y) x##y
#define TOKEN(x) CAT2(HW_LEXTOKEN_, x)

#define _peek(s) (*(s)->at)
#define _isend(s) ((s)->at >= (s)->end)

#define hw_Lexer_check(lex, is) ((lex).token.type == is)

hw_CStr get_token_name(hw_uint token_type)
{
    HW_ASSERT(token_type < HW_LEXTOKEN_TOTAL);
    return hw_TOKEN_NAMES[token_type];
}

static inline void _make_token(hw_Lexer *lex, enum hw_LexTokenType token_type)
{
    lex->token.type = token_type;
    lex->token.size = lex->at - lex->token.start;
}

static inline void _make_token_err(hw_Lexer *lex, hw_byte *msg, hw_uint size)
{
    _make_token(lex, HW_LEXTOKEN_ERROR);
    lex->token.start = msg;
    lex->token.size = size;
}

static inline hw_uint _advance(hw_Lexer *lex)
{
    lex->at += 1;
    lex->token.column += 1;
    return lex->at[-1];
}

static int _check_if_symbol(hw_Lexer *lex)
{
    if(!(isalpha(lex->token.start[0]) || lex->token.start[0] == '_')) {
        return 0;
    }
    while( (isalnum(_peek(lex)) || (_peek(lex) == '_')) 
        && !_isend(lex)) {
        _advance(lex);
    }

    _make_token(lex, HW_LEXTOKEN_SYMBOL);
    return 1;
}

static int _check_if_number(hw_Lexer *lex)
{
    if(!isdigit(lex->token.start[0])) {
        return 0;
    }
    hw_byte dot = 0;
    if(_peek(lex) == '.') {
        dot += 1;
        _advance(lex);
    }

    while(isdigit(_peek(lex)) && !_isend(lex)) {
        _advance(lex);
        if(_peek(lex) == '.') {
            if(dot) {
                _make_token(lex, HW_LEXTOKEN_FLOAT);
                return 1;
            } else {
                dot += 1;
            }
        }
    }

    _make_token(lex, HW_LEXTOKEN_NUMBER);
    return 1;
}

static int _check_if_char(hw_Lexer *lex)
{
    switch (lex->token.start[0]) {

        /* Single Char Case */
        #define _match(ch, Token)\
            break; case ch: {_make_token(lex, TOKEN(Token)); return 1;}

        _match(' ', SPACE);
        _match('\t', TABSPACE);
        case '\n': {
            lex->token.line += 1; 
            lex->token.column = 0;
            lex->token.line_start = lex->at;
            _make_token(lex, TOKEN(NEWLINE));
            return 1;
        } break;

        //_match(EOF, END_OF_SOURCE);

        _match('(', PAREN_LEFT);
        _match(')', PAREN_RIGHT);
        _match('{', BRACE_LEFT);
        _match('}', BRACE_RIGHT);
        _match('[', SQR_BRACE_LEFT);
        _match(']', SQR_BRACE_RIGHT);

        _match(',', COMMA);
        _match('.', DOT);
        _match(':', COLON);
        _match(';', SEMI_COLON);
        _match('\'', QUOTE);
        _match('"', DOUBLE_QUOTE);
        _match('$', DOLLAR);
        _match('@', AT);
        _match('%', PERCENT);
        _match('#', HASH);
        _match('&', AND);

        _match('\\', BACK_SLASH);
    
        /**/

    /**
     * NOTE: OLD SOURCE USING z_, now deprecated
        #define check2_if(c, T)\
            if(_match_char(s, c)){ return _make_token(T, s); };

        #define check2_ifexpand(x) check2_if x

        #define check2(c1, T1, ...)\
                break; case c1: zpp__Args_map(check2_ifexpand, __VA_ARGS__);\
                                return _make_token(T1, s); break
      **/

        #define _match2(c1, T1, c2, T2)\
            case c1: {\
                _advance(lex);\
                if(_peek(lex) == c2) {\
                    _make_token(lex, TOKEN(T2));\
                } else {\
                    _make_token(lex, TOKEN(T1));\
                }\
                return 1;\
            } break;\

        _match2('!', BANG
            , '=', BANG_EQUAL);
        
        _match2('=', EQUAL
             , '=', EQUAL_EQUAL);

        _match2('>', GREATER
             , '=', GREATER_EQUAL);

        _match2('<', LESS
             , '=', LESS_EQUAL);

        _match2('+', PLUS
             , '=', PLUS_EQUAL);

        _match2('-', MINUS
             , '=', MINUS_EQUAL);

        _match2('*', ASTER
             , '=', ASTER_EQUAL);

        _match2('/', SLASH
             , '=', SLASH_EQUAL);

    break;
    }

    #undef _match
    #undef _match2

    return 0;
}

void hw_Lexer_start(hw_Lexer* lex, const hw_byte *string_data, hw_uint size)
{
    lex->at = string_data;
    lex->end = string_data + size;
    lex->token.start = lex->at;
    lex->token.line_start = lex->at;
    lex->token.size = 0;
    lex->token.line = 1;
    lex->token.type = HW_LEXTOKEN_UNKNOWN;
}

#include <stdio.h>
static void _print_token(hw_Lexer *lex)
{
    HW_ASSERT(lex->token.type < HW_LEXTOKEN_TOTAL);
    if(lex->token.type == HW_LEXTOKEN_NEWLINE) {
        printf(" -> NEWLINE\n");
        return;
    }
    hw_CStr const tokname = get_token_name(lex->token.type);
    printf("%.*s -> %.*s\n | \n", (int)lex->token.size, lex->token.start,
            (int)tokname.len, tokname.data);
    
}

void hw_Lexer_next(hw_Lexer *lex)
{
    HW_DEBUG(_print_token(lex));

    if(_isend(lex)) {
        _make_token(lex, HW_LEXTOKEN_END_OF_SOURCE);
        return;
    }
    
    lex->token.start = lex->at;
    _advance(lex);
    
    if(_check_if_char(lex))   { return; }
    if(_check_if_number(lex)) { return; }
    if(_check_if_symbol(lex)) { return; }

    //_check_if_char(lex);
    //if(!hw_Lexer_check(*lex, HW_LEXTOKEN_NOT_FOUND)) { return; }

    _make_token_err(lex, HW_STR("NO TOKEN MATCHED"));
}

void hw_Lexer_next_skipws(hw_Lexer *lex)
{
    do {
        hw_Lexer_next(lex);
    } while(hw_LexToken_is_ws(lex->token) && !_isend(lex));
}

void hw_Lexer_next_until(hw_Lexer *lex, enum hw_LexTokenType type)
{
    do {
        hw_Lexer_next(lex);
    } while (hw_LexToken_is_not(lex->token, type) && !_isend(lex));
}

hw_bool hw_Lexer_next_expect(hw_Lexer *lex, enum hw_LexTokenType type)
{
    hw_Lexer_next(lex);
    return lex->token.type == type;
}

hw_bool hw_Lexer_tiseq(hw_Lexer *lex, char const *string, hw_uint string_size)
{
    if(string_size == lex->token.size) {
        return !memcmp(lex->token.start, string, string_size);
    }
    return 0;
}

