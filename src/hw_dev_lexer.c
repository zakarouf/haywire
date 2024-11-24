#include "hw.h"
#include "hw_dev.h"
#include <stdlib.h>
#include <ctype.h>


#define CAT2(x, y) x##y
#define TOKEN(x) CAT2(HW_LEXTOKEN_, x)

#define _peek(s) (*(s)->at)
#define _isend(s) (*(s)->at >= *(s)->end)

#define _make_token_not_found(s) hw_Scanner_make_token_not_found(s)
#define _match_char(s, ch) hw_Scanner_match_char(s, ch)
#define _advance(s) hw_Scanner_advance(s)
#define _scan_as(W, s) zpp__CAT(hw_Scanner_scan_as_, W)(s)

#define hw_Lexer_check(lex, is) ((lex).token.type == is)

static inline void _make_token(hw_Lexer *lex, enum hw_LexTokenType token_type)
{
    lex->token.type = token_type;
    lex->token.size = lex->at - lex->token.start;
}

static inline void _make_token_err(hw_Lexer *lex, hw_byte *msg, hw_uint size)
{
    lex->token.start = msg;
    lex->token.size = size;
}

static inline hw_uint _advance(hw_Lexer *lex)
{
    lex->at += 1;
    return lex->at[-1];
}

void hw_Lexer_start(hw_Lexer* lex, const hw_byte *string_data, hw_uint size)
{
    lex->at = string_data;
    lex->end = string_data + size;
    lex->token.start = lex->at;
    lex->token.size = 0;
    lex->token.line = 1;
    lex->token.type = HW_LEXTOKEN_UNKNOWN;
}

static int _check_if_symbol(hw_Lexer *lex)
{
    if(!(isalpha(lex->token.start[0]) || lex->token.start[0] == '_')) {
        return 0;
    }
    while( isalnum(_peek(lex)) && !_isend(lex)) {
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
            case ch: _make_token(lex, TOKEN(Token)); return 1; break

        _match(' ', SPACE);
        _match('\t', TABSPACE);
        case '\n': {
            lex->token.line += 1; 
            _make_token(lex, TOKEN(NEWLINE));
        } break;

        _match(EOF, END_OF_SOURCE);

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
        _match('#', HASH);

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


void hw_Lexer_next(hw_Lexer *lex)
{
    if(_isend(lex)) {
        _make_token(lex, HW_LEXTOKEN_END_OF_SOURCE);
        return;
    }
    
    lex->token.start = lex->at;
    _advance(lex);
    
    if(_check_if_number(lex)) { return; }
    if(_check_if_symbol(lex)) { return; }
    if(_check_if_char(lex))   { return; }

    //_check_if_char(lex);
    //if(!hw_Lexer_check(*lex, HW_LEXTOKEN_NOT_FOUND)) { return; }

    (void)malloc;

    _make_token_err(lex, HW_STR("NO TOKEN MATCHED"));
}

void hw_Lexer_next_skipws(hw_Lexer *lex)
{
    do {
        hw_Lexer_next(lex);
    } while(hw_LexToken_is_ws(lex->token) && !_isend(lex));
}
