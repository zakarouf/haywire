#include "def.h"
#include "dev.h"
#include "cstd.h"
#include "hwfn.h"
#include <string.h>

/**********************************************************************/
/* All Types VTables:
 */
/**********************************************************************/
#define DEFN(NAME)\
    void hwfn_##NAME (       \
        hw_State     *hw        \
      , hw_Var       *args      \
      , hw_byte      *tids      \
      , hw_uint const argc)

#define _ALLOC(SIZE)            HW_THREAD_ALLOC(hw, SIZE)
#define _REALLOC(PTR, SIZE)     HW_THREAD_REALLOC(hw, PTR, SIZE)
#define _FREE(PTR)              HW_THREAD_FREE(hw, PTR)

#define _GET_SELF() (args[0])
#define _GET_SELF_TID() (tids[0])
#define _SELF(T) T self
#define _SELF_ASSIGN(as) (_GET_SELF()).as = self;
#define _SELF_BIND(T, as) T self = _GET_SELF().as

#define _GET_ARG_RAW(n) (args[n+1])
#define _GET_ARG(n, as) (_GET_ARG_RAW(n).as)
#define _GET_ARG_TID(n) (tids[1 + n])
#define _MAKE_VAR(value, as) ((hw_Var){.as = value})

#define _SET_ARG_RAW(n, val) { _GET_ARG_RAW(n) = val; }
#define _SET_ARG(n, as, val) { _GET_ARG(n, as) = val; }
#define _SET_ARG_TID(n, type)  { _GET_ARG_TID(n) = type; }

#define _CHECK_ARGS(_c, ...)\
    do {\
        hw_uint c = _c + 1;\
        HW_ASSERT(c == argc);\
        hw_byte type_ids[] = {__VA_ARGS__};\
        for (size_t i = 0; i < c; i++) {\
            if(type_ids[i] != hw_TypeID_nil) {\
                HW_ASSERTEX(type_ids[i] == tids[i]\
                , "Mis-match argument type:%.*s at position '%" PRIu64 "'"\
                , hw->ts->types[tids[i]].name_size      \
                , hw->ts->types[tids[i]].name           \
                , i);                                   \
            }\
        }\
    } while(0)

/*************************************************************************
 *                          PRIVATE
 *************************************************************************/
DEFN(VarFn_UNREACHABLE) {
    (void)hw;
    (void)args;
    (void)tids;
    (void)argc;
    HW_ASSERT(0 && "Reached Unreachable");
    
}

/* newFrom_Fmt(self, lexer: hw_Lexer) */
/* to_string(self, result: string) NOTE: String is already initialized */

/*******************
// INT
 *******************/
DEFN(int_to_string) {
    (void)argc;
    (void)tids;

    hw_String *str = _GET_ARG(0, as_string);
    _SELF_BIND(hw_int, as_int);
    hw_byte buffer[32];
    hw_byte len = snprintf((char *)buffer, 31, "%"PRIi64, self);
    hw_String_append_data(hw, &str, buffer, len);
    _SET_ARG(0, as_string, str);
    
}

/*******************
// UINT
 *******************/
DEFN(uint_to_string) {
    (void)argc;
    (void)tids;

    hw_String *str = _GET_ARG(0, as_string);
    _SELF_BIND(hw_uint, as_uint);
    hw_byte buffer[32];
    hw_byte len = snprintf((char *)buffer, 31, "%"PRIi64, self);
    hw_String_append_data(hw, &str, buffer, len);
    _SET_ARG(0, as_string, str);
    
}

/*******************
// FLOAT
 *******************/
DEFN(float_to_string) {
    (void)argc;
    (void)tids;

    hw_String *str = _GET_ARG(0, as_string);
    _SELF_BIND(hw_float, as_float);
    hw_byte buffer[32];
    hw_byte len = snprintf((char *)buffer, 31, "%lg", self);
    hw_String_append_data(hw, &str, buffer, len);
    _SET_ARG(0, as_string, str);
    
}

/*******************
// STRINGS
 *******************/

DEFN(String_new) {
    (void)args;
    (void)tids;
    (void)argc;

    const hw_u32 default_size = 8;
    _SELF(hw_String *) =  hw_String_new(hw, default_size);
    _SELF_ASSIGN(as_string);
    _GET_SELF_TID() = hw_TypeID_string;
    
}

static hw_CStr hw_Lexer_convert_string(hw_Lexer *l, enum hw_LexTokenType end)
{
    hw_CStr string = { .data = (void *)l->at, .len = 0 };
    while (hw_LexToken_is_not(l->token, HW_LEXTOKEN_END_OF_SOURCE)) {
        hw_Lexer_next_until(l, end);
        if(l->token.start[-1] != '\\') {
            string.len = l->at - string.data;
            return string;
        }
    }
    return string;
}

DEFN(String_newFrom_fmt) {
    (void)argc;
    hw_CStr string = hw_Lexer_convert_string(_GET_ARG(0, as_lexer)
                                            , HW_LEXTOKEN_DOUBLE_QUOTE);

    if(!string.len) { _GET_SELF_TID() = hw_TypeID_nil; };

    _SELF(hw_String *) = hw_String_newFrom_dataRaw(hw, string.data, string.len);
    _SELF_ASSIGN(as_string);
    _GET_SELF_TID() = hw_TypeID_string;
    
}

DEFN(String_newFrom_data) {
    (void)tids;
    (void)argc;

    hw_byte *data = _GET_ARG(0, as_byte_p);
    hw_u32 dsize = _GET_ARG(1, as_uint);

    _SELF(hw_String *) = hw_String_newFrom_data(hw, data, dsize);
    _SELF_ASSIGN(as_string);
    _GET_SELF_TID() = hw_TypeID_string;
    
}

DEFN(String_newFrom_copy) {
    (void)args;
    (void)tids;
    (void)argc;
    
    hw_String *source = _GET_ARG(0, as_string);

    _SELF(hw_String *) = hw_String_newFrom_data(hw, 
            source->data, source->lenUsed);
    _SELF_ASSIGN(as_string);
    _GET_SELF_TID() = hw_TypeID_string;

    
}
DEFN(String_newFrom_deserialize) { // &self, &index, bytearray
    (void)argc;
    (void)tids;
    hw_uint index = _GET_ARG(0, as_uint);
    hw_byteArr *buffer = _GET_ARG(1, as_bytearr);
    
    hw_u32 len;
    hw_byteArr_loadinc(buffer, &index, &len, sizeof(len));
    _SELF(hw_String *) = hw_String_new(hw, len + 1);
    hw_byteArr_loadinc(buffer, &index, self->data, len * sizeof(*self->data));
    self->lenUsed = len;

    _SET_ARG(0, as_uint, index);
    _SELF_ASSIGN(as_string);
    
}

DEFN(String_to_serialize) { // &self, &bytearray
                               //
    (void)argc;
    (void)tids;
    _SELF_BIND(hw_String *, as_string);
    hw_byteArr *buffer = _GET_ARG(0, as_bytearr);
    HW_ARR_PUSHSTREAM(hw, buffer, &self->lenUsed, sizeof(self->lenUsed));
    HW_ARR_PUSHSTREAM(hw, buffer, self->data
            , self->lenUsed * sizeof(*self->data));
    _SET_ARG(0, as_bytearr, buffer);
    
}

DEFN(String_delete) {
    (void)args;
    (void)tids;
    (void)argc;
    hw_String_delete(hw, _GET_SELF().as_string);
}

DEFN(String_append_bytes) {
    (void)tids;
    (void)argc;

    _SELF_BIND(hw_String *, as_string);
    hw_byte *data = _GET_ARG(0, as_byte_p);
    hw_uint dsize = _GET_ARG(1, as_uint);

    hw_uint const availiable_len = self->len - self->lenUsed;
    if(availiable_len < dsize) { hw_String_expand(hw, &self, dsize + 1); }
    
    memcpy(self->data + self->lenUsed, data, dsize);
    self->lenUsed += dsize;
    _SELF_ASSIGN(as_string);
    
}

DEFN(String_newFrom_file) {
    HW_DEBUG(_CHECK_ARGS(2, hw_TypeID_string, hw_TypeID_string));

    hw_String *path = _GET_ARG(0, as_string);
    hw_Var filesize;
    hw_Var filedata = { 
        .as_ptr = hw_loadfile(hw, (void const *)path->data
                , path->lenUsed, &filesize.as_uint)
    };
    
    hw_Var args_list[] = { {0}, filedata, filesize };
    hw_byte tids_list[] = { 0, hw_TypeID_ptr, hw_TypeID_uint };
    hwfn_String_newFrom_data(hw, args_list, tids_list, 3);

    _GET_SELF().as_string = args_list[0].as_string;
}

DEFN(String_to_string) {
    (void)argc;
    (void)tids;

    _SELF_BIND(hw_String *, as_string);
    hw_String_push(hw, &_GET_ARG(0, as_string), '"');
    hw_String_append_data(hw, &_GET_ARG(0, as_string), self->data, self->lenUsed);
    hw_String_push(hw, &_GET_ARG(0, as_string), '"');
    _GET_SELF_TID() = hw_TypeID_string;
    
}

/*******************
 *    Var Arr
 *******************/
DEFN(VarArr_newFrom_conf) {
    (void)args;
    (void)tids;
    (void)argc;

    hw_uint const default_len = 8;
    _SELF(hw_VarArr *);
    
    self = _ALLOC(sizeof(*self)
                + (sizeof(*self->data) * default_len));

    self->data = HW_CAST(void *, self + 1);
    self->len = default_len;
    self->lenUsed = 0;

    self->tid = _GET_ARG(0, as_uint);

    _SELF_ASSIGN(as_arr);
    
}

DEFN(VarArr_delete) {
    (void)args;
    (void)tids;
    (void)argc;

    _SELF_BIND(hw_VarArr *, as_arr);
    hw_TypeSys const *ts = hw->ts;
    hw_byte tid = self->tid;
    hw_VarFn deletefn = hw_Type_getvt(self->tid + ts->types, "delete", 6);
    if (ts->types[self->tid].is_obj) {
        for (hw_uint i = 0; i < self->lenUsed; i++) {
                deletefn(hw, self->data + i, &tid, 1);
        }
    }

    _FREE(self);
    _SELF_ASSIGN(as_arr);
    
}

DEFN(VarArr_push) {
    _SELF_BIND(hw_VarArr *, as_arr);

    HW_DEBUG(
            HW_ASSERT(argc == 2);
            HW_ASSERT(tids[0] == hw_TypeID_array);
            HW_ASSERT(tids[1] == self->tid);
    );
    
    
    if(self->len <= self->lenUsed) {
        self->len *= 2; 
        self = _REALLOC(self, (sizeof(*self))
                            +  (sizeof(*self->data) * (self->len)));
        self->data = HW_CAST(void *, self + 1);
        
    }

    self->data[self->lenUsed] = _GET_ARG_RAW(0);
    self->lenUsed += 1;

    _SELF_ASSIGN(as_arr);
    
}
 
DEFN(VarArr_pushStream) {
    (void)args;
    (void)tids;
    (void)argc;
    (void)hw;
    _SELF_BIND(hw_VarArr *, as_arr);
    
    HW_ASSERT(0 && "NOT IMPLEMENTED");

    _SELF_ASSIGN(as_arr);
    
}

DEFN(VarArr_pop) {
    HW_DEBUG((void)hw;(void)argc;(void)args;(void)tids;);
    HW_DEBUG(HW_LOG("NOT IMPLEMENTED REALLOC%s", ""));

    _SELF_BIND(hw_VarArr *, as_arr);
    self->lenUsed -= 1;
    
}

DEFN(VarArr_popStream) {
    HW_DEBUG((void)hw;(void)argc;(void)args;(void)tids;);
    HW_DEBUG(HW_LOG("NOT IMPLEMENTED REALLOC%s", ""));

    _SELF_BIND(hw_VarArr *, as_arr);
    self->lenUsed -= _GET_ARG(0, as_uint);
    
}

/*******************
 *    Structured Array (SArr)
 *******************/

#define HW_FOREACH(T, iterator, ptr, len)\
    for(T iterator = ptr; iterator < ptr+len; iterator += 1)

DEFN(SArr_newFrom_conf) {
    (void)args;
    (void)tids;
    (void)argc;
    
    _SELF(hw_SArr *);
    hw_uint unitsize = _GET_ARG(0, as_uint);
    HW_DEBUG(HW_ASSERT(unitsize));

    hw_uint const default_len = 8;
    self = _ALLOC(sizeof(*self)
                + (unitsize * default_len));

    self->data = HW_CAST(void *, self + 1);
    self->unitsize = unitsize;
    self->len = default_len;
    self->lenUsed = 0;
    
    _SELF_ASSIGN(as_sarr);
    
}

DEFN(SArr_delete) {
    (void)args;
    (void)tids;
    (void)argc;
    
    _SELF_BIND(hw_SArr* , as_sarr);
    _FREE(self);
    
}

DEFN(SArr_push) {
    (void)args;
    (void)tids;
    (void)argc;
    
    _SELF_BIND(hw_SArr* , as_sarr);
    if(self->len >= self->lenUsed) {
        self->len *= 2;
        self = _REALLOC(self, sizeof(*self)
                            + (self->len * self->unitsize));
    } 
    hw_ptr data = _GET_ARG(0, as_ptr);
    memmove(self->data 
        + (self->unitsize * self->lenUsed)
        , data, self->unitsize);
    self->lenUsed += 1;
    
}

DEFN(SArr_pop) {
    (void)hw;
    (void)args;
    (void)tids;
    (void)argc;
    
    _SELF_BIND(hw_SArr* , as_sarr);
    self->lenUsed -= 1;
    
}

DEFN(SArr_get) {
    (void)hw;
    (void)args;
    (void)tids;
    (void)argc;
    
    _SELF_BIND(hw_SArr* , as_sarr);
    hw_ptr get = _GET_ARG(0, as_ptr);
    hw_uint index = _GET_ARG(1, as_uint);
    memmove(get, self->data + (index * self->unitsize), self->unitsize);
    
}

/*******************
 *    Var List
 *******************/

DEFN(VarList_new) {
    (void)args;
    (void)tids;
    (void)argc;
    
    _SELF(hw_VarList *);
    const hw_uint default_len = 8;
    self = hw_VarList_new(hw, default_len);
    _SELF_ASSIGN(as_list);
    
}

DEFN(VarList_newFrom_copy) {
    (void)argc;
    (void)tids;

    hw_VarList *src = _GET_ARG(0, as_list);
    _SELF(hw_VarList *) = hw_VarList_new(hw, src->lenUsed + 1);
    
    memcpy(self->tid, src->tid, sizeof(*self->tid) * src->lenUsed);
    for (size_t i = 0; i < src->lenUsed; i++) {
        hw_Type *T = hw_TypeSys_get_via_id(hw->ts, self->tid[i]);
        if(T->is_obj) {
            hw_VarFn newFrom_copy = hw_Type_getvt(T, "newFrom_copy", 4*3);
            hw_Var a[2] = { [1] = src->data[i] };
            hw_byte t[2] = { [1] = src->tid[i] };
            newFrom_copy(hw, a, t, 2);
            self->data[i] = a[0];
        } else {
            self->data[i] = src->data[i];
        }
    }
    
    self->lenUsed = src->lenUsed;

    _SELF_ASSIGN(as_list);
    
}

DEFN(VarList_expand) {
    (void)tids;
    (void)argc;

    _SELF_BIND(hw_VarList *, as_list);
    self->len += _GET_ARG(0, as_uint);
    self = _REALLOC(self, ( (sizeof(*self)) 
                          + (sizeof(*self->data) * (self->len))
                        ));

    HW_DEBUG(HW_ASSERT(self));
    self->data = HW_CAST(void *, self + 1);
    self->tid = _REALLOC(self->tid, sizeof(*self->tid) * (self->len));
    HW_DEBUG(HW_ASSERT(self));

    _SELF_ASSIGN(as_list);
    
}

DEFN(VarList_newFrom_fmt) {
    (void)argc;

    hw_Lexer *l = _GET_ARG(0, as_lexer);
    if(!hw_LexToken_is(l->token, HW_LEXTOKEN_PAREN_LEFT)) { 
        _GET_SELF_TID() = hw_TypeID_nil; 
        
    }
    hw_Lexer_next_skipws(l);

    hwfn_VarList_new(hw, args, tids, 1);
    _SELF_BIND(hw_VarList *, as_list);

    while (hw_LexToken_is_not(l->token, HW_LEXTOKEN_END_OF_SOURCE)
        && hw_LexToken_is_not(l->token, HW_LEXTOKEN_PAREN_RIGHT)) {
        hw_Var val = {0};
        hw_byte val_tid = hw_Var_parse(hw, &val, l);
        hw_Var arg[2] = { (hw_Var){.as_list = self }, val };
        hw_byte tid[2] = { hw_TypeID_list, val_tid };
        hwfn_VarList_push_shallow(hw, arg, tid, 2);
        self = arg[0].as_list;
        hw_Lexer_next_skipws(l);
    }

    _SELF_ASSIGN(as_list);
    _GET_SELF_TID() = hw_TypeID_list;
    
}

DEFN(VarList_to_serialize) { // &self, &bytearray
    _SELF_BIND(hw_VarList *, as_list);
    hw_byteArr *buffer = _GET_ARG(0, as_bytearr);
    HW_ARR_PUSHSTREAM(hw, buffer, &self->lenUsed, sizeof(self->lenUsed));
    HW_ARR_PUSHSTREAM(hw, buffer, self->tid, sizeof(*self->tid) * self->lenUsed);
    for (size_t i = 0; i < self->lenUsed; i++) {
        hw_Type *T = hw_TypeSys_get_via_id(hw->ts, self->tid[i]);
        if (T->is_obj) {
            hw_VarFn serialize = hw_Type_getvt(T, "serialize", 5+4);
            args[0] = self->data[i];
            tids[0] = self->tid[i];
            serialize(hw, args, tids, argc);
        } else {
            HW_ARR_PUSHSTREAM(hw, buffer, self->data + i, sizeof(*self->data));
        }
    }
    _SELF_ASSIGN(as_list);
    _SET_ARG(0, as_bytearr, buffer);
    
}

DEFN(VarList_newFrom_deserialize) { // &self, &index, &bytearray
    hw_uint index = _GET_ARG(0, as_uint);
    hw_byteArr *stream = _GET_ARG(1, as_bytearr);
    hw_u32 len = 0;
    hw_byteArr_loadinc(stream, &index, &len, sizeof(len));
    _SELF(hw_VarList *) = hw_VarList_new(hw, len);
    hw_byteArr_loadinc(stream, &index, self->tid, sizeof(*self->tid) * len);
    for (size_t i = 0; i < len; i++) {
        hw_Type *T = hw_TypeSys_get_via_id(hw->ts, self->tid[i]);
        if (T->is_obj) {
            #define HW_CONST_STR(x) x, (sizeof(x)-1)
            hw_VarFn deserialize = hw_Type_getvt(T, HW_CONST_STR("newFrom_deserialize"));
            deserialize(hw, args, tids, argc);
            self->data[i] = args[0];
            index = args[0].as_uint;
        } else {
            hw_byteArr_loadinc(
                stream, &index, self->data + i, sizeof(*self->data));
        }
    }
    self->lenUsed = len;
    _SELF_ASSIGN(as_list);
    _SET_ARG(0, as_uint, index);
    
}

DEFN(VarList_push_shallow) {
    (void)tids;
    (void)argc;

    _SELF_BIND(hw_VarList *, as_list);
    if(self->len <= self->lenUsed) {
        hw_Var expand_args[] = { _GET_SELF(), (hw_Var){.as_uint = self->len * 2} };
        hw_byte expand_args_tids[] = { hw_TypeID_list, hw_TypeID_uint };
        hwfn_VarList_expand(hw, expand_args, expand_args_tids, 2);
        _GET_SELF() = expand_args[0];
        self = _GET_SELF().as_list;
    }
    
    self->data[self->lenUsed] = _GET_ARG_RAW(0);
    self->tid[self->lenUsed] = _GET_ARG_TID(0);

    self->lenUsed += 1;
    
    
}

DEFN(VarList_pop_dtor) {
    HW_DEBUG((void)hw;(void)argc;(void)args;(void)tids;);
    _SELF_BIND(hw_VarList *, as_list);
    HW_ASSERT(0 && "NOT IMPLEMENTED");

    HW_DEBUG(HW_ASSERT(self->lenUsed != 0));
    self->lenUsed -= 1;
    hw_byte tid = self->tid[self->lenUsed];
    
    HW_DEBUG(HW_ASSERT(tid < hw_TypeID_TOTAL));
    hw_Type *T = hw->ts->types + tid;
    if(T->is_obj) {
        hw_Var *top = self->data + self->lenUsed;
        
        HW_DEBUG(
            HW_ASSERT(T->vt[1] == hw_Type_getvt(T, "delete", 6));
        );

        T->vt[1](hw, top, &tid, 1);
    }


    
}

DEFN(VarList_reserve) {
    _SELF_BIND(hw_VarList *, as_list);
    hw_uint size = _GET_ARG(0, as_uint);
    hw_uint avail = self->len - self->lenUsed;
    if(avail < size) { hwfn_VarList_expand(hw, args, tids, argc); }
     // expand will set list proper, so we dont.
}

DEFN(VarList_delete) {
    (void)args;
    (void)tids;
    (void)argc;

    _SELF_BIND(hw_VarList *, as_list);
    for (hw_uint i = 0; i < self->lenUsed; i++) {
        HW_DEBUG(
            printf("{}==== %"PRIu64 "===%" PRIu32 "\n", i, self->lenUsed);
            HW_ASSERT_OP(self->tid[i], <, hw_TypeID_TOTAL, PRIu8, PRIu8));
        hw_Type const *T = hw_TypeSys_get_via_id(hw->ts, self->tid[i]);
        HW_ASSERT(T);
        if(T->is_obj) {
            HW_DEBUG(HW_LOG("USING vt[1] here %s", ""););
            hw_VarFn delete = hw_Type_getvt(T, "delete", 6);
            delete(hw, self->data + i, self->tid + i, 1);
        }
    }
    _FREE(self->tid);
    _FREE(self);
    
}

DEFN(VarList_to_string) {
    (void)argc;
    (void)tids;

    _SELF_BIND(hw_VarList *, as_list);
    hw_String_append_data(hw, &_GET_ARG(0, as_string), (void *)"( ", 2);
    for (size_t i = 0; i < self->lenUsed; i++) {
        args[0] = self->data[i];
        tids[0] = self->tid[i];
        hw_Type *T = hw_TypeSys_get_via_id(hw->ts, tids[0]);
        HW_DEBUG(HW_ASSERT(T));
        hw_VarFn to_string = hw_Type_getvt(T, "to_string", 9);
        to_string(hw, args, tids, 2);
        hw_String_append_data(hw, &_GET_ARG(0, as_string), (void *)" ", 1);
    }

    hw_String_append_data(hw, &_GET_ARG(0, as_string), (void *)")", 1);

    _SELF_ASSIGN(as_list);
    
}

/**************************
 * Section: hw_VarTable
 **************************/
DEFN(SymTable_new) {
    (void)argc;
    (void)tids;

    hw_uint const default_len = 1 << 4;
    _GET_SELF().as_symtable = hw_SymTable_new(hw, default_len);
    _GET_SELF_TID() = hw_TypeID_symtable;
    
}

DEFN(SymTable_newFrom_fmt) {
    (void)argc;

    hw_Lexer *l = _GET_ARG(0, as_lexer);
    if(!hw_LexToken_is(l->token, HW_LEXTOKEN_BRACE_LEFT)) { 
        _GET_SELF_TID() = hw_TypeID_nil; 
        
    }
    hw_Lexer_next_skipws(l);
    
    hwfn_SymTable_new(hw, args, tids, 1);
    _SELF_BIND(hw_SymTable *, as_symtable);
    
    _SELF_ASSIGN(as_symtable);
    
}

// NOTE: IMPLEMENT non hwfn version
DEFN(SymTable_delete) {
    (void)argc;
    (void)tids;

    _SELF_BIND(hw_SymTable *, as_symtable);

    for (size_t i = 0; i < self->len; i++) {
        if(self->key[i] != NULL) {
            HW_DEBUG(HW_ASSERT_OP(
                self->valTs[i], <, hw_TypeID_TOTAL, PRIu8, PRIu8));
            hw_Type const *T = hw->ts->types + self->valTs[i];
            if(T->is_obj) {
                hw_VarFn deletefn = hw_Type_getvt(T, "delete", 6);
                deletefn(hw, self->val + i, self->valTs + i, 1);
            }
            _FREE(self->key[i]);
        }
    }
    _FREE(self);
    
}

DEFN(SymTable_expand) {
    (void)argc;
    (void)tids;

    _SELF_BIND(hw_SymTable *, as_symtable);
    hw_uint const new_len = self->len << 1;
    hw_SymTable *new_table = hw_SymTable_new(hw, new_len);
    for (size_t i = 0; i < new_len; i++) {
        if(self->key[i] != NULL) {
            hw_uint index = hw_SymTable_index(
                    new_table, self->key[i]->data, self->key[i]->len);

            new_table->key[index] = self->key[i];
            new_table->val[index] = self->val[i];
        }
    }

    _FREE(self);
    self = new_table;
    _SELF_ASSIGN(as_symtable);
    
}

DEFN(SymTable_reset) {
    (void)argc;
    (void)tids;
    _SELF_BIND(hw_SymTable *, as_symtable);

    for (size_t i = 0; i < self->len; i++) {
        if(self->key[i] != NULL) {
            HW_THREAD_FREE(hw, self->key[i]);
            hw_Type const *T = hw->ts->types + self->valTs[i];
            if(T->is_obj) {
                hw_VarFn deletefn = hw_Type_getvt(T, "delete", 6);
                deletefn(hw, self->val + i, self->valTs + i, 1);
            }
            self->key[i] = NULL;
            self->valTs[i] = hw_TypeID_nil;
        }
    }

    self->lenUsed = 0;

    
}

DEFN(SymTable_set) {
    (void)argc;
    (void)tids;

    _SELF_BIND(hw_SymTable *, as_symtable);    
    hw_String *key = _GET_ARG(0, as_string); 
    if(self->lenUsed > self->len/2) {
        hwfn_SymTable_expand(hw, args, tids, 1);
        self = args[0].as_symtable;
    }
    
    hw_uint index = hw_SymTable_index(self, key->data, key->lenUsed);
    if(self->key[index] == NULL) { 
        hw_CStr *table_key = _ALLOC( sizeof(*table_key) 
                                  + (sizeof(*key->data) * key->len));
        table_key->data = HW_CAST(void *, table_key + 1);
        memcpy(table_key->data, key->data, key->lenUsed);
        table_key->len = key->lenUsed;

        self->key[index] = table_key;
        self->lenUsed += 1;
    }

    self->val[index] = _GET_ARG_RAW(1);
    self->valTs[index] = _GET_ARG_TID(1);

    
}

DEFN(SymTable_get) {
    (void)hw;
    (void)argc;
    (void)tids;

    // args: 2
    _SELF_BIND(hw_SymTable *, as_symtable);

    hw_String *key = _GET_ARG(0, as_string);
    _SET_ARG_TID(1, hw_TypeID_nil);
    
    hw_uint index = hw_SymTable_index(self, key->data, key->lenUsed);
    if(self->key[index]) {
        _SET_ARG_RAW(1, self->val[index]);
        _SET_ARG_TID(1, self->valTs[index]);
    }

    
}

/**
 * ByteArr
 */
DEFN(byteArr_new) {
    (void)argc;
    (void)tids;

    _SELF(hw_byteArr *);
    HW_ARR_NEW(hw, self, 8);
    _SELF_ASSIGN(as_bytearr);
    
}

DEFN(byteArr_delete) {
    (void)argc;
    (void)tids;

    _SELF_BIND(hw_byteArr *, as_bytearr);
    HW_ARR_DELETE(hw, self);
    
}

DEFN(byteArr_pushptr) {
    (void)argc;
    (void)tids;

    _SELF_BIND(hw_byteArr *, as_bytearr);
    hw_ptr dats = _GET_ARG(0, as_ptr);
    hw_uint dat_size = _GET_ARG(0, as_uint);
    HW_ARR_PUSHSTREAM(hw, self, dats, dat_size);
    _SELF_ASSIGN(as_bytearr);
    
}

/**
 *  Module
 */
DEFN(Module_newFrom_deserialize) // &self, &index, bytearr
{
    (void)argc;
    (void)tids;

    hw_uint index = _GET_ARG(0, as_uint);
    hw_byteArr *stream = _GET_ARG(1, as_bytearr);
    hw_uint mod_size = 0;

    HW_ASSERT(hw_byteArr_loadinc(stream, &index, &mod_size, sizeof(mod_size)));

    hw_Module _m;
    HW_ASSERT(hw_byteArr_loadinc(stream, &index, &_m, sizeof(_m)));
    _SELF(hw_Module *) = hw_Module_newblank(
            hw, _m.fn_count, _m.code_len, _m.data_size, _m.k_count, 0);
    
    HW_ASSERT(hw_byteArr_loadinc(stream, &index, self + 1, mod_size - sizeof(_m)));
    
    hw_uint kdata_index = index;
    for (size_t i = 0; i < self->k_count; i++) {
        hw_Type *T = hw_TypeSys_get_via_id(hw->ts, self->knst_t[i]);
        HW_ASSERT(T);
        if(T->is_obj) {
            hw_VarFn newFrom_deserialize = hw_Type_getvt(T, HW_CONST_STR("newFrom_deserialize"));
            args[1].as_uint = kdata_index + self->knst[i].as_uint;
            newFrom_deserialize(hw, args, tids, argc);
            self->knst[i] = args[0];
        }
    }

    _SET_ARG(0, as_uint, index);
    _SELF_ASSIGN(as_module);
    
}


DEFN(Module_to_serialize) // &self, &bytearray
{
    (void)argc;
    (void)tids;

    hw_Module *m = _GET_SELF().as_module;
    hw_byteArr *buffer = _GET_ARG(0, as_bytearr);
    hw_uint mod_size = hw_Module_calcsize(m);

    HW_ARR_EXPAND(hw, buffer, mod_size);
    HW_ARR_PUSHSTREAM(hw, buffer, &mod_size, sizeof(hw_uint));
    HW_ARR_PUSHSTREAM(hw, buffer, m, mod_size);
    hw_Var *knst = HW_CAST(hw_Var *,  buffer->data 
                                    + (buffer->lenUsed 
                                        - (sizeof(*knst) * m->k_count)));
    hw_uint kdata_index = buffer->lenUsed;
    for (size_t i = 0; i < m->k_count; i++) {
        hw_Type *T = hw_TypeSys_get_via_id(hw->ts, m->knst_t[i]);
        if(T->is_obj) {
            hw_VarFn to_serialize = hw_Type_getvt(
                    T, HW_CONST_STR("to_serialize"));
            args[0] = knst[i];
            args[1].as_bytearr = buffer;
            hw_uint index = kdata_index - buffer->lenUsed;
            to_serialize(hw, args, tids, 2);
            buffer = args[1].as_bytearr;
            knst[i].as_uint = index;
        }
    }
    
    _SET_ARG(0, as_bytearr, buffer);
    _GET_SELF().as_module = m;
    
}

DEFN(Module_delete) {
    (void)argc;
    (void)tids;
    hw_Module_delete(hw, _GET_SELF().as_module);
}

/*
 * Default TypeSys
 */
#define FNINFO(T, _name, _mutc, ...)\
    {{\
        .name = (void *) #_name\
      , .name_size = sizeof(#_name)-1\
      , .arg_count = sizeof((hw_byte[]){__VA_ARGS__})/sizeof(hw_byte)\
      , .mut_count = _mutc\
      , .types = (hw_byte[]){__VA_ARGS__, hw_TypeID_nil}\
    }, hwfn_##T##_##_name} 

#define _TYPEVT_MAX 8

const struct {
    hw_FnInfo info;
    hw_VarFn  fn;
} static TYPEVT[hw_TypeID_TOTAL][_TYPEVT_MAX]= {

    [hw_TypeID_uint] = {
        FNINFO(uint, to_string, 1, hw_TypeID_uint, hw_TypeID_string)
    },

    [hw_TypeID_int] = {
        FNINFO(int, to_string, 1, hw_TypeID_int, hw_TypeID_string)
    },

    [hw_TypeID_float] = {
        FNINFO(float, to_string, 1, hw_TypeID_float, hw_TypeID_string)
    },

    [hw_TypeID_string] = {
        FNINFO(String, new, 0, hw_TypeID_string),
        FNINFO(String, newFrom_deserialize, 0, hw_TypeID_string, hw_TypeID_uint, hw_TypeID_bytearr),
        FNINFO(String, delete, 0, hw_TypeID_string),
        FNINFO(String, newFrom_data, 0, hw_TypeID_string
                , hw_TypeID_ptr, hw_TypeID_uint),
        
        FNINFO(String, newFrom_copy, 0, hw_TypeID_string),
        FNINFO(String, to_string, 1, hw_TypeID_string, hw_TypeID_string),
        FNINFO(String, to_serialize, 1, hw_TypeID_string, hw_TypeID_bytearr)
    },

    [hw_TypeID_array] = {
        FNINFO(VarArr, newFrom_conf, 1, hw_TypeID_array, hw_TypeID_uint),
        FNINFO(VarArr, delete, 0, hw_TypeID_array),
        FNINFO(VarArr, push, 1, hw_TypeID_array, hw_TypeID_any),
        FNINFO(VarArr, pushStream, 2, hw_TypeID_array, hw_TypeID_uint, hw_TypeID_ptr),
        FNINFO(VarArr, pop, 0, hw_TypeID_array),
        //FNINFO(VarArr, popStream, 1, hw_TypeID_array, hw_TypeID_uint),
    },

    [hw_TypeID_list] = {
        FNINFO(VarList, new, 0, hw_TypeID_list),
        FNINFO(VarList, newFrom_copy, 1, hw_TypeID_list, hw_TypeID_list),
        FNINFO(VarList, newFrom_deserialize, 1, hw_TypeID_list, hw_TypeID_uint, hw_TypeID_bytearr),
        FNINFO(VarList, delete, 0, hw_TypeID_list),
        FNINFO(VarList, push_shallow, 1, hw_TypeID_list, hw_TypeID_any),
        FNINFO(VarList, pop_dtor, 0, hw_TypeID_list),
        FNINFO(VarList, to_string, 1, hw_TypeID_list, hw_TypeID_string),
        FNINFO(VarList, to_serialize, 0, hw_TypeID_list, hw_TypeID_bytearr)
    },

    [hw_TypeID_symtable] = {
        FNINFO(SymTable, new, 0, hw_TypeID_symtable),
        FNINFO(SymTable, delete, 0, hw_TypeID_symtable),
        FNINFO(SymTable, get, 2
                , hw_TypeID_symtable, hw_TypeID_string, hw_TypeID_any),
        FNINFO(SymTable, set, 2
                , hw_TypeID_symtable, hw_TypeID_string, hw_TypeID_any),
        FNINFO(SymTable, reset, 0, hw_TypeID_symtable)
    },

    [hw_TypeID_bytearr] = {
        FNINFO(byteArr, new, 0, hw_TypeID_bytearr),
        FNINFO(byteArr, delete, 0, hw_TypeID_bytearr),
        FNINFO(byteArr, pushptr, 2
                , hw_TypeID_bytearr, hw_TypeID_ptr, hw_TypeID_uint)
    },

    [hw_TypeID_module] = {
        FNINFO(Module, newFrom_deserialize, 1
            , hw_TypeID_module, hw_TypeID_uint, hw_TypeID_bytearr),
        FNINFO(Module, delete, 1, hw_TypeID_module),
        FNINFO(Module, to_serialize, 1
            , hw_TypeID_module, hw_TypeID_bytearr)
    },
};


#define TYPE(hwtype, ctype, isobj)\
    (hw_Type){\
        .id = hw_TypeID_##hwtype            \
      , .is_obj = isobj                     \
      , .name = #hwtype                     \
      , .name_size = sizeof(#hwtype)-1      \
      , .unitsize = sizeof(ctype)           \
      , .c_name = #ctype                    \
      , .c_name_size = sizeof(#ctype)-1     \
    }

static void _default_setall_atoms(hw_TypeSys *ts)
{
    HW_ASSERT(hw_TypeSys_set(ts, &TYPE(any,   hw_byte,  0)));
    HW_ASSERT(hw_TypeSys_set(ts, &TYPE(nil,   hw_byte,  0)));
    HW_ASSERT(hw_TypeSys_set(ts, &TYPE(ptr,   hw_ptr,   0)));
    HW_ASSERT(hw_TypeSys_set(ts, &TYPE(int,   hw_int,   0)));
    HW_ASSERT(hw_TypeSys_set(ts, &TYPE(uint,  hw_uint,  0)));
    HW_ASSERT(hw_TypeSys_set(ts, &TYPE(float, hw_float, 0)));
}

static void _default_setall_objects(hw_TypeSys *ts)
{
    HW_ASSERT(hw_TypeSys_set(ts, &TYPE(array,       hw_VarArr,     1)));
    HW_ASSERT(hw_TypeSys_set(ts, &TYPE(sarr,        hw_SArr,       1)));
    HW_ASSERT(hw_TypeSys_set(ts, &TYPE(list,        hw_VarList,    1)));
    HW_ASSERT(hw_TypeSys_set(ts, &TYPE(symtable,    hw_SymTable,   1)));
    HW_ASSERT(hw_TypeSys_set(ts, &TYPE(symtableord, hw_SymTableOrd,1)));
    HW_ASSERT(hw_TypeSys_set(ts, &TYPE(bytearr,     hw_byteArr,    1)));
    HW_ASSERT(hw_TypeSys_set(ts, &TYPE(string,      hw_String,     1)));
    HW_ASSERT(hw_TypeSys_set(ts, &TYPE(module,      hw_Module,     1)));
    HW_ASSERT(hw_TypeSys_set(ts, &TYPE(thread,      hw_State,      1)));
}

#undef TYPE

static void _default_vt_binds(hw_TypeSys *ts)
{
    for (size_t tid = 0; tid < hw_TypeID_TOTAL; tid++) {
        hw_Type *T = ts->types + tid;
        size_t vt_id = 0;
        for (; vt_id < _TYPEVT_MAX; vt_id++) {
            hw_FnInfo finfo = TYPEVT[tid][vt_id].info;
            hw_VarFn fn = TYPEVT[tid][vt_id].fn;

            T->vt[vt_id] = fn;
            T->vtinfo[vt_id]= finfo;
            if (!finfo.name_size && !fn) {
                T->vt[vt_id] = hwfn_VarFn_UNREACHABLE;
            }
        }
        T->vt_count = vt_id;
    }
}

static void _show_types(hw_TypeSys const *ts)
{
    HW_LOG("Showing all the Loaded Types, Total:%d", (int)ts->types_total);
    for (size_t i = 0; i < ts->types_total; i++) {
        hw_Type *T = ts->types + i;
        if(!T->name_size) { hw_loglnp("\t(%d) = nil", (int)i); goto _L_skip; }
        hw_loglnp("\t(%d)\n"
                  "\tName: %.*s\n"
                  "\tID: %d\n"
                  "\tUnitsize: %d\n"
                  "\tFunctions: %d\n"
                , (int)i
                , (int)T->name_size, T->name
                , (int)T->id
                , (int)T->unitsize
                , (int)T->vt_count);
        for (size_t vt_i = 0; vt_i < T->vt_count; vt_i++) {
            hw_VarFn fn = T->vt[vt_i];
            hw_FnInfo finfo = T->vtinfo[vt_i];
            hw_loglnp("\t ID: %d\n"
                      "\t\tName: %.*s\n"
                      "\t\tLocation: %p\n"
                      "\t\tArguments: %d\n"
                      "\t\tMutables: %d"
                , (int)vt_i 
                , (int)finfo.name_size, finfo.name
                , (void*)fn
                , (int)finfo.arg_count
                , (int)finfo.mut_count);
            hw_logp("\t\tArgs: ");
            for (size_t arg_i = 0; arg_i < finfo.arg_count; arg_i++) {
                HW_ASSERT(finfo.types[arg_i] < hw_TypeID_TOTAL);
                char *name = (char *)ts->types[finfo.types[arg_i]].name;
                hw_uint name_size = ts->types[finfo.types[arg_i]].name_size;
                hw_logp(" %.*s |", (int)name_size, name);
            }
            hw_logp("\n");
        }
        _L_skip:;
    }
}

hw_TypeSys* hw_TypeSys_new_default(hw_Allocator *allocator)
{
    hw_TypeSys *ts = hw_TypeSys_new(hw_TypeID_TOTAL, allocator);
    HW_ASSERT(ts != NULL);
    
    _default_setall_atoms(ts);
    _default_setall_objects(ts);
    _default_vt_binds(ts);
    HW_DEBUG(
        _show_types(ts);
    );
    return ts;
}

