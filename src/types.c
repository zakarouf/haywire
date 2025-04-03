#include "dev.h"
#include "hwfn.h"
#include "cstd.h"

#define _ALLOC(SIZE)            HW_THREAD_ALLOC(hw, SIZE)
#define _REALLOC(PTR, SIZE)     HW_THREAD_REALLOC(hw, PTR, SIZE)
#define _FREE(PTR)              HW_THREAD_FREE(hw, PTR)

/**
 * GENERIC
 */
hw_uint hw_Var_parse(hw_State *hw, hw_Var *result, hw_Lexer *l)
{
    hw_uint type = 0;
    switch (l->token.type) {
        break; case HW_LEXTOKEN_SYMBOL: {
            #define symeq(x) hw_Lexer_tiseq(l, x, sizeof(x)-1)

            if(symeq("nil")) {
                type = hw_TypeID_nil;
            } else if(symeq("true")) {
                result->as_bool = HW_TRUE;
                type = hw_TypeID_bool;
            }  else if(symeq("false")) {
                result->as_bool = HW_FALSE;
                type = hw_TypeID_bool;
            } 
            #undef symeq
        }
        break; case HW_LEXTOKEN_NUMBER: {
            hw_strto_int(&result->as_int, l->token.start, l->token.size);
            type = hw_TypeID_int;
        }
        break; case HW_LEXTOKEN_FLOAT: {
            hw_strto_float(&result->as_float, l->token.start, l->token.size);
            type = hw_TypeID_float;
        }
        break; case HW_LEXTOKEN_PAREN_LEFT: {
            hw_Var args[2] = { (hw_Var){.as_list = NULL}
                             , (hw_Var){.as_lexer = l} };
            hw_byte tid[2] = { hw_TypeID_nil, hw_TypeID_nil };

            hwfn_VarList_newFrom_fmt(hw, args, tid, 2);
            *result = args[0];
            type = tid[0];
        }
        break; case HW_LEXTOKEN_BRACE_LEFT: {
            hw_Var args[2] = { (hw_Var){.as_symtable = NULL}
                             , (hw_Var){.as_lexer = l} };
            hw_byte tid[2] = { hw_TypeID_nil, hw_TypeID_nil };

            hwfn_SymTable_newFrom_fmt(hw, args, tid, 2);
            *result = args[0];
            type = tid[0];
        }
        break; case HW_LEXTOKEN_DOUBLE_QUOTE: {
            hw_Var args[2] = { (hw_Var){.as_string = NULL}
                             , (hw_Var){.as_lexer = l} };
            hw_byte tid[2] = { hw_TypeID_nil, hw_TypeID_nil };

            hwfn_String_newFrom_fmt(hw, args, tid, 2);
            *result = args[0];
            type = tid[0];
        }
    }    
    return type;
}


/*------------------------ String ---------------------------------*/
hw_String *hw_String_new(hw_State *hw, hw_u32 _len)
{
    hw_String *self = _ALLOC( sizeof(hw_String)
                            + (sizeof(*self->data) * _len) );

    self->data = HW_CAST(void *, self + 1);
    self->len = _len;
    self->lenUsed = 0;
    return self;
}

void hw_String_delete(hw_State *hw, hw_String *self) { _FREE(self); }

hw_String *hw_String_newFrom_data(
    hw_State *hw, hw_byte const *data, hw_u32 _len)
{
    hw_String *self = hw_String_new(hw, _len);
    self->lenUsed = _len;
    memcpy(self->data, data, _len * sizeof(*self->data));
    return self;
}

void hw_String_nullterm(hw_State *hw, hw_String **self)
{
    hw_String_push(hw, self, '\0');
    (*self)->lenUsed -= 1;
}

hw_bool hw_String_expand(hw_State *hw, hw_String **selfp, hw_u32 const by)
{
    hw_String *self = *selfp;    
    self->len += by;
    self = _REALLOC(*selfp, sizeof(*self) 
                                    + (sizeof(*self->data) * self->len) );
    self->data = HW_CAST(void *, self + 1);
    *selfp = self;
    return 1;
}

void hw_String_append_data(
    hw_State *hw, hw_String **selfp, hw_byte const *data, hw_u32 _len)
{
    hw_String *self = *selfp;
    if((self->len) < (self->lenUsed + _len)) {
        hw_String_expand(hw, &self, _len + 1);
    }

    memcpy(self->data + self->lenUsed, data, _len);
    self->lenUsed += _len;
    *selfp = self;
}

void hw_String_push(hw_State *hw, hw_String **selfp, hw_byte ch)
{
    hw_String *self = *selfp;
    if(self->lenUsed >= self->len) {
        hw_String_expand(hw, &self, self->len);
        *selfp = self;
    }
    self->data[self->lenUsed] = ch;
    self->lenUsed += 1;
}

void hw_String_push_hexchar(hw_State *hw, hw_String **selfp, hw_byte n1, hw_byte n2)
{
    if(!hw_char_is_hex(n1)) { hw_String_push(hw, selfp, n1); return; }
    if(!hw_char_is_hex(n2)) { hw_String_push(hw, selfp, n2); return; }
    n1 = hw_char_hex_to_num(n1);
    n2 = hw_char_hex_to_num(n2);
    hw_byte xh = (n1 << 4) | (n2 & 0xf);
    hw_String_push(hw, selfp, xh);
}

hw_String* hw_String_newFrom_dataRaw(
    hw_State *hw, hw_byte const *data, hw_u32 _len) {
    hw_byte const *data_end = data + _len;
    
    hw_String *self = hw_String_new(hw, _len);
    hw_byte const *start = data;

    while(data < data_end) {
        if(*data == '\\') {
            hw_String_append_data(hw, &self, start, data - start);
            data += 1;
            
            #define next(ch)\
                { hw_String_push(hw, &self, ch); data += 1; }

            if(data >= data_end) { return self; }
            switch (*data) {
                case 'n':  next('\n'); break; // Newline
                case 't':  next('\t'); break; // Tab
                case 'r':  next('\r'); break; // Carriage return
                case '0':  next('\0'); break; // Null character
                case 'a':  next('\a'); break; // Alert (bell)
                case 'b':  next('\b'); break; // Backspace
                case 'f':  next('\f'); break; // Form feed
                case 'v':  next('\v'); break; // Vertical tab
                case '\\': next('\\'); break; // Backslash
                case '\'': next('\''); break; // Single quote
                case '\"': next('\"'); break; // Double quote

                #define check_nextx() { \
                    data++; if(data >= data_end) { return self; }\
                }

                case 'x': {
                            check_nextx(); hw_byte n1 = *data;
                            check_nextx(); hw_byte n2 = *data; 
                            hw_String_push_hexchar(hw, &self, n1, n2);
                        } break;
                default: next(*data);  break;
            }

            start = data;
        } else if(*data == '\"') {
            hw_String_append_data(hw, &self, start, data - start);
            return self;
        }
        data += 1;
    }
    return self;
}

void hw_String_append_fmt(hw_State *hw, hw_String **buffer, char const *restrict format, ...)
{
    va_list vargs;
    va_start(vargs, format);
    hw_String *buf = *buffer;
    hw_uint len = vsnprintf(NULL, 0, format, vargs)+1;
    va_end(vargs);

    if((len + buf->lenUsed) > buf->len) {
        hw_String_expand(hw, &buf, len + 1);
    }
    va_start(vargs, format);
    buf->lenUsed += vsnprintf(
            (char *)buf->data + buf->lenUsed
          , buf->len - buf->lenUsed, format, vargs);
    va_end(vargs);
    *buffer = buf;
}


/*------------------------ SymTable ---------------------------------*/

static void _symtable_print(hw_State *s, hw_SymTable *p)
{
    HW_LOG("SymTable  %p", (void *)p);
    for (size_t i = 0; i < p->len; i++) {
        if(p->key[i] != NULL) {
            HW_LOG("[%"PRIu64"]\"%.*s\": %.*s", i
                , (int)p->key[i]->len, p->key[i]->data
                , (int)s->ts->types[p->valTs[i]].name_size, s->ts->types[p->valTs[i]].name);
        } else {
            HW_LOG("[%"PRIu64"]%p: %.*s", i, (void *)p->key[i]
                , (int)s->ts->types[p->valTs[i]].name_size, s->ts->types[p->valTs[i]].name);
        }
    }
}

hw_SymTable *hw_SymTable_new(hw_State *hw, hw_uint len)
{
    hw_SymTable *self;
    hw_uint const size = sizeof(*self)
                            +  (sizeof(*self->key) * len)
                            +  (sizeof(*self->val) * len)
                            +  (sizeof(*self->valTs) * len);
    self = _ALLOC(size);
    memset(self, 0, size);

    self->key = HW_CAST(void *, self + 1);
    self->val = HW_CAST(void *, self->key + len);
    self->valTs = HW_CAST(void *, self->val + len);
    self->len = len;
    self->lenUsed = 0;

    for (size_t i = 0; i < len; i++) {
        self->key[i] = NULL;
    }

    HW_DEBUG(_symtable_print(hw, self));
    return self;
}

hw_uint hw_SymTable_index(hw_SymTable *sym, hw_byte const *key, hw_uint key_size)
{
    hw_uint index = hw_hash_string_fnv(key, key_size) % sym->len;
    HW_DEBUG(HW_LOG("HASH %"PRIu64, index));

    while (sym->key[index]) {
        if(sym->key[index]->len == key_size) {
            if(memcmp(key, sym->key[index]->data, key_size) == 0) {
                return index;
            }
        }
        index += 1; if(index >= sym->len) index = 0;
    }
    return index;
}

void hw_SymTable_set(
    hw_State *hw, hw_SymTable **t
  , hw_byte const *key, hw_uint key_size
  , hw_Var value, hw_byte value_t)
{
    hw_String str = { 
        .data = (hw_byte *) key
      , .len = key_size
      , .lenUsed = key_size
    };

     hw_Var st_args[3] = {
        [0].as_symtable = *t
      , [1].as_string = &str
      , [2] = value
    };

    hw_byte st_tid[3] = { hw_TypeID_symtable, hw_TypeID_string, hw_TypeID_nil };

    st_tid[2] = value_t;
    hwfn_SymTable_set(hw, st_args, st_tid, 3);
    *t = st_args[0].as_symtable;
}

hw_Var hw_SymTable_get(hw_State *hw, hw_SymTable *s, hw_CStr key)
{
    hw_String k = {
        key.data, .len = key.len, .lenUsed = key.len
    };

    hw_Var args[3] = { [0].as_symtable = s, [1].as_string = &k };
    hw_byte tid[3] = { hw_TypeID_symtable, hw_TypeID_string, hw_TypeID_nil };

    hwfn_SymTable_get(hw, args, tid, 3);
    return args[2];
}

/*------------------------------- VarList --------------------------------*/
hw_VarList *hw_VarList_new(hw_State *hw, hw_u32 len)
{
    hw_VarList *self = _ALLOC(sizeof(*self) + (sizeof(hw_Var) * len));

    self->len = len;
    self->lenUsed = 0;

    self->data = HW_CAST(void *, self + 1);
    self->tid  = _ALLOC(sizeof(*self->tid) * len);
    return self;
}

void hw_VarList_delete(hw_State *hw, hw_VarList *self)
{
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

void hw_VarList_expand(hw_State *hw, hw_VarList **vl, hw_u32 by)
{
    hw_VarList *self = *vl;
    self->len += by;
    self = _REALLOC(self, ( (sizeof(*self)) 
                          + (sizeof(*self->data) * (self->len))
                        ));

    HW_DEBUG(HW_ASSERT(self));
    self->data = HW_CAST(void *, self + 1);
    self->tid = _REALLOC(self->tid, sizeof(*self->tid) * (self->len));
    *vl = self;
}

void hw_VarList_push_shallow(
        hw_State *hw, hw_VarList **vl, hw_Var v, hw_byte tid) {

    hw_VarList *self = *vl;
    if(self->len <= self->lenUsed) {
        hw_VarList_expand(hw, &self, self->len);
    }
    
    self->data[self->lenUsed] = v;
    self->tid[self->lenUsed] = tid;
    self->lenUsed += 1;

    **vl = *self;
}


/*------------------------------- VarList --------------------------------*/
hw_SymTableOrd *hw_SymTableOrd_new(hw_State *hw, hw_u32 len)
{
    hw_SymTableOrd *table = _ALLOC(sizeof(*table));

    table->indices  = _ALLOC(sizeof(*table->indices) * len); 
    table->keys     = _ALLOC(sizeof(*table->keys) * len);
    table->key_size = _ALLOC(sizeof(*table->key_size) * len);
    table->vals     = _ALLOC(sizeof(*table->vals) * len);
    table->valT     = _ALLOC(sizeof(*table->valT) * len);

    memset(table->key_size, 0, sizeof(*table->key_size) * len);
    memset(table->indices, -1, sizeof(*table->indices) * len);

    table->len = len;
    table->vlen = len;
    table->lenUsed = 0;
    table->vlenUsed = 0;

    return table;
}

void hw_SymTableOrd_delete(hw_State *hw, hw_SymTableOrd *table)
{
    for (size_t i = 0; i < table->lenUsed; i++) {
        _FREE(table->keys[i]);
        hw_Type *T = hw_TypeSys_get_via_id(hw->ts, table->valT[i]);
        if (T->is_obj) {
            HW_VAR_CALLEX(hw, table->valT[i], table->vals[i]
                    , "delete", (), (),);
        }
    }
    _FREE(table->valT);
    _FREE(table->vals);
    _FREE(table->keys);
    _FREE(table->key_size);
    _FREE(table->indices);
    _FREE(table);
}

static void _SymTableOrd_expand_indices(hw_State *hw, hw_SymTableOrd *table, hw_u32 by)
{
    _FREE(table->indices);
    table->len += by;
    table->indices =_ALLOC((sizeof(*table->indices) * table->len)); 
    memset(table->indices, -1, sizeof(*table->indices) * table->len);

    for (size_t i = 0; i < table->lenUsed; i++) {
        table->indices[hw_SymTableOrd_get_index(table
                , table->keys[i]
                , table->key_size[i])] = i;
    }
}

static void _SymTableOrd_expand_data(hw_State *hw, hw_SymTableOrd *table, hw_u32 by)
{
    table->vlen += by;
    table->keys =       _REALLOC(table->keys, sizeof(*table->keys) * table->vlen);
    table->key_size =   _REALLOC(table->key_size, sizeof(*table->key_size) * table->vlen);
    table->vals =       _REALLOC(table->vals, sizeof(*table->vals) * table->vlen);
    table->valT =       _REALLOC(table->valT, sizeof(*table->valT) * table->vlen);

    memset(table->key_size + table->vlen-by, 0, sizeof(*table->key_size) * by);
}

hw_u32 hw_SymTableOrd_get_index(hw_SymTableOrd *table, hw_byte const *str, hw_u32 len)
{
    hw_u32 i = hw_hash_string_fnv(str, len) % (hw_uint)table->len;
    HW_DEBUG(HW_LOG("HASH(%.*s) = %u", len, str, i));
    hw_u32 id = table->indices[i];
    while (id < table->vlenUsed) {
        HW_DEBUG(HW_LOG("ID(%u) < %u, H_%u", id, table->vlenUsed, i));
        if(table->key_size[id] == len
        && 0 == memcmp(table->keys[id], str, len)) { 
            HW_DEBUG(HW_LOG("DUP KEY @ %u", id));
            return i; 
        }
        i = i + 1;
        if(i > table->len) { i = 0; }
        id = table->indices[i];
    }

    return i;
}

hw_u32 hw_SymTableOrd_push(hw_State *hw, hw_SymTableOrd *table
                , hw_Var v, hw_byte vtid)
{
    if(table->vlenUsed >= table->vlen) {
        _SymTableOrd_expand_data(hw, table, table->vlen);
    }
    
    hw_u32 top = table->vlenUsed;
    table->vlenUsed += 1;

    table->vals[top] = v;
    table->valT[top] = vtid;
    return top;
}

hw_bool hw_SymTableOrd_setkey(hw_State *hw, hw_SymTableOrd *table
                            , hw_byte const *key, hw_u32 keysize, hw_u32 id)
{
    HW_DEBUG(
        if(id >= table->vlenUsed ) { HW_LOG("ID POISON %"PRIu32, id);  
                                 return 0; }
    )
    if(table->key_size[id]) { return 0; }
    hw_u32 index = hw_SymTableOrd_get_index(table, key, keysize);
    if(table->indices[index] < table->vlenUsed) return 0;

    table->key_size[id] = keysize;
    table->keys[id] = _ALLOC(keysize);
    memcpy(table->keys[id], key, keysize);

    if(table->lenUsed >= table->len >> 1) {
        _SymTableOrd_expand_indices(hw, table, table->len);
    }
    table->lenUsed +=1;
    return 1;
}

hw_bool hw_SymTableOrd_set(hw_State *hw, hw_SymTableOrd *table
                           , hw_byte const *key, hw_u32 keysize
                           , hw_Var v, hw_byte vtid)
{
    if(table->lenUsed >= table->len >> 1) {
        _SymTableOrd_expand_indices(hw, table, table->len);
    }
                    // return either id, where key is matched and id < vlenUsed
                    //        or id = 0xFFFFFFFF
    hw_u32 index = hw_SymTableOrd_get_index(table, key, keysize);
    hw_u32 id = table->indices[index];
    
    hw_bool update = 0;
    if(table->vlenUsed < id) { // true if new key
        id = hw_SymTableOrd_push(hw, table, v, vtid);
        table->indices[index] = id;
        table->key_size[id] = keysize;
        table->keys[id] = _ALLOC(keysize);
        memcpy(table->keys[id], key, keysize);
        table->lenUsed += 1;
        update = 1;
    }

    table->vals[id] = v;
    table->valT[id] = vtid;
    return update;
}


