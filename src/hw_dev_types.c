#include "hw.h"
#include "hw_dev.h"
#include <limits.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <inttypes.h>
#include <assert.h>

hw_TypeSys *hw_TypeSys_new(hw_uint type_count, hw_Allocator *allocator)
{
    const hw_uint size = sizeof(hw_TypeSys) 
                        + ( sizeof(hw_Type) * type_count );
    hw_TypeSys *tsys = allocator->alloc(allocator, size);
    memset(tsys, 0, size);
    
    tsys->types = (void *)(tsys + 1);
    tsys->types_total = type_count;
    tsys->types_used = 0;
    return tsys;
}

void hw_TypeSys_delete(hw_TypeSys *t, hw_Allocator *allocator)
{
    void (*_free)(hw_Allocator *self, void *) = allocator->free;
    _free(allocator, t);
}

hw_Type *hw_TypeSys_set(hw_TypeSys *ts, hw_Type const *type)
{
    if(type->id >= ts->types_total) { return NULL; }
    hw_Type *dest = &ts->types[type->id];

    HW_DEBUG(
        if(dest->name_size) {
            HW_LOG("RE SET OF TypeSys Type %s", dest->name);
        }
    )

    memcpy(dest, type, sizeof(*type));
    return dest;
}

hw_Type *hw_TypeSys_get(hw_TypeSys const *ts, char const *key, hw_uint key_size)
{
    for (size_t i = 0; i < ts->types_total; i++) {
        hw_Type *T = ts->types + i;
        if(key_size == T->name_size
        && 0 == memcmp(T->name, key, key_size)) {
            return T;
        }
    }
    return NULL;
}

hw_Type *hw_TypeSys_get_via_id(hw_TypeSys const *ts, hw_uint typeid)
{
    if(typeid >= hw_TypeID_TOTAL) return NULL;
    return ts->types + typeid;
}

hw_VarFn hw_Type_getvt(hw_Type const *T, char const *name, hw_uint name_size)
{
    HW_LOG("Requested VT:%.*s for TYPE:%.*s"
            , (int)name_size, name, (int)T->name_size, T->name);
    
    for (size_t i = 0; i < T->vt_count; i++) {
        if(T->vtinfo[i].name_size == name_size) {
            if(!memcmp(T->vtinfo[i].name, name, name_size)) {
                return T->vt[i];
            }
        }
    }

    HW_DEBUG(
        HW_LOG("No function exist '%.*s' for type: '%.*s'"
            , (int)name_size, name, (int)T->name_size, T->name);
    );
    return hw_VarFn_UNREACHABLE;
}

/**********************************************************************/
/* All Types VTables:
 */
/**********************************************************************/
#define DEFN(NAME)\
    hw_VarP NAME (              \
        hw_State     *state     \
      , hw_Var       *args      \
      , hw_byte      *tids      \
      , hw_uint const argc)

#define _ALLOC(SIZE)            HW_THREAD_ALLOC(state, SIZE)
#define _REALLOC(PTR, SIZE)     HW_THREAD_REALLOC(state, PTR, SIZE)
#define _FREE(PTR)              HW_THREAD_FREE(state, PTR)

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
#define _SET_ARG(n, as, val) { _GET_ARG(n).as  = val; }
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
                , state->ts->types[tids[i]].name_size      \
                , state->ts->types[tids[i]].name           \
                , i);                                   \
            }\
        }\
    } while(0)

/*************************************************************************
 *                          PRIVATE
 *************************************************************************/
DEFN(hw_VarFn_UNREACHABLE) {
    (void)state;
    (void)args;
    (void)tids;
    (void)argc;
    HW_ASSERT(0 && "Reached Unreachable");
    return HW_VARP_NIL();
}

/*******************
// STRINGS
 *******************/
DEFN(hw_String_new) {
    (void)args;
    (void)tids;
    (void)argc;

    const hw_uint default_size = 8;
    _SELF(hw_String *) = _ALLOC(sizeof(hw_String)
                            + (sizeof(*self->data) * default_size));

    self->len = default_size;
    self->lenUsed = 0;

    _SELF_ASSIGN(as_string);
    return HW_VARP_NIL();
}

DEFN(hw_String_delete) {
    (void)args;
    (void)tids;
    (void)argc;

    _SELF_BIND(hw_String *, as_string);
    _FREE(self);
    return HW_VARP_NIL();
}

DEFN(hw_String_newFrom_data) {
    (void)tids;
    (void)argc;

    _SELF(hw_String *);
    
    hw_byte *data = _GET_ARG(0, as_byte_p);
    hw_uint dsize = _GET_ARG(1, as_uint);

    self = _ALLOC( (sizeof(*self))
                  +(sizeof(*self->data) * dsize));

    self->data = HW_CAST(void *, self + 1);
    self->len = dsize;
    self->lenUsed = dsize;

    memcpy(self->data, data, dsize);

    _SELF_ASSIGN(as_string);
    return HW_VARP_NIL();
}

DEFN(hw_String_newFrom_copy) {
    (void)args;
    (void)tids;
    (void)argc;

    hw_String const *string = _GET_ARG(0, as_string);

    hw_Var const data = _MAKE_VAR(string->data, as_ptr);
    hw_Var const size = _MAKE_VAR(string->lenUsed, as_uint);

    hw_Var ar[] = { _GET_SELF(), data, size };
    hw_byte ti[] = { _GET_SELF_TID(), hw_TypeID_ptr, hw_TypeID_uint };
    hw_VarP ret = hw_String_newFrom_data(state, ar, ti, 3);
    _SET_ARG_RAW(0, ret.value);
    _SET_ARG_TID(0, hw_TypeID_string);

    return ret;
}

DEFN(hw_String_append_cstr) {
    (void)tids;
    (void)argc;

    _SELF_BIND(hw_String *, as_string);
    hw_byte *data = _GET_ARG(0, as_byte_p);
    hw_uint dsize = _GET_ARG(1, as_uint);

    hw_uint const availiable_len = self->len - self->lenUsed;
    if(availiable_len < dsize) {
        self->len += (1+dsize);
        self = _REALLOC(self, (sizeof(*self))
                            +  (sizeof(*self->data) * (self->len)));
        self->data = HW_CAST(void *, self + 1);
    }
    
    memcpy(self->data + self->lenUsed, data, dsize);
    self->lenUsed += dsize;
    _SELF_ASSIGN(as_string);
    return HW_VARP_NIL();
}

DEFN(hw_String_newFrom_file) {
    HW_DEBUG(_CHECK_ARGS(2, hw_TypeID_string, hw_TypeID_string));

    hw_String *path = _GET_ARG(0, as_string);
    hw_Var filesize;
    hw_Var filedata = { 
        .as_ptr = hw_loadfile(
                state, (void const *)path->data
                , path->lenUsed, &filesize.as_uint)
    };
    
    hw_Var args_list[] = { {0}, filedata, filesize };
    hw_byte tids_list[] = { 0, hw_TypeID_ptr, hw_TypeID_uint };
    hw_String_newFrom_data(state, args_list, tids_list, 3);

    if(filedata.as_ptr == NULL) {
        return HW_VARP_ERROR(
            &((hw_Error){.error = 0, .str = (void*)"FNF", .str_size = 3}));
    }
    
    _GET_SELF().as_string = args_list[0].as_string;
    return HW_VARP_NIL();
}

/*******************
 *    Var Arr
 *******************/
DEFN(hw_VarArr_newFrom_conf) {
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
    return HW_VARP_NIL();
}

DEFN(hw_VarArr_delete) {
    (void)args;
    (void)tids;
    (void)argc;

    _SELF_BIND(hw_VarArr *, as_arr);
    hw_TypeSys const *ts = state->ts;
    hw_byte tid = self->tid;
    hw_VarFn deletefn = hw_Type_getvt(self->tid + ts->types, "delete", 6);
    if (ts->types[self->tid].is_obj) {
        for (hw_uint i = 0; i < self->lenUsed; i++) {
                deletefn(state, self->data + i, &tid, 1);
        }
    }

    _FREE(self);
    _SELF_ASSIGN(as_arr);
    return HW_VARP_NIL();
}

DEFN(hw_VarArr_push) {
    _SELF_BIND(hw_VarArr *, as_arr);

    HW_DEBUG(
            HW_ASSERT(argc == 2);
            HW_ASSERT(tids[0] == hw_TypeID_arr);
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
    return HW_VARP_NIL();
}
 
DEFN(hw_VarArr_pushStream) {
    (void)args;
    (void)tids;
    (void)argc;
    (void)state;
    _SELF_BIND(hw_VarArr *, as_arr);
    
    HW_ASSERT(0 && "NOT IMPLEMENTED");

    _SELF_ASSIGN(as_arr);
    return HW_VARP_NIL();
}

DEFN(hw_VarArr_pop) {
    HW_DEBUG((void)state;(void)argc;(void)args;(void)tids;);
    HW_DEBUG(HW_LOG("NOT IMPLEMENTED REALLOC%s", ""));

    _SELF_BIND(hw_VarArr *, as_arr);
    self->lenUsed -= 1;
    return HW_VARP_NIL();
}

DEFN(hw_VarArr_popStream) {
    HW_DEBUG((void)state;(void)argc;(void)args;(void)tids;);
    HW_DEBUG(HW_LOG("NOT IMPLEMENTED REALLOC%s", ""));

    _SELF_BIND(hw_VarArr *, as_arr);
    self->lenUsed -= _GET_ARG(0, as_uint);
    return HW_VARP_NIL();
}

/*******************
 *    Structured Array (SArr)
 *******************/

#define HW_FOREACH(T, iterator, ptr, len)\
    for(T iterator = ptr; iterator < ptr+len; iterator += 1)

DEFN(hw_SArr_newFrom_conf) {
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
    return HW_VARP_NIL();
}

DEFN(hw_SArr_delete) {
    (void)args;
    (void)tids;
    (void)argc;
    
    _SELF_BIND(hw_SArr* , as_sarr);
    _FREE(self);
    return HW_VARP_NIL();
}

DEFN(hw_SArr_push) {
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
    return HW_VARP_NIL();
}

DEFN(hw_SArr_pop) {
    (void)state;
    (void)args;
    (void)tids;
    (void)argc;
    
    _SELF_BIND(hw_SArr* , as_sarr);
    self->lenUsed -= 1;
    return HW_VARP_NIL();
}

DEFN(hw_SArr_get) {
    (void)state;
    (void)args;
    (void)tids;
    (void)argc;
    
    _SELF_BIND(hw_SArr* , as_sarr);
    hw_ptr get = _GET_ARG(0, as_ptr);
    hw_uint index = _GET_ARG(1, as_uint);
    memmove(get, self->data + (index * self->unitsize), self->unitsize);
    return HW_VARP_NIL();
}

/*******************
 *    Var List
 *******************/

DEFN(hw_VarList_new) {
    (void)args;
    (void)tids;
    (void)argc;
    
    HW_DEBUG(
        _CHECK_ARGS(0, hw_TypeID_nil)
    );

    _SELF(hw_VarList *);
    const hw_uint default_len = 8;
    
    self = _ALLOC(sizeof(hw_VarList)
                            + (sizeof(hw_Var) * default_len)
                            + (sizeof(hw_byte) * default_len));

    self->len = default_len;
    self->lenUsed = 0;

    self->data = HW_CAST(void *, self + 1);
    self->tid  = HW_CAST(void *, self->data + self->len);


    _SELF_ASSIGN(as_list);
    return HW_VARP_NIL();
}

DEFN(hw_VarList_expand) {
    (void)tids;
    (void)argc;

    _SELF_BIND(hw_VarList *, as_list);
    hw_uint size = _GET_ARG(0, as_uint);
    self = _REALLOC(self, ( (sizeof(*self)) 
                          + (sizeof(*self->data) * (self->len + size))
                          + (sizeof(*self->tid) * (self->len + size))
                        ));

    HW_DEBUG(HW_ASSERT(self));
    self->len += size;
    self->data = HW_CAST(void *, self + 1);
    self->tid = HW_CAST(void *, self->data + self->len);

    _SELF_ASSIGN(as_list);
    return HW_VARP_NIL();
}


DEFN(hw_VarList_push_shallow) {
    (void)tids;
    (void)argc;

    _SELF_BIND(hw_VarList *, as_list);
    if(self->len <= self->lenUsed) {
        hw_Var expand_args[] = { _GET_SELF(), (hw_Var){.as_uint = self->len * 2} };
        hw_byte expand_args_tids[] = { hw_TypeID_list, hw_TypeID_uint };
        hw_VarList_expand(state, expand_args, expand_args_tids, 2);
        _GET_SELF() = expand_args[0];
        self = _GET_SELF().as_list;
    }
    
    self->data[self->lenUsed] = _GET_ARG_RAW(0);
    self->tid[self->lenUsed] = _GET_ARG_TID(0);


    self->lenUsed += 1;
    
    return HW_VARP_NIL();
}

DEFN(hw_VarList_pop_dtor) {
    HW_DEBUG((void)state;(void)argc;(void)args;(void)tids;);
    _SELF_BIND(hw_VarList *, as_list);
    HW_ASSERT(0 && "NOT IMPLEMENTED");

    HW_DEBUG(HW_ASSERT(self->lenUsed != 0));
    self->lenUsed -= 1;
    hw_byte tid = self->tid[self->lenUsed];
    
    HW_DEBUG(HW_ASSERT(tid < hw_TypeID_TOTAL));
    hw_Type *T = state->ts->types + tid;
    if(T->is_obj) {
        hw_Var *top = self->data + self->lenUsed;
        
        HW_DEBUG(
            HW_ASSERT(T->vt[1] == hw_Type_getvt(T, "delete", 6));
        );

        T->vt[1](state, top, &tid, 1);
    }


    return HW_VARP_NIL();
}

DEFN(hw_VarList_reserve) {
    _SELF_BIND(hw_VarList *, as_list);
    hw_uint size = _GET_ARG(0, as_uint);
    hw_uint avail = self->len - self->lenUsed;
    if(avail < size) { hw_VarList_expand(state, args, tids, argc); }
    return HW_VARP_NIL(); // expand will set list proper, so we dont.
}

DEFN(hw_VarList_delete) {
    (void)args;
    (void)tids;
    (void)argc;

    _SELF_BIND(hw_VarList *, as_list);
    for (hw_uint i = 0; i < self->lenUsed; i++) {
        HW_DEBUG(
            printf("{}==== %"PRIu64 "===%" PRIu32 "\n", i, self->lenUsed);
            HW_ASSERT_OP(self->tid[i], <, hw_TypeID_TOTAL, PRIu8, PRIu8));
        hw_byte const T = self->tid[i];
        if(state->ts->types[T].is_obj) {
            HW_DEBUG(HW_LOG("USING vt[1] here %s", ""););
            HW_VAR_CALL(T, state, vt[1], self->data + i, self->tid + i, 1);
        }
    }
    _FREE(self);
    return HW_VARP_NIL();
}





/**************************
 * Section: hw_VarTable
 **************************/

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


hw_SymTable *_new_Symtable(hw_State *state, hw_uint len)
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

    HW_DEBUG(_symtable_print(state, self));
    return self;
}

DEFN(hw_SymTable_new) {
    (void)argc;
    (void)tids;

    hw_uint const default_len = 1 << 4;
    _GET_SELF().as_symtable = _new_Symtable(state, default_len);
    return HW_VARP_NIL();
}

DEFN(hw_SymTable_delete) {
    (void)argc;
    (void)tids;

    _SELF_BIND(hw_SymTable *, as_symtable);
    HW_DEBUG(_symtable_print(state, self));

    for (size_t i = 0; i < self->len; i++) {
        if(self->key[i] != NULL) {
            HW_DEBUG(HW_ASSERT_OP(
                self->valTs[i], <, hw_TypeID_TOTAL, PRIu8, PRIu8));
            hw_Type const *T = state->ts->types + self->valTs[i];
            if(T->is_obj) {
                hw_VarFn deletefn = hw_Type_getvt(T, "delete", 6);
                deletefn(state, self->val + i, self->valTs + i, 1);
            }
            _FREE(self->key[i]);
        }
    }
    _FREE(self);
    return HW_VARP_NIL();
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

DEFN(hw_SymTable_expand) {
    (void)argc;
    (void)tids;

    _SELF_BIND(hw_SymTable *, as_symtable);
    hw_uint const new_len = self->len << 1;
    hw_SymTable *new_table = _new_Symtable(state, new_len);
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
    return HW_VARP_NIL();
}

DEFN(hw_SymTable_reset) {
    (void)argc;
    (void)tids;
    _SELF_BIND(hw_SymTable *, as_symtable);

    for (size_t i = 0; i < self->len; i++) {
        if(self->key[i] != NULL) {
            HW_THREAD_FREE(state, self->key[i]);
            hw_Type const *T = state->ts->types + self->valTs[i];
            if(T->is_obj) {
                hw_VarFn deletefn = hw_Type_getvt(T, "delete", 6);
                deletefn(state, self->val + i, self->valTs + i, 1);
            }
            self->key[i] = NULL;
            self->valTs[i] = hw_TypeID_nil;
        }
    }

    self->lenUsed = 0;

    return HW_VARP_NIL();
}

DEFN(hw_SymTable_set) {
    (void)argc;
    (void)tids;

    _SELF_BIND(hw_SymTable *, as_symtable);    
    hw_String *key = _GET_ARG(0, as_string); 
    if(self->lenUsed > self->len/2) {
        hw_SymTable_expand(state, args, tids, 1);
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

    return HW_VARP_NIL();
}

DEFN(hw_SymTable_get) {
    (void)state;
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

    return HW_VARP_NIL();
}

/*
 * Default TypeSys
 */
#define FNINFO(T, _name, _argc, _mutc, ...)\
    {{\
        .name = (void *) #_name\
      , .name_size = sizeof(#_name)-1\
      , .arg_count = _argc+1\
      , .mut_count = _mutc\
      , .types = (hw_byte[]){__VA_ARGS__, hw_TypeID_nil}\
    }, T##_##_name} 

#define _TYPEVT_MAX 6
struct {
    hw_FnInfo info;
    hw_VarFn  fn;
} static TYPEVT[hw_TypeID_TOTAL][_TYPEVT_MAX]= {

    [hw_TypeID_string] = {
        FNINFO(hw_String, new, 0, 0, hw_TypeID_string),
        FNINFO(hw_String, delete, 0, 0, hw_TypeID_string),
        FNINFO(hw_String, newFrom_data, 2, 0
                , hw_TypeID_string, hw_TypeID_ptr, hw_TypeID_uint),
    },

    [hw_TypeID_arr] = {
        FNINFO(hw_VarArr, newFrom_conf, 1, 0, hw_TypeID_arr, hw_TypeID_uint),
        FNINFO(hw_VarArr, delete, 0, 0, hw_TypeID_arr),
        FNINFO(hw_VarArr, push, 1, 0, hw_TypeID_arr, hw_TypeID_any),
        FNINFO(hw_VarArr, pushStream, 2, 0, hw_TypeID_arr, hw_TypeID_uint, hw_TypeID_ptr),
        FNINFO(hw_VarArr, pop, 0, 0, hw_TypeID_arr),
        //FNINFO(hw_VarArr, popStream, 1, 0, hw_TypeID_arr, hw_TypeID_uint),
    },

    [hw_TypeID_list] = {
        FNINFO(hw_VarList, new, 0, 0, hw_TypeID_list),
        FNINFO(hw_VarList, delete, 0, 0, hw_TypeID_list),
        FNINFO(hw_VarList, push_shallow, 1, 0, hw_TypeID_list, hw_TypeID_any),
        FNINFO(hw_VarList, pop_dtor, 0, 0, hw_TypeID_list),
    },

    [hw_TypeID_symtable] = {
        FNINFO(hw_SymTable, new, 0, 0, hw_TypeID_symtable),
        FNINFO(hw_SymTable, delete, 0, 0, hw_TypeID_symtable),
        FNINFO(hw_SymTable, get, 2, 0
                , hw_TypeID_symtable, hw_TypeID_string, hw_TypeID_any),
        FNINFO(hw_SymTable, set, 2, 0
                , hw_TypeID_symtable, hw_TypeID_string, hw_TypeID_any),
        FNINFO(hw_SymTable, reset, 0, 0, hw_TypeID_symtable)
    },
};


static void _default_setall_atoms(hw_TypeSys *ts)
{
    hw_Type *T = (hw_TypeSys_set(ts, &(hw_Type){
                        .id = hw_TypeID_nil
                    ,   .name = "nil"
                    ,   .name_size = 3
                    ,   .unitsize = 0
                }));
    HW_ASSERT(T);

    T = (hw_TypeSys_set(ts, &(hw_Type){
                        .id = hw_TypeID_ptr
                    ,   .name = "ptr"
                    ,   .name_size = 3
                    ,   .unitsize = sizeof(hw_ptr)
                }));
    HW_ASSERT(T);

    T = (hw_TypeSys_set(ts, &(hw_Type){
                        .id = hw_TypeID_int
                    ,   .name = "int"
                    ,   .name_size = 3
                    ,   .unitsize = sizeof(hw_int)
                }));
    HW_ASSERT(T);

    T = (hw_TypeSys_set(ts, &(hw_Type){
                        .id = hw_TypeID_uint
                    ,   .name = "uint"
                    ,   .name_size = 4
                    ,   .unitsize = sizeof(hw_uint)
                }));

    T = (hw_TypeSys_set(ts, &(hw_Type){
                        .id = hw_TypeID_float
                    ,   .name = "float"
                    ,   .name_size = 5
                    ,   .unitsize = sizeof(hw_float)
                }));
    HW_ASSERT(T);

}

static void _default_setall_objects(hw_TypeSys *ts)
{
    hw_Type *T = (hw_TypeSys_set(ts, &(hw_Type){
                        .id = hw_TypeID_arr
                    ,   .is_obj = 1
                    ,   .name = "arr"
                    ,   .name_size = 3
                    ,   .unitsize = sizeof(hw_VarArr)
                    }));
    HW_ASSERT(T);

    T = (hw_TypeSys_set(ts, &(hw_Type){
                        .id = hw_TypeID_sarr
                    ,   .is_obj = 1
                    ,   .name = "sarr"
                    ,   .name_size = 4
                    ,   .unitsize = sizeof(hw_SArr)
            }));

    T = (hw_TypeSys_set(ts, &(hw_Type){
                        .id = hw_TypeID_list
                    ,   .is_obj = 1
                    ,   .name = "list"
                    ,   .name_size = 4
                    ,   .unitsize = sizeof(hw_VarList)
                    }));
    HW_ASSERT(T);

    T = (hw_TypeSys_set(ts, &(hw_Type){
                        .id = hw_TypeID_symtable
                    ,   .is_obj = 1
                    ,   .name = "symtable"
                    ,   .name_size = 5
                    ,   .unitsize = sizeof(hw_SymTable)
                    }));
    HW_ASSERT(T);


    T = (hw_TypeSys_set(ts, &(hw_Type){
                        .id = hw_TypeID_string
                    ,   .is_obj = 1
                    ,   .name = "string"
                    ,   .name_size = 6
                    ,   .unitsize = sizeof(hw_String)
                    }));
    HW_ASSERT(T);
}

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
                T->vt[vt_id] = hw_VarFn_UNREACHABLE;
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

/**
 * Wrappers
 */

void hw_SymTable_set__wrap(
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
    hw_SymTable_set(hw, st_args, st_tid, 3);
    *t = st_args[0].as_symtable;
}

hw_Var hw_SymTable_get__wrap(hw_State *hw, hw_SymTable *s, hw_CStr key)
{
    hw_String k = {
        key.data, .len = key.len, .lenUsed = key.len
    };

    hw_Var args[3] = { [0].as_symtable = s, [1].as_string = &k };
    hw_byte tid[3] = { hw_TypeID_symtable, hw_TypeID_string, hw_TypeID_nil };

    hw_SymTable_get(hw, args, tid, 3);
    return args[2];
}

