#include "hw.h"
#include "hw_dev.h"
#include <stddef.h>
#include <stdio.h>
#include <string.h>
#include <inttypes.h>
#include <assert.h>

hw_TypeSys *hw_TypeSys_new(hw_uint type_count, hw_Allocator allocator)
{
    const hw_uint size = sizeof(hw_TypeSys) 
                        + ( sizeof(hw_Type) * type_count );
    hw_TypeSys *tsys = allocator.alloc(&allocator.state, size);
    memset(tsys, 0, size);
    
    tsys->types = (void *)(tsys + 1);
    tsys->types_total = type_count;
    tsys->types_used = 0;

    tsys->allocator = allocator;

    return tsys;
}

void hw_TypeSys_delete(hw_TypeSys *t)
{
    void (*_free)(hw_ptr *state, void *) = t->allocator.free;
    _free(&t->allocator.state, t);
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
        hw_TypeSys   *ts        \
      , hw_Var       *args      \
      , hw_byte      *tids      \
      , hw_uint const argc)

#define _ALLOC(SIZE)            HW_TYPESYS_ALLOC(ts, SIZE)
#define _REALLOC(PTR, SIZE)     HW_TYPESYS_REALLOC(ts, PTR, SIZE)
#define _FREE(PTR)              HW_TYPESYS_FREE(ts, PTR)

#define _GET_SELF() (args[0])
#define _GET_SELF_TID() (tids[0])
#define _SELF(T) T self
#define _SELF_ASSIGN(as) (_GET_SELF()).as = self;
#define _SELF_BIND(T, as) T self = _GET_SELF().as

#define _GET_ARG_RAW(n) (args[n+1])
#define _GET_ARG(n, as) (_GET_ARG_RAW(n).as)
#define _GET_ARG_TID(n) (tids[1 + n])
#define _MAKE_VAR(value, as) ((hw_Var){.as = value})

#define _CHECK_ARGS(_c, ...)\
    do {\
        hw_uint c = _c + 1;\
        HW_ASSERT(c == argc);\
        hw_byte type_ids[] = {__VA_ARGS__};\
        for (size_t i = 0; i < c; i++) {\
            if(type_ids[i] != hw_TypeID_nil) {\
                HW_ASSERTEX(type_ids[i] == tids[i]\
                , "Mis-match argument type:%.*s at position '%" PRIu64 "'"\
                , ts->types[tids[i]].name_size, ts->types[tids[i]].name\
                , i);\
            }\
        }\
    } while(0)

/*************************************************************************
 *                          PRIVATE
 *************************************************************************/

static void *_loadfile(
    hw_TypeSys *ts, char const path[], hw_uint unitsize, hw_uint *len)
{
    FILE *fp;
    if ((fp = fopen(path, "rb")) == NULL) {
        return NULL;
    }

    fseek(fp, 0, SEEK_END);
    hw_uint fsize = ftell(fp);
    *len = fsize;
    fseek(fp, 0, SEEK_SET);  /* same as rewind(f); */

    hw_uint extra = fsize/unitsize;

    void *data = _ALLOC(fsize * extra);
    fread(data, 1, fsize, fp);
    fclose(fp);

    return data;
}

DEFN(hw_VarFn_UNREACHABLE) {
    (void)ts;
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
                            + (sizeof(self->data) * default_size));

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

DEFN(hw_String_newFrom_cstr) {
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
    hw_VarP ret = hw_String_newFrom_cstr(ts, ar, ti, 3);
    args[0] = ar[0];

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

    hw_String *path = _GET_ARG(1, as_string);
    hw_Var filesize;
    hw_Var filedata = { 
        .as_ptr = _loadfile(
                ts, (void const *)path->data
                , path->lenUsed, &filesize.as_uint)
    };
    
    hw_Var args_list[] = { {0}, filedata, filesize };
    hw_byte tids_list[] = { 0, hw_TypeID_ptr, hw_TypeID_uint };
    hw_String_newFrom_cstr(ts, args_list, tids_list, 3);

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
    hw_byte tid = self->tid;
    hw_VarFn deletefn = hw_Type_getvt(self->tid + ts->types, "delete", 6);
    if (ts->types[self->tid].is_obj) {
        for (hw_uint i = 0; i < self->lenUsed; i++) {
                deletefn(ts, self->data + i, &tid, 1);
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
    (void)ts;
    _SELF_BIND(hw_VarArr *, as_arr);
    
    HW_ASSERT(0 && "NOT IMPLEMENTED");

    _SELF_ASSIGN(as_arr);
    return HW_VARP_NIL();
}

DEFN(hw_VarArr_pop) {
    HW_DEBUG((void)ts;(void)argc;(void)args;(void)tids;);
    HW_DEBUG(HW_LOG("NOT IMPLEMENTED REALLOC%s", ""));

    _SELF_BIND(hw_VarArr *, as_arr);
    self->lenUsed -= 1;
    return HW_VARP_NIL();
}

DEFN(hw_VarArr_popStream) {
    HW_DEBUG((void)ts;(void)argc;(void)args;(void)tids;);
    HW_DEBUG(HW_LOG("NOT IMPLEMENTED REALLOC%s", ""));

    _SELF_BIND(hw_VarArr *, as_arr);
    self->lenUsed -= _GET_ARG(0, as_uint);
    return HW_VARP_NIL();
}

/*******************
 *    Structured Array (SArr)
 *******************/

hw_SArr* hw_SArr_new(hw_TypeSys *ts, hw_uint unitsize, hw_byte *types, hw_uint type_count);
hw_ptr hw_SArr_push(hw_TypeSys *ts, hw_SArr **_self, hw_ptr value);
hw_ptr hw_SArr_get(hw_SArr *self, hw_uint index);
void hw_SArr_delete(hw_TypeSys *ts, hw_SArr *self);

hw_SArr* hw_SArr_new(hw_TypeSys *ts, hw_uint unitsize, hw_byte *types, hw_uint type_count)
{
    hw_uint const default_len = 8;
    hw_SArr *self = HW_TYPESYS_ALLOC(ts
                        , sizeof(*self)
                          + (type_count * sizeof(*self->type_inf))
                          + (unitsize * default_len));

    HW_DEBUG(HW_ASSERT(sizeof(*self->type_inf) == sizeof(*types)));
    self->type_inf = HW_CAST(void *, self + 1);
    self->data = HW_CAST(void *, self->type_inf + type_count);
    
    self->unitsize = unitsize;
    self->len = 8;
    self->lenUsed = 0;

    if(type_count) {
        memcpy(self->type_inf, types, type_count * sizeof(*self->type_inf));
    } else {
        self->type_inf = NULL;
    }

    return self;
}

hw_ptr hw_SArr_push(hw_TypeSys *ts, hw_SArr **_self, hw_ptr value)
{
    hw_SArr *self = *_self;
    if(self->lenUsed >= self->len) {
        hw_uint new_len = self->len << 1;
        self = HW_TYPESYS_REALLOC(ts, self, sizeof(*self)
                          + (self->type_count * sizeof(*self->type_inf))
                          + (self->unitsize * new_len));
        self->len = new_len;
    }

    hw_byte *top = self->data + (self->unitsize * self->lenUsed);
    memcpy(top, value, self->unitsize);
    self->lenUsed += 1;
    
    return top;
}

hw_ptr hw_SArr_get(hw_SArr *self, hw_uint index)
{
    return self->data + (self->unitsize * index);
}

void hw_SArr_delete(hw_TypeSys *ts, hw_SArr *self)
{
    HW_TYPESYS_FREE(ts, self);
}

DEFN(hw_SArr_fn_new);
{

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

    self->data = HW_CAST(void *, self + 1);
    self->tid  = HW_CAST(void *, self->data + default_len);

    self->len = default_len;
    self->lenUsed = 0;

    _SELF_ASSIGN(as_list);
    return HW_VARP_NIL();
}

DEFN(hw_VarList_expand) {
    (void)tids;
    (void)argc;

    _SELF_BIND(hw_VarList *, as_list);
    hw_uint size = _GET_ARG(0, as_uint);
    self = _REALLOC(self, ( (sizeof(*self->data) * (self->len + size))
                          + (sizeof(*self->tid) * (self->len + size))
                        ));

    HW_DEBUG(HW_ASSERT(self));
    self->len += size;

    _SELF_ASSIGN(as_list);
    return HW_VARP_NIL();
}

DEFN(hw_VarList_push) {
    (void)tids;
    (void)argc;

    _SELF_BIND(hw_VarList *, as_list);
    if(self->len <= self->lenUsed) {
        hw_Var expand_args[] = { _GET_SELF(), (hw_Var){.as_uint = self->len * 2} };
        hw_byte expand_args_tids[] = { hw_TypeID_list, hw_TypeID_uint };
        hw_VarList_expand(ts, expand_args, expand_args_tids, 2);
        _GET_SELF() = expand_args[0];
        self = _GET_SELF().as_list;
    }
    
    self->data[self->lenUsed] = _GET_ARG_RAW(0);
    self->lenUsed += 1;
    
    return HW_VARP_NIL();
}

DEFN(hw_VarList_pop) {
    HW_DEBUG((void)ts;(void)argc;(void)args;(void)tids;);
    _SELF_BIND(hw_VarList *, as_list);
    HW_ASSERT(0 && "NOT IMPLEMENTED");

    HW_DEBUG(HW_ASSERT(self->lenUsed != 0));
    self->lenUsed -= 1;
    hw_byte tid = self->tid[self->lenUsed];
    
    HW_DEBUG(HW_ASSERT(tid < hw_TypeID_TOTAL));
    hw_Type *T = ts->types + tid;
    if(T->is_obj) {
        hw_Var *top = self->data + self->lenUsed;
        
        HW_DEBUG(
            HW_ASSERT(T->vt[1] == hw_Type_getvt(T, "delete", 6));
        );

        T->vt[1](ts, top, &tid, 1);
    }


    return HW_VARP_NIL();
}

DEFN(hw_VarList_reserve) {
    _SELF_BIND(hw_VarList *, as_list);
    hw_uint size = _GET_ARG(0, as_uint);
    hw_uint avail = self->len - self->lenUsed;
    if(avail < size) { hw_VarList_expand(ts, args, tids, argc); }
    return HW_VARP_NIL(); // expand will set list proper, so we dont.
}

DEFN(hw_VarList_delete) {
    (void)args;
    (void)tids;
    (void)argc;

    _SELF_BIND(hw_VarList *, as_list);
    for (hw_uint i = 0; i < self->lenUsed; i++) {
        hw_byte const T = self->tid[i];
        if(ts->types[T].is_obj) {
            HW_DEBUG(HW_LOG("USING vt[1] here %s", ""););
            HW_VAR_CALL_CORE(T, ts, vt[1], self->data + i, self->tid + i, 1);
        }
    }
    _FREE(self);
    return HW_VARP_NIL();
}





/**************************
 * Section: hw_VarTable
 **************************/
hw_VarTable *_VarTable_new(
    hw_TypeSys *ts
  , enum hw_TypeID tid
  , hw_VarFn eqfn
  , hw_VarFn hashfn
  , hw_uint len)
{
    hw_VarTable *self = _ALLOC( (sizeof(hw_VarTable))
            + (sizeof(*self->key) * len)
            + (sizeof(*self->val) * len)
            + (sizeof(*self->valTs) * len)
        );
    self->key = HW_CAST(hw_Var *, self + 1);
    self->val = HW_CAST(hw_Var *, (self->key + len));
   
    self->keyT = tid;
    memset(self->valTs, hw_TypeID_nil, len);

    self->eqfn = eqfn;
    self->hashfn = hashfn;
    self->len = len;
    self->lenUsed = 0;

    return self;
}

DEFN(hw_VarTable_newFrom_conf) {
    (void)args;
    (void)tids;
    (void)argc;

    _SELF(hw_VarTable *);
    hw_byte key_tid = _GET_ARG(0, as_uint);
    self = _VarTable_new(ts, key_tid
                , hw_Type_getvt(ts->types + key_tid, "eq", 2)
                , hw_Type_getvt(ts->types + key_tid, "to_hash", 7)
                    // (5:32) (4:16) (3:8) (2:4) (1:2) (0:1)
                , 1 << 4);

    HW_DEBUG(HW_ASSERT_NOT_EQ(self->hashfn, hw_VarFn_UNREACHABLE));
    HW_DEBUG(HW_ASSERT_NOT_EQ(self->eqfn, hw_VarFn_UNREACHABLE));

    _SELF_ASSIGN(as_table);
    return HW_VARP_NIL();
}

DEFN(hw_VarTable_delete_detatch) {
    (void)args;
    (void)tids;
    (void)argc;

    _SELF_BIND(hw_VarTable *, as_table);
    _FREE(self);
    return HW_VARP_NIL();
}

DEFN(hw_VarTable_set_entry) {
    (void)args;
    (void)tids;
    (void)argc;

    _SELF_BIND(hw_VarTable *, as_table);

    hw_Var key = _GET_ARG_RAW(0);
    hw_Var val = _GET_ARG_RAW(1);

    HW_DEBUG(HW_ASSERT(self->keyT == _GET_ARG_TID(0)));
    hw_Var hashv = (hw_Var){.as_uint = 0};
    hw_Var hash_arg[] = { key,  hashv};
    self->hashfn(ts, hash_arg, (hw_byte[]){self->keyT, hw_TypeID_uint}, 2);
    hw_uint hash = hashv.as_uint;

    hw_uint idx = (hw_uint)(hash & ((hw_uint)(self->len - 1)));
    
    hw_VarFn eqfn = self->eqfn;
    HW_DEBUG(
        hw_uint _debug_loopback = 0;
    );
    while(self->valTs[idx] > hw_TypeID_nil) {
        hw_Var _args_[3] = { self->key[idx], key, (hw_Var){.as_uint = 0}};
        eqfn(ts, _args_, (hw_byte[]){self->keyT, self->keyT, hw_TypeID_uint}, 3);
        if(_args_[2].as_uint == 0) {
            hw_byte _tid_ = self->keyT;
            hw_VarFn delete = hw_Type_getvt(ts->types + self->keyT, "delete", 6);
            delete(ts, self->key + idx, &_tid_, 1);
            goto _L_set;
        }
        idx += 1;
        if(idx >= self->len) {
            idx = 0;
            HW_DEBUG(
                HW_ASSERTEX(_debug_loopback < 2,
                    "Var Table Cant Find Empty Space (%p)", (void *)self);
                _debug_loopback += 1;
            )
        }
    }

    _L_set:
    self->key[idx] = key;
    self->val[idx] = val;
    self->valTs[idx] = _GET_ARG_TID(1);
    self->lenUsed += 1;

    return HW_VARP_NIL();
}

DEFN(hw_VarTable_expand) {
    (void)args;
    (void)tids;
    (void)argc;

    _SELF_BIND(hw_VarTable *, as_table);
    HW_ASSERT(0 && "NOT IMPLEMENTED");

    const hw_uint new_len = self->len << 1;
    HW_SAFE_GAURD(HW_ASSERT(new_len < UINT16_MAX));
    
    hw_VarTable *old = _GET_SELF().as_table;
    self = _VarTable_new(ts
            , self->keyT, self->eqfn
            , self->hashfn, self->len << 1);

    for (size_t i = 0; i < old->len; i++) {
        if(old->valTs[i] > hw_TypeID_nil) {
            hw_Var new_table = HW_VAR(self, as_table);
            hw_Var _args[3] = { new_table, old->key[i], old->val[i] };
            hw_byte _tids[3] = { hw_TypeID_table, old->keyT, old->valTs[i] };
            hw_VarTable_set_entry(ts, _args, _tids, 3);
        }
    }

    _SELF_ASSIGN(as_table);
    return HW_VARP_NIL();
}

DEFN(hw_VarTable_set) {
    _SELF_BIND(hw_VarTable *, as_table);
    if(self->lenUsed > (self->len >> 1)) {
        hw_VarTable_expand(ts, args, tids, argc);
    }
    hw_VarTable_set_entry(ts, args, tids, argc);

    _SELF_ASSIGN(as_table);
    return HW_VARP_NIL();
}

DEFN(hw_VarTable_newFrom_copy) {
    (void)args;
    (void)tids;
    (void)argc;
    (void)ts;

//    _SELF_BIND(hw_VarTable *, as_table);
    HW_ASSERT(0 && "NOT IMPLEMENTED");
    return HW_VARP_NIL();
}
DEFN(hw_VarTable_get) {
    (void)args;
    (void)tids;
    (void)argc;
    (void)ts;

 //   _SELF_BIND(hw_VarTable *, as_table);
    HW_ASSERT(0 && "NOT IMPLEMENTED");
    return HW_VARP_NIL();
}

DEFN(hw_VarTable_get_index) {
    (void)args;
    (void)tids;
    (void)argc;
    (void)ts;

    //_SELF_BIND(hw_VarTable *, as_table);
    HW_ASSERT(0 && "NOT IMPLEMENTED");
    return HW_VARP_NIL();
}

DEFN(hw_VarTable_set_index) {
    (void)args;
    (void)tids;
    (void)argc;
    (void)ts;

//    _SELF_BIND(hw_VarTable *, as_table);
    HW_ASSERT(0 && "NOT IMPLEMENTED");
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
      , .argT = (hw_byte[]){__VA_ARGS__, hw_TypeID_nil}\
    }, T##_##_name} 

#define _TYPEVT_MAX 6
struct {
    hw_FnInfo info;
    hw_VarFn  fn;
} static TYPEVT[hw_TypeID_TOTAL][_TYPEVT_MAX]= {

    [hw_TypeID_string] = {
        FNINFO(hw_String, new, 0, 0, hw_TypeID_string),
        FNINFO(hw_String, delete, 0, 0, hw_TypeID_string),
        FNINFO(hw_String, newFrom_cstr, 2, 0
                , hw_TypeID_string, hw_TypeID_uint, hw_TypeID_ptr),
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
        FNINFO(hw_VarList, push, 1, 0, hw_TypeID_list, hw_TypeID_any),
        FNINFO(hw_VarList, pop, 0, 0, hw_TypeID_list),
    },

    [hw_TypeID_table] = {
        FNINFO(hw_VarTable, newFrom_conf, 0, 0, hw_TypeID_table),
        FNINFO(hw_VarTable, delete_detatch, 0, 0, hw_TypeID_table),
        FNINFO(hw_VarTable, get, 2, 0
                , hw_TypeID_table, hw_TypeID_any, hw_TypeID_any),
        FNINFO(hw_VarTable, set, 2, 0
                , hw_TypeID_table, hw_TypeID_any, hw_TypeID_any),
        FNINFO(hw_VarTable, get_index, 2, 1
                , hw_TypeID_table, hw_TypeID_any, hw_TypeID_uint),
        FNINFO(hw_VarTable, set_index, 3, 1
                , hw_TypeID_table, hw_TypeID_any, hw_TypeID_uint)
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
                        .id = hw_TypeID_table
                    ,   .is_obj = 1
                    ,   .name = "table"
                    ,   .name_size = 5
                    ,   .unitsize = sizeof(hw_VarTable)
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
                HW_ASSERT(finfo.argT[arg_i] < hw_TypeID_TOTAL);
                char *name = (char *)ts->types[finfo.argT[arg_i]].name;
                hw_uint name_size = ts->types[finfo.argT[arg_i]].name_size;
                hw_logp(" %.*s |", (int)name_size, name);
            }
            hw_logp("\n");
        }
        _L_skip:;
    }
}

hw_TypeSys* hw_TypeSys_default_with_allocator(hw_Allocator allocator)
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

