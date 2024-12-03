#include "hw.h"
#include "hw_dev.h"
#include <stddef.h>
#include <stdio.h>
#include <string.h>
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

/**********************************************************************/
/* All Types VTables:
 */
/**********************************************************************/
#define DEFN(NAME)\
    hw_VarP NAME (              \
        hw_TypeSys   *ts        \
      , hw_Var       *args      \
      , hw_byte      *tid       \
      , hw_uint const count)

#define _ALLOC(SIZE)            HW_TYPESYS_ALLOC(ts, SIZE)
#define _REALLOC(PTR, SIZE)     HW_TYPESYS_REALLOC(ts,PTR, SIZE)
#define _FREE(PTR)              HW_TYPESYS_FREE(ts, PTR)

#define _GET_SELF() (args[0])
#define _GET_SELF_TID() (tid[0])
#define _SELF(T) T self
#define _SELF_ASSIGN(as) (_GET_SELF()).as = self;
#define _SELF_BIND(T, as) T self = _GET_SELF().as

#define _GET_ARG_RAW(n) (args[n+1])
#define _GET_ARG(n, as) (_GET_ARG_RAW(n).as)
#define _MAKE_VAR(value, as) ((hw_Var){.as = value})

#define _CHECK_ARGS(_c, ...)\
    do {\
        hw_uint c = _c;\
        HW_ASSERT(c == count);\
        HW_ASSERT(!memcmp((hw_byte[]){__VA_ARGS__}, tid, count));\
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

static DEFN(hw_unreachable_func) {
    (void)ts;
    (void)args;
    (void)tid;
    (void)count;
    HW_ASSERT(0);
    return HW_VARP_NIL();
}

/**
 * Var List
 */

DEFN(hw_VarList_new) {
    (void)args;
    (void)tid;
    (void)count;
    
    HW_DEBUG(
            _CHECK_ARGS(1, hw_TypeID_nil)
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
    (void)count;
    (void)tid;

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

DEFN(hw_VarList_push_shallow) {
    (void)count;
    (void)tid;

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

DEFN(hw_VarList_reserve) {
    _SELF_BIND(hw_VarList *, as_list);
    hw_uint size = _GET_ARG(0, as_uint);
    hw_uint avail = self->len - self->lenUsed;
    if(avail < size) { hw_VarList_expand(ts, args, tid, count); }
    return HW_VARP_NIL();
}

DEFN(hw_VarList_delete) {
    (void)args;
    (void)tid;
    (void)count;

    _SELF_BIND(hw_VarList *, as_list);
    for (hw_uint i = 0; i < self->lenUsed; i++) {
        hw_byte const T = self->tid[i];
        if(ts->types[T].is_obj) {
            HW_VAR_CALL_CORE(T, ts, vtcore.delete, self->data + i, self->tid + i, 1);
        }
    }
    _FREE(self);
    return HW_VARP_NIL();
}


// STRINGS
DEFN(hw_String_new) {
    (void)args;
    (void)tid;
    (void)count;

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
    (void)tid;
    (void)count;

    _SELF_BIND(hw_String *, as_string);
    _FREE(self);
    return HW_VARP_NIL();
}

DEFN(hw_String_newFrom_cstr) {
    (void)tid;
    (void)count;

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
    (void)tid;
    (void)count;

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
    (void)tid;
    (void)count;

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
    _SELF(hw_String *);
    HW_ASSERT(0 && "NOT IMPLEMENTED");
    hw_uint filesize = 0;
    hw_ptr filedata = _loadfile(ts, "X", 1, &filesize);
    if(filedata == NULL) {
        return HW_VARP_ERROR(
            &((hw_Error){.error = 0, .str = "FNF", .str_size = 3}));
    }
    
    _SELF_ASSIGN(as_string);
    return HW_VARP_NIL();
}

/**
 * VarArr
 */
DEFN(hw_VarArr_newFrom_conf) {
    (void)args;
    (void)tid;
    (void)count;

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
    (void)tid;
    (void)count;

    _SELF_BIND(hw_VarArr *, as_arr);

    if (ts->types[self->tid].is_obj) {
        for (hw_uint i = 0; i < self->lenUsed; i++) {
            HW_VAR_CALL_CORE(
                self->tid, ts, vtcore.delete, self->data + i, &self->tid, 1);
        }
    }

    _FREE(self);
    _SELF_ASSIGN(as_arr);
    return HW_VARP_NIL();
}



DEFN(hw_VarArr_push) {
    _SELF_BIND(hw_VarArr *, as_arr);

    HW_DEBUG(
            HW_ASSERT(count == 2);
            HW_ASSERT(tid[0] == hw_TypeID_arr);
            HW_ASSERT(tid[1] == self->tid);
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
    _SELF_BIND(hw_VarArr *, as_arr);
    
    HW_ASSERT(0 && "NOT IMPLEMENTED");

    _SELF_ASSIGN(as_arr);
    return HW_VARP_NIL();
}

/*
 * Default TypeSys
 */

hw_TypeSys* hw_TypeSys_default_with_allocator(hw_Allocator allocator)
{
    HW_ASSERT(1); // Checking assert sanity
                  //
    hw_TypeSys *ts = hw_TypeSys_new(hw_TypeID_TOTAL, allocator);
    HW_ASSERT(ts != NULL);
    
    HW_ASSERT(hw_TypeSys_set(ts, &(hw_Type){
                        .id = hw_TypeID_nil
                    ,   .name = "nil"
                    ,   .name_size = 3
                    ,   .unitsize = 0
                }));

    HW_ASSERT(hw_TypeSys_set(ts, &(hw_Type){
                        .id = hw_TypeID_ptr
                    ,   .name = "ptr"
                    ,   .name_size = 3
                    ,   .unitsize = sizeof(hw_ptr)
                }));

    HW_ASSERT(hw_TypeSys_set(ts, &(hw_Type){
                        .id = hw_TypeID_int
                    ,   .name = "int"
                    ,   .name_size = 3
                    ,   .unitsize = sizeof(hw_int)
                }));

    HW_ASSERT(hw_TypeSys_set(ts, &(hw_Type){
                        .id = hw_TypeID_uint
                    ,   .name = "uint"
                    ,   .name_size = 4
                    ,   .unitsize = sizeof(hw_uint)
                }));

    HW_ASSERT(hw_TypeSys_set(ts, &(hw_Type){
                        .id = hw_TypeID_float
                    ,   .name = "float"
                    ,   .name_size = 5
                    ,   .unitsize = sizeof(hw_float)
                }));

    HW_ASSERT(hw_TypeSys_set(ts, &(hw_Type){
                        .id = hw_TypeID_list
                    ,   .is_obj = 1
                    ,   .name = "list"
                    ,   .name_size = 4
                    ,   .unitsize = sizeof(hw_VarList)
                    ,   .vtcore = {
                            .new = hw_VarList_new
                           ,.delete = hw_VarList_delete
                        }
                }));

    HW_ASSERT(hw_TypeSys_set(ts, &(hw_Type){
                        .id = hw_TypeID_arr
                    ,   .is_obj = 1
                    ,   .name = "arr"
                    ,   .name_size = 3
                    ,   .unitsize = sizeof(hw_VarArr)
                    ,   .vtcore = {
                            .new = hw_unreachable_func
                        ,   .delete = hw_VarArr_delete
                    }
                }));

    HW_ASSERT(hw_TypeSys_set(ts, &(hw_Type){
                        .id = hw_TypeID_string
                    ,   .is_obj = 1
                    ,   .name = "string"
                    ,   .name_size = 6
                    ,   .unitsize = sizeof(hw_String)
                    ,   .vtcore = {
                            .new = hw_String_new
                           ,.delete = hw_String_delete
                        }
                }));

    return ts;
}

