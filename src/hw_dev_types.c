#include "hw.h"
#include "hw_dev.h"
#include <stddef.h>
#include <string.h>

hw_TypeSys *hw_TypeSys_new(hw_uint type_count, hw_Allocator allocator)
{
    hw_TypeSys *tsys = allocator.alloc(&allocator.state, sizeof(hw_TypeSys) 
                        + (sizeof(hw_Type) * type_count) );

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
            HW_DLOG("RE SET OF TypeSys Type %s", dest->name);
        }
    )

    memcpy(dest, type, sizeof(*type));
    return dest;
}

/**********************************************************************/
/* All Types VTables:
 * IMPORTANT ONES:
 *      -> VarArr
 *      -> VarList
 *      -> VarHash
 */
/**********************************************************************/
#define DEFN(NAME)\
    hw_VarP NAME (              \
        hw_Var *_self_          \
      , hw_TypeSys *ts          \
      , hw_Var  const *args     \
      , hw_byte const *tid      \
      , hw_uint const count)

#define _ALLOC(SIZE)            HW_TYPESYS_ALLOC(ts, SIZE)
#define _REALLOC(PTR, SIZE)     HW_TYPESYS_REALLOC(ts,PTR, SIZE)
#define _FREE(PTR)              HW_TYPESYS_FREE(ts, PTR)

#define _SELF(T) T self
#define _SELF_ASSIGN(as) _self_->as = self;
#define _SELF_BIND(T, as) T self = _self_->as

#define _GET_ARG(n, as) (args[n].as)
#define _MAKE_VAR(value, as) ((hw_Var){.as = value})

/**
 * Var List
 */

DEFN(hw_unreachable_func) {
    HW_ASSERT(0);
    return (hw_VarP){0};
}

DEFN(hw_VarList_new) {
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
    return (hw_VarP){0};
}

DEFN(hw_VarList_delete) {
    _SELF_BIND(hw_VarList *, as_list);
    for (hw_uint i = 0; i < self->lenUsed; i++) {
        hw_byte const T = self->tid[i];
        if(ts->types[T].is_obj) {
            ts->types[T].vtcore.delete( self->data + i, ts, NULL, NULL, 0);
        }
    }
    _FREE(self);
    return (hw_VarP){0};
}


// STRINGS
DEFN(hw_String_new) {
    
    const hw_uint default_size = 8;
    _SELF(hw_String *) = _ALLOC(sizeof(hw_String)
                            + (sizeof(self->data) * default_size));

    self->len = default_size;
    self->lenUsed = 0;

    _SELF_ASSIGN(as_string);
    return (hw_VarP){0};
}

DEFN(hw_String_delete) {
    _SELF_BIND(hw_String *, as_string);
    _FREE(self);
    return (hw_VarP){0};
}

DEFN(hw_String_newFrom_cstr) {
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
    return (hw_VarP){0};
}

DEFN(hw_String_newFrom_copy) {
    hw_String const *string = _GET_ARG(0, as_string);

    hw_Var const data = _MAKE_VAR(string->data, as_ptr);
    hw_Var const size = _MAKE_VAR(string->lenUsed, as_uint);

    return hw_String_newFrom_cstr(
        _self_, ts
      , (hw_Var const[]){ data, size }
      , (hw_byte const[]){ hw_TypeID_PTR, hw_TypeID_UINT }
      , 2);
}

DEFN(hw_String_append_cstr) {
    _SELF_BIND(hw_String *, as_string);
    hw_byte *data = _GET_ARG(0, as_byte_p);
    hw_uint dsize = _GET_ARG(1, as_uint);

    hw_uint const availiable_len = self->len - self->lenUsed;
    if(availiable_len < dsize) {
        self = _REALLOC(self, (sizeof(*self))
                            +  (sizeof(*self->data) * (1 + self->len + dsize)));
        self->data = HW_CAST(void *, self + 1);
    }
    
    memcpy(self->data + self->lenUsed, data, dsize);
    self->lenUsed += dsize;
    _SELF_ASSIGN(as_string);
    return (hw_VarP){0};
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
                        .id = hw_TypeID_NIL
                    ,   .name = "nil"
                    ,   .name_size = 3
                    ,   .unitsize = 0
                }));

    HW_ASSERT(hw_TypeSys_set(ts, &(hw_Type){
                        .id = hw_TypeID_LIST
                    ,   .name = "list"
                    ,   .name_size = 4
                    ,   .unitsize = sizeof(hw_VarList)
                    ,   .vtcore = {
                            .new = hw_VarList_new
                           ,.delete = hw_VarList_delete
                        }
                }));

    HW_ASSERT(hw_TypeSys_set(ts, &(hw_Type){
                        .id = hw_TypeID_STRING
                    ,   .name = "string"
                    ,   .name_size = 6
                    ,   .unitsize = sizeof(hw_String)
                    ,   .vtcore = {
                            .new = hw_unreachable_func
                           ,.delete = hw_unreachable_func
                        }
                }));

    return ts;
}

