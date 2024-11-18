#include "hw.h"
#include "hw_dev.h"
#include <malloc.h>
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
    static hw_VarP NAME (       \
        hw_Var *self            \
      , hw_TypeSys *ts    \
      , hw_Var  const *args     \
      , hw_byte const *tid      \
      , hw_uint const count)

#define _ALLOC(SIZE)            HW_TYPESYS_ALLOC(ts, SIZE)
#define _REALLOC(PTR, SIZE)     HW_TYPESYS_REALLOC(ts,PTR, SIZE)
#define _FREE(PTR)              HW_TYPESYS_FREE(ts, PTR)

/**
 * Var List
 */

DEFN(hw_unreachable_func) {
    HW_ASSERT(0);
    return (hw_VarP){0};
}

DEFN(hw_VarList_init_default) {
    const hw_uint default_len = 8;
    self->as_list = _ALLOC(sizeof(hw_VarList)
                            + (sizeof(hw_Var) * default_len)
                            + (sizeof(hw_byte) * default_len)
                        );

    self->as_list->data = HW_CAST(void *, self->as_list + 1);
    self->as_list->tid  = HW_CAST(void *, self->as_list->data + default_len);

    self->as_list->len = default_len;
    self->as_list->lenUsed = 0;

    return (hw_VarP){0};
}

DEFN(hw_VarList_deinit) {
    _FREE(self->as_list);
    return (hw_VarP){0};
}


// STRINGS
DEFN(hw_String_vt_core_init_default) {
    const hw_uint default_size = 8;
    self->as_string = _ALLOC(sizeof(hw_String)
                            + (sizeof(self->as_string->data) * ));


    return (hw_VarP){0};
}

DEFN(hw_String_vt_core_deinit) {
    _FREE(self)
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
                            .init = hw_unreachable_func
                           ,.init_default = hw_VarList_init_default
                           ,.deinit = hw_VarList_deinit
                        }
                }));

    HW_ASSERT(hw_TypeSys_set(ts, &(hw_Type){
                        .id = hw_TypeID_STRING
                    ,   .name = "string"
                    ,   .name_size = 6
                    ,   .unitsize = sizeof(hw_String)
                    ,   .vtcore = {
                            .init = hw_unreachable_func
                           ,.init_default = hw_unreachable_func
                           ,.deinit = hw_unreachable_func
                        }
                }));

    return ts;
}

