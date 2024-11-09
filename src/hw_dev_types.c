#include "hw.h"
#include "hw_dev.h"
#include <malloc.h>
#include <stddef.h>
#include <string.h>

hw_TypeSys *hw_TypeSys_new(hw_uint type_count
        , void *(*_alloc)(size_t size)
        , void *(*_realloc)(void *, size_t)
        , hw_uint (*_free)(void *))
{
    hw_TypeSys *tsys = _alloc(sizeof(hw_TypeSys) 
                        + (sizeof(hw_Type) * type_count) );

    tsys->types = (void *)(tsys + 1);
    tsys->types_total = type_count;
    tsys->types_used = 0;

    tsys->alloc = _alloc;
    tsys->realloc = _realloc;
    tsys->free = _free;

    return tsys;
}

void hw_TypeSys_delete(hw_TypeSys *t)
{
    hw_uint (*_free)(void *) = t->free;
    _free(t);
}

hw_Type *hw_TypeSys_set(hw_TypeSys *ts, hw_Type const *type)
{
    if(type->id >= ts->types_total) { return NULL; }
    hw_Type *dest = &ts->types[type->id];
    memcpy(dest, type, sizeof(*type));
    return dest;
}


