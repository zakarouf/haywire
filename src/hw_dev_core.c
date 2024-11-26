#include "hw.h"
#include "hw_dev.h"

#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

/**
 * Section: Pre-Processor
 */
#define CAT2(X, Y) X##_##Y
#define CAT(X, Y) CAT2(X, Y)


/**
 * Log & Exit
 */
void hw_logp(const char *fmt, ...)
{
    va_list arg;
    va_start(arg, fmt);
    vprintf(fmt, arg);
    va_end(arg);
}

void hw_logstr(const char *msg, size_t const size)
{
    fwrite(msg, size, 1, stdout);
    fputc('\n', stdout);
}

void hw_exit(hw_int code, const char *msg, size_t const size)
{
    hw_logstr(msg, size);
    exit(code);
}

/**
 * Section: Array Macro
 */
static void* _malloc_wrapper(void **state, size_t size)
{
    return HW_MALLOC(size);
}

static void* _realloc_wrapper(void **state, void *ptr, size_t size)
{
    return HW_REALLOC(ptr, size);
}

static void  _free_wrapper(void **state, void *ptr)
{
    HW_FREE(ptr);
}

void hw_Allocator_default(hw_Allocator *self)
{
    self->state = NULL;
    self->alloc = _malloc_wrapper;
    self->realloc = _realloc_wrapper;
    self->free = _free_wrapper;
}


/**
 * Threads
 */

hw_VarP hw_Thread_init(
    hw_Thread *t
  , hw_State const *global
  , hw_uint id
  , const char *name
  , hw_byte name_size)
{
    void *(*alloc)(void **, size_t) = global->tsys->allocator.alloc;
    void **alloc_obj = &global->tsys->allocator.state;

    t->id = id;
    t->name_size = name_size;
    memcpy(t->name, name, name_size);

    #define DEFAULT_STACK_SIZE 8
    t->fstack.data = alloc(alloc_obj, sizeof(*t->fstack.data) * DEFAULT_STACK_SIZE);
    t->fstack.len = DEFAULT_STACK_SIZE;
    t->fstack.lenUsed = 0;
    
    hw_Var vstack = {0};
    hw_VarList_new(
        &vstack, global->tsys, (const hw_Var[]){0}, (const hw_byte[]){0}, 0);
    t->vstack = vstack.as_list;
    
    memset(&t->fn, 0, sizeof(t->fn));
    
    t->global = global;
    return HW_VARP_NIL();
}

hw_VarP hw_Thread_deinit(hw_Thread *t)
{
    hw_VarList_delete(
        &(hw_Var){.as_list = t->vstack}
      , t->global->tsys, (const hw_Var[]){0}
      , (const hw_byte[]){0}
      , 0);

    void(*mfree)(void **, void *) = t->global->tsys->allocator.free;
    void **alloc_obj = &t->global->tsys->allocator.state;
    mfree(alloc_obj, t->fstack.data);

    return HW_VARP_NIL();
}
