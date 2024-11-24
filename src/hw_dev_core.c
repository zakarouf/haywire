#include "hw.h"
#include "hw_dev.h"

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

