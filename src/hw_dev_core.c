#include "hw.h"
#include "hw_dev.h"

#include <malloc.h>
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
#define ARR_ALLOC_SIZE(ARR, LEN)\
    ( sizeof(*ARR) + (sizeof( *(ARR)->data ) * LEN ))

static void *realloc_arr(void *p, hw_uint objsize, hw_uint unitsize, hw_uint len)
{
    p = HW_REALLOC(p, objsize + (unitsize * len));
    if (p == NULL) { perror("OUT OF MEMORY"); abort(); }
    return p;
}


#define ARR_IS_FULL(ARR)\
    ( (ARR)->len <= (ARR)->lenUsed)

#define ARR_NEW(ARR, LEN)\
    {\
        (ARR) = HW_MALLOC(ARR_ALLOC_SIZE(ARR, LEN));   \
        (ARR)->data = (void*) (ARR + 1);            \
        (ARR)->len = LEN;                           \
        (ARR)->lenUsed = 0;                         \
    }

#define ARR_DELETE(ARR) \
    {                   \
        HW_FREE(ARR);      \
    }

#define ARR_PUSH(ARR, V) \
    {                                               \
        if(ARR_IS_FULL(ARR)) {                      \
            (ARR) = realloc_arr(ARR, sizeof(*ARR)           \
                    , sizeof(*(ARR)->data), (ARR)->len*2);  \
            (ARR)->data = (void*)((ARR) + 1);               \
            (ARR)->len *=2;                                 \
        }                                                   \
                                                            \
        (ARR)->data[(ARR)->lenUsed] = V;                    \
        (ARR)->lenUsed += 1;                                \
    }                                                       \

#define ARR_PUSHSTREAM(ARR, VS, LENGTH)\
    {                                                               \
        if(((ARR)->lenUsed + LENGTH) >= (ARR)->len) {               \
            (ARR) = realloc_arr(ARR, sizeof(*ARR)                   \
                    , sizeof(*(ARR)->data), (ARR)->len + LENGTH);   \
            (ARR)->data = (void*)((ARR) + 1);                       \
            (ARR)->len +=LENGTH;                                    \
        }                                                               \
        memcpy((ARR)->data + (ARR)->lenUsed, VS, sizeof(*VS)*LENGTH);   \
        (ARR)->lenUsed += LENGTH;                                   \
    }

#define ARR_POP(ARR)            \
    {                           \
        (ARR)->lenUsed -= 1;    \
    }

#define ARR_NEWFROM(ARR, DATA, LEN)\
    {                                                           \
        ARR_NEW(ARR, LEN);                                      \
        memcpy((ARR)->data, DATA, sizeof(*(ARR)->data) * LEN);  \
        (ARR)->lenUsed = LEN;                                   \
    }
    
#define ARR_CLONE(ARR, DEST_ARR)\
    {\
        ARR_NEWFROM(ARR, (DEST_ARR)->data, (DEST_ARR)->lenUsed);\
    }

/*************************************************************/
/*************************************************************/

#define ARR_IMPL_NEW(ARRT, name, VT)\
    ARRT* CAT(name, new)(hw_uint len) {     \
        ARRT *arr;                          \
        ARR_NEW(arr, len);                  \
        return arr;                         \
    }                                       \

#define ARR_IMPL_NEWFROM(ARRT, name, VT)\
    ARRT *CAT(name, newFrom)(VT data[static const 1], hw_uint const len) {\
        ARRT *arr;\
        ARR_NEWFROM(arr, data, len);\
        return arr;\
    }

#define ARR_IMPL_CLONE(ARRT, name, VT)     \
    ARRT *CAT(name, clone)(ARRT const **arr) {  \
        ARRT *res = NULL;                       \
        ARR_CLONE(res,  *arr);                  \
        return res;                             \
    }

#define ARR_IMPL_DELETE(ARRT, name, VT)\
    void CAT(name, delete)(ARRT* arr) {     \
        ARR_DELETE(arr);                    \
    }                                       \

#define ARR_IMPL_PUSH(ARRT, name, VT)\
    VT* CAT(name, push)(ARRT** arr, VT value){  \
        ARRT* prep_arr = *arr;                  \
        ARR_PUSH(prep_arr, value);              \
        *arr = prep_arr;                                    \
        return (prep_arr)->data + ((prep_arr)->lenUsed-1);  \
    }                                                       \

#define ARR_IMPL_PUSHSTREAM(ARRT, name, VT)\
    VT* CAT(name, pushstream)(ARRT** arr, VT values[static const 1], hw_uint len){    \
        ARRT* prep_arr = *arr;                                  \
        ARR_PUSHSTREAM(prep_arr, values, len);                  \
        *arr = prep_arr;                                    \
        return (prep_arr)->data + ((prep_arr)->lenUsed-1);  \
    }                                                       \


#define ARR_IMPL_POP(ARRT, name, VT)\
    hw_uint CAT(name, pop)(ARRT** arr) {        \
        ARRT *prep_arr = *arr;                  \
        ARR_POP(prep_arr);                      \
        return (prep_arr)->lenUsed;             \
    }                                           \

#define ARR_IMPL_SERIALIZE(ARRT, name, VT)

#define ARR_IMPL(ARRT, name, VT)\
    ARR_IMPL_NEW(ARRT, name, VT)\
    ARR_IMPL_NEWFROM(ARRT, name, VT)\
    ARR_IMPL_DELETE(ARRT, name, VT)\
    ARR_IMPL_PUSH(ARRT, name, VT)\
    ARR_IMPL_PUSHSTREAM(ARRT, name, VT)\
    ARR_IMPL_POP(ARRT, name, VT)\
    //ARR_IMPL_COPY(ARRT, name, VT)\

/**
 * Section: hw_uintArr
 */
ARR_IMPL(hw_uintArr, hw_uintArr, hw_uint)

/**
 * Section: hw_byteArr
 */
ARR_IMPL(hw_byteArr, hw_byteArr, hw_byte)

/**
 * Section: hw_codeArr
 */

ARR_IMPL(hw_codeArr, hw_codeArr, hw_code)

/**
 * Section: hw_String
 */
ARR_IMPL(hw_String, hw_String, hw_byte)

/**************************************************************************/
/**************************************************************************/

/**
 * : Basic Type Operations
 *      - var_new -> Create a new variable/object from the passed data
 *      - var_del -> delete the variable
 *      - var_tobyte -> convert data to bytestream
 *      - var_tostr  -> convert data to string_data
 */

