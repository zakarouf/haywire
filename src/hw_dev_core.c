#include "hw.h"
#include "hw_dev.h"

#include <malloc.h>
#include <stdio.h>
#include <stdlib.h>

/**
 * Section: Pre-Processor
 */
#define CAT2(X, Y) X##_##Y
#define CAT(X, Y) CAT2(X, Y)

/**
 * Section: Array Macro
 */
#define ARR_ALLOC_SIZE(ARR, LEN)\
    ( sizeof(*ARR) + (sizeof( *(ARR)->data ) * LEN ))

static void *realloc_arr(void *p, hw_uint objsize, hw_uint unitsize, hw_uint len)
{
    void *op = p;
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

#define ARR_CLONE(ARR, DEST_ARR)\
    {\
        ARR_NEW(DEST_ARR, (ARR)->len);\
        memcpy(                                                         \
            (DEST_ARR), (ARR)                                           \
          , (sizeof(*(ARR)->data) * (ARR)->lenUsed) + sizeof(*(ARR)) );   \
    }

#define ARR_SERIALIZE(ARR, BARR)\
    {\
        hw_byteArr_pushstream(          \
            BARR                        \
          , (void *)&(ARR)->lenUsed     \
          , sizeof((ARR)->lenUsed));    \
        \
    }

#define _ARR_DESERIALIZE_SIZE_CHECK(SIZE_LEFT, AGAINST, ...)\
    {\
        if(SIZE_LEFT < AGAINST) { __VA_ARGS__; }\
    }

/*************************************************************/
/*************************************************************/

#define ARR_IMPL_NEW(ARRT, name, VT, ...)\
    ARRT* CAT(name, new)(hw_uint len) {     \
        ARRT *arr;                          \
        ARR_NEW(arr, len);                  \
        __VA_ARGS__;                        \
        return arr;                         \
    }                                       \

#define ARR_IMPL_DELETE(ARRT, name, VT, ...)\
    void CAT(name, delete)(ARRT* arr) {     \
        ARR_DELETE(arr);                    \
    }                                       \

#define ARR_IMPL_PUSH(ARRT, name, VT, ...)\
    VT* CAT(name, push)(ARRT** arr, VT value){  \
        ARRT* prep_arr = *arr;                  \
        ARR_PUSH(prep_arr, value);              \
        *arr = prep_arr;                                    \
        return (prep_arr)->data + ((prep_arr)->lenUsed-1);  \
    }                                                       \

#define ARR_IMPL_PUSHSTREAM(ARRT, name, VT, ...)\
    VT* CAT(name, pushstream)(ARRT** arr, VT values[static 1], hw_uint len){    \
        ARRT* prep_arr = *arr;                                  \
        ARR_PUSHSTREAM(prep_arr, values, len);                  \
        *arr = prep_arr;                                    \
        return (prep_arr)->data + ((prep_arr)->lenUsed-1);  \
    }                                                       \


#define ARR_IMPL_POP(ARRT, name, VT, ...)\
    hw_uint CAT(name, pop)(ARRT** arr) {        \
        ARRT *prep_arr = *arr;                  \
        ARR_POP(prep_arr);                      \
        return (prep_arr)->lenUsed;             \
    }                                           \

#define ARR_IMPL_CLONE(ARRT, name, VT, ...)     \
    ARRT *CAT(name, clone)(ARRT const **arr) {  \
        ARRT *res = NULL;                       \
        ARR_CLONE(*arr,  res);                  \
        return res;                             \
    }

#define ARR_IMPL_SERIALIZE(ARRT, name, VT, ...)

#define ARR_IMPL(ARRT, name, VT)\
    ARR_IMPL_NEW(ARRT, name, VT)\
    ARR_IMPL_DELETE(ARRT, name, VT)\
    ARR_IMPL_PUSH(ARRT, name, VT)\
    ARR_IMPL_PUSHSTREAM(ARRT, name, VT)\
    ARR_IMPL_POP(ARRT, name, VT)\
    ARR_IMPL_CLONE(ARRT, name, VT)\
    //ARR_IMPL_COPY(ARRT, name, VT)\

/**
 * Section: hw_uintArr
 */
ARR_IMPL(hw_uintArr, hw_uintArr, hw_uint);

/**
 * Section: hw_byteArr
 */
ARR_IMPL(hw_byteArr, hw_byteArr, hw_byte);

/**
 * Section: hw_codeArr
 */

ARR_IMPL(hw_codeArr, hw_codeArr, hw_code);


/**
 * Section: hw_String
 */

ARR_IMPL(hw_String, hw_String, hw_byte);
hw_String *hw_String_newFrom(hw_byte *data, hw_uint size)
{
    
}

