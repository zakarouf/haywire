#ifndef ZAKAROUF_HAYWIRE_H
#define ZAKAROUF_HAYWIRE_H

#include <stdint.h>
#include <float.h>

typedef int64_t             hw_int;
typedef uint64_t            hw_uint;
typedef uint8_t             hw_byte;
typedef double              hw_float;
typedef void*               hw_ptr;

typedef union   hw_Var      hw_Var;
typedef struct  hw_VarList  hw_VarList;
typedef struct  hw_VarArr   hw_VarArr;
typedef struct  hw_String   hw_String;
typedef struct  hw_CStr     hw_CStr;
typedef struct  hw_uintArr  hw_uintArr;
typedef struct  hw_byteArr  hw_byteArr;

typedef union   hw_code     hw_code;
typedef struct  hw_codeArr  hw_codeArr;

typedef struct  hw_FnState      hw_FnState;
typedef struct  hw_FnSave       hw_FnSave;
typedef struct  hw_FnSaveArr    hw_FnSaveArr;

typedef struct  hw_Type         hw_Type;

typedef struct  hw_Module       hw_Module;
typedef struct  hw_ModuleArr    hw_ModuleArr;
typedef struct  hw_Thread       hw_Thread;
typedef struct  hw_ThreadArr    hw_ThreadArr;
typedef struct  hw_State        hw_State;

union hw_Var {
    hw_ptr          as_ptr;
    hw_Var*         as_var;

    hw_uint         as_uint,    *as_uintp,  **as_uintpp;
    hw_int          as_int,     *as_intp,   **as_intpp;
    hw_float        as_float,   *as_floatp, **as_floatpp;
};

struct hw_uintArr {
    hw_uint*        data;
    hw_uint         len;
    hw_uint         lenUsed;
};

struct hw_byteArr {
    hw_byte*        data;
    hw_uint         len;
    hw_uint         lenUsed;
};

struct hw_String {
    hw_byte*        data;
    hw_uint         len;
    hw_uint         lenUsed;
};

struct hw_CStr {
    hw_byte const   *data;
    hw_uint         len;
};

struct hw_VarList {
    hw_Var          *data;
    hw_byte         *tid;
    hw_uint         len;
    hw_uint         lenUsed;
};

struct hw_VarArr {
    hw_Var          *data;
    hw_uint         len;
    hw_uint         lenUsed;
    hw_byte         tid;
};

/****************************************************/
/****************************************************/

union hw_code {
    uint64_t         raw;
    struct {
        hw_byte opcode, attr;
        uint16_t A;
        union {
            uint32_t x32 ;
            int32_t  xi32;
            float    xf32;
            struct {
                uint16_t B, C;
            }; 
        };
    };
};

struct hw_codeArr {
    hw_code *data;
    hw_uint len;
    hw_uint lenUsed;
};

struct hw_FnState {
    hw_Var          *vars;
    hw_uint         var_count;
    hw_code const   *pc;
    hw_uint         id;
    hw_uint         mod;
};

struct hw_FnSave {
    hw_uint     var_start;
    hw_uint     var_count;
    hw_uint     id;
    hw_uint     mod;
    hw_uint     pc;
};

struct hw_FnSaveArr {
    hw_FnSave   *data;
    hw_uint     len;
    hw_uint     lenUsed;
};

/****************************************************/
/****************************************************/

/**
 * LEGEND: 
 *    . -> bit
 *    o -> byte, hw_byte
 *    X -> 8byte, hw_uint | hw_int | hw_float
 *    [] -> Stream of data, [o] bytes, [X] 8 bytes
 *  NOTE: If there is a number prefix the symbol that just a mutiple,
 *        4o = 4 bytes,
 *        8. = 8bits = 1byte = o,
 *        8o = X = 8bytes
 *-------------------------------------------------------------------
 * Module Layout
 *    X -> Module Magic Number & Module version
 *    X -> Module Size
 *
 *    X -> Module Name Size
 *    [o] -> Module Name
 *
 *    X -> Module Function Count
 *    [X] -> Module Function Start points
 *
 *    X -> Public Function Count
 *    [] => {
 *      [X] -> Function Name Size
 *      [o] -> Function Name
 *    }
 *    
 *    X -> Data Length
 *    [] -> Data Stream { Function Data }
 * 
 *    X -> Code Length
 *    [] => Functions { [X] -> Function Code }
 * ------------------------------------------------------------------
 * Function Data Layout
 *    [o] -> Name Data      <- Function_Layout::Data_Start_Index
 *    [X] -> Arg Types
 *    [X] -> Mut Types
 *    [] => Rest            <- [end] = Function_Layout::Data_Size
 *-------------------------------------------------------------------
 * Function Layout
 *    10X 1  -> Data Size 
 *        2  -> Name Size
 *        3  -> Code Size
 *        4  -> Arg Count
 *        5  -> Mut Count
 *        6  -> Var Count
 *        7  -> Data Start (Inside the Module data stream)
 *        8  -> Name Start (Inside the Function data stream)
 *        9  -> ArgT Start (Inside the Function data stream)
 *        10 -> MutT Start (Inside the Function data stream)
 *    [X] -> Code
 *-------------------------------------------------------------------
 */



struct hw_Module {
    hw_uint head;
    hw_byteArr *data;
    hw_codeArr *code;
    hw_uintArr fn_points;
};

struct hw_ModuleArr {
    hw_Module *data;
    hw_uint len;
    hw_uint lenUsed;
};

struct hw_Thread {
    hw_uint         id;
    hw_CStr         name;

    hw_VarList      vstack;
    hw_FnSaveArr    fstack;

    hw_FnState      fn;

    hw_State *global;
};

struct hw_ThreadArr {
    hw_Thread       *data;
    hw_uint         len;
    hw_uint         lenUsed;
};

struct hw_State {
    hw_CStr         name;
    hw_uint         pid;
    hw_ThreadArr    threads;
};

#define hw_CStr_makelit(s)     ((hw_CStr){.data = #s, .len = sizeof(#s) - 1})

#endif
