#ifndef ZAKAROUF_HAYWIRE_H
#define ZAKAROUF_HAYWIRE_H

#include <stdint.h>
#include <float.h>

typedef void*               hw_ptr;
typedef uint8_t             hw_byte;

typedef int64_t             hw_int;
typedef uint64_t            hw_uint;
typedef double              hw_float;

#define HW_WORD_SIZE        (sizeof(hw_uint))

#define HW_UINT_MAX         UINT64_MAX
#define HW_INT_MAX          INT64_MAX
#define HW_TYPEID_MAX       UINT8_MAX

typedef union   hw_Var      hw_Var;

typedef struct  hw_VarP     hw_VarP;
typedef struct  hw_VarList  hw_VarList;
typedef struct  hw_VarArr   hw_VarArr;
typedef struct  hw_String   hw_String;
typedef struct  hw_CStr     hw_CStr;
typedef struct  hw_uintArr  hw_uintArr;
typedef struct  hw_byteArr  hw_byteArr;

/************************************************************/
typedef struct hw_Error         hw_Error;
/************************************************************/

typedef struct  hw_Allocator    hw_Allocator;

typedef struct  hw_Type         hw_Type;
typedef struct  hw_TypeSys      hw_TypeSys;

typedef struct  hw_Var_VTCore   hw_Var_VTCore;
typedef struct  hw_VarFnArr     hw_VarFnArr;

/************************************************************/

typedef union   hw_code     hw_code;
typedef struct  hw_codeArr  hw_codeArr;

typedef struct  hw_FnState      hw_FnState;
typedef struct  hw_FnSave       hw_FnSave;
typedef struct  hw_FnSaveArr    hw_FnSaveArr;

typedef struct  hw_Module       hw_Module;
typedef struct  hw_ModuleArr    hw_ModuleArr;
typedef struct  hw_Thread       hw_Thread;
typedef struct  hw_ThreadArr    hw_ThreadArr;
typedef struct  hw_State        hw_State;

/************************************************************/

typedef struct hw_ModuleObj hw_ModuleObj;
typedef struct hw_ObjCompiler hw_ObjCompiler;

/************************************************************/
/************************************************************/

union hw_Var {
    hw_ptr          as_ptr,     *as_ptr_p;
    hw_int          as_reff,    *as_reff_p;

    hw_Error const  *as_error;
    hw_VarList      *as_list,   **as_list_p;
    hw_VarArr       *as_arr,    **as_arr_p;

    hw_byteArr      *as_barr,   **as_barr_p;
    hw_String       *as_string, **as_string_p;
    hw_CStr         *as_cstr,   **as_cstr_p;

    hw_byte         as_byte,    *as_byte_p,  **as_byte_pp;
    hw_byte         as_word[HW_WORD_SIZE],   *(as_word_p[HW_WORD_SIZE]);

    hw_uint         as_uint,    *as_uint_p,  **as_uint_pp;
    hw_int          as_int,     *as_int_p,   **as_int_pp;
    hw_float        as_float,   *as_float_p, **as_float_pp;

    hw_Type    const    *as_type,     **as_type_p;
    hw_TypeSys const    *as_typesys,  **as_typesys_p;

    hw_Type         *as_type_mut, **as_type_mut_p;

    hw_Module       *as_module, **as_module_p;
    hw_Thread       *as_thread, **as_thread_p;
    hw_State        *as_vm,  **as_vm_p;
};

struct hw_VarP {
    hw_Var  value;
    hw_byte type;
};

enum hw_TypeID {
      hw_TypeID_NIL = 0
    , hw_TypeID_REFF

    , hw_TypeID_ERROR
    , hw_TypeID_LIST
    , hw_TypeID_ARR

    , hw_TypeID_BARR
    , hw_TypeID_STRING

    , hw_TypeID_UINT
    , hw_TypeID_INT
    , hw_TypeID_FLOAT


    , hw_TypeID_TYPE
    , hw_TypeID_TYPESYS

    , hw_TypeID_MODULE
    , hw_TypeID_THREAD
    , hw_TypeID_STATE

    , hw_TypeID_TOTAL
};

struct hw_Error {
    hw_uint ecode;
    hw_byte const *str;
    hw_uint       str_size;
};

struct hw_Allocator {
    hw_ptr  state;
    hw_ptr  (*alloc)    (hw_ptr *state, size_t size);
    void    (*free)     (hw_ptr *state, hw_ptr pointer);
    hw_ptr  (*realloc)   (hw_ptr *state, hw_ptr, size_t size);
};

/*************************************************************/
/*************************************************************/

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

struct hw_VarHash {
    hw_Var *key;
    hw_Var *val;

    hw_byte *keyT;
    hw_byte *valT;
    hw_byte *is_used;
};

/****************************************************/
/****************************************************/

typedef hw_VarP (*hw_VarFn)
        (hw_Var *self, hw_TypeSys *T_sys
         , hw_Var const *args, hw_byte const *tid, hw_uint const count);

struct hw_Var_VTCore {
    /**
     * Default initialization
     * args: none
     **/
    hw_VarFn    new;

    /**
     * Init from Serialized data
     * args: (hw_ptr ptr, hw_uint size)
     **/
    hw_VarFn    newFrom_data;

    /**
     * Init from Serialized String
     * args: (hw_String *string)
     **/
    hw_VarFn    newFrom_string;

    /**
     * Init from Serialized String
     * args: (typeof(self))
     **/
    hw_VarFn    newFrom_copy;
 
    /**
     * args: NONE
     **/
    hw_VarFn    delete;

    /**
     * args: NONE
     **/
    hw_VarFn    reset;

    /**
     * args: (typeof(self))
     **/
    hw_VarFn    reset_copy;

    /**
     * args: (typeof(self))
     * ret: hw_int(-MAX, 0, +MAX)
     **/
    hw_VarFn    compare_bin;

    /**
     * args: (list(any))
     **/
    hw_VarFn    compare_all;

    /**
     * args: NONE
     * ret: hw_String *
     **/
    hw_VarFn    to_string;

    /**
     * args: none
     * ret: hw_byteArr *
     **/
    hw_VarFn    to_data;

    /**
     * args: none
     * ret: hw_uint 
     **/
    hw_VarFn    to_hash;
};

struct hw_VarFnArr {
    hw_VarFn        *data;
    hw_uint         len;
    hw_uint         lenUsed;
};

struct hw_Type {
    hw_uint             id;
    hw_byte             name[256];
    hw_uint             name_size;
    hw_uint             unitsize;
    hw_Var_VTCore       vtcore;
    hw_VarFn            vt[8];
    hw_byte             is_obj;
};

struct hw_TypeSys {
    hw_uint     head;

    hw_Type     *types;
    hw_uint     types_used;
    hw_uint     types_total;

    hw_Allocator allocator;
};

/****************************************************/
/****************************************************/

union hw_code {
    uint64_t         as_raw;
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

struct hw_FuncInfo {
    
    hw_byte *name;
    hw_uint name_size;

    /**
     * var = local variable
     * arg = passed variable
     * ret = returned variable,
     *          pushed to stack, on function return
     */
    hw_uint var_count;
    hw_uint arg_count;
    hw_uint ret_count;

    // Total Instructions
    hw_uint code_len;

    // Type IDs
    hw_byte *varT;
    hw_byte *mutT;
    hw_byte *retT;
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
 * hw_Module:
 * LEGEND: 
 *    . -> bit
 *    o -> byte, hw_byte
 *    X -> 8byte = 8o = 64. , hw_uint | hw_int | hw_float
 *    [] -> Stream of data, [o] bytes, [X] 8 bytes
 *  NOTE: If there is a number prefix the symbol that just a mutiple,
 *        4o = 4 bytes,
 *        8. = 8bits = 1byte = o,
 *        8o = X = 8bytes
 *   {o} -> Byte Stream of irregular objects
 *-------------------------------------------------------------------
 * Module Layout
 *    X -> Module Magic Number & Version
 *    X -> Module Size
 *
 *    X -> Module Name Size
 *    X -> Module Name Hash
 *    
 *    X -> FuncInfo (Index under .data)
 *    X -> Const Objects (Index under .data)
 *
 *    X -> Pub Func uintArray (Index under .data)
 *    X -> Pub Const uintArray (Index under .data)
 *
 *    X -> .data Size
 *    X -> .code Size
 *
 *    {o} => .data Section
 *              {o} -> Func Info (hw_FuncInfo)
 *              {o} -> Const Var (hw_Var)
 *              {o} -> rest
 *    [X] => .code Section (hw_code)
 *-------------------------------------------------------------------
 */
struct hw_Module {
    hw_uint data_size;
    hw_uint code_len;
    
    hw_uint func;
    hw_uint pubfunc;

    hw_uint constant;
    hw_uint pubconst;

    hw_byte *data;
    hw_code *code;
};

struct hw_ModuleArr {
    hw_Module *data;
    hw_uint len;
    hw_uint lenUsed;
};

struct hw_Thread {
    hw_uint         id;
    hw_CStr         name;

    hw_VarList      *vstack;
    hw_FnSaveArr    *fstack;

    hw_FnState      fn;

    hw_State const  *global;
};

struct hw_ThreadArr {
    hw_Thread       *data;
    hw_uint         len;
    hw_uint         lenUsed;
};

struct hw_State {
    hw_CStr         name;
    hw_uint         pid;
    hw_ThreadArr    *threads;
    hw_TypeSys      *tsys;
};


/**
 *  Section: Compiler
 *
 * Filetype:
 *  .hw : Source File
 *  .hwasm : Intermediate Bytecode Assembly
 *  .hwo : Compiled Code From .hwasm for the vm
 * Other File Types:
 *  .c : C source file 
 *  .s : Native Assembly
 *  .o : Native object
 *
 * Functionality of various Compiler in hw:
 *  to_hwasm: Convert higher level language source .hw -> .hwasm
 *  to_hwo: Compile .hwasm -> .hwo
 *  to_asm: Convert .hwasm -> .s
 *  to_c: Convert .hwasm -> .c
 */

struct hw_ModuleObj {
    hw_codeArr *code;
    hw_byteArr *data;
    hw_VarList *constants;
};

struct hw_ObjCompiler {
    hw_State *const vm_parent;
    hw_State vm_child;
    
    hw_ModuleObj *obj;
    
    hw_String *in;
    hw_Module *out;
};

#define hw_CStr_makelit(s)     ((hw_CStr){.data = #s, .len = sizeof(#s) - 1})

#endif
