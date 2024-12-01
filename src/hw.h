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
/*
 * NOTE: `args` passed as list assume the list is allocated from somewhere else,
 *      therefore it doesn't mutate the list but can mutate its members.
 *      This interface will result in Failure if the allocated list is created
 *      on function call. Such as:=
 *
 *          fn(typesystem, (hw_Var[]){ var }, ...)
 *
 *      Here `var` is only exist on the scope of function call.
 *      If `var` is mutated in anyway, such as allocating memory on heap and
 *      assigning ptr to var; results in memory leak
 */
typedef hw_VarP (*hw_VarFn)
        (hw_TypeSys *T_sys, hw_Var *args, hw_byte *tid, hw_uint const count);

/************************************************************/

typedef struct hw_ModuleObj hw_ModuleObj;
typedef struct hw_ObjCompiler hw_ObjCompiler;

/************************************************************/
/************************************************************/

union hw_Var {
    /* Primitives */
    hw_byte         as_nil;
    hw_ptr          as_ptr,     *as_ptr_p;
    hw_int          as_reff,    *as_reff_p;
    hw_Var          *as_vptr;

    hw_int          as_bool;
    hw_byte         as_byte,    *as_byte_p,  **as_byte_pp;
    hw_uint         as_uint,    *as_uint_p,  **as_uint_pp;
    hw_int          as_int,     *as_int_p,   **as_int_pp;
    hw_float        as_float,   *as_float_p, **as_float_pp;

    hw_byte         as_word[HW_WORD_SIZE],   *(as_word_p[HW_WORD_SIZE]);
    
    /* FnPtr */
    hw_VarFn        as_nativefn;
    
    /* Error Object */
    hw_Error const  *as_error;

    /* Objects */
    hw_VarList      *as_list,   **as_list_p;
    hw_VarArr       *as_arr,    **as_arr_p;

    hw_byteArr      *as_barr,   **as_barr_p;
    hw_String       *as_string, **as_string_p;
    hw_CStr         *as_cstr,   **as_cstr_p;

    /* Type System */
    hw_Type    const    *as_type,     **as_type_p;
    hw_TypeSys const    *as_typesys,  **as_typesys_p;

    hw_Type         *as_type_mut, **as_type_mut_p;

    /* VM */
    hw_Module       *as_module, **as_module_p;
    hw_Thread       *as_thread, **as_thread_p;
    hw_State        *as_vm,  **as_vm_p;
};

struct hw_VarP {
    hw_Var  value;
    hw_byte type;
};

enum hw_TypeID {
      hw_TypeID_nil = 0
    , hw_TypeID_ptr
    , hw_TypeID_reff
 
    , hw_TypeID_bool
    , hw_TypeID_byte
    , hw_TypeID_uint
    , hw_TypeID_int
    , hw_TypeID_float

    , hw_TypeID_error
    , hw_TypeID_list
    , hw_TypeID_arr

    , hw_TypeID_barr
    , hw_TypeID_string
    , hw_TypeID_type
    , hw_TypeID_typesys

    , hw_TypeID_module
    , hw_TypeID_thread
    , hw_TypeID_state

    , hw_TypeID_TOTAL
};

struct hw_Error {
    hw_uint error;
    hw_uint table;
    hw_byte const *str;
    hw_uint       str_size;
    hw_VarP context;
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


struct hw_VarFnInfo {
    char const *name;
    hw_uint name_size;
    hw_byte const *argT;
    hw_byte const *retT;
};


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
     * Init from passed config as hw_Vars
     * args: (...) <- dependent on each type
     */
    hw_VarFn    newFrom_conf;

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
     * args: NONE
     * ret: hw_String *
     **/
    hw_VarFn    to_info;
 
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
    hw_uint             unitsize;
    hw_Var_VTCore       vtcore;
    hw_VarFn            vt[8];
    hw_byte             name[256];

    hw_byte             is_obj;
    hw_byte             name_size;
};

struct hw_TypeSys {
    hw_Type     *types;
    hw_uint     types_used;
    hw_uint     types_total;

    hw_State    **state;
    hw_Allocator allocator;
};

/****************************************************/
/****************************************************/
struct hw_CodeStructX {
    uint8_t opcode;
    uint8_t attr;
    uint16_t A;
    uint32_t x32;
};

struct hw_CodeStructS {
    uint8_t opcode;
    uint8_t attr;
    uint16_t A;
     int32_t s32;
};

struct hw_CodeStruct {
    uint8_t opcode;
    uint8_t attr;
    uint16_t A, B, C;
};

union hw_code {
    uint64_t                raw;
    struct hw_CodeStruct    get;
    struct hw_CodeStructX   getx;
    struct hw_CodeStructS   gets;
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

enum hw_ThreadStatus {
    hw_ThreadStatus_READY
  , hw_ThreadStatus_DONE
  , hw_ThreadStatus_RUNNING
  , hw_ThreadStatus_WAITING
  , hw_ThreadStatus_SLEEPING
  , hw_ThreadStatus_CRASH
  , hw_ThreadStatus_TOTAL
};

struct hw_Thread {
    hw_uint                 id;
    hw_byte                 name[128];

    hw_VarList              *vstack;
    hw_FnSaveArr            fstack;

    hw_FnState              fn;

    hw_State const          *global;

    hw_byte                 name_size;
    enum hw_ThreadStatus    status;
};

struct hw_ThreadArr {
    hw_Thread       *data;
    hw_uint         len;
    hw_uint         lenUsed;
};

struct hw_State {
    hw_CStr         name;
    hw_uint         pid;
    hw_Thread       main_thread;
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

#define HW_STATIC_ASSERT(exp)\
    ((void)(char[(exp)? 1:-1]){0})

#define hw_CStr_makelit(s)     ((hw_CStr){.data = #s, .len = sizeof(#s) - 1})

#define HW_VARP(_value, _as_)\
    ((hw_VarP){.value.as_##_as_ = _value, .type = hw_TypeID_##_as_})

#define hw_code_x32(code, ins, a, _32)\
    ((hw_code){.opcode = ins, .getx.A = a, .getx.x32 = _32})

#define HW_FALSE 0
#define HW_TRUE (!HW_FALSE)

#define HW_VARP_NIL()       HW_VARP(0, nil)
#define HW_VARP_INT(v)      HW_VARP(v, int)
#define HW_VARP_UINT(v)     HW_VARP(v, uint)
#define HW_VARP_FLOAT(v)    HW_VARP(v, float)
#define HW_VARP_BYTE(v)     HW_VARP(v, byte)
#define HW_VARP_BOOL(v)     HW_VARP(v, bool)
#define HW_VARP_ERROR(v)    HW_VARP(v, error)

#endif
