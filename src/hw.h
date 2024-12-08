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
typedef struct  hw_VarTable hw_VarTable;
typedef struct  hw_SVTable  hw_SVTable;
typedef struct  hw_SITable  hw_SITable;

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
typedef struct  hw_FnInfo       hw_FnInfo;
typedef struct  hw_VarFnArr     hw_VarFnArr;

/************************************************************/

typedef union   hw_code     hw_code;
typedef struct  hw_codeArr  hw_codeArr;

typedef struct  hw_FnState      hw_FnState;
typedef struct  hw_FnStateArr    hw_FnStateArr;

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
    hw_VarArr       *as_arr,    **as_arr_p;
    hw_VarList      *as_list,   **as_list_p;
    hw_VarTable     *as_table,  **as_table_p;

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
    //NOTE: Any is not a internal type.
      hw_TypeID_any = 0

    , hw_TypeID_nil
    , hw_TypeID_ptr
    , hw_TypeID_reff
 
    , hw_TypeID_bool
    , hw_TypeID_byte
    , hw_TypeID_uint
    , hw_TypeID_int
    , hw_TypeID_float

    , hw_TypeID_error
    , hw_TypeID_arr
    , hw_TypeID_list
    , hw_TypeID_table

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

struct hw_VarTable {
    hw_Var      *key;
    hw_Var      *val;
    hw_byte     *valTs;

    hw_uint     len;
    hw_uint     lenUsed;

    hw_VarFn    hashfn;
    hw_VarFn    eqfn;
    hw_byte     keyT;
};

/****************************************************/
/****************************************************/


/**
 * Function Signature;
 * fn name(a, b, c, d, e)
 *    ~~~~ ~~~~~~~~|~~~~ < mut_count
 *  *name^ ^arg_count^
 *  const_count = arg_count - mut_count
 */
struct hw_FnInfo {
    hw_byte const *name;
    hw_byte const *argT;
    hw_uint name_size;
    hw_uint arg_count;
    hw_uint mut_count;
};

struct hw_VarFnArr {
    hw_VarFn        *data;
    hw_uint         len;
    hw_uint         lenUsed;
};

struct hw_Type {
    hw_uint             id;
    hw_uint             unitsize;

    hw_uint             vt_count;
    hw_VarFn            vt[16];
    hw_FnInfo           vtinfo[16];
    
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

enum hw_InstType {
    hw_InstType_nop  // [ins nil nil nil]
  , hw_InstType_a    // [ins ax nil nil]
  , hw_InstType_ab   // [ins ax bx nil] 
  , hw_InstType_abc  // [ins ax bx cx]
  , hw_InstType_ax32 // [ins ax x32]
  , hw_InstType_x32  // [ins nil x32]
  , hw_InstType_as32 // [ins ax s32]
  , hw_InstType_s32  // [ins nil s32]
};

typedef struct hw_InstData hw_InstData;
struct hw_InstData {
    char const       name[16];
    hw_byte          name_size;
    enum hw_InstType inst_type;
    hw_byte          does_return;
};

struct hw_codeArr {
    hw_code *data;
    hw_uint len;
    hw_uint lenUsed;
};

struct hw_FnState {
    hw_uint     v_start;
    hw_uint     id;
    hw_uint     mod;
    hw_uint     pc;
};

struct hw_FnStateArr {
    hw_FnState  *data;
    hw_uint     len;
    hw_uint     lenUsed;
};

/****************************************************/
/****************************************************/

/**
 * hw_Module:
 *-------------------------------------------------------------------
 * Module Layout (Revision)
 *      magic_and_version: u64
 *
 *      module_namesize: u64
 *      module_hash: u64
 *      
 *      func_count: u64
 *      cnst_count: u64
 *      
 *      pub_fn_count: u64
 *      pub_const_count: u64
 *
 *      code_len: u64
 *      data_sz: u64
 *
 *      .data := 
 *          module_name:    [ sizeof(byte) * module_namesize ]
 *          fn_points:      [ sizeof(u64) * func_count ]
 *          const_points:   [ sizeof(u64) * cnst_count ]
 *          pub_fnInfo:     [ hw_FuncInfo * pub_fn_count ]
 *          pub_const:      [ hw_Var * pub_const_count ]
 *
 *      .code := [sizeof(code) * code_len]
 *-------------------------------------------------------------------
 */
struct hw_Module {
    //TODO: Match the specifications above
    hw_uint magic;
    hw_uint name_size;
    hw_uint name_hash;

    hw_uint func;
    hw_uint constant;
    
    hw_uint pubfunc;
    hw_uint pubconst;

    hw_uint data_size;
    hw_uint code_len;

    hw_byte *data;
    hw_code *code;
};

struct hw_ModuleArr {
    hw_Module **data;
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
    hw_FnStateArr            fstack;

    hw_State const          *global;

    hw_byte                 name_size;
    enum hw_ThreadStatus    status;
};

struct hw_ThreadArr {
    hw_Thread       *data;
    hw_uint         len;
    hw_uint         lenUsed;
};

typedef struct hw_ModulePack hw_ModulePack;
struct hw_ModulePack {
    hw_ModuleArr    *loaded;
    hw_VarTable     *table;
};

struct hw_State {
    hw_CStr         name;

    hw_uint         pid;
    hw_Thread       main_thread;
    hw_ThreadArr    *threads;
    hw_ModulePack   modules;

    hw_TypeSys      *tsys;
    hw_InstData     const *insts;
    hw_byte         insts_count;
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
