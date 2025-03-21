#ifndef ZAKAROUF_HAYWIRE_H
#define ZAKAROUF_HAYWIRE_H

#include <stdint.h>
#include <float.h>
#include <pthread.h>

/**
 * Generic Array
 */
#define HW_ARR(T) struct { T *data; hw_u32 lenUsed; hw_u32 len; }
#define HW_ARR_NEW(s, a, _len)\
    {                                                       \
        (a) = HW_THREAD_ALLOC(s,                            \
                sizeof(*a) + (sizeof(*(a)->data) * _len));  \
        (a)->data = HW_CAST(void*, a + 1);                  \
        (a)->len = _len;                                    \
        (a)->lenUsed = 0;                                   \
    }

#define HW_ARR_DELETE(s, a)\
    {\
        HW_THREAD_FREE(s, a);\
    }

#define HW_ARR_RESERVE(s, a, size)\
  {                                             \
      if(((s)->len - (s)->lenUsed) < (size)) {  \
          HW_ARR_EXPAND(s, a, (size));          \
      }                                         \
  }

#define HW_ARR_EXPAND(s, a, by)\
    {                                                       \
        (a)->len += by;                                     \
        a = HW_THREAD_REALLOC(s, a                          \
            , sizeof(*a) + (sizeof(*(a)->data) * (a)->len));\
        (a)->data = HW_CAST(void*, a + 1);                  \
    }

#define HW_ARR_PUSHINC(s, a)\
    {\
        if((a)->lenUsed >= (a)->len ) {                         \
            HW_ARR_EXPAND(s, a, (a)->len);                      \
        }                                                       \
        (a)->lenUsed += 1;                                      \
    }

#define HW_ARR_PUSH(s, a, dat)\
    {                                                           \
        HW_ARR_PUSHINC(s, a)                                    \
        (a)->data[(a)->lenUsed-1] = dat;                        \
    }                                                           \

#define HW_ARR_PUSHSTREAM(s, a, dats, dat_size)                 \
    {                                                           \
        if(((a)->lenUsed+(dat_size)) >= (a)->len ) {            \
            HW_ARR_EXPAND(s, a, ((dat_size) + 1))               \
        }                                                       \
        memcpy((a)->data + (a)->lenUsed, dats                   \
                , ((dat_size) + 0) * sizeof(*(a)->data));       \
        (a)->lenUsed += (dat_size);                             \
    }                                                           \

#define HW_ARR_TOP(ARR) (ARR)->data[(ARR)->lenUsed-1]

/**
 * Types
 */

typedef void*               hw_ptr;
typedef uint8_t             hw_byte;

typedef int64_t             hw_int;
typedef uint64_t            hw_uint;

typedef double              hw_float;
typedef float               hw_f32x2[2];

typedef int32_t             hw_i32;
typedef uint32_t            hw_u32;
typedef int16_t             hw_i16;
typedef uint16_t            hw_u16;

typedef hw_u32              hw_u32x2[2];
typedef hw_i32              hw_i32x2[2];
typedef hw_u16              hw_u16x4[4];
typedef hw_i16              hw_i16x4[4];
typedef hw_byte             hw_byte8[8];

typedef _Bool               hw_bool;

#define HW_UINT_MAX         UINT64_MAX
#define HW_INT_MAX          INT64_MAX
#define HW_INT_MIN          INT64_MIN
#define HW_FLOAT_MAX        DBL_MAX
#define HW_FLOAT_MIN        DBL_MIN
#define HW_TYPEID_MAX       UINT8_MAX

#define HW_WORD_SIZE        (sizeof(hw_uint))

typedef union   hw_Var      hw_Var;
typedef struct  hw_VarP     hw_VarP;

typedef struct  hw_VarArr   hw_VarArr;
typedef struct  hw_SArr     hw_SArr;
typedef struct  hw_VarList  hw_VarList;

typedef struct  hw_VarTable hw_VarTable; // (ANY:ANY)
typedef struct  hw_SymTable hw_SymTable; // (STRING:ANY)
typedef struct  hw_KaTable  hw_KaTable;  // (K:ANY)
typedef struct  hw_KvTable  hw_KvTable;  // (K:V)

typedef struct  hw_String   hw_String;
typedef struct  hw_CStr     hw_CStr;
typedef struct  hw_u32Arr   hw_u32Arr;
typedef struct  hw_uintArr  hw_uintArr;
typedef struct  hw_byteArr  hw_byteArr;

typedef struct hw_Struct hw_Struct;
typedef struct hw_StructObj hw_StructObj;
typedef struct hw_Sum hw_Sum;
typedef struct hw_SumDef hw_SumDef;

typedef struct hw_Lexer hw_Lexer;
typedef struct hw_LexToken hw_LexToken;
typedef struct hw_LexTokenArr hw_LexTokenArr;

/************************************************************/
typedef struct hw_Error         hw_Error;
/************************************************************/

/**
 * Allocator
 */
typedef struct  hw_Allocator    hw_Allocator;
typedef struct hw_ArenaRegion hw_ArenaRegion;
typedef struct hw_Arena hw_Arena;
#define HW_ARENAREGION_FRAG_COUNT 16

/**/
typedef struct  hw_Type         hw_Type;
typedef struct  hw_TypeSys      hw_TypeSys;
typedef struct  hw_FnInfo       hw_FnInfo;
typedef struct  hw_FnInfoArr    hw_FnInfoArr;
typedef struct  hw_VarFnArr     hw_VarFnArr;

/************************************************************/

typedef union   hw_code         hw_code;
typedef struct  hw_codeArr      hw_codeArr;

typedef struct  hw_FnState      hw_FnState;
typedef struct  hw_FnStateArr   hw_FnStateArr;

typedef struct  hw_Module       hw_Module;
typedef struct  hw_ModuleArr    hw_ModuleArr;
typedef struct  hw_State        hw_State;
typedef struct  hw_Global       hw_Global;

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
        (hw_State *state, hw_Var *args, hw_byte *tid, hw_uint const count);

/************************************************************/

typedef struct hw_ModuleObj hw_ModuleObj;
typedef struct hw_CompilerBC hw_CompilerBC;

/************************************************************/
/************************************************************/

union hw_Var {
    /* Primitives */
    hw_byte         as_nil;
    hw_ptr          as_ptr,     *as_ptr_p;
    hw_int          as_reff,    *as_reff_p;
    hw_Var          *as_var;

    hw_int          as_bool;
    hw_byte         as_byte,    *as_byte_p,  **as_byte_pp;
    hw_byte               const *as_cbyte_p;
    hw_uint         as_uint,    *as_uint_p,  **as_uint_pp;
    hw_int          as_int,     *as_int_p,   **as_int_pp;
    hw_float        as_float,   *as_float_p, **as_float_pp;

    hw_byte         as_word[HW_WORD_SIZE],   *(as_word_p[HW_WORD_SIZE]);
    hw_u32x2        as_u32x2;
    hw_i32x2        as_i32x2;
    hw_u16x4        as_u16x4;
    hw_i16x4        as_i16x4;

    /* FnPtr */
    hw_VarFn        as_nativefn;
    
    /* Error Object */
    hw_Error const  *as_error;

    /* Structs */
    hw_Struct       *as_struct;
    hw_Sum          *as_sum;
    hw_StructObj    *as_structobj;
    hw_SumDef       *as_sumdef;

    /* Objects */
    hw_byteArr      *as_bytearr,   **as_bytearr_p;
    hw_String       *as_string, **as_string_p;
    hw_CStr         *as_cstr,   **as_cstr_p;
    hw_Lexer        *as_lexer;

    hw_VarArr       *as_arr,    **as_arr_p;
    hw_SArr         *as_sarr,   **as_sarr_p;
    hw_VarList      *as_list,   **as_list_p;
    hw_VarTable     *as_table,  **as_table_p;
    hw_SymTable     *as_symtable,  **as_symtable_p;

    /* Type System */
    hw_Type    const    *as_type,     **as_type_p;
    hw_TypeSys const    *as_typesys,  **as_typesys_p;

    hw_Type             *as_type_mut, **as_type_mut_p;

    /* VM */
    hw_Module       *as_module, **as_module_p;
    hw_State        *as_state,  **as_state_p;
    hw_Global       *as_global, **as_global_p;
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
    , hw_TypeID_struct
    , hw_TypeID_sum
    , hw_TypeID_structobj
    , hw_TypeID_sumdef

    , hw_TypeID_array
    , hw_TypeID_sarr
    , hw_TypeID_list
    , hw_TypeID_table
    , hw_TypeID_symtable
    , hw_TypeID_katable
    , hw_TypeID_kvtable

    , hw_TypeID_bytearr
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

/**
 * Memory Allocators
 */
struct hw_Allocator {
    hw_Var  state;
    void    (*delete)   (hw_Allocator *self);
    hw_ptr  (*alloc)    (hw_Allocator *self, size_t size);
    void    (*free)     (hw_Allocator *self, hw_ptr pointer);
    hw_ptr  (*realloc)  (hw_Allocator *self, hw_ptr, size_t size);
};

struct hw_ArenaRegion {
    hw_u32    capacity;
    hw_u32    used;
    hw_u32    frags[HW_ARENAREGION_FRAG_COUNT];
    hw_ArenaRegion  *next;
    hw_byte         *pool;
};

struct hw_Arena {
    hw_ArenaRegion *begin
                 , *at
                 , *end;
    hw_u32  default_size;
};

/*************************************************************/
/*************************************************************/
struct hw_LexToken {
    hw_byte const   *start;
    hw_uint         size;
    hw_byte         type;
};

struct hw_LexTokenArr {
    hw_LexToken     *data;
    hw_u32          len;
    hw_u32          lenUsed;
};

struct hw_Lexer {
    hw_byte const   *begin;
    hw_byte const   *at;
    hw_byte const   *end;
    hw_LexToken     token;
};

struct hw_Struct {
    hw_u32  count;
    hw_u32  tag;
    hw_byte *tids;
    hw_Var  *data;
};

struct hw_StructObj {
    hw_uint *tags;
    hw_Var  *data;
};

struct hw_Sum {
    hw_uint tid:8, tag:56;
    hw_Var value;
};

struct hw_SumDef {
    hw_Struct   *defs;
    hw_Var      *symbols;
};

struct hw_u32Arr {
    hw_u32  *data;
    hw_u32  len;
    hw_u32  lenUsed;
};

struct hw_uintArr {
    hw_uint*        data;
    hw_u32         len;
    hw_u32         lenUsed;
};

struct hw_byteArr {
    hw_byte*        data;
    hw_uint         len;
    hw_uint         lenUsed;
};

struct hw_String {
    hw_byte*        data;
    hw_u32         len;
    hw_u32         lenUsed;
};

struct hw_CStr {
    hw_byte         *data;
    hw_uint         len;
};


struct hw_VarList {
    hw_Var          *data;
    hw_byte         *tid;
    hw_u32         len;
    hw_u32         lenUsed;
};

struct hw_VarArr {
    hw_Var          *data;
    hw_u32          len;
    hw_u32          lenUsed;
    hw_byte         tid;
};

struct hw_SArr {
    hw_byte        *data;
    hw_u32         len;
    hw_u32         lenUsed;
    hw_u32         unitsize;
};

struct hw_VarTable {
    hw_Var *key;
    hw_Var *val;

    hw_byte *valTs;
    hw_byte *keyTs;

    hw_uint len;
    hw_uint lenUsed;
};

struct hw_SymTable {
    hw_CStr     **key;
    hw_Var      *val;
    hw_byte     *valTs;

    hw_uint     len;
    hw_uint     lenUsed;
};

struct hw_KaTable {
    hw_Var      *key;
    hw_Var      *val;
    hw_byte     *valTs;

    hw_uint     len;
    hw_uint     lenUsed;

    hw_VarFn    hashfn;
    hw_VarFn    eqfn;
    hw_byte     keyT;
};

struct hw_KvTable {
    hw_Var      *key;
    hw_Var      *val;

    hw_uint     len;
    hw_uint     lenUsed;

    hw_VarFn    hashfn;
    hw_VarFn    eqfn;

    hw_byte     keyT;
    hw_byte     valT;
};


/****************************************************/
/****************************************************/


/**
 * Function Signature;
 * fn name(a, b, c, d, e)
 *    ~~~~ |~~~~|~~~~~~~~ <args
 *  *name^   ^mut_arg
 *  passed_count = arg_count - mut_count
 *  mut args are values passed to and then return from a function;
 */
struct hw_FnInfo {
    hw_byte const *name;
    hw_byte const *types;
    hw_uint name_size;
    hw_uint arg_count;
    hw_uint mut_count;
    hw_uint stack_sz;
};

struct hw_FnInfoArr {
    hw_FnInfo   *data;
    hw_u32      len;
    hw_u32      lenUsed;
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
    
    hw_byte             name[32];
    hw_byte             c_name[32];
    hw_byte             is_obj;
    hw_byte             name_size;
    hw_byte             c_name_size;
};

struct hw_TypeSys {
    hw_Type     *types;
    hw_uint     types_used;
    hw_uint     types_total;
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

typedef struct hw_InstInfo hw_InstInfo;
struct hw_InstInfo {
    char const       name[16];
    hw_byte          name_size;
    enum hw_InstType inst_type;
    hw_byte          inst_code;    
    hw_byte          no_direct;
    hw_byte const *  brief;
    hw_u32           brief_size;
};

struct hw_codeArr {
    hw_code *data;
    hw_uint len;
    hw_uint lenUsed;
};

struct hw_FnState {
    hw_uint     fn;
    hw_uint     mod;
    hw_uint     pc;
    hw_uint     var;
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
 *          knst:           [ hw_Var * k_count]
 *          pub_fnInfo:     [ hw_FuncInfo * pub_fn_count ]
 *
 *      .code := [sizeof(code) * code_len]
 *-------------------------------------------------------------------
 */
struct hw_Module {
    //TODO: Match the specifications above
    hw_uint magic;

    hw_uint fn_count;
    hw_uint pubfn_count;
    hw_uint pubk_count;

    hw_uint data_size;
    hw_uint code_len;
    hw_uint k_count;

    hw_uint *fnpt;
    hw_byte *data;
    hw_code *code;

    hw_byte *knst_t;
    hw_Var  *knst;
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

struct hw_State {
    hw_uint                 id;
    hw_byte                 name[128];

    hw_VarList              *vstack;
    hw_FnStateArr           *fstack;

    hw_TypeSys const        *ts;
    hw_Global const         *global;
    hw_Allocator            allocator;

    void                    *stdout;
    void                    *stdin;

    hw_byte                 name_size;
    enum hw_ThreadStatus    status;
};


typedef struct hw_ModulePack hw_ModulePack;
struct hw_ModulePack {
    hw_ModuleArr    *loaded;
    hw_VarTable     *table;
};

struct hw_Global {
    hw_CStr         name;
    hw_uint         pid;

    pthread_mutex_t mutex;

    hw_ModulePack   modules;
    hw_State        *parent;
    hw_VarList      *constants;

    hw_VarFnArr     *builtin;
    hw_SymTable     *builtin_names;

    hw_TypeSys      *tsys;
    hw_InstInfo     const *insts;
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

typedef struct hw_DeferInstArr hw_DeferInstArr;
typedef struct hw_DeferInst hw_DeferInst;

typedef struct hw_VarInfo hw_VarInfo;
typedef struct hw_FnObj hw_FnObj;

struct hw_ModuleObj {
    hw_uintArr      *fnpt;
    hw_SymTable     *fntable;
    
    HW_ARR(struct { hw_byte const *symb;
                    hw_u32 symb_sz; 
                    hw_u32 pc; 
            }) *defer_fncall;
    
    hw_codeArr      *code;
    hw_byteArr      *data;

    hw_VarList      *knst;
    hw_SymTable     *knsttable;
};

struct hw_VarInfo {
    hw_Var  value;
    hw_uint unique_reff;
    hw_byte is_mut:1
          , is_unique:1
          , is_val_tracked:1;
    hw_byte type;
};

struct hw_VarInfoArr {
    struct hw_VarInfo *data;
    hw_u32 len;
    hw_u32 lenUsed;
};

struct hw_DeferInst {
    hw_LexToken symbol;
    hw_uint inst;
    hw_byte operand;
};

struct hw_DeferInstArr {
    struct hw_DeferInst *data;
    hw_u32 len;
    hw_u32 lenUsed;
};

struct hw_FnObj {
    hw_uint name_size;
    hw_uint name_sizeUsed;
    hw_uint name_hash;
    hw_uint current_fn;
    hw_uint args_passed;
    hw_uint mut_count;
    hw_uint defn_pc;

    hw_byte *name;
    hw_SymTable *lables;
    hw_SymTable *vartable;
    struct hw_VarInfoArr *var_infos;
    struct hw_DeferInstArr *defer_lables;

    hw_byte lock;
    hw_byte operand;
};

/**
 * Section: Lexer
 */
struct hw_CompilerBC {
    hw_State        *vm_parent;
    hw_State        *vm_child;

    hw_FnObj        *fnobj;
    hw_ModuleObj    *mobj;
    
    hw_Lexer        lexer;   
    hw_CStr         sname;
    hw_CStr         source;
    hw_CStr         out_name;
};

#define HW_STATIC_ASSERT(exp)\
    ((void)(char[(exp)? 1:-1]){0})

#define hw_CStr_LIT(s)     ((hw_CStr){.data = (void*) #s, .len = sizeof(#s) - 1})

#define HW_VAR(_value, _as_)\
    ((hw_Var){._as_ = _value})

#define HW_VARP(_value, _as_)\
    ((hw_VarP){.value.as_##_as_ = _value, .type = hw_TypeID_##_as_})

#define hw_code_abc(ins, a, b, c)\
    ((hw_code){ .get.opcode = ins, .get.A = a, .get.B = b, .get.C = c})

#define hw_code_as32(ins, a, _32)\
    ((hw_code){ .gets.opcode = ins, .gets.A = a, .gets.s32 = _32})

#define hw_code_ax32(ins, a, _32)\
    ((hw_code){ .getx.opcode = ins, .getx.A = a, .getx.x32 = _32})


#define HW_FALSE 0
#define HW_TRUE (!HW_FALSE)

#define HW_VARP_NIL()       HW_VARP(0, nil)
#define HW_VARP_INT(v)      HW_VARP(v, int)
#define HW_VARP_UINT(v)     HW_VARP(v, uint)
#define HW_VARP_FLOAT(v)    HW_VARP(v, float)
#define HW_VARP_BYTE(v)     HW_VARP(v, byte)
#define HW_VARP_BOOL(v)     HW_VARP(v, bool)
#define HW_VARP_ERROR(v)    HW_VARP(v, error)

#define HW_MODULE_SIZE(m, fn_count, code_len, data_size, k_count)\
          (sizeof(*m)\
              + (fn_count *    sizeof(*m->fnpt))\
              + (code_len *    sizeof(*m->code))\
              + (data_size *   sizeof(*m->data))\
              + (k_count *     sizeof(*m->knst))\
              + (k_count *     sizeof(*m->knst_t)))

#endif
