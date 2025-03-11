#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>
#include <string.h>

#include "hw.h"
#include "hw_dev.h"

#ifdef HW_COMPILE_SINGLE
#include "hw_debug.c"
#include "hw_dev_core.c"
#include "hw_dev_lexer.c"
#include "hw_dev_types.c"
#include "hw_compbc.c"
#include "hw_vm.c"
#endif

static void _check_vm_inst(hw_State const *hw)
{
    hw_Global const *global = hw->global;
    size_t i;
    for (i = 0; i < global->insts_count; i++) {
        HW_DEBUG( 
          hw_logp("%"PRIu64 "->", i);
          hw_logstr(global->insts[i].name, global->insts[i].name_size);
          putc(' ', stdout);
        )
        HW_ASSERTEX(global->insts[i].name, "%"PRIu64"", i);
        HW_ASSERTEX(global->insts[i].name_size, "%"PRIu64, i);
    }
}

static hw_VarArr *_wrap_args(hw_State *s, int argc, char *argv[])
{
    hw_Var arr;
    hw_Var args[4] = {
                {0}
              , (hw_Var){.as_uint = hw_TypeID_string} };

    hw_VarArr_newFrom_conf( s, args 
      , (hw_byte[]){hw_TypeID_array, hw_TypeID_uint}, 2);
    arr = args[0];

    for (int i = 0; i < argc; i++) {
        hw_Var string;

        args[1].as_ptr = argv[i];
        args[2].as_uint = strlen(argv[i]);
        hw_String_newFrom_data(s, args
            , (hw_byte[]){
            hw_TypeID_nil, 
            hw_TypeID_ptr, 
            hw_TypeID_uint}, 3);

        string = args[0];
      
        args[0] = arr;
        args[1] = string;
        hw_VarArr_push(s, args, (hw_byte[]){
              hw_TypeID_array,
              hw_TypeID_string}, 2);

        arr = args[0];
    }
    
    return arr.as_arr;
}


static void _static_checks(void)
{
    #define X(exp) HW_STATIC_ASSERT(exp);

    // Sanity
    X(1);
    X(!0);
    X(!!10);

    // Type Size
    #define WORD_SIZE (8)
    X(sizeof(hw_float) == WORD_SIZE);
    X(sizeof(hw_int) == WORD_SIZE);
    X(sizeof(hw_uint) == WORD_SIZE);
    X(sizeof(hw_code) == WORD_SIZE);
}

#if 0
static hw_Module* test_module(hw_State *hw)
{
    char const source[] = "nosource";
    hw_uint source_size = sizeof(source);
    hw_CompilerBC *comp = hw_compbc_new(
            hw, source, source_size);

    hw_compbc_w_defn(comp, (void *)"add_loop", 8
        , 5, 1, (hw_byte[]){  hw_TypeID_int
                            , hw_TypeID_int
                            , hw_TypeID_int
                            , hw_TypeID_int
                            , hw_TypeID_int
                        }
        , (hw_CStr[]){ {(void *)"res", 3}
                     , {(void *)"x", 1} 
                     , {(void *)"y", 1} 
                     , {(void *)"step", 4} 
                     , {(void *)"unused", 6}
        });
    
    hw_uint v_result = 0
      ,     v_initial = 1
      ,     v_step    = 2
      ,     v_iter    = 3;
    hw_compbc_inst(comp, hw_code_abc(hw_Inst_dup, v_result, v_initial, 0));

    hw_int lable_repeat = hw_compbc_inst(comp
        , hw_code_abc(hw_Inst_i_add, v_result, v_result, v_step));
    hw_compbc_inst(comp, hw_code_abc(hw_Inst_i_sub, v_iter, v_iter, v_initial)); 
    hw_compbc_inst(comp, hw_code_as32(hw_Inst_jtk, v_iter
          , lable_repeat - comp->obj->code->lenUsed - 2));
    
    hw_compbc_w_endfn(comp);

    hw_Module *mod = hw_compbc_convert(comp);
    hw_compbc_delete(comp);
    return mod; 
}

void test_main(hw_State *hw, hw_VarArr *args)
{
    hw_Module *mod = test_module(hw);
    hw_debug_Module_disasm(hw, mod);

    hw_uint mod_id = hw_Global_add_module((void *)hw->global, mod);

    hw_VarP v = {.value.as_int = 0, .type = hw_TypeID_int};
    hw_State_vstack_push(hw, v.value, v.type); v.value.as_int = 1;
    hw_State_vstack_push(hw, v.value, v.type); v.value.as_int = 1;
    hw_State_vstack_push(hw, v.value, v.type); v.value.as_int = 10;
    hw_State_vstack_push(hw, v.value, v.type);
    hw_State_vstack_push(hw, v.value, v.type);
    
    hw_vm_prepare_call(hw, mod_id, 0);
    hw_vm(hw);

    hw_Var _vt_args[] = { (hw_Var){.as_arr = args} };
    hw_byte tid = hw_TypeID_array;
    hw_VarArr_delete(hw, _vt_args, &tid, 1);
}

#endif

hw_CStr hw_get_token_name(hw_uint token_type);

void print_tokens(hw_Lexer lexer)
{
    while(lexer.token.type != HW_LEXTOKEN_END_OF_SOURCE) {
        hw_Lexer_next(&lexer);
        hw_CStr name = hw_get_token_name(lexer.token.type);
        printf("%p %p %lu - %.*s | %.*s", (void *)lexer.at, (void *)lexer.end
            , lexer.end - lexer.at
            , (int)lexer.token.size, lexer.token.start
            , (int)name.len, name.data);

        HW_ASSERT(lexer.token.type != HW_LEXTOKEN_ERROR);
    }
}



int main(int argc, char *argv[])
{
    HW_ASSERTEX(argc > 1, "`%s` requires one argument, \nUsage: %s <file>", argv[0], argv[0]);
    hw_State *hw = hw_State_new_default(NULL);
    _static_checks();
    _check_vm_inst(hw);
    
    hw_CompilerBC *comp = hw_compbc_new(hw);
    HW_ASSERT(hw_compbc_load_source_fromFile(comp, argv[1], strlen(argv[1]))
             && "FILE NOT LOADED");

    hw_compbc_compile_from_source(comp);
    hw_Module *mod = hw_compbc_convert(comp);
    hw_compbc_delete(comp);

    if(argc > 2) {
        hw_debug_Module_disasm(hw, mod);
    }

    hw_uint mod_id = hw_Global_add_module((void *)hw->global, mod);
    hw_uint fn_id = 1;
    HW_ASSERT(hw_Module_get_fn(mod, (void *)"main", 4, &fn_id));

    hw_vm_prepare_call(hw, mod_id, fn_id);
    hw_vm(hw);

    hw_State_delete(hw);
    return EXIT_SUCCESS;
}

/**
int main_memory(int argc, char *argv[])
{
    hw_State *hw = hw_State_new_default(NULL);
    _check_vm_inst(hw);
    hw_CompilerBC *comp = hw_compbc_new(hw);
    hw_compbc_load_source_fromData(comp, "; no source", 11);

    hw_compbc_w_defn(comp, (void *)"fn1", 3, 0, 0, NULL, NULL);
    hw_debug_print_fnobj(comp); 
    hw_compbc_w_endfn(comp);

    hw_compbc_w_defn(comp, (void *)"fn2", 3, 0, 0, NULL, NULL);
    hw_compbc_deflocalvar(comp, (void *)"x", 1, (hw_VarInfo){.type = hw_TypeID_int});
    hw_debug_print_fnobj(comp); 
    hw_compbc_w_endfn(comp);

    hw_debug_print_mobj(comp); 

    hw_Module *mod = hw_compbc_convert(comp);
        
    hw_compbc_delete(comp);

    hw_debug_Module_disasm(hw, mod);
    hw_State_delete(hw);
    
    return EXIT_SUCCESS;
}
**/
