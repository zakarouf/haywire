#include "hw.h"
#include "hw_dev.h"
#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>

void hw_debug_code_disasm(hw_State const *hw, hw_code code)
{
    FILE *out = hw->stdout;

    hw_uint opcode = code.get.opcode;
    HW_DEBUG(HW_ASSERT(opcode < hw->global->insts_count));
    struct hw_InstData const *insdata = hw->global->insts + opcode;

    fwrite(insdata->name, insdata->name_size, 1, out);
    fputc(' ', out);
    
    switch (insdata->inst_type) {
       break; case hw_InstType_nop:
           fprintf(out, "");
       break; case hw_InstType_a:
           fprintf(out, "%hu                ; a", code.get.A);
       break; case hw_InstType_ab:
           fprintf(out, "%hu %hu            ; a b", code.get.A, code.get.B);
       break; case hw_InstType_abc:
           fprintf(out, "%hu %hu %hu        ; a b c"
                   , code.get.A, code.get.B, code.get.C);
       break; case hw_InstType_ax32:
           fprintf(out, "%hu %u             ; a x32 | s32=%i", code.get.A
                   , code.getx.x32, code.gets.s32);
       break; case hw_InstType_x32:
           fprintf(out, "%u                 ; x32 | s32=%i"
                   , code.getx.x32, code.gets.s32);
       break; case hw_InstType_as32:
           fprintf(out, "%hu %i             ; a s32 | x32=%u", code.get.A
                   , code.gets.s32, code.getx.x32);
       break; case hw_InstType_s32:
           fprintf(out, "%i                 ; s32 | x32=%u"
                   , code.gets.s32, code.getx.x32);
    }    
}

void hw_debug_Module_fn_disasm(hw_State *hw, const hw_Module *m, hw_uint fn)
{
    hw_FnInfo info;
    hw_Module_get_FnInfo(m, fn, &info);
    fprintf(hw->stdout
            , "\n"
              ";; function: %.*s\n"
              ";; args count: %"PRIu64"\n"
              ";; muts count: %"PRIu64"\n"
              ";; stack size: %"PRIu64"\n"
              ";; types: (at)%"PRIu64"\n"
            , (int)info.name_size, info.name
            , info.arg_count
            , info.mut_count
            , info.stack_sz
            , info.types - m->data);

    fprintf(hw->stdout, "defn %.*s {\n", (int)info.name_size, info.name);
    for (size_t i = 0; i < info.arg_count; i++) {
        hw_Type *T = hw_TypeSys_get_via_id(hw->ts, info.types[i]);
        HW_ASSERTEX(T, "%d", info.types[i]);

        fprintf(hw->stdout, "        _%"PRIu64":%.*s,\n"
                , i, (int)T->name_size, T->name);
    }
    fprintf(hw->stdout, "  } %"PRIu64, info.mut_count);
}


void hw_debug_Module_disasm(hw_State *hw, hw_Module const *m)
{
    fprintf(stdout
        , "; Module Disassamble\n"
          "; instructions %"PRIu64"\n"
          "; data %"PRIu64" bytes\n"
          "; functions %"PRIu64"\n"
          "; constants %"PRIu64"\n"
        , m->code_len
        , m->data_size
        , m->fn_count
        , m->k_count);

    hw_uint fn = 0;
    for (size_t i = 0; i < m->code_len; i++) {
        if(m->code[i].get.opcode == hw_Inst_defn) {
            hw_debug_Module_fn_disasm(hw, m, fn);
            fn += 1;
        } else {
            hw_debug_code_disasm(hw, m->code[i]);
            printf("; %"PRIu64, i);
        }
        fputc('\n', stdout);
    }
}

static void _debug_State_trace_print_last_values(hw_State *hw, hw_uint count)
{
    count = HW_MIN(count, hw->vstack->lenUsed);
    HW_LOG("Last `%"PRIu64"` Stack Values", count);
    for (size_t i = 1; i <= count; i++) {
        hw_uint const index = hw->vstack->lenUsed - i;
        HW_LOG("    [%"PRIu64"]:%"PRIu8" -> %"PRIu64
                         , index, hw->vstack->tid[index]
                         , hw->vstack->data[index].as_uint );
    }
}

void hw_debug_State_trace(hw_State *hw)
{
    HW_ASSERT(hw->fstack->lenUsed);
    HW_LOG("===== Stack Trace =====%s", "");
    HW_LOG("Function Depth: %"PRIu64, hw->fstack->lenUsed);
    HW_LOG("Stack Usage: %"PRIu64, hw->vstack->lenUsed);

    hw_uint vtrace_count = 10;
    _debug_State_trace_print_last_values(hw, vtrace_count);
    
    hw_FnState *f = hw_State_fstack_top(hw);
    HW_LOG("Top Stack Start: %"PRIu64, f->var);

    hw_Module const *m = hw_Global_get_module(hw->global, f->mod);
    hw_FnInfo finfo;
    hw_Module_get_FnInfo(m, f->fn, &finfo);

    HW_LOG("Function Name: %.*s (%d)", (int)finfo.name_size, finfo.name, (int)finfo.name_size);
    HW_LOG("         Args: %"PRIu64, finfo.arg_count);
    HW_LOG("         Muts: %"PRIu64, finfo.mut_count);
    HW_LOG("        Stack: %"PRIu64, finfo.stack_sz);
    
}


enum {
    _vm_step_input_continue = 'c'
  , _vm_step_input_stacktrace = 's'
  , _vm_step_input_variable_view = 'v'
  , _vm_step_input_code_disasm = 'd'
  , _vm_step_input_mod_disasm = 'm'
};

void hw_debug_vm_step(
    hw_State *hw, hw_Module const *m, hw_code const *pc, hw_Var const *v)
{
    _L_repeat: {}
    hw_uint input = getchar();

    #define input(x) break; case _vm_step_input_##x:

    switch (input) {
        input(continue)     goto _L_end;
        input(mod_disasm)   hw_debug_Module_disasm(hw, m);
        input(code_disasm)  hw_debug_code_disasm(hw, *pc);
        input(stacktrace)   hw_debug_State_trace(hw);
        default:break;
    }
    goto _L_repeat;
    _L_end:
    return ;
}
