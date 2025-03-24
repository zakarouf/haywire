#include "def.h"
#include "dev.h"
#include "cstd.h"
#include "hwfn.h"
#include <stdatomic.h>
#include <stdio.h>

void hw_debug_print_inst(hw_State *hw)
{
    hw_InstInfo const *instinfo = hw->global->insts;
    hw_u32 const inst_count = hw->global->insts_count;

    hw_loglnp("Instructions: Total(%"PRIu32")", inst_count); 
    for (size_t i = 0; i < inst_count; i++) {
        hw_loglnp("  %02"PRIx64" %.*s\n"
                  "     (%.*s)"
                , i, instinfo->name_size, instinfo->name
                , instinfo->brief_size, instinfo->brief);
        instinfo += 1;
    }
}

void hw_debug_print_symtable_ord(hw_State *hw, hw_SymTableOrd *table)
{
    fprintf(stdout, "symtable:\n"
                    "  len:        %"PRIu32 "\n" 
                    "  lenUsed:    %"PRIu32 "\n"
                    "  vlen:       %"PRIu32 "\n"
                    "  vlenUsed:   %"PRIu32 "\n",
                    table->len,
                    table->lenUsed,
                    table->vlen,
                    table->vlenUsed);
    for (hw_u32 i = 0; i < table->vlenUsed; i++) {
        hw_Type *T = hw_TypeSys_get_via_id(hw->ts, table->valT[i]);
        fprintf(stdout, "    [%u] = \"%.*s\":%.*s\n", 
                i, table->key_size[i], table->keys[i]
                , T->name_size, T->name);
    }
    fprintf(stdout, "INDICES::");
    for (hw_u32 i = 0; i < table->len; i++) {
        fprintf(stdout, " - [%u] = %i\n", i, table->indices[i]);
    }
}

void hw_debug_print_mobj(hw_CompilerBC const *comp)
{
    hw_ModuleObj *mo = comp->mobj;

    hw_loglnp(
        "ModuleObj:\n"
        "   inst: %"PRIu64"\n"
        "   code: %"PRIu64"\n"
      , mo->code->lenUsed
      , mo->data->lenUsed
    );

    for (size_t i = 0; i < mo->fnpt->lenUsed; i++) {
        hw_loglnp("  fn pc %"PRIu64 " -> %"PRIu64, i, mo->fnpt->data[i]);
    }

    for (size_t i = 0; i < mo->code->lenUsed; i++) {
        hw_debug_code_disasm(comp->vm_child, mo->code->data[i]);
        hw_logp("%s", "\n");
    }
}

void hw_debug_print_fnobj(hw_CompilerBC const *comp)
{
    hw_FnObj *fnobj = comp->fnobj;
    hw_loglnp(
        "FnObject: \n"
        "   %.*s (%"PRIu64")\n"

        "   id:%"PRIu64 "\n"
        "   pc:%"PRIu64 "\n"
        "   args:%"PRIu64 "\n"

        "   muts:%"PRIu64 "\n"
        "   locked:%s\n"
      , (int)fnobj->name_sizeUsed, fnobj->name, fnobj->name_hash

      , fnobj->current_fn
      , fnobj->defn_pc
      , fnobj->args_passed

      , fnobj->mut_count
      , fnobj->lock? "true": "false"
    );
}

void hw_debug_code_disasm(hw_State const *hw, hw_code code)
{
    FILE *out = hw->stdout;

    hw_uint opcode = code.get.opcode;
    HW_DEBUG(HW_ASSERT(opcode < hw->global->insts_count));
    hw_InstInfo const *insdata = hw->global->insts + opcode;

    fwrite("    ", 4, 1, out);
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

void hw_debug_print_var(hw_State *hw, hw_Var v, hw_byte t)
{
    hw_Var argv[2];
    hw_byte type[2];
    
    hwfn_String_new(hw, argv, type, 1);
    argv[1] = argv[0];
    argv[0] = v;
    type[0] = t;
    
    hw_Type *T = hw_TypeSys_get_via_id(hw->ts, t);
    HW_ASSERT(T);
    hw_VarFn to_string = hw_Type_getvt(T, "to_string", 9);
    if(to_string) {
        to_string(hw, argv, type, 2);
        hw_String *str = argv[1].as_string;
        hw_logp("%.*s", str->lenUsed, str->data);
    } else {
        hw_logp("(NILTYPE)");
    }
    hwfn_String_delete(hw, argv+1, type+1, 1);
}

static void _mod_print_constant(hw_State *hw, hw_Module const *m)
{
    for (size_t i = 0; i < m->k_count; i++) {
        hw_Type *T = hw_TypeSys_get_via_id(hw->ts, m->knst_t[i]);
        HW_ASSERT(T != NULL);
        fprintf(hw->stdout, "@const _knst_%"PRIu64 ":%.*s "
                , i, T->name_size, T->name);
        hw_debug_print_var(hw, m->knst[i], m->knst_t[i]);
        fprintf(hw->stdout, "\n");
    }
}

void hw_debug_Module_fn_disasm(hw_State *hw, const hw_Module *m, hw_uint fn)
{
    hw_FnInfo info;
    hw_Module_get_FnInfo(m, fn, &info);
    fprintf(hw->stdout
            , "\n"
              ";; function: '%.*s' (id:%"PRIu64", %"PRIu64")\n"
              ";; args count: %"PRIu64"\n"
              ";; muts count: %"PRIu64"\n"
              ";; stack size: %"PRIu64"\n"
              ";; types: (at)%"PRIu64"\n"
              ";; data : (at)%"PRIu32"\n"
            , (int)info.name_size, info.name
            , fn, info.name_size
            , info.arg_count
            , info.mut_count
            , info.stack_sz
            , info.types - m->data
            , m->code[m->fnpt[fn]].getx.x32);
    
    fprintf(hw->stdout, "@defn %.*s (", (int)info.name_size, info.name);
    for (size_t i = 0; i < info.arg_count; i++) {
        hw_Type *T = hw_TypeSys_get_via_id(hw->ts, info.types[i]);
        HW_ASSERTEX(T, "%d", info.types[i]);
        if(i == info.mut_count) { fprintf(hw->stdout, "|"); }

        fprintf(hw->stdout, " _%"PRIu64":%.*s,"
                , i, (int)T->name_size, T->name);
    }
    fprintf(hw->stdout, "  ) ; total(%"PRIu64") mut (%"PRIu64")",
            info.arg_count, info.mut_count);
}


void hw_debug_Module_disasm(hw_State *hw, hw_Module const *m)
{
    fprintf(stdout
        , ";; Module Disassamble\n"
          ";; instructions %"PRIu64"\n"
          ";; data %"PRIu64" bytes\n"
          ";; functions %"PRIu64"\n"
        , m->code_len
        , m->data_size
        , m->fn_count);

    for (size_t i = 0; i < m->fn_count; i++) {
        fprintf(stdout, ";   defn `%"PRIu64"`  -> %"PRIu64 "\n"
            , i, m->fnpt[i]);
    }

    fprintf(stdout
        , "; constants %"PRIu64"\n"
        , m->k_count);
    
    _mod_print_constant(hw, m);

    hw_uint fn = 0;
    for (size_t i = 0; i < m->code_len; i++) {
        if(m->code[i].get.opcode == hw_Inst_defn) {
            if(m->code[i].getx.x32 < m->data_size) {
                hw_debug_Module_fn_disasm(hw, m, fn);
                fn += 1;
            } else {
                fprintf(hw->stdout, "defn %"PRIu32" ; more than data%"PRIu64
                        , m->code[i].getx.x32, m->data_size);
                fn += 1;
            }
        } else {
            hw_debug_code_disasm(hw, m->code[i]);
            fprintf(hw->stdout, "; %"PRIu64, i);
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
    HW_LOG("Stack Usage: %"PRIu32, hw->vstack->lenUsed);

    hw_uint vtrace_count = 10;
    _debug_State_trace_print_last_values(hw, vtrace_count);
    
    hw_FnState *f = hw_State_fstack_top(hw);
    HW_LOG("Top Stack Start: %"PRIu32, f->var);

    HW_ASSERT(hw->global->symbols->valT[f->mod] == hw_TypeID_module);
    hw_Module const *m = hw_Global_get_symb_via_id(hw->global, f->mod)
                                .as_module;
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
    (void)v;
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
