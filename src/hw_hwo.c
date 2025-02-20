#include "hw.h"
#include "hw_dev.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdarg.h>
#include <inttypes.h>

hw_CStr hw_get_token_name(hw_uint token_type);

static void hw_compbc_ERROR(hw_CompilerBC *comp, const char *restrict fmt, ...)
__attribute__ ((format (printf, 2, 3)));

static void hw_compbc_ERROR(hw_CompilerBC *comp, const char *restrict fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    vfprintf(comp->vm_child->stdout, fmt, args);
    fputc('\n', comp->vm_child->stdout);
    va_end(args);

    hw_loglnp("file: %.*s:"
        , (int)comp->sname.len, comp->sname.data);

    
    int line_part1_size = comp->lexer.token.start 
                                - comp->lexer.token.line_start;
    
    int line_part2_size = HW_MIN(8, comp->lexer.end - (comp->lexer.token.start + comp->lexer.token.size));
    
    hw_loglnp("%"PRIu64":%"PRIu64"| "
                "%.*s ~~%.*s~~ %.*s\n"
//                "%d''%s ~~%d''%s~~ %d''%s\n"
            , comp->lexer.token.line, comp->lexer.token.column
            , line_part1_size, comp->lexer.token.line_start
            , (int)comp->lexer.token.size, comp->lexer.token.start
            , line_part2_size, comp->lexer.token.start + comp->lexer.token.size);
        

        HW_DEBUG(
          //, (int)comp->lexer.token.size, comp->lexer.token.start
         //   , (int)token_name.len, token_name.data);
        )

    
    exit(-1);
}

#define _ERROR(fmt, ...)\
    hw_compbc_ERROR(comp, "%d|" fmt, __LINE__, __VA_ARGS__)

static void hw_FnObj_new(hw_CompilerBC *comp)
{
    hw_FnObj *fnobj = HW_THREAD_ALLOC(comp->vm_child, sizeof(*fnobj));

    memset(fnobj, 0, sizeof(*fnobj));
    HW_ARR_NEW(comp->vm_child, fnobj->var_infos, 8);
    HW_ARR_NEW(comp->vm_child, fnobj->defer_lable_names, 16);
    HW_ARR_NEW(comp->vm_child, fnobj->defer_lables, 16);

    hw_VarP symtable;

    hw_SymTable_new(comp->vm_child, &symtable.value, &symtable.type, 1);
    fnobj->vartable = symtable.value.as_symtable;
    HW_DEBUG(HW_LOG("vartable %p", (void *)fnobj->vartable));

    hw_SymTable_new(comp->vm_child, &symtable.value, &symtable.type, 1);
    fnobj->lables = symtable.value.as_symtable;
    HW_DEBUG(HW_LOG("lables %p", (void *)fnobj->lables));

    fnobj->name_size = 32;
    fnobj->name = HW_THREAD_ALLOC(comp->vm_child, fnobj->name_size);

    comp->fnobj = fnobj;
}

static void hw_FnObj_delete(hw_CompilerBC *comp)
{
    hw_FnObj *fnobj = comp->fnobj;
    comp->fnobj = NULL;

    for (size_t i = 0; i < fnobj->var_infos->lenUsed; i++) {
        struct hw_VarInfo *vinf = fnobj->var_infos->data + i;
        (void)vinf;
    }
    HW_ARR_DELETE(comp->vm_child, fnobj->var_infos);

    hw_VarP symtable = {.value.as_symtable = fnobj->lables, .type = hw_TypeID_symtable};
    hw_SymTable_delete(comp->vm_child, &symtable.value, &symtable.type, 1);
    symtable.value.as_symtable = fnobj->vartable;
    hw_SymTable_delete(comp->vm_child, &symtable.value, &symtable.type, 1);


    HW_ARR_DELETE(comp->vm_child, fnobj->defer_lable_names);
    HW_ARR_DELETE(comp->vm_child, fnobj->defer_lables);
    HW_THREAD_FREE(comp->vm_child, fnobj->name);
    HW_THREAD_FREE(comp->vm_child, fnobj);
}

static void hw_FnObj_start(hw_CompilerBC *comp, hw_byte const *name, hw_uint name_size)
{
    hw_FnObj *fnobj = comp->fnobj;
    fnobj->current_fn += 1;
    fnobj->lock = 1;

    hw_VarP symtable = {.value.as_symtable = fnobj->lables, .type = hw_TypeID_symtable};
    hw_SymTable_reset(comp->vm_child, &symtable.value, &symtable.type, 1);
    symtable.value.as_symtable = fnobj->vartable;
    hw_SymTable_reset(comp->vm_child, &symtable.value, &symtable.type, 1);

    fnobj->var_infos->lenUsed = 0;

    if(name_size > fnobj->name_size) {
        fnobj->name = HW_THREAD_REALLOC(
                comp->vm_child, fnobj->name, name_size + 1);
        fnobj->name_size = name_size + 1;
    }
    
    fnobj->name_sizeUsed = name_size;
    memcpy(fnobj->name, name, name_size);
}

static void hw_ModuleObj_new(hw_CompilerBC *comp)
{
    hw_ModuleObj *obj = HW_THREAD_ALLOC(comp->vm_child, sizeof(*obj));
    memset(obj, 0, sizeof(*obj));
    HW_ARR_NEW(comp->vm_child, obj->fnpt, 16);
    HW_ARR_NEW(comp->vm_child, obj->knst, 8); 
    HW_ARR_NEW(comp->vm_child, obj->knst_t, 8);
    HW_ARR_NEW(comp->vm_child, obj->data, 128);

    HW_ARR_NEW(comp->vm_child, obj->code, 80);
    HW_ARR_NEW(comp->vm_child, obj->defer_fncall_pc, 16);
    HW_ARR_NEW(comp->vm_child, obj->defer_fncall_names, 16);

    hw_Var arg;
    hw_byte tid = hw_TypeID_symtable;
    hw_SymTable_new(comp->vm_child, &arg, &tid, 1);
    obj->fntable = arg.as_symtable;

    comp->obj = obj;
}

static void hw_ModuleObj_delete(hw_CompilerBC *comp)
{
    HW_ARR_DELETE(comp->vm_child, comp->obj->defer_fncall_names);
    HW_ARR_DELETE(comp->vm_child, comp->obj->defer_fncall_pc);

    HW_ARR_DELETE(comp->vm_child, comp->obj->knst_t);
    HW_ARR_DELETE(comp->vm_child, comp->obj->knst);
    HW_ARR_DELETE(comp->vm_child, comp->obj->code);
    HW_ARR_DELETE(comp->vm_child, comp->obj->fnpt);

    HW_ARR_DELETE(comp->vm_child, comp->obj->data);

    hw_Var arg = { .as_symtable = comp->obj->fntable };
    hw_byte tid = hw_TypeID_symtable;
    hw_SymTable_delete(comp->vm_child, &arg, &tid, 1);

    HW_THREAD_FREE(comp->vm_child, comp->obj);
}

hw_bool hw_compbc_load_source_fromFile(hw_CompilerBC *comp 
        , char const *source_name, hw_uint source_name_size)
{
    HW_ASSERT(source_name_size);
    HW_ASSERT(source_name);

    hw_uint source_size = 0;
    void *source = hw_loadfile(comp->vm_child, source_name, 1, &source_size);
    HW_ASSERT(source_size < UINT32_MAX); // 4GB is Max File Size;
                                         //
    if(source == NULL) return 0;

    comp->sname.len = source_name_size;
    comp->sname.data = HW_THREAD_ALLOC(comp->vm_child, source_name_size);
    memcpy(comp->sname.data, source_name, source_name_size);

    comp->source.len = source_size;
    comp->source.data = source;
    
    hw_Lexer_start(&comp->lexer, comp->source.data, comp->source.len);
    return 1;
}

void hw_compbc_load_source_fromData(
        hw_CompilerBC *comp, void const *source, hw_uint size)
{
    comp->sname.len = sizeof("<memory>");
    comp->sname.data = HW_THREAD_ALLOC(comp->vm_child, comp->sname.len);
    memcpy(comp->sname.data, "<memory>", sizeof("<memory>"));

    comp->source.len = size;
    comp->source.data = HW_THREAD_ALLOC(comp->vm_child, comp->source.len);
    memcpy(comp->source.data, source, size);

    hw_Lexer_start(&comp->lexer, comp->source.data, comp->source.len);
}

hw_CompilerBC *hw_compbc_new(hw_State *parent)
{
    hw_State *s = hw_State_new_default(parent);
    hw_CompilerBC *comp = HW_THREAD_ALLOC(s, sizeof(*comp));
    memset(comp, 0, sizeof(*comp));
    comp->vm_parent = parent;
    comp->vm_child = s;

    hw_ModuleObj_new(comp);
    hw_FnObj_new(comp);

    return comp;
}

void hw_compbc_delete(hw_CompilerBC *comp)
{
    hw_FnObj_delete(comp);
    hw_ModuleObj_delete(comp);

    HW_THREAD_FREE(comp->vm_child, comp->sname.data);
    HW_THREAD_FREE(comp->vm_child, comp->source.data);

    hw_State *child = comp->vm_child;
    HW_THREAD_FREE(comp->vm_child, comp);
    hw_State_delete(child);
}

hw_Module* hw_compbc_convert(hw_CompilerBC *comp)
{
    hw_ModuleObj *obj = comp->obj;
    hw_uint const module_size = sizeof(hw_Module)
                + (sizeof(*(obj->data->data)) * obj->data->lenUsed)
                + (sizeof(*(obj->fnpt->data)) * obj->fnpt->lenUsed)
                + (sizeof(*(obj->code->data)) * obj->code->lenUsed)
                + (sizeof(*(obj->knst->data)) * obj->knst->lenUsed)
                + (sizeof(*(obj->knst_t->data)* obj->knst->lenUsed));

    hw_Module *mod = HW_THREAD_ALLOC(comp->vm_parent, module_size);
    
    mod->code_len = obj->code->lenUsed;
    mod->data_size = obj->data->lenUsed;
    mod->k_count = obj->knst->lenUsed;
    mod->fn_count = obj->fnpt->lenUsed;
    
    mod->data =     HW_CAST(void *, mod + 1);
    mod->fnpt =     HW_CAST(void *, mod->data + mod->data_size);
    mod->code =     HW_CAST(void *, mod->fnpt + mod->fn_count);
    mod->knst =     HW_CAST(void *, mod->code + mod->code_len);
    mod->knst_t =   HW_CAST(void *, mod->knst + mod->k_count);

    memcpy(mod->data, obj->data->data, obj->data->lenUsed);

    memcpy(mod->fnpt, obj->fnpt->data
            , mod->fn_count * sizeof(*mod->fnpt));

    memcpy(mod->code, obj->code->data
            , obj->code->lenUsed * sizeof(*mod->code));

    memcpy(mod->knst_t, obj->knst_t->data
            , obj->knst_t->lenUsed * sizeof(*mod->knst_t));
    
    for (size_t i = 0; i < mod->k_count; i++) {
        mod->knst[i] = obj->knst->data[i];
        hw_Type     *T = comp->vm_parent->ts->types + mod->knst_t[i];
        if(T->is_obj) {
            hw_VarFn    copy = hw_Type_getvt(T, "newFrom_copy", 4*3);
            hw_Var      args[2] = {mod->knst[i], obj->knst->data[i]};
            hw_byte     tids[2] = {mod->knst_t[i], mod->knst_t[i]};
            copy(comp->vm_parent, args, tids, 2);
            mod->knst[i] = args[0];
        }
    }

    return mod;
}

hw_uint hw_compbc_inst(hw_CompilerBC *comp, hw_code const inst)
{
    HW_ARR_PUSH(comp->vm_child, comp->obj->code, inst);
    return comp->obj->code->lenUsed-1;
}

hw_uint hw_compbc_data(hw_CompilerBC *comp, void const *data, hw_uint const size)
{
    hw_uint const index = comp->obj->data->lenUsed;
    HW_ARR_PUSHSTREAM(comp->vm_child, comp->obj->data, data, size);
    HW_DEBUG(HW_LOG("DATA INDEX %"PRIu64 "\n", index));
    return index;
}

hw_uint hw_compbc_knst(hw_CompilerBC *comp, hw_Var const value, hw_byte const tid)
{
    HW_ARR_PUSH(comp->vm_child, comp->obj->knst_t, tid);
    
    HW_DEBUG(HW_ASSERT(tid < hw_TypeID_TOTAL));
    hw_Type *T = comp->vm_child->ts->types + tid;

    hw_Var copy_val = value;
    if(T->is_obj) {
        hw_VarFn copy = hw_Type_getvt(T, "newFrom_copy", 12);
        hw_Var args[2] = {value, value};
        hw_byte tids[2] = {tid, tid};
        copy(comp->vm_child, args, tids, 2);
        copy_val = args[0];
    }

    HW_ARR_PUSH(comp->vm_child, comp->obj->knst, copy_val);

    return comp->obj->knst->lenUsed-1;
}

int hw_compbc_deflocalvar(hw_CompilerBC *comp
        , hw_byte const *var_name, hw_uint name_size, hw_VarInfo vinf)
{
    hw_FnObj *fnobj = comp->fnobj;
    HW_ASSERTEX(fnobj->vartable->key[
        hw_SymTable_index(fnobj->vartable, var_name, name_size)] == NULL,
        "Variable :%.*s already set", (int)name_size, var_name);

    HW_ARR_PUSH(comp->vm_child, fnobj->var_infos, vinf);
    hw_SymTable_set__wrap(
        comp->vm_child, &fnobj->vartable, var_name, name_size
      , (hw_Var){.as_uint = fnobj->var_infos->lenUsed-1}, hw_TypeID_uint);
    return HW_TRUE;
}

hw_uint hw_compbc_w_defn(
    hw_CompilerBC *comp, const hw_byte *name, const hw_uint name_size
  , const hw_uint total_arg, const hw_uint mut_count, const hw_byte *tids
  , hw_CStr const *arg_names)
{
    HW_ASSERT(comp->fnobj->lock == 0);
    hw_ModuleObj *obj = comp->obj;
    hw_FnObj_start(comp, name, name_size);
    hw_FnObj *fnobj = comp->fnobj;

    HW_ASSERTEX(
        !obj->fntable->key[hw_SymTable_index(obj->fntable, name, name_size)],
        "%.*s function already exist", (int)name_size, name);

    hw_SymTable_set__wrap(comp->vm_child, &obj->fntable, name, name_size
            , (hw_Var){.as_uint = obj->fnpt->lenUsed}, hw_TypeID_uint);

    fnobj->defn_pc = obj->code->lenUsed;
    HW_ARR_PUSH(comp->vm_child, obj->fnpt, obj->code->lenUsed);

    fnobj->args_passed = total_arg;
    fnobj->mut_count = mut_count;

    hw_compbc_inst(comp, hw_code_ax32(hw_Inst_defn, total_arg, UINT32_MAX));
    for (size_t i = 0; i < total_arg; i++) {
        hw_compbc_deflocalvar(comp, arg_names[i].data, arg_names[i].len
            , (hw_VarInfo){ 
                    .is_mut = i < mut_count? 1:0
                  , .type = tids[i]
        });
    }
    
    return obj->fnpt->lenUsed - 1;
}

void hw_compbc_w_endfn(hw_CompilerBC *comp)
{
    hw_FnObj *fnobj = comp->fnobj;
    HW_ASSERT(fnobj->lock);
    comp->fnobj->lock = 0;
    HW_ASSERT(fnobj->var_infos->lenUsed == fnobj->vartable->lenUsed);
     
    hw_uint const fninfo_index = 
    hw_compbc_data(comp, (hw_uint[]){fnobj->name_sizeUsed}, sizeof(hw_uint));
    hw_compbc_data(comp, (hw_uint[]){fnobj->mut_count}, sizeof(hw_uint));
    hw_compbc_data(comp, (hw_uint[]){fnobj->args_passed}, sizeof(hw_uint));
    hw_compbc_data(comp, (hw_uint[]){fnobj->vartable->lenUsed}, sizeof(hw_uint));
    hw_compbc_data(comp, fnobj->name, fnobj->name_sizeUsed);

    for (size_t i = 0; i < fnobj->var_infos->lenUsed; i++) {
        hw_byte typeid = fnobj->var_infos->data[i].type;
        HW_DEBUG(HW_ASSERT(
            hw_TypeSys_get_via_id(comp->vm_child->ts, typeid)));
        hw_compbc_data(comp, &typeid, 1);
    }

    comp->obj->code->data[fnobj->defn_pc].getx.x32 = fninfo_index;
    hw_compbc_inst(comp, hw_code_as32(hw_Inst_return, 0, 0));
}

hw_uint hw_compbc_w_lable(
    hw_CompilerBC *comp, hw_byte const *name, hw_uint name_size)
{
    hw_FnObj *fnobj = comp->fnobj;
    hw_SymTable_set__wrap(
        comp->vm_child, &fnobj->lables
      , name, name_size
      , (hw_Var){ .as_uint = comp->obj->code->lenUsed }
      , hw_TypeID_uint);
    return comp->obj->code->lenUsed;
}

void hw_compbc_defer_fncall(hw_CompilerBC *comp, hw_LexToken name, hw_uint inst)
{
    hw_ModuleObj *obj = comp->obj;
    HW_ARR_PUSH(comp->vm_child, obj->defer_fncall_names, name);
    HW_ARR_PUSH(comp->vm_child, obj->defer_fncall_pc, inst);
}


/**
 * Compiler
 */

void hw_compbc_lex_next(hw_CompilerBC *comp) {
    hw_Lexer_next(&comp->lexer);
}

void hw_compbc_lex_next_expect(hw_CompilerBC *comp, enum hw_LexTokenType ttype)
{
    if(!hw_Lexer_next_expect(&comp->lexer, ttype)) {
        hw_CStr const tname = hw_get_token_name(ttype);
        _ERROR("Expected %.*s, got: %.*s"
                , (int)tname.len, tname.data
                , (int)comp->lexer.token.size, comp->lexer.token.start );
    }
}

#define HW_COMP_ASSERT(exp) (exp? (void)0: _ERROR(""))

void hw_compbc_lex_next_skipws(hw_CompilerBC *comp)
{
    hw_Lexer_next_skipws(&comp->lexer);
}

void hw_compbc_lex_next_skipws_expect(hw_CompilerBC *comp, enum hw_LexTokenType ttype)
{
    hw_Lexer_next_skipws(&comp->lexer);
    if(hw_LexToken_is_not(comp->lexer.token, ttype)) {
        hw_CStr const tname = hw_get_token_name(ttype);
        _ERROR("Expected %.*s, got: %.*s"
                , (int)tname.len, tname.data
                , (int)comp->lexer.token.size, comp->lexer.token.start );
    }
}

hw_int hw_compbc_lex_getint(hw_CompilerBC *comp)
{
    hw_Lexer *lex = &comp->lexer;
     
    #define buffsize 64
    hw_byte buffer[buffsize] = {[buffsize - 1] = '\0'};
    hw_byte *buffer_end = NULL;

    if(lex->token.size > (buffsize-1)) {
        _ERROR("Number Too big%s", "");
    }

    memcpy(buffer, lex->token.start, lex->token.size);

    hw_int val = strtold((void *) buffer, (void *) &buffer_end);
    HW_ASSERT(buffer != buffer_end);

    return val;
    #undef buffsize
}

static const struct hw_InstData *_get_instruction(
    hw_CompilerBC *comp, hw_byte const *str, hw_uint size)
{
    hw_Global const *g = comp->vm_child->global;
    for (hw_byte i = 0; i < g->insts_count; i++) {
        if(!hw_ptrcmp(str, size, g->insts[i].name, g->insts[i].name_size)) {
             return g->insts + i;
        }
    }
    return NULL;
}

static hw_int _compiler_next_eval_operand(hw_CompilerBC *comp)
{
    hw_Lexer_next_skipws(&comp->lexer);
    // -/+[0-9]+ => int
    // -/+[0-9.]+ => float
    //sym => var
    //%sym => fn
    //&sym => lable - pc
    hw_int operand = 0;
    hw_Lexer *lex = &comp->lexer;
   
    switch (lex->token.type) {
        break; case HW_LEXTOKEN_NUMBER: 
            { hw_int point = hw_compbc_lex_getint(comp);
            memcpy(&operand, &point, sizeof(hw_int)); }
        break; case HW_LEXTOKEN_SYMBOL:{
                hw_SymTable *symtable = comp->fnobj->vartable;
                hw_uint index = hw_SymTable_index(symtable
                        , lex->token.start, lex->token.size);
                if(!symtable->key[index]) {
                    _ERROR("Undefined Variable '%.*s'"
                            , (int)lex->token.size, lex->token.start);
                }
                operand = symtable->val[index].as_uint;
            }
        break; case HW_LEXTOKEN_AND:{
                hw_SymTable *symtable = comp->fnobj->lables;
                hw_compbc_lex_next_expect(comp, HW_LEXTOKEN_SYMBOL);
                hw_uint index = hw_SymTable_index(symtable
                    , lex->token.start, lex->token.size);
                if(!symtable->key[index]) {
                    _ERROR("Undefined Lable '%.*s'"
                        , (int)lex->token.size, lex->token.start);
                }
                operand = symtable->val[index].as_uint - comp->obj->code->lenUsed;
                operand -= 1;
        }
        break; case HW_LEXTOKEN_PERCENT:{
                hw_SymTable *symtable = comp->obj->fntable;
                hw_compbc_lex_next_expect(comp, HW_LEXTOKEN_SYMBOL);
                hw_uint index = hw_SymTable_index(symtable
                    , lex->token.start, lex->token.size);
                if(!symtable->key[index]) {
                    _ERROR("Undefined Function '%.*s'"
                        , (int)lex->token.size, lex->token.start);
                }
                operand = symtable->val[index].as_uint;
        }
        break; default:
            _ERROR("Unknown Operand%s", "");
    }

    return operand;
}

static void _compiler_inst(hw_CompilerBC *comp)
{
    hw_LexToken inst = comp->lexer.token;
    hw_InstData const* instdata = _get_instruction(comp, inst.start, inst.size);
    
    if(instdata == NULL) {
        _ERROR("Unknown Instruction '%.*s'"
            , (int)comp->lexer.token.size , comp->lexer.token.start);
    }

    HW_ASSERT(instdata->no_direct == 0);
    hw_code instruction = { 0 };

    switch (instdata->inst_type) {
        break; case hw_InstType_a:
            instruction.get.A = _compiler_next_eval_operand(comp);
        break; case hw_InstType_ab:
            instruction.get.A = _compiler_next_eval_operand(comp);
            instruction.get.B = _compiler_next_eval_operand(comp);
        break; case hw_InstType_abc:
            instruction.get.A = _compiler_next_eval_operand(comp);
            instruction.get.B = _compiler_next_eval_operand(comp);
            instruction.get.C = _compiler_next_eval_operand(comp);
        break; case hw_InstType_as32:
            instruction.gets.A = _compiler_next_eval_operand(comp);
            instruction.gets.s32 = _compiler_next_eval_operand(comp);
        break; case hw_InstType_ax32:
            instruction.getx.A = _compiler_next_eval_operand(comp);
            instruction.getx.x32 = _compiler_next_eval_operand(comp);
        break; case hw_InstType_nop:
        break; case hw_InstType_s32:
            instruction.gets.s32 = _compiler_next_eval_operand(comp);
        break; case hw_InstType_x32:
            instruction.getx.x32 = _compiler_next_eval_operand(comp);
        break;
    }

    instruction.get.opcode = instdata->inst_code;
    hw_compbc_inst(comp, instruction);
}

static void _compiler_defvar(hw_CompilerBC *comp)
{
    hw_compbc_lex_next_skipws_expect(comp, HW_LEXTOKEN_SYMBOL);
    hw_CStr var_name = { .data = (void *)comp->lexer.token.start, .len = comp->lexer.token.size };
    hw_compbc_lex_next_skipws_expect(comp, HW_LEXTOKEN_COLON);
    hw_compbc_lex_next_skipws_expect(comp, HW_LEXTOKEN_SYMBOL);
    hw_Type *T = hw_TypeSys_get(
        comp->vm_child->ts
        , (void *)comp->lexer.token.start, comp->lexer.token.size);
    HW_ASSERT(T && " Unknown Type ");

    hw_compbc_deflocalvar(comp, var_name.data, var_name.len, (hw_VarInfo){.type = T->id, .is_unique = HW_TRUE});
}

static void _compiler_atsym_defn(hw_CompilerBC *comp)
{
    hw_compbc_lex_next_skipws_expect(comp, HW_LEXTOKEN_SYMBOL);
    
    hw_LexToken tok_fn_name = comp->lexer.token;
    
    HW_ARR(hw_CStr) *arg_names;
    hw_byteArr *types;

    HW_ARR_NEW(comp->vm_child, types, 8);
    HW_ARR_NEW(comp->vm_child, arg_names, 8);

    hw_compbc_lex_next_skipws_expect(comp, HW_LEXTOKEN_BRACE_LEFT);
    while(1) {

        hw_compbc_lex_next_skipws(comp);
        if(hw_LexToken_is(comp->lexer.token, HW_LEXTOKEN_BRACE_RIGHT)) {
            goto _L_done;
        } else if(hw_LexToken_is(comp->lexer.token, HW_LEXTOKEN_SYMBOL)) {

        } else {
            _ERROR("expected 'SYMBOL' or '}'%s", "");
        }
        
        
        hw_CStr arg_name = { .data = (void *)comp->lexer.token.start
                           , .len = comp->lexer.token.size };
        hw_compbc_lex_next_skipws_expect(comp, HW_LEXTOKEN_COLON);
        hw_compbc_lex_next_skipws_expect(comp, HW_LEXTOKEN_SYMBOL);
        hw_Type *T = hw_TypeSys_get(
            comp->vm_child->ts
            , (void *)comp->lexer.token.start, comp->lexer.token.size);
        HW_ASSERT(T && " Unknown Type ");

        HW_ARR_PUSH(comp->vm_child, types, T->id);
        HW_ARR_PUSH(comp->vm_child, arg_names, arg_name);
    
        hw_compbc_lex_next_skipws(comp);

        if(hw_LexToken_is(comp->lexer.token, HW_LEXTOKEN_BRACE_RIGHT)) {
            goto _L_done;
        } else if(hw_LexToken_is(comp->lexer.token, HW_LEXTOKEN_COMMA)) {
        
        } else {
            _ERROR("expected ',' or '}'%s", "");
        }

    }
    _L_done:
    HW_ASSERT(types->lenUsed == arg_names->lenUsed);

    hw_compbc_lex_next_skipws_expect(comp, HW_LEXTOKEN_NUMBER);
    hw_uint mut = hw_compbc_lex_getint(comp);

    hw_compbc_w_defn(comp, tok_fn_name.start, tok_fn_name.size, types->lenUsed, mut, types->data, arg_names->data);

    HW_ARR_DELETE(comp->vm_child, types);
    HW_ARR_DELETE(comp->vm_child, arg_names);
}

static void _compiler_atsym_endfn(hw_CompilerBC *comp)
{
    hw_compbc_w_endfn(comp);
}

static void _compiler_atsym(hw_CompilerBC *comp)
{
    hw_compbc_lex_next_skipws_expect(comp, HW_LEXTOKEN_SYMBOL);

    #define TOK_MATCH_START \
                if(0) {
    
    #define TOKEN_MATCH(str) }\
                else if (!hw_ptrcmp(#str, sizeof(#str)-1\
                        , comp->lexer.token.start, comp->lexer.token.size)) {

    #define TOK_MATCH_END } else { _ERROR("Unknown intrinsic @%.*s"\
            , (int)comp->lexer.token.size, comp->lexer.token.start); }


    TOK_MATCH_START
    
    TOKEN_MATCH(defn) _compiler_atsym_defn(comp);
    TOKEN_MATCH(endfn) _compiler_atsym_endfn(comp);
    TOKEN_MATCH(defvar) _compiler_defvar(comp);

    TOK_MATCH_END
}

static void _compiler_lable(hw_CompilerBC *comp)
{
    hw_compbc_lex_next_expect(comp, HW_LEXTOKEN_SYMBOL);
    hw_compbc_w_lable(comp, comp->lexer.token.start, comp->lexer.token.size);
}

static hw_bool _compiler_top(hw_CompilerBC *comp)
{
    hw_Lexer *lex = &comp->lexer;
    #define TOKEN(x) break; case HW_LEXTOKEN_##x:

    if(hw_LexToken_is_ws(comp->lexer.token)) { 
        hw_compbc_lex_next_skipws(comp);
    }
    if(hw_LexToken_is(comp->lexer.token, HW_LEXTOKEN_END_OF_SOURCE)) {
        return HW_TRUE;
    }

    switch (lex->token.type) {

        // Comment Block
        TOKEN(SEMI_COLON) { hw_Lexer_next_until(lex, HW_LEXTOKEN_NEWLINE); }

        // Instruction
        TOKEN(SYMBOL) { _compiler_inst(comp); }

        // Function Defn, endef
        TOKEN(AT) { _compiler_atsym(comp); }

        TOKEN(COLON) { _compiler_lable(comp); }
        
        break; default: {
                hw_CStr const tokenname = hw_get_token_name(comp->lexer.token.type);
                _ERROR("Expected 'Comment', 'Instruction', Compiler Invoke got %.*s"
                        , (int)tokenname.len, tokenname.data);
            }
        break;
    }

    return HW_FALSE;
}

void hw_compbc_compile_from_source(hw_CompilerBC *comp)
{
    HW_DEBUG(
        HW_LOG("COMPILING FILE:%.*s"
            , (int)comp->sname.len, comp->sname.data);
    );

    hw_Lexer_start(&comp->lexer, comp->source.data, comp->source.len);
    hw_compbc_lex_next_skipws(comp);
    hw_bool isend = 0;
    while(!isend) {
        isend = _compiler_top(comp);
        hw_compbc_lex_next_skipws(comp);
    }
}

