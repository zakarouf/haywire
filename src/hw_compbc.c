#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <inttypes.h>

#include "hw.h"
#include "hw_dev.h"

hw_CStr hw_get_token_name(hw_uint token_type);

enum DeferLableOperand {
    DEFER_LABLE_OPERAND_A = 0,
    DEFER_LABLE_OPERAND_B,
    DEFER_LABLE_OPERAND_C,
    DEFER_LABLE_OPERAND_x32,
    DEFER_LABLE_OPERAND_s32
};

#define _ERROR(fmt, ...)\
    hw_compbc_ERROR(comp, "%d|" fmt, __LINE__, __VA_ARGS__)

static void hw_compbc_ERROR(hw_CompilerBC *comp, const char *restrict fmt, ...)
__attribute__ ((format (printf, 2, 3)));

static hw_VarP _get_symbol(hw_SymTable *symtable
        , hw_byte const *sym, hw_u32 size)
{
    hw_uint index = hw_SymTable_index(symtable, sym, size);
    if(!symtable->key[index]) { return (hw_VarP){ .type = hw_TypeID_nil}; }
    return (hw_VarP){ .type = symtable->valTs[index]
                    , .value = symtable->val[index]  };
}

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
    
    hw_uint column = line_part1_size;

    hw_loglnp("%"PRIu64":%"PRIu64"| "
                "%.*s\x1b[0;31m%.*s\x1b[0m%.*s\n"
//                "%d''%s ~~%d''%s~~ %d''%s\n"
            , comp->lexer.token.line, column
            , line_part1_size, comp->lexer.token.line_start
            , (int)comp->lexer.token.size, comp->lexer.token.start
            , line_part2_size, comp->lexer.token.start + comp->lexer.token.size);
        

        HW_DEBUG(
          //, (int)comp->lexer.token.size, comp->lexer.token.start
         //   , (int)token_name.len, token_name.data);
        )

    
    exit(-1);
}


static void hw_FnObj_new(hw_CompilerBC *comp)
{
    hw_FnObj *fnobj = HW_THREAD_ALLOC(comp->vm_child, sizeof(*fnobj));

    memset(fnobj, 0, sizeof(*fnobj));
    HW_ARR_NEW(comp->vm_child, fnobj->var_infos, 8);
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
    fnobj->defer_lables->lenUsed = 0;

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

    hw_Var list = {0};
    hw_VarList_new(
        comp->vm_child, &list
      , (hw_byte[1]){hw_TypeID_nil}, 1); obj->knst = list.as_list;

    HW_ARR_NEW(comp->vm_child, obj->data, 128);
    HW_ARR_NEW(comp->vm_child, obj->code, 80);
    HW_ARR_NEW(comp->vm_child, obj->defer_fncall_pc, 16);
    HW_ARR_NEW(comp->vm_child, obj->defer_fncall_names, 16);

    hw_Var arg;
    hw_byte tid = hw_TypeID_symtable;
    hw_SymTable_new(comp->vm_child, &arg, &tid, 1);
    obj->fntable = arg.as_symtable;

    hw_SymTable_new(comp->vm_child, &arg, &tid, 1);
    obj->knsttable = arg.as_symtable;

    comp->obj = obj;
}

static void hw_ModuleObj_delete(hw_CompilerBC *comp)
{
    HW_ARR_DELETE(comp->vm_child, comp->obj->defer_fncall_names);
    HW_ARR_DELETE(comp->vm_child, comp->obj->defer_fncall_pc);

    hw_VarList_delete(
        comp->vm_child, &(hw_Var){.as_list = comp->obj->knst}
        , (hw_byte[1]){hw_TypeID_list}, 1);

    HW_ARR_DELETE(comp->vm_child, comp->obj->code);
    HW_ARR_DELETE(comp->vm_child, comp->obj->fnpt);

    HW_ARR_DELETE(comp->vm_child, comp->obj->data);

    hw_Var arg = { .as_symtable = comp->obj->fntable };
    hw_byte tid = hw_TypeID_symtable;
    hw_SymTable_delete(comp->vm_child, &arg, &tid, 1);

    arg.as_symtable = comp->obj->knsttable;
    hw_SymTable_delete(comp->vm_child, &arg, &tid, 1);

    HW_THREAD_FREE(comp->vm_child, comp->obj);
}

static void hw_ModuleObj_reset(hw_CompilerBC *comp)
{
    hw_ModuleObj *mobj = comp->obj;
    mobj->code->lenUsed = 0;
    mobj->data->lenUsed = 0;
    mobj->defer_fncall_names->lenUsed = 0;
    mobj->defer_fncall_pc->lenUsed = 0;
    
    hw_Var args[] = { [0].as_symtable = mobj->fntable };
    hw_byte tid[] = {hw_TypeID_symtable};
    hw_SymTable_reset(comp->vm_child, args, tid, 1);

    args[0].as_symtable = mobj->knsttable;
    hw_SymTable_reset(comp->vm_child, args, tid, 1);

    for (size_t i = 0; i < mobj->knst->lenUsed; i++) {
        HW_VAR_CALLEX(comp->vm_child
                    , mobj->knst->tid[i]
                    , mobj->knst->data[i]
                    , "delete", (), ());
    }

    mobj->knst->lenUsed = 0;
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
    if(source == NULL) {
        HW_DEBUG(HW_LOG("~~FAILED TO LOAD FILE: %s", source_name));
    };

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

void hw_compbc_reset(hw_CompilerBC *comp)
{
    hw_ModuleObj_reset(comp);
}

hw_Module* hw_compbc_convert(hw_CompilerBC *comp)
{
    hw_ModuleObj *obj = comp->obj;
    hw_Module *mod = hw_Module_newblank(comp->vm_parent, obj->fnpt->lenUsed
                                                       , obj->code->lenUsed
                                                       , obj->data->lenUsed
                                                       , obj->knst->lenUsed, 0);

    memcpy(mod->data, obj->data->data, obj->data->lenUsed);
    memcpy(mod->fnpt, obj->fnpt->data
            , mod->fn_count * sizeof(*mod->fnpt));
    memcpy(mod->code, obj->code->data
            , obj->code->lenUsed * sizeof(*mod->code));
    memcpy(mod->knst_t, obj->knst->tid
            , obj->knst->lenUsed * sizeof(*mod->knst_t));

    for (size_t i = 0; i < mod->k_count; i++) {
        HW_DEBUG(HW_LOG("@CONST %"PRIu64 ", typeid(%"PRIu8")", i, mod->knst_t[i] );)
        hw_Type *T = hw_TypeSys_get_via_id(comp->vm_parent->ts, mod->knst_t[i]);
        HW_ASSERT_NOTNULL(T);
        if(T->is_obj) {
            hw_VarFn    copy = hw_Type_getvt(T, "newFrom_copy", 4*3);
            hw_Var      args[2] = {mod->knst[i], obj->knst->data[i]};
            hw_byte     tids[2] = {mod->knst_t[i], mod->knst_t[i]};
            copy(comp->vm_parent, args, tids, 2);
            mod->knst[i] = args[0];
        } else {
            mod->knst[i] = obj->knst->data[i];
        }
    }

    return mod;
}

hw_uint hw_compbc_inst(hw_CompilerBC *comp, hw_code const inst)
{
    HW_ARR_PUSH(comp->vm_child, comp->obj->code, inst);
    return comp->obj->code->lenUsed-1;
}

hw_uint hw_compbc_inststream(hw_CompilerBC *comp, hw_code const *insts, hw_u32 i_count)
{
    hw_u32 at = comp->obj->code->lenUsed;
    HW_ARR_PUSHSTREAM(comp->vm_child, comp->obj->code, insts, i_count);
    return at;
}

hw_uint hw_compbc_data(hw_CompilerBC *comp, void const *data, hw_uint const size)
{
    hw_uint const index = comp->obj->data->lenUsed;
    HW_ARR_PUSHSTREAM(comp->vm_child, comp->obj->data, data, size);
    HW_DEBUG(HW_LOG("DATA INDEX %"PRIu64 "\n", index));
    return index;
}

hw_uint hw_compbc_fndata(
    hw_CompilerBC *comp, hw_byte const *name, hw_byte const *tids
  , hw_u32 name_size, hw_u32 mut_count, hw_u32 args_passed, hw_u32 stack_size)
{
    hw_uint const fninfo_index = 
    hw_compbc_data(comp, (hw_uint[]){name_size}, sizeof(hw_uint));
    hw_compbc_data(comp, (hw_uint[]){mut_count}, sizeof(hw_uint));
    hw_compbc_data(comp, (hw_uint[]){args_passed}, sizeof(hw_uint));
    hw_compbc_data(comp, (hw_uint[]){stack_size}, sizeof(hw_uint));
    hw_compbc_data(comp, name, name_size);

    hw_compbc_data(comp, tids, sizeof(*tids) * stack_size);
    return fninfo_index;
}

hw_uint hw_compbc_knst(hw_CompilerBC *comp, hw_Var val, hw_byte val_tid)
{
    hw_Var args[2] = { 
        [0].as_list = comp->obj->knst,
        [1] = val
    };
    hw_byte tid[2] = { hw_TypeID_list, val_tid };
    hw_VarList_push_shallow(comp->vm_child, args, tid, 2);
    comp->obj->knst = args[0].as_list;
    return comp->obj->knst->lenUsed-1;
}

hw_uint hw_compbc_knstcopy(hw_CompilerBC *comp, hw_Var val, hw_byte val_tid)
{
    hw_Var args[2] = { 
        [0].as_list = comp->obj->knst,
        [1] = val
    };
    hw_byte tid[2] = { hw_TypeID_list, val_tid };
    hw_Type *T = hw_TypeSys_get_via_id(comp->vm_child->ts, val_tid);
    HW_ASSERT(T);
    if(T->is_obj) {
        hw_VarFn newFrom_copy = hw_Type_getvt(T, "newFrom_copy", 4*3);
        hw_Var copy_args[2] = { [1] = val };
        hw_byte copy_args_tid[2] = { [1] = val_tid};
        newFrom_copy(comp->vm_child, copy_args, copy_args_tid, 2);
        val = copy_args[0];
    }
    hw_VarList_push_shallow(comp->vm_child, args, tid, 2);
    comp->obj->knst = args[0].as_list;
    return comp->obj->knst->lenUsed-1;
}

hw_uint hw_compbc_defknst(
    hw_CompilerBC *comp, hw_byte const *knst_name, hw_uint name_size
    , hw_Var val, hw_byte tid)
{
    hw_ModuleObj *obj = comp->obj;
    HW_ASSERTEX(obj->knsttable->key[
        hw_SymTable_index(obj->knsttable, knst_name, name_size)] == NULL,
        "Constant :%.*s already set", (int)name_size, knst_name);
    
    hw_uint id = hw_compbc_knst(comp, val, tid);
    hw_SymTable_set__wrap(
               comp->vm_child, &obj->knsttable
            , knst_name, name_size
            , (hw_Var){ .as_uint = id }, hw_TypeID_uint);

    return HW_TRUE;
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

static void _compbc_resolve_lables(hw_CompilerBC *comp)
{
    hw_FnObj *fnobj = comp->fnobj;
    HW_DEBUG(HW_LOG("Resolving %"PRIu32" Lables for %.*s"
                , fnobj->defer_lables->lenUsed
                , (int)comp->fnobj->name_sizeUsed, comp->fnobj->name));
    while (fnobj->defer_lables->lenUsed) {
        hw_DeferInst defer = HW_ARR_TOP(fnobj->defer_lables);
        hw_VarP result = _get_symbol(fnobj->lables, defer.symbol.start, defer.symbol.size);
        if (result.type == hw_TypeID_nil) {
            comp->lexer.token = defer.symbol;
            _ERROR("Lable Does not exist %d", 1);
        }
        
        hw_code *inst = comp->obj->code->data + defer.inst;
        HW_DEBUG(
            hw_logp("Instruction: ");
            hw_debug_code_disasm(comp->vm_child, *inst);
            hw_logp("%c",'\n'));

        hw_int relative_lable_pos = result.value.as_uint - defer.inst;
        switch (defer.operand) {
            #define operand(get, X) \
                case DEFER_LABLE_OPERAND_##X: \
                        inst->get.X = relative_lable_pos; break;

                operand(get, A)
                operand(get, B)
                operand(get, C)
                operand(gets, s32)
                operand(getx, x32)
            default: _ERROR("Unknown Operand %s", "");
        }
        HW_DEBUG(
            hw_logp("Resolved: ");
            hw_debug_code_disasm(comp->vm_child, *inst);
            hw_logp("%c",'\n'));

        fnobj->defer_lables->lenUsed -= 1;
    }
}

void hw_compbc_w_endfn(hw_CompilerBC *comp)
{
    hw_FnObj *fnobj = comp->fnobj;
    _compbc_resolve_lables(comp);

    HW_ASSERT(fnobj->lock);
    comp->fnobj->lock = 0;
    HW_ASSERT(fnobj->var_infos->lenUsed == fnobj->vartable->lenUsed);
    
    hw_byte *tids = HW_THREAD_ALLOC(comp->vm_child, fnobj->var_infos->lenUsed);
    for (size_t i = 0; i < fnobj->var_infos->lenUsed; i++) {
        hw_byte typeid = fnobj->var_infos->data[i].type;
        HW_DEBUG(HW_ASSERT(
            hw_TypeSys_get_via_id(comp->vm_child->ts, typeid)));
        tids[i] = typeid;
    }

    hw_uint const fninfo_index = hw_compbc_fndata(comp
            , fnobj->name
            , tids
            , fnobj->name_sizeUsed
            , fnobj->mut_count
            , fnobj->args_passed
            , fnobj->var_infos->lenUsed);

    HW_THREAD_FREE(comp->vm_child, tids);

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


void hw_compbc_defer_lable(hw_CompilerBC *comp, hw_LexToken symbol, hw_byte operand)
{
    hw_FnObj *fnobj = comp->fnobj;
    hw_DeferInst lable = {
        .inst = comp->obj->code->lenUsed,
        .symbol = symbol,
        .operand = operand
    };

    HW_ARR_PUSH(comp->vm_child, fnobj->defer_lables, lable);
}


/**
 * Compiler For HWS
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

void hw_compbc_lex_expect(hw_CompilerBC *comp, enum hw_LexTokenType ttype)
{
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

static hw_LexToken _compiler_try_promote_symbol_todotted(hw_CompilerBC *comp)
{
    hw_Lexer *l = &comp->lexer;
    hw_LexToken fn_name = l->token;
    hw_compbc_lex_next(comp);
    while ( hw_LexToken_is_not(l->token, HW_LEXTOKEN_END_OF_SOURCE)
         && (  hw_LexToken_is(l->token, HW_LEXTOKEN_DOT)
            || hw_LexToken_is(l->token, HW_LEXTOKEN_SYMBOL))) {
        hw_compbc_lex_next(comp);
    }
    fn_name.size = l->token.start - fn_name.start;
    return fn_name;
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
   
    #define assign_operand(table, name, vtype){ \
            hw_VarP result = _get_symbol(\
                      comp->table, lex->token.start, lex->token.size);\
            if(result.type == hw_TypeID_nil) {                  \
                _ERROR("Undefined "name" '%.*s'"                \
                    , (int)lex->token.size, lex->token.start);  \
            }                                                   \
            operand = result.value.vtype;                       \
        }

    switch (lex->token.type) {
        break; case HW_LEXTOKEN_NUMBER: 
            { hw_int point = hw_compbc_lex_getint(comp);
            memcpy(&operand, &point, sizeof(hw_int)); }
        break; case HW_LEXTOKEN_SYMBOL: 
            assign_operand(fnobj->vartable, "Variable", as_uint);
        break; case HW_LEXTOKEN_AND:{
                hw_compbc_lex_next_expect(comp, HW_LEXTOKEN_SYMBOL);
                hw_VarP result = _get_symbol(
                        comp->fnobj->lables
                        , lex->token.start, lex->token.size);
                if( result.type == hw_TypeID_nil ) {
                    hw_compbc_defer_lable(
                        comp, lex->token, comp->fnobj->operand);
                    operand = -1;
                }
                operand = result.value.as_uint - comp->obj->code->lenUsed ;
        }
        break; case HW_LEXTOKEN_PERCENT:{
                hw_compbc_lex_next_expect(comp, HW_LEXTOKEN_SYMBOL);
                comp->lexer.token = _compiler_try_promote_symbol_todotted(comp);                
                assign_operand(obj->fntable, "Function", as_uint);
        }
        break; case HW_LEXTOKEN_HASH: {
                hw_compbc_lex_next_expect(comp, HW_LEXTOKEN_SYMBOL);
                assign_operand(obj->knsttable, "Constant", as_uint);
        }
        break; default:
            _ERROR("Unknown Operand%s", "");
    }
    #undef assign_operand

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

    #define assign_inst(get, X)\
        { comp->fnobj->operand = DEFER_LABLE_OPERAND_##X;\
          instruction.get.X = _compiler_next_eval_operand(comp); }

    switch (instdata->inst_type) {
        break; case hw_InstType_a:      assign_inst(get, A);

        break; case hw_InstType_ab:     assign_inst(get, A);
                                        assign_inst(get, B);

        break; case hw_InstType_abc:    assign_inst(get, A);
                                        assign_inst(get, B);
                                        assign_inst(get, C);

        break; case hw_InstType_as32:   assign_inst(get, A);
                                        assign_inst(gets, s32);

        break; case hw_InstType_ax32:   assign_inst(get, A);
                                        assign_inst(getx, x32);
        break; case hw_InstType_nop:
        break; case hw_InstType_s32:    assign_inst(gets, s32);
        break; case hw_InstType_x32:    assign_inst(getx, x32);
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
    // @defn main (..|..)
    //      
    // @endfn
    hw_compbc_lex_next_skipws_expect(comp, HW_LEXTOKEN_SYMBOL);
    hw_LexToken tok_fn_name = _compiler_try_promote_symbol_todotted(comp);
    
    HW_ARR(hw_CStr) *arg_names;
    hw_byteArr *types;

    HW_ARR_NEW(comp->vm_child, types, 8);
    HW_ARR_NEW(comp->vm_child, arg_names, 8);

    hw_uint mut = 0;
    hw_bool is_defining_mut = 1;
    
    if(hw_LexToken_is_ws(comp->lexer.token)) {
        hw_compbc_lex_next_skipws_expect(comp, HW_LEXTOKEN_PAREN_LEFT);
    } else {
        hw_compbc_lex_expect(comp, HW_LEXTOKEN_PAREN_LEFT);
    }

    _L_again:
    while(1) {
        hw_compbc_lex_next_skipws(comp);
        if(hw_LexToken_is(comp->lexer.token, HW_LEXTOKEN_PAREN_RIGHT)) {
            goto _L_done;
        } else if(hw_LexToken_is(comp->lexer.token, HW_LEXTOKEN_PIPE)) {
            is_defining_mut = 0;
            goto _L_again;
        } else if(hw_LexToken_is(comp->lexer.token, HW_LEXTOKEN_SYMBOL)) {
        } else {
            _ERROR("expected 'SYMBOL', ')', '|'%s", "");
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
        if(is_defining_mut) { mut += 1; }

        hw_compbc_lex_next_skipws(comp);

        if(hw_LexToken_is(comp->lexer.token, HW_LEXTOKEN_PAREN_RIGHT)) {
            goto _L_done;
        } else if(hw_LexToken_is(comp->lexer.token, HW_LEXTOKEN_PIPE)) {
            is_defining_mut = 0;
        } else if(hw_LexToken_is(comp->lexer.token, HW_LEXTOKEN_COMMA)) {
        } else {
            _ERROR("expected ',' or '}'%s", "");
        }

    }
    _L_done:
    HW_ASSERT(types->lenUsed == arg_names->lenUsed);

    hw_compbc_w_defn(comp, tok_fn_name.start, tok_fn_name.size
            , types->lenUsed, mut, types->data, arg_names->data);

    HW_ARR_DELETE(comp->vm_child, types);
    HW_ARR_DELETE(comp->vm_child, arg_names);
}

static void _compiler_atsym_endfn(hw_CompilerBC *comp)
{
    hw_compbc_w_endfn(comp);
}

static void _compiler_atsym_const(hw_CompilerBC *comp)
{
    hw_compbc_lex_next_skipws_expect(comp, HW_LEXTOKEN_SYMBOL);
    hw_LexToken name = comp->lexer.token;
    hw_compbc_lex_next_skipws(comp);
    hw_Var val = {0};
    hw_uint val_tid = hw_Var_parse(comp->vm_child, &val, &comp->lexer);
    hw_compbc_defknst(comp, name.start, name.size, val, val_tid);
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
    TOKEN_MATCH(const) _compiler_atsym_const(comp);

    TOK_MATCH_END



    #undef TOK_MATCH_START
    #undef TOK_MATCH
    #undef TOK_MATCH_END
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

    #undef TOKEN
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

void hw_compbc_combine_addmod(hw_CompilerBC *comp, hw_Module const *m, hw_String *name)
{
    hw_ModuleObj *mobj = comp->obj;
    hw_uint code_start = hw_compbc_inststream(comp, m->code, m->code_len);
    hw_uint fnpt_start = mobj->fnpt->lenUsed;
    hw_uint knst_start = mobj->knst->lenUsed;

    HW_ARR_PUSHSTREAM(comp->vm_child, mobj->fnpt, m->fnpt, m->fn_count);

    hw_Var buffer;
    hw_String_new(comp->vm_child, &buffer, (hw_byte[]){hw_TypeID_nil}, 1);

    //hw_uint data_start = mobj->data->lenUsed;
    for (size_t i = 0; i < m->fn_count; i++) {
        hw_uint *fnpt = mobj->fnpt->data + fnpt_start + i;
        mobj->fnpt->data[fnpt_start + i] += code_start;
        hw_FnInfo finfo;
        hw_Module_get_FnInfo(m, i, &finfo);
        
        buffer.as_string->lenUsed = 0;
        hw_String_fmt(comp->vm_child, &buffer.as_string, "%.*s.%.*s"
                , name->lenUsed, name->data
                , finfo.name_size, finfo.name);
        HW_DEBUG(HW_LOG("FN NAMESPACE, %s", "");
                hw_debug_print_var(comp->vm_child, buffer, hw_TypeID_string));
        
        hw_uint data_at = hw_compbc_fndata(comp
                , buffer.as_string->data, finfo.types
                , buffer.as_string->lenUsed
                , finfo.mut_count, finfo.arg_count, finfo.stack_sz);
        
        HW_ASSERT(mobj->code->data[*fnpt].get.opcode == hw_Inst_defn);
        mobj->code->data[*fnpt].getx.x32 = data_at;
    }
    hw_String_delete(comp->vm_child, &buffer, (hw_byte[]){hw_TypeID_nil}, 1);

    for (size_t i = 0; i < m->k_count; i++) {
        hw_compbc_knst(comp, m->knst[i], m->knst_t[i]);
    }

    for (size_t i = code_start; i < mobj->code->lenUsed; i++) {
        hw_code *inst = mobj->code->data + i;
        switch (inst->get.opcode) {
                   case hw_Inst_loadknst:
                            inst->getx.x32 += knst_start;
            break; case hw_Inst_call:
                            inst->getx.x32 += fnpt_start;
            break;

        }
    }
}

hw_Module *hw_compbc_combine(hw_State *hw, hw_u32 mod_count, hw_Module **mods, hw_String **names)
{
    hw_CompilerBC *comp = hw_compbc_new(hw);
    for (size_t i = 0; i < mod_count; i++) {
        hw_compbc_combine_addmod(comp, mods[i], names[i]);
    }
    hw_Module *m = hw_compbc_convert(comp);
    hw_compbc_delete(comp);
    return m;
}

static hw_String *_Transform_filename_to_modname(hw_State *hw, hw_byte const *file_name, hw_u32 file_namesize)
{
    hw_byte const* end = file_name + (file_namesize - 1);
    hw_byte const* start = file_name;
    hw_byte const* at = start;
    while (at < end) {
        if(*at == '/') { start = at+1; }
        at += 1;
    }
    at = end;
    while (at > start && *at != '.') {
        at -= 1;
    }
    end = at;
    at = start;

    hw_Var args[3] = { 
          [1].as_cbyte_p = start
        , [2].as_uint = end - start};
    hw_byte tids[3];
    hw_String_newFrom_data(hw, args, tids, 3);
    return args[0].as_string;
}

hw_uint hw_compbc_compile_files_and_combine(hw_State *hw, hw_Module **out_mod, hw_u32 files, hw_String **filenames)
{
    HW_ARR(hw_Module *) *mods;
    HW_ARR(hw_String *) *mod_names;

    HW_ARR_NEW(hw, mods, files);
    HW_ARR_NEW(hw, mod_names, files);

    for (size_t i = 0; i < files; i++) {
        hw_CompilerBC *comp = hw_compbc_new(hw);
        hw_String *mod_name = _Transform_filename_to_modname(hw,
                filenames[i]->data, filenames[i]->lenUsed);

        HW_ARR_PUSH(hw, mod_names, mod_name);

        if(!hw_compbc_load_source_fromFile(
                comp, (char *)filenames[i]->data, filenames[i]->lenUsed)) {
            HW_ASSERTEX(0, "FILE NOT LOADED: %.*s"
                    , filenames[i]->lenUsed, filenames[i]->data);
        }

        hw_compbc_compile_from_source(comp);
        hw_Module *mod = hw_compbc_convert(comp);
        HW_ARR_PUSH(hw, mods, mod);
        hw_compbc_delete(comp);
    }

    hw_Module *out = hw_compbc_combine(
            hw, mods->lenUsed, mods->data, mod_names->data);

    for (size_t i = 0; i < files; i++) {
        hw_Module_delete_detatch_knstobj(hw, mods->data[i]);
        hw_String_delete(hw, (hw_Var[]){ [0].as_string = mod_names->data[i]}
                            , (hw_byte[]){ hw_TypeID_string }, 1);
    }
    HW_ARR_DELETE(hw, mod_names);
    HW_ARR_DELETE(hw, mods);
    
    *out_mod = out;
    return hw_Global_add_module((void *)hw->global, out);
}

#undef _ERROR
