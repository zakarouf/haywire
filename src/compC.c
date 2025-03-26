#include "def.h"
#include "dev.h"
#include "cstd.h"
#include "hwfn.h"

#define call(...) { if(!(__VA_ARGS__)) { HW_LOG("ERROR @" #__VA_ARGS__ "%s", ""); return 0; }  }
#define indent(i) _comp_indent(comp, i)
#define appendln_fmt(fmt, ...)                                      \
    {  indent(comp->indent);                                        \
       hw_String_append_fmt(comp->vm_child                          \
                         , &comp->out, fmt "\n", __VA_ARGS__);      \
    }

#define set_indent(i) { comp->indent = i; }

typedef struct hw_CompilerC hw_CompilerC;
struct hw_CompilerC {
    hw_State *vm_parent;
    hw_State *vm_child;
    hw_Module const *source;
    hw_Error error;
    hw_String *tmp_buffer;
    hw_String *out;
    hw_byte indent;
};

hw_CompilerC *hw_compc_new(hw_State *parent)
{
    hw_State *child = hw_State_new_default(parent);
    hw_CompilerC *c = HW_THREAD_ALLOC(child, sizeof(*c));
    c->vm_child = child;
    c->vm_parent = parent;
    c->out = hw_String_new(child, 256);
    c->tmp_buffer = hw_String_new(child, 128);
    c->indent = 0;
    return c;
}

void hw_compc_delete(hw_CompilerC *c)
{
    HW_VARFN(c->vm_child, hwfn_String_delete
            , ([0].as_string = c->out)
            , (hw_TypeID_string),);

    HW_VARFN(c->vm_child, hwfn_String_delete
            , ([0].as_string = c->tmp_buffer)
            , (hw_TypeID_string),);

    HW_THREAD_FREE(c->vm_child, c);
    hw_State *child = c->vm_child;
    hw_State_delete(child);
}

static void _comp_indent(hw_CompilerC *c, hw_byte count)
{
    #define TABSPACE 4
    for (size_t i = 0; i < count * TABSPACE; i++) {
        hw_String_push(c->vm_child, &c->out, ' ');
    }
    #undef MAX_TABSPACE
    #undef TABSPACE
}

static hw_u32 _comp_fn_inst(hw_CompilerC *comp, hw_code const *instp)
{
    #define error(fmt, ...) { comp->tmp_buffer->lenUsed = 0;         \
       hw_String_append_fmt(comp->vm_child                           \
                         , &comp->tmp_buffer, fmt "\n", __VA_ARGS__);\
        return 0;                                                    \
    }

    #define instex(opcode) break; case hw_Inst_##opcode:
    #define inst(opcode, fmt, ...) instex(opcode) { \
                                            appendln_fmt(fmt, __VA_ARGS__);\
                                        }
    hw_u32 inst_read = 1;
    hw_code inst = *instp;
    switch (inst.get.opcode) {
        inst(defn, "// defn info = %u", inst.getx.x32);
        inst(return, "goto _L_return%c", ';');
        
        instex(call) { 
                hw_FnInfo called_info;
                hw_Module_get_FnInfo(comp->source
                        , inst.getx.x32, &called_info);
                
                appendln_fmt("_%u(hw,"
                             " &vars[(STACK_SZ - %u)], "
                             " &tids[(STACK_SZ - %u)], %u);"
                        , inst.getx.x32
                        , (int)called_info.arg_count
                        , (int)called_info.arg_count
                        , (int)called_info.arg_count
                        );

        }
        
        inst(dup, "vars[%u] = vars[%u]; tids[%u] = tids[%u];"
                    , inst.get.A, inst.get.B, inst.get.A, inst.get.B);

        inst(loada32, "vars[%u].as_uint = %u;", inst.getx.A, inst.getx.x32);
        inst(loadknst, "vars[%u] = _knst[%u];", inst.getx.A, inst.getx.x32);

        inst(i_ksub, "vars[%u].as_int = vars[%u].as_int - %i;"
                        , inst.get.A, inst.get.B, inst.get.C);

        inst(i_add, "vars[%u].as_int = vars[%u].as_int + vars[%u].as_int;"
                        , inst.get.A, inst.get.B, inst.get.C);

        instex(i_kles){ 
            appendln_fmt("if((vars[%u].as_int <= %u)) {"
                            , inst.getx.A, inst.getx.x32); {
                set_indent(comp->indent + 1);
                inst_read += _comp_fn_inst(comp, instp + 1);
                set_indent(comp->indent - 1);
            }
            appendln_fmt("}%c", '\n');
        }

        inst(prnt, "hw_debug_print_var(hw, vars[%u], tids[%u]);"
                    , inst.get.A, inst.get.A);

        break; default:
            error("Instruction not Implement '%.*s'", comp
                    ->vm_child->global->insts[inst.get.opcode].name_size
              , comp->vm_child->global->insts[inst.get.opcode].name);
    }

    return inst_read;
}

static hw_code const *_comp_fn_get_pc_and_len(hw_Module const *m
                                     , hw_u32 id
                                     , hw_u32 *len)
{
    hw_u32 fnpt = m->fnpt[id];
    hw_code const *code = m->code + fnpt;
    if(id == m->fn_count - 1) {
        *len = (m->code + m->code_len) - code;
    } else {
        *len = (m->code + m->fnpt[id + 1]) - code;
    }
    return code;
}

static void _comp_fn_set_types(hw_CompilerC *comp, hw_FnInfo const *info)
{
    for (hw_u32 i = 0; i < info->stack_sz; i++) {
        appendln_fmt("tids[%u] = %u;", i, info->types[i]);
    }
}

static hw_bool _comp_fn(hw_CompilerC *comp, hw_u32 fn_id)
{
    set_indent(0);
    appendln_fmt("static void _%u(hw_State *hw, hw_Var *args, hw_byte *argTs, hw_uint argc) {", fn_id);

    set_indent(1);

    hw_FnInfo fn_info;
    hw_Module_get_FnInfo(comp->source, fn_id, &fn_info);

    appendln_fmt("(void)hw%c", ';');
    appendln_fmt("(void)args%c", ';');
    appendln_fmt("(void)argTs%c", ';');
    appendln_fmt("(void)argc%c", ';');

    appendln_fmt("#define STACK_SZ (%u)", (hw_u32)fn_info.stack_sz);
    appendln_fmt("hw_Var vars[STACK_SZ] = {0}%c", ';');
    appendln_fmt("hw_byte tids[STACK_SZ] = {0}%c", ';');
    _comp_fn_set_types(comp, &fn_info);

    if(fn_info.arg_count) {
        appendln_fmt("memcpy(vars, args, sizeof(*args) * %"PRIu64");"
                , fn_info.arg_count);
    }

    hw_u32 inst_count = 0;
    hw_code const *inst = _comp_fn_get_pc_and_len(comp->source
                                                , fn_id, &inst_count);
    for (size_t i = 0; i < inst_count;) {
        hw_u32 inc = _comp_fn_inst(comp, inst + i);
        call(inc);
        i += inc;
    }

    appendln_fmt("_L_return:%c", ';');

    if(fn_info.mut_count) {
        appendln_fmt("memcpy(args, vars, sizeof(*args) * %"PRIu64");"
                , fn_info.mut_count);
        appendln_fmt("memcpy(argTs, tids, sizeof(*tids) * %"PRIu64");"
                , fn_info.mut_count);
    }

    appendln_fmt("#undef STACK_SZ%c", ' ');
    set_indent(0);
    appendln_fmt("%c\n", '}');

    return 1;
}

static void _comp_add__hwfn_info(hw_CompilerC *comp)
{
    set_indent(0);
    appendln_fmt("void __hwfn_info(hw_u32 *fn, hw_u32 *k)%c", '{');
    set_indent(1);
    appendln_fmt("*fn = %u;", (hw_u32)comp->source->fn_count);
    appendln_fmt("*k = %u;", (hw_u32)comp->source->k_count);
    set_indent(0);
    appendln_fmt("%c", '}');
}

static void _comp_add__hwfn_init(hw_CompilerC *comp)
{
    set_indent(0);
    appendln_fmt("static hw_CModule _cmod = %c", '{');

    set_indent(1);

    appendln_fmt("  .fn_count = %u", (hw_u32)comp->source->fn_count);
    appendln_fmt(", .k_count = %u", (hw_u32)comp->source->k_count);

    if(comp->source->fn_count) {
        appendln_fmt(", .fn = (hw_VarFn[]){%c", ' ');
        set_indent(3);
        for (hw_u32 i = 0; i < comp->source->fn_count; i++) {
            appendln_fmt("_%u,", i);
        }

        set_indent(1);
        appendln_fmt("}%c", ' ');


        appendln_fmt(", .fn_name = (hw_CStr*[]){%c", ' ');
        set_indent(3);
        for (hw_u32 i = 0; i < comp->source->fn_count; i++) {
            hw_FnInfo info; 
            hw_Module_get_FnInfo(comp->source, i, &info);
            appendln_fmt("&(hw_CStr){.data = (void *)\"%.*s\", .len = %u},"
                    , (int)info.name_size, info.name
                    , (int)info.name_size);
        }
        set_indent(1);
        appendln_fmt("}%c", ' ');
    }

    if(comp->source->k_count) {
        appendln_fmt(", .knst = _knst%c", ' ');
    }

    

    // Done
    set_indent(0);
    appendln_fmt("%c;\n", '}');   

    set_indent(0);
    appendln_fmt("HWAPI hw_CModule* _hwfn_init(hw_State *hw)%c", '{');
    set_indent(1);
    appendln_fmt("(void)hw%c", ';');
    appendln_fmt("return &_cmod%c", ';');
    set_indent(0);
    appendln_fmt("%c", '}');   
}

hw_String* hw_compc_mod_to_c(hw_State *hw, hw_Module const *m) {
    hw_CompilerC *comp = hw_compc_new(hw);
    comp->source = m;

    appendln_fmt("#include %s", "\"src/hw.h\"");
   
    // Const Decl
    appendln_fmt("static hw_Var _knst[%u+1] = {", (hw_u32) m->k_count);
    set_indent(1);
    for (hw_u32 i = 0; i < m->k_count; i++) {
        hw_Type const *T = hw_TypeSys_get_via_id(comp->vm_child->ts, m->knst_t[i]);
        if(!T->is_obj) {
            appendln_fmt("[%u].as_uint = %"PRIu64","
                    , i, m->knst[i].as_uint);
        }
    }
    set_indent(0);
    appendln_fmt("};%s", "");

    // Forward Decl
    for (hw_u32 i = 0; i < m->fn_count; i++) {
        appendln_fmt("static void _%u(hw_State *, hw_Var*, hw_byte*, hw_uint);", i)
    }
    appendln_fmt("%c", '\n');

    for (hw_u32 i = 0; i < m->fn_count; i++) {
        if(!_comp_fn(comp, i)) {
            HW_LOG("Transpiler Error %s", "");
            hw_debug_print_var(hw, (hw_Var){.as_string = comp->tmp_buffer}
                                , hw_TypeID_string);
        }
    }

    _comp_add__hwfn_init(comp);
    _comp_add__hwfn_info(comp);

    hw_String *out = hw_String_newFrom_data(hw, comp->out->data
                                         , comp->out->lenUsed);

    hw_compc_delete(comp);
    return out;
}

#undef appendln_fmt
#undef set_indent
