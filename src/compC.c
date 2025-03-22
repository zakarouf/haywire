#include "dev.h"
#include "cstd.h"

typedef struct hw_CompilerC hw_CompilerC;
struct hw_CompilerC {
    hw_State *vm_parent;
    hw_State *vm_child;
    hw_Module const *source;
    hw_String *out;
};

hw_CompilerC *hw_compc_new(hw_State *parent)
{
    hw_State *child = hw_State_new_default(parent);
    hw_CompilerC *c = HW_THREAD_ALLOC(child, sizeof(*c));
    c->vm_child = child;
    c->vm_parent = parent;
    HW_VARFN(c->vm_child, hw_String_new, ([0].as_string = 0), (hw_TypeID_string)
            , c->out = __args[0].as_string;);
    return c;
}

void hw_compc_delete(hw_CompilerC *c)
{
    HW_VARFN(c->vm_child, hw_String_delete
            , ([0].as_string = c->out)
            , (hw_TypeID_string),);

    HW_THREAD_FREE(c->vm_child, c);
    hw_State *child = c->vm_child;
    hw_State_delete(child);

}

static void _append_string(hw_CompilerC *c, hw_String *s)
{
    HW_ARR_PUSHSTREAM(c->vm_child, c->out, s->data, s->lenUsed);
}

hw_String* hw_compc_mod_to_c(hw_State *hw, hw_Module const *m) {
    hw_CompilerC *c = hw_compc_new(hw);
    c->source = m;

    hw_String_append_fmt(hw, &c->out
            , "#include \"<hw.h>\"\n"
              "#include \"<hw_dev.h>\"\n"
              "\n"
              ""
            );

    hw_compc_delete(c);
}
