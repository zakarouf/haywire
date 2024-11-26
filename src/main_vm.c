#include <stdlib.h>
#include <string.h>
#include "hw.h"
#include "hw_dev.h"

hw_State *hw_new(void)
{
    hw_Allocator allocator;
    hw_Allocator_default(&allocator);
    hw_TypeSys *ts = hw_TypeSys_default_with_allocator(allocator);

    hw_State *self = HW_TYPESYS_ALLOC(ts, sizeof(*self));
    memset(self, 0, sizeof(*self));

    self->tsys = ts;
    
    hw_Thread_init(&self->main_thread, self, 0, "main", 4);  

    return self;
}

void hw_delete(hw_State *self)
{
    hw_TypeSys *ts = self->tsys;

    hw_Thread_deinit(&self->main_thread);

    HW_TYPESYS_FREE(ts, self);
    hw_TypeSys_delete(ts);
}

static hw_VarArr *wrap_args(hw_TypeSys *ts, int argc, char *argv[])
{
    hw_Var arr = {0};
    hw_VarArr_newFrom_conf(
        &arr, ts
      , (hw_Var[]){ (hw_Var){.as_uint = hw_TypeID_string} }
      , (hw_byte[]){hw_TypeID_uint}, 1);

    for (int i = 0; i < argc; i++) {
        hw_Var string;
        hw_Var arg0_data = {.as_ptr = argv[i]};
        hw_Var arg1_size = {.as_uint = strlen(argv[i])};
        hw_String_newFrom_cstr(&string, ts
            , (hw_Var[]){ arg0_data, arg1_size }
            , (hw_byte[]){ hw_TypeID_ptr, hw_TypeID_uint}, 2);

        hw_VarArr_push(&arr, ts, &string, (hw_byte[]){hw_TypeID_string}, 1);
    }

    return arr.as_arr;
}

int main(int argc, char *argv[])
{
    hw_State *s = hw_new();
    
    hw_VarArr *args = wrap_args(s->tsys, argc, argv);

    hw_delete(s);
    return EXIT_SUCCESS;
}
