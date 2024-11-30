#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include "hw.h"
#include "hw_dev.h"

struct hw_InstData {
    char const      *name;
    hw_byte const   name_size;
    enum hw_InstType const   inst_type;
    hw_byte const   does_return;
};

#define INST(x) hw_Inst_##x
#define ID(_name, dret, it)             \
    [INST(_name)] = {                   \
        .name = #_name                  \
      , .name_size = sizeof(#_name)-1   \
      , .inst_type = hw_InstType_##it   \
      , .does_return = dret             \
    }

struct hw_InstData const HW_INST_DATA[] = {
    ID(nop,     HW_FALSE, nop)
  , ID(defn,    HW_FALSE, nop)
  , ID(return,  HW_FALSE, a)
  , ID(reserve, HW_FALSE, a)

  // ------
  , ID(v_new,       HW_FALSE, a)
  , ID(v_newc,      HW_FALSE, ab)
  , ID(v_newd,      HW_FALSE, ax32)
  , ID(v_del,       HW_FALSE, a)
  , ID(v_reset,     HW_FALSE, a)
  , ID(v_resetc,    HW_FALSE, ab)
  , ID(v_copy,      HW_FALSE, ab)
  , ID(v_hash,      HW_TRUE , ab)
  , ID(v_data,      HW_TRUE , ab)
  , ID(v_string,    HW_TRUE , ab)

  // ------
  , ID(v_call,      HW_TRUE, abc)

  // ------
  , ID(set_dup,     HW_FALSE, ab)
  , ID(set_type,    HW_FALSE, ab)
  , ID(set_nil,     HW_FALSE, a)
  , ID(set_32a,     HW_FALSE, ax32)
  , ID(set_32b,     HW_FALSE, ax32)
  , ID(set_list,    HW_FALSE, ax32)
  , ID(set_string,  HW_FALSE, ax32)
  
  /* Comaparism And Jump*/
  , ID(jmp,     HW_FALSE, a)
  , ID(jmp0,    HW_FALSE, x32)
  
  , ID(cmp,     HW_FALSE, abc) 
  , ID(typeq,   HW_FALSE, abc) 
  , ID(tideq,   HW_FALSE, abc) 
  , ID(veq,     HW_FALSE, abc) 
  , ID(vle,     HW_FALSE, abc)
  , ID(vlt,     HW_FALSE, abc)

  /* Print */
  , ID(print, HW_FALSE, a)
  , ID(pinfo, HW_FALSE, a)

  , ID(TOTAL, HW_FALSE, nop)
};

#undef INST
#undef ID


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

void check_vm_inst(void)
{
    for (size_t i = 0; i < hw_Inst_TOTAL; i++) {
        HW_ASSERT(HW_INST_DATA[i].name);
        HW_ASSERT(HW_INST_DATA[i].name_size);
    }
}


void hw_code_disasm_default(
    struct hw_InstData insdata, hw_code code)
{
    FILE *out = stdout;
    fwrite(insdata.name, insdata.name_size, 1, out);
    fputc(' ', out);
    
    switch (insdata.inst_type) {
       break; case hw_InstType_nop:
           fprintf(out, "NOP");
       break; case hw_InstType_a:
           fprintf(out, "a=%hu", code.get.A);
       break; case hw_InstType_ab:
           fprintf(out, "a=%hu b=%hu", code.get.A, code.get.B);
       break; case hw_InstType_abc:
           fprintf(out, "a=%hu b=%hu c=%hu"
                   , code.get.A, code.get.B, code.get.C);
       break; case hw_InstType_ax32:
           fprintf(out, "a=%hu x=%u|s=%i", code.get.A
                   , code.getx.x32, code.gets.s32);
       break; case hw_InstType_x32:
           fprintf(out, "x=%u|s=%i", code.getx.x32, code.gets.s32);
            break;
    }    
}

void hw_Module_disasm(hw_Module *m)
{
    for (size_t i = 0; i < m->code_len; i++) {
        struct hw_InstData id = HW_INST_DATA[m->code[i].get.opcode];
        hw_code_disasm_default(id, m->code[i]);
        fputc('\n', stdout);
    }
}

void static_checks(void)
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


int main(int argc, char *argv[])
{
    hw_State *s = hw_new();
    check_vm_inst();

    hw_VarArr *args = wrap_args(s->tsys, argc, argv);
    HW_DEBUG( 
        for (size_t i = 0; i < args->lenUsed; i++) {
            fwrite(
                args->data[i].as_string->data
              , args->data[i].as_string->lenUsed
              , 1, stdout);
            printf(" ");
        }
    );

    hw_logp("All Ok\n");
    hw_delete(s);
    return EXIT_SUCCESS;
}
