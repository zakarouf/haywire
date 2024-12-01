#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <inttypes.h>
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
  , ID(tailret, HW_FALSE, abc) // ret with function call
  , ID(reserve, HW_FALSE, a)
  , ID(release, HW_FALSE, a) // release variables in stack with (optional) delete

  /* Gets */
  , ID(get_type, HW_FALSE, ab)     // R(Ax) = @typeof(R(Bx))
  , ID(get_routine, HW_FALSE, ab)  // R(Ax) = Thread(R(Bx))

  /* Call */
  , ID(call,      HW_TRUE, abc)
  , ID(calln,      HW_TRUE, abc)
  , ID(callc,      HW_TRUE, abc)

  // ------
  , ID(dup,     HW_FALSE, ab)
  , ID(dups,     HW_FALSE, abc)
  , ID(type,    HW_FALSE, ab)
  , ID(nil,     HW_FALSE, a)
  , ID(a32_0,     HW_FALSE, ax32)
  , ID(a32_1,     HW_FALSE, ax32)
  , ID(list,    HW_FALSE, ax32)
  , ID(string,  HW_FALSE, ax32)
  
  /* Comaparism And Jump*/
  , ID(jmp,     HW_FALSE, a)
  , ID(jmp0,    HW_FALSE, x32)
  , ID(jmpif,   HW_FALSE, ax32)
  
  , ID(cmp,     HW_FALSE, abc) 
  , ID(typeq,   HW_FALSE, abc) 
  , ID(tideq,   HW_FALSE, abc) 
  , ID(veq,     HW_FALSE, abc) 
  , ID(vle,     HW_FALSE, abc)
  , ID(vlt,     HW_FALSE, abc)

  , ID(TOTAL, HW_FALSE, nop)
};

#undef INST
#undef ID

static void _check_vm_inst(void)
{
    size_t i;
    for (i = 0; i < hw_Inst_TOTAL; i++) {
        HW_DEBUG( 
          hw_logstr(HW_INST_DATA[i].name, HW_INST_DATA[i].name_size);
          putc(' ', stdout);
        )
        HW_ASSERTEX(HW_INST_DATA[i].name, "%"PRIu64"", i);
        HW_ASSERTEX(HW_INST_DATA[i].name_size, "%"PRIu64, i);
    }
}

static hw_VarArr *_wrap_args(hw_TypeSys *ts, int argc, char *argv[])
{
    hw_Var arr;
    hw_Var args[4] = {
                {0}
              , (hw_Var){.as_uint = hw_TypeID_string} };

    hw_VarArr_newFrom_conf( ts, args 
      , (hw_byte[]){hw_TypeID_list, hw_TypeID_uint}, 2);
    arr = args[0];

    for (int i = 0; i < argc; i++) {
        hw_Var string;

        args[1].as_ptr = argv[i];
        args[2].as_uint = strlen(argv[i]);
        hw_String_newFrom_cstr(ts, args
            , (hw_byte[]){
            hw_TypeID_nil, 
            hw_TypeID_ptr, 
            hw_TypeID_uint}, 3);
        string = args[0];
      
        args[0] = arr;
        args[1] = string;
        hw_VarArr_push(ts, args, (hw_byte[]){
              hw_TypeID_arr,
              hw_TypeID_string}, 2);
        HW_LOG("vm = %p", (void *)arr.as_arr->data[i].as_string);
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


/**********************************************************************
 *                            Public
 **********************************************************************/

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

int main(int argc, char *argv[])
{
    hw_State *s = hw_new();
    _static_checks();
    _check_vm_inst();

    hw_VarArr *c_args = _wrap_args(s->tsys, argc, argv);
    HW_DEBUG( 
        for (size_t i = 0; i < c_args->lenUsed; i++) {
            fwrite(
                c_args->data[i].as_string->data
              , c_args->data[i].as_string->lenUsed
              , 1, stdout);
            printf(" ");
        }
    );

    hw_logp("All Ok\n");
    hw_Var args = {.as_arr = c_args};
    hw_byte tid = hw_TypeID_arr;
    hw_delete(s);
    return EXIT_SUCCESS;
}
