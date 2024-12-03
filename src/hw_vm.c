#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <inttypes.h>
#include "hw.h"
#include "hw_dev.h"

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
  , ID(jmps32,  HW_FALSE, x32)
  , ID(jmpif,   HW_FALSE, ax32)
  , ID(jmpifs32, HW_FALSE, ax32)
  
  , ID(typeq,   HW_FALSE, abc) 
  , ID(tideq,   HW_FALSE, abc) 
  , ID(iseq,    HW_FALSE, abc)

  /* Maths (number) */
  , ID(addn,  HW_FALSE, abc)
  , ID(muln,  HW_FALSE, abc)
  , ID(ltn,   HW_FALSE, abc)
  , ID(lten,  HW_FALSE, abc)

  /* Maths (floats) */
  , ID(addf,  HW_FALSE, abc)
  , ID(mulf,  HW_FALSE, abc)
  , ID(ltf,   HW_FALSE, abc)


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
    self->insts = HW_INST_DATA;
    self->insts_count = hw_Inst_TOTAL;
    
    HW_DEBUG(HW_LOG("Loading VM with %u Instructions", hw_Inst_TOTAL));

    hw_Thread_init(&self->main_thread, self, 0, "main", 4);  

    return self;
}

void hw_delete(hw_State *self)
{
    hw_TypeSys *ts = self->tsys;

    hw_Thread_deinit(&self->main_thread);

    HW_TYPESYS_FREE(ts, self);
    hw_TypeSys_delete(ts);
    HW_DEBUG(HW_LOG("VM Exit %s", ""));
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
       break; case hw_InstType_as32:
           fprintf(out, "a=%hu x=%u|s=%i", code.get.A
                   , code.gets.s32, code.gets.s32);
       break; case hw_InstType_s32:
           fprintf(out, "x=%u|s=%i", code.getx.x32, code.gets.s32);
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

void hw_vm(hw_State *hw, hw_Thread *th)
{
    hw_FnState f = th->f;
    register hw_code const *pc = f.pc;

    #define r(_r) (f.vars[pc->get._r])
    #define t(_r) (f.tids[pc->get._r])
    #define a(_r) (pc->get._r)
    #define x(_r) (pc->getx._r)
    #define s(_r) (pc->gets._r)

    #define HOOK_r(_r) (r##_r##x = r(_r))
    #define HOOK_x32() (x32 = f.vars[pc->get.r])

    #ifdef HW_VM_USE_COMPUTED_GOTO
    #else
        #define ON_INST(x) break; case hw_Inst_##x:
        #define NON_INST(x)\
            ON_INST(x) HW_ASSERT(0 && " INSTRUCTION :"#x" NOT IMPLEMENTED YET");
        #define START() while(1) { switch (pc->get.opcode) {
        #define END() } pc++; }
    #endif
    
    START()
       ON_INST(nop);
       ON_INST(defn);
      NON_INST(return);
      NON_INST(tailret);
      NON_INST(reserve);
      NON_INST(release);
      
       ON_INST(get_type)       r(A).as_uint = t(B);
      NON_INST(get_routine);

      NON_INST(call);
      NON_INST(calln);
      NON_INST(callc);
          
       ON_INST(dup)       r(A) = r(B);
      NON_INST(dups)    { memmove(&r(A), &r(B), a(C) * sizeof(r(A)));
                          memmove(&t(A), &t(B), a(C) * sizeof(t(A))); }
      NON_INST(type)      t(A) = r(B).as_byte;
       ON_INST(nil)       t(A) = hw_TypeID_nil;
       ON_INST(a32_0)     r(A).as_uint = x(x32);
      NON_INST(a32_1)     ;
      NON_INST(list)      ;
      NON_INST(string)    ;

       ON_INST(jmp)       pc += r(A).as_int;
       ON_INST(jmps32)    pc += s(s32);
       ON_INST(jmpif)     if(r(A).as_uint) { pc += r(B).as_int; };
       ON_INST(jmpifs32)  if(r(A).as_uint) { pc += s(s32); };

       ON_INST(typeq) r(A).as_uint = t(B) == t(C);
       ON_INST(tideq) r(A).as_uint = t(B) == a(C);
       ON_INST(iseq)  r(A).as_uint = r(B).as_uint == r(C).as_uint;

      /* Maths */
       ON_INST(addn) r(A).as_uint = r(B).as_uint + r(C).as_uint;
       ON_INST(muln) r(A).as_uint = r(B).as_uint * r(C).as_uint;
       ON_INST(ltn)  r(A).as_uint = r(B).as_uint < r(C).as_uint;
       ON_INST(lten) r(A).as_uint = r(B).as_uint <= r(C).as_uint;

      /* Maths (floats) */
       ON_INST(addf) r(A).as_float = r(B).as_float + r(C).as_float;
       ON_INST(mulf) r(A).as_float = r(B).as_float * r(C).as_float;
       ON_INST(ltf)  r(A).as_uint =  r(B).as_float < r(B).as_float;

      NON_INST(TOTAL);
    END()
}

void test_main(hw_State *hw, hw_VarArr *args)
{
    
}

int main(int argc, char *argv[])
{
    hw_State *s = hw_new();
    _static_checks();
    _check_vm_inst();

    hw_VarArr *c_args = _wrap_args(s->tsys, argc, argv);
    test_main(s, c_args);
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
    hw_delete(s);
    return EXIT_SUCCESS;
}
