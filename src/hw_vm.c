//#include <ctime>
#include <inttypes.h>
#include <string.h>
#include "hw.h"
#include "hw_dev.h"

/**********************************************************************
 *                            Public
 **********************************************************************/

void static _fnState_print(hw_FnState *f)
{
    hw_loglnp(
        "FnState: "
        "\n     fn  -> %"PRIu64
        "\n     mod -> %"PRIu64
        "\n     pc  -> %"PRIu64
        "\n     var ->%"PRIu64
      , f->fn
      , f->mod
      , f->pc
      , f->var
    );
}

void hw_vm_prepare_call(hw_State *hw, hw_uint mod_id, hw_uint fn_id)
{
    hw_State_fstack_push(hw, mod_id, fn_id);
    hw_FnState *f = hw_State_fstack_top(hw);
    hw_FnInfo info;
    hw_Module const *mod = hw_Global_get_module(hw->global, f->mod);
    hw_Module_get_FnInfo(mod, f->fn, &info);
    f->pc = mod->fnpt[f->fn];

    HW_DEBUG(HW_ASSERTEX((hw_int)(f->var - info.arg_count) >= 0
        , "Stack Underflow: %"PRIi64, (f->var - info.arg_count)));
    
    hw_State_vstack_push_mult(hw, info.stack_sz - info.arg_count);
    memcpy(hw->vstack->tid + f->var
            , info.types + info.arg_count
            , info.stack_sz - info.arg_count);

    f->var -= info.arg_count;
}

void hw_vm_prepare_ret(hw_State *hw)
{
    hw_FnState *f = hw_State_fstack_top(hw);
    hw_FnInfo info;
    hw_Module const *mod = hw_Global_get_module(hw->global, f->mod);
    hw_Module_get_FnInfo(mod, f->fn, &info);
    f->var += info.mut_count;
    hw_uint const count = hw->vstack->lenUsed - f->var;

    hw_State_vstack_pop_mult_dtor(hw, count);
    hw_State_fstack_pop(hw);
}

static void _illegal_inst(hw_State *hw, hw_code const *pc)
{
    hw_byte inst = pc->get.opcode;
    if(inst < hw->global->insts_count) {
        hw_InstData const *ix = hw->global->insts + inst;
        hw_loglnp("error: instruction not implemented %.*s"
                , (int)ix->name_size, ix->name);
        return;
    } 
    hw_loglnp("error: illegal instruction %"PRIu8, inst);
}


void hw_vm(hw_State *hw)
{
    _L_again: {}
    if(!hw->fstack->lenUsed) { HW_DEBUG(HW_LOG("VM DONE~~%s", "")); return;}
    hw_FnState const *f = hw_State_fstack_top(hw);

    hw_Module const *mod = hw_Global_get_module(hw->global, f->mod);

    register hw_code const *pc = hw_Module_get_fnpc(mod, f->mod) + f->pc;

    _L_reset_var: {}
    register hw_Var* vars = hw->vstack->data + f->var;
    register hw_byte* tids = hw->vstack->tid + f->var;

    #define r(_r) (vars[pc->get._r])
    #define t(_r) (tids[pc->get._r])
    #define a(_r) (pc->get._r)
    #define x(_r) (pc->getx._r)
    #define s(_r) (pc->gets._r)

    #define HOOK_r(_r) (r##_r##x = r(_r))
    #define HOOK_x32() (x32 = vars[pc->get.r])

    //#define HW_DEBUG_VM_STEP 1
    #if defined (HW_DEBUG_VM_STEP) && HW_DEBUG_VM_STEP == 1
        #define vm_debug(hw, m, pc, v)\
                    hw_debug_code_disasm(hw, *pc); \
                    hw_debug_vm_step(hw, m, pc, v);
    #endif

    #if defined (vm_debug)
    #else
        #define vm_debug(hw, m, pc, v) 
    #endif

    #define ON_INST(x) break; case hw_Inst_##x:
    #define NON_INST(x)\
        ON_INST(x) HW_ASSERT(0 && " INSTRUCTION :"#x" NOT IMPLEMENTED YET");

    #define START() while(1) {                        \
                      vm_debug(hw, mod, pc, vars);    \
                      switch (pc->get.opcode) {

    #define top(x) hw->vstack->data[hw->vstack->lenUsed-(x)-1]
    #define topt(x) hw->vstack->tid[hw->vstack->lenUsed-(x)-1]

    #define END() break; default: _illegal_inst(hw, pc); } pc++; }
  
    pc++;
    START()
        ON_INST(nop);
        ON_INST(defn);
        ON_INST(return) { hw_vm_prepare_ret(hw); goto _L_again; };
        
        // 
        ON_INST(push) { 
            hw_State_vstack_push(hw, r(A), t(A));
            goto _L_reset_var; 
        }
        ON_INST(pop); {
                r(A) = top(0); // dup
                hw_State_vstack_pop_mult_dtor(hw, 1);
        }

        //
        ON_INST(get_type)       r(A).as_uint = t(B);

        //
        ON_INST(call) { 
            hw_State_fstack_top_save(hw, pc, vars);
            hw_vm_prepare_call(hw, f->mod, x(x32)); 
            goto _L_again; 
        }
       
        //
        ON_INST(top)     { 
            r(A) = top(x(x32));
            t(A) = topt(x(x32));
        };
        ON_INST(dup)     { r(A) = r(B); t(A) = t(B); }
        ON_INST(dups)    { memmove(&r(A), &r(B), a(C) * sizeof(r(A)));
                          memmove(&t(A), &t(B), a(C) * sizeof(t(A))); }

        ON_INST(type)      memset(&t(A), a(C), a(B));


       ON_INST(loada32)     r(A).as_uint = x(x32);

       ON_INST(jmp)       pc += r(A).as_int;
       ON_INST(jk)        pc += s(s32);
       ON_INST(jt)        if(r(A).as_uint) { pc += r(B).as_int; };
       ON_INST(jtk)       if(r(A).as_uint) { pc  = (pc) + s(s32); };

       ON_INST(typeq) r(A).as_uint = t(B) == t(C);
       ON_INST(tideq) r(A).as_uint = t(B) == a(C);

      /* Maths (int) */
       ON_INST(i_add) r(A).as_int = r(B).as_int + r(C).as_int;
       ON_INST(i_sub) r(A).as_int = r(B).as_int - r(C).as_int;
       ON_INST(i_mul) r(A).as_int = r(B).as_int * r(C).as_int;
       ON_INST(i_lt)  r(A).as_int = r(B).as_int < r(C).as_int;
       ON_INST(i_le)  r(A).as_int = r(B).as_int <= r(C).as_int;
       ON_INST(i_eq)  r(A).as_int = r(B).as_int == r(C).as_int;

      /* Maths (floats) */
       ON_INST(f_add) r(A).as_float = r(B).as_float + r(C).as_float;
       ON_INST(f_mul) r(A).as_float = r(B).as_float * r(C).as_float;
       ON_INST(f_lt)  r(A).as_uint =  r(B).as_float < r(B).as_float;

       ON_INST(prnt_int)  hw_logp("%"PRIi64, r(A).as_int);
       ON_INST(prnt_chk)  hw_logp("%c", a(A));
       ON_INST(prnt_chv)  hw_logp("%C", (wchar_t)r(A).as_uint);
    END()
}


#if 0
// MAKE_ARGS(...)

#endif

