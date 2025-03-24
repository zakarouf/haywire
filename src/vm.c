#include "dev.h"
#include "cstd.h"

/**********************************************************************
 *                            Public
 **********************************************************************/

inline hw_FnState *hw_vm_prepare_call(hw_State *hw, hw_uint mod_id, hw_uint fn_id)
{
    hw_State_fstack_push(hw, mod_id, fn_id);
    hw_FnState *f = hw_State_fstack_top(hw);
    hw_FnInfo info;
    hw_Module const *mod = hw_Global_get_symb_via_id(hw->global, f->mod)
                                .as_module;
    hw_Module_get_FnInfo(mod, f->fn, &info);
    f->pc = mod->fnpt[f->fn];

    HW_DEBUG(HW_ASSERTEX((hw_int)(f->var - info.arg_count) >= 0
        , "Stack Underflow: %"PRIi64, (f->var - info.arg_count)));
    
    hw_State_vstack_push_mult(hw, info.stack_sz - info.arg_count);
    memcpy(hw->vstack->tid + f->var
            , info.types + info.arg_count
            , info.stack_sz - info.arg_count);

    f->var -= info.arg_count;
    return f;
}

static inline hw_FnState* hw_vm_prepare_localcall(hw_State *hw, hw_uint fn_id, hw_Module const *mod, hw_uint mod_id)
{
    hw_State_fstack_push(hw, mod_id, fn_id);
    hw_FnState *f = hw_State_fstack_top(hw);

    f->pc = mod->fnpt[f->fn];

    hw_FnInfo info;
    hw_Module_get_FnInfo(mod, f->fn, &info);

    HW_DEBUG(HW_ASSERTEX((hw_int)(f->var - info.arg_count) >= 0
        , "Stack Underflow: %"PRIi64, (f->var - info.arg_count)));

    hw_State_vstack_push_mult(hw, info.stack_sz - info.arg_count);

    memcpy(hw->vstack->tid + f->var
            , info.types + info.arg_count
            , info.stack_sz - info.arg_count);

    f->var -= info.arg_count;
    return f;
}

void hw_vm_prepare_ret(hw_State *hw)
{
    hw_FnState *f = hw_State_fstack_top(hw);
    hw_FnInfo info;
    hw_Module const *mod = hw->global->symbols->vals[f->mod].as_module;
    hw_Module_get_FnInfo(mod, f->fn, &info);
    f->var += info.mut_count;
    hw_uint const count = hw->vstack->lenUsed - f->var;

    hw_State_vstack_pop_mult(hw, count);
    hw_State_fstack_pop(hw);
}


static void _illegal_inst(hw_State *hw, hw_code const *pc)
{
    hw_byte inst = pc->get.opcode;
    if(inst < hw->global->insts_count) {
        hw_InstInfo const *ix = hw->global->insts + inst;
        hw_loglnp("error: instruction not implemented %.*s"
                , (int)ix->name_size, ix->name);
        return;
    } 
    hw_loglnp("error: illegal instruction %"PRIu8, inst);
}


void hw_vm(hw_State *hw)
{
    _L_Return: {}
    if(!hw->fstack->lenUsed) { HW_DEBUG(HW_LOG("VM DONE~~%s", "")); return;}
    hw_FnState const *f = hw_State_fstack_top(hw);

    //_L_Call: {}
    hw_Module const *mod = hw_Global_get_symb_via_id(hw->global, f->mod)
                            .as_module;

    _L_LocalCall: {}
    register hw_code const *pc = mod->code + f->pc;
    HW_DEBUG( HW_ASSERTEX(pc >= mod->code && pc < (mod->code + mod->code_len)
                    , "%zu", pc - mod->code) );

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

    #define START() while(1) {                        \
                      vm_debug(hw, mod, pc, vars);    \
                      switch (pc->get.opcode) {


    #define top(x) hw->vstack->data[hw->vstack->lenUsed-(x)-1]
    #define topt(x) hw->vstack->tid[hw->vstack->lenUsed-(x)-1]

    #define END() break; default: _illegal_inst(hw, pc); } pc++; }
  
    pc++;
    _L_Start:

    START()
        ON_INST(nop);
        ON_INST(defn);
        ON_INST(return) { hw_vm_prepare_ret(hw); goto _L_Return; };
        
        // 
        ON_INST(push) { 
            hw_State_vstack_push(hw, r(A), t(A));
            goto _L_reset_var; 
        }
        ON_INST(pop); {
                r(A) = top(0); // dup
                t(A) = topt(0);
                hw->vstack->lenUsed -= 1;
        }

        //
        ON_INST(get_type)       r(A).as_uint = t(B);

        //
        ON_INST(call) { 
            hw_State_fstack_top_save(hw, pc, vars);
            f = hw_vm_prepare_localcall(hw, x(x32), mod, f->mod); 
            goto _L_LocalCall; 
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
       ON_INST(loadknst)    { r(A) = mod->knst[x(x32)];
                              t(A) = mod->knst_t[x(x32)]; }

       ON_INST(jmp)       pc += r(A).as_int; goto _L_Start;
       ON_INST(jk)        pc += s(s32); goto _L_Start;

       ON_INST(jt)        if(r(A).as_uint) { pc += r(B).as_int; 
                                             goto _L_Start; };

       ON_INST(jtk)       if(r(A).as_uint) { pc  = (pc) + s(s32);
                                             goto _L_Start; };

       ON_INST(typeq) r(A).as_uint = t(B) == t(C);
       ON_INST(tideq) r(A).as_uint = t(B) == a(C);

      /* Maths (int) */
       ON_INST(i_add) r(A).as_int = r(B).as_int + r(C).as_int;
       ON_INST(i_sub) r(A).as_int = r(B).as_int - r(C).as_int;
       ON_INST(i_mul) r(A).as_int = r(B).as_int * r(C).as_int;
       ON_INST(i_lt)  r(A).as_int = r(B).as_int < r(C).as_int;
       ON_INST(i_le)  r(A).as_int = r(B).as_int <= r(C).as_int;
       ON_INST(i_eq)  r(A).as_int = r(B).as_int == r(C).as_int;

       /* Maths Constant */
       ON_INST(i_kadd) r(A).as_int = r(B).as_int + a(C);
       ON_INST(i_ksub) r(A).as_int = r(B).as_int - a(C);
       ON_INST(i_kmul) r(A).as_int = r(B).as_int * a(C);
       ON_INST(i_keq)  r(A).as_int = r(B).as_int == a(C);
       ON_INST(i_klt)  r(A).as_int = r(B).as_int < a(C);
       ON_INST(i_kle)  r(A).as_int = r(B).as_int <= a(C);
       ON_INST(i_kles) if(!(r(A).as_int <= s(s32))) pc++;

      /* Maths (floats) */
       ON_INST(f_add) r(A).as_float = r(B).as_float + r(C).as_float;
       ON_INST(f_mul) r(A).as_float = r(B).as_float * r(C).as_float;
       ON_INST(f_lt)  r(A).as_uint =  r(B).as_float < r(B).as_float;

       ON_INST(prnt) {
            hw_debug_print_var(hw, r(A), t(A));
       }
    END()

    #undef t
    #undef a 
    #undef r 
    #undef x 
    #undef s
    #undef top
    #undef topt
    #undef START 
    #undef END 
    #undef ON_INST
    #undef NON_INST
}


#if 0
// MAKE_ARGS(...)

#endif

