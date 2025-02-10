#include "hw.h"
#include "hw_dev.h"

#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

/**
 * Section: Pre-Processor
 */
#define CAT2(X, Y) X##_##Y
#define CAT(X, Y) CAT2(X, Y)


#define INST(x) hw_Inst_##x
#define ID(_name, dret, it)             \
    [INST(_name)] = {                   \
        .name = #_name                  \
      , .name_size = sizeof(#_name)-1   \
      , .inst_type = hw_InstType_##it   \
      , .inst_code = INST(_name)        \
      , .no_direct = dret        \
    }

struct hw_InstData const HW_INST_DATA[] = {
    ID(nop,     HW_FALSE, abc)
  , ID(defn,    HW_TRUE, ax32) // a = mut, [x32] = data(finfo)
  , ID(return,  HW_FALSE, a)
  , ID(tailret, HW_FALSE, abc) // ret with function call
  , ID(push, HW_FALSE, ab)
  , ID(pushex, HW_FALSE, abc)
  , ID(pop, HW_FALSE, a) // release variables in stack with (optional) delete

  /* Gets */
  , ID(get_type, HW_FALSE, ab)     // R(Ax) = @typeof(R(Bx))
  , ID(get_routine, HW_FALSE, ab)  // R(Ax) = Thread(R(Bx))
  , ID(get_native, HW_FALSE, ab)
  , ID(get_vt, HW_FALSE, ab)

  /* Call */
  , ID(call,       HW_TRUE, abc)
  , ID(callm,      HW_TRUE, abc)
  , ID(calln,      HW_TRUE, abc)
  , ID(callc,      HW_TRUE, abc)

  // ------
  , ID(dup,     HW_FALSE, ab)
  , ID(dups,     HW_FALSE, abc)
  , ID(type,    HW_FALSE, ab)

  , ID(reff,    HW_FALSE, ab)
  , ID(dreff,    HW_FALSE, ab)

  , ID(loada32,     HW_FALSE, ax32)
  , ID(loadb32,     HW_FALSE, ax32)
  , ID(load,        HW_FALSE, ax32)
  , ID(loadobj,     HW_FALSE, ax32)

  , ID(list,    HW_FALSE, abc)
  , ID(unlist,  HW_FALSE, abc)
  
  /* Comaparism And Jump*/
  , ID(jmp,     HW_FALSE, a)
  , ID(jk,      HW_FALSE, x32)
  , ID(jt,      HW_FALSE, ab)
  , ID(jtk,     HW_FALSE, ax32)
  
  , ID(typeq,   HW_FALSE, abc) 
  , ID(tideq,   HW_FALSE, abc) 

  /* Maths (number) */
  , ID(i_add,  HW_FALSE, abc)
  , ID(i_sub,  HW_FALSE, abc)
  , ID(i_mul,  HW_FALSE, abc)
  , ID(i_div,  HW_FALSE, abc)
  , ID(i_mod,  HW_FALSE, abc)
  , ID(i_lt,  HW_FALSE, abc)
  , ID(i_le,  HW_FALSE, abc)
  , ID(i_eq,  HW_FALSE, abc)

  /* Maths (floats) */
  , ID(f_add,  HW_FALSE, abc)
  , ID(f_mul,  HW_FALSE, abc)
  , ID(f_lt,   HW_FALSE, abc)
  , ID(prnt_int, HW_FALSE, a)
  , ID(prnt_chk, HW_FALSE, a)
  , ID(prnt_chv, HW_FALSE, a)


  , ID(TOTAL, HW_FALSE, nop)
};

#undef INST
#undef ID


/**
 * Log & Exit
 */
void hw_logp(const char *fmt, ...)
{
    va_list arg;
    va_start(arg, fmt);
    vprintf(fmt, arg);
    va_end(arg);
}

void hw_logstr(const char *msg, size_t const size)
{
    fwrite(msg, size, 1, stdout);
    fputc('\n', stdout);
}

void hw_exit(hw_int code, const char *msg, size_t const size)
{
    hw_logstr(msg, size);
    exit(code);
}

hw_uint hw_hash_string_fnv(hw_byte const *str, hw_uint len)
{
    HW_STATIC_ASSERT(sizeof(hw_uint) == 8);
    #define FNV_OFFSET 14695981039346656037UL
    #define FNV_PRIME  1099511628211UL

    hw_uint hash = FNV_OFFSET;
    intptr_t _len = len;
    for(const hw_byte *p = str; p - str < _len; p++) {
        hash ^= (hw_uint)(*p);
        hash *= FNV_PRIME;
    }

    #undef FNV_OFFSET
    #undef FNV_PRIME
    return hash;
}

void *hw_loadfile(
    hw_State *state, char const path[], hw_uint unitsize, hw_uint *len)
{
    FILE *fp;
    if ((fp = fopen(path, "rb")) == NULL) {
        return NULL;
    }

    fseek(fp, 0, SEEK_END);
    hw_uint fsize = ftell(fp);
    *len = fsize;
    fseek(fp, 0, SEEK_SET);  /* same as rewind(f); */

    hw_uint extra = fsize/unitsize;

    void *data = HW_THREAD_ALLOC(state, fsize * extra);
    fread(data, 1, fsize, fp);

    fclose(fp);

    return data;
}

hw_uint hw_ptrcmp(void const* lhs, hw_uint lhs_size, void const* rhs, hw_uint rhs_size)
{
    if((lhs_size != rhs_size)) {return  lhs_size - rhs_size;}
    return memcmp(lhs, rhs, lhs_size);
}


/**
 * Section: Array Macro
 */
static void* _malloc_wrapper(hw_Allocator *self, size_t size)
{
    (void)self;
    return HW_MALLOC(size);
}

static void* _realloc_wrapper(hw_Allocator *self, void *ptr, size_t size)
{
    (void)self;
    return HW_REALLOC(ptr, size);
}

static void  _free_wrapper(hw_Allocator *self, void *ptr)
{
    (void)self;
    HW_FREE(ptr);
}

void hw_Allocator_default(hw_Allocator *self)
{
    self->state = NULL;
    self->alloc = _malloc_wrapper;
    self->realloc = _realloc_wrapper;
    self->free = _free_wrapper;
}

void hw_Allocator_default_delete(hw_Allocator *allocator)
{
    (void)allocator;
}

/**
 * Global
 */
#if 0
hw_State *hw_new(void)
{
    hw_Allocator allocator;
    hw_Allocator_default(&allocator);
    hw_State *self = allocator.alloc(&allocator, sizeof(*self));
  
    hw_TypeSys *ts = hw_TypeSys_default_with_allocator(allocator);

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
#endif

void hw_Module_delete(hw_State *hw, hw_Module *m)
{
    for (size_t i = 0; i < m->k_count; i++) {
        hw_Type *T = hw->ts->types + m->knst_t[i];
        HW_DEBUG(HW_ASSERT(i < hw->ts->types_total));
        if(T->is_obj) {
            hw_VarFn delete = hw_Type_getvt(T, "delete", 6);
            delete(hw, m->knst + i, m->knst_t, 1);
        }
    }
    HW_THREAD_FREE(hw, m);
}

hw_code const *hw_Module_get_fnpc(hw_Module const *m, hw_uint fn_id)
{
    return (m->code + m->fnpt[fn_id]);
}

void hw_Module_get_FnInfo(hw_Module const *mod, hw_uint fn_id, hw_FnInfo *info)
{
    hw_code pc = *hw_Module_get_fnpc(mod, fn_id);
    hw_byte *data = mod->data + pc.getx.x32;

    info->name_size = *HW_CAST(hw_uint *, HW_CAST(hw_ptr, data));
                      data += sizeof(hw_uint);
    info->mut_count = *HW_CAST(hw_uint *, HW_CAST(hw_ptr, data)); 
                      data += sizeof(hw_uint);
    info->arg_count = *HW_CAST(hw_uint *, HW_CAST(hw_ptr, data));
                      data += sizeof(hw_uint);
    info->stack_sz  = *HW_CAST(hw_uint *, HW_CAST(hw_ptr, data));
                      data += sizeof(hw_uint);

    info->name = data; 
                      data += info->name_size;

    info->types = data;
}

hw_Global *hw_Global_new(hw_State *parent)
{
    hw_Global *g = HW_THREAD_ALLOC(parent, sizeof(*g));

    hw_Var vlist;
    hw_VarList_new(parent, &vlist, (hw_byte[]){hw_TypeID_list}, 1);
    g->constants = vlist.as_list;

    g->insts = HW_INST_DATA;
    g->insts_count = hw_Inst_TOTAL;

    g->tsys = hw_TypeSys_new_default(&parent->allocator);
    g->parent = parent;

    HW_ARR_NEW(parent, g->modules.loaded, 8);

    return g;
}

void hw_Global_delete(hw_Global *g, hw_State *parent)
{
    hw_VarList_delete(g->parent, (hw_Var[]){ [0].as_list = g->constants }
            , (hw_byte[]){hw_TypeID_list}, 1);
    for (size_t i = 0; i < g->modules.loaded->lenUsed; i++) {
        hw_Module_delete(g->parent, g->modules.loaded->data[i]);
    }
    HW_ARR_DELETE(g->parent, g->modules.loaded);
    /** Always delete the type sys at the end **/
    hw_TypeSys_delete(g->tsys, &parent->allocator);
    HW_THREAD_FREE(parent, g);
}

hw_uint hw_Global_add_module(hw_Global *g, hw_Module *mod)
{
    HW_DEBUG(HW_LOG("MUTEX NOT IMPLEMENTED%s", ""));
    HW_ARR_PUSH(g->parent, g->modules.loaded, mod);
    return g->modules.loaded->lenUsed -1;
}

hw_Module* hw_Global_get_module(hw_Global const *g, hw_uint mod_id)
{
   return g->modules.loaded->data[mod_id];
}

hw_Module* hw_Global_get_module_from_name(
    hw_Global const *g, hw_byte const *name, hw_uint const name_size)
{
    hw_uint count = g->modules.loaded->lenUsed;
    for (size_t i = 0; i < count; i++) {
        hw_Module *m = g->modules.loaded->data[i];
        if(name_size == m->name_size) {
            if(!memcmp(m->data, name, name_size)) {
                return m;
            }
        }
    }
    return NULL;
}

hw_State *hw_State_new_default(hw_State *parent)
{
    hw_Allocator allocator;
    hw_Allocator_default(&allocator);
    hw_State *s = allocator.alloc(&allocator, sizeof(*s));
    s->allocator = allocator;

    s->stdin = stdin;
    s->stdout = stdout;
    
    if(NULL == parent) {
        s->global = hw_Global_new(s);
        s->ts = s->global->tsys;
        HW_DEBUG(HW_LOG("state->global not set, default setting%s", ""));
    } else {
        s->ts = parent->ts;
        s->global = parent->global;
    }

    HW_ARR_NEW(s, s->fstack, 16);

    hw_Var args[1];
    hw_byte tid[1] = {hw_TypeID_list};
    hw_VarList_new(s, args, tid, 1);
    s->vstack = args[0].as_list;

    return s;
}


void hw_State_delete(hw_State *s)
{
    hw_VarList_delete(s, (hw_Var[]){[0].as_list = s->vstack}
                       , (hw_byte[]){hw_TypeID_list}, 1);
    if(s->global->parent == s) {
        hw_Global_delete((void *)s->global, s);
    }
    HW_ARR_DELETE(s, s->fstack);
    hw_Allocator allocator = s->allocator;
    allocator.free(&allocator, s);
    hw_Allocator_default_delete(&allocator);
}

void hw_State_vstack_reserve(hw_State *hw, hw_uint const by)
{
    hw_Var args[2] = { [0].as_list = hw->vstack, [1].as_uint = by };
    hw_byte tid[2] = {hw_TypeID_list, hw_TypeID_uint};
    hw_VarList_reserve(hw, args, tid, 2);
    hw->vstack = args[0].as_list;
}

hw_uint hw_State_vstack_push_mult(hw_State *hw, const hw_uint by)
{
    if((hw->vstack->lenUsed + by) >= hw->vstack->len) {
        hw_Var args[2] = { [0].as_list = hw->vstack, [1].as_uint = by };
        hw_byte tid[2] = {hw_TypeID_list, hw_TypeID_uint};
        hw_VarList_expand(hw, args, tid, 2);
        hw->vstack = args[0].as_list;
    }
    hw->vstack->lenUsed += by;
    return hw->vstack->lenUsed - by - 1;
}

hw_uint hw_State_vstack_push(hw_State *hw, hw_Var v, hw_byte tid)
{
    hw_Var args[2] = { [0].as_list = hw->vstack, [1] = v };
    hw_byte tids[2] = {hw_TypeID_list, tid};
    hw_VarList_push_shallow(hw, args, tids, 2);
    hw->vstack = args[0].as_list;
    return hw->vstack->lenUsed - 1;
}

void hw_State_vstack_pop_mult_dtor(hw_State *hw, const hw_uint by)
{
    hw_Var *var = hw->vstack->data + hw->vstack->lenUsed - 1 - by;
    hw_byte *tid = hw->vstack->tid + hw->vstack->lenUsed - 1 - by;
    for (size_t i = 0; i < by; i++) {
        hw_Type *T = hw->ts->types + tid[i];
        if(T->is_obj) {
            hw_VarFn f = hw_Type_getvt(T, "delete", 6);
            f(hw, var + i, tid + i, 1);
        }
    }
    
    hw->vstack->lenUsed -= by;
}

void hw_State_fstack_push(
    hw_State *s, hw_uint const mod_id, hw_uint const fn_id)
{
    HW_ARR_PUSH(s, s->fstack, ((hw_FnState){
                                    .fn = fn_id
                                  , .mod = mod_id
                                  , .pc = 0
                                  , .var = s->vstack->lenUsed
                                }));
}

void hw_State_fstack_pop(hw_State *hw)
{
    HW_DEBUG(HW_ASSERT(hw->fstack->lenUsed));
    hw->fstack->lenUsed -= 1;
}

hw_FnState* hw_State_fstack_top(hw_State *hw)
{
    return hw->fstack->data +(hw->fstack->lenUsed-1);
}

void hw_State_fstack_save(hw_State *hw, hw_code const *pc, hw_Var *var)
{
    hw_FnState *f = hw_State_fstack_top(hw);
    hw_Module const *m = hw_Global_get_module(hw->global, f->mod);
    f->pc = pc - hw_Module_get_fnpc(m, f->fn);
    f->var = var - hw->vstack->data;
}



