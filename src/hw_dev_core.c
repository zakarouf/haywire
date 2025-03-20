#include "hw.h"
#include "hw_dev.h"

#include <inttypes.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <threads.h>

/**
 * Section: Pre-Processor
 */


#define INST(x) hw_Inst_##x
#define ID(_name, dret, it, _brief)     \
    [INST(_name)] = {                   \
        .name = #_name                  \
      , .name_size = sizeof(#_name)-1   \
      , .inst_type = hw_InstType_##it   \
      , .inst_code = INST(_name)        \
      , .no_direct = dret               \
      , .brief = (void *)_brief         \
      , .brief_size = sizeof(_brief)-1  \
    }

static struct hw_InstInfo const INSTRUCTION_INFO[] = {
    ID(nop,     HW_FALSE, abc,  "NOP")
  , ID(defn,    HW_TRUE, ax32,  "a = mut, [x32] = data(finfo)")
  , ID(return,  HW_FALSE, nop,  "return call")
  , ID(tailret, HW_FALSE, abc,  "ret with function call")
  , ID(push,    HW_FALSE, a,    "push r(A) to stack")
  , ID(pushex,  HW_FALSE, abc,  "push r(A)..r(B) to stack")
  , ID(pop,     HW_FALSE, ab,   "release top variables in stack"
                                ", copy to stack variable a, if b is true")
  , ID(popex,   HW_FALSE, abc,  "")

  /* Heap Memory */
  , ID(alloc,   HW_FALSE, ax32,  "Allocate memory of size r(x32) on heap"
                                ", set r(A) as the pointer")

  , ID(allock,  HW_FALSE, ax32, "Allocate memory of size x32 on heap"
                                ", set r(A) as the pointer")

  , ID(realloc, HW_FALSE, ax32, "ReAllocate memory of ptr r(A)"
                                "to size r(B) on heap"
                                ", set r(A) as the pointer")

  , ID(free,    HW_FALSE, x32,  "Free memory of ptr r(x32)")

  /* Gets */
  , ID(get_type,    HW_FALSE, ab, "R(Ax) = @typeof(R(Bx))") 
  , ID(get_routine, HW_FALSE, ab, "R(Ax) = Thread(R(Bx))")  // 
  , ID(get_native,  HW_FALSE, ab, "")
  , ID(get_vt,      HW_FALSE, ab, "")

  /* Call */
  , ID(call,          HW_FALSE, x32,  "")
  , ID(call_mod,      HW_FALSE, ax32,   "")
  , ID(call_native,   HW_FALSE, ax32,  "")
  , ID(call_c,        HW_FALSE, ax32,  "")
  , ID(call_sym,      HW_FALSE, ax32,  "")

  // ------
  , ID(top,     HW_FALSE, ax32, "")
  , ID(dup,     HW_FALSE, ab,   "")
  , ID(dups,    HW_FALSE, abc,  "")
  , ID(type,    HW_FALSE, ab,   "")

  , ID(reff,    HW_FALSE, ab,   "")
  , ID(dreff,   HW_FALSE, ab,   "")

  , ID(loada32,     HW_FALSE, ax32, "")
  , ID(loadb32,     HW_FALSE, ax32, "")
  , ID(load,        HW_FALSE, ax32, "")
  , ID(loadknst,    HW_FALSE, ax32, "")
  , ID(loadobj,     HW_FALSE, ax32, "")

  , ID(list,    HW_FALSE, abc, "")
  , ID(unlist,  HW_FALSE, abc, "")
  
  /* Comaparism And Jump*/
  , ID(jmp,     HW_FALSE, a,    "")
  , ID(jk,      HW_FALSE, x32,  "")
  , ID(jt,      HW_FALSE, ab,   "")
  , ID(jtk,     HW_FALSE, ax32, "")
  
  , ID(typeq,   HW_FALSE, abc, "")
  , ID(tideq,   HW_FALSE, abc, "")

  /* Maths (number) */
  , ID(i_add, HW_FALSE, abc, "")
  , ID(i_sub, HW_FALSE, abc, "")
  , ID(i_mul, HW_FALSE, abc, "")
  , ID(i_div, HW_FALSE, abc, "")
  , ID(i_mod, HW_FALSE, abc, "")
  , ID(i_lt,  HW_FALSE, abc, "")
  , ID(i_le,  HW_FALSE, abc, "")
  , ID(i_eq,  HW_FALSE, abc, "")

  , ID(i_kadd,  HW_FALSE, abc, "")
  , ID(i_ksub,  HW_FALSE, abc, "")
  , ID(i_kmul,  HW_FALSE, abc, "")
  , ID(i_kdiv,  HW_FALSE, abc, "")
  , ID(i_kmod,  HW_FALSE, abc, "")
  , ID(i_klt,  HW_FALSE, abc, "")
  , ID(i_kle,  HW_FALSE, abc, "")
  , ID(i_keq,  HW_FALSE, abc, "")

  /* Maths (floats) */
  , ID(f_add,  HW_FALSE, abc, "")
  , ID(f_mul,  HW_FALSE, abc, "")
  , ID(f_lt,   HW_FALSE, abc, "")

  , ID(prnt, HW_FALSE, a, "")
  , ID(TOTAL, HW_FALSE, nop, "")
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

inline hw_bool hw_byteArr_loadinc(
    hw_byteArr const *stream, hw_uint* index
  , hw_ptr dest, hw_uint const dest_size)
{
    if((*index + dest_size) > stream->lenUsed) return 0;
    memcpy(dest, stream->data + *index, dest_size);
    *index += dest_size;
    return 1;
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

hw_bool hw_writefile(char const path[], void *data, hw_u32 unitsize, hw_u32 len)
{
    FILE *fp;
    if ((fp = fopen(path, "wb")) == NULL) {
        return HW_FALSE;
    }
    fwrite(data, unitsize, len, fp);
    return HW_FALSE;
}

hw_byteArr *hw_byteArr_newloadfile(hw_State *hw, const char path[])
{
    FILE *fp;
    if ((fp = fopen(path, "rb")) == NULL) {
        return NULL;
    }

    fseek(fp, 0, SEEK_END);
    hw_uint fsize = ftell(fp);
    fseek(fp, 0, SEEK_SET);  /* same as rewind(f); */

    hw_byteArr *self;
    HW_ARR_NEW(hw, self, fsize + 1);
    fread(self->data, 1, fsize, fp);
    fclose(fp);

    self->lenUsed = fsize;
    return self;
}

hw_uint hw_ptrcmp(void const* lhs, hw_uint lhs_size, void const* rhs, hw_uint rhs_size)
{
    if((lhs_size != rhs_size)) {return  lhs_size - rhs_size;}
    return memcmp(lhs, rhs, lhs_size);
}

void hw_String_trim(hw_String *str, hw_byte ch)
{
    hw_u32 cur = str->lenUsed-1;
    while(cur) {
        if(str->data[cur] == ch) str->lenUsed = cur;
        cur -= 1;
    }
}

hw_i32 hw_strto_int(hw_int *ret, hw_byte const *str, hw_u32 size)
{
    hw_int result = 0;
    hw_int const sign = (*str == '-')? -1:1;
    hw_byte const *end = str + size;

    while (str < end && hw_char_is_num(*str)) {
        hw_byte digit = *str - '0';
        if(result > (HW_INT_MAX - digit)) {
            *ret = sign * HW_INT_MAX;
            return end - str;
        }
        result = (result * 10) + (digit);
        str++;
    }

    *ret = sign * result;
    return end - str;
}

hw_i32 hw_strto_uint(hw_uint *ret, hw_byte const *str, hw_u32 size)
{
    hw_uint result = 0;
    hw_byte const *end = str + size;

    while (str < end && hw_char_is_num(*str)) {
        hw_byte digit = *str - '0';
        if(result > (HW_UINT_MAX - digit)) {
            return end - str;
        }
        result = (result * 10) + (digit);
        str++;
    }
    
    *ret = result;
    return end - str;
}

hw_i32 hw_strto_float(hw_float *ret, hw_byte const *str, hw_u32 size)
{
    hw_float result = 0.0;
    hw_int sign = 1;
    if(*str == '-') { sign = -1; str++; }

    hw_byte const *end = str + size;
    
    // Integer Part
    while( str < end && hw_char_is_num(*str) ) {
        hw_float digit = *str - '0';
        if(result > (HW_FLOAT_MAX - digit)) {
            *ret = sign * HW_FLOAT_MAX;
            return end - str;
        }
        result = (result * 10.0) + (digit);
        str++;
    }

    if(*str == '.') {
        hw_float fraction = 0.0;
        while( str < end && hw_char_is_num(*str) ) {
            hw_float digit = *str - '0';
            fraction = (fraction * 10.0) + (digit);
            str++;
        }
        result += (1.0/fraction);
    }
    
    if(*str == 'e' || *str == 'E') {
        str++;
        hw_uint exp = 0;
        hw_bool exp_sign = 1;
        if(str < end && *str == '-') {
            str ++;
            exp_sign = 0;
        }

        hw_u32 const left = hw_strto_uint(&exp, str, end - str);
        str += left;
        hw_float power = 1.0f;
        for (size_t i = 0; i < exp; i++) {
            power *= 10.0;
        }

        if(exp_sign) { result *= power; }
        else         { result /= power; }

    }

    *ret = sign * result;
    return end - str;
}

hw_String *hw_stripfile_path_ext(hw_State *hw, hw_byte const *file_name
                                             , hw_u32 file_namesize)
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

/**
 * Section: Array Macro
 */


HW_DEBUG(
    static thread_local hw_uint total_alloc_size = 0;
    static thread_local hw_uint total_alloc_call = 0;
)
static void* _gpa_malloc(hw_Allocator *self, size_t size)
{
    (void)self;
    HW_DEBUG(
      total_alloc_call += 1;
      total_alloc_size += size;
    );
    return HW_MALLOC(size);
}

static void* _gpa_realloc(hw_Allocator *self, void *ptr, size_t size)
{
    (void)self;
    return HW_REALLOC(ptr, size);
}

static void  _gpa_free(hw_Allocator *self, void *ptr)
{
    (void)self;
    HW_FREE(ptr);
}

void hw_Allocator_gpa_delete(hw_Allocator *allocator)
{
    (void)allocator;
    
    HW_DEBUG(
        hw_float alloc_size_kb = (hw_float)total_alloc_size/1000.0;
        HW_LOG("MEMORY USAGE: \n"
                    "  Alloc:%lf KBs\n"
                    "  Calls:%"PRIu64 "\n"
                    , alloc_size_kb, total_alloc_call));
}

void hw_Allocator_new_gpa(hw_Allocator *self)
{
    self->state.as_ptr = NULL;
    self->alloc = _gpa_malloc;
    self->realloc = _gpa_realloc;
    self->free = _gpa_free;
    self->delete = hw_Allocator_gpa_delete;
}

hw_ArenaRegion *hw_ArenaRegion_new(hw_u32 max_capacity)
{
    hw_ArenaRegion *region = HW_MALLOC(sizeof(*region) + max_capacity + sizeof(hw_u32));
    memset(region->frags, 0, sizeof(region->frags));
    region->pool = HW_CAST(void *, region + 1);
    HW_CAST_SET(hw_u32, region->pool, 0, 0);
    region->next = NULL;
    region->capacity = max_capacity;
    region->used = sizeof(hw_u32);
    return region;
}

inline static hw_ptr hw_ArenaRegion_setalloc(hw_byte *pool, hw_u32 size)
{
    HW_CAST_SET(hw_u32, pool, 0, size);
    return pool + sizeof(hw_u32);
}

inline static hw_u32 hw_ArenaRegion_getptr_size(hw_byte *ptr)
{
    return *HW_CAST(hw_u32 *, ptr - sizeof(hw_u32));
}

hw_ptr hw_ArenaRegion_alloc(hw_ArenaRegion *region, hw_u32 size)
{
    #if 0
    for (size_t i = 0; i < HW_ARENAREGION_FRAG_COUNT; i++) {
        if(hw_ArenaRegion_getptr_size(region->pool 
                                    + region->frags[i]) > size) {
            hw_ptr ptr = region->pool + region->frags[i];
            region->frags[i] = 0;
            return ptr;
        }
    }
    #endif
    hw_uint total_size = size + sizeof(hw_u32);
    if((region->capacity - region->used) < (total_size)) return NULL;
    hw_ptr ptr = hw_ArenaRegion_setalloc(region->pool + region->used, size);
    region->used += total_size;
    return ptr;
}

/*
hw_bool hw_ArenaRegion_free(hw_ArenaRegion *region, hw_ptr ptr)
{
    hw_int pool_index = (hw_byte *)ptr - region->pool;
}

void hw_ArenaRegion_realloc(hw_ArenaRegion *region
                          , hw_ptr ptr, hw_u32 new_size) {
}
*/

hw_bool hw_ArenaRegion_check(hw_ArenaRegion *r)
{
    if(!r->capacity) return 0;
    if(r->used > r->capacity) return 0;
    if(!r->pool) return 0;

    return 1;
}

hw_Arena *hw_Arena_new(hw_u32 pool_capacity)
{
    hw_ArenaRegion *reg = hw_ArenaRegion_new(pool_capacity + sizeof(hw_Arena));
    hw_Arena *arena = HW_CAST(void *, reg->pool);
    reg->used += sizeof(*arena);

    arena->begin = reg;
    arena->at = reg;
    arena->end = reg;
    arena->default_size = pool_capacity;

    return arena;
}

void hw_Arena_delete(hw_Arena *arena)
{
    hw_ArenaRegion *reg = arena->begin;
    hw_ArenaRegion *reg_next = reg->next;
    HW_FREE(reg);
    reg = reg_next;
    while(reg) {
        reg->next = reg;
        HW_FREE(reg);
        reg = reg->next;
    }
}

void *hw_Arena_alloc(hw_Arena *arena, hw_u32 size)
{
    hw_ptr ptr = hw_ArenaRegion_alloc(arena->end, size);
    if(ptr) return ptr;
    hw_ArenaRegion *region = hw_ArenaRegion_new(arena->default_size > size
                                               ?arena->default_size : size);
    arena->end->next = region;
    arena->end = region;
    return hw_ArenaRegion_alloc(region, size);
}

hw_u32 hw_Arena_total_used(hw_Arena *arena)
{
    hw_ArenaRegion *r = arena->begin;
    hw_u32 total = r->used;
    r = r->next;
    while(r != NULL) {
        total += r->used;
        r = r->next;
    }
    return total;
}

hw_u32 hw_Arena_total(hw_Arena *arena)
{
    hw_ArenaRegion *r = arena->begin;
    hw_u32 total = r->capacity;
    r = r->next;
    while(r != NULL) {
        total += r->capacity;
        r = r->next;
    }
    return total;
}

hw_bool hw_Arena_check(hw_Arena *arena)
{
    if(!arena) return 0;
    hw_bool line_check = 0;

    hw_ArenaRegion *r = arena->begin;
    if(!hw_ArenaRegion_check(r)) return 0;

    r = r->next;
    while(r != NULL) {
        if(!hw_ArenaRegion_check(r)) return 0;
        r = r->next;
        if(r == arena->end) {
            if(r->next == NULL) {
                line_check = 1;
            }
        }
    }

    return line_check;
}

static void* _arena_wrap_alloc(hw_Allocator *self, size_t size)
{
    return hw_Arena_alloc(self->state.as_ptr, size);
}

static void _arena_wrap_free(hw_Allocator *self, void *p)
{
    (void)self;
    (void)p;
}

static void *_arena_wrap_realloc(hw_Allocator *self, void *p, size_t new_size)
{
    hw_ptr new_p = hw_Arena_alloc(self->state.as_ptr, new_size);
    hw_u32 size = hw_ArenaRegion_getptr_size(p);
    memcpy(new_p, p, size);
    return new_p;
}

void hw_Allocator_arena_delete(hw_Allocator *self)
{
    hw_Arena_delete(self->state.as_ptr);
}

void hw_Allocator_new_arena(hw_Allocator *self)
{
    self->state.as_ptr = hw_Arena_new(1000 * 1000 * 10);
    self->alloc = _arena_wrap_alloc;
    self->realloc = _arena_wrap_realloc;
    self->free = _arena_wrap_free;
    self->delete = hw_Allocator_arena_delete;
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


inline hw_uint hw_Module_calcsize(hw_Module *m)
{
    return HW_MODULE_SIZE(m, m->fn_count, m->code_len, m->data_size, m->k_count);
}

hw_Module *hw_Module_newblank(
    hw_State *hw, hw_u32 fn_count, hw_u32 code_len, hw_u32 data_size, hw_u32 knst_count, hw_bool set_0)
{
    hw_Module *m = NULL;
    hw_uint const mod_size = HW_MODULE_SIZE(
        m, fn_count, code_len, data_size, knst_count);

    m = HW_THREAD_ALLOC(hw, mod_size);
    m->fn_count = fn_count;
    m->code_len = code_len;
    m->data_size = data_size;
    m->k_count = knst_count;

    // NOTE: If you change this order
    //       You have to also change how serialize and deserialize works
    m->fnpt = HW_CAST(void *, m + 1);
    m->code = HW_CAST(void *, m->fnpt + fn_count );
    m->data = HW_CAST(void *, m->code + code_len);
    m->knst_t = HW_CAST(void *, m->data + data_size);
    m->knst = HW_CAST(void *, m->knst_t + knst_count );

    if(set_0) memset(m + 1, 0, mod_size - sizeof(*m));

    return m;
}

void hw_Module_delete_detatch_knstobj(hw_State *hw, hw_Module *m)
{
    HW_THREAD_FREE(hw, m);
}

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
    hw_Module_delete_detatch_knstobj(hw, m);
}

hw_code const *hw_Module_get_fnpc(hw_Module const *m, hw_uint fn_id)
{
    HW_DEBUG(HW_ASSERT_OP(fn_id, <, m->fn_count, PRIu64, PRIu64));
    HW_DEBUG(HW_ASSERT_OP(m->fnpt[fn_id], <, m->code_len, PRIu64, PRIu64));
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

hw_bool hw_Module_get_fn(hw_Module *m, hw_byte const *name, hw_uint name_size, hw_uint *fn_id)
{
    for (size_t i = 0; i < m->fn_count; i++) {
        hw_FnInfo info;
        hw_Module_get_FnInfo(m, i, &info);
        HW_DEBUG(HW_LOG("FNINFO: %.*s ~= %.*s", (int)name_size, name, (int)info.name_size, info.name));
        if(name_size == info.name_size) {
            if(!memcmp(name, info.name, name_size)) {
                *fn_id = i;
                return 1;
            }
        }
    }
    return 0;
}

hw_Global *hw_Global_new(hw_State *parent)
{
    hw_Global *g = HW_THREAD_ALLOC(parent, sizeof(*g));

    hw_Var vlist;
    hw_VarList_new(parent, &vlist, (hw_byte[]){hw_TypeID_list}, 1);
    g->constants = vlist.as_list;

    g->insts = INSTRUCTION_INFO;
    g->insts_count = hw_Inst_TOTAL;

    g->tsys = hw_TypeSys_new_default(&parent->allocator);
    g->parent = parent;

    HW_ARR_NEW(parent, g->builtin, 8);

//    hw_SymTable_new(parent, &vlist, (hw_byte[]){hw_TypeID_symtable}, 1);
//    g->builtin_names = vlist.as_symtable;

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
    
    HW_ARR_DELETE(g->parent, g->builtin);
//    hw_SymTable_delete(g->parent, &(hw_Var){ 
//        .as_symtable = g->builtin_names}, (hw_byte[]){hw_TypeID_symtable}, 1);
  
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
    (void)g;
    (void)name;
    (void)name_size;
    HW_ASSERT(0 && "NOT IMPLEMENTED, HINT: USE HASHTABLE, MOD NAMES ARE DECOUPLED");
    return NULL;
}

hw_uint hw_Global_set_builtin(hw_Global *g, hw_VarFn fn, hw_CStr const name)
{
    hw_SymTable_set__wrap(
        g->parent, &g->builtin_names, name.data, name.len
        , (hw_Var){.as_uint = g->builtin->lenUsed}, hw_TypeID_uint); 
    HW_ARR_PUSH(g->parent, g->builtin, fn);
    return g->builtin->lenUsed-1;
}

hw_VarFn hw_Global_get_builtin(hw_Global *g, hw_CStr const name)
{
    hw_uint index = hw_SymTable_index(g->builtin_names, name.data, name.len);
    return g->builtin_names->key[index] == NULL? hw_VarFn_UNREACHABLE: g->builtin->data[index];
}

hw_State *hw_State_new_default(hw_State *parent)
{
    hw_Allocator allocator;
    hw_Allocator_new_gpa(&allocator);
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
    allocator.delete(&allocator);
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
    HW_DEBUG(HW_ASSERT_OP(by, <=, hw->vstack->lenUsed, PRIu64, PRIu32));
    hw->vstack->lenUsed -= by;

    hw_Var *vars = hw->vstack->data + hw->vstack->lenUsed;
    hw_byte *tid = hw->vstack->tid  + hw->vstack->lenUsed;
    for (size_t i = 0; i < by; i++) {
        hw_Type *T = hw->ts->types + tid[i];
        HW_DEBUG(HW_ASSERTEX(tid[i] < hw_TypeID_TOTAL
              , "var %"PRIu64 "\n"
                "iteration %"PRIu64 "\n"
                "by %"PRIu64 "\n"
                "tid %"PRIu8
              , hw->vstack->lenUsed - by + i
              , i, by, tid[i]));
        if(T->is_obj) {
            hw_VarFn f = hw_Type_getvt(T, "delete", 6);
            f(hw, vars + i, tid + i, 1);
        }
    }
}

inline void hw_State_vstack_pop_mult(hw_State *hw, const hw_uint by)
{
    hw->vstack->lenUsed -= by;
}

inline void hw_State_fstack_push(
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

void hw_State_fstack_top_save(hw_State *hw, hw_code const *pc, hw_Var const *var)
{
    hw_FnState *f = hw_State_fstack_top(hw);
    hw_Module const *m = hw_Global_get_module(hw->global, f->mod);
    f->pc = pc - m->code;
    f->var = var - hw->vstack->data;
}


