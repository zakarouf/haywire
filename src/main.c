#include "dev.h"
#include "cstd.h"

#ifdef HW_COMPILE_SINGLE
#include "hw_debug.c"
#include "hw_dev_core.c"
#include "hw_dev_lexer.c"
#include "hw_dev_types.c"
#include "hw_compbc.c"
#include "hw_vm.c"
#endif

static void _check_vm_inst(hw_State const *hw)
{
    hw_Global const *global = hw->global;
    size_t i;
    for (i = 0; i < global->insts_count; i++) {
        HW_DEBUG( 
          hw_logp("%"PRIu64 "->", i);
          hw_logstr(global->insts[i].name, global->insts[i].name_size);
          putc(' ', stdout);
        )
        HW_ASSERTEX(global->insts[i].name, "%"PRIu64"", i);
        HW_ASSERTEX(global->insts[i].name_size, "%"PRIu64, i);
    }
}

static hw_VarArr *_wrap_args(hw_State *s, int argc, char *argv[])
{
    hw_Var arr;
    hw_Var args[4] = {
                {0}
              , (hw_Var){.as_uint = hw_TypeID_string} };

    hw_VarArr_newFrom_conf( s, args 
      , (hw_byte[]){hw_TypeID_array, hw_TypeID_uint}, 2);
    arr = args[0];

    for (int i = 0; i < argc; i++) {
        hw_Var string;

        args[1].as_ptr = argv[i];
        args[2].as_uint = strlen(argv[i]) + 1;
        hw_String_newFrom_data(s, args
            , (hw_byte[]){
            hw_TypeID_nil, 
            hw_TypeID_ptr, 
            hw_TypeID_uint}, 3);

        string = args[0];
        string.as_string->lenUsed -= 1;
        string.as_string->data[string.as_string->lenUsed] = '\0';
      
        args[0] = arr;
        args[1] = string;
        hw_VarArr_push(s, args, (hw_byte[]){
              hw_TypeID_array,
              hw_TypeID_string}, 2);

        arr = args[0];
    }
    
    HW_DEBUG(
        HW_LOG("ARGS: %"PRIu32, arr.as_arr->lenUsed);
        for (size_t i = 0; i < arr.as_arr->lenUsed; i++) {
            hw_debug_print_var(s, arr.as_arr->data[i]
                    , arr.as_arr->tid);
            hw_logp("\n");
        }
    );


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

hw_CStr hw_get_token_name(hw_uint token_type);

void print_tokens(hw_Lexer lexer)
{
    while(lexer.token.type != HW_LEXTOKEN_END_OF_SOURCE) {
        hw_Lexer_next(&lexer);
        hw_CStr name = hw_get_token_name(lexer.token.type);
        printf("%p %p %lu - %.*s | %.*s", (void *)lexer.at, (void *)lexer.end
            , lexer.end - lexer.at
            , (int)lexer.token.size, lexer.token.start
            , (int)name.len, name.data);

        HW_ASSERT(lexer.token.type != HW_LEXTOKEN_ERROR);
    }
}

typedef struct hw_Config hw_Config;
struct hw_Config {
    HW_ARR(hw_String *) *files;
    HW_ARR(hw_String *) *namespaces;
    hw_String *out_file;
    hw_String *call;
    hw_VarArr *args;
    struct hw_ConfigFlags {
        hw_byte run:1
              , disasm:1
              , print_inst_info:1
              , no_namespace: 1
              ;
    } flags;
};

static hw_byte _argparse_is_arg(hw_String *arg)
{
    if(arg->lenUsed == 2) { return arg->data[0] == '-'; }
    if(arg->lenUsed > 2 
    && (arg->data[0] == '-' && arg->data[1] == '-')) {return 2;}
    return 0;
}

#define ARG() (args->data[ARG].as_string)

inline static hw_String *argnext(hw_VarArr *args, hw_uint *arg_i)
{
    *arg_i += 1;
    if(*arg_i >= args->lenUsed) { return NULL; }
    return args->data[*arg_i].as_string;
}

static hw_uint _argparse_single_char_conf(hw_State *hw,
    hw_VarArr *args, hw_Config *conf, hw_byte ch, hw_uint ARG)
{
    switch (ch) {
               case 'd': conf->flags.disasm = 1;
        break; case 'r': conf->flags.run = 1;
        break; case 'i': conf->flags.print_inst_info = 1;
        break; case 'n': conf->flags.no_namespace = 1;
        break; case 'c': conf->call = argnext(args, &ARG);
        break; case 'o': conf->out_file = argnext(args, &ARG);
        break; default: hw_loglnp("Unknown command '-%c'", ch);
    }
    return ARG;
}

static hw_uint _argparse_mult_string(
    hw_VarArr *args, hw_Config *conf, hw_uint ARG)
{
    (void)args;
    (void)conf;
    (void)ARG;
    #define argparse_start()\
        hw_String *_arg = args->data[ARG].as_string; if(0) { }
    #define argparse_end() else { hw_logp("Unknown args: %.*s"\
                                    , _arg->lenUsed, _arg->data); }
    #define argcheck(name) else if(hw_ptrcmp(_arg->data + 2             \
                                             , _arg->lenUsed - 2        \
                                             , name, sizeof(name)-1))
    argparse_start()
    argcheck("") {}
    argparse_end()
    
    return ARG + 1;
}

hw_int hw_argparse(
    hw_State *hw, hw_Config *conf, int argc, char *argv[]) {
    HW_ARR_NEW(hw, conf->files, 8);
    HW_ARR_NEW(hw, conf->namespaces, 8);
    hw_VarArr *args = _wrap_args(hw, argc, argv);
    conf->args = args;
    hw_uint ARG = 0;
    hw_String *arg = argnext(args, &ARG);
    while(arg) {
        hw_byte argtype = _argparse_is_arg(arg);
        if(argtype == 1) {
            ARG = _argparse_single_char_conf(hw, args, conf, ARG()->data[1], ARG);
        } else if(argtype == 2) {
            ARG = _argparse_mult_string(args, conf, ARG);
        } else {
            HW_ARR_PUSH(hw, conf->files, ARG());
            if(conf->flags.no_namespace) {
                HW_ARR_PUSH(hw, conf->namespaces, NULL);
            } else {
                HW_ARR_PUSH(hw, conf->namespaces
                          , hw_stripfile_path_ext(hw, ARG()->data
                                                    , ARG()->lenUsed));
            }
        }
        arg = argnext(args, &ARG);
    }
    return 0;
}
#undef ARG

void config_del(hw_State *hw, hw_Config *conf)
{
    hw_VarArr_delete(hw
        , (hw_Var[]){[0].as_arr = conf->args}, (hw_byte[]){hw_TypeID_array}, 1);
    HW_ARR_DELETE(hw, conf->files);
    for (size_t i = 0; i < conf->namespaces->lenUsed; i++) {
        if(conf->namespaces->data[i]) {
            hw_String_delete(hw, (hw_Var[]){ [0].as_string = conf->namespaces->data[i]}
                            , (hw_byte[]){ hw_TypeID_string }, 1);
        }
    }
    HW_ARR_DELETE(hw, conf->namespaces);

}

static hw_Module* _compile_files(
    hw_State *hw, hw_Config *conf, hw_uint *mod_id)
{
    hw_ModuleArr *mods;
    HW_ARR_NEW(hw, mods, 8);
    hw_compbc_compile_files(hw, &mods, conf->files->data, conf->files->lenUsed);
    hw_Module *mod = hw_Module_combine(hw, mods->lenUsed
                                         , mods->data
                                         , conf->namespaces->data);

    for (size_t i = 0; i < mods->lenUsed; i++) {
        hw_Module_delete_detatch_knstobj(hw, mods->data[i]);
    }
    HW_ARR_DELETE(hw, mods);
    *mod_id = hw_Global_add_module((void *)hw->global, mod);
    return mod;
}

#ifdef HAYWIRE_LIBRARY_IMPLEMENTATION
int hw_main(int argc, char *argv[])
#else
int main(int argc, char *argv[])
#endif
{
    hw_State *hw = hw_State_new_default(NULL);
    _static_checks();
    _check_vm_inst(hw);
    hw_Config conf = {0};
    HW_ASSERT(hw_argparse(hw, &conf, argc, argv) == 0);
    

    if(conf.flags.print_inst_info) {
        hw_debug_print_inst(hw);
    }
    if(conf.files->lenUsed) {
        hw_Module *mod = NULL;
        hw_uint mod_id = 0;
        mod = _compile_files(hw, &conf, &mod_id);

        if(conf.call != NULL) {
            hw_uint fn_id = 0;
            HW_ASSERT(hw_Module_get_fn(mod, conf.call->data, conf.call->lenUsed, &fn_id));
            hw_vm_prepare_call(hw, mod_id, fn_id);
            hw_vm(hw);       
        }
        if(conf.flags.disasm) {
            hw_debug_Module_disasm(hw, mod);
        }
        if(conf.out_file) {
            HW_VARFN(hw, hw_String_append_bytes
                , (     [0].as_string = conf.out_file
                      , [1].as_byte_p = (void *)".hwo"
                      , [2].as_uint   = 4)
                , (hw_TypeID_string)
                , conf.out_file = __args[0].as_string );

            hw_Module_writetofile(hw, mod, (void *)conf.out_file->data);
        }
    }

    config_del(hw, &conf);
    hw_State_delete(hw);
    
    return EXIT_SUCCESS;
}

