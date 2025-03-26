#include "def.h"
#include "dev.h"
#include "cstd.h"
#include "hwfn.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef HW_COMPILE_SINGLE
#include "hw_debug.c"
#include "hw_dev_core.c"
#include "hw_dev_lexer.c"
#include "hw_dev_types.c"
#include "hw_compbc.c"
#include "hw_vm.c"
#endif


#define line(s) s "\n"
const hw_CStr HELP_TXT = hw_CStr_LIT(
line("A Small VM with an in-build Programing Language")
line("")
line("Usage: hw [OPTIONS] [file]... -- [vmargs]")
line("")
line("Arguments:")
line("    [file]...")
line("    The files which are to be loaded")
line("")
line("Options:")
line("    -h, --help")
line("            Show this message")
line("")
line("    -d, --disasm")
line("            Disasmble the loaded file and print it to `stdout`")
line("")
line("    -o [modname]")
line("            Write the compiled bytecode into a mod file")
line("")
line("    --ct")
line("            Enable C-Transpiler")
line("")
line("        --ct-print")
line("            Print the transpiled c code to `stdout`")
line("")
line("        --ct-compile [file]")
line("            Compile and write a shared object [file], requires c compiler")
line("")
line("        --ct-cc [bin]")
line("            Set the c compiler,")
line("            default is gcc or clang if gcc not available")
line("")
line("    --ct")
);
#undef line


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

    hwfn_VarArr_newFrom_conf( s, args 
      , (hw_byte[]){hw_TypeID_array, hw_TypeID_uint}, 2);
    arr = args[0];

    for (int i = 0; i < argc; i++) {
        hw_Var string = { .as_string = hw_String_newFrom_data(
                                              s, (void *)argv[i]
                                            , strlen(argv[i]) + 1) };
        string.as_string->lenUsed -= 1;
        string.as_string->data[string.as_string->lenUsed] = '\0';
      
        args[0] = arr;
        args[1] = string;
        hwfn_VarArr_push(s, args, (hw_byte[]){
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
    hw_String *cmod_so;
    hw_String *cmod_call;
    hw_VarArr *args;
    struct hw_ConfigFlags {
        hw_byte help:1
              , disasm:1
              , print_inst_info:1
              , no_namespace: 1
              , transpile: 1
              , cmod_run:1
              , cmod_source_print:1
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

inline static hw_String *argnext_req(hw_VarArr *args, hw_uint *arg_i)
{
    hw_String *arg = argnext(args, arg_i);
    if(arg == NULL) hw_exit(-1, HW_STR("insufficiant argument"));
    return arg;
}

static hw_uint _argparse_single_char_conf(
    hw_VarArr *args, hw_Config *conf, hw_byte ch, hw_uint ARG)
{
    switch (ch) {
               case 'd': conf->flags.disasm = 1;
        break; case 'h': conf->flags.help = 1;
        break; case 'n': conf->flags.no_namespace = 1;
        break; case 'c': conf->call = argnext_req(args, &ARG);
        break; case 'o': conf->out_file = argnext_req(args, &ARG);
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
        hw_String *_arg = args->data[ARG].as_string;\
        if(0) { printf("Unreachabke"); }

    #define argparse_end() else { hw_logp("Unknown args: `%.*s`\n"\
                                    , _arg->lenUsed, _arg->data); }

    #define argcheck(name) else if(0 == hw_ptrcmp(_arg->data + 2 \
                                             , _arg->lenUsed - 2        \
                                             , name, sizeof(name)-1))

    argparse_start()
    argcheck("help")        { conf->flags.help = 1; }
    argcheck("cmd")         { hw_cmd((void*)argnext_req(args, &ARG)->data); }
    argcheck("disasm")      { conf->flags.disasm = 1; }
    argcheck("print_inst")  { conf->flags.print_inst_info = 1; }
    argcheck("call")        { conf->call = argnext_req(args, &ARG); }
    argcheck("ct")          { conf->flags.transpile = 1; }
    argcheck("ct-print")    { conf->flags.cmod_source_print = 1; }
    argcheck("ct-compile")  { conf->cmod_so = argnext_req(args, &ARG); }
    argcheck("ct-call")     { conf->cmod_call = argnext_req(args, &ARG); }
    argparse_end()
    
    return ARG;
}

hw_int hw_argparse(
    hw_State *hw, hw_Config *conf, int argc, char *argv[]) {
    HW_ARR_NEW(hw, conf->files, 8);
    HW_ARR_NEW(hw, conf->namespaces, 8);
    conf->cmod_so = NULL;
    conf->cmod_call = NULL;
    hw_VarArr *args = _wrap_args(hw, argc, argv);
    conf->args = args;
    hw_uint ARG = 0;
    hw_String *arg = argnext(args, &ARG);
    while(arg) {
        hw_byte argtype = _argparse_is_arg(arg);
        if(argtype == 1) {
            ARG = _argparse_single_char_conf(args, conf, ARG()->data[1], ARG);
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
    hwfn_VarArr_delete(hw
        , (hw_Var[]){[0].as_arr = conf->args}, (hw_byte[]){hw_TypeID_array}, 1);
    HW_ARR_DELETE(hw, conf->files);
    for (size_t i = 0; i < conf->namespaces->lenUsed; i++) {
        if(conf->namespaces->data[i]) {
            hwfn_String_delete(hw, (hw_Var[]){ [0].as_string = conf->namespaces->data[i]}
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
    *mod_id = hw_Global_add_anonsymb(
            (void *)hw->global, (hw_Var){.as_module = mod}, hw_TypeID_module);
    return mod;
}


void hw_test_check_symtableord(hw_State *hw, hw_u32 count)
{
    hw_SymTableOrd *table = hw_SymTableOrd_new(hw, 64);
    char buffer[64] = {[63] = 0};
    for (size_t i = 0; i < count; i++) {
        snprintf(buffer, 63, "symbols_%lu", i);
        hw_SymTableOrd_set(hw, table,(hw_byte*)buffer, strnlen(buffer, 63)
                            , (hw_Var){.as_uint = i}, hw_TypeID_int);
    }
    hw_bool *instances = HW_THREAD_ALLOC(hw, sizeof(hw_bool) * count);
    memset(instances, 0, sizeof(*instances) * count);
    for (size_t i = 0; i < table->len; i++) {
        hw_u32 index = table->indices[i];
        if(index < table->vlenUsed) {
            HW_ASSERT(instances[index] == 0);
            instances[index] = 1;
        }
    }
    for (size_t i = 0; i < count; i++) {
        HW_ASSERT(instances[i]);
    }
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

    if(conf.flags.help) {
        fputs((void *)HELP_TXT.data, hw->stdout);
        goto _L_early_exit;
    }

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
            HW_VARFN(hw, hwfn_String_append_bytes
                , (     [0].as_string = conf.out_file
                      , [1].as_byte_p = (void *)".hwo"
                      , [2].as_uint   = 4)
                , (hw_TypeID_string)
                , conf.out_file = __args[0].as_string );

            hw_Module_writetofile(hw, mod, (void *)conf.out_file->data);
        }

        if(conf.flags.transpile) {
            hw_String *c_code = hw_compc_mod_to_c(hw, mod);

            if(conf.flags.cmod_source_print) {
                hw_debug_print_var(hw, (hw_Var){.as_string = c_code}
                                     , hw_TypeID_string);
            }

            if(conf.cmod_so) {
                char const *file = "tmp-xyW2.c"; 
                FILE *fp = fopen(file, "wb");
                fwrite(c_code->data, c_code->lenUsed, 1, fp);
                fflush(fp);
                fclose(fp);

                hw_String *cmd = hw_String_new(hw, 64);
                hw_String_append_fmt(hw, &cmd, 
                        "clang -std=c99 -O3 -Wall -Wextra -fPIC -shared %s -o %.*s"
                        , file, conf.cmod_so->lenUsed, conf.cmod_so->data);
                hw_String_push(hw, &cmd, '\0'); cmd->lenUsed -= 1;
                hw_cmd_lock((void *)cmd->data);

                if(conf.cmod_call) {
                    hw_CModule *cmod = hw_CModule_newFrom_file(
                            hw, (void*) conf.cmod_so->data);
                    hw_VarFn fn = hw_CModule_getfn(cmod, conf.cmod_call->data
                                                , conf.cmod_call->lenUsed);
                    HW_DEBUG(HW_LOG("NO ARGS%s", ""));
                    fn(hw, NULL, NULL, 0);
                    //hw_debug_print_cmod(hw, cmod);
                }
            }

            HW_THREAD_FREE(hw, c_code);

        }
    }


    _L_early_exit:
    config_del(hw, &conf);
    hw_State_delete(hw);
    
    return EXIT_SUCCESS;
}

