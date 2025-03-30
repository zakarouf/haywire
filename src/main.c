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
line("    -h, --help            : Show this message")
line("    -n                    : Do not prefix file name as the namespace while complilation")
line("    -m, --modname         : Assign name to the compiled module")
line("                            otherwise use the name of the first file passed in")
line("    -o                    : Write the compiled bytecode into a file with .hwo extention")
line("                            Will use passed in mod name via -m")
line("")
line("    -c, --c-compile       : Compile the loaded module in to shared object, requires c compiler")
line("        --cc [bin]        : Set the c compiler, default is clang")
line("        --ccflags \"[f]\"   : Append cc flags")
line("                            flags \"-std=c99 -Wall -Wextra -fPIC -shared\", are always set")
line("")
line("    -d, --disasm          : Disassemble the loaded file and print it to `stdout`")
line("        --disasm-out [n]  : Same as `-d` but print it out to a specific file instead")
line("    --ct-print            : Print the transpiled c code to `stdout`")
line("")
line("    --mcall               : Call The loaded module")
line("    --cmcall              : Call The Compiled C Module")
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
    hw_String *mod_name;
    hw_String *call;
    hw_String *cc;
    hw_String *cmod_so;
    hw_String *cmod_ccflags;
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
              , cmod_out:1
              , write_to_file: 1
              ;
    } flags;
};

enum ArgType {
    ARG_TYPE_CONF_CHAR,
    ARG_TYPE_CONF_STR,
    ARG_TYPE_STRING,
    ARG_TYPE_VMARG,
    ARG_TYPE_ERROR
};

static enum ArgType _argparse_is_arg(hw_String *arg)
{
    if(arg->lenUsed == 2) {
        if(arg->data[0] == '-') {
            if(arg->data[1] == '-') return ARG_TYPE_VMARG; // ... -- ...
            else return ARG_TYPE_CONF_CHAR;  // ... '-a' ...
        }
        return ARG_TYPE_STRING;           // ... 'a' ...
    } else if(arg->lenUsed > 2) {
        if(arg->data[0] == '-') {
            if(arg->data[1] == '-') return ARG_TYPE_CONF_STR;     // ...
        } else return ARG_TYPE_STRING;
    }

    return ARG_TYPE_ERROR;
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
        break; case 'o': conf->flags.write_to_file = 1;
        break; case 'c': conf->flags.cmod_out = 1;
        break; default: hw_loglnp("Unknown command '-%c'", ch);
    }
    return ARG;
}

static hw_uint _argparse_mult_string(hw_State *hw,
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
    argcheck("disasm")      { conf->flags.disasm = 1; }
    argcheck("print_inst")  { conf->flags.print_inst_info = 1; }

    argcheck("ct")          { conf->flags.transpile = 1; }
    argcheck("ct-print")    { conf->flags.cmod_source_print = 1; }

    argcheck("cmd")         { hw_cmd((void*)argnext_req(args, &ARG)->data); }

    argcheck("mcall")       { conf->call = argnext_req(args, &ARG); }
    argcheck("cmcall")      { conf->cmod_call = argnext_req(args, &ARG); }

    argcheck("cc")          { conf->cc = argnext(args, &ARG); }
    argcheck("c-compile")   { conf->flags.cmod_out = 1; }
    argcheck("ccflags")     { hw_String *ccflags = argnext(args, &ARG); 
                              hw_String_append_fmt(hw, &conf->cmod_ccflags
                                      , "%.*s", ccflags->lenUsed
                                      , ccflags->data); }
    argparse_end()
    
    return ARG;
}

hw_String *hw_String_newFrom_str(hw_State *hw, hw_byte const *str, hw_u32 size)
{
    hw_String *string = hw_String_new(hw, size+1);
    memcpy(string->data, str, size);
    string->lenUsed += size;
    return string;
}

hw_int hw_argparse(
    hw_State *hw, hw_Config *conf, int argc, char *argv[]) {
    HW_ARR_NEW(hw, conf->files, 8);
    conf->cmod_so = NULL;
    conf->cmod_call = NULL;
    conf->cmod_ccflags = hw_String_newFrom_str(hw, 
            HW_STR("-std=c99 -Wall -Wextra -fPIC -shared "));
    hw_VarArr *args = _wrap_args(hw, argc, argv);
    conf->args = args;
    hw_uint ARG = 0;
    hw_String *arg = argnext(args, &ARG);
    while(arg) {
        switch (_argparse_is_arg(arg)) {
            case ARG_TYPE_CONF_CHAR:        ARG = _argparse_single_char_conf(
                                                    args, conf, ARG()->data[1], ARG);
            break; case ARG_TYPE_CONF_STR:  ARG = _argparse_mult_string(
                                                    hw, args, conf, ARG);
            break; case ARG_TYPE_STRING:    HW_ARR_PUSH(hw, conf->files, ARG());
            break; case ARG_TYPE_VMARG:
            default: hw_loglnp("Unknown argument %.*s"
                , ARG()->lenUsed, ARG()->data);
        }
        arg = argnext(args, &ARG);
    }
    return 0;
}
#undef ARG

void config_del(hw_State *hw, hw_Config *conf)
{
    hw_String_delete(hw, conf->cmod_so);
    hw_String_delete(hw, conf->cmod_ccflags);
    if(conf->mod_name) hw_String_delete(hw, conf->mod_name);
    hwfn_VarArr_delete(hw
        , (hw_Var[]){[0].as_arr = conf->args}, (hw_byte[]){hw_TypeID_array}, 1);
    HW_ARR_DELETE(hw, conf->files);
}

static int _load_files(hw_State *hw, hw_Config *conf)
{
    hw_ModuleArr *mods;
    HW_ARR(hw_String *) *mod_files;
    HW_ARR(hw_String *) *source_files;
    HW_ARR(hw_String *) *cmod_files;
    HW_ARR(hw_String *) *mod_namespace;
    
    HW_ARR_NEW(hw, mods, 8);
    HW_ARR_NEW(hw, mod_files, 8);
    HW_ARR_NEW(hw, source_files, 8);
    HW_ARR_NEW(hw, cmod_files, 8);
    HW_ARR_NEW(hw, mod_namespace, 8);

    for (hw_u32 i = 0; i < conf->files->lenUsed; i++) {
        hw_u32 ext_len = 0;
        hw_String *file = conf->files->data[i];

        hw_byte const *ext = hw_str_file_extension(file->data
                                                 , file->lenUsed
                                                 , &ext_len);
        #define check_ext(x) (0 == hw_ptrcmp(x, sizeof(x)-1, ext, ext_len))
        if(check_ext("hw")) {
            HW_ASSERT(0 && ".hw NOT IMPLEMENTED ");
            HW_ARR_PUSH(hw, source_files, file);
            HW_ARR_PUSH(hw, mod_namespace, hw_stripfile_path_ext(hw, file->data
                                                             , file->lenUsed));
        } else if(check_ext("hwo")) {
            HW_ARR_PUSH(hw, mod_files, file);
            HW_ARR_PUSH(hw, mod_namespace, conf->flags.no_namespace? NULL:
                    hw_stripfile_path_ext(hw, file->data, file->lenUsed));
        } else if(check_ext("hws")) {
            HW_ARR_PUSH(hw, mod_files, file);
            HW_ARR_PUSH(hw, mod_namespace, conf->flags.no_namespace? NULL:
                    hw_stripfile_path_ext(hw, file->data, file->lenUsed));
        } else if(check_ext("hwso")) {
            HW_ARR_PUSH(hw, cmod_files, file);
        } else {
            hw_loglnp("Unknown: filetype '%.*s', file: '%.*s'", ext_len, ext
                           , file->lenUsed
                           , file->data);
            hw_exit(-1, HW_STR("filetype error"));
        }
    }

    int compile = (mod_files->lenUsed > 0);

    if(compile) {
        hw_compbc_compile_files(hw, &mods, mod_files->data, mod_files->lenUsed);
        hw_Module *mod = hw_Module_combine(hw, mods->lenUsed, mods->data
                                         , mod_namespace->data);
    
        for (hw_u32 i = 0; i < mods->lenUsed; i++) {
            hw_Module_delete_detatch_knstobj(hw, mods->data[i]);
        }

        if(NULL == conf->mod_name) {
            hw_String *firstfile = conf->files->data[0];
            conf->mod_name = hw_stripfile_path_ext(hw
                    , firstfile->data, firstfile->lenUsed);
        }

        hw_Global_add_symb((void *)hw->global, conf->mod_name->data
        , conf->mod_name->lenUsed, (hw_Var){.as_module = mod}, hw_TypeID_module);
    }

    HW_ARR_DELETE(hw, mods);
    HW_ARR_DELETE(hw, mod_files);
    HW_ARR_DELETE(hw, source_files);
    HW_ARR_DELETE(hw, cmod_files);

    for (hw_u32 i = 0; i < mod_namespace->lenUsed; i++) {
        HW_VARFN(hw, hwfn_String_delete, ([0].as_string = mod_namespace->data[i])
                , (hw_TypeID_string),);
    }

    HW_ARR_DELETE(hw, mod_namespace);

    return compile;
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

    if(argc < 2) { fputs("-h, --help to see options\n", hw->stdout);
                   goto _L_early_exit;}
    if(conf.flags.help) {
        fputs((void *)HELP_TXT.data, hw->stdout);
        goto _L_early_exit;
    }

    if(conf.flags.print_inst_info) {
        hw_debug_print_inst(hw);
    }
    if(conf.files->lenUsed && _load_files(hw, &conf)) {
        hw_u32 mod_id = hw_Global_get_symb_id(hw->global, conf.mod_name->data, conf.mod_name->lenUsed);
        hw_Module *mod = hw_Global_get_symb_via_id(hw->global, mod_id).as_module;
        HW_ASSERT(mod);
        if(conf.call != NULL) {
            hw_uint fn_id = 0;
            HW_ASSERT(hw_Module_get_fn(mod, conf.call->data, conf.call->lenUsed, &fn_id));
            hw_vm_prepare_call(hw, mod_id, fn_id);
            hw_vm(hw);       
        }

        if(conf.flags.disasm) {
            hw_debug_Module_disasm(hw, mod);
        }

        if(conf.flags.write_to_file) {
            
            hw_String *outfile = hw_String_new(hw, conf.mod_name->lenUsed + 5);
            hw_String_append_fmt(hw, &outfile, "%.*s.hwo", conf.mod_name->lenUsed
                                                         , conf.mod_name->data);
            hw_String_nullterm(hw, &outfile);
            hw_Module_writetofile(hw, mod, (void *)outfile->data);
            hw_String_delete(hw, outfile);
        }

        hw_String *c_code = NULL;


        if(conf.flags.cmod_out) {
            c_code = hw_compc_mod_to_c(hw, mod);
            hw_String *cfile = hw_String_new(hw, conf.mod_name->lenUsed + 3);
            hw_String_append_fmt(hw, &cfile, "%.*s.c"
                    , conf.mod_name->lenUsed, conf.mod_name->data); hw_String_nullterm(hw, &cfile);
            hw_writefile((void *)cfile->data, c_code->data
                                         , 1, c_code->len);

            hw_String *s = hw_String_new(hw, conf.mod_name->lenUsed + 5);
            hw_String_append_fmt(hw, &s, "./%.*s.hwso"
                      , conf.mod_name->lenUsed
                      , conf.mod_name->data);
            conf.cmod_so = s;

            hw_String *cmd = hw_String_new(hw, 64);

            if(conf.cc) {
                hw_String_append_fmt(hw, &cmd, "%.*s ", conf.cc->lenUsed
                                                      , conf.cc->data);
            } else {
                hw_String_append_fmt(hw, &cmd, "clang ");
            }

            hw_String_append_fmt(hw, &cmd, 
                    "%.*s %.*s -o %.*s"
                    , conf.cmod_ccflags->lenUsed, conf.cmod_ccflags->data
                    , cfile->lenUsed, cfile->data
                    , conf.cmod_so->lenUsed, conf.cmod_so->data);

            hw_String_push(hw, &cmd, '\0'); cmd->lenUsed -= 1;
            hw_logstr((void *)cmd->data, cmd->lenUsed);
            hw_cmd_lock((void *)cmd->data);
            hw_String_delete(hw, cmd);

            if(conf.cmod_call) {
                hw_CModule *cmod = hw_CModule_newFrom_file(
                        hw, (void*) conf.cmod_so->data);
                HW_DEBUG(
                    HW_ASSERT(cmod);
                );
                hw_VarFn fn = hw_CModule_getfn(cmod, conf.cmod_call->data
                                            , conf.cmod_call->lenUsed);
                HW_DEBUG(HW_LOG("NO ARGS%s", ""));
                fn(hw, NULL, NULL, 0);
            }
            hw_String_delete(hw, cfile);
        }

        if(conf.flags.cmod_source_print) {
            if(c_code == NULL) c_code = hw_compc_mod_to_c(hw, mod);
            hw_debug_print_var(hw, (hw_Var){.as_string = c_code}
                                 , hw_TypeID_string);
        }
        if(c_code) hw_String_delete(hw, c_code);

    } // _load_files



    _L_early_exit:
    config_del(hw, &conf);
    hw_State_delete(hw);
    
    return EXIT_SUCCESS;
}

