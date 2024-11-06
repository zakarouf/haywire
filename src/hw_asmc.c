#include "hw.h"
#include "hw_dev.h"
#include <stdio.h>
#include <stdlib.h>

typedef struct hw_CompilerConfig hw_CompilerConfig;
struct hw_CompilerConfig {
    hw_uint head;
};

typedef struct hw_Compiler hw_Compiler;
struct hw_Compiler {
    hw_uint head;

    hw_String **files;
    hw_uint file_count;
    hw_uint file_current;

    hw_Lexer lexer;
    hw_Module module;
    hw_uint error;
    hw_uint line;
};


hw_uint g_dummy_ofile[] = {
    0x0000111100001111,
    0x0011223344556677
};

/**
 *  Section: Compiler Core
 */
hw_Compiler* hw_asmc_new(hw_CompilerConfig conf)
{
    hw_Compiler *compiler = HW_MALLOC(sizeof*compiler);
    compiler->module.data = hw_byteArr_new(512);
    compiler->module.code = hw_codeArr_new(64);
    return compiler;
}

void hw_asmc_delete(hw_Compiler *compiler)
{
    hw_byteArr_delete(compiler->module.data);
    hw_codeArr_delete(compiler->module.code);
    HW_FREE(compiler);
}

static hw_uint hw_asmc_write_code__raw(hw_Compiler *compiler, hw_code code)
{
    hw_uint at = compiler->module.code->lenUsed;
    hw_codeArr_push(&compiler->module.code, code);
    return at;
}

static hw_uint hw_asmc_write_data(
    hw_Compiler *compiler, void *data, hw_uint size)
{
    hw_uint at = compiler->module.data->lenUsed;
    hw_byteArr_pushstream(&compiler->module.data, data, size);
    return at;
}

#define hw_asmc_write_code(compiler, ...)\
    hw_asmc_write_code__raw(compiler, (hw_code){__VA_ARGS__})


hw_uint hw_asmc_write_module_to_file(hw_Compiler *compiler)
{

    hw_Module *module = &compiler->module;
    hw_byteArr *bs = hw_byteArr_new(1024);
    

    FILE *fp = fopen("out.hwo", "w");

    fclose(fp);
    hw_byteArr_delete(bs);
}

/**
 * Section: Compiler
 */

static int hw_asmc_compile(hw_CompilerConfig conf)
{
    /* Init */
    int ecode = 0;
    hw_Compiler *compiler = hw_asmc_new(conf);
    
    /* Compile */
    

    /* De-init */
    hw_asmc_delete(compiler);
    return ecode;
}

int main(int argc, char *argv[])
{
    hw_asmc_compile((hw_CompilerConfig){0});
    return EXIT_SUCCESS;
}
