#include "hw.h"
#include "hw_dev.h"

typedef struct hw_CompilerC hw_CompilerC;
struct hw_CompilerC {
    hw_State *vm_parent;
    hw_State *vm_child;
    hw_Module *source;
    hw_String *out;
};

void hw_compc(void) {}
