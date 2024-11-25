#include <stdio.h>
#include <stdlib.h>
#include "hw.h"
#include "hw_dev.h"

hw_State *hw_new(void)
{
    hw_Allocator allocator;
    hw_Allocator_default(&allocator);
    hw_TypeSys *ts = hw_TypeSys_default_with_allocator(allocator);
    hw_State *self = HW_TYPESYS_ALLOC(ts, sizeof(*self));
    self->tsys = ts;

    return self;
}

void hw_delete(hw_State *self)
{
    hw_TypeSys *ts = self->tsys;
    HW_TYPESYS_FREE(ts, self);
    hw_TypeSys_delete(ts);
}



int main(int argc, char *argv[])
{
    hw_State *s = hw_new();

    hw_delete(s);
    return EXIT_SUCCESS;
}
