#include "../mazeppa.h"

#include <stdio.h>
#include <stdbool.h>

extern mz_Value run(void);

static bool foo = false, bar = false;

extern mz_Value mz_ffi_foo(mz_Value v)
{
    const mz_prim_I32 x = MZ_GET(I32, v);
    foo = true;
    return MZ_INT(I, 32, x);
}

extern mz_Value mz_ffi_bar(mz_Value v)
{
    const mz_prim_I32 x = MZ_GET(I32, v);
    bar = true;
    return MZ_INT(I, 32, x);
}

int main(void)
{
    GC_INIT();
    const int32_t x = MZ_GET(I32, run());
    assert(12 == x);
    assert(foo);
    assert(bar);
}
