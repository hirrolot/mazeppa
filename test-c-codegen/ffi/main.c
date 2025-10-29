#include "../mazeppa.h"

#include <stdio.h>
#include <stdbool.h>
#include <assert.h>
#include <stdint.h>

extern mz_Value run(void);

static bool foo = false, bar = false, baz = false;

extern mz_Value mz_ffi_foo(void)
{
    foo = true;
    return MZ_INT(I, 32, 1);
}

extern mz_Value mz_ffi_bar(mz_Value x)
{
    bar = true;
    return MZ_INT(I, 32, MZ_GET(I32, x) * 2);
}

extern mz_Value mz_ffi_baz(mz_Value x, mz_Value y)
{
    baz = true;
    return MZ_INT(I, 32, MZ_GET(I32, x) + MZ_GET(I32, y));
}

int main(void)
{
    GC_INIT();
    const int32_t result = MZ_GET(I32, run());
    assert(24 == result);
    assert(foo);
    assert(bar);
    assert(baz);
}
