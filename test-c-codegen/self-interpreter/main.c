#include "../mazeppa.h"

extern mz_Value run(void);

int main(void)
{
    GC_INIT();
    mz_Value v = run();
    const int32_t x = MZ_GET(I32, mz_force(v.payload[0]));
    assert(15 == x);
}
