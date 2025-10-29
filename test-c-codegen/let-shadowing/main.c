#include "../mazeppa.h"

extern mz_Value run(void);

int main(void)
{
    GC_INIT();
    const int32_t x = MZ_GET(I32, run());
    assert(100 == x);
}
