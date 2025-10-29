#include "../mazeppa.h"

int main(void)
{
    (void)MZ_OP2(MZ_INT(I, 32, 1), div, MZ_INT(I, 32, 0));
    assert(false);
}
