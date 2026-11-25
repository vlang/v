#include <stdint.h>
static __thread int initialized = 17;
static __thread int zero;
static __thread unsigned char aligned[64] __attribute__((aligned(64)));
uintptr_t dynamic_base_address(void) { return (uintptr_t)&initialized; }
int dynamic_base_step(int iteration) {
    if (initialized != 17 + iteration || zero != iteration || aligned[0] != (unsigned char)iteration || (uintptr_t)aligned % 64) return 0;
    initialized++; zero++; aligned[0]++;
    return 1;
}
