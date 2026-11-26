#include <stdint.h>
int dynamic_step(int, void *);
uintptr_t dynamic_address(void);
void dynamic_complete(void);
int dynamic_completed(void);
#include <stddef.h>
size_t dynamic_mapped_bytes(void);
