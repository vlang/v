#include <mach/mach.h>
#include <mach/mach_vm.h>
#include <stdio.h>

void *v_test_closure_table_a(void);
void *v_test_closure_table_b(void);

static int target(void) { return 7; }

static int check_table(void *base) {
	if (!base) return 0;
	void **metadata = (void **)((char *)base + 16);
	metadata[0] = NULL;
	metadata[1] = (void *)target;
	int (*callback)(void) = (int (*)(void))((char *)base + 16384 + 16);
	int valid = callback() == 7;
	kern_return_t released = mach_vm_deallocate(mach_task_self(), (mach_vm_address_t)base, 32768);
	return valid && released == KERN_SUCCESS;
}

int main(void) {
	if (!check_table(v_test_closure_table_a()) || !check_table(v_test_closure_table_b())) return 1;
	puts("Mach closure tables from multiple translation units passed");
	return 0;
}
