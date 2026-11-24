#include <mach/mach.h>
#include <mach/mach_vm.h>
#include <stdio.h>

void *v_test_closure_table_a(void);
void *v_test_closure_table_b(void);

static int target(void) { return 7; }

static int check_table(void *source) {
	mach_vm_address_t address = 0;
	if (mach_vm_allocate(mach_task_self(), &address, 32768, VM_FLAGS_ANYWHERE) != KERN_SUCCESS)
		return 0;
	mach_vm_address_t code = address + 16384;
	vm_prot_t current, maximum;
	kern_return_t result = mach_vm_remap(mach_task_self(), &code, 16384, 0,
		VM_FLAGS_FIXED | VM_FLAGS_OVERWRITE, mach_task_self(),
		(mach_vm_address_t)source, FALSE, &current, &maximum, VM_INHERIT_SHARE);
	if (result != KERN_SUCCESS || code != address + 16384 ||
		current != (VM_PROT_READ | VM_PROT_EXECUTE)) {
		mach_vm_deallocate(mach_task_self(), address, 32768);
		return 0;
	}
	void *base = (void *)address;
	void **metadata = (void **)((char *)base + 16);
	metadata[0] = NULL;
	metadata[1] = (void *)target;
	int (*callback)(void) = (int (*)(void))((char *)base + 16384 + 16);
	int valid = callback() == 7;
	kern_return_t released = mach_vm_deallocate(mach_task_self(), (mach_vm_address_t)base, 32768);
	return valid && released == KERN_SUCCESS;
}

int main(void) {
	if (v_test_closure_table_a() != v_test_closure_table_b()) return 2;
	if (!check_table(v_test_closure_table_a()) || !check_table(v_test_closure_table_b())) return 1;
	puts("Mach closure tables from multiple translation units passed");
	return 0;
}
