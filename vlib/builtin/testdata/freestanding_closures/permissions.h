#ifndef V_CLOSURE_TEST_PERMISSIONS_H
#define V_CLOSURE_TEST_PERMISSIONS_H
#if defined(__APPLE__)
#include <mach/mach.h>
#include <mach/mach_vm.h>
static int v_closure_test_region(void *pointer, vm_prot_t expected) {
	mach_vm_address_t address = (mach_vm_address_t)pointer;
	mach_vm_size_t size = 0;
	vm_region_basic_info_data_64_t info;
	mach_msg_type_number_t count = VM_REGION_BASIC_INFO_COUNT_64;
	mach_port_t object = MACH_PORT_NULL;
	kern_return_t result = mach_vm_region(mach_task_self(), &address, &size,
		VM_REGION_BASIC_INFO_64, (vm_region_info_t)&info, &count, &object);
	if (object != MACH_PORT_NULL) mach_port_deallocate(mach_task_self(), object);
	return result == KERN_SUCCESS && info.protection == expected &&
		address <= (mach_vm_address_t)pointer && (mach_vm_address_t)pointer < address + size;
}
static inline int v_closure_test_permissions(void *callback) {
	return v_closure_test_region(callback, VM_PROT_READ | VM_PROT_EXECUTE) &&
		v_closure_test_region((char *)callback - 16384, VM_PROT_READ | VM_PROT_WRITE);
}
#endif
#endif
