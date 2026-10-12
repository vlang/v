#include <stdint.h>
#include <mach/mach.h>

int v_runtime_free_memory_darwin(uint64_t *bytes, int *code) {
	vm_statistics64_data_t statistics;
	mach_msg_type_number_t count = HOST_VM_INFO64_COUNT;
	vm_size_t page_size;
	host_t host = mach_host_self();
	int stage = 0;
	*code = host_statistics64(host, HOST_VM_INFO64, (host_info64_t)&statistics, &count);
	if (*code != KERN_SUCCESS) {
		stage = 1;
	} else {
		*code = host_page_size(host, &page_size);
		if (*code != KERN_SUCCESS) stage = 2;
	}
	// Release the send right on both success and failure.
	mach_port_deallocate(mach_task_self(), host);
	if (stage == 0) *bytes = (uint64_t)statistics.free_count * page_size;
	return stage;
}
