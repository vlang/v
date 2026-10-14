#include <sys/types.h>
#include <sys/sysctl.h>
#include <uvm/uvmexp.h>
#include <stddef.h>

/* uvmexp changes with the SDK; V only receives the resulting byte count. */
int v_runtime_openbsd_free_memory(size_t *free_bytes) {
	int mib[] = { CTL_VM, VM_UVMEXP };
	struct uvmexp memory = { 0 };
	size_t length = sizeof(memory);
	int result = sysctl(mib, 2, &memory, &length, NULL, 0);
	if (result == 0) *free_bytes = (size_t)memory.pagesize * (size_t)memory.free;
	return result;
}
