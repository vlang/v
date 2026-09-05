module transform

#include <mach/mach.h>

#include <mach/mach_vm.h>

#include <unistd.h>

fn C.mach_vm_allocate(target C.task_t, address &u64, size u64, flags int) int

fn C.mach_vm_deallocate(target C.task_t, address u64, size u64) int

fn C.mach_vm_remap(target C.task_t, address &u64, size u64, mask u64, flags int, source C.task_t, source_address u64, copy int, current_protection &int, max_protection &int, inheritance u32) int

fn C.getpagesize() int

fn snapshot_ast_buffer(data voidptr, len u64, capacity u64) ?AstBufferSnapshot {
	if len == 0 || capacity < len || data == unsafe { nil } {
		return none
	}
	page_size := u64(C.getpagesize())
	source := u64(data)
	offset := source % page_size
	bytes := (capacity + offset + page_size - 1) / page_size * page_size
	copy_bytes := (len + offset + page_size - 1) / page_size * page_size
	task := C.mach_task_self()
	mut address := u64(0)
	if C.mach_vm_allocate(task, &address, bytes, C.VM_FLAGS_ANYWHERE) != 0 {
		return none
	}
	reservation := address
	mut current_protection := 0
	mut max_protection := 0
	// Only replace the prefix of our own reservation. copy=1 gives both source
	// and worker independent writes while initially sharing physical pages.
	if C.mach_vm_remap(task, &address, copy_bytes, 0, C.VM_FLAGS_FIXED | C.VM_FLAGS_OVERWRITE, task, source - offset, 1, &current_protection, &max_protection, C.VM_INHERIT_NONE) != 0 {
		C.mach_vm_deallocate(task, reservation, bytes)
		return none
	}
	return AstBufferSnapshot{
		data: unsafe { voidptr(address + offset) }
		address: address
		bytes: bytes
	}
}

fn release_ast_buffer_snapshot(snapshot AstBufferSnapshot) {
	if snapshot.bytes > 0 {
		C.mach_vm_deallocate(C.mach_task_self(), snapshot.address, snapshot.bytes)
	}
}
