module main

#flag @DIR/probe.c
#include "@DIR/probe.h"

fn C.v_macos_kernel_services_probe() i32

@[export: 'v_macos_probe_tid']
fn kernel_thread_id() u64 {
	return bf_gettid()
}

fn main() {
	if bf_gettid() == 0 { panic('missing kernel thread ID') }
	code := C.v_macos_kernel_services_probe()
	if code != 0 { panic('macOS kernel service probe failed: ${code}') }
	if bf_close(-1) != 9 { panic('could not seed BF errno') }
	before := bf_last_errno()
	mut predicate := u32(1)
	if bf_wait_u32(&predicate, 0, 0) != 0 { panic('changed predicate should not block') }
	if bf_wait_u32(&predicate, 1, 0) != 110 { panic('zero timeout should expire') }
	if bf_wake_u32(&predicate, true) != 0 { panic('wake without waiters should succeed') }
	if bf_ulock_wait(0x01000001, &predicate, 1, 1) != 22 { panic('ULF_NO_ERRNO accepted') }
	if bf_last_errno() != before { panic('wait/wake changed BF errno') }
	page_size, page_error := bf_page_size()
	if page_error != 0 || page_size == 0 { panic('V page-size query failed') }
	semaphore, created := bf_mach_semaphore_create(0, 1)
	if created != 0 || semaphore == 0 { panic('V semaphore creation failed') }
	if bf_mach_semaphore_wait(semaphore) != 0 { panic('V semaphore wait failed') }
	if bf_mach_semaphore_destroy(semaphore) != 0 { panic('V semaphore destruction failed') }
	if bf_mach_port_deallocate(semaphore) != 0 { panic('V semaphore right release failed') }
	task := bf_mach_task_self()
	kind, type_error := bf_mach_port_type(task)
	if type_error != 0 || kind & 0x10000 == 0 { panic('V task port query failed') }
	if bf_mach_port_deallocate(task) != 0 { panic('V task reference release failed') }
	address, allocation := bf_mach_vm_allocate(0, 16384, 1)
	if allocation != 0 || address == 0 { panic('V VM allocation failed') }
	alias, current, maximum, remapped := bf_mach_vm_remap(address, 16384, 0, 1, false)
	if remapped != 0 || alias == 0 || current & 3 != 3 || maximum & 3 != 3 {
		panic('V VM remap failed')
	}
	if bf_mach_vm_deallocate(alias, 16384) != 0 { panic('V VM alias release failed') }
	if bf_mach_vm_deallocate(address, 16384) != 0 { panic('V VM release failed') }
	if bf_last_errno() != before { panic('kernel service changed BF errno') }
	println('macOS kernel services ok')
}
