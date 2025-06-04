module runtime

#include <mach/mach.h>
#include <mach/task.h>

@[typedef]
pub struct C.vm_size_t {
}

@[typedef]
pub struct C.vm_statistics64_data_t {
	free_count          u32
	purgeable_count     u32
	speculative_count   u32
	external_page_count u32
}

@[typedef]
pub struct C.host_t {}

@[typedef]
pub struct C.task_t {}

fn C.mach_host_self() C.host_t
fn C.mach_task_self() C.task_t
fn C.mach_port_deallocate(task C.task_t, host C.host_t) int
fn C.host_page_size(host C.host_t, out_page_size &C.vm_size_t) int
fn C.host_statistics64(host C.host_t, flavor int, host_info_out &int, host_info_outCnt &u32) int

fn free_memory_impl() !usize {
	$if macos {
		mut hs := C.vm_statistics64_data_t{}
		mut vmsz := u32(C.HOST_VM_INFO64_COUNT)
		mut hps := u32(0)
		mut host := C.mach_host_self()
		defer {
			// Critical: Release send right for host port
			// --------------------------------------------------
			// Mach ports are system resources. Calling mach_host_self()
			// increments the port's reference count. We must manually release
			// to prevent resource leaks (port exhaustion can cause kernel failures).
			// mach_port_deallocate decrements the reference count, allowing
			// system resource reclamation when count reaches zero.
			// Parameters:
			//   C.mach_task_self() - Port for current task
			//   host - Host port to release
			// Return value ignored (_) since we only care about resource cleanup
			_ := C.mach_port_deallocate(C.mach_task_self(), host)
		}
		unsafe {
			retval_1 := C.host_statistics64(host, C.HOST_VM_INFO64, &int(&hs), &vmsz)
			if retval_1 != C.KERN_SUCCESS {
				return error('free_memory: `C.host_statistics64()` return = ${retval_1}')
			}
			retval_2 := C.host_page_size(host, &C.vm_size_t(&hps))
			if retval_2 != C.KERN_SUCCESS {
				return error('free_memory: `C.host_page_size()` return = ${retval_2}')
			}
		}
		return usize(u64(hs.free_count) * u64(hps))
	}
	return error('free_memory: not implemented')
}
