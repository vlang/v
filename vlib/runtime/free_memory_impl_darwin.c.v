module runtime

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
fn C.mach_port_deallocate(task C.task_t, host C.host_t) i32
fn C.host_page_size(host C.host_t, out_page_size &C.vm_size_t) i32
fn C.host_statistics64(host C.host_t, flavor i32, host_info_out &int, host_info_outCnt &u32) i32

fn C.v_runtime_free_memory_darwin(bytes &u64, code &i32) i32

#flag @VEXEROOT/vlib/runtime/free_memory_darwin.o

fn free_memory_impl() !usize {
	$if macos || ios {
		mut bytes := u64(0)
		mut code := i32(0)
		stage := C.v_runtime_free_memory_darwin(&bytes, &code)
		if stage == 1 {
			return error('free_memory: `C.host_statistics64()` return = ${code}')
		}
		if stage == 2 {
			return error('free_memory: `C.host_page_size()` return = ${code}')
		}
		return usize(bytes)
	}
	return error('free_memory: not implemented')
}
