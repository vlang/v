module builtin

$if !freestanding || v_bare_libc ? {
	#include "@DIR/vlibc_hosted_abi.h"
	fn C.v_hosted_vlibc_create(&uint, fn (voidptr) voidptr, voidptr) i32
	fn C.v_hosted_vlibc_join(uint) i32

	@[heap]
	struct VlibcHostedThreadStart {
		function VlibcThreadFn
		argument u64
	}
	fn vlibc_hosted_start_allocate() voidptr {
		$if gcboehm ? {
			// Keep the transferred callback and argument rooted until the new,
			// collector-registered thread has copied them to its own stack.
			return C.GC_MALLOC_UNCOLLECTABLE(sizeof(VlibcHostedThreadStart))
		}
		return C.malloc(sizeof(VlibcHostedThreadStart))
	}
	fn vlibc_hosted_start_release(pointer voidptr) {
		$if gcboehm ? {
			C.GC_FREE(pointer)
			return
		}
		C.free(pointer)
	}

	fn vlibc_hosted_thread_entry(argument voidptr) voidptr {
		// The new pthread exclusively owns this payload after successful creation.
		start := unsafe { &VlibcHostedThreadStart(argument) }
		function := start.function
		value := start.argument
		vlibc_hosted_start_release(start)
		function(value)
		return unsafe { nil }
	}

	fn vlibc_hosted_thread_spawn(function VlibcThreadFn, argument u64) (VlibcThread, VlibcErrno) {
		start := unsafe { &VlibcHostedThreadStart(vlibc_hosted_start_allocate()) }
		if start == unsafe { nil } { return VlibcThread{}, unsafe { VlibcErrno(12) } }
		// Raw heap storage needs initialization before ownership passes to the child.
		unsafe { *start = VlibcHostedThreadStart{ function: function, argument: argument } }
		mut handle := uint(0)
		code := C.v_hosted_vlibc_create(&handle, vlibc_hosted_thread_entry, start)
		if code != 0 {
			vlibc_hosted_start_release(start)
			return VlibcThread{}, unsafe { VlibcErrno(code) }
		}
		return VlibcThread{ fn_ptr: function, arg: argument, host_handle: handle }, .enoerror
	}

	fn vlibc_hosted_thread_join(mut thread VlibcThread) VlibcErrno {
		if thread.host_handle == 0 { return .enoerror }
		code := C.v_hosted_vlibc_join(thread.host_handle)
		if code != 0 { return unsafe { VlibcErrno(code) } }
		thread.host_handle = 0
		thread.done = 1
		return .enoerror
	}
}
