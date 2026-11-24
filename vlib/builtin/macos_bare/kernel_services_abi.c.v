module builtin

// The SDK owns each wire layout. These declarations expose only fields used by
// the V implementation; the compiler neither parses nor reconstructs headers.
$if (freestanding || syscalls) && !native && !i386 {
	#include <mach/mach.h>
	#include <mach/mach_vm.h>
	#include <mach/mach_port.h>
	#include <mach/mig_errors.h>
	#include <mach/task.h>
	#include <mach/thread_act.h>
	#include "@DIR/kernel_services.h"

	// Declarative metadata preserves the read-only native state pointer on the
	// scalar C export used by independent test oracles.
	fn C.v_macos_kernel_thread_create_running(flavor i32, const_state &u32, count u32, thread &u32) i32

	@[typedef]
	struct C.mach_msg_header_t {
	mut:
		msgh_bits         u32
		msgh_size         u32
		msgh_remote_port  u32
		msgh_local_port   u32
		msgh_voucher_port u32
		msgh_id           i32
	}

	@[typedef]
	struct C.mach_msg_body_t {
	mut:
		msgh_descriptor_count u32
	}

	@[typedef]
	struct C.mach_msg_port_descriptor_t {
	mut:
		name        u32
		disposition u8
		type        u8
	}

	@[typedef]
	struct C.NDR_record_t {
	mut:
		int_rep u8
	}

	@[typedef]
	struct C.mig_reply_error_t {
	mut:
		Head    C.mach_msg_header_t
		RetCode i32
	}

	@[typedef]
	struct C.__Request__mach_port_type_t {
	mut:
		Head C.mach_msg_header_t
		NDR  C.NDR_record_t
		name u32
	}

	@[typedef]
	struct C.__Reply__mach_port_type_t {
	mut:
		Head  C.mach_msg_header_t
		ptype u32
	}

	@[typedef]
	struct C.__Request__thread_create_running_t {
	mut:
		Head         C.mach_msg_header_t
		NDR          C.NDR_record_t
		flavor       i32
		new_stateCnt u32
		// Only the first element's address is used. The SDK supplies the bound.
		new_state [1]u32
	}

	@[typedef]
	struct C.__Reply__thread_create_running_t {
	mut:
		Head      C.mach_msg_header_t
		msgh_body C.mach_msg_body_t
		child_act C.mach_msg_port_descriptor_t
	}

	@[typedef]
	struct C.__Request__thread_terminate_t {
	mut:
		Head C.mach_msg_header_t
	}

	@[typedef]
	struct C.__Reply__thread_terminate_t {
	mut:
		Head C.mach_msg_header_t
	}

	@[typedef]
	struct C.__Request__semaphore_create_t {
	mut:
		Head   C.mach_msg_header_t
		NDR    C.NDR_record_t
		policy i32
		value  i32
	}

	@[typedef]
	struct C.__Reply__semaphore_create_t {
	mut:
		Head      C.mach_msg_header_t
		msgh_body C.mach_msg_body_t
		semaphore C.mach_msg_port_descriptor_t
	}

	@[typedef]
	struct C.__Request__semaphore_destroy_t {
	mut:
		Head      C.mach_msg_header_t
		msgh_body C.mach_msg_body_t
		semaphore C.mach_msg_port_descriptor_t
	}

	@[typedef]
	struct C.__Reply__semaphore_destroy_t {
	mut:
		Head C.mach_msg_header_t
	}

	@[typedef]
	struct C.__Request__mach_vm_remap_t {
	mut:
		Head           C.mach_msg_header_t
		msgh_body      C.mach_msg_body_t
		src_task       C.mach_msg_port_descriptor_t
		NDR            C.NDR_record_t
		target_address u64
		size           u64
		mask           u64
		flags          i32
		src_address    u64
		copy           i32
		inheritance    i32
	}

	@[typedef]
	struct C.__Reply__mach_vm_remap_t {
	mut:
		Head           C.mach_msg_header_t
		target_address u64
		cur_protection i32
		max_protection i32
	}

	// A V-owned union supplies aligned request/reply storage; every member's
	// C type and layout comes from the SDK. Padding also accommodates trailers.
	union MacKernelMessage {
	mut:
		header            C.mach_msg_header_t
		error             C.mig_reply_error_t
		port_request      C.__Request__mach_port_type_t
		port_reply        C.__Reply__mach_port_type_t
		thread_request    C.__Request__thread_create_running_t
		thread_reply      C.__Reply__thread_create_running_t
		terminate_request C.__Request__thread_terminate_t
		terminate_reply   C.__Reply__thread_terminate_t
		semaphore_request C.__Request__semaphore_create_t
		semaphore_reply   C.__Reply__semaphore_create_t
		destroy_request   C.__Request__semaphore_destroy_t
		destroy_reply     C.__Reply__semaphore_destroy_t
		remap_request     C.__Request__mach_vm_remap_t
		remap_reply       C.__Reply__mach_vm_remap_t
		room              [512]u64
	}

	// No prologue may move the seventh/eighth arguments from the kernel's
	// incoming C-ABI stack slots. All transport policy remains in ordinary V.
	$if amd64 {
		@[_naked; no_profile; no_trace]
		fn mac_kernel_msg2(_message voidptr, _options u64, _bits_size u64, _ports u64,
			_voucher_id u64, _descriptors_reply u64, _receive_size u64, _timeout u64) i32 {
			asm volatile amd64 {
				mov r10, rcx
				mov eax, 16777263
				syscall
				ret
			}
		}
	} $else {
		@[_naked; no_profile; no_trace]
		fn mac_kernel_msg2(_message voidptr, _options u64, _bits_size u64, _ports u64,
			_voucher_id u64, _descriptors_reply u64, _receive_size u64, _timeout u64) i32 {
			asm volatile arm64 {
				mov x16, #-47
				svc #0x80
				ret
			}
		}
	}
}
