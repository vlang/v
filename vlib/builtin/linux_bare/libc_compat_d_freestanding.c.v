module builtin

$if linux && (amd64 || arm64) {
	#include "@DIR/libc_sort.h"
	fn C.v_freestanding_qsort(voidptr, uint, uint, C.qsort_callback_func)
	// A library/object uses its host's FILE and pthread ABIs.
	$if !v_bare_no_startup ? {
		#flag @DIR/libc_sort.c
		// These translation units provide the out-of-line stdio definitions.
		#flag -D__NO_INLINE__=1
		#flag -D_GNU_SOURCE=1
		#flag @DIR/libc_descriptors.c
		#flag @DIR/libc_directory_abi.c
		#flag @DIR/libc_socket.c
		#flag @DIR/libc_interfaces_abi.c
		#flag @DIR/libc_ipc_abi.c
		#flag @DIR/libc_resolver.c
		#flag @DIR/libc_nameinfo_abi.c
		#flag @DIR/libc_stdio.c
		#flag @DIR/libc_format.c
		#flag @DIR/libc_fortify_abi.c
		#flag @DIR/libc_services.c
		#flag @DIR/libc_time_abi.c
		#flag @DIR/libc_syslog.c
		#flag @DIR/libc_mutex.c
		#flag @DIR/libc_pthread_abi.c
		#flag @DIR/libc_cancel_abi.c
		#flag @DIR/libc_system.c
		#flag @DIR/libc_native_abi.c
		#flag @DIR/libc_dynamic_abi.c
		#flag @DIR/libc_signal_abi.c
		#flag @DIR/libc_shell_abi.c
		#flag @DIR/libc_strings.c
		#flag @DIR/libc_string_abi.c
		#flag @DIR/libc_accounts_abi.c
		#flag @DIR/libc_environment.c
		#include "@DIR/libc_runtime.h"
		#flag @DIR/libc_locale.c
		#flag @DIR/libc_locale_abi.c
		#flag @DIR/libc_locale_gnu_abi.c
		#flag @DIR/libc_messages_abi.c
		#flag @DIR/libc_iconv_abi.c
		#flag @DIR/libc_wide_abi.c
		#flag @DIR/libc_pattern_abi.c
		#flag @DIR/libc_glob_abi.c
		#flag @DIR/libc_scan.c
		#flag @DIR/libc_scanf.c
		#flag @DIR/libc_intscan.c
		#flag @DIR/libc_floatscan.c
		#flag @DIR/libc_misc.c
		#flag @DIR/libc_realpath.c
		#flag @DIR/libc_strftime.c
		#flag @DIR/libc_random.c
		#flag @DIR/libc_math.c
		#flag @DIR/libc_numeric_abi.c
		#flag @DIR/libc_jump.c
		#flag @DIR/libc_exit.c

		fn C.v_bare_run_finalizers()

		// Native C environment entries use the same durable heap ownership as os.
		@[export: 'v_freestanding_environment_allocate'; markused; unsafe]
		fn bare_environment_allocate(size uint) voidptr {
			if size > uint(max_int) {
				bf_native_errno_cell = 12
				return unsafe { nil }
			}
			return unsafe { vcalloc_scoped_impl(int(size), false, false, true) }
		}

		@[export: 'v_freestanding_environment_release'; markused; unsafe]
		fn bare_environment_release(pointer voidptr) {
			unsafe { vfree_heap(pointer) }
		}

		fn bare_flush_streams() {
			C.v_bare_run_finalizers()
		}
	}
}
