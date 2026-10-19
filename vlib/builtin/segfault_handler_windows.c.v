module builtin

$if !native && !no_segfault_handler ?&& !freestanding && !v2_native_windows_pe_minimal ? {
	#include "@VEXEROOT/vlib/builtin/segfault_handler_windows.h"
}

fn C.v_install_windows_stack_overflow_handler()

// install_windows_stack_overflow_handler reserves exception stack space and installs
// an overflow-only vectored handler. Shared libraries leave their host's handlers alone.
fn install_windows_stack_overflow_handler() {
	$if !native && !no_segfault_handler ?&& !freestanding && !v2_native_windows_pe_minimal ? {
		if g_main_argv != unsafe { nil } {
			C.v_install_windows_stack_overflow_handler()
		}
	}
}
