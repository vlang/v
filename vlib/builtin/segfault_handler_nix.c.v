module builtin

// The handler runs on an alternate signal stack, so that it can still report a stack
// overflow, instead of the process dying silently. See segfault_handler_nix.h.
// Native startup does not install this C runtime handler.
$if !native && !no_segfault_handler ?&& !freestanding && !vinix {
	#include "@VEXEROOT/vlib/builtin/segfault_handler_nix.h"
}

fn C.v_install_segfault_handler(fallback voidptr, main_argv voidptr)

// install_segfault_handler makes a segmentation fault print a message and a backtrace.
// It is called by builtin_init. Only the main thread of an executable, whose C main
// stored argv, installs it: a shared library must not take over the signals of its
// host process.
fn install_segfault_handler() {
	$if !native && !no_segfault_handler ?&& !freestanding && !vinix {
		if g_main_argv != unsafe { nil } {
			C.v_install_segfault_handler(voidptr(v_segmentation_fault_handler), g_main_argv)
		}
	}
}
