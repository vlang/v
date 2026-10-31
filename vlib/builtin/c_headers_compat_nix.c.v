module builtin

// Older bootstrap compilers assume libc and process helpers come from headers.
// The current backend emits their declarations from the bodyless V bindings.
$if !v3_c_extern_declarations ? {
	#include <sys/uio.h>
	#include <poll.h>
	#include <sys/file.h>
	#include <stdlib.h>
	#include "@VMODROOT/vlib/os/execute_capture_legacy.h"
	$if macos {
		#include <mach/mach.h>
		#include <mach/mach_vm.h>
		#include <mach/mach_time.h>
		#include <CommonCrypto/CommonDigest.h>
	}
}
