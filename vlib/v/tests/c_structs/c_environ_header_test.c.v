module main

#include <stdio.h>

@[unsafe]
fn test_c_environment_is_declared_with_system_headers() {
	$if !windows {
		assert unsafe { voidptr(C.environ) } != unsafe { nil }
	}
}
