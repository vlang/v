#include <stdio.h>

fn test_shadowed_c_fn_call() {
	printf := c''
	unsafe {
		C.printf(printf)
	}
}
