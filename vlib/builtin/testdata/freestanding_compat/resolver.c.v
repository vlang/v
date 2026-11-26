module main

import os

#include "@DIR/probe.h"
#flag @DIR/resolver_probe.c
#flag -Wl,--wrap=malloc
#flag -Wl,--wrap=calloc
#flag -Wl,--wrap=realloc
#flag -Wl,--wrap=free
#flag -Wl,--wrap=socket

fn C.v_freestanding_resolver_check(directory &char, port u16, configured i32) i32

fn main() {
	assert os.args.len == 4
	status := C.v_freestanding_resolver_check(&char(os.args[1].str), u16(os.args[2].int()),
		i32(os.args[3].int()))
	assert status == 0, status.str()
	println('freestanding resolver ok')
}
