module main

#include "@DIR/unwind_probe.h"
#flag @DIR/unwind_probe.c
#flag -lgcc_eh
#flag -funwind-tables
#flag -fno-optimize-sibling-calls
#flag -Wl,--eh-frame-hdr

fn C.v_unwind_probe(i32) i32

fn main() {
	code := C.v_unwind_probe(0)
	if code != 0 {
		eprintln('static unwinding regression failed at C line ${code}')
		exit(1)
	}
	println('static ELF metadata and native unwinding passed')
}
