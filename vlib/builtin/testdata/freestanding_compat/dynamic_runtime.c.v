module main

#include "@DIR/dynamic_probe.h"
#include "@DIR/unwind_probe.h"
#flag @DIR/dynamic_probe.c
#flag -lv_dynamic_root
#flag @DIR/unwind_probe.c
#flag -lgcc_eh
#flag -funwind-tables
#flag -fno-optimize-sibling-calls
#flag -Wl,--eh-frame-hdr

fn C.v_dynamic_probe() i32
fn C.v_unwind_probe(i32) i32

fn main() {
	code := C.v_dynamic_probe()
	if code != 0 {
		eprintln('dynamic lookup regression failed at C line ${code}')
		exit(1)
	}
	unwind_code := C.v_unwind_probe(1)
	if unwind_code != 0 {
		eprintln('dynamic unwinding regression failed at C line ${unwind_code}')
		exit(1)
	}
	println('GNU/SysV ELF lookup, dependency scope, native constructors and thread errors passed')
}
