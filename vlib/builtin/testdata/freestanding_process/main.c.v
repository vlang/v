module main

#include "@DIR/probe.h"
#flag @DIR/probe.c

fn C.v_process_probe() i32

fn main() {
	result := C.v_process_probe()
	if result != 0 {
		eprintln('process compatibility failed at line ${result}')
		exit(95)
	}
	println('process compatibility ok')
}
