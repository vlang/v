module main

import os
import time

#include "@DIR/native_cpp_probe.h"
#flag -lv_cpp_probe

fn C.v_native_cpp_probe(&char) i32

fn main() {
	if os.args.len != 2 { exit(2) }
	gate := os.getenv('V_NATIVE_CPP_GATE')
	if gate != '' {
		println('native C++ fixture ready')
		flush_stdout()
		for !os.exists(gate) { time.sleep(5 * time.millisecond) }
	}
	code := C.v_native_cpp_probe(&char(os.args[1].str))
	if code != 0 {
		eprintln('native C++ regression failed at C++ line ${code}')
		exit(1)
	}
}
