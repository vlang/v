module main

import os

fn failed (msg string) {
	println ("!!! failed: $msg")
}

fn passed (msg string) {
	println (">>> passed: $msg")
}


fn vcheck(vfile string) {
	run_check := "v -freestanding --enable-globals run "
	if 0 == os.system("$run_check $vfile/${vfile}.v") {
		passed(run_check)
	} else {
		failed(run_check)
	}
	os.system("rm -f $vfile/$vfile")
}

fn main() {
	vcheck("string")
	vcheck("linuxsys")
	exit(0)
}

