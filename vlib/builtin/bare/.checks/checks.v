module main

import os

fn failed (msg string) {
	println ("!!! failed: $msg")
}

fn passed (msg string) {
	println (">>> passed: $msg")
}


fn vcheck(vfile string) {
	//os.system("ln -s ../forkedtest $vfile/forkedtest")
	run_check := "v -user_mod_path . -freestanding --enable-globals run "
	if 0 == os.system("$run_check $vfile/${vfile}.v") {
		passed(run_check)
	} else {
		failed(run_check)
	}
	os.system("ls -lh $vfile/$vfile")
	os.system("rm -f $vfile/$vfile")
	//os.system("rm -f $vfile/forkedtest")
}

fn main() {
	vcheck("linuxsys")
	vcheck("string")
	vcheck("consts")
	exit(0)
}

