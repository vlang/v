module main

import os

fn failed(msg string) {
	println('!!! failed: ${msg}')
}

fn passed(msg string) {
	println('>>> passed: ${msg}')
}

fn vcheck(vfile string) {
	run_check := 'v -user_mod_path . -freestanding run '
	if 0 == os.system_args([...(os.split_args(run_check) or { panic(err) }),
		'${vfile}' + '/' + '${vfile}' + '.v']) {
		passed(run_check)
	} else {
		failed(run_check)
	}
	os.system_args(['ls', '-lh', '${vfile}' + '/' + '${vfile}'])
	os.system_args(['rm', '-f', '${vfile}' + '/' + '${vfile}'])
}

fn main() {
	vcheck('linuxsys')
	vcheck('string')
	vcheck('consts')
	vcheck('structs')
	exit(0)
}
