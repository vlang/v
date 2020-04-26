module main

import os
import testing

fn main() {
	args := os.args
	args_string := args[1..].join(' ')
	if testing.v_build_failing(args_string.all_before('build-tools'), 'cmd/tools') {
		exit(1)
	}
}
