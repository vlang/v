module main

import (
	os
	testing
)

fn main() {
	args := os.args
	args_string := args[1..].join(' ')
	if testing.v_build_failing(
		args_string.all_before('build-examples'), 'examples')
	{
		exit(1)
	}
	// Test -live
	vexe := args[1]
	ret := os.system('$vexe -live examples/hot_reload/message.v')
	if ret != 0 {
		println('-live message.v failed')
		exit(1)
	}
}
