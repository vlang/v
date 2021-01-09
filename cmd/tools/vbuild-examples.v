module main

import os
import testing

fn main() {
	args_string := os.args[1..].join(' ')
	params := args_string.all_before('build-examples')
	if testing.v_build_failing(params, 'examples') {
		exit(1)
	}
	if testing.v_build_failing(params + '-live', os.join_path('examples', 'hot_reload')) {
		exit(1)
	}
}
