module main

import (
	os
	testing
	filepath
)

fn main() {
	args := os.args
	args_string := args[1..].join(' ')
	params := args_string.all_before('build-examples')
	
	if testing.v_build_failing(params, 'examples'){
		exit(1)
	}

	if testing.v_build_failing(params + '-live', filepath.join( 'examples', 'hot_reload')){
		exit(1)
	}

}
