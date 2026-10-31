module main

$if !conditional_import_script_mode ? {
	import os
}

fn test_conditional_import_does_not_start_script_mode() {
	assert os.args.len > 0
}
