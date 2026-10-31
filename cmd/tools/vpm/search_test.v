// vtest build: network?
// vtest retry: 3
module main

import test_utils { cmd_ok_args }

fn testsuite_begin() {
	unbuffer_stdout()
}

fn test_search_ui() {
	res := cmd_ok_args(@LOCATION, [vexe, 'search', 'ui'])
	dump(res)
	assert res.output.contains('1. ui'), res.output
}
