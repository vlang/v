// vtest retry: 3
module main

import test_utils { cmd_ok }

fn testsuite_begin() {
	$if !network ? {
		eprintln('> skipping ${@FILE}, when `-d network` is missing')
		exit(0)
	}
}

fn testsuite_end() {
}

fn test_search_ui() {
	res := cmd_ok(@LOCATION, '${vexe} search ui')
	dump(res)
	assert res.output.contains('1. ui'), res.output
}
