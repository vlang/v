module main

import os
import testing
import v.util

const (
	known_failing_exceptions = [
		'vlib/crypto/aes/const.v',
		// multiple narrow columns of []string turned to 1 long single column, otherwise works
		'vlib/v/gen/js/tests/life.v',
		// error: unexpected `,`, expecting ), on JS.setInterval(fn () { show(game) game = step(game) }, 500)
		'vlib/builtin/js/builtin.v',
		// JS.console.error(s) => JS.error(s), JS.process.exit(c) => JS.exit(c)
		'vlib/builtin/js/jsfns_node.js.v',
		'vlib/builtin/js/jsfns.js.v',
		'vlib/builtin/js/jsfns_browser.js.v',
		// error: expr(): bad token `asm`, on `asm {}`
		'vlib/builtin/bare/linuxsys_bare.v',
		// total chaos (duplicated code several times) in array_eq_test.v
		'vlib/builtin/array_eq_test.v',
		// the fn args are removed, then `cb fn (picohttpparser.Request, mut picohttpparser.Response)` can not be reparsed
		'vlib/picoev/picoev.v',
	]
)

fn main() {
	args_string := os.args[1..].join(' ')
	v_test_formatting(args_string.all_before('test-fmt'))
}

fn v_test_formatting(vargs string) {
	all_v_files := v_files()
	util.prepare_tool_when_needed('vfmt.v')
	testing.eheader('Run "v fmt" over all .v files')
	mut vfmt_test_session := testing.new_test_session('$vargs fmt -worker')
	vfmt_test_session.files << all_v_files
	vfmt_test_session.skip_files << known_failing_exceptions
	vfmt_test_session.test()
	eprintln(vfmt_test_session.benchmark.total_message('running vfmt over V files'))
	if vfmt_test_session.benchmark.nfail > 0 {
		eprintln('\nWARNING: v fmt failed $vfmt_test_session.benchmark.nfail times.\n')
		exit(1)
	}
}

fn v_files() []string {
	mut files_that_can_be_formatted := []string{}
	all_test_files := os.walk_ext('.', '.v')
	for tfile in all_test_files {
		if tfile.starts_with('./vlib/v/cgen/tests') {
			continue
		}
		files_that_can_be_formatted << tfile
	}
	return files_that_can_be_formatted
}
