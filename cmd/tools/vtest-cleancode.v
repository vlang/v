module main

import os
import testing
import v.util

const (
	vet_known_failing_exceptions    = []string{}
	vet_folders                     = [
		'vlib/sqlite',
		'vlib/v',
		'vlib/x/ttf/',
		'cmd/v',
		'cmd/tools',
		'examples/2048',
		'examples/tetris',
		'examples/term.ui',
	]
	verify_known_failing_exceptions = [
		'vlib/builtin/int_test.v' /* special number formatting that should be tested */,
		'vlib/gg/m4/graphic.v' /* has hand crafted meaningful formatting of matrices */,
		'vlib/gg/m4/m4_test.v' /* has hand crafted meaningful formatting of matrices */,
		'vlib/gg/m4/matrix.v' /* has hand crafted meaningful formatting of matrices */,
		'vlib/v/tests/array_append_short_struct_test.v', /* extra empty line */
		'vlib/v/tests/fixed_array_const_size_test.v', /* fixed arr type is changed */
		'vlib/v/tests/fn_high_test.v', /* param name removed */
		'vlib/v/tests/fn_test.v', /* bad comment formatting */
		'vlib/v/tests/generics_return_generics_struct_test.v', /* generic fn param removed */
		'vlib/v/tests/interop_test.v', /* bad comment formatting */
		'vlib/v/tests/generics_test.v', /* some silent error */
		'vlib/v/gen/js/tests/js.v', /* local `hello` fn, gets replaced with module `hello` aliased as `hl` */
	]
	vfmt_verify_list                = [
		'cmd/',
		'vlib/arrays/',
		'vlib/benchmark/',
		'vlib/bitfield/',
		'vlib/builtin/',
		'vlib/cli/',
		'vlib/dl/',
		'vlib/flag/',
		'vlib/gg/',
		'vlib/math/bits/bits.v',
		'vlib/orm/',
		'vlib/runtime/',
		'vlib/term/colors.v',
		'vlib/term/term.v',
		'vlib/v/ast/',
		'vlib/v/builder/',
		'vlib/v/cflag/',
		'vlib/v/checker/',
		'vlib/v/depgraph/',
		'vlib/v/doc/',
		'vlib/v/embed_file/',
		'vlib/v/errors/',
		'vlib/v/eval/',
		'vlib/v/fmt/',
		'vlib/v/gen/c/',
		'vlib/v/gen/js/',
		'vlib/v/gen/x64/',
		'vlib/v/live/',
		'vlib/v/markused/',
		'vlib/v/parser/',
		'vlib/v/pkgconfig/',
		'vlib/v/pref/',
		'vlib/v/preludes',
		'vlib/v/scanner/',
		'vlib/v/tests/',
		'vlib/v/token/',
		'vlib/v/util/',
		'vlib/v/vcache/',
		'vlib/v/vet/',
		'vlib/v/vmod/',
		'vlib/cli/',
		'vlib/flag/',
		'vlib/math/big/',
		'vlib/os/',
		'vlib/semver/',
		'vlib/strings/',
		'vlib/time/',
		'vlib/vweb/',
		'vlib/x/websocket/',
	]
)

const (
	vexe   = os.getenv('VEXE')
	vroot  = os.dir(vexe)
	is_fix = '-fix' in os.args
)

fn main() {
	args_string := os.args[1..].join(' ')
	pass_args := args_string.all_before('test-cleancode')
	v_test_vetting(pass_args)
}

fn tsession(vargs string, tool_source string, tool_cmd string, tool_args string, flist []string, slist []string) testing.TestSession {
	os.chdir(vroot)
	title_message := 'running $tool_cmd over most .v files'
	testing.eheader(title_message)
	mut test_session := testing.new_test_session('$vargs $tool_args')
	test_session.files << flist
	test_session.skip_files << slist
	util.prepare_tool_when_needed(tool_source)
	// note that util.prepare_tool_when_needed will put its temporary files
	// in the VTMP from the test session too, so they will be cleaned up
	// at the end
	test_session.test()
	eprintln(test_session.benchmark.total_message(title_message))
	return test_session
}

fn v_test_vetting(vargs string) {
	vet_session := tsession(vargs, 'vvet', 'v vet', 'vet', vet_folders, vet_known_failing_exceptions)
	fmt_cmd, fmt_args := if is_fix { 'v fmt -w', 'fmt -w' } else { 'v fmt -verify', 'fmt -verify' }
	expanded_vfmt_list := util.find_all_v_files(vfmt_verify_list) or { return }
	verify_session := tsession(vargs, 'vfmt.v', fmt_cmd, fmt_args, expanded_vfmt_list,
		verify_known_failing_exceptions)
	//
	if vet_session.benchmark.nfail > 0 || verify_session.benchmark.nfail > 0 {
		eprintln('\n')
		if vet_session.benchmark.nfail > 0 {
			eprintln('WARNING: `v vet` failed $vet_session.benchmark.nfail times.')
		}
		if verify_session.benchmark.nfail > 0 {
			eprintln('WARNING: `v fmt -verify` failed $verify_session.benchmark.nfail times.')
		}
		exit(1)
	}
}
