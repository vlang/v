module main

import os
import testing
import v.util

const (
	vet_known_failing_exceptions    = []string{}
	vet_folders                     = [
		'vlib/sqlite',
		'vlib/v',
		'cmd/v',
		'cmd/tools',
		'examples/2048',
		'examples/tetris',
		'examples/term.ui',
	]
	verify_known_failing_exceptions = []string{}
	vfmt_verify_list                = [
		'cmd/v/v.v',
		'cmd/tools/vdoc/',
		'cmd/tools/vvet/',
		'vlib/arrays/',
		'vlib/benchmark/',
		'vlib/bitfield/',
		'vlib/builtin/array.v',
		'vlib/builtin/array_test.v',
		'vlib/builtin/string.v',
		'vlib/builtin/map.v',
		'vlib/math/bits/bits.v',
		'vlib/orm/',
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
		/* 'vlib/v/gen/js/', */
		'vlib/v/gen/x64/',
		'vlib/v/live/',
		'vlib/v/markused/',
		'vlib/v/parser/',
		/* 'vlib/v/pkgconfig/', */
		'vlib/v/pref/',
		'vlib/v/preludes',
		'vlib/v/scanner/',
		'vlib/v/table/',
		/* 'vlib/v/tests/', */
		'vlib/v/token/',
		'vlib/v/util/',
		'vlib/v/vcache/',
		'vlib/v/vet/',
		'vlib/v/vmod/',
		'vlib/cli/',
		'vlib/flag/',
		'vlib/gg/gg.v',
		'vlib/math/big/',
		'vlib/os/',
		'vlib/semver/',
		'vlib/strings/',
		'vlib/time/',
		'vlib/vweb/',
		'vlib/x/websocket/',
	]
)

const vexe = os.getenv('VEXE')

const vroot = os.dir(vexe)

fn main() {
	args_string := os.args[1..].join(' ')
	pass_args := args_string.all_before('test-cleancode')
	v_test_vetting(pass_args)
}

fn tsession(vargs string, tool_source string, tool_cmd string, tool_args string, flist []string, slist []string) testing.TestSession {
	os.chdir(vroot)
	testing.eheader('Run `$tool_cmd` over most .v files')
	mut test_session := testing.new_test_session('$vargs $tool_args')
	test_session.files << flist
	test_session.skip_files << slist
	util.prepare_tool_when_needed(tool_source)
	// note that util.prepare_tool_when_needed will put its temporary files
	// in the VTMP from the test session too, so they will be cleaned up
	// at the end
	test_session.test()
	eprintln(test_session.benchmark.total_message('running `$tool_cmd` over most .v files'))
	return test_session
}

fn v_test_vetting(vargs string) {
	vet_session := tsession(vargs, 'vvet', 'v vet', 'vet', vet_folders, vet_known_failing_exceptions)
	verify_session := tsession(vargs, 'vfmt.v', 'v fmt -verify', 'fmt -verify', vfmt_verify_list,
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
