module main

import os
import testing
import v.util
import arrays

const (
	vet_known_failing_exceptions    = []string{}
	vet_folders                     = [
		'vlib/sqlite',
		'vlib/v',
		'vlib/x/json2',
		'vlib/x/ttf',
		'cmd/v',
		'cmd/tools',
		'examples/2048',
		'examples/tetris',
		'examples/term.ui',
	]
	verify_known_failing_exceptions = [
		// Handcrafted meaningful formatting of code parts (mostly arrays)
		'examples/sokol/02_cubes_glsl/cube_glsl.v',
		'examples/sokol/03_march_tracing_glsl/rt_glsl.v',
		'examples/sokol/04_multi_shader_glsl/rt_glsl.v',
		'examples/sokol/05_instancing_glsl/rt_glsl.v',
		'examples/sokol/06_obj_viewer/show_obj.v',
		'vlib/gg/m4/graphic.v',
		'vlib/gg/m4/m4_test.v',
		'vlib/gg/m4/matrix.v',
		'vlib/builtin/int_test.v' /* special number formatting that should be tested */,
		// TODOs and unfixed vfmt bugs
		'vlib/v/gen/js/tests/js.v', /* local `hello` fn, gets replaced with module `hello` aliased as `hl` */
	]
	vfmt_verify_list                = [
		'cmd/',
		'examples/',
		'tutorials/',
		'vlib/',
	]
	vfmt_known_failing_exceptions   = arrays.merge(verify_known_failing_exceptions, [
		'vlib/regex/regex_test.v' /* contains meaningfull formatting of the test case data */,
		'vlib/crypto/sha512/sha512block_generic.v' /* formatting of large constant arrays wraps to too many lines */,
		'vlib/crypto/aes/const.v' /* formatting of large constant arrays wraps to too many lines */,
	])
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
	os.chdir(vroot) or {}
	title_message := 'running $tool_cmd over most .v files'
	testing.eheader(title_message)
	mut test_session := testing.new_test_session('$vargs $tool_args', false)
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
	expanded_vet_list := util.find_all_v_files(vet_folders) or { return }
	vet_session := tsession(vargs, 'vvet', '${os.quoted_path(vexe)} vet', 'vet', expanded_vet_list,
		vet_known_failing_exceptions)
	//
	fmt_cmd, fmt_args := if is_fix {
		'${os.quoted_path(vexe)} fmt -w', 'fmt -w'
	} else {
		'${os.quoted_path(vexe)} fmt -verify', 'fmt -verify'
	}
	vfmt_list := util.find_all_v_files(vfmt_verify_list) or { return }
	exceptions := util.find_all_v_files(vfmt_known_failing_exceptions) or { return }
	verify_session := tsession(vargs, 'vfmt.v', fmt_cmd, fmt_args, vfmt_list, exceptions)
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
