module main

import os
import testing
import v.util

const (
	vet_known_failing_exceptions    = [
		'nonexistent',
	]
	vet_folders                     = [
		'vlib/sqlite',
		'vlib/v',
		'cmd/v',
		'cmd/tools',
	]
	verify_known_failing_exceptions = [
		'nonexistant',
	]
	vfmt_verify_list                = [
		'cmd/v/v.v',
		'vlib/builtin/array.v',
		'vlib/os/file.v',
		'vlib/math/bits/bits.v',
		'vlib/time/time.v',
		'vlib/term/colors.v',
		'vlib/term/term.v',
		'vlib/v/ast/',
		'vlib/v/builder/',
		'vlib/v/cflag/',
		'vlib/v/checker/',
		'vlib/v/depgraph/',
		'vlib/v/doc/',
		'vlib/v/errors/',
		'vlib/v/eval/',
		'vlib/v/fmt/',
		'vlib/v/gen/auto_str_methods.v',
		'vlib/v/gen/cgen.v',
		'vlib/v/gen/cgen_test.v',
		'vlib/v/gen/cmain.v',
		'vlib/v/gen/comptime.v',
		'vlib/v/gen/fn.v',
		'vlib/v/gen/json.v',
		'vlib/v/gen/live.v',
		'vlib/v/gen/profile.v',
		'vlib/v/gen/sql.v',
		'vlib/v/gen/str.v',
		'vlib/v/gen/x64/elf.v',
		'vlib/v/gen/x64/elf_obj.v',
		'vlib/v/gen/x64/gen.v',
		'vlib/v/parser/',
		'vlib/v/pref/',
		'vlib/v/scanner/',
		'vlib/v/table/',
		'vlib/v/util/',
		'vlib/v/vet/',
		'vlib/v/vmod/',
		'vlib/gg/gg.v',
	]
)

fn main() {
	args := os.args
	args_string := args[1..].join(' ')
	pass_args := args_string.all_before('test-cleancode')
	v_test_vetting(pass_args)
}

fn tsession(vargs string, tool_source string, tool_cmd string, tool_args string, flist []string, slist []string) testing.TestSession {
	testing.setup_new_vtmp_folder()
	util.prepare_tool_when_needed(tool_source)
	testing.eheader('Run `$tool_cmd` over most .v files')
	mut test_session := testing.new_test_session('$vargs $tool_args')
	test_session.files << flist
	test_session.skip_files << slist
	test_session.test()
	eprintln(test_session.benchmark.total_message('running `$tool_cmd` over most .v files'))
	return test_session
}

fn v_test_vetting(vargs string) {
	vet_session := tsession(vargs, 'vvet.v', 'v vet', 'vet', vet_folders, vet_known_failing_exceptions)
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
