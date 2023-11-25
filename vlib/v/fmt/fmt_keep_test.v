// Copyright (c) 2019-2023 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
import os
import term
import benchmark
import v.fmt
import v.parser
import v.ast
import v.pref
import v.util.diff
import v.util.vtest

const error_missing_vexe = 1
const error_failed_tests = 2
const fpref = &pref.Preferences{
	is_fmt: true
}
const vexe = os.getenv('VEXE')

fn test_fmt() {
	fmt_message := 'checking that v fmt keeps already formatted files *unchanged*'
	eprintln(term.header(fmt_message, '-'))
	if vexe.len == 0 || !os.exists(vexe) {
		eprintln('VEXE must be set')
		exit(error_missing_vexe)
	}
	vroot := os.dir(vexe)
	os.chdir(vroot) or {}
	basepath := vroot + '/'
	tmpfolder := os.temp_dir()
	diff_cmd := diff.find_working_diff_command() or { '' }
	mut fmt_bench := benchmark.new_benchmark()
	keep_input_files := os.walk_ext('vlib/v/fmt/tests', '_keep.vv')
	expected_input_files := os.walk_ext('vlib/v/fmt/tests', '_expected.vv')
	mut input_files := []string{}
	input_files << keep_input_files
	input_files << expected_input_files
	input_files = vtest.filter_vtest_only(input_files, basepath: vroot)
	input_files.sort()
	fmt_bench.set_total_expected_steps(input_files.len + 1)
	for istep, ipath in input_files {
		fmt_bench.cstep = istep + 1
		fmt_bench.step()
		ifilename := os.file_name(ipath)
		vrelpath := ipath.replace(basepath, '')
		opath := ipath
		expected_ocontent := os.read_file(opath) or {
			fmt_bench.fail()
			eprintln(fmt_bench.step_message_fail('cannot read from ${vrelpath}'))
			continue
		}
		table := ast.new_table()
		file_ast := parser.parse_file(ipath, table, .parse_comments, fpref)
		result_ocontent := fmt.fmt(file_ast, table, fpref, false)
		if expected_ocontent != result_ocontent {
			fmt_bench.fail()
			eprintln(fmt_bench.step_message_fail('file ${vrelpath} after formatting, does not look as expected.'))
			if diff_cmd == '' {
				eprintln('>> sorry, but no working "diff" CLI command can be found')
				continue
			}
			vfmt_result_file := os.join_path(tmpfolder, 'vfmt_run_over_${ifilename}')
			os.write_file(vfmt_result_file, result_ocontent) or { panic(err) }
			eprintln(diff.color_compare_files(diff_cmd, opath, vfmt_result_file))
			continue
		}
		fmt_bench.ok()
		eprintln(fmt_bench.step_message_ok(vrelpath))
	}
	fmt_bench.stop()
	eprintln(term.h_divider('-'))
	eprintln(fmt_bench.total_message(fmt_message))
	if fmt_bench.nfail > 0 {
		exit(error_failed_tests)
	}
}
