// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
import os
import term
import benchmark
import v.ast
import v.fmt
import v.parser
import v.pref
import v.util.diff

const error_missing_vexe = 1
const error_failed_tests = 2
const fpref = &pref.Preferences{
	is_fmt: true
}

fn test_fmt() {
	fmt_message := 'vfmt tests'
	eprintln(term.header(fmt_message, '-'))
	vexe := os.getenv('VEXE')
	if vexe.len == 0 || !os.exists(vexe) {
		eprintln('VEXE must be set')
		exit(error_missing_vexe)
	}
	vroot := os.dir(vexe)
	tmpfolder := os.temp_dir()
	diff_cmd := diff.find_working_diff_command() or { '' }
	mut fmt_bench := benchmark.new_benchmark()
	// Lookup the existing test _input.vv files:
	input_files := os.walk_ext('${vroot}/vlib/v/fmt/tests', '_input.vv')
	fmt_bench.set_total_expected_steps(input_files.len)
	for istep, ipath in input_files {
		fmt_bench.cstep = istep
		fmt_bench.step()
		ifilename := os.file_name(ipath)
		opath := ipath.replace('_input.vv', '_expected.vv')
		if !os.exists(opath) {
			fmt_bench.fail()
			eprintln(fmt_bench.step_message_fail('missing file ${opath}'))
			continue
		}
		expected_ocontent := os.read_file(opath) or {
			fmt_bench.fail()
			eprintln(fmt_bench.step_message_fail('cannot read from ${opath}'))
			continue
		}
		table := ast.new_table()
		file_ast := parser.parse_file(ipath, table, .parse_comments, fpref)
		result_ocontent := fmt.fmt(file_ast, table, fpref, false)
		if expected_ocontent != result_ocontent {
			fmt_bench.fail()
			eprintln(fmt_bench.step_message_fail('file ${ipath} after formatting, does not look as expected.'))
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
		eprintln(fmt_bench.step_message_ok('${ipath}'))
	}
	fmt_bench.stop()
	eprintln(term.h_divider('-'))
	eprintln(fmt_bench.total_message(fmt_message))
	if fmt_bench.nfail > 0 {
		exit(error_failed_tests)
	}
}
