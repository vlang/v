// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
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

const vroot = get_vroot()
const tdir = os.join_path(vroot, 'vlib', 'v', 'fmt', 'tests')
const error_missing_vexe = 1
const error_failed_tests = 2
const fpref = &pref.Preferences{
	is_fmt: true
}

fn get_vroot() string {
	vexe := pref.vexe_path()
	if vexe == '' || !os.exists(vexe) {
		eprintln('VEXE must be set')
		exit(ecode_missing_vexe)
	}
	return os.dir(vexe)
}

fn test_fmt() {
	fmt_message := 'checking that v fmt keeps already formatted files *unchanged*'
	eprintln(term.header(fmt_message, '-'))
	mut input_files := []string{}
	input_files << os.walk_ext(tdir, '_keep.vv')
	input_files << os.walk_ext(tdir, '_expected.vv')
	input_files.sort()
	mut fmt_bench := benchmark.new_benchmark()
	fmt_bench.set_total_expected_steps(input_files.len + 1)
	basepath := vroot + os.path_separator
	tmpfolder := os.temp_dir()
	diff_cmd := diff.find_working_diff_command() or { '' }
	for istep, ipath in input_files {
		fmt_bench.cstep = istep + 1
		fmt_bench.step()
		ifilename := os.file_name(ipath)
		vrelpath := ipath.all_after(basepath)
		expected_ocontent := os.read_file(ipath) or {
			fmt_bench.fail()
			eprintln(fmt_bench.step_message_fail('cannot read from ${vrelpath}'))
			continue
		}
		mut table := ast.new_table()
		file_ast := parser.parse_file(ipath, mut table, .parse_comments, fpref)
		result_ocontent := fmt.fmt(file_ast, mut table, fpref, false)
		if expected_ocontent != result_ocontent {
			fmt_bench.fail()
			eprintln(fmt_bench.step_message_fail('file ${vrelpath} after formatting, does not look as expected.'))
			if diff_cmd == '' {
				eprintln('>> sorry, but no working "diff" CLI command can be found')
				continue
			}
			vfmt_result_file := os.join_path(tmpfolder, 'vfmt_run_over_${ifilename}')
			os.write_file(vfmt_result_file, result_ocontent) or { panic(err) }
			eprintln(diff.color_compare_files(diff_cmd, ipath, vfmt_result_file))
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

fn test_fmt_vmodules() {
	os.setenv('VMODULES', tdir, true)
	os.chdir(tdir)!
	test_fmt()
}
