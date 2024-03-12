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
import v.util.vtest

const vroot = @VEXEROOT
const tdir = os.join_path(vroot, 'vlib', 'v', 'fmt', 'tests')
const fpref = &pref.Preferences{
	is_fmt: true
}

fn test_fmt() {
	fmt_message := 'checking that v fmt keeps already formatted files *unchanged*'
	eprintln(term.header(fmt_message, '-'))
	mut input_files := []string{}
	mut ref := &input_files
	os.walk(tdir, fn [mut ref] (path string) {
		if path.ends_with('_keep.vv') || path.ends_with('_expected.vv') {
			ref << path
		}
	})
	input_files = vtest.filter_vtest_only(input_files)
	assert input_files.len > 0
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
	assert fmt_bench.nfail == 0
}

fn test_fmt_vmodules() {
	os.setenv('VMODULES', tdir, true)
	os.chdir(tdir)!
	test_fmt()
}
