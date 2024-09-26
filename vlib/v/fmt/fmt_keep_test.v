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
const fpref = &pref.Preferences{
	is_fmt: true
}

fn run_fmt(mut input_files []string) {
	fmt_message := 'checking that v fmt keeps already formatted files *unchanged*'
	eprintln(term.header(fmt_message, '-'))
	assert input_files.len > 0
	input_files = vtest.filter_vtest_only(input_files)
	if input_files.len == 0 {
		// No need to produce a failing test here.
		eprintln('no tests found with VTEST_ONLY filter set to: ' + os.getenv('VTEST_ONLY'))
		exit(0)
	}
	input_files.sort()
	mut fmt_bench := benchmark.new_benchmark()
	fmt_bench.set_total_expected_steps(input_files.len + 1)
	tmpfolder := os.temp_dir()
	for istep, ipath in input_files {
		fmt_bench.cstep = istep + 1
		fmt_bench.step()
		expected_ocontent := os.read_file(ipath) or {
			fmt_bench.fail()
			eprintln(fmt_bench.step_message_fail('cannot read from ${ipath}'))
			continue
		}
		mut table := ast.new_table()
		file_ast := parser.parse_file(ipath, mut table, .parse_comments, fpref)
		result_ocontent := fmt.fmt(file_ast, mut table, fpref, false)
		if expected_ocontent != result_ocontent {
			fmt_bench.fail()
			eprintln(fmt_bench.step_message_fail('file ${ipath} after formatting, does not look as expected.'))
			vfmt_result_file := os.join_path(tmpfolder, 'vfmt_run_over_${os.file_name(ipath)}')
			os.write_file(vfmt_result_file, result_ocontent) or { panic(err) }
			println(diff.compare_files(ipath, vfmt_result_file) or { err.msg() })
			continue
		}
		fmt_bench.ok()
		eprintln(fmt_bench.step_message_ok(ipath))
	}
	fmt_bench.stop()
	eprintln(term.h_divider('-'))
	eprintln(fmt_bench.total_message(fmt_message))
	assert fmt_bench.nfail == 0
}

fn get_test_files(path string) []string {
	mut files := []string{}
	mut ref := &files
	os.walk(path, fn [mut ref] (p string) {
		if p.ends_with('_keep.vv') || p.ends_with('_expected.vv') {
			ref << p
		}
	})
	return files
}

fn test_fmt() {
	mut input_files := get_test_files(os.join_path(vroot, 'vlib', 'v', 'fmt', 'tests'))
	run_fmt(mut input_files)
}

fn test_fmt_vmodules() {
	vmodules_tdir := os.join_path(vroot, 'vlib', 'v', 'fmt', 'testdata', 'vmodules')
	os.setenv('VMODULES', vmodules_tdir, true)
	mut input_files := get_test_files(vmodules_tdir)
	run_fmt(mut input_files)
}
