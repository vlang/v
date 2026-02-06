// Copyright (c) 2025 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
import os
import benchmark
import v.ast
import v.fmt
import v.parser
import v.pref
import v.util.diff
import v.util.vtest

const tmpfolder = os.temp_dir()
const fpref = &pref.Preferences{
	is_fmt: true
}

fn test_new_int_fmt() {
	suffix_for_expected := '_expected_new_int.vv'
	suffix_for_input := '_input.vv'
	mut input_files := os.walk_ext(os.join_path(@VEXEROOT, 'vlib/v/fmt/tests'), suffix_for_expected)
	input_files = input_files.map(it.replace(suffix_for_expected, suffix_for_input))
	input_files = vtest.filter_vtest_only(input_files)
	if input_files.len == 0 {
		eprintln('no tests found with VTEST_ONLY filter set to: ' + os.getenv('VTEST_ONLY'))
		exit(0)
	}
	mut fmt_bench := benchmark.new_benchmark()
	fmt_bench.set_total_expected_steps(input_files.len)
	for istep, ipath in input_files {
		fmt_bench.cstep = istep
		fmt_bench.step()
		opath := ipath.replace('_input.vv', '_expected_new_int.vv')
		expected_ocontent := os.read_file(opath) or {
			fmt_bench.fail()
			eprintln(fmt_bench.step_message_fail('cannot read from ${opath}'))
			continue
		}
		mut table := ast.new_table()
		file_ast := parser.parse_file(ipath, mut table, .parse_comments, fpref)
		table.new_int = true
		result_ocontent := fmt.fmt(file_ast, mut table, fpref, false)
		if expected_ocontent != result_ocontent {
			fmt_bench.fail()
			eprintln(fmt_bench.step_message_fail('file ${ipath} after formatting, does not look as expected ${opath}.'))
			vfmt_result_file := os.join_path(tmpfolder, 'vfmt_run_over_${os.file_name(ipath)}')
			os.write_file(vfmt_result_file, result_ocontent) or { panic(err) }
			println(diff.compare_files(opath, vfmt_result_file) or { err.msg() })
			continue
		}
		fmt_bench.ok()
		eprintln(fmt_bench.step_message_ok(ipath))
	}
	fmt_bench.stop()
	println(fmt_bench.total_message('vfmt new_int tests'))
	assert fmt_bench.nfail == 0
}
