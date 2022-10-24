// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
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

const (
	error_missing_vexe = 1
	error_failed_tests = 2
	b2v_keep_path      = os.join_path('vlib', 'v', 'fmt', 'tests', 'bin2v_keep.vv')
	fpref              = &pref.Preferences{
		is_fmt: true
	}
	vexe               = os.getenv('VEXE')
)

fn test_fmt() {
	fmt_message := 'checking that v fmt keeps already formatted files *unchanged*'
	eprintln(term.header(fmt_message, '-'))
	if vexe.len == 0 || !os.exists(vexe) {
		eprintln('VEXE must be set')
		exit(error_missing_vexe)
	}
	vroot := os.dir(vexe)
	os.chdir(vroot) or {}
	basepath := os.join_path(vroot, '')
	tmpfolder := os.temp_dir()
	diff_cmd := diff.find_working_diff_command() or { '' }
	mut fmt_bench := benchmark.new_benchmark()
	keep_input_files := os.walk_ext('vlib/v/fmt/tests', '_keep.vv')
	expected_input_files := os.walk_ext('vlib/v/fmt/tests', '_expected.vv')
	mut input_files := []string{}
	input_files << keep_input_files
	input_files << expected_input_files
	input_files = vtest.filter_vtest_only(input_files, basepath: vroot)
	fmt_bench.set_total_expected_steps(input_files.len + 1)
	prepare_bin2v_file(mut fmt_bench)
	for istep, ipath in input_files {
		fmt_bench.cstep = istep + 1
		fmt_bench.step()
		ifilename := os.file_name(ipath)
		vrelpath := ipath.replace(basepath, '')
		opath := ipath
		expected_ocontent := os.read_file(opath) or {
			fmt_bench.fail()
			eprintln(fmt_bench.step_message_fail('cannot read from $vrelpath'))
			continue
		}
		table := ast.new_table()
		file_ast := parser.parse_file(ipath, table, .parse_comments, fpref)
		result_ocontent := fmt.fmt(file_ast, table, fpref, false)
		if expected_ocontent != result_ocontent {
			fmt_bench.fail()
			eprintln(fmt_bench.step_message_fail('file $vrelpath after formatting, does not look as expected.'))
			if ipath.ends_with(b2v_keep_path) {
				continue
			}
			if diff_cmd == '' {
				eprintln('>> sorry, but no working "diff" CLI command can be found')
				continue
			}
			vfmt_result_file := os.join_path(tmpfolder, 'vfmt_run_over_$ifilename')
			os.write_file(vfmt_result_file, result_ocontent) or { panic(err) }
			eprintln(diff.color_compare_files(diff_cmd, opath, vfmt_result_file))
			continue
		}
		fmt_bench.ok()
		eprintln(fmt_bench.step_message_ok(vrelpath))
	}
	restore_bin2v_placeholder() or {
		eprintln('failed restoring vbin2v_keep.vv placeholder: $err.msg()')
	}
	fmt_bench.stop()
	eprintln(term.h_divider('-'))
	eprintln(fmt_bench.total_message(fmt_message))
	if fmt_bench.nfail > 0 {
		exit(error_failed_tests)
	}
}

fn prepare_bin2v_file(mut fmt_bench benchmark.Benchmark) {
	fmt_bench.cstep = 0
	fmt_bench.step()
	write_bin2v_keep_content() or {
		fmt_bench.fail()
		eprintln(fmt_bench.step_message_fail('Failed preparing bin2v_keep.vv: $err.msg()'))
		return
	}
	fmt_bench.ok()
	eprintln(fmt_bench.step_message_ok('Prepared bin2v_keep.vv'))
}

fn write_bin2v_keep_content() ! {
	img0 := os.join_path('vlib', 'v', 'embed_file', 'tests', 'v.png')
	img1 := os.join_path('tutorials', 'building_a_simple_web_blog_with_vweb', 'img', 'time.png')
	os.rm(b2v_keep_path)!
	res := os.execute('${os.quoted_path(vexe)} bin2v -w ${os.quoted_path(b2v_keep_path)} ${os.quoted_path(img0)} ${os.quoted_path(img1)}')
	if res.exit_code != 0 {
		restore_bin2v_placeholder() or {}
		return error_with_code(res.output.trim_space(), res.exit_code)
	}
}

fn restore_bin2v_placeholder() ! {
	text := '// This is a placeholder file which will be filled with bin2v output before the test.
// HINT: do NOT delete, move or rename this file!\n'
	os.write_file(b2v_keep_path, text)!
}
