import os
import term
import benchmark

fn test_wasm_when_decompiled_must_have() {
	vexe := @VEXE
	vroot := os.dir(vexe).replace(os.path_separator, '/')
	decompiler := os.find_abs_path_of_executable('wasm-decompile') or {
		eprintln('> skipping test, since it needs `wasm-decompile`, which is not present')
		return
	}

	dir := vroot + '/vlib/v/gen/wasm/tests_decompile'
	vv_files := os.walk_ext(dir, '.vv')
	assert vv_files.len > 0

	mut bench := benchmark.new_benchmark()
	bench.set_total_expected_steps(vv_files.len)

	wrkdir := os.join_path(os.vtmp_dir(), 'wasm_tests_decompile')
	os.mkdir_all(wrkdir)!
	os.chdir(wrkdir)!

	vv_files_loop: for vv_file in vv_files {
		bench.step()
		must_have_path := vv_file.replace('.vv', '.wasm.must_have')
		vv_file_name := os.file_name(vv_file).replace('.vv', '')
		full_vv_path := os.real_path(vv_file).replace(os.path_separator, '/')
		relative_vv_path := full_vv_path.replace(vroot + '/', '')
		full_wasm_path := '${wrkdir}/${vv_file_name}.wasm'
		full_dcmp_path := '${wrkdir}/${vv_file_name}.dcmp'

		file_options := get_file_options(vv_file)
		cmd := '${os.quoted_path(vexe)} -b wasm ${file_options.vflags} -o ${os.quoted_path(full_wasm_path)} ${os.quoted_path(full_vv_path)}'
		// println(cmd)
		res_wasm := os.execute(cmd)
		if res_wasm.exit_code != 0 {
			bench.fail()
			eprintln(bench.step_message_fail(cmd))
			eprintln('   res_wasm.exit_code: ${res_wasm.exit_code}')
			eprintln('   res_wasm.output   : ${res_wasm.output}')
			continue
		}

		cmd_decompile := '${os.quoted_path(decompiler)} ${os.quoted_path(full_wasm_path)} --output=${os.quoted_path(full_dcmp_path)}'
		// println(cmd_decompile)
		res_dcmp := os.execute(cmd_decompile)
		if res_dcmp.exit_code != 0 {
			bench.fail()
			eprintln(bench.step_message_fail(cmd_decompile))
			eprintln('   res_dcmp.exit_code: ${res_dcmp.exit_code}')
			eprintln('   res_dcmp.output   : ${res_dcmp.output}')
			continue
		}

		expected_lines := os.read_lines(must_have_path) or { [] }
		generated_wasm_lines := os.read_lines(full_dcmp_path) or {
			bench.fail()
			eprintln('missing ${full_dcmp_path}')
			continue
		}

		mut failed_patterns := []string{}
		for idx_expected_line, eline in expected_lines {
			if eline == '' {
				continue
			}
			if !does_line_match_one_of_generated_lines(eline, generated_wasm_lines) {
				failed_patterns << eline
				eprintln('${must_have_path}:${idx_expected_line + 1}: expected match error:')
				eprintln('`${cmd_decompile}` did NOT produce expected line:')
				eprintln(term.colorize(term.red, eline))
				continue
			}
		}
		if failed_patterns.len > 0 {
			eprintln('> failed match patterns: ${failed_patterns.len}')
			bench.fail()
			continue
		}

		bench.ok()
		eprintln(bench.step_message_ok(relative_vv_path))
	}
	bench.stop()
	eprintln(term.h_divider('-'))
	eprintln(bench.total_message('decompiled wasm must have'))
	if bench.nfail > 0 {
		exit(1)
	}
	os.rmdir_all(wrkdir) or {}
}

struct FileOptions {
mut:
	vflags string
}

pub fn get_file_options(file string) FileOptions {
	mut res := FileOptions{}
	lines := os.read_lines(file) or { [] }
	for line in lines {
		if line.starts_with('// vtest vflags:') {
			res.vflags = line.all_after(':').trim_space()
		}
	}
	return res
}

fn does_line_match_one_of_generated_lines(line string, generated_c_lines []string) bool {
	for cline in generated_c_lines {
		if line == cline {
			return true
		}
		if cline.contains(line) {
			return true
		}
	}
	return false
}
