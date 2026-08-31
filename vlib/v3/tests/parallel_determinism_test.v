import os
import rand
import strings
import v3.cmdexec

const determinism_vexe = @VEXE
const determinism_tests_dir = os.dir(@FILE)
const determinism_v3_dir = os.dir(determinism_tests_dir)
const determinism_vlib_dir = os.dir(determinism_v3_dir)
const determinism_v3_src = os.join_path(determinism_v3_dir, 'v3.v')

fn determinism_build_v3() string {
	bin := os.join_path(os.temp_dir(), 'v3_parallel_determinism_${os.getpid()}_${rand.ulid()}')
	result := cmdexec.run(determinism_vexe, ['-gc', 'none', '-path',
		'${determinism_vlib_dir}|@vlib|@vmodules', '-o', bin, determinism_v3_src])
	assert result.exit_code == 0, result.output
	return bin
}

fn determinism_write_project() string {
	root := os.join_path(os.temp_dir(),
		'v3_parallel_determinism_project_${os.getpid()}_${rand.ulid()}')
	os.mkdir_all(root) or { panic(err) }
	os.write_file(os.join_path(root, 'v.mod'), 'Module { name: "parallel_determinism" }\n') or {
		panic(err)
	}
	mut calls := strings.new_builder(32_000)
	calls.writeln('module main')
	calls.writeln('fn main() {')
	calls.writeln('\tmut total := 0')
	for file_index in 0 .. 12 {
		mut source := strings.new_builder(16_000)
		source.writeln('module main')
		for fn_index in 0 .. 40 {
			id := file_index * 40 + fn_index
			source.writeln('fn deterministic_${id}(x int) int { return x + ${id} }')
			calls.writeln('\ttotal += deterministic_${id}(${id})')
		}
		os.write_file(os.join_path(root, 'part_${file_index}.v'), source.str()) or { panic(err) }
	}
	calls.writeln('\tprintln(total)')
	calls.writeln('}')
	os.write_file(os.join_path(root, 'main.v'), calls.str()) or { panic(err) }
	return root
}

fn determinism_diff_context(left []u8, right []u8) string {
	limit := if left.len < right.len { left.len } else { right.len }
	mut first := limit
	for idx in 0 .. limit {
		if left[idx] != right[idx] {
			first = idx
			break
		}
	}
	start := if first > 120 { first - 120 } else { 0 }
	left_end := if first + 240 < left.len { first + 240 } else { left.len }
	right_end := if first + 240 < right.len { first + 240 } else { right.len }
	return 'first difference at byte ${first}; lengths ${left.len}/${right.len}\nparallel:\n${left[start..left_end].bytestr()}\nserial:\n${right[start..right_end].bytestr()}'
}

fn test_parallel_pipeline_matches_serial_generated_c() {
	v3_bin := determinism_build_v3()
	root := determinism_write_project()
	parallel_c := os.join_path(root, 'parallel.c')
	serial_c := os.join_path(root, 'serial.c')
	defer {
		os.rm(v3_bin) or {}
		os.rmdir_all(root) or {}
	}
	old_jobs := os.getenv_opt('VJOBS')
	os.setenv('VJOBS', '4', true)
	defer {
		if jobs := old_jobs {
			os.setenv('VJOBS', jobs, true)
		} else {
			os.unsetenv('VJOBS')
		}
	}
	parallel := cmdexec.run(v3_bin, ['-o', parallel_c, root])
	assert parallel.exit_code == 0, parallel.output
	serial := cmdexec.run(v3_bin, ['-no-parallel', '-o', serial_c, root])
	assert serial.exit_code == 0, serial.output
	parallel_bytes := os.read_bytes(parallel_c) or { panic(err) }
	serial_bytes := os.read_bytes(serial_c) or { panic(err) }
	if parallel_bytes != serial_bytes {
		panic(determinism_diff_context(parallel_bytes, serial_bytes))
	}
}
