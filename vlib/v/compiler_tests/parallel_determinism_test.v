import os
import strings
import v.cmdexec
import v.tempname

fn determinism_write_project() string {
	root := os.join_path(os.vtmp_dir(), 'v3_parallel_determinism_project_${tempname.unique_token()}')
	os.mkdir(root) or { panic(err) }
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
	root := determinism_write_project()
	parallel_c := os.join_path(root, 'parallel.c')
	serial_c := os.join_path(root, 'serial.c')
	defer { os.rmdir_all(root) or {} }
	env := {
		'VJOBS': '4'
	}
	parallel := cmdexec.run_in_merged_with_env(@VEXE, ['-o', parallel_c, root], '', env)
	assert parallel.exit_code == 0, parallel.output
	serial := cmdexec.run_in_merged_with_env(@VEXE, ['-no-parallel', '-o', serial_c, root], '', env)
	assert serial.exit_code == 0, serial.output
	parallel_bytes := os.read_bytes(parallel_c) or { panic(err) }
	serial_bytes := os.read_bytes(serial_c) or { panic(err) }
	if parallel_bytes != serial_bytes {
		panic(determinism_diff_context(parallel_bytes, serial_bytes))
	}
}
