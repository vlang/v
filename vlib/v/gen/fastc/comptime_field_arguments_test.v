module fastc

import os
import v.cmdexec

fn test_fastc_comptime_field_metadata_preserves_argument_delimiters() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_field_arguments_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), r'module main

struct Entry {
	name string
	hidden string @[skip]
}

struct State {
	numbers shared map[string]int
	plain int
}

fn count(name string, attrs []string) int {
	assert name != ""
	return attrs.len
}

fn followed(attrs []string, suffix string) int {
	assert suffix == "ok"
	return attrs.len
}

fn inspect[T](value T) int {
	mut total := 0
	$for field in T.fields {
		total += count(field.name, field.attrs)
		total += followed(field.attrs, "ok")
	}
	return total
}

fn count_shared[T]() int {
	mut count := 0
	$for field in T.fields {
		$if field.is_shared { count++ }
	}
	return count
}

fn main() {
	assert inspect(Entry{}) == 2
	assert count_shared[Entry]() == 0
	assert count_shared[State]() == 1
}
') or { panic(err) }
	program := os.join_path(root, 'program')
	build := cmdexec.run(@VEXE, ['-b', 'fastc', '-nocache', '-o', program, root])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
}
