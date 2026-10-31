import os

fn scalar_array_child(executable string, args []string) (int, string) {
	mut child := os.new_process(executable)
	child.set_args(args)
	child.set_redirect_stdio_merged()
	child.run()
	output := child.stdout_slurp()
	child.wait()
	code := child.code
	child.close()
	return code, output
}

fn test_scalar_array_fast_paths_preserve_bounds_growth_and_overflow_panics() {
	dir := os.join_path(os.vtmp_dir(), 'scalar_array_panic_${os.getpid()}')
	os.mkdir_all(dir)!
	defer {
		os.rmdir_all(dir) or {}
	}
	source := os.join_path(dir, 'main.v')
	program := os.join_path(dir, 'scalar_array_panic')
	os.write_file(source, '
import os

fn clear_for_scalar_store(mut values []i64) i64 {
	values.clear()
	return 42
}

fn main() {
	mut values := []i64{len: 1, cap: 2}
	mode := os.args[1]
	if mode == "negative" || mode == "upper" {
		index := if mode == "negative" { -1 } else { 1 }
		values[index] = 42
	} else if mode == "clear" {
		values[0] = clear_for_scalar_store(mut values)
	} else if mode == "nogrow" {
		unsafe { values.flags.set(.nogrow) }
		values << 1
		values << 2
	} else {
		// A synthetic full header exercises the guard without allocating its capacity.
		unsafe { values.len = max_int; values.cap = max_int }
		values << 42
	}
}
')!
	code, output := scalar_array_child(@VEXE, ['-o', program, source])
	assert code == 0, output
	for mode in ['negative', 'upper', 'clear', 'nogrow', 'overflow'] {
		exit_code, panic_output := scalar_array_child(program, [mode])
		assert exit_code != 0, '${mode}: ${panic_output}'
		if mode in ['negative', 'upper', 'clear'] {
			assert panic_output.contains('array.set: index out of range'), panic_output
		} else if mode == 'nogrow' {
			assert panic_output.contains('array.ensure_cap: array with the flag'), panic_output
		} else {
			assert panic_output.contains('array.push: len bigger than max_int'), panic_output
		}
	}
}
