import os
import v.tempname

fn test_generic_sum_str_is_used_for_string_interpolation() {
	root := os.join_path(os.vtmp_dir(), 'v3_generic_sum_str_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	bin := os.join_path(root, 'v3_generic_sum_str_input')
	src := bin + '.v'
	os.write_file(src, "type Maybe[T] = Empty | Some[T]

struct Empty {}

struct Some[T] {
	value T
}

fn (m Maybe[T]) str[T]() string {
	return 'maybe'
}

fn main() {
	x := Maybe[int](Some[int]{
		value: 7
	})
	interpolated := 'value \${x}'
	assert interpolated == 'value maybe'
	println(interpolated)
}
") or {
		panic(err)
	}

	compile := os.exec([@VEXE, src, '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'value maybe'

	generated := os.read_file(bin + '.c') or { panic(err) }
	assert generated.contains('Maybe_int__str'), generated
	assert !generated.contains(' = x;'), generated
	assert !generated.contains('Maybe_T__str'), generated
}

fn test_monomorphization_synthesizes_late_auto_str_helpers() {
	root := os.join_path(os.vtmp_dir(), 'v3_late_auto_str_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	vlib_dir := os.dir(os.dir(os.dir(@FILE)))
	src := os.join_path(vlib_dir, 'toml', 'tests', 'reflect_test.v')
	bin := os.join_path(root, 'v3_late_auto_str_test')
	compile := os.exec([@VEXE, src, '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	run := os.exec([bin])
	assert run.exit_code == 0, run.output
}
