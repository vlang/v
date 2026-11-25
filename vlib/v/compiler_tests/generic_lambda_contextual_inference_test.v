import os
import v.tempname

fn test_generic_lambda_infers_return_from_contextual_parameter() {
	root := os.join_path(os.vtmp_dir(), 'v3_generic_lambda_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	source_path := os.join_path(root, 'main.v')
	output_path := os.join_path(root, 'app')
	os.write_file(source_path, "struct Box[T] {
	value T
}

fn (value Box[T]) map[U](op fn (T) U) Box[U] {
	return Box[U]{
		value: op(value.value)
	}
}

fn apply[T, U](value T, op fn (T) U) U {
	return op(value)
}

fn main() {
	mapped := Box[string]{value: 'hello'}.map(|s| s.len)
	applied := apply('world', |s| s.len)
	println(mapped.value)
	println(applied)
}
") or {
		panic(err)
	}
	compile := os.exec([@VEXE, '-o', output_path, source_path])
	assert compile.exit_code == 0, compile.output
	run := os.exec([output_path])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '5\n5'
}
