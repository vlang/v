module eval

import os

fn run_eval_backend(code string) !os.Result {
	vexe := os.getenv('VEXE')
	if vexe == '' {
		return error('VEXE is not set')
	}
	vroot := os.dir(vexe)
	tmp_dir := os.join_path(os.temp_dir(), 'v2_eval_integration_${os.getpid()}')
	os.rmdir_all(tmp_dir) or {}
	os.mkdir_all(tmp_dir)!
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	tmp_file := os.join_path(tmp_dir, 'sample.v')
	v2_exe := os.join_path(tmp_dir, 'v2_eval_runner')
	os.write_file(tmp_file, code)!
	build_res := os.execute('${os.quoted_path(vexe)} -o ${os.quoted_path(v2_exe)} ${os.quoted_path(os.join_path(vroot,
		'cmd', 'v2', 'v2.v'))}')
	if build_res.exit_code != 0 {
		return error(build_res.output)
	}
	return os.execute('${os.quoted_path(v2_exe)} -backend eval ${os.quoted_path(tmp_file)}')
}

fn test_eval_function_call_and_for_range() {
	mut e := create()
	e.run_text('
fn sum(n int) int {
	mut acc := 0
	for i in 0 .. n {
		acc += i
	}
	return acc
}

fn main() {
	println(sum(5))
}
') or {
		panic(err)
	}
	assert e.stdout() == '10\n'
}

fn test_eval_if_expr_value() {
	mut e := create()
	e.run_text('
fn main() {
	x := if 3 > 2 {
		41
	} else {
		0
	}
	println(x + 1)
}
') or {
		panic(err)
	}
	assert e.stdout() == '42\n'
}

fn test_eval_array_append_and_string_interpolation() {
	mut e := create()
	e.run_text('
fn main() {
	mut arr := []int{}
	arr << 7
	arr << 9
	println("\${arr[0]}:\${arr[1]}:\${arr.len}")
}
') or {
		panic(err)
	}
	assert e.stdout() == '7:9:2\n'
}

fn test_eval_transformed_array_append_and_string_interpolation() {
	res := run_eval_backend('
fn main() {
	mut arr := []int{}
	arr << 7
	arr << 9
	println("\${arr[0]}:\${arr[1]}:\${arr.len}")
}
') or {
		panic(err)
	}
	if res.exit_code != 0 {
		panic(res.output)
	}
	assert res.output.contains('7:9:2\n')
}
