import os

// The runtime's own `array` struct is typed into C under that name, and a local that
// takes the name for itself hides the type from the rest of the function it is in, and the generated code still spells the type: slicing a field of
// a parameter called `array` builds an `(array[]){...}` compound literal, and inside
// that function `array` no longer names a type.
//
//   error: expected expression
//     return array__clone(&((array[]){array_slice(array.values, 0, array.cols)})[0]);

const local_shadow_vexe = @VEXE
const local_shadow_tests_dir = os.dir(@FILE)
const local_shadow_v3_dir = os.dir(local_shadow_tests_dir)
const local_shadow_vlib_dir = os.dir(local_shadow_v3_dir)
const local_shadow_v3_src = os.join_path(local_shadow_v3_dir, 'v.v')

fn local_shadow_build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_local_shadow_compiler_${os.getpid()}')
	os.rm(v3_bin) or {}
	build := os.execute('${local_shadow_vexe} -gc none -path "${local_shadow_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${local_shadow_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn local_shadow_build_and_run(v3_bin string, root string, source string) os.Result {
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	main_v := os.join_path(root, 'main.v')
	os.write_file(main_v, source) or { panic(err) }
	exe := os.join_path(root, 'prog')
	compile := os.execute('${v3_bin} -nocache ${main_v} -b c -o ${exe}')
	if compile.exit_code != 0 {
		return compile
	}
	return os.execute(exe)
}

fn test_a_local_named_like_a_runtime_type_does_not_hide_it() {
	v3_bin := local_shadow_build_v3()
	root := os.join_path(os.vtmp_dir(), 'v3_local_shadow_${os.getpid()}')
	defer {
		os.rmdir_all(root) or {}
	}
	res := local_shadow_build_and_run(v3_bin, root, 'struct Arg {
	values []int
	cols   int
}

fn slice_of(array Arg) []int {
	return array.values[0..array.cols].clone()
}

fn main() {
	a := Arg{ values: [1, 2, 3], cols: 2 }
	println(slice_of(a))
}
')
	assert res.exit_code == 0, res.output
	assert res.output.trim_space().split('\n').map(it.trim_space()) == ['[1, 2]'], res.output
}

// Wherever such a local is introduced, the name it is declared under has to be the
// name the body will go on using. An if guard, an if guard over several values, a
// multi-return declaration and a smartcast each introduce one, and each of them once
// wrote the declaration through the plain name while the body read the renamed one.
fn test_every_way_of_introducing_such_a_local_agrees_on_its_name() {
	v3_bin := local_shadow_build_v3()
	root := os.join_path(os.vtmp_dir(), 'v3_local_shadow_bindings_${os.getpid()}')
	defer {
		os.rmdir_all(root) or {}
	}
	res := local_shadow_build_and_run(v3_bin, root, 'struct Box {
	n int
}

type Shape = Box | int

fn maybe() ?Box {
	return Box{ n: 5 }
}

fn maybe_pair() ?(int, int) {
	return 3, 4
}

fn pair() (int, int) {
	return 1, 2
}

fn from_guard() int {
	if array := maybe() {
		return array.n
	}
	return -1
}

fn from_guard_pair() int {
	if array, other := maybe_pair() {
		return array + other
	}
	return -1
}

fn from_multi_return() int {
	array, other := pair()
	return array + other
}

fn from_smartcast(array Shape) int {
	if array is Box {
		return array.n
	}
	return -1
}

fn main() {
	println(from_guard())
	println(from_guard_pair())
	println(from_multi_return())
	println(from_smartcast(Shape(Box{ n: 9 })))
}
')
	assert res.exit_code == 0, res.output
	assert res.output.trim_space().split('\n').map(it.trim_space()) == ['5', '7', '3', '9'],
		res.output
}
