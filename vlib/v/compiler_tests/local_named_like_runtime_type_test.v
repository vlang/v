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
	assert res.output.trim_space().split('\n').map(it.trim_space()) == ['5', '7', '3', '9'], res.output
}

// A deferred closure reads the local through a slot declared at the top of the
// function, and that slot is the local: it has to carry the same name every read of
// it uses, or the reads inside the closure name something that was never declared.
fn test_a_deferred_closure_reads_the_local_it_captured() {
	v3_bin := local_shadow_build_v3()
	root := os.join_path(os.vtmp_dir(), 'v3_local_shadow_defer_${os.getpid()}')
	defer {
		os.rmdir_all(root) or {}
	}
	res := local_shadow_build_and_run(v3_bin, root, 'fn with_defer() {
	array := 1
	defer(fn) {
		println(array)
	}
	println("body")
}

fn main() {
	with_defer()
}
')
	assert res.exit_code == 0, res.output
	assert res.output.trim_space().split('\n').map(it.trim_space()) == ['body', '1'], res.output
}

// The renamed name is taken out of the namespace reserved for generated symbols
// rather than built by adding a suffix. A suffix would bring a function that has both
// `array` and a name spelled like the renamed one under a single name: as parameters
// that is two parameters called the same thing, and as locals the outer one quietly
// becomes the inner one.
fn test_a_source_name_cannot_collide_with_the_renamed_one() {
	v3_bin := local_shadow_build_v3()
	root := os.join_path(os.vtmp_dir(), 'v3_local_shadow_collision_${os.getpid()}')
	defer {
		os.rmdir_all(root) or {}
	}
	res := local_shadow_build_and_run(v3_bin, root, 'struct Arg {
	values []int
}

fn both_params(array Arg, array__local int) int {
	return array.values.len + array__local
}

fn both_locals() int {
	array__local := 10
	array := 1
	return array + array__local
}

fn main() {
	println(both_params(Arg{ values: [1, 2, 3] }, 7))
	println(both_locals())
}
')
	assert res.exit_code == 0, res.output
	assert res.output.trim_space().split('\n').map(it.trim_space()) == ['10', '11'], res.output
}

// A mutable parameter already holds an address, so taking one of it is the parameter
// itself, and passing it on is written as the parameter's own name. That name is the
// renamed one, the same as every other read of it.
fn test_a_mutable_parameter_is_passed_on_under_its_declared_name() {
	v3_bin := local_shadow_build_v3()
	root := os.join_path(os.vtmp_dir(), 'v3_local_shadow_mut_param_${os.getpid()}')
	defer {
		os.rmdir_all(root) or {}
	}
	res := local_shadow_build_and_run(v3_bin, root, 'struct Box {
mut:
	n int
}

fn (mut b Box) bump() {
	b.n++
}

type Shape = Box | int

fn sort_them(mut array []int) {
	array.sort()
}

fn fill(mut array map[string]int) {
	array["a"] = 1
}

fn take_sum(mut s Shape) {
	if mut s is Box {
		s.bump()
	}
}

fn pass_on(mut array Shape) {
	take_sum(mut array)
}

fn bump_it(mut array Box) {
	array.bump()
}

fn main() {
	mut a := [3, 1, 2]
	sort_them(mut a)
	println(a)
	mut m := map[string]int{}
	fill(mut m)
	println(m["a"])
	mut sh := Shape(Box{ n: 1 })
	pass_on(mut sh)
	if sh is Box {
		println(sh.n)
	}
	mut b := Box{ n: 5 }
	bump_it(mut b)
	println(b.n)
}
')
	assert res.exit_code == 0, res.output
	assert res.output.trim_space().split('\n').map(it.trim_space()) == ['[1, 2, 3]', '1', '2', '6'], res.output
}

// A shared local is passed as its wrapper storage, including when it is forwarded
// as a shared argument or used as a shared receiver. That storage keeps the same
// renamed identifier as the declaration when the source name shadows a C type.
fn test_a_shared_local_is_forwarded_under_its_declared_name() {
	v3_bin := local_shadow_build_v3()
	root := os.join_path(os.vtmp_dir(), 'v3_local_shadow_shared_${os.getpid()}')
	defer {
		os.rmdir_all(root) or {}
	}
	res := local_shadow_build_and_run(v3_bin, root, 'struct State {
mut:
	n int
}

fn read(shared state State) int {
	rlock state {
		return state.n
	}
}

fn (shared state State) read_method() int {
	rlock state {
		return state.n
	}
}

fn forward_local_arg() int {
	shared array := State{ n: 9 }
	return read(shared array)
}

fn forward_local_receiver() int {
	shared array := State{ n: 11 }
	return array.read_method()
}

fn main() {
	println(forward_local_arg())
	println(forward_local_receiver())
}
')
	assert res.exit_code == 0, res.output
	assert res.output.trim_space().split('\n').map(it.trim_space()) == ['9', '11'], res.output
}
