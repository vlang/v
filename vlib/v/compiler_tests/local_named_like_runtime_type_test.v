import os
import v.tempname

// The runtime's own `array` struct is typed into C under that name, and a local that
// takes the name for itself hides the type from the rest of the function it is in, and the generated code still spells the type: slicing a field of
// a parameter called `array` builds an `(array[]){...}` compound literal, and inside
// that function `array` no longer names a type.
//
//   error: expected expression
//     return array__clone(&((array[]){array_slice(array.values, 0, array.cols)})[0]);

fn local_shadow_fixture_dir(name string) string {
	root := os.join_path(os.vtmp_dir(), 'v3_local_shadow_${name}_${tempname.unique_token()}')
	os.mkdir(root) or { panic(err) }
	return root
}

struct LocalShadowCase {
	name     string
	source   string
	expected []string
}

fn local_shadow_check_batch(cases []LocalShadowCase) {
	root := local_shadow_fixture_dir('runtime')
	defer { os.rmdir_all(root) or {} }
	mut imports := []string{cap: cases.len}
	mut calls := []string{cap: cases.len}
	for case in cases {
		dir := os.join_path(root, case.name)
		os.mkdir(dir) or { panic(err) }
		imports << 'import ${case.name}'
		calls << "\t\t'${case.name}' { ${case.name}.exercise() }"
		source := 'module ${case.name}\n\n' + case.source
		os.write_file(os.join_path(dir, 'case.v'), source.replace_once('\nfn main()',
			'\npub fn exercise()')) or { panic(err) }
	}
	main_v := os.join_path(root, 'main.v')
	os.write_file(main_v, "module main\n\n${imports.join('\n')}\n\nfn main() {\n\tmatch arguments()[1] {\n${calls.join('\n')}\n\t\telse { panic('unknown fixture') }\n\t}\n}\n") or {
		panic(err)
	}
	exe := os.join_path(root, 'prog')
	compile := os.exec([@VEXE, '-nocache', main_v, '-b', 'c', '-o', exe])
	assert compile.exit_code == 0, compile.output
	for case in cases {
		run := os.exec([exe, case.name])
		assert run.exit_code == 0, '${case.name}: ${run.output}'
		lines := run.output.trim_space().split('\n').map(it.trim_space())
		assert lines == case.expected, '${case.name}: expected ${case.expected}\n${run.output}'
	}
}

fn test_local_runtime_names() {
	local_shadow_check_batch([
		LocalShadowCase{'slice_parameter', 'struct Arg {
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
', ['[1, 2]']},
		// Wherever such a local is introduced, the name it is declared under has to be the
		// name the body will go on using. An if guard, an if guard over several values, a
		// multi-return declaration and a smartcast each introduce one, and each of them once
		// wrote the declaration through the plain name while the body read the renamed one.
		LocalShadowCase{'bindings', 'struct Box {
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
', ['5', '7', '3', '9']},
		// A deferred closure reads the local through a slot declared at the top of the
		// function, and that slot is the local: it has to carry the same name every read of
		// it uses, or the reads inside the closure name something that was never declared.
		LocalShadowCase{'deferred_binding', 'fn with_defer() {
	array := 1
	defer(fn) {
		println(array)
	}
	println("body")
}

fn main() {
	with_defer()
}
', ['body', '1']},
		// The renamed name is taken out of the namespace reserved for generated symbols
		// rather than built by adding a suffix. A suffix would bring a function that has both
		// `array` and a name spelled like the renamed one under a single name: as parameters
		// that is two parameters called the same thing, and as locals the outer one quietly
		// becomes the inner one.
		LocalShadowCase{'name_collision', 'struct Arg {
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
', ['10', '11']},
		// A mutable parameter already holds an address, so taking one of it is the parameter
		// itself, and passing it on is written as the parameter's own name. That name is the
		// renamed one, the same as every other read of it.
		LocalShadowCase{'mut_parameter', 'struct Box {
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
', ['[1, 2, 3]', '1', '2', '6']},
		// A shared local is passed as its wrapper storage, including when it is forwarded
		// as a shared argument or used as a shared receiver. That storage keeps the same
		// renamed identifier as the declaration when the source name shadows a C type.
		LocalShadowCase{'shared_local', 'struct State {
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
', ['9', '11']},
	])
}

// Main-module object wrappers declare their parameters through the normal function
// parameter path. Their call into the object-local implementation must use the same
// renamed parameter spelling.
fn test_an_object_file_wrapper_forwards_a_renamed_parameter() {
	root := local_shadow_fixture_dir('object')
	defer { os.rmdir_all(root) or {} }
	main_v := os.join_path(root, 'main.v')
	os.write_file(main_v, 'module main

pub fn answer(array int) int {
	return array + 1
}

fn main() {
	println(answer(1))
}
') or {
		panic(err)
	}
	object_path := os.join_path(root, 'answer.o')
	compile := os.exec([@VEXE, '-nocache', '-cc', 'cc', '-o', object_path, '${main_v}'])
	assert compile.exit_code == 0, compile.output
	assert os.is_file(object_path)
}
