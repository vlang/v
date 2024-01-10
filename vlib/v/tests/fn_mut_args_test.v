fn func(mut a []int) {
	a = [1, 2, 3, 4]
	println('inside fn: ${a}')
	assert '${a}' == '[1, 2, 3, 4]'
}

fn test_fn_mut_args_of_array() {
	mut a := [1, 2, 3]
	func(mut a)
	println('inside main: ${a}')
	assert '${a}' == '[1, 2, 3, 4]'
}

fn init_map(mut n map[string]int) {
	n = {
		'one': 1
	}
}

fn test_fn_mut_args_of_map() {
	mut m := map[string]int{}
	init_map(mut m)
	println(m)
	assert m == {
		'one': 1
	}
}

struct MyData {
pub mut:
	ar []int
}

fn pass_array_mut(mut ar []int) int {
	if ar.len > 0 && ar.last() == 99 {
		return 99
	}
	return 0
}

fn test_fn_mut_args_of_array_last() {
	mut m := MyData{}
	m.ar << 99
	assert pass_array_mut(mut m.ar) == 99
}

interface ChildInterface {
	data int
}

struct Child {
	data int
}

struct Parent {
mut:
	children []ChildInterface
}

fn (mut p Parent) add(mut x ChildInterface) {
	p.children << x
}

fn test_fn_mut_args_of_interface() {
	mut x := Parent{}
	x.add(mut Child{ data: 123 })
	println(x.children[0].data)
	assert x.children[0].data == 123
}

struct LinuxFile {
}

interface File {
}

fn b(parent File) {
	println(parent)
	assert '${parent}' == 'File(LinuxFile{})'
}

fn a(mut parent File) {
	b(parent)
}

fn test_fn_mut_args_of_interface2() {
	mut file := LinuxFile{}
	a(mut file)
}
