struct Test<T> {
	v T
}

fn get_test<T>(v T) Test<T> {
	return Test{
		v: v
	}
}

fn test_generics_assign_generics_struct() {
	x := get_test(1)
	println('$x.v')
	assert x.v == 1
}

// test generics assign generics struct_init
struct Node<T> {
pub mut:
	val  T
	next &Node<T> = 0
}

fn new<T>() &Node<T> {
	return &Node<T>{}
}

fn (mut n Node<T>) add(val T) {
	node := &Node<T>{val, 0}
	n.next = node
}

fn test_generic_fn_assign_generic_struct_init() {
	mut list := new<int>()
	list.add(100)
	println(list.next)
	assert list.next.val == 100
}
