struct Test<T> {
	v T
}

fn get_test<T>(v T) Test<T> {
	return Test<T>{
		v: v
	}
}

fn test_generics_assign_generics_struct() {
	x1 := get_test(1)
	println('${x1.v}')
	assert x1.v == 1

	x2 := get_test(2.2)
	println('${x2.v}')
	assert x2.v == 2.2

	x3 := get_test('aaa')
	println('${x3.v}')
	assert x3.v == 'aaa'

	x4 := get_test(true)
	println('${x4.v}')
	assert x4.v == true
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
	mut list1 := new<int>()
	list1.add(100)
	println(list1.next)
	assert list1.next.val == 100

	mut list2 := new<f64>()
	list2.add(2.22)
	println(list2.next)
	assert list2.next.val == 2.22

	mut list3 := new<bool>()
	list3.add(false)
	println(list3.next)
	assert list3.next.val == false

	mut list4 := new<string>()
	list4.add('aaa')
	println(list4.next)
	assert list4.next.val == 'aaa'
}
