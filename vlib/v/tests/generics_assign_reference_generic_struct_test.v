pub struct List<T> {
pub mut:
	head &ListNode<T> = unsafe { nil }
}

pub struct ListNode<T> {
pub mut:
	value T
	next  &ListNode<T> = unsafe { nil }
}

pub fn list_new<T>() List<T> {
	return List<T>{}
}

pub fn (mut l List<T>) add(value T) {
	mut node := &ListNode<T>{value, 0}
	if unsafe { l.head == 0 } {
		l.head = node
	} else {
		node.next = l.head
		l.head = node
	}
}

fn test_generic_assign_reference_generic_struct() {
	mut list1 := list_new<string>()
	list1.add('hello')
	println(list1.head.value)
	assert list1.head.value == 'hello'

	mut list2 := list_new<int>()
	list2.add(100)
	println(list2.head.value)
	assert list2.head.value == 100

	mut list3 := list_new<f64>()
	list3.add(22.2)
	println(list3.head.value)
	assert list3.head.value == 22.2

	mut list4 := list_new<bool>()
	list4.add(true)
	println(list4.head.value)
	assert list4.head.value == true
}
