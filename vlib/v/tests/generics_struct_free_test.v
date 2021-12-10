struct List<T> {
pub mut:
	head &ListNode<T> = 0
}

struct ListNode<T> {
pub mut:
	value T
	next  &ListNode<T> = 0
}

fn list_new<T>() List<T> {
	return List<T>{}
}

fn listnode_new<T>() &ListNode<T> {
	return &ListNode<T>{0, 0}
}

fn (mut l List<T>) free() {
	//
}

fn test_generic_struct_free() {
	mut list := list_new<string>()
	println(list)
	list.free()
	assert true
}
