fn test_nested_generic_struct_init() {
	mut list := list_new<int>()
	println(list)
	assert '$list'.contains('head: &nil')
}

struct List<T> {
pub mut:
	head &ListNode<T> = 0
}

struct ListNode<T> {
pub mut:
	value T
	next  &ListNode<T> = 0
}

pub fn list_new<T>() &List<T> {
	return &List<T>{}
}
