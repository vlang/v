fn test_nested_generic_struct_init() {
	mut list1 := &List<int>{}
	println(list1)
	assert '${list1}'.contains('head: &nil')

	mut list2 := list_new<int>()
	println(list2)
	assert '${list2}'.contains('head: &nil')
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
