struct List<T> {
mut:
	count u32
	first &ListNode<T>
	last  &ListNode<T>
}

struct ListNode<T> {
mut:
	val  T
	next &ListNode<T> = 0
}

fn create<T>(arr []T) &List<T> {
	assert arr.len > 0
	mut n := &ListNode<T>{
		val: arr[0]
		next: 0
	}
	mut l := &List<T>{
		first: n
		last: n
		count: 1
	}
	return l
}

fn test_generics_with_generic_structs_init() {
	list1 := create([1, 2, 3])
	println(list1)
	assert list1.count == 1
	assert list1.first.val == 1

	list2 := create(['a', 'b', 'c'])
	println(list2)
	assert list2.count == 1
	assert list2.first.val == 'a'

	list3 := create([1.1, 2.2, 3.3])
	println(list3)
	assert list3.count == 1
	assert list3.first.val == 1.1

	list4 := create([true, false, true])
	println(list4)
	assert list4.count == 1
	assert list4.first.val == true
}
