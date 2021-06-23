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
	mut n := &ListNode{
		val: arr[0]
		next: 0
	}
	mut l := &List{
		first: n
		last: n
		count: 1
	}
	return l
}

fn test_generics_with_generic_structs_init() {
	n := create([1, 2, 3])
	println(n)
	assert n.count == 1
}
