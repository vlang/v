struct Foo[T] {
	bar ?&Bar[T]
}

struct Bar[T] {
	foo ?&Foo[T]
}

fn foo_new[T]() &Foo[T] {
	return &Foo[T]{}
}

fn test_mutual_recursive_generic_struct_reference() {
	foo := foo_new[int]()
	assert foo != unsafe { nil }
}

pub struct Issue26675List[T] {
	front ?&Issue26675ListNode[T]
	back  ?&Issue26675ListNode[T]
}

pub struct Issue26675ListNode[T] {
	value T
	list  ?&Issue26675List[T]
	prev  ?&Issue26675ListNode[T]
	next  ?&Issue26675ListNode[T]
}

pub fn Issue26675ListNode.new[T]() &Issue26675ListNode[T] {
	return &Issue26675ListNode[T]{}
}

struct Issue26675Foo {
	node &Issue26675ListNode[Issue26675Foo]
}

fn test_recursive_generic_struct_instantiation_does_not_crash() {
	node := Issue26675ListNode.new[Issue26675Foo]()
	assert node != unsafe { nil }
}
