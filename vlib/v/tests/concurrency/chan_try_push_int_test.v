struct Foo {
mut:
	value int = 123
}

fn test_main() {
	shared m := Foo{}
	ch := chan int{cap: 1}
	ch.try_push(rlock m {
		m.value
	})
	mut tmp := int(0)
	ch.try_pop(mut tmp)
	assert tmp == 123
}
