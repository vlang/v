struct Group[T] {
	len int
	val []T
mut:
	index int
}

fn group_new[T](val ...T) Group[T] {
	mut arr := []T{cap: val.len}
	for i in val {
		arr << i
	}
	mut g := Group[T]{
		len: val.len
		val: arr
	}

	return g
}

fn (mut it Group[T]) next() ?T {
	if it.index >= it.len {
		return none
	}
	v := it.val[it.index]
	it.index++
	return v
}

fn test_generics_with_embed_generics() {
	gx1 := group_new[int](1, 2, 3)
	for x in gx1.val {
		println(x)
	}
	assert gx1.val == [1, 2, 3]

	gx2 := group_new[f64](1.1, 2.2, 3.3)
	for x in gx2.val {
		println(x)
	}
	assert gx2.val == [1.1, 2.2, 3.3]

	gx3 := group_new[bool](true, true, false)
	for x in gx3.val {
		println(x)
	}
	assert gx3.val == [true, true, false]

	gx4 := group_new[string]('aa', 'bb', 'cc')
	for x in gx4.val {
		println(x)
	}
	assert gx4.val == ['aa', 'bb', 'cc']
}
