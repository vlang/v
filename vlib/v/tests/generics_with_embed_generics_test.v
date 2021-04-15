struct Group<T> {
	len int
	val []T
mut:
	index int
}

fn group_new<T>(val ...T) Group<T> {
	mut arr := []T{cap: val.len}
	for i in val {
		arr << i
	}
	mut g := Group{
		len: val.len
		val: arr
	}

	return g
}

fn (mut it Group<T>) next<T>() ?T {
	if it.index >= it.len {
		return none
	}
	v := it.val[it.index]
	it.index++
	return v
}

fn test_generics_with_embed_generics() {
	gx := group_new<int>(1, 2, 3)
	for x in gx.val {
		println(x)
	}
	assert gx.val == [1, 2, 3]
}
