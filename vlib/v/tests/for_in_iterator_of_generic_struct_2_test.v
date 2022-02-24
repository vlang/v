pub struct Item<V> {
pub:
	value V
}

pub struct Iter<V> {
	arr []V
mut:
	ix int
}

pub fn (mut iter Iter<V>) next() ?Item<V> {
	if iter.ix >= iter.arr.len {
		return none
	}

	val := iter.arr[iter.ix]
	iter.ix += 1
	return Item<V>{val}
}

fn iterator<V>(arr []V) Iter<V> {
	return Iter<V>{arr, 0}
}

fn test_for_in_iterator_of_generic_struct() {
	mut ret := []int{}
	mut x := iterator<int>([1, 2, 3, 4, 5])
	for item in x {
		println(item.value)
		ret << item.value
	}
	println(ret)
	assert ret == [1, 2, 3, 4, 5]
}
