module datatypes

fn test_exists() {
	mut set := Set<string>{}
	set.add('foo')
	assert set.exists('foo')
	assert set.exists('bar') == false
}

fn test_remove() {
	mut set := Set<string>{}
	set.remove('foo')
	set.add('foo')
	assert set.exists('foo')
	set.remove('foo')
	assert set.exists('foo') == false
}

fn test_size() {
	mut set := Set<string>{}
	set.add('foo')
	set.add('foo')
	assert set.size() == 1
}

fn test_pop() {
	mut set := Set<string>{}
	set.add('foo')
	set.pop() or { return }
	assert set.exists('foo') == false
}

fn test_clear() {
	mut set := Set<string>{}
	set.add('foo')
	set.clear()
	assert set.size() == 0
}

fn test_rest() {
	mut set := Set<string>{}
	set.add('foo')
	set.add('bar')
	array := set.rest() or { return }
	assert array.len == 1
}

fn test_equal() {
	mut first_set := Set<string>{}
	mut second_set := Set<string>{}
	first_set.add('foo')
	assert second_set.equal(first_set) == false
	second_set.add('foo')
	assert second_set.equal(first_set)
}

fn test_is_empty() {
	mut set := Set<string>{}
	assert set.is_empty()
	set.add('foo')
	assert set.is_empty() == false
}

fn test_union() {
	mut first_set := Set<string>{}
	mut second_set := Set<string>{}
	first_set.update(['b', 'c', 'd'])
	second_set.update(['a', 'e'])
	mut third_set := first_set.union_(second_set)
	assert third_set.exists('a')
	assert third_set.exists('b')
	assert third_set.exists('c')
	assert third_set.exists('d')
	assert third_set.exists('e')
}

fn test_intersection() {
	mut first_set := Set<string>{}
	first_set.add('foo')
	first_set.add('bar')
	first_set.add('baz')
	mut second_set := Set<string>{}
	second_set.add('bar')
	second_set.add('baz')
	second_set.add('boo')
	mut third_set := first_set.intersection(second_set)
	assert third_set.exists('foo') == false
	assert third_set.exists('bar')
	assert third_set.exists('baz')
	assert third_set.exists('boo') == false
}

fn test_difference() {
	mut first_set := Set<string>{}
	mut second_set := Set<string>{}
	first_set.add('foo')
	first_set.add('bar')
	first_set.add('baz')
	second_set.add('bar')
	second_set.add('baz')
	second_set.add('boo')
	mut third_set := first_set.difference(second_set)
	assert third_set.exists('foo')
	assert third_set.exists('bar') == false
	assert third_set.exists('baz') == false
	assert third_set.exists('boo') == false
	first_set.clear()
	second_set.clear()
	third_set.clear()
	first_set.add('bar')
	first_set.add('baz')
	first_set.add('boo')
	second_set.add('foo')
	second_set.add('bar')
	second_set.add('baz')
	third_set = first_set.difference(second_set)
	assert third_set.exists('foo') == false
	assert third_set.exists('bar') == false
	assert third_set.exists('baz') == false
	assert third_set.exists('boo')
}

fn test_subset() {
	mut set := Set<string>{}
	set.update(['a', 'b', 'c'])
	mut subset := Set<string>{}
	subset.update(['b', 'c'])
	assert set.subset(subset)
}
