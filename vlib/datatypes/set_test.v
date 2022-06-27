module datatypes

fn test_new_set_with_capacity() {
	mut set := new_set_with_capacity<string>(1)
	set.add('foo')
	set.add('bar')
	assert set.size() == 1
	assert set.exists('foo') == false
	assert set.exists('bar')
}

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
