module datatypes

fn test_add() {
	mut set := Set<string>{}
	set.add('foo')
	set.add('bar')
}

fn test_exists() {
	mut set := Set<string>{}
	set.add('foo')
	assert set.exists('foo')
	assert set.exists('bar') == false
}

fn test_remove() {
	mut set := Set<string>{}
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
	set.pop() or { println('Pop failed') }
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
	array := set.rest() or { panic("Don't panic, it's organic!") }
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
