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
