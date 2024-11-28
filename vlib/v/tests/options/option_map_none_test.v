fn test_main() {
	mut a := {
		'foo': ?int(1)
		'bar': none
	}
	a['foo'] = 1
	assert dump(a) == a
}

fn test_none() {
	mut a := {
		'foo': ?int(1)
		'bar': none
	}
	a['foo'] = none
	assert dump(a) == a
}
