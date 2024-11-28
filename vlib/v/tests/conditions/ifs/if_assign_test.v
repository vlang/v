struct Foo {
mut:
	option ?string
}

fn test_main() {
	mut foo := Foo{}
	some_val := 1
	foo.option = if some_val > 0 {
		'awesome'
	} else {
		none
	}
	assert foo.option? == 'awesome'
}
