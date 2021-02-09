struct Foo {
}

struct Bar {
mut:
	text string
}

struct Baz {
mut:
	text string
}

type FBB = Bar | Baz | Foo

fn test_sumtype_assign() {
	mut arr := []FBB{}
	arr << Foo{}
	arr << Bar{
		text: 'bar'
	}
	arr << Baz{
		text: 'baz'
	}
	mut results := []string{}
	for a in arr {
		match mut a {
			Bar, Baz {
				a.text = 'I am ' + a.text
				println(a.text)
				results << a.text
			}
			else {}
		}
	}
	assert results[0] == 'I am bar'
	assert results[1] == 'I am baz'
}
