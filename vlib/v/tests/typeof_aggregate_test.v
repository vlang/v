struct Foo {
}

struct Bar {
	text string
}

struct Baz {
	text string
}

struct Bazaar {
	text string
}

type FBB = Bar | Baz | Bazaar | Foo

fn test_typeof_aggregate() {
	mut arr := []FBB{}
	arr << Foo{}
	arr << Bar{
		text: 'bar'
	}
	arr << Baz{
		text: 'baz'
	}

	mut rets := []string{}
	for a in arr {
		match a {
			Foo {
				println('The type of `a` is `${typeof(a).name}`')
				rets << 'The type of `a` is `${typeof(a).name}`'
			}
			Bar, Baz, Bazaar {
				println('The type of `a` is `${typeof(a).name}` and its text says ${a.text}')
				rets << 'The type of `a` is `${typeof(a).name}` and its text says ${a.text}'
			}
		}
	}

	assert rets.len == 3
	assert rets[0] == 'The type of `a` is `Foo`'
	assert rets[1] == 'The type of `a` is `(Bar | Baz | Bazaar)` and its text says bar'
	assert rets[2] == 'The type of `a` is `(Bar | Baz | Bazaar)` and its text says baz'
}
