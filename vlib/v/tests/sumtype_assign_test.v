struct Foo {
mut:
	num int
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
	arr << Foo{
		num: 22
	}
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
			Foo {
				a.num = 11
				results << 'Num is $a.num'
			}
		}
	}
	assert results[0] == 'Num is 11'
	assert results[1] == 'I am bar'
	assert results[2] == 'I am baz'
}
