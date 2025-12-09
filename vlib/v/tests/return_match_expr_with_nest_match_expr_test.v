struct Foo {
}

struct Bar {
	b bool
}

type FooBar = Foo | Bar

fn get_foobar(f FooBar) FooBar {
	return match f {
		Foo {
			Foo{}
		}
		Bar {
			b := match f.b {
				true { false }
				false { true }
			}
			Bar{
				b: b
			}
		}
	}
}

fn test_return_match_expr_with_nest_match_expr() {
	assert get_foobar(Bar{}) == FooBar(Bar{
		b: true
	})
}
