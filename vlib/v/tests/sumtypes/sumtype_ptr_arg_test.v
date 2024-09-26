struct Foo {
	foo i32
}

struct Bar {
	bar f32
}

type Foobar = Bar | Foo

fn match_sum(sum Foobar) {
	match sum {
		Foo {
			sum_foo(sum)
		}
		Bar {
			sum_bar(sum)
		}
	}
}

fn sum_foo(i &Foo) {
	assert true
}

fn sum_bar(i Bar) {
	assert true
}

fn test_main() {
	match_sum(Foo{ foo: 5 })
	match_sum(Bar{ bar: 5 })
}
