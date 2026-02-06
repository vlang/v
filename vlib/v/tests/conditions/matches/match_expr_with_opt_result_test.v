struct Foo {
pub:
	a int
}

fn foo() !Foo {
	return Foo{1}
}

fn bar() ?Foo {
	return Foo{2}
}

fn test_main() {
	match foo()!.a {
		1 {
			assert true
		}
		else {
			assert false
		}
	}

	match bar()?.a {
		2 {
			assert true
		}
		else {
			assert false
		}
	}
}
