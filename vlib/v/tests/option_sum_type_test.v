fn is_opt(f Foo) bool {
	return match f {
		Baz {
			false
		}
		// vfmt off
		?Bar {
			true
		}
		// vfmt on
	}
}

fn test_main() {
	dump(is_opt(Foo(?Bar(none))))
	dump(is_opt(Foo(Baz{})))
}

struct Baz {}

struct Bar {}

type Foo = ?Bar | Baz
