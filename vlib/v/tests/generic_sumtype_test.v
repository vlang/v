struct None {}

// not named `Option` to avoid conflicts with the built-in type:
type MyOption<T> = Error | None | T

fn unwrap_if<T>(o MyOption<T>) T {
	if o is T {
		return o
	}
	panic('no value')
}

fn unwrap_match<T>(o MyOption<T>) string {
	match o {
		None {
			return 'none'
		}
		Error {
			return 'none'
		}
		T {
			return 'value'
		}
	}
}

fn test_generic_sumtype_unwrapping() {
	y := MyOption<bool>(false)

	assert unwrap_if(y) == false
	assert unwrap_match(y) == 'value'
}

fn test_generic_sumtype_auto_str() {
	x := MyOption<string>('hi')
	y := MyOption<bool>(None{})
	assert '${x}, ${y}' == "MyOption<string>('hi'), MyOption<bool>(None{})"
}

struct Foo<T> {
	x T
}

struct Bar<T> {
	x T
}

type MyType<T> = Bar<T> | Foo<T>

fn test_generic_struct_members() {
	// TODO: this is currently needed to properly resolve that variant's type:
	_ = Bar<string>{''}

	f := Foo<string>{'hi'}
	t := MyType<string>(f)
	assert t.type_name() == 'Foo<string>'
	// accessing a field common to all variants, just like with a normal sumtype:
	assert t.x == 'hi'
}

type MultiGeneric<X, Y, Z> = X | Y | Z

fn test_multi_generic_type() {
	mut m := MultiGeneric<bool, int, string>(1234)
	m = 'hi'
	match m {
		bool {
			assert false
		}
		int {
			assert false
		}
		string {
			return
		}
	}
	assert false
}
