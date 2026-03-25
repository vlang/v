struct Foo[U] {
	f fn (string) U @[required]
}

fn map_foo[T, U](f Foo[T], m fn (T) U) Foo[U] {
	return Foo[U]{
		f: fn [f, m] [U](s string) U {
			v := f.f(s)
			return m(v)
		}
	}
}

fn test_generic_struct_fn_field_closure() {
	f := Foo[string]{
		f: fn (s string) string {
			return s
		}
	}
	v := map_foo(f, fn (_ string) int {
		return -1
	})
	assert v.f('foo') == -1
}
