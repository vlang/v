struct Foo {
	a ?int
	b ?string
}

fn receives_int(a int) {}

fn receives_string(a string) {}

fn test_main() {
	t := Foo{
		a: 1
		b: 'foo'
	}
	mut c := 0
	$for f in t.fields {
		$if f.typ is ?int {
			assert t.$(f.name) ?.str() == '1'
			w := t.$(f.name) ?
			assert w == 1
			receives_int(w)
			receives_int(t.$(f.name) ?)
			c++
		}
		$if f.typ is ?string {
			assert t.$(f.name) ?.str() == 'foo'
			a := t.$(f.name) ?
			assert a == 'foo'
			receives_string(a)
			receives_string(t.$(f.name) ?)
			c++
		}
	}
	assert c == 2
}
