type MyAlias = string
type MyAlias2 = rune

struct Foo {
	a string
	b ?string
	c MyAlias
	d MyAlias2
	e []int
}

fn is_t[T](val T) bool {
	$if val is T {
		return true
	}
	return false
}

fn test_main() {
	a := Foo{}
	$for field in Foo.fields {
		$if field.unaliased_typ is ?string {
			assert field.name == 'b'
			println('is option string')
		} $else $if field.unaliased_typ is string {
			assert field.name in ['a', 'c']
			println('is string')
		} $else $if field.unaliased_typ is MyAlias {
			assert false
			println('is string')
		} $else $if field.typ is MyAlias2 {
			assert field.name == 'd'
			println('is MyAlias')
		} $else $if field.typ is []int {
			assert field.name == 'e'
			println('is int')
		} $else {
			assert false
		}
	}
}

fn test_generic() {
	a := Foo{}
	$for field in Foo.fields {
		if is_t(field) {
			println('T')
			assert true
		} else {
			assert false
		}
		val := a.$(field.name)
		if is_t(val) {
			println('T')
			assert true
		} else {
			assert false
		}
	}
}
