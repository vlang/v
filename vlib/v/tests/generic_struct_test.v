struct Test {}

fn (t Test) test[T](val T) {
	$for f in T.fields {
		value := val.$(f.name)
		$if f.is_option {
			$if f.typ is $struct {
				_ := 1
				t.test(value)
			} $else $if f.typ is string {
				_ := 2
			} $else $if f.typ is ?string {
				_ := 3
			} $else {
				_ := 4
			}
		} $else {
			$if f.typ is $struct {
				t.test(value)
			} $else $if f.typ is string {
				_ := 5
			}
		}
	}
}

struct Bb {
	a ?string
}

struct Cc {
	b Bb
}

struct Aa[T] {
	a T
}

fn test_main() {
	a := Aa[Cc]{}
	t := Test{}
	t.test(a)
	assert true
}
