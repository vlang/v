struct Struct {
	a int
	b string
}

struct Abc {}

fn (a Abc) generic[T](b T) {
	$if T is $struct {
		$for f in T.fields {
			a.generic(b.$(f.name))
		}
	}
}

fn test_main() {
	Abc{}.generic(Struct{})
	assert true
}
