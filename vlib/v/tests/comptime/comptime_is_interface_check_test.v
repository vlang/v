module main

interface TestInterface {
	test_func() f64
}

struct Struct1 {
	num f64
}

fn (o &Struct1) test_func() f64 {
	return o.num
}

struct Struct2 {
mut:
	s1 ?&Struct1
}

fn do_thing[T](s1 Struct1) T {
	mut t := T{}
	t.s1 = &s1
	$for field in T.fields {
		$if field.typ is TestInterface {
			i := TestInterface(t.$(field.name))
			assert false
		} $else $if field.typ is ?TestInterface {
			i := TestInterface(t.$(field.name) ?)
			assert true
		}
	}
	return t
}

fn test_main() {
	assert do_thing[Struct2](Struct1{1.23}).s1?.num == 1.23
}
