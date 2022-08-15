struct Calc<S> {
mut:
	typ S
}

struct TypeA {}

struct TypeB {}

fn (mut c Calc<S>) next<T>(input T) f64 {
	$if S is TypeA || S is TypeB {
		return c.typ.next(input)
	} $else {
		return 99.0
	}
}

fn (mut t TypeA) next<T>(input T) f64 {
	return 10.0
}

fn (mut t TypeB) next<T>(input T) f64 {
	return 11.0
}

fn test_generics_with_nested_generic_method_call() {
	{
		mut c := Calc<TypeA>{
			typ: TypeA{}
		}
		assert c.next(100) == 10.0
	}
	{
		mut c := Calc<TypeB>{
			typ: TypeB{}
		}
		assert c.next(100) == 11.0
	}
	println('OK!!')
}
