struct Wrapper<S> {
mut:
	calc Calc<S>
}

fn (mut w Wrapper<S>) next<T>(input T) f64 {
	$if S is TypeA || S is TypeB {
		$if T is f64 {
			return w.calc.next(input)
		} $else {
			panic('"${T.name}" is not supported')
			return -1
		}
		return -1
	} $else {
		panic('"${S.name}" is not supported')
		return -1
	}
}

struct Calc<S> {
mut:
	typ S
}

struct TypeA {}

struct TypeB {}

fn (mut c Calc<S>) next<T>(input T) f64 {
	$if S is TypeA || S is TypeB {
		$if T is f64 {
			return c.typ.next(input)
		} $else {
			panic('Unsupported type ${T.name}')
		}
	} $else {
		panic('Unsupported type ${S.name}')
	}
}

fn (mut t TypeA) next<T>(input T) f64 {
	return 10
}

fn (mut t TypeB) next<T>(input T) f64 {
	return 11
}

fn test_generics_with_multi_nested_generic_method_call() {
	{
		mut c := Wrapper<TypeA>{
			calc: Calc<TypeA>{
				typ: TypeA{}
			}
		}
		assert c.next(100.0) == 10.0
	}
	{
		mut c := Wrapper<TypeB>{
			calc: Calc<TypeB>{
				typ: TypeB{}
			}
		}
		assert c.next(100.0) == 11.0
	}
	println('OK!!')
}
