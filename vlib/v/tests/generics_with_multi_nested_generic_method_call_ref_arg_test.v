struct Wrapper<S> {
mut:
	calc Calc<S>
}

fn (mut w Wrapper<S>) next<T>(input T) f64 {
	$if S is TypeA || S is TypeB {
		$if T is f64 {
			return w.calc.next(input)
		} $else $if T is SecretCase {
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
	typ   S
	inner InnerCalc
}

struct InnerCalc {}

fn (mut c InnerCalc) next<T>(input T) f64 {
	$if T is f64 {
		return 64.2
	} $else {
		return 100.0
	}
}

struct TypeA {}

struct TypeB {}

interface SecretCase {
	secret() f64
}

interface SpecialCase {
	SecretCase
}

struct SecretSkill {}

fn (s &SecretSkill) secret() f64 {
	return 101.0
}

fn (mut c Calc<S>) next<T>(input T) f64 {
	$if S is TypeA || S is TypeB {
		$if T is f64 {
			return c.typ.next(input)
		} $else $if T is SpecialCase {
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

fn new<S>() Wrapper<S> {
	$if S is TypeA {
		return Wrapper<TypeA>{
			calc: Calc<TypeA>{
				typ: TypeA{}
			}
		}
	} $else $if S is TypeB {
		return Wrapper<TypeB>{
			calc: Calc<TypeB>{
				typ: TypeB{}
			}
		}
	} $else {
		panic('unknown type ${S.name}')
	}
}

fn test_generics_with_multi_nested_generic_method_call_ref_arg() {
	{
		mut c := new<TypeA>()
		s := SecretSkill{}
		assert c.next(&s) == 10.0
	}
	{
		mut c := new<TypeB>()
		s := SecretSkill{}
		assert c.next(&s) == 11.0
	}
	println('OK!!')
}
