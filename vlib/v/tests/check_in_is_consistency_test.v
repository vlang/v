type Abc = f64 | int

struct Test {
	a int
	b ?int
}

fn check[T](val T) string {
	$if T in [?int, ?int] {
		return 'option int'
	}
	$if T in [int, int] {
		return 'int'
	}
	return ''
}

fn check2[T](val T) string {
	mut str := string{}
	$for field in T.fields {
		$if field.typ in [?int, ?int] {
			str += 'option int'
		}
		$if field.typ in [int, int] {
			str += 'int'
		}
	}
	return str
}

fn check_is[T](val T) string {
	$if T is ?int {
		return 'option int'
	}
	$if T is int {
		return 'int'
	}
	return ''
}

fn check_is2[T](val T) string {
	mut str := string{}
	$for field in T.fields {
		$if field.typ is ?int {
			str += 'option int'
		}
		$if field.typ is int {
			str += 'int'
		}
	}
	return str
}

fn test_in() {
	var := Test{
		a: 1
		b: 2
	}
	assert check(var.a) == 'int'
	assert check(var.b?) == 'int'
}

fn test_in_2() {
	var := Test{
		a: 1
		b: 2
	}
	assert check2(var) == 'intoption int'
}

fn test_is() {
	var := Test{
		a: 1
		b: 2
	}
	assert check_is(var.a) == 'int'
	assert check_is(var.b?) == 'int'
}

fn test_is_2() {
	var := Test{
		a: 1
		b: 2
	}
	assert check_is2(var) == 'intoption int'
}
