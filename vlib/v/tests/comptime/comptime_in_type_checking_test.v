type Abc = f64 | int

struct Test {
	a int
	b []string
	c f64
	d Abc
	e map[string]string
	f struct {}

}

fn check[T](val T) string {
	$if T in [$array, $struct] {
		return 'array or struct'
	} $else $if T in [u8, u16, u32] {
		return 'unsigned int'
	} $else $if T in [int, $int] {
		return 'int'
	} $else $if T in [$float, f64, f32] {
		return 'f64'
	} $else $if T in [$map, map] {
		return 'map'
	} $else $if T in [Abc, Abc] {
		return 'Abc'
	} $else $if T in [$sumtype, Abc] {
		return 'sumtype'
	}

	return ''
}

fn check_not[T](val T) string {
	mut str := string{}
	$if T !in [$array, $array] {
		str += '1'
	}
	$if T !in [u8, u16, u32] {
		str += '2'
	}
	$if T !in [int, $int] {
		str += '3'
	}
	$if T !in [$float, f64, f32] {
		str += '4'
	} $else $if T !in [$map, map] {
		str += '5'
	} $else $if T !in [Abc, Abc] {
		str += '6'
	} $else $if T !in [$sumtype, Abc] {
		str += '7'
	}

	return str
}

fn test_in() {
	var := Test{
		a: 1
		b: ['foo']
		c: 1.2
		d: 1
		e: {
			'a': 'b'
		}
	}
	assert check(var) == 'array or struct'
	assert check(var.a) == 'int'
	assert check(var.b) == 'array or struct'
	assert check(var.c) == 'f64'
	assert check(var.d) == 'Abc'
	assert check(var.e) == 'map'
	assert check(var.f) == 'array or struct'
	assert check(Test{}) == 'array or struct'
}

fn test_not_in() {
	var := Test{
		a: 1
		b: ['foo']
		c: 1.2
		d: 1
		e: {
			'a': 'b'
		}
	}
	assert check_not(var) == '1234'
	assert check_not(var.a) == '124'
	assert check_not(var.b) == '234'
	assert check_not(var.c) == '1235'
	assert check_not(var.d) == '1234'
	assert check_not(var.e) == '1234'
	assert check_not(var.f) == '1234'
	assert check_not(Test{}) == '1234'
}
