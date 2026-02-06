type TestSum = int | string

struct Abc {
	s TestSum
}

fn gen[T, R](struc T) R {
	$if T is $struct {
		$for field in T.fields {
			field_value := struc.$(field.name)
			$if field_value is $sumtype {
				$for v in field_value.variants {
					if field_value is v { // smartcast
						$if field_value is R {
							dump(field_value)
							return field_value
						}
					}
				}
			}
		}
	}
	return R{}
}

fn test_int() {
	a := Abc{TestSum(123)}
	int_var := gen[Abc, int](a)
	assert dump(int_var) == 123
}

fn test_str() {
	b := Abc{TestSum('foo')}
	str_var := gen[Abc, string](b)
	assert dump(str_var) == 'foo'
}

fn test_both() {
	a := Abc{TestSum(123)}
	b := Abc{TestSum('foo')}
	int_var := gen[Abc, int](a)
	str_var := gen[Abc, string](b)
	assert dump(str_var) == 'foo'
	assert dump(int_var) == 123
}
