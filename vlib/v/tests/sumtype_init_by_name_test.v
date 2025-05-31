type Sum = int | string

fn get[T](val T, type_name string) T {
	$if T is $sumtype {
		$for v in val.variants {
			if type_name == typeof(v.typ).name {
				return T(v)
			}
		}
	}
	return T{}
}

fn get2[T](val T, type_name string) T {
	$if T is $sumtype {
		$for v in val.variants {
			if type_name == typeof(v.typ).name {
				return Sum(v)
			}
		}
	}
	return T{}
}

fn test_main() {
	assert dump(get(Sum{}, 'int')) == Sum(0)
	assert dump(get(Sum{}, 'string')) == Sum('')

	assert dump(get2(Sum{}, 'int')) == Sum(0)
	assert dump(get2(Sum{}, 'string')) == Sum('')
}
