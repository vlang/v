type Sum = int | string
type SumAlias = Sum

fn get_alias[T](val T, type_name string) T {
	$if T is $alias && T.unaliased_typ is $sumtype {
		$for v in val.variants {
			if type_name == typeof(v.typ).name {
				return T(v)
			}
		}
	}
	return T{}
}

fn test_sumtype_alias_init_by_variant_name() {
	a_int := get_alias(SumAlias(Sum(10)), 'int')
	if a_int is int {
		assert a_int == 0
	} else {
		assert false
	}

	a_str := get_alias(SumAlias('foo'), 'string')
	if a_str is string {
		assert a_str == ''
	} else {
		assert false
	}
}
