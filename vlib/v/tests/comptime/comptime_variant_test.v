type TestSum = int | string

fn gen[T](val T) {
	$if val is $sumtype {
		$for f in T.variants {
			dump(f)
			dump(f.typ)
			$if f.typ is $int {
				dump('is int')
				assert f.typ == typeof[int]().idx
			} $else $if f.typ is string {
				dump('is string')
				assert f.typ == typeof[string]().idx
			}
		}
	}
}

fn test_main() {
	a := TestSum(123)
	gen(a)
}

type FieldVariantsMixed = []bool | []int | []string | string

struct FieldVariantsHolder {
mut:
	t FieldVariantsMixed
}

fn enumerate_sumtype_field_variants[T]() int {
	mut count := 0
	$for field in T.fields {
		$if field.typ is $sumtype {
			$for variant in field.typ.variants {
				if typeof(variant.typ).name != '' {
					count++
				}
			}
		}
	}
	return count
}

fn test_comptime_for_field_typ_variants() {
	assert enumerate_sumtype_field_variants[FieldVariantsHolder]() == 4
}
