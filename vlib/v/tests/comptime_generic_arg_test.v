struct StructType[T] {
mut:
	val T
}

type SumTypes = StructType[string] | []SumTypes | []string | bool | int | string

pub struct Count {
mut:
	total int
}

// count_chars count json sizen without new encode
pub fn (mut count Count) count_chars[T](val T) {
	$if T is $sumtype {
		$for v in val.variants {
			if val is v {
				// dump(typeof(val).name)
				count.count_chars(val)
			}
		}
	} $else $if T is $struct {
		count.chars_in_struct(val)
	} $else {
	}
}

// chars_in_struct
fn (mut count Count) chars_in_struct[T](val T) {
	$for field in T.fields {
		va := val.$(field.name)
		count.count_chars(va)
		count.count_chars(val.$(field.name))
		assert true
	}
	assert true
}

fn test_main() {
	mut count := Count{}
	count.count_chars(StructType[SumTypes]{ val: '' })
	assert true
}
