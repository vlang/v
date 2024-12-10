struct Encoder {}

struct Writer {}

struct StructTypePointer[T] {
mut:
	val &T
}

pub fn (e &Encoder) encode_value[T](val T, mut wr Writer) ! {
	assert e.encode_struct[T](val, 1, mut wr)! == 'a'
}

fn (e &Encoder) encode_struct[U](val U, level int, mut wr Writer) !string {
	$for field in U.fields {
		if val.$(field.name) != unsafe { nil } {
			$if field.indirections > 0 {
				assert field.indirections == 1
				return 'a'
			} $else {
				return 'b'
			}
		}
	}
	return 'z'
}

fn test_check() {
	e := Encoder{}
	mut sb := Writer{}

	mut string_initialized_with_reference := 'ads'
	e.encode_value(StructTypePointer[string]{ val: &string_initialized_with_reference }, mut
		sb) or {}
}
