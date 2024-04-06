struct Encoder {}

struct Writer {}

struct StructType[T] {
mut:
	val  &T
	val2 T
}

fn (e &Encoder) encode_struct[U](val U, mut wr Writer) ! {
	$for field in U.fields {
		if field.indirections == 1 {
			assert field.indirections == 1
			value := val.$(field.name)
			$if field.indirections == 1 {
				assert *value == 'ads'
			} $else {
				assert false
			}
		} else {
			assert field.name == 'val2'
		}
	}
}

fn test_indirection_checking() {
	e := Encoder{}
	mut sb := Writer{}
	mut string_pointer := 'ads'
	e.encode_struct(StructType[string]{ val: &string_pointer }, mut sb)!
}
