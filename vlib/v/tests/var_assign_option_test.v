struct StructType {
mut:
	b ?string
	a string
}

struct Decoder {}

fn (d &Decoder) decode[T]() []string {
	mut typ := T{}
	mut out := []string{}
	$if T is $Struct {
		$for field in T.fields {
			$if field.is_optional {
				typ.$(field.name) = 'a'
				out << typ.$(field.name).str()
			} $else {
				typ.$(field.name) = 'b'
				out << typ.$(field.name).str()
			}
		}
	}
	return out
}

fn test_main() {
	d := Decoder{}
	result := d.decode[StructType]()
	assert result[0] == "Option('a')"
	assert result[1] == 'b'
}
