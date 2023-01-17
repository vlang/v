struct StructType {
mut:
	a string
	b ?string
	//	c ?int
	//	d ?f64
	//	e ?[]string
}

struct Decoder {}

fn (d &Decoder) decode[T](typ T) T {
	$if T is $Struct {
		dump('is Struct')
		$for field in T.fields {
			$if field.is_option {
				dump(typ.$(field.name) ?.str())
				typ.$(field.name) = none
				dump(typ.$(field.name) ?.str())
			}
		}
	}
	return typ
}

fn test_comptime() {
	d := Decoder{}
	result := d.decode(StructType{
		a: 'foo'
		b: 'foo'
	})
	println(result)
}

fn test_cast_option() {
	mut x := ?int(123)
	dump(x)
	x = none
	dump(x)
}

fn test_assign_from_option() {
	mut x := ?int(123)
	mut y := x
	println(y)
}

fn test_blank_assign() {
	_ := ?bool(false)
}
