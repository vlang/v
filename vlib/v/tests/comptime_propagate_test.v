struct FixedStruct1 {
	a int
	b string
	c ?int
	d ?string
}

struct Encoder {}

fn test_main() {
	fixed := FixedStruct1{123, '456', 789, '321'}
	assert fixed.a.str() == '123'
	assert fixed.b.int() == 456

	assert fixed.c?.str() == '789'
	assert fixed.d?.int() == 321

	e := Encoder{}
	e.encode_struct(fixed)
}

fn (e &Encoder) encode_struct[T](val T) {
	$for field in T.fields {
		$if !field.is_option {
			$if field.typ is int {
				assert val.$(field.name).str() == '123'
			} $else $if field.typ is string {
				assert val.$(field.name).int() == 456
			}
		} $else {
			$if field.typ is ?int {
				assert val.$(field.name) ?.str() == '789'
			} $else $if field.typ is ?string {
				assert val.$(field.name) ?.int() == 321
			}
		}
	}
}
