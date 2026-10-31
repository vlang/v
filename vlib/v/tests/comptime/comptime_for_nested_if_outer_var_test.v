// A `$if` in a nested reflection loop can use the variables of both loops, also when
// the outer loop is over enum values or sum type variants.
struct Point {
	x int
	y string
}

enum Axis {
	x
	z
}

type Value = int | string

fn test_enum_value_and_field() {
	mut hits := []string{}
	$for v in Axis.values {
		$for dev in Point.fields {
			$if v.name == dev.name && dev.name != 'v.name' {
				hits << '${v.name}:${dev.name}'
			}
		}
	}
	assert hits == ['x:x']
}

fn test_variant_and_field() {
	mut hits := []string{}
	$for variant in Value.variants {
		$for field in Point.fields {
			$if variant.typ is int && field.name == 'x' {
				hits << 'int:${field.name}'
			} $else $if variant.typ is string && field.name.starts_with('y') {
				hits << 'string:${field.name}'
			}
		}
	}
	assert hits == ['int:x', 'string:y']
}

fn test_field_and_variant() {
	mut hits := []string{}
	$for field in Point.fields {
		$for variant in Value.variants {
			$if variant.typ is string && field.name == 'y' {
				hits << field.name
			}
		}
	}
	assert hits == ['y']
}
