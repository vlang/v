struct Struc {
	a string
	b int
	c ?string
	d ?int
}

pub struct Count {
mut:
	total int
}

// count_chars count json sizen without new encode
pub fn (mut count Count) count_chars[T](val T) {
	$if val is $option {
		workaround := val
		if workaround != none {
			count.count_chars(val)
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
		count.count_chars(va) // works
		count.count_chars(val.$(field.name)) // Not works
	}
}

fn test_main() {
	struc := Struc{}
	mut count := Count{}
	count.count_chars(struc)
	assert true
}
