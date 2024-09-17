struct Encoder {}

struct Writer {}

struct APrice {}

struct Association {
	association &Association = unsafe { nil }
	price       APrice
}

fn get_value_from_ref[T](val &T) T {
	return *val
}

fn (e &Encoder) encode_struct[U](val U, mut wr Writer) ! {
	$for field in U.fields {
		$if field.typ is &Association {
			assert get_value_from_ref(val.$(field.name)) == Association{
				association: unsafe { nil }
				price:       APrice{}
			}
		}
	}
}

fn test_main() {
	e := Encoder{}
	mut sb := Writer{}

	value := Association{
		association: &Association{
			price: APrice{}
		}
		price:       APrice{}
	}

	assert get_value_from_ref(&Association{
		price: APrice{}
	}) == Association{
		association: unsafe { nil }
		price:       APrice{}
	}

	e.encode_struct(value, mut sb)!
}
