struct FixedStruct1 {
	a int
	b ?int
	c ?int = 4
}

// struct FixedStruct2 {
//     b ?int
// }

struct Encoder {}

struct Writer {}

fn write1[T](val T) {
	println(val)
}

fn (wr &Writer) write2[T](val T) {
	println(val)
}

fn encode_struct[T](val T) map[string][]string {
	wr := Writer{}
	mut out := map[string][]string{}
	$if T is $struct {
		$for field in T.fields {
			value := val.$(field.name)
			$if field.typ is ?int {
				// work if comment lines 27 and 28
				write1(value)
				wr.write2(value)
				out[field.name] << '${value:d}'
			} $else {
				write1(value)
				wr.write2(value)
				out[field.name] << value.str()
			}
			// This work well
			$if field.is_option {
				write1(value)
				wr.write2(value)
				out[field.name] << '${value:d}'
			} $else {
				write1(value)
				wr.write2(value)
				out[field.name] << value.str()
			}
		}
	}
	return out
}

fn test_main() {
	// cgen error: cannot convert 'struct _option_int' to 'int'
	out := encode_struct(FixedStruct1{})
	assert out['a'] == ['0', '0']
	assert out['b'] == ['0', '0']
	assert out['c'] == ['4', '4']
}
