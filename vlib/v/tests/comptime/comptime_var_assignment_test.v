struct FixedStruct1 {
	a int
	b string
	c ?int
	d ?string
}

fn encode_struct[T](val T) []string {
	mut out := []string{}
	$for field in T.fields {
		value := val.$(field.name)
		$if field.is_option {
			gg := value ?
			println(gg)
			out << '${value}'
		} $else {
			gg := value
			println(gg)
			out << gg.str()
		}
	}
	return out
}

fn test_main() {
	out := encode_struct(FixedStruct1{1, 'foo', 4, 'bar'})
	assert out[0] == '1'
	assert out[1] == 'foo'
	assert out[2] == 'Option(4)'
	assert out[3] == "Option('bar')"
}
