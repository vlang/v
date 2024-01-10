struct FixedStruct1 {
	a int
	b string
}

struct FixedStruct2 {
	c ?int
	d ?string
}

struct Writer {}

fn write1[T](val T) {
	println(val)
}

fn (wr &Writer) write2[T](val T) {
	println(val)
}

fn encode_struct[T](val T) bool {
	wr := Writer{}
	$for field in T.fields {
		value := val.$(field.name)
		write1(value)
		wr.write2(value)
	}
	return true
}

fn test_main() {
	assert encode_struct(FixedStruct1{}) == true
	assert encode_struct(FixedStruct2{}) == true
}
