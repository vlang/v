struct Aa {
	sub AliasType
}

type AliasType = Bb

struct Bb {
	a int
}

fn encode_struct[U](val U) {
	dump(val)
	println(val)
	$for field in U.fields {
		encode_struct(val.$(field.name))
	}
}

fn test_main() {
	aa := Aa{}

	encode_struct(aa)
	assert true
}
