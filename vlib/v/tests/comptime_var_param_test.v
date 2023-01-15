struct Aa {
	sub AliasType
}

type AliasType = Bb

struct Bb {
	a int
}

fn encode_struct[U](val U) string {
	$for field in U.fields {
		encode_struct(val.$(field.name))
	}
	return val.str()
}

fn test_main() {
	aa := Aa{}

	assert encode_struct(aa) == 'Aa{
    sub:     AliasType(Bb{
    a: 0
})
}'
}
