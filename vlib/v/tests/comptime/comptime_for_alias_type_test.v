module main

struct Aa {
	sub AliasType
}

type AliasType = Bb

struct Bb {
	a int
}

fn encode_struct[U](val U, mut out []string) []string {
	$for field in U.fields {
		encode_struct(val.$(field.name), mut out)
		out << field.str()
	}
	return out
}

fn test_main() {
	aa := Aa{}
	mut out := []string{}
	assert encode_struct(aa, mut out).len == 2
}
