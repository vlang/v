enum WHex {
	a = 0x001
	b = 0x010
	c = 0x100
}

enum WDecimal {
	a = 1
	b = 16
	c = 256
}

const (
	ca = 1
	cb = 16
	cc = 256
)

fn test_enum_hex() {
	assert ca == int(WDecimal.a)
	assert cb == int(WDecimal.b)
	assert cc == int(WDecimal.c)
	assert int(WHex.a) == ca
	assert int(WHex.b) == cb
	assert int(WHex.c) == cc
	assert int(WHex.a) == int(WDecimal.a)
	assert int(WHex.b) == int(WDecimal.b)
	assert int(WHex.c) == int(WDecimal.c)
}
