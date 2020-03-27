enum w_hex {
	a = 0x001
	b = 0x010
	c = 0x100
}

enum w_decimal {
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
	assert ca == int(w_decimal.a)
	assert cb == int(w_decimal.b)
	assert cc == int(w_decimal.c)

	assert int(w_hex.a) == ca
	assert int(w_hex.b) == cb
	assert int(w_hex.c) == cc

	assert int(w_hex.a) == int(w_decimal.a)
	assert int(w_hex.b) == int(w_decimal.b)
	assert int(w_hex.c) == int(w_decimal.c)
}

