module main

type NLRI = []u8

fn make_loopback() []NLRI {
	raw := [u8(32), 10, 0, 0, 1]
	mut nlris := []NLRI{cap: 1}
	nlris << NLRI(raw[0..5])
	return nlris
}

fn test_main() {
	r := make_loopback()
	assert r[0][0] == 32
	assert r[0][1] == 10
	assert r[0][2] == 0
	assert r[0][3] == 0
	assert r[0][4] == 1
}
