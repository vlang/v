import math.big

fn test_new_big() {
	n := big.new()
	assert sizeof(big.Number) == 128
	assert n.hexstr() == '0'
}

fn test_from_int() {
	assert big.from_int(255).hexstr() == 'ff'
	assert big.from_int(127).hexstr() == '7f'
	assert big.from_int(1024).hexstr() == '400'
	assert big.from_int(2147483647).hexstr() == '7fffffff'
	assert big.from_int(-1).hexstr() == 'ffffffffffffffff'
}

fn test_from_u64() {
	assert big.from_u64(255).hexstr() == 'ff'
	assert big.from_u64(127).hexstr() == '7f'
	assert big.from_u64(1024).hexstr() == '400'
	assert big.from_u64(4294967295).hexstr() == 'ffffffff'
	assert big.from_u64(4398046511104).hexstr() == '40000000000'
	assert big.from_u64(-1).hexstr() == 'ffffffffffffffff'
}

fn test_plus() {
	mut a := big.from_u64(2)
	b := big.from_u64(3)
	c := a + b
	assert c.hexstr() == '5'
	assert (big.from_u64(1024) + big.from_u64(1024)).hexstr() == '800'
	a += b
	assert a.hexstr() == '5'
	a.inc()
	assert a.hexstr() == '6'
	a.dec()
	a.dec()
	assert a.hexstr() == '4'
}

fn test_minus() {
	a := big.from_u64(2)
	mut b := big.from_u64(3)
	c := b - a
	assert c.hexstr() == '1'
	e := big.from_u64(1024)
	ee := e - e
	assert ee.hexstr() == '0'
	b -= a
	assert b.hexstr() == '1'
}

fn test_divide() {
	a := big.from_u64(2)
	mut b := big.from_u64(3)
	c := b / a
	assert c.hexstr() == '1'
	assert (b % a).hexstr() == '1'
	e := big.from_u64(1024) // dec(1024) == hex(0x400)
	ee := e / e
	assert ee.hexstr() == '1'
	assert (e / a).hexstr() == '200'
	assert (e / (a * a)).hexstr() == '100'
	b /= a
	assert b.hexstr() == '1'
}

fn test_multiply() {
	mut a := big.from_u64(2)
	b := big.from_u64(3)
	c := b * a
	assert c.hexstr() == '6'
	e := big.from_u64(1024)
	e2 := e * e
	e4 := e2 * e2
	e8 := e2 * e2 * e2 * e2
	e9 := e8 + big.from_u64(1)
	d := ((e9 * e9) + b) * c
	assert e4.hexstr() == '10000000000'
	assert e8.hexstr() == '100000000000000000000'
	assert e9.hexstr() == '100000000000000000001'
	assert d.hexstr() == '60000000000000000000c00000000000000000018'
	a *= b
	assert a.hexstr() == '6'
}

fn test_mod() {
	assert (big.from_u64(13) % big.from_u64(10)).int() == 3
	assert (big.from_u64(13) % big.from_u64(9)).int() == 4
	assert (big.from_u64(7) % big.from_u64(5)).int() == 2
}

fn test_divmod() {
	x, y := big.divmod(big.from_u64(13), big.from_u64(10))
	assert x.int() == 1
	assert y.int() == 3
	p, q := big.divmod(big.from_u64(13), big.from_u64(9))
	assert p.int() == 1
	assert q.int() == 4
	c, d := big.divmod(big.from_u64(7), big.from_u64(5))
	assert c.int() == 1
	assert d.int() == 2
}

fn test_from_str() {
	assert big.from_string('9870123').str() == '9870123'
	assert big.from_string('').str() == '0'
	assert big.from_string('0').str() == '0'
	assert big.from_string('1').str() == '1'
	for i := 1; i < 307; i += 61 {
		input := '9'.repeat(i)
		out := big.from_string(input).str()
		// eprintln('>> i: $i input: $input.str()')
		// eprintln('>> i: $i   out: $out.str()')
		assert input == out
	}
}

fn test_from_hex_str() {
	assert big.from_hex_string('0x123').hexstr() == '123'
	for i in 1 .. 33 {
		input := 'e'.repeat(i)
		out := big.from_hex_string(input).hexstr()
		assert input == out
	}
	assert big.from_string('0').hexstr() == '0'
}

fn test_str() {
	assert big.from_u64(255).str() == '255'
	assert big.from_u64(127).str() == '127'
	assert big.from_u64(1024).str() == '1024'
	assert big.from_u64(4294967295).str() == '4294967295'
	assert big.from_u64(4398046511104).str() == '4398046511104'
	assert big.from_int(int(4294967295)).str() == '18446744073709551615'
	assert big.from_int(-1).str() == '18446744073709551615'
	assert big.from_hex_string('e'.repeat(80)).str() == '1993587900192849410235353592424915306962524220866209251950572167300738410728597846688097947807470'
}

fn test_factorial() {
	f5 := big.factorial(big.from_u64(5))
	assert f5.hexstr() == '78'
	f100 := big.factorial(big.from_u64(100))
	assert f100.hexstr() == '1b30964ec395dc24069528d54bbda40d16e966ef9a70eb21b5b2943a321cdf10391745570cca9420c6ecb3b72ed2ee8b02ea2735c61a000000000000000000000000'
}

fn trimbytes(n int, x []byte) []byte {
	mut res := x.clone()
	res.trim(n)
	return res
}

fn test_bytes() {
	assert big.from_int(0).bytes().len == 128
	assert big.from_hex_string('e'.repeat(100)).bytes().len == 128
	assert trimbytes(3, big.from_int(1).bytes()) == [byte(0x01), 0x00, 0x00]
	assert trimbytes(3, big.from_int(1024).bytes()) == [byte(0x00), 0x04, 0x00]
	assert trimbytes(3, big.from_int(1048576).bytes()) == [byte(0x00), 0x00, 0x10]
}

fn test_bytes_trimmed() {
	assert big.from_int(0).bytes_trimmed().len == 0
	assert big.from_hex_string('AB'.repeat(50)).bytes_trimmed().len == 50
	assert big.from_int(1).bytes_trimmed() == [byte(0x01)]
	assert big.from_int(1024).bytes_trimmed() == [byte(0x00), 0x04]
	assert big.from_int(1048576).bytes_trimmed() == [byte(0x00), 0x00, 0x10]
}

fn test_from_bytes() ? {
	assert big.from_bytes([]) ?.hexstr() == '0'
	assert big.from_bytes([byte(0x13)]) ?.hexstr() == '13'
	assert big.from_bytes([byte(0x13), 0x37]) ?.hexstr() == '1337'
	assert big.from_bytes([byte(0x13), 0x37, 0xca]) ?.hexstr() == '1337ca'
	assert big.from_bytes([byte(0x13), 0x37, 0xca, 0xfe]) ?.hexstr() == '1337cafe'
	assert big.from_bytes([byte(0x13), 0x37, 0xca, 0xfe, 0xba]) ?.hexstr() == '1337cafeba'
	assert big.from_bytes([byte(0x13), 0x37, 0xca, 0xfe, 0xba, 0xbe]) ?.hexstr() == '1337cafebabe'
	assert big.from_bytes([]byte{len: 128, init: 0x0}) ?.hexstr() == '0'
	if x := big.from_bytes([]byte{len: 129, init: 0x0}) {
		return error('expected error, got $x')
	}
}
