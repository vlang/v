import math.big

fn test_integer_from_int() {
	assert big.integer_from_int(0).hex() == '0'
	assert big.integer_from_int(1).hex() == '1'
	assert big.integer_from_int(255).hex() == 'ff'
	assert big.integer_from_int(127).hex() == '7f'
	assert big.integer_from_int(1024).hex() == '400'
	assert big.integer_from_int(2147483647).hex() == '7fffffff'
}

fn test_integer_from_u64() {
	assert big.integer_from_u64(0).hex() == '0'
	assert big.integer_from_u64(1).hex() == '1'
	assert big.integer_from_u64(255).hex() == 'ff'
	assert big.integer_from_u64(127).hex() == '7f'
	assert big.integer_from_u64(1024).hex() == '400'
	assert big.integer_from_u64(4294967295).hex() == 'ffffffff'
	assert big.integer_from_u64(4398046511104).hex() == '40000000000'
	max_value := big.integer_from_u64(-1)

	assert max_value.hex() == 'ffffffffffffffff'
}

fn test_integer_from_bytes() {
	assert big.integer_from_bytes([]).hex() == '0'
	assert big.integer_from_bytes([byte(0)]).hex() == '0'
	assert big.integer_from_bytes([byte(0x13), 0x37]).hex() == '1337'
	assert big.integer_from_bytes([byte(0x13), 0x37, 0xca]).hex() == '1337ca'
	assert big.integer_from_bytes([byte(0x13), 0x37, 0xca, 0xfe]).hex() == '1337cafe'
	assert big.integer_from_bytes([byte(0x13), 0x37, 0xca, 0xfe, 0xba]).hex() == '1337cafeba'
	assert big.integer_from_bytes([byte(0x13), 0x37, 0xca, 0xfe, 0xba, 0xbe]).hex() == '1337cafebabe'

	mut bytes := []byte{cap: 1024}
	mut expected := ''
	for i := 0; i < bytes.cap; i++ {
		bytes << byte(i)
		expected = expected + byte(i).hex()
	}
	assert big.integer_from_bytes(bytes).hex() == expected.trim_left('0')
}

fn test_bytes() {
	result1, sign1 := big.integer_from_u64(0x1337cafebabe).bytes()
	assert result1 == [byte(0x13), 0x37, 0xca, 0xfe, 0xba, 0xbe]
	assert sign1 == 1

	mut bytes := []byte{cap: 1024}
	mut expected := ''
	for i := 0; i < bytes.cap; i++ {
		bytes << byte(i | 1)
		expected = expected + byte(i).hex()
	}
	result2, sign2 := big.integer_from_bytes(bytes).bytes()
	assert result2 == bytes
	assert sign2 == 1
}

fn test_addition() {
	a := big.integer_from_int(2)
	b := big.integer_from_int(3)
	c := a + b
	assert c.hex() == '5'

	assert (big.integer_from_int(1024) + big.integer_from_int(1024)).hex() == '800'

	fib1 := big.integer_from_string('84885164052257330097714121751630835360966663883732297726369399') or {
		panic('Cannot read decimal')
	}
	fib2 := big.integer_from_string('137347080577163115432025771710279131845700275212767467264610201') or {
		panic('Cannot read decimal')
	}
	assert (fib1 + fib2).str() == '222232244629420445529739893461909967206666939096499764990979600'
}

fn test_subtraction() {
	a := big.integer_from_int(2)
	b := big.integer_from_int(3)
	assert (a - b).hex() == '-1'
	assert (b - a).hex() == '1'

	c := big.integer_from_int(1024)
	assert (c - c) == big.zero_int

	assert big.integer_from_int(-37) - big.integer_from_int(-54) == big.integer_from_int(17)
}

fn test_multiplication() {
	a := big.integer_from_int(2)
	b := big.integer_from_int(3)
	c := big.integer_from_int(6)
	assert a * b == c
	assert big.integer_from_int(-869) * big.integer_from_int(789) == big.integer_from_int(-685641)
	e := big.integer_from_u32(1024)
	e2 := e * e
	e4 := e2 * e2
	e8 := e2 * e2 * e2 * e2
	e9 := e8 + big.one_int
	d := ((e9 * e9) + b) * c
	assert e4.hex() == '10000000000'
	assert e8.hex() == '100000000000000000000'
	assert e9.hex() == '100000000000000000001'
	assert d.hex() == '60000000000000000000c00000000000000000018'
}

fn test_division() {
	a := big.integer_from_u64(2)
	b := big.integer_from_u64(3)
	c := b / a
	assert c.hex() == '1'
	assert (b % a).hex() == '1'
	e := big.integer_from_u64(1024) // dec(1024) == hex(0x400)
	ee := e / e
	assert ee.hex() == '1'
	assert (e / a).hex() == '200'
	assert (e / (a * a)).hex() == '100'

	assert (b / a).hex() == '1'
}

fn test_mod() {
	assert (big.integer_from_u64(13) % big.integer_from_u64(10)).int() == 3
	assert (big.integer_from_u64(13) % big.integer_from_u64(9)).int() == 4
	assert (big.integer_from_u64(7) % big.integer_from_u64(5)).int() == 2
}

fn test_divmod() {
	x, y := big.integer_from_u64(13).div_mod(big.integer_from_u64(10))
	assert x.int() == 1
	assert y.int() == 3
	p, q := big.integer_from_u64(13).div_mod(big.integer_from_u64(9))
	assert p.int() == 1
	assert q.int() == 4
	c, d := big.integer_from_u64(7).div_mod(big.integer_from_u64(5))
	assert c.int() == 1
	assert d.int() == 2
	x1 := big.integer_from_string('2103180314840157') or { panic('Cannot read decimal') }
	y1 := big.integer_from_string('1631403814113') or { panic('Cannot read decimal') }
	q0 := big.integer_from_int(1289)
	r0 := big.integer_from_string('300798448500') or { panic('Cannot read decimal') }
	q1, r1 := x1.div_mod(y1)
	assert q1 == q0
	assert r1 == r0

	e := big.integer_from_string('21408410031413414147401') or { panic('Cannot read decimal') }
	f := big.integer_from_string('3130541314113413') or { panic('Cannot read decimal') }
	g, h := e.div_mod(f)
	assert g.str() == '6838564'
	assert h.str() == '2900204736088469'
}

fn test_comparison() {
	values := [-3, 13, 52, 6, 41]
	for value in values {
		x := big.integer_from_int(value)
		assert x == x
		assert x <= x
		assert x >= x
	}

	a := big.integer_from_int(-45)
	b := big.integer_from_int(35)

	assert a < b
	assert a <= b
	assert b > a
	assert b >= a
}

fn test_conversion() {
	ten := big.integer_from_int(10)

	mut n := big.integer_from_u64(-1)

	mut digits := []rune{}
	for n.signum != 0 {
		quot, rem := n.div_mod(ten)
		digits << rune(rem.int()) + `0`
		n = quot
	}

	assert digits.reverse().string() == '18446744073709551615'
}

fn test_integer_from_string() {
	a := big.integer_from_string('00000000') or { panic('Zero int test fails') }
	assert a == big.zero_int
	b := big.integer_from_radix('00', 4) or { panic('Zero int test fails') }
	assert b == big.zero_int
	assert a == b

	string_values := ['0', '1', '0012', '1349173614', '+24', '-325']
	int_values := [0, 1, 12, 1349173614, 24, -325]
	for index in 0 .. string_values.len {
		x := big.integer_from_string(string_values[index]) or {
			panic('Could not convert decimal string')
		}
		y := big.integer_from_int(int_values[index])
		assert x == y
	}
}

fn test_integer_from_powers_of_2() {
	assert (big.integer_from_radix('101010', 2) or { panic('Cannot read binary') }).int() == 42
	assert (big.integer_from_radix('1010', 2) or { panic('Cannot read binary') }).int() == 10
	assert (big.integer_from_radix('-0000101', 2) or { panic('Cannot read binary') }).int() == -5

	assert (big.integer_from_radix('CAFE', 16) or { panic('Cannot read hexadecimal') }).int() == 0xCAFE
	assert (big.integer_from_radix('DED', 16) or { panic('Cannot read hexadecimal') }).int() == 0xDED
	assert (big.integer_from_radix('-abcd', 16) or { panic('Cannot read hexadecimal') }).int() == -0xabcd
}

fn test_from_and_to_hex() {
	assert (big.integer_from_radix('123', 16) or { panic('Cannot read hexadecimal') }).hex() == '123'
	for i in 1 .. 33 {
		input := 'e'.repeat(i)
		output := (big.integer_from_radix(input, 16) or { panic('Cannot read hexadecimal') }).hex()
		assert input == output
	}
	assert (big.integer_from_string('0') or { panic('Cannot read decimal') }).str() == '0'
}

fn test_str() {
	assert big.integer_from_u64(255).str() == '255'
	assert big.integer_from_u64(127).str() == '127'
	assert big.integer_from_u64(1024).str() == '1024'
	assert big.integer_from_u64(4294967295).str() == '4294967295'
	assert big.integer_from_u64(4398046511104).str() == '4398046511104'
	assert big.integer_from_u64(-1).str() == '18446744073709551615'
	assert (big.integer_from_radix('e'.repeat(80), 16) or { panic('Cannot read hexadecimal') }).str() == '1993587900192849410235353592424915306962524220866209251950572167300738410728597846688097947807470'
}

fn test_exponentiation() {
	a := big.integer_from_int(2)
	assert a.pow(0).int() == 1
	assert a.pow(1).int() == 2
	assert a.pow(5).int() == 32
	assert a.pow(10).int() == 1024
	assert a.pow(30).int() == 1073741824

	exp_array := [u32(5), 7, 234, 524, 291, 13051]
	for exp in exp_array {
		expected := '1' + '0'.repeat(int(exp))
		actual := a.pow(exp)
		assert actual.binary_str() == expected
	}

	result := '66325146064916587705822805477951823674769212922003325230500180789514487101799702287247301347816140714887582527826252837635296749781071351621748491469338347097923896026211183517655658952346069454893422558286798338709431368762851475568899541999504754550056265493269010870696623999709399529395247064542825851568385196637089440522882877102429945439977107582295420418108331098961838419917230847980056560488541780255425015021238743932289115066701337398107639567748102191005710201353615093246958907555634902309636451244444952203735074916066229982498598205421944122042066749035283837586883383420374291325389757869347147357807188516650352693616763867685354382631931356465247637321960345782811272139101785279798666504361229957479336436466489780129445016691164329417001378480690804715301830926348058624'

	assert big.integer_from_int(324).pow(u32(315)).str() == result
}

fn test_mod_exponentiation() {
	divisor := big.integer_from_int(632)
	assert big.integer_from_int(324).mod_pow(u32(315), divisor) == big.integer_from_int(512)

	a := big.integer_from_int(65)
	b := big.integer_from_int(2790)
	div := big.integer_from_int(3233)

	assert a.mod_pow(17, div) == b
	assert b.mod_pow(413, div) == a
}

fn test_big_mod_exponentiation_1() {
	a := big.integer_from_int(23)
	b := big.integer_from_int(35)
	c := big.integer_from_int(4205)
	result := big.integer_from_int(552)
	assert a.big_mod_pow(b, c) == result
}

fn test_big_mod_exponentiation_2() {
	a := big.integer_from_string('2222589987119231759186196754430278233855361024') or {
		panic('Could not read big integer')
	}
	b := big.integer_from_string('3104719823194124242') or { panic('Could not read big integer') }
	c := big.integer_from_string('15121308410741') or { panic('Could not read big integer') }
	result := big.integer_from_string('487881863537') or { panic('Could not read big integer') }
	assert a.big_mod_pow(b, c) == result
}

fn test_big_mod_exponentiation_3() {
	a := big.integer_from_string('3192874698137469817346981364918346578619384619387463987413') or {
		panic('Could not read big integer')
	}
	b := big.integer_from_string('3104981983749813749137493871037') or {
		panic('Could not read big integer')
	}
	c := big.integer_from_string('2137476918375698711341313') or {
		panic('Could not read big integer')
	}
	result := big.integer_from_string('418107071760838517119254') or {
		panic('Could not read big integer')
	}
	assert a.big_mod_pow(b, c) == result
}

fn test_gcd() {
	assert big.integer_from_int(0).gcd(big.integer_from_int(0)) == big.zero_int
	assert big.integer_from_int(10).gcd(big.integer_from_int(0)) == big.integer_from_int(10)
	assert big.integer_from_int(0).gcd(big.integer_from_int(-18)) == big.integer_from_int(18)
	assert big.integer_from_int(51).gcd(big.integer_from_int(22)) == big.one_int
	assert big.integer_from_int(98).gcd(big.integer_from_int(56)) == big.integer_from_int(14)
	assert big.integer_from_int(98).gcd(big.integer_from_int(56)) == big.integer_from_int(14)

	a := big.integer_from_string('67116917544110') or { panic('Could not read decimal') }
	b := big.integer_from_string('60943431483592') or { panic('Could not read decimal') }
	c := big.integer_from_string('6299482') or { panic('Could not read decimal') }
	assert a.gcd(b) == c
}

fn test_factorial() {
	f5 := big.integer_from_u64(5).factorial()
	assert f5.hex() == '78'
	f100 := big.integer_from_u64(100).factorial()
	assert f100.hex() == '1b30964ec395dc24069528d54bbda40d16e966ef9a70eb21b5b2943a321cdf10391745570cca9420c6ecb3b72ed2ee8b02ea2735c61a000000000000000000000000'
}

fn test_inc_and_dec() {
	mut a := big.integer_from_int(2)
	mut b := big.integer_from_int(3)
	mut c := big.integer_from_int(4)

	a.inc()
	c.dec()
	assert a == b
	assert b == c
}

fn test_lshift() {
	assert big.integer_from_int(45).lshift(2) == big.integer_from_int(45 * 4)
	assert big.integer_from_int(45).lshift(3) == big.integer_from_int(45 * 8)
	assert big.integer_from_int(45).lshift(4) == big.integer_from_int(45 * 16)
	assert big.integer_from_int(45).lshift(5) == big.integer_from_int(45 * 32)
	assert big.integer_from_u64(0xabcedabcde).lshift(20) == big.integer_from_u64(0xabcedabcde00000)
	assert big.integer_from_bytes([byte(1), 1, 1]).lshift(56) == big.integer_from_bytes([
		byte(1),
		1,
		1,
		0,
		0,
		0,
		0,
		0,
		0,
		0,
	])
}

fn test_rshift() {
	assert big.integer_from_int(45).rshift(3) == big.integer_from_int(5)
	assert big.integer_from_int(0x13374956).rshift(16) == big.integer_from_int(0x1337)
	assert big.integer_from_bytes([
		byte(1),
		1,
		1,
		0,
		0,
		0,
		0,
		0,
		0,
		0,
	]).rshift(56) == big.integer_from_bytes([byte(1), 1, 1])
}

fn test_isqrt() {
	for i in 0 .. 1000 {
		a := big.integer_from_int(i)
		b := big.integer_from_int(i * i)
		assert b.isqrt() == a
	}
	values := [
		'314',
		'213149',
		'2198614',
		'318014',
		'1000000000',
		'1000131039410',
		'2148170394871039847019843349714981',
	]
	for value in values {
		a := big.integer_from_string(value) or { panic('Cannot read from decimal') }
		b := a * a
		assert b.isqrt() == a
	}
}

fn test_bitwise_ops() {
	a := big.integer_from_int(1).lshift(512)
	b := a - big.one_int
	assert a.bitwise_and(b) == big.zero_int
	assert b.bitwise_xor(b) == big.zero_int
	assert b.bitwise_or(b) == b
	assert b.bitwise_and(b) == b
	assert b.bitwise_not() == big.zero_int
}

fn test_get_bit() {
	x := big.integer_from_int(42)
	assert x.get_bit(0) == false
	assert x.get_bit(1) == true
	assert x.get_bit(2) == false
	assert x.get_bit(3) == true
	assert x.get_bit(4) == false
	assert x.get_bit(5) == true
	assert x.get_bit(10) == false

	values := ['1001001001001010010010100100100101011101001001010',
		'1111111111111111111111111111111111111111111111111',
		'0000000000000000000000000000000000000000010000', '1110010', '0001001']
	for value in values {
		a := big.integer_from_radix(value, 2) or { panic('Could not read binary') }
		bits := []bool{len: value.len, init: value[value.len - 1 - it] == `1`}
		for i in 0 .. value.len {
			assert a.get_bit(i) == bits[i]
		}
	}
}

fn test_set_bit() {
	mut a := big.integer_from_int(32)
	a.set_bit(1, true)
	assert a.int() == 34
	a.set_bit(1, false)
	a.set_bit(3, true)
	assert a.int() == 40
	a.set_bit(50, true)
	assert a == big.one_int.lshift(50) + big.integer_from_int(40)
	b := a
	a.set_bit(100, false)
	assert a == b
}
