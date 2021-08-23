module big

struct JS.BigInt {}

#const jsNumber = Number;

pub struct Number {
}

pub fn new() Number {
	return Number{}
}

pub fn from_int(i int) Number {
	n := Number{}
	#n.value = BigInt(+i)

	return n
}

pub fn from_u64(u u64) Number {
	n := Number{}
	#n.value = BigInt(u.val)

	return n
}

pub fn from_hex_string(input string) Number {
	n := Number{}
	#n.value = BigInt(input.val)

	return n
}

pub fn from_string(input string) Number {
	n := Number{}
	#n.value = BigInt(input.val)

	return n
}

pub fn (n &Number) int() int {
	r := 0
	#r.val = jsNumber(n.val.value)

	return r
}

pub fn (n &Number) str() string {
	s := ''
	#s.str = n.val.value + ""

	return s
}

pub fn (a &Number) + (b &Number) Number {
	c := Number{}
	#c.value = a.val.value + b.val.value

	return c
}

pub fn (a &Number) - (b &Number) Number {
	c := Number{}
	#c.value = a.val.value - b.val.value

	return c
}

pub fn (a &Number) / (b &Number) Number {
	c := Number{}
	#c.value = a.val.value / b.val.value

	return c
}

pub fn (a &Number) * (b &Number) Number {
	c := Number{}
	#c.value = a.val.value * b.val.value

	return c
}

/*
pub fn (a &Number) % (b &Number) Number {
	c := Number{}
	# c.value = a.val.value % b.val.value
	return c
}*/

pub fn divmod(a &Number, b &Number) (Number, Number) {
	c := Number{}
	d := Number{}
	#c.value = a.val.value / b.val.value
	#d.value = a.val.value % b.val.value

	return c, d
}

pub fn cmp(a &Number, b &Number) int {
	res := 0

	#if (a.val.value < b.val.value) res.val = -1
	#else if (a.val.value > b.val.value) res.val = 1
	#else res.val = 0

	return res
}

pub fn (a &Number) is_zero() bool {
	res := false
	#res.val = a.val.value == BigInt(0)

	return res
}

pub fn (mut a Number) inc() {
	#a.val.value = a.val.value + BigInt(1)
}

pub fn (mut a Number) dec() {
	#a.val.value = a.val.value - BigInt(1)
}

pub fn (a &Number) isqrt() Number {
	b := Number{}
	#let x0 = a.val.value >> 1n
	#if (x0) {
	#let x1 = (x0 + a.val.value / x0) >> 1n
	#while (x1 < x0) {
	#x0 = x1
	#x1 = (x0 + a.val.value / x0) >> 1n
	#}
	#b.value = x0
	#} else { b.value = a.val.value; }

	return b
}

pub fn b_and(a &Number, b &Number) Number {
	c := Number{}
	#c.value = a.val.value & b.val.value

	return c
}

pub fn b_or(a &Number, b &Number) Number {
	c := Number{}
	#c.value = a.val.value | b.val.value

	return c
}

pub fn b_xor(a &Number, b &Number) Number {
	c := Number{}
	#c.value = a.val.value ^ b.val.value

	return c
}

pub fn (a &Number) lshift(nbits int) Number {
	c := Number{}
	#c.value = a.val.value << BigInt(+nbits)

	return c
}

pub fn (a &Number) rshift(nbits int) Number {
	c := Number{}
	#c.value = a.val.value << BigInt(+nbits)

	return c
}

pub fn (a &Number) clone() Number {
	b := Number{}
	#b.value = a.val.value

	return b
}

pub fn factorial(nn &Number) Number {
	mut n := nn.clone()
	mut a := nn.clone()
	n.dec()
	mut i := 1
	for !n.is_zero() {
		res := a * n
		n.dec()
		a = res
		i++
	}
	return a
}

pub fn fact(n int) Number {
	return factorial(from_int(n))
}
