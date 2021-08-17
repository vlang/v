module big

// Wrapper for https://github.com/kokke/tiny-bignum-c
#flag -I @VEXEROOT/thirdparty/bignum
#flag @VEXEROOT/thirdparty/bignum/bn.o
#include "bn.h"

struct C.bn {
mut:
	array [32]u32
}

// Big unsigned integer number.
type Number = C.bn

fn C.bignum_init(n &Number)

fn C.bignum_from_int(n &Number, i u64)

fn C.bignum_to_int(n &Number) int

fn C.bignum_from_string(n &Number, s &char, nbytes int)

fn C.bignum_to_string(n &Number, s &char, maxsize int)

// c = a + b
fn C.bignum_add(a &Number, b &Number, c &Number)

// c = a - b
fn C.bignum_sub(a &Number, b &Number, c &Number)

// c = a * b
fn C.bignum_mul(a &Number, b &Number, c &Number)

// c = a / b
fn C.bignum_div(a &Number, b &Number, c &Number)

// c = a % b
fn C.bignum_mod(a &Number, b &Number, c &Number)

// c = a/b d=a%b
fn C.bignum_divmod(a &Number, b &Number, c &Number, d &Number)

// c = a & b
fn C.bignum_and(a &Number, b &Number, c &Number)

// c = a | b
fn C.bignum_or(a &Number, b &Number, c &Number)

// c = a xor b
fn C.bignum_xor(a &Number, b &Number, c &Number)

// b = a << nbits
fn C.bignum_lshift(a &Number, b &Number, nbits int)

// b = a >> nbits
fn C.bignum_rshift(a &Number, b &Number, nbits int)

fn C.bignum_cmp(a &Number, b &Number) int

fn C.bignum_is_zero(a &Number) int

// n++
fn C.bignum_inc(n &Number)

// n--
fn C.bignum_dec(n &Number)

// c = a ^ b
fn C.bignum_pow(a &Number, b &Number, c &Number)

// b = integer_square_root_of(a)
fn C.bignum_isqrt(a &Number, b &Number)

// copy src number to dst number
fn C.bignum_assign(dst &Number, src &Number)

// new returns a bignum, initialized to 0
pub fn new() Number {
	return Number{}
}

// conversion actions to/from big numbers:
// from_int converts an ordinary int number `i` to big.Number
pub fn from_int(i int) Number {
	n := Number{}
	C.bignum_from_int(&n, i)
	return n
}

// from_u64 converts an ordinary u64 number `u` to big.Number
pub fn from_u64(u u64) Number {
	n := Number{}
	C.bignum_from_int(&n, u)
	return n
}

// from_hex_string converts a hex string to big.Number
pub fn from_hex_string(input string) Number {
	mut s := input.trim_prefix('0x')
	if s.len == 0 {
		s = '0'
	}
	padding := '0'.repeat((8 - s.len % 8) % 8)
	s = padding + s
	n := Number{}
	C.bignum_from_string(&n, &char(s.str), s.len)
	return n
}

// from_string converts a decimal string to big.Number
pub fn from_string(input string) Number {
	mut n := from_int(0)
	for _, c in input {
		d := from_int(int(c - `0`))
		n = (n * big.ten) + d
	}
	return n
}

// from_bytes converts an array of bytes (little-endian) to a big.Number.
// Higher precedence bytes are expected at lower indices in the array.
pub fn from_bytes(input []byte) ?Number {
	if input.len > 128 {
		return error('input array too large. big.Number can only hold up to 1024 bit numbers')
	}
	// pad input
	mut padded_input := []byte{len: ((input.len + 3) & ~0x3) - input.len, cap: (input.len + 3) & ~0x3, init: 0x0}
	padded_input << input
	// combine every 4 bytes into a u32 and insert into n.array
	mut n := Number{}
	for i := 0; i < padded_input.len; i += 4 {
		x3 := u32(padded_input[i])
		x2 := u32(padded_input[i + 1])
		x1 := u32(padded_input[i + 2])
		x0 := u32(padded_input[i + 3])
		val := (x3 << 24) | (x2 << 16) | (x1 << 8) | x0
		n.array[(padded_input.len - i) / 4 - 1] = val
	}
	return n
}

// .int() converts (a small) big.Number `n` to an ordinary integer.
pub fn (n &Number) int() int {
	r := C.bignum_to_int(n)
	return r
}

const (
	ten = from_int(10)
)

// .str returns a decimal representation of the big unsigned integer number n.
pub fn (n &Number) str() string {
	if n.is_zero() {
		return '0'
	}
	mut digits := []byte{}
	mut x := n.clone()

	for !x.is_zero() {
		// changes to reflect new api
		div, mod := divmod(&x, &big.ten)
		digits << byte(mod.int()) + `0`
		x = div
	}
	return digits.reverse().bytestr()
}

// .hexstr returns a hexadecimal representation of the bignum `n`
pub fn (n &Number) hexstr() string {
	mut buf := [8192]byte{}
	mut s := ''
	unsafe {
		bp := &buf[0]
		// NB: C.bignum_to_string(), returns the HEXADECIMAL representation of the bignum n
		C.bignum_to_string(n, &char(bp), 8192)
		s = tos_clone(bp)
	}
	if s.len == 0 {
		return '0'
	}
	return s
}

// //////////////////////////////////////////////////////////
// overloaded ops for the numbers:
pub fn (a &Number) + (b &Number) Number {
	c := Number{}
	C.bignum_add(a, b, &c)
	return c
}

pub fn (a &Number) - (b &Number) Number {
	c := Number{}
	C.bignum_sub(a, b, &c)
	return c
}

pub fn (a &Number) * (b &Number) Number {
	c := Number{}
	C.bignum_mul(a, b, &c)
	return c
}

pub fn (a &Number) / (b &Number) Number {
	c := Number{}
	C.bignum_div(a, b, &c)
	return c
}

pub fn (a &Number) % (b &Number) Number {
	c := Number{}
	C.bignum_mod(a, b, &c)
	return c
}

// divmod returns a pair of quotient and remainder from div modulo operation
// between two bignums `a` and `b`
pub fn divmod(a &Number, b &Number) (Number, Number) {
	c := Number{}
	d := Number{}
	C.bignum_divmod(a, b, &c, &d)
	return c, d
}

// //////////////////////////////////////////////////////////
pub fn cmp(a &Number, b &Number) int {
	return C.bignum_cmp(a, b)
}

pub fn (a &Number) is_zero() bool {
	return C.bignum_is_zero(a) != 0
}

pub fn (mut a Number) inc() {
	C.bignum_inc(&a)
}

pub fn (mut a Number) dec() {
	C.bignum_dec(&a)
}

pub fn pow(a &Number, b &Number) Number {
	c := Number{}
	C.bignum_pow(a, b, &c)
	return c
}

pub fn (a &Number) isqrt() Number {
	b := Number{}
	C.bignum_isqrt(a, &b)
	return b
}

// //////////////////////////////////////////////////////////
pub fn b_and(a &Number, b &Number) Number {
	c := Number{}
	C.bignum_and(a, b, &c)
	return c
}

pub fn b_or(a &Number, b &Number) Number {
	c := Number{}
	C.bignum_or(a, b, &c)
	return c
}

pub fn b_xor(a &Number, b &Number) Number {
	c := Number{}
	C.bignum_xor(a, b, &c)
	return c
}

pub fn (a &Number) lshift(nbits int) Number {
	b := Number{}
	C.bignum_lshift(a, &b, nbits)
	return b
}

pub fn (a &Number) rshift(nbits int) Number {
	b := Number{}
	C.bignum_rshift(a, &b, nbits)
	return b
}

pub fn (a &Number) clone() Number {
	b := Number{}
	C.bignum_assign(&b, a)
	return b
}

// //////////////////////////////////////////////////////////
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

// bytes returns an array of the bytes for the number `n`,
// in little endian format, where .bytes()[0] is the least
// significant byte. The result is NOT trimmed, and will contain 0s, even
// after the significant bytes.
// This method is faster than .bytes_trimmed(), but may be less convenient.
// Example: assert big.from_int(1).bytes()[0] == byte(0x01)
// Example: assert big.from_int(1024).bytes()[1] == byte(0x04)
// Example: assert big.from_int(1048576).bytes()[2] == byte(0x10)
pub fn (n &Number) bytes() []byte {
	mut res := []byte{len: 128, init: 0}
	unsafe { C.memcpy(res.data, n, 128) }
	return res
}

// bytes_trimmed returns an array of the bytes for the number `n`,
// in little endian format, where .bytes_trimmed()[0] is the least
// significant byte. The result is trimmed, so that *the last* byte
// of the result is also the the last meaningfull byte, != 0 .
// Example: assert big.from_int(1).bytes_trimmed() == [byte(0x01)]
// Example: assert big.from_int(1024).bytes_trimmed() == [byte(0x00), 0x04]
// Example: assert big.from_int(1048576).bytes_trimmed() == [byte(0x00), 0x00, 0x10]
pub fn (n &Number) bytes_trimmed() []byte {
	mut res := []byte{len: 128, init: 0}
	unsafe { C.memcpy(res.data, n, 128) }
	mut non_zero_idx := 127
	for ; non_zero_idx >= 0; non_zero_idx-- {
		if res[non_zero_idx] != 0 {
			break
		}
	}
	res.trim(non_zero_idx + 1)
	return res
}
