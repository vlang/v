module big

// Compares the magnitude of the two unsigned integers represented the given
// digit arrays. Returns -1 if a < b, 0 if a == b and +1 if a > b. Here
// a is operand_a and b is operand_b (for brevity).
@[direct_array_access]
fn compare_digit_array(operand_a []u64, operand_b []u64) int {
	a_len := operand_a.len
	b_len := operand_b.len
	if a_len != b_len {
		return if a_len < b_len { -1 } else { 1 }
	}
	// They have the same number of digits now
	// Go from the most significant digit to the least significant one
	for index := a_len - 1; index >= 0; index-- {
		a_digit := operand_a[index]
		b_digit := operand_b[index]
		if a_digit != b_digit {
			return if a_digit < b_digit { -1 } else { 1 }
		}
	}
	return 0
}

// Add the digits in operand_a and operand_b and stores the result in sum.
// This function does not perform any allocation and assumes that the storage is
// large enough. It may affect the last element, based on the presence of a carry
@[direct_array_access]
fn add_digit_array(operand_a []u64, operand_b []u64, mut sum []u64) {
	// Zero length cases
	if operand_a.len == 0 {
		for index in 0 .. operand_b.len {
			sum[index] = operand_b[index]
		}
	}
	if operand_b.len == 0 {
		for index in 0 .. operand_a.len {
			sum[index] = operand_a[index]
		}
	}

	// First pass intersects with both operands
	smaller_limit := imin(operand_a.len, operand_b.len)
	larger_limit := imax(operand_a.len, operand_b.len)
	mut a, mut b := if operand_a.len >= operand_b.len {
		operand_a, operand_b
	} else {
		operand_b, operand_a
	}
	mut carry := u64(0)
	for index in 0 .. smaller_limit {
		partial := carry + a[index] + b[index]
		sum[index] = u64(partial) & max_digit
		carry = u64(partial >> digit_bits)
	}

	for index in smaller_limit .. larger_limit {
		partial := carry + a[index]
		sum[index] = u64(partial) & max_digit
		carry = u64(partial >> digit_bits)
	}

	if carry == 0 {
		sum.delete_last()
	} else {
		sum[larger_limit] = carry
	}
}

// Subtracts operand_b from operand_a and stores the difference in storage.
// It assumes operand_a contains the larger "integer" and that storage is
// the same size as operand_a and is 0
@[direct_array_access]
fn subtract_digit_array(operand_a []u64, operand_b []u64, mut storage []u64) {
	// Zero length cases
	if operand_a.len == 0 {
		// nothing to subtract from
		return
	}
	if operand_b.len == 0 {
		// nothing to subtract
		for index in 0 .. operand_a.len {
			storage[index] = operand_a[index]
		}
	}

	mut carry := false
	for index in 0 .. operand_b.len {
		mut a_digit := operand_a[index]
		b_digit := operand_b[index] + if carry { u64(1) } else { u64(0) }
		carry = a_digit < b_digit
		if carry {
			a_digit = a_digit | (u64(1) << digit_bits)
		}
		storage[index] = a_digit - b_digit
	}

	for index in operand_b.len .. operand_a.len {
		mut a_digit := operand_a[index]
		b_digit := if carry { u64(1) } else { u64(0) }
		carry = a_digit < b_digit
		if carry {
			a_digit = a_digit | (u64(1) << digit_bits)
		}
		storage[index] = a_digit - b_digit
	}

	shrink_tail_zeros(mut storage)
}

const karatsuba_multiplication_limit = 70

const toom3_multiplication_limit = 360

@[inline]
fn multiply_digit_array(operand_a []u64, operand_b []u64, mut storage []u64) {
	max_len := if operand_a.len >= operand_b.len {
		operand_a.len
	} else {
		operand_b.len
	}
	if max_len >= toom3_multiplication_limit {
		toom3_multiply_digit_array(operand_a, operand_b, mut storage)
	} else if max_len >= karatsuba_multiplication_limit {
		karatsuba_multiply_digit_array(operand_a, operand_b, mut storage)
	} else {
		simple_multiply_digit_array(operand_a, operand_b, mut storage)
	}
}

// Multiplies the unsigned (non-negative) integers represented in a and b and the product is
// stored in storage. It assumes that storage has length equal to the sum of lengths
// of a and b. Length refers to length of array, that is, digit count.
@[direct_array_access]
fn simple_multiply_digit_array(operand_a []u64, operand_b []u64, mut storage []u64) {
	for b_index in 0 .. operand_b.len {
		mut hi := u64(0)
		mut lo := u64(0)
		for a_index in 0 .. operand_a.len {
			hi, lo = mul_add_64(operand_a[a_index], operand_b[b_index], storage[a_index + b_index] +
				hi)
			storage[a_index + b_index] = lo & max_digit
			hi = (hi << (64 - digit_bits)) | (lo >> digit_bits)
		}
		if hi != 0 {
			storage[b_index + operand_a.len] = hi
		}
	}
	shrink_tail_zeros(mut storage)
}

// Stores the product of the unsigned (non-negative) integer represented in a and the digit in value
// in the storage array. It assumes storage is pre-initialised and populated with 0's
@[direct_array_access]
fn multiply_array_by_digit(operand_a []u64, value u64, mut storage []u64) {
	if value == 0 {
		storage.clear()
		return
	}
	if value == 1 {
		for index in 0 .. operand_a.len {
			storage[index] = operand_a[index]
		}
		shrink_tail_zeros(mut storage)
		return
	}
	mut hi := u64(0)
	mut lo := u64(0)
	for index in 0 .. operand_a.len {
		hi, lo = mul_add_64(operand_a[index], value, hi)
		storage[index] = lo & max_digit
		hi = hi << (64 - digit_bits) + (lo >> digit_bits)
	}

	if hi > 0 {
		storage[operand_a.len] = hi
	}
	shrink_tail_zeros(mut storage)
}

// Divides the non-negative integer in a by non-negative integer b and store the two results
// in quotient and remainder respectively. It is different from the rest of the functions
// because it assumes that quotient and remainder are empty zero length arrays. They can be
// made to have appropriate capacity though
@[direct_array_access]
fn divide_digit_array(operand_a []u64, operand_b []u64, mut quotient []u64, mut remainder []u64) {
	cmp_result := compare_digit_array(operand_a, operand_b)
	// a == b => q, r = 1, 0
	if cmp_result == 0 {
		quotient << 1
		for quotient.len > 1 {
			quotient.delete_last()
		}
		remainder.clear()
		return
	}

	// a < b => q, r = 0, a
	if cmp_result < 0 {
		quotient.clear()
		remainder << operand_a
		return
	}
	if operand_b.len == 1 {
		divide_array_by_digit(operand_a, operand_b[0], mut quotient, mut remainder)
	} else {
		divide_array_by_array(operand_a, operand_b, mut quotient, mut remainder)
	}
}

// Performs division on the non-negative dividend in a by the single digit divisor b. It assumes
// quotient and remainder are empty zero length arrays without previous allocation
@[direct_array_access]
fn divide_array_by_digit(operand_a []u64, divisor u64, mut quotient []u64, mut remainder []u64) {
	if operand_a.len == 1 {
		// 1 digit for both dividend and divisor
		dividend := operand_a[0]
		q := dividend / divisor
		if q != 0 {
			quotient << q
		}
		rem := dividend % divisor
		if rem != 0 {
			remainder << rem
		}
		return
	}
	// Dividend has more digits
	mut rem := u64(0)
	mut quo := u64(0)
	mut qtemp := []u64{len: quotient.cap}
	divisor64 := u64(divisor)

	// Perform division step by step
	for index := operand_a.len - 1; index >= 0; index-- {
		hi := rem >> (64 - digit_bits)
		lo := rem << digit_bits | operand_a[index]
		quo, rem = div_64(hi, lo, divisor64)
		qtemp[index] = quo & max_digit
	}
	// Remove leading zeros from quotient
	shrink_tail_zeros(mut qtemp)
	quotient << qtemp
	remainder << rem
	shrink_tail_zeros(mut remainder)
}

const newton_division_limit = 1_000_000

@[inline]
fn divide_array_by_array(operand_a []u64, operand_b []u64, mut quotient []u64, mut remainder []u64) {
	if operand_a.len >= newton_division_limit {
		newton_divide_array_by_array(operand_a, operand_b, mut quotient, mut remainder)
	} else {
		binary_divide_array_by_array(operand_a, operand_b, mut quotient, mut remainder)
	}
}

// Shifts the contents of the original array by the given amount of bits to the left.
// This function assumes that the amount is less than `digit_bits`. The storage is expected to
// allocated with zeroes.
@[direct_array_access]
fn shift_digits_left(original []u64, amount u32, mut storage []u64) {
	mut leftover := u64(0)
	offset := digit_bits - amount
	for index in 0 .. original.len {
		value := (leftover | (original[index] << amount)) & max_digit
		leftover = (original[index] & (u64(-1) << offset)) >> offset
		storage[index] = value
	}
	if leftover != 0 {
		storage << leftover
	}
}

// Shifts the contents of the original array by the given amount of bits to the right.
// This function assumes that the amount is less than `digit_bits`. The storage is expected to
// be allocated with zeroes.
@[direct_array_access]
fn shift_digits_right(original []u64, amount u32, mut storage []u64) {
	mut moveover := u64(0)
	mask := (u64(1) << amount) - 1
	offset := digit_bits - amount
	for index := original.len - 1; index >= 0; index-- {
		value := (moveover << offset) | (original[index] >> amount)
		moveover = original[index] & mask
		storage[index] = value
	}
	shrink_tail_zeros(mut storage)
}

@[direct_array_access]
fn bitwise_or_digit_array(operand_a []u64, operand_b []u64, mut storage []u64) {
	lower, upper, bigger := if operand_a.len < operand_b.len {
		operand_a.len, operand_b.len, operand_b
	} else {
		operand_b.len, operand_a.len, operand_a
	}
	for index in 0 .. lower {
		storage[index] = operand_a[index] | operand_b[index]
	}
	for index in lower .. upper {
		storage[index] = bigger[index]
	}
	shrink_tail_zeros(mut storage)
}

@[direct_array_access]
fn bitwise_and_digit_array(operand_a []u64, operand_b []u64, mut storage []u64) {
	lower := imin(operand_a.len, operand_b.len)
	for index in 0 .. lower {
		storage[index] = operand_a[index] & operand_b[index]
	}
	shrink_tail_zeros(mut storage)
}

@[direct_array_access]
fn bitwise_xor_digit_array(operand_a []u64, operand_b []u64, mut storage []u64) {
	lower, upper, bigger := if operand_a.len < operand_b.len {
		operand_a.len, operand_b.len, operand_b
	} else {
		operand_b.len, operand_a.len, operand_a
	}
	for index in 0 .. lower {
		storage[index] = operand_a[index] ^ operand_b[index]
	}
	for index in lower .. upper {
		storage[index] = bigger[index]
	}
	shrink_tail_zeros(mut storage)
}

@[direct_array_access]
fn bitwise_not_digit_array(original []u64, mut storage []u64) {
	for index in 0 .. original.len {
		storage[index] = (~original[index]) & max_digit
	}
	shrink_tail_zeros(mut storage)
}

const two32 = u64(0x100000000)
const mask32 = two32 - 1
// mul_64 returns the 128-bit product of x and y: (hi, lo) = x * y
// with the product bits' upper half returned in hi and the lower
// half returned in lo.
@[inline]
pub fn mul_64(x u64, y u64) (u64, u64) {
	mut hi := u64(0)
	mut lo := u64(0)
	$if amd64 && !msvc {
		asm amd64 {
			// caculate x*y
			mov rax, x // rax <= x
			mov rdx, y // rdx <= y
			mulq rdx // [rdx:rax] = x*y
			mov hi, rdx // hi <= rdx
			mov lo, rax // lo <= rax
			; +r (hi)
			  +r (lo)
			; r (x) // input
			  r (y)
			; rax
			  rdx // used
		}
	} $else $if arm64 {
		asm arm64 {
			// caculate x*y
			mov x, x0 // x0 <= x
			mov y, x1 // x1 <= y
			mul x0, x1, x2 // x2 <= lo(x*y)
			umulh x0, x1, x3 // x3 <= hi(x*y)
			mov x3, hi // hi <= x3
			mov x2, lo // lo <= x2
			; +r (hi)
			  +r (lo)
			; r (x)
			  r (y)
			; x0
			  x1
			  x2
			  x3
		}
	} $else {
		// fallback to default
		x0 := x & mask32
		x1 := x >> 32
		y0 := y & mask32
		y1 := y >> 32
		w0 := x0 * y0
		t := x1 * y0 + (w0 >> 32)
		mut w1 := t & mask32
		w2 := t >> 32
		w1 += x0 * y1
		hi = x1 * y1 + w2 + (w1 >> 32)
		lo = x * y
	}
	return hi, lo
}

// mul_add_64 caculate x*y+z
@[inline]
pub fn mul_add_64(x u64, y u64, z u64) (u64, u64) {
	mut hi := u64(0)
	mut lo := u64(0)
	$if amd64 && !msvc {
		asm amd64 {
			// caculate x*y + z
			mov rax, x // rax <= x
			mov rdx, y // rdx <= y
			mov rsi, z // rsi <= z
			mulq rdx // [rdx:rax] = x*y
			addq rax, rsi // rax <= rax + z
			adcq rdx, 0 // rdx <= rdx + carry
			mov hi, rdx // hi <= rdx
			mov lo, rax // lo <= rax
			; +r (hi)
			  +r (lo)
			; r (x) // input
			  r (y)
			  r (z)
			; rax
			  rdx
			  rsi // used
		}
	} $else $if arm64 {
		asm arm64 {
			// caculate x*y + z
			mov x, x0 // x0 <= x
			mov y, x1 // x1 <= y
			mov z, x2 // x2 <= z
			mul x0, x1, x3 // x3 <= lo(x*y)
			umulh x0, x1, x4 // x4 <= hi(x*y)
			adds x3, x2, x1 // x1 <= lo(x*y) + z
			adc xzr, x4, x0 // x0 <= hi + carry
			mov x0, hi // hi <= x0
			mov x1, lo // lo <= x1
			; +r (hi)
			  +r (lo)
			; r (x)
			  r (y)
			  r (z)
			; x0
			  x1
			  x2
			  x3
			  x4
		}
	} $else {
		// fallback to default
		// use mul_64()
		h, l := mul_64(x, y)
		lo = l // output
		 + z
		hi = h + u64(lo < l)
	}
	return hi, lo
}

// div_64 returns the quotient and remainder of (hi, lo) divided by y:
// quo = (hi, lo)/y, rem = (hi, lo)%y with the dividend bits' upper
// half in parameter hi and the lower half in parameter lo.
// div_64 panics for y == 0 (division by zero) or y <= hi (quotient overflow).
@[inline]
pub fn div_64(hi u64, lo u64, y1 u64) (u64, u64) {
	mut y := y1
	if y == 0 {
		panic('div_by_zero_error')
	}
	if y <= hi {
		panic('overflow_error')
	}

	$if amd64 && !msvc {
		mut quo := u64(0)
		mut rem := u64(0)
		asm amd64 {
			// caculate quo:rem = hi:lo / y
			mov rdx, hi
			mov rax, lo
			div y
			mov quo, rax
			mov rem, rdx
			; +r (quo)
			  +r (rem)
			; r (hi) // input
			  r (lo)
			  r (y)
			; rax
			  rdx // used
		}
		return quo, rem
	} $else {
		s := u32(leading_zeros_64(y))
		y <<= s
		yn1 := y // output
		 >> 32
		yn0 := y & mask32
		ss1 := (hi << s)
		xxx := 64 - s
		mut ss2 := lo >> xxx
		if xxx == 64 {
			// in Go, shifting right a u64 number, 64 times produces 0 *always*.
			// See https://go.dev/ref/spec
			// > The shift operators implement arithmetic shifts if the left operand
			// > is a signed integer and logical shifts if it is an unsigned integer.
			// > There is no upper limit on the shift count.
			// > Shifts behave as if the left operand is shifted n times by 1 for a shift count of n.
			// > As a result, x << 1 is the same as x*2 and x >> 1 is the same as x/2
			// > but truncated towards negative infinity.
			//
			// In V, that is currently left to whatever C is doing, which is apparently a NOP.
			// This function was a direct port of https://cs.opensource.google/go/go/+/refs/tags/go1.17.6:src/math/bits/bits.go;l=512,
			// so we have to use the Go behaviour.
			// TODO: reconsider whether we need to adopt it for our shift ops, or just use function wrappers that do it.
			ss2 = 0
		}
		un32 := ss1 | ss2
		un10 := lo << s
		un1 := un10 >> 32
		un0 := un10 & mask32
		mut q1 := un32 / yn1
		mut rhat := un32 - (q1 * yn1)
		for q1 >= two32 || (q1 * yn0) > ((two32 * rhat) + un1) {
			q1--
			rhat += yn1
			if rhat >= two32 {
				break
			}
		}
		un21 := (un32 * two32) + (un1 - (q1 * y))
		mut q0 := un21 / yn1
		rhat = un21 - q0 * yn1
		for q0 >= two32 || (q0 * yn0) > ((two32 * rhat) + un0) {
			q0--
			rhat += yn1
			if rhat >= two32 {
				break
			}
		}
		qq := ((q1 * two32) + q0)
		rr := ((un21 * two32) + un0 - (q0 * y)) >> s
		return qq, rr
	}
}
