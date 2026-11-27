module big

const decimal_pairs = '00010203040506070809101112131415161718192021222324252627282930313233343536373839404142434445464748495051525354555657585960616263646566676869707172737475767778798081828384858687888990919293949596979899'
const decimal_conversion_limit = 16

fn (integer Integer) decimal_str() string {
	// Ignore redundant high zero limbs without changing the input's digits or sign.
	mut len := integer.digits.len
	for len > 0 && integer.digits[len - 1] == 0 {
		len--
	}
	value := Integer{ digits: integer.digits[..len], signum: integer.signum }
	// 30103 / 100000 is an upper bound for log10(2). Round up to whole chunks.
	size := int(i64(value.bit_len()) * 30103 / 100000) + 1
	mut buf := []u8{len: (size + 8) / 9 * 9 + 1, init: `0`}
	mut powers := []DecimalPower{}
	if value.digits.len >= decimal_conversion_limit {
		powers = decimal_powers(size)
	}
	mut start := decimal_write(value.digits, powers, powers.len - 1, mut buf, buf.len)
	if integer.signum == -1 {
		start--
		buf[start] = `-`
	}
	return buf[start..].bytestr()
}

fn decimal_write(digits []u64, powers []DecimalPower, level int, mut buf []u8, end int) int {
	if digits.len == 0 {
		return end - 1
	}
	if digits.len < decimal_conversion_limit || level < 0 {
		return decimal_write_small(digits, mut buf, end)
	}
	power := powers[level]
	if compare_digit_array(digits, power.value.digits) < 0 {
		return decimal_write(digits, powers, level - 1, mut buf, end)
	}
	value := Integer{ digits: digits, signum: 1 }
	q, r := value.div_mod_internal(power.value)
	// The buffer starts with zeroes, including the fixed-width remainder's padding.
	decimal_write(r.digits, powers, level - 1, mut buf, end)
	return decimal_write(q.digits, powers, level - 1, mut buf, end - power.width)
}

@[direct_array_access]
fn decimal_write_small(digits []u64, mut buf []u8, end int) int {
	mut q := digits.clone()
	mut pos := end
	for q.len > 1 {
		mut rem := u64(0)
		for i := q.len - 1; i >= 0; i-- {
			// Each intermediate fits in 60 bits; no double-width division is needed.
			hi := (rem << 30) | (q[i] >> 30)
			qh := hi / 1_000_000_000
			lo := ((hi - qh * 1_000_000_000) << 30) | (q[i] & 0x3fffffff)
			ql := lo / 1_000_000_000
			rem = lo - ql * 1_000_000_000
			q[i] = (qh << 30) | ql
		}
		shrink_tail_zeros(mut q)
		for _ in 0 .. 4 {
			pair := int(rem % 100) * 2
			pos -= 2
			buf[pos] = decimal_pairs[pair]
			buf[pos + 1] = decimal_pairs[pair + 1]
			rem /= 100
		}
		pos--
		buf[pos] = u8(rem) + `0`
	}
	mut last := q[0]
	for last >= 100 {
		pair := int(last % 100) * 2
		pos -= 2
		buf[pos] = decimal_pairs[pair]
		buf[pos + 1] = decimal_pairs[pair + 1]
		last /= 100
	}
	if last >= 10 {
		pos -= 2
		buf[pos] = decimal_pairs[int(last) * 2]
		buf[pos + 1] = decimal_pairs[int(last) * 2 + 1]
	} else {
		pos--
		buf[pos] = u8(last) + `0`
	}
	return pos
}
