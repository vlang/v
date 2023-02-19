module builtin

/* const (
	// digit pairs in reverse order
	digit_pairs = '00102030405060708090011121314151617181910212223242526272829203132333435363738393041424344454647484940515253545556575859506162636465666768696071727374757677787970818283848586878889809192939495969798999'
)

// This implementation is the quickest with gcc -O2
// str_l returns the string representation of the integer nn with max chars.
[direct_array_access; inline]
fn (nn int) str_l(max int) string {
	unsafe {
		mut n := i64(nn)
		mut d := 0
		if n == 0 {
			return '0'
		}

		mut is_neg := false
		if n < 0 {
			n = -n
			is_neg = true
		}
		mut index := max
		mut buf := malloc(max + 1)
		buf[index] = 0
		index--

		for n > 0 {
			n1 := int(n / 100)
			// calculate the digit_pairs start index
			d = int(u32(int(n) - (n1 * 100)) << 1)
			n = n1
			buf[index] = digit_pairs.str[d]
			index--
			d++
			buf[index] = digit_pairs.str[d]
			index--
		}
		index++
		// remove head zero
		if d < 20 {
			index++
		}
		// Prepend - if it's negative
		if is_neg {
			index--
			buf[index] = `-`
		}
		diff := max - index
		vmemmove(buf, voidptr(buf + index), diff + 1)
		/*
		// === manual memory move for bare metal ===
		mut c:= 0
		for c < diff {
			buf[c] = buf[c+index]
			c++
		}
		buf[c] = 0
		*/
		return tos(buf, diff)

		// return tos(memdup(&buf[0] + index, (max - index)), (max - index))
	}
}

// str returns the value of the `int` as a `string`.
// Example: assert int(-2020).str() == '-2020'
pub fn (n int) str() string {
	return n.str_l(12)
} */