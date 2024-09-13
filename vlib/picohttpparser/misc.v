module picohttpparser

// vfmt off
const g_digits_lut = [
	`0`, `0`, `0`, `1`, `0`, `2`, `0`, `3`, `0`, `4`, `0`, `5`, `0`, `6`, `0`, `7`, `0`, `8`, `0`, `9`,
	`1`, `0`, `1`, `1`, `1`, `2`, `1`, `3`, `1`, `4`, `1`, `5`, `1`, `6`, `1`, `7`, `1`, `8`, `1`, `9`,
	`2`, `0`, `2`, `1`, `2`, `2`, `2`, `3`, `2`, `4`, `2`, `5`, `2`, `6`, `2`, `7`, `2`, `8`, `2`, `9`,
	`3`, `0`, `3`, `1`, `3`, `2`, `3`, `3`, `3`, `4`, `3`, `5`, `3`, `6`, `3`, `7`, `3`, `8`, `3`, `9`,
	`4`, `0`, `4`, `1`, `4`, `2`, `4`, `3`, `4`, `4`, `4`, `5`, `4`, `6`, `4`, `7`, `4`, `8`, `4`, `9`,
	`5`, `0`, `5`, `1`, `5`, `2`, `5`, `3`, `5`, `4`, `5`, `5`, `5`, `6`, `5`, `7`, `5`, `8`, `5`, `9`,
	`6`, `0`, `6`, `1`, `6`, `2`, `6`, `3`, `6`, `4`, `6`, `5`, `6`, `6`, `6`, `7`, `6`, `8`, `6`, `9`,
	`7`, `0`, `7`, `1`, `7`, `2`, `7`, `3`, `7`, `4`, `7`, `5`, `7`, `6`, `7`, `7`, `7`, `8`, `7`, `9`,
	`8`, `0`, `8`, `1`, `8`, `2`, `8`, `3`, `8`, `4`, `8`, `5`, `8`, `6`, `8`, `7`, `8`, `8`, `8`, `9`,
	`9`, `0`, `9`, `1`, `9`, `2`, `9`, `3`, `9`, `4`, `9`, `5`, `9`, `6`, `9`, `7`, `9`, `8`, `9`, `9`,
]!
// vfmt on

// u64toa converts `value` to an ASCII string and stores it at `buf_start`.
// It returns the length of the ASCII string (branch lookup table implementation).
// Note: `buf_start` should point to a memory buffer, that is *at least* 9 bytes long.
@[direct_array_access; unsafe]
pub fn u64toa(buf_start &u8, value u64) !int {
	mut buf := unsafe { buf_start }
	// set maximum length to 100MB
	if value >= 100_000_000 {
		return error('Maximum size of 100MB exceeded!')
	}

	v := u32(value)
	if v < 10_000 {
		d1 := u32((v / 100) << 1)
		d2 := u32((v % 100) << 1)
		unsafe {
			if v >= 1000 {
				*buf++ = g_digits_lut[d1]
			}
			if v >= 100 {
				*buf++ = g_digits_lut[d1 + 1]
			}
			if v >= 10 {
				*buf++ = g_digits_lut[d2]
			}
			*buf++ = g_digits_lut[d2 + 1]
		}
	} else {
		b := v / 10_000
		c := v % 10_000

		d1 := u32((b / 100) << 1)
		d2 := u32((b % 100) << 1)

		d3 := u32((c / 100) << 1)
		d4 := u32((c % 100) << 1)

		unsafe {
			if value >= 10_000_000 {
				*buf++ = g_digits_lut[d1]
			}
			if value >= 1_000_000 {
				*buf++ = g_digits_lut[d1 + 1]
			}
			if value >= 100_000 {
				*buf++ = g_digits_lut[d2]
			}
			*buf++ = g_digits_lut[d2 + 1]

			*buf++ = g_digits_lut[d3]
			*buf++ = g_digits_lut[d3 + 1]
			*buf++ = g_digits_lut[d4]
			*buf++ = g_digits_lut[d4 + 1]
		}
	}

	return unsafe { buf - buf_start }
}
