module builtin

$if linux && (amd64 || arm64) && !v_bare_no_startup ? {
	// The optional native quadmath dependency owns binary128 conversion. This
	// allocation-free adapter validates strfrom's grammar without narrowing its
	// value through V's f64 ABI. The native caller provides a 16-byte format buffer.
	@[export: 'v_float128_format'; markused]
	fn bare_float128_format(format uint, output uint) i32 {
		if unsafe { *(&u8(format)) } != `%` {
			bare_runtime_fail('*** invalid strfrom format ***\n')
		}
		mut input := uint(1)
		mut precision := uint(0)
		mut has_precision := false
		mut overflow := false
		if unsafe { (&u8(format))[input] } == `.` {
			has_precision = true
			input++
			for {
				digit := unsafe { (&u8(format))[input] }
				if digit < `0` || digit > `9` { break }
				if precision > (uint(2147483647) - uint(digit - `0`)) / 10 {
					overflow = true
				} else if !overflow {
					precision = precision * 10 + uint(digit - `0`)
				}
				input++
			}
		}
		specifier := unsafe { (&u8(format))[input] }
		if specifier !in [`a`, `A`, `e`, `E`, `f`, `F`, `g`, `G`] || unsafe { (&u8(format))[input + 1] } != 0 {
			bare_runtime_fail('*** invalid strfrom format ***\n')
		}
		if overflow {
			bf_native_errno_cell = 75
			return -1
		}
		mut position := uint(0)
		// The format is at most "%.2147483647Qf" plus NUL; every access
		// stays within the caller's fixed native bridge buffer.
		unsafe { (&u8(output))[position] = `%` }
		position++
		if has_precision {
			unsafe { (&u8(output))[position] = `.` }
			position++
			mut digits := [10]u8{}
			mut count := 0
			for {
				digits[count] = u8(precision % 10) + `0`
				count++
				precision /= 10
				if precision == 0 { break }
			}
			for count > 0 {
				count--
				unsafe { (&u8(output))[position] = digits[count] }
				position++
			}
		}
		unsafe {
			(&u8(output))[position] = `Q`
			(&u8(output))[position + 1] = specifier
			(&u8(output))[position + 2] = 0
		}
		return 0
	}
}
