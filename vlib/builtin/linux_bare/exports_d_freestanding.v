module builtin

import math

// exports_d_freestanding.v: the libc-named symbols the freestanding runtime
// provides to the link. A libc-linked `-syscalls` build must not carry them: the
// program would override libc's errno accessor and math functions.
// The math module's portable source family is selected for freestanding Linux,
// so these ABI exports share its kernels without reaching hosted libm.
$if freestanding && !i386 {
	@[export: '__errno_location']
	@[markused]
	fn bf_errno_export() &i32 {
		return &bf_native_errno_cell
	}

	@[export: 'fabs']
	@[markused]
	fn bf_fabs(x f64) f64 {
		// Clear the sign of every IEEE value without changing a NaN payload.
		return math.copysign(x, 1)
	}

	@[export: 'floor']
	@[markused]
	fn bf_floor(x f64) f64 {
		return math.floor(x)
	}

	@[export: 'trunc']
	@[markused]
	fn bf_trunc(x f64) f64 {
		bits := math.f64_bits(x)
		exponent := int((bits >> 52) & 0x7ff) - 1023
		if exponent < 0 { return math.f64_from_bits(bits & 0x8000000000000000) }
		if exponent == 1024 { return x + x } // Quiet a signaling NaN with the native exception.
		if exponent >= 52 { return x }
		return math.f64_from_bits(bits & ~(u64(0xfffffffffffff) >> exponent))
	}

	@[export: 'pow']
	@[markused]
	fn bf_pow(x f64, y f64) f64 {
		return math.pow(x, y)
	}
}
