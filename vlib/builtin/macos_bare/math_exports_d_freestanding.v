module builtin

// The compiler selects the portable math kernels for syscall-only C builds.
// These libc ABI names are also used by builtin float formatting and **.
$if freestanding && amd64 && !native {
	import math

	@[export: 'fabs'; markused]
	fn bf_fabs(x f64) f64 { return math.copysign(x, 1.0) }

	@[export: 'floor'; markused]
	fn bf_floor(x f64) f64 { return math.floor(x) }

	@[export: 'pow'; markused]
	fn bf_pow(x f64, y f64) f64 { return math.pow(x, y) }
}
