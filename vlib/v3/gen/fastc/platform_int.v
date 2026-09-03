@[has_globals]
module fastc

// fastc_platform_int_c_type is the C spelling FastC emits for V's
// platform-width `int`. Keep the state in its own module file so FastC's
// generation entry point can follow upstream refactors without carrying a
// conflict-prone module-header edit. It defaults to the 64-bit spelling so the
// generator is usable before a target is configured; generation calls
// `fastc_set_platform_int_bits` with the target pointer width first.
__global fastc_platform_int_c_type = 'i64'

// fastc_set_platform_int_bits selects the C spelling for the platform `int`
// from the target pointer width. Generation calls this once, before any worker
// reads the spelling, so every emitted declaration agrees on the width. Taking
// the width from the target rather than the compiler host keeps cross builds
// correct, and matches `types.set_platform_int_bits` in the C backend.
fn fastc_set_platform_int_bits(bits int) {
	fastc_platform_int_c_type = if bits == 32 { 'i32' } else { 'i64' }
}
