fn describe(target voidptr, format &u8, args ...voidptr) string {
	return '${usize(target)}:${unsafe { cstring_to_vstring(format) }}:${args.len}'
}

// An argument before an `unsafe { if ... }` argument is evaluated into a temporary
// first (source order); that temporary must stay declared before the call.
fn test_argument_before_an_unsafe_if_value() {
	target := voidptr(usize(7))
	mut results := []string{}
	for cond in [true, false] {
		if !cond {
			results << describe(target, c'a', voidptr(if cond { c'x' } else { c'y' }))
		} else {
			results << describe(target, unsafe { if cond { c'x' } else { c'y' } }, voidptr(0))
		}
	}
	assert results == ['7:x:1', '7:a:1']
}
