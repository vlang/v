module builtin

// Portable C (`-os cross`, which vc/v.c is) is compiled later, for a target of
// either width. A `$if` expression, such as the one that defines `min_int` for
// other builds, is resolved for the machine that generates the C. A statement-level
// `$if x64` stays in that C as a preprocessor `#if`, so these functions give the
// limits of the C compiler's target.

// portable_min_int returns the smallest `int` of the target that compiles the C.
fn portable_min_int() int {
	$if x64 {
		return int(min_i64)
	} $else {
		return int(min_i32)
	}
}

// portable_max_int returns the largest `int` of the target that compiles the C.
fn portable_max_int() int {
	$if x64 {
		return int(max_i64)
	} $else {
		return int(max_i32)
	}
}
