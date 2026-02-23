module main

// Both a.v and b.v define an anonymous function with the same signature (fn() int)
// at the same byte offset from the start of their respective files.
// Before the fix in get_anon_fn_name(), both would produce the same C symbol name,
// causing a linker collision or silent wrong behaviour.
fn get_fn_a() fn () int {
	return fn () int {
		return 1
	}
}
