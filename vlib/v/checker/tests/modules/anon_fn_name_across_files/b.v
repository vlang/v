module main

// This file intentionally pads its content so that the anonymous function below
// starts at the same byte offset as the one in a.v. Before the fix in
// get_anon_fn_name() (which now includes pos.file_idx in the C symbol name),
// both would produce the same name and trigger a linker error.
fn get_fn_b() fn () int {
	return fn () int {
		return 2
	}
}
