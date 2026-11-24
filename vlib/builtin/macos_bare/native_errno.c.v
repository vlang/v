module builtin

$if freestanding && !native && amd64 && !v_bare_no_startup {
	// Native Darwin errno uses C int and remains separate from normalized BF errors.
	@[export: '__error'; markused]
	@[no_profile; no_trace]
	fn bare_macos_errno_pointer() &i32 {
		return &bf_native_errno_cell
	}
}
