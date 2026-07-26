type Issue27921Fn = fn ()

type Issue27921ResultFn = fn () !int

fn issue27921_plugin_main() {}

fn issue27921_fallible_plugin_main() !int {
	return 21
}

fn test_inline_function_alias_cast_call() {
	sym := voidptr(issue27921_plugin_main)
	// vfmt off
	(Issue27921Fn(sym))()
	// vfmt on
}

fn test_inline_fallible_function_alias_cast_call() {
	sym := voidptr(issue27921_fallible_plugin_main)
	// vfmt off
	res := (Issue27921ResultFn(sym))() or { panic(err) }
	// vfmt on
	assert res == 21
}
