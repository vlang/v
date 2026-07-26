type Issue27921Fn = fn ()

fn issue27921_plugin_main() {}

fn test_inline_function_alias_cast_call() {
	sym := voidptr(issue27921_plugin_main)
	// vfmt off
	(Issue27921Fn(sym))()
	// vfmt on
}
