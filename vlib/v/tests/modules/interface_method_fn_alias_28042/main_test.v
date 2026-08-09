module main

import callback_api

struct App {}

// connect implements callback_api.Initializer for this regression test.
pub fn (mut app App) connect(_ string, _ string, _ callback_api.Handler) {}

// https://github.com/vlang/v/issues/28042
fn test_interface_method_with_imported_function_type_alias_parameter() {
	mut app := App{}
	callback_api.initialize(mut app)
}
