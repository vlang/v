module cleanc

import v2.ast

fn test_c_string_literal_content_to_c_single_line() {
	out := c_string_literal_content_to_c('hello')
	assert out == '"hello"'
}

fn test_c_string_literal_content_to_c_multiline() {
	out := c_string_literal_content_to_c('hello\nworld')
	assert out == '"hello\\n"\n"world"'
}

fn test_c_string_literal_content_to_c_trailing_newline() {
	out := c_string_literal_content_to_c('hello\n')
	assert out == '"hello\\n"\n""'
}

fn test_c_string_literal_content_to_c_escapes_quote() {
	out := c_string_literal_content_to_c('say "hello"')
	assert out == '"say \\"hello\\""'
}

fn test_c_string_literal_content_to_c_preserves_percent_placeholders() {
	out := c_string_literal_content_to_c('"%s"')
	assert out == '"\\"%s\\""'
}

fn test_c_string_literal_content_to_c_splits_hex_escape_before_hex_digit() {
	out := c_string_literal_content_to_c(r'\x0c8')
	assert out == '"\\x0c""8"'
}

fn test_struct_generic_params_need_bindings_returns_false_for_lifetime_only_params() {
	params := [
		ast.Expr(ast.LifetimeExpr{
			name: 'a'
		}),
		ast.Expr(ast.LifetimeExpr{
			name: 'b'
		}),
	]
	assert !struct_generic_params_need_bindings(params)
}

fn test_struct_generic_params_need_bindings_returns_true_for_runtime_generic_params() {
	params := [
		ast.Expr(ast.LifetimeExpr{
			name: 'a'
		}),
		ast.Expr(ast.Ident{
			name: 'T'
		}),
	]
	assert struct_generic_params_need_bindings(params)
}
