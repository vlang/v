fn query_short_syntax_struct_arg[T](query_config T) (int, string) {
	mut a := 0
	mut b := ''
	$for field in T.fields {
		$if field.typ is int {
			a = query_config.$(field.name)
		} $else $if field.typ is string {
			b = query_config.$(field.name)
		}
	}
	return a, b
}

fn test_generic_fn_short_syntax_struct_param() {
	a, b := query_short_syntax_struct_arg(a: 1, b: 'hello')
	assert a == 1
	assert b == 'hello'
}
