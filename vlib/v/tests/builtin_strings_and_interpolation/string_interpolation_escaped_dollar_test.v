// An escaped `${` in a string literal is plain text, even when the literal sits
// next to or inside a real interpolation. The names below are declared on purpose:
// expanding the escaped text would print their values instead of failing to build.

fn identity(s string) string {
	return s
}

fn test_escaped_dollar_after_interpolation_stays_literal() {
	x := 'X'
	assert '${x} \${x}' == 'X ' + r'${x}'
	assert '\${x} ${x} \${x}' == r'${x} X ${x}'
	// vfmt off
	assert '${x} \x24{x}' == 'X ' + r'${x}'
	assert '${x} \044{x}' == 'X ' + r'${x}'
	// vfmt on
}

fn test_escaped_undeclared_name_after_interpolation_compiles() {
	name := 'form.vml'
	src := "eprintln('Could not load ${name}: \${err}')\nprintln('event: \${event}')"
	assert src == "eprintln('Could not load form.vml: " + r"${err}')" + "\nprintln('event: " +
		r"${event}')"
}

fn test_literal_inside_interpolation_expr_is_not_reparsed() {
	x := 'X'
	tags := ['a', 'b']
	assert '${identity('\${x}')} ${x}' == r'${x} X'
	assert '${tags.map('--\${x}').join(',')} ${x}' == r'--${x},--${x} X'
	assert '${tags.map('--\${it}').join(',')}' == r'--${it},--${it}'
	// vfmt off
	assert '${r'${x}'} ${x}' == r'${x} X'
	// vfmt on
}

fn test_real_nested_interpolation_still_expands() {
	x := 'X'
	tags := ['a', 'b']
	assert '${tags.map('--${it}').join(',')}' == '--a,--b'
	assert 'L ${'a ${x}'}' == 'L a X'
	assert 'outer ${if x.len > 0 { 'inner ${x}' } else { 'empty' }} done' == 'outer inner X done'
}
