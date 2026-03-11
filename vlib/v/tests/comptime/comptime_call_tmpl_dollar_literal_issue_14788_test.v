fn test_tmpl_bare_dollar_interpolation_is_literal() {
	name := 'Peter'
	age := 25
	numbers := [1, 2, 3]
	out := $tmpl('comptime_call_tmpl_dollar_literal_issue_14788.tpl')
	assert out.trim_space() == 'name: \$name\n\nage: \$age\n\nnumbers: \$numbers\n\n  \$number\n  \$number\n  \$number'
}
