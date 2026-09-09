struct TmplBraceBlockUser {
	name string
	age  int
}

fn render_tmpl_for_brace_block(users []TmplBraceBlockUser) string {
	return $tmpl('brace_for_block.tmpl')
}

fn test_tmpl_for_brace_block() {
	users := [
		TmplBraceBlockUser{
			name: 'John Doe'
			age:  27
		},
		TmplBraceBlockUser{
			name: 'Jane Doe'
			age:  16
		},
	]
	assert render_tmpl_for_brace_block(users).trim_space() == 'John Doe is 27 years old
Jane Doe is 16 years old'
}
