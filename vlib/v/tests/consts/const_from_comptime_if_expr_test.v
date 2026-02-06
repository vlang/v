import term

const color = $if windows {
	term.bright_cyan('WINDOWS')
} $else {
	term.bright_green('UNIX')
}

fn test_const_from_comptime_if_expr() {
	salutation := 'hello'
	val := match salutation {
		'hello' { color + ' some text' }
		'goodbyte' { color + ' some other text' }
		else { 'invalid' }
	}
	println(val)
	assert true
}
