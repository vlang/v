module main

const x = c(c(c('123') or { '456' }) or { '444' }) or { 'xx' }

fn a(s string) ?string {
	if s == 'b' {
		return 'a'
	} else {
		return none
	}
}

fn b(s string) ?string {
	if s == 'a' {
		return 'b'
	} else {
		return none
	}
}

fn c(s string) !string {
	return a(s) or { b(s) or { 'c' } }
}

fn test_main() {
	assert x == 'c'
}
