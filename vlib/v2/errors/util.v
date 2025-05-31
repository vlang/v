module errors

import v2.token
import term

pub enum Kind {
	warning
	notice
	error
}

pub fn (e Kind) str() string {
	return match e {
		.warning { 'warning' }
		.notice { 'notice' }
		.error { 'error' }
	}
}

pub fn (e Kind) color(s string) string {
	return match e {
		.warning { term.yellow(s) }
		.notice { term.blue(s) }
		.error { term.red(s) }
	}
}

pub fn error(msg string, details string, kind Kind, pos token.Position) {
	eprintln(pos.str() + ' -> ' + term.bold(kind.color(kind.str())) + ': ' + msg)
	if details.len > 0 {
		eprintln(details)
	}
}
