module errors

import v3.ansi
import v3.token

// Kind lists kind values used by errors.
pub enum Kind {
	warning
	notice
	error
}

// str returns the string form for Kind.
pub fn (e Kind) str() string {
	return match e {
		.warning { 'warning' }
		.notice { 'notice' }
		.error { 'error' }
	}
}

// color supports color handling for Kind.
pub fn (e Kind) color(s string) string {
	return match e {
		.warning { ansi.yellow(s) }
		.notice { ansi.blue(s) }
		.error { ansi.red(s) }
	}
}

// error supports error handling for errors.
pub fn error(msg string, details string, kind Kind, pos token.Position) {
	eprintln(pos.str() + ' -> ' + ansi.bold(kind.color(kind.str())) + ': ' + msg)
	if details.len > 0 {
		eprintln(details)
	}
}
