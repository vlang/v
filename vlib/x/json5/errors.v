module json5

// ParseError describes a lexical or syntactic problem in the input.
pub struct ParseError {
	Error
pub:
	message string
	line    int
	col     int
}

// msg formats a ParseError for `IError.msg()`.
pub fn (e &ParseError) msg() string {
	return 'json5: ${e.line}:${e.col}: ${e.message}'
}

// TypeError describes a value that does not fit the requested V type.
pub struct TypeError {
	Error
pub:
	message string
	path    string // dotted path of the offending value, empty at the root
}

// msg formats a TypeError for `IError.msg()`.
pub fn (e &TypeError) msg() string {
	if e.path == '' {
		return 'json5: ${e.message}'
	}
	return 'json5: at `${e.path}`: ${e.message}'
}

// NameError describes a missing object key.
pub struct NameError {
	Error
pub:
	key string
}

// msg formats a NameError for `IError.msg()`.
pub fn (e &NameError) msg() string {
	return 'json5: no key `${e.key}`'
}

// EnumError describes a string that does not name a variant of the target enum.
pub struct EnumError {
	Error
pub:
	enum_name string
	value     string
}

// msg formats an EnumError for `IError.msg()`.
pub fn (e &EnumError) msg() string {
	return 'json5: `${e.value}` is not a variant of enum `${e.enum_name}`'
}

fn syntax_error(message string, line int, col int) &ParseError {
	return &ParseError{
		message: message
		line:    line
		col:     col
	}
}

fn type_error(value Any, expected string) &TypeError {
	return &TypeError{
		message: 'expected ${expected}, found ${describe(value)}'
	}
}

// describe names the JSON5 kind of `value` for error messages.
fn describe(value Any) string {
	return match value {
		Null { '`null`' }
		bool { 'a boolean' }
		Number, Raw { 'the number `${value.text}`' }
		f64 { 'a number' }
		string { 'a string' }
		[]Any { 'an array' }
		map[string]Any { 'an object' }
	}
}
