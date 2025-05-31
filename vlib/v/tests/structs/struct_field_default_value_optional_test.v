import regex

struct RegexCache {
mut:
	tag_script_start regex.RE = regex.regex_opt(r'^script.*>') or { panic(err) }
}

pub struct Tokenizer {
mut:
	regex_cache RegexCache = RegexCache{}
}

fn new_parser() &Parser {
	mut parser := &Parser{}
	return parser
}

pub struct Parser {
mut:
	tnzr Tokenizer
}

fn test_struct_field_default_value_optional() {
	p := new_parser()
	println(p)
	assert true
}
