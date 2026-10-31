module main

// max_field_number is the largest field number a protobuf message can carry. It
// bounds an open-ended `reserved N to max` range.
pub const max_field_number = 536870911

// Parser turns .proto text into a File. It is recursive descent over the
// grammar's top-level statements, which is all a proto3 schema needs: unlike V
// there is no expression language, and the only nesting is message inside
// message.
pub struct Parser {
pub mut:
	// scanner is held by value rather than by pointer. V3 mishandles mutation
	// through a chain of pointers, and a tool is not worth fighting over: one
	// copy per parse costs nothing.
	scanner Scanner
	// cur is the current token, and `ahead` the one after it. Holding a real
	// lookahead is what lets the parser decide on a statement's shape --
	// `repeated` and `map` both need a second token -- without consuming
	// anything.
	cur       Token
	ahead     Token
	has_ahead bool
	// path is recorded on the File for diagnostics and for the module name.
	path string
	// errors is every diagnostic seen so far, so a single run can report
	// several problems instead of stopping at the first.
	errors []string
}

// new_parser returns a Parser over `text`, reporting diagnostics against `path`.
pub fn new_parser(path string, text string) !&Parser {
	mut p := &Parser{
		scanner: Scanner{
			text: text
		}
		path:    path
	}
	p.cur = p.scanner.next()!
	return p
}

// parse reads the whole file.
pub fn (mut p Parser) parse() !File {
	mut file := File{
		path:     p.path
		syntax:   ''
		comments: p.scanner.take_comments()
	}
	for p.cur.kind != .eof {
		if p.is_cur(';') {
			p.advance() // a stray semicolon
			continue
		}
		comments := p.scanner.take_comments()

		if p.cur.text == 'syntax' {
			p.parse_syntax(mut file)!
		} else if p.cur.text == 'package' {
			p.parse_package(mut file)!
		} else if p.cur.text == 'import' {
			p.parse_import(mut file)!
		} else if p.cur.text == 'option' {
			p.skip_option()!
		} else if p.cur.text == 'message' {
			msg := p.parse_message(comments)!
			file.messages << msg
		} else if p.cur.text == 'enum' {
			e := p.parse_enum(comments)!
			file.enums << e
		} else if p.cur.text == 'service' {
			svc := p.parse_service(comments)!
			file.services << svc
		} else {
			p.fail('unexpected `${p.cur.text}` at top level')
			p.advance()
		}
	}
	if file.syntax != '' && file.syntax != 'proto3' {
		p.fail('only proto3 is supported, found syntax = "${file.syntax}"')
	}
	file.package_parts = if file.package == '' {
		[]string{}
	} else {
		file.package.split('.')
	}
	return file
}

// is_cur reports whether the current token is the given punctuation.
pub fn (p &Parser) is_cur(text string) bool {
	return p.cur.kind == .punct && p.cur.text == text
}

// peek_is_ident reports whether the token after `cur` is the given keyword.
pub fn (p &Parser) peek_is_ident(text string) bool {
	next := p.look_ahead()
	return next.kind == .ident && next.text == text
}

// advance consumes `cur`, moves the lookahead into its place, and reads a new
// lookahead.
pub fn (mut p Parser) advance() Token {
	t := p.cur
	if p.has_ahead {
		p.cur = p.ahead
		p.has_ahead = false
	} else {
		p.cur = p.read_token()
	}
	return t
}

// look_ahead returns the token after `cur` without consuming it. The result is
// cached, so asking twice gives the same token.
pub fn (mut p Parser) look_ahead() Token {
	if !p.has_ahead {
		p.ahead = p.read_token()
		p.has_ahead = true
	}
	return p.ahead
}

// read_token reads one token from the scanner, turning a read failure into an
// end-of-input token so the parser's loops terminate and the diagnostic already
// recorded is the one the user sees.
fn (mut p Parser) read_token() Token {
	return p.scanner.next() or {
		p.fail('read error: ${err.msg()}')
		Token{
			kind: .eof
		}
	}
}

// expect consumes a token with the given text, or records a diagnostic.
pub fn (mut p Parser) expect(text string) !Token {
	if p.cur.text == text {
		return p.advance()
	}
	p.fail('expected `${text}` but found `${p.cur.text}`')
	return Token{}
}

// expect_ident consumes an identifier, or records a diagnostic.
pub fn (mut p Parser) expect_ident(what string) !string {
	if p.cur.kind == .ident {
		return p.advance().text
	}
	p.fail('expected ${what} but found `${p.cur.text}`')
	return ''
}

// expect_number consumes an integer literal, or records a diagnostic.
pub fn (mut p Parser) expect_number(what string) !int {
	if p.cur.kind == .number {
		return p.advance().text.int()
	}
	p.fail('expected ${what} but found `${p.cur.text}`')
	return 0
}

// max_reported_errors bounds the diagnostic list. A malformed file can send the
// parser into a recovery loop, and an unbounded list turns that into an
// out-of-memory failure that hides the first, useful message.
pub const max_reported_errors = 50

// fail records a diagnostic against the current token's line.
pub fn (mut p Parser) fail(message string) {
	if p.errors.len >= max_reported_errors {
		return
	}
	p.errors << 'pbgen: ${p.path}:${p.cur.pos.line}:${p.cur.pos.col}: ${message}'
	if p.errors.len == max_reported_errors {
		p.errors << 'pbgen: ${p.path}: too many errors, stopping at ${max_reported_errors}'
	}
}

// stuck_at returns a token position used to detect that a statement consumed
// nothing. Every parse loop checks it, so a construct the parser does not
// understand costs one diagnostic instead of spinning until the process dies.
pub fn (p &Parser) stuck_at() bool {
	return p.cur.pos.line == 0 && p.cur.pos.col == 0 && p.cur.text == ''
}

// parse_syntax reads `syntax = "proto3";`.
pub fn (mut p Parser) parse_syntax(mut file File) ! {
	p.advance() // syntax
	p.expect('=')!
	if p.cur.kind != .str {
		p.fail('expected a quoted syntax version')
		p.advance()
		return
	}
	file.syntax = p.advance().text
	p.expect(';')!
}

// parse_package reads `package a.b.c;`.
pub fn (mut p Parser) parse_package(mut file File) ! {
	p.advance() // package
	file.package = p.parse_dotted_name()!
	p.expect(';')!
}

// parse_dotted_name reads a possibly-qualified name such as `google.rpc.Status`.
pub fn (mut p Parser) parse_dotted_name() !string {
	mut out := p.expect_ident('a name')!
	for p.is_cur('.') {
		p.advance()
		out += '.' + p.expect_ident('a name after `.`')!
	}
	return out
}

// parse_import reads `import [public|weak] "path";`.
pub fn (mut p Parser) parse_import(mut file File) ! {
	p.advance() // import
	mut imp := Import{}
	if p.cur.kind == .ident {
		// `public` and `weak` are modifiers, not a path.
		imp.public = p.cur.text == 'public'
		imp.weak = p.cur.text == 'weak'
		if imp.public || imp.weak {
			p.advance()
		}
	}
	if p.cur.kind != .str {
		p.fail('expected a quoted import path')
		p.advance()
		return
	}
	imp.path = p.advance().text
	p.expect(';')!
	file.imports << imp
}

// skip_option reads and discards an `option ...;` statement.
//
// A file, message, or service option describes the schema rather than its
// encoding, so none of them change the generated code. The value is still
// skipped correctly so a schema using options does not fail to parse.
pub fn (mut p Parser) skip_option() ! {
	p.read_option()!
}

// read_option reads an `option name = value;` statement and returns the name
// and, for a scalar or a dotted name, the value's text. An aggregate value in
// braces is skipped and reported as an empty string, and a custom option in
// parentheses is skipped whole.
pub fn (mut p Parser) read_option() !(string, string) {
	p.advance() // option
	if p.is_cur('(') {
		p.skip_balanced('(', ')')!
		return '', ''
	}
	name := p.parse_option_name()!
	p.expect('=')!
	// A value can be a scalar, a message literal in braces, or a dotted name.
	mut value := ''
	if p.is_cur('{') {
		p.skip_balanced('{', '}')!
	} else if p.cur.kind == .str || p.cur.kind == .number {
		value = p.advance().text
	} else {
		value = p.parse_dotted_name()!
	}
	p.expect(';')!
	return name, value
}

// parse_option_name reads a `(fully.qualified.option)` name without the parens.
pub fn (mut p Parser) parse_option_name() !string {
	mut out := ''
	if p.is_cur('(') {
		p.advance()
		out = p.parse_dotted_name()!
		p.expect(')')!
		return out
	}
	return p.expect_ident('an option name')!
}

// skip_balanced consumes a bracketed region, nesting included. It is what makes
// skipping an arbitrary option value possible without understanding it.
pub fn (mut p Parser) skip_balanced(open string, close string) ! {
	p.expect(open)!
	mut depth := 1
	for depth > 0 {
		if p.cur.kind == .eof {
			p.fail('unbalanced `${open}`')
			return
		}
		if p.cur.text == open {
			depth++
		} else if p.cur.text == close {
			depth--
		}
		p.advance()
	}
}

// parse_message reads `message Name { ... }`, recursing for nested messages.
pub fn (mut p Parser) parse_message(comments []string) !Message {
	pos := p.cur.pos
	p.advance() // message
	mut msg := Message{
		name:     p.expect_ident('a message name')!
		comments: comments
		pos:      pos
	}
	p.expect('{')!
	for !p.is_cur('}') {
		if p.cur.kind == .eof {
			p.fail('unterminated message `${msg.name}`')
			return msg
		}
		if p.is_cur(';') {
			p.advance()
			continue
		}
		field_comments := p.scanner.take_comments()
		// Recovery: remember where the statement started, and force progress if
		// it consumed nothing. Without this a construct the parser does not
		// understand would loop until the process ran out of memory, hiding the
		// first useful message behind thousands of repeats.
		before := p.cur.pos
		if p.cur.text == 'message' {
			msg.messages << p.parse_message(field_comments)!
		} else if p.cur.text == 'enum' {
			msg.enums << p.parse_enum(field_comments)!
		} else if p.cur.text == 'oneof' {
			group := p.parse_oneof(mut msg)!
			msg.oneofs << group
		} else if p.cur.text == 'reserved' {
			p.parse_reserved(mut msg)!
		} else if p.cur.text == 'option' {
			p.skip_option()!
		} else if p.cur.text == 'map' && p.look_ahead().text == '<' {
			f := p.parse_map_field(field_comments)!
			msg.fields << f
		} else {
			f := p.parse_field(field_comments, '')!
			if f.name != '' {
				msg.fields << f
			}
		}
		if p.cur.kind != .eof && p.cur.pos == before {
			p.fail('could not parse a statement in message `${msg.name}`')
			p.advance()
		}
	}
	p.expect('}')!
	return msg
}

// parse_oneof reads `oneof name { Type field = 1; ... }` and folds each member
// into the enclosing message's field list, tagged with the group name.
//
// The flattening is what the generator wants: V sumtype variants carry no
// attributes in this compiler, so a `oneof` becomes parallel optional fields
// that the emitter keeps exclusive by clearing the others when one is read.
pub fn (mut p Parser) parse_oneof(mut msg Message) !Oneof {
	p.advance() // oneof
	mut group := Oneof{
		name:     p.expect_ident('a oneof name')!
		comments: p.scanner.take_comments()
	}
	p.expect('{')!
	for !p.is_cur('}') {
		if p.cur.kind == .eof {
			p.fail('unterminated oneof `${group.name}`')
			return group
		}
		if p.is_cur(';') {
			p.advance()
			continue
		}
		comments := p.scanner.take_comments()

		if p.cur.text == 'option' {
			p.skip_option()!
			continue
		}
		f := p.parse_field(comments, group.name)!
		if f.name != '' {
			msg.fields << f
		}
	}
	p.expect('}')!
	return group
}

// parse_reserved reads `reserved 2, 15, 9 to 11;` or `reserved "foo", "bar";`.
//
// The numbers are recorded rather than enforced here, so a schema that reuses
// one can be reported by the generator against the offending field.
pub fn (mut p Parser) parse_reserved(mut msg Message) ! {
	p.advance() // reserved
	for {
		if p.cur.kind == .str {
			msg.reserved_names << p.advance().text
		} else if p.cur.kind == .number {
			start := p.advance().text.int()
			mut end := start
			if p.cur.text == 'to' {
				p.advance()
				// `to max` is an open-ended range, which runs to the top of the
				// field-number space.
				if p.cur.text == 'max' {
					p.advance()
					end = max_field_number
				} else {
					end = p.expect_number('the end of a reserved range')!
				}
			}
			msg.reserved_ranges << ReservedRange{
				start: start
				end:   end
			}
		} else {
			p.fail('unexpected `${p.cur.text}` in a reserved declaration')
			break
		}
		if p.is_cur(',') {
			p.advance()
			continue
		}
		break
	}
	p.expect(';')!
}

// parse_map_field reads `map<string, Value> name = 1;`.
pub fn (mut p Parser) parse_map_field(comments []string) !Field {
	pos := p.cur.pos
	p.advance() // map
	p.expect('<')!
	mut f := Field{
		kind:     .map
		label:    .repeated
		comments: comments
		pos:      pos
	}
	// Assigned after the literal: a call returning a Result cannot be unwrapped
	// inside a composite literal in this compiler.
	f.key_type = p.parse_type_name()!
	p.expect(',')!
	f.value_type = p.parse_type_name()!
	p.expect('>')!
	f.name = p.expect_ident('a field name')!
	p.expect('=')!
	f.number = p.expect_number('a field number')!
	f.type_name = 'map'
	// A map field carries no `packed`: it is a repeated Entry message, which has
	// a length-delimited element and so has nothing to pack.
	p.parse_field_options()!
	p.expect(';')!
	return f
}

// parse_field reads `[label] Type name = number [options];`.
//
// `oneof_group` is non-empty for a member of a `oneof`, which proto3 spells
// without a label.
pub fn (mut p Parser) parse_field(comments []string, oneof_group string) !Field {
	pos := p.cur.pos
	mut f := Field{
		comments: comments
		oneof:    oneof_group
		pos:      pos
	}
	if oneof_group != '' {
		f.label = .optional
	} else {
		f.label = .singular
	}
	// The three cardinality keywords are all optional context, and which one is
	// present decides how the field is written.
	if p.cur.text == 'repeated' {
		f.label = .repeated
		p.advance()
	} else if p.cur.text == 'optional' {
		f.label = .optional
		p.advance()
	} else if p.cur.text == 'required' {
		// proto2 only. Reported rather than silently treated as optional,
		// because `required` changes what a decoder must accept.
		p.fail('`required` is proto2 and is not supported; use `optional`')
		p.advance()
	}
	f.type_name = p.parse_type_name()!
	if f.type_name == '' {
		return Field{}
	}
	f.name = p.expect_ident('a field name')!
	p.expect('=')!
	f.number = p.expect_number('a field number')!
	f.kind = classify_type(f.type_name)
	f.packed_option = p.parse_field_options()!
	p.expect(';')!
	return f
}

// parse_type_name reads a scalar keyword or a possibly-qualified type name. A
// leading dot, as in `.pkg.Message`, marks a fully qualified name and is kept,
// since the resolver needs it to skip the scope search.
pub fn (mut p Parser) parse_type_name() !string {
	if p.is_cur('.') {
		p.advance()
		return '.' + p.parse_dotted_name()!
	}
	if p.cur.kind != .ident {
		p.fail('expected a type name but found `${p.cur.text}`')
		return ''
	}
	return p.parse_dotted_name()!
}

// parse_field_options consumes a trailing `[ ... ]` field option list and
// returns what the generator acts on.
//
// Only `packed` is read. The rest are skipped correctly so a schema using them
// parses, and discarding them is safe because none of them change the bytes: a
// `deprecated` or `json_name` option describes the schema rather than its
// encoding.
fn (mut p Parser) parse_field_options() !string {
	if !p.is_cur('[') {
		return ''
	}
	p.advance() // [
	mut packed := ''
	for p.cur.text != ']' {
		name := p.parse_option_name()!
		if p.is_cur('=') {
			p.advance()
		}
		mut value := p.cur.text
		if value == 'true' || value == 'false' {
			p.advance()
		} else {
			// A value the generator has no opinion about, such as
			// `[default = 5]` or an aggregate, is skipped rather than guessed at.
			p.skip_option_value()!
		}
		if name == 'packed' {
			packed = value
		}
		if p.is_cur(',') {
			p.advance()
		}
	}
	p.expect(']')!
	return packed
}

// skip_option_value consumes an option value the generator does not interpret.
fn (mut p Parser) skip_option_value() ! {
	if p.is_cur('{') {
		p.skip_balanced('{', '}')!
		return
	}
	if p.cur.kind == .str || p.cur.kind == .number {
		p.advance()
		return
	}
	p.parse_dotted_name()!
}

// classify_type decides how a declared type name is written, based on the
// proto3 scalar names. Anything else is a message or an enum, which only
// resolution can tell apart, so it is recorded as `.message` and revisited
// later.
pub fn classify_type(name string) FieldKind {
	return match name {
		'string' { FieldKind.text }
		'bytes' { FieldKind.bytes }
		'double', 'float', 'int32', 'int64', 'uint32', 'uint64', 'sint32', 'sint64',
		'fixed32', 'sfixed32', 'fixed64', 'sfixed64', 'bool' {
			FieldKind.scalar
		}
		else { FieldKind.message }
	}
}

// parse_enum reads `enum Name { VALUE = 0; ... }`.
pub fn (mut p Parser) parse_enum(comments []string) !EnumDecl {
	pos := p.cur.pos
	p.advance() // enum
	mut e := EnumDecl{
		name:     p.expect_ident('an enum name')!
		comments: comments
		pos:      pos
	}
	p.expect('{')!
	// proto3 requires the first enum value to be zero, so numbering starts there
	// and an omitted number continues from the previous value.
	mut last_value := 0
	for !p.is_cur('}') {
		if p.cur.kind == .eof {
			p.fail('unterminated enum `${e.name}`')
			return e
		}
		if p.is_cur(';') {
			p.advance()
			continue
		}
		value_comments := p.scanner.take_comments()

		if p.cur.text == 'option' {
			// `allow_alias` is the one enum option that changes what is legal:
			// it lets two values share a number.
			option_name, option_value := p.read_option()!
			if option_name == 'allow_alias' {
				e.allow_alias = option_value == 'true'
			}
			continue
		}
		if p.cur.text == 'reserved' {
			p.advance()
			for !p.is_cur(';') && p.cur.kind != .eof {
				p.advance()
			}
			p.advance()
			continue
		}
		name := p.expect_ident('an enum value name')!
		// proto3 lets a value omit its number, in which case it continues from
		// the previous one. The generated enum always spells every value out, so
		// resolving the numbering here keeps that promise.
		mut number := last_value
		if p.is_cur('=') {
			p.advance()
			// A negative value is legal and the lexer keeps the sign.
			number = p.expect_number('an enum value')!
		}
		p.parse_field_options()!
		p.expect(';')!
		e.values << EnumValue{
			name:     name
			number:   number
			comments: value_comments
		}
		last_value = number + 1
	}
	p.expect('}')!
	return e
}

// parse_service reads `service Name { rpc M(Req) returns (Resp); ... }`.
pub fn (mut p Parser) parse_service(comments []string) !Service {
	pos := p.cur.pos
	p.advance() // service
	mut svc := Service{
		name:     p.expect_ident('a service name')!
		comments: comments
		pos:      pos
	}
	p.expect('{')!
	for !p.is_cur('}') {
		if p.cur.kind == .eof {
			p.fail('unterminated service `${svc.name}`')
			return svc
		}
		if p.is_cur(';') {
			p.advance()
			continue
		}
		rpc_comments := p.scanner.take_comments()

		if p.cur.text == 'option' {
			p.skip_option()!
			continue
		}
		if p.cur.text == 'rpc' {
			svc.rpcs << p.parse_rpc(rpc_comments)!
		} else {
			p.fail('unexpected `${p.cur.text}` in service `${svc.name}`')
			p.advance()
		}
	}
	p.expect('}')!
	return svc
}

// parse_rpc reads `rpc Method(stream Req) returns (stream Resp);`.
pub fn (mut p Parser) parse_rpc(comments []string) !Rpc {
	pos := p.cur.pos
	p.advance() // rpc
	mut r := Rpc{
		name:     p.expect_ident('an rpc name')!
		comments: comments
		pos:      pos
	}
	p.expect('(')!
	if p.cur.text == 'stream' {
		r.client_stream = true
		p.advance()
	}
	r.request_type = p.parse_type_name()!
	p.expect(')')!
	p.expect('returns')!
	p.expect('(')!
	if p.cur.text == 'stream' {
		r.server_stream = true
		p.advance()
	}
	r.response_type = p.parse_type_name()!
	p.expect(')')!
	if p.is_cur('{') {
		// An rpc body holds options, which are not modelled.
		p.skip_balanced('{', '}')!
	} else {
		p.expect(';')!
	}
	return r
}
