module parser

import os
import strings
import v3.flat

enum VmlTokenKind {
	name
	string_
	number
	lbrace
	rbrace
	colon
	lpar
	rpar
	comma
	question
	plus
	minus
	mul
	div
	mod
	not
	eq
	ne
	lt
	le
	gt
	ge
	and
	or
	eof
}

struct VmlToken {
	kind VmlTokenKind
	text string
	line int
}

struct VmlLexer {
	source string
mut:
	pos  int
	line int = 1
}

fn (mut l VmlLexer) advance() u8 {
	c := l.source[l.pos]
	l.pos++
	if c == `\n` {
		l.line++
	}
	return c
}

fn (mut l VmlLexer) skip_space() {
	for l.pos < l.source.len {
		c := l.source[l.pos]
		if c in [` `, `\t`, `\r`, `\n`] {
			l.advance()
		} else if c == `/` && l.pos + 1 < l.source.len && l.source[l.pos + 1] == `/` {
			for l.pos < l.source.len && l.source[l.pos] != `\n` {
				l.pos++
			}
		} else {
			break
		}
	}
}

fn vml_is_name_char(c u8) bool {
	return c.is_alnum() || c in [`_`, `.`, `#`]
}

fn (mut l VmlLexer) read_name() VmlToken {
	start := l.pos
	line := l.line
	for l.pos < l.source.len && vml_is_name_char(l.source[l.pos]) {
		l.pos++
	}
	return VmlToken{.name, l.source[start..l.pos], line}
}

fn (mut l VmlLexer) read_number() VmlToken {
	start := l.pos
	line := l.line
	mut dot := false
	for l.pos < l.source.len {
		c := l.source[l.pos]
		if c.is_digit() {
			l.pos++
		} else if c == `.` && !dot {
			dot = true
			l.pos++
		} else {
			break
		}
	}
	return VmlToken{.number, l.source[start..l.pos], line}
}

fn (mut l VmlLexer) read_string() !VmlToken {
	line := l.line
	quote := l.advance()
	mut value := []u8{}
	for l.pos < l.source.len {
		c := l.advance()
		if c == `\\` && l.pos < l.source.len {
			next := l.advance()
			match next {
				`n` { value << `\n` }
				`t` { value << `\t` }
				`\\` { value << `\\` }
				`"` { value << `"` }
				`'` { value << `'` }
				else { value << next }
			}
		} else if c == quote {
			return VmlToken{.string_, value.bytestr(), line}
		} else {
			value << c
		}
	}
	return error('unterminated string at line ${line}')
}

fn tokenize_vml(source string) ![]VmlToken {
	mut lexer := VmlLexer{ source: source }
	mut tokens := []VmlToken{}
	for {
		lexer.skip_space()
		if lexer.pos >= source.len {
			tokens << VmlToken{.eof, '', lexer.line}
			return tokens
		}
		line := lexer.line
		c := source[lexer.pos]
		match c {
			`{` { tokens << VmlToken{.lbrace, '{', line} }
			`}` { tokens << VmlToken{.rbrace, '}', line} }
			`:` { tokens << VmlToken{.colon, ':', line} }
			`(` { tokens << VmlToken{.lpar, '(', line} }
			`)` { tokens << VmlToken{.rpar, ')', line} }
			`,` { tokens << VmlToken{.comma, ',', line} }
			`?` { tokens << VmlToken{.question, '?', line} }
			`+` { tokens << VmlToken{.plus, '+', line} }
			`-` { tokens << VmlToken{.minus, '-', line} }
			`*` { tokens << VmlToken{.mul, '*', line} }
			`/` { tokens << VmlToken{.div, '/', line} }
			`%` { tokens << VmlToken{.mod, '%', line} }
			`!` {
				lexer.pos++
				if lexer.pos < source.len && source[lexer.pos] == `=` {
					tokens << VmlToken{.ne, '!=', line}
				} else {
					tokens << VmlToken{.not, '!', line}
					continue
				}
			}
			`=` {
				lexer.pos++
				if lexer.pos >= source.len || source[lexer.pos] != `=` {
					return error('expected `==` at line ${line}')
				}
				tokens << VmlToken{.eq, '==', line}
			}
			`<` {
				lexer.pos++
				if lexer.pos < source.len && source[lexer.pos] == `=` {
					tokens << VmlToken{.le, '<=', line}
				} else {
					tokens << VmlToken{.lt, '<', line}
					continue
				}
			}
			`>` {
				lexer.pos++
				if lexer.pos < source.len && source[lexer.pos] == `=` {
					tokens << VmlToken{.ge, '>=', line}
				} else {
					tokens << VmlToken{.gt, '>', line}
					continue
				}
			}
			`&` {
				lexer.pos++
				if lexer.pos >= source.len || source[lexer.pos] != `&` {
					return error('expected `&&` at line ${line}')
				}
				tokens << VmlToken{.and, '&&', line}
			}
			`|` {
				lexer.pos++
				if lexer.pos >= source.len || source[lexer.pos] != `|` {
					return error('expected `||` at line ${line}')
				}
				tokens << VmlToken{.or, '||', line}
			}
			`"`, `'` {
				tokens << lexer.read_string()!
				continue
			}
			else {
				if c.is_digit() {
					tokens << lexer.read_number()
					continue
				}
				if vml_is_name_char(c) {
					tokens << lexer.read_name()
					continue
				}
				return error('unexpected character `${[c].bytestr()}` at line ${line}')
			}
		}
		lexer.pos++
	}
	return tokens
}

enum VmlExprKind {
	literal
	path
	call
	unary
	binary
	conditional
	interpolation
}

struct VmlInterpolationPart {
	text string
	// A nil expression marks a literal-text part. V requires `unsafe` only to
	// declare that sentinel pointer default; interpolation logic checks it before use.
	expr &VmlExpr = unsafe { nil }
}

struct VmlExpr {
	kind  VmlExprKind
	value string
	line  int
	// The tagged expression tree is recursive. Nil marks child links unused by
	// the current kind, and V requires `unsafe` only for those pointer defaults.
	left   &VmlExpr = unsafe { nil }
	right  &VmlExpr = unsafe { nil }
	third  &VmlExpr = unsafe { nil }
	args   []&VmlExpr
	parts  []VmlInterpolationPart
	quoted bool
}

struct VmlProperty {
	name          string
	declared_type string
	expr          &VmlExpr
}

struct VmlNode {
	tag  string
	line int
mut:
	id         string
	properties []VmlProperty
	children   []&VmlNode
}

struct VmlSourceParser {
	tokens []VmlToken
mut:
	pos int
}

fn (p &VmlSourceParser) at() VmlToken {
	return if p.pos < p.tokens.len { p.tokens[p.pos] } else { VmlToken{.eof, '', 0} }
}

fn (mut p VmlSourceParser) take(kind VmlTokenKind) !VmlToken {
	token := p.at()
	if token.kind != kind {
		return error('expected ${kind}, got ${token.kind} (`${token.text}`) at line ${token.line}')
	}
	p.pos++
	return token
}

fn (mut p VmlSourceParser) parse_node() !&VmlNode {
	tag := p.take(.name)!
	p.take(.lbrace)!
	mut node := &VmlNode{ tag: tag.text, line: tag.line }
	for p.at().kind !in [.rbrace, .eof] {
		if p.at().kind != .name {
			return error('unexpected token `${p.at().text}` at line ${p.at().line}')
		}
		if p.at().text == 'property' {
			p.pos++
			typ := p.take(.name)!
			name := p.take(.name)!
			p.take(.colon)!
			node.properties << VmlProperty{ name: name.text, declared_type: typ.text, expr: p.parse_expression()! }
		} else if p.pos + 1 < p.tokens.len && p.tokens[p.pos + 1].kind == .lbrace {
			node.children << p.parse_node()!
		} else if p.pos + 1 < p.tokens.len && p.tokens[p.pos + 1].kind == .colon {
			name := p.take(.name)!
			p.take(.colon)!
			expr := p.parse_expression()!
			if name.text == 'id' {
				if expr.kind !in [.literal, .path] {
					return error('id must be a literal identifier at line ${name.line}')
				}
				node.id = expr.value
			}
			node.properties << VmlProperty{ name: name.text, expr: expr }
		} else {
			return error('unexpected token `${p.at().text}` at line ${p.at().line}')
		}
	}
	p.take(.rbrace)!
	return node
}

fn (mut p VmlSourceParser) parse_expression() !&VmlExpr {
	return p.parse_conditional()
}

fn (mut p VmlSourceParser) parse_conditional() !&VmlExpr {
	condition := p.parse_or()!
	if p.at().kind != .question {
		return condition
	}
	line := p.take(.question)!.line
	when_true := p.parse_expression()!
	p.take(.colon)!
	return &VmlExpr{
		kind: .conditional
		line: line
		left: condition
		right: when_true
		third: p.parse_expression()!
	}
}

fn (mut p VmlSourceParser) parse_or() !&VmlExpr {
	mut left := p.parse_and()!
	for p.at().kind == .or {
		op := p.at()
		p.pos++
		left = &VmlExpr{
			kind: .binary
			value: op.text
			line: op.line
			left: left
			right: p.parse_and()!
		}
	}
	return left
}

fn (mut p VmlSourceParser) parse_and() !&VmlExpr {
	mut left := p.parse_equality()!
	for p.at().kind == .and {
		op := p.at()
		p.pos++
		left = &VmlExpr{
			kind: .binary
			value: op.text
			line: op.line
			left: left
			right: p.parse_equality()!
		}
	}
	return left
}

fn (mut p VmlSourceParser) parse_equality() !&VmlExpr {
	mut left := p.parse_comparison()!
	for p.at().kind in [.eq, .ne] {
		op := p.at()
		p.pos++
		left = &VmlExpr{
			kind: .binary
			value: op.text
			line: op.line
			left: left
			right: p.parse_comparison()!
		}
	}
	return left
}

fn (mut p VmlSourceParser) parse_comparison() !&VmlExpr {
	mut left := p.parse_term()!
	for p.at().kind in [.lt, .le, .gt, .ge] {
		op := p.at()
		p.pos++
		left = &VmlExpr{
			kind: .binary
			value: op.text
			line: op.line
			left: left
			right: p.parse_term()!
		}
	}
	return left
}

fn (mut p VmlSourceParser) parse_term() !&VmlExpr {
	mut left := p.parse_factor()!
	for p.at().kind in [.plus, .minus] {
		op := p.at()
		p.pos++
		left = &VmlExpr{
			kind: .binary
			value: op.text
			line: op.line
			left: left
			right: p.parse_factor()!
		}
	}
	return left
}

fn (mut p VmlSourceParser) parse_factor() !&VmlExpr {
	mut left := p.parse_unary()!
	for p.at().kind in [.mul, .div, .mod] {
		op := p.at()
		p.pos++
		left = &VmlExpr{
			kind: .binary
			value: op.text
			line: op.line
			left: left
			right: p.parse_unary()!
		}
	}
	return left
}

fn (mut p VmlSourceParser) parse_unary() !&VmlExpr {
	if p.at().kind in [.not, .minus] {
		op := p.at()
		p.pos++
		return &VmlExpr{ kind: .unary, value: op.text, line: op.line, left: p.parse_unary()! }
	}
	return p.parse_primary()
}

fn (mut p VmlSourceParser) parse_primary() !&VmlExpr {
	token := p.at()
	match token.kind {
		.string_ {
			p.pos++
			return parse_vml_interpolation(token.text, token.line)!
		}
		.name, .number {
			p.pos++
			if p.at().kind == .lpar {
				p.pos++
				mut args := []&VmlExpr{}
				if p.at().kind != .rpar {
					for {
						args << p.parse_expression()!
						if p.at().kind != .comma {
							break
						}
						p.pos++
					}
				}
				p.take(.rpar)!
				return &VmlExpr{ kind: .call, value: token.text, line: token.line, args: args }
			}
			return &VmlExpr{
				kind: if token.kind == .number || token.text in ['true', 'false'] {
					VmlExprKind.literal
				} else {
					VmlExprKind.path
				}
				value: token.text
				line: token.line
			}
		}
		.lpar {
			p.pos++
			expr := p.parse_expression()!
			p.take(.rpar)!
			return expr
		}
		else {
			return error('expected expression, got ${token.kind} (`${token.text}`) at line ${token.line}')
		}
	}
}

fn vml_interpolation_end(value string, start int) ?int {
	mut quoted := false
	mut escaped := false
	mut nested_braces := 0
	for i := start; i < value.len; i++ {
		ch := value[i]
		if quoted {
			if escaped {
				escaped = false
			} else if ch == `\\` {
				escaped = true
			} else if ch == `"` {
				quoted = false
			}
			continue
		}
		if ch == `"` {
			quoted = true
		} else if ch == `{` {
			nested_braces++
		} else if ch == `}` {
			if nested_braces == 0 {
				return i
			}
			nested_braces--
		}
	}
	return none
}

fn parse_vml_interpolation(value string, line int) !&VmlExpr {
	if !value.contains(r'${') {
		return &VmlExpr{ kind: .literal, value: value, line: line, quoted: true }
	}
	mut parts := []VmlInterpolationPart{}
	mut cursor := 0
	for cursor < value.len {
		start_relative := value[cursor..].index(r'${') or {
			parts << VmlInterpolationPart{ text: value[cursor..] }
			break
		}
		start := cursor + start_relative
		if start > cursor {
			parts << VmlInterpolationPart{ text: value[cursor..start] }
		}
		end := vml_interpolation_end(value, start + 2) or {
			return error('unterminated interpolation at line ${line}')
		}
		tokens := tokenize_vml(value[start + 2..end])!
		mut parser := VmlSourceParser{ tokens: tokens }
		parts << VmlInterpolationPart{ expr: parser.parse_expression()! }
		parser.take(.eof) or { return error('invalid interpolation at line ${line}: ${err}') }
		cursor = end + 1
	}
	return &VmlExpr{ kind: .interpolation, line: line, parts: parts }
}

fn parse_vml_source(source string) !&VmlNode {
	tokens := tokenize_vml(source)!
	mut parser := VmlSourceParser{ tokens: tokens }
	root := parser.parse_node()!
	parser.take(.eof)!
	validate_compiled_vml_node(root)!
	return root
}

fn validate_compiled_vml_node(node &VmlNode) ! {
	mut bindings := 0
	for property in node.properties {
		if property.name.starts_with('bind.') {
			bindings++
			bound_property := property.name.all_after('bind.')
			if bound_property !in ['text', 'checked', 'active', 'value'] {
				return error('two-way binding is not supported for `${bound_property}` at line ${property.expr.line}')
			}
			if property.expr.kind != .path || !property.expr.value.starts_with('app.')
				|| property.expr.value.count('.') != 1 {
				return error('`${property.name}` must target a mutable top-level app field at line ${property.expr.line}')
			}
		}
		if property.name in ['on_tap', 'on_change', 'on_active', 'on_text', 'on_submit']
			&& property.expr.kind == .call {
			if !property.expr.value.starts_with('app.') || property.expr.value.count('.') != 1 {
				return error('event handlers must call an app action at line ${property.expr.line}')
			}
			if property.expr.args.len > 1 {
				return error('app actions support at most one argument at line ${property.expr.line}')
			}
		}
	}
	if bindings > 1 {
		return error('an element can only have one two-way binding at line ${node.line}')
	}
	if node.tag == 'Repeater' {
		if vml_find_property(node, 'model') == none {
			return error('Repeater requires `model` at line ${node.line}')
		}
		if vml_find_property(node, 'key') == none {
			return error('Repeater requires a stable `key` at line ${node.line}')
		}
	}
	for child in node.children {
		validate_compiled_vml_node(child)!
	}
}

fn vml_expr_text(expr &VmlExpr) string {
	return match expr.kind {
		.literal, .path { expr.value }
		.call { expr.value + '(' + expr.args.map(vml_expr_text(it)).join(', ') + ')' }
		.unary { expr.value + vml_expr_text(expr.left) }
		.binary { '${vml_expr_text(expr.left)} ${expr.value} ${vml_expr_text(expr.right)}' }
		.conditional {
			'${vml_expr_text(expr.left)} ? ${vml_expr_text(expr.right)} : ${vml_expr_text(expr.third)}'
		}
		.interpolation {
			mut value := ''
			for part in expr.parts {
				value += if isnil(part.expr) {
					part.text
				} else {
					r'${' + vml_expr_text(part.expr) + '}'
				}
			}
			value
		}
	}
}

// parse_vml_template_expr compiles `$vml('file.vml')` into a direct ui2.Element builder.
fn (mut p Parser) parse_vml_template_expr(call_start int) flat.NodeId {
	p.next() // vml
	if p.tok != .lpar {
		p.record_diagnostic('expected `(` after `\$vml`', p.tok_pos)
		return p.add_val_id(5, '')
	}
	p.next()
	arg_id := p.expr(.lowest)
	arg := p.resolve_tmpl_path_arg(arg_id)
	for p.tok != .rpar && p.tok != .eof && p.tok != .semicolon {
		p.next()
	}
	if p.tok == .rpar {
		p.next()
	}
	if arg.len == 0 {
		p.record_diagnostic('`\$vml()` path must be a compile-time string', call_start)
		return p.add_val_id(5, '')
	}
	path := p.resolve_veb_template_path(false, arg)
	if !os.exists(path) {
		p.record_diagnostic('VML file `${path}` does not exist', call_start)
		return p.add_val_id(5, '')
	}
	source := os.read_file(path) or {
		p.record_diagnostic('cannot read VML file `${path}`: ${err.msg()}', call_start)
		return p.add_val_id(5, '')
	}
	root := parse_vml_source(source) or {
		p.record_diagnostic('${path}: ${err.msg()}', call_start)
		return p.add_val_id(5, '')
	}
	mut compiler := VmlCompiler{
		uses_app: vml_node_uses_path(root, 'app')
	}
	generated := compiler.compile(root)
	template := flat.Node{
		value: path
		pos: p.span_to(call_start)
	}
	return p.parse_veb_template_replacement_expr(generated, template, []TemplateSourceLine{}) or {
		p.record_diagnostic('could not lower VML file `${path}`', call_start)
		p.add_val_id(5, '')
	}
}

fn vml_node_uses_path(node &VmlNode, base string) bool {
	for property in node.properties {
		if vml_expr_uses_path(property.expr, base) {
			return true
		}
	}
	for child in node.children {
		if vml_node_uses_path(child, base) {
			return true
		}
	}
	return false
}

fn vml_expr_uses_path(expr &VmlExpr, base string) bool {
	if expr.kind in [.path, .call] && (expr.value == base || expr.value.starts_with(base + '.')) {
		return true
	}
	if !isnil(expr.left) && vml_expr_uses_path(expr.left, base) {
		return true
	}
	if !isnil(expr.right) && vml_expr_uses_path(expr.right, base) {
		return true
	}
	if !isnil(expr.third) && vml_expr_uses_path(expr.third, base) {
		return true
	}
	for arg in expr.args {
		if vml_expr_uses_path(arg, base) {
			return true
		}
	}
	for part in expr.parts {
		if !isnil(part.expr) && vml_expr_uses_path(part.expr, base) {
			return true
		}
	}
	return false
}

struct VmlNamedValue {
	frame string
	props map[string]string
}

struct VmlScope {
mut:
	ids     map[string]VmlNamedValue
	special map[string]string
}

struct VmlCompiler {
	uses_app bool
mut:
	out strings.Builder
}

fn (mut c VmlCompiler) compile(root &VmlNode) string {
	c.out = strings.new_builder(4096)
	capture := if c.uses_app { '[app] ' } else { '' }
	c.out.writeln('(fn ${capture}() ui2.Element {')
	c.out.writeln('\tvml_input_0 := ui2.bounds()')
	scope := VmlScope{
		ids: map[string]VmlNamedValue{}
		special: map[string]string{}
	}
	c.compile_node(root, '0', 'vml_input_0', scope, '')
	c.out.writeln('\treturn vml_element_0')
	c.out.write_string('}())')
	return c.out.str()
}

fn vml_clone_scope(scope VmlScope) VmlScope {
	return VmlScope{
		ids: scope.ids.clone()
		special: scope.special.clone()
	}
}

fn vml_var(path string) string {
	return path.replace('.', '_').replace('-', '_')
}

fn vml_quote(value string) string {
	return "'" + value.replace('\\', '\\\\').replace("'", "\\'").replace('\n', '\\n').replace('\r', '\\r').replace('\t', '\\t').replace(r'$', r'\$') + "'"
}

fn vml_interpolation_text(value string) string {
	return value.replace('\\', '\\\\').replace("'", "\\'").replace('\n', '\\n').replace('\r', '\\r').replace('\t', '\\t').replace(r'$', r'\$')
}

fn vml_stringify(expression string) string {
	return "'" + r'$' + '{' + expression + "}'"
}

fn vml_color_call(expression string) string {
	value := vml_stringify(expression)
	return "(if typeof(${expression}).name == 'string' { ui2.parse_hex_color(${value}) } else { u32(${value}.u64()) })"
}

fn vml_numeric_cast(type_name string, expression string) string {
	if expression.starts_with('(') && expression.ends_with(')') {
		return type_name + expression
	}
	return '${type_name}(${expression})'
}

enum VmlExprUse {
	raw
	number
	string_
	bool_
	color
}

fn vml_property_use(property VmlProperty) VmlExprUse {
	if property.declared_type.len > 0 {
		return match property.declared_type {
			'f64', 'f32', 'int' { .number }
			'bool' { .bool_ }
			'string' { .string_ }
			'color' { .color }
			else { .raw }
		}
	}
	if property.name in ['x', 'y', 'width', 'height', 'padding', 'spacing', 'corner_radius', 'radius',
		'rotation', 'font_size', 'size', 'head_indent', 'first_line_indent', 'hyphenation_factor',
		'lines', 'pad_left', 'dialog_width', 'dialog_height', 'border_width', 'border_left',
		'border_top', 'border_right', 'border_bottom', 'value', 'min', 'max', 'step', 'track_width',
		'thumb_size'] {
		return .number
	}
	if property.name in ['background', 'color', 'background_color', 'border_color',
		'value_track_color', 'thumb_color', 'inactive_color', 'active_color', 'disabled_track_color',
		'disabled_thumb_color'] {
		return .color
	}
	if property.name in ['checked', 'hidden', 'enabled', 'native', 'editable', 'emit_change', 'secure',
		'clickable', 'draggable', 'long_press', 'swipe_left', 'persistent', 'autocorrect', 'bold',
		'italic', 'underline', 'strikethrough', 'shadow', 'outline', 'value_track', 'active',
		'text_autoupdate']
		|| property.name in ['bind.checked', 'bind.active'] {
		return .bool_
	}
	if property.name == 'bind.value' {
		return .number
	}
	if property.name == 'model' {
		return .raw
	}
	return .string_
}

fn vml_expr_is_string(expr &VmlExpr) bool {
	if expr.kind == .interpolation || (expr.kind == .literal && expr.quoted) {
		return true
	}
	if expr.kind == .conditional {
		return vml_expr_is_string(expr.right) || vml_expr_is_string(expr.third)
	}
	return false
}

fn (c &VmlCompiler) resolve_path(path string, scope VmlScope) (string, bool) {
	parts := path.split('.')
	if parts.len == 0 {
		return path, false
	}
	if replacement := scope.special[parts[0]] {
		return replacement + if parts.len > 1 { '.' + parts[1..].join('.') } else { '' }, true
	}
	if named := scope.ids[parts[0]] {
		if parts.len >= 2 {
			if prop := named.props[parts[1]] {
				return prop + if parts.len > 2 { '.' + parts[2..].join('.') } else { '' }, true
			}
		}
		if parts.len == 2 {
			if parts[1] in ['x', 'y', 'width', 'height'] {
				return '${named.frame}.${parts[1]}', true
			}
		}
	}
	return path, path.contains('.')
}

fn (c &VmlCompiler) expr(expr &VmlExpr, scope VmlScope, use VmlExprUse) string {
	match expr.kind {
		.literal {
			if expr.quoted {
				if use == .color {
					if expr.value.starts_with('#') && expr.value.len == 7 {
						return 'u32(0x${expr.value[1..]})'
					}
					return 'ui2.parse_hex_color(${vml_quote(expr.value)})'
				}
				return vml_quote(expr.value)
			}
			return match use {
				.string_ { vml_quote(expr.value) }
				.number { 'f64(${expr.value})' }
				else { expr.value }
			}
		}
		.path {
			resolved, known := c.resolve_path(expr.value, scope)
			if use == .color {
				if expr.value.starts_with('#') && expr.value.len == 7 {
					return 'u32(0x${expr.value[1..]})'
				}
				if !known {
					return 'u32(0xffffff)'
				}
				return vml_color_call(resolved)
			}
			if use == .string_ {
				if !known {
					return vml_quote(expr.value)
				}
				return vml_stringify(resolved)
			}
			if use == .number {
				return 'f64(${resolved})'
			}
			return resolved
		}
		.call {
			args := expr.args.map(c.expr(it, scope, .raw)).join(', ')
			resolved, _ := c.resolve_path(expr.value, scope)
			call := '${resolved}(${args})'
			return match use {
				.string_ { vml_stringify(call) }
				.number { 'f64(${call})' }
				.color { vml_color_call(call) }
				else { call }
			}
		}
		.unary {
			operand_use := if use in [.raw, .string_] {
				VmlExprUse.raw
			} else if expr.value == '!' {
				VmlExprUse.bool_
			} else {
				VmlExprUse.number
			}
			result := '(${expr.value}${c.expr(expr.left, scope, operand_use)})'
			return if use == .string_ { vml_stringify(result) } else { result }
		}
		.binary {
			if expr.value in ['&&', '||'] {
				result := '(${c.expr(expr.left, scope, .bool_)} ${expr.value} ${c.expr(expr.right, scope, .bool_)})'
				return if use == .string_ { vml_stringify(result) } else { result }
			}
			if expr.value in ['==', '!=', '<', '<=', '>', '>='] {
				operand_use := if vml_expr_is_string(expr.left) || vml_expr_is_string(expr.right) {
					VmlExprUse.string_
				} else {
					VmlExprUse.raw
				}
				result := '(${c.expr(expr.left, scope, operand_use)} ${expr.value} ${c.expr(expr.right, scope, operand_use)})'
				return if use == .string_ { vml_stringify(result) } else { result }
			}
			if use == .raw {
				return '(${c.expr(expr.left, scope, .raw)} ${expr.value} ${c.expr(expr.right, scope, .raw)})'
			}
			if expr.value == '+' && use == .string_ {
				if vml_expr_is_string(expr.left) || vml_expr_is_string(expr.right) {
					return '(${c.expr(expr.left, scope, .string_)} + ${c.expr(expr.right, scope, .string_)})'
				}
				return vml_stringify('(${c.expr(expr.left, scope, .raw)} + ${c.expr(expr.right, scope, .raw)})')
			}
			if use == .string_ && expr.value in ['-', '*', '/', '%'] {
				return vml_stringify('(${c.expr(expr.left, scope, .raw)} ${expr.value} ${c.expr(expr.right, scope, .raw)})')
			}
			if expr.value == '%' {
				return 'f64(int(${c.expr(expr.left, scope, .number)}) % int(${c.expr(expr.right, scope, .number)}))'
			}
			return '(${c.expr(expr.left, scope, .number)} ${expr.value} ${c.expr(expr.right, scope, .number)})'
		}
		.conditional {
			return '(if ${c.expr(expr.left, scope, .bool_)} { ${c.expr(expr.right, scope, use)} } else { ${c.expr(expr.third, scope, use)} })'
		}
		.interpolation {
			mut value := "'"
			for part in expr.parts {
				if isnil(part.expr) {
					value += vml_interpolation_text(part.text)
				} else {
					value += r'$' + '{' + c.expr(part.expr, scope, .raw) + '}'
				}
			}
			return value + "'"
		}
	}
}

fn vml_find_property(node &VmlNode, name string) ?VmlProperty {
	for property in node.properties {
		if property.name == name {
			return property
		}
	}
	return none
}

fn vml_prop(properties map[string]string, name string, default_ string) string {
	return properties[name] or { default_ }
}

fn vml_property_is_geometry(property VmlProperty) bool {
	return property.name in ['x', 'y', 'width', 'height']
}

fn vml_order_properties_by_dependencies(properties []VmlProperty, node_id string) []VmlProperty {
	mut remaining := properties.clone()
	if node_id.len == 0 || remaining.len < 2 {
		return remaining
	}
	mut ordered := []VmlProperty{cap: remaining.len}
	for remaining.len > 0 {
		mut deferred := []VmlProperty{cap: remaining.len}
		for property in remaining {
			mut has_pending_dependency := false
			for candidate in remaining {
				if candidate.name != property.name
					&& vml_expr_uses_path(property.expr, '${node_id}.${candidate.name}') {
					has_pending_dependency = true
					break
				}
			}
			if has_pending_dependency {
				deferred << property
			} else {
				ordered << property
			}
		}
		if deferred.len == remaining.len {
			// Preserve source order for a dependency cycle; generated V will report
			// the invalid forward reference without making this pass loop forever.
			ordered << deferred
			break
		}
		remaining = deferred.clone()
	}
	return ordered
}

fn (mut c VmlCompiler) compile_node(node &VmlNode, path string, input string, incoming VmlScope, default_key string) VmlScope {
	suffix := vml_var(path)
	c.out.writeln('\t_ = ${input}')
	mut scope := vml_clone_scope(incoming)
	mut named_props := map[string]string{}
	// Declared properties are visible before expressions are resolved. Geometry
	// bindings are added after they are emitted below, so self-geometry can use an
	// earlier computed binding while declared properties can still read the input.
	for property in node.properties {
		if property.declared_type.len > 0 {
			named_props[property.name] = 'vml_property_${suffix}_${vml_var(property.name)}'
		}
	}
	if node.id.len > 0 {
		scope.ids[node.id] = VmlNamedValue{ frame: input, props: named_props.clone() }
	}
	mut properties := map[string]string{}
	mut ordered_properties := vml_order_properties_by_dependencies(node.properties.filter(it.declared_type.len > 0), node.id)
	ordered_properties << vml_order_properties_by_dependencies(node.properties.filter(it.declared_type.len == 0
		&& vml_property_is_geometry(it)), node.id)
	ordered_properties << node.properties.filter(it.declared_type.len == 0
		&& !vml_property_is_geometry(it))
	for property in ordered_properties {
		if property.name == 'id'
			|| property.name in ['on_tap', 'on_change', 'on_active', 'on_text', 'on_submit'] {
			continue
		}
		name := 'vml_property_${suffix}_${vml_var(property.name)}'
		property_use := vml_property_use(property)
		value := if property.declared_type == 'int' {
			numeric_value := c.expr(property.expr, scope, .raw)
			vml_numeric_cast(property.declared_type, numeric_value)
		} else if property.declared_type == 'f32' {
			numeric_value := c.expr(property.expr, scope, .number)
			vml_numeric_cast(property.declared_type, numeric_value)
		} else {
			c.expr(property.expr, scope, property_use)
		}
		if property.declared_type.len == 0 && property_use == .color
			&& property.expr.kind in [.literal, .path]
			&& property.expr.value.starts_with('#') && property.expr.value.len == 7 {
			// Static colors can be used directly without a generated local.
			properties[property.name] = value
			continue
		}
		c.out.writeln('\t${name} := ${value}')
		properties[property.name] = name
		if property.declared_type.len == 0 && vml_property_is_geometry(property) {
			named_props[property.name] = name
			if node.id.len > 0 {
				scope.ids[node.id] = VmlNamedValue{ frame: input, props: named_props.clone() }
			}
		}
	}
	frame := 'vml_frame_${suffix}'
	c.out.writeln('\t${frame} := ui2.rect(${vml_prop(properties, 'x', input + '.x')}, ${vml_prop(properties, 'y', input + '.y')}, ${vml_prop(properties, 'width', input + '.width')}, ${vml_prop(properties, 'height', input + '.height')})')
	if node.id.len > 0 {
		scope.ids[node.id] = VmlNamedValue{ frame: frame, props: named_props.clone() }
	}
	c.write_action_type_checks(node, suffix, scope)
	children := 'vml_children_${suffix}'
	container := node.tag in ['Screen', 'View', 'Rectangle', 'Column', 'Row', 'Scroll']
		|| node.tag !in ['Label', 'Image', 'Button', 'MessageBox', 'Checkbox', 'Dropdown', 'TextArea',
			'TextField', 'ProgressBar', 'Slider', 'Switch', 'Spinner']
	visible_children := if container {
		node.children.filter(it.tag !in ['MenuItem', 'Option'])
	} else {
		[]&VmlNode{}
	}
	if container {
		c.out.writeln('\tmut ${children} := []ui2.Element{cap: ${visible_children.len}}')
	}
	mut cursor := ''
	if container && node.tag in ['Column', 'Row'] {
		cursor = 'vml_cursor_${suffix}'
		c.out.writeln('\tmut ${cursor} := ${vml_prop(properties, 'padding', 'f64(0)')}')
	}
	for child_index, child in node.children {
		child_path := '${path}.${child_index}'
		if child.tag == 'MenuItem' {
			c.write_action_type_checks(child, '${vml_var(child_path)}_menu_action', scope)
			continue
		}
		if !container || child.tag == 'Option' {
			continue
		}
		if child.tag == 'Repeater' {
			c.compile_repeater(child, child_path, frame, node.tag, properties, cursor, scope, children, '')
			continue
		}
		child_input := 'vml_input_${vml_var(child_path)}'
		c.out.writeln('\t${child_input} := ${vml_child_input(node.tag, frame, properties, cursor)}')
		child_scope := c.compile_node(child, child_path, child_input, scope, '')
		for id, named in child_scope.ids {
			scope.ids[id] = named
		}
		c.out.writeln('\t${children} << vml_element_${vml_var(child_path)}')
		vml_advance_cursor(mut c.out, node.tag, cursor, child_path, properties)
	}
	c.compile_element(node, suffix, frame, children, properties, scope, default_key)
	return scope
}

fn (mut c VmlCompiler) write_action_type_checks(node &VmlNode, suffix string, scope VmlScope) {
	for property in node.properties {
		if property.name !in ['on_tap', 'on_change', 'on_active', 'on_text', 'on_submit'] || property.expr.kind != .call {
			continue
		}
		method_name := property.expr.value.all_after('app.')
		check_name := 'vml_action_check_${suffix}_${vml_var(property.name)}'
		arguments := property.expr.args.map(c.expr(it, scope, .raw)).join(', ')
		// The branch is never taken, but V still checks that the method exists and
		// that its argument has the declared type.
		c.out.writeln('\tif false {')
		c.out.writeln('\t\tmut ${check_name} := *app')
		// The runtime dispatcher exposes public methods only. Reflection makes a
		// same-module private method a compile error before its action is serialized.
		c.out.writeln('\t\t\$for method in ${check_name}.methods {')
		c.out.writeln('\t\t\t\$if method.name == ${vml_quote(method_name)} {')
		c.out.writeln('\t\t\t\t\$if !method.is_pub {')
		c.out.writeln('\t\t\t\t\t\$compile_error(${vml_quote('VML action method `${method_name}` must be public')})')
		c.out.writeln('\t\t\t\t}')
		c.out.writeln('\t\t\t}')
		c.out.writeln('\t\t}')
		c.out.writeln('\t\t${check_name}.${method_name}(${arguments})')
		c.out.writeln('\t}')
	}
}

fn vml_child_input(tag string, frame string, properties map[string]string, cursor string) string {
	padding := vml_prop(properties, 'padding', 'f64(0)')
	return match tag {
		'Column' {
			'ui2.rect(${padding}, ${cursor}, ${frame}.width - ${padding} * f64(2), f64(32))'
		}
		'Row' {
			'ui2.rect(${cursor}, ${padding}, f64(80), ${frame}.height - ${padding} * f64(2))'
		}
		else { 'ui2.rect(f64(0), f64(0), ${frame}.width, ${frame}.height)' }
	}
}

fn vml_advance_cursor(mut out strings.Builder, tag string, cursor string, child_path string, properties map[string]string) {
	spacing := vml_prop(properties, 'spacing', 'f64(0)')
	if tag == 'Column' {
		out.writeln('\t${cursor} += vml_frame_${vml_var(child_path)}.height + ${spacing}')
	} else if tag == 'Row' {
		out.writeln('\t${cursor} += vml_frame_${vml_var(child_path)}.width + ${spacing}')
	}
}

fn (mut c VmlCompiler) compile_repeater(node &VmlNode, path string, parent_frame string, parent_tag string, parent_properties map[string]string, cursor string, incoming VmlScope, output string, outer_key string) {
	model := vml_find_property(node, 'model') or { return }
	key := vml_find_property(node, 'key') or { return }
	suffix := vml_var(path)
	index_name := 'vml_index_${suffix}'
	item_name := 'vml_item_${suffix}'
	key_name := 'vml_key_${suffix}'
	c.out.writeln('\tfor ${index_name}, ${item_name} in ${c.expr(model.expr, incoming, .raw)} {')
	mut scope := vml_clone_scope(incoming)
	scope.special['item'] = item_name
	scope.special['index'] = index_name
	key_value := c.expr(key.expr, scope, .string_)
	if outer_key.len > 0 {
		local_key_name := '${key_name}_local'
		c.out.writeln('\t\t${local_key_name} := ${key_value}')
		c.out.writeln("\t\t${key_name} := ${outer_key} + ':' + ${local_key_name}")
	} else {
		c.out.writeln('\t\t${key_name} := ${key_value}')
	}
	visible_count := node.children.filter(it.tag !in ['MenuItem', 'Option']).len
	mut visible_index := 0
	for child_index, child in node.children {
		if child.tag in ['MenuItem', 'Option'] {
			continue
		}
		child_path := '${path}.${child_index}'
		if child.tag == 'Repeater' {
			c.compile_repeater(child, child_path, parent_frame, parent_tag, parent_properties, cursor, scope, output, key_name)
			continue
		}
		child_input := 'vml_input_${vml_var(child_path)}'
		c.out.writeln('\t\t${child_input} := ${vml_child_input(parent_tag, parent_frame, parent_properties, cursor)}')
		default_key := if visible_count == 1 {
			key_name
		} else {
			"'" + r'$' + '{' + key_name + '}' + ':${visible_index}' + "'"
		}
		child_scope := c.compile_node(child, child_path, child_input, scope, default_key)
		for id, named in child_scope.ids {
			scope.ids[id] = named
		}
		c.out.writeln('\t\t${output} << vml_element_${vml_var(child_path)}')
		vml_advance_cursor(mut c.out, parent_tag, cursor, child_path, parent_properties)
		visible_index++
	}
	c.out.writeln('\t}')
}

fn vml_value(properties map[string]string, name string, alternative string, default_ string) string {
	if value := properties[name] {
		return value
	}
	if alternative.len > 0 {
		if value := properties[alternative] {
			return value
		}
	}
	return default_
}

fn vml_binding_for_event(node &VmlNode, event_name string) ?VmlProperty {
	if event_name == 'on_change' {
		if binding := vml_find_property(node, 'bind.value') {
			return binding
		}
		return vml_find_property(node, 'bind.text')
	}
	if event_name == 'on_tap' {
		return vml_find_property(node, 'bind.checked')
	}
	if event_name == 'on_active' {
		return vml_find_property(node, 'bind.active')
	}
	if event_name == 'on_text' {
		return vml_find_property(node, 'bind.text')
	}
	return none
}

fn (c &VmlCompiler) compiled_event_value(node &VmlNode, event_name string, scope VmlScope, control string) string {
	action := vml_find_property(node, event_name)
	binding := vml_binding_for_event(node, event_name)
	if action == none && binding == none {
		return "''"
	}
	if property := action {
		if property.expr.kind != .call && binding == none {
			return c.expr(property.expr, scope, .string_)
		}
	}
	binding_property := if property := binding { property.name.all_after('bind.') } else { '' }
	binding_target := if property := binding { vml_expr_text(property.expr) } else { '' }
	mut action_name := ''
	mut arguments := []string{}
	if property := action {
		if property.expr.kind == .call {
			action_name = property.expr.value.all_after('app.')
			for argument in property.expr.args {
				arguments << vml_stringify(c.expr(argument, scope, .raw))
			}
		}
	}
	if arguments.len == 0 {
		return 'ui2.compiled_vml_event(${control}, ${vml_quote(binding_property)}, ${vml_quote(binding_target)}, ${vml_quote(action_name)})'
	}
	if property := action {
		argument := property.expr.args[0]
		if argument.kind == .path && argument.value.starts_with('app.') {
			return 'ui2.compiled_vml_event_arg_path(${control}, ${vml_quote(binding_property)}, ${vml_quote(binding_target)}, ${vml_quote(action_name)}, ${vml_quote(argument.value)})'
		}
	}
	return 'ui2.compiled_vml_event_arg(${control}, ${vml_quote(binding_property)}, ${vml_quote(binding_target)}, ${vml_quote(action_name)}, ${arguments[0]})'
}

fn (c &VmlCompiler) box_style(properties map[string]string) string {
	border_width := vml_prop(properties, 'border_width', 'f64(0)')
	return 'ui2.BoxStyle{bg: ${vml_prop(properties, 'background', 'u32(0xffffff)')}, radius: ${vml_value(properties, 'corner_radius', 'radius', 'f64(0)')}, border_color: ${vml_prop(properties, 'border_color', 'u32(0)')}, border_left: ${vml_prop(properties, 'border_left', border_width)}, border_top: ${vml_prop(properties, 'border_top', border_width)}, border_right: ${vml_prop(properties, 'border_right', border_width)}, border_bottom: ${vml_prop(properties, 'border_bottom', border_width)}}'
}

fn (c &VmlCompiler) text_style(properties map[string]string) string {
	align := vml_prop(properties, 'align', "''")
	fields := [
		'color: ${vml_prop(properties, 'color', 'u32(0x111111)')}',
		'background_color: ${vml_prop(properties, 'background_color', 'u32(0)')}',
		'size: ${vml_value(properties, 'font_size', 'size', 'f64(15)')}',
		'font_family: ${vml_prop(properties, 'font_family', "''")}',
		'bold: ${vml_prop(properties, 'bold', 'false')}',
		'italic: ${vml_prop(properties, 'italic', 'false')}',
		'underline: ${vml_prop(properties, 'underline', 'false')}',
		'strikethrough: ${vml_prop(properties, 'strikethrough', 'false')}',
		'shadow: ${vml_prop(properties, 'shadow', 'false')}',
		'outline: ${vml_prop(properties, 'outline', 'false')}',
		'vertical_align: ${vml_prop(properties, 'vertical_align', "''")}',
		'link: ${vml_prop(properties, 'link', "''")}',
		"align: match ${align} { 'center' { .center } 'right' { .right } else { .left } }",
		'head_indent: ${vml_prop(properties, 'head_indent', 'f64(0)')}',
		'first_line_indent: ${vml_prop(properties, 'first_line_indent', 'f64(0)')}',
		'hyphenation_factor: ${vml_prop(properties, 'hyphenation_factor', 'f64(0)')}',
		'lines: int(${vml_prop(properties, 'lines', 'f64(1)')})',
	]
	return 'ui2.TextStyle{${fields.join(', ')}}'
}

fn (c &VmlCompiler) menu_value(node &VmlNode, scope VmlScope) string {
	menu_items := node.children.filter(it.tag == 'MenuItem')
	if menu_items.len > 0 {
		mut entries := []string{cap: menu_items.len}
		for item in menu_items {
			fallback_id := vml_quote(item.id)
			id := c.compiled_event_value(item, 'on_tap', scope, fallback_id)
			text := if property := vml_find_property(item, 'text') {
				c.expr(property.expr, scope, .string_)
			} else {
				"''"
			}
			entries << 'ui2.MenuEntry{id: if ${id}.len > 0 { ${id} } else { ${fallback_id} }, title: ${text}}'
		}
		return '[]ui2.MenuEntry{${entries.join(', ')}}'
	}
	if node.tag in ['Dropdown', 'Spinner'] {
		mut entries := []string{}
		for option in node.children {
			if option.tag != 'Option' {
				continue
			}
			text := if property := vml_find_property(option, 'text') {
				c.expr(property.expr, scope, .string_)
			} else {
				"''"
			}
			entries << 'ui2.MenuEntry{id: ${text}, title: ${text}}'
		}
		return '[]ui2.MenuEntry{${entries.join(', ')}}'
	}
	return '[]ui2.MenuEntry{}'
}

fn (c &VmlCompiler) option_values(node &VmlNode, scope VmlScope) string {
	mut values := []string{}
	for option in node.children {
		if option.tag != 'Option' {
			continue
		}
		value := if property := vml_find_property(option, 'text') {
			c.expr(property.expr, scope, .string_)
		} else {
			"''"
		}
		values << value
	}
	return '[]string{${values.join(', ')}}'
}

fn (mut c VmlCompiler) compile_element(node &VmlNode, suffix string, frame string, children string, properties map[string]string, scope VmlScope, default_key string) {
	has_binding := vml_find_property(node, 'bind.text') != none
		|| vml_find_property(node, 'bind.checked') != none
		|| vml_find_property(node, 'bind.active') != none || vml_find_property(node, 'bind.value') != none
	id := if node.id.len > 0 {
		vml_quote(node.id)
	} else if has_binding && default_key.len > 0 {
		"'__vml_control_${suffix}_" + r'$' + '{' + default_key + "}'"
	} else if has_binding {
		vml_quote('__vml_control_${suffix}')
	} else {
		"''"
	}
	key := vml_prop(properties, 'key', if default_key.len > 0 { default_key } else { "''" })
	on_tap := c.compiled_event_value(node, 'on_tap', scope, id)
	on_change := c.compiled_event_value(node, 'on_change', scope, id)
	on_active := c.compiled_event_value(node, 'on_active', scope, id)
	on_text := c.compiled_event_value(node, 'on_text', scope, id)
	on_submit := c.compiled_event_value(node, 'on_submit', scope, id)
	text_action := if vml_find_property(node, 'on_text') != none { on_text } else { on_change }
	action := match node.tag {
		'Button', 'Checkbox' { on_tap }
		'Dropdown', 'Slider' { 'if ${on_change}.len > 0 { ${on_change} } else { ${on_tap} }' }
		'Switch' {
			'if ${on_active}.len > 0 { ${on_active} } else if ${on_change}.len > 0 { ${on_change} } else { ${on_tap} }'
		}
		'Spinner' {
			'if ${on_text}.len > 0 { ${on_text} } else if ${on_change}.len > 0 { ${on_change} } else { ${on_tap} }'
		}
		'TextArea', 'TextField' { text_action }
		else { on_tap }
	}
	if node.tag == 'MessageBox' {
		c.compile_message_box(node, suffix, frame, properties, scope, key, action)
		return
	}
	if node.tag == 'ProgressBar' {
		c.compile_progress_bar(node, suffix, frame, properties, scope, key, action)
		return
	}
	if node.tag == 'Slider' {
		c.compile_slider(node, suffix, frame, properties, scope, key, action)
		return
	}
	if node.tag == 'Switch' {
		c.compile_switch(node, suffix, frame, properties, scope, key, action)
		return
	}
	if node.tag == 'Spinner' {
		c.compile_spinner(node, suffix, frame, properties, scope, key, action)
		return
	}
	kind := match node.tag {
		'Screen' { 'screen' }
		'Label' { 'label' }
		'Image' { 'image' }
		'Button' { 'button' }
		'Checkbox' { 'checkbox' }
		'Dropdown' { 'dropdown' }
		'TextField' { 'text_field' }
		'TextArea' { 'text_area' }
		'Scroll' { 'scroll' }
		else { 'view' }
	}
	c.out.writeln('\tvml_element_${suffix} := ui2.Element{')
	c.out.writeln('\t\tkind: .${kind}')
	if node.tag != 'Screen' {
		c.out.writeln('\t\tid: ${id}')
		c.out.writeln('\t\tframe: ${frame}')
	}
	c.out.writeln('\t\taction_id: ${action}')
	c.out.writeln('\t\tsubmit_id: ${on_submit}')
	c.out.writeln('\t\tkey: ${key}')
	if node.tag in ['Label', 'Button', 'Checkbox', 'Dropdown', 'TextField', 'TextArea'] {
		c.out.writeln('\t\ttext: ${vml_value(properties, 'text', 'bind.text', "''")}')
	}
	if node.tag == 'Checkbox' {
		c.out.writeln('\t\tchecked: ${vml_value(properties, 'checked', 'bind.checked', 'false')}')
	}
	if node.tag == 'Image' {
		c.out.writeln('\t\timage_path: ${vml_value(properties, 'source', 'path', "''")}')
	}
	if node.tag == 'TextField' {
		c.out.writeln('\t\tplaceholder: ${vml_prop(properties, 'placeholder', "''")}')
		keyboard := vml_prop(properties, 'keyboard', "''")
		c.out.writeln("\t\tkeyboard: if ${keyboard} in ['decimal', 'numeric', 'number'] { ui2.keyboard_decimal } else { ui2.keyboard_default }")
		c.out.writeln('\t\temit_change: ${vml_prop(properties, 'emit_change', 'false')} || ${text_action}.len > 0')
	}
	if node.tag == 'TextArea' {
		c.out.writeln('\t\treadonly: ${vml_prop(properties, 'editable', 'true')} == false')
		c.out.writeln('\t\temit_change: ${text_action}.len > 0')
	}
	if node.tag == 'Screen' {
		c.out.writeln('\t\tbox: ui2.BoxStyle{bg: ${vml_prop(properties, 'background', 'u32(0xffffff)')}}')
	} else if node.tag == 'Scroll' {
		c.out.writeln('\t\tbox: ${c.box_style(properties)}')
		c.out.writeln('\t\tpersistent_scrollbars: ${vml_prop(properties, 'persistent', 'false')}')
	} else if node.tag == 'Checkbox' {
		c.out.writeln('\t\tbox: ui2.BoxStyle{transparent: true}')
	} else if node.tag in ['View', 'Rectangle', 'Column', 'Row', 'Button', 'Dropdown', 'TextField',
		'TextArea'] || kind == 'view' {
		c.out.writeln('\t\tbox: ${c.box_style(properties)}')
	}
	if node.tag in ['Label', 'Button', 'Checkbox', 'Dropdown', 'TextField', 'TextArea'] {
		c.out.writeln('\t\ttext_style: ${c.text_style(properties)}')
	}
	if node.tag in ['Screen', 'View', 'Rectangle', 'Column', 'Row', 'Scroll'] || kind == 'view' {
		c.out.writeln('\t\tchildren: ${children}')
	}
	role_default := if node.tag == 'Checkbox' { "'checkbox'" } else { "''" }
	label_default := if node.tag == 'Checkbox' {
		vml_value(properties, 'text', 'bind.text', "''")
	} else {
		"''"
	}
	value_default := if node.tag == 'Checkbox' {
		"if ${vml_value(properties, 'checked', 'bind.checked', 'false')} { 'checked' } else { 'unchecked' }"
	} else {
		"''"
	}
	c.write_common_fields(node, properties, scope, role_default, label_default, value_default)
	c.out.writeln('\t}')
}

fn (mut c VmlCompiler) write_common_fields(node &VmlNode, properties map[string]string, scope VmlScope, role_default string, label_default string, value_default string) {
	c.out.writeln('\t\tmenu: ${c.menu_value(node, scope)}')
	c.out.writeln('\t\tsecure: ${vml_prop(properties, 'secure', 'false')}')
	c.out.writeln('\t\tclickable: ${vml_prop(properties, 'clickable', 'false')}')
	c.out.writeln('\t\tdraggable: ${vml_prop(properties, 'draggable', 'false')}')
	c.out.writeln('\t\tlong_press: ${vml_prop(properties, 'long_press', 'false')}')
	c.out.writeln('\t\tswipe_left: ${vml_prop(properties, 'swipe_left', 'false')}')
	c.out.writeln('\t\trotation: ${vml_prop(properties, 'rotation', 'f64(0)')}')
	c.out.writeln('\t\tcursor: ${vml_prop(properties, 'cursor', "''")}')
	c.out.writeln('\t\ttooltip: ${vml_prop(properties, 'tooltip', "''")}')
	c.out.writeln('\t\thidden: ${vml_prop(properties, 'hidden', 'false')}')
	c.out.writeln('\t\tenabled: ${vml_prop(properties, 'enabled', 'true')}')
	c.out.writeln('\t\taccessibility_role: ${vml_prop(properties, 'accessibility_role', role_default)}')
	c.out.writeln('\t\taccessibility_label: ${vml_prop(properties, 'accessibility_label', label_default)}')
	c.out.writeln('\t\taccessibility_value: ${vml_prop(properties, 'accessibility_value', value_default)}')
	c.out.writeln('\t\tnative_style: ${vml_prop(properties, 'native', 'false')}')
	c.out.writeln('\t\tautocorrect: ${vml_prop(properties, 'autocorrect', 'true')}')
	c.out.writeln('\t\tpadding_left: ${vml_prop(properties, 'pad_left', 'f64(12)')}')
}

fn (mut c VmlCompiler) compile_progress_bar(node &VmlNode, suffix string, frame string, properties map[string]string, scope VmlScope, key string, action string) {
	base := 'vml_progress_${suffix}'
	c.out.writeln('\t${base} := ui2.progress_bar(')
	c.out.writeln('\t\tid: ${vml_quote(node.id)}')
	c.out.writeln('\t\tframe: ${frame}')
	c.out.writeln('\t\tvalue: ${vml_prop(properties, 'value', 'f64(0)')}')
	c.out.writeln('\t\tmax: ${vml_prop(properties, 'max', 'f64(100)')}')
	c.out.writeln('\t\tbackground: ${vml_prop(properties, 'background', 'u32(0xe2e8f0)')}')
	c.out.writeln('\t\tcolor: ${vml_prop(properties, 'color', 'u32(0x3b82f6)')}')
	c.out.writeln('\t\tradius: ${vml_value(properties, 'corner_radius', 'radius', 'f64(4)')}')
	c.out.writeln('\t)')
	c.out.writeln('\tvml_element_${suffix} := ui2.Element{')
	c.out.writeln('\t\t...${base}')
	c.out.writeln('\t\taction_id: ${action}')
	c.out.writeln('\t\tkey: ${key}')
	c.write_common_fields(node, properties, scope, '${base}.accessibility_role', '${base}.accessibility_label', '${base}.accessibility_value')
	c.out.writeln('\t}')
}

fn (mut c VmlCompiler) compile_slider(node &VmlNode, suffix string, frame string, properties map[string]string, scope VmlScope, key string, action string) {
	base := 'vml_slider_${suffix}'
	style := '${base}_style'
	orientation_value := '${base}_orientation'
	orientation := vml_prop(properties, 'orientation', "''")
	c.out.writeln('\t${style} := ui2.SliderStyle{')
	c.out.writeln('\t\ttrack_color: ${vml_prop(properties, 'background', 'u32(0xcbd5e1)')}')
	c.out.writeln('\t\tvalue_track_color: ${vml_value(properties, 'value_track_color', 'color', 'u32(0x93c5fd)')}')
	c.out.writeln('\t\tthumb_color: ${vml_prop(properties, 'thumb_color', 'u32(0x2563eb)')}')
	c.out.writeln('\t\ttrack_width: ${vml_prop(properties, 'track_width', 'f64(4)')}')
	c.out.writeln('\t\tthumb_size: ${vml_prop(properties, 'thumb_size', 'f64(20)')}')
	c.out.writeln('\t}')
	c.out.writeln("\t${orientation_value} := if ${orientation} == 'vertical' { ui2.Orientation.vertical } else { ui2.Orientation.horizontal }")
	c.out.writeln('\t${base} := ui2.slider(')
	c.out.writeln('\t\tid: ${vml_quote(node.id)}')
	c.out.writeln('\t\taction_id: ${action}')
	c.out.writeln('\t\tframe: ${frame}')
	c.out.writeln('\t\tmin: ${vml_prop(properties, 'min', 'f64(0)')}')
	c.out.writeln('\t\tmax: ${vml_prop(properties, 'max', 'f64(100)')}')
	c.out.writeln('\t\tvalue: ${vml_value(properties, 'value', 'bind.value', 'f64(0)')}')
	c.out.writeln('\t\tstep: ${vml_prop(properties, 'step', 'f64(0)')}')
	c.out.writeln('\t\torientation: ${orientation_value}')
	c.out.writeln('\t\tpadding: ${vml_prop(properties, 'padding', 'f64(16)')}')
	c.out.writeln('\t\tvalue_track: ${vml_prop(properties, 'value_track', 'false')}')
	c.out.writeln('\t\tstyle: ${style}')
	c.out.writeln('\t)')
	c.out.writeln('\tvml_element_${suffix} := ui2.Element{')
	c.out.writeln('\t\t...${base}')
	c.out.writeln('\t\tkey: ${key}')
	c.write_common_fields(node, properties, scope, '${base}.accessibility_role', '${base}.accessibility_label', '${base}.accessibility_value')
	c.out.writeln('\t}')
}

fn (mut c VmlCompiler) compile_switch(node &VmlNode, suffix string, frame string, properties map[string]string, scope VmlScope, key string, action string) {
	base := 'vml_switch_${suffix}'
	style := '${base}_style'
	c.out.writeln('\t${style} := ui2.SwitchStyle{')
	c.out.writeln('\t\tinactive_track_color: ${vml_prop(properties, 'inactive_color', 'u32(0xcbd5e1)')}')
	c.out.writeln('\t\tactive_track_color: ${vml_value(properties, 'active_color', 'color', 'u32(0x22c55e)')}')
	c.out.writeln('\t\tthumb_color: ${vml_prop(properties, 'thumb_color', 'u32(0xffffff)')}')
	c.out.writeln('\t\tdisabled_track_color: ${vml_prop(properties, 'disabled_track_color', 'u32(0xe2e8f0)')}')
	c.out.writeln('\t\tdisabled_thumb_color: ${vml_prop(properties, 'disabled_thumb_color', 'u32(0xf8fafc)')}')
	c.out.writeln('\t}')
	c.out.writeln('\t${base} := ui2.switch_control(')
	c.out.writeln('\t\tid: ${vml_quote(node.id)}')
	c.out.writeln('\t\taction_id: ${action}')
	c.out.writeln('\t\tframe: ${frame}')
	c.out.writeln('\t\tactive: ${vml_value(properties, 'active', 'bind.active', 'false')}')
	c.out.writeln('\t\tstyle: ${style}')
	c.out.writeln('\t)')
	c.out.writeln('\tvml_element_${suffix} := ui2.Element{')
	c.out.writeln('\t\t...${base}')
	c.out.writeln('\t\tkey: ${key}')
	c.write_common_fields(node, properties, scope, '${base}.accessibility_role', '${base}.accessibility_label', '${base}.accessibility_value')
	c.out.writeln('\t}')
}

fn (mut c VmlCompiler) compile_spinner(node &VmlNode, suffix string, frame string, properties map[string]string, scope VmlScope, key string, action string) {
	base := 'vml_spinner_${suffix}'
	box := '${base}_box'
	text_style := '${base}_text_style'
	c.out.writeln('\t${box} := ${c.box_style(properties)}')
	c.out.writeln('\t${text_style} := ${c.text_style(properties)}')
	c.out.writeln('\t${base} := ui2.spinner(')
	c.out.writeln('\t\tid: ${vml_quote(node.id)}')
	c.out.writeln('\t\taction_id: ${action}')
	c.out.writeln('\t\tframe: ${frame}')
	c.out.writeln('\t\ttext: ${vml_value(properties, 'text', 'bind.text', "''")}')
	c.out.writeln('\t\tvalues: ${c.option_values(node, scope)}')
	c.out.writeln('\t\ttext_autoupdate: ${vml_prop(properties, 'text_autoupdate', 'false')}')
	c.out.writeln('\t\tbox: ${box}')
	c.out.writeln('\t\ttext_style: ${text_style}')
	c.out.writeln('\t)')
	c.out.writeln('\tvml_element_${suffix} := ui2.Element{')
	c.out.writeln('\t\t...${base}')
	c.out.writeln('\t\tkey: ${key}')
	c.write_common_fields(node, properties, scope, '${base}.accessibility_role', '${base}.accessibility_label', '${base}.accessibility_value')
	c.out.writeln('\t}')
}

fn (mut c VmlCompiler) compile_message_box(node &VmlNode, suffix string, frame string, properties map[string]string, scope VmlScope, key string, action string) {
	mut actions := []string{}
	for child_index, child in node.children {
		if child.tag != 'Button' {
			continue
		}
		c.write_action_type_checks(child, '${suffix}_message_action_${child_index}', scope)
		child_id := vml_quote(child.id)
		child_action := c.compiled_event_value(child, 'on_tap', scope, child_id)
		text := if property := vml_find_property(child, 'text') {
			c.expr(property.expr, scope, .string_)
		} else {
			"''"
		}
		actions << 'ui2.MessageBoxAction{id: ${vml_quote(child.id)}, action_id: ${child_action}, title: ${text}}'
	}
	c.out.writeln('\tvml_message_box_${suffix} := ui2.custom_message_box(')
	c.out.writeln('\t\tid: ${vml_quote(node.id)}')
	c.out.writeln('\t\tframe: ${frame}')
	c.out.writeln('\t\ttitle: ${vml_prop(properties, 'title', "''")}')
	c.out.writeln('\t\ttext: ${vml_prop(properties, 'text', "''")}')
	c.out.writeln('\t\thidden: ${vml_prop(properties, 'hidden', 'false')}')
	c.out.writeln('\t\twidth: ${vml_prop(properties, 'dialog_width', 'f64(300)')}')
	c.out.writeln('\t\theight: ${vml_prop(properties, 'dialog_height', 'f64(150)')}')
	c.out.writeln('\t\tactions: []ui2.MessageBoxAction{${actions.join(', ')}}')
	c.out.writeln('\t)')
	c.out.writeln('\tvml_element_${suffix} := ui2.Element{')
	c.out.writeln('\t\t...vml_message_box_${suffix}')
	c.out.writeln('\t\taction_id: ${action}')
	c.out.writeln('\t\tkey: ${key}')
	c.write_common_fields(node, properties, scope, "''", "''", "''")
	c.out.writeln('\t}')
}
