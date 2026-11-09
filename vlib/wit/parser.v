module wit

pub struct Parser {
pub:
	toks []Token
mut:
	idx int
}

pub fn new_parser(toks []Token) Parser {
	return Parser{ toks: toks, idx: 0 }
}

pub fn parse(src string) !WitDoc {
	mut l := new_lexer(src)
	toks := l.tokenize()!
	mut p := new_parser(toks)
	return p.parse_doc()
}

fn (mut p Parser) parse_doc() !WitDoc {
	mut doc := WitDoc{}
	// package
	if p.peek_ident() == 'package' {
		doc.package = p.parse_package()!
	}
	for p.peek_kind() != .eof {
		if p.peek_ident() == 'interface' {
			iface := p.parse_interface()!
			doc.interfaces << iface
		} else if p.peek_ident() == 'world' {
			w := p.parse_world(mut doc)!
			doc.worlds << w
		} else {
			return error('expected interface or world at ${p.pos()}')
		}
	}
	return doc
}

fn (p Parser) pos() int {
	if p.idx < p.toks.len { return p.toks[p.idx].pos }
	return -1
}

fn (mut p Parser) peek() Token {
	if p.idx < p.toks.len { return p.toks[p.idx] }
	return Token{ kind: .eof }
}

fn (mut p Parser) peek_kind() TokKind {
	return p.peek().kind
}

fn (mut p Parser) peek_ident() string {
	t := p.peek()
	if t.kind == .ident { return t.val }
	return ''
}

fn (mut p Parser) advance() {
	if p.idx < p.toks.len { p.idx++ }
}

fn (mut p Parser) expect_ident_val(v string) ! {
	t := p.peek()
	if t.kind != .ident || t.val != v {
		return error('expected identifier ${v} at ${t.pos}')
	}
	p.advance()
}

fn (mut p Parser) expect_ident() !string {
	t := p.peek()
	if t.kind != .ident {
		return error('expected identifier at ${t.pos}')
	}
	p.advance()
	return t.val
}

fn (mut p Parser) expect_punct(v string) ! {
	t := p.peek()
	if t.kind != .punct || t.val != v {
		return error('expected ${v} at ${t.pos}')
	}
	p.advance()
}

fn (mut p Parser) parse_package() !WitPackage {
	p.expect_ident_val('package')!
	mut pkg := WitPackage{}
	// namespace:name[/subpath]
	pkg.namespace = p.expect_ident()!
	p.expect_punct(':')!
	pkg.name = p.expect_ident()!
	// optional subpath
	for p.peek().kind == .punct && p.peek().val == '/' {
		p.advance()
		sub := p.expect_ident()!
		pkg.name += '/' + sub
	}
	// optional version
	if p.peek().kind == .punct && p.peek().val == '@' {
		p.advance()
		// consume version tokens until ;
		mut ver := ''
		for p.peek().kind != .punct || p.peek().val != ';' {
			if p.peek().kind == .eof { return error('unterminated package') }
			ver += p.peek().val
			p.advance()
		}
		pkg.version = ver
	}
	p.expect_punct(';')!
	return pkg
}

fn (mut p Parser) parse_interface() !WitInterface {
	p.expect_ident_val('interface')!
	name := p.expect_ident()!
	mut iface := WitInterface{ name: name }
	p.expect_punct('{')!
	for !(p.peek().kind == .punct && p.peek().val == '}') {
		if p.peek().kind == .eof { return error('unterminated interface') }
		item := p.parse_item()!
		iface.items << item
	}
	p.expect_punct('}')!
	return iface
}

fn (mut p Parser) parse_item() !WitItem {
	// use
	if p.peek_ident() == 'use' {
		u := p.parse_use()!
		return WitItem{ kind: .use, use_: u }
	}
	// type alias
	if p.peek_ident() == 'type' {
		td := p.parse_type_def()!
		return WitItem{ kind: .type_alias, type_def: td }
	}
	// record
	if p.peek_ident() == 'record' {
		r := p.parse_record()!
		return WitItem{ kind: .record, record: r }
	}
	// variant
	if p.peek_ident() == 'variant' {
		v := p.parse_variant()!
		return WitItem{ kind: .variant, variant: v }
	}
	// enum
	if p.peek_ident() == 'enum' {
		e := p.parse_enum()!
		return WitItem{ kind: .enum_, enum_: e }
	}
	// flags
	if p.peek_ident() == 'flags' {
		f := p.parse_flags()!
		return WitItem{ kind: .flags, flags: f }
	}
	// resource
	if p.peek_ident() == 'resource' {
		r := p.parse_resource()!
		return WitItem{ kind: .resource, resource: r }
	}
	// func item: name: func(...) or name: static func(...)
	if p.peek().kind == .ident && p.idx + 1 < p.toks.len && p.toks[p.idx + 1].val == ':' {
		f := p.parse_func_item()!
		return WitItem{ kind: .func, func: f }
	}
	return error('unexpected item at ${p.pos()}')
}

fn (mut p Parser) parse_use() !WitUse {
	p.expect_ident_val('use')!
	// path may contain / and :
	mut path := p.expect_ident()!
	for p.peek().kind == .punct && (p.peek().val == '/' || p.peek().val == ':') {
		// careful: ':' ends path? use foo.{bar}; path is before '{'
		if p.peek().val == ':' && p.idx + 1 < p.toks.len && p.toks[p.idx + 1].val == '{' {
			break
		}
		tok := p.peek().val
		p.advance()
		if tok == ':' {
			// maybe namespace:name, keep
			path += ':'
			path += p.expect_ident()!
		} else {
			path += '/' + p.expect_ident()!
		}
	}
	p.expect_punct('.')!
	p.expect_punct('{')!
	mut items := []string{}
	for !(p.peek().kind == .punct && p.peek().val == '}') {
		name := p.expect_ident()!
		items << name
		// optional alias
		if p.peek().kind == .ident && p.peek().val == 'as' {
			p.advance()
			alias := p.expect_ident()!
			items[items.len - 1] = name + ' as ' + alias
		}
		if p.peek().kind == .punct && p.peek().val == ',' { p.advance() }
	}
	p.expect_punct('}')!
	p.expect_punct(';')!
	return WitUse{ path: path, items: items }
}

fn (mut p Parser) parse_type_def() !WitTypeDef {
	p.expect_ident_val('type')!
	name := p.expect_ident()!
	p.expect_punct('=')!
	t := p.parse_type()!
	p.expect_punct(';')!
	return WitTypeDef{ name: name, typ: t }
}

fn (mut p Parser) parse_record() !WitRecord {
	p.expect_ident_val('record')!
	name := p.expect_ident()!
	mut r := WitRecord{ name: name }
	p.expect_punct('{')!
	for !(p.peek().kind == .punct && p.peek().val == '}') {
		fname := p.expect_ident()!
		p.expect_punct(':')!
		ft := p.parse_type()!
		r.fields << WitField{ name: fname, typ: ft }
		if p.peek().kind == .punct && p.peek().val == ',' { p.advance() }
		// allow trailing
		if p.peek().kind == .punct && p.peek().val == ';' { p.advance() }
	}
	p.expect_punct('}')!
	return r
}

fn (mut p Parser) parse_variant() !WitVariant {
	p.expect_ident_val('variant')!
	name := p.expect_ident()!
	mut v := WitVariant{ name: name }
	p.expect_punct('{')!
	for !(p.peek().kind == .punct && p.peek().val == '}') {
		cname := p.expect_ident()!
		mut ct := ?WitType(none)
		if p.peek().kind == .punct && p.peek().val == '(' {
			p.advance()
			t := p.parse_type()!
			ct = t
			p.expect_punct(')')!
		}
		v.cases << WitVariantCase{ name: cname, typ: ct }
		if p.peek().kind == .punct && (p.peek().val == ',' || p.peek().val == ';') { p.advance() }
	}
	p.expect_punct('}')!
	return v
}

fn (mut p Parser) parse_enum() !WitEnum {
	p.expect_ident_val('enum')!
	name := p.expect_ident()!
	mut e := WitEnum{ name: name }
	p.expect_punct('{')!
	for !(p.peek().kind == .punct && p.peek().val == '}') {
		cname := p.expect_ident()!
		e.cases << cname
		if p.peek().kind == .punct && (p.peek().val == ',' || p.peek().val == ';') { p.advance() }
	}
	p.expect_punct('}')!
	return e
}

fn (mut p Parser) parse_flags() !WitFlags {
	p.expect_ident_val('flags')!
	name := p.expect_ident()!
	mut f := WitFlags{ name: name }
	p.expect_punct('{')!
	for !(p.peek().kind == .punct && p.peek().val == '}') {
		fname := p.expect_ident()!
		f.flags << fname
		if p.peek().kind == .punct && (p.peek().val == ',' || p.peek().val == ';') { p.advance() }
	}
	p.expect_punct('}')!
	return f
}

fn (mut p Parser) parse_resource() !WitResource {
	p.expect_ident_val('resource')!
	name := p.expect_ident()!
	mut r := WitResource{ name: name }
	p.expect_punct('{')!
	for !(p.peek().kind == .punct && p.peek().val == '}') {
		if p.peek_ident() == 'constructor' {
			p.advance()
			p.expect_punct('(')!
			params := p.parse_param_list()!
			p.expect_punct(')')!
			f := WitFunc{ name: 'constructor', params: params, results: [] }
			r.methods << f
			if p.peek().kind == .punct && (p.peek().val == ';' || p.peek().val == ',') {
				p.advance()
			}
			continue
		}
		// method: name: [static] func(...)
		if p.peek().kind == .ident && p.idx + 1 < p.toks.len && p.toks[p.idx + 1].val == ':' {
			f := p.parse_func_item()!
			r.methods << f
			continue
		}
		return error('unexpected in resource at ${p.pos()}')
	}
	p.expect_punct('}')!
	return r
}

fn (mut p Parser) parse_func_item() !WitFunc {
	name := p.expect_ident()!
	p.expect_punct(':')!
	// optional static
	if p.peek_ident() == 'static' { p.advance() }
	if p.peek_ident() == 'func' { p.advance() }
	p.expect_punct('(')!
	params := p.parse_param_list()!
	p.expect_punct(')')!
	mut results := []WitType{}
	if p.peek().kind == .punct && p.peek().val == '->' {
		p.advance()
		results = p.parse_result_list()!
	}
	if p.peek().kind == .punct && (p.peek().val == ';' || p.peek().val == ',') { p.advance() }
	return WitFunc{ name: name, params: params, results: results }
}

fn (mut p Parser) parse_param_list() ![]WitParam {
	mut params := []WitParam{}
	if p.peek().kind == .punct && p.peek().val == ')' { return params }
	for {
		pname := p.expect_ident()!
		p.expect_punct(':')!
		pt := p.parse_type()!
		params << WitParam{ name: pname, typ: pt }
		if p.peek().kind == .punct && p.peek().val == ',' {
			p.advance()
			if p.peek().kind == .punct && p.peek().val == ')' { break }
			continue
		}
		break
	}
	return params
}

fn (mut p Parser) parse_result_list() ![]WitType {
	// single type or tuple in parens
	if p.peek().kind == .punct && p.peek().val == '(' {
		p.advance()
		mut items := []WitType{}
		if !(p.peek().kind == .punct && p.peek().val == ')') {
			items << p.parse_type()!
			for p.peek().kind == .punct && p.peek().val == ',' {
				p.advance()
				items << p.parse_type()!
			}
		}
		p.expect_punct(')')!
		return items
	}
	t := p.parse_type()!
	return [t]
}

fn (mut p Parser) parse_type() !WitType {
	if p.peek().kind == .punct && p.peek().val == '(' {
		p.advance()
		mut items := []WitType{}
		if !(p.peek().kind == .punct && p.peek().val == ')') {
			items << p.parse_type()!
			for p.peek().kind == .punct && p.peek().val == ',' {
				p.advance()
				items << p.parse_type()!
			}
		}
		p.expect_punct(')')!
		return WitType{ kind: .tuple, items: items }
	}
	if p.peek().kind != .ident {
		return error('expected type at ${p.pos()}')
	}
	name := p.peek().val
	p.advance()
	// generic
	if p.peek().kind == .punct && p.peek().val == '<' {
		p.advance()
		if name == 'list' {
			t := p.parse_type()!
			p.expect_punct('>')!
			return WitType{ kind: .list, items: [t] }
		}
		if name == 'option' {
			t := p.parse_type()!
			p.expect_punct('>')!
			return WitType{ kind: .option, items: [t] }
		}
		if name == 'result' {
			okt := p.parse_type()!
			mut errt := ?WitType(none)
			if p.peek().kind == .punct && p.peek().val == ',' {
				p.advance()
				et := p.parse_type()!
				errt = et
			}
			p.expect_punct('>')!
			mut its := [okt]
			if e := errt { its << e }
			return WitType{ kind: .result, items: its }
		}
		if name == 'tuple' {
			mut items := []WitType{}
			items << p.parse_type()!
			for p.peek().kind == .punct && p.peek().val == ',' {
				p.advance()
				items << p.parse_type()!
			}
			p.expect_punct('>')!
			return WitType{ kind: .tuple, items: items }
		}
		// own/borrow -> named
		t := p.parse_type()!
		p.expect_punct('>')!
		return WitType{ kind: .named, name: '${name}<${t.str()}>' }
	}
	if is_prim_name(name) {
		return prim_type(name)
	}
	return named_type(name)
}

fn (mut p Parser) parse_world(mut doc WitDoc) !WitWorld {
	p.expect_ident_val('world')!
	name := p.expect_ident()!
	mut w := WitWorld{ name: name }
	p.expect_punct('{')!
	for !(p.peek().kind == .punct && p.peek().val == '}') {
		if p.peek().kind == .eof { return error('unterminated world') }
		kind := p.peek_ident()
		if kind == 'import' || kind == 'export' {
			is_export := kind == 'export'
			p.advance()
			iname := p.expect_ident()!
			mut iface := ''
			mut f := ?WitFunc(none)
			if p.peek().kind == .punct && p.peek().val == ':' {
				p.advance()
				if p.peek_ident() == 'interface' {
					p.advance()
					inline := p.parse_inline_interface(mut doc, '${name}_${iname}')!
					iface = inline.name
				} else if p.peek_ident() == 'func' {
					p.advance()
					p.expect_punct('(')!
					params := p.parse_param_list()!
					p.expect_punct(')')!
					mut results := []WitType{}
					if p.peek().kind == .punct && p.peek().val == '->' {
						p.advance()
						results = p.parse_result_list()!
					}
					f = WitFunc{ name: iname, params: params, results: results }
					if p.peek().kind == .punct && (p.peek().val == ';' || p.peek().val == ',') {
						p.advance()
					}
				} else {
					return error('expected interface or func after : at ${p.pos()}')
				}
			} else {
				iface = iname
				if p.peek().kind == .punct && (p.peek().val == ';' || p.peek().val == ',') {
					p.advance()
				}
			}
			if is_export {
				w.exports << WitWorldExport{ name: iname, interface: iface, func: f }
			} else {
				w.imports << WitWorldImport{ name: iname, interface: iface, func: f }
			}
			continue
		}
		if kind == 'include' {
			p.advance()
			_ := p.expect_ident()!
			if p.peek().kind == .punct && (p.peek().val == ';' || p.peek().val == ',') {
				p.advance()
			}
			continue
		}
		return error('unexpected in world at ${p.pos()}')
	}
	p.expect_punct('}')!
	return w
}

fn (mut p Parser) parse_inline_interface(mut doc WitDoc, name string) !WitInterface {
	p.expect_punct('{')!
	mut iface := WitInterface{ name: name }
	for !(p.peek().kind == .punct && p.peek().val == '}') {
		item := p.parse_item()!
		iface.items << item
	}
	p.expect_punct('}')!
	doc.interfaces << iface
	return iface
}
