module parser

import v.ast
import v.token
import v.util
import strings

fn (mut p Parser) enum_val_expr(mod string) ast.EnumVal {
	// `Color.green`
	mut enum_name := p.check_name()
	enum_name_pos := p.prev_tok.pos()
	if mod != '' {
		enum_name = mod + '.' + enum_name
	} else {
		enum_name = p.imported_symbols[enum_name] or { p.prepend_mod(enum_name) }
	}
	p.check(.dot)
	val := p.check_name()
	p.expr_mod = ''
	p.last_enum_name = enum_name
	p.last_enum_mod = mod
	return ast.EnumVal{
		enum_name: enum_name
		val:       val
		pos:       enum_name_pos.extend(p.prev_tok.pos())
		mod:       mod
	}
}

// `.green`
// `pref.BuildMode.default_mode`
fn (mut p Parser) enum_val() ast.EnumVal {
	start_pos := p.tok.pos()
	p.check(.dot)
	val := p.check_name()
	return ast.EnumVal{
		val: val
		pos: start_pos.extend(p.prev_tok.pos())
	}
}

fn (mut p Parser) enum_decl() ast.EnumDecl {
	p.top_level_statement_start()
	is_pub := p.tok.kind == .key_pub
	start_pos := p.tok.pos()
	if is_pub {
		p.next()
	}
	p.check(.key_enum)
	end_pos := p.tok.pos()
	if p.disallow_declarations_in_script_mode() {
		return ast.EnumDecl{}
	}
	enum_name := p.check_name()
	if enum_name.len == 0 {
		p.error_with_pos('enum names can not be empty', end_pos)
		return ast.EnumDecl{}
	}
	if enum_name.len == 1 {
		p.error_with_pos('single letter capital names are reserved for generic template types.',
			end_pos)
		return ast.EnumDecl{}
	}
	if enum_name in p.imported_symbols {
		p.error_with_pos('cannot register enum `${enum_name}`, this type was already imported',
			end_pos)
		return ast.EnumDecl{}
	}
	name := p.prepend_mod(enum_name)
	already_exists := if _ := p.table.enum_decls[name] { true } else { false }
	mut enum_type := ast.int_type
	mut typ_pos := token.Pos{}
	if p.tok.kind == .key_as {
		p.next()
		typ_pos = p.tok.pos()
		enum_type = p.parse_type()
	}
	mut enum_decl_comments := p.eat_comments()
	p.check(.lcbr)
	enum_decl_comments << p.eat_comments()
	senum_type := p.table.get_type_name(enum_type)
	mut vals := []string{}
	// mut default_exprs := []ast.Expr{}
	mut fields := []ast.EnumField{}
	mut uses_exprs := false
	mut enum_attrs := map[string][]ast.Attr{}
	for p.tok.kind != .eof && p.tok.kind != .rcbr {
		pre_comments := p.eat_comments()
		pos := p.tok.pos()
		has_prev_newline := p.has_prev_newline()
		has_break_line := has_prev_newline || p.has_prev_line_comment_or_label()
		val := p.check_name()
		vals << val
		mut expr := ast.empty_expr
		mut has_expr := false
		// p.warn('enum val $val')
		if p.tok.kind == .assign {
			p.next()
			expr = p.expr(0)
			has_expr = true
			uses_exprs = true
		}
		mut attrs := []ast.Attr{}
		if p.tok.kind == .lsbr || p.tok.kind == .at {
			p.attributes()
			attrs << p.attrs
			enum_attrs[val] = attrs
			p.attrs = []
		}
		comments := p.eat_comments(same_line: true)
		next_comments := p.eat_comments(follow_up: true)
		fields << ast.EnumField{
			name:             val
			source_name:      source_name(val)
			pos:              pos
			expr:             expr
			has_expr:         has_expr
			has_prev_newline: has_prev_newline
			has_break_line:   has_break_line
			pre_comments:     pre_comments
			comments:         comments
			next_comments:    next_comments
			attrs:            attrs
		}
	}
	p.top_level_statement_end()
	p.check(.rcbr)
	is_flag := p.attrs.contains('flag')
	is_multi_allowed := p.attrs.contains('_allow_multiple_values')
	pubfn := if p.mod == 'main' { '@[flag_enum_fn] fn' } else { '@[flag_enum_fn] pub fn' }
	if is_flag {
		if fields.len > 64 {
			p.error('when an enum is used as bit field, it must have a max of 64 fields')
			return ast.EnumDecl{}
		}
		for f in fields {
			if f.has_expr {
				p.error_with_pos('when an enum is used as a bit field, you can not assign custom values',
					f.pos)
				return ast.EnumDecl{}
			}
		}
		if !already_exists {
			// enum already exists, skip method creation to avoid duplicate method errors
			all_bits_set_value := '0b' + '1'.repeat(fields.len)
			p.codegen('
//
@[inline] ${pubfn} (    e &${enum_name}) is_empty() bool           { return  ${senum_type}(*e) == 0 }
@[inline] ${pubfn} (    e &${enum_name}) has(flag_ ${enum_name}) bool { return  (${senum_type}(*e) &  (${senum_type}(flag_))) != 0 }
@[inline] ${pubfn} (    e &${enum_name}) all(flag_ ${enum_name}) bool { return  (${senum_type}(*e) &  (${senum_type}(flag_))) == ${senum_type}(flag_) }
@[inline] ${pubfn} (mut e  ${enum_name}) set(flag_ ${enum_name})      { unsafe{ *e = ${enum_name}(${senum_type}(*e) |  (${senum_type}(flag_))) } }
@[inline] ${pubfn} (mut e  ${enum_name}) set_all()                   { unsafe{ *e = ${enum_name}(${all_bits_set_value}) } }
@[inline] ${pubfn} (mut e  ${enum_name}) clear(flag_ ${enum_name})    { unsafe{ *e = ${enum_name}(${senum_type}(*e) & ~(${senum_type}(flag_))) } }
@[inline] ${pubfn} (mut e  ${enum_name}) clear_all()                 { unsafe{ *e = ${enum_name}(0) } }
@[inline] ${pubfn} (mut e  ${enum_name}) toggle(flag_ ${enum_name})   { unsafe{ *e = ${enum_name}(${senum_type}(*e) ^  (${senum_type}(flag_))) } }
//
')
		}
	}
	// Add the generic `Enum.from[T](x T) !T {` static method too:
	mut isb := strings.new_builder(1024)
	isb.write_string('\n')
	if is_flag {
		isb.write_string('@[inline] ${pubfn} ${enum_name}.zero() ${enum_name} {\n')
		isb.write_string('		return unsafe{ ${enum_name}(0) }\n')
		isb.write_string('}\n')
	}
	// TODO: see why changing `W` to `T` below, later fails `v vlib/vweb/tests/middleware_test_server.v` with seemingly unrelated error
	isb.write_string('${pubfn} ${enum_name}.from[W](input W) !${enum_name} {\n')
	isb.write_string('	\$if input is \$int {\n')
	isb.write_string('		val := unsafe{ ${enum_name}(input) }\n')
	if is_flag {
		isb.write_string('		if input == 0 { return val }\n')
		all_bits_set_value := '0b' + '1'.repeat(fields.len)
		isb.write_string('		if input & ~${all_bits_set_value} == 0 { return val }\n')
	} else {
		isb.write_string('		match val {\n')
		for f in fields {
			isb.write_string('			.${f.source_name} { return ${enum_name}.${f.source_name} }\n')
		}
		if is_flag {
			isb.write_string('			else{}\n')
		}
		isb.write_string('		}\n')
	}
	isb.write_string('	}\n')
	isb.write_string('	\$if input is \$string {\n')
	isb.write_string('		val := input.str()\n') // TODO: this should not be needed, the `$if input is $string` above should have already smartcasted `input`
	if is_flag {
		isb.write_string('		if val == \'\' { return unsafe{ ${enum_name}(0) } }\n')
	}
	isb.write_string('		match val {\n')
	for f in fields {
		isb.write_string('			\'${f.name}\' { return ${enum_name}.${f.source_name} }\n')
	}
	isb.write_string('			else{}\n')
	isb.write_string('		}\n')
	isb.write_string('	}\n')
	isb.write_string("	return error('invalid value')\n")
	isb.write_string('}\n')
	isb.write_string('\n')
	code_for_from_fn := isb.str()
	$if debug_enumcodegen ? {
		if p.mod == 'main' {
			dump(code_for_from_fn)
		}
	}
	if enum_name[0].is_capital() && fields.len > 0 {
		p.codegen(code_for_from_fn)
	}

	idx := p.table.register_sym(ast.TypeSymbol{
		kind:   .enum
		name:   name
		cname:  util.no_dots(name)
		mod:    p.mod
		info:   ast.Enum{
			vals:             vals
			is_flag:          is_flag
			is_multi_allowed: is_multi_allowed
			uses_exprs:       uses_exprs
			typ:              enum_type
			attrs:            enum_attrs
		}
		is_pub: is_pub
	})

	if idx in [ast.string_type_idx, ast.rune_type_idx, ast.array_type_idx, ast.map_type_idx] {
		p.error_with_pos('cannot register enum `${name}`, another type with this name exists',
			end_pos)
	}

	if idx == ast.invalid_type_idx {
		enum_type = idx
	}

	enum_decl := ast.EnumDecl{
		name:             name
		typ:              enum_type
		typ_pos:          typ_pos
		is_pub:           is_pub
		is_flag:          is_flag
		is_multi_allowed: is_multi_allowed
		fields:           fields
		pos:              start_pos.extend_with_last_line(end_pos, p.prev_tok.line_nr)
		attrs:            p.attrs
		comments:         enum_decl_comments
	}

	if !already_exists {
		p.table.register_enum_decl(enum_decl)
	}
	return enum_decl
}
