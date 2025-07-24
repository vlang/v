// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module parser

import v.ast
import v.token
import v.util

fn (mut p Parser) struct_decl(is_anon bool) ast.StructDecl {
	p.top_level_statement_start()
	// save attributes, they will be changed later in fields
	attrs := p.attrs
	start_pos := p.tok.pos()
	mut is_pub := p.tok.kind == .key_pub
	mut is_shared := p.tok.kind == .key_shared
	is_option := is_anon && p.prev_tok.kind == .question
	if is_pub {
		p.next()
	}
	if is_anon {
		if is_shared {
			p.register_auto_import('sync')
			p.next()
		}
		is_pub = true
	}
	is_union := p.tok.kind == .key_union
	if p.tok.kind == .key_struct {
		p.next()
	} else {
		p.check(.key_union)
	}
	language := p.parse_language()
	name_pos := p.tok.pos()
	p.check_for_impure_v(language, name_pos)
	if p.disallow_declarations_in_script_mode() {
		return ast.StructDecl{}
	}
	mut name := if is_anon {
		if is_union {
			p.table.anon_union_counter++
			'_VAnonUnion${p.table.anon_union_counter}'
		} else {
			p.table.anon_struct_counter++
			'_VAnonStruct${p.table.anon_struct_counter}'
		}
	} else {
		p.check_name()
	}
	if name.len == 1 && name[0].is_capital() {
		p.error_with_pos('single letter capital names are reserved for generic template types.',
			name_pos)
		return ast.StructDecl{}
	}
	if name == 'IError' && p.mod != 'builtin' {
		p.error_with_pos('cannot register struct `IError`, it is builtin interface type',
			name_pos)
	}
	// append module name before any type of parsing to enable recursion parsing
	p.table.start_parsing_type(p.prepend_mod(name))
	defer {
		p.table.reset_parsing_type()
	}
	generic_types, _ := p.parse_generic_types()
	mut pre_comments := p.eat_comments()
	no_body := p.tok.kind != .lcbr && p.tok.kind != .key_implements
	if language == .v && no_body {
		p.error_with_pos('`${p.tok.lit}` lacks body', name_pos)
		return ast.StructDecl{}
	}
	if name.len == 1 {
		p.error_with_pos('struct names must have more than one character', name_pos)
		return ast.StructDecl{}
	}
	if name in p.imported_symbols {
		p.error_with_pos('cannot register struct `${name}`, this type was already imported',
			name_pos)
		return ast.StructDecl{}
	}
	mut orig_name := name
	if language == .c {
		name = 'C.${name}'
		orig_name = name
	} else if language == .js {
		name = 'JS.${name}'
		orig_name = name
	} else if language == .wasm {
		name = 'WASM.${name}'
		orig_name = name
	} else {
		name = p.prepend_mod(name)
	}
	mut ast_fields := []ast.StructField{}
	mut fields := []ast.StructField{}
	mut embed_types := []ast.Type{}
	mut embeds := []ast.Embed{}
	mut embed_field_names := []string{}
	mut mut_pos := -1
	mut pub_pos := -1
	mut pub_mut_pos := -1
	mut global_pos := -1
	mut module_pos := -1
	mut is_field_mut := language == .c
	mut is_field_pub := language == .c
	mut is_field_global := false
	mut is_implements := false
	mut implements_types := []ast.TypeNode{cap: 3} // ast.void_type
	mut last_line := p.prev_tok.pos().line_nr + 1
	mut end_comments := []ast.Comment{}
	mut has_option := false
	if !no_body {
		if p.tok.kind == .key_implements {
			is_implements = true
			for {
				p.next()
				type_pos := p.tok.pos()
				implements_types << ast.TypeNode{
					typ: p.parse_type()
					pos: type_pos
				}
				if p.tok.kind != .comma {
					break
				}
			}
		}
		p.check(.lcbr)
		pre_comments << p.eat_comments()
		mut i := 0
		for p.tok.kind != .rcbr {
			mut comments := []ast.Comment{}
			if p.tok.kind == .rcbr {
				end_comments = p.eat_comments(same_line: true)
				break
			}
			if p.tok.kind == .key_pub && p.peek_tok.kind in [.key_mut, .colon] {
				p.next()
				if p.tok.kind == .key_mut {
					if pub_mut_pos != -1 {
						p.error('redefinition of `pub mut` section')
						return ast.StructDecl{}
					}
					p.next()
					pub_mut_pos = ast_fields.len
					is_field_pub = true
					is_field_mut = true
					is_field_global = false
				} else {
					if pub_pos != -1 {
						p.error('redefinition of `pub` section')
						return ast.StructDecl{}
					}
					pub_pos = ast_fields.len
					is_field_pub = true
					is_field_mut = false
					is_field_global = false
				}
				p.check(.colon)
			} else if p.tok.kind == .key_mut && p.peek_tok.kind == .colon {
				if mut_pos != -1 {
					p.error('redefinition of `mut` section')
					return ast.StructDecl{}
				}
				p.next()
				p.check(.colon)
				mut_pos = ast_fields.len
				is_field_pub = false
				is_field_mut = true
				is_field_global = false
			} else if p.tok.kind == .key_global && p.peek_tok.kind == .colon {
				if global_pos != -1 {
					p.error('redefinition of `global` section')
					return ast.StructDecl{}
				}
				p.next()
				p.check(.colon)
				global_pos = ast_fields.len
				is_field_pub = true
				is_field_mut = true
				is_field_global = true
			} else if p.tok.kind == .key_module && p.peek_tok.kind == .colon {
				if module_pos != -1 {
					p.error('redefinition of `module` section')
					return ast.StructDecl{}
				}
				p.next()
				p.check(.colon)
				module_pos = ast_fields.len
				is_field_pub = false
				is_field_mut = false
				is_field_global = false
			}
			pre_field_comments := p.eat_comments()
			mut next_field_comments := []ast.Comment{}
			field_start_pos := p.tok.pos()
			mut is_field_volatile := false
			mut is_field_deprecated := false
			if p.tok.kind == .key_volatile && p.peek_token(2).line_nr == p.tok.line_nr {
				p.next()
				is_field_volatile = true
			}
			is_embed := ((p.tok.lit.len > 1 && p.tok.lit[0].is_capital()
				&& (p.peek_tok.line_nr != p.tok.line_nr || p.peek_tok.kind !in [.name, .amp])
				&& (p.peek_tok.kind != .lsbr || p.peek_token(2).kind != .rsbr))
				|| p.peek_tok.kind == .dot) && language == .v && p.peek_tok.kind != .key_fn
			is_on_top := ast_fields.len == 0 && !(is_field_pub || is_field_mut || is_field_global)
			has_prev_newline := p.has_prev_newline()
			has_break_line := has_prev_newline || p.has_prev_line_comment_or_label()
			mut field_name := ''
			mut typ := ast.no_type
			mut type_pos := token.Pos{}
			mut field_pos := token.Pos{}
			mut option_pos := token.Pos{}

			if p.tok.kind == .rcbr {
				if ast_fields.len > 0 {
					ast_fields.last().next_comments << pre_field_comments
				}
				break
			}

			if is_embed {
				if p.peek_tok.kind == .dot && p.peek_tok.line_nr == p.peek_token(3).line_nr
					&& p.peek_token(3).kind == .name {
					p.error_with_pos('invalid field name', p.tok.pos())
					return ast.StructDecl{}
				}
				// struct embedding
				type_pos = p.tok.pos()
				typ = p.parse_type()
				comments << p.eat_comments()
				type_pos = type_pos.extend(p.prev_tok.pos())
				if !is_on_top {
					p.error_with_pos('struct embedding must be declared at the beginning of the struct body',
						type_pos)
					return ast.StructDecl{}
				}
				sym := p.table.sym(typ)
				if typ in embed_types {
					p.error_with_pos('cannot embed `${sym.name}` more than once', type_pos)
					return ast.StructDecl{}
				}
				field_name = sym.embed_name()
				if field_name in embed_field_names {
					p.error_with_pos('duplicate field `${field_name}`', type_pos)
					return ast.StructDecl{}
				}
				if p.tok.kind == .lsbr {
					p.error('cannot use attributes on embedded structs')
				}
				embed_field_names << field_name
				embed_types << typ
				embeds << ast.Embed{
					typ:      typ
					pos:      type_pos
					comments: comments
				}
			} else {
				// struct field
				field_name = p.check_name()
				p.inside_struct_field_decl = true
				is_anon_struct := p.tok.kind == .key_struct
					|| (p.tok.kind == .key_shared && p.peek_tok.kind == .key_struct)
				is_anon_union := p.tok.kind == .key_union
					|| (p.tok.kind == .key_shared && p.peek_tok.kind == .key_union)
				if is_anon_struct || is_anon_union {
					// Anon structs
					field_is_shared := p.tok.kind == .key_shared
					p.anon_struct_decl = p.struct_decl(true)
					p.anon_struct_decl.language = language
					// Find the registered anon struct type, it was registered above in `p.struct_decl()`
					typ = p.table.find_type_idx(p.anon_struct_decl.name)
					if field_is_shared {
						typ = typ.set_flag(.shared_f)
						typ = typ.set_nr_muls(1)
					}
				} else {
					start_type_pos := p.tok.pos()
					typ = p.parse_type()
					type_pos = start_type_pos.extend(p.prev_tok.pos())
				}
				p.inside_struct_field_decl = false
				if typ.idx() == 0 {
					// error is set in parse_type
					return ast.StructDecl{}
				}
				field_pos = field_start_pos.extend(p.prev_tok.pos())
				if typ.has_option_or_result() {
					option_pos = p.peek_token(-2).pos()
					has_option = true
				}
			}
			// Comments after type (same line)
			prev_attrs := p.attrs
			p.attrs = []
			// TODO: remove once old syntax is no longer supported
			if p.tok.kind == .lsbr {
				p.inside_struct_attr_decl = true
				// attrs are stored in `p.attrs`
				p.attributes()
				for fa in p.attrs {
					if fa.name == 'deprecated' {
						is_field_deprecated = true
					}
				}
				p.inside_struct_attr_decl = false
			}
			comments << p.eat_comments(same_line: true)
			mut default_expr := ast.empty_expr
			mut has_default_expr := false
			if !is_embed {
				if p.tok.kind == .assign {
					// Default value
					p.next()
					default_expr = p.expr(0)
					match mut default_expr {
						ast.EnumVal { default_expr.typ = typ }
						// TODO: implement all types??
						else {}
					}
					has_default_expr = true
					comments << p.eat_comments(same_line: true)
				}
				if p.tok.kind == .at {
					p.inside_struct_attr_decl = true
					// attrs are stored in `p.attrs`
					p.attributes()
					for fa in p.attrs {
						if fa.name == 'deprecated' {
							is_field_deprecated = true
						}
					}
					p.inside_struct_attr_decl = false
					comments << p.eat_comments(same_line: true)
				}
				next_field_comments = p.eat_comments(follow_up: true)
				ast_fields << ast.StructField{
					name:             field_name
					typ:              typ
					pos:              field_pos
					type_pos:         type_pos
					option_pos:       option_pos
					pre_comments:     pre_field_comments
					comments:         comments
					next_comments:    next_field_comments
					i:                i
					default_expr:     default_expr
					has_default_expr: has_default_expr
					has_prev_newline: has_prev_newline
					has_break_line:   has_break_line
					attrs:            p.attrs
					is_pub:           is_embed || is_field_pub
					is_mut:           is_embed || is_field_mut
					is_global:        is_field_global
					is_volatile:      is_field_volatile
					is_deprecated:    is_field_deprecated
					anon_struct_decl: p.anon_struct_decl
				}
			}
			// save embeds as table fields too, it will be used in generation phase
			fields << ast.StructField{
				name:             field_name
				typ:              typ
				pos:              field_pos
				type_pos:         type_pos
				option_pos:       option_pos
				pre_comments:     pre_field_comments
				comments:         comments
				next_comments:    next_field_comments
				i:                i
				default_expr:     default_expr
				has_default_expr: has_default_expr
				attrs:            p.attrs
				is_pub:           is_embed || is_field_pub
				is_mut:           is_embed || is_field_mut
				is_embed:         is_embed
				is_global:        is_field_global
				is_volatile:      is_field_volatile
				is_deprecated:    is_field_deprecated
				anon_struct_decl: p.anon_struct_decl
			}
			p.anon_struct_decl = ast.StructDecl{}
			p.attrs = prev_attrs
			i++
		}
		p.top_level_statement_end()
		last_line = p.tok.line_nr
		p.check(.rcbr)
		end_comments = p.eat_comments(same_line: true)
	}
	mut scoped_name := ''
	if !is_anon && p.inside_fn && p.cur_fn_scope != unsafe { nil } {
		scoped_name = '_${name}_${p.cur_fn_scope.start_pos}'
	}
	is_minify := attrs.contains('minify')
	mut sym := ast.TypeSymbol{
		kind:       .struct
		language:   language
		name:       name
		cname:      util.no_dots(name)
		mod:        p.mod
		info:       ast.Struct{
			scoped_name:   scoped_name
			embeds:        embed_types
			fields:        fields
			is_typedef:    attrs.contains('typedef')
			is_union:      is_union
			is_heap:       attrs.contains('heap')
			is_markused:   attrs.contains('markused')
			is_minify:     is_minify
			is_generic:    generic_types.len > 0
			generic_types: generic_types
			attrs:         attrs
			is_anon:       is_anon
			is_shared:     is_shared
			has_option:    has_option
		}
		is_pub:     is_pub
		is_builtin: name in ast.builtins
	}
	if p.table.has_deep_child_no_ref(&sym, name) {
		p.error_with_pos('invalid recursive struct `${orig_name}`', name_pos)
		return ast.StructDecl{}
	}
	mut ret := p.table.register_sym(sym)
	if is_anon {
		if is_union {
			p.table.register_anon_union(name, ret)
		} else {
			p.table.register_anon_struct(name, ret)
		}
	}
	// allow duplicate c struct declarations
	if ret == -1 && language != .c {
		p.error_with_pos('cannot register struct `${name}`, another type with this name exists',
			name_pos)
		return ast.StructDecl{}
	}
	p.expr_mod = ''
	return ast.StructDecl{
		name:             name
		scoped_name:      scoped_name
		is_pub:           is_pub
		fields:           ast_fields
		pos:              start_pos.extend_with_last_line(name_pos, last_line)
		mut_pos:          mut_pos
		pub_pos:          pub_pos
		pub_mut_pos:      pub_mut_pos
		global_pos:       global_pos
		module_pos:       module_pos
		language:         language
		is_union:         is_union
		is_option:        is_option
		attrs:            if is_anon { []ast.Attr{} } else { attrs } // anon structs can't have attributes
		pre_comments:     pre_comments
		end_comments:     end_comments
		generic_types:    generic_types
		embeds:           embeds
		is_implements:    is_implements
		implements_types: implements_types
	}
}

fn (mut p Parser) struct_init(typ_str string, kind ast.StructInitKind, is_option bool) ast.StructInit {
	first_pos := (if kind == .short_syntax && p.prev_tok.kind == .lcbr { p.prev_tok } else { p.tok }).pos()
	p.init_generic_types = []ast.Type{}
	mut typ := if kind == .short_syntax { ast.void_type } else { p.parse_type() }
	struct_init_generic_types := p.init_generic_types.clone()
	if is_option {
		typ = typ.set_flag(.option)
	}
	p.expr_mod = ''
	if kind != .short_syntax {
		p.check(.lcbr)
	}
	pre_comments := p.eat_comments()
	mut init_fields := []ast.StructInitField{}
	mut i := 0
	no_keys := p.peek_tok.kind != .colon && p.tok.kind != .rcbr && p.tok.kind != .ellipsis // `Vec{a,b,c}
	saved_is_amp := p.is_amp
	p.is_amp = false
	mut update_expr := ast.empty_expr
	mut update_expr_comments := []ast.Comment{}
	mut has_update_expr := false
	mut update_expr_pos := token.Pos{}
	mut has_prev_newline := false
	mut has_break_line := false
	for p.tok.kind !in [.rcbr, .rpar, .eof] {
		mut field_name := ''
		mut expr := ast.empty_expr
		mut field_pos := token.Pos{}
		mut first_field_pos := token.Pos{}
		mut prev_comments := []ast.Comment{}
		mut end_comments := []ast.Comment{}
		mut nline_comments := []ast.Comment{}
		is_update_expr := init_fields.len == 0 && p.tok.kind == .ellipsis
		if no_keys {
			// name will be set later in checker
			expr = p.expr(0)
			field_pos = expr.pos()
			first_field_pos = field_pos
			end_comments = p.eat_comments(same_line: true)
		} else if is_update_expr {
			// struct updating syntax; f2 := Foo{ ...f, name: 'f2' }
			update_expr_pos = p.tok.pos()
			p.check(.ellipsis)
			update_expr = p.expr(0)
			update_expr_comments << p.eat_comments(same_line: true)
			has_update_expr = true
		} else {
			prev_comments = p.eat_comments()
			first_field_pos = p.tok.pos()
			has_prev_newline = p.has_prev_newline()
			has_break_line = has_prev_newline || p.has_prev_line_comment_or_label()
			field_name = p.check_name()
			if p.is_vls {
				// In VLS mode allow unfinished struct inits without the ending }
				// `Foo{
				//  field: name.`

				if p.tok.kind != .colon {
					unsafe {
						goto end
					}
				}
			}
			p.check(.colon)
			expr = p.expr(0)
			end_comments = p.eat_comments(same_line: true)
			last_field_pos := expr.pos()
			field_len := if last_field_pos.len > 0 {
				last_field_pos.pos - first_field_pos.pos + last_field_pos.len
			} else {
				first_field_pos.len + 1
			}
			field_pos = token.Pos{
				line_nr: first_field_pos.line_nr
				pos:     first_field_pos.pos
				len:     field_len
				col:     first_field_pos.col
			}
		}
		i++
		if p.tok.kind == .comma {
			p.next()
		}
		end_comments << p.eat_comments(same_line: true)
		nline_comments << p.eat_comments(follow_up: true)
		if !is_update_expr {
			init_fields << ast.StructInitField{
				name:             field_name
				expr:             expr
				pos:              field_pos
				name_pos:         first_field_pos
				pre_comments:     prev_comments
				end_comments:     end_comments
				next_comments:    nline_comments
				parent_type:      typ
				has_prev_newline: has_prev_newline
				has_break_line:   has_break_line
				is_embed:         field_name.len > 0 && field_name[0].is_capital()
			}
		}
	}
	if kind != .short_syntax {
		p.check(.rcbr)
	}
	p.is_amp = saved_is_amp
	end:
	return ast.StructInit{
		unresolved:           typ.has_flag(.generic)
		typ_str:              typ_str
		typ:                  typ
		init_fields:          init_fields
		update_expr:          update_expr
		update_expr_pos:      update_expr_pos
		update_expr_comments: update_expr_comments
		has_update_expr:      has_update_expr
		name_pos:             first_pos
		pos:                  first_pos.extend(if kind == .short_syntax {
			p.tok.pos()
		} else {
			p.prev_tok.pos()
		})
		no_keys:              no_keys
		is_short_syntax:      kind == .short_syntax
		is_anon:              kind == .anon
		pre_comments:         pre_comments
		generic_types:        struct_init_generic_types
	}
}

fn (mut p Parser) interface_decl() ast.InterfaceDecl {
	p.top_level_statement_start()
	mut pos := p.tok.pos()
	attrs := p.attrs
	is_pub := p.tok.kind == .key_pub
	if is_pub {
		p.next()
	}
	p.next() // `interface`
	language := p.parse_language()
	name_pos := p.tok.pos()
	p.check_for_impure_v(language, name_pos)
	if p.disallow_declarations_in_script_mode() {
		return ast.InterfaceDecl{}
	}
	modless_name := p.check_name()
	if modless_name.len == 1 && modless_name[0].is_capital() {
		p.error_with_pos('single letter capital names are reserved for generic template types.',
			name_pos)
		return ast.InterfaceDecl{}
	}
	if modless_name == 'IError' && p.mod != 'builtin' {
		p.error_with_pos('cannot register interface `IError`, it is builtin interface type',
			name_pos)
	}
	mut interface_name := ''
	if language == .js {
		interface_name = 'JS.' + modless_name
	} else {
		interface_name = p.prepend_mod(modless_name)
	}
	generic_types, _ := p.parse_generic_types()
	mut pre_comments := p.eat_comments()
	p.check(.lcbr)
	pre_comments << p.eat_comments()
	if modless_name in p.imported_symbols {
		p.error_with_pos('cannot register interface `${interface_name}`, this type was already imported',
			name_pos)
		return ast.InterfaceDecl{}
	}
	// Declare the type
	reg_idx := p.table.register_sym(
		is_pub:   is_pub
		kind:     .interface
		name:     interface_name
		cname:    util.no_dots(interface_name)
		mod:      p.mod
		info:     ast.Interface{
			types:         []
			is_generic:    generic_types.len > 0
			is_markused:   attrs.contains('markused')
			generic_types: generic_types
		}
		language: language
	)
	if reg_idx == -1 {
		p.error_with_pos('cannot register interface `${interface_name}`, another type with this name exists',
			name_pos)
		return ast.InterfaceDecl{}
	}
	typ := ast.new_type(reg_idx)
	mut ts := p.table.sym(typ)
	mut info := ts.info as ast.Interface
	// if methods were declared before, it's an error, ignore them
	ts.methods = []ast.Fn{cap: 20}
	// Parse fields or methods
	mut fields := []ast.StructField{cap: 20}
	mut methods := []ast.FnDecl{cap: 20}
	mut embeds := []ast.InterfaceEmbedding{}
	mut is_mut := false
	mut mut_pos := -1
	for p.tok.kind != .rcbr && p.tok.kind != .eof {
		// check embedded interface from internal module
		if p.tok.kind == .name && p.tok.lit.len > 0 && p.tok.lit[0].is_capital()
			&& (p.peek_tok.line_nr != p.tok.line_nr
			|| p.peek_tok.kind !in [.name, .amp, .lsbr, .lpar]
			|| (p.peek_tok.kind == .lsbr && p.peek_tok.is_next_to(p.tok))) {
			iface_pos := p.tok.pos()
			mut iface_name := p.tok.lit
			iface_type := p.parse_type()
			if iface_name == 'JS' {
				iface_name = p.table.sym(iface_type).name
			}
			comments := p.eat_comments()
			embeds << ast.InterfaceEmbedding{
				name:     iface_name
				typ:      iface_type
				pos:      iface_pos
				comments: comments
			}
			if p.tok.kind == .rcbr {
				break
			}
			continue
		}
		// check embedded interface from external module
		if p.tok.kind == .name && p.peek_tok.kind == .dot {
			if p.tok.lit !in p.imports {
				p.error_with_pos('mod `${p.tok.lit}` not imported', p.tok.pos())
				break
			}
			mod_name := p.tok.lit
			from_mod_typ := p.parse_type()
			from_mod_name := '${mod_name}.${p.prev_tok.lit}'
			if from_mod_name.is_lower() {
				p.error_with_pos('the interface name need to have the pascal case', p.prev_tok.pos())
				break
			}
			comments := p.eat_comments()
			embeds << ast.InterfaceEmbedding{
				name:     from_mod_name
				typ:      from_mod_typ
				pos:      p.prev_tok.pos()
				comments: comments
			}
			if p.tok.kind == .rcbr {
				break
			}
			continue
		}

		if p.tok.kind == .key_mut {
			if is_mut {
				p.error_with_pos('redefinition of `mut` section', p.tok.pos())
				return ast.InterfaceDecl{}
			}
			p.next()
			p.check(.colon)
			is_mut = true
			mut_pos = fields.len
		}
		if p.peek_tok.kind == .lsbr && p.peek_tok.is_next_to(p.tok) {
			if generic_types.len == 0 {
				p.error_with_pos('non-generic interface `${interface_name}` cannot define a generic method',
					p.peek_tok.pos())
			} else {
				p.error_with_pos("no need to add generic type names in generic interface's method",
					p.peek_tok.pos())
			}
			return ast.InterfaceDecl{}
		}
		mut comments := p.eat_comments()
		if p.peek_tok.kind == .lpar {
			method_start_pos := p.tok.pos()
			has_prev_newline := p.has_prev_newline()
			has_break_line := has_prev_newline || p.has_prev_line_comment_or_label()
			line_nr := p.tok.line_nr
			name := p.check_name()

			if name in ['type_name', 'type_idx'] {
				p.error_with_pos('cannot override built-in method `${name}`', method_start_pos)
				return ast.InterfaceDecl{}
			}
			if ts.has_method(name) {
				p.error_with_pos('duplicate method `${name}`', method_start_pos)
				return ast.InterfaceDecl{}
			}
			params_t, _, is_variadic, _ := p.fn_params() // TODO: merge ast.Param and ast.Arg to avoid this
			mut params := [
				ast.Param{
					name:      'x'
					is_mut:    is_mut
					typ:       typ
					is_hidden: true
				},
			]
			params << params_t
			mut method := ast.FnDecl{
				name:             name
				short_name:       name
				mod:              p.mod
				params:           params
				file:             p.file_path
				return_type:      ast.void_type
				is_variadic:      is_variadic
				is_pub:           true
				pos:              method_start_pos.extend(p.prev_tok.pos())
				scope:            p.scope
				has_prev_newline: has_prev_newline
				has_break_line:   has_break_line
			}
			if p.tok.kind.is_start_of_type() && p.tok.line_nr == line_nr {
				method.return_type_pos = p.tok.pos()
				method.return_type = p.parse_type()
				method.return_type_pos = method.return_type_pos.extend(p.tok.pos())
				method.pos = method.pos.extend(method.return_type_pos)
			}
			comments << p.eat_comments(same_line: true)
			mnext_comments := p.eat_comments(follow_up: true)
			method.comments = comments
			method.next_comments = mnext_comments
			methods << method
			tmethod := ast.Fn{
				name:          name
				params:        params
				pos:           method.pos
				return_type:   method.return_type
				is_variadic:   is_variadic
				is_pub:        true
				is_method:     true
				receiver_type: typ
				no_body:       true
			}
			ts.register_method(tmethod)
			info.methods << tmethod
		} else {
			// interface fields
			field_pos := p.tok.pos()
			has_prev_newline := p.has_prev_newline()
			has_break_line := has_prev_newline || p.has_prev_line_comment_or_label()
			field_name := p.check_name()
			mut type_pos := p.tok.pos()
			field_typ := p.parse_type()
			type_pos = type_pos.extend(p.prev_tok.pos())
			comments << p.eat_comments(follow_up: true)
			fields << ast.StructField{
				name:             field_name
				pos:              field_pos
				type_pos:         type_pos
				typ:              field_typ
				comments:         comments
				is_pub:           true
				has_prev_newline: has_prev_newline
				has_break_line:   has_break_line
			}
			info.fields << ast.StructField{
				name:             field_name
				typ:              field_typ
				is_pub:           true
				is_mut:           is_mut
				has_prev_newline: has_prev_newline
				has_break_line:   has_break_line
			}
		}
	}
	info.embeds = embeds.map(it.typ)
	ts.info = info
	p.top_level_statement_end()
	p.check(.rcbr)
	pos = pos.extend_with_last_line(p.prev_tok.pos(), p.prev_tok.line_nr)
	res := ast.InterfaceDecl{
		name:          interface_name
		language:      language
		typ:           typ
		fields:        fields
		methods:       methods
		embeds:        embeds
		is_pub:        is_pub
		attrs:         attrs
		pos:           pos
		pre_comments:  pre_comments
		generic_types: generic_types
		mut_pos:       mut_pos
		name_pos:      name_pos
	}
	p.table.register_interface(res)
	return res
}
