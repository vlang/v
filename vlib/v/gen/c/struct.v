// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module c

import v.ast

const skip_struct_init = ['struct stat', 'struct addrinfo']

fn (mut g Gen) struct_init(node ast.StructInit) {
	mut is_update_tmp_var := false
	mut tmp_update_var := ''
	if node.has_update_expr && !node.update_expr.is_lvalue() {
		is_update_tmp_var = true

		tmp_update_var = g.new_tmp_var()
		s := g.go_before_last_stmt()
		g.empty_line = true

		styp := g.typ(node.update_expr_type)
		g.write('${styp} ${tmp_update_var} = ')
		g.expr(node.update_expr)
		g.writeln(';')
		g.empty_line = false

		g.write(s)
	}
	unalised_typ := g.table.unaliased_type(node.typ)
	styp := if g.table.sym(unalised_typ).language == .v {
		g.typ(unalised_typ).replace('*', '')
	} else {
		g.typ(node.typ)
	}
	mut shared_styp := '' // only needed for shared x := St{...
	if styp in c.skip_struct_init {
		// needed for c++ compilers
		g.go_back(3)
		return
	}
	mut sym := g.table.final_sym(g.unwrap_generic(node.typ))
	is_amp := g.is_amp
	is_multiline := node.init_fields.len > 5
	g.is_amp = false // reset the flag immediately so that other struct inits in this expr are handled correctly
	if is_amp {
		g.go_back(1) // delete the `&` already generated in `prefix_expr()
	}
	mut is_anon := false
	if mut sym.info is ast.Struct {
		is_anon = sym.info.is_anon
	}
	is_array := sym.kind in [.array_fixed, .array]

	// detect if we need type casting on msvc initialization
	const_msvc_init := g.is_cc_msvc && g.inside_const && !g.inside_cast && g.inside_array_item

	if !g.inside_cinit && !is_anon && !is_array && !const_msvc_init {
		g.write('(')
		defer {
			g.write(')')
		}
	}
	if is_anon {
		g.writeln('(${styp}){')
	} else if g.is_shared && !g.inside_opt_data && !g.is_arraymap_set {
		mut shared_typ := node.typ.set_flag(.shared_f)
		shared_styp = g.typ(shared_typ)
		g.writeln('(${shared_styp}*)__dup${shared_styp}(&(${shared_styp}){.mtx = {0}, .val =(${styp}){')
	} else if is_amp || g.inside_cast_in_heap > 0 {
		if node.typ.has_flag(.option) {
			basetyp := g.base_type(node.typ)
			g.write('(${basetyp}*)memdup(&(${basetyp}){')
		} else {
			g.write('(${styp}*)memdup(&(${styp}){')
		}
	} else if node.typ.is_ptr() {
		basetyp := g.typ(node.typ.set_nr_muls(0))
		if is_multiline {
			g.writeln('&(${basetyp}){')
		} else {
			g.write('&(${basetyp}){')
		}
	} else if node.typ.has_flag(.option) {
		g.write('(${g.base_type(node.typ)}){')
	} else if g.inside_cinit {
		if is_multiline {
			g.writeln('{')
		} else {
			g.write('{')
		}
	} else {
		// alias to pointer type
		if g.table.sym(node.typ).kind == .alias && g.table.unaliased_type(node.typ).is_ptr() {
			g.write('&')
		}
		if is_array || const_msvc_init {
			g.write('{')
		} else if is_multiline {
			g.writeln('(${styp}){')
		} else {
			g.write('(${styp}){')
		}
	}
	mut inited_fields := map[string]int{}
	if is_multiline {
		g.indent++
	}
	// User set fields
	mut initialized := false
	mut old_is_shared := g.is_shared
	for i, init_field in node.init_fields {
		if !init_field.typ.has_flag(.shared_f) {
			g.is_shared = false
		}
		mut field_name := init_field.name
		if node.no_keys && sym.kind == .struct_ {
			info := sym.info as ast.Struct
			if info.fields.len == node.init_fields.len {
				field_name = info.fields[i].name
			}
		}
		inited_fields[field_name] = i
		if sym.kind != .struct_ {
			if init_field.typ == 0 {
				g.checker_bug('struct init, field.typ is 0', init_field.pos)
			}
			g.struct_init_field(init_field, sym.language)
			if i != node.init_fields.len - 1 {
				if is_multiline {
					g.writeln(',')
				} else {
					g.write(', ')
				}
			}
			initialized = true
		}
		g.is_shared = old_is_shared
	}
	g.is_shared = old_is_shared
	// The rest of the fields are zeroed.
	// `inited_fields` is a list of fields that have been init'ed, they are skipped
	mut nr_fields := 1
	if sym.kind == .struct_ {
		mut info := sym.info as ast.Struct
		nr_fields = info.fields.len
		if info.is_union && node.init_fields.len > 1 {
			verror('union must not have more than 1 initializer')
		}
		if !info.is_union {
			old_is_shared2 := g.is_shared
			mut used_embed_fields := []string{}
			init_field_names := info.fields.map(it.name)
			// fields that are initialized but belong to the embedding
			init_fields_to_embed := node.init_fields.filter(it.name !in init_field_names)
			for embed in info.embeds {
				embed_sym := g.table.sym(embed)
				embed_name := embed_sym.embed_name()
				if embed_name !in inited_fields {
					embed_info := embed_sym.info as ast.Struct
					embed_field_names := embed_info.fields.map(it.name)
					fields_to_embed := init_fields_to_embed.filter(it.name !in used_embed_fields
						&& it.name in embed_field_names)
					used_embed_fields << fields_to_embed.map(it.name)
					default_init := ast.StructInit{
						...node
						typ: embed
						is_update_embed: true
						init_fields: init_fields_to_embed
					}
					inside_cast_in_heap := g.inside_cast_in_heap
					g.inside_cast_in_heap = 0 // prevent use of pointers in child structs

					g.write('.${embed_name} = ')
					g.struct_init(default_init)

					g.inside_cast_in_heap = inside_cast_in_heap // restore value for further struct inits
					if is_multiline {
						g.writeln(',')
					} else {
						g.write(',')
					}
					initialized = true
				}
			}
			g.is_shared = old_is_shared2
		}
		for mut field in info.fields {
			g.is_shared = field.typ.has_flag(.shared_f)
			if mut sym.info is ast.Struct {
				mut found_equal_fields := 0
				for mut sifield in sym.info.fields {
					if sifield.name == field.name {
						found_equal_fields++
						break
					}
				}
				if found_equal_fields == 0 {
					continue
				}
			}
			if already_inited_node_field_index := inited_fields[field.name] {
				mut sfield := node.init_fields[already_inited_node_field_index]
				if sfield.typ == 0 {
					continue
				}
				if sfield.expected_type.has_flag(.generic) && g.cur_fn != unsafe { nil } {
					mut t_generic_names := g.table.cur_fn.generic_names.clone()
					mut t_concrete_types := g.cur_concrete_types.clone()
					ts := g.table.sym(node.typ)
					if ts.generic_types.len > 0 && ts.generic_types.len == info.generic_types.len
						&& ts.generic_types != info.generic_types {
						t_generic_names = info.generic_types.map(g.table.sym(it).name)
						t_concrete_types = []
						for t_typ in ts.generic_types {
							if !t_typ.has_flag(.generic) {
								t_concrete_types << t_typ
							} else if g.table.sym(t_typ).kind == .any {
								tname := g.table.sym(t_typ).name
								index := g.table.cur_fn.generic_names.index(tname)
								if index >= 0 && index < g.cur_concrete_types.len {
									t_concrete_types << g.cur_concrete_types[index]
								}
							} else {
								if tt := g.table.resolve_generic_to_concrete(t_typ, g.table.cur_fn.generic_names,
									g.cur_concrete_types)
								{
									t_concrete_types << tt
								}
							}
						}
					}
					if tt := g.table.resolve_generic_to_concrete(sfield.expected_type,
						t_generic_names, t_concrete_types)
					{
						sfield.expected_type = tt
					}
				}
				if node.no_keys && sym.kind == .struct_ {
					sym_info := sym.info as ast.Struct
					if sym_info.fields.len == node.init_fields.len {
						sfield.name = sym_info.fields[already_inited_node_field_index].name
					}
				}
				g.struct_init_field(sfield, sym.language)
				if is_multiline {
					g.writeln(',')
				} else {
					g.write(',')
				}
				initialized = true
				continue
			}
			if info.is_union {
				// unions thould have exactly one explicit initializer
				continue
			}
			field_name := c_name(field.name)
			if field.typ in info.embeds {
				continue
			}
			if node.has_update_expr {
				g.write('.${field_name} = ')
				if is_update_tmp_var {
					g.write(tmp_update_var)
				} else {
					g.write('(')
					g.expr(node.update_expr)
					g.write(')')
				}
				if node.update_expr_type.is_ptr() {
					g.write('->')
				} else {
					g.write('.')
				}
				if node.is_update_embed {
					update_sym := g.table.sym(node.update_expr_type)
					_, embeds := g.table.find_field_from_embeds(update_sym, field.name) or {
						ast.StructField{}, []ast.Type{}
					}
					for embed in embeds {
						esym := g.table.sym(embed)
						ename := esym.embed_name()
						g.write(ename)
						if embed.is_ptr() {
							g.write('->')
						} else {
							g.write('.')
						}
					}
				}
				g.write(c_name(field.name))
			} else {
				if !g.zero_struct_field(field) {
					nr_fields--
					continue
				}
			}
			if is_multiline {
				g.writeln(',')
			} else {
				g.write(',')
			}
			initialized = true
		}
		g.is_shared = old_is_shared
	}
	if is_multiline {
		g.indent--
	}

	if !initialized {
		if nr_fields > 0 {
			g.write('0')
		} else {
			g.write('EMPTY_STRUCT_INITIALIZATION')
		}
	}

	g.write('}')
	if g.is_shared && !g.inside_opt_data && !g.is_arraymap_set {
		g.write('}, sizeof(${shared_styp}))')
	} else if is_amp || g.inside_cast_in_heap > 0 {
		if node.typ.has_flag(.option) {
			basetyp := g.base_type(node.typ)
			g.write(', sizeof(${basetyp}))')
		} else {
			g.write(', sizeof(${styp}))')
		}
	}
}

fn (mut g Gen) zero_struct_field(field ast.StructField) bool {
	old_inside_cast_in_heap := g.inside_cast_in_heap
	g.inside_cast_in_heap = 0
	defer {
		g.inside_cast_in_heap = old_inside_cast_in_heap
	}
	sym := g.table.sym(field.typ)
	field_name := if sym.language == .v { c_name(field.name) } else { field.name }
	if sym.info is ast.Struct {
		if sym.info.fields.len == 0 {
			return false
		} else if !field.has_default_expr {
			mut has_option_field := false
			for fd in sym.info.fields {
				if fd.typ.has_flag(.option) {
					has_option_field = true
					break
				}
			}
			if has_option_field || field.anon_struct_decl.fields.len > 0 {
				default_init := ast.StructInit{
					typ: field.typ
				}
				g.write('.${field_name} = ')
				if field.typ.has_flag(.option) {
					if field.is_recursive || field.typ.is_ptr() {
						g.expr_with_opt(ast.None{}, ast.none_type, field.typ)
					} else {
						tmp_var := g.new_tmp_var()
						g.expr_with_tmp_var(default_init, field.typ, field.typ, tmp_var)
					}
				} else {
					g.struct_init(default_init)
				}
				return true
			}
		}
	}
	g.write('.${field_name} = ')
	if field.has_default_expr {
		if sym.kind in [.sum_type, .interface_] {
			if field.typ.has_flag(.option) {
				g.expr_with_opt(field.default_expr, field.default_expr_typ, field.typ)
			} else {
				g.expr_with_cast(field.default_expr, field.default_expr_typ, field.typ)
			}
			return true
		}

		if field.default_expr is ast.None {
			g.gen_option_error(field.typ, ast.None{})
			return true
		} else if field.typ.has_flag(.option) {
			tmp_var := g.new_tmp_var()
			g.expr_with_tmp_var(field.default_expr, field.default_expr_typ, field.typ,
				tmp_var)
			return true
		} else if field.typ.has_flag(.result) && !field.default_expr_typ.has_flag(.result) {
			tmp_var := g.new_tmp_var()
			g.expr_with_tmp_var(field.default_expr, field.default_expr_typ, field.typ,
				tmp_var)
			return true
		}
		g.expr(field.default_expr)
	} else if field.typ.has_flag(.option) {
		g.gen_option_error(field.typ, ast.None{})
		return true
	} else if sym.info is ast.ArrayFixed {
		g.write('{')
		for i in 0 .. sym.info.size {
			if sym.info.elem_type.has_flag(.option) {
				g.expr_with_opt(ast.None{}, ast.none_type, sym.info.elem_type)
			} else {
				g.write(g.type_default(sym.info.elem_type))
			}
			if i != sym.info.size - 1 {
				g.write(', ')
			}
		}
		g.write('}')
	} else {
		g.write(g.type_default(field.typ))
	}
	return true
}

fn (mut g Gen) is_empty_struct(t Type) bool {
	sym := t.unaliased_sym
	match sym.info {
		ast.Struct {
			if sym.info.fields.len > 0 || sym.info.embeds.len > 0 {
				return false
			}
			return true
		}
		else {
			return false
		}
	}
}

fn (mut g Gen) struct_decl(s ast.Struct, name string, is_anon bool) {
	if s.is_generic {
		return
	}
	if name.contains('_T_') {
		if s.is_union {
			g.typedefs.writeln('typedef union ${name} ${name};')
		} else {
			g.typedefs.writeln('typedef struct ${name} ${name};')
		}
	}
	// TODO avoid buffer manip
	start_pos := g.type_definitions.len

	mut pre_pragma := ''
	mut post_pragma := ''

	for attr in s.attrs {
		match attr.name {
			'_pack' {
				pre_pragma += '#pragma pack(push, ${attr.arg})\n'
				post_pragma += '#pragma pack(pop)'
			}
			'packed' {
				pre_pragma += '#pragma pack(push, 1)\n'
				post_pragma += '#pragma pack(pop)'
			}
			else {}
		}
	}

	is_minify := s.is_minify
	g.type_definitions.writeln(pre_pragma)

	mut aligned_attr := ''
	if attr := s.attrs.find_first('aligned') {
		attr_arg := if attr.arg == '' { '' } else { ' (${attr.arg})' }
		aligned_attr += if g.is_cc_msvc {
			'__declspec(align${attr_arg})'
		} else {
			' __attribute__((aligned${attr_arg}))'
		}
	}
	if is_anon {
		g.type_definitions.write_string('\t${name} ')
		return
	} else if s.is_union {
		if g.is_cc_msvc && aligned_attr != '' {
			g.type_definitions.writeln('union ${aligned_attr} ${name} {')
		} else {
			g.type_definitions.writeln('union ${name} {')
		}
	} else {
		if g.is_cc_msvc && aligned_attr != '' {
			g.type_definitions.writeln('struct ${aligned_attr} ${name} {')
		} else {
			g.type_definitions.writeln('struct ${name} {')
		}
	}

	if s.fields.len > 0 || s.embeds.len > 0 {
		for field in s.fields {
			// Some of these structs may want to contain
			// options that may not be defined at this point
			// if this is the case then we are going to
			// buffer manip out in front of the struct
			// write the option in and then continue
			// FIXME: for parallel cgen (two different files using the same option in struct fields)
			if field.typ.has_flag(.option) {
				// Dont use g.typ() here because it will register
				// option and we dont want that
				styp, base := g.option_type_name(field.typ)
				lock g.done_options {
					if base !in g.done_options {
						g.done_options << base
						last_text := g.type_definitions.after(start_pos).clone()
						g.type_definitions.go_back_to(start_pos)
						g.typedefs.writeln('typedef struct ${styp} ${styp};')
						g.type_definitions.writeln('${g.option_type_text(styp, base)};')
						g.type_definitions.write_string(last_text)
					}
				}
			}
			if field.typ.has_flag(.result) {
				// Dont use g.typ() here because it will register
				// result and we dont want that
				styp, base := g.result_type_name(field.typ)
				lock g.done_results {
					if base !in g.done_results {
						g.done_results << base
						last_text := g.type_definitions.after(start_pos).clone()
						g.type_definitions.go_back_to(start_pos)
						g.typedefs.writeln('typedef struct ${styp} ${styp};')
						g.type_definitions.writeln('${g.result_type_text(styp, base)};')
						g.type_definitions.write_string(last_text)
					}
				}
			}
			type_name := g.typ(field.typ)
			field_name := c_name(field.name)
			volatile_prefix := if field.is_volatile { 'volatile ' } else { '' }
			mut size_suffix := ''
			if is_minify && !g.is_cc_msvc && !g.pref.output_cross_c {
				if field.typ == ast.bool_type_idx {
					size_suffix = ' : 1'
				} else {
					field_sym := g.table.sym(field.typ)
					if field_sym.info is ast.Enum {
						if !field_sym.info.is_flag && !field_sym.info.uses_exprs {
							mut bits_needed := 0
							mut l := field_sym.info.vals.len
							for l > 0 {
								bits_needed++
								l >>= 1
							}
							size_suffix = ' : ${bits_needed}'
						}
					}
				}
			}
			field_sym := g.table.sym(field.typ)
			mut field_is_anon := false
			if field_sym.info is ast.Struct {
				if field_sym.info.is_anon {
					field_is_anon = true
					// Recursively generate code for this anon struct (this is the field's type)
					g.struct_decl(field_sym.info, field_sym.cname, true)
					// Now the field's name
					g.type_definitions.writeln(' ${field_name}${size_suffix};')
				}
			}
			if !field_is_anon {
				g.type_definitions.writeln('\t${volatile_prefix}${type_name} ${field_name}${size_suffix};')
			}
		}
	} else {
		g.type_definitions.writeln('\tEMPTY_STRUCT_DECLARATION;')
	}
	ti_attrs := if !g.is_cc_msvc && s.attrs.contains('packed') {
		'__attribute__((__packed__))'
	} else {
		''
	}
	g.type_definitions.write_string('}${ti_attrs}')
	if !g.is_cc_msvc && aligned_attr != '' {
		g.type_definitions.write_string(' ${aligned_attr}')
	}
	if !is_anon {
		g.type_definitions.write_string(';')
	}
	g.type_definitions.writeln('')
	if post_pragma.len > 0 {
		g.type_definitions.writeln(post_pragma)
	}
}

fn (mut g Gen) struct_init_field(sfield ast.StructInitField, language ast.Language) {
	field_name := if language == .v { c_name(sfield.name) } else { sfield.name }
	g.write('.${field_name} = ')
	field_type_sym := g.table.sym(sfield.typ)
	mut cloned := false
	if g.is_autofree && !sfield.typ.is_ptr() && field_type_sym.kind in [.array, .string] {
		g.write('/*clone1*/')
		if g.gen_clone_assignment(sfield.expr, sfield.typ, false) {
			cloned = true
		}
	}
	if !cloned {
		inside_cast_in_heap := g.inside_cast_in_heap
		g.inside_cast_in_heap = 0 // prevent use of pointers in child structs

		field_unwrap_sym := g.table.sym(g.unwrap_generic(sfield.typ))
		if field_unwrap_sym.kind == .array_fixed && sfield.expr in [ast.Ident, ast.SelectorExpr] {
			info := field_unwrap_sym.info as ast.ArrayFixed
			g.fixed_array_var_init(sfield.expr, info.size)
		} else {
			if sfield.typ != ast.voidptr_type && sfield.typ != ast.nil_type
				&& (sfield.expected_type.is_ptr() && !sfield.expected_type.has_flag(.shared_f))
				&& !sfield.expected_type.has_flag(.option) && !sfield.typ.is_any_kind_of_pointer()
				&& !sfield.typ.is_number() {
				g.write('/* autoref */&')
			}

			if (sfield.expected_type.has_flag(.option) && !sfield.typ.has_flag(.option))
				|| (sfield.expected_type.has_flag(.result) && !sfield.typ.has_flag(.result)) {
				g.expr_with_opt(sfield.expr, sfield.typ, sfield.expected_type)
			} else if sfield.expr is ast.LambdaExpr && sfield.expected_type.has_flag(.option) {
				g.expr_opt_with_cast(sfield.expr, sfield.typ, sfield.expected_type)
			} else {
				g.left_is_opt = true
				g.expr_with_cast(sfield.expr, sfield.typ, sfield.expected_type)
			}
		}
		g.inside_cast_in_heap = inside_cast_in_heap // restore value for further struct inits
	}
}
