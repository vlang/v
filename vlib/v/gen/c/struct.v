// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module c

import v.ast

const skip_struct_init = ['struct stat', 'struct addrinfo']

fn (mut g Gen) struct_init(node ast.StructInit) {
	styp := g.typ(node.typ)
	mut shared_styp := '' // only needed for shared x := St{...
	if styp in c.skip_struct_init {
		// needed for c++ compilers
		g.out.go_back(3)
		return
	}
	mut sym := g.table.final_sym(g.unwrap_generic(node.typ))
	is_amp := g.is_amp
	is_multiline := node.fields.len > 5
	g.is_amp = false // reset the flag immediately so that other struct inits in this expr are handled correctly
	if is_amp {
		g.out.go_back(1) // delete the `&` already generated in `prefix_expr()
	}
	g.write('(')
	defer {
		g.write(')')
	}
	if g.is_shared && !g.inside_opt_data && !g.is_arraymap_set {
		mut shared_typ := node.typ.set_flag(.shared_f)
		shared_styp = g.typ(shared_typ)
		g.writeln('($shared_styp*)__dup${shared_styp}(&($shared_styp){.mtx = {0}, .val =($styp){')
	} else if is_amp || g.inside_cast_in_heap > 0 {
		g.write('($styp*)memdup(&($styp){')
	} else if node.typ.is_ptr() {
		basetyp := g.typ(node.typ.set_nr_muls(0))
		if is_multiline {
			g.writeln('&($basetyp){')
		} else {
			g.write('&($basetyp){')
		}
	} else {
		if is_multiline {
			g.writeln('($styp){')
		} else {
			g.write('($styp){')
		}
	}
	// mut fields := []string{}
	mut inited_fields := map[string]int{} // TODO this is done in checker, move to ast node
	/*
	if node.fields.len == 0 && node.exprs.len > 0 {
		// Get fields for {a,b} short syntax. Fields array wasn't set in the parser.
		for f in info.fields {
			fields << f.name
		}
	} else {
		fields = node.fields
	}
	*/
	if is_multiline {
		g.indent++
	}
	// User set fields
	mut initialized := false
	mut old_is_shared := g.is_shared
	for i, field in node.fields {
		if !field.typ.has_flag(.shared_f) {
			g.is_shared = false
		}
		inited_fields[field.name] = i
		if sym.kind != .struct_ {
			field_name := if sym.language == .v { c_name(field.name) } else { field.name }
			g.write('.$field_name = ')
			if field.typ == 0 {
				g.checker_bug('struct init, field.typ is 0', field.pos)
			}
			field_type_sym := g.table.sym(field.typ)
			mut cloned := false
			if g.is_autofree && !field.typ.is_ptr() && field_type_sym.kind in [.array, .string] {
				g.write('/*clone1*/')
				if g.gen_clone_assignment(field.expr, field.typ, false) {
					cloned = true
				}
			}
			if !cloned {
				if (field.expected_type.is_ptr() && !field.expected_type.has_flag(.shared_f))
					&& !(field.typ.is_ptr() || field.typ.is_pointer()) && !field.typ.is_number() {
					g.write('/* autoref */&')
				}
				g.expr_with_cast(field.expr, field.typ, field.expected_type)
			}
			if i != node.fields.len - 1 {
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
		if info.is_union && node.fields.len > 1 {
			verror('union must not have more than 1 initializer')
		}
		if !info.is_union {
			old_is_shared2 := g.is_shared
			mut used_embed_fields := []string{}
			init_field_names := info.fields.map(it.name)
			// fields that are initialized but belong to the embedding
			init_fields_to_embed := node.fields.filter(it.name !in init_field_names)
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
						fields: init_fields_to_embed
					}
					inside_cast_in_heap := g.inside_cast_in_heap
					g.inside_cast_in_heap = 0 // prevent use of pointers in child structs

					g.write('.$embed_name = ')
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
		// g.zero_struct_fields(info, inited_fields)
		// nr_fields = info.fields.len
		for mut field in info.fields {
			if !field.typ.has_flag(.shared_f) {
				g.is_shared = false
			}
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
			if field.name in inited_fields {
				sfield := node.fields[inited_fields[field.name]]
				field_name := if sym.language == .v { c_name(field.name) } else { field.name }
				if sfield.typ == 0 {
					continue
				}
				g.write('.$field_name = ')
				field_type_sym := g.table.sym(sfield.typ)
				mut cloned := false
				if g.is_autofree && !sfield.typ.is_ptr() && field_type_sym.kind in [.array, .string] {
					g.write('/*clone1*/')
					if g.gen_clone_assignment(sfield.expr, sfield.typ, false) {
						cloned = true
					}
				}
				if !cloned {
					if field_type_sym.kind == .array_fixed && sfield.expr is ast.Ident {
						fixed_array_info := field_type_sym.info as ast.ArrayFixed
						g.write('{')
						for i in 0 .. fixed_array_info.size {
							g.expr(sfield.expr)
							g.write('[$i]')
							if i != fixed_array_info.size - 1 {
								g.write(', ')
							}
						}
						g.write('}')
					} else {
						if (sfield.expected_type.is_ptr()
							&& !sfield.expected_type.has_flag(.shared_f)) && !(sfield.typ.is_ptr()
							|| sfield.typ.is_pointer()) && !sfield.typ.is_number() {
							g.write('/* autoref */&')
						}
						g.expr_with_cast(sfield.expr, sfield.typ, sfield.expected_type)
					}
				}
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
			if field.typ.has_flag(.optional) {
				field_name := c_name(field.name)
				g.write('.$field_name = {EMPTY_STRUCT_INITIALIZATION},')
				initialized = true
				continue
			}
			if field.typ in info.embeds {
				continue
			}
			if node.has_update_expr {
				g.expr(node.update_expr)
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
			g.is_shared = old_is_shared
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
		g.write('}, sizeof($shared_styp))')
	} else if is_amp || g.inside_cast_in_heap > 0 {
		g.write(', sizeof($styp))')
	}
}

fn (mut g Gen) zero_struct_field(field ast.StructField) bool {
	sym := g.table.sym(field.typ)
	if sym.kind == .struct_ {
		info := sym.info as ast.Struct
		if info.fields.len == 0 {
			return false
		}
	}
	field_name := if sym.language == .v { c_name(field.name) } else { field.name }
	g.write('.$field_name = ')
	if field.has_default_expr {
		if sym.kind in [.sum_type, .interface_] {
			g.expr_with_cast(field.default_expr, field.default_expr_typ, field.typ)
			return true
		}
		g.expr(field.default_expr)
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
