// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module c

import v.ast
import v.util
import strings
import v.depgraph

// global or const variable definition string
struct GlobalConstDef {
	mod            string   // module name
	def            string   // definition
	init           string   // init later (in _vinit)
	dep_names      []string // the names of all the consts, that this const depends on
	order          int      // -1 for simple defines, string literals, anonymous function names, extern declarations etc
	is_precomputed bool     // can be declared as a const in C: primitive, and a simple definition
}

fn (mut g Gen) const_decl(node ast.ConstDecl) {
	g.inside_const = true
	defer {
		g.inside_const = false
	}
	for field in node.fields {
		if g.pref.skip_unused {
			if field.name !in g.table.used_features.used_consts {
				$if trace_skip_unused_consts ? {
					eprintln('>> skipping unused const name: ${field.name}')
				}
				continue
			}
		}
		name := c_name(field.name)
		mut const_name := '_const_' + name
		if g.pref.translated && !g.is_builtin_mod
			&& !util.module_is_builtin(field.name.all_before_last('.')) {
			if name.starts_with('main__') {
				const_name = name.all_after_first('main__')
			}
		}
		if !g.is_builtin_mod {
			if cattr := node.attrs.find_first('export') {
				const_name = cattr.arg
			}
		}
		field_expr := field.expr
		match field.expr {
			ast.ArrayInit {
				elems_are_const := field.expr.exprs.all(g.check_expr_is_const(it))
				if field.expr.is_fixed && !field.expr.has_index
					&& g.pref.build_mode != .build_module
					&& (!g.is_cc_msvc || field.expr.elem_type != ast.string_type) && elems_are_const {
					styp := g.styp(field.expr.typ)
					val := g.expr_string(field.expr)
					// eprintln('> const_name: ${const_name} | name: ${name} | styp: ${styp} | val: ${val}')
					g.global_const_defs[name] = GlobalConstDef{
						mod:       field.mod
						def:       '${g.static_non_parallel}${styp} ${const_name} = ${val}; // fixed array const'
						dep_names: g.table.dependent_names_in_expr(field_expr)
					}
				} else if field.expr.is_fixed && !field.expr.has_index
					&& ((g.is_cc_msvc && field.expr.elem_type == ast.string_type)
					|| !elems_are_const) {
					g.const_decl_init_later_msvc_string_fixed_array(field.mod, name, field.expr,
						field.typ)
				} else {
					g.const_decl_init_later(field.mod, name, field.expr, field.typ, false)
				}
			}
			ast.StringLiteral {
				val := g.expr_string(field.expr)
				typ := if field.expr.language == .c { 'char*' } else { 'string' }
				g.global_const_defs[name] = GlobalConstDef{
					mod:   field.mod
					def:   '${typ} ${const_name}; // a string literal, inited later'
					init:  '\t${const_name} = ${val};'
					order: -1
				}
			}
			ast.CallExpr {
				if field.expr.return_type.has_flag(.option)
					|| field.expr.return_type.has_flag(.result) {
					old_inside_const_opt_or_res := g.inside_const_opt_or_res
					g.inside_const_opt_or_res = true
					unwrap_opt_res := field.expr.or_block.kind != .absent
					g.const_decl_init_later(field.mod, name, field.expr, field.typ, unwrap_opt_res)
					g.inside_const_opt_or_res = old_inside_const_opt_or_res
				} else {
					g.const_decl_init_later(field.mod, name, field.expr, field.typ, false)
				}
			}
			else {
				// Note: -usecache uses prebuilt modules, each compiled with:
				// `v build-module vlib/module`
				// combined with a top level program, that is compiled with:
				// `v -usecache toplevel`
				// For it to work, the consts optimisations should be identical, because
				// only the top level program will have the const initialisation code for
				// all the modules.
				// TODO: encapsulate const initialisation for each module in a separate function,
				// that is just called by the top level program in _vinit, instead of generating
				// all the code inside _vinit for each module.
				use_cache_mode := g.pref.build_mode == .build_module || g.pref.use_cache
				if !use_cache_mode {
					if ct_value := field.comptime_expr_value() {
						if g.const_decl_precomputed(field.mod, name, field.name, ct_value,
							field.typ)
						{
							continue
						}
					}
				}
				if field.is_simple_define_const() {
					// "Simple" expressions are not going to need multiple statements,
					// only the ones which are inited later, so it's safe to use expr_string
					g.const_decl_simple_define(field.mod, field.name, g.expr_string(field_expr))
				} else if field.expr is ast.CastExpr {
					if field.expr.expr is ast.ArrayInit {
						if field.expr.expr.is_fixed && g.pref.build_mode != .build_module {
							styp := g.styp(field.expr.typ)
							val := g.expr_string(field.expr.expr)
							g.global_const_defs[name] = GlobalConstDef{
								mod:       field.mod
								def:       '${g.static_non_parallel}${styp} ${const_name} = ${val}; // fixed array const'
								dep_names: g.table.dependent_names_in_expr(field_expr)
							}
							continue
						}
					}
					should_surround := field.expr.expr is ast.CallExpr
						&& field.expr.expr.or_block.kind != .absent
					g.const_decl_init_later(field.mod, name, field.expr, field.typ, should_surround)
				} else if field.expr is ast.InfixExpr {
					mut has_unwrap_opt_res := false
					if field.expr.left is ast.CallExpr {
						has_unwrap_opt_res = field.expr.left.or_block.kind != .absent
					} else if field.expr.right is ast.CallExpr {
						has_unwrap_opt_res = field.expr.right.or_block.kind != .absent
					}
					g.const_decl_init_later(field.mod, name, field.expr, field.typ, has_unwrap_opt_res)
				} else {
					g.const_decl_init_later(field.mod, name, field.expr, field.typ, true)
				}
			}
		}
	}
}

fn (mut g Gen) const_decl_precomputed(mod string, name string, field_name string, ct_value ast.ComptTimeConstValue,
	typ ast.Type) bool {
	mut styp := g.styp(typ)
	cname := if g.pref.translated && !g.is_builtin_mod { name } else { '_const_${name}' }
	$if trace_const_precomputed ? {
		eprintln('> styp: ${styp} | cname: ${cname} | ct_value: ${ct_value} | ${ct_value.type_name()}')
	}
	match ct_value {
		i8 {
			g.const_decl_write_precomputed(mod, styp, cname, field_name, ct_value.str())
		}
		i16 {
			g.const_decl_write_precomputed(mod, styp, cname, field_name, ct_value.str())
		}
		i32 {
			g.const_decl_write_precomputed(mod, styp, cname, field_name, ct_value.str())
		}
		// int {
		// XTODO int64
		// g.const_decl_write_precomputed(mod, styp, cname, field_name, ct_value.str())
		//}
		i64 {
			if typ == ast.i64_type {
				return false
			}
			if typ == ast.int_type {
				// TODO: use g.const_decl_write_precomputed here too.
				// For now, use #define macros, so existing code compiles
				// with -cstrict. Add checker errors for overflows instead,
				// so V can catch them earlier, instead of relying on the
				// C compiler for that.
				g.const_decl_simple_define(mod, name, ct_value.str())
				return true
			}
			if typ == ast.u64_type {
				g.const_decl_write_precomputed(mod, styp, cname, field_name, ct_value.str() + 'U')
			} else {
				g.const_decl_write_precomputed(mod, styp, cname, field_name, ct_value.str())
			}
		}
		u8 {
			g.const_decl_write_precomputed(mod, styp, cname, field_name, ct_value.str())
		}
		u16 {
			g.const_decl_write_precomputed(mod, styp, cname, field_name, ct_value.str())
		}
		u32 {
			g.const_decl_write_precomputed(mod, styp, cname, field_name, ct_value.str())
		}
		u64 {
			g.const_decl_write_precomputed(mod, styp, cname, field_name, ct_value.str() + 'U')
		}
		f32 {
			g.const_decl_write_precomputed(mod, styp, cname, field_name, ct_value.str())
		}
		f64 {
			g.const_decl_write_precomputed(mod, styp, cname, field_name, ct_value.str())
		}
		rune {
			rune_code := u32(ct_value)
			if rune_code <= 127 {
				if rune_code in [`"`, `\\`, `'`] {
					return false
				}
				escval := util.smart_quote(u8(rune_code).ascii_str(), false)

				g.global_const_defs[util.no_dots(field_name)] = GlobalConstDef{
					mod:   mod
					def:   "#define ${cname} '${escval}'"
					order: -1
				}
			} else {
				g.const_decl_write_precomputed(mod, styp, cname, field_name, u32(ct_value).str())
			}
		}
		string {
			escaped_val := util.smart_quote(ct_value, false)
			// g.const_decl_write_precomputed(line_nr, styp, cname, '_S("$escaped_val")')
			// TODO: ^ the above for strings, cause:
			// `error C2099: initializer is not a constant` errors in MSVC,
			// so fall back to the delayed initialisation scheme:
			init := if typ == ast.string_type {
				'_S("${escaped_val}")'
			} else {
				'(${styp})"${escaped_val}"'
			}
			g.global_const_defs[util.no_dots(field_name)] = GlobalConstDef{
				mod:   mod
				def:   '${styp} ${cname}; // str inited later'
				init:  '\t${cname} = ${init};'
				order: -1
			}
			if g.is_autofree {
				g.cleanups[mod].writeln('\tstring_free(&${cname});')
			}
		}
		voidptr {
			g.const_decl_write_precomputed(mod, styp, cname, field_name, '(voidptr)(0x${ct_value})')
		}
		ast.EmptyExpr {
			return false
		}
	}
	return true
}

fn (mut g Gen) const_decl_write_precomputed(mod string, styp string, cname string, field_name string, ct_value string) {
	if g.pref.is_livemain || g.pref.is_liveshared {
		// Note: tcc has problems reloading .so files with consts in them, when the consts are then used inside the reloaded
		// live functions. As a workaround, just use simple #define macros in this case.
		//
		// If you change it, please also test with `v -live run examples/hot_reload/graph.v` which uses `math.pi` .
		g.global_const_defs[util.no_dots(field_name)] = GlobalConstDef{
			mod:   mod
			def:   '#define ${cname} ${ct_value} // precomputed3, -live mode'
			order: -1
		}
		return
	}
	g.global_const_defs[util.no_dots(field_name)] = GlobalConstDef{
		mod: mod
		def: '${g.static_non_parallel}const ${styp} ${cname} = ${ct_value}; // precomputed2'
		// is_precomputed: true
	}
}

fn (mut g Gen) const_decl_simple_define(mod string, name string, val string) {
	// Simple expressions should use a #define
	// so that we don't pollute the binary with unnecessary global vars
	// Do not do this when building a module, otherwise the consts
	// will not be accessible.
	mut x := util.no_dots(name)
	if g.pref.translated && !g.is_builtin_mod && !util.module_is_builtin(name.all_before_last('.')) {
		// Don't prepend "_const" to translated C consts,
		// but only in user code, continue prepending "_const" to builtin consts.
		if x.starts_with('main__') {
			x = x['main__'.len..]
		}
	} else {
		x = '_const_${x}'
	}
	if g.pref.translated {
		g.global_const_defs[util.no_dots(name)] = GlobalConstDef{
			mod:   mod
			def:   'const int ${x} = ${val};'
			order: -1
		}
	} else {
		g.global_const_defs[util.no_dots(name)] = GlobalConstDef{
			mod:   mod
			def:   '#define ${x} ${val}'
			order: -1
		}
	}
}

fn (mut g Gen) c_const_name(name string) string {
	return if g.pref.translated && !g.is_builtin_mod { name } else { '_const_${name}' }
}

fn (mut g Gen) const_decl_init_later(mod string, name string, expr ast.Expr, typ ast.Type, surround_cbr bool) {
	if name.starts_with('C__') {
		return
	}
	// Initialize more complex consts in `void _vinit/2{}`
	// (C doesn't allow init expressions that can't be resolved at compile time).
	mut styp := g.styp(typ)
	cname := g.c_const_name(name)
	mut init := strings.new_builder(100)

	if surround_cbr {
		init.writeln('{')
	}
	if expr is ast.ArrayInit && (expr as ast.ArrayInit).has_index {
		init.writeln(g.expr_string_surround('\tmemcpy(&${cname}, &', expr, ', sizeof(${styp}));'))
	} else if expr is ast.CallExpr
		&& g.table.final_sym(g.unwrap_generic((expr as ast.CallExpr).return_type)).kind == .array_fixed {
		init.writeln(g.expr_string_surround('\tmemcpy(&${cname}, ', expr, ', sizeof(${styp}));'))
	} else {
		init.writeln(g.expr_string_surround('\t${cname} = ', expr, ';'))
	}
	if surround_cbr {
		init.writeln('}')
	}
	mut def := '${styp} ${cname}'
	if g.pref.parallel_cc {
		// So that the const is usable in other files
		// def = 'extern ${def}'
	}
	expr_sym := g.table.sym(typ)
	if expr_sym.kind == .function {
		// allow for: `const xyz = abc`, where `abc` is `fn abc() {}`
		func := (expr_sym.info as ast.FnType).func
		def = g.fn_var_signature(func.return_type, func.params.map(it.typ), cname)
	}
	init_str := init.str().trim_right('\n')
	g.global_const_defs[util.no_dots(name)] = GlobalConstDef{
		mod:       mod
		def:       '${def}; // inited later'
		init:      if init_str.count('\n') > 1 {
			'{\n${init_str}\n}'
		} else {
			init_str
		}
		dep_names: g.table.dependent_names_in_expr(expr)
	}
	if g.is_autofree {
		sym := g.table.sym(typ)
		if styp.starts_with('Array_') {
			if sym.has_method_with_generic_parent('free') {
				g.cleanup.writeln('\t${styp}_free(&${cname});')
			} else {
				g.cleanup.writeln('\tarray_free(&${cname});')
			}
		} else if styp == 'string' {
			g.cleanup.writeln('\tstring_free(&${cname});')
		} else if sym.kind == .map {
			g.cleanup.writeln('\tmap_free(&${cname});')
		} else if styp == 'IError' {
			g.cleanup.writeln('\tIError_free(&${cname});')
		}
	}
}

fn (mut g Gen) const_decl_init_later_msvc_string_fixed_array(mod string, name string, expr ast.ArrayInit,
	typ ast.Type) {
	mut styp := g.styp(typ)
	cname := g.c_const_name(name)
	mut init := strings.new_builder(100)
	for i, elem_expr in expr.exprs {
		if elem_expr is ast.ArrayInit && elem_expr.is_fixed {
			elem_typ := g.styp(elem_expr.typ)
			init.writeln(g.expr_string_surround('\tmemcpy(${cname}[${i}], (${elem_typ})',
				elem_expr, ', sizeof(${elem_typ}));'))
		} else if elem_expr is ast.Ident {
			elem_typ := elem_expr.obj.typ
			if g.table.final_sym(elem_typ).kind == .array_fixed {
				elem_styp := g.styp(elem_expr.obj.typ)
				init.writeln(g.expr_string_surround('\tmemcpy(${cname}[${i}], ', elem_expr,
					', sizeof(${elem_styp}));'))
			} else {
				init.writeln(g.expr_string_surround('\t${cname}[${i}] = ', elem_expr,
					';'))
			}
		} else {
			init.writeln(g.expr_string_surround('\t${cname}[${i}] = ', elem_expr, ';'))
		}
	}
	mut def := '${styp} ${cname}'
	g.global_const_defs[util.no_dots(name)] = GlobalConstDef{
		mod:       mod
		def:       '${def}; // inited later'
		init:      init.str().trim_right('\n')
		dep_names: g.table.dependent_names_in_expr(expr)
	}
	if g.is_autofree {
		sym := g.table.sym(typ)
		if sym.has_method_with_generic_parent('free') {
			g.cleanup.writeln('\t${styp}_free(&${cname});')
		} else {
			g.cleanup.writeln('\tarray_free(&${cname});')
		}
	}
}

fn (mut g Gen) global_decl(node ast.GlobalDecl) {
	// was static used here to to make code optimizable? it was removed when
	// 'extern' was used to fix the duplicate symbols with usecache && clang
	// visibility_kw := if g.pref.build_mode == .build_module && g.is_builtin_mod { 'static ' }
	visibility_kw := if
		(g.pref.use_cache || (g.pref.build_mode == .build_module && g.module_built != node.mod))
		&& !util.should_bundle_module(node.mod) {
		'extern '
	} else {
		''
		// g.static_modifier // TODO: used to be '' before parallel_cc, may cause issues
	}
	// should the global be initialized now, not later in `vinit()`
	cinit := node.attrs.contains('cinit')
	g.inside_cinit = cinit
	g.inside_global_decl = true
	defer {
		g.inside_cinit = false
		g.inside_global_decl = false
	}
	cextern := node.attrs.contains('c_extern')
	should_init := (!g.pref.use_cache && g.pref.build_mode != .build_module)
		|| (g.pref.build_mode == .build_module && g.module_built == node.mod)
	mut attributes := ''
	if node.attrs.contains('weak') {
		attributes += 'VWEAK '
	}
	if node.attrs.contains('hidden') {
		attributes += 'VHIDDEN '
	}
	if node.attrs.contains('export') {
		attributes += 'VV_EXP '
	}
	if attr := node.attrs.find_first('_linker_section') {
		attributes += '__attribute__ ((section ("${attr.arg}"))) '
	}
	for field in node.fields {
		name := c_name(field.name)
		if g.pref.skip_unused {
			if field.name !in g.table.used_features.used_globals {
				$if trace_skip_unused_globals ? {
					eprintln('>> skipping unused global name: ${field.name}')
				}
				continue
			}
		}
		styp := g.styp(field.typ)
		mut anon_fn_expr := unsafe { field.expr }
		if field.has_expr && mut anon_fn_expr is ast.AnonFn {
			g.gen_anon_fn_decl(mut anon_fn_expr)
			fn_type_name := g.get_anon_fn_type_name(mut anon_fn_expr, field.name)
			g.global_const_defs[util.no_dots(fn_type_name)] = GlobalConstDef{
				mod:   node.mod
				def:   '${fn_type_name} = ${g.table.sym(field.typ).name}; // global 1'
				order: -1
			}
			continue
		}
		mut def_builder := strings.new_builder(100)
		mut init := ''
		extern := if cextern { 'extern ' } else { '' }
		modifier := if field.is_volatile { ' volatile ' } else { '' }
		def_builder.write_string('${extern}${visibility_kw}${modifier}${styp} ${attributes}${field.name}')
		if cextern {
			def_builder.writeln('; // global 2')
			g.global_const_defs[name] = GlobalConstDef{
				mod:   node.mod
				def:   def_builder.str()
				order: -1
			}
			continue
		}
		if field.has_expr || cinit {
			// `__global x = unsafe { nil }` should still use the simple direct initialisation, `g_main_argv` needs it.
			mut is_simple_unsafe_expr := false
			if field.expr is ast.UnsafeExpr {
				if field.expr.expr is ast.Nil {
					is_simple_unsafe_expr = true
				}
				if field.expr.expr.is_literal() {
					is_simple_unsafe_expr = true
				}
			}
			if g.pref.translated {
				def_builder.write_string(' = ${g.expr_string(field.expr)}')
			} else if (field.expr.is_literal() && should_init) || cinit
				|| (field.expr is ast.ArrayInit && field.expr.is_fixed)
				|| (is_simple_unsafe_expr && should_init) {
				// Simple literals can be initialized right away in global scope in C.
				// e.g. `int myglobal = 10;`
				def_builder.write_string(' = ${g.expr_string(field.expr)}')
			} else {
				// More complex expressions need to be moved to `_vinit()`
				// e.g. `__global ( mygblobal = 'hello ' + world' )`
				if field.name in ['g_main_argc', 'g_main_argv'] {
					init = '\t// skipping ${field.name}, it was initialised in main'
				} else {
					init = '\t${field.name} = ${g.expr_string(field.expr)}; // global 3'
				}
			}
		} else if !g.pref.translated { // don't zero globals from C code
			g.type_default_vars.clear()
			default_initializer := g.type_default(field.typ)
			if default_initializer == '{0}' && should_init {
				def_builder.write_string(' = {0}')
			} else if default_initializer == '{E_STRUCT}' && should_init {
				init = '\tmemcpy(${field.name}, (${styp}){${default_initializer}}, sizeof(${styp})); // global 4'
			} else {
				if field.name !in ['as_cast_type_indexes', 'g_memory_block', 'global_allocator'] {
					decls := g.type_default_vars.str()
					if decls != '' {
						init = '\t${decls}'
					}
					init += '\t${field.name} = *(${styp}*)&((${styp}[]){${default_initializer}}[0]); // global 5'
				}
			}
		}
		def_builder.writeln('; // global 6')
		g.global_const_defs[name] = GlobalConstDef{
			mod:       node.mod
			def:       def_builder.str()
			init:      init
			dep_names: g.table.dependent_names_in_expr(field.expr)
		}
	}
}

fn (mut g Gen) sort_globals_consts() {
	util.timing_start(@METHOD)
	defer {
		util.timing_measure(@METHOD)
	}
	g.sorted_global_const_names.clear()
	mut dep_graph := depgraph.new_dep_graph()
	for var_name, var_info in g.global_const_defs {
		dep_graph.add_with_value(var_name, var_info.dep_names, var_info.order)
	}
	dep_graph_sorted := dep_graph.resolve()
	for order in [-1, 0] {
		for node in dep_graph_sorted.nodes {
			if node.value == order {
				g.sorted_global_const_names << node.name
			}
		}
	}
}
