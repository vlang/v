// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module c

import v.ast

fn (mut g Gen) unwrap_generic(typ ast.Type) ast.Type {
	if typ.has_flag(.generic) {
		// NOTE: `convert_generic_type` should not mutate the table.
		//
		// It mutates if the generic type is for example `[]T` and the concrete
		// type is an array type that has not been registered yet.
		//
		// This should have already happened in the checker, since it also calls
		// `convert_generic_type`. `g.table` is made non-mut to make sure
		// no one else can accidentally mutates the table.
		if g.cur_fn != unsafe { nil } && g.cur_fn.generic_names.len > 0 {
			if t_typ := g.table.convert_generic_type(typ, g.cur_fn.generic_names, g.cur_concrete_types) {
				return t_typ
			}
		} else if g.inside_struct_init {
			if g.cur_struct_init_typ != 0 {
				sym := g.table.sym(g.cur_struct_init_typ)
				if sym.info is ast.Struct {
					if sym.info.generic_types.len > 0 {
						generic_names := sym.info.generic_types.map(g.table.sym(it).name)
						if t_typ := g.table.convert_generic_type(typ, generic_names, sym.info.concrete_types) {
							return t_typ
						}
					}
				}
			}
		} else if g.table.sym(typ).kind == .struct {
			// resolve selector `a.foo` where `a` is struct[T] on non generic function
			sym := g.table.sym(typ)
			if sym.info is ast.Struct {
				if sym.info.generic_types.len > 0 {
					generic_names := sym.info.generic_types.map(g.table.sym(it).name)
					if t_typ := g.table.convert_generic_type(typ, generic_names, sym.info.concrete_types) {
						return t_typ
					}

					if t_typ := g.table.convert_generic_type(typ, generic_names, g.cur_concrete_types) {
						return t_typ
					}
				}
			}
		}
	}
	return typ
}

struct Type {
	// typ is the original type
	typ ast.Type        @[required]
	sym &ast.TypeSymbol @[required]
	// unaliased is `typ` once aliased have been resolved
	// it may not contain information such as flags and nr_muls
	unaliased     ast.Type        @[required]
	unaliased_sym &ast.TypeSymbol @[required]
}

// unwrap returns the following variants of a type:
// * generics unwrapped
// * alias unwrapped
fn (mut g Gen) unwrap(typ ast.Type) Type {
	no_generic := g.unwrap_generic(typ)
	no_generic_sym := g.table.sym(no_generic)
	if no_generic_sym.kind != .alias {
		return Type{
			typ:           no_generic
			sym:           no_generic_sym
			unaliased:     no_generic
			unaliased_sym: no_generic_sym
		}
	}
	return Type{
		typ:           no_generic
		sym:           no_generic_sym
		unaliased:     no_generic_sym.parent_idx
		unaliased_sym: g.table.sym(ast.idx_to_type(no_generic_sym.parent_idx))
	}
}

// generate function variable definition, e.g. `void (*var_name) (int, string)`
fn (mut g Gen) fn_var_signature(return_type ast.Type, arg_types []ast.Type, var_name string) string {
	ret_styp := g.styp(return_type)
	mut sig := '${ret_styp} (*${c_name(var_name)}) ('
	for j, arg_typ in arg_types {
		arg_sym := g.table.sym(arg_typ)
		if arg_sym.info is ast.FnType {
			func := arg_sym.info.func
			arg_sig := g.fn_var_signature(func.return_type, func.params.map(it.typ), '')
			sig += arg_sig
		} else {
			arg_styp := g.styp(arg_typ)
			sig += arg_styp
		}
		if j < arg_types.len - 1 {
			sig += ', '
		}
	}
	sig += ')'
	return sig
}

// generate anon fn cname, e.g. `anon_fn_void_int_string`, `anon_fn_void_int_ptr_string`
fn (mut g Gen) anon_fn_cname(return_type ast.Type, arg_types []ast.Type) string {
	ret_styp := g.styp(return_type).replace('*', '_ptr')
	mut sig := 'anon_fn_${ret_styp}_'
	for j, arg_typ in arg_types {
		arg_sym := g.table.sym(arg_typ)
		if arg_sym.info is ast.FnType {
			sig += g.anon_fn_cname(arg_sym.info.func.return_type, arg_sym.info.func.params.map(it.typ))
		} else {
			sig += g.styp(arg_typ).replace('*', '_ptr')
		}
		if j < arg_types.len - 1 {
			sig += '_'
		}
	}
	return sig
}

// escape quotes for string
fn escape_quotes(val string) string {
	bs := '\\'
	unescaped_val := val.replace('${bs}${bs}', '\x01').replace_each([
		"${bs}'",
		"'",
		'${bs}"',
		'"',
	])
	return unescaped_val.replace_each(['\x01', '${bs}${bs}', "'", "${bs}'", '"', '${bs}"'])
}

@[inline]
fn (mut g Gen) dot_or_ptr(val_type ast.Type) string {
	return if val_type.has_flag(.shared_f) {
		'->val.'
	} else if val_type.is_ptr() {
		'->'
	} else {
		'.'
	}
}

fn (mut g Gen) unwrap_option_type(typ ast.Type, name string, is_auto_heap bool) {
	styp := g.base_type(typ)
	if is_auto_heap {
		g.write('(*(${styp}*)${name}->data)')
	} else {
		type_sym := g.table.sym(typ)
		if type_sym.kind == .alias {
			// Alias to Option type
			parent_typ := (type_sym.info as ast.Alias).parent_type
			if parent_typ.has_flag(.option) {
				g.write('*((${g.base_type(parent_typ)}*)')
			}
			g.write('(*(${styp}*)${name}.data)')
			if parent_typ.has_flag(.option) {
				g.write('.data)')
			}
		} else if typ.has_flag(.option_mut_param_t) {
			g.write('(*(${styp}*)${name}->data)')
		} else {
			g.write('(*(${styp}*)${name}.data)')
		}
	}
}
