// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module c

import v.ast

fn (mut g Gen) unwrap_generic(typ ast.Type) ast.Type {
	if typ.has_flag(.generic) {
		/*
		resolve_generic_to_concrete should not mutate the table.
		It mutates if the generic type is for example []T and the
		concrete type is an array type that has not been registered
		yet. This should have already happened in the checker, since
		it also calls resolve_generic_to_concrete. g.table is made
		non-mut to make sure no one else can accidentally mutates the table.
		*/
		mut muttable := unsafe { &ast.Table(g.table) }
		if t_typ := muttable.resolve_generic_to_concrete(typ, if unsafe { g.cur_fn != 0 } {
			g.cur_fn.generic_names
		} else {
			[]string{}
		}, g.cur_concrete_types)
		{
			return t_typ
		}
	}
	return typ
}

struct Type {
	// typ is the original type
	typ ast.Type        [required]
	sym &ast.TypeSymbol [required]
	// unaliased is `typ` once aliased have been resolved
	// it may not contain informations such as flags and nr_muls
	unaliased     ast.Type        [required]
	unaliased_sym &ast.TypeSymbol [required]
}

// unwrap returns the following variants of a type:
// * generics unwrapped
// * alias unwrapped
fn (mut g Gen) unwrap(typ ast.Type) Type {
	no_generic := g.unwrap_generic(typ)
	no_generic_sym := g.table.sym(no_generic)
	if no_generic_sym.kind != .alias {
		return Type{
			typ: no_generic
			sym: no_generic_sym
			unaliased: no_generic
			unaliased_sym: no_generic_sym
		}
	}
	return Type{
		typ: no_generic
		sym: no_generic_sym
		unaliased: no_generic_sym.parent_idx
		unaliased_sym: g.table.sym(no_generic_sym.parent_idx)
	}
}
