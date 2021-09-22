// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module c

import v.ast

fn (mut g Gen) unwrap_generic(typ ast.Type) ast.Type {
	if typ.has_flag(.generic) {
		if t_typ := g.table.resolve_generic_to_concrete(typ, g.table.cur_fn.generic_names,
			g.table.cur_concrete_types)
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
	no_generic_sym := g.table.get_type_symbol(no_generic)
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
		unaliased_sym: g.table.get_type_symbol(no_generic_sym.parent_idx)
	}
}
