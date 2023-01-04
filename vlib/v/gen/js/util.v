module js

import v.ast

struct Type {
	// typ is the original type
	typ ast.Type        [required]
	sym &ast.TypeSymbol [required] = unsafe { nil }
	// unaliased is `typ` once aliased have been resolved
	// it may not contain informations such as flags and nr_muls
	unaliased     ast.Type        [required]
	unaliased_sym &ast.TypeSymbol [required] = unsafe { nil }
}

// unwrap returns the following variants of a type:
// * generics unwrapped
// * alias unwrapped
fn (mut g JsGen) unwrap(typ ast.Type) Type {
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
