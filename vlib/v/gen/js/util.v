module js

import v.ast
import arrays

struct Type {
	// typ is the original type
	typ ast.Type        @[required]
	sym &ast.TypeSymbol = unsafe { nil } @[required]
	// unaliased is `typ` once aliased have been resolved
	// it may not contain information such as flags and nr_muls
	unaliased     ast.Type        @[required]
	unaliased_sym &ast.TypeSymbol = unsafe { nil } @[required]
}

fn (a Type) == (b Type) bool {
	return a.unaliased == b.unaliased
}

fn (a Type) < (b Type) bool {
	return a.unaliased_sym.name < b.unaliased_sym.name
}

// unwrap returns the following variants of a type:
// * generics unwrapped
// * alias unwrapped
fn (mut g JsGen) unwrap(typ ast.Type) Type {
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
	no_generic_unaliased := g.table.unaliased_type(no_generic)
	return Type{
		typ:           no_generic
		sym:           no_generic_sym
		unaliased:     no_generic_unaliased
		unaliased_sym: g.table.sym(no_generic_unaliased)
	}
}

fn (mut g JsGen) unwrap_sum_type(typ ast.Type) []Type {
	mut types := []Type{}
	sym := g.table.sym(typ)
	if sym.info is ast.SumType {
		for v in sym.info.variants {
			types << g.unwrap_sum_type(v)
		}
	} else {
		types << g.unwrap(typ)
	}
	return arrays.distinct(types)
}
