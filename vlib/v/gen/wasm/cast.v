module wasm

import v.ast
import v.gen.wasm.binaryen

fn (mut g Gen) is_signed(typ ast.Type) bool {
	if typ.is_pure_float() {
		return true
	}
	return typ.is_signed()
}

fn (mut g Gen) unary_cast(from binaryen.Type, is_signed bool, to binaryen.Type) binaryen.Op {
	if is_signed {
		match from {
			type_i32 {
				match to {
					type_i64 { return binaryen.extendsint32() }
					type_f32 { return binaryen.convertsint32tofloat32() }
					type_f64 { return binaryen.convertsint32tofloat64() }
					else {}
				}
			}
			type_i64 {
				match to {
					type_i32 { return binaryen.wrapint64() }
					type_f32 { return binaryen.convertsint64tofloat32() }
					type_f64 { return binaryen.convertsint64tofloat64() }
					else {}
				}
			}
			type_f32 {
				match to {
					type_i32 { return binaryen.truncsfloat32toint32() }
					type_i64 { return binaryen.truncsfloat32toint64() }
					type_f64 { return binaryen.promotefloat32() }
					else {}
				}
			}
			type_f64 {
				match to {
					type_i32 { return binaryen.truncsfloat64toint32() }
					type_i64 { return binaryen.truncsfloat64toint64() }
					type_f32 { return binaryen.demotefloat64() }
					else {}
				}
			}
			else {}
		}
	} else {
		match from {
			type_i32 {
				match to {
					type_i64 { return binaryen.extenduint32() }
					type_f32 { return binaryen.convertuint32tofloat32() }
					type_f64 { return binaryen.convertuint32tofloat64() }
					else {}
				}
			}
			type_i64 {
				match to {
					type_i32 { return binaryen.wrapint64() }
					type_f32 { return binaryen.convertuint64tofloat32() }
					type_f64 { return binaryen.convertuint64tofloat64() }
					else {}
				}
			}
			else {}
		}
	}
	g.w_error('bad cast: from ${from} (is signed: ${is_signed}) to ${to}')
}

fn (mut g Gen) cast_t(expr binaryen.Expression, from ast.Type, to ast.Type) binaryen.Expression {
	return g.cast(expr, g.get_wasm_type(from), g.is_signed(from), g.get_wasm_type(to))
}

fn (mut g Gen) cast(expr binaryen.Expression, from binaryen.Type, is_signed bool, to binaryen.Type) binaryen.Expression {
	if from == to {
		return expr
	}

	// In the official spec, integers are represented in twos complement.
	// WebAssembly does not keep signedness information in it's types
	// and uses instructions with variants for signed or unsigned values.
	//
	// You only need to know if the original type is signed or not to
	// perform casting.

	return binaryen.unary(g.mod, g.unary_cast(from, is_signed, to), expr)
}
