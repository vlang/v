module wasm

import v.ast
import binaryen as wa

const (
	cast_unsigned = {
		type_i32: {
			type_i64: wa.extenduint32()
			type_f32: wa.convertuint32tofloat32()
			type_f64: wa.convertuint32tofloat64()
		}
		type_i64: {
			type_i32: wa.wrapint64()
			type_f32: wa.convertuint64tofloat32()
			type_f64: wa.convertuint64tofloat64()
		}
		type_f32: {
			type_i32: wa.truncsatufloat32toint32()
			type_i64: wa.truncsatufloat32toint64()
		}
		type_f64: {
			type_i32: wa.truncsatufloat64toint32()
			type_i64: wa.truncsatufloat64toint64()
		}
	}
)

fn (mut g Gen) is_signed(typ ast.Type) bool {
	if typ.is_pure_float() {
		return true
	}
	return typ.is_signed()
}

fn (mut g Gen) unary_cast(from wa.Type, is_signed bool, to wa.Type) wa.Op {
	if is_signed {
		match from {
			type_i32 {
				match to {
					type_i64 { return wa.extendsint32() }
					type_f32 { return wa.convertsint32tofloat32() }
					type_f64 { return wa.convertsint32tofloat64() }
					else {}
				}
			}
			type_i64 {
				match to {
					type_i32 { return wa.wrapint64() }
					type_f32 { return wa.convertsint64tofloat32() }
					type_f64 { return wa.convertsint64tofloat64() }
					else {}
				}
			}
			type_f32 {
				match to {
					type_i32 { return wa.truncsatsfloat32toint32() }
					type_i64 { return wa.truncsatsfloat32toint64() }
					type_f64 { return wa.promotefloat32() }
					else {}
				}
			}
			type_f64 {
				match to {
					type_i32 { return wa.truncsatsfloat64toint32() }
					type_i64 { return wa.truncsatsfloat64toint64() }
					type_f32 { return wa.demotefloat64() }
					else {}
				}
			}
			else {}
		}
	} else {
		match from {
			type_i32 {
				match to {
					type_i64 { return wa.extenduint32() }
					type_f32 { return wa.convertuint32tofloat32() }
					type_f64 { return wa.convertuint32tofloat64() }
					else {}
				}
			}
			type_i64 {
				match to {
					type_i32 { return wa.wrapint64() }
					type_f32 { return wa.convertuint64tofloat32() }
					type_f64 { return wa.convertuint64tofloat64() }
					else {}
				}
			}
			else {}
		}
	}
	g.w_error('bad cast: from ${from} (is signed: ${is_signed}) to ${to}')
}

fn (mut g Gen) cast(expr wa.Expression, from wa.Type, is_signed bool, to wa.Type) wa.Expression {
	if from == to {
		return expr
	}

	// In the official spec, integers are represented in twos complement.
	// WebAssembly does not keep signedness information in it's types
	// and uses instructions with variants for signed or unsigned values.
	//
	// You only need to know if the original type is signed or not to
	// perform casting.

	return wa.unary(g.mod, g.unary_cast(from, is_signed, to), expr)
}
