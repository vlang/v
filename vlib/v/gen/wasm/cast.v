module wasm

import v.ast
import binaryen as wa

const (
	cast_signed = {
		type_i32: {
			type_i64: wa.extendsint32()
			type_f32: wa.convertsint32tofloat32()
			type_f64: wa.convertsint32tofloat64()
		}
		type_i64: {
			type_i32: wa.wrapint64()
			type_f32: wa.convertsint64tofloat32()
			type_f64: wa.convertsint64tofloat64()
		}
		type_f32: {
			type_i32: wa.truncsatsfloat32toint32()
			type_i64: wa.truncsatsfloat32toint64()
			type_f64: wa.promotefloat32()
		}
		type_f64: {
			type_i32: wa.truncsatsfloat64toint32()
			type_i64: wa.truncsatsfloat64toint64()
			type_f32: wa.demotefloat64()
		}
	}
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

	// A large match statement performs better than hardcoded map values.
	// However, it is not as readable. Spy says they stay.

	val := if is_signed {
		wasm.cast_signed[from][to] or { -1 }
	} else {
		wasm.cast_unsigned[from][to] or { -1 }
	}
	if val == -1 {
		println(
"
type_none: ${type_none}
type_auto: ${type_auto}
type_i32: ${type_i32}
type_i64: ${type_i64}
type_f32: ${type_f32}
type_f64: ${type_f64}")
		g.w_error('bad cast: from ${from} (is signed: ${is_signed}) to ${to}')
	}
	return wa.unary(g.mod, val, expr)
}
