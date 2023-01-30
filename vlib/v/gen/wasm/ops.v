module wasm

import v.ast
import v.token
import binaryen as wa

fn (mut g Gen) get_wasm_type(typ_ ast.Type) wa.Type {
	typ := ast.mktyp(typ_)
	if typ == ast.void_type_idx {
		return type_none
	}
	if typ.is_real_pointer() {
		return type_i32
	}
	if typ in ast.number_type_idxs {
		return match typ {
			ast.isize_type_idx, ast.usize_type_idx, ast.i8_type_idx, ast.u8_type_idx,
			ast.char_type_idx, ast.rune_type_idx, ast.i16_type_idx, ast.u16_type_idx,
			ast.int_type_idx, ast.u32_type_idx { type_i32 }
			ast.i64_type_idx, ast.u64_type_idx, ast.int_literal_type_idx { type_i64 }
			ast.f32_type_idx { type_f32 }
			ast.f64_type_idx, ast.float_literal_type_idx { type_f64 }
			else { type_i32 }
		}
	}
	if typ == ast.bool_type_idx {
		return type_i32
	}
	g.w_error("get_wasm_type: unreachable type '${typ}'")
}

fn (mut g Gen) infix_from_typ(typ ast.Type, op token.Kind) wa.Op {
	wasm_typ := g.get_wasm_type(typ)

	match wasm_typ {
		type_i32 {
			match op {
				.plus {
					return wa.addint32()
				}
				.minus {
					return wa.subint32()
				}
				.mul {
					return wa.mulint32()
				}
				.mod {
					if typ.is_signed() {
						return wa.remsint32()
					} else {
						return wa.remuint32()
					}
				}
				.div {
					if typ.is_signed() {
						return wa.divsint32()
					} else {
						return wa.divuint32()
					}
				}
				else {}
			}
		}
		type_i64 {
			match op {
				.plus {
					return wa.addint64()
				}
				.minus {
					return wa.subint64()
				}
				.mul {
					return wa.mulint64()
				}
				.mod {
					if typ.is_signed() {
						return wa.remsint64()
					} else {
						return wa.remuint64()
					}
				}
				.div {
					if typ.is_signed() {
						return wa.divsint64()
					} else {
						return wa.divuint64()
					}
				}
				else {}
			}
		}
		type_f32 {
			match op {
				.plus { return wa.addfloat32() }
				.minus { return wa.subfloat32() }
				.mul { return wa.mulfloat32() }
				.div { return wa.divfloat32() }
				else {}
			}
		}
		type_f64 {
			match op {
				.plus { return wa.addfloat64() }
				.minus { return wa.subfloat64() }
				.mul { return wa.mulfloat64() }
				.div { return wa.divfloat64() }
				else {}
			}
		}
		else {}
	}
	g.w_error('bad infix: op `${op}`')
}
