module wasm

import v.ast
import v.token
import binaryen as wa

const (
	type_none = wa.typenone()
	type_auto = wa.typeauto()
	type_i32  = wa.typeint32()
	type_i64  = wa.typeint64()
	type_f32  = wa.typefloat32()
	type_f64  = wa.typefloat64()
)

// "Register size" types such as int, i64 and bool boil down to their WASM counterparts.
// Structures and unions are pointers, i32.
fn (mut g Gen) get_wasm_type(typ_ ast.Type) wa.Type {
	typ := ast.mktyp(typ_)
	if typ == ast.void_type_idx {
		return wasm.type_none
	}
	if typ.is_real_pointer() {
		return wasm.type_i32
	}
	if typ in ast.number_type_idxs {
		return match typ {
			ast.isize_type_idx, ast.usize_type_idx, ast.i8_type_idx, ast.u8_type_idx,
			ast.char_type_idx, ast.rune_type_idx, ast.i16_type_idx, ast.u16_type_idx,
			ast.int_type_idx, ast.u32_type_idx {
				wasm.type_i32
			}
			ast.i64_type_idx, ast.u64_type_idx, ast.int_literal_type_idx {
				wasm.type_i64
			}
			ast.f32_type_idx {
				wasm.type_f32
			}
			ast.f64_type_idx, ast.float_literal_type_idx {
				wasm.type_f64
			}
			else {
				wasm.type_i32
			}
		}
	}
	if typ == ast.bool_type_idx {
		return wasm.type_i32
	}
	ts := g.table.sym(typ)
	match ts.info {
		ast.Struct {
			g.get_type_size_align(typ)
			return wasm.type_i32 // pointer
		}
		ast.MultiReturn {
			// TODO: cache??
			mut paraml := ts.info.types.map(g.get_wasm_type(it))
			return wa.typecreate(paraml.data, paraml.len)
		}
		ast.Alias {
			return g.get_wasm_type(ts.info.parent_type)
		}
		else {}
	}

	g.w_error("get_wasm_type: unreachable type '${*g.table.sym(typ)}'")
}

fn infix_kind_return_bool(op token.Kind) bool {
	return op in [.eq, .ne, .gt, .lt, .ge, .le]
}

fn (mut g Gen) infix_from_typ(typ ast.Type, op token.Kind) wa.Op {
	wasm_typ := g.get_wasm_type(typ)

	match wasm_typ {
		wasm.type_i32 {
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
				.eq {
					return wa.eqint32()
				}
				.ne {
					return wa.neint32()
				}
				.gt {
					if typ.is_signed() {
						return wa.gtsint32()
					} else {
						return wa.gtuint32()
					}
				}
				.lt {
					if typ.is_signed() {
						return wa.ltsint32()
					} else {
						return wa.ltuint32()
					}
				}
				.ge {
					if typ.is_signed() {
						return wa.gesint32()
					} else {
						return wa.geuint32()
					}
				}
				.le {
					if typ.is_signed() {
						return wa.lesint32()
					} else {
						return wa.leuint32()
					}
				}
				/*.logical_or {
					return wa.orint32() // TODO: logical or
				}*/
				.xor {
					return wa.xorint32()
				}
				.pipe {
					return wa.orint32()
				}
				.amp {
					return wa.andint32()
				}
				.left_shift {
					return wa.shlint32()
				}
				.right_shift {
					return wa.shrsint32()
				}
				.unsigned_right_shift {
					return wa.shruint32()
				}
				else {}
			}
		}
		wasm.type_i64 {
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
				.eq {
					return wa.eqint64()
				}
				.ne {
					return wa.neint64()
				}
				.gt {
					if typ.is_signed() {
						return wa.gtsint64()
					} else {
						return wa.gtuint64()
					}
				}
				.lt {
					if typ.is_signed() {
						return wa.ltsint64()
					} else {
						return wa.ltuint64()
					}
				}
				.ge {
					if typ.is_signed() {
						return wa.gesint64()
					} else {
						return wa.geuint64()
					}
				}
				.le {
					if typ.is_signed() {
						return wa.lesint64()
					} else {
						return wa.leuint64()
					}
				}
				/*.logical_or {
					return wa.orint64() // TODO: logical or
				}*/
				.xor {
					return wa.xorint64()
				}
				.pipe {
					return wa.orint64()
				}
				.amp {
					return wa.andint64()
				}
				.left_shift {
					return wa.shlint64()
				}
				.right_shift {
					return wa.shrsint64()
				}
				.unsigned_right_shift {
					return wa.shruint64()
				}
				else {}
			}
		}
		wasm.type_f32 {
			match op {
				.plus {
					return wa.addfloat32()
				}
				.minus {
					return wa.subfloat32()
				}
				.mul {
					return wa.mulfloat32()
				}
				.div {
					return wa.divfloat32()
				}
				.eq {
					return wa.eqfloat32()
				}
				.ne {
					return wa.nefloat32()
				}
				.gt {
					return wa.gtfloat32()
				}
				.lt {
					return wa.ltfloat32()
				}
				.ge {
					return wa.gefloat32()
				}
				.le {
					return wa.lefloat32()
				}
				else {}
			}
		}
		wasm.type_f64 {
			match op {
				.plus {
					return wa.addfloat64()
				}
				.minus {
					return wa.subfloat64()
				}
				.mul {
					return wa.mulfloat64()
				}
				.div {
					return wa.divfloat64()
				}
				.eq {
					return wa.eqfloat64()
				}
				.ne {
					return wa.nefloat64()
				}
				.gt {
					return wa.gtfloat64()
				}
				.lt {
					return wa.ltfloat64()
				}
				.ge {
					return wa.gefloat64()
				}
				.le {
					return wa.lefloat64()
				}
				else {}
			}
		}
		else {}
	}
	g.w_error('bad infix: op `${op}`')
}
