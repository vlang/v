module wasm

import v.ast
import v.token
import v.gen.wasm.binaryen

const (
	type_none = binaryen.typenone()
	type_auto = binaryen.typeauto()
	type_i32  = binaryen.typeint32()
	type_i64  = binaryen.typeint64()
	type_f32  = binaryen.typefloat32()
	type_f64  = binaryen.typefloat64()
)

// "Register size" types such as int, i64 and bool boil down to their WASM counterparts.
// Structures and unions are pointers, i32.
fn (mut g Gen) get_wasm_type(typ_ ast.Type) binaryen.Type {
	typ := ast.mktyp(typ_)
	if typ == ast.void_type_idx {
		return wasm.type_none
	}
	if typ.is_any_kind_of_pointer() {
		g.needs_stack = true
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
			return binaryen.typecreate(paraml.data, paraml.len)
		}
		ast.Alias {
			return g.get_wasm_type(ts.info.parent_type)
		}
		ast.ArrayFixed {
			return wasm.type_i32
		}
		ast.Enum {
			return g.get_wasm_type(ts.info.typ)
		}
		else {}
	}

	g.w_error("get_wasm_type: unreachable type '${*g.table.sym(typ)}' ${ts.info}")
}

fn infix_kind_return_bool(op token.Kind) bool {
	return op in [.eq, .ne, .gt, .lt, .ge, .le]
}

fn (mut g Gen) infix_from_typ(typ ast.Type, op token.Kind) binaryen.Op {
	wasm_typ := g.get_wasm_type(typ)

	match wasm_typ {
		wasm.type_i32 {
			match op {
				.plus {
					return binaryen.addint32()
				}
				.minus {
					return binaryen.subint32()
				}
				.mul {
					return binaryen.mulint32()
				}
				.mod {
					if typ.is_signed() {
						return binaryen.remsint32()
					} else {
						return binaryen.remuint32()
					}
				}
				.div {
					if typ.is_signed() {
						return binaryen.divsint32()
					} else {
						return binaryen.divuint32()
					}
				}
				.eq {
					return binaryen.eqint32()
				}
				.ne {
					return binaryen.neint32()
				}
				.gt {
					if typ.is_signed() {
						return binaryen.gtsint32()
					} else {
						return binaryen.gtuint32()
					}
				}
				.lt {
					if typ.is_signed() {
						return binaryen.ltsint32()
					} else {
						return binaryen.ltuint32()
					}
				}
				.ge {
					if typ.is_signed() {
						return binaryen.gesint32()
					} else {
						return binaryen.geuint32()
					}
				}
				.le {
					if typ.is_signed() {
						return binaryen.lesint32()
					} else {
						return binaryen.leuint32()
					}
				}
				/*.logical_or {
					return binaryen.orint32() // TODO: logical or
				}*/
				.xor {
					return binaryen.xorint32()
				}
				.pipe {
					return binaryen.orint32()
				}
				.amp {
					return binaryen.andint32()
				}
				.left_shift {
					return binaryen.shlint32()
				}
				.right_shift {
					return binaryen.shrsint32()
				}
				.unsigned_right_shift {
					return binaryen.shruint32()
				}
				else {}
			}
		}
		wasm.type_i64 {
			match op {
				.plus {
					return binaryen.addint64()
				}
				.minus {
					return binaryen.subint64()
				}
				.mul {
					return binaryen.mulint64()
				}
				.mod {
					if typ.is_signed() {
						return binaryen.remsint64()
					} else {
						return binaryen.remuint64()
					}
				}
				.div {
					if typ.is_signed() {
						return binaryen.divsint64()
					} else {
						return binaryen.divuint64()
					}
				}
				.eq {
					return binaryen.eqint64()
				}
				.ne {
					return binaryen.neint64()
				}
				.gt {
					if typ.is_signed() {
						return binaryen.gtsint64()
					} else {
						return binaryen.gtuint64()
					}
				}
				.lt {
					if typ.is_signed() {
						return binaryen.ltsint64()
					} else {
						return binaryen.ltuint64()
					}
				}
				.ge {
					if typ.is_signed() {
						return binaryen.gesint64()
					} else {
						return binaryen.geuint64()
					}
				}
				.le {
					if typ.is_signed() {
						return binaryen.lesint64()
					} else {
						return binaryen.leuint64()
					}
				}
				/*.logical_or {
					return binaryen.orint64() // TODO: logical or
				}*/
				.xor {
					return binaryen.xorint64()
				}
				.pipe {
					return binaryen.orint64()
				}
				.amp {
					return binaryen.andint64()
				}
				.left_shift {
					return binaryen.shlint64()
				}
				.right_shift {
					return binaryen.shrsint64()
				}
				.unsigned_right_shift {
					return binaryen.shruint64()
				}
				else {}
			}
		}
		wasm.type_f32 {
			match op {
				.plus {
					return binaryen.addfloat32()
				}
				.minus {
					return binaryen.subfloat32()
				}
				.mul {
					return binaryen.mulfloat32()
				}
				.div {
					return binaryen.divfloat32()
				}
				.eq {
					return binaryen.eqfloat32()
				}
				.ne {
					return binaryen.nefloat32()
				}
				.gt {
					return binaryen.gtfloat32()
				}
				.lt {
					return binaryen.ltfloat32()
				}
				.ge {
					return binaryen.gefloat32()
				}
				.le {
					return binaryen.lefloat32()
				}
				else {}
			}
		}
		wasm.type_f64 {
			match op {
				.plus {
					return binaryen.addfloat64()
				}
				.minus {
					return binaryen.subfloat64()
				}
				.mul {
					return binaryen.mulfloat64()
				}
				.div {
					return binaryen.divfloat64()
				}
				.eq {
					return binaryen.eqfloat64()
				}
				.ne {
					return binaryen.nefloat64()
				}
				.gt {
					return binaryen.gtfloat64()
				}
				.lt {
					return binaryen.ltfloat64()
				}
				.ge {
					return binaryen.gefloat64()
				}
				.le {
					return binaryen.lefloat64()
				}
				else {}
			}
		}
		else {}
	}
	g.w_error('bad infix: op `${op}`')
}
