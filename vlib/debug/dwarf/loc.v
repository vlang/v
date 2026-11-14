module dwarf

import encoding.binary
import encoding.leb128

// Common location operation codes.
pub const dw_op_addr = u8(0x03)
pub const dw_op_deref = u8(0x06)
pub const dw_op_const1u = u8(0x08)
pub const dw_op_const1s = u8(0x09)
pub const dw_op_const2u = u8(0x0a)
pub const dw_op_const2s = u8(0x0b)
pub const dw_op_const4u = u8(0x0c)
pub const dw_op_const4s = u8(0x0d)
pub const dw_op_const8u = u8(0x0e)
pub const dw_op_const8s = u8(0x0f)
pub const dw_op_constu = u8(0x10)
pub const dw_op_consts = u8(0x11)
pub const dw_op_dup = u8(0x12)
pub const dw_op_drop = u8(0x13)
pub const dw_op_over = u8(0x14)
pub const dw_op_pick = u8(0x15)
pub const dw_op_swap = u8(0x16)
pub const dw_op_rot = u8(0x17)
pub const dw_op_xderef = u8(0x18)
pub const dw_op_abs = u8(0x19)
pub const dw_op_and = u8(0x1a)
pub const dw_op_div = u8(0x1b)
pub const dw_op_minus = u8(0x1c)
pub const dw_op_mod = u8(0x1d)
pub const dw_op_mul = u8(0x1e)
pub const dw_op_neg = u8(0x1f)
pub const dw_op_not = u8(0x20)
pub const dw_op_or = u8(0x21)
pub const dw_op_plus = u8(0x22)
pub const dw_op_plus_uconst = u8(0x23)
pub const dw_op_shl = u8(0x24)
pub const dw_op_shr = u8(0x25)
pub const dw_op_shra = u8(0x26)
pub const dw_op_xor = u8(0x27)
pub const dw_op_bra = u8(0x28)
pub const dw_op_eq = u8(0x29)
pub const dw_op_ge = u8(0x2a)
pub const dw_op_gt = u8(0x2b)
pub const dw_op_le = u8(0x2c)
pub const dw_op_lt = u8(0x2d)
pub const dw_op_ne = u8(0x2e)
pub const dw_op_skip = u8(0x2f)
pub const dw_op_reg0 = u8(0x50)
pub const dw_op_reg31 = u8(0x6f)
pub const dw_op_breg0 = u8(0x70)
pub const dw_op_breg31 = u8(0x8f)
pub const dw_op_regx = u8(0x90)
pub const dw_op_fbreg = u8(0x91)
pub const dw_op_bregx = u8(0x92)
pub const dw_op_piece = u8(0x93)
pub const dw_op_deref_size = u8(0x94)
pub const dw_op_xderef_size = u8(0x95)
pub const dw_op_nop = u8(0x96)
pub const dw_op_push_object_address = u8(0x97)
pub const dw_op_call2 = u8(0x98)
pub const dw_op_call4 = u8(0x99)
pub const dw_op_call_ref = u8(0x9a)
pub const dw_op_form_tls_address = u8(0x9b)
pub const dw_op_call_frame_cfa = u8(0x9c)
pub const dw_op_bit_piece = u8(0x9d)
pub const dw_op_implicit_value = u8(0x9e)
pub const dw_op_stack_value = u8(0x9f)

// LocOp is one decoded location operation with raw operands.
pub struct LocOp {
pub:
	op       u8
	operands []u64
}

// parse_loc_expr decodes a location expression into operations.
// Numeric operands are decoded (ULEB/SLEB/byte widths); blocks are
// reported as their length. Unknown opcodes are an error rather
// than a guess.
pub fn parse_loc_expr(data []u8, addr_size u8) ![]LocOp {
	if addr_size != 4 && addr_size != 8 {
		return error('dwarf: unsupported address size ${addr_size}')
	}
	mut out := []LocOp{}
	mut pos := 0
	for pos < data.len {
		op := data[pos]
		pos++
		// lit0..lit31 and reg0..reg31 carry no operands.
		if (op >= 0x30 && op <= 0x4f) || (op >= dw_op_reg0 && op <= dw_op_reg31) {
			out << LocOp{ op: op }
			continue
		}
		// breg0..breg31 carry one SLEB operand.
		if op >= dw_op_breg0 && op <= dw_op_breg31 {
			v, n := leb128.decode_i64(data[pos..])
			pos += n
			out << LocOp{ op: op, operands: [u64(i64(v))] }
			continue
		}
		match op {
			dw_op_addr {
				if pos + int(addr_size) > data.len {
					return error('dwarf: truncated addr op')
				}
				out << LocOp{ op: op, operands: [read_addr(data[pos..], addr_size)] }
				pos += int(addr_size)
			}
			dw_op_const1u, dw_op_pick, dw_op_deref_size, dw_op_xderef_size {
				if pos >= data.len {
					return error('dwarf: truncated 1-byte op')
				}
				out << LocOp{ op: op, operands: [u64(data[pos])] }
				pos++
			}
			dw_op_const1s {
				if pos >= data.len {
					return error('dwarf: truncated 1-byte op')
				}
				out << LocOp{ op: op, operands: [u64(i64(i8(data[pos])))] }
				pos++
			}
			dw_op_const2u {
				if pos + 2 > data.len {
					return error('dwarf: truncated 2-byte op')
				}
				out << LocOp{ op: op, operands: [u64(binary.little_endian_u16(data[pos..pos + 2]))] }
				pos += 2
			}
			dw_op_const2s {
				if pos + 2 > data.len {
					return error('dwarf: truncated 2-byte op')
				}
				out << LocOp{ op: op, operands: [u64(i64(i16(binary.little_endian_u16(data[pos..pos + 2]))))] }
				pos += 2
			}
			dw_op_const4u, dw_op_call4 {
				if pos + 4 > data.len {
					return error('dwarf: truncated 4-byte op')
				}
				out << LocOp{ op: op, operands: [u64(binary.little_endian_u32(data[pos..pos + 4]))] }
				pos += 4
			}
			dw_op_const4s {
				if pos + 4 > data.len {
					return error('dwarf: truncated 4-byte op')
				}
				out << LocOp{ op: op, operands: [u64(i64(i32(binary.little_endian_u32(data[pos..pos + 4]))))] }
				pos += 4
			}
			dw_op_const8u, dw_op_const8s, dw_op_call_ref {
				if pos + 8 > data.len {
					return error('dwarf: truncated 8-byte op')
				}
				out << LocOp{ op: op, operands: [binary.little_endian_u64(data[pos..pos + 8])] }
				pos += 8
			}
			dw_op_constu, dw_op_plus_uconst, dw_op_regx {
				v, n := leb128.decode_u64(data[pos..])
				pos += n
				out << LocOp{ op: op, operands: [v] }
			}
			dw_op_consts, dw_op_fbreg {
				v, n := leb128.decode_i64(data[pos..])
				pos += n
				out << LocOp{ op: op, operands: [u64(i64(v))] }
			}
			dw_op_bregx {
				r, n := leb128.decode_u64(data[pos..])
				pos += n
				off, n2 := leb128.decode_i64(data[pos..])
				pos += n2
				out << LocOp{ op: op, operands: [r, u64(i64(off))] }
			}
			dw_op_bra, dw_op_skip, dw_op_call2 {
				if pos + 2 > data.len {
					return error('dwarf: truncated branch op')
				}
				out << LocOp{ op: op, operands: [u64(binary.little_endian_u16(data[pos..pos + 2]))] }
				pos += 2
			}
			dw_op_piece, dw_op_bit_piece {
				// Piece sizes are validated as present; the piece
				// stack itself is a consumer concern.
				if op == dw_op_piece {
					v, n := leb128.decode_u64(data[pos..])
					pos += n
					out << LocOp{ op: op, operands: [v] }
				} else {
					size, n := leb128.decode_u64(data[pos..])
					pos += n
					off, n2 := leb128.decode_u64(data[pos..])
					pos += n2
					out << LocOp{ op: op, operands: [size, off] }
				}
			}
			dw_op_implicit_value {
				v, n := leb128.decode_u64(data[pos..])
				pos += n
				if pos + int(v) > data.len {
					return error('dwarf: truncated implicit value')
				}
				pos += int(v)
				out << LocOp{ op: op, operands: [v] }
			}
			dw_op_deref, dw_op_dup, dw_op_drop, dw_op_over, dw_op_swap, dw_op_rot,
			dw_op_xderef, dw_op_abs, dw_op_and, dw_op_div, dw_op_minus, dw_op_mod,
			dw_op_mul, dw_op_neg, dw_op_not, dw_op_or, dw_op_plus, dw_op_shl, dw_op_shr,
			dw_op_shra, dw_op_xor, dw_op_eq, dw_op_ge, dw_op_gt, dw_op_le, dw_op_lt,
			dw_op_ne, dw_op_nop, dw_op_push_object_address, dw_op_call_frame_cfa,
			dw_op_stack_value, dw_op_form_tls_address {
				out << LocOp{ op: op }
			}
			else {
				return error('dwarf: unsupported location op ${op}')
			}
		}
	}
	return out
}
