// Copyright (c) 2023 l-m.dev. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module wasm

import encoding.leb128

fn (mut func Function) u32(v u32) {
	func.code << leb128.encode_u32(v)
}

fn (mut func Function) blocktype(typ FuncType) {
	if typ.parameters.len == 0 {
		if typ.results.len == 0 {
			func.code << 0x40 // empty type
			return
		} else if typ.results.len == 1 {
			func.code << u8(typ.results[0]) // encode a single result type
			return
		}
	}

	// encode full type
	tidx := func.mod.new_functype(typ)
	func.code << leb128.encode_i32(i32(tidx))
}

pub type PatchPos = int

// patch_pos returns a `PatchPos` for use with `patch`.
pub fn (func Function) patch_pos() PatchPos {
	return func.code.len
}

// patch "patches" the code generated starting from the last `patch_pos` call in `begin` to `loc`.
//
// ```v
// start := func.patch_pos()
//
// ...
//
// patch_block := func.patch_pos()
// {
//     func.i32_const(10)
//     func.local_set(idx)
// }
// func.patch(start, patch_block) // will patch code to the `start`.
// // func.code[patch_block..]
// ```
pub fn (mut func Function) patch(loc PatchPos, begin PatchPos) {
	if loc == begin {
		return
	}
	assert loc < begin

	v := func.code[begin..].clone()
	func.code.trim(begin)
	func.code.insert(int(loc), v)

	for mut patch in func.patches {
		if patch.pos >= begin {
			patch.pos -= begin - loc
		} else if patch.pos >= loc {
			patch.pos += func.code.len - begin
		}
	}

	func.patches.sort(a.pos < b.pos)

	/*
	lenn := begin
	diff := loc - begin

	for mut patch in func.patches {
		if patch.pos >= begin {
			patch.pos += diff
		}
		if patch.pos <= lenn {
			continue
		}
		delta := patch.pos - lenn
		patch.pos = loc + delta
	}

	func.patches.sort(a.pos < b.pos)*/
}

// new_local creates a function local and returns it's index.
// See `local_get`, `local_set`, `local_tee`.
pub fn (mut func Function) new_local(v ValType) LocalIndex {
	ret := func.locals.len
	func.locals << FunctionLocal{
		typ: v
	}
	return ret
}

// new_local_named creates a function local with a name and returns it's index.
// The `name` is used in debug information, where applicable.
// See `local_get`, `local_set`, `local_tee`.
pub fn (mut func Function) new_local_named(v ValType, name string) LocalIndex {
	ret := func.locals.len
	func.locals << FunctionLocal{
		typ:  v
		name: name
	}
	return ret
}

// i32_const places a constant i32 value on the stack.
// WebAssembly instruction: `i32.const`.
pub fn (mut func Function) i32_const(v i32) {
	func.code << 0x41 // i32.const
	func.code << leb128.encode_i32(v)
}

// i64_const places a constant i64 value on the stack.
// WebAssembly instruction: `i64.const`.
pub fn (mut func Function) i64_const(v i64) {
	func.code << 0x42 // i64.const
	func.code << leb128.encode_i64(v)
}

// f32_const places a constant f32 value on the stack.
// WebAssembly instruction: `f32.const`.
pub fn (mut func Function) f32_const(v f32) {
	func.code << 0x43 // f32.const
	push_f32(mut func.code, v)
}

// f64_const places a constant f64 value on the stack.
// WebAssembly instruction: `f64.const`.
pub fn (mut func Function) f64_const(v f64) {
	func.code << 0x44 // f64.const
	push_f64(mut func.code, v)
}

// local_get places the value of the local at the index `local` on the stack.
// WebAssembly instruction: `local.get`.
pub fn (mut func Function) local_get(local LocalIndex) {
	func.code << 0x20 // local.get
	func.u32(u32(local))
}

// local_get sets the local at the index `local` to the value on the stack.
// WebAssembly instruction: `local.set`.
pub fn (mut func Function) local_set(local LocalIndex) {
	func.code << 0x21 // local.set
	func.u32(u32(local))
}

// local_tee sets the local at the index `local` to the value on the stack, then places it's value on the stack.
// WebAssembly instruction: `local.tee`.
pub fn (mut func Function) local_tee(local LocalIndex) {
	func.code << 0x22 // local.tee
	func.u32(u32(local))
}

type GlobalIndices = GlobalImportIndex | GlobalIndex

// global_get places the value of the global at the index `global` on the stack.
// WebAssembly instruction: `global.get`.
pub fn (mut func Function) global_get(global GlobalIndices) {
	func.code << 0x23 // global.get
	match global {
		GlobalIndex {
			func.patches << FunctionGlobalPatch{
				idx: global
				pos: func.code.len
			}
		}
		GlobalImportIndex {
			func.u32(u32(global))
		}
	}
}

// global_set sets the global at the index `global` to the value on the stack.
// WebAssembly instruction: `global.set`.
pub fn (mut func Function) global_set(global GlobalIndices) {
	func.code << 0x24 // global.set
	match global {
		GlobalIndex {
			func.patches << FunctionGlobalPatch{
				idx: global
				pos: func.code.len
			}
		}
		GlobalImportIndex {
			func.u32(u32(global))
		}
	}
}

// drop drops the value on the stack
// WebAssembly instruction: `drop`.
pub fn (mut func Function) drop() {
	func.code << 0x1A
}

// c_select selects one of its first two operands based on an i32 condition.
// WebAssembly instruction: `select`.
pub fn (mut func Function) c_select() {
	func.code << 0x1B
}

// add adds two values on the stack with type `typ`.
// WebAssembly instructions: `i32|i64|f32|f64.add`.
pub fn (mut func Function) add(typ NumType) {
	match typ {
		.i32_t { func.code << 0x6A } // i32.add
		.i64_t { func.code << 0x7C } // i64.add
		.f32_t { func.code << 0x92 } // f32.add
		.f64_t { func.code << 0xA0 } // f64.add
	}
}

// sub subtracts two values on the stack with type `typ`.
// WebAssembly instructions: `i32|i64|f32|f64.sub`.
pub fn (mut func Function) sub(typ NumType) {
	match typ {
		.i32_t { func.code << 0x6B } // i32.sub
		.i64_t { func.code << 0x7D } // i64.sub
		.f32_t { func.code << 0x93 } // f32.sub
		.f64_t { func.code << 0xA1 } // f64.sub
	}
}

// mul multiplies two values on the stack with type `typ`.
// WebAssembly instructions: `i32|i64|f32|f64.mul`.
pub fn (mut func Function) mul(typ NumType) {
	match typ {
		.i32_t { func.code << 0x6C } // i32.mul
		.i64_t { func.code << 0x7E } // i64.mul
		.f32_t { func.code << 0x94 } // f32.mul
		.f64_t { func.code << 0xA2 } // f64.mul
	}
}

// div divides two values on the stack with type `typ`, with respect to `is_signed`.
// WebAssembly instructions: `i32|i64.div_s`, `i32|i64.div_u`, `f32|f64.div`.
pub fn (mut func Function) div(typ NumType, is_signed bool) {
	match typ {
		.i32_t {
			if is_signed {
				func.code << 0x6D // i32.div_s
			} else {
				func.code << 0x6E // i32.div_u
			}
		}
		.i64_t {
			if is_signed {
				func.code << 0x7F // i64.div_s
			} else {
				func.code << 0x80 // i64.div_u
			}
		}
		.f32_t {
			func.code << 0x95 // f32.div
		}
		.f64_t {
			func.code << 0xA3 // f64.div
		}
	}
}

// rem takes the remainder of two values on the stack with type `typ`, with respect to `is_signed`.
// WebAssembly instructions: `i32|i64.rem_s`, `i32|i64.rem_u`.
pub fn (mut func Function) rem(typ NumType, is_signed bool) {
	assert typ in [.i32_t, .i64_t]

	match typ {
		.i32_t {
			if is_signed {
				func.code << 0x6F // i32.rem_s
			} else {
				func.code << 0x70 // i32.rem_u
			}
		}
		.i64_t {
			if is_signed {
				func.code << 0x81 // i64.rem_s
			} else {
				func.code << 0x82 // i64.rem_u
			}
		}
		else {}
	}
}

// and takes the bitwise and of two values on the stack with type `typ`.
// WebAssembly instruction: `i32|i64.and`.
pub fn (mut func Function) b_and(typ NumType) {
	assert typ in [.i32_t, .i64_t]

	match typ {
		.i32_t { func.code << 0x71 } // i32.and
		.i64_t { func.code << 0x83 } // i64.and
		else {}
	}
}

// or takes the bitwise or of two values on the stack with type `typ`.
// WebAssembly instruction: `i32|i64.or`.
pub fn (mut func Function) b_or(typ NumType) {
	assert typ in [.i32_t, .i64_t]

	match typ {
		.i32_t { func.code << 0x72 } // i32.or
		.i64_t { func.code << 0x84 } // i64.or
		else {}
	}
}

// xor takes the bitwise xor of two values on the stack with type `typ`.
// WebAssembly instruction: `i32|i64.xor`.
pub fn (mut func Function) b_xor(typ NumType) {
	assert typ in [.i32_t, .i64_t]

	match typ {
		.i32_t { func.code << 0x73 } // i32.xor
		.i64_t { func.code << 0x85 } // i64.xor
		else {}
	}
}

// shl performs bitwise left-shift on a value with type `typ`.
// WebAssembly instruction: `i32|i64.shl`.
pub fn (mut func Function) b_shl(typ NumType) {
	assert typ in [.i32_t, .i64_t]

	match typ {
		.i32_t { func.code << 0x74 } // i32.shl
		.i64_t { func.code << 0x86 } // i64.shl
		else {}
	}
}

// shr performs bitwise right-shift on a value with type `typ`, with respect to `is_signed`.
// WebAssembly instructions: `i32|i64.shr_s`, `i32|i64.shr_u`.
pub fn (mut func Function) b_shr(typ NumType, is_signed bool) {
	assert typ in [.i32_t, .i64_t]

	match typ {
		.i32_t {
			if is_signed {
				func.code << 0x75 // i32.shr_s
			} else {
				func.code << 0x76 // i32.shr_u
			}
		}
		.i64_t {
			if is_signed {
				func.code << 0x87 // i64.shr_s
			} else {
				func.code << 0x88 // i64.shr_u
			}
		}
		else {}
	}
}

// clz counts the amount of leading zeros in the numbers binary representation.
// WebAssembly instruction: `i32|i64.clz`.
pub fn (mut func Function) clz(typ NumType) {
	assert typ in [.i32_t, .i64_t]

	match typ {
		.i32_t { func.code << 0x67 } // i32.clz
		.i64_t { func.code << 0x79 } // i64.clz
		else {}
	}
}

// ctz counts the amount of trailing zeros in the numbers binary representation.
// WebAssembly instruction: `i32|i64.ctz`.
pub fn (mut func Function) ctz(typ NumType) {
	assert typ in [.i32_t, .i64_t]

	match typ {
		.i32_t { func.code << 0x68 } // i32.ctz
		.i64_t { func.code << 0x7A } // i64.ctz
		else {}
	}
}

// popcnt counts the amount of 1s in a numbers binary representation.
// WebAssembly instruction: `i32|i64.popcnt`.
pub fn (mut func Function) popcnt(typ NumType) {
	assert typ in [.i32_t, .i64_t]

	match typ {
		.i32_t { func.code << 0x69 } // i32.popcnt
		.i64_t { func.code << 0x7B } // i64.popcnt
		else {}
	}
}

// rotl performs bitwise left-rotate on a value with type `typ`.
// WebAssembly instruction: `i32|i64.rotl`.
pub fn (mut func Function) rotl(typ NumType) {
	assert typ in [.i32_t, .i64_t]

	match typ {
		.i32_t { func.code << 0x77 } // i32.rotl
		.i64_t { func.code << 0x89 } // i64.rotl
		else {}
	}
}

// rotr performs bitwise right-rotate on a value with type `typ`.
// WebAssembly instruction: `i32|i64.rotr`.
pub fn (mut func Function) rotr(typ NumType) {
	assert typ in [.i32_t, .i64_t]

	match typ {
		.i32_t { func.code << 0x78 } // i32.rotr
		.i64_t { func.code << 0xA8 } // i64.rotr
		else {}
	}
}

// abs gets the absolute value of a float with type `typ`.
// WebAssembly instruction: `f32|f64.abs`.
pub fn (mut func Function) abs(typ NumType) {
	assert typ in [.f32_t, .f64_t]

	match typ {
		.f32_t { func.code << 0x8B } // f32.abs
		.f64_t { func.code << 0x99 } // f64.abs
		else {}
	}
}

// neg negates the value of a float with type `typ`.
// WebAssembly instruction: `f32|f64.neg`.
pub fn (mut func Function) neg(typ NumType) {
	assert typ in [.f32_t, .f64_t]

	match typ {
		.f32_t { func.code << 0x8C } // f32.neg
		.f64_t { func.code << 0x9A } // f64.neg
		else {}
	}
}

// ceil rounds up the value of a float with type `typ` to the nearest integer.
// WebAssembly instruction: `f32|f64.ceil`.
pub fn (mut func Function) ceil(typ NumType) {
	assert typ in [.f32_t, .f64_t]

	match typ {
		.f32_t { func.code << 0x8D } // f32.ceil
		.f64_t { func.code << 0x9B } // f64.ceil
		else {}
	}
}

// floor rounds down the value of a float with type `typ` to the nearest integer.
// WebAssembly instruction: `f32|f64.floor`.
pub fn (mut func Function) floor(typ NumType) {
	assert typ in [.f32_t, .f64_t]

	match typ {
		.f32_t { func.code << 0x8E } // f32.floor
		.f64_t { func.code << 0x9C } // f64.floor
		else {}
	}
}

// trunc discards the fractional part of the value of a float with type `typ`.
// WebAssembly instruction: `f32|f64.trunc`.
pub fn (mut func Function) trunc(typ NumType) {
	assert typ in [.f32_t, .f64_t]

	match typ {
		.f32_t { func.code << 0x8F } // f32.trunc
		.f64_t { func.code << 0x9D } // f64.trunc
		else {}
	}
}

// nearest rounds the value of a float with type `typ` to the nearest integer.
// WebAssembly instruction: `f32|f64.nearest`.
pub fn (mut func Function) nearest(typ NumType) {
	assert typ in [.f32_t, .f64_t]

	match typ {
		.f32_t { func.code << 0x90 } // f32.nearest
		.f64_t { func.code << 0x9E } // f64.nearest
		else {}
	}
}

// sqrt performs the square root on the value of a float with type `typ`.
// WebAssembly instruction: `f32|f64.sqrt`.
pub fn (mut func Function) sqrt(typ NumType) {
	assert typ in [.f32_t, .f64_t]

	match typ {
		.f32_t { func.code << 0x91 } // f32.sqrt
		.f64_t { func.code << 0x9F } // f64.sqrt
		else {}
	}
}

// min gets the smaller value of two floats with type `typ`.
// WebAssembly instruction: `f32|f64.min`.
pub fn (mut func Function) min(typ NumType) {
	assert typ in [.f32_t, .f64_t]

	match typ {
		.f32_t { func.code << 0x96 } // f32.min
		.f64_t { func.code << 0xA4 } // f64.min
		else {}
	}
}

// max gets the higher value of two floats with type `typ`.
// WebAssembly instruction: `f32|f64.max`.
pub fn (mut func Function) max(typ NumType) {
	assert typ in [.f32_t, .f64_t]

	match typ {
		.f32_t { func.code << 0x97 } // f32.max
		.f64_t { func.code << 0xA5 } // f64.max
		else {}
	}
}

// copysign copies the sign bit of one float value to another float, both with type `typ`.
// WebAssembly instruction: `f32|f64.copysign`.
pub fn (mut func Function) copysign(typ NumType) {
	assert typ in [.f32_t, .f64_t]

	match typ {
		.f32_t { func.code << 0x98 } // f32.copysign
		.f64_t { func.code << 0xA6 } // f64.copysign
		else {}
	}
}

// eqz checks if the value with type `typ` is equal to zero, places an i32 boolean value on the stack.
// WebAssembly instruction: `i32|i64.eqz`.
pub fn (mut func Function) eqz(typ NumType) {
	assert typ in [.i32_t, .i64_t]

	match typ {
		.i32_t { func.code << 0x45 } // i32.eqz
		.i64_t { func.code << 0x50 } // i64.eqz
		else {}
	}
}

// eq checks if two values with type `typ` are equal, places an i32 boolean value on the stack.
// WebAssembly instruction: `i32|i64|f32|f64.eq`.
pub fn (mut func Function) eq(typ NumType) {
	match typ {
		.i32_t { func.code << 0x46 } // i32.eq
		.i64_t { func.code << 0x51 } // i64.eq
		.f32_t { func.code << 0x5B } // f32.eq
		.f64_t { func.code << 0x61 } // f64.eq
	}
}

// ne checks if two values with type `typ` are not equal, places an i32 boolean value on the stack.
// WebAssembly instruction: `i32|i64|f32|f64.ne`.
pub fn (mut func Function) ne(typ NumType) {
	match typ {
		.i32_t { func.code << 0x47 } // i32.ne
		.i64_t { func.code << 0x52 } // i64.ne
		.f32_t { func.code << 0x5C } // f32.ne
		.f64_t { func.code << 0x62 } // f64.ne
	}
}

// lt checks if two values with type `typ` with respect to `is_signed` are less than another, places an i32 boolean value on the stack.
// WebAssembly instructions: `i32|i64.lt_s`, `i32|i64.lt_u`, `f32|f64.lt`.
pub fn (mut func Function) lt(typ NumType, is_signed bool) {
	match typ {
		.i32_t {
			if is_signed {
				func.code << 0x48 // i32.lt_s
			} else {
				func.code << 0x49 // i32.lt_u
			}
		}
		.i64_t {
			if is_signed {
				func.code << 0x53 // i64.lt_s
			} else {
				func.code << 0x54 // i64.lt_u
			}
		}
		.f32_t {
			func.code << 0x5D // f32.lt
		}
		.f64_t {
			func.code << 0x63 // f64.lt
		}
	}
}

// gt checks if two values with type `typ` with respect to `is_signed` are greater than another, places an i32 boolean value on the stack.
// WebAssembly instructions: `i32|i64.gt_s`, `i32|i64.gt_u`, `f32|f64.gt`.
pub fn (mut func Function) gt(typ NumType, is_signed bool) {
	match typ {
		.i32_t {
			if is_signed {
				func.code << 0x4A // i32.gt_s
			} else {
				func.code << 0x4B // i32.gt_u
			}
		}
		.i64_t {
			if is_signed {
				func.code << 0x55 // i64.gt_s
			} else {
				func.code << 0x56 // i64.gt_u
			}
		}
		.f32_t {
			func.code << 0x5E
		} // f32.gt
		.f64_t {
			func.code << 0x64
		} // f64.gt
	}
}

// le checks if two values with type `typ` with respect to `is_signed` are less than or equal to another, places an i32 boolean value on the stack.
// WebAssembly instructions: `i32|i64.le_s`, `i32|i64.le_u`, `f32|f64.le`.
pub fn (mut func Function) le(typ NumType, is_signed bool) {
	match typ {
		.i32_t {
			if is_signed {
				func.code << 0x4C // i32.le_s
			} else {
				func.code << 0x4D // i32.le_u
			}
		}
		.i64_t {
			if is_signed {
				func.code << 0x57 // i64.le_s
			} else {
				func.code << 0x58 // i64.le_u
			}
		}
		.f32_t {
			func.code << 0x5F
		} // f32.le
		.f64_t {
			func.code << 0x65
		} // f64.le
	}
}

// ge checks if two values with type `typ` with respect to `is_signed` are greater than or equal to another, places an i32 boolean value on the stack.
// WebAssembly instructions: `i32|i64.ge_s`, `i32|i64.ge_u`, `f32|f64.ge`.
pub fn (mut func Function) ge(typ NumType, is_signed bool) {
	match typ {
		.i32_t {
			if is_signed {
				func.code << 0x4E // i32.ge_s
			} else {
				func.code << 0x4F // i32.ge_u
			}
		}
		.i64_t {
			if is_signed {
				func.code << 0x59 // i64.ge_s
			} else {
				func.code << 0x5A // i64.ge_u
			}
		}
		.f32_t {
			func.code << 0x60
		} // f32.ge
		.f64_t {
			func.code << 0x66
		} // f64.ge
	}
}

// sign_extend8 extends the value of a 8-bit integer of type `typ`.
// WebAssembly instruction: `i32|i64.extend8_s`.
pub fn (mut func Function) sign_extend8(typ ValType) {
	assert typ in [.i32_t, .i64_t]

	match typ {
		.i32_t { func.code << 0xC0 } // i32.extend8_s
		.i64_t { func.code << 0xC2 } // i64.extend8_s
		else {}
	}
}

// sign_extend16 extends the value of a 16-bit integer of type `typ`.
// WebAssembly instruction: `i32|i64.extend16_s`.
pub fn (mut func Function) sign_extend16(typ ValType) {
	assert typ in [.i32_t, .i64_t]

	match typ {
		.i32_t { func.code << 0xC1 } // i32.extend16_s
		.i64_t { func.code << 0xC3 } // i64.extend16_s
		else {}
	}
}

// sign_extend32_i64 extends the value of a 32-bit integer of type i64.
// WebAssembly instruction: `i64.extend32_s`.
pub fn (mut func Function) sign_extend32() {
	func.code << 0xC4 // i64.extend32_s
}

// cast casts a value of type `a` with respect to `is_signed`, to type `b`.
// A generic utility function over a large amount of WebAssembly instructions.
// Note: This function uses non-trapping float conversion operators, see `cast_trapping` to use opcodes that cause a runtime exception.
// WebAssembly instructions:
// - `i32|i64.trunc_sat_f32_s`, `i32|i64.trunc_sat_f64_s`.
// - `f32.demote_f64`, `f64.promote_f32`.
// - `i32.wrap_i64`, `i64.extend_i32_s`, `i64.extend_i32_u`.
// - `f32|f64.convert_i32_s`, `f32|f64.convert_i32_u`.
// - `f32|f64.convert_i64_s`, `f32|f64.convert_i64_u`.
pub fn (mut func Function) cast(a NumType, is_signed bool, b NumType) {
	if a in [.f32_t, .f64_t] {
		if a == .f32_t {
			match b {
				.i32_t {
					func.code << 0xFC // sat opcode
					func.code << 0x00 // i32.trunc_sat_f32_s
				}
				.i64_t {
					func.code << 0xFC // sat opcode
					func.code << 0x04 // i64.trunc_sat_f32_s
				}
				.f64_t {
					func.code << 0xBB // f64.promote_f32
				}
				else {}
			}
		} else {
			match b {
				.i32_t {
					func.code << 0xFC // sat opcode
					func.code << 0x02 // i32.trunc_sat_f64_s
				}
				.i64_t {
					func.code << 0xFC // sat opcode
					func.code << 0x06 // i64.trunc_sat_f64_s
				}
				.f32_t {
					func.code << 0xB6 // f32.demote_f64
				}
				else {}
			}
		}
		return
	}

	if a == .i64_t && b == .i32_t {
		func.code << 0xA7 // i32.wrap_i64
		return
	}

	if is_signed {
		match a {
			.i32_t {
				match b {
					.i64_t {
						func.code << 0xAC // i64.extend_i32_s
					}
					.f32_t {
						func.code << 0xB2 // f32.convert_i32_s
					}
					.f64_t {
						func.code << 0xB7 // f64.convert_i32_s
					}
					else {}
				}
			}
			.i64_t {
				match b {
					.f32_t {
						func.code << 0xB4 // f32.convert_i64_s
					}
					.f64_t {
						func.code << 0xB9 // f64.convert_i64_s
					}
					else {}
				}
			}
			else {}
		}
	} else {
		match a {
			.i32_t {
				match b {
					.i64_t {
						func.code << 0xAD // i64.extend_i32_u
					}
					.f32_t {
						func.code << 0xB3 // f32.convert_i32_u
					}
					.f64_t {
						func.code << 0xB8 // f64.convert_i32_u
					}
					else {}
				}
			}
			.i64_t {
				match b {
					.f32_t {
						func.code << 0xB5 // f32.convert_i64_u
					}
					.f64_t {
						func.code << 0xBA // f64.convert_i64_u
					}
					else {}
				}
			}
			else {}
		}
	}
}

// cast_trapping casts a value of type `a` with respect to `is_signed`, to type `b`.
// A generic utility function over a large amount of WebAssembly instructions.
// Note: This function uses trapping float conversion operators, see `cast` to use opcodes that do NOT cause a runtime exception.
// WebAssembly instructions:
// - `i32|i64.trunc_f32_s`, `i32|i64.trunc_f64_s`.
// - See function `cast` for the rest.
pub fn (mut func Function) cast_trapping(a NumType, is_signed bool, b NumType) {
	if a in [.f32_t, .f64_t] {
		if is_signed {
			if a == .f32_t {
				match b {
					.i32_t {
						func.code << 0xA8 // i32.trunc_f32_s
						return
					}
					.i64_t {
						func.code << 0xAE // i64.trunc_f32_s
						return
					}
					else {}
				}
			} else {
				match b {
					.i32_t {
						func.code << 0xAA // i32.trunc_f64_s
						return
					}
					.i64_t {
						func.code << 0xB0 // i64.trunc_f64_s
						return
					}
					else {}
				}
			}
		} else {
			if a == .f32_t {
				match b {
					.i32_t {
						func.code << 0xA9 // i32.trunc_f32_u
						return
					}
					.i64_t {
						func.code << 0xAF // i64.trunc_f32_u
						return
					}
					else {}
				}
			} else {
				match b {
					.i32_t {
						func.code << 0xAB // i32.trunc_f64_u
						return
					}
					.i64_t {
						func.code << 0xB1 // i64.trunc_f64_u
						return
					}
					else {}
				}
			}
		}
	}

	func.cast(a, is_signed, b)
}

// reinterpret returns a value which has the same bit-pattern as its operand value, in its result type.
// WebAssembly instruction: `f32.reinterpret_i32`, `i32.reinterpret_f32`, `f64.reinterpret_i64`, `i64.reinterpret_f64`.
pub fn (mut func Function) reinterpret(a NumType) {
	match a {
		.f32_t { func.code << 0xBC } // i32.reinterpret_f32
		.i32_t { func.code << 0xBE } // f32.reinterpret_i32
		.f64_t { func.code << 0xBD } // i64.reinterpret_f64
		.i64_t { func.code << 0xBF } // f64.reinterpret_i64
	}
}

// unreachable denotes a point in code that should not be reachable, it is an unconditional trap.
// WebAssembly instruction: `unreachable`.
pub fn (mut func Function) unreachable() {
	func.code << 0x00
}

// nop instruction, does nothing.
// WebAssembly instruction: `nop`.
pub fn (mut func Function) nop() {
	func.code << 0x01
}

pub type LabelIndex = int

// c_block creates a label that can later be branched out of with `c_br` and `c_br_if`.
// Blocks are strongly typed, you must supply a list of types for `parameters` and `results`.
// All blocks must be ended, see the `c_end` function.
pub fn (mut func Function) c_block(parameters []ValType, results []ValType) LabelIndex {
	func.label++
	func.code << 0x02
	func.blocktype(parameters: parameters, results: results)
	return func.label
}

// c_loop creates a label that can later be branched to with `c_br` and `c_br_if`.
// Loops are strongly typed, you must supply a list of types for `parameters` and `results`.
// All loops must be ended, see the `c_end` function.
pub fn (mut func Function) c_loop(parameters []ValType, results []ValType) LabelIndex {
	func.label++
	func.code << 0x03
	func.blocktype(parameters: parameters, results: results)
	return func.label
}

// c_if opens an if expression. It executes a statement if the last item on the stack is true.
// It creates a label that can later be branched out of with `c_br` and `c_br_if`.
// If expressions are strongly typed, you must supply a list of types for `parameters` and `results`.
// Call `c_else` to open the else case of an if expression, or close it by calling `c_end_if`.
// All if expressions must be ended, see the `c_end` function.
pub fn (mut func Function) c_if(parameters []ValType, results []ValType) LabelIndex {
	func.label++
	func.code << 0x04
	func.blocktype(parameters: parameters, results: results)
	return func.label
}

// c_else opens the else case of an if expression, it must be closed by calling `c_end`.
pub fn (mut func Function) c_else(label LabelIndex) {
	assert func.label == label, 'c_else: called with an invalid label ${label}'
	func.code << 0x05
}

// c_return returns from a function.
// WebAssembly instruction: `return`.
pub fn (mut func Function) c_return() {
	func.code << 0x0F // return
}

// c_end ends the block, loop or if expression with the label passed in at `label`.
pub fn (mut func Function) c_end(label LabelIndex) {
	assert func.label == label, 'c_end: called with an invalid label ${label}'
	func.label--
	assert func.label >= 0, 'c_end: negative label index, unbalanced calls'
	func.code << 0x0B // END expression opcode
}

// c_br branches to a loop or block with the label passed in at `label`.
// WebAssembly instruction: `br`.
pub fn (mut func Function) c_br(label LabelIndex) {
	v := func.label - label
	assert v >= 0, 'c_br: malformed label index'
	func.code << 0x0C // br
	func.u32(u32(v))
}

// c_br_if branches to a loop or block with the label passed in at `label`, based on an i32 condition.
// WebAssembly instruction: `br_if`.
pub fn (mut func Function) c_br_if(label LabelIndex) {
	v := func.label - label
	assert v >= 0, 'c_br_if: malformed label index'
	func.code << 0x0D // br_if
	func.u32(u32(v))
}

// call calls a locally defined function.
// If this function does not exist when calling `compile` on the module, it will panic.
// WebAssembly instruction: `call`.
pub fn (mut func Function) call(name string) {
	func.code << 0x10 // call
	func.patches << CallPatch(FunctionCallPatch{
		name: name
		pos:  func.code.len
	})
}

// call calls an imported function.
// If the imported function does not exist when calling `compile` on the module, it will panic.
// WebAssembly instruction: `call`.
pub fn (mut func Function) call_import(mod string, name string) {
	func.code << 0x10 // call
	func.patches << CallPatch(ImportCallPatch{
		mod:  mod
		name: name
		pos:  func.code.len
	})
}

// load loads a value with type `typ` from memory.
// WebAssembly instruction: `i32|i64|f32|f64.load`.
pub fn (mut func Function) load(typ NumType, align int, offset int) {
	match typ {
		.i32_t { func.code << 0x28 } // i32.load
		.i64_t { func.code << 0x29 } // i64.load
		.f32_t { func.code << 0x2A } // f32.load
		.f64_t { func.code << 0x2B } // f64.load
	}
	func.u32(u32(align))
	func.u32(u32(offset))
}

// load8 loads a 8-bit value with type `typ` with respect to `is_signed` from memory.
// WebAssembly instructions: `i32|i64.load8_s`, `i32|i64.load8_u`.
pub fn (mut func Function) load8(typ NumType, is_signed bool, align int, offset int) {
	assert typ in [.i32_t, .i64_t]

	match typ {
		.i32_t {
			if is_signed {
				func.code << 0x2C // i32.load8_s
			} else {
				func.code << 0x2D // i32.load8_u
			}
		}
		.i64_t {
			if is_signed {
				func.code << 0x30 // i64.load8_s
			} else {
				func.code << 0x31 // i64.load8_u
			}
		}
		else {}
	}
	func.u32(u32(align))
	func.u32(u32(offset))
}

// load16 loads a 16-bit value with type `typ` with respect to `is_signed` from memory.
// WebAssembly instructions: `i32|i64.load16_s`, `i32|i64.load16_u`.
pub fn (mut func Function) load16(typ NumType, is_signed bool, align int, offset int) {
	assert typ in [.i32_t, .i64_t]

	match typ {
		.i32_t {
			if is_signed {
				func.code << 0x2E // i32.load16_s
			} else {
				func.code << 0x2F // i32.load16_u
			}
		}
		.i64_t {
			if is_signed {
				func.code << 0x32 // i64.load16_s
			} else {
				func.code << 0x33 // i64.load16_u
			}
		}
		else {}
	}
	func.u32(u32(align))
	func.u32(u32(offset))
}

// load32_i64 loads a 32-bit value of type i64 with respect to `is_signed` from memory.
// WebAssembly instructions: `i64.load32_s`, `i64.load32_u`.
pub fn (mut func Function) load32_i64(is_signed bool, align int, offset int) {
	if is_signed {
		func.code << 0x34 // i64.load32_s
	} else {
		func.code << 0x35 // i64.load32_u
	}
	func.u32(u32(align))
	func.u32(u32(offset))
}

// store stores a value with type `typ` into memory.
// WebAssembly instruction: `i32|i64|f32|f64.store`.
pub fn (mut func Function) store(typ NumType, align int, offset int) {
	match typ {
		.i32_t { func.code << 0x36 } // i32.store
		.i64_t { func.code << 0x37 } // i64.store
		.f32_t { func.code << 0x38 } // f32.store
		.f64_t { func.code << 0x39 } // f64.store
	}
	func.u32(u32(align))
	func.u32(u32(offset))
}

// store8 stores a 8-bit value with type `typ` into memory.
// WebAssembly instruction: `i32|i64.store8`.
pub fn (mut func Function) store8(typ NumType, align int, offset int) {
	assert typ in [.i32_t, .i64_t]

	match typ {
		.i32_t { func.code << 0x3A } // i32.store8
		.i64_t { func.code << 0x3C } // i64.store8
		else {}
	}
	func.u32(u32(align))
	func.u32(u32(offset))
}

// store16 stores a 16-bit value with type `typ` into memory.
// WebAssembly instruction: `i32|i64.store16`.
pub fn (mut func Function) store16(typ NumType, align int, offset int) {
	assert typ in [.i32_t, .i64_t]

	match typ {
		.i32_t { func.code << 0x3B } // i32.store16
		.i64_t { func.code << 0x3D } // i64.store16
		else {}
	}
	func.u32(u32(align))
	func.u32(u32(offset))
}

// store16 stores a 32-bit value of type i64 into memory.
// WebAssembly instruction: `i64.store32`.
pub fn (mut func Function) store32_i64(align int, offset int) {
	func.code << 0x3E // i64.store32
	func.u32(u32(align))
	func.u32(u32(offset))
}

// memory_size gets the size of the memory instance.
// WebAssembly instruction: `memory.size`.
pub fn (mut func Function) memory_size() {
	func.code << 0x3F
	func.code << 0x00
}

// memory_grow increases the size of the memory instance.
// WebAssembly instruction: `memory.grow`.
pub fn (mut func Function) memory_grow() {
	func.code << 0x40
	func.code << 0x00
}

// memory_init copies from a passive memory segment to the memory instance.
// WebAssembly instruction: `memory.init`.
pub fn (mut func Function) memory_init(idx DataSegmentIndex) {
	func.code << 0xFC
	func.code << 0x08
	func.u32(u32(idx))
	func.code << 0x00
}

// data_drop prevents further use of a passive memory segment.
// WebAssembly instruction: `data.drop`.
pub fn (mut func Function) data_drop(idx DataSegmentIndex) {
	func.code << 0xFC
	func.code << 0x09
	func.u32(u32(idx))
}

// memory_copy copies one region of memory to another.
// Similar to `memcpy` and `memmove`, memory regions can overlap.
// WebAssembly instruction: `memory.copy`.
pub fn (mut func Function) memory_copy() {
	func.code << [u8(0xFC), 0x0A, 0x00, 0x00]
}

// memory_fill sets a memory region to a byte value.
// Similar to `memset`.
// WebAssembly instruction: `memory.copy`.
pub fn (mut func Function) memory_fill() {
	func.code << [u8(0xFC), 0x0B, 0x00]
}

// ref_null places a null reference on the stack.
// WebAssembly instruction: `ref.null`.
pub fn (mut func Function) ref_null(rt RefType) {
	func.code << 0xD0 // ref.null
	func.code << u8(rt)
}

// ref_is_null checks if the reference value on the stack is null, places an i32 boolean value on the stack.
// WebAssembly instruction: `ref_is_null`.
pub fn (mut func Function) ref_is_null(rt RefType) {
	func.code << 0xD1 // ref_is_null
}

// ref_func places a reference to a function with `name` on the stack.
// If this function does not exist when calling `compile` on the module, it will panic.
// WebAssembly instruction: `ref.func`.
pub fn (mut func Function) ref_func(name string) {
	func.code << 0xD2 // ref.func
	func.patches << CallPatch(FunctionCallPatch{
		name: name
		pos:  func.code.len
	})
}

// ref_func_import places a reference to an imported function with `name` on the stack.
// If the imported function does not exist when calling `compile` on the module, it will panic.
// WebAssembly instruction: `ref.func`.
pub fn (mut func Function) ref_func_import(mod string, name string) {
	func.code << 0xD2 // ref.func
	func.patches << CallPatch(ImportCallPatch{
		mod:  mod
		name: name
		pos:  func.code.len
	})
}
