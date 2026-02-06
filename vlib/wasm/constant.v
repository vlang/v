// Copyright (c) 2023 l-m.dev. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module wasm

import encoding.leb128

// constexpr_value returns a constant expression that evaluates to a single value.
pub fn constexpr_value[T](v T) ConstExpression {
	mut expr := ConstExpression{}

	$if T is i64 {
		expr.i64_const(v)
	} $else $if T is $int {
		expr.i32_const(i32(v))
	} $else $if T is f32 {
		expr.f32_const(v)
	} $else $if T is f64 {
		expr.f64_const(v)
	} $else {
		$compile_error('values can only be int, i32, i64, f32, f64')
	}

	return expr
}

// constexpr_value_zero returns a constant expression that evaluates to zero.
pub fn constexpr_value_zero(v ValType) ConstExpression {
	mut expr := ConstExpression{}

	match v {
		.i32_t {
			expr.i32_const(0)
		}
		.i64_t {
			expr.i64_const(0)
		}
		.f32_t {
			expr.f32_const(0.0)
		}
		.f64_t {
			expr.f64_const(0.0)
		}
		.funcref_t, .externref_t {
			expr.ref_null(RefType(v))
		}
		.v128_t {
			panic('type `v128_t` not permitted in a constant expression')
		}
	}

	return expr
}

// constexpr_ref_null returns a constant expression that evaluates to a null reference.
pub fn constexpr_ref_null(rt RefType) ConstExpression {
	mut expr := ConstExpression{}

	expr.ref_null(rt)

	return expr
}

// WebAssembly constant expressions are permitted to use a subset of valid instructions.
pub struct ConstExpression {
mut:
	call_patches []CallPatch
	code         []u8
}

// i32_const places a constant i32 value on the stack.
// WebAssembly instruction: `i32.const`.
pub fn (mut expr ConstExpression) i32_const(v i32) {
	expr.code << 0x41 // i32.const
	expr.code << leb128.encode_i32(v)
}

// i64_const places a constant i64 value on the stack.
// WebAssembly instruction: `i64.const`.
pub fn (mut expr ConstExpression) i64_const(v i64) {
	expr.code << 0x42 // i64.const
	expr.code << leb128.encode_i64(v)
}

// f32_const places a constant f32 value on the stack.
// WebAssembly instruction: `f32.const`.
pub fn (mut expr ConstExpression) f32_const(v f32) {
	expr.code << 0x43 // f32.const
	push_f32(mut expr.code, v)
}

// f64_const places a constant f64 value on the stack.
// WebAssembly instruction: `f64.const`.
pub fn (mut expr ConstExpression) f64_const(v f64) {
	expr.code << 0x44 // f64.const
	push_f64(mut expr.code, v)
}

// add adds two values on the stack with type `typ`.
// WebAssembly instructions: `i32|i64.add`.
pub fn (mut expr ConstExpression) add(typ NumType) {
	assert typ in [.i32_t, .i64_t]

	match typ {
		.i32_t { expr.code << 0x6A } // i32.add
		.i64_t { expr.code << 0x7C } // i64.add
		else {}
	}
}

// sub subtracts two values on the stack with type `typ`.
// WebAssembly instructions: `i32|i64.sub`.
pub fn (mut expr ConstExpression) sub(typ NumType) {
	assert typ in [.i32_t, .i64_t]

	match typ {
		.i32_t { expr.code << 0x6B } // i32.sub
		.i64_t { expr.code << 0x7D } // i64.sub
		else {}
	}
}

// mul multiplies two values on the stack with type `typ`.
// WebAssembly instructions: `i32|i64.mul`.
pub fn (mut expr ConstExpression) mul(typ NumType) {
	assert typ in [.i32_t, .i64_t]

	match typ {
		.i32_t { expr.code << 0x6C } // i32.mul
		.i64_t { expr.code << 0x7E } // i64.mul
		else {}
	}
}

// global_get places the value of the global at the index `global` on the stack.
// Constant expressions are only allowed to refer to imported globals.
// WebAssembly instruction: `global.get`.
pub fn (mut expr ConstExpression) global_get(global GlobalImportIndex) {
	expr.code << 0x23 // global.get
	expr.code << leb128.encode_u32(u32(global))
}

// ref_null places a null reference on the stack.
// WebAssembly instruction: `ref.null`.
pub fn (mut expr ConstExpression) ref_null(rt RefType) {
	expr.code << 0xD0 // ref.null
	expr.code << u8(rt)
}

// ref_func places a reference to a function with `name` on the stack.
// If this function does not exist when calling `compile` on the module, it will panic.
// WebAssembly instruction: `ref.func`.
pub fn (mut expr ConstExpression) ref_func(name string) {
	expr.code << 0xD2 // ref.func
	expr.call_patches << FunctionCallPatch{
		name: name
		pos:  expr.code.len
	}
}

// ref_func places a reference to an imported function with `name` on the stack.
// If the imported function does not exist when calling `compile` on the module, it will panic.
// WebAssembly instruction: `ref.func`.
pub fn (mut expr ConstExpression) ref_func_import(mod string, name string) {
	expr.code << 0xD2 // ref.func
	expr.call_patches << ImportCallPatch{
		mod:  mod
		name: name
		pos:  expr.code.len
	}
}
