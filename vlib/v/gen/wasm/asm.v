// Copyright (c) 2023 l-m.dev. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module wasm

import v.ast

pub fn (mut g Gen) asm_literal_arg(node ast.AsmTemplate) {
	// i32.const
	// i64.const
	// f32.const
	// f64.const

	if node.args.len != 1 {
		g.v_error('too many arguments to `${node.name}`', node.pos)
	}

	is_float := node.name[0] == `f`
	arg := node.args[0]

	if is_float {
		literal := match arg {
			ast.FloatLiteral {
				arg.val
			}
			else {
				g.v_error('must supply float value to `${node.name}`', node.pos)
			}
		}

		match node.name {
			'f32.const' {
				g.func.f32_const(literal.f32())
			}
			'f64.const' {
				g.func.f64_const(literal.f64())
			}
			else {
				panic('unreachable')
			}
		}
		return
	}

	literal := match arg {
		ast.BoolLiteral {
			if arg.val {
				'1'
			} else {
				'0'
			}
		}
		ast.CharLiteral {
			u32(arg.val.runes()[0]).str() // there is a better way.
		}
		ast.IntegerLiteral {
			arg.val
		}
		else {
			g.v_error('must supply integer-like value to `${node.name}`', node.pos)
		}
	}

	match node.name {
		'i32.const' {
			g.func.i32_const(i32(literal.int()))
		}
		'i64.const' {
			g.func.i64_const(literal.i64())
		}
		else {
			panic('unreachable')
		}
	}
}

pub fn (mut g Gen) asm_template(parent ast.AsmStmt, node ast.AsmTemplate) {
	if node.is_label || node.is_directive {
		g.v_error("`asm wasm` doesn't support labels or directives", node.pos)
	}

	match node.name {
		'unreachable' {
			g.func.unreachable()
		}
		'nop' {
			g.func.nop()
		}
		'drop' {
			g.func.drop()
		}
		'return' {
			g.func.c_return()
		}
		'i32.const' {
			g.asm_literal_arg(node)
		}
		'i64.const' {
			g.asm_literal_arg(node)
		}
		'f32.const' {
			g.asm_literal_arg(node)
		}
		'f64.const' {
			g.asm_literal_arg(node)
		}
		'i32.add' {
			g.func.add(.i32_t)
		}
		'i32.sub' {
			g.func.sub(.i32_t)
		}
		'i32.mul' {
			g.func.mul(.i32_t)
		}
		'i32.div_s' {
			g.func.div(.i32_t, true)
		}
		'i32.div_u' {
			g.func.div(.i32_t, false)
		}
		'i32.rem_s' {
			g.func.rem(.i32_t, true)
		}
		'i32.rem_u' {
			g.func.rem(.i32_t, false)
		}
		'i64.add' {
			g.func.add(.i64_t)
		}
		'i64.sub' {
			g.func.sub(.i64_t)
		}
		'i64.mul' {
			g.func.mul(.i64_t)
		}
		'i64.div_s' {
			g.func.div(.i64_t, true)
		}
		'i64.div_u' {
			g.func.div(.i64_t, false)
		}
		'i64.rem_s' {
			g.func.rem(.i64_t, true)
		}
		'i64.rem_u' {
			g.func.rem(.i64_t, false)
		}
		'f32.add' {
			g.func.add(.f32_t)
		}
		'f32.sub' {
			g.func.sub(.f32_t)
		}
		'f32.mul' {
			g.func.mul(.f32_t)
		}
		'f32.div' {
			g.func.div(.f32_t, true)
		}
		'f64.add' {
			g.func.add(.f64_t)
		}
		'f64.sub' {
			g.func.sub(.f64_t)
		}
		'f64.mul' {
			g.func.mul(.f64_t)
		}
		'f64.div' {
			g.func.div(.f64_t, true)
		}
		'i32.eqz' {
			g.func.eqz(.i32_t)
		}
		'i32.eq' {
			g.func.eq(.i32_t)
		}
		'i32.ne' {
			g.func.ne(.i32_t)
		}
		'i32.lt_s' {
			g.func.lt(.i32_t, true)
		}
		'i32.lt_u' {
			g.func.lt(.i32_t, false)
		}
		'i32.gt_s' {
			g.func.gt(.i32_t, true)
		}
		'i32.gt_u' {
			g.func.gt(.i32_t, false)
		}
		'i32.le_s' {
			g.func.le(.i32_t, true)
		}
		'i32.le_u' {
			g.func.le(.i32_t, false)
		}
		'i32.ge_s' {
			g.func.ge(.i32_t, true)
		}
		'i32.ge_u' {
			g.func.ge(.i32_t, false)
		}
		'i64.eqz' {
			g.func.eqz(.i64_t)
		}
		'i64.eq' {
			g.func.eq(.i64_t)
		}
		'i64.ne' {
			g.func.ne(.i64_t)
		}
		'i64.lt_s' {
			g.func.lt(.i64_t, true)
		}
		'i64.lt_u' {
			g.func.lt(.i64_t, false)
		}
		'i64.gt_s' {
			g.func.gt(.i64_t, true)
		}
		'i64.gt_u' {
			g.func.gt(.i64_t, false)
		}
		'i64.le_s' {
			g.func.le(.i64_t, true)
		}
		'i64.le_u' {
			g.func.le(.i64_t, false)
		}
		'i64.ge_s' {
			g.func.ge(.i64_t, true)
		}
		'i64.ge_u' {
			g.func.ge(.i64_t, false)
		}
		'f32.eq' {
			g.func.eq(.f32_t)
		}
		'f32.ne' {
			g.func.ne(.f32_t)
		}
		'f32.lt' {
			g.func.lt(.f32_t, true)
		}
		'f32.gt' {
			g.func.gt(.f32_t, true)
		}
		'f32.le' {
			g.func.le(.f32_t, true)
		}
		'f32.ge' {
			g.func.ge(.f32_t, true)
		}
		'f64.eq' {
			g.func.eq(.f64_t)
		}
		'f64.ne' {
			g.func.ne(.f64_t)
		}
		'f64.lt' {
			g.func.lt(.f64_t, true)
		}
		'f64.gt' {
			g.func.gt(.f64_t, true)
		}
		'f64.le' {
			g.func.le(.f64_t, true)
		}
		'f64.ge' {
			g.func.ge(.f64_t, true)
		}
		'i32.and' {
			g.func.b_and(.i32_t)
		}
		'i32.or' {
			g.func.b_or(.i32_t)
		}
		'i32.xor' {
			g.func.b_xor(.i32_t)
		}
		'i32.shl' {
			g.func.b_shl(.i32_t)
		}
		'i32.shr_s' {
			g.func.b_shr(.i32_t, true)
		}
		'i32.shr_u' {
			g.func.b_shr(.i32_t, true)
		}
		'i32.rotl' {
			g.func.rotl(.i32_t)
		}
		'i32.rotr' {
			g.func.rotr(.i32_t)
		}
		'i32.clz' {
			g.func.clz(.i32_t)
		}
		'i32.ctz' {
			g.func.ctz(.i32_t)
		}
		'i32.popcnt' {
			g.func.popcnt(.i32_t)
		}
		'i64.and' {
			g.func.b_and(.i64_t)
		}
		'i64.or' {
			g.func.b_or(.i64_t)
		}
		'i64.xor' {
			g.func.b_xor(.i64_t)
		}
		'i64.shl' {
			g.func.b_shl(.i64_t)
		}
		'i64.shr_s' {
			g.func.b_shr(.i64_t, true)
		}
		'i64.shr_u' {
			g.func.b_shr(.i64_t, false)
		}
		'i64.rotl' {
			g.func.rotl(.i64_t)
		}
		'i64.rotr' {
			g.func.rotr(.i64_t)
		}
		'i64.clz' {
			g.func.clz(.i64_t)
		}
		'i64.ctz' {
			g.func.ctz(.i64_t)
		}
		'i64.popcnt' {
			g.func.popcnt(.i64_t)
		}
		'f32.neg' {
			g.func.neg(.f32_t)
		}
		'f32.ceil' {
			g.func.ceil(.f32_t)
		}
		'f32.floor' {
			g.func.floor(.f32_t)
		}
		'f32.trunc' {
			g.func.trunc(.f32_t)
		}
		'f32.nearest' {
			g.func.nearest(.f32_t)
		}
		'f32.sqrt' {
			g.func.sqrt(.f32_t)
		}
		'f32.min' {
			g.func.min(.f32_t)
		}
		'f32.max' {
			g.func.max(.f32_t)
		}
		'f32.copysign' {
			g.func.copysign(.f32_t)
		}
		'f64.abs' {
			g.func.abs(.f64_t)
		}
		'f64.neg' {
			g.func.neg(.f64_t)
		}
		'f64.ceil' {
			g.func.ceil(.f64_t)
		}
		'f64.floor' {
			g.func.floor(.f64_t)
		}
		'f64.trunc' {
			g.func.trunc(.f64_t)
		}
		'f64.nearest' {
			g.func.nearest(.f64_t)
		}
		'f64.sqrt' {
			g.func.sqrt(.f64_t)
		}
		'f64.min' {
			g.func.min(.f64_t)
		}
		'f64.max' {
			g.func.max(.f64_t)
		}
		'f64.copysign' {
			g.func.copysign(.f64_t)
		}
		'i32.wrap_i64' {
			g.func.cast(.i64_t, false, .i32_t)
		}
		'i32.trunc_f32_s' {
			g.func.cast_trapping(.f32_t, true, .i32_t)
		}
		'i32.trunc_f32_u' {
			g.func.cast_trapping(.f32_t, false, .i32_t)
		}
		'i32.trunc_f64_s' {
			g.func.cast_trapping(.f64_t, true, .i32_t)
		}
		'i32.trunc_f64_u' {
			g.func.cast_trapping(.f64_t, false, .i32_t)
		}
		'i64.extend_i32_s' {
			g.func.cast(.i32_t, true, .i64_t)
		}
		'i64.extend_i32_u' {
			g.func.cast(.i32_t, false, .i64_t)
		}
		'i64.trunc_f32_s' {
			g.func.cast_trapping(.f32_t, true, .i64_t)
		}
		'i64.trunc_f32_u' {
			g.func.cast_trapping(.f32_t, false, .i64_t)
		}
		'i64.trunc_f64_s' {
			g.func.cast_trapping(.f64_t, true, .i64_t)
		}
		'i64.trunc_f64_u' {
			g.func.cast_trapping(.f64_t, false, .i64_t)
		}
		'f32.convert_i32_s' {
			g.func.cast(.i32_t, true, .f32_t)
		}
		'f32.convert_i32_u' {
			g.func.cast(.i32_t, false, .f32_t)
		}
		'f32.convert_i64_s' {
			g.func.cast(.i64_t, true, .f32_t)
		}
		'f32.convert_i64_u' {
			g.func.cast(.i64_t, false, .f32_t)
		}
		'f32.demote_f64' {
			g.func.cast(.f64_t, true, .f32_t)
		}
		'f64.convert_i32_s' {
			g.func.cast(.i32_t, true, .f64_t)
		}
		'f64.convert_i32_u' {
			g.func.cast(.i32_t, false, .f64_t)
		}
		'f64.convert_i64_s' {
			g.func.cast(.i64_t, true, .f64_t)
		}
		'f64.convert_i64_u' {
			g.func.cast(.i64_t, false, .f64_t)
		}
		'f64.promote_f32' {
			g.func.cast(.f32_t, true, .f64_t)
		}
		'i32.reinterpret_f32' {
			g.func.reinterpret(.f32_t)
		}
		'i64.reinterpret_f64' {
			g.func.reinterpret(.f64_t)
		}
		'f32.reinterpret_i32' {
			g.func.reinterpret(.i32_t)
		}
		'f64.reinterpret_i64' {
			g.func.reinterpret(.i64_t)
		}
		'i32.extend8_s' {
			g.func.sign_extend8(.i32_t)
		}
		'i32.extend16_s' {
			g.func.sign_extend16(.i32_t)
		}
		'i64.extend8_s' {
			g.func.sign_extend8(.i64_t)
		}
		'i64.extend32_s' {
			g.func.sign_extend32_i64()
		}
		else {
			g.v_error('unknown opcode', node.pos)
		}
	}
}

pub fn (mut g Gen) asm_stmt(node ast.AsmStmt) {
	for tmpl in node.templates {
		g.asm_template(node, tmpl)
	}
}