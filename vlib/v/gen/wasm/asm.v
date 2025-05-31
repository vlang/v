// Copyright (c) 2023 l-m.dev. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module wasm

import v.ast

pub fn (mut g Gen) asm_call(node ast.AsmTemplate) {
	// call 'main.test'
	// call 'main.Struct.+'
	//
	// call 'wasi_unstable' 'proc_exit'
	// call 'console' 'log'

	if node.args.len !in [1, 2] {
		g.v_error('incorrect number of arguments to `${node.name}`', node.pos)
	}

	arg0 := node.args[0]
	sarg0 := if arg0 is string {
		arg0
	} else if node.args.len == 1 {
		g.v_error('`${node.name}` must accept a string to call', node.pos)
	} else {
		g.v_error('`${node.name}` must accept a namespace for call', node.pos)
	}

	if node.args.len == 1 {
		g.func.call(sarg0)
		return
	}

	arg1 := node.args[1]
	sarg1 := if arg1 is string {
		arg1
	} else {
		g.v_error('`${node.name}` must accept a string for call', node.pos)
	}

	g.func.call_import(sarg0, sarg1)
}

pub fn (mut g Gen) asm_local_get_set_or_tee(node ast.AsmTemplate, vars AsmVars) {
	if node.args.len != 1 {
		g.v_error('incorrect number of arguments to `${node.name}`', node.pos)
	}

	arg0 := node.args[0]
	alias := match arg0 {
		ast.AsmAlias {
			arg0
		}
		else {
			g.v_error('must reference local by identifier', node.pos)
		}
	}

	target_var := if alias.name == '__vbp' {
		Var{
			idx: g.bp()
		}
	} else {
		var := vars[alias.name] or { g.v_error('unknown identifier', alias.pos) }
		var
	}
	// -- doesn't work, cgen error
	// else if var := vars[alias.name] {
	//     var
	// }

	if target_var.is_global {
		g.v_error('`${alias.name}` is global, cannot use with this instruction', alias.pos)
	}

	match node.name {
		'local.get' {
			g.get(target_var)
		}
		'local.set' {
			g.set(target_var)
		}
		'local.tee' {
			g.tee(target_var)
		}
		else {
			panic('unreachable')
		}
	}
}

pub fn (mut g Gen) asm_global_get_or_set(node ast.AsmTemplate, vars AsmVars) {
	if node.args.len != 1 {
		g.v_error('incorrect number of arguments to `${node.name}`', node.pos)
	}

	arg0 := node.args[0]
	alias := match arg0 {
		ast.AsmAlias {
			arg0
		}
		else {
			g.v_error('must reference global by identifier', node.pos)
		}
	}

	target_var := if alias.name == '__vsp' {
		Var{
			g_idx:     g.sp()
			is_global: true
		}
	} else if alias.name == '__heap_base' {
		Var{
			g_idx:     g.hp()
			is_global: true
		}
	} else {
		var := vars[alias.name] or { g.v_error('unknown identifier', alias.pos) }
		var
	}

	if !target_var.is_global {
		g.v_error('`${alias.name}` is a local, cannot use with this instruction', alias.pos)
	}

	match node.name {
		'global.get' {
			g.get(target_var)
		}
		'global.set' {
			g.set(target_var)
		}
		else {
			panic('unreachable')
		}
	}
}

pub fn (mut g Gen) asm_literal_arg(node ast.AsmTemplate) {
	// i32.const
	// i64.const
	// f32.const
	// f64.const

	if node.args.len != 1 {
		g.v_error('incorrect number of arguments to `${node.name}`', node.pos)
	}

	is_float := node.name[0] == `f`
	arg := node.args[0]

	if is_float {
		literal := match arg {
			ast.FloatLiteral {
				arg.val
			}
			ast.IntegerLiteral {
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

pub fn (mut g Gen) asm_parse_align_offset(node ast.AsmTemplate) (int, int) {
	if node.args.len != 2 {
		g.v_error('incorrect number of arguments to `${node.name}`', node.pos)
	}

	arg0 := node.args[0]
	arg1 := node.args[1]

	align := match arg0 {
		ast.IntegerLiteral {
			arg0.val.int()
		}
		else {
			g.v_error('must supply integer value to align', node.pos)
		}
	}

	offset := match arg1 {
		ast.IntegerLiteral {
			arg1.val.int()
		}
		else {
			g.v_error('must supply integer value to offset', node.pos)
		}
	}

	return align, offset
}

pub fn (mut g Gen) asm_load_or_store(node ast.AsmTemplate) {
	align, offset := g.asm_parse_align_offset(node)

	match node.name {
		'i32.load' {
			g.func.load(.i32_t, align, offset)
		}
		'i64.load' {
			g.func.load(.i64_t, align, offset)
		}
		'f32.load' {
			g.func.load(.f32_t, align, offset)
		}
		'f64.load' {
			g.func.load(.f64_t, align, offset)
		}
		'i32.store' {
			g.func.store(.i32_t, align, offset)
		}
		'i64.store' {
			g.func.store(.i64_t, align, offset)
		}
		'f32.store' {
			g.func.store(.f32_t, align, offset)
		}
		'f64.store' {
			g.func.store(.f64_t, align, offset)
		}
		'i32.load8_s' {
			g.func.load8(.i32_t, true, align, offset)
		}
		'i64.load8_s' {
			g.func.load8(.i64_t, true, align, offset)
		}
		'i32.load8_u' {
			g.func.load8(.i32_t, false, align, offset)
		}
		'i64.load8_u' {
			g.func.load8(.i64_t, false, align, offset)
		}
		'i32.load16_s' {
			g.func.load16(.i32_t, true, align, offset)
		}
		'i64.load16_s' {
			g.func.load16(.i64_t, true, align, offset)
		}
		'i32.load16_u' {
			g.func.load16(.i32_t, false, align, offset)
		}
		'i64.load16_u' {
			g.func.load16(.i64_t, false, align, offset)
		}
		'i64.load32_s' {
			g.func.load32_i64(true, align, offset)
		}
		'i64.load32_u' {
			g.func.load32_i64(false, align, offset)
		}
		'i32.store8' {
			g.func.store8(.i32_t, align, offset)
		}
		'i64.store8' {
			g.func.store8(.i64_t, align, offset)
		}
		'i32.store16' {
			g.func.store16(.i32_t, align, offset)
		}
		'i64.store16' {
			g.func.store16(.i64_t, align, offset)
		}
		'i64.store32' {
			g.func.store32_i64(align, offset)
		}
		else {
			panic('unreachable')
		}
	}
}

pub fn (mut g Gen) asm_template(parent ast.AsmStmt, node ast.AsmTemplate, vars AsmVars) {
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
		'select' {
			g.func.c_select()
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
		'local.get' {
			g.asm_local_get_set_or_tee(node, vars)
		}
		'local.set' {
			g.asm_local_get_set_or_tee(node, vars)
		}
		'local.tee' {
			g.asm_local_get_set_or_tee(node, vars)
		}
		'global.get' {
			g.asm_global_get_or_set(node, vars)
		}
		'global.set' {
			g.asm_global_get_or_set(node, vars)
		}
		'i32.load' {
			g.asm_load_or_store(node)
		}
		'i64.load' {
			g.asm_load_or_store(node)
		}
		'f32.load' {
			g.asm_load_or_store(node)
		}
		'f64.load' {
			g.asm_load_or_store(node)
		}
		'i32.store' {
			g.asm_load_or_store(node)
		}
		'i64.store' {
			g.asm_load_or_store(node)
		}
		'f32.store' {
			g.asm_load_or_store(node)
		}
		'f64.store' {
			g.asm_load_or_store(node)
		}
		'i32.load8_s' {
			g.asm_load_or_store(node)
		}
		'i64.load8_s' {
			g.asm_load_or_store(node)
		}
		'i32.load8_u' {
			g.asm_load_or_store(node)
		}
		'i64.load8_u' {
			g.asm_load_or_store(node)
		}
		'i32.load16_s' {
			g.asm_load_or_store(node)
		}
		'i64.load16_s' {
			g.asm_load_or_store(node)
		}
		'i32.load16_u' {
			g.asm_load_or_store(node)
		}
		'i64.load16_u' {
			g.asm_load_or_store(node)
		}
		'i64.load32_s' {
			g.asm_load_or_store(node)
		}
		'i64.load32_u' {
			g.asm_load_or_store(node)
		}
		'i32.store8' {
			g.asm_load_or_store(node)
		}
		'i64.store8' {
			g.asm_load_or_store(node)
		}
		'i32.store16' {
			g.asm_load_or_store(node)
		}
		'i64.store16' {
			g.asm_load_or_store(node)
		}
		'i64.store32' {
			g.asm_load_or_store(node)
		}
		'call' {
			g.asm_call(node)
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
		'i64.extend16_s' {
			g.func.sign_extend16(.i64_t)
		}
		'i64.extend32_s' {
			g.func.sign_extend32()
		}
		'i32.trunc_sat_f32_s' {
			g.func.cast(.f32_t, true, .i32_t)
		}
		'i32.trunc_sat_f32_u' {
			g.func.cast(.f32_t, false, .i32_t)
		}
		'i32.trunc_sat_f64_s' {
			g.func.cast(.f64_t, true, .i32_t)
		}
		'i32.trunc_sat_f64_u' {
			g.func.cast(.f64_t, false, .i32_t)
		}
		'i64.trunc_sat_f32_s' {
			g.func.cast(.f32_t, true, .i64_t)
		}
		'i64.trunc_sat_f32_u' {
			g.func.cast(.f32_t, false, .i64_t)
		}
		'i64.trunc_sat_f64_s' {
			g.func.cast(.f64_t, true, .i64_t)
		}
		'i64.trunc_sat_f64_u' {
			g.func.cast(.f64_t, false, .i64_t)
		}
		'memory.size' {
			g.func.memory_size()
		}
		'memory.grow' {
			g.func.memory_grow()
		}
		'memory.copy' {
			g.func.memory_copy()
		}
		'memory.fill' {
			g.func.memory_fill()
		}
		// TODO: impl later
		/*
		'ref.null' {
			g.func.ref_null()
		}
		*/
		else {
			g.v_error('unknown opcode', node.pos)
		}
	}
}

type AsmVars = map[string]Var

pub fn (mut g Gen) asm_stmt(node ast.AsmStmt) {
	mut vars := AsmVars(map[string]Var{})

	for var_expr in node.output {
		vars[var_expr.alias] = g.get_var_or_make_from_expr(var_expr.expr, var_expr.typ)
	}
	for var_expr in node.input {
		vars[var_expr.alias] = g.get_var_or_make_from_expr(var_expr.expr, var_expr.typ)
	}

	if node.clobbered.len != 0 {
		g.v_error('wasm does not support clobber lists', node.pos)
	}
	if node.global_labels.len != 0 || node.local_labels.len != 0 {
		g.v_error('wasm does not support labels', node.pos)
	}

	for tmpl in node.templates {
		g.asm_template(node, tmpl, vars)
	}
}
