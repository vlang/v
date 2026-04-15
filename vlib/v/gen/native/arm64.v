// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module native

import v.ast

const arm64_bl_opcode = u32(0x94000000)
const arm64_branch_imm_mask = u32(0x03ffffff)
const arm64_branch_imm_limit = i64(33_554_432)

enum Arm64Register {
	x0  // v----
	x1  // |
	x2  // |
	x3  // | parameter and result registers
	x4  // |
	x5  // |
	x6  // |
	x7  // ^----
	x8  // XR - indirect result location register
	x9  //  v----
	x10 // |
	x11 // |
	x12 // | caller saved registers
	x13 // |
	x14 // |
	x15 // ^----
	x16 // IP0 - inter procedure call scratch register
	x17 // IP1 - inter procedure call scratch register
	x18 // PR - platform register
	x19 // v----
	x20 // |
	x21 // |
	x22 // |
	x23 // | callee saved registers
	x24 // |
	x25 // |
	x26 // |
	x27 // |
	x28 // ^----
	x29 // FP - frame pointer
	x30 // LR - link register
}

fn (r Register) arm64() Arm64Register {
	return match r {
		.reg0 { .x0 }
		.reg1 { .x1 }
		.reg2 { .x2 }
		.reg3 { .x3 }
	}
}

pub struct Arm64 {
mut:
	g &Gen = unsafe { nil }
	// arm64 specific stuff for code generation
}

fn (mut x Arm64) cg_allocate_stack_var(name string, size i32, initial_val Number) i32 {
	eprintln('TODO: allocating var on arm64 (${name}) = ${size} = ${initial_val}')
	return 0
}

fn (mut c Arm64) cg_mov(reg Register, val i32) {
	c.mov(reg.arm64(), u64(val))
}

fn (mut c Arm64) mov(reg Arm64Register, val u64) {
	// m := u64(0xffff)
	// x := u64(val)
	// println('========')
	// println(x & ~m)
	// println(x & ~(m << 16))
	// g.write32(0x777777)
	r := i32(reg)
	if r >= 0 && r <= 16 {
		c.g.write32(i32(u32(0xd2800000 + u32(r) + (u32(val) << 5))))
		c.g.println('mov x${r}, ${val}')
	} else {
		c.g.n_error('mov_arm unsupported values')
	}
	/*
	if 1 ^ (x & ~m) != 0 {
		// println('yep')
		g.write32(i32(u64(0x52800000) | u64(r) | x << 5))
		g.write32(0x88888888)
		g.write32(i32(u64(0x52800000) | u64(r) | x >> 11))
	} else if 1 ^ (x & ~(m << 16)) != 0 {
		// g.write32(i32(u64(0x52800000) | u64(r) | x >> 11))
		// println('yep2')
		// g.write32(0x52a00000 | r | val >> 11)
	}
	*/
}

fn (mut c Arm64) neg(r Arm64Register) {
	c.neg_regs(r, r)
}

fn (mut c Arm64) neg_regs(a Arm64Register, b Arm64Register) {
	if u32(a) < 0x0f && u32(b) < 0x0f {
		c.g.write32(i32(0xe2600000 | (u32(a) << 16) | u32(b) << 12))
		c.g.println('neg ${a}, ${b}')
	} else {
		c.g.n_error('unhandled neg ${a}, ${b}')
	}
}

fn (mut c Arm64) sub_sp(v i32) {
	if c.g.pref.arch != .arm64 {
		c.g.n_error('sub_sp is arm64-specific')
		return
	}
	// this is for 0x20 only
	if v < 0 {
		c.g.write32(i32(0x910083ff)) // add sp, X
	} else {
		c.g.write32(i32(0xd10083ff)) // sub sp, X
	}
}

pub fn (mut c Arm64) cg_fn_decl(node ast.FnDecl) {
	if node.attrs.contains('flag_enum_fn') {
		// TODO: remove, when the native backend can process all flagged enum generated functions
		return
	}
	c.g.gen_arm64_helloworld()
	/*
	0x100003f6c      ff8300d1       sub sp, sp, 0x20           ; [00] -r-x section size 52 named 0.__TEXT.__text
            0x100003f70      fd7b01a9       stp x29, x30, [sp, 0x10]
            0x100003f74      fd430091       add x29, sp, 0x10
            0x100003f78      bfc31fb8       stur wzr, [x29, -4]
            0x100003f7c      68008052       mov w8, 3
            0x100003f80      e80b00b9       str w8, [sp, 8]
            0x100003f84      00000090       adrp x0, 0x100003000
            0x100003f88      00b03e91       add x0, x0, 0xfac
            0x100003f8c      05000094       bl sym.imp.puts            ;[1]
            0x100003f90      e00b40b9       ldr w0, [sp, 8]            ; 5
            0x100003f94      fd7b41a9       ldp x29, x30, [sp, 0x10]
            0x100003f98      ff830091       add sp, sp, 0x20
            0x100003f9c      c0035fd6       ret
	*/

	/*
	/*
	g.push(.rbp)
	g.mov_rbp_rsp()
*/
	locals_count := node.scope.objects.len + node.params.len + node.defer_stmts.len
	g.stackframe_size = (locals_count * 8) + 0x10
//	g.sub8(.rsp, g.stackframe_size)
	g.sub_sp(32)

	// Copy values from registers to local vars (calling convention)
	mut offset := 0
	for i in 0 .. node.params.len {
		name := node.params[i].name
		// TODO: optimize. Right now 2 mov's are used instead of 1.
		g.allocate_var(name, 4, 0)
		// `mov DWORD PTR [rbp-0x4],edi`
		offset += 4
		g.mov_reg_to_var(offset, native.fn_arg_registers[i])
	}
	// define defer vars
	for i in 0 .. node.defer_stmts.len {
		name := '_defer${i}'
		g.allocate_var(name, 8, 0)
	}
	//
	g.stmts(node.stmts)
	is_main := node.name == 'main.main'
	if is_main {
		// println('end of main: gen exit')
		zero := ast.IntegerLiteral{}
		g.gen_exit(zero)
		g.cg_ret()
		return
	}
	// g.leave()
	g.labels.addrs[0] = g.pos()
	g.println('; label 0: return')
/*
	if g.defer_stmts.len != 0 {
		// save return value
		g.push(.rax)
		for defer_stmt in g.defer_stmts.reverse() {
			defer_var := g.get_var_offset('_defer${defer_stmt.idx_in_fn}')
			g.code_gen.mov_var_to_reg(.rax, defer_var)
			g.cmp_zero(.rax)
			label := g.labels.new_label()
			jump_addr := g.cjmp(.je)
			g.labels.patches << LabelPatch{
				id: label
				pos: jump_addr
			}
			g.stmts(defer_stmt.stmts)
			g.labels.addrs[label] = g.pos()
		}
		//g.pop(.rax)
	}
*/
	g.sub_sp(-32)
	g.ret()
	*/
}

pub fn (mut c Arm64) cg_call_fn(node ast.CallExpr) {
	name := node.name
	// println('call fn ${name}')
	addr := c.g.fn_addr[name]
	if addr == 0 {
		c.g.n_error('fn addr of `${name}` = 0')
	}
	// Copy values to registers (calling convention)
	// c.mov(.eax, 0)
	for i in 0 .. node.args.len {
		expr := node.args[i].expr
		match expr {
			ast.IntegerLiteral {
				// `foo(2)` => `mov edi,0x2`
				// c.mov(native.fn_arg_registers[i], i32(expr.val.int()))
			}
			/*
			ast.Ident {
				// `foo(x)` => `mov edi,DWORD PTR [rbp-0x8]`
				var_offset := c.g.get_var_offset(expr.name)
				if c.g.pref.is_verbose {
					println('i=${i} fn name= ${name} offset=${var_offset}')
					println(i32(native.fn_arg_registers[i]))
				}
				c.g.code_gen.mov_var_to_reg(native.fn_arg_registers[i], var_offset)
			}
			*/
			else {
				c.g.n_error('unhandled call_fn (name=${name}) node: ' + expr.type_name())
			}
		}
	}
	if node.args.len > 6 {
		c.g.n_error('more than 6 args not allowed for now')
	}
	c.cg_call(i32(addr))
	c.g.println('fn call `${name}()`')
	// println('call ${name} ${addr}')
}

fn (mut g Gen) gen_arm64_helloworld() {
	mut c := g.cg
	if mut c is Arm64 {
		if g.pref.os == .linux {
			c.mov(Arm64Register.x0, 1)
			c.adr(Arm64Register.x1, 0x10)
			c.mov(Arm64Register.x2, 13)
			c.mov(Arm64Register.x8, 64) // write (linux-arm64)
			c.svc()
		} else {
			c.mov(Arm64Register.x0, 1)
			c.adr(Arm64Register.x1, 0x10 + 4)
			c.mov(Arm64Register.x2, 13)
			c.mov(Arm64Register.x16, 4) // write
			c.svc()
			c.mov(Arm64Register.x0, 0)
			c.mov(Arm64Register.x16, 1)
			c.svc()
		}
		zero := ast.IntegerLiteral{}
		g.cg.cg_gen_exit(zero)
		g.write_string('Hello World!\n')
		g.write8(0) // padding?
		g.write8(0)
		g.write8(0)
	}
}

fn (mut c Arm64) adr(r Arm64Register, delta i32) {
	c.g.write32(i32(0x10000000 | i32(r) | i32(u32(delta) << 4)))
	c.g.println('adr ${r}, ${delta}')
}

fn arm64_encode_bl_instruction(delta i64) !u32 {
	if (delta % 4) != 0 {
		return error('arm64 branch target must be 4-byte aligned, got delta ${delta}')
	}
	imm26 := delta / 4
	if imm26 < -arm64_branch_imm_limit || imm26 > arm64_branch_imm_limit - 1 {
		return error('arm64 branch target is out of range, got delta ${delta}')
	}
	return arm64_bl_opcode | (u32(i32(imm26)) & arm64_branch_imm_mask)
}

fn (mut c Arm64) bl() {
	// g.write32(0xa9400000)
	c.g.write32(i32(arm64_bl_opcode))
	c.g.println('bl 0')
}

fn (mut c Arm64) svc() {
	if c.g.pref.os == .linux {
		c.g.write32(i32(0xd4001001))
		c.g.println('svc 0x80')
	} else {
		c.g.write32(i32(0xd4000001))
		c.g.println('svc 0')
	}
}

fn (mut c Arm64) cg_syscall() {
	panic('the `syscall` instruction is not available with arm64')
}

pub fn (mut c Arm64) cg_gen_exit(expr ast.Expr) {
	mut return_code := u64(0)
	match expr {
		ast.IntegerLiteral {
			return_code = expr.val.u64()
		}
		else {
			c.g.n_error('native builtin exit expects a numeric argument')
		}
	}
	match c.g.pref.os {
		.macos {
			c.mov(.x0, return_code)
			c.mov(.x16, 1) // syscall exit
		}
		.linux {
			c.mov(.x16, return_code)
			c.mov(.x8, 93)
			c.mov(.x0, 0)
		}
		else {
			c.g.n_error('unsupported os ${c.g.pref.os}')
		}
	}
	c.svc()
}

pub fn (mut c Arm64) gen_arm64_exit(expr ast.Expr) {
	match expr {
		ast.IntegerLiteral {
			c.mov(.x16, expr.val.u64())
		}
		else {
			c.g.n_error('native builtin exit expects a numeric argument')
		}
	}
	c.mov(.x0, 0)
	c.svc()
}

fn (mut c Arm64) cg_address_size() i32 {
	return 8
}

fn (mut c Arm64) cg_gen_print(_s string, _fd i32) {
	panic('Arm64.cg_gen_print() is not implemented')
}

fn (mut c Arm64) cg_gen_print_reg(_r Register, _n i32, _fd i32) {
	panic('Arm64.cg_gen_print_reg() is not implemented')
}

fn (mut c Arm64) cg_learel(_reg Register, _val i32) {
	panic('Arm64.cg_learel() not implemented')
}

fn (mut c Arm64) cg_lea_var_to_reg(_reg Register, _var_offset i32) {
	panic('Arm64.cg_lea_var_to_reg() not implemented')
}

fn (mut c Arm64) cg_gen_match_expr(_expr ast.MatchExpr) {
	panic('Arm64.cg_gen_match_expr() not implemented')
}

fn (mut c Arm64) cg_convert_int_to_string(_a Register, _b Register) {
	panic('Arm64.cg_convert_int_to_string() not implemented')
}

fn (mut c Arm64) cg_convert_bool_to_string(_r Register) {
	panic('Arm64.cg_convert_bool_to_string() not implemented')
}

fn (mut c Arm64) cg_reverse_string(_r Register) {
	panic('Arm64.cg_reverse_string() not implemented')
}

fn (mut c Arm64) cg_mov_var_to_reg(_reg Register, _var Var, _config VarConfig) {
	panic('Arm64.cg_mov_var_to_reg() not implemented')
}

fn (mut c Arm64) cg_mov_reg(_r1 Register, _r2 Register) {
	panic('Arm64.cg_mov_reg() not implemented')
}

fn (mut c Arm64) cg_mov64(_r Register, _val Number) {
	panic('Arm64.cg_mov64() not implemented')
}

fn (mut c Arm64) cg_convert_rune_to_string(_r Register, _buffer i32, _var Var, _config VarConfig) {
	panic('Arm64.cg_convert_rune_to_string() not implemented')
}

fn (mut c Arm64) cg_trap() {
	c.g.write32(i32(0xcccccccc))
	c.g.println('trap')
}

fn (mut c Arm64) cg_leave() {
	panic('Arm64.cg_leave() not implemented')
}

fn (mut c Arm64) cg_ret() {
	c.g.write32(i32(0xd65f03c0))
	c.g.println('ret')
}

fn (mut c Arm64) cg_assign_stmt(_node ast.AssignStmt) {
	panic('Arm64.cg_assign_stmt() not implemented')
}

fn (mut c Arm64) cg_builtin_decl(_builtin BuiltinFn) {
	panic('Arm64.cg_builtin_decl() not implemented')
}

fn (mut c Arm64) cg_infix_expr(_node ast.InfixExpr) {
	panic('Arm64.cg_infix_expr() not implemented')
}

fn (mut c Arm64) cg_return_stmt(_node ast.Return) {
	panic('Arm64.cg_return_stmt() not implemented')
}

fn (mut c Arm64) cg_gen_cast_expr(_expr ast.CastExpr) {
	panic('Arm64.cg_gen_cast_expr() not implemented')
}

fn (mut c Arm64) cg_prefix_expr(_expr ast.PrefixExpr) {
	panic('Arm64.cg_prefix_expr() not implemented')
}

fn (mut c Arm64) cg_call_builtin(_name Builtin) i64 {
	panic('Arm64.cg_call_builtin() not implemented')
}

fn (mut c Arm64) cg_gen_asm_stmt(_asm_node ast.AsmStmt) {
	panic('Arm64.cg_gen_asm_stmt() not implemented')
}

fn (mut c Arm64) cg_infloop() {
	c.g.write32(u8(0x14))
	c.g.println('jmp $$')
}

fn (mut c Arm64) cg_jmp_back(_start i64) {
	panic('Arm64.cg_jmp_back() not implemented')
}

fn (mut c Arm64) cg_init_struct(_var Var, _init ast.StructInit) {
	panic('Arm64.cg_init_struct() not implemented')
}

fn (mut c Arm64) cg_load_fp_var(_var Var, _config VarConfig) {
	panic('Arm64.cg_load_fp_var() not implemented')
}

fn (mut c Arm64) cg_load_fp(_val f64) {
	panic('Arm64.cg_load_fp() not implemented')
}

fn (mut c Arm64) for_in_stmt(_node ast.ForInStmt) {
	panic('Arm64.for_in_stmt() not implemented')
}

fn (mut c Arm64) cg_cmp_zero(_reg Register) {
	panic('Arm64.cg_cmp_zero() not implemented')
}

fn (mut c Arm64) cg_cmp_var_reg(_var Var, _reg Register, _config VarConfig) {
	panic('Arm64.cg_cmp_var_reg() not implemented')
}

fn (mut c Arm64) cg_cmp_var(_var Var, _val i32, _config VarConfig) {
	panic('Arm64.cg_cmp_var() not implemented')
}

fn (mut c Arm64) cg_dec_var(_var Var, _config VarConfig) {
	panic('Arm64.cg_dec_var() not implemented')
}

fn (mut c Arm64) cg_inc_var(_var Var, _config VarConfig) {
	panic('Arm64.cg_inc_var() not implemented')
}

fn (mut c Arm64) cg_cjmp(_op JumpOp) i32 {
	panic('Arm64.cg_cjmp() not implemented')
}

fn (mut c Arm64) cg_jmp(_addr i32) i32 {
	panic('Arm64.cg_jmp() not implemented')
}

fn (mut c Arm64) cg_gen_syscall(_node ast.CallExpr) {
	panic('Arm64.cg_gen_syscall() not implemented')
}

fn (mut c Arm64) cg_movabs(_reg Register, _val i64) {
	panic('Arm64.cg_movabs() not implemented')
}

fn (mut c Arm64) gen_selector_expr(_expr ast.SelectorExpr) {
	panic('Arm64.gen_selector_expr() not implemented')
}

fn (mut c Arm64) cg_mov_reg_to_var(_var Var, _reg Register, _config VarConfig) {
	panic('Arm64.cg_mov_reg_to_var() not implemented')
}

fn (mut c Arm64) cg_mov_int_to_var(_var Var, _integer i32, _config VarConfig) {
	panic('Arm64.cg_mov_int_to_var() not implemented')
}

fn (mut c Arm64) cg_call(addr i32) i64 {
	call_addr := c.g.pos()
	if addr == 0 {
		c.bl()
		return call_addr
	}
	instruction := arm64_encode_bl_instruction(i64(addr) - call_addr) or {
		c.g.n_error(err.msg())
		return call_addr
	}
	c.g.write32(i32(instruction))
	c.g.println('bl ${addr}')
	return call_addr
}

fn (mut c Arm64) cg_zero_fill(_size i32, _var LocalVar) {
	panic('Arm64.cg_zero_fill() not implemented')
}

fn (mut c Arm64) cg_call_addr_at(addr i32, at i64) i64 {
	instruction := arm64_encode_bl_instruction(i64(addr) - at) or {
		c.g.n_error(err.msg())
		return 0
	}
	return i64(instruction & arm64_branch_imm_mask)
}

fn (mut c Arm64) cg_push(_r Register) {
	panic('Arm64.cg_push() not implemented')
}

pub fn (mut c Arm64) cg_add(_r Register, _val i32) {
	panic('Arm64.cg_add() not implemented')
}

pub fn (mut c Arm64) cg_add_reg(_r Register, _r2 Register) {
	panic('Arm64.cg_add_reg() not implemented')
}

fn (mut c Arm64) cg_pop(_r Register) {
	panic('Arm64.cg_pop() not implemented')
}

fn (mut c Arm64) cg_cmp_reg(_reg Register, _reg2 Register) {
	panic('Arm64.cg_add_reg() not implemented')
}

fn (mut c Arm64) cg_create_string_struct(_typ ast.Type, _name string, _str string) i32 {
	panic('Arm64.cg_create_string_struct() not implemented')
}

fn (mut c Arm64) cg_mov_deref(_reg Register, _regptr Register, _typ ast.Type) {
	panic('Arm64.cg_mov_deref() not implemented')
}

fn (mut c Arm64) cg_patch_relative_jmp(_pos i32, _addr i64) {
	panic('Arm64.cg_patch_relative_jmp() not implemented')
}

fn (mut c Arm64) cg_mul_reg(_a Register, _b Register) {
	panic('Arm64.cg_mul_reg() not implemented')
}

fn (mut c Arm64) cg_assign_var(_var IdentVar, _raw_type ast.Type) {
	panic('Arm64.cg_assign_var() not implemented')
}

fn (mut c Arm64) cg_sub_reg(_a Register, _b Register) {
	panic('Arm64.cg_sub_reg() not implemented')
}
