// Copyright (c) 2019-2023 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module native

import arrays
import v.ast
import v.token

pub struct Amd64 {
mut:
	g &Gen = unsafe { nil }
	// arm64 specific stuff for code generation
	is_16bit_aligned bool
}

// The registers are ordered for faster generation
// push rax => 50
// push rcx => 51 etc
enum Amd64Register {
	rax
	rcx
	rdx
	rbx
	rsp
	rbp
	rsi
	rdi
	r8
	r9
	r10
	r11
	r12
	r13
	r14
	r15
	eax
	edi
	edx
}

enum Amd64SSERegister {
	xmm0
	xmm1
	xmm2
	xmm3
	xmm4
	xmm5
	xmm6
	xmm7
	xmm8
	xmm9
	xmm10
	xmm11
	xmm12
	xmm13
	xmm14
	xmm15
}

enum Amd64SetOp {
	e = 0x940f
	ne = 0x950f
	g = 0x9f0f
	ge = 0x9d0f
	l = 0x9c0f
	le = 0x9e0f
	a = 0x970f
	ae = 0x930f
	b = 0x920f
	be = 0x960f
	p = 0x9a0f
	np = 0x9b0f
}

[params]
struct AvailableAmd64Register {
	available Amd64Register
}

[params]
struct Amd64RegisterOption {
	reg    Amd64Register    = Amd64Register.rax
	ssereg Amd64SSERegister = Amd64SSERegister.xmm0
}

const (
	fn_arg_registers     = [Amd64Register.rdi, .rsi, .rdx, .rcx, .r8, .r9]
	fn_arg_sse_registers = [Amd64SSERegister.xmm0, .xmm1, .xmm2, .xmm3, .xmm4, .xmm5, .xmm6, .xmm7]
	amd64_cpuregs        = ['eax', 'ecx', 'edx', 'ebx', 'esp', 'ebp', 'esi', 'edi']
)

fn (mut c Amd64) main_reg() Register {
	return Amd64Register.rax
}

fn (mut c Amd64) dec(reg Amd64Register) {
	c.g.write16(0xff48)
	match reg {
		.rax { c.g.write8(0xc8) }
		.rbx { c.g.write8(0xcb) }
		.rcx { c.g.write8(0xc9) }
		.rsi { c.g.write8(0xce) }
		.rdi { c.g.write8(0xcf) }
		.r12 { c.g.write8(0xc4) }
		else { panic('unhandled inc ${reg}') }
	}
	c.g.println('dec ${reg}')
}

[inline]
fn byt(n int, s int) u8 {
	return u8((n >> (s * 8)) & 0xff)
}

fn (mut c Amd64) inc(reg Amd64Register) {
	c.g.write8(0x48)
	c.g.write8(0xff)
	c.g.write8(0xc0 + int(reg))
	c.g.println('inc ${reg}')
}

fn (mut c Amd64) neg(reg Amd64Register) {
	c.g.write8(0x48)
	c.g.write8(0xf7)
	match reg {
		.rax { c.g.write8(0xd8) }
		else { panic('unhandled neg ${reg}') }
	}
	c.g.println('neg ${reg}')
}

fn (mut c Amd64) cmp(reg Amd64Register, size Size, val i64) {
	if c.g.pref.arch != .amd64 {
		panic('cmp')
	}
	// Second byte depends on the size of the value
	match size {
		._8 {
			c.g.write8(0x48)
			c.g.write8(0x83)
		}
		._32 {
			c.g.write8(0x4a)
			c.g.write8(0x81)
		}
		else {
			panic('unhandled cmp')
		}
	}
	// Third byte depends on the register being compared to
	match reg {
		.r12 { c.g.write8(0xfc) }
		.rsi { c.g.write8(0x3f) }
		.eax { c.g.write8(0xf8) }
		.rbx { c.g.write8(0xfb) }
		else { panic('unhandled cmp') }
	}
	match size {
		._8 {
			c.g.write8(int(val))
		}
		._32 {
			c.g.write32(int(val))
		}
		else {
			panic('unhandled cmp')
		}
	}
	c.g.println('cmp ${reg}, ${val}')
}

// `cmp rax, rbx`
fn (mut c Amd64) cmp_reg(reg Amd64Register, reg2 Amd64Register) {
	match reg {
		.rax {
			match reg2 {
				.rdx {
					c.g.write([u8(0x48), 0x39, 0xd0])
				}
				.rbx {
					c.g.write([u8(0x48), 0x39, 0xd8])
				}
				else {
					c.g.n_error('Cannot compare ${reg} and ${reg2}')
				}
			}
		}
		.rdx {
			match reg2 {
				.rax {
					c.g.write([u8(0x48), 0x39, 0xc2])
				}
				else {
					c.g.n_error('Cannot compare ${reg} and ${reg2}')
				}
			}
		}
		.rbx {
			match reg2 {
				.rax {
					c.g.write([u8(0x48), 0x39, 0xc3])
				}
				else {
					c.g.n_error('Cannot compare ${reg} and ${reg2}')
				}
			}
		}
		.rdi {
			match reg2 {
				.rsi {
					c.g.write([u8(0x48), 0x39, 0xf7])
				}
				else {
					c.g.n_error('Cannot compare ${reg} and ${reg2}')
				}
			}
		}
		else {
			c.g.n_error('Cannot compare ${reg} and ${reg2}')
		}
	}
	c.g.println('cmp ${reg}, ${reg2}')
}

// cmp $reg, 0
fn (mut c Amd64) cmp_zero(reg Register) {
	match reg as Amd64Register {
		.rax {
			c.g.write8(0x48)
			c.g.write8(0x83)
			c.g.write8(0xf8)
		}
		.eax {
			c.g.write8(0x83)
			c.g.write8(0xf8)
		}
		else {
			c.g.n_error('unhandled cmp ${reg}, 0')
		}
	}

	c.g.write8(0x00)
	c.g.println('cmp ${reg}, 0')
}

fn (mut c Amd64) cmp_var_reg(var Var, reg Register, config VarConfig) {
	match var {
		ast.Ident {
			var_object := c.g.get_var_from_ident(var)
			match var_object {
				LocalVar {
					c.cmp_var_reg(var_object as LocalVar, reg, config)
				}
				GlobalVar {
					c.cmp_var_reg(var_object as GlobalVar, reg, config)
				}
				Register {
					// TODO
					// g.cmp()
				}
			}
		}
		LocalVar {
			// TODO: size
			c.g.write8(0x48) // 83 for 1 byte?
			c.g.write8(0x39)
			offset := var.offset - config.offset
			is_far_var := offset > 0x80 || offset < -0x7f
			c.g.write8(if is_far_var { 0x85 } else { 0x45 })
			if is_far_var {
				c.g.write32(int((0xffffffff - i64(offset) + 1) % 0x100000000))
			} else {
				c.g.write8((0xff - offset + 1) % 0x100)
			}
			c.g.println('cmp var `${var.name}`, ${reg}')
		}
		GlobalVar {
			// TODO
		}
	}
}

fn (mut c Amd64) cmp_var(var Var, val int, config VarConfig) {
	match var {
		ast.Ident {
			var_object := c.g.get_var_from_ident(var)
			match var_object {
				LocalVar {
					c.cmp_var(var_object as LocalVar, val, config)
				}
				GlobalVar {
					c.cmp_var(var_object as GlobalVar, val, config)
				}
				Register {
					// TODO
					// g.cmp()
				}
			}
		}
		LocalVar {
			// TODO: size
			c.g.write8(0x81) // 83 for 1 byte?
			offset := var.offset - config.offset
			is_far_var := offset > 0x80 || offset < -0x7f
			c.g.write8(if is_far_var { 0xbd } else { 0x7d })
			if is_far_var {
				c.g.write32(int((0xffffffff - i64(offset) + 1) % 0x100000000))
			} else {
				c.g.write8((0xff - offset + 1) % 0x100)
			}
			c.g.write32(val)
			c.g.println('cmp var `${var.name}` ${val}')
		}
		GlobalVar {
			// TODO
		}
	}
}

// `sub DWORD [rbp-0x4], 1`
fn (mut c Amd64) dec_var(var Var, config VarConfig) {
	match var {
		ast.Ident {
			var_object := c.g.get_var_from_ident(var)
			match var_object {
				LocalVar {
					c.dec_var(var_object as LocalVar, config)
				}
				GlobalVar {
					c.dec_var(var_object as GlobalVar, config)
				}
				Register {
					// TODO
					// g.dec()
				}
			}
		}
		LocalVar {
			// TODO: size
			c.g.write8(0x81) // 83 for 1 byte
			offset := var.offset - config.offset
			is_far_var := offset > 0x80 || offset < -0x7f
			c.g.write8(if is_far_var { 0xad } else { 0x6d })
			if is_far_var {
				c.g.write32(int((0xffffffff - i64(offset) + 1) % 0x100000000))
			} else {
				c.g.write8((0xff - offset + 1) % 0x100)
			}
			c.g.write32(1)
			c.g.println('dec_var `${var.name}`')
		}
		GlobalVar {
			// TODO
		}
	}
}

// `add DWORD [rbp-0x4], 1`
fn (mut c Amd64) inc_var(var Var, config VarConfig) {
	match var {
		ast.Ident {
			var_object := c.g.get_var_from_ident(var)
			match var_object {
				LocalVar {
					c.inc_var(var_object as LocalVar, config)
				}
				GlobalVar {
					c.inc_var(var_object as GlobalVar, config)
				}
				Register {
					// TODO
					// g.inc()
				}
			}
		}
		LocalVar {
			// TODO: size
			c.g.write8(0x81) // 83 for 1 byte
			offset := var.offset - config.offset
			is_far_var := offset > 0x80 || offset < -0x7f
			c.g.write8(if is_far_var { 0x85 } else { 0x45 })
			if is_far_var {
				c.g.write32(int((0xffffffff - i64(offset) + 1) % 0x100000000))
			} else {
				c.g.write8((0xff - offset + 1) % 0x100)
			}
			c.g.write32(1)
			c.g.println('inc_var `${var.name}`')
		}
		GlobalVar {
			// TODO
		}
	}
}

fn (op JumpOp) amd64() u16 {
	return match op {
		.je {
			0x840f
		}
		.jne {
			0x850f
		}
		.jg {
			0x8f0f
		}
		.jge {
			0x8d0f
		}
		.jl {
			0x8c0f
		}
		.jle {
			0x8e0f
		}
		.js {
			0x880f
		}
		.jnb {
			0x830f
		}
	}
}

fn (mut c Amd64) cjmp(op JumpOp) int {
	c.g.write16(op.amd64())
	pos := c.g.pos()
	c.g.write32(placeholder)
	c.g.println('${op}')
	return int(pos)
}

fn (mut c Amd64) jmp(addr int) int {
	c.g.write8(0xe9)
	pos := c.g.pos()
	c.g.write32(addr) // 0xffffff
	c.g.println('jmp')
	// return the position of jump address for placeholder
	return int(pos)
}

fn (mut c Amd64) jmp_back(start i64) {
	c.jmp(int(0xffffffff - (c.g.pos() + 5 - start) + 1))
}

// SETcc al
fn (mut c Amd64) cset(op Amd64SetOp) {
	c.g.write16(u16(op))
	c.g.write8(0xc0)
	c.g.println('set${op} al')
}

fn abs(a i64) i64 {
	return if a < 0 { -a } else { a }
}

fn (mut c Amd64) tmp_jle(addr i64) {
	// Calculate the relative offset to jump to
	// (`addr` is absolute address)
	offset := 0xff - c.g.abs_to_rel_addr(addr)
	c.g.write8(0x7e)
	c.g.write8(offset)
	c.g.println('jle')
}

fn (mut c Amd64) jl(addr i64) {
	offset := 0xff - c.g.abs_to_rel_addr(addr)
	c.g.write8(0x7c)
	c.g.write8(offset)
	c.g.println('jl')
}

fn (mut c Amd64) mov32(reg Amd64Register, val int) {
	if int(reg) >= int(Amd64Register.r8) {
		c.g.write8(0x41)
	}
	c.g.write8(0xb8 + int(reg) % 8)
	c.g.write32(val)
	c.g.println('mov32 ${reg}, ${val}')
}

fn (mut c Amd64) mov64(reg Register, val i64) {
	match reg as Amd64Register {
		.eax {
			c.g.write8(0xb8)
			c.g.write8(0x49)
		}
		.rax {
			c.g.write8(0x48)
			c.g.write8(0xb8)
		}
		.rcx {
			c.g.write8(0x48)
			c.g.write8(0xc7)
			c.g.write8(0xc1)
		}
		.rdx {
			c.g.write8(0x48)
			c.g.write8(0xc7)
			c.g.write8(0xc2)
			c.g.write32(int(val))
			c.g.println('mov32 ${reg}, ${val}')
			return
		}
		.rbx {
			c.g.write8(0x48)
			c.g.write8(0xc7)
			c.g.write8(0xc3)
		}
		.edi {
			c.g.write8(0xbe)
		}
		.rsi {
			c.g.write8(0x48)
			c.g.write8(0xbe)
		}
		.rdi {
			c.g.write8(0x48)
			c.g.write8(0xbf)
		}
		else {
			eprintln('unhandled mov64 ${reg}')
		}
	}
	c.g.write64(val)
	c.g.println('mov64 ${reg}, ${val}')
}

fn (mut c Amd64) movabs(r Register, val i64) {
	reg := r as Amd64Register
	c.g.write8(0x48 + int(reg) / 8)
	c.g.write8(0xb8 + int(reg) % 8)
	c.g.write64(val)
	c.g.println('movabs ${reg}, ${val}')
}

fn (mut c Amd64) mov_deref(reg Amd64Register, regptr Amd64Register, typ ast.Type) {
	size := c.g.get_type_size(typ)
	if size !in [1, 2, 4, 8] {
		c.g.n_error('Invalid size on dereferencing')
	}
	is_signed := !typ.is_any_kind_of_pointer() && typ.is_signed()
	rex := int(reg) / 8 * 4 + int(regptr) / 8
	if size == 4 && !is_signed {
		if rex > 0 {
			c.g.write8(0x40 + rex)
		}
		c.g.write8(0x8b)
	} else {
		c.g.write8(0x48 + int(reg) / 8 * 4 + int(regptr) / 8)
		if size <= 2 {
			c.g.write8(0x0f)
		}
		c.g.write8(match true {
			size == 1 && is_signed { 0xbe }
			size == 1 && !is_signed { 0xb6 }
			size == 2 && is_signed { 0xbf }
			size == 2 && !is_signed { 0xb7 }
			size == 4 && is_signed { 0x63 }
			else { 0x8b }
		})
	}
	c.g.write8(int(reg) % 8 * 8 + int(regptr) % 8)
	c.g.println('mov ${reg}, [${regptr}]')
}

fn (mut c Amd64) mov_store(regptr Amd64Register, reg Amd64Register, size Size) {
	if size == ._16 {
		c.g.write8(0x66)
	}
	if size == ._64 {
		c.g.write8(0x48 + int(reg) / 8 * 4 + int(regptr) / 8)
	}
	c.g.write8(if size == ._8 { 0x88 } else { 0x89 })
	c.g.write8(int(reg) % 8 * 8 + int(regptr) % 8)
	c.g.println('mov [${regptr}], ${reg}')
}

fn (mut c Amd64) mov_reg_to_var(var Var, r Register, config VarConfig) {
	reg := r as Amd64Register
	match var {
		ast.Ident {
			var_object := c.g.get_var_from_ident(var)
			match var_object {
				LocalVar {
					c.mov_reg_to_var(var_object as LocalVar, reg, config)
				}
				GlobalVar {
					c.mov_reg_to_var(var_object as GlobalVar, reg, config)
				}
				Register {
					// TODO
				}
			}
		}
		LocalVar {
			offset := var.offset - config.offset
			is_far_var := offset > 0x80 || offset < -0x7f
			typ := if config.typ == 0 { var.typ } else { config.typ }

			mut size_str := 'UNKNOWN'
			is_extended_register := int(reg) >= int(Amd64Register.r8)
				&& int(reg) <= int(Amd64Register.r15)
			match typ {
				ast.i64_type_idx, ast.u64_type_idx, ast.isize_type_idx, ast.usize_type_idx,
				ast.int_literal_type_idx {
					c.g.write16(0x8948 + if is_extended_register { 4 } else { 0 })
					size_str = 'QWORD'
				}
				ast.int_type_idx, ast.u32_type_idx, ast.rune_type_idx {
					if is_extended_register {
						c.g.write8(0x44)
					}
					c.g.write8(0x89)
					size_str = 'DWORD'
				}
				ast.i16_type_idx, ast.u16_type_idx {
					c.g.write8(0x66)
					if is_extended_register {
						c.g.write8(0x44)
					}
					c.g.write8(0x89)
					size_str = 'WORD'
				}
				ast.i8_type_idx, ast.u8_type_idx, ast.char_type_idx, ast.bool_type_idx {
					if is_extended_register {
						c.g.write8(0x44)
					}
					c.g.write8(0x88)
					size_str = 'BYTE'
				}
				else {
					if typ.is_any_kind_of_pointer() {
						c.g.write16(0x8948 + if is_extended_register { 4 } else { 0 })
						size_str = 'QWORD'
					} else {
						ts := c.g.table.sym(typ.idx())
						if ts.info is ast.Enum {
							if is_extended_register {
								c.g.write8(0x44)
							}
							c.g.write8(0x89)
							size_str = 'DWORD'
						} else {
							c.g.n_error('unsupported type for mov_reg_to_var')
						}
					}
				}
			}
			far_var_offset := if is_far_var { 0x40 } else { 0 }
			match reg {
				.eax, .rax, .r8 { c.g.write8(0x45 + far_var_offset) }
				.edi, .rdi { c.g.write8(0x7d + far_var_offset) }
				.rsi { c.g.write8(0x75 + far_var_offset) }
				.rdx { c.g.write8(0x55 + far_var_offset) }
				.rcx, .r9 { c.g.write8(0x4d + far_var_offset) }
				else { c.g.n_error('mov_from_reg ${reg}') }
			}
			if is_far_var {
				c.g.write32(int((0xffffffff - i64(offset) + 1) % 0x100000000))
			} else {
				c.g.write8((0xff - offset + 1) % 0x100)
			}
			c.g.println('mov ${size_str} PTR [rbp-${offset.hex2()}],${reg}')
		}
		GlobalVar {
			// TODO
		}
	}
}

fn (mut c Amd64) mov_int_to_var(var Var, integer int, config VarConfig) {
	match var {
		ast.Ident {
			var_object := c.g.get_var_from_ident(var)
			match var_object {
				LocalVar {
					c.mov_int_to_var(var_object as LocalVar, integer, config)
				}
				GlobalVar {
					c.mov_int_to_var(var_object as GlobalVar, integer, config)
				}
				Register {
					// TODO
				}
			}
		}
		LocalVar {
			offset := var.offset - config.offset
			typ := if config.typ == 0 { var.typ } else { config.typ }
			is_far_var := offset > 0x80 || offset < -0x7f

			match typ {
				ast.i8_type_idx, ast.u8_type_idx, ast.char_type_idx {
					c.g.write8(0xc6)
					c.g.write8(if is_far_var { 0x85 } else { 0x45 })
					if is_far_var {
						c.g.write32(int((0xffffffff - i64(offset) + 1) % 0x100000000))
					} else {
						c.g.write8((0xff - offset + 1) % 0x100)
					}
					c.g.write8(u8(integer))
					c.g.println('mov BYTE PTR[rbp-${offset.hex2()}], ${integer}')
				}
				ast.i16_type_idx, ast.u16_type_idx {
					c.g.write16(0xc766)
					c.g.write8(if is_far_var { 0x85 } else { 0x45 })
					if is_far_var {
						c.g.write32(int((0xffffffff - i64(offset) + 1) % 0x100000000))
					} else {
						c.g.write8((0xff - offset + 1) % 0x100)
					}
					c.g.write16(u16(integer))
					c.g.println('mov WORD PTR[rbp-${offset.hex2()}], ${integer}')
				}
				ast.int_type_idx, ast.u32_type_idx, ast.rune_type_idx {
					c.g.write8(0xc7)
					c.g.write8(if is_far_var { 0x85 } else { 0x45 })
					if is_far_var {
						c.g.write32(int((0xffffffff - i64(offset) + 1) % 0x100000000))
					} else {
						c.g.write8((0xff - offset + 1) % 0x100)
					}
					c.g.write32(integer)
					c.g.println('mov DWORD PTR[rbp-${offset.hex2()}], ${integer}')
				}
				ast.i64_type_idx, ast.u64_type_idx, ast.isize_type_idx, ast.usize_type_idx,
				ast.int_literal_type_idx {
					c.g.write8(0x48)
					c.g.write8(0xc7)
					c.g.write8(if is_far_var { 0x85 } else { 0x45 })
					if is_far_var {
						c.g.write32(int((0xffffffff - i64(offset) + 1) % 0x100000000))
					} else {
						c.g.write8((0xff - offset + 1) % 0x100)
					}
					c.g.write32(integer)
					c.g.println('mov QWORD PTR[rbp-${offset.hex2()}], ${integer}')
				}
				else {
					c.g.n_error('unhandled mov int type: ${typ}')
				}
			}
		}
		GlobalVar {
			// TODO
		}
	}
}

fn (mut c Amd64) lea_var_to_reg(r Register, var_offset int) {
	reg := r as Amd64Register

	is_far_var := var_offset > 0x80 || var_offset < -0x7f
	match reg {
		.rax, .rbx, .rsi, .rdi {
			c.g.write8(0x48)
		}
		else {}
	}
	c.g.write8(0x8d)
	far_var_offset := if is_far_var { 0x40 } else { 0 }
	match reg {
		.eax, .rax { c.g.write8(0x45 + far_var_offset) }
		.edi, .rdi { c.g.write8(0x7d + far_var_offset) }
		.rsi { c.g.write8(0x75 + far_var_offset) }
		.rdx { c.g.write8(0x55 + far_var_offset) }
		.rbx { c.g.write8(0x5d + far_var_offset) }
		.rcx { c.g.write8(0x4d + far_var_offset) }
		else { c.g.n_error('lea_var_to_reg ${reg}') }
	}
	if is_far_var {
		c.g.write32(int((0xffffffff - i64(var_offset) + 1) % 0x100000000))
	} else {
		c.g.write8((0xff - var_offset + 1) % 0x100)
	}
	c.g.println('lea ${reg}, [rbp-${var_offset.hex2()}]')
}

fn (mut c Amd64) mov_var_to_reg(reg Register, var Var, config VarConfig) {
	match var {
		ast.Ident {
			var_object := c.g.get_var_from_ident(var)
			match var_object {
				LocalVar {
					c.mov_var_to_reg(reg, var_object as LocalVar, config)
				}
				GlobalVar {
					c.mov_var_to_reg(reg, var_object as GlobalVar, config)
				}
				Register {
					// TODO
				}
			}
		}
		LocalVar {
			offset := var.offset - config.offset
			is_far_var := offset > 0x80 || offset < -0x7f
			typ := if config.typ == 0 { var.typ } else { config.typ }
			size := c.g.get_type_size(typ)
			is_signed := !typ.is_any_kind_of_pointer() && typ.is_signed()

			instruction, size_str := match true {
				size == 4 && is_signed {
					// movsxd rax, DWORD PTR [rbp-0x8]
					c.g.write16(0x6348)
					'movsxd', 'DWORD'
				}
				size == 4 && !is_signed {
					// mov eax, DWORD PTR [rbp-0x8]
					c.g.write8(0x8b)
					'mov', 'DWORD'
				}
				size == 2 && is_signed {
					// movsx rax, WORD PTR [rbp-0x8]
					c.g.write([u8(0x48), 0x0f, 0xbf])
					'movsx', 'WORD'
				}
				size == 2 && !is_signed {
					// movzx rax, WORD PTR [rbp-0x8]
					c.g.write([u8(0x48), 0x0f, 0xb7])
					'movzx', 'WORD'
				}
				size == 1 && is_signed {
					// movsx rax, BYTE PTR [rbp-0x8]
					c.g.write([u8(0x48), 0x0f, 0xbe])
					'movsx', 'BYTE'
				}
				size == 1 && !is_signed {
					// movzx rax, BYTE PTR [rbp-0x8]
					c.g.write([u8(0x48), 0x0f, 0xb6])
					'movzx', 'BYTE'
				}
				else {
					// mov rax, QWORD PTR [rbp-0x8]
					c.g.write16(0x8b48)
					'mov', 'QWORD'
				}
			}
			far_var_offset := if is_far_var { 0x40 } else { 0 }
			match reg as Amd64Register {
				.eax, .rax { c.g.write8(0x45 + far_var_offset) }
				.edi, .rdi { c.g.write8(0x7d + far_var_offset) }
				.rsi { c.g.write8(0x75 + far_var_offset) }
				.rdx { c.g.write8(0x55 + far_var_offset) }
				.rbx { c.g.write8(0x5d + far_var_offset) }
				.rcx { c.g.write8(0x4d + far_var_offset) }
				else { c.g.n_error('mov_var_to_reg ${reg}') }
			}
			if is_far_var {
				c.g.write32(int((0xffffffff - i64(offset) + 1) % 0x100000000))
			} else {
				c.g.write8((0xff - offset + 1) % 0x100)
			}
			c.g.println('${instruction} ${reg}, ${size_str} PTR [rbp-${offset.hex2()}]')
		}
		GlobalVar {
			// TODO
		}
	}
}

fn (mut c Amd64) mov_extend_reg(a Amd64Register, b Amd64Register, typ ast.Type) {
	size := c.g.get_type_size(typ)
	is_signed := !typ.is_any_kind_of_pointer() && typ.is_signed()

	if size in [1, 2, 4] {
		if size == 4 && !is_signed {
			c.g.write8(0x40 + if int(a) >= int(Amd64Register.r8) { 1 } else { 0 } +
				if int(b) >= int(Amd64Register.r8) { 4 } else { 0 })
			c.g.write8(0x89)
		} else {
			c.g.write8(0x48 + if int(a) >= int(Amd64Register.r8) { 1 } else { 0 } +
				if int(b) >= int(Amd64Register.r8) { 4 } else { 0 })
			if size in [1, 2] {
				c.g.write8(0x0f)
			}
			c.g.write8(match true {
				size == 1 && is_signed { 0xbe }
				size == 1 && !is_signed { 0xb6 }
				size == 2 && is_signed { 0xbf }
				size == 2 && !is_signed { 0xb7 }
				else { 0x63 }
			})
		}
		c.g.write8(0xc0 + int(a) % 8 * 8 + int(b) % 8)
		instruction := if is_signed { 's' } else { 'z' }
		c.g.println('mov${instruction}x ${a}, ${b}')
	}
}

fn (mut c Amd64) call_addr_at(addr int, at i64) i64 {
	// Need to calculate the difference between current position (position after the e8 call)
	// and the function to call.f
	// +5 is to get the posistion "e8 xx xx xx xx"
	// Not sure about the -1.
	return 0xffffffff - (at + 5 - addr - 1)
}

fn (mut c Amd64) call(addr int) i64 {
	rel := c.call_addr_at(addr, c.g.pos())
	c_addr := c.g.pos()
	// println('call addr=$addr.hex2() rel_addr=$rel.hex2() pos=$g.buf.len')
	c.g.write8(0xe8)

	c.g.write32(int(rel))
	c.g.println('call ${addr}')

	return c_addr
}

fn (mut c Amd64) extern_call(addr int) {
	match c.g.pref.os {
		.linux {
			c.g.write8(0xff)
			c.g.write8(0x15)
			c.g.write32(0)
			c.g.println('call *@GOTPCREL(%rip)')
		}
		else {
			c.g.n_error('extern calls are not implemented for ${c.g.pref.os}')
		}
	}
}

fn (mut c Amd64) gen_syscall(node ast.CallExpr) {
	mut i := 0
	mut ra := [Amd64Register.rax, .rdi, .rsi, .rdx]
	for i < node.args.len {
		expr := node.args[i].expr
		if i >= ra.len {
			c.g.warning('Too many arguments for syscall', node.pos)
			return
		}
		match expr {
			ast.IntegerLiteral {
				c.mov(ra[i], expr.val.int())
			}
			ast.BoolLiteral {
				c.mov(ra[i], if expr.val { 1 } else { 0 })
			}
			ast.SelectorExpr {
				mut done := false
				if expr.field_name == 'str' {
					match expr.expr {
						ast.StringLiteral {
							s := c.g.eval_str_lit_escape_codes(expr.expr)
							c.g.allocate_string(s, 2, .abs64)
							c.mov64(ra[i], 1)
							done = true
						}
						else {}
					}
				}
				if !done {
					c.g.v_error('Unknown selector in syscall argument type ${expr}', node.pos)
				}
			}
			ast.StringLiteral {
				if expr.language != .c {
					c.g.warning('C.syscall expects c"string" or "string".str, C backend will crash',
						node.pos)
				}
				s := c.g.eval_str_lit_escape_codes(expr)
				c.g.allocate_string(s, 2, .abs64)
				c.mov64(ra[i], 1)
			}
			else {
				c.g.v_error('Unknown syscall ${expr.type_name()} argument type ${expr}',
					node.pos)
				return
			}
		}
		i++
	}
	c.syscall()
}

fn (mut c Amd64) syscall() {
	c.g.write8(0x0f)
	c.g.write8(0x05)
	c.g.println('syscall')
}

fn (mut c Amd64) svc() {
	panic('the svc instruction is not available with amd64')
}

fn (mut c Amd64) cdq() {
	c.g.write8(0x99)
	c.g.println('cdq')
}

fn (mut c Amd64) ret() {
	c.g.write8(0xc3)
	c.g.println('ret')
}

fn (mut c Amd64) push(reg Amd64Register) {
	if int(reg) < int(Amd64Register.r8) {
		c.g.write8(0x50 + int(reg))
	} else {
		c.g.write8(0x41)
		c.g.write8(0x50 + int(reg) - 8)
	}
	c.is_16bit_aligned = !c.is_16bit_aligned
	c.g.println('push ${reg}')
}

pub fn (mut c Amd64) pop(reg Amd64Register) {
	if int(reg) >= int(Amd64Register.r8) && int(reg) <= int(Amd64Register.r15) {
		c.g.write8(0x41)
	}
	c.g.write8(0x58 + int(reg) % 8)
	c.is_16bit_aligned = !c.is_16bit_aligned
	c.g.println('pop ${reg}')
}

pub fn (mut c Amd64) sub8(reg Amd64Register, val int) {
	c.g.write8(0x48)
	c.g.write8(0x83)
	c.g.write8(0xe8 + int(reg)) // TODO rax is different?
	c.g.write8(val)
	c.g.println('sub8 ${reg},${val.hex2()}')
}

pub fn (mut c Amd64) sub(reg Amd64Register, val int) {
	c.g.write8(0x48)
	if reg == .rax {
		c.g.write8(0x2d)
	} else {
		c.g.write8(0x81)
		c.g.write8(0xe8 + int(reg))
	}
	c.g.write32(val)
	c.g.println('sub ${reg},${val.hex2()}')
}

pub fn (mut c Amd64) add(reg Amd64Register, val int) {
	c.g.write8(0x48)
	if reg == .rax {
		c.g.write8(0x05)
	} else {
		c.g.write8(0x81)
		c.g.write8(0xc0 + int(reg))
	}
	c.g.write32(val)
	c.g.println('add ${reg},${val.hex2()}')
}

pub fn (mut c Amd64) add8(reg Amd64Register, val int) {
	c.g.write8(0x48)
	c.g.write8(0x83)
	c.g.write8(0xc0 + int(reg))
	c.g.write8(val)
	c.g.println('add8 ${reg},${val.hex2()}')
}

fn (mut c Amd64) bitand_reg(a Amd64Register, b Amd64Register) {
	c.g.write8(0x48 + if int(a) >= int(Amd64Register.r8) { 1 } else { 0 } +
		if int(b) >= int(Amd64Register.r8) { 4 } else { 0 })
	c.g.write8(0x21)
	c.g.write8(0xc0 + int(a) % 8 + int(b) % 8 * 8)
	c.g.println('and ${a}, ${b}')
}

fn (mut c Amd64) bitor_reg(a Amd64Register, b Amd64Register) {
	c.g.write8(0x48 + if int(a) >= int(Amd64Register.r8) { 1 } else { 0 } +
		if int(b) >= int(Amd64Register.r8) { 4 } else { 0 })
	c.g.write8(0x09)
	c.g.write8(0xc0 + int(a) % 8 + int(b) % 8 * 8)
	c.g.println('or ${a}, ${b}')
}

fn (mut c Amd64) bitxor_reg(a Amd64Register, b Amd64Register) {
	c.g.write8(0x48 + if int(a) >= int(Amd64Register.r8) { 1 } else { 0 } +
		if int(b) >= int(Amd64Register.r8) { 4 } else { 0 })
	c.g.write8(0x31)
	c.g.write8(0xc0 + int(a) % 8 + int(b) % 8 * 8)
	c.g.println('xor ${a}, ${b}')
}

fn (mut c Amd64) bitnot_reg(a Amd64Register) {
	c.g.write8(0x48 + if int(a) >= int(Amd64Register.r8) { 1 } else { 0 })
	c.g.write8(0xf7)
	c.g.write8(0xd0 + int(a) % 8)
	c.g.println('not ${a}')
}

fn (mut c Amd64) shl_reg(a Amd64Register, b Amd64Register) {
	if b != .rcx {
		c.mov_reg(Amd64Register.rcx, b)
	}
	c.g.write8(if int(a) >= int(Amd64Register.r8) { 0x49 } else { 0x48 })
	c.g.write8(0xd3)
	c.g.write8(0xe0 + int(a) % 8)
	c.g.println('shl ${a}, ${b}')
}

fn (mut c Amd64) sar_reg(a Amd64Register, b Amd64Register) {
	if b != .rcx {
		c.mov_reg(Amd64Register.rcx, b)
	}
	c.g.write8(if int(a) > 7 { 0x49 } else { 0x48 })
	c.g.write8(0xd3)
	c.g.write8(0xf8 + int(a) % 8)
	c.g.println('sar ${a}, ${b}')
}

fn (mut c Amd64) shr_reg(a Amd64Register, b Amd64Register) {
	if b != .rcx {
		c.mov_reg(Amd64Register.rcx, b)
	}
	c.g.write8(if int(a) > 7 { 0x49 } else { 0x48 })
	c.g.write8(0xd3)
	c.g.write8(0xe8 + int(a) % 8)
	c.g.println('shr ${a}, ${b}')
}

fn (mut c Amd64) leave() {
	c.g.println('; label 0: return')
	if c.g.defer_stmts.len != 0 {
		// save return value
		c.push(.rax)
		for defer_stmt in c.g.defer_stmts.reverse() {
			name := '_defer${defer_stmt.idx_in_fn}'
			defer_var := c.g.get_var_offset(name)
			c.mov_var_to_reg(Amd64Register.rax, LocalVar{defer_var, ast.i64_type_idx, name})
			c.cmp_zero(Amd64Register.rax)
			label := c.g.labels.new_label()
			jump_addr := c.cjmp(.je)
			c.g.labels.patches << LabelPatch{
				id: label
				pos: jump_addr
			}
			c.g.stmts(defer_stmt.stmts)
			c.g.labels.addrs[label] = c.g.pos()
		}
		c.pop(.rax)
	}
	c.mov_reg(Amd64Register.rsp, Amd64Register.rbp)
	c.pop(.rbp)
	c.ret()
}

// not used?
pub fn (mut c Amd64) var_zero(vo int, size int) {
	c.mov32(.rcx, size)
	c.lea_var_to_reg(Amd64Register.rdi, vo)
	c.g.write8(0xb0)
	c.g.write8(0x00)
	c.g.println('mov al, 0')
	c.rep_stosb()
}

pub fn (mut c Amd64) rep_stosb() {
	c.g.write8(0xf3)
	c.g.write8(0xaa)
	c.g.println('rep stosb')
}

pub fn (mut c Amd64) std() {
	c.g.write8(0xfd)
	c.g.println('std')
}

pub fn (mut c Amd64) cld() {
	c.g.write8(0xfc)
	c.g.println('cld')
}

pub fn (mut c Amd64) cld_repne_scasb() {
	c.cld()
	c.g.write8(0xf2)
	c.g.write8(0xae)
	c.g.println('repne scasb')
}

pub fn (mut c Amd64) xor(r Amd64Register, v int) {
	if v == -1 {
		match r {
			.rcx {
				c.g.write8(0x48)
				c.g.write8(0x83)
				c.g.write8(0xf1)
				c.g.write8(0xff)
				c.g.println('xor rcx, -1')
			}
			else {
				c.g.n_error('unhandled xor')
			}
		}
	} else {
		c.g.n_error('unhandled xor')
	}
}

pub fn (mut c Amd64) test_reg(r Amd64Register) {
	c.g.write8(0x48 + if int(r) >= int(Amd64Register.r8) { 1 } else { 0 } +
		if int(r) >= int(Amd64Register.r8) { 4 } else { 0 })
	c.g.write8(0x85)
	c.g.write8(0xc0 + int(r) % 8 + int(r) % 8 * 8)
	c.g.println('test ${r}, ${r}')
}

// return length in .rax of string pointed by given register
pub fn (mut c Amd64) inline_strlen(r Amd64Register) {
	c.mov_reg(Amd64Register.rdi, r)
	c.mov(Amd64Register.rcx, -1)
	c.mov(Amd64Register.eax, 0)
	c.cld_repne_scasb()
	c.xor(.rcx, -1)
	c.dec(.rcx)
	c.mov_reg(Amd64Register.rax, Amd64Register.rcx)
	c.g.println('strlen rax, ${r}')
}

pub fn (mut c Amd64) apicall(call ApiCall) {
	if c.g.pref.os != .windows {
		c.g.n_error('apicalls are only for windows')
	}
	c.g.write8(0xff)
	c.g.write8(0x15)
	delta := match call {
		.write_file {
			-(0xbcc + c.g.buf.len)
		}
		.get_std_handle {
			-(0xbcc + c.g.buf.len + 8)
		}
		.exit_process {
			-(0xbcc + c.g.buf.len + 16)
		}
	}
	c.g.write32(delta)
}

fn (mut c Amd64) gen_print(s string, fd int) {
	if c.g.pref.os == .windows {
		c.sub(.rsp, 0x38)
		c.mov(Amd64Register.rcx, -11)
		c.apicall(.get_std_handle)
		c.mov_reg(Amd64Register.rcx, Amd64Register.rax)
		// c.mov64(Amd64Register.rdx, c.g.allocate_string(s, 3))
		c.lea(.rdx, c.g.allocate_string(s, 3, .abs64))
		c.mov(Amd64Register.r8, s.len) // string length
		c.g.write([u8(0x4c), 0x8d, 0x4c, 0x24, 0x20]) // lea r9, [rsp+0x20]
		c.g.write([u8(0x48), 0xc7, 0x44, 0x24, 0x20])
		c.g.write32(0) // mov qword[rsp+0x20], 0
		// c.mov(Amd64Register.r9, rsp+0x20)
		c.apicall(.write_file)
	} else {
		c.mov(Amd64Register.eax, c.g.nsyscall(.write))
		c.mov(Amd64Register.edi, fd)
		c.learel(Amd64Register.rsi, c.g.allocate_string(s, 3, .rel32)) // for rsi its 2
		c.mov(Amd64Register.edx, s.len) // len
		c.syscall()
	}
}

// TODO: strlen of string at runtime
pub fn (mut c Amd64) gen_print_reg(r Register, n int, fd int) {
	c.mov_reg(Amd64Register.rsi, r)
	if n < 0 {
		c.inline_strlen(.rsi)
		c.mov_reg(Amd64Register.rdx, Amd64Register.rax)
	} else {
		c.mov(Amd64Register.edx, n)
	}
	c.mov(Amd64Register.eax, c.g.nsyscall(.write))
	c.mov(Amd64Register.edi, fd)
	c.syscall()
}

pub fn (mut c Amd64) gen_exit(expr ast.Expr) {
	c.g.expr(expr)
	c.mov_reg(Amd64Register.rdi, Amd64Register.rax)

	if c.g.pref.os == .windows {
		c.mov_reg(Amd64Register.rcx, Amd64Register.rdi)
		c.apicall(.exit_process)
	} else {
		c.mov(Amd64Register.rax, c.g.nsyscall(.exit))
		c.syscall()
	}
	c.trap() // should never be reached, just in case
}

fn (mut c Amd64) relpc(dst Amd64Register, src Amd64Register) {
	//  488d1d 00000000 lea 0(%rip),%dst
	//  4801d8          add %dst, %src
	match dst {
		.rax {
			match src {
				.rsi {
					c.g.write([u8(0x48), 0x8d, 0x35, 0x00, 0x00, 0x00, 0x00]) // lea rsi, rip
					c.g.write([u8(0x48), 0x01, 0xf0]) // add rax, rsi
				}
				.rbx {
					c.g.write([u8(0x48), 0x8d, 0x1d, 0x00, 0x00, 0x00, 0x00])
					c.g.write([u8(0x48), 0x01, 0xd8])
				}
				else {
					c.g.n_error('relpc requires .rax, {.rsi,.rbx}')
				}
			}
		}
		else {
			c.g.n_error('relpc requires .rax, {.rsi,.rbx}')
		}
	}
}

fn (mut c Amd64) learel(reg Register, val int) {
	c.g.write8(0x48)
	c.g.write8(0x8d)
	match reg as Amd64Register {
		.rax {
			c.g.write8(0x05)
		}
		.rsi {
			c.g.write8(0x35)
		}
		.rcx {
			c.g.write8(0x0d)
		}
		else {
			c.g.n_error('learel must use rsi, rcx or rax')
		}
	}
	c.g.write32(val)
	c.g.println('lea ${reg}, rip + ${val}')
}

fn (mut c Amd64) lea(reg Amd64Register, val int) {
	c.g.write8(0x48)
	c.g.write8(0x8d)
	c.g.write8(0x15)
	c.g.write32(val)
	c.g.println('lea ${reg}, ${val}')
}

fn (mut c Amd64) mov(r Register, val int) {
	reg := r as Amd64Register
	if val == -1 {
		match reg {
			.rax {
				c.g.write8(0x48)
				c.g.write8(0xc7)
				c.g.write8(0xc0)
				c.g.write32(-1)
				c.g.println('mov ${reg}, ${val}')
			}
			.rcx {
				if val == -1 {
					c.g.write8(0x48)
					c.g.write8(0xc7)
					c.g.write8(0xc1)
					c.g.write32(-1)
				} else {
					c.g.write8(0xff)
					c.g.write8(0xff) // mov rcx 0xffff5
				}
				c.g.println('mov ${reg}, ${val}')
			}
			else {
				c.g.n_error('unhandled mov ${reg}, -1')
			}
		}
		c.g.println('mov ${reg}, ${val}')
		return
	}
	if val == 0 {
		// Optimise to xor reg, reg when val is 0
		match reg {
			.eax, .rax {
				c.g.write8(0x31)
				c.g.write8(0xc0)
			}
			.edi, .rdi {
				c.g.write8(0x31)
				c.g.write8(0xff)
			}
			.rcx {
				c.g.write8(0x48)
				c.g.write8(0x31)
				c.g.write8(0xc7)
			}
			.rdx {
				c.g.write8(0x48)
				c.g.write8(0x31)
				c.g.write8(0xd2)
			}
			.edx {
				c.g.write8(0x31)
				c.g.write8(0xd2)
			}
			.rsi {
				c.g.write8(0x48)
				c.g.write8(0x31)
				c.g.write8(0xf6)
			}
			.r12 {
				c.g.write8(0x4d)
				c.g.write8(0x31)
				c.g.write8(0xe4)
			}
			else {
				c.g.n_error('unhandled mov ${reg}, ${reg}')
			}
		}
		c.g.println('xor ${reg}, ${reg}')
	} else {
		match reg {
			.eax, .rax {
				c.g.write8(0xb8)
			}
			.edi, .rdi {
				c.g.write8(0xbf)
			}
			.rcx {
				c.g.write8(0x48)
				c.g.write8(0xc7)
				c.g.write8(0xc1)
			}
			.r8 {
				c.g.write8(0x41)
				c.g.write8(0xb8)
			}
			.r9 {
				c.g.write8(0xb9)
			}
			.rdx, .edx {
				c.g.write8(0xba)
			}
			.rsi {
				//	c.g.write8(0x48) // its 32bit!
				c.g.write8(0xbe)
			}
			.r12 {
				c.g.write8(0x41)
				c.g.write8(0xbc) // r11 is 0xbb etc
			}
			.rbx {
				c.g.write8(0xbb)
			}
			else {
				c.g.n_error('unhandled mov ${reg}')
			}
		}
		c.g.write32(val)
		c.g.println('mov ${reg}, ${val}')
	}
}

fn (mut c Amd64) mul_reg(a Amd64Register, b Amd64Register) {
	if a != .rax {
		panic('mul always operates on rax')
	}
	match b {
		.rax {
			c.g.write8(0x48)
			c.g.write8(0xf7)
			c.g.write8(0xe8)
		}
		.rbx {
			c.g.write8(0x48)
			c.g.write8(0xf7)
			c.g.write8(0xeb)
		}
		.rdx {
			c.g.write8(0x48)
			c.g.write8(0xf7)
			c.g.write8(0xe2)
		}
		else {
			panic('unhandled mul ${b}')
		}
	}
	c.g.println('mul ${b}')
}

fn (mut c Amd64) imul_reg(r Amd64Register) {
	match r {
		.rsi {
			c.g.write8(0x48)
			c.g.write8(0xf7)
			c.g.write8(0xee)
			c.g.println('imul ${r}')
		}
		else {
			panic('unhandled imul ${r}')
		}
	}
}

fn (mut c Amd64) div_reg(a Amd64Register, b Amd64Register) {
	if a != .rax {
		panic('div always operates on rax')
	}
	match b {
		.rax {
			c.g.write8(0x48)
			c.g.write8(0xf7)
			c.g.write8(0xf8)
		}
		.rbx {
			c.mov(Amd64Register.edx, 0)
			c.g.write8(0x48)
			c.g.write8(0xf7)
			c.g.write8(0xfb) // idiv ebx
		}
		.rdx {
			c.g.write8(0x48)
			c.g.write8(0xf7)
			c.g.write8(0xf2)
		}
		else {
			panic('unhandled div ${b}')
		}
	}
	c.g.println('div ${b}')
}

fn (mut c Amd64) mod_reg(a Amd64Register, b Amd64Register) {
	c.div_reg(a, b)
	c.mov_reg(Amd64Register.rdx, Amd64Register.rax)
}

fn (mut c Amd64) sub_reg(a Amd64Register, b Amd64Register) {
	if int(a) <= int(Amd64Register.r15) && int(b) <= int(Amd64Register.r15) {
		c.g.write8(0x48 + if int(a) >= int(Amd64Register.r8) { 1 } else { 0 } +
			if int(b) >= int(Amd64Register.r8) { 4 } else { 0 })
		c.g.write8(0x29)
		c.g.write8(0xc0 + int(a) % 8 + int(b) % 8 * 8)
	} else {
		c.g.n_error('unhandled sub ${a}, ${b}')
	}
	c.g.println('sub ${a}, ${b}')
}

fn (mut c Amd64) add_reg(a Amd64Register, b Amd64Register) {
	if int(a) <= int(Amd64Register.r15) && int(b) <= int(Amd64Register.r15) {
		c.g.write8(0x48 + if int(a) >= int(Amd64Register.r8) { 1 } else { 0 } +
			if int(b) >= int(Amd64Register.r8) { 4 } else { 0 })
		c.g.write8(0x01)
		c.g.write8(0xc0 + int(a) % 8 + int(b) % 8 * 8)
	} else {
		c.g.n_error('unhandled add ${a}, ${b}')
	}
	c.g.println('add ${a}, ${b}')
}

fn (mut c Amd64) mov_reg_amd64(a Amd64Register, b Amd64Register) {
	if int(a) <= int(Amd64Register.r15) && int(b) <= int(Amd64Register.r15) {
		c.g.write8(0x48 + if int(a) >= int(Amd64Register.r8) { 1 } else { 0 } +
			if int(b) >= int(Amd64Register.r8) { 4 } else { 0 })
		c.g.write8(0x89)
		c.g.write8(0xc0 + int(a) % 8 + int(b) % 8 * 8)
	} else {
		c.g.n_error('unhandled mov_reg combination for ${a} ${b}')
	}
	c.g.println('mov ${a}, ${b}')
}

fn (mut c Amd64) mov_reg(a Register, b Register) {
	c.mov_reg_amd64(a as Amd64Register, b as Amd64Register)
}

fn (mut c Amd64) add_store(a Amd64Register, b Amd64Register, size Size) {
	if size == ._16 {
		c.g.write8(0x66)
	}
	if size == ._64 {
		c.g.write8(0x48 + int(b) / 8 * 4 + int(a) / 8)
	}
	c.g.write8(if size == ._8 { 0x00 } else { 0x01 })
	c.g.write8(int(b) % 8 * 8 + int(a) % 8)
	c.g.println('add [${a}], ${b}')
}

fn (mut c Amd64) sub_store(a Amd64Register, b Amd64Register, size Size) {
	if size == ._16 {
		c.g.write8(0x66)
	}
	if size == ._64 {
		c.g.write8(0x48 + int(b) / 8 * 4 + int(a) / 8)
	}
	c.g.write8(if size == ._8 { 0x28 } else { 0x29 })
	c.g.write8(int(b) % 8 * 8 + int(a) % 8)
	c.g.println('sub [${a}], ${b}')
}

fn (mut c Amd64) mul_store(a Amd64Register, b Amd64Register, size Size) {
	if size == ._8 {
	} else {
		if size == ._16 {
			c.g.write8(0x66)
		}
		if size == ._64 {
			c.g.write8(0x48 + int(b) / 8 * 4 + int(a) / 8)
		}
		c.g.write16(0xaf0f)
		c.g.write8(int(b) % 8 * 8 + int(a) % 8)
		c.g.println('imul ${b}, [${a}]')
		c.mov_store(a, b, size)
	}
}

fn (mut c Amd64) sar8(r Amd64Register, val u8) {
	c.g.write8(0x48)
	c.g.write8(0xc1)

	match r {
		.rax {
			c.g.write8(0xf8)
		}
		.rdx {
			c.g.write8(0xfa)
		}
		else {
			panic('unhandled sar ${r}, ${val}')
		}
	}
	c.g.write8(val)
	c.g.println('sar ${r}, ${val}')
}

pub fn (mut c Amd64) call_fn(node ast.CallExpr) {
	name := node.name
	mut n := name
	if !n.contains('.') {
		n = 'main.${n}'
	}
	if node.is_method {
		n = '${c.g.table.get_type_name(node.receiver_type)}.${node.name}'
	}
	addr := c.g.fn_addr[n]

	mut reg_args := []int{}
	mut ssereg_args := []int{}
	mut stack_args := []int{}
	mut args := []ast.CallArg{cap: node.args.len + 2}

	ts := c.g.table.sym(node.return_type)
	return_size := c.g.get_type_size(node.return_type)
	mut return_pos := -1
	mut is_struct_return := false
	if ts.kind in [.struct_, .multi_return] {
		return_pos = c.g.allocate_by_type('', node.return_type)
		if return_size > 16 {
			is_struct_return = true
			args << ast.CallArg{
				typ: ast.voidptr_type_idx
			}
		}
	}

	if node.is_method {
		expr := if !node.left_type.is_ptr() && node.receiver_type.is_ptr() {
			ast.Expr(ast.PrefixExpr{
				op: .amp
				right: node.left
			})
		} else {
			node.left
		}
		args << ast.CallArg{
			expr: expr
			typ: node.receiver_type
		}
	}

	args_offset := args.len
	args << node.args
	args_size := args.map(c.g.get_type_size(it.typ))
	is_floats := args.map(it.typ.is_pure_float())

	mut reg_left := 6 - reg_args.len
	mut ssereg_left := 8
	for i, size in args_size {
		if is_floats[i] && ssereg_left > 0 {
			ssereg_args << i
			ssereg_left--
			continue
		}
		if reg_left > 0 {
			if size <= 8 {
				reg_args << i
				reg_left--
				continue
			} else if size <= 16 && reg_left > 1 {
				reg_args << i
				reg_left -= 2
				continue
			}
		}
		stack_args << i
	}
	reg_size := arrays.fold(reg_args.map((args_size[it] + 7) / 8), 0, fn (acc int, elem int) int {
		return acc + elem
	})
	stack_size := arrays.fold(stack_args.map((args_size[it] + 7) / 8), 0, fn (acc int, elem int) int {
		return acc + elem
	})

	// not aligned now XOR pushed args will be odd
	is_16bit_aligned := c.is_16bit_aligned != (stack_size % 2 == 1)
	if !is_16bit_aligned {
		// dummy data
		c.push(.rbp)
	}
	reg_args << ssereg_args
	reg_args << stack_args
	for i in reg_args.reverse() {
		if i == 0 && is_struct_return {
			c.lea_var_to_reg(Amd64Register.rax, return_pos)
			c.push(.rax)
			continue
		}
		c.g.expr(args[i].expr)
		if c.g.table.sym(args[i].typ).kind == .struct_ && !args[i].typ.is_ptr() {
			match args_size[i] {
				1...8 {
					c.mov_deref(.rax, .rax, ast.i64_type_idx)
					if args_size[i] != 8 {
						c.movabs(Amd64Register.rdx, i64((u64(1) << (args_size[i] * 8)) - 1))
						c.bitand_reg(.rax, .rdx)
					}
				}
				9...16 {
					c.add(.rax, 8)
					c.mov_deref(.rdx, .rax, ast.i64_type_idx)
					c.sub(.rax, 8)
					c.mov_deref(.rax, .rax, ast.i64_type_idx)
					if args_size[i] != 16 {
						c.movabs(Amd64Register.rbx, i64((u64(1) << ((args_size[i] - 8) * 8)) - 1))
						c.bitand_reg(.rdx, .rbx)
					}
				}
				else {}
			}
		}
		if is_floats[i] {
			if args_size[i] == 8 && node.expected_arg_types[i + args_offset] == ast.f32_type_idx {
				c.g.write32(0xc05a0ff2)
				c.g.println('cvtsd2ss xmm0, xmm0')
			}
			c.push_sse(.xmm0)
		} else {
			match args_size[i] {
				1...8 {
					c.push(.rax)
				}
				9...16 {
					c.push(.rdx)
					c.push(.rax)
				}
				else {
					c.add(.rax, args_size[i] - ((args_size[i] + 7) % 8 + 1))
					for _ in 0 .. (args_size[i] + 7) / 8 {
						c.mov_deref(.rdx, .rax, ast.i64_type_idx)
						c.push(.rdx)
						c.sub(.rax, 8)
					}
				}
			}
		}
	}
	for i in 0 .. reg_size {
		c.pop(native.fn_arg_registers[i])
	}
	for i in 0 .. ssereg_args.len {
		c.pop_sse(native.fn_arg_sse_registers[i])
	}
	c.mov(Amd64Register.rax, ssereg_args.len)
	if node.name in c.g.extern_symbols {
		c.g.extern_fn_calls[c.g.pos()] = node.name
		c.extern_call(int(addr))
	} else if addr == 0 {
		c.g.delay_fn_call(n)
		c.call(int(0))
	} else {
		c.call(int(addr))
	}
	c.g.println('call `${n}()`')

	if ts.kind in [.struct_, .multi_return] {
		match return_size {
			1...7 {
				c.mov_var_to_reg(Amd64Register.rdx, LocalVar{
					offset: return_pos
					typ: ast.i64_type_idx
				})
				c.movabs(Amd64Register.rcx, i64(
					0xffffffffffffffff - (u64(1) << (return_size * 8)) + 1))
				c.bitand_reg(.rdx, .rcx)
				c.bitor_reg(.rdx, .rax)
				c.mov_reg_to_var(LocalVar{ offset: return_pos, typ: ast.i64_type_idx },
					Amd64Register.rdx)
			}
			8 {
				c.mov_reg_to_var(LocalVar{ offset: return_pos, typ: ast.i64_type_idx },
					Amd64Register.rax)
			}
			9...15 {
				c.mov_reg_to_var(LocalVar{ offset: return_pos, typ: ast.i64_type_idx },
					Amd64Register.rax)
				c.mov_var_to_reg(Amd64Register.rax, LocalVar{
					offset: return_pos
					typ: ast.i64_type_idx
				},
					offset: 8
				)
				c.movabs(Amd64Register.rcx, i64(
					0xffffffffffffffff - (u64(1) << (return_size * 8)) + 1))
				c.bitand_reg(.rax, .rcx)
				c.bitor_reg(.rax, .rdx)
				c.mov_reg_to_var(LocalVar{ offset: return_pos, typ: ast.i64_type_idx },
					Amd64Register.rax,
					offset: 8
				)
			}
			16 {
				c.mov_reg_to_var(LocalVar{ offset: return_pos, typ: ast.i64_type_idx },
					Amd64Register.rax)
				c.mov_reg_to_var(LocalVar{ offset: return_pos, typ: ast.i64_type_idx },
					Amd64Register.rdx,
					offset: 8
				)
			}
			else {}
		}
		c.lea_var_to_reg(Amd64Register.rax, return_pos)
	}

	if !is_16bit_aligned {
		// dummy data
		c.pop(.rdi)
	}
	for _ in 0 .. stack_size {
		// args
		c.pop(.rdi)
	}
}

fn (mut c Amd64) call_builtin(name Builtin) i64 {
	call_addr := c.call(0)
	c.g.println('call builtin `${name}`')
	return call_addr
}

fn (mut c Amd64) for_in_stmt(node ast.ForInStmt) {
	if node.is_range {
		// for a in node.cond .. node.high {
		i := c.allocate_var(node.val_var, 8, 0) // iterator variable
		c.g.expr(node.cond)
		c.mov_reg_to_var(LocalVar{i, ast.i64_type_idx, node.val_var}, Amd64Register.rax) // i = node.cond // initial value
		start := c.g.pos() // label-begin:
		start_label := c.g.labels.new_label()
		c.mov_var_to_reg(Amd64Register.rbx, LocalVar{i, ast.i64_type_idx, node.val_var}) // rbx = iterator value
		c.g.expr(node.high) // final value
		c.cmp_reg(.rbx, .rax) // rbx = iterator, rax = max value
		jump_addr := c.cjmp(.jge) // leave loop if i is beyond end
		end_label := c.g.labels.new_label()
		c.g.labels.patches << LabelPatch{
			id: end_label
			pos: jump_addr
		}
		c.g.println('; jump to label ${end_label}')
		c.g.labels.branches << BranchLabel{
			name: node.label
			start: start_label
			end: end_label
		}
		c.g.stmts(node.stmts)
		c.g.labels.addrs[start_label] = c.g.pos()
		c.g.println('; label ${start_label}')
		c.inc_var(LocalVar{i, ast.i64_type_idx, node.val_var})
		c.g.labels.branches.pop()
		c.jmp_back(start)
		c.g.labels.addrs[end_label] = c.g.pos()
		c.g.println('; label ${end_label}')
		/*
		} else if node.kind == .array {
	} else if node.kind == .array_fixed {
	} else if node.kind == .map {
	} else if node.kind == .string {
	} else if node.kind == .struct_ {
	} else if it.kind in [.array, .string] || it.cond_type.has_flag(.variadic) {
	} else if it.kind == .map {
		*/
	} else {
		c.g.v_error('for-in statement is not yet implemented', node.pos)
	}
}

fn (mut c Amd64) gen_concat_expr(node ast.ConcatExpr) {
	typ := node.return_type
	ts := c.g.table.sym(typ)
	size := c.g.get_type_size(typ)
	// construct a struct variable contains the return value
	var := LocalVar{
		offset: c.g.allocate_by_type('', typ)
		typ: typ
	}
	// zero fill
	mut left := if size >= 16 {
		c.mov(Amd64Register.rax, 0)
		c.mov(Amd64Register.rcx, size / 8)
		c.lea_var_to_reg(Amd64Register.rdi, var.offset)
		c.g.write([u8(0xf3), 0x48, 0xab])
		c.g.println('rep stosq')
		size % 8
	} else {
		size
	}
	if left >= 8 {
		c.mov_int_to_var(var, 0, offset: size - left, typ: ast.i64_type_idx)
		left -= 8
	}
	if left >= 4 {
		c.mov_int_to_var(var, 0, offset: size - left, typ: ast.int_type_idx)
		left -= 4
	}
	if left >= 2 {
		c.mov_int_to_var(var, 0, offset: size - left, typ: ast.i16_type_idx)
		left -= 2
	}
	if left == 1 {
		c.mov_int_to_var(var, 0, offset: size - left, typ: ast.i8_type_idx)
	}
	// store exprs to the variable
	for i, expr in node.vals {
		offset := c.g.structs[typ.idx()].offsets[i]
		c.g.expr(expr)
		// TODO expr not on rax
		c.mov_reg_to_var(var, c.main_reg(),
			offset: offset
			typ: ts.mr_info().types[i]
		)
	}
	// store the multi return struct value
	c.lea_var_to_reg(c.main_reg(), var.offset)
}

// !!!!!
// TODO: this *must* be done better and platform independant
// !!!!!
fn (mut c Amd64) assign_right_expr(node ast.AssignStmt, i int, right ast.Expr, name string, ident ast.Ident) {
	match right {
		ast.IntegerLiteral {
			// c.allocate_var(name, 4, right.val.int())
			match node.op {
				.plus_assign {
					c.mov_var_to_reg(Amd64Register.rax, ident)
					c.add(.rax, right.val.int())
					c.mov_reg_to_var(ident, Amd64Register.rax)
				}
				.minus_assign {
					c.mov_var_to_reg(Amd64Register.rax, ident)
					c.sub(.rax, right.val.int())
					c.mov_reg_to_var(ident, Amd64Register.rax)
				}
				.mult_assign {
					c.mov_var_to_reg(Amd64Register.rax, ident)
					c.mov64(Amd64Register.rdx, right.val.int())
					c.mul_reg(.rax, .rdx)
					c.mov_reg_to_var(ident, Amd64Register.rax)
				}
				.div_assign {
					c.mov_var_to_reg(Amd64Register.rax, ident)
					c.mov64(Amd64Register.rdx, right.val.int())
					c.div_reg(.rax, .rdx)
					c.mov_reg_to_var(ident, Amd64Register.rax)
				}
				.decl_assign {
					c.allocate_var(name, 8, right.val.int())
				}
				.assign {
					// dump(c.g.typ(node.left_types[i]))
					match node.left[i] {
						ast.Ident {
							// lname := '${node.left[i]}'
							// c.g.expr(node.right[i])
							c.mov(Amd64Register.rax, right.val.int())
							c.mov_reg_to_var(ident, Amd64Register.rax)
						}
						else {
							tn := node.left[i].type_name()
							dump(node.left_types)
							c.g.n_error('unhandled assign type: ${tn}')
						}
					}
				}
				else {
					eprintln('ERROR 2')
					dump(node)
				}
			}
		}
		ast.Ident {
			// eprintln('identr') dump(node) dump(right)
			match node.op {
				.plus_assign {
					c.mov_var_to_reg(Amd64Register.rax, ident)
					c.mov_var_to_reg(Amd64Register.rbx, right as ast.Ident)
					c.add_reg(.rax, .rbx)
					c.mov_reg_to_var(ident, Amd64Register.rax)
				}
				.minus_assign {
					c.mov_var_to_reg(Amd64Register.rax, ident)
					c.mov_var_to_reg(Amd64Register.rbx, right as ast.Ident)
					c.sub_reg(.rax, .rbx)
					c.mov_reg_to_var(ident, Amd64Register.rax)
				}
				.div_assign {
					// this should be called when `a /= b` but it's not :?
					c.mov_var_to_reg(Amd64Register.rax, ident)
					c.mov_var_to_reg(Amd64Register.rbx, right as ast.Ident)
					c.div_reg(.rax, .rbx)
					c.mov_reg_to_var(ident, Amd64Register.rax)
				}
				.decl_assign {
					typ := node.left_types[i]
					if typ.is_number() || typ.is_any_kind_of_pointer() || typ.is_bool() {
						c.allocate_var(name, c.g.get_type_size(typ), 0)
					} else {
						ts := c.g.table.sym(typ)
						match ts.info {
							ast.Struct {
								c.g.allocate_by_type(name, typ)
							}
							else {}
						}
					}
					var_ := c.g.get_var_from_ident(ident)
					// TODO global var
					right_var := c.g.get_var_from_ident(right) as LocalVar
					match var_ {
						LocalVar {
							var := var_ as LocalVar
							if var.typ.is_number() || var.typ.is_any_kind_of_pointer()
								|| var.typ.is_bool() {
								c.mov_var_to_reg(Amd64Register.rax, right as ast.Ident)
								c.mov_reg_to_var(ident, Amd64Register.rax)
							} else {
								ts := c.g.table.sym(var.typ)
								match ts.info {
									ast.Struct {
										size := c.g.get_type_size(var.typ)
										if size >= 8 {
											for offset in 0 .. size / 8 {
												c.mov_var_to_reg(Amd64Register.rax, right_var,
													offset: offset * 8
													typ: ast.i64_type_idx
												)
												c.mov_reg_to_var(var, Amd64Register.rax,
													offset: offset * 8
													typ: ast.i64_type_idx
												)
											}
											if size % 8 != 0 {
												c.mov_var_to_reg(Amd64Register.rax, right_var,
													offset: size - 8
													typ: ast.i64_type_idx
												)
												c.mov_reg_to_var(var, Amd64Register.rax,
													offset: size - 8
													typ: ast.i64_type_idx
												)
											}
										} else {
											mut left_size := if size >= 4 {
												c.mov_var_to_reg(Amd64Register.rax, right_var,
													typ: ast.int_type_idx
												)
												c.mov_reg_to_var(var, Amd64Register.rax,
													typ: ast.int_type_idx
												)
												size - 4
											} else {
												size
											}
											if left_size >= 2 {
												c.mov_var_to_reg(Amd64Register.rax, right_var,
													offset: size - left_size
													typ: ast.i16_type_idx
												)
												c.mov_reg_to_var(var, Amd64Register.rax,
													offset: size - left_size
													typ: ast.i16_type_idx
												)
												left_size -= 2
											}
											if left_size == 1 {
												c.mov_var_to_reg(Amd64Register.rax, right_var,
													offset: size - left_size
													typ: ast.i8_type_idx
												)
												c.mov_reg_to_var(var, Amd64Register.rax,
													offset: size - left_size
													typ: ast.i8_type_idx
												)
											}
										}
									}
									else {
										c.g.n_error('Unsupported variable type')
									}
								}
							}
						}
						else {
							c.g.n_error('Unsupported variable kind')
						}
					}
				}
				.assign {
					var_ := c.g.get_var_from_ident(ident)
					// TODO global var
					right_var := c.g.get_var_from_ident(right) as LocalVar
					match var_ {
						LocalVar {
							var := var_ as LocalVar
							if var.typ.is_number() || var.typ.is_any_kind_of_pointer()
								|| var.typ.is_bool() {
								c.mov_var_to_reg(Amd64Register.rax, right as ast.Ident)
								c.mov_reg_to_var(ident, Amd64Register.rax)
							} else {
								ts := c.g.table.sym(var.typ)
								match ts.info {
									ast.Struct {
										size := c.g.get_type_size(var.typ)
										if size >= 8 {
											for offset in 0 .. size / 8 {
												c.mov_var_to_reg(Amd64Register.rax, right_var,
													offset: offset * 8
													typ: ast.i64_type_idx
												)
												c.mov_reg_to_var(var, Amd64Register.rax,
													offset: offset * 8
													typ: ast.i64_type_idx
												)
											}
											if size % 8 != 0 {
												c.mov_var_to_reg(Amd64Register.rax, right_var,
													offset: size - 8
													typ: ast.i64_type_idx
												)
												c.mov_reg_to_var(var, Amd64Register.rax,
													offset: size - 8
													typ: ast.i64_type_idx
												)
											}
										} else {
											mut left_size := if size >= 4 {
												c.mov_var_to_reg(Amd64Register.rax, right_var,
													typ: ast.int_type_idx
												)
												c.mov_reg_to_var(var, Amd64Register.rax,
													typ: ast.int_type_idx
												)
												size - 4
											} else {
												size
											}
											if left_size >= 2 {
												c.mov_var_to_reg(Amd64Register.rax, right_var,
													offset: size - left_size
													typ: ast.i16_type_idx
												)
												c.mov_reg_to_var(var, Amd64Register.rax,
													offset: size - left_size
													typ: ast.i16_type_idx
												)
												left_size -= 2
											}
											if left_size == 1 {
												c.mov_var_to_reg(Amd64Register.rax, right_var,
													offset: size - left_size
													typ: ast.i8_type_idx
												)
												c.mov_reg_to_var(var, Amd64Register.rax,
													offset: size - left_size
													typ: ast.i8_type_idx
												)
											}
										}
									}
									else {
										c.g.n_error('Unsupported variable type')
									}
								}
							}
						}
						else {
							c.g.n_error('Unsupported variable kind')
						}
					}
				}
				else {
					eprintln('TODO: unhandled assign ident case')
					dump(node)
				}
			}
			// a += b
		}
		ast.StructInit {
			match node.op {
				.decl_assign {
					c.g.allocate_by_type(name, right.typ)
					c.init_struct(ident, right)
				}
				else {
					c.g.n_error('Unexpected operator `${node.op}`')
				}
			}
		}
		ast.ArrayInit {
			// check if array is empty
			mut pos := c.g.allocate_array(name, 8, right.exprs.len)
			// allocate array of right.exprs.len vars
			for e in right.exprs {
				match e {
					ast.IntegerLiteral {
						c.mov(Amd64Register.rax, e.val.int())
						c.mov_reg_to_var(LocalVar{pos, ast.i64_type_idx, ''}, Amd64Register.rax)
						pos += 8
					}
					ast.StringLiteral {
						// TODO: use learel
						str := c.g.eval_str_lit_escape_codes(e)
						c.mov64(Amd64Register.rsi, c.g.allocate_string(str, 2, .abs64)) // for rsi its 2
						c.mov_reg_to_var(LocalVar{pos, ast.u64_type_idx, ''}, Amd64Register.rsi)
						pos += 8
					}
					else {
						dump(e)
						c.g.n_error('unhandled array init type')
					}
				}
			}
		}
		ast.IndexExpr {
			// a := arr[0]
			offset := c.allocate_var(name, c.g.get_sizeof_ident(ident), 0)
			if c.g.pref.is_verbose {
				println('infix assignment ${name} offset=${offset.hex2()}')
			}
			ie := right as ast.IndexExpr
			var := ie.left as ast.Ident
			dest := c.g.get_var_offset(var.name)
			if ie.index is ast.IntegerLiteral {
				index := ie.index
				ie_offset := index.val.int() * 8
				c.mov_var_to_reg(Amd64Register.rax, var,
					typ: ast.i64_type_idx
					offset: ie_offset
				)
			} else if ie.index is ast.Ident {
				ie_ident := ie.index
				c.lea_var_to_reg(Amd64Register.rax, dest)
				c.mov_var_to_reg(Amd64Register.rdi, ie_ident)
				c.add_reg(.rax, .rdi)
				c.mov_deref(.rax, .rax, ast.i64_type_idx)
			} else {
				c.g.n_error('only integers and idents can be used as indexes')
			}
			// TODO check if out of bounds access
			c.mov_reg_to_var(ident, Amd64Register.eax)
		}
		ast.StringLiteral {
			dest := c.allocate_var(name, 8, 0)
			ie := right as ast.StringLiteral
			str := c.g.eval_str_lit_escape_codes(ie)
			c.learel(Amd64Register.rsi, c.g.allocate_string(str, 3, .rel32))
			c.mov_reg_to_var(LocalVar{dest, ast.u64_type_idx, name}, Amd64Register.rsi)
		}
		ast.GoExpr {
			c.g.v_error('threads not implemented for the native backend', node.pos)
		}
		ast.TypeOf {
			c.g.gen_typeof_expr(right as ast.TypeOf, true)
			c.mov_reg(Amd64Register.rsi, Amd64Register.rax)
		}
		ast.AtExpr {
			dest := c.allocate_var(name, 8, 0)
			c.learel(Amd64Register.rsi, c.g.allocate_string(c.g.comptime_at(right), 3,
				.rel32))
			c.mov_reg_to_var(LocalVar{dest, ast.u64_type_idx, name}, Amd64Register.rsi)
		}
		else {
			if right is ast.IfExpr && (right as ast.IfExpr).is_comptime {
				if stmts := c.g.comptime_conditional(right) {
					for j, stmt in stmts {
						if j + 1 == stmts.len {
							if stmt is ast.ExprStmt {
								c.assign_right_expr(node, i, stmt.expr, name, ident)
							} else {
								c.g.n_error('last stmt must be expr')
							}
						} else {
							c.g.stmt(stmt)
						}
					}
				} else {
					c.g.n_error('missing value for assignment')
				}
				return
			}

			// dump(node)
			size := c.g.get_type_size(node.left_types[i])
			if size !in [1, 2, 4, 8] || node.op !in [.assign, .decl_assign] {
				c.g.v_error('unhandled assign_stmt expression: ${right.type_name()}',
					right.pos())
			}
			if node.op == .decl_assign {
				c.allocate_var(name, size, 0)
			}
			c.g.expr(right)
			var := c.g.get_var_from_ident(ident)
			if node.left_types[i].is_pure_float() {
				match var {
					LocalVar { c.mov_ssereg_to_var(var as LocalVar, .xmm0) }
					GlobalVar { c.mov_ssereg_to_var(var as GlobalVar, .xmm0) }
					// Amd64Register { c.g.mov_ssereg(var as Amd64Register, .xmm0) }
					else {}
				}
			} else {
				match var {
					LocalVar { c.mov_reg_to_var(var as LocalVar, Amd64Register.rax) }
					GlobalVar { c.mov_reg_to_var(var as GlobalVar, Amd64Register.rax) }
					Register { c.mov_reg(var as Amd64Register, Amd64Register.rax) }
				}
			}
		}
	}
}

fn (mut c Amd64) gen_type_promotion(from ast.Type, to ast.Type, option Amd64RegisterOption) {
	if !to.is_pure_float() {
		return
	}
	if c.g.is_register_type(from) {
		// integer -> float
		if c.g.get_type_size(from) == 8 && !from.is_signed() {
			label1 := c.g.labels.new_label()
			label2 := c.g.labels.new_label()
			c.test_reg(option.reg)
			addr1 := c.cjmp(.js)
			c.g.labels.patches << LabelPatch{
				id: label1
				pos: addr1
			}
			// if castee is in the range of i64
			prefix, inst := if c.g.get_type_size(to) == 4 {
				0xf3, 's'
			} else {
				0xf2, 'd'
			}
			c.g.write8(prefix)
			c.g.write8(0x48 + int(option.ssereg) / 8 * 4 + int(option.reg) / 8)
			c.g.write16(0x2a0f)
			c.g.write8(0xc0 + int(option.ssereg) % 8 * 8 + int(option.reg) % 8)
			c.g.println('cvtsi2s${inst} ${option.ssereg}, ${option.reg}')
			addr2 := c.jmp(0)
			c.g.labels.patches << LabelPatch{
				id: label2
				pos: addr2
			}
			c.g.labels.addrs[label1] = c.g.pos()
			// if castee has the leftmost bit
			c.mov_reg(Amd64Register.rbx, Amd64Register.rax)
			c.g.write([u8(0x48), 0xd1, 0xe8])
			c.g.println('shr rax')
			c.g.write([u8(0x83), 0xe3, 0x01])
			c.g.println('and ebx, 0x1')
			c.bitor_reg(.rax, .rbx)
			c.g.write8(prefix)
			c.g.write8(0x48 + int(option.ssereg) / 8 * 4 + int(option.reg) / 8)
			c.g.write16(0x2a0f)
			c.g.write8(0xc0 + int(option.ssereg) % 8 * 8 + int(option.reg) % 8)
			c.g.println('cvtsi2s${inst} ${option.ssereg}, ${option.reg}')
			c.add_sse(option.ssereg, option.ssereg, to)
			c.g.labels.addrs[label2] = c.g.pos()
		} else {
			prefix, inst := if c.g.get_type_size(to) == 4 {
				0xf3, 's'
			} else {
				0xf2, 'd'
			}
			c.g.write8(prefix)
			c.g.write8(0x48 + int(option.ssereg) / 8 * 4 + int(option.reg) / 8)
			c.g.write16(0x2a0f)
			c.g.write8(0xc0 + int(option.ssereg) % 8 * 8 + int(option.reg) % 8)
			c.g.println('cvtsi2s${inst} ${option.ssereg}, ${option.reg}')
		}
	} else {
		if from == ast.f32_type_idx && to != ast.f32_type_idx {
			// f32 -> f64
			c.g.write8(0xf3)
			if int(option.ssereg) >= int(Amd64SSERegister.xmm8) {
				c.g.write8(0x45)
			}
			c.g.write16(0x5a0f)
			c.g.write8(0xc0 + int(option.ssereg) % 8 * 9)
			c.g.println('cvtss2sd ${option.ssereg}, ${option.ssereg}')
		}
	}
}

fn (mut c Amd64) return_stmt(node ast.Return) {
	mut s := '?' //${node.exprs[0].val.str()}'
	if node.exprs.len == 1 {
		match node.exprs[0] {
			ast.StringLiteral {
				s = c.g.eval_str_lit_escape_codes(node.exprs[0] as ast.StringLiteral)
				c.g.expr(node.exprs[0])
				c.mov64(Amd64Register.rax, c.g.allocate_string(s, 2, .abs64))
			}
			else {
				c.g.expr(node.exprs[0])
			}
		}
		typ := if node.types.len > 1 { c.g.return_type } else { node.types[0] }
		if typ == ast.float_literal_type_idx && c.g.return_type == ast.f32_type_idx {
			if c.g.pref.arch == .amd64 {
				c.g.write32(0xc05a0ff2)
				c.g.println('cvtsd2ss xmm0, xmm0')
			}
		}
		// store the struct value
		if !c.g.is_register_type(typ) && !typ.is_pure_float() {
			ts := c.g.table.sym(typ)
			size := c.g.get_type_size(typ)
			if c.g.pref.arch == .amd64 {
				match ts.kind {
					.struct_, .multi_return {
						if size <= 8 {
							c.mov_deref(.rax, .rax, ast.i64_type_idx)
							if size != 8 {
								c.movabs(Amd64Register.rbx, i64((u64(1) << (size * 8)) - 1))
								c.bitand_reg(.rax, .rbx)
							}
						} else if size <= 16 {
							c.add(.rax, 8)
							c.mov_deref(.rdx, .rax, ast.i64_type_idx)
							c.sub(.rax, 8)
							c.mov_deref(.rax, .rax, ast.i64_type_idx)
							if size != 16 {
								c.movabs(Amd64Register.rbx, i64((u64(1) << ((size - 8) * 8)) - 1))
								c.bitand_reg(.rdx, .rbx)
							}
						} else {
							offset := c.g.get_var_offset('_return_val_addr')
							c.mov_var_to_reg(Amd64Register.rdx, LocalVar{
								offset: offset
								typ: ast.i64_type_idx
							})
							for i in 0 .. size / 8 {
								c.mov_deref(.rcx, .rax, ast.i64_type_idx)
								c.mov_store(.rdx, .rcx, ._64)
								if i != size / 8 - 1 {
									c.add(.rax, 8)
									c.add(.rdx, 8)
								}
							}
							if size % 8 != 0 {
								c.add(.rax, size % 8)
								c.add(.rdx, size % 8)
								c.mov_deref(.rcx, .rax, ast.i64_type_idx)
								c.mov_store(.rdx, .rcx, ._64)
							}
							c.mov_var_to_reg(c.main_reg(), LocalVar{
								offset: offset
								typ: ast.i64_type_idx
							})
						}
					}
					else {}
				}
			}
		}
	} else if node.exprs.len > 1 {
		typ := c.g.return_type
		ts := c.g.table.sym(typ)
		size := c.g.get_type_size(typ)
		// construct a struct variable contains the return value
		var := LocalVar{
			offset: c.g.allocate_by_type('', typ)
			typ: typ
		}
		// zero fill
		mut left := if size >= 16 {
			c.mov(Amd64Register.rax, 0)
			c.mov(Amd64Register.rcx, size / 8)
			c.lea_var_to_reg(Amd64Register.rdi, var.offset)
			c.g.write([u8(0xf3), 0x48, 0xab])
			c.g.println('rep stosq')
			size % 8
		} else {
			size
		}
		if left >= 8 {
			c.mov_int_to_var(var, 0, offset: size - left, typ: ast.i64_type_idx)
			left -= 8
		}
		if left >= 4 {
			c.mov_int_to_var(var, 0, offset: size - left, typ: ast.int_type_idx)
			left -= 4
		}
		if left >= 2 {
			c.mov_int_to_var(var, 0, offset: size - left, typ: ast.i16_type_idx)
			left -= 2
		}
		if left == 1 {
			c.mov_int_to_var(var, 0, offset: size - left, typ: ast.i8_type_idx)
		}
		// store exprs to the variable
		for i, expr in node.exprs {
			offset := c.g.structs[typ.idx()].offsets[i]
			c.g.expr(expr)
			// TODO expr not on rax
			c.mov_reg_to_var(var, Amd64Register.rax, offset: offset, typ: ts.mr_info().types[i])
		}
		// store the multi return struct value
		c.lea_var_to_reg(Amd64Register.rax, var.offset)
		if c.g.pref.arch == .amd64 {
			if size <= 8 {
				c.mov_deref(.rax, .rax, ast.i64_type_idx)
				if size != 8 {
					c.movabs(Amd64Register.rbx, i64((u64(1) << (size * 8)) - 1))
					c.bitand_reg(.rax, .rbx)
				}
			} else if size <= 16 {
				c.add(.rax, 8)
				c.mov_deref(.rdx, .rax, ast.i64_type_idx)
				c.sub(.rax, 8)
				c.mov_deref(.rax, .rax, ast.i64_type_idx)
				if size != 16 {
					c.movabs(Amd64Register.rbx, i64((u64(1) << ((size - 8) * 8)) - 1))
					c.bitand_reg(.rdx, .rbx)
				}
			} else {
				offset := c.g.get_var_offset('_return_val_addr')
				c.mov_var_to_reg(Amd64Register.rdx, LocalVar{
					offset: offset
					typ: ast.i64_type_idx
				})
				for i in 0 .. size / 8 {
					c.mov_deref(.rcx, .rax, ast.i64_type_idx)
					c.mov_store(.rdx, .rcx, ._64)
					if i != size / 8 - 1 {
						c.add(.rax, 8)
						c.add(.rdx, 8)
					}
				}
				if size % 8 != 0 {
					c.add(.rax, size % 8)
					c.add(.rdx, size % 8)
					c.mov_deref(.rcx, .rax, ast.i64_type_idx)
					c.mov_store(.rdx, .rcx, ._64)
				}
				c.mov_var_to_reg(c.main_reg(), LocalVar{
					offset: offset
					typ: ast.i64_type_idx
				})
			}
		}
	}

	// jump to return label
	label := 0
	pos := c.jmp(0)
	c.g.labels.patches << LabelPatch{
		id: label
		pos: pos
	}
	c.g.println('; jump to label ${label}')
}

fn (mut c Amd64) multi_assign_stmt(node ast.AssignStmt) {
	multi_return := c.g.get_multi_return(node.right_types)
	if node.has_cross_var {
		// `a, b = b, a`
		// construct a struct variable contains the return value
		size := multi_return.size
		align := multi_return.align
		padding := (align - c.g.stack_var_pos % align) % align
		c.g.stack_var_pos += size + padding
		var := LocalVar{
			offset: c.g.stack_var_pos
		}
		// zero fill
		mut left := if size >= 16 {
			c.mov(Amd64Register.rax, 0)
			c.mov(Amd64Register.rcx, size / 8)
			c.lea_var_to_reg(Amd64Register.rdi, var.offset)
			c.g.write([u8(0xf3), 0x48, 0xab])
			c.g.println('rep stosq')
			size % 8
		} else {
			size
		}
		if left >= 8 {
			c.mov_int_to_var(var, 0, offset: size - left, typ: ast.i64_type_idx)
			left -= 8
		}
		if left >= 4 {
			c.mov_int_to_var(var, 0, offset: size - left, typ: ast.int_type_idx)
			left -= 4
		}
		if left >= 2 {
			c.mov_int_to_var(var, 0, offset: size - left, typ: ast.i16_type_idx)
			left -= 2
		}
		if left == 1 {
			c.mov_int_to_var(var, 0, offset: size - left, typ: ast.i8_type_idx)
		}
		// store exprs to the variable
		for i, expr in node.right {
			offset := multi_return.offsets[i]
			c.g.expr(expr)
			// TODO expr not on rax
			c.mov_reg_to_var(var, Amd64Register.rax, offset: offset, typ: node.right_types[i])
		}
		// store the multi return struct value
		c.lea_var_to_reg(Amd64Register.rax, var.offset)
	} else {
		c.g.expr(node.right[0])
	}
	c.mov_reg(Amd64Register.rdx, Amd64Register.rax)

	mut current_offset := 0
	for i, offset in multi_return.offsets {
		if node.left[i] is ast.Ident {
			name := (node.left[i] as ast.Ident).name
			if name == '_' {
				continue
			}
			if node.op == .decl_assign {
				c.g.allocate_by_type(name, node.left_types[i])
			}
		}
		if offset != current_offset {
			c.add(.rdx, offset - current_offset)
			current_offset = offset
		}
		c.gen_left_value(node.left[i])
		left_type := node.left_types[i]
		right_type := node.right_types[i]
		if c.g.is_register_type(right_type) {
			c.mov_deref(.rcx, .rdx, right_type)
		} else if node.right_types[i].is_pure_float() {
			c.mov_deref_sse(.xmm0, .rdx, right_type)
		}
		c.gen_type_promotion(right_type, left_type, reg: .rcx)
		if c.g.is_register_type(left_type) {
			match node.op {
				.assign, .decl_assign {
					c.mov_store(.rax, .rcx, match c.g.get_type_size(left_type) {
						1 { ._8 }
						2 { ._16 }
						4 { ._32 }
						else { ._64 }
					})
				}
				else {
					c.g.n_error('Unsupported assign instruction')
				}
			}
		} else if left_type.is_pure_float() {
			is_f32 := left_type == ast.f32_type_idx
			if node.op !in [.assign, .decl_assign] {
				c.mov_ssereg(.xmm1, .xmm0)
				if is_f32 {
					c.g.write32(0x00100ff3)
					c.g.println('movss xmm0, [rax]')
				} else {
					c.g.write32(0x00100ff2)
					c.g.println('movsd xmm0, [rax]')
				}
			}
			match node.op {
				.plus_assign {
					c.add_sse(.xmm0, .xmm1, left_type)
				}
				.minus_assign {
					c.sub_sse(.xmm0, .xmm1, left_type)
				}
				.mult_assign {
					c.mul_sse(.xmm0, .xmm1, left_type)
				}
				.div_assign {
					c.div_sse(.xmm0, .xmm1, left_type)
				}
				else {}
			}
			if is_f32 {
				c.g.write32(0x00110ff3)
				c.g.println('movss [rax], xmm0')
			} else {
				c.g.write32(0x00110ff2)
				c.g.println('movsd [rax], xmm0')
			}
		} else {
			c.g.n_error('multi return for struct is not supported yet')
		}
	}
}

fn (mut c Amd64) assign_stmt(node ast.AssignStmt) {
	// `a, b := foo()`
	// `a, b := if cond { 1, 2 } else { 3, 4 }`
	// `a, b = b, a`
	if (node.left.len > 1 && node.right.len == 1) || node.has_cross_var {
		c.multi_assign_stmt(node)
		return
	}
	// `a := 1` | `a,b := 1,2`
	for i, left in node.left {
		right := node.right[i]
		typ := node.left_types[i]
		// this branch would be removed, but left for compatibility
		if left is ast.Ident && !typ.is_pure_float() {
			ident := left as ast.Ident
			c.assign_right_expr(node, i, right, ident.name, ident)
			continue
		}
		if left is ast.Ident && node.op == .decl_assign {
			c.g.allocate_by_type((left as ast.Ident).name, typ)
		}
		c.gen_left_value(left)
		c.push(Amd64Register.rax)
		c.g.expr(right)
		c.pop(.rdx)
		c.gen_type_promotion(node.right_types[0], typ)
		if c.g.is_register_type(typ) {
			match node.op {
				.decl_assign, .assign {
					c.mov_store(.rdx, .rax, match c.g.get_type_size(typ) {
						1 { ._8 }
						2 { ._16 }
						4 { ._32 }
						else { ._64 }
					})
				}
				else {
					c.g.n_error('Unsupported assign instruction')
				}
			}
		} else if typ.is_pure_float() {
			is_f32 := typ == ast.f32_type_idx
			if node.op !in [.assign, .decl_assign] {
				c.mov_ssereg(.xmm1, .xmm0)
				if is_f32 {
					c.g.write32(0x02100ff3)
					c.g.println('movss xmm0, [rdx]')
				} else {
					c.g.write32(0x02100ff2)
					c.g.println('movsd xmm0, [rdx]')
				}
			}
			match node.op {
				.plus_assign {
					c.add_sse(.xmm0, .xmm1, typ)
				}
				.minus_assign {
					c.sub_sse(.xmm0, .xmm1, typ)
				}
				.mult_assign {
					c.mul_sse(.xmm0, .xmm1, typ)
				}
				.div_assign {
					c.div_sse(.xmm0, .xmm1, typ)
				}
				else {}
			}
			if is_f32 {
				c.g.write32(0x02110ff3)
				c.g.println('movss [rdx], xmm0')
			} else {
				c.g.write32(0x02110ff2)
				c.g.println('movsd [rdx], xmm0')
			}
		} else {
			if node.op !in [.assign, .decl_assign] {
				c.g.n_error('Unsupported assign instruction')
			}
			ts := c.g.table.sym(typ)
			match ts.kind {
				.struct_ {
					size := c.g.get_type_size(typ)
					if size >= 8 {
						for j in 0 .. size / 8 {
							c.mov_deref(.rcx, .rdx, ast.u64_type_idx)
							c.mov_store(.rax, .rcx, ._64)
							offset := if j == size / 8 - 1 && size % 8 != 0 {
								size % 8
							} else {
								8
							}
							c.add(.rax, offset)
							c.add(.rdx, offset)
						}
						if size % 8 != 0 {
							c.mov_deref(.rcx, .rdx, ast.u64_type_idx)
							c.mov_store(.rax, .rcx, ._64)
						}
					} else {
						mut left_size := if size >= 4 {
							c.mov_deref(.rcx, .rdx, ast.u32_type_idx)
							c.mov_store(.rax, .rcx, ._32)
							if size > 4 {
								c.add(.rax, 4)
								c.add(.rdx, 4)
							}
							size - 4
						} else {
							size
						}
						if left_size >= 2 {
							c.mov_deref(.rcx, .rdx, ast.u16_type_idx)
							c.mov_store(.rax, .rcx, ._16)
							if left_size > 2 {
								c.add(.rax, 2)
								c.add(.rdx, 2)
							}
							left_size -= 2
						}
						if left_size == 1 {
							c.mov_deref(.rcx, .rdx, ast.u8_type_idx)
							c.mov_store(.rax, .rcx, ._8)
						}
					}
				}
				.enum_ {
					c.mov_store(.rdx, .rax, ._32)
				}
				else {}
			}
		}
	}
}

fn (mut c Amd64) cset_op(op token.Kind) {
	match op {
		.gt {
			c.cset(.g)
		}
		.lt {
			c.cset(.l)
		}
		.ne {
			c.cset(.ne)
		}
		.eq {
			c.cset(.e)
		}
		.ge {
			c.cset(.ge)
		}
		.le {
			c.cset(.le)
		}
		else {
			c.cset(.ne)
		}
	}
}

fn (mut c Amd64) gen_left_value(node ast.Expr) {
	match node {
		ast.Ident {
			offset := c.g.get_var_offset(node.name)
			c.lea_var_to_reg(Amd64Register.rax, offset)
		}
		ast.SelectorExpr {
			c.g.expr(node.expr)
			offset := c.g.get_field_offset(node.expr_type, node.field_name)
			if offset != 0 {
				c.add(.rax, offset)
			}
		}
		ast.IndexExpr {} // TODO
		ast.PrefixExpr {
			if node.op != .mul {
				c.g.n_error('Unsupported left value')
			}
			c.g.expr(node.right)
		}
		else {
			c.g.n_error('Unsupported left value')
		}
	}
}

fn (mut c Amd64) prefix_expr(node ast.PrefixExpr) {
	match node.op {
		.minus {
			c.g.expr(node.right)
			if node.right_type.is_pure_float() {
				c.mov_ssereg_to_reg(.rax, .xmm0, node.right_type)
				if node.right_type == ast.f32_type_idx {
					c.mov32(.rdx, int(u32(0x80000000)))
				} else {
					c.movabs(Amd64Register.rdx, i64(u64(0x8000000000000000)))
				}
				c.bitxor_reg(.rax, .rdx)
				c.mov_reg_to_ssereg(.xmm0, .rax, node.right_type)
			} else {
				c.neg(.rax)
			}
		}
		.amp {
			c.gen_left_value(node.right)
		}
		.mul {
			c.g.expr(node.right)
			c.mov_deref(.rax, .rax, node.right_type.deref())
		}
		.not {
			c.g.expr(node.right)
			c.cmp_zero(Amd64Register.rax)
			// TODO mov_extend_reg
			c.mov64(Amd64Register.rax, 0)
			c.cset(.e)
		}
		.bit_not {
			c.g.expr(node.right)
			c.bitnot_reg(.rax)
		}
		else {}
	}
}

fn (mut c Amd64) infix_expr(node ast.InfixExpr) {
	if node.op in [.logical_or, .and] {
		c.g.expr(node.left)
		label := c.g.labels.new_label()
		c.cmp_zero(Amd64Register.rax)
		jump_addr := c.cjmp(if node.op == .logical_or { .jne } else { .je })
		c.g.labels.patches << LabelPatch{
			id: label
			pos: jump_addr
		}
		c.g.expr(node.right)
		c.g.labels.addrs[label] = c.g.pos()
		return
	} else {
		c.g.expr(node.right)
		if node.left_type.is_pure_float() {
			typ := node.left_type
			// optimize for ast.Ident
			match node.left {
				ast.Ident {
					c.mov_ssereg(.xmm1, .xmm0)
					c.mov_var_to_ssereg(.xmm0, node.left as ast.Ident)
				}
				else {
					c.push_sse(.xmm0)
					c.g.expr(node.left)
					c.pop_sse(.xmm1)
				}
			}
			match node.op {
				.eq, .ne {
					c.g.write32(0xc1c20ff3)
					c.g.write8(if node.op == .eq { 0x00 } else { 0x04 })
					inst := if node.op == .eq { 'cmpeqss' } else { 'cmpneqss' }
					c.g.println('${inst} xmm0, xmm1')
					c.mov_ssereg_to_reg(.rax, .xmm0, ast.f32_type_idx)
					c.g.write([u8(0x83), 0xe0, 0x01])
					c.g.println('and eax, 0x1')
				}
				.gt, .lt, .ge, .le {
					c.cmp_sse(.xmm0, .xmm1, typ)
					// TODO mov_extend_reg
					c.mov64(Amd64Register.rax, 0)
					c.cset(match node.op {
						.gt { .a }
						.lt { .b }
						.ge { .ae }
						else { .be }
					})
				}
				.plus {
					c.add_sse(.xmm0, .xmm1, typ)
				}
				.minus {
					c.sub_sse(.xmm0, .xmm1, typ)
				}
				.mul {
					c.mul_sse(.xmm0, .xmm1, typ)
				}
				.div {
					c.div_sse(.xmm0, .xmm1, typ)
				}
				else {
					c.g.n_error('`${node.op}` expression is not supported right now')
				}
			}
			return
		}
		// optimize for ast.Ident
		match node.left {
			ast.Ident {
				c.mov_reg(match node.op {
					.left_shift, .right_shift, .unsigned_right_shift, .div, .mod { Amd64Register.rcx }
					else { Amd64Register.rdx }
				}, Amd64Register.rax)
				c.mov_var_to_reg(Amd64Register.rax, node.left as ast.Ident)
			}
			else {
				c.push(Amd64Register.rax)
				c.g.expr(node.left)
				c.pop(match node.op {
					.left_shift, .right_shift, .unsigned_right_shift, .div, .mod { .rcx }
					else { .rdx }
				})
			}
		}
		if node.left_type !in ast.integer_type_idxs && node.left_type != ast.bool_type_idx
			&& c.g.table.sym(node.left_type).info !is ast.Enum && !node.left_type.is_ptr()
			&& !node.left_type.is_voidptr() {
			c.g.n_error('unsupported type for `${node.op}`: ${node.left_type}')
		}
		// left: rax, right: rdx
		match node.op {
			.eq, .ne, .gt, .lt, .ge, .le {
				c.cmp_reg(.rax, .rdx)
				// TODO mov_extend_reg
				c.mov64(Amd64Register.rax, 0)
				c.cset_op(node.op)
			}
			.plus {
				c.add_reg(.rax, .rdx)
			}
			.minus {
				c.sub_reg(.rax, .rdx)
			}
			.mul {
				c.g.write32(0xc2af0f48)
				c.g.println('imul rax, rdx')
			}
			.div {
				if node.left_type in ast.unsigned_integer_type_idxs {
					c.g.write8(0xba)
					c.g.write32(0)
					c.g.println('mov edx, 0')
					c.g.write([u8(0x48), 0xf7, 0xf1])
					c.g.println('div rcx')
				} else {
					c.g.write16(0x9948)
					c.g.println('cqo')
					c.g.write([u8(0x48), 0xf7, 0xf9])
					c.g.println('idiv rcx')
				}
			}
			.mod {
				if node.left_type in ast.unsigned_integer_type_idxs {
					c.g.write8(0xba)
					c.g.write32(0)
					c.g.println('mov edx, 0')
					c.g.write([u8(0x48), 0xf7, 0xf1])
					c.g.println('div rcx')
				} else {
					c.g.write16(0x9948)
					c.g.println('cqo')
					c.g.write([u8(0x48), 0xf7, 0xf9])
					c.g.println('idiv rcx')
				}
				c.mov_reg(Amd64Register.rax, Amd64Register.rdx)
			}
			.amp {
				c.bitand_reg(.rax, .rdx)
			}
			.pipe {
				c.bitor_reg(.rax, .rdx)
			}
			.xor {
				c.bitxor_reg(.rax, .rdx)
			}
			.left_shift {
				c.shl_reg(.rax, .rcx)
			}
			.right_shift {
				c.sar_reg(.rax, .rcx)
			}
			.unsigned_right_shift {
				c.shr_reg(.rax, .rcx)
			}
			else {
				c.g.n_error('`${node.op}` expression is not supported right now')
			}
		}
	}
}

fn (mut c Amd64) trap() {
	// funnily works on x86 and arm64
	if c.g.pref.arch == .arm64 {
		c.g.write32(0xcccccccc)
	} else {
		c.g.write8(0xcc)
	}
	c.g.println('trap')
}

fn (mut c Amd64) gen_asm_stmt(asm_node ast.AsmStmt) {
	// inline assembly using vasm
	c.g.println('// asm inline')
	mut reg := 0
	mut imm := 0
	mut regname := ''
	// dump(asm_node)
	for t in asm_node.templates {
		mut line := t.name
		mut comma := false
		for a in t.args {
			if comma {
				line += ', '
			} else {
				comma = true
			}
			match a {
				ast.AsmRegister {
					regname = a.name
					reg = native.amd64_cpuregs.index(regname)
					line += a.typ.str()
				}
				ast.IntegerLiteral {
					line += a.val
					imm = a.val.int()
				}
				ast.BoolLiteral {
					line += a.val.str()
					imm = if a.val { 1 } else { 0 }
				}
				ast.CharLiteral {
					line += a.val.str()
					imm = a.val.int()
				}
				/*
				ast.AsmAddressing {
				}
				ast.AsmAlias {
				}
				ast.AsmDisp {
				}
				*/
				ast.FloatLiteral {
					c.g.v_error('floating point arithmetic is not yet implemented for the native backend',
						asm_node.pos)
				}
				string {
					// XXX
					c.g.v_error('no strings allowed in this context', asm_node.pos)
				}
				else {
					c.g.v_error('unsupported instruction argument argument', asm_node.pos)
				}
			}
		}
		c.g.println(': ${line}')
		match t.name {
			'nop' {
				c.g.write8(u8(0x90))
				c.g.println('nop')
			}
			'syscall' {
				c.g.write8(u8(0x0f))
				c.g.write8(u8(0x05))
				c.g.println('syscall')
			}
			'ret' {
				c.g.write8(u8(0xc3))
				c.g.println('ret')
			}
			'int3' {
				c.g.write8(u8(0xcc))
				c.g.write8(u8(imm))
				c.g.println('int3')
			}
			'sti' {
				c.g.write8(u8(0xfb))
				c.g.println('sti')
			}
			'cli' {
				c.g.write8(u8(0xfa))
				c.g.println('cli')
			}
			'int' {
				c.g.write8(u8(0xcd))
				c.g.write8(u8(imm))
				c.g.println('int')
			}
			'cpuid' {
				c.g.write8(u8(0x0f))
				c.g.write8(u8(0xa2))
				c.g.println('cpuid')
			}
			'mov' {
				c.g.write8(u8(0xb8 + reg))
				c.g.write8(byt(imm, 0))
				c.g.write8(byt(imm, 1))
				c.g.write8(byt(imm, 2))
				c.g.write8(byt(imm, 3))
				c.g.println('mov ${reg}, ${imm}')
			}
			else {
				c.g.v_error('unsupported instruction ${t.name}', asm_node.pos)
			}
		}
	}
}

fn (mut c Amd64) gen_selector_expr(expr ast.SelectorExpr) {
	c.g.expr(expr.expr)
	offset := c.g.get_field_offset(expr.expr_type, expr.field_name)
	c.add(.rax, offset)
	c.mov_deref(.rax, .rax, expr.typ)
}

fn (mut c Amd64) gen_assert(assert_node ast.AssertStmt) {
	mut cjmp_addr := 0
	ane := assert_node.expr
	label := c.g.labels.new_label()
	cjmp_addr = c.g.condition(ane, true)
	c.g.labels.patches << LabelPatch{
		id: label
		pos: cjmp_addr
	}
	c.g.println('; jump to label ${label}')
	c.g.expr(assert_node.expr)
	c.trap()
	c.g.labels.addrs[label] = c.g.pos()
	c.g.println('; label ${label}')
}

fn (mut c Amd64) cjmp_notop(op token.Kind) int {
	return match op {
		.gt {
			c.cjmp(.jle)
		}
		.lt {
			c.cjmp(.jge)
		}
		.ne {
			c.cjmp(.je)
		}
		.eq {
			c.cjmp(.jne)
		}
		else {
			c.cjmp(.je)
		}
	}
}

fn (mut c Amd64) cjmp_op(op token.Kind) int {
	return match op {
		.gt {
			c.cjmp(.jg)
		}
		.lt {
			c.cjmp(.jl)
		}
		.ne {
			c.cjmp(.jne)
		}
		.eq {
			c.cjmp(.je)
		}
		else {
			c.cjmp(.jne)
		}
	}
}

fn (mut c Amd64) infloop() {
	c.g.write8(u8(0xeb))
	c.g.write8(u8(0xfe))
	c.g.println('jmp $$')
}

fn (mut c Amd64) fn_decl(node ast.FnDecl) {
	c.push(.rbp)
	c.mov_reg(Amd64Register.rbp, Amd64Register.rsp)
	local_alloc_pos := c.g.pos()
	c.sub(.rsp, 0)

	// Copy values from registers to local vars (calling convention)
	mut reg_args := []int{}
	mut ssereg_args := []int{}
	mut stack_args := []int{}
	mut params := []ast.Param{cap: node.params.len + 2}

	// The first parameter is an address of returned struct if size > 16
	ts := c.g.table.sym(node.return_type)
	return_size := c.g.get_type_size(node.return_type)
	if ts.kind in [.struct_, .multi_return] {
		if return_size > 16 {
			params << ast.Param{
				name: '_return_val_addr'
				typ: ast.voidptr_type_idx
			}
		}
	}

	params << node.params

	args_size := params.map(c.g.get_type_size(it.typ))
	is_floats := params.map(it.typ.is_pure_float())

	mut reg_left := 6
	mut ssereg_left := 8
	for i, size in args_size {
		if is_floats[i] && ssereg_left > 0 {
			ssereg_args << i
			ssereg_left--
			continue
		}
		if reg_left > 0 {
			if size <= 8 {
				reg_args << i
				reg_left--
				continue
			} else if size <= 16 && reg_left > 1 {
				reg_args << i
				reg_left -= 2
				continue
			}
		}
		stack_args << i
	}

	// define and copy args on register
	mut reg_idx := 0
	for i in reg_args {
		name := params[i].name
		c.g.stack_var_pos += (8 - args_size[i] % 8) % 8
		offset := c.g.allocate_by_type(name, params[i].typ)
		// copy
		c.mov_reg_to_var(LocalVar{ offset: offset, typ: ast.i64_type_idx, name: name },
			native.fn_arg_registers[reg_idx])
		reg_idx++
		if args_size[i] > 8 {
			c.mov_reg_to_var(LocalVar{ offset: offset, typ: ast.i64_type_idx, name: name },
				native.fn_arg_registers[reg_idx],
				offset: 8
			)
			reg_idx++
		}
	}
	// define and copy args on sse register
	for idx, i in ssereg_args {
		name := params[i].name
		offset := c.g.allocate_by_type(name, params[i].typ)
		// copy
		c.mov_ssereg_to_var(LocalVar{ offset: offset, typ: params[i].typ }, native.fn_arg_sse_registers[idx])
	}
	// define args on stack
	mut offset := -2
	for i in stack_args {
		name := params[i].name
		c.g.var_offset[name] = offset * 8
		c.g.var_alloc_size[name] = args_size[i]
		offset -= (args_size[i] + 7) / 8
	}
	// define defer vars
	for i in 0 .. node.defer_stmts.len {
		name := '_defer${i}'
		c.allocate_var(name, 8, 0)
	}
	// body
	c.g.stmts(node.stmts)
	// 16 bytes align
	c.g.stack_var_pos += 23
	c.g.stack_var_pos /= 16
	c.g.stack_var_pos *= 16
	c.g.println('; stack frame size: ${c.g.stack_var_pos}')
	c.g.write32_at(local_alloc_pos + 3, c.g.stack_var_pos)
	is_main := node.name == 'main.main'
	if is_main && c.g.pref.os != .linux {
		// println('end of main: gen exit')
		zero := ast.IntegerLiteral{}
		c.gen_exit(zero)
		c.ret()
		return
	}
	c.g.labels.addrs[0] = c.g.pos()
	c.leave()
}

pub fn (mut c Amd64) builtin_decl(builtin BuiltinFn) {
	c.push(Amd64Register.rbp)
	c.mov_reg(Amd64Register.rbp, Amd64Register.rsp)
	local_alloc_pos := c.g.pos()
	c.sub(.rsp, 0)

	builtin.body(builtin, mut c.g)
	// 16 bytes align
	c.g.stack_var_pos += 7
	c.g.stack_var_pos /= 16
	c.g.stack_var_pos *= 16
	c.g.println('; stack frame size: ${c.g.stack_var_pos}')
	c.g.write32_at(local_alloc_pos + 3, c.g.stack_var_pos)

	c.g.labels.addrs[0] = c.g.pos()
	c.leave()
}

pub fn (mut c Amd64) allocate_var_two_step(name string, size int, initial_val int) int {
	// TODO: replace int by i64 or bigger
	c.allocate_var(name, size - 8, 0)
	return c.allocate_var(name, 8, initial_val)
}

pub fn (mut c Amd64) allocate_var(name string, size int, initial_val int) int {
	if size > 8 {
		return c.allocate_var_two_step(name, size, initial_val)
	}

	padding := (size - c.g.stack_var_pos % size) % size
	n := c.g.stack_var_pos + size + padding
	is_far_var := n > 0x80 || n < -0x7f
	far_var_offset := if is_far_var { 0x40 } else { 0 }
	// `a := 3`  => `mov DWORD [rbp-0x4],0x3`
	match size {
		1 {
			// BYTE
			c.g.write8(0xc6)
			c.g.write8(0x45 + far_var_offset)
		}
		2 {
			// WORD
			c.g.write8(0x66)
			c.g.write8(0xc7)
			c.g.write8(0x45 + far_var_offset)
		}
		4 {
			// DWORD
			c.g.write8(0xc7)
			c.g.write8(0x45 + far_var_offset)
		}
		8 {
			// QWORD
			c.g.write8(0x48)
			c.g.write8(0xc7)
			c.g.write8(0x45 + far_var_offset)
		}
		else {
			c.g.n_error('allocate_var: bad size ${size}')
		}
	}
	// Generate N in `[rbp-N]`
	if is_far_var {
		c.g.write32(int((0xffffffff - i64(n) + 1) % 0x100000000))
	} else {
		c.g.write8((0xff - n + 1) % 0x100)
	}
	c.g.stack_var_pos += size + padding
	c.g.var_offset[name] = c.g.stack_var_pos
	c.g.var_alloc_size[name] = size

	// Generate the value assigned to the variable
	match size {
		1 {
			c.g.write8(initial_val)
		}
		2 {
			c.g.write16(initial_val)
		}
		4 {
			c.g.write32(initial_val)
		}
		8 {
			c.g.write32(initial_val) // fixme: 64-bit segfaulting
		}
		else {
			c.g.n_error('allocate_var: bad size ${size}')
		}
	}

	// println('allocate_var(size=$size, initial_val=$initial_val)')
	c.g.println('mov [rbp-${n.hex2()}], ${initial_val} ; Allocate var `${name}`')
	return c.g.stack_var_pos
}

fn (mut c Amd64) init_struct(var Var, init ast.StructInit) {
	match var {
		ast.Ident {
			var_object := c.g.get_var_from_ident(var)
			match var_object {
				LocalVar {
					c.init_struct(var_object as LocalVar, init)
				}
				GlobalVar {
					c.init_struct(var_object as GlobalVar, init)
				}
				Register {
					// TODO
					// c.g.cmp()
				}
			}
		}
		LocalVar {
			size := c.g.get_type_size(var.typ)

			// zero fill
			mut left := if size >= 16 {
				c.mov(Amd64Register.rax, 0)
				c.mov(Amd64Register.rcx, size / 8)
				c.lea_var_to_reg(Amd64Register.rdi, var.offset)
				c.g.write([u8(0xf3), 0x48, 0xab])
				c.g.println('rep stosq')
				size % 8
			} else {
				size
			}
			if left >= 8 {
				c.mov_int_to_var(var, 0, offset: size - left, typ: ast.i64_type_idx)
				left -= 8
			}
			if left >= 4 {
				c.mov_int_to_var(var, 0, offset: size - left, typ: ast.int_type_idx)
				left -= 4
			}
			if left >= 2 {
				c.mov_int_to_var(var, 0, offset: size - left, typ: ast.i16_type_idx)
				left -= 2
			}
			if left == 1 {
				c.mov_int_to_var(var, 0, offset: size - left, typ: ast.i8_type_idx)
			}

			ts := c.g.table.sym(var.typ)
			match ts.info {
				ast.Struct {
					for i, f in ts.info.fields {
						if f.has_default_expr && !init.init_fields.map(it.name).contains(f.name) {
							offset := c.g.structs[var.typ.idx()].offsets[i]
							c.g.expr(f.default_expr)
							// TODO expr not on rax
							c.mov_reg_to_var(var, Amd64Register.rax, offset: offset, typ: f.typ)
						}
					}
				}
				else {}
			}
			for f in init.init_fields {
				field := ts.find_field(f.name) or {
					c.g.n_error('Could not find field `${f.name}` on init')
				}
				offset := c.g.structs[var.typ.idx()].offsets[field.i]

				c.g.expr(f.expr)
				// TODO expr not on rax
				c.mov_reg_to_var(var, Amd64Register.rax, offset: offset, typ: field.typ)
			}
		}
		GlobalVar {
			// TODO
		}
	}
}

fn (mut c Amd64) convert_bool_to_string(r Register) {
	reg := r as Amd64Register

	c.cmp_zero(reg)
	false_label := c.g.labels.new_label()
	false_cjmp_addr := c.cjmp(.je)
	c.g.labels.patches << LabelPatch{
		id: false_label
		pos: false_cjmp_addr
	}
	c.g.println('; jump to label ${false_label}')

	c.learel(reg, c.g.allocate_string('true', 3, .rel32))

	end_label := c.g.labels.new_label()
	end_jmp_addr := c.jmp(0)
	c.g.labels.patches << LabelPatch{
		id: end_label
		pos: end_jmp_addr
	}
	c.g.println('; jump to label ${end_label}')

	c.g.labels.addrs[false_label] = c.g.pos()
	c.g.println('; label ${false_label}')
	c.learel(reg, c.g.allocate_string('false', 3, .rel32))

	c.g.labels.addrs[end_label] = c.g.pos()
	c.g.println('; label ${end_label}')
}

fn (mut c Amd64) convert_rune_to_string(reg Register, buffer int, var Var, config VarConfig) {
	c.lea_var_to_reg(reg, buffer)

	match reg as Amd64Register {
		.rax {
			c.mov_var_to_reg(Amd64Register.rdi, var, config)
			c.g.write8(0x48)
			c.g.write8(0x89)
			c.g.write8(0x38)
		}
		else {
			c.g.n_error('rune to string not implemented for ${reg}')
		}
	}
}

fn (mut c Amd64) convert_int_to_string(a Register, b Register) {
	r1 := a as Amd64Register
	r2 := b as Amd64Register

	if r1 != .rax {
		c.mov_reg_amd64(.rax, r1)
	}

	if r2 != .rdi {
		c.mov_reg_amd64(.rdi, r2)
	}

	// check if value in rax is zero
	c.cmp_zero(Amd64Register.rax)
	skip_zero_label := c.g.labels.new_label()
	skip_zero_cjmp_addr := c.cjmp(.jne)
	c.g.labels.patches << LabelPatch{
		id: skip_zero_label
		pos: skip_zero_cjmp_addr
	}
	c.g.println('; jump to label ${skip_zero_label}')

	// handle zeros seperately
	// c.mov_int_to_var(LocalVar{buffer, ast.u8_type_idx, ''}, '0'[0])

	c.g.write8(0xc6)
	c.g.write8(0x07)
	c.g.write8(0x30)
	c.g.println("mov BYTE PTR [rdi], '0'")

	end_label := c.g.labels.new_label()
	end_jmp_addr := c.jmp(0)
	c.g.labels.patches << LabelPatch{
		id: end_label
		pos: end_jmp_addr
	}
	c.g.println('; jump to label ${end_label}')

	c.g.labels.addrs[skip_zero_label] = c.g.pos()
	c.g.println('; label ${skip_zero_label}')

	// load a pointer to the string to rdi
	// c.lea_var_to_reg(Amd64Register.rdi, buffer)

	// detect if value in rax is negative
	c.cmp_zero(Amd64Register.rax)
	skip_minus_label := c.g.labels.new_label()
	skip_minus_cjmp_addr := c.cjmp(.jge)
	c.g.labels.patches << LabelPatch{
		id: skip_minus_label
		pos: skip_minus_cjmp_addr
	}
	c.g.println('; jump to label ${skip_minus_label}')

	// add a `-` sign as the first character
	c.g.write8(0xc6)
	c.g.write8(0x07)
	c.g.write8(0x2d)
	c.g.println("mov BYTE PTR [rdi], '-'")

	c.neg(.rax) // negate our integer to make it positive
	c.inc(.rdi) // increment rdi to skip the `-` character
	c.g.labels.addrs[skip_minus_label] = c.g.pos()
	c.g.println('; label ${skip_minus_label}')

	c.mov_reg_amd64(.r12, .rdi) // copy the buffer position to r12

	loop_label := c.g.labels.new_label()
	loop_start := c.g.pos()
	c.g.println('; label ${loop_label}')

	c.push(.rax)

	c.mov(Amd64Register.rdx, 0)
	c.mov(Amd64Register.rbx, 10)
	c.div_reg(.rax, .rbx)
	c.add8(.rdx, '0'[0])

	c.g.write8(0x66)
	c.g.write8(0x89)
	c.g.write8(0x17)
	c.g.println('mov BYTE PTR [rdi], rdx')

	// divide the integer in rax by 10 for next iteration
	c.pop(.rax)
	c.mov(Amd64Register.rbx, 10)
	c.cdq()
	c.g.write8(0x48)
	c.g.write8(0xf7)
	c.g.write8(0xfb)
	c.g.println('idiv rbx')

	// go to the next character
	c.inc(.rdi)

	// if the number in rax still isn't zero, repeat
	c.cmp_zero(Amd64Register.rax)
	loop_cjmp_addr := c.cjmp(.jg)
	c.g.labels.patches << LabelPatch{
		id: loop_label
		pos: loop_cjmp_addr
	}
	c.g.println('; jump to label ${skip_minus_label}')
	c.g.labels.addrs[loop_label] = loop_start

	// after all was converted, reverse the string
	reg := c.g.get_builtin_arg_reg(.reverse_string, 0) as Amd64Register
	c.mov_reg_amd64(reg, .r12)
	c.g.call_builtin(.reverse_string)

	c.g.labels.addrs[end_label] = c.g.pos()
	c.g.println('; label ${end_label}')
}

fn (mut c Amd64) reverse_string(r Register) {
	reg := r as Amd64Register

	if reg != .rdi {
		c.mov_reg_amd64(.rdi, reg)
	}

	c.mov(Amd64Register.eax, 0)

	c.g.write8(0x48)
	c.g.write8(0x8d)
	c.g.write8(0x48)
	c.g.write8(0xff)
	c.g.println('lea rcx, [rax-0x1]')

	c.mov_reg_amd64(.rsi, .rdi)

	c.g.write8(0xf2)
	c.g.write8(0xae)
	c.g.println('repnz scas al, BYTE PTR es:[rdi]')

	c.sub8(.rdi, 0x2)
	c.cmp_reg(.rdi, .rsi)

	c.g.write8(0x7e)
	c.g.write8(0x0a)
	c.g.println('jle 0x1e')

	c.g.write8(0x86)
	c.g.write8(0x07)
	c.g.println('xchg BYTE PTR [rdi], al')

	c.g.write8(0x86)
	c.g.write8(0x06)
	c.g.println('xchg BYTE PTR [rsi], al')

	c.std()

	c.g.write8(0xaa)
	c.g.println('stos BYTE PTR es:[rdi], al')

	c.cld()

	c.g.write8(0xac)
	c.g.println('lods al, BYTE PTR ds:[rsi]')

	c.g.write8(0xeb)
	c.g.write8(0xf1)
	c.g.println('jmp 0xf')
}

fn (mut c Amd64) gen_match_expr(expr ast.MatchExpr) {
	branch_labels := []int{len: expr.branches.len, init: c.g.labels.new_label() + index * 0} // call new_label for all elements in the array
	end_label := c.g.labels.new_label()

	if expr.is_sum_type {
		// TODO
	} else {
		c.g.expr(expr.cond)
	}
	c.push(.rax)

	mut else_label := 0
	for i, branch in expr.branches {
		if branch.is_else {
			else_label = branch_labels[i]
		} else {
			for cond in branch.exprs {
				match cond {
					ast.RangeExpr {
						c.pop(.rdx)
						c.g.expr(cond.low)
						c.cmp_reg(.rax, .rdx)
						c.g.write([u8(0x0f), 0x9e, 0xc3])
						c.g.println('setle bl')
						c.g.expr(cond.high)
						c.cmp_reg(.rax, .rdx)
						c.g.write([u8(0x0f), 0x9d, 0xc1])
						c.g.println('setge cl')
						c.g.write([u8(0x20), 0xcb])
						c.g.println('and bl, cl')
						c.g.write([u8(0x84), 0xdb])
						c.g.println('test bl, bl')
						then_addr := c.cjmp(.jne)
						c.g.labels.patches << LabelPatch{
							id: branch_labels[i]
							pos: then_addr
						}
						c.push(.rdx)
					}
					else {
						c.g.expr(cond)
						c.pop(.rdx)
						c.cmp_reg(.rax, .rdx)
						then_addr := c.cjmp(.je)
						c.g.labels.patches << LabelPatch{
							id: branch_labels[i]
							pos: then_addr
						}
						c.push(.rdx)
					}
				}
			}
		}
	}
	c.pop(.rdx)
	else_addr := c.jmp(0)
	c.g.labels.patches << LabelPatch{
		id: else_label
		pos: else_addr
	}
	for i, branch in expr.branches {
		c.g.labels.addrs[branch_labels[i]] = c.g.pos()
		for stmt in branch.stmts {
			c.g.stmt(stmt)
		}
		c.g.labels.patches << LabelPatch{
			id: end_label
			pos: c.jmp(0)
		}
	}
	c.g.labels.addrs[end_label] = c.g.pos()
}

fn (mut c Amd64) mov_ssereg_to_var(var Var, reg Amd64SSERegister, config VarConfig) {
	match var {
		ast.Ident {
			var_object := c.g.get_var_from_ident(var)
			match var_object {
				LocalVar {
					c.mov_ssereg_to_var(var_object as LocalVar, reg, config)
				}
				GlobalVar {
					c.mov_ssereg_to_var(var_object as GlobalVar, reg, config)
				}
				Register {}
			}
		}
		LocalVar {
			offset := var.offset - config.offset
			is_far_var := offset > 0x80 || offset < -0x7f
			typ := if config.typ == 0 { var.typ } else { config.typ }

			far_var_offset := if is_far_var { 0x40 } else { 0 }
			c.g.write8(if typ == ast.f32_type_idx { 0xf3 } else { 0xf2 })
			if int(reg) >= int(Amd64SSERegister.xmm8) {
				c.g.write8(0x44)
			}
			c.g.write16(0x110f)
			c.g.write8(0x45 + int(reg) % 8 * 8 + far_var_offset)
			if is_far_var {
				c.g.write32(int((0xffffffff - i64(offset) + 1) % 0x100000000))
			} else {
				c.g.write8((0xff - offset + 1) % 0x100)
			}
			inst := if typ == ast.f32_type_idx { 'movss' } else { 'movsd' }
			c.g.println('${inst} [rbp-${offset.hex2()}], ${reg}')
		}
		GlobalVar {
			// TODO
		}
	}
}

fn (mut c Amd64) load_fp_var(var Var, config VarConfig) {
	c.mov_var_to_ssereg(.xmm0, var, config)
}

fn (mut c Amd64) mov_var_to_ssereg(reg Amd64SSERegister, var Var, config VarConfig) {
	match var {
		ast.Ident {
			var_object := c.g.get_var_from_ident(var)
			match var_object {
				LocalVar {
					c.mov_var_to_ssereg(reg, var_object as LocalVar, config)
				}
				GlobalVar {
					c.mov_var_to_ssereg(reg, var_object as GlobalVar, config)
				}
				Register {}
			}
		}
		LocalVar {
			offset := var.offset - config.offset
			is_far_var := offset > 0x80 || offset < -0x7f
			typ := if config.typ == 0 { var.typ } else { config.typ }

			far_var_offset := if is_far_var { 0x40 } else { 0 }
			c.g.write8(if typ == ast.f32_type_idx { 0xf3 } else { 0xf2 })
			if int(reg) >= int(Amd64SSERegister.xmm8) {
				c.g.write8(0x44)
			}
			c.g.write16(0x100f)
			c.g.write8(0x45 + int(reg) % 8 * 8 + far_var_offset)
			if is_far_var {
				c.g.write32(int((0xffffffff - i64(offset) + 1) % 0x100000000))
			} else {
				c.g.write8((0xff - offset + 1) % 0x100)
			}
			inst := if typ == ast.f32_type_idx { 'movss' } else { 'movsd' }
			c.g.println('${inst} ${reg}, [rbp-${offset.hex2()}]')
		}
		GlobalVar {
			// TODO
		}
	}
}

fn (mut c Amd64) mov_ssereg(a Amd64SSERegister, b Amd64SSERegister) {
	c.g.write8(0xf2)
	if int(a) >= int(Amd64SSERegister.xmm8) || int(b) >= int(Amd64SSERegister.xmm8) {
		c.g.write8(0x40 + int(a) / 8 * 4 + int(b) / 8)
	}
	c.g.write16(0x100f)
	c.g.write8(0xc0 + int(a) % 8 * 8 + int(b) % 8)
	c.g.println('movsd ${a}, ${b}')
}

fn (mut c Amd64) mov_ssereg_to_reg(a Amd64Register, b Amd64SSERegister, typ ast.Type) {
	c.g.write8(0x66)
	rex_base, inst := if typ == ast.f32_type_idx {
		0x40, 'movd'
	} else {
		0x48, 'movq'
	}
	if rex_base == 0x48 || int(a) >= int(Amd64Register.r8) || int(b) >= int(Amd64SSERegister.xmm8) {
		c.g.write8(rex_base + int(a) / 8 * 4 + int(b) / 8)
	}
	c.g.write16(0x7e0f)
	c.g.write8(0xc0 + int(a) % 8 * 8 + int(b) % 8)
	c.g.println('${inst} ${a}, ${b}')
}

fn (mut c Amd64) load_fp(f f64) {
	val := unsafe {
		F64I64{
			f: f
		}.i
	}
	c.movabs(Amd64Register.rax, val)
	c.g.println('; ${f}')
	c.mov_reg_to_ssereg(.xmm0, .rax, ast.f64_type_idx)
}

fn (mut c Amd64) mov_reg_to_ssereg(a Amd64SSERegister, b Amd64Register, typ ast.Type) {
	c.g.write8(0x66)
	rex_base, inst := if typ == ast.f32_type_idx {
		0x40, 'movd'
	} else {
		0x48, 'movq'
	}
	if rex_base == 0x48 || int(a) >= int(Amd64SSERegister.xmm8) || int(b) >= int(Amd64Register.r8) {
		c.g.write8(rex_base + int(a) / 8 * 4 + int(b) / 8)
	}
	c.g.write16(0x6e0f)
	c.g.write8(0xc0 + int(a) % 8 * 8 + int(b) % 8)
	c.g.println('${inst} ${a}, ${b}')
}

fn (mut c Amd64) mov_deref_sse(a Amd64SSERegister, b Amd64Register, typ ast.Type) {
	op, inst, len := if typ == ast.f32_type_idx {
		0xf3, 'movss', 'DWORD'
	} else {
		0xf2, 'movsd', 'QWORD'
	}
	c.g.write8(op)
	if int(a) >= int(Amd64SSERegister.xmm8) || int(b) >= int(Amd64Register.r8) {
		c.g.write8(0x40 + int(a) / 8 * 4 + int(b) / 8)
	}
	c.g.write16(0x100f)
	c.g.write8(int(a) % 8 * 8 + int(b) % 8)
	c.g.println('${inst} ${a}, ${len} PTR [${b}]')
}

fn (mut c Amd64) add_sse(a Amd64SSERegister, b Amd64SSERegister, typ ast.Type) {
	c.g.write8(if typ == ast.f32_type_idx { 0xf3 } else { 0xf2 })
	if int(a) >= int(Amd64SSERegister.xmm8) || int(b) >= int(Amd64SSERegister.xmm8) {
		c.g.write8(0x40 + int(a) / 8 * 4 + int(b) / 8)
	}
	c.g.write16(0x580f)
	c.g.write8(0xc0 + int(a) % 8 * 8 + int(b) % 8)
	inst := if typ == ast.f32_type_idx { 'addss' } else { 'addsd' }
	c.g.println('${inst} ${a}, ${b}')
}

fn (mut c Amd64) sub_sse(a Amd64SSERegister, b Amd64SSERegister, typ ast.Type) {
	c.g.write8(if typ == ast.f32_type_idx { 0xf3 } else { 0xf2 })
	if int(a) >= int(Amd64SSERegister.xmm8) || int(b) >= int(Amd64SSERegister.xmm8) {
		c.g.write8(0x40 + int(a) / 8 * 4 + int(b) / 8)
	}
	c.g.write16(0x5c0f)
	c.g.write8(0xc0 + int(a) % 8 * 8 + int(b) % 8)
	inst := if typ == ast.f32_type_idx { 'subss' } else { 'subsd' }
	c.g.println('${inst} ${a}, ${b}')
}

fn (mut c Amd64) mul_sse(a Amd64SSERegister, b Amd64SSERegister, typ ast.Type) {
	c.g.write8(if typ == ast.f32_type_idx { 0xf3 } else { 0xf2 })
	if int(a) >= int(Amd64SSERegister.xmm8) || int(b) >= int(Amd64SSERegister.xmm8) {
		c.g.write8(0x40 + int(a) / 8 * 4 + int(b) / 8)
	}
	c.g.write16(0x590f)
	c.g.write8(0xc0 + int(a) % 8 * 8 + int(b) % 8)
	inst := if typ == ast.f32_type_idx { 'mulss' } else { 'mulsd' }
	c.g.println('${inst} ${a}, ${b}')
}

fn (mut c Amd64) div_sse(a Amd64SSERegister, b Amd64SSERegister, typ ast.Type) {
	c.g.write8(if typ == ast.f32_type_idx { 0xf3 } else { 0xf2 })
	if int(a) >= int(Amd64SSERegister.xmm8) || int(b) >= int(Amd64SSERegister.xmm8) {
		c.g.write8(0x40 + int(a) / 8 * 4 + int(b) / 8)
	}
	c.g.write16(0x5e0f)
	c.g.write8(0xc0 + int(a) % 8 * 8 + int(b) % 8)
	inst := if typ == ast.f32_type_idx { 'divss' } else { 'divsd' }
	c.g.println('${inst} ${a}, ${b}')
}

fn (mut c Amd64) cmp_sse(a Amd64SSERegister, b Amd64SSERegister, typ ast.Type) {
	if typ != ast.f32_type_idx {
		c.g.write8(0x66)
	}
	if int(a) >= int(Amd64SSERegister.xmm8) || int(b) >= int(Amd64SSERegister.xmm8) {
		c.g.write8(0x40 + int(a) / 8 * 4 + int(b) / 8)
	}
	c.g.write16(0x2e0f)
	c.g.write8(0xc0 + int(a) % 8 * 8 + int(b) % 8)
	inst := if typ == ast.f32_type_idx { 'ucomiss' } else { 'ucomisd' }
	c.g.println('${inst} ${a}, ${b}')
}

fn (mut c Amd64) push_sse(reg Amd64SSERegister) {
	c.g.write32(0x08ec8348)
	c.g.println('sub rsp, 0x8')
	c.g.write8(0xf2)
	if int(reg) >= int(Amd64SSERegister.xmm8) {
		c.g.write8(0x44)
	}
	c.g.write16(0x110f)
	c.g.write8(0x04 + int(reg) % 8 * 8)
	c.g.write8(0x24)
	c.g.println('movsd [rsp], ${reg}')
	c.is_16bit_aligned = !c.is_16bit_aligned
	c.g.println('; push ${reg}')
}

fn (mut c Amd64) pop_sse(reg Amd64SSERegister) {
	c.g.write8(0xf2)
	if int(reg) >= int(Amd64SSERegister.xmm8) {
		c.g.write8(0x44)
	}
	c.g.write16(0x100f)
	c.g.write8(0x04 + int(reg) % 8 * 8)
	c.g.write8(0x24)
	c.g.println('movsd ${reg}, [rsp]')
	c.g.write32(0x08c48348)
	c.g.println('add rsp, 0x8')
	c.is_16bit_aligned = !c.is_16bit_aligned
	c.g.println('; pop ${reg}')
}

fn (mut c Amd64) gen_cast_expr(expr ast.CastExpr) {
	c.g.expr(expr.expr)
	if expr.typ != expr.expr_type {
		if expr.typ.is_pure_float() && expr.expr_type.is_pure_float() {
			from_size := c.g.get_type_size(expr.expr_type)
			to_size := c.g.get_type_size(expr.typ)
			if from_size == 4 && to_size == 8 {
				c.g.write32(0xc05a0ff3)
				c.g.println('cvtss2sd xmm0, xmm0')
			}
			if from_size == 8 && to_size == 4 {
				c.g.write32(0xc05a0ff2)
				c.g.println('cvtsd2ss xmm0, xmm0')
			}
		} else if expr.typ.is_pure_float() {
			if c.g.get_type_size(expr.expr_type) == 8 && !expr.expr_type.is_signed() {
				label1 := c.g.labels.new_label()
				label2 := c.g.labels.new_label()
				c.test_reg(.rax)
				addr1 := c.cjmp(.js)
				c.g.labels.patches << LabelPatch{
					id: label1
					pos: addr1
				}
				// if castee is in the range of i64
				match c.g.get_type_size(expr.typ) {
					4 {
						c.g.write([u8(0xf3), 0x48, 0x0f, 0x2a, 0xc0])
						c.g.println('cvtsi2ss xmm0, rax')
					}
					8 {
						c.g.write([u8(0xf2), 0x48, 0x0f, 0x2a, 0xc0])
						c.g.println('cvtsi2sd xmm0, rax')
					}
					else {}
				}
				addr2 := c.jmp(0)
				c.g.labels.patches << LabelPatch{
					id: label2
					pos: addr2
				}
				c.g.labels.addrs[label1] = c.g.pos()
				// if castee has the leftmost bit
				c.mov_reg(Amd64Register.rdx, Amd64Register.rax)
				c.g.write([u8(0x48), 0xd1, 0xe8])
				c.g.println('shr rax')
				c.g.write([u8(0x83), 0xe2, 0x01])
				c.g.println('and edx, 0x1')
				c.bitor_reg(.rax, .rdx)
				match c.g.get_type_size(expr.typ) {
					4 {
						c.g.write([u8(0xf3), 0x48, 0x0f, 0x2a, 0xc0])
						c.g.println('cvtsi2ss xmm0, rax')
					}
					8 {
						c.g.write([u8(0xf2), 0x48, 0x0f, 0x2a, 0xc0])
						c.g.println('cvtsi2sd xmm0, rax')
					}
					else {}
				}
				c.add_sse(.xmm0, .xmm0, expr.typ)
				c.g.labels.addrs[label2] = c.g.pos()
			} else {
				match c.g.get_type_size(expr.typ) {
					4 {
						c.g.write([u8(0xf3), 0x48, 0x0f, 0x2a, 0xc0])
						c.g.println('cvtsi2ss xmm0, rax')
					}
					8 {
						c.g.write([u8(0xf2), 0x48, 0x0f, 0x2a, 0xc0])
						c.g.println('cvtsi2sd xmm0, rax')
					}
					else {}
				}
			}
		} else if expr.expr_type.is_pure_float() {
			if c.g.get_type_size(expr.typ) == 8 && !expr.typ.is_signed() {
				label1 := c.g.labels.new_label()
				label2 := c.g.labels.new_label()
				// TODO constant
				c.movabs(Amd64Register.rdx, i64(u64(0x4000000000000000)))
				match c.g.get_type_size(expr.expr_type) {
					4 {
						c.g.write([u8(0xf3), 0x48, 0x0f, 0x2a, 0xca])
						c.g.println('cvtsi2ss xmm1, rdx')
					}
					8 {
						c.g.write([u8(0xf2), 0x48, 0x0f, 0x2a, 0xca])
						c.g.println('cvtsi2sd xmm1, rdx')
					}
					else {}
				}
				c.add_sse(.xmm1, .xmm1, expr.expr_type)
				c.add_reg(.rdx, .rdx)
				c.cmp_sse(.xmm0, .xmm1, expr.expr_type)
				addr1 := c.cjmp(.jnb)
				c.g.labels.patches << LabelPatch{
					id: label1
					pos: addr1
				}
				match c.g.get_type_size(expr.expr_type) {
					4 {
						c.g.write([u8(0xf3), 0x48, 0x0f, 0x2c, 0xc0])
						c.g.println('cvttss2si rax, xmm0')
					}
					8 {
						c.g.write([u8(0xf2), 0x48, 0x0f, 0x2c, 0xc0])
						c.g.println('cvttsd2si rax, xmm0')
					}
					else {}
				}
				addr2 := c.jmp(0)
				c.g.labels.patches << LabelPatch{
					id: label2
					pos: addr2
				}
				c.g.labels.addrs[label1] = c.g.pos()
				c.sub_sse(.xmm0, .xmm1, expr.expr_type)
				match c.g.get_type_size(expr.expr_type) {
					4 {
						c.g.write([u8(0xf3), 0x48, 0x0f, 0x2c, 0xc0])
						c.g.println('cvttss2si rax, xmm0')
					}
					8 {
						c.g.write([u8(0xf2), 0x48, 0x0f, 0x2c, 0xc0])
						c.g.println('cvttsd2si rax, xmm0')
					}
					else {}
				}
				c.add_reg(.rax, .rdx)
				c.g.labels.addrs[label2] = c.g.pos()
			} else {
				match c.g.get_type_size(expr.expr_type) {
					4 {
						c.g.write([u8(0xf3), 0x48, 0x0f, 0x2c, 0xc0])
						c.g.println('cvttss2si rax, xmm0')
					}
					8 {
						c.g.write([u8(0xf2), 0x48, 0x0f, 0x2c, 0xc0])
						c.g.println('cvttsd2si rax, xmm0')
					}
					else {}
				}
			}
		} else {
			c.mov_extend_reg(.rax, .rax, expr.typ)
		}
	}
}

// Temporary!
fn (mut c Amd64) adr(r Arm64Register, delta int) {
	panic('`adr` instruction not supported with amd64')
}
