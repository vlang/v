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

const (
	fn_arg_registers     = [Amd64Register.rdi, .rsi, .rdx, .rcx, .r8, .r9]
	fn_arg_sse_registers = [Amd64SSERegister.xmm0, .xmm1, .xmm2, .xmm3, .xmm4, .xmm5, .xmm6, .xmm7]
	amd64_cpuregs        = ['eax', 'ecx', 'edx', 'ebx', 'esp', 'ebp', 'esi', 'edi']
)

fn (mut c Amd64) main_reg() Register {
	return Amd64Register.rax
}

fn (mut g Gen) dec(reg Amd64Register) {
	g.write16(0xff48)
	match reg {
		.rax { g.write8(0xc8) }
		.rbx { g.write8(0xcb) }
		.rcx { g.write8(0xc9) }
		.rsi { g.write8(0xce) }
		.rdi { g.write8(0xcf) }
		.r12 { g.write8(0xc4) }
		else { panic('unhandled inc ${reg}') }
	}
	g.println('dec ${reg}')
}

[inline]
fn byt(n int, s int) u8 {
	return u8((n >> (s * 8)) & 0xff)
}

fn (mut g Gen) inc(reg Amd64Register) {
	g.write8(0x48)
	g.write8(0xff)
	g.write8(0xc0 + int(reg))
	g.println('inc ${reg}')
}

fn (mut g Gen) neg(reg Amd64Register) {
	g.write8(0x48)
	g.write8(0xf7)
	match reg {
		.rax { g.write8(0xd8) }
		else { panic('unhandled neg ${reg}') }
	}
	g.println('neg ${reg}')
}

fn (mut g Gen) cmp(reg Amd64Register, size Size, val i64) {
	if g.pref.arch != .amd64 {
		panic('cmp')
	}
	// Second byte depends on the size of the value
	match size {
		._8 {
			g.write8(0x48)
			g.write8(0x83)
		}
		._32 {
			g.write8(0x4a)
			g.write8(0x81)
		}
		else {
			panic('unhandled cmp')
		}
	}
	// Third byte depends on the register being compared to
	match reg {
		.r12 { g.write8(0xfc) }
		.rsi { g.write8(0x3f) }
		.eax { g.write8(0xf8) }
		.rbx { g.write8(0xfb) }
		else { panic('unhandled cmp') }
	}
	match size {
		._8 {
			g.write8(int(val))
		}
		._32 {
			g.write32(int(val))
		}
		else {
			panic('unhandled cmp')
		}
	}
	g.println('cmp ${reg}, ${val}')
}

// `cmp rax, rbx`
fn (mut g Gen) cmp_reg(reg Amd64Register, reg2 Amd64Register) {
	match reg {
		.rax {
			match reg2 {
				.rdx {
					g.write([u8(0x48), 0x39, 0xd0])
				}
				.rbx {
					g.write([u8(0x48), 0x39, 0xd8])
				}
				else {
					g.n_error('Cannot compare ${reg} and ${reg2}')
				}
			}
		}
		.rdx {
			match reg2 {
				.rax {
					g.write([u8(0x48), 0x39, 0xc2])
				}
				else {
					g.n_error('Cannot compare ${reg} and ${reg2}')
				}
			}
		}
		.rbx {
			match reg2 {
				.rax {
					g.write([u8(0x48), 0x39, 0xc3])
				}
				else {
					g.n_error('Cannot compare ${reg} and ${reg2}')
				}
			}
		}
		.rdi {
			match reg2 {
				.rsi {
					g.write([u8(0x48), 0x39, 0xf7])
				}
				else {
					g.n_error('Cannot compare ${reg} and ${reg2}')
				}
			}
		}
		else {
			g.n_error('Cannot compare ${reg} and ${reg2}')
		}
	}
	g.println('cmp ${reg}, ${reg2}')
}

// cmp $reg, 0
fn (mut g Gen) cmp_zero(reg Amd64Register) {
	match reg {
		.rax {
			g.write8(0x48)
			g.write8(0x83)
			g.write8(0xf8)
		}
		.eax {
			g.write8(0x83)
			g.write8(0xf8)
		}
		else {
			g.n_error('unhandled cmp ${reg}, 0')
		}
	}

	g.write8(0x00)
	g.println('cmp ${reg}, 0')
}

fn (mut g Gen) cmp_var_reg(var Var, reg Amd64Register, config VarConfig) {
	match var {
		ast.Ident {
			var_object := g.get_var_from_ident(var)
			match var_object {
				LocalVar {
					g.cmp_var_reg(var_object as LocalVar, reg, config)
				}
				GlobalVar {
					g.cmp_var_reg(var_object as GlobalVar, reg, config)
				}
				Register {
					// TODO
					// g.cmp()
				}
			}
		}
		LocalVar {
			// TODO: size
			g.write8(0x48) // 83 for 1 byte?
			g.write8(0x39)
			offset := var.offset - config.offset
			is_far_var := offset > 0x80 || offset < -0x7f
			g.write8(if is_far_var { 0x85 } else { 0x45 })
			if is_far_var {
				g.write32(int((0xffffffff - i64(offset) + 1) % 0x100000000))
			} else {
				g.write8((0xff - offset + 1) % 0x100)
			}
			g.println('cmp var `${var.name}`, ${reg}')
		}
		GlobalVar {
			// TODO
		}
	}
}

fn (mut g Gen) cmp_var(var Var, val int, config VarConfig) {
	match var {
		ast.Ident {
			var_object := g.get_var_from_ident(var)
			match var_object {
				LocalVar {
					g.cmp_var(var_object as LocalVar, val, config)
				}
				GlobalVar {
					g.cmp_var(var_object as GlobalVar, val, config)
				}
				Register {
					// TODO
					// g.cmp()
				}
			}
		}
		LocalVar {
			// TODO: size
			g.write8(0x81) // 83 for 1 byte?
			offset := var.offset - config.offset
			is_far_var := offset > 0x80 || offset < -0x7f
			g.write8(if is_far_var { 0xbd } else { 0x7d })
			if is_far_var {
				g.write32(int((0xffffffff - i64(offset) + 1) % 0x100000000))
			} else {
				g.write8((0xff - offset + 1) % 0x100)
			}
			g.write32(val)
			g.println('cmp var `${var.name}` ${val}')
		}
		GlobalVar {
			// TODO
		}
	}
}

// `sub DWORD [rbp-0x4], 1`
fn (mut g Gen) dec_var(var Var, config VarConfig) {
	match var {
		ast.Ident {
			var_object := g.get_var_from_ident(var)
			match var_object {
				LocalVar {
					g.dec_var(var_object as LocalVar, config)
				}
				GlobalVar {
					g.dec_var(var_object as GlobalVar, config)
				}
				Register {
					// TODO
					// g.dec()
				}
			}
		}
		LocalVar {
			// TODO: size
			g.write8(0x81) // 83 for 1 byte
			offset := var.offset - config.offset
			is_far_var := offset > 0x80 || offset < -0x7f
			g.write8(if is_far_var { 0xad } else { 0x6d })
			if is_far_var {
				g.write32(int((0xffffffff - i64(offset) + 1) % 0x100000000))
			} else {
				g.write8((0xff - offset + 1) % 0x100)
			}
			g.write32(1)
			g.println('dec_var `${var.name}`')
		}
		GlobalVar {
			// TODO
		}
	}
}

// `add DWORD [rbp-0x4], 1`
fn (mut g Gen) inc_var(var Var, config VarConfig) {
	match var {
		ast.Ident {
			var_object := g.get_var_from_ident(var)
			match var_object {
				LocalVar {
					g.inc_var(var_object as LocalVar, config)
				}
				GlobalVar {
					g.inc_var(var_object as GlobalVar, config)
				}
				Register {
					// TODO
					// g.inc()
				}
			}
		}
		LocalVar {
			// TODO: size
			g.write8(0x81) // 83 for 1 byte
			offset := var.offset - config.offset
			is_far_var := offset > 0x80 || offset < -0x7f
			g.write8(if is_far_var { 0x85 } else { 0x45 })
			if is_far_var {
				g.write32(int((0xffffffff - i64(offset) + 1) % 0x100000000))
			} else {
				g.write8((0xff - offset + 1) % 0x100)
			}
			g.write32(1)
			g.println('inc_var `${var.name}`')
		}
		GlobalVar {
			// TODO
		}
	}
}

enum JumpOp {
	je = 0x840f
	jne = 0x850f
	jg = 0x8f0f
	jge = 0x8d0f
	jl = 0x8c0f
	jle = 0x8e0f
	js = 0x880f
	jnb = 0x830f
}

fn (mut g Gen) cjmp(op JumpOp) int {
	g.write16(u16(op))
	pos := g.pos()
	g.write32(placeholder)
	g.println('${op}')
	return int(pos)
}

fn (mut g Gen) jmp(addr int) int {
	g.write8(0xe9)
	pos := g.pos()
	g.write32(addr) // 0xffffff
	g.println('jmp')
	// return the position of jump address for placeholder
	return int(pos)
}

enum SetOp {
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

// SETcc al
fn (mut g Gen) cset(op SetOp) {
	g.write16(u16(op))
	g.write8(0xc0)
	g.println('set${op} al')
}

fn abs(a i64) i64 {
	return if a < 0 { -a } else { a }
}

fn (mut g Gen) tmp_jle(addr i64) {
	// Calculate the relative offset to jump to
	// (`addr` is absolute address)
	offset := 0xff - g.abs_to_rel_addr(addr)
	g.write8(0x7e)
	g.write8(offset)
	g.println('jle')
}

fn (mut g Gen) jl(addr i64) {
	offset := 0xff - g.abs_to_rel_addr(addr)
	g.write8(0x7c)
	g.write8(offset)
	g.println('jl')
}

fn (g &Gen) abs_to_rel_addr(addr i64) int {
	return int(abs(addr - g.buf.len)) - 1
}

/*
fn (mut g Gen) jmp(addr i64) {
	offset := 0xff - g.abs_to_rel_addr(addr)
	g.write8(0xe9)
	g.write8(offset)
}
*/

fn (mut g Gen) mov32(reg Amd64Register, val int) {
	if int(reg) >= int(Amd64Register.r8) {
		g.write8(0x41)
	}
	g.write8(0xb8 + int(reg) % 8)
	g.write32(val)
	g.println('mov32 ${reg}, ${val}')
}

fn (mut g Gen) mov64(reg Amd64Register, val i64) {
	match reg {
		.eax {
			g.write8(0xb8)
			g.write8(0x49)
		}
		.rax {
			g.write8(0x48)
			g.write8(0xb8)
		}
		.rcx {
			g.write8(0x48)
			g.write8(0xc7)
			g.write8(0xc1)
		}
		.rdx {
			g.write8(0x48)
			g.write8(0xc7)
			g.write8(0xc2)
			g.write32(int(val))
			g.println('mov32 ${reg}, ${val}')
			return
		}
		.rbx {
			g.write8(0x48)
			g.write8(0xc7)
			g.write8(0xc3)
		}
		.edi {
			g.write8(0xbe)
		}
		.rsi {
			g.write8(0x48)
			g.write8(0xbe)
		}
		.rdi {
			g.write8(0x48)
			g.write8(0xbf)
		}
		else {
			eprintln('unhandled mov64 ${reg}')
		}
	}
	g.write64(val)
	g.println('mov64 ${reg}, ${val}')
}

fn (mut g Gen) movabs(reg Amd64Register, val i64) {
	g.write8(0x48 + int(reg) / 8)
	g.write8(0xb8 + int(reg) % 8)
	g.write64(val)
	g.println('movabs ${reg}, ${val}')
}

fn (mut g Gen) mov_deref(reg Amd64Register, regptr Amd64Register, typ ast.Type) {
	size := g.get_type_size(typ)
	if size !in [1, 2, 4, 8] {
		g.n_error('Invalid size on dereferencing')
	}
	is_signed := !typ.is_real_pointer() && typ.is_signed()
	rex := int(reg) / 8 * 4 + int(regptr) / 8
	if size == 4 && !is_signed {
		if rex > 0 {
			g.write8(0x40 + rex)
		}
		g.write8(0x8b)
	} else {
		g.write8(0x48 + int(reg) / 8 * 4 + int(regptr) / 8)
		if size <= 2 {
			g.write8(0x0f)
		}
		g.write8(match true {
			size == 1 && is_signed { 0xbe }
			size == 1 && !is_signed { 0xb6 }
			size == 2 && is_signed { 0xbf }
			size == 2 && !is_signed { 0xb7 }
			size == 4 && is_signed { 0x63 }
			else { 0x8b }
		})
	}
	g.write8(int(reg) % 8 * 8 + int(regptr) % 8)
	g.println('mov ${reg}, [${regptr}]')
}

fn (mut g Gen) mov_store(regptr Amd64Register, reg Amd64Register, size Size) {
	if size == ._16 {
		g.write8(0x66)
	}
	if size == ._64 {
		g.write8(0x48 + int(reg) / 8 * 4 + int(regptr) / 8)
	}
	g.write8(if size == ._8 { 0x88 } else { 0x89 })
	g.write8(int(reg) % 8 * 8 + int(regptr) % 8)
	g.println('mov [${regptr}], ${reg}')
}

fn (mut g Gen) mov_reg_to_var(var Var, reg Amd64Register, config VarConfig) {
	if g.pref.arch != .amd64 {
		panic('invalid arch for mov_reg_to_var')
	}
	match var {
		ast.Ident {
			var_object := g.get_var_from_ident(var)
			match var_object {
				LocalVar {
					g.mov_reg_to_var(var_object as LocalVar, reg, config)
				}
				GlobalVar {
					g.mov_reg_to_var(var_object as GlobalVar, reg, config)
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
					g.write16(0x8948 + if is_extended_register { 4 } else { 0 })
					size_str = 'QWORD'
				}
				ast.int_type_idx, ast.u32_type_idx, ast.rune_type_idx {
					if is_extended_register {
						g.write8(0x44)
					}
					g.write8(0x89)
					size_str = 'DWORD'
				}
				ast.i16_type_idx, ast.u16_type_idx {
					g.write8(0x66)
					if is_extended_register {
						g.write8(0x44)
					}
					g.write8(0x89)
					size_str = 'WORD'
				}
				ast.i8_type_idx, ast.u8_type_idx, ast.char_type_idx, ast.bool_type_idx {
					if is_extended_register {
						g.write8(0x44)
					}
					g.write8(0x88)
					size_str = 'BYTE'
				}
				else {
					if typ.is_real_pointer() {
						g.write16(0x8948 + if is_extended_register { 4 } else { 0 })
						size_str = 'QWORD'
					} else {
						ts := g.table.sym(typ.idx())
						if ts.info is ast.Enum {
							if is_extended_register {
								g.write8(0x44)
							}
							g.write8(0x89)
							size_str = 'DWORD'
						} else {
							g.n_error('unsupported type for mov_reg_to_var')
						}
					}
				}
			}
			far_var_offset := if is_far_var { 0x40 } else { 0 }
			match reg {
				.eax, .rax, .r8 { g.write8(0x45 + far_var_offset) }
				.edi, .rdi { g.write8(0x7d + far_var_offset) }
				.rsi { g.write8(0x75 + far_var_offset) }
				.rdx { g.write8(0x55 + far_var_offset) }
				.rcx, .r9 { g.write8(0x4d + far_var_offset) }
				else { g.n_error('mov_from_reg ${reg}') }
			}
			if is_far_var {
				g.write32(int((0xffffffff - i64(offset) + 1) % 0x100000000))
			} else {
				g.write8((0xff - offset + 1) % 0x100)
			}
			g.println('mov ${size_str} PTR [rbp-${offset.hex2()}],${reg}')
		}
		GlobalVar {
			// TODO
		}
	}
}

fn (mut g Gen) mov_int_to_var(var Var, integer int, config VarConfig) {
	match var {
		ast.Ident {
			var_object := g.get_var_from_ident(var)
			match var_object {
				LocalVar {
					g.mov_int_to_var(var_object as LocalVar, integer, config)
				}
				GlobalVar {
					g.mov_int_to_var(var_object as GlobalVar, integer, config)
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
					g.write8(0xc6)
					g.write8(if is_far_var { 0x85 } else { 0x45 })
					if is_far_var {
						g.write32(int((0xffffffff - i64(offset) + 1) % 0x100000000))
					} else {
						g.write8((0xff - offset + 1) % 0x100)
					}
					g.write8(u8(integer))
					g.println('mov BYTE PTR[rbp-${offset.hex2()}], ${integer}')
				}
				ast.i16_type_idx, ast.u16_type_idx {
					g.write16(0xc766)
					g.write8(if is_far_var { 0x85 } else { 0x45 })
					if is_far_var {
						g.write32(int((0xffffffff - i64(offset) + 1) % 0x100000000))
					} else {
						g.write8((0xff - offset + 1) % 0x100)
					}
					g.write16(u16(integer))
					g.println('mov WORD PTR[rbp-${offset.hex2()}], ${integer}')
				}
				ast.int_type_idx, ast.u32_type_idx, ast.rune_type_idx {
					g.write8(0xc7)
					g.write8(if is_far_var { 0x85 } else { 0x45 })
					if is_far_var {
						g.write32(int((0xffffffff - i64(offset) + 1) % 0x100000000))
					} else {
						g.write8((0xff - offset + 1) % 0x100)
					}
					g.write32(integer)
					g.println('mov DWORD PTR[rbp-${offset.hex2()}], ${integer}')
				}
				ast.i64_type_idx, ast.u64_type_idx, ast.isize_type_idx, ast.usize_type_idx,
				ast.int_literal_type_idx {
					g.write8(0x48)
					g.write8(0xc7)
					g.write8(if is_far_var { 0x85 } else { 0x45 })
					if is_far_var {
						g.write32(int((0xffffffff - i64(offset) + 1) % 0x100000000))
					} else {
						g.write8((0xff - offset + 1) % 0x100)
					}
					g.write32(integer)
					g.println('mov QWORD PTR[rbp-${offset.hex2()}], ${integer}')
				}
				else {
					g.n_error('unhandled mov int type: ${typ}')
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
			is_signed := !typ.is_real_pointer() && typ.is_signed()

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

fn (mut g Gen) mov_extend_reg(a Amd64Register, b Amd64Register, typ ast.Type) {
	size := g.get_type_size(typ)
	is_signed := !typ.is_real_pointer() && typ.is_signed()

	if size in [1, 2, 4] {
		if size == 4 && !is_signed {
			g.write8(0x40 + if int(a) >= int(Amd64Register.r8) { 1 } else { 0 } +
				if int(b) >= int(Amd64Register.r8) { 4 } else { 0 })
			g.write8(0x89)
		} else {
			g.write8(0x48 + if int(a) >= int(Amd64Register.r8) { 1 } else { 0 } +
				if int(b) >= int(Amd64Register.r8) { 4 } else { 0 })
			if size in [1, 2] {
				g.write8(0x0f)
			}
			g.write8(match true {
				size == 1 && is_signed { 0xbe }
				size == 1 && !is_signed { 0xb6 }
				size == 2 && is_signed { 0xbf }
				size == 2 && !is_signed { 0xb7 }
				else { 0x63 }
			})
		}
		g.write8(0xc0 + int(a) % 8 * 8 + int(b) % 8)
		instruction := if is_signed { 's' } else { 'z' }
		g.println('mov${instruction}x ${a}, ${b}')
	}
}

fn (mut g Gen) call_addr_at(addr int, at i64) i64 {
	// Need to calculate the difference between current position (position after the e8 call)
	// and the function to call.
	// +5 is to get the posistion "e8 xx xx xx xx"
	// Not sure about the -1.
	return 0xffffffff - (at + 5 - addr - 1)
}

fn (mut g Gen) call(addr int) i64 {
	rel := g.call_addr_at(addr, g.pos())
	c_addr := g.pos()
	// println('call addr=$addr.hex2() rel_addr=$rel.hex2() pos=$g.buf.len')
	g.write8(0xe8)

	g.write32(int(rel))
	g.println('call ${addr}')

	return c_addr
}

fn (mut g Gen) extern_call(addr int) {
	match g.pref.os {
		.linux {
			g.write8(0xff)
			g.write8(0x15)
			g.write32(0)
			g.println('call *@GOTPCREL(%rip)')
		}
		else {
			g.n_error('extern calls are not implemented for ${g.pref.os}')
		}
	}
}

fn (mut c Amd64) syscall() {
	c.g.write8(0x0f)
	c.g.write8(0x05)
	c.g.println('syscall')
}

fn (mut c Amd64) svc() {
	panic('the svc instruction is not available with amd64')
}

fn (mut g Gen) cdq() {
	g.write8(0x99)
	g.println('cdq')
}

pub fn (mut g Gen) ret() {
	if g.pref.arch == .amd64 {
		g.write8(0xc3)
		g.println('ret')
	} else if g.pref.arch == .arm64 {
		g.write32(0xd65f03c0)
	} else {
		panic('invalid arch')
	}
}

pub fn (mut g Gen) push(reg Amd64Register) {
	if int(reg) < int(Amd64Register.r8) {
		g.write8(0x50 + int(reg))
	} else {
		g.write8(0x41)
		g.write8(0x50 + int(reg) - 8)
	}
	if mut g.code_gen is Amd64 {
		g.code_gen.is_16bit_aligned = !g.code_gen.is_16bit_aligned
	}
	g.println('push ${reg}')
}

pub fn (mut g Gen) pop(reg Amd64Register) {
	if int(reg) >= int(Amd64Register.r8) && int(reg) <= int(Amd64Register.r15) {
		g.write8(0x41)
	}
	g.write8(0x58 + int(reg) % 8)
	if mut g.code_gen is Amd64 {
		g.code_gen.is_16bit_aligned = !g.code_gen.is_16bit_aligned
	}
	g.println('pop ${reg}')
}

pub fn (mut g Gen) sub8(reg Amd64Register, val int) {
	g.write8(0x48)
	g.write8(0x83)
	g.write8(0xe8 + int(reg)) // TODO rax is different?
	g.write8(val)
	g.println('sub8 ${reg},${val.hex2()}')
}

pub fn (mut g Gen) sub(reg Amd64Register, val int) {
	g.write8(0x48)
	if reg == .rax {
		g.write8(0x2d)
	} else {
		g.write8(0x81)
		g.write8(0xe8 + int(reg))
	}
	g.write32(val)
	g.println('sub ${reg},${val.hex2()}')
}

pub fn (mut g Gen) add(reg Amd64Register, val int) {
	g.write8(0x48)
	if reg == .rax {
		g.write8(0x05)
	} else {
		g.write8(0x81)
		g.write8(0xc0 + int(reg))
	}
	g.write32(val)
	g.println('add ${reg},${val.hex2()}')
}

pub fn (mut g Gen) add8(reg Amd64Register, val int) {
	g.write8(0x48)
	g.write8(0x83)
	g.write8(0xc0 + int(reg))
	g.write8(val)
	g.println('add8 ${reg},${val.hex2()}')
}

[deprecated: 'use add_reg']
[deprecated_after: '2999-01-01']
fn (mut g Gen) add8_var(reg Amd64Register, var_offset int) {
	g.write8(0x03)
	match reg {
		.eax, .rax { g.write8(0x45) }
		else { g.n_error('add8_var') }
	}
	g.write8(0xff - var_offset + 1)
	g.println('add8 ${reg},DWORD PTR[rbp-${var_offset.hex2()}]')
}

[deprecated: 'use sub_reg']
[deprecated_after: '2999-01-01']
fn (mut g Gen) sub8_var(reg Amd64Register, var_offset int) {
	g.write8(0x2b)
	match reg {
		.eax, .rax { g.write8(0x45) }
		else { g.n_error('sub8_var') }
	}
	g.write8(0xff - var_offset + 1)
	g.println('sub8 ${reg},DWORD PTR[rbp-${var_offset.hex2()}]')
}

[deprecated: 'use div_reg']
[deprecated_after: '2999-01-01']
fn (mut g Gen) div8_var(reg Amd64Register, var_offset int) {
	if reg == .rax || reg == .eax {
		g.code_gen.mov_var_to_reg(Amd64Register.rbx, LocalVar{var_offset, ast.i64_type_idx, ''})
		g.div_reg(.rax, .rbx)
		g.mov_reg_to_var(LocalVar{var_offset, ast.i64_type_idx, ''}, .rax)
	} else {
		panic('div8_var invalid source register')
	}
}

[deprecated: 'use mul_reg']
[deprecated_after: '2999-01-01']
fn (mut g Gen) mul8_var(reg Amd64Register, var_offset int) {
	g.write8(0x0f)
	g.write8(0xaf)
	match reg {
		.eax, .rax { g.write8(0x45) }
		else { g.n_error('mul8_var') }
	}
	g.write8(0xff - var_offset + 1)
	g.println('mul8 ${reg},DWORD PTR[rbp-${var_offset.hex2()}]')
}

fn (mut g Gen) bitand_reg(a Amd64Register, b Amd64Register) {
	g.write8(0x48 + if int(a) >= int(Amd64Register.r8) { 1 } else { 0 } +
		if int(b) >= int(Amd64Register.r8) { 4 } else { 0 })
	g.write8(0x21)
	g.write8(0xc0 + int(a) % 8 + int(b) % 8 * 8)
	g.println('and ${a}, ${b}')
}

fn (mut g Gen) bitor_reg(a Amd64Register, b Amd64Register) {
	g.write8(0x48 + if int(a) >= int(Amd64Register.r8) { 1 } else { 0 } +
		if int(b) >= int(Amd64Register.r8) { 4 } else { 0 })
	g.write8(0x09)
	g.write8(0xc0 + int(a) % 8 + int(b) % 8 * 8)
	g.println('or ${a}, ${b}')
}

fn (mut g Gen) bitxor_reg(a Amd64Register, b Amd64Register) {
	g.write8(0x48 + if int(a) >= int(Amd64Register.r8) { 1 } else { 0 } +
		if int(b) >= int(Amd64Register.r8) { 4 } else { 0 })
	g.write8(0x31)
	g.write8(0xc0 + int(a) % 8 + int(b) % 8 * 8)
	g.println('xor ${a}, ${b}')
}

fn (mut g Gen) bitnot_reg(a Amd64Register) {
	g.write8(0x48 + if int(a) >= int(Amd64Register.r8) { 1 } else { 0 })
	g.write8(0xf7)
	g.write8(0xd0 + int(a) % 8)
	g.println('not ${a}')
}

fn (mut g Gen) shl_reg(a Amd64Register, b Amd64Register) {
	if b != .rcx {
		g.code_gen.mov_reg(Amd64Register.rcx, b)
	}
	g.write8(if int(a) >= int(Amd64Register.r8) { 0x49 } else { 0x48 })
	g.write8(0xd3)
	g.write8(0xe0 + int(a) % 8)
	g.println('shl ${a}, ${b}')
}

fn (mut g Gen) sar_reg(a Amd64Register, b Amd64Register) {
	if b != .rcx {
		g.code_gen.mov_reg(Amd64Register.rcx, b)
	}
	g.write8(if int(a) > 7 { 0x49 } else { 0x48 })
	g.write8(0xd3)
	g.write8(0xf8 + int(a) % 8)
	g.println('sar ${a}, ${b}')
}

fn (mut g Gen) shr_reg(a Amd64Register, b Amd64Register) {
	if b != .rcx {
		g.code_gen.mov_reg(Amd64Register.rcx, b)
	}
	g.write8(if int(a) > 7 { 0x49 } else { 0x48 })
	g.write8(0xd3)
	g.write8(0xe8 + int(a) % 8)
	g.println('shr ${a}, ${b}')
}

fn (mut g Gen) leave() {
	g.println('; label 0: return')
	if g.defer_stmts.len != 0 {
		// save return value
		g.push(.rax)
		for defer_stmt in g.defer_stmts.reverse() {
			name := '_defer${defer_stmt.idx_in_fn}'
			defer_var := g.get_var_offset(name)
			g.code_gen.mov_var_to_reg(Amd64Register.rax, LocalVar{defer_var, ast.i64_type_idx, name})
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
		g.pop(.rax)
	}
	g.code_gen.mov_reg(Amd64Register.rsp, Amd64Register.rbp)
	g.pop(.rbp)
	g.ret()
}

// returns label's relative address
pub fn (mut g Gen) gen_loop_start(from int) int {
	g.code_gen.mov(Amd64Register.r12, from)
	label := g.buf.len
	g.inc(.r12)
	return label
}

pub fn (mut g Gen) gen_loop_end(to int, label int) {
	g.cmp(.r12, ._8, to)
	g.jl(label)
}

// not used?
pub fn (mut g Gen) var_zero(vo int, size int) {
	g.mov32(.rcx, size)
	g.code_gen.lea_var_to_reg(Amd64Register.rdi, vo)
	g.write8(0xb0)
	g.write8(0x00)
	g.println('mov al, 0')
	g.rep_stosb()
}

pub fn (mut g Gen) rep_stosb() {
	g.write8(0xf3)
	g.write8(0xaa)
	g.println('rep stosb')
}

pub fn (mut g Gen) std() {
	g.write8(0xfd)
	g.println('std')
}

pub fn (mut g Gen) cld() {
	g.write8(0xfc)
	g.println('cld')
}

pub fn (mut g Gen) cld_repne_scasb() {
	g.cld()
	g.write8(0xf2)
	g.write8(0xae)
	g.println('repne scasb')
}

pub fn (mut g Gen) xor(r Amd64Register, v int) {
	if v == -1 {
		match r {
			.rcx {
				g.write8(0x48)
				g.write8(0x83)
				g.write8(0xf1)
				g.write8(0xff)
				g.println('xor rcx, -1')
			}
			else {
				g.n_error('unhandled xor')
			}
		}
	} else {
		g.n_error('unhandled xor')
	}
}

pub fn (mut g Gen) test_reg(r Amd64Register) {
	g.write8(0x48 + if int(r) >= int(Amd64Register.r8) { 1 } else { 0 } +
		if int(r) >= int(Amd64Register.r8) { 4 } else { 0 })
	g.write8(0x85)
	g.write8(0xc0 + int(r) % 8 + int(r) % 8 * 8)
	g.println('test ${r}, ${r}')
}

// return length in .rax of string pointed by given register
pub fn (mut g Gen) inline_strlen(r Amd64Register) {
	g.code_gen.mov_reg(Amd64Register.rdi, r)
	g.code_gen.mov(Amd64Register.rcx, -1)
	g.code_gen.mov(Amd64Register.eax, 0)
	g.cld_repne_scasb()
	g.xor(.rcx, -1)
	g.dec(.rcx)
	g.code_gen.mov_reg(Amd64Register.rax, Amd64Register.rcx)
	g.println('strlen rax, ${r}')
}

// TODO: strlen of string at runtime
pub fn (mut g Gen) gen_print_reg(r Amd64Register, n int, fd int) {
	g.code_gen.mov_reg(Amd64Register.rsi, r)
	if n < 0 {
		g.inline_strlen(.rsi)
		g.code_gen.mov_reg(Amd64Register.rdx, Amd64Register.rax)
	} else {
		g.code_gen.mov(Amd64Register.edx, n)
	}
	g.code_gen.mov(Amd64Register.eax, g.nsyscall_write())
	g.code_gen.mov(Amd64Register.edi, fd)
	g.code_gen.syscall()
}

pub fn (mut g Gen) apicall(s string) {
	if g.pref.os != .windows {
		g.n_error('apicalls are only for windows')
	}
	g.write8(0xff)
	g.write8(0x15)
	delta := match s {
		'WriteFile' {
			-(0xbcc + g.buf.len)
		}
		'GetStdHandle' {
			-(0xbcc + g.buf.len + 8)
		}
		'ExitProcess' {
			-(0xbcc + g.buf.len + 16)
		}
		else {
			0
		}
	}
	g.write32(delta)
}

pub fn (mut g Gen) gen_print(s string, fd int) {
	if g.pref.os == .windows {
		g.sub(.rsp, 0x38)
		g.code_gen.mov(Amd64Register.rcx, -11)
		g.apicall('GetStdHandle')
		g.code_gen.mov_reg(Amd64Register.rcx, Amd64Register.rax)
		// g.mov64(.rdx, g.allocate_string(s, 3))
		g.lea(.rdx, g.allocate_string(s, 3, .abs64))
		g.code_gen.mov(Amd64Register.r8, s.len) // string length
		g.write([u8(0x4c), 0x8d, 0x4c, 0x24, 0x20]) // lea r9, [rsp+0x20]
		g.write([u8(0x48), 0xc7, 0x44, 0x24, 0x20])
		g.write32(0) // mov qword[rsp+0x20], 0
		// g.code_gen.mov(Amd64Register.r9, rsp+0x20)
		g.apicall('WriteFile')
	} else {
		g.code_gen.mov(Amd64Register.eax, g.nsyscall_write())
		g.code_gen.mov(Amd64Register.edi, fd)
		g.code_gen.learel(Amd64Register.rsi, g.allocate_string(s, 3, .rel32)) // for rsi its 2
		g.code_gen.mov(Amd64Register.edx, s.len) // len
		g.code_gen.syscall()
	}
}

fn (mut g Gen) nsyscall_write() int {
	match g.pref.os {
		.linux {
			return 1
		}
		.windows {
			return 0
		}
		.macos {
			return 0x2000004
		}
		else {
			g.n_error('unsupported write syscall for this platform')
		}
	}
	return 0
}

fn (mut g Gen) nsyscall_exit() int {
	match g.pref.os {
		.linux {
			return 60
		}
		.macos {
			return 0x2000001
		}
		.windows {
			return 0
		}
		else {
			g.n_error('unsupported exit syscall for this platform')
		}
	}
	return 0
}

pub fn (mut a Amd64) gen_exit(node ast.Expr) {
	a.g.gen_amd64_exit(node)
}

pub fn (mut g Gen) gen_amd64_exit(expr ast.Expr) {
	g.expr(expr)
	g.code_gen.mov_reg(Amd64Register.rdi, Amd64Register.rax)

	if g.pref.os == .windows {
		g.code_gen.mov_reg(Amd64Register.rcx, Amd64Register.rdi)
		g.apicall('ExitProcess')
	} else {
		g.code_gen.mov(Amd64Register.rax, g.nsyscall_exit())
		g.code_gen.syscall()
	}
	g.trap() // should never be reached, just in case
}

fn (mut g Gen) relpc(dst Amd64Register, src Amd64Register) {
	//  488d1d 00000000 lea 0(%rip),%dst
	//  4801d8          add %dst, %src
	match dst {
		.rax {
			match src {
				.rsi {
					g.write([u8(0x48), 0x8d, 0x35, 0x00, 0x00, 0x00, 0x00]) // lea rsi, rip
					g.write([u8(0x48), 0x01, 0xf0]) // add rax, rsi
				}
				.rbx {
					g.write([u8(0x48), 0x8d, 0x1d, 0x00, 0x00, 0x00, 0x00])
					g.write([u8(0x48), 0x01, 0xd8])
				}
				else {
					panic('relpc requires .rax, {.rsi,.rbx}')
				}
			}
		}
		else {
			panic('relpc requires .rax, {.rsi,.rbx}')
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

fn (mut g Gen) lea(reg Amd64Register, val int) {
	g.write8(0x48)
	g.write8(0x8d)
	g.write8(0x15)
	g.write32(val)
	g.println('lea ${reg}, ${val}')
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

fn (mut g Gen) mul_reg(a Amd64Register, b Amd64Register) {
	if a != .rax {
		panic('mul always operates on rax')
	}
	match b {
		.rax {
			g.write8(0x48)
			g.write8(0xf7)
			g.write8(0xe8)
		}
		.rbx {
			g.write8(0x48)
			g.write8(0xf7)
			g.write8(0xeb)
		}
		.rdx {
			g.write8(0x48)
			g.write8(0xf7)
			g.write8(0xe2)
		}
		else {
			panic('unhandled mul ${b}')
		}
	}
	g.println('mul ${b}')
}

fn (mut g Gen) imul_reg(r Amd64Register) {
	match r {
		.rsi {
			g.write8(0x48)
			g.write8(0xf7)
			g.write8(0xee)
			g.println('imul ${r}')
		}
		else {
			panic('unhandled imul ${r}')
		}
	}
}

fn (mut g Gen) div_reg(a Amd64Register, b Amd64Register) {
	if a != .rax {
		panic('div always operates on rax')
	}
	match b {
		.rax {
			g.write8(0x48)
			g.write8(0xf7)
			g.write8(0xf8)
		}
		.rbx {
			g.code_gen.mov(Amd64Register.edx, 0)
			g.write8(0x48)
			g.write8(0xf7)
			g.write8(0xfb) // idiv ebx
		}
		.rdx {
			g.write8(0x48)
			g.write8(0xf7)
			g.write8(0xf2)
		}
		else {
			panic('unhandled div ${b}')
		}
	}
	g.println('div ${b}')
}

fn (mut g Gen) mod_reg(a Amd64Register, b Amd64Register) {
	g.div_reg(a, b)
	g.code_gen.mov_reg(Amd64Register.rdx, Amd64Register.rax)
}

fn (mut g Gen) sub_reg(a Amd64Register, b Amd64Register) {
	if int(a) <= int(Amd64Register.r15) && int(b) <= int(Amd64Register.r15) {
		g.write8(0x48 + if int(a) >= int(Amd64Register.r8) { 1 } else { 0 } +
			if int(b) >= int(Amd64Register.r8) { 4 } else { 0 })
		g.write8(0x29)
		g.write8(0xc0 + int(a) % 8 + int(b) % 8 * 8)
	} else {
		g.n_error('unhandled sub ${a}, ${b}')
	}
	g.println('sub ${a}, ${b}')
}

fn (mut g Gen) add_reg(a Amd64Register, b Amd64Register) {
	if int(a) <= int(Amd64Register.r15) && int(b) <= int(Amd64Register.r15) {
		g.write8(0x48 + if int(a) >= int(Amd64Register.r8) { 1 } else { 0 } +
			if int(b) >= int(Amd64Register.r8) { 4 } else { 0 })
		g.write8(0x01)
		g.write8(0xc0 + int(a) % 8 + int(b) % 8 * 8)
	} else {
		g.n_error('unhandled add ${a}, ${b}')
	}
	g.println('add ${a}, ${b}')
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

fn (mut g Gen) add_store(a Amd64Register, b Amd64Register, size Size) {
	if size == ._16 {
		g.write8(0x66)
	}
	if size == ._64 {
		g.write8(0x48 + int(b) / 8 * 4 + int(a) / 8)
	}
	g.write8(if size == ._8 { 0x00 } else { 0x01 })
	g.write8(int(b) % 8 * 8 + int(a) % 8)
	g.println('add [${a}], ${b}')
}

fn (mut g Gen) sub_store(a Amd64Register, b Amd64Register, size Size) {
	if size == ._16 {
		g.write8(0x66)
	}
	if size == ._64 {
		g.write8(0x48 + int(b) / 8 * 4 + int(a) / 8)
	}
	g.write8(if size == ._8 { 0x28 } else { 0x29 })
	g.write8(int(b) % 8 * 8 + int(a) % 8)
	g.println('sub [${a}], ${b}')
}

[params]
struct AvailableAmd64Register {
	available Amd64Register
}

fn (mut g Gen) mul_store(a Amd64Register, b Amd64Register, size Size) {
	if size == ._8 {
	} else {
		if size == ._16 {
			g.write8(0x66)
		}
		if size == ._64 {
			g.write8(0x48 + int(b) / 8 * 4 + int(a) / 8)
		}
		g.write16(0xaf0f)
		g.write8(int(b) % 8 * 8 + int(a) % 8)
		g.println('imul ${b}, [${a}]')
		g.mov_store(a, b, size)
	}
}

fn (mut g Gen) sar8(r Amd64Register, val u8) {
	g.write8(0x48)
	g.write8(0xc1)

	match r {
		.rax {
			g.write8(0xf8)
		}
		.rdx {
			g.write8(0xfa)
		}
		else {
			panic('unhandled sar ${r}, ${val}')
		}
	}
	g.write8(val)
	g.println('sar ${r}, ${val}')
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
		c.g.push(.rbp)
	}
	reg_args << ssereg_args
	reg_args << stack_args
	for i in reg_args.reverse() {
		if i == 0 && is_struct_return {
			c.lea_var_to_reg(Amd64Register.rax, return_pos)
			c.g.push(.rax)
			continue
		}
		c.g.expr(args[i].expr)
		if c.g.table.sym(args[i].typ).kind == .struct_ && !args[i].typ.is_ptr() {
			match args_size[i] {
				1...8 {
					c.g.mov_deref(.rax, .rax, ast.i64_type_idx)
					if args_size[i] != 8 {
						c.g.movabs(.rdx, i64((u64(1) << (args_size[i] * 8)) - 1))
						c.g.bitand_reg(.rax, .rdx)
					}
				}
				9...16 {
					c.g.add(.rax, 8)
					c.g.mov_deref(.rdx, .rax, ast.i64_type_idx)
					c.g.sub(.rax, 8)
					c.g.mov_deref(.rax, .rax, ast.i64_type_idx)
					if args_size[i] != 16 {
						c.g.movabs(.rbx, i64((u64(1) << ((args_size[i] - 8) * 8)) - 1))
						c.g.bitand_reg(.rdx, .rbx)
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
			c.g.push_sse(.xmm0)
		} else {
			match args_size[i] {
				1...8 {
					c.g.push(.rax)
				}
				9...16 {
					c.g.push(.rdx)
					c.g.push(.rax)
				}
				else {
					c.g.add(.rax, args_size[i] - ((args_size[i] + 7) % 8 + 1))
					for _ in 0 .. (args_size[i] + 7) / 8 {
						c.g.mov_deref(.rdx, .rax, ast.i64_type_idx)
						c.g.push(.rdx)
						c.g.sub(.rax, 8)
					}
				}
			}
		}
	}
	for i in 0 .. reg_size {
		c.g.pop(native.fn_arg_registers[i])
	}
	for i in 0 .. ssereg_args.len {
		c.g.pop_sse(native.fn_arg_sse_registers[i])
	}
	c.mov(Amd64Register.rax, ssereg_args.len)
	if node.name in c.g.extern_symbols {
		c.g.extern_fn_calls[c.g.pos()] = node.name
		c.g.extern_call(int(addr))
	} else if addr == 0 {
		c.g.delay_fn_call(n)
		c.g.call(int(0))
	} else {
		c.g.call(int(addr))
	}
	c.g.println('call `${n}()`')

	if ts.kind in [.struct_, .multi_return] {
		match return_size {
			1...7 {
				c.mov_var_to_reg(Amd64Register.rdx, LocalVar{
					offset: return_pos
					typ: ast.i64_type_idx
				})
				c.g.movabs(.rcx, i64(0xffffffffffffffff - (u64(1) << (return_size * 8)) + 1))
				c.g.bitand_reg(.rdx, .rcx)
				c.g.bitor_reg(.rdx, .rax)
				c.g.mov_reg_to_var(LocalVar{ offset: return_pos, typ: ast.i64_type_idx },
					.rdx)
			}
			8 {
				c.g.mov_reg_to_var(LocalVar{ offset: return_pos, typ: ast.i64_type_idx },
					.rax)
			}
			9...15 {
				c.g.mov_reg_to_var(LocalVar{ offset: return_pos, typ: ast.i64_type_idx },
					.rax)
				c.mov_var_to_reg(Amd64Register.rax, LocalVar{
					offset: return_pos
					typ: ast.i64_type_idx
				},
					offset: 8
				)
				c.g.movabs(.rcx, i64(0xffffffffffffffff - (u64(1) << (return_size * 8)) + 1))
				c.g.bitand_reg(.rax, .rcx)
				c.g.bitor_reg(.rax, .rdx)
				c.g.mov_reg_to_var(LocalVar{ offset: return_pos, typ: ast.i64_type_idx },
					.rax,
					offset: 8
				)
			}
			16 {
				c.g.mov_reg_to_var(LocalVar{ offset: return_pos, typ: ast.i64_type_idx },
					.rax)
				c.g.mov_reg_to_var(LocalVar{ offset: return_pos, typ: ast.i64_type_idx },
					.rdx,
					offset: 8
				)
			}
			else {}
		}
		c.lea_var_to_reg(Amd64Register.rax, return_pos)
	}

	if !is_16bit_aligned {
		// dummy data
		c.g.pop(.rdi)
	}
	for _ in 0 .. stack_size {
		// args
		c.g.pop(.rdi)
	}
}

fn (mut g Gen) call_builtin_amd64(name Builtin) i64 {
	call_addr := g.call(0)
	g.println('call builtin `${name}`')
	return call_addr
}

fn (mut g Gen) patch_calls() {
	for c in g.callpatches {
		addr := g.fn_addr[c.name]
		if addr == 0 {
			g.n_error('fn addr of `${c.name}` = 0')
			return
		}
		last := g.buf.len
		g.call(int(addr + last - c.pos))
		mut patch := []u8{}
		for last < g.buf.len {
			patch << g.buf.pop()
		}
		for i := 0; i < patch.len; i++ {
			g.buf[c.pos + i] = patch[patch.len - i - 1]
		}
	}
}

fn (mut g Gen) patch_labels() {
	for label in g.labels.patches {
		addr := g.labels.addrs[label.id]
		if addr == 0 {
			g.n_error('label addr = 0')
			return
		}
		// Update jmp or cjmp address.
		// The value is the relative address, difference between current position and the location
		// after `jxx 00 00 00 00`
		g.write32_at(label.pos, int(addr - label.pos - 4))
	}
}

fn (mut g Gen) delay_fn_call(name string) {
	pos := g.buf.len
	g.callpatches << CallPatch{name, pos}
	// do nothing for now
}

fn (mut g Gen) assign_right_expr(node ast.AssignStmt, i int, right ast.Expr, name string, ident ast.Ident) {
	match right {
		ast.IntegerLiteral {
			// g.allocate_var(name, 4, right.val.int())
			match node.op {
				.plus_assign {
					g.code_gen.mov_var_to_reg(Amd64Register.rax, ident)
					g.add(.rax, right.val.int())
					g.mov_reg_to_var(ident, .rax)
				}
				.minus_assign {
					g.code_gen.mov_var_to_reg(Amd64Register.rax, ident)
					g.sub(.rax, right.val.int())
					g.mov_reg_to_var(ident, .rax)
				}
				.mult_assign {
					g.code_gen.mov_var_to_reg(Amd64Register.rax, ident)
					g.mov64(.rdx, right.val.int())
					g.mul_reg(.rax, .rdx)
					g.mov_reg_to_var(ident, .rax)
				}
				.div_assign {
					g.code_gen.mov_var_to_reg(Amd64Register.rax, ident)
					g.mov64(.rdx, right.val.int())
					g.div_reg(.rax, .rdx)
					g.mov_reg_to_var(ident, .rax)
				}
				.decl_assign {
					g.allocate_var(name, 8, right.val.int())
				}
				.assign {
					// dump(g.typ(node.left_types[i]))
					match node.left[i] {
						ast.Ident {
							// lname := '${node.left[i]}'
							// g.expr(node.right[i])
							g.code_gen.mov(Amd64Register.rax, right.val.int())
							g.mov_reg_to_var(ident, .rax)
						}
						else {
							tn := node.left[i].type_name()
							dump(node.left_types)
							g.n_error('unhandled assign type: ${tn}')
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
					g.code_gen.mov_var_to_reg(Amd64Register.rax, ident)
					g.code_gen.mov_var_to_reg(Amd64Register.rbx, right as ast.Ident)
					g.add_reg(.rax, .rbx)
					g.mov_reg_to_var(ident, .rax)
				}
				.minus_assign {
					g.code_gen.mov_var_to_reg(Amd64Register.rax, ident)
					g.code_gen.mov_var_to_reg(Amd64Register.rbx, right as ast.Ident)
					g.sub_reg(.rax, .rbx)
					g.mov_reg_to_var(ident, .rax)
				}
				.div_assign {
					// this should be called when `a /= b` but it's not :?
					g.code_gen.mov_var_to_reg(Amd64Register.rax, ident)
					g.code_gen.mov_var_to_reg(Amd64Register.rbx, right as ast.Ident)
					g.div_reg(.rax, .rbx)
					g.mov_reg_to_var(ident, .rax)
				}
				.decl_assign {
					typ := node.left_types[i]
					if typ.is_number() || typ.is_real_pointer() || typ.is_bool() {
						g.allocate_var(name, g.get_type_size(typ), 0)
					} else {
						ts := g.table.sym(typ)
						match ts.info {
							ast.Struct {
								g.allocate_by_type(name, typ)
							}
							else {}
						}
					}
					var_ := g.get_var_from_ident(ident)
					// TODO global var
					right_var := g.get_var_from_ident(right) as LocalVar
					match var_ {
						LocalVar {
							var := var_ as LocalVar
							if var.typ.is_number() || var.typ.is_real_pointer() || var.typ.is_bool() {
								g.code_gen.mov_var_to_reg(Amd64Register.rax, right as ast.Ident)
								g.mov_reg_to_var(ident, .rax)
							} else {
								ts := g.table.sym(var.typ)
								match ts.info {
									ast.Struct {
										size := g.get_type_size(var.typ)
										if size >= 8 {
											for offset in 0 .. size / 8 {
												g.code_gen.mov_var_to_reg(Amd64Register.rax,
													right_var,
													offset: offset * 8
													typ: ast.i64_type_idx
												)
												g.mov_reg_to_var(var, .rax,
													offset: offset * 8
													typ: ast.i64_type_idx
												)
											}
											if size % 8 != 0 {
												g.code_gen.mov_var_to_reg(Amd64Register.rax,
													right_var,
													offset: size - 8
													typ: ast.i64_type_idx
												)
												g.mov_reg_to_var(var, .rax,
													offset: size - 8
													typ: ast.i64_type_idx
												)
											}
										} else {
											mut left_size := if size >= 4 {
												g.code_gen.mov_var_to_reg(Amd64Register.rax,
													right_var,
													typ: ast.int_type_idx
												)
												g.mov_reg_to_var(var, .rax,
													typ: ast.int_type_idx
												)
												size - 4
											} else {
												size
											}
											if left_size >= 2 {
												g.code_gen.mov_var_to_reg(Amd64Register.rax,
													right_var,
													offset: size - left_size
													typ: ast.i16_type_idx
												)
												g.mov_reg_to_var(var, .rax,
													offset: size - left_size
													typ: ast.i16_type_idx
												)
												left_size -= 2
											}
											if left_size == 1 {
												g.code_gen.mov_var_to_reg(Amd64Register.rax,
													right_var,
													offset: size - left_size
													typ: ast.i8_type_idx
												)
												g.mov_reg_to_var(var, .rax,
													offset: size - left_size
													typ: ast.i8_type_idx
												)
											}
										}
									}
									else {
										g.n_error('Unsupported variable type')
									}
								}
							}
						}
						else {
							g.n_error('Unsupported variable kind')
						}
					}
				}
				.assign {
					var_ := g.get_var_from_ident(ident)
					// TODO global var
					right_var := g.get_var_from_ident(right) as LocalVar
					match var_ {
						LocalVar {
							var := var_ as LocalVar
							if var.typ.is_number() || var.typ.is_real_pointer() || var.typ.is_bool() {
								g.code_gen.mov_var_to_reg(Amd64Register.rax, right as ast.Ident)
								g.mov_reg_to_var(ident, .rax)
							} else {
								ts := g.table.sym(var.typ)
								match ts.info {
									ast.Struct {
										size := g.get_type_size(var.typ)
										if size >= 8 {
											for offset in 0 .. size / 8 {
												g.code_gen.mov_var_to_reg(Amd64Register.rax,
													right_var,
													offset: offset * 8
													typ: ast.i64_type_idx
												)
												g.mov_reg_to_var(var, .rax,
													offset: offset * 8
													typ: ast.i64_type_idx
												)
											}
											if size % 8 != 0 {
												g.code_gen.mov_var_to_reg(Amd64Register.rax,
													right_var,
													offset: size - 8
													typ: ast.i64_type_idx
												)
												g.mov_reg_to_var(var, .rax,
													offset: size - 8
													typ: ast.i64_type_idx
												)
											}
										} else {
											mut left_size := if size >= 4 {
												g.code_gen.mov_var_to_reg(Amd64Register.rax,
													right_var,
													typ: ast.int_type_idx
												)
												g.mov_reg_to_var(var, .rax,
													typ: ast.int_type_idx
												)
												size - 4
											} else {
												size
											}
											if left_size >= 2 {
												g.code_gen.mov_var_to_reg(Amd64Register.rax,
													right_var,
													offset: size - left_size
													typ: ast.i16_type_idx
												)
												g.mov_reg_to_var(var, .rax,
													offset: size - left_size
													typ: ast.i16_type_idx
												)
												left_size -= 2
											}
											if left_size == 1 {
												g.code_gen.mov_var_to_reg(Amd64Register.rax,
													right_var,
													offset: size - left_size
													typ: ast.i8_type_idx
												)
												g.mov_reg_to_var(var, .rax,
													offset: size - left_size
													typ: ast.i8_type_idx
												)
											}
										}
									}
									else {
										g.n_error('Unsupported variable type')
									}
								}
							}
						}
						else {
							g.n_error('Unsupported variable kind')
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
					g.allocate_by_type(name, right.typ)
					g.init_struct(ident, right)
				}
				else {
					g.n_error('Unexpected operator `${node.op}`')
				}
			}
		}
		ast.ArrayInit {
			// check if array is empty
			mut pos := g.allocate_array(name, 8, right.exprs.len)
			// allocate array of right.exprs.len vars
			for e in right.exprs {
				match e {
					ast.IntegerLiteral {
						g.code_gen.mov(Amd64Register.rax, e.val.int())
						g.mov_reg_to_var(LocalVar{pos, ast.i64_type_idx, ''}, .rax)
						pos += 8
					}
					ast.StringLiteral {
						// TODO: use learel
						str := g.eval_str_lit_escape_codes(e)
						g.mov64(.rsi, g.allocate_string(str, 2, .abs64)) // for rsi its 2
						g.mov_reg_to_var(LocalVar{pos, ast.u64_type_idx, ''}, .rsi)
						pos += 8
					}
					else {
						dump(e)
						g.n_error('unhandled array init type')
					}
				}
			}
		}
		ast.IndexExpr {
			// a := arr[0]
			offset := g.allocate_var(name, g.get_sizeof_ident(ident), 0)
			if g.pref.is_verbose {
				println('infix assignment ${name} offset=${offset.hex2()}')
			}
			ie := right as ast.IndexExpr
			var := ie.left as ast.Ident
			dest := g.get_var_offset(var.name)
			if ie.index is ast.IntegerLiteral {
				index := ie.index
				ie_offset := index.val.int() * 8
				g.code_gen.mov_var_to_reg(Amd64Register.rax, var,
					typ: ast.i64_type_idx
					offset: ie_offset
				)
			} else if ie.index is ast.Ident {
				ie_ident := ie.index
				g.code_gen.lea_var_to_reg(Amd64Register.rax, dest)
				g.code_gen.mov_var_to_reg(Amd64Register.rdi, ie_ident)
				g.add_reg(.rax, .rdi)
				g.mov_deref(.rax, .rax, ast.i64_type_idx)
			} else {
				g.n_error('only integers and idents can be used as indexes')
			}
			// TODO check if out of bounds access
			g.mov_reg_to_var(ident, .eax)
		}
		ast.StringLiteral {
			dest := g.allocate_var(name, 8, 0)
			ie := right as ast.StringLiteral
			str := g.eval_str_lit_escape_codes(ie)
			g.code_gen.learel(Amd64Register.rsi, g.allocate_string(str, 3, .rel32))
			g.mov_reg_to_var(LocalVar{dest, ast.u64_type_idx, name}, .rsi)
		}
		ast.GoExpr {
			g.v_error('threads not implemented for the native backend', node.pos)
		}
		ast.TypeOf {
			g.gen_typeof_expr(right as ast.TypeOf, true)
			g.code_gen.mov_reg(Amd64Register.rsi, Amd64Register.rax)
		}
		ast.AtExpr {
			dest := g.allocate_var(name, 8, 0)
			g.code_gen.learel(Amd64Register.rsi, g.allocate_string(g.comptime_at(right),
				3, .rel32))
			g.mov_reg_to_var(LocalVar{dest, ast.u64_type_idx, name}, .rsi)
		}
		else {
			if right is ast.IfExpr && (right as ast.IfExpr).is_comptime {
				if stmts := g.comptime_conditional(right) {
					for j, stmt in stmts {
						if j + 1 == stmts.len {
							if stmt is ast.ExprStmt {
								g.assign_right_expr(node, i, stmt.expr, name, ident)
							} else {
								g.n_error('last stmt must be expr')
							}
						} else {
							g.stmt(stmt)
						}
					}
				} else {
					g.n_error('missing value for assignment')
				}
				return
			}

			// dump(node)
			size := g.get_type_size(node.left_types[i])
			if size !in [1, 2, 4, 8] || node.op !in [.assign, .decl_assign] {
				g.v_error('unhandled assign_stmt expression: ${right.type_name()}', right.pos())
			}
			if node.op == .decl_assign {
				g.allocate_var(name, size, 0)
			}
			g.expr(right)
			var := g.get_var_from_ident(ident)
			if node.left_types[i].is_pure_float() {
				match var {
					LocalVar { g.mov_ssereg_to_var(var as LocalVar, .xmm0) }
					GlobalVar { g.mov_ssereg_to_var(var as GlobalVar, .xmm0) }
					// Amd64Register { g.mov_ssereg(var as Amd64Register, .xmm0) }
					else {}
				}
			} else {
				match var {
					LocalVar { g.mov_reg_to_var(var as LocalVar, .rax) }
					GlobalVar { g.mov_reg_to_var(var as GlobalVar, .rax) }
					Register { g.code_gen.mov_reg(var as Amd64Register, Amd64Register.rax) }
				}
			}
		}
	}
}

[params]
struct Amd64RegisterOption {
	reg    Amd64Register    = Amd64Register.rax
	ssereg Amd64SSERegister = Amd64SSERegister.xmm0
}

fn (mut g Gen) gen_type_promotion(from ast.Type, to ast.Type, option Amd64RegisterOption) {
	if !to.is_pure_float() {
		return
	}
	if g.is_register_type(from) {
		// integer -> float
		if g.get_type_size(from) == 8 && !from.is_signed() {
			label1 := g.labels.new_label()
			label2 := g.labels.new_label()
			g.test_reg(option.reg)
			addr1 := g.cjmp(.js)
			g.labels.patches << LabelPatch{
				id: label1
				pos: addr1
			}
			// if castee is in the range of i64
			prefix, inst := if g.get_type_size(to) == 4 {
				0xf3, 's'
			} else {
				0xf2, 'd'
			}
			g.write8(prefix)
			g.write8(0x48 + int(option.ssereg) / 8 * 4 + int(option.reg) / 8)
			g.write16(0x2a0f)
			g.write8(0xc0 + int(option.ssereg) % 8 * 8 + int(option.reg) % 8)
			g.println('cvtsi2s${inst} ${option.ssereg}, ${option.reg}')
			addr2 := g.jmp(0)
			g.labels.patches << LabelPatch{
				id: label2
				pos: addr2
			}
			g.labels.addrs[label1] = g.pos()
			// if castee has the leftmost bit
			g.code_gen.mov_reg(Amd64Register.rbx, Amd64Register.rax)
			g.write([u8(0x48), 0xd1, 0xe8])
			g.println('shr rax')
			g.write([u8(0x83), 0xe3, 0x01])
			g.println('and ebx, 0x1')
			g.bitor_reg(.rax, .rbx)
			g.write8(prefix)
			g.write8(0x48 + int(option.ssereg) / 8 * 4 + int(option.reg) / 8)
			g.write16(0x2a0f)
			g.write8(0xc0 + int(option.ssereg) % 8 * 8 + int(option.reg) % 8)
			g.println('cvtsi2s${inst} ${option.ssereg}, ${option.reg}')
			g.add_sse(option.ssereg, option.ssereg, to)
			g.labels.addrs[label2] = g.pos()
		} else {
			prefix, inst := if g.get_type_size(to) == 4 {
				0xf3, 's'
			} else {
				0xf2, 'd'
			}
			g.write8(prefix)
			g.write8(0x48 + int(option.ssereg) / 8 * 4 + int(option.reg) / 8)
			g.write16(0x2a0f)
			g.write8(0xc0 + int(option.ssereg) % 8 * 8 + int(option.reg) % 8)
			g.println('cvtsi2s${inst} ${option.ssereg}, ${option.reg}')
		}
	} else {
		if from == ast.f32_type_idx && to != ast.f32_type_idx {
			// f32 -> f64
			g.write8(0xf3)
			if int(option.ssereg) >= int(Amd64SSERegister.xmm8) {
				g.write8(0x45)
			}
			g.write16(0x5a0f)
			g.write8(0xc0 + int(option.ssereg) % 8 * 9)
			g.println('cvtss2sd ${option.ssereg}, ${option.ssereg}')
		}
	}
}

fn (mut g Gen) multi_assign_stmt(node ast.AssignStmt) {
	multi_return := g.get_multi_return(node.right_types)
	if node.has_cross_var {
		// `a, b = b, a`
		// construct a struct variable contains the return value
		size := multi_return.size
		align := multi_return.align
		padding := (align - g.stack_var_pos % align) % align
		g.stack_var_pos += size + padding
		var := LocalVar{
			offset: g.stack_var_pos
		}
		// zero fill
		mut left := if size >= 16 {
			g.code_gen.mov(Amd64Register.rax, 0)
			g.code_gen.mov(Amd64Register.rcx, size / 8)
			g.code_gen.lea_var_to_reg(Amd64Register.rdi, var.offset)
			g.write([u8(0xf3), 0x48, 0xab])
			g.println('rep stosq')
			size % 8
		} else {
			size
		}
		if left >= 8 {
			g.mov_int_to_var(var, 0, offset: size - left, typ: ast.i64_type_idx)
			left -= 8
		}
		if left >= 4 {
			g.mov_int_to_var(var, 0, offset: size - left, typ: ast.int_type_idx)
			left -= 4
		}
		if left >= 2 {
			g.mov_int_to_var(var, 0, offset: size - left, typ: ast.i16_type_idx)
			left -= 2
		}
		if left == 1 {
			g.mov_int_to_var(var, 0, offset: size - left, typ: ast.i8_type_idx)
		}
		// store exprs to the variable
		for i, expr in node.right {
			offset := multi_return.offsets[i]
			g.expr(expr)
			// TODO expr not on rax
			g.mov_reg_to_var(var, .rax, offset: offset, typ: node.right_types[i])
		}
		// store the multi return struct value
		g.code_gen.lea_var_to_reg(Amd64Register.rax, var.offset)
	} else {
		g.expr(node.right[0])
	}
	g.code_gen.mov_reg(Amd64Register.rdx, Amd64Register.rax)

	mut current_offset := 0
	for i, offset in multi_return.offsets {
		if node.left[i] is ast.Ident {
			name := (node.left[i] as ast.Ident).name
			if name == '_' {
				continue
			}
			if node.op == .decl_assign {
				g.allocate_by_type(name, node.left_types[i])
			}
		}
		if offset != current_offset {
			g.add(.rdx, offset - current_offset)
			current_offset = offset
		}
		g.gen_left_value(node.left[i])
		left_type := node.left_types[i]
		right_type := node.right_types[i]
		if g.is_register_type(right_type) {
			g.mov_deref(.rcx, .rdx, right_type)
		} else if node.right_types[i].is_pure_float() {
			g.mov_deref_sse(.xmm0, .rdx, right_type)
		}
		g.gen_type_promotion(right_type, left_type, reg: .rcx)
		if g.is_register_type(left_type) {
			match node.op {
				.assign, .decl_assign {
					g.mov_store(.rax, .rcx, match g.get_type_size(left_type) {
						1 { ._8 }
						2 { ._16 }
						4 { ._32 }
						else { ._64 }
					})
				}
				else {
					g.n_error('Unsupported assign instruction')
				}
			}
		} else if left_type.is_pure_float() {
			is_f32 := left_type == ast.f32_type_idx
			if node.op !in [.assign, .decl_assign] {
				g.mov_ssereg(.xmm1, .xmm0)
				if is_f32 {
					g.write32(0x00100ff3)
					g.println('movss xmm0, [rax]')
				} else {
					g.write32(0x00100ff2)
					g.println('movsd xmm0, [rax]')
				}
			}
			match node.op {
				.plus_assign {
					g.add_sse(.xmm0, .xmm1, left_type)
				}
				.minus_assign {
					g.sub_sse(.xmm0, .xmm1, left_type)
				}
				.mult_assign {
					g.mul_sse(.xmm0, .xmm1, left_type)
				}
				.div_assign {
					g.div_sse(.xmm0, .xmm1, left_type)
				}
				else {}
			}
			if is_f32 {
				g.write32(0x00110ff3)
				g.println('movss [rax], xmm0')
			} else {
				g.write32(0x00110ff2)
				g.println('movsd [rax], xmm0')
			}
		} else {
			g.n_error('multi return for struct is not supported yet')
		}
	}
}

fn (mut g Gen) assign_stmt(node ast.AssignStmt) {
	// `a, b := foo()`
	// `a, b := if cond { 1, 2 } else { 3, 4 }`
	// `a, b = b, a`
	if (node.left.len > 1 && node.right.len == 1) || node.has_cross_var {
		g.multi_assign_stmt(node)
		return
	}
	// `a := 1` | `a,b := 1,2`
	for i, left in node.left {
		right := node.right[i]
		typ := node.left_types[i]
		// this branch would be removed, but left for compatibility
		if left is ast.Ident && !typ.is_pure_float() {
			ident := left as ast.Ident
			g.assign_right_expr(node, i, right, ident.name, ident)
			continue
		}
		if left is ast.Ident && node.op == .decl_assign {
			g.allocate_by_type((left as ast.Ident).name, typ)
		}
		g.gen_left_value(left)
		g.push(.rax)
		g.expr(right)
		g.pop(.rdx)
		g.gen_type_promotion(node.right_types[0], typ)
		if g.is_register_type(typ) {
			match node.op {
				.decl_assign, .assign {
					g.mov_store(.rdx, .rax, match g.get_type_size(typ) {
						1 { ._8 }
						2 { ._16 }
						4 { ._32 }
						else { ._64 }
					})
				}
				else {
					g.n_error('Unsupported assign instruction')
				}
			}
		} else if typ.is_pure_float() {
			is_f32 := typ == ast.f32_type_idx
			if node.op !in [.assign, .decl_assign] {
				g.mov_ssereg(.xmm1, .xmm0)
				if is_f32 {
					g.write32(0x02100ff3)
					g.println('movss xmm0, [rdx]')
				} else {
					g.write32(0x02100ff2)
					g.println('movsd xmm0, [rdx]')
				}
			}
			match node.op {
				.plus_assign {
					g.add_sse(.xmm0, .xmm1, typ)
				}
				.minus_assign {
					g.sub_sse(.xmm0, .xmm1, typ)
				}
				.mult_assign {
					g.mul_sse(.xmm0, .xmm1, typ)
				}
				.div_assign {
					g.div_sse(.xmm0, .xmm1, typ)
				}
				else {}
			}
			if is_f32 {
				g.write32(0x02110ff3)
				g.println('movss [rdx], xmm0')
			} else {
				g.write32(0x02110ff2)
				g.println('movsd [rdx], xmm0')
			}
		} else {
			if node.op !in [.assign, .decl_assign] {
				g.n_error('Unsupported assign instruction')
			}
			ts := g.table.sym(typ)
			match ts.kind {
				.struct_ {
					size := g.get_type_size(typ)
					if size >= 8 {
						for j in 0 .. size / 8 {
							g.mov_deref(.rcx, .rdx, ast.u64_type_idx)
							g.mov_store(.rax, .rcx, ._64)
							offset := if j == size / 8 - 1 && size % 8 != 0 {
								size % 8
							} else {
								8
							}
							g.add(.rax, offset)
							g.add(.rdx, offset)
						}
						if size % 8 != 0 {
							g.mov_deref(.rcx, .rdx, ast.u64_type_idx)
							g.mov_store(.rax, .rcx, ._64)
						}
					} else {
						mut left_size := if size >= 4 {
							g.mov_deref(.rcx, .rdx, ast.u32_type_idx)
							g.mov_store(.rax, .rcx, ._32)
							if size > 4 {
								g.add(.rax, 4)
								g.add(.rdx, 4)
							}
							size - 4
						} else {
							size
						}
						if left_size >= 2 {
							g.mov_deref(.rcx, .rdx, ast.u16_type_idx)
							g.mov_store(.rax, .rcx, ._16)
							if left_size > 2 {
								g.add(.rax, 2)
								g.add(.rdx, 2)
							}
							left_size -= 2
						}
						if left_size == 1 {
							g.mov_deref(.rcx, .rdx, ast.u8_type_idx)
							g.mov_store(.rax, .rcx, ._8)
						}
					}
				}
				.enum_ {
					g.mov_store(.rdx, .rax, ._32)
				}
				else {}
			}
		}
	}
}

fn (mut g Gen) cset_op(op token.Kind) {
	match op {
		.gt {
			g.cset(.g)
		}
		.lt {
			g.cset(.l)
		}
		.ne {
			g.cset(.ne)
		}
		.eq {
			g.cset(.e)
		}
		.ge {
			g.cset(.ge)
		}
		.le {
			g.cset(.le)
		}
		else {
			g.cset(.ne)
		}
	}
}

fn (mut g Gen) gen_left_value(node ast.Expr) {
	match node {
		ast.Ident {
			offset := g.get_var_offset(node.name)
			g.code_gen.lea_var_to_reg(Amd64Register.rax, offset)
		}
		ast.SelectorExpr {
			g.expr(node.expr)
			offset := g.get_field_offset(node.expr_type, node.field_name)
			if offset != 0 {
				g.add(.rax, offset)
			}
		}
		ast.IndexExpr {} // TODO
		ast.PrefixExpr {
			if node.op != .mul {
				g.n_error('Unsupported left value')
			}
			g.expr(node.right)
		}
		else {
			g.n_error('Unsupported left value')
		}
	}
}

fn (mut g Gen) prefix_expr(node ast.PrefixExpr) {
	match node.op {
		.minus {
			g.expr(node.right)
			if node.right_type.is_pure_float() {
				g.mov_ssereg_to_reg(.rax, .xmm0, node.right_type)
				if node.right_type == ast.f32_type_idx {
					g.mov32(.rdx, int(u32(0x80000000)))
				} else {
					g.movabs(.rdx, i64(u64(0x8000000000000000)))
				}
				g.bitxor_reg(.rax, .rdx)
				g.mov_reg_to_ssereg(.xmm0, .rax, node.right_type)
			} else {
				g.neg(.rax)
			}
		}
		.amp {
			g.gen_left_value(node.right)
		}
		.mul {
			g.expr(node.right)
			g.mov_deref(.rax, .rax, node.right_type.deref())
		}
		.not {
			g.expr(node.right)
			g.cmp_zero(.rax)
			// TODO mov_extend_reg
			g.mov64(.rax, 0)
			g.cset(.e)
		}
		.bit_not {
			g.expr(node.right)
			g.bitnot_reg(.rax)
		}
		else {}
	}
}

fn (mut g Gen) infix_expr(node ast.InfixExpr) {
	if node.op in [.logical_or, .and] {
		g.expr(node.left)
		label := g.labels.new_label()
		g.cmp_zero(.rax)
		jump_addr := g.cjmp(if node.op == .logical_or { .jne } else { .je })
		g.labels.patches << LabelPatch{
			id: label
			pos: jump_addr
		}
		g.expr(node.right)
		g.labels.addrs[label] = g.pos()
		return
	} else {
		g.expr(node.right)
		if node.left_type.is_pure_float() {
			typ := node.left_type
			// optimize for ast.Ident
			match node.left {
				ast.Ident {
					g.mov_ssereg(.xmm1, .xmm0)
					g.mov_var_to_ssereg(.xmm0, node.left as ast.Ident)
				}
				else {
					g.push_sse(.xmm0)
					g.expr(node.left)
					g.pop_sse(.xmm1)
				}
			}
			match node.op {
				.eq, .ne {
					g.write32(0xc1c20ff3)
					g.write8(if node.op == .eq { 0x00 } else { 0x04 })
					inst := if node.op == .eq { 'cmpeqss' } else { 'cmpneqss' }
					g.println('${inst} xmm0, xmm1')
					g.mov_ssereg_to_reg(.rax, .xmm0, ast.f32_type_idx)
					g.write([u8(0x83), 0xe0, 0x01])
					g.println('and eax, 0x1')
				}
				.gt, .lt, .ge, .le {
					g.cmp_sse(.xmm0, .xmm1, typ)
					// TODO mov_extend_reg
					g.mov64(.rax, 0)
					g.cset(match node.op {
						.gt { .a }
						.lt { .b }
						.ge { .ae }
						else { .be }
					})
				}
				.plus {
					g.add_sse(.xmm0, .xmm1, typ)
				}
				.minus {
					g.sub_sse(.xmm0, .xmm1, typ)
				}
				.mul {
					g.mul_sse(.xmm0, .xmm1, typ)
				}
				.div {
					g.div_sse(.xmm0, .xmm1, typ)
				}
				else {
					g.n_error('`${node.op}` expression is not supported right now')
				}
			}
			return
		}
		// optimize for ast.Ident
		match node.left {
			ast.Ident {
				g.code_gen.mov_reg(match node.op {
					.left_shift, .right_shift, .unsigned_right_shift, .div, .mod { Amd64Register.rcx }
					else { Amd64Register.rdx }
				}, Amd64Register.rax)
				g.code_gen.mov_var_to_reg(Amd64Register.rax, node.left as ast.Ident)
			}
			else {
				g.push(.rax)
				g.expr(node.left)
				g.pop(match node.op {
					.left_shift, .right_shift, .unsigned_right_shift, .div, .mod { .rcx }
					else { .rdx }
				})
			}
		}
		if node.left_type !in ast.integer_type_idxs && node.left_type != ast.bool_type_idx
			&& g.table.sym(node.left_type).info !is ast.Enum && !node.left_type.is_ptr()
			&& !node.left_type.is_voidptr() {
			g.n_error('unsupported type for `${node.op}`: ${node.left_type}')
		}
		// left: rax, right: rdx
		match node.op {
			.eq, .ne, .gt, .lt, .ge, .le {
				g.cmp_reg(.rax, .rdx)
				// TODO mov_extend_reg
				g.mov64(.rax, 0)
				g.cset_op(node.op)
			}
			.plus {
				g.add_reg(.rax, .rdx)
			}
			.minus {
				g.sub_reg(.rax, .rdx)
			}
			.mul {
				g.write32(0xc2af0f48)
				g.println('imul rax, rdx')
			}
			.div {
				if node.left_type in ast.unsigned_integer_type_idxs {
					g.write8(0xba)
					g.write32(0)
					g.println('mov edx, 0')
					g.write([u8(0x48), 0xf7, 0xf1])
					g.println('div rcx')
				} else {
					g.write16(0x9948)
					g.println('cqo')
					g.write([u8(0x48), 0xf7, 0xf9])
					g.println('idiv rcx')
				}
			}
			.mod {
				if node.left_type in ast.unsigned_integer_type_idxs {
					g.write8(0xba)
					g.write32(0)
					g.println('mov edx, 0')
					g.write([u8(0x48), 0xf7, 0xf1])
					g.println('div rcx')
				} else {
					g.write16(0x9948)
					g.println('cqo')
					g.write([u8(0x48), 0xf7, 0xf9])
					g.println('idiv rcx')
				}
				g.code_gen.mov_reg(Amd64Register.rax, Amd64Register.rdx)
			}
			.amp {
				g.bitand_reg(.rax, .rdx)
			}
			.pipe {
				g.bitor_reg(.rax, .rdx)
			}
			.xor {
				g.bitxor_reg(.rax, .rdx)
			}
			.left_shift {
				g.shl_reg(.rax, .rcx)
			}
			.right_shift {
				g.sar_reg(.rax, .rcx)
			}
			.unsigned_right_shift {
				g.shr_reg(.rax, .rcx)
			}
			else {
				g.n_error('`${node.op}` expression is not supported right now')
			}
		}
	}
}

fn (mut g Gen) trap() {
	// funnily works on x86 and arm64
	if g.pref.arch == .arm64 {
		g.write32(0xcccccccc)
	} else {
		g.write8(0xcc)
	}
	g.println('trap')
}

fn (mut g Gen) gen_asm_stmt_amd64(asm_node ast.AsmStmt) {
	// inline assembly using vasm
	g.println('// asm inline')
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
					g.v_error('floating point arithmetic is not yet implemented for the native backend',
						asm_node.pos)
				}
				string {
					// XXX
					g.v_error('no strings allowed in this context', asm_node.pos)
				}
				else {
					g.v_error('unsupported instruction argument argument', asm_node.pos)
				}
			}
		}
		g.println(': ${line}')
		match t.name {
			'nop' {
				g.write8(u8(0x90))
				g.println('nop')
			}
			'syscall' {
				g.write8(u8(0x0f))
				g.write8(u8(0x05))
				g.println('syscall')
			}
			'ret' {
				g.write8(u8(0xc3))
				g.println('ret')
			}
			'int3' {
				g.write8(u8(0xcc))
				g.write8(u8(imm))
				g.println('int3')
			}
			'sti' {
				g.write8(u8(0xfb))
				g.println('sti')
			}
			'cli' {
				g.write8(u8(0xfa))
				g.println('cli')
			}
			'int' {
				g.write8(u8(0xcd))
				g.write8(u8(imm))
				g.println('int')
			}
			'cpuid' {
				g.write8(u8(0x0f))
				g.write8(u8(0xa2))
				g.println('cpuid')
			}
			'mov' {
				g.write8(u8(0xb8 + reg))
				g.write8(byt(imm, 0))
				g.write8(byt(imm, 1))
				g.write8(byt(imm, 2))
				g.write8(byt(imm, 3))
				g.println('mov ${reg}, ${imm}')
			}
			else {
				g.v_error('unsupported instruction ${t.name}', asm_node.pos)
			}
		}
	}
}

fn (mut g Gen) gen_assert(assert_node ast.AssertStmt) {
	mut cjmp_addr := 0
	ane := assert_node.expr
	label := g.labels.new_label()
	cjmp_addr = g.condition(ane, true)
	g.labels.patches << LabelPatch{
		id: label
		pos: cjmp_addr
	}
	g.println('; jump to label ${label}')
	g.expr(assert_node.expr)
	g.trap()
	g.labels.addrs[label] = g.pos()
	g.println('; label ${label}')
}

fn (mut g Gen) cjmp_notop(op token.Kind) int {
	return match op {
		.gt {
			g.cjmp(.jle)
		}
		.lt {
			g.cjmp(.jge)
		}
		.ne {
			g.cjmp(.je)
		}
		.eq {
			g.cjmp(.jne)
		}
		else {
			g.cjmp(.je)
		}
	}
}

fn (mut g Gen) cjmp_op(op token.Kind) int {
	return match op {
		.gt {
			g.cjmp(.jg)
		}
		.lt {
			g.cjmp(.jl)
		}
		.ne {
			g.cjmp(.jne)
		}
		.eq {
			g.cjmp(.je)
		}
		else {
			g.cjmp(.jne)
		}
	}
}

fn (mut g Gen) condition(expr ast.Expr, neg bool) int {
	g.expr(expr)
	g.cmp_zero(.rax)
	return g.cjmp(if neg { .jne } else { .je })
}

fn (mut g Gen) if_expr(node ast.IfExpr) {
	if node.is_comptime {
		if stmts := g.comptime_conditional(node) {
			g.stmts(stmts)
		}
		return
	}
	if node.branches.len == 0 {
		return
	}
	mut endif_label := 0
	has_endif := node.branches.len > 1
	if has_endif {
		endif_label = g.labels.new_label()
	}
	for idx in 0 .. node.branches.len {
		branch := node.branches[idx]
		if idx == node.branches.len - 1 && node.has_else {
			g.stmts(branch.stmts)
		} else {
			if branch.cond is ast.BoolLiteral {
				if branch.cond.val {
					g.stmts(branch.stmts)
				}
				continue
			}
			expr := branch.cond
			label := g.labels.new_label()
			cjmp_addr := g.condition(expr, false)
			g.labels.patches << LabelPatch{
				id: label
				pos: cjmp_addr
			}
			g.println('; jump to label ${label}')
			g.stmts(branch.stmts)
			if has_endif {
				jump_addr := g.jmp(0)
				g.labels.patches << LabelPatch{
					id: endif_label
					pos: jump_addr
				}
				g.println('; jump to label ${endif_label}')
			}
			// println('after if g.pos=$g.pos() jneaddr=$cjmp_addr')
			g.labels.addrs[label] = g.pos()
			g.println('; label ${label}')
		}
	}
	if has_endif {
		g.labels.addrs[endif_label] = g.pos()
		g.println('; label ${endif_label}')
	}
}

fn (mut g Gen) infloop() {
	if g.pref.arch == .arm64 {
		g.write32(u8(0x14))
	} else {
		g.write8(u8(0xeb))
		g.write8(u8(0xfe))
	}
	g.println('jmp $$')
}

fn (mut g Gen) for_stmt(node ast.ForStmt) {
	if node.is_inf {
		if node.stmts.len == 0 {
			g.infloop()
			return
		}
		// infinite loop
		start := g.pos()
		start_label := g.labels.new_label()
		g.labels.addrs[start_label] = start
		g.println('; label ${start_label}')
		end_label := g.labels.new_label()
		g.labels.branches << BranchLabel{
			name: node.label
			start: start_label
			end: end_label
		}
		g.stmts(node.stmts)
		g.labels.branches.pop()
		g.jmp(int(0xffffffff - (g.pos() + 5 - start) + 1))
		g.println('jmp after infinite for')
		g.labels.addrs[end_label] = g.pos()
		g.println('; label ${end_label}')
		return
	}
	infix_expr := node.cond as ast.InfixExpr
	mut jump_addr := 0 // location of `jne *00 00 00 00*`
	start := g.pos()
	start_label := g.labels.new_label()
	g.labels.addrs[start_label] = start
	g.println('; label ${start_label}')
	match infix_expr.left {
		ast.Ident {
			match infix_expr.right {
				ast.Ident {
					g.code_gen.mov_var_to_reg(Amd64Register.rax, infix_expr.right as ast.Ident)
					g.cmp_var_reg(infix_expr.left as ast.Ident, .rax)
				}
				ast.IntegerLiteral {
					lit := infix_expr.right as ast.IntegerLiteral
					g.cmp_var(infix_expr.left as ast.Ident, lit.val.int())
				}
				else {
					g.n_error('unhandled expression type')
				}
			}
			match infix_expr.left.tok_kind {
				.lt {
					jump_addr = g.cjmp(.jge)
				}
				.gt {
					jump_addr = g.cjmp(.jle)
				}
				.le {
					jump_addr = g.cjmp(.jg)
				}
				.ge {
					jump_addr = g.cjmp(.jl)
				}
				.ne {
					jump_addr = g.cjmp(.je)
				}
				.eq {
					jump_addr = g.cjmp(.jne)
				}
				else {
					g.n_error('unhandled infix cond token')
				}
			}
		}
		else {
			g.n_error('unhandled infix.left')
		}
	}
	end_label := g.labels.new_label()
	g.labels.patches << LabelPatch{
		id: end_label
		pos: jump_addr
	}
	g.println('; jump to label ${end_label}')
	g.labels.branches << BranchLabel{
		name: node.label
		start: start_label
		end: end_label
	}
	g.stmts(node.stmts)
	g.labels.branches.pop()
	// Go back to `cmp ...`
	// Diff between `jmp 00 00 00 00 X` and `cmp`
	g.jmp(int(0xffffffff - (g.pos() + 5 - start) + 1))
	// Update the jump addr to current pos
	g.labels.addrs[end_label] = g.pos()
	g.println('; label ${end_label}')
	g.println('jmp after for')
}

fn (mut c Amd64) fn_decl(node ast.FnDecl) {
	c.g.push(.rbp)
	c.mov_reg(Amd64Register.rbp, Amd64Register.rsp)
	local_alloc_pos := c.g.pos()
	c.g.sub(.rsp, 0)

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
		c.g.mov_reg_to_var(LocalVar{ offset: offset, typ: ast.i64_type_idx, name: name },
			native.fn_arg_registers[reg_idx])
		reg_idx++
		if args_size[i] > 8 {
			c.g.mov_reg_to_var(LocalVar{ offset: offset, typ: ast.i64_type_idx, name: name },
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
		c.g.mov_ssereg_to_var(LocalVar{ offset: offset, typ: params[i].typ }, native.fn_arg_sse_registers[idx])
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
		c.g.allocate_var(name, 8, 0)
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
		c.g.ret()
		return
	}
	c.g.labels.addrs[0] = c.g.pos()
	c.g.leave()
}

pub fn (mut g Gen) builtin_decl_amd64(builtin BuiltinFn) {
	g.push(.rbp)
	g.code_gen.mov_reg(Amd64Register.rbp, Amd64Register.rsp)
	local_alloc_pos := g.pos()
	g.sub(.rsp, 0)

	builtin.body(builtin, mut g)
	// 16 bytes align
	g.stack_var_pos += 7
	g.stack_var_pos /= 16
	g.stack_var_pos *= 16
	g.println('; stack frame size: ${g.stack_var_pos}')
	g.write32_at(local_alloc_pos + 3, g.stack_var_pos)

	g.labels.addrs[0] = g.pos()
	g.leave()
}

/*
pub fn (mut x Amd64) allocate_var(name string, size int, initial_val int) {
	// do nothing as interface call is crashing
}
*/

pub fn (mut g Gen) allocate_array(name string, size int, items int) int {
	pos := g.allocate_var(name, size, items)
	g.stack_var_pos += (size * items)
	return pos
}

pub fn (mut g Gen) allocate_var_two_step(name string, size int, initial_val int) int {
	// TODO: replace int by i64 or bigger
	g.allocate_var(name, size - 8, 0)
	return g.allocate_var(name, 8, initial_val)
}

pub fn (mut g Gen) allocate_var(name string, size int, initial_val int) int {
	if g.pref.arch == .arm64 {
		// TODO
		return 0
	}

	if size > 8 {
		return g.allocate_var_two_step(name, size, initial_val)
	}

	padding := (size - g.stack_var_pos % size) % size
	n := g.stack_var_pos + size + padding
	is_far_var := n > 0x80 || n < -0x7f
	far_var_offset := if is_far_var { 0x40 } else { 0 }
	// `a := 3`  => `mov DWORD [rbp-0x4],0x3`
	match size {
		1 {
			// BYTE
			g.write8(0xc6)
			g.write8(0x45 + far_var_offset)
		}
		2 {
			// WORD
			g.write8(0x66)
			g.write8(0xc7)
			g.write8(0x45 + far_var_offset)
		}
		4 {
			// DWORD
			g.write8(0xc7)
			g.write8(0x45 + far_var_offset)
		}
		8 {
			// QWORD
			g.write8(0x48)
			g.write8(0xc7)
			g.write8(0x45 + far_var_offset)
		}
		else {
			g.n_error('allocate_var: bad size ${size}')
		}
	}
	// Generate N in `[rbp-N]`
	if is_far_var {
		g.write32(int((0xffffffff - i64(n) + 1) % 0x100000000))
	} else {
		g.write8((0xff - n + 1) % 0x100)
	}
	g.stack_var_pos += size + padding
	g.var_offset[name] = g.stack_var_pos
	g.var_alloc_size[name] = size

	// Generate the value assigned to the variable
	match size {
		1 {
			g.write8(initial_val)
		}
		2 {
			g.write16(initial_val)
		}
		4 {
			g.write32(initial_val)
		}
		8 {
			g.write32(initial_val) // fixme: 64-bit segfaulting
		}
		else {
			g.n_error('allocate_var: bad size ${size}')
		}
	}

	// println('allocate_var(size=$size, initial_val=$initial_val)')
	g.println('mov [rbp-${n.hex2()}], ${initial_val} ; Allocate var `${name}`')
	return g.stack_var_pos
}

fn (mut g Gen) allocate_by_type(name string, typ ast.Type) int {
	if g.pref.arch == .arm64 {
		// TODO
		return 0
	}
	size := g.get_type_size(typ)
	align := g.get_type_align(typ)
	padding := (align - g.stack_var_pos % align) % align
	g.stack_var_pos += size + padding
	g.var_offset[name] = g.stack_var_pos
	g.var_alloc_size[name] = size

	return g.stack_var_pos
}

fn (mut g Gen) init_struct(var Var, init ast.StructInit) {
	match var {
		ast.Ident {
			var_object := g.get_var_from_ident(var)
			match var_object {
				LocalVar {
					g.init_struct(var_object as LocalVar, init)
				}
				GlobalVar {
					g.init_struct(var_object as GlobalVar, init)
				}
				Register {
					// TODO
					// g.cmp()
				}
			}
		}
		LocalVar {
			size := g.get_type_size(var.typ)

			// zero fill
			mut left := if size >= 16 {
				g.code_gen.mov(Amd64Register.rax, 0)
				g.code_gen.mov(Amd64Register.rcx, size / 8)
				g.code_gen.lea_var_to_reg(Amd64Register.rdi, var.offset)
				g.write([u8(0xf3), 0x48, 0xab])
				g.println('rep stosq')
				size % 8
			} else {
				size
			}
			if left >= 8 {
				g.mov_int_to_var(var, 0, offset: size - left, typ: ast.i64_type_idx)
				left -= 8
			}
			if left >= 4 {
				g.mov_int_to_var(var, 0, offset: size - left, typ: ast.int_type_idx)
				left -= 4
			}
			if left >= 2 {
				g.mov_int_to_var(var, 0, offset: size - left, typ: ast.i16_type_idx)
				left -= 2
			}
			if left == 1 {
				g.mov_int_to_var(var, 0, offset: size - left, typ: ast.i8_type_idx)
			}

			ts := g.table.sym(var.typ)
			match ts.info {
				ast.Struct {
					for i, f in ts.info.fields {
						if f.has_default_expr && !init.fields.map(it.name).contains(f.name) {
							offset := g.structs[var.typ.idx()].offsets[i]
							g.expr(f.default_expr)
							// TODO expr not on rax
							g.mov_reg_to_var(var, .rax, offset: offset, typ: f.typ)
						}
					}
				}
				else {}
			}
			for f in init.fields {
				field := ts.find_field(f.name) or {
					g.n_error('Could not find field `${f.name}` on init')
				}
				offset := g.structs[var.typ.idx()].offsets[field.i]

				g.expr(f.expr)
				// TODO expr not on rax
				g.mov_reg_to_var(var, .rax, offset: offset, typ: field.typ)
			}
		}
		GlobalVar {
			// TODO
		}
	}
}

fn (mut c Amd64) convert_bool_to_string(r Register) {
	reg := r as Amd64Register

	c.g.cmp_zero(reg)
	false_label := c.g.labels.new_label()
	false_cjmp_addr := c.g.cjmp(.je)
	c.g.labels.patches << LabelPatch{
		id: false_label
		pos: false_cjmp_addr
	}
	c.g.println('; jump to label ${false_label}')

	c.learel(reg, c.g.allocate_string('true', 3, .rel32))

	end_label := c.g.labels.new_label()
	end_jmp_addr := c.g.jmp(0)
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
	c.g.cmp_zero(.rax)
	skip_zero_label := c.g.labels.new_label()
	skip_zero_cjmp_addr := c.g.cjmp(.jne)
	c.g.labels.patches << LabelPatch{
		id: skip_zero_label
		pos: skip_zero_cjmp_addr
	}
	c.g.println('; jump to label ${skip_zero_label}')

	// handle zeros seperately
	// c.g.mov_int_to_var(LocalVar{buffer, ast.u8_type_idx, ''}, '0'[0])

	c.g.write8(0xc6)
	c.g.write8(0x07)
	c.g.write8(0x30)
	c.g.println("mov BYTE PTR [rdi], '0'")

	end_label := c.g.labels.new_label()
	end_jmp_addr := c.g.jmp(0)
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
	c.g.cmp_zero(.rax)
	skip_minus_label := c.g.labels.new_label()
	skip_minus_cjmp_addr := c.g.cjmp(.jge)
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

	c.g.neg(.rax) // negate our integer to make it positive
	c.g.inc(.rdi) // increment rdi to skip the `-` character
	c.g.labels.addrs[skip_minus_label] = c.g.pos()
	c.g.println('; label ${skip_minus_label}')

	c.mov_reg_amd64(.r12, .rdi) // copy the buffer position to r12

	loop_label := c.g.labels.new_label()
	loop_start := c.g.pos()
	c.g.println('; label ${loop_label}')

	c.g.push(.rax)

	c.mov(Amd64Register.rdx, 0)
	c.mov(Amd64Register.rbx, 10)
	c.g.div_reg(.rax, .rbx)
	c.g.add8(.rdx, '0'[0])

	c.g.write8(0x66)
	c.g.write8(0x89)
	c.g.write8(0x17)
	c.g.println('mov BYTE PTR [rdi], rdx')

	// divide the integer in rax by 10 for next iteration
	c.g.pop(.rax)
	c.mov(Amd64Register.rbx, 10)
	c.g.cdq()
	c.g.write8(0x48)
	c.g.write8(0xf7)
	c.g.write8(0xfb)
	c.g.println('idiv rbx')

	// go to the next character
	c.g.inc(.rdi)

	// if the number in rax still isn't zero, repeat
	c.g.cmp_zero(.rax)
	loop_cjmp_addr := c.g.cjmp(.jg)
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

	c.g.sub8(.rdi, 0x2)
	c.g.cmp_reg(.rdi, .rsi)

	c.g.write8(0x7e)
	c.g.write8(0x0a)
	c.g.println('jle 0x1e')

	c.g.write8(0x86)
	c.g.write8(0x07)
	c.g.println('xchg BYTE PTR [rdi], al')

	c.g.write8(0x86)
	c.g.write8(0x06)
	c.g.println('xchg BYTE PTR [rsi], al')

	c.g.std()

	c.g.write8(0xaa)
	c.g.println('stos BYTE PTR es:[rdi], al')

	c.g.cld()

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
	c.g.push(.rax)

	mut else_label := 0
	for i, branch in expr.branches {
		if branch.is_else {
			else_label = branch_labels[i]
		} else {
			for cond in branch.exprs {
				match cond {
					ast.RangeExpr {
						c.g.pop(.rdx)
						c.g.expr(cond.low)
						c.g.cmp_reg(.rax, .rdx)
						c.g.write([u8(0x0f), 0x9e, 0xc3])
						c.g.println('setle bl')
						c.g.expr(cond.high)
						c.g.cmp_reg(.rax, .rdx)
						c.g.write([u8(0x0f), 0x9d, 0xc1])
						c.g.println('setge cl')
						c.g.write([u8(0x20), 0xcb])
						c.g.println('and bl, cl')
						c.g.write([u8(0x84), 0xdb])
						c.g.println('test bl, bl')
						then_addr := c.g.cjmp(.jne)
						c.g.labels.patches << LabelPatch{
							id: branch_labels[i]
							pos: then_addr
						}
						c.g.push(.rdx)
					}
					else {
						c.g.expr(cond)
						c.g.pop(.rdx)
						c.g.cmp_reg(.rax, .rdx)
						then_addr := c.g.cjmp(.je)
						c.g.labels.patches << LabelPatch{
							id: branch_labels[i]
							pos: then_addr
						}
						c.g.push(.rdx)
					}
				}
			}
		}
	}
	c.g.pop(.rdx)
	else_addr := c.g.jmp(0)
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
			pos: c.g.jmp(0)
		}
	}
	c.g.labels.addrs[end_label] = c.g.pos()
}

fn (mut g Gen) mov_ssereg_to_var(var Var, reg Amd64SSERegister, config VarConfig) {
	match var {
		ast.Ident {
			var_object := g.get_var_from_ident(var)
			match var_object {
				LocalVar {
					g.mov_ssereg_to_var(var_object as LocalVar, reg, config)
				}
				GlobalVar {
					g.mov_ssereg_to_var(var_object as GlobalVar, reg, config)
				}
				Register {}
			}
		}
		LocalVar {
			offset := var.offset - config.offset
			is_far_var := offset > 0x80 || offset < -0x7f
			typ := if config.typ == 0 { var.typ } else { config.typ }

			far_var_offset := if is_far_var { 0x40 } else { 0 }
			g.write8(if typ == ast.f32_type_idx { 0xf3 } else { 0xf2 })
			if int(reg) >= int(Amd64SSERegister.xmm8) {
				g.write8(0x44)
			}
			g.write16(0x110f)
			g.write8(0x45 + int(reg) % 8 * 8 + far_var_offset)
			if is_far_var {
				g.write32(int((0xffffffff - i64(offset) + 1) % 0x100000000))
			} else {
				g.write8((0xff - offset + 1) % 0x100)
			}
			inst := if typ == ast.f32_type_idx { 'movss' } else { 'movsd' }
			g.println('${inst} [rbp-${offset.hex2()}], ${reg}')
		}
		GlobalVar {
			// TODO
		}
	}
}

fn (mut g Gen) mov_var_to_ssereg(reg Amd64SSERegister, var Var, config VarConfig) {
	match var {
		ast.Ident {
			var_object := g.get_var_from_ident(var)
			match var_object {
				LocalVar {
					g.mov_var_to_ssereg(reg, var_object as LocalVar, config)
				}
				GlobalVar {
					g.mov_var_to_ssereg(reg, var_object as GlobalVar, config)
				}
				Register {}
			}
		}
		LocalVar {
			offset := var.offset - config.offset
			is_far_var := offset > 0x80 || offset < -0x7f
			typ := if config.typ == 0 { var.typ } else { config.typ }

			far_var_offset := if is_far_var { 0x40 } else { 0 }
			g.write8(if typ == ast.f32_type_idx { 0xf3 } else { 0xf2 })
			if int(reg) >= int(Amd64SSERegister.xmm8) {
				g.write8(0x44)
			}
			g.write16(0x100f)
			g.write8(0x45 + int(reg) % 8 * 8 + far_var_offset)
			if is_far_var {
				g.write32(int((0xffffffff - i64(offset) + 1) % 0x100000000))
			} else {
				g.write8((0xff - offset + 1) % 0x100)
			}
			inst := if typ == ast.f32_type_idx { 'movss' } else { 'movsd' }
			g.println('${inst} ${reg}, [rbp-${offset.hex2()}]')
		}
		GlobalVar {
			// TODO
		}
	}
}

fn (mut g Gen) mov_ssereg(a Amd64SSERegister, b Amd64SSERegister) {
	g.write8(0xf2)
	if int(a) >= int(Amd64SSERegister.xmm8) || int(b) >= int(Amd64SSERegister.xmm8) {
		g.write8(0x40 + int(a) / 8 * 4 + int(b) / 8)
	}
	g.write16(0x100f)
	g.write8(0xc0 + int(a) % 8 * 8 + int(b) % 8)
	g.println('movsd ${a}, ${b}')
}

fn (mut g Gen) mov_ssereg_to_reg(a Amd64Register, b Amd64SSERegister, typ ast.Type) {
	g.write8(0x66)
	rex_base, inst := if typ == ast.f32_type_idx {
		0x40, 'movd'
	} else {
		0x48, 'movq'
	}
	if rex_base == 0x48 || int(a) >= int(Amd64Register.r8) || int(b) >= int(Amd64SSERegister.xmm8) {
		g.write8(rex_base + int(a) / 8 * 4 + int(b) / 8)
	}
	g.write16(0x7e0f)
	g.write8(0xc0 + int(a) % 8 * 8 + int(b) % 8)
	g.println('${inst} ${a}, ${b}')
}

fn (mut g Gen) mov_reg_to_ssereg(a Amd64SSERegister, b Amd64Register, typ ast.Type) {
	g.write8(0x66)
	rex_base, inst := if typ == ast.f32_type_idx {
		0x40, 'movd'
	} else {
		0x48, 'movq'
	}
	if rex_base == 0x48 || int(a) >= int(Amd64SSERegister.xmm8) || int(b) >= int(Amd64Register.r8) {
		g.write8(rex_base + int(a) / 8 * 4 + int(b) / 8)
	}
	g.write16(0x6e0f)
	g.write8(0xc0 + int(a) % 8 * 8 + int(b) % 8)
	g.println('${inst} ${a}, ${b}')
}

fn (mut g Gen) mov_deref_sse(a Amd64SSERegister, b Amd64Register, typ ast.Type) {
	op, inst, len := if typ == ast.f32_type_idx {
		0xf3, 'movss', 'DWORD'
	} else {
		0xf2, 'movsd', 'QWORD'
	}
	g.write8(op)
	if int(a) >= int(Amd64SSERegister.xmm8) || int(b) >= int(Amd64Register.r8) {
		g.write8(0x40 + int(a) / 8 * 4 + int(b) / 8)
	}
	g.write16(0x100f)
	g.write8(int(a) % 8 * 8 + int(b) % 8)
	g.println('${inst} ${a}, ${len} PTR [${b}]')
}

fn (mut g Gen) add_sse(a Amd64SSERegister, b Amd64SSERegister, typ ast.Type) {
	g.write8(if typ == ast.f32_type_idx { 0xf3 } else { 0xf2 })
	if int(a) >= int(Amd64SSERegister.xmm8) || int(b) >= int(Amd64SSERegister.xmm8) {
		g.write8(0x40 + int(a) / 8 * 4 + int(b) / 8)
	}
	g.write16(0x580f)
	g.write8(0xc0 + int(a) % 8 * 8 + int(b) % 8)
	inst := if typ == ast.f32_type_idx { 'addss' } else { 'addsd' }
	g.println('${inst} ${a}, ${b}')
}

fn (mut g Gen) sub_sse(a Amd64SSERegister, b Amd64SSERegister, typ ast.Type) {
	g.write8(if typ == ast.f32_type_idx { 0xf3 } else { 0xf2 })
	if int(a) >= int(Amd64SSERegister.xmm8) || int(b) >= int(Amd64SSERegister.xmm8) {
		g.write8(0x40 + int(a) / 8 * 4 + int(b) / 8)
	}
	g.write16(0x5c0f)
	g.write8(0xc0 + int(a) % 8 * 8 + int(b) % 8)
	inst := if typ == ast.f32_type_idx { 'subss' } else { 'subsd' }
	g.println('${inst} ${a}, ${b}')
}

fn (mut g Gen) mul_sse(a Amd64SSERegister, b Amd64SSERegister, typ ast.Type) {
	g.write8(if typ == ast.f32_type_idx { 0xf3 } else { 0xf2 })
	if int(a) >= int(Amd64SSERegister.xmm8) || int(b) >= int(Amd64SSERegister.xmm8) {
		g.write8(0x40 + int(a) / 8 * 4 + int(b) / 8)
	}
	g.write16(0x590f)
	g.write8(0xc0 + int(a) % 8 * 8 + int(b) % 8)
	inst := if typ == ast.f32_type_idx { 'mulss' } else { 'mulsd' }
	g.println('${inst} ${a}, ${b}')
}

fn (mut g Gen) div_sse(a Amd64SSERegister, b Amd64SSERegister, typ ast.Type) {
	g.write8(if typ == ast.f32_type_idx { 0xf3 } else { 0xf2 })
	if int(a) >= int(Amd64SSERegister.xmm8) || int(b) >= int(Amd64SSERegister.xmm8) {
		g.write8(0x40 + int(a) / 8 * 4 + int(b) / 8)
	}
	g.write16(0x5e0f)
	g.write8(0xc0 + int(a) % 8 * 8 + int(b) % 8)
	inst := if typ == ast.f32_type_idx { 'divss' } else { 'divsd' }
	g.println('${inst} ${a}, ${b}')
}

fn (mut g Gen) cmp_sse(a Amd64SSERegister, b Amd64SSERegister, typ ast.Type) {
	if typ != ast.f32_type_idx {
		g.write8(0x66)
	}
	if int(a) >= int(Amd64SSERegister.xmm8) || int(b) >= int(Amd64SSERegister.xmm8) {
		g.write8(0x40 + int(a) / 8 * 4 + int(b) / 8)
	}
	g.write16(0x2e0f)
	g.write8(0xc0 + int(a) % 8 * 8 + int(b) % 8)
	inst := if typ == ast.f32_type_idx { 'ucomiss' } else { 'ucomisd' }
	g.println('${inst} ${a}, ${b}')
}

pub fn (mut g Gen) push_sse(reg Amd64SSERegister) {
	g.write32(0x08ec8348)
	g.println('sub rsp, 0x8')
	g.write8(0xf2)
	if int(reg) >= int(Amd64SSERegister.xmm8) {
		g.write8(0x44)
	}
	g.write16(0x110f)
	g.write8(0x04 + int(reg) % 8 * 8)
	g.write8(0x24)
	g.println('movsd [rsp], ${reg}')
	if mut g.code_gen is Amd64 {
		g.code_gen.is_16bit_aligned = !g.code_gen.is_16bit_aligned
	}
	g.println('; push ${reg}')
}

pub fn (mut g Gen) pop_sse(reg Amd64SSERegister) {
	g.write8(0xf2)
	if int(reg) >= int(Amd64SSERegister.xmm8) {
		g.write8(0x44)
	}
	g.write16(0x100f)
	g.write8(0x04 + int(reg) % 8 * 8)
	g.write8(0x24)
	g.println('movsd ${reg}, [rsp]')
	g.write32(0x08c48348)
	g.println('add rsp, 0x8')
	if mut g.code_gen is Amd64 {
		g.code_gen.is_16bit_aligned = !g.code_gen.is_16bit_aligned
	}
	g.println('; pop ${reg}')
}

fn (mut g Gen) gen_cast_expr_amd64(expr ast.CastExpr) {
	g.expr(expr.expr)
	if expr.typ != expr.expr_type {
		if expr.typ.is_pure_float() && expr.expr_type.is_pure_float() {
			from_size := g.get_type_size(expr.expr_type)
			to_size := g.get_type_size(expr.typ)
			if from_size == 4 && to_size == 8 {
				g.write32(0xc05a0ff3)
				g.println('cvtss2sd xmm0, xmm0')
			}
			if from_size == 8 && to_size == 4 {
				g.write32(0xc05a0ff2)
				g.println('cvtsd2ss xmm0, xmm0')
			}
		} else if expr.typ.is_pure_float() {
			if g.get_type_size(expr.expr_type) == 8 && !expr.expr_type.is_signed() {
				label1 := g.labels.new_label()
				label2 := g.labels.new_label()
				g.test_reg(.rax)
				addr1 := g.cjmp(.js)
				g.labels.patches << LabelPatch{
					id: label1
					pos: addr1
				}
				// if castee is in the range of i64
				match g.get_type_size(expr.typ) {
					4 {
						g.write([u8(0xf3), 0x48, 0x0f, 0x2a, 0xc0])
						g.println('cvtsi2ss xmm0, rax')
					}
					8 {
						g.write([u8(0xf2), 0x48, 0x0f, 0x2a, 0xc0])
						g.println('cvtsi2sd xmm0, rax')
					}
					else {}
				}
				addr2 := g.jmp(0)
				g.labels.patches << LabelPatch{
					id: label2
					pos: addr2
				}
				g.labels.addrs[label1] = g.pos()
				// if castee has the leftmost bit
				g.code_gen.mov_reg(Amd64Register.rdx, Amd64Register.rax)
				g.write([u8(0x48), 0xd1, 0xe8])
				g.println('shr rax')
				g.write([u8(0x83), 0xe2, 0x01])
				g.println('and edx, 0x1')
				g.bitor_reg(.rax, .rdx)
				match g.get_type_size(expr.typ) {
					4 {
						g.write([u8(0xf3), 0x48, 0x0f, 0x2a, 0xc0])
						g.println('cvtsi2ss xmm0, rax')
					}
					8 {
						g.write([u8(0xf2), 0x48, 0x0f, 0x2a, 0xc0])
						g.println('cvtsi2sd xmm0, rax')
					}
					else {}
				}
				g.add_sse(.xmm0, .xmm0, expr.typ)
				g.labels.addrs[label2] = g.pos()
			} else {
				match g.get_type_size(expr.typ) {
					4 {
						g.write([u8(0xf3), 0x48, 0x0f, 0x2a, 0xc0])
						g.println('cvtsi2ss xmm0, rax')
					}
					8 {
						g.write([u8(0xf2), 0x48, 0x0f, 0x2a, 0xc0])
						g.println('cvtsi2sd xmm0, rax')
					}
					else {}
				}
			}
		} else if expr.expr_type.is_pure_float() {
			if g.get_type_size(expr.typ) == 8 && !expr.typ.is_signed() {
				label1 := g.labels.new_label()
				label2 := g.labels.new_label()
				// TODO constant
				g.movabs(.rdx, i64(u64(0x4000000000000000)))
				match g.get_type_size(expr.expr_type) {
					4 {
						g.write([u8(0xf3), 0x48, 0x0f, 0x2a, 0xca])
						g.println('cvtsi2ss xmm1, rdx')
					}
					8 {
						g.write([u8(0xf2), 0x48, 0x0f, 0x2a, 0xca])
						g.println('cvtsi2sd xmm1, rdx')
					}
					else {}
				}
				g.add_sse(.xmm1, .xmm1, expr.expr_type)
				g.add_reg(.rdx, .rdx)
				g.cmp_sse(.xmm0, .xmm1, expr.expr_type)
				addr1 := g.cjmp(.jnb)
				g.labels.patches << LabelPatch{
					id: label1
					pos: addr1
				}
				match g.get_type_size(expr.expr_type) {
					4 {
						g.write([u8(0xf3), 0x48, 0x0f, 0x2c, 0xc0])
						g.println('cvttss2si rax, xmm0')
					}
					8 {
						g.write([u8(0xf2), 0x48, 0x0f, 0x2c, 0xc0])
						g.println('cvttsd2si rax, xmm0')
					}
					else {}
				}
				addr2 := g.jmp(0)
				g.labels.patches << LabelPatch{
					id: label2
					pos: addr2
				}
				g.labels.addrs[label1] = g.pos()
				g.sub_sse(.xmm0, .xmm1, expr.expr_type)
				match g.get_type_size(expr.expr_type) {
					4 {
						g.write([u8(0xf3), 0x48, 0x0f, 0x2c, 0xc0])
						g.println('cvttss2si rax, xmm0')
					}
					8 {
						g.write([u8(0xf2), 0x48, 0x0f, 0x2c, 0xc0])
						g.println('cvttsd2si rax, xmm0')
					}
					else {}
				}
				g.add_reg(.rax, .rdx)
				g.labels.addrs[label2] = g.pos()
			} else {
				match g.get_type_size(expr.expr_type) {
					4 {
						g.write([u8(0xf3), 0x48, 0x0f, 0x2c, 0xc0])
						g.println('cvttss2si rax, xmm0')
					}
					8 {
						g.write([u8(0xf2), 0x48, 0x0f, 0x2c, 0xc0])
						g.println('cvttsd2si rax, xmm0')
					}
					else {}
				}
			}
		} else {
			g.mov_extend_reg(.rax, .rax, expr.typ)
		}
	}
}

// Temporary!
fn (mut c Amd64) adr(r Arm64Register, delta int) {
	panic('`adr` instruction not supported with amd64')
}
