module native

import v.ast
import v.token

pub struct Amd64 {
mut:
	g &Gen
	// arm64 specific stuff for code generation
}

// The registers are ordered for faster generation
// push rax => 50
// push rcx => 51 etc
enum Register {
	rax
	rcx
	rdx
	rbx
	rsp
	rbp
	rsi
	rdi
	eax
	edi
	edx
	r8
	r9
	r10
	r11
	r12
	r13
	r14
	r15
}

const (
	fn_arg_registers = [Register.rdi, .rsi, .rdx, .rcx, .r8, .r9]
	amd64_cpuregs    = ['eax', 'ecx', 'edx', 'ebx', 'esp', 'ebp', 'esi', 'edi']
)

fn (mut g Gen) dec(reg Register) {
	g.write16(0xff48)
	match reg {
		.rax { g.write8(0xc8) }
		.rbx { g.write8(0xcb) }
		.rcx { g.write8(0xc9) }
		.rsi { g.write8(0xce) }
		.rdi { g.write8(0xcf) }
		.r12 { g.write8(0xc4) }
		else { panic('unhandled inc $reg') }
	}
	g.println('dec $reg')
}

[inline]
fn byt(n int, s int) u8 {
	return u8((n >> (s * 8)) & 0xff)
}

fn (mut g Gen) inc(reg Register) {
	g.write8(0x48)
	g.write8(0xff)
	g.write8(0xc0 + int(reg))
	g.println('inc $reg')
}

fn (mut g Gen) neg(reg Register) {
	g.write8(0x48)
	g.write8(0xf7)
	match reg {
		.rax { g.write8(0xd8) }
		else { panic('unhandled neg $reg') }
	}
	g.println('neg $reg')
}

fn (mut g Gen) cmp(reg Register, size Size, val i64) {
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
	g.println('cmp $reg, $val')
}

// `cmp rax, rbx`
fn (mut g Gen) cmp_reg(reg Register, reg2 Register) {
	match reg {
		.rax {
			match reg2 {
				.rbx {
					g.write([u8(0x48), 0x39, 0xd8])
				}
				else {
					g.n_error('Cannot compare $reg and $reg2')
				}
			}
		}
		.rbx {
			match reg2 {
				.rax {
					g.write([u8(0x48), 0x39, 0xc3])
				}
				else {
					g.n_error('Cannot compare $reg and $reg2')
				}
			}
		}
		.rdi {
			match reg2 {
				.rsi {
					g.write([u8(0x48), 0x39, 0xf7])
				}
				else {
					g.n_error('Cannot compare $reg and $reg2')
				}
			}
		}
		else {
			g.n_error('Cannot compare $reg and $reg2')
		}
	}
	g.println('cmp $reg, $reg2')
}

// cmp $reg, 0
fn (mut g Gen) cmp_zero(reg Register) {
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
			g.n_error('unhandled cmp $reg, 0')
		}
	}

	g.write8(0x00)
	g.println('cmp $reg, 0')
}

fn (mut g Gen) cmp_var_reg(var_name string, reg Register) {
	g.write8(0x48) // 83 for 1 byte?
	g.write8(0x39)
	g.write8(0x45)
	offset := g.get_var_offset(var_name)
	g.write8(0xff - offset + 1)
	g.println('cmp var `$var_name`, $reg')
}

fn (mut g Gen) cmp_var(var_name string, val int) {
	g.write8(0x81) // 83 for 1 byte?
	g.write8(0x7d)
	offset := g.get_var_offset(var_name)
	g.write8(0xff - offset + 1)
	g.write32(val)
	g.println('cmp var `$var_name` $val')
}

// `sub DWORD [rbp-0x4], 1`
fn (mut g Gen) dec_var(var_name string) {
	g.write16(0x6d81) // 83 for 1 byte
	offset := g.get_var_offset(var_name)
	g.write8(0xff - offset + 1)
	g.write32(1)
	g.println('dec_var `$var_name`')
}

// `add DWORD [rbp-0x4], 1`
fn (mut g Gen) inc_var(var_name string) {
	g.write16(0x4581) // 83 for 1 byte
	offset := g.get_var_offset(var_name)
	g.write8(0xff - offset + 1)
	g.write32(1)
	g.println('inc_var `$var_name`')
}

enum JumpOp {
	je = 0x840f
	jne = 0x850f
	jg = 0x8f0f
	jge = 0x8d0f
	lt = 0x8c0f
	jle = 0x8e0f
}

fn (mut g Gen) cjmp(op JumpOp) int {
	g.write16(u16(op))
	pos := g.pos()
	g.write32(placeholder)
	g.println('$op')
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

fn (mut g Gen) mov32(reg Register, val int) {
	match reg {
		.rax {
			g.write8(0xb8)
		}
		.rdi {
			g.write8(0xbf)
		}
		.rcx {
			g.write8(0xb9)
		}
		else {
			panic('unhandled mov32 $reg')
		}
	}
	g.write32(val)
	g.println('mov32 $reg, $val')
}

fn (mut g Gen) mov64(reg Register, val i64) {
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
			g.write32(i32(int(val)))
			g.println('mov32 $reg, $val')
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
			eprintln('unhandled mov64 $reg')
		}
	}
	g.write64(val)
	g.println('mov64 $reg, $val')
}

fn (mut g Gen) movabs(reg Register, val i64) {
	match reg {
		.rsi {
			g.write8(0x48)
			g.write8(0xbe)
		}
		else {
			panic('unhandled movabs $reg, $val')
		}
	}

	g.write64(val)
	g.println('movabs $reg, $val')
}

fn (mut g Gen) mov_reg_to_var(var_offset int, reg Register) {
	if g.pref.arch != .amd64 {
		panic('invalid arch for mov_reg_to_var')
	}
	// 89 7d fc     mov DWORD PTR [rbp-0x4],edi
	match reg {
		.rax {
			g.write8(0x48)
		}
		else {}
	}
	g.write8(0x89)
	match reg {
		.eax, .rax { g.write8(0x45) }
		.edi, .rdi { g.write8(0x7d) }
		.rsi { g.write8(0x75) }
		.rdx { g.write8(0x55) }
		.rcx { g.write8(0x4d) }
		else { g.n_error('mov_from_reg $reg') }
	}
	g.write8(0xff - var_offset + 1)
	g.println('mov DWORD PTR[rbp-$var_offset.hex2()],$reg')
}

fn (mut g Gen) mov_int_to_var(var_offset int, size Size, integer int) {
	var_addr := 0xff - var_offset + 1

	match size {
		._8 {
			g.write8(0xc6)
			g.write8(0x45)
			g.write8(var_addr)
			g.write8(u8(integer))
			g.println('mov BYTE PTR[rbp-$var_offset.hex2()], $integer')
		}
		else {
			g.n_error('unhandled mov int')
		}
	}
}

fn (mut g Gen) lea_var_to_reg(reg Register, var_offset int) {
	match reg {
		.rax, .rbx, .rsi, .rdi {
			g.write8(0x48)
		}
		else {}
	}
	g.write8(0x8d)
	match reg {
		.eax, .rax { g.write8(0x45) }
		.edi, .rdi { g.write8(0x7d) }
		.rsi { g.write8(0x75) }
		.rdx { g.write8(0x55) }
		.rbx { g.write8(0x5d) }
		.rcx { g.write8(0x4d) }
		else { g.n_error('lea_var_to_reg $reg') }
	}
	g.write8(0xff - var_offset + 1)
	g.println('lea $reg, [rbp-$var_offset.hex2()]')
}

fn (mut g Gen) mov_var_to_reg(reg Register, var_offset int) {
	// 8b 7d f8    mov edi,DWORD PTR [rbp-0x8]
	match reg {
		.rax, .rbx, .rsi {
			g.write8(0x48)
		}
		else {}
	}
	g.write8(0x8b)
	match reg {
		.eax, .rax { g.write8(0x45) }
		.edi, .rdi { g.write8(0x7d) }
		.rsi { g.write8(0x75) }
		.rdx { g.write8(0x55) }
		.rbx { g.write8(0x5d) }
		.rcx { g.write8(0x4d) }
		else { g.n_error('mov_var_to_reg $reg') }
	}
	g.write8(0xff - var_offset + 1)
	g.println('mov $reg,DWORD PTR[rbp-$var_offset.hex2()]')
}

fn (mut g Gen) call(addr int) {
	if g.pref.arch == .arm64 {
		g.bl()
		return
	}
	// Need to calculate the difference between current position (position after the e8 call)
	// and the function to call.
	// +5 is to get the posistion "e8 xx xx xx xx"
	// Not sure about the -1.
	rel := 0xffffffff - (g.buf.len + 5 - addr - 1)
	// println('call addr=$addr.hex2() rel_addr=$rel.hex2() pos=$g.buf.len')
	g.write8(0xe8)
	g.write32(rel)
	g.println('call $addr')
}

fn (mut g Gen) syscall() {
	// g.write(0x050f)
	g.write8(0x0f)
	g.write8(0x05)
	g.println('syscall')
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

pub fn (mut g Gen) push(reg Register) {
	if int(reg) < int(Register.r8) {
		g.write8(0x50 + int(reg))
	} else {
		g.write8(0x41)
		g.write8(0x50 + int(reg) - 8)
	}
	/*
	match reg {
		.rbp { g.write8(0x55) }
		else {}
	}
	*/
	g.println('push $reg')
}

pub fn (mut g Gen) pop(reg Register) {
	g.write8(0x58 + int(reg))
	// TODO r8...
	g.println('pop $reg')
}

pub fn (mut g Gen) sub32(reg Register, val int) {
	g.write8(0x48)
	g.write8(0x81)
	g.write8(0xe8 + int(reg)) // TODO rax is different?
	g.write32(val)
	g.println('sub32 $reg,$val.hex2()')
}

pub fn (mut g Gen) sub8(reg Register, val int) {
	g.write8(0x48)
	g.write8(0x83)
	g.write8(0xe8 + int(reg)) // TODO rax is different?
	g.write8(val)
	g.println('sub8 $reg,$val.hex2()')
}

pub fn (mut g Gen) sub(reg Register, val int) {
	g.write8(0x48)
	g.write8(0x81)
	g.write8(0xe8 + int(reg)) // TODO rax is different?
	g.write32(val)
	g.println('add $reg,$val.hex2()')
}

pub fn (mut g Gen) add(reg Register, val int) {
	if reg != .rax {
		panic('add only works with .rax')
	}
	g.write8(0x48)
	g.write8(0x05)
	g.write32(val)
	g.println('add $reg,$val.hex2()')
}

pub fn (mut g Gen) add8(reg Register, val int) {
	g.write8(0x48)
	g.write8(0x83)
	g.write8(0xc0 + int(reg))
	g.write8(val)
	g.println('add8 $reg,$val.hex2()')
}

fn (mut g Gen) add8_var(reg Register, var_offset int) {
	g.write8(0x03)
	match reg {
		.eax, .rax { g.write8(0x45) }
		else { g.n_error('add8_var') }
	}
	g.write8(0xff - var_offset + 1)
	g.println('add8 $reg,DWORD PTR[rbp-$var_offset.hex2()]')
}

fn (mut g Gen) sub8_var(reg Register, var_offset int) {
	g.write8(0x2b)
	match reg {
		.eax, .rax { g.write8(0x45) }
		else { g.n_error('sub8_var') }
	}
	g.write8(0xff - var_offset + 1)
	g.println('sub8 $reg,DWORD PTR[rbp-$var_offset.hex2()]')
}

fn (mut g Gen) div8_var(reg Register, var_offset int) {
	if reg == .rax || reg == .eax {
		g.mov_var_to_reg(.rbx, var_offset)
		g.div_reg(.rax, .rbx)
		g.mov_reg_to_var(var_offset, .rax)
	} else {
		panic('div8_var invalid source register')
	}
}

fn (mut g Gen) mul8_var(reg Register, var_offset int) {
	g.write8(0x0f)
	g.write8(0xaf)
	match reg {
		.eax, .rax { g.write8(0x45) }
		else { g.n_error('mul8_var') }
	}
	g.write8(0xff - var_offset + 1)
	g.println('mul8 $reg,DWORD PTR[rbp-$var_offset.hex2()]')
}

fn (mut g Gen) leave() {
	g.write8(0xc9)
	g.println('leave')
}

// returns label's relative address
pub fn (mut g Gen) gen_loop_start(from int) int {
	g.mov(.r12, from)
	label := g.buf.len
	g.inc(.r12)
	return label
}

pub fn (mut g Gen) gen_loop_end(to int, label int) {
	g.cmp(.r12, ._8, to)
	g.jl(label)
}

pub fn (mut g Gen) allocate_string(s string, opsize int, typ RelocType) int {
	str_pos := g.buf.len + opsize
	g.strs << String{s, str_pos, typ}
	return str_pos
}

pub fn (mut g Gen) var_zero(vo int, size int) {
	g.mov32(.rcx, size)
	g.lea_var_to_reg(.rdi, vo)
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

pub fn (mut g Gen) xor(r Register, v int) {
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

pub fn (mut g Gen) test_reg(r Register) {
	match r {
		.rdi {
			g.write8(0x48)
			g.write8(0x85)
			g.write8(0xff)
			g.println('test rdi, rdi')
		}
		else {
			panic('unhandled test $r, $r')
		}
	}
}

// return length in .rax of string pointed by given register
pub fn (mut g Gen) inline_strlen(r Register) {
	g.mov_reg(.rdi, r)
	g.mov(.rcx, -1)
	g.mov(.eax, 0)
	g.cld_repne_scasb()
	g.xor(.rcx, -1)
	g.dec(.rcx)
	g.mov_reg(.rax, .rcx)
	g.println('strlen rax, $r')
}

// TODO: strlen of string at runtime
pub fn (mut g Gen) gen_print_reg(r Register, n int, fd int) {
	mystrlen := true // if n < 0 maybe?
	g.mov_reg(.rsi, r)
	if mystrlen {
		g.inline_strlen(.rsi)
		g.mov_reg(.rdx, .rax)
	} else {
		g.mov(.edx, n)
	}
	g.mov(.eax, g.nsyscall_write())
	g.mov(.edi, fd)
	g.syscall()
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
		g.mov(.rcx, -11)
		g.apicall('GetStdHandle')
		g.mov_reg(.rcx, .rax)
		// g.mov64(.rdx, g.allocate_string(s, 3))
		g.lea(.rdx, g.allocate_string(s, 3, .abs64))
		g.mov(.r8, s.len) // string length
		g.write([u8(0x4c), 0x8d, 0x4c, 0x24, 0x20]) // lea r9, [rsp+0x20]
		g.write([u8(0x48), 0xc7, 0x44, 0x24, 0x20])
		g.write32(0) // mov qword[rsp+0x20], 0
		// g.mov(.r9, rsp+0x20)
		g.apicall('WriteFile')
	} else {
		g.mov(.eax, g.nsyscall_write())
		g.mov(.edi, fd)
		g.learel(.rsi, g.allocate_string(s, 3, .rel32)) // for rsi its 2
		g.mov(.edx, s.len) // len
		g.syscall()
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

pub fn (mut a Amd64) gen_exit(mut g Gen, node ast.Expr) {
	g.gen_amd64_exit(node)
}

pub fn (mut g Gen) gen_amd64_exit(expr ast.Expr) {
	// ret value
	match expr {
		ast.CallExpr {
			right := expr.return_type
			g.n_error('native exit builtin: Unsupported call $right')
		}
		ast.Ident {
			var_offset := g.get_var_offset(expr.name)
			g.mov_var_to_reg(.edi, var_offset)
		}
		ast.IntegerLiteral {
			g.mov(.edi, expr.val.int())
		}
		else {
			g.n_error('native builtin exit expects a numeric argument')
		}
	}
	if g.pref.os == .windows {
		g.mov_reg(.rcx, .rdi)
		g.apicall('ExitProcess')
	} else {
		g.mov(.eax, g.nsyscall_exit())
		g.syscall()
	}
	g.trap() // should never be reached, just in case
}

fn (mut g Gen) relpc(dst Register, src Register) {
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

fn (mut g Gen) learel(reg Register, val int) {
	g.write8(0x48)
	g.write8(0x8d)
	match reg {
		.rax {
			g.write8(0x05)
		}
		.rsi {
			g.write8(0x35)
		}
		.rcx {
			g.write8(0x0d)
		}
		else {
			g.n_error('learel must use rsi, rcx or rax')
		}
	}
	g.write32(val)
	g.println('lea $reg, rip + $val')
}

fn (mut g Gen) lea(reg Register, val int) {
	g.write8(0x48)
	g.write8(0x8d)
	g.write8(0x15)
	g.write32(val)
	g.println('lea $reg, $val')
}

fn (mut g Gen) mov(reg Register, val int) {
	if val == -1 {
		match reg {
			.rax {
				g.write8(0x48)
				g.write8(0xc7)
				g.write8(0xc0)
				g.write32(-1)
				g.println('mov $reg, $val')
			}
			.rcx {
				if val == -1 {
					g.write8(0x48)
					g.write8(0xc7)
					g.write8(0xc1)
					g.write32(-1)
				} else {
					g.write8(0xff)
					g.write8(0xff) // mov rcx 0xffff5
				}
				g.println('mov $reg, $val')
			}
			else {
				g.n_error('unhandled mov $reg, -1')
			}
		}
		g.println('mov $reg, $val')
		return
	}
	if val == 0 {
		// Optimise to xor reg, reg when val is 0
		match reg {
			.eax, .rax {
				g.write8(0x31)
				g.write8(0xc0)
			}
			.edi, .rdi {
				g.write8(0x31)
				g.write8(0xff)
			}
			.rcx {
				g.write8(0x48)
				g.write8(0x31)
				g.write8(0xc7)
			}
			.rdx {
				g.write8(0x48)
				g.write8(0x31)
				g.write8(0xd2)
			}
			.edx {
				g.write8(0x31)
				g.write8(0xd2)
			}
			.rsi {
				g.write8(0x48)
				g.write8(0x31)
				g.write8(0xf6)
			}
			.r12 {
				g.write8(0x4d)
				g.write8(0x31)
				g.write8(0xe4)
			}
			else {
				g.n_error('unhandled mov $reg, $reg')
			}
		}
		g.println('xor $reg, $reg')
	} else {
		match reg {
			.eax, .rax {
				g.write8(0xb8)
			}
			.edi, .rdi {
				g.write8(0xbf)
			}
			.rcx {
				g.write8(0x48)
				g.write8(0xc7)
				g.write8(0xc1)
			}
			.r8 {
				g.write8(0x41)
				g.write8(0xb8)
			}
			.r9 {
				g.write8(0xb9)
			}
			.rdx, .edx {
				g.write8(0xba)
			}
			.rsi {
				//	g.write8(0x48) // its 32bit!
				g.write8(0xbe)
			}
			.r12 {
				g.write8(0x41)
				g.write8(0xbc) // r11 is 0xbb etc
			}
			.rbx {
				g.write8(0xbb)
			}
			else {
				g.n_error('unhandled mov $reg')
			}
		}
		g.write32(val)
		g.println('mov $reg, $val')
	}
}

fn (mut g Gen) mul_reg(a Register, b Register) {
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
		else {
			panic('unhandled div $a')
		}
	}
	g.println('mul $a')
}

fn (mut g Gen) imul_reg(r Register) {
	match r {
		.rsi {
			g.write8(0x48)
			g.write8(0xf7)
			g.write8(0xee)
			g.println('imul $r')
		}
		else {
			panic('unhandled imul $r')
		}
	}
}

fn (mut g Gen) div_reg(a Register, b Register) {
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
			g.mov(.edx, 0)
			g.write8(0x48)
			g.write8(0xf7)
			g.write8(0xfb) // idiv ebx
		}
		else {
			panic('unhandled div $a')
		}
	}
	g.println('div $a')
}

fn (mut g Gen) mod_reg(a Register, b Register) {
	g.div_reg(a, b)
	g.mov_reg(.rdx, .rax)
}

fn (mut g Gen) sub_reg(a Register, b Register) {
	if a == .rax && b == .rbx {
		g.write8(0x48)
		g.write8(0x29)
		g.write8(0xd8)
	} else if a == .rdx && b == .rax {
		g.write8(0x48)
		g.write8(0x29)
		g.write8(0xc2)
	} else if a == .rdi && b == .rax {
		g.write8(0x48)
		g.write8(0x29)
		g.write8(0xc7)
	} else {
		panic('unhandled add $a, $b')
	}
	g.println('sub $a, $b')
}

fn (mut g Gen) add_reg(a Register, b Register) {
	if a == .rax && b == .rbx {
		g.write8(0x48)
		g.write8(0x01)
		g.write8(0xd8)
	} else if a == .rax && b == .rdi {
		g.write8(0x48)
		g.write8(0x01)
		g.write8(0xf8)
	} else if a == .rax && b == .rax {
		g.write8(0x48)
		g.write8(0x01)
		g.write8(0xc0)
	} else {
		panic('unhandled add $a, $b')
	}
	g.println('add $a, $b')
}

fn (mut g Gen) mov_reg(a Register, b Register) {
	if a == .rax && b == .rsi {
		g.write([u8(0x48), 0x89, 0xf0])
	} else if a == .rbp && b == .rsp {
		g.write8(0x48)
		g.write8(0x89)
	} else if a == .rdx && b == .rax {
		g.write8(0x48)
		g.write8(0x89)
		g.write8(0xc2)
	} else if a == .rax && b == .rcx {
		g.write8(0x48)
		g.write8(0x89)
		g.write8(0xc8)
	} else if a == .rax && b == .rdi {
		g.write8(0x48)
		g.write8(0x89)
		g.write8(0xf8)
	} else if a == .rcx && b == .rdi {
		g.write8(0x48)
		g.write8(0x89)
		g.write8(0xf9)
	} else if a == .rcx && b == .rax {
		g.write8(0x48)
		g.write8(0x89)
		g.write8(0xc1)
	} else if a == .rdi && b == .rsi {
		g.write8(0x48)
		g.write8(0x89)
		g.write8(0xf7)
	} else if a == .rsi && b == .rax {
		g.write8(0x48)
		g.write8(0x89)
		g.write8(0xc6)
	} else if a == .rdi && b == .rdx {
		g.write8(0x48)
		g.write8(0x89)
		g.write8(0xd7)
	} else if a == .rdi && b == .rax {
		g.write8(0x48)
		g.write8(0x89)
		g.write8(0xc7)
	} else if a == .r12 && b == .rdi {
		g.write8(0x49)
		g.write8(0x89)
		g.write8(0xfc)
	} else if a == .rdi && b == .r12 {
		g.write8(0x4c)
		g.write8(0x89)
		g.write8(0xe7)
	} else if a == .rsi && b == .rdi {
		g.write8(0x48)
		g.write8(0x89)
		g.write8(0xfe)
	} else {
		g.n_error('unhandled mov_reg combination for $a $b')
	}
	g.println('mov $a, $b')
}

fn (mut g Gen) sar8(r Register, val u8) {
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
			panic('unhandled sar $r, $val')
		}
	}
	g.write8(val)
	g.println('sar $r, $val')
}

// generates `mov rbp, rsp`
fn (mut g Gen) mov_rbp_rsp() {
	g.write8(0x48)
	g.write8(0x89)
	g.write8(0xe5)
	g.println('mov rbp, rsp')
}

pub fn (mut g Gen) call_fn(node ast.CallExpr) {
	if g.pref.arch == .arm64 {
		g.call_fn_arm64(node)
		return
	}
	name := node.name
	mut n := name
	if !n.contains('.') {
		n = 'main.$n'
	}
	addr := g.fn_addr[n]
	// Copy values to registers (calling convention)
	// g.mov(.eax, 0)
	for i in 0 .. node.args.len {
		expr := node.args[i].expr
		match expr {
			ast.IntegerLiteral {
				// `foo(2)` => `mov edi,0x2`
				g.mov(native.fn_arg_registers[i], expr.val.int())
			}
			ast.Ident {
				// `foo(x)` => `mov edi,DWORD PTR [rbp-0x8]`
				var_offset := g.get_var_offset(expr.name)
				if g.pref.is_verbose {
					println('i=$i fn name= $name offset=$var_offset')
					println(int(native.fn_arg_registers[i]))
				}
				g.mov_var_to_reg(native.fn_arg_registers[i], var_offset)
			}
			else {
				g.v_error('unhandled call_fn (name=$name) node: $expr.type_name()', node.pos)
			}
		}
	}
	if node.args.len > 6 {
		g.v_error('more than 6 args not allowed for now', node.pos)
	}
	if addr == 0 {
		g.delay_fn_call(name)
		g.call(int(0))
	} else {
		g.call(int(addr))
	}
	g.println('call `${name}()`')
}

fn (mut g Gen) patch_calls() {
	for c in g.callpatches {
		addr := g.fn_addr[c.name]
		if addr == 0 {
			g.n_error('fn addr of `$c.name` = 0')
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

fn (mut g Gen) assign_stmt(node ast.AssignStmt) {
	// `a := 1` | `a,b := 1,2`
	for i, left in node.left {
		right := node.right[i]
		name := left.str()
		// if left is ast.Ident {
		// ident := left as ast.Ident
		match right {
			ast.IntegerLiteral {
				// g.allocate_var(name, 4, right.val.int())
				match node.op {
					.plus_assign {
						dest := g.get_var_offset(name)
						g.mov_var_to_reg(.rax, dest)
						g.add(.rax, right.val.int())
						g.mov_reg_to_var(dest, .rax)
					}
					.minus_assign {
						dest := g.get_var_offset(name)
						g.mov_var_to_reg(.rax, dest)
						g.mov_var_to_reg(.rbx, g.get_var_offset(name))
						g.sub_reg(.rax, .rbx)
						g.mov_reg_to_var(dest, .rax)
					}
					.mult_assign {
						dest := g.get_var_offset(name)
						g.mov_var_to_reg(.rax, dest)
						g.mov_var_to_reg(.rbx, g.get_var_offset(name))
						g.mul_reg(.rax, .rbx)
						g.mov_reg_to_var(dest, .rax)
					}
					.div_assign {
						dest := g.get_var_offset(name)
						g.mov_var_to_reg(.rax, dest)
						g.mov_var_to_reg(.rbx, g.get_var_offset(name))
						g.div_reg(.rax, .rbx)
						g.mov_reg_to_var(dest, .rax)
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
								g.mov(.rax, right.val.int())
								offset := g.get_var_offset('i') // node.left[i])
								g.mov_reg_to_var(offset, .rax)
							}
							ast.InfixExpr {
								// eprintln('assign')
								// dump(node.left[i])
								offset := g.get_var_offset('i') // node.left[i])
								g.mov_reg_to_var(offset, native.fn_arg_registers[i])
							}
							/*
							ast.int_type_idx {
								g.expr(node.left[i])
								match node.left[i] {
								ast.IndexExpr {
									ie := node.left[i] as ast.IndexExpr
									bracket := name.index('[') or {
										g.v_error('bracket expected', node.pos)
										exit(1)
									}
									var_name := name[0 .. bracket]
									mut dest := g.get_var_offset(var_name)
									index := ie.index as ast.IntegerLiteral
									dest += index.val.int() * 8
									// TODO check if out of bounds access
									g.mov(.rax, right.val.int())
									g.mov_reg_to_var(dest, .rax)
									// eprintln('${var_name}[$index] = ${right.val.int()}')
								} else {
									dump(node)
									g.v_error('oops', node.pos)
								}
								}
							}
							*/
							else {
								tn := node.left[i].type_name()
								dump(node.left_types)
								g.n_error('unhandled assign type: $tn')
							}
						}
					}
					else {
						eprintln('ERROR 2')
						dump(node)
					}
				}
			}
			ast.InfixExpr {
				// eprintln('infix') dump(node) dump(right)
				g.infix_expr(right)
				offset := g.allocate_var(name, 8, 0)
				// `mov DWORD PTR [rbp-0x8],eax`
				if g.pref.is_verbose {
					println('infix assignment $name offset=$offset.hex2()')
				}
				g.mov_reg_to_var(offset, .rax)
			}
			ast.Ident {
				// eprintln('identr') dump(node) dump(right)
				match node.op {
					.plus_assign {
						dest := g.get_var_offset(name)
						g.mov_var_to_reg(.rax, dest)
						g.add8_var(.rax, g.get_var_offset(right.name))
						g.mov_reg_to_var(dest, .rax)
					}
					.minus_assign {
						dest := g.get_var_offset(name)
						g.mov_var_to_reg(.rax, dest)
						g.mov_var_to_reg(.rbx, g.get_var_offset(right.name))
						g.sub_reg(.rax, .rbx)
						g.mov_reg_to_var(dest, .rax)
					}
					.div_assign {
						// this should be called when `a /= b` but it's not :?
						dest := g.get_var_offset(name)
						g.mov_var_to_reg(.rax, dest)
						g.mov_var_to_reg(.rbx, g.get_var_offset(right.name))
						g.div_reg(.rax, .rbx)
						g.mov_reg_to_var(dest, .rax)
					}
					.decl_assign {
						dest := g.allocate_var(name, 8, 0)
						g.mov_var_to_reg(.rax, g.get_var_offset(right.name))
						g.mov_reg_to_var(dest, .rax)
					}
					.assign {
						g.mov_var_to_reg(.rax, g.get_var_offset(right.name))
						g.mov_reg_to_var(g.get_var_offset(name), .rax)
					}
					else {
						eprintln('TODO: unhandled assign ident case')
						dump(node)
					}
				}
				// a += b
			}
			ast.StructInit {
				sym := g.table.sym(right.typ)
				info := sym.info as ast.Struct
				for field in info.fields {
					field_name := name + '.' + field.name
					println(field_name)
					g.allocate_var(field_name, 4, 0)
				}
			}
			ast.ArrayInit {
				// check if array is empty
				mut pos := g.allocate_array(name, 8, right.exprs.len)
				// allocate array of right.exprs.len vars
				for e in right.exprs {
					match e {
						ast.IntegerLiteral {
							g.mov(.rax, e.val.int())
							g.mov_reg_to_var(pos, .rax)
							pos += 8
						}
						ast.StringLiteral {
							// TODO: use learel
							g.mov64(.rsi, g.allocate_string('$e.val', 2, .abs64)) // for rsi its 2
							g.mov_reg_to_var(pos, .rsi)
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
				offset := g.allocate_var(name, 8, 0)
				if g.pref.is_verbose {
					println('infix assignment $name offset=$offset.hex2()')
				}
				ie := node.right[i] as ast.IndexExpr
				var_name := ie.left.str()
				mut dest := g.get_var_offset(var_name)
				if ie.index is ast.IntegerLiteral {
					index := ie.index
					dest += index.val.int() * 8
					g.mov_var_to_reg(.rax, dest)
				} else if ie.index is ast.Ident {
					ident := ie.index
					var_offset := g.get_var_offset(ident.name)
					g.mov_var_to_reg(.edi, var_offset)
					g.mov_var_to_reg(.rax, dest)
					g.add_reg(.rax, .rdi)
				} else {
					g.n_error('only integers and idents can be used as indexes')
				}
				// TODO check if out of bounds access
				g.mov_reg_to_var(offset, .eax)
			}
			ast.StringLiteral {
				dest := g.allocate_var(name, 4, 0)
				ie := node.right[i] as ast.StringLiteral
				g.learel(.rsi, g.allocate_string(ie.val.str(), 3, .rel32))
				g.mov_reg_to_var(dest, .rsi)
			}
			ast.CallExpr {
				dest := g.allocate_var(name, 4, 0)
				g.call_fn(right)
				g.mov_reg_to_var(dest, .rax)
				g.mov_var_to_reg(.rsi, dest)
			}
			ast.SelectorExpr {
				g.v_error('unhandled selectors', node.pos)
			}
			ast.GoExpr {
				g.v_error('threads not implemented for the native backend', node.pos)
			}
			ast.CastExpr {
				g.warning('cast expressions are work in progress', right.pos)
				match right.typname {
					'u64' {
						g.allocate_var(name, 8, right.expr.str().int())
					}
					'int' {
						g.allocate_var(name, 4, right.expr.str().int())
					}
					else {
						g.v_error('unsupported cast type $right.typ', node.pos)
					}
				}
			}
			ast.FloatLiteral {
				g.v_error('floating point arithmetic not yet implemented for the native backend',
					node.pos)
			}
			ast.TypeOf {
				g.gen_typeof_expr(node.right[i] as ast.TypeOf, true)
				g.mov_reg(.rsi, .rax)
			}
			else {
				// dump(node)
				g.v_error('unhandled assign_stmt expression: $right.type_name()', right.pos())
			}
		}
		// }
	}
}

fn (mut g Gen) infix_expr(node ast.InfixExpr) {
	// TODO
	if node.left is ast.InfixExpr {
		g.n_error('only simple expressions are supported right now (not more than 2 operands)')
	}
	match node.left {
		ast.Ident {
			g.mov_var_to_reg(.eax, g.get_var_offset(node.left.name))
		}
		else {}
	}
	if node.right is ast.Ident {
		var_offset := g.get_var_offset(node.right.name)
		match node.op {
			.plus { g.add8_var(.eax, var_offset) }
			.mul { g.mul8_var(.eax, var_offset) }
			.div { g.div8_var(.eax, var_offset) }
			.minus { g.sub8_var(.eax, var_offset) }
			else {}
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

fn (mut g Gen) gen_asm_stmt(asm_node ast.AsmStmt) {
	if g.pref.arch == .arm64 {
		g.gen_asm_stmt_arm64(asm_node)
	} else {
		g.gen_asm_stmt_amd64(asm_node)
	}
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
		g.println(': $line')
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
				g.println('mov $reg, $imm')
			}
			else {
				g.v_error('unsupported instruction $t.name', asm_node.pos)
			}
		}
	}
}

fn (mut g Gen) gen_assert(assert_node ast.AssertStmt) {
	mut cjmp_addr := 0
	mut ine := ast.InfixExpr{}
	ane := assert_node.expr
	if ane is ast.ParExpr { // assert(1==1)
		ine = ane.expr as ast.InfixExpr
	} else if ane is ast.InfixExpr { // assert 1==1
		ine = ane
	} else {
		g.n_error('Unsupported expression in assert')
	}
	label := g.labels.new_label()
	cjmp_addr = g.condition(ine, true)
	g.labels.patches << LabelPatch{
		id: label
		pos: cjmp_addr
	}
	g.println('; jump to label $label')
	g.expr(assert_node.expr)
	g.trap()
	g.labels.addrs[label] = g.pos()
	g.println('; label $label')
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
			g.cjmp(.lt)
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

fn (mut g Gen) condition(infix_expr ast.InfixExpr, neg bool) int {
	match infix_expr.left {
		ast.IntegerLiteral {
			match infix_expr.right {
				ast.IntegerLiteral {
					// 3 < 4
					a0 := infix_expr.left.val.int()
					// a0 := (infix_expr.left as ast.IntegerLiteral).val.int()
					// XXX this will not compile
					a1 := (infix_expr.right as ast.IntegerLiteral).val.int()
					// TODO. compute at compile time
					g.mov(.eax, a0)
					g.cmp(.eax, ._32, a1)
				}
				ast.Ident {
					// 3 < var
					// lit := infix_expr.right as ast.IntegerLiteral
					// g.cmp_var(infix_expr.left.name, lit.val.int())
					// +not
					g.n_error('unsupported if construction')
				}
				else {
					g.n_error('unsupported if construction')
				}
			}
		}
		ast.Ident {
			match infix_expr.right {
				ast.IntegerLiteral {
					// var < 4
					lit := infix_expr.right as ast.IntegerLiteral
					g.cmp_var(infix_expr.left.name, lit.val.int())
				}
				ast.Ident {
					// var < var2
					g.n_error('unsupported if construction')
				}
				else {
					g.n_error('unsupported if construction')
				}
			}
		}
		else {
			// dump(infix_expr)
			g.n_error('unhandled $infix_expr.left')
		}
	}

	// mut cjmp_addr := 0 // location of `jne *00 00 00 00*`
	return if neg { g.cjmp_op(infix_expr.op) } else { g.cjmp_notop(infix_expr.op) }
}

fn (mut g Gen) if_expr(node ast.IfExpr) {
	if node.is_comptime {
		g.n_error('ignored comptime')
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
			infix_expr := branch.cond as ast.InfixExpr
			label := g.labels.new_label()
			cjmp_addr := g.condition(infix_expr, false)
			g.labels.patches << LabelPatch{
				id: label
				pos: cjmp_addr
			}
			g.println('; jump to label $label')
			g.stmts(branch.stmts)
			if has_endif {
				jump_addr := g.jmp(0)
				g.labels.patches << LabelPatch{
					id: endif_label
					pos: jump_addr
				}
				g.println('; jump to label $endif_label')
			}
			// println('after if g.pos=$g.pos() jneaddr=$cjmp_addr')
			g.labels.addrs[label] = g.pos()
			g.println('; label $label')
		}
	}
	if has_endif {
		g.labels.addrs[endif_label] = g.pos()
		g.println('; label $endif_label')
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
		g.println('; label $start_label')
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
		g.println('; label $end_label')
		return
	}
	infix_expr := node.cond as ast.InfixExpr
	mut jump_addr := 0 // location of `jne *00 00 00 00*`
	start := g.pos()
	start_label := g.labels.new_label()
	g.labels.addrs[start_label] = start
	g.println('; label $start_label')
	match infix_expr.left {
		ast.Ident {
			match infix_expr.right {
				ast.Ident {
					var_offset := g.get_var_offset(infix_expr.right.name)
					g.mov_var_to_reg(.rax, var_offset)
					g.cmp_var_reg(infix_expr.left.name, .rax)
				}
				ast.IntegerLiteral {
					lit := infix_expr.right as ast.IntegerLiteral
					g.cmp_var(infix_expr.left.name, lit.val.int())
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
	g.println('; jump to label $end_label')
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
	g.println('; label $end_label')
	g.println('jmp after for')
}

fn (mut g Gen) fn_decl_amd64(node ast.FnDecl) {
	g.push(.rbp)
	g.mov_rbp_rsp()
	locals_count := node.scope.objects.len + node.params.len + node.defer_stmts.len
	g.stackframe_size = (locals_count * 8) + 0x10
	g.sub8(.rsp, g.stackframe_size)

	// Copy values from registers to local vars (calling convention)
	mut offset := 0
	for i in 0 .. node.params.len {
		name := node.params[i].name
		// TODO optimize. Right now 2 mov's are used instead of 1.
		g.allocate_var(name, 4, 0)
		// `mov DWORD PTR [rbp-0x4],edi`
		offset += 4
		g.mov_reg_to_var(offset, native.fn_arg_registers[i])
	}
	// define defer vars
	for i in 0 .. node.defer_stmts.len {
		name := '_defer$i'
		g.allocate_var(name, 8, 0)
	}
	//
	g.stmts(node.stmts)
	is_main := node.name == 'main.main'
	if is_main {
		// println('end of main: gen exit')
		zero := ast.IntegerLiteral{}
		g.gen_exit(zero)
		g.ret()
		return
	}
	// g.leave()
	g.labels.addrs[0] = g.pos()
	g.println('; label 0: return')
	if g.defer_stmts.len != 0 {
		// save return value
		g.push(.rax)
		for defer_stmt in g.defer_stmts.reverse() {
			defer_var := g.get_var_offset('_defer$defer_stmt.idx_in_fn')
			g.mov_var_to_reg(.rax, defer_var)
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
	g.add8(.rsp, g.stackframe_size)
	g.pop(.rbp)
	g.ret()
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

pub fn (mut g Gen) allocate_var(name string, size int, initial_val int) int {
	if g.pref.arch == .arm64 {
		// TODO
		return 0
	}
	// `a := 3`  => `mov DWORD [rbp-0x4],0x3`
	match size {
		1 {
			// BYTE
			g.write8(0xc6)
			g.write8(0x45)
		}
		4 {
			// DWORD
			g.write8(0xc7)
			g.write8(0x45)
		}
		8 {
			// QWORD
			g.write8(0x48)
			g.write8(0xc7)
			g.write8(0x45)
		}
		else {
			g.n_error('allocate_var: bad size $size')
		}
	}
	// Generate N in `[rbp-N]`
	n := g.stack_var_pos + size
	g.write8(0xff - n + 1)
	g.stack_var_pos += size
	g.var_offset[name] = g.stack_var_pos

	// Generate the value assigned to the variable
	match size {
		1 {
			g.write8(initial_val)
		}
		4 {
			g.write32(initial_val)
		}
		8 {
			g.write32(initial_val) // fixme: 64-bit segfaulting
		}
		else {
			g.n_error('allocate_var: bad size $size')
		}
	}

	// println('allocate_var(size=$size, initial_val=$initial_val)')
	g.println('mov [rbp-$n.hex2()], $initial_val ; Allocate var `$name`')
	return g.stack_var_pos
}

fn (mut g Gen) convert_int_to_string(r Register, buffer int) {
	if r != .rax {
		g.mov_reg(.rax, r)
	}

	// check if value in rax is zero
	g.cmp_zero(.rax)
	skip_zero_label := g.labels.new_label()
	skip_zero_cjmp_addr := g.cjmp(.jne)
	g.labels.patches << LabelPatch{
		id: skip_zero_label
		pos: skip_zero_cjmp_addr
	}
	g.println('; jump to label $skip_zero_label')

	// handle zeros seperately
	g.mov_int_to_var(buffer, ._8, '0'[0])
	end_label := g.labels.new_label()
	end_jmp_addr := g.jmp(0)
	g.labels.patches << LabelPatch{
		id: end_label
		pos: end_jmp_addr
	}
	g.println('; jump to label $end_label')

	g.labels.addrs[skip_zero_label] = g.pos()
	g.println('; label $skip_zero_label')

	// load a pointer to the string to rdi
	g.lea_var_to_reg(.rdi, buffer)

	// detect if value in rax is negative
	g.cmp_zero(.rax)
	skip_minus_label := g.labels.new_label()
	skip_minus_cjmp_addr := g.cjmp(.jge)
	g.labels.patches << LabelPatch{
		id: skip_minus_label
		pos: skip_minus_cjmp_addr
	}
	g.println('; jump to label $skip_minus_label')

	// add a `-` sign as the first character
	g.mov_int_to_var(buffer, ._8, '-'[0])
	g.neg(.rax) // negate our integer to make it positive
	g.inc(.rdi) // increment rdi to skip the `-` character
	g.labels.addrs[skip_minus_label] = g.pos()
	g.println('; label $skip_minus_label')

	g.mov_reg(.r12, .rdi) // copy the buffer position to rcx

	loop_label := g.labels.new_label()
	loop_start := g.pos()
	g.println('; label $loop_label')

	g.push(.rax)

	g.mov(.rdx, 0)
	g.mov(.rbx, 10)
	g.div_reg(.rax, .rbx)
	g.add8(.rdx, '0'[0])

	g.write8(0x66)
	g.write8(0x89)
	g.write8(0x17)
	g.println('mov BYTE PTR [rdi], rdx')

	// divide the integer in rax by 10 for next iteration
	g.pop(.rax)
	g.mov(.rbx, 10)
	g.cdq()
	g.write8(0x48)
	g.write8(0xf7)
	g.write8(0xfb)
	g.println('idiv rbx')

	// go to the next character
	g.inc(.rdi)

	// if the number in rax still isn't zero, repeat
	g.cmp_zero(.rax)
	loop_cjmp_addr := g.cjmp(.jg)
	g.labels.patches << LabelPatch{
		id: loop_label
		pos: loop_cjmp_addr
	}
	g.println('; jump to label $skip_minus_label')
	g.labels.addrs[loop_label] = loop_start

	// after all was converted, reverse the string
	g.reverse_string(.r12)

	g.labels.addrs[end_label] = g.pos()
	g.println('; label $end_label')
}

fn (mut g Gen) reverse_string(reg Register) {
	if reg != .rdi {
		g.mov_reg(.rdi, reg)
	}

	g.mov(.eax, 0)

	g.write8(0x48)
	g.write8(0x8d)
	g.write8(0x48)
	g.write8(0xff)
	g.println('lea rcx, [rax-0x1]')

	g.mov_reg(.rsi, .rdi)

	g.write8(0xf2)
	g.write8(0xae)
	g.println('repnz scas al, BYTE PTR es:[rdi]')

	g.sub8(.rdi, 0x2)
	g.cmp_reg(.rdi, .rsi)

	g.write8(0x7e)
	g.write8(0x0a)
	g.println('jle 0x1e')

	g.write8(0x86)
	g.write8(0x07)
	g.println('xchg BYTE PTR [rdi], al')

	g.write8(0x86)
	g.write8(0x06)
	g.println('xchg BYTE PTR [rsi], al')

	g.std()

	g.write8(0xaa)
	g.println('stos BYTE PTR es:[rdi], al')

	g.cld()

	g.write8(0xac)
	g.println('lods al, BYTE PTR ds:[rsi]')

	g.write8(0xeb)
	g.write8(0xf1)
	g.println('jmp 0xf')
}
