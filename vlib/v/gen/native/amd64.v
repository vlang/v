module native

import term
import v.ast

pub struct Amd64 {
mut:
	g &Gen
	// arm64 specific stuff for code generation
}

// string_addr map[string]i64
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
)

fn (mut g Gen) inc(reg Register) {
	g.write16(0xff49)
	match reg {
		.r12 { g.write8(0xc4) }
		else { panic('unhandled inc $reg') }
	}
}

fn (mut g Gen) cmp(reg Register, size Size, val i64) {
	g.write8(0x49)
	// Second byte depends on the size of the value
	match size {
		._8 { g.write8(0x83) }
		._32 { g.write8(0x81) }
		else { panic('unhandled cmp') }
	}
	// Third byte depends on the register being compared to
	match reg {
		.r12 { g.write8(0xfc) }
		else { panic('unhandled cmp') }
	}
	g.write8(int(val))
}

/*
rax // 0
	rcx // 1
	rdx // 2
	rbx // 3
	rsp // 4
	rbp // 5
	rsi // 6
	rdi // 7
*/
// `a == 1`
// `cmp DWORD [rbp-0x4],0x1`
fn (mut g Gen) cmp_var(var_name string, val int) {
	g.write8(0x81) // 83 for 1 byte?
	g.write8(0x7d)
	offset := g.get_var_offset(var_name)
	g.write8(0xff - offset + 1)
	g.write32(val)
	g.println('cmp var `$var_name` $val')
}

// `add DWORD [rbp-0x4], 1`
fn (mut g Gen) inc_var(var_name string) {
	g.write16(0x4581) // 83 for 1 byte
	offset := g.get_var_offset(var_name)
	g.write8(0xff - offset + 1)
	g.write32(1)
	g.println('inc_var `$var_name`')
}

// Returns the position of the address to jump to (set later).
fn (mut g Gen) jne() int {
	g.write16(0x850f)
	pos := g.pos()
	g.write32(placeholder)
	g.println('jne')
	return int(pos)
}

fn (mut g Gen) jge() int {
	g.write16(0x8d0f)
	pos := g.pos()
	g.write32(placeholder)
	g.println('jne')
	return int(pos)
}

fn (mut g Gen) jmp(addr int) {
	g.write8(0xe9)
	g.write32(addr) // 0xffffff
	g.println('jmp')
}

fn abs(a i64) i64 {
	return if a < 0 { -a } else { a }
}

fn (mut g Gen) jle(addr i64) {
	// Calculate the relative offset to jump to
	// (`addr` is absolute address)
	offset := 0xff - int(abs(addr - g.buf.len)) - 1
	g.write8(0x7e)
	g.write8(offset)
	g.println('jle')
}

fn (mut g Gen) jl(addr i64) {
	offset := 0xff - int(abs(addr - g.buf.len)) - 1
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
fn (mut g Gen) mov64(reg Register, val i64) {
	match reg {
		.rsi {
			g.write8(0x48)
			g.write8(0xbe)
		}
		else {
			eprintln('unhandled mov $reg')
		}
	}
	g.write64(val)
	g.println('mov64 $reg, $val')
}

fn (mut g Gen) mov_reg_to_rbp(var_offset int, reg Register) {
	// 89 7d fc     mov DWORD PTR [rbp-0x4],edi
	g.write8(0x89)
	match reg {
		.eax, .rax { g.write8(0x45) }
		.edi, .rdi { g.write8(0x7d) }
		.rsi { g.write8(0x75) }
		.rdx { g.write8(0x55) }
		.rcx { g.write8(0x4d) }
		else { verror('mov_from_reg $reg') }
	}
	g.write8(0xff - var_offset + 1)
	g.println('mov DWORD PTR[rbp-$var_offset.hex2()],$reg')
}

fn (mut g Gen) mov_var_to_reg(reg Register, var_offset int) {
	// 8b 7d f8          mov edi,DWORD PTR [rbp-0x8]
	g.write8(0x8b)
	match reg {
		.eax, .rax { g.write8(0x45) }
		.edi, .rdi { g.write8(0x7d) }
		.rsi { g.write8(0x75) }
		.rdx { g.write8(0x55) }
		.rcx { g.write8(0x4d) }
		else { verror('mov_var_to_reg $reg') }
	}
	g.write8(0xff - var_offset + 1)
	g.println('mov $reg,DWORD PTR[rbp-$var_offset.hex2()]')
}

fn (mut g Gen) call(addr int) {
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

pub fn (mut g Gen) ret() {
	g.write8(0xc3)
	g.println('ret')
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

pub fn (mut g Gen) add(reg Register, val int) {
	g.write8(0x48)
	g.write8(0x81)
	g.write8(0xe8 + int(reg)) // TODO rax is different?
	g.write32(val)
	g.println('add $reg,$val.hex2()')
}

pub fn (mut g Gen) add8(reg Register, val int) {
	g.write8(0x48)
	g.write8(0x83)
	// g.write8(0xe8 + reg) // TODO rax is different?
	g.write8(0xc4)
	g.write8(val)
	g.println('add8 $reg,$val.hex2()')
}

fn (mut g Gen) add8_var(reg Register, var_offset int) {
	g.write8(0x03)
	match reg {
		.eax, .rax { g.write8(0x45) }
		else { verror('add8_var') }
	}
	g.write8(0xff - var_offset + 1)
	g.println('add8 $reg,DWORD PTR[rbp-$var_offset.hex2()]')
}

fn (mut g Gen) sub8_var(reg Register, var_offset int) {
	g.write8(0x2b)
	match reg {
		.eax, .rax { g.write8(0x45) }
		else { verror('sub8_var') }
	}
	g.write8(0xff - var_offset + 1)
	g.println('sub8 $reg,DWORD PTR[rbp-$var_offset.hex2()]')
}

fn (mut g Gen) mul8_var(reg Register, var_offset int) {
	g.write8(0x0f)
	g.write8(0xaf)
	match reg {
		.eax, .rax { g.write8(0x45) }
		else { verror('mul8_var') }
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

pub fn (mut g Gen) save_main_fn_addr() {
	g.main_fn_addr = i64(g.buf.len)
}

pub fn (mut g Gen) gen_print(s string) {
	//
	// qq := s + '\n'
	//
	g.strings << s
	// g.string_addr[s] = str_pos
	g.mov(.eax, g.nsyscall_write())
	g.mov(.edi, 1)
	str_pos := g.buf.len + 2
	g.str_pos << str_pos
	g.mov64(.rsi, 0) // segment_start +  0x9f) // str pos // placeholder
	g.mov(.edx, s.len) // len
	g.syscall()
}

fn (mut g Gen) nsyscall_write() int {
	match g.pref.os {
		.linux {
			return 1
		}
		.macos {
			return 0x2000004
		}
		else {
			verror('unsupported exit syscall for this platform')
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
		else {
			verror('unsupported exit syscall for this platform')
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
			verror('native exit builtin: Unsupported call $right')
		}
		ast.Ident {
			var_offset := g.get_var_offset(expr.name)
			g.mov_var_to_reg(.edi, var_offset)
		}
		ast.IntegerLiteral {
			g.mov(.edi, expr.val.int())
		}
		else {
			verror('native builtin exit expects a numeric argument')
		}
	}
	g.mov(.eax, g.nsyscall_exit())
	g.syscall()
}

fn (mut g Gen) mov(reg Register, val int) {
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
				panic('unhandled mov $reg')
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
			.edx {
				g.write8(0xba)
			}
			.rsi {
				g.write8(0x48)
				g.write8(0xbe)
			}
			.r12 {
				g.write8(0x41)
				g.write8(0xbc) // r11 is 0xbb etc
			}
			else {
				panic('unhandled mov $reg')
			}
		}
		g.write32(val)
		g.println('mov $reg, $val')
	}
}

fn (mut g Gen) mov_reg(a Register, b Register) {
	match a {
		.rbp {
			g.write8(0x48)
			g.write8(0x89)
		}
		else {}
	}
}

// generates `mov rbp, rsp`
fn (mut g Gen) mov_rbp_rsp() {
	g.write8(0x48)
	g.write8(0x89)
	g.write8(0xe5)
	g.println('mov rbp,rsp')
}

pub fn (mut g Gen) call_fn(node ast.CallExpr) {
	name := node.name
	// println('call fn $name')
	addr := g.fn_addr[name]
	if addr == 0 {
		verror('fn addr of `$name` = 0')
	}
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
				verror('unhandled call_fn (name=$name) node: ' + expr.type_name())
			}
		}
	}
	if node.args.len > 6 {
		verror('more than 6 args not allowed for now')
	}
	g.call(int(addr))
	g.println('fn call `${name}()`')
	// println('call $name $addr')
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
				g.allocate_var(name, 4, right.val.int())
			}
			ast.InfixExpr {
				g.infix_expr(right)
				g.allocate_var(name, 4, 0)
				// `mov DWORD PTR [rbp-0x8],eax`
				offset := g.get_var_offset(name)
				if g.pref.is_verbose {
					println('infix assignment $name offset=$offset.hex2()')
				}
				g.mov_reg_to_rbp(offset, .eax)
			}
			ast.StructInit {
				sym := g.table.get_type_symbol(right.typ)
				// println(sym)
				// println(typeof(sym.info))
				info := sym.info as ast.Struct
				for field in info.fields {
					field_name := name + '.' + field.name
					println(field_name)
					g.allocate_var(field_name, 4, 0)
				}
			}
			else {
				g.error_with_pos('native assign_stmt unhandled expr: ' + right.type_name(),
					right.position())
			}
		}
		// }
	}
}

fn (mut g Gen) infix_expr(node ast.InfixExpr) {
	println('infix expr op=$node.op')
	if node.left is ast.InfixExpr {
		verror('only simple expressions are supported right now (not more than 2 operands)')
	}
	match mut node.left {
		ast.Ident { g.mov_var_to_reg(.eax, g.get_var_offset(node.left.name)) }
		else {}
	}
	if mut node.right is ast.Ident {
		var_offset := g.get_var_offset(node.right.name)
		match node.op {
			.plus { g.add8_var(.eax, var_offset) }
			.mul { g.mul8_var(.eax, var_offset) }
			.minus { g.sub8_var(.eax, var_offset) }
			else {}
		}
	}
}

fn (mut g Gen) if_expr(node ast.IfExpr) {
	branch := node.branches[0]
	infix_expr := branch.cond as ast.InfixExpr
	mut jne_addr := 0 // location of `jne *00 00 00 00*`
	match mut infix_expr.left {
		ast.Ident {
			lit := infix_expr.right as ast.IntegerLiteral
			g.cmp_var(infix_expr.left.name, lit.val.int())
			jne_addr = g.jne()
		}
		else {
			verror('unhandled infix.left')
		}
	}
	g.stmts(branch.stmts)
	// Now that we know where we need to jump if the condition is false, update the `jne` call.
	// The value is the relative address, difference between current position and the location
	// after `jne 00 00 00 00`
	// println('after if g.pos=$g.pos() jneaddr=$jne_addr')
	g.write32_at(jne_addr, int(g.pos() - jne_addr - 4)) // 4 is for "00 00 00 00"
}

fn (mut g Gen) for_stmt(node ast.ForStmt) {
	infix_expr := node.cond as ast.InfixExpr
	// g.mov(.eax, 0x77777777)
	mut jump_addr := 0 // location of `jne *00 00 00 00*`
	start := g.pos()
	match mut infix_expr.left {
		ast.Ident {
			lit := infix_expr.right as ast.IntegerLiteral
			g.cmp_var(infix_expr.left.name, lit.val.int())
			jump_addr = g.jge()
		}
		else {
			verror('unhandled infix.left')
		}
	}
	g.stmts(node.stmts)
	// Go back to `cmp ...`
	// Diff between `jmp 00 00 00 00 X` and `cmp`
	g.jmp(int(0xffffffff - (g.pos() + 5 - start) + 1))
	// Update the jump addr to current pos
	g.write32_at(jump_addr, int(g.pos() - jump_addr - 4)) // 4 is for "00 00 00 00"
	g.println('jpm after for')
}

fn (mut g Gen) fn_decl(node ast.FnDecl) {
	if g.pref.is_verbose {
		println(term.green('\n$node.name:'))
	}
	g.stack_var_pos = 0
	is_main := node.name == 'main.main'
	// println('saving addr $node.name $g.buf.len.hex2()')
	if is_main {
		g.save_main_fn_addr()
	} else {
		g.register_function_address(node.name)
	}
	if g.pref.arch == .arm64 {
		g.fn_decl_arm64(node)
		return
	}
	g.push(.rbp)
	g.mov_rbp_rsp()
	// if !is_main {
	g.sub8(.rsp, 0x10)
	// }
	if node.params.len > 0 {
		// g.mov(.r12, 0x77777777)
	}
	// Copy values from registers to local vars (calling convention)
	mut offset := 0
	for i in 0 .. node.params.len {
		name := node.params[i].name
		// TODO optimize. Right now 2 mov's are used instead of 1.
		g.allocate_var(name, 4, 0)
		// `mov DWORD PTR [rbp-0x4],edi`
		offset += 4
		g.mov_reg_to_rbp(offset, native.fn_arg_registers[i])
	}
	//
	g.stmts(node.stmts)
	if is_main {
		// println('end of main: gen exit')
		zero := ast.IntegerLiteral{}
		g.gen_exit(zero)
		// return
	}
	if !is_main {
		// g.leave()
		g.add8(.rsp, 0x10)
		g.pop(.rbp)
	}
	g.ret()
}

pub fn (mut x Amd64) allocate_var(name string, size int, initial_val int) {
	// do nothing as interface call is crashing
}

pub fn (mut g Gen) allocate_var(name string, size int, initial_val int) {
	// `a := 3`  =>
	// `move DWORD [rbp-0x4],0x3`
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
			verror('allocate_var: bad size $size')
		}
	}
	// Generate N in `[rbp-N]`
	n := g.stack_var_pos + size
	g.write8(0xff - n + 1)
	g.stack_var_pos += size
	g.var_offset[name] = g.stack_var_pos
	// Generate the value assigned to the variable
	g.write32(initial_val)
	// println('allocate_var(size=$size, initial_val=$initial_val)')
	g.println('mov DWORD [rbp-$n.hex2()],$initial_val (Allocate var `$name`)')
}
