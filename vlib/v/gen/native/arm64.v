module native

import v.ast

enum Arm64Register {
	x0 // v----
	x1 // |
	x2 // |
	x3 // | parameter and result registers
	x4 // |
	x5 // |
	x6 // |
	x7 // ^----
	x8 // XR - indirect result location register
	x9 //  v----
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

pub struct Arm64 {
mut:
	g &Gen
	// arm64 specific stuff for code generation
}

pub fn (mut x Arm64) allocate_var(name string, size int, initial_val int) {
	eprintln('TODO: allocating var on arm64 ($name) = $size = $initial_val')
}

fn (mut g Gen) mov_arm(reg Arm64Register, val u64) {
	// m := u64(0xffff)
	// x := u64(val)
	// println('========')
	// println(x & ~m)
	// println(x & ~(m << 16))
	// g.write32(0x777777)
	r := int(reg)
	if r >= 0 && r <= 16 {
		g.write32(int(u32(0xd2800000 + int(r) + (int(val) << 5))))
		g.println('mov x$r, $val')
	} else {
		g.n_error('mov_arm unsupported values')
	}
	/*
	if 1 ^ (x & ~m) != 0 {
		// println('yep')
		g.write32(int(u64(0x52800000) | u64(r) | x << 5))
		g.write32(0x88888888)
		g.write32(int(u64(0x52800000) | u64(r) | x >> 11))
	} else if 1 ^ (x & ~(m << 16)) != 0 {
		// g.write32(int(u64(0x52800000) | u64(r) | x >> 11))
		// println('yep2')
		// g.write32(0x52a00000 | r | val >> 11)
	}
	*/
}

fn (mut g Gen) neg_arm(r Arm64Register) {
	g.neg_regs_arm(r, r)
}

fn (mut g Gen) neg_regs_arm(a Arm64Register, b Arm64Register) {
	if u32(a) < 0x0f && u32(b) < 0x0f {
		g.write32(int(0xe2600000 | (u32(a) << 16) | u32(b) << 12))
		g.println('neg $a, $b')
	} else {
		g.n_error('unhandled neg $a, $b')
	}
}

pub fn (mut g Gen) fn_decl_arm64(node ast.FnDecl) {
	g.gen_arm64_helloworld()
	// TODO
}

pub fn (mut g Gen) call_fn_arm64(node ast.CallExpr) {
	name := node.name
	// println('call fn $name')
	addr := g.fn_addr[name]
	if addr == 0 {
		g.n_error('fn addr of `$name` = 0')
	}
	// Copy values to registers (calling convention)
	// g.mov_arm(.eax, 0)
	for i in 0 .. node.args.len {
		expr := node.args[i].expr
		match expr {
			ast.IntegerLiteral {
				// `foo(2)` => `mov edi,0x2`
				// g.mov_arm(native.fn_arg_registers[i], expr.val.int())
			}
			/*
			ast.Ident {
				// `foo(x)` => `mov edi,DWORD PTR [rbp-0x8]`
				var_offset := g.get_var_offset(expr.name)
				if g.pref.is_verbose {
					println('i=$i fn name= $name offset=$var_offset')
					println(int(native.fn_arg_registers[i]))
				}
				g.mov_var_to_reg(native.fn_arg_registers[i], var_offset)
			}
			*/
			else {
				g.n_error('unhandled call_fn (name=$name) node: ' + expr.type_name())
			}
		}
	}
	if node.args.len > 6 {
		g.n_error('more than 6 args not allowed for now')
	}
	g.call(int(addr))
	g.println('fn call `${name}()`')
	// println('call $name $addr')
}

fn (mut g Gen) gen_arm64_helloworld() {
	if g.pref.os == .linux {
		g.mov_arm(.x0, 1)
		g.adr(.x1, 0x10)
		g.mov_arm(.x2, 13)
		g.mov_arm(.x8, 64) // write (linux-arm64)
		g.svc()
	} else {
		g.mov_arm(.x0, 1)
		g.adr(.x1, 0x10 + 4)
		g.mov_arm(.x2, 13)
		g.mov_arm(.x16, 4) // write
		g.svc()
		g.mov_arm(.x0, 0)
		g.mov_arm(.x16, 1)
		g.svc()
	}
	zero := ast.IntegerLiteral{}
	g.gen_exit(zero)
	g.write_string('Hello World!\n')
	g.write8(0) // padding?
	g.write8(0)
	g.write8(0)
}

fn (mut g Gen) adr(r Arm64Register, delta int) {
	g.write32(int(0x10000000 | int(r) | int(u32(delta) << 4)))
	g.println('adr $r, $delta')
}

fn (mut g Gen) bl() {
	// g.write32(0xa9400000)
	g.write32(0x94000000)
	g.println('bl 0')
}

fn (mut g Gen) svc() {
	if g.pref.os == .linux {
		g.write32(0xd4001001)
		g.println('svc 0x80')
	} else {
		g.write32(0xd4000001)
		g.println('svc 0')
	}
}

pub fn (mut c Arm64) gen_exit(mut g Gen, expr ast.Expr) {
	mut return_code := u64(0)
	match expr {
		ast.IntegerLiteral {
			return_code = expr.val.u64()
		}
		else {
			g.n_error('native builtin exit expects a numeric argument')
		}
	}
	match c.g.pref.os {
		.macos {
			c.g.mov_arm(.x0, return_code)
			c.g.mov_arm(.x16, 1) // syscall exit
		}
		.linux {
			c.g.mov_arm(.x16, return_code)
			c.g.mov_arm(.x8, 93)
			c.g.mov_arm(.x0, 0)
		}
		else {
			g.n_error('unsupported os $c.g.pref.os')
		}
	}
	g.svc()
}

pub fn (mut g Gen) gen_arm64_exit(expr ast.Expr) {
	match expr {
		ast.IntegerLiteral {
			g.mov_arm(.x16, expr.val.u64())
		}
		else {
			g.n_error('native builtin exit expects a numeric argument')
		}
	}
	g.mov_arm(.x0, 0)
	g.svc()
}

fn (mut g Gen) gen_asm_stmt_arm64(asm_node ast.AsmStmt) {
	g.v_error('The asm statement for arm64 not yet implemented', asm_node.pos)
}
