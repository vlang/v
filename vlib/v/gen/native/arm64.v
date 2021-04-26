module native

import v.ast

enum Arm64Register {
	x0
	x1
	x2
	x3
	x4
	x5
	x6
	x7
	x8
	x9
	x10
	x11
	x12
	x13
	x14
	x15
	x16
}

pub struct Arm64 {
mut:
	// arm64 specific stuff for code generation
	g Gen
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
	if r == 0 && val == 1 {
		g.write32(0xd2800020)
		g.println('mov x0, 1')
	} else if r == 0 {
		g.write32(0xd2800000)
		g.println('mov x0, 0')
	} else if r == 16 {
		g.write32(0xd2800030)
		g.println('mov x16, 1')
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

pub fn (mut g Gen) fn_decl_arm64(node ast.FnDecl) {
	// TODO
}

fn (mut g Gen) gen_arm64_helloworld() {
	// g.write32(0x77777777)
	// assembly
	g.mov_arm(.x0, 1)
	g.adr()
	g.bl()
	g.mov_arm(.x0, 0)
	g.mov_arm(.x16, 1)
	g.svc()
	//
	g.write_string('Hello World!\n')
	g.write8(0) // padding?
	g.write8(0)
	g.write8(0)
}

fn (mut g Gen) adr() {
	g.write32(0x100000a0)
	g.println('adr x0, 0x14')
}

fn (mut g Gen) bl() {
	// g.write32(0xa9400000)
	g.write32(0x94000000)
	g.println('bl 0')
}

fn (mut g Gen) svc() {
	g.write32(0xd4001001)
	g.println('svc')
}

pub fn (mut g Gen) gen_arm64_exit(expr ast.Expr) {
	match expr {
		ast.IntegerLiteral {
			g.mov_arm(.x16, expr.val.u64())
		}
		else {
			verror('native builtin exit expects a numeric argument')
		}
	}
	g.mov_arm(.x0, 0)
	g.svc()
}
