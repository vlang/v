// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module x64

import (
	v.ast
	// term
)

pub struct Gen {
	out_name             string
mut:
	buf                  []byte
	sect_header_name_pos int
	offset               i64
	str_pos              []i64
	strings              []string // TODO use a map and don't duplicate strings
	file_size_pos        i64
	main_fn_addr         i64
	code_start_pos       i64 // location of the start of the assembly instructions
	fn_addr              map[string]i64
	// string_addr map[string]i64
}

enum Register {
	eax
	edi
	rax
	rdi
	rsi
	edx
	rdx
	r12
}

enum Size {
	_8
	_16
	_32
	_64
}

pub fn gen(files []ast.File, out_name string) {
	mut g := Gen{
		sect_header_name_pos: 0
		// buf: []
		
		out_name: out_name
	}
	g.generate_elf_header()
	for file in files {
		for stmt in file.stmts {
			g.stmt(stmt)
			g.writeln('')
		}
	}
	g.generate_elf_footer()
}

/*
pub fn new_gen(out_name string) &Gen {
	return &Gen{
		sect_header_name_pos: 0
		buf: []
		out_name: out_name
	}
}
*/


pub fn (g &Gen) pos() i64 {
	return g.buf.len
}

fn (g mut Gen) write8(n int) {
	// write 1 byte
	g.buf << byte(n)
}

fn (g mut Gen) write16(n int) {
	// write 2 bytes
	g.buf << byte(n)
	g.buf << byte(n>>8)
}

fn (g mut Gen) write32(n int) {
	// write 4 bytes
	g.buf << byte(n)
	g.buf << byte(n>>8)
	g.buf << byte(n>>16)
	g.buf << byte(n>>24)
}

fn (g mut Gen) write64(n i64) {
	// write 8 bytes
	g.buf << byte(n)
	g.buf << byte(n>>8)
	g.buf << byte(n>>16)
	g.buf << byte(n>>24)
	g.buf << byte(n>>32)
	g.buf << byte(n>>40)
	g.buf << byte(n>>48)
	g.buf << byte(n>>56)
}

fn (g mut Gen) write64_at(n i64, at i64) {
	// write 8 bytes
	g.buf[at] = byte(n)
	g.buf[at + 1] = byte(n>>8)
	g.buf[at + 2] = byte(n>>16)
	g.buf[at + 3] = byte(n>>24)
	g.buf[at + 4] = byte(n>>32)
	g.buf[at + 5] = byte(n>>40)
	g.buf[at + 6] = byte(n>>48)
	g.buf[at + 7] = byte(n>>56)
}

fn (g mut Gen) write_string(s string) {
	for c in s {
		g.write8(int(c))
	}
}

fn (g mut Gen) inc(reg Register) {
	g.write16(0xff49)
	match reg {
		.r12 {
			g.write8(0xc4)
		}
		else {
			panic('unhandled inc $reg')
		}
	}
}

fn (g mut Gen) cmp(reg Register, size Size, val i64) {
	g.write8(0x49)
	// Second byte depends on the size of the value
	match size {
		._8 {
			g.write8(0x83)
		}
		._32 {
			g.write8(0x81)
		}
		else {
			panic('unhandled cmp')
		}
	}
	// Third byte depends on the register being compared to
	match reg {
		.r12 {
			g.write8(0xfc)
		}
		else {
			panic('unhandled cmp')
		}
	}
	g.write8(int(val))
}

fn abs(a i64) i64 {
	return if a < 0 { -a } else { a }
}

fn (g mut Gen) jle(addr i64) {
	// Calculate the relative offset to jump to
	// (`addr` is absolute address)
	offset := 0xff - int(abs(addr - g.buf.len)) - 1
	g.write8(0x7e)
	g.write8(offset)
}

fn (g mut Gen) jl(addr i64) {
	offset := 0xff - int(abs(addr - g.buf.len)) - 1
	g.write8(0x7c)
	g.write8(offset)
}

fn (g &Gen) abs_to_rel_addr(addr i64) int {
	return int(abs(addr - g.buf.len)) - 1
}

fn (g mut Gen) jmp(addr i64) {
	offset := 0xff - g.abs_to_rel_addr(addr)
	g.write8(0xe9)
	g.write8(offset)
}

fn (g mut Gen) mov64(reg Register, val i64) {
	match reg {
		.rsi {
			g.write8(0x48)
			g.write8(0xbe)
		}
		else {
			println('unhandled mov $reg')
		}
	}
	g.write64(val)
}

fn (g mut Gen) call(addr int) {
	// rel := g.abs_to_rel_addr(addr)
	// rel := 0xffffffff - int(abs(addr - g.buf.len))-1
	println('call addr=$addr rel_addr=$addr pos=$g.buf.len')
	g.write8(0xe8)
	g.write32(addr)
}

fn (g mut Gen) syscall() {
	// g.write(0x050f)
	g.write8(0x0f)
	g.write8(0x05)
}

pub fn (g mut Gen) ret() {
	g.write8(0xc3)
}

// returns label's relative address
pub fn (g mut Gen) gen_loop_start(from int) int {
	g.mov(.r12, from)
	label := g.buf.len
	g.inc(.r12)
	return label
}

pub fn (g mut Gen) gen_loop_end(to int, label int) {
	g.cmp(.r12, ._8, to)
	g.jl(label)
}

pub fn (g mut Gen) save_main_fn_addr() {
	g.main_fn_addr = g.buf.len
}

pub fn (g mut Gen) gen_print_from_expr(expr ast.Expr) {
	match expr {
		ast.StringLiteral {
			g.gen_print(it.val)
		}
		else {}
	}
}

pub fn (g mut Gen) gen_print(s string) {
	//
	// qq := s + '\n'
	//
	g.strings << s + '\n'
	// g.string_addr[s] = str_pos
	g.mov(.eax, 1)
	g.mov(.edi, 1)
	str_pos := g.buf.len + 2
	g.str_pos << str_pos
	g.mov64(.rsi, 0) // segment_start +  0x9f) // str pos // PLACEHOLDER
	g.mov(.edx, s.len + 1) // len
	g.syscall()
}

pub fn (g mut Gen) gen_exit() {
	// Return 0
	g.mov(.edi, 0) // ret value
	g.mov(.eax, 60)
	g.syscall()
}

fn (g mut Gen) mov(reg Register, val int) {
	match reg {
		.eax {
			g.write8(0xb8)
		}
		.edi {
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
}

pub fn (g mut Gen) register_function_address(name string) {
	addr := g.pos()
	// println('reg fn addr $name $addr')
	g.fn_addr[name] = addr
}

pub fn (g &Gen) write(s string) {}

pub fn (g &Gen) writeln(s string) {}

pub fn (g mut Gen) call_fn(name string) {
	if !name.contains('__') {
		return
	}
	addr := g.fn_addr[name]
	g.call(int(addr))
	println('call $name $addr')
}

fn (g mut Gen) stmt(node ast.Stmt) {
	match node {
		ast.ConstDecl {}
		ast.FnDecl {
			is_main := it.name == 'main'
			if is_main {
				g.save_main_fn_addr()
			}
			for arg in it.args {}
			for stmt in it.stmts {
				g.stmt(stmt)
			}
			if is_main {
				println('end of main: gen exit')
				g.gen_exit()
			}
			g.ret()
		}
		ast.Return {}
		ast.AssignStmt {}
		ast.VarDecl {}
		ast.ForStmt {}
		ast.StructDecl {}
		ast.ExprStmt {
			g.expr(it.expr)
		}
		else {
			verror('x64.stmt(): bad node')
		}
	}
}

fn (g mut Gen) expr(node ast.Expr) {
	// println('cgen expr()')
	match node {
		ast.AssignExpr {}
		ast.IntegerLiteral {}
		ast.FloatLiteral {}
		/*
		ast.UnaryExpr {
			g.expr(it.left)
		}
		*/

		ast.StringLiteral {}
		ast.InfixExpr {}
		// `user := User{name: 'Bob'}`
		ast.StructInit {}
		ast.CallExpr {
			if it.name == 'println' || it.name == 'print' {
				expr := it.args[0]
				g.gen_print_from_expr(expr)
			}
			/*
			g.write('${it.name}(')
			for i, expr in it.args {
				g.expr(expr)
				if i != it.args.len - 1 {
					g.write(', ')
				}
			}
			g.write(')')
			*/

		}
		ast.ArrayInit {}
		ast.Ident {}
		ast.BoolLiteral {}
		ast.IfExpr {}
		else {
			// println(term.red('x64.expr(): bad node'))
			}
	}
	}

	fn verror(s string) {
		println(s)
		exit(1)
	}
