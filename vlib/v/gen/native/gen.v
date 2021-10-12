// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module native

import os
import strings
import v.ast
import v.util
import v.token
import v.errors
import v.pref
import term

pub const builtins = ['assert', 'print', 'eprint', 'println', 'eprintln', 'exit', 'C.syscall']

interface CodeGen {
mut:
	g &Gen
	gen_exit(mut g Gen, expr ast.Expr)
	// XXX WHY gen_exit fn (expr ast.Expr)
}

[heap]
pub struct Gen {
	out_name string
	pref     &pref.Preferences // Preferences shared from V struct
mut:
	code_gen             CodeGen
	table                &ast.Table
	buf                  []byte
	sect_header_name_pos int
	offset               i64
	str_pos              []i64
	stackframe_size      int
	strings              []string // TODO use a map and don't duplicate strings
	file_size_pos        i64
	main_fn_addr         i64
	code_start_pos       i64 // location of the start of the assembly instructions
	fn_addr              map[string]i64
	var_offset           map[string]int // local var stack offset
	stack_var_pos        int
	debug_pos            int
	errors               []errors.Error
	warnings             []errors.Warning
	syms                 []Symbol
	relocs               []Reloc
	size_pos             []int
	nlines               int
}

enum Size {
	_8
	_16
	_32
	_64
}

fn get_backend(arch pref.Arch) ?CodeGen {
	match arch {
		.arm64 {
			return Arm64{
				g: 0
			}
		}
		.amd64 {
			return Amd64{
				g: 0
			}
		}
		else {}
	}
	return error('unsupported architecture')
}

pub fn gen(files []&ast.File, table &ast.Table, out_name string, pref &pref.Preferences) (int, int) {
	exe_name := if pref.os == .windows && !out_name.ends_with('.exe') {
		out_name + '.exe'
	} else {
		out_name
	}
	mut g := &Gen{
		table: table
		sect_header_name_pos: 0
		out_name: exe_name
		pref: pref
		// TODO: workaround, needs to support recursive init
		code_gen: get_backend(pref.arch) or {
			eprintln('No available backend for this configuration. Use `-a arm64` or `-a amd64`.')
			exit(1)
		}
	}
	g.code_gen.g = g
	g.generate_header()
	for file in files {
		if file.warnings.len > 0 {
			eprintln('warning: ${file.warnings[0]}')
		}
		if file.errors.len > 0 {
			g.n_error(file.errors[0].str())
		}
		g.stmts(file.stmts)
	}
	g.generate_footer()
	return g.nlines, g.buf.len
}

pub fn (mut g Gen) typ(a int) &ast.TypeSymbol {
	return &g.table.type_symbols[a]
}

pub fn (mut g Gen) generate_header() {
	match g.pref.os {
		.macos {
			g.generate_macho_header()
		}
		.windows {
			g.generate_pe_header()
		}
		.linux {
			g.generate_elf_header()
		}
		.raw {
			if g.pref.arch == .arm64 {
				g.gen_arm64_helloworld()
			}
		}
		else {
			g.n_error('only `raw`, `linux` and `macos` are supported for -os in -native')
		}
	}
}

pub fn (mut g Gen) create_executable() {
	// Create the binary // should be .o ?
	os.write_file_array(g.out_name, g.buf) or { panic(err) }
	os.chmod(g.out_name, 0o775) or { panic(err) } // make it executable
	if g.pref.is_verbose {
		println('\n$g.out_name: native binary has been successfully generated')
	}
}

pub fn (mut g Gen) generate_footer() {
	match g.pref.os {
		.macos {
			g.generate_macho_footer()
		}
		.windows {
			g.generate_pe_footer()
		}
		.linux {
			g.generate_elf_footer()
		}
		.raw {
			g.create_executable()
		}
		else {}
	}
}

pub fn (mut g Gen) stmts(stmts []ast.Stmt) {
	for stmt in stmts {
		g.stmt(stmt)
	}
}

pub fn (g &Gen) pos() i64 {
	return g.buf.len
}

fn (mut g Gen) write8(n int) {
	// write 1 byte
	g.buf << byte(n)
}

fn (mut g Gen) write16(n int) {
	// write 2 bytes
	g.buf << byte(n)
	g.buf << byte(n >> 8)
}

fn (mut g Gen) read32_at(at int) int {
	return int(g.buf[at] | (g.buf[at + 1] << 8) | (g.buf[at + 2] << 16) | (g.buf[at + 3] << 24))
}

fn (mut g Gen) write32(n int) {
	// write 4 bytes
	g.buf << byte(n)
	g.buf << byte(n >> 8)
	g.buf << byte(n >> 16)
	g.buf << byte(n >> 24)
}

fn (mut g Gen) write64(n i64) {
	// write 8 bytes
	g.buf << byte(n)
	g.buf << byte(n >> 8)
	g.buf << byte(n >> 16)
	g.buf << byte(n >> 24)
	g.buf << byte(n >> 32)
	g.buf << byte(n >> 40)
	g.buf << byte(n >> 48)
	g.buf << byte(n >> 56)
}

fn (mut g Gen) write64_at(n i64, at i64) {
	// write 8 bytes
	g.buf[at] = byte(n)
	g.buf[at + 1] = byte(n >> 8)
	g.buf[at + 2] = byte(n >> 16)
	g.buf[at + 3] = byte(n >> 24)
	g.buf[at + 4] = byte(n >> 32)
	g.buf[at + 5] = byte(n >> 40)
	g.buf[at + 6] = byte(n >> 48)
	g.buf[at + 7] = byte(n >> 56)
}

fn (mut g Gen) write32_at(at i64, n int) {
	// write 4 bytes
	g.buf[at] = byte(n)
	g.buf[at + 1] = byte(n >> 8)
	g.buf[at + 2] = byte(n >> 16)
	g.buf[at + 3] = byte(n >> 24)
}

fn (mut g Gen) write_string(s string) {
	for c in s {
		g.write8(int(c))
	}
	// g.write8(0) // null terminated strings
}

fn (mut g Gen) write_string_with_padding(s string, max int) {
	for c in s {
		g.write8(int(c))
	}
	for _ in 0 .. max - s.len {
		g.write8(0)
	}
}

fn (mut g Gen) get_var_offset(var_name string) int {
	offset := g.var_offset[var_name]
	if offset == 0 {
		g.n_error('unknown variable `$var_name`')
	}
	return offset
}

pub fn (mut g Gen) gen_print_from_expr(expr ast.Expr, name string) {
	newline := name in ['println', 'eprintln']
	fd := if name in ['eprint', 'eprintln'] { 2 } else { 1 }
	match expr {
		ast.StringLiteral {
			if newline {
				g.gen_print(expr.val + '\n', fd)
			} else {
				g.gen_print(expr.val, fd)
			}
		}
		ast.CallExpr {
			g.call_fn(expr)
			g.gen_print_reg(.rax, 3, fd)
		}
		ast.Ident {
			g.expr(expr)
			g.gen_print_reg(.rax, 3, fd)
		}
		ast.IntegerLiteral {
			g.mov64(.rax, g.allocate_string('$expr.val\n', 2))
			g.gen_print_reg(.rax, 3, fd)
		}
		ast.BoolLiteral {
			// register 'true' and 'false' strings // g.expr(expr)
			if expr.val {
				g.mov64(.rax, g.allocate_string('true', 2))
			} else {
				g.mov64(.rax, g.allocate_string('false', 2))
			}
			g.gen_print_reg(.rax, 3, fd)
		}
		ast.SizeOf {}
		ast.OffsetOf {
			styp := g.typ(expr.struct_type)
			field_name := expr.field
			if styp.kind == .struct_ {
				s := styp.info as ast.Struct
				ptrsz := 4 // should be 8, but for locals is used 8 and C backend shows that too
				mut off := 0
				for f in s.fields {
					if f.name == field_name {
						g.mov64(.rax, g.allocate_string('$off\n', 2))
						g.gen_print_reg(.rax, 3, fd)
						break
					}
					off += ptrsz
				}
			} else {
				g.v_error('_offsetof expects a struct Type as first argument', expr.pos)
			}
		}
		ast.None {}
		ast.PostfixExpr {}
		ast.PrefixExpr {}
		ast.SelectorExpr {
			// struct.field
			g.expr(expr)
			g.gen_print_reg(.rax, 3, fd)
			/*
			field_name := expr.field_name
g.expr
			if expr.is_mut {
				// mutable field access (rw)
			}
			*/
			dump(expr)
			g.v_error('struct.field selector not yet implemented for this backend', expr.pos)
		}
		/*
		ast.AnonFn {}
		ast.ArrayDecompose {}
		ast.ArrayInit {}
		ast.AsCast {}
		ast.Assoc {}
		ast.AtExpr {}
		ast.CTempVar {}
		ast.CastExpr {}
		ast.ChanInit {}
		ast.CharLiteral {}
		ast.Comment {}
		ast.ComptimeCall {}
		ast.ComptimeSelector {}
		ast.ConcatExpr {}
		ast.DumpExpr {}
		ast.EmptyExpr {}
		ast.EnumVal {}
		ast.FloatLiteral {}
		ast.GoExpr {}
		ast.IfExpr {}
		ast.IfGuardExpr {}
		ast.IndexExpr {}
		ast.InfixExpr {}
		ast.IsRefType {}
		ast.Likely {}
		ast.LockExpr {}
		ast.MapInit {}
		ast.MatchExpr {}
		ast.NodeError {}
		ast.OrExpr {}
		ast.ParExpr {}
		ast.RangeExpr {}
		ast.SelectExpr {}
		ast.SqlExpr {}
		ast.StringInterLiteral {}
		ast.StructInit {}
		ast.TypeNode {}
		ast.TypeOf {}
		ast.UnsafeExpr {}
		*/
		else {
			dump(typeof(expr).name)
			dump(expr)
			//	g.v_error('expected string as argument for print', expr.pos)
			g.n_error('expected string as argument for print') // , expr.pos)
			// g.warning('expected string as argument for print')
		}
	}
}

pub fn (mut g Gen) register_function_address(name string) {
	addr := g.pos()
	// eprintln('register function $name = $addr')
	g.fn_addr[name] = addr
}

fn (mut g Gen) println(comment string) {
	g.nlines++
	if !g.pref.is_verbose {
		return
	}
	addr := g.debug_pos.hex()
	// println('$g.debug_pos "$addr"')
	print(term.red(strings.repeat(`0`, 6 - addr.len) + addr + '  '))
	for i := g.debug_pos; i < g.pos(); i++ {
		s := g.buf[i].hex()
		if s.len == 1 {
			print(term.blue('0'))
		}
		gbihex := g.buf[i].hex()
		hexstr := term.blue(gbihex) + ' '
		print(hexstr)
	}
	g.debug_pos = g.buf.len
	println(' ' + comment)
}

fn (mut g Gen) for_in_stmt(node ast.ForInStmt) {
	g.v_error('for-in statement is not yet implemented', node.pos)
	/*
	if node.is_range {
		// `for x in 1..10 {`
		// i := if node.val_var == '_' { g.new_tmp_var() } else { c_name(node.val_var) }
		// val_typ := g.table.mktyp(node.val_type)
		g.write32(0x3131) // 'for (${g.typ(val_typ)} $i = ')
		g.expr(node.cond)
		g.write32(0x3232) // ; $i < ')
		g.expr(node.high)
		g.write32(0x3333) // '; ++$i) {')
		} else if node.kind == .array {
	} else if node.kind == .array_fixed {
	} else if node.kind == .map {
	} else if node.kind == .string {
	} else if node.kind == .struct_ {
	}
	*/
}

pub fn (mut g Gen) gen_exit(node ast.Expr) {
	// check node type and then call the code_gen method
	g.code_gen.gen_exit(mut g, node)
}

fn (mut g Gen) stmt(node ast.Stmt) {
	match node {
		ast.AssignStmt {
			g.assign_stmt(node)
		}
		ast.Block {
			g.stmts(node.stmts)
		}
		ast.ConstDecl {}
		ast.ExprStmt {
			g.expr(node.expr)
		}
		ast.FnDecl {
			g.fn_decl(node)
		}
		ast.ForInStmt {
			g.for_in_stmt(node)
		}
		ast.ForStmt {
			g.for_stmt(node)
		}
		ast.HashStmt {
			words := node.val.split(' ')
			for word in words {
				if word.len != 2 {
					g.n_error('opcodes format: xx xx xx xx')
				}
				b := unsafe { C.strtol(&char(word.str), 0, 16) }
				// b := word.byte()
				// println('"$word" $b')
				g.write8(b)
			}
		}
		ast.Module {}
		ast.Return {
			// dump(node.exprs[0])
			// if in main
			// zero := ast.IntegerLiteral{}
			// g.gen_exit(zero)
			// dump(node)
			// dump(node.types)
			mut s := '?' //${node.exprs[0].val.str()}'
			e0 := node.exprs[0]
			match e0 {
				ast.IntegerLiteral {
					g.mov64(.rax, e0.val.int())
				}
				ast.InfixExpr {
					g.infix_expr(e0)
				}
				ast.CastExpr {
					g.mov64(.rax, e0.expr.str().int())
					// do the job
				}
				ast.StringLiteral {
					s = e0.val.str()
					g.expr(node.exprs[0])
					g.mov64(.rax, g.allocate_string(s, 2))
				}
				ast.Ident {
					g.expr(e0)
				}
				else {
					g.n_error('unknown return type $e0.type_name()')
				}
			}
			// intel specific
			g.add8(.rsp, g.stackframe_size)
			g.pop(.rbp)
			g.ret()
		}
		ast.AsmStmt {
			g.gen_asm_stmt(node)
		}
		ast.AssertStmt {
			g.gen_assert(node)
		}
		ast.Import {} // do nothing here
		ast.StructDecl {}
		else {
			eprintln('native.stmt(): bad node: ' + node.type_name())
		}
	}
}

fn C.strtol(str &char, endptr &&char, base int) int

fn (mut g Gen) gen_syscall(node ast.CallExpr) {
	mut i := 0
	mut ra := [Register.rax, .rdi, .rsi, .rdx]
	for i < node.args.len {
		expr := node.args[i].expr
		if i >= ra.len {
			g.warning('Too many arguments for syscall', node.pos)
			return
		}
		match expr {
			ast.IntegerLiteral {
				g.mov(ra[i], expr.val.int())
			}
			ast.BoolLiteral {
				g.mov(ra[i], if expr.val { 1 } else { 0 })
			}
			ast.SelectorExpr {
				mut done := false
				if expr.field_name == 'str' {
					match expr.expr {
						ast.StringLiteral {
							s := expr.expr.val.replace('\\n', '\n')
							g.allocate_string(s, 2)
							g.mov64(ra[i], 1)
							done = true
						}
						else {}
					}
				}
				if !done {
					g.v_error('Unknown selector in syscall argument type $expr', node.pos)
				}
			}
			ast.StringLiteral {
				if expr.language != .c {
					g.warning('C.syscall expects c"string" or "string".str, C backend will crash',
						node.pos)
				}
				s := expr.val.replace('\\n', '\n')
				g.allocate_string(s, 2)
				g.mov64(ra[i], 1)
			}
			else {
				g.v_error('Unknown syscall $expr.type_name() argument type $expr', node.pos)
				return
			}
		}
		i++
	}
	g.syscall()
}

fn (mut g Gen) expr(node ast.Expr) {
	match node {
		ast.ParExpr {
			g.expr(node.expr)
		}
		ast.ArrayInit {
			g.n_error('array init expr not supported yet')
		}
		ast.BoolLiteral {}
		ast.CallExpr {
			if node.name == 'C.syscall' {
				g.gen_syscall(node)
			} else if node.name == 'exit' {
				g.gen_exit(node.args[0].expr)
			} else if node.name in ['println', 'print', 'eprintln', 'eprint'] {
				expr := node.args[0].expr
				g.gen_print_from_expr(expr, node.name)
			} else {
				g.call_fn(node)
			}
		}
		ast.FloatLiteral {}
		ast.Ident {}
		ast.IfExpr {
			if node.is_comptime {
				eprintln('Warning: ignored compile time conditional not yet supported for the native backend.')
			} else {
				g.if_expr(node)
			}
		}
		ast.InfixExpr {}
		ast.IntegerLiteral {}
		ast.PostfixExpr {
			g.postfix_expr(node)
		}
		ast.StringLiteral {}
		ast.StructInit {}
		ast.GoExpr {
			g.v_error('native backend doesnt support threads yet', node.pos) // token.Position{})
		}
		else {
			g.n_error('expr: unhandled node type: $node.type_name()')
		}
	}
}

/*
fn (mut g Gen) allocate_var(name string, size int, initial_val int) {
	g.code_gen.allocate_var(name, size, initial_val)
}
*/

fn (mut g Gen) postfix_expr(node ast.PostfixExpr) {
	if node.expr !is ast.Ident {
		return
	}
	ident := node.expr as ast.Ident
	var_name := ident.name
	if node.op == .inc {
		g.inc_var(var_name)
	}
}

[noreturn]
pub fn (mut g Gen) n_error(s string) {
	util.verror('native error', s)
}

pub fn (mut g Gen) warning(s string, pos token.Position) {
	if g.pref.output_mode == .stdout {
		werror := util.formatted_error('warning', s, g.pref.path, pos)
		eprintln(werror)
	} else {
		g.warnings << errors.Warning{
			file_path: g.pref.path
			pos: pos
			reporter: .gen
			message: s
		}
	}
}

pub fn (mut g Gen) v_error(s string, pos token.Position) {
	// TODO: store a file index in the Position too,
	// so that the file path can be retrieved from the pos, instead
	// of guessed from the pref.path ...
	mut kind := 'error:'
	if g.pref.output_mode == .stdout {
		ferror := util.formatted_error(kind, s, g.pref.path, pos)
		eprintln(ferror)
		exit(1)
	} else {
		g.errors << errors.Error{
			file_path: g.pref.path
			pos: pos
			reporter: .gen
			message: s
		}
	}
}
