// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module native

import os
import strings
import v.ast
import v.util
import v.mathutil as mu
import v.token
import v.errors
import v.pref
import term

interface CodeGen {
mut:
	g &Gen
	gen_exit(mut g Gen, expr ast.Expr)
	// XXX WHY gen_exit fn (expr ast.Expr)
}

[heap; minify]
pub struct Gen {
	out_name string
	pref     &pref.Preferences // Preferences shared from V struct
mut:
	code_gen             CodeGen
	table                &ast.Table
	buf                  []u8
	sect_header_name_pos int
	offset               i64
	file_size_pos        i64
	main_fn_addr         i64
	start_symbol_addr    i64
	code_start_pos       i64 // location of the start of the assembly instructions
	fn_addr              map[string]i64
	var_offset           map[string]int // local var stack offset
	var_alloc_size       map[string]int // local var allocation size
	stack_var_pos        int
	debug_pos            int
	errors               []errors.Error
	warnings             []errors.Warning
	syms                 []Symbol
	size_pos             []int
	nlines               int
	callpatches          []CallPatch
	strs                 []String
	labels               &LabelTable
	defer_stmts          []ast.DeferStmt
	builtins             map[string]BuiltinFn
	// macho specific
	macho_ncmds   int
	macho_cmdsize int
}

enum RelocType {
	rel8
	rel16
	rel32
	rel64
	abs64
}

struct String {
	str string
	pos int
	typ RelocType
}

struct CallPatch {
	name string
	pos  int
}

struct LabelTable {
mut:
	label_id int
	addrs    []i64 = [i64(0)] // register address of label here
	patches  []LabelPatch // push placeholders
	branches []BranchLabel
}

struct LabelPatch {
	id  int
	pos int
}

struct BranchLabel {
	name  string
	start int
	end   int
}

fn (mut l LabelTable) new_label() int {
	l.label_id++
	l.addrs << 0
	return l.label_id
}

enum Size {
	_8
	_16
	_32
	_64
}

// you can use these structs manually if you don't have ast.Ident
struct LocalVar {
	offset int // offset from the base pointer
	typ    ast.Type
	name   string
}

struct GlobalVar {}

[params]
struct VarConfig {
	offset int      // offset from the variable
	typ    ast.Type // type of the value you want to process e.g. struct fields.
}

type Var = GlobalVar | LocalVar | ast.Ident

fn (mut g Gen) get_var_from_ident(ident ast.Ident) LocalVar|GlobalVar|Register {
	mut obj := ident.obj
	if obj !in [ast.Var, ast.ConstField, ast.GlobalField, ast.AsmRegister] {
		obj = ident.scope.find(ident.name) or { g.n_error('unknown variable $ident.name') }
	}
	match obj {
		ast.Var {
			offset := g.get_var_offset(obj.name)
			typ := obj.typ
			return LocalVar{
				offset: offset
				typ: typ
				name: obj.name
			}
		}
		else {
			g.n_error('unsupported variable type type:$obj name:$ident.name')
		}
	}
}

fn (mut g Gen) get_type_from_var(var Var) ast.Type {
	match var {
		ast.Ident {
			return g.get_type_from_var(g.get_var_from_ident(var) as LocalVar)
		}
		LocalVar {
			return var.typ
		}
		GlobalVar {
			g.n_error('cannot get type from GlobalVar yet')
		}
	}
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
		._auto {
			$if amd64 {
				return Amd64{
					g: 0
				}
			} $else $if arm64 {
				return Arm64{
					g: 0
				}
			} $else {
				eprintln('-native only have amd64 and arm64 codegens')
				exit(1)
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
		labels: 0
	}
	g.code_gen.g = g
	g.generate_header()
	g.init_builtins()
	for file in files {
		/*
		if file.warnings.len > 0 {
			eprintln('warning: ${file.warnings[0]}')
		}
		*/
		if file.errors.len > 0 {
			g.n_error(file.errors[0].str())
		}
		g.stmts(file.stmts)
	}
	g.generate_builtins()
	g.generate_footer()
	return g.nlines, g.buf.len
}

pub fn (mut g Gen) typ(a int) &ast.TypeSymbol {
	return g.table.type_symbols[a]
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
		eprintln('\n$g.out_name: native binary has been successfully generated')
	}
}

pub fn (mut g Gen) generate_footer() {
	g.patch_calls()
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
		else {
			eprintln('Unsupported target file format')
			exit(1)
		}
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

fn (mut g Gen) write(bytes []u8) {
	for _, b in bytes {
		g.buf << b
	}
}

fn (mut g Gen) write8(n int) {
	// write 1 byte
	g.buf << u8(n)
}

fn (mut g Gen) write16(n int) {
	// write 2 bytes
	g.buf << u8(n)
	g.buf << u8(n >> 8)
}

fn (mut g Gen) read32_at(at int) int {
	return int(u32(g.buf[at]) | (u32(g.buf[at + 1]) << 8) | (u32(g.buf[at + 2]) << 16) | (u32(g.buf[
		at + 3]) << 24))
}

fn (mut g Gen) write32(n int) {
	// write 4 bytes
	g.buf << u8(n)
	g.buf << u8(n >> 8)
	g.buf << u8(n >> 16)
	g.buf << u8(n >> 24)
}

fn (mut g Gen) write64(n i64) {
	// write 8 bytes
	g.buf << u8(n)
	g.buf << u8(n >> 8)
	g.buf << u8(n >> 16)
	g.buf << u8(n >> 24)
	g.buf << u8(n >> 32)
	g.buf << u8(n >> 40)
	g.buf << u8(n >> 48)
	g.buf << u8(n >> 56)
}

fn (mut g Gen) write64_at(at i64, n i64) {
	// write 8 bytes
	g.buf[at] = u8(n)
	g.buf[at + 1] = u8(n >> 8)
	g.buf[at + 2] = u8(n >> 16)
	g.buf[at + 3] = u8(n >> 24)
	g.buf[at + 4] = u8(n >> 32)
	g.buf[at + 5] = u8(n >> 40)
	g.buf[at + 6] = u8(n >> 48)
	g.buf[at + 7] = u8(n >> 56)
}

fn (mut g Gen) write32_at(at i64, n int) {
	// write 4 bytes
	g.buf[at] = u8(n)
	g.buf[at + 1] = u8(n >> 8)
	g.buf[at + 2] = u8(n >> 16)
	g.buf[at + 3] = u8(n >> 24)
}

fn (mut g Gen) write16_at(at i64, n int) {
	// write 2 bytes
	g.buf[at] = u8(n)
	g.buf[at + 1] = u8(n >> 8)
}

fn (mut g Gen) write_string(s string) {
	for c in s {
		g.write8(int(c))
	}
	g.zeroes(1)
}

fn (mut g Gen) write_string_with_padding(s string, max int) {
	for c in s {
		g.write8(int(c))
	}
	for _ in 0 .. max - s.len {
		g.write8(0)
	}
}

fn (mut g Gen) try_var_offset(var_name string) int {
	offset := g.var_offset[var_name] or { return -1 }
	if offset == 0 {
		return -1
	}
	return offset
}

fn (mut g Gen) get_var_offset(var_name string) int {
	r := g.try_var_offset(var_name)
	if r == -1 {
		g.n_error('unknown variable `$var_name`')
	}
	return r
}

fn (mut g Gen) get_type_size(typ ast.Type) int {
	if typ in ast.number_type_idxs {
		return match typ {
			ast.i8_type_idx { 1 }
			ast.u8_type_idx { 1 }
			ast.i16_type_idx { 2 }
			ast.u16_type_idx { 2 }
			ast.int_type_idx { 4 }
			ast.u32_type_idx { 4 }
			ast.i64_type_idx { 8 }
			ast.u64_type_idx { 8 }
			ast.isize_type_idx { 8 }
			ast.usize_type_idx { 8 }
			ast.int_literal_type_idx { 8 }
			ast.f32_type_idx { 4 }
			ast.f64_type_idx { 8 }
			ast.float_literal_type_idx { 8 }
			ast.char_type_idx { 1 }
			ast.rune_type_idx { 4 }
			else { 8 }
		}
	}
	if typ in ast.pointer_type_idxs {
		return 8
	}
	if typ == ast.bool_type_idx {
		return 1
	}
	// g.n_error('unknown type size')
	return 0
}

fn (mut g Gen) get_sizeof_ident(ident ast.Ident) int {
	typ := match ident.obj {
		ast.AsmRegister { ast.i64_type_idx }
		ast.ConstField { ident.obj.typ }
		ast.GlobalField { ident.obj.typ }
		ast.Var { ident.obj.typ }
	}
	if typ != 0 {
		return g.get_type_size(typ)
	}
	size := g.var_alloc_size[ident.name] or {
		g.n_error('unknown variable `$ident`')
		return 0
	}
	return size
}

fn (mut g Gen) gen_typeof_expr(it ast.TypeOf, newline bool) {
	nl := if newline { '\n' } else { '' }
	r := g.typ(it.expr_type).name
	g.learel(.rax, g.allocate_string('$r$nl', 3, .rel32))
}

fn (mut g Gen) call_fn(node ast.CallExpr) {
	if g.pref.arch == .arm64 {
		g.call_fn_arm64(node)
	} else {
		g.call_fn_amd64(node)
	}
}

fn (mut g Gen) gen_var_to_string(reg Register, var Var, config VarConfig) {
	typ := g.get_type_from_var(var)
	if typ.is_int() {
		buffer := g.allocate_array('itoa-buffer', 1, 32) // 32 characters should be enough
		g.mov_var_to_reg(g.get_builtin_arg_reg('int_to_string', 0), var, config)
		g.lea_var_to_reg(g.get_builtin_arg_reg('int_to_string', 1), buffer)
		g.call_builtin('int_to_string')
		g.lea_var_to_reg(reg, buffer)
	} else {
		g.n_error('int-to-string conversion not implemented for type $typ')
	}
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
			vo := g.try_var_offset(expr.name)

			if vo != -1 {
				g.gen_var_to_string(.rax, expr as ast.Ident)
				g.gen_print_reg(.rax, 3, fd)
				if newline {
					g.gen_print('\n', fd)
				}
			} else {
				g.gen_print_reg(.rax, 3, fd)
			}
		}
		ast.IntegerLiteral {
			g.learel(.rax, g.allocate_string('$expr.val\n', 3, .rel32))
			g.gen_print_reg(.rax, 3, fd)
		}
		ast.BoolLiteral {
			// register 'true' and 'false' strings // g.expr(expr)
			// XXX mov64 shuoldnt be used for addressing
			if expr.val {
				g.learel(.rax, g.allocate_string('true', 3, .rel32))
			} else {
				g.learel(.rax, g.allocate_string('false', 3, .rel32))
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
						g.learel(.rax, g.allocate_string('$off\n', 3, .rel32))
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
		ast.EmptyExpr {
			g.n_error('unhandled EmptyExpr')
		}
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
		ast.NodeError {}
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
		ast.EnumVal {}
		ast.GoExpr {}
		ast.IfGuardExpr {}
		ast.IndexExpr {}
		ast.InfixExpr {}
		ast.IsRefType {}
		ast.MapInit {}
		ast.MatchExpr {}
		ast.OrExpr {}
		ast.ParExpr {}
		ast.RangeExpr {}
		ast.SelectExpr {}
		ast.SqlExpr {}
		ast.TypeNode {}
		*/
		ast.TypeOf {
			g.gen_typeof_expr(expr, newline)
		}
		ast.LockExpr {
			// passthru
			eprintln('Warning: locks not implemented yet in the native backend')
			g.expr(expr)
		}
		ast.Likely {
			// passthru
			g.expr(expr)
		}
		ast.UnsafeExpr {
			// passthru
			g.expr(expr)
		}
		ast.StringInterLiteral {
			g.n_error('Interlaced string literals are not yet supported in the native backend.') // , expr.pos)
		}
		else {
			dump(typeof(expr).name)
			dump(expr)
			//	g.v_error('expected string as argument for print', expr.pos)
			g.n_error('expected string as argument for print') // , expr.pos)
		}
	}
}

fn (mut g Gen) fn_decl(node ast.FnDecl) {
	if g.pref.is_verbose {
		println(term.green('\n$node.name:'))
	}
	if node.is_deprecated {
		g.warning('fn_decl: $node.name is deprecated', node.pos)
	}
	if node.is_builtin {
		g.warning('fn_decl: $node.name is builtin', node.pos)
	}
	g.stack_var_pos = 0
	g.register_function_address(node.name)
	g.labels = &LabelTable{}
	g.defer_stmts.clear()
	if g.pref.arch == .arm64 {
		g.fn_decl_arm64(node)
	} else {
		g.fn_decl_amd64(node)
	}
	g.patch_labels()
}

pub fn (mut g Gen) register_function_address(name string) {
	if name == 'main.main' {
		g.main_fn_addr = i64(g.buf.len)
	} else {
		g.fn_addr[name] = g.pos()
	}
}

fn (mut g Gen) println(comment string) {
	g.nlines++
	if !g.pref.is_verbose {
		return
	}
	addr := g.debug_pos.hex()
	mut sb := strings.new_builder(80)
	// println('$g.debug_pos "$addr"')
	sb.write_string(term.red(strings.repeat(`0`, 6 - addr.len) + addr + '  '))
	for i := g.debug_pos; i < g.pos(); i++ {
		s := g.buf[i].hex()
		if s.len == 1 {
			sb.write_string(term.blue('0'))
		}
		gbihex := g.buf[i].hex()
		hexstr := term.blue(gbihex) + ' '
		sb.write_string(hexstr)
	}
	g.debug_pos = g.buf.len
	//
	colored := sb.str()
	plain := term.strip_ansi(colored)
	padding := ' '.repeat(mu.max(1, 40 - plain.len))
	final := '$colored$padding$comment'
	println(final)
}

fn (mut g Gen) gen_forc_stmt(node ast.ForCStmt) {
	if node.has_init {
		g.stmts([node.init])
	}
	start := g.pos()
	start_label := g.labels.new_label()
	mut jump_addr := i64(0)
	if node.has_cond {
		cond := node.cond
		match cond {
			ast.InfixExpr {
				// g.infix_expr(node.cond)
				match cond.left {
					ast.Ident {
						lit := cond.right as ast.IntegerLiteral
						g.cmp_var(cond.left as ast.Ident, lit.val.int())
						match cond.op {
							.gt {
								jump_addr = g.cjmp(.jle)
							}
							.lt {
								jump_addr = g.cjmp(.jge)
							}
							else {
								g.n_error('unsupported conditional in for-c loop')
							}
						}
					}
					else {
						g.n_error('unhandled infix.left')
					}
				}
			}
			else {}
		}
		// dump(node.cond)
		g.expr(node.cond)
	}
	end_label := g.labels.new_label()
	g.labels.patches << LabelPatch{
		id: end_label
		pos: int(jump_addr)
	}
	g.println('; jump to label $end_label')
	g.labels.branches << BranchLabel{
		name: node.label
		start: start_label
		end: end_label
	}
	g.stmts(node.stmts)
	g.labels.addrs[start_label] = g.pos()
	g.println('; label $start_label')
	if node.has_inc {
		g.stmts([node.inc])
	}
	g.labels.branches.pop()
	g.jmp(int(0xffffffff - (g.pos() + 5 - start) + 1))
	g.labels.addrs[end_label] = g.pos()
	g.println('; jump to label $end_label')

	// loop back
}

fn (mut g Gen) for_in_stmt(node ast.ForInStmt) {
	if node.stmts.len == 0 {
		// if no statements, just dont make it
		return
	}
	if node.is_range {
		// for a in node.cond .. node.high {
		i := g.allocate_var(node.val_var, 8, 0) // iterator variable
		g.expr(node.cond)
		g.mov_reg_to_var(LocalVar{i, ast.i64_type_idx, node.val_var}, .rax) // i = node.cond // initial value
		start := g.pos() // label-begin:
		start_label := g.labels.new_label()
		g.mov_var_to_reg(.rbx, LocalVar{i, ast.i64_type_idx, node.val_var}) // rbx = iterator value
		g.expr(node.high) // final value
		g.cmp_reg(.rbx, .rax) // rbx = iterator, rax = max value
		jump_addr := g.cjmp(.jge) // leave loop if i is beyond end
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
		g.labels.addrs[start_label] = g.pos()
		g.println('; label $start_label')
		g.inc_var(LocalVar{i, ast.i64_type_idx, node.val_var})
		g.labels.branches.pop()
		g.jmp(int(0xffffffff - (g.pos() + 5 - start) + 1))
		g.labels.addrs[end_label] = g.pos()
		g.println('; label $end_label')
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
		g.v_error('for-in statement is not yet implemented', node.pos)
	}
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
		ast.BranchStmt {
			label_name := node.label
			for i := g.labels.branches.len - 1; i >= 0; i-- {
				branch := g.labels.branches[i]
				if label_name == '' || label_name == branch.name {
					label := if node.kind == .key_break {
						branch.end
					} else { // continue
						branch.start
					}
					jump_addr := g.jmp(0)
					g.labels.patches << LabelPatch{
						id: label
						pos: jump_addr
					}
					g.println('; jump to $label: $node.kind')
					break
				}
			}
		}
		ast.ConstDecl {}
		ast.DeferStmt {
			name := '_defer$g.defer_stmts.len'
			defer_var := g.get_var_offset(name)
			g.mov_int_to_var(LocalVar{defer_var, ast.i64_type_idx, name}, 1)
			g.defer_stmts << node
			g.defer_stmts[g.defer_stmts.len - 1].idx_in_fn = g.defer_stmts.len - 1
		}
		ast.ExprStmt {
			g.expr(node.expr)
		}
		ast.FnDecl {
			g.fn_decl(node)
		}
		ast.ForCStmt {
			g.gen_forc_stmt(node)
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
				// b := word.u8()
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
			// TODO: void return
			if e0 := node.exprs[0] {
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
						g.mov64(.rax, g.allocate_string(s, 2, .abs64))
					}
					ast.Ident {
						g.expr(e0)
					}
					else {
						size := g.get_type_size(node.types[0])
						if size !in [1, 2, 4, 8] {
							g.n_error('unknown return type $e0.type_name()')
						}
						g.expr(e0)
					}
				}
			}

			// jump to return label
			label := 0
			pos := g.jmp(0)
			g.labels.patches << LabelPatch{
				id: label
				pos: pos
			}
			g.println('; jump to label $label')
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
							g.allocate_string(s, 2, .abs64)
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
				g.allocate_string(s, 2, .abs64)
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
		ast.BoolLiteral {
			g.mov64(.rax, if node.val { 1 } else { 0 })
		}
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
		ast.Ident {
			// XXX this is intel specific
			g.mov_var_to_reg(.rax, node as ast.Ident)
		}
		ast.IfExpr {
			if node.is_comptime {
				eprintln('Warning: ignored compile time conditional not yet supported for the native backend.')
			} else {
				g.if_expr(node)
			}
		}
		ast.InfixExpr {
			g.infix_expr(node)
			// get variable by name
			// save the result in rax
		}
		ast.IntegerLiteral {
			g.mov64(.rax, node.val.int())
			// g.gen_print_reg(.rax, 3, fd)
		}
		ast.PostfixExpr {
			g.postfix_expr(node)
		}
		ast.StringLiteral {
			g.allocate_string(node.val, 3, .rel32)
		}
		ast.StructInit {}
		ast.GoExpr {
			g.v_error('native backend doesnt support threads yet', node.pos)
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
	match node.op {
		.inc {
			g.inc_var(ident)
		}
		.dec {
			g.dec_var(ident)
		}
		else {}
	}
}

[noreturn]
pub fn (mut g Gen) n_error(s string) {
	util.verror('native error', s)
}

pub fn (mut g Gen) warning(s string, pos token.Pos) {
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

pub fn (mut g Gen) v_error(s string, pos token.Pos) {
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
