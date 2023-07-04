// Copyright (c) 2019-2023 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module native

import os
import strings
import v.ast
import v.ast.walker
import v.util
import v.mathutil as mu
import v.token
import v.errors
import v.pref
import v.eval
import term
import strconv

[heap; minify]
pub struct Gen {
	out_name string
	pref     &pref.Preferences = unsafe { nil } // Preferences shared from V struct
	files    []&ast.File
mut:
	code_gen             CodeGen
	table                &ast.Table = unsafe { nil }
	buf                  []u8
	sect_header_name_pos int
	offset               i64
	file_size_pos        i64
	elf_text_header_addr i64 = -1
	elf_rela_section     Section
	main_fn_addr         i64
	main_fn_size         i64
	start_symbol_addr    i64
	code_start_pos       i64 // location of the start of the assembly instructions
	symbol_table         []SymbolTableSection
	extern_symbols       []string
	extern_fn_calls      map[i64]string
	fn_addr              map[string]i64
	var_offset           map[string]int // local var stack offset
	var_alloc_size       map[string]int // local var allocation size
	stack_var_pos        int
	stack_depth          int
	debug_pos            int
	errors               []errors.Error
	warnings             []errors.Warning
	syms                 []Symbol
	size_pos             []int
	nlines               int
	callpatches          []CallPatch
	strs                 []String
	labels               &LabelTable = unsafe { nil }
	defer_stmts          []ast.DeferStmt
	builtins             map[Builtin]BuiltinFn
	structs              []Struct
	eval                 eval.Eval
	enum_vals            map[string]Enum
	return_type          ast.Type
	// macho specific
	macho_ncmds   int
	macho_cmdsize int

	requires_linking bool
}

interface CodeGen {
mut:
	g &Gen
	address_size() int
	adr(r Arm64Register, delta int) // Note: Temporary!
	allocate_var(name string, size int, initial_val int) int
	apicall(call ApiCall) // winapi calls
	assign_stmt(node ast.AssignStmt) // TODO: make platform-independant
	builtin_decl(builtin BuiltinFn)
	call_addr_at(addr int, at i64) i64
	call_builtin(name Builtin) i64
	call_fn(node ast.CallExpr)
	call(addr int) i64
	cjmp(op JumpOp) int
	cmp_to_stack_top(r Register)
	cmp_var_reg(var Var, reg Register, config VarConfig)
	cmp_var(var Var, val int, config VarConfig)
	cmp_zero(reg Register)
	convert_bool_to_string(r Register)
	convert_int_to_string(a Register, b Register)
	convert_rune_to_string(r Register, buffer int, var Var, config VarConfig)
	dec_var(var Var, config VarConfig)
	fn_decl(node ast.FnDecl)
	gen_asm_stmt(asm_node ast.AsmStmt)
	gen_assert(assert_node ast.AssertStmt)
	gen_cast_expr(expr ast.CastExpr)
	gen_concat_expr(expr ast.ConcatExpr)
	gen_exit(expr ast.Expr)
	gen_match_expr(expr ast.MatchExpr)
	gen_print_reg(r Register, n int, fd int)
	gen_print(s string, fd int)
	gen_selector_expr(expr ast.SelectorExpr)
	gen_syscall(node ast.CallExpr)
	inc_var(var Var, config VarConfig)
	infix_expr(node ast.InfixExpr) // TODO: make platform-independant
	infloop()
	init_struct(var Var, init ast.StructInit)
	init_array(var Var, init ast.ArrayInit)
	jmp_back(start i64)
	jmp(addr int) int
	lea_var_to_reg(r Register, var_offset int)
	learel(reg Register, val int)
	leave()
	load_fp_var(var Var, config VarConfig)
	load_fp(val f64)
	main_reg() Register
	mov_int_to_var(var Var, integer int, config VarConfig)
	mov_reg_to_var(var Var, reg Register, config VarConfig)
	mov_reg(r1 Register, r2 Register)
	mov_var_to_reg(reg Register, var Var, config VarConfig)
	mov(r Register, val int)
	mov64(r Register, val i64)
	movabs(reg Register, val i64)
	prefix_expr(node ast.PrefixExpr)
	push(r Register)
	ret()
	return_stmt(node ast.Return)
	reverse_string(r Register)
	svc()
	syscall() // unix syscalls
	trap()
}

type Register = Amd64Register | Arm64Register

fn (r Register) str() string {
	return match r {
		Amd64Register {
			'${r as Amd64Register}'
		}
		Arm64Register {
			'${r as Arm64Register}'
		}
	}
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

struct Struct {
mut:
	offsets []int
}

struct Enum {
mut:
	fields map[string]int
}

struct MultiReturn {
mut:
	offsets []int
	size    int
	align   int
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

type IdentVar = GlobalVar | LocalVar | Register

enum JumpOp {
	je
	jne
	jg
	jge
	jl
	jle
	js
	jnb
}

union F64I64 {
	f f64
	i i64
}

[inline]
fn byt(n int, s int) u8 {
	return u8((n >> (s * 8)) & 0xff)
}

fn (mut g Gen) get_var_from_ident(ident ast.Ident) IdentVar {
	mut obj := ident.obj
	if obj !in [ast.Var, ast.ConstField, ast.GlobalField, ast.AsmRegister] {
		obj = ident.scope.find(ident.name) or { g.n_error('unknown variable ${ident.name}') }
	}
	match obj {
		ast.Var {
			offset := g.get_var_offset(obj.name)
			return LocalVar{
				offset: offset
				typ: obj.typ
				name: obj.name
			}
		}
		else {
			g.n_error('unsupported variable type type:${obj} name:${ident.name}')
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

fn get_backend(arch pref.Arch) !CodeGen {
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

pub fn gen(files []&ast.File, table &ast.Table, out_name string, pref_ &pref.Preferences) (int, int) {
	exe_name := if pref_.os == .windows && !out_name.ends_with('.exe') {
		out_name + '.exe'
	} else {
		out_name
	}
	mut g := &Gen{
		table: table
		sect_header_name_pos: 0
		out_name: exe_name
		pref: pref_
		files: files
		// TODO: workaround, needs to support recursive init
		code_gen: get_backend(pref_.arch) or {
			eprintln('No available backend for this configuration. Use `-a arm64` or `-a amd64`.')
			exit(1)
		}
		labels: 0
		structs: []Struct{len: table.type_symbols.len}
		eval: eval.new_eval(table, pref_)
	}

	g.code_gen.g = g
	g.generate_header()
	g.init_builtins()
	g.calculate_all_size_align()
	g.calculate_enum_fields()
	for file in g.files {
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

// used in macho_test.v
pub fn macho_test_new_gen(p &pref.Preferences, out_name string) &Gen {
	mut g := Gen{
		pref: p
		out_name: out_name
		table: ast.new_table()
		code_gen: Amd64{
			g: 0
		}
		labels: 0
	}
	g.code_gen.g = &mut g
	return &mut g
}

pub fn (mut g Gen) typ(a int) &ast.TypeSymbol {
	return g.table.type_symbols[a]
}

pub fn (mut g Gen) ast_has_external_functions() bool {
	for file in g.files {
		walker.inspect(file, unsafe { &mut g }, fn (node &ast.Node, data voidptr) bool {
			if node is ast.Expr && (node as ast.Expr) is ast.CallExpr
				&& ((node as ast.Expr) as ast.CallExpr).language != .v {
				call := node as ast.CallExpr
				unsafe {
					mut g := &Gen(data)
					if call.name !in g.extern_symbols {
						g.extern_symbols << call.name
					}
				}
				return true
			}

			return true
		})
	}

	return g.extern_symbols.len != 0
}

pub fn (mut g Gen) generate_header() {
	g.requires_linking = g.ast_has_external_functions()

	match g.pref.os {
		.macos {
			g.generate_macho_header()
		}
		.windows {
			g.generate_pe_header()
		}
		.linux {
			if g.requires_linking {
				g.generate_linkable_elf_header()
			} else {
				g.generate_simple_elf_header()
			}
		}
		.raw {
			if g.pref.arch == .arm64 {
				g.gen_arm64_helloworld()
			}
		}
		else {
			g.n_error('only `raw`, `linux`, `windows` and `macos` are supported for -os in -native')
		}
	}
}

pub fn (mut g Gen) create_executable() {
	obj_name := match g.pref.os {
		.linux {
			if g.requires_linking {
				g.out_name + '.o'
			} else {
				g.out_name
			}
		}
		else {
			g.out_name
		}
	}

	os.write_file_array(obj_name, g.buf) or { panic(err) }

	if g.requires_linking {
		match g.pref.os {
			// TEMPORARY
			.linux { // TEMPORARY
				g.link(obj_name)
			} // TEMPORARY
			else {} // TEMPORARY
		} // TEMPORARY
	}

	os.chmod(g.out_name, 0o775) or { panic(err) } // make it executable
	if g.pref.is_verbose {
		eprintln('\n${g.out_name}: native binary has been successfully generated')
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

pub fn (mut g Gen) link(obj_name string) {
	match g.pref.os {
		.linux {
			g.link_elf_file(obj_name)
		}
		else {
			g.n_error('native linking is not implemented for ${g.pref.os}')
		}
	}
}

pub fn (mut g Gen) calculate_all_size_align() {
	for mut ts in g.table.type_symbols {
		if ts.idx == 0 {
			continue
		}
		ts.size = g.get_type_size(ast.new_type(ts.idx))
		ts.align = g.get_type_align(ast.new_type(ts.idx))
	}
}

pub fn (mut g Gen) calculate_enum_fields() {
	for name, decl in g.table.enum_decls {
		mut enum_vals := Enum{}
		mut value := if decl.is_flag { 1 } else { 0 }
		for field in decl.fields {
			if field.has_expr {
				value = int(g.eval.expr(field.expr, ast.int_type_idx).int_val())
			}
			enum_vals.fields[field.name] = value
			if decl.is_flag {
				value <<= 1
			} else {
				value++
			}
		}
		g.enum_vals[name] = enum_vals
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

fn (g &Gen) abs_to_rel_addr(addr i64) int {
	return int(mu.abs(addr - g.buf.len)) - 1
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
		g.n_error('unknown variable `${var_name}`')
	}
	return r
}

fn (mut g Gen) get_field_offset(in_type ast.Type, name string) int {
	typ := g.unwrap(in_type)
	ts := g.table.sym(typ)
	field := ts.find_field(name) or { g.n_error('Could not find field `${name}` on init') }
	return g.structs[typ.idx()].offsets[field.i]
}

fn (mut g Gen) unwrap(typ ast.Type) ast.Type {
	ts := g.table.sym(typ)
	return if ts.info is ast.Alias { g.unwrap(ts.info.parent_type) } else { typ }
}

// get type size, and calculate size and align and store them to the cache when the type is struct
fn (mut g Gen) get_type_size(raw_type ast.Type) int {
	// TODO type flags
	typ := g.unwrap(raw_type)
	if raw_type.is_any_kind_of_pointer() || typ.is_any_kind_of_pointer() {
		return g.code_gen.address_size()
	}
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
	if typ.is_bool() {
		return 1
	}
	ts := g.table.sym(typ)
	if ts.size != -1 {
		return ts.size
	}
	mut size := 0
	mut align := 1
	mut strc := Struct{}
	match ts.info {
		ast.Struct {
			for f in ts.info.fields {
				f_size := g.get_type_size(f.typ)
				f_align := g.get_type_align(f.typ)
				padding := (f_align - size % f_align) % f_align
				strc.offsets << size + padding
				size += f_size + padding
				if f_align > align {
					align = f_align
				}
			}
			size = (size + align - 1) / align * align
			g.structs[typ.idx()] = strc
		}
		ast.Enum {
			size = 4
			align = 4
		}
		ast.MultiReturn {
			for t in ts.info.types {
				t_size := g.get_type_size(t)
				t_align := g.get_type_align(t)
				padding := (t_align - size % t_align) % t_align
				strc.offsets << size + padding
				size += t_size + padding
				if t_align > align {
					align = t_align
				}
			}
			g.structs[typ.idx()] = strc
		}
		else {}
	}
	mut ts_ := g.table.sym(typ)
	ts_.size = size
	ts_.align = align
	// g.n_error('unknown type size')
	return size
}

fn (mut g Gen) get_type_align(typ ast.Type) int {
	// also calculate align of a struct
	size := g.get_type_size(typ)
	if g.is_register_type(typ) || typ.is_pure_float() {
		return size
	}
	ts := g.table.sym(g.unwrap(typ))
	if ts.align != -1 {
		return ts.align
	}
	// g.n_error('unknown type align')
	return 0
}

fn (mut g Gen) get_multi_return(types []ast.Type) MultiReturn {
	mut size := 0
	mut align := 1
	mut ret := MultiReturn{
		offsets: []int{cap: types.len}
	}
	for t in types {
		t_size := g.get_type_size(t)
		t_align := g.get_type_align(t)
		padding := (t_align - size % t_align) % t_align
		ret.offsets << size + padding
		size += t_size + padding
		if t_align > align {
			align = t_align
		}
	}
	ret.size = size
	ret.align = align
	return ret
}

fn (mut g Gen) is_register_type(typ ast.Type) bool {
	return typ.is_pure_int() || typ == ast.char_type_idx
		|| typ.is_any_kind_of_pointer() || typ.is_bool()
		|| (g.table.sym(typ).info is ast.Alias && g.is_register_type(g.unwrap(typ)))
}

fn (mut g Gen) is_fp_type(typ ast.Type) bool {
	return typ.is_pure_float()
		|| (g.table.sym(typ).info is ast.Alias && g.is_fp_type(g.unwrap(typ)))
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
		g.n_error('unknown variable `${ident}`')
		return 0
	}
	return size
}

fn (mut g Gen) allocate_by_type(name string, typ ast.Type) int {
	size := g.get_type_size(typ)
	align := g.get_type_align(typ)
	padding := (align - g.stack_var_pos % align) % align
	g.stack_var_pos += size + padding
	g.var_offset[name] = g.stack_var_pos
	g.var_alloc_size[name] = size

	return g.stack_var_pos
}

fn (mut g Gen) allocate_string(s string, opsize int, typ RelocType) int {
	str_pos := g.buf.len + opsize
	g.strs << String{s, str_pos, typ}
	return str_pos
}

fn (mut g Gen) allocate_array(name string, size int, items int) int {
	pos := g.code_gen.allocate_var(name, size, items)
	g.stack_var_pos += (size * items)
	return pos
}

fn (mut g Gen) eval_str_lit_escape_codes(str_lit ast.StringLiteral) string {
	if str_lit.is_raw {
		return str_lit.val
	} else {
		return g.eval_escape_codes(str_lit.val)
	}
}

fn (mut g Gen) eval_escape_codes(str string) string {
	mut buffer := []u8{}

	mut i := 0
	for i < str.len {
		if str[i] != `\\` {
			buffer << str[i]
			i++
			continue
		}

		// skip \
		i++
		match str[i] {
			`\\`, `'`, `"`, `\`` {
				buffer << str[i]
				i++
			}
			`a`, `b`, `f` {
				buffer << str[i] - u8(90)
				i++
			}
			`n` {
				buffer << `\n`
				i++
			}
			`r` {
				buffer << `\r`
				i++
			}
			`t` {
				buffer << `\t`
				i++
			}
			`u` {
				i++
				utf8 := strconv.parse_int(str[i..i + 4], 16, 16) or {
					g.n_error('invalid \\u escape code (${str[i..i + 4]})')
					0
				}
				i += 4
				buffer << u8(utf8)
				buffer << u8(utf8 >> 8)
			}
			`v` {
				buffer << `\v`
				i++
			}
			`x` {
				i++
				c := strconv.parse_int(str[i..i + 2], 16, 8) or {
					g.n_error('invalid \\x escape code (${str[i..i + 2]})')
					0
				}
				i += 2
				buffer << u8(c)
			}
			`0`...`7` {
				c := strconv.parse_int(str[i..i + 3], 8, 8) or {
					g.n_error('invalid escape code \\${str[i..i + 3]}')
					0
				}
				i += 3
				buffer << u8(c)
			}
			else {
				g.n_error('invalid escape code \\${str[i]}')
			}
		}
	}

	return buffer.bytestr()
}

fn (mut g Gen) gen_to_string(reg Register, typ ast.Type) {
	if typ.is_int() {
		buffer := g.allocate_array('itoa-buffer', 1, 32) // 32 characters should be enough
		g.code_gen.lea_var_to_reg(g.get_builtin_arg_reg(.int_to_string, 1), buffer)

		arg0_reg := g.get_builtin_arg_reg(.int_to_string, 0)
		if arg0_reg != reg {
			g.code_gen.mov_reg(arg0_reg, reg)
		}

		g.call_builtin(.int_to_string)
		g.code_gen.lea_var_to_reg(g.code_gen.main_reg(), buffer)
	} else if typ.is_bool() {
		arg_reg := g.get_builtin_arg_reg(.bool_to_string, 0)
		if arg_reg != reg {
			g.code_gen.mov_reg(arg_reg, reg)
		}
		g.call_builtin(.bool_to_string)
	} else if typ.is_string() {
		if reg != g.code_gen.main_reg() {
			g.code_gen.mov_reg(g.code_gen.main_reg(), reg)
		}
	} else {
		g.n_error('int-to-string conversion not implemented for type ${typ}')
	}
}

fn (mut g Gen) gen_var_to_string(reg Register, expr ast.Expr, var Var, config VarConfig) {
	typ := g.get_type_from_var(var)
	if typ == ast.rune_type_idx {
		buffer := g.code_gen.allocate_var('rune-buffer', 8, 0)
		g.code_gen.convert_rune_to_string(reg, buffer, var, config)
	} else if typ.is_int() {
		buffer := g.allocate_array('itoa-buffer', 1, 32) // 32 characters should be enough
		g.code_gen.mov_var_to_reg(g.get_builtin_arg_reg(.int_to_string, 0), var, config)
		g.code_gen.lea_var_to_reg(g.get_builtin_arg_reg(.int_to_string, 1), buffer)
		g.call_builtin(.int_to_string)
		g.code_gen.lea_var_to_reg(reg, buffer)
	} else if typ.is_bool() {
		g.code_gen.mov_var_to_reg(g.get_builtin_arg_reg(.bool_to_string, 0), var, config)
		g.call_builtin(.bool_to_string)
	} else if typ.is_string() {
		g.code_gen.mov_var_to_reg(reg, var, config)
	} else {
		g.n_error('int-to-string conversion not implemented for type ${typ}')
	}
}

fn (mut g Gen) is_used_by_main(node ast.FnDecl) bool {
	mut used := true
	if g.pref.skip_unused {
		fkey := node.fkey()
		used = g.table.used_fns[fkey]
	}
	return used
}

fn (mut g Gen) patch_calls() {
	for c in g.callpatches {
		addr := g.fn_addr[c.name]
		if addr == 0 {
			g.n_error('fn addr of `${c.name}` = 0')
			return
		}
		last := g.buf.len
		g.code_gen.call(int(addr + last - c.pos))
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

fn (mut g Gen) fn_decl(node ast.FnDecl) {
	name := if node.is_method {
		'${g.table.get_type_name(node.receiver.typ)}.${node.name}'
	} else {
		node.name
	}
	if node.no_body || !g.is_used_by_main(node) || g.is_blacklisted(name, node.is_builtin) {
		return
	}
	if g.pref.is_verbose {
		println(term.green('\n${name}:'))
	}
	if node.is_deprecated {
		g.warning('fn_decl: ${name} is deprecated', node.pos)
	}

	g.stack_var_pos = 0
	g.stack_depth = 0
	g.register_function_address(name)
	g.labels = &LabelTable{}
	g.defer_stmts.clear()
	g.return_type = node.return_type
	g.code_gen.fn_decl(node)
	g.patch_labels()

	if g.stack_depth != 0 {
		g.println('^^^ stack_depth != 0 (${g.stack_depth}) !!!')
	}
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
	final := '${colored}${padding}${comment}'
	println(final)
}

[noreturn]
pub fn (mut g Gen) n_error(s string) {
	util.verror('native error', s)
}

pub fn (mut g Gen) warning(s string, pos token.Pos) {
	if g.pref.output_mode == .stdout {
		util.show_compiler_message('warning:', pos: pos, file_path: g.pref.path, message: s)
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
		util.show_compiler_message(kind, pos: pos, file_path: g.pref.path, message: s)
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
