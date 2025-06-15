// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module native

import os
import strings
import v.ast
import v.ast.walker
import v.util
import v.token
import v.errors
import v.pref
import v.eval
import term
import strconv

const c_preprocessed = {
	'C.EOF': -1
}

@[heap; minify]
pub struct Gen {
	out_name string
	pref     &pref.Preferences = unsafe { nil } // Preferences shared from V struct
	files    []&ast.File
mut:
	code_gen                  CodeGen
	table                     &ast.Table = unsafe { nil }
	buf                       []u8
	sect_header_name_pos      i32
	offset                    i64
	file_size_pos             i64
	main_fn_addr              i64
	main_fn_size              i64
	start_symbol_addr         i64
	code_start_pos            i64 // location of the start of the assembly instructions
	symbol_table              []SymbolTableSection
	extern_symbols            []string
	linker_include_paths      []string
	linker_libs               []string
	extern_vars               map[i64]string
	extern_fn_calls           map[i64]string
	fn_addr                   map[string]i64
	fn_names                  []string
	var_offset                map[string]i32 // local var stack offset
	var_alloc_size            map[string]i32 // local var allocation size
	stack_var_pos             i32
	stack_depth               i32
	debug_pos                 i32
	current_file              &ast.File = unsafe { nil }
	errors                    []errors.Error
	warnings                  []errors.Warning
	syms                      []Symbol
	size_pos                  []i32
	nlines                    i32
	callpatches               []CallPatch
	strs                      []String
	labels                    &LabelTable = unsafe { nil }
	defer_stmts               []ast.DeferStmt
	builtins                  map[Builtin]BuiltinFn
	structs                   []Struct
	eval                      eval.Eval
	enum_vals                 map[string]Enum
	return_type               ast.Type
	comptime_omitted_branches []ast.IfBranch
	// elf specific
	elf_text_header_addr i64 = -1
	elf_rela_section     Section
	// macho specific
	macho_ncmds   i32
	macho_cmdsize i32
	// pe specific
	pe_coff_hdr_pos    i64
	pe_opt_hdr_pos     i64
	pe_text_size_pos   i64
	pe_data_dirs       PeDataDirs = get_pe_data_dirs()
	pe_sections        []PeSection
	pe_dll_relocations map[string]i64

	requires_linking bool
	is_builtin_mod   bool // true for .v files inside `builtin`
}

interface CodeGen {
mut:
	g &Gen
	add(r Register, val i32)
	add_reg2(r Register, r2 Register)
	address_size() i32
	adr(r Arm64Register, delta i32) // Note: Temporary!
	allocate_var(name string, size i32, initial_val Number) i32
	assign_stmt(node ast.AssignStmt) // TODO: make platform-independent
	builtin_decl(builtin BuiltinFn)
	call_addr_at(addr i32, at i64) i64
	call_builtin(name Builtin) i64
	call_fn(node ast.CallExpr)
	call(addr i32) i64
	cjmp(op JumpOp) i32
	cmp_to_stack_top(r Register)
	cmp_var_reg(var Var, reg Register, config VarConfig)
	cmp_var(var Var, val i32, config VarConfig)
	cmp_reg2(reg Register, reg2 Register)
	cmp_zero(reg Register)
	convert_bool_to_string(r Register)
	convert_int_to_string(a Register, b Register)
	convert_rune_to_string(r Register, buffer i32, var Var, config VarConfig)
	create_string_struct(typ ast.Type, name string, str string) i32
	dec_var(var Var, config VarConfig)
	fn_decl(node ast.FnDecl)
	gen_asm_stmt(asm_node ast.AsmStmt)
	gen_cast_expr(expr ast.CastExpr)
	gen_exit(expr ast.Expr)
	gen_match_expr(expr ast.MatchExpr)
	gen_print_reg(r Register, n i32, fd i32)
	gen_print(s string, fd i32)
	gen_syscall(node ast.CallExpr)
	inc_var(var Var, config VarConfig)
	infix_expr(node ast.InfixExpr) // TODO: make platform-independent
	infloop()
	init_struct(var Var, init ast.StructInit)
	init_array(var Var, init ast.ArrayInit)
	jmp_back(start i64)
	jmp(addr i32) i32
	lea_var_to_reg(r Register, var_offset i32)
	learel(reg Register, val i32)
	leave()
	load_fp_var(var Var, config VarConfig)
	load_fp(val f64)
	main_reg() Register
	mov_deref(reg Register, regptr Register, typ ast.Type)
	mov_int_to_var(var Var, integer i32, config VarConfig)
	mov_reg_to_var(var Var, reg Register, config VarConfig)
	mov_reg(r1 Register, r2 Register)
	mov_var_to_reg(reg Register, var Var, config VarConfig)
	mov(r Register, val i32)
	mov64(r Register, val Number)
	movabs(reg Register, val i64)
	patch_relative_jmp(pos i32, addr i64)
	pop2(r Register)
	prefix_expr(node ast.PrefixExpr)
	push(r Register)
	ret()
	return_stmt(node ast.Return)
	reverse_string(r Register)
	svc()
	syscall() // unix syscalls
	trap()
	zero_fill(size i32, var LocalVar)
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
	pos i32
	typ RelocType
}

struct CallPatch {
	name string
	pos  i32
}

struct LabelTable {
mut:
	label_id i32
	addrs    []i64 = [i64(0)] // register address of label here
	patches  []LabelPatch // push placeholders
	branches []BranchLabel
}

struct LabelPatch {
	id  i32 // id of the needed label address
	pos i32 // where (in the generated code) to fix the address (of a jump for example)
}

struct BranchLabel {
	name  string
	start i32
	end   i32
}

fn (mut l LabelTable) new_label() i32 {
	l.label_id++
	l.addrs << 0
	return l.label_id
}

struct Struct {
mut:
	offsets []i32
}

type Number = u64 | i64

struct Enum {
	size i32 // size of the type of the enum in bytes
mut:
	fields map[string]EnumVal
}

type EnumVal = Number | ast.Expr

struct MultiReturn {
mut:
	offsets []i32
	size    i32
	align   i32
}

enum Size {
	_8
	_16
	_32
	_64
}

// you can use these structs manually if you don't have ast.Ident
struct LocalVar {
	offset i32 // offset from the base pointer
	typ    ast.Type
	name   string
}

struct ExternVar {
	typ  ast.Type
	name string
}

struct PreprocVar {
	typ  ast.Type
	name string
	val  i64
}

struct GlobalVar {}

@[params]
struct VarConfig {
pub:
	offset i32      // offset from the variable
	typ    ast.Type // type of the value you want to process e.g. struct fields.
}

type Var = GlobalVar | ExternVar | LocalVar | PreprocVar | ast.Ident

type IdentVar = GlobalVar | ExternVar | LocalVar | Register | PreprocVar

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

@[inline]
fn byt(n i32, s i32) u8 {
	return u8((n >> (s * 8)) & 0xff)
}

fn (mut g Gen) get_var_from_ident(ident ast.Ident) IdentVar {
	if ident.name in g.extern_symbols {
		return ExternVar{ident.info.typ, ident.name}
	}
	mut is_preprocessed := true
	mut preprocessed_val := c_preprocessed[ident.name] or {
		is_preprocessed = false
		0
	}
	if is_preprocessed {
		return PreprocVar{ident.info.typ, ident.name, preprocessed_val}
	}
	mut obj := ident.obj
	if obj !in [ast.Var, ast.ConstField, ast.GlobalField, ast.AsmRegister] {
		obj = ident.scope.find(ident.name) or {
			g.n_error('${@LOCATION} unknown variable ${ident.name}')
		}
	}
	match obj {
		ast.Var {
			offset := g.get_var_offset(obj.name)
			return LocalVar{
				offset: offset
				typ:    obj.typ
				name:   obj.name
			}
		}
		else {
			g.n_error('${@LOCATION} unsupported variable type type:${obj} name:${ident.name}')
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
			g.n_error('${@LOCATION} cannot get type from GlobalVar yet')
		}
		else {
			g.n_error('${@LOCATION} unsupported var type ${var}')
		}
	}
}

fn get_backend(arch pref.Arch, target_os pref.OS) !CodeGen {
	match arch {
		.arm64 {
			return Arm64{}
		}
		.amd64 {
			return Amd64{
				fn_arg_registers:     amd64_get_call_regs(target_os)
				fn_arg_sse_registers: amd64_get_call_sseregs(target_os)
			}
		}
		._auto {
			$if amd64 {
				return Amd64{}
			} $else $if arm64 {
				return Arm64{}
			} $else {
				eprintln('-native only have amd64 and arm64 codegens')
				exit(1)
			}
		}
		else {}
	}
	return error('unsupported architecture')
}

pub fn gen(files []&ast.File, mut table ast.Table, out_name string, pref_ &pref.Preferences) (int, int) {
	exe_name := if pref_.os == .windows && !out_name.ends_with('.exe') {
		out_name + '.exe'
	} else {
		out_name
	}
	mut g := &Gen{
		table:                table
		sect_header_name_pos: 0
		out_name:             exe_name
		pref:                 pref_
		files:                files
		// TODO: workaround, needs to support recursive init
		code_gen: get_backend(pref_.arch, pref_.os) or {
			eprintln('No available backend for this configuration. Use `-a arm64` or `-a amd64`.')
			exit(1)
		}
		structs:  []Struct{len: table.type_symbols.len}
		eval:     eval.new_eval(table, pref_)
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
		g.current_file = file
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
		pref:     p
		out_name: out_name
		table:    ast.new_table()
		code_gen: Amd64{}
	}
	g.code_gen.g = &mut g
	return &mut g
}

pub fn (mut g Gen) styp(a ast.Type) &ast.TypeSymbol {
	return g.table.type_symbols[a]
}

fn node_fetch_external_deps(node &ast.Node, data voidptr) bool {
	mut g := unsafe { &Gen(data) }

	if node is ast.Expr {
		if node is ast.IfExpr && (node as ast.IfExpr).is_comptime {
			eval_branch := g.comptime_conditional(node) or {
				g.comptime_omitted_branches << node.branches
				return false
			}

			g.comptime_omitted_branches << node.branches.filter(it != eval_branch)
		} else if node is ast.CallExpr && (node as ast.CallExpr).language != .v {
			call := node as ast.CallExpr
			if call.name !in g.extern_symbols {
				g.extern_symbols << call.name
			}
		} else if node is ast.Ident && (node as ast.Ident).language != .v {
			ident := node as ast.Ident
			if ident.name !in g.extern_symbols {
				g.extern_symbols << ident.name
			}
		}
	} else if node is ast.Stmt && (node as ast.Stmt) is ast.HashStmt {
		hash_stmt := node as ast.HashStmt
		if hash_stmt.kind == 'flag' && g.should_emit_hash_stmt(hash_stmt) {
			g.gen_flag_hash_stmt(hash_stmt)
		}
	} else if node is ast.IfBranch {
		return node !in g.comptime_omitted_branches
	}

	return true
}

pub fn (mut g Gen) has_external_deps() bool {
	return g.extern_symbols.len != 0
}

pub fn (mut g Gen) ast_fetch_external_deps() {
	for file in g.files {
		g.current_file = file
		walker.inspect(file, unsafe { &mut g }, node_fetch_external_deps)
	}

	g.requires_linking = g.has_external_deps()
}

pub fn (mut g Gen) generate_header() {
	g.ast_fetch_external_deps()

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
			g.n_error('${@LOCATION} only `raw`, `linux`, `windows` and `macos` are supported for -os in -native')
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
		g.link(obj_name)
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
		.windows {
			// windows linking is already done before codegen
		}
		.macos {
			// TODO: implement linking for macos!
		}
		else {
			g.n_error('${@LOCATION} native linking is not implemented for ${g.pref.os}')
		}
	}
}

pub fn (mut g Gen) calculate_all_size_align() {
	for mut ts in g.table.type_symbols {
		if ts.idx == 0 {
			continue
		}
		ts.size = int(g.get_type_size(ast.new_type(ts.idx)))
		ts.align = int(g.get_type_align(ast.new_type(ts.idx)))
	}
}

pub fn (mut g Gen) calculate_enum_fields() {
	for name, decl in g.table.enum_decls {
		enum_size := g.get_type_size(decl.typ)
		mut enum_vals := Enum{
			size: i32(enum_size)
		}
		mut value := Number(if decl.is_flag { i64(1) } else { i64(0) })
		mut has_expr := false
		for field in decl.fields {
			if field.has_expr {
				if field.expr.is_literal() { // does not depend on other declarations (C compile time csts)
					str_val := g.eval.expr(field.expr, ast.int_type_idx).string()
					if str_val.len >= 0 && str_val[0] == `-` {
						value = str_val.i64()
					} else {
						value = str_val.u64()
					}
				} else {
					enum_vals.fields[field.name] = field.expr
					has_expr = true
					continue
				}
			} else {
				if has_expr {
					g.n_error('${@LOCATION} unsupported auto incr after C consts')
				}
			}
			match value {
				// Dereferences the sumtype (it would get assigned by address and messed up)
				i64 {
					enum_vals.fields[field.name] = Number(value as i64)
				}
				u64 {
					enum_vals.fields[field.name] = Number(value as u64)
				}
			}
			if decl.is_flag {
				match mut value {
					i64 {
						value = value * 2 // same as << 1 but without notice
					}
					u64 {
						value = value << u64(1)
					}
				}
			} else {
				match mut value {
					i64 {
						value++
					}
					u64 {
						value++
					}
				}
			}
		}
		g.enum_vals[name] = enum_vals
	}
}

pub fn (g &Gen) pos() i64 {
	return g.buf.len
}

fn (mut g Gen) write(bytes []u8) {
	g.buf << bytes
}

fn (mut g Gen) write8(n i32) {
	// write 1 byte
	g.buf << u8(n)
}

fn (mut g Gen) write16(n i32) {
	// write 2 bytes
	g.buf << u8(n)
	g.buf << u8(n >> 8)
}

fn (mut g Gen) read32_at(at i32) i32 {
	return i32(u32(g.buf[at]) | (u32(g.buf[at + 1]) << 8) | (u32(g.buf[at + 2]) << 16) | (u32(g.buf[
		at + 3]) << 24))
}

fn (mut g Gen) write32(n i32) {
	// write 4 bytes
	g.buf << u8(n)
	g.buf << u8(n >> 8)
	g.buf << u8(n >> 16)
	g.buf << u8(n >> 24)
}

fn (mut g Gen) write64(n Number) {
	// write 8 bytes
	match n {
		i64 {
			g.buf << u8(n)
			g.buf << u8(n >> 8)
			g.buf << u8(n >> 16)
			g.buf << u8(n >> 24)
			g.buf << u8(n >> 32)
			g.buf << u8(n >> 40)
			g.buf << u8(n >> 48)
			g.buf << u8(n >> 56)
		}
		u64 {
			g.buf << u8(n)
			g.buf << u8(n >> 8)
			g.buf << u8(n >> 16)
			g.buf << u8(n >> 24)
			g.buf << u8(n >> 32)
			g.buf << u8(n >> 40)
			g.buf << u8(n >> 48)
			g.buf << u8(n >> 56)
		}
	}
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

fn (mut g Gen) write32_at(at i64, n i32) {
	// write 4 bytes
	g.buf[at] = u8(n)
	g.buf[at + 1] = u8(n >> 8)
	g.buf[at + 2] = u8(n >> 16)
	g.buf[at + 3] = u8(n >> 24)
}

fn (mut g Gen) write16_at(at i64, n i32) {
	// write 2 bytes
	g.buf[at] = u8(n)
	g.buf[at + 1] = u8(n >> 8)
}

fn (mut g Gen) read64_at(at i64) i64 {
	return i64(u64(g.buf[at]) | u64(g.buf[at + 1]) << 8 | u64(g.buf[at + 2]) << 16 | u64(g.buf[at +
		3]) << 24 | u64(g.buf[at + 4]) << 32 | u64(g.buf[at + 5]) << 40 | u64(g.buf[at + 6]) << 48 | u64(g.buf[
		at + 7]) << 56)
}

pub fn (mut g Gen) zeroes(n i32) {
	for _ in 0 .. n {
		g.buf << 0
	}
}

fn (mut g Gen) write_string(s string) {
	for c in s {
		g.write8(i32(c))
	}
	g.zeroes(1)
}

fn (mut g Gen) write_string_with_padding(s string, max i32) {
	for c in s {
		g.write8(i32(c))
	}
	for _ in 0 .. int(max) - s.len {
		g.write8(0)
	}
}

fn (mut g Gen) pad_to(len i32) {
	for g.buf.len < len {
		g.buf << u8(0)
	}
}

fn (mut g Gen) align_to(align i32) {
	padded := (i32(g.buf.len) + align - 1) & ~(align - 1)
	for g.buf.len < padded {
		g.buf << u8(0)
	}
}

fn (g &Gen) abs_to_rel_addr(addr i64) i32 {
	return i32(abs(addr - g.buf.len)) - 1
}

fn abs(a i64) i64 {
	return if a > 0 { a } else { -a }
}

fn (mut g Gen) try_var_offset(var_name string) i32 {
	offset := g.var_offset[var_name] or { return -1 }
	if offset == 0 {
		return -1
	}
	return offset
}

fn (mut g Gen) get_var_offset(var_name string) i32 {
	r := g.try_var_offset(var_name)
	if r == -1 {
		g.n_error('${@LOCATION} unknown variable `${var_name}`')
	}
	return r
}

fn (mut g Gen) get_field_offset(in_type ast.Type, name string) i32 {
	typ := g.unwrap(in_type)
	ts := g.table.sym(typ)
	field := ts.find_field(name) or {
		g.n_error('${@LOCATION} Could not find field `${name}` on init')
	}
	return g.structs[typ.idx()].offsets[field.i]
}

fn (mut g Gen) unwrap(typ ast.Type) ast.Type {
	ts := g.table.sym(typ)
	return if ts.info is ast.Alias { g.unwrap(ts.info.parent_type) } else { typ }
}

// get type size, and calculate size and align and store them to the cache when the type is struct
fn (mut g Gen) get_type_size(raw_type ast.Type) i32 {
	// TODO: type flags
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
			ast.int_type_idx { 4 } // TODO: change when V will have changed
			ast.i32_type_idx { 4 }
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
			else { g.n_error('${@LOCATION} unknown type size ${typ}') }
		}
	}
	if typ.is_bool() {
		return 1
	}
	ts := g.table.sym(typ)
	if ts.size != -1 {
		return i32(ts.size)
	}
	mut size := i32(0)
	mut align := i32(1)
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
			size = g.get_type_size(ts.info.typ)
			align = g.get_type_align(ts.info.typ)
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
	ts_.size = int(size)
	ts_.align = int(align)
	// g.n_error('unknown type size')
	return size
}

fn (mut g Gen) get_type_align(typ ast.Type) i32 {
	// also calculate align of a struct
	size := g.get_type_size(typ)
	if g.is_register_type(typ) || typ.is_pure_float() {
		return size
	}
	ts := g.table.sym(g.unwrap(typ))
	if ts.align != -1 {
		return i32(ts.align)
	}
	// g.n_error('unknown type align')
	return 0
}

fn (mut g Gen) get_multi_return(types []ast.Type) MultiReturn {
	mut size := i32(0)
	mut align := i32(1)
	mut ret := MultiReturn{
		offsets: []i32{cap: types.len}
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

fn (mut g Gen) get_sizeof_ident(ident ast.Ident) i32 {
	typ := match ident.obj {
		ast.EmptyScopeObject { ident.obj.typ }
		ast.AsmRegister { ast.i64_type }
		ast.ConstField { ident.obj.typ }
		ast.GlobalField { ident.obj.typ }
		ast.Var { ident.obj.typ }
	}
	if typ != 0 {
		return g.get_type_size(typ)
	}
	size := g.var_alloc_size[ident.name] or {
		g.n_error('${@LOCATION} unknown variable `${ident}`')
		return 0
	}
	return size
}

fn (mut g Gen) allocate_by_type(name string, typ ast.Type) i32 {
	size := g.get_type_size(typ)
	align := g.get_type_align(typ)
	padding := (align - g.stack_var_pos % align) % align
	g.stack_var_pos += size + padding
	g.var_offset[name] = g.stack_var_pos
	g.var_alloc_size[name] = size

	return g.stack_var_pos
}

fn (mut g Gen) allocate_string(s string, opsize i32, typ RelocType) i32 {
	str_pos := i32(g.buf.len) + opsize
	g.strs << String{s, str_pos, typ}
	return str_pos
}

// allocates a buffer variable: name, size of stored type (nb of bytes), nb of items
fn (mut g Gen) allocate_array(name string, size i32, items i32) i32 {
	g.println('; allocate array `${name}` item-size:${size} items:${items}:')
	pos := g.code_gen.allocate_var(name, 4, i64(items)) // store the length of the array on the stack in a 4 byte var
	g.stack_var_pos += (size * items) // reserve space on the stack for the items
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
					g.n_error('${@LOCATION} invalid \\u escape code (${str[i..i + 4]})')
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
					g.n_error('${@LOCATION} invalid \\x escape code (${str[i..i + 2]})')
					0
				}
				i += 2
				buffer << u8(c)
			}
			`0`...`7` {
				c := strconv.parse_int(str[i..i + 3], 8, 8) or {
					g.n_error('${@LOCATION} invalid escape code \\${str[i..i + 3]}')
					0
				}
				i += 3
				buffer << u8(c)
			}
			else {
				g.n_error('${@LOCATION} invalid escape code \\${str[i]}')
			}
		}
	}

	return buffer.bytestr()
}

fn (mut g Gen) gen_to_string(reg Register, typ ast.Type) {
	g.println('; to_string (reg:${reg}) {')
	if typ.is_int() {
		buffer := g.allocate_array('itoa-buffer', 1, 32) // 32 characters should be enough
		end_of_buffer := buffer + 4 + 32 - 1 // 4 bytes for the size and 32 for the chars, -1 to not go out of array
		g.code_gen.lea_var_to_reg(g.get_builtin_arg_reg(.int_to_string, 1), end_of_buffer)

		arg0_reg := g.get_builtin_arg_reg(.int_to_string, 0)
		if arg0_reg != reg {
			g.code_gen.mov_reg(arg0_reg, reg)
		}

		g.call_builtin(.int_to_string)
		g.code_gen.lea_var_to_reg(g.code_gen.main_reg(), end_of_buffer) // the (int) string starts at the end of the buffer
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
		g.n_error('${@LOCATION} int-to-string conversion not implemented for type ${typ}')
	}
	g.println('; to_string }')
}

fn (mut g Gen) gen_var_to_string(reg Register, expr ast.Expr, var Var, config VarConfig) {
	g.println('; var_to_string {')
	typ := g.get_type_from_var(var)
	if typ == ast.rune_type_idx {
		buffer := g.code_gen.allocate_var('rune-buffer', 8, i64(0))
		g.code_gen.convert_rune_to_string(reg, buffer, var, config)
	} else if typ.is_int() {
		if typ.is_unsigned() {
			g.n_error('${@LOCATION} Unsigned integer print is not supported')
		} else {
			buffer := g.allocate_array('itoa-buffer', 1, 32) // 32 characters should be enough
			end_of_buffer := buffer + 4 + 32 - 1 // 4 bytes for the size and 32 for the chars, -1 to not go out of array
			g.code_gen.mov_var_to_reg(g.get_builtin_arg_reg(.int_to_string, 0), var, config)
			g.code_gen.lea_var_to_reg(g.get_builtin_arg_reg(.int_to_string, 1), end_of_buffer)
			g.call_builtin(.int_to_string)
			g.code_gen.lea_var_to_reg(reg, end_of_buffer) // the (int) string starts at the end of the buffer
		}
	} else if typ.is_bool() {
		g.code_gen.mov_var_to_reg(g.get_builtin_arg_reg(.bool_to_string, 0), var, config)
		g.call_builtin(.bool_to_string)
	} else if typ.is_string() {
		g.code_gen.mov_var_to_reg(reg, var, config)
	} else {
		g.n_error('${@LOCATION} int-to-string conversion not implemented for type ${typ}')
	}
	g.println('; var_to_string }')
}

fn (mut g Gen) is_used_by_main(node ast.FnDecl) bool {
	mut used := true
	if g.pref.skip_unused {
		fkey := node.fkey()
		used = g.table.used_features.used_fns[fkey]
	}
	return used
}

fn (mut g Gen) patch_calls() {
	for c in g.callpatches {
		addr := g.fn_addr[c.name]
		if addr == 0 {
			g.n_error('${@LOCATION} fn addr of `${c.name}` = 0')
			return
		}
		last := i32(g.buf.len)
		g.code_gen.call(i32(i32(addr) + last - c.pos))
		mut patch := []u8{}
		for last < g.buf.len {
			patch << g.buf.pop()
		}
		for i := 0; i < patch.len; i++ {
			g.buf[int(c.pos) + i] = patch[patch.len - i - 1]
		}
	}
}

fn (mut g Gen) patch_labels() {
	for label in g.labels.patches {
		addr := g.labels.addrs[label.id]
		if addr == 0 {
			g.n_error('${@LOCATION} label addr = 0')
			return
		}

		g.code_gen.patch_relative_jmp(label.pos, addr)
	}
}

fn (mut g Gen) delay_fn_call(name string) {
	pos := i32(g.buf.len)
	g.callpatches << CallPatch{name, pos}
	// do nothing for now
}

fn (mut g Gen) fn_decl(node ast.FnDecl) {
	if node.attrs.contains('flag_enum_fn') {
		// TODO: remove, when the native backend can process all flagged enum generated functions
		return
	}

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
	g.fn_names << name
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
	addr := int(g.debug_pos).hex()
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
	g.debug_pos = i32(g.buf.len)

	colored := sb.str()
	plain := term.strip_ansi(colored)
	padding := ' '.repeat(int_max(1, 40 - plain.len))
	final := '${colored}${padding}${comment}'
	println(final)
}

@[noreturn]
pub fn (mut g Gen) n_error(s string) {
	print_backtrace()
	util.verror('native error', s)
}

pub fn (mut g Gen) warning(s string, pos token.Pos) {
	if g.pref.skip_warnings {
		return
	}

	if g.pref.output_mode == .stdout {
		util.show_compiler_message('warning:', pos: pos, file_path: g.current_file.path, message: s)
	} else {
		g.warnings << errors.Warning{
			file_path: g.current_file.path
			pos:       pos
			reporter:  .gen
			message:   s
		}
	}
}

pub fn (mut g Gen) v_error(s string, pos token.Pos) {
	// TODO: store a file index in the Position too,
	// so that the file path can be retrieved from the pos, instead
	// of guessed from the pref.path ...
	mut kind := 'error:'
	if g.pref.output_mode == .stdout {
		util.show_compiler_message(kind, pos: pos, file_path: g.current_file.path, message: s)
		exit(1)
	} else {
		g.errors << errors.Error{
			file_path: g.current_file.path
			pos:       pos
			reporter:  .gen
			message:   s
		}
	}
}

fn (mut g Gen) gen_concat_expr(node ast.ConcatExpr) {
	typ := node.return_type
	ts := g.table.sym(typ)
	size := g.get_type_size(typ)
	// construct a struct variable contains the return value
	var := LocalVar{
		offset: g.allocate_by_type('', typ)
		typ:    typ
	}

	g.code_gen.zero_fill(size, var)
	main_reg := g.code_gen.main_reg()
	// store exprs to the variable
	for i, expr in node.vals {
		offset := g.structs[typ.idx()].offsets[i]
		g.expr(expr)
		// TODO: expr not on rax
		g.code_gen.mov_reg_to_var(var, main_reg,
			offset: offset
			typ:    ts.mr_info().types[i]
		)
	}
	// store the multi return struct value
	g.code_gen.lea_var_to_reg(main_reg, var.offset)
}

fn (mut g Gen) sym_string_table() i32 {
	begin := i32(g.buf.len)
	g.zeroes(1)
	g.println('')
	g.println('=== strings ===')

	mut generated := map[string]i32{}

	for _, s in g.strs {
		pos := generated[s.str] or { i32(g.buf.len) }

		match s.typ {
			.rel32 {
				g.write32_at(i64(s.pos), pos - s.pos - 4)
			}
			else {
				if g.pref.os == .windows {
					// that should be .rel32, not windows-specific
					g.write32_at(i64(s.pos), pos - s.pos - 4)
				} else {
					g.write64_at(i64(s.pos), i64(pos) + base_addr)
				}
			}
		}

		if s.str !in generated {
			generated[s.str] = pos
			g.write_string(s.str)
			if g.pref.is_verbose {
				g.println('"${escape_string(s.str)}"')
			}
		}
	}
	return i32(g.buf.len) - begin
}

const escape_char = u8(`\\`)

const escape_codes = {
	u8(`\a`):    u8(`a`)
	u8(`\b`):    u8(`b`)
	u8(`\f`):    u8(`f`)
	u8(`\n`):    u8(`n`)
	u8(`\r`):    u8(`r`)
	u8(`\t`):    u8(`t`)
	u8(`\v`):    u8(`v`)
	escape_char: escape_char
	u8(`"`):     u8(`"`)
}

pub fn escape_string(s string) string {
	mut out := []u8{cap: s.len}

	for c in s {
		if c in escape_codes {
			out << escape_char
			out << escape_codes[c]
		} else {
			out << c
		}
	}
	return out.bytestr()
}
