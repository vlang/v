// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module ssa

import v2.ast
import v2.types

pub struct Builder {
mut:
	mod        &Module
	env        &types.Environment = unsafe { nil }
	cur_func   int                = -1
	cur_block  BlockID            = -1
	cur_module string             = 'main'
	// Variable name -> SSA ValueID (alloca pointer)
	vars map[string]ValueID
	// Loop break/continue targets
	loop_stack []LoopInfo
	// Struct name -> SSA TypeID
	struct_types map[string]TypeID
	// Enum name -> field values
	enum_values map[string]int
	// Function name -> SSA function index
	fn_index map[string]int
}

struct LoopInfo {
	cond_block BlockID
	exit_block BlockID
}

pub fn Builder.new(mod &Module) &Builder {
	return Builder.new_with_env(mod, unsafe { nil })
}

pub fn Builder.new_with_env(mod &Module, env &types.Environment) &Builder {
	mut b := &Builder{
		mod:          mod
		vars:         map[string]ValueID{}
		loop_stack:   []LoopInfo{}
		struct_types: map[string]TypeID{}
		enum_values:  map[string]int{}
		fn_index:     map[string]int{}
	}
	unsafe {
		b.env = env
		mod.env = env
	}
	return b
}

pub fn (mut b Builder) build_all(files []ast.File) {
	// Register builtin globals needed by all backends
	i32_t := b.mod.type_store.get_int(32)
	i8_t := b.mod.type_store.get_int(8)
	ptr_t := b.mod.type_store.get_ptr(i8_t)
	ptr_ptr_t := b.mod.type_store.get_ptr(ptr_t)
	b.mod.add_global('g_main_argc', i32_t, false)
	b.mod.add_global('g_main_argv', ptr_ptr_t, false)

	// Phase 1a: Register core builtin types first (string, array) since other structs depend on them
	for file in files {
		b.cur_module = file_module_name(file)
		if b.cur_module == 'builtin' {
			for stmt in file.stmts {
				if stmt is ast.StructDecl {
					if stmt.name in ['string', 'array'] {
						b.register_struct(stmt)
					}
				}
			}
		}
	}
	// Phase 1b: Register all struct types and enums
	for file in files {
		b.cur_module = file_module_name(file)
		b.register_types(file)
	}
	// Phase 2: Register consts and globals
	for file in files {
		b.cur_module = file_module_name(file)
		b.register_consts_and_globals(file)
	}
	// Phase 3: Register function signatures
	for file in files {
		b.cur_module = file_module_name(file)
		b.register_fn_signatures(file)
	}
	// Phase 4: Build function bodies
	for file in files {
		b.cur_module = file_module_name(file)
		b.build_fn_bodies(file)
	}
}

fn file_module_name(file ast.File) string {
	for stmt in file.stmts {
		if stmt is ast.ModuleStmt {
			return stmt.name.replace('.', '_')
		}
	}
	return 'main'
}

// --- Type resolution using types.Environment ---

fn (mut b Builder) type_to_ssa(t types.Type) TypeID {
	match t {
		types.Primitive {
			if t.props.has(.boolean) {
				return b.mod.type_store.get_int(1)
			}
			if t.props.has(.float) {
				width := if t.size == 32 { 32 } else { 64 }
				return b.mod.type_store.get_float(width)
			}
			if t.props.has(.integer) {
				size := if t.size == 0 { 32 } else { int(t.size) }
				return b.mod.type_store.get_int(size)
			}
			return b.mod.type_store.get_int(32)
		}
		types.Pointer {
			base := b.type_to_ssa(t.base_type)
			return b.mod.type_store.get_ptr(base)
		}
		types.String {
			return b.get_string_type()
		}
		types.Struct {
			if t.name in b.struct_types {
				return b.struct_types[t.name]
			}
			return b.mod.type_store.get_int(64) // fallback
		}
		types.Enum {
			return b.mod.type_store.get_int(32)
		}
		types.Void {
			return 0 // void
		}
		types.Char {
			return b.mod.type_store.get_int(8)
		}
		types.Rune {
			return b.mod.type_store.get_int(32)
		}
		types.ISize {
			return b.mod.type_store.get_int(64)
		}
		types.USize {
			return b.mod.type_store.get_int(64)
		}
		types.Alias {
			return b.type_to_ssa(t.base_type)
		}
		types.Array {
			// Dynamic arrays are struct-like: {data*, len, cap, element_size}
			return b.get_array_type()
		}
		types.Nil {
			i8_t := b.mod.type_store.get_int(8)
			return b.mod.type_store.get_ptr(i8_t)
		}
		types.None {
			return 0
		}
		else {
			return b.mod.type_store.get_int(64) // fallback for unhandled
		}
	}
}

fn (mut b Builder) get_string_type() TypeID {
	return b.struct_types['string'] or { 0 }
}

fn (mut b Builder) get_array_type() TypeID {
	return b.struct_types['array'] or { 0 }
}

fn (mut b Builder) expr_type(e ast.Expr) TypeID {
	if b.env != unsafe { nil } {
		pos := e.pos()
		if pos.id != 0 {
			if typ := b.env.get_expr_type(pos.id) {
				return b.type_to_ssa(typ)
			}
		}
	}
	// Fallback for literals
	match e {
		ast.BasicLiteral {
			if e.kind == .key_true || e.kind == .key_false {
				return b.mod.type_store.get_int(1)
			}
			if e.kind == .number && e.value.contains('.') {
				return b.mod.type_store.get_float(64)
			}
			return b.mod.type_store.get_int(64)
		}
		ast.StringLiteral {
			return b.get_string_type()
		}
		else {
			return b.mod.type_store.get_int(64)
		}
	}
}

fn (mut b Builder) types_type_c_name(t types.Type) string {
	match t {
		types.Primitive {
			if t.props.has(.boolean) {
				return 'bool'
			}
			if t.props.has(.float) {
				return if t.size == 32 { 'f32' } else { 'f64' }
			}
			if t.props.has(.integer) {
				if t.props.has(.untyped) {
					return 'int'
				}
				size := if t.size == 0 { 32 } else { int(t.size) }
				is_signed := !t.props.has(.unsigned)
				return if is_signed {
					match size {
						8 { 'i8' }
						16 { 'i16' }
						64 { 'i64' }
						else { 'int' }
					}
				} else {
					match size {
						8 { 'u8' }
						16 { 'u16' }
						32 { 'u32' }
						else { 'u64' }
					}
				}
			}
			return 'int'
		}
		types.Pointer {
			return b.types_type_c_name(t.base_type) + '*'
		}
		types.String {
			return 'string'
		}
		types.Struct {
			return t.name
		}
		types.Enum {
			return t.name
		}
		types.Void {
			return 'void'
		}
		types.Char {
			return 'char'
		}
		types.Alias {
			return b.types_type_c_name(t.base_type)
		}
		else {
			return 'int'
		}
	}
}

// --- Phase 1: Register types ---

fn (mut b Builder) register_types(file ast.File) {
	for stmt in file.stmts {
		match stmt {
			ast.StructDecl {
				b.register_struct(stmt)
			}
			ast.EnumDecl {
				b.register_enum(stmt)
			}
			else {}
		}
	}
}

fn (mut b Builder) register_struct(decl ast.StructDecl) {
	// For core builtin types (array, string, map, DenseArray), use short names
	// to match the C preamble typedefs
	name := if b.cur_module == 'builtin'
		&& decl.name in ['array', 'string', 'map', 'DenseArray', 'IError', 'Error', 'MessageError', 'None__', '_option', '_result', 'Option'] {
		decl.name
	} else if b.cur_module != '' && b.cur_module != 'main' {
		'${b.cur_module}__${decl.name}'
	} else {
		decl.name
	}

	if name in b.struct_types {
		return
	}

	mut field_types := []TypeID{}
	mut field_names := []string{}

	for field in decl.fields {
		ft := b.ast_type_to_ssa(field.typ)
		field_types << ft
		field_names << field.name
	}

	type_id := b.mod.type_store.register(Type{
		kind:        .struct_t
		fields:      field_types
		field_names: field_names
	})
	b.struct_types[name] = type_id
	b.mod.c_struct_names[type_id] = name
}

fn (mut b Builder) register_enum(decl ast.EnumDecl) {
	name := if b.cur_module != '' && b.cur_module != 'main' {
		'${b.cur_module}__${decl.name}'
	} else {
		decl.name
	}

	for i, field in decl.fields {
		key := '${name}__${field.name}'
		b.enum_values[key] = i
	}
}

fn (mut b Builder) register_consts_and_globals(file ast.File) {
	for stmt in file.stmts {
		match stmt {
			ast.ConstDecl {
				for field in stmt.fields {
					const_name := if b.cur_module != '' && b.cur_module != 'main' {
						'${b.cur_module}__${field.name}'
					} else {
						field.name
					}
					const_type := b.expr_type(field.value)
					b.mod.add_global(const_name, const_type, true)
				}
			}
			ast.GlobalDecl {
				for field in stmt.fields {
					glob_name := if b.cur_module != '' && b.cur_module != 'main' {
						'${b.cur_module}__${field.name}'
					} else {
						field.name
					}
					glob_type := if field.typ != ast.empty_expr {
						b.ast_type_to_ssa(field.typ)
					} else {
						b.mod.type_store.get_int(64)
					}
					b.mod.add_global(glob_name, glob_type, false)
				}
			}
			else {}
		}
	}
}

fn (mut b Builder) ast_type_to_ssa(typ ast.Expr) TypeID {
	match typ {
		ast.Ident {
			return b.ident_type_to_ssa(typ.name)
		}
		ast.Type {
			return b.ast_type_node_to_ssa(typ)
		}
		ast.PrefixExpr {
			if typ.op == .amp {
				base := b.ast_type_to_ssa(typ.expr)
				return b.mod.type_store.get_ptr(base)
			}
			return b.mod.type_store.get_int(64)
		}
		ast.SelectorExpr {
			// module.Type
			return b.ident_type_to_ssa(typ.rhs.name)
		}
		ast.EmptyExpr {
			return 0 // void
		}
		else {
			return b.mod.type_store.get_int(64)
		}
	}
}

fn (mut b Builder) ast_type_node_to_ssa(typ ast.Type) TypeID {
	match typ {
		ast.ArrayType {
			return b.get_array_type()
		}
		ast.MapType {
			return b.mod.type_store.get_int(64) // maps are complex structs
		}
		ast.FnType {
			i8_t := b.mod.type_store.get_int(8)
			return b.mod.type_store.get_ptr(i8_t) // fn pointers
		}
		ast.OptionType {
			return b.mod.type_store.get_int(64) // TODO
		}
		ast.ResultType {
			return b.mod.type_store.get_int(64) // TODO
		}
		else {
			return b.mod.type_store.get_int(64)
		}
	}
}

fn (mut b Builder) ident_type_to_ssa(name string) TypeID {
	return match name {
		'int' {
			b.mod.type_store.get_int(32)
		}
		'i8' {
			b.mod.type_store.get_int(8)
		}
		'i16' {
			b.mod.type_store.get_int(16)
		}
		'i32' {
			b.mod.type_store.get_int(32)
		}
		'i64' {
			b.mod.type_store.get_int(64)
		}
		'u8', 'byte' {
			b.mod.type_store.get_int(8)
		}
		'u16' {
			b.mod.type_store.get_int(16)
		}
		'u32' {
			b.mod.type_store.get_int(32)
		}
		'u64' {
			b.mod.type_store.get_int(64)
		}
		'f32' {
			b.mod.type_store.get_float(32)
		}
		'f64' {
			b.mod.type_store.get_float(64)
		}
		'bool' {
			b.mod.type_store.get_int(1)
		}
		'string' {
			b.get_string_type()
		}
		'voidptr' {
			i8_t := b.mod.type_store.get_int(8)
			b.mod.type_store.get_ptr(i8_t)
		}
		'rune' {
			b.mod.type_store.get_int(32)
		}
		'char' {
			b.mod.type_store.get_int(8)
		}
		else {
			// Check struct types
			if name in b.struct_types {
				b.struct_types[name]
			} else {
				// Try module-qualified
				qualified := '${b.cur_module}__${name}'
				if qualified in b.struct_types {
					b.struct_types[qualified]
				} else {
					b.mod.type_store.get_int(64)
				}
			}
		}
	}
}

// --- Phase 2: Register function signatures ---

fn (mut b Builder) register_fn_signatures(file ast.File) {
	for stmt in file.stmts {
		if stmt is ast.FnDecl {
			if stmt.language == .c && stmt.stmts.len == 0 {
				continue
			}
			if stmt.typ.generic_params.len > 0 {
				continue
			}
			b.register_fn_sig(stmt)
		}
	}
}

fn (mut b Builder) register_fn_sig(decl ast.FnDecl) {
	fn_name := b.mangle_fn_name(decl)
	if fn_name in b.fn_index {
		return
	}

	ret_type := b.ast_type_to_ssa(decl.typ.return_type)
	idx := b.mod.new_function(fn_name, ret_type, []TypeID{})
	b.fn_index[fn_name] = idx

	// Register parameter types for correct forward declarations.
	// For methods, add receiver as the first parameter.
	if decl.is_method {
		recv_type := b.ast_type_to_ssa(decl.receiver.typ)
		is_ptr := b.is_mut_receiver(decl.receiver.typ)
		actual_type := if is_ptr {
			b.mod.type_store.get_ptr(recv_type)
		} else {
			recv_type
		}
		receiver_name := if decl.receiver.name != '' {
			decl.receiver.name
		} else {
			'self'
		}
		param_val := b.mod.add_value_node(.argument, actual_type, receiver_name, 0)
		b.mod.funcs[idx].params << param_val
	}
	for param in decl.typ.params {
		param_type := b.ast_type_to_ssa(param.typ)
		actual_type := if param.is_mut {
			b.mod.type_store.get_ptr(param_type)
		} else {
			param_type
		}
		param_val := b.mod.add_value_node(.argument, actual_type, param.name, 0)
		b.mod.funcs[idx].params << param_val
	}
}

fn (mut b Builder) mangle_fn_name(decl ast.FnDecl) string {
	if decl.is_method {
		receiver_name := b.receiver_type_name(decl.receiver.typ)
		return '${receiver_name}__${decl.name}'
	}
	if decl.name == 'main' && b.cur_module == 'main' {
		return 'main'
	}
	if b.cur_module != '' && b.cur_module != 'main' {
		return '${b.cur_module}__${decl.name}'
	}
	return decl.name
}

fn (mut b Builder) receiver_type_name(typ ast.Expr) string {
	match typ {
		ast.Ident {
			if b.cur_module != '' && b.cur_module != 'main' {
				return '${b.cur_module}__${typ.name}'
			}
			return typ.name
		}
		ast.PrefixExpr {
			return b.receiver_type_name(typ.expr)
		}
		ast.ModifierExpr {
			return b.receiver_type_name(typ.expr)
		}
		ast.SelectorExpr {
			return '${typ.lhs.name()}__${typ.rhs.name}'
		}
		else {
			return 'unknown'
		}
	}
}

// --- Phase 3: Build function bodies ---

fn (mut b Builder) build_fn_bodies(file ast.File) {
	for stmt in file.stmts {
		if stmt is ast.FnDecl {
			if stmt.language == .c && stmt.stmts.len == 0 {
				continue
			}
			if stmt.typ.generic_params.len > 0 {
				continue
			}
			b.build_fn(stmt)
		}
	}
}

fn (mut b Builder) build_fn(decl ast.FnDecl) {
	fn_name := b.mangle_fn_name(decl)
	// Skip C-language extern functions without bodies
	if decl.language == .c {
		return
	}
	func_idx := b.fn_index[fn_name] or { return }
	// Skip if already built (can happen with .c.v and .v files)
	if b.mod.funcs[func_idx].blocks.len > 0 {
		return
	}

	// Skip functions without a body (e.g., extern declarations) or non-main-module functions
	if decl.stmts.len == 0 || b.cur_module != 'main' {
		// Emit a minimal function body (entry + ret) so backends have a valid function
		b.cur_func = func_idx
		entry := b.mod.add_block(func_idx, 'entry')
		b.cur_block = entry
		ret_type := b.mod.funcs[func_idx].typ
		if ret_type != 0 {
			ret_type_info := b.mod.type_store.types[ret_type]
			if ret_type_info.kind == .struct_t {
				// For struct return types, alloca + zero init + load
				ptr_type := b.mod.type_store.get_ptr(ret_type)
				alloca := b.mod.add_instr(.alloca, b.cur_block, ptr_type, []ValueID{})
				ret_val := b.mod.add_instr(.load, b.cur_block, ret_type, [alloca])
				b.mod.add_instr(.ret, b.cur_block, 0, [ret_val])
			} else {
				zero := b.mod.get_or_add_const(ret_type, '0')
				b.mod.add_instr(.ret, b.cur_block, 0, [zero])
			}
		} else {
			b.mod.add_instr(.ret, b.cur_block, 0, []ValueID{})
		}
		return
	}

	b.cur_func = func_idx

	// Reset local variables
	b.vars = map[string]ValueID{}

	// Clear params (they were registered in register_fn_sig for forward decls,
	// but we need to re-create them here with proper alloca bindings)
	b.mod.funcs[func_idx].params = []ValueID{}

	// Create entry block
	entry := b.mod.add_block(func_idx, 'entry')
	b.cur_block = entry

	// Add parameters
	if decl.is_method {
		// Receiver is the first parameter
		receiver_name := if decl.receiver.name != '' {
			decl.receiver.name
		} else {
			'self'
		}
		recv_type := b.ast_type_to_ssa(decl.receiver.typ)
		// If receiver is mut/ref, it's a pointer
		is_ptr := b.is_mut_receiver(decl.receiver.typ)
		actual_type := if is_ptr {
			b.mod.type_store.get_ptr(recv_type)
		} else {
			recv_type
		}
		param_val := b.mod.add_value_node(.argument, actual_type, receiver_name, 0)
		b.mod.funcs[func_idx].params << param_val
		// Alloca + store for receiver
		alloca := b.mod.add_instr(.alloca, entry, b.mod.type_store.get_ptr(actual_type),
			[]ValueID{})
		b.mod.add_instr(.store, entry, 0, [param_val, alloca])
		b.vars[receiver_name] = alloca
	}

	for param in decl.typ.params {
		param_type := b.ast_type_to_ssa(param.typ)
		actual_type := if param.is_mut {
			b.mod.type_store.get_ptr(param_type)
		} else {
			param_type
		}
		param_val := b.mod.add_value_node(.argument, actual_type, param.name, 0)
		b.mod.funcs[func_idx].params << param_val
		// Alloca + store
		alloca := b.mod.add_instr(.alloca, entry, b.mod.type_store.get_ptr(actual_type),
			[]ValueID{})
		b.mod.add_instr(.store, entry, 0, [param_val, alloca])
		b.vars[param.name] = alloca
	}

	// Build body
	b.build_stmts(decl.stmts)

	// If no terminator, add implicit return
	if !b.block_has_terminator(b.cur_block) {
		if fn_name == 'main' {
			zero := b.mod.get_or_add_const(b.mod.type_store.get_int(32), '0')
			b.mod.add_instr(.ret, b.cur_block, 0, [zero])
		} else {
			b.mod.add_instr(.ret, b.cur_block, 0, []ValueID{})
		}
	}
}

fn (mut b Builder) is_mut_receiver(typ ast.Expr) bool {
	if typ is ast.ModifierExpr {
		return typ.kind == .key_mut || typ.kind == .key_shared
	}
	if typ is ast.PrefixExpr {
		return typ.op == .amp
	}
	return false
}

fn (mut b Builder) block_has_terminator(block BlockID) bool {
	instrs := b.mod.blocks[block].instrs
	if instrs.len == 0 {
		return false
	}
	last := instrs[instrs.len - 1]
	last_instr := b.mod.instrs[b.mod.values[last].index]
	return last_instr.op in [.ret, .br, .jmp, .unreachable]
}

// --- Statement building ---

fn (mut b Builder) build_stmts(stmts []ast.Stmt) {
	for stmt in stmts {
		b.build_stmt(stmt)
	}
}

fn (mut b Builder) build_stmt(stmt ast.Stmt) {
	match stmt {
		ast.AssignStmt {
			b.build_assign(stmt)
		}
		ast.ExprStmt {
			b.build_expr_stmt(stmt)
		}
		ast.ReturnStmt {
			b.build_return(stmt)
		}
		ast.ForStmt {
			b.build_for(stmt)
		}
		ast.FlowControlStmt {
			b.build_flow_control(stmt)
		}
		ast.BlockStmt {
			b.build_stmts(stmt.stmts)
		}
		ast.LabelStmt {}
		ast.ModuleStmt {
			b.cur_module = stmt.name.replace('.', '_')
		}
		ast.ImportStmt {}
		ast.ConstDecl {}
		ast.StructDecl {}
		ast.EnumDecl {}
		ast.TypeDecl {}
		ast.InterfaceDecl {}
		ast.GlobalDecl {}
		ast.FnDecl {} // nested fn decls handled separately
		ast.Directive {}
		ast.ComptimeStmt {}
		ast.DeferStmt {}
		ast.AssertStmt {
			b.build_assert(stmt)
		}
		[]ast.Attribute {}
		ast.EmptyStmt {}
		ast.AsmStmt {}
		ast.ForInStmt {
			panic('SSA builder: ForInStmt should have been lowered by transformer')
		}
	}
}

fn (mut b Builder) build_expr_stmt(stmt ast.ExprStmt) {
	if stmt.expr is ast.UnsafeExpr {
		for s in stmt.expr.stmts {
			b.build_stmt(s)
		}
		return
	}
	if stmt.expr is ast.IfExpr {
		b.build_if_stmt(stmt.expr)
		return
	}
	b.build_expr(stmt.expr)
}

fn (mut b Builder) unwrap_ident(expr ast.Expr) ?ast.Ident {
	if expr is ast.Ident {
		return expr
	}
	if expr is ast.ModifierExpr {
		return b.unwrap_ident(expr.expr)
	}
	return none
}

fn (mut b Builder) build_assign(stmt ast.AssignStmt) {
	for i, lhs in stmt.lhs {
		if i >= stmt.rhs.len {
			break
		}
		rhs := stmt.rhs[i]
		rhs_val := b.build_expr(rhs)

		if ident := b.unwrap_ident(lhs) {
			if stmt.op == .decl_assign {
				// New variable declaration - use the actual type of the built RHS value
				rhs_type := b.mod.values[rhs_val].typ
				alloca := b.mod.add_instr(.alloca, b.cur_block, b.mod.type_store.get_ptr(rhs_type),
					[]ValueID{})
				b.mod.add_instr(.store, b.cur_block, 0, [rhs_val, alloca])
				b.vars[ident.name] = alloca
			} else if ident.name == '_' {
				// Discard
				continue
			} else {
				// Assignment to existing variable (local or global)
				mut ptr := ValueID(0)
				if p := b.vars[ident.name] {
					ptr = p
				} else if glob_id := b.find_global(ident.name) {
					ptr = glob_id
				}
				if ptr != 0 {
					if stmt.op == .assign {
						b.mod.add_instr(.store, b.cur_block, 0, [rhs_val, ptr])
					} else {
						// Compound assignment (+=, -=, etc.)
						ptr_typ := b.mod.values[ptr].typ
						elem_typ := b.mod.type_store.types[ptr_typ].elem_type
						loaded := b.mod.add_instr(.load, b.cur_block, elem_typ, [ptr])
						op := match stmt.op {
							.plus_assign { OpCode.add }
							.minus_assign { OpCode.sub }
							.mul_assign { OpCode.mul }
							.div_assign { OpCode.sdiv }
							.mod_assign { OpCode.srem }
							.left_shift_assign { OpCode.shl }
							.right_shift_assign { OpCode.ashr }
							.and_assign { OpCode.and_ }
							.or_assign { OpCode.or_ }
							.xor_assign { OpCode.xor }
							else { OpCode.add }
						}
						result := b.mod.add_instr(op, b.cur_block, b.mod.values[loaded].typ,
							[loaded, rhs_val])
						b.mod.add_instr(.store, b.cur_block, 0, [result, ptr])
					}
				}
			}
		} else if lhs is ast.SelectorExpr {
			// Field assignment: obj.field = val
			base := b.build_addr(lhs)
			if base != 0 {
				b.mod.add_instr(.store, b.cur_block, 0, [rhs_val, base])
			}
		} else if lhs is ast.IndexExpr {
			// Index assignment: arr[i] = val
			base := b.build_addr(lhs)
			if base != 0 {
				b.mod.add_instr(.store, b.cur_block, 0, [rhs_val, base])
			}
		}
	}
}

fn (mut b Builder) build_return(stmt ast.ReturnStmt) {
	if stmt.exprs.len == 0 {
		b.mod.add_instr(.ret, b.cur_block, 0, []ValueID{})
	} else if stmt.exprs.len == 1 {
		val := b.build_expr(stmt.exprs[0])
		b.mod.add_instr(.ret, b.cur_block, 0, [val])
	} else {
		// Multiple return values -> tuple
		mut vals := []ValueID{cap: stmt.exprs.len}
		for expr in stmt.exprs {
			vals << b.build_expr(expr)
		}
		// For now, just return the first value
		b.mod.add_instr(.ret, b.cur_block, 0, [vals[0]])
	}
}

fn (mut b Builder) build_for(stmt ast.ForStmt) {
	has_init := stmt.init !is ast.EmptyStmt
	has_cond := stmt.cond !is ast.EmptyExpr
	has_post := stmt.post !is ast.EmptyStmt

	// Build init in current block
	if has_init {
		b.build_stmt(stmt.init)
	}

	// Create blocks
	cond_block := b.mod.add_block(b.cur_func, 'for_cond')
	body_block := b.mod.add_block(b.cur_func, 'for_body')
	post_block := if has_post {
		b.mod.add_block(b.cur_func, 'for_post')
	} else {
		cond_block
	}
	exit_block := b.mod.add_block(b.cur_func, 'for_exit')

	// Jump to condition
	b.mod.add_instr(.jmp, b.cur_block, 0, [b.mod.blocks[cond_block].val_id])
	b.add_edge(b.cur_block, cond_block)

	// Condition block
	b.cur_block = cond_block
	if has_cond {
		cond_val := b.build_expr(stmt.cond)
		b.mod.add_instr(.br, b.cur_block, 0, [cond_val, b.mod.blocks[body_block].val_id, b.mod.blocks[exit_block].val_id])
		b.add_edge(cond_block, body_block)
		b.add_edge(cond_block, exit_block)
	} else {
		// Infinite loop
		b.mod.add_instr(.jmp, b.cur_block, 0, [b.mod.blocks[body_block].val_id])
		b.add_edge(cond_block, body_block)
	}

	// Body block
	b.cur_block = body_block
	b.loop_stack << LoopInfo{
		cond_block: post_block
		exit_block: exit_block
	}
	b.build_stmts(stmt.stmts)
	b.loop_stack.pop()

	if !b.block_has_terminator(b.cur_block) {
		// Jump to post (or back to cond)
		target := if has_post { post_block } else { cond_block }
		b.mod.add_instr(.jmp, b.cur_block, 0, [b.mod.blocks[target].val_id])
		b.add_edge(b.cur_block, target)
	}

	// Post block
	if has_post {
		b.cur_block = post_block
		b.build_stmt(stmt.post)
		if !b.block_has_terminator(b.cur_block) {
			b.mod.add_instr(.jmp, b.cur_block, 0, [b.mod.blocks[cond_block].val_id])
			b.add_edge(post_block, cond_block)
		}
	}

	b.cur_block = exit_block
}

fn (mut b Builder) build_if_stmt(node ast.IfExpr) {
	if node.cond is ast.EmptyExpr {
		// Pure else block
		b.build_stmts(node.stmts)
		return
	}

	then_block := b.mod.add_block(b.cur_func, 'if_then')
	merge_block := b.mod.add_block(b.cur_func, 'if_merge')

	has_else := node.else_expr !is ast.EmptyExpr
	else_block := if has_else {
		b.mod.add_block(b.cur_func, 'if_else')
	} else {
		merge_block
	}

	// Condition
	cond_val := b.build_expr(node.cond)
	b.mod.add_instr(.br, b.cur_block, 0, [cond_val, b.mod.blocks[then_block].val_id, b.mod.blocks[else_block].val_id])
	b.add_edge(b.cur_block, then_block)
	b.add_edge(b.cur_block, else_block)

	// Then
	b.cur_block = then_block
	b.build_stmts(node.stmts)
	if !b.block_has_terminator(b.cur_block) {
		b.mod.add_instr(.jmp, b.cur_block, 0, [b.mod.blocks[merge_block].val_id])
		b.add_edge(b.cur_block, merge_block)
	}

	// Else
	if has_else {
		b.cur_block = else_block
		if node.else_expr is ast.IfExpr {
			else_if := node.else_expr as ast.IfExpr
			if else_if.cond is ast.EmptyExpr {
				// Pure else
				b.build_stmts(else_if.stmts)
			} else {
				b.build_if_stmt(else_if)
			}
		} else if node.else_expr is ast.UnsafeExpr {
			for s in node.else_expr.stmts {
				b.build_stmt(s)
			}
		}
		if !b.block_has_terminator(b.cur_block) {
			b.mod.add_instr(.jmp, b.cur_block, 0, [b.mod.blocks[merge_block].val_id])
			b.add_edge(b.cur_block, merge_block)
		}
	}

	b.cur_block = merge_block
	// If the merge block has no predecessors (both branches returned/jumped elsewhere),
	// mark it as unreachable so no implicit return is added
	if b.mod.blocks[merge_block].preds.len == 0 {
		b.mod.add_instr(.unreachable, b.cur_block, 0, []ValueID{})
	}
}

fn (mut b Builder) build_flow_control(stmt ast.FlowControlStmt) {
	if b.loop_stack.len == 0 {
		return
	}
	loop_info := b.loop_stack[b.loop_stack.len - 1]
	if stmt.op == .key_break {
		b.mod.add_instr(.jmp, b.cur_block, 0, [b.mod.blocks[loop_info.exit_block].val_id])
		b.add_edge(b.cur_block, loop_info.exit_block)
	} else if stmt.op == .key_continue {
		b.mod.add_instr(.jmp, b.cur_block, 0, [b.mod.blocks[loop_info.cond_block].val_id])
		b.add_edge(b.cur_block, loop_info.cond_block)
	}
}

fn (mut b Builder) build_assert(stmt ast.AssertStmt) {
	// assert expr -> if (!expr) { exit(1) }
	cond := b.build_expr(stmt.expr)
	pass_block := b.mod.add_block(b.cur_func, 'assert_pass')
	fail_block := b.mod.add_block(b.cur_func, 'assert_fail')

	b.mod.add_instr(.br, b.cur_block, 0, [cond, b.mod.blocks[pass_block].val_id, b.mod.blocks[fail_block].val_id])
	b.add_edge(b.cur_block, pass_block)
	b.add_edge(b.cur_block, fail_block)

	// Fail block: call exit(1)
	b.cur_block = fail_block
	one := b.mod.get_or_add_const(b.mod.type_store.get_int(32), '1')
	exit_ref := b.get_or_create_fn_ref('exit', b.mod.type_store.get_int(32))
	b.mod.add_instr(.call, b.cur_block, 0, [exit_ref, one])
	b.mod.add_instr(.unreachable, b.cur_block, 0, []ValueID{})

	b.cur_block = pass_block
}

// --- Expression building ---

fn (mut b Builder) build_expr(expr ast.Expr) ValueID {
	match expr {
		ast.BasicLiteral {
			return b.build_basic_literal(expr)
		}
		ast.StringLiteral {
			return b.build_string_literal(expr)
		}
		ast.Ident {
			return b.build_ident(expr)
		}
		ast.InfixExpr {
			return b.build_infix(expr)
		}
		ast.PrefixExpr {
			return b.build_prefix(expr)
		}
		ast.CallExpr {
			return b.build_call(expr)
		}
		ast.SelectorExpr {
			return b.build_selector(expr)
		}
		ast.IndexExpr {
			return b.build_index(expr)
		}
		ast.IfExpr {
			return b.build_if_expr(expr)
		}
		ast.InitExpr {
			return b.build_init_expr(expr)
		}
		ast.CastExpr {
			return b.build_cast(expr)
		}
		ast.ParenExpr {
			return b.build_expr(expr.expr)
		}
		ast.ModifierExpr {
			return b.build_expr(expr.expr)
		}
		ast.UnsafeExpr {
			if expr.stmts.len > 0 {
				for i := 0; i < expr.stmts.len - 1; i++ {
					b.build_stmt(expr.stmts[i])
				}
				last := expr.stmts[expr.stmts.len - 1]
				if last is ast.ExprStmt {
					return b.build_expr(last.expr)
				}
				b.build_stmt(last)
			}
			return b.mod.get_or_add_const(b.mod.type_store.get_int(32), '0')
		}
		ast.Keyword {
			return b.build_keyword(expr)
		}
		ast.KeywordOperator {
			return b.build_keyword_operator(expr)
		}
		ast.PostfixExpr {
			return b.build_postfix(expr)
		}
		ast.ArrayInitExpr {
			return b.build_array_init_expr(expr)
		}
		ast.MapInitExpr {
			// TODO: map init
			return b.mod.get_or_add_const(b.mod.type_store.get_int(64), '0')
		}
		ast.StringInterLiteral {
			// TODO: string interpolation
			return b.build_string_literal(ast.StringLiteral{
				kind:  .v
				value: ''
			})
		}
		ast.MatchExpr {
			// Should be lowered by transformer
			return b.mod.get_or_add_const(b.mod.type_store.get_int(32), '0')
		}
		ast.OrExpr {
			return b.build_expr(expr.expr)
		}
		ast.FnLiteral {
			// TODO: closures
			return b.mod.get_or_add_const(b.mod.type_store.get_int(64), '0')
		}
		ast.RangeExpr {
			return b.mod.get_or_add_const(b.mod.type_store.get_int(64), '0')
		}
		ast.CallOrCastExpr {
			return b.build_call_or_cast(expr)
		}
		ast.AsCastExpr {
			return b.build_expr(expr.expr)
		}
		ast.AssocExpr {
			return b.mod.get_or_add_const(b.mod.type_store.get_int(64), '0')
		}
		ast.Tuple {
			if expr.exprs.len > 0 {
				return b.build_expr(expr.exprs[0])
			}
			return b.mod.get_or_add_const(b.mod.type_store.get_int(32), '0')
		}
		else {
			return b.mod.get_or_add_const(b.mod.type_store.get_int(32), '0')
		}
	}
}

fn (mut b Builder) build_basic_literal(lit ast.BasicLiteral) ValueID {
	match lit.kind {
		.key_true {
			return b.mod.get_or_add_const(b.mod.type_store.get_int(1), '1')
		}
		.key_false {
			return b.mod.get_or_add_const(b.mod.type_store.get_int(1), '0')
		}
		.char {
			typ := b.mod.type_store.get_int(8)
			return b.mod.get_or_add_const(typ, lit.value)
		}
		.number {
			if lit.value.contains('.') {
				typ := b.mod.type_store.get_float(64)
				return b.mod.get_or_add_const(typ, lit.value)
			}
			typ := b.expr_type(ast.Expr(lit))
			return b.mod.get_or_add_const(typ, lit.value)
		}
		else {
			// Integer or other
			typ := b.expr_type(ast.Expr(lit))
			return b.mod.get_or_add_const(typ, lit.value)
		}
	}
}

fn (mut b Builder) build_string_literal(lit ast.StringLiteral) ValueID {
	// Strip surrounding quotes from V string literal values
	mut val := lit.value
	if val.len >= 2 && ((val[0] == `'` && val[val.len - 1] == `'`)
		|| (val[0] == `"` && val[val.len - 1] == `"`)) {
		val = val[1..val.len - 1]
	}
	if lit.kind == .c {
		// C string literal -> raw i8* pointer
		i8_t := b.mod.type_store.get_int(8)
		ptr_t := b.mod.type_store.get_ptr(i8_t)
		return b.mod.add_value_node(.c_string_literal, ptr_t, val, 0)
	}
	typ := b.get_string_type()
	return b.mod.add_value_node(.string_literal, typ, val, 0)
}

fn (mut b Builder) build_ident(ident ast.Ident) ValueID {
	if ident.name in b.vars {
		ptr := b.vars[ident.name]
		ptr_typ := b.mod.values[ptr].typ
		elem_typ := b.mod.type_store.types[ptr_typ].elem_type
		return b.mod.add_instr(.load, b.cur_block, elem_typ, [ptr])
	}
	// Could be a constant, enum value, or function reference
	if ident.name in b.enum_values {
		val := b.enum_values[ident.name]
		return b.mod.get_or_add_const(b.mod.type_store.get_int(32), val.str())
	}
	// Try as function reference
	if ident.name in b.fn_index {
		return b.get_or_create_fn_ref(ident.name, 0)
	}
	// Try as global variable
	if glob_id := b.find_global(ident.name) {
		glob_typ := b.mod.values[glob_id].typ
		elem_typ := b.mod.type_store.types[glob_typ].elem_type
		return b.mod.add_instr(.load, b.cur_block, elem_typ, [glob_id])
	}
	return b.mod.get_or_add_const(b.mod.type_store.get_int(64), '0')
}

fn (mut b Builder) find_global(name string) ?ValueID {
	for v in b.mod.values {
		if v.kind == .global && v.name == name {
			return v.id
		}
	}
	return none
}

fn (mut b Builder) build_infix(expr ast.InfixExpr) ValueID {
	lhs := b.build_expr(expr.lhs)
	rhs := b.build_expr(expr.rhs)
	result_type := b.expr_type(ast.Expr(expr))

	op := match expr.op {
		.plus { OpCode.add }
		.minus { OpCode.sub }
		.mul { OpCode.mul }
		.div { OpCode.sdiv }
		.mod { OpCode.srem }
		.eq { OpCode.eq }
		.ne { OpCode.ne }
		.lt { OpCode.lt }
		.gt { OpCode.gt }
		.le { OpCode.le }
		.ge { OpCode.ge }
		.and { OpCode.and_ }
		.logical_or { OpCode.or_ }
		.amp { OpCode.and_ }
		.pipe { OpCode.or_ }
		.xor { OpCode.xor }
		.left_shift { OpCode.shl }
		.right_shift { OpCode.ashr }
		else { OpCode.add }
	}

	return b.mod.add_instr(op, b.cur_block, result_type, [lhs, rhs])
}

fn (mut b Builder) build_prefix(expr ast.PrefixExpr) ValueID {
	// Special case: &InitExpr (heap/pointer to struct init)
	// Build the struct via alloca+GEP+store but return the pointer instead of loading
	if expr.op == .amp && expr.expr is ast.InitExpr {
		return b.build_init_expr_ptr(expr.expr)
	}

	val := b.build_expr(expr.expr)

	match expr.op {
		.minus {
			zero := b.mod.get_or_add_const(b.mod.values[val].typ, '0')
			return b.mod.add_instr(.sub, b.cur_block, b.mod.values[val].typ, [zero, val])
		}
		.not {
			one := b.mod.get_or_add_const(b.mod.type_store.get_int(1), '1')
			return b.mod.add_instr(.xor, b.cur_block, b.mod.type_store.get_int(1), [
				val,
				one,
			])
		}
		.amp {
			// Address-of: return the alloca pointer for the variable
			if expr.expr is ast.Ident {
				if ptr := b.vars[expr.expr.name] {
					return ptr
				}
			}
			return val
		}
		.bit_not {
			neg_one := b.mod.get_or_add_const(b.mod.values[val].typ, '-1')
			return b.mod.add_instr(.xor, b.cur_block, b.mod.values[val].typ, [val, neg_one])
		}
		else {
			return val
		}
	}
}

fn (mut b Builder) build_call(expr ast.CallExpr) ValueID {
	// Resolve function name
	fn_name := b.resolve_call_name(expr)
	ret_type := b.expr_type(ast.Expr(expr))

	// Build arguments
	mut args := []ValueID{}
	// For method calls, add receiver as first arg
	if expr.lhs is ast.SelectorExpr {
		sel := expr.lhs as ast.SelectorExpr
		// Check if this is a method call (not a module function call)
		if !b.is_module_name(sel.lhs) {
			mut receiver := b.build_expr(sel.lhs)
			// Auto-deref pointer receivers: if receiver is a pointer to a struct
			// but the method expects a value receiver, load through the pointer
			recv_typ := b.mod.values[receiver].typ
			if recv_typ < b.mod.type_store.types.len
				&& b.mod.type_store.types[recv_typ].kind == .ptr_t {
				pointee := b.mod.type_store.types[recv_typ].elem_type
				if pointee < b.mod.type_store.types.len
					&& b.mod.type_store.types[pointee].kind == .struct_t {
					// Check if method expects value receiver (not pointer)
					if fn_name in b.fn_index {
						fn_idx := b.fn_index[fn_name]
						if b.mod.funcs[fn_idx].params.len > 0 {
							param_type := b.mod.values[b.mod.funcs[fn_idx].params[0]].typ
							if param_type < b.mod.type_store.types.len
								&& b.mod.type_store.types[param_type].kind != .ptr_t {
								receiver = b.mod.add_instr(.load, b.cur_block, pointee,
									[receiver])
							}
						}
					} else {
						// Function not yet built — assume value receiver, auto-deref
						receiver = b.mod.add_instr(.load, b.cur_block, pointee, [
							receiver,
						])
					}
				}
			}
			args << receiver
		}
	}
	for arg in expr.args {
		// For mut arguments, pass the address (pointer) instead of the value
		if arg is ast.ModifierExpr && arg.kind == .key_mut {
			addr := b.build_addr(arg.expr)
			if addr != 0 {
				args << addr
			} else {
				args << b.build_expr(arg)
			}
		} else {
			args << b.build_expr(arg)
		}
	}

	// Auto-deref/ref arguments to match function parameter types
	if fn_name in b.fn_index {
		fn_idx := b.fn_index[fn_name]
		fn_params := b.mod.funcs[fn_idx].params
		for ai := 0; ai < args.len && ai < fn_params.len; ai++ {
			arg_type := b.mod.values[args[ai]].typ
			param_type := b.mod.values[fn_params[ai]].typ
			// Pointer arg but value param: auto-deref
			if arg_type < b.mod.type_store.types.len
				&& b.mod.type_store.types[arg_type].kind == .ptr_t
				&& param_type < b.mod.type_store.types.len
				&& b.mod.type_store.types[param_type].kind != .ptr_t {
				pointee := b.mod.type_store.types[arg_type].elem_type
				args[ai] = b.mod.add_instr(.load, b.cur_block, pointee, [args[ai]])
			}
		}
	}

	fn_ref := b.get_or_create_fn_ref(fn_name, ret_type)
	mut operands := []ValueID{cap: args.len + 1}
	operands << fn_ref
	operands << args

	return b.mod.add_instr(.call, b.cur_block, ret_type, operands)
}

fn (mut b Builder) is_module_name(expr ast.Expr) bool {
	if expr is ast.Ident {
		if b.env != unsafe { nil } {
			if scope := b.env.get_scope(b.cur_module) {
				if obj := scope.lookup_parent(expr.name, 0) {
					return obj is types.Module
				}
			}
		}
	}
	return false
}

fn (mut b Builder) resolve_call_name(expr ast.CallExpr) string {
	match expr.lhs {
		ast.Ident {
			name := expr.lhs.name
			// Check if it's a known function
			if name in b.fn_index {
				return name
			}
			// Try module-qualified
			qualified := '${b.cur_module}__${name}'
			if qualified in b.fn_index {
				return qualified
			}
			// Try builtin-qualified (transformer remaps builtin__X to X)
			builtin_qualified := 'builtin__${name}'
			if builtin_qualified in b.fn_index {
				return builtin_qualified
			}
			// For names with module prefix (e.g., v__error), try replacing
			// the module prefix with builtin__
			if idx := name.index('__') {
				suffix := name[idx + 2..]
				alt := 'builtin__${suffix}'
				if alt in b.fn_index {
					return alt
				}
			}
			// C functions: strip C__ prefix for direct C interop
			if name.starts_with('C__') {
				return name[3..]
			}
			return name
		}
		ast.SelectorExpr {
			sel := expr.lhs as ast.SelectorExpr
			if b.is_module_name(sel.lhs) {
				// Module function call: module.fn()
				mod_name := sel.lhs.name()
				// C functions: C.puts() → just 'puts' for direct C interop
				if mod_name == 'C' {
					return sel.rhs.name
				}
				qualified := '${mod_name}__${sel.rhs.name}'
				if qualified in b.fn_index {
					return qualified
				}
				// Try resolving the module alias
				if b.env != unsafe { nil } {
					if scope := b.env.get_scope(b.cur_module) {
						if obj := scope.lookup_parent(mod_name, 0) {
							if obj is types.Module {
								real_mod := obj.name.replace('.', '_')
								real_qualified := '${real_mod}__${sel.rhs.name}'
								if real_qualified in b.fn_index {
									return real_qualified
								}
								return real_qualified
							}
						}
					}
				}
				return qualified
			}
			// Method call: expr.method()
			mut receiver_type := b.get_receiver_type_name(sel.lhs)
			// Strip pointer prefix for method resolution (e.g., Point* → Point)
			for receiver_type.ends_with('*') {
				receiver_type = receiver_type[..receiver_type.len - 1]
			}
			method_name := '${receiver_type}__${sel.rhs.name}'
			if method_name in b.fn_index {
				return method_name
			}
			// Try searching all functions for one matching __method_name
			suffix := '__${sel.rhs.name}'
			for fname, _ in b.fn_index {
				if fname.ends_with(suffix) {
					return fname
				}
			}
			return method_name
		}
		else {
			return 'unknown_fn'
		}
	}
}

fn (mut b Builder) get_receiver_type_name(expr ast.Expr) string {
	if b.env != unsafe { nil } {
		pos := expr.pos()
		if pos.id != 0 {
			if typ := b.env.get_expr_type(pos.id) {
				return b.types_type_c_name(typ)
			}
		}
	}
	if expr is ast.Ident {
		return expr.name
	}
	return 'unknown'
}

fn (mut b Builder) build_selector(expr ast.SelectorExpr) ValueID {
	// Check for enum shorthand: .field in certain contexts
	if expr.lhs is ast.EmptyExpr || (expr.lhs is ast.Ident && expr.lhs.name == '') {
		// Enum shorthand — try to look up a resolved name
		field_name := expr.rhs.name
		// Try common enum patterns
		for key, val in b.enum_values {
			if key.ends_with('__${field_name}') {
				return b.mod.get_or_add_const(b.mod.type_store.get_int(32), val.str())
			}
		}
		return b.mod.get_or_add_const(b.mod.type_store.get_int(32), '0')
	}

	// Check for qualified enum access: EnumType.value
	if expr.lhs is ast.Ident {
		// Try: EnumType__value (local) or module__EnumType__value (qualified)
		enum_key := '${expr.lhs.name}__${expr.rhs.name}'
		if enum_key in b.enum_values {
			return b.mod.get_or_add_const(b.mod.type_store.get_int(32), b.enum_values[enum_key].str())
		}
		qualified_key := '${b.cur_module}__${enum_key}'
		if qualified_key in b.enum_values {
			return b.mod.get_or_add_const(b.mod.type_store.get_int(32), b.enum_values[qualified_key].str())
		}
	}

	// Use extractvalue for struct field access
	mut base := b.build_expr(expr.lhs)
	// If base is a pointer to struct (mut param or heap alloc), auto-deref
	base_typ := b.mod.values[base].typ
	if base_typ < b.mod.type_store.types.len && b.mod.type_store.types[base_typ].kind == .ptr_t {
		pointee := b.mod.type_store.types[base_typ].elem_type
		if pointee < b.mod.type_store.types.len && b.mod.type_store.types[pointee].kind == .struct_t {
			base = b.mod.add_instr(.load, b.cur_block, pointee, [base])
		}
	}
	field_idx := b.field_index(expr, base)
	// Determine result type: prefer type environment, fall back to SSA struct field type
	mut result_type := b.expr_type(ast.Expr(expr))
	if result_type == b.mod.type_store.get_int(64) {
		// expr_type returned i64 fallback — try to get actual field type from SSA struct
		actual_base_type := b.mod.values[base].typ
		if actual_base_type < b.mod.type_store.types.len {
			typ := b.mod.type_store.types[actual_base_type]
			if typ.kind == .struct_t && field_idx < typ.fields.len {
				result_type = typ.fields[field_idx]
			}
		}
	}
	return b.mod.add_instr(.extractvalue, b.cur_block, result_type, [base,
		b.mod.get_or_add_const(b.mod.type_store.get_int(32), field_idx.str())])
}

fn (mut b Builder) field_index(expr ast.SelectorExpr, base ValueID) int {
	// Use type environment to find field index
	if b.env != unsafe { nil } {
		pos := expr.lhs.pos()
		if pos.id != 0 {
			if typ := b.env.get_expr_type(pos.id) {
				st := b.unwrap_to_struct(typ)
				if st.name != '' {
					for i, f in st.fields {
						if f.name == expr.rhs.name {
							return i
						}
					}
				}
			}
		}
	}
	// Fallback: look up field name in the SSA struct type of the base value
	base_type_id := b.mod.values[base].typ
	if base_type_id < b.mod.type_store.types.len {
		mut typ := b.mod.type_store.types[base_type_id]
		// Dereference pointer(s) to get to the struct type
		for typ.kind == .ptr_t && typ.elem_type < b.mod.type_store.types.len {
			typ = b.mod.type_store.types[typ.elem_type]
		}
		if typ.kind == .struct_t {
			for i, name in typ.field_names {
				if name == expr.rhs.name {
					return i
				}
			}
		}
	}
	return 0
}

fn (mut b Builder) unwrap_to_struct(t types.Type) types.Struct {
	match t {
		types.Struct {
			return t
		}
		types.Pointer {
			return b.unwrap_to_struct(t.base_type)
		}
		types.Alias {
			return b.unwrap_to_struct(t.base_type)
		}
		else {
			return types.Struct{}
		}
	}
}

fn (mut b Builder) build_index(expr ast.IndexExpr) ValueID {
	base := b.build_expr(expr.lhs)
	index := b.build_expr(expr.expr)
	result_type := b.expr_type(ast.Expr(expr))

	base_type_id := b.mod.values[base].typ
	array_type := b.get_array_type()

	// Check if base is a dynamic array (array struct) — need to access .data field
	if array_type != 0 && base_type_id == array_type {
		// Extract .data field (index 0), cast to element pointer, then GEP
		i8_t := b.mod.type_store.get_int(8)
		void_ptr := b.mod.type_store.get_ptr(i8_t)
		data_ptr := b.mod.add_instr(.extractvalue, b.cur_block, void_ptr, [base,
			b.mod.get_or_add_const(b.mod.type_store.get_int(32), '0')])
		// Cast void* to element*
		elem_ptr_type := b.mod.type_store.get_ptr(result_type)
		typed_ptr := b.mod.add_instr(.bitcast, b.cur_block, elem_ptr_type, [data_ptr])
		// GEP to the element
		elem_addr := b.mod.add_instr(.get_element_ptr, b.cur_block, elem_ptr_type, [
			typed_ptr,
			index,
		])
		// Load the element
		return b.mod.add_instr(.load, b.cur_block, result_type, [elem_addr])
	}

	// Check if base is a pointer (e.g., from alloca for fixed-size array literal)
	// For ptr(T), element type is T — use that instead of expr_type fallback
	if base_type_id < b.mod.type_store.types.len {
		base_typ := b.mod.type_store.types[base_type_id]
		if base_typ.kind == .ptr_t && base_typ.elem_type != 0 {
			elem_type := base_typ.elem_type
			// GEP to the element address, then load
			elem_addr := b.mod.add_instr(.get_element_ptr, b.cur_block, base_type_id,
				[base, index])
			return b.mod.add_instr(.load, b.cur_block, elem_type, [elem_addr])
		}
	}

	return b.mod.add_instr(.get_element_ptr, b.cur_block, result_type, [base, index])
}

fn (mut b Builder) build_if_expr(node ast.IfExpr) ValueID {
	// If used as expression, returns a value
	result_type := b.expr_type(ast.Expr(node))
	result_alloca := b.mod.add_instr(.alloca, b.cur_block, b.mod.type_store.get_ptr(result_type),
		[]ValueID{})

	then_block := b.mod.add_block(b.cur_func, 'ifx_then')
	merge_block := b.mod.add_block(b.cur_func, 'ifx_merge')
	has_else := node.else_expr !is ast.EmptyExpr
	else_block := if has_else {
		b.mod.add_block(b.cur_func, 'ifx_else')
	} else {
		merge_block
	}

	cond := b.build_expr(node.cond)
	b.mod.add_instr(.br, b.cur_block, 0, [cond, b.mod.blocks[then_block].val_id, b.mod.blocks[else_block].val_id])
	b.add_edge(b.cur_block, then_block)
	b.add_edge(b.cur_block, else_block)

	// Then
	b.cur_block = then_block
	mut then_val := ValueID(0)
	if node.stmts.len > 0 {
		for i := 0; i < node.stmts.len - 1; i++ {
			b.build_stmt(node.stmts[i])
		}
		last := node.stmts[node.stmts.len - 1]
		if last is ast.ExprStmt {
			then_val = b.build_expr(last.expr)
		} else {
			b.build_stmt(last)
		}
	}
	if then_val != 0 {
		b.mod.add_instr(.store, b.cur_block, 0, [then_val, result_alloca])
	}
	if !b.block_has_terminator(b.cur_block) {
		b.mod.add_instr(.jmp, b.cur_block, 0, [b.mod.blocks[merge_block].val_id])
		b.add_edge(b.cur_block, merge_block)
	}

	// Else
	if has_else {
		b.cur_block = else_block
		mut else_val := ValueID(0)
		if node.else_expr is ast.IfExpr {
			else_if := node.else_expr as ast.IfExpr
			if else_if.cond is ast.EmptyExpr {
				// Pure else
				if else_if.stmts.len > 0 {
					for i := 0; i < else_if.stmts.len - 1; i++ {
						b.build_stmt(else_if.stmts[i])
					}
					last := else_if.stmts[else_if.stmts.len - 1]
					if last is ast.ExprStmt {
						else_val = b.build_expr(last.expr)
					} else {
						b.build_stmt(last)
					}
				}
			} else {
				else_val = b.build_if_expr(else_if)
			}
		} else {
			else_val = b.build_expr(node.else_expr)
		}
		if else_val != 0 {
			b.mod.add_instr(.store, b.cur_block, 0, [else_val, result_alloca])
		}
		if !b.block_has_terminator(b.cur_block) {
			b.mod.add_instr(.jmp, b.cur_block, 0, [b.mod.blocks[merge_block].val_id])
			b.add_edge(b.cur_block, merge_block)
		}
	}

	b.cur_block = merge_block
	return b.mod.add_instr(.load, b.cur_block, result_type, [result_alloca])
}

fn (mut b Builder) build_array_init_expr(expr ast.ArrayInitExpr) ValueID {
	// If the array init has elements, alloca a fixed-size array on the stack,
	// store each element, and return the pointer.
	if expr.exprs.len > 0 {
		mut elem_vals := []ValueID{cap: expr.exprs.len}
		for e in expr.exprs {
			elem_vals << b.build_expr(e)
		}
		mut elem_type := b.mod.type_store.get_int(32) // default int
		if elem_vals.len > 0 {
			elem_type = b.mod.values[elem_vals[0]].typ
		}
		// Allocate fixed-size array on stack and store each element
		ptr_type := b.mod.type_store.get_ptr(elem_type)
		alloca := b.mod.add_instr(.alloca, b.cur_block, ptr_type, [
			b.mod.get_or_add_const(b.mod.type_store.get_int(32), elem_vals.len.str()),
		])
		// Store each element via GEP + store
		i32_t := b.mod.type_store.get_int(32)
		for i, val in elem_vals {
			idx := b.mod.get_or_add_const(i32_t, i.str())
			gep := b.mod.add_instr(.get_element_ptr, b.cur_block, ptr_type, [alloca, idx])
			b.mod.add_instr(.store, b.cur_block, 0, [val, gep])
		}
		return alloca
	}

	// Empty array or dynamic array with len/cap - these should have been
	// transformed to __new_array_with_default_noscan calls by the transformer.
	// Return zero-initialized array struct as fallback.
	arr_type := b.get_array_type()
	return b.mod.get_or_add_const(arr_type, '0')
}

fn (mut b Builder) build_init_expr(expr ast.InitExpr) ValueID {
	// Struct initialization: Type{ field: value, ... }
	// Uses struct_init opcode: keeps aggregates as SSA values for better optimization.
	// Lowered to alloca/GEP/store only when address is taken (build_init_expr_ptr).

	// Resolve the struct type
	mut struct_type := b.ast_type_to_ssa(expr.typ)
	if struct_type == b.mod.type_store.get_int(64) {
		env_type := b.expr_type(ast.Expr(expr))
		if env_type != b.mod.type_store.get_int(64) {
			struct_type = env_type
		}
	}

	// Get the type info for field name lookup
	typ_info := b.mod.type_store.types[struct_type]
	num_fields := typ_info.field_names.len

	if num_fields == 0 {
		// Not a known struct or has no fields: fall back to zero constant
		return b.mod.get_or_add_const(struct_type, '0')
	}

	// Build field values in declaration order
	mut field_vals := []ValueID{cap: num_fields}
	mut initialized_fields := map[string]int{} // field name -> index in expr.fields

	// Map explicit field inits by name
	for fi, field in expr.fields {
		initialized_fields[field.name] = fi
	}

	for fi in 0 .. num_fields {
		fname := typ_info.field_names[fi]
		field_type := if fi < typ_info.fields.len {
			typ_info.fields[fi]
		} else {
			b.mod.type_store.get_int(64)
		}

		if idx := initialized_fields[fname] {
			if idx >= 0 && idx < expr.fields.len {
				field_vals << b.build_expr(expr.fields[idx].value)
			} else {
				field_vals << b.mod.get_or_add_const(field_type, '0')
			}
		} else {
			// Zero-initialize unset fields
			field_vals << b.mod.get_or_add_const(field_type, '0')
		}
	}

	return b.mod.add_instr(.struct_init, b.cur_block, struct_type, field_vals)
}

// build_init_expr_ptr: like build_init_expr but returns the pointer (for &Point{...}).
// This is the "address taken" case — forces lowering to memory.
fn (mut b Builder) build_init_expr_ptr(expr ast.InitExpr) ValueID {
	struct_val := b.build_init_expr(expr)
	val_type := b.mod.values[struct_val].typ
	ptr_type := b.mod.type_store.get_ptr(val_type)
	alloca := b.mod.add_instr(.alloca, b.cur_block, ptr_type, []ValueID{})
	b.mod.add_instr(.store, b.cur_block, 0, [struct_val, alloca])
	return alloca
}

fn (mut b Builder) build_cast(expr ast.CastExpr) ValueID {
	val := b.build_expr(expr.expr)
	target_type := b.ast_type_to_ssa(expr.typ)
	src_type := b.mod.values[val].typ

	if src_type == target_type {
		return val
	}

	// Determine cast kind based on types
	src := b.mod.type_store.types[src_type]
	dst := b.mod.type_store.types[target_type]

	if src.kind == .int_t && dst.kind == .int_t {
		if src.width < dst.width {
			return b.mod.add_instr(.sext, b.cur_block, target_type, [val])
		} else if src.width > dst.width {
			return b.mod.add_instr(.trunc, b.cur_block, target_type, [val])
		}
	} else if src.kind == .int_t && dst.kind == .float_t {
		return b.mod.add_instr(.sitofp, b.cur_block, target_type, [val])
	} else if src.kind == .float_t && dst.kind == .int_t {
		return b.mod.add_instr(.fptosi, b.cur_block, target_type, [val])
	}

	return b.mod.add_instr(.bitcast, b.cur_block, target_type, [val])
}

fn (mut b Builder) build_call_or_cast(expr ast.CallOrCastExpr) ValueID {
	// CallOrCastExpr is produced by the parser when it can't distinguish
	// between a function call and a type cast, e.g., int(x) or voidptr(p).
	// After type checking, the transformer usually resolves these, but some
	// may remain as casts. Treat as cast: convert expr to target type.
	val := b.build_expr(expr.expr)
	target_type := b.ast_type_to_ssa(expr.lhs)
	src_type := b.mod.values[val].typ

	if src_type == target_type || target_type == 0 {
		return val
	}

	src := b.mod.type_store.types[src_type]
	dst := b.mod.type_store.types[target_type]

	if src.kind == .int_t && dst.kind == .int_t {
		if src.width < dst.width {
			return b.mod.add_instr(.sext, b.cur_block, target_type, [val])
		} else if src.width > dst.width {
			return b.mod.add_instr(.trunc, b.cur_block, target_type, [val])
		}
		return val
	} else if src.kind == .int_t && dst.kind == .float_t {
		return b.mod.add_instr(.sitofp, b.cur_block, target_type, [val])
	} else if src.kind == .float_t && dst.kind == .int_t {
		return b.mod.add_instr(.fptosi, b.cur_block, target_type, [val])
	} else if src.kind == .ptr_t && dst.kind == .ptr_t {
		return b.mod.add_instr(.bitcast, b.cur_block, target_type, [val])
	} else if src.kind == .int_t && dst.kind == .ptr_t {
		return b.mod.add_instr(.bitcast, b.cur_block, target_type, [val])
	} else if src.kind == .ptr_t && dst.kind == .int_t {
		return b.mod.add_instr(.bitcast, b.cur_block, target_type, [val])
	}

	return b.mod.add_instr(.bitcast, b.cur_block, target_type, [val])
}

fn (mut b Builder) build_keyword(kw ast.Keyword) ValueID {
	match kw.tok {
		.key_true {
			return b.mod.get_or_add_const(b.mod.type_store.get_int(1), '1')
		}
		.key_false {
			return b.mod.get_or_add_const(b.mod.type_store.get_int(1), '0')
		}
		.key_nil {
			i8_t := b.mod.type_store.get_int(8)
			ptr_t := b.mod.type_store.get_ptr(i8_t)
			return b.mod.get_or_add_const(ptr_t, '0')
		}
		else {
			return b.mod.get_or_add_const(b.mod.type_store.get_int(32), '0')
		}
	}
}

fn (mut b Builder) build_keyword_operator(kw ast.KeywordOperator) ValueID {
	match kw.op {
		.key_sizeof {
			if kw.exprs.len > 0 {
				size := b.sizeof_value(kw.exprs[0])
				if size > 0 {
					return b.mod.get_or_add_const(b.mod.type_store.get_int(32), size.str())
				}
			}
			// Fallback: sizeof(int) = 4
			return b.mod.get_or_add_const(b.mod.type_store.get_int(32), '4')
		}
		else {
			return b.mod.get_or_add_const(b.mod.type_store.get_int(64), '0')
		}
	}
}

fn (b &Builder) sizeof_value(expr ast.Expr) int {
	match expr {
		ast.Ident {
			return match expr.name {
				'int', 'i32', 'u32', 'f32' {
					4
				}
				'i8', 'u8', 'byte', 'bool' {
					1
				}
				'i16', 'u16' {
					2
				}
				'i64', 'u64', 'f64', 'isize', 'usize', 'voidptr', 'byteptr', 'charptr' {
					8
				}
				'rune' {
					4
				}
				else {
					// Look up struct type
					if tid := b.struct_types[expr.name] {
						return b.type_byte_size(tid)
					}
					8 // pointer size fallback
				}
			}
		}
		ast.Type {
			match expr {
				ast.ArrayType {
					if tid := b.struct_types['array'] {
						return b.type_byte_size(tid)
					}
					return 48 // approximate
				}
				ast.MapType {
					if tid := b.struct_types['map'] {
						return b.type_byte_size(tid)
					}
					return 120 // approximate
				}
				else {
					return 8
				}
			}
		}
		ast.SelectorExpr {
			lhs_name := expr.lhs.name()
			rhs_name := expr.rhs.name
			if lhs_name.len > 0 && rhs_name.len > 0 {
				qualified := '${lhs_name}__${rhs_name}'
				if tid := b.struct_types[qualified] {
					return b.type_byte_size(tid)
				}
			}
			return 8
		}
		else {
			return 8
		}
	}
}

fn (b &Builder) type_byte_size(tid TypeID) int {
	if tid == 0 || tid >= b.mod.type_store.types.len {
		return 0
	}
	typ := b.mod.type_store.types[tid]
	match typ.kind {
		.void_t {
			return 0
		}
		.int_t {
			return if typ.width <= 8 {
				1
			} else if typ.width <= 16 {
				2
			} else if typ.width <= 32 {
				4
			} else {
				8
			}
		}
		.float_t {
			return if typ.width <= 32 { 4 } else { 8 }
		}
		.ptr_t {
			return 8
		}
		.struct_t {
			mut size := 0
			for field in typ.fields {
				fs := b.type_byte_size(field)
				// Align to field size (simplified alignment)
				align := if fs >= 8 {
					8
				} else if fs >= 4 {
					4
				} else if fs >= 2 {
					2
				} else {
					1
				}
				rem := size % align
				if rem != 0 {
					size += align - rem
				}
				size += fs
			}
			return size
		}
		.array_t {
			return b.type_byte_size(typ.elem_type) * typ.len
		}
		.func_t {
			return 8
		}
		.label_t {
			return 0
		}
		.metadata_t {
			return 0
		}
	}
}

fn (mut b Builder) build_postfix(expr ast.PostfixExpr) ValueID {
	// expr++ or expr--
	if expr.expr is ast.Ident {
		if ptr := b.vars[expr.expr.name] {
			ptr_typ := b.mod.values[ptr].typ
			elem_typ := b.mod.type_store.types[ptr_typ].elem_type
			loaded := b.mod.add_instr(.load, b.cur_block, elem_typ, [ptr])
			one := b.mod.get_or_add_const(elem_typ, '1')
			op := if expr.op == .inc { OpCode.add } else { OpCode.sub }
			result := b.mod.add_instr(op, b.cur_block, elem_typ, [loaded, one])
			b.mod.add_instr(.store, b.cur_block, 0, [result, ptr])
			return loaded // postfix returns old value
		}
	}
	return b.build_expr(expr.expr)
}

// --- Address computation ---

fn (mut b Builder) build_addr(expr ast.Expr) ValueID {
	match expr {
		ast.Ident {
			if expr.name in b.vars {
				return b.vars[expr.name]
			}
			return 0
		}
		ast.SelectorExpr {
			// Get address of base (not the loaded value)
			mut base := b.build_addr(expr.lhs)
			if base == 0 {
				return 0
			}
			// If base is ptr-to-ptr-to-struct (mut param), load to get the struct pointer
			base_typ := b.mod.values[base].typ
			if base_typ < b.mod.type_store.types.len
				&& b.mod.type_store.types[base_typ].kind == .ptr_t {
				inner := b.mod.type_store.types[base_typ].elem_type
				if inner < b.mod.type_store.types.len
					&& b.mod.type_store.types[inner].kind == .ptr_t {
					pointee := b.mod.type_store.types[inner].elem_type
					if pointee < b.mod.type_store.types.len
						&& b.mod.type_store.types[pointee].kind == .struct_t {
						// Load from alloca to get the struct pointer
						base = b.mod.add_instr(.load, b.cur_block, inner, [base])
					}
				}
			}
			idx_val := b.mod.get_or_add_const(b.mod.type_store.get_int(32), b.field_index(expr,
				base).str())
			result_type := b.expr_type(ast.Expr(expr))
			return b.mod.add_instr(.get_element_ptr, b.cur_block, b.mod.type_store.get_ptr(result_type),
				[base, idx_val])
		}
		ast.IndexExpr {
			base := b.build_expr(expr.lhs)
			index := b.build_expr(expr.expr)
			result_type := b.expr_type(ast.Expr(expr))
			return b.mod.add_instr(.get_element_ptr, b.cur_block, b.mod.type_store.get_ptr(result_type),
				[base, index])
		}
		else {
			return 0
		}
	}
}

// --- Helpers ---

fn (mut b Builder) add_edge(from BlockID, to BlockID) {
	b.mod.blocks[from].succs << to
	b.mod.blocks[to].preds << from
}

fn (mut b Builder) get_or_create_fn_ref(name string, typ TypeID) ValueID {
	// Look for existing func_ref value
	for v in b.mod.values {
		if v.kind == .func_ref && v.name == name {
			return v.id
		}
	}
	return b.mod.add_value_node(.func_ref, typ, name, 0)
}
