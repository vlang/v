// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module abi

import os
import v2.ast
import v2.mir
import v2.parser
import v2.pref
import v2.ssa
import v2.token

fn parse_code_for_test(code string) []ast.File {
	tmp_file := '/tmp/v2_abi_parser_test_${os.getpid()}.v'
	os.write_file(tmp_file, code) or { panic(err) }
	defer {
		os.rm(tmp_file) or {}
	}
	prefs := &pref.Preferences{}
	mut file_set := token.FileSet.new()
	mut p := parser.Parser.new(prefs)
	return p.parse_files([tmp_file], mut file_set)
}

fn first_fn_decl(file ast.File) ast.FnDecl {
	for stmt in file.stmts {
		if stmt is ast.FnDecl {
			return stmt
		}
	}
	panic('expected function declaration in parsed file')
}

fn test_parser_sql_expr_allows_nested_braces_in_body() {
	files := parse_code_for_test("
module main

fn main() {
	req_status := true
	sql db {
		update User set
		name = 'Jengro',
		status = if req_status { 1 } else { 0 }
		status = fn [req_status] () u8 { if req_status { return 1 } else { return 0 } }()
		where id == '100'
	}
}
")
	assert files.len == 1
	main_fn := first_fn_decl(files[0])
	assert main_fn.stmts.len == 2
	assert main_fn.stmts[1] is ast.ExprStmt
	sql_stmt := main_fn.stmts[1] as ast.ExprStmt
	assert sql_stmt.expr is ast.SqlExpr
}

fn test_arm64_large_struct_call_is_lowered_to_call_sret() {
	mut ssa_mod := ssa.Module.new('abi_test')
	i64_t := ssa_mod.type_store.get_int(64)
	i8_t := ssa_mod.type_store.get_int(8)
	ptr_t := ssa_mod.type_store.get_ptr(i8_t)
	large_struct_t := ssa_mod.type_store.register(ssa.Type{
		kind:   .struct_t
		fields: [ptr_t, i64_t, i64_t] // 24 bytes
	})

	callee_id := ssa_mod.new_function('callee', large_struct_t, [])
	callee_entry := ssa_mod.add_block(callee_id, 'entry')
	zero := ssa_mod.get_or_add_const(i64_t, '0')
	ssa_mod.add_instr(.ret, callee_entry, 0, [zero])

	caller_id := ssa_mod.new_function('caller', i64_t, [])
	caller_entry := ssa_mod.add_block(caller_id, 'entry')
	fn_val := ssa_mod.add_value_node(.unknown, 0, 'callee', 0)
	call_val := ssa_mod.add_instr(.call, caller_entry, large_struct_t, [fn_val])
	ssa_mod.add_instr(.ret, caller_entry, 0, [zero])

	mut mir_mod := mir.lower_from_ssa(ssa_mod)
	lower(mut mir_mod, .arm64)

	call_instr := mir_mod.instrs[mir_mod.values[call_val].index]
	assert call_instr.op == .call_sret
}

fn test_x64_large_struct_call_is_lowered_to_call_sret() {
	mut ssa_mod := ssa.Module.new('abi_test_x64')
	i64_t := ssa_mod.type_store.get_int(64)
	i8_t := ssa_mod.type_store.get_int(8)
	ptr_t := ssa_mod.type_store.get_ptr(i8_t)
	large_struct_t := ssa_mod.type_store.register(ssa.Type{
		kind:   .struct_t
		fields: [ptr_t, i64_t, i64_t]
	})

	callee_id := ssa_mod.new_function('callee', large_struct_t, [])
	callee_entry := ssa_mod.add_block(callee_id, 'entry')
	zero := ssa_mod.get_or_add_const(i64_t, '0')
	ssa_mod.add_instr(.ret, callee_entry, 0, [zero])

	caller_id := ssa_mod.new_function('caller', i64_t, [])
	caller_entry := ssa_mod.add_block(caller_id, 'entry')
	fn_val := ssa_mod.add_value_node(.unknown, 0, 'callee', 0)
	call_val := ssa_mod.add_instr(.call, caller_entry, large_struct_t, [fn_val])
	ssa_mod.add_instr(.ret, caller_entry, 0, [zero])

	mut mir_mod := mir.lower_from_ssa(ssa_mod)
	lower(mut mir_mod, .x64)

	call_instr := mir_mod.instrs[mir_mod.values[call_val].index]
	assert call_instr.op == .call_sret
}

fn test_arm64_external_large_struct_call_is_lowered_to_call_sret() {
	mut ssa_mod := ssa.Module.new('abi_test_external')
	i64_t := ssa_mod.type_store.get_int(64)
	i8_t := ssa_mod.type_store.get_int(8)
	ptr_t := ssa_mod.type_store.get_ptr(i8_t)
	large_struct_t := ssa_mod.type_store.register(ssa.Type{
		kind:   .struct_t
		fields: [ptr_t, i64_t, i64_t]
	})

	caller_id := ssa_mod.new_function('caller', i64_t, [])
	caller_entry := ssa_mod.add_block(caller_id, 'entry')
	external_fn := ssa_mod.add_value_node(.unknown, 0, 'external_make', 0)
	call_val := ssa_mod.add_instr(.call, caller_entry, large_struct_t, [external_fn])
	zero := ssa_mod.get_or_add_const(i64_t, '0')
	ssa_mod.add_instr(.ret, caller_entry, 0, [zero])

	mut mir_mod := mir.lower_from_ssa(ssa_mod)
	lower(mut mir_mod, .arm64)

	call_instr := mir_mod.instrs[mir_mod.values[call_val].index]
	assert call_instr.op == .call_sret
}

fn test_arm64_indirect_call_large_struct_return_is_lowered_to_call_sret() {
	mut ssa_mod := ssa.Module.new('abi_test_indirect')
	i64_t := ssa_mod.type_store.get_int(64)
	i8_t := ssa_mod.type_store.get_int(8)
	ptr_t := ssa_mod.type_store.get_ptr(i8_t)
	large_struct_t := ssa_mod.type_store.register(ssa.Type{
		kind:   .struct_t
		fields: [ptr_t, i64_t, i64_t]
	})
	fn_sig_t := ssa_mod.type_store.register(ssa.Type{
		kind:     .func_t
		params:   []ssa.TypeID{}
		ret_type: large_struct_t
	})
	fn_ptr_t := ssa_mod.type_store.get_ptr(fn_sig_t)

	caller_id := ssa_mod.new_function('caller', i64_t, [])
	fn_ptr := ssa_mod.add_value_node(.argument, fn_ptr_t, 'fn_ptr', 0)
	ssa_mod.funcs[caller_id].params << fn_ptr
	caller_entry := ssa_mod.add_block(caller_id, 'entry')
	call_val := ssa_mod.add_instr(.call_indirect, caller_entry, large_struct_t, [
		fn_ptr,
	])
	zero := ssa_mod.get_or_add_const(i64_t, '0')
	ssa_mod.add_instr(.ret, caller_entry, 0, [zero])

	mut mir_mod := mir.lower_from_ssa(ssa_mod)
	lower(mut mir_mod, .arm64)

	call_instr := mir_mod.instrs[mir_mod.values[call_val].index]
	assert call_instr.op == .call_sret
}

fn test_arm64_callsite_marks_large_struct_arg_indirect() {
	mut ssa_mod := ssa.Module.new('abi_test_arg_class')
	i64_t := ssa_mod.type_store.get_int(64)
	i8_t := ssa_mod.type_store.get_int(8)
	ptr_t := ssa_mod.type_store.get_ptr(i8_t)
	large_struct_t := ssa_mod.type_store.register(ssa.Type{
		kind:   .struct_t
		fields: [ptr_t, i64_t, i64_t]
	})

	callee_id := ssa_mod.new_function('callee', i64_t, [])
	callee_param := ssa_mod.add_value_node(.argument, large_struct_t, 'arg', 0)
	ssa_mod.funcs[callee_id].params << callee_param
	callee_entry := ssa_mod.add_block(callee_id, 'entry')
	zero := ssa_mod.get_or_add_const(i64_t, '0')
	ssa_mod.add_instr(.ret, callee_entry, 0, [zero])

	caller_id := ssa_mod.new_function('caller', i64_t, [])
	caller_param := ssa_mod.add_value_node(.argument, large_struct_t, 'in', 0)
	ssa_mod.funcs[caller_id].params << caller_param
	caller_entry := ssa_mod.add_block(caller_id, 'entry')
	fn_val := ssa_mod.add_value_node(.unknown, 0, 'callee', 0)
	call_val := ssa_mod.add_instr(.call, caller_entry, i64_t, [fn_val, caller_param])
	ssa_mod.add_instr(.ret, caller_entry, 0, [zero])

	mut mir_mod := mir.lower_from_ssa(ssa_mod)
	lower(mut mir_mod, .arm64)

	call_instr := mir_mod.instrs[mir_mod.values[call_val].index]
	assert call_instr.abi_arg_class.len == 1
	assert call_instr.abi_arg_class[0] == .indirect
}

fn test_arm64_callsite_marks_large_array_arg_indirect() {
	mut ssa_mod := ssa.Module.new('abi_test_array_arg_class')
	i64_t := ssa_mod.type_store.get_int(64)
	large_array_t := ssa_mod.type_store.register(ssa.Type{
		kind:      .array_t
		elem_type: i64_t
		len:       3
	})

	callee_id := ssa_mod.new_function('callee', i64_t, [])
	callee_param := ssa_mod.add_value_node(.argument, large_array_t, 'arg', 0)
	ssa_mod.funcs[callee_id].params << callee_param
	callee_entry := ssa_mod.add_block(callee_id, 'entry')
	zero := ssa_mod.get_or_add_const(i64_t, '0')
	ssa_mod.add_instr(.ret, callee_entry, 0, [zero])

	caller_id := ssa_mod.new_function('caller', i64_t, [])
	caller_param := ssa_mod.add_value_node(.argument, large_array_t, 'in', 0)
	ssa_mod.funcs[caller_id].params << caller_param
	caller_entry := ssa_mod.add_block(caller_id, 'entry')
	fn_val := ssa_mod.add_value_node(.unknown, 0, 'callee', 0)
	call_val := ssa_mod.add_instr(.call, caller_entry, i64_t, [fn_val, caller_param])
	ssa_mod.add_instr(.ret, caller_entry, 0, [zero])

	mut mir_mod := mir.lower_from_ssa(ssa_mod)
	lower(mut mir_mod, .arm64)

	call_instr := mir_mod.instrs[mir_mod.values[call_val].index]
	assert call_instr.abi_arg_class.len == 1
	assert call_instr.abi_arg_class[0] == .indirect
}

fn test_arm64_unresolved_callsite_marks_large_array_alloca_arg_indirect() {
	mut ssa_mod := ssa.Module.new('abi_test_array_alloca_arg_class')
	i64_t := ssa_mod.type_store.get_int(64)
	large_array_t := ssa_mod.type_store.register(ssa.Type{
		kind:      .array_t
		elem_type: i64_t
		len:       3
	})
	large_array_ptr_t := ssa_mod.type_store.get_ptr(large_array_t)

	caller_id := ssa_mod.new_function('caller', i64_t, [])
	caller_entry := ssa_mod.add_block(caller_id, 'entry')
	tmp := ssa_mod.add_instr(.alloca, caller_entry, large_array_ptr_t, []ssa.ValueID{})
	external_fn := ssa_mod.add_value_node(.unknown, 0, 'external_append_like', 0)
	call_val := ssa_mod.add_instr(.call, caller_entry, i64_t, [external_fn, tmp])
	zero := ssa_mod.get_or_add_const(i64_t, '0')
	ssa_mod.add_instr(.ret, caller_entry, 0, [zero])

	mut mir_mod := mir.lower_from_ssa(ssa_mod)
	lower(mut mir_mod, .arm64)

	call_instr := mir_mod.instrs[mir_mod.values[call_val].index]
	assert call_instr.abi_arg_class.len == 1
	assert call_instr.abi_arg_class[0] == .indirect
}
