// Native IR generator: V AST → Erlang term IR → vbeam_native → native binary.
// Supports: integer arithmetic, floats, strings, arrays, structs, function calls,
// for loops, if/else, method calls, and type conversions.
// Activated via VBEAM_TARGET=arm64|x86_64 environment variable.
module beam

import v.ast
import strings
import os

struct LoopLabels {
	continue_lbl string
	break_lbl    string
}

// StructLayout caches field names and byte offsets for a user-defined struct.
struct StructLayout {
	field_names   []string
	field_offsets []int // byte offset of each field (8-byte slots)
	total_size    int   // total bytes (fields * 8)
}

struct IRGen {
	table  &ast.Table
	target NativeTarget
mut:
	functions      []string // Erlang term text per function
	data_entries   []string // Erlang term text per data entry
	vreg_counter   int
	var_regs       map[string]int // V var name → vreg#
	var_types      map[string]ast.Type // V var name → type (for dispatch)
	label_counter  int
	cur_fn_instrs  []string // body instructions for current function
	string_counter int
	loop_stack     []LoopLabels // for break/continue
	struct_layouts map[int]StructLayout // type idx → layout cache
}

// Top-level: generate IR module, write to file, invoke vbeam_native.
fn native_gen(files []&ast.File, table ast.Table, out_name string, target NativeTarget) {
	mut g := IRGen{
		table: &table
		target: target
	}

	out_dir := if out_name.len > 0 { out_name } else { 'native_output' }
	if !os.exists(out_dir) {
		os.mkdir_all(out_dir) or {}
	}

	ir_text := g.gen_module(files)

	ir_path := os.join_path(out_dir, '.vbeam_native.ir')
	os.write_file(ir_path, ir_text) or {
		eprintln('Error writing IR: ${err}')
		return
	}

	compile_native(ir_path, out_dir)
}

fn (mut g IRGen) gen_module(files []&ast.File) string {
	// Only process the main module — skip builtins and stdlib
	for file in files {
		if file.mod.name != 'main' {
			continue
		}
		for stmt in file.stmts {
			if stmt is ast.FnDecl {
				g.gen_function(stmt)
			}
		}
	}

	mut out := strings.new_builder(4096)
	out.writeln('#{')
	out.writeln('    target => ${g.target_atom()},')
	out.writeln('    format => ${g.format_atom()},')
	out.writeln('    functions => [')
	for i, f in g.functions {
		out.write_string(f)
		if i < g.functions.len - 1 {
			out.writeln(',')
		} else {
			out.writeln('')
		}
	}
	out.writeln('    ],')
	out.writeln('    data => [')
	for i, d in g.data_entries {
		out.write_string('        ${d}')
		if i < g.data_entries.len - 1 {
			out.writeln(',')
		} else {
			out.writeln('')
		}
	}
	out.writeln('    ],')
	out.writeln('    bss => [],')
	out.writeln('    imports => []')
	out.writeln('}.')
	return out.str()
}

fn (mut g IRGen) gen_function(node ast.FnDecl) {
	g.vreg_counter = 0
	g.var_regs.clear()
	g.label_counter = 0
	g.cur_fn_instrs.clear()

	name := if node.short_name == 'main' || node.is_main {
		'main'
	} else {
		node.short_name
	}
	arity := node.params.len
	is_exported := node.is_pub || name == 'main'

	mut params := []string{}
	for param in node.params {
		vreg := g.next_vreg()
		g.var_regs[param.name] = vreg
		params << '{vreg, ${vreg}}'
	}

	for stmt in node.stmts {
		g.gen_stmt(stmt)
	}

	// main gets an exit(0) epilogue
	if name == 'main' {
		g.emit_exit(0)
	}

	mut fn_out := strings.new_builder(512)
	fn_out.writeln('        #{')
	fn_out.writeln('            name => <<"${name}">>,')
	fn_out.writeln('            arity => ${arity},')
	fn_out.writeln('            exported => ${is_exported},')
	fn_out.writeln('            params => [${params.join(', ')}],')
	fn_out.writeln('            locals => ${g.vreg_counter},')
	fn_out.writeln('            body => [')
	// Join instructions with commas — no trailing comma
	for i, instr in g.cur_fn_instrs {
		fn_out.write_string('                ${instr}')
		if i < g.cur_fn_instrs.len - 1 {
			fn_out.writeln(',')
		} else {
			fn_out.writeln('')
		}
	}
	fn_out.writeln('            ]')
	fn_out.write_string('        }')
	g.functions << fn_out.str()
}

// --- Statement generation ---

fn (mut g IRGen) gen_stmt(node ast.Stmt) {
	match node {
		ast.ExprStmt { g.gen_expr_stmt(node) }
		ast.AssignStmt { g.gen_assign(node) }
		ast.Return { g.gen_return(node) }
		ast.ForCStmt { g.gen_for_c(node) }
		ast.ForStmt { g.gen_for(node) }
		ast.ForInStmt { g.gen_for_in(node) }
		ast.BranchStmt {
			if g.loop_stack.len > 0 {
				top := g.loop_stack.last()
				if node.kind == .key_break {
					g.emit('{jmp, <<"${top.break_lbl}">>}')
				} else if node.kind == .key_continue {
					g.emit('{jmp, <<"${top.continue_lbl}">>}')
				}
			}
		}
		else {}
	}
}

fn (mut g IRGen) gen_if_stmt(node ast.IfExpr) {
	end_lbl := g.next_label('if_end')

	for i, branch in node.branches {
		is_last := i == node.branches.len - 1
		is_else_branch := is_last && node.has_else && branch.cond is ast.EmptyExpr

		if is_else_branch {
			// Final else: no condition check, just body
			for stmt in branch.stmts {
				g.gen_stmt(stmt)
			}
		} else {
			// Conditional branch: if cond false → skip to next_lbl
			next_lbl := if is_last { end_lbl } else { g.next_label('elif') }
			g.gen_branch_cond(branch.cond, next_lbl)

			for stmt in branch.stmts {
				g.gen_stmt(stmt)
			}

			if !is_last {
				g.emit('{jmp, <<"${end_lbl}">>}')
				g.emit('{label, <<"${next_lbl}">>}')
			}
		}
	}

	g.emit('{label, <<"${end_lbl}">>}')
}

fn (mut g IRGen) gen_expr_stmt(node ast.ExprStmt) {
	if node.expr is ast.CallExpr {
		call := node.expr
		if call.name == 'println' && call.args.len == 1 {
			g.gen_println(call.args[0].expr)
			return
		}
	}
	if node.expr is ast.PostfixExpr {
		g.gen_postfix(node.expr)
		return
	}
	if node.expr is ast.IfExpr {
		g.gen_if_stmt(node.expr)
		return
	}
	g.gen_expr(node.expr)
}

fn (mut g IRGen) gen_assign(node ast.AssignStmt) {
	// Multi-return: left has more identifiers than right expressions.
	// e.g., `a, b := some_call()` or `a, b = some_call()`
	// Evaluate the single RHS once; first LHS gets the return value,
	// subsequent LHS vars get zero (our IR has no multi-return decomposition).
	if node.left.len > 1 && node.right.len == 1 {
		call_vreg := g.gen_expr(node.right[0])
		for i, left in node.left {
			if left is ast.Ident {
				if left.name == '_' {
					continue
				}
				if i == 0 {
					g.var_regs[left.name] = call_vreg
				} else {
					vreg := g.next_vreg()
					g.emit('{mov_imm, {vreg, ${vreg}}, 0}')
					g.var_regs[left.name] = vreg
				}
				// Track type for later dispatch
				if i < node.left_types.len {
					g.var_types[left.name] = node.left_types[i]
				}
			}
		}
		return
	}
	for i, left in node.left {
		if left is ast.Ident {
			if node.op == .decl_assign {
				// := declaration: evaluate RHS, assign vreg
				vreg := g.gen_expr(node.right[i])
				g.var_regs[left.name] = vreg
				// Track type for later dispatch (string ops, array ops, etc.)
				if i < node.right_types.len {
					g.var_types[left.name] = node.right_types[i]
				} else if i < node.left_types.len {
					g.var_types[left.name] = node.left_types[i]
				}
			} else if node.op == .assign {
				// = assignment
				rhs := g.gen_expr(node.right[i])
				if existing := g.var_regs[left.name] {
					if existing != rhs {
						g.emit('{mov, {vreg, ${existing}}, {vreg, ${rhs}}}')
					}
				}
			} else if node.op == .plus_assign {
				if existing := g.var_regs[left.name] {
					// Check if this is string concatenation
					typ := g.var_types[left.name] or { ast.int_type }
					if typ.is_string() {
						rhs := g.gen_expr(node.right[i])
						dst := g.next_vreg()
						g.emit('{string_concat, {vreg, ${dst}}, {vreg, ${existing}}, {vreg, ${rhs}}}')
						g.emit('{mov, {vreg, ${existing}}, {vreg, ${dst}}}')
					} else {
						rhs := g.gen_expr(node.right[i])
						g.emit('{add, {vreg, ${existing}}, {vreg, ${existing}}, {vreg, ${rhs}}}')
					}
				}
			} else if node.op == .minus_assign {
				if existing := g.var_regs[left.name] {
					rhs := g.gen_expr(node.right[i])
					g.emit('{sub, {vreg, ${existing}}, {vreg, ${existing}}, {vreg, ${rhs}}}')
				}
			} else if node.op == .mult_assign {
				if existing := g.var_regs[left.name] {
					rhs := g.gen_expr(node.right[i])
					g.emit('{mul, {vreg, ${existing}}, {vreg, ${existing}}, {vreg, ${rhs}}}')
				}
			} else if node.op == .div_assign {
				if existing := g.var_regs[left.name] {
					rhs := g.gen_expr(node.right[i])
					g.emit('{sdiv, {vreg, ${existing}}, {vreg, ${existing}}, {vreg, ${rhs}}}')
				}
			} else if node.op == .mod_assign {
				if existing := g.var_regs[left.name] {
					rhs := g.gen_expr(node.right[i])
					g.emit('{srem, {vreg, ${existing}}, {vreg, ${existing}}, {vreg, ${rhs}}}')
				}
			}
		} else if left is ast.IndexExpr {
			// array[idx] = val  or  map[key] = val
			g.gen_index_assign(left, node.right[i])
		} else if left is ast.SelectorExpr {
			// struct.field = val
			g.gen_selector_assign(left, node.right[i])
		}
	}
}

// gen_index_assign handles  arr[idx] = val  or  map[key] = val
fn (mut g IRGen) gen_index_assign(idx_expr ast.IndexExpr, rhs ast.Expr) {
	if idx_expr.is_map {
		// Map index assignment: map[key] = val → map_put
		map_vreg := g.gen_expr(idx_expr.left)
		key_vreg := g.gen_expr(idx_expr.index)
		val_vreg := g.gen_expr(rhs)
		result := g.next_vreg()
		g.emit('{map_put, {vreg, ${result}}, {vreg, ${map_vreg}}, {vreg, ${key_vreg}}, {vreg, ${val_vreg}}}')
		g.emit('{mov, {vreg, ${map_vreg}}, {vreg, ${result}}}')
	} else {
		// Array index assignment
		arr_vreg := g.gen_expr(idx_expr.left)
		idx_vreg := g.gen_expr(idx_expr.index)
		val_vreg := g.gen_expr(rhs)
		g.emit('{array_set, {vreg, ${arr_vreg}}, {vreg, ${idx_vreg}}, {vreg, ${val_vreg}}, {imm, 8}}')
	}
}

// gen_selector_assign handles  s.field = val
fn (mut g IRGen) gen_selector_assign(sel ast.SelectorExpr, rhs ast.Expr) {
	struct_vreg := g.gen_expr(sel.expr)
	val_vreg := g.gen_expr(rhs)
	offset := g.field_offset(sel.expr_type, sel.field_name)
	g.emit('{field_set, {vreg, ${struct_vreg}}, {imm, ${offset}}, {vreg, ${val_vreg}}}')
}

fn (mut g IRGen) gen_return(node ast.Return) {
	if node.exprs.len > 0 {
		vreg := g.gen_expr(node.exprs[0])
		g.emit('{mov, {preg, ${g.ret_reg()}}, {vreg, ${vreg}}}')
	}
	g.emit('ret')
}

fn (mut g IRGen) gen_for_c(node ast.ForCStmt) {
	if node.has_init {
		g.gen_stmt(node.init)
	}
	loop_lbl := g.next_label('for')
	inc_lbl := g.next_label('for_inc')
	end_lbl := g.next_label('for_end')

	g.loop_stack << LoopLabels{
		continue_lbl: inc_lbl
		break_lbl: end_lbl
	}

	g.emit('{label, <<"${loop_lbl}">>}')

	if node.has_cond {
		g.gen_branch_cond(node.cond, end_lbl)
	}

	for stmt in node.stmts {
		g.gen_stmt(stmt)
	}

	g.emit('{label, <<"${inc_lbl}">>}')
	if node.has_inc {
		g.gen_stmt(node.inc)
	}

	g.emit('{jmp, <<"${loop_lbl}">>}')
	g.emit('{label, <<"${end_lbl}">>}')
	g.loop_stack.delete_last()
}

fn (mut g IRGen) gen_for(node ast.ForStmt) {
	loop_lbl := g.next_label('loop')
	end_lbl := g.next_label('loop_end')

	g.loop_stack << LoopLabels{
		continue_lbl: loop_lbl
		break_lbl: end_lbl
	}

	g.emit('{label, <<"${loop_lbl}">>}')

	if node.cond !is ast.EmptyExpr {
		g.gen_branch_cond(node.cond, end_lbl)
	}

	for stmt in node.stmts {
		g.gen_stmt(stmt)
	}

	g.emit('{jmp, <<"${loop_lbl}">>}')
	g.emit('{label, <<"${end_lbl}">>}')
	g.loop_stack.delete_last()
}

// gen_for_in handles `for i in 0..10 { }` (range) and `for elem in arr { }` (array).
fn (mut g IRGen) gen_for_in(node ast.ForInStmt) {
	if node.is_range {
		g.gen_for_in_range(node)
	} else if node.kind == .array {
		g.gen_for_in_array(node)
	} else if node.kind == .map {
		g.gen_for_in_map(node)
	} else {
		// Unsupported for-in kind — skip silently
	}
}

// gen_for_in_range handles `for i in low..high { body }`.
// Lowers to: i = low; loop: if i >= high goto end; body; i++; goto loop; end:
fn (mut g IRGen) gen_for_in_range(node ast.ForInStmt) {
	// Evaluate low bound (cond) and high bound
	low_vreg := g.gen_expr(node.cond)
	high_vreg := g.gen_expr(node.high)

	// Allocate the iteration variable
	iter_vreg := g.next_vreg()
	g.emit('{mov, {vreg, ${iter_vreg}}, {vreg, ${low_vreg}}}')
	g.var_regs[node.val_var] = iter_vreg
	g.var_types[node.val_var] = ast.int_type

	loop_lbl := g.next_label('forin')
	inc_lbl := g.next_label('forin_inc')
	end_lbl := g.next_label('forin_end')

	g.loop_stack << LoopLabels{
		continue_lbl: inc_lbl
		break_lbl: end_lbl
	}

	g.emit('{label, <<"${loop_lbl}">>}')
	// if iter >= high, exit
	g.emit('{cmp, {vreg, ${iter_vreg}}, {vreg, ${high_vreg}}}')
	g.emit('{jcc, ge, <<"${end_lbl}">>}')

	// Body
	for stmt in node.stmts {
		g.gen_stmt(stmt)
	}

	// Increment
	g.emit('{label, <<"${inc_lbl}">>}')
	g.emit('{add, {vreg, ${iter_vreg}}, {vreg, ${iter_vreg}}, {imm, 1}}')
	g.emit('{jmp, <<"${loop_lbl}">>}')
	g.emit('{label, <<"${end_lbl}">>}')
	g.loop_stack.delete_last()
}

// gen_for_in_array handles `for elem in arr { body }` and `for i, elem in arr { body }`.
// Lowers to: idx = 0; len = array_len(arr); loop: if idx >= len goto end; elem = arr[idx]; body; idx++; goto loop; end:
fn (mut g IRGen) gen_for_in_array(node ast.ForInStmt) {
	arr_vreg := g.gen_expr(node.cond)

	// Get array length
	len_vreg := g.next_vreg()
	g.emit('{array_len, {vreg, ${len_vreg}}, {vreg, ${arr_vreg}}}')

	// Index counter
	idx_vreg := g.next_vreg()
	g.emit('{mov_imm, {vreg, ${idx_vreg}}, 0}')

	// If there's a key_var (index variable), register it
	if node.key_var.len > 0 {
		g.var_regs[node.key_var] = idx_vreg
		g.var_types[node.key_var] = ast.int_type
	}

	loop_lbl := g.next_label('forin_arr')
	inc_lbl := g.next_label('forin_arr_inc')
	end_lbl := g.next_label('forin_arr_end')

	g.loop_stack << LoopLabels{
		continue_lbl: inc_lbl
		break_lbl: end_lbl
	}

	g.emit('{label, <<"${loop_lbl}">>}')
	g.emit('{cmp, {vreg, ${idx_vreg}}, {vreg, ${len_vreg}}}')
	g.emit('{jcc, ge, <<"${end_lbl}">>}')

	// Load element: val = arr[idx]
	val_vreg := g.next_vreg()
	g.emit('{array_get, {vreg, ${val_vreg}}, {vreg, ${arr_vreg}}, {vreg, ${idx_vreg}}, {imm, 8}}')
	g.var_regs[node.val_var] = val_vreg
	if int(node.val_type) != 0 {
		g.var_types[node.val_var] = node.val_type
	}

	// Body
	for stmt in node.stmts {
		g.gen_stmt(stmt)
	}

	// Increment index
	g.emit('{label, <<"${inc_lbl}">>}')
	g.emit('{add, {vreg, ${idx_vreg}}, {vreg, ${idx_vreg}}, {imm, 1}}')
	g.emit('{jmp, <<"${loop_lbl}">>}')
	g.emit('{label, <<"${end_lbl}">>}')
	g.loop_stack.delete_last()
}

// gen_for_in_map handles `for key, val in mymap { body }`.
// Maps are not iterable at the native IR level yet — emit a stub comment.
fn (mut g IRGen) gen_for_in_map(node ast.ForInStmt) {
	// Maps need a map_keys or map_iter opcode which doesn't exist yet.
	// For now, emit a comment so compilation doesn't silently drop the loop.
	g.emit('{comment, <<"for-in map not yet supported">>}')
}

fn (mut g IRGen) gen_branch_cond(cond ast.Expr, end_label string) {
	// gen_branch_cond: if cond is FALSE, jump to end_label.
	if cond is ast.ParExpr {
		g.gen_branch_cond(cond.expr, end_label)
		return
	}
	if cond is ast.InfixExpr {
		// Short-circuit AND: if A is false, skip; if B is false, skip.
		if cond.op == .and {
			g.gen_branch_cond(cond.left, end_label)
			g.gen_branch_cond(cond.right, end_label)
			return
		}
		// Short-circuit OR: if A is true, enter body; else check B.
		if cond.op == .logical_or {
			body_lbl := g.next_label('or_ok')
			g.gen_branch_cond_true(cond.left, body_lbl)
			g.gen_branch_cond(cond.right, end_label)
			g.emit('{label, <<"${body_lbl}">>}')
			return
		}
		left_vreg := g.gen_expr(cond.left)
		// Invert condition: jump to end if NOT true
		inv := match cond.op {
			.lt { 'ge' }
			.le { 'gt' }
			.gt { 'le' }
			.ge { 'lt' }
			.eq { 'ne' }
			.ne { 'eq' }
			else { 'eq' }
		}
		if cond.right is ast.IntegerLiteral {
			imm_val := cond.right.val.i64()
			g.emit('{cmp, {vreg, ${left_vreg}}, {imm, ${imm_val}}}')
		} else {
			right_vreg := g.gen_expr(cond.right)
			g.emit('{cmp, {vreg, ${left_vreg}}, {vreg, ${right_vreg}}}')
		}
		g.emit('{jcc, ${inv}, <<"${end_label}">>}')
	} else if cond is ast.PrefixExpr && cond.op == .not {
		// !expr: if expr is TRUE, jump to end_label
		g.gen_branch_cond_true(cond.right, end_label)
	}
}

// gen_branch_cond_true: if cond is TRUE, jump to target_label.
fn (mut g IRGen) gen_branch_cond_true(cond ast.Expr, target_label string) {
	if cond is ast.ParExpr {
		g.gen_branch_cond_true(cond.expr, target_label)
		return
	}
	if cond is ast.InfixExpr {
		// Short-circuit AND for true-jump: both must be true.
		if cond.op == .and {
			skip_lbl := g.next_label('and_skip')
			g.gen_branch_cond(cond.left, skip_lbl)
			g.gen_branch_cond_true(cond.right, target_label)
			g.emit('{label, <<"${skip_lbl}">>}')
			return
		}
		// Short-circuit OR for true-jump: either true suffices.
		if cond.op == .logical_or {
			g.gen_branch_cond_true(cond.left, target_label)
			g.gen_branch_cond_true(cond.right, target_label)
			return
		}
		left_vreg := g.gen_expr(cond.left)
		// NON-inverted: jump if condition IS true
		cc := match cond.op {
			.lt { 'lt' }
			.le { 'le' }
			.gt { 'gt' }
			.ge { 'ge' }
			.eq { 'eq' }
			.ne { 'ne' }
			else { 'ne' }
		}
		if cond.right is ast.IntegerLiteral {
			imm_val := cond.right.val.i64()
			g.emit('{cmp, {vreg, ${left_vreg}}, {imm, ${imm_val}}}')
		} else {
			right_vreg := g.gen_expr(cond.right)
			g.emit('{cmp, {vreg, ${left_vreg}}, {vreg, ${right_vreg}}}')
		}
		g.emit('{jcc, ${cc}, <<"${target_label}">>}')
	}
}

fn (mut g IRGen) gen_postfix(node ast.PostfixExpr) {
	if node.expr is ast.Ident {
		if vreg := g.var_regs[node.expr.name] {
			op := if node.op == .inc { 'add' } else { 'sub' }
			g.emit('{${op}, {vreg, ${vreg}}, {vreg, ${vreg}}, {imm, 1}}')
		}
	}
}

fn (mut g IRGen) gen_println(expr ast.Expr) {
	fd_r, buf_r, len_r, num_r, write_n, _ := g.syscall_regs()
	if expr is ast.StringInterLiteral {
		// String interpolation: emit each piece directly using print_str/print_int/print_float.
		// This avoids needing int_to_str/float_to_str pseudo-ops.
		g.gen_println_interpolation(expr)
		return
	}
	if expr is ast.StringLiteral {
		data_name := g.add_string_data(expr.val + '\n')
		g.emit('{lea, {preg, ${buf_r}}, {data_ref, <<"${data_name}">>}}')
		g.emit('{mov_imm, {preg, ${fd_r}}, 1}')
		g.emit('{mov_imm, {preg, ${len_r}}, ${expr.val.len + 1}}')
		g.emit('{mov_imm, {preg, ${num_r}}, ${write_n}}')
		g.emit('syscall')
	} else if g.expr_is_string(expr) {
		// String variable or expression: concatenate with newline, then print_str.
		// We concat with "\n" to produce a single fat pointer, avoiding
		// raw syscall register clobber on the original string vreg.
		str_vreg := g.gen_expr(expr)
		nl_vreg := g.next_vreg()
		nl_data := g.add_string_data('\n')
		g.emit('{string_lit, {vreg, ${nl_vreg}}, {data_ref, <<"${nl_data}">>}, 1}')
		concat_vreg := g.next_vreg()
		g.emit('{string_concat, {vreg, ${concat_vreg}}, {vreg, ${str_vreg}}, {vreg, ${nl_vreg}}}')
		g.emit('{print_str, {vreg, ${concat_vreg}}}')
	} else if g.expr_is_float(expr) {
		// Float expression: use print_float pseudo-op.
		vreg := g.gen_expr(expr)
		g.emit('{print_float, {vreg, ${vreg}}}')
	} else {
		// Non-string: evaluate expression and use print_int pseudo-op.
		// Pass the vreg directly — print_int accepts any register and
		// preserves all other registers, so no need to force into x0.
		vreg := g.gen_expr(expr)
		g.emit('{print_int, {vreg, ${vreg}}}')
	}
}

// gen_println_interpolation handles println('text ${expr} text') by emitting
// a sequence of print_str / print_int / print_float calls for each piece.
fn (mut g IRGen) gen_println_interpolation(node ast.StringInterLiteral) {
	// Print each literal part and expression part in sequence
	for i, expr in node.exprs {
		// Print the literal part before this expression
		if i < node.vals.len && node.vals[i].len > 0 {
			lit_vreg := g.gen_string_literal_raw(node.vals[i])
			g.emit('{print_str, {vreg, ${lit_vreg}}}')
		}
		// Print the expression value
		typ := if i < node.expr_types.len { node.expr_types[i] } else { ast.int_type }
		val_vreg := g.gen_expr(expr)
		if typ.is_string() {
			g.emit('{print_str, {vreg, ${val_vreg}}}')
		} else if typ.is_float() {
			g.emit('{print_float, {vreg, ${val_vreg}}}')
		} else {
			g.emit('{print_int, {vreg, ${val_vreg}}}')
		}
	}
	// Print the final literal part (after the last expression) + newline
	last_idx := node.exprs.len
	if last_idx < node.vals.len && node.vals[last_idx].len > 0 {
		final_str := node.vals[last_idx] + '\n'
		lit_vreg := g.gen_string_literal_raw(final_str)
		g.emit('{print_str, {vreg, ${lit_vreg}}}')
	} else {
		// Just print newline
		nl_vreg := g.next_vreg()
		nl_data := g.add_string_data('\n')
		g.emit('{string_lit, {vreg, ${nl_vreg}}, {data_ref, <<"${nl_data}">>}, 1}')
		g.emit('{print_str, {vreg, ${nl_vreg}}}')
	}
}

// expr_is_string checks whether the given expression evaluates to a string type.
fn (g &IRGen) expr_is_string(expr ast.Expr) bool {
	if expr is ast.StringLiteral || expr is ast.StringInterLiteral {
		return true
	}
	if expr is ast.Ident {
		if typ := g.var_types[expr.name] {
			return typ.is_string()
		}
	}
	if expr is ast.CallExpr {
		return expr.return_type.is_string()
	}
	if expr is ast.InfixExpr {
		return expr.left_type.is_string()
	}
	if expr is ast.SelectorExpr {
		return expr.typ.is_string()
	}
	if expr is ast.MatchExpr {
		return expr.return_type.is_string()
	}
	if expr is ast.IfExpr {
		return expr.typ.is_string()
	}
	return false
}

// expr_is_float checks whether the given expression evaluates to a float type.
fn (g &IRGen) expr_is_float(expr ast.Expr) bool {
	if expr is ast.FloatLiteral {
		return true
	}
	if expr is ast.Ident {
		if typ := g.var_types[expr.name] {
			return typ.is_float()
		}
	}
	if expr is ast.CallExpr {
		return expr.return_type.is_float()
	}
	if expr is ast.InfixExpr {
		return expr.left_type.is_float() || expr.right_type.is_float()
	}
	if expr is ast.CastExpr {
		return expr.typ.is_float()
	}
	if expr is ast.SelectorExpr {
		return expr.typ.is_float()
	}
	return false
}

// --- Expression generation (returns vreg holding result) ---

fn (mut g IRGen) gen_expr(expr ast.Expr) int {
	match expr {
		ast.IntegerLiteral {
			vreg := g.next_vreg()
			// Convert V integer literals (0x..., 0o..., 0b...) to decimal
			// because Erlang's erl_scan rejects C-style prefix notation.
			int_val := expr.val.i64()
			g.emit('{mov_imm, {vreg, ${vreg}}, ${int_val}}')
			return vreg
		}
		ast.FloatLiteral {
			return g.gen_float_literal(expr)
		}
		ast.BoolLiteral {
			vreg := g.next_vreg()
			val := if expr.val { 1 } else { 0 }
			g.emit('{mov_imm, {vreg, ${vreg}}, ${val}}')
			return vreg
		}
		ast.StringLiteral {
			return g.gen_string_literal(expr)
		}
		ast.Ident {
			if vreg := g.var_regs[expr.name] {
				return vreg
			}
			return 0
		}
		ast.InfixExpr {
			return g.gen_infix(expr)
		}
		ast.PrefixExpr {
			return g.gen_prefix(expr)
		}
		ast.CallExpr {
			return g.gen_call(expr)
		}
		ast.ParExpr {
			return g.gen_expr(expr.expr)
		}
		ast.IfExpr {
			return g.gen_if_expr(expr)
		}
		ast.ArrayInit {
			return g.gen_array_init(expr)
		}
		ast.StructInit {
			return g.gen_struct_init(expr)
		}
		ast.SelectorExpr {
			return g.gen_selector_expr(expr)
		}
		ast.IndexExpr {
			return g.gen_index_expr(expr)
		}
		ast.CastExpr {
			return g.gen_cast(expr)
		}
		ast.MatchExpr {
			return g.gen_match_expr(expr)
		}
		ast.MapInit {
			return g.gen_map_init(expr)
		}
		ast.StringInterLiteral {
			return g.gen_string_inter(expr)
		}
		ast.DumpExpr {
			// dump(expr) in V just evaluates and returns the expression
			return g.gen_expr(expr.expr)
		}
		ast.CharLiteral {
			// Character literal: treat as integer (byte value)
			vreg := g.next_vreg()
			if expr.val.len > 0 {
				g.emit('{mov_imm, {vreg, ${vreg}}, ${int(expr.val[0])}}')
			} else {
				g.emit('{mov_imm, {vreg, ${vreg}}, 0}')
			}
			return vreg
		}
		ast.ConcatExpr {
			// Multi-return concatenation: evaluate last expression
			if expr.vals.len > 0 {
				return g.gen_expr(expr.vals.last())
			}
			return 0
		}
		else {
			return 0
		}
	}
}

fn (mut g IRGen) gen_infix(node ast.InfixExpr) int {
	// Array append: arr << val
	if node.op == .left_shift {
		left := g.gen_expr(node.left)
		right := g.gen_expr(node.right)
		result := g.next_vreg()
		g.emit('{array_append, {vreg, ${result}}, {vreg, ${left}}, {vreg, ${right}}, {imm, 8}}')
		return result
	}
	// Check if this is a string operation
	if node.left_type.is_string() {
		return g.gen_string_infix(node)
	}
	// Check if this is a float operation
	if node.left_type.is_float() || node.right_type.is_float() {
		return g.gen_float_infix(node)
	}

	left := g.gen_expr(node.left)
	op := match node.op {
		.plus { 'add' }
		.minus { 'sub' }
		.mul { 'mul' }
		.div { 'sdiv' }
		.mod { 'srem' }
		else { 'add' }
	}
	if node.right is ast.IntegerLiteral {
		result := g.next_vreg()
		imm_val := node.right.val.i64()
		g.emit('{${op}, {vreg, ${result}}, {vreg, ${left}}, {imm, ${imm_val}}}')
		return result
	}
	right := g.gen_expr(node.right)
	result := g.next_vreg()
	g.emit('{${op}, {vreg, ${result}}, {vreg, ${left}}, {vreg, ${right}}}')
	return result
}

// gen_string_infix handles string + string (concatenation) and == != < > comparisons.
fn (mut g IRGen) gen_string_infix(node ast.InfixExpr) int {
	left := g.gen_expr(node.left)
	right := g.gen_expr(node.right)
	if node.op == .plus {
		result := g.next_vreg()
		g.emit('{string_concat, {vreg, ${result}}, {vreg, ${left}}, {vreg, ${right}}}')
		return result
	}
	// String comparison: string_cmp returns -1/0/1
	cmp_result := g.next_vreg()
	g.emit('{string_cmp, {vreg, ${cmp_result}}, {vreg, ${left}}, {vreg, ${right}}}')
	result := g.next_vreg()
	// Compare the result with 0 and produce 0/1 boolean
	g.emit('{cmp, {vreg, ${cmp_result}}, {imm, 0}}')
	cc := match node.op {
		.eq { 'eq' }
		.ne { 'ne' }
		.lt { 'lt' }
		.le { 'le' }
		.gt { 'gt' }
		.ge { 'ge' }
		else { 'eq' }
	}
	// Set result to 1 if condition true, 0 otherwise
	set_lbl := g.next_label('scmp_set')
	end_lbl := g.next_label('scmp_end')
	g.emit('{jcc, ${cc}, <<"${set_lbl}">>}')
	g.emit('{mov_imm, {vreg, ${result}}, 0}')
	g.emit('{jmp, <<"${end_lbl}">>}')
	g.emit('{label, <<"${set_lbl}">>}')
	g.emit('{mov_imm, {vreg, ${result}}, 1}')
	g.emit('{label, <<"${end_lbl}">>}')
	return result
}

// gen_float_infix handles float arithmetic: +, -, *, /
fn (mut g IRGen) gen_float_infix(node ast.InfixExpr) int {
	left := g.gen_expr(node.left)
	right := g.gen_expr(node.right)
	result := g.next_vreg()
	fop := match node.op {
		.plus { 'fadd' }
		.minus { 'fsub' }
		.mul { 'fmul' }
		.div { 'fdiv' }
		else { 'fadd' }
	}
	g.emit('{${fop}, {vreg, ${result}}, {vreg, ${left}}, {vreg, ${right}}}')
	return result
}

fn (mut g IRGen) gen_call(node ast.CallExpr) int {
	// Dispatch method calls to specialized handlers
	if node.is_method {
		return g.gen_call_method(node)
	}

	param_regs := g.call_arg_regs()
	// Evaluate ALL arguments first, then move to physical regs.
	// This prevents the register allocator from reusing a vreg
	// that holds a previous argument's value.
	mut arg_vregs := []int{}
	for i, arg in node.args {
		if i < param_regs.len {
			vreg := g.gen_expr(arg.expr)
			arg_vregs << vreg
		}
	}
	for i, vreg in arg_vregs {
		g.emit('{mov, {preg, ${param_regs[i]}}, {vreg, ${vreg}}}')
	}
	// Strip module prefix from name (V uses "main.fib", we define as "fib")
	fn_name := if node.name.contains('.') { node.name.all_after_last('.') } else { node.name }
	g.emit('{call, {sym, <<"${fn_name}">>}}')
	result := g.next_vreg()
	g.emit('{mov, {vreg, ${result}}, {preg, ${g.ret_reg()}}}')
	return result
}

fn (mut g IRGen) gen_prefix(node ast.PrefixExpr) int {
	operand := g.gen_expr(node.right)
	result := g.next_vreg()
	match node.op {
		.minus {
			// -x: negate
			g.emit('{neg, {vreg, ${result}}, {vreg, ${operand}}}')
		}
		.not {
			// !x: logical not (XOR with 1 for booleans)
			g.emit('{xor_, {vreg, ${result}}, {vreg, ${operand}}, {imm, 1}}')
		}
		.bit_not {
			// ~x: bitwise not
			g.emit('{not_, {vreg, ${result}}, {vreg, ${operand}}}')
		}
		else {
			g.emit('{mov, {vreg, ${result}}, {vreg, ${operand}}}')
		}
	}
	return result
}

fn (mut g IRGen) gen_if_expr(node ast.IfExpr) int {
	// If expression that returns a value
	result := g.next_vreg()
	end_lbl := g.next_label('ife_end')

	for i, branch in node.branches {
		is_last := i == node.branches.len - 1
		is_else_branch := is_last && node.has_else && branch.cond is ast.EmptyExpr

		if is_else_branch {
			// else branch: last expr is the value
			if branch.stmts.len > 0 {
				for j, stmt in branch.stmts {
					if j == branch.stmts.len - 1 {
						if stmt is ast.ExprStmt {
							val := g.gen_expr(stmt.expr)
							g.emit('{mov, {vreg, ${result}}, {vreg, ${val}}}')
						} else {
							g.gen_stmt(stmt)
						}
					} else {
						g.gen_stmt(stmt)
					}
				}
			}
		} else {
			next_lbl := if is_last { end_lbl } else { g.next_label('ife') }
			g.gen_branch_cond(branch.cond, next_lbl)

			if branch.stmts.len > 0 {
				for j, stmt in branch.stmts {
					if j == branch.stmts.len - 1 {
						if stmt is ast.ExprStmt {
							val := g.gen_expr(stmt.expr)
							g.emit('{mov, {vreg, ${result}}, {vreg, ${val}}}')
						} else {
							g.gen_stmt(stmt)
						}
					} else {
						g.gen_stmt(stmt)
					}
				}
			}

			if !is_last {
				g.emit('{jmp, <<"${end_lbl}">>}')
				g.emit('{label, <<"${next_lbl}">>}')
			}
		}
	}

	g.emit('{label, <<"${end_lbl}">>}')
	return result
}

// --- Match expression generation ---

// gen_match_expr handles `match cond { val1 { body1 } val2 { body2 } else { body3 } }`.
// For `match true { cond1 { ... } cond2 { ... } else { ... } }`, acts like if/elif/else.
fn (mut g IRGen) gen_match_expr(node ast.MatchExpr) int {
	result := g.next_vreg()
	end_lbl := g.next_label('match_end')

	// Check if this is `match true { ... }` pattern (used as if/elif chain)
	is_match_true := node.cond is ast.BoolLiteral && (node.cond as ast.BoolLiteral).val

	if is_match_true {
		return g.gen_match_true_expr(node, result, end_lbl)
	}

	// General match: evaluate condition once, compare against each branch's values
	cond_vreg := g.gen_expr(node.cond)

	for i, branch in node.branches {
		is_last := i == node.branches.len - 1

		if branch.is_else {
			// else branch: execute body unconditionally
			g.gen_match_branch_body(branch.stmts, result)
		} else {
			next_lbl := if is_last { end_lbl } else { g.next_label('match_next') }

			// Compare against each expression in this branch (branches can have multiple values)
			if branch.exprs.len == 1 {
				// Single value comparison
				val_vreg := g.gen_expr(branch.exprs[0])
				g.emit('{cmp, {vreg, ${cond_vreg}}, {vreg, ${val_vreg}}}')
				g.emit('{jcc, ne, <<"${next_lbl}">>}')
			} else if branch.exprs.len > 1 {
				// Multiple values: if any matches, enter branch
				body_lbl := g.next_label('match_body')
				for expr in branch.exprs {
					val_vreg := g.gen_expr(expr)
					g.emit('{cmp, {vreg, ${cond_vreg}}, {vreg, ${val_vreg}}}')
					g.emit('{jcc, eq, <<"${body_lbl}">>}')
				}
				g.emit('{jmp, <<"${next_lbl}">>}')
				g.emit('{label, <<"${body_lbl}">>}')
			}

			g.gen_match_branch_body(branch.stmts, result)

			if !is_last {
				g.emit('{jmp, <<"${end_lbl}">>}')
				g.emit('{label, <<"${next_lbl}">>}')
			}
		}
	}

	g.emit('{label, <<"${end_lbl}">>}')
	return result
}

// gen_match_true_expr handles the `match true { cond1 { ... } cond2 { ... } else { ... } }` pattern.
// This is equivalent to an if/elif/else chain where each branch's expression is a condition.
fn (mut g IRGen) gen_match_true_expr(node ast.MatchExpr, result int, end_lbl string) int {
	for i, branch in node.branches {
		is_last := i == node.branches.len - 1

		if branch.is_else {
			g.gen_match_branch_body(branch.stmts, result)
		} else {
			next_lbl := if is_last { end_lbl } else { g.next_label('mt_next') }

			// Each branch expression is a condition (like in an if/elif chain)
			if branch.exprs.len > 0 {
				g.gen_branch_cond(branch.exprs[0], next_lbl)
			}

			g.gen_match_branch_body(branch.stmts, result)

			if !is_last {
				g.emit('{jmp, <<"${end_lbl}">>}')
				g.emit('{label, <<"${next_lbl}">>}')
			}
		}
	}

	g.emit('{label, <<"${end_lbl}">>}')
	return result
}

// gen_match_branch_body executes the body of a match branch, assigning the last
// expression's value to result_vreg.
fn (mut g IRGen) gen_match_branch_body(stmts []ast.Stmt, result_vreg int) {
	for i, stmt in stmts {
		if i == stmts.len - 1 {
			if stmt is ast.ExprStmt {
				val := g.gen_expr(stmt.expr)
				g.emit('{mov, {vreg, ${result_vreg}}, {vreg, ${val}}}')
			} else {
				g.gen_stmt(stmt)
			}
		} else {
			g.gen_stmt(stmt)
		}
	}
}

// --- String literal generation ---

// gen_string_literal creates a fat pointer {ptr, len} for a string value.
fn (mut g IRGen) gen_string_literal(node ast.StringLiteral) int {
	data_name := g.add_string_data(node.val)
	vreg := g.next_vreg()
	g.emit('{string_lit, {vreg, ${vreg}}, {data_ref, <<"${data_name}">>}, ${node.val.len}}')
	return vreg
}

// --- Float literal generation ---

// gen_float_literal encodes a float as its IEEE 754 bit pattern (integer).
fn (mut g IRGen) gen_float_literal(node ast.FloatLiteral) int {
	// Parse the float value and encode as u64 bit pattern.
	// The native backend stores floats as integer bit patterns.
	vreg := g.next_vreg()
	// Store the float string in the data section and use int_to_float at runtime.
	// For simplicity, we parse to integer representation.
	// V's FloatLiteral.val is a string like "3.14", "1.0", etc.
	fval := node.val.f64()
	bits := *unsafe { &u64(&fval) }
	g.emit('{mov_imm, {vreg, ${vreg}}, ${i64(bits)}}')
	return vreg
}

// --- Array generation ---

// gen_array_init handles `[1, 2, 3]` or `[]int{len: N, cap: N}`.
fn (mut g IRGen) gen_array_init(node ast.ArrayInit) int {
	elem_size := 8 // all elements are 8 bytes (pointers/ints/floats)
	init_cap := if node.exprs.len > 0 {
		node.exprs.len
	} else if node.has_cap {
		// For cap expressions that are integer literals, use that value
		if node.cap_expr is ast.IntegerLiteral {
			node.cap_expr.val.int()
		} else {
			8
		}
	} else if node.has_len {
		if node.len_expr is ast.IntegerLiteral {
			node.len_expr.val.int()
		} else {
			8
		}
	} else {
		4
	}
	arr_vreg := g.next_vreg()
	g.emit('{array_new, {vreg, ${arr_vreg}}, {imm, ${elem_size}}, {imm, ${init_cap}}}')

	// If there are initial elements, set them
	for i, elem_expr in node.exprs {
		val_vreg := g.gen_expr(elem_expr)
		g.emit('{array_set, {vreg, ${arr_vreg}}, {imm, ${i}}, {vreg, ${val_vreg}}, {imm, ${elem_size}}}')
	}

	// If elements were provided, update the length in the header
	if node.exprs.len > 0 {
		len_vreg := g.next_vreg()
		g.emit('{mov_imm, {vreg, ${len_vreg}}, ${node.exprs.len}}')
		// Store length directly: array header is at arr_vreg, len at offset 8
		g.emit('{store, {vreg, ${arr_vreg}}, 8, {vreg, ${len_vreg}}}')
	}
	return arr_vreg
}

// gen_index_expr handles `arr[idx]` or `map[key]` for read access.
fn (mut g IRGen) gen_index_expr(node ast.IndexExpr) int {
	if node.is_map {
		// Map index read: map[key] → map_get
		map_vreg := g.gen_expr(node.left)
		key_vreg := g.gen_expr(node.index)
		result := g.next_vreg()
		g.emit('{map_get, {vreg, ${result}}, {vreg, ${map_vreg}}, {vreg, ${key_vreg}}}')
		return result
	}
	arr_vreg := g.gen_expr(node.left)
	result := g.next_vreg()
	if node.index is ast.IntegerLiteral {
		idx_val := node.index.val.int()
		g.emit('{array_get, {vreg, ${result}}, {vreg, ${arr_vreg}}, {imm, ${idx_val}}, {imm, 8}}')
	} else {
		idx_vreg := g.gen_expr(node.index)
		g.emit('{array_get, {vreg, ${result}}, {vreg, ${arr_vreg}}, {vreg, ${idx_vreg}}, {imm, 8}}')
	}
	return result
}

// --- Struct generation ---

// gen_struct_init handles `Foo{field1: val1, field2: val2}`.
fn (mut g IRGen) gen_struct_init(node ast.StructInit) int {
	layout := g.get_struct_layout(node.typ)
	struct_vreg := g.next_vreg()
	g.emit('{struct_new, {vreg, ${struct_vreg}}, {imm, ${layout.total_size}}}')

	for field in node.init_fields {
		val_vreg := g.gen_expr(field.expr)
		offset := g.field_name_offset(layout, field.name)
		g.emit('{field_set, {vreg, ${struct_vreg}}, {imm, ${offset}}, {vreg, ${val_vreg}}}')
	}
	return struct_vreg
}

// gen_selector_expr handles `s.field` for read access and `.len` on strings/arrays.
fn (mut g IRGen) gen_selector_expr(node ast.SelectorExpr) int {
	// Check for .len on strings
	if node.field_name == 'len' {
		// Determine whether it's a string or array
		expr_vreg := g.gen_expr(node.expr)
		result := g.next_vreg()
		if node.expr_type.is_string() {
			g.emit('{string_len, {vreg, ${result}}, {vreg, ${expr_vreg}}}')
		} else {
			// array.len
			g.emit('{array_len, {vreg, ${result}}, {vreg, ${expr_vreg}}}')
		}
		return result
	}

	// Struct field access
	struct_vreg := g.gen_expr(node.expr)
	result := g.next_vreg()
	offset := g.field_offset(node.expr_type, node.field_name)
	g.emit('{field_get, {vreg, ${result}}, {vreg, ${struct_vreg}}, {imm, ${offset}}}')
	return result
}

// --- Type cast generation ---

// gen_cast handles type casts like `f64(x)` or `int(x)`.
fn (mut g IRGen) gen_cast(node ast.CastExpr) int {
	src_vreg := g.gen_expr(node.expr)
	// int → float
	if node.typ.is_float() && !node.expr_type.is_float() {
		result := g.next_vreg()
		g.emit('{int_to_float, {vreg, ${result}}, {vreg, ${src_vreg}}}')
		return result
	}
	// float → int
	if !node.typ.is_float() && node.expr_type.is_float() {
		result := g.next_vreg()
		g.emit('{float_to_int, {vreg, ${result}}, {vreg, ${src_vreg}}}')
		return result
	}
	// Same-kind cast: no-op
	return src_vreg
}

// --- Struct layout helpers ---

// get_struct_layout returns the cached layout for a struct type.
fn (mut g IRGen) get_struct_layout(typ ast.Type) StructLayout {
	idx := typ.idx()
	if idx in g.struct_layouts {
		return g.struct_layouts[idx]
	}
	// Guard against void/invalid type index (0)
	if idx == 0 {
		layout := StructLayout{
			field_names: []string{}
			field_offsets: []int{}
			total_size: 8
		}
		g.struct_layouts[idx] = layout
		return layout
	}
	sym := g.table.sym(typ)
	mut names := []string{}
	mut offsets := []int{}
	if sym.info is ast.Struct {
		for i, field in sym.info.fields {
			names << field.name
			offsets << i * 8 // 8-byte slots
		}
	}
	total := names.len * 8
	if total == 0 {
		// minimum 8 bytes
		layout := StructLayout{
			field_names: names
			field_offsets: offsets
			total_size: 8
		}
		g.struct_layouts[idx] = layout
		return layout
	}
	layout := StructLayout{
		field_names: names
		field_offsets: offsets
		total_size: total
	}
	g.struct_layouts[idx] = layout
	return layout
}

// field_offset returns the byte offset for a named field in a struct type.
fn (mut g IRGen) field_offset(typ ast.Type, field_name string) int {
	layout := g.get_struct_layout(typ)
	return g.field_name_offset(layout, field_name)
}

// field_name_offset looks up a field name in a StructLayout and returns byte offset.
fn (g &IRGen) field_name_offset(layout StructLayout, field_name string) int {
	for i, name in layout.field_names {
		if name == field_name {
			return layout.field_offsets[i]
		}
	}
	return 0 // field not found — default to offset 0
}

// --- Expanded gen_expr_stmt for method calls ---

// gen_call expanded to handle method calls on strings/arrays
fn (mut g IRGen) gen_call_method(node ast.CallExpr) int {
	// Handle built-in method calls on known types
	if node.left_type.is_string() {
		return g.gen_string_method(node)
	}
	// Array methods
	if node.name == 'len' {
		obj_vreg := g.gen_expr(node.left)
		result := g.next_vreg()
		g.emit('{array_len, {vreg, ${result}}, {vreg, ${obj_vreg}}}')
		return result
	}
	// Fallback: generic method_call
	obj_vreg := g.gen_expr(node.left)
	mut arg_list := []string{}
	arg_list << '{vreg, ${obj_vreg}}'
	for arg in node.args {
		a_vreg := g.gen_expr(arg.expr)
		arg_list << '{vreg, ${a_vreg}}'
	}
	result := g.next_vreg()
	// Guard against void/invalid type index (0) which panics in table.sym()
	recv_type := if int(node.left_type) != 0 {
		g.table.sym(node.left_type).name
	} else {
		'unknown'
	}
	g.emit('{method_call, {vreg, ${result}}, <<"${recv_type}">>, <<"${node.name}">>, [${arg_list.join(', ')}]}')
	return result
}

// gen_string_method handles string methods like .len, .contains, etc.
fn (mut g IRGen) gen_string_method(node ast.CallExpr) int {
	obj_vreg := g.gen_expr(node.left)
	result := g.next_vreg()
	match node.name {
		'len' {
			g.emit('{string_len, {vreg, ${result}}, {vreg, ${obj_vreg}}}')
		}
		else {
			// Emit generic method call for other string methods
			mut arg_list := []string{}
			arg_list << '{vreg, ${obj_vreg}}'
			for arg in node.args {
				a_vreg := g.gen_expr(arg.expr)
				arg_list << '{vreg, ${a_vreg}}'
			}
			g.emit('{method_call, {vreg, ${result}}, <<"string">>, <<"${node.name}">>, [${arg_list.join(', ')}]}')
		}
	}
	return result
}

// --- Map generation ---

// gen_map_init handles `map[string]int{}` and `{'a': 1, 'b': 2}` map literals.
fn (mut g IRGen) gen_map_init(node ast.MapInit) int {
	map_vreg := g.next_vreg()
	g.emit('{map_new, {vreg, ${map_vreg}}}')

	// Insert initial key-value pairs
	for i, key_expr in node.keys {
		key_vreg := g.gen_expr(key_expr)
		val_vreg := g.gen_expr(node.vals[i])
		result := g.next_vreg()
		g.emit('{map_put, {vreg, ${result}}, {vreg, ${map_vreg}}, {vreg, ${key_vreg}}, {vreg, ${val_vreg}}}')
		// map_put returns the new map — update our reference
		g.emit('{mov, {vreg, ${map_vreg}}, {vreg, ${result}}}')
	}
	return map_vreg
}

// --- String interpolation ---

// gen_string_inter handles `'hello ${name}, you are ${age}'`.
// Builds the string by concatenating literal parts with stringified expressions.
fn (mut g IRGen) gen_string_inter(node ast.StringInterLiteral) int {
	// Start with the first literal part (may be empty string "")
	mut result_vreg := if node.vals.len > 0 && node.vals[0].len > 0 {
		g.gen_string_literal_raw(node.vals[0])
	} else {
		vreg := g.next_vreg()
		empty := g.add_string_data('')
		g.emit('{string_lit, {vreg, ${vreg}}, {data_ref, <<"${empty}">>}, 0}')
		vreg
	}

	// Interleave expressions and literal parts
	for i, expr in node.exprs {
		// Convert the expression to a string representation
		expr_str_vreg := g.gen_expr_to_string(expr, if i < node.expr_types.len {
			node.expr_types[i]
		} else {
			ast.int_type
		})
		// Concatenate
		concat_vreg := g.next_vreg()
		g.emit('{string_concat, {vreg, ${concat_vreg}}, {vreg, ${result_vreg}}, {vreg, ${expr_str_vreg}}}')
		result_vreg = concat_vreg

		// Append the next literal part (after the expression)
		lit_idx := i + 1
		if lit_idx < node.vals.len && node.vals[lit_idx].len > 0 {
			lit_vreg := g.gen_string_literal_raw(node.vals[lit_idx])
			cat_vreg := g.next_vreg()
			g.emit('{string_concat, {vreg, ${cat_vreg}}, {vreg, ${result_vreg}}, {vreg, ${lit_vreg}}}')
			result_vreg = cat_vreg
		}
	}
	return result_vreg
}

// gen_string_literal_raw creates a string literal vreg from a raw V string.
fn (mut g IRGen) gen_string_literal_raw(s string) int {
	data_name := g.add_string_data(s)
	vreg := g.next_vreg()
	g.emit('{string_lit, {vreg, ${vreg}}, {data_ref, <<"${data_name}">>}, ${s.len}}')
	return vreg
}

// gen_expr_to_string converts an expression to a string vreg.
// For strings, returns as-is. For non-strings, returns a placeholder
// since int_to_str/float_to_str are not yet implemented on the Erlang side.
// NOTE: println with interpolation is handled specially via gen_println_interpolation
// which uses print_int/print_float directly without string conversion.
fn (mut g IRGen) gen_expr_to_string(expr ast.Expr, typ ast.Type) int {
	if typ.is_string() {
		return g.gen_expr(expr)
	}
	// Non-string expression in string interpolation context:
	// We can't convert to string at IR level yet, so return a placeholder.
	// The println path handles this case directly via print_int/print_float.
	val_vreg := g.gen_expr(expr)
	// Return a "?" placeholder string — this is a best-effort fallback
	placeholder := g.gen_string_literal_raw('?')
	_ = val_vreg // evaluated for side effects
	return placeholder
}

// --- Helpers ---

fn (mut g IRGen) next_vreg() int {
	v := g.vreg_counter
	g.vreg_counter++
	return v
}

fn (mut g IRGen) next_label(prefix string) string {
	l := g.label_counter
	g.label_counter++
	return '${prefix}_${l}'
}

fn (mut g IRGen) emit(s string) {
	g.cur_fn_instrs << s
}

fn (mut g IRGen) add_string_data(s string) string {
	name := 'str_${g.string_counter}'
	g.string_counter++
	// Escape bytes for Erlang binary string syntax
	mut escaped := strings.new_builder(s.len + 8)
	for ch in s.bytes() {
		if ch == 0x0A {
			escaped.write_string('\\n')
		} else if ch == 0x0D {
			escaped.write_string('\\r')
		} else if ch == 0x09 {
			escaped.write_string('\\t')
		} else if ch == 0x5C {
			escaped.write_string('\\\\')
		} else if ch == 0x22 {
			escaped.write_string('\\"')
		} else {
			escaped.write_u8(ch)
		}
	}
	g.data_entries << '{<<"${name}">>, 1, <<"${escaped.str()}">>}'
	return name
}

fn (mut g IRGen) emit_exit(code int) {
	_, _, _, num_r, _, exit_n := g.syscall_regs()
	exit_r := match g.target {
		.arm64 { 'x0' }
		.x86_64 { 'rdi' }
		.none { '' }
	}
	g.emit('{mov_imm, {preg, ${exit_r}}, ${code}}')
	g.emit('{mov_imm, {preg, ${num_r}}, ${exit_n}}')
	g.emit('syscall')
}

fn (g &IRGen) syscall_regs() (string, string, string, string, int, int) {
	// (fd_reg, buf_reg, len_reg, num_reg, write_num, exit_num)
	return match g.target {
		.arm64 { 'x0', 'x1', 'x2', 'x16', 4, 1 }
		.x86_64 { 'rdi', 'rsi', 'rdx', 'rax', 1, 60 }
		.none { '', '', '', '', 0, 0 }
	}
}

fn (g &IRGen) ret_reg() string {
	return match g.target {
		.arm64 { 'x0' }
		.x86_64 { 'rax' }
		.none { '' }
	}
}

fn (g &IRGen) call_arg_regs() []string {
	return match g.target {
		.arm64 { ['x0', 'x1', 'x2', 'x3', 'x4', 'x5', 'x6', 'x7'] }
		.x86_64 { ['rdi', 'rsi', 'rdx', 'rcx', 'r8', 'r9'] }
		.none { []string{} }
	}
}

fn (g &IRGen) target_atom() string {
	return match g.target {
		.arm64 { 'arm64' }
		.x86_64 { 'x86_64' }
		.none { 'arm64' }
	}
}

fn (g &IRGen) format_atom() string {
	// VBEAM_FORMAT overrides default: pe, elf64, macho
	fmt := os.getenv('VBEAM_FORMAT')
	if fmt.len > 0 {
		return fmt
	}
	return match g.target {
		.arm64 { 'macho' }
		.x86_64 { 'elf64' }
		.none { 'macho' }
	}
}

// --- Native compilation invocation ---

fn compile_native(ir_path string, out_dir string) {
	// Find vbeam_native modules via VBEAM_NATIVE_DIR or compile from source
	native_dir := os.getenv('VBEAM_NATIVE_DIR')
	if native_dir.len == 0 {
		eprintln('VBEAM_NATIVE_DIR not set. Point it to directory with compiled vbeam_native_*.beam files.')
		eprintln('Example: cd vbeam_rt/src && erlc *.erl && export VBEAM_NATIVE_DIR=\$(pwd)')
		return
	}

	// Ensure modules are compiled
	beam_count := (os.glob('${native_dir}/vbeam_native*.beam') or { []string{} }).len
	if beam_count == 0 {
		erl_files := os.glob('${native_dir}/vbeam_native*.erl') or { []string{} }
		for erl_file in erl_files {
			r := os.execute('erlc -o "${native_dir}" "${erl_file}"')
			if r.exit_code != 0 {
				eprintln('erlc error: ${r.output}')
				return
			}
		}
	}

	out_path := os.join_path(out_dir, 'a.out')
	// Use -eval (not -run) because -run stops at '-o' thinking it's an erl flag
	cmd := 'erl -noshell -pa "${native_dir}" -eval \'vbeam_native:main(["${ir_path}", "-o", "${out_path}"])\' -s init stop'
	result := os.execute(cmd)
	if result.exit_code != 0 {
		eprintln('vbeam_native error: ${result.output}')
	} else if result.output.len > 0 {
		for line in result.output.split_into_lines() {
			if line.len > 0 {
				println(line)
			}
		}
	}
}
