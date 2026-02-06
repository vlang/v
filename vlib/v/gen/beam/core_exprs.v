module beam

import v.ast
import v.token
import strings

// core_expr dispatches expression generation for Core Erlang.
// All output is inline (no newlines) - the caller handles line breaks.
fn (mut g CoreGen) core_expr(node ast.Expr) {
	match node {
		ast.IntegerLiteral { g.core_integer_literal(node) }
		ast.FloatLiteral { g.core_float_literal(node) }
		ast.StringLiteral { g.core_string_literal(node) }
		ast.BoolLiteral { g.write_core(if node.val { "'true'" } else { "'false'" }) }
		ast.Ident { g.core_ident(node) }
		ast.CallExpr { g.core_call_expr(node) }
		ast.InfixExpr { g.core_infix_expr(node) }
		ast.StringInterLiteral { g.core_string_inter(node) }
		ast.SelectorExpr { g.core_selector_expr(node) }
		ast.ArrayInit { g.core_array_init(node) }
		ast.StructInit { g.core_struct_init(node) }
		ast.MapInit { g.core_map_init(node) }
		ast.IndexExpr { g.core_index_expr(node) }
		ast.ParExpr { g.core_expr(node.expr) }
		ast.PrefixExpr { g.core_prefix_expr(node) }
		ast.IfExpr { g.core_if_expr(node) }
		ast.MatchExpr { g.core_match_expr(node) }
		ast.EnumVal { g.core_enum_val(node) }
		else { g.write_core("'todo'") }
	}
}

fn (mut g CoreGen) core_integer_literal(node ast.IntegerLiteral) {
	val := node.val
	if val.len > 2 && val[0] == `0` && (val[1] == `x` || val[1] == `X`) {
		g.write_core('16#${val[2..]}')
	} else if val.len > 2 && val[0] == `0` && (val[1] == `o` || val[1] == `O`) {
		g.write_core('8#${val[2..]}')
	} else if val.len > 2 && val[0] == `0` && (val[1] == `b` || val[1] == `B`) {
		g.write_core('2#${val[2..]}')
	} else {
		g.write_core(val)
	}
}

fn (mut g CoreGen) core_float_literal(node ast.FloatLiteral) {
	val := node.val
	// Core Erlang requires a decimal point in float literals.
	// V may produce "1e+10" or "1e10" which needs "1.0e+10" or "1.0e10"
	if val.contains('e') || val.contains('E') {
		if !val.contains('.') {
			// Insert .0 before the exponent
			e_pos := if val.contains('e') { val.index('e') or { -1 } } else { val.index('E') or { -1 } }
			if e_pos > 0 {
				g.write_core('${val[..e_pos]}.0${val[e_pos..]}')
				return
			}
		}
	}
	// Ensure there's a decimal point
	if !val.contains('.') {
		g.write_core('${val}.0')
	} else {
		g.write_core(val)
	}
}

fn (mut g CoreGen) core_string_literal(node ast.StringLiteral) {
	// Core Erlang bitstring: #{#<72>(8,1,'integer',['unsigned'|['big']]),... }#
	g.write_core(core_bitstring(node.val))
}

fn (mut g CoreGen) core_ident(node ast.Ident) {
	// Handle constants by inlining
	if node.kind == .constant {
		if node.obj is ast.ConstField {
			g.core_expr(node.obj.expr)
			return
		}
	}
	// Look up Core Erlang variable name
	g.write_core(g.core_var(node.name))
}

fn (mut g CoreGen) core_call_expr(node ast.CallExpr) {
	if node.is_method {
		g.core_method_call(node)
		return
	}

	full_name := node.name
	if full_name == 'println' {
		g.core_println_call(node)
		return
	}

	call_mod := node.mod
	if call_mod != g.cur_mod && call_mod.len > 0 {
		// Cross-module: call 'v.mod':'fn'(args)
		erl_mod := g.core_v_mod_to_erl_mod(call_mod)
		short_name := full_name.all_after_last('.')
		g.write_core("call '${erl_mod}':'${short_name}'(")
	} else {
		// Same-module: apply 'fn'/arity(args)
		name := g.core_call_fn_name(full_name, call_mod)
		g.write_core("apply '${name}'/${node.args.len}(")
	}
	for i, arg in node.args {
		if i > 0 {
			g.write_core(', ')
		}
		g.core_expr(arg.expr)
	}
	g.write_core(')')
}

fn (mut g CoreGen) core_call_fn_name(full_name string, call_mod string) string {
	if call_mod == g.cur_mod || call_mod == '' {
		if full_name.contains('.') {
			parts := full_name.split('.')
			return parts[parts.len - 1]
		}
	}
	return full_name
}

fn (mut g CoreGen) core_method_call(node ast.CallExpr) {
	left_type := node.left_type

	if int(left_type) == 0 {
		g.write_core("apply 'unknown.${node.name}'/${node.args.len + 1}(")
		g.core_expr(node.left)
		for arg in node.args {
			g.write_core(', ')
			g.core_expr(arg.expr)
		}
		g.write_core(')')
		return
	}

	type_sym := g.table.sym(left_type)

	// Handle .str() on primitive types
	if node.name == 'str' && node.args.len == 0 {
		type_name := type_sym.name
		is_int := type_sym.kind == .int || type_sym.kind == .i8 || type_sym.kind == .i16 ||
			type_sym.kind == .i32 || type_sym.kind == .i64 || type_sym.kind == .u8 ||
			type_sym.kind == .u16 || type_sym.kind == .u32 || type_sym.kind == .u64 ||
			type_sym.kind == .int_literal || type_name == 'int' || type_name == 'i32' ||
			type_name == 'i64' || type_name == 'u32' || type_name == 'u64'
		is_float := type_sym.kind == .f32 || type_sym.kind == .f64 ||
			type_sym.kind == .float_literal || type_name == 'f32' || type_name == 'f64'
		is_bool := type_sym.kind == .bool || type_name == 'bool'

		if is_int {
			g.write_core("call 'erlang':'integer_to_binary'(")
			g.core_expr(node.left)
			g.write_core(')')
			return
		} else if is_float {
			g.write_core("call 'erlang':'float_to_binary'(")
			g.core_expr(node.left)
			g.write_core(')')
			return
		} else if is_bool {
			g.write_core("call 'erlang':'atom_to_binary'(")
			g.core_expr(node.left)
			g.write_core(')')
			return
		}
	}

	short_type := type_sym.name.all_after_last('.')
	arity := node.args.len + 1
	g.write_core("apply '${short_type}.${node.name}'/${arity}(")
	g.core_expr(node.left)
	for arg in node.args {
		g.write_core(', ')
		g.core_expr(arg.expr)
	}
	g.write_core(')')
}

fn (mut g CoreGen) core_println_call(node ast.CallExpr) {
	if node.args.len == 0 {
		// println() -> io:format("~n")
		g.write_core("call 'io':'format'(${core_charlist('~n')})")
		return
	}

	arg := node.args[0]

	if arg.expr is ast.StringLiteral {
		// String literal: io:format("message~n") as single charlist arg
		g.write_core("call 'io':'format'(${core_charlist(arg.expr.val + '~n')})")
	} else if arg.expr is ast.StringInterLiteral {
		// String interpolation: build binary, then print with ~s~n
		fmt := core_charlist('~s~n')
		g.write_core("call 'io':'format'(${fmt}, [")
		g.core_expr(arg.expr)
		g.write_core('|[]])')
	} else {
		// General expression: io:format("~s~n", [Expr])
		// For numeric types, convert to binary first
		arg_type := arg.typ
		fmt := core_charlist('~s~n')

		if int(arg_type) != 0 {
			type_sym := g.table.sym(arg_type)
			if g.core_is_numeric_type(type_sym) {
				g.write_core("call 'io':'format'(${fmt}, [")
				g.core_to_binary_expr(arg.expr, arg_type)
				g.write_core('|[]])')
				return
			}
		}

		g.write_core("call 'io':'format'(${fmt}, [")
		g.core_expr(arg.expr)
		g.write_core('|[]])')
	}
}

fn (mut g CoreGen) core_is_numeric_type(sym ast.TypeSymbol) bool {
	return sym.kind == .int || sym.kind == .i8 || sym.kind == .i16 ||
		sym.kind == .i32 || sym.kind == .i64 || sym.kind == .u8 ||
		sym.kind == .u16 || sym.kind == .u32 || sym.kind == .u64 ||
		sym.kind == .int_literal || sym.kind == .f32 || sym.kind == .f64 ||
		sym.kind == .float_literal || sym.name == 'int' || sym.name == 'i32' ||
		sym.name == 'f64'
}

fn (mut g CoreGen) core_to_binary_expr(expr ast.Expr, typ ast.Type) {
	if int(typ) == 0 {
		g.core_expr(expr)
		return
	}
	type_sym := g.table.sym(typ)
	type_name := type_sym.name

	is_int := type_sym.kind == .int || type_sym.kind == .i8 || type_sym.kind == .i16 ||
		type_sym.kind == .i32 || type_sym.kind == .i64 || type_sym.kind == .u8 ||
		type_sym.kind == .u16 || type_sym.kind == .u32 || type_sym.kind == .u64 ||
		type_sym.kind == .int_literal || type_name == 'int' || type_name == 'i32'
	is_float := type_sym.kind == .f32 || type_sym.kind == .f64 ||
		type_sym.kind == .float_literal || type_name == 'f32' || type_name == 'f64'

	if is_int {
		g.write_core("call 'erlang':'integer_to_binary'(")
		g.core_expr(expr)
		g.write_core(')')
	} else if is_float {
		g.write_core("call 'erlang':'float_to_binary'(")
		g.core_expr(expr)
		g.write_core(')')
	} else if type_sym.kind == .bool || type_name == 'bool' {
		g.write_core("call 'erlang':'atom_to_binary'(")
		g.core_expr(expr)
		g.write_core(')')
	} else {
		g.core_expr(expr)
	}
}

fn (mut g CoreGen) core_infix_expr(node ast.InfixExpr) {
	// String concatenation: <<A/binary, B/binary>> via iolist_to_binary
	if node.op == .plus {
		left_is_string := g.core_is_string_expr(node.left, node.left_type)
		right_is_string := g.core_is_string_expr(node.right, node.right_type)
		if left_is_string && right_is_string {
			g.write_core("call 'erlang':'iolist_to_binary'([")
			g.core_expr(node.left)
			g.write_core('|[')
			g.core_expr(node.right)
			g.write_core('|[]]])')
			return
		}
	}

	// Integer division: V's / with ints -> erlang:div
	if node.op == .div {
		left_is_int := g.core_is_int_expr(node.left, node.left_type)
		right_is_int := g.core_is_int_expr(node.right, node.right_type)
		if left_is_int && right_is_int {
			g.write_core("call 'erlang':'div'(")
			g.core_expr(node.left)
			g.write_core(', ')
			g.core_expr(node.right)
			g.write_core(')')
			return
		}
	}

	// 'in' operator: lists:member
	if node.op == .key_in {
		g.write_core("call 'lists':'member'(")
		g.core_expr(node.left)
		g.write_core(', ')
		g.core_expr(node.right)
		g.write_core(')')
		return
	}

	// 'not in' operator
	if node.op == .not_in {
		g.write_core("call 'erlang':'not'(call 'lists':'member'(")
		g.core_expr(node.left)
		g.write_core(', ')
		g.core_expr(node.right)
		g.write_core('))')
		return
	}

	// All other operators -> call 'erlang':'OP'(Left, Right)
	op_str := core_op(node.op)
	g.write_core("call 'erlang':'${op_str}'(")
	g.core_expr(node.left)
	g.write_core(', ')
	g.core_expr(node.right)
	g.write_core(')')
}

fn (g CoreGen) core_is_string_expr(expr ast.Expr, typ ast.Type) bool {
	if expr is ast.StringLiteral || expr is ast.StringInterLiteral {
		return true
	}
	if int(typ) != 0 {
		type_sym := g.table.sym(typ)
		return type_sym.kind == .string || type_sym.name == 'string'
	}
	return false
}

fn (g CoreGen) core_is_int_expr(expr ast.Expr, typ ast.Type) bool {
	if expr is ast.IntegerLiteral {
		return true
	}
	if expr is ast.FloatLiteral {
		return false
	}
	if int(typ) != 0 {
		type_sym := g.table.sym(typ)
		return type_sym.kind == .int || type_sym.kind == .i8 || type_sym.kind == .i16 ||
			type_sym.kind == .i32 || type_sym.kind == .i64 || type_sym.kind == .u8 ||
			type_sym.kind == .u16 || type_sym.kind == .u32 || type_sym.kind == .u64 ||
			type_sym.kind == .int_literal
	}
	return false
}

fn (mut g CoreGen) core_prefix_expr(node ast.PrefixExpr) {
	match node.op {
		.not {
			g.write_core("call 'erlang':'not'(")
			g.core_expr(node.right)
			g.write_core(')')
		}
		.minus {
			g.write_core("call 'erlang':'-'(")
			g.core_expr(node.right)
			g.write_core(')')
		}
		.bit_not {
			g.write_core("call 'erlang':'bnot'(")
			g.core_expr(node.right)
			g.write_core(')')
		}
		.amp {
			// Address-of has no meaning on BEAM - just output value
			g.core_expr(node.right)
		}
		else {
			g.core_expr(node.right)
		}
	}
}

fn (mut g CoreGen) core_string_inter(node ast.StringInterLiteral) {
	if node.vals.len == 0 && node.exprs.len == 0 {
		g.write_core(core_bitstring(''))
		return
	}

	// Single expression with no surrounding text
	if node.exprs.len == 1 && node.expr_types.len >= 1 && node.vals.len == 2
		&& node.vals[0].len == 0 && node.vals[1].len == 0 {
		g.core_to_binary_expr(node.exprs[0], node.expr_types[0])
		return
	}

	// Build iolist and convert to binary
	// call 'erlang':'iolist_to_binary'([part1|[part2|[...|[]]]])
	mut parts := []string{}

	for i, val in node.vals {
		if val.len > 0 {
			parts << core_bitstring(val)
		}
		if i < node.exprs.len && i < node.expr_types.len {
			mut expr_buf := strings.new_builder(64)
			old_out := g.out
			g.out = expr_buf
			g.core_to_binary_expr(node.exprs[i], node.expr_types[i])
			parts << g.out.str()
			g.out = old_out
		}
	}

	if parts.len == 0 {
		g.write_core(core_bitstring(''))
	} else if parts.len == 1 {
		g.write_core(parts[0])
	} else {
		// Build cons list: [p1|[p2|[...|[]]]]
		g.write_core("call 'erlang':'iolist_to_binary'(")
		g.core_write_cons_list(parts)
		g.write_core(')')
	}
}

// core_write_cons_list writes a proper cons list from string parts
fn (mut g CoreGen) core_write_cons_list(parts []string) {
	if parts.len == 0 {
		g.write_core('[]')
		return
	}
	for part in parts {
		g.write_core('[${part}|')
	}
	g.write_core('[]')
	for _ in parts {
		g.write_core(']')
	}
}

fn (mut g CoreGen) core_selector_expr(node ast.SelectorExpr) {
	field := node.field_name

	if field == 'len' {
		expr_type := node.expr_type
		if int(expr_type) != 0 {
			type_sym := g.table.sym(expr_type)
			if type_sym.kind == .map || type_sym.name.starts_with('map[') {
				g.write_core("call 'maps':'size'(")
				g.core_expr(node.expr)
				g.write_core(')')
				return
			}
		}
		g.write_core("call 'erlang':'length'(")
		g.core_expr(node.expr)
		g.write_core(')')
		return
	}

	// Field access: maps:get(field, Obj)
	g.write_core("call 'erlang':'map_get'('${field}', ")
	g.core_expr(node.expr)
	g.write_core(')')
}

fn (mut g CoreGen) core_array_init(node ast.ArrayInit) {
	if node.exprs.len == 0 {
		g.write_core('[]')
		return
	}
	// Build cons list: [e1|[e2|[...|[]]]]
	for i, expr in node.exprs {
		_ = i
		g.write_core('[')
		g.core_expr(expr)
		g.write_core('|')
	}
	g.write_core('[]')
	for _ in node.exprs {
		g.write_core(']')
	}
}

fn (mut g CoreGen) core_map_init(node ast.MapInit) {
	// Core Erlang map: ~{key1=>val1, key2=>val2}~
	g.write_core('~{')
	for i, key in node.keys {
		if i > 0 {
			g.write_core(',')
		}
		g.core_expr(key)
		g.write_core('=>')
		g.core_expr(node.vals[i])
	}
	g.write_core('}~')
}

fn (mut g CoreGen) core_struct_init(node ast.StructInit) {
	// Struct as map with type tag
	type_sym := g.table.sym(node.typ)
	type_name := type_sym.name.all_after_last('.')

	g.write_core('~{')
	for i, field in node.init_fields {
		if i > 0 {
			g.write_core(',')
		}
		g.write_core("'${field.name}'=>")
		g.core_expr(field.expr)
	}
	if node.init_fields.len > 0 {
		g.write_core(',')
	}
	g.write_core("{'vbeam','type'}=>'${type_name}'")
	g.write_core('}~')
}

fn (mut g CoreGen) core_index_expr(node ast.IndexExpr) {
	left_type := node.left_type

	if int(left_type) != 0 {
		type_sym := g.table.sym(left_type)
		if type_sym.kind == .map || type_sym.name.starts_with('map[') {
			g.write_core("call 'erlang':'map_get'(")
			g.core_expr(node.index)
			g.write_core(', ')
			g.core_expr(node.left)
			g.write_core(')')
			return
		}
	}

	// Array access: lists:nth(I + 1, Arr)
	g.write_core("call 'lists':'nth'(")
	if node.index is ast.IntegerLiteral {
		idx := node.index.val.int() + 1
		g.write_core('${idx}')
	} else {
		g.write_core("call 'erlang':'+'(")
		g.core_expr(node.index)
		g.write_core(', 1)')
	}
	g.write_core(', ')
	g.core_expr(node.left)
	g.write_core(')')
}

fn (mut g CoreGen) core_if_expr(node ast.IfExpr) {
	if node.is_comptime {
		// TODO: comptime if
		g.write_core("'ok'")
		return
	}
	g.core_if_branches(node.branches, 0)
}

fn (mut g CoreGen) core_if_branches(branches []ast.IfBranch, idx int) {
	if idx >= branches.len {
		g.write_core("'ok'")
		return
	}

	branch := branches[idx]
	is_last := idx == branches.len - 1
	is_else := is_last && (branch.cond is ast.NodeError || branch.cond is ast.EmptyExpr)

	if is_else {
		g.core_branch_value(branch)
	} else {
		// case COND of
		//     <'true'> when 'true' -> TRUE_BODY
		//     <'false'> when 'true' -> FALSE_BODY
		// end
		g.write_core('case ')
		g.core_expr(branch.cond)
		g.write_core(" of <'true'> when 'true' -> ")
		g.core_branch_value(branch)
		g.write_core(" <'false'> when 'true' -> ")
		if idx + 1 < branches.len {
			g.core_if_branches(branches, idx + 1)
		} else {
			g.write_core("'ok'")
		}
		g.write_core(' end')
	}
}

fn (mut g CoreGen) core_branch_value(branch ast.IfBranch) {
	if branch.stmts.len == 0 {
		g.write_core("'ok'")
		return
	}

	if branch.stmts.len == 1 {
		stmt := branch.stmts[0]
		match stmt {
			ast.Return {
				if stmt.exprs.len > 0 {
					g.core_expr(stmt.exprs[0])
				} else {
					g.write_core("'ok'")
				}
			}
			ast.ExprStmt {
				g.core_expr(stmt.expr)
			}
			else {
				g.write_core("'ok'")
			}
		}
		return
	}

	// Multiple statements - need do/let chain inline
	// For now, output the last statement value
	last := branch.stmts[branch.stmts.len - 1]
	match last {
		ast.Return {
			if last.exprs.len > 0 {
				g.core_expr(last.exprs[0])
			} else {
				g.write_core("'ok'")
			}
		}
		ast.ExprStmt {
			g.core_expr(last.expr)
		}
		else {
			g.write_core("'ok'")
		}
	}
}

fn (mut g CoreGen) core_match_expr(node ast.MatchExpr) {
	// match true { ... } -> case with guards
	if node.cond is ast.BoolLiteral && node.cond.val {
		g.core_match_true_as_case(node)
		return
	}

	g.write_core('case ')
	g.core_expr(node.cond)
	g.write_core(' of ')

	for i, branch in node.branches {
		if i > 0 {
			g.write_core(' ')
		}
		// Pattern
		g.write_core('<')
		if branch.is_else {
			g.write_core('_')
		} else {
			for j, pattern in branch.exprs {
				if j > 0 {
					g.write_core('> when ')
				}
				g.core_match_pattern(pattern)
			}
		}
		g.write_core("> when 'true' -> ")
		g.core_match_branch_val(branch.stmts)
	}
	g.write_core(' end')
}

fn (mut g CoreGen) core_match_true_as_case(node ast.MatchExpr) {
	// Generate nested case on 'true' for each branch
	// case COND1 of <'true'> -> BODY1 <'false'> -> (case COND2 of ...)
	for i, branch in node.branches {
		if branch.is_else {
			g.core_match_branch_val(branch.stmts)
		} else if branch.exprs.len > 0 {
			g.write_core('case ')
			g.core_expr(branch.exprs[0])
			g.write_core(" of <'true'> when 'true' -> ")
			g.core_match_branch_val(branch.stmts)
			g.write_core(" <'false'> when 'true' -> ")
			if i + 1 >= node.branches.len {
				g.write_core("'ok'")
			}
			// The next iteration will fill in the false branch
		}
	}
	// Close all the case expressions
	for i, branch in node.branches {
		_ = i
		if !branch.is_else && branch.exprs.len > 0 {
			g.write_core(' end')
		}
	}
}

fn (mut g CoreGen) core_match_pattern(expr ast.Expr) {
	match expr {
		ast.EnumVal {
			g.write_core("'${expr.val}'")
		}
		ast.Ident {
			if expr.name == '_' {
				g.write_core('_')
			} else {
				g.write_core(g.core_var(expr.name))
			}
		}
		else {
			g.core_expr(expr)
		}
	}
}

fn (mut g CoreGen) core_match_branch_val(stmts []ast.Stmt) {
	if stmts.len == 0 {
		g.write_core("'ok'")
		return
	}
	last := stmts[stmts.len - 1]
	match last {
		ast.Return {
			if last.exprs.len > 0 {
				g.core_expr(last.exprs[0])
			} else {
				g.write_core("'ok'")
			}
		}
		ast.ExprStmt {
			g.core_expr(last.expr)
		}
		else {
			g.write_core("'ok'")
		}
	}
}

fn (mut g CoreGen) core_enum_val(node ast.EnumVal) {
	g.write_core("'${node.val}'")
}
