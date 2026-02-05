module beam

import v.ast
import v.token
import strings

fn (mut g Gen) expr(node ast.Expr) {
	match node {
		ast.IntegerLiteral { g.write(node.val) }
		ast.FloatLiteral { g.write(node.val) }
		ast.StringLiteral { g.string_literal(node) }
		ast.BoolLiteral { g.write(if node.val { 'true' } else { 'false' }) }
		ast.Ident { g.ident(node) }
		ast.CallExpr { g.call_expr(node) }
		ast.InfixExpr { g.infix_expr(node) }
		ast.StringInterLiteral { g.string_inter(node) }
		ast.SelectorExpr { g.selector_expr(node) }
		ast.ArrayInit { g.array_init(node) }
		ast.StructInit { g.struct_init(node) }
		ast.MapInit { g.map_init(node) }
		ast.IndexExpr { g.index_expr(node) }
		ast.ParExpr { g.par_expr(node) }
		ast.PrefixExpr { g.prefix_expr(node) }
		ast.IfExpr { g.if_expr(node) }
		ast.MatchExpr { g.match_expr(node) }
		ast.EnumVal { g.enum_val(node) }
		ast.ComptimeSelector { g.comptime_selector(node) }
		else { g.write('todo') }
	}
}

fn (mut g Gen) string_literal(node ast.StringLiteral) {
	escaped := escape_erlang_string(node.val)
	g.write('<<"${escaped}">>')
}

fn (mut g Gen) ident(node ast.Ident) {
	// Check if this is a comptime loop variable
	if g.comptime_ident_expr(node) {
		return
	}

	// Handle constants by inlining their compile-time value
	if node.kind == .constant {
		if node.obj is ast.ConstField {
			// Inline the constant's expression value
			g.expr(node.obj.expr)
			return
		}
	}

	// Look up current SSA version
	name := node.name
	if ver := g.var_versions[name] {
		if ver == 0 {
			g.write(g.erl_var(name))
		} else {
			g.write(g.erl_var_versioned(name, ver))
		}
	} else {
		g.write(g.erl_var(name))
	}
}

fn (mut g Gen) call_expr(node ast.CallExpr) {
	if node.is_method {
		g.method_call(node)
		return
	}

	full_name := node.name
	if full_name == 'println' {
		g.println_call(node)
	} else {
		// Extract short name if in same module
		name := g.call_fn_name(full_name, node.mod)

		// Regular function call
		if g.needs_atom_quote(name) {
			g.write("'${name}'(")
		} else {
			g.write('${name}(')
		}
		for i, arg in node.args {
			if i > 0 {
				g.write(', ')
			}
			g.expr(arg.expr)
		}
		g.write(')')
	}
}

fn (mut g Gen) call_fn_name(full_name string, call_mod string) string {
	// If the function is from the current module, use short name
	// full_name is like 'main.add', call_mod is 'main'
	if call_mod == g.cur_mod || call_mod == '' {
		// Extract short name: 'main.add' -> 'add'
		if full_name.contains('.') {
			parts := full_name.split('.')
			return parts[parts.len - 1]
		}
	}
	return full_name
}

fn (mut g Gen) method_call(node ast.CallExpr) {
	// Method call: obj.method() -> 'Type.method'(Obj)
	left_type := node.left_type

	// Safety check for invalid types
	if int(left_type) == 0 {
		// Unknown type - fall back to simple function call style
		g.write("'unknown.${node.name}'(")
		g.expr(node.left)
		for arg in node.args {
			g.write(', ')
			g.expr(arg.expr)
		}
		g.write(')')
		return
	}

	type_sym := g.table.sym(left_type)

	// Handle .str() on primitive types -> Erlang BIF conversions
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
			g.write('integer_to_binary(')
			g.expr(node.left)
			g.write(')')
			return
		} else if is_float {
			g.write('float_to_binary(')
			g.expr(node.left)
			g.write(')')
			return
		} else if is_bool {
			g.write('atom_to_binary(')
			g.expr(node.left)
			g.write(')')
			return
		}
	}

	// Use short type name (without module prefix)
	short_type := type_sym.name.all_after_last('.')
	method_name := "'${short_type}.${node.name}'"

	g.write('${method_name}(')
	g.expr(node.left)
	for arg in node.args {
		g.write(', ')
		g.expr(arg.expr)
	}
	g.write(')')
}

fn (mut g Gen) println_call(node ast.CallExpr) {
	if node.args.len == 0 {
		g.write('io:format("~n", [])')
		return
	}

	arg := node.args[0]
	arg_type := arg.typ

	// Check if the argument is a string literal
	if arg.expr is ast.StringLiteral {
		g.write('io:format("~s~n", [')
		g.expr(arg.expr)
		g.write('])')
	} else if arg.expr is ast.StringInterLiteral {
		// String interpolation: build the binary inline
		g.write('vbeam_io:println(')
		g.expr(arg.expr)
		g.write(')')
	} else {
		// Check if type is valid before looking it up
		if int(arg_type) != 0 && g.is_string_type(arg_type) {
			// Already a string/binary
			g.write('vbeam_io:println(')
			g.expr(arg.expr)
			g.write(')')
		} else if int(arg_type) != 0 {
			// Need to convert to binary based on type
			g.write('vbeam_io:println(')
			g.to_binary_expr(arg.expr, arg_type)
			g.write(')')
		} else {
			// Type is unknown/void - just print directly
			g.write('vbeam_io:println(')
			g.expr(arg.expr)
			g.write(')')
		}
	}
}

fn (mut g Gen) infix_expr(node ast.InfixExpr) {
	// Check for string concatenation: string + string -> binary concatenation
	if node.op == .plus {
		// Check if both operands are strings
		left_is_string := g.is_string_type_or_literal(node.left, node.left_type)
		right_is_string := g.is_string_type_or_literal(node.right, node.right_type)

		if left_is_string && right_is_string {
			// Generate Erlang binary concatenation: <<A/binary, B/binary>>
			g.write('<<(')
			g.expr(node.left)
			g.write(')/binary, (')
			g.expr(node.right)
			g.write(')/binary>>')
			return
		}
	}

	// Default: output with translated operator
	g.expr(node.left)
	// Translate V operators to Erlang equivalents
	// Erlang uses =< for less-than-or-equal (not <=)
	// Erlang uses /= for not-equal (not !=)
	op_str := g.erlang_operator(node.op)
	g.write(' ${op_str} ')
	g.expr(node.right)
}

// is_string_type_or_literal checks if an expression is a string type or string literal
fn (g Gen) is_string_type_or_literal(expr ast.Expr, typ ast.Type) bool {
	// Check if it's a string literal
	if expr is ast.StringLiteral {
		return true
	}
	// Check if it's a string interpolation
	if expr is ast.StringInterLiteral {
		return true
	}
	// Check the type
	if int(typ) != 0 && g.is_string_type(typ) {
		return true
	}
	return false
}

// erlang_operator translates V operator tokens to Erlang equivalents
fn (g Gen) erlang_operator(op token.Kind) string {
	return match op {
		.le { '=<' }
		.ne { '/=' }
		.mod { 'rem' }
		else { op.str() }
	}
}

fn (mut g Gen) string_inter(node ast.StringInterLiteral) {
	// String interpolation: '${x}' -> converts each expr to binary and concatenates
	if node.vals.len == 0 && node.exprs.len == 0 {
		g.write('<<>>')
		return
	}

	// If it's a simple single expression with no surrounding text, just convert directly
	if node.exprs.len == 1 && node.expr_types.len >= 1 && node.vals.len == 2 && node.vals[0].len == 0
		&& node.vals[1].len == 0 {
		// Simple case: '${expr}' -> to_binary(expr)
		g.to_binary_expr(node.exprs[0], node.expr_types[0])
		return
	}

	// Complex case: multiple expressions or text mixed in
	mut parts := []string{}

	for i, val in node.vals {
		if val.len > 0 {
			escaped := escape_erlang_string(val)
			parts << '"${escaped}"'
		}
		if i < node.exprs.len && i < node.expr_types.len {
			// Get the expression type and convert appropriately
			expr_type := node.expr_types[i]
			mut expr_str := g.expr_to_string(node.exprs[i], expr_type)
			parts << '(${expr_str})/binary'
		}
	}

	if parts.len == 0 {
		g.write('<<>>')
	} else if parts.len == 1 && !parts[0].contains('/binary') {
		g.write('<<${parts[0]}>>')
	} else {
		g.write('<<${parts.join(', ')}>>')
	}
}

fn (mut g Gen) expr_to_string(expr ast.Expr, typ ast.Type) string {
	// Convert expression to binary representation
	mut out := strings.new_builder(64)
	old_out := g.out
	g.out = out

	g.to_binary_expr(expr, typ)

	result := g.out.str()
	g.out = old_out
	return result
}

fn (mut g Gen) to_binary_expr(expr ast.Expr, typ ast.Type) {
	// Safety check for invalid types
	if int(typ) == 0 {
		// Unknown type - output expression directly
		g.expr(expr)
		return
	}

	type_sym := g.table.sym(typ)

	// Check for built-in integer type names
	type_name := type_sym.name
	is_int := type_sym.kind == .int || type_sym.kind == .i8 || type_sym.kind == .i16 ||
		type_sym.kind == .i32 || type_sym.kind == .i64 || type_sym.kind == .u8 ||
		type_sym.kind == .u16 || type_sym.kind == .u32 || type_sym.kind == .u64 ||
		type_sym.kind == .int_literal || type_name == 'int' || type_name == 'i32' ||
		type_name == 'i64' || type_name == 'u32' || type_name == 'u64'

	is_float := type_sym.kind == .f32 || type_sym.kind == .f64 ||
		type_sym.kind == .float_literal || type_name == 'f32' || type_name == 'f64'

	// Special case: .len on arrays/strings returns int (type checker may not fill this in)
	if expr is ast.SelectorExpr && expr.field_name == 'len' {
		g.write('integer_to_binary(')
		g.expr(expr)
		g.write(')')
		return
	}

	if is_int {
		g.write('integer_to_binary(')
		g.expr(expr)
		g.write(')')
	} else if is_float {
		g.write('float_to_binary(')
		g.expr(expr)
		g.write(')')
	} else if type_sym.kind == .string || type_name == 'string' {
		g.expr(expr)
	} else if type_sym.kind == .bool || type_name == 'bool' {
		g.write('atom_to_binary(')
		g.expr(expr)
		g.write(')')
	} else {
		// Default: try to use the expression directly
		g.expr(expr)
	}
}

fn (mut g Gen) selector_expr(node ast.SelectorExpr) {
	// Check for comptime selector (field.name, method.typ, etc.)
	if g.comptime_selector_expr(node) {
		return
	}

	// Field access: p.x -> maps:get(x, P)
	field := node.field_name

	// Check for built-in properties
	if field == 'len' {
		// Check if this is a map type - use maps:size() instead of length()
		expr_type := node.expr_type
		if int(expr_type) != 0 {
			type_sym := g.table.sym(expr_type)
			if type_sym.kind == .map || type_sym.name.starts_with('map[') {
				g.write('maps:size(')
				g.expr(node.expr)
				g.write(')')
				return
			}
		}
		// Array or string - use length()
		g.write('length(')
		g.expr(node.expr)
		g.write(')')
		return
	}

	g.write("maps:get(${field}, ")
	g.expr(node.expr)
	g.write(')')
}

fn (mut g Gen) array_init(node ast.ArrayInit) {
	// Array literal: [1, 2, 3]
	g.write('[')
	for i, expr in node.exprs {
		if i > 0 {
			g.write(', ')
		}
		g.expr(expr)
	}
	g.write(']')
}

fn (mut g Gen) map_init(node ast.MapInit) {
	// Map literal: {'key': value} -> #{key => Value}
	g.write('#{')
	for i, key in node.keys {
		if i > 0 {
			g.write(', ')
		}
		// Key expression
		g.expr(key)
		g.write(' => ')
		// Value expression
		g.expr(node.vals[i])
	}
	g.write('}')
}

fn (mut g Gen) struct_init(node ast.StructInit) {
	// Struct init: Point{x: 1, y: 2} -> #{x => 1, y => 2, {vbeam,type} => 'Point'}
	type_sym := g.table.sym(node.typ)
	// Use short name (without module prefix)
	type_name := type_sym.name.all_after_last('.')

	g.write('#{')

	// Write fields
	for i, field in node.init_fields {
		if i > 0 {
			g.write(', ')
		}
		g.write('${field.name} => ')
		g.expr(field.expr)
	}

	// Add type tag
	if node.init_fields.len > 0 {
		g.write(', ')
	}
	g.write("{vbeam, type} => '${type_name}'")

	g.write('}')
}

fn (mut g Gen) index_expr(node ast.IndexExpr) {
	// Check if this is a map access or array access
	left_type := node.left_type

	// Safety check for invalid types
	if int(left_type) == 0 {
		// Unknown type - assume array access
		g.write('lists:nth(')
		if node.index is ast.IntegerLiteral {
			idx := node.index.val.int() + 1
			g.write('${idx}')
		} else {
			g.expr(node.index)
			g.write(' + 1')
		}
		g.write(', ')
		g.expr(node.left)
		g.write(')')
		return
	}

	type_sym := g.table.sym(left_type)

	if type_sym.kind == .map || type_sym.name.starts_with('map[') {
		// Map access: m['key'] -> maps:get(Key, M)
		g.write('maps:get(')
		g.expr(node.index)
		g.write(', ')
		g.expr(node.left)
		g.write(')')
	} else {
		// Array index: arr[i] -> lists:nth(I + 1, Arr)
		// Note: Erlang lists:nth is 1-indexed, V arrays are 0-indexed
		g.write('lists:nth(')
		// If index is a literal integer, we can optimize by adding 1 at compile time
		if node.index is ast.IntegerLiteral {
			idx := node.index.val.int() + 1
			g.write('${idx}')
		} else {
			g.expr(node.index)
			g.write(' + 1')
		}
		g.write(', ')
		g.expr(node.left)
		g.write(')')
	}
}

fn (mut g Gen) par_expr(node ast.ParExpr) {
	g.write('(')
	g.expr(node.expr)
	g.write(')')
}

fn (mut g Gen) prefix_expr(node ast.PrefixExpr) {
	g.write('${node.op}')
	g.expr(node.right)
}

fn (mut g Gen) if_expr(node ast.IfExpr) {
	// Check if this is a comptime $if
	if node.is_comptime {
		g.comptime_if(node)
		return
	}

	// V if expressions -> Erlang case expressions
	// if cond { return a } else if cond2 { return b } else { return c }
	// ->
	// case Cond of
	//     true -> A;
	//     false ->
	//         case Cond2 of
	//             true -> B;
	//             false -> C
	//         end
	// end

	g.gen_if_branch(node.branches, 0)
}

fn (mut g Gen) gen_if_branch(branches []ast.IfBranch, idx int) {
	if idx >= branches.len {
		return
	}

	branch := branches[idx]
	is_last := idx == branches.len - 1
	// else branch has no condition (NodeError or EmptyExpr)
	is_else := is_last && (branch.cond is ast.NodeError || branch.cond is ast.EmptyExpr)

	if is_else {
		// else branch - just output the body value
		g.gen_branch_value_inline(branch)
	} else {
		// if or else if branch
		g.write('case ')
		g.expr(branch.cond)
		g.write(' of')
		g.out.writeln('')
		g.indent++
		g.write_indent()
		g.write('true -> ')
		g.gen_branch_value_inline(branch)
		g.out.writeln(';')
		g.write_indent()
		g.write('false -> ')
		if idx + 1 < branches.len {
			g.gen_if_branch(branches, idx + 1)
		} else {
			// No else branch - return ok
			g.write('ok')
		}
		g.out.writeln('')
		g.indent--
		g.write_indent()
		g.write('end')
	}
}

fn (mut g Gen) gen_branch_value(branch ast.IfBranch) {
	// Get the value from the branch body
	// Usually the last statement is a return or expression
	if branch.stmts.len == 0 {
		g.writeln('ok')
		return
	}

	for i, stmt in branch.stmts {
		is_last := i == branch.stmts.len - 1
		match stmt {
			ast.Return {
				if stmt.exprs.len > 0 {
					g.write_indent()
					g.expr(stmt.exprs[0])
					if !is_last {
						g.write(',')
					}
					g.out.writeln('')
				}
			}
			ast.ExprStmt {
				g.write_indent()
				g.expr(stmt.expr)
				if !is_last {
					g.write(',')
				}
				g.out.writeln('')
			}
			else {
				g.stmt(stmt)
			}
		}
	}
}

// gen_branch_value_inline outputs branch value without newlines (for inline case expressions)
fn (mut g Gen) gen_branch_value_inline(branch ast.IfBranch) {
	if branch.stmts.len == 0 {
		g.write('ok')
		return
	}

	// For inline, we only handle simple cases (single return/expr)
	// Complex branches with multiple statements need block form
	if branch.stmts.len == 1 {
		stmt := branch.stmts[0]
		match stmt {
			ast.Return {
				if stmt.exprs.len > 0 {
					g.expr(stmt.exprs[0])
				} else {
					g.write('ok')
				}
			}
			ast.ExprStmt {
				g.expr(stmt.expr)
			}
			else {
				g.write('ok')
			}
		}
	} else {
		// Multiple statements - use begin/end block
		g.out.writeln('begin')
		g.indent++
		for i, stmt in branch.stmts {
			is_last := i == branch.stmts.len - 1
			match stmt {
				ast.Return {
					if stmt.exprs.len > 0 {
						g.write_indent()
						g.expr(stmt.exprs[0])
						if !is_last {
							g.write(',')
						}
						g.out.writeln('')
					}
				}
				ast.ExprStmt {
					g.write_indent()
					g.expr(stmt.expr)
					if !is_last {
						g.write(',')
					}
					g.out.writeln('')
				}
				else {
					g.stmt(stmt)
				}
			}
		}
		g.indent--
		g.write_indent()
		g.write('end')
	}
}

fn (mut g Gen) match_expr(node ast.MatchExpr) {
	// V match expression -> Erlang case expression
	// match x {
	//     .red { return 'Red' }
	//     .green { return 'Green' }
	//     else { return 'Unknown' }
	// }
	// ->
	// case X of
	//     red -> <<"Red">>;
	//     green -> <<"Green">>;
	//     _ -> <<"Unknown">>
	// end
	//
	// SPECIAL CASE: match true { ... } -> Erlang if expression
	// V pattern:
	//   match true {
	//       n % 15 == 0 { 'FizzBuzz' }
	//       n % 5 == 0 { 'Buzz' }
	//       else { n }
	//   }
	// Erlang output:
	//   if
	//       N rem 15 == 0 -> <<"FizzBuzz">>;
	//       N rem 5 == 0 -> <<"Buzz">>;
	//       true -> N
	//   end

	// Check if this is "match true { conditions }" pattern
	if node.cond is ast.BoolLiteral && node.cond.val {
		g.match_true_as_if(node)
		return
	}

	g.write('case ')
	g.expr(node.cond)
	g.write(' of')
	g.out.writeln('')
	g.indent++

	for i, branch in node.branches {
		is_last := i == node.branches.len - 1

		// Write pattern
		g.write_indent()
		if branch.is_else {
			g.write('_')
		} else {
			// Each branch can have multiple patterns
			for j, pattern in branch.exprs {
				if j > 0 {
					g.write('; ')  // Erlang pattern alternatives
				}
				g.match_pattern(pattern)
			}
		}
		g.write(' -> ')

		// Write branch body inline
		g.gen_match_branch_inline(branch.stmts)

		if !is_last {
			g.out.writeln(';')
		} else {
			g.out.writeln('')
		}
	}

	g.indent--
	g.write_indent()
	g.write('end')
}

// match_true_as_if generates Erlang 'if' expression for V's 'match true { cond { ... } }' pattern
fn (mut g Gen) match_true_as_if(node ast.MatchExpr) {
	g.write('if')
	g.out.writeln('')
	g.indent++

	for i, branch in node.branches {
		is_last := i == node.branches.len - 1

		g.write_indent()
		if branch.is_else {
			// else branch -> true guard in Erlang if
			g.write('true')
		} else {
			// The branch condition is in branch.exprs[0]
			// It's a boolean expression that becomes an Erlang guard
			if branch.exprs.len > 0 {
				g.expr(branch.exprs[0])
			} else {
				g.write('true')
			}
		}
		g.write(' -> ')

		// Write branch body inline
		g.gen_match_branch_inline(branch.stmts)

		if !is_last {
			g.out.writeln(';')
		} else {
			g.out.writeln('')
		}
	}

	g.indent--
	g.write_indent()
	g.write('end')
}

fn (mut g Gen) match_pattern(expr ast.Expr) {
	// Match patterns - enum values become atoms
	match expr {
		ast.EnumVal {
			// .red -> red (atom)
			g.write(expr.val)
		}
		ast.Ident {
			// Named patterns
			if expr.name == '_' {
				g.write('_')
			} else {
				g.write(g.erl_var(expr.name))
			}
		}
		else {
			// Other patterns (literals, etc.)
			g.expr(expr)
		}
	}
}

fn (mut g Gen) gen_match_branch_value(stmts []ast.Stmt) {
	if stmts.len == 0 {
		g.writeln('ok')
		return
	}

	for i, stmt in stmts {
		is_last := i == stmts.len - 1
		match stmt {
			ast.Return {
				if stmt.exprs.len > 0 {
					g.write_indent()
					g.expr(stmt.exprs[0])
					if !is_last {
						g.write(',')
					}
					g.out.writeln('')
				}
			}
			ast.ExprStmt {
				g.write_indent()
				g.expr(stmt.expr)
				if !is_last {
					g.write(',')
				}
				g.out.writeln('')
			}
			else {
				g.stmt(stmt)
			}
		}
	}
}

// gen_match_branch_inline outputs match branch value inline (no newlines)
fn (mut g Gen) gen_match_branch_inline(stmts []ast.Stmt) {
	if stmts.len == 0 {
		g.write('ok')
		return
	}

	// For inline, handle simple cases (single return/expr)
	if stmts.len == 1 {
		stmt := stmts[0]
		match stmt {
			ast.Return {
				if stmt.exprs.len > 0 {
					g.expr(stmt.exprs[0])
				} else {
					g.write('ok')
				}
			}
			ast.ExprStmt {
				g.expr(stmt.expr)
			}
			else {
				g.write('ok')
			}
		}
	} else {
		// Multiple statements - use begin/end block
		g.out.writeln('begin')
		g.indent++
		for i, stmt in stmts {
			is_last := i == stmts.len - 1
			match stmt {
				ast.Return {
					if stmt.exprs.len > 0 {
						g.write_indent()
						g.expr(stmt.exprs[0])
						if !is_last {
							g.write(',')
						}
						g.out.writeln('')
					}
				}
				ast.ExprStmt {
					g.write_indent()
					g.expr(stmt.expr)
					if !is_last {
						g.write(',')
					}
					g.out.writeln('')
				}
				else {
					g.stmt(stmt)
				}
			}
		}
		g.indent--
		g.write_indent()
		g.write('end')
	}
}

fn (mut g Gen) enum_val(node ast.EnumVal) {
	// Enum values become atoms: Color.red -> red, .red -> red
	g.write(node.val)
}
