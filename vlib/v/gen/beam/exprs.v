module beam

import v.ast
import v.token
import strings

fn (mut g Gen) expr(node ast.Expr) {
	match node {
		ast.IntegerLiteral { g.integer_literal(node) }
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

fn (mut g Gen) integer_literal(node ast.IntegerLiteral) {
	val := node.val
	// Convert V hex literals (0x...) to Erlang format (16#...)
	if val.len > 2 && val[0] == `0` && (val[1] == `x` || val[1] == `X`) {
		g.write('16#${val[2..]}')
	} else if val.len > 2 && val[0] == `0` && (val[1] == `o` || val[1] == `O`) {
		// Octal: 0o77 -> 8#77
		g.write('8#${val[2..]}')
	} else if val.len > 2 && val[0] == `0` && (val[1] == `b` || val[1] == `B`) {
		// Binary: 0b1010 -> 2#1010
		g.write('2#${val[2..]}')
	} else {
		g.write(val)
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

	name := node.name

	// Handle function references like main.double, Type.method, or term.green
	// In Erlang source: fun name/arity (same-module) or fun 'v.mod':name/arity (cross-module)
	if name.contains('.') {
		fn_name := name.all_after_last('.')
		mod_prefix := name.all_before_last('.')
		// Look up arity in fn_infos
		mut arity := 1 // default
		for info in g.fn_infos {
			if info.name == fn_name {
				arity = info.arity
				break
			}
		}
		// Check if mod_prefix is a type in current module (method reference)
		// or an imported module (cross-module function reference)
		mut is_local_type := false
		for info in g.fn_infos {
			if info.name.starts_with('${mod_prefix}.') {
				is_local_type = true
				break
			}
		}
		if !is_local_type && mod_prefix.len > 0 {
			// Cross-module: fun 'v.othermod':fn_name/arity
			erl_mod := g.v_mod_to_erl_mod(mod_prefix)
			g.write("fun '${erl_mod}':'${fn_name}'/${arity}")
		} else {
			// Same-module: fun fn_name/arity (or 'Type.method'/arity)
			if g.needs_atom_quote(fn_name) {
				g.write("fun '${fn_name}'/${arity}")
			} else {
				g.write('fun ${fn_name}/${arity}')
			}
		}
		return
	}

	// Look up current SSA version
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
	short_name := full_name.all_after_last('.')
	if full_name == 'println' {
		g.println_call(node)
		return
	}
	// V builtins with special handling
	if short_name == 'print' {
		g.print_call(node)
		return
	}
	if short_name == 'eprintln' {
		g.eprintln_call(node)
		return
	}
	if short_name == 'eprint' {
		g.eprint_call(node)
		return
	}
	if g.erl_builtin_call(short_name, node) {
		return
	}

	call_mod := node.mod
	// Detect cross-module calls: V resolves imported functions like
	// mymodules.add_xy with node.mod still set to the calling module.
	// Check if full_name has a module prefix different from cur_mod.
	mut is_cross_module := call_mod != g.cur_mod && call_mod.len > 0
	mut cross_mod_name := call_mod
	if !is_cross_module && full_name.contains('.') {
		mod_prefix := full_name.all_before_last('.')
		if mod_prefix != g.cur_mod && mod_prefix.len > 0 {
			is_cross_module = true
			cross_mod_name = mod_prefix
		}
	}
	if is_cross_module {
		// Cross-module call: 'v.other_module':'fn_name'(args)
		erl_mod := g.v_mod_to_erl_mod(cross_mod_name)
		fn_name := full_name.all_after_last('.')
		g.write("'${erl_mod}':'${fn_name}'(")
	} else {
		// Same-module call: fn_name(args)
		name := g.call_fn_name(full_name, call_mod)
		// Check if this is a variable holding a function (not a declared function)
		// In Erlang, calling a function stored in a variable requires uppercase: F(X)
		// Named function calls use lowercase: double(X)
		mut is_fn_var := true
		for info in g.fn_infos {
			if info.name == name {
				is_fn_var = false
				break
			}
		}
		if is_fn_var && !name.contains('.') && name.len > 0 {
			// Variable function call: F(X)
			g.write('${g.erl_var(name)}(')
		} else if g.needs_atom_quote(name) {
			g.write("'${name}'(")
		} else {
			g.write('${name}(')
		}
	}
	for i, arg in node.args {
		if i > 0 {
			g.write(', ')
		}
		g.expr(arg.expr)
	}
	g.write(')')
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

	// Handle string methods
	if type_sym.kind == .string || type_sym.name == 'string' {
		if g.erl_string_method(node) {
			return
		}
	}

	// Handle array methods
	if type_sym.kind == .array || type_sym.name.starts_with('[]') {
		if g.erl_array_method(node) {
			return
		}
	}

	// Handle map methods
	if type_sym.kind == .map || type_sym.name.starts_with('map[') {
		if g.erl_map_method(node) {
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

fn (mut g Gen) erl_string_method(node ast.CallExpr) bool {
	match node.name {
		'int' {
			g.write('binary_to_integer(')
			g.expr(node.left)
			g.write(')')
			return true
		}
		'f64' {
			g.write('binary_to_float(')
			g.expr(node.left)
			g.write(')')
			return true
		}
		'split' {
			if node.args.len > 0 {
				g.write('binary:split(')
				g.expr(node.left)
				g.write(', ')
				g.expr(node.args[0].expr)
				g.write(', [global])')
				return true
			}
			return false
		}
		'split_into_lines' {
			g.write('binary:split(')
			g.expr(node.left)
			g.write(', <<\"\\n\">>, [global])')
			return true
		}
		'contains' {
			if node.args.len > 0 {
				g.write('case binary:match(')
				g.expr(node.left)
				g.write(', ')
				g.expr(node.args[0].expr)
				g.write(') of nomatch -> false; _ -> true end')
				return true
			}
			return false
		}
		'starts_with' {
			if node.args.len > 0 {
				g.write('case string:prefix(')
				g.expr(node.left)
				g.write(', ')
				g.expr(node.args[0].expr)
				g.write(') of nomatch -> false; _ -> true end')
				return true
			}
			return false
		}
		'ends_with' {
			if node.args.len > 0 {
				g.write('case binary:longest_common_suffix([')
				g.expr(node.left)
				g.write(', ')
				g.expr(node.args[0].expr)
				g.write(']) of 0 -> false; _ -> true end')
				return true
			}
			return false
		}
		'to_lower' {
			g.write('string:lowercase(')
			g.expr(node.left)
			g.write(')')
			return true
		}
		'to_upper' {
			g.write('string:uppercase(')
			g.expr(node.left)
			g.write(')')
			return true
		}
		'trim_space', 'trim' {
			g.write('string:trim(')
			g.expr(node.left)
			g.write(')')
			return true
		}
		'replace' {
			if node.args.len >= 2 {
				g.write('binary:replace(')
				g.expr(node.left)
				g.write(', ')
				g.expr(node.args[0].expr)
				g.write(', ')
				g.expr(node.args[1].expr)
				g.write(', [global])')
				return true
			}
			return false
		}
		'bytes' {
			g.write('binary_to_list(')
			g.expr(node.left)
			g.write(')')
			return true
		}
		else {
			return false
		}
	}
}

fn (mut g Gen) erl_array_method(node ast.CallExpr) bool {
	match node.name {
		'reverse' {
			g.write('lists:reverse(')
			g.expr(node.left)
			g.write(')')
			return true
		}
		'sort' {
			g.write('lists:sort(')
			g.expr(node.left)
			g.write(')')
			return true
		}
		'clone' {
			g.expr(node.left)
			return true
		}
		'first' {
			g.write('hd(')
			g.expr(node.left)
			g.write(')')
			return true
		}
		'last' {
			g.write('lists:last(')
			g.expr(node.left)
			g.write(')')
			return true
		}
		'join' {
			if node.args.len > 0 {
				g.write('iolist_to_binary(lists:join(')
				g.expr(node.args[0].expr)
				g.write(', ')
				g.expr(node.left)
				g.write('))')
				return true
			}
			return false
		}
		'delete' {
			if node.args.len > 0 {
				g.write('lists:delete(')
				g.expr(node.args[0].expr)
				g.write(', ')
				g.expr(node.left)
				g.write(')')
				return true
			}
			return false
		}
		'contains' {
			if node.args.len > 0 {
				g.write('lists:member(')
				g.expr(node.args[0].expr)
				g.write(', ')
				g.expr(node.left)
				g.write(')')
				return true
			}
			return false
		}
		'filter' {
			if node.args.len > 0 {
				g.write('lists:filter(')
				g.expr(node.args[0].expr)
				g.write(', ')
				g.expr(node.left)
				g.write(')')
				return true
			}
			return false
		}
		'map' {
			if node.args.len > 0 {
				g.write('lists:map(')
				g.expr(node.args[0].expr)
				g.write(', ')
				g.expr(node.left)
				g.write(')')
				return true
			}
			return false
		}
		'wait' {
			g.write('ok')
			return true
		}
		else {
			return false
		}
	}
}

fn (mut g Gen) erl_map_method(node ast.CallExpr) bool {
	match node.name {
		'keys' {
			g.write('maps:keys(')
			g.expr(node.left)
			g.write(')')
			return true
		}
		'values' {
			g.write('maps:values(')
			g.expr(node.left)
			g.write(')')
			return true
		}
		'clone' {
			g.expr(node.left)
			return true
		}
		'delete' {
			if node.args.len > 0 {
				g.write('maps:remove(')
				g.expr(node.args[0].expr)
				g.write(', ')
				g.expr(node.left)
				g.write(')')
				return true
			}
			return false
		}
		else {
			return false
		}
	}
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

fn (mut g Gen) print_call(node ast.CallExpr) {
	if node.args.len == 0 {
		g.write('ok')
		return
	}
	arg := node.args[0]
	arg_type := arg.typ
	if arg.expr is ast.StringLiteral {
		g.write('io:format("~s", [')
		g.expr(arg.expr)
		g.write('])')
	} else if int(arg_type) != 0 {
		g.write('io:format("~s", [')
		g.to_binary_expr(arg.expr, arg_type)
		g.write('])')
	} else {
		g.write('io:format("~s", [')
		g.expr(arg.expr)
		g.write('])')
	}
}

fn (mut g Gen) eprintln_call(node ast.CallExpr) {
	if node.args.len == 0 {
		g.write('io:format(standard_error, "~n", [])')
		return
	}
	arg := node.args[0]
	arg_type := arg.typ
	if arg.expr is ast.StringLiteral {
		g.write('io:format(standard_error, "~s~n", [')
		g.expr(arg.expr)
		g.write('])')
	} else if int(arg_type) != 0 {
		g.write('io:format(standard_error, "~s~n", [')
		g.to_binary_expr(arg.expr, arg_type)
		g.write('])')
	} else {
		g.write('io:format(standard_error, "~s~n", [')
		g.expr(arg.expr)
		g.write('])')
	}
}

fn (mut g Gen) eprint_call(node ast.CallExpr) {
	if node.args.len == 0 {
		g.write('ok')
		return
	}
	g.write('io:format(standard_error, "~s", [')
	g.expr(node.args[0].expr)
	g.write('])')
}

fn (mut g Gen) erl_builtin_call(name string, node ast.CallExpr) bool {
	match name {
		'panic' {
			g.write('erlang:error({panic, ')
			if node.args.len > 0 {
				g.expr(node.args[0].expr)
			} else {
				g.write('panic')
			}
			g.write('})')
			return true
		}
		'error' {
			// V's error() constructor -> erlang:error(Message)
			g.write('erlang:error(')
			if node.args.len > 0 {
				g.expr(node.args[0].expr)
			} else {
				g.write('error')
			}
			g.write(')')
			return true
		}
		'sleep' {
			g.write('timer:sleep(')
			if node.args.len > 0 {
				g.expr(node.args[0].expr)
			} else {
				g.write('0')
			}
			g.write(')')
			return true
		}
		'sqrt' {
			g.write('math:sqrt(')
			if node.args.len > 0 {
				g.expr(node.args[0].expr)
			} else {
				g.write('0')
			}
			g.write(')')
			return true
		}
		'arguments' {
			g.write('init:get_plain_arguments()')
			return true
		}
		else {
			return false
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

	// Check for integer division: V's / with integers should use Erlang's div
	// In V: 10 / 3 = 3 (integer division)
	// In Erlang: 10 / 3 = 3.333... (float division)
	// In Erlang: 10 div 3 = 3 (integer division)
	if node.op == .div {
		left_is_int := g.is_integer_type_or_literal(node.left, node.left_type)
		right_is_int := g.is_integer_type_or_literal(node.right, node.right_type)
		left_is_float := g.is_float_type_or_literal(node.left, node.left_type)
		right_is_float := g.is_float_type_or_literal(node.right, node.right_type)

		// Use div only if both are integers AND neither is a float
		if left_is_int && right_is_int && !left_is_float && !right_is_float {
			g.expr(node.left)
			g.write(' div ')
			g.expr(node.right)
			return
		}
	}

	// Handle 'in' operator: x in list -> lists:member(X, List)
	if node.op == .key_in {
		g.write('lists:member(')
		g.expr(node.left)
		g.write(', ')
		g.expr(node.right)
		g.write(')')
		return
	}

	// Handle 'not in' operator: x !in list -> not lists:member(X, List)
	if node.op == .not_in {
		g.write('(not lists:member(')
		g.expr(node.left)
		g.write(', ')
		g.expr(node.right)
		g.write('))')
		return
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

// is_integer_type_or_literal checks if an expression is an integer type or integer literal
fn (g Gen) is_integer_type_or_literal(expr ast.Expr, typ ast.Type) bool {
	// Check if it's an integer literal
	if expr is ast.IntegerLiteral {
		return true
	}
	// Check the type
	if int(typ) != 0 && g.is_int_type(typ) {
		return true
	}
	return false
}

// is_float_type_or_literal checks if an expression is a float type or float literal
fn (g Gen) is_float_type_or_literal(expr ast.Expr, typ ast.Type) bool {
	// Check if it's a float literal
	if expr is ast.FloatLiteral {
		return true
	}
	// Check the type
	if int(typ) != 0 && g.is_float_type(typ) {
		return true
	}
	return false
}

// erlang_operator translates V operator tokens to Erlang equivalents
fn (g Gen) erlang_operator(op token.Kind) string {
	return match op {
		// Comparison operators
		.le { '=<' } // V's <= -> Erlang's =<
		.ne { '/=' } // V's != -> Erlang's /=
		// Arithmetic operators
		.mod { 'rem' } // V's % -> Erlang's rem
		// Logical operators (short-circuit)
		.and { 'andalso' } // V's && -> Erlang's andalso
		.logical_or { 'orelse' } // V's || -> Erlang's orelse
		// Bitwise operators
		.amp { 'band' } // V's & (bitwise AND) -> Erlang's band
		.pipe { 'bor' } // V's | (bitwise OR) -> Erlang's bor
		.xor { 'bxor' } // V's ^ (bitwise XOR) -> Erlang's bxor
		.left_shift { 'bsl' } // V's << -> Erlang's bsl
		.right_shift { 'bsr' } // V's >> -> Erlang's bsr
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
	// Handle V prefix operators that differ in Erlang
	match node.op {
		.not {
			// V's !x -> Erlang's not X
			g.write('not ')
			g.expr(node.right)
		}
		.amp {
			// V's &x (address-of) - in Erlang, just output the value
			// Since Erlang has no pointers, we pass by value
			g.expr(node.right)
		}
		.minus {
			// Unary minus
			g.write('-')
			g.expr(node.right)
		}
		.bit_not {
			// V's ~x (bitwise NOT) -> Erlang's bnot X
			g.write('bnot ')
			g.expr(node.right)
		}
		else {
			// Other prefix operators
			g.write('${node.op}')
			g.expr(node.right)
		}
	}
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
