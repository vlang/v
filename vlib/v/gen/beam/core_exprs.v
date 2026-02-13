module beam

import v.ast
import strings

// core_expr dispatches expression generation for Core Erlang.
// All output is inline (no newlines) - the caller handles line breaks.
fn (mut g CoreGen) core_expr(node ast.Expr) {
	if g.core_expr_dispatch_literals(node) || g.core_expr_dispatch_calls(node)
		|| g.core_expr_dispatch_collections(node) || g.core_expr_dispatch_control(node)
		|| g.core_expr_dispatch_misc(node) {
		return
	}
	g.emit_atom('false')
}

fn (mut g CoreGen) core_expr_dispatch_literals(node ast.Expr) bool {
	match node {
		ast.IntegerLiteral {
			g.core_integer_literal(node)
			return true
		}
		ast.FloatLiteral {
			g.core_float_literal(node)
			return true
		}
		ast.StringLiteral {
			g.core_string_literal(node)
			return true
		}
		ast.BoolLiteral {
			g.emit_atom(if node.val { 'true' } else { 'false' })
			return true
		}
		ast.CharLiteral {
			g.core_char_literal(node)
			return true
		}
		ast.EnumVal {
			g.core_enum_val(node)
			return true
		}
		ast.None {
			g.emit_atom('none')
			return true
		}
		ast.Nil {
			g.emit_atom('nil')
			return true
		}
		else {}
	}
	return false
}

fn (mut g CoreGen) core_expr_dispatch_calls(node ast.Expr) bool {
	match node {
		ast.Ident {
			g.core_ident(node)
			return true
		}
		ast.CallExpr {
			if node.or_block.kind != .absent {
				g.core_expr_with_or(node, node.or_block)
			} else {
				g.core_call_expr(node)
			}
			return true
		}
		ast.InfixExpr {
			g.core_infix_expr(node)
			return true
		}
		ast.PrefixExpr {
			g.core_prefix_expr(node)
			return true
		}
		ast.CastExpr {
			g.core_cast_expr(node)
			return true
		}
		ast.StringInterLiteral {
			g.core_string_inter(node)
			return true
		}
		ast.SelectorExpr {
			g.core_selector_expr(node)
			return true
		}
		ast.IndexExpr {
			g.core_index_expr(node)
			return true
		}
		else {}
	}
	return false
}

fn (mut g CoreGen) core_expr_dispatch_collections(node ast.Expr) bool {
	match node {
		ast.ArrayInit {
			g.core_array_init(node)
			return true
		}
		ast.StructInit {
			g.core_struct_init(node)
			return true
		}
		ast.MapInit {
			g.core_map_init(node)
			return true
		}
		ast.ConcatExpr {
			g.core_concat_expr(node)
			return true
		}
		ast.Assoc {
			g.core_assoc(node)
			return true
		}
		else {}
	}
	return false
}

fn (mut g CoreGen) core_expr_dispatch_control(node ast.Expr) bool {
	match node {
		ast.IfExpr {
			g.core_if_expr(node)
			return true
		}
		ast.MatchExpr {
			g.core_match_expr(node)
			return true
		}
		ast.SelectExpr {
			g.core_select_expr(node)
			return true
		}
		ast.SpawnExpr {
			g.core_spawn_call(node.call_expr)
			return true
		}
		ast.GoExpr {
			g.core_spawn_call(node.call_expr)
			return true
		}
		ast.AnonFn {
			g.core_anon_fn(node)
			return true
		}
		ast.LambdaExpr {
			if node.func != unsafe { nil } {
				g.core_anon_fn(*node.func)
			} else {
				g.emit_atom('false')
			}
			return true
		}
		else {}
	}
	return false
}

fn (mut g CoreGen) core_expr_dispatch_misc(node ast.Expr) bool {
	match node {
		ast.ParExpr {
			g.core_expr(node.expr)
			return true
		}
		ast.UnsafeExpr, ast.PostfixExpr, ast.Likely, ast.AsCast, ast.DumpExpr, ast.ArrayDecompose {
			g.core_expr(node.expr)
			return true
		}
		ast.IsRefType {
			g.emit_atom('true')
			return true
		}
		ast.SizeOf {
			g.emit_int('8')
			return true
		}
		ast.AtExpr {
			g.core_at_expr(node)
			return true
		}
		ast.TypeOf {
			g.core_typeof(node)
			return true
		}
		ast.RangeExpr {
			g.core_range_expr(node)
			return true
		}
		ast.IfGuardExpr {
			g.emit_atom('false')
			return true
		}
		ast.EmptyExpr {
			g.emit_atom('ok')
			return true
		}
		ast.Comment {
			return true
		}
		else {}
	}
	return false
}

fn (mut g CoreGen) core_integer_literal(node ast.IntegerLiteral) {
	val := node.val
	if val.len > 2 && val[0] == `0` && (val[1] == `x` || val[1] == `X`) {
		g.emit_int('16#${val[2..]}')
	} else if val.len > 2 && val[0] == `0` && (val[1] == `o` || val[1] == `O`) {
		g.emit_int('8#${val[2..]}')
	} else if val.len > 2 && val[0] == `0` && (val[1] == `b` || val[1] == `B`) {
		g.emit_int('2#${val[2..]}')
	} else {
		g.emit_int(val)
	}
}

fn (mut g CoreGen) core_float_literal(node ast.FloatLiteral) {
	val := node.val
	// Core Erlang requires a decimal point in float literals.
	// V may produce "1e+10" or "1e10" which needs "1.0e+10" or "1.0e10"
	if val.contains('e') || val.contains('E') {
		if !val.contains('.') {
			// Insert .0 before the exponent
			e_pos := if val.contains('e') { val.index('e') or { -1 } } else { val.index('E') or {
					-1} }
			if e_pos > 0 {
				g.emit_float('${val[..e_pos]}.0${val[e_pos..]}')
				return
			}
		}
	}
	// Ensure there's a decimal point
	if !val.contains('.') {
		g.emit_float('${val}.0')
	} else if val.starts_with('.') {
		// .75 -> 0.75 (Core Erlang requires leading digit)
		g.emit_float('0${val}')
	} else {
		g.emit_float(val)
	}
}

fn (mut g CoreGen) core_string_literal(node ast.StringLiteral) {
	if g.etf_mode {
		g.emit_binary(core_escape_binary(node.val))
	} else {
		// Core Erlang bitstring: #{#<72>(8,1,'integer',['unsigned'|['big']]),... }#
		g.write_core(core_bitstring(node.val))
	}
}

fn (mut g CoreGen) core_char_literal(node ast.CharLiteral) {
	// V char literal → integer codepoint in Core Erlang
	val := node.val
	if val.len == 0 {
		g.emit_int('0')
		return
	}
	// Handle escape sequences
	if val.len >= 2 && val[0] == `\\` {
		match val[1] {
			`n` { g.emit_int('10') } // \n = newline
			`r` { g.emit_int('13') } // \r = carriage return
			`t` { g.emit_int('9') } // \t = tab
			`\\` { g.emit_int('92') } // \\ = backslash
			`'` { g.emit_int('39') } // \' = single quote
			`"` { g.emit_int('34') } // \" = double quote
			`0` { g.emit_int('0') } // \0 = null
			`a` { g.emit_int('7') } // \a = bell
			`b` { g.emit_int('8') } // \b = backspace
			`e` { g.emit_int('27') } // \e = escape
			`f` { g.emit_int('12') } // \f = form feed
			`v` { g.emit_int('11') } // \v = vertical tab
			else { g.emit_int('${int(val[1])}') }
		}
		return
	}
	// Single character → its Unicode codepoint
	runes := val.runes()
	if runes.len > 0 {
		g.emit_int('${int(runes[0])}')
	} else {
		g.emit_int('${int(val[0])}')
	}
}

fn (mut g CoreGen) core_cast_expr(node ast.CastExpr) {
	if node.typ.idx() == 0 || node.expr_type.idx() == 0 {
		g.core_expr(node.expr)
		return
	}
	target_sym := g.table.sym(node.typ)
	source_sym := g.table.sym(node.expr_type)
	target_name := target_sym.name
	source_name := source_sym.name
	is_float_source := source_name == 'f64' || source_name == 'f32'
		|| source_name == 'float_literal'
	if g.core_cast_int_to_float(node, source_name, target_name)
		|| g.core_cast_float_to_int(node, target_name, is_float_source)
		|| g.core_cast_string_to_int(node, source_name, target_name)
		|| g.core_cast_int_to_string(node, source_name, target_name)
		|| g.core_cast_unsigned_narrow(node, target_name, is_float_source)
		|| g.core_cast_signed_narrow(node, target_name, is_float_source)
		|| g.core_cast_i32(node, target_name, is_float_source)
		|| g.core_cast_u64(node, target_name, is_float_source) {
		return
	}
	g.core_expr(node.expr)
}

fn (mut g CoreGen) core_cast_int_to_float(node ast.CastExpr, source_name string, target_name string) bool {
	is_int_source := source_name in ['int', 'i64', 'i32', 'i16', 'i8', 'u8', 'u16', 'u32', 'u64']
	if !is_int_source || !(target_name == 'f64' || target_name == 'f32') {
		return false
	}
	g.begin_call('erlang', 'float')
	g.core_expr(node.expr)
	g.end_call()
	return true
}

fn (mut g CoreGen) core_cast_float_to_int(node ast.CastExpr, target_name string, is_float_source bool) bool {
	if !is_float_source {
		return false
	}
	if !(target_name == 'int' || target_name == 'i64' || target_name == 'i32'
		|| target_name == 'u64') {
		return false
	}
	g.begin_call('erlang', 'trunc')
	g.core_expr(node.expr)
	g.end_call()
	return true
}

fn (mut g CoreGen) core_cast_string_to_int(node ast.CastExpr, source_name string, target_name string) bool {
	if source_name != 'string' || !(target_name == 'int' || target_name == 'i64') {
		return false
	}
	g.begin_call('erlang', 'binary_to_integer')
	g.core_expr(node.expr)
	g.end_call()
	return true
}

fn (mut g CoreGen) core_cast_int_to_string(node ast.CastExpr, source_name string, target_name string) bool {
	if !(source_name == 'int' || source_name == 'i64') || target_name != 'string' {
		return false
	}
	g.begin_call('erlang', 'integer_to_binary')
	g.core_expr(node.expr)
	g.end_call()
	return true
}

fn (mut g CoreGen) core_cast_unsigned_narrow(node ast.CastExpr, target_name string, is_float_source bool) bool {
	mask := match target_name {
		'u8' { '255' }
		'u16' { '65535' }
		'u32' { '4294967295' }
		else { '' }
	}
	if mask == '' {
		return false
	}
	g.begin_call('erlang', 'band')
	if is_float_source {
		g.begin_call('erlang', 'trunc')
	}
	g.core_expr(node.expr)
	if is_float_source {
		g.end_call()
	}
	g.emit_sep()
	g.emit_int(mask)
	g.end_call()
	return true
}

fn (mut g CoreGen) core_cast_signed_narrow(node ast.CastExpr, target_name string, is_float_source bool) bool {
	params := match target_name {
		'i8' { ['255', '128', '256', '128'] }
		'i16' { ['65535', '32768', '65536', '32768'] }
		else { []string{} }
	}
	if params.len == 0 {
		return false
	}
	g.begin_call('erlang', '-')
	g.begin_call('erlang', 'rem')
	g.begin_call('erlang', '+')
	g.begin_call('erlang', 'band')
	if is_float_source {
		g.begin_call('erlang', 'trunc')
	}
	g.core_expr(node.expr)
	if is_float_source {
		g.end_call()
	}
	for p in params {
		g.emit_sep()
		g.emit_int(p)
		g.end_call()
	}
	return true
}

fn (mut g CoreGen) core_cast_i32(node ast.CastExpr, target_name string, is_float_source bool) bool {
	if target_name != 'i32' {
		return false
	}
	g.begin_call('erlang', '-')
	g.begin_call('erlang', 'rem')
	g.begin_call('erlang', '+')
	g.begin_call('erlang', 'band')
	if is_float_source {
		g.begin_call('erlang', 'trunc')
	}
	g.core_expr(node.expr)
	if is_float_source {
		g.end_call()
	}
	g.emit_sep()
	g.emit_int('4294967295')
	g.end_call()
	g.emit_sep()
	g.emit_int('2147483648')
	g.end_call()
	g.emit_sep()
	g.emit_int('4294967296')
	g.end_call()
	g.emit_sep()
	g.emit_int('2147483648')
	g.end_call()
	return true
}

fn (mut g CoreGen) core_cast_u64(node ast.CastExpr, target_name string, is_float_source bool) bool {
	if target_name != 'u64' {
		return false
	}
	g.begin_call('erlang', 'band')
	if is_float_source {
		g.begin_call('erlang', 'trunc')
	}
	g.core_expr(node.expr)
	if is_float_source {
		g.end_call()
	}
	g.emit_sep()
	g.emit_int('18446744073709551615')
	g.end_call()
	return true
}

fn (mut g CoreGen) core_expr_with_or(expr ast.Expr, or_block ast.OrExpr) {
	// Wrap any expression with an or { } block in try/catch
	// try EXPR of <V> -> V catch <_,_,_> -> OR_BODY
	g.temp_counter++
	tmp := '_cor${g.temp_counter}'
	e1 := '_cor_c${g.temp_counter}'
	e2 := '_cor_r${g.temp_counter}'
	e3 := '_cor_s${g.temp_counter}'
	if g.etf_mode {
		g.begin_try()
		match expr {
			ast.CallExpr { g.core_call_expr(expr) }
			ast.SelectorExpr { g.core_selector_expr(expr) }
			else { g.core_expr(expr) }
		}
		g.mid_try_of(tmp)
		g.emit_var(tmp)
		g.mid_try_catch(e1, e2, e3)
		g.core_or_body(or_block)
		g.end_try()
	} else {
		g.write_core('try ')
		// Call inner handler directly to avoid recursion (these types have or_block)
		match expr {
			ast.CallExpr { g.core_call_expr(expr) }
			ast.SelectorExpr { g.core_selector_expr(expr) }
			else { g.core_expr(expr) }
		}
		g.write_core(" of <${tmp}> when 'true' -> ${tmp}")
		g.write_core(' catch <${e1},${e2},${e3}> when \'true\' -> ')
		g.core_or_body(or_block)
	}
}

// core_or_body emits the body of an or-block (shared between text and ETF modes).
fn (mut g CoreGen) core_or_body(or_block ast.OrExpr) {
	if or_block.stmts.len > 0 {
		if or_block.stmts.len == 1 {
			stmt := or_block.stmts[0]
			match stmt {
				ast.ExprStmt {
					g.core_expr(stmt.expr)
				}
				ast.Return {
					if stmt.exprs.len > 0 {
						g.core_expr(stmt.exprs[0])
					} else {
						g.emit_atom('ok')
					}
				}
				else {
					g.emit_atom('ok')
				}
			}
		} else {
			g.core_fn_body(or_block.stmts)
		}
	} else {
		g.emit_atom('ok')
	}
}

fn (mut g CoreGen) core_concat_expr(node ast.ConcatExpr) {
	// Multi-return value unpacking: emit as tuple
	if g.etf_mode {
		g.begin_tuple()
		for i, val in node.vals {
			if i > 0 {
				g.emit_sep()
			}
			g.core_expr(val)
		}
		g.end_tuple()
	} else {
		g.write_core('{')
		for i, val in node.vals {
			if i > 0 {
				g.write_core(', ')
			}
			g.core_expr(val)
		}
		g.write_core('}')
	}
}

fn (mut g CoreGen) core_at_expr(node ast.AtExpr) {
	// Compile-time attributes: @FN, @LINE, @FILE, etc.
	// node.val is pre-resolved by V frontend
	if g.etf_mode {
		g.emit_binary(core_escape_binary(node.val))
	} else {
		g.write_core(core_bitstring(node.val))
	}
}

fn (mut g CoreGen) core_typeof(node ast.TypeOf) {
	// typeof(expr) → type name as string
	if node.typ.idx() != 0 {
		type_sym := g.table.sym(node.typ)
		if g.etf_mode {
			g.emit_binary(core_escape_binary(type_sym.name))
		} else {
			g.write_core(core_bitstring(type_sym.name))
		}
	} else {
		if g.etf_mode {
			g.emit_binary('unknown')
		} else {
			g.write_core(core_bitstring('unknown'))
		}
	}
}

fn (mut g CoreGen) core_range_expr(node ast.RangeExpr) {
	// a..b → lists:seq(A, B)
	g.begin_call('lists', 'seq')
	if node.has_low {
		g.core_expr(node.low)
	} else {
		g.emit_int('0')
	}
	g.emit_sep()
	if node.has_high {
		g.core_expr(node.high)
	} else {
		g.emit_int('0')
	}
	g.end_call()
}

fn (mut g CoreGen) core_spawn_call(node ast.CallExpr) {
	// spawn fn() / go fn() → vbeam_task:async(fun() -> call() end)
	// Using vbeam_task:async for spawn-as-expression (result can be awaited)
	if g.etf_mode {
		g.begin_call('vbeam_task', 'async')
		g.begin_fun([]string{})
		g.core_call_expr(node)
		g.end_fun()
		g.end_call()
	} else {
		g.write_core("call 'vbeam_task':'async'(fun () -> ")
		g.core_call_expr(node)
		g.write_core(')')
	}
}

fn (mut g CoreGen) core_anon_fn(node ast.AnonFn) {
	// Anonymous function → Core Erlang fun
	decl := node.decl
	params := decl.params
	// Save outer scope var_map entries for param names to prevent leaking
	mut saved_vars := map[string]string{}
	for p in params {
		if p.name in g.var_map {
			saved_vars[p.name] = g.var_map[p.name]
		}
	}
	mut param_names := []string{}
	for p in params {
		param_names << g.next_core_var(p.name)
	}
	if g.etf_mode {
		g.begin_fun(param_names)
		if decl.stmts.len > 0 {
			g.core_fn_body(decl.stmts)
		} else {
			g.emit_atom('ok')
		}
		g.end_fun()
	} else {
		g.write_core('fun (${param_names.join(', ')}) -> ')
		if decl.stmts.len > 0 {
			g.core_fn_body(decl.stmts)
		} else {
			g.write_core("'ok'")
		}
		g.write_core('')
	}
	// Restore outer scope var_map entries
	for p in params {
		if p.name in saved_vars {
			g.var_map[p.name] = saved_vars[p.name]
		} else {
			g.var_map.delete(p.name)
		}
	}
}

fn (mut g CoreGen) core_assoc(node ast.Assoc) {
	// Struct update: {s | field: val, ...}
	// In Core Erlang: maps:merge(OldMap, #{field => val, ...})
	// Or: call 'maps':'put'(Key, Val, Map) for single field
	if node.fields.len == 0 {
		vname := g.core_var(node.var_name)
		if g.etf_mode {
			g.emit_var(vname)
		} else {
			g.write_core(vname)
		}
		return
	}
	// Build up nested map_put calls
	vname := g.core_var(node.var_name)
	if g.etf_mode {
		for i := node.fields.len - 1; i >= 0; i-- {
			g.begin_call('maps', 'put')
			g.emit_atom(node.fields[i])
			g.emit_sep()
			g.core_expr(node.exprs[i])
			g.emit_sep()
		}
		g.emit_var(vname)
		for _ in node.fields {
			g.end_call()
		}
	} else {
		for i := node.fields.len - 1; i >= 0; i-- {
			g.write_core("call 'maps':'put'('${node.fields[i]}', ")
			g.core_expr(node.exprs[i])
			g.write_core(', ')
		}
		g.write_core(vname)
		for _ in node.fields {
			g.write_core(')')
		}
	}
}

fn (mut g CoreGen) core_ident(node ast.Ident) {
	if g.core_ident_constant(node) || g.core_ident_function_ref(node) {
		return
	}
	g.core_ident_emit_var(node.name)
}

fn (mut g CoreGen) core_call_expr(node ast.CallExpr) {
	if node.is_method {
		g.core_method_call(node)
		return
	}
	full_name := node.name
	short_name := full_name.all_after_last('.')
	if g.core_call_expr_print_builtin(node, full_name, short_name)
		|| g.core_builtin_call(short_name, node) || g.core_call_expr_fn_var(node, short_name) {
		return
	}
	g.core_call_expr_regular(node, full_name)
}

fn (mut g CoreGen) core_ident_constant(node ast.Ident) bool {
	if node.kind == .constant && node.obj is ast.ConstField {
		g.core_expr(node.obj.expr)
		return true
	}
	return false
}

fn (mut g CoreGen) core_ident_function_ref(node ast.Ident) bool {
	name := node.name
	if !name.contains('.') {
		return false
	}
	fn_name := name.all_after_last('.')
	mod_prefix := name.all_before_last('.')
	arity := g.core_ident_fn_arity(fn_name)
	params := g.core_ident_ref_params(arity)
	is_local_type := g.core_ident_is_local_type(mod_prefix)
	if g.etf_mode {
		g.core_ident_function_ref_etf(fn_name, mod_prefix, arity, params, is_local_type)
	} else {
		g.core_ident_function_ref_text(fn_name, mod_prefix, arity, params, is_local_type)
	}
	return true
}

fn (mut g CoreGen) core_ident_function_ref_etf(fn_name string, mod_prefix string, arity int, params []string, is_local_type bool) {
	if !is_local_type && mod_prefix.len > 0 {
		erl_mod := g.core_v_mod_to_erl_mod(mod_prefix)
		g.begin_fun(params)
		g.begin_call(erl_mod, fn_name)
		for i, p in params {
			if i > 0 {
				g.emit_sep()
			}
			g.emit_var(p)
		}
		g.end_call()
		g.end_fun()
		return
	}
	g.begin_fun(params)
	g.begin_apply(fn_name, arity)
	for i, p in params {
		if i > 0 {
			g.emit_sep()
		}
		g.emit_var(p)
	}
	g.end_apply()
	g.end_fun()
}

fn (mut g CoreGen) core_ident_function_ref_text(fn_name string, mod_prefix string, arity int, params []string, is_local_type bool) {
	if !is_local_type && mod_prefix.len > 0 {
		erl_mod := g.core_v_mod_to_erl_mod(mod_prefix)
		g.write_core("fun (${params.join(', ')}) -> call '${erl_mod}':'${fn_name}'(${params.join(', ')})")
		return
	}
	g.write_core('fun (${params.join(', ')}) -> apply ' +
		"'${fn_name}'/${arity}(${params.join(', ')})")
}

fn (g CoreGen) core_ident_fn_arity(fn_name string) int {
	mut arity := 1
	for info in g.fn_infos {
		if info.name == fn_name {
			arity = info.arity
			break
		}
	}
	return arity
}

fn (g CoreGen) core_ident_ref_params(arity int) []string {
	mut params := []string{}
	for i in 0 .. arity {
		params << '_fref${i}'
	}
	return params
}

fn (g CoreGen) core_ident_is_local_type(mod_prefix string) bool {
	for info in g.fn_infos {
		if info.name.starts_with('${mod_prefix}.') {
			return true
		}
	}
	return false
}

fn (mut g CoreGen) core_ident_emit_var(name string) {
	vname := g.core_var(name)
	if g.etf_mode {
		g.emit_var(vname)
	} else {
		g.write_core(vname)
	}
}

fn (mut g CoreGen) core_call_expr_print_builtin(node ast.CallExpr, full_name string, short_name string) bool {
	if full_name == 'println' {
		g.core_println_call(node)
		return true
	}
	if short_name == 'print' {
		g.core_print_call(node)
		return true
	}
	if short_name == 'eprintln' {
		g.core_eprintln_call(node)
		return true
	}
	if short_name == 'eprint' {
		g.core_eprint_call(node)
		return true
	}
	return false
}

fn (mut g CoreGen) core_call_expr_fn_var(node ast.CallExpr, short_name string) bool {
	if !node.is_fn_var {
		return false
	}
	var_name := g.core_var(short_name)
	if g.etf_mode {
		g.begin_apply_var()
		g.emit_var(var_name)
		g.mid_apply_args()
		for i, arg in node.args {
			if i > 0 {
				g.emit_sep()
			}
			g.core_expr(arg.expr)
		}
		g.end_apply()
	} else {
		g.write_core('apply ${var_name}(')
		for i, arg in node.args {
			if i > 0 {
				g.write_core(', ')
			}
			g.core_expr(arg.expr)
		}
		g.write_core(')')
	}
	return true
}

fn (mut g CoreGen) core_call_expr_regular(node ast.CallExpr, full_name string) {
	is_cross_module, cross_mod_name := g.core_call_expr_cross_module(full_name, node.mod)
	if g.etf_mode {
		g.core_call_expr_regular_etf(node, full_name, is_cross_module, cross_mod_name)
	} else {
		g.core_call_expr_regular_text(node, full_name, is_cross_module, cross_mod_name)
	}
}

fn (g CoreGen) core_call_expr_cross_module(full_name string, call_mod string) (bool, string) {
	mut is_cross_module := call_mod != g.cur_mod && call_mod.len > 0
	mut cross_mod_name := call_mod
	if !is_cross_module && full_name.contains('.') {
		mod_prefix := full_name.all_before_last('.')
		if mod_prefix != g.cur_mod && mod_prefix.len > 0 {
			is_cross_module = true
			cross_mod_name = mod_prefix
		}
	}
	return is_cross_module, cross_mod_name
}

fn (mut g CoreGen) core_call_expr_regular_etf(node ast.CallExpr, full_name string, is_cross_module bool, cross_mod_name string) {
	name := g.core_call_fn_name(full_name, node.mod)
	if is_cross_module {
		erl_mod := g.core_v_mod_to_erl_mod(cross_mod_name)
		g.begin_call(erl_mod, full_name.all_after_last('.'))
	} else if g.core_is_erlang_bif(name) {
		g.begin_call('erlang', name)
	} else {
		g.begin_apply(name, node.args.len)
	}
	for i, arg in node.args {
		if i > 0 {
			g.emit_sep()
		}
		g.core_expr(arg.expr)
	}
	if is_cross_module || g.core_is_erlang_bif(name) {
		g.end_call()
	} else {
		g.end_apply()
	}
}

fn (mut g CoreGen) core_call_expr_regular_text(node ast.CallExpr, full_name string, is_cross_module bool, cross_mod_name string) {
	if is_cross_module {
		erl_mod := g.core_v_mod_to_erl_mod(cross_mod_name)
		g.write_core("call '${erl_mod}':'${full_name.all_after_last('.')}'(")
	} else {
		name := g.core_call_fn_name(full_name, node.mod)
		if g.core_is_erlang_bif(name) {
			g.write_core("call 'erlang':'${name}'(")
		} else {
			g.write_core("apply '${name}'/${node.args.len}(")
		}
	}
	for i, arg in node.args {
		if i > 0 {
			g.write_core(', ')
		}
		g.core_expr(arg.expr)
	}
	g.write_core(')')
}

// core_is_erlang_bif returns true for Erlang BIF names that need
// call 'erlang':'fn'(args) in Core Erlang instead of apply 'fn'/arity(args)
fn (g CoreGen) core_is_erlang_bif(name string) bool {
	return name in ['exit', 'error', 'throw', 'abs', 'self', 'spawn', 'spawn_link', 'spawn_monitor',
		'send', 'is_integer', 'is_float', 'is_atom', 'is_list', 'is_binary', 'is_tuple', 'is_map',
		'is_boolean', 'is_number', 'is_pid', 'is_port', 'is_reference', 'is_function', 'hd', 'tl',
		'length', 'tuple_size', 'byte_size', 'bit_size', 'map_size', 'element', 'setelement',
		'make_ref', 'node', 'nodes', 'register', 'whereis', 'monitor', 'demonitor', 'link', 'unlink',
		'process_flag', 'put', 'get', 'erase', 'binary_to_list', 'list_to_binary', 'atom_to_list',
		'list_to_atom', 'integer_to_list', 'list_to_integer', 'float_to_list', 'list_to_float',
		'integer_to_binary', 'binary_to_integer', 'float_to_binary', 'binary_to_float',
		'atom_to_binary', 'binary_to_atom', 'iolist_to_binary', 'tuple_to_list', 'list_to_tuple',
		'term_to_binary', 'binary_to_term', 'apply', 'halt', 'round', 'trunc', 'max', 'min']
}

// core_builtin_call handles V builtin function calls that map to Erlang stdlib
fn (mut g CoreGen) core_builtin_call(name string, node ast.CallExpr) bool {
	return g.core_builtin_math(name, node) || g.core_builtin_system(name, node)
		|| g.core_builtin_os(name, node) || g.core_builtin_time(name, node)
		|| g.core_builtin_random(name, node) || g.core_builtin_io(name, node)
		|| g.core_builtin_string_conv(name, node) || g.core_builtin_terminal(name, node)
		|| g.core_builtin_net(name, node) || g.core_builtin_concurrency(name, node)
		|| g.core_builtin_encoding(name, node) || g.core_builtin_misc(name, node)
}

fn (mut g CoreGen) core_builtin_math(name string, node ast.CallExpr) bool {
	return g.core_builtin_math_1(name, node) || g.core_builtin_math_2(name, node)
		|| g.core_builtin_math_3(name, node) || g.core_builtin_math_4(name, node)
}

fn (mut g CoreGen) core_builtin_math_1(name string, node ast.CallExpr) bool {
	match name {
		'sqrt' {
			// V math.sqrt(n) -> math:sqrt(N)
			g.begin_call('math', 'sqrt')
			if node.args.len > 0 {
				g.core_expr(node.args[0].expr)
			} else {
				g.emit_int('0')
			}
			g.end_call()
			return true
		}
		'abs' {
			g.begin_call('erlang', 'abs')
			if node.args.len > 0 {
				g.core_expr(node.args[0].expr)
			} else {
				g.emit_int('0')
			}
			g.end_call()
			return true
		}
		'log' {
			// V math.log(n) -> math:log(N)
			g.begin_call('math', 'log')
			if node.args.len > 0 {
				g.core_expr(node.args[0].expr)
			}
			g.end_call()
			return true
		}
		'log2' {
			g.begin_call('math', 'log2')
			if node.args.len > 0 {
				g.core_expr(node.args[0].expr)
			}
			g.end_call()
			return true
		}
		else {
			return false
		}
	}
}

fn (mut g CoreGen) core_builtin_math_2(name string, node ast.CallExpr) bool {
	match name {
		'log10' {
			g.begin_call('math', 'log10')
			if node.args.len > 0 {
				g.core_expr(node.args[0].expr)
			}
			g.end_call()
			return true
		}
		'pow' {
			// V math.pow(a, b) -> math:pow(A, B)
			g.begin_call('math', 'pow')
			if node.args.len > 0 {
				g.core_expr(node.args[0].expr)
			}
			if node.args.len > 1 {
				g.emit_sep()
				g.core_expr(node.args[1].expr)
			}
			g.end_call()
			return true
		}
		'ceil' {
			g.begin_call('math', 'ceil')
			if node.args.len > 0 {
				g.core_expr(node.args[0].expr)
			}
			g.end_call()
			return true
		}
		'floor' {
			g.begin_call('math', 'floor')
			if node.args.len > 0 {
				g.core_expr(node.args[0].expr)
			}
			g.end_call()
			return true
		}
		else {
			return false
		}
	}
}

fn (mut g CoreGen) core_builtin_math_3(name string, node ast.CallExpr) bool {
	match name {
		'sin' {
			g.begin_call('math', 'sin')
			if node.args.len > 0 {
				g.core_expr(node.args[0].expr)
			}
			g.end_call()
			return true
		}
		'cos' {
			g.begin_call('math', 'cos')
			if node.args.len > 0 {
				g.core_expr(node.args[0].expr)
			}
			g.end_call()
			return true
		}
		'tan' {
			g.begin_call('math', 'tan')
			if node.args.len > 0 {
				g.core_expr(node.args[0].expr)
			}
			g.end_call()
			return true
		}
		'asin' {
			g.begin_call('math', 'asin')
			if node.args.len > 0 {
				g.core_expr(node.args[0].expr)
			}
			g.end_call()
			return true
		}
		'acos' {
			g.begin_call('math', 'acos')
			if node.args.len > 0 {
				g.core_expr(node.args[0].expr)
			}
			g.end_call()
			return true
		}
		else {
			return false
		}
	}
}

fn (mut g CoreGen) core_builtin_math_4(name string, node ast.CallExpr) bool {
	match name {
		'atan' {
			g.begin_call('math', 'atan')
			if node.args.len > 0 {
				g.core_expr(node.args[0].expr)
			}
			g.end_call()
			return true
		}
		'atan2' {
			g.begin_call('math', 'atan2')
			if node.args.len > 0 {
				g.core_expr(node.args[0].expr)
			}
			if node.args.len > 1 {
				g.emit_sep()
				g.core_expr(node.args[1].expr)
			}
			g.end_call()
			return true
		}
		'exp' {
			g.begin_call('math', 'exp')
			if node.args.len > 0 {
				g.core_expr(node.args[0].expr)
			}
			g.end_call()
			return true
		}
		else {
			return false
		}
	}
}

fn (mut g CoreGen) core_builtin_system(name string, node ast.CallExpr) bool {
	return g.core_builtin_system_1(name, node) || g.core_builtin_system_2(name, node)
}

fn (mut g CoreGen) core_builtin_system_1(name string, node ast.CallExpr) bool {
	match name {
		'exit' {
			// V exit(code) -> erlang:halt(Code)
			g.begin_call('erlang', 'halt')
			if node.args.len > 0 {
				g.core_expr(node.args[0].expr)
			} else {
				g.emit_int('0')
			}
			g.end_call()
			return true
		}
		'panic' {
			// V panic(msg) -> erlang:error({panic, Msg})
			if g.etf_mode {
				g.begin_call('erlang', 'error')
				g.begin_tuple()
				g.emit_atom('panic')
				g.emit_sep()
				if node.args.len > 0 {
					g.core_expr(node.args[0].expr)
				} else {
					g.emit_atom('panic')
				}
				g.end_tuple()
				g.end_call()
			} else {
				g.write_core("call 'erlang':'error'({'panic', ")
				if node.args.len > 0 {
					g.core_expr(node.args[0].expr)
				} else {
					g.write_core("'panic'")
				}
				g.write_core('})')
			}
			return true
		}
		else {
			return false
		}
	}
}

fn (mut g CoreGen) core_builtin_system_2(name string, node ast.CallExpr) bool {
	match name {
		'sleep' {
			// V sleep(duration) -> timer:sleep(Ms)
			g.begin_call('timer', 'sleep')
			if node.args.len > 0 {
				g.core_expr(node.args[0].expr)
			} else {
				g.emit_int('0')
			}
			g.end_call()
			return true
		}
		'arguments' {
			// V os.args -> init:get_plain_arguments()
			g.begin_call('init', 'get_plain_arguments')
			g.end_call()
			return true
		}
		'getpid' {
			// V os.getpid() -> list_to_integer(os:getpid())
			// os:getpid() returns a charlist of the OS PID
			if g.etf_mode {
				g.begin_call('erlang', 'list_to_integer')
				g.begin_call('os', 'getpid')
				g.end_call()
				g.end_call()
			} else {
				g.write_core("call 'erlang':'list_to_integer'(call 'os':'getpid'())")
			}
			return true
		}
		'signal_opt' {
			// V os.signal_opt(sig, handler) -> ok
			g.emit_atom('ok')
			return true
		}
		'signal' {
			// V os.signal(sig, handler) -> ok
			g.emit_atom('ok')
			return true
		}
		else {
			return false
		}
	}
}

fn (mut g CoreGen) core_builtin_os(name string, node ast.CallExpr) bool {
	return g.core_builtin_os_1(name, node) || g.core_builtin_os_2(name, node)
		|| g.core_builtin_os_3(name, node) || g.core_builtin_os_4(name, node)
		|| g.core_builtin_os_5(name, node) || g.core_builtin_os_6(name, node)
		|| g.core_builtin_os_7(name, node) || g.core_builtin_os_8(name, node)
		|| g.core_builtin_os_9(name, node) || g.core_builtin_os_10(name, node)
		|| g.core_builtin_os_11(name, node) || g.core_builtin_os_12(name, node)
		|| g.core_builtin_os_13(name, node)
}

fn (mut g CoreGen) core_builtin_os_1(name string, node ast.CallExpr) bool {
	match name {
		'join_path' {
			// V os.join_path(a, b) -> filename:join(A, B)
			g.begin_call('filename', 'join')
			if node.args.len > 0 {
				g.core_expr(node.args[0].expr)
			}
			if node.args.len > 1 {
				g.emit_sep()
				g.core_expr(node.args[1].expr)
			}
			g.end_call()
			return true
		}
		'temp_dir' {
			// V os.temp_dir() -> '/tmp'
			if g.etf_mode {
				g.emit_binary('/tmp')
			} else {
				g.write_core(core_bitstring('/tmp'))
			}
			return true
		}
		else {
			return false
		}
	}
}

fn (mut g CoreGen) core_builtin_os_2(name string, node ast.CallExpr) bool {
	match name {
		'getenv' {
			// V os.getenv(name) -> os:getenv(Name)
			// os:getenv returns charlist or false; we need to handle the false case
			// Use case to convert: false -> <<>> (empty binary), charlist -> list_to_binary
			if g.etf_mode {
				tmp := g.new_temp()
				g.begin_let(tmp)
				g.begin_call('os', 'getenv')
				g.begin_call('erlang', 'binary_to_list')
				if node.args.len > 0 {
					g.core_expr(node.args[0].expr)
				}
				g.end_call()
				g.end_call()
				g.mid_let()
				g.begin_case()
				g.emit_var(tmp)
				g.mid_case_clauses()
				// false clause -> empty binary
				g.begin_clause()
				g.emit_atom('false')
				g.mid_clause_guard()
				g.emit_true_guard()
				g.mid_clause_body()
				g.emit_binary('')
				g.end_clause()
				g.clause_sep()
				// charlist clause -> convert to binary
				tmp2 := g.new_temp()
				g.begin_clause()
				g.emit_var(tmp2)
				g.mid_clause_guard()
				g.emit_true_guard()
				g.mid_clause_body()
				g.begin_call('erlang', 'list_to_binary')
				g.emit_var(tmp2)
				g.end_call()
				g.end_clause()
				g.end_case()
				g.end_let()
			} else {
				tmp := g.new_temp()
				tmp2 := g.new_temp()
				g.write_core("let <${tmp}> = call 'os':'getenv'(call 'erlang':'binary_to_list'(")
				if node.args.len > 0 {
					g.core_expr(node.args[0].expr)
				}
				g.write_core(")) in case ${tmp} of <'false'> when 'true' -> ")
				g.write_core(core_bitstring(''))
				g.write_core(" <${tmp2}> when 'true' -> call 'erlang':'list_to_binary'(${tmp2}) end")
			}
			return true
		}
		else {
			return false
		}
	}
}

fn (mut g CoreGen) core_builtin_os_3(name string, node ast.CallExpr) bool {
	match name {
		'setenv' {
			// V os.setenv(key, val, overwrite) -> os:putenv(Key, Val)
			// os:putenv takes charlists, so convert binaries first
			if g.etf_mode {
				g.begin_call('os', 'putenv')
				g.begin_call('erlang', 'binary_to_list')
				if node.args.len > 0 {
					g.core_expr(node.args[0].expr)
				}
				g.end_call()
				g.emit_sep()
				g.begin_call('erlang', 'binary_to_list')
				if node.args.len > 1 {
					g.core_expr(node.args[1].expr)
				}
				g.end_call()
				g.end_call()
			} else {
				g.write_core("call 'os':'putenv'(call 'erlang':'binary_to_list'(")
				if node.args.len > 0 {
					g.core_expr(node.args[0].expr)
				}
				g.write_core("), call 'erlang':'binary_to_list'(")
				if node.args.len > 1 {
					g.core_expr(node.args[1].expr)
				}
				g.write_core('))')
			}
			return true
		}
		else {
			return false
		}
	}
}

fn (mut g CoreGen) core_builtin_os_4(name string, node ast.CallExpr) bool {
	match name {
		'unsetenv' {
			// V os.unsetenv(name) -> os:unsetenv(Name)
			if g.etf_mode {
				g.begin_call('os', 'unsetenv')
				g.begin_call('erlang', 'binary_to_list')
				if node.args.len > 0 {
					g.core_expr(node.args[0].expr)
				}
				g.end_call()
				g.end_call()
			} else {
				g.write_core("call 'os':'unsetenv'(call 'erlang':'binary_to_list'(")
				if node.args.len > 0 {
					g.core_expr(node.args[0].expr)
				}
				g.write_core('))')
			}
			return true
		}
		'getwd' {
			// V os.getwd() -> element(2, file:get_cwd())
			// file:get_cwd() returns {ok, Dir} where Dir is a charlist
			// We extract Dir and convert to binary
			if g.etf_mode {
				g.begin_call('erlang', 'list_to_binary')
				g.begin_call('erlang', 'element')
				g.emit_int('2')
				g.emit_sep()
				g.begin_call('file', 'get_cwd')
				g.end_call()
				g.end_call()
				g.end_call()
			} else {
				g.write_core("call 'erlang':'list_to_binary'(call 'erlang':'element'(2, call 'file':'get_cwd'()))")
			}
			return true
		}
		else {
			return false
		}
	}
}

fn (mut g CoreGen) core_builtin_os_5(name string, node ast.CallExpr) bool {
	match name {
		'mkdir' {
			// V os.mkdir(path) -> file:make_dir(binary_to_list(Path))
			if g.etf_mode {
				g.begin_call('file', 'make_dir')
				g.begin_call('erlang', 'binary_to_list')
				if node.args.len > 0 {
					g.core_expr(node.args[0].expr)
				}
				g.end_call()
				g.end_call()
			} else {
				g.write_core("call 'file':'make_dir'(call 'erlang':'binary_to_list'(")
				if node.args.len > 0 {
					g.core_expr(node.args[0].expr)
				}
				g.write_core('))')
			}
			return true
		}
		'rmdir' {
			// V os.rmdir(path) -> file:del_dir(binary_to_list(Path))
			if g.etf_mode {
				g.begin_call('file', 'del_dir')
				g.begin_call('erlang', 'binary_to_list')
				if node.args.len > 0 {
					g.core_expr(node.args[0].expr)
				}
				g.end_call()
				g.end_call()
			} else {
				g.write_core("call 'file':'del_dir'(call 'erlang':'binary_to_list'(")
				if node.args.len > 0 {
					g.core_expr(node.args[0].expr)
				}
				g.write_core('))')
			}
			return true
		}
		else {
			return false
		}
	}
}

fn (mut g CoreGen) core_builtin_os_6(name string, node ast.CallExpr) bool {
	match name {
		'chdir' {
			// V os.chdir(path) -> file:set_cwd(binary_to_list(Path))
			if g.etf_mode {
				g.begin_call('file', 'set_cwd')
				g.begin_call('erlang', 'binary_to_list')
				if node.args.len > 0 {
					g.core_expr(node.args[0].expr)
				}
				g.end_call()
				g.end_call()
			} else {
				g.write_core("call 'file':'set_cwd'(call 'erlang':'binary_to_list'(")
				if node.args.len > 0 {
					g.core_expr(node.args[0].expr)
				}
				g.write_core('))')
			}
			return true
		}
		else {
			return false
		}
	}
}

fn (mut g CoreGen) core_builtin_os_7(name string, node ast.CallExpr) bool {
	match name {
		'rename' {
			// V os.rename(src, dst) -> file:rename(Src, Dst)
			if g.etf_mode {
				g.begin_call('file', 'rename')
				g.begin_call('erlang', 'binary_to_list')
				if node.args.len > 0 {
					g.core_expr(node.args[0].expr)
				}
				g.end_call()
				g.emit_sep()
				g.begin_call('erlang', 'binary_to_list')
				if node.args.len > 1 {
					g.core_expr(node.args[1].expr)
				}
				g.end_call()
				g.end_call()
			} else {
				g.write_core("call 'file':'rename'(call 'erlang':'binary_to_list'(")
				if node.args.len > 0 {
					g.core_expr(node.args[0].expr)
				}
				g.write_core("), call 'erlang':'binary_to_list'(")
				if node.args.len > 1 {
					g.core_expr(node.args[1].expr)
				}
				g.write_core('))')
			}
			return true
		}
		'home_dir' {
			// V os.home_dir() -> os:getenv("HOME")
			if g.etf_mode {
				g.begin_call('erlang', 'list_to_binary')
				g.begin_call('os', 'getenv')
				g.emit_charlist('HOME')
				g.end_call()
				g.end_call()
			} else {
				g.write_core("call 'erlang':'list_to_binary'(call 'os':'getenv'(${core_charlist('HOME')}))")
			}
			return true
		}
		else {
			return false
		}
	}
}

fn (mut g CoreGen) core_builtin_os_8(name string, node ast.CallExpr) bool {
	match name {
		'ls' {
			// V os.ls(path) -> file:list_dir(binary_to_list(Path))
			if g.etf_mode {
				g.begin_call('file', 'list_dir')
				g.begin_call('erlang', 'binary_to_list')
				if node.args.len > 0 {
					g.core_expr(node.args[0].expr)
				}
				g.end_call()
				g.end_call()
			} else {
				g.write_core("call 'file':'list_dir'(call 'erlang':'binary_to_list'(")
				if node.args.len > 0 {
					g.core_expr(node.args[0].expr)
				}
				g.write_core('))')
			}
			return true
		}
		'rm' {
			// V os.rm(path) -> file:delete(Path)
			g.begin_call('file', 'delete')
			if node.args.len > 0 {
				g.core_expr(node.args[0].expr)
			}
			g.end_call()
			return true
		}
		'input_password' {
			// V os.input_password(prompt) -> io:get_password()
			g.begin_call('io', 'get_password')
			g.end_call()
			return true
		}
		else {
			return false
		}
	}
}

fn (mut g CoreGen) core_builtin_os_9(name string, node ast.CallExpr) bool {
	match name {
		'find_abs_path_of_executable' {
			// V os.find_abs_path_of_executable(name) -> os:find_executable
			if g.etf_mode {
				g.begin_call('erlang', 'list_to_binary')
				g.begin_call('os', 'find_executable')
				g.begin_call('erlang', 'binary_to_list')
				if node.args.len > 0 {
					g.core_expr(node.args[0].expr)
				}
				g.end_call()
				g.end_call()
				g.end_call()
			} else {
				g.write_core("call 'erlang':'list_to_binary'(call 'os':'find_executable'(call 'erlang':'binary_to_list'(")
				if node.args.len > 0 {
					g.core_expr(node.args[0].expr)
				}
				g.write_core(')))')
			}
			return true
		}
		'quoted_path' {
			// V os.quoted_path(p) -> just return the path
			if node.args.len > 0 {
				g.core_expr(node.args[0].expr)
			}
			return true
		}
		'execve' {
			// V os.execve(cmd, args, env) -> os:cmd placeholder
			g.emit_atom('ok')
			return true
		}
		'at_exit' {
			// V os.at_exit(fn) -> 'ok' (no-op on BEAM)
			g.emit_atom('ok')
			return true
		}
		else {
			return false
		}
	}
}

fn (mut g CoreGen) core_builtin_os_10(name string, node ast.CallExpr) bool {
	match name {
		'resource_abs_path' {
			// V resource path -> just return the argument
			if node.args.len > 0 {
				g.core_expr(node.args[0].expr)
			} else {
				g.emit_binary('.')
			}
			return true
		}
		'is_file' {
			// V os.is_file(path) -> filelib:is_file
			if g.etf_mode {
				g.begin_call('filelib', 'is_file')
				g.begin_call('erlang', 'binary_to_list')
				if node.args.len > 0 {
					g.core_expr(node.args[0].expr)
				}
				g.end_call()
				g.end_call()
			} else {
				g.write_core("call 'filelib':'is_file'(call 'erlang':'binary_to_list'(")
				if node.args.len > 0 {
					g.core_expr(node.args[0].expr)
				}
				g.write_core('))')
			}
			return true
		}
		else {
			return false
		}
	}
}

fn (mut g CoreGen) core_builtin_os_11(name string, node ast.CallExpr) bool {
	match name {
		'is_dir' {
			// V os.is_dir(path) -> filelib:is_dir
			if g.etf_mode {
				g.begin_call('filelib', 'is_dir')
				g.begin_call('erlang', 'binary_to_list')
				if node.args.len > 0 {
					g.core_expr(node.args[0].expr)
				}
				g.end_call()
				g.end_call()
			} else {
				g.write_core("call 'filelib':'is_dir'(call 'erlang':'binary_to_list'(")
				if node.args.len > 0 {
					g.core_expr(node.args[0].expr)
				}
				g.write_core('))')
			}
			return true
		}
		'exists' {
			// V os.exists(path) -> filelib:is_regular
			if g.etf_mode {
				g.begin_call('filelib', 'is_regular')
				g.begin_call('erlang', 'binary_to_list')
				if node.args.len > 0 {
					g.core_expr(node.args[0].expr)
				}
				g.end_call()
				g.end_call()
			} else {
				g.write_core("call 'filelib':'is_regular'(call 'erlang':'binary_to_list'(")
				if node.args.len > 0 {
					g.core_expr(node.args[0].expr)
				}
				g.write_core('))')
			}
			return true
		}
		'cp' {
			// V os.cp(src, dst, flags) -> file:copy placeholder
			g.emit_atom('ok')
			return true
		}
		else {
			return false
		}
	}
}

fn (mut g CoreGen) core_builtin_os_12(name string, node ast.CallExpr) bool {
	match name {
		'mkdir_all' {
			// V os.mkdir_all(path) -> filelib:ensure_dir
			if g.etf_mode {
				g.begin_call('filelib', 'ensure_dir')
				g.begin_call('erlang', 'binary_to_list')
				if node.args.len > 0 {
					g.core_expr(node.args[0].expr)
				}
				g.end_call()
				g.end_call()
			} else {
				g.write_core("call 'filelib':'ensure_dir'(call 'erlang':'binary_to_list'(")
				if node.args.len > 0 {
					g.core_expr(node.args[0].expr)
				}
				g.write_core('))')
			}
			return true
		}
		'file_size' {
			// V os.file_size(path) -> filelib:file_size
			if g.etf_mode {
				g.begin_call('filelib', 'file_size')
				g.begin_call('erlang', 'binary_to_list')
				if node.args.len > 0 {
					g.core_expr(node.args[0].expr)
				}
				g.end_call()
				g.end_call()
			} else {
				g.write_core("call 'filelib':'file_size'(call 'erlang':'binary_to_list'(")
				if node.args.len > 0 {
					g.core_expr(node.args[0].expr)
				}
				g.write_core('))')
			}
			return true
		}
		'stdin_fileno' {
			g.emit_int('0')
			return true
		}
		else {
			return false
		}
	}
}

fn (mut g CoreGen) core_builtin_os_13(name string, node ast.CallExpr) bool {
	match name {
		'stdout_fileno' {
			g.emit_int('1')
			return true
		}
		'stderr_fileno' {
			g.emit_int('2')
			return true
		}
		'mmap_file' {
			// V os.mmap_file -> placeholder
			g.emit_binary('')
			return true
		}
		else {
			return false
		}
	}
}

fn (mut g CoreGen) core_builtin_time(name string, node ast.CallExpr) bool {
	return g.core_builtin_time_1(name, node) || g.core_builtin_time_2(name, node)
}

fn (mut g CoreGen) core_builtin_time_1(name string, node ast.CallExpr) bool {
	match name {
		'ticks' {
			// V time.ticks() -> erlang:monotonic_time(millisecond)
			g.begin_call('erlang', 'monotonic_time')
			g.emit_atom('millisecond')
			g.end_call()
			return true
		}
		'utc' {
			// V time.utc() -> erlang:universaltime()
			// Returns {{Year,Month,Day},{Hour,Min,Sec}} tuple
			g.begin_call('erlang', 'universaltime')
			g.end_call()
			return true
		}
		'sys_mono_now' {
			// V time.sys_mono_now() -> erlang:monotonic_time(nanosecond)
			g.begin_call('erlang', 'monotonic_time')
			g.emit_atom('nanosecond')
			g.end_call()
			return true
		}
		'now' {
			// V time.now() -> erlang:system_time(microsecond)
			// Returns a large integer timestamp; Time struct construction handled by caller
			g.begin_call('erlang', 'system_time')
			g.emit_atom('microsecond')
			g.end_call()
			return true
		}
		else {
			return false
		}
	}
}

fn (mut g CoreGen) core_builtin_time_2(name string, node ast.CallExpr) bool {
	match name {
		'unix' {
			// V time.unix() -> erlang:system_time(second)
			// Returns Unix timestamp in seconds
			if !node.is_method && node.args.len == 0 {
				g.begin_call('erlang', 'system_time')
				g.emit_atom('second')
				g.end_call()
				return true
			}
			// If called with args (e.g. time.unix(ts) for struct construction), placeholder
			if g.etf_mode {
				g.emit_atom('undefined')
			} else {
				g.write_core("~{{'vbeam','type'}=>'Time'}~")
			}
			return true
		}
		'unix_milli' {
			// V time.unix_milli() -> erlang:system_time(millisecond)
			g.begin_call('erlang', 'system_time')
			g.emit_atom('millisecond')
			g.end_call()
			return true
		}
		else {
			return false
		}
	}
}

fn (mut g CoreGen) core_builtin_random(name string, node ast.CallExpr) bool {
	return g.core_builtin_random_1(name, node) || g.core_builtin_random_2(name, node)
		|| g.core_builtin_random_3(name, node)
}

fn (mut g CoreGen) core_builtin_random_1(name string, node ast.CallExpr) bool {
	match name {
		'intn' {
			// V rand.intn(n) -> rand:uniform(N)
			g.begin_call('rand', 'uniform')
			if node.args.len > 0 {
				g.core_expr(node.args[0].expr)
			}
			g.end_call()
			return true
		}
		'int' {
			// V rand.int() -> rand:uniform(2147483647)
			if !node.is_method && node.args.len == 0 {
				g.begin_call('rand', 'uniform')
				g.emit_int('2147483647')
				g.end_call()
				return true
			}
			return false
		}
		'seed' {
			// V rand.seed(s) -> rand:seed(exsss, [S])
			if g.etf_mode {
				g.begin_call('rand', 'seed')
				g.emit_atom('exsss')
				g.emit_sep()
				g.begin_cons()
				if node.args.len > 0 {
					g.core_expr(node.args[0].expr)
				}
				g.mid_cons()
				g.emit_nil()
				g.end_cons()
				g.end_call()
			} else {
				g.write_core("call 'rand':'seed'('exsss', [")
				if node.args.len > 0 {
					g.core_expr(node.args[0].expr)
				}
				g.write_core('|[]])')
			}
			return true
		}
		else {
			return false
		}
	}
}

fn (mut g CoreGen) core_builtin_random_2(name string, node ast.CallExpr) bool {
	match name {
		'f64' {
			// V rand.f64() -> rand:uniform()
			g.begin_call('rand', 'uniform')
			g.end_call()
			return true
		}
		'int_in_range' {
			// V rand.int_in_range(low, high) -> rand:uniform(high-low) + low
			if g.etf_mode {
				g.begin_call('erlang', '+')
				g.begin_call('rand', 'uniform')
				g.begin_call('erlang', '-')
				if node.args.len > 1 {
					g.core_expr(node.args[1].expr)
					g.emit_sep()
					g.core_expr(node.args[0].expr)
				}
				g.end_call()
				g.end_call()
				g.emit_sep()
				if node.args.len > 0 {
					g.core_expr(node.args[0].expr)
				}
				g.end_call()
			} else {
				g.write_core("call 'erlang':'+'(call 'rand':'uniform'(call 'erlang':'-'(")
				if node.args.len > 1 {
					g.core_expr(node.args[1].expr)
					g.write_core(', ')
					g.core_expr(node.args[0].expr)
				}
				g.write_core(')), ')
				if node.args.len > 0 {
					g.core_expr(node.args[0].expr)
				}
				g.write_core(')')
			}
			return true
		}
		else {
			return false
		}
	}
}

fn (mut g CoreGen) core_builtin_random_3(name string, node ast.CallExpr) bool {
	match name {
		'generate' {
			// V lorem.generate -> placeholder
			g.emit_binary('Lorem ipsum dolor sit amet')
			return true
		}
		'u32' {
			// V rand.u32() or type cast
			if !node.is_method && node.args.len == 0 {
				g.begin_call('rand', 'uniform')
				g.emit_int('4294967295')
				g.end_call()
				return true
			}
			return false
		}
		'f32' {
			// V rand.f32() or type cast
			if !node.is_method && node.args.len == 0 {
				g.begin_call('rand', 'uniform')
				g.end_call()
				return true
			}
			return false
		}
		else {
			return false
		}
	}
}

fn (mut g CoreGen) core_builtin_io(name string, node ast.CallExpr) bool {
	return g.core_builtin_io_1(name, node) || g.core_builtin_io_2(name, node)
		|| g.core_builtin_io_3(name, node) || g.core_builtin_io_4(name, node)
}

fn (mut g CoreGen) core_builtin_io_1(name string, node ast.CallExpr) bool {
	match name {
		'write_file' {
			// V os.write_file(path, content) -> file:write_file(Path, Content)
			g.begin_call('file', 'write_file')
			if node.args.len > 0 {
				g.core_expr(node.args[0].expr)
			}
			if node.args.len > 1 {
				g.emit_sep()
				g.core_expr(node.args[1].expr)
			}
			g.end_call()
			return true
		}
		'read_file' {
			// V os.read_file(path) -> file:read_file(Path)
			g.begin_call('file', 'read_file')
			if node.args.len > 0 {
				g.core_expr(node.args[0].expr)
			}
			g.end_call()
			return true
		}
		else {
			return false
		}
	}
}

fn (mut g CoreGen) core_builtin_io_2(name string, node ast.CallExpr) bool {
	match name {
		'read_lines' {
			// V os.read_lines(path) -> read file and split by newline
			if g.etf_mode {
				g.begin_call('binary', 'split')
				g.begin_call('erlang', 'element')
				g.emit_int('2')
				g.emit_sep()
				g.begin_call('file', 'read_file')
				if node.args.len > 0 {
					g.core_expr(node.args[0].expr)
				}
				g.end_call()
				g.end_call()
				g.emit_sep()
				g.emit_binary('\n')
				g.emit_sep()
				g.begin_cons()
				g.emit_atom('global')
				g.mid_cons()
				g.emit_nil()
				g.end_cons()
				g.end_call()
			} else {
				g.write_core("call 'binary':'split'(call 'erlang':'element'(2, call 'file':'read_file'(")
				if node.args.len > 0 {
					g.core_expr(node.args[0].expr)
				}
				g.write_core(")), ${core_bitstring('\n')}, ['global'|[]])")
			}
			return true
		}
		else {
			return false
		}
	}
}

fn (mut g CoreGen) core_builtin_io_3(name string, node ast.CallExpr) bool {
	match name {
		'open_file' {
			// V os.open_file(path, mode) -> file:open(Path, Modes)
			if g.etf_mode {
				g.begin_tuple()
				g.emit_atom('ok')
				g.emit_sep()
				g.emit_atom('undefined')
				g.end_tuple()
			} else {
				g.write_core("{'ok', ~{{'vbeam','type'}=>'File'}~}")
			}
			return true
		}
		'create' {
			// V os.create(path) -> file:open placeholder
			if g.etf_mode {
				g.begin_tuple()
				g.emit_atom('ok')
				g.emit_sep()
				g.emit_atom('undefined')
				g.end_tuple()
			} else {
				g.write_core("{'ok', ~{{'vbeam','type'}=>'File'}~}")
			}
			return true
		}
		'open' {
			// V os.open(path) -> file:open placeholder
			if g.etf_mode {
				g.begin_tuple()
				g.emit_atom('ok')
				g.emit_sep()
				g.emit_atom('undefined')
				g.end_tuple()
			} else {
				g.write_core("{'ok', ~{{'vbeam','type'}=>'File'}~}")
			}
			return true
		}
		else {
			return false
		}
	}
}

fn (mut g CoreGen) core_builtin_io_4(name string, node ast.CallExpr) bool {
	match name {
		'read_bytes' {
			// V os.read_bytes(path) -> file:read_file
			g.begin_call('file', 'read_file')
			if node.args.len > 0 {
				g.core_expr(node.args[0].expr)
			}
			g.end_call()
			return true
		}
		else {
			return false
		}
	}
}

fn (mut g CoreGen) core_builtin_string_conv(name string, node ast.CallExpr) bool {
	return g.core_builtin_string_conv_1(name, node) || g.core_builtin_string_conv_2(name, node)
		|| g.core_builtin_string_conv_3(name, node)
}

fn (mut g CoreGen) core_builtin_string_conv_1(name string, node ast.CallExpr) bool {
	match name {
		'dump' {
			// V dump(expr) -> just pass through the expression
			if node.args.len > 0 {
				g.core_expr(node.args[0].expr)
			} else {
				g.emit_atom('false')
			}
			return true
		}
		'integer_from_int' {
			// V math.big.integer_from_int(n) -> just use the integer directly
			if node.args.len > 0 {
				g.core_expr(node.args[0].expr)
			}
			return true
		}
		'atoi' {
			// V strconv.atoi(s) -> binary_to_integer(S)
			g.begin_call('erlang', 'binary_to_integer')
			if node.args.len > 0 {
				g.core_expr(node.args[0].expr)
			}
			g.end_call()
			return true
		}
		'from' {
			// V IError.from(x) / error(msg) -> {error, Msg}
			g.begin_tuple()
			g.emit_atom('error')
			g.emit_sep()
			if node.args.len > 0 {
				g.core_expr(node.args[0].expr)
			} else {
				g.emit_atom('error')
			}
			g.end_tuple()
			return true
		}
		else {
			return false
		}
	}
}

fn (mut g CoreGen) core_builtin_string_conv_2(name string, node ast.CallExpr) bool {
	match name {
		'atof64' {
			// V strconv.atof64(s) -> binary_to_float
			g.begin_call('erlang', 'binary_to_float')
			if node.args.len > 0 {
				g.core_expr(node.args[0].expr)
			}
			g.end_call()
			return true
		}
		'tos' {
			// V tos(s, len) -> binary:part(S, 0, Len)
			// On BEAM: take the first `len` bytes of the binary
			if node.args.len >= 2 {
				g.begin_call('binary', 'part')
				g.core_expr(node.args[0].expr)
				g.emit_sep()
				g.emit_int('0')
				g.emit_sep()
				g.core_expr(node.args[1].expr)
				g.end_call()
				return true
			}
			return false
		}
		'tos2', 'tos3', 'tos4', 'tos5', 'cstring_to_vstring' {
			// V tos2(s) / cstring_to_vstring(s) -> scan for null byte, return prefix
			// On BEAM: hd(binary:split(S, <<0>>)) — finds null terminator in binary
			// If no null found, binary:split returns [S], so hd gives S unchanged
			if node.args.len >= 1 {
				g.begin_call('erlang', 'hd')
				g.begin_call('binary', 'split')
				g.core_expr(node.args[0].expr)
				g.emit_sep()
				g.emit_binary('\x00')
				g.end_call()
				g.end_call()
				return true
			}
			return false
		}
		else {
			return false
		}
	}
}

fn (mut g CoreGen) core_builtin_string_conv_3(name string, node ast.CallExpr) bool {
	match name {
		'tos_clone' {
			// V tos_clone(s) -> binary:copy(S)
			// On BEAM: creates an independent copy of the binary
			if node.args.len >= 1 {
				g.begin_call('binary', 'copy')
				g.core_expr(node.args[0].expr)
				g.end_call()
				return true
			}
			return false
		}
		else {
			return false
		}
	}
}

fn (mut g CoreGen) core_builtin_terminal(name string, node ast.CallExpr) bool {
	return g.core_builtin_terminal_1(name, node) || g.core_builtin_terminal_2(name, node)
		|| g.core_builtin_terminal_3(name, node)
}

fn (mut g CoreGen) core_builtin_terminal_1(name string, node ast.CallExpr) bool {
	match name {
		'colorize' {
			// V term.colorize(str, color) -> just return string
			if node.args.len > 0 {
				g.core_expr(node.args[0].expr)
			} else {
				g.emit_binary('')
			}
			return true
		}
		'bold' {
			// V term.bold(str) -> just return string
			if node.args.len > 0 {
				g.core_expr(node.args[0].expr)
			} else {
				g.emit_binary('')
			}
			return true
		}
		'green' {
			// V term.green(s) -> just return string
			if node.args.len > 0 {
				g.core_expr(node.args[0].expr)
			} else {
				g.emit_binary('')
			}
			return true
		}
		'red' {
			// V term.red(s) -> just return string
			if node.args.len > 0 {
				g.core_expr(node.args[0].expr)
			} else {
				g.emit_binary('')
			}
			return true
		}
		else {
			return false
		}
	}
}

fn (mut g CoreGen) core_builtin_terminal_2(name string, node ast.CallExpr) bool {
	match name {
		'yellow' {
			// V term.yellow(s) -> just return string
			if node.args.len > 0 {
				g.core_expr(node.args[0].expr)
			} else {
				g.emit_binary('')
			}
			return true
		}
		'cursor_up' {
			// V term.cursor_up(n) -> escape seq placeholder
			g.emit_binary('')
			return true
		}
		'cursor_down' {
			g.emit_binary('')
			return true
		}
		'cursor_forward' {
			g.emit_binary('')
			return true
		}
		'cursor_back' {
			g.emit_binary('')
			return true
		}
		'clear' {
			// V term.clear() -> placeholder
			g.emit_binary('')
			return true
		}
		'supports_sixel' {
			// V term detection -> false
			g.emit_atom('false')
			return true
		}
		'erase_clear' {
			// V term.erase_clear() -> placeholder
			g.emit_binary('')
			return true
		}
		else {
			return false
		}
	}
}

fn (mut g CoreGen) core_builtin_terminal_3(name string, node ast.CallExpr) bool {
	match name {
		'dim' {
			// V terminal dimension -> placeholder
			g.emit_int('80')
			return true
		}
		'set_terminal_title' {
			g.emit_atom('ok')
			return true
		}
		'gray' {
			// V term.gray(s) -> just return string
			if node.args.len > 0 {
				g.core_expr(node.args[0].expr)
			} else {
				g.emit_binary('')
			}
			return true
		}
		'set_cursor_position' {
			// V term.set_cursor_position(pos) -> placeholder
			g.emit_atom('ok')
			return true
		}
		'white' {
			// V term.white(s) -> just return string
			if node.args.len > 0 {
				g.core_expr(node.args[0].expr)
			} else {
				g.emit_binary('')
			}
			return true
		}
		'to_doc' {
			// V gg.to_doc -> placeholder
			g.emit_binary('')
			return true
		}
		else {
			return false
		}
	}
}

fn (mut g CoreGen) core_builtin_net(name string, node ast.CallExpr) bool {
	return g.core_builtin_net_1(name, node) || g.core_builtin_net_2(name, node)
		|| g.core_builtin_net_3(name, node)
}

fn (mut g CoreGen) core_builtin_net_1(name string, node ast.CallExpr) bool {
	match name {
		'get_text' {
			// V http.get_text(url) -> pass through as placeholder
			g.emit_binary('')
			return true
		}
		'new_request' {
			// V http.new_request(method, url, body) -> placeholder
			if g.etf_mode {
				g.emit_atom('undefined')
			} else {
				g.write_core("~{{'vbeam','type'}=>'Request'}~")
			}
			return true
		}
		'fetch' {
			// V http.fetch(url) -> placeholder tuple
			if g.etf_mode {
				g.begin_tuple()
				g.emit_atom('ok')
				g.emit_sep()
				g.emit_binary('')
				g.end_tuple()
			} else {
				g.write_core("{'ok', ~{'text'=>")
				g.write_core(core_bitstring(''))
				g.write_core(",{'vbeam','type'}=>'Response'}~}")
			}
			return true
		}
		'dial_tcp' {
			// V net.dial_tcp(addr) -> gen_tcp:connect placeholder
			g.begin_tuple()
			g.emit_atom('ok')
			g.emit_sep()
			g.emit_atom('false')
			g.end_tuple()
			return true
		}
		else {
			return false
		}
	}
}

fn (mut g CoreGen) core_builtin_net_2(name string, node ast.CallExpr) bool {
	match name {
		'listen_tcp' {
			// V net.listen_tcp(addr) -> gen_tcp:listen placeholder
			g.begin_tuple()
			g.emit_atom('ok')
			g.emit_sep()
			g.emit_atom('false')
			g.end_tuple()
			return true
		}
		'dial_udp' {
			// V net.dial_udp -> placeholder
			g.begin_tuple()
			g.emit_atom('ok')
			g.emit_sep()
			g.emit_atom('false')
			g.end_tuple()
			return true
		}
		'new_header_from_map' {
			// V http.new_header_from_map(map) -> placeholder
			if g.etf_mode {
				g.emit_atom('undefined')
			} else {
				g.write_core("~{{'vbeam','type'}=>'Header'}~")
			}
			return true
		}
		'listen_udp' {
			// V net.listen_udp(addr) -> placeholder
			g.begin_tuple()
			g.emit_atom('ok')
			g.emit_sep()
			g.emit_atom('false')
			g.end_tuple()
			return true
		}
		else {
			return false
		}
	}
}

fn (mut g CoreGen) core_builtin_net_3(name string, node ast.CallExpr) bool {
	match name {
		'new_client' {
			// V jsonrpc.new_client -> placeholder
			if g.etf_mode {
				g.emit_atom('undefined')
			} else {
				g.write_core("~{{'vbeam','type'}=>'Client'}~")
			}
			return true
		}
		else {
			return false
		}
	}
}

fn (mut g CoreGen) core_builtin_concurrency(name string, node ast.CallExpr) bool {
	return g.core_builtin_concurrency_1(name, node)
}

fn (mut g CoreGen) core_builtin_concurrency_1(name string, node ast.CallExpr) bool {
	match name {
		'new_waitgroup' {
			// V sync.new_waitgroup() -> just return 0 (counter)
			g.emit_int('0')
			return true
		}
		'new_mutex' {
			// V sync.new_mutex() -> placeholder
			if g.etf_mode {
				g.emit_atom('undefined')
			} else {
				g.write_core("~{{'vbeam','type'}=>'Mutex'}~")
			}
			return true
		}
		'new_rwmutex' {
			// V sync.new_rwmutex() -> placeholder
			if g.etf_mode {
				g.emit_atom('undefined')
			} else {
				g.write_core("~{{'vbeam','type'}=>'RwMutex'}~")
			}
			return true
		}
		'new_channel' {
			// V sync.new_channel(T) -> vbeam_concurrency:channel_new()
			g.begin_call('vbeam_concurrency', 'channel_new')
			g.end_call()
			return true
		}
		'new_pool_processor' {
			// V sync.new_pool_processor() -> placeholder
			if g.etf_mode {
				g.emit_atom('undefined')
			} else {
				g.write_core("~{{'vbeam','type'}=>'PoolProcessor'}~")
			}
			return true
		}
		else {
			return false
		}
	}
}

fn (mut g CoreGen) core_builtin_encoding(name string, node ast.CallExpr) bool {
	return g.core_builtin_encoding_1(name, node) || g.core_builtin_encoding_2(name, node)
}

fn (mut g CoreGen) core_builtin_encoding_1(name string, node ast.CallExpr) bool {
	match name {
		'decode' {
			// V json.decode(type, str) or vmod.decode(str) -> pass through
			if node.args.len > 0 {
				g.core_expr(node.args[node.args.len - 1].expr)
			} else {
				g.emit_atom('false')
			}
			return true
		}
		'encode' {
			// V json.encode(obj) -> term_to_binary(Obj)
			g.begin_call('erlang', 'term_to_binary')
			if node.args.len > 0 {
				g.core_expr(node.args[0].expr)
			}
			g.end_call()
			return true
		}
		'encode_pretty' {
			// V json.encode_pretty(obj) -> term_to_binary (placeholder)
			g.begin_call('erlang', 'term_to_binary')
			if node.args.len > 0 {
				g.core_expr(node.args[0].expr)
			}
			g.end_call()
			return true
		}
		'sha256' {
			// V crypto.sha256.sum(data) -> crypto:hash(sha256, Data)
			g.begin_call('crypto', 'hash')
			g.emit_atom('sha256')
			g.emit_sep()
			if node.args.len > 0 {
				g.core_expr(node.args[0].expr)
			}
			g.end_call()
			return true
		}
		else {
			return false
		}
	}
}

fn (mut g CoreGen) core_builtin_encoding_2(name string, node ast.CallExpr) bool {
	match name {
		'sum' {
			// V crypto hash sum -> crypto:hash
			g.begin_call('crypto', 'hash')
			g.emit_atom('sha256')
			g.emit_sep()
			if node.args.len > 0 {
				g.core_expr(node.args[0].expr)
			}
			g.end_call()
			return true
		}
		'diff' {
			// V diff.diff(a, b) -> placeholder empty string
			g.emit_binary('')
			return true
		}
		'json' {
			// V toml.json(t) -> placeholder
			g.emit_binary('{}')
			return true
		}
		'parse_text' {
			// V toml.parse_text(str) -> placeholder map
			if g.etf_mode {
				g.emit_atom('undefined')
			} else {
				g.write_core('~{}~')
			}
			return true
		}
		else {
			return false
		}
	}
}

fn (mut g CoreGen) core_builtin_misc(name string, node ast.CallExpr) bool {
	return g.core_builtin_misc_1(name, node) || g.core_builtin_misc_2(name, node)
		|| g.core_builtin_misc_3(name, node) || g.core_builtin_misc_4(name, node)
		|| g.core_builtin_misc_5(name, node)
}

fn (mut g CoreGen) core_builtin_misc_1(name string, node ast.CallExpr) bool {
	match name {
		'unbuffer_stdout' {
			// No-op on BEAM - stdout is already unbuffered
			g.emit_atom('ok')
			return true
		}
		'option' {
			// V cli.option() -> placeholder
			g.emit_atom('false')
			return true
		}
		'level_from_tag' {
			// V log.level_from_tag -> placeholder
			g.emit_atom('info')
			return true
		}
		'new_buffered_reader' {
			// V io.new_buffered_reader -> placeholder empty reader
			if g.etf_mode {
				g.emit_atom('undefined')
			} else {
				g.write_core("~{{'vbeam','type'}=>'BufferedReader'}~")
			}
			return true
		}
		'start_new_command' {
			// V process start -> placeholder
			g.emit_atom('ok')
			return true
		}
		'new_flag_parser' {
			// V flag.new_flag_parser(args) -> placeholder
			if g.etf_mode {
				g.emit_atom('undefined')
			} else {
				g.write_core("~{{'vbeam','type'}=>'FlagParser'}~")
			}
			return true
		}
		else {
			return false
		}
	}
}

fn (mut g CoreGen) core_builtin_misc_2(name string, node ast.CallExpr) bool {
	match name {
		'regex_opt' {
			// V regex.regex_opt(pattern) -> placeholder
			if g.etf_mode {
				g.emit_atom('undefined')
			} else {
				g.write_core("~{{'vbeam','type'}=>'RE'}~")
			}
			return true
		}
		'new_log' {
			// V log.new_log() -> placeholder
			if g.etf_mode {
				g.emit_atom('undefined')
			} else {
				g.write_core("~{{'vbeam','type'}=>'Log'}~")
			}
			return true
		}
		'new_event_bus' {
			// V eventbus.new() -> placeholder
			if g.etf_mode {
				g.emit_atom('undefined')
			} else {
				g.write_core("~{{'vbeam','type'}=>'EventBus'}~")
			}
			return true
		}
		'new' {
			// V Type.new() -> generic constructor placeholder
			if g.etf_mode {
				g.emit_atom('undefined')
			} else {
				g.write_core("~{{'vbeam','type'}=>'unknown'}~")
			}
			return true
		}
		'get_int' {
			// V cli.get_int(args, name) -> 0
			g.emit_int('0')
			return true
		}
		else {
			return false
		}
	}
}

fn (mut g CoreGen) core_builtin_misc_3(name string, node ast.CallExpr) bool {
	match name {
		'get_string' {
			// V cli.get_string(args, name) -> empty
			g.emit_binary('')
			return true
		}
		'do_work' {
			// V eventbus example do_work -> ok
			g.emit_atom('ok')
			return true
		}
		'read_all' {
			// V io.read_all(reader) -> placeholder empty binary
			g.emit_binary('')
			return true
		}
		'new_process' {
			// V os.new_process(cmd) -> placeholder Process struct
			if g.etf_mode {
				g.emit_atom('undefined')
			} else {
				g.write_core("~{{'vbeam','type'}=>'Process'}~")
			}
			return true
		}
		'get_subscriber' {
			// V eventbus.get_subscriber -> placeholder
			if g.etf_mode {
				g.emit_atom('undefined')
			} else {
				g.write_core("~{{'vbeam','type'}=>'Subscriber'}~")
			}
			return true
		}
		'debug' {
			// V log.debug(msg) -> 'ok'
			g.emit_atom('ok')
			return true
		}
		else {
			return false
		}
	}
}

fn (mut g CoreGen) core_builtin_misc_4(name string, node ast.CallExpr) bool {
	match name {
		'info' {
			// V log.info(msg) -> 'ok'
			if !node.is_method {
				g.emit_atom('ok')
				return true
			}
			return false
		}
		'warn' {
			// V log.warn(msg) -> 'ok'
			if !node.is_method {
				g.emit_atom('ok')
				return true
			}
			return false
		}
		'set_level' {
			// V log.set_level(lvl) -> 'ok'
			if !node.is_method {
				g.emit_atom('ok')
				return true
			}
			return false
		}
		'init' {
			// V gg.init / user init -> placeholder
			if !node.is_method {
				g.emit_atom('ok')
				return true
			}
			return false
		}
		'set_state' {
			// V terminal state setting -> ok
			g.emit_atom('ok')
			return true
		}
		else {
			return false
		}
	}
}

fn (mut g CoreGen) core_builtin_misc_5(name string, node ast.CallExpr) bool {
	match name {
		'tcgetattr' {
			// V C interop - terminal attributes (not available on BEAM)
			if g.etf_mode {
				g.emit_atom('undefined')
			} else {
				g.write_core("~{{'vbeam','type'}=>'Termios'}~")
			}
			return true
		}
		'tcsetattr' {
			g.emit_atom('ok')
			return true
		}
		'NULL' {
			// V pointer null
			g.emit_int('0')
			return true
		}
		else {
			return false
		}
	}
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
	if g.core_method_call_unknown(node, left_type) {
		return
	}

	type_sym := g.table.sym(left_type)
	short_name := g.core_method_short_name(type_sym.name)
	if g.core_method_call_handlers(node, type_sym, short_name) {
		return
	}
	g.core_method_call_fallback(node, type_sym)
}

fn (mut g CoreGen) core_method_call_unknown(node ast.CallExpr, left_type ast.Type) bool {
	if int(left_type) != 0 {
		return false
	}
	if int(left_type) == 0 {
		if g.etf_mode {
			g.begin_apply_var()
			g.emit_fname('unknown.${node.name}', node.args.len + 1)
			g.mid_apply_args()
			g.core_expr(node.left)
			for arg in node.args {
				g.emit_sep()
				g.core_expr(arg.expr)
			}
			g.end_apply()
		} else {
			g.write_core("apply 'unknown.${node.name}'/${node.args.len + 1}(")
			g.core_expr(node.left)
			for arg in node.args {
				g.write_core(', ')
				g.core_expr(arg.expr)
			}
			g.write_core(')')
		}
		return true
	}
	return false
}

fn (g CoreGen) core_method_short_name(full_name string) string {
	if full_name.contains('[') {
		return full_name.all_before('[').all_after_last('.')
	}
	return full_name.all_after_last('.')
}

fn (mut g CoreGen) core_method_call_handlers(node ast.CallExpr, type_sym ast.TypeSymbol, short_name string) bool {
	return g.core_method_call_pre_1(node, type_sym, short_name)
		|| g.core_method_call_pre_2(node, type_sym, short_name)
		|| g.core_method_call_pre_3(node, type_sym, short_name)
		|| g.core_method_call_pre_4(node, type_sym, short_name)
		|| g.core_method_call_pre_5(node, type_sym, short_name)
		|| g.core_method_call_post_1(node, type_sym, short_name)
		|| g.core_method_call_post_2(node, type_sym, short_name)
		|| g.core_method_call_post_3(node, type_sym, short_name)
		|| g.core_method_call_post_4(node, type_sym, short_name)
		|| g.core_method_call_post_5(node, type_sym, short_name)
		|| g.core_method_call_post_6(node, type_sym, short_name)
		|| g.core_method_call_post_7(node, type_sym, short_name)
		|| g.core_method_call_post_8(node, type_sym, short_name)
		|| g.core_method_call_post_9(node, type_sym, short_name)
		|| g.core_method_call_post_10(node, type_sym, short_name)
		|| g.core_method_call_post_11(node, type_sym, short_name)
		|| g.core_method_call_post_12(node, type_sym, short_name)
		|| g.core_method_call_post_13(node, type_sym, short_name)
		|| g.core_method_call_post_14(node, type_sym, short_name)
}

fn (mut g CoreGen) core_method_call_pre_1(node ast.CallExpr, type_sym ast.TypeSymbol, short_name string) bool {
	_ = short_name
	// Handle .str() on primitive types
	if node.name == 'str' && node.args.len == 0 {
		type_name := type_sym.name
		is_int := type_sym.kind == .int || type_sym.kind == .i8 || type_sym.kind == .i16
			|| type_sym.kind == .i32 || type_sym.kind == .i64 || type_sym.kind == .u8
			|| type_sym.kind == .u16 || type_sym.kind == .u32 || type_sym.kind == .u64
			|| type_sym.kind == .int_literal || type_name == 'int' || type_name == 'i32'
			|| type_name == 'i64' || type_name == 'u32' || type_name == 'u64'
		is_float := type_sym.kind == .f32 || type_sym.kind == .f64
			|| type_sym.kind == .float_literal || type_name == 'f32' || type_name == 'f64'
		is_bool := type_sym.kind == .bool || type_name == 'bool'

		if is_int {
			g.begin_call('erlang', 'integer_to_binary')
			g.core_expr(node.left)
			g.end_call()
			return true
		} else if is_float {
			g.begin_call('erlang', 'float_to_binary')
			g.core_expr(node.left)
			g.write_core(', ')
			g.core_float_format_opts()
			g.end_call()
			return true
		} else if is_bool {
			g.begin_call('erlang', 'atom_to_binary')
			g.core_expr(node.left)
			g.end_call()
			return true
		}
	}
	// Handle string methods
	if type_sym.kind == .string || type_sym.name == 'string' {
		if g.core_string_method(node) {
			return true
		}
	}
	// Handle array methods
	if type_sym.kind == .array || type_sym.name.starts_with('[]') {
		if g.core_array_method(node) {
			return true
		}
	}
	return false
}

fn (mut g CoreGen) core_method_call_pre_2(node ast.CallExpr, type_sym ast.TypeSymbol, short_name string) bool {
	_ = short_name
	// Handle map methods
	if type_sym.kind == .map || type_sym.name.starts_with('map[') {
		if g.core_map_method(node) {
			return true
		}
	}
	// Handle primitive type methods
	if type_sym.kind == .u8 || type_sym.kind == .char {
		if g.core_method_u8_char(node) {
			return true
		}
	}
	if type_sym.kind == .u32 || type_sym.kind == .u64 || type_sym.kind == .int
		|| type_sym.kind == .i32 || type_sym.kind == .i64 {
		match node.name {
			'hex' {
				// u32.hex() -> integer_to_binary(N, 16)
				g.begin_call('erlang', 'integer_to_binary')
				g.core_expr(node.left)
				g.emit_sep()
				g.emit_int('16')
				g.end_call()
				return true
			}
			else {}
		}
	}
	// Handle string.u32() method
	if type_sym.kind == .string || type_sym.name == 'string' {
		if node.name == 'u32' {
			g.begin_call('erlang', 'binary_to_integer')
			g.core_expr(node.left)
			g.end_call()
			return true
		}
	}
	return false
}

fn (mut g CoreGen) core_method_call_pre_3(node ast.CallExpr, type_sym ast.TypeSymbol, short_name string) bool {
	_ = short_name
	// Handle thread.wait() -> vbeam_task:await (BEAM processes)
	if type_sym.name.contains('thread') || type_sym.name.contains('Thread') {
		match node.name {
			'wait' {
				g.begin_call('vbeam_task', 'await')
				g.core_expr(node.left)
				g.end_call()
				return true
			}
			else {}
		}
	}
	return false
}

fn (mut g CoreGen) core_method_call_pre_4(node ast.CallExpr, type_sym ast.TypeSymbol, short_name string) bool {
	_ = short_name
	// Channel operations
	if type_sym.name.contains('Channel') || type_sym.name.contains('chan ') {
		match node.name {
			'push' {
				// ch.push(val) or ch <- val
				g.begin_call('vbeam_concurrency', 'channel_send')
				g.core_expr(node.left)
				g.emit_sep()
				if node.args.len > 0 {
					g.core_expr(node.args[0].expr)
				} else {
					g.emit_atom('nil')
				}
				g.end_call()
				return true
			}
			'pop' {
				// val := <- ch or ch.pop()
				g.begin_call('vbeam_concurrency', 'channel_receive')
				g.core_expr(node.left)
				g.end_call()
				return true
			}
			'try_push' {
				g.begin_call('vbeam_concurrency', 'channel_send')
				g.core_expr(node.left)
				g.emit_sep()
				if node.args.len > 0 {
					g.core_expr(node.args[0].expr)
				} else {
					g.emit_atom('nil')
				}
				g.end_call()
				return true
			}
			'try_pop' {
				g.begin_call('vbeam_concurrency', 'channel_try_receive')
				g.core_expr(node.left)
				g.end_call()
				return true
			}
			'close' {
				g.begin_call('vbeam_concurrency', 'channel_close')
				g.core_expr(node.left)
				g.end_call()
				return true
			}
			'len' {
				g.begin_call('vbeam_concurrency', 'channel_len')
				g.core_expr(node.left)
				g.end_call()
				return true
			}
			else {}
		}
	}
	return false
}

fn (mut g CoreGen) core_method_call_pre_5(node ast.CallExpr, type_sym ast.TypeSymbol, short_name string) bool {
	_ = short_name
	// Shared object methods (lock/rlock)
	if type_sym.name.contains('Mutex') || type_sym.name.contains('RwMutex') {
		match node.name {
			'@lock' {
				g.emit_atom('ok') // BEAM processes don't need mutex locking
				return true
			}
			'@rlock' {
				g.emit_atom('ok')
				return true
			}
			'unlock' {
				g.emit_atom('ok')
				return true
			}
			'runlock' {
				g.emit_atom('ok')
				return true
			}
			else {}
		}
	}
	return false
}

fn (mut g CoreGen) core_method_call_post_1(node ast.CallExpr, type_sym ast.TypeSymbol, short_name string) bool {
	_ = type_sym
	if short_name == 'WaitGroup' {
		match node.name {
			'wait' {
				g.emit_atom('ok')
				return true
			}
			'done' {
				g.emit_atom('ok')
				return true
			}
			'add' {
				g.emit_atom('ok')
				return true
			}
			else {}
		}
	}
	// Handle Log methods
	if short_name == 'Log' || short_name == 'Logger' {
		match node.name {
			'fatal' {
				g.begin_call('erlang', 'error')
				if node.args.len > 0 {
					g.core_expr(node.args[0].expr)
				} else {
					g.emit_atom('fatal')
				}
				g.end_call()
				return true
			}
			'set_level', 'info', 'warn', 'debug', 'error', 'log_to_console_too', 'set_output_path',
			'set_output_label', 'set_full_logpath' {
				g.emit_atom('ok')
				return true
			}
			else {}
		}
	}
	return false
}

fn (mut g CoreGen) core_method_call_post_2(node ast.CallExpr, type_sym ast.TypeSymbol, short_name string) bool {
	_ = type_sym
	// Handle TcpConn methods
	if short_name == 'TcpConn' || short_name == 'UdpConn' {
		match node.name {
			'peer_addr', 'addr' {
				if g.etf_mode {
					g.emit_atom('undefined')
				} else {
					g.write_core("~{'addr'=>${core_bitstring('0.0.0.0')},'port'=>0,{'vbeam','type'}=>'Addr'}~")
				}
				return true
			}
			'write_string', 'write', 'close' {
				g.emit_atom('ok')
				return true
			}
			'read' {
				g.emit_binary('')
				return true
			}
			else {}
		}
	}
	// Handle Command methods
	if short_name == 'Command' {
		match node.name {
			'start' {
				g.emit_atom('ok')
				return true
			}
			'read_line' {
				g.emit_binary('')
				return true
			}
			'wait' {
				g.emit_atom('ok')
				return true
			}
			else {}
		}
	}
	return false
}

fn (mut g CoreGen) core_method_call_post_3(node ast.CallExpr, type_sym ast.TypeSymbol, short_name string) bool {
	_ = type_sym
	// Handle Time methods
	if short_name == 'Time' {
		match node.name {
			'format' {
				g.emit_binary('1970-01-01 00:00:00')
				return true
			}
			'unix', 'unix_milli', 'unix_micro', 'unix_nano' {
				g.emit_int('0')
				return true
			}
			else {}
		}
	}
	// Handle Digest (crypto) methods
	if short_name == 'Digest' || short_name == 'Hash' {
		match node.name {
			'sum' {
				g.begin_call('crypto', 'hash')
				g.emit_atom('sha256')
				g.emit_sep()
				if node.args.len > 0 {
					g.core_expr(node.args[0].expr)
				} else {
					g.emit_binary('')
				}
				g.end_call()
				return true
			}
			'write' {
				g.emit_atom('ok')
				return true
			}
			'reset' {
				g.emit_atom('ok')
				return true
			}
			else {}
		}
	}
	return false
}

fn (mut g CoreGen) core_method_call_post_4(node ast.CallExpr, type_sym ast.TypeSymbol, short_name string) bool {
	_ = type_sym
	// Handle Vec/Vec2/Vec3 methods (math.vec)
	if short_name == 'Vec' || short_name == 'Vec2' || short_name == 'Vec3' || short_name == 'Vec4' {
		match node.name {
			'cross' {
				// Simplified: return zero vector
				if g.etf_mode {
					g.emit_atom('undefined')
				} else {
					g.write_core("~{'x'=>0.0,'y'=>0.0,'z'=>0.0,{'vbeam','type'}=>'Vec3'}~")
				}
				return true
			}
			'dot' {
				g.emit_float('0.0')
				return true
			}
			'normalize', 'unit' {
				g.core_expr(node.left)
				return true
			}
			'length', 'magnitude' {
				g.emit_float('0.0')
				return true
			}
			'mul_scalar', 'div_scalar', 'add', 'sub', 'scale' {
				g.core_expr(node.left)
				return true
			}
			else {}
		}
	}
	return false
}

fn (mut g CoreGen) core_method_call_post_5(node ast.CallExpr, type_sym ast.TypeSymbol, short_name string) bool {
	_ = type_sym
	// Handle Header methods (http)
	if short_name == 'Header' {
		match node.name {
			'add', 'set', 'delete' {
				g.emit_atom('ok')
				return true
			}
			'get' {
				g.emit_binary('')
				return true
			}
			else {}
		}
	}
	// Handle KvStore methods (jsonrpc server example)
	if short_name == 'KvStore' {
		match node.name {
			'get' {
				g.emit_atom('false')
				return true
			}
			'set', 'delete' {
				g.emit_atom('ok')
				return true
			}
			else {}
		}
	}
	return false
}

fn (mut g CoreGen) core_method_call_post_6(node ast.CallExpr, type_sym ast.TypeSymbol, short_name string) bool {
	_ = type_sym
	// Handle File methods
	if short_name == 'File' {
		match node.name {
			'writeln', 'write', 'write_string', 'flush', 'close', 'seek' {
				g.emit_atom('ok')
				return true
			}
			'read' {
				g.emit_binary('')
				return true
			}
			'tell' {
				g.emit_int('0')
				return true
			}
			else {}
		}
	}
	// Handle Process methods
	if short_name == 'Process' {
		match node.name {
			'run', 'start', 'close', 'kill', 'wait', 'set_args', 'set_redirect_stdio',
			'set_environment', 'set_work_folder', 'stdin_write' {
				g.emit_atom('ok')
				return true
			}
			'read_line' {
				g.emit_binary('')
				return true
			}
			'stdout_read', 'stderr_read', 'stdout_slurp', 'stderr_slurp' {
				g.emit_binary('')
				return true
			}
			else {}
		}
	}
	return false
}

fn (mut g CoreGen) core_method_call_post_7(node ast.CallExpr, type_sym ast.TypeSymbol, short_name string) bool {
	_ = type_sym
	// Handle FlagParser methods
	if short_name == 'FlagParser' || short_name == 'Flag' {
		match node.name {
			'application', 'description', 'footer', 'version', 'limit_free_args',
			'limit_free_args_all', 'limit_free_args_to_exactly', 'skip_executable', 'usage' {
				g.emit_atom('ok')
				return true
			}
			'remaining_parameters' {
				g.emit_nil()
				return true
			}
			'string', 'string_opt' {
				g.emit_binary('')
				return true
			}
			'int', 'int_opt' {
				g.emit_int('0')
				return true
			}
			'bool', 'bool_opt' {
				g.emit_atom('false')
				return true
			}
			'remaining' {
				g.emit_nil()
				return true
			}
			'finalize' {
				g.emit_nil()
				return true
			}
			else {}
		}
	}
	return false
}

fn (mut g CoreGen) core_method_call_post_8(node ast.CallExpr, type_sym ast.TypeSymbol, short_name string) bool {
	_ = type_sym
	// Handle CLI Command methods (V cli module)
	if short_name == 'Command' {
		match node.name {
			'add_command', 'set_help_command', 'add_flag', 'set_defaults', 'setup', 'execute' {
				g.emit_atom('ok')
				return true
			}
			'parse' {
				g.emit_atom('ok')
				return true
			}
			else {}
		}
	}
	// Handle Mutex/RwMutex methods
	if short_name == 'Mutex' || short_name == 'RwMutex' {
		match node.name {
			'lock', 'm_lock', 'unlock', 'm_unlock', 'rlock', 'runlock' {
				g.emit_atom('ok')
				return true
			}
			else {}
		}
	}
	return false
}

fn (mut g CoreGen) core_method_call_post_9(node ast.CallExpr, type_sym ast.TypeSymbol, short_name string) bool {
	_ = type_sym
	// Handle Regex (RE) methods
	if short_name == 'RE' || short_name == 'Regex' {
		match node.name {
			'get_group_bounds_by_name', 'get_group_list' {
				// Return empty list (no match groups found)
				g.emit_nil()
				return true
			}
			'match_str', 'match_string' {
				g.begin_tuple()
				g.emit_atom('ok')
				g.emit_sep()
				g.emit_nil()
				g.end_tuple()
				return true
			}
			'replace', 'replace_simple' {
				// Return the original string
				g.core_expr(node.left)
				return true
			}
			else {}
		}
	}
	// Handle Context methods (gg/graphics)
	if short_name == 'Context' || short_name == 'GgContext' {
		match node.name {
			'clear', 'draw', 'draw_text', 'draw_rect', 'draw_line', 'draw_circle', 'draw_image',
			'draw_rounded_rect', 'draw_pixel', 'flush', 'begin', 'reset', 'run', 'quit' {
				g.emit_atom('ok')
				return true
			}
			else {}
		}
	}
	return false
}

fn (mut g CoreGen) core_method_call_post_10(node ast.CallExpr, type_sym ast.TypeSymbol, short_name string) bool {
	_ = type_sym
	// Handle Termios methods
	if short_name == 'Termios' {
		match node.name {
			'disable_echo', 'enable_echo', 'set_raw_mode', 'reset' {
				g.emit_atom('ok')
				return true
			}
			else {}
		}
	}
	// Handle TcpListener methods
	if short_name == 'TcpListener' {
		match node.name {
			'accept' {
				g.begin_tuple()
				g.emit_atom('ok')
				g.emit_sep()
				g.emit_atom('false')
				g.end_tuple()
				return true
			}
			'addr' {
				if g.etf_mode {
					g.emit_atom('undefined')
				} else {
					g.write_core("~{'addr'=>${core_bitstring('0.0.0.0')},'port'=>0,{'vbeam','type'}=>'Addr'}~")
				}
				return true
			}
			'close' {
				g.emit_atom('ok')
				return true
			}
			else {}
		}
	}
	return false
}

fn (mut g CoreGen) core_method_call_post_11(node ast.CallExpr, type_sym ast.TypeSymbol, short_name string) bool {
	_ = type_sym
	// Handle FdNotifier methods
	if short_name == 'FdNotifier' {
		match node.name {
			'add', 'remove', 'modify' {
				g.emit_atom('ok')
				return true
			}
			'wait' {
				g.emit_nil()
				return true
			}
			else {}
		}
	}
	// Handle Subscriber/EventBus methods
	if short_name == 'Subscriber' || short_name == 'EventBus' {
		match node.name {
			'subscribe', 'subscribe_method', 'publish', 'unsubscribe' {
				g.emit_atom('ok')
				return true
			}
			'is_subscriber' {
				g.emit_atom('false')
				return true
			}
			else {}
		}
	}
	// Handle PoolProcessor methods
	if short_name == 'PoolProcessor' {
		match node.name {
			'get_item' {
				g.emit_atom('false')
				return true
			}
			'set_shared', 'work_on_items', 'set_max_thread_count' {
				g.emit_atom('ok')
				return true
			}
			else {}
		}
	}
	return false
}

fn (mut g CoreGen) core_method_call_post_12(node ast.CallExpr, type_sym ast.TypeSymbol, short_name string) bool {
	_ = type_sym
	// Handle Client/Server (jsonrpc, http) methods
	if short_name == 'Client' {
		match node.name {
			'batch', 'call', 'send', 'recv', 'close', 'notify', 'request' {
				g.emit_atom('ok')
				return true
			}
			else {}
		}
	}
	if short_name == 'Server' {
		match node.name {
			'listen_and_serve', 'handle', 'handle_func', 'close' {
				g.emit_atom('ok')
				return true
			}
			else {}
		}
	}
	// Handle Queue/Deque methods
	if short_name == 'Queue' || short_name == 'Deque' {
		match node.name {
			'push', 'push_back', 'push_front', 'enqueue' {
				g.emit_atom('ok')
				return true
			}
			'pop', 'pop_front', 'pop_back', 'dequeue' {
				g.emit_atom('false')
				return true
			}
			'is_empty' {
				g.emit_atom('true')
				return true
			}
			else {}
		}
	}
	return false
}

fn (mut g CoreGen) core_method_call_post_13(node ast.CallExpr, type_sym ast.TypeSymbol, short_name string) bool {
	_ = type_sym
	// Handle Any (json/toml) methods
	if short_name == 'Any' {
		match node.name {
			'as_array' {
				g.emit_nil()
				return true
			}
			'string', 'str' {
				g.emit_binary('')
				return true
			}
			'int' {
				g.emit_int('0')
				return true
			}
			'f64' {
				g.emit_float('0.0')
				return true
			}
			'bool' {
				g.emit_atom('false')
				return true
			}
			'as_map' {
				if g.etf_mode {
					g.emit_atom('undefined')
				} else {
					g.write_core('~{}~')
				}
				return true
			}
			else {}
		}
	}
	return false
}

fn (mut g CoreGen) core_method_call_post_14(node ast.CallExpr, type_sym ast.TypeSymbol, short_name string) bool {
	_ = type_sym
	// Handle Show/Enum bitfield methods
	if short_name == 'Show' {
		match node.name {
			'has' {
				g.emit_atom('true')
				return true
			}
			'toggle' {
				g.core_expr(node.left)
				return true
			}
			else {}
		}
	}
	// Handle DiffContext methods
	if short_name == 'DiffContext' {
		match node.name {
			'generate_patch', 'diff' {
				g.emit_binary('')
				return true
			}
			else {}
		}
	}
	return false
}

fn (mut g CoreGen) core_method_u8_char(node ast.CallExpr) bool {
	return g.core_method_u8_char_1(node) || g.core_method_u8_char_2(node)
		|| g.core_method_u8_char_3(node)
}

fn (mut g CoreGen) core_method_u8_char_1(node ast.CallExpr) bool {
	match node.name {
		'ascii_str' {
			// u8.ascii_str() -> <<X>>
			if g.etf_mode {
				g.begin_call('erlang', 'list_to_binary')
				g.begin_cons()
				g.core_expr(node.left)
				g.mid_cons()
				g.emit_nil()
				g.end_cons()
				g.end_call()
			} else {
				g.write_core("call 'erlang':'list_to_binary'([")
				g.core_expr(node.left)
				g.write_core('|[]])')
			}
			return true
		}
		'is_digit' {
			// u8.is_digit() -> X >= $0 andalso X =< $9
			if g.etf_mode {
				g.begin_call('erlang', 'andalso')
				g.begin_call('erlang', '>=')
				g.core_expr(node.left)
				g.emit_sep()
				g.emit_int('48')
				g.end_call()
				g.emit_sep()
				g.begin_call('erlang', '=<')
				g.core_expr(node.left)
				g.emit_sep()
				g.emit_int('57')
				g.end_call()
				g.end_call()
			} else {
				g.write_core("call 'erlang':'andalso'(call 'erlang':'>='(")
				g.core_expr(node.left)
				g.write_core(", 48), call 'erlang':'=<'(")
				g.core_expr(node.left)
				g.write_core(', 57))')
			}
			return true
		}
		else {
			return false
		}
	}
}

fn (mut g CoreGen) core_method_u8_char_2(node ast.CallExpr) bool {
	match node.name {
		'is_letter' {
			if g.etf_mode {
				g.begin_call('erlang', 'orelse')
				g.begin_call('erlang', 'andalso')
				g.begin_call('erlang', '>=')
				g.core_expr(node.left)
				g.emit_sep()
				g.emit_int('65')
				g.end_call()
				g.emit_sep()
				g.begin_call('erlang', '=<')
				g.core_expr(node.left)
				g.emit_sep()
				g.emit_int('90')
				g.end_call()
				g.end_call()
				g.emit_sep()
				g.begin_call('erlang', 'andalso')
				g.begin_call('erlang', '>=')
				g.core_expr(node.left)
				g.emit_sep()
				g.emit_int('97')
				g.end_call()
				g.emit_sep()
				g.begin_call('erlang', '=<')
				g.core_expr(node.left)
				g.emit_sep()
				g.emit_int('122')
				g.end_call()
				g.end_call()
				g.end_call()
			} else {
				g.write_core("call 'erlang':'orelse'(call 'erlang':'andalso'(call 'erlang':'>='(")
				g.core_expr(node.left)
				g.write_core(", 65), call 'erlang':'=<'(")
				g.core_expr(node.left)
				g.write_core(", 90)), call 'erlang':'andalso'(call 'erlang':'>='(")
				g.core_expr(node.left)
				g.write_core(", 97), call 'erlang':'=<'(")
				g.core_expr(node.left)
				g.write_core(', 122)))')
			}
			return true
		}
		else {
			return false
		}
	}
}

fn (mut g CoreGen) core_method_u8_char_3(node ast.CallExpr) bool {
	match node.name {
		'is_space' {
			// u8.is_space() -> X =:= 32 orelse X =:= 9 orelse X =:= 10 orelse X =:= 13
			if g.etf_mode {
				g.begin_call('erlang', 'orelse')
				g.begin_call('erlang', 'orelse')
				g.begin_call('erlang', 'orelse')
				g.begin_call('erlang', '=:=')
				g.core_expr(node.left)
				g.emit_sep()
				g.emit_int('32')
				g.end_call()
				g.emit_sep()
				g.begin_call('erlang', '=:=')
				g.core_expr(node.left)
				g.emit_sep()
				g.emit_int('9')
				g.end_call()
				g.end_call()
				g.emit_sep()
				g.begin_call('erlang', '=:=')
				g.core_expr(node.left)
				g.emit_sep()
				g.emit_int('10')
				g.end_call()
				g.end_call()
				g.emit_sep()
				g.begin_call('erlang', '=:=')
				g.core_expr(node.left)
				g.emit_sep()
				g.emit_int('13')
				g.end_call()
				g.end_call()
			} else {
				g.write_core("call 'erlang':'orelse'(call 'erlang':'orelse'(call 'erlang':'orelse'(call 'erlang':'=:='(")
				g.core_expr(node.left)
				g.write_core(", 32), call 'erlang':'=:='(")
				g.core_expr(node.left)
				g.write_core(", 9)), call 'erlang':'=:='(")
				g.core_expr(node.left)
				g.write_core(", 10)), call 'erlang':'=:='(")
				g.core_expr(node.left)
				g.write_core(', 13))')
			}
			return true
		}
		else {
			return false
		}
	}
}

fn (mut g CoreGen) core_method_call_fallback(node ast.CallExpr, type_sym ast.TypeSymbol) {
	// Strip generic type parameters properly:
	// main.BST[main.KeyVal] -> BST (not KeyVal])
	full_name_raw := type_sym.name
	mut short_type := if full_name_raw.contains('[') {
		full_name_raw.all_before('[').all_after_last('.')
	} else {
		full_name_raw.all_after_last('.')
	}
	// Handle array types: []Flag -> Flag, []u8 -> array
	if short_type.len == 0 || short_type.starts_with('[]') {
		short_type = 'array'
	}
	arity := node.args.len + 1
	if g.etf_mode {
		g.begin_apply('${short_type}.${node.name}', arity)
		g.core_expr(node.left)
		for arg in node.args {
			g.emit_sep()
			g.core_expr(arg.expr)
		}
		g.end_apply()
	} else {
		g.write_core("apply '${short_type}.${node.name}'/${arity}(")
		g.core_expr(node.left)
		for arg in node.args {
			g.write_core(', ')
			g.core_expr(arg.expr)
		}
		g.write_core(')')
	}
}

// core_string_method handles string method calls mapped to Erlang stdlib
fn (mut g CoreGen) core_string_method(node ast.CallExpr) bool {
	return g.core_string_method_numeric(node) || g.core_string_method_split(node)
		|| g.core_string_method_search(node) || g.core_string_method_transform(node)
		|| g.core_string_method_misc(node)
}

fn (mut g CoreGen) core_string_method_numeric(node ast.CallExpr) bool {
	return g.core_string_method_numeric_1(node) || g.core_string_method_numeric_2(node)
}

fn (mut g CoreGen) core_string_method_numeric_1(node ast.CallExpr) bool {
	match node.name {
		'int' {
			// string.int() -> binary_to_integer(Str)
			g.begin_call('erlang', 'binary_to_integer')
			g.core_expr(node.left)
			g.end_call()
			return true
		}
		'f64' {
			// string.f64() -> binary_to_float(Str)
			g.begin_call('erlang', 'binary_to_float')
			g.core_expr(node.left)
			g.end_call()
			return true
		}
		'bytes' {
			// string.bytes() -> binary_to_list(Str)
			g.begin_call('erlang', 'binary_to_list')
			g.core_expr(node.left)
			g.end_call()
			return true
		}
		'len' {
			g.begin_call('erlang', 'byte_size')
			g.core_expr(node.left)
			g.end_call()
			return true
		}
		'runes' {
			// string.runes() -> unicode:characters_to_list(S)
			// Returns list of Unicode codepoints (runes)
			g.begin_call('unicode', 'characters_to_list')
			g.core_expr(node.left)
			g.end_call()
			return true
		}
		else {
			return false
		}
	}
}

fn (mut g CoreGen) core_string_method_numeric_2(node ast.CallExpr) bool {
	match node.name {
		'ascii_str' {
			// u8.ascii_str() -> <<Byte>> (single-byte binary)
			// Codegen: erlang:list_to_binary([Byte])
			g.begin_call('erlang', 'list_to_binary')
			if g.etf_mode {
				g.begin_cons()
				g.core_expr(node.left)
				g.mid_cons()
				g.emit_nil()
				g.end_cons()
			} else {
				g.write_core('[')
				g.core_expr(node.left)
				g.write_core('|[]]')
			}
			g.end_call()
			return true
		}
		else {
			return false
		}
	}
}

fn (mut g CoreGen) core_string_method_split(node ast.CallExpr) bool {
	return g.core_string_method_split_1(node) || g.core_string_method_split_2(node)
}

fn (mut g CoreGen) core_string_method_split_1(node ast.CallExpr) bool {
	match node.name {
		'split' {
			// string.split(delim) -> binary:split(Str, Delim, [global])
			if g.etf_mode {
				g.begin_call('binary', 'split')
				g.core_expr(node.left)
				g.emit_sep()
				if node.args.len > 0 {
					g.core_expr(node.args[0].expr)
				}
				g.emit_sep()
				g.begin_cons()
				g.emit_atom('global')
				g.mid_cons()
				g.emit_nil()
				g.end_cons()
				g.end_call()
			} else {
				g.write_core("call 'binary':'split'(")
				g.core_expr(node.left)
				g.write_core(', ')
				if node.args.len > 0 {
					g.core_expr(node.args[0].expr)
				}
				g.write_core(", ['global'|[]])")
			}
			return true
		}
		else {
			return false
		}
	}
}

fn (mut g CoreGen) core_string_method_split_2(node ast.CallExpr) bool {
	match node.name {
		'split_into_lines' {
			// string.split_into_lines() -> binary:split(Str, <<"\n">>, [global])
			if g.etf_mode {
				g.begin_call('binary', 'split')
				g.core_expr(node.left)
				g.emit_sep()
				g.emit_binary('\n')
				g.emit_sep()
				g.begin_cons()
				g.emit_atom('global')
				g.mid_cons()
				g.emit_nil()
				g.end_cons()
				g.end_call()
			} else {
				g.write_core("call 'binary':'split'(")
				g.core_expr(node.left)
				g.write_core(', ${core_bitstring('\n')}, [' + "'global'|[]])")
			}
			return true
		}
		else {
			return false
		}
	}
}

fn (mut g CoreGen) core_string_method_search(node ast.CallExpr) bool {
	return g.core_string_method_search_1(node) || g.core_string_method_search_2(node)
		|| g.core_string_method_search_3(node)
}

fn (mut g CoreGen) core_string_method_search_1(node ast.CallExpr) bool {
	match node.name {
		'contains' {
			// string.contains(sub) -> case binary:match(Str, Sub) of nomatch -> false; _ -> true
			if g.etf_mode {
				g.begin_case()
				g.begin_call('binary', 'match')
				g.core_expr(node.left)
				g.emit_sep()
				if node.args.len > 0 {
					g.core_expr(node.args[0].expr)
				}
				g.end_call()
				g.mid_case_clauses()
				g.begin_clause()
				g.emit_atom('nomatch')
				g.mid_clause_guard()
				g.emit_true_guard()
				g.mid_clause_body()
				g.emit_atom('false')
				g.end_clause()
				g.clause_sep()
				g.begin_clause()
				g.emit_var('_')
				g.mid_clause_guard()
				g.emit_true_guard()
				g.mid_clause_body()
				g.emit_atom('true')
				g.end_clause()
				g.end_case()
			} else {
				g.write_core("case call 'binary':'match'(")
				g.core_expr(node.left)
				g.write_core(', ')
				if node.args.len > 0 {
					g.core_expr(node.args[0].expr)
				}
				g.write_core(") of <'nomatch'> when 'true' -> 'false' <_> when 'true' -> 'true' end")
			}
			return true
		}
		else {
			return false
		}
	}
}

fn (mut g CoreGen) core_string_method_search_2(node ast.CallExpr) bool {
	match node.name {
		'starts_with' {
			// string.starts_with(prefix) -> call string:prefix(Str, Prefix) != nomatch
			if g.etf_mode {
				g.begin_case()
				g.begin_call('string', 'prefix')
				g.core_expr(node.left)
				g.emit_sep()
				if node.args.len > 0 {
					g.core_expr(node.args[0].expr)
				}
				g.end_call()
				g.mid_case_clauses()
				g.begin_clause()
				g.emit_atom('nomatch')
				g.mid_clause_guard()
				g.emit_true_guard()
				g.mid_clause_body()
				g.emit_atom('false')
				g.end_clause()
				g.clause_sep()
				g.begin_clause()
				g.emit_var('_')
				g.mid_clause_guard()
				g.emit_true_guard()
				g.mid_clause_body()
				g.emit_atom('true')
				g.end_clause()
				g.end_case()
			} else {
				g.write_core("case call 'string':'prefix'(")
				g.core_expr(node.left)
				g.write_core(', ')
				if node.args.len > 0 {
					g.core_expr(node.args[0].expr)
				}
				g.write_core(") of <'nomatch'> when 'true' -> 'false' <_> when 'true' -> 'true' end")
			}
			return true
		}
		else {
			return false
		}
	}
}

fn (mut g CoreGen) core_string_method_search_3(node ast.CallExpr) bool {
	match node.name {
		'ends_with' {
			// Use binary pattern matching approach
			if g.etf_mode {
				g.begin_case()
				g.begin_call('binary', 'longest_common_suffix')
				g.begin_cons()
				g.core_expr(node.left)
				g.mid_cons()
				g.begin_cons()
				if node.args.len > 0 {
					g.core_expr(node.args[0].expr)
				}
				g.mid_cons()
				g.emit_nil()
				g.end_cons()
				g.end_cons()
				g.end_call()
				g.mid_case_clauses()
				g.begin_clause()
				g.emit_int('0')
				g.mid_clause_guard()
				g.emit_true_guard()
				g.mid_clause_body()
				g.emit_atom('false')
				g.end_clause()
				g.clause_sep()
				g.begin_clause()
				g.emit_var('_')
				g.mid_clause_guard()
				g.emit_true_guard()
				g.mid_clause_body()
				g.emit_atom('true')
				g.end_clause()
				g.end_case()
			} else {
				g.write_core("case call 'binary':'longest_common_suffix'([")
				g.core_expr(node.left)
				g.write_core('|[')
				if node.args.len > 0 {
					g.core_expr(node.args[0].expr)
				}
				g.write_core("|[]]]) of <0> when 'true' -> 'false' <_> when 'true' -> 'true' end")
			}
			return true
		}
		else {
			return false
		}
	}
}

fn (mut g CoreGen) core_string_method_transform(node ast.CallExpr) bool {
	return g.core_string_method_transform_1(node) || g.core_string_method_transform_2(node)
}

fn (mut g CoreGen) core_string_method_transform_1(node ast.CallExpr) bool {
	match node.name {
		'to_lower' {
			g.begin_call('string', 'lowercase')
			g.core_expr(node.left)
			g.end_call()
			return true
		}
		'to_upper' {
			g.begin_call('string', 'uppercase')
			g.core_expr(node.left)
			g.end_call()
			return true
		}
		'trim_space', 'trim' {
			g.begin_call('string', 'trim')
			g.core_expr(node.left)
			g.end_call()
			return true
		}
		else {
			return false
		}
	}
}

fn (mut g CoreGen) core_string_method_transform_2(node ast.CallExpr) bool {
	match node.name {
		'replace' {
			// string.replace(old, new) -> binary:replace(Str, Old, New, [global])
			if node.args.len >= 2 {
				if g.etf_mode {
					g.begin_call('binary', 'replace')
					g.core_expr(node.left)
					g.emit_sep()
					g.core_expr(node.args[0].expr)
					g.emit_sep()
					g.core_expr(node.args[1].expr)
					g.emit_sep()
					g.begin_cons()
					g.emit_atom('global')
					g.mid_cons()
					g.emit_nil()
					g.end_cons()
					g.end_call()
				} else {
					g.write_core("call 'binary':'replace'(")
					g.core_expr(node.left)
					g.write_core(', ')
					g.core_expr(node.args[0].expr)
					g.write_core(', ')
					g.core_expr(node.args[1].expr)
					g.write_core(", ['global'|[]])")
				}
				return true
			}
			return false
		}
		else {
			return false
		}
	}
}

fn (mut g CoreGen) core_string_method_misc(node ast.CallExpr) bool {
	return false
}

// core_array_method handles array method calls mapped to Erlang stdlib
fn (mut g CoreGen) core_array_method(node ast.CallExpr) bool {
	return g.core_array_method_1(node) || g.core_array_method_2(node) || g.core_array_method_3(node)
		|| g.core_array_method_4(node) || g.core_array_method_5(node) || g.core_array_method_6(node)
}

fn (mut g CoreGen) core_array_method_1(node ast.CallExpr) bool {
	match node.name {
		'reverse' {
			g.begin_call('lists', 'reverse')
			g.core_expr(node.left)
			g.end_call()
			return true
		}
		'sort' {
			g.begin_call('lists', 'sort')
			g.core_expr(node.left)
			g.end_call()
			return true
		}
		'clone' {
			// On BEAM, lists are immutable - clone is identity
			g.core_expr(node.left)
			return true
		}
		'first' {
			g.begin_call('erlang', 'hd')
			g.core_expr(node.left)
			g.end_call()
			return true
		}
		'last' {
			g.begin_call('lists', 'last')
			g.core_expr(node.left)
			g.end_call()
			return true
		}
		'pop' {
			// Returns last element (simplified)
			g.begin_call('lists', 'last')
			g.core_expr(node.left)
			g.end_call()
			return true
		}
		else {
			return false
		}
	}
}

fn (mut g CoreGen) core_array_method_2(node ast.CallExpr) bool {
	match node.name {
		'join' {
			// []string.join(sep) -> lists:join(Sep, List) wrapped in iolist_to_binary
			if node.args.len > 0 {
				if g.etf_mode {
					g.begin_call('erlang', 'iolist_to_binary')
					g.begin_call('lists', 'join')
					g.core_expr(node.args[0].expr)
					g.emit_sep()
					g.core_expr(node.left)
					g.end_call()
					g.end_call()
				} else {
					g.write_core("call 'erlang':'iolist_to_binary'(call 'lists':'join'(")
					g.core_expr(node.args[0].expr)
					g.write_core(', ')
					g.core_expr(node.left)
					g.write_core('))')
				}
				return true
			}
			return false
		}
		else {
			return false
		}
	}
}

fn (mut g CoreGen) core_array_method_3(node ast.CallExpr) bool {
	match node.name {
		'index' {
			// arr.index(elem) -> length(lists:takewhile(fun(X) -> X =/= Elem end, List))
			// Returns index of first occurrence (-1 if not found handled by caller)
			if node.args.len > 0 {
				tmp := g.new_temp()
				if g.etf_mode {
					g.begin_let(tmp)
					g.begin_fun(['{tmp}_x'])
					g.begin_call('erlang', '=/=')
					g.emit_var('{tmp}_x')
					g.emit_sep()
					g.core_expr(node.args[0].expr)
					g.end_call()
					g.end_fun()
					g.mid_let()
					g.begin_call('erlang', 'length')
					g.begin_call('lists', 'takewhile')
					g.emit_var(tmp)
					g.emit_sep()
					g.core_expr(node.left)
					g.end_call()
					g.end_call()
					g.end_let()
				} else {
					g.write_core('let <${tmp}> = fun (${tmp}_x) -> call ')
					g.write_core("'erlang':'=/='(${tmp}_x, ")
					g.core_expr(node.args[0].expr)
					g.write_core(") in call 'erlang':'length'(call 'lists':'takewhile'(${tmp}, ")
					g.core_expr(node.left)
					g.write_core('))')
				}
				return true
			}
			return false
		}
		else {
			return false
		}
	}
}

fn (mut g CoreGen) core_array_method_4(node ast.CallExpr) bool {
	match node.name {
		'delete' {
			// Remove element at index
			if node.args.len > 0 {
				g.begin_call('lists', 'delete')
				g.core_expr(node.args[0].expr)
				g.emit_sep()
				g.core_expr(node.left)
				g.end_call()
				return true
			}
			return false
		}
		'filter' {
			if node.args.len > 0 {
				g.begin_call('lists', 'filter')
				g.core_expr(node.args[0].expr)
				g.emit_sep()
				g.core_expr(node.left)
				g.end_call()
				return true
			}
			return false
		}
		'map' {
			if node.args.len > 0 {
				g.begin_call('lists', 'map')
				g.core_expr(node.args[0].expr)
				g.emit_sep()
				g.core_expr(node.left)
				g.end_call()
				return true
			}
			return false
		}
		else {
			return false
		}
	}
}

fn (mut g CoreGen) core_array_method_5(node ast.CallExpr) bool {
	match node.name {
		'contains' {
			if node.args.len > 0 {
				g.begin_call('lists', 'member')
				g.core_expr(node.args[0].expr)
				g.emit_sep()
				g.core_expr(node.left)
				g.end_call()
				return true
			}
			return false
		}
		'wait' {
			// thread.wait() - for V concurrency
			// For BEAM, spawn returns a pid; wait isn't really needed
			// Just return ok
			g.emit_atom('ok')
			return true
		}
		'bytestr' {
			// []u8.bytestr() -> list_to_binary(List)
			g.begin_call('erlang', 'list_to_binary')
			g.core_expr(node.left)
			g.end_call()
			return true
		}
		'get_int' {
			// V []Flag.get_int(name) -> 0 (cli flag helper)
			g.emit_int('0')
			return true
		}
		'get_string' {
			// V []Flag.get_string(name) -> empty
			g.emit_binary('')
			return true
		}
		'get_bool' {
			g.emit_atom('false')
			return true
		}
		'get_strings' {
			g.emit_nil()
			return true
		}
		else {
			return false
		}
	}
}

fn (mut g CoreGen) core_array_method_6(node ast.CallExpr) bool {
	match node.name {
		'hex' {
			// []u8.hex() -> binary:encode_hex(list_to_binary(List))
			if g.etf_mode {
				g.begin_call('binary', 'encode_hex')
				g.begin_call('erlang', 'list_to_binary')
				g.core_expr(node.left)
				g.end_call()
				g.end_call()
			} else {
				g.write_core("call 'binary':'encode_hex'(call 'erlang':'list_to_binary'(")
				g.core_expr(node.left)
				g.write_core('))')
			}
			return true
		}
		else {
			return false
		}
	}
}

// core_map_method handles map method calls mapped to Erlang stdlib
fn (mut g CoreGen) core_map_method(node ast.CallExpr) bool {
	match node.name {
		'keys' {
			g.begin_call('maps', 'keys')
			g.core_expr(node.left)
			g.end_call()
			return true
		}
		'values' {
			g.begin_call('maps', 'values')
			g.core_expr(node.left)
			g.end_call()
			return true
		}
		'clone' {
			// Maps are immutable on BEAM
			g.core_expr(node.left)
			return true
		}
		'delete' {
			if node.args.len > 0 {
				g.begin_call('maps', 'remove')
				g.core_expr(node.args[0].expr)
				g.emit_sep()
				g.core_expr(node.left)
				g.end_call()
				return true
			}
			return false
		}
		else {
			return false
		}
	}
}

// core_io_format_one emits: call 'io':'format'(Fmt, [Arg|[]])
// where Fmt is a charlist format string and the single arg is already emitted by the callback.
fn (mut g CoreGen) core_io_format_one(fmt_str string, emit_arg fn (mut CoreGen)) {
	if g.etf_mode {
		g.begin_call('io', 'format')
		g.emit_charlist(fmt_str)
		g.emit_sep()
		g.begin_cons()
		emit_arg(mut g)
		g.mid_cons()
		g.emit_nil()
		g.end_cons()
		g.end_call()
	} else {
		fmt := core_charlist(fmt_str)
		g.write_core("call 'io':'format'(${fmt}, [")
		emit_arg(mut g)
		g.write_core('|[]])')
	}
}

// core_io_format_stderr_one emits: call 'io':'format'('standard_error', Fmt, [Arg|[]])
fn (mut g CoreGen) core_io_format_stderr_one(fmt_str string, emit_arg fn (mut CoreGen)) {
	if g.etf_mode {
		g.begin_call('io', 'format')
		g.emit_atom('standard_error')
		g.emit_sep()
		g.emit_charlist(fmt_str)
		g.emit_sep()
		g.begin_cons()
		emit_arg(mut g)
		g.mid_cons()
		g.emit_nil()
		g.end_cons()
		g.end_call()
	} else {
		fmt := core_charlist(fmt_str)
		g.write_core("call 'io':'format'('standard_error', ${fmt}, [")
		emit_arg(mut g)
		g.write_core('|[]])')
	}
}

fn (mut g CoreGen) core_println_call(node ast.CallExpr) {
	if node.args.len == 0 {
		// println() -> io:format("~n")
		if g.etf_mode {
			g.begin_call('io', 'format')
			g.emit_charlist('~n')
			g.end_call()
		} else {
			g.write_core("call 'io':'format'(${core_charlist('~n')})")
		}
		return
	}

	arg := node.args[0]

	if arg.expr is ast.StringLiteral {
		// String literal: use ~s~n format to avoid interpreting ~ in user strings
		str_val := arg.expr.val
		g.core_io_format_one('~s~n', fn [str_val] (mut gg CoreGen) {
			if gg.etf_mode {
				gg.emit_binary(core_escape_binary(str_val))
			} else {
				gg.write_core(core_bitstring(str_val))
			}
		})
	} else if arg.expr is ast.StringInterLiteral {
		// String interpolation: build binary, then print with ~s~n
		g.core_io_format_one('~s~n', fn [node] (mut gg CoreGen) {
			gg.core_expr(node.args[0].expr)
		})
	} else {
		// General expression: io:format("~s~n", [Expr])
		// For numeric types, convert to binary first
		arg_type := arg.typ

		if int(arg_type) != 0 {
			type_sym := g.table.sym(arg_type)
			if g.core_is_numeric_type(type_sym) {
				g.core_io_format_one('~s~n', fn [node, arg_type] (mut gg CoreGen) {
					gg.core_to_binary_expr(node.args[0].expr, arg_type)
				})
				return
			}
		}

		g.core_io_format_one('~s~n', fn [node] (mut gg CoreGen) {
			gg.core_expr(node.args[0].expr)
		})
	}
}

fn (mut g CoreGen) core_print_call(node ast.CallExpr) {
	if node.args.len == 0 {
		g.emit_atom('ok')
		return
	}
	arg := node.args[0]
	arg_type := arg.typ

	if arg.expr is ast.StringLiteral {
		str_val := arg.expr.val
		g.core_io_format_one('~s', fn [str_val] (mut gg CoreGen) {
			if gg.etf_mode {
				gg.emit_binary(core_escape_binary(str_val))
			} else {
				gg.write_core(core_bitstring(str_val))
			}
		})
	} else if arg.expr is ast.StringInterLiteral {
		g.core_io_format_one('~s', fn [node] (mut gg CoreGen) {
			gg.core_expr(node.args[0].expr)
		})
	} else {
		if int(arg_type) != 0 {
			type_sym := g.table.sym(arg_type)
			if g.core_is_numeric_type(type_sym) {
				g.core_io_format_one('~s', fn [node, arg_type] (mut gg CoreGen) {
					gg.core_to_binary_expr(node.args[0].expr, arg_type)
				})
				return
			}
		}
		g.core_io_format_one('~s', fn [node] (mut gg CoreGen) {
			gg.core_expr(node.args[0].expr)
		})
	}
}

fn (mut g CoreGen) core_eprintln_call(node ast.CallExpr) {
	if node.args.len == 0 {
		if g.etf_mode {
			g.begin_call('io', 'format')
			g.emit_atom('standard_error')
			g.emit_sep()
			g.emit_charlist('~n')
			g.emit_sep()
			g.emit_nil()
			g.end_call()
		} else {
			g.write_core("call 'io':'format'('standard_error', ${core_charlist('~n')}, [])")
		}
		return
	}
	arg := node.args[0]
	arg_type := arg.typ

	if arg.expr is ast.StringLiteral {
		str_val := arg.expr.val
		if g.etf_mode {
			// In ETF mode, emit ~s~n format with binary arg
			g.core_io_format_stderr_one('~s~n', fn [str_val] (mut gg CoreGen) {
				gg.emit_binary(core_escape_binary(str_val))
			})
		} else {
			g.write_core("call 'io':'format'('standard_error', ${core_charlist(str_val + '~n')}, [])")
		}
	} else {
		if int(arg_type) != 0 {
			type_sym := g.table.sym(arg_type)
			if g.core_is_numeric_type(type_sym) {
				g.core_io_format_stderr_one('~s~n', fn [node, arg_type] (mut gg CoreGen) {
					gg.core_to_binary_expr(node.args[0].expr, arg_type)
				})
				return
			}
		}
		g.core_io_format_stderr_one('~s~n', fn [node] (mut gg CoreGen) {
			gg.core_expr(node.args[0].expr)
		})
	}
}

fn (mut g CoreGen) core_eprint_call(node ast.CallExpr) {
	if node.args.len == 0 {
		g.emit_atom('ok')
		return
	}
	g.core_io_format_stderr_one('~s', fn [node] (mut gg CoreGen) {
		gg.core_expr(node.args[0].expr)
	})
}

fn (mut g CoreGen) core_is_numeric_type(sym ast.TypeSymbol) bool {
	return sym.kind == .int || sym.kind == .i8 || sym.kind == .i16 || sym.kind == .i32
		|| sym.kind == .i64 || sym.kind == .u8 || sym.kind == .u16 || sym.kind == .u32
		|| sym.kind == .u64 || sym.kind == .int_literal || sym.kind == .f32 || sym.kind == .f64
		|| sym.kind == .float_literal || sym.name == 'int' || sym.name == 'i32' || sym.name == 'f64'
}

fn (mut g CoreGen) core_to_binary_expr(expr ast.Expr, typ ast.Type) {
	if int(typ) == 0 {
		g.core_expr(expr)
		return
	}
	type_sym := g.table.sym(typ)
	type_name := type_sym.name

	is_int := type_sym.kind == .int || type_sym.kind == .i8 || type_sym.kind == .i16
		|| type_sym.kind == .i32 || type_sym.kind == .i64 || type_sym.kind == .u8
		|| type_sym.kind == .u16 || type_sym.kind == .u32 || type_sym.kind == .u64
		|| type_sym.kind == .int_literal || type_name == 'int' || type_name == 'i32'
	is_float := type_sym.kind == .f32 || type_sym.kind == .f64 || type_sym.kind == .float_literal
		|| type_name == 'f32' || type_name == 'f64'

	if is_int {
		g.begin_call('erlang', 'integer_to_binary')
		g.core_expr(expr)
		g.end_call()
	} else if is_float {
		g.begin_call('erlang', 'float_to_binary')
		g.core_expr(expr)
		g.write_core(', ')
		g.core_float_format_opts()
		g.end_call()
	} else if type_sym.kind == .bool || type_name == 'bool' {
		g.begin_call('erlang', 'atom_to_binary')
		g.core_expr(expr)
		g.end_call()
	} else {
		g.core_expr(expr)
	}
}

fn (mut g CoreGen) core_infix_expr(node ast.InfixExpr) {
	if g.core_infix_plus(node) || g.core_infix_div(node) || g.core_infix_key_in(node)
		|| g.core_infix_not_in(node) || g.core_infix_left_shift(node) || g.core_infix_and(node)
		|| g.core_infix_or(node) {
		return
	}
	op_str := core_op(node.op)
	g.begin_call('erlang', op_str)
	g.core_expr(node.left)
	g.emit_sep()
	g.core_expr(node.right)
	g.end_call()
}

fn (mut g CoreGen) core_infix_plus(node ast.InfixExpr) bool {
	if node.op != .plus {
		return false
	}
	left_is_string := g.core_is_string_expr(node.left, node.left_type)
	right_is_string := g.core_is_string_expr(node.right, node.right_type)
	if !(left_is_string && right_is_string) {
		return false
	}
	if g.etf_mode {
		g.begin_call('erlang', 'iolist_to_binary')
		g.begin_cons()
		g.core_expr(node.left)
		g.mid_cons()
		g.begin_cons()
		g.core_expr(node.right)
		g.mid_cons()
		g.emit_nil()
		g.end_cons()
		g.end_cons()
		g.end_call()
	} else {
		g.write_core("call 'erlang':'iolist_to_binary'([")
		g.core_expr(node.left)
		g.write_core('|[')
		g.core_expr(node.right)
		g.write_core('|[]]])')
	}
	return true
}

fn (mut g CoreGen) core_infix_div(node ast.InfixExpr) bool {
	if node.op != .div {
		return false
	}
	left_is_int := g.core_is_int_expr(node.left, node.left_type)
	right_is_int := g.core_is_int_expr(node.right, node.right_type)
	if !(left_is_int && right_is_int) {
		return false
	}
	g.begin_call('erlang', 'div')
	g.core_expr(node.left)
	g.emit_sep()
	g.core_expr(node.right)
	g.end_call()
	return true
}

fn (mut g CoreGen) core_infix_key_in(node ast.InfixExpr) bool {
	if node.op != .key_in {
		return false
	}
	left_tmp := '_in_left_${g.temp_counter}'
	g.temp_counter++
	right_tmp := '_in_right_${g.temp_counter}'
	g.temp_counter++
	g.begin_let(left_tmp)
	g.core_expr(node.left)
	g.mid_let()
	g.begin_let(right_tmp)
	g.core_expr(node.right)
	g.mid_let()
	g.begin_case()
	g.begin_call('erlang', 'is_map')
	g.emit_var(right_tmp)
	g.end_call()
	g.mid_case_clauses()
	g.begin_clause()
	g.emit_atom('true')
	g.mid_clause_guard()
	g.emit_true_guard()
	g.mid_clause_body()
	g.begin_call('maps', 'is_key')
	g.emit_var(left_tmp)
	g.emit_sep()
	g.emit_var(right_tmp)
	g.end_call()
	g.end_clause()
	g.clause_sep()
	g.begin_clause()
	g.emit_atom('false')
	g.mid_clause_guard()
	g.emit_true_guard()
	g.mid_clause_body()
	g.begin_call('lists', 'member')
	g.emit_var(left_tmp)
	g.emit_sep()
	g.emit_var(right_tmp)
	g.end_call()
	g.end_clause()
	g.end_case()
	g.end_let()
	g.end_let()
	return true
}

fn (mut g CoreGen) core_infix_not_in(node ast.InfixExpr) bool {
	if node.op != .not_in {
		return false
	}
	left_tmp := '_in_left_${g.temp_counter}'
	g.temp_counter++
	right_tmp := '_in_right_${g.temp_counter}'
	g.temp_counter++
	g.begin_let(left_tmp)
	g.core_expr(node.left)
	g.mid_let()
	g.begin_let(right_tmp)
	g.core_expr(node.right)
	g.mid_let()
	g.begin_call('erlang', 'not')
	g.begin_case()
	g.begin_call('erlang', 'is_map')
	g.emit_var(right_tmp)
	g.end_call()
	g.mid_case_clauses()
	g.begin_clause()
	g.emit_atom('true')
	g.mid_clause_guard()
	g.emit_true_guard()
	g.mid_clause_body()
	g.begin_call('maps', 'is_key')
	g.emit_var(left_tmp)
	g.emit_sep()
	g.emit_var(right_tmp)
	g.end_call()
	g.end_clause()
	g.clause_sep()
	g.begin_clause()
	g.emit_atom('false')
	g.mid_clause_guard()
	g.emit_true_guard()
	g.mid_clause_body()
	g.begin_call('lists', 'member')
	g.emit_var(left_tmp)
	g.emit_sep()
	g.emit_var(right_tmp)
	g.end_call()
	g.end_clause()
	g.end_case()
	g.end_call()
	g.end_let()
	g.end_let()
	return true
}

fn (mut g CoreGen) core_infix_left_shift(node ast.InfixExpr) bool {
	if node.op != .left_shift {
		return false
	}
	left_type_sym := g.table.sym(node.left_type)
	if left_type_sym.kind != .array && !left_type_sym.name.starts_with('[]') {
		return false
	}
	g.begin_call('erlang', '++')
	g.core_expr(node.left)
	g.emit_sep()
	g.begin_cons()
	g.core_expr(node.right)
	g.mid_cons()
	g.emit_nil()
	g.end_cons()
	g.end_call()
	return true
}

fn (mut g CoreGen) core_infix_and(node ast.InfixExpr) bool {
	if node.op != .and {
		return false
	}
	if g.etf_mode {
		g.begin_case()
		g.core_expr(node.left)
		g.mid_case_clauses()
		g.begin_clause()
		g.emit_atom('true')
		g.mid_clause_guard()
		g.emit_true_guard()
		g.mid_clause_body()
		g.core_expr(node.right)
		g.end_clause()
		g.clause_sep()
		g.begin_clause()
		g.emit_atom('false')
		g.mid_clause_guard()
		g.emit_true_guard()
		g.mid_clause_body()
		g.emit_atom('false')
		g.end_clause()
		g.end_case()
	} else {
		g.write_core('case ')
		g.core_expr(node.left)
		g.write_core(" of <'true'> when 'true' -> ")
		g.core_expr(node.right)
		g.write_core(" <'false'> when 'true' -> 'false' end")
	}
	return true
}

fn (mut g CoreGen) core_infix_or(node ast.InfixExpr) bool {
	if node.op != .logical_or {
		return false
	}
	if g.etf_mode {
		g.core_infix_or_etf(node)
	} else {
		g.core_infix_or_text(node)
	}
	return true
}

fn (mut g CoreGen) core_infix_or_etf(node ast.InfixExpr) {
	g.begin_case()
	g.core_expr(node.left)
	g.mid_case_clauses()
	g.begin_clause()
	g.emit_atom('true')
	g.mid_clause_guard()
	g.emit_true_guard()
	g.mid_clause_body()
	g.emit_atom('true')
	g.end_clause()
	g.clause_sep()
	g.begin_clause()
	g.emit_atom('false')
	g.mid_clause_guard()
	g.emit_true_guard()
	g.mid_clause_body()
	g.core_expr(node.right)
	g.end_clause()
	g.end_case()
}

fn (mut g CoreGen) core_infix_or_text(node ast.InfixExpr) {
	g.write_core('case ')
	g.core_expr(node.left)
	g.write_core(" of <'true'> when 'true' -> 'true' <'false'> when 'true' -> ")
	g.core_expr(node.right)
	g.write_core(' end')
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
		return type_sym.kind == .int || type_sym.kind == .i8 || type_sym.kind == .i16
			|| type_sym.kind == .i32 || type_sym.kind == .i64 || type_sym.kind == .u8
			|| type_sym.kind == .u16 || type_sym.kind == .u32 || type_sym.kind == .u64
			|| type_sym.kind == .int_literal
	}
	return false
}

fn (mut g CoreGen) core_prefix_expr(node ast.PrefixExpr) {
	match node.op {
		.not {
			g.begin_call('erlang', 'not')
			g.core_expr(node.right)
			g.end_call()
		}
		.minus {
			g.begin_call('erlang', '-')
			g.core_expr(node.right)
			g.end_call()
		}
		.bit_not {
			g.begin_call('erlang', 'bnot')
			g.core_expr(node.right)
			g.end_call()
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
	if g.core_string_inter_trivial(node) {
		return
	}
	parts := g.core_string_inter_parts(node)
	g.core_string_inter_emit_parts(parts)
}

fn (mut g CoreGen) core_string_inter_trivial(node ast.StringInterLiteral) bool {
	if node.vals.len == 0 && node.exprs.len == 0 {
		if g.etf_mode {
			g.emit_binary('')
		} else {
			g.write_core(core_bitstring(''))
		}
		return true
	}
	if node.exprs.len == 1 && node.expr_types.len >= 1 && node.vals.len == 2
		&& node.vals[0].len == 0 && node.vals[1].len == 0 {
		g.core_to_binary_expr(node.exprs[0], node.expr_types[0])
		return true
	}
	return false
}

fn (mut g CoreGen) core_string_inter_parts(node ast.StringInterLiteral) []string {
	mut parts := []string{}
	for i, val in node.vals {
		if val.len > 0 {
			if g.etf_mode {
				mut buf := strings.new_builder(64)
				old_out := g.out
				g.out = buf
				g.emit_binary(core_escape_binary(val))
				parts << g.out.str()
				g.out = old_out
			} else {
				parts << core_bitstring(val)
			}
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
	return parts
}

fn (mut g CoreGen) core_string_inter_emit_parts(parts []string) {
	if parts.len == 0 {
		if g.etf_mode {
			g.emit_binary('')
		} else {
			g.write_core(core_bitstring(''))
		}
		return
	}
	if parts.len == 1 {
		g.write_core(parts[0])
		return
	}
	if g.etf_mode {
		g.begin_call('erlang', 'iolist_to_binary')
		g.core_write_cons_list(parts)
		g.end_call()
	} else {
		g.write_core("call 'erlang':'iolist_to_binary'(")
		g.core_write_cons_list(parts)
		g.write_core(')')
	}
}

// core_write_cons_list writes a proper cons list from string parts
fn (mut g CoreGen) core_write_cons_list(parts []string) {
	if parts.len == 0 {
		if g.etf_mode {
			g.emit_nil()
		} else {
			g.write_core('[]')
		}
		return
	}
	if g.etf_mode {
		for part in parts {
			g.begin_cons()
			g.write_core(part) // part is already formatted for current mode
			g.mid_cons()
		}
		g.emit_nil()
		for _ in parts {
			g.end_cons()
		}
	} else {
		for part in parts {
			g.write_core('[${part}|')
		}
		g.write_core('[]')
		for _ in parts {
			g.write_core(']')
		}
	}
}

fn (mut g CoreGen) core_selector_expr(node ast.SelectorExpr) {
	field := node.field_name

	if field == 'len' {
		expr_type := node.expr_type
		if int(expr_type) != 0 {
			type_sym := g.table.sym(expr_type)
			if type_sym.kind == .map || type_sym.name.starts_with('map[') {
				g.begin_call('maps', 'size')
				g.core_expr(node.expr)
				g.end_call()
				return
			}
			// Strings are binaries in Erlang — use byte_size, not length
			if type_sym.kind == .string {
				g.begin_call('erlang', 'byte_size')
				g.core_expr(node.expr)
				g.end_call()
				return
			}
		}
		g.begin_call('erlang', 'length')
		g.core_expr(node.expr)
		g.end_call()
		return
	}

	// Field access: maps:get(field, Obj)
	g.begin_call('erlang', 'map_get')
	g.emit_atom(field)
	g.emit_sep()
	g.core_expr(node.expr)
	g.end_call()
}

fn (mut g CoreGen) core_array_init(node ast.ArrayInit) {
	if node.exprs.len == 0 {
		if g.etf_mode {
			g.emit_nil()
		} else {
			g.write_core('[]')
		}
		return
	}
	if g.etf_mode {
		// Build cons list: {c_cons,[],H,{c_cons,[],H2,...{c_literal,[],[]}}}
		for i, expr in node.exprs {
			_ = i
			g.begin_cons()
			g.core_expr(expr)
			g.mid_cons()
		}
		g.emit_nil()
		for _ in node.exprs {
			g.end_cons()
		}
	} else {
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
}

fn (mut g CoreGen) core_map_init(node ast.MapInit) {
	if g.etf_mode {
		// ETF: call 'maps':'from_list'([{K,V}|...])
		g.begin_call('maps', 'from_list')
		if node.keys.len == 0 {
			g.emit_nil()
		} else {
			for i, key in node.keys {
				g.begin_cons()
				g.begin_tuple()
				g.core_expr(key)
				g.emit_sep()
				g.core_expr(node.vals[i])
				g.end_tuple()
				g.mid_cons()
				_ = i
			}
			g.emit_nil()
			for _ in node.keys {
				g.end_cons()
			}
		}
		g.end_call()
	} else {
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
}

fn (mut g CoreGen) core_struct_init(node ast.StructInit) {
	type_sym := g.table.sym(node.typ)
	type_name := if type_sym.name.contains('[') {
		type_sym.name.all_before('[').all_after_last('.')
	} else {
		type_sym.name.all_after_last('.')
	}
	if g.etf_mode {
		g.core_struct_init_etf(node, type_name)
	} else {
		g.core_struct_init_text(node, type_name)
	}
}

fn (mut g CoreGen) core_struct_init_etf(node ast.StructInit, type_name string) {
	g.begin_call('maps', 'from_list')
	mut idx := 0
	for field in node.init_fields {
		g.begin_cons()
		g.begin_tuple()
		g.emit_atom(field.name)
		g.emit_sep()
		g.core_expr(field.expr)
		g.end_tuple()
		g.mid_cons()
		idx++
	}
	g.begin_cons()
	g.begin_tuple()
	g.begin_tuple()
	g.emit_atom('vbeam')
	g.emit_sep()
	g.emit_atom('type')
	g.end_tuple()
	g.emit_sep()
	g.emit_atom(type_name)
	g.end_tuple()
	g.mid_cons()
	g.emit_nil()
	g.end_cons()
	for _ in 0 .. idx {
		g.end_cons()
	}
	g.end_call()
}

fn (mut g CoreGen) core_struct_init_text(node ast.StructInit, type_name string) {
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
			g.begin_call('erlang', 'map_get')
			g.core_expr(node.index)
			g.emit_sep()
			g.core_expr(node.left)
			g.end_call()
			return
		}
	}

	// Array access: lists:nth(I + 1, Arr)
	g.begin_call('lists', 'nth')
	if node.index is ast.IntegerLiteral {
		idx := node.index.val.int() + 1
		g.emit_int('${idx}')
	} else {
		g.begin_call('erlang', '+')
		g.core_expr(node.index)
		g.emit_sep()
		g.emit_int('1')
		g.end_call()
	}
	g.emit_sep()
	g.core_expr(node.left)
	g.end_call()
}

fn (mut g CoreGen) core_if_expr(node ast.IfExpr) {
	if node.is_comptime {
		// TODO: comptime if
		g.emit_atom('ok')
		return
	}
	g.core_if_branches(node.branches, 0)
}

fn (mut g CoreGen) core_if_branches(branches []ast.IfBranch, idx int) {
	if idx >= branches.len {
		g.emit_atom('ok')
		return
	}

	branch := branches[idx]
	is_last := idx == branches.len - 1
	is_else := is_last && (branch.cond is ast.NodeError || branch.cond is ast.EmptyExpr)

	if is_else {
		g.core_branch_value(branch)
	} else if branch.cond is ast.IfGuardExpr {
		g.core_if_guard_branch(branch.cond, branch, branches, idx)
	} else if g.etf_mode {
		// ETF: {c_case,[],COND,[{c_clause,[],[true],true,BODY},{c_clause,[],[false],true,ELSE}]}
		g.begin_case()
		g.core_expr(branch.cond)
		g.mid_case_clauses()
		// true clause
		g.begin_clause()
		g.emit_atom('true')
		g.mid_clause_guard()
		g.emit_true_guard()
		g.mid_clause_body()
		g.core_branch_value(branch)
		g.end_clause()
		g.clause_sep()
		// false clause
		g.begin_clause()
		g.emit_atom('false')
		g.mid_clause_guard()
		g.emit_true_guard()
		g.mid_clause_body()
		if idx + 1 < branches.len {
			g.core_if_branches(branches, idx + 1)
		} else {
			g.emit_atom('ok')
		}
		g.end_clause()
		g.end_case()
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

fn (mut g CoreGen) core_if_guard_branch(guard ast.IfGuardExpr, branch ast.IfBranch, branches []ast.IfBranch, idx int) {
	// `if value := expr {` → pattern match on {ok, Value} vs error
	// Register guard variables
	mut var_names := []string{}
	for gvar in guard.vars {
		vname := g.next_core_var(gvar.name)
		var_names << vname
	}

	// Determine if this is a map lookup (IndexExpr on map type)
	is_map_lookup := if guard.expr is ast.IndexExpr {
		guard.expr.left_type.idx() != 0 && g.table.sym(guard.expr.left_type).kind == .map
	} else {
		false
	}

	if g.etf_mode {
		g.core_if_guard_branch_etf(guard, branch, branches, idx, var_names, is_map_lookup)
	} else {
		g.core_if_guard_branch_text(guard, branch, branches, idx, var_names, is_map_lookup)
	}
}

// core_if_guard_branch_etf handles if-guard expressions in ETF mode
fn (mut g CoreGen) core_if_guard_branch_etf(guard ast.IfGuardExpr, branch ast.IfBranch, branches []ast.IfBranch, idx int, var_names []string, is_map_lookup bool) {
	// ETF mode: build case expression with proper AST terms
	g.begin_case()
	g.core_if_guard_etf_condition(guard, is_map_lookup)
	g.mid_case_clauses()
	g.core_if_guard_etf_success_clause(branch, var_names)
	g.clause_sep()
	g.core_if_guard_etf_wildcard_clause(branches, idx)
	g.end_case()
}

// core_if_guard_etf_condition emits the guard condition expression in ETF mode
fn (mut g CoreGen) core_if_guard_etf_condition(guard ast.IfGuardExpr, is_map_lookup bool) {
	if is_map_lookup {
		index_expr := guard.expr as ast.IndexExpr
		g.begin_call('maps', 'find')
		g.core_expr(index_expr.index)
		g.emit_sep()
		g.core_expr(index_expr.left)
		g.end_call()
	} else {
		g.temp_counter++
		tmp := '_cor${g.temp_counter}'
		g.begin_try()
		g.core_expr(guard.expr)
		g.mid_try_of(tmp)
		// {ok, Tmp}
		g.begin_tuple()
		g.emit_atom('ok')
		g.emit_sep()
		g.emit_var(tmp)
		g.end_tuple()
		g.mid_try_catch('_cor_c', '_cor_r', '_cor_s')
		g.emit_atom('error')
		g.end_try()
	}
}

// core_if_guard_etf_success_clause emits the success pattern and body in ETF mode
fn (mut g CoreGen) core_if_guard_etf_success_clause(branch ast.IfBranch, var_names []string) {
	g.begin_clause()
	g.core_if_guard_etf_success_pattern(var_names)
	g.mid_clause_guard()
	g.emit_true_guard()
	g.mid_clause_body()
	g.core_branch_value(branch)
	g.end_clause()
}

// core_if_guard_etf_success_pattern emits the {ok, Var} pattern in ETF mode
fn (mut g CoreGen) core_if_guard_etf_success_pattern(var_names []string) {
	if var_names.len == 1 {
		g.begin_tuple()
		g.emit_atom('ok')
		g.emit_sep()
		g.emit_var(var_names[0])
		g.end_tuple()
	} else if var_names.len > 1 {
		g.begin_tuple()
		g.emit_atom('ok')
		g.emit_sep()
		g.begin_tuple()
		for i, vn in var_names {
			if i > 0 {
				g.emit_sep()
			}
			g.emit_var(vn)
		}
		g.end_tuple()
		g.end_tuple()
	} else {
		g.begin_tuple()
		g.emit_atom('ok')
		g.emit_sep()
		g.emit_var('_')
		g.end_tuple()
	}
}

// core_if_guard_etf_wildcard_clause emits the wildcard clause in ETF mode
fn (mut g CoreGen) core_if_guard_etf_wildcard_clause(branches []ast.IfBranch, idx int) {
	g.begin_clause()
	g.emit_var('_')
	g.mid_clause_guard()
	g.emit_true_guard()
	g.mid_clause_body()
	if idx + 1 < branches.len {
		g.core_if_branches(branches, idx + 1)
	} else {
		g.emit_atom('ok')
	}
	g.end_clause()
}

// core_if_guard_branch_text handles if-guard expressions in text mode
fn (mut g CoreGen) core_if_guard_branch_text(guard ast.IfGuardExpr, branch ast.IfBranch, branches []ast.IfBranch, idx int, var_names []string, is_map_lookup bool) {
	if is_map_lookup {
		// Map lookup: use maps:find/2 which returns {ok, Value} | error
		index_expr := guard.expr as ast.IndexExpr
		g.write_core("case call 'maps':'find'(")
		g.core_expr(index_expr.index)
		g.write_core(', ')
		g.core_expr(index_expr.left)
		g.write_core(')')
	} else {
		// General optional: wrap in try/catch
		g.temp_counter++
		tmp := '_cor${g.temp_counter}'
		g.write_core('case try ')
		g.core_expr(guard.expr)
		g.write_core(' of <${tmp}> when \'true\' -> {\'ok\', ${tmp}}')
		g.write_core(" catch <_cor_c,_cor_r,_cor_s> when 'true' -> 'error'")
	}

	// Pattern match: {ok, Var} -> then, _ -> else
	if var_names.len == 1 {
		g.write_core(" of <{'ok', ${var_names[0]}}> when 'true' -> ")
	} else if var_names.len > 1 {
		// Multiple guard vars: {ok, {V1, V2, ...}}
		g.write_core(" of <{'ok', {${var_names.join(', ')}}}> when 'true' -> ")
	} else {
		// No vars: just check success
		g.write_core(" of <{'ok', _}> when 'true' -> ")
	}

	g.core_branch_value(branch)

	g.write_core(" <_> when 'true' -> ")
	if idx + 1 < branches.len {
		g.core_if_branches(branches, idx + 1)
	} else {
		g.write_core("'ok'")
	}
	g.write_core(' end')
}

fn (mut g CoreGen) core_branch_value(branch ast.IfBranch) {
	if branch.stmts.len == 0 {
		g.emit_atom('ok')
		return
	}

	if branch.stmts.len == 1 {
		stmt := branch.stmts[0]
		match stmt {
			ast.Return {
				if stmt.exprs.len > 0 {
					g.core_expr(stmt.exprs[0])
				} else {
					g.emit_atom('ok')
				}
			}
			ast.ExprStmt {
				g.core_expr(stmt.expr)
			}
			else {
				g.emit_atom('ok')
			}
		}
		return
	}

	// Multiple statements - generate proper let/do chain
	// This ensures variable declarations (let bindings) are visible
	// to subsequent statements in the branch body
	if !g.etf_mode {
		g.out.writeln('')
		g.indent++
	}
	g.core_fn_body(branch.stmts)
	if !g.etf_mode {
		g.indent--
		g.write_indent_core()
	}
}

fn (mut g CoreGen) core_match_expr(node ast.MatchExpr) {
	if node.cond is ast.BoolLiteral && node.cond.val {
		g.core_match_true_as_case(node)
		return
	}
	if g.core_match_has_multi_patterns(node) {
		g.core_match_comparison(node)
		return
	}
	if g.etf_mode {
		g.core_match_expr_etf(node)
	} else {
		g.core_match_expr_text(node)
	}
}

// core_match_comparison generates a match expression using nested
// comparison-based case expressions. Used when any branch has multiple
// patterns (e.g., match x { 'a', 'b' => ... }).
fn (mut g CoreGen) core_match_comparison(node ast.MatchExpr) {
	// Let-bind the condition to prevent re-evaluation with side effects
	g.temp_counter++
	cond_var := '_match_cond_${g.temp_counter}'
	g.begin_let(cond_var)
	g.core_expr(node.cond)
	g.mid_let()
	g.match_cond_var = cond_var
	g.core_match_cmp_branch(node, 0)
	g.match_cond_var = ''
	g.end_let()
}

fn (mut g CoreGen) core_match_cmp_branch(node ast.MatchExpr, idx int) {
	if idx >= node.branches.len {
		g.emit_atom('ok')
		return
	}
	branch := node.branches[idx]
	if branch.is_else {
		g.core_match_branch_val(branch.stmts)
		return
	}
	if g.etf_mode {
		g.core_match_cmp_branch_etf(node, idx, branch)
	} else {
		g.core_match_cmp_branch_text(node, idx, branch)
	}
}

fn (mut g CoreGen) core_match_true_as_case(node ast.MatchExpr) {
	if g.etf_mode {
		g.core_match_true_as_case_etf(node)
	} else {
		g.core_match_true_as_case_text(node)
	}
}

fn (g CoreGen) core_match_has_multi_patterns(node ast.MatchExpr) bool {
	for branch in node.branches {
		if branch.exprs.len > 1 {
			return true
		}
	}
	return false
}

fn (mut g CoreGen) core_match_expr_etf(node ast.MatchExpr) {
	g.begin_case()
	g.core_expr(node.cond)
	g.mid_case_clauses()
	for i, branch in node.branches {
		if i > 0 {
			g.clause_sep()
		}
		g.begin_clause()
		if branch.is_else {
			g.emit_var('_')
		} else if branch.exprs.len == 1 {
			g.core_match_pattern(branch.exprs[0])
		}
		g.mid_clause_guard()
		g.emit_true_guard()
		g.mid_clause_body()
		g.core_match_branch_val(branch.stmts)
		g.end_clause()
	}
	g.end_case()
}

fn (mut g CoreGen) core_match_expr_text(node ast.MatchExpr) {
	g.write_core('case ')
	g.core_expr(node.cond)
	g.write_core(' of ')
	for i, branch in node.branches {
		if i > 0 {
			g.write_core(' ')
		}
		g.write_core('<')
		if branch.is_else {
			g.write_core('_')
		} else if branch.exprs.len == 1 {
			g.core_match_pattern(branch.exprs[0])
		}
		g.write_core("> when 'true' -> ")
		g.core_match_branch_val(branch.stmts)
	}
	g.write_core(' end')
}

fn (mut g CoreGen) core_match_cmp_branch_etf(node ast.MatchExpr, idx int, branch ast.MatchBranch) {
	if branch.exprs.len == 1 {
		g.core_match_cmp_branch_etf_single(node, idx, branch)
	} else {
		g.core_match_cmp_branch_etf_multi(node, idx, branch)
	}
}

fn (mut g CoreGen) core_match_cmp_branch_etf_single(node ast.MatchExpr, idx int, branch ast.MatchBranch) {
	g.begin_case()
	g.begin_call('erlang', '=:=')
	g.emit_var(g.match_cond_var)
	g.emit_sep()
	g.core_expr(branch.exprs[0])
	g.end_call()
	g.mid_case_clauses()
	g.begin_clause()
	g.emit_atom('true')
	g.mid_clause_guard()
	g.emit_true_guard()
	g.mid_clause_body()
	g.core_match_branch_val(branch.stmts)
	g.end_clause()
	g.clause_sep()
	g.begin_clause()
	g.emit_atom('false')
	g.mid_clause_guard()
	g.emit_true_guard()
	g.mid_clause_body()
	g.core_match_cmp_branch(node, idx + 1)
	g.end_clause()
	g.end_case()
}

fn (mut g CoreGen) core_match_cmp_branch_etf_multi(node ast.MatchExpr, idx int, branch ast.MatchBranch) {
	for j, expr in branch.exprs {
		g.begin_case()
		g.begin_call('erlang', '=:=')
		g.emit_var(g.match_cond_var)
		g.emit_sep()
		g.core_expr(expr)
		g.end_call()
		g.mid_case_clauses()
		g.begin_clause()
		g.emit_atom('true')
		g.mid_clause_guard()
		g.emit_true_guard()
		g.mid_clause_body()
		g.core_match_branch_val(branch.stmts)
		g.end_clause()
		g.clause_sep()
		g.begin_clause()
		g.emit_atom('false')
		g.mid_clause_guard()
		g.emit_true_guard()
		g.mid_clause_body()
		if j == branch.exprs.len - 1 {
			g.core_match_cmp_branch(node, idx + 1)
		}
	}
	for _ in branch.exprs {
		g.end_clause()
		g.end_case()
	}
}

fn (mut g CoreGen) core_match_cmp_branch_text(node ast.MatchExpr, idx int, branch ast.MatchBranch) {
	if branch.exprs.len == 1 {
		g.core_match_cmp_branch_text_single(node, idx, branch)
	} else {
		g.core_match_cmp_branch_text_multi(node, idx, branch)
	}
}

fn (mut g CoreGen) core_match_cmp_branch_text_single(node ast.MatchExpr, idx int, branch ast.MatchBranch) {
	g.write_core("case call 'erlang':'=:='(")
	g.write_core(g.match_cond_var)
	g.write_core(', ')
	g.core_expr(branch.exprs[0])
	g.write_core(')')
	g.write_core(" of <'true'> when 'true' -> ")
	g.core_match_branch_val(branch.stmts)
	g.write_core(" <'false'> when 'true' -> ")
	g.core_match_cmp_branch(node, idx + 1)
	g.write_core(' end')
}

fn (mut g CoreGen) core_match_cmp_branch_text_multi(node ast.MatchExpr, idx int, branch ast.MatchBranch) {
	for j, expr in branch.exprs {
		g.write_core("case call 'erlang':'=:='(")
		g.write_core(g.match_cond_var)
		g.write_core(', ')
		g.core_expr(expr)
		g.write_core(')')
		g.write_core(" of <'true'> when 'true' -> ")
		g.core_match_branch_val(branch.stmts)
		g.write_core(" <'false'> when 'true' -> ")
		if j == branch.exprs.len - 1 {
			g.core_match_cmp_branch(node, idx + 1)
		}
	}
	for _ in branch.exprs {
		g.write_core(' end')
	}
}

fn (mut g CoreGen) core_match_true_as_case_etf(node ast.MatchExpr) {
	mut case_count := 0
	for i, branch in node.branches {
		if branch.is_else {
			g.core_match_branch_val(branch.stmts)
		} else if branch.exprs.len > 0 {
			for expr in branch.exprs {
				g.begin_case()
				g.core_expr(expr)
				g.mid_case_clauses()
				g.begin_clause()
				g.emit_atom('true')
				g.mid_clause_guard()
				g.emit_true_guard()
				g.mid_clause_body()
				g.core_match_branch_val(branch.stmts)
				g.end_clause()
				g.clause_sep()
				g.begin_clause()
				g.emit_atom('false')
				g.mid_clause_guard()
				g.emit_true_guard()
				g.mid_clause_body()
				case_count++
			}
			if i + 1 >= node.branches.len {
				g.emit_atom('ok')
			}
		}
	}
	for _ in 0 .. case_count {
		g.end_clause()
		g.end_case()
	}
}

fn (mut g CoreGen) core_match_true_as_case_text(node ast.MatchExpr) {
	mut end_count := 0
	for i, branch in node.branches {
		if branch.is_else {
			g.core_match_branch_val(branch.stmts)
		} else if branch.exprs.len > 0 {
			for expr in branch.exprs {
				g.write_core('case ')
				g.core_expr(expr)
				g.write_core(" of <'true'> when 'true' -> ")
				g.core_match_branch_val(branch.stmts)
				g.write_core(" <'false'> when 'true' -> ")
				end_count++
			}
			if i + 1 >= node.branches.len {
				g.write_core("'ok'")
			}
		}
	}
	for _ in 0 .. end_count {
		g.write_core(' end')
	}
}

fn (mut g CoreGen) core_match_pattern(expr ast.Expr) {
	if g.etf_mode {
		match expr {
			ast.EnumVal {
				g.emit_atom(expr.val)
			}
			ast.Ident {
				if expr.name == '_' {
					g.emit_var('_')
				} else {
					g.emit_var(g.core_var(expr.name))
				}
			}
			else {
				g.core_expr(expr)
			}
		}
	} else {
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
}

fn (mut g CoreGen) core_match_branch_val(stmts []ast.Stmt) {
	if stmts.len == 0 {
		g.emit_atom('ok')
		return
	}
	if stmts.len == 1 {
		// Single statement — handle directly as return value
		last := stmts[0]
		match last {
			ast.Return {
				if last.exprs.len > 0 {
					g.core_expr(last.exprs[0])
				} else {
					g.emit_atom('ok')
				}
			}
			ast.ExprStmt {
				g.core_expr(last.expr)
			}
			else {
				g.emit_atom('ok')
			}
		}
		return
	}
	// Multiple statements — use core_body_chain which handles
	// let-bindings, sequencing, and tail position correctly
	g.core_body_chain(stmts, 0, false)
}

fn (mut g CoreGen) core_enum_val(node ast.EnumVal) {
	g.emit_atom(node.val)
}

// V select { branch } → Core Erlang receive expression
// V's select maps to Erlang's receive for channel/message operations.
// SelectBranch.stmt is a channel op (a := <-ch or ch <- a)
// SelectBranch.stmts is the body
fn (mut g CoreGen) core_select_expr(node ast.SelectExpr) {
	if g.etf_mode {
		g.core_select_expr_etf(node)
	} else {
		g.core_select_expr_text(node)
	}
}

fn (mut g CoreGen) core_select_expr_etf(node ast.SelectExpr) {
	g.begin_receive()
	has_timeout := g.core_select_expr_etf_clauses(node)
	g.mid_receive_after()
	g.core_select_expr_etf_timeout(node, has_timeout)
	g.end_receive()
}

fn (mut g CoreGen) core_select_expr_etf_clauses(node ast.SelectExpr) bool {
	mut has_timeout := false
	mut clause_idx := 0
	for branch in node.branches {
		if branch.is_timeout || branch.is_else {
			has_timeout = true
			continue
		}
		if clause_idx > 0 {
			g.clause_sep()
		}
		g.core_select_expr_etf_emit_clause(branch)
		clause_idx++
	}
	return has_timeout
}

fn (mut g CoreGen) core_select_expr_etf_emit_clause(branch ast.SelectBranch) {
	g.temp_counter++
	msg_var := '_msg${g.temp_counter}'
	g.begin_clause()
	g.emit_var(msg_var)
	g.mid_clause_guard()
	g.emit_true_guard()
	g.mid_clause_body()
	stmt := branch.stmt
	if stmt is ast.AssignStmt {
		for left in stmt.left {
			if left is ast.Ident {
				vname := g.next_core_var(left.name)
				g.begin_let(vname)
				g.emit_var(msg_var)
				g.mid_let()
			}
		}
	}
	if branch.stmts.len > 0 {
		g.core_match_branch_val(branch.stmts)
	} else {
		g.emit_atom('ok')
	}
	if stmt is ast.AssignStmt {
		for left in stmt.left {
			if left is ast.Ident {
				g.end_let()
			}
		}
	}
	g.end_clause()
}

fn (mut g CoreGen) core_select_expr_etf_timeout(node ast.SelectExpr, has_timeout bool) {
	if !has_timeout {
		g.emit_atom('infinity')
		g.mid_receive_action()
		g.emit_atom('ok')
		return
	}
	for branch in node.branches {
		if branch.is_timeout {
			stmt := branch.stmt
			if stmt is ast.ExprStmt {
				g.core_expr(stmt.expr)
			} else {
				g.emit_atom('infinity')
			}
			g.mid_receive_action()
			g.core_select_expr_timeout_body_etf(branch)
		} else if branch.is_else {
			g.emit_int('0')
			g.mid_receive_action()
			g.core_select_expr_timeout_body_etf(branch)
		}
	}
}

fn (mut g CoreGen) core_select_expr_timeout_body_etf(branch ast.SelectBranch) {
	if branch.stmts.len > 0 {
		g.core_match_branch_val(branch.stmts)
	} else {
		g.emit_atom('ok')
	}
}

fn (mut g CoreGen) core_select_expr_text(node ast.SelectExpr) {
	g.write_core('receive ')
	has_timeout := g.core_select_expr_text_clauses(node)
	g.core_select_expr_text_timeout(node, has_timeout)
	g.write_core(' end')
}

fn (mut g CoreGen) core_select_expr_text_clauses(node ast.SelectExpr) bool {
	mut has_timeout := false
	mut clause_idx := 0
	for branch in node.branches {
		if branch.is_timeout || branch.is_else {
			has_timeout = true
			continue
		}
		if clause_idx > 0 {
			g.write_core(' ')
		}
		g.core_select_expr_text_emit_clause(branch)
		clause_idx++
	}
	return has_timeout
}

fn (mut g CoreGen) core_select_expr_text_emit_clause(branch ast.SelectBranch) {
	g.temp_counter++
	msg_var := '_msg${g.temp_counter}'
	g.write_core("<${msg_var}> when 'true' -> ")
	stmt := branch.stmt
	if stmt is ast.AssignStmt {
		for left in stmt.left {
			if left is ast.Ident {
				vname := g.next_core_var(left.name)
				g.write_core('let <${vname}> = ${msg_var} in ')
			}
		}
	}
	if branch.stmts.len > 0 {
		g.core_match_branch_val(branch.stmts)
	} else {
		g.write_core("'ok'")
	}
}

fn (mut g CoreGen) core_select_expr_text_timeout(node ast.SelectExpr, has_timeout bool) {
	if !has_timeout {
		g.write_core(" after 'infinity' -> 'ok'")
		return
	}
	for branch in node.branches {
		if branch.is_timeout {
			g.write_core(' after ')
			stmt := branch.stmt
			if stmt is ast.ExprStmt {
				g.core_expr(stmt.expr)
			} else {
				g.write_core('infinity')
			}
			g.write_core(' -> ')
			g.core_select_expr_timeout_body_text(branch)
		} else if branch.is_else {
			g.write_core(' after 0 -> ')
			g.core_select_expr_timeout_body_text(branch)
		}
	}
}

fn (mut g CoreGen) core_select_expr_timeout_body_text(branch ast.SelectBranch) {
	if branch.stmts.len > 0 {
		g.core_match_branch_val(branch.stmts)
	} else {
		g.write_core("'ok'")
	}
}
