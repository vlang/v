// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module gen

import v.ast
import v.table
import v.util

fn (mut g Gen) gen_fn_decl(it ast.FnDecl, skip bool) {
	// TODO For some reason, build fails with autofree with this line
	// as it's only informative, comment it for now
	// g.gen_attrs(it.attrs)
	if it.language == .c {
		// || it.no_body {
		return
	}
	g.returned_var_name = ''
	// if g.fileis('vweb.v') {
	// println('\ngen_fn_decl() $it.name $it.is_generic $g.cur_generic_type')
	// }
	if it.is_generic && g.cur_generic_type == 0 { // need the cur_generic_type check to avoid inf. recursion
		// loop thru each generic type and generate a function
		for gen_type in g.table.fn_gen_types[it.name] {
			sym := g.table.get_type_symbol(gen_type)
			if g.pref.is_verbose {
				println('gen fn `$it.name` for type `$sym.name`')
			}
			g.cur_generic_type = gen_type
			g.gen_fn_decl(it, skip)
		}
		g.cur_generic_type = 0
		return
	}
	// g.cur_fn = it
	fn_start_pos := g.out.len
	g.write_v_source_line_info(it.pos)
	msvc_attrs := g.write_fn_attrs(it.attrs)
	// Live
	is_livefn := it.attrs.contains('live')
	is_livemain := g.pref.is_livemain && is_livefn
	is_liveshared := g.pref.is_liveshared && is_livefn
	is_livemode := g.pref.is_livemain || g.pref.is_liveshared
	is_live_wrap := is_livefn && is_livemode
	if is_livefn && !is_livemode {
		eprintln('INFO: compile with `v -live $g.pref.path `, if you want to use the [live] function $it.name .')
	}
	//
	mut name := it.name
	if name[0] in [`+`, `-`, `*`, `/`, `%`] {
		name = util.replace_op(name)
	}
	if it.is_method {
		name = g.cc_type2(it.receiver.typ) + '_' + name
		// name = g.table.get_type_symbol(it.receiver.typ).name + '_' + name
	}
	if it.language == .c {
		name = util.no_dots(name)
	} else {
		name = c_name(name)
	}
	mut type_name := g.typ(it.return_type)
	if g.cur_generic_type != 0 {
		// foo<T>() => foo_T_int(), foo_T_string() etc
		gen_name := g.typ(g.cur_generic_type)
		// Using _T_ to differentiate between get<string> and get_string
		name += '_T_' + gen_name
	}
	// if g.pref.show_cc && it.is_builtin {
	// println(name)
	// }
	// type_name := g.table.Type_to_str(it.return_type)
	// Live functions are protected by a mutex, because otherwise they
	// can be changed by the live reload thread, *while* they are
	// running, with unpredictable results (usually just crashing).
	// For this purpose, the actual body of the live function,
	// is put under a non publicly accessible function, that is prefixed
	// with 'impl_live_' .
	if is_livemain {
		g.hotcode_fn_names << name
	}
	mut impl_fn_name := name
	if is_live_wrap {
		impl_fn_name = 'impl_live_$name'
	}
	g.last_fn_c_name = impl_fn_name
	//
	if is_live_wrap {
		if is_livemain {
			g.definitions.write('$type_name (* $impl_fn_name)(')
			g.write('$type_name no_impl_${name}(')
		}
		if is_liveshared {
			g.definitions.write('$type_name ${impl_fn_name}(')
			g.write('$type_name ${impl_fn_name}(')
		}
	} else {
		if !(it.is_pub || g.pref.is_debug) {
			// Private functions need to marked as static so that they are not exportable in the
			// binaries
			if g.pref.build_mode != .build_module {
				// if !(g.pref.build_mode == .build_module && g.is_builtin_mod) {
				// If we are building vlib/builtin, we need all private functions like array_get
				// to be public, so that all V programs can access them.
				g.write('VV_LOCAL_SYMBOL ')
				g.definitions.write('VV_LOCAL_SYMBOL ')
			}
		}
		fn_header := if msvc_attrs.len > 0 {
			'$type_name $msvc_attrs ${name}('
		} else {
			'$type_name ${name}('
		}
		g.definitions.write(fn_header)
		g.write(fn_header)
	}
	arg_start_pos := g.out.len
	fargs, fargtypes := g.fn_args(it.params, it.is_variadic)
	arg_str := g.out.after(arg_start_pos)
	if it.no_body ||
		((g.pref.use_cache && g.pref.build_mode != .build_module) && it.is_builtin) || skip {
		// Just a function header. Builtin function bodies are defined in builtin.o
		g.definitions.writeln(');') // // NO BODY')
		g.writeln(');')
		return
	}
	g.definitions.writeln(');')
	g.writeln(') {')
	if is_live_wrap {
		// The live function just calls its implementation dual, while ensuring
		// that the call is wrapped by the mutex lock & unlock calls.
		// Adding the mutex lock/unlock inside the body of the implementation
		// function is not reliable, because the implementation function can do
		// an early exit, which will leave the mutex locked.
		mut fn_args_list := []string{}
		for ia, fa in fargs {
			fn_args_list << '${fargtypes[ia]} $fa'
		}
		mut live_fncall := '${impl_fn_name}(' + fargs.join(', ') + ');'
		mut live_fnreturn := ''
		if type_name != 'void' {
			live_fncall = '$type_name res = $live_fncall'
			live_fnreturn = 'return res;'
		}
		g.definitions.writeln('$type_name ${name}(' + fn_args_list.join(', ') + ');')
		g.hotcode_definitions.writeln('$type_name ${name}(' + fn_args_list.join(', ') + '){')
		g.hotcode_definitions.writeln('  pthread_mutex_lock(&live_fn_mutex);')
		g.hotcode_definitions.writeln('  $live_fncall')
		g.hotcode_definitions.writeln('  pthread_mutex_unlock(&live_fn_mutex);')
		g.hotcode_definitions.writeln('  $live_fnreturn')
		g.hotcode_definitions.writeln('}')
	}
	// Profiling mode? Start counting at the beginning of the function (save current time).
	if g.pref.is_prof {
		g.profile_fn(it)
	}
	// we could be in an anon fn so save outer fn defer stmts
	prev_defer_stmts := g.defer_stmts
	g.defer_stmts = []
	g.stmts(it.stmts)
	//
	if it.return_type == table.void_type {
		g.write_defer_stmts_when_needed()
	}
	if it.is_anon {
		g.defer_stmts = prev_defer_stmts
	} else {
		g.defer_stmts = []
	}
	if it.return_type != table.void_type && it.stmts.len > 0 && it.stmts.last() !is ast.Return {
		default_expr := g.type_default(it.return_type)
		// TODO: perf?
		if default_expr == '{0}' {
			g.writeln('\treturn ($type_name)$default_expr;')
		} else {
			g.writeln('\treturn $default_expr;')
		}
	}
	g.writeln('}')
	if g.pref.printfn_list.len > 0 && g.last_fn_c_name in g.pref.printfn_list {
		println(g.out.after(fn_start_pos))
	}
	for attr in it.attrs {
		if attr.name == 'export' {
			g.writeln('// export alias: $attr.arg -> $name')
			export_alias := '$type_name ${attr.arg}($arg_str)'
			g.definitions.writeln('VV_EXPORTED_SYMBOL $export_alias; // exported fn $it.name')
			g.writeln('$export_alias {')
			g.write('\treturn ${name}(')
			g.write(fargs.join(', '))
			g.writeln(');')
			g.writeln('}')
		}
	}
}

fn (mut g Gen) write_defer_stmts_when_needed() {
	if g.defer_stmts.len > 0 {
		g.write_defer_stmts()
	}
	if g.defer_profile_code.len > 0 {
		g.writeln('')
		g.writeln('\t// defer_profile_code')
		g.writeln(g.defer_profile_code)
		g.writeln('')
	}
}

// fn decl args
fn (mut g Gen) fn_args(args []table.Param, is_variadic bool) ([]string, []string) {
	mut fargs := []string{}
	mut fargtypes := []string{}
	for i, arg in args {
		caname := c_name(arg.name)
		typ := g.unwrap_generic(arg.typ)
		arg_type_sym := g.table.get_type_symbol(typ)
		mut arg_type_name := g.typ(typ) // util.no_dots(arg_type_sym.name)
		if arg_type_sym.kind == .function {
			info := arg_type_sym.info as table.FnType
			func := info.func
			if !info.is_anon {
				g.write(arg_type_name + ' ' + caname)
				g.definitions.write(arg_type_name + ' ' + caname)
				fargs << caname
				fargtypes << arg_type_name
			} else {
				g.write('${g.typ(func.return_type)} (*$caname)(')
				g.definitions.write('${g.typ(func.return_type)} (*$caname)(')
				g.fn_args(func.params, func.is_variadic)
				g.write(')')
				g.definitions.write(')')
			}
		} else {
			s := arg_type_name + ' ' + caname
			g.write(s)
			g.definitions.write(s)
			fargs << caname
			fargtypes << arg_type_name
		}
		if i < args.len - 1 {
			g.write(', ')
			g.definitions.write(', ')
		}
	}
	return fargs, fargtypes
}

fn (mut g Gen) call_expr(node ast.CallExpr) {
	// NOTE: everything could be done this way
	// see my comment in parser near anon_fn
	if node.left is ast.AnonFn {
		g.expr(node.left)
	}
	if node.left is ast.IndexExpr && node.name == '' {
		g.is_fn_index_call = true
		g.expr(node.left)
		g.is_fn_index_call = false
	}
	if node.should_be_skipped {
		return
	}
	g.inside_call = true
	defer {
		g.inside_call = false
	}
	gen_or := node.or_block.kind != .absent && !g.pref.autofree
	// if gen_or {
	// g.writeln('/*start*/')
	// }
	is_gen_or_and_assign_rhs := gen_or && g.is_assign_rhs
	cur_line := if is_gen_or_and_assign_rhs && !g.pref.autofree {
		line := g.go_before_stmt(0)
		g.out.write(tabs[g.indent])
		line
	} else {
		''
	}
	tmp_opt := if gen_or { g.new_tmp_var() } else { '' }
	if gen_or {
		styp := g.typ(node.return_type.set_flag(.optional))
		g.write('$styp $tmp_opt = ')
	}
	if node.is_method && !node.is_field {
		if node.name == 'writeln' && g.pref.experimental && node.args.len > 0 && node.args[0].expr is
			ast.StringInterLiteral && g.table.get_type_symbol(node.receiver_type).name == 'strings.Builder' {
			g.string_inter_literal_sb_optimized(node)
		} else {
			g.method_call(node)
		}
	} else {
		g.fn_call(node)
	}
	if gen_or { // && !g.pref.autofree {
		if !g.pref.autofree {
			g.or_block(tmp_opt, node.or_block, node.return_type)
		}
		if is_gen_or_and_assign_rhs {
			g.write('\n $cur_line $tmp_opt')
			// g.write('\n /*call_expr cur_line:*/ $cur_line /*C*/ $tmp_opt /*end*/')
			// g.insert_before_stmt('\n /* VVV */ $tmp_opt')
		}
	}
}

[inline]
pub fn (g &Gen) unwrap_generic(typ table.Type) table.Type {
	if typ.has_flag(.generic) {
		// return g.cur_generic_type
		return g.cur_generic_type.derive(typ).clear_flag(.generic)
	}
	return typ
}

fn (mut g Gen) method_call(node ast.CallExpr) {
	// TODO: there are still due to unchecked exprs (opt/some fn arg)
	if node.left_type == 0 {
		g.checker_bug('CallExpr.left_type is 0 in method_call', node.pos)
	}
	if node.receiver_type == 0 {
		g.checker_bug('CallExpr.receiver_type is 0 in method_call', node.pos)
	}
	// mut receiver_type_name := g.cc_type(node.receiver_type)
	// mut receiver_type_name := g.typ(node.receiver_type)
	typ_sym := g.table.get_type_symbol(g.unwrap_generic(node.receiver_type))
	// mut receiver_type_name := util.no_dots(typ_sym.name)
	mut receiver_type_name := util.no_dots(g.cc_type2(g.unwrap_generic(node.receiver_type)))
	if typ_sym.kind == .interface_ {
		// Speaker_name_table[s._interface_idx].speak(s._object)
		$if debug_interface_method_call ? {
			eprintln('>>> interface typ_sym.name: $typ_sym.name | receiver_type_name: $receiver_type_name')
		}
		g.write('${c_name(receiver_type_name)}_name_table[')
		g.expr(node.left)
		dot := if node.left_type.is_ptr() { '->' } else { '.' }
		g.write('${dot}_interface_idx].${node.name}(')
		g.expr(node.left)
		g.write('${dot}_object')
		if node.args.len > 0 {
			g.write(', ')
			// g.call_args(node.args, node.expected_arg_types) // , [])
			g.call_args(node)
		}
		g.write(')')
		return
	}
	left_sym := g.table.get_type_symbol(node.left_type)
	if left_sym.kind == .array {
		match node.name {
			'filter' {
				g.gen_array_filter(node)
				return
			}
			'sort' {
				g.gen_array_sort(node)
				return
			}
			'insert' {
				g.gen_array_insert(node)
				return
			}
			'map' {
				g.gen_array_map(node)
				return
			}
			'prepend' {
				g.gen_array_prepend(node)
				return
			}
			'contains' {
				g.gen_array_contains(node)
				return
			}
			'index' {
				g.gen_array_index(node)
				return
			}
			else {}
		}
	}
	if left_sym.kind == .sum_type && node.name == 'type_name' {
		g.write('tos3( /* $left_sym.name */ v_typeof_sumtype_${node.receiver_type}( (')
		g.expr(node.left)
		g.write(').typ ))')
		return
	}
	if node.name == 'str' {
		g.gen_str_for_type(node.receiver_type)
	}
	mut has_cast := false
	if left_sym.kind == .map && node.name == 'clone' {
		receiver_type_name = 'map'
	}
	// TODO performance, detect `array` method differently
	if left_sym.kind == .array && node.name in
		['repeat', 'sort_with_compare', 'free', 'push_many', 'trim', 'first', 'last', 'pop', 'clone', 'reverse', 'slice'] {
		// && rec_sym.name == 'array' {
		// && rec_sym.name == 'array' && receiver_name.starts_with('array') {
		// `array_byte_clone` => `array_clone`
		receiver_type_name = 'array'
		if node.name in ['last', 'first', 'pop'] {
			return_type_str := g.typ(node.return_type)
			has_cast = true
			g.write('(*($return_type_str*)')
		}
	}
	mut name := util.no_dots('${receiver_type_name}_$node.name')
	if left_sym.kind == .chan {
		if node.name in ['close', 'try_pop', 'try_push'] {
			name = 'sync__Channel_$node.name'
		}
	}
	// Check if expression is: arr[a..b].clone(), arr[a..].clone()
	// if so, then instead of calling array_clone(&array_slice(...))
	// call array_clone_static(array_slice(...))
	mut is_range_slice := false
	if node.receiver_type.is_ptr() && !node.left_type.is_ptr() {
		if node.left is ast.IndexExpr {
			idx := node.left.index
			if idx is ast.RangeExpr {
				// expr is arr[range].clone()
				// use array_clone_static instead of array_clone
				name = util.no_dots('${receiver_type_name}_${node.name}_static')
				is_range_slice = true
			}
		}
	}
	// TODO2
	// g.generate_tmp_autofree_arg_vars(node, name)
	//
	// if node.receiver_type != 0 {
	// g.write('/*${g.typ(node.receiver_type)}*/')
	// g.write('/*expr_type=${g.typ(node.left_type)} rec type=${g.typ(node.receiver_type)}*/')
	// }
	if !node.receiver_type.is_ptr() && node.left_type.is_ptr() && node.name == 'str' {
		g.write('ptr_str(')
	} else {
		g.write('${name}(')
	}
	if node.receiver_type.is_ptr() && (!node.left_type.is_ptr() || node.from_embed_type != 0) {
		// The receiver is a reference, but the caller provided a value
		// Add `&` automatically.
		// TODO same logic in call_args()
		if !is_range_slice {
			g.write('&')
		}
	} else if !node.receiver_type.is_ptr() && node.left_type.is_ptr() && node.name != 'str' {
		g.write('/*rec*/*')
	}
	if node.free_receiver && !g.inside_lambda && !g.is_builtin_mod {
		// The receiver expression needs to be freed, use the temp var.
		fn_name := node.name.replace('.', '_')
		arg_name := '_arg_expr_${fn_name}_0_$node.pos.pos'
		g.write('/*af receiver arg*/' + arg_name)
	} else {
		g.expr(node.left)
		if node.from_embed_type != 0 {
			embed_name := typ_sym.embed_name()
			if node.left_type.is_ptr() {
				g.write('->')
			} else {
				g.write('.')
			}
			g.write(embed_name)
		}
	}
	if has_cast {
		g.write(')')
	}
	is_variadic := node.expected_arg_types.len > 0 && node.expected_arg_types[node.expected_arg_types.len -
		1].has_flag(.variadic)
	if node.args.len > 0 || is_variadic {
		g.write(', ')
	}
	// /////////
	/*
	if name.contains('subkeys') {
	println('call_args $name $node.arg_types.len')
	for t in node.arg_types {
		sym := g.table.get_type_symbol(t)
		print('$sym.name ')
	}
	println('')
}
	*/
	// ///////
	// g.call_args(node.args, node.expected_arg_types) // , [])
	g.call_args(node)
	g.write(')')
}

fn (mut g Gen) fn_call(node ast.CallExpr) {
	// call struct field with fn type
	// TODO: test node.left instead
	// left & left_type will be `x` and `x type` in `x.fieldfn()`
	// will be `0` for `foo()`
	if node.left_type != 0 {
		g.expr(node.left)
		if node.left_type.is_ptr() {
			g.write('->')
		} else {
			g.write('.')
		}
	}
	mut name := node.name
	is_print := name in ['print', 'println', 'eprint', 'eprintln']
	print_method := name
	is_json_encode := name == 'json.encode'
	is_json_decode := name == 'json.decode'
	g.is_json_fn = is_json_encode || is_json_decode
	mut json_type_str := ''
	mut json_obj := ''
	if g.is_json_fn {
		json_obj = g.new_tmp_var()
		mut tmp2 := ''
		cur_line := g.go_before_stmt(0)
		if is_json_encode {
			g.gen_json_for_type(node.args[0].typ)
			json_type_str = g.typ(node.args[0].typ)
			// `json__encode` => `json__encode_User`
			// encode_name := c_name(name) + '_' + util.no_dots(json_type_str)
			encode_name := js_enc_name(json_type_str)
			g.writeln('// json.encode')
			g.write('cJSON* $json_obj = ${encode_name}(')
			// g.call_args(node.args, node.expected_arg_types) // , [])
			if node.args[0].typ.is_ptr() {
				g.write('*')
			}
			g.call_args(node)
			g.writeln(');')
			tmp2 = g.new_tmp_var()
			g.writeln('string $tmp2 = json__json_print($json_obj);')
		} else {
			ast_type := node.args[0].expr as ast.Type
			// `json.decode(User, s)` => json.decode_User(s)
			typ := c_name(g.typ(ast_type.typ))
			fn_name := c_name(name) + '_' + typ
			g.gen_json_for_type(ast_type.typ)
			g.writeln('// json.decode')
			g.write('cJSON* $json_obj = json__json_parse(')
			// Skip the first argument in json.decode which is a type
			// its name was already used to generate the function call
			// g.call_args(node.args[1..], node.expected_arg_types) // , [])
			g.is_js_call = true
			g.call_args(node)
			g.is_js_call = false
			g.writeln(');')
			tmp2 = g.new_tmp_var()
			g.writeln('Option_$typ $tmp2 = $fn_name ($json_obj);')
		}
		if !g.pref.autofree {
			g.write('cJSON_Delete($json_obj); //del')
		}
		g.write('\n$cur_line')
		name = ''
		json_obj = tmp2
	}
	if node.language == .c {
		// Skip "C."
		g.is_c_call = true
		name = util.no_dots(name[2..])
	} else {
		name = c_name(name)
	}
	if node.generic_type != table.void_type && node.generic_type != 0 {
		// Using _T_ to differentiate between get<string> and get_string
		// `foo<int>()` => `foo_T_int()`
		name += '_T_' + g.typ(node.generic_type)
	}
	// TODO2
	// cgen shouldn't modify ast nodes, this should be moved
	// g.generate_tmp_autofree_arg_vars(node, name)
	// Handle `print(x)`
	mut print_auto_str := false
	if is_print && node.args[0].typ != table.string_type { // && !free_tmp_arg_vars {
		mut typ := node.args[0].typ
		if typ == 0 {
			g.checker_bug('print arg.typ is 0', node.pos)
		}
		mut sym := g.table.get_type_symbol(typ)
		if mut sym.info is table.Alias {
			typ = sym.info.parent_type
			sym = g.table.get_type_symbol(typ)
		}
		// check if alias parent also not a string
		if typ != table.string_type {
			expr := node.args[0].expr
			if g.autofree && !typ.has_flag(.optional) {
				// Create a temporary variable so that the value can be freed
				tmp := g.new_tmp_var()
				// tmps << tmp
				g.write('string $tmp = ')
				g.gen_expr_to_string(expr, typ)
				g.writeln('; ${print_method}($tmp); string_free(&$tmp);')
			} else {
				g.write('${print_method}(')
				g.gen_expr_to_string(expr, typ)
				g.write(')')
			}
			print_auto_str = true
		}
	}
	if !print_auto_str {
		if g.pref.is_debug && node.name == 'panic' {
			paline, pafile, pamod, pafn := g.panic_debug_info(node.pos)
			g.write('panic_debug($paline, tos3("$pafile"), tos3("$pamod"), tos3("$pafn"),  ')
			// g.call_args(node.args, node.expected_arg_types) // , [])
			g.call_args(node)
			g.write(')')
		} else {
			// Simple function call
			// if free_tmp_arg_vars {
			// g.writeln(';')
			// g.write(cur_line + ' /* <== af cur line*/')
			// }
			g.write('${g.get_ternary_name(name)}(')
			if g.is_json_fn {
				g.write(json_obj)
			} else {
				// g.call_args(node.args, node.expected_arg_types) // , tmp_arg_vars_to_free)
				g.call_args(node)
			}
			g.write(')')
		}
	}
	g.is_c_call = false
	g.is_json_fn = false
}

fn (mut g Gen) autofree_call_pregen(node ast.CallExpr) {
	// g.writeln('// autofree_call_pregen()')
	// Create a temporary var before fn call for each argument in order to free it (only if it's a complex expression,
	// like `foo(get_string())` or `foo(a + b)`
	mut free_tmp_arg_vars := g.autofree && !g.is_builtin_mod && node.args.len > 0 && !node.args[0].typ.has_flag(.optional) // TODO copy pasta checker.v
	if !free_tmp_arg_vars {
		return
	}
	if g.is_js_call {
		return
	}
	if g.inside_const {
		return
	}
	free_tmp_arg_vars = false // set the flag to true only if we have at least one arg to free
	g.tmp_count2++
	mut scope := g.file.scope.innermost(node.pos.pos)
	// prepend the receiver for now (TODO turn the receiver into a CallArg everywhere?)
	mut args := [ast.CallArg{
		typ: node.receiver_type
		expr: node.left
		is_tmp_autofree: node.free_receiver
	}]
	args << node.args
	// for i, arg in node.args {
	for i, arg in args {
		if !arg.is_tmp_autofree {
			continue
		}
		if arg.expr is ast.CallExpr {
			// Any argument can be an expression that has to be freed. Generate a tmp expression
			// for each of those recursively.
			g.autofree_call_pregen(arg.expr)
		}
		free_tmp_arg_vars = true
		// t := g.new_tmp_var() + '_arg_expr_${name}_$i'
		fn_name := node.name.replace('.', '_') // can't use name...
		// t := '_tt${g.tmp_count2}_arg_expr_${fn_name}_$i'
		t := '_arg_expr_${fn_name}_${i}_$node.pos.pos'
		// g.called_fn_name = name
		used := false // scope.known_var(t)
		mut s := '$t = '
		if used {
			// This means this tmp var name was already used (the same function was called and
			// `_arg_fnname_1` was already generated).
			// We do not need to declare this variable again, so just generate `t = ...`
			// instead of `string t = ...`, and we need to mark this variable as unused,
			// so that it's freed after the call. (Used tmp arg vars are not freed to avoid double frees).
			if x := scope.find(t) {
				match mut x {
					ast.Var { x.is_used = false }
					else {}
				}
			}
			s = '$t = '
		} else {
			scope.register(ast.Var{
				name: t
				typ: table.string_type
				is_autofree_tmp: true
				pos: node.pos
			})
			s = 'string $t = '
		}
		// g.expr(arg.expr)
		s += g.write_expr_to_string(arg.expr)
		// g.writeln(';// new af pre')
		s += ';// new af2 pre'
		g.strs_to_free0 << s
		// This tmp arg var will be freed with the rest of the vars at the end of the scope.
	}
}

fn (mut g Gen) autofree_call_postgen(node_pos int) {
	/*
	if g.strs_to_free.len == 0 {
		return
	}
	*/
	/*
	g.writeln('\n/* strs_to_free3: $g.nr_vars_to_free */')
	if g.nr_vars_to_free <= 0 {
		return
	}
	*/
	/*
	for s in g.strs_to_free {
		g.writeln('string_free(&$s);')
	}
	if !g.inside_or_block {
		// we need to free the vars both inside the or block (in case of an error) and after it
		// if we reset the array here, then the vars will not be freed after the block.
		g.strs_to_free = []
	}
	*/
	if g.inside_vweb_tmpl {
		return
	}
	// g.doing_autofree_tmp = true
	// g.write('/* postgen */')
	scope := g.file.scope.innermost(node_pos)
	for _, obj in scope.objects {
		match mut obj {
			ast.Var {
				// if var.typ == 0 {
				// // TODO why 0?
				// continue
				// }
				is_optional := obj.typ.has_flag(.optional)
				if is_optional {
					// TODO: free optionals
					continue
				}
				if !obj.is_autofree_tmp {
					continue
				}
				if obj.is_used {
					// this means this tmp expr var has already been freed
					continue
				}
				obj.is_used = true
				g.autofree_variable(obj)
				// g.nr_vars_to_free--
			}
			else {}
		}
	}
	// g.write('/* postgen end */')
	// g.doing_autofree_tmp = false
}

fn (mut g Gen) call_args(node ast.CallExpr) {
	args := if g.is_js_call { node.args[1..] } else { node.args }
	expected_types := node.expected_arg_types
	is_variadic := expected_types.len > 0 && expected_types[expected_types.len - 1].has_flag(.variadic)
	for i, arg in args {
		if is_variadic && i == expected_types.len - 1 {
			break
		}
		use_tmp_var_autofree := g.autofree && arg.typ == table.string_type && arg.is_tmp_autofree &&
			!g.inside_const && !g.is_builtin_mod
		// g.write('/* af=$arg.is_tmp_autofree */')
		mut is_interface := false
		// some c fn definitions dont have args (cfns.v) or are not updated in checker
		// when these are fixed we wont need this check
		if i < expected_types.len {
			if expected_types[i] != 0 {
				// Cast a type to interface
				// `foo(dog)` => `foo(I_Dog_to_Animal(dog))`
				exp_sym := g.table.get_type_symbol(expected_types[i])
				// exp_styp := g.typ(expected_types[arg_no]) // g.table.get_type_symbol(expected_types[arg_no])
				// styp := g.typ(arg.typ) // g.table.get_type_symbol(arg.typ)
				if exp_sym.kind == .interface_ {
					g.interface_call(arg.typ, expected_types[i])
					is_interface = true
				}
			}
			if is_interface {
				g.expr(arg.expr)
			} else if use_tmp_var_autofree {
				if arg.is_tmp_autofree { // && !g.is_js_call {
					// We saved expressions in temp variables so that they can be freed later.
					// `foo(str + str2) => x := str + str2; foo(x); x.free()`
					// g.write('_arg_expr_${g.called_fn_name}_$i')
					// Use these variables here.
					fn_name := node.name.replace('.', '_')
					// name := '_tt${g.tmp_count2}_arg_expr_${fn_name}_$i'
					name := '_arg_expr_${fn_name}_${i + 1}_$node.pos.pos'
					g.write('/*af arg*/' + name)
				}
			} else {
				g.ref_or_deref_arg(arg, expected_types[i])
			}
		} else {
			if use_tmp_var_autofree {
				// TODO copypasta, move to an inline fn
				fn_name := node.name.replace('.', '_')
				// name := '_tt${g.tmp_count2}_arg_expr_${fn_name}_$i'
				name := '_arg_expr_${fn_name}_${i + 1}_$node.pos.pos'
				g.write('/*af arg2*/' + name)
			} else {
				g.expr(arg.expr)
			}
		}
		if is_interface {
			g.write(')')
		}
		if i < args.len - 1 || is_variadic {
			g.write(', ')
		}
	}
	arg_nr := expected_types.len - 1
	if is_variadic {
		varg_type := expected_types[expected_types.len - 1]
		variadic_count := args.len - arg_nr
		arr_sym := g.table.get_type_symbol(varg_type)
		arr_info := arr_sym.info as table.Array
		elem_type := g.typ(arr_info.elem_type)
		if args.len > 0 && args[args.len - 1].expr is ast.ArrayDecompose {
			g.expr(args[args.len - 1].expr)
		} else {
			if variadic_count > 0 {
				g.write('new_array_from_c_array($variadic_count, $variadic_count, sizeof($elem_type), _MOV(($elem_type[$variadic_count]){')
				for j in arg_nr .. args.len {
					g.ref_or_deref_arg(args[j], arr_info.elem_type)
					if j < args.len - 1 {
						g.write(', ')
					}
				}
				g.write('}))')
			} else {
				g.write('__new_array_with_default(0, 0, sizeof($elem_type), 0)')
			}
		}
	}
}

[inline]
fn (mut g Gen) ref_or_deref_arg(arg ast.CallArg, expected_type table.Type) {
	arg_is_ptr := expected_type.is_ptr() || expected_type.idx() in table.pointer_type_idxs
	expr_is_ptr := arg.typ.is_ptr() || arg.typ.idx() in table.pointer_type_idxs
	if expected_type == 0 {
		g.checker_bug('ref_or_deref_arg expected_type is 0', arg.pos)
	}
	exp_sym := g.table.get_type_symbol(expected_type)
	if arg.is_mut && !arg_is_ptr {
		g.write('&/*mut*/')
	} else if arg_is_ptr && !expr_is_ptr {
		if arg.is_mut {
			if exp_sym.kind == .array {
				if arg.expr is ast.Ident && (arg.expr as ast.Ident).kind == .variable {
					g.write('&/*arr*/')
					g.expr(arg.expr)
				} else {
					// Special case for mutable arrays. We can't `&` function
					// results,	have to use `(array[]){ expr }[0]` hack.
					g.write('&/*111*/(array[]){')
					g.expr(arg.expr)
					g.write('}[0]')
				}
				return
			}
		}
		if !g.is_json_fn {
			if arg.typ == 0 {
				g.checker_bug('ref_or_deref_arg arg.typ is 0', arg.pos)
			}
			arg_typ_sym := g.table.get_type_symbol(arg.typ)
			expected_deref_type := if expected_type.is_ptr() {
				expected_type.deref()
			} else {
				expected_type
			}
			is_sum_type := g.table.get_type_symbol(expected_deref_type).kind == .sum_type
			if !((arg_typ_sym.kind == .function) || is_sum_type) {
				g.write('(voidptr)&/*qq*/')
			}
		}
	}
	g.expr_with_cast(arg.expr, arg.typ, expected_type)
}

fn (mut g Gen) is_gui_app() bool {
	$if windows {
		if g.force_main_console {
			return false
		}
		for cf in g.table.cflags {
			if cf.value == 'gdi32' {
				return true
			}
		}
	}
	return false
}

fn (g &Gen) fileis(s string) bool {
	return g.file.path.contains(s)
}

fn (mut g Gen) write_fn_attrs(attrs []table.Attr) string {
	mut msvc_attrs := ''
	for attr in attrs {
		match attr.name {
			'inline' {
				g.write('inline ')
			}
			'no_inline' {
				// since these are supported by GCC, clang and MSVC, we can consider them officially supported.
				g.write('__NOINLINE ')
			}
			'irq_handler' {
				g.write('__IRQHANDLER ')
			}
			'_cold' {
				// GCC/clang attributes
				// prefixed by _ to indicate they're for advanced users only and not really supported by V.
				// source for descriptions: https://gcc.gnu.org/onlinedocs/gcc/Common-Function-Attributes.html#Common-Function-Attributes
				// The cold attribute on functions is used to inform the compiler that the function is unlikely
				// to be executed. The function is optimized for size rather than speed and on many targets it
				// is placed into a special subsection of the text section so all cold functions appear close
				// together, improving code locality of non-cold parts of program.
				g.write('__attribute__((cold)) ')
			}
			'_constructor' {
				// The constructor attribute causes the function to be called automatically before execution
				// enters main ().
				g.write('__attribute__((constructor)) ')
			}
			'_destructor' {
				// The destructor attribute causes the function to be called automatically after main ()
				// completes or exit () is called.
				g.write('__attribute__((destructor)) ')
			}
			'_flatten' {
				// Generally, inlining into a function is limited. For a function marked with this attribute,
				// every call inside this function is inlined, if possible.
				g.write('__attribute__((flatten)) ')
			}
			'_hot' {
				// The hot attribute on a function is used to inform the compiler that the function is a hot
				// spot of the compiled program.
				g.write('__attribute__((hot)) ')
			}
			'_malloc' {
				// This tells the compiler that a function is malloc-like, i.e., that the pointer P returned by
				// the function cannot alias any other pointer valid when the function returns, and moreover no
				// pointers to valid objects occur in any storage addressed by P.
				g.write('__attribute__((malloc)) ')
			}
			'_pure' {
				// Calls to functions whose return value is not affected by changes to the observable state
				// of the program and that have no observable effects on such state other than to return a
				// value may lend themselves to optimizations such as common subexpression elimination.
				// Declaring such functions with the const attribute allows GCC to avoid emitting some calls in
				// repeated invocations of the function with the same argument values.
				g.write('__attribute__((const)) ')
			}
			'windows_stdcall' {
				// windows attributes (msvc/mingw)
				// prefixed by windows to indicate they're for advanced users only and not really supported by V.
				msvc_attrs += '__stdcall '
			}
			'console' {
				g.force_main_console = true
			}
			else {
				// nothing but keep V happy
			}
		}
	}
	return msvc_attrs
}
