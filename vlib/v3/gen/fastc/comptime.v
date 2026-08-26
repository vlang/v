module fastc

import os
import v3.pref
import v3.scanner
import v3.token

fn fastc_scan_comptime_unary(mut scan scanner.Scanner, first token.Token, path string, prefs &pref.Preferences) !FastcComptimeCondition {
	if first == .not {
		result := fastc_scan_comptime_unary(mut scan, scan.scan(), path, prefs)!
		return FastcComptimeCondition{
			value: !result.value
			tok:   result.tok
		}
	}
	if first == .lpar {
		result := fastc_scan_comptime_or(mut scan, scan.scan(), path, prefs)!
		if result.tok != .rpar {
			return error('fastc parser does not support compile-time condition in ${path}')
		}
		return FastcComptimeCondition{
			value: result.value
			tok:   scan.scan()
		}
	}
	if first == .key_true {
		return FastcComptimeCondition{
			value: true
			tok:   scan.scan()
		}
	}
	if first == .key_false {
		return FastcComptimeCondition{
			value: false
			tok:   scan.scan()
		}
	}
	if first != .name {
		return error('fastc parser does not support compile-time condition `${first.str()}` in ${path}')
	}
	name := scan.lit
	mut tok := scan.scan()
	is_optional := tok == .question
	if is_optional {
		tok = scan.scan()
	}
	value := if is_optional {
		pref.comptime_optional_flag_value(prefs, name)
	} else {
		pref.comptime_flag_value(prefs, name)
	}
	return FastcComptimeCondition{
		value: value
		tok:   tok
	}
}

fn fastc_scan_comptime_and(mut scan scanner.Scanner, first token.Token, path string, prefs &pref.Preferences) !FastcComptimeCondition {
	first_result := fastc_scan_comptime_unary(mut scan, first, path, prefs)!
	mut value := first_result.value
	mut tok := first_result.tok
	for tok == .and {
		right := fastc_scan_comptime_unary(mut scan, scan.scan(), path, prefs)!
		value = value && right.value
		tok = right.tok
	}
	return FastcComptimeCondition{
		value: value
		tok:   tok
	}
}

fn fastc_scan_comptime_or(mut scan scanner.Scanner, first token.Token, path string, prefs &pref.Preferences) !FastcComptimeCondition {
	first_result := fastc_scan_comptime_and(mut scan, first, path, prefs)!
	mut value := first_result.value
	mut tok := first_result.tok
	for tok == .logical_or {
		right := fastc_scan_comptime_and(mut scan, scan.scan(), path, prefs)!
		value = value || right.value
		tok = right.tok
	}
	return FastcComptimeCondition{
		value: value
		tok:   tok
	}
}

fn fastc_scan_comptime_block(mut scan scanner.Scanner, first token.Token, path string) !FastcComptimeBlock {
	if first != .lcbr {
		return error('fastc parser does not support compile-time branch without a block in ${path}')
	}
	start := scan.offset
	mut depth := 1
	mut tok := scan.scan()
	for {
		if tok == .eof {
			return error('fastc parser does not support unfinished compile-time block in ${path}')
		}
		if tok == .lcbr {
			depth++
		} else if tok == .rcbr {
			depth--
			if depth == 0 {
				return FastcComptimeBlock{
					source: scan.src[start..scan.pos]
					tok:    scan.scan()
				}
			}
		}
		tok = scan.scan()
	}
	return FastcComptimeBlock{}
}

fn fastc_scan_skip_semicolons(mut scan scanner.Scanner, first token.Token) token.Token {
	mut tok := first
	for tok == .semicolon {
		tok = scan.scan()
	}
	return tok
}

fn fastc_scan_selected_comptime_branch(mut scan scanner.Scanner, first token.Token, path string, prefs &pref.Preferences) !FastcComptimeBlock {
	mut tok := first
	mut selected := ''
	mut branch_selected := false
	for {
		if tok != .key_if {
			return error('fastc parser does not support compile-time branch `${tok.str()}` in ${path}')
		}
		condition := fastc_scan_comptime_or(mut scan, scan.scan(), path, prefs)!
		block := fastc_scan_comptime_block(mut scan, condition.tok, path)!
		if condition.value && !branch_selected {
			selected = block.source
			branch_selected = true
		}
		tok = fastc_scan_skip_semicolons(mut scan, block.tok)
		if tok != .dollar {
			return FastcComptimeBlock{
				source: selected
				tok:    tok
			}
		}
		mut lookahead := scan
		if lookahead.scan() != .key_else {
			return FastcComptimeBlock{
				source: selected
				tok:    tok
			}
		}
		_ = scan.scan()
		tok = scan.scan()
		if tok == .dollar {
			tok = scan.scan()
			continue
		}
		else_block := fastc_scan_comptime_block(mut scan, tok, path)!
		if !branch_selected {
			selected = else_block.source
		}
		return FastcComptimeBlock{
			source: selected
			tok:    fastc_scan_skip_semicolons(mut scan, else_block.tok)
		}
	}
	return FastcComptimeBlock{}
}

fn fastc_collect_selected_comptime_function_signatures(source string, path string, header FastcSourceHeader, prefs &pref.Preferences, declared_types map[string]bool, params_structs map[string]bool, mut functions map[string]FastcFunctionSignature) ! {
	mut file_set := token.FileSet.new()
	mut file := file_set.add_file(path, source.len)
	file.index_lines_without_digest(source)
	mut scan := scanner.new_scanner(prefs, .normal)
	scan.init(file, source)
	mut brace_depth := 0
	mut tok := scan.scan()
	for tok != .eof {
		if brace_depth == 0 && tok == .dollar {
			mut lookahead := scan
			if lookahead.scan() == .key_if {
				selected := fastc_scan_selected_comptime_branch(mut scan, scan.scan(), path, prefs)!
				if selected.source != '' {
					collect_function_signatures(selected.source, path, header, prefs,
						declared_types, params_structs, mut functions)!
				}
				tok = selected.tok
				continue
			}
		}
		if tok == .lcbr {
			brace_depth++
		} else if tok == .rcbr && brace_depth > 0 {
			brace_depth--
		}
		tok = scan.scan()
	}
}

fn (mut g Parser) parse_top_level_comptime_if() ! {
	g.expect(.dollar)!
	g.expect(.key_if)!
	condition := g.parse_comptime_or()!
	g.expect(.lcbr)!
	if condition {
		g.parse_top_level_items(true)!
	} else {
		g.skip_open_block()!
	}
	if g.tok != .dollar || !g.dollar_keyword_is('else') {
		return
	}
	g.next()
	g.expect(.key_else)!
	if g.tok == .dollar {
		if condition {
			g.skip_comptime_if_chain()!
		} else {
			g.parse_top_level_comptime_if()!
		}
		return
	}
	g.expect(.lcbr)!
	if condition {
		g.skip_open_block()!
	} else {
		g.parse_top_level_items(true)!
	}
}

fn (mut g Parser) parse_comptime_if_statement() !bool {
	g.expect(.dollar)!
	g.expect(.key_if)!
	condition := g.parse_comptime_or()!
	g.expect(.lcbr)!
	mut terminates := false
	if condition {
		terminates = g.parse_block_body()!
	} else {
		g.skip_open_block()!
	}
	if g.tok != .dollar || !g.dollar_keyword_is('else') {
		return terminates
	}
	g.next()
	if g.tok != .key_else {
		return g.unsupported('compile-time branch after `$if`')
	}
	g.next()
	if g.tok == .dollar {
		if condition {
			g.skip_comptime_if_chain()!
			return terminates
		}
		return g.parse_comptime_if_statement()!
	}
	g.expect(.lcbr)!
	if condition {
		g.skip_open_block()!
		return terminates
	}
	return g.parse_block_body()!
}

fn (mut g Parser) parse_comptime_or() !bool {
	mut value := g.parse_comptime_and()!
	for g.tok == .logical_or {
		g.next()
		right := g.parse_comptime_and()!
		value = value || right
	}
	return value
}

fn (mut g Parser) parse_comptime_and() !bool {
	mut value := g.parse_comptime_unary()!
	for g.tok == .and {
		g.next()
		right := g.parse_comptime_unary()!
		value = value && right
	}
	return value
}

fn (mut g Parser) parse_comptime_unary() !bool {
	if g.tok == .not {
		g.next()
		return !g.parse_comptime_unary()!
	}
	if g.tok == .lpar {
		g.next()
		value := g.parse_comptime_or()!
		g.expect(.rpar)!
		return value
	}
	if g.tok == .key_true {
		g.next()
		return true
	}
	if g.tok == .key_false {
		g.next()
		return false
	}
	if g.tok != .name {
		return g.unsupported('compile-time condition `${g.token_source()}`')
	}
	name := g.lit
	g.next()
	if g.tok in [.key_is, .not_is] {
		operator := g.tok
		g.next()
		mut target_is_group := false
		mut target_type := ''
		if g.tok == .dollar {
			target_is_group = true
			g.next()
			if g.tok != .name && !g.tok.is_keyword() {
				return g.unsupported('compile-time type group `${g.token_source()}`')
			}
			target_type = g.lit
			g.next()
		} else {
			target_type = g.parse_type()!
		}
		left_type := if local := g.locals[name] {
			local.typ.trim_right('*')
		} else if primitive := fastc_primitive_c_type(name) {
			primitive
		} else if name.len <= 3 && name[0].is_capital() {
			'voidptr'
		} else {
			fastc_c_declared_type_name(fastc_type_key(g.module_name, name))
		}
		matches := g.comptime_type_matches(left_type, target_type, target_is_group)
		return if operator == .key_is { matches } else { !matches }
	}
	is_optional := g.tok == .question
	if is_optional {
		g.next()
	}
	return if is_optional {
		pref.comptime_optional_flag_value(g.prefs, name)
	} else {
		pref.comptime_flag_value(g.prefs, name)
	}
}

fn (g &Parser) comptime_type_matches(left_type string, target_type string, target_is_group bool) bool {
	left := g.underlying_alias_type(left_type)
	if !target_is_group {
		return left == g.underlying_alias_type(target_type)
	}
	left_key := g.semantic_type_key(left)
	original_key := g.semantic_type_key(left_type)
	return match target_type {
		'array' {
			left.starts_with('Array_') || left == 'array'
		}
		'array_fixed' {
			left.starts_with('FixedArray_')
		}
		'map' {
			left.starts_with('Map_') || left == 'map'
		}
		'string' {
			left == 'string'
		}
		'int' {
			left in ['i8', 'i16', 'i32', 'i64', 'int', 'isize', 'u8', 'u16', 'u32', 'u64', 'uint',
				'usize']
		}
		'float' {
			left in ['f32', 'f64']
		}
		'struct' {
			left_key in g.declared_kinds && g.declared_kinds[left_key] == .struct_
		}
		'enum' {
			left_key in g.declared_kinds && g.declared_kinds[left_key] == .enum_
		}
		'interface' {
			left_key in g.declared_kinds && g.declared_kinds[left_key] == .interface_
		}
		'alias' {
			original_key in g.declared_kinds && g.declared_kinds[original_key] == .alias_
		}
		else {
			false
		}
	}
}

fn (mut g Parser) skip_open_block() ! {
	mut depth := 1
	for depth > 0 {
		if g.tok == .eof {
			return g.unsupported('unfinished compile-time block')
		}
		if g.tok == .lcbr {
			depth++
		} else if g.tok == .rcbr {
			depth--
		}
		g.next()
	}
	g.skip_semicolons()
}

fn (mut g Parser) skip_comptime_if_chain() ! {
	if g.tok != .dollar {
		return g.unsupported('compile-time `$else` branch')
	}
	g.next()
	g.expect(.key_if)!
	_ = g.parse_comptime_or()!
	g.expect(.lcbr)!
	g.skip_open_block()!
	if g.tok == .dollar && g.dollar_keyword_is('else') {
		g.next()
		g.expect(.key_else)!
		if g.tok == .dollar {
			g.skip_comptime_if_chain()!
		} else {
			g.expect(.lcbr)!
			g.skip_open_block()!
		}
	}
}

fn (g &Parser) dollar_keyword_is(keyword string) bool {
	mut offset := g.s.offset
	for offset < g.s.src.len && g.s.src[offset] in [` `, `\t`] {
		offset++
	}
	return offset + keyword.len <= g.s.src.len && g.s.src[offset..offset + keyword.len] == keyword
}

fn (g &Parser) comptime_pseudo_expression(name string) ?string {
	line, column := fastc_line_column(g.s.src, g.s.pos)
	module_name := if g.module_name == '' { 'main' } else { g.module_name }
	function_name := g.current_function
	receiver_name := g.current_receiver.all_after_last('.')
	method_name := if receiver_name != '' {
		'${receiver_name}.${function_name}'
	} else {
		function_name
	}
	location_method := if receiver_name == '' {
		'${module_name}.${function_name}'
	} else if g.current_method_is_static {
		'${module_name}.${receiver_name}.${function_name} (static)'
	} else {
		'${module_name}.${receiver_name}{}.${function_name}'
	}
	value := match name {
		'@FN' {
			function_name
		}
		'@METHOD' {
			method_name
		}
		'@STRUCT' {
			receiver_name
		}
		'@MOD' {
			module_name
		}
		'@FILE' {
			g.path
		}
		'@DIR' {
			os.dir(g.path)
		}
		'@LINE' {
			line.str()
		}
		'@COLUMN' {
			column.str()
		}
		'@FILE_LINE' {
			'${os.file_name(g.path)}:${line}'
		}
		'@LOCATION' {
			'${g.path}:${line}, ${location_method}'
		}
		'@VEXEROOT', '@VROOT' {
			g.prefs.vroot
		}
		'@VMODROOT' {
			fastc_vmod_root_for_file(g.path)
		}
		'@VEXE' {
			g.prefs.vexe
		}
		'@VMOD_FILE' {
			vmod_file := os.join_path_single(fastc_vmod_root_for_file(g.path), 'v.mod')
			content := os.read_file(vmod_file) or { return none }
			content.replace('\r\n', '\n')
		}
		'@VHASH' {
			g.prefs.vhash
		}
		'@VCURRENTHASH' {
			g.prefs.vcurrent_hash
		}
		'@BUILD_DATE' {
			g.prefs.build_date
		}
		'@BUILD_TIME' {
			g.prefs.build_time
		}
		'@BUILD_TIMESTAMP' {
			g.prefs.build_timestamp
		}
		'@OS' {
			g.prefs.normalized_target_os()
		}
		'@CCOMPILER' {
			g.prefs.ccompiler
		}
		'@BACKEND' {
			g.prefs.backend
		}
		'@PLATFORM' {
			g.prefs.comptime_platform()
		}
		else {
			return none
		}
	}
	return '_S(${fastc_c_string_value(value)})'
}

fn (mut g Parser) read_comptime_if_expression() !string {
	g.expect(.dollar)!
	g.expect(.key_if)!
	condition := g.parse_comptime_or()!
	g.expect(.lcbr)!
	if condition {
		value := g.read_expression([token.Token.rcbr])!
		value_type := g.last_expression_type
		g.skip_semicolons()
		g.expect(.rcbr)!
		if g.tok == .dollar {
			g.next()
			g.expect(.key_else)!
			if g.tok == .dollar {
				g.skip_comptime_if_chain()!
			} else {
				g.expect(.lcbr)!
				g.skip_open_block()!
			}
		}
		g.last_expression_type = value_type
		g.last_expression = []FastcExpressionToken{}
		return value
	}
	g.skip_open_block()!
	if g.tok != .dollar {
		return g.unsupported('compile-time if expression without `$else`')
	}
	g.next()
	g.expect(.key_else)!
	if g.tok == .dollar {
		return g.read_comptime_if_expression()!
	}
	g.expect(.lcbr)!
	value := g.read_expression([token.Token.rcbr])!
	value_type := g.last_expression_type
	g.skip_semicolons()
	g.expect(.rcbr)!
	g.last_expression_type = value_type
	g.last_expression = []FastcExpressionToken{}
	return value
}
