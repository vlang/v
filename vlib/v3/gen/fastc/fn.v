module fastc

import v3.pref
import v3.token

fn (mut g Parser) run() !string {
	g.next()
	g.parse_top_level_items(false)!
	return g.out.str()
}

fn (mut g Parser) parse_top_level_items(stop_at_block_end bool) ! {
	for g.tok != .eof {
		g.skip_semicolons()
		if stop_at_block_end && g.tok == .rcbr {
			g.next()
			g.skip_semicolons()
			return
		}
		if g.tok == .eof {
			break
		}
		if g.selfhost && g.tok == .rcbr {
			g.next()
			continue
		}
		mut item_enabled := true
		for g.tok == .attribute {
			item_enabled = g.skip_attribute()! && item_enabled
			g.skip_semicolons()
		}
		if g.tok == .key_module {
			g.parse_module()!
			continue
		}
		if g.tok == .key_import {
			g.skip_import()!
			continue
		}
		if g.tok == .hash {
			g.parse_c_directive()!
			continue
		}
		if g.selfhost && g.tok == .dollar {
			g.parse_top_level_comptime_if()!
			continue
		}
		if g.tok == .key_pub || g.tok == .key_static {
			g.next()
		}
		if g.tok in [.key_struct, .key_union, .key_enum, .key_interface, .key_type, .key_const,
			.key_global] {
			g.skip_top_level_declaration()!
			continue
		}
		if g.tok == .key_fn {
			g.parse_function(item_enabled)!
			continue
		}
		if g.has_main {
			return g.unsupported('top-level `${g.token_source()}` after `main`')
		}
		if g.selfhost {
			return g.unsupported('unexpected top-level `${g.token_source()}` token `${g.tok.str()}`')
		}
		g.parse_script()!
		break
	}
	if stop_at_block_end {
		return g.unsupported('unfinished top-level compile-time block')
	}
}

fn (mut g Parser) parse_c_directive() ! {
	directive := g.lit.trim_space()
	g.next()
	if directive == 'flag' || directive.starts_with('flag ') || directive == 'pkgconfig'
		|| directive.starts_with('pkgconfig ') {
		// FastC's compiler invocation supplies its own target flags. The bootstrap
		// dependency set only uses these directives for optional libraries.
		if g.prefs.selfhost {
			return
		}
		return g.unsupported('C build directive `#${directive}`')
	}
	mut c_directive := fastc_resolve_c_pseudo_paths(directive, g.prefs.vroot, g.path)
	if c_directive.starts_with('insert ') {
		c_directive = 'include ' + c_directive['insert '.len..]
	}
	if c_directive.starts_with('include ') {
		remainder := c_directive['include '.len..]
		qualifier := remainder.all_before(' ')
		if qualifier in ['windows', 'macos', 'linux', 'freebsd', 'openbsd', 'netbsd', 'dragonfly',
			'solaris', 'android'] {
			if !pref.comptime_flag_value(g.prefs, qualifier) {
				return
			}
			c_directive = 'include ' + remainder.all_after(' ')
		}
	}
	if c_directive == '' {
		return g.unsupported('empty C directive')
	}
	g.out.writeln('#${c_directive}')
}

fn (mut g Parser) skip_attribute() !bool {
	if g.tok != .attribute {
		return true
	}
	g.next()
	mut depth := 1
	mut has_condition := false
	mut condition_value := true
	mut at_item_start := true
	for depth > 0 {
		if g.tok == .eof {
			return g.unsupported('unfinished attribute')
		}
		if depth == 1 && at_item_start && g.tok == .key_if {
			if has_condition {
				return g.unsupported('multiple conditional attributes')
			}
			has_condition = true
			g.next()
			condition_value = g.parse_comptime_or()!
			if g.tok !in [.semicolon, .rsbr] {
				return g.unsupported('conditional attribute expression near `${g.token_source()}`')
			}
			continue
		}
		if depth == 1 && g.tok == .semicolon {
			at_item_start = true
		} else if depth == 1 {
			at_item_start = false
		}
		if g.tok == .lsbr {
			depth++
		} else if g.tok == .rsbr {
			depth--
		}
		g.next()
	}
	return if has_condition { condition_value } else { true }
}

fn (mut g Parser) skip_top_level_declaration() ! {
	body_declaration := g.tok in [.key_struct, .key_union, .key_enum, .key_interface]
	type_declaration := g.tok == .key_type
	mut brace_depth := 0
	mut paren_depth := 0
	mut bracket_depth := 0
	for g.tok != .eof {
		if g.tok == .lcbr {
			brace_depth++
		} else if g.tok == .rcbr {
			if brace_depth == 0 {
				return
			}
			brace_depth--
			g.next()
			if body_declaration && brace_depth == 0 {
				g.skip_semicolons()
				return
			}
			continue
		} else if g.tok == .lpar {
			paren_depth++
		} else if g.tok == .rpar && paren_depth > 0 {
			paren_depth--
		} else if g.tok == .lsbr {
			bracket_depth++
		} else if g.tok == .rsbr && bracket_depth > 0 {
			bracket_depth--
		} else if g.tok == .semicolon && brace_depth == 0 && paren_depth == 0 && bracket_depth == 0 {
			g.next()
			if type_declaration && g.tok == .pipe {
				continue
			}
			return
		}
		g.next()
	}
}

fn (mut g Parser) next() {
	g.tok = g.s.scan()
	g.lit = g.s.lit
}

fn (mut g Parser) temporary_name(kind string) string {
	name := '__v_fastc_${kind}_${g.temp_id}'
	g.temp_id++
	return name
}

fn (g &Parser) temporary_namespace(kind string) string {
	mut index := 0
	for {
		namespace := '__v_fastc_${kind}' + if index == 0 { '' } else { '_${index}' }
		prefix := namespace + '_'
		mut collision := false
		for local_name in g.locals.keys() {
			if fastc_c_identifier(local_name).starts_with(prefix) {
				collision = true
				break
			}
		}
		if !collision {
			// Every candidate prefix starts with `__v_fastc_`, so only the
			// program's few `__v_fastc_`-prefixed function and global C names
			// can collide (see fastc_reserved_temporary_c_names); rescanning
			// every function key here made each temporary O(program).
			for c_name in g.fastc_prefixed_c_names {
				if c_name.starts_with(prefix) {
					collision = true
					break
				}
			}
		}
		if !collision {
			return namespace
		}
		index++
	}
	return ''
}

// fastc_reserved_temporary_c_names collects the generated function and global
// C names that begin with the `__v_fastc_` temporary namespace prefix. Only
// these can collide with a temporary_namespace candidate.
fn fastc_reserved_temporary_c_names(functions map[string]FastcFunctionSignature, globals map[string]string) []string {
	mut names := []string{}
	for function_key in functions.keys() {
		function_c_name := fastc_c_function_name_for_key(function_key)
		if function_c_name.starts_with('__v_fastc_') {
			names << function_c_name
		}
	}
	for c_name in globals.values() {
		if c_name.starts_with('__v_fastc_') {
			names << c_name
		}
	}
	return names
}

fn (mut g Parser) skip_semicolons() {
	for g.tok == .semicolon {
		g.next()
	}
}

fn (g &Parser) unsupported(feature string) IError {
	return error('fastc parser does not support ${feature} at byte ${g.s.pos} in ${g.path}')
}

fn (mut g Parser) expect(expected token.Token) ! {
	if g.tok != expected {
		return g.unsupported('`${expected.str()}` after `${g.token_source()}`')
	}
	g.next()
}

fn (mut g Parser) parse_module() ! {
	g.next()
	if g.tok != .name {
		return g.unsupported('module declaration')
	}
	if g.lit != g.module_name.all_after_last('.') {
		return g.unsupported('module `${g.lit}` in `${g.module_name}` source')
	}
	g.next()
	g.skip_semicolons()
}

fn (mut g Parser) skip_import() ! {
	g.next()
	if g.tok == .lpar {
		mut depth := 1
		g.next()
		for depth > 0 {
			if g.tok == .eof {
				return g.unsupported('unfinished import group')
			}
			if g.tok == .lpar {
				depth++
			} else if g.tok == .rpar {
				depth--
			}
			g.next()
		}
		g.skip_semicolons()
		return
	}
	mut selective_depth := 0
	for g.tok != .eof {
		if selective_depth == 0 && g.tok == .semicolon {
			g.next()
			return
		}
		if g.tok == .lcbr {
			selective_depth++
		} else if g.tok == .rcbr {
			if selective_depth == 0 {
				return
			}
			selective_depth--
		}
		g.next()
	}
}

fn (mut g Parser) parse_function(enabled bool) ! {
	g.locals = map[string]FastcLocal{}
	g.next()
	mut receiver_type := ''
	mut receiver_key := ''
	mut receiver_name := ''
	mut receiver_is_mut := false
	mut params := []string{}
	if g.tok == .lpar {
		g.next()
		if g.tok == .key_mut {
			receiver_is_mut = true
			g.next()
		}
		if g.tok != .name {
			return g.unsupported('method receiver')
		}
		receiver_name = g.lit
		g.next()
		if g.tok == .name && g.lit != 'C' {
			receiver_key = fastc_type_key(g.module_name, g.lit)
		} else if g.tok == .key_none {
			receiver_key = 'none'
		}
		receiver_type = g.parse_type()!
		if receiver_key == '' {
			receiver_key = g.semantic_type_key(receiver_type)
		}
		g.expect(.rpar)!
		receiver_is_reference := receiver_is_mut && !receiver_type.ends_with('*')
		receiver_parameter_type := if receiver_is_reference {
			receiver_type + '*'
		} else {
			receiver_type
		}
		params << '${receiver_parameter_type} ${fastc_c_identifier(receiver_name)}'
		g.locals[receiver_name] = FastcLocal{
			is_mut:       receiver_is_mut
			is_reference: receiver_is_reference
			typ:          receiver_parameter_type
		}
	}
	if g.tok != .name && !(receiver_type != '' && (g.tok.is_overloadable() || g.tok.is_keyword())) {
		return g.unsupported('function declaration')
	}
	mut name := if g.tok == .name || g.tok.is_keyword() { g.lit } else { g.tok.str() }
	g.next()
	mut is_c_function := false
	mut is_static_method := false
	if receiver_type == '' && name == 'C' && g.tok == .dot {
		is_c_function = true
		g.next()
		if g.tok != .name && !g.tok.is_keyword() {
			return g.unsupported('C function declaration')
		}
		name = g.lit
		g.next()
	} else if receiver_type == '' && g.tok == .dot {
		type_key := fastc_type_key(g.module_name, name)
		if type_key !in g.declared_types {
			return g.unsupported('static method owner `${name}`')
		}
		receiver_type = fastc_c_declared_type_name(type_key)
		receiver_key = type_key
		is_static_method = true
		g.next()
		if g.tok != .name {
			return g.unsupported('static method declaration')
		}
		name = g.lit
		g.next()
	}
	if g.tok == .lsbr {
		g.skip_balanced(.lsbr, .rsbr)!
	}
	if is_c_function {
		g.skip_c_function_declaration()!
		return
	}
	g.expect(.lpar)!
	params << g.parse_parameters()!
	mut return_type := 'void'
	mut return_types := []string{}
	mut option_return_type := ''
	if g.tok != .lcbr && g.tok != .semicolon {
		if g.tok in [.not, .question] {
			g.next()
			return_type = 'Option'
			if g.tok in [.lcbr, .semicolon] {
				option_return_type = 'void'
			} else if g.tok == .lpar {
				return_types = g.parse_multi_return_types()!
				option_return_type = 'MultiReturn'
			} else {
				option_return_type = g.parse_type()!
			}
		} else if g.tok == .lpar {
			return_types = g.parse_multi_return_types()!
			return_type = 'MultiReturn'
		} else {
			return_type = g.parse_type()!
		}
	}
	function_key := if receiver_type == '' {
		fastc_function_key(g.module_name, name)
	} else {
		'${receiver_key}.${name}'
	}
	is_main := !is_static_method && receiver_type == '' && g.module_name in ['', 'main']
		&& name == 'main'
	is_module_init := !is_static_method && receiver_type == '' && name == 'init'
	is_module_cleanup := !is_static_method && receiver_type == '' && name == 'cleanup'
	if is_main {
		if params.len > 0 {
			return g.unsupported('main function with parameters')
		}
	}
	if is_module_init && params.len > 0 {
		return g.unsupported('module `init` with parameters')
	}
	if is_module_cleanup && params.len > 0 {
		return g.unsupported('module `cleanup` with parameters')
	}
	if g.tok == .semicolon {
		g.next()
		return
	}
	is_fastc_source := name.starts_with('fastc_') || g.path.ends_with('/fastc/fastc.v')
		|| g.module_name.ends_with('fastc')
	if g.selfhost && name != 'fastc_collect_referenced_function_names' && !is_fastc_source
		&& name !in ['main', 'init', 'cleanup'] && name !in g.used_function_names && name.len > 0
		&& (name[0].is_letter() || name[0] == `_`) {
		g.skip_balanced(.lcbr, .rcbr)!
		return
	}
	if signature := g.functions[function_key] {
		if signature.path != g.path && name != 'fastc_collect_referenced_function_names'
			&& !is_fastc_source {
			g.skip_balanced(.lcbr, .rcbr)!
			return
		}
	}
	if g.selfhost && g.open_block_contains_select_statement() {
		// The self-host reachability prepass deliberately groups overloaded methods
		// by name. Omit an unsupported overload that only became reachable through
		// that conservative grouping. A real reference remains undefined and makes
		// C validation fail instead of emitting select with changed semantics.
		g.skip_balanced(.lcbr, .rcbr)!
		return
	}
	c_name := if receiver_type == '' {
		fastc_c_function_name(g.module_name, name)
	} else {
		fastc_method_c_name(g.module_name, fastc_c_declared_type_name(receiver_key), name)
	}
	c_return_type := if is_main { 'int' } else { return_type }
	c_params := if is_main && g.selfhost {
		'int argc, char **argv'
	} else if params.len == 0 {
		'void'
	} else {
		params.join(', ')
	}
	g.protos.writeln('${c_return_type} ${c_name}(${c_params});')
	if !enabled {
		g.write_line('${c_return_type} ${c_name}(${c_params}) {')
		g.indent++
		if return_type != 'void' {
			g.write_line('return (${return_type}){0};')
		}
		g.indent--
		g.write_line('}')
		g.out.writeln('')
		g.skip_balanced(.lcbr, .rcbr)!
		return
	}
	g.expect(.lcbr)!
	if is_main {
		g.has_main = true
	}
	g.write_line('${c_return_type} ${c_name}(${c_params}) {')
	g.indent++
	if is_main {
		g.write_line('setvbuf(stdout, NULL, _IONBF, 0);')
		if g.selfhost {
			g.write_line('g_main_argc = argc;')
			g.write_line('g_main_argv = argv;')
		}
		if g.has_startup_inits {
			g.write_line('v_fastc_init_globals();')
		}
		if g.has_cleanup_hooks {
			g.write_line('atexit(v_fastc_cleanup_modules);')
		}
	}
	previous_in_main := g.in_main
	previous_return_type := g.return_type
	previous_return_types := g.return_types.clone()
	previous_option_return_type := g.option_return_type
	previous_function := g.current_function
	previous_receiver := g.current_receiver
	previous_method_is_static := g.current_method_is_static
	previous_deferred_lines := g.deferred_lines.clone()
	previous_deferred_block_starts := g.deferred_block_starts.clone()
	previous_loop_defer_block_starts := g.loop_defer_block_starts.clone()
	previous_loop_has_breaks := g.loop_has_breaks.clone()
	previous_statement_reachable := g.statement_reachable
	g.in_main = is_main
	g.return_type = return_type
	g.return_types = return_types.clone()
	g.option_return_type = option_return_type
	g.current_function = name
	g.current_receiver = receiver_key
	g.current_method_is_static = is_static_method
	g.deferred_lines.clear()
	g.deferred_block_starts.clear()
	g.loop_defer_block_starts.clear()
	g.loop_has_breaks.clear()
	g.statement_reachable = true
	terminates := g.parse_block_body()!
	g.in_main = previous_in_main
	g.return_type = previous_return_type
	g.return_types = previous_return_types.clone()
	g.option_return_type = previous_option_return_type
	g.current_function = previous_function
	g.current_receiver = previous_receiver
	g.current_method_is_static = previous_method_is_static
	g.deferred_lines = previous_deferred_lines.clone()
	g.deferred_block_starts = previous_deferred_block_starts.clone()
	g.loop_defer_block_starts = previous_loop_defer_block_starts.clone()
	g.loop_has_breaks = previous_loop_has_breaks.clone()
	g.statement_reachable = previous_statement_reachable
	if return_type != 'void' && !terminates {
		if !g.selfhost {
			return g.unsupported('non-void function `${name}` that can fall through')
		}
		// Self-host input was already accepted by the bootstrap compiler. Keep C's
		// control-flow rules satisfied when the streaming parser cannot prove that
		// every nested source branch terminates.
		g.write_line('return (${return_type}){0};')
	}
	if is_main {
		g.write_line('return 0;')
	}
	g.indent--
	g.write_line('}')
	g.out.writeln('')
}

fn fastc_method_c_name(module_name string, receiver_type string, name string) string {
	module_prefix := if module_name in ['', 'main'] {
		''
	} else {
		module_name.replace('.', '__') + '__'
	}
	receiver := receiver_type.trim_right('*').all_after_last('__')
	method := match name {
		'+' { 'plus' }
		'-' { 'minus' }
		'*' { 'mul' }
		'/' { 'div' }
		'==' { 'eq' }
		'!=' { 'ne' }
		'<' { 'lt' }
		'<=' { 'le' }
		'>' { 'gt' }
		'>=' { 'ge' }
		else { name }
	}
	return '${module_prefix}${receiver}_${method}'
}

fn (mut g Parser) skip_balanced(open token.Token, close token.Token) ! {
	if g.tok != open {
		return g.unsupported('`${open.str()}` group')
	}
	mut depth := 0
	for {
		if g.tok == open {
			depth++
		} else if g.tok == close {
			depth--
			g.next()
			if depth == 0 {
				return
			}
			continue
		} else if g.tok == .eof {
			return g.unsupported('unfinished `${open.str()}` group')
		}
		g.next()
	}
}

fn (mut g Parser) skip_c_function_declaration() ! {
	mut parens := 0
	for g.tok != .eof {
		if g.tok == .lpar {
			parens++
		} else if g.tok == .rpar {
			parens--
		} else if g.tok == .semicolon && parens == 0 {
			g.next()
			return
		} else if g.tok == .lcbr && parens == 0 {
			g.skip_balanced(.lcbr, .rcbr)!
			return
		}
		g.next()
	}
}

fn (mut g Parser) parse_script() ! {
	g.locals = map[string]FastcLocal{}
	g.has_main = true
	g.protos.writeln('int main(void);')
	g.write_line('int main(void) {')
	g.indent++
	g.write_line('setvbuf(stdout, NULL, _IONBF, 0);')
	if g.has_startup_inits {
		g.write_line('v_fastc_init_globals();')
	}
	if g.has_cleanup_hooks {
		g.write_line('atexit(v_fastc_cleanup_modules);')
	}
	g.in_main = true
	g.skip_semicolons()
	for g.tok != .eof {
		if g.tok in [.key_module, .key_pub, .key_static, .key_fn] {
			return g.unsupported('declaration after top-level statements')
		}
		_ = g.parse_statement()!
		g.skip_semicolons()
	}
	g.write_line('return 0;')
	g.indent--
	g.write_line('}')
	g.out.writeln('')
}

fn (mut g Parser) parse_parameters() ![]string {
	mut params := []string{}
	g.skip_semicolons()
	for g.tok != .rpar {
		mut is_mut := false
		if g.tok in [.key_mut, .key_shared] {
			is_mut = true
			g.next()
		}
		if g.tok != .name {
			return g.unsupported('function parameters')
		}
		name := g.lit
		g.next()
		mut names := [name]
		for g.tok == .comma {
			g.next()
			if g.tok != .name && !g.tok.is_keyword() {
				return g.unsupported('grouped parameter names')
			}
			names << g.lit
			g.next()
		}
		mut type_name := g.parse_type()!
		is_reference := is_mut && !type_name.ends_with('*')
		if is_reference {
			type_name += '*'
		}
		for parameter_name in names {
			params << '${type_name} ${fastc_c_identifier(parameter_name)}'
			g.locals[parameter_name] = FastcLocal{
				is_mut:       is_mut
				is_reference: is_reference
				typ:          type_name
			}
		}
		if g.tok == .comma {
			g.next()
			g.skip_semicolons()
			continue
		}
		if g.tok != .rpar {
			return g.unsupported('function parameter separator')
		}
	}
	g.next()
	return params
}

fn (mut g Parser) parse_type() !string {
	first_lit := g.lit
	type_name, next_token := fastc_scan_type(mut g.s, g.tok, g.path, g.module_name, g.imports,
		g.declared_types, g.selfhost) or { return g.unsupported(err.msg()) }
	g.tok = next_token
	g.lit = g.s.lit
	if !g.selfhost && (first_lit in ['charptr', 'rune'] || type_name == 'char*') {
		return g.unsupported('type `${first_lit}`')
	}
	return type_name
}

fn (mut g Parser) parse_multi_return_types() ![]string {
	g.expect(.lpar)!
	mut types := []string{}
	for g.tok != .rpar {
		g.skip_semicolons()
		if g.tok == .rpar {
			break
		}
		types << g.parse_type()!
		if g.tok == .comma {
			g.next()
		} else if g.tok != .rpar {
			return g.unsupported('multi-return type separator')
		}
	}
	g.expect(.rpar)!
	return types
}

fn fastc_primitive_c_type(raw_type string) ?string {
	return match raw_type {
		'bool' { 'bool' }
		'byte' { 'byte' }
		'char' { 'char' }
		'f32' { 'f32' }
		'f64' { 'f64' }
		'float_literal' { 'f64' }
		'i8' { 'i8' }
		'i16' { 'i16' }
		'i32' { 'i32' }
		'i64' { 'i64' }
		'int' { 'int' }
		'int_literal' { 'i64' }
		'isize' { 'isize' }
		'rune' { 'rune' }
		'string' { 'string' }
		'u8' { 'u8' }
		'u16' { 'u16' }
		'u32' { 'u32' }
		'u64' { 'u64' }
		'uint' { 'unsigned int' }
		'usize' { 'usize' }
		'voidptr' { 'voidptr' }
		'byteptr' { 'byteptr' }
		'charptr' { 'charptr' }
		'chan' { 'chan' }
		'array' { 'array' }
		'map' { 'map' }
		'Option' { 'Option' }
		'any' { 'voidptr' }
		else { none }
	}
}

fn fastc_expression_tokens_contain(tokens []FastcExpressionToken, wanted token.Token) bool {
	for item in tokens {
		if item.tok == wanted {
			return true
		}
	}
	return false
}

fn fastc_expression_tokens_contain_boolean_operator(tokens []FastcExpressionToken) bool {
	for item in tokens {
		if item.tok in [.eq, .ne, .gt, .lt, .ge, .le, .and, .logical_or, .not] {
			return true
		}
	}
	return false
}

fn fastc_expression_tokens_contain_assignment_or_mutation(tokens []FastcExpressionToken) bool {
	for item in tokens {
		if item.tok.is_assignment() || item.tok in [.inc, .dec] {
			return true
		}
	}
	return false
}

fn fastc_expression_tokens_contain_statement_method(tokens []FastcExpressionToken) bool {
	for item in tokens {
		if item.tok == .name && item.lit in ['set', 'clear'] {
			return true
		}
	}
	return false
}

fn fastc_expression_tokens_debug(tokens []FastcExpressionToken) string {
	mut details := []string{cap: tokens.len}
	for item in tokens {
		details << '${item.tok.str()}:${item.lit}'
	}
	return details.join(',')
}

fn fastc_all_true(values []bool) bool {
	for value in values {
		if !value {
			return false
		}
	}
	return true
}
