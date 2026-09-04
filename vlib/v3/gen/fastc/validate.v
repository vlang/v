module fastc

import v3.token

fn (g &Parser) expression_token(previous token.Token, previous_lit string, qualified_name_owner string, module_separator bool) !string {
	return match g.tok {
		.name {
			g.expression_name(previous, qualified_name_owner)!
		}
		.number {
			if g.selfhost {
				fastc_c_selfhost_number(g.lit)
			} else {
				fastc_c_number(g.lit)!
			}
		}
		.string {
			literal := fastc_c_string(g.lit) or {
				return g.unsupported('string literal `${g.lit}`: ${err.msg()}')
			}
			if g.selfhost {
				'_S(${literal})'
			} else {
				literal
			}
		}
		.char {
			if g.selfhost && g.lit.starts_with('c:') {
				fastc_c_string("'" + g.lit['c:'.len..] + "'") or {
					return g.unsupported('C string literal `${g.lit}`: ${err.msg()}')
				}
			} else if g.selfhost {
				fastc_c_rune(g.lit) or {
					return g.unsupported('rune literal `${g.lit}`: ${err.msg()}')
				}
			} else {
				g.unsupported('rune or C character literals')
			}
		}

		// stdbool's true/false macros have C type int. Cast them so _Generic
		// dispatch preserves V's bool type when no operator requires promotion.
		.key_true {
			'((bool)true)'
		}
		.key_false {
			'((bool)false)'
		}
		.key_nil {
			g.nil_expression()!
		}
		.key_none {
			if g.selfhost {
				'(Option){.state=2}'
			} else {
				g.unsupported('none expressions')
			}
		}
		.key_likely, .key_unlikely {
			''
		}
		.semicolon {
			';'
		}
		.dot {
			g.dot_piece(previous, previous_lit, module_separator)
		}
		.right_shift_unsigned {
			// V's logical right shift `>>>` has no C spelling; it is used on unsigned operands
			// (V code casts first, e.g. `u64(x) >>> n`), where a plain C `>>` is logical.
			'>>'
		}
		else {
			g.tok.str()
		}
	}
}

fn (g &Parser) nil_expression() !string {
	if g.unsafe_depth == 0 && !g.in_mono_drain {
		return g.unsupported('`nil` outside an `unsafe` block')
	}
	return 'NULL'
}

fn (g &Parser) expression_name(previous token.Token, qualified_name_owner string) !string {
	if g.selfhost {
		return g.resolved_expression_name(g.lit, previous)
	}
	if previous == .dot {
		if imported_module := g.imports[qualified_name_owner] {
			type_key := fastc_type_key(imported_module, g.lit)
			if type_key in g.declared_types && !g.declared_types[type_key] {
				return g.unsupported('private type `${g.lit}` from imported module `${imported_module}`')
			}
			constant_key := fastc_constant_key(imported_module, g.lit)
			if constant_key in g.constants && constant_key !in g.public_constants {
				return g.unsupported('private constant `${g.lit}` from imported module `${imported_module}`')
			}
			g.validate_imported_global_visibility(imported_module, g.lit)!
		}
	}
	g.validate_expression_name(g.lit, previous)!
	return g.resolved_expression_name(g.lit, previous)
}

fn (g &Parser) validate_imported_global_visibility(imported_module string, name string) ! {
	global_key := fastc_global_key(imported_module, name)
	if global_key in g.globals && global_key !in g.public_globals {
		return g.unsupported('private global `${name}` from imported module `${imported_module}`')
	}
}

// local_c_name is the C spelling of a local: its smart-cast override name when one is active
// (see FastcLocal.c_name), otherwise the sanitized identifier.
fn (g &Parser) local_c_name(name string) string {
	if local := g.locals[name] {
		if local.c_name != '' {
			return local.c_name
		}
	}
	return fastc_c_identifier(name)
}

fn (g &Parser) resolved_expression_name(name string, previous token.Token) string {
	if previous != .dot && name == 'C' {
		return ''
	}
	if previous != .dot {
		if local := g.locals[name] {
			if local.c_name != '' {
				return local.c_name
			}
		}
	}
	if previous != .dot && name !in g.locals {
		return g.resolved_nonlocal_expression_name_cached(name)
	}
	return fastc_c_identifier(name)
}

fn (g &Parser) resolved_nonlocal_expression_name_cached(name string) string {
	if cached := g.resolved_name_memo[name] {
		return cached
	}
	resolved := g.resolved_nonlocal_expression_name(name)
	mut w := unsafe { &Parser(g) }
	w.resolved_name_memo[name] = resolved
	return resolved
}

// resolved_nonlocal_expression_name renders a bare name that is not a local:
// an import, function, type, primitive, constant, or global, else the plain
// C identifier. Every table it consults is fixed for the file, so
// resolved_expression_name memoizes the answer per name.
fn (g &Parser) resolved_nonlocal_expression_name(name string) string {
	if imported_module := g.imports[name] {
		return imported_module.replace('.', '__')
	}
	function_key := g.unqualified_function_key(name)
	if function_key in g.functions {
		return g.c_function_name_for_key(function_key)
	}
	if type_key := g.resolve_declared_type_key(name) {
		return fastc_c_declared_type_name(type_key)
	}
	if primitive := fastc_primitive_c_type(name) {
		return primitive
	}
	constant_key := fastc_constant_key(g.module_name, name)
	if c_name := g.constants[constant_key] {
		return c_name
	}
	if c_name := g.constants[fastc_constant_key('builtin', name)] {
		return c_name
	}
	global_key := fastc_global_key(g.module_name, name)
	if c_name := g.globals[global_key] {
		return c_name
	}
	if c_name := g.globals[fastc_global_key('builtin', name)] {
		return c_name
	}
	// A `__global` is truly global, including from a different module than its declaration.
	suffix := '.${name}'
	for key, c_name in g.globals {
		if key == name || key.ends_with(suffix) {
			return c_name
		}
	}
	return fastc_c_identifier(name)
}

fn (g &Parser) validate_expression_name(name string, previous token.Token) ! {
	// Self-host sources have already passed the bootstrap compiler. Their unresolved
	// names are deliberately left for C to diagnose, so none of the lookup work below
	// can change the result.
	if g.selfhost {
		return
	}
	if name == 'charptr' {
		return g.unsupported('charptr expressions')
	}
	if name == 'rune' {
		return g.unsupported('rune expressions')
	}
	function_key := g.unqualified_function_key(name)
	constant_key := fastc_constant_key(g.module_name, name)
	global_key := fastc_global_key(g.module_name, name)
	if previous == .dot || (name == 'C' && g.has_declared_c_function()) || name in g.locals || name in g.imports || function_key in g.functions || constant_key in g.constants || global_key in g.globals || g.resolve_declared_type_key(name) != none || name in [
		'print',
		'println',
		'bool',
		'byte',
		'char',
		'f32',
		'f64',
		'i8',
		'i16',
		'i32',
		'i64',
		'int',
		'isize',
		'rune',
		'string',
		'u8',
		'u16',
		'u32',
		'u64',
		'uint',
		'usize',
		'voidptr',
		'byteptr',
		'charptr',
	] {
		return
	}
	return g.unsupported('unresolved name `${name}` (locals: ${g.locals.keys().join(', ')})')
}

fn (g &Parser) has_declared_c_function() bool {
	return g.has_c_functions
}

// fastc_functions_declare_c collects once whether any collected function key
// names a `C.` function; has_declared_c_function previously cloned every
// function key per name-resolution query to answer this fixed question.
fn fastc_functions_declare_c(functions map[string]FastcFunctionSignature) bool {
	for function_key, _ in functions {
		if function_key.starts_with('C.') {
			return true
		}
	}
	return false
}

fn (g &Parser) function_key_for_call(tokens []FastcExpressionToken, name_index int) string {
	if name_index >= 2 && tokens[name_index - 1].tok == .dot && tokens[name_index - 2].tok == .name {
		if static_key := g.static_function_key_for_call(tokens, name_index) {
			return static_key
		}
		if tokens[name_index - 2].lit == 'C' {
			return 'C.${tokens[name_index].lit}'
		}
		if imported_module := g.imports[tokens[name_index - 2].lit] {
			return fastc_function_key(g.resolve_module_alias(imported_module), tokens[name_index].lit)
		}
	}
	return g.unqualified_function_key(tokens[name_index].lit)
}

// resolve_module_alias maps an import path that re-exports another module (via an
// `@[alias]` module file, e.g. `json2` importable as `x.json2`) to the module name
// its functions were actually loaded/keyed under. Empty when no aliases exist (so
// the self-host, which imports no aliased modules, is unaffected).
fn (g &Parser) resolve_module_alias(module_name string) string {
	return g.module_aliases[module_name] or { module_name }
}

fn (g &Parser) static_function_key_for_call(tokens []FastcExpressionToken, name_index int) ?string {
	if name_index < 2 || tokens[name_index - 1].tok != .dot || tokens[name_index - 2].tok != .name {
		return none
	}
	owner_name := tokens[name_index - 2].lit
	if owner_name.len == 0 || !owner_name[0].is_capital() {
		return none
	}
	mut type_key := ''
	if name_index >= 4 && tokens[name_index - 3].tok == .dot && tokens[name_index - 4].tok == .name {
		module_name := g.imports[tokens[name_index - 4].lit] or { return none }
		type_key = fastc_type_key(module_name, owner_name)
	} else {
		type_key = g.resolve_declared_type_key(owner_name) or { return none }
	}
	function_key := '${type_key}.${tokens[name_index].lit}'
	return if function_key in g.functions { function_key } else { none }
}

fn (g &Parser) local_is_pointer(name string) bool {
	local := g.locals[name] or { return false }
	return local.typ.ends_with('*')
}

fn (g &Parser) is_enum_type_name(name string) bool {
	type_key := g.resolve_declared_type_key(name) or { return false }
	return g.underlying_enum_type_key(type_key) != none
}

fn (g &Parser) underlying_enum_type_key(type_key string) ?string {
	if type_key == '' {
		return none
	}
	resolved_type := g.underlying_alias_type(fastc_c_declared_type_name(type_key))
	resolved_key := g.semantic_type_key(resolved_type)
	return if g.declared_kinds[resolved_key] == .enum_ { resolved_key } else { none }
}

fn (g &Parser) flag_enum_type_key(c_type string) ?string {
	type_key := g.semantic_type_key(c_type)
	enum_key := g.underlying_enum_type_key(type_key) or { return none }
	return if g.enum_flags[enum_key] { enum_key } else { none }
}

fn (g &Parser) enum_member_owner_type_key(tokens []FastcExpressionToken, owner_index int) ?string {
	if owner_index < 0 || owner_index + 2 >= tokens.len || tokens[owner_index].tok != .name || tokens[owner_index + 1].tok != .dot || tokens[owner_index + 2].tok != .name {
		return none
	}
	mut type_key := ''
	if owner_index >= 2 && tokens[owner_index - 1].tok == .dot && tokens[owner_index - 2].tok == .name {
		imported_module := g.imports[tokens[owner_index - 2].lit] or { return none }
		type_key = fastc_type_key(imported_module, tokens[owner_index].lit)
	} else if owner_index > 0 && tokens[owner_index - 1].tok == .dot {
		return none
	} else {
		type_key = g.resolve_declared_type_key(tokens[owner_index].lit) or { return none }
	}
	_ = g.underlying_enum_type_key(type_key) or { return none }
	return type_key
}

fn (g &Parser) render_enum_alias_member_references(tokens []FastcExpressionToken, source string) string {
	if tokens.len < 3 {
		return source
	}
	mut rendered := source
	for owner_index in 0 .. tokens.len - 2 {
		type_key := g.enum_member_owner_type_key(tokens, owner_index) or { continue }
		enum_type_key := g.underlying_enum_type_key(type_key) or { continue }
		if enum_type_key == type_key {
			continue
		}
		member := tokens[owner_index + 2].lit
		alias_symbol := '${fastc_c_declared_type_name(type_key)}__${member}'
		enum_symbol := '${fastc_c_declared_type_name(enum_type_key)}__${member}'
		rendered = rendered.replace(alias_symbol, enum_symbol)
	}
	return rendered
}

fn (g &Parser) declared_cast_type_key(tokens []FastcExpressionToken, name_index int) ?string {
	if name_index >= 2 && tokens[name_index - 1].tok == .dot && tokens[name_index - 2].tok == .name {
		module_name := g.imports[tokens[name_index - 2].lit] or { return none }
		type_key := fastc_type_key(module_name, tokens[name_index].lit)
		return if type_key in g.declared_types { type_key } else { none }
	}
	if name_index > 0 && tokens[name_index - 1].tok == .dot {
		return none
	}
	return g.resolve_declared_type_key(tokens[name_index].lit)
}

fn (g &Parser) validate_expression_calls(tokens []FastcExpressionToken) ! {
	mut i := 0
	for i + 1 < tokens.len {
		if tokens[i].tok !in [.name, .key_select] || tokens[i + 1].tok != .lpar {
			i++
			continue
		}
		call_end := fastc_matching_rpar(tokens, i + 1) or {
			return g.unsupported('unbalanced function call `${tokens[i].lit}`')
		}
		call_args := fastc_call_arguments(tokens, i + 1, call_end) or {
			return g.unsupported('function call `${tokens[i].lit}` arguments')
		}
		for argument in call_args {
			g.validate_expression_calls(argument)!
		}
		name := tokens[i].lit
		mut function_key := g.function_key_for_call(tokens, i)
		is_static_call := g.static_function_key_for_call(tokens, i) != none
		mut is_method_call := false
		mut has_method_receiver := false
		mut receiver_type := ''
		// `mod.func()` is a module-qualified call, NOT a method — but only when `mod` is a
		// bare module reference. `recv.mod.method()` (a FIELD named like an imported module,
		// e.g. `c.ssl.set_read_timeout()` where `ssl` is both a field and `import net.ssl`)
		// is a real method call, so the import exclusion must not apply when the name is
		// itself preceded by a `.` (a member access).
		name_is_module_ref := i >= 2 && tokens[i - 2].tok == .name && (tokens[i - 2].lit in g.imports || tokens[i - 2].lit == 'C') && (i < 3 || tokens[i - 3].tok != .dot)
		if !is_static_call && i >= 2 && tokens[i - 1].tok == .dot && !name_is_module_ref {
			receiver_start := fastc_method_receiver_start(tokens, i - 1)
			receiver_type = g.infer_expression_type(tokens[receiver_start..i - 1])!
			if receiver_type != '' {
				has_method_receiver = true
				function_key, _ = g.resolve_method(receiver_type, name)
				is_method_call = function_key in g.functions
				if !is_method_call && function_key in g.mono_functions {
					// An on-demand monomorphized generic method (queued during rendering): its
					// per-Parser signature is a minimal stub, so accept the call and skip the
					// detailed arg validation (the concrete instance is emitted in the drain).
					i = call_end + 1
					continue
				}
			}
		}
		// A primitive type name applied to a single argument is a cast (`u32(1)`),
		// never a call — even when a same-named function exists (e.g. `rand.u32()`).
		if call_args.len == 1 && !is_method_call && !is_static_call && (i == 0 || tokens[i - 1].tok != .dot) && fastc_primitive_c_type(name) != none {
			i = call_end + 1
			continue
		}
		// A declared type applied to one argument is a cast even when a function-pointer
		// alias has a signature entry for calls through locals of that type.
		if !is_method_call && !is_static_call {
			if type_key := g.declared_cast_type_key(tokens, i) {
				if call_args.len != 1 {
					return g.unsupported('cast `${name}` with ${call_args.len} arguments')
				}
				if g.declared_kinds[type_key] == .interface_ {
					g.validate_interface_cast_shape(type_key, call_args[0])!
				}
				i = call_end + 1
				continue
			}
		}
		has_signature := function_key in g.functions || function_key in g.mono_functions
		if has_signature {
			signature := if function_key in g.functions {
				g.functions[function_key]
			} else {
				g.mono_functions[function_key]
			}
			if !signature.is_public && signature.module_name != '' && signature.module_name != g.module_name && signature.module_name != 'builtin' && signature.module_name in g.imports.values() {
				return g.unsupported('private function `${name}` from imported module `${signature.module_name}`')
			}
			argument_offset := if is_method_call { 1 } else { 0 }
			is_variadic := signature.is_variadic
			expected_arguments := signature.parameter_types.len - argument_offset - if is_variadic {
				1
			} else {
				0
			}
			omits_params_struct := signature.last_parameter_is_params && expected_arguments > 0 && call_args.len == expected_arguments - 1
			// Trailing named arguments (`name: value`) at the last params-struct
			// position collapse into one struct initializer, so they read as one arg.
			mut named_argument_start := -1
			for argument_index, argument in call_args {
				if argument.len >= 2 && argument[0].tok == .name && argument[1].tok == .colon {
					named_argument_start = argument_index
					break
				}
			}
			// Trailing named args collapse into the callee's last struct parameter — V allows
			// this for any struct last-parameter (`time.new(year: ...)`), not just `@[params]`.
			provides_params_struct := expected_arguments > 0 && named_argument_start == expected_arguments - 1 && (signature.last_parameter_is_params || g.fastc_type_is_declared_struct(signature.parameter_types[named_argument_start + argument_offset]))
			uses_default_array_sort := function_key == 'array.sort' && call_args.len == 0
			if (!is_variadic && call_args.len != expected_arguments && !omits_params_struct && !provides_params_struct && !uses_default_array_sort) || (is_variadic && call_args.len < expected_arguments) {
				return g.unsupported('function `${name}` call with ${call_args.len} arguments instead of ${expected_arguments}')
			}
			if is_method_call && signature.parameter_types.len > 0 {
				receiver_is_mut := signature.parameter_mutability.len > 0 && signature.parameter_mutability[0]
				if receiver_is_mut {
					receiver_start := fastc_method_receiver_start(tokens, i - 1)
					g.validate_mutating_method_receiver(tokens[receiver_start..i - 1], name)!
				}
			}
			for argument_index, argument in call_args {
				if is_variadic && argument_index >= expected_arguments && function_key.starts_with('C.') {
					continue
				}
				is_variadic_argument := is_variadic && argument_index >= expected_arguments
				parameter_index := if is_variadic_argument {
					signature.parameter_types.len - 1
				} else {
					argument_index + argument_offset
				}
				parameter_is_mut := parameter_index < signature.parameter_mutability.len && signature.parameter_mutability[parameter_index]
				argument_is_mut := fastc_argument_is_marked_mut(argument)
				if parameter_is_mut && !argument_is_mut {
					return g.unsupported('function `${name}` parameter ${argument_index + 1} requires a mutable argument written with `mut`')
				}
				if parameter_is_mut && !g.selfhost {
					argument_name := fastc_mut_argument_root_name(argument)
					global_key := fastc_global_key(g.module_name, argument_name)
					if local := g.locals[argument_name] {
						if !local.is_mut {
							return g.unsupported('mutable argument `${argument_name}` to function `${name}` is immutable')
						}
					} else if global_key !in g.globals {
						return g.unsupported('unverifiable mutable argument ${argument_index + 1} to function `${name}`')
					}
					g.validate_mutable_argument_fields(argument, name, argument_index)!
				}
			}
		} else if has_method_receiver && name in ['has', 'set', 'clear'] && call_args.len == 1 && g.flag_enum_type_key(receiver_type) != none {
			if name in ['set', 'clear'] {
				receiver_start := fastc_method_receiver_start(tokens, i - 1)
				g.validate_mutating_method_receiver(tokens[receiver_start..i - 1], name)!
			}
			i = call_end + 1
			continue
		} else if i == 0 && name in ['print', 'println'] {
			if call_args.len != 1 {
				return g.unsupported('function `${name}` call with ${call_args.len} arguments')
			}
			argument_type := g.infer_expression_type(call_args[0])!
			if !g.selfhost && !g.ordinary_print_type_is_supported(argument_type) {
				return g.unsupported('printing value of type `${argument_type}`')
			}
		} else {
			if g.selfhost && i >= 2 && tokens[i - 2].tok == .name && tokens[i - 2].lit == 'C' && tokens[i - 1].tok == .dot {
				i = call_end + 1
				continue
			}
			if g.selfhost && (i == 0 || tokens[i - 1].tok != .dot) && name in g.locals {
				// Calling a local/parameter that holds a function pointer (`cb(x, y)`), e.g.
				// vqsort's `sort_cb`: a plain identifier that is a local can only be called
				// through its function-pointer value, which renders as a direct C call.
				i = call_end + 1
				continue
			}
			if i == 0 || tokens[i - 1].tok != .dot {
				if fastc_primitive_c_type(name) != none {
					if call_args.len != 1 {
						return g.unsupported('cast `${name}` with ${call_args.len} arguments')
					}
					i = call_end + 1
					continue
				}
			}
			if g.selfhost && has_method_receiver && g.struct_member_type(receiver_type, name) != '' {
				i = call_end + 1
				continue
			}
			if name == 'wait' && receiver_type.starts_with(fastc_thread_type_prefix) {
				// `.wait()` on a spawned thread resolves through the generated
				// join helper, not the collected function signatures.
				if call_args.len != 0 {
					return g.unsupported('`.wait()` on a spawned thread with ${call_args.len} arguments')
				}
				i = call_end + 1
				continue
			}
			if g.selfhost && has_method_receiver && name == 'str' && call_args.len == 0 && g.can_generate_default_struct_str(receiver_type) {
				i = call_end + 1
				continue
			}
			if g.selfhost && has_method_receiver {
				return g.unsupported('unresolved method call `${g.semantic_type_key(receiver_type)}.${name}` of `${receiver_type}`')
			}
			if g.selfhost {
				return g.unsupported('unresolved function call `${name}` tokens `${fastc_expression_tokens_debug(tokens)}`')
			}
			return g.unsupported('unresolved function call `${name}`')
		}
		i = call_end + 1
	}
}

fn (g &Parser) ordinary_print_type_is_supported(typ string) bool {
	normalized_type := fastc_normalize_inferred_type(typ)
	underlying_type := g.underlying_alias_type(normalized_type)
	if underlying_type in ['string', 'bool'] || fastc_is_integer_type(underlying_type) {
		return true
	}
	return g.underlying_enum_type_key(g.semantic_type_key(normalized_type)) != none
}

fn fastc_argument_is_marked_mut(argument []FastcExpressionToken) bool {
	for item in argument {
		if item.is_mut_argument {
			return true
		}
	}
	return false
}

fn fastc_mut_argument_root_name(argument []FastcExpressionToken) string {
	for item in argument {
		if item.tok == .name {
			return item.lit
		}
	}
	return ''
}

fn (g &Parser) validate_mutating_method_receiver(receiver []FastcExpressionToken, method_name string) ! {
	// A pointer receiver (a `&T` field/local, e.g. a `&sync.Mutex` guarded by an
	// immutable `&Struct` param) already carries mutable access to its pointee, so a
	// `mut`-receiver method call through it is valid regardless of the root variable's
	// mutability.
	if receiver_type := g.infer_expression_type(receiver) {
		if receiver_type.ends_with('*') {
			return
		}
	}
	root_name := fastc_mut_argument_root_name(receiver)
	if root_name == '' || root_name == 'C' {
		return g.unsupported('unverifiable mutating method `${method_name}` receiver')
	}
	global_key := fastc_global_key(g.module_name, root_name)
	if local := g.locals[root_name] {
		if !local.is_mut && receiver[0].unsafe_depth == 0 {
			return g.unsupported('mutating method `${method_name}` receiver `${root_name}` is immutable')
		}
	} else if global_key !in g.globals {
		return g.unsupported('mutating method `${method_name}` receiver `${root_name}` is immutable or unknown')
	}
	mut selector_depth := 0
	for i, item in receiver {
		match item.tok {
			.lpar, .lsbr, .lcbr {
				selector_depth++
				continue
			}
			.rpar, .rsbr, .rcbr {
				selector_depth--
				continue
			}
			else {}
		}
		if selector_depth != 0 || item.tok != .dot || i == 0 || i + 1 >= receiver.len || receiver[i + 1].tok != .name || g.expression_dot_is_module_separator(receiver, i) {
			continue
		}
		receiver_start := fastc_method_receiver_start(receiver, i)
		receiver_type := g.infer_expression_type(receiver[receiver_start..i]) or { continue }
		field := g.struct_field_metadata(receiver_type, receiver[i + 1].lit) or { continue }
		if field.module_name !in ['', g.module_name] && !field.is_mutable && item.unsafe_depth == 0 {
			type_name := g.semantic_type_key(receiver_type).all_after_last('.')
			return g.unsupported('mutating method `${method_name}` receiver field `${type_name}.${field.name}` is not `pub mut` in imported module `${field.module_name}`')
		}
	}
}

fn (g &Parser) validate_mutable_argument_fields(argument []FastcExpressionToken, function_name string, argument_index int) ! {
	mut selector_depth := 0
	for i, item in argument {
		match item.tok {
			.lpar, .lsbr, .lcbr {
				selector_depth++
				continue
			}
			.rpar, .rsbr, .rcbr {
				selector_depth--
				continue
			}
			else {}
		}
		if selector_depth != 0 || item.tok != .dot || i == 0 || i + 1 >= argument.len || argument[i + 1].tok != .name || g.expression_dot_is_module_separator(argument, i) {
			continue
		}
		receiver_start := fastc_method_receiver_start(argument, i)
		receiver_type := g.infer_expression_type(argument[receiver_start..i]) or { continue }
		field := g.struct_field_metadata(receiver_type, argument[i + 1].lit) or { continue }
		if field.module_name !in ['', g.module_name] && !field.is_mutable {
			type_name := g.semantic_type_key(receiver_type).all_after_last('.')
			return g.unsupported('mutable argument field `${type_name}.${field.name}` to function `${function_name}` parameter ${argument_index + 1} is not `pub mut` in imported module `${field.module_name}`')
		}
	}
}

fn (g &Parser) validate_interface_cast_shape(interface_key string, operand []FastcExpressionToken) ! {
	actual_type := fastc_normalize_inferred_type(g.infer_expression_type(operand)!)
	if actual_type == '' {
		return
	}
	actual_key := g.semantic_type_key(actual_type)
	if actual_key == interface_key {
		return
	}
	interface_type := fastc_c_declared_type_name(interface_key)
	prefix := interface_key + '.'
	mut interface_method_keys := g.functions.keys()
	interface_method_keys.sort()
	for interface_method_key in interface_method_keys {
		if !interface_method_key.starts_with(prefix) {
			continue
		}
		interface_signature := g.functions[interface_method_key]
		if interface_signature.parameter_types.len == 0 || interface_signature.parameter_types[0] != interface_type {
			continue
		}
		method_name := interface_method_key.all_after_last('.')
		candidate_signature := g.functions['${actual_key}.${method_name}'] or {
			if g.selfhost {
				continue
			}
			return g.unsupported('type `${actual_type}` does not implement interface `${interface_type}` method `${method_name}`')
		}
		if candidate_signature.parameter_types.len != interface_signature.parameter_types.len {
			return g.unsupported('type `${actual_type}` has an incompatible parameter count for interface `${interface_type}` method `${method_name}`')
		}
		for i in 0 .. interface_signature.parameter_types.len {
			interface_parameter_is_mut := i < interface_signature.parameter_mutability.len && interface_signature.parameter_mutability[i]
			candidate_parameter_is_mut := i < candidate_signature.parameter_mutability.len && candidate_signature.parameter_mutability[i]
			if candidate_parameter_is_mut != interface_parameter_is_mut {
				return g.unsupported('type `${actual_type}` has incompatible mutability for interface `${interface_type}` method `${method_name}` parameter ${i + 1}')
			}
		}
	}
	if g.selfhost && actual_key !in g.declared_kinds {
		return
	}
	mut interface_field_keys := g.interface_fields.keys()
	interface_field_keys.sort()
	for field_key in interface_field_keys {
		if !field_key.starts_with(prefix) {
			continue
		}
		required_field := g.interface_fields[field_key]
		actual_field := g.interface_implementation_field(actual_type, actual_key, required_field.name) or {
			return g.unsupported('type `${actual_type}` does not implement interface `${interface_type}` field `${required_field.name}`')
		}
		if required_field.is_mutable && !actual_field.is_mutable {
			return g.unsupported('type `${actual_type}` does not implement interface `${interface_type}`: field `${required_field.name}` must be mutable')
		}
	}
}

fn (g &Parser) interface_implementation_field(actual_type string, actual_key string, field_name string) ?FastcInterfaceField {
	if g.declared_kinds[actual_key] == .interface_ {
		return g.interface_fields['${actual_key}.${field_name}'] or { return none }
	}
	field := g.struct_field_metadata(actual_type, field_name) or { return none }
	return FastcInterfaceField{
		name: field.name
		typ: field.typ
		is_mutable: field.is_mutable
	}
}

fn fastc_expression_is_zero(tokens []FastcExpressionToken) bool {
	return tokens.len == 1 && tokens[0].tok == .number && tokens[0].lit.replace('_', '').trim_left('0') == ''
}

fn fastc_expression_is_c_qualified_name(tokens []FastcExpressionToken) bool {
	return tokens.len == 3 && tokens[0].tok == .name && tokens[0].lit == 'C' && tokens[1].tok == .dot && tokens[2].tok == .name
}

fn fastc_expression_is_enum_shorthand(tokens []FastcExpressionToken) bool {
	return tokens.len == 2 && tokens[0].tok == .dot && tokens[1].tok == .name
}

fn fastc_top_level_mutation_index(tokens []FastcExpressionToken) ?int {
	mut depth := 0
	for i, item in tokens {
		if depth == 0 && (item.tok.is_assignment() || item.tok in [.inc, .dec]) {
			return i
		}
		match item.tok {
			.lpar, .lsbr, .lcbr { depth++ }
			.rpar, .rsbr, .rcbr { depth-- }
			else {}
		}
	}
	return none
}

fn (g &Parser) validate_expression_mutation_lvalue(tokens []FastcExpressionToken) ! {
	mutation_index := fastc_top_level_mutation_index(tokens) or { return }
	if mutation_index == 0 {
		return g.unsupported('mutation without a target')
	}
	lvalue := tokens[..mutation_index]
	if lvalue.len == 1 {
		return
	}
	if lvalue[0].tok != .name {
		return
	}
	root_name := lvalue[0].lit
	if root_name == 'C' {
		return
	}
	global_key := fastc_global_key(g.module_name, root_name)
	mut selfhost_pointer_root := false
	if local := g.locals[root_name] {
		selfhost_pointer_root = g.selfhost && fastc_is_pointer_type(local.typ)
		if !local.is_mut && lvalue[0].unsafe_depth == 0 && !selfhost_pointer_root {
			return g.unsupported('mutation of immutable or unknown name `${root_name}`')
		}
	} else if global_key !in g.globals {
		return g.unsupported('mutation of immutable or unknown name `${root_name}`')
	}
	mut selector_depth := 0
	for i, item in lvalue {
		match item.tok {
			.lpar, .lsbr, .lcbr {
				selector_depth++
				continue
			}
			.rpar, .rsbr, .rcbr {
				selector_depth--
				continue
			}
			else {}
		}
		if selector_depth != 0 || item.tok != .dot || i == 0 || i + 1 >= lvalue.len || lvalue[i + 1].tok != .name || g.expression_dot_is_module_separator(lvalue, i) {
			continue
		}
		receiver_start := fastc_method_receiver_start(lvalue, i)
		receiver_type := g.infer_expression_type(lvalue[receiver_start..i]) or { continue }
		field := g.struct_field_metadata(receiver_type, lvalue[i + 1].lit) or { continue }
		// A late generic specialization may be a reflection setter (for example
		// json2's `$for field in T.fields { value.$(field.name) = ... }`). V permits
		// that generated assignment even when the concrete field is not declared
		// `mut`; the specialization therefore needs the same privilege as its private
		// field access above.
		if !field.is_mutable && item.unsafe_depth == 0 && !selfhost_pointer_root && !g.in_mono_drain {
			type_name := g.semantic_type_key(receiver_type).all_after_last('.')
			return g.unsupported('mutation of immutable field `${type_name}.${field.name}`')
		}
	}
}

fn (g &Parser) struct_direct_member_type(receiver_type string, field_name string) string {
	mut layout_type := fastc_trim_pointer_suffix(receiver_type)
	if layout_type.starts_with('Array_') {
		layout_type = 'array'
	} else if layout_type.starts_with('Map_') {
		layout_type = 'map'
	}
	if fields := g.struct_fields[layout_type] {
		return fields[field_name] or { '' }
	}
	return ''
}

fn (g &Parser) struct_member_type(receiver_type string, field_name string) string {
	field := g.struct_field_metadata(receiver_type, field_name) or { return '' }
	return field.typ
}

fn (g &Parser) struct_field_metadata(receiver_type string, field_name string) ?FastcStructField {
	if g.last_field_known && receiver_type == g.last_field_receiver && field_name == g.last_field_name {
		if g.last_field.name == '' {
			return none
		}
		return g.last_field
	}
	mut w := unsafe { &Parser(g) }
	if by_name := g.field_memo[receiver_type] {
		if cached := by_name[field_name] {
			w.last_field_receiver = receiver_type
			w.last_field_name = field_name
			w.last_field = cached
			w.last_field_known = true
			if cached.name == '' {
				return none
			}
			return cached
		}
	}
	if receiver_type !in w.field_memo {
		w.field_memo[receiver_type] = map[string]FastcStructField{}
	}
	if field := g.struct_field_metadata_impl(receiver_type, field_name) {
		w.field_memo[receiver_type][field_name] = field
		w.last_field_receiver = receiver_type
		w.last_field_name = field_name
		w.last_field = field
		w.last_field_known = true
		return field
	}
	miss := FastcStructField{}
	w.field_memo[receiver_type][field_name] = miss
	w.last_field_receiver = receiver_type
	w.last_field_name = field_name
	w.last_field = miss
	w.last_field_known = true
	return none
}

fn (g &Parser) struct_field_metadata_impl(receiver_type string, field_name string) ?FastcStructField {
	mut layout_type := fastc_trim_pointer_suffix(receiver_type)
	if layout_type.starts_with('Array_') {
		layout_type = 'array'
	} else if layout_type.starts_with('Map_') {
		layout_type = 'map'
	}
	if fields := g.struct_field_lookup[layout_type] {
		if field := fields[field_name] {
			return field
		}
	}
	for field in g.struct_field_info[layout_type] {
		if field.name == field_name {
			return field
		}
	}
	direct_type := g.struct_direct_member_type(receiver_type, field_name)
	if direct_type != '' {
		return FastcStructField{
			name: field_name
			typ: direct_type
			is_public: true
			is_mutable: true
		}
	}
	for field in g.struct_field_info[layout_type] {
		if !field.name.starts_with('__embedded_') {
			continue
		}
		// Accessing the embed itself by its type name (`d.SilentStreamingDownloader`):
		// resolve to the `__embedded_N` field, keeping its concrete type.
		if field.typ == field_name || field.typ.all_after_last('__') == field_name {
			// The `__embedded_N` field IS the target; its name already renders the
			// access, so return it as-is (no extra storage path).
			return field
		}
		if embedded := g.struct_field_metadata(field.typ, field_name) {
			mut promoted := embedded
			mut storage_path := [field.name]
			storage_path << embedded.storage_path
			promoted.storage_path = storage_path
			return promoted
		}
	}
	return none
}

// dot_piece renders a `.` in the token stream: `__` for a module/enum separator,
// the embedded-struct access path for `local.embedded_field` (so `ss.pos` becomes
// `ss->__embedded_0.pos`), or the ordinary `->`/`.` for a pointer/value receiver.
fn (g &Parser) dot_piece(previous token.Token, previous_lit string, module_separator bool) string {
	if module_separator {
		return if previous_lit == 'C' { '' } else { '__' }
	}
	if g.selfhost && previous == .name && previous_lit in g.locals {
		promoted := g.embedded_dot_piece_for_local(previous_lit)
		if promoted != '' {
			return promoted
		}
		if g.local_is_pointer(previous_lit) {
			return '->'
		}
		return '.'
	}
	return '.'
}

// embedded_dot_piece_for_local promotes `receiver.field` through embedded structs.
// The scanner is positioned just past the `.`, so the field name is the next token.
// Returns '' when the field is a direct member or cannot be resolved.
fn (g &Parser) embedded_dot_piece_for_local(receiver_name string) string {
	local := g.locals[receiver_name] or { return '' }
	if local.typ == '' {
		return ''
	}
	mut look := g.s
	if look.scan() != .name {
		return ''
	}
	return g.embedded_field_dot_piece_for_type(local.typ, look.lit)
}

// embedded_field_dot_piece_for_type returns the C separator-and-path that replaces
// the `.` in `receiver.field` when `field` is reached through embedded structs (e.g.
// `->__embedded_0.`), or '' when `field` is a direct member (or unknown). The field
// name itself is emitted by the caller after this separator.
fn (g &Parser) embedded_field_dot_piece_for_type(receiver_type string, field_name string) string {
	field := g.struct_field_metadata(receiver_type, field_name) or { return '' }
	if field.storage_path.len == 0 {
		return ''
	}
	mut current_type := receiver_type
	mut piece := ''
	for storage_name in field.storage_path {
		separator := if current_type.ends_with('*') { '->' } else { '.' }
		piece += separator + fastc_c_identifier(storage_name)
		current_type = g.struct_direct_member_type(current_type, storage_name)
		if current_type == '' {
			return ''
		}
	}
	piece += if current_type.ends_with('*') { '->' } else { '.' }
	return piece
}

fn (g &Parser) struct_field_is_visible(field FastcStructField) bool {
	// A compiler-generated generic specialization keeps V's reflection privileges:
	// json2's `$for field in T.fields` may access private fields of the concrete caller type.
	return g.in_mono_drain || field.is_public || field.module_name in ['', g.module_name]
}

fn (g &Parser) validate_expression_field_visibility(tokens []FastcExpressionToken) ! {
	for i, item in tokens {
		if item.tok == .lcbr {
			g.validate_struct_literal_field_visibility(tokens, i)!
		}
		if item.tok != .dot || i == 0 || i + 1 >= tokens.len || tokens[i + 1].tok != .name || g.expression_dot_is_module_separator(tokens, i) {
			continue
		}
		receiver_start := fastc_method_receiver_start(tokens, i)
		receiver_type := g.infer_expression_type(tokens[receiver_start..i]) or { continue }
		if i + 2 < tokens.len && tokens[i + 2].tok == .lpar && g.method_function_key(receiver_type, tokens[i + 1].lit) in g.functions {
			continue
		}
		field := g.struct_field_metadata(receiver_type, tokens[i + 1].lit) or { continue }
		if !g.struct_field_is_visible(field) {
			type_name := g.semantic_type_key(receiver_type).all_after_last('.')
			return g.unsupported('private field `${type_name}.${field.name}` from imported module `${field.module_name}`')
		}
	}
}

fn (g &Parser) validate_struct_literal_field_visibility(tokens []FastcExpressionToken, open int) ! {
	mut type_start := open - 1
	if open >= 3 && tokens[open - 3].tok == .name && tokens[open - 2].tok == .dot && tokens[open - 1].tok == .name {
		type_start = open - 3
	}
	if type_start < 0 {
		return
	}
	c_type := g.type_from_expression_tokens(tokens[type_start..open]) or { return }
	layout_type := fastc_trim_pointer_suffix(c_type)
	if layout_type !in g.struct_field_info {
		return
	}
	close := fastc_matching_delimiter(tokens, open, .lcbr, .rcbr) or { return }
	mut index := open + 1
	mut initialized_fields := map[string]bool{}
	mut has_update := false
	mut has_field := false
	for index < close {
		for index < close && tokens[index].tok in [.semicolon, .comma] {
			index++
		}
		if index >= close {
			break
		}
		if tokens[index].tok == .ellipsis {
			if has_update {
				return g.unsupported('duplicate struct update expression')
			}
			if has_field {
				return g.unsupported('struct update expression must be first')
			}
			has_update = true
			index++
			for index < close && tokens[index].tok !in [.semicolon, .comma] {
				index++
			}
			continue
		}
		if tokens[index].tok != .name {
			return
		}
		field_token := tokens[index]
		field_name := tokens[index].lit
		has_field = true
		if field_name in initialized_fields {
			type_name := g.semantic_type_key(c_type).all_after_last('.')
			return g.unsupported('duplicate field `${type_name}.${field_name}` in struct literal')
		}
		field := g.struct_field_metadata(c_type, field_name) or { return }
		initialized_fields[field_name] = true
		if !g.struct_field_is_visible(field) {
			type_name := g.semantic_type_key(c_type).all_after_last('.')
			return g.unsupported('private field `${type_name}.${field.name}` from imported module `${field.module_name}`')
		}
		index++
		mut has_explicit_value := false
		mut value_start := index - 1
		if index < close && tokens[index].tok == .colon {
			has_explicit_value = true
			index++
			value_start = index
		}
		mut parens := 0
		mut brackets := 0
		mut braces := 0
		for index < close {
			match tokens[index].tok {
				.lpar {
					parens++
				}
				.rpar {
					parens--
				}
				.lsbr {
					brackets++
				}
				.rsbr {
					brackets--
				}
				.lcbr {
					braces++
				}
				.rcbr {
					braces--
				}
				.semicolon, .comma {
					if parens == 0 && brackets == 0 && braces == 0 {
						break
					}
				}
				else {}
			}
			index++
		}
		value_tokens := if has_explicit_value {
			tokens[value_start..index]
		} else {
			[field_token]
		}
		if fastc_fixed_array_element_type(field.typ) != none {
			g.validate_fixed_array_struct_field_length(c_type, field, value_tokens)!
		}
	}
	if !has_update {
		for field in g.struct_field_info[layout_type] {
			if field.is_required && field.name !in initialized_fields {
				type_name := g.semantic_type_key(c_type).all_after_last('.')
				return g.unsupported('field `${type_name}.${field.name}` must be initialized')
			}
		}
	}
}

fn (g &Parser) validate_fixed_array_struct_field_length(c_type string, field FastcStructField, value_tokens []FastcExpressionToken) ! {
	element_type := fastc_fixed_array_element_type(field.typ) or {
		return g.unsupported('unverifiable fixed-array type for struct field `${field.name}`')
	}
	array_end := if value_tokens.len > 0 && value_tokens.last().tok == .not {
		value_tokens.len - 1
	} else {
		value_tokens.len
	}
	type_name := g.semantic_type_key(c_type).all_after_last('.')
	if array_end < 2 || value_tokens[0].tok != .lsbr || value_tokens[array_end - 1].tok != .rsbr {
		return
	}
	items := fastc_expression_list_items(value_tokens, 1, array_end - 1)!
	expected_length_source := fastc_fixed_array_length(field.typ) or {
		return g.unsupported('unverifiable fixed-array length for struct field `${type_name}.${field.name}`')
	}
	if expected_length := g.fixed_array_length_value(expected_length_source) {
		if items.len != expected_length {
			return g.unsupported('fixed-array struct field `${type_name}.${field.name}` expects ${expected_length} elements, got ${items.len}')
		}
	}
	if fastc_fixed_array_element_type(element_type) != none {
		for item in items {
			nested_field := FastcStructField{
				name: field.name
				typ: element_type
			}
			g.validate_fixed_array_struct_field_length(c_type, nested_field, item)!
		}
	}
}
