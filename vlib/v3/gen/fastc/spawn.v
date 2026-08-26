module fastc

import strings
import v3.token

// FastC lowers `spawn f(args)` to a generated pthread creator: the arguments
// are packed into a heap block whose first field receives the result, a run
// wrapper unpacks and calls `f` on the new thread, and `.wait()` joins and
// returns the packed result. Helpers are registered per call target and
// deduplicated program-wide; the pthread runtime block is emitted only when a
// program spawns.
const fastc_thread_type_prefix = '__v_fastc_thread_'

const c_spawn_runtime = r'#include <pthread.h>
'

// fastc_name_key hex-encodes the original UTF-8 bytes. The readable sanitized
// part of a generated name is lossy, so this C-safe key keeps names injective.
fn fastc_name_key(text string) string {
	hex_digits := '0123456789abcdef'
	mut encoded := strings.new_builder(text.len * 2)
	for value in text.bytes() {
		encoded.write_u8(hex_digits[value >> 4])
		encoded.write_u8(hex_digits[value & 0x0f])
	}
	return encoded.str()
}

fn fastc_thread_type_name(value_type string) string {
	if value_type in ['', 'void'] {
		return '${fastc_thread_type_prefix}void'
	}
	sanitized := value_type.replace('*', '_ptr').replace(' ', '_').replace('.', '__')
	return '${fastc_thread_type_prefix}k${fastc_name_key(value_type)}_${sanitized}'
}

fn fastc_thread_wait_name(thread_type string) string {
	return '__v_fastc_thread_wait_${thread_type.all_after(fastc_thread_type_prefix)}'
}

// read_spawn_expression parses `spawn callee(arguments)` eagerly at an
// expression start. The callee must be a collected plain or imported
// function; its signature supplies the packed-argument field types.
fn (mut g Parser) read_spawn_expression() !string {
	if !g.selfhost {
		return g.unsupported('spawn expressions')
	}
	if g.declaration_initializer_mode {
		// These parsers discard spawn helper registrations, so the emitted C
		// would reference undefined creators and thread types.
		return g.unsupported('spawn in constant, global, or field default initializers')
	}
	if g.prefs.normalized_target_os() == 'windows' {
		return g.unsupported('spawn for windows targets')
	}
	g.next()
	if g.tok != .name {
		return g.unsupported('spawn callee token `${g.token_source()}`')
	}
	mut callee := g.lit
	g.next()
	mut function_key := ''
	if g.tok == .dot {
		imported_module := g.imports[callee] or {
			return g.unsupported('spawn on unimported qualifier `${callee}`')
		}
		g.next()
		if g.tok != .name {
			return g.unsupported('spawn qualified callee')
		}
		callee = g.lit
		function_key = fastc_function_key(imported_module, callee)
		g.next()
	} else {
		function_key = g.unqualified_function_key(callee)
	}
	signature := g.functions[function_key] or {
		return g.unsupported('spawn of undeclared function `${callee}`')
	}
	// Ordinary disabled calls elide the call and its arguments. A spawn cannot
	// provide a valid thread handle for an elided call, so reject it before
	// parsing any arguments.
	if signature.is_disabled {
		return g.unsupported('spawn of disabled function `${callee}`')
	}
	// Mirror validate_expression_calls: read_spawn_expression consumes the
	// call eagerly, so the ordinary call-visibility validation never runs.
	if !signature.is_public && signature.module_name != '' && signature.module_name != g.module_name
		&& signature.module_name != 'builtin' && signature.module_name in g.imports.values() {
		return g.unsupported('spawn of private function `${callee}` from imported module `${signature.module_name}`')
	}
	if signature.is_variadic {
		return g.unsupported('spawn of variadic function `${callee}`')
	}
	if signature.option_type != '' {
		return g.unsupported('spawn of option or result function `${callee}`')
	}
	if signature.return_types.len > 1 {
		return g.unsupported('spawn of multi-return function `${callee}`')
	}
	if g.tok != .lpar {
		return g.unsupported('spawn call arguments')
	}
	g.next()
	mut arguments := []string{}
	for g.tok != .rpar {
		if g.tok == .eof {
			return g.unsupported('unfinished spawn call')
		}
		if g.tok == .key_mut {
			return g.unsupported('spawn with a `mut` argument')
		}
		expected_type := if arguments.len < signature.parameter_types.len {
			signature.parameter_types[arguments.len]
		} else {
			''
		}
		previous_expected_type := g.expected_expression_type
		g.expected_expression_type = expected_type
		mut argument := g.read_expression([token.Token.comma, token.Token.rpar])!
		g.expected_expression_type = previous_expected_type
		// Mirror ordinary calls: arguments that need contextual typing (enum
		// shorthand, boxing) render through the parameter-typed pipeline.
		argument_tokens := g.last_expression.clone()
		if expected_type != '' && argument_tokens.len > 0 {
			if contextual := g.render_call_argument_expression(argument_tokens, expected_type) {
				argument = contextual
			}
		}
		arguments << argument
		if g.tok == .comma {
			g.next()
		}
	}
	g.next()
	if signature.last_parameter_is_params && arguments.len + 1 == signature.parameter_types.len {
		parameter_type := signature.parameter_types.last()
		arguments << '(${parameter_type}){0}'
	}
	if arguments.len != signature.parameter_types.len {
		return g.unsupported('spawn of `${callee}` with ${arguments.len} arguments, expected ${signature.parameter_types.len}')
	}
	value_type := if signature.return_type in ['', 'void'] {
		''
	} else {
		signature.return_type
	}
	thread_type := g.fastc_unclaimed_generated_name(fastc_thread_type_name(value_type))
	start_name := g.fastc_unclaimed_generated_name(fastc_spawn_start_name(function_key))
	g.register_spawn_helpers(function_key, thread_type, value_type, signature.parameter_types,
		start_name)
	g.last_expression = []FastcExpressionToken{}
	g.last_expression_type = thread_type
	g.last_multi_return_types = []string{}
	return '${start_name}(${arguments.join(', ')})'
}

fn fastc_spawn_start_name(function_key string) string {
	return '__v_fastc_spawn_start_${fastc_spawn_target_stem(function_key)}'
}

// fastc_spawn_target_stem puts the injective target key before the readable C
// name. A collision suffix on one target therefore cannot equal another
// target's natural generated name, and parallel file parsers choose alike.
fn fastc_spawn_target_stem(function_key string) string {
	readable := fastc_c_identifier(fastc_c_function_name_for_key(function_key))
	return 'k${fastc_name_key(function_key)}_${readable}'
}

// fastc_unclaimed_generated_name screens a deterministic generated name
// against the program's collected `__v_fastc_`-prefixed function and global C
// names and its declared type spellings, suffixing until free. Both inputs
// are frozen program-wide, so every file resolves the same name.
fn (g &Parser) fastc_unclaimed_generated_name(base string) string {
	mut candidate := base
	mut suffix := 0
	for g.generated_name_is_claimed(candidate) {
		suffix++
		candidate = '${base}_c${suffix}'
	}
	return candidate
}

fn (g &Parser) generated_name_is_claimed(candidate string) bool {
	if candidate in g.declared_type_c_names {
		return true
	}
	for claimed in g.fastc_prefixed_c_names {
		if claimed == candidate {
			return true
		}
	}
	return false
}

// register_spawn_helpers records the thread typedef, waiter, packed-argument
// struct, run wrapper, and creator for one spawn target. The result is the
// first packed field, so the per-type waiter reads it without knowing which
// call site produced the thread.
fn (mut g Parser) register_spawn_helpers(function_key string, thread_type string, value_type string, parameter_types []string, start_name string) {
	g.thread_value_types[thread_type] = value_type
	if thread_type !in g.spawn_typedefs {
		g.spawn_typedefs[thread_type] = 'typedef struct { pthread_t handle; void *packed; } ${thread_type};'
		// Thread handles are routinely collected into arrays for joining.
		g.composite_types['Array_${thread_type}'] = true
	}
	wait_name := g.fastc_unclaimed_generated_name(fastc_thread_wait_name(thread_type))
	if wait_name !in g.spawn_helpers {
		mut waiter := ''
		if value_type == '' {
			waiter = 'static void ${wait_name}(${thread_type} t) {
	int join_code = pthread_join(t.handle, NULL);
	if (join_code != 0) {
		fprintf(stderr, "spawn: thread join failed (%d)\\n", join_code);
		exit(1);
	}
	free(t.packed);
}'
		} else {
			waiter = 'static ${value_type} ${wait_name}(${thread_type} t) {
	int join_code = pthread_join(t.handle, NULL);
	if (join_code != 0) {
		fprintf(stderr, "spawn: thread join failed (%d)\\n", join_code);
		exit(1);
	}
	${value_type} result = *(${value_type} *)t.packed;
	free(t.packed);
	return result;
}'
		}
		g.spawn_helpers[wait_name] = waiter
	}
	if start_name in g.spawn_helpers {
		return
	}
	target := fastc_c_function_name_for_key(function_key)
	target_stem := fastc_spawn_target_stem(function_key)
	args_struct := g.fastc_unclaimed_generated_name('__v_fastc_spawn_args_${target_stem}')
	run_name := g.fastc_unclaimed_generated_name('__v_fastc_spawn_run_${target_stem}')
	mut fields := ''
	if value_type != '' {
		fields += '\t${value_type} result;\n'
	}
	if value_type == '' && parameter_types.len == 0 {
		// C forbids empty struct definitions.
		fields += '\tint unused;\n'
	}
	mut parameters := ''
	mut fills := ''
	mut forwards := ''
	for index, parameter_type in parameter_types {
		fields += '\t${parameter_type} arg${index};\n'
		if index > 0 {
			parameters += ', '
			forwards += ', '
		}
		parameters += '${parameter_type} arg${index}'
		fills += '\targs->arg${index} = arg${index};\n'
		forwards += 'args->arg${index}'
	}
	if parameters == '' {
		parameters = 'void'
	}
	call := if value_type == '' {
		'\t${target}(${forwards});'
	} else {
		'\targs->result = ${target}(${forwards});'
	}
	g.spawn_helpers[start_name] = 'typedef struct {
${fields}} ${args_struct};

static void *${run_name}(void *raw) {
	${args_struct} *args = (${args_struct} *)raw;
${call}
	return NULL;
}

static ${thread_type} ${start_name}(${parameters}) {
	${args_struct} *args = (${args_struct} *)malloc(sizeof(${args_struct}));
	if (args == NULL) {
		fprintf(stderr, "spawn: out of memory\\n");
		exit(1);
	}
${fills}	${thread_type} t;
	t.packed = args;
	pthread_attr_t attributes;
	if (pthread_attr_init(&attributes) != 0) {
		free(args);
		fprintf(stderr, "spawn: thread attribute init failed\\n");
		exit(1);
	}
	if (pthread_attr_setstacksize(&attributes, 8 * 1024 * 1024) != 0) {
		pthread_attr_destroy(&attributes);
		free(args);
		fprintf(stderr, "spawn: thread stack size setup failed\\n");
		exit(1);
	}
	if (pthread_create(&t.handle, &attributes, ${run_name}, args) != 0) {
		pthread_attr_destroy(&attributes);
		free(args);
		fprintf(stderr, "spawn: thread creation failed\\n");
		exit(1);
	}
	pthread_attr_destroy(&attributes);
	return t;
}'
}
