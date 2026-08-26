module fastc

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

// fastc_type_text_hash is an FNV-1a digest of a type spelling. The readable
// sanitized name alone is lossy (`Foo*` and a declared `Foo_ptr` collapse),
// so thread type names append this stable discriminator to stay injective.
fn fastc_type_text_hash(text string) string {
	mut digest := u32(2166136261)
	for i in 0 .. text.len {
		digest = (digest ^ u32(text[i])) * 16777619
	}
	return digest.hex()
}

fn fastc_thread_type_name(value_type string) string {
	if value_type in ['', 'void'] {
		return '${fastc_thread_type_prefix}void'
	}
	sanitized := value_type.replace('*', '_ptr').replace(' ', '_').replace('.', '__')
	return '${fastc_thread_type_prefix}${sanitized}_${fastc_type_text_hash(value_type)}'
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
	// Mirror validate_expression_calls: read_spawn_expression consumes the
	// call eagerly, so the ordinary call-visibility validation never runs.
	if !signature.is_public && signature.module_name != '' && signature.module_name != g.module_name
		&& signature.module_name != 'builtin' && signature.module_name in g.imports.values() {
		return g.unsupported('spawn of private function `${callee}` from imported module `${signature.module_name}`')
	}
	if signature.is_variadic || signature.last_parameter_is_params {
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
		argument := g.read_expression([token.Token.comma, token.Token.rpar])!
		arguments << argument
		if g.tok == .comma {
			g.next()
		}
	}
	g.next()
	if arguments.len != signature.parameter_types.len {
		return g.unsupported('spawn of `${callee}` with ${arguments.len} arguments, expected ${signature.parameter_types.len}')
	}
	value_type := if signature.return_type in ['', 'void'] {
		''
	} else {
		signature.return_type
	}
	thread_type := fastc_thread_type_name(value_type)
	g.register_spawn_helpers(function_key, thread_type, value_type, signature.parameter_types)
	start_name := fastc_spawn_start_name(function_key)
	g.last_expression = []FastcExpressionToken{}
	g.last_expression_type = thread_type
	g.last_multi_return_types = []string{}
	return '${start_name}(${arguments.join(', ')})'
}

fn fastc_spawn_start_name(function_key string) string {
	return '__v_fastc_spawn_start_${fastc_c_identifier(fastc_c_function_name_for_key(function_key))}'
}

// register_spawn_helpers records the thread typedef, waiter, packed-argument
// struct, run wrapper, and creator for one spawn target. The result is the
// first packed field, so the per-type waiter reads it without knowing which
// call site produced the thread.
fn (mut g Parser) register_spawn_helpers(function_key string, thread_type string, value_type string, parameter_types []string) {
	g.thread_value_types[thread_type] = value_type
	if thread_type !in g.spawn_typedefs {
		g.spawn_typedefs[thread_type] = 'typedef struct { pthread_t handle; void *packed; } ${thread_type};'
		// Thread handles are routinely collected into arrays for joining.
		g.composite_types['Array_${thread_type}'] = true
	}
	wait_name := fastc_thread_wait_name(thread_type)
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
	start_name := fastc_spawn_start_name(function_key)
	if start_name in g.spawn_helpers {
		return
	}
	target := fastc_c_function_name_for_key(function_key)
	args_struct := '__v_fastc_spawn_args_${fastc_c_identifier(target)}'
	run_name := '__v_fastc_spawn_run_${fastc_c_identifier(target)}'
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
	pthread_attr_init(&attributes);
	pthread_attr_setstacksize(&attributes, 8 * 1024 * 1024);
	if (pthread_create(&t.handle, &attributes, ${run_name}, args) != 0) {
		free(args);
		fprintf(stderr, "spawn: thread creation failed\\n");
		exit(1);
	}
	pthread_attr_destroy(&attributes);
	return t;
}'
}
