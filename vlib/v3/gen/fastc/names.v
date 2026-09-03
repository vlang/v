module fastc

import v3.gen.c.naming

fn fastc_type_key(module_name string, name string) string {
	if module_name in ['', 'main', 'builtin'] {
		return name
	}
	return '${module_name}.${name}'
}

fn fastc_c_declared_type_name(type_key string) string {
	return type_key.replace('.', '__')
}

// fastc_declared_type_c_names indexes declared type keys by their generated C
// spelling. semantic_type_key previously rescanned every declared type per
// query, allocating each candidate's C spelling on the way; that scan dominated
// selfhost code generation. The first key registered for a spelling wins,
// matching the old first-match scan over the insertion-ordered key list.
fn fastc_declared_type_c_names(declared_types map[string]bool) map[string]string {
	mut index := map[string]string{}
	for key in declared_types.keys() {
		c_name := fastc_c_declared_type_name(key)
		if c_name !in index {
			index[c_name] = key
		}
	}
	return index
}

fn fastc_declared_type_key_by_name(declared_types map[string]bool) map[string]string {
	mut index := map[string]string{}
	for key in declared_types.keys() {
		name := key.all_after_last('.')
		if name in index {
			index[name] = ''
		} else {
			index[name] = key
		}
	}
	return index
}

fn fastc_function_key(module_name string, name string) string {
	if module_name in ['', 'main'] {
		return name
	}
	return '${module_name}.${name}'
}

fn fastc_constant_key(module_name string, name string) string {
	if module_name in ['', 'main', 'builtin'] {
		return name
	}
	return '${module_name}.${name}'
}

fn fastc_c_constant_name(module_name string, name string) string {
	module_prefix := if module_name == '' { 'main' } else { module_name }
	return '${module_prefix.replace('.', '__')}__${name}'
}

fn fastc_global_key(module_name string, name string) string {
	if module_name in ['', 'main', 'builtin'] {
		return name
	}
	return '${module_name}.${name}'
}

fn fastc_c_global_name(key string) string {
	return fastc_c_identifier(key.replace('.', '__'))
}

fn fastc_c_function_name(module_name string, name string) string {
	return fastc_c_function_name_for_key(if module_name in ['', 'main'] {
		name
	} else {
		'${module_name}.${name}'
	})
}

fn (g &Parser) unqualified_function_key(name string) string {
	if cached := g.unqualified_key_memo[name] {
		return cached
	}
	local_key := fastc_function_key(g.module_name, name)
	mut key := local_key
	if local_key !in g.functions {
		builtin_key := fastc_function_key('builtin', name)
		if builtin_key in g.functions {
			key = builtin_key
		}
	}
	mut w := unsafe { &Parser(g) }
	w.unqualified_key_memo[name] = key
	return key
}

// c_function_name_for_key memoizes fastc_c_function_name_for_key, which
// sanitizes its key twice and is asked for the same keys at every call
// site. The mapping does not depend on the parser's context, so the memo
// is never reset.
fn (g &Parser) c_function_name_for_key(key string) string {
	if cached := g.c_function_name_memo[key] {
		return cached
	}
	c_name := fastc_c_function_name_for_key(key)
	mut w := unsafe { &Parser(g) }
	w.c_function_name_memo[key] = c_name
	return c_name
}

fn fastc_c_function_name_for_key(key string) string {
	if key.starts_with('C.') {
		return naming.c_name(key)
	}
	sanitized := naming.sanitize(key)
	c_name := naming.c_name(key)
	if c_name != sanitized || sanitized.starts_with('v_fastc_') {
		return '__v_fastc_function_${sanitized}'
	}
	return c_name
}

fn fastc_disabled_call_expression(return_type string) string {
	if return_type in ['', 'void'] {
		return '((void)0)'
	}
	return '((${return_type}){0})'
}
