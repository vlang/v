module fastc

import os
import v.pref
import v.scanner
import v.token

// FastcConstantCallScan caches what fastc_constant_call_dependencies reads: the
// function key of each C function name, the source files and the names in each
// function body.
struct FastcConstantCallScan {
mut:
	keys_by_c_name map[string]string
	files          map[string]string
	bodies         map[string]FastcFunctionBodyNames
}

// FastcFunctionBodyNames lists the unqualified names in a function body, and those
// of them that are called.
struct FastcFunctionBodyNames {
	names []string
	calls []string
}

// fastc_constant_call_dependencies returns the constants of a runtime constant's module
// that the functions called by its initializer read, also through the calls those
// functions make in the module. `const table = build()` must be initialized after
// the `const lengths` that `build` reads, also when `lengths` is in a later file.
fn fastc_constant_call_dependencies(value FastcConstantValue, constant_keys map[string]bool, functions map[string]FastcFunctionSignature, function_c_names map[string]string, prefs &pref.Preferences, mut scan FastcConstantCallScan) []string {
	called := fastc_rendered_call_names(value.value)
	if called.len == 0 {
		return []string{}
	}
	if scan.keys_by_c_name.len == 0 {
		for key, signature in functions {
			if !key.starts_with('C.') {
				c_name := function_c_names[key] or { fastc_signature_c_name(key, signature) }
				scan.keys_by_c_name[c_name] = key
			}
		}
	}
	mut pending := []string{}
	for c_name in called {
		key := scan.keys_by_c_name[c_name] or { continue }
		signature := functions[key] or { continue }
		if signature.module_name == value.module_name {
			pending << key
		}
	}
	mut seen := map[string]bool{}
	mut dependencies := []string{}
	for pending.len > 0 {
		key := pending.pop()
		if seen[key] {
			continue
		}
		seen[key] = true
		signature := functions[key] or { continue }
		body := fastc_function_body_names(key, signature, prefs, mut scan)
		for name in body.names {
			constant := fastc_constant_key(value.module_name, name)
			if constant != value.key && constant_keys[constant] && constant !in dependencies {
				dependencies << constant
			}
		}
		for name in body.calls {
			callee := fastc_function_key(value.module_name, name)
			if !seen[callee] {
				if callee_signature := functions[callee] {
					if callee_signature.module_name == value.module_name {
						pending << callee
					}
				}
			}
		}
	}
	return dependencies
}

// fastc_rendered_call_names returns the identifiers that the rendered C expression
// `source` calls.
fn fastc_rendered_call_names(source string) []string {
	mut names := []string{}
	mut offset := 0
	for offset < source.len {
		if source[offset] in [`'`, `"`] {
			offset = fastc_c_literal_end(source, offset)
			continue
		}
		if !fastc_c_identifier_start_byte(source[offset]) {
			offset++
			continue
		}
		start := offset
		offset++
		for offset < source.len && fastc_identifier_byte(source[offset]) {
			offset++
		}
		mut next := offset
		for next < source.len && source[next] == ` ` {
			next++
		}
		if next < source.len && source[next] == `(` {
			name := source[start..offset]
			if name !in names {
				names << name
			}
		}
	}
	return names
}

// fastc_function_body_names scans the body of the function declared at
// `signature.position` in `signature.path`.
fn fastc_function_body_names(key string, signature FastcFunctionSignature, prefs &pref.Preferences, mut scan FastcConstantCallScan) FastcFunctionBodyNames {
	if cached := scan.bodies[key] {
		return cached
	}
	mut text := scan.files[signature.path] or { '' }
	if text == '' {
		text = os.read_file(signature.path) or { '' }
		scan.files[signature.path] = text
	}
	if signature.position < 0 || signature.position >= text.len {
		return FastcFunctionBodyNames{}
	}
	source := text[signature.position..]
	mut scanner_ := scanner.new_scanner(prefs, .normal)
	scanner_.init(token.File.unindexed(signature.path, source.len), source)
	mut names := []string{}
	mut calls := []string{}
	mut depth := 0
	mut previous := token.Token.unknown
	mut previous_name := ''
	mut tok := scanner_.scan()
	for tok != .eof {
		if tok == .lcbr {
			depth++
		} else if tok == .rcbr {
			depth--
			if depth == 0 {
				break
			}
		} else if depth > 0 && tok == .lpar && previous == .name && previous_name != '' {
			if previous_name !in calls {
				calls << previous_name
			}
		}
		if depth > 0 && tok == .name && previous != .dot {
			previous_name = scanner_.lit
			if previous_name !in names {
				names << previous_name
			}
		} else {
			previous_name = ''
		}
		previous = tok
		tok = scanner_.scan()
	}
	body := FastcFunctionBodyNames{
		names: names
		calls: calls
	}
	scan.bodies[key] = body
	return body
}
