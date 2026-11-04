module driver

import os
import v.cmdexec
import v.flat
import v.gen.c as cgen
import v.pref
import v.tempname

// These shipped interfaces declare ABI types and inline helpers only. Their
// implementations and mutable library state belong to the separate #flag objects.
// This contract is explicit; V never opens a C header to infer its ownership.
fn v3_web_native_interface(path string, vroot string) bool {
	root := os.real_path(vroot)
	public_headers := os.join_path(root, 'thirdparty', 'mbedtls', 'include') + os.path_separator
	if path.starts_with(public_headers) && path.ends_with('.h') { return true }
	return path in [
		os.real_path(os.join_path(root, 'vlib', 'net', 'mbedtls', 'mbedtls_helpers.h')),
		os.real_path(os.join_path(root, 'vlib', 'net', 'mbedtls', 'mbedtls_threading.h')),
	]
}

fn v3_native_interface_closure(a &flat.FlatAst, prefs &pref.Preferences, user_flags []string, compiler string, native_inputs &cgen.CacheNativeInputs) V3NativeInputClosure {
	mut result := v3_native_input_closure(native_inputs)
	if result.unassignable == '' { return result }
	// TinyCC cannot reliably reject date/time macro expansion.
	if pref.ccompiler_is_tinycc(compiler) { return result }
	for paths in native_inputs.module_inputs.values() {
		for path in paths {
			if native_inputs.macro_paths[path]
				|| (native_inputs.native_paths[path] && !v3_web_native_interface(path, prefs.vroot)) {
				return result
			}
		}
	}
	for node in a.nodes {
		if node.kind == .directive && node.value in ['define', 'undef']
			&& (node.typ_text().contains('MBEDTLS_CONFIG_FILE')
				|| node.typ_text().contains('MBEDTLS_USER_CONFIG_FILE')) {
			return result
		}
	}
	mut flags := user_flags.clone()
	flags << cgen.cache_directive_flags(a, prefs.vroot, prefs.target, prefs.compile_values)
	// The ownership contract covers the bundled configuration. An alternate
	// configuration can inject arbitrary native storage or implementation code.
	for flag in flags {
		if flag.contains('MBEDTLS_CONFIG_FILE') || flag.contains('MBEDTLS_USER_CONFIG_FILE')
			|| flag.starts_with('@') || flag.starts_with('-Wp,')
			|| flag.starts_with('--config') || flag.starts_with('-specs')
			|| flag.starts_with('--specs') || flag.starts_with('-include-pch') {
			return result
		}
	}
	if os.getenv('MBEDTLS_CONFIG_FILE') != '' || os.getenv('MBEDTLS_USER_CONFIG_FILE') != '' {
		return result
	}
	path := os.join_path(os.real_path(os.vtmp_dir()), 'v3_native_interfaces_${tempname.unique_token()}.c')
	defer { os.rm(path) or {} }
	os.write_file(path, cgen.cache_native_dependency_directives(a, prefs.vroot, flags, prefs.target)) or { return result }
	mut args := c_object_compile_flags(flags)
	args << ['-x', 'c']
	args << '-Werror=date-time'
	// -M does not expand macros in ordinary code. Let the selected C
	// preprocessor validate them too, discarding its output without inspecting it.
	mut check_args := args.clone()
	check_args << ['-E', '-o', os.path_devnull, path]
	checked := cmdexec.run(compiler, check_args)
	if checked.exit_code != 0 { return result }
	deps := c_object_dependencies(compiler, args, path)
	if deps.used_fallback { return result }
	// A fresh compiler manifest captures nested includes and current search-path
	// choices. Every cached unit sees these interfaces, so all depend on it.
	result.inputs['__v3_c_flags__'] << deps.files.filter(it != path)
	result.unassignable = ''
	return result
}
