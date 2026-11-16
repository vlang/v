module driver

import os
import crypto.sha256
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
		os.real_path(os.join_path(root, 'vlib', 'os', 'fd_poll.h')),
	]
}

// A TinyCC build cannot reliably diagnose date/time macros. Its interface
// contract is limited to this audited bundled snapshot, hashed as opaque bytes.
// Audited against master 8b3c052fc74eb10434bdcb1a97d6409bc1e08cc3 (100 files),
// plus the stateless fd_poll.h interface at 645f534631f9a4b2eb69071116e79c039ebcec11.
// Changing the snapshot requires reviewing its ownership and determinism again.
fn v3_pinned_web_native_interfaces(vroot string) bool {
	root := os.real_path(vroot)
	mut paths := os.walk_ext(os.join_path(root, 'thirdparty', 'mbedtls', 'include'), '',
		hidden: true
	)
	paths << [os.join_path(root, 'vlib', 'net', 'mbedtls', 'mbedtls_helpers.h'),
		os.join_path(root, 'vlib', 'net', 'mbedtls', 'mbedtls_threading.h'),
		os.join_path(root, 'vlib', 'os', 'fd_poll.h')]
	paths.sort()
	mut parts := []string{cap: paths.len}
	for path in paths {
		digest := v3_file_sha256_hex(path) or { return false }
		name := path[root.len + 1..].replace('\\', '/')
		parts << name + '\x00' + digest
	}
	return sha256.hexhash(parts.join('\n')) == '098217945ff40c6779f5c5a4f6167e63107e04c17426c34c0ea2c0eb6d4efe84'
}

fn v3_native_interface_configuration_override(value string) bool {
	return value.contains('MBEDTLS_CONFIG_FILE') || value.contains('MBEDTLS_USER_CONFIG_FILE')
		|| value.contains('MBEDTLS_PSA_CRYPTO_CONFIG')
		|| value.contains('MBEDTLS_PSA_CRYPTO_USER_CONFIG_FILE')
}

fn v3_native_interface_suppresses_date_time(flag string) bool {
	return flag in ['-w', '--no-warnings'] || flag.starts_with('-Wno-')
}

fn v3_native_interface_diagnostic_flags(flags []string) []string {
	mut result := []string{cap: flags.len}
	mut i := 0
	for i < flags.len {
		flag := flags[i]
		if v3_native_interface_suppresses_date_time(flag)
			|| (flag.starts_with('-Xclang=')
				&& v3_native_interface_suppresses_date_time(flag.all_after('='))) {
			i++
			continue
		}
		if flag in ['-Xclang', '-Xpreprocessor'] && i + 1 < flags.len
			&& v3_native_interface_suppresses_date_time(flags[i + 1]) {
			i += 2
			continue
		}
		result << flag
		if c_flag_consumes_next_operand(flag) && i + 1 < flags.len {
			i++
			result << flags[i]
		}
		i++
	}
	return result
}

fn v3_native_interface_closure(a &flat.FlatAst, prefs &pref.Preferences, user_flags []string, compiler string, native_inputs &cgen.CacheNativeInputs) V3NativeInputClosure {
	mut result := v3_native_input_closure(native_inputs)
	if result.unassignable == '' { return result }
	tinycc := pref.ccompiler_is_tinycc(compiler)
	if tinycc && !v3_pinned_web_native_interfaces(prefs.vroot) { return result }
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
			&& v3_native_interface_configuration_override(node.typ_text()) {
			return result
		}
	}
	mut flags := user_flags.clone()
	flags << cgen.cache_directive_flags(a, prefs.vroot, prefs.target, prefs.compile_values)
	// The ownership contract covers the bundled configuration. An alternate
	// configuration can inject arbitrary native storage or implementation code.
	for flag in flags {
		if v3_native_interface_configuration_override(flag)
			|| flag.starts_with('@') || flag.starts_with('-Wp,')
			|| flag.starts_with('--config') || flag.starts_with('-specs')
			|| flag.starts_with('--specs') || flag.starts_with('-include-pch') {
			return result
		}
	}
	for name in ['MBEDTLS_CONFIG_FILE', 'MBEDTLS_USER_CONFIG_FILE', 'MBEDTLS_PSA_CRYPTO_CONFIG',
		'MBEDTLS_PSA_CRYPTO_CONFIG_FILE', 'MBEDTLS_PSA_CRYPTO_USER_CONFIG_FILE'] {
		if os.getenv(name) != '' { return result }
	}
	path := os.join_path(os.real_path(os.vtmp_dir()), 'v3_native_interfaces_${tempname.unique_token()}.c')
	defer { os.rm(path) or {} }
	probe := cgen.cache_native_dependency_directives(a, prefs.vroot, flags, prefs.target)
	checked_probe := if tinycc {
		probe
	} else {
		'#pragma GCC poison __DATE__ __TIME__ __TIMESTAMP__\n' + probe
	}
	os.write_file(path, checked_probe) or { return result }
	args := c_object_compile_flags(flags)
	// -M does not expand macros in ordinary code. Let the selected C
	// preprocessor validate them too, discarding its output without inspecting it.
	if !tinycc {
		// -w also suppresses -Werror=date-time, including in forced headers
		// processed before the poison pragma in the probe itself.
		mut check_args := v3_native_interface_diagnostic_flags(args)
		check_args << ['-x', 'c', '-Werror=date-time', '-E', '-o', os.path_devnull, path]
		checked := cmdexec.run(compiler, check_args)
		if checked.exit_code != 0 { return result }
	}
	// Suppress warnings only for the dependency manifest, whose parser expects
	// compiler-produced dependency data without interleaved diagnostics.
	deps := c_object_dependencies(compiler, [...args, '-x', 'c', '-w'], path)
	if deps.used_fallback { return result }
	// A fresh compiler manifest captures nested includes and current search-path
	// choices. Every cached unit sees these interfaces, so all depend on it.
	result.inputs['__v3_c_flags__'] << deps.files.filter(it != path)
	result.unassignable = ''
	return result
}
