// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module x64

const x64_backend_feature_prefix = 'backend feature: '
pub const x64_backend_limitation_hint = 'x64: native backend limitation: retry with the C backend or reduce the source to supported x64 features'

const x64_minimal_unsupported_crt_stdio_symbols = ['stdin', 'stdout', 'stderr', 'printf', 'fprintf',
	'dprintf', 'sprintf', 'snprintf', 'wprintf', 'puts', 'fputs', 'setvbuf', 'fflush', 'fopen',
	'_wfopen', 'fdopen', 'freopen', '_wfreopen', 'fclose', 'pclose', '_pclose', 'fread', 'fwrite',
	'fgets', 'getc', 'feof', 'ferror', 'fseek', 'ftell', 'rewind', 'fileno', '_fileno',
	'_get_osfhandle', '_open_osfhandle', 'fgetpos']

const x64_unsupported_captured_fn_literal_symbol = 'v2_unsupported_captured_fn_literal'
const x64_missing_v_runtime_helpers = ['builtin__Map_string_int__keys']

fn x64_normalize_backend_feature(feature string) string {
	if feature.starts_with(x64_backend_feature_prefix) {
		return feature[x64_backend_feature_prefix.len..]
	}
	return feature
}

fn x64_unsupported_backend_feature_message(feature string) string {
	return 'x64: unsupported backend feature: ${x64_normalize_backend_feature(feature)}'
}

fn x64_unsupported(message string) {
	x64_abort_with_diagnostic(x64_unsupported_backend_feature_message(message))
}

fn x64_abort_with_diagnostic(message string) {
	eprintln(message)
	eprintln(x64_backend_limitation_hint)
	exit(1)
}

fn x64_linker_name(format ObjectFormat) string {
	return match format {
		.elf { 'ELF linker' }
		.macho { 'Mach-O linker' }
		.coff { 'PE linker' }
	}
}

fn x64_unresolved_external_symbol_message(format ObjectFormat, name string, context string) string {
	linker_name := x64_linker_name(format)
	if format == .coff && name in x64_minimal_unsupported_crt_stdio_symbols {
		return x64_unsupported_backend_feature_message('${linker_name} cannot resolve C stdio/file-descriptor symbol `${name}`: Windows x64 native backend uses Kernel32 handles, not C FILE/stdio calls; ${context}')
	}
	if name == x64_unsupported_captured_fn_literal_symbol {
		return x64_unsupported_backend_feature_message('native x64 cannot lower captured function literal: closure environments are not implemented yet; ${context}')
	}
	if name in x64_missing_v_runtime_helpers {
		return x64_unsupported_backend_feature_message('${linker_name} cannot resolve V runtime helper `${name}`: native x64 backend does not implement this feature for this target yet; ${context}')
	}
	return x64_unsupported_backend_feature_message('${linker_name} cannot resolve external symbol `${name}` yet; ${context}')
}

pub fn unsupported_external_symbol_message_for_name(format ObjectFormat, raw_name string, context string) ?string {
	name := x64_normalize_external_symbol_name(format, raw_name)
	if x64_symbol_needs_backend_runtime_support(name) {
		return x64_unresolved_external_symbol_message(format, name, context)
	}
	return none
}

fn x64_normalize_external_symbol_name(format ObjectFormat, name string) string {
	if format == .macho && name.starts_with('_') {
		return name[1..]
	}
	return name
}

fn x64_symbol_needs_backend_runtime_support(name string) bool {
	return name == x64_unsupported_captured_fn_literal_symbol
		|| name in x64_missing_v_runtime_helpers
}
