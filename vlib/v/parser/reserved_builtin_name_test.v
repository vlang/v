module parser

import os
import v.pref

fn reserved_builtin_name_diagnostics(source string, formatting bool) []Diagnostic {
	path := os.join_path(os.vtmp_dir(), 'reserved_builtin_names_${os.getpid()}.v')
	os.write_file(path, source) or { panic(err) }
	defer {
		os.rm(path) or {}
	}
	mut prefs := pref.new_preferences()
	prefs.is_fmt = formatting
	mut p := Parser.new(prefs)
	_ := p.parse_file(path)
	return p.diagnostics.clone()
}

fn test_parser_builtin_names_are_rejected_at_main_function_declarations() {
	for name in ['dump', 'sizeof', 'typeof', 'isreftype'] {
		for prefix in ['', 'pub '] {
			source := '${prefix}fn ${name}(a int, b int) int { return a + b }\nfn main() { ${name}(1, 2) }\n'
			diagnostics := reserved_builtin_name_diagnostics(source, false)
			matching := diagnostics.filter(it.message == 'cannot redefine builtin function `${name}`')
			assert matching.len == 1, diagnostics.str()
			assert source[matching[0].pos.offset..matching[0].pos.end] == name
		}
	}
}

fn test_parser_builtin_names_keep_methods_interop_and_module_functions() {
	for name in ['dump', 'sizeof', 'typeof', 'isreftype'] {
		for source in [
			'module other\npub fn ${name}(a int, b int) int { return a + b }\n',
			'struct Reader {}\nfn (r Reader) ${name}(a int, b int) int { return a + b }\nfn main() { _ = Reader{}.${name}(1, 2) }\n',
			'fn C.${name}(a int, b int) int\nfn main() {}\n',
		] {
			diagnostics := reserved_builtin_name_diagnostics(source, false)
			assert diagnostics.len == 0, diagnostics.str()
		}
	}
}

fn test_formatter_can_parse_main_functions_named_like_parser_builtins() {
	for name in ['dump', 'sizeof', 'typeof', 'isreftype'] {
		diagnostics := reserved_builtin_name_diagnostics('fn ${name}(a int, b int) int { return a + b }\n',
			true)
		assert diagnostics.len == 0, diagnostics.str()
	}
}
