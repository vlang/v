module fastc

import v.pref
import v.scanner
import v.token

fn fastc_unaccelerated_header_block(mut scan scanner.Scanner) token.Token {
	mut depth := 1
	for {
		tok := scan.scan()
		if tok == .lcbr {
			depth++
		} else if tok == .rcbr {
			depth--
			if depth == 0 { return tok }
		} else if tok == .eof {
			return tok
		}
	}
	return .eof
}

fn test_fastc_header_block_skip_preserves_closing_brace_and_following_tokens() {
	prefs := pref.new_preferences()
	bodies := [
		'value_one := parameter + item_2\nresult := value_one\n',
		r'text := "import { text }"; raw := r"${ import { raw }"; C.printf(c"{ import")',
		r'text := "${if true { 1 } else { 2 }} import"; result := value_one',
		r'text := "${"nested } import ${value_one}"} tail"; result := value_two',
		'// import dead }\nvalue := 1\n/* outer /* nested */ } */\nnext := 2',
		'/* /*/ value := 1 /* //*/ next := 2',
		r'@r"${raw}"; @c"${normal}"; @js"${normal}"',
		r'text := js"${if true { "import }" } else { "{" }}"; result := value_one',
		'value_one.type\nvalue_two.import\nresult := value_one\n',
		'value_one := parameter\r\n// import }\r\nresult := value_one\r\n',
		'#flag ignored } import "text"\nnext := value_one\n',
		'value := values#[..]\nnext := 1.25e+3\n',
	]
	for body in bodies {
		source := '{\n${body}\n}\nimport after_block\nfn following() {}'
		mut actual := scanner.new_scanner(prefs, .normal)
		mut expected := scanner.new_scanner(prefs, .normal)
		actual.init(token.File.unindexed('header_block.v', source.len), source)
		expected.init(token.File.unindexed('header_block.v', source.len), source)
		assert actual.scan() == .lcbr
		assert expected.scan() == .lcbr
		assert fastc_skip_header_block(mut actual) == fastc_unaccelerated_header_block(mut expected), body
		assert actual.offset == expected.offset, body
		assert actual.pos == expected.pos, body
		for _ in 0 .. 8 {
			actual_tok := actual.scan()
			expected_tok := expected.scan()
			assert actual_tok == expected_tok, body
			assert actual.lit == expected.lit, body
			assert actual.offset == expected.offset, body
			assert actual.pos == expected.pos, body
		}
	}
}

fn fastc_assert_header_block_scan_parity(source string, prefs &pref.Preferences) {
	mut expected := FastcSourceHeader{}
	mut expected_error := ''
	if header := fastc_probe_unaccelerated_source_header(source, 'header_block.v', prefs) {
		expected = header
	} else {
		expected_error = err.msg()
	}
	if actual := fastc_scan_source_header(source, 'header_block.v', prefs) {
		assert expected_error == '', source
		assert actual.module_name == expected.module_name, source
		assert actual.imports == expected.imports, source
		assert actual.import_order == expected.import_order, source
		assert actual.blank_imports == expected.blank_imports, source
		assert actual.has_globals == expected.has_globals, source
		assert actual.translated == expected.translated, source
	} else {
		assert err.msg() == expected_error, source
	}
}

fn test_fastc_header_block_skip_matches_previous_header_results_and_errors_on_prefixes() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	seeds := [
		r'module main
import before
fn work() {
	value_name := parameter_name + another_name
	text := "${if true { 1 } else { 2 }} import"
	// import ignored
}
import after
struct Holder { value int = 1 }
import last
',
		r'module main
const text = r"import fake { raw }"
fn work() { text := "${"nested } import ${value}"} import tail" }
import after
',
		r'module main
fn work() { /* outer /* nested */ } */ value := values#[..] }
import after
fn follow() { /* /*/ value := 1 /* //*/ }
',
		r'module main
fn work() {
	@r"${raw}"; @c"${normal}"; @js"${normal}"
	value := 1e+c"${ import dangling"
}
import after
',
		r'module main
$if true { fn work() { C.called(c"}") }
import selected }
$else { fn other() { ignored := r"import }" }
import unselected }
import after
',
		'fn work() { @[has_globals] value := 1 }\nimport after\n',
		'fn work() { module unexpected }\nimport after\n',
		'\xef\xbb\xbfmodule main\nfn work() { évalue := 1 }\nimport after\n',
		'module main\nfn work() { value := 1 }\nimport first as same\nimport other as same\n',
		r'module main
const text = "${if true { "import }" } else { "{" }}"
import after
fn work() { text := js"${if true { "import }" } else { "{" }}" }
import last
',
		'module main\r\nfn work() { value_one.type\r\nvalue_two.import\r\n}\r\nimport after\r\n',
	]
	for seed in seeds {
		for end in 0 .. seed.len + 1 {
			fastc_assert_header_block_scan_parity(seed[..end], prefs)
		}
	}
}

// Differential oracle: header discovery before the block fast path.
fn fastc_probe_unaccelerated_source_header(source string, path string, prefs &pref.Preferences) !FastcSourceHeader {
	file := token.File.unindexed(path, source.len)
	mut scan := scanner.new_scanner(prefs, .normal)
	scan.init(file, source)
	mut module_name := ''
	mut imports := map[string]string{}
	mut import_order := []string{}
	mut blank_imports := []string{}
	mut has_globals := false
	mut translated := false
	mut brace_depth := 0
	// Imports can follow declarations. A byte bound keeps ordinary files from
	// tokenizing function bodies once their final possible import is behind us.
	last_import_offset := fastc_source_last_word_index(source, 'import')
	mut tok := scan.scan()
	for tok != .eof {
		if brace_depth == 0 && fastc_keyword_is_qualifier(tok, scan) {
			tok = scan.scan()
			continue
		}
		if module_name == '' && tok == .attribute {
			mut attribute_depth := 1
			mut is_attribute_name := true
			tok = scan.scan()
			for attribute_depth > 0 && tok != .eof {
				if attribute_depth == 1 && is_attribute_name && tok == .name
					&& scan.lit in ['has_globals', 'translated'] {
					has_globals = true
					if scan.lit == 'translated' {
						translated = true
					}
				}
				if attribute_depth == 1 {
					is_attribute_name = tok == .semicolon
				}
				if tok in [.lsbr, .lpar, .lcbr, .attribute] {
					attribute_depth++
				} else if tok in [.rsbr, .rpar, .rcbr] {
					attribute_depth--
				}
				tok = scan.scan()
			}
			continue
		}
		if module_name == '' && tok == .key_module {
			tok = scan.scan()
			if (tok != .name && !tok.is_keyword()) || scan.lit.starts_with('@') {
				return error('fastc parser does not support module declaration in ${path}')
			}
			module_name = scan.lit
			tok = scan.scan()
			continue
		}
		if brace_depth == 0 && tok == .dollar {
			mut lookahead := scan
			if lookahead.scan() == .key_if {
				selected := fastc_scan_selected_comptime_branch(mut scan, scan.scan(), path, prefs)!
				if selected.source != '' {
					selected_header := fastc_probe_unaccelerated_source_header(selected.source, path, prefs)!
					fastc_merge_source_header_imports(selected_header, path, mut imports, mut import_order, mut blank_imports)!
					has_globals = has_globals || selected_header.has_globals
				}
				tok = selected.tok
				continue
			}
		}
		if brace_depth == 0 && scan.pos > last_import_offset
			&& tok in [.key_fn, .key_struct, .key_enum, .key_interface, .key_type, .key_const,
				.key_global] {
			break
		}
		if tok != .key_import || brace_depth > 0 {
			if tok == .lcbr {
				brace_depth++
			} else if tok == .rcbr && brace_depth > 0 {
				brace_depth--
			}
			tok = scan.scan()
			continue
		}
		tok = scan.scan()
		if tok == .lpar {
			tok = scan.scan()
			for tok != .rpar && tok != .eof {
				if tok == .semicolon || tok == .comma {
					tok = scan.scan()
					continue
				}
				import_path, alias, selected_names, next_token := fastc_scan_import(mut scan, tok, path)!
				fastc_register_import_alias(import_path, alias, path, mut imports, mut blank_imports)!
				fastc_register_selective_imports(import_path, selected_names, path, mut imports)!
				if import_path !in import_order {
					import_order << import_path
				}
				tok = next_token
			}
			if tok == .rpar {
				tok = scan.scan()
			}
			continue
		}
		import_path, alias, selected_names, next_token := fastc_scan_import(mut scan, tok, path)!
		fastc_register_import_alias(import_path, alias, path, mut imports, mut blank_imports)!
		fastc_register_selective_imports(import_path, selected_names, path, mut imports)!
		if import_path !in import_order {
			import_order << import_path
		}
		tok = next_token
	}
	if module_name == '' {
		module_name = 'main'
	}
	if prefs.backend == 'fastc' && prefs.building_v && prefs.target.os == 'linux'
		&& !(prefs.target.arch == 'amd64' && 'musl' in prefs.user_defines)
		&& module_name == 'builtin' && path.ends_with('builtin_d_gcboehm.c.v') {
		// TinyCC's GC helpers require the platform atomic declarations and
		// library even when the application itself does not import sync.
		// On musl amd64, TinyCC supplies the helpers and the driver supplies
		// the fence object; importing stdatomic would add unsupported atomic.S.
		imports['__fastc_gc_atomic'] = 'sync.stdatomic'
		if 'sync.stdatomic' !in import_order {
			import_order << 'sync.stdatomic'
		}
	}

	if prefs.building_v && !prefs.selfhost && fastc_source_uses_captures(source, prefs) {
		imports['__fastc_closure'] = 'builtin.closure'
		if 'builtin.closure' !in import_order {
			import_order << 'builtin.closure'
		}
	}
	// V auto-injects `import orm` for files that use `sql <conn> { ... }` blocks.
	// Mirror that so the ORM lowering resolves `orm.Table`/`orm.QueryData`/... and the
	// `orm` module is pulled into the source set. Only the real-builtin path has the
	// runtime the ORM needs; the toy runtime cannot compile `orm`.
	// The compiler's own sources never use the ORM, and the probe rescans every
	// file that mentions `sql` anywhere (FastC's ORM lowering does), so self-host
	// builds skip it.
	if prefs.building_v && !prefs.selfhost && module_name != 'orm' && 'orm' !in imports && fastc_source_uses_sql(source, prefs) {
		imports['orm'] = 'orm'
		if 'orm' !in import_order {
			import_order << 'orm'
		}
	}
	if prefs.building_v && prefs.backend == 'fastc' && imports['driver'] == 'v.driver' && 'fastcdriver' in imports {
		fastcdriver_module := imports['fastcdriver']
		imports['driver'] = fastcdriver_module
		for i, imported_module in import_order {
			if imported_module == 'v.driver' {
				import_order[i] = fastcdriver_module
			}
		}
	}
	// The declaration keyword flags are filled in by fastc_collect_generic_method_sources,
	// the first parallel pass, so discovery waves only scan imports.
	return FastcSourceHeader{
		module_name:   module_name
		imports:       imports
		import_order:  import_order
		blank_imports: blank_imports
		has_globals:   has_globals
		translated:    translated
	}
}
