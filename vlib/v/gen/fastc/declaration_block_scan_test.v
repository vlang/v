module fastc

import strings
import v.pref
import v.scanner
import v.token

struct FastcDeclarationBlockProbe {
mut:
	has_types           bool
	declared_types      map[string]bool
	declared_kinds      map[string]FastcDeclaredTypeKind
	enum_flags          map[string]bool
	params_structs      map[string]bool
	declaration_paths   map[string]string
	declaration_modules map[string]string
	type_source         string
	source_origins      []FastcSourceOrigin
	body_spans          []int
	error_message       string
}

fn fastc_probe_declaration_blocks(source string, prefs &pref.Preferences, accelerated bool) FastcDeclarationBlockProbe {
	mut result := FastcDeclarationBlockProbe{}
	mut type_source := strings.new_builder(128)
	if accelerated {
		result.has_types = collect_declared_types(source, 'declaration_blocks.v', 'sample', prefs,
			mut result.declared_types, mut result.declared_kinds, mut result.enum_flags,
			mut result.params_structs, mut result.declaration_paths,
			mut result.declaration_modules, mut type_source, mut result.source_origins, mut result.body_spans) or {
			result.error_message = err.msg()
			false
		}
	} else {
		result.has_types = fastc_probe_unaccelerated_declared_types(source, 'declaration_blocks.v', 'sample', prefs,
			mut result.declared_types, mut result.declared_kinds, mut result.enum_flags,
			mut result.params_structs, mut result.declaration_paths,
			mut result.declaration_modules, mut type_source, mut result.source_origins, mut result.body_spans) or {
			result.error_message = err.msg()
			false
		}
	}
	result.type_source = type_source.str()
	return result
}

fn test_fastc_declaration_block_skip_matches_previous_results_and_errors_on_prefixes() {
	seeds := [
		r'module sample
@[params]
struct Config { value int = 4 }
fn before(value int) Config {
	text := "${if true { "}" } else { "{" }}"
	callback := fn (inner int) int { return inner + value }
	return Config{value: callback(value)}
}
@[flag]
pub enum Mode { first second }
fn (config Config) type() Config { return config }
type Callback = fn (value int) int
fn after() { for value in [1, 2] { assert value > 0 } }
struct Tail { callback Callback }
',
		r'module sample
fn work() {
	text := js"${if true { "import }" } else { "{" }}"
	raw := r"${ ignored }"; C.puts(c"{")
	value.type
	value.struct
	// } struct Ignored {}
	/* outer /* nested */ } struct Ignored {} */
	value := values#[..]
}
struct Tail {}
',
		r'module sample
$if picked ? {
	struct Chosen { value int }
	fn chosen() { text := "${"nested ${42} }"}" }
} $else {
	type Chosen = int
	fn fallback() { ignored := r"}" }
}
fn before() { $if false { struct Local {} } }
$if false { struct Inactive {} } $else $if true { struct Active {} }
struct Tail {}
',
		r'module sample
const callback = fn (value int) int { return value + 1 }
__global other = fn () { text := "}" }
struct WithDefault { callback fn () = fn () { ignored := "}" } }
fn C.external(value int) int
fn later() {}
struct Tail {}
',
		'module sample\r\nfn body() { value.type\r\n// }\r\n}\r\nstruct Tail {}\r\n',
		'module sample\nfn body() { évalue := 1; value := 0b123 }\nstruct Tail {}\n',
		r'module sample
fn body() { value := 1e+c"${ dangling }" }
struct Tail {}
',
		'module sample\nfn before() {}\nstruct Duplicate {}\nfn between() {}\nstruct Duplicate {}\n',
		'module sample\n@[if false]\nstruct Disabled {}\nfn body() {}\n@[params]\nstruct Tail {}\n',
		'module sample\nfn body() { /* /*/ value := 1 /* //*/ }\nstruct Tail {}\n',
	]
	for picked in [false, true] {
		mut prefs := pref.new_preferences()
		prefs.building_v = true
		if picked { prefs.user_defines << 'picked' }
		for seed in seeds {
			for end in 0 .. seed.len + 1 {
				source := seed[..end]
				expected := fastc_probe_declaration_blocks(source, prefs, false)
				actual := fastc_probe_declaration_blocks(source, prefs, true)
				assert actual == expected, source
			}
		}
	}
}

fn test_fastc_declaration_block_skip_preserves_matching_close_diagnostics_and_resume() {
	prefs := pref.new_preferences()
	bodies := [
		'plain_identifier.other_keyword\nnext_identifier := 2\n',
		'unknown := évalue + 0b123\n',
		r'text := js"${if true { "}" } else { "{" }}"; raw := r"${ ignored }"',
		'// ignored }\n/* outer /* nested */ ignored } */\nvalue := 1\n',
		'/* /*/ value := 1 /* //*/\n',
		r'C.called(c"{"); value := values#[..]; @r"${value}"',
	]
	for body in bodies {
		source := '{\n${body}\n}\nfn after() {}\n'
		mut expected := scanner.new_scanner(prefs, .normal)
		mut actual := scanner.new_scanner(prefs, .normal)
		expected.init(token.File.unindexed('body.v', source.len), source)
		actual.init(token.File.unindexed('body.v', source.len), source)
		assert expected.scan() == .lcbr
		assert actual.scan() == .lcbr
		mut depth := 1
		for depth > 0 {
			tok := expected.scan()
			assert tok != .eof, body
			if tok == .lcbr { depth++ }
			if tok == .rcbr { depth-- }
		}
		assert fastc_skip_header_block(mut actual) == .rcbr, body
		assert actual.offset == expected.offset, body
		assert actual.pos == expected.pos, body
		assert actual.lit == expected.lit, body
		assert actual.diagnostics == expected.diagnostics, body
		assert actual.in_str_incomplete == expected.in_str_incomplete, body
		assert actual.in_str_inter == expected.in_str_inter, body
		assert actual.in_str_inter_format == expected.in_str_inter_format, body
		assert actual.str_inter_cbr_depth == expected.str_inter_cbr_depth, body
		assert actual.str_quote == expected.str_quote, body
		assert actual.str_parent_quotes == expected.str_parent_quotes, body
		assert actual.str_parent_depths == expected.str_parent_depths, body
		for _ in 0 .. 8 {
			assert actual.scan() == expected.scan(), body
			assert actual.offset == expected.offset, body
			assert actual.pos == expected.pos, body
			assert actual.lit == expected.lit, body
			assert actual.diagnostics == expected.diagnostics, body
		}
	}
}

// Differential oracle: declaration indexing before the function-body fast path.
fn fastc_probe_unaccelerated_declared_types(source string, path string, module_name string, prefs &pref.Preferences, mut declared_types map[string]bool, mut declared_kinds map[string]FastcDeclaredTypeKind, mut enum_flags map[string]bool, mut params_structs map[string]bool, mut declaration_paths map[string]string, mut declaration_modules map[string]string, mut type_source strings.Builder, mut source_origins []FastcSourceOrigin, mut body_spans []int) !bool {
	mut scratch_body_spans := []int{}
	mut pending_fn := false
	mut fn_body_start := -1
	file := token.File.unindexed(path, source.len)
	mut scan := scanner.new_scanner(prefs, .normal)
	scan.init(file, source)
	mut brace_depth := 0
	mut next_c_struct_is_typedef := false
	mut next_enum_is_flag := false
	mut next_struct_is_params := false
	mut next_type_is_enabled := true
	mut has_type_declarations := false
	mut previous_tok := token.Token.unknown
	mut emitted_end := 0
	mut pending_start := -1
	mut declaration_start := -1
	mut declaration_has_block := false
	mut tok := scan.scan()
	for tok != .eof {
		if brace_depth == 0 && fastc_keyword_is_qualifier(tok, scan) {
			tok = scan.scan()
			continue
		}
		if brace_depth == 0 && tok in [.key_module, .key_import] {
			tok = fastc_skip_module_or_import(mut scan, tok, path)!
			previous_tok = .unknown
			continue
		}
		if brace_depth == 0 && tok == .dollar {
			comptime_start := scan.pos
			mut lookahead := scan
			if lookahead.scan() == .key_if {
				selected := fastc_scan_selected_comptime_branch(mut scan, scan.scan(), path, prefs)!
				comptime_end := if selected.tok == .eof { scan.offset } else { scan.pos }
				if selected.source != '' {
					mut selected_types := strings.new_builder(256)
					mut selected_origins := []FastcSourceOrigin{}
					selected_has_types := fastc_probe_unaccelerated_declared_types(selected.source, path, module_name, prefs, mut declared_types, mut declared_kinds, mut enum_flags, mut params_structs, mut declaration_paths, mut declaration_modules, mut selected_types, mut selected_origins, mut scratch_body_spans)!
					has_type_declarations = has_type_declarations || selected_has_types
					if selected_types.len > 0 {
						start := if pending_start >= 0 { pending_start } else { comptime_start }
						fastc_append_filtered_source_span(source, emitted_end, start, comptime_end, mut type_source, mut source_origins)
						emitted_end = comptime_end
					}
				}
				pending_start = -1
				tok = selected.tok
				previous_tok = .unknown
				continue
			}
		}
		if brace_depth == 0 && tok == .attribute {
			if pending_start < 0 {
				pending_start = scan.pos
			}
			attribute := fastc_scan_declaration_attribute(mut scan, path, prefs)!
			tok = attribute.tok
			next_c_struct_is_typedef = next_c_struct_is_typedef || attribute.is_typedef
			next_enum_is_flag = next_enum_is_flag || attribute.is_flag
			next_struct_is_params = next_struct_is_params || attribute.is_params
			next_type_is_enabled = next_type_is_enabled && attribute.is_enabled
			continue
		}
		if brace_depth == 0 && tok in [.key_pub, .key_static] && pending_start < 0 {
			pending_start = scan.pos
		}
		if brace_depth == 0 && tok in [.key_fn, .key_const, .key_global] {
			pending_fn = tok == .key_fn
			next_c_struct_is_typedef = false
			next_enum_is_flag = false
			next_struct_is_params = false
			next_type_is_enabled = true
			pending_start = -1
			if tok == .key_fn && declaration_start < 0 {
				// A method whose name is a keyword (`fn (e Expr) type() Type`) tokenizes
				// that name as e.g. `.key_type`. Skip the whole function header so the
				// keyword name does not open a bogus type-declaration span here. The
				// `declaration_start < 0` guard keeps the `fn` in a `type X = fn(...)`
				// alias value (which has no receiver or name) out of this path.
				previous_tok = tok
				tok = fastc_skip_function_header(mut scan)!
				continue
			}
		}
		if brace_depth == 0 && tok in [.key_struct, .key_enum, .key_interface, .key_type, .key_union] {
			has_type_declarations = true
			declaration_start = if pending_start >= 0 { pending_start } else { scan.pos }
			declaration_has_block = false
			pending_start = -1
			is_public := previous_tok == .key_pub
			kind := match tok {
				.key_enum { FastcDeclaredTypeKind.enum_ }
				.key_interface { FastcDeclaredTypeKind.interface_ }
				.key_type { FastcDeclaredTypeKind.alias_ }
				.key_union { FastcDeclaredTypeKind.union_ }
				else { FastcDeclaredTypeKind.struct_ }
			}
			tok = scan.scan()
			if tok == .name && next_type_is_enabled {
				name := scan.lit
				tok = scan.scan()
				if name == 'C' && tok == .dot {
					tok = scan.scan()
					if tok == .name {
						key := if next_c_struct_is_typedef {
							'#Ctypedef#${scan.lit}'
						} else {
							'#Cstruct#${scan.lit}'
						}
						declared_types[key] = true
						declaration_paths[key] = path
						declaration_modules[key] = module_name
					}
					next_c_struct_is_typedef = false
					next_enum_is_flag = false
					next_struct_is_params = false
					continue
				}
				key := fastc_type_key(module_name, name)
				if key in declared_types {
					return error('fastc parser does not support duplicate type declaration `${name}` in module `${module_name}` in ${path}')
				}
				declared_types[key] = is_public
				declared_kinds[key] = kind
				declaration_paths[key] = path
				declaration_modules[key] = module_name
				if kind == .enum_ && next_enum_is_flag {
					enum_flags[key] = true
				}
				if kind == .struct_ && next_struct_is_params {
					params_structs[fastc_c_declared_type_name(key)] = true
				}
			}
			next_c_struct_is_typedef = false
			next_enum_is_flag = false
			next_struct_is_params = false
			next_type_is_enabled = true
			continue
		}
		if tok == .lcbr {
			if brace_depth == 0 && pending_fn && fn_body_start < 0 {
				fn_body_start = scan.pos
			}
			brace_depth++
			if declaration_start >= 0 {
				declaration_has_block = true
			}
		} else if tok == .rcbr && brace_depth > 0 {
			brace_depth--
			if brace_depth == 0 && fn_body_start >= 0 {
				body_spans << fn_body_start
				body_spans << scan.offset
				fn_body_start = -1
				pending_fn = false
			}
			if declaration_start >= 0 && declaration_has_block && brace_depth == 0 {
				fastc_append_filtered_source_span(source, emitted_end, declaration_start, scan.offset, mut type_source, mut source_origins)
				emitted_end = scan.offset
				declaration_start = -1
				declaration_has_block = false
			}
		} else if tok == .semicolon && brace_depth == 0 && declaration_start >= 0 && !declaration_has_block {
			// A multi-line sum type (`type Expr = A\n\t| B\n\t| C`) has an inserted
			// semicolon after each variant. Do not close the declaration span while a
			// `|` continuation follows, or only the first variant would be captured and
			// the type would never register as a sum type.
			mut lookahead := scan
			if lookahead.scan() == .pipe {
				previous_tok = tok
				tok = scan.scan()
				continue
			}
			declaration_end := if scan.offset > scan.pos { scan.offset } else { scan.pos }
			fastc_append_filtered_source_span(source, emitted_end, declaration_start, declaration_end, mut type_source, mut source_origins)
			emitted_end = declaration_end
			declaration_start = -1
		}
		if tok == .semicolon && brace_depth == 0 {
			pending_fn = false
		}
		previous_tok = tok
		tok = scan.scan()
	}
	if declaration_start >= 0 {
		fastc_append_filtered_source_span(source, emitted_end, declaration_start, source.len, mut type_source, mut source_origins)
	}
	return has_type_declarations
}

fn test_fastc_serial_declaration_index_publishes_recorded_body_spans() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	prefs.no_parallel = true
	mut sources := []FastcSourceFile{}
	for index in 0 .. 4 {
		source := 'module sample\nstruct Item${index} { value int }\nfn work_${index}() { nested := fn () { value := "}" } }\nfn tail_${index}() {}\n'
		sources << FastcSourceFile{
			path:   'source_${index}.v'
			source: source
			header: fastc_header_with_scan_flags(FastcSourceHeader{ module_name: 'sample' }, fastc_source_scan_flags(source))
		}
	}
	expected := fastc_collect_declaration_chunk(sources, prefs, 0, sources.len)
	assert !expected.failed, expected.error_message
	mut declared_types := map[string]bool{}
	mut declared_kinds := map[string]FastcDeclaredTypeKind{}
	mut enum_flags := map[string]bool{}
	mut params_structs := map[string]bool{}
	mut type_source_paths := map[string]bool{}
	mut type_sources := map[string]FastcDeclarationSource{}
	mut constants := map[string]string{}
	mut public_constants := map[string]bool{}
	mut constant_sources := map[string]FastcDeclarationSource{}
	mut constant_spans := map[string][]int{}
	mut global_sources := map[string]FastcDeclarationSource{}
	mut globals := map[string]string{}
	mut public_globals := map[string]bool{}
	_ := fastc_collect_generic_and_declaration_indexes(mut sources, prefs, mut declared_types,
		mut declared_kinds, mut enum_flags, mut params_structs, mut type_source_paths,
		mut type_sources, mut constants, mut public_constants, mut constant_sources,
		mut constant_spans, mut global_sources, mut globals, mut public_globals) or { panic(err) }
	for source_file in sources {
		assert source_file.header.body_spans.len == 4, source_file.path
		assert source_file.header.body_spans == expected.body_spans[source_file.path], source_file.path
	}
	assert declared_types == expected.declared_types
	assert declared_kinds == expected.declared_kinds
	assert type_sources == expected.type_sources
}

fn test_fastc_published_declaration_body_spans_preserve_source_coordinates() {
	original := FastcSourceFile{
		path:                 'positioned.v'
		source:               'fn body() {}'
		source_offset:        137
		source_line_offset:   23
		source_column_offset: 9
		header:               FastcSourceHeader{ module_name: 'sample' }
	}
	mut sources := [original]
	body_spans := {
		'positioned.v': [10, 12]
	}
	fastc_apply_declaration_body_spans(mut sources, body_spans, 0, 1)
	assert sources[0] == FastcSourceFile{
		...original
		header: fastc_header_with_body_spans(original.header, body_spans[original.path])
	}
}
