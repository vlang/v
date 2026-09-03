module fastc

import os
import strings
import v3.gen.arm64
import v3.pref
import v3.scanner
import v3.ssa
import v3.token

struct FastArm64Value {
	id                   ssa.ValueID
	typ                  ssa.TypeID
	typ_name             string
	address              ssa.ValueID
	tuple_types          []string
	is_none              bool
	option_failed        ssa.ValueID
	option_error_type    ssa.ValueID
	option_error_message ssa.ValueID
	option_error_code    ssa.ValueID
	is_temporary         bool
	is_spawned           bool
	spawn_handle         ssa.ValueID
	spawn_context        ssa.ValueID
	spawn_context_type   ssa.TypeID
	spawn_result_type    ssa.TypeID
	spawn_result_name    string
	map_found            ssa.ValueID
	map_id               ssa.ValueID
	map_address          ssa.ValueID
	map_type             string
	map_key_id           ssa.ValueID
	map_key_type         ssa.TypeID
	map_key_name         string
}

struct FastArm64Local {
	addr                 ssa.ValueID
	typ                  ssa.TypeID
	typ_name             string
	option_failed        ssa.ValueID
	option_error_type    ssa.ValueID
	option_error_message ssa.ValueID
	option_error_code    ssa.ValueID
	is_spawned           bool
	spawn_handle         ssa.ValueID
	spawn_context        ssa.ValueID
	spawn_context_type   ssa.TypeID
	spawn_result_type    ssa.TypeID
	spawn_result_name    string
}

struct FastArm64MapLoopWriteback {
	map_value            ssa.ValueID
	state                ssa.ValueID
	iteration_generation ssa.ValueID
	key_address          ssa.ValueID
	value_address        ssa.ValueID
	snapshot_keys_slot   ssa.ValueID
	snapshot_values_slot ssa.ValueID
}

struct FastArm64FieldDecl {
	name           string
	typ            string
	default_source string
	default_path   string
	default_header FastcSourceHeader
}

struct FastArm64ConstantDecl {
	source string
	path   string
	header FastcSourceHeader
}

struct FastArm64InterpolationFormat {
	width     int
	precision int = -1
	specifier u8
	left      bool
	zero_pad  bool
}

struct FastArm64DeclarationAttribute {
	tok        token.Token
	is_enabled bool
	is_packed  bool
	is_aligned bool
	alignment  int
}

struct FastArm64TypeDecl {
	key       string
	c_name    string
	fields    []FastArm64FieldDecl
	embeds    []string
	alias_of  string
	is_union  bool
	is_c      bool
	is_packed bool
	alignment int
}

@[heap]
struct FastArm64Program {
	prefs            &pref.Preferences = unsafe { nil }
	declared_types   map[string]bool
	functions        map[string]FastcFunctionSignature
	type_decls       map[string]FastArm64TypeDecl
	constant_sources map[string]FastArm64ConstantDecl
	enum_values      map[string]FastArm64ConstantDecl
mut:
	m                          &ssa.Module = unsafe { nil }
	void_type                  ssa.TypeID
	i1_type                    ssa.TypeID
	i8_type                    ssa.TypeID
	i16_type                   ssa.TypeID
	i32_type                   ssa.TypeID
	i64_type                   ssa.TypeID
	u8_type                    ssa.TypeID
	u16_type                   ssa.TypeID
	u32_type                   ssa.TypeID
	u64_type                   ssa.TypeID
	f32_type                   ssa.TypeID
	f64_type                   ssa.TypeID
	ptr_i8                     ssa.TypeID
	str_type                   ssa.TypeID
	array_type                 ssa.TypeID
	map_state_type             ssa.TypeID
	map_type                   ssa.TypeID
	type_ids                   map[string]ssa.TypeID
	type_aliases               map[string]string
	fn_ids                     map[string]int
	fn_returns                 map[string]ssa.TypeID
	fn_symbols                 map[string]string
	function_keys_by_name      map[string][]string
	type_decls_by_id           map[int]FastArm64TypeDecl
	native_used_function_names map[string]bool
	module_init_function_keys  []string
	main_argc_global           ssa.ValueID
	main_argv_global           ssa.ValueID
	option_state_type          ssa.TypeID
	option_state_key_global    ssa.ValueID
	spawn_context_types        map[string]ssa.TypeID
	spawn_wrapper_ids          map[string]int
}

struct FastArm64Parser {
mut:
	source_file              FastcSourceFile
	program                  &FastArm64Program = unsafe { nil }
	s                        scanner.Scanner
	tok                      token.Token
	lit                      string
	func_id                  int
	cur_block                ssa.BlockID
	return_typ               ssa.TypeID
	return_name              string
	return_names             []string
	return_is_option         bool
	locals                   map[string]FastArm64Local
	terminated               map[int]bool
	labels                   map[string]ssa.BlockID
	break_to                 []ssa.BlockID
	continue_to              []ssa.BlockID
	break_scopes             []int
	continue_scopes          []int
	map_loop_writebacks      []FastArm64MapLoopWriteback
	defer_sources            []string
	defer_starts             []int
	local_names              []string
	local_values             []FastArm64Local
	local_existed            []bool
	local_starts             []int
	array_element            string
	map_key                  string
	map_value                string
	last_map_found           ssa.ValueID
	current_function         string
	current_receiver         string
	current_method_is_static bool
	parsing_spawn            bool
	// Set while `sizeof` speculatively parses an expression that is thrown away
	// afterwards. Emitting a spawn wrapper here would register a cached, body-less
	// wrapper that discard_emission cannot roll back, so wrappers are suppressed.
	suppress_spawn_wrapper bool
}

struct FastArm64EmissionCheckpoint {
	value_count       int
	instruction_count int
	block_count       int
	cur_block         ssa.BlockID
	last_map_found    ssa.ValueID
	parsing_spawn     bool
mut:
	terminated            map[int]bool
	native_used_functions map[string]bool
}

struct FastArm64Generation {
	source_paths []string
}

// generate_arm64_files parses FastC source tokens directly into SSA and emits a Mach-O binary.
// It deliberately does not create Flat AST nodes or C source.
pub fn generate_arm64_files(paths []string, prefs &pref.Preferences, output string) !FastArm64Generation {
	mut timer := fastc_new_phase_timer()
	input_sources, _ := fastc_resolve_source_files(paths, prefs)!
	timer.mark('arm64.resolve')
	fast_arm64_validate_output_source_paths(output, input_sources)!
	fast_arm64_validate_unsupported_calls(input_sources, prefs)!
	timer.mark('arm64.validate')
	mut sources := fastc_monomorphize_sources(input_sources, prefs)!
	timer.mark('arm64.monomorphize')
	mut declared_types := map[string]bool{}
	mut declared_kinds := map[string]FastcDeclaredTypeKind{}
	mut enum_flags := map[string]bool{}
	mut params_structs := map[string]bool{}
	mut type_source_paths := map[string]bool{}
	mut constants := map[string]string{}
	mut public_constants := map[string]bool{}
	mut globals := map[string]string{}
	mut public_globals := map[string]bool{}
	// The same parallel first pass as the C path: it records the per-file scan
	// flags and function body spans that the declaration and signature passes
	// rely on, and indexes the declarations. The generic-method sources and
	// the per-file declaration texts and spans are not used here: the native
	// path collects its type and constant declarations itself below.
	mut index_type_sources := map[string]string{}
	mut index_constant_sources := map[string]string{}
	mut index_constant_spans := map[string][]int{}
	mut index_global_sources := map[string]string{}
	_ = fastc_collect_generic_and_declaration_indexes(mut sources, prefs, mut declared_types, mut declared_kinds, mut enum_flags, mut params_structs, mut type_source_paths, mut index_type_sources, mut constants, mut public_constants, mut index_constant_sources, mut index_constant_spans, mut index_global_sources, mut globals, mut public_globals)!
	timer.mark('arm64.declaration_indexes')
	// Non-selfhost programs do not resolve builtin source files, but imported module
	// signatures can still use V's intrinsic error interface.
	declared_types['IError'] = true
	declared_types['Error'] = true
	// Library generic definitions remain in the monomorphized source until they are
	// instantiated on demand. Treat their parameters as opaque while indexing the
	// otherwise unreachable signatures in the imported module.
	for source_index, source_file in sources {
		// The first pass flags the files that can hold `fn name[T]` syntax.
		if !source_file.header.has_generic_fn_syntax {
			continue
		}
		for generic in fastc_scan_generic_fns(source_file.source, source_file.path, prefs, source_index) {
			declared_types[generic.type_param] = true
		}
	}
	declared_type_c_names := fastc_declared_type_c_names(declared_types)
	mut functions := map[string]FastcFunctionSignature{}
	mut interface_methods := map[string]bool{}
	mut interface_fields := map[string]FastcInterfaceField{}
	mut embed_embedders := []string{}
	mut embed_embeddeds := []string{}
	fastc_collect_signatures(sources, prefs, declared_types, declared_type_c_names, params_structs, mut functions, mut interface_methods, mut interface_fields, mut embed_embedders, mut embed_embeddeds)!
	timer.mark('arm64.signatures')
	type_decls, constant_sources, enum_values := fast_arm64_collect_declarations(sources, prefs, declared_types, enum_flags, type_source_paths)!
	timer.mark('arm64.declarations')

	mut program := FastArm64Program.new(prefs, declared_types, functions, type_decls, constant_sources, enum_values)
	program.register_functions()
	// The lifecycle hooks run in module dependency order; the resolver returns
	// discovery order.
	lifecycle_sources := fastc_sources_in_dependency_order(sources)!
	module_init_calls := fastc_module_init_calls(lifecycle_sources, functions)!
	module_cleanup_calls := fastc_module_cleanup_calls(lifecycle_sources, functions)!
	module_init_function_keys := fast_arm64_lifecycle_function_keys(module_init_calls, 'init', functions)
	module_cleanup_function_keys := fast_arm64_lifecycle_function_keys(module_cleanup_calls, 'cleanup', functions)
	program.register_module_lifecycle(module_init_function_keys, module_cleanup_function_keys)
	program.register_print_runtime()
	timer.mark('arm64.register')
	reachable_modules := fast_arm64_reachable_modules(sources)
	mut pending_paths := fast_arm64_entry_source_paths(functions)
	for {
		before := fast_arm64_body_count(program.m)
		if pending_paths.len == 0 {
			break
		}
		for source_file in sources {
			// FastC native provides the small builtin ABI below. Exact function reachability
			// lets the remaining builtin helpers be parsed without importing the whole runtime.
			// The SSA module's legacy builder imports the Flat AST and checker. Direct
			// FastC creates SSA while scanning, so none of those function bodies belong
			// in the native compiler.
			if !fast_arm64_uses_source_file(source_file, reachable_modules) {
				continue
			}
			if source_file.path !in pending_paths {
				continue
			}
			mut parser := FastArm64Parser.new(mut program, source_file)
			parser.parse_file()!
		}
		if fast_arm64_body_count(program.m) == before {
			break
		}
		pending_paths = fast_arm64_pending_source_paths(program)
	}
	if 'main' !in program.fn_ids || program.m.funcs[program.fn_ids['main']].blocks.len == 0 {
		return error('fastc arm64 parser did not find a `main` function')
	}
	timer.mark('arm64.parse')
	fast_arm64_hide_unused_prototypes(mut program.m)
	mut gen := arm64.Gen.new(program.m)
	gen.gen()
	timer.mark('arm64.codegen')
	gen.write_and_link(output)
	timer.mark('arm64.link')
	mut source_paths := []string{cap: sources.len}
	for source_file in sources {
		source_paths << source_file.path
	}
	return FastArm64Generation{
		source_paths: source_paths
	}
}

fn fast_arm64_validate_unsupported_calls(sources []FastcSourceFile, prefs &pref.Preferences) ! {
	for source_file in sources {
		if source_file.header.module_name == 'os' {
			continue
		}
		file := token.File.unindexed(source_file.path, source_file.source.len)
		mut scan := scanner.new_scanner(prefs, .normal)
		scan.init(file, source_file.source)
		mut previous_3 := token.Token.unknown
		mut previous_3_literal := ''
		mut previous_2 := token.Token.unknown
		mut previous_2_literal := ''
		mut previous_1 := token.Token.unknown
		mut previous_1_literal := ''
		mut tok := scan.scan()
		for tok != .eof {
			if tok == .lpar && previous_1 == .name && previous_1_literal == 'exec' && previous_2 == .dot && previous_3 == .name {
				module_name := source_file.header.imports[previous_3_literal] or {
					previous_3_literal
				}
				if module_name == 'os' {
					return error('fastc parser does not support `os.exec` on the direct ARM64 backend')
				}
			}
			previous_3 = previous_2
			previous_3_literal = previous_2_literal
			previous_2 = previous_1
			previous_2_literal = previous_1_literal
			previous_1 = tok
			previous_1_literal = if tok == .name { scan.lit } else { '' }
			tok = scan.scan()
		}
	}
}

fn fast_arm64_validate_output_source_paths(output string, sources []FastcSourceFile) ! {
	absolute_output := os.abs_path(output)
	canonical_output := if os.exists(output) {
		os.real_path(output)
	} else {
		os.join_path_single(os.real_path(os.dir(absolute_output)), os.file_name(absolute_output))
	}
	for source in sources {
		if canonical_output == source.path {
			return error('fastc output path `${output}` aliases source `${source.path}`')
		}
	}
}

fn fast_arm64_hide_unused_prototypes(mut m ssa.Module) {
	for mut function in m.funcs {
		if function.is_prototype && function.blocks.len == 0 {
			function.is_c_extern = true
		}
	}
}

fn fast_arm64_entry_source_paths(functions map[string]FastcFunctionSignature) map[string]bool {
	mut paths := map[string]bool{}
	for key, signature in functions {
		if signature.is_disabled || signature.path == '' {
			continue
		}
		for name in ['main', 'init', 'cleanup'] {
			if key == fastc_function_key(signature.module_name, name) {
				paths[signature.path] = true
				break
			}
		}
	}
	return paths
}

fn fast_arm64_lifecycle_function_keys(calls []string, hook_name string, functions map[string]FastcFunctionSignature) []string {
	mut function_keys := []string{cap: calls.len}
	for call in calls {
		for function_key, signature in functions {
			if function_key == fastc_function_key(signature.module_name, hook_name) && call == fastc_c_function_name(signature.module_name, hook_name) {
				function_keys << function_key
				break
			}
		}
	}
	return function_keys
}

fn fast_arm64_pending_source_paths(program &FastArm64Program) map[string]bool {
	mut paths := map[string]bool{}
	for key, _ in program.native_used_function_names {
		if key.starts_with('C.') {
			continue
		}
		if id := program.fn_ids[key] {
			function := program.m.funcs[id]
			if function.blocks.len > 0 || function.is_c_extern {
				continue
			}
		}
		signature := program.functions[key] or { continue }
		if !signature.is_disabled && signature.path != '' {
			paths[signature.path] = true
		}
	}
	return paths
}

fn fast_arm64_body_count(m &ssa.Module) int {
	mut count := 0
	for function in m.funcs {
		if function.blocks.len > 0 {
			count++
		}
	}
	return count
}

fn fast_arm64_reachable_modules(sources []FastcSourceFile) map[string]bool {
	mut reachable := {
		'main': true
	}
	mut changed := true
	for changed {
		changed = false
		for source_file in sources {
			if source_file.header.module_name !in reachable || source_file.path.ends_with('/v3/ssa/builder.v') {
				continue
			}
			mut imports := source_file.header.imports.values()
			imports << source_file.header.blank_imports
			for imported_module in imports {
				if imported_module !in reachable {
					reachable[imported_module] = true
					changed = true
				}
			}
		}
	}
	return reachable
}

fn fast_arm64_uses_source_file(source_file FastcSourceFile, reachable_modules map[string]bool) bool {
	return (source_file.header.module_name == 'builtin' || source_file.header.module_name in reachable_modules) && source_file.header.module_name !in [
		'crypto.sha256',
		'encoding.binary',
	] && !source_file.path.ends_with('/v3/ssa/builder.v')
}

fn fast_arm64_vmod_file(source_path string) !string {
	vmod_file := os.join_path_single(fastc_vmod_root_for_file(source_path), 'v.mod')
	content := os.read_file(vmod_file) or {
		return error('`@VMOD_FILE` can only be used in projects that have a `v.mod` file')
	}
	return content.replace('\r\n', '\n')
}

fn fast_arm64_vmod_hash(source_path string) !string {
	vmod_root := fastc_vmod_root_for_file(source_path)
	if !os.is_file(os.join_path_single(vmod_root, 'v.mod')) {
		return error('`@VMODHASH` can only be used in projects that have a `v.mod` file')
	}
	head_file := os.join_path(vmod_root, '.git', 'HEAD')
	head_content := os.read_file(head_file) or {
		return error('failed to read `${head_file}`')
	}
	mut hash := head_content
	if head_content.starts_with('ref: ') {
		revision_path := os.join_path(vmod_root, '.git', head_content[5..].trim_space())
		hash = os.read_file(revision_path) or {
			return error('failed to read `${revision_path}`')
		}
	}
	return hash[..7] or { error('failed to limit hash `${hash}` to 7 characters') }
}

fn fast_arm64_collect_declarations(sources []FastcSourceFile, prefs &pref.Preferences, declared_types map[string]bool, enum_flags map[string]bool, type_source_paths map[string]bool) !(map[string]FastArm64TypeDecl, map[string]FastArm64ConstantDecl, map[string]FastArm64ConstantDecl) {
	mut declarations := {
		'Error': FastArm64TypeDecl{
			key: 'Error'
			c_name: 'Error'
		}
	}
	mut constants := map[string]FastArm64ConstantDecl{}
	mut enum_values := map[string]FastArm64ConstantDecl{}
	for source_file in sources {
		if source_file.path !in type_source_paths && !source_file.header.has_constants {
			continue
		}
		fast_arm64_collect_source_declarations(source_file, prefs, declared_types, enum_flags, mut declarations, mut constants, mut enum_values)!
	}
	for key, declaration in declarations {
		if declaration.c_name != key || declaration.embeds.len == 0 {
			continue
		}
		mut visiting := map[string]bool{}
		expanded := fast_arm64_expand_embedded_declaration(key, declarations, mut visiting)!
		declarations[key] = expanded
		declarations[declaration.key] = expanded
	}
	return declarations, constants, enum_values
}

fn fast_arm64_expand_embedded_declaration(key string, declarations map[string]FastArm64TypeDecl, mut visiting map[string]bool) !FastArm64TypeDecl {
	declaration := declarations[key] or { return error('unknown embedded declaration `${key}`') }
	if declaration.embeds.len == 0 {
		return declaration
	}
	if visiting[key] {
		return error('recursive embedded declaration `${key}`')
	}
	visiting[key] = true
	mut fields := []FastArm64FieldDecl{}
	for embed in declaration.embeds {
		embed_key := if embed in declarations { embed } else { embed.all_after_last('.') }
		embedded := fast_arm64_expand_embedded_declaration(embed_key, declarations, mut visiting)!
		fields << embedded.fields
	}
	fields << declaration.fields
	visiting.delete(key)
	return FastArm64TypeDecl{
		...declaration
		fields: fields
		embeds: []string{}
	}
}

fn fast_arm64_collect_constant_sources(sources []FastcSourceFile, prefs &pref.Preferences) !map[string]FastArm64ConstantDecl {
	mut constants := map[string]FastArm64ConstantDecl{}
	for source_file in sources {
		if !source_file.header.has_constants {
			continue
		}
		file := token.File.unindexed(source_file.path, source_file.source.len)
		mut scan := scanner.new_scanner(prefs, .normal)
		scan.init(file, source_file.source)
		mut tok := scan.scan()
		mut depth := 0
		for tok != .eof {
			if depth == 0 && tok == .key_const {
				tok = fast_arm64_collect_constant_declaration(mut scan, source_file, mut constants)!
				continue
			}
			if tok == .lcbr {
				depth++
			} else if tok == .rcbr && depth > 0 {
				depth--
			}
			tok = scan.scan()
		}
	}
	return constants
}

fn fast_arm64_collect_enum_values(sources []FastcSourceFile, prefs &pref.Preferences, type_source_paths map[string]bool) !map[string]FastArm64ConstantDecl {
	mut values := map[string]FastArm64ConstantDecl{}
	for source_file in sources {
		if source_file.path !in type_source_paths {
			continue
		}
		file := token.File.unindexed(source_file.path, source_file.source.len)
		mut scan := scanner.new_scanner(prefs, .normal)
		scan.init(file, source_file.source)
		mut tok := scan.scan()
		mut depth := 0
		for tok != .eof {
			if depth == 0 && tok == .key_enum {
				tok = scan.scan()
				if tok != .name {
					return error('fastc arm64 expected an enum name in ${source_file.path}')
				}
				enum_name := scan.lit
				tok = scan.scan()
				for tok !in [.lcbr, .semicolon, .eof] {
					tok = scan.scan()
				}
				if tok != .lcbr {
					return error('fastc arm64 expected `${enum_name}` enum body in ${source_file.path}')
				}
				tok = scan.scan()
				mut ordinal := i64(0)
				for tok !in [.rcbr, .eof] {
					if tok == .semicolon {
						tok = scan.scan()
						continue
					}
					if tok == .attribute {
						attribute := fastc_scan_declaration_attribute(mut scan, source_file.path, prefs)!
						tok = attribute.tok
						continue
					}
					if tok != .name && !tok.is_keyword() {
						return error('fastc arm64 expected an enum field in ${source_file.path}')
					}
					field := scan.lit
					tok = scan.scan()
					mut value := ordinal.str()
					if tok == .assign {
						tok = scan.scan()
						start := scan.pos
						value, tok = fast_arm64_constant_expression_source(mut scan, tok, start, false)!
					}
					module_prefix := if source_file.header.module_name in ['', 'main'] {
						''
					} else {
						'${source_file.header.module_name}.'
					}
					declaration := FastArm64ConstantDecl{
						source: value
						path: source_file.path
						header: source_file.header
					}
					values['${module_prefix}${enum_name}.${field}'] = declaration
					values['${enum_name}.${field}'] = declaration
					ordinal++
				}
				if tok == .rcbr {
					tok = scan.scan()
				}
				continue
			}
			if tok == .lcbr {
				depth++
			} else if tok == .rcbr && depth > 0 {
				depth--
			}
			tok = scan.scan()
		}
	}
	return values
}

fn fast_arm64_collect_constant_declaration(mut scan scanner.Scanner, source_file FastcSourceFile, mut constants map[string]FastArm64ConstantDecl) !token.Token {
	mut tok := scan.scan()
	if tok == .lpar {
		tok = scan.scan()
		for tok !in [.rpar, .eof] {
			if tok == .semicolon {
				tok = scan.scan()
				continue
			}
			if tok != .name {
				return error('fastc arm64 expected a constant name in ${source_file.path}')
			}
			name := scan.lit
			tok = scan.scan()
			if name == 'C' && tok == .dot {
				for tok !in [.semicolon, .rpar, .eof] {
					tok = scan.scan()
				}
				continue
			}
			for tok !in [.assign, .semicolon, .rpar, .eof] {
				tok = scan.scan()
			}
			if tok != .assign {
				return error('fastc arm64 expected `=` after constant `${name}` in ${source_file.path}')
			}
			tok = scan.scan()
			expression_start := scan.pos
			expression, next_token := fast_arm64_constant_expression_source(mut scan, tok, expression_start, true)!
			constants[fastc_constant_key(source_file.header.module_name, name)] = FastArm64ConstantDecl{
				source: expression
				path: source_file.path
				header: source_file.header
			}
			tok = next_token
		}
		if tok == .rpar {
			return scan.scan()
		}
		return tok
	}
	if tok != .name {
		return error('fastc arm64 expected a constant name in ${source_file.path}')
	}
	name := scan.lit
	tok = scan.scan()
	if name == 'C' && tok == .dot {
		for tok !in [.semicolon, .eof] {
			tok = scan.scan()
		}
		return if tok == .semicolon { scan.scan() } else { tok }
	}
	for tok !in [.assign, .semicolon, .eof] {
		tok = scan.scan()
	}
	if tok != .assign {
		return error('fastc arm64 expected `=` after constant `${name}` in ${source_file.path}')
	}
	tok = scan.scan()
	expression_start := scan.pos
	expression, next_token := fast_arm64_constant_expression_source(mut scan, tok, expression_start, false)!
	constants[fastc_constant_key(source_file.header.module_name, name)] = FastArm64ConstantDecl{
		source: expression
		path: source_file.path
		header: source_file.header
	}
	return next_token
}

fn fast_arm64_constant_expression_source(mut scan scanner.Scanner, first_token token.Token, start int, stop_at_rpar bool) !(string, token.Token) {
	mut tok := first_token
	mut depth := 0
	for tok != .eof {
		if depth == 0 && (tok == .semicolon || (stop_at_rpar && tok == .rpar)) {
			return scan.src[start..scan.pos].trim_space(), tok
		}
		if tok in [.lpar, .lsbr, .lcbr] {
			depth++
		} else if tok in [.rpar, .rsbr, .rcbr] && depth > 0 {
			depth--
		}
		tok = scan.scan()
	}
	return scan.src[start..scan.src.len].trim_space(), tok
}

fn fast_arm64_scan_layout_attribute(mut scan scanner.Scanner, path string, prefs &pref.Preferences) !FastArm64DeclarationAttribute {
	mut tok := scan.scan()
	mut depth := 1
	mut is_enabled := true
	mut is_packed := false
	mut is_aligned := false
	mut alignment := 0
	mut reading_alignment := false
	mut at_item_start := true
	for depth > 0 {
		if tok == .eof {
			return error('fastc arm64 does not support unfinished declaration attribute in ${path}')
		}
		if depth == 1 && at_item_start && tok == .key_if {
			condition := fastc_scan_comptime_or(mut scan, scan.scan(), path, prefs)!
			is_enabled = is_enabled && condition.value
			tok = condition.tok
			if tok !in [.semicolon, .rsbr] {
				return error('fastc arm64 does not support conditional attribute expression in ${path}')
			}
			continue
		}
		if depth == 1 && at_item_start && tok == .name && scan.lit == 'packed' {
			is_packed = true
		}
		if depth == 1 && at_item_start && tok == .name && scan.lit == 'aligned' {
			is_aligned = true
			reading_alignment = true
		} else if depth == 1 && reading_alignment && tok == .number {
			alignment = scan.lit.int()
			reading_alignment = false
		}
		if depth == 1 && tok == .semicolon {
			at_item_start = true
			reading_alignment = false
		} else if depth == 1 {
			at_item_start = false
		}
		if tok == .lsbr {
			depth++
		} else if tok == .rsbr {
			depth--
		}
		tok = scan.scan()
	}
	return FastArm64DeclarationAttribute{
		tok: tok
		is_enabled: is_enabled
		is_packed: is_packed
		is_aligned: is_aligned
		alignment: alignment
	}
}

fn fast_arm64_collect_source_declarations(source_file FastcSourceFile, prefs &pref.Preferences, declared_types map[string]bool, enum_flags map[string]bool, mut declarations map[string]FastArm64TypeDecl, mut constants map[string]FastArm64ConstantDecl, mut enum_values map[string]FastArm64ConstantDecl) ! {
	file := token.File.unindexed(source_file.path, source_file.source.len)
	mut scan := scanner.new_scanner(prefs, .normal)
	scan.init(file, source_file.source)
	mut tok := scan.scan()
	mut depth := 0
	mut next_struct_is_packed := false
	mut next_struct_alignment := 0
	for tok != .eof {
		if depth == 0 && tok == .dollar {
			mut lookahead := scan
			if lookahead.scan() == .key_if {
				selected := fastc_scan_selected_comptime_branch(mut scan, scan.scan(), source_file.path, prefs)!
				if selected.source != '' {
					fast_arm64_collect_source_declarations(FastcSourceFile{
						path: source_file.path
						source: selected.source
						header: source_file.header
					}, prefs, declared_types, enum_flags, mut declarations, mut constants, mut enum_values)!
				}
				tok = selected.tok
				continue
			}
		}
		if depth == 0 && tok == .attribute {
			attribute := fast_arm64_scan_layout_attribute(mut scan, source_file.path, prefs)!
			if attribute.is_enabled {
				next_struct_is_packed = next_struct_is_packed || attribute.is_packed
				if attribute.is_aligned {
					next_struct_alignment = if attribute.alignment > 0 {
						attribute.alignment
					} else {
						16
					}
					if next_struct_alignment & (next_struct_alignment - 1) != 0 {
						return error('fastc arm64 struct alignment must be a power of two in ${source_file.path}')
					}
				}
			}
			tok = attribute.tok
			continue
		}
		if depth == 0 && tok in [.key_pub, .key_static] {
			tok = scan.scan()
			continue
		}
		if depth == 0 && tok == .key_const {
			next_struct_is_packed = false
			next_struct_alignment = 0
			tok = fast_arm64_collect_constant_declaration(mut scan, source_file, mut constants)!
			continue
		}
		if depth == 0 && tok in [.key_struct, .key_union] {
			is_union := tok == .key_union
			tok = scan.scan()
			if tok != .name {
				return error('fastc arm64 expected a type name in ${source_file.path}')
			}
			mut short_name := scan.lit
			tok = scan.scan()
			mut is_c := false
			if short_name == 'C' && tok == .dot {
				tok = scan.scan()
				if tok != .name {
					return error('fastc arm64 expected a C type name in ${source_file.path}')
				}
				short_name = 'C.${scan.lit}'
				is_c = true
				tok = scan.scan()
			}
			key := if is_c {
				short_name
			} else {
				fastc_type_key(source_file.header.module_name, short_name)
			}
			c_name := if is_c { key } else { fastc_c_declared_type_name(key) }
			for tok !in [.lcbr, .eof] {
				tok = scan.scan()
			}
			if tok == .eof {
				return error('fastc arm64 unfinished type `${short_name}` in ${source_file.path}')
			}
			tok = scan.scan()
			mut fields := []FastArm64FieldDecl{}
			mut embeds := []string{}
			mut field_depth := 1
			for field_depth > 0 && tok != .eof {
				if tok == .attribute {
					attribute := fastc_scan_declaration_attribute(mut scan, source_file.path, prefs)!
					tok = attribute.tok
					continue
				}
				if tok == .lcbr {
					field_depth++
					tok = scan.scan()
					continue
				}
				if tok == .rcbr {
					field_depth--
					tok = scan.scan()
					continue
				}
				if field_depth != 1 || tok in [.semicolon, .key_pub, .key_mut, .key_global] {
					tok = scan.scan()
					if tok == .colon {
						tok = scan.scan()
					}
					continue
				}
				if tok != .name && !tok.is_keyword() {
					tok = scan.scan()
					continue
				}
				field_name := scan.lit
				first_type_token := scan.scan()
				if first_type_token in [.semicolon, .rcbr] {
					embeds << fastc_type_key(source_file.header.module_name, field_name)
					tok = first_type_token
					continue
				}
				field_type, next_token := fastc_scan_type(mut scan, first_type_token, source_file.path, source_file.header.module_name, source_file.header.imports, declared_types, prefs.building_v) or {
					return error('fastc arm64 field `${field_name}`: ${err.msg()}')
				}
				tok = next_token
				mut default_source := ''
				if tok == .assign {
					tok = scan.scan()
					expression_start := scan.pos
					default_source, tok = fast_arm64_constant_expression_source(mut scan, tok, expression_start, false)!
				}
				fields << FastArm64FieldDecl{
					name: field_name
					typ: field_type
					default_source: default_source
					default_path: source_file.path
					default_header: source_file.header
				}
			}
			declaration := FastArm64TypeDecl{
				key: key
				c_name: c_name
				fields: fields
				embeds: embeds
				is_union: is_union
				is_c: is_c
				is_packed: next_struct_is_packed
				alignment: next_struct_alignment
			}
			declarations[key] = declaration
			declarations[c_name] = declaration
			next_struct_is_packed = false
			next_struct_alignment = 0
			continue
		}
		if depth == 0 && tok == .key_enum {
			next_struct_is_packed = false
			next_struct_alignment = 0
			tok = scan.scan()
			if tok != .name {
				return error('fastc arm64 expected an enum name in ${source_file.path}')
			}
			short_name := scan.lit
			key := fastc_type_key(source_file.header.module_name, short_name)
			c_name := fastc_c_declared_type_name(key)
			is_flag := enum_flags[key]
			mut underlying := if is_flag { 'u64' } else { 'int' }
			tok = scan.scan()
			if tok == .key_as {
				tok = scan.scan()
				underlying, tok = fastc_scan_type(mut scan, tok, source_file.path, source_file.header.module_name, source_file.header.imports, declared_types, prefs.building_v) or {
					return error('fastc arm64 enum `${short_name}`: ${err.msg()}')
				}
			}
			for tok !in [.lcbr, .eof] {
				tok = scan.scan()
			}
			if tok == .lcbr {
				tok = scan.scan()
				mut ordinal := i64(0)
				for tok !in [.rcbr, .eof] {
					if tok == .semicolon {
						tok = scan.scan()
						continue
					}
					if tok == .attribute {
						attribute := fastc_scan_declaration_attribute(mut scan, source_file.path, prefs)!
						tok = attribute.tok
						continue
					}
					if tok != .name && !tok.is_keyword() {
						return error('fastc arm64 expected an enum field in ${source_file.path}')
					}
					field := scan.lit
					tok = scan.scan()
					mut value := if is_flag {
						(u64(1) << u64(ordinal)).str()
					} else {
						ordinal.str()
					}
					if tok == .assign {
						if is_flag {
							return error('fastc arm64 does not support custom value for flag enum field `${short_name}.${field}` in ${source_file.path}')
						}
						tok = scan.scan()
						start := scan.pos
						value, tok = fast_arm64_constant_expression_source(mut scan, tok, start, false)!
					}
					module_prefix := if source_file.header.module_name in ['', 'main'] {
						''
					} else {
						'${source_file.header.module_name}.'
					}
					declaration := FastArm64ConstantDecl{
						source: value
						path: source_file.path
						header: source_file.header
					}
					enum_values['${module_prefix}${short_name}.${field}'] = declaration
					enum_values['${short_name}.${field}'] = declaration
					ordinal++
				}
				if tok == .rcbr {
					tok = scan.scan()
				}
			}
			declaration := FastArm64TypeDecl{
				key: key
				c_name: c_name
				alias_of: underlying
			}
			declarations[key] = declaration
			declarations[c_name] = declaration
			continue
		}
		if depth == 0 && tok == .key_type {
			next_struct_is_packed = false
			next_struct_alignment = 0
			tok = scan.scan()
			if tok != .name {
				return error('fastc arm64 expected an alias name in ${source_file.path}')
			}
			short_name := scan.lit
			key := fastc_type_key(source_file.header.module_name, short_name)
			c_name := fastc_c_declared_type_name(key)
			tok = scan.scan()
			if tok == .assign {
				tok = scan.scan()
				alias_type, next_token := fastc_scan_type(mut scan, tok, source_file.path, source_file.header.module_name, source_file.header.imports, declared_types, prefs.building_v) or {
					tok = scan.scan()
					continue
				}
				declaration := FastArm64TypeDecl{
					key: key
					c_name: c_name
					alias_of: alias_type
				}
				declarations[key] = declaration
				declarations[c_name] = declaration
				tok = next_token
				continue
			}
		}
		if tok == .lcbr {
			depth++
		} else if tok == .rcbr && depth > 0 {
			depth--
		}
		if depth == 0 && tok != .semicolon {
			next_struct_is_packed = false
			next_struct_alignment = 0
		}
		tok = scan.scan()
	}
}

fn (mut p FastArm64Program) register_declared_types() {
	for name, declaration in p.type_decls {
		if declaration.alias_of != '' {
			p.type_aliases[name] = declaration.alias_of
			p.type_aliases[declaration.key.all_after_last('.')] = declaration.alias_of
			continue
		}
		if declaration.c_name != name || name in p.type_ids {
			continue
		}
		id := p.m.type_store.register(ssa.Type{
			kind: .struct_t
			is_union: declaration.is_union
			is_packed: declaration.is_packed
			alignment: declaration.alignment
		})
		p.type_ids[declaration.key] = id
		p.type_ids[declaration.c_name] = id
		if !declaration.is_c {
			p.type_ids[declaration.key.all_after_last('.')] = id
		}
	}
	for name, declaration in p.type_decls {
		if declaration.alias_of != '' || declaration.c_name != name {
			continue
		}
		id := p.type_ids[name] or { continue }
		mut fields := []ssa.TypeID{cap: declaration.fields.len}
		mut field_names := []string{cap: declaration.fields.len}
		for field in declaration.fields {
			fields << p.type_id(field.typ)
			field_names << field.name
		}
		p.m.type_store.types[id] = ssa.Type{
			kind: .struct_t
			fields: fields
			field_names: field_names
			is_union: declaration.is_union
			is_c_struct: declaration.is_c
			is_packed: declaration.is_packed
			alignment: declaration.alignment
		}
		p.type_decls_by_id[int(id)] = declaration
	}
}

fn FastArm64Program.new(prefs &pref.Preferences, declared_types map[string]bool, functions map[string]FastcFunctionSignature, type_decls map[string]FastArm64TypeDecl, constant_sources map[string]FastArm64ConstantDecl, enum_values map[string]FastArm64ConstantDecl) &FastArm64Program {
	mut m := ssa.Module.new()
	// FastC hands this SSA directly to the ARM64 emitter. It does not run SSA
	// rewrites, so maintaining a unique user list for every operand is pure cost.
	m.track_uses = false
	mut program := &FastArm64Program{
		prefs: unsafe { prefs }
		declared_types: declared_types
		functions: functions
		type_decls: type_decls
		constant_sources: constant_sources
		enum_values: enum_values
		m: m
		fn_ids: map[string]int{}
		fn_returns: map[string]ssa.TypeID{}
		fn_symbols: map[string]string{}
		function_keys_by_name: map[string][]string{}
		type_decls_by_id: map[int]FastArm64TypeDecl{}
		type_ids: map[string]ssa.TypeID{}
		type_aliases: map[string]string{}
		native_used_function_names: map[string]bool{}
		spawn_context_types: map[string]ssa.TypeID{}
		spawn_wrapper_ids: map[string]int{}
	}
	program.void_type = ssa.TypeID(0)
	program.i1_type = m.type_store.get_int(1)
	program.i8_type = m.type_store.get_int(8)
	program.i16_type = m.type_store.get_int(16)
	program.i32_type = m.type_store.get_int(32)
	program.i64_type = m.type_store.get_int(64)
	program.u8_type = m.type_store.get_uint(8)
	program.u16_type = m.type_store.get_uint(16)
	program.u32_type = m.type_store.get_uint(32)
	program.u64_type = m.type_store.get_uint(64)
	program.f32_type = m.type_store.get_float(32)
	program.f64_type = m.type_store.get_float(64)
	program.ptr_i8 = m.type_store.get_ptr(program.i8_type)
	program.str_type = m.type_store.register(ssa.Type{
		kind: .struct_t
		fields: [program.ptr_i8, program.i32_type, program.i32_type]
		field_names: ['str', 'len', 'is_lit']
	})
	program.array_type = m.type_store.register(ssa.Type{
		kind: .struct_t
		fields: [program.ptr_i8, program.i32_type, program.i32_type, program.i32_type,
			program.i32_type, program.i32_type]
		field_names: ['data', 'offset', 'len', 'cap', 'flags', 'element_size']
	})
	program.map_state_type = m.type_store.register(ssa.Type{
		kind: .struct_t
		fields: [program.ptr_i8, program.ptr_i8, program.i64_type, program.i64_type,
			program.i64_type, program.i64_type, program.i64_type, program.ptr_i8, program.ptr_i8,
			program.i64_type, program.ptr_i8, program.i64_type]
		field_names: ['keys', 'vals', 'cap', 'len', 'key_size', 'val_size', 'string_key', 'buckets',
			'next', 'bucket_cap', 'zero_value', 'generation']
	})
	program.map_type = m.type_store.register(ssa.Type{
		kind: .struct_t
		fields: [m.type_store.get_ptr(program.map_state_type)]
		field_names: ['state']
	})
	program.register_declared_types()
	return program
}

fn (mut p FastArm64Program) type_id(name string) ssa.TypeID {
	// Type spellings come from FastC's scanner and declaration index, which already
	// remove surrounding trivia. Keeping this path allocation-free matters during
	// self-hosting, where it is called for every parameter and field.
	clean := name
	if clean.ends_with('*') {
		return p.m.type_store.get_ptr(p.type_id(clean.trim_right('*')))
	}
	if clean.starts_with('struct ') {
		return p.type_id('C.${clean['struct '.len..]}')
	}
	if id := p.type_ids['C.${clean}'] {
		return id
	}
	if element_type := fastc_fixed_array_element_type(clean) {
		if id := p.type_ids[clean] {
			return id
		}
		length_source := fastc_fixed_array_length(clean) or { return p.i64_type }
		length := fastc_decimal_integer_value(length_source) or { return p.i64_type }
		id := p.m.type_store.get_array(p.type_id(element_type), length)
		p.type_ids[clean] = id
		return id
	}
	if clean !in ['bool', 'i8', 'char', 'i16', 'int', 'i32', 'rune', 'i64', 'isize', 'int_literal',
		'u8', 'byte', 'u16', 'u32', 'unsigned int', 'u64', 'usize', 'f32', 'f64', 'float_literal',
		'string', 'voidptr', 'byteptr', 'charptr'] {
		if alias := p.type_aliases[clean] {
			return p.type_id(alias)
		}
		if id := p.type_ids[clean] {
			return id
		}
	}
	return match clean {
		'', 'void' { p.void_type }
		'bool' { p.i1_type }
		'i8', 'char' { p.i8_type }
		'i16' { p.i16_type }
		'int', 'i32', 'rune' { p.i32_type }
		'i64', 'isize', 'int_literal' { p.i64_type }
		'u8', 'byte' { p.u8_type }
		'u16' { p.u16_type }
		'u32', 'unsigned int' { p.u32_type }
		'u64', 'usize' { p.u64_type }
		'f32' { p.f32_type }
		'f64', 'float_literal' { p.f64_type }
		'string' { p.str_type }
		'voidptr', 'byteptr', 'charptr' { p.ptr_i8 }
		else {
			if clean.starts_with('Array_') || clean.starts_with('[]') {
				p.array_type
			} else if clean.starts_with('Map_') || clean.starts_with('map[') {
				p.map_type
			} else {
				p.i64_type
			}
		}
	}
}

fn (mut p FastArm64Program) register_function(key string, symbol string, ret ssa.TypeID, is_extern bool) int {
	if id := p.fn_ids[key] {
		return id
	}
	id := p.m.new_function(symbol, ret)
	p.fn_ids[key] = id
	p.fn_returns[key] = ret
	p.fn_symbols[key] = symbol
	mut function := p.m.funcs[id]
	function.is_prototype = true
	function.is_c_extern = is_extern
	if is_extern {
		function.linkage = .external
	}
	p.m.funcs[id] = function
	return id
}

fn (mut p FastArm64Program) register_signature_function(key string) ?int {
	if id := p.fn_ids[key] {
		return id
	}
	signature := p.functions[key] or { return none }
	if signature.is_disabled {
		return none
	}
	effective_return_type := if signature.return_type == 'Option' {
		signature.option_type
	} else {
		signature.return_type
	}
	mut ret := p.type_id(effective_return_type)
	if signature.return_types.len > 0 && (signature.return_type != 'Option' || signature.option_type == 'MultiReturn') {
		mut return_types := []ssa.TypeID{cap: signature.return_types.len}
		for return_type in signature.return_types {
			return_types << p.type_id(return_type)
		}
		ret = p.m.type_store.get_tuple(return_types)
	}
	if key.starts_with('C.') {
		symbol := key['C.'.len..]
		id := p.register_function(key, symbol, ret, true)
		if signature.is_variadic {
			mut function := p.m.funcs[id]
			function.is_variadic = true
			function.variadic_start = fast_arm64_c_variadic_fixed_parameter_count(signature)
			p.m.funcs[id] = function
		}
		p.fn_ids[symbol] = id
		p.fn_returns[symbol] = ret
		p.fn_symbols[symbol] = symbol
		return id
	}
	symbol := fastc_c_function_name_for_key(key)
	return p.register_function(key, symbol, ret, false)
}

fn (mut p FastArm64Program) register_functions() {
	for key, signature in p.functions {
		if signature.is_disabled {
			continue
		}
		name := key.all_after_last('.')
		mut keys := p.function_keys_by_name[name] or { []string{} }
		keys << key
		p.function_keys_by_name[name] = keys
	}
	// Runtime functions used by the direct parser do not depend on builtin bodies.
	p.register_function('write', 'write', p.i64_type, true)
	p.register_function('exit', 'exit', p.void_type, true)
	p.register_function('malloc', 'malloc', p.ptr_i8, true)
	p.register_function('calloc', 'calloc', p.ptr_i8, true)
	p.register_function('realloc', 'realloc', p.ptr_i8, true)
	p.register_function('free', 'free', p.void_type, true)
	p.register_function('strlen', 'strlen', p.i64_type, true)
	p.register_function('memcmp', 'memcmp', p.i32_type, true)
	p.register_function('memcpy', 'memcpy', p.ptr_i8, true)
	p.register_function('memset', 'memset', p.ptr_i8, true)
	p.register_function('memmove', 'memmove', p.ptr_i8, true)
	p.register_function('system', 'system', p.i32_type, true)
	p.register_function('popen', 'popen', p.ptr_i8, true)
	p.register_function('pclose', 'pclose', p.i32_type, true)
	p.register_function('fread', 'fread', p.u64_type, true)
	p.register_function('atexit', 'atexit', p.i32_type, true)
	p.register_function('pthread_attr_init', 'pthread_attr_init', p.i32_type, true)
	p.register_function('pthread_attr_setstacksize', 'pthread_attr_setstacksize', p.i32_type, true)
	p.register_function('pthread_attr_destroy', 'pthread_attr_destroy', p.i32_type, true)
	p.register_function('pthread_create', 'pthread_create', p.i32_type, true)
	p.register_function('pthread_join', 'pthread_join', p.i32_type, true)
	p.register_function('pthread_key_create', 'pthread_key_create', p.i32_type, true)
	p.register_function('pthread_getspecific', 'pthread_getspecific', p.ptr_i8, true)
	p.register_function('pthread_setspecific', 'pthread_setspecific', p.i32_type, true)
	p.register_function('C.pthread_mutex_init', 'pthread_mutex_init', p.i32_type, true)
	p.register_function('C.pthread_mutex_lock', 'pthread_mutex_lock', p.i32_type, true)
	p.register_function('C.pthread_mutex_unlock', 'pthread_mutex_unlock', p.i32_type, true)
	p.register_function('getcwd', 'getcwd', p.ptr_i8, true)
	p.register_function('gcvt', 'gcvt', p.ptr_i8, true)
	p.register_function('ecvt', 'ecvt', p.ptr_i8, true)
	p.register_function('__error', '__error', p.m.type_store.get_ptr(p.i32_type), true)
	p.main_argc_global = p.m.add_global('g_main_argc', p.i64_type)
	p.main_argv_global = p.m.add_global('g_main_argv', p.m.type_store.get_ptr(p.ptr_i8))
	p.option_state_type = p.m.type_store.get_tuple([p.i1_type, p.u64_type, p.i32_type, p.str_type])
	p.option_state_key_global = p.m.add_global('g_fastc_option_state_key', p.u64_type)
	p.register_option_state_runtime()
	p.register_fast_path_normalize_runtime()
	p.register_os_path_runtime()
	p.register_os_abs_path_runtime()
	p.register_os_process_runtime()
	p.register_fastc_vmod_root_runtime()
	p.register_pointer_predicate_runtime()
	p.register_memory_wrapper_runtime()
	p.register_cleanup_runtime()
	p.register_string_conversion_runtime()
	p.register_preferences_runtime()
	p.register_array_new_runtime()
	p.register_array_buffer_runtime()
	p.register_arguments_runtime()
	p.register_map_runtime()
	p.register_integer_string_runtime()
	p.register_integer_format_runtime()
	p.register_bool_string_runtime()
	p.register_character_string_runtime()
	p.register_string_padding_runtime()
	p.register_string_zero_extension_runtime()
	p.register_fixed_float_string_runtime()
	p.register_scientific_float_string_runtime()
	p.register_float_string_runtime()
	p.register_integer_str_wrappers()
	p.register_string_sort_runtime()
}

fn (mut p FastArm64Program) register_module_lifecycle(init_function_keys []string, cleanup_function_keys []string) {
	p.module_init_function_keys = init_function_keys.clone()
	for function_key in init_function_keys {
		p.register_signature_function(function_key) or { continue }
		p.native_used_function_names[function_key] = true
	}
	if cleanup_function_keys.len == 0 {
		return
	}
	cleanup_id := p.register_function('v_fastc_cleanup_modules', 'v_fastc_cleanup_modules', p.void_type, false)
	entry := p.m.add_block(cleanup_id, 'cleanup_modules_entry')
	for function_key in cleanup_function_keys {
		function_id := p.register_signature_function(function_key) or { continue }
		p.native_used_function_names[function_key] = true
		function_ref := p.m.add_value(.func_ref, p.fn_returns[function_key], p.fn_symbols[function_key], function_id)
		p.m.add_instr(.call, entry, p.fn_returns[function_key], [function_ref])
	}
	p.instr0(.ret, entry, p.void_type)
}

fn (mut p FastArm64Program) register_fast_path_normalize_runtime() {
	id := p.register_function('fast_path_normalize', 'fast_path_normalize', p.str_type, false)
	entry := p.m.add_block(id, 'path_norm_entry')
	check_root := p.m.add_block(id, 'path_norm_check_root')
	rooted_setup := p.m.add_block(id, 'path_norm_rooted_setup')
	separator_condition := p.m.add_block(id, 'path_norm_separator_condition')
	separator_body := p.m.add_block(id, 'path_norm_separator_body')
	component_start := p.m.add_block(id, 'path_norm_component_start')
	component_condition := p.m.add_block(id, 'path_norm_component_condition')
	component_body := p.m.add_block(id, 'path_norm_component_body')
	component_done := p.m.add_block(id, 'path_norm_component_done')
	dot_policy := p.m.add_block(id, 'path_norm_dot_policy')
	check_dotdot := p.m.add_block(id, 'path_norm_check_dotdot')
	dotdot_second := p.m.add_block(id, 'path_norm_dotdot_second')
	dotdot_policy := p.m.add_block(id, 'path_norm_dotdot_policy')
	dotdot_handle := p.m.add_block(id, 'path_norm_dotdot_handle')
	dotdot_top := p.m.add_block(id, 'path_norm_dotdot_top')
	dotdot_pop := p.m.add_block(id, 'path_norm_dotdot_pop')
	dotdot_empty := p.m.add_block(id, 'path_norm_dotdot_empty')
	set_normal_append := p.m.add_block(id, 'path_norm_set_normal_append')
	set_dotdot_append := p.m.add_block(id, 'path_norm_set_dotdot_append')
	append_prepare := p.m.add_block(id, 'path_norm_append_prepare')
	append_separator := p.m.add_block(id, 'path_norm_append_separator')
	copy_component := p.m.add_block(id, 'path_norm_copy_component')
	done := p.m.add_block(id, 'path_norm_done')
	emit_dot := p.m.add_block(id, 'path_norm_emit_dot')
	finish := p.m.add_block(id, 'path_norm_finish')
	path := p.add_arg(id, p.str_type, 'path')
	resolve_backlinks := p.add_arg(id, p.i1_type, 'resolve_backlinks')
	preserve_final_dot := p.add_arg(id, p.i1_type, 'preserve_final_dot')
	normalize_backslash := p.add_arg(id, p.i1_type, 'normalize_backslash')
	path_slot := p.instr0(.alloca, entry, p.m.type_store.get_ptr(p.str_type))
	p.instr2(.store, entry, p.void_type, path, path_slot)
	data := p.instr1(.load, entry, p.ptr_i8, p.string_field_ptr(entry, path_slot, 0))
	length32 := p.instr1(.load, entry, p.i32_type, p.string_field_ptr(entry, path_slot, 1))
	length := p.instr1(.zext, entry, p.i64_type, length32)
	zero64 := p.m.get_or_add_const(p.i64_type, '0')
	one64 := p.m.get_or_add_const(p.i64_type, '1')
	two64 := p.m.get_or_add_const(p.i64_type, '2')
	eight64 := p.m.get_or_add_const(p.i64_type, '8')
	allocation_size := p.instr2(.add, entry, p.i64_type, length, two64)
	calloc_ref := p.m.add_value(.func_ref, p.ptr_i8, 'calloc', p.fn_ids['calloc'])
	output := p.m.add_instr(.call, entry, p.ptr_i8, [calloc_ref, one64, allocation_size])
	stack_bytes := p.instr2(.mul, entry, p.i64_type, allocation_size, eight64)
	component_starts := p.m.add_instr(.call, entry, p.ptr_i8, [calloc_ref, one64, stack_bytes])
	component_kinds := p.m.add_instr(.call, entry, p.ptr_i8, [calloc_ref, one64, allocation_size])
	ptr_i64 := p.m.type_store.get_ptr(p.i64_type)
	ptr_i1 := p.m.type_store.get_ptr(p.i1_type)
	read_slot := p.instr0(.alloca, entry, ptr_i64)
	write_slot := p.instr0(.alloca, entry, ptr_i64)
	depth_slot := p.instr0(.alloca, entry, ptr_i64)
	component_slot := p.instr0(.alloca, entry, ptr_i64)
	rooted_slot := p.instr0(.alloca, entry, ptr_i1)
	append_dotdot_slot := p.instr0(.alloca, entry, ptr_i1)
	false_value := p.m.get_or_add_const(p.i1_type, '0')
	p.instr2(.store, entry, p.void_type, zero64, read_slot)
	p.instr2(.store, entry, p.void_type, zero64, write_slot)
	p.instr2(.store, entry, p.void_type, zero64, depth_slot)
	p.instr2(.store, entry, p.void_type, false_value, rooted_slot)
	has_data := p.instr2(.gt, entry, p.i1_type, length, zero64)
	p.instr3(.br, entry, p.void_type, has_data, ssa.ValueID(check_root), ssa.ValueID(done))
	slash := p.m.get_or_add_const(p.u8_type, '47')
	backslash := p.m.get_or_add_const(p.u8_type, '92')
	first := p.instr1(.load, check_root, p.u8_type, data)
	first_is_slash := p.instr2(.eq, check_root, p.i1_type, first, slash)
	first_is_backslash := p.instr2(.eq, check_root, p.i1_type, first, backslash)
	first_is_normalized_backslash := p.instr2(.and_, check_root, p.i1_type, normalize_backslash, first_is_backslash)
	is_rooted := p.instr2(.or_, check_root, p.i1_type, first_is_slash, first_is_normalized_backslash)
	p.instr3(.br, check_root, p.void_type, is_rooted, ssa.ValueID(rooted_setup), ssa.ValueID(separator_condition))
	true_value := p.m.get_or_add_const(p.i1_type, '1')
	p.instr2(.store, rooted_setup, p.void_type, true_value, rooted_slot)
	p.instr2(.store, rooted_setup, p.void_type, one64, read_slot)
	p.instr2(.store, rooted_setup, p.void_type, one64, write_slot)
	p.instr2(.store, rooted_setup, p.void_type, slash, output)
	p.instr1(.jmp, rooted_setup, p.void_type, ssa.ValueID(separator_condition))
	read_index := p.instr1(.load, separator_condition, p.i64_type, read_slot)
	has_input := p.instr2(.lt, separator_condition, p.i1_type, read_index, length)
	p.instr3(.br, separator_condition, p.void_type, has_input, ssa.ValueID(separator_body), ssa.ValueID(done))
	separator_address := p.instr2(.add, separator_body, p.ptr_i8, data, read_index)
	separator := p.instr1(.load, separator_body, p.u8_type, separator_address)
	is_slash := p.instr2(.eq, separator_body, p.i1_type, separator, slash)
	is_backslash := p.instr2(.eq, separator_body, p.i1_type, separator, backslash)
	is_normalized_backslash := p.instr2(.and_, separator_body, p.i1_type, normalize_backslash, is_backslash)
	is_separator := p.instr2(.or_, separator_body, p.i1_type, is_slash, is_normalized_backslash)
	p.instr3(.br, separator_body, p.void_type, is_separator, ssa.ValueID(component_start), ssa.ValueID(component_start))
	separator_next := p.instr2(.add, component_start, p.i64_type, read_index, one64)
	component_read := p.integer_select(component_start, is_separator, separator_next, read_index, p.i64_type)
	p.instr2(.store, component_start, p.void_type, component_read, read_slot)
	p.instr2(.store, component_start, p.void_type, read_index, component_slot)
	p.instr3(.br, component_start, p.void_type, is_separator, ssa.ValueID(separator_condition), ssa.ValueID(component_condition))
	current_read := p.instr1(.load, component_condition, p.i64_type, read_slot)
	component_has_input := p.instr2(.lt, component_condition, p.i1_type, current_read, length)
	p.instr3(.br, component_condition, p.void_type, component_has_input, ssa.ValueID(component_body), ssa.ValueID(component_done))
	character_address := p.instr2(.add, component_body, p.ptr_i8, data, current_read)
	character := p.instr1(.load, component_body, p.u8_type, character_address)
	character_is_slash := p.instr2(.eq, component_body, p.i1_type, character, slash)
	character_is_backslash := p.instr2(.eq, component_body, p.i1_type, character, backslash)
	character_is_normalized_backslash := p.instr2(.and_, component_body, p.i1_type, normalize_backslash, character_is_backslash)
	character_is_separator := p.instr2(.or_, component_body, p.i1_type, character_is_slash, character_is_normalized_backslash)
	component_next := p.instr2(.add, component_body, p.i64_type, current_read, one64)
	next_component_read := p.integer_select(component_body, character_is_separator, current_read, component_next, p.i64_type)
	p.instr2(.store, component_body, p.void_type, next_component_read, read_slot)
	p.instr3(.br, component_body, p.void_type, character_is_separator, ssa.ValueID(component_done), ssa.ValueID(component_condition))
	component_begin := p.instr1(.load, component_done, p.i64_type, component_slot)
	component_end := p.instr1(.load, component_done, p.i64_type, read_slot)
	component_length := p.instr2(.sub, component_done, p.i64_type, component_end, component_begin)
	component_data := p.instr2(.add, component_done, p.ptr_i8, data, component_begin)
	component_first := p.instr1(.load, component_done, p.u8_type, component_data)
	dot := p.m.get_or_add_const(p.u8_type, '46')
	first_is_dot := p.instr2(.eq, component_done, p.i1_type, component_first, dot)
	length_is_one := p.instr2(.eq, component_done, p.i1_type, component_length, one64)
	is_dot := p.instr2(.and_, component_done, p.i1_type, first_is_dot, length_is_one)
	p.instr3(.br, component_done, p.void_type, is_dot, ssa.ValueID(dot_policy), ssa.ValueID(check_dotdot))
	ended_at_input := p.instr2(.eq, dot_policy, p.i1_type, component_end, length)
	preserve_dot := p.instr2(.and_, dot_policy, p.i1_type, preserve_final_dot, ended_at_input)
	p.instr3(.br, dot_policy, p.void_type, preserve_dot, ssa.ValueID(set_normal_append), ssa.ValueID(separator_condition))
	length_is_two := p.instr2(.eq, check_dotdot, p.i1_type, component_length, two64)
	could_be_dotdot := p.instr2(.and_, check_dotdot, p.i1_type, length_is_two, first_is_dot)
	p.instr3(.br, check_dotdot, p.void_type, could_be_dotdot, ssa.ValueID(dotdot_second), ssa.ValueID(set_normal_append))
	second_address := p.instr2(.add, dotdot_second, p.ptr_i8, component_data, one64)
	second := p.instr1(.load, dotdot_second, p.u8_type, second_address)
	second_is_dot := p.instr2(.eq, dotdot_second, p.i1_type, second, dot)
	p.instr3(.br, dotdot_second, p.void_type, second_is_dot, ssa.ValueID(dotdot_policy), ssa.ValueID(set_normal_append))
	p.instr3(.br, dotdot_policy, p.void_type, resolve_backlinks, ssa.ValueID(dotdot_handle), ssa.ValueID(set_dotdot_append))
	depth := p.instr1(.load, dotdot_handle, p.i64_type, depth_slot)
	has_component := p.instr2(.gt, dotdot_handle, p.i1_type, depth, zero64)
	p.instr3(.br, dotdot_handle, p.void_type, has_component, ssa.ValueID(dotdot_top), ssa.ValueID(dotdot_empty))
	top_index := p.instr2(.sub, dotdot_top, p.i64_type, depth, one64)
	top_kind_address := p.instr2(.add, dotdot_top, p.ptr_i8, component_kinds, top_index)
	top_kind_pointer := p.instr1(.bitcast, dotdot_top, ptr_i1, top_kind_address)
	top_is_dotdot := p.instr1(.load, dotdot_top, p.i1_type, top_kind_pointer)
	p.instr3(.br, dotdot_top, p.void_type, top_is_dotdot, ssa.ValueID(set_dotdot_append), ssa.ValueID(dotdot_pop))
	top_start_offset := p.instr2(.mul, dotdot_pop, p.i64_type, top_index, eight64)
	top_start_address := p.instr2(.add, dotdot_pop, p.ptr_i8, component_starts, top_start_offset)
	top_start_pointer := p.instr1(.bitcast, dotdot_pop, ptr_i64, top_start_address)
	top_start := p.instr1(.load, dotdot_pop, p.i64_type, top_start_pointer)
	p.instr2(.store, dotdot_pop, p.void_type, top_start, write_slot)
	p.instr2(.store, dotdot_pop, p.void_type, top_index, depth_slot)
	p.instr1(.jmp, dotdot_pop, p.void_type, ssa.ValueID(separator_condition))
	rooted := p.instr1(.load, dotdot_empty, p.i1_type, rooted_slot)
	p.instr3(.br, dotdot_empty, p.void_type, rooted, ssa.ValueID(separator_condition), ssa.ValueID(set_dotdot_append))
	p.instr2(.store, set_normal_append, p.void_type, false_value, append_dotdot_slot)
	p.instr1(.jmp, set_normal_append, p.void_type, ssa.ValueID(append_prepare))
	p.instr2(.store, set_dotdot_append, p.void_type, true_value, append_dotdot_slot)
	p.instr1(.jmp, set_dotdot_append, p.void_type, ssa.ValueID(append_prepare))
	append_write := p.instr1(.load, append_prepare, p.i64_type, write_slot)
	append_depth := p.instr1(.load, append_prepare, p.i64_type, depth_slot)
	append_start_offset := p.instr2(.mul, append_prepare, p.i64_type, append_depth, eight64)
	append_start_address := p.instr2(.add, append_prepare, p.ptr_i8, component_starts, append_start_offset)
	append_start_pointer := p.instr1(.bitcast, append_prepare, ptr_i64, append_start_address)
	p.instr2(.store, append_prepare, p.void_type, append_write, append_start_pointer)
	append_kind_address := p.instr2(.add, append_prepare, p.ptr_i8, component_kinds, append_depth)
	append_kind_pointer := p.instr1(.bitcast, append_prepare, ptr_i1, append_kind_address)
	append_kind := p.instr1(.load, append_prepare, p.i1_type, append_dotdot_slot)
	p.instr2(.store, append_prepare, p.void_type, append_kind, append_kind_pointer)
	next_depth := p.instr2(.add, append_prepare, p.i64_type, append_depth, one64)
	p.instr2(.store, append_prepare, p.void_type, next_depth, depth_slot)
	append_rooted := p.instr1(.load, append_prepare, p.i1_type, rooted_slot)
	root_min := p.integer_select(append_prepare, append_rooted, one64, zero64, p.i64_type)
	needs_separator := p.instr2(.gt, append_prepare, p.i1_type, append_write, root_min)
	p.instr3(.br, append_prepare, p.void_type, needs_separator, ssa.ValueID(append_separator), ssa.ValueID(copy_component))
	separator_output := p.instr2(.add, append_separator, p.ptr_i8, output, append_write)
	p.instr2(.store, append_separator, p.void_type, slash, separator_output)
	write_after_separator := p.instr2(.add, append_separator, p.i64_type, append_write, one64)
	p.instr2(.store, append_separator, p.void_type, write_after_separator, write_slot)
	p.instr1(.jmp, append_separator, p.void_type, ssa.ValueID(copy_component))
	copy_write := p.instr1(.load, copy_component, p.i64_type, write_slot)
	copy_destination := p.instr2(.add, copy_component, p.ptr_i8, output, copy_write)
	copy_begin := p.instr1(.load, copy_component, p.i64_type, component_slot)
	copy_source := p.instr2(.add, copy_component, p.ptr_i8, data, copy_begin)
	copy_end := p.instr1(.load, copy_component, p.i64_type, read_slot)
	copy_length := p.instr2(.sub, copy_component, p.i64_type, copy_end, copy_begin)
	memcpy_ref := p.m.add_value(.func_ref, p.ptr_i8, 'memcpy', p.fn_ids['memcpy'])
	p.m.add_instr(.call, copy_component, p.ptr_i8, [memcpy_ref, copy_destination, copy_source,
		copy_length])
	write_after_copy := p.instr2(.add, copy_component, p.i64_type, copy_write, copy_length)
	p.instr2(.store, copy_component, p.void_type, write_after_copy, write_slot)
	p.instr1(.jmp, copy_component, p.void_type, ssa.ValueID(separator_condition))
	done_write := p.instr1(.load, done, p.i64_type, write_slot)
	is_empty := p.instr2(.eq, done, p.i1_type, done_write, zero64)
	p.instr3(.br, done, p.void_type, is_empty, ssa.ValueID(emit_dot), ssa.ValueID(finish))
	p.instr2(.store, emit_dot, p.void_type, dot, output)
	p.instr2(.store, emit_dot, p.void_type, one64, write_slot)
	p.instr1(.jmp, emit_dot, p.void_type, ssa.ValueID(finish))
	final_length := p.instr1(.load, finish, p.i64_type, write_slot)
	end := p.instr2(.add, finish, p.ptr_i8, output, final_length)
	zero_byte := p.m.get_or_add_const(p.u8_type, '0')
	p.instr2(.store, finish, p.void_type, zero_byte, end)
	result_slot := p.instr0(.alloca, finish, p.m.type_store.get_ptr(p.str_type))
	p.instr2(.store, finish, p.void_type, output, p.string_field_ptr(finish, result_slot, 0))
	final_length32 := p.instr1(.trunc, finish, p.i32_type, final_length)
	p.instr2(.store, finish, p.void_type, final_length32, p.string_field_ptr(finish, result_slot, 1))
	zero32 := p.m.get_or_add_const(p.i32_type, '0')
	p.instr2(.store, finish, p.void_type, zero32, p.string_field_ptr(finish, result_slot, 2))
	result := p.instr1(.load, finish, p.str_type, result_slot)
	free_ref := p.m.add_value(.func_ref, p.void_type, 'free', p.fn_ids['free'])
	p.m.add_instr(.call, finish, p.void_type, [free_ref, component_starts])
	p.m.add_instr(.call, finish, p.void_type, [free_ref, component_kinds])
	p.instr1(.ret, finish, p.void_type, result)
}

fn (mut p FastArm64Program) register_os_path_runtime() {
	for key, signature in p.functions {
		if signature.module_name != 'os' || key != 'os.join_path_single' {
			continue
		}
		id := p.register_signature_function(key) or { return }
		entry := p.m.add_block(id, 'os_join_path_entry')
		trim_condition := p.m.add_block(id, 'os_join_path_trim_condition')
		trim_character := p.m.add_block(id, 'os_join_path_trim_character')
		trim_decrement := p.m.add_block(id, 'os_join_path_trim_decrement')
		build := p.m.add_block(id, 'os_join_path_build')
		normalize := p.m.add_block(id, 'os_join_path_normalize')
		trim_root_check := p.m.add_block(id, 'os_join_path_trim_root_check')
		trim_root := p.m.add_block(id, 'os_join_path_trim_root')
		return_result := p.m.add_block(id, 'os_join_path_return')
		base := p.add_arg(id, p.str_type, 'base')
		elem := p.add_arg(id, p.str_type, 'elem')
		base_slot := p.instr0(.alloca, entry, p.m.type_store.get_ptr(p.str_type))
		elem_slot := p.instr0(.alloca, entry, p.m.type_store.get_ptr(p.str_type))
		result_slot := p.instr0(.alloca, entry, p.m.type_store.get_ptr(p.str_type))
		trim_length_slot := p.instr0(.alloca, entry, p.m.type_store.get_ptr(p.i32_type))
		p.instr2(.store, entry, p.void_type, base, base_slot)
		p.instr2(.store, entry, p.void_type, elem, elem_slot)
		base_pointer := p.instr1(.load, entry, p.ptr_i8, p.string_field_ptr(entry, base_slot, 0))
		elem_pointer := p.instr1(.load, entry, p.ptr_i8, p.string_field_ptr(entry, elem_slot, 0))
		base_length := p.instr1(.load, entry, p.i32_type, p.string_field_ptr(entry, base_slot, 1))
		elem_length := p.instr1(.load, entry, p.i32_type, p.string_field_ptr(entry, elem_slot, 1))
		p.instr2(.store, entry, p.void_type, base_length, trim_length_slot)
		p.instr1(.jmp, entry, p.void_type, ssa.ValueID(trim_condition))
		zero32 := p.m.get_or_add_const(p.i32_type, '0')
		one32 := p.m.get_or_add_const(p.i32_type, '1')
		trimmed_length := p.instr1(.load, trim_condition, p.i32_type, trim_length_slot)
		has_trim_character := p.instr2(.gt, trim_condition, p.i1_type, trimmed_length, zero32)
		p.instr3(.br, trim_condition, p.void_type, has_trim_character, ssa.ValueID(trim_character), ssa.ValueID(build))
		last_index := p.instr2(.sub, trim_character, p.i32_type, trimmed_length, one32)
		last_index64 := p.instr1(.zext, trim_character, p.i64_type, last_index)
		last_address := p.instr2(.add, trim_character, p.ptr_i8, base_pointer, last_index64)
		last := p.instr1(.load, trim_character, p.u8_type, last_address)
		slash := p.m.get_or_add_const(p.u8_type, '47')
		backslash := p.m.get_or_add_const(p.u8_type, '92')
		last_is_slash := p.instr2(.eq, trim_character, p.i1_type, last, slash)
		last_is_backslash := p.instr2(.eq, trim_character, p.i1_type, last, backslash)
		last_is_separator := p.instr2(.or_, trim_character, p.i1_type, last_is_slash, last_is_backslash)
		p.instr3(.br, trim_character, p.void_type, last_is_separator, ssa.ValueID(trim_decrement), ssa.ValueID(build))
		decremented_length := p.instr2(.sub, trim_decrement, p.i32_type, trimmed_length, one32)
		p.instr2(.store, trim_decrement, p.void_type, decremented_length, trim_length_slot)
		p.instr1(.jmp, trim_decrement, p.void_type, ssa.ValueID(trim_condition))
		base_trimmed_length := p.instr1(.load, build, p.i32_type, trim_length_slot)
		elem_present := p.instr2(.gt, build, p.i1_type, elem_length, zero32)
		separator_length := p.integer_select(build, elem_present, one32, zero32, p.i32_type)
		mut joined_length := p.instr2(.add, build, p.i32_type, base_trimmed_length, elem_length)
		joined_length = p.instr2(.add, build, p.i32_type, joined_length, separator_length)
		base_length64 := p.instr1(.zext, build, p.i64_type, base_trimmed_length)
		elem_length64 := p.instr1(.zext, build, p.i64_type, elem_length)
		separator_length64 := p.instr1(.zext, build, p.i64_type, separator_length)
		joined_length64 := p.instr1(.zext, build, p.i64_type, joined_length)
		one64 := p.m.get_or_add_const(p.i64_type, '1')
		allocation_size := p.instr2(.add, build, p.i64_type, joined_length64, one64)
		malloc_ref := p.m.add_value(.func_ref, p.ptr_i8, 'malloc', p.fn_ids['malloc'])
		joined_pointer := p.m.add_instr(.call, build, p.ptr_i8, [malloc_ref, allocation_size])
		memcpy_ref := p.m.add_value(.func_ref, p.ptr_i8, 'memcpy', p.fn_ids['memcpy'])
		p.m.add_instr(.call, build, p.ptr_i8, [memcpy_ref, joined_pointer, base_pointer,
			base_length64])
		separator_pointer := p.instr2(.add, build, p.ptr_i8, joined_pointer, base_length64)
		p.instr2(.store, build, p.void_type, slash, separator_pointer)
		elem_destination := p.instr2(.add, build, p.ptr_i8, separator_pointer, separator_length64)
		p.m.add_instr(.call, build, p.ptr_i8, [memcpy_ref, elem_destination, elem_pointer,
			elem_length64])
		end := p.instr2(.add, build, p.ptr_i8, joined_pointer, joined_length64)
		zero_byte := p.m.get_or_add_const(p.u8_type, '0')
		p.instr2(.store, build, p.void_type, zero_byte, end)
		raw_slot := p.instr0(.alloca, build, p.m.type_store.get_ptr(p.str_type))
		p.instr2(.store, build, p.void_type, joined_pointer, p.string_field_ptr(build, raw_slot, 0))
		p.instr2(.store, build, p.void_type, joined_length, p.string_field_ptr(build, raw_slot, 1))
		p.instr2(.store, build, p.void_type, zero32, p.string_field_ptr(build, raw_slot, 2))
		raw := p.instr1(.load, build, p.str_type, raw_slot)
		p.instr2(.store, build, p.void_type, raw, result_slot)
		has_joined_path := p.instr2(.gt, build, p.i1_type, joined_length, zero32)
		p.instr3(.br, build, p.void_type, has_joined_path, ssa.ValueID(normalize), ssa.ValueID(return_result))
		norm_ref := p.m.add_value(.func_ref, p.str_type, 'fast_path_normalize', p.fn_ids['fast_path_normalize'])
		false_value := p.m.get_or_add_const(p.i1_type, '0')
		true_value := p.m.get_or_add_const(p.i1_type, '1')
		normalized := p.m.add_instr(.call, normalize, p.str_type, [norm_ref, raw, false_value,
			true_value, true_value])
		p.instr2(.store, normalize, p.void_type, normalized, result_slot)
		base_is_empty := p.instr2(.eq, normalize, p.i1_type, base_length, zero32)
		p.instr3(.br, normalize, p.void_type, base_is_empty, ssa.ValueID(trim_root_check), ssa.ValueID(return_result))
		normalized_length := p.instr1(.load, trim_root_check, p.i32_type, p.string_field_ptr(trim_root_check, result_slot, 1))
		has_normalized_path := p.instr2(.gt, trim_root_check, p.i1_type, normalized_length, zero32)
		normalized_pointer := p.instr1(.load, trim_root_check, p.ptr_i8, p.string_field_ptr(trim_root_check, result_slot, 0))
		first_normalized := p.instr1(.load, trim_root_check, p.u8_type, normalized_pointer)
		first_is_root := p.instr2(.eq, trim_root_check, p.i1_type, first_normalized, slash)
		trim_leading_root := p.instr2(.and_, trim_root_check, p.i1_type, has_normalized_path, first_is_root)
		p.instr3(.br, trim_root_check, p.void_type, trim_leading_root, ssa.ValueID(trim_root), ssa.ValueID(return_result))
		trimmed_source := p.instr2(.add, trim_root, p.ptr_i8, normalized_pointer, one64)
		trimmed_result_length := p.instr2(.sub, trim_root, p.i32_type, normalized_length, one32)
		trimmed_result_length64 := p.instr1(.zext, trim_root, p.i64_type, trimmed_result_length)
		memmove_ref := p.m.add_value(.func_ref, p.ptr_i8, 'memmove', p.fn_ids['memmove'])
		p.m.add_instr(.call, trim_root, p.ptr_i8, [memmove_ref, normalized_pointer, trimmed_source,
			trimmed_result_length64])
		trimmed_end := p.instr2(.add, trim_root, p.ptr_i8, normalized_pointer, trimmed_result_length64)
		p.instr2(.store, trim_root, p.void_type, zero_byte, trimmed_end)
		p.instr2(.store, trim_root, p.void_type, normalized_pointer, p.string_field_ptr(trim_root, result_slot, 0))
		p.instr2(.store, trim_root, p.void_type, trimmed_result_length, p.string_field_ptr(trim_root, result_slot, 1))
		p.instr1(.jmp, trim_root, p.void_type, ssa.ValueID(return_result))
		result := p.instr1(.load, return_result, p.str_type, result_slot)
		p.instr1(.ret, return_result, p.void_type, result)
		mut function := p.m.funcs[id]
		function.is_prototype = false
		function.is_c_extern = false
		p.m.funcs[id] = function
		return
	}
}

fn (mut p FastArm64Program) cwd_string(block ssa.BlockID) ssa.ValueID {
	buffer_size := p.m.get_or_add_const(p.i64_type, '4096')
	malloc_ref := p.m.add_value(.func_ref, p.ptr_i8, 'malloc', p.fn_ids['malloc'])
	buffer := p.m.add_instr(.call, block, p.ptr_i8, [malloc_ref, buffer_size])
	getcwd_ref := p.m.add_value(.func_ref, p.ptr_i8, 'getcwd', p.fn_ids['getcwd'])
	p.m.add_instr(.call, block, p.ptr_i8, [getcwd_ref, buffer, buffer_size])
	strlen_ref := p.m.add_value(.func_ref, p.i64_type, 'strlen', p.fn_ids['strlen'])
	length64 := p.m.add_instr(.call, block, p.i64_type, [strlen_ref, buffer])
	length := p.instr1(.trunc, block, p.i32_type, length64)
	zero := p.m.get_or_add_const(p.i32_type, '0')
	slot := p.instr0(.alloca, block, p.m.type_store.get_ptr(p.str_type))
	p.instr2(.store, block, p.void_type, buffer, p.string_field_ptr(block, slot, 0))
	p.instr2(.store, block, p.void_type, length, p.string_field_ptr(block, slot, 1))
	p.instr2(.store, block, p.void_type, zero, p.string_field_ptr(block, slot, 2))
	return p.instr1(.load, block, p.str_type, slot)
}

fn (mut p FastArm64Program) register_os_abs_path_runtime() {
	id := p.register_signature_function('os.abs_path') or { return }
	if p.m.funcs[id].blocks.len > 0 {
		return
	}
	entry := p.m.add_block(id, 'os_abs_path_entry')
	check_first := p.m.add_block(id, 'os_abs_path_check_first')
	absolute := p.m.add_block(id, 'os_abs_path_absolute')
	relative := p.m.add_block(id, 'os_abs_path_relative')
	empty := p.m.add_block(id, 'os_abs_path_empty')
	path := p.add_arg(id, p.str_type, 'path')
	path_slot := p.instr0(.alloca, entry, p.m.type_store.get_ptr(p.str_type))
	p.instr2(.store, entry, p.void_type, path, path_slot)
	path_length := p.instr1(.load, entry, p.i32_type, p.string_field_ptr(entry, path_slot, 1))
	zero32 := p.m.get_or_add_const(p.i32_type, '0')
	has_path := p.instr2(.gt, entry, p.i1_type, path_length, zero32)
	p.instr3(.br, entry, p.void_type, has_path, ssa.ValueID(check_first), ssa.ValueID(empty))
	norm_ref := p.m.add_value(.func_ref, p.str_type, 'fast_path_normalize', p.fn_ids['fast_path_normalize'])
	true_value := p.m.get_or_add_const(p.i1_type, '1')
	false_value := p.m.get_or_add_const(p.i1_type, '0')
	normalized := p.m.add_instr(.call, check_first, p.str_type, [norm_ref, path, true_value,
		false_value, false_value])
	p.instr2(.store, check_first, p.void_type, normalized, path_slot)
	path_data := p.instr1(.load, check_first, p.ptr_i8, p.string_field_ptr(check_first, path_slot, 0))
	first := p.instr1(.load, check_first, p.u8_type, path_data)
	slash := p.m.get_or_add_const(p.u8_type, '47')
	is_absolute := p.instr2(.eq, check_first, p.i1_type, first, slash)
	p.instr3(.br, check_first, p.void_type, is_absolute, ssa.ValueID(absolute), ssa.ValueID(relative))
	p.instr1(.ret, absolute, p.void_type, normalized)
	cwd := p.cwd_string(relative)
	join_id := p.fn_ids['os.join_path_single']
	join_ref := p.m.add_value(.func_ref, p.str_type, p.fn_symbols['os.join_path_single'], join_id)
	joined := p.m.add_instr(.call, relative, p.str_type, [join_ref, cwd, normalized])
	p.instr1(.ret, relative, p.void_type, joined)
	empty_cwd := p.cwd_string(empty)
	p.instr1(.ret, empty, p.void_type, empty_cwd)
	mut function := p.m.funcs[id]
	function.is_prototype = false
	function.is_c_extern = false
	p.m.funcs[id] = function
}

fn (mut p FastArm64Program) register_memory_wrapper_runtime() {
	for key, signature in p.functions {
		if signature.is_disabled || key.starts_with('C.') {
			continue
		}
		symbol := fastc_c_function_name_for_key(key)
		mut external := ''
		if symbol in ['builtin__malloc', 'builtin__malloc_noscan', 'builtin__malloc_uninit'] {
			external = 'malloc'
		} else if symbol in ['builtin__vcalloc', 'builtin__vcalloc_noscan'] {
			external = 'calloc'
		} else if symbol == 'builtin__vmemcpy' {
			external = 'memcpy'
		} else if symbol == 'builtin__vmemmove' {
			external = 'memmove'
		} else if symbol == 'builtin__vmemset' {
			external = 'memset'
		} else if symbol == 'builtin__vmemcmp' {
			external = 'memcmp'
		} else if symbol == 'builtin__free' {
			external = 'free'
		}
		if external == '' {
			continue
		}
		id := p.register_signature_function(key) or { continue }
		if p.m.funcs[id].blocks.len > 0 {
			continue
		}
		entry := p.m.add_block(id, 'memory_wrapper_entry')
		mut wrapper_args := []ssa.ValueID{}
		for i, parameter_type in signature.parameter_types {
			wrapper_args << p.add_arg(id, p.type_id(parameter_type), 'arg_${i}')
		}
		mut call_arguments := wrapper_args.clone()
		if external == 'calloc' {
			one := p.m.get_or_add_const(p.i64_type, '1')
			call_arguments = [one, wrapper_args[0]]
		}
		external_return := match external {
			'free' { p.void_type }
			'memcmp' { p.i32_type }
			else { p.ptr_i8 }
		}
		external_ref := p.m.add_value(.func_ref, external_return, external, p.fn_ids[external])
		mut operands := [external_ref]
		operands << call_arguments
		result := p.m.add_instr(.call, entry, external_return, operands)
		if p.fn_returns[key] == p.void_type {
			p.instr0(.ret, entry, p.void_type)
		} else {
			p.instr1(.ret, entry, p.void_type, result)
		}
		mut function := p.m.funcs[id]
		function.is_prototype = false
		function.is_c_extern = false
		p.m.funcs[id] = function
	}
}

fn (mut p FastArm64Program) register_cleanup_runtime() {
	for key, signature in p.functions {
		if signature.is_disabled || key.starts_with('C.') {
			continue
		}
		symbol := fastc_c_function_name_for_key(key)
		if !symbol.ends_with('__free') {
			continue
		}
		id := p.register_signature_function(key) or { continue }
		if p.m.funcs[id].blocks.len > 0 {
			continue
		}
		entry := p.m.add_block(id, 'builtin_cleanup_entry')
		for i, parameter_type in signature.parameter_types {
			p.add_arg(id, p.type_id(parameter_type), 'arg_${i}')
		}
		p.instr0(.ret, entry, p.void_type)
		mut function := p.m.funcs[id]
		function.is_prototype = false
		function.is_c_extern = false
		p.m.funcs[id] = function
	}
}

fn (mut p FastArm64Program) register_pointer_predicate_runtime() {
	for key, signature in p.functions {
		if signature.is_disabled || key.starts_with('C.') {
			continue
		}
		symbol := fastc_c_function_name_for_key(key)
		if symbol != 'builtin__isnil' {
			continue
		}
		id := p.register_signature_function(key) or { continue }
		if p.m.funcs[id].blocks.len > 0 {
			continue
		}
		entry := p.m.add_block(id, 'isnil_entry')
		pointer := p.add_arg(id, p.ptr_i8, 'pointer')
		zero := p.m.get_or_add_const(p.ptr_i8, '0')
		result := p.instr2(.eq, entry, p.i1_type, pointer, zero)
		p.instr1(.ret, entry, p.void_type, result)
		mut function := p.m.funcs[id]
		function.is_prototype = false
		function.is_c_extern = false
		p.m.funcs[id] = function
	}
}

fn (mut p FastArm64Program) register_string_conversion_runtime() {
	for key, signature in p.functions {
		if signature.is_disabled || key.starts_with('C.') {
			continue
		}
		symbol := fastc_c_function_name_for_key(key)
		if symbol !in ['builtin__tos_clone', 'builtin__tos2', 'builtin__tos3', 'builtin__tos4',
			'builtin__tos5', 'builtin__string_clone'] {
			continue
		}
		id := p.register_signature_function(key) or { continue }
		if p.m.funcs[id].blocks.len > 0 {
			continue
		}
		entry := p.m.add_block(id, 'string_conversion_entry')
		mut pointer := ssa.ValueID(0)
		mut length64 := ssa.ValueID(0)
		mut length := ssa.ValueID(0)
		if symbol == 'builtin__string_clone' {
			value := p.add_arg(id, p.str_type, 'string_value')
			value_slot := p.instr0(.alloca, entry, p.m.type_store.get_ptr(p.str_type))
			p.instr2(.store, entry, p.void_type, value, value_slot)
			pointer = p.instr1(.load, entry, p.ptr_i8, p.string_field_ptr(entry, value_slot, 0))
			length = p.instr1(.load, entry, p.i32_type, p.string_field_ptr(entry, value_slot, 1))
			length64 = p.instr1(.sext, entry, p.i64_type, length)
		} else {
			pointer = p.add_arg(id, p.ptr_i8, 'string_pointer')
			strlen_ref := p.m.add_value(.func_ref, p.i64_type, 'strlen', p.fn_ids['strlen'])
			length64 = p.m.add_instr(.call, entry, p.i64_type, [strlen_ref, pointer])
			length = p.instr1(.trunc, entry, p.i32_type, length64)
		}
		mut result_pointer := pointer
		if symbol in ['builtin__tos_clone', 'builtin__string_clone'] {
			one := p.m.get_or_add_const(p.i64_type, '1')
			allocation_size := p.instr2(.add, entry, p.i64_type, length64, one)
			malloc_ref := p.m.add_value(.func_ref, p.ptr_i8, 'malloc', p.fn_ids['malloc'])
			result_pointer = p.m.add_instr(.call, entry, p.ptr_i8, [malloc_ref, allocation_size])
			memcpy_ref := p.m.add_value(.func_ref, p.ptr_i8, 'memcpy', p.fn_ids['memcpy'])
			p.m.add_instr(.call, entry, p.ptr_i8, [memcpy_ref, result_pointer, pointer, length64])
			end := p.instr2(.add, entry, p.ptr_i8, result_pointer, length64)
			zero_byte := p.m.get_or_add_const(p.u8_type, '0')
			p.instr2(.store, entry, p.void_type, zero_byte, end)
		}
		result_slot := p.instr0(.alloca, entry, p.m.type_store.get_ptr(p.str_type))
		p.instr2(.store, entry, p.void_type, result_pointer, p.string_field_ptr(entry, result_slot, 0))
		p.instr2(.store, entry, p.void_type, length, p.string_field_ptr(entry, result_slot, 1))
		zero := p.m.get_or_add_const(p.i32_type, '0')
		p.instr2(.store, entry, p.void_type, zero, p.string_field_ptr(entry, result_slot, 2))
		result := p.instr1(.load, entry, p.str_type, result_slot)
		p.instr1(.ret, entry, p.void_type, result)
		mut function := p.m.funcs[id]
		function.is_prototype = false
		function.is_c_extern = false
		p.m.funcs[id] = function
	}
}

fn (mut p FastArm64Program) register_fastc_vmod_root_runtime() {
	for key, signature in p.functions {
		if !key.ends_with('.fastc_vmod_root_for_file') {
			continue
		}
		id := p.register_signature_function(key) or { return }
		entry := p.m.add_block(id, 'fastc_vmod_root_entry')
		for i, parameter_type in signature.parameter_types {
			p.add_arg(id, p.type_id(parameter_type), 'arg_${i}')
		}
		root := p.m.add_value(.string_literal, p.str_type, p.prefs.vroot, 0)
		p.instr1(.ret, entry, p.void_type, root)
		mut function := p.m.funcs[id]
		function.is_prototype = false
		function.is_c_extern = false
		p.m.funcs[id] = function
		return
	}
}

fn (mut p FastArm64Program) register_preferences_runtime() {
	for key, signature in p.functions {
		if signature.module_name != 'v3.pref' || !key.ends_with('.new_preferences') {
			continue
		}
		id := p.register_signature_function(key) or { return }
		pointer_type := p.fn_returns[key]
		pointer_layout := p.m.type_store.types[pointer_type]
		if pointer_layout.kind != .ptr_t {
			return
		}
		preferences_type := pointer_layout.elem_type
		entry := p.m.add_block(id, 'preferences_entry')
		size := p.m.get_or_add_const(p.i64_type, p.m.type_size(preferences_type).str())
		malloc_ref := p.m.add_value(.func_ref, p.ptr_i8, 'malloc', p.fn_ids['malloc'])
		bytes := p.m.add_instr(.call, entry, p.ptr_i8, [malloc_ref, size])
		preferences := p.instr1(.bitcast, entry, pointer_type, bytes)
		memset_ref := p.m.add_value(.func_ref, p.ptr_i8, 'memset', p.fn_ids['memset'])
		zero := p.m.get_or_add_const(p.i32_type, '0')
		p.m.add_instr(.call, entry, p.ptr_i8, [memset_ref, bytes, zero, size])
		layout := p.m.type_store.types[preferences_type]
		for field, field_name in layout.field_names {
			mut literal := ''
			if field_name == 'backend' {
				literal = 'c'
			} else if field_name == 'ccompiler' {
				literal = 'gcc'
			} else if field_name == 'vroot' {
				literal = p.prefs.vroot
			} else if field_name == 'vexe' {
				literal = p.prefs.vexe
			} else {
				continue
			}
			value := p.m.add_value(.string_literal, p.str_type, literal, 0)
			p.instr2(.store, entry, p.void_type, value, p.struct_field_ptr(entry, preferences, preferences_type, field))
		}
		p.instr1(.ret, entry, p.void_type, preferences)
		mut function := p.m.funcs[id]
		function.is_prototype = false
		function.is_c_extern = false
		p.m.funcs[id] = function
		return
	}
}

fn (mut p FastArm64Program) register_os_process_runtime() {
	for key, signature in p.functions {
		if signature.module_name != 'os' || key != 'os.execute' {
			continue
		}
		id := p.register_signature_function(key) or { continue }
		ret_type := p.fn_returns[key]
		if p.m.type_store.types[ret_type].kind != .struct_t {
			continue
		}
		layout := p.m.type_store.types[ret_type]
		mut exit_field := -1
		mut output_field := -1
		for i, field_name in layout.field_names {
			if field_name == 'exit_code' {
				exit_field = i
			} else if field_name == 'output' {
				output_field = i
			}
		}
		if exit_field < 0 || output_field < 0 {
			continue
		}
		entry := p.m.add_block(id, 'os_execute_entry')
		opened := p.m.add_block(id, 'os_execute_opened')
		open_failed := p.m.add_block(id, 'os_execute_open_failed')
		read_condition := p.m.add_block(id, 'os_execute_read_condition')
		grow := p.m.add_block(id, 'os_execute_grow')
		read := p.m.add_block(id, 'os_execute_read')
		close_process := p.m.add_block(id, 'os_execute_close')
		return_result := p.m.add_block(id, 'os_execute_return')
		mut process_args := []ssa.ValueID{}
		for i, parameter_type in signature.parameter_types {
			process_args << p.add_arg(id, p.type_id(parameter_type), 'arg_${i}')
		}
		result_slot := p.instr0(.alloca, entry, p.m.type_store.get_ptr(ret_type))
		buffer_slot := p.instr0(.alloca, entry, p.m.type_store.get_ptr(p.ptr_i8))
		length_slot := p.instr0(.alloca, entry, p.m.type_store.get_ptr(p.u64_type))
		capacity_slot := p.instr0(.alloca, entry, p.m.type_store.get_ptr(p.u64_type))
		output_slot := p.instr0(.alloca, entry, p.m.type_store.get_ptr(p.str_type))
		result_bytes := p.instr1(.bitcast, entry, p.ptr_i8, result_slot)
		memset_ref := p.m.add_value(.func_ref, p.ptr_i8, 'memset', p.fn_ids['memset'])
		zero32 := p.m.get_or_add_const(p.i32_type, '0')
		zero64 := p.m.get_or_add_const(p.u64_type, '0')
		size := p.m.get_or_add_const(p.i64_type, p.m.type_size(ret_type).str())
		p.m.add_instr(.call, entry, p.ptr_i8, [memset_ref, result_bytes, zero32, size])
		command_slot := p.instr0(.alloca, entry, p.m.type_store.get_ptr(p.str_type))
		p.instr2(.store, entry, p.void_type, process_args[0], command_slot)
		command := p.instr1(.load, entry, p.ptr_i8, p.string_field_ptr(entry, command_slot, 0))
		command_length32 := p.instr1(.load, entry, p.i32_type, p.string_field_ptr(entry, command_slot, 1))
		command_length := p.instr1(.zext, entry, p.u64_type, command_length32)
		suffix_value := p.m.add_value(.string_literal, p.str_type, ') 2>&1', 0)
		suffix_slot := p.instr0(.alloca, entry, p.m.type_store.get_ptr(p.str_type))
		p.instr2(.store, entry, p.void_type, suffix_value, suffix_slot)
		suffix := p.instr1(.load, entry, p.ptr_i8, p.string_field_ptr(entry, suffix_slot, 0))
		suffix_length := p.m.get_or_add_const(p.u64_type, '6')
		one64 := p.m.get_or_add_const(p.u64_type, '1')
		prefixed_length := p.instr2(.add, entry, p.u64_type, command_length, one64)
		merged_length := p.instr2(.add, entry, p.u64_type, prefixed_length, suffix_length)
		allocation_size := p.instr2(.add, entry, p.u64_type, merged_length, one64)
		malloc_ref := p.m.add_value(.func_ref, p.ptr_i8, 'malloc', p.fn_ids['malloc'])
		merged_command := p.m.add_instr(.call, entry, p.ptr_i8, [malloc_ref, allocation_size])
		open_parenthesis := p.m.get_or_add_const(p.u8_type, '40')
		p.instr2(.store, entry, p.void_type, open_parenthesis, merged_command)
		memcpy_ref := p.m.add_value(.func_ref, p.ptr_i8, 'memcpy', p.fn_ids['memcpy'])
		command_destination := p.instr2(.add, entry, p.ptr_i8, merged_command, one64)
		p.m.add_instr(.call, entry, p.ptr_i8, [memcpy_ref, command_destination, command,
			command_length])
		suffix_destination := p.instr2(.add, entry, p.ptr_i8, command_destination, command_length)
		p.m.add_instr(.call, entry, p.ptr_i8, [memcpy_ref, suffix_destination, suffix, suffix_length])
		terminator := p.instr2(.add, entry, p.ptr_i8, merged_command, merged_length)
		zero8 := p.m.get_or_add_const(p.u8_type, '0')
		p.instr2(.store, entry, p.void_type, zero8, terminator)
		mode_value := p.m.add_value(.string_literal, p.str_type, 'r', 0)
		mode_slot := p.instr0(.alloca, entry, p.m.type_store.get_ptr(p.str_type))
		p.instr2(.store, entry, p.void_type, mode_value, mode_slot)
		mode := p.instr1(.load, entry, p.ptr_i8, p.string_field_ptr(entry, mode_slot, 0))
		popen_ref := p.m.add_value(.func_ref, p.ptr_i8, 'popen', p.fn_ids['popen'])
		stream := p.m.add_instr(.call, entry, p.ptr_i8, [popen_ref, merged_command, mode])
		free_ref := p.m.add_value(.func_ref, p.void_type, 'free', p.fn_ids['free'])
		p.m.add_instr(.call, entry, p.void_type, [free_ref, merged_command])
		null_pointer := p.m.get_or_add_const(p.ptr_i8, '0')
		open_succeeded := p.instr2(.ne, entry, p.i1_type, stream, null_pointer)
		p.instr3(.br, entry, p.void_type, open_succeeded, ssa.ValueID(opened), ssa.ValueID(open_failed))

		initial_capacity := p.m.get_or_add_const(p.u64_type, '4096')
		initial_allocation_size := p.instr2(.add, opened, p.u64_type, initial_capacity, one64)
		buffer := p.m.add_instr(.call, opened, p.ptr_i8, [malloc_ref, initial_allocation_size])
		p.instr2(.store, opened, p.void_type, buffer, buffer_slot)
		p.instr2(.store, opened, p.void_type, zero64, length_slot)
		p.instr2(.store, opened, p.void_type, initial_capacity, capacity_slot)
		p.instr1(.jmp, opened, p.void_type, ssa.ValueID(read_condition))

		length := p.instr1(.load, read_condition, p.u64_type, length_slot)
		capacity := p.instr1(.load, read_condition, p.u64_type, capacity_slot)
		is_full := p.instr2(.ge, read_condition, p.i1_type, length, capacity)
		p.instr3(.br, read_condition, p.void_type, is_full, ssa.ValueID(grow), ssa.ValueID(read))

		current_buffer := p.instr1(.load, grow, p.ptr_i8, buffer_slot)
		two := p.m.get_or_add_const(p.u64_type, '2')
		new_capacity := p.instr2(.mul, grow, p.u64_type, capacity, two)
		new_allocation_size := p.instr2(.add, grow, p.u64_type, new_capacity, one64)
		realloc_ref := p.m.add_value(.func_ref, p.ptr_i8, 'realloc', p.fn_ids['realloc'])
		grown_buffer := p.m.add_instr(.call, grow, p.ptr_i8, [realloc_ref, current_buffer,
			new_allocation_size])
		p.instr2(.store, grow, p.void_type, grown_buffer, buffer_slot)
		p.instr2(.store, grow, p.void_type, new_capacity, capacity_slot)
		p.instr1(.jmp, grow, p.void_type, ssa.ValueID(read_condition))

		read_buffer := p.instr1(.load, read, p.ptr_i8, buffer_slot)
		read_length := p.instr1(.load, read, p.u64_type, length_slot)
		read_capacity := p.instr1(.load, read, p.u64_type, capacity_slot)
		destination := p.instr2(.add, read, p.ptr_i8, read_buffer, read_length)
		available := p.instr2(.sub, read, p.u64_type, read_capacity, read_length)
		one := p.m.get_or_add_const(p.u64_type, '1')
		fread_ref := p.m.add_value(.func_ref, p.u64_type, 'fread', p.fn_ids['fread'])
		read_count := p.m.add_instr(.call, read, p.u64_type, [fread_ref, destination, one, available,
			stream])
		new_length := p.instr2(.add, read, p.u64_type, read_length, read_count)
		p.instr2(.store, read, p.void_type, new_length, length_slot)
		has_data := p.instr2(.gt, read, p.i1_type, read_count, zero64)
		p.instr3(.br, read, p.void_type, has_data, ssa.ValueID(read_condition), ssa.ValueID(close_process))

		pclose_ref := p.m.add_value(.func_ref, p.i32_type, 'pclose', p.fn_ids['pclose'])
		status := p.m.add_instr(.call, close_process, p.i32_type, [pclose_ref, stream])
		eight := p.m.get_or_add_const(p.i32_type, '8')
		close_failed := p.instr2(.lt, close_process, p.i1_type, status, zero32)
		shifted_exit := p.instr2(.ashr, close_process, p.i32_type, status, eight)
		byte_mask := p.m.get_or_add_const(p.i32_type, '255')
		normal_exit := p.instr2(.and_, close_process, p.i32_type, shifted_exit, byte_mask)
		signal_mask := p.m.get_or_add_const(p.i32_type, '127')
		signal := p.instr2(.and_, close_process, p.i32_type, status, signal_mask)
		has_signal := p.instr2(.ne, close_process, p.i1_type, signal, zero32)
		not_stopped := p.instr2(.ne, close_process, p.i1_type, signal, signal_mask)
		is_signaled := p.instr2(.and_, close_process, p.i1_type, has_signal, not_stopped)
		decoded_status := p.integer_select(close_process, is_signaled, signal, normal_exit, p.i32_type)
		exit_code := p.integer_select(close_process, close_failed, status, decoded_status, p.i32_type)
		mut stored_exit := exit_code
		if layout.fields[exit_field] != p.i32_type {
			stored_exit = p.instr1(.sext, close_process, layout.fields[exit_field], exit_code)
		}
		p.instr2(.store, close_process, p.void_type, stored_exit, p.struct_field_ptr(close_process, result_slot, ret_type, exit_field))
		captured_buffer := p.instr1(.load, close_process, p.ptr_i8, buffer_slot)
		captured_length64 := p.instr1(.load, close_process, p.u64_type, length_slot)
		captured_length := p.instr1(.trunc, close_process, p.i32_type, captured_length64)
		captured_terminator := p.instr2(.add, close_process, p.ptr_i8, captured_buffer, captured_length64)
		p.instr2(.store, close_process, p.void_type, zero8, captured_terminator)
		p.instr2(.store, close_process, p.void_type, captured_buffer, p.string_field_ptr(close_process, output_slot, 0))
		p.instr2(.store, close_process, p.void_type, captured_length, p.string_field_ptr(close_process, output_slot, 1))
		p.instr2(.store, close_process, p.void_type, zero32, p.string_field_ptr(close_process, output_slot, 2))
		captured_output := p.instr1(.load, close_process, p.str_type, output_slot)
		p.instr2(.store, close_process, p.void_type, captured_output, p.struct_field_ptr(close_process, result_slot, ret_type, output_field))
		p.instr1(.jmp, close_process, p.void_type, ssa.ValueID(return_result))

		minus_one := p.m.get_or_add_const(p.i32_type, '-1')
		mut stored_failure := minus_one
		if layout.fields[exit_field] != p.i32_type {
			stored_failure = p.instr1(.sext, open_failed, layout.fields[exit_field], minus_one)
		}
		p.instr2(.store, open_failed, p.void_type, stored_failure, p.struct_field_ptr(open_failed, result_slot, ret_type, exit_field))
		failure_message := p.m.add_value(.string_literal, p.str_type, 'could not start command', 0)
		p.instr2(.store, open_failed, p.void_type, failure_message, p.struct_field_ptr(open_failed, result_slot, ret_type, output_field))
		p.instr1(.jmp, open_failed, p.void_type, ssa.ValueID(return_result))

		result := p.instr1(.load, return_result, ret_type, result_slot)
		p.instr1(.ret, return_result, p.void_type, result)
		mut function := p.m.funcs[id]
		function.is_prototype = false
		function.is_c_extern = false
		p.m.funcs[id] = function
	}
}

fn (mut p FastArm64Program) add_arg(func_id int, typ ssa.TypeID, name string) ssa.ValueID {
	value := p.m.add_value(.argument, typ, name, 0)
	p.m.func_add_param(func_id, value)
	return value
}

fn (mut p FastArm64Program) register_spawn_wrapper(function_key string, target_id int, target_symbol string, return_type ssa.TypeID, parameter_types []ssa.TypeID) (ssa.TypeID, int) {
	if context_type := p.spawn_context_types[function_key] {
		return context_type, p.spawn_wrapper_ids[function_key]
	}
	mut fields := []ssa.TypeID{}
	mut field_names := []string{}
	if return_type != p.void_type {
		fields << return_type
		field_names << 'result'
	}
	for i, parameter_type in parameter_types {
		fields << parameter_type
		field_names << 'arg_${i}'
	}
	if fields.len == 0 {
		fields << p.u8_type
		field_names << 'unused'
	}
	context_type := p.m.type_store.register(ssa.Type{
		kind: .struct_t
		fields: fields
		field_names: field_names
	})
	name_key := fastc_name_key(function_key)
	wrapper_key := '__v_fastc_arm64_spawn_wrapper_${name_key}'
	wrapper_id := p.register_function(wrapper_key, wrapper_key, p.ptr_i8, false)
	entry := p.m.add_block(wrapper_id, 'spawn_wrapper_entry')
	raw_context := p.add_arg(wrapper_id, p.ptr_i8, 'context')
	context := p.instr1(.bitcast, entry, p.m.type_store.get_ptr(context_type), raw_context)
	target_ref := p.m.add_value(.func_ref, return_type, target_symbol, target_id)
	mut operands := []ssa.ValueID{cap: parameter_types.len + 1}
	operands << target_ref
	argument_start := if return_type == p.void_type { 0 } else { 1 }
	for i, parameter_type in parameter_types {
		argument_address := p.struct_field_ptr(entry, context, context_type, argument_start + i)
		operands << p.instr1(.load, entry, parameter_type, argument_address)
	}
	result := p.m.add_instr(.call, entry, return_type, operands)
	if return_type != p.void_type {
		p.instr2(.store, entry, p.void_type, result, p.struct_field_ptr(entry, context, context_type, 0))
	}
	null_pointer := p.m.get_or_add_const(p.ptr_i8, '0')
	p.instr1(.ret, entry, p.void_type, null_pointer)
	mut function := p.m.funcs[wrapper_id]
	function.is_prototype = false
	function.is_c_extern = false
	p.m.funcs[wrapper_id] = function
	p.spawn_context_types[function_key] = context_type
	p.spawn_wrapper_ids[function_key] = wrapper_id
	return context_type, wrapper_id
}

fn (mut p FastArm64Program) instr0(op ssa.OpCode, block ssa.BlockID, typ ssa.TypeID) ssa.ValueID {
	return p.m.add_instr(op, block, typ, []ssa.ValueID{})
}

fn (mut p FastArm64Program) instr1(op ssa.OpCode, block ssa.BlockID, typ ssa.TypeID, a ssa.ValueID) ssa.ValueID {
	return p.m.add_instr(op, block, typ, [a])
}

fn (mut p FastArm64Program) instr2(op ssa.OpCode, block ssa.BlockID, typ ssa.TypeID, a ssa.ValueID, b ssa.ValueID) ssa.ValueID {
	return p.m.add_instr(op, block, typ, [a, b])
}

fn (mut p FastArm64Program) instr3(op ssa.OpCode, block ssa.BlockID, typ ssa.TypeID, a ssa.ValueID, b ssa.ValueID, c ssa.ValueID) ssa.ValueID {
	return p.m.add_instr(op, block, typ, [a, b, c])
}

fn (mut p FastArm64Program) instr4(op ssa.OpCode, block ssa.BlockID, typ ssa.TypeID, a ssa.ValueID, b ssa.ValueID, c ssa.ValueID, d ssa.ValueID) ssa.ValueID {
	return p.m.add_instr(op, block, typ, [a, b, c, d])
}

fn (mut p FastArm64Program) integer_select(block ssa.BlockID, condition ssa.ValueID, if_true ssa.ValueID, if_false ssa.ValueID, typ ssa.TypeID) ssa.ValueID {
	condition_integer := p.instr1(.zext, block, typ, condition)
	zero := p.m.get_or_add_const(typ, '0')
	mask := p.instr2(.sub, block, typ, zero, condition_integer)
	difference := p.instr2(.xor, block, typ, if_true, if_false)
	masked := p.instr2(.and_, block, typ, difference, mask)
	return p.instr2(.xor, block, typ, if_false, masked)
}

fn (mut p FastArm64Program) struct_field_ptr(block ssa.BlockID, base ssa.ValueID, typ ssa.TypeID, field int) ssa.ValueID {
	offset := p.m.get_or_add_const(p.i64_type, p.m.struct_field_offset(typ, field).str())
	field_type := p.m.type_store.types[typ].fields[field]
	return p.instr2(.get_element_ptr, block, p.m.type_store.get_ptr(field_type), base, offset)
}

fn (mut p FastArm64Program) string_field_ptr(block ssa.BlockID, base ssa.ValueID, field int) ssa.ValueID {
	return p.struct_field_ptr(block, base, p.str_type, field)
}

fn (mut p FastArm64Program) register_option_state_runtime() {
	state_ptr_type := p.m.type_store.get_ptr(p.option_state_type)
	id := p.register_function('fastc_option_state', 'fastc_option_state', state_ptr_type, false)
	entry := p.m.add_block(id, 'option_state_entry')
	key := p.instr1(.load, entry, p.u64_type, p.option_state_key_global)
	get_ref := p.m.add_value(.func_ref, p.ptr_i8, 'pthread_getspecific', p.fn_ids['pthread_getspecific'])
	current := p.m.add_instr(.call, entry, p.ptr_i8, [get_ref, key])
	null_pointer := p.m.get_or_add_const(p.ptr_i8, '0')
	has_state := p.instr2(.ne, entry, p.i1_type, current, null_pointer)
	result_slot := p.instr0(.alloca, entry, p.m.type_store.get_ptr(p.ptr_i8))
	p.instr2(.store, entry, p.void_type, current, result_slot)
	ready := p.m.add_block(id, 'option_state_ready')
	missing := p.m.add_block(id, 'option_state_missing')
	p.instr3(.br, entry, p.void_type, has_state, ssa.ValueID(ready), ssa.ValueID(missing))
	one := p.m.get_or_add_const(p.i64_type, '1')
	size := p.m.get_or_add_const(p.i64_type, p.m.type_size(p.option_state_type).str())
	calloc_ref := p.m.add_value(.func_ref, p.ptr_i8, 'calloc', p.fn_ids['calloc'])
	created := p.m.add_instr(.call, missing, p.ptr_i8, [calloc_ref, one, size])
	set_ref := p.m.add_value(.func_ref, p.i32_type, 'pthread_setspecific', p.fn_ids['pthread_setspecific'])
	p.m.add_instr(.call, missing, p.i32_type, [set_ref, key, created])
	p.instr2(.store, missing, p.void_type, created, result_slot)
	p.instr1(.jmp, missing, p.void_type, ssa.ValueID(ready))
	result := p.instr1(.load, ready, p.ptr_i8, result_slot)
	typed_result := p.instr1(.bitcast, ready, state_ptr_type, result)
	p.instr1(.ret, ready, p.void_type, typed_result)
	mut function := p.m.funcs[id]
	function.is_prototype = false
	function.is_c_extern = false
	p.m.funcs[id] = function
}

fn (mut p FastArm64Program) register_arguments_runtime() {
	mut id := -1
	for key, signature in p.functions {
		if signature.is_disabled || key.starts_with('C.') {
			continue
		}
		symbol := fastc_c_function_name_for_key(key)
		if symbol == 'builtin__arguments' {
			id = p.register_signature_function(key) or { -1 }
			break
		}
	}
	if id < 0 {
		id = p.register_function('arguments', 'arguments', p.array_type, false)
	}
	p.fn_ids['arguments'] = id
	p.fn_returns['arguments'] = p.array_type
	p.fn_symbols['arguments'] = p.m.funcs[id].name
	entry := p.m.add_block(id, 'arguments_entry')
	argc_global := p.main_argc_global
	argv_global := p.main_argv_global
	ptr_ptr_i8 := p.m.type_store.get_ptr(p.ptr_i8)
	ptr_array := p.m.type_store.get_ptr(p.array_type)
	ptr_string := p.m.type_store.get_ptr(p.str_type)
	ptr_i64 := p.m.type_store.get_ptr(p.i64_type)
	argc := p.instr1(.load, entry, p.i64_type, argc_global)
	argv := p.instr1(.load, entry, ptr_ptr_i8, argv_global)
	zero64 := p.m.get_or_add_const(p.i64_type, '0')
	one64 := p.m.get_or_add_const(p.i64_type, '1')
	eight64 := p.m.get_or_add_const(p.i64_type, '8')
	sixteen64 := p.m.get_or_add_const(p.i64_type, '16')
	one32 := p.m.get_or_add_const(p.i32_type, '1')
	new_ref := p.m.add_value(.func_ref, p.array_type, 'fast_array_new', p.fn_ids['fast_array_new'])
	array_value := p.m.add_instr(.call, entry, p.array_type, [new_ref, sixteen64, argc, argc])
	array_slot := p.instr0(.alloca, entry, ptr_array)
	p.instr2(.store, entry, p.void_type, array_value, array_slot)
	data := p.instr1(.load, entry, p.ptr_i8, p.struct_field_ptr(entry, array_slot, p.array_type, 0))
	index_slot := p.instr0(.alloca, entry, ptr_i64)
	p.instr2(.store, entry, p.void_type, zero64, index_slot)
	condition := p.m.add_block(id, 'arguments_condition')
	body := p.m.add_block(id, 'arguments_body')
	done := p.m.add_block(id, 'arguments_done')
	p.instr1(.jmp, entry, p.void_type, ssa.ValueID(condition))
	index := p.instr1(.load, condition, p.i64_type, index_slot)
	more := p.instr2(.lt, condition, p.i1_type, index, argc)
	p.instr3(.br, condition, p.void_type, more, ssa.ValueID(body), ssa.ValueID(done))
	argv_offset := p.instr2(.mul, body, p.i64_type, index, eight64)
	argv_address := p.instr2(.add, body, ptr_ptr_i8, argv, argv_offset)
	c_string := p.instr1(.load, body, p.ptr_i8, argv_address)
	strlen_ref := p.m.add_value(.func_ref, p.i64_type, 'strlen', p.fn_ids['strlen'])
	string_length64 := p.m.add_instr(.call, body, p.i64_type, [strlen_ref, c_string])
	string_length := p.instr1(.trunc, body, p.i32_type, string_length64)
	string_slot := p.instr0(.alloca, body, ptr_string)
	p.instr2(.store, body, p.void_type, c_string, p.string_field_ptr(body, string_slot, 0))
	p.instr2(.store, body, p.void_type, string_length, p.string_field_ptr(body, string_slot, 1))
	p.instr2(.store, body, p.void_type, one32, p.string_field_ptr(body, string_slot, 2))
	string_value := p.instr1(.load, body, p.str_type, string_slot)
	data_offset := p.instr2(.mul, body, p.i64_type, index, sixteen64)
	destination := p.instr2(.add, body, p.ptr_i8, data, data_offset)
	destination_string := p.instr1(.bitcast, body, ptr_string, destination)
	p.instr2(.store, body, p.void_type, string_value, destination_string)
	next_index := p.instr2(.add, body, p.i64_type, index, one64)
	p.instr2(.store, body, p.void_type, next_index, index_slot)
	p.instr1(.jmp, body, p.void_type, ssa.ValueID(condition))
	result := p.instr1(.load, done, p.array_type, array_slot)
	p.instr1(.ret, done, p.void_type, result)
	mut function := p.m.funcs[id]
	function.is_prototype = false
	function.is_c_extern = false
	p.m.funcs[id] = function
}

fn (mut p FastArm64Program) register_array_new_runtime() {
	id := p.register_function('fast_array_new', 'fast_array_new', p.array_type, false)
	entry := p.m.add_block(id, 'array_new_entry')
	invalid := p.m.add_block(id, 'array_new_invalid')
	ready := p.m.add_block(id, 'array_new_ready')
	allocate := p.m.add_block(id, 'array_new_allocate')
	done := p.m.add_block(id, 'array_new_done')
	element_size := p.add_arg(id, p.i64_type, 'element_size')
	length := p.add_arg(id, p.i64_type, 'length')
	capacity := p.add_arg(id, p.i64_type, 'capacity')
	zero64 := p.m.get_or_add_const(p.i64_type, '0')
	negative_length := p.instr2(.lt, entry, p.i1_type, length, zero64)
	negative_capacity := p.instr2(.lt, entry, p.i1_type, capacity, zero64)
	has_negative_size := p.instr2(.or_, entry, p.i1_type, negative_length, negative_capacity)
	p.instr3(.br, entry, p.void_type, has_negative_size, ssa.ValueID(invalid), ssa.ValueID(ready))
	exit_ref := p.m.add_value(.func_ref, p.void_type, 'exit', p.fn_ids['exit'])
	exit_code := p.m.get_or_add_const(p.i32_type, '1')
	p.m.add_instr(.call, invalid, p.void_type, [exit_ref, exit_code])
	p.instr0(.unreachable, invalid, p.void_type)
	capacity_is_short := p.instr2(.lt, ready, p.i1_type, capacity, length)
	normalized_capacity := p.integer_select(ready, capacity_is_short, length, capacity, p.i64_type)
	has_capacity := p.instr2(.gt, ready, p.i1_type, normalized_capacity, zero64)
	slot := p.instr0(.alloca, ready, p.m.type_store.get_ptr(p.array_type))
	null_pointer := p.m.get_or_add_const(p.ptr_i8, '0')
	p.instr2(.store, ready, p.void_type, null_pointer, p.struct_field_ptr(ready, slot, p.array_type, 0))
	zero32 := p.m.get_or_add_const(p.i32_type, '0')
	p.instr2(.store, ready, p.void_type, zero32, p.struct_field_ptr(ready, slot, p.array_type, 1))
	length32 := p.instr1(.trunc, ready, p.i32_type, length)
	capacity32 := p.instr1(.trunc, ready, p.i32_type, normalized_capacity)
	element_size32 := p.instr1(.trunc, ready, p.i32_type, element_size)
	for field, value in {
		2: length32
		3: capacity32
		4: zero32
		5: element_size32
	} {
		p.instr2(.store, ready, p.void_type, value, p.struct_field_ptr(ready, slot, p.array_type, field))
	}
	p.instr3(.br, ready, p.void_type, has_capacity, ssa.ValueID(allocate), ssa.ValueID(done))
	data_size := p.instr2(.mul, allocate, p.i64_type, normalized_capacity, element_size)
	header_size := p.m.get_or_add_const(p.i64_type, '8')
	allocation_size := p.instr2(.add, allocate, p.i64_type, data_size, header_size)
	calloc_ref := p.m.add_value(.func_ref, p.ptr_i8, 'calloc', p.fn_ids['calloc'])
	one64 := p.m.get_or_add_const(p.i64_type, '1')
	raw_data := p.m.add_instr(.call, allocate, p.ptr_i8, [calloc_ref, one64, allocation_size])
	data := p.instr2(.add, allocate, p.ptr_i8, raw_data, header_size)
	p.instr2(.store, allocate, p.void_type, data, p.struct_field_ptr(allocate, slot, p.array_type, 0))
	managed32 := p.m.get_or_add_const(p.i32_type, '16')
	p.instr2(.store, allocate, p.void_type, managed32, p.struct_field_ptr(allocate, slot, p.array_type, 4))
	p.instr1(.jmp, allocate, p.void_type, ssa.ValueID(done))
	result := p.instr1(.load, done, p.array_type, slot)
	p.instr1(.ret, done, p.void_type, result)
}

fn (mut p FastArm64Program) register_array_buffer_runtime() {
	mark_id := p.register_function('fast_array_mark_has_slices', 'fast_array_mark_has_slices', p.void_type, false)
	mark_entry := p.m.add_block(mark_id, 'array_mark_slices_entry')
	mark_buffer := p.m.add_block(mark_id, 'array_mark_slices_buffer')
	mark_done := p.m.add_block(mark_id, 'array_mark_slices_done')
	mark_array := p.add_arg(mark_id, p.array_type, 'array_value')
	mark_slot := p.instr0(.alloca, mark_entry, p.m.type_store.get_ptr(p.array_type))
	p.instr2(.store, mark_entry, p.void_type, mark_array, mark_slot)
	mark_managed := p.array_is_managed(mark_entry, mark_slot)
	mark_data := p.instr1(.load, mark_entry, p.ptr_i8, p.struct_field_ptr(mark_entry, mark_slot, p.array_type, 0))
	zero_pointer := p.m.get_or_add_const(p.ptr_i8, '0')
	mark_has_data := p.instr2(.ne, mark_entry, p.i1_type, mark_data, zero_pointer)
	mark_ready := p.instr2(.and_, mark_entry, p.i1_type, mark_managed, mark_has_data)
	p.instr3(.br, mark_entry, p.void_type, mark_ready, ssa.ValueID(mark_buffer), ssa.ValueID(mark_done))
	mark_header := p.array_data_header(mark_buffer, mark_slot)
	one8 := p.m.get_or_add_const(p.i8_type, '1')
	p.instr2(.store, mark_buffer, p.void_type, one8, mark_header)
	p.instr1(.jmp, mark_buffer, p.void_type, ssa.ValueID(mark_done))
	p.instr0(.ret, mark_done, p.void_type)

	has_id := p.register_function('fast_array_buffer_has_slices', 'fast_array_buffer_has_slices', p.i1_type, false)
	has_entry := p.m.add_block(has_id, 'array_has_slices_entry')
	has_buffer := p.m.add_block(has_id, 'array_has_slices_buffer')
	has_none := p.m.add_block(has_id, 'array_has_slices_none')
	has_array := p.add_arg(has_id, p.array_type, 'array_value')
	has_slot := p.instr0(.alloca, has_entry, p.m.type_store.get_ptr(p.array_type))
	p.instr2(.store, has_entry, p.void_type, has_array, has_slot)
	has_managed := p.array_is_managed(has_entry, has_slot)
	has_data := p.instr1(.load, has_entry, p.ptr_i8, p.struct_field_ptr(has_entry, has_slot, p.array_type, 0))
	has_has_data := p.instr2(.ne, has_entry, p.i1_type, has_data, zero_pointer)
	has_ready := p.instr2(.and_, has_entry, p.i1_type, has_managed, has_has_data)
	p.instr3(.br, has_entry, p.void_type, has_ready, ssa.ValueID(has_buffer), ssa.ValueID(has_none))
	header := p.array_data_header(has_buffer, has_slot)
	marked := p.instr1(.load, has_buffer, p.i8_type, header)
	zero8 := p.m.get_or_add_const(p.i8_type, '0')
	has_slices := p.instr2(.ne, has_buffer, p.i1_type, marked, zero8)
	p.instr1(.ret, has_buffer, p.void_type, has_slices)
	false_value := p.m.get_or_add_const(p.i1_type, '0')
	p.instr1(.ret, has_none, p.void_type, false_value)
}

fn (mut p FastArm64Program) array_is_managed(block ssa.BlockID, array_slot ssa.ValueID) ssa.ValueID {
	flags := p.instr1(.load, block, p.i32_type, p.struct_field_ptr(block, array_slot, p.array_type, 4))
	managed_flag := p.m.get_or_add_const(p.i32_type, '16')
	managed_flags := p.instr2(.and_, block, p.i32_type, flags, managed_flag)
	zero32 := p.m.get_or_add_const(p.i32_type, '0')
	return p.instr2(.ne, block, p.i1_type, managed_flags, zero32)
}

fn (mut p FastArm64Program) array_data_header(block ssa.BlockID, array_slot ssa.ValueID) ssa.ValueID {
	data := p.instr1(.load, block, p.ptr_i8, p.struct_field_ptr(block, array_slot, p.array_type, 0))
	offset32 := p.instr1(.load, block, p.i32_type, p.struct_field_ptr(block, array_slot, p.array_type, 1))
	byte_offset := p.instr1(.zext, block, p.i64_type, offset32)
	header_size := p.m.get_or_add_const(p.i64_type, '8')
	data_address := p.instr1(.bitcast, block, p.u64_type, data)
	base_address := p.instr2(.sub, block, p.u64_type, data_address, byte_offset)
	header_address := p.instr2(.sub, block, p.u64_type, base_address, header_size)
	return p.instr1(.bitcast, block, p.ptr_i8, header_address)
}

fn (mut p FastArm64Program) register_map_runtime() {
	p.register_map_key_equal_runtime()
	p.register_map_hash_runtime()
	p.register_map_rehash_runtime()
	p.register_map_find_runtime()
	p.register_map_new_runtime()
	p.register_map_get_runtime()
	p.register_map_set_runtime()
	p.register_map_delete_runtime()
	p.register_map_clone_runtime()
}

fn (mut p FastArm64Program) register_map_key_equal_runtime() {
	id := p.register_function('fast_map_key_equal', 'fast_map_key_equal', p.i1_type, false)
	entry := p.m.add_block(id, 'map_key_equal_entry')
	string_block := p.m.add_block(id, 'map_key_equal_string')
	raw_block := p.m.add_block(id, 'map_key_equal_raw')
	false_block := p.m.add_block(id, 'map_key_equal_false')
	compare_string_block := p.m.add_block(id, 'map_key_equal_compare_string')
	stored := p.add_arg(id, p.ptr_i8, 'stored')
	query := p.add_arg(id, p.ptr_i8, 'query')
	key_size := p.add_arg(id, p.i64_type, 'key_size')
	string_key := p.add_arg(id, p.i64_type, 'string_key')
	zero64 := p.m.get_or_add_const(p.i64_type, '0')
	zero32 := p.m.get_or_add_const(p.i32_type, '0')
	is_string := p.instr2(.ne, entry, p.i1_type, string_key, zero64)
	p.instr3(.br, entry, p.void_type, is_string, ssa.ValueID(string_block), ssa.ValueID(raw_block))
	ptr_string := p.m.type_store.get_ptr(p.str_type)
	stored_string := p.instr1(.bitcast, string_block, ptr_string, stored)
	query_string := p.instr1(.bitcast, string_block, ptr_string, query)
	stored_len := p.instr1(.load, string_block, p.i32_type, p.string_field_ptr(string_block, stored_string, 1))
	query_len := p.instr1(.load, string_block, p.i32_type, p.string_field_ptr(string_block, query_string, 1))
	lengths_equal := p.instr2(.eq, string_block, p.i1_type, stored_len, query_len)
	p.instr3(.br, string_block, p.void_type, lengths_equal, ssa.ValueID(compare_string_block), ssa.ValueID(false_block))
	stored_data := p.instr1(.load, compare_string_block, p.ptr_i8, p.string_field_ptr(compare_string_block, stored_string, 0))
	query_data := p.instr1(.load, compare_string_block, p.ptr_i8, p.string_field_ptr(compare_string_block, query_string, 0))
	length64 := p.instr1(.zext, compare_string_block, p.i64_type, stored_len)
	memcmp_ref := p.m.add_value(.func_ref, p.i32_type, 'memcmp', p.fn_ids['memcmp'])
	string_order := p.m.add_instr(.call, compare_string_block, p.i32_type, [
		memcmp_ref,
		stored_data,
		query_data,
		length64,
	])
	string_equal := p.instr2(.eq, compare_string_block, p.i1_type, string_order, zero32)
	p.instr1(.ret, compare_string_block, p.void_type, string_equal)
	raw_order := p.m.add_instr(.call, raw_block, p.i32_type, [memcmp_ref, stored, query, key_size])
	raw_equal := p.instr2(.eq, raw_block, p.i1_type, raw_order, zero32)
	p.instr1(.ret, raw_block, p.void_type, raw_equal)
	false_value := p.m.get_or_add_const(p.i1_type, '0')
	p.instr1(.ret, false_block, p.void_type, false_value)
}

fn (mut p FastArm64Program) register_map_hash_runtime() {
	id := p.register_function('fast_map_hash', 'fast_map_hash', p.i64_type, false)
	entry := p.m.add_block(id, 'map_hash_entry')
	string_block := p.m.add_block(id, 'map_hash_string')
	raw_block := p.m.add_block(id, 'map_hash_raw')
	loop_start := p.m.add_block(id, 'map_hash_loop_start')
	condition := p.m.add_block(id, 'map_hash_condition')
	body := p.m.add_block(id, 'map_hash_body')
	done := p.m.add_block(id, 'map_hash_done')
	key := p.add_arg(id, p.ptr_i8, 'key')
	key_size := p.add_arg(id, p.i64_type, 'key_size')
	string_key := p.add_arg(id, p.i64_type, 'string_key')
	data_slot := p.instr0(.alloca, entry, p.m.type_store.get_ptr(p.ptr_i8))
	count_slot := p.instr0(.alloca, entry, p.m.type_store.get_ptr(p.i64_type))
	hash_slot := p.instr0(.alloca, entry, p.m.type_store.get_ptr(p.i64_type))
	index_slot := p.instr0(.alloca, entry, p.m.type_store.get_ptr(p.i64_type))
	zero := p.m.get_or_add_const(p.i64_type, '0')
	is_string := p.instr2(.ne, entry, p.i1_type, string_key, zero)
	p.instr3(.br, entry, p.void_type, is_string, ssa.ValueID(string_block), ssa.ValueID(raw_block))
	string_pointer := p.instr1(.bitcast, string_block, p.m.type_store.get_ptr(p.str_type), key)
	string_data := p.instr1(.load, string_block, p.ptr_i8, p.string_field_ptr(string_block, string_pointer, 0))
	string_length32 := p.instr1(.load, string_block, p.i32_type, p.string_field_ptr(string_block, string_pointer, 1))
	string_length := p.instr1(.zext, string_block, p.i64_type, string_length32)
	p.instr2(.store, string_block, p.void_type, string_data, data_slot)
	p.instr2(.store, string_block, p.void_type, string_length, count_slot)
	p.instr1(.jmp, string_block, p.void_type, ssa.ValueID(loop_start))
	p.instr2(.store, raw_block, p.void_type, key, data_slot)
	p.instr2(.store, raw_block, p.void_type, key_size, count_slot)
	p.instr1(.jmp, raw_block, p.void_type, ssa.ValueID(loop_start))
	offset_basis := p.m.get_or_add_const(p.i64_type, '1469598103934665603')
	p.instr2(.store, loop_start, p.void_type, offset_basis, hash_slot)
	p.instr2(.store, loop_start, p.void_type, zero, index_slot)
	p.instr1(.jmp, loop_start, p.void_type, ssa.ValueID(condition))
	index := p.instr1(.load, condition, p.i64_type, index_slot)
	count := p.instr1(.load, condition, p.i64_type, count_slot)
	more := p.instr2(.lt, condition, p.i1_type, index, count)
	p.instr3(.br, condition, p.void_type, more, ssa.ValueID(body), ssa.ValueID(done))
	data := p.instr1(.load, body, p.ptr_i8, data_slot)
	byte_address := p.instr2(.add, body, p.ptr_i8, data, index)
	byte := p.instr1(.load, body, p.u8_type, byte_address)
	byte64 := p.instr1(.zext, body, p.i64_type, byte)
	current_hash := p.instr1(.load, body, p.i64_type, hash_slot)
	mixed := p.instr2(.xor, body, p.i64_type, current_hash, byte64)
	prime := p.m.get_or_add_const(p.i64_type, '1099511628211')
	next_hash := p.instr2(.mul, body, p.i64_type, mixed, prime)
	p.instr2(.store, body, p.void_type, next_hash, hash_slot)
	one := p.m.get_or_add_const(p.i64_type, '1')
	next_index := p.instr2(.add, body, p.i64_type, index, one)
	p.instr2(.store, body, p.void_type, next_index, index_slot)
	p.instr1(.jmp, body, p.void_type, ssa.ValueID(condition))
	result := p.instr1(.load, done, p.i64_type, hash_slot)
	p.instr1(.ret, done, p.void_type, result)
}

fn (mut p FastArm64Program) register_map_rehash_runtime() {
	id := p.register_function('fast_map_rehash', 'fast_map_rehash', p.void_type, false)
	entry := p.m.add_block(id, 'map_rehash_entry')
	condition := p.m.add_block(id, 'map_rehash_condition')
	body := p.m.add_block(id, 'map_rehash_body')
	done := p.m.add_block(id, 'map_rehash_done')
	state_type := p.m.type_store.get_ptr(p.map_state_type)
	state := p.add_arg(id, state_type, 'state')
	capacity := p.instr1(.load, entry, p.i64_type, p.struct_field_ptr(entry, state, p.map_state_type, 2))
	length := p.instr1(.load, entry, p.i64_type, p.struct_field_ptr(entry, state, p.map_state_type, 3))
	key_size := p.instr1(.load, entry, p.i64_type, p.struct_field_ptr(entry, state, p.map_state_type, 4))
	string_key := p.instr1(.load, entry, p.i64_type, p.struct_field_ptr(entry, state, p.map_state_type, 6))
	old_buckets := p.instr1(.load, entry, p.ptr_i8, p.struct_field_ptr(entry, state, p.map_state_type, 7))
	two := p.m.get_or_add_const(p.i64_type, '2')
	eight := p.m.get_or_add_const(p.i64_type, '8')
	bucket_capacity := p.instr2(.mul, entry, p.i64_type, capacity, two)
	calloc_ref := p.m.add_value(.func_ref, p.ptr_i8, 'calloc', p.fn_ids['calloc'])
	buckets := p.m.add_instr(.call, entry, p.ptr_i8, [calloc_ref, bucket_capacity, eight])
	next_bytes := p.instr2(.mul, entry, p.i64_type, capacity, eight)
	old_next := p.instr1(.load, entry, p.ptr_i8, p.struct_field_ptr(entry, state, p.map_state_type, 8))
	realloc_ref := p.m.add_value(.func_ref, p.ptr_i8, 'realloc', p.fn_ids['realloc'])
	next_entries := p.m.add_instr(.call, entry, p.ptr_i8, [realloc_ref, old_next, next_bytes])
	p.instr2(.store, entry, p.void_type, buckets, p.struct_field_ptr(entry, state, p.map_state_type, 7))
	p.instr2(.store, entry, p.void_type, next_entries, p.struct_field_ptr(entry, state, p.map_state_type, 8))
	p.instr2(.store, entry, p.void_type, bucket_capacity, p.struct_field_ptr(entry, state, p.map_state_type, 9))
	index_slot := p.instr0(.alloca, entry, p.m.type_store.get_ptr(p.i64_type))
	zero := p.m.get_or_add_const(p.i64_type, '0')
	p.instr2(.store, entry, p.void_type, zero, index_slot)
	p.instr1(.jmp, entry, p.void_type, ssa.ValueID(condition))
	index := p.instr1(.load, condition, p.i64_type, index_slot)
	more := p.instr2(.lt, condition, p.i1_type, index, length)
	p.instr3(.br, condition, p.void_type, more, ssa.ValueID(body), ssa.ValueID(done))
	keys := p.instr1(.load, body, p.ptr_i8, p.struct_field_ptr(body, state, p.map_state_type, 0))
	key_offset := p.instr2(.mul, body, p.i64_type, index, key_size)
	key := p.instr2(.add, body, p.ptr_i8, keys, key_offset)
	hash_ref := p.m.add_value(.func_ref, p.i64_type, 'fast_map_hash', p.fn_ids['fast_map_hash'])
	hash := p.m.add_instr(.call, body, p.i64_type, [hash_ref, key, key_size, string_key])
	one := p.m.get_or_add_const(p.i64_type, '1')
	mask := p.instr2(.sub, body, p.i64_type, bucket_capacity, one)
	bucket_index := p.instr2(.and_, body, p.i64_type, hash, mask)
	bucket_offset := p.instr2(.mul, body, p.i64_type, bucket_index, eight)
	bucket_bytes := p.instr2(.add, body, p.ptr_i8, buckets, bucket_offset)
	bucket_pointer := p.instr1(.bitcast, body, p.m.type_store.get_ptr(p.i64_type), bucket_bytes)
	head := p.instr1(.load, body, p.i64_type, bucket_pointer)
	next_offset := p.instr2(.mul, body, p.i64_type, index, eight)
	next_bytes_pointer := p.instr2(.add, body, p.ptr_i8, next_entries, next_offset)
	next_pointer := p.instr1(.bitcast, body, p.m.type_store.get_ptr(p.i64_type), next_bytes_pointer)
	p.instr2(.store, body, p.void_type, head, next_pointer)
	entry_value := p.instr2(.add, body, p.i64_type, index, one)
	p.instr2(.store, body, p.void_type, entry_value, bucket_pointer)
	p.instr2(.store, body, p.void_type, entry_value, index_slot)
	p.instr1(.jmp, body, p.void_type, ssa.ValueID(condition))
	free_ref := p.m.add_value(.func_ref, p.void_type, 'free', p.fn_ids['free'])
	p.m.add_instr(.call, done, p.void_type, [free_ref, old_buckets])
	p.instr0(.ret, done, p.void_type)
}

fn (mut p FastArm64Program) register_map_find_runtime() {
	id := p.register_function('fast_map_find', 'fast_map_find', p.i64_type, false)
	entry := p.m.add_block(id, 'map_find_entry')
	ready := p.m.add_block(id, 'map_find_ready')
	condition := p.m.add_block(id, 'map_find_condition')
	body := p.m.add_block(id, 'map_find_body')
	found := p.m.add_block(id, 'map_find_found')
	next := p.m.add_block(id, 'map_find_next')
	missing := p.m.add_block(id, 'map_find_missing')
	map_value := p.add_arg(id, p.map_type, 'map_value')
	key := p.add_arg(id, p.ptr_i8, 'key')
	map_slot := p.instr0(.alloca, entry, p.m.type_store.get_ptr(p.map_type))
	entry_slot := p.instr0(.alloca, entry, p.m.type_store.get_ptr(p.i64_type))
	p.instr2(.store, entry, p.void_type, map_value, map_slot)
	state := p.instr1(.load, entry, p.m.type_store.get_ptr(p.map_state_type), p.struct_field_ptr(entry, map_slot, p.map_type, 0))
	zero := p.m.get_or_add_const(p.i64_type, '0')
	one := p.m.get_or_add_const(p.i64_type, '1')
	state_bytes := p.instr1(.bitcast, entry, p.ptr_i8, state)
	nonempty := p.instr2(.ne, entry, p.i1_type, state_bytes, p.m.get_or_add_const(p.ptr_i8, '0'))
	p.instr3(.br, entry, p.void_type, nonempty, ssa.ValueID(ready), ssa.ValueID(missing))
	keys := p.instr1(.load, ready, p.ptr_i8, p.struct_field_ptr(ready, state, p.map_state_type, 0))
	key_size := p.instr1(.load, ready, p.i64_type, p.struct_field_ptr(ready, state, p.map_state_type, 4))
	string_key := p.instr1(.load, ready, p.i64_type, p.struct_field_ptr(ready, state, p.map_state_type, 6))
	buckets := p.instr1(.load, ready, p.ptr_i8, p.struct_field_ptr(ready, state, p.map_state_type, 7))
	next_entries := p.instr1(.load, ready, p.ptr_i8, p.struct_field_ptr(ready, state, p.map_state_type, 8))
	bucket_capacity := p.instr1(.load, ready, p.i64_type, p.struct_field_ptr(ready, state, p.map_state_type, 9))
	hash_ref := p.m.add_value(.func_ref, p.i64_type, 'fast_map_hash', p.fn_ids['fast_map_hash'])
	hash := p.m.add_instr(.call, ready, p.i64_type, [hash_ref, key, key_size, string_key])
	mask := p.instr2(.sub, ready, p.i64_type, bucket_capacity, one)
	bucket_index := p.instr2(.and_, ready, p.i64_type, hash, mask)
	eight := p.m.get_or_add_const(p.i64_type, '8')
	bucket_offset := p.instr2(.mul, ready, p.i64_type, bucket_index, eight)
	bucket_bytes := p.instr2(.add, ready, p.ptr_i8, buckets, bucket_offset)
	bucket_pointer := p.instr1(.bitcast, ready, p.m.type_store.get_ptr(p.i64_type), bucket_bytes)
	first_entry := p.instr1(.load, ready, p.i64_type, bucket_pointer)
	p.instr2(.store, ready, p.void_type, first_entry, entry_slot)
	p.instr1(.jmp, ready, p.void_type, ssa.ValueID(condition))
	current_entry := p.instr1(.load, condition, p.i64_type, entry_slot)
	more := p.instr2(.gt, condition, p.i1_type, current_entry, zero)
	p.instr3(.br, condition, p.void_type, more, ssa.ValueID(body), ssa.ValueID(missing))
	index := p.instr2(.sub, body, p.i64_type, current_entry, one)
	offset := p.instr2(.mul, body, p.i64_type, index, key_size)
	stored := p.instr2(.add, body, p.ptr_i8, keys, offset)
	equal_ref := p.m.add_value(.func_ref, p.i1_type, 'fast_map_key_equal', p.fn_ids['fast_map_key_equal'])
	equal := p.m.add_instr(.call, body, p.i1_type, [equal_ref, stored, key, key_size, string_key])
	p.instr3(.br, body, p.void_type, equal, ssa.ValueID(found), ssa.ValueID(next))
	p.instr1(.ret, found, p.void_type, index)
	next_offset := p.instr2(.mul, next, p.i64_type, index, eight)
	next_bytes := p.instr2(.add, next, p.ptr_i8, next_entries, next_offset)
	next_pointer := p.instr1(.bitcast, next, p.m.type_store.get_ptr(p.i64_type), next_bytes)
	next_entry := p.instr1(.load, next, p.i64_type, next_pointer)
	p.instr2(.store, next, p.void_type, next_entry, entry_slot)
	p.instr1(.jmp, next, p.void_type, ssa.ValueID(condition))
	minus_one := p.m.get_or_add_const(p.i64_type, '-1')
	p.instr1(.ret, missing, p.void_type, minus_one)
}

fn (mut p FastArm64Program) register_map_new_runtime() {
	id := p.register_function('fast_map_new', 'fast_map_new', p.map_type, false)
	entry := p.m.add_block(id, 'map_new_entry')
	key_size := p.add_arg(id, p.i64_type, 'key_size')
	val_size := p.add_arg(id, p.i64_type, 'val_size')
	string_key := p.add_arg(id, p.i64_type, 'string_key')
	default_value := p.add_arg(id, p.ptr_i8, 'default_value')
	eight := p.m.get_or_add_const(p.i64_type, '8')
	zero := p.m.get_or_add_const(p.i64_type, '0')
	one := p.m.get_or_add_const(p.i64_type, '1')
	state_size := p.m.get_or_add_const(p.i64_type, p.m.type_size(p.map_state_type).str())
	calloc_ref := p.m.add_value(.func_ref, p.ptr_i8, 'calloc', p.fn_ids['calloc'])
	state_bytes := p.m.add_instr(.call, entry, p.ptr_i8, [calloc_ref, one, state_size])
	state := p.instr1(.bitcast, entry, p.m.type_store.get_ptr(p.map_state_type), state_bytes)
	keys := p.m.add_instr(.call, entry, p.ptr_i8, [calloc_ref, eight, key_size])
	vals := p.m.add_instr(.call, entry, p.ptr_i8, [calloc_ref, eight, val_size])
	zero_value := p.m.add_instr(.call, entry, p.ptr_i8, [calloc_ref, one, val_size])
	memcpy_ref := p.m.add_value(.func_ref, p.ptr_i8, 'memcpy', p.fn_ids['memcpy'])
	p.m.add_instr(.call, entry, p.ptr_i8, [memcpy_ref, zero_value, default_value, val_size])
	for field, value in [keys, vals, eight, zero, key_size, val_size, string_key] {
		p.instr2(.store, entry, p.void_type, value, p.struct_field_ptr(entry, state, p.map_state_type, field))
	}
	p.instr2(.store, entry, p.void_type, zero_value, p.struct_field_ptr(entry, state, p.map_state_type, 10))
	rehash_ref := p.m.add_value(.func_ref, p.void_type, 'fast_map_rehash', p.fn_ids['fast_map_rehash'])
	p.m.add_instr(.call, entry, p.void_type, [rehash_ref, state])
	map_slot := p.instr0(.alloca, entry, p.m.type_store.get_ptr(p.map_type))
	p.instr2(.store, entry, p.void_type, state, p.struct_field_ptr(entry, map_slot, p.map_type, 0))
	result := p.instr1(.load, entry, p.map_type, map_slot)
	p.instr1(.ret, entry, p.void_type, result)
}

fn (mut p FastArm64Program) register_map_get_runtime() {
	id := p.register_function('fast_map_get', 'fast_map_get', p.ptr_i8, false)
	entry := p.m.add_block(id, 'map_get_entry')
	found := p.m.add_block(id, 'map_get_found')
	missing := p.m.add_block(id, 'map_get_missing')
	missing_state := p.m.add_block(id, 'map_get_missing_state')
	missing_empty := p.m.add_block(id, 'map_get_missing_empty')
	map_value := p.add_arg(id, p.map_type, 'map_value')
	key := p.add_arg(id, p.ptr_i8, 'key')
	empty_value := p.add_arg(id, p.ptr_i8, 'empty_value')
	empty_value_size := p.add_arg(id, p.i64_type, 'empty_value_size')
	map_slot := p.instr0(.alloca, entry, p.m.type_store.get_ptr(p.map_type))
	p.instr2(.store, entry, p.void_type, map_value, map_slot)
	state := p.instr1(.load, entry, p.m.type_store.get_ptr(p.map_state_type), p.struct_field_ptr(entry, map_slot, p.map_type, 0))
	find_ref := p.m.add_value(.func_ref, p.i64_type, 'fast_map_find', p.fn_ids['fast_map_find'])
	index := p.m.add_instr(.call, entry, p.i64_type, [find_ref, map_value, key])
	zero := p.m.get_or_add_const(p.i64_type, '0')
	is_found := p.instr2(.ge, entry, p.i1_type, index, zero)
	p.instr3(.br, entry, p.void_type, is_found, ssa.ValueID(found), ssa.ValueID(missing))
	vals := p.instr1(.load, found, p.ptr_i8, p.struct_field_ptr(found, state, p.map_state_type, 1))
	val_size := p.instr1(.load, found, p.i64_type, p.struct_field_ptr(found, state, p.map_state_type, 5))
	offset := p.instr2(.mul, found, p.i64_type, index, val_size)
	result := p.instr2(.add, found, p.ptr_i8, vals, offset)
	p.instr1(.ret, found, p.void_type, result)
	state_bytes := p.instr1(.bitcast, missing, p.ptr_i8, state)
	has_state := p.instr2(.ne, missing, p.i1_type, state_bytes, p.m.get_or_add_const(p.ptr_i8, '0'))
	p.instr3(.br, missing, p.void_type, has_state, ssa.ValueID(missing_state), ssa.ValueID(missing_empty))
	missing_value := p.instr1(.load, missing_state, p.ptr_i8, p.struct_field_ptr(missing_state, state, p.map_state_type, 10))
	p.instr1(.ret, missing_state, p.void_type, missing_value)
	memset_ref := p.m.add_value(.func_ref, p.ptr_i8, 'memset', p.fn_ids['memset'])
	zero_byte := p.m.get_or_add_const(p.i32_type, '0')
	p.m.add_instr(.call, missing_empty, p.ptr_i8, [memset_ref, empty_value, zero_byte,
		empty_value_size])
	p.instr1(.ret, missing_empty, p.void_type, empty_value)
}

fn (mut p FastArm64Program) register_map_set_runtime() {
	id := p.register_function('fast_map_set', 'fast_map_set', p.void_type, false)
	entry := p.m.add_block(id, 'map_set_entry')
	existing := p.m.add_block(id, 'map_set_existing')
	append_check := p.m.add_block(id, 'map_set_append_check')
	grow := p.m.add_block(id, 'map_set_grow')
	append := p.m.add_block(id, 'map_set_append')
	done := p.m.add_block(id, 'map_set_done')
	map_value := p.add_arg(id, p.map_type, 'map_value')
	key := p.add_arg(id, p.ptr_i8, 'key')
	value := p.add_arg(id, p.ptr_i8, 'value')
	map_slot := p.instr0(.alloca, entry, p.m.type_store.get_ptr(p.map_type))
	p.instr2(.store, entry, p.void_type, map_value, map_slot)
	state := p.instr1(.load, entry, p.m.type_store.get_ptr(p.map_state_type), p.struct_field_ptr(entry, map_slot, p.map_type, 0))
	find_ref := p.m.add_value(.func_ref, p.i64_type, 'fast_map_find', p.fn_ids['fast_map_find'])
	index := p.m.add_instr(.call, entry, p.i64_type, [find_ref, map_value, key])
	zero := p.m.get_or_add_const(p.i64_type, '0')
	is_existing := p.instr2(.ge, entry, p.i1_type, index, zero)
	p.instr3(.br, entry, p.void_type, is_existing, ssa.ValueID(existing), ssa.ValueID(append_check))
	vals := p.instr1(.load, existing, p.ptr_i8, p.struct_field_ptr(existing, state, p.map_state_type, 1))
	val_size := p.instr1(.load, existing, p.i64_type, p.struct_field_ptr(existing, state, p.map_state_type, 5))
	existing_offset := p.instr2(.mul, existing, p.i64_type, index, val_size)
	existing_destination := p.instr2(.add, existing, p.ptr_i8, vals, existing_offset)
	memcpy_ref := p.m.add_value(.func_ref, p.ptr_i8, 'memcpy', p.fn_ids['memcpy'])
	p.m.add_instr(.call, existing, p.ptr_i8, [memcpy_ref, existing_destination, value, val_size])
	generation_ptr := p.struct_field_ptr(existing, state, p.map_state_type, 11)
	generation := p.instr1(.load, existing, p.i64_type, generation_ptr)
	one := p.m.get_or_add_const(p.i64_type, '1')
	next_generation := p.instr2(.add, existing, p.i64_type, generation, one)
	p.instr2(.store, existing, p.void_type, next_generation, generation_ptr)
	p.instr1(.jmp, existing, p.void_type, ssa.ValueID(done))
	length := p.instr1(.load, append_check, p.i64_type, p.struct_field_ptr(append_check, state, p.map_state_type, 3))
	capacity := p.instr1(.load, append_check, p.i64_type, p.struct_field_ptr(append_check, state, p.map_state_type, 2))
	is_full := p.instr2(.ge, append_check, p.i1_type, length, capacity)
	p.instr3(.br, append_check, p.void_type, is_full, ssa.ValueID(grow), ssa.ValueID(append))
	two := p.m.get_or_add_const(p.i64_type, '2')
	new_capacity := p.instr2(.mul, grow, p.i64_type, capacity, two)
	key_size := p.instr1(.load, grow, p.i64_type, p.struct_field_ptr(grow, state, p.map_state_type, 4))
	grow_val_size := p.instr1(.load, grow, p.i64_type, p.struct_field_ptr(grow, state, p.map_state_type, 5))
	keys := p.instr1(.load, grow, p.ptr_i8, p.struct_field_ptr(grow, state, p.map_state_type, 0))
	grow_vals := p.instr1(.load, grow, p.ptr_i8, p.struct_field_ptr(grow, state, p.map_state_type, 1))
	key_bytes := p.instr2(.mul, grow, p.i64_type, new_capacity, key_size)
	val_bytes := p.instr2(.mul, grow, p.i64_type, new_capacity, grow_val_size)
	realloc_ref := p.m.add_value(.func_ref, p.ptr_i8, 'realloc', p.fn_ids['realloc'])
	new_keys := p.m.add_instr(.call, grow, p.ptr_i8, [realloc_ref, keys, key_bytes])
	new_vals := p.m.add_instr(.call, grow, p.ptr_i8, [realloc_ref, grow_vals, val_bytes])
	p.instr2(.store, grow, p.void_type, new_keys, p.struct_field_ptr(grow, state, p.map_state_type, 0))
	p.instr2(.store, grow, p.void_type, new_vals, p.struct_field_ptr(grow, state, p.map_state_type, 1))
	p.instr2(.store, grow, p.void_type, new_capacity, p.struct_field_ptr(grow, state, p.map_state_type, 2))
	rehash_ref := p.m.add_value(.func_ref, p.void_type, 'fast_map_rehash', p.fn_ids['fast_map_rehash'])
	p.m.add_instr(.call, grow, p.void_type, [rehash_ref, state])
	p.instr1(.jmp, grow, p.void_type, ssa.ValueID(append))
	append_keys := p.instr1(.load, append, p.ptr_i8, p.struct_field_ptr(append, state, p.map_state_type, 0))
	append_vals := p.instr1(.load, append, p.ptr_i8, p.struct_field_ptr(append, state, p.map_state_type, 1))
	append_key_size := p.instr1(.load, append, p.i64_type, p.struct_field_ptr(append, state, p.map_state_type, 4))
	append_val_size := p.instr1(.load, append, p.i64_type, p.struct_field_ptr(append, state, p.map_state_type, 5))
	append_string_key := p.instr1(.load, append, p.i64_type, p.struct_field_ptr(append, state, p.map_state_type, 6))
	append_buckets := p.instr1(.load, append, p.ptr_i8, p.struct_field_ptr(append, state, p.map_state_type, 7))
	append_next := p.instr1(.load, append, p.ptr_i8, p.struct_field_ptr(append, state, p.map_state_type, 8))
	append_bucket_capacity := p.instr1(.load, append, p.i64_type, p.struct_field_ptr(append, state, p.map_state_type, 9))
	key_offset := p.instr2(.mul, append, p.i64_type, length, append_key_size)
	val_offset := p.instr2(.mul, append, p.i64_type, length, append_val_size)
	key_destination := p.instr2(.add, append, p.ptr_i8, append_keys, key_offset)
	val_destination := p.instr2(.add, append, p.ptr_i8, append_vals, val_offset)
	p.m.add_instr(.call, append, p.ptr_i8, [memcpy_ref, key_destination, key, append_key_size])
	p.m.add_instr(.call, append, p.ptr_i8, [memcpy_ref, val_destination, value, append_val_size])
	eight := p.m.get_or_add_const(p.i64_type, '8')
	hash_ref := p.m.add_value(.func_ref, p.i64_type, 'fast_map_hash', p.fn_ids['fast_map_hash'])
	hash := p.m.add_instr(.call, append, p.i64_type, [hash_ref, key, append_key_size,
		append_string_key])
	mask := p.instr2(.sub, append, p.i64_type, append_bucket_capacity, one)
	bucket_index := p.instr2(.and_, append, p.i64_type, hash, mask)
	bucket_offset := p.instr2(.mul, append, p.i64_type, bucket_index, eight)
	bucket_bytes := p.instr2(.add, append, p.ptr_i8, append_buckets, bucket_offset)
	bucket_pointer := p.instr1(.bitcast, append, p.m.type_store.get_ptr(p.i64_type), bucket_bytes)
	head := p.instr1(.load, append, p.i64_type, bucket_pointer)
	next_offset := p.instr2(.mul, append, p.i64_type, length, eight)
	next_bytes := p.instr2(.add, append, p.ptr_i8, append_next, next_offset)
	next_pointer := p.instr1(.bitcast, append, p.m.type_store.get_ptr(p.i64_type), next_bytes)
	p.instr2(.store, append, p.void_type, head, next_pointer)
	new_length := p.instr2(.add, append, p.i64_type, length, one)
	p.instr2(.store, append, p.void_type, new_length, bucket_pointer)
	p.instr2(.store, append, p.void_type, new_length, p.struct_field_ptr(append, state, p.map_state_type, 3))
	append_generation_ptr := p.struct_field_ptr(append, state, p.map_state_type, 11)
	append_generation := p.instr1(.load, append, p.i64_type, append_generation_ptr)
	next_append_generation := p.instr2(.add, append, p.i64_type, append_generation, one)
	p.instr2(.store, append, p.void_type, next_append_generation, append_generation_ptr)
	p.instr1(.jmp, append, p.void_type, ssa.ValueID(done))
	p.instr0(.ret, done, p.void_type)
}

fn (mut p FastArm64Program) register_map_delete_runtime() {
	id := p.register_function('fast_map_delete', 'fast_map_delete', p.void_type, false)
	entry := p.m.add_block(id, 'map_delete_entry')
	found := p.m.add_block(id, 'map_delete_found')
	copy_last := p.m.add_block(id, 'map_delete_copy_last')
	rehash := p.m.add_block(id, 'map_delete_rehash')
	done := p.m.add_block(id, 'map_delete_done')
	map_value := p.add_arg(id, p.map_type, 'map_value')
	key := p.add_arg(id, p.ptr_i8, 'key')
	find_ref := p.m.add_value(.func_ref, p.i64_type, 'fast_map_find', p.fn_ids['fast_map_find'])
	index := p.m.add_instr(.call, entry, p.i64_type, [find_ref, map_value, key])
	zero := p.m.get_or_add_const(p.i64_type, '0')
	is_found := p.instr2(.ge, entry, p.i1_type, index, zero)
	p.instr3(.br, entry, p.void_type, is_found, ssa.ValueID(found), ssa.ValueID(done))
	map_slot := p.instr0(.alloca, found, p.m.type_store.get_ptr(p.map_type))
	p.instr2(.store, found, p.void_type, map_value, map_slot)
	state := p.instr1(.load, found, p.m.type_store.get_ptr(p.map_state_type), p.struct_field_ptr(found, map_slot, p.map_type, 0))
	length := p.instr1(.load, found, p.i64_type, p.struct_field_ptr(found, state, p.map_state_type, 3))
	one := p.m.get_or_add_const(p.i64_type, '1')
	new_length := p.instr2(.sub, found, p.i64_type, length, one)
	p.instr2(.store, found, p.void_type, new_length, p.struct_field_ptr(found, state, p.map_state_type, 3))
	generation_ptr := p.struct_field_ptr(found, state, p.map_state_type, 11)
	generation := p.instr1(.load, found, p.i64_type, generation_ptr)
	next_generation := p.instr2(.add, found, p.i64_type, generation, one)
	p.instr2(.store, found, p.void_type, next_generation, generation_ptr)
	needs_copy := p.instr2(.lt, found, p.i1_type, index, new_length)
	p.instr3(.br, found, p.void_type, needs_copy, ssa.ValueID(copy_last), ssa.ValueID(rehash))
	keys := p.instr1(.load, copy_last, p.ptr_i8, p.struct_field_ptr(copy_last, state, p.map_state_type, 0))
	values := p.instr1(.load, copy_last, p.ptr_i8, p.struct_field_ptr(copy_last, state, p.map_state_type, 1))
	key_size := p.instr1(.load, copy_last, p.i64_type, p.struct_field_ptr(copy_last, state, p.map_state_type, 4))
	value_size := p.instr1(.load, copy_last, p.i64_type, p.struct_field_ptr(copy_last, state, p.map_state_type, 5))
	key_destination_offset := p.instr2(.mul, copy_last, p.i64_type, index, key_size)
	key_source_offset := p.instr2(.mul, copy_last, p.i64_type, new_length, key_size)
	value_destination_offset := p.instr2(.mul, copy_last, p.i64_type, index, value_size)
	value_source_offset := p.instr2(.mul, copy_last, p.i64_type, new_length, value_size)
	key_destination := p.instr2(.add, copy_last, p.ptr_i8, keys, key_destination_offset)
	key_source := p.instr2(.add, copy_last, p.ptr_i8, keys, key_source_offset)
	value_destination := p.instr2(.add, copy_last, p.ptr_i8, values, value_destination_offset)
	value_source := p.instr2(.add, copy_last, p.ptr_i8, values, value_source_offset)
	memcpy_ref := p.m.add_value(.func_ref, p.ptr_i8, 'memcpy', p.fn_ids['memcpy'])
	p.m.add_instr(.call, copy_last, p.ptr_i8, [memcpy_ref, key_destination, key_source, key_size])
	p.m.add_instr(.call, copy_last, p.ptr_i8, [memcpy_ref, value_destination, value_source,
		value_size])
	p.instr1(.jmp, copy_last, p.void_type, ssa.ValueID(rehash))
	rehash_ref := p.m.add_value(.func_ref, p.void_type, 'fast_map_rehash', p.fn_ids['fast_map_rehash'])
	p.m.add_instr(.call, rehash, p.void_type, [rehash_ref, state])
	p.instr1(.jmp, rehash, p.void_type, ssa.ValueID(done))
	p.instr0(.ret, done, p.void_type)
}

fn (mut p FastArm64Program) register_map_clone_runtime() {
	id := p.register_function('fast_map_clone', 'fast_map_clone', p.map_type, false)
	entry := p.m.add_block(id, 'map_clone_entry')
	copy_map := p.m.add_block(id, 'map_clone_copy')
	empty := p.m.add_block(id, 'map_clone_empty')
	map_value := p.add_arg(id, p.map_type, 'map_value')
	map_slot := p.instr0(.alloca, entry, p.m.type_store.get_ptr(p.map_type))
	p.instr2(.store, entry, p.void_type, map_value, map_slot)
	state_ptr_type := p.m.type_store.get_ptr(p.map_state_type)
	state := p.instr1(.load, entry, state_ptr_type, p.struct_field_ptr(entry, map_slot, p.map_type, 0))
	zero_state := p.m.get_or_add_const(state_ptr_type, '0')
	has_state := p.instr2(.ne, entry, p.i1_type, state, zero_state)
	p.instr3(.br, entry, p.void_type, has_state, ssa.ValueID(copy_map), ssa.ValueID(empty))
	capacity := p.instr1(.load, copy_map, p.i64_type, p.struct_field_ptr(copy_map, state, p.map_state_type, 2))
	length := p.instr1(.load, copy_map, p.i64_type, p.struct_field_ptr(copy_map, state, p.map_state_type, 3))
	key_size := p.instr1(.load, copy_map, p.i64_type, p.struct_field_ptr(copy_map, state, p.map_state_type, 4))
	value_size := p.instr1(.load, copy_map, p.i64_type, p.struct_field_ptr(copy_map, state, p.map_state_type, 5))
	string_key := p.instr1(.load, copy_map, p.i64_type, p.struct_field_ptr(copy_map, state, p.map_state_type, 6))
	zero_value := p.instr1(.load, copy_map, p.ptr_i8, p.struct_field_ptr(copy_map, state, p.map_state_type, 10))
	new_ref := p.m.add_value(.func_ref, p.map_type, 'fast_map_new', p.fn_ids['fast_map_new'])
	result := p.m.add_instr(.call, copy_map, p.map_type, [new_ref, key_size, value_size, string_key,
		zero_value])
	result_slot := p.instr0(.alloca, copy_map, p.m.type_store.get_ptr(p.map_type))
	p.instr2(.store, copy_map, p.void_type, result, result_slot)
	result_state := p.instr1(.load, copy_map, state_ptr_type, p.struct_field_ptr(copy_map, result_slot, p.map_type, 0))
	keys := p.instr1(.load, copy_map, p.ptr_i8, p.struct_field_ptr(copy_map, state, p.map_state_type, 0))
	values := p.instr1(.load, copy_map, p.ptr_i8, p.struct_field_ptr(copy_map, state, p.map_state_type, 1))
	initial_result_keys := p.instr1(.load, copy_map, p.ptr_i8, p.struct_field_ptr(copy_map, result_state, p.map_state_type, 0))
	initial_result_values := p.instr1(.load, copy_map, p.ptr_i8, p.struct_field_ptr(copy_map, result_state, p.map_state_type, 1))
	key_capacity_bytes := p.instr2(.mul, copy_map, p.i64_type, capacity, key_size)
	value_capacity_bytes := p.instr2(.mul, copy_map, p.i64_type, capacity, value_size)
	realloc_ref := p.m.add_value(.func_ref, p.ptr_i8, 'realloc', p.fn_ids['realloc'])
	result_keys := p.m.add_instr(.call, copy_map, p.ptr_i8, [realloc_ref, initial_result_keys,
		key_capacity_bytes])
	result_values := p.m.add_instr(.call, copy_map, p.ptr_i8, [realloc_ref, initial_result_values,
		value_capacity_bytes])
	p.instr2(.store, copy_map, p.void_type, result_keys, p.struct_field_ptr(copy_map, result_state, p.map_state_type, 0))
	p.instr2(.store, copy_map, p.void_type, result_values, p.struct_field_ptr(copy_map, result_state, p.map_state_type, 1))
	p.instr2(.store, copy_map, p.void_type, capacity, p.struct_field_ptr(copy_map, result_state, p.map_state_type, 2))
	key_bytes := p.instr2(.mul, copy_map, p.i64_type, length, key_size)
	value_bytes := p.instr2(.mul, copy_map, p.i64_type, length, value_size)
	memcpy_ref := p.m.add_value(.func_ref, p.ptr_i8, 'memcpy', p.fn_ids['memcpy'])
	p.m.add_instr(.call, copy_map, p.ptr_i8, [memcpy_ref, result_keys, keys, key_bytes])
	p.m.add_instr(.call, copy_map, p.ptr_i8, [memcpy_ref, result_values, values, value_bytes])
	p.instr2(.store, copy_map, p.void_type, length, p.struct_field_ptr(copy_map, result_state, p.map_state_type, 3))
	rehash_ref := p.m.add_value(.func_ref, p.void_type, 'fast_map_rehash', p.fn_ids['fast_map_rehash'])
	p.m.add_instr(.call, copy_map, p.void_type, [rehash_ref, result_state])
	p.instr1(.ret, copy_map, p.void_type, result)
	p.instr1(.ret, empty, p.void_type, map_value)
}

fn (mut p FastArm64Program) register_print_runtime() {
	for runtime_name, newline in {
		'print':   false
		'println': true
	} {
		id := p.register_function(runtime_name, runtime_name, p.void_type, false)
		p.fn_ids['builtin.${runtime_name}'] = id
		p.fn_returns['builtin.${runtime_name}'] = p.void_type
		p.fn_symbols['builtin.${runtime_name}'] = runtime_name
		entry := p.m.add_block(id, '${runtime_name}_entry')
		message := p.add_arg(id, p.str_type, 'message')
		message_slot := p.instr0(.alloca, entry, p.m.type_store.get_ptr(p.str_type))
		p.instr2(.store, entry, p.void_type, message, message_slot)
		data_ptr := p.string_field_ptr(entry, message_slot, 0)
		len_ptr := p.string_field_ptr(entry, message_slot, 1)
		data := p.instr1(.load, entry, p.ptr_i8, data_ptr)
		len32 := p.instr1(.load, entry, p.i32_type, len_ptr)
		length := p.instr1(.zext, entry, p.i64_type, len32)
		fd := p.m.get_or_add_const(p.i64_type, '1')
		write_id := p.fn_ids['write']
		write_ref := p.m.add_value(.func_ref, p.void_type, 'write', write_id)
		p.m.add_instr(.call, entry, p.i64_type, [write_ref, fd, data, length])
		if newline {
			line := p.m.add_value(.string_literal, p.str_type, '\n', 0)
			line_slot := p.instr0(.alloca, entry, p.m.type_store.get_ptr(p.str_type))
			p.instr2(.store, entry, p.void_type, line, line_slot)
			line_data_ptr := p.string_field_ptr(entry, line_slot, 0)
			line_data := p.instr1(.load, entry, p.ptr_i8, line_data_ptr)
			one := p.m.get_or_add_const(p.i64_type, '1')
			p.m.add_instr(.call, entry, p.i64_type, [write_ref, fd, line_data, one])
		}
		p.instr0(.ret, entry, p.void_type)
	}
}

fn (mut p FastArm64Program) register_integer_string_runtime() {
	id := p.register_function('fast_i64_to_string', 'fast_i64_to_string', p.str_type, false)
	entry := p.m.add_block(id, 'i64_string_entry')
	digit := p.m.add_block(id, 'i64_string_digit')
	minus := p.m.add_block(id, 'i64_string_minus')
	done := p.m.add_block(id, 'i64_string_done')
	value := p.add_arg(id, p.i64_type, 'value')
	allocation_size := p.m.get_or_add_const(p.i64_type, '32')
	malloc_ref := p.m.add_value(.func_ref, p.ptr_i8, 'malloc', p.fn_ids['malloc'])
	buffer := p.m.add_instr(.call, entry, p.ptr_i8, [malloc_ref, allocation_size])
	last_offset := p.m.get_or_add_const(p.i64_type, '31')
	end := p.instr2(.add, entry, p.ptr_i8, buffer, last_offset)
	zero8 := p.m.get_or_add_const(p.u8_type, '0')
	p.instr2(.store, entry, p.void_type, zero8, end)
	zero64 := p.m.get_or_add_const(p.i64_type, '0')
	negative := p.instr2(.lt, entry, p.i1_type, value, zero64)
	negated := p.instr1(.neg, entry, p.i64_type, value)
	magnitude := p.integer_select(entry, negative, negated, value, p.i64_type)
	number_slot := p.instr0(.alloca, entry, p.m.type_store.get_ptr(p.i64_type))
	cursor_slot := p.instr0(.alloca, entry, p.m.type_store.get_ptr(p.ptr_i8))
	length_slot := p.instr0(.alloca, entry, p.m.type_store.get_ptr(p.i32_type))
	p.instr2(.store, entry, p.void_type, magnitude, number_slot)
	p.instr2(.store, entry, p.void_type, end, cursor_slot)
	zero32 := p.m.get_or_add_const(p.i32_type, '0')
	p.instr2(.store, entry, p.void_type, zero32, length_slot)
	p.instr1(.jmp, entry, p.void_type, ssa.ValueID(digit))

	number := p.instr1(.load, digit, p.i64_type, number_slot)
	ten := p.m.get_or_add_const(p.i64_type, '10')
	remainder := p.instr2(.urem, digit, p.i64_type, number, ten)
	quotient := p.instr2(.udiv, digit, p.i64_type, number, ten)
	digit_byte := p.instr1(.trunc, digit, p.u8_type, remainder)
	ascii_zero := p.m.get_or_add_const(p.u8_type, '48')
	ascii_digit := p.instr2(.add, digit, p.u8_type, digit_byte, ascii_zero)
	cursor := p.instr1(.load, digit, p.ptr_i8, cursor_slot)
	minus_one := p.m.get_or_add_const(p.i64_type, '-1')
	next_cursor := p.instr2(.add, digit, p.ptr_i8, cursor, minus_one)
	p.instr2(.store, digit, p.void_type, ascii_digit, next_cursor)
	p.instr2(.store, digit, p.void_type, next_cursor, cursor_slot)
	length := p.instr1(.load, digit, p.i32_type, length_slot)
	one32 := p.m.get_or_add_const(p.i32_type, '1')
	next_length := p.instr2(.add, digit, p.i32_type, length, one32)
	p.instr2(.store, digit, p.void_type, next_length, length_slot)
	p.instr2(.store, digit, p.void_type, quotient, number_slot)
	has_more := p.instr2(.ne, digit, p.i1_type, quotient, zero64)
	after_digits := p.m.add_block(id, 'i64_string_after_digits')
	p.instr3(.br, digit, p.void_type, has_more, ssa.ValueID(digit), ssa.ValueID(after_digits))
	p.instr3(.br, after_digits, p.void_type, negative, ssa.ValueID(minus), ssa.ValueID(done))

	minus_cursor := p.instr1(.load, minus, p.ptr_i8, cursor_slot)
	sign_cursor := p.instr2(.add, minus, p.ptr_i8, minus_cursor, minus_one)
	minus_byte := p.m.get_or_add_const(p.u8_type, '45')
	p.instr2(.store, minus, p.void_type, minus_byte, sign_cursor)
	p.instr2(.store, minus, p.void_type, sign_cursor, cursor_slot)
	minus_length := p.instr1(.load, minus, p.i32_type, length_slot)
	signed_length := p.instr2(.add, minus, p.i32_type, minus_length, one32)
	p.instr2(.store, minus, p.void_type, signed_length, length_slot)
	p.instr1(.jmp, minus, p.void_type, ssa.ValueID(done))

	result := p.finish_reverse_written_string(done, buffer, cursor_slot, length_slot)
	p.instr1(.ret, done, p.void_type, result)
}

fn (mut p FastArm64Program) register_integer_format_runtime() {
	id := p.register_function('fast_integer_to_string', 'fast_integer_to_string', p.str_type, false)
	entry := p.m.add_block(id, 'integer_format_entry')
	digit := p.m.add_block(id, 'integer_format_digit')
	number_digit := p.m.add_block(id, 'integer_format_number_digit')
	letter_digit := p.m.add_block(id, 'integer_format_letter_digit')
	write_digit := p.m.add_block(id, 'integer_format_write_digit')
	after_digits := p.m.add_block(id, 'integer_format_after_digits')
	minus := p.m.add_block(id, 'integer_format_minus')
	done := p.m.add_block(id, 'integer_format_done')
	value := p.add_arg(id, p.i64_type, 'value')
	base := p.add_arg(id, p.i64_type, 'base')
	uppercase := p.add_arg(id, p.i1_type, 'uppercase')
	is_signed := p.add_arg(id, p.i1_type, 'is_signed')
	allocation_size := p.m.get_or_add_const(p.i64_type, '66')
	malloc_ref := p.m.add_value(.func_ref, p.ptr_i8, 'malloc', p.fn_ids['malloc'])
	buffer := p.m.add_instr(.call, entry, p.ptr_i8, [malloc_ref, allocation_size])
	last_offset := p.m.get_or_add_const(p.i64_type, '65')
	end := p.instr2(.add, entry, p.ptr_i8, buffer, last_offset)
	zero8 := p.m.get_or_add_const(p.u8_type, '0')
	p.instr2(.store, entry, p.void_type, zero8, end)
	zero64 := p.m.get_or_add_const(p.i64_type, '0')
	value_negative := p.instr2(.lt, entry, p.i1_type, value, zero64)
	negative := p.instr2(.and_, entry, p.i1_type, is_signed, value_negative)
	negated := p.instr1(.neg, entry, p.i64_type, value)
	magnitude := p.integer_select(entry, negative, negated, value, p.i64_type)
	number_slot := p.instr0(.alloca, entry, p.m.type_store.get_ptr(p.i64_type))
	cursor_slot := p.instr0(.alloca, entry, p.m.type_store.get_ptr(p.ptr_i8))
	length_slot := p.instr0(.alloca, entry, p.m.type_store.get_ptr(p.i32_type))
	digit_slot := p.instr0(.alloca, entry, p.m.type_store.get_ptr(p.u8_type))
	p.instr2(.store, entry, p.void_type, magnitude, number_slot)
	p.instr2(.store, entry, p.void_type, end, cursor_slot)
	zero32 := p.m.get_or_add_const(p.i32_type, '0')
	p.instr2(.store, entry, p.void_type, zero32, length_slot)
	p.instr1(.jmp, entry, p.void_type, ssa.ValueID(digit))

	number := p.instr1(.load, digit, p.i64_type, number_slot)
	remainder := p.instr2(.urem, digit, p.i64_type, number, base)
	quotient := p.instr2(.udiv, digit, p.i64_type, number, base)
	ten := p.m.get_or_add_const(p.i64_type, '10')
	is_letter := p.instr2(.uge, digit, p.i1_type, remainder, ten)
	p.instr3(.br, digit, p.void_type, is_letter, ssa.ValueID(letter_digit), ssa.ValueID(number_digit))
	number_byte := p.instr1(.trunc, number_digit, p.u8_type, remainder)
	ascii_zero := p.m.get_or_add_const(p.u8_type, '48')
	ascii_number := p.instr2(.add, number_digit, p.u8_type, number_byte, ascii_zero)
	p.instr2(.store, number_digit, p.void_type, ascii_number, digit_slot)
	p.instr1(.jmp, number_digit, p.void_type, ssa.ValueID(write_digit))
	letter_offset64 := p.instr2(.sub, letter_digit, p.i64_type, remainder, ten)
	letter_offset := p.instr1(.trunc, letter_digit, p.u8_type, letter_offset64)
	lowercase_a := p.m.get_or_add_const(p.u8_type, '97')
	uppercase_a := p.m.get_or_add_const(p.u8_type, '65')
	letter_base := p.integer_select(letter_digit, uppercase, uppercase_a, lowercase_a, p.u8_type)
	ascii_letter := p.instr2(.add, letter_digit, p.u8_type, letter_offset, letter_base)
	p.instr2(.store, letter_digit, p.void_type, ascii_letter, digit_slot)
	p.instr1(.jmp, letter_digit, p.void_type, ssa.ValueID(write_digit))
	ascii_digit := p.instr1(.load, write_digit, p.u8_type, digit_slot)
	cursor := p.instr1(.load, write_digit, p.ptr_i8, cursor_slot)
	minus_one := p.m.get_or_add_const(p.i64_type, '-1')
	next_cursor := p.instr2(.add, write_digit, p.ptr_i8, cursor, minus_one)
	p.instr2(.store, write_digit, p.void_type, ascii_digit, next_cursor)
	p.instr2(.store, write_digit, p.void_type, next_cursor, cursor_slot)
	length := p.instr1(.load, write_digit, p.i32_type, length_slot)
	one32 := p.m.get_or_add_const(p.i32_type, '1')
	next_length := p.instr2(.add, write_digit, p.i32_type, length, one32)
	p.instr2(.store, write_digit, p.void_type, next_length, length_slot)
	p.instr2(.store, write_digit, p.void_type, quotient, number_slot)
	has_more := p.instr2(.ne, write_digit, p.i1_type, quotient, zero64)
	p.instr3(.br, write_digit, p.void_type, has_more, ssa.ValueID(digit), ssa.ValueID(after_digits))
	p.instr3(.br, after_digits, p.void_type, negative, ssa.ValueID(minus), ssa.ValueID(done))

	minus_cursor := p.instr1(.load, minus, p.ptr_i8, cursor_slot)
	sign_cursor := p.instr2(.add, minus, p.ptr_i8, minus_cursor, minus_one)
	minus_byte := p.m.get_or_add_const(p.u8_type, '45')
	p.instr2(.store, minus, p.void_type, minus_byte, sign_cursor)
	p.instr2(.store, minus, p.void_type, sign_cursor, cursor_slot)
	minus_length := p.instr1(.load, minus, p.i32_type, length_slot)
	signed_length := p.instr2(.add, minus, p.i32_type, minus_length, one32)
	p.instr2(.store, minus, p.void_type, signed_length, length_slot)
	p.instr1(.jmp, minus, p.void_type, ssa.ValueID(done))

	result := p.finish_reverse_written_string(done, buffer, cursor_slot, length_slot)
	p.instr1(.ret, done, p.void_type, result)
}

fn (mut p FastArm64Program) finish_reverse_written_string(block ssa.BlockID, buffer ssa.ValueID, cursor_slot ssa.ValueID, length_slot ssa.ValueID) ssa.ValueID {
	result_cursor := p.instr1(.load, block, p.ptr_i8, cursor_slot)
	result_length := p.instr1(.load, block, p.i32_type, length_slot)
	result_length64 := p.instr1(.zext, block, p.i64_type, result_length)
	memmove_ref := p.m.add_value(.func_ref, p.ptr_i8, 'memmove', p.fn_ids['memmove'])
	p.m.add_instr(.call, block, p.ptr_i8, [memmove_ref, buffer, result_cursor, result_length64])
	terminator := p.instr2(.add, block, p.ptr_i8, buffer, result_length64)
	zero8 := p.m.get_or_add_const(p.u8_type, '0')
	p.instr2(.store, block, p.void_type, zero8, terminator)
	result_slot := p.instr0(.alloca, block, p.m.type_store.get_ptr(p.str_type))
	p.instr2(.store, block, p.void_type, buffer, p.string_field_ptr(block, result_slot, 0))
	p.instr2(.store, block, p.void_type, result_length, p.string_field_ptr(block, result_slot, 1))
	zero32 := p.m.get_or_add_const(p.i32_type, '0')
	p.instr2(.store, block, p.void_type, zero32, p.string_field_ptr(block, result_slot, 2))
	return p.instr1(.load, block, p.str_type, result_slot)
}

fn (mut p FastArm64Program) register_bool_string_runtime() {
	id := p.register_function('fast_bool_to_string', 'fast_bool_to_string', p.str_type, false)
	entry := p.m.add_block(id, 'bool_string_entry')
	true_block := p.m.add_block(id, 'bool_string_true')
	false_block := p.m.add_block(id, 'bool_string_false')
	value := p.add_arg(id, p.i1_type, 'value')
	p.instr3(.br, entry, p.void_type, value, ssa.ValueID(true_block), ssa.ValueID(false_block))
	true_value := p.m.add_value(.string_literal, p.str_type, 'true', 0)
	false_value := p.m.add_value(.string_literal, p.str_type, 'false', 0)
	p.instr1(.ret, true_block, p.void_type, true_value)
	p.instr1(.ret, false_block, p.void_type, false_value)
}

fn (mut p FastArm64Program) register_character_string_runtime() {
	id := p.register_function('fast_character_to_string', 'fast_character_to_string', p.str_type, false)
	entry := p.m.add_block(id, 'character_string_entry')
	ascii := p.m.add_block(id, 'character_string_ascii')
	check_two := p.m.add_block(id, 'character_string_check_two')
	two := p.m.add_block(id, 'character_string_two')
	check_three := p.m.add_block(id, 'character_string_check_three')
	three := p.m.add_block(id, 'character_string_three')
	check_four := p.m.add_block(id, 'character_string_check_four')
	four := p.m.add_block(id, 'character_string_four')
	done := p.m.add_block(id, 'character_string_done')
	value := p.add_arg(id, p.i64_type, 'value')
	allocation_size := p.m.get_or_add_const(p.i64_type, '5')
	malloc_ref := p.m.add_value(.func_ref, p.ptr_i8, 'malloc', p.fn_ids['malloc'])
	buffer := p.m.add_instr(.call, entry, p.ptr_i8, [malloc_ref, allocation_size])
	length_slot := p.instr0(.alloca, entry, p.m.type_store.get_ptr(p.i32_type))
	zero32 := p.m.get_or_add_const(p.i32_type, '0')
	p.instr2(.store, entry, p.void_type, zero32, length_slot)
	ascii_limit := p.m.get_or_add_const(p.i64_type, '127')
	is_ascii := p.instr2(.ule, entry, p.i1_type, value, ascii_limit)
	p.instr3(.br, entry, p.void_type, is_ascii, ssa.ValueID(ascii), ssa.ValueID(check_two))

	ascii_byte := p.instr1(.trunc, ascii, p.u8_type, value)
	p.instr2(.store, ascii, p.void_type, ascii_byte, buffer)
	one32 := p.m.get_or_add_const(p.i32_type, '1')
	p.instr2(.store, ascii, p.void_type, one32, length_slot)
	p.instr1(.jmp, ascii, p.void_type, ssa.ValueID(done))

	two_limit := p.m.get_or_add_const(p.i64_type, '2047')
	fits_two := p.instr2(.ule, check_two, p.i1_type, value, two_limit)
	p.instr3(.br, check_two, p.void_type, fits_two, ssa.ValueID(two), ssa.ValueID(check_three))
	shift_six := p.m.get_or_add_const(p.i64_type, '6')
	shifted_six := p.instr2(.lshr, two, p.i64_type, value, shift_six)
	first_two := p.instr1(.trunc, two, p.u8_type, shifted_six)
	two_prefix := p.m.get_or_add_const(p.u8_type, '192')
	first_two_encoded := p.instr2(.or_, two, p.u8_type, first_two, two_prefix)
	p.instr2(.store, two, p.void_type, first_two_encoded, buffer)
	continuation_mask := p.m.get_or_add_const(p.i64_type, '63')
	second_two_bits := p.instr2(.and_, two, p.i64_type, value, continuation_mask)
	second_two := p.instr1(.trunc, two, p.u8_type, second_two_bits)
	continuation_prefix := p.m.get_or_add_const(p.u8_type, '128')
	second_two_encoded := p.instr2(.or_, two, p.u8_type, second_two, continuation_prefix)
	one64 := p.m.get_or_add_const(p.i64_type, '1')
	second_two_address := p.instr2(.add, two, p.ptr_i8, buffer, one64)
	p.instr2(.store, two, p.void_type, second_two_encoded, second_two_address)
	two32 := p.m.get_or_add_const(p.i32_type, '2')
	p.instr2(.store, two, p.void_type, two32, length_slot)
	p.instr1(.jmp, two, p.void_type, ssa.ValueID(done))

	three_limit := p.m.get_or_add_const(p.i64_type, '65535')
	fits_three := p.instr2(.ule, check_three, p.i1_type, value, three_limit)
	p.instr3(.br, check_three, p.void_type, fits_three, ssa.ValueID(three), ssa.ValueID(check_four))
	shift_twelve := p.m.get_or_add_const(p.i64_type, '12')
	shifted_twelve := p.instr2(.lshr, three, p.i64_type, value, shift_twelve)
	first_three := p.instr1(.trunc, three, p.u8_type, shifted_twelve)
	three_prefix := p.m.get_or_add_const(p.u8_type, '224')
	first_three_encoded := p.instr2(.or_, three, p.u8_type, first_three, three_prefix)
	p.instr2(.store, three, p.void_type, first_three_encoded, buffer)
	shifted_three_six := p.instr2(.lshr, three, p.i64_type, value, shift_six)
	second_three_bits := p.instr2(.and_, three, p.i64_type, shifted_three_six, continuation_mask)
	second_three := p.instr1(.trunc, three, p.u8_type, second_three_bits)
	second_three_encoded := p.instr2(.or_, three, p.u8_type, second_three, continuation_prefix)
	second_three_address := p.instr2(.add, three, p.ptr_i8, buffer, one64)
	p.instr2(.store, three, p.void_type, second_three_encoded, second_three_address)
	third_three_bits := p.instr2(.and_, three, p.i64_type, value, continuation_mask)
	third_three := p.instr1(.trunc, three, p.u8_type, third_three_bits)
	third_three_encoded := p.instr2(.or_, three, p.u8_type, third_three, continuation_prefix)
	two64 := p.m.get_or_add_const(p.i64_type, '2')
	third_three_address := p.instr2(.add, three, p.ptr_i8, buffer, two64)
	p.instr2(.store, three, p.void_type, third_three_encoded, third_three_address)
	three32 := p.m.get_or_add_const(p.i32_type, '3')
	p.instr2(.store, three, p.void_type, three32, length_slot)
	p.instr1(.jmp, three, p.void_type, ssa.ValueID(done))

	four_limit := p.m.get_or_add_const(p.i64_type, '1114111')
	fits_four := p.instr2(.ule, check_four, p.i1_type, value, four_limit)
	p.instr3(.br, check_four, p.void_type, fits_four, ssa.ValueID(four), ssa.ValueID(done))
	shift_eighteen := p.m.get_or_add_const(p.i64_type, '18')
	shifted_eighteen := p.instr2(.lshr, four, p.i64_type, value, shift_eighteen)
	first_four := p.instr1(.trunc, four, p.u8_type, shifted_eighteen)
	four_prefix := p.m.get_or_add_const(p.u8_type, '240')
	first_four_encoded := p.instr2(.or_, four, p.u8_type, first_four, four_prefix)
	p.instr2(.store, four, p.void_type, first_four_encoded, buffer)
	shifted_four_twelve := p.instr2(.lshr, four, p.i64_type, value, shift_twelve)
	second_four_bits := p.instr2(.and_, four, p.i64_type, shifted_four_twelve, continuation_mask)
	second_four := p.instr1(.trunc, four, p.u8_type, second_four_bits)
	second_four_encoded := p.instr2(.or_, four, p.u8_type, second_four, continuation_prefix)
	second_four_address := p.instr2(.add, four, p.ptr_i8, buffer, one64)
	p.instr2(.store, four, p.void_type, second_four_encoded, second_four_address)
	shifted_four_six := p.instr2(.lshr, four, p.i64_type, value, shift_six)
	third_four_bits := p.instr2(.and_, four, p.i64_type, shifted_four_six, continuation_mask)
	third_four := p.instr1(.trunc, four, p.u8_type, third_four_bits)
	third_four_encoded := p.instr2(.or_, four, p.u8_type, third_four, continuation_prefix)
	third_four_address := p.instr2(.add, four, p.ptr_i8, buffer, two64)
	p.instr2(.store, four, p.void_type, third_four_encoded, third_four_address)
	fourth_four_bits := p.instr2(.and_, four, p.i64_type, value, continuation_mask)
	fourth_four := p.instr1(.trunc, four, p.u8_type, fourth_four_bits)
	fourth_four_encoded := p.instr2(.or_, four, p.u8_type, fourth_four, continuation_prefix)
	three64 := p.m.get_or_add_const(p.i64_type, '3')
	fourth_four_address := p.instr2(.add, four, p.ptr_i8, buffer, three64)
	p.instr2(.store, four, p.void_type, fourth_four_encoded, fourth_four_address)
	four32 := p.m.get_or_add_const(p.i32_type, '4')
	p.instr2(.store, four, p.void_type, four32, length_slot)
	p.instr1(.jmp, four, p.void_type, ssa.ValueID(done))

	length := p.instr1(.load, done, p.i32_type, length_slot)
	length64 := p.instr1(.zext, done, p.i64_type, length)
	terminator := p.instr2(.add, done, p.ptr_i8, buffer, length64)
	zero8 := p.m.get_or_add_const(p.u8_type, '0')
	p.instr2(.store, done, p.void_type, zero8, terminator)
	result_slot := p.instr0(.alloca, done, p.m.type_store.get_ptr(p.str_type))
	p.instr2(.store, done, p.void_type, buffer, p.string_field_ptr(done, result_slot, 0))
	p.instr2(.store, done, p.void_type, length, p.string_field_ptr(done, result_slot, 1))
	p.instr2(.store, done, p.void_type, zero32, p.string_field_ptr(done, result_slot, 2))
	result := p.instr1(.load, done, p.str_type, result_slot)
	p.instr1(.ret, done, p.void_type, result)
}

fn (mut p FastArm64Program) register_string_padding_runtime() {
	id := p.register_function('fast_string_pad', 'fast_string_pad', p.str_type, false)
	entry := p.m.add_block(id, 'string_pad_entry')
	pad := p.m.add_block(id, 'string_pad')
	left_pad := p.m.add_block(id, 'string_pad_left')
	right_check := p.m.add_block(id, 'string_pad_right_check')
	zero_check := p.m.add_block(id, 'string_pad_zero_check')
	right_pad := p.m.add_block(id, 'string_pad_right')
	signed_zero_pad := p.m.add_block(id, 'string_pad_signed_zero')
	done := p.m.add_block(id, 'string_pad_done')
	unchanged := p.m.add_block(id, 'string_pad_unchanged')
	value := p.add_arg(id, p.str_type, 'value')
	width := p.add_arg(id, p.i32_type, 'width')
	left := p.add_arg(id, p.i1_type, 'left')
	zero_pad := p.add_arg(id, p.i1_type, 'zero_pad')
	value_slot := p.instr0(.alloca, entry, p.m.type_store.get_ptr(p.str_type))
	p.instr2(.store, entry, p.void_type, value, value_slot)
	data := p.instr1(.load, entry, p.ptr_i8, p.string_field_ptr(entry, value_slot, 0))
	length := p.instr1(.load, entry, p.i32_type, p.string_field_ptr(entry, value_slot, 1))
	needs_padding := p.instr2(.lt, entry, p.i1_type, length, width)
	p.instr3(.br, entry, p.void_type, needs_padding, ssa.ValueID(pad), ssa.ValueID(unchanged))
	p.instr1(.ret, unchanged, p.void_type, value)
	one64 := p.m.get_or_add_const(p.i64_type, '1')
	width64 := p.instr1(.zext, pad, p.i64_type, width)
	allocation_size := p.instr2(.add, pad, p.i64_type, width64, one64)
	malloc_ref := p.m.add_value(.func_ref, p.ptr_i8, 'malloc', p.fn_ids['malloc'])
	buffer := p.m.add_instr(.call, pad, p.ptr_i8, [malloc_ref, allocation_size])
	padding32 := p.instr2(.sub, pad, p.i32_type, width, length)
	padding64 := p.instr1(.zext, pad, p.i64_type, padding32)
	length64 := p.instr1(.zext, pad, p.i64_type, length)
	p.instr3(.br, pad, p.void_type, left, ssa.ValueID(left_pad), ssa.ValueID(right_check))
	memcpy_ref := p.m.add_value(.func_ref, p.ptr_i8, 'memcpy', p.fn_ids['memcpy'])
	memset_ref := p.m.add_value(.func_ref, p.ptr_i8, 'memset', p.fn_ids['memset'])
	p.m.add_instr(.call, left_pad, p.ptr_i8, [memcpy_ref, buffer, data, length64])
	left_destination := p.instr2(.add, left_pad, p.ptr_i8, buffer, length64)
	space := p.m.get_or_add_const(p.i32_type, '32')
	p.m.add_instr(.call, left_pad, p.ptr_i8, [memset_ref, left_destination, space, padding64])
	p.instr1(.jmp, left_pad, p.void_type, ssa.ValueID(done))
	p.instr3(.br, right_check, p.void_type, zero_pad, ssa.ValueID(zero_check), ssa.ValueID(right_pad))
	first := p.instr1(.load, zero_check, p.u8_type, data)
	minus := p.m.get_or_add_const(p.u8_type, '45')
	has_minus := p.instr2(.eq, zero_check, p.i1_type, first, minus)
	p.instr3(.br, zero_check, p.void_type, has_minus, ssa.ValueID(signed_zero_pad), ssa.ValueID(right_pad))
	zero_byte := p.m.get_or_add_const(p.i32_type, '48')
	pad_byte := p.integer_select(right_pad, zero_pad, zero_byte, space, p.i32_type)
	p.m.add_instr(.call, right_pad, p.ptr_i8, [memset_ref, buffer, pad_byte, padding64])
	right_destination := p.instr2(.add, right_pad, p.ptr_i8, buffer, padding64)
	p.m.add_instr(.call, right_pad, p.ptr_i8, [memcpy_ref, right_destination, data, length64])
	p.instr1(.jmp, right_pad, p.void_type, ssa.ValueID(done))
	p.instr2(.store, signed_zero_pad, p.void_type, minus, buffer)
	zero_destination := p.instr2(.add, signed_zero_pad, p.ptr_i8, buffer, one64)
	p.m.add_instr(.call, signed_zero_pad, p.ptr_i8, [memset_ref, zero_destination, zero_byte,
		padding64])
	signed_source := p.instr2(.add, signed_zero_pad, p.ptr_i8, data, one64)
	after_padding := p.instr2(.add, signed_zero_pad, p.i64_type, padding64, one64)
	signed_destination := p.instr2(.add, signed_zero_pad, p.ptr_i8, buffer, after_padding)
	signed_length := p.instr2(.sub, signed_zero_pad, p.i64_type, length64, one64)
	p.m.add_instr(.call, signed_zero_pad, p.ptr_i8, [memcpy_ref, signed_destination, signed_source,
		signed_length])
	p.instr1(.jmp, signed_zero_pad, p.void_type, ssa.ValueID(done))
	terminator := p.instr2(.add, done, p.ptr_i8, buffer, width64)
	zero8 := p.m.get_or_add_const(p.u8_type, '0')
	p.instr2(.store, done, p.void_type, zero8, terminator)
	result_slot := p.instr0(.alloca, done, p.m.type_store.get_ptr(p.str_type))
	p.instr2(.store, done, p.void_type, buffer, p.string_field_ptr(done, result_slot, 0))
	p.instr2(.store, done, p.void_type, width, p.string_field_ptr(done, result_slot, 1))
	zero32 := p.m.get_or_add_const(p.i32_type, '0')
	p.instr2(.store, done, p.void_type, zero32, p.string_field_ptr(done, result_slot, 2))
	result := p.instr1(.load, done, p.str_type, result_slot)
	p.instr1(.ret, done, p.void_type, result)
}

fn (mut p FastArm64Program) register_string_zero_extension_runtime() {
	id := p.register_function('fast_string_extend_zeros', 'fast_string_extend_zeros', p.str_type, false)
	entry := p.m.add_block(id, 'string_extend_zeros_entry')
	value := p.add_arg(id, p.str_type, 'value')
	extra := p.add_arg(id, p.i32_type, 'extra')
	value_slot := p.instr0(.alloca, entry, p.m.type_store.get_ptr(p.str_type))
	p.instr2(.store, entry, p.void_type, value, value_slot)
	data := p.instr1(.load, entry, p.ptr_i8, p.string_field_ptr(entry, value_slot, 0))
	length := p.instr1(.load, entry, p.i32_type, p.string_field_ptr(entry, value_slot, 1))
	result_length := p.instr2(.add, entry, p.i32_type, length, extra)
	result_length64 := p.instr1(.zext, entry, p.i64_type, result_length)
	one64 := p.m.get_or_add_const(p.i64_type, '1')
	allocation_size := p.instr2(.add, entry, p.i64_type, result_length64, one64)
	malloc_ref := p.m.add_value(.func_ref, p.ptr_i8, 'malloc', p.fn_ids['malloc'])
	buffer := p.m.add_instr(.call, entry, p.ptr_i8, [malloc_ref, allocation_size])
	length64 := p.instr1(.zext, entry, p.i64_type, length)
	memcpy_ref := p.m.add_value(.func_ref, p.ptr_i8, 'memcpy', p.fn_ids['memcpy'])
	p.m.add_instr(.call, entry, p.ptr_i8, [memcpy_ref, buffer, data, length64])
	extension := p.instr2(.add, entry, p.ptr_i8, buffer, length64)
	extra64 := p.instr1(.zext, entry, p.i64_type, extra)
	ascii_zero := p.m.get_or_add_const(p.i32_type, '48')
	memset_ref := p.m.add_value(.func_ref, p.ptr_i8, 'memset', p.fn_ids['memset'])
	p.m.add_instr(.call, entry, p.ptr_i8, [memset_ref, extension, ascii_zero, extra64])
	terminator := p.instr2(.add, entry, p.ptr_i8, buffer, result_length64)
	zero8 := p.m.get_or_add_const(p.u8_type, '0')
	p.instr2(.store, entry, p.void_type, zero8, terminator)
	result_slot := p.instr0(.alloca, entry, p.m.type_store.get_ptr(p.str_type))
	p.instr2(.store, entry, p.void_type, buffer, p.string_field_ptr(entry, result_slot, 0))
	p.instr2(.store, entry, p.void_type, result_length, p.string_field_ptr(entry, result_slot, 1))
	zero32 := p.m.get_or_add_const(p.i32_type, '0')
	p.instr2(.store, entry, p.void_type, zero32, p.string_field_ptr(entry, result_slot, 2))
	result := p.instr1(.load, entry, p.str_type, result_slot)
	p.instr1(.ret, entry, p.void_type, result)
}

fn (mut p FastArm64Program) register_fixed_float_string_runtime() {
	id := p.register_function('fast_fixed_from_scaled', 'fast_fixed_from_scaled', p.str_type, false)
	entry := p.m.add_block(id, 'fixed_scaled_entry')
	without_fraction := p.m.add_block(id, 'fixed_scaled_integer')
	with_fraction := p.m.add_block(id, 'fixed_scaled_fraction')
	leading_zero := p.m.add_block(id, 'fixed_scaled_leading_zero')
	split_digits := p.m.add_block(id, 'fixed_scaled_split')
	done := p.m.add_block(id, 'fixed_scaled_done')
	scaled := p.add_arg(id, p.i64_type, 'scaled')
	precision := p.add_arg(id, p.i32_type, 'precision')
	convert_ref := p.m.add_value(.func_ref, p.str_type, 'fast_i64_to_string', p.fn_ids['fast_i64_to_string'])
	digits := p.m.add_instr(.call, entry, p.str_type, [convert_ref, scaled])
	digits_slot := p.instr0(.alloca, entry, p.m.type_store.get_ptr(p.str_type))
	p.instr2(.store, entry, p.void_type, digits, digits_slot)
	data := p.instr1(.load, entry, p.ptr_i8, p.string_field_ptr(entry, digits_slot, 0))
	length := p.instr1(.load, entry, p.i32_type, p.string_field_ptr(entry, digits_slot, 1))
	zero64 := p.m.get_or_add_const(p.i64_type, '0')
	negative := p.instr2(.lt, entry, p.i1_type, scaled, zero64)
	one32 := p.m.get_or_add_const(p.i32_type, '1')
	zero32 := p.m.get_or_add_const(p.i32_type, '0')
	sign_length := p.integer_select(entry, negative, one32, zero32, p.i32_type)
	digit_count := p.instr2(.sub, entry, p.i32_type, length, sign_length)
	has_fraction := p.instr2(.gt, entry, p.i1_type, precision, zero32)
	integer_candidate := p.instr2(.sub, entry, p.i32_type, digit_count, precision)
	needs_leading_zero := p.instr2(.le, entry, p.i1_type, digit_count, precision)
	integer_length := p.integer_select(entry, needs_leading_zero, one32, integer_candidate, p.i32_type)
	fraction_extra := p.instr2(.add, entry, p.i32_type, precision, one32)
	fraction_size := p.integer_select(entry, has_fraction, fraction_extra, zero32, p.i32_type)
	content_length := p.instr2(.add, entry, p.i32_type, sign_length, integer_length)
	total_length := p.instr2(.add, entry, p.i32_type, content_length, fraction_size)
	total64 := p.instr1(.zext, entry, p.i64_type, total_length)
	one64 := p.m.get_or_add_const(p.i64_type, '1')
	allocation_size := p.instr2(.add, entry, p.i64_type, total64, one64)
	malloc_ref := p.m.add_value(.func_ref, p.ptr_i8, 'malloc', p.fn_ids['malloc'])
	buffer := p.m.add_instr(.call, entry, p.ptr_i8, [malloc_ref, allocation_size])
	sign64 := p.instr1(.zext, entry, p.i64_type, sign_length)
	source_digits := p.instr2(.add, entry, p.ptr_i8, data, sign64)
	content_start := p.instr2(.add, entry, p.ptr_i8, buffer, sign64)
	minus := p.m.get_or_add_const(p.u8_type, '45')
	p.instr2(.store, entry, p.void_type, minus, buffer)
	p.instr3(.br, entry, p.void_type, has_fraction, ssa.ValueID(with_fraction), ssa.ValueID(without_fraction))
	digit_count64 := p.instr1(.zext, without_fraction, p.i64_type, digit_count)
	memcpy_ref := p.m.add_value(.func_ref, p.ptr_i8, 'memcpy', p.fn_ids['memcpy'])
	p.m.add_instr(.call, without_fraction, p.ptr_i8, [memcpy_ref, content_start, source_digits,
		digit_count64])
	p.instr1(.jmp, without_fraction, p.void_type, ssa.ValueID(done))
	p.instr3(.br, with_fraction, p.void_type, needs_leading_zero, ssa.ValueID(leading_zero), ssa.ValueID(split_digits))
	ascii_zero := p.m.get_or_add_const(p.u8_type, '48')
	dot := p.m.get_or_add_const(p.u8_type, '46')
	p.instr2(.store, leading_zero, p.void_type, ascii_zero, content_start)
	dot_position := p.instr2(.add, leading_zero, p.ptr_i8, content_start, one64)
	p.instr2(.store, leading_zero, p.void_type, dot, dot_position)
	two64 := p.m.get_or_add_const(p.i64_type, '2')
	fraction_start := p.instr2(.add, leading_zero, p.ptr_i8, content_start, two64)
	leading_count32 := p.instr2(.sub, leading_zero, p.i32_type, precision, digit_count)
	leading_count64 := p.instr1(.zext, leading_zero, p.i64_type, leading_count32)
	memset_ref := p.m.add_value(.func_ref, p.ptr_i8, 'memset', p.fn_ids['memset'])
	zero_int := p.m.get_or_add_const(p.i32_type, '48')
	p.m.add_instr(.call, leading_zero, p.ptr_i8, [memset_ref, fraction_start, zero_int,
		leading_count64])
	leading_destination := p.instr2(.add, leading_zero, p.ptr_i8, fraction_start, leading_count64)
	digit_count64_leading := p.instr1(.zext, leading_zero, p.i64_type, digit_count)
	p.m.add_instr(.call, leading_zero, p.ptr_i8, [memcpy_ref, leading_destination, source_digits,
		digit_count64_leading])
	p.instr1(.jmp, leading_zero, p.void_type, ssa.ValueID(done))
	integer_length64 := p.instr1(.zext, split_digits, p.i64_type, integer_length)
	p.m.add_instr(.call, split_digits, p.ptr_i8, [memcpy_ref, content_start, source_digits,
		integer_length64])
	split_dot := p.instr2(.add, split_digits, p.ptr_i8, content_start, integer_length64)
	p.instr2(.store, split_digits, p.void_type, dot, split_dot)
	fraction_destination := p.instr2(.add, split_digits, p.ptr_i8, split_dot, one64)
	fraction_source := p.instr2(.add, split_digits, p.ptr_i8, source_digits, integer_length64)
	precision64 := p.instr1(.zext, split_digits, p.i64_type, precision)
	p.m.add_instr(.call, split_digits, p.ptr_i8, [memcpy_ref, fraction_destination, fraction_source,
		precision64])
	p.instr1(.jmp, split_digits, p.void_type, ssa.ValueID(done))
	terminator := p.instr2(.add, done, p.ptr_i8, buffer, total64)
	zero8 := p.m.get_or_add_const(p.u8_type, '0')
	p.instr2(.store, done, p.void_type, zero8, terminator)
	result_slot := p.instr0(.alloca, done, p.m.type_store.get_ptr(p.str_type))
	p.instr2(.store, done, p.void_type, buffer, p.string_field_ptr(done, result_slot, 0))
	p.instr2(.store, done, p.void_type, total_length, p.string_field_ptr(done, result_slot, 1))
	p.instr2(.store, done, p.void_type, zero32, p.string_field_ptr(done, result_slot, 2))
	result := p.instr1(.load, done, p.str_type, result_slot)
	p.instr1(.ret, done, p.void_type, result)
}

fn (mut p FastArm64Program) register_scientific_float_string_runtime() {
	id := p.register_function('fast_scientific_from_float', 'fast_scientific_from_float', p.str_type, false)
	entry := p.m.add_block(id, 'scientific_float_entry')
	value := p.add_arg(id, p.f64_type, 'value')
	precision := p.add_arg(id, p.i32_type, 'precision')
	uppercase := p.add_arg(id, p.i1_type, 'uppercase')
	zero32 := p.m.get_or_add_const(p.i32_type, '0')
	one32 := p.m.get_or_add_const(p.i32_type, '1')
	two32 := p.m.get_or_add_const(p.i32_type, '2')
	max_real_precision := p.m.get_or_add_const(p.i32_type, '16')
	precision_is_large := p.instr2(.gt, entry, p.i1_type, precision, max_real_precision)
	real_precision := p.integer_select(entry, precision_is_large, max_real_precision, precision, p.i32_type)
	significant_digits := p.instr2(.add, entry, p.i32_type, real_precision, one32)
	decimal_slot := p.instr0(.alloca, entry, p.m.type_store.get_ptr(p.i32_type))
	sign_slot := p.instr0(.alloca, entry, p.m.type_store.get_ptr(p.i32_type))
	ecvt_ref := p.m.add_value(.func_ref, p.ptr_i8, 'ecvt', p.fn_ids['ecvt'])
	digits := p.m.add_instr(.call, entry, p.ptr_i8, [ecvt_ref, value, significant_digits,
		decimal_slot, sign_slot])
	decimal := p.instr1(.load, entry, p.i32_type, decimal_slot)
	exponent := p.instr2(.sub, entry, p.i32_type, decimal, one32)
	exponent_negative := p.instr2(.lt, entry, p.i1_type, exponent, zero32)
	negated_exponent := p.instr1(.neg, entry, p.i32_type, exponent)
	exponent_magnitude := p.integer_select(entry, exponent_negative, negated_exponent, exponent, p.i32_type)
	exponent64 := p.instr1(.sext, entry, p.i64_type, exponent_magnitude)
	integer_ref := p.m.add_value(.func_ref, p.str_type, 'fast_i64_to_string', p.fn_ids['fast_i64_to_string'])
	exponent_string := p.m.add_instr(.call, entry, p.str_type, [integer_ref, exponent64])
	exponent_slot := p.instr0(.alloca, entry, p.m.type_store.get_ptr(p.str_type))
	p.instr2(.store, entry, p.void_type, exponent_string, exponent_slot)
	exponent_data := p.instr1(.load, entry, p.ptr_i8, p.string_field_ptr(entry, exponent_slot, 0))
	exponent_length := p.instr1(.load, entry, p.i32_type, p.string_field_ptr(entry, exponent_slot, 1))
	exponent_needs_padding := p.instr2(.lt, entry, p.i1_type, exponent_length, two32)
	exponent_padding_candidate := p.instr2(.sub, entry, p.i32_type, two32, exponent_length)
	exponent_padding := p.integer_select(entry, exponent_needs_padding, exponent_padding_candidate, zero32, p.i32_type)
	exponent_width := p.instr2(.add, entry, p.i32_type, exponent_length, exponent_padding)
	sign_value := p.instr1(.load, entry, p.i32_type, sign_slot)
	has_sign := p.instr2(.ne, entry, p.i1_type, sign_value, zero32)
	sign_length := p.instr1(.zext, entry, p.i32_type, has_sign)
	has_fraction := p.instr2(.gt, entry, p.i1_type, precision, zero32)
	fraction_candidate := p.instr2(.add, entry, p.i32_type, precision, one32)
	fraction_size := p.integer_select(entry, has_fraction, fraction_candidate, zero32, p.i32_type)
	mantissa_length := p.instr2(.add, entry, p.i32_type, fraction_size, one32)
	exponent_section_length := p.instr2(.add, entry, p.i32_type, exponent_width, two32)
	signed_mantissa_length := p.instr2(.add, entry, p.i32_type, sign_length, mantissa_length)
	total_length := p.instr2(.add, entry, p.i32_type, signed_mantissa_length, exponent_section_length)
	total64 := p.instr1(.zext, entry, p.i64_type, total_length)
	one64 := p.m.get_or_add_const(p.i64_type, '1')
	allocation_size := p.instr2(.add, entry, p.i64_type, total64, one64)
	malloc_ref := p.m.add_value(.func_ref, p.ptr_i8, 'malloc', p.fn_ids['malloc'])
	buffer := p.m.add_instr(.call, entry, p.ptr_i8, [malloc_ref, allocation_size])
	minus := p.m.get_or_add_const(p.u8_type, '45')
	p.instr2(.store, entry, p.void_type, minus, buffer)
	sign_length64 := p.instr1(.zext, entry, p.i64_type, sign_length)
	mantissa := p.instr2(.add, entry, p.ptr_i8, buffer, sign_length64)
	first_digit := p.instr1(.load, entry, p.u8_type, digits)
	p.instr2(.store, entry, p.void_type, first_digit, mantissa)
	dot_address := p.instr2(.add, entry, p.ptr_i8, mantissa, one64)
	dot := p.m.get_or_add_const(p.u8_type, '46')
	p.instr2(.store, entry, p.void_type, dot, dot_address)
	fraction_start := p.instr2(.add, entry, p.ptr_i8, dot_address, one64)
	digit_source := p.instr2(.add, entry, p.ptr_i8, digits, one64)
	real_precision64 := p.instr1(.zext, entry, p.i64_type, real_precision)
	memcpy_ref := p.m.add_value(.func_ref, p.ptr_i8, 'memcpy', p.fn_ids['memcpy'])
	p.m.add_instr(.call, entry, p.ptr_i8, [memcpy_ref, fraction_start, digit_source,
		real_precision64])
	extra_precision := p.instr2(.sub, entry, p.i32_type, precision, real_precision)
	extra_precision64 := p.instr1(.zext, entry, p.i64_type, extra_precision)
	extra_start := p.instr2(.add, entry, p.ptr_i8, fraction_start, real_precision64)
	ascii_zero := p.m.get_or_add_const(p.i32_type, '48')
	memset_ref := p.m.add_value(.func_ref, p.ptr_i8, 'memset', p.fn_ids['memset'])
	p.m.add_instr(.call, entry, p.ptr_i8, [memset_ref, extra_start, ascii_zero, extra_precision64])
	mantissa_length64 := p.instr1(.zext, entry, p.i64_type, mantissa_length)
	marker_address := p.instr2(.add, entry, p.ptr_i8, mantissa, mantissa_length64)
	lowercase_e := p.m.get_or_add_const(p.u8_type, '101')
	uppercase_e := p.m.get_or_add_const(p.u8_type, '69')
	marker := p.integer_select(entry, uppercase, uppercase_e, lowercase_e, p.u8_type)
	p.instr2(.store, entry, p.void_type, marker, marker_address)
	exponent_sign_address := p.instr2(.add, entry, p.ptr_i8, marker_address, one64)
	plus := p.m.get_or_add_const(p.u8_type, '43')
	exponent_sign := p.integer_select(entry, exponent_negative, minus, plus, p.u8_type)
	p.instr2(.store, entry, p.void_type, exponent_sign, exponent_sign_address)
	exponent_digits_start := p.instr2(.add, entry, p.ptr_i8, exponent_sign_address, one64)
	exponent_padding64 := p.instr1(.zext, entry, p.i64_type, exponent_padding)
	p.m.add_instr(.call, entry, p.ptr_i8, [memset_ref, exponent_digits_start, ascii_zero,
		exponent_padding64])
	exponent_destination := p.instr2(.add, entry, p.ptr_i8, exponent_digits_start, exponent_padding64)
	exponent_length64 := p.instr1(.zext, entry, p.i64_type, exponent_length)
	p.m.add_instr(.call, entry, p.ptr_i8, [memcpy_ref, exponent_destination, exponent_data,
		exponent_length64])
	terminator := p.instr2(.add, entry, p.ptr_i8, buffer, total64)
	zero8 := p.m.get_or_add_const(p.u8_type, '0')
	p.instr2(.store, entry, p.void_type, zero8, terminator)
	result_slot := p.instr0(.alloca, entry, p.m.type_store.get_ptr(p.str_type))
	p.instr2(.store, entry, p.void_type, buffer, p.string_field_ptr(entry, result_slot, 0))
	p.instr2(.store, entry, p.void_type, total_length, p.string_field_ptr(entry, result_slot, 1))
	p.instr2(.store, entry, p.void_type, zero32, p.string_field_ptr(entry, result_slot, 2))
	result := p.instr1(.load, entry, p.str_type, result_slot)
	p.instr1(.ret, entry, p.void_type, result)
}

fn (mut p FastArm64Program) register_float_string_runtime() {
	id := p.register_function('fast_float_to_string', 'fast_float_to_string', p.str_type, false)
	entry := p.m.add_block(id, 'float_string_entry')
	condition := p.m.add_block(id, 'float_string_condition')
	body := p.m.add_block(id, 'float_string_body')
	done := p.m.add_block(id, 'float_string_done')
	append_dot := p.m.add_block(id, 'float_string_append_dot')
	finish := p.m.add_block(id, 'float_string_finish')
	value := p.add_arg(id, p.f64_type, 'value')
	digits := p.add_arg(id, p.i32_type, 'digits')
	append_decimal := p.add_arg(id, p.i1_type, 'append_decimal')
	uppercase := p.add_arg(id, p.i1_type, 'uppercase')
	buffer_size := p.m.get_or_add_const(p.i64_type, '64')
	malloc_ref := p.m.add_value(.func_ref, p.ptr_i8, 'malloc', p.fn_ids['malloc'])
	buffer := p.m.add_instr(.call, entry, p.ptr_i8, [malloc_ref, buffer_size])
	gcvt_ref := p.m.add_value(.func_ref, p.ptr_i8, 'gcvt', p.fn_ids['gcvt'])
	p.m.add_instr(.call, entry, p.ptr_i8, [gcvt_ref, value, digits, buffer])
	strlen_ref := p.m.add_value(.func_ref, p.i64_type, 'strlen', p.fn_ids['strlen'])
	length64 := p.m.add_instr(.call, entry, p.i64_type, [strlen_ref, buffer])
	length := p.instr1(.trunc, entry, p.i32_type, length64)
	length_slot := p.instr0(.alloca, entry, p.m.type_store.get_ptr(p.i32_type))
	p.instr2(.store, entry, p.void_type, length, length_slot)
	index_slot := p.instr0(.alloca, entry, p.m.type_store.get_ptr(p.i64_type))
	zero64 := p.m.get_or_add_const(p.i64_type, '0')
	p.instr2(.store, entry, p.void_type, zero64, index_slot)
	plain_slot := p.instr0(.alloca, entry, p.m.type_store.get_ptr(p.i1_type))
	true_value := p.m.get_or_add_const(p.i1_type, '1')
	p.instr2(.store, entry, p.void_type, true_value, plain_slot)
	p.instr1(.jmp, entry, p.void_type, ssa.ValueID(condition))
	index := p.instr1(.load, condition, p.i64_type, index_slot)
	more := p.instr2(.lt, condition, p.i1_type, index, length64)
	p.instr3(.br, condition, p.void_type, more, ssa.ValueID(body), ssa.ValueID(done))
	character_ptr := p.instr2(.add, body, p.ptr_i8, buffer, index)
	character := p.instr1(.load, body, p.u8_type, character_ptr)
	ascii_zero := p.m.get_or_add_const(p.u8_type, '48')
	ascii_nine := p.m.get_or_add_const(p.u8_type, '57')
	minus := p.m.get_or_add_const(p.u8_type, '45')
	at_least_zero := p.instr2(.ge, body, p.i1_type, character, ascii_zero)
	at_most_nine := p.instr2(.le, body, p.i1_type, character, ascii_nine)
	is_digit := p.instr2(.and_, body, p.i1_type, at_least_zero, at_most_nine)
	is_minus := p.instr2(.eq, body, p.i1_type, character, minus)
	is_plain_character := p.instr2(.or_, body, p.i1_type, is_digit, is_minus)
	lowercase_a := p.m.get_or_add_const(p.u8_type, '97')
	lowercase_z := p.m.get_or_add_const(p.u8_type, '122')
	is_at_least_a := p.instr2(.ge, body, p.i1_type, character, lowercase_a)
	is_at_most_z := p.instr2(.le, body, p.i1_type, character, lowercase_z)
	is_lowercase := p.instr2(.and_, body, p.i1_type, is_at_least_a, is_at_most_z)
	should_uppercase := p.instr2(.and_, body, p.i1_type, uppercase, is_lowercase)
	case_offset := p.m.get_or_add_const(p.u8_type, '32')
	upper_character := p.instr2(.sub, body, p.u8_type, character, case_offset)
	output_character := p.integer_select(body, should_uppercase, upper_character, character, p.u8_type)
	p.instr2(.store, body, p.void_type, output_character, character_ptr)
	was_plain := p.instr1(.load, body, p.i1_type, plain_slot)
	still_plain := p.instr2(.and_, body, p.i1_type, was_plain, is_plain_character)
	p.instr2(.store, body, p.void_type, still_plain, plain_slot)
	one64 := p.m.get_or_add_const(p.i64_type, '1')
	next := p.instr2(.add, body, p.i64_type, index, one64)
	p.instr2(.store, body, p.void_type, next, index_slot)
	p.instr1(.jmp, body, p.void_type, ssa.ValueID(condition))
	plain_integer := p.instr1(.load, done, p.i1_type, plain_slot)
	needs_decimal := p.instr2(.and_, done, p.i1_type, plain_integer, append_decimal)
	p.instr3(.br, done, p.void_type, needs_decimal, ssa.ValueID(append_dot), ssa.ValueID(finish))
	dot_ptr := p.instr2(.add, append_dot, p.ptr_i8, buffer, length64)
	dot := p.m.get_or_add_const(p.u8_type, '46')
	p.instr2(.store, append_dot, p.void_type, dot, dot_ptr)
	zero_ptr := p.instr2(.add, append_dot, p.ptr_i8, dot_ptr, one64)
	p.instr2(.store, append_dot, p.void_type, ascii_zero, zero_ptr)
	terminator_ptr := p.instr2(.add, append_dot, p.ptr_i8, zero_ptr, one64)
	zero8 := p.m.get_or_add_const(p.u8_type, '0')
	p.instr2(.store, append_dot, p.void_type, zero8, terminator_ptr)
	two32 := p.m.get_or_add_const(p.i32_type, '2')
	extended_length := p.instr2(.add, append_dot, p.i32_type, length, two32)
	p.instr2(.store, append_dot, p.void_type, extended_length, length_slot)
	p.instr1(.jmp, append_dot, p.void_type, ssa.ValueID(finish))
	result_slot := p.instr0(.alloca, finish, p.m.type_store.get_ptr(p.str_type))
	p.instr2(.store, finish, p.void_type, buffer, p.string_field_ptr(finish, result_slot, 0))
	result_length := p.instr1(.load, finish, p.i32_type, length_slot)
	p.instr2(.store, finish, p.void_type, result_length, p.string_field_ptr(finish, result_slot, 1))
	zero32 := p.m.get_or_add_const(p.i32_type, '0')
	p.instr2(.store, finish, p.void_type, zero32, p.string_field_ptr(finish, result_slot, 2))
	result := p.instr1(.load, finish, p.str_type, result_slot)
	p.instr1(.ret, finish, p.void_type, result)
}

fn (mut p FastArm64Program) register_integer_str_wrappers() {
	for key, signature in p.functions {
		if signature.is_disabled || key.starts_with('C.') {
			continue
		}
		symbol := fastc_c_function_name_for_key(key)
		if symbol !in ['int__str', 'i8__str', 'i16__str', 'i64__str', 'isize__str', 'rune__str',
			'u8__str', 'u16__str', 'u32__str', 'u64__str', 'usize__str'] {
			continue
		}
		id := p.register_signature_function(key) or { continue }
		if p.m.funcs[id].blocks.len > 0 {
			continue
		}
		if signature.parameter_types.len == 0 {
			continue
		}
		entry := p.m.add_block(id, 'integer_str_entry')
		argument_type := p.type_id(signature.parameter_types[0])
		argument := p.add_arg(id, argument_type, 'value')
		mut integer := argument
		if p.m.type_size(argument_type) < 8 {
			integer = if p.m.type_store.types[argument_type].is_unsigned {
				p.instr1(.zext, entry, p.i64_type, argument)
			} else {
				p.instr1(.sext, entry, p.i64_type, argument)
			}
		} else if argument_type != p.i64_type {
			integer = p.instr1(.bitcast, entry, p.i64_type, argument)
		}
		convert_ref := p.m.add_value(.func_ref, p.str_type, 'fast_i64_to_string', p.fn_ids['fast_i64_to_string'])
		result := p.m.add_instr(.call, entry, p.str_type, [convert_ref, integer])
		p.instr1(.ret, entry, p.void_type, result)
		mut function := p.m.funcs[id]
		function.is_prototype = false
		function.is_c_extern = false
		p.m.funcs[id] = function
	}
}

fn (mut p FastArm64Program) register_string_sort_runtime() {
	less_id := p.register_function('fast_string_less', 'fast_string_less', p.i1_type, false)
	less_entry := p.m.add_block(less_id, 'string_less_entry')
	left := p.add_arg(less_id, p.str_type, 'left')
	right := p.add_arg(less_id, p.str_type, 'right')
	left_slot := p.instr0(.alloca, less_entry, p.m.type_store.get_ptr(p.str_type))
	right_slot := p.instr0(.alloca, less_entry, p.m.type_store.get_ptr(p.str_type))
	p.instr2(.store, less_entry, p.void_type, left, left_slot)
	p.instr2(.store, less_entry, p.void_type, right, right_slot)
	left_data := p.instr1(.load, less_entry, p.ptr_i8, p.string_field_ptr(less_entry, left_slot, 0))
	right_data := p.instr1(.load, less_entry, p.ptr_i8, p.string_field_ptr(less_entry, right_slot, 0))
	left_len := p.instr1(.load, less_entry, p.i32_type, p.string_field_ptr(less_entry, left_slot, 1))
	right_len := p.instr1(.load, less_entry, p.i32_type, p.string_field_ptr(less_entry, right_slot, 1))
	left_shorter := p.instr2(.lt, less_entry, p.i1_type, left_len, right_len)
	minimum := p.integer_select(less_entry, left_shorter, left_len, right_len, p.i32_type)
	minimum64 := p.instr1(.zext, less_entry, p.i64_type, minimum)
	memcmp_ref := p.m.add_value(.func_ref, p.i32_type, 'memcmp', p.fn_ids['memcmp'])
	comparison := p.m.add_instr(.call, less_entry, p.i32_type, [memcmp_ref, left_data, right_data,
		minimum64])
	zero32 := p.m.get_or_add_const(p.i32_type, '0')
	bytes_less := p.instr2(.lt, less_entry, p.i1_type, comparison, zero32)
	bytes_equal := p.instr2(.eq, less_entry, p.i1_type, comparison, zero32)
	shorter_equal_prefix := p.instr2(.and_, less_entry, p.i1_type, bytes_equal, left_shorter)
	less_result := p.instr2(.or_, less_entry, p.i1_type, bytes_less, shorter_equal_prefix)
	p.instr1(.ret, less_entry, p.void_type, less_result)

	sort_id := p.register_function('fast_array_sort_strings', 'fast_array_sort_strings', p.void_type, false)
	entry := p.m.add_block(sort_id, 'array_sort_entry')
	outer_condition := p.m.add_block(sort_id, 'array_sort_outer_condition')
	outer_body := p.m.add_block(sort_id, 'array_sort_outer_body')
	inner_condition := p.m.add_block(sort_id, 'array_sort_inner_condition')
	inner_body := p.m.add_block(sort_id, 'array_sort_inner_body')
	swap := p.m.add_block(sort_id, 'array_sort_swap')
	inner_increment := p.m.add_block(sort_id, 'array_sort_inner_increment')
	outer_increment := p.m.add_block(sort_id, 'array_sort_outer_increment')
	done := p.m.add_block(sort_id, 'array_sort_done')
	array_value := p.add_arg(sort_id, p.array_type, 'array_value')
	array_slot := p.instr0(.alloca, entry, p.m.type_store.get_ptr(p.array_type))
	i_slot := p.instr0(.alloca, entry, p.m.type_store.get_ptr(p.i32_type))
	j_slot := p.instr0(.alloca, entry, p.m.type_store.get_ptr(p.i32_type))
	p.instr2(.store, entry, p.void_type, array_value, array_slot)
	data := p.instr1(.load, entry, p.ptr_i8, p.struct_field_ptr(entry, array_slot, p.array_type, 0))
	length := p.instr1(.load, entry, p.i32_type, p.struct_field_ptr(entry, array_slot, p.array_type, 2))
	p.instr2(.store, entry, p.void_type, zero32, i_slot)
	p.instr1(.jmp, entry, p.void_type, ssa.ValueID(outer_condition))
	i := p.instr1(.load, outer_condition, p.i32_type, i_slot)
	outer_more := p.instr2(.lt, outer_condition, p.i1_type, i, length)
	p.instr3(.br, outer_condition, p.void_type, outer_more, ssa.ValueID(outer_body), ssa.ValueID(done))
	one32 := p.m.get_or_add_const(p.i32_type, '1')
	p.instr2(.store, outer_body, p.void_type, one32, j_slot)
	p.instr1(.jmp, outer_body, p.void_type, ssa.ValueID(inner_condition))
	inner_i := p.instr1(.load, inner_condition, p.i32_type, i_slot)
	j := p.instr1(.load, inner_condition, p.i32_type, j_slot)
	bound := p.instr2(.sub, inner_condition, p.i32_type, length, inner_i)
	inner_more := p.instr2(.lt, inner_condition, p.i1_type, j, bound)
	p.instr3(.br, inner_condition, p.void_type, inner_more, ssa.ValueID(inner_body), ssa.ValueID(outer_increment))
	j_body := p.instr1(.load, inner_body, p.i32_type, j_slot)
	j64 := p.instr1(.zext, inner_body, p.i64_type, j_body)
	previous := p.instr2(.sub, inner_body, p.i32_type, j_body, one32)
	previous64 := p.instr1(.zext, inner_body, p.i64_type, previous)
	string_size := p.m.get_or_add_const(p.i64_type, p.m.type_size(p.str_type).str())
	current_offset := p.instr2(.mul, inner_body, p.i64_type, j64, string_size)
	previous_offset := p.instr2(.mul, inner_body, p.i64_type, previous64, string_size)
	current_bytes := p.instr2(.add, inner_body, p.ptr_i8, data, current_offset)
	previous_bytes := p.instr2(.add, inner_body, p.ptr_i8, data, previous_offset)
	ptr_string := p.m.type_store.get_ptr(p.str_type)
	current_address := p.instr1(.bitcast, inner_body, ptr_string, current_bytes)
	previous_address := p.instr1(.bitcast, inner_body, ptr_string, previous_bytes)
	current_value := p.instr1(.load, inner_body, p.str_type, current_address)
	previous_value := p.instr1(.load, inner_body, p.str_type, previous_address)
	less_ref := p.m.add_value(.func_ref, p.i1_type, 'fast_string_less', less_id)
	is_less := p.m.add_instr(.call, inner_body, p.i1_type, [less_ref, current_value, previous_value])
	p.instr3(.br, inner_body, p.void_type, is_less, ssa.ValueID(swap), ssa.ValueID(inner_increment))
	p.instr2(.store, swap, p.void_type, previous_value, current_address)
	p.instr2(.store, swap, p.void_type, current_value, previous_address)
	p.instr1(.jmp, swap, p.void_type, ssa.ValueID(inner_increment))
	current_j := p.instr1(.load, inner_increment, p.i32_type, j_slot)
	next_j := p.instr2(.add, inner_increment, p.i32_type, current_j, one32)
	p.instr2(.store, inner_increment, p.void_type, next_j, j_slot)
	p.instr1(.jmp, inner_increment, p.void_type, ssa.ValueID(inner_condition))
	current_i := p.instr1(.load, outer_increment, p.i32_type, i_slot)
	next_i := p.instr2(.add, outer_increment, p.i32_type, current_i, one32)
	p.instr2(.store, outer_increment, p.void_type, next_i, i_slot)
	p.instr1(.jmp, outer_increment, p.void_type, ssa.ValueID(outer_condition))
	p.instr0(.ret, done, p.void_type)
}

fn FastArm64Parser.new(mut program FastArm64Program, source_file FastcSourceFile) &FastArm64Parser {
	file := token.File.unindexed(source_file.path, source_file.source.len)
	mut scan := scanner.new_scanner(program.prefs, .normal)
	scan.init(file, source_file.source)
	return &FastArm64Parser{
		program: unsafe { &program }
		source_file: source_file
		s: scan
		locals: map[string]FastArm64Local{}
		terminated: map[int]bool{}
		labels: map[string]ssa.BlockID{}
		break_to: []ssa.BlockID{}
		continue_to: []ssa.BlockID{}
	}
}

fn (mut p FastArm64Parser) next() {
	p.tok = p.s.scan()
	p.lit = p.s.lit
}

fn (p &FastArm64Parser) unsupported(feature string) IError {
	return error('fastc arm64 parser does not support ${feature} at byte ${p.s.pos} in ${p.source_file.path}')
}

fn (mut p FastArm64Parser) expect(wanted token.Token) ! {
	if p.tok != wanted {
		function_name := p.program.m.funcs[p.func_id].name
		return p.unsupported('`${wanted.str()}` while at `${p.tok.str()}` `${p.lit}` in `${function_name}`')
	}
	p.next()
}

fn (mut p FastArm64Parser) skip_group(open token.Token, close token.Token) ! {
	if p.tok != open {
		return p.unsupported('`${open.str()}` group')
	}
	mut depth := 0
	for {
		if p.tok == open {
			depth++
		} else if p.tok == close {
			depth--
			p.next()
			if depth == 0 {
				return
			}
			continue
		} else if p.tok == .eof {
			return p.unsupported('unfinished `${open.str()}` group')
		}
		p.next()
	}
}

fn (mut p FastArm64Parser) skip_attribute() ! {
	if p.tok != .attribute {
		return
	}
	p.next()
	mut depth := 1
	for depth > 0 {
		if p.tok == .eof {
			return p.unsupported('unfinished attribute')
		}
		if p.tok in [.attribute, .lsbr] {
			depth++
		} else if p.tok == .rsbr {
			depth--
		}
		p.next()
	}
}

fn (mut p FastArm64Parser) skip_to_statement_end() {
	mut parens := 0
	mut brackets := 0
	for p.tok != .eof {
		if parens == 0 && brackets == 0 && p.tok == .semicolon {
			p.next()
			return
		}
		match p.tok {
			.lpar { parens++ }
			.rpar { parens-- }
			.lsbr { brackets++ }
			.rsbr { brackets-- }
			else {}
		}
		p.next()
	}
}

fn (mut p FastArm64Parser) skip_declaration() ! {
	for p.tok !in [.eof, .lcbr, .semicolon] {
		p.next()
	}
	if p.tok == .lcbr {
		p.skip_group(.lcbr, .rcbr)!
	} else if p.tok == .semicolon {
		p.next()
	}
}

fn (mut p FastArm64Parser) skip_value_declaration() {
	mut parens := 0
	mut brackets := 0
	mut braces := 0
	for p.tok != .eof {
		if p.tok == .semicolon && parens == 0 && brackets == 0 && braces == 0 {
			p.next()
			return
		}
		match p.tok {
			.lpar { parens++ }
			.rpar { parens-- }
			.lsbr { brackets++ }
			.rsbr { brackets-- }
			.lcbr { braces++ }
			.rcbr { braces-- }
			else {}
		}
		p.next()
	}
}

fn (mut p FastArm64Parser) enter_source(source string) {
	file := token.File.unindexed(p.source_file.path, source.len)
	mut scan := scanner.new_scanner(p.program.prefs, .normal)
	scan.init(file, source)
	p.s = scan
	p.next()
}

fn (mut p FastArm64Parser) parse_selected_top_level(source string) ! {
	if source == '' {
		return
	}
	outer_scanner := p.s
	outer_tok := p.tok
	outer_lit := p.lit
	p.enter_source(source)
	p.parse_file_from_current_token()!
	p.s = outer_scanner
	p.tok = outer_tok
	p.lit = outer_lit
}

fn (mut p FastArm64Parser) parse_selected_statements(source string) ! {
	if source == '' {
		return
	}
	outer_scanner := p.s
	outer_tok := p.tok
	outer_lit := p.lit
	p.push_local_scope()
	p.push_defer_scope()
	p.enter_source(source)
	for p.tok != .eof {
		if p.tok == .semicolon {
			p.next()
			continue
		}
		p.parse_statement()!
	}
	if !p.block_is_terminated(p.cur_block) {
		p.emit_deferred_scopes(p.defer_starts.len - 1)!
	}
	p.pop_defer_scope()
	p.pop_local_scope()
	p.s = outer_scanner
	p.tok = outer_tok
	p.lit = outer_lit
}

fn (mut p FastArm64Parser) parse_file() ! {
	p.next()
	p.parse_file_from_current_token()!
}

fn (mut p FastArm64Parser) parse_file_from_current_token() ! {
	for p.tok != .eof {
		if p.tok == .semicolon {
			p.next()
			continue
		}
		if p.tok == .attribute {
			p.skip_attribute()!
			continue
		}
		if p.tok in [.key_module, .key_import, .hash] {
			p.skip_to_statement_end()
			continue
		}
		if p.tok in [.key_pub, .key_static] {
			p.next()
		}
		if p.tok == .key_fn {
			p.parse_function()!
			continue
		}
		if p.tok in [.key_struct, .key_union, .key_enum, .key_interface, .key_type] {
			p.next()
			p.skip_declaration()!
			continue
		}
		if p.tok in [.key_const, .key_global] {
			p.next()
			p.skip_value_declaration()
			continue
		}
		if p.tok == .dollar {
			selected := fastc_scan_selected_comptime_branch(mut p.s, p.s.scan(), p.source_file.path, p.program.prefs)!
			p.tok = selected.tok
			p.lit = p.s.lit
			p.parse_selected_top_level(selected.source)!
			continue
		}
		if p.source_file.header.module_name in ['', 'main'] {
			p.parse_script()!
			return
		}
		return p.unsupported('top-level `${p.tok.str()}`')
	}
}

fn (mut p FastArm64Parser) parse_script() ! {
	func_id := p.program.register_function('main', 'main', p.program.i32_type, false)
	if p.program.m.funcs[func_id].blocks.len > 0 {
		return p.unsupported('top-level statements in more than one source file')
	}
	p.func_id = func_id
	p.return_typ = p.program.i32_type
	p.return_is_option = false
	p.current_function = 'main'
	p.current_receiver = ''
	p.current_method_is_static = false
	p.locals = map[string]FastArm64Local{}
	p.terminated = map[int]bool{}
	p.labels = map[string]ssa.BlockID{}
	p.defer_sources = []string{}
	p.defer_starts = []int{}
	p.push_defer_scope()
	p.local_names = []string{}
	p.local_values = []FastArm64Local{}
	p.local_existed = []bool{}
	p.local_starts = []int{}
	p.push_local_scope()
	p.break_scopes = []int{}
	p.continue_scopes = []int{}
	p.map_loop_writebacks = []FastArm64MapLoopWriteback{}
	p.cur_block = p.program.m.add_block(func_id, 'main_entry')
	p.emit_main_startup()!
	for p.tok != .eof {
		if p.tok == .semicolon {
			p.next()
			continue
		}
		p.parse_statement()!
	}
	if !p.block_is_terminated(p.cur_block) {
		p.emit_deferred_scopes(0)!
		zero := p.program.m.get_or_add_const(p.program.i32_type, '0')
		p.program.instr1(.ret, p.cur_block, p.program.void_type, zero)
		p.mark_terminated(p.cur_block)
	}
}

fn (mut p FastArm64Parser) parse_function() ! {
	p.next()
	mut receiver_name := ''
	mut receiver_type := ''
	mut receiver_is_mut := false
	if p.tok == .lpar {
		p.next()
		if p.tok in [.key_mut, .key_shared] {
			receiver_is_mut = true
			p.next()
		}
		if p.tok != .name {
			return p.unsupported('method receiver name')
		}
		receiver_name = p.lit
		p.next()
		mut receiver_next_token := token.Token.unknown
		receiver_type, receiver_next_token = fastc_scan_type(mut p.s, p.tok, p.source_file.path, p.source_file.header.module_name, p.source_file.header.imports, p.program.declared_types, p.program.prefs.building_v) or {
			return p.unsupported('method receiver type')
		}
		p.tok = receiver_next_token
		p.lit = p.s.lit
		if receiver_is_mut && !receiver_type.ends_with('*') {
			receiver_type += '*'
		}
		p.expect(.rpar)!
	}
	if p.tok != .name && !p.tok.is_keyword() && !p.tok.is_overloadable() {
		return p.unsupported('function name')
	}
	mut name := if p.tok.is_overloadable() { p.tok.str() } else { p.lit }
	p.next()
	if name == 'C' && p.tok == .dot {
		p.skip_declaration()!
		return
	}
	mut static_owner := ''
	if p.tok == .dot {
		static_owner = name
		p.next()
		if p.tok != .name && !p.tok.is_keyword() {
			return p.unsupported('static function name')
		}
		name = p.lit
		p.next()
	}
	if p.tok == .lsbr {
		p.skip_group(.lsbr, .rsbr)!
		p.skip_declaration()!
		return
	}
	mut key := ''
	if static_owner != '' {
		key = '${fastc_type_key(p.source_file.header.module_name, static_owner)}.${name}'
	} else if receiver_name == '' {
		key = fastc_function_key(p.source_file.header.module_name, name)
	} else {
		key = p.find_declared_method_key(receiver_type, name) or { '' }
		if key == '' {
			p.skip_declaration()!
			return
		}
	}
	signature := p.program.functions[key] or {
		p.skip_declaration()!
		return
	}
	if signature.is_disabled {
		p.skip_declaration()!
		return
	}
	is_entry_function := receiver_name == '' && key == fastc_function_key(signature.module_name, name) && name in [
		'main',
		'init',
		'cleanup',
	]
	if p.program.prefs.building_v && !is_entry_function && key !in p.program.native_used_function_names {
		p.skip_declaration()!
		return
	}
	if p.tok == .semicolon {
		p.next()
		return
	}
	if p.tok != .lpar {
		return p.unsupported('parameters of function `${key}` while at `${p.tok.str()}`')
	}
	p.next()
	mut parameter_names := if receiver_name == '' {
		[]string{}
	} else {
		[
			receiver_name,
		]
	}
	mut parameter_types := if receiver_name == '' {
		[]string{}
	} else {
		[
			receiver_type,
		]
	}
	mut parameter_is_mut := if receiver_name == '' {
		[]bool{}
	} else {
		[
			receiver_is_mut,
		]
	}
	for p.tok != .rpar {
		is_mut := p.tok in [.key_mut, .key_shared]
		if is_mut {
			p.next()
		}
		if p.tok != .name {
			return p.unsupported('function parameter')
		}
		parameter_names << p.lit
		parameter_is_mut << is_mut
		p.next()
		typ, next_token := fastc_scan_type(mut p.s, p.tok, p.source_file.path, p.source_file.header.module_name, p.source_file.header.imports, p.program.declared_types, false) or { return p.unsupported('function parameter type') }
		parameter_types << typ
		p.tok = next_token
		p.lit = p.s.lit
		if p.tok == .comma {
			p.next()
		} else if p.tok != .rpar {
			return p.unsupported('function parameter separator')
		}
	}
	p.next()
	for p.tok !in [.lcbr, .semicolon, .eof] {
		p.next()
	}
	if p.tok == .semicolon {
		p.next()
		return
	}
	if p.tok != .lcbr {
		return p.unsupported('function body')
	}
	func_id := p.program.register_signature_function(key) or {
		return p.unsupported('registered function `${key}`')
	}
	if p.program.m.funcs[func_id].blocks.len > 0 {
		p.skip_group(.lcbr, .rcbr)!
		return
	}
	p.func_id = func_id
	p.current_function = name
	p.current_receiver = if static_owner != '' {
		static_owner
	} else {
		receiver_type.trim_right('*')
	}
	p.current_method_is_static = static_owner != ''
	p.return_typ = p.program.fn_returns[key]
	p.return_name = if signature.return_type == 'Option' {
		signature.option_type
	} else {
		signature.return_type
	}
	p.return_names = signature.return_types.clone()
	p.return_is_option = signature.return_type == 'Option'
	p.locals = map[string]FastArm64Local{}
	p.terminated = map[int]bool{}
	p.defer_sources = []string{}
	p.defer_starts = []int{}
	p.local_names = []string{}
	p.local_values = []FastArm64Local{}
	p.local_existed = []bool{}
	p.local_starts = []int{}
	p.break_scopes = []int{}
	p.continue_scopes = []int{}
	p.map_loop_writebacks = []FastArm64MapLoopWriteback{}
	p.cur_block = p.program.m.add_block(func_id, '${name}_entry')
	for i, parameter_name in parameter_names {
		mut typ_name := parameter_types[i]
		mut typ := p.program.type_id(typ_name)
		abi_type_name := if i < signature.parameter_types.len {
			signature.parameter_types[i]
		} else {
			typ_name
		}
		abi_type := p.program.type_id(abi_type_name)
		abi_layout := p.program.m.type_store.types[abi_type]
		if i == 0 && receiver_name != '' && parameter_is_mut[i] && abi_type == typ && abi_layout.kind == .ptr_t {
			typ = abi_layout.elem_type
			if typ_name.ends_with('*') {
				typ_name = typ_name[..typ_name.len - 1]
			}
		}
		argument := p.program.add_arg(func_id, abi_type, parameter_name)
		address := p.parameter_local_address(abi_type, typ, argument)
		p.locals[parameter_name] = FastArm64Local{
			addr: address
			typ: typ
			typ_name: typ_name
		}
	}
	if func_id == p.program.fn_ids['main'] {
		p.emit_main_startup()!
	}
	p.parse_block()!
	if !p.block_is_terminated(p.cur_block) {
		if name == 'main' || p.return_typ == p.program.void_type {
			if p.return_is_option {
				p.store_option_success()
			}
			if name == 'main' {
				zero := p.program.m.get_or_add_const(p.program.i32_type, '0')
				p.program.instr1(.ret, p.cur_block, p.program.void_type, zero)
			} else {
				p.program.instr0(.ret, p.cur_block, p.program.void_type)
			}
		} else {
			return p.unsupported('non-void function `${name}` that can fall through')
		}
		p.mark_terminated(p.cur_block)
	}
}

fn (mut p FastArm64Parser) emit_main_startup() ! {
	create_key_ref := p.program.m.add_value(.func_ref, p.program.i32_type, 'pthread_key_create', p.program.fn_ids['pthread_key_create'])
	free_ref := p.program.m.add_value(.func_ref, p.program.void_type, 'free', p.program.fn_ids['free'])
	p.program.m.add_instr(.call, p.cur_block, p.program.i32_type, [create_key_ref,
		p.program.option_state_key_global, free_ref])
	for function_key in p.program.module_init_function_keys {
		function_id := p.program.register_signature_function(function_key) or {
			return p.unsupported('registered module init `${function_key}`')
		}
		function_ref := p.program.m.add_value(.func_ref, p.program.fn_returns[function_key], p.program.fn_symbols[function_key], function_id)
		p.program.m.add_instr(.call, p.cur_block, p.program.fn_returns[function_key], [
			function_ref,
		])
	}
	if cleanup_id := p.program.fn_ids['v_fastc_cleanup_modules'] {
		cleanup_ref := p.program.m.add_value(.func_ref, p.program.void_type, 'v_fastc_cleanup_modules', cleanup_id)
		atexit_ref := p.program.m.add_value(.func_ref, p.program.i32_type, 'atexit', p.program.fn_ids['atexit'])
		p.program.m.add_instr(.call, p.cur_block, p.program.i32_type, [atexit_ref, cleanup_ref])
	}
}

fn (mut p FastArm64Parser) parameter_local_address(abi_type ssa.TypeID, typ ssa.TypeID, argument ssa.ValueID) ssa.ValueID {
	abi_layout := p.program.m.type_store.types[abi_type]
	if abi_layout.kind == .ptr_t && abi_layout.elem_type == typ {
		return argument
	}
	slot := p.program.instr0(.alloca, p.cur_block, p.program.m.type_store.get_ptr(typ))
	p.program.instr2(.store, p.cur_block, p.program.void_type, argument, slot)
	return slot
}

fn (mut p FastArm64Parser) find_declared_method_key(receiver_type string, name string) ?string {
	receiver_id := p.program.type_id(receiver_type)
	for key in p.program.function_keys_by_name[name] {
		signature := p.program.functions[key]
		if signature.path != p.source_file.path || signature.parameter_types.len == 0 {
			continue
		}
		if p.program.type_id(signature.parameter_types[0]) == receiver_id {
			return key
		}
	}
	return none
}

fn (p &FastArm64Parser) block_is_terminated(block ssa.BlockID) bool {
	return p.terminated[int(block)]
}

fn (mut p FastArm64Parser) mark_terminated(block ssa.BlockID) {
	p.terminated[int(block)] = true
}

fn (mut p FastArm64Parser) parse_block() ! {
	p.push_local_scope()
	p.push_defer_scope()
	p.expect(.lcbr)!
	for p.tok !in [.rcbr, .eof] {
		if p.tok == .semicolon {
			p.next()
			continue
		}
		p.parse_statement()!
		if p.block_is_terminated(p.cur_block) && p.tok !in [.rcbr, .eof] {
			mut depth := 0
			for p.tok != .eof {
				if p.tok == .lcbr {
					depth++
				} else if p.tok == .rcbr {
					if depth == 0 {
						break
					}
					depth--
				}
				p.next()
			}
		}
	}
	if !p.block_is_terminated(p.cur_block) {
		p.emit_deferred_scopes(p.defer_starts.len - 1)!
	}
	p.pop_defer_scope()
	p.pop_local_scope()
	p.expect(.rcbr)!
}

fn (mut p FastArm64Parser) parse_statement() ! {
	match p.tok {
		.dollar {
			selected := fastc_scan_selected_comptime_branch(mut p.s, p.s.scan(), p.source_file.path, p.program.prefs)!
			p.tok = selected.tok
			p.lit = p.s.lit
			p.parse_selected_statements(selected.source)!
		}
		.key_mut {
			p.next()
			p.parse_name_statement(true)!
		}
		.key_return {
			p.parse_return()!
		}
		.key_if {
			p.parse_if()!
		}
		.key_for {
			p.parse_for()!
		}
		.key_defer {
			p.next()
			block := fastc_scan_comptime_block(mut p.s, p.tok, p.source_file.path)!
			if p.defer_starts.len == 0 {
				return p.unsupported('`defer` outside a lexical scope')
			}
			p.defer_sources << block.source
			p.tok = block.tok
			p.lit = p.s.lit
		}
		.key_match {
			p.parse_match_statement()!
		}
		.key_break {
			if p.break_to.len == 0 {
				return p.unsupported('`break` outside a loop')
			}
			p.emit_deferred_scopes(p.break_scopes.last())!
			p.emit_active_map_loop_writebacks(p.map_loop_writebacks.len - 1, false)
			p.program.instr1(.jmp, p.cur_block, p.program.void_type, ssa.ValueID(p.break_to.last()))
			p.mark_terminated(p.cur_block)
			p.next()
		}
		.key_continue {
			if p.continue_to.len == 0 {
				return p.unsupported('`continue` outside a loop')
			}
			p.emit_deferred_scopes(p.continue_scopes.last())!
			p.program.instr1(.jmp, p.cur_block, p.program.void_type, ssa.ValueID(p.continue_to.last()))
			p.mark_terminated(p.cur_block)
			p.next()
		}
		.key_goto {
			p.next()
			if p.tok != .name {
				return p.unsupported('goto label')
			}
			target := p.labels[p.lit] or {
				return p.unsupported('forward goto `${p.lit}`')
			}
			p.program.instr1(.jmp, p.cur_block, p.program.void_type, ssa.ValueID(target))
			p.mark_terminated(p.cur_block)
			p.next()
		}
		.key_unsafe {
			p.next()
			p.parse_block()!
		}
		.name {
			mut look := p.s
			if look.scan() == .colon {
				name := p.lit
				p.next()
				p.expect(.colon)!
				label := p.program.m.add_block(p.func_id, 'label_${name}')
				p.labels[name] = label
				if !p.block_is_terminated(p.cur_block) {
					p.program.instr1(.jmp, p.cur_block, p.program.void_type, ssa.ValueID(label))
					p.mark_terminated(p.cur_block)
				}
				p.cur_block = label
			} else {
				p.parse_name_statement(false)!
			}
		}
		else {
			p.parse_expression_statement()!
		}
	}
	if p.tok == .semicolon {
		p.next()
	}
}

fn (p &FastArm64Parser) simple_index_assignment_follows() bool {
	mut look := p.s
	if look.scan() != .lsbr {
		return false
	}
	mut depth := 1
	for depth > 0 {
		tok := look.scan()
		if tok == .eof {
			return false
		}
		if tok == .lsbr {
			depth++
		} else if tok == .rsbr {
			depth--
		}
	}
	return look.scan() in [.assign, .plus_assign, .minus_assign, .mul_assign, .div_assign,
		.mod_assign, .left_shift, .left_shift_assign, .right_shift_assign,
		.right_shift_unsigned_assign, .and_assign, .or_assign, .xor_assign]
}

fn (mut p FastArm64Parser) parse_name_statement(after_mut bool) ! {
	name := p.lit
	mut look := p.s
	next_token := look.scan()
	if name == '_' && next_token in [.assign, .decl_assign] {
		p.next()
		p.next()
		_ = p.parse_expression(0)!
		return
	}
	if next_token == .comma {
		mut names := [name]
		p.next()
		for p.tok == .comma {
			p.next()
			if p.tok == .key_mut {
				p.next()
			}
			if p.tok != .name {
				return p.unsupported('multi-declaration name')
			}
			names << p.lit
			p.next()
		}
		if p.tok !in [.decl_assign, .assign] {
			return p.unsupported('multi-declaration assignment')
		}
		is_declaration := p.tok == .decl_assign
		p.next()
		value := p.parse_expression(0)!
		layout := p.program.m.type_store.types[value.typ]
		if layout.kind != .struct_t || layout.fields.len < names.len {
			return p.unsupported('multi-declaration value `${value.typ_name}`')
		}
		value_slot := p.program.instr0(.alloca, p.cur_block, p.program.m.type_store.get_ptr(value.typ))
		p.program.instr2(.store, p.cur_block, p.program.void_type, value.id, value_slot)
		for i, item_name in names {
			if item_name == '_' {
				continue
			}
			item_type := layout.fields[i]
			item_type_name := if i < value.tuple_types.len { value.tuple_types[i] } else { '' }
			item_address := p.program.struct_field_ptr(p.cur_block, value_slot, value.typ, i)
			item := p.program.instr1(.load, p.cur_block, item_type, item_address)
			if !is_declaration {
				local := p.locals[item_name] or {
					return p.unsupported('multi-assignment to unknown `${item_name}`')
				}
				mut assigned := FastArm64Value{
					id: item
					typ: item_type
				}
				if item_type != local.typ {
					assigned = p.convert_value(assigned, local.typ, local.typ_name)
				}
				p.program.instr2(.store, p.cur_block, p.program.void_type, assigned.id, local.addr)
				continue
			}
			address := p.program.instr0(.alloca, p.cur_block, p.program.m.type_store.get_ptr(item_type))
			p.program.instr2(.store, p.cur_block, p.program.void_type, item, address)
			p.declare_local(item_name, FastArm64Local{
				addr: address
				typ: item_type
				typ_name: item_type_name
			})
		}
		return
	}
	if next_token == .lsbr && name in p.locals && p.simple_index_assignment_follows() {
		local := p.locals[name]
		p.next()
		p.next()
		key := p.parse_expression(0)!
		p.expect(.rsbr)!
		if p.tok == .left_shift && local.typ == p.program.array_type {
			array_value := p.program.instr1(.load, p.cur_block, local.typ, local.addr)
			array_slot := p.program.instr0(.alloca, p.cur_block, p.program.m.type_store.get_ptr(p.program.array_type))
			p.program.instr2(.store, p.cur_block, p.program.void_type, array_value, array_slot)
			length := p.program.instr1(.load, p.cur_block, p.program.i32_type, p.program.struct_field_ptr(p.cur_block, array_slot, p.program.array_type, 2))
			index64 := p.checked_array_index(key, length, 'array_nested_index')
			data := p.program.instr1(.load, p.cur_block, p.program.ptr_i8, p.program.struct_field_ptr(p.cur_block, array_slot, p.program.array_type, 0))
			element_type_name := p.program.array_element_type_name(local.typ_name) or {
				return p.unsupported('array type `${local.typ_name}`')
			}
			element_type := p.program.type_id(element_type_name)
			element_size := p.program.m.get_or_add_const(p.program.i64_type, p.program.m.type_size(element_type).str())
			offset := p.program.instr2(.mul, p.cur_block, p.program.i64_type, index64, element_size)
			address := p.program.instr2(.add, p.cur_block, p.program.ptr_i8, data, offset)
			typed_address := p.program.instr1(.bitcast, p.cur_block, p.program.m.type_store.get_ptr(element_type), address)
			selected := FastArm64Value{
				id: p.program.instr1(.load, p.cur_block, element_type, typed_address)
				typ: element_type
				typ_name: element_type_name
				address: typed_address
			}
			p.next()
			item := p.parse_contextual_value(selected.typ_name)!
			updated := p.emit_array_push(selected, item, false)!
			p.program.instr2(.store, p.cur_block, p.program.void_type, updated.id, typed_address)
			return
		}
		if p.tok !in [.assign, .plus_assign, .minus_assign, .mul_assign, .div_assign, .mod_assign,
			.left_shift_assign, .right_shift_assign, .right_shift_unsigned_assign, .and_assign,
			.or_assign, .xor_assign] {
			return p.unsupported('indexed compound assignment')
		}
		op := p.tok
		p.next()
		local_layout := p.program.m.type_store.types[local.typ]
		mut indexed_value_type_name := ''
		if local_layout.kind == .ptr_t && local.typ_name.starts_with('&') {
			indexed_value_type_name = local.typ_name[1..]
		} else if local_layout.kind == .array_t {
			indexed_value_type_name = fastc_fixed_array_element_type(local.typ_name) or { '' }
		} else if local.typ == p.program.array_type {
			indexed_value_type_name = p.program.array_element_type_name(local.typ_name) or { '' }
		} else if local.typ == p.program.map_type {
			_, map_value_type_name := fastc_map_key_value_types(local.typ_name) or {
				return p.unsupported('map type `${local.typ_name}`')
			}
			indexed_value_type_name = map_value_type_name
		}
		mut value := if indexed_value_type_name == '' {
			p.parse_expression(0)!
		} else {
			p.parse_contextual_value(indexed_value_type_name)!
		}
		if p.program.m.type_store.types[local.typ].kind == .ptr_t {
			base := p.program.instr1(.load, p.cur_block, local.typ, local.addr)
			element_type := p.program.m.type_store.types[local.typ].elem_type
			element_size := p.program.m.get_or_add_const(p.program.i64_type, p.program.m.type_size(element_type).str())
			index64 := p.integer_to_i64(key)
			offset := p.program.instr2(.mul, p.cur_block, p.program.i64_type, index64, element_size)
			address := p.program.instr2(.add, p.cur_block, local.typ, base, offset)
			if value.typ != element_type {
				value = p.convert_value(value, element_type, '')
			}
			if op != .assign {
				current := p.program.instr1(.load, p.cur_block, element_type, address)
				value = FastArm64Value{
					id: p.program.instr2(fast_arm64_compound_opcode(op, p.program.m.type_store.types[element_type].is_unsigned), p.cur_block, element_type, current, value.id)
					typ: element_type
				}
			}
			p.program.instr2(.store, p.cur_block, p.program.void_type, value.id, address)
			return
		}
		if p.program.m.type_store.types[local.typ].kind == .array_t {
			layout := p.program.m.type_store.types[local.typ]
			element_type := layout.elem_type
			element_type_name := fastc_fixed_array_element_type(local.typ_name) or { 'u8' }
			length := p.program.m.get_or_add_const(p.program.i32_type, layout.len.str())
			index64 := p.checked_array_index(key, length, 'fixed_array_assignment_index')
			base := p.program.instr1(.bitcast, p.cur_block, p.program.ptr_i8, local.addr)
			element_size := p.program.m.get_or_add_const(p.program.i64_type, p.program.m.type_size(element_type).str())
			offset := p.program.instr2(.mul, p.cur_block, p.program.i64_type, index64, element_size)
			address := p.program.instr2(.add, p.cur_block, p.program.ptr_i8, base, offset)
			typed_address := p.program.instr1(.bitcast, p.cur_block, p.program.m.type_store.get_ptr(element_type), address)
			mut assigned := value
			if assigned.typ != element_type {
				assigned = p.convert_value(assigned, element_type, element_type_name)
			}
			if op != .assign {
				current := p.program.instr1(.load, p.cur_block, element_type, typed_address)
				assigned = FastArm64Value{
					id: p.program.instr2(fast_arm64_compound_opcode(op, p.program.m.type_store.types[element_type].is_unsigned), p.cur_block, element_type, current, assigned.id)
					typ: element_type
					typ_name: element_type_name
				}
			}
			p.program.instr2(.store, p.cur_block, p.program.void_type, assigned.id, typed_address)
			return
		}
		if local.typ == p.program.array_type {
			array_value := p.program.instr1(.load, p.cur_block, local.typ, local.addr)
			array_slot := p.program.instr0(.alloca, p.cur_block, p.program.m.type_store.get_ptr(p.program.array_type))
			p.program.instr2(.store, p.cur_block, p.program.void_type, array_value, array_slot)
			length := p.program.instr1(.load, p.cur_block, p.program.i32_type, p.program.struct_field_ptr(p.cur_block, array_slot, p.program.array_type, 2))
			index64 := p.checked_array_index(key, length, 'array_assignment_index')
			data := p.program.instr1(.load, p.cur_block, p.program.ptr_i8, p.program.struct_field_ptr(p.cur_block, array_slot, p.program.array_type, 0))
			element_type_name := p.program.array_element_type_name(local.typ_name) or {
				return p.unsupported('array type `${local.typ_name}`')
			}
			element_type := p.program.type_id(element_type_name)
			element_size := p.program.m.get_or_add_const(p.program.i64_type, p.program.m.type_size(element_type).str())
			offset := p.program.instr2(.mul, p.cur_block, p.program.i64_type, index64, element_size)
			address := p.program.instr2(.add, p.cur_block, p.program.ptr_i8, data, offset)
			typed_address := p.program.instr1(.bitcast, p.cur_block, p.program.m.type_store.get_ptr(element_type), address)
			mut assigned := value
			if assigned.typ != element_type {
				assigned = p.convert_value(assigned, element_type, element_type_name)
			}
			if op != .assign {
				current := p.program.instr1(.load, p.cur_block, element_type, typed_address)
				assigned = FastArm64Value{
					id: p.program.instr2(fast_arm64_compound_opcode(op, p.program.m.type_store.types[element_type].is_unsigned), p.cur_block, element_type, current, assigned.id)
					typ: element_type
					typ_name: element_type_name
				}
			}
			p.program.instr2(.store, p.cur_block, p.program.void_type, assigned.id, typed_address)
			return
		}
		if local.typ != p.program.map_type {
			return p.unsupported('indexed assignment to `${local.typ_name}`')
		}
		map_value := FastArm64Value{
			id: p.program.instr1(.load, p.cur_block, local.typ, local.addr)
			typ: local.typ
			typ_name: local.typ_name
			address: local.addr
		}
		if op != .assign {
			return p.unsupported('map indexed compound assignment')
		}
		p.emit_map_set(map_value, key, value)!
		return
	}
	if next_token in [.inc, .dec] {
		local := p.locals[name] or { return p.unsupported('increment of unknown `${name}`') }
		p.next()
		op := p.tok
		p.next()
		current := p.program.instr1(.load, p.cur_block, local.typ, local.addr)
		one := p.program.m.get_or_add_const(local.typ, '1')
		updated := if op == .inc {
			p.program.instr2(.add, p.cur_block, local.typ, current, one)
		} else {
			p.program.instr2(.sub, p.cur_block, local.typ, current, one)
		}
		p.program.instr2(.store, p.cur_block, p.program.void_type, updated, local.addr)
		return
	}
	if next_token == .decl_assign {
		p.next()
		p.next()
		value := p.parse_expression(0)!
		address := p.program.instr0(.alloca, p.cur_block, p.program.m.type_store.get_ptr(value.typ))
		p.program.instr2(.store, p.cur_block, p.program.void_type, value.id, address)
		p.declare_local(name, FastArm64Local{
			addr: address
			typ: value.typ
			typ_name: value.typ_name
			option_failed: value.option_failed
			option_error_type: value.option_error_type
			option_error_message: value.option_error_message
			option_error_code: value.option_error_code
			is_spawned: value.is_spawned
			spawn_handle: value.spawn_handle
			spawn_context: value.spawn_context
			spawn_context_type: value.spawn_context_type
			spawn_result_type: value.spawn_result_type
			spawn_result_name: value.spawn_result_name
		})
		return
	}
	if next_token in [.assign, .plus_assign, .minus_assign, .mul_assign, .div_assign, .mod_assign,
		.left_shift_assign, .right_shift_assign, .right_shift_unsigned_assign, .and_assign,
		.or_assign, .xor_assign] {
		local := p.locals[name] or { return p.unsupported('assignment to unknown `${name}`') }
		p.next()
		op := p.tok
		p.next()
		mut right := p.parse_contextual_value(local.typ_name)!
		if right.typ != local.typ {
			right = p.convert_value(right, local.typ, local.typ_name)
		}
		mut value := right
		if op != .assign {
			left_id := p.program.instr1(.load, p.cur_block, local.typ, local.addr)
			if op == .plus_assign && local.typ == p.program.str_type {
				value = p.emit_string_binary(.plus, FastArm64Value{
					id: left_id
					typ: local.typ
					typ_name: local.typ_name
				}, right)!
			} else {
				binary := fast_arm64_compound_opcode(op, p.program.m.type_store.types[local.typ].is_unsigned)
				value = FastArm64Value{
					id: p.program.instr2(binary, p.cur_block, local.typ, left_id, right.id)
					typ: local.typ
					typ_name: local.typ_name
				}
			}
		}
		p.program.instr2(.store, p.cur_block, p.program.void_type, value.id, local.addr)
		if op == .assign {
			p.locals[name] = FastArm64Local{
				...local
				option_failed: value.option_failed
				option_error_type: value.option_error_type
				option_error_message: value.option_error_message
				option_error_code: value.option_error_code
				is_spawned: value.is_spawned
				spawn_handle: value.spawn_handle
				spawn_context: value.spawn_context
				spawn_context_type: value.spawn_context_type
				spawn_result_type: value.spawn_result_type
				spawn_result_name: value.spawn_result_name
			}
		}
		return
	}
	if after_mut {
		return p.unsupported('mutable declaration without `:=`')
	}
	p.parse_expression_statement()!
}

// parse_expression_statement parses an expression statement, storing into
// the expression when an assignment follows it (a field of a cast pointer,
// `(&T(p)).field = v`, an indexed element, a map entry, ...).
fn (mut p FastArm64Parser) parse_expression_statement() ! {
	left := p.parse_expression(0)!
	if p.tok in [.assign, .plus_assign, .minus_assign, .mul_assign, .div_assign, .mod_assign,
		.left_shift_assign, .right_shift_assign, .right_shift_unsigned_assign, .and_assign,
		.or_assign, .xor_assign] {
		if left.address == ssa.ValueID(0) && left.map_id == ssa.ValueID(0) {
			return p.unsupported('assignment to a non-addressable value')
		}
		op := p.tok
		p.next()
		mut right := p.parse_contextual_value(left.typ_name)!
		if right.typ != left.typ {
			right = p.convert_value(right, left.typ, left.typ_name)
		}
		if left.map_id != ssa.ValueID(0) {
			if op != .assign {
				return p.unsupported('map indexed compound assignment')
			}
			p.emit_map_set(FastArm64Value{
				id: left.map_id
				typ: p.program.map_type
				typ_name: left.map_type
				address: left.map_address
			}, FastArm64Value{
				id: left.map_key_id
				typ: left.map_key_type
				typ_name: left.map_key_name
			}, right)!
			return
		}
		mut result := right.id
		if op != .assign {
			if op == .plus_assign && left.typ == p.program.str_type {
				joined := p.emit_string_binary(.plus, left, right)!
				result = joined.id
			} else {
				result = p.program.instr2(fast_arm64_compound_opcode(op, p.program.m.type_store.types[left.typ].is_unsigned), p.cur_block, left.typ, left.id, right.id)
			}
		}
		p.program.instr2(.store, p.cur_block, p.program.void_type, result, left.address)
	}
}

fn (mut p FastArm64Parser) parse_enum_shorthand(type_name string) !FastArm64Value {
	p.expect(.dot)!
	if p.tok != .name && !p.tok.is_keyword() {
		return p.unsupported('enum shorthand field')
	}
	field := p.lit
	p.next()
	module_prefix := if p.source_file.header.module_name in ['', 'main'] {
		''
	} else {
		'${p.source_file.header.module_name}.'
	}
	short_type_name := type_name.all_after_last('.').all_after_last('__')
	key := '${module_prefix}${short_type_name}.${field}'
	mut declaration := p.program.enum_values[key] or {
		p.program.enum_values['${short_type_name}.${field}'] or { FastArm64ConstantDecl{} }
	}
	if declaration.source == '' {
		// FastC signatures lower some enum aliases to their integer storage type.
		// Recover shorthand fields from the unique short enum key in that case.
		mut candidate := FastArm64ConstantDecl{}
		for enum_key, enum_declaration in p.program.enum_values {
			if enum_key.count('.') != 1 || !enum_key.ends_with('.${field}') {
				continue
			}
			if candidate.source != '' && (candidate.source != enum_declaration.source || candidate.path != enum_declaration.path) {
				candidate = FastArm64ConstantDecl{}
				break
			}
			candidate = enum_declaration
		}
		declaration = candidate
	}
	if declaration.source == '' {
		return p.unsupported('enum shorthand `${type_name}.${field}`')
	}
	mut value := p.parse_constant_declaration(declaration)!
	typ := p.program.type_id(type_name)
	value = p.convert_value(value, typ, type_name)
	return value
}

fn (mut p FastArm64Parser) parse_return() ! {
	p.next()
	if p.tok in [.semicolon, .rcbr] {
		p.emit_return_cleanup()!
		if p.return_is_option {
			p.store_option_success()
		}
		p.program.instr0(.ret, p.cur_block, p.program.void_type)
	} else {
		first_type_name := if p.return_names.len > 0 { p.return_names[0] } else { p.return_name }
		first := p.parse_contextual_value(first_type_name)!
		if p.tok == .comma {
			mut values := [first]
			for p.tok == .comma {
				p.next()
				value_index := values.len
				value_type_name := if value_index < p.return_names.len {
					p.return_names[value_index]
				} else {
					''
				}
				values << if value_type_name == '' {
					p.parse_expression(0)!
				} else {
					p.parse_contextual_value(value_type_name)!
				}
			}
			layout := p.program.m.type_store.types[p.return_typ]
			if layout.kind != .struct_t || layout.fields.len != values.len {
				return p.unsupported('multi-return layout')
			}
			slot := p.program.instr0(.alloca, p.cur_block, p.program.m.type_store.get_ptr(p.return_typ))
			for i, value in values {
				mut stored_value := value
				if stored_value.typ != layout.fields[i] {
					type_name := if i < p.return_names.len { p.return_names[i] } else { '' }
					stored_value = p.convert_value(stored_value, layout.fields[i], type_name)
				}
				address := p.program.struct_field_ptr(p.cur_block, slot, p.return_typ, i)
				p.program.instr2(.store, p.cur_block, p.program.void_type, stored_value.id, address)
			}
			mut result := FastArm64Value{
				id: p.program.instr1(.load, p.cur_block, p.return_typ, slot)
				typ: p.return_typ
				typ_name: p.return_name
			}
			p.emit_return_cleanup()!
			if p.return_is_option {
				result = p.prepare_option_return(result)
			}
			p.program.instr1(.ret, p.cur_block, p.program.void_type, result.id)
		} else {
			p.emit_return_cleanup()!
			mut result := first
			if p.return_is_option {
				result = p.prepare_option_return(first)
			} else if result.typ != p.return_typ {
				result = p.convert_value(result, p.return_typ, p.return_name)
			}
			p.program.instr1(.ret, p.cur_block, p.program.void_type, result.id)
		}
	}
	p.mark_terminated(p.cur_block)
}

fn (mut p FastArm64Parser) prepare_option_return(value FastArm64Value) FastArm64Value {
	if value.option_failed != ssa.ValueID(0) {
		p.store_option_failure(value.option_failed)
		p.store_option_error_details(value.option_error_type, value.option_error_message, value.option_error_code)
		return value
	}
	if value.is_none || value.typ_name == 'IError' || p.option_return_types_are_incompatible(value.typ, p.return_typ) {
		mut error_type := value.option_error_type
		mut error_message := value.option_error_message
		mut error_code := value.option_error_code
		if error_type == ssa.ValueID(0) && !value.is_none {
			error_type = p.program.m.get_or_add_const(p.program.u64_type, u64(value.typ).str())
		}
		if !value.is_none && value.typ_name != 'IError' {
			if error_message == ssa.ValueID(0) {
				if message := p.emit_zero_argument_method(value, 'msg') {
					error_message = message.id
				}
			}
			if error_code == ssa.ValueID(0) {
				if code := p.emit_zero_argument_method(value, 'code') {
					converted := p.convert_value(code, p.program.i32_type, 'int')
					error_code = converted.id
				}
			}
		}
		p.store_option_failure(p.program.m.get_or_add_const(p.program.i1_type, '1'))
		p.store_option_error_details(error_type, error_message, error_code)
		return p.zero_value(p.return_typ, p.return_name)
	}
	p.store_option_success()
	if value.typ != p.return_typ {
		return p.convert_value(value, p.return_typ, p.return_name)
	}
	return value
}

fn (p &FastArm64Parser) option_return_types_are_incompatible(actual ssa.TypeID, expected ssa.TypeID) bool {
	if actual == expected {
		return false
	}
	actual_kind := p.program.m.type_store.types[actual].kind
	expected_kind := p.program.m.type_store.types[expected].kind
	if actual_kind == expected_kind && actual_kind in [.int_t, .float_t, .ptr_t] {
		return false
	}
	return true
}

fn (mut p FastArm64Parser) option_state_pointer() ssa.ValueID {
	state_ptr_type := p.program.m.type_store.get_ptr(p.program.option_state_type)
	state_ref := p.program.m.add_value(.func_ref, state_ptr_type, 'fastc_option_state', p.program.fn_ids['fastc_option_state'])
	return p.program.m.add_instr(.call, p.cur_block, state_ptr_type, [state_ref])
}

fn (mut p FastArm64Parser) store_option_failure(failed ssa.ValueID) {
	state := p.option_state_pointer()
	p.program.instr2(.store, p.cur_block, p.program.void_type, failed, p.program.struct_field_ptr(p.cur_block, state, p.program.option_state_type, 0))
}

fn (mut p FastArm64Parser) store_option_error_details(error_type ssa.ValueID, error_message ssa.ValueID, error_code ssa.ValueID) {
	typ := if error_type == ssa.ValueID(0) {
		p.program.m.get_or_add_const(p.program.u64_type, '0')
	} else {
		error_type
	}
	code := if error_code == ssa.ValueID(0) {
		p.program.m.get_or_add_const(p.program.i32_type, '0')
	} else {
		error_code
	}
	message := if error_message == ssa.ValueID(0) {
		p.program.m.add_value(.string_literal, p.program.str_type, '', 0)
	} else {
		error_message
	}
	state := p.option_state_pointer()
	p.program.instr2(.store, p.cur_block, p.program.void_type, typ, p.program.struct_field_ptr(p.cur_block, state, p.program.option_state_type, 1))
	p.program.instr2(.store, p.cur_block, p.program.void_type, code, p.program.struct_field_ptr(p.cur_block, state, p.program.option_state_type, 2))
	p.program.instr2(.store, p.cur_block, p.program.void_type, message, p.program.struct_field_ptr(p.cur_block, state, p.program.option_state_type, 3))
}

fn (mut p FastArm64Parser) load_option_error_message(state ssa.ValueID) ssa.ValueID {
	return p.program.instr1(.load, p.cur_block, p.program.str_type, p.program.struct_field_ptr(p.cur_block, state, p.program.option_state_type, 3))
}

fn (mut p FastArm64Parser) store_option_success() {
	p.store_option_failure(p.program.m.get_or_add_const(p.program.i1_type, '0'))
	p.store_option_error_details(ssa.ValueID(0), ssa.ValueID(0), ssa.ValueID(0))
}

fn (mut p FastArm64Parser) emit_deferred_scopes(first_scope int) ! {
	first_defer := p.defer_starts[first_scope]
	for defer_index := p.defer_sources.len - 1; defer_index >= first_defer; defer_index-- {
		source := p.defer_sources[defer_index]
		p.parse_selected_statements(source)!
	}
}

fn (mut p FastArm64Parser) push_defer_scope() {
	p.defer_starts << p.defer_sources.len
}

fn (mut p FastArm64Parser) pop_defer_scope() {
	start := p.defer_starts.last()
	for p.defer_sources.len > start {
		p.defer_sources.delete_last()
	}
	p.defer_starts.delete_last()
}

fn (mut p FastArm64Parser) push_local_scope() {
	p.local_starts << p.local_names.len
}

fn (mut p FastArm64Parser) declare_local(name string, local FastArm64Local) {
	if previous := p.locals[name] {
		p.local_values << previous
		p.local_existed << true
	} else {
		p.local_values << FastArm64Local{}
		p.local_existed << false
	}
	p.local_names << name
	p.locals[name] = local
}

fn (mut p FastArm64Parser) pop_local_scope() {
	start := p.local_starts.last()
	for p.local_names.len > start {
		name := p.local_names.last()
		if p.local_existed.last() {
			p.locals[name] = p.local_values.last()
		} else {
			p.locals.delete(name)
		}
		p.local_names.delete_last()
		p.local_values.delete_last()
		p.local_existed.delete_last()
	}
	p.local_starts.delete_last()
}

fn (mut p FastArm64Parser) push_loop(break_to ssa.BlockID, continue_to ssa.BlockID) {
	p.break_to << break_to
	p.continue_to << continue_to
	p.break_scopes << p.defer_starts.len
	p.continue_scopes << p.defer_starts.len
	p.map_loop_writebacks << FastArm64MapLoopWriteback{}
}

fn (mut p FastArm64Parser) emit_map_loop_writeback_to(writeback FastArm64MapLoopWriteback, continuation ssa.BlockID) {
	if writeback.map_value == ssa.ValueID(0) {
		p.program.instr1(.jmp, p.cur_block, p.program.void_type, ssa.ValueID(continuation))
		p.mark_terminated(p.cur_block)
		p.cur_block = continuation
		return
	}
	current_generation := p.program.instr1(.load, p.cur_block, p.program.i64_type, p.program.struct_field_ptr(p.cur_block, writeback.state, p.program.map_state_type, 11))
	map_unchanged := p.program.instr2(.eq, p.cur_block, p.program.i1_type, current_generation, writeback.iteration_generation)
	writeback_block := p.program.m.add_block(p.func_id, 'map_collection_writeback')
	p.program.instr3(.br, p.cur_block, p.program.void_type, map_unchanged, ssa.ValueID(writeback_block), ssa.ValueID(continuation))
	p.mark_terminated(p.cur_block)
	set_ref := p.program.m.add_value(.func_ref, p.program.void_type, 'fast_map_set', p.program.fn_ids['fast_map_set'])
	writeback_key := p.program.instr1(.bitcast, writeback_block, p.program.ptr_i8, writeback.key_address)
	writeback_value := p.program.instr1(.bitcast, writeback_block, p.program.ptr_i8, writeback.value_address)
	p.program.m.add_instr(.call, writeback_block, p.program.void_type, [set_ref, writeback.map_value,
		writeback_key, writeback_value])
	p.program.instr1(.jmp, writeback_block, p.program.void_type, ssa.ValueID(continuation))
	p.mark_terminated(writeback_block)
	p.cur_block = continuation
}

fn (mut p FastArm64Parser) emit_active_map_loop_writebacks(first int, cleanup_snapshots bool) {
	for i := p.map_loop_writebacks.len - 1; i >= first; i-- {
		writeback := p.map_loop_writebacks[i]
		if writeback.map_value == ssa.ValueID(0) {
			continue
		}
		continuation := p.program.m.add_block(p.func_id, 'map_collection_exit')
		p.emit_map_loop_writeback_to(writeback, continuation)
		if cleanup_snapshots {
			free_ref := p.program.m.add_value(.func_ref, p.program.void_type, 'free', p.program.fn_ids['free'])
			keys := p.program.instr1(.load, p.cur_block, p.program.ptr_i8, writeback.snapshot_keys_slot)
			values := p.program.instr1(.load, p.cur_block, p.program.ptr_i8, writeback.snapshot_values_slot)
			p.program.m.add_instr(.call, p.cur_block, p.program.void_type, [free_ref, keys])
			p.program.m.add_instr(.call, p.cur_block, p.program.void_type, [free_ref, values])
		}
	}
}

fn (mut p FastArm64Parser) emit_return_cleanup() ! {
	p.emit_deferred_scopes(0)!
	p.emit_active_map_loop_writebacks(0, true)
}

fn (mut p FastArm64Parser) pop_loop() {
	p.break_to.delete_last()
	p.continue_to.delete_last()
	p.break_scopes.delete_last()
	p.continue_scopes.delete_last()
	p.map_loop_writebacks.delete_last()
}

fn (mut p FastArm64Parser) parse_if() ! {
	p.push_local_scope()
	p.next()
	mut condition := FastArm64Value{}
	if p.tok == .name {
		mut look := p.s
		if look.scan() == .decl_assign {
			name := p.lit
			p.next()
			p.next()
			p.last_map_found = ssa.ValueID(0)
			value := p.parse_expression(0)!
			address := p.program.instr0(.alloca, p.cur_block, p.program.m.type_store.get_ptr(value.typ))
			p.program.instr2(.store, p.cur_block, p.program.void_type, value.id, address)
			p.declare_local(name, FastArm64Local{
				addr: address
				typ: value.typ
				typ_name: value.typ_name
				option_failed: value.option_failed
				option_error_type: value.option_error_type
				option_error_message: value.option_error_message
				option_error_code: value.option_error_code
				is_spawned: value.is_spawned
				spawn_handle: value.spawn_handle
				spawn_context: value.spawn_context
				spawn_context_type: value.spawn_context_type
				spawn_result_type: value.spawn_result_type
				spawn_result_name: value.spawn_result_name
			})
			condition = if p.last_map_found != ssa.ValueID(0) {
				FastArm64Value{
					id: p.last_map_found
					typ: p.program.i1_type
					typ_name: 'bool'
				}
			} else {
				p.truthy_value(value)
			}
			p.last_map_found = ssa.ValueID(0)
		} else {
			condition = p.parse_expression(0)!
		}
	} else {
		condition = p.parse_expression(0)!
	}
	for p.tok == .semicolon {
		p.next()
	}
	if p.tok != .lcbr {
		return p.unsupported('`if` condition before `${p.tok.str()}` `${p.lit}` in `${p.program.m.funcs[p.func_id].name}`')
	}
	then_block := p.program.m.add_block(p.func_id, 'if_then')
	else_block := p.program.m.add_block(p.func_id, 'if_else')
	merge_block := p.program.m.add_block(p.func_id, 'if_merge')
	p.program.instr3(.br, p.cur_block, p.program.void_type, condition.id, ssa.ValueID(then_block), ssa.ValueID(else_block))
	p.mark_terminated(p.cur_block)
	p.cur_block = then_block
	p.parse_block()!
	then_terminated := p.block_is_terminated(p.cur_block)
	if !then_terminated {
		p.program.instr1(.jmp, p.cur_block, p.program.void_type, ssa.ValueID(merge_block))
		p.mark_terminated(p.cur_block)
	}
	p.cur_block = else_block
	for p.tok == .semicolon {
		p.next()
	}
	if p.tok == .key_else {
		p.next()
		if p.tok == .key_if {
			p.parse_if()!
		} else {
			p.parse_block()!
		}
	}
	else_terminated := p.block_is_terminated(p.cur_block)
	if !else_terminated {
		p.program.instr1(.jmp, p.cur_block, p.program.void_type, ssa.ValueID(merge_block))
		p.mark_terminated(p.cur_block)
	}
	p.cur_block = merge_block
	if then_terminated && else_terminated {
		p.program.instr0(.unreachable, merge_block, p.program.void_type)
		p.mark_terminated(merge_block)
	}
	p.pop_local_scope()
}

fn (mut p FastArm64Parser) truthy_value(value FastArm64Value) FastArm64Value {
	if value.map_found != ssa.ValueID(0) {
		return FastArm64Value{
			id: value.map_found
			typ: p.program.i1_type
			typ_name: 'bool'
		}
	}
	if value.option_failed != ssa.ValueID(0) {
		zero := p.program.m.get_or_add_const(p.program.i1_type, '0')
		return FastArm64Value{
			id: p.program.instr2(.eq, p.cur_block, p.program.i1_type, value.option_failed, zero)
			typ: p.program.i1_type
			typ_name: 'bool'
		}
	}
	mut scalar := value
	if value.typ == p.program.str_type {
		slot := p.program.instr0(.alloca, p.cur_block, p.program.m.type_store.get_ptr(value.typ))
		p.program.instr2(.store, p.cur_block, p.program.void_type, value.id, slot)
		scalar = FastArm64Value{
			id: p.program.instr1(.load, p.cur_block, p.program.i32_type, p.program.string_field_ptr(p.cur_block, slot, 1))
			typ: p.program.i32_type
			typ_name: 'int'
		}
	} else if value.typ == p.program.array_type {
		slot := p.program.instr0(.alloca, p.cur_block, p.program.m.type_store.get_ptr(value.typ))
		p.program.instr2(.store, p.cur_block, p.program.void_type, value.id, slot)
		scalar = FastArm64Value{
			id: p.program.instr1(.load, p.cur_block, p.program.i32_type, p.program.struct_field_ptr(p.cur_block, slot, value.typ, 2))
			typ: p.program.i32_type
			typ_name: 'int'
		}
	} else if p.program.m.type_store.types[value.typ].kind == .struct_t {
		return FastArm64Value{
			id: p.program.m.get_or_add_const(p.program.i1_type, '1')
			typ: p.program.i1_type
			typ_name: 'bool'
		}
	}
	zero := p.program.m.get_or_add_const(scalar.typ, '0')
	return FastArm64Value{
		id: p.program.instr2(.ne, p.cur_block, p.program.i1_type, scalar.id, zero)
		typ: p.program.i1_type
		typ_name: 'bool'
	}
}

fn (mut p FastArm64Parser) parse_for() ! {
	p.push_local_scope()
	p.parse_for_inner()!
	p.pop_local_scope()
}

fn (mut p FastArm64Parser) parse_for_inner() ! {
	p.next()
	mut value_is_mut := false
	if p.tok == .key_mut {
		value_is_mut = true
		p.next()
	}
	if p.tok == .semicolon {
		p.parse_c_for_without_initializer()!
		return
	}
	if p.tok == .name {
		mut look := p.s
		next_token := look.scan()
		if next_token == .key_in {
			p.parse_range_for(value_is_mut)!
			return
		}
		if next_token == .comma {
			p.parse_collection_for_pair()!
			return
		}
		if next_token == .decl_assign {
			p.parse_c_for()!
			return
		}
		if next_token == .assign {
			p.parse_c_for_assigning()!
			return
		}
	}
	preheader := p.cur_block
	condition_block := p.program.m.add_block(p.func_id, 'for_condition')
	body_block := p.program.m.add_block(p.func_id, 'for_body')
	done_block := p.program.m.add_block(p.func_id, 'for_done')
	p.program.instr1(.jmp, preheader, p.program.void_type, ssa.ValueID(condition_block))
	p.mark_terminated(preheader)
	p.cur_block = condition_block
	condition := if p.tok == .lcbr {
		FastArm64Value{
			id: p.program.m.get_or_add_const(p.program.i1_type, '1')
			typ: p.program.i1_type
			typ_name: 'bool'
		}
	} else {
		p.parse_expression(0)!
	}
	if p.tok != .lcbr {
		return p.unsupported('C-style and range `for` loops')
	}
	p.program.instr3(.br, p.cur_block, p.program.void_type, condition.id, ssa.ValueID(body_block), ssa.ValueID(done_block))
	p.mark_terminated(p.cur_block)
	p.push_loop(done_block, condition_block)
	p.cur_block = body_block
	p.parse_block()!
	if !p.block_is_terminated(p.cur_block) {
		p.program.instr1(.jmp, p.cur_block, p.program.void_type, ssa.ValueID(condition_block))
		p.mark_terminated(p.cur_block)
	}
	p.pop_loop()
	p.cur_block = done_block
}

fn (mut p FastArm64Parser) parse_match_statement() ! {
	p.expect(.key_match)!
	value := p.parse_expression(0)!
	p.expect(.lcbr)!
	merge_block := p.program.m.add_block(p.func_id, 'match_merge')
	mut test_block := p.cur_block
	mut has_else := false
	mut reaches_merge := false
	for p.tok !in [.rcbr, .eof] {
		if p.tok == .semicolon {
			p.next()
			continue
		}
		body_block := p.program.m.add_block(p.func_id, 'match_body')
		if p.tok == .key_else {
			has_else = true
			p.next()
			p.program.instr1(.jmp, test_block, p.program.void_type, ssa.ValueID(body_block))
			p.mark_terminated(test_block)
			p.cur_block = body_block
			p.parse_block()!
			if !p.block_is_terminated(p.cur_block) {
				p.program.instr1(.jmp, p.cur_block, p.program.void_type, ssa.ValueID(merge_block))
				p.mark_terminated(p.cur_block)
				reaches_merge = true
			}
			break
		}
		p.cur_block = test_block
		mut condition := FastArm64Value{}
		mut has_condition := false
		for {
			case_value := if p.tok == .dot {
				p.parse_enum_shorthand(value.typ_name)!
			} else {
				p.parse_contextual_value(value.typ_name)!
			}
			case_condition := p.emit_binary(.eq, value, case_value)!
			if has_condition {
				condition = FastArm64Value{
					id: p.program.instr2(.or_, p.cur_block, p.program.i1_type, condition.id, case_condition.id)
					typ: p.program.i1_type
					typ_name: 'bool'
				}
			} else {
				condition = case_condition
				has_condition = true
			}
			if p.tok != .comma {
				break
			}
			p.next()
		}
		if p.tok != .lcbr {
			return p.unsupported('match case body')
		}
		next_test := p.program.m.add_block(p.func_id, 'match_next')
		p.program.instr3(.br, p.cur_block, p.program.void_type, condition.id, ssa.ValueID(body_block), ssa.ValueID(next_test))
		p.mark_terminated(p.cur_block)
		p.cur_block = body_block
		p.parse_block()!
		if !p.block_is_terminated(p.cur_block) {
			p.program.instr1(.jmp, p.cur_block, p.program.void_type, ssa.ValueID(merge_block))
			p.mark_terminated(p.cur_block)
			reaches_merge = true
		}
		test_block = next_test
		p.cur_block = test_block
	}
	for p.tok == .semicolon {
		p.next()
	}
	p.expect(.rcbr)!
	if !has_else && !p.block_is_terminated(test_block) {
		p.program.instr1(.jmp, test_block, p.program.void_type, ssa.ValueID(merge_block))
		p.mark_terminated(test_block)
		reaches_merge = true
	}
	p.cur_block = merge_block
	if !reaches_merge {
		p.program.instr0(.unreachable, merge_block, p.program.void_type)
		p.mark_terminated(merge_block)
	}
}

fn (mut p FastArm64Parser) parse_range_for(value_is_mut bool) ! {
	name := p.lit
	p.next()
	p.expect(.key_in)!
	start := p.parse_expression(0)!
	if p.tok != .dotdot {
		return p.parse_collection_for('', name, start, value_is_mut)
	}
	p.expect(.dotdot)!
	end := p.parse_expression(0)!
	if p.tok != .lcbr {
		return p.unsupported('range loop body')
	}
	address := p.program.instr0(.alloca, p.cur_block, p.program.m.type_store.get_ptr(start.typ))
	p.program.instr2(.store, p.cur_block, p.program.void_type, start.id, address)
	p.declare_local(name, FastArm64Local{
		addr: address
		typ: start.typ
		typ_name: start.typ_name
	})
	condition_block := p.program.m.add_block(p.func_id, 'range_condition')
	body_block := p.program.m.add_block(p.func_id, 'range_body')
	increment_block := p.program.m.add_block(p.func_id, 'range_increment')
	done_block := p.program.m.add_block(p.func_id, 'range_done')
	p.program.instr1(.jmp, p.cur_block, p.program.void_type, ssa.ValueID(condition_block))
	p.mark_terminated(p.cur_block)
	index := p.program.instr1(.load, condition_block, start.typ, address)
	comparison := if p.program.m.type_store.types[start.typ].is_unsigned {
		ssa.OpCode.ult
	} else {
		ssa.OpCode.lt
	}
	more := p.program.instr2(comparison, condition_block, p.program.i1_type, index, end.id)
	p.program.instr3(.br, condition_block, p.program.void_type, more, ssa.ValueID(body_block), ssa.ValueID(done_block))
	p.mark_terminated(condition_block)
	p.push_loop(done_block, increment_block)
	p.cur_block = body_block
	p.parse_block()!
	if !p.block_is_terminated(p.cur_block) {
		p.program.instr1(.jmp, p.cur_block, p.program.void_type, ssa.ValueID(increment_block))
		p.mark_terminated(p.cur_block)
	}
	p.cur_block = increment_block
	current := p.program.instr1(.load, increment_block, start.typ, address)
	one := p.program.m.get_or_add_const(start.typ, '1')
	next := p.program.instr2(.add, increment_block, start.typ, current, one)
	p.program.instr2(.store, increment_block, p.program.void_type, next, address)
	p.program.instr1(.jmp, increment_block, p.program.void_type, ssa.ValueID(condition_block))
	p.mark_terminated(increment_block)
	p.pop_loop()
	p.cur_block = done_block
}

fn (mut p FastArm64Parser) parse_collection_for_pair() ! {
	index_name := p.lit
	p.next()
	p.expect(.comma)!
	mut value_is_mut := false
	if p.tok == .key_mut {
		value_is_mut = true
		p.next()
	}
	if p.tok != .name {
		return p.unsupported('collection loop value name')
	}
	value_name := p.lit
	p.next()
	p.expect(.key_in)!
	collection := p.parse_expression(0)!
	p.parse_collection_for(index_name, value_name, collection, value_is_mut)!
}

fn (mut p FastArm64Parser) parse_collection_for(index_name string, name string, collection FastArm64Value, value_is_mut bool) ! {
	if collection.typ == p.program.map_type {
		return p.parse_map_collection_for(index_name, name, collection, value_is_mut)
	}
	if p.tok != .lcbr || collection.typ !in [p.program.str_type, p.program.array_type] {
		return p.unsupported('collection loop')
	}
	collection_slot := p.program.instr0(.alloca, p.cur_block, p.program.m.type_store.get_ptr(collection.typ))
	p.program.instr2(.store, p.cur_block, p.program.void_type, collection.id, collection_slot)
	length_field := if collection.typ == p.program.str_type { 1 } else { 2 }
	length := p.program.instr1(.load, p.cur_block, p.program.i32_type, p.program.struct_field_ptr(p.cur_block, collection_slot, collection.typ, length_field))
	index_slot := p.program.instr0(.alloca, p.cur_block, p.program.m.type_store.get_ptr(p.program.i32_type))
	zero := p.program.m.get_or_add_const(p.program.i32_type, '0')
	p.program.instr2(.store, p.cur_block, p.program.void_type, zero, index_slot)
	condition_block := p.program.m.add_block(p.func_id, 'collection_condition')
	body_block := p.program.m.add_block(p.func_id, 'collection_body')
	increment_block := p.program.m.add_block(p.func_id, 'collection_increment')
	done_block := p.program.m.add_block(p.func_id, 'collection_done')
	p.program.instr1(.jmp, p.cur_block, p.program.void_type, ssa.ValueID(condition_block))
	p.mark_terminated(p.cur_block)
	index := p.program.instr1(.load, condition_block, p.program.i32_type, index_slot)
	more := p.program.instr2(.lt, condition_block, p.program.i1_type, index, length)
	p.program.instr3(.br, condition_block, p.program.void_type, more, ssa.ValueID(body_block), ssa.ValueID(done_block))
	p.mark_terminated(condition_block)
	p.cur_block = body_block
	data := p.program.instr1(.load, body_block, p.program.ptr_i8, p.program.struct_field_ptr(body_block, collection_slot, collection.typ, 0))
	index64 := p.program.instr1(.zext, body_block, p.program.i64_type, index)
	mut element_type := p.program.u8_type
	mut element_type_name := 'u8'
	mut offset := index64
	if collection.typ == p.program.array_type {
		element_type_name = p.program.array_element_type_name(collection.typ_name) or {
			return p.unsupported('array loop type `${collection.typ_name}`')
		}
		element_type = p.program.type_id(element_type_name)
		element_size := p.program.m.get_or_add_const(p.program.i64_type, p.program.m.type_size(element_type).str())
		offset = p.program.instr2(.mul, body_block, p.program.i64_type, index64, element_size)
	}
	element_address := p.program.instr2(.add, body_block, p.program.ptr_i8, data, offset)
	typed_address := p.program.instr1(.bitcast, body_block, p.program.m.type_store.get_ptr(element_type), element_address)
	mut local_address := typed_address
	if !value_is_mut {
		loaded_element := p.program.instr1(.load, body_block, element_type, typed_address)
		address := p.program.instr0(.alloca, body_block, p.program.m.type_store.get_ptr(element_type))
		p.program.instr2(.store, body_block, p.program.void_type, loaded_element, address)
		local_address = address
	}
	p.declare_local(name, FastArm64Local{
		addr: local_address
		typ: element_type
		typ_name: element_type_name
	})
	if index_name != '_' && index_name != '' {
		p.declare_local(index_name, FastArm64Local{
			addr: index_slot
			typ: p.program.i32_type
			typ_name: 'int'
		})
	}
	p.push_loop(done_block, increment_block)
	p.parse_block()!
	if !p.block_is_terminated(p.cur_block) {
		p.program.instr1(.jmp, p.cur_block, p.program.void_type, ssa.ValueID(increment_block))
		p.mark_terminated(p.cur_block)
	}
	p.cur_block = increment_block
	current := p.program.instr1(.load, increment_block, p.program.i32_type, index_slot)
	one := p.program.m.get_or_add_const(p.program.i32_type, '1')
	next := p.program.instr2(.add, increment_block, p.program.i32_type, current, one)
	p.program.instr2(.store, increment_block, p.program.void_type, next, index_slot)
	p.program.instr1(.jmp, increment_block, p.program.void_type, ssa.ValueID(condition_block))
	p.mark_terminated(increment_block)
	p.pop_loop()
	p.cur_block = done_block
}

fn (mut p FastArm64Parser) parse_map_collection_for(key_name string, value_name string, collection FastArm64Value, value_is_mut bool) ! {
	if p.tok != .lcbr {
		return p.unsupported('map loop body')
	}
	key_type_name, value_type_name := fastc_map_key_value_types(collection.typ_name) or {
		return p.unsupported('map loop type `${collection.typ_name}`')
	}
	key_type := p.program.type_id(key_type_name)
	value_type := p.program.type_id(value_type_name)
	map_slot := p.program.instr0(.alloca, p.cur_block, p.program.m.type_store.get_ptr(p.program.map_type))
	p.program.instr2(.store, p.cur_block, p.program.void_type, collection.id, map_slot)
	state_type := p.program.m.type_store.get_ptr(p.program.map_state_type)
	state := p.program.instr1(.load, p.cur_block, state_type, p.program.struct_field_ptr(p.cur_block, map_slot, p.program.map_type, 0))
	index_slot := p.program.instr0(.alloca, p.cur_block, p.program.m.type_store.get_ptr(p.program.i64_type))
	snapshot_keys_slot := p.program.instr0(.alloca, p.cur_block, p.program.m.type_store.get_ptr(p.program.ptr_i8))
	snapshot_values_slot := p.program.instr0(.alloca, p.cur_block, p.program.m.type_store.get_ptr(p.program.ptr_i8))
	snapshot_length_slot := p.program.instr0(.alloca, p.cur_block, p.program.m.type_store.get_ptr(p.program.i64_type))
	zero := p.program.m.get_or_add_const(p.program.i64_type, '0')
	null_pointer := p.program.m.get_or_add_const(p.program.ptr_i8, '0')
	p.program.instr2(.store, p.cur_block, p.program.void_type, zero, index_slot)
	p.program.instr2(.store, p.cur_block, p.program.void_type, zero, snapshot_length_slot)
	p.program.instr2(.store, p.cur_block, p.program.void_type, null_pointer, snapshot_keys_slot)
	p.program.instr2(.store, p.cur_block, p.program.void_type, null_pointer, snapshot_values_slot)
	snapshot_block := p.program.m.add_block(p.func_id, 'map_collection_snapshot')
	condition_block := p.program.m.add_block(p.func_id, 'map_collection_condition')
	body_block := p.program.m.add_block(p.func_id, 'map_collection_body')
	increment_block := p.program.m.add_block(p.func_id, 'map_collection_increment')
	advance_block := p.program.m.add_block(p.func_id, 'map_collection_advance')
	done_block := p.program.m.add_block(p.func_id, 'map_collection_done')
	state_bytes := p.program.instr1(.bitcast, p.cur_block, p.program.ptr_i8, state)
	has_state := p.program.instr2(.ne, p.cur_block, p.program.i1_type, state_bytes, null_pointer)
	p.program.instr3(.br, p.cur_block, p.program.void_type, has_state, ssa.ValueID(snapshot_block), ssa.ValueID(done_block))
	p.mark_terminated(p.cur_block)
	length := p.program.instr1(.load, snapshot_block, p.program.i64_type, p.program.struct_field_ptr(snapshot_block, state, p.program.map_state_type, 3))
	key_size := p.program.instr1(.load, snapshot_block, p.program.i64_type, p.program.struct_field_ptr(snapshot_block, state, p.program.map_state_type, 4))
	value_size := p.program.instr1(.load, snapshot_block, p.program.i64_type, p.program.struct_field_ptr(snapshot_block, state, p.program.map_state_type, 5))
	keys := p.program.instr1(.load, snapshot_block, p.program.ptr_i8, p.program.struct_field_ptr(snapshot_block, state, p.program.map_state_type, 0))
	values := p.program.instr1(.load, snapshot_block, p.program.ptr_i8, p.program.struct_field_ptr(snapshot_block, state, p.program.map_state_type, 1))
	key_bytes := p.program.instr2(.mul, snapshot_block, p.program.i64_type, length, key_size)
	value_bytes := p.program.instr2(.mul, snapshot_block, p.program.i64_type, length, value_size)
	malloc_ref := p.program.m.add_value(.func_ref, p.program.ptr_i8, 'malloc', p.program.fn_ids['malloc'])
	snapshot_keys := p.program.m.add_instr(.call, snapshot_block, p.program.ptr_i8, [
		malloc_ref,
		key_bytes,
	])
	snapshot_values := p.program.m.add_instr(.call, snapshot_block, p.program.ptr_i8, [
		malloc_ref,
		value_bytes,
	])
	memcpy_ref := p.program.m.add_value(.func_ref, p.program.ptr_i8, 'memcpy', p.program.fn_ids['memcpy'])
	p.program.m.add_instr(.call, snapshot_block, p.program.ptr_i8, [memcpy_ref, snapshot_keys, keys,
		key_bytes])
	p.program.m.add_instr(.call, snapshot_block, p.program.ptr_i8, [memcpy_ref, snapshot_values,
		values, value_bytes])
	p.program.instr2(.store, snapshot_block, p.program.void_type, snapshot_keys, snapshot_keys_slot)
	p.program.instr2(.store, snapshot_block, p.program.void_type, snapshot_values, snapshot_values_slot)
	p.program.instr2(.store, snapshot_block, p.program.void_type, length, snapshot_length_slot)
	p.program.instr1(.jmp, snapshot_block, p.program.void_type, ssa.ValueID(condition_block))
	p.mark_terminated(snapshot_block)
	snapshot_length := p.program.instr1(.load, condition_block, p.program.i64_type, snapshot_length_slot)
	index := p.program.instr1(.load, condition_block, p.program.i64_type, index_slot)
	more := p.program.instr2(.lt, condition_block, p.program.i1_type, index, snapshot_length)
	p.program.instr3(.br, condition_block, p.program.void_type, more, ssa.ValueID(body_block), ssa.ValueID(done_block))
	p.mark_terminated(condition_block)
	p.cur_block = body_block
	iteration_generation := p.program.instr1(.load, body_block, p.program.i64_type, p.program.struct_field_ptr(body_block, state, p.program.map_state_type, 11))
	body_keys := p.program.instr1(.load, body_block, p.program.ptr_i8, snapshot_keys_slot)
	body_values := p.program.instr1(.load, body_block, p.program.ptr_i8, snapshot_values_slot)
	key_offset := p.program.instr2(.mul, body_block, p.program.i64_type, index, key_size)
	value_offset := p.program.instr2(.mul, body_block, p.program.i64_type, index, value_size)
	key_item_bytes := p.program.instr2(.add, body_block, p.program.ptr_i8, body_keys, key_offset)
	value_item_bytes := p.program.instr2(.add, body_block, p.program.ptr_i8, body_values, value_offset)
	key_address := p.program.instr1(.bitcast, body_block, p.program.m.type_store.get_ptr(key_type), key_item_bytes)
	value_address := p.program.instr1(.bitcast, body_block, p.program.m.type_store.get_ptr(value_type), value_item_bytes)
	mut local_value_address := value_address
	if value_is_mut && key_name != '' {
		value_item := p.program.instr1(.load, body_block, value_type, value_address)
		local_value_address = p.program.instr0(.alloca, body_block, p.program.m.type_store.get_ptr(value_type))
		p.program.instr2(.store, body_block, p.program.void_type, value_item, local_value_address)
	}
	if key_name != '' && key_name != '_' {
		p.declare_local(key_name, FastArm64Local{
			addr: key_address
			typ: key_type
			typ_name: key_type_name
		})
	}
	item_name := if key_name == '' { value_name } else { value_name }
	item_address := if key_name == '' { key_address } else { local_value_address }
	item_type := if key_name == '' { key_type } else { value_type }
	item_type_name := if key_name == '' { key_type_name } else { value_type_name }
	if item_name != '_' {
		p.declare_local(item_name, FastArm64Local{
			addr: item_address
			typ: item_type
			typ_name: item_type_name
		})
	}
	p.push_loop(done_block, increment_block)
	if value_is_mut && key_name != '' {
		p.map_loop_writebacks[p.map_loop_writebacks.len - 1] = FastArm64MapLoopWriteback{
			map_value: collection.id
			state: state
			iteration_generation: iteration_generation
			key_address: key_address
			value_address: local_value_address
			snapshot_keys_slot: snapshot_keys_slot
			snapshot_values_slot: snapshot_values_slot
		}
	}
	p.parse_block()!
	if !p.block_is_terminated(p.cur_block) {
		p.program.instr1(.jmp, p.cur_block, p.program.void_type, ssa.ValueID(increment_block))
		p.mark_terminated(p.cur_block)
	}
	p.cur_block = increment_block
	p.emit_map_loop_writeback_to(p.map_loop_writebacks.last(), advance_block)
	current := p.program.instr1(.load, advance_block, p.program.i64_type, index_slot)
	one := p.program.m.get_or_add_const(p.program.i64_type, '1')
	next := p.program.instr2(.add, advance_block, p.program.i64_type, current, one)
	p.program.instr2(.store, advance_block, p.program.void_type, next, index_slot)
	p.program.instr1(.jmp, advance_block, p.program.void_type, ssa.ValueID(condition_block))
	p.mark_terminated(advance_block)
	p.pop_loop()
	p.cur_block = done_block
	free_ref := p.program.m.add_value(.func_ref, p.program.void_type, 'free', p.program.fn_ids['free'])
	keys_to_free := p.program.instr1(.load, done_block, p.program.ptr_i8, snapshot_keys_slot)
	values_to_free := p.program.instr1(.load, done_block, p.program.ptr_i8, snapshot_values_slot)
	p.program.m.add_instr(.call, done_block, p.program.void_type, [free_ref, keys_to_free])
	p.program.m.add_instr(.call, done_block, p.program.void_type, [free_ref, values_to_free])
}

fn (mut p FastArm64Parser) parse_c_for() ! {
	name := p.lit
	p.next()
	p.expect(.decl_assign)!
	initial := p.parse_expression(0)!
	address := p.program.instr0(.alloca, p.cur_block, p.program.m.type_store.get_ptr(initial.typ))
	p.program.instr2(.store, p.cur_block, p.program.void_type, initial.id, address)
	p.declare_local(name, FastArm64Local{
		addr: address
		typ: initial.typ
		typ_name: initial.typ_name
	})
	p.expect(.semicolon)!
	p.parse_c_for_rest(name, address, initial.typ, initial.typ_name)!
}

// parse_c_for_assigning lowers `for name = initial; condition; step { ... }`,
// whose initializer assigns an existing local instead of declaring one.
fn (mut p FastArm64Parser) parse_c_for_assigning() ! {
	name := p.lit
	local := p.locals[name] or { return p.unsupported('C-style loop assigning to unknown `${name}`') }
	p.next()
	p.expect(.assign)!
	mut initial := p.parse_expression(0)!
	if initial.typ != local.typ {
		initial = p.convert_value(initial, local.typ, local.typ_name)
	}
	p.program.instr2(.store, p.cur_block, p.program.void_type, initial.id, local.addr)
	p.expect(.semicolon)!
	p.parse_c_for_rest(name, local.addr, local.typ, local.typ_name)!
}

// parse_c_for_rest lowers the condition, step and body of a C-style loop whose
// counter `name` lives at `address`.
fn (mut p FastArm64Parser) parse_c_for_rest(name string, address ssa.ValueID, typ ssa.TypeID, typ_name string) ! {
	condition_block := p.program.m.add_block(p.func_id, 'c_for_condition')
	body_block := p.program.m.add_block(p.func_id, 'c_for_body')
	increment_block := p.program.m.add_block(p.func_id, 'c_for_increment')
	done_block := p.program.m.add_block(p.func_id, 'c_for_done')
	p.program.instr1(.jmp, p.cur_block, p.program.void_type, ssa.ValueID(condition_block))
	p.mark_terminated(p.cur_block)
	p.cur_block = condition_block
	condition := p.parse_expression(0)!
	p.expect(.semicolon)!
	p.program.instr3(.br, p.cur_block, p.program.void_type, condition.id, ssa.ValueID(body_block), ssa.ValueID(done_block))
	p.mark_terminated(p.cur_block)
	p.cur_block = increment_block
	if p.tok == .lcbr {
		p.program.instr1(.jmp, increment_block, p.program.void_type, ssa.ValueID(condition_block))
		p.mark_terminated(increment_block)
	} else {
		if p.tok != .name || p.lit != name {
			return p.unsupported('C-style loop increment')
		}
		p.next()
		if p.tok in [.inc, .dec] {
			current := p.program.instr1(.load, increment_block, typ, address)
			one := p.program.m.get_or_add_const(typ, '1')
			op := if p.tok == .inc { ssa.OpCode.add } else { ssa.OpCode.sub }
			next := p.program.instr2(op, increment_block, typ, current, one)
			p.program.instr2(.store, increment_block, p.program.void_type, next, address)
			p.next()
		} else if p.tok in [.plus_assign, .minus_assign, .mul_assign, .div_assign, .mod_assign,
			.left_shift_assign, .right_shift_assign, .right_shift_unsigned_assign, .and_assign,
			.or_assign, .xor_assign] {
			op := p.tok
			p.next()
			mut step := p.parse_expression(0)!
			if step.typ != typ {
				step = p.convert_value(step, typ, typ_name)
			}
			current := p.program.instr1(.load, increment_block, typ, address)
			next := p.program.instr2(fast_arm64_compound_opcode(op, p.program.m.type_store.types[typ].is_unsigned), increment_block, typ, current, step.id)
			p.program.instr2(.store, increment_block, p.program.void_type, next, address)
		} else {
			return p.unsupported('C-style loop increment operator')
		}
		p.program.instr1(.jmp, increment_block, p.program.void_type, ssa.ValueID(condition_block))
		p.mark_terminated(increment_block)
	}
	if p.tok != .lcbr {
		return p.unsupported('C-style loop body')
	}
	p.push_loop(done_block, increment_block)
	p.cur_block = body_block
	p.parse_block()!
	if !p.block_is_terminated(p.cur_block) {
		p.program.instr1(.jmp, p.cur_block, p.program.void_type, ssa.ValueID(increment_block))
		p.mark_terminated(p.cur_block)
	}
	p.pop_loop()
	p.cur_block = done_block
}

fn (mut p FastArm64Parser) parse_c_for_without_initializer() ! {
	p.expect(.semicolon)!
	condition_block := p.program.m.add_block(p.func_id, 'c_for_condition')
	body_block := p.program.m.add_block(p.func_id, 'c_for_body')
	increment_block := p.program.m.add_block(p.func_id, 'c_for_increment')
	done_block := p.program.m.add_block(p.func_id, 'c_for_done')
	p.program.instr1(.jmp, p.cur_block, p.program.void_type, ssa.ValueID(condition_block))
	p.mark_terminated(p.cur_block)
	p.cur_block = condition_block
	condition := p.parse_expression(0)!
	p.expect(.semicolon)!
	p.program.instr3(.br, p.cur_block, p.program.void_type, condition.id, ssa.ValueID(body_block), ssa.ValueID(done_block))
	p.mark_terminated(p.cur_block)
	p.cur_block = increment_block
	if p.tok == .lcbr {
		p.program.instr1(.jmp, increment_block, p.program.void_type, ssa.ValueID(condition_block))
		p.mark_terminated(increment_block)
	} else {
		if p.tok != .name {
			return p.unsupported('C-style loop increment')
		}
		name := p.lit
		local := p.locals[name] or { return p.unsupported('C-style loop variable `${name}`') }
		p.next()
		op := p.tok
		mut step := FastArm64Value{}
		if op in [.inc, .dec] {
			p.next()
			step = FastArm64Value{
				id: p.program.m.get_or_add_const(local.typ, '1')
				typ: local.typ
				typ_name: local.typ_name
			}
		} else if op in [.plus_assign, .minus_assign, .mul_assign, .div_assign, .mod_assign,
			.left_shift_assign, .right_shift_assign, .right_shift_unsigned_assign, .and_assign,
			.or_assign, .xor_assign] {
			p.next()
			step = p.parse_expression(0)!
			if step.typ != local.typ {
				step = p.convert_value(step, local.typ, local.typ_name)
			}
		} else {
			return p.unsupported('C-style loop increment operator')
		}
		current := p.program.instr1(.load, increment_block, local.typ, local.addr)
		opcode := if op == .inc {
			ssa.OpCode.add
		} else if op == .dec {
			ssa.OpCode.sub
		} else {
			fast_arm64_compound_opcode(op, p.program.m.type_store.types[local.typ].is_unsigned)
		}
		next := p.program.instr2(opcode, increment_block, local.typ, current, step.id)
		p.program.instr2(.store, increment_block, p.program.void_type, next, local.addr)
		p.program.instr1(.jmp, increment_block, p.program.void_type, ssa.ValueID(condition_block))
		p.mark_terminated(increment_block)
	}
	if p.tok != .lcbr {
		return p.unsupported('C-style loop body')
	}
	p.push_loop(done_block, increment_block)
	p.cur_block = body_block
	p.parse_block()!
	if !p.block_is_terminated(p.cur_block) {
		p.program.instr1(.jmp, p.cur_block, p.program.void_type, ssa.ValueID(increment_block))
		p.mark_terminated(p.cur_block)
	}
	p.pop_loop()
	p.cur_block = done_block
}

fn fast_arm64_precedence(tok token.Token) int {
	return match tok {
		.logical_or { 1 }
		.and { 2 }
		.eq, .ne, .key_in, .not_in, .key_is, .not_is { 3 }
		.lt, .le, .gt, .ge { 4 }
		.pipe { 5 }
		.xor { 6 }
		.amp { 7 }
		.left_shift, .right_shift, .right_shift_unsigned { 8 }
		.plus, .minus { 9 }
		.mul, .div, .mod { 10 }
		else { -1 }
	}
}

fn fast_arm64_string_quote(literal string) u8 {
	if literal.len > 1 && literal[0] == `r` && literal[1] in [`'`, `"`] {
		return literal[1]
	}
	if literal.len > 0 && literal[0] in [`'`, `"`] {
		return literal[0]
	}
	return 0
}

fn fast_arm64_string_part(literal string, strip_start bool, strip_end bool, quote u8) !string {
	mut content := literal
	mut is_raw := false
	if strip_start && content.len > 1 && content[0] == `r` && content[1] == quote {
		is_raw = true
		content = content[1..]
	}
	if strip_start && content.len > 0 && content[0] == quote {
		content = content[1..]
	}
	if strip_end && content.len > 0 && quote != 0 && content[content.len - 1] == quote {
		mut trailing_backslashes := 0
		mut i := content.len - 2
		for i >= 0 && content[i] == `\\` {
			trailing_backslashes++
			i--
		}
		if trailing_backslashes % 2 == 0 {
			content = content[..content.len - 1]
		}
	}
	if is_raw || !content.contains('\\') {
		return content
	}
	mut out := strings.new_builder(content.len)
	mut i := 0
	for i < content.len {
		if content[i] != `\\` || i + 1 >= content.len {
			out.write_u8(content[i])
			i++
			continue
		}
		next := content[i + 1]
		match next {
			`n` { out.write_u8(`\n`) }
			`r` { out.write_u8(`\r`) }
			`t` { out.write_u8(`\t`) }
			`e` { out.write_u8(0x1b) }
			`a` { out.write_u8(0x07) }
			`b` { out.write_u8(0x08) }
			`f` { out.write_u8(0x0c) }
			`v` { out.write_u8(0x0b) }
			`\\`, `'`, `"`, `$`, `\``, `@`, `?`, `{`, `}` { out.write_u8(next) }
			`x` {
				if i + 3 >= content.len {
					return error('invalid hexadecimal string escape')
				}
				high := fastc_hex_digit_value(content[i + 2])!
				low := fastc_hex_digit_value(content[i + 3])!
				out.write_u8((high << 4) | low)
				i += 2
			}
			else {
				return error('unsupported string escape `\\${next.ascii_str()}`')
			}
		}
		i += 2
	}
	return out.str()
}

fn fast_arm64_string_value(literal string) !string {
	quote := fast_arm64_string_quote(literal)
	return fast_arm64_string_part(literal, quote != 0, quote != 0, quote)
}

fn fast_arm64_integer_literal_magnitude(literal string) u64 {
	mut base := u64(10)
	mut start := 0
	if literal.len > 2 && literal[0] == `0` {
		match literal[1] {
			`x`, `X` {
				base = 16
				start = 2
			}
			`b`, `B` {
				base = 2
				start = 2
			}
			`o`, `O` {
				base = 8
				start = 2
			}
			else {}
		}
	}
	mut magnitude := u64(0)
	for i in start .. literal.len {
		c := literal[i]
		mut digit := u64(base)
		if c >= `0` && c <= `9` {
			digit = u64(c - `0`)
		} else if c >= `a` && c <= `f` {
			digit = u64(c - `a` + 10)
		} else if c >= `A` && c <= `F` {
			digit = u64(c - `A` + 10)
		}
		if digit < base {
			magnitude = magnitude * base + digit
		}
	}
	return magnitude
}

fn (mut p FastArm64Parser) parse_expression(min_precedence int) !FastArm64Value {
	mut left := p.parse_prefix()!
	for {
		if p.tok == .semicolon {
			mut look := p.s
			if fast_arm64_precedence(look.scan()) >= 0 {
				p.next()
				continue
			}
		}
		precedence := fast_arm64_precedence(p.tok)
		if precedence < min_precedence {
			break
		}
		op := p.tok
		p.next()
		if op in [.key_is, .not_is] {
			if p.tok != .name {
				return p.unsupported('type test target')
			}
			mut target_type := p.source_type_id(p.lit)
			qualifier := p.lit
			p.next()
			if p.tok == .dot {
				p.next()
				if p.tok != .name {
					return p.unsupported('qualified type test target')
				}
				module_name := p.source_file.header.imports[qualifier] or { qualifier }
				target_type = p.program.type_id(fastc_type_key(module_name, p.lit))
				p.next()
			}
			mut condition := p.program.m.get_or_add_const(p.program.i1_type, '0')
			if left.option_error_type != ssa.ValueID(0) {
				target_tag := p.program.m.get_or_add_const(p.program.u64_type, u64(target_type).str())
				opcode := if op == .key_is { ssa.OpCode.eq } else { ssa.OpCode.ne }
				condition = p.program.instr2(opcode, p.cur_block, p.program.i1_type, left.option_error_type, target_tag)
			} else {
				matches := left.typ == target_type
				result := if (op == .key_is && matches) || (op == .not_is && !matches) {
					'1'
				} else {
					'0'
				}
				condition = p.program.m.get_or_add_const(p.program.i1_type, result)
			}
			left = FastArm64Value{
				id: condition
				typ: p.program.i1_type
				typ_name: 'bool'
			}
			continue
		}
		if op in [.and, .logical_or] {
			if left.typ != p.program.i1_type {
				return p.unsupported('logical operator `${op.str()}` with `${left.typ_name}`')
			}
			result_slot := p.program.instr0(.alloca, p.cur_block, p.program.m.type_store.get_ptr(p.program.i1_type))
			p.program.instr2(.store, p.cur_block, p.program.void_type, left.id, result_slot)
			right_block := p.program.m.add_block(p.func_id, 'logical_right')
			done_block := p.program.m.add_block(p.func_id, 'logical_done')
			if op == .and {
				p.program.instr3(.br, p.cur_block, p.program.void_type, left.id, ssa.ValueID(right_block), ssa.ValueID(done_block))
			} else {
				p.program.instr3(.br, p.cur_block, p.program.void_type, left.id, ssa.ValueID(done_block), ssa.ValueID(right_block))
			}
			p.mark_terminated(p.cur_block)
			p.cur_block = right_block
			right := p.parse_expression(precedence + 1)!
			if right.typ != p.program.i1_type {
				return p.unsupported('logical operator `${op.str()}` with `${right.typ_name}`')
			}
			p.program.instr2(.store, p.cur_block, p.program.void_type, right.id, result_slot)
			p.program.instr1(.jmp, p.cur_block, p.program.void_type, ssa.ValueID(done_block))
			p.mark_terminated(p.cur_block)
			p.cur_block = done_block
			left = FastArm64Value{
				id: p.program.instr1(.load, done_block, p.program.i1_type, result_slot)
				typ: p.program.i1_type
				typ_name: 'bool'
				address: result_slot
			}
			continue
		}
		contextual_right_type := if op in [.key_in, .not_in] && p.tok == .lsbr {
			fastc_array_c_type(left.typ_name)
		} else if op == .left_shift && left.typ == p.program.array_type {
			p.program.array_element_type_name(left.typ_name) or { '' }
		} else {
			''
		}
		right := if contextual_right_type != '' {
			p.parse_contextual_value_with_precedence(contextual_right_type, precedence + 1)!
		} else if p.tok == .dot {
			p.parse_enum_shorthand(left.typ_name)!
		} else {
			p.parse_expression(precedence + 1)!
		}
		left = p.emit_binary(op, left, right)!
	}
	return left
}

fn (mut p FastArm64Parser) parse_prefix() !FastArm64Value {
	if p.tok == .and {
		p.next()
		if p.tok != .name {
			return p.unsupported('double-pointer cast type')
		}
		type_name := p.lit
		p.next()
		p.expect(.lpar)!
		value := p.parse_expression(0)!
		p.expect(.rpar)!
		target := p.program.m.type_store.get_ptr(p.program.m.type_store.get_ptr(p.program.type_id(type_name)))
		return FastArm64Value{
			id: p.program.instr1(.bitcast, p.cur_block, target, value.id)
			typ: target
			typ_name: '&&${type_name}'
		}
	}
	if p.tok == .amp {
		p.next()
		if p.tok == .name && p.lit == 'C' {
			mut c_look := p.s
			if c_look.scan() == .dot && c_look.scan() == .name && c_look.scan() == .lpar {
				p.next()
				p.expect(.dot)!
				c_type_name := p.lit
				p.next()
				p.expect(.lpar)!
				value := p.parse_expression(0)!
				p.expect(.rpar)!
				target := if c_type_name == 'FILE' {
					p.program.ptr_i8
				} else {
					p.program.m.type_store.get_ptr(p.program.type_id('C.${c_type_name}'))
				}
				return FastArm64Value{
					id: p.program.instr1(.bitcast, p.cur_block, target, value.id)
					typ: target
					typ_name: '&C.${c_type_name}'
				}
			}
		}
		if p.tok == .lsbr {
			// `&[]T(value)` reinterprets a pointer as a pointer to an array; every
			// V array shares the runtime array layout, so this is a bitcast.
			mut array_look := p.s
			if array_look.scan() == .rsbr && array_look.scan() == .name && array_look.scan() == .lpar {
				p.next()
				p.expect(.rsbr)!
				type_name := '[]' + p.lit
				p.next()
				p.expect(.lpar)!
				value := p.parse_expression(0)!
				p.expect(.rpar)!
				target := p.program.m.type_store.get_ptr(p.program.type_id(type_name))
				// A pointer operand is reinterpreted; an addressable value (such as
				// a `mut` receiver) contributes its address.
				mut operand := value.id
				if p.program.m.type_store.types[value.typ].kind != .ptr_t
					&& value.address != ssa.ValueID(0) {
					operand = value.address
				}
				return FastArm64Value{
					id: p.program.instr1(.bitcast, p.cur_block, target, operand)
					typ: target
					typ_name: '&${type_name}'
				}
			}
		}
		if p.tok == .name && (p.lit in ['bool', 'i8', 'char', 'i16', 'int', 'i32', 'rune', 'i64',
			'u8', 'byte', 'u16', 'u32', 'u64', 'isize', 'usize', 'f32', 'f64', 'voidptr', 'byteptr',
			'charptr'] || p.lit in p.program.type_ids || p.lit in p.program.type_aliases) {
			mut look := p.s
			if look.scan() == .lpar {
				type_name := p.lit
				p.next()
				p.expect(.lpar)!
				value := p.parse_expression(0)!
				p.expect(.rpar)!
				target := p.program.m.type_store.get_ptr(p.program.type_id(type_name))
				return FastArm64Value{
					id: p.program.instr1(.bitcast, p.cur_block, target, value.id)
					typ: target
					typ_name: '&${type_name}'
				}
			}
		}
		value := p.parse_prefix()!
		if value.is_temporary {
			pointer_type := p.program.m.type_store.get_ptr(value.typ)
			size := p.program.m.get_or_add_const(p.program.i64_type, p.program.m.type_size(value.typ).str())
			malloc_ref := p.program.m.add_value(.func_ref, p.program.ptr_i8, 'malloc', p.program.fn_ids['malloc'])
			bytes := p.program.m.add_instr(.call, p.cur_block, p.program.ptr_i8, [
				malloc_ref,
				size,
			])
			pointer := p.program.instr1(.bitcast, p.cur_block, pointer_type, bytes)
			p.program.instr2(.store, p.cur_block, p.program.void_type, value.id, pointer)
			return FastArm64Value{
				id: pointer
				typ: pointer_type
				typ_name: '&${value.typ_name}'
			}
		}
		if value.address != ssa.ValueID(0) {
			return FastArm64Value{
				id: value.address
				typ: p.program.m.type_store.get_ptr(value.typ)
				typ_name: '&${value.typ_name}'
			}
		}
		slot := p.program.instr0(.alloca, p.cur_block, p.program.m.type_store.get_ptr(value.typ))
		p.program.instr2(.store, p.cur_block, p.program.void_type, value.id, slot)
		return FastArm64Value{
			id: slot
			typ: p.program.m.type_store.get_ptr(value.typ)
			typ_name: '&${value.typ_name}'
		}
	}
	if p.tok == .mul {
		p.next()
		pointer := p.parse_prefix()!
		layout := p.program.m.type_store.types[pointer.typ]
		if layout.kind != .ptr_t {
			return p.unsupported('dereference of a non-pointer')
		}
		return FastArm64Value{
			id: p.program.instr1(.load, p.cur_block, layout.elem_type, pointer.id)
			typ: layout.elem_type
			typ_name: pointer.typ_name.trim_left('&')
			address: pointer.id
		}
	}
	if p.tok in [.minus, .not, .bit_not, .plus] {
		op := p.tok
		p.next()
		value := p.parse_prefix()!
		if op == .plus {
			return value
		}
		zero := p.program.m.get_or_add_const(value.typ, '0')
		if op == .minus {
			return FastArm64Value{
				id: p.program.instr2(.sub, p.cur_block, value.typ, zero, value.id)
				typ: value.typ
				typ_name: value.typ_name
			}
		}
		if op == .not {
			return FastArm64Value{
				id: p.program.instr2(.eq, p.cur_block, p.program.i1_type, value.id, zero)
				typ: p.program.i1_type
				typ_name: 'bool'
			}
		}
		minus_one := p.program.m.get_or_add_const(value.typ, '-1')
		return FastArm64Value{
			id: p.program.instr2(.xor, p.cur_block, value.typ, value.id, minus_one)
			typ: value.typ
			typ_name: value.typ_name
		}
	}
	return p.parse_primary()
}

fn (mut p FastArm64Parser) parse_primary() !FastArm64Value {
	mut value := p.parse_atom()!
	for p.tok in [.lsbr, .dot] {
		if p.tok == .lsbr {
			value = p.parse_array_index_or_slice(value)!
		} else {
			value = p.parse_selector(value)!
		}
	}
	if p.tok in [.inc, .dec] {
		if value.address == ssa.ValueID(0) {
			return p.unsupported('increment of a non-addressable value')
		}
		op := p.tok
		p.next()
		one := p.program.m.get_or_add_const(value.typ, '1')
		updated := if op == .inc {
			p.program.instr2(.add, p.cur_block, value.typ, value.id, one)
		} else {
			p.program.instr2(.sub, p.cur_block, value.typ, value.id, one)
		}
		p.program.instr2(.store, p.cur_block, p.program.void_type, updated, value.address)
	}
	if p.tok in [.not, .question] {
		p.next()
		value = p.propagate_option_failure(value)!
	}
	if p.tok == .key_or {
		value = p.parse_option_handler(value)!
	}
	return value
}

fn (mut p FastArm64Parser) value_option_failure(value FastArm64Value) ssa.ValueID {
	if value.option_failed != ssa.ValueID(0) {
		return value.option_failed
	}
	if value.map_found != ssa.ValueID(0) {
		zero := p.program.m.get_or_add_const(p.program.i1_type, '0')
		return p.program.instr2(.eq, p.cur_block, p.program.i1_type, value.map_found, zero)
	}
	return ssa.ValueID(0)
}

fn (mut p FastArm64Parser) propagate_option_failure(value FastArm64Value) !FastArm64Value {
	failed := p.value_option_failure(value)
	if failed == ssa.ValueID(0) {
		return value
	}
	failure_block := p.program.m.add_block(p.func_id, 'option_propagate_failure')
	success_block := p.program.m.add_block(p.func_id, 'option_propagate_success')
	p.program.instr3(.br, p.cur_block, p.program.void_type, failed, ssa.ValueID(failure_block), ssa.ValueID(success_block))
	p.mark_terminated(p.cur_block)
	p.cur_block = failure_block
	p.emit_return_cleanup()!
	if p.return_is_option {
		p.store_option_error_details(value.option_error_type, value.option_error_message, value.option_error_code)
		p.store_option_failure(p.program.m.get_or_add_const(p.program.i1_type, '1'))
		if p.return_typ == p.program.void_type {
			p.program.instr0(.ret, p.cur_block, p.program.void_type)
		} else {
			result := p.zero_value(p.return_typ, p.return_name)
			p.program.instr1(.ret, p.cur_block, p.program.void_type, result.id)
		}
	} else {
		exit_ref := p.program.m.add_value(.func_ref, p.program.void_type, 'exit', p.program.fn_ids['exit'])
		exit_code := p.program.m.get_or_add_const(p.program.i32_type, '1')
		p.program.m.add_instr(.call, p.cur_block, p.program.void_type, [exit_ref, exit_code])
		p.program.instr0(.unreachable, p.cur_block, p.program.void_type)
	}
	p.mark_terminated(p.cur_block)
	p.cur_block = success_block
	return FastArm64Value{
		...value
		option_failed: ssa.ValueID(0)
		option_error_type: ssa.ValueID(0)
		option_error_message: ssa.ValueID(0)
		option_error_code: ssa.ValueID(0)
		map_found: ssa.ValueID(0)
	}
}

fn (mut p FastArm64Parser) parse_option_handler(value FastArm64Value) !FastArm64Value {
	failed := p.value_option_failure(value)
	p.next()
	if failed == ssa.ValueID(0) {
		p.skip_group(.lcbr, .rcbr)!
		return value
	}
	mut result_slot := ssa.ValueID(0)
	if value.typ != p.program.void_type {
		result_slot = p.program.instr0(.alloca, p.cur_block, p.program.m.type_store.get_ptr(value.typ))
		p.program.instr2(.store, p.cur_block, p.program.void_type, value.id, result_slot)
	}
	handler_block := p.program.m.add_block(p.func_id, 'option_handler')
	done_block := p.program.m.add_block(p.func_id, 'option_handler_done')
	p.program.instr3(.br, p.cur_block, p.program.void_type, failed, ssa.ValueID(handler_block), ssa.ValueID(done_block))
	p.mark_terminated(p.cur_block)
	p.cur_block = handler_block
	fallback, has_fallback := p.parse_option_handler_block(value)!
	if !p.block_is_terminated(p.cur_block) {
		if value.typ != p.program.void_type {
			if !has_fallback {
				return p.unsupported('non-terminating `or` block without a fallback value')
			}
			mut converted := fallback
			if converted.typ != value.typ {
				converted = p.convert_value(converted, value.typ, value.typ_name)
			}
			p.program.instr2(.store, p.cur_block, p.program.void_type, converted.id, result_slot)
		}
		p.program.instr1(.jmp, p.cur_block, p.program.void_type, ssa.ValueID(done_block))
		p.mark_terminated(p.cur_block)
	}
	p.cur_block = done_block
	if value.typ == p.program.void_type {
		return FastArm64Value{
			typ: p.program.void_type
			typ_name: value.typ_name
		}
	}
	return FastArm64Value{
		id: p.program.instr1(.load, done_block, value.typ, result_slot)
		typ: value.typ
		typ_name: value.typ_name
		tuple_types: value.tuple_types
		address: result_slot
	}
}

fn (mut p FastArm64Parser) parse_option_handler_block(option_value FastArm64Value) !(FastArm64Value, bool) {
	p.push_local_scope()
	p.push_defer_scope()
	p.expect(.lcbr)!
	err_type := p.program.type_id('IError')
	err_value := p.zero_value(err_type, 'IError')
	mut err_address := err_value.address
	if err_address == ssa.ValueID(0) {
		err_address = p.program.instr0(.alloca, p.cur_block, p.program.m.type_store.get_ptr(err_type))
		p.program.instr2(.store, p.cur_block, p.program.void_type, err_value.id, err_address)
	}
	p.declare_local('err', FastArm64Local{
		addr: err_address
		typ: err_type
		typ_name: 'IError'
		option_error_type: option_value.option_error_type
		option_error_message: option_value.option_error_message
		option_error_code: option_value.option_error_code
	})
	mut fallback := FastArm64Value{}
	mut has_fallback := false
	for p.tok !in [.rcbr, .eof] {
		if p.tok == .semicolon {
			p.next()
			continue
		}
		if p.block_is_terminated(p.cur_block) {
			for p.tok !in [.rcbr, .eof] {
				p.next()
			}
			break
		}
		if p.option_handler_token_is_statement() {
			p.parse_statement()!
			continue
		}
		candidate := if p.option_handler_expression_is_final() {
			p.parse_contextual_value(option_value.typ_name)!
		} else {
			p.parse_expression(0)!
		}
		mut is_last := p.tok == .rcbr
		if p.tok == .semicolon {
			p.next()
			is_last = p.tok == .rcbr
		}
		if is_last {
			fallback = candidate
			has_fallback = true
		}
	}
	if !p.block_is_terminated(p.cur_block) {
		p.emit_deferred_scopes(p.defer_starts.len - 1)!
	}
	p.expect(.rcbr)!
	p.pop_defer_scope()
	p.pop_local_scope()
	return fallback, has_fallback
}

fn (p &FastArm64Parser) option_handler_expression_is_final() bool {
	mut look := p.s
	mut parentheses := if p.tok == .lpar { 1 } else { 0 }
	mut brackets := if p.tok == .lsbr { 1 } else { 0 }
	mut braces := if p.tok == .lcbr { 1 } else { 0 }
	for {
		tok := look.scan()
		match tok {
			.lpar { parentheses++ }
			.rpar { parentheses-- }
			.lsbr { brackets++ }
			.rsbr { brackets-- }
			.lcbr { braces++ }
			.rcbr {
				if parentheses == 0 && brackets == 0 && braces == 0 {
					return true
				}
				braces--
			}
			.semicolon {
				if parentheses == 0 && brackets == 0 && braces == 0 {
					mut next := look.scan()
					for next == .semicolon {
						next = look.scan()
					}
					return next == .rcbr
				}
			}
			.eof {
				return false
			}
			else {}
		}
	}
	return false
}

fn (mut p FastArm64Parser) option_handler_token_is_statement() bool {
	if p.tok in [.key_mut, .key_return, .key_if, .key_for, .key_defer, .key_match, .key_break,
		.key_continue, .key_goto, .key_unsafe] {
		return true
	}
	if p.tok != .name {
		return false
	}
	mut look := p.s
	mut parentheses := 0
	mut brackets := 0
	mut braces := 0
	for {
		tok := look.scan()
		if tok == .eof {
			return false
		}
		if tok == .lpar {
			parentheses++
		} else if tok == .rpar {
			parentheses--
		} else if tok == .lsbr {
			brackets++
		} else if tok == .rsbr {
			brackets--
		} else if tok == .lcbr {
			braces++
		} else if tok == .rcbr {
			if parentheses == 0 && brackets == 0 && braces == 0 {
				return false
			}
			braces--
		} else if tok == .semicolon && parentheses == 0 && brackets == 0 && braces == 0 {
			return false
		} else if tok in [.assign, .decl_assign, .inc, .dec] && parentheses == 0 && brackets == 0 && braces == 0 {
			return true
		}
	}
	return false
}

fn (mut p FastArm64Parser) parse_atom() !FastArm64Value {
	match p.tok {
		.number {
			literal := p.lit.replace('_', '')
			is_based_integer := literal.starts_with('0x') || literal.starts_with('0X') || literal.starts_with('0b') || literal.starts_with('0B') || literal.starts_with('0o') || literal.starts_with('0O')
			is_float := !is_based_integer && (literal.contains('.') || literal.contains('e') || literal.contains('E'))
			typ := if is_float {
				p.program.f64_type
			} else if fast_arm64_integer_literal_magnitude(literal) > 0x7fffffff {
				p.program.i64_type
			} else {
				p.program.i32_type
			}
			p.next()
			return FastArm64Value{
				id: p.program.m.get_or_add_const(typ, literal)
				typ: typ
				typ_name: if is_float { 'f64' } else { 'int' }
			}
		}
		.string {
			literal_source := p.lit
			quote := fast_arm64_string_quote(literal_source)
			p.next()
			if p.tok == .str_dollar {
				return p.parse_interpolated_string(literal_source, quote)
			}
			literal := fast_arm64_string_part(literal_source, quote != 0, quote != 0, quote) or {
				return p.unsupported(err.msg())
			}
			return FastArm64Value{
				id: p.program.m.add_value(.string_literal, p.program.str_type, literal, 0)
				typ: p.program.str_type
				typ_name: 'string'
			}
		}
		.char {
			// Scanner character token payloads do not include their backtick delimiters.
			// Decode escapes directly so a payload like `'` remains the apostrophe byte
			// used by the scanner's own single-quoted-string recognition.
			decoded := fast_arm64_string_part(p.lit, false, false, 0) or {
				return p.unsupported(err.msg())
			}
			literal := if decoded.len > 0 { int(decoded.runes()[0]).str() } else { '0' }
			p.next()
			return FastArm64Value{
				id: p.program.m.get_or_add_const(p.program.i32_type, literal)
				typ: p.program.i32_type
				typ_name: 'rune'
			}
		}
		.key_true, .key_false {
			literal := if p.tok == .key_true { '1' } else { '0' }
			p.next()
			return FastArm64Value{
				id: p.program.m.get_or_add_const(p.program.i1_type, literal)
				typ: p.program.i1_type
				typ_name: 'bool'
			}
		}
		.key_none {
			p.next()
			value := p.zero_value(p.return_typ, '')
			return FastArm64Value{
				id: value.id
				typ: value.typ
				typ_name: value.typ_name
				address: value.address
				tuple_types: value.tuple_types
				is_none: true
			}
		}
		.key_nil {
			p.next()
			return p.zero_value(p.program.ptr_i8, 'voidptr')
		}
		.lpar {
			p.next()
			value := p.parse_expression(0)!
			p.expect(.rpar)!
			return value
		}
		.key_if {
			return p.parse_if_expression('')
		}
		.key_match {
			return p.parse_match_expression()
		}
		.key_sizeof {
			return p.parse_sizeof_expression()
		}
		.key_unsafe {
			p.next()
			p.expect(.lcbr)!
			value := p.parse_expression(0)!
			for p.tok == .semicolon {
				p.next()
			}
			p.expect(.rcbr)!
			return value
		}
		.key_spawn {
			p.next()
			if p.program.prefs.building_v {
				// Keep the native bootstrap's compiler phases serial: its worker
				// pools are deliberately disabled while bootstrapping.
				value := p.parse_atom()!
				return FastArm64Value{
					...value
					is_spawned: true
				}
			}
			p.parsing_spawn = true
			value := p.parse_atom() or {
				p.parsing_spawn = false
				return err
			}
			if p.parsing_spawn {
				p.parsing_spawn = false
				return p.unsupported('spawn expression without a function call')
			}
			return FastArm64Value{
				...value
				is_spawned: true
			}
		}
		.dollar {
			return p.parse_comptime_if_expression()
		}
		.lsbr {
			return p.parse_array_literal()
		}
		.lcbr {
			return p.parse_inferred_map_literal()
		}
		.name {
			return p.parse_name_expression()
		}
		else {
			function_name := p.program.m.funcs[p.func_id].name
			return p.unsupported('expression token `${p.tok.str()}` `${p.lit}` in `${function_name}`')
		}
	}
}

fn (mut p FastArm64Parser) parse_match_expression() !FastArm64Value {
	p.expect(.key_match)!
	value := p.parse_expression(0)!
	p.expect(.lcbr)!
	merge_block := p.program.m.add_block(p.func_id, 'match_expr_merge')
	option_failed_slot := p.program.instr0(.alloca, p.cur_block, p.program.m.type_store.get_ptr(p.program.i1_type))
	option_error_type_slot := p.program.instr0(.alloca, p.cur_block, p.program.m.type_store.get_ptr(p.program.u64_type))
	option_error_code_slot := p.program.instr0(.alloca, p.cur_block, p.program.m.type_store.get_ptr(p.program.i32_type))
	option_error_message_slot := p.program.instr0(.alloca, p.cur_block, p.program.m.type_store.get_ptr(p.program.str_type))
	mut test_block := p.cur_block
	mut result_slot := ssa.ValueID(0)
	mut result_type := ssa.TypeID(0)
	mut result_name := ''
	mut result_tuple_types := []string{}
	mut has_option_metadata := false
	mut has_else := false
	for p.tok !in [.rcbr, .eof] {
		if p.tok == .semicolon {
			p.next()
			continue
		}
		body_block := p.program.m.add_block(p.func_id, 'match_expr_body')
		if p.tok == .key_else {
			has_else = true
			p.next()
			p.program.instr1(.jmp, test_block, p.program.void_type, ssa.ValueID(body_block))
			p.mark_terminated(test_block)
		} else {
			p.cur_block = test_block
			mut condition := FastArm64Value{}
			mut has_condition := false
			for {
				case_value := if p.tok == .dot {
					p.parse_enum_shorthand(value.typ_name)!
				} else {
					p.parse_contextual_value(value.typ_name)!
				}
				case_condition := p.emit_binary(.eq, value, case_value)!
				if has_condition {
					condition = FastArm64Value{
						id: p.program.instr2(.or_, p.cur_block, p.program.i1_type, condition.id, case_condition.id)
						typ: p.program.i1_type
						typ_name: 'bool'
					}
				} else {
					condition = case_condition
					has_condition = true
				}
				if p.tok != .comma {
					break
				}
				p.next()
			}
			next_test := p.program.m.add_block(p.func_id, 'match_expr_test')
			p.program.instr3(.br, p.cur_block, p.program.void_type, condition.id, ssa.ValueID(body_block), ssa.ValueID(next_test))
			p.mark_terminated(p.cur_block)
			test_block = next_test
		}
		p.cur_block = body_block
		p.expect(.lcbr)!
		mut arm_value := FastArm64Value{}
		mut arm_terminated := false
		if p.tok == .key_return {
			p.parse_return()!
			arm_terminated = true
		} else {
			expected_arm_type := if result_tuple_types.len > 0 {
				result_tuple_types[0]
			} else {
				result_name
			}
			arm_value = if result_slot == ssa.ValueID(0) {
				p.parse_expression(0)!
			} else {
				p.parse_contextual_value(expected_arm_type)!
			}
			if p.tok == .comma {
				mut values := [arm_value]
				for p.tok == .comma {
					p.next()
					value_index := values.len
					values << if result_slot != ssa.ValueID(0)
						&& value_index < result_tuple_types.len {
						p.parse_contextual_value(result_tuple_types[value_index])!
					} else {
						p.parse_expression(0)!
					}
				}
				mut types := []ssa.TypeID{cap: values.len}
				mut type_names := []string{cap: values.len}
				for item in values {
					types << item.typ
					type_names << item.typ_name
				}
				tuple_type := p.program.m.type_store.get_tuple(types)
				tuple_slot := p.program.instr0(.alloca, p.cur_block, p.program.m.type_store.get_ptr(tuple_type))
				for i, item in values {
					address := p.program.struct_field_ptr(p.cur_block, tuple_slot, tuple_type, i)
					p.program.instr2(.store, p.cur_block, p.program.void_type, item.id, address)
				}
				arm_value = FastArm64Value{
					id: p.program.instr1(.load, p.cur_block, tuple_type, tuple_slot)
					typ: tuple_type
					typ_name: 'MultiReturn'
					tuple_types: type_names
				}
			}
		}
		for p.tok == .semicolon {
			p.next()
		}
		p.expect(.rcbr)!
		if !arm_terminated {
			has_option_metadata = has_option_metadata || p.if_expression_value_has_option_metadata(arm_value)
			p.store_if_expression_option_metadata(arm_value, option_failed_slot, option_error_type_slot, option_error_code_slot, option_error_message_slot)
			if result_slot == ssa.ValueID(0) {
				result_type = arm_value.typ
				result_name = arm_value.typ_name
				result_tuple_types = arm_value.tuple_types.clone()
				result_slot = p.program.instr0(.alloca, p.cur_block, p.program.m.type_store.get_ptr(result_type))
			} else if arm_value.typ != result_type {
				arm_value = p.convert_value(arm_value, result_type, result_name)
			}
			p.program.instr2(.store, p.cur_block, p.program.void_type, arm_value.id, result_slot)
			p.program.instr1(.jmp, p.cur_block, p.program.void_type, ssa.ValueID(merge_block))
			p.mark_terminated(p.cur_block)
		}
		if has_else {
			break
		}
	}
	for p.tok == .semicolon {
		p.next()
	}
	p.expect(.rcbr)!
	if !has_else {
		p.program.instr1(.jmp, test_block, p.program.void_type, ssa.ValueID(merge_block))
		p.mark_terminated(test_block)
	}
	if result_slot == ssa.ValueID(0) {
		return p.unsupported('empty match expression')
	}
	p.cur_block = merge_block
	return FastArm64Value{
		id: p.program.instr1(.load, merge_block, result_type, result_slot)
		typ: result_type
		typ_name: result_name
		tuple_types: result_tuple_types
		option_failed: if has_option_metadata {
			p.program.instr1(.load, merge_block, p.program.i1_type, option_failed_slot)
		} else {
			ssa.ValueID(0)
		}
		option_error_type: if has_option_metadata {
			p.program.instr1(.load, merge_block, p.program.u64_type, option_error_type_slot)
		} else {
			ssa.ValueID(0)
		}
		option_error_code: if has_option_metadata {
			p.program.instr1(.load, merge_block, p.program.i32_type, option_error_code_slot)
		} else {
			ssa.ValueID(0)
		}
		option_error_message: if has_option_metadata {
			p.program.instr1(.load, merge_block, p.program.str_type, option_error_message_slot)
		} else {
			ssa.ValueID(0)
		}
	}
}

fn (mut p FastArm64Parser) parse_sizeof_expression() !FastArm64Value {
	p.expect(.key_sizeof)!
	p.expect(.lpar)!
	if p.tok == .name {
		mut look := p.s
		if look.scan() == .rpar && (p.lit in ['bool', 'i8', 'char', 'i16', 'int', 'i32', 'rune',
			'i64', 'u8', 'byte', 'u16', 'u32', 'u64', 'isize', 'usize', 'f32', 'f64', 'voidptr',
			'byteptr', 'charptr', 'string'] || p.lit in p.program.type_ids || p.lit in p.program.type_aliases) {
			type_name := p.lit
			p.next()
			p.expect(.rpar)!
			typ := p.program.type_id(type_name)
			return FastArm64Value{
				id: p.program.m.get_or_add_const(p.program.i64_type, p.program.m.type_size(typ).str())
				typ: p.program.i64_type
				typ_name: 'usize'
			}
		}
	}
	mut checkpoint := p.emission_checkpoint()
	previous_suppress := p.suppress_spawn_wrapper
	p.suppress_spawn_wrapper = true
	value := p.parse_expression(0) or {
		p.suppress_spawn_wrapper = previous_suppress
		p.discard_emission(mut checkpoint)
		return err
	}
	p.suppress_spawn_wrapper = previous_suppress
	p.discard_emission(mut checkpoint)
	p.expect(.rpar)!
	return FastArm64Value{
		id: p.program.m.get_or_add_const(p.program.i64_type, p.program.m.type_size(value.typ).str())
		typ: p.program.i64_type
		typ_name: 'usize'
	}
}

fn (p &FastArm64Parser) emission_checkpoint() FastArm64EmissionCheckpoint {
	return FastArm64EmissionCheckpoint{
		value_count: p.program.m.values.len
		instruction_count: p.program.m.instrs.len
		block_count: p.program.m.blocks.len
		cur_block: p.cur_block
		terminated: p.terminated.clone()
		native_used_functions: p.program.native_used_function_names.clone()
		last_map_found: p.last_map_found
		parsing_spawn: p.parsing_spawn
	}
}

fn (mut p FastArm64Parser) discard_emission(mut checkpoint FastArm64EmissionCheckpoint) {
	p.program.m.discard_emission_since(checkpoint.value_count, checkpoint.instruction_count, checkpoint.block_count)
	p.cur_block = checkpoint.cur_block
	p.terminated = checkpoint.terminated.move()
	p.program.native_used_function_names = checkpoint.native_used_functions.move()
	p.last_map_found = checkpoint.last_map_found
	p.parsing_spawn = checkpoint.parsing_spawn
}

fn (mut p FastArm64Parser) zero_value(typ ssa.TypeID, type_name string) FastArm64Value {
	layout := p.program.m.type_store.types[typ]
	if layout.kind in [.struct_t, .array_t] {
		slot := p.program.instr0(.alloca, p.cur_block, p.program.m.type_store.get_ptr(typ))
		byte_slot := p.program.instr1(.bitcast, p.cur_block, p.program.ptr_i8, slot)
		memset_ref := p.program.m.add_value(.func_ref, p.program.ptr_i8, 'memset', p.program.fn_ids['memset'])
		zero := p.program.m.get_or_add_const(p.program.i32_type, '0')
		size := p.program.m.get_or_add_const(p.program.i64_type, p.program.m.type_size(typ).str())
		p.program.m.add_instr(.call, p.cur_block, p.program.ptr_i8, [memset_ref, byte_slot, zero,
			size])
		return FastArm64Value{
			id: p.program.instr1(.load, p.cur_block, typ, slot)
			typ: typ
			typ_name: type_name
			address: slot
		}
	}
	return FastArm64Value{
		id: p.program.m.get_or_add_const(typ, '0')
		typ: typ
		typ_name: type_name
	}
}

fn (p &FastArm64Parser) type_needs_default_initialization(typ ssa.TypeID, depth int) bool {
	if typ == p.program.map_type {
		return true
	}
	if depth >= 32 {
		return true
	}
	layout := p.program.m.type_store.types[typ]
	if layout.kind == .array_t {
		return p.type_needs_default_initialization(layout.elem_type, depth + 1)
	}
	declaration := p.program.type_decls_by_id[int(typ)] or { return false }
	if declaration.is_union || declaration.is_c {
		return false
	}
	for i, field in declaration.fields {
		if field.default_source != '' {
			return true
		}
		if i < layout.fields.len && p.type_needs_default_initialization(layout.fields[i], depth + 1) {
			return true
		}
	}
	return false
}

fn (mut p FastArm64Parser) default_value_for_type(typ ssa.TypeID, type_name string) !FastArm64Value {
	if typ == p.program.map_type {
		return p.new_empty_map_value(type_name)
	}
	layout := p.program.m.type_store.types[typ]
	if layout.kind == .array_t {
		return p.default_fixed_array_value(typ, type_name)
	}
	if declaration := p.program.type_decls_by_id[int(typ)] {
		if !declaration.is_union && !declaration.is_c {
			return p.default_struct_value_for_type(typ, type_name)
		}
	}
	return p.zero_value(typ, type_name)
}

fn (mut p FastArm64Parser) default_fixed_array_value(typ ssa.TypeID, type_name string) !FastArm64Value {
	mut result := p.zero_value(typ, type_name)
	layout := p.program.m.type_store.types[typ]
	if layout.kind != .array_t || layout.len <= 0 || !p.type_needs_default_initialization(layout.elem_type, 0) {
		return result
	}
	element_type_name := fastc_fixed_array_element_type(type_name) or { '' }
	index_slot := p.program.instr0(.alloca, p.cur_block, p.program.m.type_store.get_ptr(p.program.i64_type))
	zero := p.program.m.get_or_add_const(p.program.i64_type, '0')
	p.program.instr2(.store, p.cur_block, p.program.void_type, zero, index_slot)
	condition := p.program.m.add_block(p.func_id, 'fixed_array_default_condition')
	body := p.program.m.add_block(p.func_id, 'fixed_array_default_body')
	increment := p.program.m.add_block(p.func_id, 'fixed_array_default_increment')
	done := p.program.m.add_block(p.func_id, 'fixed_array_default_done')
	p.program.instr1(.jmp, p.cur_block, p.program.void_type, ssa.ValueID(condition))
	p.mark_terminated(p.cur_block)
	index := p.program.instr1(.load, condition, p.program.i64_type, index_slot)
	length := p.program.m.get_or_add_const(p.program.i64_type, layout.len.str())
	more := p.program.instr2(.ult, condition, p.program.i1_type, index, length)
	p.program.instr3(.br, condition, p.program.void_type, more, ssa.ValueID(body), ssa.ValueID(done))
	p.mark_terminated(condition)
	p.cur_block = body
	element := p.default_value_for_type(layout.elem_type, element_type_name)!
	base := p.program.instr1(.bitcast, p.cur_block, p.program.ptr_i8, result.address)
	element_size := p.program.m.get_or_add_const(p.program.i64_type, p.program.m.type_size(layout.elem_type).str())
	offset := p.program.instr2(.mul, p.cur_block, p.program.i64_type, index, element_size)
	address := p.program.instr2(.add, p.cur_block, p.program.ptr_i8, base, offset)
	typed_address := p.program.instr1(.bitcast, p.cur_block, p.program.m.type_store.get_ptr(layout.elem_type), address)
	p.program.instr2(.store, p.cur_block, p.program.void_type, element.id, typed_address)
	p.program.instr1(.jmp, p.cur_block, p.program.void_type, ssa.ValueID(increment))
	p.mark_terminated(p.cur_block)
	current := p.program.instr1(.load, increment, p.program.i64_type, index_slot)
	one := p.program.m.get_or_add_const(p.program.i64_type, '1')
	next := p.program.instr2(.add, increment, p.program.i64_type, current, one)
	p.program.instr2(.store, increment, p.program.void_type, next, index_slot)
	p.program.instr1(.jmp, increment, p.program.void_type, ssa.ValueID(condition))
	p.mark_terminated(increment)
	p.cur_block = done
	result = FastArm64Value{
		...result
		id: p.program.instr1(.load, done, typ, result.address)
	}
	return result
}

fn (mut p FastArm64Parser) parse_comptime_if_expression() !FastArm64Value {
	selected := fastc_scan_selected_comptime_branch(mut p.s, p.s.scan(), p.source_file.path, p.program.prefs)!
	outer_scanner := p.s
	outer_tok := selected.tok
	outer_lit := p.s.lit
	p.enter_source(selected.source)
	value := p.parse_expression(0)!
	p.s = outer_scanner
	p.tok = outer_tok
	p.lit = outer_lit
	return value
}

fn (mut p FastArm64Parser) parse_interpolated_string(first_literal string, quote u8) !FastArm64Value {
	first_part := fast_arm64_string_part(first_literal, quote != 0, false, quote) or {
		return p.unsupported(err.msg())
	}
	mut result := FastArm64Value{
		id: p.program.m.add_value(.string_literal, p.program.str_type, first_part, 0)
		typ: p.program.str_type
		typ_name: 'string'
	}
	for p.tok == .str_dollar {
		p.next()
		p.expect(.lcbr)!
		expression := p.parse_expression(0)!
		mut format := ''
		if p.tok == .colon {
			old_format := p.s.in_str_inter_format
			p.s.in_str_inter_format = true
			p.next()
			for p.tok !in [.rcbr, .eof] {
				format += if p.lit.len > 0 { p.lit } else { p.tok.str() }
				p.next()
			}
			p.s.in_str_inter_format = old_format
		}
		p.expect(.rcbr)!
		interpolated := if format.len > 0 {
			p.format_interpolation(expression, format)!
		} else {
			p.stringify(expression)!
		}
		result = p.emit_string_binary(.plus, result, interpolated)!
		if p.tok == .string {
			part_source := p.lit
			p.next()
			part := fast_arm64_string_part(part_source, false, p.tok != .str_dollar, quote) or {
				return p.unsupported(err.msg())
			}
			if part.len > 0 {
				part_value := FastArm64Value{
					id: p.program.m.add_value(.string_literal, p.program.str_type, part, 0)
					typ: p.program.str_type
					typ_name: 'string'
				}
				result = p.emit_string_binary(.plus, result, part_value)!
			}
		}
	}
	return result
}

fn fast_arm64_interpolation_format(source string) FastArm64InterpolationFormat {
	if source.len == 0 {
		return FastArm64InterpolationFormat{}
	}
	mut body := source
	mut specifier := u8(0)
	last := body[body.len - 1]
	if last in [`d`, `e`, `E`, `f`, `F`, `g`, `G`, `s`, `x`, `X`, `o`, `b`, `c`] {
		specifier = last
		body = body[..body.len - 1]
	}
	mut left := false
	if body.starts_with('-') {
		left = true
		body = body[1..]
	}
	zero_pad := !left && body.starts_with('0')
	dot := body.index('.') or { -1 }
	width_source := if dot >= 0 { body[..dot] } else { body }
	precision_source := if dot >= 0 { body[dot + 1..] } else { '' }
	return FastArm64InterpolationFormat{
		width: if width_source.len > 0 { width_source.int() } else { 0 }
		precision: if dot >= 0 {
			if precision_source.len > 0 { precision_source.int() } else { 0 }
		} else {
			-1
		}
		specifier: specifier
		left: left
		zero_pad: zero_pad
	}
}

fn (mut p FastArm64Parser) format_interpolation(value FastArm64Value, source string) !FastArm64Value {
	format := fast_arm64_interpolation_format(source)
	mut formatted := FastArm64Value{}
	is_float := value.typ in [p.program.f32_type, p.program.f64_type]
	float_value := if value.typ == p.program.f64_type {
		value
	} else if is_float {
		p.convert_value(value, p.program.f64_type, 'f64')
	} else {
		FastArm64Value{}
	}
	uppercase_float := p.program.m.get_or_add_const(p.program.i1_type, if format.specifier in [
		`E`,
		`F`,
		`G`,
	] {
		'1'
	} else {
		'0'
	})
	if is_float && format.precision < 0 && format.specifier in [u8(0), `e`, `E`, `f`, `F`, `g`,
		`G`] {
		digits := p.program.m.get_or_add_const(p.program.i32_type, if value.typ == p.program.f32_type {
			'8'
		} else {
			'17'
		})
		append_decimal := p.program.m.get_or_add_const(p.program.i1_type, '1')
		format_ref := p.program.m.add_value(.func_ref, p.program.str_type, 'fast_float_to_string', p.program.fn_ids['fast_float_to_string'])
		formatted = FastArm64Value{
			id: p.program.m.add_instr(.call, p.cur_block, p.program.str_type, [
				format_ref,
				float_value.id,
				digits,
				append_decimal,
				uppercase_float,
			])
			typ: p.program.str_type
			typ_name: 'string'
		}
	} else if is_float && format.specifier in [`e`, `E`] {
		precision := if format.precision >= 0 { format.precision } else { 6 }
		precision_value := p.program.m.get_or_add_const(p.program.i32_type, precision.str())
		format_ref := p.program.m.add_value(.func_ref, p.program.str_type, 'fast_scientific_from_float', p.program.fn_ids['fast_scientific_from_float'])
		formatted = FastArm64Value{
			id: p.program.m.add_instr(.call, p.cur_block, p.program.str_type, [
				format_ref,
				float_value.id,
				precision_value,
				uppercase_float,
			])
			typ: p.program.str_type
			typ_name: 'string'
		}
	} else if is_float && format.specifier in [`g`, `G`] {
		precision := if format.precision > 0 { format.precision } else { 1 }
		digits := p.program.m.get_or_add_const(p.program.i32_type, precision.str())
		append_decimal := p.program.m.get_or_add_const(p.program.i1_type, '0')
		format_ref := p.program.m.add_value(.func_ref, p.program.str_type, 'fast_float_to_string', p.program.fn_ids['fast_float_to_string'])
		formatted = FastArm64Value{
			id: p.program.m.add_instr(.call, p.cur_block, p.program.str_type, [
				format_ref,
				float_value.id,
				digits,
				append_decimal,
				uppercase_float,
			])
			typ: p.program.str_type
			typ_name: 'string'
		}
	} else if is_float && (format.specifier in [
		u8(0),
		`f`,
		`F`,
	] || format.precision >= 0) {
		precision := if format.precision >= 0 { format.precision } else { 6 }
		scale_precision := if precision > 15 { 15 } else { precision }
		mut factor := i64(1)
		for _ in 0 .. scale_precision {
			factor *= 10
		}
		factor_value := p.program.m.get_or_add_const(p.program.f64_type, factor.str() + '.0')
		scaled := p.program.instr2(.fmul, p.cur_block, p.program.f64_type, float_value.id, factor_value)
		zero_float := p.program.m.get_or_add_const(p.program.f64_type, '0.0')
		negative := p.program.instr2(.lt, p.cur_block, p.program.i1_type, scaled, zero_float)
		negative_number := p.program.instr1(.zext, p.cur_block, p.program.i64_type, negative)
		negative_float := p.program.instr1(.uitofp, p.cur_block, p.program.f64_type, negative_number)
		half := p.program.m.get_or_add_const(p.program.f64_type, '0.5')
		rounding := p.program.instr2(.fsub, p.cur_block, p.program.f64_type, half, negative_float)
		rounded := p.program.instr2(.fadd, p.cur_block, p.program.f64_type, scaled, rounding)
		scaled_integer := p.program.instr1(.fptosi, p.cur_block, p.program.i64_type, rounded)
		precision_value := p.program.m.get_or_add_const(p.program.i32_type, scale_precision.str())
		format_ref := p.program.m.add_value(.func_ref, p.program.str_type, 'fast_fixed_from_scaled', p.program.fn_ids['fast_fixed_from_scaled'])
		formatted = FastArm64Value{
			id: p.program.m.add_instr(.call, p.cur_block, p.program.str_type, [
				format_ref,
				scaled_integer,
				precision_value,
			])
			typ: p.program.str_type
			typ_name: 'string'
		}
		if precision > scale_precision {
			extra := p.program.m.get_or_add_const(p.program.i32_type, (precision - scale_precision).str())
			extend_ref := p.program.m.add_value(.func_ref, p.program.str_type, 'fast_string_extend_zeros', p.program.fn_ids['fast_string_extend_zeros'])
			formatted = FastArm64Value{
				id: p.program.m.add_instr(.call, p.cur_block, p.program.str_type, [
					extend_ref,
					formatted.id,
					extra,
				])
				typ: p.program.str_type
				typ_name: 'string'
			}
		}
	} else if p.program.m.type_store.types[value.typ].kind == .int_t && format.specifier == `c` {
		format_ref := p.program.m.add_value(.func_ref, p.program.str_type, 'fast_character_to_string', p.program.fn_ids['fast_character_to_string'])
		formatted = FastArm64Value{
			id: p.program.m.add_instr(.call, p.cur_block, p.program.str_type, [
				format_ref,
				p.integer_to_i64(value),
			])
			typ: p.program.str_type
			typ_name: 'string'
		}
	} else if p.program.m.type_store.types[value.typ].kind == .int_t && format.specifier in [
		`d`,
		`x`,
		`X`,
		`o`,
		`b`,
	] {
		base_number := match format.specifier {
			`x`, `X` { 16 }
			`o` { 8 }
			`b` { 2 }
			else { 10 }
		}
		base := p.program.m.get_or_add_const(p.program.i64_type, base_number.str())
		uppercase := p.program.m.get_or_add_const(p.program.i1_type, if format.specifier == `X` {
			'1'
		} else {
			'0'
		})
		is_signed := p.program.m.get_or_add_const(p.program.i1_type, if p.program.m.type_store.types[value.typ].is_unsigned {
			'0'
		} else {
			'1'
		})
		format_ref := p.program.m.add_value(.func_ref, p.program.str_type, 'fast_integer_to_string', p.program.fn_ids['fast_integer_to_string'])
		formatted = FastArm64Value{
			id: p.program.m.add_instr(.call, p.cur_block, p.program.str_type, [
				format_ref,
				p.integer_to_i64(value),
				base,
				uppercase,
				is_signed,
			])
			typ: p.program.str_type
			typ_name: 'string'
		}
	} else {
		formatted = p.stringify(value)!
	}
	if format.width <= 0 {
		return formatted
	}
	width := p.program.m.get_or_add_const(p.program.i32_type, format.width.str())
	left := p.program.m.get_or_add_const(p.program.i1_type, if format.left { '1' } else { '0' })
	zero_pad := p.program.m.get_or_add_const(p.program.i1_type, if format.zero_pad {
		'1'
	} else {
		'0'
	})
	pad_ref := p.program.m.add_value(.func_ref, p.program.str_type, 'fast_string_pad', p.program.fn_ids['fast_string_pad'])
	return FastArm64Value{
		id: p.program.m.add_instr(.call, p.cur_block, p.program.str_type, [pad_ref, formatted.id,
			width, left, zero_pad])
		typ: p.program.str_type
		typ_name: 'string'
	}
}

fn (mut p FastArm64Parser) stringify(value FastArm64Value) !FastArm64Value {
	if value.typ == p.program.str_type {
		return value
	}
	if value.typ == p.program.i1_type {
		convert_ref := p.program.m.add_value(.func_ref, p.program.str_type, 'fast_bool_to_string', p.program.fn_ids['fast_bool_to_string'])
		return FastArm64Value{
			id: p.program.m.add_instr(.call, p.cur_block, p.program.str_type, [
				convert_ref,
				value.id,
			])
			typ: p.program.str_type
			typ_name: 'string'
		}
	}
	if value.typ in [p.program.f32_type, p.program.f64_type] {
		float_value := if value.typ == p.program.f64_type {
			value
		} else {
			p.convert_value(value, p.program.f64_type, 'f64')
		}
		digits := p.program.m.get_or_add_const(p.program.i32_type, if value.typ == p.program.f32_type {
			'8'
		} else {
			'17'
		})
		convert_ref := p.program.m.add_value(.func_ref, p.program.str_type, 'fast_float_to_string', p.program.fn_ids['fast_float_to_string'])
		append_decimal := p.program.m.get_or_add_const(p.program.i1_type, '1')
		uppercase := p.program.m.get_or_add_const(p.program.i1_type, '0')
		return FastArm64Value{
			id: p.program.m.add_instr(.call, p.cur_block, p.program.str_type, [
				convert_ref,
				float_value.id,
				digits,
				append_decimal,
				uppercase,
			])
			typ: p.program.str_type
			typ_name: 'string'
		}
	}
	value_layout := p.program.m.type_store.types[value.typ]
	if value_layout.kind == .int_t && value_layout.is_unsigned {
		base := p.program.m.get_or_add_const(p.program.i64_type, '10')
		flag := p.program.m.get_or_add_const(p.program.i1_type, '0')
		convert_ref := p.program.m.add_value(.func_ref, p.program.str_type, 'fast_integer_to_string', p.program.fn_ids['fast_integer_to_string'])
		return FastArm64Value{
			id: p.program.m.add_instr(.call, p.cur_block, p.program.str_type, [
				convert_ref,
				p.integer_to_i64(value),
				base,
				flag,
				flag,
			])
			typ: p.program.str_type
			typ_name: 'string'
		}
	}
	mut integer := value.id
	if value.typ == p.program.ptr_i8 || value.typ == p.program.m.type_store.get_ptr(p.program.ptr_i8) {
		integer = p.program.instr1(.bitcast, p.cur_block, p.program.i64_type, value.id)
	} else if p.program.m.type_size(value.typ) < 8 {
		if value.typ in [p.program.u8_type, p.program.u16_type, p.program.u32_type,
			p.program.i1_type] {
			integer = p.program.instr1(.zext, p.cur_block, p.program.i64_type, value.id)
		} else {
			integer = p.program.instr1(.sext, p.cur_block, p.program.i64_type, value.id)
		}
	}
	convert_ref := p.program.m.add_value(.func_ref, p.program.str_type, 'fast_i64_to_string', p.program.fn_ids['fast_i64_to_string'])
	return FastArm64Value{
		id: p.program.m.add_instr(.call, p.cur_block, p.program.str_type, [
			convert_ref,
			integer,
		])
		typ: p.program.str_type
		typ_name: 'string'
	}
}

fn (mut p FastArm64Parser) parse_if_expression(expected_type_name string) !FastArm64Value {
	p.expect(.key_if)!
	condition := p.parse_expression(0)!
	for p.tok == .semicolon {
		p.next()
	}
	if p.tok != .lcbr {
		return p.unsupported('if expression condition')
	}
	then_block := p.program.m.add_block(p.func_id, 'if_expr_then')
	else_block := p.program.m.add_block(p.func_id, 'if_expr_else')
	merge_block := p.program.m.add_block(p.func_id, 'if_expr_merge')
	p.program.instr3(.br, p.cur_block, p.program.void_type, condition.id, ssa.ValueID(then_block), ssa.ValueID(else_block))
	p.mark_terminated(p.cur_block)
	p.cur_block = then_block
	p.expect(.lcbr)!
	then_value := if expected_type_name == '' {
		p.parse_expression(0)!
	} else {
		p.parse_contextual_value(expected_type_name)!
	}
	for p.tok == .semicolon {
		p.next()
	}
	p.expect(.rcbr)!
	result_slot := p.program.instr0(.alloca, p.cur_block, p.program.m.type_store.get_ptr(then_value.typ))
	option_failed_slot := p.program.instr0(.alloca, p.cur_block, p.program.m.type_store.get_ptr(p.program.i1_type))
	option_error_type_slot := p.program.instr0(.alloca, p.cur_block, p.program.m.type_store.get_ptr(p.program.u64_type))
	option_error_code_slot := p.program.instr0(.alloca, p.cur_block, p.program.m.type_store.get_ptr(p.program.i32_type))
	option_error_message_slot := p.program.instr0(.alloca, p.cur_block, p.program.m.type_store.get_ptr(p.program.str_type))
	p.program.instr2(.store, p.cur_block, p.program.void_type, then_value.id, result_slot)
	p.store_if_expression_option_metadata(then_value, option_failed_slot, option_error_type_slot, option_error_code_slot, option_error_message_slot)
	p.program.instr1(.jmp, p.cur_block, p.program.void_type, ssa.ValueID(merge_block))
	p.mark_terminated(p.cur_block)
	for p.tok == .semicolon {
		p.next()
	}
	if p.tok != .key_else {
		return p.unsupported('if expression without else')
	}
	p.next()
	p.cur_block = else_block
	mut else_value := FastArm64Value{}
	mut else_terminated := false
	if p.tok == .key_if {
		else_value = p.parse_if_expression(then_value.typ_name)!
	} else {
		p.expect(.lcbr)!
		if p.tok == .key_return {
			p.parse_return()!
			else_terminated = true
		} else {
			else_value = p.parse_contextual_value(then_value.typ_name)!
		}
		for p.tok == .semicolon {
			p.next()
		}
		p.expect(.rcbr)!
	}
	if !else_terminated && else_value.typ != then_value.typ {
		then_kind := p.program.m.type_store.types[then_value.typ].kind
		else_kind := p.program.m.type_store.types[else_value.typ].kind
		if then_kind != .int_t || else_kind != .int_t {
			return p.unsupported('if expression with different branch types')
		}
		else_value = p.convert_value(else_value, then_value.typ, then_value.typ_name)
	}
	if !else_terminated {
		p.program.instr2(.store, p.cur_block, p.program.void_type, else_value.id, result_slot)
		p.store_if_expression_option_metadata(else_value, option_failed_slot, option_error_type_slot, option_error_code_slot, option_error_message_slot)
		p.program.instr1(.jmp, p.cur_block, p.program.void_type, ssa.ValueID(merge_block))
		p.mark_terminated(p.cur_block)
	}
	p.cur_block = merge_block
	has_option_metadata := p.if_expression_value_has_option_metadata(then_value) || (!else_terminated && p.if_expression_value_has_option_metadata(else_value))
	return FastArm64Value{
		id: p.program.instr1(.load, p.cur_block, then_value.typ, result_slot)
		typ: then_value.typ
		typ_name: then_value.typ_name
		option_failed: if has_option_metadata {
			p.program.instr1(.load, p.cur_block, p.program.i1_type, option_failed_slot)
		} else {
			ssa.ValueID(0)
		}
		option_error_type: if has_option_metadata {
			p.program.instr1(.load, p.cur_block, p.program.u64_type, option_error_type_slot)
		} else {
			ssa.ValueID(0)
		}
		option_error_code: if has_option_metadata {
			p.program.instr1(.load, p.cur_block, p.program.i32_type, option_error_code_slot)
		} else {
			ssa.ValueID(0)
		}
		option_error_message: if has_option_metadata {
			p.program.instr1(.load, p.cur_block, p.program.str_type, option_error_message_slot)
		} else {
			ssa.ValueID(0)
		}
	}
}

fn (p &FastArm64Parser) if_expression_value_has_option_metadata(value FastArm64Value) bool {
	return value.is_none || value.option_failed != ssa.ValueID(0) || value.option_error_type != ssa.ValueID(0) || value.option_error_code != ssa.ValueID(0) || value.option_error_message != ssa.ValueID(0)
}

fn (mut p FastArm64Parser) store_if_expression_option_metadata(value FastArm64Value, failed_slot ssa.ValueID, error_type_slot ssa.ValueID, error_code_slot ssa.ValueID, error_message_slot ssa.ValueID) {
	failed := if value.option_failed != ssa.ValueID(0) {
		value.option_failed
	} else if value.is_none {
		p.program.m.get_or_add_const(p.program.i1_type, '1')
	} else {
		p.program.m.get_or_add_const(p.program.i1_type, '0')
	}
	error_type := if value.option_error_type == ssa.ValueID(0) {
		p.program.m.get_or_add_const(p.program.u64_type, '0')
	} else {
		value.option_error_type
	}
	error_code := if value.option_error_code == ssa.ValueID(0) {
		p.program.m.get_or_add_const(p.program.i32_type, '0')
	} else {
		value.option_error_code
	}
	error_message := if value.option_error_message == ssa.ValueID(0) {
		p.program.m.add_value(.string_literal, p.program.str_type, '', 0)
	} else {
		value.option_error_message
	}
	p.program.instr2(.store, p.cur_block, p.program.void_type, failed, failed_slot)
	p.program.instr2(.store, p.cur_block, p.program.void_type, error_type, error_type_slot)
	p.program.instr2(.store, p.cur_block, p.program.void_type, error_code, error_code_slot)
	p.program.instr2(.store, p.cur_block, p.program.void_type, error_message, error_message_slot)
}

fn (mut p FastArm64Parser) parse_name_expression() !FastArm64Value {
	first_name := p.lit
	pseudo_position := p.s.pos
	p.next()
	if first_name in ['error', 'error_with_code'] && p.tok == .lpar {
		p.next()
		mut message := p.parse_expression(0)!
		mut error_code := p.program.m.get_or_add_const(p.program.i32_type, '0')
		if first_name == 'error_with_code' {
			p.expect(.comma)!
			code := p.parse_expression(0)!
			error_code = p.convert_value(code, p.program.i32_type, 'int').id
		}
		p.expect(.rpar)!
		if message.typ != p.program.str_type {
			message = p.convert_value(message, p.program.str_type, 'string')
		}
		value := p.zero_value(p.return_typ, p.return_name)
		return FastArm64Value{
			...value
			is_none: true
			option_error_message: message.id
			option_error_code: error_code
		}
	}
	if first_name.starts_with('@') {
		pseudo_line, pseudo_column := fastc_line_column(p.s.src, pseudo_position)
		module_name := if p.source_file.header.module_name == '' {
			'main'
		} else {
			p.source_file.header.module_name
		}
		function_name := if p.current_function != '' {
			p.current_function
		} else {
			p.program.m.funcs[p.func_id].name
		}
		receiver_name := p.current_receiver.all_after_last('.').all_after_last('__')
		method_name := if receiver_name != '' {
			'${receiver_name}.${function_name}'
		} else {
			function_name
		}
		location_method := if receiver_name == '' {
			'${module_name}.${function_name}'
		} else if p.current_method_is_static {
			'${module_name}.${receiver_name}.${function_name} (static)'
		} else {
			'${module_name}.${receiver_name}{}.${function_name}'
		}
		literal := match first_name {
			'@FN' { function_name }
			'@METHOD' { method_name }
			'@STRUCT' { receiver_name }
			'@MOD' { module_name }
			'@FILE' { p.source_file.path }
			'@DIR' { p.source_file.path.all_before_last('/') }
			'@LINE' { pseudo_line.str() }
			'@COLUMN' { pseudo_column.str() }
			'@FILE_LINE' { '${os.file_name(p.source_file.path)}:${pseudo_line}' }
			'@LOCATION' { '${p.source_file.path}:${pseudo_line}, ${location_method}' }
			'@VEXE' { p.program.prefs.vexe }
			'@VEXEROOT', '@VROOT' { p.program.prefs.vroot }
			'@VMODROOT' { fastc_vmod_root_for_file(p.source_file.path) }
			'@VMOD_FILE' {
				fast_arm64_vmod_file(p.source_file.path) or {
					return p.unsupported(err.msg())
				}
			}
			'@VMODHASH' {
				fast_arm64_vmod_hash(p.source_file.path) or {
					return p.unsupported(err.msg())
				}
			}
			'@VHASH' { p.program.prefs.vhash }
			'@VCURRENTHASH' { p.program.prefs.vcurrent_hash }
			'@BUILD_DATE' { p.program.prefs.build_date }
			'@BUILD_TIME' { p.program.prefs.build_time }
			'@BUILD_TIMESTAMP' { p.program.prefs.build_timestamp }
			'@OS' { p.program.prefs.normalized_target_os() }
			'@CCOMPILER' { p.program.prefs.ccompiler }
			'@BACKEND' { p.program.prefs.backend }
			'@PLATFORM' { p.program.prefs.comptime_platform() }
			else {
				return p.unsupported('compile-time pseudo value `${first_name}`')
			}
		}
		return FastArm64Value{
			id: p.program.m.add_value(.string_literal, p.program.str_type, literal, 0)
			typ: p.program.str_type
			typ_name: 'string'
		}
	}
	if first_name == 'map' && p.tok == .lsbr {
		return p.parse_map_literal()
	}
	if local := p.locals[first_name] {
		return FastArm64Value{
			id: p.program.instr1(.load, p.cur_block, local.typ, local.addr)
			typ: local.typ
			typ_name: local.typ_name
			address: local.addr
			option_failed: local.option_failed
			option_error_type: local.option_error_type
			option_error_message: local.option_error_message
			option_error_code: local.option_error_code
			is_spawned: local.is_spawned
			spawn_handle: local.spawn_handle
			spawn_context: local.spawn_context
			spawn_context_type: local.spawn_context_type
			spawn_result_type: local.spawn_result_type
			spawn_result_name: local.spawn_result_name
		}
	}
	if first_name == 'g_v_os_execute_mutex_storage' {
		mut global_id := ssa.ValueID(0)
		for existing in p.program.m.values {
			if existing.kind == .global && existing.name == first_name {
				global_id = existing.id
				break
			}
		}
		if global_id == ssa.ValueID(0) {
			storage_type := p.program.m.type_store.get_array(p.program.u8_type, 128)
			global_id = p.program.m.add_global(first_name, storage_type)
		}
		return FastArm64Value{
			id: global_id
			typ: p.program.ptr_i8
			typ_name: '&u8'
		}
	}
	if p.tok == .lcbr && (first_name in p.program.type_ids || first_name in p.program.type_aliases) {
		return p.parse_struct_literal(first_name)
	}
	local_constant_key := fastc_constant_key(p.source_file.header.module_name, first_name)
	if declaration := p.program.constant_sources[local_constant_key] {
		return p.parse_constant_declaration(declaration)
	}
	if declaration := p.program.constant_sources[first_name] {
		return p.parse_constant_declaration(declaration)
	}
	mut short_constant := FastArm64ConstantDecl{}
	mut has_short_constant := false
	for constant_name, declaration in p.program.constant_sources {
		if constant_name.ends_with('.${first_name}') {
			if has_short_constant {
				has_short_constant = false
				break
			}
			short_constant = declaration
			has_short_constant = true
		}
	}
	if has_short_constant {
		return p.parse_constant_declaration(short_constant)
	}
	mut key := fastc_function_key(p.source_file.header.module_name, first_name)
	mut constant_key := fastc_constant_key(p.source_file.header.module_name, first_name)
	mut display_name := first_name
	mut qualified_type_key := ''
	if p.tok == .dot {
		p.next()
		if p.tok != .name && !p.tok.is_keyword() {
			return p.unsupported('qualified function name')
		}
		member := p.lit
		module_name := p.source_file.header.imports[first_name] or { first_name }
		key = if first_name in p.program.type_ids {
			'${fastc_type_key(p.source_file.header.module_name, first_name)}.${member}'
		} else {
			fastc_function_key(module_name, member)
		}
		constant_key = fastc_constant_key(module_name, member)
		display_name = '${first_name}.${member}'
		p.next()
		qualified_type_key = fastc_type_key(module_name, member)
		if p.tok == .dot && (qualified_type_key in p.program.type_ids || qualified_type_key in p.program.type_aliases || qualified_type_key in p.program.declared_types) {
			p.next()
			if p.tok != .name && !p.tok.is_keyword() {
				return p.unsupported('qualified static function name')
			}
			static_member := p.lit
			key = '${qualified_type_key}.${static_member}'
			constant_key = '${qualified_type_key}.${static_member}'
			display_name = '${first_name}.${member}.${static_member}'
			p.next()
		}
	}
	if p.tok == .lpar {
		if (qualified_type_key in p.program.type_ids || qualified_type_key in p.program.type_aliases) && display_name.count('.') == 1 && key !in p.program.functions {
			return p.parse_cast(qualified_type_key)
		}
		if (first_name in ['int', 'i8', 'char', 'i16', 'i32', 'rune', 'i64', 'u8', 'byte', 'u16',
			'u32', 'u64', 'isize', 'usize', 'f32', 'f64', 'bool', 'voidptr', 'byteptr', 'charptr'] || first_name in p.program.type_ids || first_name in p.program.type_aliases) && display_name == first_name {
			return p.parse_cast(first_name)
		}
		return p.parse_call(key, display_name)
	}
	if p.tok == .lcbr && qualified_type_key in p.program.type_ids {
		return p.parse_struct_literal(qualified_type_key)
	}
	if p.tok == .lcbr && (display_name in p.program.type_ids || display_name in [
		'C.fd_set',
		'C.timeval',
	]) {
		if display_name in ['C.fd_set', 'C.timeval'] {
			p.program.ensure_c_fd_type(display_name)
		}
		return p.parse_struct_literal(display_name)
	}
	if display_name == 'os.args' {
		func_id := p.program.fn_ids['arguments']
		fn_ref := p.program.m.add_value(.func_ref, p.program.array_type, p.program.fn_symbols['arguments'], func_id)
		return FastArm64Value{
			id: p.program.m.add_instr(.call, p.cur_block, p.program.array_type, [
				fn_ref,
			])
			typ: p.program.array_type
			typ_name: 'Array_string'
		}
	}
	if display_name == 'C.environ' {
		environ_type := p.program.m.type_store.get_ptr(p.program.ptr_i8)
		environ_address := p.program.m.add_external_global('environ', environ_type)
		return FastArm64Value{
			id: p.program.instr1(.load, p.cur_block, environ_type, environ_address)
			typ: environ_type
			typ_name: '&&char'
		}
	}
	if display_name in ['C.stdin', 'C.stdout', 'C.stderr'] {
		symbol := match display_name {
			'C.stdout' { '__stdoutp' }
			'C.stderr' { '__stderrp' }
			else { '__stdinp' }
		}
		address := p.program.m.add_external_global(symbol, p.program.ptr_i8)
		return FastArm64Value{
			id: p.program.instr1(.load, p.cur_block, p.program.ptr_i8, address)
			typ: p.program.ptr_i8
			typ_name: '&C.FILE'
			address: address
		}
	}
	if display_name == 'C.errno' {
		ptr_i32 := p.program.m.type_store.get_ptr(p.program.i32_type)
		error_ref := p.program.m.add_value(.func_ref, ptr_i32, '__error', p.program.fn_ids['__error'])
		address := p.program.m.add_instr(.call, p.cur_block, ptr_i32, [error_ref])
		return FastArm64Value{
			id: p.program.instr1(.load, p.cur_block, p.program.i32_type, address)
			typ: p.program.i32_type
			typ_name: 'int'
			address: address
		}
	}
	if display_name == 'C.NULL' {
		return p.zero_value(p.program.ptr_i8, 'voidptr')
	}
	if display_name in ['C.EINTR', 'C.FIONREAD', 'C.CLOCK_REALTIME', 'C.WNOHANG'] {
		literal := match display_name {
			'C.FIONREAD' { '1074030207' }
			'C.CLOCK_REALTIME' { '0' }
			'C.WNOHANG' { '1' }
			else { '4' }
		}
		return FastArm64Value{
			id: p.program.m.get_or_add_const(p.program.i32_type, literal)
			typ: p.program.i32_type
			typ_name: 'int'
		}
	}
	if display_name in ['C.O_RDONLY', 'C.O_WRONLY', 'C.O_RDWR', 'C.O_NONBLOCK', 'C.O_APPEND',
		'C.O_SYNC', 'C.O_CREAT', 'C.O_TRUNC', 'C.O_EXCL', 'C.O_NOCTTY'] {
		literal := match display_name {
			'C.O_WRONLY' { '1' }
			'C.O_RDWR' { '2' }
			'C.O_NONBLOCK' { '4' }
			'C.O_APPEND' { '8' }
			'C.O_SYNC' { '128' }
			'C.O_CREAT' { '512' }
			'C.O_TRUNC' { '1024' }
			'C.O_EXCL' { '2048' }
			'C.O_NOCTTY' { '131072' }
			else { '0' }
		}
		return FastArm64Value{
			id: p.program.m.get_or_add_const(p.program.i32_type, literal)
			typ: p.program.i32_type
			typ_name: 'int'
		}
	}
	if display_name in ['C.S_IRUSR', 'C.S_IWUSR', 'C.S_IXUSR', 'C.S_IRGRP', 'C.S_IWGRP', 'C.S_IXGRP',
		'C.S_IROTH', 'C.S_IWOTH', 'C.S_IXOTH', 'C.S_IREAD', 'C.S_IWRITE', 'C.S_IEXEC', 'C.S_IFMT',
		'C.S_IFIFO', 'C.S_IFCHR', 'C.S_IFDIR', 'C.S_IFBLK', 'C.S_IFREG', 'C.S_IFLNK', 'C.S_IFSOCK'] {
		literal := match display_name {
			'C.S_IRUSR', 'C.S_IREAD' { '256' }
			'C.S_IWUSR', 'C.S_IWRITE' { '128' }
			'C.S_IXUSR', 'C.S_IEXEC' { '64' }
			'C.S_IRGRP' { '32' }
			'C.S_IWGRP' { '16' }
			'C.S_IXGRP' { '8' }
			'C.S_IROTH' { '4' }
			'C.S_IWOTH' { '2' }
			'C.S_IXOTH' { '1' }
			'C.S_IFMT' { '61440' }
			'C.S_IFIFO' { '4096' }
			'C.S_IFCHR' { '8192' }
			'C.S_IFDIR' { '16384' }
			'C.S_IFBLK' { '24576' }
			'C.S_IFREG' { '32768' }
			'C.S_IFLNK' { '40960' }
			else { '49152' }
		}
		return FastArm64Value{
			id: p.program.m.get_or_add_const(p.program.i32_type, literal)
			typ: p.program.i32_type
			typ_name: 'int'
		}
	}
	if display_name in ['C.SEEK_SET', 'C.SEEK_CUR', 'C.SEEK_END'] {
		literal := match display_name {
			'C.SEEK_CUR' { '1' }
			'C.SEEK_END' { '2' }
			else { '0' }
		}
		return FastArm64Value{
			id: p.program.m.get_or_add_const(p.program.i64_type, literal)
			typ: p.program.i64_type
			typ_name: 'i64'
		}
	}
	if display_name in ['C._SC_PAGESIZE', 'C._SC_NPROCESSORS_ONLN'] {
		literal := if display_name == 'C._SC_PAGESIZE' { '29' } else { '58' }
		return FastArm64Value{
			id: p.program.m.get_or_add_const(p.program.i32_type, literal)
			typ: p.program.i32_type
			typ_name: 'int'
		}
	}
	if display_name != first_name {
		if display_name.count('.') > 1 {
			if enum_declaration := p.program.enum_values[constant_key] {
				mut enum_value := p.parse_constant_declaration(enum_declaration)!
				enum_type := p.program.type_id(qualified_type_key)
				enum_value = p.convert_value(enum_value, enum_type, qualified_type_key)
				return enum_value
			}
		}
		module_prefix := if p.source_file.header.module_name in ['', 'main'] {
			''
		} else {
			'${p.source_file.header.module_name}.'
		}
		enum_key := '${module_prefix}${display_name}'
		if enum_declaration := p.program.enum_values[enum_key] {
			mut enum_value := p.parse_constant_declaration(enum_declaration)!
			enum_type := p.program.type_id(first_name)
			enum_value = p.convert_value(enum_value, enum_type, first_name)
			return enum_value
		}
		if enum_declaration := p.program.enum_values[display_name] {
			mut enum_value := p.parse_constant_declaration(enum_declaration)!
			enum_type := p.program.type_id(first_name)
			enum_value = p.convert_value(enum_value, enum_type, first_name)
			return enum_value
		}
	}
	if declaration := p.program.constant_sources[constant_key] {
		return p.parse_constant_declaration(declaration)
	}
	if declaration := p.program.constant_sources[first_name] {
		return p.parse_constant_declaration(declaration)
	}
	if display_name != first_name {
		return p.unsupported('qualified value `${display_name}`')
	}
	return p.unsupported('unknown value `${first_name}`')
}

fn (mut p FastArm64Program) ensure_c_fd_type(name string) {
	if existing := p.type_ids[name] {
		if name == 'C.fd_set' && p.m.type_store.types[existing].fields.len == 0 {
			mut fields := []ssa.TypeID{}
			mut field_names := []string{}
			for i in 0 .. 16 {
				fields << p.u64_type
				field_names << 'bits_${i}'
			}
			p.m.type_store.types[existing] = ssa.Type{
				kind: .struct_t
				fields: fields
				field_names: field_names
				is_c_struct: true
			}
		}
		return
	}
	mut fields := []ssa.TypeID{}
	mut field_names := []string{}
	if name == 'C.timeval' {
		fields = [p.u64_type, p.u64_type]
		field_names = ['tv_sec', 'tv_usec']
	} else {
		for i in 0 .. 16 {
			fields << p.u64_type
			field_names << 'bits_${i}'
		}
	}
	if id := p.type_ids[name] {
		if p.m.type_store.types[id].fields.len > 0 {
			return
		}
		p.m.type_store.types[id] = ssa.Type{
			kind: .struct_t
			fields: fields
			field_names: field_names
			is_c_struct: true
		}
		return
	}
	id := p.m.type_store.register(ssa.Type{
		kind: .struct_t
		fields: fields
		field_names: field_names
		is_c_struct: true
	})
	p.type_ids[name] = id
}

fn (mut p FastArm64Parser) parse_struct_literal(type_name string) !FastArm64Value {
	typ := p.source_type_id(type_name)
	layout := p.program.m.type_store.types[typ]
	if layout.kind != .struct_t {
		return p.unsupported('`${type_name}` literal')
	}
	initial := p.default_struct_value_for_type(typ, type_name)!
	slot := initial.address
	p.expect(.lcbr)!
	mut positional_field := 0
	for p.tok != .rcbr {
		if p.tok == .semicolon {
			p.next()
			continue
		}
		if p.tok == .ellipsis {
			p.next()
			mut base := p.parse_expression(0)!
			if base.typ != typ {
				base = p.convert_value(base, typ, type_name)
			}
			p.program.instr2(.store, p.cur_block, p.program.void_type, base.id, slot)
			if p.tok in [.comma, .semicolon] {
				p.next()
			}
			continue
		}
		mut field := -1
		mut field_value := FastArm64Value{}
		if p.tok == .name || p.tok.is_keyword() {
			mut look := p.s
			if look.scan() == .colon {
				field_name := p.lit
				p.next()
				p.expect(.colon)!
				for candidate_index, candidate_name in layout.field_names {
					if candidate_name == field_name {
						field = candidate_index
						break
					}
				}
				if field < 0 {
					return p.unsupported('unknown `${type_name}.${field_name}` field among ${layout.field_names}')
				}
				field_value = p.parse_contextual_value(p.program.field_type_name(typ, field))!
			}
		}
		if field < 0 {
			if positional_field >= layout.fields.len {
				return p.unsupported('too many positional `${type_name}` fields')
			}
			field = positional_field
			positional_field++
			field_value = p.parse_contextual_value(p.program.field_type_name(typ, field))!
		}
		field_type := layout.fields[field]
		if field_value.typ != field_type {
			field_value = p.convert_value(field_value, field_type, field_value.typ_name)
		}
		address := p.program.struct_field_ptr(p.cur_block, slot, typ, field)
		p.program.instr2(.store, p.cur_block, p.program.void_type, field_value.id, address)
		if p.tok == .comma || p.tok == .semicolon {
			p.next()
		} else if p.tok != .rcbr {
			return p.unsupported('`${type_name}` field separator')
		}
	}
	p.next()
	return FastArm64Value{
		id: p.program.instr1(.load, p.cur_block, typ, slot)
		typ: typ
		typ_name: type_name
		address: slot
		is_temporary: true
	}
}

fn (mut p FastArm64Parser) parse_contextual_value(type_name string) !FastArm64Value {
	return p.parse_contextual_value_with_precedence(type_name, 0)
}

fn fast_arm64_contextual_map_types(type_name string) (string, string) {
	return fastc_map_key_value_types(type_name) or { return '', '' }
}

fn (mut p FastArm64Parser) parse_contextual_value_with_precedence(type_name string, precedence int) !FastArm64Value {
	previous_array_element := p.array_element
	previous_map_key := p.map_key
	previous_map_value := p.map_value
	p.array_element = p.program.array_element_type_name(type_name) or { '' }
	p.map_key = ''
	p.map_value = ''
	key_type, value_type := fast_arm64_contextual_map_types(p.program.resolved_type_name(type_name))
	if key_type != '' {
		p.map_key = key_type
		p.map_value = value_type
	}
	value := if p.tok == .dot {
		p.parse_enum_shorthand(type_name)!
	} else {
		p.parse_expression(precedence)!
	}
	p.array_element = previous_array_element
	p.map_key = previous_map_key
	p.map_value = previous_map_value
	return value
}

fn (mut p FastArm64Parser) source_type_id(type_name string) ssa.TypeID {
	if !type_name.contains('.') && !type_name.contains('__') {
		key := fastc_type_key(p.source_file.header.module_name, type_name)
		if id := p.program.type_ids[key] {
			return id
		}
	}
	return p.program.type_id(type_name)
}

fn (mut p FastArm64Parser) parse_constant_expression(source string) !FastArm64Value {
	outer_scanner := p.s
	outer_tok := p.tok
	outer_lit := p.lit
	p.enter_source(source)
	value := p.parse_expression(0)!
	p.s = outer_scanner
	p.tok = outer_tok
	p.lit = outer_lit
	return value
}

fn (mut p FastArm64Parser) parse_constant_declaration(declaration FastArm64ConstantDecl) !FastArm64Value {
	outer_source_file := p.source_file
	p.source_file = FastcSourceFile{
		path: declaration.path
		source: declaration.source
		header: declaration.header
	}
	defer {
		p.source_file = outer_source_file
	}
	return p.parse_constant_expression(declaration.source)
}

fn (p &FastArm64Parser) fixed_array_type_follows(mut look scanner.Scanner) bool {
	first_type_token := look.scan()
	if first_type_token == .eof {
		return false
	}
	_, next_token := fastc_scan_type(mut look, first_type_token, p.source_file.path, p.source_file.header.module_name, p.source_file.header.imports, p.program.declared_types, p.program.prefs.building_v) or { return false }
	return next_token == .lcbr
}

fn (p &FastArm64Parser) fixed_array_length(value FastArm64Value) ?int {
	if value.id <= ssa.ValueID(0) || int(value.id) >= p.program.m.values.len {
		return none
	}
	constant := p.program.m.values[value.id]
	if constant.kind != .constant {
		return none
	}
	length := fastc_decimal_integer_value(constant.name)?
	if length < 0 {
		return none
	}
	return length
}

fn (mut p FastArm64Parser) parse_array_literal() !FastArm64Value {
	expected_element_type_name := p.array_element
	p.expect(.lsbr)!
	if p.tok != .rsbr {
		mut look := p.s
		mut look_token := p.tok
		mut nested := 0
		mut could_be_fixed := true
		for look_token != .eof {
			if nested == 0 && look_token == .comma {
				could_be_fixed = false
				break
			}
			if nested == 0 && look_token == .rsbr {
				break
			}
			if look_token in [.lpar, .lsbr, .lcbr] {
				nested++
			} else if look_token in [.rpar, .rsbr, .rcbr] && nested > 0 {
				nested--
			}
			look_token = look.scan()
		}
		if could_be_fixed && look_token == .rsbr && p.fixed_array_type_follows(mut look) {
			length := p.parse_expression(0)!
			p.expect(.rsbr)!
			element_type_name, next_token := fastc_scan_type(mut p.s, p.tok, p.source_file.path, p.source_file.header.module_name, p.source_file.header.imports, p.program.declared_types, p.program.prefs.building_v) or {
				return p.unsupported('fixed array element type')
			}
			p.tok = next_token
			p.lit = p.s.lit
			p.expect(.lcbr)!
			if p.tok != .rcbr {
				return p.unsupported('initialized fixed array literal')
			}
			p.next()
			fixed_length := p.fixed_array_length(length) or {
				return p.unsupported('fixed array length')
			}
			fixed_type_name := fastc_fixed_array_type(fixed_length.str(), element_type_name)
			return p.default_fixed_array_value(p.program.type_id(fixed_type_name), fixed_type_name)
		}
	}
	if p.tok != .rsbr {
		mut initial := []FastArm64Value{}
		for p.tok != .rsbr {
			mut item := if p.tok == .dot && (initial.len > 0 || p.array_element != '') {
				p.parse_enum_shorthand(if initial.len > 0 {
					initial[0].typ_name
				} else {
					p.array_element
				})!
			} else if expected_element_type_name != '' {
				p.parse_contextual_value(expected_element_type_name)!
			} else if initial.len > 0 {
				p.parse_contextual_value(initial[0].typ_name)!
			} else {
				p.parse_expression(0)!
			}
			if expected_element_type_name != '' && item.typ != p.program.type_id(expected_element_type_name) {
				item = p.convert_value(item, p.program.type_id(expected_element_type_name), expected_element_type_name)
			} else if initial.len > 0 && item.typ != initial[0].typ {
				item = p.convert_value(item, initial[0].typ, initial[0].typ_name)
			}
			initial << item
			if p.tok == .comma {
				p.next()
			} else if p.tok != .rsbr {
				return p.unsupported('array literal separator')
			}
		}
		p.next()
		if initial.len == 0 {
			return p.unsupported('untyped empty array literal')
		}
		length := FastArm64Value{
			id: p.program.m.get_or_add_const(p.program.i32_type, initial.len.str())
			typ: p.program.i32_type
			typ_name: 'int'
		}
		element_type_name := if expected_element_type_name == '' {
			initial[0].typ_name
		} else {
			expected_element_type_name
		}
		return p.make_array(element_type_name, initial, length, length)
	}
	p.next()
	if p.tok != .name && !p.tok.is_keyword() && p.tok !in [.amp, .and, .mul, .question, .not, .lsbr,
		.lpar, .key_fn] {
		element_type_name := p.program.array_element_type_name(p.return_name) or { 'u8' }
		zero := FastArm64Value{
			id: p.program.m.get_or_add_const(p.program.i32_type, '0')
			typ: p.program.i32_type
			typ_name: 'int'
		}
		return p.make_array(element_type_name, []FastArm64Value{}, zero, zero)
	}
	element_type_name, next_token := fastc_scan_type(mut p.s, p.tok, p.source_file.path, p.source_file.header.module_name, p.source_file.header.imports, p.program.declared_types, p.program.prefs.building_v) or { return p.unsupported('array element type') }
	p.tok = next_token
	p.lit = p.s.lit
	p.expect(.lcbr)!
	mut length := FastArm64Value{
		id: p.program.m.get_or_add_const(p.program.i32_type, '0')
		typ: p.program.i32_type
		typ_name: 'int'
	}
	mut capacity := length
	mut explicit_length := false
	mut explicit_capacity := false
	mut has_init := false
	mut init_value := FastArm64Value{}
	mut initial := []FastArm64Value{}
	for p.tok != .rcbr {
		if p.tok == .name && p.lit in ['len', 'cap', 'init'] {
			mut look := p.s
			if look.scan() == .colon {
				field := p.lit
				p.next()
				p.next()
				field_value := if field == 'init' {
					p.parse_contextual_value(element_type_name)!
				} else {
					p.parse_expression(0)!
				}
				if field == 'len' {
					length = field_value
					explicit_length = true
				} else if field == 'cap' {
					capacity = field_value
					explicit_capacity = true
				} else {
					init_value = field_value
					has_init = true
				}
			} else {
				initial << p.parse_contextual_value(element_type_name)!
			}
		} else {
			initial << p.parse_contextual_value(element_type_name)!
		}
		if p.tok == .comma {
			p.next()
		} else if p.tok != .rcbr {
			return p.unsupported('array literal separator')
		}
	}
	p.next()
	if !explicit_length {
		length = FastArm64Value{
			id: p.program.m.get_or_add_const(p.program.i32_type, initial.len.str())
			typ: p.program.i32_type
			typ_name: 'int'
		}
	}
	if !explicit_capacity {
		capacity = length
	}
	result := p.make_array(element_type_name, initial, length, capacity)
	if has_init {
		return p.fill_array(result, element_type_name, init_value, length)
	}
	element_type := p.program.type_id(element_type_name)
	if explicit_length && initial.len == 0 && p.type_needs_default_initialization(element_type, 0) {
		default_value := p.default_value_for_type(element_type, element_type_name)!
		return p.fill_array(result, element_type_name, default_value, length)
	}
	return result
}

fn (mut p FastArm64Parser) make_array(element_type_name string, initial []FastArm64Value, length FastArm64Value, capacity FastArm64Value) FastArm64Value {
	element_type := p.program.type_id(element_type_name)
	element_size := p.program.m.get_or_add_const(p.program.i64_type, p.program.m.type_size(element_type).str())
	length64 := p.integer_to_i64(length)
	capacity64 := p.integer_to_i64(capacity)
	new_ref := p.program.m.add_value(.func_ref, p.program.array_type, 'fast_array_new', p.program.fn_ids['fast_array_new'])
	result := FastArm64Value{
		id: p.program.m.add_instr(.call, p.cur_block, p.program.array_type, [new_ref, element_size,
			length64, capacity64])
		typ: p.program.array_type
		typ_name: fastc_array_c_type(element_type_name)
	}
	if initial.len > 0 {
		slot := p.program.instr0(.alloca, p.cur_block, p.program.m.type_store.get_ptr(p.program.array_type))
		p.program.instr2(.store, p.cur_block, p.program.void_type, result.id, slot)
		data := p.program.instr1(.load, p.cur_block, p.program.ptr_i8, p.program.struct_field_ptr(p.cur_block, slot, p.program.array_type, 0))
		for i, item in initial {
			mut stored_item := item
			if stored_item.typ != element_type {
				stored_item = p.convert_value(stored_item, element_type, element_type_name)
			}
			offset := p.program.m.get_or_add_const(p.program.i64_type, (i * p.program.m.type_size(element_type)).str())
			address := p.program.instr2(.add, p.cur_block, p.program.ptr_i8, data, offset)
			typed_address := p.program.instr1(.bitcast, p.cur_block, p.program.m.type_store.get_ptr(element_type), address)
			p.program.instr2(.store, p.cur_block, p.program.void_type, stored_item.id, typed_address)
		}
	}
	return result
}

fn (mut p FastArm64Parser) fill_array(array FastArm64Value, element_type_name string, init_value FastArm64Value, length FastArm64Value) FastArm64Value {
	element_type := p.program.type_id(element_type_name)
	mut fill := init_value
	if fill.typ != element_type {
		fill = p.convert_value(fill, element_type, element_type_name)
	}
	array_slot := p.program.instr0(.alloca, p.cur_block, p.program.m.type_store.get_ptr(p.program.array_type))
	p.program.instr2(.store, p.cur_block, p.program.void_type, array.id, array_slot)
	data := p.program.instr1(.load, p.cur_block, p.program.ptr_i8, p.program.struct_field_ptr(p.cur_block, array_slot, p.program.array_type, 0))
	index_slot := p.program.instr0(.alloca, p.cur_block, p.program.m.type_store.get_ptr(p.program.i32_type))
	zero := p.program.m.get_or_add_const(p.program.i32_type, '0')
	p.program.instr2(.store, p.cur_block, p.program.void_type, zero, index_slot)
	mut length32 := length
	if length32.typ != p.program.i32_type {
		length32 = p.convert_value(length32, p.program.i32_type, 'int')
	}
	condition := p.program.m.add_block(p.func_id, 'array_fill_condition')
	body := p.program.m.add_block(p.func_id, 'array_fill_body')
	done := p.program.m.add_block(p.func_id, 'array_fill_done')
	p.program.instr1(.jmp, p.cur_block, p.program.void_type, ssa.ValueID(condition))
	p.mark_terminated(p.cur_block)
	index := p.program.instr1(.load, condition, p.program.i32_type, index_slot)
	more := p.program.instr2(.lt, condition, p.program.i1_type, index, length32.id)
	p.program.instr3(.br, condition, p.program.void_type, more, ssa.ValueID(body), ssa.ValueID(done))
	p.mark_terminated(condition)
	p.cur_block = body
	stored_fill := p.clone_array_default_value(fill, element_type_name)
	index64 := p.program.instr1(.zext, p.cur_block, p.program.i64_type, index)
	element_size := p.program.m.get_or_add_const(p.program.i64_type, p.program.m.type_size(element_type).str())
	offset := p.program.instr2(.mul, p.cur_block, p.program.i64_type, index64, element_size)
	address := p.program.instr2(.add, p.cur_block, p.program.ptr_i8, data, offset)
	typed_address := p.program.instr1(.bitcast, p.cur_block, p.program.m.type_store.get_ptr(element_type), address)
	p.program.instr2(.store, p.cur_block, p.program.void_type, stored_fill.id, typed_address)
	one := p.program.m.get_or_add_const(p.program.i32_type, '1')
	next := p.program.instr2(.add, p.cur_block, p.program.i32_type, index, one)
	p.program.instr2(.store, p.cur_block, p.program.void_type, next, index_slot)
	p.program.instr1(.jmp, p.cur_block, p.program.void_type, ssa.ValueID(condition))
	p.mark_terminated(p.cur_block)
	p.cur_block = done
	return array
}

fn (mut p FastArm64Parser) parse_selector(value FastArm64Value) !FastArm64Value {
	p.expect(.dot)!
	if p.tok != .name && !p.tok.is_keyword() {
		return p.unsupported('selector name')
	}
	member := p.lit
	p.next()
	if p.tok == .lpar {
		if member == 'wait' && value.is_spawned {
			p.expect(.lpar)!
			p.expect(.rpar)!
			if value.spawn_context != ssa.ValueID(0) {
				return p.emit_spawn_wait(value)
			}
			return value
		}
		value_layout := p.program.m.type_store.types[value.typ]
		is_array_receiver := value.typ == p.program.array_type || (value_layout.kind == .ptr_t && value_layout.elem_type == p.program.array_type)
		if is_array_receiver && member in ['clear', 'trim'] {
			p.expect(.lpar)!
			mut length := if member == 'clear' {
				FastArm64Value{
					id: p.program.m.get_or_add_const(p.program.i32_type, '0')
					typ: p.program.i32_type
					typ_name: 'int'
				}
			} else {
				p.parse_expression(0)!
			}
			p.expect(.rpar)!
			if length.typ != p.program.i32_type {
				length = p.convert_value(length, p.program.i32_type, 'int')
			}
			mut array_slot := value.address
			if value_layout.kind == .ptr_t {
				array_slot = value.id
			} else if array_slot == ssa.ValueID(0) {
				array_slot = p.program.instr0(.alloca, p.cur_block, p.program.m.type_store.get_ptr(p.program.array_type))
				p.program.instr2(.store, p.cur_block, p.program.void_type, value.id, array_slot)
			}
			if member == 'clear' {
				p.emit_array_clear(array_slot)
				return FastArm64Value{
					id: p.program.instr1(.load, p.cur_block, p.program.array_type, array_slot)
					typ: p.program.array_type
					typ_name: value.typ_name.trim_right('*')
					address: array_slot
				}
			}
			length_ptr := p.program.struct_field_ptr(p.cur_block, array_slot, p.program.array_type, 2)
			current_length := p.program.instr1(.load, p.cur_block, p.program.i32_type, length_ptr)
			shorter := p.program.instr2(.lt, p.cur_block, p.program.i1_type, length.id, current_length)
			trim_array := p.program.m.add_block(p.func_id, 'array_trim_shorter')
			trim_done := p.program.m.add_block(p.func_id, 'array_trim_done')
			p.program.instr3(.br, p.cur_block, p.program.void_type, shorter, ssa.ValueID(trim_array), ssa.ValueID(trim_done))
			p.mark_terminated(p.cur_block)
			p.cur_block = trim_array
			p.emit_array_detach_if_slice(array_slot, length.id, 'array_trim')
			p.program.instr2(.store, p.cur_block, p.program.void_type, length.id, length_ptr)
			p.program.instr1(.jmp, p.cur_block, p.program.void_type, ssa.ValueID(trim_done))
			p.mark_terminated(p.cur_block)
			p.cur_block = trim_done
			return FastArm64Value{
				id: p.program.instr1(.load, p.cur_block, p.program.array_type, array_slot)
				typ: p.program.array_type
				typ_name: value.typ_name.trim_right('*')
				address: array_slot
			}
		}
		if is_array_receiver && member == 'push_many' {
			p.expect(.lpar)!
			source := p.parse_expression(0)!
			p.expect(.comma)!
			count := p.parse_expression(0)!
			p.expect(.rpar)!
			array_type_name := value.typ_name.trim_right('*')
			element_type_name := p.program.array_element_type_name(array_type_name) or {
				return p.unsupported('array push_many type `${value.typ_name}`')
			}
			mut array := value
			if value.typ != p.program.array_type {
				array = FastArm64Value{
					id: p.program.instr1(.load, p.cur_block, p.program.array_type, value.id)
					typ: p.program.array_type
					typ_name: array_type_name
					address: value.id
				}
			}
			items := p.raw_array_value(source, count, element_type_name)
			return p.emit_array_append_many(array, items, element_type_name, false, FastArm64Value{})
		}
		if p.program.m.type_store.types[value.typ].kind == .int_t && member == 'has' {
			p.expect(.lpar)!
			flag := if p.tok == .dot {
				p.parse_enum_shorthand(value.typ_name)!
			} else {
				p.parse_expression(0)!
			}
			p.expect(.rpar)!
			masked := p.program.instr2(.and_, p.cur_block, value.typ, value.id, flag.id)
			zero := p.program.m.get_or_add_const(value.typ, '0')
			return FastArm64Value{
				id: p.program.instr2(.ne, p.cur_block, p.program.i1_type, masked, zero)
				typ: p.program.i1_type
				typ_name: 'bool'
			}
		}
		if value.typ in [p.program.array_type, p.program.map_type] && member == 'clone' {
			p.expect(.lpar)!
			p.expect(.rpar)!
			if value.typ == p.program.array_type {
				return p.emit_array_clone(value)
			}
			return p.emit_map_clone(value)
		}
		if value.typ == p.program.map_type && member == 'move' {
			p.expect(.lpar)!
			p.expect(.rpar)!
			if value.address != ssa.ValueID(0) {
				state_type := p.program.m.type_store.get_ptr(p.program.map_state_type)
				zero_state := p.program.m.get_or_add_const(state_type, '0')
				state_address := p.program.struct_field_ptr(p.cur_block, value.address, p.program.map_type, 0)
				p.program.instr2(.store, p.cur_block, p.program.void_type, zero_state, state_address)
			}
			return FastArm64Value{
				...value
				address: ssa.ValueID(0)
			}
		}
		if value.typ == p.program.array_type && member == 'reverse' {
			p.expect(.lpar)!
			p.expect(.rpar)!
			return p.emit_array_reverse(value)
		}
		if value.typ == p.program.array_type && member in ['last', 'delete_last', 'pop'] {
			p.expect(.lpar)!
			p.expect(.rpar)!
			return p.emit_array_tail_method(value, member in ['delete_last', 'pop'], member != 'delete_last')
		}
		if value.typ == p.program.array_type && member == 'sort' {
			p.expect(.lpar)!
			if p.tok != .rpar {
				return p.unsupported('array sort comparator')
			}
			p.next()
			element_type_name := p.program.array_element_type_name(value.typ_name) or {
				return p.unsupported('array sort type `${value.typ_name}`')
			}
			if p.program.type_id(element_type_name) != p.program.str_type {
				return p.unsupported('array sort of `${element_type_name}`')
			}
			sort_ref := p.program.m.add_value(.func_ref, p.program.void_type, 'fast_array_sort_strings', p.program.fn_ids['fast_array_sort_strings'])
			p.program.m.add_instr(.call, p.cur_block, p.program.void_type, [sort_ref, value.id])
			return FastArm64Value{
				typ: p.program.void_type
				typ_name: 'void'
			}
		}
		if value.typ == p.program.array_type && member in ['prepend', 'insert'] {
			p.expect(.lpar)!
			element_type_name := p.program.array_element_type_name(value.typ_name) or {
				return p.unsupported('array ${member} type `${value.typ_name}`')
			}
			if member == 'insert' {
				index := p.parse_expression(0)!
				p.expect(.comma)!
				item := p.parse_contextual_value(element_type_name)!
				p.expect(.rpar)!
				return p.emit_array_insert(value, item, index)
			}
			item := p.parse_contextual_value(element_type_name)!
			p.expect(.rpar)!
			return p.emit_array_push(value, item, true)
		}
		if value.typ == p.program.array_type && member == 'delete' {
			p.expect(.lpar)!
			index := p.parse_expression(0)!
			p.expect(.rpar)!
			return p.emit_array_delete(value, index)
		}
		if value.typ == p.program.map_type && member in ['keys', 'values'] {
			p.expect(.lpar)!
			p.expect(.rpar)!
			return p.emit_map_items_array(value, member == 'keys')
		}
		if value.typ == p.program.map_type && member == 'delete' {
			p.expect(.lpar)!
			mut key := p.parse_expression(0)!
			p.expect(.rpar)!
			key_type_name, _ := fastc_map_key_value_types(value.typ_name) or {
				return p.unsupported('map type `${value.typ_name}`')
			}
			key_type := p.program.type_id(key_type_name)
			if key.typ != key_type {
				key = p.convert_value(key, key_type, key_type_name)
			}
			key_slot := p.program.instr0(.alloca, p.cur_block, p.program.m.type_store.get_ptr(key_type))
			p.program.instr2(.store, p.cur_block, p.program.void_type, key.id, key_slot)
			key_pointer := p.program.instr1(.bitcast, p.cur_block, p.program.ptr_i8, key_slot)
			delete_ref := p.program.m.add_value(.func_ref, p.program.void_type, 'fast_map_delete', p.program.fn_ids['fast_map_delete'])
			p.program.m.add_instr(.call, p.cur_block, p.program.void_type, [
				delete_ref,
				value.id,
				key_pointer,
			])
			return FastArm64Value{
				typ: p.program.void_type
				typ_name: 'void'
			}
		}
		if value.typ in [p.program.i32_type, p.program.u32_type, p.program.i64_type,
			p.program.u64_type] && member in ['set', 'clear'] {
			p.expect(.lpar)!
			mut flag := if p.tok == .dot {
				p.parse_enum_shorthand(value.typ_name)!
			} else {
				p.parse_expression(0)!
			}
			p.expect(.rpar)!
			if value.address == ssa.ValueID(0) {
				return p.unsupported('`${member}` on a non-addressable flag value')
			}
			if flag.typ != value.typ {
				flag = p.convert_value(flag, value.typ, value.typ_name)
			}
			mut updated := ssa.ValueID(0)
			if member == 'set' {
				updated = p.program.instr2(.or_, p.cur_block, value.typ, value.id, flag.id)
			} else {
				updated = p.program.instr2(.and_, p.cur_block, value.typ, value.id, p.program.instr2(.xor, p.cur_block, value.typ, flag.id, p.program.m.get_or_add_const(value.typ, '-1')))
			}
			p.program.instr2(.store, p.cur_block, p.program.void_type, updated, value.address)
			return FastArm64Value{
				id: updated
				typ: value.typ
				typ_name: value.typ_name
				address: value.address
			}
		}
		return p.parse_method_call(value, member)
	}
	if value.typ == p.program.map_type && member == 'len' {
		map_slot := p.program.instr0(.alloca, p.cur_block, p.program.m.type_store.get_ptr(p.program.map_type))
		p.program.instr2(.store, p.cur_block, p.program.void_type, value.id, map_slot)
		state := p.program.instr1(.load, p.cur_block, p.program.m.type_store.get_ptr(p.program.map_state_type), p.program.struct_field_ptr(p.cur_block, map_slot, p.program.map_type, 0))
		state_bytes := p.program.instr1(.bitcast, p.cur_block, p.program.ptr_i8, state)
		null_pointer := p.program.m.get_or_add_const(p.program.ptr_i8, '0')
		has_state := p.program.instr2(.ne, p.cur_block, p.program.i1_type, state_bytes, null_pointer)
		state_block := p.program.m.add_block(p.func_id, 'map_len_state')
		empty_block := p.program.m.add_block(p.func_id, 'map_len_empty')
		done_block := p.program.m.add_block(p.func_id, 'map_len_done')
		result_slot := p.program.instr0(.alloca, p.cur_block, p.program.m.type_store.get_ptr(p.program.i32_type))
		p.program.instr3(.br, p.cur_block, p.program.void_type, has_state, ssa.ValueID(state_block), ssa.ValueID(empty_block))
		p.mark_terminated(p.cur_block)
		length64 := p.program.instr1(.load, state_block, p.program.i64_type, p.program.struct_field_ptr(state_block, state, p.program.map_state_type, 3))
		length := p.program.instr1(.trunc, state_block, p.program.i32_type, length64)
		p.program.instr2(.store, state_block, p.program.void_type, length, result_slot)
		p.program.instr1(.jmp, state_block, p.program.void_type, ssa.ValueID(done_block))
		p.mark_terminated(state_block)
		zero := p.program.m.get_or_add_const(p.program.i32_type, '0')
		p.program.instr2(.store, empty_block, p.program.void_type, zero, result_slot)
		p.program.instr1(.jmp, empty_block, p.program.void_type, ssa.ValueID(done_block))
		p.mark_terminated(empty_block)
		p.cur_block = done_block
		return FastArm64Value{
			id: p.program.instr1(.load, done_block, p.program.i32_type, result_slot)
			typ: p.program.i32_type
			typ_name: 'int'
		}
	}
	mut field := -1
	mut field_type := ssa.TypeID(0)
	mut field_type_name := ''
	if value.typ == p.program.str_type {
		match member {
			'str' {
				field = 0
				field_type = p.program.ptr_i8
				field_type_name = 'byteptr'
			}
			'len' {
				field = 1
				field_type = p.program.i32_type
				field_type_name = 'int'
			}
			'is_lit' {
				field = 2
				field_type = p.program.i32_type
				field_type_name = 'int'
			}
			else {}
		}
	} else if value.typ == p.program.array_type {
		field = match member {
			'data' { 0 }
			'offset' { 1 }
			'len' { 2 }
			'cap' { 3 }
			'flags' { 4 }
			'element_size' { 5 }
			else { -1 }
		}
		if field == 0 {
			field_type = p.program.ptr_i8
			field_type_name = 'voidptr'
		} else if field >= 0 {
			field_type = p.program.i32_type
			field_type_name = 'int'
		}
	} else {
		mut layout_type := value.typ
		mut base_address := value.address
		if p.program.m.type_store.types[layout_type].kind == .ptr_t {
			layout_type = p.program.m.type_store.types[layout_type].elem_type
			base_address = value.id
		}
		layout := p.program.m.type_store.types[layout_type]
		for i, name in layout.field_names {
			if name != member {
				continue
			}
			field = i
			field_type = layout.fields[i]
			field_type_name = p.program.field_type_name(layout_type, i)
			if base_address == ssa.ValueID(0) {
				base_address = p.program.instr0(.alloca, p.cur_block, p.program.m.type_store.get_ptr(layout_type))
				p.program.instr2(.store, p.cur_block, p.program.void_type, value.id, base_address)
			}
			value_slot := p.program.struct_field_ptr(p.cur_block, base_address, layout_type, field)
			return FastArm64Value{
				id: p.program.instr1(.load, p.cur_block, field_type, value_slot)
				typ: field_type
				typ_name: field_type_name
				address: value_slot
			}
		}
	}
	if field < 0 {
		return p.unsupported('field `${value.typ_name}.${member}` in `${p.program.m.funcs[p.func_id].name}` among ${p.program.m.type_store.types[value.typ].field_names}')
	}
	value_slot := p.program.instr0(.alloca, p.cur_block, p.program.m.type_store.get_ptr(value.typ))
	p.program.instr2(.store, p.cur_block, p.program.void_type, value.id, value_slot)
	field_address := p.program.struct_field_ptr(p.cur_block, value_slot, value.typ, field)
	return FastArm64Value{
		id: p.program.instr1(.load, p.cur_block, field_type, field_address)
		typ: field_type
		typ_name: field_type_name
		address: field_address
	}
}

fn (mut p FastArm64Parser) emit_array_clone(array FastArm64Value) FastArm64Value {
	array_slot := p.program.instr0(.alloca, p.cur_block, p.program.m.type_store.get_ptr(p.program.array_type))
	p.program.instr2(.store, p.cur_block, p.program.void_type, array.id, array_slot)
	data := p.program.instr1(.load, p.cur_block, p.program.ptr_i8, p.program.struct_field_ptr(p.cur_block, array_slot, p.program.array_type, 0))
	length32 := p.program.instr1(.load, p.cur_block, p.program.i32_type, p.program.struct_field_ptr(p.cur_block, array_slot, p.program.array_type, 2))
	capacity32 := p.program.instr1(.load, p.cur_block, p.program.i32_type, p.program.struct_field_ptr(p.cur_block, array_slot, p.program.array_type, 3))
	element_size32 := p.program.instr1(.load, p.cur_block, p.program.i32_type, p.program.struct_field_ptr(p.cur_block, array_slot, p.program.array_type, 5))
	length := p.program.instr1(.zext, p.cur_block, p.program.i64_type, length32)
	capacity := p.program.instr1(.zext, p.cur_block, p.program.i64_type, capacity32)
	element_size := p.program.instr1(.zext, p.cur_block, p.program.i64_type, element_size32)
	new_ref := p.program.m.add_value(.func_ref, p.program.array_type, 'fast_array_new', p.program.fn_ids['fast_array_new'])
	result := p.program.m.add_instr(.call, p.cur_block, p.program.array_type, [
		new_ref,
		element_size,
		length,
		capacity,
	])
	result_slot := p.program.instr0(.alloca, p.cur_block, p.program.m.type_store.get_ptr(p.program.array_type))
	p.program.instr2(.store, p.cur_block, p.program.void_type, result, result_slot)
	result_data := p.program.instr1(.load, p.cur_block, p.program.ptr_i8, p.program.struct_field_ptr(p.cur_block, result_slot, p.program.array_type, 0))
	element_type_name := p.program.array_element_type_name(array.typ_name) or { '' }
	element_type := p.program.type_id(element_type_name)
	if p.array_default_value_needs_clone(element_type, 0) {
		index_slot := p.program.instr0(.alloca, p.cur_block, p.program.m.type_store.get_ptr(p.program.i32_type))
		zero := p.program.m.get_or_add_const(p.program.i32_type, '0')
		p.program.instr2(.store, p.cur_block, p.program.void_type, zero, index_slot)
		condition := p.program.m.add_block(p.func_id, 'array_clone_condition')
		body := p.program.m.add_block(p.func_id, 'array_clone_body')
		done := p.program.m.add_block(p.func_id, 'array_clone_done')
		p.program.instr1(.jmp, p.cur_block, p.program.void_type, ssa.ValueID(condition))
		p.mark_terminated(p.cur_block)
		index := p.program.instr1(.load, condition, p.program.i32_type, index_slot)
		more := p.program.instr2(.lt, condition, p.program.i1_type, index, length32)
		p.program.instr3(.br, condition, p.program.void_type, more, ssa.ValueID(body), ssa.ValueID(done))
		p.mark_terminated(condition)
		index64 := p.program.instr1(.zext, body, p.program.i64_type, index)
		offset := p.program.instr2(.mul, body, p.program.i64_type, index64, element_size)
		source_address := p.program.instr2(.add, body, p.program.ptr_i8, data, offset)
		typed_source := p.program.instr1(.bitcast, body, p.program.m.type_store.get_ptr(element_type), source_address)
		element := FastArm64Value{
			id: p.program.instr1(.load, body, element_type, typed_source)
			typ: element_type
			typ_name: element_type_name
			address: typed_source
		}
		p.cur_block = body
		cloned_element := p.clone_array_default_value(element, element_type_name)
		destination_address := p.program.instr2(.add, p.cur_block, p.program.ptr_i8, result_data, offset)
		typed_destination := p.program.instr1(.bitcast, p.cur_block, p.program.m.type_store.get_ptr(element_type), destination_address)
		p.program.instr2(.store, p.cur_block, p.program.void_type, cloned_element.id, typed_destination)
		one := p.program.m.get_or_add_const(p.program.i32_type, '1')
		next := p.program.instr2(.add, p.cur_block, p.program.i32_type, index, one)
		p.program.instr2(.store, p.cur_block, p.program.void_type, next, index_slot)
		p.program.instr1(.jmp, p.cur_block, p.program.void_type, ssa.ValueID(condition))
		p.mark_terminated(p.cur_block)
		p.cur_block = done
	} else {
		bytes := p.program.instr2(.mul, p.cur_block, p.program.i64_type, length, element_size)
		memcpy_ref := p.program.m.add_value(.func_ref, p.program.ptr_i8, 'memcpy', p.program.fn_ids['memcpy'])
		p.program.m.add_instr(.call, p.cur_block, p.program.ptr_i8, [memcpy_ref, result_data, data,
			bytes])
	}
	return FastArm64Value{
		id: result
		typ: p.program.array_type
		typ_name: array.typ_name
		address: result_slot
	}
}

fn (mut p FastArm64Parser) emit_map_clone(map_value FastArm64Value) FastArm64Value {
	clone_ref := p.program.m.add_value(.func_ref, p.program.map_type, 'fast_map_clone', p.program.fn_ids['fast_map_clone'])
	result := p.program.m.add_instr(.call, p.cur_block, p.program.map_type, [
		clone_ref,
		map_value.id,
	])
	mut cloned := FastArm64Value{
		id: result
		typ: p.program.map_type
		typ_name: map_value.typ_name
	}
	key_type_name, value_type_name := fastc_map_key_value_types(map_value.typ_name) or {
		return cloned
	}
	key_type := p.program.type_id(key_type_name)
	value_type := p.program.type_id(value_type_name)
	clone_keys := p.array_default_value_needs_clone(key_type, 0)
	clone_values := p.array_default_value_needs_clone(value_type, 0)
	if !clone_keys && !clone_values {
		return cloned
	}
	result_slot := p.program.instr0(.alloca, p.cur_block, p.program.m.type_store.get_ptr(p.program.map_type))
	p.program.instr2(.store, p.cur_block, p.program.void_type, result, result_slot)
	state := p.program.instr1(.load, p.cur_block, p.program.m.type_store.get_ptr(p.program.map_state_type), p.program.struct_field_ptr(p.cur_block, result_slot, p.program.map_type, 0))
	zero_state := p.program.m.get_or_add_const(p.program.m.type_store.get_ptr(p.program.map_state_type), '0')
	has_state := p.program.instr2(.ne, p.cur_block, p.program.i1_type, state, zero_state)
	owned := p.program.m.add_block(p.func_id, 'map_clone_owned')
	condition := p.program.m.add_block(p.func_id, 'map_clone_owned_condition')
	body := p.program.m.add_block(p.func_id, 'map_clone_owned_body')
	finished := p.program.m.add_block(p.func_id, 'map_clone_owned_finished')
	done := p.program.m.add_block(p.func_id, 'map_clone_owned_done')
	p.program.instr3(.br, p.cur_block, p.program.void_type, has_state, ssa.ValueID(owned), ssa.ValueID(done))
	p.mark_terminated(p.cur_block)
	length := p.program.instr1(.load, owned, p.program.i64_type, p.program.struct_field_ptr(owned, state, p.program.map_state_type, 3))
	keys := p.program.instr1(.load, owned, p.program.ptr_i8, p.program.struct_field_ptr(owned, state, p.program.map_state_type, 0))
	values := p.program.instr1(.load, owned, p.program.ptr_i8, p.program.struct_field_ptr(owned, state, p.program.map_state_type, 1))
	key_size := p.program.m.get_or_add_const(p.program.i64_type, p.program.m.type_size(key_type).str())
	value_size := p.program.m.get_or_add_const(p.program.i64_type, p.program.m.type_size(value_type).str())
	index_slot := p.program.instr0(.alloca, owned, p.program.m.type_store.get_ptr(p.program.i64_type))
	zero := p.program.m.get_or_add_const(p.program.i64_type, '0')
	one := p.program.m.get_or_add_const(p.program.i64_type, '1')
	p.program.instr2(.store, owned, p.program.void_type, zero, index_slot)
	p.program.instr1(.jmp, owned, p.program.void_type, ssa.ValueID(condition))
	p.mark_terminated(owned)
	index := p.program.instr1(.load, condition, p.program.i64_type, index_slot)
	more := p.program.instr2(.lt, condition, p.program.i1_type, index, length)
	p.program.instr3(.br, condition, p.program.void_type, more, ssa.ValueID(body), ssa.ValueID(finished))
	p.mark_terminated(condition)
	p.cur_block = body
	if clone_keys {
		key_offset := p.program.instr2(.mul, p.cur_block, p.program.i64_type, index, key_size)
		key_address := p.program.instr2(.add, p.cur_block, p.program.ptr_i8, keys, key_offset)
		typed_key := p.program.instr1(.bitcast, p.cur_block, p.program.m.type_store.get_ptr(key_type), key_address)
		key := FastArm64Value{
			id: p.program.instr1(.load, p.cur_block, key_type, typed_key)
			typ: key_type
			typ_name: key_type_name
			address: typed_key
		}
		cloned_key := p.clone_array_default_value(key, key_type_name)
		p.program.instr2(.store, p.cur_block, p.program.void_type, cloned_key.id, typed_key)
	}
	if clone_values {
		value_offset := p.program.instr2(.mul, p.cur_block, p.program.i64_type, index, value_size)
		value_address := p.program.instr2(.add, p.cur_block, p.program.ptr_i8, values, value_offset)
		typed_value := p.program.instr1(.bitcast, p.cur_block, p.program.m.type_store.get_ptr(value_type), value_address)
		value := FastArm64Value{
			id: p.program.instr1(.load, p.cur_block, value_type, typed_value)
			typ: value_type
			typ_name: value_type_name
			address: typed_value
		}
		cloned_value := p.clone_array_default_value(value, value_type_name)
		p.program.instr2(.store, p.cur_block, p.program.void_type, cloned_value.id, typed_value)
	}
	next := p.program.instr2(.add, p.cur_block, p.program.i64_type, index, one)
	p.program.instr2(.store, p.cur_block, p.program.void_type, next, index_slot)
	p.program.instr1(.jmp, p.cur_block, p.program.void_type, ssa.ValueID(condition))
	p.mark_terminated(p.cur_block)
	if clone_keys {
		rehash_ref := p.program.m.add_value(.func_ref, p.program.void_type, 'fast_map_rehash', p.program.fn_ids['fast_map_rehash'])
		p.program.m.add_instr(.call, finished, p.program.void_type, [rehash_ref, state])
	}
	p.program.instr1(.jmp, finished, p.program.void_type, ssa.ValueID(done))
	p.mark_terminated(finished)
	p.cur_block = done
	cloned = FastArm64Value{
		...cloned
		address: result_slot
	}
	return cloned
}

fn (mut p FastArm64Parser) clone_array_default_value(value FastArm64Value, type_name string) FastArm64Value {
	if value.typ == p.program.array_type {
		return p.emit_array_clone(value)
	}
	if value.typ == p.program.map_type {
		return p.emit_map_clone(FastArm64Value{
			...value
			typ_name: type_name
		})
	}
	layout := p.program.m.type_store.types[value.typ]
	if layout.kind == .array_t && p.array_default_value_needs_clone(layout.elem_type, 1) {
		element_type_name := fastc_fixed_array_element_type(type_name) or { '' }
		slot := p.program.instr0(.alloca, p.cur_block, p.program.m.type_store.get_ptr(value.typ))
		p.program.instr2(.store, p.cur_block, p.program.void_type, value.id, slot)
		base := p.program.instr1(.bitcast, p.cur_block, p.program.ptr_i8, slot)
		element_size := p.program.m.type_size(layout.elem_type)
		for i in 0 .. layout.len {
			offset := p.program.m.get_or_add_const(p.program.i64_type, (i * element_size).str())
			address := p.program.instr2(.add, p.cur_block, p.program.ptr_i8, base, offset)
			typed_address := p.program.instr1(.bitcast, p.cur_block, p.program.m.type_store.get_ptr(layout.elem_type), address)
			element := FastArm64Value{
				id: p.program.instr1(.load, p.cur_block, layout.elem_type, typed_address)
				typ: layout.elem_type
				typ_name: element_type_name
				address: typed_address
			}
			cloned_element := p.clone_array_default_value(element, element_type_name)
			p.program.instr2(.store, p.cur_block, p.program.void_type, cloned_element.id, typed_address)
		}
		return FastArm64Value{
			id: p.program.instr1(.load, p.cur_block, value.typ, slot)
			typ: value.typ
			typ_name: type_name
			address: slot
		}
	}
	if !p.array_default_value_needs_clone(value.typ, 0) {
		return value
	}
	declaration := p.program.type_decls_by_id[int(value.typ)] or { return value }
	slot := p.program.instr0(.alloca, p.cur_block, p.program.m.type_store.get_ptr(value.typ))
	p.program.instr2(.store, p.cur_block, p.program.void_type, value.id, slot)
	for i, field in declaration.fields {
		if i >= layout.fields.len {
			break
		}
		field_type := layout.fields[i]
		if !p.array_default_value_needs_clone(field_type, 1) {
			continue
		}
		field_address := p.program.struct_field_ptr(p.cur_block, slot, value.typ, i)
		field_value := FastArm64Value{
			id: p.program.instr1(.load, p.cur_block, field_type, field_address)
			typ: field_type
			typ_name: field.typ
			address: field_address
		}
		cloned_field := p.clone_array_default_value(field_value, field.typ)
		p.program.instr2(.store, p.cur_block, p.program.void_type, cloned_field.id, field_address)
	}
	return FastArm64Value{
		id: p.program.instr1(.load, p.cur_block, value.typ, slot)
		typ: value.typ
		typ_name: type_name
		address: slot
	}
}

fn (p &FastArm64Parser) array_default_value_needs_clone(typ ssa.TypeID, depth int) bool {
	if typ in [p.program.array_type, p.program.map_type] {
		return true
	}
	if depth >= 32 {
		return false
	}
	layout := p.program.m.type_store.types[typ]
	if layout.kind == .array_t {
		return p.array_default_value_needs_clone(layout.elem_type, depth + 1)
	}
	declaration := p.program.type_decls_by_id[int(typ)] or { return false }
	if declaration.is_union || declaration.is_c {
		return false
	}
	if layout.kind != .struct_t {
		return false
	}
	for field_type in layout.fields {
		if p.array_default_value_needs_clone(field_type, depth + 1) {
			return true
		}
	}
	return false
}

fn (mut p FastArm64Parser) emit_array_reverse(array FastArm64Value) FastArm64Value {
	element_type_name := p.program.array_element_type_name(array.typ_name) or { '' }
	element_type := p.program.type_id(element_type_name)
	clone_elements := p.array_default_value_needs_clone(element_type, 0)
	array_slot := p.program.instr0(.alloca, p.cur_block, p.program.m.type_store.get_ptr(p.program.array_type))
	p.program.instr2(.store, p.cur_block, p.program.void_type, array.id, array_slot)
	data := p.program.instr1(.load, p.cur_block, p.program.ptr_i8, p.program.struct_field_ptr(p.cur_block, array_slot, p.program.array_type, 0))
	length32 := p.program.instr1(.load, p.cur_block, p.program.i32_type, p.program.struct_field_ptr(p.cur_block, array_slot, p.program.array_type, 2))
	capacity32 := p.program.instr1(.load, p.cur_block, p.program.i32_type, p.program.struct_field_ptr(p.cur_block, array_slot, p.program.array_type, 3))
	element_size32 := p.program.instr1(.load, p.cur_block, p.program.i32_type, p.program.struct_field_ptr(p.cur_block, array_slot, p.program.array_type, 5))
	length := p.program.instr1(.zext, p.cur_block, p.program.i64_type, length32)
	capacity := p.program.instr1(.zext, p.cur_block, p.program.i64_type, capacity32)
	element_size := p.program.instr1(.zext, p.cur_block, p.program.i64_type, element_size32)
	new_ref := p.program.m.add_value(.func_ref, p.program.array_type, 'fast_array_new', p.program.fn_ids['fast_array_new'])
	result := p.program.m.add_instr(.call, p.cur_block, p.program.array_type, [
		new_ref,
		element_size,
		length,
		capacity,
	])
	result_slot := p.program.instr0(.alloca, p.cur_block, p.program.m.type_store.get_ptr(p.program.array_type))
	p.program.instr2(.store, p.cur_block, p.program.void_type, result, result_slot)
	result_data := p.program.instr1(.load, p.cur_block, p.program.ptr_i8, p.program.struct_field_ptr(p.cur_block, result_slot, p.program.array_type, 0))
	index_slot := p.program.instr0(.alloca, p.cur_block, p.program.m.type_store.get_ptr(p.program.i64_type))
	zero := p.program.m.get_or_add_const(p.program.i64_type, '0')
	one := p.program.m.get_or_add_const(p.program.i64_type, '1')
	p.program.instr2(.store, p.cur_block, p.program.void_type, zero, index_slot)
	condition := p.program.m.add_block(p.func_id, 'array_reverse_condition')
	body := p.program.m.add_block(p.func_id, 'array_reverse_body')
	done := p.program.m.add_block(p.func_id, 'array_reverse_done')
	p.program.instr1(.jmp, p.cur_block, p.program.void_type, ssa.ValueID(condition))
	p.mark_terminated(p.cur_block)
	index := p.program.instr1(.load, condition, p.program.i64_type, index_slot)
	more := p.program.instr2(.lt, condition, p.program.i1_type, index, length)
	p.program.instr3(.br, condition, p.program.void_type, more, ssa.ValueID(body), ssa.ValueID(done))
	p.mark_terminated(condition)
	last := p.program.instr2(.sub, body, p.program.i64_type, length, one)
	source_index := p.program.instr2(.sub, body, p.program.i64_type, last, index)
	source_offset := p.program.instr2(.mul, body, p.program.i64_type, source_index, element_size)
	destination_offset := p.program.instr2(.mul, body, p.program.i64_type, index, element_size)
	source := p.program.instr2(.add, body, p.program.ptr_i8, data, source_offset)
	destination := p.program.instr2(.add, body, p.program.ptr_i8, result_data, destination_offset)
	p.cur_block = body
	if clone_elements {
		typed_source := p.program.instr1(.bitcast, p.cur_block, p.program.m.type_store.get_ptr(element_type), source)
		typed_destination := p.program.instr1(.bitcast, p.cur_block, p.program.m.type_store.get_ptr(element_type), destination)
		element := FastArm64Value{
			id: p.program.instr1(.load, p.cur_block, element_type, typed_source)
			typ: element_type
			typ_name: element_type_name
			address: typed_source
		}
		cloned_element := p.clone_array_default_value(element, element_type_name)
		p.program.instr2(.store, p.cur_block, p.program.void_type, cloned_element.id, typed_destination)
	} else {
		memcpy_ref := p.program.m.add_value(.func_ref, p.program.ptr_i8, 'memcpy', p.program.fn_ids['memcpy'])
		p.program.m.add_instr(.call, p.cur_block, p.program.ptr_i8, [memcpy_ref, destination, source,
			element_size])
	}
	next := p.program.instr2(.add, p.cur_block, p.program.i64_type, index, one)
	p.program.instr2(.store, p.cur_block, p.program.void_type, next, index_slot)
	p.program.instr1(.jmp, p.cur_block, p.program.void_type, ssa.ValueID(condition))
	p.mark_terminated(p.cur_block)
	p.cur_block = done
	return FastArm64Value{
		id: result
		typ: p.program.array_type
		typ_name: array.typ_name
		address: result_slot
	}
}

fn (mut p FastArm64Parser) mutable_array_slot(array FastArm64Value) ssa.ValueID {
	if array.address != ssa.ValueID(0) {
		return array.address
	}
	slot := p.program.instr0(.alloca, p.cur_block, p.program.m.type_store.get_ptr(p.program.array_type))
	p.program.instr2(.store, p.cur_block, p.program.void_type, array.id, slot)
	return slot
}

fn (mut p FastArm64Parser) emit_array_tail_method(array FastArm64Value, remove bool, return_value bool) !FastArm64Value {
	element_type_name := p.program.array_element_type_name(array.typ_name) or {
		return p.unsupported('array tail type `${array.typ_name}`')
	}
	element_type := p.program.type_id(element_type_name)
	array_slot := p.mutable_array_slot(array)
	length_ptr := p.program.struct_field_ptr(p.cur_block, array_slot, p.program.array_type, 2)
	length := p.program.instr1(.load, p.cur_block, p.program.i32_type, length_ptr)
	one := p.program.m.get_or_add_const(p.program.i32_type, '1')
	last_index := p.program.instr2(.sub, p.cur_block, p.program.i32_type, length, one)
	index64 := p.checked_array_index(FastArm64Value{
		id: last_index
		typ: p.program.i32_type
		typ_name: 'int'
	}, length, 'array_tail')
	data := p.program.instr1(.load, p.cur_block, p.program.ptr_i8, p.program.struct_field_ptr(p.cur_block, array_slot, p.program.array_type, 0))
	element_size := p.program.m.get_or_add_const(p.program.i64_type, p.program.m.type_size(element_type).str())
	offset := p.program.instr2(.mul, p.cur_block, p.program.i64_type, index64, element_size)
	bytes := p.program.instr2(.add, p.cur_block, p.program.ptr_i8, data, offset)
	address := p.program.instr1(.bitcast, p.cur_block, p.program.m.type_store.get_ptr(element_type), bytes)
	selected := p.program.instr1(.load, p.cur_block, element_type, address)
	if remove {
		p.emit_array_detach_if_slice(array_slot, last_index, 'array_tail')
		p.program.instr2(.store, p.cur_block, p.program.void_type, last_index, length_ptr)
	}
	if !return_value {
		return FastArm64Value{
			typ: p.program.void_type
			typ_name: 'void'
		}
	}
	return FastArm64Value{
		id: selected
		typ: element_type
		typ_name: element_type_name
		address: if remove { ssa.ValueID(0) } else { address }
	}
}

fn (mut p FastArm64Parser) emit_array_delete(array FastArm64Value, index FastArm64Value) FastArm64Value {
	array_slot := p.mutable_array_slot(array)
	length_ptr := p.program.struct_field_ptr(p.cur_block, array_slot, p.program.array_type, 2)
	length32 := p.program.instr1(.load, p.cur_block, p.program.i32_type, length_ptr)
	index64 := p.checked_array_index(index, length32, 'array_delete')
	p.emit_array_delete_storage(array_slot, index64, length32)
	return FastArm64Value{
		typ: p.program.void_type
		typ_name: 'void'
	}
}

fn (mut p FastArm64Parser) emit_array_delete_storage(array_slot ssa.ValueID, index64 ssa.ValueID, length32 ssa.ValueID) {
	data := p.program.instr1(.load, p.cur_block, p.program.ptr_i8, p.program.struct_field_ptr(p.cur_block, array_slot, p.program.array_type, 0))
	element_size32 := p.program.instr1(.load, p.cur_block, p.program.i32_type, p.program.struct_field_ptr(p.cur_block, array_slot, p.program.array_type, 5))
	length := p.program.instr1(.zext, p.cur_block, p.program.i64_type, length32)
	element_size := p.program.instr1(.zext, p.cur_block, p.program.i64_type, element_size32)
	one := p.program.m.get_or_add_const(p.program.i64_type, '1')
	next_index := p.program.instr2(.add, p.cur_block, p.program.i64_type, index64, one)
	remaining := p.program.instr2(.sub, p.cur_block, p.program.i64_type, length, next_index)
	new_length := p.program.instr2(.sub, p.cur_block, p.program.i64_type, length, one)
	new_length32 := p.program.instr1(.trunc, p.cur_block, p.program.i32_type, new_length)
	needs_detach := p.emit_array_needs_unique_shrink(array_slot)
	detach := p.program.m.add_block(p.func_id, 'array_delete_detach_slice')
	in_place := p.program.m.add_block(p.func_id, 'array_delete_in_place')
	ready := p.program.m.add_block(p.func_id, 'array_delete_ready')
	p.program.instr3(.br, p.cur_block, p.program.void_type, needs_detach, ssa.ValueID(detach), ssa.ValueID(in_place))
	p.mark_terminated(p.cur_block)
	new_ref := p.program.m.add_value(.func_ref, p.program.array_type, 'fast_array_new', p.program.fn_ids['fast_array_new'])
	detached_array := p.program.m.add_instr(.call, detach, p.program.array_type, [
		new_ref,
		element_size,
		new_length,
		new_length,
	])
	detached_slot := p.program.instr0(.alloca, detach, p.program.m.type_store.get_ptr(p.program.array_type))
	p.program.instr2(.store, detach, p.program.void_type, detached_array, detached_slot)
	detached_data := p.program.instr1(.load, detach, p.program.ptr_i8, p.program.struct_field_ptr(detach, detached_slot, p.program.array_type, 0))
	prefix_bytes := p.program.instr2(.mul, detach, p.program.i64_type, index64, element_size)
	suffix_bytes := p.program.instr2(.mul, detach, p.program.i64_type, remaining, element_size)
	source_offset := p.program.instr2(.mul, detach, p.program.i64_type, next_index, element_size)
	suffix_source := p.program.instr2(.add, detach, p.program.ptr_i8, data, source_offset)
	suffix_destination := p.program.instr2(.add, detach, p.program.ptr_i8, detached_data, prefix_bytes)
	memcpy_ref := p.program.m.add_value(.func_ref, p.program.ptr_i8, 'memcpy', p.program.fn_ids['memcpy'])
	p.program.m.add_instr(.call, detach, p.program.ptr_i8, [memcpy_ref, detached_data, data,
		prefix_bytes])
	p.program.m.add_instr(.call, detach, p.program.ptr_i8, [memcpy_ref, suffix_destination,
		suffix_source, suffix_bytes])
	p.program.instr2(.store, detach, p.program.void_type, detached_array, array_slot)
	p.program.instr1(.jmp, detach, p.program.void_type, ssa.ValueID(ready))
	p.mark_terminated(detach)
	in_place_data := p.program.instr1(.load, in_place, p.program.ptr_i8, p.program.struct_field_ptr(in_place, array_slot, p.program.array_type, 0))
	bytes := p.program.instr2(.mul, in_place, p.program.i64_type, remaining, element_size)
	destination_offset := p.program.instr2(.mul, in_place, p.program.i64_type, index64, element_size)
	in_place_source_offset := p.program.instr2(.mul, in_place, p.program.i64_type, next_index, element_size)
	destination := p.program.instr2(.add, in_place, p.program.ptr_i8, in_place_data, destination_offset)
	source := p.program.instr2(.add, in_place, p.program.ptr_i8, in_place_data, in_place_source_offset)
	memmove_ref := p.program.m.add_value(.func_ref, p.program.ptr_i8, 'memmove', p.program.fn_ids['memmove'])
	p.program.m.add_instr(.call, in_place, p.program.ptr_i8, [memmove_ref, destination, source,
		bytes])
	in_place_length_ptr := p.program.struct_field_ptr(in_place, array_slot, p.program.array_type, 2)
	p.program.instr2(.store, in_place, p.program.void_type, new_length32, in_place_length_ptr)
	p.program.instr1(.jmp, in_place, p.program.void_type, ssa.ValueID(ready))
	p.mark_terminated(in_place)
	p.cur_block = ready
}

fn (mut p FastArm64Parser) emit_array_detach_if_slice(array_slot ssa.ValueID, detached_length ssa.ValueID, label string) {
	data_ptr := p.program.struct_field_ptr(p.cur_block, array_slot, p.program.array_type, 0)
	element_size_ptr := p.program.struct_field_ptr(p.cur_block, array_slot, p.program.array_type, 5)
	old_data := p.program.instr1(.load, p.cur_block, p.program.ptr_i8, data_ptr)
	element_size := p.program.instr1(.load, p.cur_block, p.program.i32_type, element_size_ptr)
	is_slice := p.emit_array_needs_unique_shrink(array_slot)
	detach := p.program.m.add_block(p.func_id, '${label}_detach_slice')
	ready := p.program.m.add_block(p.func_id, '${label}_detached')
	p.program.instr3(.br, p.cur_block, p.program.void_type, is_slice, ssa.ValueID(detach), ssa.ValueID(ready))
	p.mark_terminated(p.cur_block)
	element_size64 := p.program.instr1(.zext, detach, p.program.i64_type, element_size)
	new_ref := p.program.m.add_value(.func_ref, p.program.array_type, 'fast_array_new', p.program.fn_ids['fast_array_new'])
	length64 := p.program.instr1(.zext, detach, p.program.i64_type, detached_length)
	detached_array := p.program.m.add_instr(.call, detach, p.program.array_type, [
		new_ref,
		element_size64,
		length64,
		length64,
	])
	detached_slot := p.program.instr0(.alloca, detach, p.program.m.type_store.get_ptr(p.program.array_type))
	p.program.instr2(.store, detach, p.program.void_type, detached_array, detached_slot)
	detached_data := p.program.instr1(.load, detach, p.program.ptr_i8, p.program.struct_field_ptr(detach, detached_slot, p.program.array_type, 0))
	copy_size := p.program.instr2(.mul, detach, p.program.i64_type, length64, element_size64)
	memcpy_ref := p.program.m.add_value(.func_ref, p.program.ptr_i8, 'memcpy', p.program.fn_ids['memcpy'])
	p.program.m.add_instr(.call, detach, p.program.ptr_i8, [memcpy_ref, detached_data, old_data,
		copy_size])
	p.program.instr2(.store, detach, p.program.void_type, detached_array, array_slot)
	p.program.instr1(.jmp, detach, p.program.void_type, ssa.ValueID(ready))
	p.mark_terminated(detach)
	p.cur_block = ready
}

fn (mut p FastArm64Parser) emit_array_needs_unique_shrink(array_slot ssa.ValueID) ssa.ValueID {
	offset_ptr := p.program.struct_field_ptr(p.cur_block, array_slot, p.program.array_type, 1)
	flags_ptr := p.program.struct_field_ptr(p.cur_block, array_slot, p.program.array_type, 4)
	offset := p.program.instr1(.load, p.cur_block, p.program.i32_type, offset_ptr)
	flags := p.program.instr1(.load, p.cur_block, p.program.i32_type, flags_ptr)
	zero32 := p.program.m.get_or_add_const(p.program.i32_type, '0')
	has_offset := p.program.instr2(.ne, p.cur_block, p.program.i1_type, offset, zero32)
	is_slice_flag := p.program.m.get_or_add_const(p.program.i32_type, '64')
	masked_flags := p.program.instr2(.and_, p.cur_block, p.program.i32_type, flags, is_slice_flag)
	has_slice_flag := p.program.instr2(.ne, p.cur_block, p.program.i1_type, masked_flags, zero32)
	descriptor_is_slice := p.program.instr2(.or_, p.cur_block, p.program.i1_type, has_offset, has_slice_flag)
	buffer_has_slices := p.emit_array_buffer_has_slices(array_slot)
	return p.program.instr2(.or_, p.cur_block, p.program.i1_type, descriptor_is_slice, buffer_has_slices)
}

fn (mut p FastArm64Parser) emit_array_clear(array_slot ssa.ValueID) {
	needs_reset := p.emit_array_needs_unique_shrink(array_slot)
	reset := p.program.m.add_block(p.func_id, 'array_clear_reset')
	ready := p.program.m.add_block(p.func_id, 'array_clear_ready')
	p.program.instr3(.br, p.cur_block, p.program.void_type, needs_reset, ssa.ValueID(reset), ssa.ValueID(ready))
	p.mark_terminated(p.cur_block)
	zero32 := p.program.m.get_or_add_const(p.program.i32_type, '0')
	null_pointer := p.program.m.get_or_add_const(p.program.ptr_i8, '0')
	data_ptr := p.program.struct_field_ptr(reset, array_slot, p.program.array_type, 0)
	offset_ptr := p.program.struct_field_ptr(reset, array_slot, p.program.array_type, 1)
	capacity_ptr := p.program.struct_field_ptr(reset, array_slot, p.program.array_type, 3)
	flags_ptr := p.program.struct_field_ptr(reset, array_slot, p.program.array_type, 4)
	flags := p.program.instr1(.load, reset, p.program.i32_type, flags_ptr)
	clear_flags_mask := p.program.m.get_or_add_const(p.program.i32_type, '-113')
	cleared_flags := p.program.instr2(.and_, reset, p.program.i32_type, flags, clear_flags_mask)
	p.program.instr2(.store, reset, p.program.void_type, null_pointer, data_ptr)
	p.program.instr2(.store, reset, p.program.void_type, zero32, offset_ptr)
	p.program.instr2(.store, reset, p.program.void_type, zero32, capacity_ptr)
	p.program.instr2(.store, reset, p.program.void_type, cleared_flags, flags_ptr)
	p.program.instr1(.jmp, reset, p.program.void_type, ssa.ValueID(ready))
	p.mark_terminated(reset)
	p.cur_block = ready
	length_ptr := p.program.struct_field_ptr(ready, array_slot, p.program.array_type, 2)
	p.program.instr2(.store, ready, p.program.void_type, zero32, length_ptr)
}

fn (mut p FastArm64Parser) emit_map_items_array(map_value FastArm64Value, keys bool) !FastArm64Value {
	key_type_name, value_type_name := fastc_map_key_value_types(map_value.typ_name) or {
		return p.unsupported('map item array type `${map_value.typ_name}`')
	}
	element_type_name := if keys { key_type_name } else { value_type_name }
	element_type := p.program.type_id(element_type_name)
	clone_items := p.array_default_value_needs_clone(element_type, 0)
	map_slot := p.program.instr0(.alloca, p.cur_block, p.program.m.type_store.get_ptr(p.program.map_type))
	p.program.instr2(.store, p.cur_block, p.program.void_type, map_value.id, map_slot)
	array_slot := p.program.instr0(.alloca, p.cur_block, p.program.m.type_store.get_ptr(p.program.array_type))
	state_ptr_type := p.program.m.type_store.get_ptr(p.program.map_state_type)
	state := p.program.instr1(.load, p.cur_block, state_ptr_type, p.program.struct_field_ptr(p.cur_block, map_slot, p.program.map_type, 0))
	zero_state := p.program.m.get_or_add_const(state_ptr_type, '0')
	has_state := p.program.instr2(.ne, p.cur_block, p.program.i1_type, state, zero_state)
	new_ref := p.program.m.add_value(.func_ref, p.program.array_type, 'fast_array_new', p.program.fn_ids['fast_array_new'])
	copy_items := p.program.m.add_block(p.func_id, 'map_items_copy')
	empty := p.program.m.add_block(p.func_id, 'map_items_empty')
	done := p.program.m.add_block(p.func_id, 'map_items_done')
	p.program.instr3(.br, p.cur_block, p.program.void_type, has_state, ssa.ValueID(copy_items), ssa.ValueID(empty))
	p.mark_terminated(p.cur_block)
	p.cur_block = copy_items
	data_field := if keys { 0 } else { 1 }
	size_field := if keys { 4 } else { 5 }
	data := p.program.instr1(.load, p.cur_block, p.program.ptr_i8, p.program.struct_field_ptr(p.cur_block, state, p.program.map_state_type, data_field))
	length64 := p.program.instr1(.load, p.cur_block, p.program.i64_type, p.program.struct_field_ptr(p.cur_block, state, p.program.map_state_type, 3))
	element_size64 := p.program.instr1(.load, p.cur_block, p.program.i64_type, p.program.struct_field_ptr(p.cur_block, state, p.program.map_state_type, size_field))
	byte_count := p.program.instr2(.mul, p.cur_block, p.program.i64_type, length64, element_size64)
	owned_array := p.program.m.add_instr(.call, p.cur_block, p.program.array_type, [
		new_ref,
		element_size64,
		length64,
		length64,
	])
	p.program.instr2(.store, p.cur_block, p.program.void_type, owned_array, array_slot)
	owned_data := p.program.instr1(.load, p.cur_block, p.program.ptr_i8, p.program.struct_field_ptr(p.cur_block, array_slot, p.program.array_type, 0))
	if clone_items {
		index_slot := p.program.instr0(.alloca, p.cur_block, p.program.m.type_store.get_ptr(p.program.i64_type))
		zero := p.program.m.get_or_add_const(p.program.i64_type, '0')
		one := p.program.m.get_or_add_const(p.program.i64_type, '1')
		p.program.instr2(.store, p.cur_block, p.program.void_type, zero, index_slot)
		condition := p.program.m.add_block(p.func_id, 'map_items_owned_condition')
		body := p.program.m.add_block(p.func_id, 'map_items_owned_body')
		copied := p.program.m.add_block(p.func_id, 'map_items_owned_done')
		p.program.instr1(.jmp, p.cur_block, p.program.void_type, ssa.ValueID(condition))
		p.mark_terminated(p.cur_block)
		index := p.program.instr1(.load, condition, p.program.i64_type, index_slot)
		more := p.program.instr2(.lt, condition, p.program.i1_type, index, length64)
		p.program.instr3(.br, condition, p.program.void_type, more, ssa.ValueID(body), ssa.ValueID(copied))
		p.mark_terminated(condition)
		p.cur_block = body
		offset := p.program.instr2(.mul, p.cur_block, p.program.i64_type, index, element_size64)
		source_address := p.program.instr2(.add, p.cur_block, p.program.ptr_i8, data, offset)
		destination_address := p.program.instr2(.add, p.cur_block, p.program.ptr_i8, owned_data, offset)
		typed_source := p.program.instr1(.bitcast, p.cur_block, p.program.m.type_store.get_ptr(element_type), source_address)
		typed_destination := p.program.instr1(.bitcast, p.cur_block, p.program.m.type_store.get_ptr(element_type), destination_address)
		item := FastArm64Value{
			id: p.program.instr1(.load, p.cur_block, element_type, typed_source)
			typ: element_type
			typ_name: element_type_name
			address: typed_source
		}
		cloned_item := p.clone_array_default_value(item, element_type_name)
		p.program.instr2(.store, p.cur_block, p.program.void_type, cloned_item.id, typed_destination)
		next := p.program.instr2(.add, p.cur_block, p.program.i64_type, index, one)
		p.program.instr2(.store, p.cur_block, p.program.void_type, next, index_slot)
		p.program.instr1(.jmp, p.cur_block, p.program.void_type, ssa.ValueID(condition))
		p.mark_terminated(p.cur_block)
		p.program.instr1(.jmp, copied, p.program.void_type, ssa.ValueID(done))
		p.mark_terminated(copied)
	} else {
		memcpy_ref := p.program.m.add_value(.func_ref, p.program.ptr_i8, 'memcpy', p.program.fn_ids['memcpy'])
		p.program.m.add_instr(.call, p.cur_block, p.program.ptr_i8, [memcpy_ref, owned_data, data,
			byte_count])
		p.program.instr1(.jmp, p.cur_block, p.program.void_type, ssa.ValueID(done))
		p.mark_terminated(p.cur_block)
	}
	p.cur_block = empty
	element_size_empty := p.program.m.get_or_add_const(p.program.i64_type, p.program.m.type_size(element_type).str())
	zero64 := p.program.m.get_or_add_const(p.program.i64_type, '0')
	empty_array := p.program.m.add_instr(.call, p.cur_block, p.program.array_type, [
		new_ref,
		element_size_empty,
		zero64,
		zero64,
	])
	p.program.instr2(.store, p.cur_block, p.program.void_type, empty_array, array_slot)
	p.program.instr1(.jmp, p.cur_block, p.program.void_type, ssa.ValueID(done))
	p.mark_terminated(p.cur_block)
	p.cur_block = done
	return FastArm64Value{
		id: p.program.instr1(.load, p.cur_block, p.program.array_type, array_slot)
		typ: p.program.array_type
		typ_name: fastc_array_c_type(element_type_name)
		address: array_slot
	}
}

fn (p &FastArm64Program) field_type_name(layout_type ssa.TypeID, field_index int) string {
	declaration := p.type_decls_by_id[int(layout_type)] or { return '' }
	if field_index >= 0 && field_index < declaration.fields.len {
		return declaration.fields[field_index].typ
	}
	return ''
}

fn (p &FastArm64Program) array_element_type_name(type_name string) ?string {
	mut current := type_name
	for _ in 0 .. 8 {
		if element := fastc_array_element_type(current) {
			return element
		}
		current = p.type_aliases[current] or { return none }
	}
	return none
}

fn (p &FastArm64Program) resolved_type_name(type_name string) string {
	mut current := fastc_normalize_inferred_type(type_name)
	for _ in 0 .. 8 {
		current = p.type_aliases[current] or { return current }
	}
	return current
}

fn (mut p FastArm64Parser) resolve_method_key(value FastArm64Value, method string) ?string {
	receiver_name := value.typ_name.trim_right('*')
	// An array-typed receiver (`[]T`, `&[]T`) takes the builtin array method:
	// an alias of `[]T` shares the array layout, so its own methods would
	// otherwise match a plain array receiver by type.
	if receiver_name.trim_left('&').starts_with('[]') {
		array_key := 'array.${method}'
		if array_key in p.program.functions {
			return array_key
		}
	}
	for semantic_receiver in [receiver_name, receiver_name.replace('__', '.')] {
		key := '${semantic_receiver}.${method}'
		if key in p.program.functions {
			return key
		}
	}
	for key in p.program.function_keys_by_name[method] {
		signature := p.program.functions[key]
		if signature.parameter_types.len == 0 {
			continue
		}
		expected := signature.parameter_types[0]
		expected_type := p.program.type_id(expected)
		if expected_type == value.typ {
			return key
		}
		if p.program.m.type_store.types[expected_type].kind == .ptr_t && p.program.m.type_store.types[expected_type].elem_type == value.typ {
			return key
		}
		if p.program.m.type_store.types[value.typ].kind == .ptr_t && p.program.m.type_store.types[value.typ].elem_type == expected_type {
			return key
		}
	}
	return none
}

fn (mut p FastArm64Parser) emit_zero_argument_method(value FastArm64Value, method string) ?FastArm64Value {
	resolved := p.resolve_method_key(value, method) or { return none }
	signature := p.program.functions[resolved]
	if signature.parameter_types.len != 1 {
		return none
	}
	p.program.native_used_function_names[resolved] = true
	expected_receiver := p.program.type_id(signature.parameter_types[0])
	mut receiver_id := value.id
	if p.program.m.type_store.types[expected_receiver].kind == .ptr_t && value.typ != expected_receiver {
		if value.address != ssa.ValueID(0) {
			receiver_id = value.address
		} else {
			receiver_slot := p.program.instr0(.alloca, p.cur_block, expected_receiver)
			p.program.instr2(.store, p.cur_block, p.program.void_type, value.id, receiver_slot)
			receiver_id = receiver_slot
		}
	}
	func_id := p.program.register_signature_function(resolved) or { return none }
	ret := p.program.fn_returns[resolved]
	symbol := p.program.fn_symbols[resolved]
	fn_ref := p.program.m.add_value(.func_ref, ret, symbol, func_id)
	return FastArm64Value{
		id: p.program.m.add_instr(.call, p.cur_block, ret, [fn_ref, receiver_id])
		typ: ret
		typ_name: signature.return_type
		tuple_types: signature.return_types
	}
}

fn (mut p FastArm64Parser) parse_method_call(value FastArm64Value, method string) !FastArm64Value {
	if value.typ_name == 'IError' && method in ['msg', 'str', 'code'] {
		p.expect(.lpar)!
		p.expect(.rpar)!
		code := if value.option_error_code == ssa.ValueID(0) {
			p.program.m.get_or_add_const(p.program.i32_type, '0')
		} else {
			value.option_error_code
		}
		if method == 'code' {
			return FastArm64Value{
				id: code
				typ: p.program.i32_type
				typ_name: 'int'
			}
		}
		message := if value.option_error_message == ssa.ValueID(0) {
			p.program.m.add_value(.string_literal, p.program.str_type, '', 0)
		} else {
			value.option_error_message
		}
		message_value := FastArm64Value{
			id: message
			typ: p.program.str_type
			typ_name: 'string'
		}
		if method == 'msg' {
			return message_value
		}
		result_slot := p.program.instr0(.alloca, p.cur_block, p.program.m.type_store.get_ptr(p.program.str_type))
		p.program.instr2(.store, p.cur_block, p.program.void_type, message, result_slot)
		with_code_block := p.program.m.add_block(p.func_id, 'error_str_with_code')
		done_block := p.program.m.add_block(p.func_id, 'error_str_done')
		zero := p.program.m.get_or_add_const(p.program.i32_type, '0')
		has_code := p.program.instr2(.gt, p.cur_block, p.program.i1_type, code, zero)
		p.program.instr3(.br, p.cur_block, p.program.void_type, has_code, ssa.ValueID(with_code_block), ssa.ValueID(done_block))
		p.mark_terminated(p.cur_block)
		p.cur_block = with_code_block
		separator := FastArm64Value{
			id: p.program.m.add_value(.string_literal, p.program.str_type, '; code: ', 0)
			typ: p.program.str_type
			typ_name: 'string'
		}
		prefix := p.emit_string_binary(.plus, message_value, separator)!
		code_string := p.stringify(FastArm64Value{
			id: code
			typ: p.program.i32_type
			typ_name: 'int'
		})!
		formatted := p.emit_string_binary(.plus, prefix, code_string)!
		p.program.instr2(.store, p.cur_block, p.program.void_type, formatted.id, result_slot)
		p.program.instr1(.jmp, p.cur_block, p.program.void_type, ssa.ValueID(done_block))
		p.mark_terminated(p.cur_block)
		p.cur_block = done_block
		return FastArm64Value{
			id: p.program.instr1(.load, done_block, p.program.str_type, result_slot)
			typ: p.program.str_type
			typ_name: 'string'
		}
	}
	resolved := p.resolve_method_key(value, method) or {
		return p.unsupported('method `${value.typ_name}.${method}`')
	}
	p.program.native_used_function_names[resolved] = true
	signature := p.program.functions[resolved]
	p.expect(.lpar)!
	mut call_args := []FastArm64Value{}
	for p.tok != .rpar {
		if p.tok == .key_mut {
			p.next()
		}
		argument_index := call_args.len + 1
		mut expected_type_name := ''
		if signature.is_variadic && argument_index >= signature.parameter_types.len - 1 {
			if !resolved.starts_with('C.') {
				expected_type_name = p.program.array_element_type_name(signature.parameter_types.last()) or {
					''
				}
			}
		} else if argument_index < signature.parameter_types.len {
			expected_type_name = signature.parameter_types[argument_index]
		}
		if p.tok == .name && expected_type_name != '' {
			mut look := p.s
			if look.scan() == .colon {
				call_args << p.parse_named_argument_struct(expected_type_name)!
				break
			}
		}
		if expected_type_name != '' {
			call_args << p.parse_contextual_value(expected_type_name)!
		} else {
			call_args << p.parse_expression(0)!
		}
		if p.tok == .comma {
			p.next()
		} else if p.tok != .rpar {
			return p.unsupported('method argument separator')
		}
	}
	p.next()
	if signature.is_variadic && !resolved.starts_with('C.') {
		call_args = p.pack_v_variadic_arguments(signature, call_args, 1)!
	}
	if signature.last_parameter_is_params && call_args.len < signature.parameter_types.len - 1 {
		call_args << p.default_struct_value(signature.parameter_types.last())!
	}
	p.validate_call_argument_count(method, resolved, signature, call_args, 1)!
	expected_receiver := p.program.type_id(signature.parameter_types[0])
	mut receiver_id := value.id
	if p.program.m.type_store.types[expected_receiver].kind == .ptr_t && value.typ != expected_receiver {
		if value.address != ssa.ValueID(0) {
			receiver_id = value.address
		} else {
			receiver_slot := p.program.instr0(.alloca, p.cur_block, expected_receiver)
			p.program.instr2(.store, p.cur_block, p.program.void_type, value.id, receiver_slot)
			receiver_id = receiver_slot
		}
	}
	func_id := p.program.register_signature_function(resolved) or {
		return p.unsupported('registered method `${resolved}`')
	}
	ret := p.program.fn_returns[resolved]
	symbol := p.program.fn_symbols[resolved]
	fn_ref := p.program.m.add_value(.func_ref, ret, symbol, func_id)
	mut operands := []ssa.ValueID{cap: call_args.len + 2}
	operands << fn_ref
	operands << receiver_id
	for i, argument in call_args {
		expected_index := i + 1
		if expected_index < signature.parameter_types.len {
			operands << p.call_argument(argument, signature.parameter_types[expected_index])
		} else {
			operands << argument.id
		}
	}
	if signature.return_type == 'Option' {
		p.store_option_success()
	}
	result := p.program.m.add_instr(.call, p.cur_block, ret, operands)
	option_state := if signature.return_type == 'Option' {
		p.option_state_pointer()
	} else {
		ssa.ValueID(0)
	}
	option_failed := if signature.return_type == 'Option' {
		p.program.instr1(.load, p.cur_block, p.program.i1_type, p.program.struct_field_ptr(p.cur_block, option_state, p.program.option_state_type, 0))
	} else {
		ssa.ValueID(0)
	}
	option_error_type := if signature.return_type == 'Option' {
		p.program.instr1(.load, p.cur_block, p.program.u64_type, p.program.struct_field_ptr(p.cur_block, option_state, p.program.option_state_type, 1))
	} else {
		ssa.ValueID(0)
	}
	option_error_code := if signature.return_type == 'Option' {
		p.program.instr1(.load, p.cur_block, p.program.i32_type, p.program.struct_field_ptr(p.cur_block, option_state, p.program.option_state_type, 2))
	} else {
		ssa.ValueID(0)
	}
	option_error_message := if signature.return_type == 'Option' {
		p.load_option_error_message(option_state)
	} else {
		ssa.ValueID(0)
	}
	return FastArm64Value{
		id: result
		typ: ret
		option_failed: option_failed
		option_error_type: option_error_type
		option_error_message: option_error_message
		option_error_code: option_error_code
		typ_name: if signature.return_type == 'Option' {
			signature.option_type
		} else {
			signature.return_type
		}
		tuple_types: signature.return_types
	}
}

fn (mut p FastArm64Parser) integer_to_i64(value FastArm64Value) ssa.ValueID {
	if value.typ == p.program.i64_type || value.typ == p.program.u64_type {
		return value.id
	}
	typ := p.program.m.type_store.types[value.typ]
	op := if typ.is_unsigned { ssa.OpCode.zext } else { ssa.OpCode.sext }
	return p.program.instr1(op, p.cur_block, p.program.i64_type, value.id)
}

fn (mut p FastArm64Parser) parse_array_index_or_slice(value FastArm64Value) !FastArm64Value {
	if value.typ == p.program.map_type {
		p.expect(.lsbr)!
		mut key := p.parse_expression(0)!
		p.expect(.rsbr)!
		key_type_name, value_type_name := fastc_map_key_value_types(value.typ_name) or {
			return p.unsupported('map type `${value.typ_name}`')
		}
		key_type := p.program.type_id(key_type_name)
		value_type := p.program.type_id(value_type_name)
		if key.typ != key_type {
			key = p.convert_value(key, key_type, key_type_name)
		}
		key_slot := p.program.instr0(.alloca, p.cur_block, p.program.m.type_store.get_ptr(key_type))
		p.program.instr2(.store, p.cur_block, p.program.void_type, key.id, key_slot)
		key_pointer := p.program.instr1(.bitcast, p.cur_block, p.program.ptr_i8, key_slot)
		empty_slot := p.program.instr0(.alloca, p.cur_block, p.program.m.type_store.get_ptr(value_type))
		empty_value := p.default_value_for_type(value_type, value_type_name)!
		p.program.instr2(.store, p.cur_block, p.program.void_type, empty_value.id, empty_slot)
		empty_pointer := p.program.instr1(.bitcast, p.cur_block, p.program.ptr_i8, empty_slot)
		value_size := p.program.m.get_or_add_const(p.program.i64_type, p.program.m.type_size(value_type).str())
		find_ref := p.program.m.add_value(.func_ref, p.program.i64_type, 'fast_map_find', p.program.fn_ids['fast_map_find'])
		index := p.program.m.add_instr(.call, p.cur_block, p.program.i64_type, [
			find_ref,
			value.id,
			key_pointer,
		])
		zero := p.program.m.get_or_add_const(p.program.i64_type, '0')
		found := p.program.instr2(.ge, p.cur_block, p.program.i1_type, index, zero)
		p.last_map_found = found
		get_ref := p.program.m.add_value(.func_ref, p.program.ptr_i8, 'fast_map_get', p.program.fn_ids['fast_map_get'])
		value_pointer := p.program.m.add_instr(.call, p.cur_block, p.program.ptr_i8, [
			get_ref,
			value.id,
			key_pointer,
			empty_pointer,
			value_size,
		])
		typed_pointer := p.program.instr1(.bitcast, p.cur_block, p.program.m.type_store.get_ptr(value_type), value_pointer)
		return FastArm64Value{
			id: p.program.instr1(.load, p.cur_block, value_type, typed_pointer)
			typ: value_type
			typ_name: value_type_name
			address: typed_pointer
			map_found: found
			map_id: value.id
			map_address: value.address
			map_type: value.typ_name
			map_key_id: key.id
			map_key_type: key.typ
			map_key_name: key.typ_name
		}
	}
	if p.program.m.type_store.types[value.typ].kind == .array_t {
		p.expect(.lsbr)!
		index := p.parse_expression(0)!
		if p.tok == .dotdot {
			return p.unsupported('fixed-array slice')
		}
		p.expect(.rsbr)!
		layout := p.program.m.type_store.types[value.typ]
		element_type := layout.elem_type
		element_type_name := fastc_fixed_array_element_type(value.typ_name) or { 'u8' }
		mut base_address := value.address
		if base_address == ssa.ValueID(0) {
			base_address = p.program.instr0(.alloca, p.cur_block, p.program.m.type_store.get_ptr(value.typ))
			p.program.instr2(.store, p.cur_block, p.program.void_type, value.id, base_address)
		}
		base := p.program.instr1(.bitcast, p.cur_block, p.program.ptr_i8, base_address)
		element_size := p.program.m.get_or_add_const(p.program.i64_type, p.program.m.type_size(element_type).str())
		length := p.program.m.get_or_add_const(p.program.i32_type, layout.len.str())
		index64 := p.checked_array_index(index, length, 'fixed_array_index')
		offset := p.program.instr2(.mul, p.cur_block, p.program.i64_type, index64, element_size)
		address := p.program.instr2(.add, p.cur_block, p.program.ptr_i8, base, offset)
		typed_address := p.program.instr1(.bitcast, p.cur_block, p.program.m.type_store.get_ptr(element_type), address)
		return FastArm64Value{
			id: p.program.instr1(.load, p.cur_block, element_type, typed_address)
			typ: element_type
			typ_name: element_type_name
			address: typed_address
		}
	}
	if p.program.m.type_store.types[value.typ].kind == .ptr_t {
		p.expect(.lsbr)!
		index := p.parse_expression(0)!
		p.expect(.rsbr)!
		element_type := p.program.m.type_store.types[value.typ].elem_type
		element_type_name := fast_arm64_pointer_element_type_name(value.typ_name)
		element_size := p.program.m.get_or_add_const(p.program.i64_type, p.program.m.type_size(element_type).str())
		index64 := p.integer_to_i64(index)
		offset := p.program.instr2(.mul, p.cur_block, p.program.i64_type, index64, element_size)
		address := p.program.instr2(.add, p.cur_block, value.typ, value.id, offset)
		return FastArm64Value{
			id: p.program.instr1(.load, p.cur_block, element_type, address)
			typ: element_type
			typ_name: element_type_name
			address: address
		}
	}
	if value.typ == p.program.str_type {
		p.expect(.lsbr)!
		mut start := FastArm64Value{
			id: p.program.m.get_or_add_const(p.program.i32_type, '0')
			typ: p.program.i32_type
			typ_name: 'int'
		}
		if p.tok != .dotdot {
			start = p.parse_expression(0)!
		}
		value_slot := p.program.instr0(.alloca, p.cur_block, p.program.m.type_store.get_ptr(p.program.str_type))
		p.program.instr2(.store, p.cur_block, p.program.void_type, value.id, value_slot)
		data := p.program.instr1(.load, p.cur_block, p.program.ptr_i8, p.program.string_field_ptr(p.cur_block, value_slot, 0))
		if p.tok != .dotdot {
			p.expect(.rsbr)!
			length := p.program.instr1(.load, p.cur_block, p.program.i32_type, p.program.string_field_ptr(p.cur_block, value_slot, 1))
			start64 := p.checked_array_index(start, length, 'string_index')
			start_data := p.program.instr2(.add, p.cur_block, p.program.ptr_i8, data, start64)
			return FastArm64Value{
				id: p.program.instr1(.load, p.cur_block, p.program.u8_type, start_data)
				typ: p.program.u8_type
				typ_name: 'u8'
				address: start_data
			}
		}
		p.next()
		original_len := p.program.instr1(.load, p.cur_block, p.program.i32_type, p.program.string_field_ptr(p.cur_block, value_slot, 1))
		end := if p.tok == .rsbr {
			FastArm64Value{
				id: original_len
				typ: p.program.i32_type
				typ_name: 'int'
			}
		} else {
			p.parse_expression(0)!
		}
		p.expect(.rsbr)!
		start64, end64 := p.checked_array_slice_bounds(start, end, original_len, 'string_slice')
		start_data := p.program.instr2(.add, p.cur_block, p.program.ptr_i8, data, start64)
		length64 := p.program.instr2(.sub, p.cur_block, p.program.i64_type, end64, start64)
		length := p.program.instr1(.trunc, p.cur_block, p.program.i32_type, length64)
		one64 := p.program.m.get_or_add_const(p.program.i64_type, '1')
		allocation_size := p.program.instr2(.add, p.cur_block, p.program.i64_type, length64, one64)
		malloc_ref := p.program.m.add_value(.func_ref, p.program.ptr_i8, 'malloc', p.program.fn_ids['malloc'])
		owned_data := p.program.m.add_instr(.call, p.cur_block, p.program.ptr_i8, [
			malloc_ref,
			allocation_size,
		])
		memcpy_ref := p.program.m.add_value(.func_ref, p.program.ptr_i8, 'memcpy', p.program.fn_ids['memcpy'])
		p.program.m.add_instr(.call, p.cur_block, p.program.ptr_i8, [memcpy_ref, owned_data,
			start_data, length64])
		end_data := p.program.instr2(.add, p.cur_block, p.program.ptr_i8, owned_data, length64)
		zero_byte := p.program.m.get_or_add_const(p.program.u8_type, '0')
		p.program.instr2(.store, p.cur_block, p.program.void_type, zero_byte, end_data)
		result_slot := p.program.instr0(.alloca, p.cur_block, p.program.m.type_store.get_ptr(p.program.str_type))
		p.program.instr2(.store, p.cur_block, p.program.void_type, owned_data, p.program.string_field_ptr(p.cur_block, result_slot, 0))
		p.program.instr2(.store, p.cur_block, p.program.void_type, length, p.program.string_field_ptr(p.cur_block, result_slot, 1))
		zero := p.program.m.get_or_add_const(p.program.i32_type, '0')
		p.program.instr2(.store, p.cur_block, p.program.void_type, zero, p.program.string_field_ptr(p.cur_block, result_slot, 2))
		return FastArm64Value{
			id: p.program.instr1(.load, p.cur_block, p.program.str_type, result_slot)
			typ: p.program.str_type
			typ_name: 'string'
		}
	}
	if value.typ != p.program.array_type {
		return p.unsupported('indexing non-array `${value.typ_name}`')
	}
	p.expect(.lsbr)!
	mut start := FastArm64Value{
		id: p.program.m.get_or_add_const(p.program.i32_type, '0')
		typ: p.program.i32_type
		typ_name: 'int'
	}
	if p.tok != .dotdot {
		start = p.parse_expression(0)!
	}
	if p.tok != .dotdot {
		p.expect(.rsbr)!
		element_type_name := p.program.array_element_type_name(value.typ_name) or {
			return p.unsupported('array type `${value.typ_name}`')
		}
		element_type := p.program.type_id(element_type_name)
		value_slot := p.program.instr0(.alloca, p.cur_block, p.program.m.type_store.get_ptr(p.program.array_type))
		p.program.instr2(.store, p.cur_block, p.program.void_type, value.id, value_slot)
		length := p.program.instr1(.load, p.cur_block, p.program.i32_type, p.program.struct_field_ptr(p.cur_block, value_slot, p.program.array_type, 2))
		index64 := p.checked_array_index(start, length, 'array_index')
		data := p.program.instr1(.load, p.cur_block, p.program.ptr_i8, p.program.struct_field_ptr(p.cur_block, value_slot, p.program.array_type, 0))
		element_size := p.program.instr1(.load, p.cur_block, p.program.i32_type, p.program.struct_field_ptr(p.cur_block, value_slot, p.program.array_type, 5))
		element_size64 := p.program.instr1(.zext, p.cur_block, p.program.i64_type, element_size)
		offset := p.program.instr2(.mul, p.cur_block, p.program.i64_type, index64, element_size64)
		address := p.program.instr2(.add, p.cur_block, p.program.ptr_i8, data, offset)
		typed_address := p.program.instr1(.bitcast, p.cur_block, p.program.m.type_store.get_ptr(element_type), address)
		return FastArm64Value{
			id: p.program.instr1(.load, p.cur_block, element_type, typed_address)
			typ: element_type
			typ_name: element_type_name
			address: typed_address
		}
	}
	p.next()
	mut has_end := false
	mut end := FastArm64Value{}
	if p.tok != .rsbr {
		end = p.parse_expression(0)!
		has_end = true
	}
	p.expect(.rsbr)!
	ptr_array := p.program.m.type_store.get_ptr(p.program.array_type)
	slot := p.program.instr0(.alloca, p.cur_block, ptr_array)
	p.program.instr2(.store, p.cur_block, p.program.void_type, value.id, slot)
	data_ptr := p.program.struct_field_ptr(p.cur_block, slot, p.program.array_type, 0)
	offset_ptr := p.program.struct_field_ptr(p.cur_block, slot, p.program.array_type, 1)
	len_ptr := p.program.struct_field_ptr(p.cur_block, slot, p.program.array_type, 2)
	cap_ptr := p.program.struct_field_ptr(p.cur_block, slot, p.program.array_type, 3)
	element_size_ptr := p.program.struct_field_ptr(p.cur_block, slot, p.program.array_type, 5)
	data := p.program.instr1(.load, p.cur_block, p.program.ptr_i8, data_ptr)
	offset := p.program.instr1(.load, p.cur_block, p.program.i32_type, offset_ptr)
	length := p.program.instr1(.load, p.cur_block, p.program.i32_type, len_ptr)
	element_size := p.program.instr1(.load, p.cur_block, p.program.i32_type, element_size_ptr)
	effective_end := if has_end {
		end
	} else {
		FastArm64Value{
			id: length
			typ: p.program.i32_type
			typ_name: 'int'
		}
	}
	start64, end64 := p.checked_array_slice_bounds(start, effective_end, length, 'array_slice')
	start32 := p.program.instr1(.trunc, p.cur_block, p.program.i32_type, start64)
	end32 := p.program.instr1(.trunc, p.cur_block, p.program.i32_type, end64)
	element_size64 := p.program.instr1(.zext, p.cur_block, p.program.i64_type, element_size)
	byte_offset := p.program.instr2(.mul, p.cur_block, p.program.i64_type, start64, element_size64)
	new_data := p.program.instr2(.add, p.cur_block, p.program.ptr_i8, data, byte_offset)
	byte_offset32 := p.program.instr1(.trunc, p.cur_block, p.program.i32_type, byte_offset)
	new_offset := p.program.instr2(.add, p.cur_block, p.program.i32_type, offset, byte_offset32)
	new_length := p.program.instr2(.sub, p.cur_block, p.program.i32_type, end32, start32)
	p.program.instr2(.store, p.cur_block, p.program.void_type, new_data, data_ptr)
	p.program.instr2(.store, p.cur_block, p.program.void_type, new_offset, offset_ptr)
	p.program.instr2(.store, p.cur_block, p.program.void_type, new_length, len_ptr)
	p.program.instr2(.store, p.cur_block, p.program.void_type, new_length, cap_ptr)
	p.emit_array_mark_has_slice(slot)
	return FastArm64Value{
		id: p.program.instr1(.load, p.cur_block, p.program.array_type, slot)
		typ: p.program.array_type
		typ_name: value.typ_name
	}
}

fn (mut p FastArm64Parser) emit_array_mark_has_slice(array_slot ssa.ValueID) {
	flags_ptr := p.program.struct_field_ptr(p.cur_block, array_slot, p.program.array_type, 4)
	flags := p.program.instr1(.load, p.cur_block, p.program.i32_type, flags_ptr)
	is_slice_flag := p.program.m.get_or_add_const(p.program.i32_type, '64')
	new_flags := p.program.instr2(.or_, p.cur_block, p.program.i32_type, flags, is_slice_flag)
	p.program.instr2(.store, p.cur_block, p.program.void_type, new_flags, flags_ptr)
	array_value := p.program.instr1(.load, p.cur_block, p.program.array_type, array_slot)
	mark_ref := p.program.m.add_value(.func_ref, p.program.void_type, 'fast_array_mark_has_slices', p.program.fn_ids['fast_array_mark_has_slices'])
	p.program.m.add_instr(.call, p.cur_block, p.program.void_type, [mark_ref, array_value])
}

fn (mut p FastArm64Parser) emit_array_buffer_has_slices(array_slot ssa.ValueID) ssa.ValueID {
	return p.emit_array_buffer_has_slices_at(array_slot, p.cur_block)
}

fn (mut p FastArm64Parser) emit_array_buffer_has_slices_at(array_slot ssa.ValueID, block ssa.BlockID) ssa.ValueID {
	array_value := p.program.instr1(.load, block, p.program.array_type, array_slot)
	has_ref := p.program.m.add_value(.func_ref, p.program.i1_type, 'fast_array_buffer_has_slices', p.program.fn_ids['fast_array_buffer_has_slices'])
	return p.program.m.add_instr(.call, block, p.program.i1_type, [has_ref, array_value])
}

fn (mut p FastArm64Parser) checked_array_index(index FastArm64Value, length ssa.ValueID, label string) ssa.ValueID {
	index64 := p.integer_to_i64(index)
	zero64 := p.program.m.get_or_add_const(p.program.i64_type, '0')
	index_layout := p.program.m.type_store.types[index.typ]
	below_start := if index_layout.is_unsigned {
		p.program.m.get_or_add_const(p.program.i1_type, '0')
	} else {
		p.program.instr2(.lt, p.cur_block, p.program.i1_type, index64, zero64)
	}
	length64 := p.program.instr1(.zext, p.cur_block, p.program.i64_type, length)
	past_end := p.program.instr2(.uge, p.cur_block, p.program.i1_type, index64, length64)
	invalid := p.program.instr2(.or_, p.cur_block, p.program.i1_type, below_start, past_end)
	invalid_block := p.program.m.add_block(p.func_id, '${label}_invalid')
	valid_block := p.program.m.add_block(p.func_id, '${label}_valid')
	p.program.instr3(.br, p.cur_block, p.program.void_type, invalid, ssa.ValueID(invalid_block), ssa.ValueID(valid_block))
	p.mark_terminated(p.cur_block)
	exit_ref := p.program.m.add_value(.func_ref, p.program.void_type, 'exit', p.program.fn_ids['exit'])
	exit_code := p.program.m.get_or_add_const(p.program.i32_type, '1')
	p.program.m.add_instr(.call, invalid_block, p.program.void_type, [exit_ref, exit_code])
	p.program.instr0(.unreachable, invalid_block, p.program.void_type)
	p.mark_terminated(invalid_block)
	p.cur_block = valid_block
	return index64
}

fn (mut p FastArm64Parser) checked_array_slice_bounds(start FastArm64Value, end FastArm64Value, length ssa.ValueID, label string) (ssa.ValueID, ssa.ValueID) {
	start64 := p.integer_to_i64(start)
	end64 := p.integer_to_i64(end)
	zero64 := p.program.m.get_or_add_const(p.program.i64_type, '0')
	length64 := p.program.instr1(.zext, p.cur_block, p.program.i64_type, length)
	start_negative := if p.program.m.type_store.types[start.typ].is_unsigned {
		p.program.m.get_or_add_const(p.program.i1_type, '0')
	} else {
		p.program.instr2(.lt, p.cur_block, p.program.i1_type, start64, zero64)
	}
	end_negative := if p.program.m.type_store.types[end.typ].is_unsigned {
		p.program.m.get_or_add_const(p.program.i1_type, '0')
	} else {
		p.program.instr2(.lt, p.cur_block, p.program.i1_type, end64, zero64)
	}
	start_past_end := p.program.instr2(.ugt, p.cur_block, p.program.i1_type, start64, length64)
	end_before_start := p.program.instr2(.ult, p.cur_block, p.program.i1_type, end64, start64)
	end_past_length := p.program.instr2(.ugt, p.cur_block, p.program.i1_type, end64, length64)
	invalid_start := p.program.instr2(.or_, p.cur_block, p.program.i1_type, start_negative, start_past_end)
	invalid_end := p.program.instr2(.or_, p.cur_block, p.program.i1_type, end_negative, end_before_start)
	invalid_range := p.program.instr2(.or_, p.cur_block, p.program.i1_type, invalid_start, invalid_end)
	invalid := p.program.instr2(.or_, p.cur_block, p.program.i1_type, invalid_range, end_past_length)
	invalid_block := p.program.m.add_block(p.func_id, '${label}_invalid')
	valid_block := p.program.m.add_block(p.func_id, '${label}_valid')
	p.program.instr3(.br, p.cur_block, p.program.void_type, invalid, ssa.ValueID(invalid_block), ssa.ValueID(valid_block))
	p.mark_terminated(p.cur_block)
	exit_ref := p.program.m.add_value(.func_ref, p.program.void_type, 'exit', p.program.fn_ids['exit'])
	exit_code := p.program.m.get_or_add_const(p.program.i32_type, '1')
	p.program.m.add_instr(.call, invalid_block, p.program.void_type, [exit_ref, exit_code])
	p.program.instr0(.unreachable, invalid_block, p.program.void_type)
	p.mark_terminated(invalid_block)
	p.cur_block = valid_block
	return start64, end64
}

fn (mut p FastArm64Parser) parse_map_literal() !FastArm64Value {
	p.expect(.lsbr)!
	key_type_name, key_next := fastc_scan_type(mut p.s, p.tok, p.source_file.path, p.source_file.header.module_name, p.source_file.header.imports, p.program.declared_types, false) or { return p.unsupported('map key type') }
	p.tok = key_next
	p.lit = p.s.lit
	p.expect(.rsbr)!
	value_type_name, value_next := fastc_scan_type(mut p.s, p.tok, p.source_file.path, p.source_file.header.module_name, p.source_file.header.imports, p.program.declared_types, false) or { return p.unsupported('map value type') }
	p.tok = value_next
	p.lit = p.s.lit
	map_type_name := fastc_map_c_type(key_type_name, value_type_name)
	mut result := p.new_empty_map_value(map_type_name)!
	key_type := p.program.type_id(key_type_name)
	p.expect(.lcbr)!
	for p.tok != .rcbr {
		if p.tok == .semicolon {
			p.next()
			continue
		}
		mut key := p.parse_expression(0)!
		if key.typ != key_type {
			key = p.convert_value(key, key_type, key_type_name)
		}
		p.expect(.colon)!
		item := p.parse_contextual_value(value_type_name)!
		p.emit_map_set(result, key, item)!
		if p.tok in [.comma, .semicolon] {
			p.next()
		} else if p.tok != .rcbr {
			return p.unsupported('map literal separator')
		}
	}
	p.next()
	return result
}

fn (mut p FastArm64Parser) new_empty_map_value(map_type_name string) !FastArm64Value {
	key_type_name, value_type_name := fastc_map_key_value_types(map_type_name) or {
		return p.unsupported('map type `${map_type_name}`')
	}
	key_type := p.program.type_id(key_type_name)
	value_type := p.program.type_id(value_type_name)
	key_size := p.program.m.get_or_add_const(p.program.i64_type, p.program.m.type_size(key_type).str())
	value_size := p.program.m.get_or_add_const(p.program.i64_type, p.program.m.type_size(value_type).str())
	string_key := p.program.m.get_or_add_const(p.program.i64_type, if key_type == p.program.str_type {
		'1'
	} else {
		'0'
	})
	default_value := p.default_value_for_type(value_type, value_type_name)!
	default_slot := p.program.instr0(.alloca, p.cur_block, p.program.m.type_store.get_ptr(value_type))
	p.program.instr2(.store, p.cur_block, p.program.void_type, default_value.id, default_slot)
	default_pointer := p.program.instr1(.bitcast, p.cur_block, p.program.ptr_i8, default_slot)
	new_ref := p.program.m.add_value(.func_ref, p.program.map_type, 'fast_map_new', p.program.fn_ids['fast_map_new'])
	return FastArm64Value{
		id: p.program.m.add_instr(.call, p.cur_block, p.program.map_type, [new_ref, key_size,
			value_size, string_key, default_pointer])
		typ: p.program.map_type
		typ_name: map_type_name
	}
}

fn (mut p FastArm64Parser) parse_inferred_map_literal() !FastArm64Value {
	expected_key_type_name := p.map_key
	expected_value_type_name := p.map_value
	p.expect(.lcbr)!
	mut keys := []FastArm64Value{}
	mut values := []FastArm64Value{}
	for p.tok != .rcbr {
		if p.tok == .semicolon {
			p.next()
			continue
		}
		keys << if expected_key_type_name == '' && keys.len == 0 {
			p.parse_expression(0)!
		} else {
			p.parse_contextual_value(if expected_key_type_name == '' {
				keys[0].typ_name
			} else {
				expected_key_type_name
			})!
		}
		p.expect(.colon)!
		values << if expected_value_type_name == '' && values.len == 0 {
			p.parse_expression(0)!
		} else {
			p.parse_contextual_value(if expected_value_type_name == '' {
				values[0].typ_name
			} else {
				expected_value_type_name
			})!
		}
		if p.tok in [.comma, .semicolon] {
			p.next()
		} else if p.tok != .rcbr {
			return p.unsupported('inferred map literal separator')
		}
	}
	p.next()
	if keys.len == 0 {
		return p.unsupported('empty inferred map literal')
	}
	key_type_name := if expected_key_type_name == '' {
		keys[0].typ_name
	} else {
		expected_key_type_name
	}
	value_type_name := if expected_value_type_name == '' {
		values[0].typ_name
	} else {
		expected_value_type_name
	}
	mut result := p.new_empty_map_value(fastc_map_c_type(key_type_name, value_type_name))!
	for i, key in keys {
		p.emit_map_set(result, key, values[i])!
	}
	return result
}

fn (mut p FastArm64Parser) emit_map_set(map_value FastArm64Value, key FastArm64Value, value FastArm64Value) ! {
	key_type_name, value_type_name := fastc_map_key_value_types(map_value.typ_name) or {
		return p.unsupported('map type `${map_value.typ_name}`')
	}
	key_type := p.program.type_id(key_type_name)
	value_type := p.program.type_id(value_type_name)
	mut stored_key := key
	mut stored_value := value
	if stored_key.typ != key_type {
		stored_key = p.convert_value(stored_key, key_type, key_type_name)
	}
	if stored_value.typ != value_type {
		stored_value = p.convert_value(stored_value, value_type, value_type_name)
	}
	mut initialized_map := map_value
	if map_value.address != ssa.ValueID(0) {
		map_slot := p.program.instr0(.alloca, p.cur_block, p.program.m.type_store.get_ptr(p.program.map_type))
		p.program.instr2(.store, p.cur_block, p.program.void_type, map_value.id, map_slot)
		state_type := p.program.m.type_store.get_ptr(p.program.map_state_type)
		state := p.program.instr1(.load, p.cur_block, state_type, p.program.struct_field_ptr(p.cur_block, map_slot, p.program.map_type, 0))
		has_state := p.program.instr2(.ne, p.cur_block, p.program.i1_type, state, p.program.m.get_or_add_const(state_type, '0'))
		existing := p.program.m.add_block(p.func_id, 'map_set_existing_state')
		initialize := p.program.m.add_block(p.func_id, 'map_set_initialize')
		ready := p.program.m.add_block(p.func_id, 'map_set_ready')
		result_slot := p.program.instr0(.alloca, p.cur_block, p.program.m.type_store.get_ptr(p.program.map_type))
		p.program.instr3(.br, p.cur_block, p.program.void_type, has_state, ssa.ValueID(existing), ssa.ValueID(initialize))
		p.mark_terminated(p.cur_block)
		p.program.instr2(.store, existing, p.program.void_type, map_value.id, result_slot)
		p.program.instr1(.jmp, existing, p.program.void_type, ssa.ValueID(ready))
		p.mark_terminated(existing)
		p.cur_block = initialize
		initialized_value := p.new_empty_map_value(map_value.typ_name)!
		p.program.instr2(.store, p.cur_block, p.program.void_type, initialized_value.id, map_value.address)
		p.program.instr2(.store, p.cur_block, p.program.void_type, initialized_value.id, result_slot)
		p.program.instr1(.jmp, p.cur_block, p.program.void_type, ssa.ValueID(ready))
		p.mark_terminated(p.cur_block)
		p.cur_block = ready
		initialized_map = FastArm64Value{
			...map_value
			id: p.program.instr1(.load, ready, p.program.map_type, result_slot)
		}
	}
	key_slot := p.program.instr0(.alloca, p.cur_block, p.program.m.type_store.get_ptr(key_type))
	value_slot := p.program.instr0(.alloca, p.cur_block, p.program.m.type_store.get_ptr(value_type))
	p.program.instr2(.store, p.cur_block, p.program.void_type, stored_key.id, key_slot)
	p.program.instr2(.store, p.cur_block, p.program.void_type, stored_value.id, value_slot)
	key_pointer := p.program.instr1(.bitcast, p.cur_block, p.program.ptr_i8, key_slot)
	value_pointer := p.program.instr1(.bitcast, p.cur_block, p.program.ptr_i8, value_slot)
	set_ref := p.program.m.add_value(.func_ref, p.program.void_type, 'fast_map_set', p.program.fn_ids['fast_map_set'])
	p.program.m.add_instr(.call, p.cur_block, p.program.void_type, [set_ref, initialized_map.id,
		key_pointer, value_pointer])
}

fn (mut p FastArm64Parser) parse_cast(type_name string) !FastArm64Value {
	p.expect(.lpar)!
	value := p.parse_expression(0)!
	p.expect(.rpar)!
	typ := p.program.type_id(type_name)
	return p.convert_value(value, typ, type_name)
}

fn (mut p FastArm64Parser) convert_value(value FastArm64Value, typ ssa.TypeID, type_name string) FastArm64Value {
	if typ == value.typ {
		return FastArm64Value{
			...value
			typ_name: type_name
		}
	}
	from := p.program.m.type_store.types[value.typ]
	to := p.program.m.type_store.types[typ]
	if to.kind == .array_t && from.kind != .array_t {
		mut result := p.zero_value(typ, type_name)
		element_type_name := fastc_fixed_array_element_type(type_name) or { '' }
		mut element := value
		if element.typ != to.elem_type {
			element = p.convert_value(element, to.elem_type, element_type_name)
		}
		address := p.program.instr1(.bitcast, p.cur_block, p.program.m.type_store.get_ptr(to.elem_type), result.address)
		p.program.instr2(.store, p.cur_block, p.program.void_type, element.id, address)
		result = FastArm64Value{
			...result
			id: p.program.instr1(.load, p.cur_block, typ, result.address)
		}
		return result
	}
	op := if from.kind == .float_t && to.kind == .int_t {
		if to.is_unsigned { ssa.OpCode.fptoui } else { ssa.OpCode.fptosi }
	} else if from.kind == .int_t && to.kind == .float_t {
		if from.is_unsigned { ssa.OpCode.uitofp } else { ssa.OpCode.sitofp }
	} else if from.kind == .float_t && to.kind == .float_t {
		ssa.OpCode.bitcast
	} else if from.width > to.width {
		ssa.OpCode.trunc
	} else if from.width < to.width {
		if from.is_unsigned { ssa.OpCode.zext } else { ssa.OpCode.sext }
	} else {
		ssa.OpCode.bitcast
	}
	return FastArm64Value{
		id: p.program.instr1(op, p.cur_block, typ, value.id)
		typ: typ
		typ_name: type_name
	}
}

fn (mut p FastArm64Parser) resolve_call_key(key string, display_name string) ?string {
	if key in p.program.functions || key in p.program.fn_ids {
		return key
	}
	builtin_key := fastc_function_key('builtin', display_name)
	if builtin_key in p.program.functions || builtin_key in p.program.fn_ids {
		return builtin_key
	}
	if display_name in p.program.functions || display_name in p.program.fn_ids {
		return display_name
	}
	return none
}

fn (mut p FastArm64Parser) parse_call(key string, display_name string) !FastArm64Value {
	is_spawn_call := p.parsing_spawn
	p.parsing_spawn = false
	if display_name == 'panic' {
		if is_spawn_call {
			return p.unsupported('spawn of `${display_name}`')
		}
		p.expect(.lpar)!
		message := p.parse_expression(0)!
		p.expect(.rpar)!
		println_id := p.program.fn_ids['println']
		println_ref := p.program.m.add_value(.func_ref, p.program.void_type, 'println', println_id)
		p.program.m.add_instr(.call, p.cur_block, p.program.void_type, [println_ref, message.id])
		exit_ref := p.program.m.add_value(.func_ref, p.program.void_type, 'exit', p.program.fn_ids['exit'])
		one := p.program.m.get_or_add_const(p.program.i32_type, '1')
		p.program.m.add_instr(.call, p.cur_block, p.program.void_type, [exit_ref, one])
		p.program.instr0(.unreachable, p.cur_block, p.program.void_type)
		p.mark_terminated(p.cur_block)
		return FastArm64Value{
			typ: p.program.void_type
			typ_name: 'void'
		}
	}
	if display_name in ['C.FD_ZERO', 'C.FD_SET', 'C.FD_ISSET'] {
		return p.parse_fd_set_macro(display_name)
	}
	if signature := p.program.functions[key] {
		if signature.is_disabled {
			p.skip_group(.lpar, .rpar)!
			return FastArm64Value{
				typ: p.program.void_type
				typ_name: 'void'
			}
		}
	}
	resolved := p.resolve_call_key(key, display_name) or {
		return p.unsupported('call to `${display_name}`')
	}
	if resolved == 'os.exec' {
		return p.unsupported('`os.exec` on the direct ARM64 backend')
	}
	p.program.native_used_function_names[resolved] = true
	signature := p.program.functions[resolved]
	p.expect(.lpar)!
	mut call_args := []FastArm64Value{}
	for p.tok != .rpar {
		if p.tok == .key_mut {
			p.next()
		}
		argument_index := call_args.len
		mut expected_type_name := ''
		if signature.is_variadic && argument_index >= signature.parameter_types.len - 1 {
			if !resolved.starts_with('C.') {
				expected_type_name = p.program.array_element_type_name(signature.parameter_types.last()) or {
					''
				}
			}
		} else if argument_index < signature.parameter_types.len {
			expected_type_name = signature.parameter_types[argument_index]
		}
		if p.tok == .name && expected_type_name != '' {
			mut look := p.s
			if look.scan() == .colon {
				call_args << p.parse_named_argument_struct(expected_type_name)!
				break
			}
		}
		if expected_type_name != '' {
			call_args << p.parse_contextual_value(expected_type_name)!
		} else {
			call_args << p.parse_expression(0)!
		}
		if p.tok == .comma {
			p.next()
		} else if p.tok != .rpar {
			return p.unsupported('call argument separator')
		}
	}
	p.next()
	if signature.is_variadic && !resolved.starts_with('C.') {
		if is_spawn_call {
			return p.unsupported('spawn of variadic function `${display_name}`')
		}
		call_args = p.pack_v_variadic_arguments(signature, call_args, 0)!
	}
	if signature.last_parameter_is_params && call_args.len < signature.parameter_types.len {
		params_type := signature.parameter_types.last()
		call_args << p.default_struct_value(params_type)!
	}
	p.validate_call_argument_count(display_name, resolved, signature, call_args, 0)!
	if resolved in ['print', 'println', 'builtin.print', 'builtin.println'] {
		if call_args.len != 1 || call_args[0].typ != p.program.str_type {
			return p.unsupported('`${display_name}` with a non-string argument')
		}
	}
	func_id := p.program.register_signature_function(resolved) or {
		return p.unsupported('registered call `${resolved}`')
	}
	ret := p.program.fn_returns[resolved]
	symbol := p.program.fn_symbols[resolved]
	fn_ref := p.program.m.add_value(.func_ref, ret, symbol, func_id)
	mut operands := []ssa.ValueID{cap: call_args.len + 1}
	operands << fn_ref
	c_variadic_fixed_count := if signature.is_variadic && resolved.starts_with('C.') {
		fast_arm64_c_variadic_fixed_parameter_count(signature)
	} else {
		-1
	}
	for i, original_argument in call_args {
		if c_variadic_fixed_count >= 0 && i >= c_variadic_fixed_count {
			operands << p.promote_c_variadic_argument(original_argument).id
		} else if i < signature.parameter_types.len {
			operands << p.call_argument(original_argument, signature.parameter_types[i])
		} else {
			operands << original_argument.id
		}
	}
	if is_spawn_call {
		if signature.return_type == 'Option' || signature.return_types.len > 0 {
			return p.unsupported('spawn of optional or multi-return function `${display_name}`')
		}
		return p.emit_spawn_call(resolved, symbol, func_id, ret, signature.return_type, signature.parameter_types, operands[1..])
	}
	if signature.return_type == 'Option' {
		p.store_option_success()
	}
	result := p.program.m.add_instr(.call, p.cur_block, ret, operands)
	option_state := if signature.return_type == 'Option' {
		p.option_state_pointer()
	} else {
		ssa.ValueID(0)
	}
	option_failed := if signature.return_type == 'Option' {
		p.program.instr1(.load, p.cur_block, p.program.i1_type, p.program.struct_field_ptr(p.cur_block, option_state, p.program.option_state_type, 0))
	} else {
		ssa.ValueID(0)
	}
	option_error_type := if signature.return_type == 'Option' {
		p.program.instr1(.load, p.cur_block, p.program.u64_type, p.program.struct_field_ptr(p.cur_block, option_state, p.program.option_state_type, 1))
	} else {
		ssa.ValueID(0)
	}
	option_error_code := if signature.return_type == 'Option' {
		p.program.instr1(.load, p.cur_block, p.program.i32_type, p.program.struct_field_ptr(p.cur_block, option_state, p.program.option_state_type, 2))
	} else {
		ssa.ValueID(0)
	}
	option_error_message := if signature.return_type == 'Option' {
		p.load_option_error_message(option_state)
	} else {
		ssa.ValueID(0)
	}
	return FastArm64Value{
		id: result
		typ: ret
		option_failed: option_failed
		option_error_type: option_error_type
		option_error_message: option_error_message
		option_error_code: option_error_code
		typ_name: if signature.return_type == 'Option' {
			signature.option_type
		} else {
			signature.return_type
		}
		tuple_types: signature.return_types
	}
}

fn (mut p FastArm64Parser) pack_v_variadic_arguments(signature FastcFunctionSignature, arguments []FastArm64Value, parameter_offset int) ![]FastArm64Value {
	fixed_count := signature.parameter_types.len - parameter_offset - 1
	if fixed_count < 0 || arguments.len < fixed_count {
		return p.unsupported('variadic argument count')
	}
	mut packed := []FastArm64Value{cap: signature.parameter_types.len - parameter_offset}
	for i in 0 .. fixed_count {
		packed << arguments[i]
	}
	mut variadic_items := []FastArm64Value{cap: arguments.len - fixed_count}
	for i in fixed_count .. arguments.len {
		variadic_items << arguments[i]
	}
	element_type_name := p.program.array_element_type_name(signature.parameter_types.last()) or {
		return p.unsupported('variadic parameter `${signature.parameter_types.last()}`')
	}
	length := FastArm64Value{
		id: p.program.m.get_or_add_const(p.program.i32_type, variadic_items.len.str())
		typ: p.program.i32_type
		typ_name: 'int'
	}
	packed << p.make_array(element_type_name, variadic_items, length, length)
	return packed
}

fn (p &FastArm64Parser) validate_call_argument_count(display_name string, resolved string, signature FastcFunctionSignature, arguments []FastArm64Value, parameter_offset int) ! {
	if resolved !in p.program.functions {
		// Builtins such as `print`/`println` are registered without a FastcFunctionSignature
		// (see register_print_runtime) and are validated by their own special-cased handling,
		// so the empty default signature here carries no real arity to check against.
		return
	}
	expected_count := signature.parameter_types.len - parameter_offset
	if expected_count < 0 {
		return p.unsupported('function `${display_name}` parameter count')
	}
	if signature.is_variadic && resolved.starts_with('C.') {
		fixed_count := fast_arm64_c_variadic_fixed_parameter_count(signature) - parameter_offset
		if fixed_count < 0 || arguments.len < fixed_count {
			return p.unsupported('function `${display_name}` call with ${arguments.len} arguments instead of at least ${fixed_count}')
		}
		return
	}
	if arguments.len != expected_count {
		return p.unsupported('function `${display_name}` call with ${arguments.len} arguments instead of ${expected_count}')
	}
}

fn fast_arm64_c_variadic_fixed_parameter_count(signature FastcFunctionSignature) int {
	if signature.is_variadic && signature.parameter_types.len > 0 && signature.parameter_types.last().starts_with('Array_') {
		return signature.parameter_types.len - 1
	}
	return signature.parameter_types.len
}

fn (mut p FastArm64Parser) promote_c_variadic_argument(argument FastArm64Value) FastArm64Value {
	layout := p.program.m.type_store.types[argument.typ]
	if layout.kind == .float_t && layout.width < 64 {
		return p.convert_value(argument, p.program.f64_type, 'f64')
	}
	if layout.kind == .int_t && layout.width < 32 {
		return p.convert_value(argument, p.program.i32_type, 'int')
	}
	return argument
}

fn (mut p FastArm64Parser) parse_fd_set_macro(display_name string) !FastArm64Value {
	p.program.ensure_c_fd_type('C.fd_set')
	p.expect(.lpar)!
	mut descriptor := FastArm64Value{}
	if display_name != 'C.FD_ZERO' {
		descriptor = p.parse_expression(0)!
		p.expect(.comma)!
	}
	set := p.parse_expression(0)!
	p.expect(.rpar)!
	if p.program.m.type_store.types[set.typ].kind != .ptr_t {
		return p.unsupported('`${display_name}` fd-set pointer')
	}
	set_bytes := p.program.instr1(.bitcast, p.cur_block, p.program.ptr_i8, set.id)
	if display_name == 'C.FD_ZERO' {
		memset_ref := p.program.m.add_value(.func_ref, p.program.ptr_i8, 'memset', p.program.fn_ids['memset'])
		zero := p.program.m.get_or_add_const(p.program.i32_type, '0')
		fd_set_size := p.program.m.get_or_add_const(p.program.i64_type, '128')
		p.program.m.add_instr(.call, p.cur_block, p.program.ptr_i8, [memset_ref, set_bytes, zero,
			fd_set_size])
		return FastArm64Value{
			typ: p.program.void_type
			typ_name: 'void'
		}
	}
	descriptor64 := p.integer_to_i64(descriptor)
	three := p.program.m.get_or_add_const(p.program.i64_type, '3')
	byte_index := p.program.instr2(.lshr, p.cur_block, p.program.i64_type, descriptor64, three)
	byte_address := p.program.instr2(.add, p.cur_block, p.program.ptr_i8, set_bytes, byte_index)
	current := p.program.instr1(.load, p.cur_block, p.program.u8_type, byte_address)
	seven := p.program.m.get_or_add_const(p.program.i64_type, '7')
	bit_index := p.program.instr2(.and_, p.cur_block, p.program.i64_type, descriptor64, seven)
	one := p.program.m.get_or_add_const(p.program.i64_type, '1')
	mask64 := p.program.instr2(.shl, p.cur_block, p.program.i64_type, one, bit_index)
	mask := p.program.instr1(.trunc, p.cur_block, p.program.u8_type, mask64)
	selected := p.program.instr2(.and_, p.cur_block, p.program.u8_type, current, mask)
	if display_name == 'C.FD_ISSET' {
		zero := p.program.m.get_or_add_const(p.program.u8_type, '0')
		is_set := p.program.instr2(.ne, p.cur_block, p.program.i1_type, selected, zero)
		return FastArm64Value{
			id: p.program.instr1(.zext, p.cur_block, p.program.i32_type, is_set)
			typ: p.program.i32_type
			typ_name: 'int'
		}
	}
	updated := p.program.instr2(.or_, p.cur_block, p.program.u8_type, current, mask)
	p.program.instr2(.store, p.cur_block, p.program.void_type, updated, byte_address)
	return FastArm64Value{
		typ: p.program.void_type
		typ_name: 'void'
	}
}

fn (mut p FastArm64Parser) emit_spawn_call(function_key string, symbol string, function_id int, return_type ssa.TypeID, return_type_name string, parameter_type_names []string, arguments []ssa.ValueID) !FastArm64Value {
	mut parameter_types := []ssa.TypeID{cap: parameter_type_names.len}
	for parameter_type_name in parameter_type_names {
		parameter_types << p.program.type_id(parameter_type_name)
	}
	if p.suppress_spawn_wrapper {
		// A `sizeof(spawn f())` only needs the thread-handle type. Registering the spawn
		// wrapper (and its inline body) here would leak a cached, body-less wrapper once
		// the speculative parse is discarded, so a later real `spawn f()` would reuse the
		// cached wrapper id and emit an unresolved call. Return the type only.
		return FastArm64Value{
			id: p.program.m.get_or_add_const(p.program.u64_type, '0')
			typ: p.program.u64_type
			typ_name: 'thread ${return_type_name}'
			is_spawned: true
			spawn_result_type: return_type
			spawn_result_name: return_type_name
		}
	}
	context_type, wrapper_id := p.program.register_spawn_wrapper(function_key, function_id, symbol, return_type, parameter_types)
	context_size := p.program.m.get_or_add_const(p.program.i64_type, p.program.m.type_size(context_type).str())
	malloc_ref := p.program.m.add_value(.func_ref, p.program.ptr_i8, 'malloc', p.program.fn_ids['malloc'])
	raw_context := p.program.m.add_instr(.call, p.cur_block, p.program.ptr_i8, [
		malloc_ref,
		context_size,
	])
	context := p.program.instr1(.bitcast, p.cur_block, p.program.m.type_store.get_ptr(context_type), raw_context)
	argument_start := if return_type == p.program.void_type { 0 } else { 1 }
	for i, argument in arguments {
		p.program.instr2(.store, p.cur_block, p.program.void_type, argument, p.program.struct_field_ptr(p.cur_block, context, context_type, argument_start + i))
	}
	handle_slot := p.program.instr0(.alloca, p.cur_block, p.program.m.type_store.get_ptr(p.program.u64_type))
	// pthread_attr_t is opaque to FastC. Reserve enough aligned storage for the
	// Darwin and Linux AArch64 layouts before configuring the promised 8 MiB stack.
	attributes_type := p.program.m.type_store.get_array(p.program.u64_type, 16)
	attributes_slot := p.program.instr0(.alloca, p.cur_block, p.program.m.type_store.get_ptr(attributes_type))
	attributes := p.program.instr1(.bitcast, p.cur_block, p.program.ptr_i8, attributes_slot)
	zero := p.program.m.get_or_add_const(p.program.i32_type, '0')
	one := p.program.m.get_or_add_const(p.program.i32_type, '1')
	free_ref := p.program.m.add_value(.func_ref, p.program.void_type, 'free', p.program.fn_ids['free'])
	exit_ref := p.program.m.add_value(.func_ref, p.program.void_type, 'exit', p.program.fn_ids['exit'])
	attr_init_ref := p.program.m.add_value(.func_ref, p.program.i32_type, 'pthread_attr_init', p.program.fn_ids['pthread_attr_init'])
	attr_init_status := p.program.m.add_instr(.call, p.cur_block, p.program.i32_type, [
		attr_init_ref,
		attributes,
	])
	attr_initialized := p.program.instr2(.eq, p.cur_block, p.program.i1_type, attr_init_status, zero)
	attr_ready := p.program.m.add_block(p.func_id, 'spawn_attr_ready')
	attr_init_failed := p.program.m.add_block(p.func_id, 'spawn_attr_init_failed')
	p.program.instr3(.br, p.cur_block, p.program.void_type, attr_initialized, ssa.ValueID(attr_ready), ssa.ValueID(attr_init_failed))
	p.mark_terminated(p.cur_block)
	p.program.m.add_instr(.call, attr_init_failed, p.program.void_type, [free_ref, raw_context])
	p.program.m.add_instr(.call, attr_init_failed, p.program.void_type, [exit_ref, one])
	p.program.instr0(.unreachable, attr_init_failed, p.program.void_type)
	p.mark_terminated(attr_init_failed)
	p.cur_block = attr_ready
	attr_stack_ref := p.program.m.add_value(.func_ref, p.program.i32_type, 'pthread_attr_setstacksize', p.program.fn_ids['pthread_attr_setstacksize'])
	stack_size := p.program.m.get_or_add_const(p.program.u64_type, (8 * 1024 * 1024).str())
	attr_stack_status := p.program.m.add_instr(.call, p.cur_block, p.program.i32_type, [
		attr_stack_ref,
		attributes,
		stack_size,
	])
	attr_configured := p.program.instr2(.eq, p.cur_block, p.program.i1_type, attr_stack_status, zero)
	create_thread := p.program.m.add_block(p.func_id, 'spawn_create')
	attr_config_failed := p.program.m.add_block(p.func_id, 'spawn_attr_config_failed')
	p.program.instr3(.br, p.cur_block, p.program.void_type, attr_configured, ssa.ValueID(create_thread), ssa.ValueID(attr_config_failed))
	p.mark_terminated(p.cur_block)
	attr_destroy_ref := p.program.m.add_value(.func_ref, p.program.i32_type, 'pthread_attr_destroy', p.program.fn_ids['pthread_attr_destroy'])
	p.program.m.add_instr(.call, attr_config_failed, p.program.i32_type, [
		attr_destroy_ref,
		attributes,
	])
	p.program.m.add_instr(.call, attr_config_failed, p.program.void_type, [free_ref, raw_context])
	p.program.m.add_instr(.call, attr_config_failed, p.program.void_type, [exit_ref, one])
	p.program.instr0(.unreachable, attr_config_failed, p.program.void_type)
	p.mark_terminated(attr_config_failed)
	p.cur_block = create_thread
	wrapper_ref := p.program.m.add_value(.func_ref, p.program.ptr_i8, p.program.m.funcs[wrapper_id].name, wrapper_id)
	create_ref := p.program.m.add_value(.func_ref, p.program.i32_type, 'pthread_create', p.program.fn_ids['pthread_create'])
	status := p.program.m.add_instr(.call, p.cur_block, p.program.i32_type, [
		create_ref,
		handle_slot,
		attributes,
		wrapper_ref,
		raw_context,
	])
	p.program.m.add_instr(.call, p.cur_block, p.program.i32_type, [attr_destroy_ref, attributes])
	created := p.program.instr2(.eq, p.cur_block, p.program.i1_type, status, zero)
	ready := p.program.m.add_block(p.func_id, 'spawn_ready')
	failed := p.program.m.add_block(p.func_id, 'spawn_failed')
	p.program.instr3(.br, p.cur_block, p.program.void_type, created, ssa.ValueID(ready), ssa.ValueID(failed))
	p.mark_terminated(p.cur_block)
	p.program.m.add_instr(.call, failed, p.program.void_type, [free_ref, raw_context])
	p.program.m.add_instr(.call, failed, p.program.void_type, [exit_ref, one])
	p.program.instr0(.unreachable, failed, p.program.void_type)
	p.mark_terminated(failed)
	p.cur_block = ready
	handle := p.program.instr1(.load, ready, p.program.u64_type, handle_slot)
	return FastArm64Value{
		id: handle
		typ: p.program.u64_type
		typ_name: 'thread ${return_type_name}'
		is_spawned: true
		spawn_handle: handle
		spawn_context: raw_context
		spawn_context_type: context_type
		spawn_result_type: return_type
		spawn_result_name: return_type_name
	}
}

fn (mut p FastArm64Parser) emit_spawn_wait(value FastArm64Value) FastArm64Value {
	null_pointer := p.program.m.get_or_add_const(p.program.ptr_i8, '0')
	join_ref := p.program.m.add_value(.func_ref, p.program.i32_type, 'pthread_join', p.program.fn_ids['pthread_join'])
	status := p.program.m.add_instr(.call, p.cur_block, p.program.i32_type, [join_ref,
		value.spawn_handle, null_pointer])
	zero := p.program.m.get_or_add_const(p.program.i32_type, '0')
	joined := p.program.instr2(.eq, p.cur_block, p.program.i1_type, status, zero)
	ready := p.program.m.add_block(p.func_id, 'thread_join_ready')
	failed := p.program.m.add_block(p.func_id, 'thread_join_failed')
	p.program.instr3(.br, p.cur_block, p.program.void_type, joined, ssa.ValueID(ready), ssa.ValueID(failed))
	p.mark_terminated(p.cur_block)
	exit_ref := p.program.m.add_value(.func_ref, p.program.void_type, 'exit', p.program.fn_ids['exit'])
	one := p.program.m.get_or_add_const(p.program.i32_type, '1')
	p.program.m.add_instr(.call, failed, p.program.void_type, [exit_ref, one])
	p.program.instr0(.unreachable, failed, p.program.void_type)
	p.mark_terminated(failed)
	p.cur_block = ready
	mut result_id := ssa.ValueID(0)
	if value.spawn_result_type != p.program.void_type {
		context := p.program.instr1(.bitcast, p.cur_block, p.program.m.type_store.get_ptr(value.spawn_context_type), value.spawn_context)
		result_address := p.program.struct_field_ptr(p.cur_block, context, value.spawn_context_type, 0)
		result_id = p.program.instr1(.load, p.cur_block, value.spawn_result_type, result_address)
	}
	free_ref := p.program.m.add_value(.func_ref, p.program.void_type, 'free', p.program.fn_ids['free'])
	p.program.m.add_instr(.call, p.cur_block, p.program.void_type, [free_ref, value.spawn_context])
	return FastArm64Value{
		id: result_id
		typ: value.spawn_result_type
		typ_name: value.spawn_result_name
	}
}

fn (mut p FastArm64Parser) parse_named_argument_struct(type_name string) !FastArm64Value {
	typ := p.program.type_id(type_name)
	layout := p.program.m.type_store.types[typ]
	if layout.kind != .struct_t {
		return p.unsupported('named arguments for `${type_name}`')
	}
	mut result := p.default_struct_value(type_name)!
	slot := result.address
	for p.tok != .rpar {
		if p.tok != .name && !p.tok.is_keyword() {
			return p.unsupported('named argument field')
		}
		field_name := p.lit
		p.next()
		p.expect(.colon)!
		mut field := -1
		for candidate_index, candidate_name in layout.field_names {
			if candidate_name == field_name {
				field = candidate_index
				break
			}
		}
		if field < 0 {
			return p.unsupported('named argument `${type_name}.${field_name}`')
		}
		field_type := layout.fields[field]
		mut field_value := p.parse_contextual_value(p.program.field_type_name(typ, field))!
		if field_value.typ != field_type {
			field_value = p.convert_value(field_value, field_type, p.program.field_type_name(typ, field))
		}
		address := p.program.struct_field_ptr(p.cur_block, slot, typ, field)
		p.program.instr2(.store, p.cur_block, p.program.void_type, field_value.id, address)
		if p.tok == .comma {
			p.next()
		} else if p.tok != .rpar {
			return p.unsupported('named argument separator')
		}
	}
	return FastArm64Value{
		...result
		id: p.program.instr1(.load, p.cur_block, typ, slot)
	}
}

fn (mut p FastArm64Parser) default_struct_value(type_name string) !FastArm64Value {
	typ := p.program.type_id(type_name)
	return p.default_struct_value_for_type(typ, type_name)
}

fn (mut p FastArm64Parser) default_struct_value_for_type(typ ssa.TypeID, type_name string) !FastArm64Value {
	mut result := p.zero_value(typ, type_name)
	declaration := p.program.type_decls_by_id[int(typ)] or {
		return result
	}
	for i, field in declaration.fields {
		field_type := p.program.m.type_store.types[typ].fields[i]
		if field.default_source == '' {
			if p.type_needs_default_initialization(field_type, 0) {
				value := p.default_value_for_type(field_type, field.typ)!
				p.program.instr2(.store, p.cur_block, p.program.void_type, value.id, p.program.struct_field_ptr(p.cur_block, result.address, typ, i))
			}
			continue
		}
		mut value := p.parse_struct_field_default(field)!
		if value.typ != field_type {
			value = p.convert_value(value, field_type, field.typ)
		}
		p.program.instr2(.store, p.cur_block, p.program.void_type, value.id, p.program.struct_field_ptr(p.cur_block, result.address, typ, i))
	}
	result = FastArm64Value{
		...result
		id: p.program.instr1(.load, p.cur_block, typ, result.address)
	}
	return result
}

fn (mut p FastArm64Parser) parse_struct_field_default(field FastArm64FieldDecl) !FastArm64Value {
	outer_scanner := p.s
	outer_tok := p.tok
	outer_lit := p.lit
	outer_source_file := p.source_file
	defer {
		p.s = outer_scanner
		p.tok = outer_tok
		p.lit = outer_lit
		p.source_file = outer_source_file
	}
	p.source_file = FastcSourceFile{
		path: field.default_path
		source: field.default_source
		header: field.default_header
	}
	p.enter_source(field.default_source)
	value := if p.tok == .dot {
		p.parse_enum_shorthand(field.typ)!
	} else {
		p.parse_contextual_value(field.typ)!
	}
	return value
}

fn (mut p FastArm64Parser) call_argument(argument FastArm64Value, expected_type_name string) ssa.ValueID {
	expected_type := p.program.type_id(expected_type_name)
	if expected_type == argument.typ {
		return argument.id
	}
	expected := p.program.m.type_store.types[expected_type]
	if expected.kind == .ptr_t && expected.elem_type == argument.typ {
		if argument.address != ssa.ValueID(0) {
			return argument.address
		}
		slot := p.program.instr0(.alloca, p.cur_block, expected_type)
		p.program.instr2(.store, p.cur_block, p.program.void_type, argument.id, slot)
		return slot
	}
	actual := p.program.m.type_store.types[argument.typ]
	if expected.kind in [.int_t, .float_t] && actual.kind in [.int_t, .float_t] {
		return p.convert_value(argument, expected_type, expected_type_name).id
	}
	return argument.id
}

fn fast_arm64_compound_opcode(op token.Token, is_unsigned bool) ssa.OpCode {
	return match op {
		.plus_assign { ssa.OpCode.add }
		.minus_assign { ssa.OpCode.sub }
		.mul_assign { ssa.OpCode.mul }
		.div_assign {
			if is_unsigned { ssa.OpCode.udiv } else { ssa.OpCode.sdiv }
		}
		.mod_assign {
			if is_unsigned { ssa.OpCode.urem } else { ssa.OpCode.srem }
		}
		.left_shift_assign { ssa.OpCode.shl }
		.right_shift_assign {
			if is_unsigned { ssa.OpCode.lshr } else { ssa.OpCode.ashr }
		}
		.right_shift_unsigned_assign { ssa.OpCode.lshr }
		.and_assign { ssa.OpCode.and_ }
		.or_assign { ssa.OpCode.or_ }
		else { ssa.OpCode.xor }
	}
}

fn (mut p FastArm64Parser) emit_value_equality(left FastArm64Value, right FastArm64Value) !FastArm64Value {
	if left.typ != right.typ {
		return p.unsupported('equality between `${left.typ_name}` and `${right.typ_name}`')
	}
	if left.typ == p.program.str_type {
		return p.emit_string_binary(.eq, left, right)
	}
	if left.typ == p.program.array_type {
		return p.emit_dynamic_array_equality(left, right)
	}
	if left.typ == p.program.map_type {
		return p.unsupported('map equality')
	}
	layout := p.program.m.type_store.types[left.typ]
	if layout.kind == .array_t {
		return p.emit_fixed_array_equality(left, right)
	}
	if layout.kind == .struct_t {
		return p.emit_struct_equality(left, right)
	}
	return FastArm64Value{
		id: p.program.instr2(.eq, p.cur_block, p.program.i1_type, left.id, right.id)
		typ: p.program.i1_type
		typ_name: 'bool'
	}
}

fn (mut p FastArm64Parser) emit_struct_equality(left FastArm64Value, right FastArm64Value) !FastArm64Value {
	layout := p.program.m.type_store.types[left.typ]
	left_slot := p.program.instr0(.alloca, p.cur_block, p.program.m.type_store.get_ptr(left.typ))
	right_slot := p.program.instr0(.alloca, p.cur_block, p.program.m.type_store.get_ptr(right.typ))
	p.program.instr2(.store, p.cur_block, p.program.void_type, left.id, left_slot)
	p.program.instr2(.store, p.cur_block, p.program.void_type, right.id, right_slot)
	mut result := p.program.m.get_or_add_const(p.program.i1_type, '1')
	for field, field_type in layout.fields {
		field_type_name := p.program.field_type_name(left.typ, field)
		left_field := p.program.instr1(.load, p.cur_block, field_type, p.program.struct_field_ptr(p.cur_block, left_slot, left.typ, field))
		right_field := p.program.instr1(.load, p.cur_block, field_type, p.program.struct_field_ptr(p.cur_block, right_slot, right.typ, field))
		field_equal := p.emit_value_equality(FastArm64Value{
			id: left_field
			typ: field_type
			typ_name: field_type_name
		}, FastArm64Value{
			id: right_field
			typ: field_type
			typ_name: field_type_name
		})!
		result = p.program.instr2(.and_, p.cur_block, p.program.i1_type, result, field_equal.id)
	}
	return FastArm64Value{
		id: result
		typ: p.program.i1_type
		typ_name: 'bool'
	}
}

fn (mut p FastArm64Parser) emit_fixed_array_equality(left FastArm64Value, right FastArm64Value) !FastArm64Value {
	layout := p.program.m.type_store.types[left.typ]
	element_type := layout.elem_type
	element_type_name := fastc_fixed_array_element_type(left.typ_name) or { '' }
	left_slot := p.program.instr0(.alloca, p.cur_block, p.program.m.type_store.get_ptr(left.typ))
	right_slot := p.program.instr0(.alloca, p.cur_block, p.program.m.type_store.get_ptr(right.typ))
	p.program.instr2(.store, p.cur_block, p.program.void_type, left.id, left_slot)
	p.program.instr2(.store, p.cur_block, p.program.void_type, right.id, right_slot)
	left_data := p.program.instr1(.bitcast, p.cur_block, p.program.ptr_i8, left_slot)
	right_data := p.program.instr1(.bitcast, p.cur_block, p.program.ptr_i8, right_slot)
	index_slot := p.program.instr0(.alloca, p.cur_block, p.program.m.type_store.get_ptr(p.program.i32_type))
	result_slot := p.program.instr0(.alloca, p.cur_block, p.program.m.type_store.get_ptr(p.program.i1_type))
	zero32 := p.program.m.get_or_add_const(p.program.i32_type, '0')
	true_value := p.program.m.get_or_add_const(p.program.i1_type, '1')
	false_value := p.program.m.get_or_add_const(p.program.i1_type, '0')
	p.program.instr2(.store, p.cur_block, p.program.void_type, zero32, index_slot)
	p.program.instr2(.store, p.cur_block, p.program.void_type, true_value, result_slot)
	condition := p.program.m.add_block(p.func_id, 'fixed_array_equality_condition')
	body := p.program.m.add_block(p.func_id, 'fixed_array_equality_body')
	unequal := p.program.m.add_block(p.func_id, 'fixed_array_equality_unequal')
	increment := p.program.m.add_block(p.func_id, 'fixed_array_equality_increment')
	done := p.program.m.add_block(p.func_id, 'fixed_array_equality_done')
	p.program.instr1(.jmp, p.cur_block, p.program.void_type, ssa.ValueID(condition))
	p.mark_terminated(p.cur_block)
	index := p.program.instr1(.load, condition, p.program.i32_type, index_slot)
	length := p.program.m.get_or_add_const(p.program.i32_type, layout.len.str())
	more := p.program.instr2(.lt, condition, p.program.i1_type, index, length)
	p.program.instr3(.br, condition, p.program.void_type, more, ssa.ValueID(body), ssa.ValueID(done))
	p.mark_terminated(condition)
	index64 := p.program.instr1(.zext, body, p.program.i64_type, index)
	element_size := p.program.m.get_or_add_const(p.program.i64_type, p.program.m.type_size(element_type).str())
	offset := p.program.instr2(.mul, body, p.program.i64_type, index64, element_size)
	left_address := p.program.instr2(.add, body, p.program.ptr_i8, left_data, offset)
	right_address := p.program.instr2(.add, body, p.program.ptr_i8, right_data, offset)
	left_pointer := p.program.instr1(.bitcast, body, p.program.m.type_store.get_ptr(element_type), left_address)
	right_pointer := p.program.instr1(.bitcast, body, p.program.m.type_store.get_ptr(element_type), right_address)
	left_element := p.program.instr1(.load, body, element_type, left_pointer)
	right_element := p.program.instr1(.load, body, element_type, right_pointer)
	p.cur_block = body
	element_equal := p.emit_value_equality(FastArm64Value{
		id: left_element
		typ: element_type
		typ_name: element_type_name
	}, FastArm64Value{
		id: right_element
		typ: element_type
		typ_name: element_type_name
	})!
	p.program.instr3(.br, p.cur_block, p.program.void_type, element_equal.id, ssa.ValueID(increment), ssa.ValueID(unequal))
	p.mark_terminated(p.cur_block)
	p.program.instr2(.store, unequal, p.program.void_type, false_value, result_slot)
	p.program.instr1(.jmp, unequal, p.program.void_type, ssa.ValueID(done))
	p.mark_terminated(unequal)
	one := p.program.m.get_or_add_const(p.program.i32_type, '1')
	next := p.program.instr2(.add, increment, p.program.i32_type, index, one)
	p.program.instr2(.store, increment, p.program.void_type, next, index_slot)
	p.program.instr1(.jmp, increment, p.program.void_type, ssa.ValueID(condition))
	p.mark_terminated(increment)
	p.cur_block = done
	return FastArm64Value{
		id: p.program.instr1(.load, done, p.program.i1_type, result_slot)
		typ: p.program.i1_type
		typ_name: 'bool'
	}
}

fn (mut p FastArm64Parser) emit_dynamic_array_equality(left FastArm64Value, right FastArm64Value) !FastArm64Value {
	element_type_name := p.program.array_element_type_name(left.typ_name) or {
		return p.unsupported('array equality type `${left.typ_name}`')
	}
	element_type := p.program.type_id(element_type_name)
	left_slot := p.program.instr0(.alloca, p.cur_block, p.program.m.type_store.get_ptr(p.program.array_type))
	right_slot := p.program.instr0(.alloca, p.cur_block, p.program.m.type_store.get_ptr(p.program.array_type))
	p.program.instr2(.store, p.cur_block, p.program.void_type, left.id, left_slot)
	p.program.instr2(.store, p.cur_block, p.program.void_type, right.id, right_slot)
	left_length := p.program.instr1(.load, p.cur_block, p.program.i32_type, p.program.struct_field_ptr(p.cur_block, left_slot, p.program.array_type, 2))
	right_length := p.program.instr1(.load, p.cur_block, p.program.i32_type, p.program.struct_field_ptr(p.cur_block, right_slot, p.program.array_type, 2))
	same_length := p.program.instr2(.eq, p.cur_block, p.program.i1_type, left_length, right_length)
	left_data := p.program.instr1(.load, p.cur_block, p.program.ptr_i8, p.program.struct_field_ptr(p.cur_block, left_slot, p.program.array_type, 0))
	right_data := p.program.instr1(.load, p.cur_block, p.program.ptr_i8, p.program.struct_field_ptr(p.cur_block, right_slot, p.program.array_type, 0))
	index_slot := p.program.instr0(.alloca, p.cur_block, p.program.m.type_store.get_ptr(p.program.i32_type))
	result_slot := p.program.instr0(.alloca, p.cur_block, p.program.m.type_store.get_ptr(p.program.i1_type))
	zero32 := p.program.m.get_or_add_const(p.program.i32_type, '0')
	false_value := p.program.m.get_or_add_const(p.program.i1_type, '0')
	p.program.instr2(.store, p.cur_block, p.program.void_type, zero32, index_slot)
	p.program.instr2(.store, p.cur_block, p.program.void_type, same_length, result_slot)
	condition := p.program.m.add_block(p.func_id, 'array_equality_condition')
	body := p.program.m.add_block(p.func_id, 'array_equality_body')
	unequal := p.program.m.add_block(p.func_id, 'array_equality_unequal')
	increment := p.program.m.add_block(p.func_id, 'array_equality_increment')
	done := p.program.m.add_block(p.func_id, 'array_equality_done')
	p.program.instr3(.br, p.cur_block, p.program.void_type, same_length, ssa.ValueID(condition), ssa.ValueID(done))
	p.mark_terminated(p.cur_block)
	index := p.program.instr1(.load, condition, p.program.i32_type, index_slot)
	more := p.program.instr2(.lt, condition, p.program.i1_type, index, left_length)
	p.program.instr3(.br, condition, p.program.void_type, more, ssa.ValueID(body), ssa.ValueID(done))
	p.mark_terminated(condition)
	index64 := p.program.instr1(.zext, body, p.program.i64_type, index)
	element_size := p.program.m.get_or_add_const(p.program.i64_type, p.program.m.type_size(element_type).str())
	offset := p.program.instr2(.mul, body, p.program.i64_type, index64, element_size)
	left_address := p.program.instr2(.add, body, p.program.ptr_i8, left_data, offset)
	right_address := p.program.instr2(.add, body, p.program.ptr_i8, right_data, offset)
	left_pointer := p.program.instr1(.bitcast, body, p.program.m.type_store.get_ptr(element_type), left_address)
	right_pointer := p.program.instr1(.bitcast, body, p.program.m.type_store.get_ptr(element_type), right_address)
	left_element := p.program.instr1(.load, body, element_type, left_pointer)
	right_element := p.program.instr1(.load, body, element_type, right_pointer)
	p.cur_block = body
	element_equal := p.emit_value_equality(FastArm64Value{
		id: left_element
		typ: element_type
		typ_name: element_type_name
	}, FastArm64Value{
		id: right_element
		typ: element_type
		typ_name: element_type_name
	})!
	p.program.instr3(.br, p.cur_block, p.program.void_type, element_equal.id, ssa.ValueID(increment), ssa.ValueID(unequal))
	p.mark_terminated(p.cur_block)
	p.program.instr2(.store, unequal, p.program.void_type, false_value, result_slot)
	p.program.instr1(.jmp, unequal, p.program.void_type, ssa.ValueID(done))
	p.mark_terminated(unequal)
	one := p.program.m.get_or_add_const(p.program.i32_type, '1')
	next := p.program.instr2(.add, increment, p.program.i32_type, index, one)
	p.program.instr2(.store, increment, p.program.void_type, next, index_slot)
	p.program.instr1(.jmp, increment, p.program.void_type, ssa.ValueID(condition))
	p.mark_terminated(increment)
	p.cur_block = done
	return FastArm64Value{
		id: p.program.instr1(.load, done, p.program.i1_type, result_slot)
		typ: p.program.i1_type
		typ_name: 'bool'
	}
}

fn (mut p FastArm64Parser) emit_binary(op token.Token, left FastArm64Value, right FastArm64Value) !FastArm64Value {
	if left.is_none && right.is_none {
		if op !in [.eq, .ne] {
			return p.unsupported('operator `${op.str()}` between two `none` values')
		}
		result := if op == .eq { '1' } else { '0' }
		return FastArm64Value{
			id: p.program.m.get_or_add_const(p.program.i1_type, result)
			typ: p.program.i1_type
			typ_name: 'bool'
		}
	}
	if left.is_none || right.is_none {
		if op !in [.eq, .ne] {
			return p.unsupported('operator `${op.str()}` with `none`')
		}
		value := if left.is_none { right } else { left }
		failed := p.value_option_failure(value)
		if failed == ssa.ValueID(0) {
			result := if op == .eq { '0' } else { '1' }
			return FastArm64Value{
				id: p.program.m.get_or_add_const(p.program.i1_type, result)
				typ: p.program.i1_type
				typ_name: 'bool'
			}
		}
		mut result := failed
		if op != .eq {
			zero := p.program.m.get_or_add_const(p.program.i1_type, '0')
			result = p.program.instr2(.eq, p.cur_block, p.program.i1_type, failed, zero)
		}
		return FastArm64Value{
			id: result
			typ: p.program.i1_type
			typ_name: 'bool'
		}
	}
	if op == .left_shift && left.typ == p.program.array_type {
		return p.emit_array_push(left, right, false)
	}
	if op in [.key_in, .not_in] && right.typ == p.program.map_type {
		key_type_name, _ := fastc_map_key_value_types(right.typ_name) or {
			return p.unsupported('map type `${right.typ_name}`')
		}
		key_type := p.program.type_id(key_type_name)
		mut key := left
		if key.typ != key_type {
			key = p.convert_value(key, key_type, key_type_name)
		}
		key_slot := p.program.instr0(.alloca, p.cur_block, p.program.m.type_store.get_ptr(key.typ))
		p.program.instr2(.store, p.cur_block, p.program.void_type, key.id, key_slot)
		key_pointer := p.program.instr1(.bitcast, p.cur_block, p.program.ptr_i8, key_slot)
		find_ref := p.program.m.add_value(.func_ref, p.program.i64_type, 'fast_map_find', p.program.fn_ids['fast_map_find'])
		index := p.program.m.add_instr(.call, p.cur_block, p.program.i64_type, [
			find_ref,
			right.id,
			key_pointer,
		])
		zero := p.program.m.get_or_add_const(p.program.i64_type, '0')
		result := if op == .key_in {
			p.program.instr2(.ge, p.cur_block, p.program.i1_type, index, zero)
		} else {
			p.program.instr2(.lt, p.cur_block, p.program.i1_type, index, zero)
		}
		return FastArm64Value{
			id: result
			typ: p.program.i1_type
			typ_name: 'bool'
		}
	}
	if op in [.key_in, .not_in] && right.typ == p.program.array_type {
		return p.emit_array_membership(op, left, right)
	}
	if left.typ == p.program.str_type || right.typ == p.program.str_type {
		if left.typ != p.program.str_type || right.typ != p.program.str_type {
			function_name := p.program.m.funcs[p.func_id].name
			return p.unsupported('string/scalar binary operator `${op.str()}` between `${left.typ_name}` and `${right.typ_name}` in `${function_name}`')
		}
		return p.emit_string_binary(op, left, right)
	}
	if op in [.eq, .ne] && left.typ == right.typ && left.typ != p.program.map_type {
		layout := p.program.m.type_store.types[left.typ]
		if left.typ == p.program.array_type || layout.kind in [.array_t, .struct_t] {
			equal := p.emit_value_equality(left, right)!
			result := if op == .ne {
				p.program.instr2(.eq, p.cur_block, p.program.i1_type, equal.id, p.program.m.get_or_add_const(p.program.i1_type, '0'))
			} else {
				equal.id
			}
			return FastArm64Value{
				id: result
				typ: p.program.i1_type
				typ_name: 'bool'
			}
		}
	}
	mut converted_left := left
	mut converted_right := right
	left_layout := p.program.m.type_store.types[left.typ]
	right_layout := p.program.m.type_store.types[right.typ]
	if left_layout.kind == .ptr_t && right_layout.kind == .int_t && op in [.plus, .minus] {
		mut offset := p.integer_to_i64(right)
		element_size := p.program.m.type_size(left_layout.elem_type)
		if element_size > 1 {
			scale := p.program.m.get_or_add_const(p.program.i64_type, element_size.str())
			offset = p.program.instr2(.mul, p.cur_block, p.program.i64_type, offset, scale)
		}
		pointer_op := if op == .plus { ssa.OpCode.add } else { ssa.OpCode.sub }
		return FastArm64Value{
			id: p.program.instr2(pointer_op, p.cur_block, left.typ, left.id, offset)
			typ: left.typ
			typ_name: left.typ_name
		}
	}
	if left.typ != right.typ && left_layout.kind in [.int_t, .float_t] && right_layout.kind in [
		.int_t,
		.float_t,
	] {
		left_is_constant := left.id > ssa.ValueID(0) && int(left.id) < p.program.m.values.len && p.program.m.values[left.id].kind == .constant
		right_is_constant := right.id > ssa.ValueID(0) && int(right.id) < p.program.m.values.len && p.program.m.values[right.id].kind == .constant
		if left_is_constant && !right_is_constant && op !in [.left_shift, .right_shift,
			.right_shift_unsigned] {
			converted_left = p.convert_value(left, right.typ, right.typ_name)
		} else {
			converted_right = p.convert_value(right, left.typ, left.typ_name)
		}
	}
	result_type := if op in [.eq, .ne, .lt, .le, .gt, .ge, .and, .logical_or] {
		p.program.i1_type
	} else {
		converted_left.typ
	}
	is_unsigned := p.program.m.type_store.types[converted_left.typ].is_unsigned
	opcode := match op {
		.plus { ssa.OpCode.add }
		.minus { ssa.OpCode.sub }
		.mul { ssa.OpCode.mul }
		.div {
			if is_unsigned { ssa.OpCode.udiv } else { ssa.OpCode.sdiv }
		}
		.mod {
			if is_unsigned { ssa.OpCode.urem } else { ssa.OpCode.srem }
		}
		.amp, .and { ssa.OpCode.and_ }
		.pipe, .logical_or { ssa.OpCode.or_ }
		.xor { ssa.OpCode.xor }
		.left_shift { ssa.OpCode.shl }
		.right_shift {
			if is_unsigned { ssa.OpCode.lshr } else { ssa.OpCode.ashr }
		}
		.right_shift_unsigned { ssa.OpCode.lshr }
		.eq { ssa.OpCode.eq }
		.ne { ssa.OpCode.ne }
		.lt {
			if is_unsigned { ssa.OpCode.ult } else { ssa.OpCode.lt }
		}
		.le {
			if is_unsigned { ssa.OpCode.ule } else { ssa.OpCode.le }
		}
		.gt {
			if is_unsigned { ssa.OpCode.ugt } else { ssa.OpCode.gt }
		}
		.ge {
			if is_unsigned { ssa.OpCode.uge } else { ssa.OpCode.ge }
		}
		else {
			return p.unsupported('binary operator `${op.str()}`')
		}
	}
	return FastArm64Value{
		id: p.program.instr2(opcode, p.cur_block, result_type, converted_left.id, converted_right.id)
		typ: result_type
		typ_name: if result_type == p.program.i1_type { 'bool' } else { converted_left.typ_name }
	}
}

fn (mut p FastArm64Parser) emit_array_push(array FastArm64Value, item FastArm64Value, prepend bool) !FastArm64Value {
	return p.emit_array_push_at(array, item, prepend, FastArm64Value{})
}

fn (mut p FastArm64Parser) emit_array_insert(array FastArm64Value, item FastArm64Value, index FastArm64Value) !FastArm64Value {
	return p.emit_array_push_at(array, item, false, index)
}

fn (mut p FastArm64Parser) emit_array_push_at(array FastArm64Value, item FastArm64Value, prepend bool, requested_index FastArm64Value) !FastArm64Value {
	element_type_name := p.program.array_element_type_name(array.typ_name) or {
		return p.unsupported('array append type `${array.typ_name}`')
	}
	if item.typ == p.program.array_type && p.program.resolved_type_name(item.typ_name) == p.program.resolved_type_name(array.typ_name) {
		return p.emit_array_append_many(array, item, element_type_name, prepend, requested_index)
	}
	element_type := p.program.type_id(element_type_name)
	mut inserted := item
	if inserted.typ != element_type {
		inserted = p.convert_value(inserted, element_type, element_type_name)
	}
	array_slot := p.mutable_array_slot(array)
	len_ptr := p.program.struct_field_ptr(p.cur_block, array_slot, p.program.array_type, 2)
	cap_ptr := p.program.struct_field_ptr(p.cur_block, array_slot, p.program.array_type, 3)
	length := p.program.instr1(.load, p.cur_block, p.program.i32_type, len_ptr)
	capacity := p.program.instr1(.load, p.cur_block, p.program.i32_type, cap_ptr)
	zero32 := p.program.m.get_or_add_const(p.program.i32_type, '0')
	mut insertion_index := if prepend { zero32 } else { length }
	has_requested_index := requested_index.typ != ssa.TypeID(0)
	if has_requested_index {
		mut normalized_index := requested_index
		if normalized_index.typ != p.program.i32_type {
			normalized_index = p.convert_value(normalized_index, p.program.i32_type, 'int')
		}
		insertion_index = normalized_index.id
		below_start := p.program.instr2(.lt, p.cur_block, p.program.i1_type, insertion_index, zero32)
		past_end := p.program.instr2(.gt, p.cur_block, p.program.i1_type, insertion_index, length)
		invalid := p.program.instr2(.or_, p.cur_block, p.program.i1_type, below_start, past_end)
		invalid_block := p.program.m.add_block(p.func_id, 'array_insert_invalid')
		valid_block := p.program.m.add_block(p.func_id, 'array_insert_valid')
		p.program.instr3(.br, p.cur_block, p.program.void_type, invalid, ssa.ValueID(invalid_block), ssa.ValueID(valid_block))
		p.mark_terminated(p.cur_block)
		exit_ref := p.program.m.add_value(.func_ref, p.program.void_type, 'exit', p.program.fn_ids['exit'])
		exit_code := p.program.m.get_or_add_const(p.program.i32_type, '1')
		p.program.m.add_instr(.call, invalid_block, p.program.void_type, [exit_ref, exit_code])
		p.program.instr0(.unreachable, invalid_block, p.program.void_type)
		p.mark_terminated(invalid_block)
		p.cur_block = valid_block
	}
	full := p.program.instr2(.ge, p.cur_block, p.program.i1_type, length, capacity)
	array_offset := p.program.instr1(.load, p.cur_block, p.program.i32_type, p.program.struct_field_ptr(p.cur_block, array_slot, p.program.array_type, 1))
	flags := p.program.instr1(.load, p.cur_block, p.program.i32_type, p.program.struct_field_ptr(p.cur_block, array_slot, p.program.array_type, 4))
	has_offset := p.program.instr2(.ne, p.cur_block, p.program.i1_type, array_offset, zero32)
	is_slice_flag := p.program.m.get_or_add_const(p.program.i32_type, '64')
	masked_flags := p.program.instr2(.and_, p.cur_block, p.program.i32_type, flags, is_slice_flag)
	has_slice_flag := p.program.instr2(.ne, p.cur_block, p.program.i1_type, masked_flags, zero32)
	is_slice := p.program.instr2(.or_, p.cur_block, p.program.i1_type, has_offset, has_slice_flag)
	mut needs_storage := p.program.instr2(.or_, p.cur_block, p.program.i1_type, full, is_slice)
	if prepend || has_requested_index {
		buffer_has_slices := p.emit_array_buffer_has_slices(array_slot)
		needs_storage = p.program.instr2(.or_, p.cur_block, p.program.i1_type, needs_storage, buffer_has_slices)
	}
	grow := p.program.m.add_block(p.func_id, 'array_push_grow')
	append := p.program.m.add_block(p.func_id, 'array_push_append')
	done := p.program.m.add_block(p.func_id, 'array_push_done')
	p.program.instr3(.br, p.cur_block, p.program.void_type, needs_storage, ssa.ValueID(grow), ssa.ValueID(append))
	p.mark_terminated(p.cur_block)
	one32 := p.program.m.get_or_add_const(p.program.i32_type, '1')
	two32 := p.program.m.get_or_add_const(p.program.i32_type, '2')
	doubled := p.program.instr2(.mul, grow, p.program.i32_type, capacity, two32)
	was_empty := p.program.instr2(.eq, grow, p.program.i1_type, capacity, zero32)
	grown_capacity := p.program.integer_select(grow, was_empty, one32, doubled, p.program.i32_type)
	new_capacity := p.program.integer_select(grow, full, grown_capacity, capacity, p.program.i32_type)
	element_size := p.program.m.get_or_add_const(p.program.i64_type, p.program.m.type_size(element_type).str())
	element_size32 := p.program.m.get_or_add_const(p.program.i32_type, p.program.m.type_size(element_type).str())
	p.emit_array_grow_storage(array_slot, grow, append, length, new_capacity, element_size, element_size32)
	append_data := p.program.instr1(.load, append, p.program.ptr_i8, p.program.struct_field_ptr(append, array_slot, p.program.array_type, 0))
	append_length := p.program.instr1(.load, append, p.program.i32_type, p.program.struct_field_ptr(append, array_slot, p.program.array_type, 2))
	append_length64 := p.program.instr1(.zext, append, p.program.i64_type, append_length)
	insertion_index64 := p.program.instr1(.zext, append, p.program.i64_type, insertion_index)
	if prepend || has_requested_index {
		tail_length := p.program.instr2(.sub, append, p.program.i32_type, append_length, insertion_index)
		tail_length64 := p.program.instr1(.zext, append, p.program.i64_type, tail_length)
		bytes := p.program.instr2(.mul, append, p.program.i64_type, tail_length64, element_size)
		source_offset := p.program.instr2(.mul, append, p.program.i64_type, insertion_index64, element_size)
		source := p.program.instr2(.add, append, p.program.ptr_i8, append_data, source_offset)
		destination := p.program.instr2(.add, append, p.program.ptr_i8, source, element_size)
		memmove_ref := p.program.m.add_value(.func_ref, p.program.ptr_i8, 'memmove', p.program.fn_ids['memmove'])
		p.program.m.add_instr(.call, append, p.program.ptr_i8, [memmove_ref, destination, source,
			bytes])
	}
	offset := if prepend || has_requested_index {
		p.program.instr2(.mul, append, p.program.i64_type, insertion_index64, element_size)
	} else {
		p.program.instr2(.mul, append, p.program.i64_type, append_length64, element_size)
	}
	destination := p.program.instr2(.add, append, p.program.ptr_i8, append_data, offset)
	typed_destination := p.program.instr1(.bitcast, append, p.program.m.type_store.get_ptr(element_type), destination)
	p.program.instr2(.store, append, p.program.void_type, inserted.id, typed_destination)
	new_length := p.program.instr2(.add, append, p.program.i32_type, append_length, one32)
	p.program.instr2(.store, append, p.program.void_type, new_length, p.program.struct_field_ptr(append, array_slot, p.program.array_type, 2))
	p.program.instr1(.jmp, append, p.program.void_type, ssa.ValueID(done))
	p.mark_terminated(append)
	p.cur_block = done
	return FastArm64Value{
		id: p.program.instr1(.load, done, p.program.array_type, array_slot)
		typ: p.program.array_type
		typ_name: array.typ_name
		address: array_slot
	}
}

fn (mut p FastArm64Parser) emit_array_grow_storage(array_slot ssa.ValueID, grow ssa.BlockID, resume ssa.BlockID, length ssa.ValueID, new_capacity ssa.ValueID, element_size ssa.ValueID, element_size32 ssa.ValueID) {
	new_capacity64 := p.program.instr1(.zext, grow, p.program.i64_type, new_capacity)
	allocation_size := p.program.instr2(.mul, grow, p.program.i64_type, new_capacity64, element_size)
	data_ptr := p.program.struct_field_ptr(grow, array_slot, p.program.array_type, 0)
	offset_ptr := p.program.struct_field_ptr(grow, array_slot, p.program.array_type, 1)
	flags_ptr := p.program.struct_field_ptr(grow, array_slot, p.program.array_type, 4)
	old_data := p.program.instr1(.load, grow, p.program.ptr_i8, data_ptr)
	offset := p.program.instr1(.load, grow, p.program.i32_type, offset_ptr)
	flags := p.program.instr1(.load, grow, p.program.i32_type, flags_ptr)
	zero32 := p.program.m.get_or_add_const(p.program.i32_type, '0')
	has_offset := p.program.instr2(.ne, grow, p.program.i1_type, offset, zero32)
	is_slice_flag := p.program.m.get_or_add_const(p.program.i32_type, '64')
	masked_flags := p.program.instr2(.and_, grow, p.program.i32_type, flags, is_slice_flag)
	has_slice_flag := p.program.instr2(.ne, grow, p.program.i1_type, masked_flags, zero32)
	descriptor_is_slice := p.program.instr2(.or_, grow, p.program.i1_type, has_offset, has_slice_flag)
	buffer_has_slices := p.emit_array_buffer_has_slices_at(array_slot, grow)
	is_slice := p.program.instr2(.or_, grow, p.program.i1_type, descriptor_is_slice, buffer_has_slices)
	detach := p.program.m.add_block(p.func_id, 'array_grow_detach_slice')
	reallocate := p.program.m.add_block(p.func_id, 'array_grow_reallocate')
	managed_reallocate := p.program.m.add_block(p.func_id, 'array_grow_reallocate_managed')
	unmanaged_reallocate := p.program.m.add_block(p.func_id, 'array_grow_reallocate_unmanaged')
	unmanaged_existing := p.program.m.add_block(p.func_id, 'array_grow_reallocate_unmanaged_existing')
	empty_allocate := p.program.m.add_block(p.func_id, 'array_grow_allocate_empty')
	finish := p.program.m.add_block(p.func_id, 'array_grow_finish')
	p.program.instr3(.br, grow, p.program.void_type, is_slice, ssa.ValueID(detach), ssa.ValueID(reallocate))
	p.mark_terminated(grow)
	new_ref := p.program.m.add_value(.func_ref, p.program.array_type, 'fast_array_new', p.program.fn_ids['fast_array_new'])
	length64 := p.program.instr1(.zext, detach, p.program.i64_type, length)
	detached_array := p.program.m.add_instr(.call, detach, p.program.array_type, [
		new_ref,
		element_size,
		length64,
		new_capacity64,
	])
	detached_slot := p.program.instr0(.alloca, detach, p.program.m.type_store.get_ptr(p.program.array_type))
	p.program.instr2(.store, detach, p.program.void_type, detached_array, detached_slot)
	detached_data := p.program.instr1(.load, detach, p.program.ptr_i8, p.program.struct_field_ptr(detach, detached_slot, p.program.array_type, 0))
	copy_size := p.program.instr2(.mul, detach, p.program.i64_type, length64, element_size)
	memcpy_ref := p.program.m.add_value(.func_ref, p.program.ptr_i8, 'memcpy', p.program.fn_ids['memcpy'])
	p.program.m.add_instr(.call, detach, p.program.ptr_i8, [memcpy_ref, detached_data, old_data,
		copy_size])
	p.program.instr2(.store, detach, p.program.void_type, detached_array, array_slot)
	p.program.instr1(.jmp, detach, p.program.void_type, ssa.ValueID(finish))
	p.mark_terminated(detach)
	is_managed := p.program.array_is_managed(reallocate, array_slot)
	p.program.instr3(.br, reallocate, p.program.void_type, is_managed, ssa.ValueID(managed_reallocate), ssa.ValueID(unmanaged_reallocate))
	p.mark_terminated(reallocate)
	realloc_ref := p.program.m.add_value(.func_ref, p.program.ptr_i8, 'realloc', p.program.fn_ids['realloc'])
	header_size := p.program.m.get_or_add_const(p.program.i64_type, '8')
	old_data_address := p.program.instr1(.bitcast, managed_reallocate, p.program.u64_type, old_data)
	raw_address := p.program.instr2(.sub, managed_reallocate, p.program.u64_type, old_data_address, header_size)
	raw_data := p.program.instr1(.bitcast, managed_reallocate, p.program.ptr_i8, raw_address)
	managed_allocation_size := p.program.instr2(.add, managed_reallocate, p.program.i64_type, allocation_size, header_size)
	reallocated_raw := p.program.m.add_instr(.call, managed_reallocate, p.program.ptr_i8, [
		realloc_ref,
		raw_data,
		managed_allocation_size,
	])
	reallocated_data := p.program.instr2(.add, managed_reallocate, p.program.ptr_i8, reallocated_raw, header_size)
	p.program.instr2(.store, managed_reallocate, p.program.void_type, reallocated_data, data_ptr)
	p.program.instr1(.jmp, managed_reallocate, p.program.void_type, ssa.ValueID(finish))
	p.mark_terminated(managed_reallocate)
	null_pointer := p.program.m.get_or_add_const(p.program.ptr_i8, '0')
	has_old_data := p.program.instr2(.ne, unmanaged_reallocate, p.program.i1_type, old_data, null_pointer)
	p.program.instr3(.br, unmanaged_reallocate, p.program.void_type, has_old_data, ssa.ValueID(unmanaged_existing), ssa.ValueID(empty_allocate))
	p.mark_terminated(unmanaged_reallocate)
	unmanaged_data := p.program.m.add_instr(.call, unmanaged_existing, p.program.ptr_i8, [
		realloc_ref,
		old_data,
		allocation_size,
	])
	p.program.instr2(.store, unmanaged_existing, p.program.void_type, unmanaged_data, data_ptr)
	p.program.instr1(.jmp, unmanaged_existing, p.program.void_type, ssa.ValueID(finish))
	p.mark_terminated(unmanaged_existing)
	empty_length := p.program.instr1(.zext, empty_allocate, p.program.i64_type, length)
	empty_array := p.program.m.add_instr(.call, empty_allocate, p.program.array_type, [
		new_ref,
		element_size,
		empty_length,
		new_capacity64,
	])
	p.program.instr2(.store, empty_allocate, p.program.void_type, empty_array, array_slot)
	p.program.instr1(.jmp, empty_allocate, p.program.void_type, ssa.ValueID(finish))
	p.mark_terminated(empty_allocate)
	p.program.instr2(.store, finish, p.program.void_type, new_capacity, p.program.struct_field_ptr(finish, array_slot, p.program.array_type, 3))
	p.program.instr2(.store, finish, p.program.void_type, element_size32, p.program.struct_field_ptr(finish, array_slot, p.program.array_type, 5))
	p.program.instr1(.jmp, finish, p.program.void_type, ssa.ValueID(resume))
	p.mark_terminated(finish)
}

fn fast_arm64_pointer_element_type_name(type_name string) string {
	if type_name.starts_with('&') {
		return type_name[1..]
	}
	if type_name.ends_with('*') {
		return type_name[..type_name.len - 1]
	}
	if type_name in ['voidptr', 'byteptr', 'charptr'] {
		return 'u8'
	}
	return type_name
}

fn (mut p FastArm64Parser) emit_array_append_many(array FastArm64Value, items FastArm64Value, element_type_name string, prepend bool, requested_index FastArm64Value) !FastArm64Value {
	element_type := p.program.type_id(element_type_name)
	element_size := p.program.m.get_or_add_const(p.program.i64_type, p.program.m.type_size(element_type).str())
	element_size32 := p.program.m.get_or_add_const(p.program.i32_type, p.program.m.type_size(element_type).str())
	array_slot := p.mutable_array_slot(array)
	items_slot := p.program.instr0(.alloca, p.cur_block, p.program.m.type_store.get_ptr(p.program.array_type))
	p.program.instr2(.store, p.cur_block, p.program.void_type, items.id, items_slot)
	length_ptr := p.program.struct_field_ptr(p.cur_block, array_slot, p.program.array_type, 2)
	capacity_ptr := p.program.struct_field_ptr(p.cur_block, array_slot, p.program.array_type, 3)
	length := p.program.instr1(.load, p.cur_block, p.program.i32_type, length_ptr)
	capacity := p.program.instr1(.load, p.cur_block, p.program.i32_type, capacity_ptr)
	zero32 := p.program.m.get_or_add_const(p.program.i32_type, '0')
	mut insertion_index := if prepend { zero32 } else { length }
	has_requested_index := requested_index.typ != ssa.TypeID(0)
	if has_requested_index {
		mut normalized_index := requested_index
		if normalized_index.typ != p.program.i32_type {
			normalized_index = p.convert_value(normalized_index, p.program.i32_type, 'int')
		}
		insertion_index = normalized_index.id
		below_start := p.program.instr2(.lt, p.cur_block, p.program.i1_type, insertion_index, zero32)
		past_end := p.program.instr2(.gt, p.cur_block, p.program.i1_type, insertion_index, length)
		invalid := p.program.instr2(.or_, p.cur_block, p.program.i1_type, below_start, past_end)
		invalid_block := p.program.m.add_block(p.func_id, 'array_insert_many_invalid')
		valid_block := p.program.m.add_block(p.func_id, 'array_insert_many_valid')
		p.program.instr3(.br, p.cur_block, p.program.void_type, invalid, ssa.ValueID(invalid_block), ssa.ValueID(valid_block))
		p.mark_terminated(p.cur_block)
		exit_ref := p.program.m.add_value(.func_ref, p.program.void_type, 'exit', p.program.fn_ids['exit'])
		exit_code := p.program.m.get_or_add_const(p.program.i32_type, '1')
		p.program.m.add_instr(.call, invalid_block, p.program.void_type, [exit_ref, exit_code])
		p.program.instr0(.unreachable, invalid_block, p.program.void_type)
		p.mark_terminated(invalid_block)
		p.cur_block = valid_block
	}
	items_length := p.program.instr1(.load, p.cur_block, p.program.i32_type, p.program.struct_field_ptr(p.cur_block, items_slot, p.program.array_type, 2))
	has_items := p.program.instr2(.gt, p.cur_block, p.program.i1_type, items_length, zero32)
	append_items := p.program.m.add_block(p.func_id, 'array_append_many_items')
	done := p.program.m.add_block(p.func_id, 'array_append_many_done')
	p.program.instr3(.br, p.cur_block, p.program.void_type, has_items, ssa.ValueID(append_items), ssa.ValueID(done))
	p.mark_terminated(p.cur_block)
	p.cur_block = append_items
	destination_data_before := p.program.instr1(.load, p.cur_block, p.program.ptr_i8, p.program.struct_field_ptr(p.cur_block, array_slot, p.program.array_type, 0))
	source_data_before := p.program.instr1(.load, p.cur_block, p.program.ptr_i8, p.program.struct_field_ptr(p.cur_block, items_slot, p.program.array_type, 0))
	items_length64 := p.program.instr1(.zext, p.cur_block, p.program.i64_type, items_length)
	items_bytes := p.program.instr2(.mul, p.cur_block, p.program.i64_type, items_length64, element_size)
	capacity64 := p.program.instr1(.zext, p.cur_block, p.program.i64_type, capacity)
	capacity_bytes := p.program.instr2(.mul, p.cur_block, p.program.i64_type, capacity64, element_size)
	destination_address := p.program.instr1(.bitcast, p.cur_block, p.program.u64_type, destination_data_before)
	source_address := p.program.instr1(.bitcast, p.cur_block, p.program.u64_type, source_data_before)
	destination_end := p.program.instr2(.add, p.cur_block, p.program.u64_type, destination_address, capacity_bytes)
	source_at_or_after_start := p.program.instr2(.uge, p.cur_block, p.program.i1_type, source_address, destination_address)
	source_before_end := p.program.instr2(.ult, p.cur_block, p.program.i1_type, source_address, destination_end)
	aliases_destination := p.program.instr2(.and_, p.cur_block, p.program.i1_type, source_at_or_after_start, source_before_end)
	source_slot := p.program.instr0(.alloca, p.cur_block, p.program.m.type_store.get_ptr(p.program.ptr_i8))
	alias_block := p.program.m.add_block(p.func_id, 'array_append_many_snapshot_alias')
	nonalias_block := p.program.m.add_block(p.func_id, 'array_append_many_keep_source')
	source_ready := p.program.m.add_block(p.func_id, 'array_append_many_source_ready')
	p.program.instr3(.br, p.cur_block, p.program.void_type, aliases_destination, ssa.ValueID(alias_block), ssa.ValueID(nonalias_block))
	p.mark_terminated(p.cur_block)
	malloc_ref := p.program.m.add_value(.func_ref, p.program.ptr_i8, 'malloc', p.program.fn_ids['malloc'])
	snapshot := p.program.m.add_instr(.call, alias_block, p.program.ptr_i8, [
		malloc_ref,
		items_bytes,
	])
	memcpy_ref := p.program.m.add_value(.func_ref, p.program.ptr_i8, 'memcpy', p.program.fn_ids['memcpy'])
	p.program.m.add_instr(.call, alias_block, p.program.ptr_i8, [memcpy_ref, snapshot,
		source_data_before, items_bytes])
	p.program.instr2(.store, alias_block, p.program.void_type, snapshot, source_slot)
	p.program.instr1(.jmp, alias_block, p.program.void_type, ssa.ValueID(source_ready))
	p.mark_terminated(alias_block)
	p.program.instr2(.store, nonalias_block, p.program.void_type, source_data_before, source_slot)
	p.program.instr1(.jmp, nonalias_block, p.program.void_type, ssa.ValueID(source_ready))
	p.mark_terminated(nonalias_block)
	p.cur_block = source_ready
	required := p.program.instr2(.add, p.cur_block, p.program.i32_type, length, items_length)
	needs_grow := p.program.instr2(.gt, p.cur_block, p.program.i1_type, required, capacity)
	offset := p.program.instr1(.load, p.cur_block, p.program.i32_type, p.program.struct_field_ptr(p.cur_block, array_slot, p.program.array_type, 1))
	flags := p.program.instr1(.load, p.cur_block, p.program.i32_type, p.program.struct_field_ptr(p.cur_block, array_slot, p.program.array_type, 4))
	has_offset := p.program.instr2(.ne, p.cur_block, p.program.i1_type, offset, zero32)
	is_slice_flag := p.program.m.get_or_add_const(p.program.i32_type, '64')
	masked_flags := p.program.instr2(.and_, p.cur_block, p.program.i32_type, flags, is_slice_flag)
	has_slice_flag := p.program.instr2(.ne, p.cur_block, p.program.i1_type, masked_flags, zero32)
	is_slice := p.program.instr2(.or_, p.cur_block, p.program.i1_type, has_offset, has_slice_flag)
	mut needs_storage := p.program.instr2(.or_, p.cur_block, p.program.i1_type, needs_grow, is_slice)
	if prepend || has_requested_index {
		buffer_has_slices := p.emit_array_buffer_has_slices(array_slot)
		needs_storage = p.program.instr2(.or_, p.cur_block, p.program.i1_type, needs_storage, buffer_has_slices)
	}
	grow := p.program.m.add_block(p.func_id, 'array_append_many_grow')
	copy_block := p.program.m.add_block(p.func_id, 'array_append_many_copy')
	free_snapshot := p.program.m.add_block(p.func_id, 'array_append_many_free_snapshot')
	p.program.instr3(.br, p.cur_block, p.program.void_type, needs_storage, ssa.ValueID(grow), ssa.ValueID(copy_block))
	p.mark_terminated(p.cur_block)
	new_capacity := p.program.integer_select(grow, needs_grow, required, capacity, p.program.i32_type)
	p.emit_array_grow_storage(array_slot, grow, copy_block, length, new_capacity, element_size, element_size32)
	destination_data := p.program.instr1(.load, copy_block, p.program.ptr_i8, p.program.struct_field_ptr(copy_block, array_slot, p.program.array_type, 0))
	source_data := p.program.instr1(.load, copy_block, p.program.ptr_i8, source_slot)
	insertion_index64 := p.program.instr1(.zext, copy_block, p.program.i64_type, insertion_index)
	if prepend || has_requested_index {
		tail_length := p.program.instr2(.sub, copy_block, p.program.i32_type, length, insertion_index)
		tail_length64 := p.program.instr1(.zext, copy_block, p.program.i64_type, tail_length)
		tail_bytes := p.program.instr2(.mul, copy_block, p.program.i64_type, tail_length64, element_size)
		insertion_offset := p.program.instr2(.mul, copy_block, p.program.i64_type, insertion_index64, element_size)
		tail_source := p.program.instr2(.add, copy_block, p.program.ptr_i8, destination_data, insertion_offset)
		shifted_destination := p.program.instr2(.add, copy_block, p.program.ptr_i8, tail_source, items_bytes)
		memmove_ref := p.program.m.add_value(.func_ref, p.program.ptr_i8, 'memmove', p.program.fn_ids['memmove'])
		p.program.m.add_instr(.call, copy_block, p.program.ptr_i8, [memmove_ref, shifted_destination,
			tail_source, tail_bytes])
	}
	insertion_offset := p.program.instr2(.mul, copy_block, p.program.i64_type, insertion_index64, element_size)
	destination := p.program.instr2(.add, copy_block, p.program.ptr_i8, destination_data, insertion_offset)
	p.program.m.add_instr(.call, copy_block, p.program.ptr_i8, [memcpy_ref, destination, source_data,
		items_bytes])
	p.program.instr2(.store, copy_block, p.program.void_type, required, p.program.struct_field_ptr(copy_block, array_slot, p.program.array_type, 2))
	p.program.instr3(.br, copy_block, p.program.void_type, aliases_destination, ssa.ValueID(free_snapshot), ssa.ValueID(done))
	p.mark_terminated(copy_block)
	free_ref := p.program.m.add_value(.func_ref, p.program.void_type, 'free', p.program.fn_ids['free'])
	p.program.m.add_instr(.call, free_snapshot, p.program.void_type, [free_ref, source_data])
	p.program.instr1(.jmp, free_snapshot, p.program.void_type, ssa.ValueID(done))
	p.mark_terminated(free_snapshot)
	p.cur_block = done
	return FastArm64Value{
		id: p.program.instr1(.load, done, p.program.array_type, array_slot)
		typ: p.program.array_type
		typ_name: array.typ_name
		address: array_slot
	}
}

fn (mut p FastArm64Parser) raw_array_value(source FastArm64Value, count FastArm64Value, element_type_name string) FastArm64Value {
	element_type := p.program.type_id(element_type_name)
	slot := p.program.instr0(.alloca, p.cur_block, p.program.m.type_store.get_ptr(p.program.array_type))
	data := if source.typ == p.program.ptr_i8 {
		source.id
	} else {
		p.program.instr1(.bitcast, p.cur_block, p.program.ptr_i8, source.id)
	}
	p.program.instr2(.store, p.cur_block, p.program.void_type, data, p.program.struct_field_ptr(p.cur_block, slot, p.program.array_type, 0))
	zero := p.program.m.get_or_add_const(p.program.i32_type, '0')
	p.program.instr2(.store, p.cur_block, p.program.void_type, zero, p.program.struct_field_ptr(p.cur_block, slot, p.program.array_type, 1))
	mut length := count
	if length.typ != p.program.i32_type {
		length = p.convert_value(length, p.program.i32_type, 'int')
	}
	for field in [2, 3] {
		p.program.instr2(.store, p.cur_block, p.program.void_type, length.id, p.program.struct_field_ptr(p.cur_block, slot, p.program.array_type, field))
	}
	p.program.instr2(.store, p.cur_block, p.program.void_type, zero, p.program.struct_field_ptr(p.cur_block, slot, p.program.array_type, 4))
	element_size := p.program.m.get_or_add_const(p.program.i32_type, p.program.m.type_size(element_type).str())
	p.program.instr2(.store, p.cur_block, p.program.void_type, element_size, p.program.struct_field_ptr(p.cur_block, slot, p.program.array_type, 5))
	return FastArm64Value{
		id: p.program.instr1(.load, p.cur_block, p.program.array_type, slot)
		typ: p.program.array_type
		typ_name: fastc_array_c_type(element_type_name)
		address: slot
	}
}

fn (mut p FastArm64Parser) emit_array_membership(op token.Token, needle FastArm64Value, array FastArm64Value) !FastArm64Value {
	element_type_name := p.program.array_element_type_name(array.typ_name) or {
		return p.unsupported('array membership type `${array.typ_name}`')
	}
	element_type := p.program.type_id(element_type_name)
	mut expected := needle
	if expected.typ != element_type {
		expected = p.convert_value(expected, element_type, element_type_name)
	}
	array_slot := p.program.instr0(.alloca, p.cur_block, p.program.m.type_store.get_ptr(p.program.array_type))
	p.program.instr2(.store, p.cur_block, p.program.void_type, array.id, array_slot)
	data := p.program.instr1(.load, p.cur_block, p.program.ptr_i8, p.program.struct_field_ptr(p.cur_block, array_slot, p.program.array_type, 0))
	length := p.program.instr1(.load, p.cur_block, p.program.i32_type, p.program.struct_field_ptr(p.cur_block, array_slot, p.program.array_type, 2))
	index_slot := p.program.instr0(.alloca, p.cur_block, p.program.m.type_store.get_ptr(p.program.i32_type))
	result_slot := p.program.instr0(.alloca, p.cur_block, p.program.m.type_store.get_ptr(p.program.i1_type))
	zero32 := p.program.m.get_or_add_const(p.program.i32_type, '0')
	false_value := p.program.m.get_or_add_const(p.program.i1_type, '0')
	p.program.instr2(.store, p.cur_block, p.program.void_type, zero32, index_slot)
	p.program.instr2(.store, p.cur_block, p.program.void_type, false_value, result_slot)
	condition := p.program.m.add_block(p.func_id, 'membership_condition')
	body := p.program.m.add_block(p.func_id, 'membership_body')
	found := p.program.m.add_block(p.func_id, 'membership_found')
	increment := p.program.m.add_block(p.func_id, 'membership_increment')
	done := p.program.m.add_block(p.func_id, 'membership_done')
	p.program.instr1(.jmp, p.cur_block, p.program.void_type, ssa.ValueID(condition))
	p.mark_terminated(p.cur_block)
	index := p.program.instr1(.load, condition, p.program.i32_type, index_slot)
	more := p.program.instr2(.lt, condition, p.program.i1_type, index, length)
	p.program.instr3(.br, condition, p.program.void_type, more, ssa.ValueID(body), ssa.ValueID(done))
	p.mark_terminated(condition)
	index64 := p.program.instr1(.zext, body, p.program.i64_type, index)
	element_size := p.program.m.get_or_add_const(p.program.i64_type, p.program.m.type_size(element_type).str())
	offset := p.program.instr2(.mul, body, p.program.i64_type, index64, element_size)
	address := p.program.instr2(.add, body, p.program.ptr_i8, data, offset)
	typed_address := p.program.instr1(.bitcast, body, p.program.m.type_store.get_ptr(element_type), address)
	element := p.program.instr1(.load, body, element_type, typed_address)
	p.cur_block = body
	equal := p.emit_value_equality(FastArm64Value{
		id: element
		typ: element_type
		typ_name: element_type_name
	}, expected)!
	p.program.instr3(.br, p.cur_block, p.program.void_type, equal.id, ssa.ValueID(found), ssa.ValueID(increment))
	p.mark_terminated(p.cur_block)
	true_value := p.program.m.get_or_add_const(p.program.i1_type, '1')
	p.program.instr2(.store, found, p.program.void_type, true_value, result_slot)
	p.program.instr1(.jmp, found, p.program.void_type, ssa.ValueID(done))
	p.mark_terminated(found)
	one := p.program.m.get_or_add_const(p.program.i32_type, '1')
	next := p.program.instr2(.add, increment, p.program.i32_type, index, one)
	p.program.instr2(.store, increment, p.program.void_type, next, index_slot)
	p.program.instr1(.jmp, increment, p.program.void_type, ssa.ValueID(condition))
	p.mark_terminated(increment)
	p.cur_block = done
	contained := p.program.instr1(.load, done, p.program.i1_type, result_slot)
	result := if op == .not_in {
		p.program.instr2(.eq, done, p.program.i1_type, contained, false_value)
	} else {
		contained
	}
	return FastArm64Value{
		id: result
		typ: p.program.i1_type
		typ_name: 'bool'
	}
}

fn (mut p FastArm64Parser) emit_string_binary(op token.Token, left FastArm64Value, right FastArm64Value) !FastArm64Value {
	ptr_string := p.program.m.type_store.get_ptr(p.program.str_type)
	left_slot := p.program.instr0(.alloca, p.cur_block, ptr_string)
	right_slot := p.program.instr0(.alloca, p.cur_block, ptr_string)
	p.program.instr2(.store, p.cur_block, p.program.void_type, left.id, left_slot)
	p.program.instr2(.store, p.cur_block, p.program.void_type, right.id, right_slot)
	left_data := p.program.instr1(.load, p.cur_block, p.program.ptr_i8, p.program.string_field_ptr(p.cur_block, left_slot, 0))
	right_data := p.program.instr1(.load, p.cur_block, p.program.ptr_i8, p.program.string_field_ptr(p.cur_block, right_slot, 0))
	left_len := p.program.instr1(.load, p.cur_block, p.program.i32_type, p.program.string_field_ptr(p.cur_block, left_slot, 1))
	right_len := p.program.instr1(.load, p.cur_block, p.program.i32_type, p.program.string_field_ptr(p.cur_block, right_slot, 1))
	if op == .plus {
		result_len := p.program.instr2(.add, p.cur_block, p.program.i32_type, left_len, right_len)
		result_len64 := p.program.instr1(.zext, p.cur_block, p.program.i64_type, result_len)
		one64 := p.program.m.get_or_add_const(p.program.i64_type, '1')
		allocation_len := p.program.instr2(.add, p.cur_block, p.program.i64_type, result_len64, one64)
		malloc_ref := p.program.m.add_value(.func_ref, p.program.ptr_i8, 'malloc', p.program.fn_ids['malloc'])
		result_data := p.program.m.add_instr(.call, p.cur_block, p.program.ptr_i8, [
			malloc_ref,
			allocation_len,
		])
		memcpy_ref := p.program.m.add_value(.func_ref, p.program.ptr_i8, 'memcpy', p.program.fn_ids['memcpy'])
		left_len64 := p.program.instr1(.zext, p.cur_block, p.program.i64_type, left_len)
		p.program.m.add_instr(.call, p.cur_block, p.program.ptr_i8, [memcpy_ref, result_data,
			left_data, left_len64])
		right_destination := p.program.instr2(.add, p.cur_block, p.program.ptr_i8, result_data, left_len64)
		right_len64 := p.program.instr1(.zext, p.cur_block, p.program.i64_type, right_len)
		p.program.m.add_instr(.call, p.cur_block, p.program.ptr_i8, [memcpy_ref, right_destination,
			right_data, right_len64])
		terminator := p.program.instr2(.add, p.cur_block, p.program.ptr_i8, result_data, result_len64)
		zero8 := p.program.m.get_or_add_const(p.program.u8_type, '0')
		p.program.instr2(.store, p.cur_block, p.program.void_type, zero8, terminator)
		result_slot := p.program.instr0(.alloca, p.cur_block, ptr_string)
		p.program.instr2(.store, p.cur_block, p.program.void_type, result_data, p.program.string_field_ptr(p.cur_block, result_slot, 0))
		p.program.instr2(.store, p.cur_block, p.program.void_type, result_len, p.program.string_field_ptr(p.cur_block, result_slot, 1))
		zero32 := p.program.m.get_or_add_const(p.program.i32_type, '0')
		p.program.instr2(.store, p.cur_block, p.program.void_type, zero32, p.program.string_field_ptr(p.cur_block, result_slot, 2))
		return FastArm64Value{
			id: p.program.instr1(.load, p.cur_block, p.program.str_type, result_slot)
			typ: p.program.str_type
			typ_name: 'string'
		}
	}
	if op !in [.eq, .ne, .lt, .le, .gt, .ge] {
		return p.unsupported('string binary operator `${op.str()}`')
	}
	left_is_shorter := p.program.instr2(.lt, p.cur_block, p.program.i1_type, left_len, right_len)
	left_is_shorter32 := p.program.instr1(.zext, p.cur_block, p.program.i32_type, left_is_shorter)
	zero_mask := p.program.m.get_or_add_const(p.program.i32_type, '0')
	mask := p.program.instr2(.sub, p.cur_block, p.program.i32_type, zero_mask, left_is_shorter32)
	length_xor := p.program.instr2(.xor, p.cur_block, p.program.i32_type, left_len, right_len)
	masked_difference := p.program.instr2(.and_, p.cur_block, p.program.i32_type, length_xor, mask)
	minimum_len := p.program.instr2(.xor, p.cur_block, p.program.i32_type, right_len, masked_difference)
	minimum_len64 := p.program.instr1(.zext, p.cur_block, p.program.i64_type, minimum_len)
	memcmp_ref := p.program.m.add_value(.func_ref, p.program.i32_type, 'memcmp', p.program.fn_ids['memcmp'])
	comparison := p.program.m.add_instr(.call, p.cur_block, p.program.i32_type, [
		memcmp_ref,
		left_data,
		right_data,
		minimum_len64,
	])
	zero32 := p.program.m.get_or_add_const(p.program.i32_type, '0')
	bytes_equal := p.program.instr2(.eq, p.cur_block, p.program.i1_type, comparison, zero32)
	lengths_equal := p.program.instr2(.eq, p.cur_block, p.program.i1_type, left_len, right_len)
	mut result := ssa.ValueID(0)
	match op {
		.eq {
			result = p.program.instr2(.and_, p.cur_block, p.program.i1_type, bytes_equal, lengths_equal)
		}
		.ne {
			bytes_differ := p.program.instr2(.ne, p.cur_block, p.program.i1_type, comparison, zero32)
			lengths_differ := p.program.instr2(.ne, p.cur_block, p.program.i1_type, left_len, right_len)
			result = p.program.instr2(.or_, p.cur_block, p.program.i1_type, bytes_differ, lengths_differ)
		}
		.lt, .le, .gt, .ge {
			bytes_order := match op {
				.lt, .le {
					p.program.instr2(.lt, p.cur_block, p.program.i1_type, comparison, zero32)
				}
				else { p.program.instr2(.gt, p.cur_block, p.program.i1_type, comparison, zero32) }
			}
			length_order := match op {
				.lt { p.program.instr2(.lt, p.cur_block, p.program.i1_type, left_len, right_len) }
				.le { p.program.instr2(.le, p.cur_block, p.program.i1_type, left_len, right_len) }
				.gt { p.program.instr2(.gt, p.cur_block, p.program.i1_type, left_len, right_len) }
				else { p.program.instr2(.ge, p.cur_block, p.program.i1_type, left_len, right_len) }
			}
			prefix_order := p.program.instr2(.and_, p.cur_block, p.program.i1_type, bytes_equal, length_order)
			result = p.program.instr2(.or_, p.cur_block, p.program.i1_type, bytes_order, prefix_order)
		}
		else {}
	}
	return FastArm64Value{
		id: result
		typ: p.program.i1_type
		typ_name: 'bool'
	}
}
