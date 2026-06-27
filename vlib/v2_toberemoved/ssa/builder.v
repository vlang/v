// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module ssa

import v2.ast
import v2.markused
import v2.token
import v2.types

const enum_field_c_keywords = ['auto', 'break', 'case', 'char', 'const', 'continue', 'default',
	'do', 'double', 'else', 'enum', 'extern', 'float', 'for', 'goto', 'if', 'inline', 'int', 'long',
	'register', 'restrict', 'return', 'short', 'signed', 'sizeof', 'static', 'struct', 'switch',
	'typedef', 'union', 'unsigned', 'void', 'volatile', 'while', '_Bool', '_Complex', '_Imaginary',
	'unix', 'linux']

const fixed_array_empty_literal_element_store_threshold = 16

fn enum_field_symbol_name(name string) string {
	n := if name.len > 0 && name[0] == `@` { 'at_${name[1..]}' } else { name }
	mut escaped := n.replace('*', 'ptr')
	escaped = escaped.replace('&', 'ref')
	escaped = escaped.replace('@', 'at_')
	escaped = escaped.replace(' ', '_')
	escaped = escaped.replace('.', '__')
	if escaped in enum_field_c_keywords {
		return '_${escaped}'
	}
	return escaped
}

fn normalize_target_os_name(target_os string) string {
	return match target_os.to_lower() {
		'macos', 'darwin' { 'macos' }
		else { target_os.to_lower() }
	}
}

fn (b &Builder) is_macos_target() bool {
	return normalize_target_os_name(b.target_os) == 'macos'
}

fn (b &Builder) is_linux_target() bool {
	return normalize_target_os_name(b.target_os) == 'linux'
}

fn (b &Builder) is_windows_target() bool {
	return normalize_target_os_name(b.target_os) == 'windows'
}

fn (mut b Builder) build_c_errno_addr() ?ValueID {
	if !b.is_macos_target() && !b.is_linux_target() && !b.is_windows_target() {
		return none
	}
	i32_t := b.mod.type_store.get_int(32)
	ptr_i32 := b.mod.type_store.get_ptr(i32_t)
	err_fn_name := if b.is_macos_target() {
		'__error'
	} else if b.is_windows_target() {
		'_errno'
	} else {
		'__errno_location'
	}
	err_fn := b.get_or_create_fn_ref(err_fn_name, ptr_i32)
	return b.mod.add_instr(.call, b.cur_block, ptr_i32, [err_fn])
}

fn (mut b Builder) build_windows_c_macro_const(name string) ?ValueID {
	if !b.is_windows_target() {
		return none
	}
	// WinAPI GetStdHandle constants are DWORD macros, not linkable symbols.
	win_std_handle_val := match name {
		'STD_INPUT_HANDLE' { '4294967286' }
		'STD_OUTPUT_HANDLE' { '4294967285' }
		'STD_ERROR_HANDLE' { '4294967284' }
		else { '' }
	}

	if win_std_handle_val.len > 0 {
		return b.mod.get_or_add_const(b.mod.type_store.get_uint(32), win_std_handle_val)
	}
	if name == 'INVALID_HANDLE_VALUE' {
		i8_t := b.mod.type_store.get_int(8)
		voidptr_t := b.mod.type_store.get_ptr(i8_t)
		return b.mod.get_or_add_const(voidptr_t, '-1')
	}
	return none
}

fn (mut b Builder) build_raw_c_global_addr(name string, typ TypeID) ValueID {
	if glob_id := b.find_global(name) {
		return glob_id
	}
	glob := b.mod.add_external_global(name, typ)
	b.global_refs[name] = glob
	return glob
}

fn (mut b Builder) build_c_errno_storage_addr() ValueID {
	if errno_addr := b.build_c_errno_addr() {
		return errno_addr
	}
	i32_t := b.mod.type_store.get_int(32)
	return b.build_raw_c_global_addr('errno', i32_t)
}

fn ssa_module_storage_name(module_name string, name string) string {
	if name == '' || name.starts_with('C.') {
		return name
	}
	if module_name == 'C' {
		return 'C.${name}'
	}
	if module_name != '' && module_name != 'main' {
		return '${module_name}__${name}'
	}
	return name
}

fn ssa_module_storage_field_is_c_extern(node ast.GlobalDecl, field ast.FieldDecl) bool {
	return field.name.starts_with('C.') || node.attributes.has('c_extern')
		|| field.attributes.has('c_extern')
}

// cursor_attrs_has (s237, fixed s246) reports whether an attribute list (a
// CursorList of .aux_attribute nodes) contains an attribute named `name`.
// Cursor mirror of `[]Attribute.has(name)`: an attribute matches when its
// name_id equals `name`, OR (the `@[flag]`/`@[c_extern]` shape) when name_id is
// empty and its value (edge 0) is an Ident whose name equals `name`. The earlier
// name_id-only version silently missed `@[flag]`/`@[c_extern]`, since the parser
// stores those as `Attribute{name:'', value: Ident{name:...}}`.
fn cursor_attrs_has(attrs ast.CursorList, name string) bool {
	for i in 0 .. attrs.len() {
		attr := attrs.at(i)
		if attr.name() == name {
			return true
		}
		if attr.name() == '' {
			val := attr.edge(0)
			if val.kind() == .expr_ident && val.name() == name {
				return true
			}
		}
	}
	return false
}

struct DynConstArray {
	arr_global_name  string // V array struct global name
	data_global_name string // raw data global name
	elem_count       int
	elem_size        int
}

struct SelectorAddr {
	addr      ValueID
	base_type TypeID
}

struct EmbeddedFieldSpan {
	source_type TypeID
	start       int
	len         int
}

struct StructInitFieldPlan {
	source_idx  int
	target_idx  int
	expand_len  int
	source_type TypeID
}

struct MatchExprIncoming {
	value ValueID
	block BlockID
}

struct MatchSumtypeBranchInfo {
	tag          int
	variant_type TypeID
}

const unsupported_captured_fn_literal_symbol = 'v2_unsupported_captured_fn_literal'

pub struct Builder {
pub mut:
	mod        &Module
	cur_module string = 'main'
	// When set, build_fn_bodies only builds this function (hot code reload optimization)
	hot_fn string
	// When set, build_all skips Phase 4 (function bodies) — caller handles it.
	skip_fn_bodies bool
	// When set, only build functions whose decl key is in this map (dead code elimination).
	used_fn_keys map[string]bool
	// When set, skip all functions from these modules (dead code elimination for unused backends).
	skip_modules map[string]bool
	// Optional path fragments for skip_modules. When populated for a module,
	// the skip only applies to files whose path matches the fragment.
	skip_module_file_fragments map[string]string
	// Native self-hosted builds use the SSA sumtype layout directly; guard
	// against null large-variant payloads before matching types.Type.
	guard_invalid_type_payloads bool
	// Target OS for lowering target-specific C globals.
	target_os string
	// When set, native Windows PE builds rely on markused instead of retaining
	// broad builtin runtime modules before linking.
	minimal_runtime_roots bool
	// When set, the backend zeroes large fixed-array allocas as a bulk operation,
	// so SSA can keep IR size bounded by skipping per-element zero stores.
	native_backend_bulk_zero_alloca bool
mut:
	env       &types.Environment = unsafe { nil }
	cur_func  int                = -1
	cur_block BlockID            = -1
	// Checker function-scope key for the function currently being lowered.
	cur_fn_scope_key string
	// Variable name -> SSA ValueID (alloca pointer)
	vars map[string]ValueID
	// Loop break/continue targets
	loop_stack []LoopInfo
	// Struct name -> SSA TypeID
	struct_types map[string]TypeID
	// Flattened embedded field ranges by owner struct TypeID.
	struct_embedded_spans map[int][]EmbeddedFieldSpan
	// Type alias name -> resolved SSA base TypeID
	type_aliases map[string]TypeID
	// Enum name -> field values
	enum_values map[string]int
	// Function name -> SSA function index
	fn_index        map[string]int
	module_fn_names map[string]string
	// Function name -> dynamic array element type by argument index.
	fn_param_array_elem_types map[string][]TypeID
	// Function name -> SSA func_ref value
	fn_refs map[string]ValueID
	// Current file's selective imports: short symbol name -> primary mangled function name.
	selective_import_fn_names map[string]string
	// Current file's selective imports: short symbol name -> ordered mangled function candidates.
	selective_import_fn_candidates map[string][]string
	// Current file's module imports: local alias/tail name -> mangled module name.
	module_import_aliases map[string]string
	// Global variable name -> SSA global value
	global_refs map[string]ValueID
	// Constant name -> evaluated integer value (for inlining)
	const_values      map[string]i64
	const_value_types map[string]TypeID // SSA type for the constant (e.g., u64 vs i64)
	// String constant name -> string literal value (for inlining)
	string_const_values map[string]string
	// Float constant name -> float literal string (for inlining as f64)
	float_const_values map[string]string
	// Label name -> SSA BlockID (for goto/label support)
	label_blocks map[string]BlockID
	// Track mut pointer params (e.g., mut buf &u8) that need extra dereference
	// when used in expressions (buf is ptr(ptr(i8)), but user sees buf as &u8)
	mut_ptr_params map[string]bool
	// Branch-local smartcasts from `if x is T` while lowering SSA.
	local_smartcasts map[string]TypeID
	// Set during sum type init _data field building to trigger heap allocation
	// for &struct_local (prevents dangling stack pointers in returned sum types)
	in_sumtype_data bool
	// Constant array globals: names of globals that store raw element data
	// (not V array structs). build_ident returns the pointer directly.
	const_array_globals    map[string]bool
	const_array_elem_count map[string]int
	// Dynamic const arrays: array struct globals that need _vinit initialization.
	// Key: array struct global name, Value: data global name + metadata.
	dyn_const_arrays []DynConstArray
	// Synthetic native wrapper types for ?T / !T.
	option_wrapper_types map[string]TypeID
	result_wrapper_types map[string]TypeID
	// Counter for generating unique anonymous function names
	anon_fn_counter int
	// Array element types by variable name (for transformer-generated functions
	// where checker position info is unavailable). Maps param/var name to element SSA type.
	array_elem_types map[string]TypeID
	// Array element types by SSA value id for dynamic array values that came from
	// typed fields/selectors but have the erased builtin `array` SSA layout.
	array_value_elem_types map[int]TypeID
	// Struct field dynamic-array element types keyed by `struct_type_id:field_name`.
	struct_field_array_elem_types map[string]TypeID
	// Expected element type for the array literal currently being built.
	array_literal_elem_type_hint TypeID
	// Current array literal is a temporary element buffer for a voidptr argument
	// such as array.push_noscan, not a dynamic array value.
	array_literal_as_element_buffer bool
}

struct LoopInfo {
	cond_block BlockID
	exit_block BlockID
}

pub fn Builder.new(mod &Module) &Builder {
	return Builder.new_with_env(mod, unsafe { nil })
}

pub fn Builder.new_with_env(mod &Module, env &types.Environment) &Builder {
	mut b := &Builder{
		mod:                            mod
		vars:                           map[string]ValueID{}
		loop_stack:                     []LoopInfo{}
		struct_types:                   map[string]TypeID{}
		struct_embedded_spans:          map[int][]EmbeddedFieldSpan{}
		type_aliases:                   map[string]TypeID{}
		enum_values:                    map[string]int{}
		fn_index:                       map[string]int{}
		module_fn_names:                map[string]string{}
		fn_param_array_elem_types:      map[string][]TypeID{}
		fn_refs:                        map[string]ValueID{}
		selective_import_fn_names:      map[string]string{}
		selective_import_fn_candidates: map[string][]string{}
		module_import_aliases:          map[string]string{}
		global_refs:                    map[string]ValueID{}
		skip_modules:                   map[string]bool{}
		skip_module_file_fragments:     map[string]string{}
		option_wrapper_types:           map[string]TypeID{}
		result_wrapper_types:           map[string]TypeID{}
		array_value_elem_types:         map[int]TypeID{}
		struct_field_array_elem_types:  map[string]TypeID{}
		local_smartcasts:               map[string]TypeID{}
	}
	unsafe {
		b.env = env
		mod.env = env
	}
	return b
}

// new_worker_clone creates a Builder for parallel SSA building.
// Shares read-only maps from the main builder, uses a separate worker Module.
// worker_idx offsets anon_fn_counter so workers don't generate conflicting names.
pub fn (mut b Builder) new_worker_clone(worker_mod &Module, worker_idx int) &Builder {
	// Clone all maps to avoid COW races between threads.
	// Maps that are read-only in Phase 4 (struct_types, enum_values, const_values, etc.)
	// still need cloning because V's map read operations can trigger internal COW writes.
	// Maps that are written in Phase 4 (fn_index, option_wrapper_types, etc.)
	// obviously need per-worker copies.
	return &Builder{
		mod:                             worker_mod
		env:                             b.env
		target_os:                       b.target_os
		minimal_runtime_roots:           b.minimal_runtime_roots
		native_backend_bulk_zero_alloca: b.native_backend_bulk_zero_alloca
		struct_types:                    b.struct_types.clone()
		struct_embedded_spans:           b.struct_embedded_spans.clone()
		type_aliases:                    b.type_aliases.clone()
		enum_values:                     b.enum_values.clone()
		fn_index:                        b.fn_index.clone()
		module_fn_names:                 b.module_fn_names.clone()
		selective_import_fn_names:       b.selective_import_fn_names.clone()
		selective_import_fn_candidates:  b.selective_import_fn_candidates.clone()
		module_import_aliases:           b.module_import_aliases.clone()
		fn_param_array_elem_types:       b.fn_param_array_elem_types.clone()
		global_refs:                     b.global_refs.clone()
		const_values:                    b.const_values.clone()
		const_value_types:               b.const_value_types.clone()
		string_const_values:             b.string_const_values.clone()
		float_const_values:              b.float_const_values.clone()
		const_array_globals:             b.const_array_globals.clone()
		const_array_elem_count:          b.const_array_elem_count.clone()
		option_wrapper_types:            b.option_wrapper_types.clone()
		result_wrapper_types:            b.result_wrapper_types.clone()
		struct_field_array_elem_types:   b.struct_field_array_elem_types.clone()
		// Offset anon_fn_counter so each worker generates unique names.
		// Stride of 100_000 per worker avoids collisions.
		anon_fn_counter: (worker_idx + 1) * 100_000
		// Per-function state is reset at start of each build_fn, so empty init is fine
		fn_refs:          map[string]ValueID{}
		vars:             map[string]ValueID{}
		loop_stack:       []LoopInfo{}
		label_blocks:     map[string]BlockID{}
		mut_ptr_params:   map[string]bool{}
		local_smartcasts: map[string]TypeID{}
	}
}

pub fn imported_symbol_fn_name(module_name string, name string) string {
	normalized_module_name := module_name_to_ssa_name(module_name)
	if normalized_module_name == '' || normalized_module_name == 'main' {
		return name
	}
	return '${normalized_module_name}__${name}'
}

fn (b &Builder) resolve_module_call_fn_name(module_name string, name string) string {
	full_name := imported_symbol_fn_name(module_name, name)
	if full_name in b.fn_index {
		return full_name
	}
	if module_name.contains('.') {
		leaf_name := imported_symbol_fn_name(module_name.all_after_last('.'), name)
		if leaf_name in b.fn_index {
			return leaf_name
		}
	}
	return full_name
}

fn module_name_to_ssa_name(module_name string) string {
	return module_name.replace('.', '_')
}

fn checked_type_name_to_ssa_name(type_name string) string {
	if type_name == '' {
		return ''
	}
	if type_name.contains('__') {
		separator_idx := type_name.last_index('__') or { -1 }
		module_name := type_name[..separator_idx]
		name := type_name[separator_idx + 2..]
		return imported_symbol_fn_name(module_name, name)
	}
	if type_name.contains('.') {
		dot_idx := type_name.last_index('.') or { -1 }
		module_name := type_name[..dot_idx]
		name := type_name[dot_idx + 1..]
		return imported_symbol_fn_name(module_name, name)
	}
	return type_name
}

fn generic_token_part_name(name string) string {
	if name == '' {
		return 'Type'
	}
	mut out := []u8{cap: name.len}
	mut wrote_sep := false
	for i in 0 .. name.len {
		ch := name[i]
		if (ch >= `a` && ch <= `z`) || (ch >= `A` && ch <= `Z`) || (ch >= `0` && ch <= `9`) {
			out << ch
			wrote_sep = false
		} else if !wrote_sep {
			out << `_`
			wrote_sep = true
		}
	}
	mut tok := out.bytestr().trim('_')
	if tok == '' {
		tok = 'Type'
	}
	return tok
}

fn import_module_name(imp ast.ImportStmt) string {
	return imp.name
}

fn selective_import_module_name(imp ast.ImportStmt) string {
	if imp.is_aliased {
		return imp.name.all_after_last('.')
	}
	if imp.alias != '' {
		return imp.alias
	}
	return imp.name.all_after_last('.')
}

pub fn module_import_aliases_from_imports(imports []ast.ImportStmt) map[string]string {
	mut aliases := map[string]string{}
	for imp in imports {
		alias := if imp.alias != '' { imp.alias } else { imp.name.all_after_last('.') }
		if !ssa_string_ok(alias) || imp.name.len == 0 {
			continue
		}
		module_name := import_module_name(imp)
		if !ssa_string_ok(module_name) {
			continue
		}
		aliases[alias] = module_name
	}
	return aliases
}

pub fn selective_import_fn_names_from_imports(imports []ast.ImportStmt) map[string]string {
	mut names := map[string]string{}
	for imp in imports {
		if imp.symbols.len == 0 {
			continue
		}
		module_name := import_module_name(imp)
		for symbol in imp.symbols {
			if symbol is ast.Ident {
				names[symbol.name] = imported_symbol_fn_name(module_name, symbol.name)
			}
		}
	}
	return names
}

pub fn selective_import_fn_candidates_from_imports(imports []ast.ImportStmt) map[string][]string {
	mut names := map[string][]string{}
	for imp in imports {
		if imp.symbols.len == 0 {
			continue
		}
		full_module_name := import_module_name(imp)
		leaf_module_name := selective_import_module_name(imp)
		for symbol in imp.symbols {
			if symbol is ast.Ident {
				full_name := imported_symbol_fn_name(full_module_name, symbol.name)
				leaf_name := imported_symbol_fn_name(leaf_module_name, symbol.name)
				mut candidates := []string{cap: 2}
				candidates << full_name
				if leaf_name != full_name {
					candidates << leaf_name
				}
				names[symbol.name] = candidates
			}
		}
	}
	return names
}

fn (b &Builder) selective_import_fn_name(name string) ?string {
	if candidates := b.selective_import_fn_candidates[name] {
		for candidate in candidates {
			if candidate in b.fn_index {
				return candidate
			}
		}
		return none
	}
	if imported := b.selective_import_fn_names[name] {
		if imported in b.fn_index {
			return imported
		}
	}
	return none
}

fn module_fn_key(module_name string, name string) string {
	return '${module_name}:${name}'
}

fn (b &Builder) current_module_fn_name(name string) ?string {
	if name == '' {
		return none
	}
	if target := b.module_fn_names[module_fn_key(b.cur_module, name)] {
		if target in b.fn_index {
			return target
		}
	}
	return none
}

pub fn (mut b Builder) set_selective_import_fn_names(names map[string]string) {
	b.selective_import_fn_names = names.clone()
	mut candidates := map[string][]string{}
	for name, imported in names {
		candidates[name] = [imported]
	}
	b.selective_import_fn_candidates = candidates.clone()
}

pub fn (mut b Builder) set_selective_import_fn_candidates(candidates map[string][]string) {
	b.selective_import_fn_candidates = candidates.clone()
}

pub fn (mut b Builder) set_module_import_aliases(aliases map[string]string) {
	b.module_import_aliases = aliases.clone()
}

pub fn (mut b Builder) build_all(files []ast.File) {
	// Register builtin globals needed by all backends
	i32_t := b.mod.type_store.get_int(32)
	i8_t := b.mod.type_store.get_int(8)
	ptr_t := b.mod.type_store.get_ptr(i8_t)
	ptr_ptr_t := b.mod.type_store.get_ptr(ptr_t)
	b.mod.add_global('g_main_argc', i32_t, false)
	b.mod.add_global('g_main_argv', ptr_ptr_t, false)

	// Pre-register libSystem stdio externs on the main module so that worker
	// modules inherit them and the arm64 backend emits GOT-indirect loads when
	// `C.stdout` / `C.stdin` / `C.stderr` are referenced.
	for stdio_sym in ['__stdoutp', '__stdinp', '__stderrp'] {
		glob := b.mod.add_external_global(stdio_sym, ptr_t)
		b.global_refs[stdio_sym] = glob
	}

	// Phase 1a: Register core builtin types first (string, array) since other structs depend on them.
	// First, register builtin enums (e.g., ArrayFlags) so their types resolve correctly
	// when registering struct fields for array/string.
	for file in files {
		b.cur_module = file_module_name(file)
		if b.cur_module == 'builtin' {
			for stmt in file.stmts {
				if stmt is ast.EnumDecl {
					b.register_enum(stmt)
				}
			}
		}
	}
	for file in files {
		b.cur_module = file_module_name(file)
		if b.cur_module == 'builtin' {
			for stmt in file.stmts {
				if stmt is ast.StructDecl {
					if stmt.name in ['string', 'array'] {
						b.register_struct(stmt)
					}
				}
			}
		}
	}
	// Phase 1b: Register all struct type names (forward declarations) and enums
	for file in files {
		b.cur_module = file_module_name(file)
		b.register_types_pass1(file)
	}
	// Phase 1c: Register type aliases after all pass-1 type names are known.
	for file in files {
		b.cur_module = file_module_name(file)
		b.register_type_aliases(file)
	}
	// Phase 1d: Fill in struct field types (now all struct names are known)
	for file in files {
		b.cur_module = file_module_name(file)
		b.register_types_pass2(file)
	}
	// Embedded structs from imported modules can appear earlier in file order
	// than the embedded type's own field pass. A second idempotent pass lets
	// those now-filled embedded fields flatten without expression lowering.
	for file in files {
		b.cur_module = file_module_name(file)
		b.register_types_pass2(file)
	}
	// Phase 2: Register consts and globals
	for file in files {
		b.cur_module = file_module_name(file)
		b.register_consts_and_globals(file)
	}
	// Phase 2b: Re-evaluate constants with forward references
	// Constants that referenced other constants from later files got value 0.
	// Now that all constants are collected, re-evaluate them.
	b.resolve_forward_const_refs(files)
	// Build global lookup cache once before expression lowering.
	b.index_global_values()
	// Phase 3: Register function signatures
	for file in files {
		b.cur_module = file_module_name(file)
		b.register_fn_signatures(file)
	}
	// Phase 3.1: Remove globals that collide with function names.
	// e.g. sgl has both `const default_context = ...` and `fn default_context() ...`.
	// The function takes precedence; the global would cause the init_consts function
	// to write to the function's TEXT address (read-only), causing a bus error.
	for mut gvar in b.mod.globals {
		if gvar.name in b.fn_index {
			gvar.linkage = .external // Mark as external so codegen skips data symbol
			b.global_refs.delete(gvar.name) // Remove from lookup so stores are not generated
		}
	}

	// Phase 3.5: Generate synthetic stubs for transformer-generated functions
	if b.hot_fn.len == 0 && !b.minimal_runtime_roots {
		b.generate_array_eq_stub()
		b.generate_wymix_stub()
		b.generate_wyhash64_stub()
		b.generate_wyhash_stub()
		b.generate_ierror_stubs()
		b.generate_fd_macro_stubs()
	}

	// Phase 4: Build function bodies
	if !b.skip_fn_bodies {
		b.build_all_fn_bodies(files)
	}
	b.generate_referenced_synthetic_runtime_stubs()

	// Phase 5: Generate _vinit for dynamic array constant initialization
	// Always generate _vinit (even if empty) so the symbol is always resolvable
	if b.hot_fn.len == 0 && !b.skip_fn_bodies {
		b.generate_vinit()
	}
}

// build_all_from_flat is the flat-input counterpart of `build_all`. Phases
// 1a–3.5 (everything before fn-body building) now walk FileCursors directly
// and only rehydrate the decls each phase actually consumes — non-decl stmts
// (ModuleStmt, ImportStmt, attributes, etc.) are never decoded.
//
// Phase 4 also consumes cursors directly: function signatures are read from
// `.stmt_fn_decl` nodes and function bodies are lowered from body stmt cursors.
pub fn (mut b Builder) build_all_from_flat(flat &ast.FlatAst) {
	// Register builtin globals needed by all backends
	i32_t := b.mod.type_store.get_int(32)
	i8_t := b.mod.type_store.get_int(8)
	ptr_t := b.mod.type_store.get_ptr(i8_t)
	ptr_ptr_t := b.mod.type_store.get_ptr(ptr_t)
	b.mod.add_global('g_main_argc', i32_t, false)
	b.mod.add_global('g_main_argv', ptr_ptr_t, false)

	// Pre-register libSystem stdio externs on the main module so that worker
	// modules inherit them and the arm64 backend emits GOT-indirect loads when
	// `C.stdout` / `C.stdin` / `C.stderr` are referenced.
	for stdio_sym in ['__stdoutp', '__stdinp', '__stderrp'] {
		glob := b.mod.add_external_global(stdio_sym, ptr_t)
		b.global_refs[stdio_sym] = glob
	}

	// Phase 1a: Register core builtin types first (string, array) since other
	// structs depend on them. First, register builtin enums (e.g., ArrayFlags)
	// so their types resolve correctly when registering struct fields for
	// array/string.
	for fi in 0 .. flat.files.len {
		fc := flat.file_cursor(fi)
		b.cur_module = fc.mod().replace('.', '_')
		if b.cur_module != 'builtin' {
			continue
		}
		stmts := fc.stmts()
		for si in 0 .. stmts.len() {
			c := stmts.at(si)
			if c.kind() != .stmt_enum_decl {
				continue
			}
			// s238: cursor-native enum registration — no decode_stmt.
			b.register_enum_from_flat(c)
		}
	}
	for fi in 0 .. flat.files.len {
		fc := flat.file_cursor(fi)
		b.cur_module = fc.mod().replace('.', '_')
		if b.cur_module != 'builtin' {
			continue
		}
		stmts := fc.stmts()
		for si in 0 .. stmts.len() {
			c := stmts.at(si)
			if c.kind() != .stmt_struct_decl {
				continue
			}
			// s239: cursor-native struct registration — no decode_stmt. The struct
			// name is the cursor's name_id.
			if c.name() in ['string', 'array'] {
				b.register_struct_from_flat(c)
			}
		}
	}
	// Phase 1b: Register all struct type names (forward declarations) and enums
	for fi in 0 .. flat.files.len {
		fc := flat.file_cursor(fi)
		b.cur_module = fc.mod().replace('.', '_')
		b.register_types_pass1_from_flat(fc)
	}
	// Phase 1c: Register type aliases after all pass-1 type names are known.
	for fi in 0 .. flat.files.len {
		fc := flat.file_cursor(fi)
		b.cur_module = fc.mod().replace('.', '_')
		b.register_type_aliases_from_flat(fc)
	}
	// Phase 1d: Fill in struct field types (now all struct names are known)
	for fi in 0 .. flat.files.len {
		fc := flat.file_cursor(fi)
		b.cur_module = fc.mod().replace('.', '_')
		b.register_types_pass2_from_flat(fc)
	}
	// See the AST pass2 note above: imported embedded field layouts may only
	// become available after the first pass over all files.
	for fi in 0 .. flat.files.len {
		fc := flat.file_cursor(fi)
		b.cur_module = fc.mod().replace('.', '_')
		b.register_types_pass2_from_flat(fc)
	}
	// Phase 2: Register consts and globals
	for fi in 0 .. flat.files.len {
		fc := flat.file_cursor(fi)
		b.cur_module = fc.mod().replace('.', '_')
		b.register_consts_and_globals_from_flat(fc)
	}
	// Phase 2b: Re-evaluate constants with forward references
	b.resolve_forward_const_refs_from_flat(flat)
	// Build global lookup cache once before expression lowering.
	b.index_global_values()
	// Phase 3: Register function signatures
	for fi in 0 .. flat.files.len {
		fc := flat.file_cursor(fi)
		b.cur_module = fc.mod().replace('.', '_')
		b.register_fn_signatures_from_flat(fc)
	}
	// Phase 3.1: Remove globals that collide with function names.
	for mut gvar in b.mod.globals {
		if gvar.name in b.fn_index {
			gvar.linkage = .external
			b.global_refs.delete(gvar.name)
		}
	}

	// Phase 3.5: Generate synthetic stubs for transformer-generated functions
	if b.hot_fn.len == 0 && !b.minimal_runtime_roots {
		b.generate_array_eq_stub()
		b.generate_wymix_stub()
		b.generate_wyhash64_stub()
		b.generate_wyhash_stub()
		b.generate_ierror_stubs()
		b.generate_fd_macro_stubs()
	}

	// Phase 4: build function bodies from `.stmt_fn_decl` cursors. Signatures,
	// filters, params, and body statements all stay on the flat path.
	if !b.skip_fn_bodies {
		for fi in 0 .. flat.files.len {
			fc := flat.file_cursor(fi)
			b.cur_module = fc.mod().replace('.', '_')
			b.build_fn_bodies_from_flat(fc)
		}
	}
	b.generate_referenced_synthetic_runtime_stubs()

	// Phase 5: Generate _vinit for dynamic array constant initialization
	if b.hot_fn.len == 0 && !b.skip_fn_bodies {
		b.generate_vinit()
	}
}

// generate_referenced_synthetic_runtime_stubs materializes native-only helpers
// that are referenced after transformation or minimal-runtime pruning.
pub fn (mut b Builder) generate_referenced_synthetic_runtime_stubs() {
	if b.hot_fn.len != 0 || b.skip_fn_bodies {
		return
	}
	referenced := b.referenced_func_ref_names()
	if referenced['builtin__string__plus_two'] {
		b.generate_string_plus_two_stub()
	}
	if !b.minimal_runtime_roots {
		return
	}
	if referenced['array__eq'] {
		b.generate_array_eq_stub()
	}
	if referenced['_wymix'] {
		b.generate_wymix_stub()
	}
	if referenced['wyhash64'] {
		b.generate_wyhash64_stub()
	}
	if referenced['wyhash'] {
		b.generate_wyhash_stub()
	}
	if referenced['IError__msg'] || referenced['IError__code'] || referenced['IError__type_name'] {
		b.generate_ierror_stubs()
	}
	if referenced['FD_ZERO'] || referenced['FD_SET'] || referenced['FD_ISSET'] {
		b.generate_fd_macro_stubs()
	}
}

fn (b &Builder) referenced_func_ref_names() map[string]bool {
	mut names := map[string]bool{}
	for val in b.mod.values {
		if val.kind != .instruction {
			continue
		}
		instr := b.mod.instrs[val.index]
		for operand in instr.operands {
			if operand <= 0 || operand >= b.mod.values.len {
				continue
			}
			ref := b.mod.values[operand]
			if ref.kind == .func_ref && ref.name != '' {
				names[ref.name] = true
			}
		}
	}
	return names
}

// build_all_fn_bodies builds SSA for all function bodies (Phase 4).
// Separated from build_all to allow the parallel builder to replace this step.
pub fn (mut b Builder) build_all_fn_bodies(files []ast.File) {
	for file in files {
		b.cur_module = file_module_name(file)
		b.build_fn_bodies(file)
	}
}

pub fn file_module_name(file ast.File) string {
	for stmt in file.stmts {
		if stmt is ast.ModuleStmt {
			return stmt.name.replace('.', '_')
		}
	}
	return 'main'
}

// --- Type resolution using types.Environment ---

fn ssa_string_ok(s string) bool {
	if s.len == 0 {
		return true
	}
	if s.len < 0 || s.len > 1024 {
		return false
	}
	ptr := unsafe { u64(s.str) }
	return ptr >= 0x10000 && ptr < 0x0000800000000000
}

fn ssa_module_tail_name(name string) string {
	if name.contains('.') {
		return name.all_after_last('.')
	}
	if name.contains('__') {
		return name.all_after_last('__')
	}
	if name.starts_with('v2_') && name.contains('_') {
		return name.all_after_last('_')
	}
	return name
}

fn ssa_is_string_struct_name(name string) bool {
	return name == 'string' || name == 'builtin__string'
}

fn ssa_is_string_str_fn_name(name string) bool {
	return name == 'string__str' || name == 'builtin__string__str'
}

fn (b &Builder) valid_value_id(val ValueID) bool {
	return val > 0 && val < b.mod.values.len
}

fn (b &Builder) valid_type_id(typ TypeID) bool {
	return typ > 0 && typ < b.mod.type_store.types.len
}

fn (mut b Builder) type_to_ssa(t types.Type) TypeID {
	if b.guard_invalid_type_payloads && !types.type_has_valid_payload(t) {
		return b.mod.type_store.get_int(64)
	}
	match t {
		types.Primitive {
			if t.props.has(.boolean) {
				return b.mod.type_store.get_int(1)
			}
			if t.props.has(.float) {
				width := if t.size == 32 { 32 } else { 64 }
				return b.mod.type_store.get_float(width)
			}
			if t.props.has(.integer) {
				size := if t.size == 0 { 32 } else { int(t.size) }
				if t.props.has(.unsigned) {
					return b.mod.type_store.get_uint(size)
				}
				return b.mod.type_store.get_int(size)
			}
			return b.mod.type_store.get_int(32)
		}
		types.Pointer {
			if t.base_type is types.Struct {
				st := t.base_type as types.Struct
				if !ssa_string_ok(st.name) {
					return b.mod.type_store.get_int(64)
				}
				if base := b.struct_name_to_ssa(st.name) {
					return b.mod.type_store.get_ptr(base)
				}
			}
			base := b.type_to_ssa(t.base_type)
			return b.mod.type_store.get_ptr(base)
		}
		types.String {
			return b.get_string_type()
		}
		types.Struct {
			if !ssa_string_ok(t.name) {
				return b.mod.type_store.get_int(64)
			}
			if id := b.struct_name_to_ssa(t.name) {
				return id
			}
			return b.mod.type_store.get_int(64) // fallback
		}
		types.Enum {
			return b.mod.type_store.get_int(32)
		}
		types.Void {
			return 0 // void
		}
		types.Char {
			return b.mod.type_store.get_int(8)
		}
		types.Rune {
			return b.mod.type_store.get_int(32)
		}
		types.ISize {
			return b.mod.type_store.get_int(64)
		}
		types.USize {
			return b.mod.type_store.get_uint(64)
		}
		types.Alias {
			base := b.resolve_alias_base_type(t) or { return b.mod.type_store.get_int(64) }
			return b.type_to_ssa(base)
		}
		types.Array {
			// Dynamic arrays are struct-like: {data*, len, cap, element_size}
			return b.get_array_type()
		}
		types.ArrayFixed {
			// Fixed-size arrays: [N]T → SSA array type
			elem_type := b.type_to_ssa(t.elem_type)
			if t.len > 0 && elem_type != 0 {
				return b.mod.type_store.get_array(elem_type, t.len)
			}
			return b.mod.type_store.get_int(64) // fallback
		}
		types.Nil {
			i8_t := b.mod.type_store.get_int(8)
			return b.mod.type_store.get_ptr(i8_t)
		}
		types.None {
			return 0
		}
		types.Tuple {
			tt := t.get_types()
			mut elem_types := []TypeID{cap: tt.len}
			for et in tt {
				elem_types << b.type_to_ssa(et)
			}
			return b.mod.type_store.get_tuple(elem_types)
		}
		types.SumType {
			if !ssa_string_ok(t.name) {
				return b.mod.type_store.get_int(32)
			}
			if id := b.struct_types[t.name] {
				return id
			}
			// Try module-qualified name
			qualified_st := '${b.cur_module}__${t.name}'
			if id := b.struct_types[qualified_st] {
				return id
			}
			// Search all known module prefixes
			for sname, sid in b.struct_types {
				if sname.ends_with('__${t.name}') {
					return sid
				}
			}
			return b.mod.type_store.get_int(64) // fallback
		}
		types.Map {
			return b.struct_types['map'] or { b.mod.type_store.get_int(64) }
		}
		types.OptionType {
			return b.get_option_wrapper_type(b.type_to_ssa(t.base_type))
		}
		types.ResultType {
			return b.get_result_wrapper_type(b.type_to_ssa(t.base_type))
		}
		types.FnType {
			i8_t := b.mod.type_store.get_int(8)
			return b.mod.type_store.get_ptr(i8_t) // fn pointers
		}
		types.Interface {
			return b.mod.type_store.get_int(64) // interfaces lowered to i64
		}
		else {
			name := types.type_name(t)
			if name != '' {
				return b.named_type_to_ssa(name)
			}
			return b.mod.type_store.get_int(64) // fallback for unhandled
		}
	}
}

fn (mut b Builder) struct_name_to_ssa(name string) ?TypeID {
	if !ssa_string_ok(name) {
		return none
	}
	if id := b.struct_types[name] {
		return id
	}
	// Try module-qualified name: C structs are registered as "os__dirent"
	// but the type checker stores them as just "dirent".
	qualified := '${b.cur_module}__${name}'
	if id := b.struct_types[qualified] {
		return id
	}
	// Try all known module prefixes for cross-module struct access.
	for sname, sid in b.struct_types {
		if sname.ends_with('__${name}') {
			return sid
		}
	}
	return none
}

fn pointer_type_part_name(base string) string {
	if base == '' {
		return ''
	}
	return '${base}ptr'
}

fn (b &Builder) generic_ident_type_part_name(name string) string {
	if name == '' {
		return ''
	}
	normalized := name.replace('.', '__')
	if normalized.contains('__') {
		return normalized
	}
	if b.env == unsafe { nil } {
		return normalized
	}
	typ := b.lookup_checked_type_by_name(name) or { return normalized }
	match typ {
		types.Struct, types.SumType {
			typ_name := types.type_name(typ)
			if typ_name == '' {
				return normalized
			}
			return checked_type_name_to_ssa_name(typ_name)
		}
		else {
			return normalized
		}
	}
}

fn (b &Builder) generic_type_part_name(expr ast.Expr) string {
	match expr {
		ast.Ident {
			return b.generic_ident_type_part_name(expr.name)
		}
		ast.SelectorExpr {
			return b.generic_selector_type_part_name(expr)
		}
		ast.PrefixExpr {
			if expr.op == .amp {
				return pointer_type_part_name(b.generic_type_part_name(expr.expr))
			}
			return ''
		}
		ast.Type {
			match expr {
				ast.ArrayType {
					elem_name := b.generic_type_part_name(expr.elem_type)
					if elem_name == '' {
						return ''
					}
					return 'Array_${elem_name}'
				}
				ast.ArrayFixedType {
					elem_name := b.generic_type_part_name(expr.elem_type)
					len_name := if expr.len is ast.BasicLiteral {
						(expr.len as ast.BasicLiteral).value
					} else {
						''
					}
					if elem_name == '' || len_name == '' {
						return ''
					}
					return 'Array_fixed_${elem_name}_${len_name}'
				}
				ast.MapType {
					key_name := b.generic_type_part_name(expr.key_type)
					value_name := b.generic_type_part_name(expr.value_type)
					if key_name == '' || value_name == '' {
						return ''
					}
					return 'Map_${key_name}_${value_name}'
				}
				ast.PointerType {
					return pointer_type_part_name(b.generic_type_part_name(expr.base_type))
				}
				ast.GenericType {
					return b.generic_type_name(expr.name, expr.params)
				}
				ast.OptionType {
					base_name := b.generic_type_part_name(expr.base_type)
					if base_name == '' {
						return ''
					}
					return 'Option_${base_name}'
				}
				ast.ResultType {
					base_name := b.generic_type_part_name(expr.base_type)
					if base_name == '' {
						return ''
					}
					return 'Result_${base_name}'
				}
				else {
					return ''
				}
			}
		}
		else {
			return ''
		}
	}
}

fn (b &Builder) generic_ident_type_arg_part_name(name string) string {
	if name == '' {
		return ''
	}
	if b.env == unsafe { nil } {
		return generic_token_part_name(name)
	}
	typ := b.lookup_checked_type_by_name(name) or { return generic_token_part_name(name) }
	match typ {
		types.Struct, types.SumType {
			typ_name := types.type_name(typ)
			if typ_name == '' {
				return generic_token_part_name(name)
			}
			return generic_token_part_name(typ_name)
		}
		else {
			return generic_token_part_name(name)
		}
	}
}

fn (b &Builder) generic_selector_type_arg_part_name(expr ast.SelectorExpr) string {
	full_name := b.generic_selector_type_part_name(expr)
	if full_name == '' {
		return ''
	}
	return generic_token_part_name(full_name)
}

fn (b &Builder) generic_type_arg_part_name(expr ast.Expr) string {
	match expr {
		ast.Ident {
			return b.generic_ident_type_arg_part_name(expr.name)
		}
		ast.SelectorExpr {
			return b.generic_selector_type_arg_part_name(expr)
		}
		ast.PrefixExpr {
			if expr.op == .amp {
				return pointer_type_part_name(b.generic_type_arg_part_name(expr.expr))
			}
			return ''
		}
		ast.Type {
			match expr {
				ast.ArrayType {
					elem_name := b.generic_type_arg_part_name(expr.elem_type)
					if elem_name == '' {
						return ''
					}
					return 'Array_${elem_name}'
				}
				ast.ArrayFixedType {
					elem_name := b.generic_type_arg_part_name(expr.elem_type)
					len_name := if expr.len is ast.BasicLiteral {
						(expr.len as ast.BasicLiteral).value
					} else {
						''
					}
					if elem_name == '' || len_name == '' {
						return ''
					}
					return 'Array_fixed_${elem_name}_${len_name}'
				}
				ast.MapType {
					key_name := b.generic_type_arg_part_name(expr.key_type)
					value_name := b.generic_type_arg_part_name(expr.value_type)
					if key_name == '' || value_name == '' {
						return ''
					}
					return 'Map_${key_name}_${value_name}'
				}
				ast.PointerType {
					return pointer_type_part_name(b.generic_type_arg_part_name(expr.base_type))
				}
				ast.GenericType {
					concrete_name := b.generic_type_name(expr.name, expr.params)
					if concrete_name == '' {
						return ''
					}
					return generic_token_part_name(concrete_name)
				}
				ast.OptionType {
					base_name := b.generic_type_arg_part_name(expr.base_type)
					if base_name == '' {
						return ''
					}
					return 'Option_${base_name}'
				}
				ast.ResultType {
					base_name := b.generic_type_arg_part_name(expr.base_type)
					if base_name == '' {
						return ''
					}
					return 'Result_${base_name}'
				}
				else {
					return ''
				}
			}
		}
		else {
			return ''
		}
	}
}

fn (b &Builder) generic_selector_type_part_name(expr ast.SelectorExpr) string {
	if expr.lhs is ast.Ident {
		lhs_name := (expr.lhs as ast.Ident).name
		rhs_name := expr.rhs.name
		if lhs_name == '' || rhs_name == '' {
			return ''
		}
		if resolved_mod := b.module_import_aliases[lhs_name] {
			return imported_symbol_fn_name(resolved_mod, rhs_name)
		}
		if resolved_mod := b.selector_module_name_for_ident(lhs_name) {
			return imported_symbol_fn_name(resolved_mod, rhs_name)
		}
		return '${lhs_name.replace('.', '__')}__${rhs_name}'
	}
	return expr.name().replace('.', '__')
}

fn generic_type_name_from_parts(base string, parts []string) string {
	if base == '' || parts.len == 0 {
		return ''
	}
	return '${base}_T_${parts.join('_')}'
}

fn concrete_generic_type_suffix_name(concrete_name string) string {
	if !concrete_name.contains('_T_') {
		return ''
	}
	base := concrete_name.all_before('_T_')
	args := concrete_name.all_after('_T_')
	if base == '' || args == '' || !base.contains('__') {
		return ''
	}
	return '${base.all_after_last('__')}_T_${args}'
}

fn (b &Builder) registered_type_by_unique_concrete_suffix(concrete_name string) ?TypeID {
	suffix_name := concrete_generic_type_suffix_name(concrete_name)
	if suffix_name == '' {
		return none
	}
	mut found := TypeID(0)
	mut found_count := 0
	for registered_name, type_id in b.struct_types {
		if registered_name == suffix_name || registered_name.ends_with('__${suffix_name}') {
			found = type_id
			found_count++
		}
	}
	if found_count == 1 {
		return found
	}
	return none
}

fn (b &Builder) generic_type_name(name ast.Expr, params []ast.Expr) string {
	base := b.generic_type_part_name(name)
	mut parts := []string{cap: params.len}
	for param in params {
		part := b.generic_type_arg_part_name(param)
		if part == '' {
			return ''
		}
		parts << part
	}
	return generic_type_name_from_parts(base, parts)
}

fn (mut b Builder) checked_sumtype_for_ssa_name(name string) ?types.SumType {
	typ := b.lookup_checked_type_by_name(name) or {
		if name.contains('_T_') {
			base_name := name.all_before('_T_')
			b.lookup_checked_type_by_name(base_name) or { return none }
		} else {
			return none
		}
	}
	if typ is types.SumType {
		return typ
	}
	return none
}

fn (mut b Builder) source_base_is_sumtype(name string) bool {
	_ := b.checked_sumtype_for_ssa_name(name) or { return false }
	return true
}

fn (mut b Builder) generic_sumtype_storage_to_ssa(base_name string, concrete_name string) ?TypeID {
	if concrete_name == '' || !ssa_string_ok(concrete_name) || !b.source_base_is_sumtype(base_name) {
		return none
	}
	if id := b.struct_name_to_ssa(concrete_name) {
		return id
	}
	i64_t := b.mod.type_store.get_int(64)
	type_id := b.mod.type_store.register(Type{
		kind:        .struct_t
		fields:      [i64_t, i64_t]
		field_names: ['_tag', '_data']
	})
	b.struct_types[concrete_name] = type_id
	b.mod.c_struct_names[type_id] = concrete_name
	return type_id
}

fn (mut b Builder) generic_type_to_ssa(name ast.Expr, params []ast.Expr) TypeID {
	concrete_name := b.generic_type_name(name, params)
	if concrete_name == '' || !ssa_string_ok(concrete_name) {
		return b.mod.type_store.get_int(64)
	}
	if id := b.struct_name_to_ssa(concrete_name) {
		return id
	}
	if id := b.registered_type_by_unique_concrete_suffix(concrete_name) {
		return id
	}
	base_name := b.generic_type_part_name(name)
	if id := b.generic_sumtype_storage_to_ssa(base_name, concrete_name) {
		return id
	}
	return b.mod.type_store.get_int(64)
}

fn (b &Builder) generic_type_part_name_from_flat(c ast.Cursor) string {
	match c.kind() {
		.expr_ident {
			return b.generic_ident_type_part_name(c.name())
		}
		.expr_selector {
			lhs := c.edge(0)
			rhs := c.edge(1)
			if lhs.kind() == .expr_ident && rhs.kind() == .expr_ident {
				return b.generic_selector_type_part_name_from_flat(lhs.name(), rhs.name())
			}
			return ''
		}
		.expr_prefix {
			op := unsafe { token.Token(int(c.aux())) }
			if op == .amp {
				return pointer_type_part_name(b.generic_type_part_name_from_flat(c.edge(0)))
			}
			return ''
		}
		.typ_array {
			elem_name := b.generic_type_part_name_from_flat(c.edge(0))
			if elem_name == '' {
				return ''
			}
			return 'Array_${elem_name}'
		}
		.typ_array_fixed {
			elem_name := b.generic_type_part_name_from_flat(c.edge(1))
			len_c := c.edge(0)
			len_name := if len_c.kind() == .expr_basic_literal { len_c.name() } else { '' }
			if elem_name == '' || len_name == '' {
				return ''
			}
			return 'Array_fixed_${elem_name}_${len_name}'
		}
		.typ_map {
			key_name := b.generic_type_part_name_from_flat(c.edge(0))
			value_name := b.generic_type_part_name_from_flat(c.edge(1))
			if key_name == '' || value_name == '' {
				return ''
			}
			return 'Map_${key_name}_${value_name}'
		}
		.typ_pointer {
			return pointer_type_part_name(b.generic_type_part_name_from_flat(c.edge(0)))
		}
		.typ_generic {
			return b.generic_type_name_from_flat(c)
		}
		.typ_option {
			base_name := b.generic_type_part_name_from_flat(c.edge(0))
			if base_name == '' {
				return ''
			}
			return 'Option_${base_name}'
		}
		.typ_result {
			base_name := b.generic_type_part_name_from_flat(c.edge(0))
			if base_name == '' {
				return ''
			}
			return 'Result_${base_name}'
		}
		else {
			return ''
		}
	}
}

fn (b &Builder) generic_selector_type_part_name_from_flat(lhs_name string, rhs_name string) string {
	if lhs_name == '' || rhs_name == '' {
		return ''
	}
	if resolved_mod := b.module_import_aliases[lhs_name] {
		return imported_symbol_fn_name(resolved_mod, rhs_name)
	}
	if resolved_mod := b.selector_module_name_for_ident(lhs_name) {
		return imported_symbol_fn_name(resolved_mod, rhs_name)
	}
	return '${lhs_name.replace('.', '__')}__${rhs_name}'
}

fn (b &Builder) generic_ident_type_arg_part_name_from_flat(name string) string {
	return b.generic_ident_type_arg_part_name(name)
}

fn (b &Builder) generic_selector_type_arg_part_name_from_flat(lhs_name string, rhs_name string) string {
	full_name := b.generic_selector_type_part_name_from_flat(lhs_name, rhs_name)
	if full_name == '' {
		return ''
	}
	return generic_token_part_name(full_name)
}

fn (b &Builder) generic_type_arg_part_name_from_flat(c ast.Cursor) string {
	match c.kind() {
		.expr_ident {
			return b.generic_ident_type_arg_part_name_from_flat(c.name())
		}
		.expr_selector {
			lhs := c.edge(0)
			rhs := c.edge(1)
			if lhs.kind() == .expr_ident && rhs.kind() == .expr_ident {
				return b.generic_selector_type_arg_part_name_from_flat(lhs.name(), rhs.name())
			}
			return ''
		}
		.expr_prefix {
			op := unsafe { token.Token(int(c.aux())) }
			if op == .amp {
				return pointer_type_part_name(b.generic_type_arg_part_name_from_flat(c.edge(0)))
			}
			return ''
		}
		.typ_array {
			elem_name := b.generic_type_arg_part_name_from_flat(c.edge(0))
			if elem_name == '' {
				return ''
			}
			return 'Array_${elem_name}'
		}
		.typ_array_fixed {
			if c.edge_count() < 2 {
				return ''
			}
			elem_name := b.generic_type_arg_part_name_from_flat(c.edge(1))
			len_c := c.edge(0)
			len_name := if len_c.kind() == .expr_basic_literal { len_c.name() } else { '' }
			if elem_name == '' || len_name == '' {
				return ''
			}
			return 'Array_fixed_${elem_name}_${len_name}'
		}
		.typ_map {
			if c.edge_count() < 2 {
				return ''
			}
			key_name := b.generic_type_arg_part_name_from_flat(c.edge(0))
			value_name := b.generic_type_arg_part_name_from_flat(c.edge(1))
			if key_name == '' || value_name == '' {
				return ''
			}
			return 'Map_${key_name}_${value_name}'
		}
		.typ_pointer {
			return pointer_type_part_name(b.generic_type_arg_part_name_from_flat(c.edge(0)))
		}
		.typ_generic {
			concrete_name := b.generic_type_name_from_flat(c)
			if concrete_name == '' {
				return ''
			}
			return generic_token_part_name(concrete_name)
		}
		.typ_option {
			base_name := b.generic_type_arg_part_name_from_flat(c.edge(0))
			if base_name == '' {
				return ''
			}
			return 'Option_${base_name}'
		}
		.typ_result {
			base_name := b.generic_type_arg_part_name_from_flat(c.edge(0))
			if base_name == '' {
				return ''
			}
			return 'Result_${base_name}'
		}
		else {
			return ''
		}
	}
}

fn (b &Builder) generic_type_name_from_flat(c ast.Cursor) string {
	if c.kind() != .typ_generic || c.edge_count() <= 1 {
		return ''
	}
	base := b.generic_type_part_name_from_flat(c.edge(0))
	mut parts := []string{cap: c.edge_count() - 1}
	for i in 1 .. c.edge_count() {
		part := b.generic_type_arg_part_name_from_flat(c.edge(i))
		if part == '' {
			return ''
		}
		parts << part
	}
	return generic_type_name_from_parts(base, parts)
}

fn (mut b Builder) generic_type_to_ssa_from_flat(c ast.Cursor) TypeID {
	concrete_name := b.generic_type_name_from_flat(c)
	if concrete_name == '' || !ssa_string_ok(concrete_name) {
		return b.mod.type_store.get_int(64)
	}
	if id := b.struct_name_to_ssa(concrete_name) {
		return id
	}
	if id := b.registered_type_by_unique_concrete_suffix(concrete_name) {
		return id
	}
	base_name := b.generic_type_part_name_from_flat(c.edge(0))
	if id := b.generic_sumtype_storage_to_ssa(base_name, concrete_name) {
		return id
	}
	return b.mod.type_store.get_int(64)
}

fn (mut b Builder) primitive_type_name_to_ssa(name string) ?TypeID {
	if !ssa_string_ok(name) {
		return none
	}
	return match name {
		'int' {
			b.mod.type_store.get_int(32)
		}
		'i8' {
			b.mod.type_store.get_int(8)
		}
		'i16' {
			b.mod.type_store.get_int(16)
		}
		'i32' {
			b.mod.type_store.get_int(32)
		}
		'i64' {
			b.mod.type_store.get_int(64)
		}
		'u8', 'byte' {
			b.mod.type_store.get_uint(8)
		}
		'u16' {
			b.mod.type_store.get_uint(16)
		}
		'u32' {
			b.mod.type_store.get_uint(32)
		}
		'u64' {
			b.mod.type_store.get_uint(64)
		}
		'f32' {
			b.mod.type_store.get_float(32)
		}
		'f64' {
			b.mod.type_store.get_float(64)
		}
		'bool' {
			b.mod.type_store.get_int(1)
		}
		'string' {
			b.get_string_type()
		}
		'voidptr' {
			i8_t := b.mod.type_store.get_int(8)
			b.mod.type_store.get_ptr(i8_t)
		}
		'rune' {
			b.mod.type_store.get_int(32)
		}
		'char' {
			b.mod.type_store.get_int(8)
		}
		'isize' {
			b.mod.type_store.get_int(64)
		}
		'usize' {
			b.mod.type_store.get_uint(64)
		}
		else {
			none
		}
	}
}

fn (mut b Builder) named_type_to_ssa(name string) TypeID {
	if !ssa_string_ok(name) {
		return b.mod.type_store.get_int(64)
	}
	if type_id := b.primitive_type_name_to_ssa(name) {
		return type_id
	}
	normalized := name.replace('.', '__')
	if type_id := b.lookup_type_alias_ssa(name) {
		return type_id
	}
	if normalized != name {
		if type_id := b.lookup_type_alias_ssa(normalized) {
			return type_id
		}
	}
	if normalized in b.struct_types {
		return b.struct_types[normalized]
	}
	if b.is_enum_type(name) || b.is_enum_type(normalized) {
		return b.mod.type_store.get_int(32)
	}
	if typ := b.lookup_checked_type_by_name(name) {
		if typ is types.Alias {
			return b.type_to_ssa(typ)
		} else {
			typ_name := types.type_name(typ)
			if typ_name != name {
				return b.type_to_ssa(typ)
			}
		}
	}
	if normalized != name {
		if typ := b.lookup_checked_type_by_name(normalized) {
			if typ is types.Alias {
				return b.type_to_ssa(typ)
			} else {
				typ_name := types.type_name(typ)
				if typ_name != name && typ_name != normalized {
					return b.type_to_ssa(typ)
				}
			}
		}
	}
	return b.mod.type_store.get_int(64)
}

fn (b &Builder) selector_module_name_for_ident(mod_name string) ?string {
	if mod_name == 'C' {
		return 'C'
	}
	if resolved_mod := b.module_import_aliases[mod_name] {
		return resolved_mod
	}
	if b.env != unsafe { nil } {
		if scope := b.env.get_scope(b.cur_module) {
			if obj := scope.lookup_parent(mod_name, 0) {
				if obj is types.Module {
					return obj.name
				}
			}
		}
	}
	return none
}

fn (b &Builder) selector_type_alias_to_ssa(mod_name string, type_name string) ?TypeID {
	if mod_name == '' || type_name == '' || !ssa_string_ok(mod_name) || !ssa_string_ok(type_name) {
		return none
	}
	normalized_mod := mod_name.replace('.', '_')
	qualified := ssa_module_storage_name(normalized_mod, type_name)
	if type_id := b.lookup_type_alias_ssa(qualified) {
		return type_id
	}
	if normalized_mod != mod_name {
		dot_qualified := ssa_module_storage_name(mod_name, type_name)
		if type_id := b.lookup_type_alias_ssa(dot_qualified) {
			return type_id
		}
	}
	short_mod := ssa_module_tail_name(normalized_mod)
	if short_mod != '' && short_mod != normalized_mod {
		short_qualified := ssa_module_storage_name(short_mod, type_name)
		if type_id := b.lookup_type_alias_ssa(short_qualified) {
			return type_id
		}
	}
	return none
}

fn (b &Builder) lookup_type_alias_ssa(name string) ?TypeID {
	if name == '' || !ssa_string_ok(name) {
		return none
	}
	if type_id := b.type_aliases[name] {
		return type_id
	}
	if b.cur_module != '' && b.cur_module != 'main' && !name.contains('__') {
		if type_id := b.type_aliases['${b.cur_module}__${name}'] {
			return type_id
		}
		short_mod := ssa_module_tail_name(b.cur_module)
		if short_mod != '' && short_mod != b.cur_module {
			if type_id := b.type_aliases['${short_mod}__${name}'] {
				return type_id
			}
		}
	}
	if name.contains('__') {
		mod_name := name.all_before_last('__')
		type_name := name.all_after_last('__')
		short_mod := ssa_module_tail_name(mod_name)
		if short_mod != '' && short_mod != mod_name {
			if type_id := b.type_aliases['${short_mod}__${type_name}'] {
				return type_id
			}
		}
		qualified_suffix := '__${name}'
		for alias_name, type_id in b.type_aliases {
			if alias_name.ends_with(qualified_suffix) {
				return type_id
			}
		}
	}
	return none
}

fn (b &Builder) sizeof_type_alias_name(name string) ?int {
	if type_id := b.lookup_type_alias_ssa(name) {
		return b.type_byte_size(type_id)
	}
	normalized := name.replace('.', '__')
	if normalized != name {
		if type_id := b.lookup_type_alias_ssa(normalized) {
			return b.type_byte_size(type_id)
		}
	}
	return none
}

fn struct_field_array_elem_key(type_id TypeID, field_name string) string {
	return '${int(type_id)}:${field_name}'
}

fn (mut b Builder) record_struct_field_array_elem_type(type_id TypeID, field_name string, field_type ast.Expr, field_ssa_type TypeID) {
	if field_name == '' || field_ssa_type != b.get_array_type() {
		return
	}
	elem_type := b.array_elem_type_from_ast_type(field_type)
	if elem_type != 0 {
		b.struct_field_array_elem_types[struct_field_array_elem_key(type_id, field_name)] = elem_type
	}
}

// record_struct_field_array_elem_type_from_flat (s239) is the cursor mirror of
// record_struct_field_array_elem_type. `field_type_c` is the field's typ cursor;
// elem-type inference reuses array_elem_type_from_ast_type_from_flat (s233).
fn (mut b Builder) record_struct_field_array_elem_type_from_flat(type_id TypeID, field_name string, field_type_c ast.Cursor, field_ssa_type TypeID) {
	if field_name == '' || field_ssa_type != b.get_array_type() {
		return
	}
	elem_type := b.array_elem_type_from_ast_type_from_flat(field_type_c)
	if elem_type != 0 {
		b.struct_field_array_elem_types[struct_field_array_elem_key(type_id, field_name)] = elem_type
	}
}

fn (b &Builder) struct_field_array_elem_type(type_id TypeID, field_name string) TypeID {
	if field_name == '' {
		return 0
	}
	mut cur_type := type_id
	for cur_type > 0 && int(cur_type) < b.mod.type_store.types.len {
		if elem_type := b.struct_field_array_elem_types[struct_field_array_elem_key(cur_type,
			field_name)]
		{
			return elem_type
		}
		typ := b.mod.type_store.types[cur_type]
		if typ.kind == .ptr_t && typ.elem_type > 0 {
			cur_type = typ.elem_type
			continue
		}
		break
	}
	return 0
}

fn (mut b Builder) get_string_type() TypeID {
	return b.struct_types['string'] or { 0 }
}

fn (mut b Builder) get_array_type() TypeID {
	return b.struct_types['array'] or { 0 }
}

fn (b &Builder) is_string_like_ssa_type(typ_id TypeID) bool {
	if typ_id == 0 || int(typ_id) >= b.mod.type_store.types.len {
		return false
	}
	str_type := b.struct_types['string'] or { TypeID(0) }
	if str_type != 0 && typ_id == str_type {
		return true
	}
	typ := b.mod.type_store.types[typ_id]
	return typ.kind == .ptr_t && typ.elem_type == str_type
}

fn (mut b Builder) load_string_like_value(val_id ValueID) ValueID {
	if val_id <= 0 || int(val_id) >= b.mod.values.len {
		return val_id
	}
	str_type := b.get_string_type()
	if str_type == 0 {
		return val_id
	}
	typ_id := b.mod.values[val_id].typ
	if typ_id == str_type {
		return val_id
	}
	if typ_id > 0 && int(typ_id) < b.mod.type_store.types.len {
		typ := b.mod.type_store.types[typ_id]
		if typ.kind == .ptr_t && typ.elem_type == str_type {
			return b.mod.add_instr(.load, b.cur_block, str_type, [val_id])
		}
	}
	return val_id
}

fn (mut b Builder) get_ierror_storage_type() TypeID {
	if 'IError' in b.struct_types {
		return b.struct_types['IError']
	}
	return b.mod.type_store.get_int(64)
}

fn (mut b Builder) get_option_wrapper_type(base_type TypeID) TypeID {
	key := base_type.str()
	if type_id := b.option_wrapper_types[key] {
		return type_id
	}
	state_type := b.mod.type_store.get_int(8)
	err_type := b.get_ierror_storage_type()
	mut field_types := []TypeID{cap: 3}
	mut field_names := []string{cap: 3}
	field_types << state_type
	field_names << 'state'
	field_types << err_type
	field_names << 'err'
	if base_type != 0 {
		field_types << base_type
		field_names << 'data'
	}
	type_id := b.mod.type_store.register(Type{
		kind:        .struct_t
		fields:      field_types
		field_names: field_names
	})
	b.option_wrapper_types[key] = type_id
	return type_id
}

fn (mut b Builder) get_result_wrapper_type(base_type TypeID) TypeID {
	key := base_type.str()
	if type_id := b.result_wrapper_types[key] {
		return type_id
	}
	bool_type := b.mod.type_store.get_int(1)
	err_type := b.get_ierror_storage_type()
	mut field_types := []TypeID{cap: 3}
	mut field_names := []string{cap: 3}
	field_types << bool_type
	field_names << 'is_error'
	field_types << err_type
	field_names << 'err'
	if base_type != 0 {
		field_types << base_type
		field_names << 'data'
	}
	type_id := b.mod.type_store.register(Type{
		kind:        .struct_t
		fields:      field_types
		field_names: field_names
	})
	b.result_wrapper_types[key] = type_id
	return type_id
}

fn (b &Builder) is_option_wrapper_type(type_id TypeID) bool {
	if type_id <= 0 || type_id >= b.mod.type_store.types.len {
		return false
	}
	typ := b.mod.type_store.types[type_id]
	return typ.kind == .struct_t && typ.field_names.len >= 2 && typ.field_names[0] == 'state'
		&& typ.field_names[1] == 'err'
}

fn (b &Builder) is_result_wrapper_type(type_id TypeID) bool {
	if type_id <= 0 || type_id >= b.mod.type_store.types.len {
		return false
	}
	typ := b.mod.type_store.types[type_id]
	return typ.kind == .struct_t && typ.field_names.len >= 2 && typ.field_names[0] == 'is_error'
		&& typ.field_names[1] == 'err'
}

fn (b &Builder) is_wrapper_type(type_id TypeID) bool {
	return b.is_option_wrapper_type(type_id) || b.is_result_wrapper_type(type_id)
}

fn (b &Builder) wrapper_has_data(type_id TypeID) bool {
	if type_id <= 0 || type_id >= b.mod.type_store.types.len {
		return false
	}
	typ := b.mod.type_store.types[type_id]
	return typ.kind == .struct_t && typ.field_names.len >= 3 && typ.field_names[2] == 'data'
}

fn (b &Builder) wrapper_data_type(type_id TypeID) TypeID {
	if !b.wrapper_has_data(type_id) {
		return 0
	}
	return b.mod.type_store.types[type_id].fields[2]
}

fn (b &Builder) current_fn_return_type() TypeID {
	if b.cur_func >= 0 && b.cur_func < b.mod.funcs.len {
		return b.mod.funcs[b.cur_func].typ
	}
	return 0
}

fn (mut b Builder) build_unwrapped_postfix(expr ast.PostfixExpr, wrapped_val ValueID) ValueID {
	if wrapped_val <= 0 || wrapped_val >= b.mod.values.len {
		return wrapped_val
	}
	wrapped_type := b.mod.values[wrapped_val].typ
	if !b.is_wrapper_type(wrapped_type) {
		return wrapped_val
	}
	wrapper_info := b.mod.type_store.types[wrapped_type]
	i32_t := b.mod.type_store.get_int(32)
	bool_t := b.mod.type_store.get_int(1)
	flag_idx := b.mod.get_or_add_const(i32_t, '0')
	err_idx := b.mod.get_or_add_const(i32_t, '1')
	flag_type := wrapper_info.fields[0]
	flag_val := b.mod.add_instr(.extractvalue, b.cur_block, flag_type, [wrapped_val, flag_idx])
	mut fail_cond := flag_val
	if b.is_option_wrapper_type(wrapped_type) {
		zero_flag := b.mod.get_or_add_const(flag_type, '0')
		fail_cond = b.mod.add_instr(.ne, b.cur_block, bool_t, [flag_val, zero_flag])
	}
	fail_block := b.mod.add_block(b.cur_func, 'postfix_fail')
	ok_block := b.mod.add_block(b.cur_func, 'postfix_ok')
	b.mod.add_instr(.br, b.cur_block, 0,
		[fail_cond, b.mod.blocks[fail_block].val_id, b.mod.blocks[ok_block].val_id])
	b.add_edge(b.cur_block, fail_block)
	b.add_edge(b.cur_block, ok_block)

	b.cur_block = fail_block
	fn_ret_type := b.current_fn_return_type()
	if fn_ret_type != 0 && b.is_wrapper_type(fn_ret_type) {
		if fn_ret_type == wrapped_type {
			b.mod.add_instr(.ret, b.cur_block, 0, [wrapped_val])
		} else {
			err_type := if wrapper_info.fields.len > 1 {
				wrapper_info.fields[1]
			} else {
				b.get_ierror_storage_type()
			}
			err_val := b.mod.add_instr(.extractvalue, b.cur_block, err_type, [
				wrapped_val,
				err_idx,
			])
			propagated := b.build_wrapper_value(fn_ret_type, false, err_val, false)
			b.mod.add_instr(.ret, b.cur_block, 0, [propagated])
		}
	} else {
		panic_name := if 'builtin__panic' in b.fn_index { 'builtin__panic' } else { 'panic' }
		if panic_name in b.fn_index {
			panic_ref := b.get_or_create_fn_ref(panic_name, 0)
			panic_msg := b.build_string_literal(ast.StringLiteral{
				kind:  .v
				value: if expr.op == .not {
					"'postfix ! unwrap failed'"
				} else {
					"'postfix ? unwrap failed'"
				}
			})
			b.mod.add_instr(.call, b.cur_block, 0, [panic_ref, panic_msg])
		}
		b.mod.add_instr(.unreachable, b.cur_block, 0, []ValueID{})
	}

	b.cur_block = ok_block
	if !b.wrapper_has_data(wrapped_type) {
		return b.mod.get_or_add_const(b.mod.type_store.get_int(32), '0')
	}
	data_type := b.wrapper_data_type(wrapped_type)
	data_idx := b.mod.get_or_add_const(i32_t, '2')
	return b.mod.add_instr(.extractvalue, b.cur_block, data_type, [wrapped_val, data_idx])
}

fn (b &Builder) is_none_expr(expr ast.Expr) bool {
	match expr {
		ast.Ident {
			return expr.name == 'none'
		}
		ast.Keyword {
			return expr.tok == .key_none
		}
		ast.Type {
			return expr is ast.NoneType
		}
		else {
			return false
		}
	}
}

fn (b &Builder) is_error_expr(expr ast.Expr) bool {
	error_fn_names := ['error', 'error_posix', 'error_with_code', 'error_win32']
	match expr {
		ast.Ident {
			return expr.name == 'err'
		}
		ast.CallExpr {
			return expr.lhs is ast.Ident && expr.lhs.name in error_fn_names
		}
		ast.CallOrCastExpr {
			return expr.lhs is ast.Ident && expr.lhs.name in error_fn_names
		}
		else {
			return false
		}
	}
}

fn (b &Builder) module_scope_has_direct_type(module_name string, type_name string) bool {
	if b.env == unsafe { nil } {
		return false
	}
	if scope := b.env.get_scope(module_name) {
		if _ := scope.lookup_type(type_name) {
			return true
		}
	}
	return false
}

fn (b &Builder) short_type_name_is_unshadowed_builtin(name string) bool {
	if name == '' || !b.module_scope_has_direct_type('builtin', name) {
		return false
	}
	if b.cur_module != '' && b.cur_module != 'builtin'
		&& b.module_scope_has_direct_type(b.cur_module, name) {
		return false
	}
	return true
}

fn (b &Builder) type_name_is_builtin_ierror(name string) bool {
	if name == 'builtin__IError' || name == 'builtin.IError' {
		return b.module_scope_has_direct_type('builtin', 'IError')
	}
	if name == 'IError' {
		return b.short_type_name_is_unshadowed_builtin('IError')
	}
	return false
}

fn (b &Builder) type_name_is_builtin_error(name string) bool {
	if name == 'builtin__Error' || name == 'builtin.Error' {
		return b.module_scope_has_direct_type('builtin', 'Error')
	}
	if name == 'Error' {
		return b.short_type_name_is_unshadowed_builtin('Error')
	}
	return false
}

fn (mut b Builder) type_is_ierror_like(typ types.Type) bool {
	unwrapped := b.unwrap_alias_type(typ)
	match unwrapped {
		types.Interface {
			return b.type_name_is_builtin_ierror(unwrapped.name)
		}
		types.Pointer {
			return b.type_is_ierror_like(unwrapped.base_type)
		}
		types.Struct {
			if b.type_name_is_builtin_error(unwrapped.name) {
				return true
			}
			for embedded in unwrapped.embedded {
				if b.type_name_is_builtin_error(embedded.name) {
					return true
				}
			}
			return false
		}
		else {
			return false
		}
	}
}

fn (mut b Builder) expr_is_ierror_like(expr ast.Expr) bool {
	typ := b.get_checked_expr_type(expr) or { return false }
	return b.type_is_ierror_like(typ)
}

fn (mut b Builder) cursor_is_ierror_like(c ast.Cursor) bool {
	typ := b.get_checked_expr_type_from_flat(c) or { return false }
	return b.type_is_ierror_like(typ)
}

fn (mut b Builder) type_is_ierror_interface(typ types.Type) bool {
	unwrapped := b.unwrap_alias_type(typ)
	match unwrapped {
		types.Interface {
			return b.type_name_is_builtin_ierror(unwrapped.name)
		}
		else {
			return false
		}
	}
}

fn (mut b Builder) type_is_concrete_ierror(typ types.Type) bool {
	unwrapped := b.unwrap_alias_type(typ)
	match unwrapped {
		types.Pointer {
			return b.type_is_concrete_ierror(unwrapped.base_type)
		}
		types.Struct {
			if b.type_name_is_builtin_error(unwrapped.name) {
				return true
			}
			for embedded in unwrapped.embedded {
				if b.type_name_is_builtin_error(embedded.name) {
					return true
				}
			}
			return false
		}
		else {
			return false
		}
	}
}

fn (mut b Builder) checked_type_for_ssa_type(type_id TypeID) ?types.Type {
	if !b.valid_type_id(type_id) {
		return none
	}
	type_name := b.mod.c_struct_names[int(type_id)] or { return none }
	return b.lookup_checked_type_by_name(type_name)
}

fn (mut b Builder) ierror_tag_for_checked_type(typ types.Type) ?ValueID {
	if !b.type_is_concrete_ierror(typ) {
		return none
	}
	unwrapped := b.unwrap_alias_type(typ)
	concrete_type := match unwrapped {
		types.Pointer {
			b.unwrap_alias_type(unwrapped.base_type)
		}
		else {
			unwrapped
		}
	}

	type_id := b.type_to_ssa(concrete_type)
	if !b.valid_type_id(type_id) || int(type_id) == 0 {
		return none
	}
	i64_t := b.mod.type_store.get_int(64)
	return b.mod.get_or_add_const(i64_t, int(type_id).str())
}

fn (mut b Builder) ierror_tag_for_ssa_type(type_id TypeID) ?ValueID {
	typ := b.checked_type_for_ssa_type(type_id) or { return none }
	return b.ierror_tag_for_checked_type(typ)
}

fn (mut b Builder) ierror_tag_for_type_expr(expr ast.Expr) ?ValueID {
	type_id := b.ast_type_to_ssa(expr)
	return b.ierror_tag_for_ssa_type(type_id)
}

fn (mut b Builder) ierror_tag_for_type_cursor(c ast.Cursor) ?ValueID {
	type_id := b.ast_type_to_ssa_from_flat(c)
	return b.ierror_tag_for_ssa_type(type_id)
}

fn (mut b Builder) type_expr_is_ierror_interface(expr ast.Expr) bool {
	name := match expr {
		ast.Ident {
			expr.name
		}
		ast.SelectorExpr {
			expr.name().replace('.', '__')
		}
		else {
			''
		}
	}

	if b.type_name_is_builtin_ierror(name) {
		return true
	}
	if name != '' {
		return false
	}
	typ := b.get_checked_expr_type(expr) or { return false }
	return b.type_is_ierror_interface(typ)
}

fn (mut b Builder) type_cursor_is_ierror_interface(c ast.Cursor) bool {
	name := match c.kind() {
		.expr_ident {
			c.name()
		}
		.expr_selector {
			lhs := c.edge(0)
			rhs := c.edge(1)
			if lhs.kind() == .expr_ident {
				'${lhs.name()}__${rhs.name()}'
			} else {
				rhs.name()
			}
		}
		else {
			''
		}
	}

	if b.type_name_is_builtin_ierror(name) {
		return true
	}
	if name != '' {
		return false
	}
	typ := b.get_checked_expr_type_from_flat(c) or { return false }
	return b.type_is_ierror_interface(typ)
}

fn (mut b Builder) ierror_tag_for_expr(expr ast.Expr) ?ValueID {
	match expr {
		ast.CallOrCastExpr {
			if b.type_expr_is_ierror_interface(expr.lhs) {
				return b.ierror_tag_for_expr(expr.expr)
			}
		}
		ast.CastExpr {
			if b.type_expr_is_ierror_interface(expr.typ) {
				return b.ierror_tag_for_expr(expr.expr)
			}
		}
		ast.ParenExpr {
			return b.ierror_tag_for_expr(expr.expr)
		}
		ast.ModifierExpr {
			return b.ierror_tag_for_expr(expr.expr)
		}
		else {}
	}

	typ := b.get_checked_expr_type(expr) or { return none }
	return b.ierror_tag_for_checked_type(typ)
}

fn (mut b Builder) ierror_tag_for_cursor(c ast.Cursor) ?ValueID {
	match c.kind() {
		.expr_call_or_cast {
			if b.type_cursor_is_ierror_interface(c.edge(0)) {
				return b.ierror_tag_for_cursor(c.edge(1))
			}
		}
		.expr_cast {
			if b.type_cursor_is_ierror_interface(c.edge(0)) {
				return b.ierror_tag_for_cursor(c.edge(1))
			}
		}
		.expr_paren, .expr_modifier {
			return b.ierror_tag_for_cursor(c.edge(0))
		}
		else {}
	}

	typ := b.get_checked_expr_type_from_flat(c) or { return none }
	return b.ierror_tag_for_checked_type(typ)
}

fn (mut b Builder) build_ierror_concrete_compare(subject ValueID, subject_expr ast.Expr, variant_expr ast.Expr, op token.Token) ?ValueID {
	if op !in [.eq, .ne, .key_is, .not_is] {
		return none
	}
	subject_type := b.get_checked_expr_type(subject_expr) or { return none }
	if !b.type_is_ierror_interface(subject_type) {
		return none
	}
	expected := b.ierror_tag_for_type_expr(variant_expr) or { return none }
	bool_type := b.mod.type_store.get_int(1)
	cmp_op := if op in [.eq, .key_is] { OpCode.eq } else { OpCode.ne }
	return b.mod.add_instr(cmp_op, b.cur_block, bool_type, [subject, expected])
}

fn (mut b Builder) build_ierror_concrete_compare_from_flat(subject ValueID, subject_c ast.Cursor, variant_c ast.Cursor, op token.Token) ?ValueID {
	if op !in [.eq, .ne, .key_is, .not_is] {
		return none
	}
	subject_type := b.get_checked_expr_type_from_flat(subject_c) or { return none }
	if !b.type_is_ierror_interface(subject_type) {
		return none
	}
	expected := b.ierror_tag_for_type_cursor(variant_c) or { return none }
	bool_type := b.mod.type_store.get_int(1)
	cmp_op := if op in [.eq, .key_is] { OpCode.eq } else { OpCode.ne }
	return b.mod.add_instr(cmp_op, b.cur_block, bool_type, [subject, expected])
}

fn (mut b Builder) wrapper_value_is_valid_payload(val ValueID, wrapper_type TypeID) bool {
	if !b.wrapper_has_data(wrapper_type) {
		return false
	}
	if val <= 0 || val >= b.mod.values.len {
		return false
	}
	return b.mod.values[val].typ == b.wrapper_data_type(wrapper_type)
}

fn (mut b Builder) build_wrapper_value(wrapper_type TypeID, success bool, payload ValueID, has_payload bool) ValueID {
	if wrapper_type <= 0 || wrapper_type >= b.mod.type_store.types.len {
		return payload
	}
	wrapper_info := b.mod.type_store.types[wrapper_type]
	if wrapper_info.kind != .struct_t || wrapper_info.field_names.len < 2 {
		return payload
	}
	mut wrapper := b.mod.get_or_add_const(wrapper_type, '0')
	i32_t := b.mod.type_store.get_int(32)
	flag_idx := b.mod.get_or_add_const(i32_t, '0')
	err_idx := b.mod.get_or_add_const(i32_t, '1')
	flag_type := wrapper_info.fields[0]
	flag_val := if b.is_option_wrapper_type(wrapper_type) {
		// V options use state==0 for success and state==2 for none/error.
		b.mod.get_or_add_const(flag_type, if success { '0' } else { '2' })
	} else {
		b.mod.get_or_add_const(flag_type, if success { '0' } else { '1' })
	}
	wrapper = b.mod.add_instr(.insertvalue, b.cur_block, wrapper_type,
		[wrapper, flag_val, flag_idx])
	if !success {
		err_type := wrapper_info.fields[1]
		mut err_val := payload
		if err_val == 0 {
			err_val = b.mod.get_or_add_const(err_type, '0')
		} else if b.mod.values[err_val].typ != err_type {
			err_val = b.cast_value_to_type(err_val, err_type)
		}
		wrapper = b.mod.add_instr(.insertvalue, b.cur_block, wrapper_type,
			[wrapper, err_val, err_idx])
	}
	if has_payload && b.wrapper_has_data(wrapper_type) {
		data_idx := b.mod.get_or_add_const(i32_t, '2')
		data_type := wrapper_info.fields[2]
		mut data_val := payload
		if data_val == 0 {
			data_val = b.mod.get_or_add_const(data_type, '0')
		} else if b.mod.values[data_val].typ != data_type {
			data_val = b.cast_value_to_type(data_val, data_type)
		}
		wrapper = b.mod.add_instr(.insertvalue, b.cur_block, wrapper_type, [wrapper, data_val,
			data_idx])
	}
	return wrapper
}

fn (mut b Builder) coerce_wrapper_value(expr ast.Expr, val ValueID, wrapper_type TypeID) ValueID {
	if !b.is_wrapper_type(wrapper_type) {
		return val
	}
	if b.is_none_expr(expr) {
		return b.build_wrapper_value(wrapper_type, false, 0, false)
	}
	if b.is_error_expr(expr) {
		return b.build_wrapper_value(wrapper_type, false, val, false)
	}
	if b.is_result_wrapper_type(wrapper_type) && b.expr_is_ierror_like(expr)
		&& !b.wrapper_value_is_valid_payload(val, wrapper_type) {
		err_payload := b.ierror_tag_for_expr(expr) or { val }
		return b.build_wrapper_value(wrapper_type, false, err_payload, false)
	}
	if val > 0 && val < b.mod.values.len && b.mod.values[val].typ == wrapper_type {
		if payload := b.wrapper_payload_bitcast_source(val, wrapper_type) {
			return b.build_wrapper_value(wrapper_type, true, payload, true)
		}
		match b.mod.values[val].kind {
			.argument, .global, .instruction {
				return val
			}
			else {}
		}
	}
	return b.build_wrapper_value(wrapper_type, true, val, true)
}

fn (b &Builder) wrapper_payload_bitcast_source(val ValueID, wrapper_type TypeID) ?ValueID {
	if val <= 0 || val >= b.mod.values.len || !b.wrapper_has_data(wrapper_type) {
		return none
	}
	value := b.mod.values[val]
	if value.kind != .instruction || value.typ != wrapper_type {
		return none
	}
	instr := b.mod.instrs[value.index]
	if instr.op != .bitcast || instr.operands.len != 1 {
		return none
	}
	payload := instr.operands[0]
	if payload <= 0 || payload >= b.mod.values.len {
		return none
	}
	payload_type := b.mod.values[payload].typ
	if payload_type == wrapper_type || b.is_wrapper_type(payload_type) {
		return none
	}
	return payload
}

fn (mut b Builder) expr_type(e ast.Expr) TypeID {
	if !builder_expr_ok(e) {
		return b.mod.type_store.get_int(64)
	}
	if typ := b.get_checked_expr_type(e) {
		return b.type_to_ssa(typ)
	}
	// Fallback for literals
	match e {
		ast.BasicLiteral {
			if e.kind == .key_true || e.kind == .key_false {
				return b.mod.type_store.get_int(1)
			}
			if e.kind == .number && (e.value.contains('.')
				|| (!e.value.starts_with('0x') && !e.value.starts_with('0X')
				&& (e.value.contains('e') || e.value.contains('E')))) {
				return b.mod.type_store.get_float(64)
			}
			return b.mod.type_store.get_int(64)
		}
		ast.StringLiteral {
			return b.get_string_type()
		}
		else {
			return b.mod.type_store.get_int(64)
		}
	}
}

fn (mut b Builder) const_field_type(field_name string, value ast.Expr) TypeID {
	if b.env != unsafe { nil } {
		if scope := b.env.get_scope(b.cur_module) {
			if obj := scope.lookup_parent(field_name, 0) {
				obj_typ := obj.typ()
				obj_type := b.type_to_ssa(obj_typ)
				if obj_type != 0 {
					return obj_type
				}
			}
		}
	}
	return b.expr_type(value)
}

// const_field_type_from_flat (s235) is the cursor mirror of const_field_type:
// the scope lookup is name-only, the fallback uses expr_type_from_flat (pos-only).
fn (mut b Builder) const_field_type_from_flat(field_name string, value_c ast.Cursor) TypeID {
	if b.env != unsafe { nil } {
		if scope := b.env.get_scope(b.cur_module) {
			if obj := scope.lookup_parent(field_name, 0) {
				obj_typ := obj.typ()
				obj_type := b.type_to_ssa(obj_typ)
				if obj_type != 0 {
					return obj_type
				}
			}
		}
	}
	return b.expr_type_from_flat(value_c)
}

fn (mut b Builder) scope_field_type(field_name string) TypeID {
	if b.env != unsafe { nil } {
		if scope := b.env.get_scope(b.cur_module) {
			if obj := scope.lookup_parent(field_name, 0) {
				obj_typ := obj.typ()
				obj_type := b.type_to_ssa(obj_typ)
				if obj_type != 0 {
					return obj_type
				}
			}
		}
	}
	return 0
}

fn (mut b Builder) global_field_type(field ast.FieldDecl) TypeID {
	scope_type := b.scope_field_type(field.name)
	if scope_type != 0 {
		return scope_type
	}
	if builder_expr_ok(field.typ) && field.typ !is ast.EmptyExpr {
		return b.ast_type_to_ssa(field.typ)
	}
	if field.value is ast.ArrayInitExpr {
		arr := field.value as ast.ArrayInitExpr
		if builder_expr_ok(arr.typ) && arr.typ !is ast.EmptyExpr {
			return b.ast_type_to_ssa(arr.typ)
		}
	}
	if builder_expr_ok(field.value) && field.value !is ast.EmptyExpr {
		return b.expr_type(field.value)
	}
	return b.mod.type_store.get_int(64)
}

// global_field_type_from_flat (s237) is the cursor mirror of global_field_type.
// field_c is an .aux_field_decl: name in name_id, typ at edge(0), value at edge(1).
// `field.typ !is ast.EmptyExpr` maps to `edge(0).kind() != .expr_empty`; the
// ArrayInitExpr.typ check uses value_c.edge(0). scope/ssa/expr lookups identical.
fn (mut b Builder) global_field_type_from_flat(field_c ast.Cursor) TypeID {
	scope_type := b.scope_field_type(field_c.name())
	if scope_type != 0 {
		return scope_type
	}
	typ_c := field_c.edge(0)
	if typ_c.kind() != .expr_empty {
		return b.ast_type_to_ssa_from_flat(typ_c)
	}
	value_c := field_c.edge(1)
	if value_c.kind() == .expr_array_init {
		arr_typ_c := value_c.edge(0)
		if arr_typ_c.kind() != .expr_empty {
			return b.ast_type_to_ssa_from_flat(arr_typ_c)
		}
	}
	if value_c.kind() != .expr_empty {
		return b.expr_type_from_flat(value_c)
	}
	return b.mod.type_store.get_int(64)
}

fn (mut b Builder) types_type_c_name(t types.Type) string {
	if !types.type_has_valid_payload(t) {
		return 'unknown'
	}
	match t {
		types.Primitive {
			if t.props.has(.boolean) {
				return 'bool'
			}
			if t.props.has(.float) {
				return if t.size == 32 { 'f32' } else { 'f64' }
			}
			if t.props.has(.integer) {
				if t.props.has(.untyped) {
					return 'int'
				}
				size := if t.size == 0 { 32 } else { int(t.size) }
				is_signed := !t.props.has(.unsigned)
				return if is_signed {
					match size {
						8 { 'i8' }
						16 { 'i16' }
						64 { 'i64' }
						else { 'int' }
					}
				} else {
					match size {
						8 { 'u8' }
						16 { 'u16' }
						32 { 'u32' }
						else { 'u64' }
					}
				}
			}
			return 'int'
		}
		types.Pointer {
			return b.types_type_c_name(t.base_type) + '*'
		}
		types.String {
			return 'string'
		}
		types.Struct {
			return t.name
		}
		types.Enum {
			return t.name
		}
		types.Void {
			return 'void'
		}
		types.Char {
			return 'char'
		}
		types.Alias {
			base := b.resolve_alias_base_type(t) or {
				return if t.name != '' { t.name } else { 'unknown' }
			}
			return b.types_type_c_name(base)
		}
		types.Array {
			// []rune → Array_rune, []int → Array_int, etc.
			return 'Array_${b.types_type_c_name(t.elem_type)}'
		}
		types.Rune {
			return 'rune'
		}
		types.SumType {
			return t.name
		}
		types.Interface {
			return t.name
		}
		types.FnType {
			return 'FnType'
		}
		types.Map {
			return 'map'
		}
		types.OptionType {
			// Unwrap Option to base type for method resolution
			// (e.g., r.str() where r was unwrapped from ?int should resolve to int__str)
			return b.types_type_c_name(t.base_type)
		}
		types.ResultType {
			// Unwrap Result to base type for method resolution
			return b.types_type_c_name(t.base_type)
		}
		types.ArrayFixed {
			return 'ArrayFixed'
		}
		else {
			return 'int'
		}
	}
}

// --- Phase 1: Register types ---

// Pass 1: Register struct names as forward declarations (empty structs),
// enums, and sumtypes. This ensures all struct names are in struct_types
// before any field types are resolved.
fn (mut b Builder) register_types_pass1(file ast.File) {
	for stmt in file.stmts {
		match stmt {
			ast.StructDecl {
				b.register_struct_name(stmt)
			}
			ast.EnumDecl {
				b.register_enum(stmt)
			}
			ast.TypeDecl {
				if stmt.variants.len > 0 {
					b.register_sumtype(stmt)
				}
			}
			else {}
		}
	}
}

// register_types_pass1_from_flat is the flat-cursor counterpart of
// `register_types_pass1`. It walks one file's top-level stmts via FileCursor
// without rehydrating the file, and rehydrates only the StructDecl /
// EnumDecl / TypeDecl nodes via `flat.decode_stmt`. ModuleStmt, FnDecl,
// ConstDecl, GlobalDecl, ImportStmt, etc. are skipped entirely — never
// decoded — which is the actual allocation reduction over the legacy walker.
fn (mut b Builder) register_types_pass1_from_flat(file_cursor ast.FileCursor) {
	stmts := file_cursor.stmts()
	for si in 0 .. stmts.len() {
		c := stmts.at(si)
		match c.kind() {
			.stmt_struct_decl {
				// s239: cursor-native struct-name registration — no decode_stmt.
				b.register_struct_name_from_flat(c)
			}
			.stmt_enum_decl {
				// s238: cursor-native enum registration — no decode_stmt.
				b.register_enum_from_flat(c)
			}
			.stmt_type_decl {
				// s240: cursor-native sumtype registration — no decode_stmt.
				// TypeDecl variants list is edge3 (list_at(3)).
				if c.list_at(3).len() > 0 {
					b.register_sumtype_from_flat(c)
				}
			}
			else {}
		}
	}
}

// Pass 2: Fill in struct field types. All struct names are now registered,
// so cross-module struct references (e.g., &scanner.Scanner in Parser)
// resolve correctly to the struct type instead of falling back to i64.
fn (mut b Builder) register_types_pass2(file ast.File) {
	b.module_import_aliases = module_import_aliases_from_imports(file.imports)
	nstmts := file.stmts.len
	for si in 0 .. nstmts {
		stmt := file.stmts[si]
		if stmt is ast.StructDecl {
			b.register_struct_fields(stmt)
		}
	}
}

// register_types_pass2_from_flat is the flat-cursor counterpart of
// `register_types_pass2`. Same shape as `register_types_pass1_from_flat`
// (s169) but filtered to `.stmt_struct_decl` only — pass2 only fills in
// struct field types. Non-StructDecl stmts (ModuleStmt, FnDecl, ConstDecl,
// EnumDecl, TypeDecl, etc.) are never decoded.
fn (mut b Builder) register_types_pass2_from_flat(file_cursor ast.FileCursor) {
	b.module_import_aliases =
		module_import_aliases_from_imports(file_cursor.imports().import_stmts())
	stmts := file_cursor.stmts()
	for si in 0 .. stmts.len() {
		c := stmts.at(si)
		if c.kind() == .stmt_struct_decl {
			// s239: cursor-native struct-fields registration — no decode_stmt.
			b.register_struct_fields_from_flat(c)
		}
	}
}

fn (mut b Builder) struct_mangled_name(decl ast.StructDecl) string {
	if !ssa_string_ok(decl.name) || decl.name == '' {
		return ''
	}
	return if b.cur_module == 'builtin'
		&& decl.name in ['array', 'string', 'map', 'DenseArray', 'IError', 'Error', 'MessageError', 'None__', '_option', '_result', 'Option'] {
		decl.name
	} else if b.cur_module != '' && b.cur_module != 'main' {
		'${b.cur_module}__${decl.name}'
	} else {
		decl.name
	}
}

// struct_mangled_name_from_flat (s239) is the cursor mirror of struct_mangled_name.
// The struct name is the cursor's name_id.
fn (mut b Builder) struct_mangled_name_from_flat(c ast.Cursor) string {
	name_str := c.name()
	if !ssa_string_ok(name_str) || name_str == '' {
		return ''
	}
	return if b.cur_module == 'builtin'
		&& name_str in ['array', 'string', 'map', 'DenseArray', 'IError', 'Error', 'MessageError', 'None__', '_option', '_result', 'Option'] {
		name_str
	} else if b.cur_module != '' && b.cur_module != 'main' {
		'${b.cur_module}__${name_str}'
	} else {
		name_str
	}
}

// register_struct_name registers a struct name with an empty struct type.
// The fields will be filled in by register_struct_fields in pass 2.
fn (mut b Builder) register_struct_name(decl ast.StructDecl) {
	name := b.struct_mangled_name(decl)
	if name == '' || !ssa_string_ok(name) {
		return
	}

	if name in b.struct_types {
		return
	}

	type_id := b.mod.type_store.register(Type{
		kind:     .struct_t
		is_union: decl.is_union
	})
	b.struct_types[name] = type_id
	b.mod.c_struct_names[type_id] = name
}

// register_struct_name_from_flat (s239) is the cursor mirror of register_struct_name.
// StructDecl flat = (.stmt_struct_decl, name in name_id, is_union in flags,
// [edge0=attrs, edge1=implements, edge2=embedded, edge3=generic_params, edge4=fields]).
fn (mut b Builder) register_struct_name_from_flat(c ast.Cursor) {
	name := b.struct_mangled_name_from_flat(c)
	if name == '' || !ssa_string_ok(name) {
		return
	}
	if name in b.struct_types {
		return
	}
	type_id := b.mod.type_store.register(Type{
		kind:     .struct_t
		is_union: c.flag(ast.flag_is_union)
	})
	b.struct_types[name] = type_id
	b.mod.c_struct_names[type_id] = name
}

// register_struct_fields fills in the field types for a previously forward-declared struct.
fn (mut b Builder) register_struct_fields(decl ast.StructDecl) {
	name := b.struct_mangled_name(decl)
	if name == '' || !ssa_string_ok(name) {
		return
	}

	type_id := b.struct_types[name] or { return }
	n_embedded := decl.embedded.len

	// Skip if fields are already populated (e.g., builtin types registered in Phase 1a)
	if b.mod.type_store.types[type_id].fields.len > 0 && n_embedded == 0 {
		return
	}

	mut field_types := []TypeID{}
	mut field_names := []string{}
	n_fields := decl.fields.len
	b.struct_embedded_spans.delete(int(type_id))

	// Flatten embedded struct fields first (e.g., ObjectCommon in Const)
	if n_embedded > 0 {
		b.record_embedded_field_spans(type_id, decl.embedded, field_names.len)
		b.collect_embedded_fields(decl.embedded, mut field_names, mut field_types)
	}

	for fi in 0 .. n_fields {
		ft := b.ast_type_to_ssa(decl.fields[fi].typ)
		field_types << ft
		field_name := decl.fields[fi].name
		field_names << if ssa_string_ok(field_name) { field_name } else { '' }
		b.record_struct_field_array_elem_type(type_id, field_name, decl.fields[fi].typ, ft)
	}
	b.mod.type_store.types[type_id] = Type{
		kind:        .struct_t
		fields:      field_types
		field_names: field_names
		is_union:    decl.is_union
	}
}

// register_struct_fields_from_flat (s239) is the cursor mirror of
// register_struct_fields. embedded = c.list_at(2); fields = c.list_at(4)
// (.aux_field_decl: name in name_id, typ at edge0).
fn (mut b Builder) register_struct_fields_from_flat(c ast.Cursor) {
	name := b.struct_mangled_name_from_flat(c)
	if name == '' || !ssa_string_ok(name) {
		return
	}

	type_id := b.struct_types[name] or { return }
	embedded := c.list_at(2)

	// Skip if fields are already populated (e.g., builtin types registered in Phase 1a)
	if b.mod.type_store.types[type_id].fields.len > 0 && embedded.len() == 0 {
		return
	}

	mut field_types := []TypeID{}
	mut field_names := []string{}
	fields := c.list_at(4)
	b.struct_embedded_spans.delete(int(type_id))

	// Flatten embedded struct fields first (e.g., ObjectCommon in Const)
	if embedded.len() > 0 {
		b.record_embedded_field_spans_from_flat(type_id, embedded, field_names.len)
		b.collect_embedded_fields_from_flat(embedded, mut field_names, mut field_types)
	}

	for fi in 0 .. fields.len() {
		field_c := fields.at(fi)
		typ_c := field_c.edge(0)
		ft := b.ast_type_to_ssa_from_flat(typ_c)
		field_types << ft
		field_name := field_c.name()
		field_names << if ssa_string_ok(field_name) { field_name } else { '' }
		b.record_struct_field_array_elem_type_from_flat(type_id, field_name, typ_c, ft)
	}
	b.mod.type_store.types[type_id] = Type{
		kind:        .struct_t
		fields:      field_types
		field_names: field_names
		is_union:    c.flag(ast.flag_is_union)
	}
}

// register_struct is the legacy combined registration (used for Phase 1a core types).
fn (mut b Builder) register_struct(decl ast.StructDecl) {
	name := b.struct_mangled_name(decl)
	if name == '' || !ssa_string_ok(name) {
		return
	}

	if name in b.struct_types {
		return
	}

	mut field_types := []TypeID{}
	mut field_names := []string{}

	// Flatten embedded struct fields first
	b.collect_embedded_fields(decl.embedded, mut field_names, mut field_types)

	for fi in 0 .. decl.fields.len {
		ft := b.ast_type_to_ssa(decl.fields[fi].typ)
		field_types << ft
		field_name := decl.fields[fi].name
		field_names << if ssa_string_ok(field_name) { field_name } else { '' }
		// This combined path registers the struct after fields are collected,
		// so array element metadata is recorded below once type_id exists.
	}

	type_id := b.mod.type_store.register(Type{
		kind:        .struct_t
		fields:      field_types
		field_names: field_names
		is_union:    decl.is_union
	})
	b.struct_types[name] = type_id
	b.mod.c_struct_names[type_id] = name
	for fi in 0 .. decl.fields.len {
		field_name := decl.fields[fi].name
		if ssa_string_ok(field_name) {
			b.record_struct_field_array_elem_type(type_id, field_name, decl.fields[fi].typ,
				field_types[fi])
		}
	}
}

// register_struct_from_flat (s239) is the cursor mirror of register_struct
// (the combined name+fields path). embedded = c.list_at(2); fields = c.list_at(4).
// The second loop's `field_types[fi]` indexing matches the AST exactly (fi indexes
// the decl fields but field_types has the embedded fields prepended).
fn (mut b Builder) register_struct_from_flat(c ast.Cursor) {
	name := b.struct_mangled_name_from_flat(c)
	if name == '' || !ssa_string_ok(name) {
		return
	}
	if name in b.struct_types {
		return
	}

	mut field_types := []TypeID{}
	mut field_names := []string{}
	embedded := c.list_at(2)
	fields := c.list_at(4)

	// Flatten embedded struct fields first
	b.collect_embedded_fields_from_flat(embedded, mut field_names, mut field_types)

	for fi in 0 .. fields.len() {
		typ_c := fields.at(fi).edge(0)
		ft := b.ast_type_to_ssa_from_flat(typ_c)
		field_types << ft
		field_name := fields.at(fi).name()
		field_names << if ssa_string_ok(field_name) { field_name } else { '' }
	}

	type_id := b.mod.type_store.register(Type{
		kind:        .struct_t
		fields:      field_types
		field_names: field_names
		is_union:    c.flag(ast.flag_is_union)
	})
	b.struct_types[name] = type_id
	b.mod.c_struct_names[type_id] = name
	for fi in 0 .. fields.len() {
		field_c := fields.at(fi)
		field_name := field_c.name()
		if ssa_string_ok(field_name) {
			b.record_struct_field_array_elem_type_from_flat(type_id, field_name, field_c.edge(0),
				field_types[fi])
		}
	}
}

fn (mut b Builder) embedded_type_id_from_expr(emb ast.Expr) ?TypeID {
	if emb is ast.Ident {
		if !ssa_string_ok(emb.name) {
			return none
		}
		return b.lookup_embedded_type_id_by_name(emb.name)
	}
	if emb is ast.SelectorExpr && emb.lhs is ast.Ident {
		lhs_name := (emb.lhs as ast.Ident).name
		if !ssa_string_ok(lhs_name) || !ssa_string_ok(emb.rhs.name) {
			return none
		}
		if type_id := b.selector_type_name_to_ssa(lhs_name, emb.rhs.name) {
			if b.valid_type_id(type_id) {
				return type_id
			}
		}
		if resolved_mod := b.module_import_aliases[lhs_name] or {
			b.selector_module_name_for_ident(lhs_name) or { '' }
		}
		{
			if resolved_mod == '' {
				return none
			}
			qualified := imported_symbol_fn_name(resolved_mod, emb.rhs.name)
			if type_id := b.struct_types[qualified] {
				return type_id
			}
			if type_id := b.selector_type_name_to_ssa(resolved_mod, emb.rhs.name) {
				if b.valid_type_id(type_id) {
					return type_id
				}
			}
		}
	}
	return none
}

fn (mut b Builder) embedded_type_id_from_flat(emb ast.Cursor) ?TypeID {
	if emb.kind() == .expr_ident {
		n := emb.name()
		if !ssa_string_ok(n) {
			return none
		}
		return b.lookup_embedded_type_id_by_name(n)
	}
	if emb.kind() == .expr_selector && emb.edge(0).kind() == .expr_ident {
		lhs_name := emb.edge(0).name()
		rhs_name := emb.edge(1).name()
		if !ssa_string_ok(lhs_name) || !ssa_string_ok(rhs_name) {
			return none
		}
		if type_id := b.selector_type_name_to_ssa(lhs_name, rhs_name) {
			if b.valid_type_id(type_id) {
				return type_id
			}
		}
		if resolved_mod := b.module_import_aliases[lhs_name] or {
			b.selector_module_name_for_ident(lhs_name) or { '' }
		}
		{
			if resolved_mod == '' {
				return none
			}
			qualified := imported_symbol_fn_name(resolved_mod, rhs_name)
			if type_id := b.struct_types[qualified] {
				return type_id
			}
			if type_id := b.selector_type_name_to_ssa(resolved_mod, rhs_name) {
				if b.valid_type_id(type_id) {
					return type_id
				}
			}
		}
	}
	return none
}

fn (mut b Builder) lookup_embedded_type_id_by_name(emb_name string) ?TypeID {
	if b.cur_module != '' && b.cur_module != 'main' {
		qualified := '${b.cur_module}__${emb_name}'
		if qualified in b.struct_types {
			return b.struct_types[qualified]
		}
	}
	if emb_name in b.struct_types {
		return b.struct_types[emb_name]
	}
	return none
}

fn (mut b Builder) record_embedded_field_spans(owner_type TypeID, embedded []ast.Expr, start_idx int) {
	mut offset := start_idx
	for emb in embedded {
		emb_type_id := b.embedded_type_id_from_expr(emb) or { continue }
		if emb_type_id <= 0 || emb_type_id >= b.mod.type_store.types.len {
			continue
		}
		emb_typ := b.mod.type_store.types[emb_type_id]
		if emb_typ.kind != .struct_t || emb_typ.field_names.len == 0 {
			continue
		}
		mut spans := b.struct_embedded_spans[int(owner_type)] or { []EmbeddedFieldSpan{} }
		spans << EmbeddedFieldSpan{
			source_type: emb_type_id
			start:       offset
			len:         emb_typ.field_names.len
		}
		b.struct_embedded_spans[int(owner_type)] = spans
		offset += emb_typ.field_names.len
	}
}

fn (mut b Builder) record_embedded_field_spans_from_flat(owner_type TypeID, embedded ast.CursorList, start_idx int) {
	mut offset := start_idx
	for ei in 0 .. embedded.len() {
		emb_type_id := b.embedded_type_id_from_flat(embedded.at(ei)) or { continue }
		if emb_type_id <= 0 || emb_type_id >= b.mod.type_store.types.len {
			continue
		}
		emb_typ := b.mod.type_store.types[emb_type_id]
		if emb_typ.kind != .struct_t || emb_typ.field_names.len == 0 {
			continue
		}
		mut spans := b.struct_embedded_spans[int(owner_type)] or { []EmbeddedFieldSpan{} }
		spans << EmbeddedFieldSpan{
			source_type: emb_type_id
			start:       offset
			len:         emb_typ.field_names.len
		}
		b.struct_embedded_spans[int(owner_type)] = spans
		offset += emb_typ.field_names.len
	}
}

// collect_embedded_fields resolves embedded type expressions and adds their
// flattened fields to the field_names and field_types lists.
// Embedded structs (e.g., `ObjectCommon` in `struct Const { ObjectCommon; int_val int }`)
// have their fields in a separate `embedded` list in the AST StructDecl.
// This function looks up each embedded type in struct_types and prepends its fields.
fn (mut b Builder) collect_embedded_fields(embedded []ast.Expr, mut field_names []string, mut field_types []TypeID) {
	for emb in embedded {
		emb_type_id := b.embedded_type_id_from_expr(emb) or { continue }
		if emb_type_id < b.mod.type_store.types.len {
			emb_typ := b.mod.type_store.types[emb_type_id]
			if emb_typ.kind == .struct_t && emb_typ.field_names.len > 0 {
				for i, fname in emb_typ.field_names {
					field_names << fname
					if i < emb_typ.fields.len {
						field_types << emb_typ.fields[i]
					} else {
						field_types << b.mod.type_store.get_int(64)
					}
				}
			}
		}
	}
}

// collect_embedded_fields_from_flat (s239) is the cursor mirror of
// collect_embedded_fields. Each embedded entry is an `.expr_ident` (`emb.name()`)
// or an `.expr_selector` with an Ident lhs (`edge(0).name()` / rhs `edge(1).name()`).
// The struct_types lookup + field-copy logic is identical (type-store reads only).
fn (mut b Builder) collect_embedded_fields_from_flat(embedded ast.CursorList, mut field_names []string, mut field_types []TypeID) {
	for ei in 0 .. embedded.len() {
		emb := embedded.at(ei)
		emb_type_id := b.embedded_type_id_from_flat(emb) or { continue }
		if emb_type_id < b.mod.type_store.types.len {
			emb_typ := b.mod.type_store.types[emb_type_id]
			if emb_typ.kind == .struct_t && emb_typ.field_names.len > 0 {
				for i, fname in emb_typ.field_names {
					field_names << fname
					if i < emb_typ.fields.len {
						field_types << emb_typ.fields[i]
					} else {
						field_types << b.mod.type_store.get_int(64)
					}
				}
			}
		}
	}
}

fn (mut b Builder) register_enum(decl ast.EnumDecl) {
	if !ssa_string_ok(decl.name) || decl.name == '' {
		return
	}
	name := if b.cur_module != '' && b.cur_module != 'main' {
		'${b.cur_module}__${decl.name}'
	} else {
		decl.name
	}
	if !ssa_string_ok(name) {
		return
	}

	is_flag := decl.attributes.has('flag')
	for i, field in decl.fields {
		if !ssa_string_ok(field.name) || field.name == '' {
			continue
		}
		key := '${name}__${enum_field_symbol_name(field.name)}'
		if is_flag {
			// @[flag] enums use power-of-2 values: 1, 2, 4, 8, ...
			b.enum_values[key] = 1 << i
		} else {
			b.enum_values[key] = i
		}
	}
}

// register_enum_from_flat (s238) is the cursor mirror of register_enum. EnumDecl
// flat = (.stmt_enum_decl, name in name_id, [edge0=as_type, edge1=attrs,
// edge2=fields]). Values come from the field INDEX (matching the AST, which
// ignores field.value), so the `flag` power-of-2 numbering and the skip-on-bad-
// name behaviour are index-identical. `flag` attr via cursor_attrs_has (s237).
fn (mut b Builder) register_enum_from_flat(c ast.Cursor) {
	name_str := c.name()
	if !ssa_string_ok(name_str) || name_str == '' {
		return
	}
	name := if b.cur_module != '' && b.cur_module != 'main' {
		'${b.cur_module}__${name_str}'
	} else {
		name_str
	}
	if !ssa_string_ok(name) {
		return
	}

	is_flag := cursor_attrs_has(c.list_at(1), 'flag')
	fields := c.list_at(2)
	for i in 0 .. fields.len() {
		field_name := fields.at(i).name()
		if !ssa_string_ok(field_name) || field_name == '' {
			continue
		}
		key := '${name}__${enum_field_symbol_name(field_name)}'
		if is_flag {
			b.enum_values[key] = 1 << i
		} else {
			b.enum_values[key] = i
		}
	}
}

// is_enum_type checks if a type name corresponds to a registered enum
// by looking for any enum_values key that starts with the name followed by '__'.
fn (b &Builder) is_enum_type(name string) bool {
	prefix := '${name}__'
	for key, _ in b.enum_values {
		if key.starts_with(prefix) {
			return true
		}
	}
	return false
}

fn (mut b Builder) register_type_aliases(file ast.File) {
	for stmt in file.stmts {
		if stmt is ast.TypeDecl && stmt.variants.len == 0 {
			b.register_type_alias(stmt)
		}
	}
}

fn (mut b Builder) register_type_aliases_from_flat(file_cursor ast.FileCursor) {
	stmts := file_cursor.stmts()
	for si in 0 .. stmts.len() {
		c := stmts.at(si)
		if c.kind() == .stmt_type_decl {
			// s236: TypeDecl flat = (.stmt_type_decl, name in name_id,
			// [edge0=base_type, edge1=attrs, edge2=generic_params, edge3=variants]).
			// Aliases have no variants — dispatch cursor-natively, no decode_stmt.
			if c.list_at(3).len() == 0 {
				b.register_type_alias_from_flat(c)
			}
		}
	}
}

fn (mut b Builder) register_type_alias(decl ast.TypeDecl) {
	if !ssa_string_ok(decl.name) || decl.name == '' || !builder_expr_ok(decl.base_type) {
		return
	}
	base_type := b.ast_type_to_ssa(decl.base_type)
	if base_type == 0 {
		return
	}
	name := if b.cur_module != '' && b.cur_module != 'main' {
		'${b.cur_module}__${decl.name}'
	} else {
		decl.name
	}
	if ssa_string_ok(name) {
		b.type_aliases[name] = base_type
	}
	short_mod := ssa_module_tail_name(b.cur_module)
	if short_mod != '' && short_mod != b.cur_module && b.cur_module != 'main' {
		short_name := '${short_mod}__${decl.name}'
		if ssa_string_ok(short_name) {
			b.type_aliases[short_name] = base_type
		}
	}
	if b.cur_module == '' || b.cur_module == 'main' {
		b.type_aliases[decl.name] = base_type
	}
}

// register_type_alias_from_flat (s236) is the cursor mirror of register_type_alias.
// `c` is the .stmt_type_decl cursor: name in name_id, base_type at edge(0). The
// builder_expr_ok corruption guard is unnecessary for a valid cursor; the
// ssa_string_ok / base_type==0 guards and the map writes are identical.
fn (mut b Builder) register_type_alias_from_flat(c ast.Cursor) {
	name_str := c.name()
	if !ssa_string_ok(name_str) || name_str == '' {
		return
	}
	base_type := b.ast_type_to_ssa_from_flat(c.edge(0))
	if base_type == 0 {
		return
	}
	name := if b.cur_module != '' && b.cur_module != 'main' {
		'${b.cur_module}__${name_str}'
	} else {
		name_str
	}
	if ssa_string_ok(name) {
		b.type_aliases[name] = base_type
	}
	short_mod := ssa_module_tail_name(b.cur_module)
	if short_mod != '' && short_mod != b.cur_module && b.cur_module != 'main' {
		short_name := '${short_mod}__${name_str}'
		if ssa_string_ok(short_name) {
			b.type_aliases[short_name] = base_type
		}
	}
	if b.cur_module == '' || b.cur_module == 'main' {
		b.type_aliases[name_str] = base_type
	}
}

fn (mut b Builder) register_sumtype(decl ast.TypeDecl) {
	if decl.variants.len == 0 {
		return
	}
	if !ssa_string_ok(decl.name) || decl.name == '' {
		return
	}
	name := if b.cur_module != '' && b.cur_module != 'main' {
		'${b.cur_module}__${decl.name}'
	} else {
		decl.name
	}
	if !ssa_string_ok(name) {
		return
	}

	if name in b.struct_types {
		return
	}

	i64_t := b.mod.type_store.get_int(64)
	type_id := b.mod.type_store.register(Type{
		kind:        .struct_t
		fields:      [i64_t, i64_t]
		field_names: ['_tag', '_data']
	})
	b.struct_types[name] = type_id
	b.mod.c_struct_names[type_id] = name
}

// register_sumtype_from_flat (s240) is the cursor mirror of register_sumtype.
// `c` is a .stmt_type_decl with variants (variants list = edge3). The variant
// types are not read (the AST also ignores them) — a sum type lowers to a
// {_tag, _data} two-i64 struct. Caller already gates on variants present.
fn (mut b Builder) register_sumtype_from_flat(c ast.Cursor) {
	if c.list_at(3).len() == 0 {
		return
	}
	name_str := c.name()
	if !ssa_string_ok(name_str) || name_str == '' {
		return
	}
	name := if b.cur_module != '' && b.cur_module != 'main' {
		'${b.cur_module}__${name_str}'
	} else {
		name_str
	}
	if !ssa_string_ok(name) {
		return
	}

	if name in b.struct_types {
		return
	}

	i64_t := b.mod.type_store.get_int(64)
	type_id := b.mod.type_store.register(Type{
		kind:        .struct_t
		fields:      [i64_t, i64_t]
		field_names: ['_tag', '_data']
	})
	b.struct_types[name] = type_id
	b.mod.c_struct_names[type_id] = name
}

fn (mut b Builder) register_consts_and_globals(file ast.File) {
	for stmt in file.stmts {
		match stmt {
			ast.ConstDecl {
				b.register_const_decl(stmt)
			}
			ast.GlobalDecl {
				b.register_global_decl(stmt)
			}
			else {}
		}
	}
}

// register_consts_and_globals_from_flat is the flat-cursor counterpart of
// `register_consts_and_globals`. Walks one file's top-level stmts via
// FileCursor and only rehydrates `.stmt_const_decl` / `.stmt_global_decl`
// nodes via `flat.decode_stmt`. ModuleStmt, FnDecl, StructDecl, EnumDecl,
// TypeDecl, ImportStmt etc. are never decoded.
fn (mut b Builder) register_consts_and_globals_from_flat(file_cursor ast.FileCursor) {
	stmts := file_cursor.stmts()
	for si in 0 .. stmts.len() {
		c := stmts.at(si)
		match c.kind() {
			.stmt_const_decl {
				// s242: cursor-native const registration — no decode_stmt.
				b.register_const_decl_from_flat(c)
			}
			.stmt_global_decl {
				// s237: cursor-native global registration — no decode_stmt.
				b.register_global_decl_from_flat(c)
			}
			else {}
		}
	}
}

// register_const_decl extracts the per-ConstDecl body from
// `register_consts_and_globals` so both the legacy walker and the
// flat-cursor port can dispatch through a single implementation.
fn (mut b Builder) register_const_decl(stmt ast.ConstDecl) {
	for field in stmt.fields {
		const_name := if b.cur_module != '' && b.cur_module != 'main' {
			'${b.cur_module}__${field.name}'
		} else {
			field.name
		}
		mut const_type := b.const_field_type(field.name, field.value)
		// Check if this is a string constant - store for inline resolution
		str_val := b.try_eval_const_string(field.value)
		if str_val.len > 0 {
			b.string_const_values[const_name] = str_val
			b.string_const_values[field.name] = str_val
		}
		// Check if this is a float constant - store for inline resolution
		if field.value is ast.BasicLiteral && field.value.kind == .number
			&& (field.value.value.contains('.')
			|| (!field.value.value.starts_with('0x') && !field.value.value.starts_with('0X')
			&& (field.value.value.contains('e') || field.value.value.contains('E')))) {
			b.float_const_values[const_name] = field.value.value
			b.float_const_values[field.name] = field.value.value
		} else if b.is_float_cast_expr(field.value) {
			if fval := b.try_eval_computed_float(field.value) {
				fval_str := fval.str()
				b.float_const_values[const_name] = fval_str
				b.float_const_values[field.name] = fval_str
			}
		}
		initial_value := b.try_eval_const_int(field.value)
		// Detect sum type constants: these are multi-word values that
		// cannot be inlined as a single i64.
		// Case 1: Transformer succeeded → InitExpr{_tag: N, _data: ...}
		// Case 2: Transformer failed → CastExpr{typ: SumType, expr: ...}
		mut is_sumtype_const := false
		if field.value is ast.InitExpr {
			for init_field in field.value.fields {
				if init_field.name == '_tag' {
					is_sumtype_const = true
					break
				}
			}
		}
		if !is_sumtype_const {
			mut cast_type_name := ''
			if field.value is ast.CastExpr {
				if field.value.typ is ast.Ident {
					cast_type_name = field.value.typ.name
				}
			} else if field.value is ast.CallOrCastExpr {
				if field.value.lhs is ast.Ident {
					cast_type_name = field.value.lhs.name
				}
			}
			if cast_type_name != '' {
				qualified_cast := if b.cur_module != '' && b.cur_module != 'main' {
					'${b.cur_module}__${cast_type_name}'
				} else {
					cast_type_name
				}
				for _, check_name in [cast_type_name, qualified_cast] {
					if st_type := b.struct_types[check_name] {
						if int(st_type) < b.mod.type_store.types.len {
							st := b.mod.type_store.types[st_type]
							if st.kind == .struct_t && st.field_names.len >= 2
								&& st.field_names[0] == '_tag' {
								is_sumtype_const = true
								// Fix the global type: use the sum type struct
								// instead of the i64 fallback from expr_type()
								const_type = st_type
								break
							}
						}
					}
				}
			}
		}
		// For sum type constants detected via InitExpr, also fix const_type
		if is_sumtype_const && field.value is ast.InitExpr {
			if field.value.typ is ast.Ident {
				type_name := field.value.typ.name
				if st_type := b.struct_types[type_name] {
					const_type = st_type
				} else {
					// Try with module prefix
					qualified_st := '${b.cur_module}__${type_name}'
					if st_type2 := b.struct_types[qualified_st] {
						const_type = st_type2
					}
				}
			}
		}
		// Detect constant FIXED arrays with all-literal elements.
		if field.value is ast.ArrayInitExpr && field.value.exprs.len > 0 {
			mut is_fixed_array := true
			// Check type environment to see if this is a dynamic array
			if b.env != unsafe { nil } {
				fpos := field.value.pos
				if fpos.id != 0 {
					if ct := b.env.get_expr_type(fpos.id) {
						if ct is types.Array {
							is_fixed_array = false
						}
					}
				}
			}
			arr_data := b.try_serialize_const_array(field.value)
			if arr_data.len > 0 {
				elem_size := arr_data.len / field.value.exprs.len
				is_float_arr := b.is_float_array(field.value)
				elem_type := if is_float_arr {
					b.mod.type_store.get_float(elem_size * 8)
				} else if elem_size == 8 {
					b.mod.type_store.get_int(64)
				} else if elem_size == 4 {
					b.mod.type_store.get_int(32)
				} else if elem_size == 2 {
					b.mod.type_store.get_int(16)
				} else {
					b.mod.type_store.get_int(8)
				}
				if is_fixed_array {
					b.mod.add_global_with_data(const_name, elem_type, true, arr_data)
					b.const_array_globals[const_name] = true
					b.const_array_globals[field.name] = true
					b.const_array_elem_count[const_name] = field.value.exprs.len
					b.const_array_elem_count[field.name] = field.value.exprs.len
					continue
				} else {
					// Dynamic array constant: serialize data, create array struct global
					data_name := '${const_name}__data'
					b.mod.add_global_with_data(data_name, elem_type, true, arr_data)
					b.const_array_globals[data_name] = true
					// Add array struct global (initialized in _vinit)
					arr_struct_type := b.get_array_type()
					b.mod.add_global(const_name, arr_struct_type, false)
					b.dyn_const_arrays << DynConstArray{
						arr_global_name:  const_name
						data_global_name: data_name
						elem_count:       field.value.exprs.len
						elem_size:        elem_size
					}
					continue
				}
			}
		}
		// For float constants, store bit pattern as initial_value
		mut actual_init := initial_value
		if const_name in b.float_const_values {
			f_val := b.float_const_values[const_name].f64()
			actual_init = i64(unsafe { *(&i64(&f_val)) })
			const_type = b.mod.type_store.get_float(64)
		}
		b.mod.add_global_with_value(const_name, const_type, true, actual_init)
		if !is_sumtype_const && (initial_value != 0 || b.is_zero_literal(field.value)) {
			b.const_values[const_name] = initial_value
			b.const_value_types[const_name] = const_type
			// Also store without module prefix for transformer-generated references
			b.const_values[field.name] = initial_value
			b.const_value_types[field.name] = const_type
		}
	}
}

// register_const_decl_from_flat (s242) is the cursor mirror of register_const_decl
// (the last decode_stmt in builder.v). ConstDecl flat = one edge -> aux_list of
// `.aux_field_init` (field name in name_id, value at edge(0)). Uses the s235
// (try_eval_const_int / const_field_type / is_zero_literal) and s241
// (try_eval_const_string / is_float_cast_expr / try_eval_computed_float /
// is_float_array / try_serialize_const_array) cursor helpers. Every branch
// (string / float / int / sumtype-const / fixed+dynamic const-array) mirrors the
// AST exactly; ArrayInitExpr exprs are edges 5..n and InitExpr fields edges 1..n.
fn (mut b Builder) register_const_decl_from_flat(c ast.Cursor) {
	fields := c.list_at(0)
	for fidx in 0 .. fields.len() {
		field_c := fields.at(fidx)
		field_name := field_c.name()
		value_c := field_c.edge(0)
		value_kind := value_c.kind()
		const_name := if b.cur_module != '' && b.cur_module != 'main' {
			'${b.cur_module}__${field_name}'
		} else {
			field_name
		}
		mut const_type := b.const_field_type_from_flat(field_name, value_c)
		// Check if this is a string constant - store for inline resolution
		str_val := b.try_eval_const_string_from_flat(value_c)
		if str_val.len > 0 {
			b.string_const_values[const_name] = str_val
			b.string_const_values[field_name] = str_val
		}
		// Check if this is a float constant - store for inline resolution
		if value_kind == .expr_basic_literal
			&& unsafe { token.Token(int(value_c.aux())) } == .number
			&& (value_c.name().contains('.') || (!value_c.name().starts_with('0x')
			&& !value_c.name().starts_with('0X')
			&& (value_c.name().contains('e') || value_c.name().contains('E')))) {
			b.float_const_values[const_name] = value_c.name()
			b.float_const_values[field_name] = value_c.name()
		} else if b.is_float_cast_expr_from_flat(value_c) {
			if fval := b.try_eval_computed_float_from_flat(value_c) {
				fval_str := fval.str()
				b.float_const_values[const_name] = fval_str
				b.float_const_values[field_name] = fval_str
			}
		}
		initial_value := b.try_eval_const_int_from_flat(value_c)
		// Detect sum type constants (Case 1: InitExpr{_tag,...}; Case 2: cast to sum type).
		mut is_sumtype_const := false
		if value_kind == .expr_init {
			n_init_fields := value_c.edge_count() - 1
			for ifi := 0; ifi < n_init_fields; ifi++ {
				if value_c.edge(1 + ifi).name() == '_tag' {
					is_sumtype_const = true
					break
				}
			}
		}
		if !is_sumtype_const {
			mut cast_type_name := ''
			if value_kind == .expr_cast {
				if value_c.edge(0).kind() == .expr_ident {
					cast_type_name = value_c.edge(0).name()
				}
			} else if value_kind == .expr_call_or_cast {
				if value_c.edge(0).kind() == .expr_ident {
					cast_type_name = value_c.edge(0).name()
				}
			}
			if cast_type_name != '' {
				qualified_cast := if b.cur_module != '' && b.cur_module != 'main' {
					'${b.cur_module}__${cast_type_name}'
				} else {
					cast_type_name
				}
				for _, check_name in [cast_type_name, qualified_cast] {
					if st_type := b.struct_types[check_name] {
						if int(st_type) < b.mod.type_store.types.len {
							st := b.mod.type_store.types[st_type]
							if st.kind == .struct_t && st.field_names.len >= 2
								&& st.field_names[0] == '_tag' {
								is_sumtype_const = true
								const_type = st_type
								break
							}
						}
					}
				}
			}
		}
		// For sum type constants detected via InitExpr, also fix const_type
		if is_sumtype_const && value_kind == .expr_init {
			if value_c.edge(0).kind() == .expr_ident {
				type_name := value_c.edge(0).name()
				if st_type := b.struct_types[type_name] {
					const_type = st_type
				} else {
					qualified_st := '${b.cur_module}__${type_name}'
					if st_type2 := b.struct_types[qualified_st] {
						const_type = st_type2
					}
				}
			}
		}
		// Detect constant FIXED arrays with all-literal elements.
		if value_kind == .expr_array_init && (value_c.edge_count() - 5) > 0 {
			n_arr_exprs := value_c.edge_count() - 5
			mut is_fixed_array := true
			// Check type environment to see if this is a dynamic array
			if b.env != unsafe { nil } {
				fpos := value_c.pos()
				if fpos.id != 0 {
					if ct := b.env.get_expr_type(fpos.id) {
						if ct is types.Array {
							is_fixed_array = false
						}
					}
				}
			}
			arr_data := b.try_serialize_const_array_from_flat(value_c)
			if arr_data.len > 0 {
				elem_size := arr_data.len / n_arr_exprs
				is_float_arr := b.is_float_array_from_flat(value_c)
				elem_type := if is_float_arr {
					b.mod.type_store.get_float(elem_size * 8)
				} else if elem_size == 8 {
					b.mod.type_store.get_int(64)
				} else if elem_size == 4 {
					b.mod.type_store.get_int(32)
				} else if elem_size == 2 {
					b.mod.type_store.get_int(16)
				} else {
					b.mod.type_store.get_int(8)
				}
				if is_fixed_array {
					b.mod.add_global_with_data(const_name, elem_type, true, arr_data)
					b.const_array_globals[const_name] = true
					b.const_array_globals[field_name] = true
					b.const_array_elem_count[const_name] = n_arr_exprs
					b.const_array_elem_count[field_name] = n_arr_exprs
					continue
				} else {
					// Dynamic array constant: serialize data, create array struct global
					data_name := '${const_name}__data'
					b.mod.add_global_with_data(data_name, elem_type, true, arr_data)
					b.const_array_globals[data_name] = true
					// Add array struct global (initialized in _vinit)
					arr_struct_type := b.get_array_type()
					b.mod.add_global(const_name, arr_struct_type, false)
					b.dyn_const_arrays << DynConstArray{
						arr_global_name:  const_name
						data_global_name: data_name
						elem_count:       n_arr_exprs
						elem_size:        elem_size
					}
					continue
				}
			}
		}
		// For float constants, store bit pattern as initial_value
		mut actual_init := initial_value
		if const_name in b.float_const_values {
			f_val := b.float_const_values[const_name].f64()
			actual_init = i64(unsafe { *(&i64(&f_val)) })
			const_type = b.mod.type_store.get_float(64)
		}
		b.mod.add_global_with_value(const_name, const_type, true, actual_init)
		if !is_sumtype_const && (initial_value != 0 || b.is_zero_literal_from_flat(value_c)) {
			b.const_values[const_name] = initial_value
			b.const_value_types[const_name] = const_type
			// Also store without module prefix for transformer-generated references
			b.const_values[field_name] = initial_value
			b.const_value_types[field_name] = const_type
		}
	}
}

// register_global_decl extracts the per-GlobalDecl body from
// `register_consts_and_globals` so both the legacy walker and the
// flat-cursor port can dispatch through a single implementation.
fn (mut b Builder) register_global_decl(stmt ast.GlobalDecl) {
	for field in stmt.fields {
		glob_type := b.global_field_type(field)
		if field.name.starts_with('C.') {
			continue
		}
		if ssa_module_storage_field_is_c_extern(stmt, field) {
			glob_id := b.mod.add_external_global(field.name, glob_type)
			b.global_refs[field.name] = glob_id
			qualified_name := ssa_module_storage_name(b.cur_module, field.name)
			if qualified_name != field.name {
				b.global_refs[qualified_name] = glob_id
			}
			continue
		}
		glob_name := ssa_module_storage_name(b.cur_module, field.name)
		initial_value := if field.value != ast.empty_expr {
			b.try_eval_const_int(field.value)
		} else {
			i64(0)
		}
		b.mod.add_global_with_value(glob_name, glob_type, false, initial_value)
	}
}

// register_global_decl_from_flat (s237) is the cursor mirror of
// register_global_decl. GlobalDecl flat = (.stmt_global_decl, [edge0=attrs,
// edge1=fields]); each field is an .aux_field_decl (name in name_id, edge0=typ,
// edge1=value, edge2=attrs). The c_extern check mirrors
// ssa_module_storage_field_is_c_extern via cursor_attrs_has on the decl-level
// (edge0) and field-level (field edge2) attribute lists; the value uses
// try_eval_const_int_from_flat (s235). Bit-identical to the AST path.
fn (mut b Builder) register_global_decl_from_flat(c ast.Cursor) {
	node_attrs := c.list_at(0)
	fields := c.list_at(1)
	for fi in 0 .. fields.len() {
		field_c := fields.at(fi)
		field_name := field_c.name()
		glob_type := b.global_field_type_from_flat(field_c)
		if field_name.starts_with('C.') {
			continue
		}
		is_c_extern := field_name.starts_with('C.') || cursor_attrs_has(node_attrs, 'c_extern')
			|| cursor_attrs_has(field_c.list_at(2), 'c_extern')
		if is_c_extern {
			glob_id := b.mod.add_external_global(field_name, glob_type)
			b.global_refs[field_name] = glob_id
			qualified_name := ssa_module_storage_name(b.cur_module, field_name)
			if qualified_name != field_name {
				b.global_refs[qualified_name] = glob_id
			}
			continue
		}
		glob_name := ssa_module_storage_name(b.cur_module, field_name)
		value_c := field_c.edge(1)
		initial_value := if value_c.kind() != .expr_empty {
			b.try_eval_const_int_from_flat(value_c)
		} else {
			i64(0)
		}
		b.mod.add_global_with_value(glob_name, glob_type, false, initial_value)
	}
}

// resolve_forward_const_refs re-evaluates constants that had value 0 due to forward references.
// After all constants are registered, references that previously failed can now be resolved.
fn (mut b Builder) resolve_forward_const_refs(files []ast.File) {
	for file in files {
		b.cur_module = file_module_name(file)
		for stmt in file.stmts {
			if stmt is ast.ConstDecl {
				for field in stmt.fields {
					const_name := if b.cur_module != '' && b.cur_module != 'main' {
						'${b.cur_module}__${field.name}'
					} else {
						field.name
					}
					// Skip constants that already have non-zero values
					if const_name in b.const_values {
						continue
					}
					// Skip zero literals (they're intentionally 0)
					if b.is_zero_literal(field.value) {
						continue
					}
					// Skip sum type constants (multi-word values stored as globals, not inline)
					if field.value is ast.InitExpr {
						mut has_tag := false
						for init_field in field.value.fields {
							if init_field.name == '_tag' {
								has_tag = true
								break
							}
						}
						if has_tag {
							continue
						}
					}
					// Re-evaluate the constant expression
					new_value := b.try_eval_const_int(field.value)
					if new_value != 0 {
						b.const_values[const_name] = new_value
						b.const_values[field.name] = new_value
						ct := b.const_field_type(field.name, field.value)
						b.const_value_types[const_name] = ct
						b.const_value_types[field.name] = ct
						// Update the global variable's initial value
						for i, g in b.mod.globals {
							if g.name == const_name {
								b.mod.globals[i] = GlobalVar{
									...g
									initial_value: new_value
								}
								break
							}
						}
					}
				}
			}
		}
	}
}

// resolve_forward_const_refs_from_flat is the flat-cursor counterpart of
// `resolve_forward_const_refs`. Walks each file's top-level stmts via
// FileCursor and only rehydrates `.stmt_const_decl` nodes via
// `flat.decode_stmt`. Non-ConstDecl stmts are never decoded.
fn (mut b Builder) resolve_forward_const_refs_from_flat(flat &ast.FlatAst) {
	for fi in 0 .. flat.files.len {
		fc := flat.file_cursor(fi)
		b.cur_module = fc.mod().replace('.', '_')
		stmts := fc.stmts()
		for si in 0 .. stmts.len() {
			c := stmts.at(si)
			if c.kind() != .stmt_const_decl {
				continue
			}
			// s235: walk the const-decl field-inits via cursors — no decode_stmt.
			// ConstDecl flat = one edge -> aux_list of .aux_field_init (field name in
			// name_id, value expr at edge(0)).
			fields := c.list_at(0)
			for fidx in 0 .. fields.len() {
				field_c := fields.at(fidx)
				field_name := field_c.name()
				value_c := field_c.edge(0)
				const_name := if b.cur_module != '' && b.cur_module != 'main' {
					'${b.cur_module}__${field_name}'
				} else {
					field_name
				}
				if const_name in b.const_values {
					continue
				}
				if b.is_zero_literal_from_flat(value_c) {
					continue
				}
				if value_c.kind() == .expr_init {
					mut has_tag := false
					n_init_fields := value_c.edge_count() - 1
					for ifi := 0; ifi < n_init_fields; ifi++ {
						if value_c.edge(1 + ifi).name() == '_tag' {
							has_tag = true
							break
						}
					}
					if has_tag {
						continue
					}
				}
				new_value := b.try_eval_const_int_from_flat(value_c)
				if new_value != 0 {
					b.const_values[const_name] = new_value
					b.const_values[field_name] = new_value
					ct := b.const_field_type_from_flat(field_name, value_c)
					b.const_value_types[const_name] = ct
					b.const_value_types[field_name] = ct
					for i, g in b.mod.globals {
						if g.name == const_name {
							b.mod.globals[i] = GlobalVar{
								...g
								initial_value: new_value
							}
							break
						}
					}
				}
			}
		}
	}
}

// resolve_const_int looks up a constant name in const_values, trying bare, module-qualified,
// and builtin-qualified names. Returns 0 if not found.
fn (b &Builder) resolve_const_int(name string) int {
	if name in b.const_values {
		return int(b.const_values[name])
	}
	qualified := '${b.cur_module}__${name}'
	if qualified in b.const_values {
		return int(b.const_values[qualified])
	}
	builtin_q := 'builtin__${name}'
	if builtin_q in b.const_values {
		return int(b.const_values[builtin_q])
	}
	return 0
}

// try_eval_const_int attempts to evaluate a constant expression to an integer value.
// Returns 0 for expressions that cannot be evaluated at compile time.
fn (b &Builder) is_float_array(arr ast.ArrayInitExpr) bool {
	if arr.exprs.len > 0 {
		first := arr.exprs[0]
		if first is ast.CallOrCastExpr {
			if first.lhs is ast.Ident {
				return first.lhs.name in ['f32', 'f64']
			}
		} else if first is ast.CastExpr {
			if first.typ is ast.Ident {
				return first.typ.name in ['f32', 'f64']
			}
		}
	}
	return false
}

// is_float_array_from_flat (s241) is the cursor mirror of is_float_array.
// ArrayInitExpr flat = (.expr_array_init, [edge0=typ, edge1=init, edge2=cap,
// edge3=len, edge4=update_expr, edge5..n=exprs]); first element = edge(5).
fn (b &Builder) is_float_array_from_flat(c ast.Cursor) bool {
	if c.edge_count() > 5 {
		first := c.edge(5)
		if first.kind() == .expr_call_or_cast {
			lhs_c := first.edge(0)
			if lhs_c.kind() == .expr_ident {
				return lhs_c.name() in ['f32', 'f64']
			}
		} else if first.kind() == .expr_cast {
			typ_c := first.edge(0)
			if typ_c.kind() == .expr_ident {
				return typ_c.name() in ['f32', 'f64']
			}
		}
	}
	return false
}

// try_serialize_const_array attempts to serialize a constant array's elements to raw bytes.
// Returns the serialized data or empty if any element can't be evaluated at compile time.
fn (mut b Builder) try_serialize_const_array(arr ast.ArrayInitExpr) []u8 {
	if arr.exprs.len == 0 {
		return []u8{}
	}
	// Untyped integer literals default to V `int`, which is 32-bit.
	mut elem_size := 4
	mut is_float := false
	// Check first element for type cast (e.g., u64(0x123), f64(0.5))
	first := arr.exprs[0]
	if first is ast.CallOrCastExpr {
		if first.lhs is ast.Ident {
			match first.lhs.name {
				'u8', 'i8', 'byte' {
					elem_size = 1
				}
				'u16', 'i16' {
					elem_size = 2
				}
				'u32', 'i32', 'int' {
					elem_size = 4
				}
				'f32' {
					elem_size = 4
					is_float = true
				}
				'u64', 'i64' {
					elem_size = 8
				}
				'f64' {
					elem_size = 8
					is_float = true
				}
				else {}
			}
		}
	} else if first is ast.CastExpr {
		if first.typ is ast.Ident {
			match first.typ.name {
				'u8', 'i8', 'byte' {
					elem_size = 1
				}
				'u16', 'i16' {
					elem_size = 2
				}
				'u32', 'i32', 'int' {
					elem_size = 4
				}
				'f32' {
					elem_size = 4
					is_float = true
				}
				'u64', 'i64' {
					elem_size = 8
				}
				'f64' {
					elem_size = 8
					is_float = true
				}
				else {}
			}
		}
	}
	mut data := []u8{cap: arr.exprs.len * elem_size}
	for ei, expr in arr.exprs {
		_ = ei
		if is_float {
			// For float arrays, parse as f64 and store IEEE 754 bits
			fval := b.try_eval_const_float(expr)
			if elem_size == 4 {
				fval32 := f32(fval)
				bits := unsafe { *(&u32(&fval32)) }
				data << u8(bits & 0xFF)
				data << u8((bits >> 8) & 0xFF)
				data << u8((bits >> 16) & 0xFF)
				data << u8((bits >> 24) & 0xFF)
			} else {
				bits := unsafe { *(&u64(&fval)) }
				data << u8(bits & 0xFF)
				data << u8((bits >> 8) & 0xFF)
				data << u8((bits >> 16) & 0xFF)
				data << u8((bits >> 24) & 0xFF)
				data << u8((bits >> 32) & 0xFF)
				data << u8((bits >> 40) & 0xFF)
				data << u8((bits >> 48) & 0xFF)
				data << u8((bits >> 56) & 0xFF)
			}
		} else {
			val := b.try_eval_const_int(expr)
			match elem_size {
				1 {
					data << u8(val)
				}
				2 {
					data << u8(val & 0xFF)
					data << u8((val >> 8) & 0xFF)
				}
				4 {
					data << u8(val & 0xFF)
					data << u8((val >> 8) & 0xFF)
					data << u8((val >> 16) & 0xFF)
					data << u8((val >> 24) & 0xFF)
				}
				else {
					v := u64(val)
					data << u8(v & 0xFF)
					data << u8((v >> 8) & 0xFF)
					data << u8((v >> 16) & 0xFF)
					data << u8((v >> 24) & 0xFF)
					data << u8((v >> 32) & 0xFF)
					data << u8((v >> 40) & 0xFF)
					data << u8((v >> 48) & 0xFF)
					data << u8((v >> 56) & 0xFF)
				}
			}
		}
	}
	return data
}

// try_serialize_const_array_from_flat (s241) is the cursor mirror of
// try_serialize_const_array. ArrayInitExpr exprs are edges 5..n; element size /
// float-ness come from the first element's CallOrCast/Cast Ident type name; values
// via try_eval_const_float_from_flat / try_eval_const_int_from_flat. Byte layout
// (little-endian per element) is identical to the AST.
fn (mut b Builder) try_serialize_const_array_from_flat(c ast.Cursor) []u8 {
	n_exprs := c.edge_count() - 5
	if n_exprs <= 0 {
		return []u8{}
	}
	mut elem_size := 8
	mut is_float := false
	first := c.edge(5)
	if first.kind() == .expr_call_or_cast {
		lhs_c := first.edge(0)
		if lhs_c.kind() == .expr_ident {
			match lhs_c.name() {
				'u8', 'i8', 'byte' {
					elem_size = 1
				}
				'u16', 'i16' {
					elem_size = 2
				}
				'u32', 'i32', 'int' {
					elem_size = 4
				}
				'f32' {
					elem_size = 4
					is_float = true
				}
				'u64', 'i64' {
					elem_size = 8
				}
				'f64' {
					elem_size = 8
					is_float = true
				}
				else {}
			}
		}
	} else if first.kind() == .expr_cast {
		typ_c := first.edge(0)
		if typ_c.kind() == .expr_ident {
			match typ_c.name() {
				'u8', 'i8', 'byte' {
					elem_size = 1
				}
				'u16', 'i16' {
					elem_size = 2
				}
				'u32', 'i32', 'int' {
					elem_size = 4
				}
				'f32' {
					elem_size = 4
					is_float = true
				}
				'u64', 'i64' {
					elem_size = 8
				}
				'f64' {
					elem_size = 8
					is_float = true
				}
				else {}
			}
		}
	}
	mut data := []u8{cap: n_exprs * elem_size}
	for ei in 0 .. n_exprs {
		expr_c := c.edge(5 + ei)
		if is_float {
			fval := b.try_eval_const_float_from_flat(expr_c)
			if elem_size == 4 {
				fval32 := f32(fval)
				bits := unsafe { *(&u32(&fval32)) }
				data << u8(bits & 0xFF)
				data << u8((bits >> 8) & 0xFF)
				data << u8((bits >> 16) & 0xFF)
				data << u8((bits >> 24) & 0xFF)
			} else {
				bits := unsafe { *(&u64(&fval)) }
				data << u8(bits & 0xFF)
				data << u8((bits >> 8) & 0xFF)
				data << u8((bits >> 16) & 0xFF)
				data << u8((bits >> 24) & 0xFF)
				data << u8((bits >> 32) & 0xFF)
				data << u8((bits >> 40) & 0xFF)
				data << u8((bits >> 48) & 0xFF)
				data << u8((bits >> 56) & 0xFF)
			}
		} else {
			val := b.try_eval_const_int_from_flat(expr_c)
			match elem_size {
				1 {
					data << u8(val)
				}
				2 {
					data << u8(val & 0xFF)
					data << u8((val >> 8) & 0xFF)
				}
				4 {
					data << u8(val & 0xFF)
					data << u8((val >> 8) & 0xFF)
					data << u8((val >> 16) & 0xFF)
					data << u8((val >> 24) & 0xFF)
				}
				else {
					v := u64(val)
					data << u8(v & 0xFF)
					data << u8((v >> 8) & 0xFF)
					data << u8((v >> 16) & 0xFF)
					data << u8((v >> 24) & 0xFF)
					data << u8((v >> 32) & 0xFF)
					data << u8((v >> 40) & 0xFF)
					data << u8((v >> 48) & 0xFF)
					data << u8((v >> 56) & 0xFF)
				}
			}
		}
	}
	return data
}

// try_eval_const_float evaluates a compile-time float constant expression.
fn (b &Builder) try_eval_const_float(expr ast.Expr) f64 {
	match expr {
		ast.BasicLiteral {
			if expr.kind == .number {
				return expr.value.f64()
			}
		}
		ast.CallOrCastExpr {
			// f64(0.5), f32(1.0), etc.
			return b.try_eval_const_float(expr.expr)
		}
		ast.PrefixExpr {
			if expr.op == .minus {
				return -b.try_eval_const_float(expr.expr)
			}
		}
		else {}
	}

	return 0.0
}

// try_eval_const_float_from_flat (s241) is the cursor mirror of try_eval_const_float.
fn (b &Builder) try_eval_const_float_from_flat(c ast.Cursor) f64 {
	match c.kind() {
		.expr_basic_literal {
			kind := unsafe { token.Token(int(c.aux())) }
			if kind == .number {
				return c.name().f64()
			}
		}
		.expr_call_or_cast {
			// f64(0.5), f32(1.0), etc. — inner value at edge(1).
			return b.try_eval_const_float_from_flat(c.edge(1))
		}
		.expr_prefix {
			op := unsafe { token.Token(int(c.aux())) }
			if op == .minus {
				return -b.try_eval_const_float_from_flat(c.edge(0))
			}
		}
		else {}
	}

	return 0.0
}

// is_float_cast_expr returns true if the expression is a cast to a float type
// (e.g., f64(literal), f32(expr)), which should be evaluated as a float constant.
// This prevents pure integer expressions like `64 - 11 - 1` from being stored
// as float constants.
fn (b &Builder) is_float_cast_expr(expr ast.Expr) bool {
	if expr is ast.CastExpr {
		if expr.typ is ast.Ident {
			return expr.typ.name == 'f64' || expr.typ.name == 'f32'
		}
	}
	if expr is ast.CallOrCastExpr {
		if expr.lhs is ast.Ident {
			return expr.lhs.name == 'f64' || expr.lhs.name == 'f32'
		}
	}
	if expr is ast.BasicLiteral {
		if expr.kind == .number {
			return expr.value.contains('.')
				|| (!expr.value.starts_with('0x') && !expr.value.starts_with('0X')
				&& (expr.value.contains('e') || expr.value.contains('E')))
		}
	}
	// Check for InfixExpr/PrefixExpr involving float operations
	// (e.g., `1.0 / ln2` or `-0.5`)
	if expr is ast.PrefixExpr {
		return b.is_float_cast_expr(expr.expr)
	}
	if expr is ast.InfixExpr {
		return b.is_float_cast_expr(expr.lhs) || b.is_float_cast_expr(expr.rhs)
	}
	return false
}

// is_float_cast_expr_from_flat (s241) is the cursor mirror of is_float_cast_expr.
// The AST's mutually-exclusive `if expr is X` chain becomes a match on c.kind().
fn (b &Builder) is_float_cast_expr_from_flat(c ast.Cursor) bool {
	match c.kind() {
		.expr_cast {
			typ_c := c.edge(0)
			if typ_c.kind() == .expr_ident {
				return typ_c.name() == 'f64' || typ_c.name() == 'f32'
			}
		}
		.expr_call_or_cast {
			lhs_c := c.edge(0)
			if lhs_c.kind() == .expr_ident {
				return lhs_c.name() == 'f64' || lhs_c.name() == 'f32'
			}
		}
		.expr_basic_literal {
			kind := unsafe { token.Token(int(c.aux())) }
			if kind == .number {
				value := c.name()
				return value.contains('.')
					|| (!value.starts_with('0x') && !value.starts_with('0X')
					&& (value.contains('e') || value.contains('E')))
			}
		}
		.expr_prefix {
			return b.is_float_cast_expr_from_flat(c.edge(0))
		}
		.expr_infix {
			return b.is_float_cast_expr_from_flat(c.edge(0))
				|| b.is_float_cast_expr_from_flat(c.edge(1))
		}
		else {}
	}

	return false
}

// try_eval_computed_float evaluates a constant expression to a float value.
// Handles computed float constants like `pi / 2.0`, `1.0 / ln2`, etc.
// Returns none if the expression can't be evaluated as a compile-time float.
fn (mut b Builder) try_eval_computed_float(expr ast.Expr) ?f64 {
	match expr {
		ast.BasicLiteral {
			if expr.kind == .number && expr.value.contains('.') {
				return expr.value.f64()
			}
			if expr.kind == .number {
				return f64(expr.value.i64())
			}
			return none
		}
		ast.Ident {
			// Look up the identifier in float_const_values
			if fval := b.float_const_values[expr.name] {
				return fval.f64()
			}
			qualified := '${b.cur_module}__${expr.name}'
			if fval := b.float_const_values[qualified] {
				return fval.f64()
			}
			return none
		}
		ast.InfixExpr {
			lhs := b.try_eval_computed_float(expr.lhs) or { return none }
			rhs := b.try_eval_computed_float(expr.rhs) or { return none }
			return match expr.op {
				.plus {
					lhs + rhs
				}
				.minus {
					lhs - rhs
				}
				.mul {
					lhs * rhs
				}
				.div {
					if rhs != 0.0 {
						lhs / rhs
					} else {
						f64(0.0)
					}
				}
				else {
					return none
				}
			}
		}
		ast.PrefixExpr {
			if expr.op == .minus {
				val := b.try_eval_computed_float(expr.expr) or { return none }
				return -val
			}
			return none
		}
		ast.CastExpr {
			// Only evaluate as float if casting to a float type (f64, f32)
			if expr.typ is ast.Ident && (expr.typ.name == 'f64' || expr.typ.name == 'f32') {
				return b.try_eval_computed_float(expr.expr)
			}
			return none
		}
		ast.CallOrCastExpr {
			// Only evaluate as float if casting to a float type (f64, f32)
			if expr.lhs is ast.Ident && (expr.lhs.name == 'f64' || expr.lhs.name == 'f32') {
				return b.try_eval_computed_float(expr.expr)
			}
			return none
		}
		else {
			return none
		}
	}
}

// try_eval_computed_float_from_flat (s241) is the cursor mirror of
// try_eval_computed_float. Infix/Prefix op in aux; Cast/CallOrCast inner at edge(1).
fn (mut b Builder) try_eval_computed_float_from_flat(c ast.Cursor) ?f64 {
	match c.kind() {
		.expr_basic_literal {
			kind := unsafe { token.Token(int(c.aux())) }
			value := c.name()
			if kind == .number && value.contains('.') {
				return value.f64()
			}
			if kind == .number {
				return f64(value.i64())
			}
			return none
		}
		.expr_ident {
			name := c.name()
			if fval := b.float_const_values[name] {
				return fval.f64()
			}
			qualified := '${b.cur_module}__${name}'
			if fval := b.float_const_values[qualified] {
				return fval.f64()
			}
			return none
		}
		.expr_infix {
			op := unsafe { token.Token(int(c.aux())) }
			lhs := b.try_eval_computed_float_from_flat(c.edge(0)) or { return none }
			rhs := b.try_eval_computed_float_from_flat(c.edge(1)) or { return none }
			return match op {
				.plus {
					lhs + rhs
				}
				.minus {
					lhs - rhs
				}
				.mul {
					lhs * rhs
				}
				.div {
					if rhs != 0.0 {
						lhs / rhs
					} else {
						f64(0.0)
					}
				}
				else {
					return none
				}
			}
		}
		.expr_prefix {
			op := unsafe { token.Token(int(c.aux())) }
			if op == .minus {
				val := b.try_eval_computed_float_from_flat(c.edge(0)) or { return none }
				return -val
			}
			return none
		}
		.expr_cast {
			typ_c := c.edge(0)
			if typ_c.kind() == .expr_ident && (typ_c.name() == 'f64' || typ_c.name() == 'f32') {
				return b.try_eval_computed_float_from_flat(c.edge(1))
			}
			return none
		}
		.expr_call_or_cast {
			lhs_c := c.edge(0)
			if lhs_c.kind() == .expr_ident && (lhs_c.name() == 'f64' || lhs_c.name() == 'f32') {
				return b.try_eval_computed_float_from_flat(c.edge(1))
			}
			return none
		}
		else {
			return none
		}
	}
}

fn parse_const_uint_literal(lit string) u64 {
	if lit.len == 0 {
		return 0
	}
	mut idx := 0
	if lit[idx] == `+` || lit[idx] == `-` {
		idx++
	}
	mut base := u64(10)
	if idx + 1 < lit.len && lit[idx] == `0` {
		match lit[idx + 1] {
			`x`, `X` {
				base = 16
				idx += 2
			}
			`o`, `O` {
				base = 8
				idx += 2
			}
			`b`, `B` {
				base = 2
				idx += 2
			}
			else {}
		}
	}
	mut val := u64(0)
	for idx < lit.len {
		ch := lit[idx]
		if ch == `_` {
			idx++
			continue
		}
		digit := match true {
			ch >= `0` && ch <= `9` { u64(ch - `0`) }
			base == 16 && ch >= `a` && ch <= `f` { u64(ch - `a` + 10) }
			base == 16 && ch >= `A` && ch <= `F` { u64(ch - `A` + 10) }
			else { break }
		}

		if digit >= base {
			break
		}
		val = val * base + digit
		idx++
	}
	return val
}

fn parse_const_int_literal(lit string) i64 {
	if lit.len == 0 {
		return 0
	}
	neg := lit[0] == `-`
	val := parse_const_uint_literal(lit)
	if neg {
		// Keep the conversion in unsigned space so `-9223372036854775808` stays intact
		// even in self-hosted ARM64 builds where string.i64()/u64() are unreliable.
		return i64(u64(0) - val)
	}
	return i64(val)
}

fn (mut b Builder) try_eval_const_int(expr ast.Expr) i64 {
	match expr {
		ast.BasicLiteral {
			if expr.kind == .number {
				// Handle float notation (e.g., 1e10, 1.5e3) cast to integer
				// But skip hex values like 0x01e8480000000000 where 'e' is a hex digit
				if !expr.value.starts_with('0x') && !expr.value.starts_with('0X')
					&& (expr.value.contains('e') || expr.value.contains('E')
					|| expr.value.contains('.')) {
					return i64(expr.value.f64())
				}
				return parse_const_int_literal(expr.value)
			}
			if expr.kind == .key_true {
				return 1
			}
			if expr.kind == .key_false {
				return 0
			}
			if expr.kind == .char {
				return b.resolve_char_const_value(expr.value)
			}
		}
		ast.InfixExpr {
			lhs := b.try_eval_const_int(expr.lhs)
			rhs := b.try_eval_const_int(expr.rhs)
			result2 := match expr.op {
				.plus {
					lhs + rhs
				}
				.minus {
					lhs - rhs
				}
				.mul {
					lhs * rhs
				}
				.div {
					if rhs != 0 { lhs / rhs } else { i64(0) }
				}
				.mod {
					if rhs != 0 { lhs % rhs } else { i64(0) }
				}
				.left_shift {
					i64(u64(lhs) << u64(rhs))
				}
				.right_shift {
					i64(u64(lhs) >> u64(rhs))
				}
				.amp {
					lhs & rhs
				}
				.pipe {
					lhs | rhs
				}
				.xor {
					lhs ^ rhs
				}
				else {
					i64(0)
				}
			}

			return result2
		}
		ast.PrefixExpr {
			val := b.try_eval_const_int(expr.expr)
			return match expr.op {
				.minus { -val }
				.bit_not { ~val }
				else { 0 }
			}
		}
		ast.Ident {
			// Try to resolve reference to another constant
			if expr.name in b.const_values {
				return b.const_values[expr.name]
			}
			qualified := '${b.cur_module}__${expr.name}'
			if qualified in b.const_values {
				return b.const_values[qualified]
			}
			builtin_qual := 'builtin__${expr.name}'
			if builtin_qual in b.const_values {
				return b.const_values[builtin_qual]
			}
		}
		ast.CastExpr {
			return b.try_eval_const_int(expr.expr)
		}
		ast.CallOrCastExpr {
			// V2 parser produces CallOrCastExpr for type casts like u32(52), u64(0xF)
			return b.try_eval_const_int(expr.expr)
		}
		ast.ParenExpr {
			return b.try_eval_const_int(expr.expr)
		}
		ast.InitExpr {
			// Handle sum type init: InitExpr{_tag: N, _data: ...}
			// Extract the _tag value so struct constants get correct tag in data section
			for field in expr.fields {
				if field.name == '_tag' {
					return b.try_eval_const_int(field.value)
				}
			}
		}
		else {}
	}

	return 0
}

fn (b &Builder) is_zero_literal(expr ast.Expr) bool {
	if expr is ast.BasicLiteral {
		return expr.kind == .number && expr.value == '0'
	}
	return false
}

// try_eval_const_int_from_flat (s235) is the cursor mirror of try_eval_const_int.
// BasicLiteral kind in aux / value in name(); Infix/Prefix op in aux with operand
// edges; Cast/CallOrCast inner value at edge(1); Paren inner at edge(0); Init walks
// its .aux_field_init children for `_tag`. Bit-identical (same recursion, same
// const_values lookups, same literal parsing).
fn (mut b Builder) try_eval_const_int_from_flat(c ast.Cursor) i64 {
	match c.kind() {
		.expr_basic_literal {
			kind := unsafe { token.Token(int(c.aux())) }
			value := c.name()
			if kind == .number {
				if !value.starts_with('0x') && !value.starts_with('0X')
					&& (value.contains('e') || value.contains('E')
					|| value.contains('.')) {
					return i64(value.f64())
				}
				return parse_const_int_literal(value)
			}
			if kind == .key_true {
				return 1
			}
			if kind == .key_false {
				return 0
			}
			if kind == .char {
				return b.resolve_char_const_value(value)
			}
		}
		.expr_infix {
			op := unsafe { token.Token(int(c.aux())) }
			lhs := b.try_eval_const_int_from_flat(c.edge(0))
			rhs := b.try_eval_const_int_from_flat(c.edge(1))
			result2 := match op {
				.plus {
					lhs + rhs
				}
				.minus {
					lhs - rhs
				}
				.mul {
					lhs * rhs
				}
				.div {
					if rhs != 0 { lhs / rhs } else { i64(0) }
				}
				.mod {
					if rhs != 0 { lhs % rhs } else { i64(0) }
				}
				.left_shift {
					i64(u64(lhs) << u64(rhs))
				}
				.right_shift {
					i64(u64(lhs) >> u64(rhs))
				}
				.amp {
					lhs & rhs
				}
				.pipe {
					lhs | rhs
				}
				.xor {
					lhs ^ rhs
				}
				else {
					i64(0)
				}
			}

			return result2
		}
		.expr_prefix {
			op := unsafe { token.Token(int(c.aux())) }
			val := b.try_eval_const_int_from_flat(c.edge(0))
			return match op {
				.minus { -val }
				.bit_not { ~val }
				else { i64(0) }
			}
		}
		.expr_ident {
			name := c.name()
			if name in b.const_values {
				return b.const_values[name]
			}
			qualified := '${b.cur_module}__${name}'
			if qualified in b.const_values {
				return b.const_values[qualified]
			}
			builtin_qual := 'builtin__${name}'
			if builtin_qual in b.const_values {
				return b.const_values[builtin_qual]
			}
		}
		.expr_cast {
			return b.try_eval_const_int_from_flat(c.edge(1))
		}
		.expr_call_or_cast {
			return b.try_eval_const_int_from_flat(c.edge(1))
		}
		.expr_paren {
			return b.try_eval_const_int_from_flat(c.edge(0))
		}
		.expr_init {
			// edge0=typ, edge1..n = .aux_field_init (name in name_id, value at edge0)
			n_fields := c.edge_count() - 1
			for fi := 0; fi < n_fields; fi++ {
				field_c := c.edge(1 + fi)
				if field_c.name() == '_tag' {
					return b.try_eval_const_int_from_flat(field_c.edge(0))
				}
			}
		}
		else {}
	}

	return 0
}

// is_zero_literal_from_flat (s235) is the cursor mirror of is_zero_literal.
fn (b &Builder) is_zero_literal_from_flat(c ast.Cursor) bool {
	if c.kind() == .expr_basic_literal {
		kind := unsafe { token.Token(int(c.aux())) }
		return kind == .number && c.name() == '0'
	}
	return false
}

// resolve_char_const_value is like resolve_char_value but returns i64 for use in try_eval_const_int.
fn (b &Builder) resolve_char_const_value(val string) i64 {
	return i64(b.resolve_char_value(val))
}

// resolve_char_value converts a V character literal value to its numeric byte value.
// Handles escape sequences like \n, \t, \r, \\, \', \0, and raw characters.
fn (b &Builder) resolve_char_value(val string) int {
	mut s := val
	// Strip surrounding quotes if present
	if s.len >= 2 && s[0] == `\`` && s[s.len - 1] == `\`` {
		s = s[1..s.len - 1]
	}
	if s.len >= 2 && ((s[0] == `'` && s[s.len - 1] == `'`) || (s[0] == `"` && s[s.len - 1] == `"`)) {
		s = s[1..s.len - 1]
	}
	if s.len == 0 {
		return 0
	}
	// Handle escape sequences
	if s.len >= 2 && s[0] == `\\` {
		return match s[1] {
			`n` { 10 }
			`t` { 9 }
			`r` { 13 }
			`\\` { 92 }
			`'` { 39 }
			`"` { 34 }
			`0` { 0 }
			`a` { 7 }
			`b` { 8 }
			`f` { 12 }
			`v` { 11 }
			`e` { 27 }
			else { int(s[1]) }
		}
	}
	// Multi-byte UTF-8 character → decode to Unicode code point
	if s.len >= 2 && s[0] >= 0xC0 {
		return utf8_to_codepoint(s)
	}
	// Single ASCII character
	return int(s[0])
}

// utf8_to_codepoint decodes the first UTF-8 character in s to its Unicode code point.
fn utf8_to_codepoint(s string) int {
	if s.len == 0 {
		return 0
	}
	b0 := s[0]
	if b0 < 0x80 {
		return int(b0)
	}
	if b0 < 0xE0 && s.len >= 2 {
		return int(u32(b0 & 0x1F) << 6 | u32(s[1] & 0x3F))
	}
	if b0 < 0xF0 && s.len >= 3 {
		return int(u32(b0 & 0x0F) << 12 | u32(s[1] & 0x3F) << 6 | u32(s[2] & 0x3F))
	}
	if s.len >= 4 {
		return int(u32(b0 & 0x07) << 18 | u32(s[1] & 0x3F) << 12 | u32(s[2] & 0x3F) << 6 | u32(s[3] & 0x3F))
	}
	return int(b0)
}

// try_eval_const_string attempts to extract a string value from a constant expression.
// Returns empty string if the expression is not a string literal.
fn (b &Builder) try_eval_const_string(expr ast.Expr) string {
	match expr {
		ast.StringLiteral {
			mut val := expr.value
			// Strip surrounding quotes if present
			if val.len >= 2 && ((val[0] == `'` && val[val.len - 1] == `'`)
				|| (val[0] == `"` && val[val.len - 1] == `"`)) {
				val = val[1..val.len - 1]
			}
			return val
		}
		ast.BasicLiteral {
			if expr.kind == .string {
				mut val := expr.value
				if val.len >= 2 && ((val[0] == `'` && val[val.len - 1] == `'`)
					|| (val[0] == `"` && val[val.len - 1] == `"`)) {
					val = val[1..val.len - 1]
				}
				return val
			}
		}
		else {}
	}

	return ''
}

// try_eval_const_string_from_flat (s241) is the cursor mirror of try_eval_const_string.
// StringLiteral value is in name_id (.expr_string); a .string BasicLiteral likewise.
fn (b &Builder) try_eval_const_string_from_flat(c ast.Cursor) string {
	match c.kind() {
		.expr_string {
			mut val := c.name()
			if val.len >= 2 && ((val[0] == `'` && val[val.len - 1] == `'`)
				|| (val[0] == `"` && val[val.len - 1] == `"`)) {
				val = val[1..val.len - 1]
			}
			return val
		}
		.expr_basic_literal {
			kind := unsafe { token.Token(int(c.aux())) }
			if kind == .string {
				mut val := c.name()
				if val.len >= 2 && ((val[0] == `'` && val[val.len - 1] == `'`)
					|| (val[0] == `"` && val[val.len - 1] == `"`)) {
					val = val[1..val.len - 1]
				}
				return val
			}
		}
		else {}
	}

	return ''
}

fn (mut b Builder) ast_type_to_ssa(typ ast.Expr) TypeID {
	match typ {
		ast.Ident {
			if !ssa_string_ok(typ.name) {
				return b.mod.type_store.get_int(64)
			}
			return b.ident_type_to_ssa(typ.name)
		}
		ast.Type {
			return b.ast_type_node_to_ssa(typ)
		}
		ast.PrefixExpr {
			if typ.op == .amp {
				base := b.ast_type_to_ssa(typ.expr)
				return b.mod.type_store.get_ptr(base)
			}
			if typ.op == .ellipsis {
				// Variadic params (...T) are lowered to []T (dynamic array)
				return b.get_array_type()
			}
			return b.mod.type_store.get_int(64)
		}
		ast.ModifierExpr {
			// mut/shared receivers: unwrap the modifier, the pointer is added by the caller
			return b.ast_type_to_ssa(typ.expr)
		}
		ast.SelectorExpr {
			// module.Type — e.g., C.dirent, os.Stat
			if typ.lhs is ast.Ident {
				mod_name := typ.lhs.name
				if !ssa_string_ok(mod_name) || !ssa_string_ok(typ.rhs.name) {
					return b.mod.type_store.get_int(64)
				}
				full_name := '${mod_name}.${typ.rhs.name}'
				// Try C.StructName → look up as module__C.StructName
				qualified := '${b.cur_module}__${full_name}'
				if qualified in b.struct_types {
					return b.struct_types[qualified]
				}
				// Also try just the full name (e.g., C.dirent)
				if full_name in b.struct_types {
					return b.struct_types[full_name]
				}
				// Try module__StructName (for module.Type references like os.Stat)
				mod_qualified := '${mod_name}__${typ.rhs.name}'
				if mod_qualified in b.struct_types {
					return b.struct_types[mod_qualified]
				}
				if alias_type := b.selector_type_alias_to_ssa(mod_name, typ.rhs.name) {
					return alias_type
				}
				if resolved_mod := b.selector_module_name_for_ident(mod_name) {
					if alias_type := b.selector_type_alias_to_ssa(resolved_mod, typ.rhs.name) {
						return alias_type
					}
				}
				// For C.X types: C structs are registered under their declaring module
				// (e.g., C.dirent in os module → "os__dirent")
				// Try cur_module__StructName
				if mod_name == 'C' {
					cur_qualified := '${b.cur_module}__${typ.rhs.name}'
					if cur_qualified in b.struct_types {
						return b.struct_types[cur_qualified]
					}
					// Search all modules for this C struct
					for sname, sid in b.struct_types {
						if sname.ends_with('__${typ.rhs.name}') {
							return sid
						}
					}
				}
				// Try looking up in the referenced module's scope via type environment
				// (e.g., token.Token where Token is an enum, not a struct)
				if b.env != unsafe { nil } {
					mod_name_v := mod_name.replace('.', '_')
					if scope := b.env.get_scope(mod_name_v) {
						if obj := scope.lookup_parent(typ.rhs.name, 0) {
							return b.type_to_ssa(obj.typ())
						}
					}
				}
			}
			return b.ident_type_to_ssa(typ.rhs.name)
		}
		ast.EmptyExpr {
			return 0 // void
		}
		ast.Tuple {
			mut elem_types := []TypeID{cap: typ.exprs.len}
			for e in typ.exprs {
				elem_types << b.ast_type_to_ssa(e)
			}
			return b.mod.type_store.get_tuple(elem_types)
		}
		else {
			return b.mod.type_store.get_int(64)
		}
	}
}

fn (mut b Builder) ast_type_node_to_ssa(typ ast.Type) TypeID {
	match typ {
		ast.ArrayType {
			return b.get_array_type()
		}
		ast.ArrayFixedType {
			// [N]T → SSA array type with N elements of T
			elem_type := b.ast_type_to_ssa(typ.elem_type)
			arr_len := if typ.len is ast.BasicLiteral {
				int(parse_const_int_literal(typ.len.value))
			} else if typ.len is ast.Ident {
				b.resolve_const_int(typ.len.name)
			} else {
				0
			}
			if arr_len > 0 {
				return b.mod.type_store.get_array(elem_type, arr_len)
			}
			return b.mod.type_store.get_int(64) // fallback
		}
		ast.MapType {
			return b.struct_types['map'] or { b.mod.type_store.get_int(64) }
		}
		ast.FnType {
			i8_t := b.mod.type_store.get_int(8)
			return b.mod.type_store.get_ptr(i8_t) // fn pointers
		}
		ast.OptionType {
			return b.get_option_wrapper_type(b.ast_type_to_ssa(typ.base_type))
		}
		ast.ResultType {
			return b.get_result_wrapper_type(b.ast_type_to_ssa(typ.base_type))
		}
		ast.PointerType {
			base := b.ast_type_to_ssa(typ.base_type)
			return b.mod.type_store.get_ptr(base)
		}
		ast.GenericType {
			return b.generic_type_to_ssa(typ.name, typ.params)
		}
		ast.TupleType {
			mut elem_types := []TypeID{cap: typ.types.len}
			for t in typ.types {
				elem_types << b.ast_type_to_ssa(t)
			}
			return b.mod.type_store.get_tuple(elem_types)
		}
		else {
			return b.mod.type_store.get_int(64)
		}
	}
}

fn (mut b Builder) ident_type_to_ssa(name string) TypeID {
	if !ssa_string_ok(name) {
		return b.mod.type_store.get_int(64)
	}
	return match name {
		'int' {
			b.mod.type_store.get_int(32)
		}
		'i8' {
			b.mod.type_store.get_int(8)
		}
		'i16' {
			b.mod.type_store.get_int(16)
		}
		'i32' {
			b.mod.type_store.get_int(32)
		}
		'i64' {
			b.mod.type_store.get_int(64)
		}
		'u8', 'byte' {
			b.mod.type_store.get_uint(8)
		}
		'u16' {
			b.mod.type_store.get_uint(16)
		}
		'u32' {
			b.mod.type_store.get_uint(32)
		}
		'u64' {
			b.mod.type_store.get_uint(64)
		}
		'f32' {
			b.mod.type_store.get_float(32)
		}
		'f64' {
			b.mod.type_store.get_float(64)
		}
		'bool' {
			b.mod.type_store.get_int(1)
		}
		'string' {
			b.get_string_type()
		}
		'voidptr' {
			i8_t := b.mod.type_store.get_int(8)
			b.mod.type_store.get_ptr(i8_t)
		}
		'rune' {
			b.mod.type_store.get_int(32)
		}
		'char' {
			b.mod.type_store.get_int(8)
		}
		else {
			// Pointer types must be checked FIRST: `Array_int*` in a CastExpr means
			// "pointer to array struct" (used by sumtype smartcast data access),
			// NOT "array of &int". The non-pointer `Array_int` (without `*`) is the
			// array struct itself.
			if name.ends_with('*') {
				// Check for pointer types (e.g., 'StructType*', 'int*', 'Array_int*')
				base_name := name[..name.len - 1]
				base_type := b.ident_type_to_ssa(base_name)
				return b.mod.type_store.get_ptr(base_type)
			} else if name.starts_with('Array_fixed_') {
				b.fixed_array_type_from_name(name)
			} else if name.starts_with('Array_') {
				// Array_* are transformer-generated mangled names for []T.
				// They always represent the builtin array struct.
				b.get_array_type()
			} else if name.starts_with('Map_') {
				b.struct_types['map'] or { b.mod.type_store.get_int(64) }
			} else if alias_type := b.lookup_type_alias_ssa(name) {
				alias_type
			} else if name in b.struct_types {
				// Check struct types
				b.struct_types[name]
			} else if name == 'strings__Builder' {
				// strings.Builder = []u8 = array (type alias)
				b.get_array_type()
			} else if name == 'Builder' {
				// Builder could be strings.Builder alias or an actual struct
				// Try module-qualified first, fall back to array alias
				qualified_b := '${b.cur_module}__Builder'
				if qualified_b in b.struct_types {
					b.struct_types[qualified_b]
				} else {
					b.get_array_type()
				}
			} else {
				// Try module-qualified
				qualified := '${b.cur_module}__${name}'
				if qualified in b.struct_types {
					b.struct_types[qualified]
				} else if b.is_enum_type(name) || b.is_enum_type(qualified) {
					// Enum types are always int (i32) in V
					b.mod.type_store.get_int(32)
				} else if b.env != unsafe { nil } {
					// Use the type checker environment to resolve aliases and other types.
					// First try the current module scope, then try the module prefix in the name
					// (e.g., 'ssa__BlockID' → look up 'BlockID' in 'ssa' module).
					mut resolved := false
					mut resolved_type := TypeID(0)
					if scope := b.env.get_scope(b.cur_module) {
						if obj := scope.lookup_parent(name, 0) {
							resolved_type = b.type_to_ssa(obj.typ())
							resolved = true
						}
					}
					if !resolved && name.contains('__') {
						// Try module-prefixed name: 'ssa__BlockID' → module='ssa', type='BlockID'
						parts := name.split('__')
						if parts.len >= 2 {
							mod_name := parts[0]
							type_name := parts[1..].join('__')
							if mod_scope := b.env.get_scope(mod_name) {
								if obj := mod_scope.lookup_parent(type_name, 0) {
									resolved_type = b.type_to_ssa(obj.typ())
									resolved = true
								}
							}
						}
					}
					if resolved && resolved_type != 0 {
						resolved_type
					} else {
						b.mod.type_store.get_int(64)
					}
				} else {
					b.mod.type_store.get_int(64)
				}
			}
		}
	}
}

fn (mut b Builder) fixed_array_type_from_name(name string) TypeID {
	if !name.starts_with('Array_fixed_') {
		return b.mod.type_store.get_int(64)
	}
	payload := name['Array_fixed_'.len..]
	len_str := payload.all_after_last('_')
	arr_len := len_str.int()
	if arr_len <= 0 {
		return b.mod.type_store.get_int(64)
	}
	elem_name := payload.all_before_last('_')
	elem_type := b.ident_type_to_ssa(elem_name)
	if elem_type == 0 {
		return b.mod.type_store.get_int(64)
	}
	return b.mod.type_store.get_array(elem_type, arr_len)
}

// --- Phase 2: Register function signatures ---

fn (mut b Builder) register_fn_signatures(file ast.File) {
	b.module_import_aliases = module_import_aliases_from_imports(file.imports)
	for stmt in file.stmts {
		if stmt is ast.FnDecl {
			if stmt.typ.generic_params.len > 0 {
				continue
			}
			b.register_fn_sig(stmt)
		}
	}
}

// register_fn_signatures_from_flat is the flat-cursor counterpart of
// `register_fn_signatures`. Walks one file's top-level stmts via FileCursor
// and reads `.stmt_fn_decl` signatures directly from cursor edges. Fn bodies
// are never materialized in this phase.
fn (mut b Builder) register_fn_signatures_from_flat(file_cursor ast.FileCursor) {
	b.module_import_aliases =
		module_import_aliases_from_imports(file_cursor.imports().import_stmts())
	stmts := file_cursor.stmts()
	for si in 0 .. stmts.len() {
		c := stmts.at(si)
		if c.kind() == .stmt_fn_decl {
			// s243: cursor-native fn-signature registration — no decode. FnType is
			// edge1; its generic_params list is list_at(0).
			if c.edge(1).list_at(0).len() > 0 {
				continue
			}
			b.register_fn_sig_from_flat(c)
		}
	}
}

fn (mut b Builder) register_fn_sig(decl ast.FnDecl) {
	fn_name := b.mangle_fn_name(decl)
	param_array_elem_types := b.fn_param_array_elem_types_from_decl(decl)
	if fn_name in b.fn_index {
		if param_array_elem_types.len > 0 {
			b.fn_param_array_elem_types[fn_name] = param_array_elem_types
		}
		return
	}
	if !decl.is_method && decl.language != .c && !is_generated_helper_fn_name(decl.name) {
		b.module_fn_names[module_fn_key(b.cur_module, decl.name)] = fn_name
	}

	ret_type := b.ast_type_to_ssa(decl.typ.return_type)
	idx := b.mod.new_function(fn_name, ret_type, []TypeID{})
	b.fn_index[fn_name] = idx
	b.mod.func_set_prototype(idx, true)
	if decl.language == .c {
		b.mod.func_set_c_extern(idx, true)
	}
	// Register parameter types for correct forward declarations.
	// For methods, add receiver as the first parameter.
	// Skip for static methods (is_static=true) — they have no receiver in the call.
	if decl.is_method && !decl.is_static {
		recv_type := b.ast_type_to_ssa(decl.receiver.typ)
		// For &Type (PrefixExpr), ast_type_to_ssa already returns ptr(Type).
		// For mut receivers, the parser sets is_mut on the Parameter (not ModifierExpr),
		// so we need to add the pointer level.
		actual_type := if decl.receiver.is_mut {
			b.mod.type_store.get_ptr(recv_type)
		} else {
			recv_type
		}
		receiver_name := if decl.receiver.name != '' {
			decl.receiver.name
		} else {
			'self'
		}
		param_val := b.mod.add_value_node(.argument, actual_type, receiver_name, 0)
		b.mod.func_add_param(idx, param_val)
	}
	for param in decl.typ.params {
		param_type := b.ast_type_to_ssa(param.typ)
		// For `mut` params, add a pointer level for pass-by-reference semantics,
		// but NOT if the type is already a pointer (e.g., `mut buf &u8` → param is already ptr(i8)).
		// In C, `mut buf &u8` is just `u8* buf`, not `u8** buf`.
		actual_type := if param.is_mut && !(param_type < b.mod.type_store.types.len
			&& b.mod.type_store.types[param_type].kind == .ptr_t) {
			b.mod.type_store.get_ptr(param_type)
		} else {
			param_type
		}
		param_val := b.mod.add_value_node(.argument, actual_type, param.name, 0)
		b.mod.func_add_param(idx, param_val)
	}
	if param_array_elem_types.len > 0 {
		b.fn_param_array_elem_types[fn_name] = param_array_elem_types
	}
}

// register_fn_sig_from_flat (s243) is the cursor mirror of register_fn_sig.
// FnDecl flat: edge0=receiver (.aux_parameter: name in name_id, is_mut flag,
// typ at edge0), edge1=typ (.typ_fn: edge0=generics list, edge1=params list,
// edge2=return_type); language in aux; is_method/is_static in flags.
fn (mut b Builder) register_fn_sig_from_flat(c ast.Cursor) {
	fn_name := b.mangle_fn_name_from_flat(c)
	param_array_elem_types := b.fn_param_array_elem_types_from_decl_from_flat(c)
	if fn_name in b.fn_index {
		if param_array_elem_types.len > 0 {
			b.fn_param_array_elem_types[fn_name] = param_array_elem_types
		}
		return
	}

	fntyp_c := c.edge(1)
	ret_type := b.ast_type_to_ssa_from_flat(fntyp_c.edge(2))
	language := unsafe { ast.Language(int(c.aux())) }
	if !c.flag(ast.flag_is_method) && language != .c && !is_generated_helper_fn_name(c.name()) {
		b.module_fn_names[module_fn_key(b.cur_module, c.name())] = fn_name
	}
	idx := b.mod.new_function(fn_name, ret_type, []TypeID{})
	b.fn_index[fn_name] = idx
	b.mod.func_set_prototype(idx, true)
	if language == .c {
		b.mod.func_set_c_extern(idx, true)
	}
	// For methods, add receiver as the first parameter (skip static methods).
	if c.flag(ast.flag_is_method) && !c.flag(ast.flag_is_static) {
		recv_c := c.edge(0)
		recv_type := b.ast_type_to_ssa_from_flat(recv_c.edge(0))
		actual_type := if recv_c.flag(ast.flag_is_mut) {
			b.mod.type_store.get_ptr(recv_type)
		} else {
			recv_type
		}
		recv_name := recv_c.name()
		receiver_name := if recv_name != '' { recv_name } else { 'self' }
		param_val := b.mod.add_value_node(.argument, actual_type, receiver_name, 0)
		b.mod.func_add_param(idx, param_val)
	}
	params := fntyp_c.list_at(1)
	for pi in 0 .. params.len() {
		param_c := params.at(pi)
		param_type := b.ast_type_to_ssa_from_flat(param_c.edge(0))
		actual_type := if param_c.flag(ast.flag_is_mut) && !(param_type < b.mod.type_store.types.len
			&& b.mod.type_store.types[param_type].kind == .ptr_t) {
			b.mod.type_store.get_ptr(param_type)
		} else {
			param_type
		}
		param_val := b.mod.add_value_node(.argument, actual_type, param_c.name(), 0)
		b.mod.func_add_param(idx, param_val)
	}
	if param_array_elem_types.len > 0 {
		b.fn_param_array_elem_types[fn_name] = param_array_elem_types
	}
}

fn (mut b Builder) fn_param_array_elem_types_from_decl(decl ast.FnDecl) []TypeID {
	mut param_array_elem_types := []TypeID{}
	if decl.is_method && !decl.is_static {
		param_array_elem_types << b.array_elem_type_from_ast_type(decl.receiver.typ)
	}
	for param in decl.typ.params {
		param_array_elem_types << b.array_elem_type_from_ast_type(param.typ)
	}
	return param_array_elem_types
}

// fn_param_array_elem_types_from_decl_from_flat (s243) is the cursor mirror.
// receiver typ = edge(0).edge(0); params list = fntyp(edge1).list_at(1); each
// param's typ = edge(0). Reuses array_elem_type_from_ast_type_from_flat (s233).
fn (mut b Builder) fn_param_array_elem_types_from_decl_from_flat(c ast.Cursor) []TypeID {
	mut param_array_elem_types := []TypeID{}
	if c.flag(ast.flag_is_method) && !c.flag(ast.flag_is_static) {
		param_array_elem_types << b.array_elem_type_from_ast_type_from_flat(c.edge(0).edge(0))
	}
	params := c.edge(1).list_at(1)
	for pi in 0 .. params.len() {
		param_array_elem_types << b.array_elem_type_from_ast_type_from_flat(params.at(pi).edge(0))
	}
	return param_array_elem_types
}

fn (mut b Builder) mangle_fn_name(decl ast.FnDecl) string {
	if decl.is_method {
		receiver_name := b.receiver_type_name(decl.receiver.typ)
		return '${receiver_name}__${decl.name}'
	}
	if is_generated_helper_fn_name(decl.name) {
		return decl.name
	}
	// C functions use their bare name (no module prefix)
	if decl.language == .c {
		return decl.name
	}
	if decl.name == 'main' {
		return 'main'
	}
	if b.cur_module != '' && b.cur_module != 'main' {
		// Don't add module prefix if name already starts with it (e.g., generated
		// enum str functions placed back in their source module).
		if decl.name.starts_with('${b.cur_module}__') {
			return decl.name
		}
		return '${b.cur_module}__${decl.name}'
	}
	return decl.name
}

// mangle_fn_name_from_flat (s243) is the cursor mirror of mangle_fn_name. FnDecl
// flat: name in name_id, language in aux, is_method flag; receiver = edge(0)
// (.aux_parameter), its type = edge(0).edge(0).
fn (mut b Builder) mangle_fn_name_from_flat(c ast.Cursor) string {
	name_str := c.name()
	if c.flag(ast.flag_is_method) {
		receiver_name := b.receiver_type_name_from_flat(c.edge(0).edge(0))
		return '${receiver_name}__${name_str}'
	}
	if is_generated_helper_fn_name(name_str) {
		return name_str
	}
	language := unsafe { ast.Language(int(c.aux())) }
	// C functions use their bare name (no module prefix)
	if language == .c {
		return name_str
	}
	if name_str == 'main' {
		return 'main'
	}
	if b.cur_module != '' && b.cur_module != 'main' {
		if name_str.starts_with('${b.cur_module}__') {
			return name_str
		}
		return '${b.cur_module}__${name_str}'
	}
	return name_str
}

fn (mut b Builder) checked_fn_scope_key(decl ast.FnDecl) string {
	scope_fn_name := if decl.is_method {
		mut recv_name := b.receiver_type_name(decl.receiver.typ)
		if b.cur_module != '' {
			prefix := '${b.cur_module}__'
			if recv_name.starts_with(prefix) {
				recv_name = recv_name[prefix.len..]
			}
		}
		'${recv_name}__${decl.name}'
	} else {
		decl.name
	}
	if b.cur_module == '' {
		return scope_fn_name
	}
	return '${b.cur_module}__${scope_fn_name}'
}

// checked_fn_scope_key_from_flat (s244) is the cursor mirror of checked_fn_scope_key.
fn (mut b Builder) checked_fn_scope_key_from_flat(c ast.Cursor) string {
	name_str := c.name()
	scope_fn_name := if c.flag(ast.flag_is_method) {
		mut recv_name := b.receiver_type_name_from_flat(c.edge(0).edge(0))
		if b.cur_module != '' {
			prefix := '${b.cur_module}__'
			if recv_name.starts_with(prefix) {
				recv_name = recv_name[prefix.len..]
			}
		}
		'${recv_name}__${name_str}'
	} else {
		name_str
	}
	if b.cur_module == '' {
		return scope_fn_name
	}
	return '${b.cur_module}__${scope_fn_name}'
}

fn is_generated_helper_fn_name(name string) bool {
	return name.starts_with('Array_') || name.starts_with('Map_') || name.starts_with('__sort_cmp_')
		|| name.starts_with('__go_wrap_') || name.starts_with('__go_entry_')
}

fn (mut b Builder) receiver_type_name(typ ast.Expr) string {
	match typ {
		ast.Ident {
			if b.cur_module != '' && b.cur_module != 'main' {
				return '${b.cur_module}__${typ.name}'
			}
			return typ.name
		}
		ast.PrefixExpr {
			return b.receiver_type_name(typ.expr)
		}
		ast.ModifierExpr {
			return b.receiver_type_name(typ.expr)
		}
		ast.SelectorExpr {
			return '${typ.lhs.name()}__${typ.rhs.name}'
		}
		ast.Type {
			// Handle type expressions used as receivers (e.g., []rune for (ra []rune) string())
			inner := ast.Type(typ)
			if inner is ast.PointerType {
				return b.receiver_type_name(inner.base_type)
			}
			if inner is ast.ArrayType {
				// []rune → Array_rune, []int → Array_int, etc.
				elem_name := if inner.elem_type is ast.Ident {
					inner.elem_type.name
				} else {
					b.receiver_type_name(inner.elem_type)
				}
				prefix := if b.cur_module != '' && b.cur_module != 'main' {
					'${b.cur_module}__'
				} else {
					''
				}
				return '${prefix}Array_${elem_name}'
			}
			return 'unknown'
		}
		else {
			return 'unknown'
		}
	}
}

// receiver_type_name_from_flat (s243) is the cursor mirror of receiver_type_name.
// Ident/Prefix/Modifier/Selector are expr kinds; PointerType/ArrayType receivers
// are `.typ_pointer`/`.typ_array` nodes (base/elem at edge(0)).
fn (mut b Builder) receiver_type_name_from_flat(typ_c ast.Cursor) string {
	match typ_c.kind() {
		.expr_ident {
			if b.cur_module != '' && b.cur_module != 'main' {
				return '${b.cur_module}__${typ_c.name()}'
			}
			return typ_c.name()
		}
		.expr_prefix {
			return b.receiver_type_name_from_flat(typ_c.edge(0))
		}
		.expr_modifier {
			return b.receiver_type_name_from_flat(typ_c.edge(0))
		}
		.expr_selector {
			lhs_c := typ_c.edge(0)
			lhs_name := if lhs_c.kind() == .expr_ident { lhs_c.name() } else { '' }
			rhs_name := typ_c.edge(1).name()
			return '${lhs_name}__${rhs_name}'
		}
		.typ_pointer {
			return b.receiver_type_name_from_flat(typ_c.edge(0))
		}
		.typ_array {
			elem_c := typ_c.edge(0)
			elem_name := if elem_c.kind() == .expr_ident {
				elem_c.name()
			} else {
				b.receiver_type_name_from_flat(elem_c)
			}
			prefix := if b.cur_module != '' && b.cur_module != 'main' {
				'${b.cur_module}__'
			} else {
				''
			}
			return '${prefix}Array_${elem_name}'
		}
		else {
			return 'unknown'
		}
	}
}

// --- Phase 3: Build function bodies ---

pub fn (mut b Builder) build_fn_bodies(file ast.File) {
	b.selective_import_fn_names = selective_import_fn_names_from_imports(file.imports)
	b.selective_import_fn_candidates = selective_import_fn_candidates_from_imports(file.imports)
	b.module_import_aliases = module_import_aliases_from_imports(file.imports)
	nstmts := file.stmts.len
	for si in 0 .. nstmts {
		if file.stmts[si] is ast.FnDecl {
			decl := file.stmts[si] as ast.FnDecl
			if decl.language == .c && decl.stmts.len == 0 {
				continue
			}
			if decl.typ.generic_params.len > 0 {
				continue
			}
			// In hot_fn mode, only build the target function
			if b.hot_fn.len > 0 {
				mangled := b.mangle_fn_name(decl)
				if mangled != b.hot_fn {
					continue
				}
			}
			// Dead code elimination: skip functions not reachable from main
			if !b.should_build_fn(file.name, decl) {
				continue
			}
			b.build_fn(decl)
		}
	}
}

// build_fn_bodies_from_flat is the flat-cursor counterpart of `build_fn_bodies`.
// Walks one file's top-level stmts via FileCursor; only `.stmt_fn_decl` nodes
// are touched. Filtering and body building read the FnDecl cursor directly, and
// body stmts are walked via cursors.
pub fn (mut b Builder) build_fn_bodies_from_flat(file_cursor ast.FileCursor) {
	file_name := file_cursor.name()
	imports := file_cursor.imports().import_stmts()
	b.selective_import_fn_names = selective_import_fn_names_from_imports(imports)
	b.selective_import_fn_candidates = selective_import_fn_candidates_from_imports(imports)
	b.module_import_aliases = module_import_aliases_from_imports(imports)
	stmts := file_cursor.stmts()
	for si in 0 .. stmts.len() {
		c := stmts.at(si)
		if c.kind() != .stmt_fn_decl {
			continue
		}
		body := c.list_at(3)
		language := unsafe { ast.Language(int(c.aux())) }
		if language == .c && body.len() == 0 {
			continue
		}
		if c.edge(1).list_at(0).len() > 0 {
			continue
		}
		if b.hot_fn.len > 0 {
			mangled := b.mangle_fn_name_from_flat(c)
			if mangled != b.hot_fn {
				continue
			}
		}
		if !b.should_build_fn_from_flat(file_name, c) {
			continue
		}
		b.build_fn_from_flat(c)
	}
}

// should_build_fn returns true if the function should be compiled.
// When used_fn_keys is populated (markused ran), only reachable functions are built.
pub fn (mut b Builder) should_build_fn(file_name string, decl ast.FnDecl) bool {
	// Skip entire modules for unused backends (e.g., cleanc/eval/x64 when building arm64-only)
	if b.should_skip_module_file(file_name) {
		return false
	}
	if b.used_fn_keys.len == 0 {
		return true // No markused data — build everything
	}
	if decl.name == 'main' {
		return true
	}
	// Always build init_consts, init, deinit unless a minimal native runtime path
	// relies on markused to keep only roots that are actually reached.
	if decl.name.starts_with('__v_init_consts_') || decl.name == 'init' || decl.name == 'deinit' {
		if b.minimal_runtime_roots {
			key := markused.decl_key(b.cur_module, decl, b.env)
			return key in b.used_fn_keys
		}
		return true
	}
	// Always build functions from core modules that the runtime needs
	if !b.minimal_runtime_roots
		&& b.cur_module in ['builtin', 'strings', 'strconv', 'bits', 'sha256', 'binary'] {
		return true
	}
	// Always build .vh header declarations
	if file_name.ends_with('.vh') {
		return true
	}
	// Keep transformer-generated array/map method specializations
	if decl.is_method {
		mangled := b.mangle_fn_name(decl)
		if !b.minimal_runtime_roots && (mangled.contains('__Array_') || mangled.contains('__Map_')) {
			return true
		}
	}
	// Check markused reachability
	key := markused.decl_key(b.cur_module, decl, b.env)
	return key in b.used_fn_keys
}

fn (mut b Builder) should_build_fn_from_flat(file_name string, c ast.Cursor) bool {
	if b.should_skip_module_file(file_name) {
		return false
	}
	if b.used_fn_keys.len == 0 {
		return true
	}
	name := c.name()
	if name == 'main' {
		return true
	}
	if name.starts_with('__v_init_consts_') || name == 'init' || name == 'deinit' {
		if b.minimal_runtime_roots {
			key := markused.decl_key_from_cursor(b.cur_module, c, b.env)
			return key in b.used_fn_keys
		}
		return true
	}
	if !b.minimal_runtime_roots
		&& b.cur_module in ['builtin', 'strings', 'strconv', 'bits', 'sha256', 'binary'] {
		return true
	}
	if file_name.ends_with('.vh') {
		return true
	}
	if c.flag(ast.flag_is_method) {
		mangled := b.mangle_fn_name_from_flat(c)
		if !b.minimal_runtime_roots && (mangled.contains('__Array_') || mangled.contains('__Map_')) {
			return true
		}
	}
	key := markused.decl_key_from_cursor(b.cur_module, c, b.env)
	return key in b.used_fn_keys
}

fn (b &Builder) should_skip_module_file(file_name string) bool {
	if b.skip_modules.len == 0 || b.cur_module !in b.skip_modules {
		return false
	}
	if b.skip_module_file_fragments.len == 0 {
		return true
	}
	fragment := b.skip_module_file_fragments[b.cur_module] or { return false }
	normalized := file_name.replace('\\', '/')
	if normalized.contains(fragment) {
		return true
	}
	if fragment.len > 0 && fragment[0] == `/` {
		return normalized.starts_with(fragment[1..])
	}
	return false
}

pub fn (mut b Builder) build_fn(decl ast.FnDecl) {
	fn_name := b.mangle_fn_name(decl)
	// Skip C-language extern functions without bodies
	if decl.language == .c {
		return
	}
	func_idx := b.fn_index[fn_name] or { return }
	// Skip if already built (can happen with .c.v and .v files).
	// Exception: user-defined methods always override auto-generated non-method functions
	// (e.g., custom Token.str() overrides auto-generated token__Token__str enum str).
	if b.mod.funcs[func_idx].blocks.len > 0 {
		if decl.is_method && decl.stmts.len > 0 {
			// Method with a body overrides previously-built auto-generated function.
			// Clear blocks so we can rebuild with the real method body.
			b.mod.func_clear_blocks(func_idx)
		} else {
			return
		}
	}

	// Skip functions without a body (e.g., extern declarations).
	// Build function bodies for ALL modules so cross-module calls work at runtime.
	if decl.stmts.len == 0 {
		// Emit a minimal function body (entry + ret) so backends have a valid function
		b.cur_func = func_idx
		entry := b.mod.add_block(func_idx, 'entry')
		b.cur_block = entry
		ret_type := b.mod.funcs[func_idx].typ
		if ret_type != 0 {
			ret_type_info := b.mod.type_store.types[ret_type]
			if ret_type_info.kind == .struct_t {
				// For struct return types, alloca + zero init + load
				ptr_type := b.mod.type_store.get_ptr(ret_type)
				alloca := b.mod.add_instr(.alloca, b.cur_block, ptr_type, []ValueID{})
				ret_val := b.mod.add_instr(.load, b.cur_block, ret_type, [alloca])
				b.mod.add_instr(.ret, b.cur_block, 0, [ret_val])
			} else {
				zero := b.mod.get_or_add_const(ret_type, '0')
				b.mod.add_instr(.ret, b.cur_block, 0, [zero])
			}
		} else {
			b.mod.add_instr(.ret, b.cur_block, 0, []ValueID{})
		}
		return
	}

	b.cur_func = func_idx
	b.cur_fn_scope_key = b.checked_fn_scope_key(decl)

	// Reset local variables
	b.vars = map[string]ValueID{}
	b.mut_ptr_params = map[string]bool{}
	b.local_smartcasts = map[string]TypeID{}
	b.label_blocks = map[string]BlockID{}
	b.array_elem_types = map[string]TypeID{}

	// Clear params (they were registered in register_fn_sig for forward decls,
	// but we need to re-create them here with proper alloca bindings)
	b.mod.func_set_params(func_idx, []ValueID{})

	// Create entry block
	entry := b.mod.add_block(func_idx, 'entry')
	b.cur_block = entry

	// Add parameters
	// Skip receiver for static methods (is_static=true) — no receiver in call
	if decl.is_method && !decl.is_static {
		// Receiver is the first parameter
		receiver_name := if decl.receiver.name != '' {
			decl.receiver.name
		} else {
			'self'
		}
		recv_type := b.ast_type_to_ssa(decl.receiver.typ)
		// For &Type (PrefixExpr), ast_type_to_ssa already returns ptr(Type).
		// For mut receivers, the parser sets is_mut on the Parameter (not ModifierExpr),
		// so we need to add the pointer level.
		actual_type := if decl.receiver.is_mut {
			b.mod.type_store.get_ptr(recv_type)
		} else {
			recv_type
		}
		param_val := b.mod.add_value_node(.argument, actual_type, receiver_name, 0)
		b.mod.func_add_param(func_idx, param_val)
		// Alloca + store for receiver
		alloca := b.mod.add_instr(.alloca, entry, b.mod.type_store.get_ptr(actual_type),
			[]ValueID{})
		b.mod.add_instr(.store, entry, 0, [param_val, alloca])
		b.vars[receiver_name] = alloca
	}

	for param in decl.typ.params {
		param_type := b.ast_type_to_ssa(param.typ)
		// For `mut` params, add pointer level only if the type isn't already a pointer.
		// `mut buf &u8` → param_type is ptr(i8), no extra level needed (like C: u8* buf).
		// `mut val int` → param_type is i32, needs ptr(i32) for pass-by-reference.
		is_already_ptr := param_type < b.mod.type_store.types.len
			&& b.mod.type_store.types[param_type].kind == .ptr_t
		actual_type := if param.is_mut && !is_already_ptr {
			b.mod.type_store.get_ptr(param_type)
		} else {
			param_type
		}
		param_val := b.mod.add_value_node(.argument, actual_type, param.name, 0)
		b.mod.func_add_param(func_idx, param_val)
		// Alloca + store
		alloca := b.mod.add_instr(.alloca, entry, b.mod.type_store.get_ptr(actual_type),
			[]ValueID{})
		b.mod.add_instr(.store, entry, 0, [param_val, alloca])
		b.vars[param.name] = alloca
		if param.is_mut && !is_already_ptr {
			b.mut_ptr_params[param.name] = true
		}

		// Track array element types for transformer-generated functions (no checker info).
		// E.g., param 'a' with type 'Array_int' → element type is 'int' → i32.
		if param.typ is ast.Ident {
			param_type_name := param.typ.name
			if param_type_name.starts_with('Array_') {
				elem_name := param_type_name['Array_'.len..]
				elem_ssa := b.ident_type_to_ssa(elem_name)
				if elem_ssa != 0 {
					b.array_elem_types[param.name] = elem_ssa
				}
			}
		}
	}

	// Build body
	b.build_stmts(decl.stmts)

	// If no terminator, add implicit return
	if !b.block_has_terminator(b.cur_block) {
		if fn_name == 'main' {
			zero := b.mod.get_or_add_const(b.mod.type_store.get_int(32), '0')
			b.mod.add_instr(.ret, b.cur_block, 0, [zero])
		} else {
			b.mod.add_instr(.ret, b.cur_block, 0, []ValueID{})
		}
	}
}

// build_fn_from_flat is the flat-cursor counterpart of `build_fn`. Takes a
// `.stmt_fn_decl` cursor directly: signature fields come from cursor edges, and
// body stmts are walked via `c.list_at(3)` + `build_stmts_from_flat`. No
// `[]ast.Stmt` rehydration of the body remains.
pub fn (mut b Builder) build_fn_from_flat(c ast.Cursor) {
	// s244: read the FnDecl signature straight from the cursor — no decode.
	// FnDecl flat: name in name_id, language in aux, is_method/is_static flags;
	// edge0=receiver (.aux_parameter), edge1=typ (.typ_fn), edge3=body stmts.
	is_method := c.flag(ast.flag_is_method)
	fn_name := b.mangle_fn_name_from_flat(c)
	// Skip C-language extern functions without bodies
	if unsafe { ast.Language(int(c.aux())) } == .c {
		return
	}
	func_idx := b.fn_index[fn_name] or { return }
	body := c.list_at(3)
	body_len := body.len()
	// Skip if already built (can happen with .c.v and .v files).
	// Exception: user-defined methods always override auto-generated non-method functions.
	if b.mod.funcs[func_idx].blocks.len > 0 {
		if is_method && body_len > 0 {
			b.mod.func_clear_blocks(func_idx)
		} else {
			return
		}
	}

	// Skip functions without a body (e.g., extern declarations).
	if body_len == 0 {
		b.cur_func = func_idx
		entry := b.mod.add_block(func_idx, 'entry')
		b.cur_block = entry
		ret_type := b.mod.funcs[func_idx].typ
		if ret_type != 0 {
			ret_type_info := b.mod.type_store.types[ret_type]
			if ret_type_info.kind == .struct_t {
				ptr_type := b.mod.type_store.get_ptr(ret_type)
				alloca := b.mod.add_instr(.alloca, b.cur_block, ptr_type, []ValueID{})
				ret_val := b.mod.add_instr(.load, b.cur_block, ret_type, [alloca])
				b.mod.add_instr(.ret, b.cur_block, 0, [ret_val])
			} else {
				zero := b.mod.get_or_add_const(ret_type, '0')
				b.mod.add_instr(.ret, b.cur_block, 0, [zero])
			}
		} else {
			b.mod.add_instr(.ret, b.cur_block, 0, []ValueID{})
		}
		return
	}

	b.cur_func = func_idx
	b.cur_fn_scope_key = b.checked_fn_scope_key_from_flat(c)

	// Reset per-fn state
	b.vars = map[string]ValueID{}
	b.mut_ptr_params = map[string]bool{}
	b.local_smartcasts = map[string]TypeID{}
	b.label_blocks = map[string]BlockID{}
	b.array_elem_types = map[string]TypeID{}

	b.mod.func_set_params(func_idx, []ValueID{})

	entry := b.mod.add_block(func_idx, 'entry')
	b.cur_block = entry

	// Add receiver param (skip for static methods). receiver = edge0 (.aux_parameter:
	// name in name_id, is_mut flag, typ at edge0).
	if is_method && !c.flag(ast.flag_is_static) {
		recv_c := c.edge(0)
		recv_name := recv_c.name()
		receiver_name := if recv_name != '' { recv_name } else { 'self' }
		recv_type := b.ast_type_to_ssa_from_flat(recv_c.edge(0))
		actual_type := if recv_c.flag(ast.flag_is_mut) {
			b.mod.type_store.get_ptr(recv_type)
		} else {
			recv_type
		}
		param_val := b.mod.add_value_node(.argument, actual_type, receiver_name, 0)
		b.mod.func_add_param(func_idx, param_val)
		alloca := b.mod.add_instr(.alloca, entry, b.mod.type_store.get_ptr(actual_type),
			[]ValueID{})
		b.mod.add_instr(.store, entry, 0, [param_val, alloca])
		b.vars[receiver_name] = alloca
	}

	// params = typ(.typ_fn = edge1).list_at(1); each is an .aux_parameter.
	params := c.edge(1).list_at(1)
	for pi in 0 .. params.len() {
		param_c := params.at(pi)
		param_name := param_c.name()
		typ_c := param_c.edge(0)
		param_type := b.ast_type_to_ssa_from_flat(typ_c)
		is_already_ptr := param_type < b.mod.type_store.types.len
			&& b.mod.type_store.types[param_type].kind == .ptr_t
		actual_type := if param_c.flag(ast.flag_is_mut) && !is_already_ptr {
			b.mod.type_store.get_ptr(param_type)
		} else {
			param_type
		}
		param_val := b.mod.add_value_node(.argument, actual_type, param_name, 0)
		b.mod.func_add_param(func_idx, param_val)
		alloca := b.mod.add_instr(.alloca, entry, b.mod.type_store.get_ptr(actual_type),
			[]ValueID{})
		b.mod.add_instr(.store, entry, 0, [param_val, alloca])
		b.vars[param_name] = alloca
		if param_c.flag(ast.flag_is_mut) && !is_already_ptr {
			b.mut_ptr_params[param_name] = true
		}

		if typ_c.kind() == .expr_ident {
			param_type_name := typ_c.name()
			if param_type_name.starts_with('Array_') {
				elem_name := param_type_name['Array_'.len..]
				elem_ssa := b.ident_type_to_ssa(elem_name)
				if elem_ssa != 0 {
					b.array_elem_types[param_name] = elem_ssa
				}
			}
		}
	}

	// Build body via flat cursors — no []ast.Stmt rehydration
	b.build_stmts_from_flat(body)

	if !b.block_has_terminator(b.cur_block) {
		if fn_name == 'main' {
			zero := b.mod.get_or_add_const(b.mod.type_store.get_int(32), '0')
			b.mod.add_instr(.ret, b.cur_block, 0, [zero])
		} else {
			b.mod.add_instr(.ret, b.cur_block, 0, []ValueID{})
		}
	}
}

fn (mut b Builder) is_mut_receiver(typ ast.Expr) bool {
	if typ is ast.ModifierExpr {
		return typ.kind == .key_mut || typ.kind == .key_shared
	}
	if typ is ast.PrefixExpr {
		return typ.op == .amp
	}
	return false
}

fn (mut b Builder) block_has_terminator(block BlockID) bool {
	if block < 0 || block >= b.mod.blocks.len {
		return true
	}
	instrs := b.mod.blocks[block].instrs
	if instrs.len == 0 {
		return false
	}
	last := instrs[instrs.len - 1]
	if !b.valid_value_id(last) {
		return false
	}
	last_val := b.mod.values[last]
	if last_val.kind != .instruction || last_val.index < 0 || last_val.index >= b.mod.instrs.len {
		return false
	}
	last_instr := b.mod.instrs[last_val.index]
	return last_instr.op in [.ret, .br, .jmp, .unreachable]
}

// --- Statement building ---

fn builder_sumtype_payload_word_is_valid(tag_word u64, data_word u64) bool {
	if data_word == 0 {
		return false
	}
	// Native v2 backends use `(tag, data_ptr)` for sumtypes. If the first word
	// looks like a small tag, the payload must be a real pointer, not a leaked
	// enum/default value like `3`.
	if tag_word < 256 {
		upper := data_word >> 32
		lower := data_word & u64(0xffffffff)
		if upper < 1024 && lower < 4096 {
			return false
		}
		return data_word >= 4096 && data_word < 281474976710656
	}
	return true
}

fn builder_stmt_ok(stmt ast.Stmt) bool {
	tag_word := unsafe { (&u64(&stmt))[0] }
	data_word := unsafe { (&u64(&stmt))[1] }
	return builder_sumtype_payload_word_is_valid(tag_word, data_word)
}

fn builder_expr_ok(expr ast.Expr) bool {
	tag_word := unsafe { (&u64(&expr))[0] }
	data_word := unsafe { (&u64(&expr))[1] }
	return builder_sumtype_payload_word_is_valid(tag_word, data_word)
}

fn (mut b Builder) build_stmts(stmts []ast.Stmt) {
	for stmt in stmts {
		b.build_stmt(stmt)
	}
}

// build_stmts_from_flat is the cursor-list counterpart of `build_stmts`.
// First step of the s175+ port arc: today every cursor decodes back to a
// legacy `ast.Stmt` and dispatches through `build_stmt`. Future sessions
// port individual statement classes (ReturnStmt, AssignStmt, IfStmt, ForStmt,
// etc.) so each kind drops its `decode_stmt` call in favour of direct
// cursor + sub-cursor walking inside the SSA builder.
fn (mut b Builder) build_stmts_from_flat(stmts ast.CursorList) {
	for si in 0 .. stmts.len() {
		b.build_stmt_from_flat(stmts.at(si))
	}
}

// build_stmt_from_flat is the cursor counterpart of `build_stmt`. As of s234
// every statement kind is dispatched explicitly: the value-producing/structural
// kinds go to their `build_<kind>_from_flat` helpers; the kinds that `build_stmt`
// treats as no-ops (decls / imports / directives / defer / comptime / asm /
// attributes / empty — all lowered or registered in earlier phases) are explicit
// no-op arms; ForInStmt panics exactly as `build_stmt` does (the transformer must
// have lowered it). No `decode_stmt` remains — build_stmt_from_flat is fully
// cursor-native. The `else` is an unreachable defensive no-op (only stmt cursors
// are ever passed here, and all stmt kinds are enumerated).
fn (mut b Builder) build_stmt_from_flat(c ast.Cursor) {
	match c.kind() {
		.stmt_return {
			b.build_return_from_flat(c)
		}
		.stmt_flow_control {
			b.build_flow_control_from_flat(c)
		}
		.stmt_label {
			b.build_label_from_flat(c)
		}
		.stmt_block {
			b.build_block_from_flat(c)
		}
		.stmt_module {
			b.build_module_from_flat(c)
		}
		.stmt_assert {
			b.build_assert_from_flat(c)
		}
		.stmt_expr {
			b.build_expr_stmt_from_flat(c)
		}
		.stmt_for {
			b.build_for_from_flat(c)
		}
		.stmt_assign {
			b.build_assign_from_flat(c)
		}
		// No-op kinds: `build_stmt` has empty bodies for these (handled by the
		// transformer / earlier registration passes), so decoding them only to
		// no-op is wasteful — match and ignore them directly.
		.stmt_import, .stmt_const_decl, .stmt_struct_decl, .stmt_enum_decl, .stmt_type_decl,
		.stmt_interface_decl, .stmt_global_decl, .stmt_fn_decl, .stmt_directive, .stmt_comptime,
		.stmt_defer, .stmt_attributes, .stmt_empty, .stmt_asm {}
		.stmt_for_in {
			panic('SSA builder: ForInStmt should have been lowered by transformer')
		}
		else {}
	}
}

// build_return_from_flat is the cursor counterpart of `build_return`. The
// flat schema stores ReturnStmt's `exprs` as direct child edges of the
// `.stmt_return` node (see `flat.v`'s ReturnStmt arm in `add_stmt`), so the
// expr count is `c.edge_count()` and each `c.edge(i)` is an expr cursor.
// Expressions are lowered through `build_expr_from_flat` so ReturnStmt no
// longer rehydrates rhs expressions before dispatch.
fn (mut b Builder) build_return_from_flat(c ast.Cursor) {
	fn_ret_type := if b.cur_func >= 0 && b.cur_func < b.mod.funcs.len {
		b.mod.funcs[b.cur_func].typ
	} else {
		TypeID(0)
	}
	is_option_ret := b.is_option_wrapper_type(fn_ret_type)
	is_result_ret := b.is_result_wrapper_type(fn_ret_type)
	nexprs := c.edge_count()
	if nexprs == 0 {
		if is_option_ret || is_result_ret {
			b.mod.add_instr(.ret, b.cur_block, 0, [
				b.build_wrapper_value(fn_ret_type, true, 0, false),
			])
		} else {
			b.mod.add_instr(.ret, b.cur_block, 0, []ValueID{})
		}
	} else if nexprs == 1 {
		ret_expr_c := c.edge(0)
		mut val := b.build_expr_from_flat(ret_expr_c)
		if (is_option_ret || is_result_ret) && b.mod.values[val].typ != fn_ret_type {
			val = b.coerce_wrapper_value_from_flat(ret_expr_c, val, fn_ret_type)
		} else if fn_ret_type > 0 && int(fn_ret_type) < b.mod.type_store.types.len
			&& b.mod.type_store.types[fn_ret_type].kind == .float_t {
			val_type := b.mod.values[val].typ
			if val_type > 0 && int(val_type) < b.mod.type_store.types.len
				&& b.mod.type_store.types[val_type].kind != .float_t {
				val = b.mod.add_instr(.sitofp, b.cur_block, fn_ret_type, [val])
			}
		}
		b.mod.add_instr(.ret, b.cur_block, 0, [val])
	} else {
		mut elem_types := []TypeID{cap: nexprs}
		mut vals := []ValueID{cap: nexprs}
		for i in 0 .. nexprs {
			ec := c.edge(i)
			v := b.build_expr_from_flat(ec)
			vals << v
			elem_types << b.mod.values[v].typ
		}
		tuple_type := b.mod.type_store.get_tuple(elem_types)
		mut tuple_val := b.mod.get_or_add_const(tuple_type, 'undef')
		for i, v in vals {
			idx := b.mod.get_or_add_const(b.mod.type_store.get_int(32), i.str())
			tuple_val = b.mod.add_instr(.insertvalue, b.cur_block, tuple_type, [
				tuple_val,
				v,
				idx,
			])
		}
		if (is_option_ret || is_result_ret) && tuple_type != fn_ret_type {
			tuple_val = b.build_wrapper_value(fn_ret_type, true, tuple_val, true)
		}
		b.mod.add_instr(.ret, b.cur_block, 0, [tuple_val])
	}
}

fn (b &Builder) is_none_expr_from_flat(c ast.Cursor) bool {
	match c.kind() {
		.expr_ident {
			return c.name() == 'none'
		}
		.expr_keyword {
			tok := unsafe { token.Token(int(c.aux())) }
			return tok == .key_none
		}
		.typ_none {
			return true
		}
		else {
			return false
		}
	}
}

fn (b &Builder) is_error_expr_from_flat(c ast.Cursor) bool {
	error_fn_names := ['error', 'error_posix', 'error_with_code', 'error_win32']
	match c.kind() {
		.expr_ident {
			return c.name() == 'err'
		}
		.expr_call, .expr_call_or_cast {
			lhs := c.edge(0)
			return lhs.kind() == .expr_ident && lhs.name() in error_fn_names
		}
		else {
			return false
		}
	}
}

fn (mut b Builder) coerce_wrapper_value_from_flat(c ast.Cursor, val ValueID, wrapper_type TypeID) ValueID {
	if !b.is_wrapper_type(wrapper_type) {
		return val
	}
	if b.is_none_expr_from_flat(c) {
		return b.build_wrapper_value(wrapper_type, false, 0, false)
	}
	if b.is_error_expr_from_flat(c) {
		return b.build_wrapper_value(wrapper_type, false, val, false)
	}
	if b.is_result_wrapper_type(wrapper_type) && b.cursor_is_ierror_like(c)
		&& !b.wrapper_value_is_valid_payload(val, wrapper_type) {
		err_payload := b.ierror_tag_for_cursor(c) or { val }
		return b.build_wrapper_value(wrapper_type, false, err_payload, false)
	}
	if val > 0 && val < b.mod.values.len && b.mod.values[val].typ == wrapper_type {
		if payload := b.wrapper_payload_bitcast_source(val, wrapper_type) {
			return b.build_wrapper_value(wrapper_type, true, payload, true)
		}
		match b.mod.values[val].kind {
			.argument, .global, .instruction {
				return val
			}
			else {}
		}
	}
	return b.build_wrapper_value(wrapper_type, true, val, true)
}

fn (mut b Builder) build_stmt(stmt ast.Stmt) {
	if !builder_stmt_ok(stmt) {
		return
	}
	match stmt {
		ast.AssignStmt {
			b.build_assign(stmt)
		}
		ast.ExprStmt {
			b.build_expr_stmt(stmt)
		}
		ast.ReturnStmt {
			b.build_return(stmt)
		}
		ast.ForStmt {
			b.build_for(stmt)
		}
		ast.FlowControlStmt {
			b.build_flow_control(stmt)
		}
		ast.BlockStmt {
			b.build_stmts(stmt.stmts)
		}
		ast.LabelStmt {
			b.build_label(stmt)
		}
		ast.ModuleStmt {
			b.cur_module = stmt.name.replace('.', '_')
		}
		ast.ImportStmt {}
		ast.ConstDecl {}
		ast.StructDecl {}
		ast.EnumDecl {}
		ast.TypeDecl {}
		ast.InterfaceDecl {}
		ast.GlobalDecl {}
		ast.FnDecl {} // nested fn decls handled separately
		ast.Directive {}
		ast.ComptimeStmt {}
		ast.DeferStmt {}
		ast.AssertStmt {
			b.build_assert(stmt)
		}
		[]ast.Attribute {}
		ast.EmptyStmt {}
		ast.AsmStmt {}
		ast.ForInStmt {
			panic('SSA builder: ForInStmt should have been lowered by transformer')
		}
	}
}

fn (mut b Builder) build_expr_stmt(stmt ast.ExprStmt) {
	if stmt.expr is ast.UnsafeExpr {
		for s in stmt.expr.stmts {
			b.build_stmt(s)
		}
		return
	}
	if stmt.expr is ast.IfExpr {
		b.build_if_stmt(stmt.expr)
		return
	}
	b.build_expr(stmt.expr)
}

fn (mut b Builder) unwrap_ident(expr ast.Expr) ?ast.Ident {
	if expr is ast.Ident {
		return expr
	}
	if expr is ast.ModifierExpr {
		return b.unwrap_ident(expr.expr)
	}
	return none
}

fn (mut b Builder) assign_target_for_ident(name string) ValueID {
	if p := b.vars[name] {
		if name in b.mut_ptr_params {
			ptr_typ := b.mod.values[p].typ
			if b.valid_type_id(ptr_typ) {
				elem_typ := b.mod.type_store.types[ptr_typ].elem_type
				if elem_typ > 0 {
					return b.mod.add_instr(.load, b.cur_block, elem_typ, [p])
				}
			}
		}
		return p
	}
	if glob_id := b.find_global_ident(name) {
		return glob_id
	}
	return 0
}

fn (mut b Builder) build_assign(stmt ast.AssignStmt) {
	// Multi-return decomposition: when LHS has more vars than RHS,
	// the single RHS is a function call returning a tuple.
	if stmt.lhs.len > 1 && stmt.rhs.len == 1 {
		mut rhs_val := b.build_expr(stmt.rhs[0])
		mut rhs_typ_id := b.mod.values[rhs_val].typ
		mut rhs_typ := b.mod.type_store.types[rhs_typ_id]
		// If RHS is an Option/Result wrapper, unwrap the data field first.
		// The transformer strips the ? postfix in tuple destructuring, leaving
		// the raw wrapper struct. Extract .data (field 2) to get the inner tuple.
		if b.is_option_wrapper_type(rhs_typ_id) || b.is_result_wrapper_type(rhs_typ_id) {
			if b.wrapper_has_data(rhs_typ_id) && rhs_typ.kind == .struct_t
				&& rhs_typ.fields.len >= 3 {
				data_type := rhs_typ.fields[2]
				data_idx := b.mod.get_or_add_const(b.mod.type_store.get_int(32), '2')
				rhs_val = b.mod.add_instr(.extractvalue, b.cur_block, data_type, [
					rhs_val,
					data_idx,
				])
				rhs_typ_id = data_type
				rhs_typ = b.mod.type_store.types[rhs_typ_id]
			}
		}
		for i, lhs in stmt.lhs {
			// Extract each element from the tuple
			mut elem_val := rhs_val
			if rhs_typ.kind == .struct_t && i < rhs_typ.fields.len {
				elem_type := rhs_typ.fields[i]
				idx_val := b.mod.get_or_add_const(b.mod.type_store.get_int(32), i.str())
				elem_val = b.mod.add_instr(.extractvalue, b.cur_block, elem_type, [
					rhs_val,
					idx_val,
				])
			}
			if ident := b.unwrap_ident(lhs) {
				if stmt.op == .decl_assign {
					elem_type := b.mod.values[elem_val].typ
					alloca := b.mod.add_instr(.alloca, b.cur_block,
						b.mod.type_store.get_ptr(elem_type), []ValueID{})
					b.mod.add_instr(.store, b.cur_block, 0, [elem_val, alloca])
					b.vars[ident.name] = alloca
					b.track_array_elem_type_for_local_value(ident.name, elem_val)
				} else if ident.name == '_' {
					continue
				} else {
					ptr := b.assign_target_for_ident(ident.name)
					if ptr != 0 {
						b.mod.add_instr(.store, b.cur_block, 0, [elem_val, ptr])
					}
				}
			}
		}
		return
	}
	// For multi-assign with plain assignment (a, b = b, a), evaluate all RHS
	// values before any stores to avoid aliasing issues.
	if stmt.lhs.len > 1 && stmt.rhs.len > 1 && stmt.op == .assign {
		mut rhs_vals := []ValueID{cap: stmt.rhs.len}
		for rhs_expr in stmt.rhs {
			rhs_vals << b.build_expr(rhs_expr)
		}
		for i, lhs in stmt.lhs {
			if i >= rhs_vals.len {
				break
			}
			rhs_val := rhs_vals[i]
			if ident := b.unwrap_ident(lhs) {
				if ident.name == '_' {
					continue
				}
				ptr := b.assign_target_for_ident(ident.name)
				if ptr != 0 {
					b.mod.add_instr(.store, b.cur_block, 0, [rhs_val, ptr])
				}
			} else if lhs is ast.SelectorExpr {
				base := b.build_addr(lhs)
				if base != 0 {
					b.mod.add_instr(.store, b.cur_block, 0, [rhs_val, base])
				}
			} else if lhs is ast.IndexExpr {
				base := b.build_addr(lhs)
				if base != 0 {
					b.mod.add_instr(.store, b.cur_block, 0, [rhs_val, base])
				}
			}
		}
		return
	}
	for i, lhs in stmt.lhs {
		if i >= stmt.rhs.len {
			break
		}
		rhs := stmt.rhs[i]
		rhs_val := b.build_expr(rhs)

		if ident := b.unwrap_ident(lhs) {
			if stmt.op == .decl_assign {
				// New variable declaration - use the actual type of the built RHS value
				rhs_type := b.mod.values[rhs_val].typ
				alloca := b.mod.add_instr(.alloca, b.cur_block, b.mod.type_store.get_ptr(rhs_type),
					[]ValueID{})
				b.mod.add_instr(.store, b.cur_block, 0, [rhs_val, alloca])
				b.vars[ident.name] = alloca
				b.track_array_elem_type_for_local_expr(ident.name, lhs, rhs, rhs_val)
			} else if ident.name == '_' && stmt.op == .assign {
				// Discard for plain assignment only; compound assignments (+=, etc.)
				// must still execute (e.g. for loop counter `_ += 1`).
				continue
			} else {
				// Assignment to existing variable (local or global)
				// Skip stores to const_array_globals — they are already initialized
				// in the data segment. The runtime const init function would overwrite
				// the first element with a stack pointer.
				// Only skip if RHS is an ArrayInitExpr (fixed array runtime init).
				// Do NOT skip if RHS is a function call (e.g. new_array_from_c_array
				// for dynamic arrays — those need their _vinit assignment).
				if rhs is ast.ArrayInitExpr {
					module_const_name := ssa_module_storage_name(b.cur_module, ident.name)
					if ident.name in b.const_array_globals
						|| module_const_name in b.const_array_globals
						|| 'builtin__${ident.name}' in b.const_array_globals {
						continue
					}
				}
				ptr := b.assign_target_for_ident(ident.name)
				if ptr != 0 {
					if stmt.op == .assign {
						b.mod.add_instr(.store, b.cur_block, 0, [rhs_val, ptr])
					} else {
						// Compound assignment (+=, -=, etc.)
						ptr_typ := b.mod.values[ptr].typ
						elem_typ := b.mod.type_store.types[ptr_typ].elem_type
						loaded := b.mod.add_instr(.load, b.cur_block, elem_typ, [ptr])
						ca_is_float := elem_typ > 0 && int(elem_typ) < b.mod.type_store.types.len
							&& b.mod.type_store.types[elem_typ].kind == .float_t
						op := if ca_is_float {
							match stmt.op {
								.plus_assign { OpCode.fadd }
								.minus_assign { OpCode.fsub }
								.mul_assign { OpCode.fmul }
								.div_assign { OpCode.fdiv }
								.mod_assign { OpCode.frem }
								else { OpCode.fadd }
							}
						} else {
							match stmt.op {
								.plus_assign { OpCode.add }
								.minus_assign { OpCode.sub }
								.mul_assign { OpCode.mul }
								.div_assign { OpCode.sdiv }
								.mod_assign { OpCode.srem }
								.left_shift_assign { OpCode.shl }
								.right_shift_assign { OpCode.ashr }
								.and_assign { OpCode.and_ }
								.or_assign { OpCode.or_ }
								.xor_assign { OpCode.xor }
								else { OpCode.add }
							}
						}
						// If LHS is float but RHS is int, convert RHS to float
						mut actual_rhs := rhs_val
						if ca_is_float {
							rhs_typ := b.mod.values[rhs_val].typ
							rhs_is_float := rhs_typ > 0 && int(rhs_typ) < b.mod.type_store.types.len
								&& b.mod.type_store.types[rhs_typ].kind == .float_t
							if !rhs_is_float {
								rhs_unsigned := rhs_typ > 0
									&& int(rhs_typ) < b.mod.type_store.types.len
									&& b.mod.type_store.types[rhs_typ].is_unsigned
								conv_op := if rhs_unsigned {
									OpCode.uitofp
								} else {
									OpCode.sitofp
								}
								actual_rhs = b.mod.add_instr(conv_op, b.cur_block, elem_typ, [
									rhs_val,
								])
							}
						}
						result := b.mod.add_instr(op, b.cur_block, b.mod.values[loaded].typ, [
							loaded,
							actual_rhs,
						])
						b.mod.add_instr(.store, b.cur_block, 0, [result, ptr])
					}
				}
			}
		} else if lhs is ast.SelectorExpr {
			// Field assignment: obj.field = val or obj.field += val
			base := b.build_addr(lhs)
			if base != 0 {
				if stmt.op == .assign {
					b.mod.add_instr(.store, b.cur_block, 0, [rhs_val, base])
				} else {
					// Compound assignment (+=, -=, etc.)
					ptr_typ := b.mod.values[base].typ
					elem_typ := if ptr_typ < b.mod.type_store.types.len {
						b.mod.type_store.types[ptr_typ].elem_type
					} else {
						b.mod.values[rhs_val].typ
					}
					loaded := b.mod.add_instr(.load, b.cur_block, elem_typ, [base])
					sf_is_float := elem_typ > 0 && int(elem_typ) < b.mod.type_store.types.len
						&& b.mod.type_store.types[elem_typ].kind == .float_t
					op := if sf_is_float {
						match stmt.op {
							.plus_assign { OpCode.fadd }
							.minus_assign { OpCode.fsub }
							.mul_assign { OpCode.fmul }
							.div_assign { OpCode.fdiv }
							.mod_assign { OpCode.frem }
							else { OpCode.fadd }
						}
					} else {
						match stmt.op {
							.plus_assign { OpCode.add }
							.minus_assign { OpCode.sub }
							.mul_assign { OpCode.mul }
							.div_assign { OpCode.sdiv }
							.mod_assign { OpCode.srem }
							.left_shift_assign { OpCode.shl }
							.right_shift_assign { OpCode.ashr }
							.and_assign { OpCode.and_ }
							.or_assign { OpCode.or_ }
							.xor_assign { OpCode.xor }
							else { OpCode.add }
						}
					}
					// If LHS is float but RHS is int, convert RHS to float
					mut actual_rhs := rhs_val
					if sf_is_float {
						rhs_typ := b.mod.values[rhs_val].typ
						rhs_is_float := rhs_typ > 0 && int(rhs_typ) < b.mod.type_store.types.len
							&& b.mod.type_store.types[rhs_typ].kind == .float_t
						if !rhs_is_float {
							rhs_unsigned := rhs_typ > 0 && int(rhs_typ) < b.mod.type_store.types.len
								&& b.mod.type_store.types[rhs_typ].is_unsigned
							conv_op := if rhs_unsigned {
								OpCode.uitofp
							} else {
								OpCode.sitofp
							}
							actual_rhs = b.mod.add_instr(conv_op, b.cur_block, elem_typ, [
								rhs_val,
							])
						}
					}
					result := b.mod.add_instr(op, b.cur_block, b.mod.values[loaded].typ, [
						loaded,
						actual_rhs,
					])
					b.mod.add_instr(.store, b.cur_block, 0, [result, base])
				}
			}
		} else if lhs is ast.PrefixExpr && lhs.op == .mul {
			// Pointer dereference assignment: *ptr = val
			// Build the inner expression to get the pointer, then store to it
			ptr := b.build_expr(lhs.expr)
			if stmt.op == .assign {
				b.mod.add_instr(.store, b.cur_block, 0, [rhs_val, ptr])
			} else {
				// Compound assignment (*ptr += val)
				ptr_typ := b.mod.values[ptr].typ
				elem_typ := if int(ptr_typ) < b.mod.type_store.types.len {
					b.mod.type_store.types[ptr_typ].elem_type
				} else {
					b.mod.values[rhs_val].typ
				}
				loaded := b.mod.add_instr(.load, b.cur_block, elem_typ, [ptr])
				pd_is_float := elem_typ > 0 && int(elem_typ) < b.mod.type_store.types.len
					&& b.mod.type_store.types[elem_typ].kind == .float_t
				op := if pd_is_float {
					match stmt.op {
						.plus_assign { OpCode.fadd }
						.minus_assign { OpCode.fsub }
						.mul_assign { OpCode.fmul }
						.div_assign { OpCode.fdiv }
						else { OpCode.fadd }
					}
				} else {
					match stmt.op {
						.plus_assign { OpCode.add }
						.minus_assign { OpCode.sub }
						.mul_assign { OpCode.mul }
						.div_assign { OpCode.sdiv }
						else { OpCode.add }
					}
				}
				// If LHS is float but RHS is int, convert RHS to float
				mut actual_rhs := rhs_val
				if pd_is_float {
					rhs_typ := b.mod.values[rhs_val].typ
					rhs_is_float := rhs_typ > 0 && int(rhs_typ) < b.mod.type_store.types.len
						&& b.mod.type_store.types[rhs_typ].kind == .float_t
					if !rhs_is_float {
						rhs_unsigned := rhs_typ > 0 && int(rhs_typ) < b.mod.type_store.types.len
							&& b.mod.type_store.types[rhs_typ].is_unsigned
						conv_op := if rhs_unsigned {
							OpCode.uitofp
						} else {
							OpCode.sitofp
						}
						actual_rhs = b.mod.add_instr(conv_op, b.cur_block, elem_typ, [
							rhs_val,
						])
					}
				}
				result := b.mod.add_instr(op, b.cur_block, b.mod.values[loaded].typ, [
					loaded,
					actual_rhs,
				])
				b.mod.add_instr(.store, b.cur_block, 0, [result, ptr])
			}
		} else if lhs is ast.IndexExpr {
			// Index assignment: arr[i] = val or arr[i] += val
			base := b.build_addr(lhs)
			if base != 0 {
				if stmt.op == .assign {
					b.mod.add_instr(.store, b.cur_block, 0, [rhs_val, base])
				} else {
					// Compound assignment (+=, -=, etc.)
					ptr_typ := b.mod.values[base].typ
					elem_typ := if ptr_typ < b.mod.type_store.types.len {
						b.mod.type_store.types[ptr_typ].elem_type
					} else {
						b.mod.values[rhs_val].typ
					}
					loaded := b.mod.add_instr(.load, b.cur_block, elem_typ, [base])
					ix_is_float := elem_typ > 0 && int(elem_typ) < b.mod.type_store.types.len
						&& b.mod.type_store.types[elem_typ].kind == .float_t
					op := if ix_is_float {
						match stmt.op {
							.plus_assign { OpCode.fadd }
							.minus_assign { OpCode.fsub }
							.mul_assign { OpCode.fmul }
							.div_assign { OpCode.fdiv }
							.mod_assign { OpCode.frem }
							else { OpCode.fadd }
						}
					} else {
						match stmt.op {
							.plus_assign { OpCode.add }
							.minus_assign { OpCode.sub }
							.mul_assign { OpCode.mul }
							.div_assign { OpCode.sdiv }
							.mod_assign { OpCode.srem }
							.left_shift_assign { OpCode.shl }
							.right_shift_assign { OpCode.ashr }
							.and_assign { OpCode.and_ }
							.or_assign { OpCode.or_ }
							.xor_assign { OpCode.xor }
							else { OpCode.add }
						}
					}
					// If LHS is float but RHS is int, convert RHS to float
					mut actual_rhs := rhs_val
					if ix_is_float {
						rhs_typ := b.mod.values[rhs_val].typ
						rhs_is_float := rhs_typ > 0 && int(rhs_typ) < b.mod.type_store.types.len
							&& b.mod.type_store.types[rhs_typ].kind == .float_t
						if !rhs_is_float {
							rhs_unsigned := rhs_typ > 0 && int(rhs_typ) < b.mod.type_store.types.len
								&& b.mod.type_store.types[rhs_typ].is_unsigned
							conv_op := if rhs_unsigned {
								OpCode.uitofp
							} else {
								OpCode.sitofp
							}
							actual_rhs = b.mod.add_instr(conv_op, b.cur_block, elem_typ, [
								rhs_val,
							])
						}
					}
					result := b.mod.add_instr(op, b.cur_block, b.mod.values[loaded].typ, [
						loaded,
						actual_rhs,
					])
					b.mod.add_instr(.store, b.cur_block, 0, [result, base])
				}
			}
		}
	}
}

fn (mut b Builder) track_array_elem_type_for_local_value(name string, val ValueID) bool {
	if elem_t := b.array_value_elem_types[val] {
		b.array_elem_types[name] = elem_t
		return true
	}
	return false
}

fn (mut b Builder) track_array_value_elem_type_from_checked_expr(val ValueID, expr ast.Expr) bool {
	if !b.valid_value_id(val) {
		return false
	}
	elem_from_expr := b.array_elem_type_from_expr(expr)
	if elem_from_expr != 0 {
		b.array_value_elem_types[val] = elem_from_expr
		return true
	}
	if typ := b.get_checked_expr_type(expr) {
		elem_t := b.unwrap_to_array_elem_ssa(typ)
		if elem_t != 0 {
			b.array_value_elem_types[val] = elem_t
			return true
		}
	}
	return false
}

fn (mut b Builder) array_elem_type_from_expr(expr ast.Expr) TypeID {
	match expr {
		ast.ParenExpr {
			return b.array_elem_type_from_expr(expr.expr)
		}
		ast.ModifierExpr {
			return b.array_elem_type_from_expr(expr.expr)
		}
		ast.UnsafeExpr {
			if expr.stmts.len == 0 {
				return 0
			}
			last := expr.stmts[expr.stmts.len - 1]
			if last is ast.ExprStmt {
				return b.array_elem_type_from_expr(last.expr)
			}
		}
		ast.PrefixExpr {
			if expr.op == .mul && expr.expr is ast.CastExpr {
				return b.array_elem_type_from_ast_type((expr.expr as ast.CastExpr).typ)
			}
		}
		ast.CastExpr {
			return b.array_elem_type_from_ast_type(expr.typ)
		}
		else {}
	}

	return 0
}

fn (mut b Builder) track_array_value_elem_type_from_checked_cursor(val ValueID, c ast.Cursor) bool {
	if !b.valid_value_id(val) {
		return false
	}
	elem_from_expr := b.array_elem_type_from_expr_cursor(c)
	if elem_from_expr != 0 {
		b.array_value_elem_types[val] = elem_from_expr
		return true
	}
	if typ := b.get_checked_expr_type_from_flat(c) {
		elem_t := b.unwrap_to_array_elem_ssa(typ)
		if elem_t != 0 {
			b.array_value_elem_types[val] = elem_t
			return true
		}
	}
	return false
}

fn (mut b Builder) array_elem_type_from_type_cursor(c ast.Cursor) TypeID {
	match c.kind() {
		.typ_array {
			elem_t := b.ast_type_to_ssa_from_flat(c.edge(0))
			if elem_t != 0 {
				return elem_t
			}
		}
		.typ_array_fixed {
			elem_t := b.ast_type_to_ssa_from_flat(c.edge(1))
			if elem_t != 0 {
				return elem_t
			}
		}
		.expr_prefix, .expr_modifier, .expr_paren {
			elem_t := b.array_elem_type_from_type_cursor(c.edge(0))
			if elem_t != 0 {
				return elem_t
			}
		}
		.expr_ident {
			name := c.name()
			if name.starts_with('Array_') {
				elem_name := name['Array_'.len..]
				elem_t := b.ident_type_to_ssa(elem_name)
				if elem_t != 0 {
					return elem_t
				}
			}
		}
		else {}
	}

	if typ := b.get_checked_expr_type_from_flat(c) {
		elem_t := b.unwrap_to_array_elem_ssa(typ)
		if elem_t != 0 {
			return elem_t
		}
	}
	// s233: cursor-native fallback — no decode.
	return b.array_elem_type_from_ast_type_from_flat(c)
}

fn (mut b Builder) array_elem_type_from_expr_cursor(c ast.Cursor) TypeID {
	match c.kind() {
		.expr_paren, .expr_modifier {
			return b.array_elem_type_from_expr_cursor(c.edge(0))
		}
		.expr_unsafe {
			n := c.edge_count()
			if n == 0 {
				return 0
			}
			return b.array_elem_type_from_expr_cursor(c.edge(n - 1))
		}
		.stmt_expr {
			if c.edge_count() == 0 {
				return 0
			}
			return b.array_elem_type_from_expr_cursor(c.edge(0))
		}
		.expr_prefix {
			op := unsafe { token.Token(int(c.aux())) }
			inner := c.edge(0)
			if op == .mul && inner.kind() == .expr_cast {
				return b.array_elem_type_from_type_cursor(inner.edge(0))
			}
		}
		.expr_cast {
			return b.array_elem_type_from_type_cursor(c.edge(0))
		}
		else {}
	}

	return 0
}

fn (mut b Builder) checked_local_type(name string) ?types.Type {
	if name == '' || b.env == unsafe { nil } || b.cur_func < 0 || b.cur_func >= b.mod.funcs.len {
		return none
	}
	if b.cur_fn_scope_key != '' {
		if scope := b.env.get_fn_scope_by_key(b.cur_fn_scope_key) {
			if typ := scope.lookup_var_type(name) {
				return typ
			}
		}
	}
	fn_name := b.mod.funcs[b.cur_func].name
	if scope := b.env.get_fn_scope_by_key(fn_name) {
		if typ := scope.lookup_var_type(name) {
			return typ
		}
	}
	if b.cur_module != '' {
		if scope := b.env.get_fn_scope(b.cur_module, fn_name) {
			if typ := scope.lookup_var_type(name) {
				return typ
			}
		}
		prefix := '${b.cur_module}__'
		if fn_name.starts_with(prefix) {
			short_name := fn_name[prefix.len..]
			if scope := b.env.get_fn_scope(b.cur_module, short_name) {
				if typ := scope.lookup_var_type(name) {
					return typ
				}
			}
		}
	}
	return none
}

fn (b &Builder) smartcast_expr_name(expr ast.Expr) string {
	match expr {
		ast.Ident {
			return expr.name
		}
		ast.SelectorExpr {
			lhs_name := b.smartcast_expr_name(expr.lhs)
			if lhs_name == '' {
				return expr.rhs.name
			}
			return '${lhs_name}.${expr.rhs.name}'
		}
		ast.ParenExpr {
			return b.smartcast_expr_name(expr.expr)
		}
		ast.AsCastExpr {
			return b.smartcast_expr_name(expr.expr)
		}
		ast.ModifierExpr {
			return b.smartcast_expr_name(expr.expr)
		}
		else {
			name := expr.name()
			if name != 'Expr' {
				return name
			}
			return ''
		}
	}
}

fn (b &Builder) smartcast_expr_name_from_flat(c ast.Cursor) string {
	match c.kind() {
		.expr_ident {
			return c.name()
		}
		.expr_selector {
			lhs_name := b.smartcast_expr_name_from_flat(c.edge(0))
			rhs_name := c.edge(1).name()
			if lhs_name == '' {
				return rhs_name
			}
			return '${lhs_name}.${rhs_name}'
		}
		.expr_paren, .expr_modifier, .expr_as_cast {
			return b.smartcast_expr_name_from_flat(c.edge(0))
		}
		else {
			return ''
		}
	}
}

fn (mut b Builder) smartcast_variant_type_from_sumtype_tag(sumtype_expr ast.Expr, tag int) TypeID {
	if tag < 0 {
		return 0
	}
	mut sumtype_type := types.Type(types.void_)
	if checked := b.get_checked_expr_type(sumtype_expr) {
		sumtype_type = checked
	} else {
		expr_name := b.smartcast_expr_name(sumtype_expr)
		if local_type := b.checked_local_type(expr_name) {
			sumtype_type = local_type
		} else {
			return 0
		}
	}
	base := b.unwrap_alias_type(sumtype_type)
	if base !is types.SumType {
		return 0
	}
	sumtype_info := base as types.SumType
	if tag >= sumtype_info.variants.len {
		return 0
	}
	return b.type_to_ssa(sumtype_info.variants[tag])
}

fn (mut b Builder) smartcast_variant_type_from_tag_check(expr ast.InfixExpr) TypeID {
	if expr.op != .eq || expr.lhs !is ast.SelectorExpr || expr.rhs !is ast.BasicLiteral {
		return 0
	}
	tag_sel := expr.lhs as ast.SelectorExpr
	if tag_sel.rhs.name != '_tag' {
		return 0
	}
	tag_lit := expr.rhs as ast.BasicLiteral
	if tag_lit.kind != .number {
		return 0
	}
	return b.smartcast_variant_type_from_sumtype_tag(tag_sel.lhs, tag_lit.value.int())
}

fn (mut b Builder) smartcast_rhs_variant_type_id(rhs ast.Expr, allow_value_style bool) TypeID {
	if !allow_value_style {
		match rhs {
			ast.Ident {
				if rhs.name.len == 0 || !u8(rhs.name[0]).is_capital() {
					return 0
				}
			}
			ast.SelectorExpr, ast.Type, ast.PrefixExpr {}
			else {
				return 0
			}
		}
	}
	return b.ast_type_to_ssa(rhs)
}

fn (mut b Builder) smartcast_variant_type_ids_from_condition(cond ast.Expr) map[string]TypeID {
	mut smartcasts := map[string]TypeID{}
	match cond {
		ast.ParenExpr {
			return b.smartcast_variant_type_ids_from_condition(cond.expr)
		}
		ast.InfixExpr {
			if cond.op == .and {
				left := b.smartcast_variant_type_ids_from_condition(cond.lhs)
				right := b.smartcast_variant_type_ids_from_condition(cond.rhs)
				for name, typ in left {
					if typ != 0 {
						smartcasts[name] = typ
					}
				}
				for name, typ in right {
					if typ != 0 {
						smartcasts[name] = typ
					}
				}
				return smartcasts
			}
			tag_typ := b.smartcast_variant_type_from_tag_check(cond)
			if tag_typ != 0 {
				tag_sel := cond.lhs as ast.SelectorExpr
				expr_name := b.smartcast_expr_name(tag_sel.lhs)
				if expr_name != '' {
					smartcasts[expr_name] = tag_typ
				}
				return smartcasts
			}
			if cond.op == .eq {
				if cond.rhs is ast.BasicLiteral && cond.rhs.kind == .number {
					tag_typ2 := b.smartcast_variant_type_from_sumtype_tag(cond.lhs,
						int(parse_const_int_literal(cond.rhs.value)))
					expr_name := b.smartcast_expr_name(cond.lhs)
					if tag_typ2 != 0 && expr_name != '' {
						smartcasts[expr_name] = tag_typ2
						return smartcasts
					}
				}
				if cond.lhs is ast.BasicLiteral && cond.lhs.kind == .number {
					tag_typ2 := b.smartcast_variant_type_from_sumtype_tag(cond.rhs,
						int(parse_const_int_literal(cond.lhs.value)))
					expr_name := b.smartcast_expr_name(cond.rhs)
					if tag_typ2 != 0 && expr_name != '' {
						smartcasts[expr_name] = tag_typ2
						return smartcasts
					}
				}
			}
			if cond.op in [.key_is, .eq] {
				expr_name := b.smartcast_expr_name(cond.lhs)
				if expr_name != '' {
					variant_type := b.smartcast_rhs_variant_type_id(cond.rhs, cond.op == .key_is)
					if variant_type != 0 {
						smartcasts[expr_name] = variant_type
					}
				}
			}
		}
		else {}
	}

	return smartcasts
}

// --- s230: cursor-native smartcast condition analysis ---
// These mirror the AST smartcast_* helpers so build_if_stmt_from_flat can derive
// then-branch smartcasts straight from the condition cursor, dropping the
// full-cond decode_expr. All type lookups go through pos.id
// (get_checked_expr_type_from_flat) or pure type logic. Expression names reuse
// the pre-existing smartcast_expr_name_from_flat (whose `else` arm returns '',
// already the codebase's behavior via selector_lhs_variant_type_id_from_flat —
// it only differs from the AST helper for exotic non-Ident/Selector condition
// lhs kinds that real smartcasts never use).

fn (mut b Builder) smartcast_variant_type_from_sumtype_tag_from_flat(sumtype_expr_c ast.Cursor, tag int) TypeID {
	if tag < 0 {
		return 0
	}
	mut sumtype_type := types.Type(types.void_)
	if checked := b.get_checked_expr_type_from_flat(sumtype_expr_c) {
		sumtype_type = checked
	} else {
		expr_name := b.smartcast_expr_name_from_flat(sumtype_expr_c)
		if local_type := b.checked_local_type(expr_name) {
			sumtype_type = local_type
		} else {
			return 0
		}
	}
	base := b.unwrap_alias_type(sumtype_type)
	if base !is types.SumType {
		return 0
	}
	sumtype_info := base as types.SumType
	if tag >= sumtype_info.variants.len {
		return 0
	}
	return b.type_to_ssa(sumtype_info.variants[tag])
}

fn (mut b Builder) smartcast_variant_type_from_tag_check_from_flat(infix_c ast.Cursor) TypeID {
	op := unsafe { token.Token(int(infix_c.aux())) }
	lhs_c := infix_c.edge(0)
	rhs_c := infix_c.edge(1)
	if op != .eq || lhs_c.kind() != .expr_selector || rhs_c.kind() != .expr_basic_literal {
		return 0
	}
	// tag_sel.rhs is the selector's rhs Ident (edge 1); tag_sel.lhs is edge 0.
	tag_sel_rhs := lhs_c.edge(1)
	tag_sel_rhs_name := if tag_sel_rhs.kind() == .expr_ident { tag_sel_rhs.name() } else { '' }
	if tag_sel_rhs_name != '_tag' {
		return 0
	}
	tag_lit_kind := unsafe { token.Token(int(rhs_c.aux())) }
	if tag_lit_kind != .number {
		return 0
	}
	return b.smartcast_variant_type_from_sumtype_tag_from_flat(lhs_c.edge(0), rhs_c.name().int())
}

fn (mut b Builder) smartcast_rhs_variant_type_id_from_flat(rhs_c ast.Cursor, allow_value_style bool) TypeID {
	if !allow_value_style {
		match rhs_c.kind() {
			.expr_ident {
				name := rhs_c.name()
				if name.len == 0 || !u8(name[0]).is_capital() {
					return 0
				}
			}
			.expr_selector, .expr_prefix {}
			else {
				// ast.Type variants are flat-encoded as the contiguous .typ_* block;
				// any other expr kind returns 0 like the AST `else`.
				if !(int(rhs_c.kind()) >= int(ast.FlatNodeKind.typ_anon_struct)
					&& int(rhs_c.kind()) <= int(ast.FlatNodeKind.typ_tuple)) {
					return 0
				}
			}
		}
	}
	return b.ast_type_to_ssa_from_flat(rhs_c)
}

fn (mut b Builder) smartcast_variant_type_ids_from_condition_from_flat(cond_c ast.Cursor) map[string]TypeID {
	mut smartcasts := map[string]TypeID{}
	match cond_c.kind() {
		.expr_paren {
			return b.smartcast_variant_type_ids_from_condition_from_flat(cond_c.edge(0))
		}
		.expr_infix {
			op := unsafe { token.Token(int(cond_c.aux())) }
			lhs_c := cond_c.edge(0)
			rhs_c := cond_c.edge(1)
			if op == .and {
				left := b.smartcast_variant_type_ids_from_condition_from_flat(lhs_c)
				right := b.smartcast_variant_type_ids_from_condition_from_flat(rhs_c)
				for name, typ in left {
					if typ != 0 {
						smartcasts[name] = typ
					}
				}
				for name, typ in right {
					if typ != 0 {
						smartcasts[name] = typ
					}
				}
				return smartcasts
			}
			tag_typ := b.smartcast_variant_type_from_tag_check_from_flat(cond_c)
			if tag_typ != 0 {
				// tag_sel = cond.lhs (a SelectorExpr); expr_name from tag_sel.lhs = lhs_c.edge(0).
				expr_name := b.smartcast_expr_name_from_flat(lhs_c.edge(0))
				if expr_name != '' {
					smartcasts[expr_name] = tag_typ
				}
				return smartcasts
			}
			if op in [.key_is, .eq] {
				expr_name := b.smartcast_expr_name_from_flat(lhs_c)
				if expr_name != '' {
					variant_type := b.smartcast_rhs_variant_type_id_from_flat(rhs_c, op == .key_is)
					if variant_type != 0 {
						smartcasts[expr_name] = variant_type
					}
				}
			}
		}
		else {}
	}

	return smartcasts
}

fn (mut b Builder) apply_local_smartcasts(smartcasts map[string]TypeID) {
	for name, typ in smartcasts {
		if name != '' && typ != 0 {
			b.local_smartcasts[name] = typ
		}
	}
}

fn (mut b Builder) selector_lhs_variant_type_id(expr ast.Expr) TypeID {
	expr_name := b.smartcast_expr_name(expr)
	if expr_name != '' {
		if variant_type := b.local_smartcasts[expr_name] {
			return variant_type
		}
	}
	if checked := b.get_checked_expr_type(expr) {
		base := b.unwrap_alias_type(checked)
		if base !is types.SumType {
			return b.type_to_ssa(checked)
		}
	}
	return 0
}

fn (mut b Builder) selector_lhs_variant_type_id_from_flat(c ast.Cursor) TypeID {
	expr_name := b.smartcast_expr_name_from_flat(c)
	if expr_name != '' {
		if variant_type := b.local_smartcasts[expr_name] {
			return variant_type
		}
	}
	if checked := b.get_checked_expr_type_from_flat(c) {
		base := b.unwrap_alias_type(checked)
		if base !is types.SumType {
			return b.type_to_ssa(checked)
		}
	}
	return 0
}

fn (mut b Builder) track_array_elem_type_for_local_checked_type(name string) bool {
	if typ := b.checked_local_type(name) {
		elem_t := b.unwrap_to_array_elem_ssa(typ)
		if elem_t != 0 {
			b.array_elem_types[name] = elem_t
			return true
		}
	}
	return false
}

fn (mut b Builder) track_array_elem_type_for_local_expr(name string, lhs ast.Expr, expr ast.Expr, val ValueID) {
	elem_from_expr := b.array_elem_type_from_expr(expr)
	if elem_from_expr != 0 {
		b.array_elem_types[name] = elem_from_expr
		return
	}
	if b.track_array_elem_type_for_local_value(name, val) {
		return
	}
	if lhs_typ := b.get_checked_expr_type(lhs) {
		elem_t := b.unwrap_to_array_elem_ssa(lhs_typ)
		if elem_t != 0 {
			b.array_elem_types[name] = elem_t
			return
		}
	}
	if b.track_array_elem_type_for_local_checked_type(name) {
		return
	}
	if expr is ast.ArrayInitExpr && builder_expr_ok(expr.typ) && expr.typ is ast.Type {
		expr_typ := expr.typ as ast.Type
		match expr_typ {
			ast.ArrayType {
				elem_t := b.ast_type_to_ssa(expr_typ.elem_type)
				if elem_t != 0 {
					b.array_elem_types[name] = elem_t
					return
				}
			}
			else {}
		}
	}
	if typ := b.get_checked_expr_type(expr) {
		elem_t := b.unwrap_to_array_elem_ssa(typ)
		if elem_t != 0 {
			b.array_elem_types[name] = elem_t
		}
	}
	if expr is ast.SelectorExpr {
		elem_t := b.array_elem_type_from_selector_expr(expr)
		if elem_t != 0 {
			b.array_elem_types[name] = elem_t
		}
	}
}

fn (b &Builder) unwrap_ident_name_from_flat(c ast.Cursor) ?string {
	match c.kind() {
		.expr_ident {
			return c.name()
		}
		.expr_modifier {
			return b.unwrap_ident_name_from_flat(c.edge(0))
		}
		else {
			return none
		}
	}
}

fn (mut b Builder) get_checked_expr_type_from_flat(c ast.Cursor) ?types.Type {
	if b.env != unsafe { nil } {
		pos := c.pos()
		if pos.id != 0 {
			if typ := b.env.get_expr_type(pos.id) {
				return typ
			}
		}
	}
	return none
}

fn (mut b Builder) expr_type_from_flat(c ast.Cursor) TypeID {
	if typ := b.get_checked_expr_type_from_flat(c) {
		return b.type_to_ssa(typ)
	}
	match c.kind() {
		.expr_basic_literal {
			kind := unsafe { token.Token(int(c.aux())) }
			if kind == .key_true || kind == .key_false {
				return b.mod.type_store.get_int(1)
			}
			value := c.name()
			if kind == .number
				&& (value.contains('.') || (!value.starts_with('0x') && !value.starts_with('0X')
				&& (value.contains('e') || value.contains('E')))) {
				return b.mod.type_store.get_float(64)
			}
			return b.mod.type_store.get_int(64)
		}
		.expr_string, .expr_string_inter {
			return b.get_string_type()
		}
		else {
			return b.mod.type_store.get_int(64)
		}
	}
}

fn (mut b Builder) const_int_from_flat(c ast.Cursor) int {
	match c.kind() {
		.expr_basic_literal {
			return int(parse_const_int_literal(c.name()))
		}
		.expr_ident {
			return b.resolve_const_int(c.name())
		}
		else {
			return 0
		}
	}
}

fn (mut b Builder) array_elem_type_from_ast_type(typ ast.Expr) TypeID {
	match typ {
		ast.Type {
			match typ {
				ast.ArrayType {
					return b.ast_type_to_ssa(typ.elem_type)
				}
				ast.ArrayFixedType {
					return b.ast_type_to_ssa(typ.elem_type)
				}
				else {}
			}
		}
		ast.Ident {
			if !ssa_string_ok(typ.name) {
				return 0
			}
			if typ.name.starts_with('Array_') {
				elem_name := typ.name['Array_'.len..]
				return b.ident_type_to_ssa(elem_name)
			}
		}
		ast.PrefixExpr {
			if typ.op == .ellipsis {
				return b.ast_type_to_ssa(typ.expr)
			}
			return b.array_elem_type_from_ast_type(typ.expr)
		}
		ast.ModifierExpr {
			return b.array_elem_type_from_ast_type(typ.expr)
		}
		else {}
	}

	return 0
}

// array_elem_type_from_ast_type_from_flat (s233) is the cursor-native mirror of
// array_elem_type_from_ast_type. ast.Type variants are flat-encoded as `.typ_*`
// (ArrayType: edge0=elem_type; ArrayFixedType: edge0=len, edge1=elem_type); the
// Ident / PrefixExpr (ellipsis vs recurse) / ModifierExpr arms map directly to
// the cursor kinds. Every other kind returns 0, like the AST `else`. This lets
// array_elem_type_from_type_cursor drop its last decode_expr fallback.
fn (mut b Builder) array_elem_type_from_ast_type_from_flat(c ast.Cursor) TypeID {
	kind := c.kind()
	if kind == .typ_array {
		return b.ast_type_to_ssa_from_flat(c.edge(0))
	}
	if kind == .typ_array_fixed {
		return b.ast_type_to_ssa_from_flat(c.edge(1))
	}
	match kind {
		.expr_ident {
			name := c.name()
			if !ssa_string_ok(name) {
				return 0
			}
			if name.starts_with('Array_') {
				elem_name := name['Array_'.len..]
				return b.ident_type_to_ssa(elem_name)
			}
		}
		.expr_prefix {
			op := unsafe { token.Token(int(c.aux())) }
			if op == .ellipsis {
				return b.ast_type_to_ssa_from_flat(c.edge(0))
			}
			return b.array_elem_type_from_ast_type_from_flat(c.edge(0))
		}
		.expr_modifier {
			return b.array_elem_type_from_ast_type_from_flat(c.edge(0))
		}
		else {}
	}

	return 0
}

fn (mut b Builder) ast_type_to_ssa_from_flat(c ast.Cursor) TypeID {
	match c.kind() {
		.expr_ident {
			if !ssa_string_ok(c.name()) {
				return b.mod.type_store.get_int(64)
			}
			return b.ident_type_to_ssa(c.name())
		}
		.expr_prefix {
			op := unsafe { token.Token(int(c.aux())) }
			if op == .amp {
				base := b.ast_type_to_ssa_from_flat(c.edge(0))
				return b.mod.type_store.get_ptr(base)
			}
			if op == .ellipsis {
				return b.get_array_type()
			}
			return b.mod.type_store.get_int(64)
		}
		.expr_modifier {
			return b.ast_type_to_ssa_from_flat(c.edge(0))
		}
		.expr_selector {
			lhs_c := c.edge(0)
			rhs_c := c.edge(1)
			if lhs_c.kind() == .expr_ident {
				mod_name := lhs_c.name()
				rhs_name := rhs_c.name()
				if !ssa_string_ok(mod_name) || !ssa_string_ok(rhs_name) {
					return b.mod.type_store.get_int(64)
				}
				full_name := '${mod_name}.${rhs_name}'
				qualified := '${b.cur_module}__${full_name}'
				if qualified in b.struct_types {
					return b.struct_types[qualified]
				}
				if full_name in b.struct_types {
					return b.struct_types[full_name]
				}
				mod_qualified := '${mod_name}__${rhs_name}'
				if mod_qualified in b.struct_types {
					return b.struct_types[mod_qualified]
				}
				if alias_type := b.selector_type_alias_to_ssa(mod_name, rhs_name) {
					return alias_type
				}
				if resolved_mod := b.selector_module_name_for_ident(mod_name) {
					if alias_type := b.selector_type_alias_to_ssa(resolved_mod, rhs_name) {
						return alias_type
					}
				}
				if mod_name == 'C' {
					cur_qualified := '${b.cur_module}__${rhs_name}'
					if cur_qualified in b.struct_types {
						return b.struct_types[cur_qualified]
					}
					for sname, sid in b.struct_types {
						if sname.ends_with('__${rhs_name}') {
							return sid
						}
					}
				}
				if b.env != unsafe { nil } {
					mod_name_v := mod_name.replace('.', '_')
					if scope := b.env.get_scope(mod_name_v) {
						if obj := scope.lookup_parent(rhs_name, 0) {
							return b.type_to_ssa(obj.typ())
						}
					}
				}
			}
			return b.ident_type_to_ssa(rhs_c.name())
		}
		.expr_empty, .typ_nil {
			return 0
		}
		.expr_tuple, .typ_tuple {
			mut elem_types := []TypeID{cap: c.edge_count()}
			for i in 0 .. c.edge_count() {
				elem_types << b.ast_type_to_ssa_from_flat(c.edge(i))
			}
			return b.mod.type_store.get_tuple(elem_types)
		}
		.typ_array {
			return b.get_array_type()
		}
		.typ_array_fixed {
			elem_type := b.ast_type_to_ssa_from_flat(c.edge(1))
			arr_len := b.const_int_from_flat(c.edge(0))
			if arr_len > 0 {
				return b.mod.type_store.get_array(elem_type, arr_len)
			}
			return b.mod.type_store.get_int(64)
		}
		.typ_map {
			return b.struct_types['map'] or { b.mod.type_store.get_int(64) }
		}
		.typ_fn {
			i8_t := b.mod.type_store.get_int(8)
			return b.mod.type_store.get_ptr(i8_t)
		}
		.typ_option {
			return b.get_option_wrapper_type(b.ast_type_to_ssa_from_flat(c.edge(0)))
		}
		.typ_result {
			return b.get_result_wrapper_type(b.ast_type_to_ssa_from_flat(c.edge(0)))
		}
		.typ_pointer {
			return b.mod.type_store.get_ptr(b.ast_type_to_ssa_from_flat(c.edge(0)))
		}
		.typ_generic {
			return b.generic_type_to_ssa_from_flat(c)
		}
		else {
			return b.mod.type_store.get_int(64)
		}
	}
}

fn (mut b Builder) array_init_elem_type_from_flat(c ast.Cursor) TypeID {
	if c.kind() != .expr_array_init || c.edge_count() == 0 {
		return 0
	}
	typ_c := c.edge(0)
	match typ_c.kind() {
		.typ_array {
			return b.ast_type_to_ssa_from_flat(typ_c.edge(0))
		}
		.typ_array_fixed {
			return b.ast_type_to_ssa_from_flat(typ_c.edge(1))
		}
		else {}
	}

	if checked_type := b.get_checked_expr_type_from_flat(c) {
		checked_base := b.unwrap_alias_type(checked_type)
		match checked_base {
			types.Array {
				return b.type_to_ssa(checked_base.elem_type)
			}
			types.ArrayFixed {
				return b.type_to_ssa(checked_base.elem_type)
			}
			else {}
		}
	}
	return 0
}

fn (mut b Builder) track_array_elem_type_for_local_from_flat(name string, lhs_c ast.Cursor, c ast.Cursor, val ValueID) {
	elem_from_expr := b.array_elem_type_from_expr_cursor(c)
	if elem_from_expr != 0 {
		b.array_elem_types[name] = elem_from_expr
		return
	}
	if b.track_array_elem_type_for_local_value(name, val) {
		return
	}
	if lhs_typ := b.get_checked_expr_type_from_flat(lhs_c) {
		elem_t := b.unwrap_to_array_elem_ssa(lhs_typ)
		if elem_t != 0 {
			b.array_elem_types[name] = elem_t
			return
		}
	}
	if b.track_array_elem_type_for_local_checked_type(name) {
		return
	}
	if c.kind() == .expr_array_init {
		elem_t := b.array_init_elem_type_from_flat(c)
		if elem_t != 0 {
			b.array_elem_types[name] = elem_t
			return
		}
	}
	if typ := b.get_checked_expr_type_from_flat(c) {
		elem_t := b.unwrap_to_array_elem_ssa(typ)
		if elem_t != 0 {
			b.array_elem_types[name] = elem_t
		}
	}
	if c.kind() == .expr_selector {
		elem_t := b.array_elem_type_from_selector_cursor(c)
		if elem_t != 0 {
			b.array_elem_types[name] = elem_t
		}
	}
}

fn (mut b Builder) array_elem_type_from_selector_cursor(c ast.Cursor) TypeID {
	if c.kind() != .expr_selector {
		return 0
	}
	lhs_c := c.edge(0)
	rhs_name := c.edge(1).name()
	if lhs_type := b.get_checked_expr_type_from_flat(lhs_c) {
		st := b.unwrap_to_struct(lhs_type)
		if st.name != '' {
			for fi in 0 .. st.fields.len {
				if st.fields[fi].name == rhs_name {
					return b.unwrap_to_array_elem_ssa(st.fields[fi].typ)
				}
			}
		}
	}
	return 0
}

fn (mut b Builder) selector_module_name_from_flat(c ast.Cursor) ?string {
	if c.kind() != .expr_selector {
		return none
	}
	lhs_c := c.edge(0)
	if lhs_c.kind() != .expr_ident {
		return none
	}
	mod_name := lhs_c.name()
	if resolved := b.selector_module_name_for_ident(mod_name) {
		return resolved
	}
	rhs_c := c.edge(1)
	if rhs_c.kind() == .expr_ident {
		qualified := '${mod_name}__${rhs_c.name()}'
		if qualified in b.fn_index {
			return mod_name
		}
	}
	return none
}

fn (mut b Builder) selector_type_name_to_ssa(mod_name string, type_name string) ?TypeID {
	if mod_name == '' || type_name == '' || !ssa_string_ok(mod_name) || !ssa_string_ok(type_name) {
		return none
	}
	normalized_mod := mod_name.replace('.', '_')
	full_name := '${mod_name}.${type_name}'
	if type_id := b.struct_types[full_name] {
		return type_id
	}
	mod_qualified := '${normalized_mod}__${type_name}'
	if type_id := b.struct_types[mod_qualified] {
		return type_id
	}
	if alias_type := b.selector_type_alias_to_ssa(mod_name, type_name) {
		return alias_type
	}
	if resolved_mod := b.selector_module_name_for_ident(mod_name) {
		if alias_type := b.selector_type_alias_to_ssa(resolved_mod, type_name) {
			return alias_type
		}
	}
	if mod_name == 'C' {
		cur_qualified := '${b.cur_module}__${type_name}'
		if type_id := b.struct_types[cur_qualified] {
			return type_id
		}
		for sname, sid in b.struct_types {
			if sname.ends_with('__${type_name}') {
				return sid
			}
		}
	}
	if b.is_enum_type(full_name) || b.is_enum_type(mod_qualified) {
		return b.mod.type_store.get_int(32)
	}
	if b.env != unsafe { nil } {
		if scope := b.env.get_scope(normalized_mod) {
			if obj := scope.lookup_parent(type_name, 0) {
				type_id := b.type_to_ssa(obj.typ())
				if b.valid_type_id(type_id) {
					return type_id
				}
			}
		}
	}
	return none
}

fn (mut b Builder) call_or_cast_selector_should_remain_cast(sel ast.SelectorExpr, fn_name string) bool {
	if sel.rhs.name == '' {
		return false
	}
	if fn_name in b.fn_index {
		if _ := b.selector_module_name(sel) {
			return false
		}
	}
	if target_type := b.call_lhs_type_to_ssa(sel) {
		if b.valid_type_id(target_type) {
			return true
		}
	}
	if sel.lhs is ast.Ident {
		if target_type := b.selector_type_name_to_ssa(sel.lhs.name, sel.rhs.name) {
			if b.valid_type_id(target_type) {
				return true
			}
		}
	}
	return looks_like_type_name(sel.rhs.name) && fn_name !in b.fn_index
}

fn (mut b Builder) call_or_cast_selector_should_remain_cast_from_flat(c ast.Cursor, fn_name string) bool {
	if c.kind() != .expr_selector {
		return false
	}
	if target_type := b.call_lhs_type_to_ssa_from_flat(c) {
		if b.valid_type_id(target_type) {
			return true
		}
	}
	lhs_c := c.edge(0)
	rhs_c := c.edge(1)
	if rhs_c.kind() != .expr_ident {
		return false
	}
	rhs_name := rhs_c.name()
	if lhs_c.kind() == .expr_ident {
		if target_type := b.selector_type_name_to_ssa(lhs_c.name(), rhs_name) {
			if b.valid_type_id(target_type) {
				return true
			}
		}
	}
	return looks_like_type_name(rhs_name) && fn_name !in b.fn_index
}

// static_receiver_type_name_from_flat (s219) cursor mirror of
// `static_receiver_type_name`. Cursor argument points at the lhs of a
// SelectorExpr (`sel.lhs` in legacy). Returns the receiver type name used
// for static-method dispatch (`Type.static_fn()`), resolved through
// `known_type_name` (struct_types / enum / env types). Handles the two
// cursor kinds the legacy match arm hits: `.expr_ident` (`Type`) and
// `.expr_selector` (`mod.Type`). All other kinds return `none`.
fn (mut b Builder) static_receiver_type_name_from_flat(c ast.Cursor) ?string {
	match c.kind() {
		.expr_ident {
			name := c.name()
			if !looks_like_type_name(name) {
				return none
			}
			if b.cur_module != '' && b.cur_module != 'main' {
				qualified := '${b.cur_module}__${name}'
				if b.known_type_name(qualified) {
					return qualified
				}
			}
			if b.known_type_name(name) {
				return name
			}
			if b.cur_module != '' && b.cur_module != 'main' {
				return '${b.cur_module}__${name}'
			}
			return name
		}
		.expr_selector {
			inner_lhs := c.edge(0)
			if inner_lhs.kind() != .expr_ident {
				return none
			}
			mod_name := inner_lhs.name().replace('.', '_')
			rhs := c.edge(1)
			if rhs.kind() != .expr_ident {
				return none
			}
			rhs_name := rhs.name()
			if !looks_like_type_name(rhs_name) {
				return none
			}
			qualified := '${mod_name}__${rhs_name}'
			if b.known_type_name(qualified) {
				return qualified
			}
			return qualified
		}
		else {
			return none
		}
	}
}

// resolve_static_method_name_from_flat (s219) cursor mirror of
// `resolve_static_method_name`. `c` is the `.expr_selector` cursor itself
// (`sel` in legacy). Resolves `Type.method()` to an fn_index key by trying
// `Type__method`, `builtin__Type__method`, then short-receiver fallbacks
// when the receiver name contains `__`. Receiver resolution delegates to
// `static_receiver_type_name_from_flat(c.edge(0))`.
fn (mut b Builder) resolve_static_method_name_from_flat(c ast.Cursor) ?string {
	if c.kind() != .expr_selector {
		return none
	}
	receiver_type := b.static_receiver_type_name_from_flat(c.edge(0)) or { return none }
	rhs := c.edge(1)
	if rhs.kind() != .expr_ident {
		return none
	}
	rhs_name := rhs.name()
	method_name := '${receiver_type}__${rhs_name}'
	if method_name in b.fn_index {
		return method_name
	}
	builtin_method := 'builtin__${method_name}'
	if builtin_method in b.fn_index {
		return builtin_method
	}
	if receiver_type.contains('__') {
		short_receiver := receiver_type.all_after_last('__')
		short_method := '${short_receiver}__${rhs_name}'
		if short_method in b.fn_index {
			return short_method
		}
		builtin_short_method := 'builtin__${short_method}'
		if builtin_short_method in b.fn_index {
			return builtin_short_method
		}
	}
	return none
}

fn (mut b Builder) field_index_from_flat(c ast.Cursor, base ValueID) int {
	rhs_name := c.edge(1).name()
	base_type_id := b.mod.values[base].typ
	if base_type_id > 0 && int(base_type_id) < b.mod.type_store.types.len {
		mut ssa_typ := b.mod.type_store.types[base_type_id]
		for ssa_typ.kind == .ptr_t && ssa_typ.elem_type > 0
			&& int(ssa_typ.elem_type) < b.mod.type_store.types.len {
			ssa_typ = b.mod.type_store.types[ssa_typ.elem_type]
		}
		if ssa_typ.kind == .struct_t {
			for i, name in ssa_typ.field_names {
				if name == rhs_name {
					return i
				}
			}
		}
	}
	if typ := b.get_checked_expr_type_from_flat(c.edge(0)) {
		st := b.unwrap_to_struct(typ)
		if st.name != '' {
			for i in 0 .. st.fields.len {
				if st.fields[i].name == rhs_name {
					return i
				}
			}
		}
	}
	if rhs_name.starts_with('arg') {
		idx_str := rhs_name[3..]
		if idx_str.len > 0 && idx_str[0] >= `0` && idx_str[0] <= `9` {
			return int(parse_const_int_literal(idx_str))
		}
	}
	return 0
}

fn (mut b Builder) infer_dynamic_array_index_type_from_flat(c ast.Cursor, base_val ValueID, current_type TypeID) TypeID {
	i64_t := b.mod.type_store.get_int(64)
	mut result_type := current_type
	if result_type == i64_t {
		if idx_typ := b.get_checked_expr_type_from_flat(c) {
			inferred := b.type_to_ssa(idx_typ)
			if inferred != 0 && inferred != i64_t {
				result_type = inferred
			}
		}
	}
	lhs_c_raw := c.edge(0)
	lhs_c := b.unwrap_index_lhs_cursor(lhs_c_raw)
	if result_type == i64_t {
		if arr_typ := b.get_checked_expr_type_from_flat(lhs_c_raw) {
			inferred := b.unwrap_to_array_elem_ssa(arr_typ)
			if inferred != 0 {
				result_type = inferred
			}
		}
	}
	if result_type == i64_t && lhs_c.id != lhs_c_raw.id {
		if arr_typ := b.get_checked_expr_type_from_flat(lhs_c) {
			inferred := b.unwrap_to_array_elem_ssa(arr_typ)
			if inferred != 0 {
				result_type = inferred
			}
		}
	}
	if result_type == i64_t && lhs_c.kind() == .expr_selector {
		inferred := b.array_elem_type_from_selector_cursor(lhs_c)
		if inferred != 0 {
			result_type = inferred
		}
	}
	if result_type == i64_t && lhs_c.kind() == .expr_call && lhs_c.edge_count() >= 2 {
		call_lhs := lhs_c.edge(0)
		if call_lhs.kind() == .expr_ident {
			call_name := call_lhs.name()
			if call_name in ['array__slice', 'array__slice_ni'] {
				if arr_typ := b.get_checked_expr_type_from_flat(lhs_c.edge(1)) {
					inferred := b.unwrap_to_array_elem_ssa(arr_typ)
					if inferred != 0 {
						result_type = inferred
					}
				}
			}
		}
	}
	if result_type == i64_t && lhs_c.kind() == .expr_ident {
		if elem_t := b.array_elem_types[lhs_c.name()] {
			result_type = elem_t
		}
	}
	if result_type == i64_t && lhs_c.kind() == .expr_ident {
		if typ := b.checked_local_type(lhs_c.name()) {
			inferred := b.unwrap_to_array_elem_ssa(typ)
			if inferred != 0 {
				result_type = inferred
			}
		}
	}
	if result_type == i64_t {
		if elem_t := b.array_value_elem_types[base_val] {
			result_type = elem_t
		}
	}
	return result_type
}

fn (mut b Builder) finish_index_value_from_flat(c ast.Cursor, val ValueID) ValueID {
	b.track_array_value_elem_type_from_checked_cursor(val, c)
	return val
}

fn (mut b Builder) unwrap_index_lhs_cursor(c ast.Cursor) ast.Cursor {
	mut cur := c
	for _ in 0 .. 8 {
		match cur.kind() {
			.expr_paren, .expr_modifier {
				if cur.edge_count() == 0 {
					return cur
				}
				cur = cur.edge(0)
				continue
			}
			else {}
		}

		break
	}
	return cur
}

fn (mut b Builder) compound_assign_opcode(op token.Token, is_float bool, full_ops bool) OpCode {
	if is_float {
		return match op {
			.plus_assign {
				OpCode.fadd
			}
			.minus_assign {
				OpCode.fsub
			}
			.mul_assign {
				OpCode.fmul
			}
			.div_assign {
				OpCode.fdiv
			}
			.mod_assign {
				if full_ops {
					OpCode.frem
				} else {
					OpCode.fadd
				}
			}
			else {
				OpCode.fadd
			}
		}
	}
	return match op {
		.plus_assign {
			OpCode.add
		}
		.minus_assign {
			OpCode.sub
		}
		.mul_assign {
			OpCode.mul
		}
		.div_assign {
			OpCode.sdiv
		}
		.mod_assign {
			if full_ops {
				OpCode.srem
			} else {
				OpCode.add
			}
		}
		.left_shift_assign {
			if full_ops {
				OpCode.shl
			} else {
				OpCode.add
			}
		}
		.right_shift_assign {
			if full_ops {
				OpCode.ashr
			} else {
				OpCode.add
			}
		}
		.and_assign {
			if full_ops {
				OpCode.and_
			} else {
				OpCode.add
			}
		}
		.or_assign {
			if full_ops {
				OpCode.or_
			} else {
				OpCode.add
			}
		}
		.xor_assign {
			if full_ops {
				OpCode.xor
			} else {
				OpCode.add
			}
		}
		else {
			OpCode.add
		}
	}
}

fn (mut b Builder) store_assign_to_ptr(ptr ValueID, rhs_val ValueID, op token.Token, full_ops bool) {
	if op == .assign {
		b.mod.add_instr(.store, b.cur_block, 0, [rhs_val, ptr])
		return
	}
	ptr_typ := b.mod.values[ptr].typ
	mut elem_typ := b.mod.values[rhs_val].typ
	if ptr_typ > 0 && int(ptr_typ) < b.mod.type_store.types.len {
		ptr_info := b.mod.type_store.types[ptr_typ]
		if ptr_info.elem_type != 0 {
			elem_typ = ptr_info.elem_type
		}
	}
	loaded := b.mod.add_instr(.load, b.cur_block, elem_typ, [ptr])
	is_float := elem_typ > 0 && int(elem_typ) < b.mod.type_store.types.len
		&& b.mod.type_store.types[elem_typ].kind == .float_t
	op_code := b.compound_assign_opcode(op, is_float, full_ops)
	mut actual_rhs := rhs_val
	if is_float {
		rhs_typ := b.mod.values[rhs_val].typ
		rhs_is_float := rhs_typ > 0 && int(rhs_typ) < b.mod.type_store.types.len
			&& b.mod.type_store.types[rhs_typ].kind == .float_t
		if !rhs_is_float {
			rhs_unsigned := rhs_typ > 0 && int(rhs_typ) < b.mod.type_store.types.len
				&& b.mod.type_store.types[rhs_typ].is_unsigned
			conv_op := if rhs_unsigned {
				OpCode.uitofp
			} else {
				OpCode.sitofp
			}
			actual_rhs = b.mod.add_instr(conv_op, b.cur_block, elem_typ, [rhs_val])
		}
	}
	result := b.mod.add_instr(op_code, b.cur_block, b.mod.values[loaded].typ, [
		loaded,
		actual_rhs,
	])
	b.mod.add_instr(.store, b.cur_block, 0, [result, ptr])
}

fn (mut b Builder) build_return(stmt ast.ReturnStmt) {
	fn_ret_type := if b.cur_func >= 0 && b.cur_func < b.mod.funcs.len {
		b.mod.funcs[b.cur_func].typ
	} else {
		TypeID(0)
	}
	is_option_ret := b.is_option_wrapper_type(fn_ret_type)
	is_result_ret := b.is_result_wrapper_type(fn_ret_type)
	if stmt.exprs.len == 0 {
		if is_option_ret || is_result_ret {
			b.mod.add_instr(.ret, b.cur_block, 0, [
				b.build_wrapper_value(fn_ret_type, true, 0, false),
			])
		} else {
			b.mod.add_instr(.ret, b.cur_block, 0, []ValueID{})
		}
	} else if stmt.exprs.len == 1 {
		ret_expr := stmt.exprs[0]
		mut val := b.build_expr(ret_expr)
		if (is_option_ret || is_result_ret) && b.mod.values[val].typ != fn_ret_type {
			val = b.coerce_wrapper_value(ret_expr, val, fn_ret_type)
		} else if fn_ret_type > 0 && int(fn_ret_type) < b.mod.type_store.types.len
			&& b.mod.type_store.types[fn_ret_type].kind == .float_t {
			// If function returns float but value is int, convert (e.g., `return 1` in fn() f64)
			val_type := b.mod.values[val].typ
			if val_type > 0 && int(val_type) < b.mod.type_store.types.len
				&& b.mod.type_store.types[val_type].kind != .float_t {
				val = b.mod.add_instr(.sitofp, b.cur_block, fn_ret_type, [val])
			}
		}
		b.mod.add_instr(.ret, b.cur_block, 0, [val])
	} else {
		// Multiple return values -> build a tuple via insertvalue
		mut elem_types := []TypeID{cap: stmt.exprs.len}
		mut vals := []ValueID{cap: stmt.exprs.len}
		for expr in stmt.exprs {
			v := b.build_expr(expr)
			vals << v
			elem_types << b.mod.values[v].typ
		}
		tuple_type := b.mod.type_store.get_tuple(elem_types)
		// Build tuple by chaining insertvalue instructions
		mut tuple_val := b.mod.get_or_add_const(tuple_type, 'undef')
		for i, v in vals {
			idx := b.mod.get_or_add_const(b.mod.type_store.get_int(32), i.str())
			tuple_val = b.mod.add_instr(.insertvalue, b.cur_block, tuple_type, [
				tuple_val,
				v,
				idx,
			])
		}
		// If function returns Option/Result, wrap the tuple in the wrapper
		if (is_option_ret || is_result_ret) && tuple_type != fn_ret_type {
			tuple_val = b.build_wrapper_value(fn_ret_type, true, tuple_val, true)
		}
		b.mod.add_instr(.ret, b.cur_block, 0, [tuple_val])
	}
}

fn (mut b Builder) build_for(stmt ast.ForStmt) {
	has_init := stmt.init !is ast.EmptyStmt
	has_cond := stmt.cond !is ast.EmptyExpr
	has_post := stmt.post !is ast.EmptyStmt

	// Build init in current block
	if has_init {
		b.build_stmt(stmt.init)
	}

	// Create blocks
	cond_block := b.mod.add_block(b.cur_func, 'for_cond')
	body_block := b.mod.add_block(b.cur_func, 'for_body')
	post_block := if has_post {
		b.mod.add_block(b.cur_func, 'for_post')
	} else {
		cond_block
	}
	exit_block := b.mod.add_block(b.cur_func, 'for_exit')

	// Jump to condition
	b.mod.add_instr(.jmp, b.cur_block, 0, [b.mod.blocks[cond_block].val_id])
	b.add_edge(b.cur_block, cond_block)

	// Condition block
	b.cur_block = cond_block
	if has_cond {
		cond_val := b.build_expr(stmt.cond)
		b.mod.add_instr(.br, b.cur_block, 0,
			[cond_val, b.mod.blocks[body_block].val_id, b.mod.blocks[exit_block].val_id])
		b.add_edge(cond_block, body_block)
		b.add_edge(cond_block, exit_block)
	} else {
		// Infinite loop
		b.mod.add_instr(.jmp, b.cur_block, 0, [b.mod.blocks[body_block].val_id])
		b.add_edge(cond_block, body_block)
	}

	// Body block
	b.cur_block = body_block
	b.loop_stack << LoopInfo{
		cond_block: post_block
		exit_block: exit_block
	}
	b.build_stmts(stmt.stmts)
	b.loop_stack.delete_last()

	if !b.block_has_terminator(b.cur_block) {
		// Jump to post (or back to cond)
		target := if has_post { post_block } else { cond_block }
		b.mod.add_instr(.jmp, b.cur_block, 0, [b.mod.blocks[target].val_id])
		b.add_edge(b.cur_block, target)
	}

	// Post block
	if has_post {
		b.cur_block = post_block
		b.build_stmt(stmt.post)
		if !b.block_has_terminator(b.cur_block) {
			b.mod.add_instr(.jmp, b.cur_block, 0, [b.mod.blocks[cond_block].val_id])
			b.add_edge(post_block, cond_block)
		}
	}

	b.cur_block = exit_block
}

fn (mut b Builder) build_if_stmt(node ast.IfExpr) {
	if node.cond is ast.EmptyExpr {
		// Pure else block
		b.build_stmts(node.stmts)
		return
	}

	then_block := b.mod.add_block(b.cur_func, 'if_then')
	merge_block := b.mod.add_block(b.cur_func, 'if_merge')

	has_else := node.else_expr !is ast.EmptyExpr
	else_block := if has_else {
		b.mod.add_block(b.cur_func, 'if_else')
	} else {
		merge_block
	}

	// Condition
	cond_val := b.build_expr(node.cond)
	b.mod.add_instr(.br, b.cur_block, 0,
		[cond_val, b.mod.blocks[then_block].val_id, b.mod.blocks[else_block].val_id])
	b.add_edge(b.cur_block, then_block)
	b.add_edge(b.cur_block, else_block)

	// Then
	b.cur_block = then_block
	then_smartcasts := b.smartcast_variant_type_ids_from_condition(node.cond)
	mut saved_local_smartcasts := b.local_smartcasts.clone()
	b.apply_local_smartcasts(then_smartcasts)
	b.build_stmts(node.stmts)
	b.local_smartcasts = saved_local_smartcasts.move()
	if !b.block_has_terminator(b.cur_block) {
		b.mod.add_instr(.jmp, b.cur_block, 0, [b.mod.blocks[merge_block].val_id])
		b.add_edge(b.cur_block, merge_block)
	}

	// Else
	if has_else {
		b.cur_block = else_block
		if node.else_expr is ast.IfExpr {
			else_if := node.else_expr as ast.IfExpr
			if else_if.cond is ast.EmptyExpr {
				// Pure else
				b.build_stmts(else_if.stmts)
			} else {
				b.build_if_stmt(else_if)
			}
		} else if node.else_expr is ast.UnsafeExpr {
			for s in node.else_expr.stmts {
				b.build_stmt(s)
			}
		}
		if !b.block_has_terminator(b.cur_block) {
			b.mod.add_instr(.jmp, b.cur_block, 0, [b.mod.blocks[merge_block].val_id])
			b.add_edge(b.cur_block, merge_block)
		}
	}
	b.cur_block = merge_block
	// If the merge block has no predecessors (both branches returned/jumped elsewhere),
	// mark it as unreachable so no implicit return is added
	if b.mod.blocks[merge_block].preds.len == 0 {
		b.mod.add_instr(.unreachable, b.cur_block, 0, []ValueID{})
	}
}

fn (mut b Builder) build_if_stmt_from_flat(c ast.Cursor) {
	cond_c := c.edge(0)
	if cond_c.kind() == .expr_empty {
		for i in 2 .. c.edge_count() {
			b.build_stmt_from_flat(c.edge(i))
		}
		return
	}

	then_block := b.mod.add_block(b.cur_func, 'if_then')
	merge_block := b.mod.add_block(b.cur_func, 'if_merge')

	else_c := c.edge(1)
	has_else := else_c.kind() != .expr_empty
	else_block := if has_else {
		b.mod.add_block(b.cur_func, 'if_else')
	} else {
		merge_block
	}

	cond_val := b.build_expr_from_flat(cond_c)
	b.mod.add_instr(.br, b.cur_block, 0,
		[cond_val, b.mod.blocks[then_block].val_id, b.mod.blocks[else_block].val_id])
	b.add_edge(b.cur_block, then_block)
	b.add_edge(b.cur_block, else_block)

	b.cur_block = then_block
	// s230: derive then-branch smartcasts straight from the condition cursor — no decode.
	then_smartcasts := b.smartcast_variant_type_ids_from_condition_from_flat(cond_c)
	mut saved_local_smartcasts := b.local_smartcasts.clone()
	b.apply_local_smartcasts(then_smartcasts)
	for i in 2 .. c.edge_count() {
		b.build_stmt_from_flat(c.edge(i))
	}
	b.local_smartcasts = saved_local_smartcasts.move()
	if !b.block_has_terminator(b.cur_block) {
		b.mod.add_instr(.jmp, b.cur_block, 0, [b.mod.blocks[merge_block].val_id])
		b.add_edge(b.cur_block, merge_block)
	}

	if has_else {
		b.cur_block = else_block
		if else_c.kind() == .expr_if {
			else_cond := else_c.edge(0)
			if else_cond.kind() == .expr_empty {
				for i in 2 .. else_c.edge_count() {
					b.build_stmt_from_flat(else_c.edge(i))
				}
			} else {
				b.build_if_stmt_from_flat(else_c)
			}
		} else if else_c.kind() == .expr_unsafe {
			for i in 0 .. else_c.edge_count() {
				b.build_stmt_from_flat(else_c.edge(i))
			}
		}
		if !b.block_has_terminator(b.cur_block) {
			b.mod.add_instr(.jmp, b.cur_block, 0, [b.mod.blocks[merge_block].val_id])
			b.add_edge(b.cur_block, merge_block)
		}
	}
	b.cur_block = merge_block
	if b.mod.blocks[merge_block].preds.len == 0 {
		b.mod.add_instr(.unreachable, b.cur_block, 0, []ValueID{})
	}
}

// build_flow_control_from_flat is the cursor counterpart of
// `build_flow_control`. The flat schema (`flat.v:1685`) stores
// FlowControlStmt's `op` as `n.aux` (u16 → token.Token) and `label` as
// `n.name_id` — no child edges. The cursor port reads both directly,
// skipping both the FlowControlStmt struct allocation and the
// `Stmt(FlowControlStmt{...})` sum-type boxing that `decode_stmt`
// would perform.
fn (mut b Builder) build_flow_control_from_flat(c ast.Cursor) {
	op := unsafe { token.Token(int(c.aux())) }
	if op == .key_goto {
		label := c.name()
		target := b.get_or_create_label_block(label)
		b.mod.add_instr(.jmp, b.cur_block, 0, [b.mod.blocks[target].val_id])
		b.add_edge(b.cur_block, target)
		dead_block := b.mod.add_block(b.cur_func, 'after_goto')
		b.cur_block = dead_block
		return
	}
	if b.loop_stack.len == 0 {
		return
	}
	loop_info := b.loop_stack[b.loop_stack.len - 1]
	if op == .key_break {
		b.mod.add_instr(.jmp, b.cur_block, 0, [b.mod.blocks[loop_info.exit_block].val_id])
		b.add_edge(b.cur_block, loop_info.exit_block)
	} else if op == .key_continue {
		b.mod.add_instr(.jmp, b.cur_block, 0, [b.mod.blocks[loop_info.cond_block].val_id])
		b.add_edge(b.cur_block, loop_info.cond_block)
	}
}

fn (mut b Builder) build_flow_control(stmt ast.FlowControlStmt) {
	if stmt.op == .key_goto {
		// goto label — jump to the label's block (create if not yet seen)
		target := b.get_or_create_label_block(stmt.label)
		b.mod.add_instr(.jmp, b.cur_block, 0, [b.mod.blocks[target].val_id])
		b.add_edge(b.cur_block, target)
		// Create a new unreachable block for any code after the goto
		dead_block := b.mod.add_block(b.cur_func, 'after_goto')
		b.cur_block = dead_block
		return
	}
	if b.loop_stack.len == 0 {
		return
	}
	loop_info := b.loop_stack[b.loop_stack.len - 1]
	if stmt.op == .key_break {
		b.mod.add_instr(.jmp, b.cur_block, 0, [b.mod.blocks[loop_info.exit_block].val_id])
		b.add_edge(b.cur_block, loop_info.exit_block)
	} else if stmt.op == .key_continue {
		b.mod.add_instr(.jmp, b.cur_block, 0, [b.mod.blocks[loop_info.cond_block].val_id])
		b.add_edge(b.cur_block, loop_info.cond_block)
	}
}

fn (mut b Builder) get_or_create_label_block(name string) BlockID {
	if name in b.label_blocks {
		return b.label_blocks[name]
	}
	block := b.mod.add_block(b.cur_func, 'label_${name}')
	b.label_blocks[name] = block
	return block
}

fn (mut b Builder) build_label_from_flat(c ast.Cursor) {
	label_block := b.get_or_create_label_block(c.name())
	b.mod.add_instr(.jmp, b.cur_block, 0, [b.mod.blocks[label_block].val_id])
	b.add_edge(b.cur_block, label_block)
	b.cur_block = label_block
}

// build_block_from_flat is the cursor counterpart of the `ast.BlockStmt`
// arm in `build_stmt`, which simply forwards `stmt.stmts` to `build_stmts`.
// The flat schema (`flat.v:1610`) emits BlockStmt via `emit_simple` so its
// inner stmts are direct child edges of the `.stmt_block` node — we walk
// them with `c.edge(i)` instead of materialising a `[]ast.Stmt`.
fn (mut b Builder) build_block_from_flat(c ast.Cursor) {
	n := c.edge_count()
	for i in 0 .. n {
		b.build_stmt_from_flat(c.edge(i))
	}
}

// build_module_from_flat is the cursor counterpart of the `ast.ModuleStmt`
// arm in `build_stmt`, which sets `b.cur_module = stmt.name.replace('.', '_')`.
// The flat schema (`flat.v:1764`) emits ModuleStmt with the module name
// interned into `name_id` and no child edges, so `c.name()` is the only
// state we need.
fn (mut b Builder) build_module_from_flat(c ast.Cursor) {
	b.cur_module = c.name().replace('.', '_')
}

// build_assign_from_flat is the cursor counterpart of `build_assign`. The
// flat schema (`flat.v:1600`) stores AssignStmt's lhs exprs at edges
// 0..extra_int(), rhs exprs at edges extra_int()..edge_count(), and the
// assignment operator in `aux` (u16 → token.Token). This helper mirrors the
// legacy assign branches without rehydrating lhs/rhs `ast.Expr` arrays.
fn (mut b Builder) build_assign_from_flat(c ast.Cursor) {
	nlhs := c.extra_int()
	n := c.edge_count()
	nrhs := n - nlhs
	op := unsafe { token.Token(int(c.aux())) }

	if nlhs > 1 && nrhs == 1 {
		rhs_c := c.edge(nlhs)
		mut rhs_val := b.build_expr_from_flat(rhs_c)
		mut rhs_typ_id := b.mod.values[rhs_val].typ
		mut rhs_typ := b.mod.type_store.types[rhs_typ_id]
		if b.is_option_wrapper_type(rhs_typ_id) || b.is_result_wrapper_type(rhs_typ_id) {
			if b.wrapper_has_data(rhs_typ_id) && rhs_typ.kind == .struct_t
				&& rhs_typ.fields.len >= 3 {
				data_type := rhs_typ.fields[2]
				data_idx := b.mod.get_or_add_const(b.mod.type_store.get_int(32), '2')
				rhs_val = b.mod.add_instr(.extractvalue, b.cur_block, data_type, [
					rhs_val,
					data_idx,
				])
				rhs_typ_id = data_type
				rhs_typ = b.mod.type_store.types[rhs_typ_id]
			}
		}
		for i in 0 .. nlhs {
			lhs_c := c.edge(i)
			mut elem_val := rhs_val
			if rhs_typ.kind == .struct_t && i < rhs_typ.fields.len {
				elem_type := rhs_typ.fields[i]
				idx_val := b.mod.get_or_add_const(b.mod.type_store.get_int(32), i.str())
				elem_val = b.mod.add_instr(.extractvalue, b.cur_block, elem_type, [
					rhs_val,
					idx_val,
				])
			}
			if ident_name := b.unwrap_ident_name_from_flat(lhs_c) {
				if op == .decl_assign {
					elem_type := b.mod.values[elem_val].typ
					alloca := b.mod.add_instr(.alloca, b.cur_block,
						b.mod.type_store.get_ptr(elem_type), []ValueID{})
					b.mod.add_instr(.store, b.cur_block, 0, [elem_val, alloca])
					b.vars[ident_name] = alloca
					b.track_array_elem_type_for_local_value(ident_name, elem_val)
				} else if ident_name == '_' {
					continue
				} else {
					ptr := b.assign_target_for_ident(ident_name)
					if ptr != 0 {
						b.mod.add_instr(.store, b.cur_block, 0, [elem_val, ptr])
					}
				}
			}
		}
		return
	}

	if nlhs > 1 && nrhs > 1 && op == .assign {
		mut rhs_vals := []ValueID{cap: nrhs}
		for i in 0 .. nrhs {
			rhs_vals << b.build_expr_from_flat(c.edge(nlhs + i))
		}
		for i in 0 .. nlhs {
			if i >= rhs_vals.len {
				break
			}
			lhs_c := c.edge(i)
			rhs_val := rhs_vals[i]
			if ident_name := b.unwrap_ident_name_from_flat(lhs_c) {
				if ident_name == '_' {
					continue
				}
				ptr := b.assign_target_for_ident(ident_name)
				if ptr != 0 {
					b.mod.add_instr(.store, b.cur_block, 0, [rhs_val, ptr])
				}
			} else if lhs_c.kind() == .expr_selector || lhs_c.kind() == .expr_index {
				base := b.build_addr_from_flat(lhs_c)
				if base != 0 {
					b.mod.add_instr(.store, b.cur_block, 0, [rhs_val, base])
				}
			}
		}
		return
	}

	for i in 0 .. nlhs {
		if i >= nrhs {
			break
		}
		lhs_c := c.edge(i)
		rhs_c := c.edge(nlhs + i)
		rhs_val := b.build_expr_from_flat(rhs_c)

		if ident_name := b.unwrap_ident_name_from_flat(lhs_c) {
			if op == .decl_assign {
				rhs_type := b.mod.values[rhs_val].typ
				alloca := b.mod.add_instr(.alloca, b.cur_block, b.mod.type_store.get_ptr(rhs_type),
					[]ValueID{})
				b.mod.add_instr(.store, b.cur_block, 0, [rhs_val, alloca])
				b.vars[ident_name] = alloca
				b.track_array_elem_type_for_local_from_flat(ident_name, lhs_c, rhs_c, rhs_val)
			} else if ident_name == '_' && op == .assign {
				continue
			} else {
				if rhs_c.kind() == .expr_array_init {
					module_const_name := ssa_module_storage_name(b.cur_module, ident_name)
					if ident_name in b.const_array_globals
						|| module_const_name in b.const_array_globals
						|| 'builtin__${ident_name}' in b.const_array_globals {
						continue
					}
				}
				ptr := b.assign_target_for_ident(ident_name)
				if ptr != 0 {
					b.store_assign_to_ptr(ptr, rhs_val, op, true)
				}
			}
		} else if lhs_c.kind() == .expr_selector {
			base := b.build_addr_from_flat(lhs_c)
			if base != 0 {
				b.store_assign_to_ptr(base, rhs_val, op, true)
			}
		} else if lhs_c.kind() == .expr_prefix {
			lhs_op := unsafe { token.Token(int(lhs_c.aux())) }
			if lhs_op == .mul {
				ptr := b.build_expr_from_flat(lhs_c.edge(0))
				b.store_assign_to_ptr(ptr, rhs_val, op, false)
			}
		} else if lhs_c.kind() == .expr_index {
			base := b.build_addr_from_flat(lhs_c)
			if base != 0 {
				b.store_assign_to_ptr(base, rhs_val, op, true)
			}
		}
	}
}

// build_for_from_flat is the cursor counterpart of `build_for`. The flat
// schema (`flat.v:1715`) stores ForStmt's init at edge 0, cond at edge 1,
// post at edge 2, and the body stmts at edges 3..edge_count(). Empty
// init/post are sentinel `.stmt_empty` cursors; empty cond is a sentinel
// `.expr_empty` cursor (matching the legacy `EmptyStmt` / `EmptyExpr`
// checks). Body stmts are walked directly through `build_stmt_from_flat`
// without materialising a `[]ast.Stmt`. Init/post are dispatched through
// `build_stmt_from_flat` so they pick up flat-native arms; cond goes through
// `build_expr_from_flat`.
fn (mut b Builder) build_for_from_flat(c ast.Cursor) {
	init_c := c.edge(0)
	cond_c := c.edge(1)
	post_c := c.edge(2)
	has_init := init_c.kind() != .stmt_empty
	has_cond := cond_c.kind() != .expr_empty
	has_post := post_c.kind() != .stmt_empty

	if has_init {
		b.build_stmt_from_flat(init_c)
	}

	cond_block := b.mod.add_block(b.cur_func, 'for_cond')
	body_block := b.mod.add_block(b.cur_func, 'for_body')
	post_block := if has_post {
		b.mod.add_block(b.cur_func, 'for_post')
	} else {
		cond_block
	}
	exit_block := b.mod.add_block(b.cur_func, 'for_exit')

	b.mod.add_instr(.jmp, b.cur_block, 0, [b.mod.blocks[cond_block].val_id])
	b.add_edge(b.cur_block, cond_block)

	b.cur_block = cond_block
	if has_cond {
		cond_val := b.build_expr_from_flat(cond_c)
		b.mod.add_instr(.br, b.cur_block, 0,
			[cond_val, b.mod.blocks[body_block].val_id, b.mod.blocks[exit_block].val_id])
		b.add_edge(cond_block, body_block)
		b.add_edge(cond_block, exit_block)
	} else {
		b.mod.add_instr(.jmp, b.cur_block, 0, [b.mod.blocks[body_block].val_id])
		b.add_edge(cond_block, body_block)
	}

	b.cur_block = body_block
	b.loop_stack << LoopInfo{
		cond_block: post_block
		exit_block: exit_block
	}
	n := c.edge_count()
	for i in 3 .. n {
		b.build_stmt_from_flat(c.edge(i))
	}
	b.loop_stack.delete_last()

	if !b.block_has_terminator(b.cur_block) {
		target := if has_post { post_block } else { cond_block }
		b.mod.add_instr(.jmp, b.cur_block, 0, [b.mod.blocks[target].val_id])
		b.add_edge(b.cur_block, target)
	}

	if has_post {
		b.cur_block = post_block
		b.build_stmt_from_flat(post_c)
		if !b.block_has_terminator(b.cur_block) {
			b.mod.add_instr(.jmp, b.cur_block, 0, [b.mod.blocks[cond_block].val_id])
			b.add_edge(post_block, cond_block)
		}
	}

	b.cur_block = exit_block
}

// build_expr_stmt_from_flat is the cursor counterpart of `build_expr_stmt`.
// The flat schema (`flat.v:1678`) stores ExprStmt's inner expr as edge 0.
// Legacy `build_expr_stmt` dispatches on the wrapped expr kind: UnsafeExpr
// walks the inner stmts via `build_stmt`; IfExpr goes through
// `build_if_stmt`; everything else goes through `build_expr`. The flat
// port mirrors that shape but walks UnsafeExpr's inner stmts directly via
// cursors (no `ast.Stmt` rehydration for that branch); the IfExpr +
// IfExpr is handled by a cursor-native statement helper because statement
// semantics differ from if-as-expression; all other expressions are routed
// through `build_expr_from_flat`.
fn (mut b Builder) build_expr_stmt_from_flat(c ast.Cursor) {
	inner := c.edge(0)
	if inner.kind() == .expr_unsafe {
		for i in 0 .. inner.edge_count() {
			b.build_stmt_from_flat(inner.edge(i))
		}
		return
	}
	if inner.kind() == .expr_if {
		b.build_if_stmt_from_flat(inner)
		return
	}
	b.build_expr_from_flat(inner)
}

// build_assert_from_flat is the cursor counterpart of `build_assert`. The
// flat schema (`flat.v:1594`) stores AssertStmt's `expr` as edge 0 and the
// optional `extra` message as edge 1 — legacy `build_assert` only reads
// `expr` (extra is informational and not lowered), so we lower edge 0 via
// `build_expr_from_flat` and drop the AssertStmt struct decode entirely.
fn (mut b Builder) build_assert_from_flat(c ast.Cursor) {
	ec := c.edge(0)
	cond := b.build_expr_from_flat(ec)
	pass_block := b.mod.add_block(b.cur_func, 'assert_pass')
	fail_block := b.mod.add_block(b.cur_func, 'assert_fail')

	b.mod.add_instr(.br, b.cur_block, 0,
		[cond, b.mod.blocks[pass_block].val_id, b.mod.blocks[fail_block].val_id])
	b.add_edge(b.cur_block, pass_block)
	b.add_edge(b.cur_block, fail_block)

	b.cur_block = fail_block
	one := b.mod.get_or_add_const(b.mod.type_store.get_int(32), '1')
	exit_ref := b.get_or_create_fn_ref('exit', b.mod.type_store.get_int(32))
	b.mod.add_instr(.call, b.cur_block, 0, [exit_ref, one])
	b.mod.add_instr(.unreachable, b.cur_block, 0, []ValueID{})

	b.cur_block = pass_block
}

fn (mut b Builder) build_label(stmt ast.LabelStmt) {
	label_block := b.get_or_create_label_block(stmt.name)
	// Fall through from current block to label block
	b.mod.add_instr(.jmp, b.cur_block, 0, [b.mod.blocks[label_block].val_id])
	b.add_edge(b.cur_block, label_block)
	b.cur_block = label_block
}

fn (mut b Builder) build_assert(stmt ast.AssertStmt) {
	// assert expr -> if (!expr) { exit(1) }
	cond := b.build_expr(stmt.expr)
	pass_block := b.mod.add_block(b.cur_func, 'assert_pass')
	fail_block := b.mod.add_block(b.cur_func, 'assert_fail')

	b.mod.add_instr(.br, b.cur_block, 0,
		[cond, b.mod.blocks[pass_block].val_id, b.mod.blocks[fail_block].val_id])
	b.add_edge(b.cur_block, pass_block)
	b.add_edge(b.cur_block, fail_block)

	// Fail block: call exit(1)
	b.cur_block = fail_block
	one := b.mod.get_or_add_const(b.mod.type_store.get_int(32), '1')
	exit_ref := b.get_or_create_fn_ref('exit', b.mod.type_store.get_int(32))
	b.mod.add_instr(.call, b.cur_block, 0, [exit_ref, one])
	b.mod.add_instr(.unreachable, b.cur_block, 0, []ValueID{})

	b.cur_block = pass_block
}

// --- Expression building ---

fn (mut b Builder) build_expr(expr ast.Expr) ValueID {
	if !builder_expr_ok(expr) {
		return b.mod.get_or_add_const(b.mod.type_store.get_int(32), '0')
	}
	match expr {
		ast.BasicLiteral {
			return b.build_basic_literal(expr)
		}
		ast.StringLiteral {
			return b.build_string_literal(expr)
		}
		ast.Ident {
			return b.build_ident(expr)
		}
		ast.InfixExpr {
			return b.build_infix(expr)
		}
		ast.PrefixExpr {
			return b.build_prefix(expr)
		}
		ast.CallExpr {
			return b.build_call(expr)
		}
		ast.SelectorExpr {
			return b.build_selector(expr)
		}
		ast.IndexExpr {
			return b.build_index(expr)
		}
		ast.IfExpr {
			return b.build_if_expr(expr)
		}
		ast.InitExpr {
			return b.build_init_expr(expr)
		}
		ast.CastExpr {
			return b.build_cast(expr)
		}
		ast.ParenExpr {
			return b.build_expr(expr.expr)
		}
		ast.ModifierExpr {
			return b.build_expr(expr.expr)
		}
		ast.UnsafeExpr {
			if expr.stmts.len > 0 {
				for i := 0; i < expr.stmts.len - 1; i++ {
					b.build_stmt(expr.stmts[i])
				}
				last := expr.stmts[expr.stmts.len - 1]
				if last is ast.ExprStmt {
					return b.build_expr(last.expr)
				}
				b.build_stmt(last)
			}
			return b.mod.get_or_add_const(b.mod.type_store.get_int(32), '0')
		}
		ast.Keyword {
			return b.build_keyword(expr)
		}
		ast.KeywordOperator {
			return b.build_keyword_operator(expr)
		}
		ast.PostfixExpr {
			return b.build_postfix(expr)
		}
		ast.ArrayInitExpr {
			return b.build_array_init_expr(expr)
		}
		ast.MapInitExpr {
			// TODO: map init
			return b.mod.get_or_add_const(b.mod.type_store.get_int(64), '0')
		}
		ast.StringInterLiteral {
			return b.build_string_inter_literal(expr)
		}
		ast.MatchExpr {
			return b.build_match_expr(expr)
		}
		ast.OrExpr {
			return b.build_expr(expr.expr)
		}
		ast.FnLiteral {
			return b.build_fn_literal(expr)
		}
		ast.RangeExpr {
			return b.mod.get_or_add_const(b.mod.type_store.get_int(64), '0')
		}
		ast.CallOrCastExpr {
			return b.build_call_or_cast(expr)
		}
		ast.AsCastExpr {
			return b.build_as_cast(expr)
		}
		ast.AssocExpr {
			return b.mod.get_or_add_const(b.mod.type_store.get_int(64), '0')
		}
		ast.Tuple {
			if expr.exprs.len > 0 {
				return b.build_expr(expr.exprs[0])
			}
			return b.mod.get_or_add_const(b.mod.type_store.get_int(32), '0')
		}
		else {
			return b.mod.get_or_add_const(b.mod.type_store.get_int(32), '0')
		}
	}
}

// build_expr_from_flat is the cursor counterpart of `build_expr`. Each
// per-kind arm consumes the cursor's flat fields directly for the outer node
// and only decodes child expressions where the existing legacy helper still
// pattern-matches on those child AST shapes.
fn (mut b Builder) build_expr_from_flat(c ast.Cursor) ValueID {
	match c.kind() {
		.expr_basic_literal {
			return b.build_basic_literal_from_flat(c)
		}
		.expr_ident {
			return b.build_ident_from_flat(c)
		}
		.expr_string {
			return b.build_string_literal_from_flat(c)
		}
		.expr_paren, .expr_modifier {
			return b.build_expr_from_flat(c.edge(0))
		}
		.expr_prefix {
			return b.build_prefix_from_flat(c)
		}
		.expr_selector {
			return b.build_selector_from_flat(c)
		}
		.expr_cast {
			return b.build_cast_from_flat(c)
		}
		.expr_infix {
			return b.build_infix_from_flat(c)
		}
		.expr_index {
			return b.build_index_from_flat(c)
		}
		.expr_if {
			return b.build_if_from_flat(c)
		}
		.expr_call {
			return b.build_call_from_flat(c)
		}
		.expr_init {
			return b.build_init_from_flat(c)
		}
		.expr_unsafe {
			return b.build_unsafe_from_flat(c)
		}
		.expr_keyword {
			return b.build_keyword_from_flat(c)
		}
		.expr_keyword_operator {
			return b.build_keyword_operator_from_flat(c)
		}
		.expr_postfix {
			return b.build_postfix_from_flat(c)
		}
		.expr_array_init {
			return b.build_array_init_from_flat(c)
		}
		.expr_string_inter {
			return b.build_string_inter_from_flat(c)
		}
		.expr_match {
			return b.build_match_expr_from_flat(c)
		}
		.expr_or {
			return b.build_or_from_flat(c)
		}
		.expr_fn_literal {
			return b.build_fn_literal_from_flat(c)
		}
		.expr_call_or_cast {
			return b.build_call_or_cast_from_flat(c)
		}
		.expr_as_cast {
			return b.build_as_cast_from_flat(c)
		}
		.expr_tuple {
			return b.build_tuple_from_flat(c)
		}
		.expr_map_init, .expr_range, .expr_assoc {
			return b.mod.get_or_add_const(b.mod.type_store.get_int(64), '0')
		}
		.expr_empty, .expr_if_guard, .expr_comptime, .expr_generic_arg_or_index,
		.expr_generic_args, .expr_lambda, .expr_lifetime, .expr_lock, .expr_select, .expr_sql {
			return b.mod.get_or_add_const(b.mod.type_store.get_int(32), '0')
		}
		else {
			return b.mod.get_or_add_const(b.mod.type_store.get_int(32), '0')
		}
	}
}

// build_ident_from_flat (s187) decodes only what `build_ident` needs from
// an `.expr_ident` cursor: `name` from `c.name()` (interned string) and
// `pos` from `c.pos()`. Ident has no aux/extra/edges. Dispatches to the
// existing `build_ident`. Pos is preserved so any `get_checked_expr_type`
// lookup downstream stays hot (Ident type can be checker-resolved when
// the var was declared in scope). Saves the `Expr(Ident{...})` sum-type box.
fn (mut b Builder) build_ident_from_flat(c ast.Cursor) ValueID {
	return b.build_ident(ast.Ident{
		name: c.name()
		pos:  c.pos()
	})
}

// build_string_literal_from_flat (s188) decodes only what `build_string_literal`
// needs from an `.expr_string` cursor: `kind` from `c.aux()`
// (u16 → StringLiteralKind), `value` from `c.name()` (interned string),
// `pos` from `c.pos()`. No edges. Dispatches to existing build_string_literal.
// Saves the `Expr(StringLiteral{...})` sum-type box.
fn (mut b Builder) build_string_literal_from_flat(c ast.Cursor) ValueID {
	kind := unsafe { ast.StringLiteralKind(int(c.aux())) }
	return b.build_string_literal(ast.StringLiteral{
		kind:  kind
		value: c.name()
		pos:   c.pos()
	})
}

// build_prefix_from_flat (s207 cursor-native rewrite) reads `op` from
// `c.aux()` and dispatches per inner-expr `kind()` directly:
// - amp + .expr_init still rehydrates the InitExpr (no cursor-native
//   `collect_init_expr_values_from_flat` yet — left for a follow-up)
// - amp + .expr_cast / .expr_call_or_cast / .expr_prefix(amp,...) take the
//   pointer-type-cast bitcast paths cursor-natively (read the inner type
//   from `edge(0)` of the cast/CoCe, the value from `edge(1)`, then call
//   `ast_type_to_ssa_from_flat` + `build_expr_from_flat`)
// - minus + .expr_basic_literal float-literal negation uses `inner_c.name()`
//   to detect the float form and emit a negated constant directly
// - default amp path calls `build_addr_from_flat(inner_c)` (already cursor-
//   native), with `build_expr_from_flat(inner_c)` as the no-address fallback
// - non-amp ops build the operand via `build_expr_from_flat` then apply the
//   per-op arithmetic verbatim from legacy `build_prefix`.
fn (mut b Builder) build_prefix_from_flat(c ast.Cursor) ValueID {
	op := unsafe { token.Token(int(c.aux())) }
	inner_c := c.edge(0)
	inner_kind := inner_c.kind()

	// &InitExpr → heap-allocated struct init (cursor-native via
	// `build_init_expr_ptr_from_flat` / `collect_init_expr_values_from_flat`).
	if op == .amp && inner_kind == .expr_init {
		return b.build_init_expr_ptr_from_flat(inner_c)
	}

	// &CastExpr / &CallOrCastExpr → pointer-type cast (bitcast).
	if op == .amp && inner_kind == .expr_cast {
		typ_c := inner_c.edge(0)
		val_c := inner_c.edge(1)
		inner_val := b.build_expr_from_flat(val_c)
		if !b.valid_value_id(inner_val) {
			return inner_val
		}
		inner_type := b.mod.values[inner_val].typ
		if inner_type > 0 && int(inner_type) < b.mod.type_store.types.len {
			inner_t := b.mod.type_store.types[inner_type]
			if inner_t.kind == .ptr_t || inner_t.kind == .int_t {
				target_elem := b.ast_type_to_ssa_from_flat(typ_c)
				ptr_type := b.mod.type_store.get_ptr(target_elem)
				return b.mod.add_instr(.bitcast, b.cur_block, ptr_type, [inner_val])
			}
		}
	}
	if op == .amp && inner_kind == .expr_call_or_cast {
		lhs_c := inner_c.edge(0)
		expr_c := inner_c.edge(1)
		inner_val := b.build_expr_from_flat(expr_c)
		if !b.valid_value_id(inner_val) {
			return inner_val
		}
		inner_type := b.mod.values[inner_val].typ
		if inner_type > 0 && int(inner_type) < b.mod.type_store.types.len {
			inner_t := b.mod.type_store.types[inner_type]
			if inner_t.kind == .ptr_t || inner_t.kind == .int_t {
				target_elem := b.ast_type_to_ssa_from_flat(lhs_c)
				ptr_type := b.mod.type_store.get_ptr(target_elem)
				return b.mod.add_instr(.bitcast, b.cur_block, ptr_type, [inner_val])
			}
		}
	}

	// &&T(expr) → **T pointer-type cast (nested PrefixExpr(.amp,...)).
	if op == .amp && inner_kind == .expr_prefix {
		inner_op := unsafe { token.Token(int(inner_c.aux())) }
		if inner_op == .amp {
			inner2_c := inner_c.edge(0)
			inner2_kind := inner2_c.kind()
			if inner2_kind == .expr_call_or_cast {
				lhs_c := inner2_c.edge(0)
				expr_c := inner2_c.edge(1)
				inner_val := b.build_expr_from_flat(expr_c)
				if !b.valid_value_id(inner_val) {
					return inner_val
				}
				inner_type := b.mod.values[inner_val].typ
				if inner_type > 0 && int(inner_type) < b.mod.type_store.types.len {
					inner_t := b.mod.type_store.types[inner_type]
					if inner_t.kind == .ptr_t || inner_t.kind == .int_t {
						target_elem := b.ast_type_to_ssa_from_flat(lhs_c)
						ptr_type := b.mod.type_store.get_ptr(target_elem)
						ptr_ptr_type := b.mod.type_store.get_ptr(ptr_type)
						return b.mod.add_instr(.bitcast, b.cur_block, ptr_ptr_type, [
							inner_val,
						])
					}
				}
			} else if inner2_kind == .expr_cast {
				typ_c := inner2_c.edge(0)
				val_c := inner2_c.edge(1)
				inner_val := b.build_expr_from_flat(val_c)
				if !b.valid_value_id(inner_val) {
					return inner_val
				}
				inner_type := b.mod.values[inner_val].typ
				if inner_type > 0 && int(inner_type) < b.mod.type_store.types.len {
					inner_t := b.mod.type_store.types[inner_type]
					if inner_t.kind == .ptr_t || inner_t.kind == .int_t {
						target_elem := b.ast_type_to_ssa_from_flat(typ_c)
						ptr_type := b.mod.type_store.get_ptr(target_elem)
						ptr_ptr_type := b.mod.type_store.get_ptr(ptr_type)
						return b.mod.add_instr(.bitcast, b.cur_block, ptr_ptr_type, [
							inner_val,
						])
					}
				}
			}
		}
	}

	// -float_literal → negated float constant (handles -0.0 sign-bit correctly).
	if op == .minus && inner_kind == .expr_basic_literal {
		val := inner_c.name()
		is_float_lit := val.contains('.')
			|| (!val.starts_with('0x') && !val.starts_with('0X')
			&& (val.contains('e') || val.contains('E')))
		if is_float_lit {
			neg_str := '-' + val
			float_type := b.mod.type_store.get_float(64)
			return b.mod.get_or_add_const(float_type, neg_str)
		}
	}

	if op == .amp {
		addr := b.build_addr_from_flat(inner_c)
		if addr != 0 {
			if b.in_sumtype_data {
				if heap_ptr := b.heap_copy_from_address(addr) {
					return heap_ptr
				}
			}
			return addr
		}
		val := b.build_expr_from_flat(inner_c)
		if !b.valid_value_id(val) {
			return val
		}
		if b.mod.values[val].kind == .func_ref {
			return val
		}
		if b.in_sumtype_data {
			if heap_ptr := b.heap_copy_value(val) {
				return heap_ptr
			}
		}
		val_type := b.mod.values[val].typ
		if val_type > 0 && int(val_type) < b.mod.type_store.types.len {
			ptr_type := b.mod.type_store.get_ptr(val_type)
			typ_info := b.mod.type_store.types[val_type]
			if typ_info.kind == .struct_t {
				heap_ptr := b.mod.add_instr(.heap_alloc, b.cur_block, ptr_type, []ValueID{})
				b.mod.add_instr(.store, b.cur_block, 0, [val, heap_ptr])
				return heap_ptr
			}
			alloca := b.mod.add_instr(.alloca, b.cur_block, ptr_type, []ValueID{})
			b.mod.add_instr(.store, b.cur_block, 0, [val, alloca])
			return alloca
		}
		return val
	}

	val := b.build_expr_from_flat(inner_c)
	if !b.valid_value_id(val) {
		return val
	}

	match op {
		.minus {
			val_type := b.mod.values[val].typ
			is_float := val_type > 0 && int(val_type) < b.mod.type_store.types.len
				&& b.mod.type_store.types[val_type].kind == .float_t
			if is_float {
				i64_type := b.mod.type_store.get_int(64)
				sign_mask := b.mod.get_or_add_const(i64_type, '0x8000000000000000')
				int_val := b.mod.add_instr(.bitcast, b.cur_block, i64_type, [val])
				xored := b.mod.add_instr(.xor, b.cur_block, i64_type, [int_val, sign_mask])
				return b.mod.add_instr(.bitcast, b.cur_block, val_type, [xored])
			}
			if val_type <= 0 {
				return val
			}
			zero := b.mod.get_or_add_const(val_type, '0')
			return b.mod.add_instr(.sub, b.cur_block, val_type, [zero, val])
		}
		.not {
			val_type := b.mod.values[val].typ
			if val_type <= 0 {
				return val
			}
			zero := b.mod.get_or_add_const(val_type, '0')
			return b.mod.add_instr(.eq, b.cur_block, b.mod.type_store.get_int(1), [
				val,
				zero,
			])
		}
		.bit_not {
			val_type := b.mod.values[val].typ
			if val_type <= 0 {
				return val
			}
			neg_one := b.mod.get_or_add_const(val_type, '-1')
			return b.mod.add_instr(.xor, b.cur_block, val_type, [val, neg_one])
		}
		.mul {
			val_type := b.mod.values[val].typ
			if val_type > 0 && int(val_type) < b.mod.type_store.types.len {
				typ := b.mod.type_store.types[val_type]
				if typ.kind == .ptr_t && typ.elem_type > 0 {
					result := b.mod.add_instr(.load, b.cur_block, typ.elem_type, [val])
					b.track_array_value_elem_type_from_checked_cursor(result, c)
					return result
				}
			}
			return val
		}
		else {
			return val
		}
	}
}

// build_selector_from_flat (s191) reads `lhs` from edge 0 and `rhs` (Ident)
// from edge 1. The lhs must be fully decoded via `decode_expr` because
// build_selector pattern-matches on it heavily (`is ast.EmptyExpr` for enum
// shorthand, `is ast.Ident` for `C.X` and module-qualified access, and
// recursive `build_expr(expr.lhs)` / `build_selector_addr(expr.lhs)` for
// chained selectors). The rhs Ident is reconstructed from `edge(1).name()` +
// `edge(1).pos()` — Ident is just (pos, name), no other fields. Saves only
// the outer `Expr(SelectorExpr{...})` sum-type box.
// build_selector_from_flat (s209 cursor-native rewrite) reads lhs from
// `c.edge(0)` and the rhs Ident name from `c.edge(1).name()`. Dispatches per
// `lhs_c.kind()` to the same set of cases as legacy `build_selector`:
// - `.expr_empty` or `.expr_ident` with empty name → enum shorthand
// - `.expr_ident` with name == 'C' → C constant / global resolution
// - `.expr_ident` → module-qualified const/global, qualified enum, const-
//   array `.len`
// - default → address-based selector lowering via
//   `build_selector_addr_from_flat`, fallback to `build_expr_from_flat(lhs_c)`
//   + `field_index_from_flat` + extractvalue / GEP+load.
fn (mut b Builder) build_selector_from_flat(c ast.Cursor) ValueID {
	lhs_c := c.edge(0)
	rhs_c := c.edge(1)
	rhs_name := rhs_c.name()
	lhs_kind := lhs_c.kind()
	lhs_name := if lhs_kind == .expr_ident { lhs_c.name() } else { '' }

	// Enum shorthand: `.field`
	if lhs_kind == .expr_empty || (lhs_kind == .expr_ident && lhs_name == '') {
		field_symbol := enum_field_symbol_name(rhs_name)
		suffix := '__${field_symbol}'
		mut match_keys := []string{}
		mut match_vals := []int{}
		for key, val in b.enum_values {
			if key.ends_with(suffix) {
				match_keys << key
				match_vals << val
			}
		}
		if match_keys.len == 1 {
			return b.mod.get_or_add_const(b.mod.type_store.get_int(32), match_vals[0].str())
		}
		if match_keys.len > 1 {
			if b.cur_module != '' {
				if b.cur_module == 'main' {
					for i, mk in match_keys {
						if mk.split('__').len == 2 {
							return b.mod.get_or_add_const(b.mod.type_store.get_int(32),
								match_vals[i].str())
						}
					}
				} else {
					for i, mk in match_keys {
						if mk.starts_with('${b.cur_module}__') {
							return b.mod.get_or_add_const(b.mod.type_store.get_int(32),
								match_vals[i].str())
						}
					}
				}
			}
			return b.mod.get_or_add_const(b.mod.type_store.get_int(32), match_vals[0].str())
		}
		return b.mod.get_or_add_const(b.mod.type_store.get_int(32), '0')
	}

	// Qualified enum access: `EnumType.value`
	if lhs_kind == .expr_ident {
		enum_key := '${lhs_name}__${enum_field_symbol_name(rhs_name)}'
		if enum_key in b.enum_values {
			return b.mod.get_or_add_const(b.mod.type_store.get_int(32),
				b.enum_values[enum_key].str())
		}
		qualified_key := '${b.cur_module}__${enum_key}'
		if qualified_key in b.enum_values {
			return b.mod.get_or_add_const(b.mod.type_store.get_int(32),
				b.enum_values[qualified_key].str())
		}
	}

	// C.<name>
	if lhs_kind == .expr_ident && lhs_name == 'C' {
		c_name := rhs_name
		if win_const := b.build_windows_c_macro_const(c_name) {
			return win_const
		}
		c_const_val := match c_name {
			'SEEK_SET' { '0' }
			'SEEK_CUR' { '1' }
			'SEEK_END' { '2' }
			'EOF' { '-1' }
			'NULL' { '0' }
			'O_RDONLY' { '0' }
			'O_WRONLY' { '1' }
			'O_RDWR' { '2' }
			'O_CREAT' { '512' }
			'O_TRUNC' { '1024' }
			'O_EXCL' { '2048' }
			'O_APPEND' { '8' }
			'S_IRUSR' { '256' }
			'S_IWUSR' { '128' }
			'S_IXUSR' { '64' }
			'S_IREAD' { '256' }
			'S_IWRITE' { '128' }
			'S_IEXEC' { '64' }
			'PROT_READ' { '1' }
			'PROT_WRITE' { '2' }
			'SIGTERM' { '15' }
			'SIGKILL' { '9' }
			'SIGINT' { '2' }
			'STDIN_FILENO' { '0' }
			'STDOUT_FILENO' { '1' }
			'STDERR_FILENO' { '2' }
			'DT_DIR' { '4' }
			'DT_REG' { '8' }
			'DT_LNK' { '10' }
			'DT_UNKNOWN' { '0' }
			'ENOENT' { '2' }
			'EXIT_SUCCESS' { '0' }
			'EXIT_FAILURE' { '1' }
			'ICANON' { '256' }
			'ECHO' { '8' }
			'TCSANOW' { '0' }
			'ISIG' { '128' }
			'IEXTEN' { '1024' }
			'TOSTOP' { '4194304' }
			'TIOCGWINSZ' { '1074295912' }
			'CLOCK_MONOTONIC' { '6' }
			'CLOCK_REALTIME' { '0' }
			'_SC_PAGESIZE' { '29' }
			'_SC_NPROCESSORS_ONLN' { '58' }
			'_SC_PHYS_PAGES' { '200' }
			'EINTR' { '4' }
			'EINVAL' { '22' }
			'EAGAIN' { '35' }
			'EWOULDBLOCK' { '35' }
			'EINPROGRESS' { '36' }
			'EACCES' { '13' }
			'EFAULT' { '14' }
			'EBUSY' { '16' }
			'ETIMEDOUT' { '60' }
			'S_IFBLK' { '24576' }
			'S_IFCHR' { '8192' }
			'S_IFDIR' { '16384' }
			'S_IFIFO' { '4096' }
			'S_IFLNK' { '40960' }
			'S_IFMT' { '61440' }
			'S_IFREG' { '32768' }
			'S_IFSOCK' { '49152' }
			'S_IRGRP' { '32' }
			'S_IROTH' { '4' }
			'S_IWGRP' { '16' }
			'S_IWOTH' { '2' }
			'S_IXGRP' { '8' }
			'S_IXOTH' { '1' }
			'PT_DETACH' { '11' }
			'PT_TRACE_ME' { '0' }
			'SIG_ERR' { '-1' }
			'SIG_BLOCK' { '1' }
			'SIG_UNBLOCK' { '2' }
			'SIG_SETMASK' { '3' }
			'SIGCONT' { '19' }
			'SIGSTOP' { '17' }
			'WNOHANG' { '1' }
			'_IOFBF' { '0' }
			'_IOLBF' { '1' }
			'_IONBF' { '2' }
			'O_NONBLOCK' { '4' }
			'O_CLOEXEC' { '16777216' }
			else { '' }
		}

		if c_const_val.len > 0 {
			return b.mod.get_or_add_const(b.mod.type_store.get_int(32), c_const_val)
		}
		if c_name == 'FLT_EPSILON' {
			return b.mod.get_or_add_const(b.mod.type_store.get_float(32), '1.19209290e-07')
		}
		if c_name == 'DBL_EPSILON' {
			return b.mod.get_or_add_const(b.mod.type_store.get_float(64), '2.2204460492503131e-16')
		}
		if c_name == '_wyp' {
			i64_t := b.mod.type_store.get_int(64)
			return b.mod.get_or_add_const(i64_t, '0')
		}
		// errno is not portable linkable data on targets with TLS errno.
		// Darwin exposes __error(); glibc/LSB expose __errno_location();
		// Windows CRT exposes _errno(). build_c_errno_storage_addr() maps each
		// target to the right accessor (with a raw `errno` global fallback).
		if c_name == 'errno' {
			i32_t := b.mod.type_store.get_int(32)
			errno_addr := b.build_c_errno_storage_addr()
			return b.mod.add_instr(.load, b.cur_block, i32_t, [errno_addr])
		}
		target_name := if b.is_macos_target() {
			match c_name {
				'stdout' { '__stdoutp' }
				'stderr' { '__stderrp' }
				'stdin' { '__stdinp' }
				else { c_name }
			}
		} else {
			c_name
		}
		i8_t := b.mod.type_store.get_int(8)
		ptr_t := b.mod.type_store.get_ptr(i8_t)
		if c_name in ['stdout', 'stderr', 'stdin'] {
			glob := b.mod.add_external_global(target_name, ptr_t)
			b.global_refs[target_name] = glob
			return b.mod.add_instr(.load, b.cur_block, ptr_t, [glob])
		}
		glob := b.mod.add_value_node(.global, ptr_t, target_name, 0)
		b.global_refs[target_name] = glob
		return glob
	}

	// Module-qualified constant/global access.
	if lhs_kind == .expr_ident {
		mut mod_name := ''
		if resolved_mod := b.selector_module_name_from_flat(c) {
			mod_name = resolved_mod
		} else if b.env == unsafe { nil } {
			mod_name = lhs_name.replace('.', '_')
		}
		if mod_name != '' {
			qualified := ssa_module_storage_name(mod_name, rhs_name)
			if fval := b.float_const_values[qualified] {
				return b.mod.get_or_add_const(b.mod.type_store.get_float(64), fval)
			}
			if qualified in b.const_values {
				ct := if qualified in b.const_value_types {
					b.const_value_types[qualified]
				} else {
					b.mod.type_store.get_int(64)
				}
				return b.mod.get_or_add_const(ct, b.const_values[qualified].str())
			}
			if qualified in b.string_const_values {
				return b.build_string_literal(ast.StringLiteral{
					kind:  .v
					value: b.string_const_values[qualified]
				})
			}
			if qualified in b.const_array_globals {
				if glob_id := b.find_global(qualified) {
					return glob_id
				}
			}
			if glob_id := b.find_global(qualified) {
				glob_typ := b.mod.values[glob_id].typ
				elem_typ := b.mod.type_store.types[glob_typ].elem_type
				return b.mod.add_instr(.load, b.cur_block, elem_typ, [glob_id])
			}
		}
	}

	// Const array global `.len`
	if rhs_name == 'len' && lhs_kind == .expr_ident {
		if count := b.const_array_elem_count[lhs_name] {
			return b.mod.get_or_add_const(b.mod.type_store.get_int(64), count.str())
		}
		qualified := '${b.cur_module}__${lhs_name}'
		if count := b.const_array_elem_count[qualified] {
			return b.mod.get_or_add_const(b.mod.type_store.get_int(64), count.str())
		}
		builtin_name := 'builtin__${lhs_name}'
		if count := b.const_array_elem_count[builtin_name] {
			return b.mod.get_or_add_const(b.mod.type_store.get_int(64), count.str())
		}
	}

	skip_addr_selector := lhs_kind == .expr_ident && b.ident_has_inline_const_value(lhs_name)
	if !skip_addr_selector {
		if selector_addr := b.build_selector_addr_from_flat(c) {
			addr_type := b.mod.values[selector_addr.addr].typ
			if addr_type > 0 && int(addr_type) < b.mod.type_store.types.len {
				ptr_type := b.mod.type_store.types[addr_type]
				if ptr_type.kind == .ptr_t && ptr_type.elem_type > 0 {
					field_val := b.mod.add_instr(.load, b.cur_block, ptr_type.elem_type, [
						selector_addr.addr,
					])
					if ptr_type.elem_type == b.get_array_type() {
						b.track_array_value_elem_type_from_selector_from_flat(c, field_val,
							selector_addr.base_type)
					}
					return field_val
				}
			}
		}
	}

	mut base := b.build_expr_from_flat(lhs_c)
	if field_val := b.build_sumtype_variant_selector_value(base,
		b.selector_lhs_variant_type_id_from_flat(lhs_c), rhs_name)
	{
		if b.mod.values[field_val].typ == b.get_array_type() {
			b.track_array_value_elem_type_from_selector_from_flat(c, field_val,
				b.mod.values[base].typ)
		}
		return field_val
	}
	// Fixed-size array `.len`
	base_typ_raw := b.mod.values[base].typ
	if base_typ_raw > 0 && int(base_typ_raw) < b.mod.type_store.types.len {
		mut check_typ := b.mod.type_store.types[base_typ_raw]
		if check_typ.kind == .ptr_t && check_typ.elem_type > 0
			&& int(check_typ.elem_type) < b.mod.type_store.types.len {
			check_typ = b.mod.type_store.types[check_typ.elem_type]
		}
		if check_typ.kind == .array_t && rhs_name == 'len' {
			return b.mod.get_or_add_const(b.mod.type_store.get_int(64), check_typ.len.str())
		}
	}
	// Auto-deref pointer-to-struct → GEP+load
	base_typ := b.mod.values[base].typ
	if base_typ > 0 && int(base_typ) < b.mod.type_store.types.len
		&& b.mod.type_store.types[base_typ].kind == .ptr_t {
		pointee := b.mod.type_store.types[base_typ].elem_type
		if pointee > 0 && int(pointee) < b.mod.type_store.types.len
			&& b.mod.type_store.types[pointee].kind == .struct_t {
			field_idx := b.field_index_from_flat(c, base)
			if field_idx < 0 {
				return 0
			}
			mut result_type := TypeID(0)
			pointee_typ := b.mod.type_store.types[pointee]
			if field_idx >= 0 && field_idx < pointee_typ.fields.len {
				result_type = pointee_typ.fields[field_idx]
			}
			if result_type == 0 {
				result_type = b.expr_type_from_flat(c)
			}
			field_ptr_type := b.mod.type_store.get_ptr(result_type)
			field_ptr := b.mod.add_instr(.get_element_ptr, b.cur_block, field_ptr_type, [
				base,
				b.mod.get_or_add_const(b.mod.type_store.get_int(32), field_idx.str()),
			])
			field_val := b.mod.add_instr(.load, b.cur_block, result_type, [field_ptr])
			if result_type == b.get_array_type() {
				elem_type := b.struct_field_array_elem_type(pointee, rhs_name)
				if elem_type != 0 {
					b.array_value_elem_types[field_val] = elem_type
				} else if checked_field_type := b.checked_struct_field_type_from_ssa(pointee,
					rhs_name)
				{
					checked_elem_type := b.unwrap_to_array_elem_ssa(checked_field_type)
					if checked_elem_type != 0 {
						b.array_value_elem_types[field_val] = checked_elem_type
					}
				}
			}
			return field_val
		}
	}
	mut field_idx := -1
	mut result_type := TypeID(0)
	actual_base_type := b.mod.values[base].typ
	if actual_base_type > 0 && int(actual_base_type) < b.mod.type_store.types.len {
		typ := b.mod.type_store.types[actual_base_type]
		if typ.kind == .struct_t {
			for i, name in typ.field_names {
				if name == rhs_name {
					field_idx = i
					result_type = typ.fields[i]
					break
				}
			}
			if field_idx < 0 && b.ssa_type_is_sumtype(actual_base_type)
				&& !rhs_name.starts_with('arg') {
				return 0
			}
		}
	}
	if field_idx < 0 {
		field_idx = b.field_index_from_flat(c, base)
	}
	if field_idx < 0 {
		return 0
	}
	if result_type == 0 {
		result_type = b.expr_type_from_flat(c)
	}
	field_val := b.mod.add_instr(.extractvalue, b.cur_block, result_type, [base,
		b.mod.get_or_add_const(b.mod.type_store.get_int(32), field_idx.str())])
	if result_type == b.get_array_type() {
		b.track_array_value_elem_type_from_selector_from_flat(c, field_val, actual_base_type)
	}
	return field_val
}

// build_selector_addr_from_flat is the cursor-native counterpart of
// `build_selector_addr`. Reads lhs from `c.edge(0)` (via build_addr_from_flat),
// resolves field index via `field_index_from_flat`, and returns the GEP-built
// SelectorAddr (field pointer + base struct type for downstream tracking).
fn (mut b Builder) build_selector_addr_from_flat(c ast.Cursor) ?SelectorAddr {
	rhs_name := c.edge(1).name()
	base := b.build_addr_from_flat(c.edge(0))
	if base == 0 {
		return none
	}
	base_type := b.mod.values[base].typ
	if base_type <= 0 || int(base_type) >= b.mod.type_store.types.len {
		return none
	}
	ptr_type := b.mod.type_store.types[base_type]
	if ptr_type.kind != .ptr_t || ptr_type.elem_type <= 0
		|| int(ptr_type.elem_type) >= b.mod.type_store.types.len {
		return none
	}
	struct_type := b.mod.type_store.types[ptr_type.elem_type]
	if struct_type.kind != .struct_t {
		return none
	}
	mut field_idx := -1
	for i, name in struct_type.field_names {
		if name == rhs_name {
			field_idx = i
			break
		}
	}
	if field_idx < 0 && b.ssa_type_is_sumtype(ptr_type.elem_type) {
		return none
	}
	if field_idx < 0 {
		field_idx = b.field_index_from_flat(c, base)
	}
	if field_idx < 0 || field_idx >= struct_type.fields.len {
		return none
	}
	field_type := struct_type.fields[field_idx]
	field_ptr_type := b.mod.type_store.get_ptr(field_type)
	idx_val := b.mod.get_or_add_const(b.mod.type_store.get_int(32), field_idx.str())
	addr := b.mod.add_instr(.get_element_ptr, b.cur_block, field_ptr_type, [base, idx_val])
	return SelectorAddr{
		addr:      addr
		base_type: ptr_type.elem_type
	}
}

// track_array_value_elem_type_from_selector_from_flat is the cursor-native
// counterpart of `track_array_value_elem_type_from_selector` for SSA-array
// values produced by selector loads.
fn (mut b Builder) track_array_value_elem_type_from_selector_from_flat(c ast.Cursor, field_val ValueID, base_type TypeID) {
	rhs_name := c.edge(1).name()
	elem_type := b.struct_field_array_elem_type(base_type, rhs_name)
	if elem_type != 0 {
		b.array_value_elem_types[field_val] = elem_type
		return
	}
	if checked_field_type := b.checked_struct_field_type_from_ssa(base_type, rhs_name) {
		checked_elem_type := b.unwrap_to_array_elem_ssa(checked_field_type)
		if checked_elem_type != 0 {
			b.array_value_elem_types[field_val] = checked_elem_type
			return
		}
	}
	cursor_elem_type := b.array_elem_type_from_selector_cursor(c)
	if cursor_elem_type != 0 {
		b.array_value_elem_types[field_val] = cursor_elem_type
		return
	}
	if checked_expr_type := b.get_checked_expr_type_from_flat(c) {
		expr_elem_type := b.unwrap_to_array_elem_ssa(checked_expr_type)
		if expr_elem_type != 0 {
			b.array_value_elem_types[field_val] = expr_elem_type
		}
	}
}

// build_cast_from_flat (s192) decodes both edges via `decode_expr`. CastExpr
// flat encoding (`flat.v:1876`) is `(.expr_cast, pos, -1, -1, 0, 0,
// [edge0=typ, edge1=expr])`. Both edges need full decode: `build_cast` calls
// `b.ast_type_to_ssa(expr.typ)` (which pattern-matches on type expr kinds —
// Ident, ArrayType, MapType, FnType, PointerType, ParenExpr, etc.) and
// `b.build_addr(expr.expr)` / `b.build_expr(expr.expr)` on the value side.
// Saves only the outer `Expr(CastExpr{...})` sum-type box.
// build_cast_from_flat (s210) mirrors `build_cast` cursor-natively. CastExpr
// flat encoding is `(.expr_cast, pos, -1, -1, 0, 0, [edge0=typ, edge1=expr])`.
// `build_cast` does only three things AST-dependent: `ast_type_to_ssa(typ)`,
// `build_addr(expr)`, and `build_expr(expr)` — all three have cursor-native
// equivalents (`ast_type_to_ssa_from_flat`, `build_addr_from_flat`,
// `build_expr_from_flat`). `wrap_address_for_sumtype_target` and
// `build_cast_value_to_type` are pure SSA helpers — no AST traversal. No
// `decode_expr` needed in the rewrite.
fn (mut b Builder) build_cast_from_flat(c ast.Cursor) ValueID {
	typ_c := c.edge(0)
	val_c := c.edge(1)
	if b.type_cursor_is_ierror_interface(typ_c) {
		if tag := b.ierror_tag_for_cursor(val_c) {
			return tag
		}
	}
	target_type := b.ast_type_to_ssa_from_flat(typ_c)
	addr := b.build_addr_from_flat(val_c)
	if addr != 0 {
		if wrapped := b.wrap_address_for_sumtype_target(addr, target_type) {
			return wrapped
		}
		if b.is_mut_ptr_param_cursor(val_c) {
			if ptr_cast := b.cast_forwarded_mut_param_addr_to_pointer(addr, target_type) {
				return ptr_cast
			}
		}
	}
	val := b.build_expr_from_flat(val_c)
	return b.build_cast_value_to_type(val, target_type)
}

fn (b &Builder) const_int_value_id(val ValueID) ?i64 {
	if val <= 0 || val >= b.mod.values.len {
		return none
	}
	value := b.mod.values[val]
	if value.kind != .constant || value.name == '' {
		return none
	}
	first := value.name[0]
	if first != `-` && (first < `0` || first > `9`) {
		return none
	}
	return parse_const_int_literal(value.name)
}

fn (mut b Builder) build_sumtype_tag_compare(sum_val ValueID, tag_const_val ValueID, op token.Token) ?ValueID {
	if op !in [.eq, .ne] || !b.valid_value_id(sum_val) || !b.valid_value_id(tag_const_val) {
		return none
	}
	sum_type := b.mod.values[sum_val].typ
	if !b.ssa_type_is_sumtype(sum_type) {
		return none
	}
	tag := b.const_int_value_id(tag_const_val) or { return none }
	sum_info := b.mod.type_store.types[sum_type]
	i64_t := b.mod.type_store.get_int(64)
	tag_type := if sum_info.fields.len > 0 { sum_info.fields[0] } else { i64_t }
	tag_idx := b.mod.get_or_add_const(b.mod.type_store.get_int(32), '0')
	tag_val := b.mod.add_instr(.extractvalue, b.cur_block, tag_type, [sum_val, tag_idx])
	expected := b.mod.get_or_add_const(tag_type, tag.str())
	bool_type := b.mod.type_store.get_int(1)
	cmp_op := if op == .eq { OpCode.eq } else { OpCode.ne }
	return b.mod.add_instr(cmp_op, b.cur_block, bool_type, [tag_val, expected])
}

fn (mut b Builder) build_sumtype_tag_compare_values(lhs ValueID, rhs ValueID, op token.Token) ?ValueID {
	if cmp := b.build_sumtype_tag_compare(lhs, rhs, op) {
		return cmp
	}
	if cmp := b.build_sumtype_tag_compare(rhs, lhs, op) {
		return cmp
	}
	return none
}

fn (mut b Builder) build_sumtype_variant_compare_expr(sum_val ValueID, variant_expr ast.Expr, op token.Token) ?ValueID {
	if op !in [.eq, .ne] || !b.valid_value_id(sum_val) {
		return none
	}
	sum_type := b.mod.values[sum_val].typ
	if !b.ssa_type_is_sumtype(sum_type) {
		return none
	}
	info := b.match_sumtype_branch_info(sum_type, variant_expr) or { return none }
	sum_info := b.mod.type_store.types[sum_type]
	i64_t := b.mod.type_store.get_int(64)
	tag_type := if sum_info.fields.len > 0 { sum_info.fields[0] } else { i64_t }
	tag_idx := b.mod.get_or_add_const(b.mod.type_store.get_int(32), '0')
	tag_val := b.mod.add_instr(.extractvalue, b.cur_block, tag_type, [sum_val, tag_idx])
	expected := b.mod.get_or_add_const(tag_type, info.tag.str())
	bool_type := b.mod.type_store.get_int(1)
	cmp_op := if op == .eq { OpCode.eq } else { OpCode.ne }
	return b.mod.add_instr(cmp_op, b.cur_block, bool_type, [tag_val, expected])
}

fn (mut b Builder) build_sumtype_variant_compare_expr_from_flat(sum_val ValueID, variant_expr ast.Cursor, op token.Token) ?ValueID {
	if op !in [.eq, .ne] || !b.valid_value_id(sum_val) {
		return none
	}
	sum_type := b.mod.values[sum_val].typ
	if !b.ssa_type_is_sumtype(sum_type) {
		return none
	}
	info := b.match_sumtype_branch_info_from_flat(sum_type, variant_expr) or { return none }
	sum_info := b.mod.type_store.types[sum_type]
	i64_t := b.mod.type_store.get_int(64)
	tag_type := if sum_info.fields.len > 0 { sum_info.fields[0] } else { i64_t }
	tag_idx := b.mod.get_or_add_const(b.mod.type_store.get_int(32), '0')
	tag_val := b.mod.add_instr(.extractvalue, b.cur_block, tag_type, [sum_val, tag_idx])
	expected := b.mod.get_or_add_const(tag_type, info.tag.str())
	bool_type := b.mod.type_store.get_int(1)
	cmp_op := if op == .eq { OpCode.eq } else { OpCode.ne }
	return b.mod.add_instr(cmp_op, b.cur_block, bool_type, [tag_val, expected])
}

fn (mut b Builder) build_sumtype_tag_selector_variant_compare(tag_val ValueID, tag_expr ast.Expr, variant_expr ast.Expr, op token.Token) ?ValueID {
	if op !in [.eq, .ne] || !b.valid_value_id(tag_val) {
		return none
	}
	if tag_expr !is ast.SelectorExpr {
		return none
	}
	tag_selector := tag_expr as ast.SelectorExpr
	if tag_selector.rhs.name != '_tag' {
		return none
	}
	sumtype_checked := b.get_checked_expr_type(tag_selector.lhs) or { return none }
	sumtype_type := b.type_to_ssa(sumtype_checked)
	if !b.ssa_type_is_sumtype(sumtype_type) {
		return none
	}
	info := b.match_sumtype_branch_info(sumtype_type, variant_expr) or { return none }
	tag_type := b.mod.values[tag_val].typ
	expected := b.mod.get_or_add_const(tag_type, info.tag.str())
	bool_type := b.mod.type_store.get_int(1)
	cmp_op := if op == .eq { OpCode.eq } else { OpCode.ne }
	return b.mod.add_instr(cmp_op, b.cur_block, bool_type, [tag_val, expected])
}

fn (mut b Builder) build_sumtype_tag_selector_variant_compare_from_flat(tag_val ValueID, tag_expr ast.Cursor, variant_expr ast.Cursor, op token.Token) ?ValueID {
	if op !in [.eq, .ne] || !b.valid_value_id(tag_val) || tag_expr.kind() != .expr_selector
		|| tag_expr.edge_count() < 2 {
		return none
	}
	tag_rhs := tag_expr.edge(1)
	if tag_rhs.kind() != .expr_ident || tag_rhs.name() != '_tag' {
		return none
	}
	tag_lhs := tag_expr.edge(0)
	sumtype_checked := b.get_checked_expr_type_from_flat(tag_lhs) or { return none }
	sumtype_type := b.type_to_ssa(sumtype_checked)
	if !b.ssa_type_is_sumtype(sumtype_type) {
		return none
	}
	info := b.match_sumtype_branch_info_from_flat(sumtype_type, variant_expr) or { return none }
	tag_type := b.mod.values[tag_val].typ
	expected := b.mod.get_or_add_const(tag_type, info.tag.str())
	bool_type := b.mod.type_store.get_int(1)
	cmp_op := if op == .eq { OpCode.eq } else { OpCode.ne }
	return b.mod.add_instr(cmp_op, b.cur_block, bool_type, [tag_val, expected])
}

// build_infix_from_flat (s211) mirrors `build_infix` cursor-natively.
// InfixExpr flat encoding (`flat.v:1956`) is
// `(.expr_infix, pos, -1, -1, u16(op), 0, [edge0=lhs, edge1=rhs])`.
// `build_infix` touches the AST only via three calls: recursive
// `b.build_expr(expr.lhs)` / `b.build_expr(expr.rhs)` → cursor-native
// `build_expr_from_flat`; `b.expr_type(ast.Expr(expr))` → cursor-native
// `expr_type_from_flat(c)` (the outer InfixExpr cursor preserves pos.id for
// the env lookup); and `b.is_none_expr(expr.lhs)` / `b.is_none_expr(expr.rhs)`
// → cursor-native `is_none_expr_from_flat`. Everything else is pure SSA
// helpers (string-like-typed dispatch, array_eq dispatch, pointer arithmetic,
// float promotion, narrow-int widening, opcode selection, result-type
// widening). The body below is a verbatim port of `build_infix`'s logic
// with the three AST entry points swapped for their cursor-native
// equivalents — no `decode_expr` remains.
fn (mut b Builder) build_infix_from_flat(c ast.Cursor) ValueID {
	op := unsafe { token.Token(int(c.aux())) }
	lhs_c := c.edge(0)
	rhs_c := c.edge(1)

	// Short-circuit evaluation for logical || and &&
	if op == .logical_or {
		bool_type := b.mod.type_store.get_int(1)
		lhs := b.build_expr_from_flat(lhs_c)
		lhs_block := b.cur_block
		rhs_block := b.mod.add_block(b.cur_func, 'or_rhs')
		merge_block := b.mod.add_block(b.cur_func, 'or_merge')
		b.mod.add_instr(.br, b.cur_block, 0,
			[lhs, b.mod.blocks[merge_block].val_id, b.mod.blocks[rhs_block].val_id])
		b.add_edge(b.cur_block, merge_block)
		b.add_edge(b.cur_block, rhs_block)
		b.cur_block = rhs_block
		rhs := b.build_expr_from_flat(rhs_c)
		rhs_end_block := b.cur_block
		if !b.block_has_terminator(b.cur_block) {
			b.mod.add_instr(.jmp, b.cur_block, 0, [b.mod.blocks[merge_block].val_id])
			b.add_edge(b.cur_block, merge_block)
		}
		b.cur_block = merge_block
		one := b.mod.get_or_add_const(bool_type, '1')
		phi_val := b.mod.add_instr(.phi, merge_block, bool_type, [one, b.mod.blocks[lhs_block].val_id,
			rhs, b.mod.blocks[rhs_end_block].val_id])
		return phi_val
	}
	if op == .and {
		bool_type := b.mod.type_store.get_int(1)
		lhs := b.build_expr_from_flat(lhs_c)
		lhs_block := b.cur_block
		rhs_block := b.mod.add_block(b.cur_func, 'and_rhs')
		merge_block := b.mod.add_block(b.cur_func, 'and_merge')
		b.mod.add_instr(.br, b.cur_block, 0,
			[lhs, b.mod.blocks[rhs_block].val_id, b.mod.blocks[merge_block].val_id])
		b.add_edge(b.cur_block, rhs_block)
		b.add_edge(b.cur_block, merge_block)
		b.cur_block = rhs_block
		rhs := b.build_expr_from_flat(rhs_c)
		rhs_end_block := b.cur_block
		if !b.block_has_terminator(b.cur_block) {
			b.mod.add_instr(.jmp, b.cur_block, 0, [b.mod.blocks[merge_block].val_id])
			b.add_edge(b.cur_block, merge_block)
		}
		b.cur_block = merge_block
		zero := b.mod.get_or_add_const(bool_type, '0')
		phi_val := b.mod.add_instr(.phi, merge_block, bool_type, [zero, b.mod.blocks[lhs_block].val_id,
			rhs, b.mod.blocks[rhs_end_block].val_id])
		return phi_val
	}

	lhs := b.build_expr_from_flat(lhs_c)
	if tag_cmp := b.build_ierror_concrete_compare_from_flat(lhs, lhs_c, rhs_c, op) {
		return tag_cmp
	}
	if tag_cmp := b.build_sumtype_tag_selector_variant_compare_from_flat(lhs, lhs_c, rhs_c, op) {
		return tag_cmp
	}
	if tag_cmp := b.build_sumtype_variant_compare_expr_from_flat(lhs, rhs_c, op) {
		return tag_cmp
	}
	rhs := b.build_expr_from_flat(rhs_c)
	if tag_cmp := b.build_sumtype_variant_compare_expr_from_flat(rhs, lhs_c, op) {
		return tag_cmp
	}
	result_type := b.expr_type_from_flat(c)
	if tag_cmp := b.build_sumtype_tag_compare_values(lhs, rhs, op) {
		return tag_cmp
	}

	// Option/Result comparison with none: x == none / x != none
	if (op == .eq || op == .ne)
		&& (b.is_none_expr_from_flat(rhs_c) || b.is_none_expr_from_flat(lhs_c)) {
		option_val := if b.is_none_expr_from_flat(rhs_c) { lhs } else { rhs }
		option_type := b.mod.values[option_val].typ
		if b.is_option_wrapper_type(option_type) {
			i32_t := b.mod.type_store.get_int(32)
			bool_t := b.mod.type_store.get_int(1)
			flag_idx := b.mod.get_or_add_const(i32_t, '0')
			flag_type := b.mod.type_store.types[option_type].fields[0]
			flag_val := b.mod.add_instr(.extractvalue, b.cur_block, flag_type, [
				option_val,
				flag_idx,
			])
			zero_flag := b.mod.get_or_add_const(flag_type, '0')
			if op == .eq {
				return b.mod.add_instr(.ne, b.cur_block, bool_t, [flag_val, zero_flag])
			} else {
				return b.mod.add_instr(.eq, b.cur_block, bool_t, [flag_val, zero_flag])
			}
		}
		if b.is_result_wrapper_type(option_type) {
			i32_t := b.mod.type_store.get_int(32)
			bool_t := b.mod.type_store.get_int(1)
			flag_idx := b.mod.get_or_add_const(i32_t, '0')
			flag_type := b.mod.type_store.types[option_type].fields[0]
			flag_val := b.mod.add_instr(.extractvalue, b.cur_block, flag_type, [
				option_val,
				flag_idx,
			])
			zero_flag := b.mod.get_or_add_const(flag_type, '0')
			if op == .eq {
				return b.mod.add_instr(.ne, b.cur_block, bool_t, [flag_val, zero_flag])
			} else {
				return b.mod.add_instr(.eq, b.cur_block, bool_t, [flag_val, zero_flag])
			}
		}
	}
	// String-like-typed dispatch
	str_type := b.get_string_type()
	if str_type != 0 {
		lhs_type := b.mod.values[lhs].typ
		rhs_type := b.mod.values[rhs].typ
		mut lhs_is_string := lhs_type == str_type
		if !lhs_is_string && lhs_type > 0 && int(lhs_type) < b.mod.type_store.types.len {
			lhs_typ := b.mod.type_store.types[lhs_type]
			lhs_is_string = lhs_typ.kind == .ptr_t && lhs_typ.elem_type == str_type
		}
		mut rhs_is_string := rhs_type == str_type
		if !rhs_is_string && rhs_type > 0 && int(rhs_type) < b.mod.type_store.types.len {
			rhs_typ := b.mod.type_store.types[rhs_type]
			rhs_is_string = rhs_typ.kind == .ptr_t && rhs_typ.elem_type == str_type
		}
		if lhs_is_string || rhs_is_string {
			string_lhs := b.load_string_like_value(lhs)
			string_rhs := b.load_string_like_value(rhs)
			if op in [.eq, .ne] {
				bool_type := b.mod.type_store.get_int(1)
				fn_ref := b.get_or_create_fn_ref('builtin__string__==', bool_type)
				eq_result := b.mod.add_instr(.call, b.cur_block, bool_type, [fn_ref, string_lhs,
					string_rhs])
				if op == .ne {
					return b.mod.add_instr(.xor, b.cur_block, bool_type, [eq_result,
						b.mod.get_or_add_const(bool_type, '1')])
				}
				return eq_result
			}
			if op in [.lt, .gt, .le, .ge] {
				bool_type := b.mod.type_store.get_int(1)
				fn_ref := b.get_or_create_fn_ref('builtin__string__<', bool_type)
				lt_lhs := if op in [.gt, .le] { string_rhs } else { string_lhs }
				lt_rhs := if op in [.gt, .le] { string_lhs } else { string_rhs }
				lt_result := b.mod.add_instr(.call, b.cur_block, bool_type,
					[fn_ref, lt_lhs, lt_rhs])
				if op in [.le, .ge] {
					return b.mod.add_instr(.xor, b.cur_block, bool_type, [lt_result,
						b.mod.get_or_add_const(bool_type, '1')])
				}
				return lt_result
			}
			if op == .plus {
				fn_ref := b.get_or_create_fn_ref('builtin__string__+', str_type)
				return b.mod.add_instr(.call, b.cur_block, str_type,
					[fn_ref, string_lhs, string_rhs])
			}
		}
	}

	// Array comparison
	array_type := b.get_array_type()
	if array_type != 0 && (op == .eq || op == .ne) {
		lhs_type := b.mod.values[lhs].typ
		rhs_type := b.mod.values[rhs].typ
		if lhs_type == array_type || rhs_type == array_type {
			bool_type := b.mod.type_store.get_int(1)
			fn_ref := b.get_or_create_fn_ref('array__eq', bool_type)
			eq_result := b.mod.add_instr(.call, b.cur_block, bool_type, [fn_ref, lhs, rhs])
			if op == .ne {
				return b.mod.add_instr(.xor, b.cur_block, bool_type, [eq_result,
					b.mod.get_or_add_const(bool_type, '1')])
			}
			return eq_result
		}
	}

	// Pointer arithmetic: ptr + int or int + ptr → GEP
	if op == .plus || op == .minus {
		lhs_t := b.mod.values[lhs].typ
		rhs_t := b.mod.values[rhs].typ
		mut ptr_val := ValueID(0)
		mut int_val := ValueID(0)
		mut is_ptr_arith := false
		if lhs_t > 0 && int(lhs_t) < b.mod.type_store.types.len
			&& b.mod.type_store.types[lhs_t].kind == .ptr_t {
			ptr_val = lhs
			int_val = rhs
			is_ptr_arith = true
		} else if rhs_t > 0 && int(rhs_t) < b.mod.type_store.types.len
			&& b.mod.type_store.types[rhs_t].kind == .ptr_t {
			ptr_val = rhs
			int_val = lhs
			is_ptr_arith = true
		}
		if is_ptr_arith {
			ptr_type := b.mod.values[ptr_val].typ
			if op == .minus {
				int_val = b.mod.add_instr(.sub, b.cur_block, b.mod.values[int_val].typ, [
					b.mod.get_or_add_const(b.mod.values[int_val].typ, '0'),
					int_val,
				])
			}
			return b.mod.add_instr(.get_element_ptr, b.cur_block, ptr_type, [
				ptr_val,
				int_val,
			])
		}
	}

	// Float / int promotion + opcode selection
	mut lhs_v := lhs
	mut rhs_v := rhs
	lhs_type := b.mod.values[lhs_v].typ
	rhs_type_id := b.mod.values[rhs_v].typ
	lhs_is_float := lhs_type > 0 && int(lhs_type) < b.mod.type_store.types.len
		&& b.mod.type_store.types[lhs_type].kind == .float_t
	rhs_is_float := rhs_type_id > 0 && int(rhs_type_id) < b.mod.type_store.types.len
		&& b.mod.type_store.types[rhs_type_id].kind == .float_t
	is_float := lhs_is_float || rhs_is_float
	if is_float {
		if lhs_is_float && !rhs_is_float {
			rhs_unsigned := rhs_type_id > 0 && int(rhs_type_id) < b.mod.type_store.types.len
				&& b.mod.type_store.types[rhs_type_id].is_unsigned
			conv_op := if rhs_unsigned { OpCode.uitofp } else { OpCode.sitofp }
			rhs_v = b.mod.add_instr(conv_op, b.cur_block, lhs_type, [rhs_v])
		} else if rhs_is_float && !lhs_is_float {
			lhs_unsigned := lhs_type > 0 && int(lhs_type) < b.mod.type_store.types.len
				&& b.mod.type_store.types[lhs_type].is_unsigned
			conv_op := if lhs_unsigned { OpCode.uitofp } else { OpCode.sitofp }
			lhs_v = b.mod.add_instr(conv_op, b.cur_block, rhs_type_id, [lhs_v])
		}
	}

	mut forced_result_type := TypeID(0)
	if !is_float && op == .minus {
		lhs_t := b.mod.values[lhs_v].typ
		rhs_t := b.mod.values[rhs_v].typ
		if lhs_t > 0 && rhs_t > 0 && int(lhs_t) < b.mod.type_store.types.len
			&& int(rhs_t) < b.mod.type_store.types.len {
			lhs_info := b.mod.type_store.types[lhs_t]
			rhs_info := b.mod.type_store.types[rhs_t]
			if lhs_info.kind == .int_t && rhs_info.kind == .int_t && lhs_info.width < 32
				&& rhs_info.width < 32 {
				int_t := b.mod.type_store.get_int(32)
				if lhs_t != int_t {
					lhs_v = b.mod.add_instr(if lhs_info.is_unsigned { .zext } else { .sext },
						b.cur_block, int_t, [lhs_v])
				}
				if rhs_t != int_t {
					rhs_v = b.mod.add_instr(if rhs_info.is_unsigned { .zext } else { .sext },
						b.cur_block, int_t, [rhs_v])
				}
				forced_result_type = int_t
			}
		}
	}

	opcode := if is_float {
		match op {
			.plus { OpCode.fadd }
			.minus { OpCode.fsub }
			.mul { OpCode.fmul }
			.div { OpCode.fdiv }
			.mod { OpCode.frem }
			.eq { OpCode.eq }
			.ne { OpCode.ne }
			.lt { OpCode.lt }
			.gt { OpCode.gt }
			.le { OpCode.le }
			.ge { OpCode.ge }
			else { OpCode.fadd }
		}
	} else {
		is_unsigned := lhs_type > 0 && int(lhs_type) < b.mod.type_store.types.len
			&& b.mod.type_store.types[lhs_type].is_unsigned
		match op {
			.plus {
				OpCode.add
			}
			.minus {
				OpCode.sub
			}
			.mul {
				OpCode.mul
			}
			.div {
				if is_unsigned {
					OpCode.udiv
				} else {
					OpCode.sdiv
				}
			}
			.mod {
				if is_unsigned {
					OpCode.urem
				} else {
					OpCode.srem
				}
			}
			.eq {
				OpCode.eq
			}
			.ne {
				OpCode.ne
			}
			.lt {
				if is_unsigned {
					OpCode.ult
				} else {
					OpCode.lt
				}
			}
			.gt {
				if is_unsigned {
					OpCode.ugt
				} else {
					OpCode.gt
				}
			}
			.le {
				if is_unsigned {
					OpCode.ule
				} else {
					OpCode.le
				}
			}
			.ge {
				if is_unsigned {
					OpCode.uge
				} else {
					OpCode.ge
				}
			}
			.amp {
				OpCode.and_
			}
			.pipe {
				OpCode.or_
			}
			.xor {
				OpCode.xor
			}
			.left_shift {
				OpCode.shl
			}
			.right_shift {
				if is_unsigned {
					OpCode.lshr
				} else {
					OpCode.ashr
				}
			}
			else {
				OpCode.add
			}
		}
	}

	mut final_type := result_type
	if forced_result_type != 0 {
		final_type = forced_result_type
	}
	if final_type > 0 && int(final_type) < b.mod.type_store.types.len {
		kind := b.mod.type_store.types[final_type].kind
		if kind == .struct_t || kind == .array_t {
			final_type = b.mod.values[lhs_v].typ
		}
	}
	if final_type > 0 && int(final_type) < b.mod.type_store.types.len {
		ft := b.mod.type_store.types[final_type]
		if ft.kind == .int_t {
			lhs_t := b.mod.values[lhs_v].typ
			rhs_t := b.mod.values[rhs_v].typ
			if lhs_t > 0 && int(lhs_t) < b.mod.type_store.types.len {
				lt := b.mod.type_store.types[lhs_t]
				if lt.kind == .int_t && lt.width > ft.width {
					final_type = lhs_t
				}
			}
			if rhs_t > 0 && int(rhs_t) < b.mod.type_store.types.len {
				rt := b.mod.type_store.types[rhs_t]
				if rt.kind == .int_t && rt.width > ft.width {
					final_type = rhs_t
				}
			}
		}
	}
	return b.mod.add_instr(opcode, b.cur_block, final_type, [lhs_v, rhs_v])
}

// build_index_from_flat (s212) mirrors `build_index` cursor-natively. IndexExpr
// flat encoding (`flat.v:1946`) is `(.expr_index, pos, -1, -1, 0, flags,
// [edge0=lhs, edge1=expr])` where `flags` carries `is_gated` (intentionally
// ignored — the SSA pipeline doesn't differentiate gated lookups; the
// transformer lowers them earlier). `build_index` touches the AST only via
// `b.build_expr(expr.lhs)` / `b.build_expr(expr.expr)`, `b.expr_type(...)`
// for the pos.id env lookup, and `b.infer_dynamic_array_index_type` — all
// have cursor-native equivalents (`build_expr_from_flat`, `expr_type_from_flat`,
// `infer_dynamic_array_index_type_from_flat`). The rest is pure SSA work
// (dynamic-array deref-load, .data extractvalue + bitcast + GEP + load,
// string-byte indexing via .str field, fixed-size array trace-back through
// load/extractvalue, ptr-base GEP+load). No `decode_expr` remains.
fn (mut b Builder) build_index_from_flat(c ast.Cursor) ValueID {
	lhs_c := c.edge(0)
	idx_c := c.edge(1)
	mut base_val := b.build_expr_from_flat(lhs_c)
	index := b.build_expr_from_flat(idx_c)
	mut result_type := b.expr_type_from_flat(c)

	base_type_id := b.mod.values[base_val].typ
	array_type := b.get_array_type()

	if array_type != 0 && base_type_id != array_type {
		if base_type_id < b.mod.type_store.types.len {
			base_typ := b.mod.type_store.types[base_type_id]
			if base_typ.kind == .ptr_t && base_typ.elem_type == array_type {
				base_val = b.mod.add_instr(.load, b.cur_block, array_type, [base_val])
			}
		}
	}

	base_type_id2 := b.mod.values[base_val].typ

	if array_type != 0 && base_type_id2 == array_type {
		result_type = b.infer_dynamic_array_index_type_from_flat(c, base_val, result_type)
		i8_t := b.mod.type_store.get_int(8)
		void_ptr := b.mod.type_store.get_ptr(i8_t)
		data_ptr := b.mod.add_instr(.extractvalue, b.cur_block, void_ptr, [base_val,
			b.mod.get_or_add_const(b.mod.type_store.get_int(32), '0')])
		elem_ptr_type := b.mod.type_store.get_ptr(result_type)
		typed_ptr := b.mod.add_instr(.bitcast, b.cur_block, elem_ptr_type, [data_ptr])
		elem_addr := b.mod.add_instr(.get_element_ptr, b.cur_block, elem_ptr_type, [
			typed_ptr,
			index,
		])
		result := b.mod.add_instr(.load, b.cur_block, result_type, [elem_addr])
		return b.finish_index_value_from_flat(c, result)
	}

	str_type := b.get_string_type()
	if str_type != 0 && base_type_id2 == str_type {
		u8_t := b.mod.type_store.get_uint(8)
		u8_ptr := b.mod.type_store.get_ptr(u8_t)
		data_ptr := b.mod.add_instr(.extractvalue, b.cur_block, u8_ptr, [base_val,
			b.mod.get_or_add_const(b.mod.type_store.get_int(32), '0')])
		elem_addr := b.mod.add_instr(.get_element_ptr, b.cur_block, u8_ptr, [
			data_ptr,
			index,
		])
		result := b.mod.add_instr(.load, b.cur_block, u8_t, [elem_addr])
		return b.finish_index_value_from_flat(c, result)
	}

	if base_type_id > 0 && base_type_id < b.mod.type_store.types.len {
		base_typ := b.mod.type_store.types[base_type_id]
		if base_typ.kind == .array_t && base_typ.elem_type != 0 {
			base_value := b.mod.values[base_val]
			if base_value.kind == .instruction {
				instr := b.mod.instrs[base_value.index]
				if instr.op == .load && instr.operands.len > 0 {
					alloca_ptr := instr.operands[0]
					elem_type := base_typ.elem_type
					elem_ptr_type := b.mod.type_store.get_ptr(elem_type)
					elem_addr := b.mod.add_instr(.get_element_ptr, b.cur_block, elem_ptr_type, [
						alloca_ptr,
						index,
					])
					result := b.mod.add_instr(.load, b.cur_block, elem_type, [
						elem_addr,
					])
					return b.finish_index_value_from_flat(c, result)
				}
				if instr.op == .extractvalue && instr.operands.len >= 2 {
					struct_val := instr.operands[0]
					field_idx_val := instr.operands[1]
					struct_value := b.mod.values[struct_val]
					if struct_value.kind == .instruction {
						struct_instr := b.mod.instrs[struct_value.index]
						if struct_instr.op == .load && struct_instr.operands.len > 0 {
							struct_ptr := struct_instr.operands[0]
							arr_ptr_type := b.mod.type_store.get_ptr(base_type_id)
							field_addr := b.mod.add_instr(.get_element_ptr, b.cur_block,
								arr_ptr_type, [struct_ptr, field_idx_val])
							elem_type := base_typ.elem_type
							elem_ptr_type := b.mod.type_store.get_ptr(elem_type)
							elem_addr := b.mod.add_instr(.get_element_ptr, b.cur_block,
								elem_ptr_type, [field_addr, index])
							result := b.mod.add_instr(.load, b.cur_block, elem_type, [
								elem_addr,
							])
							return b.finish_index_value_from_flat(c, result)
						}
					}
				}
			}
		}
	}

	if base_type_id < b.mod.type_store.types.len {
		base_typ := b.mod.type_store.types[base_type_id]
		if base_typ.kind == .ptr_t && base_typ.elem_type != 0 {
			mut elem_type := base_typ.elem_type
			if elem_type < b.mod.type_store.types.len {
				inner_typ := b.mod.type_store.types[elem_type]
				if inner_typ.kind == .array_t && inner_typ.elem_type != 0 {
					elem_type = inner_typ.elem_type
				}
			}
			elem_ptr_type := b.mod.type_store.get_ptr(elem_type)
			elem_addr := b.mod.add_instr(.get_element_ptr, b.cur_block, elem_ptr_type, [
				base_val,
				index,
			])
			result := b.mod.add_instr(.load, b.cur_block, elem_type, [elem_addr])
			return b.finish_index_value_from_flat(c, result)
		}
	}

	result := b.mod.add_instr(.get_element_ptr, b.cur_block, result_type, [base_val, index])
	return b.finish_index_value_from_flat(c, result)
}

// build_if_from_flat (s195, s231) dispatches the .expr_if arm of
// build_expr_from_flat to the cursor-native build_if_expr_from_flat. IfExpr flat
// encoding (`flat.v:1951`) is `(.expr_if, pos, [edge0=cond, edge1=else_expr,
// edge2..n=stmts])`. As of s231 the cond, branch stmts, else-chain and phi are
// all built from cursors; the only residual decode is the infer_if_expr_type
// fallback inside build_if_expr_from_flat (slated for s232).
fn (mut b Builder) build_if_from_flat(c ast.Cursor) ValueID {
	return b.build_if_expr_from_flat(c)
}

// lhs_name_from_flat (s214, s217) mirrors `expr.lhs.name()` cursor-natively
// for the kinds `build_call`'s struct-cast fast path ever queries with a hit:
// `.expr_ident` (interned name) and `.expr_selector` (recursive `lhs.rhs` walk
// with '.' separators — matches `SelectorExpr.name()`). All other kinds
// return ''; the caller treats '' as "no name", which matches the legacy
// semantics for the cast-shortcut (CallExpr-as-name like `add()` produces a
// string that will never be in `struct_types`). The selector walk only
// descends through nested Ident/Selector chains — any other inner expr kind
// short-circuits to '' (deferred to a future port — those cases fall through
// to the legacy decode path).
fn (b &Builder) lhs_name_from_flat(c ast.Cursor) string {
	match c.kind() {
		.expr_ident {
			return c.name()
		}
		.expr_selector {
			lhs_part := b.lhs_name_from_flat(c.edge(0))
			if lhs_part == '' {
				return ''
			}
			rhs := c.edge(1)
			if rhs.kind() != .expr_ident {
				return ''
			}
			return '${lhs_part}.${rhs.name()}'
		}
		else {
			return ''
		}
	}
}

// build_cast_expr_to_type_id_from_flat (s214) is the cursor mirror of
// `build_cast_expr_to_type_id`. Both helpers do the same SSA work — try the
// addressable + sumtype-wrap path first, then fall back to value-build +
// type-coerce. The cursor variant uses `build_addr_from_flat` and
// `build_expr_from_flat`, both already bit-identical to their AST counterparts
// (verified across s194/s210), so this is a pure dispatch swap.
fn (mut b Builder) build_cast_expr_to_type_id_from_flat(c ast.Cursor, target_type TypeID) ValueID {
	addr := b.build_addr_from_flat(c)
	if addr != 0 {
		if wrapped := b.wrap_address_for_sumtype_target(addr, target_type) {
			return wrapped
		}
		if b.is_mut_ptr_param_cursor(c) {
			if ptr_cast := b.cast_forwarded_mut_param_addr_to_pointer(addr, target_type) {
				return ptr_cast
			}
		}
	}
	val := b.build_expr_from_flat(c)
	return b.build_cast_value_to_type(val, target_type)
}

// call_lhs_type_to_ssa_from_flat (s215, s217) is the cursor mirror of
// `call_lhs_type_to_ssa`. Handles `.expr_ident` (raw name) and `.expr_selector`
// (recursive walk → `lhs.rhs[.rhs2]` with dots replaced by `__` to match the
// legacy `lhs.name().replace('.', '__')` semantics). Other kinds return
// `none`. Covers two cases:
//   - `MyType(value)` where `MyType` is in env's type scope but not in
//     `b.struct_types` (generic instantiation, alias type).
//   - `mod.Type(value)` where the qualified `mod__Type` is in env's type
//     scope but not in `b.struct_types`.
fn (mut b Builder) call_lhs_type_to_ssa_from_flat(c ast.Cursor) ?TypeID {
	mut name := ''
	match c.kind() {
		.expr_ident {
			name = c.name()
		}
		.expr_selector {
			dotted := b.lhs_name_from_flat(c)
			if dotted == '' {
				return none
			}
			name = dotted.replace('.', '__')
		}
		else {
			return none
		}
	}

	if name == '' {
		return none
	}
	if typ := b.lookup_checked_type_by_name(name) {
		type_id := b.type_to_ssa(typ)
		if type_id > 0 {
			return type_id
		}
	}
	if b.cur_module != '' && !name.contains('__') {
		if typ := b.lookup_checked_type_by_name('${b.cur_module}__${name}') {
			type_id := b.type_to_ssa(typ)
			if type_id > 0 {
				return type_id
			}
		}
	}
	return none
}

// build_call_from_flat (s196, s214) cursor-native fast path for the
// single-arg struct-cast disguised as a call (e.g. `IError(ptr)`,
// `Point(x)`). When `edges == 2` and edge(0) is an Ident whose name resolves
// to a `struct_types` entry but not a `fn_index` entry, dispatch to the
// cursor-native `build_cast_expr_to_type_id_from_flat` directly — no lhs
// decode, no full args decode loop. All other call shapes fall through to
// the legacy `build_call(ast.CallExpr{...})` path, which still decodes lhs
// and every arg via `decode_expr` (pending future per-kind ports — see
// `resolve_call_name`'s heavy pattern-match on `.lhs is ast.SelectorExpr` /
// `.lhs is ast.Ident`).
fn (mut b Builder) build_call_from_flat(c ast.Cursor) ValueID {
	lhs_c := c.edge(0)
	n_edges := c.edge_count()

	// Fast path: single-arg struct-cast disguised as call. Avoids lhs decode.
	// Mirrors the two struct-cast shortcuts in `build_call`: the top-of-fn
	// `struct_types` check + the post-resolve_call_name `call_lhs_type_to_ssa`
	// check (s215). s216 also handles the C__-prefix-strip edge case:
	// `resolve_call_name` for `C__Foo` (when not in fn_index) returns `Foo`
	// without re-checking fn_index, so the legacy second cast shortcut can fire
	// on the stripped name. Cursor-native `resolve_call_name_ident_from_flat`
	// runs the same Ident-arm string cascade as the AST path (both delegate to
	// `resolve_call_name_for_ident_name`), so checking `struct_types[fn_name]`
	// here is bit-identical to the legacy post-resolve path.
	if n_edges == 2 {
		lhs_name := b.lhs_name_from_flat(lhs_c)
		if lhs_name != '' && lhs_name !in b.fn_index {
			if target_type := b.struct_types[lhs_name] {
				return b.build_cast_expr_to_type_id_from_flat(c.edge(1), target_type)
			}
			qualified_lhs := '${b.cur_module}__${lhs_name}'
			if qualified_lhs !in b.fn_index {
				if target_type := b.struct_types[qualified_lhs] {
					return b.build_cast_expr_to_type_id_from_flat(c.edge(1), target_type)
				}
			}
			// s250: do NOT run the `call_lhs_type_to_ssa_from_flat` cast here. This
			// fast-path is gated on the RAW `lhs_name !in fn_index`, but a builtin
			// method like `string__f64` is registered as `builtin__string__f64`, so
			// the raw name is absent from fn_index even though the call resolves to a
			// real function. Legacy `build_call` gates its s215 `call_lhs_type_to_ssa`
			// cast on the RESOLVED fn_name; `build_call_resolved_from_flat` does the
			// same (line ~11712). Casting on the raw name here bitcast `string__f64(s)`
			// to f64 → garbage floats. Let such calls fall through to the resolved
			// path; genuine casts (`f64(x)`, resolved name still not in fn_index) cast
			// there correctly.
			fn_name := b.resolve_call_name_ident_from_flat(lhs_c)
			if fn_name != lhs_name && fn_name != '' && fn_name !in b.fn_index {
				if target_type := b.struct_types[fn_name] {
					return b.build_cast_expr_to_type_id_from_flat(c.edge(1), target_type)
				}
				qualified_fn := '${b.cur_module}__${fn_name}'
				if target_type := b.struct_types[qualified_fn] {
					return b.build_cast_expr_to_type_id_from_flat(c.edge(1), target_type)
				}
			}
		}
	}

	// s218/s223: resolve fn_name cursor-natively for both Ident and
	// SelectorExpr lhs kinds (s216 + s223 share their helpers/cascade with the
	// AST path), so dispatch always avoids `resolve_call_name(expr)`.
	fn_name := b.resolve_call_name_from_flat(lhs_c)

	// s224: cast-shortcut #2 cursor-native check for SelectorExpr-lhs single-arg
	// calls. Ident-lhs cases were already covered by the fast-path block above
	// (s216 — runs `resolve_call_name_ident_from_flat` + struct_types lookup).
	// SelectorExpr-lhs `mod.Foo(x)` (where Foo is a struct in module `mod`)
	// resolves to fn_name=`mod__Foo`; if `struct_types['mod__Foo']` hits and
	// fn_name is not in fn_index, dispatch to cursor-native cast directly,
	// skipping the lhs+args decode. Mirrors `build_call_resolved`'s second
	// cast shortcut bit-identically: `if fn_name !in b.fn_index && args.len == 1
	// && struct_types[fn_name]` → cast.
	if n_edges == 2 && lhs_c.kind() == .expr_selector && fn_name != '' && fn_name !in b.fn_index {
		if target_type := b.struct_types[fn_name] {
			return b.build_cast_expr_to_type_id_from_flat(c.edge(1), target_type)
		}
		qualified_fn := '${b.cur_module}__${fn_name}'
		if target_type := b.struct_types[qualified_fn] {
			return b.build_cast_expr_to_type_id_from_flat(c.edge(1), target_type)
		}
	}

	// s225: dispatch to the cursor-native body. build_call_resolved_from_flat
	// decodes the lhs once (for the SelectorExpr-metadata + receiver path) but
	// builds every value argument from its cursor, so the per-arg decode_expr
	// loop that lived here is gone. Bit-identical to the previous
	// build_call_resolved(fn_name, ast.CallExpr{lhs, args, pos}) dispatch.
	return b.build_call_resolved_from_flat(fn_name, c)
}

// build_init_from_flat (s213) mirrors `build_init_expr` cursor-natively.
// InitExpr flat encoding (`flat.v:1962`) is
// `(.expr_init, pos, -1, -1, 0, 0, [edge0=typ, edge1..n=field_init])`.
// `build_init_expr` just delegates to `collect_init_expr_values` and emits
// `.struct_init`; the cursor-native rewrite calls
// `collect_init_expr_values_from_flat` (already in place since s208) and
// emits the same instruction. No `decode_expr` remains.
fn (mut b Builder) build_init_from_flat(c ast.Cursor) ValueID {
	struct_type, field_vals := b.collect_init_expr_values_from_flat(c)
	if field_vals.len == 0 {
		return b.mod.get_or_add_const(struct_type, '0')
	}
	return b.mod.add_instr(.struct_init, b.cur_block, struct_type, field_vals)
}

fn (mut b Builder) build_unsafe_from_flat(c ast.Cursor) ValueID {
	n := c.edge_count()
	if n > 0 {
		for i := 0; i < n - 1; i++ {
			b.build_stmt_from_flat(c.edge(i))
		}
		last := c.edge(n - 1)
		if last.kind() == .stmt_expr {
			return b.build_expr_from_flat(last.edge(0))
		}
		b.build_stmt_from_flat(last)
	}
	return b.mod.get_or_add_const(b.mod.type_store.get_int(32), '0')
}

fn (mut b Builder) build_keyword_from_flat(c ast.Cursor) ValueID {
	tok := unsafe { token.Token(int(c.aux())) }
	return b.build_keyword(ast.Keyword{
		tok: tok
	})
}

fn (mut b Builder) build_keyword_operator_from_flat(c ast.Cursor) ValueID {
	op := unsafe { token.Token(int(c.aux())) }
	match op {
		.key_sizeof {
			if c.edge_count() > 0 {
				size := b.sizeof_value_from_flat(c.edge(0))
				if size > 0 {
					return b.mod.get_or_add_const(b.mod.type_store.get_int(32), size.str())
				}
			}
			return b.mod.get_or_add_const(b.mod.type_store.get_int(32), '4')
		}
		.key_go {
			if c.edge_count() > 0 {
				return b.build_go_or_spawn_from_flat(c.edge(0), .go_call)
			}
			return b.mod.get_or_add_const(b.mod.type_store.get_int(64), '0')
		}
		.key_spawn {
			if c.edge_count() > 0 {
				return b.build_go_or_spawn_from_flat(c.edge(0), .spawn_call)
			}
			return b.mod.get_or_add_const(b.mod.type_store.get_int(64), '0')
		}
		.key_dump {
			if c.edge_count() > 0 {
				return b.build_expr_from_flat(c.edge(0))
			}
			return b.mod.get_or_add_const(b.mod.type_store.get_int(64), '0')
		}
		else {
			return b.mod.get_or_add_const(b.mod.type_store.get_int(64), '0')
		}
	}
}

// build_postfix_from_flat lowers a `.expr_postfix` cursor without rehydrating
// the inner operand. PostfixExpr flat encoding stores `op` in `aux` and the
// operand at `edge(0)`. The cursor-native path mirrors `build_postfix`:
// `!`/`?` unwraps go through `build_expr_from_flat` + `build_unwrapped_postfix`
// (only the op is consulted by the latter, so the wrapping PostfixExpr's
// `.expr` field is left empty); ident/selector/index inc-dec paths read the
// var name directly from `c.name()` or take the address via
// `build_addr_from_flat`; the fallthrough returns the operand value via
// `build_expr_from_flat`. No `decode_expr` is needed because every branch
// either reads cursor-level metadata or delegates to existing cursor helpers.
fn (mut b Builder) build_postfix_from_flat(c ast.Cursor) ValueID {
	op := unsafe { token.Token(int(c.aux())) }
	inner_c := c.edge(0)
	if op in [.not, .question] {
		wrapped_val := b.build_expr_from_flat(inner_c)
		return b.build_unwrapped_postfix(ast.PostfixExpr{
			op:   op
			expr: ast.empty_expr
			pos:  c.pos()
		}, wrapped_val)
	}
	if inner_c.kind() == .expr_ident {
		name := inner_c.name()
		if ptr := b.vars[name] {
			ptr_typ := b.mod.values[ptr].typ
			elem_typ := b.mod.type_store.types[ptr_typ].elem_type
			loaded := b.mod.add_instr(.load, b.cur_block, elem_typ, [ptr])
			is_float := elem_typ > 0 && int(elem_typ) < b.mod.type_store.types.len
				&& b.mod.type_store.types[elem_typ].kind == .float_t
			one := if is_float {
				b.mod.get_or_add_const(elem_typ, '1.0')
			} else {
				b.mod.get_or_add_const(elem_typ, '1')
			}
			arith_op := if is_float {
				if op == .inc { OpCode.fadd } else { OpCode.fsub }
			} else {
				if op == .inc { OpCode.add } else { OpCode.sub }
			}
			result := b.mod.add_instr(arith_op, b.cur_block, elem_typ, [loaded, one])
			b.mod.add_instr(.store, b.cur_block, 0, [result, ptr])
			return loaded
		}
	}
	if inner_c.kind() == .expr_selector || inner_c.kind() == .expr_index {
		ptr := b.build_addr_from_flat(inner_c)
		if ptr != 0 {
			ptr_typ := b.mod.values[ptr].typ
			elem_typ := if ptr_typ < b.mod.type_store.types.len {
				b.mod.type_store.types[ptr_typ].elem_type
			} else {
				b.mod.type_store.get_int(32)
			}
			loaded := b.mod.add_instr(.load, b.cur_block, elem_typ, [ptr])
			is_float := elem_typ > 0 && int(elem_typ) < b.mod.type_store.types.len
				&& b.mod.type_store.types[elem_typ].kind == .float_t
			one := if is_float {
				b.mod.get_or_add_const(elem_typ, '1.0')
			} else {
				b.mod.get_or_add_const(elem_typ, '1')
			}
			arith_op := if is_float {
				if op == .inc { OpCode.fadd } else { OpCode.fsub }
			} else {
				if op == .inc { OpCode.add } else { OpCode.sub }
			}
			result := b.mod.add_instr(arith_op, b.cur_block, elem_typ, [loaded, one])
			b.mod.add_instr(.store, b.cur_block, 0, [result, ptr])
			return loaded
		}
	}
	return b.build_expr_from_flat(inner_c)
}

fn (mut b Builder) array_bound_or_zero(val ValueID, int_type TypeID) ValueID {
	zero := b.mod.get_or_add_const(int_type, '0')
	if !b.valid_value_id(val) {
		return zero
	}
	if b.mod.values[val].typ == int_type {
		return val
	}
	if b.mod.values[val].kind == .constant && b.mod.values[val].typ > 0
		&& int(b.mod.values[val].typ) < b.mod.type_store.types.len
		&& int(int_type) < b.mod.type_store.types.len {
		src := b.mod.type_store.types[b.mod.values[val].typ]
		dst := b.mod.type_store.types[int_type]
		if src.kind == .int_t && dst.kind == .int_t {
			return b.mod.get_or_add_const(int_type, b.mod.values[val].name)
		}
	}
	return b.build_cast_value_to_type(val, int_type)
}

fn ssa_number_literal_is_float(value string) bool {
	return value.contains('.')
		|| (!value.starts_with('0x') && !value.starts_with('0X')
		&& (value.contains('e') || value.contains('E')))
}

fn (b &Builder) array_literal_value_type_can_infer_elem(expr ast.Expr, val ValueID) bool {
	if !b.valid_value_id(val) {
		return false
	}
	if expr is ast.BasicLiteral && expr.kind == .number && !ssa_number_literal_is_float(expr.value) {
		return false
	}
	return true
}

fn (b &Builder) array_literal_flat_value_type_can_infer_elem(c ast.Cursor, val ValueID) bool {
	if !b.valid_value_id(val) {
		return false
	}
	if c.kind() == .expr_basic_literal {
		kind := unsafe { token.Token(int(c.aux())) }
		if kind == .number && !ssa_number_literal_is_float(c.name()) {
			return false
		}
	}
	return true
}

fn (mut b Builder) build_dynamic_array_from_element_buffer(elem_type TypeID, elem_vals []ValueID, buffer ValueID) ValueID {
	arr_type := b.get_array_type()
	if arr_type == 0 {
		return b.mod.get_or_add_const(arr_type, '0')
	}
	i32_t := b.mod.type_store.get_int(32)
	len_arg := b.mod.get_or_add_const(i32_t, elem_vals.len.str())
	mut elem_size := b.type_byte_size(elem_type)
	if elem_size <= 0 {
		elem_size = 8
	}
	elem_size_arg := b.mod.get_or_add_const(i32_t, elem_size.str())
	i8_t := b.mod.type_store.get_int(8)
	voidptr_t := b.mod.type_store.get_ptr(i8_t)
	buffer_arg := if b.valid_value_id(buffer) && b.mod.values[buffer].typ == voidptr_t {
		buffer
	} else {
		b.mod.add_instr(.bitcast, b.cur_block, voidptr_t, [buffer])
	}
	fn_name := if 'builtin__new_array_from_c_array_noscan' in b.fn_index {
		'builtin__new_array_from_c_array_noscan'
	} else {
		'new_array_from_c_array_noscan'
	}
	fn_ref := b.get_or_create_fn_ref(fn_name, arr_type)
	call_val := b.mod.add_instr(.call, b.cur_block, arr_type, [
		fn_ref,
		len_arg,
		len_arg,
		elem_size_arg,
		buffer_arg,
	])
	b.array_value_elem_types[call_val] = elem_type
	return call_val
}

fn (mut b Builder) build_empty_dynamic_array(elem_type_in TypeID, len_val ValueID, cap_val ValueID) ValueID {
	arr_type := b.get_array_type()
	if arr_type == 0 {
		return b.mod.get_or_add_const(arr_type, '0')
	}
	i32_t := b.mod.type_store.get_int(32)
	elem_type := if b.valid_type_id(elem_type_in) {
		elem_type_in
	} else {
		i32_t
	}
	len_arg := b.array_bound_or_zero(len_val, i32_t)
	cap_arg := if b.valid_value_id(cap_val) {
		b.array_bound_or_zero(cap_val, i32_t)
	} else {
		len_arg
	}
	mut elem_size := b.type_byte_size(elem_type)
	if elem_size <= 0 {
		elem_size = 8
	}
	elem_size_arg := b.mod.get_or_add_const(i32_t, elem_size.str())
	fn_name := if 'builtin____new_array_noscan' in b.fn_index {
		'builtin____new_array_noscan'
	} else {
		'__new_array_noscan'
	}
	fn_ref := b.get_or_create_fn_ref(fn_name, arr_type)
	call_val := b.mod.add_instr(.call, b.cur_block, arr_type, [
		fn_ref,
		len_arg,
		cap_arg,
		elem_size_arg,
	])
	b.array_value_elem_types[call_val] = elem_type
	return call_val
}

fn (mut b Builder) build_array_init_from_flat(c ast.Cursor) ValueID {
	typ_c := c.edge(0)
	init_c := c.edge(1)
	cap_c := c.edge(2)
	len_c := c.edge(3)
	update_c := c.edge(4)
	nedges := c.edge_count()
	array_elem_hint := b.array_literal_elem_type_hint
	b.array_literal_elem_type_hint = 0

	if nedges > 5 {
		mut elem_vals := []ValueID{cap: nedges - 5}
		for i in 5 .. nedges {
			elem_vals << b.build_expr_from_flat(c.edge(i))
		}
		mut elem_type := b.mod.type_store.get_int(32)
		mut has_declared_type := false
		match typ_c.kind() {
			.typ_array {
				declared_elem := b.ast_type_to_ssa_from_flat(typ_c.edge(0))
				if declared_elem > 0 {
					elem_type = declared_elem
					has_declared_type = true
				}
			}
			.typ_array_fixed {
				declared_elem := b.ast_type_to_ssa_from_flat(typ_c.edge(1))
				if declared_elem > 0 {
					elem_type = declared_elem
					has_declared_type = true
				}
			}
			else {}
		}

		if array_elem_hint != 0 {
			elem_type = array_elem_hint
			has_declared_type = true
		}
		if !has_declared_type {
			if checked_type := b.get_checked_expr_type_from_flat(c) {
				checked_base := b.unwrap_alias_type(checked_type)
				match checked_base {
					types.Array {
						checked_elem := b.type_to_ssa(checked_base.elem_type)
						if checked_elem > 0 {
							elem_type = checked_elem
							has_declared_type = true
						}
					}
					types.ArrayFixed {
						checked_elem := b.type_to_ssa(checked_base.elem_type)
						if checked_elem > 0 {
							elem_type = checked_elem
							has_declared_type = true
						}
					}
					else {}
				}
			}
		}
		if !has_declared_type && elem_vals.len > 0 {
			for i, val in elem_vals {
				if b.array_literal_flat_value_type_can_infer_elem(c.edge(i + 5), val) {
					elem_type = b.mod.values[val].typ
					break
				}
			}
		}
		if !b.valid_type_id(elem_type) {
			elem_type = b.mod.type_store.get_int(32)
		}
		if elem_vals.len > 0 {
			zero := b.mod.get_or_add_const(elem_type, '0')
			for i, val in elem_vals {
				if !b.valid_value_id(val) {
					elem_vals[i] = zero
				}
			}
		}
		{
			elem_kind := b.mod.type_store.types[elem_type].kind
			elem_width := b.mod.type_store.types[elem_type].width
			for i, val in elem_vals {
				val_type := b.mod.values[val].typ
				if val_type == elem_type {
					continue
				}
				if wrapped := b.wrap_value_for_sumtype_target(val, elem_type) {
					elem_vals[i] = wrapped
					continue
				}
				val_kind := b.mod.type_store.types[val_type].kind
				val_width := b.mod.type_store.types[val_type].width
				if val_kind == .int_t && elem_kind == .float_t {
					elem_vals[i] = b.mod.add_instr(.sitofp, b.cur_block, elem_type, [
						val,
					])
				} else if val_kind == .float_t && elem_kind == .float_t && val_width > elem_width {
					elem_vals[i] = b.mod.add_instr(.trunc, b.cur_block, elem_type, [
						val,
					])
				} else if val_kind == .float_t && elem_kind == .float_t && val_width < elem_width {
					elem_vals[i] = b.mod.add_instr(.zext, b.cur_block, elem_type, [
						val,
					])
				} else if val_kind == .int_t && elem_kind == .int_t && val_width > 0
					&& val_width < elem_width {
					elem_vals[i] = b.mod.add_instr(.zext, b.cur_block, elem_type, [
						val,
					])
				}
			}
		}
		arr_fixed_type := b.mod.type_store.get_array(elem_type, elem_vals.len)
		ptr_type := b.mod.type_store.get_ptr(arr_fixed_type)
		alloca := b.mod.add_instr(.alloca, b.cur_block, ptr_type, []ValueID{})
		elem_ptr_type := b.mod.type_store.get_ptr(elem_type)
		i32_t := b.mod.type_store.get_int(32)
		for i, val in elem_vals {
			idx := b.mod.get_or_add_const(i32_t, i.str())
			gep := b.mod.add_instr(.get_element_ptr, b.cur_block, elem_ptr_type, [
				alloca,
				idx,
			])
			b.mod.add_instr(.store, b.cur_block, 0, [val, gep])
		}
		mut is_fixed := false
		if len_c.kind() == .expr_postfix {
			len_op := unsafe { token.Token(int(len_c.aux())) }
			if len_op == .not {
				is_fixed = true
			}
		}
		if !is_fixed && typ_c.kind() == .typ_array_fixed {
			is_fixed = true
		}
		if is_fixed {
			return b.mod.add_instr(.load, b.cur_block, arr_fixed_type, [alloca])
		}
		if b.array_literal_as_element_buffer {
			return alloca
		}
		return b.build_dynamic_array_from_element_buffer(elem_type, elem_vals, alloca)
	}

	if typ_c.kind() == .typ_array_fixed {
		elem_type := b.ast_type_to_ssa_from_flat(typ_c.edge(1))
		arr_len := b.const_int_from_flat(typ_c.edge(0))
		if arr_len > 0 {
			arr_fixed_type := b.mod.type_store.get_array(elem_type, arr_len)
			ptr_type := b.mod.type_store.get_ptr(arr_fixed_type)
			alloca := b.mod.add_instr(.alloca, b.cur_block, ptr_type, []ValueID{})
			// Match the legacy build_array_init fixed-array zero-init exactly:
			// per-element zero-store unless the native backend bulk-zeroes large
			// allocas (x64). On arm64 (no bulk-zero) this runs for ALL sizes — the
			// old `arr_len <= 16` cap left larger fixed buffers (e.g. strconv float
			// format buffers) uninitialized, which broke the flat-SSA self-host.
			if !b.native_backend_bulk_zero_alloca
				|| arr_len <= fixed_array_empty_literal_element_store_threshold {
				zero := b.mod.get_or_add_const(elem_type, '0')
				elem_ptr_type := b.mod.type_store.get_ptr(elem_type)
				i32_t := b.mod.type_store.get_int(32)
				for i in 0 .. arr_len {
					idx := b.mod.get_or_add_const(i32_t, i.str())
					gep := b.mod.add_instr(.get_element_ptr, b.cur_block, elem_ptr_type, [
						alloca,
						idx,
					])
					b.mod.add_instr(.store, b.cur_block, 0, [zero, gep])
				}
			}
			return b.mod.add_instr(.load, b.cur_block, arr_fixed_type, [alloca])
		}
	}

	if typ_c.kind() == .typ_array && init_c.kind() == .expr_empty && update_c.kind() == .expr_empty {
		elem_type := if array_elem_hint != 0 {
			array_elem_hint
		} else {
			b.array_init_elem_type_from_flat(c)
		}
		len_val := if len_c.kind() == .expr_empty {
			ValueID(0)
		} else {
			b.build_expr_from_flat(len_c)
		}
		cap_val := if cap_c.kind() == .expr_empty {
			ValueID(0)
		} else {
			b.build_expr_from_flat(cap_c)
		}
		return b.build_empty_dynamic_array(elem_type, len_val, cap_val)
	}

	arr_type := b.get_array_type()
	return b.mod.get_or_add_const(arr_type, '0')
}

fn (mut b Builder) build_snprintf_string(val ValueID, fmt string) ValueID {
	str_type := b.get_string_type()
	i8_t := b.mod.type_store.get_int(8)
	ptr_type := b.mod.type_store.get_ptr(i8_t)
	i32_t := b.mod.type_store.get_int(32)
	i64_t := b.mod.type_store.get_int(64)
	bool_t := b.mod.type_store.get_int(1)
	len_fn_name := if b.is_windows_target() { '_scprintf' } else { 'snprintf' }
	write_fn_name := if b.is_windows_target() { '_snprintf' } else { 'snprintf' }
	len_fn_ref := b.get_or_create_fn_ref(len_fn_name, i32_t)
	write_fn_ref := b.get_or_create_fn_ref(write_fn_name, i32_t)
	fmt_val := b.mod.add_value_node(.c_string_literal, ptr_type, fmt, 0)
	formatted_val := b.prepare_snprintf_arg(val, fmt)
	null_ptr := b.mod.get_or_add_const(ptr_type, '0')
	zero_size := b.mod.get_or_add_const(i64_t, '0')
	sn_len := if b.is_windows_target() {
		b.mod.add_instr(.call, b.cur_block, i32_t, [len_fn_ref, fmt_val, formatted_val])
	} else {
		b.mod.add_instr(.call, b.cur_block, i32_t, [len_fn_ref, null_ptr, zero_size, fmt_val,
			formatted_val])
	}
	zero_i32 := b.mod.get_or_add_const(i32_t, '0')
	is_negative := b.mod.add_instr(.lt, b.cur_block, bool_t, [sn_len, zero_i32])
	fail_block := b.mod.add_block(b.cur_func, 'snprintf_fail')
	ok_block := b.mod.add_block(b.cur_func, 'snprintf_ok')
	merge_block := b.mod.add_block(b.cur_func, 'snprintf_merge')
	b.mod.add_instr(.br, b.cur_block, 0,
		[is_negative, b.mod.blocks[fail_block].val_id, b.mod.blocks[ok_block].val_id])
	b.add_edge(b.cur_block, fail_block)
	b.add_edge(b.cur_block, ok_block)

	b.cur_block = fail_block
	empty_str := b.mod.add_value_node(.string_literal, str_type, '', 0)
	fail_end_block := b.cur_block
	b.mod.add_instr(.jmp, b.cur_block, 0, [b.mod.blocks[merge_block].val_id])
	b.add_edge(b.cur_block, merge_block)

	b.cur_block = ok_block
	sn_len_i64 := b.mod.add_instr(.sext, b.cur_block, i64_t, [sn_len])
	one_i64 := b.mod.get_or_add_const(i64_t, '1')
	alloc_len := b.mod.add_instr(.add, b.cur_block, i64_t, [sn_len_i64, one_i64])
	malloc_ref := b.get_or_create_fn_ref('builtin__malloc_noscan', ptr_type)
	buf_ptr := b.mod.add_instr(.call, b.cur_block, ptr_type, [malloc_ref, alloc_len])
	write_result := b.mod.add_instr(.call, b.cur_block, i32_t, [write_fn_ref, buf_ptr, alloc_len,
		fmt_val, formatted_val])
	write_negative := b.mod.add_instr(.lt, b.cur_block, bool_t, [write_result, zero_i32])
	write_fail_block := b.mod.add_block(b.cur_func, 'snprintf_write_fail')
	write_ok_block := b.mod.add_block(b.cur_func, 'snprintf_write_ok')
	b.mod.add_instr(.br, b.cur_block, 0, [write_negative, b.mod.blocks[write_fail_block].val_id,
		b.mod.blocks[write_ok_block].val_id])
	b.add_edge(b.cur_block, write_fail_block)
	b.add_edge(b.cur_block, write_ok_block)

	b.cur_block = write_fail_block
	write_fail_end_block := b.cur_block
	b.mod.add_instr(.jmp, b.cur_block, 0, [b.mod.blocks[merge_block].val_id])
	b.add_edge(b.cur_block, merge_block)

	b.cur_block = write_ok_block
	tos_ref := b.get_or_create_fn_ref('builtin__tos', str_type)
	ok_str := b.mod.add_instr(.call, b.cur_block, str_type, [tos_ref, buf_ptr, sn_len_i64])
	ok_end_block := b.cur_block
	b.mod.add_instr(.jmp, b.cur_block, 0, [b.mod.blocks[merge_block].val_id])
	b.add_edge(b.cur_block, merge_block)

	b.cur_block = merge_block
	return b.mod.add_instr(.phi, merge_block, str_type, [empty_str, b.mod.blocks[fail_end_block].val_id,
		empty_str, b.mod.blocks[write_fail_end_block].val_id, ok_str, b.mod.blocks[ok_end_block].val_id])
}

fn (mut b Builder) build_string_inter_from_flat(c ast.Cursor) ValueID {
	str_type := b.get_string_type()
	plus_fn := b.get_or_create_fn_ref('builtin__string__+', str_type)
	values_l := c.list_at(0)
	inters_l := c.list_at(1)
	nvalues := values_l.len()
	ninters := inters_l.len()

	mut parts := []ValueID{}
	for i in 0 .. nvalues {
		mut val := values_l.at(i).name()
		if i == 0 && val.len > 0 && (val[0] == `'` || val[0] == `"`) {
			val = val[1..]
		}
		if i == nvalues - 1 && val.len > 0 && (val[val.len - 1] == `'` || val[val.len - 1] == `"`) {
			val = val[..val.len - 1]
		}
		val = process_v_escapes(val)
		if val.len > 0 {
			parts << b.mod.add_value_node(.string_literal, str_type, val, val.len)
		}
		if i < ninters {
			inter_c := inters_l.at(i)
			format := unsafe { ast.StringInterFormat(int(inter_c.aux())) }
			resolved_fmt := inter_c.name()
			expr_c := inter_c.edge(0)
			needs_snprintf := format != .unformatted && format != .string && resolved_fmt.len > 0
			if needs_snprintf {
				inter_val := b.build_expr_from_flat(expr_c)
				parts << b.build_snprintf_string(inter_val, resolved_fmt)
			} else {
				mut use_expr_c := expr_c
				if expr_c.kind() == .expr_selector {
					rhs_c := expr_c.edge(1)
					if rhs_c.is_valid() && rhs_c.kind() == .expr_ident && rhs_c.name() == 'str' {
						use_expr_c = expr_c.edge(0)
					}
				}
				inter_val := b.build_expr_from_flat(use_expr_c)
				inter_type := b.mod.values[inter_val].typ
				str_val := b.convert_to_string(inter_val, inter_type)
				parts << str_val
			}
		}
	}

	if parts.len == 0 {
		return b.mod.add_value_node(.string_literal, str_type, '', 0)
	}
	if parts.len == 1 {
		return parts[0]
	}

	// s257: fold the concat with `plus_two` (s+a+b, ONE allocation) two parts at
	// a time instead of a left-fold of pairwise `+`. The `+`-chain heap-allocates
	// an intermediate string per step; on the native backends (no GC, `-gc none`)
	// every intermediate is retained, which is the dominant arm64 over-allocation
	// vs the C backend's `str_intp`. plus_two halves the intermediates and is
	// always built (markused keep-list + the transformer emits it for `a+b+c`).
	mut result := parts[0]
	mut idx := 1
	if parts.len >= 3 {
		plus_two_fn := b.get_or_create_fn_ref('builtin__string__plus_two', str_type)
		for idx + 1 < parts.len {
			result = b.mod.add_instr(.call, b.cur_block, str_type, [plus_two_fn, result, parts[idx],
				parts[idx + 1]])
			idx += 2
		}
	}
	for idx < parts.len {
		result = b.mod.add_instr(.call, b.cur_block, str_type, [plus_fn, result, parts[idx]])
		idx += 1
	}
	return result
}

fn (mut b Builder) build_or_from_flat(c ast.Cursor) ValueID {
	if c.edge_count() == 0 {
		return b.mod.get_or_add_const(b.mod.type_store.get_int(32), '0')
	}
	return b.build_expr_from_flat(c.edge(0))
}

fn (mut b Builder) build_fn_literal_from_flat(c ast.Cursor) ValueID {
	typ_c := c.edge(0)
	params_list := typ_c.list_at(1)
	return_type_c := typ_c.edge(2)
	ncaptured := c.extra_int()

	if ncaptured > 0 {
		return b.get_or_create_fn_ref(unsupported_captured_fn_literal_symbol, 0)
	}

	anon_name := '_anon_fn_${b.anon_fn_counter}'
	b.anon_fn_counter++
	ret_type := b.ast_type_to_ssa_from_flat(return_type_c)

	func_idx := b.mod.new_function(anon_name, ret_type, []TypeID{})
	b.fn_index[anon_name] = func_idx

	saved_func := b.cur_func
	saved_block := b.cur_block
	mut saved_vars := b.vars.clone()
	mut saved_mut_ptr_params := b.mut_ptr_params.clone()
	mut saved_local_smartcasts := b.local_smartcasts.clone()
	saved_loop_stack := b.loop_stack.clone()

	b.cur_func = func_idx
	b.vars = map[string]ValueID{}
	b.mut_ptr_params = map[string]bool{}
	b.local_smartcasts = map[string]TypeID{}
	b.loop_stack = []LoopInfo{}

	entry := b.mod.add_block(func_idx, 'entry')
	b.cur_block = entry

	for i in 0 .. params_list.len() {
		param_c := params_list.at(i)
		param_name := param_c.name()
		param_is_mut := param_c.flag(ast.flag_is_mut)
		param_type := b.ast_type_to_ssa_from_flat(param_c.edge(0))
		actual_type := if param_is_mut && !(param_type < b.mod.type_store.types.len
			&& b.mod.type_store.types[param_type].kind == .ptr_t) {
			b.mod.type_store.get_ptr(param_type)
		} else {
			param_type
		}
		param_val := b.mod.add_value_node(.argument, actual_type, param_name, 0)
		b.mod.func_add_param(func_idx, param_val)
		alloca := b.mod.add_instr(.alloca, entry, b.mod.type_store.get_ptr(actual_type),
			[]ValueID{})
		b.mod.add_instr(.store, entry, 0, [param_val, alloca])
		b.vars[param_name] = alloca
		if param_is_mut && !(param_type < b.mod.type_store.types.len
			&& b.mod.type_store.types[param_type].kind == .ptr_t) {
			b.mut_ptr_params[param_name] = true
		}
	}

	for i in (1 + ncaptured) .. c.edge_count() {
		b.build_stmt_from_flat(c.edge(i))
	}

	if !b.block_has_terminator(b.cur_block) {
		b.mod.add_instr(.ret, b.cur_block, 0, []ValueID{})
	}

	b.cur_func = saved_func
	b.cur_block = saved_block
	b.vars = saved_vars.move()
	b.mut_ptr_params = saved_mut_ptr_params.move()
	b.local_smartcasts = saved_local_smartcasts.move()
	b.loop_stack = saved_loop_stack

	fn_ptr_type := b.mod.type_store.get_ptr(b.mod.type_store.get_int(8))
	fn_ref := b.mod.add_value_node(.func_ref, fn_ptr_type, anon_name, 0)
	b.fn_refs[anon_name] = fn_ref
	return fn_ref
}

// build_call_or_cast_from_flat lowers a `.expr_call_or_cast` cursor without
// rehydrating either edge. CallOrCastExpr flat encoding stores the target
// type at `edge(0)` and the value at `edge(1)`. The cursor-native rewrite
// mirrors `build_call_or_cast`: `ast_type_to_ssa_from_flat(lhs_c)` resolves
// the target SSA type directly from the type-expr cursor (matching the legacy
// `ast_type_to_ssa(expr.lhs)` outcome), then `build_addr_from_flat(expr_c)`
// is consulted for sumtype-target wrapping, and finally `build_expr_from_flat(
// expr_c)` produces the value passed to `build_cast_value_to_type`.
fn (mut b Builder) build_call_or_cast_from_flat(c ast.Cursor) ValueID {
	lhs_c := c.edge(0)
	expr_c := c.edge(1)
	if lhs_c.kind() == .expr_ident {
		name := lhs_c.name()
		fn_name := b.resolve_call_name_ident_from_flat(lhs_c)
		if fn_name in b.fn_index && !is_builtin_cast_type_name(name) && !b.known_type_name(name) {
			return b.build_call_resolved_from_flat(fn_name, c)
		}
	}
	if lhs_c.kind() == .expr_selector {
		fn_name := b.resolve_call_name_from_flat(lhs_c)
		if mod_name := b.selector_module_name_from_flat(lhs_c) {
			if (mod_name == 'C' || fn_name in b.fn_index)
				&& !b.call_or_cast_selector_should_remain_cast_from_flat(lhs_c, fn_name) {
				return b.build_call_resolved_from_flat(fn_name, c)
			}
		}
		if fn_name in b.fn_index
			&& !b.call_or_cast_selector_should_remain_cast_from_flat(lhs_c, fn_name) {
			return b.build_call_resolved_from_flat(fn_name, c)
		}
	}
	target_type := b.ast_type_to_ssa_from_flat(lhs_c)
	if b.type_cursor_is_ierror_interface(lhs_c) {
		if tag := b.ierror_tag_for_cursor(expr_c) {
			return tag
		}
	}
	addr := b.build_addr_from_flat(expr_c)
	if addr != 0 {
		if wrapped := b.wrap_address_for_sumtype_target(addr, target_type) {
			return wrapped
		}
	}
	val := b.build_expr_from_flat(expr_c)
	return b.build_cast_value_to_type(val, target_type)
}

fn (mut b Builder) build_as_cast_from_flat(c ast.Cursor) ValueID {
	expr_c := c.edge(0)
	typ_c := c.edge(1)
	val := b.build_expr_from_flat(expr_c)
	target_type := b.ast_type_to_ssa_from_flat(typ_c)
	if target_type == 0 || b.mod.values[val].typ == target_type {
		return val
	}
	if b.ssa_type_is_sumtype(b.mod.values[val].typ) {
		return b.build_sumtype_as_cast(val, target_type)
	}
	src_checked_type := b.get_checked_expr_type_from_flat(expr_c) or { return val }
	match b.unwrap_alias_type(src_checked_type) {
		types.SumType {
			return b.build_sumtype_as_cast(val, target_type)
		}
		else {
			return val
		}
	}
}

fn (mut b Builder) build_tuple_from_flat(c ast.Cursor) ValueID {
	if c.edge_count() > 0 {
		return b.build_expr_from_flat(c.edge(0))
	}
	return b.mod.get_or_add_const(b.mod.type_store.get_int(32), '0')
}

// build_basic_literal_from_flat (s186) decodes only what BasicLiteral needs
// — `kind` from `c.aux()` (u16 → token.Token), `value` from `c.name()`
// (interned string), `pos` from `c.pos()` — and dispatches to
// `build_basic_literal`. Preserving `pos` keeps the type-checker lookup
// (`get_checked_expr_type(pos.id)`) hot so the resolved SSA type matches
// the legacy path exactly. Saves only the `Expr(BasicLiteral{...})`
// sum-type box; a future refactor could split build_basic_literal into a
// `build_basic_literal_inner(kind, value, pos)` if needed.
fn (mut b Builder) build_basic_literal_from_flat(c ast.Cursor) ValueID {
	kind := unsafe { token.Token(int(c.aux())) }
	return b.build_basic_literal(ast.BasicLiteral{
		kind:  kind
		value: c.name()
		pos:   c.pos()
	})
}

fn (mut b Builder) build_basic_literal(lit ast.BasicLiteral) ValueID {
	match lit.kind {
		.key_true {
			return b.mod.get_or_add_const(b.mod.type_store.get_int(1), '1')
		}
		.key_false {
			return b.mod.get_or_add_const(b.mod.type_store.get_int(1), '0')
		}
		.char {
			// Resolve character literal to its numeric value (Unicode code point)
			char_val := b.resolve_char_value(lit.value)
			// Always use 32-bit (rune) type — V rune literals are i32
			typ := b.mod.type_store.get_int(32)
			return b.mod.get_or_add_const(typ, char_val.str())
		}
		.number {
			if lit.value.contains('.')
				|| (!lit.value.starts_with('0x') && !lit.value.starts_with('0X')
				&& (lit.value.contains('e') || lit.value.contains('E'))) {
				typ := b.mod.type_store.get_float(64)
				return b.mod.get_or_add_const(typ, lit.value)
			}
			mut typ := b.expr_type(ast.Expr(lit))
			// Number literals should never inherit non-numeric checked types. This can
			// happen when expression positions map a literal to an enclosing expression.
			// Also keep the older i1 guard for negation/boolean contexts.
			if typ != 0 && int(typ) < b.mod.type_store.types.len {
				t0 := b.mod.type_store.types[typ]
				if t0.kind !in [.int_t, .float_t] {
					typ = b.mod.type_store.get_int(64)
				} else if t0.kind == .int_t && t0.width == 1 {
					typ = b.mod.type_store.get_int(32)
				}
			}
			// Widen to i64 if the literal value overflows the assigned type.
			// The type checker may assign `int` (i32) to a literal like 9223372036854775807
			// which doesn't fit in 32 bits.
			if typ != 0 && int(typ) < b.mod.type_store.types.len {
				t := b.mod.type_store.types[typ]
				if t.kind == .int_t && t.width <= 32 {
					val := lit.value
					// Check if the value exceeds i32 range
					if val.len >= 10 {
						parsed := val.i64()
						if parsed > 2147483647 || parsed < -2147483648 {
							return b.mod.get_or_add_const(b.mod.type_store.get_int(64), val)
						}
					}
				}
			}
			return b.mod.get_or_add_const(typ, lit.value)
		}
		else {
			// Integer or other
			typ := b.expr_type(ast.Expr(lit))
			return b.mod.get_or_add_const(typ, lit.value)
		}
	}
}

fn (mut b Builder) build_string_literal(lit ast.StringLiteral) ValueID {
	// Strip surrounding quotes from V string literal values
	mut val := lit.value
	if val.len >= 2 && ((val[0] == `'` && val[val.len - 1] == `'`)
		|| (val[0] == `"` && val[val.len - 1] == `"`)) {
		val = val[1..val.len - 1]
	}
	if lit.kind == .c {
		// C string literal -> raw i8* pointer
		i8_t := b.mod.type_store.get_int(8)
		ptr_t := b.mod.type_store.get_ptr(i8_t)
		return b.mod.add_value_node(.c_string_literal, ptr_t, val, 0)
	}
	// Process V escape sequences for non-raw strings
	if lit.kind != .raw {
		val = process_v_escapes(val)
	}
	typ := b.get_string_type()
	result := b.mod.add_value_node(.string_literal, typ, val, val.len)
	return result
}

// process_v_escapes converts V escape sequences (\n, \t, etc.) in a string
// to their actual byte values. The V2 scanner stores raw escape sequences
// (e.g., \t as two chars '\' + 't'), not processed bytes.
fn process_v_escapes(s string) string {
	// Fast path: no backslashes means no escapes
	if !s.contains('\\') {
		return s
	}
	mut result := []u8{cap: s.len}
	mut i := 0
	for i < s.len {
		if s[i] == `\\` && i + 1 < s.len {
			match s[i + 1] {
				`n` {
					result << 0x0A
				}
				`t` {
					result << 0x09
				}
				`r` {
					result << 0x0D
				}
				`\\` {
					result << 0x5C
				}
				`'` {
					result << 0x27
				}
				`"` {
					result << 0x22
				}
				`0` {
					result << 0x00
				}
				`a` {
					result << 0x07
				}
				`b` {
					result << 0x08
				}
				`f` {
					result << 0x0C
				}
				`v` {
					result << 0x0B
				}
				`e` {
					result << 0x1B
				}
				0x0A {
					// Backslash followed by newline: line continuation
					// Skip the backslash, newline, and leading whitespace on next line
					i += 2
					for i < s.len && (s[i] == ` ` || s[i] == `\t`) {
						i++
					}
					continue
				}
				`x` {
					// \xNN hex escape
					if i + 3 < s.len {
						hi := hex_digit(s[i + 2])
						lo := hex_digit(s[i + 3])
						if hi >= 0 && lo >= 0 {
							result << u8(hi * 16 + lo)
							i += 4
							continue
						}
					}
					result << s[i]
					i++
					continue
				}
				`u` {
					code, ok := parse_hex_escape(s, i + 2, 4)
					if ok && append_utf8_codepoint(mut result, code) {
						i += 6
						continue
					}
					result << s[i]
					i++
					continue
				}
				`U` {
					code, ok := parse_hex_escape(s, i + 2, 8)
					if ok && append_utf8_codepoint(mut result, code) {
						i += 10
						continue
					}
					result << s[i]
					i++
					continue
				}
				else {
					result << s[i]
					i++
					continue
				}
			}

			i += 2
		} else {
			result << s[i]
			i++
		}
	}
	return result.bytestr()
}

fn parse_hex_escape(s string, start int, count int) (int, bool) {
	if start + count > s.len {
		return 0, false
	}
	mut code := 0
	for j := 0; j < count; j++ {
		digit := hex_digit(s[start + j])
		if digit < 0 {
			return 0, false
		}
		code = code * 16 + digit
	}
	return code, true
}

fn append_utf8_codepoint(mut result []u8, code int) bool {
	if code < 0 || code > 0x10FFFF {
		return false
	}
	if code <= 0x7F {
		result << u8(code)
	} else if code <= 0x7FF {
		result << u8(0xC0 | (code >> 6))
		result << u8(0x80 | (code & 0x3F))
	} else if code <= 0xFFFF {
		result << u8(0xE0 | (code >> 12))
		result << u8(0x80 | ((code >> 6) & 0x3F))
		result << u8(0x80 | (code & 0x3F))
	} else {
		result << u8(0xF0 | (code >> 18))
		result << u8(0x80 | ((code >> 12) & 0x3F))
		result << u8(0x80 | ((code >> 6) & 0x3F))
		result << u8(0x80 | (code & 0x3F))
	}
	return true
}

fn hex_digit(c u8) int {
	if c >= `0` && c <= `9` {
		return int(c - `0`)
	}
	if c >= `a` && c <= `f` {
		return int(c - `a` + 10)
	}
	if c >= `A` && c <= `F` {
		return int(c - `A` + 10)
	}
	return -1
}

fn (mut b Builder) build_string_inter_literal(expr ast.StringInterLiteral) ValueID {
	// String interpolation: concatenate literal parts and expression-to-string conversions.
	// Pattern: values[0] + str(inters[0]) + values[1] + str(inters[1]) + ...
	str_type := b.get_string_type()
	plus_fn := b.get_or_create_fn_ref('builtin__string__+', str_type)

	mut parts := []ValueID{}
	for i, raw_val in expr.values {
		// Strip surrounding quotes from first and last values (parser artifact)
		mut val := raw_val
		if i == 0 && val.len > 0 && (val[0] == `'` || val[0] == `"`) {
			val = val[1..]
		}
		if i == expr.values.len - 1 && val.len > 0
			&& (val[val.len - 1] == `'` || val[val.len - 1] == `"`) {
			val = val[..val.len - 1]
		}
		// Process V escape sequences in interpolation literal parts
		val = process_v_escapes(val)
		if val.len > 0 {
			parts << b.mod.add_value_node(.string_literal, str_type, val, val.len)
		}
		if i < expr.inters.len {
			inter := expr.inters[i]
			// Check if this interpolation needs C.snprintf formatting.
			// Use snprintf for explicit format specifiers (:.3f, :03d, :c, :x, :o, etc.)
			// but not for :s (string) or unformatted — those use V string concatenation.
			needs_snprintf := inter.format != .unformatted && inter.format != .string
				&& inter.resolved_fmt.len > 0
			if needs_snprintf {
				inter_val := b.build_expr(inter.expr)
				parts << b.build_snprintf_string(inter_val, inter.resolved_fmt)
			} else {
				// Unformatted or simple format: use string concatenation.
				// If the inter.expr is a SelectorExpr accessing '.str' on a string,
				// use the base expression (the V string) directly.
				mut use_expr := inter.expr
				if inter.expr is ast.SelectorExpr {
					sel := inter.expr as ast.SelectorExpr
					if sel.rhs.name == 'str' {
						use_expr = sel.lhs
					}
				}
				inter_val := b.build_expr(use_expr)
				inter_type := b.mod.values[inter_val].typ
				// Convert the interpolation value to string
				str_val := b.convert_to_string(inter_val, inter_type)
				parts << str_val
			}
		}
	}

	if parts.len == 0 {
		return b.mod.add_value_node(.string_literal, str_type, '', 0)
	}
	if parts.len == 1 {
		return parts[0]
	}

	// s257: fold via `plus_two` (one allocation per 2 parts) instead of a
	// left-fold of allocating `+` calls — see build_string_inter_from_flat.
	mut result := parts[0]
	mut idx := 1
	if parts.len >= 3 {
		plus_two_fn := b.get_or_create_fn_ref('builtin__string__plus_two', str_type)
		for idx + 1 < parts.len {
			result = b.mod.add_instr(.call, b.cur_block, str_type, [plus_two_fn, result, parts[idx],
				parts[idx + 1]])
			idx += 2
		}
	}
	for idx < parts.len {
		result = b.mod.add_instr(.call, b.cur_block, str_type, [plus_fn, result, parts[idx]])
		idx += 1
	}
	return result
}

fn (mut b Builder) prepare_snprintf_arg(val ValueID, fmt string) ValueID {
	if fmt.len == 0 || val <= 0 || val >= b.mod.values.len {
		return val
	}
	typ_id := b.mod.values[val].typ
	if typ_id <= 0 || typ_id >= b.mod.type_store.types.len {
		return val
	}
	typ := b.mod.type_store.types[typ_id]
	fmt_char := fmt[fmt.len - 1]
	match fmt_char {
		`d`, `i`, `u`, `o`, `x`, `X`, `c` {
			if typ.kind != .int_t {
				return val
			}
			needs_u32 := fmt_char in [`u`, `o`, `x`, `X`]
			if fmt.contains('ll') {
				target := if needs_u32 {
					b.mod.type_store.get_uint(64)
				} else {
					b.mod.type_store.get_int(64)
				}
				return b.cast_value_to_type(val, target)
			}
			target := if needs_u32 {
				b.mod.type_store.get_uint(32)
			} else {
				b.mod.type_store.get_int(32)
			}
			return b.cast_value_to_type(val, target)
		}
		`f`, `e`, `g` {
			if typ.kind == .float_t && typ.width == 32 {
				return b.cast_value_to_type(val, b.mod.type_store.get_float(64))
			}
		}
		else {}
	}

	return val
}

fn (mut b Builder) convert_to_string(val ValueID, typ TypeID) ValueID {
	str_type := b.get_string_type()
	// If already a string, return as-is.
	if b.is_string_struct_type(typ) {
		return val
	}
	if val <= 0 || val >= b.mod.values.len || typ <= 0 || int(typ) >= b.mod.type_store.types.len {
		return b.mod.add_value_node(.string_literal, str_type, '', 0)
	}
	// Determine the conversion function based on the type
	type_info := b.mod.type_store.types[typ]
	mut fn_name := ''
	if type_info.kind == .int_t {
		if type_info.width == 1 {
			// bool
			fn_name = 'builtin__bool__str'
		} else if type_info.width == 64 {
			fn_name = 'builtin__i64__str'
		} else {
			fn_name = 'builtin__int__str'
		}
	} else if type_info.kind == .float_t {
		if type_info.width == 32 {
			fn_name = 'builtin__f32__str'
		} else {
			fn_name = 'builtin__f64__str'
		}
	}
	if fn_name != '' {
		fn_ref := b.get_or_create_fn_ref(fn_name, str_type)
		return b.mod.add_instr(.call, b.cur_block, str_type, [fn_ref, val])
	}
	if str_fn_name := b.str_fn_name_for_ssa_type(typ) {
		fn_ref := b.get_or_create_fn_ref(str_fn_name, str_type)
		return b.mod.add_instr(.call, b.cur_block, str_type, [fn_ref, val])
	}
	// Fallback: return empty string
	return b.mod.add_value_node(.string_literal, str_type, '', 0)
}

fn (b &Builder) str_fn_name_for_ssa_type(typ TypeID) ?string {
	if typ <= 0 || int(typ) >= b.mod.type_store.types.len {
		return none
	}
	type_name := b.mod.c_struct_names[int(typ)] or { return none }
	if type_name == '' || ssa_is_string_struct_name(type_name) {
		return none
	}
	fn_name := '${type_name}__str'
	if fn_name in b.fn_index {
		return fn_name
	}
	if type_name.contains('__') {
		short_name := type_name.all_after_last('__')
		short_fn_name := '${short_name}__str'
		if short_fn_name in b.fn_index {
			return short_fn_name
		}
	}
	return none
}

fn (b &Builder) is_string_struct_type(typ TypeID) bool {
	str_type := b.struct_types['string'] or { TypeID(0) }
	if str_type != 0 && typ == str_type {
		return true
	}
	if typ <= 0 || int(typ) >= b.mod.type_store.types.len {
		return false
	}
	if name := b.mod.c_struct_names[int(typ)] {
		if ssa_is_string_struct_name(name) {
			return true
		}
	}
	tinfo := b.mod.type_store.types[typ]
	return tinfo.kind == .struct_t && tinfo.field_names.len == 3 && tinfo.field_names[0] == 'str'
		&& tinfo.field_names[1] == 'len' && tinfo.field_names[2] == 'is_lit'
}

fn (mut b Builder) build_ident(ident ast.Ident) ValueID {
	// Handle 'nil' identifier (generated by transformer from `unsafe { nil }`)
	if ident.name == 'nil' {
		i8_t := b.mod.type_store.get_int(8)
		ptr_t := b.mod.type_store.get_ptr(i8_t)
		return b.mod.get_or_add_const(ptr_t, '0')
	}
	if ident.name in b.vars {
		ptr := b.vars[ident.name]
		ptr_typ := b.mod.values[ptr].typ
		elem_typ := b.mod.type_store.types[ptr_typ].elem_type
		val := b.mod.add_instr(.load, b.cur_block, elem_typ, [ptr])
		if elem_t := b.array_elem_types[ident.name] {
			b.array_value_elem_types[val] = elem_t
		}
		// For mut pointer params (e.g., mut buf &u8), the alloca stores ptr(ptr(T)).
		// One load gives ptr(ptr(T)), but user sees buf as ptr(T).
		// Add extra dereference to get the actual pointer value.
		if ident.name in b.mut_ptr_params {
			inner_typ := b.mod.type_store.types[elem_typ].elem_type
			if inner_typ != 0 {
				return b.mod.add_instr(.load, b.cur_block, inner_typ, [val])
			}
		}
		return val
	}
	// Could be a constant, enum value, or function reference
	if ident.name in b.enum_values {
		val := b.enum_values[ident.name]
		return b.mod.get_or_add_const(b.mod.type_store.get_int(32), val.str())
	}
	// Try enum value with module prefix (e.g., Token__key_fn → token__Token__key_fn)
	{
		enum_qualified := '${b.cur_module}__${ident.name}'
		if enum_qualified in b.enum_values {
			val := b.enum_values[enum_qualified]
			return b.mod.get_or_add_const(b.mod.type_store.get_int(32), val.str())
		}
	}
	// Try as function reference
	qualified_name := '${b.cur_module}__${ident.name}'
	if local_fn := b.current_module_fn_name(ident.name) {
		return b.get_or_create_fn_ref(local_fn, 0)
	}
	if imported := b.selective_import_fn_name(ident.name) {
		return b.get_or_create_fn_ref(imported, 0)
	}
	if ident.name in b.fn_index {
		return b.get_or_create_fn_ref(ident.name, 0)
	}
	if qualified_name in b.fn_index {
		return b.get_or_create_fn_ref(qualified_name, 0)
	}
	// Try with module prefix (transformer strips module prefix for builtin functions)
	builtin_name := 'builtin__${ident.name}'
	if builtin_name in b.fn_index {
		return b.get_or_create_fn_ref(builtin_name, 0)
	}
	// Try as float constant (inline as f64)
	if fval := b.float_const_values[ident.name] {
		return b.mod.get_or_add_const(b.mod.type_store.get_float(64), fval)
	}
	if fval := b.float_const_values[qualified_name] {
		return b.mod.get_or_add_const(b.mod.type_store.get_float(64), fval)
	}
	builtin_const := 'builtin__${ident.name}'
	if fval := b.float_const_values[builtin_const] {
		return b.mod.get_or_add_const(b.mod.type_store.get_float(64), fval)
	}
	// Try as compile-time constant (inline the value directly)
	if ident.name in b.const_values {
		ct := if ident.name in b.const_value_types {
			b.const_value_types[ident.name]
		} else {
			b.mod.type_store.get_int(64)
		}
		return b.mod.get_or_add_const(ct, b.const_values[ident.name].str())
	}
	if qualified_name in b.const_values {
		ct := if qualified_name in b.const_value_types {
			b.const_value_types[qualified_name]
		} else {
			b.mod.type_store.get_int(64)
		}
		return b.mod.get_or_add_const(ct, b.const_values[qualified_name].str())
	}
	if builtin_const in b.const_values {
		ct := if builtin_const in b.const_value_types {
			b.const_value_types[builtin_const]
		} else {
			b.mod.type_store.get_int(64)
		}
		return b.mod.get_or_add_const(ct, b.const_values[builtin_const].str())
	}
	// Try as string constant (inline the string literal directly)
	if ident.name in b.string_const_values {
		return b.build_string_literal(ast.StringLiteral{
			kind:  .v
			value: b.string_const_values[ident.name]
		})
	}
	if qualified_name in b.string_const_values {
		return b.build_string_literal(ast.StringLiteral{
			kind:  .v
			value: b.string_const_values[qualified_name]
		})
	}
	if builtin_const in b.string_const_values {
		return b.build_string_literal(ast.StringLiteral{
			kind:  .v
			value: b.string_const_values[builtin_const]
		})
	}
	// Try as constant array global (return pointer directly for indexing)
	if ident.name in b.const_array_globals {
		if glob_id := b.find_global(ident.name) {
			return glob_id
		}
		// Cross-module const array: transformer strips the module prefix
		// (e.g., "strconv__pow5_split_64_x" → "pow5_split_64_x").
		// Scan globals for a match with any module prefix.
		suffix := '__${ident.name}'
		for v in b.mod.values {
			if v.kind == .global && v.name.ends_with(suffix) {
				return v.id
			}
		}
	}
	if qualified_name in b.const_array_globals {
		if glob_id := b.find_global(qualified_name) {
			return glob_id
		}
	}
	if builtin_const in b.const_array_globals {
		if glob_id := b.find_global(builtin_const) {
			return glob_id
		}
	}
	if b.cur_module == 'builtin' && ident.name in ['g_main_argc', 'g_main_argv'] {
		if glob_id := b.find_global(builtin_const) {
			glob_typ := b.mod.values[glob_id].typ
			elem_typ := b.mod.type_store.types[glob_typ].elem_type
			return b.mod.add_instr(.load, b.cur_block, elem_typ, [glob_id])
		}
	}
	// Try as global variable
	if glob_id := b.find_global_ident(ident.name) {
		glob_typ := b.mod.values[glob_id].typ
		elem_typ := b.mod.type_store.types[glob_typ].elem_type
		return b.mod.add_instr(.load, b.cur_block, elem_typ, [glob_id])
	}
	return b.mod.get_or_add_const(b.mod.type_store.get_int(64), '0')
}

fn (b &Builder) ident_has_inline_const_value(name string) bool {
	qualified_name := '${b.cur_module}__${name}'
	builtin_name := 'builtin__${name}'
	return name in b.const_values || qualified_name in b.const_values
		|| builtin_name in b.const_values || name in b.string_const_values
		|| qualified_name in b.string_const_values || builtin_name in b.string_const_values
		|| name in b.float_const_values || qualified_name in b.float_const_values
		|| builtin_name in b.float_const_values
}

fn (mut b Builder) find_global(name string) ?ValueID {
	if name in b.global_refs {
		return b.global_refs[name]
	}
	return none
}

fn (mut b Builder) find_global_ident(name string) ?ValueID {
	if name.starts_with('C.') {
		return b.find_global(name)
	}
	qualified_name := ssa_module_storage_name(b.cur_module, name)
	if qualified_name != name {
		if glob_id := b.find_global(qualified_name) {
			return glob_id
		}
	}
	if glob_id := b.find_global(name) {
		return glob_id
	}
	builtin_name := ssa_module_storage_name('builtin', name)
	if builtin_name != name && builtin_name != qualified_name {
		if glob_id := b.find_global(builtin_name) {
			return glob_id
		}
	}
	return none
}

fn (mut b Builder) index_global_values() {
	for v in b.mod.values {
		if v.kind == .global {
			b.global_refs[v.name] = v.id
		}
	}
}

fn (mut b Builder) build_infix(expr ast.InfixExpr) ValueID {
	// Short-circuit evaluation for logical || and &&
	if expr.op == .logical_or {
		bool_type := b.mod.type_store.get_int(1)
		// Evaluate LHS
		lhs := b.build_expr(expr.lhs)
		lhs_block := b.cur_block
		// Create blocks: rhs_block (evaluate RHS), merge_block
		rhs_block := b.mod.add_block(b.cur_func, 'or_rhs')
		merge_block := b.mod.add_block(b.cur_func, 'or_merge')
		// If LHS is true, short-circuit to merge; else evaluate RHS
		b.mod.add_instr(.br, b.cur_block, 0,
			[lhs, b.mod.blocks[merge_block].val_id, b.mod.blocks[rhs_block].val_id])
		b.add_edge(b.cur_block, merge_block)
		b.add_edge(b.cur_block, rhs_block)
		// RHS block
		b.cur_block = rhs_block
		rhs := b.build_expr(expr.rhs)
		rhs_end_block := b.cur_block
		if !b.block_has_terminator(b.cur_block) {
			b.mod.add_instr(.jmp, b.cur_block, 0, [b.mod.blocks[merge_block].val_id])
			b.add_edge(b.cur_block, merge_block)
		}
		// Merge block: use phi to select result (no alloca/store/load)
		b.cur_block = merge_block
		one := b.mod.get_or_add_const(bool_type, '1')
		phi_val := b.mod.add_instr(.phi, merge_block, bool_type, [one, b.mod.blocks[lhs_block].val_id,
			rhs, b.mod.blocks[rhs_end_block].val_id])
		return phi_val
	}
	if expr.op == .and {
		bool_type := b.mod.type_store.get_int(1)
		// Evaluate LHS
		lhs := b.build_expr(expr.lhs)
		lhs_block := b.cur_block
		// Create blocks: rhs_block (evaluate RHS), merge_block
		rhs_block := b.mod.add_block(b.cur_func, 'and_rhs')
		merge_block := b.mod.add_block(b.cur_func, 'and_merge')
		// If LHS is false, short-circuit to merge with false; else evaluate RHS
		b.mod.add_instr(.br, b.cur_block, 0,
			[lhs, b.mod.blocks[rhs_block].val_id, b.mod.blocks[merge_block].val_id])
		b.add_edge(b.cur_block, rhs_block)
		b.add_edge(b.cur_block, merge_block)
		// RHS block
		b.cur_block = rhs_block
		rhs := b.build_expr(expr.rhs)
		rhs_end_block := b.cur_block
		if !b.block_has_terminator(b.cur_block) {
			b.mod.add_instr(.jmp, b.cur_block, 0, [b.mod.blocks[merge_block].val_id])
			b.add_edge(b.cur_block, merge_block)
		}
		// Merge block: use phi to select result (no alloca/store/load)
		b.cur_block = merge_block
		zero := b.mod.get_or_add_const(bool_type, '0')
		phi_val := b.mod.add_instr(.phi, merge_block, bool_type, [zero, b.mod.blocks[lhs_block].val_id,
			rhs, b.mod.blocks[rhs_end_block].val_id])
		return phi_val
	}

	lhs := b.build_expr(expr.lhs)
	if tag_cmp := b.build_ierror_concrete_compare(lhs, expr.lhs, expr.rhs, expr.op) {
		return tag_cmp
	}
	if tag_cmp := b.build_sumtype_tag_selector_variant_compare(lhs, expr.lhs, expr.rhs, expr.op) {
		return tag_cmp
	}
	if tag_cmp := b.build_sumtype_variant_compare_expr(lhs, expr.rhs, expr.op) {
		return tag_cmp
	}
	rhs := b.build_expr(expr.rhs)
	if tag_cmp := b.build_sumtype_variant_compare_expr(rhs, expr.lhs, expr.op) {
		return tag_cmp
	}
	result_type := b.expr_type(ast.Expr(expr))
	if tag_cmp := b.build_sumtype_tag_compare_values(lhs, rhs, expr.op) {
		return tag_cmp
	}
	// Handle Option/Result comparison with none: x == none / x != none
	// Compare the state field (field 0) with 0 (success state) instead of
	// doing a whole-struct comparison with an integer.
	if (expr.op == .eq || expr.op == .ne) && (b.is_none_expr(expr.rhs) || b.is_none_expr(expr.lhs)) {
		option_val := if b.is_none_expr(expr.rhs) { lhs } else { rhs }
		option_type := b.mod.values[option_val].typ
		if b.is_option_wrapper_type(option_type) {
			// Extract state field and check: state != 0 means none/error
			i32_t := b.mod.type_store.get_int(32)
			bool_t := b.mod.type_store.get_int(1)
			flag_idx := b.mod.get_or_add_const(i32_t, '0')
			flag_type := b.mod.type_store.types[option_type].fields[0]
			flag_val := b.mod.add_instr(.extractvalue, b.cur_block, flag_type, [
				option_val,
				flag_idx,
			])
			zero_flag := b.mod.get_or_add_const(flag_type, '0')
			if expr.op == .eq {
				// x == none → state != 0 (non-zero state means none)
				return b.mod.add_instr(.ne, b.cur_block, bool_t, [flag_val, zero_flag])
			} else {
				// x != none → state == 0 (zero state means success/has value)
				return b.mod.add_instr(.eq, b.cur_block, bool_t, [flag_val, zero_flag])
			}
		}
		if b.is_result_wrapper_type(option_type) {
			i32_t := b.mod.type_store.get_int(32)
			bool_t := b.mod.type_store.get_int(1)
			flag_idx := b.mod.get_or_add_const(i32_t, '0')
			flag_type := b.mod.type_store.types[option_type].fields[0]
			flag_val := b.mod.add_instr(.extractvalue, b.cur_block, flag_type, [
				option_val,
				flag_idx,
			])
			zero_flag := b.mod.get_or_add_const(flag_type, '0')
			if expr.op == .eq {
				return b.mod.add_instr(.ne, b.cur_block, bool_t, [flag_val, zero_flag])
			} else {
				return b.mod.add_instr(.eq, b.cur_block, bool_t, [flag_val, zero_flag])
			}
		}
	}
	// Check for string comparison: if either operand is a string-like type
	// (string or &string), emit string calls instead of integer comparisons.
	// This handles match-on-string expressions where the transformer creates
	// InfixExpr{.eq, ...} that bypasses the normal string comparison lowering.
	str_type := b.get_string_type()
	if str_type != 0 {
		lhs_type := b.mod.values[lhs].typ
		rhs_type := b.mod.values[rhs].typ
		mut lhs_is_string := lhs_type == str_type
		if !lhs_is_string && lhs_type > 0 && int(lhs_type) < b.mod.type_store.types.len {
			lhs_typ := b.mod.type_store.types[lhs_type]
			lhs_is_string = lhs_typ.kind == .ptr_t && lhs_typ.elem_type == str_type
		}
		mut rhs_is_string := rhs_type == str_type
		if !rhs_is_string && rhs_type > 0 && int(rhs_type) < b.mod.type_store.types.len {
			rhs_typ := b.mod.type_store.types[rhs_type]
			rhs_is_string = rhs_typ.kind == .ptr_t && rhs_typ.elem_type == str_type
		}
		if lhs_is_string || rhs_is_string {
			string_lhs := b.load_string_like_value(lhs)
			string_rhs := b.load_string_like_value(rhs)
			if expr.op in [.eq, .ne] {
				bool_type := b.mod.type_store.get_int(1)
				fn_ref := b.get_or_create_fn_ref('builtin__string__==', bool_type)
				eq_result := b.mod.add_instr(.call, b.cur_block, bool_type, [fn_ref, string_lhs,
					string_rhs])
				if expr.op == .ne {
					return b.mod.add_instr(.xor, b.cur_block, bool_type, [eq_result,
						b.mod.get_or_add_const(bool_type, '1')])
				}
				return eq_result
			}
			if expr.op in [.lt, .gt, .le, .ge] {
				bool_type := b.mod.type_store.get_int(1)
				fn_ref := b.get_or_create_fn_ref('builtin__string__<', bool_type)
				lt_lhs := if expr.op in [.gt, .le] { string_rhs } else { string_lhs }
				lt_rhs := if expr.op in [.gt, .le] { string_lhs } else { string_rhs }
				lt_result := b.mod.add_instr(.call, b.cur_block, bool_type,
					[fn_ref, lt_lhs, lt_rhs])
				if expr.op in [.le, .ge] {
					return b.mod.add_instr(.xor, b.cur_block, bool_type, [lt_result,
						b.mod.get_or_add_const(bool_type, '1')])
				}
				return lt_result
			}
			if expr.op == .plus {
				fn_ref := b.get_or_create_fn_ref('builtin__string__+', str_type)
				return b.mod.add_instr(.call, b.cur_block, str_type,
					[fn_ref, string_lhs, string_rhs])
			}
		}
	}

	// Check for array comparison: if either operand is an array type,
	// call array__eq instead of bitwise comparison.
	// This handles cases where the transformer didn't lower the comparison
	// (e.g., type aliases like `type Strings = []string`).
	array_type := b.get_array_type()
	if array_type != 0 && (expr.op == .eq || expr.op == .ne) {
		lhs_type := b.mod.values[lhs].typ
		rhs_type := b.mod.values[rhs].typ
		if lhs_type == array_type || rhs_type == array_type {
			bool_type := b.mod.type_store.get_int(1)
			fn_ref := b.get_or_create_fn_ref('array__eq', bool_type)
			eq_result := b.mod.add_instr(.call, b.cur_block, bool_type, [fn_ref, lhs, rhs])
			if expr.op == .ne {
				return b.mod.add_instr(.xor, b.cur_block, bool_type, [eq_result,
					b.mod.get_or_add_const(bool_type, '1')])
			}
			return eq_result
		}
	}

	// Pointer arithmetic: ptr + int or int + ptr → GEP for proper element scaling.
	// Without this, `*i32 + 1` adds 1 byte instead of 4 bytes.
	if expr.op == .plus || expr.op == .minus {
		lhs_t := b.mod.values[lhs].typ
		rhs_t := b.mod.values[rhs].typ
		mut ptr_val := ValueID(0)
		mut int_val := ValueID(0)
		mut is_ptr_arith := false
		if lhs_t > 0 && int(lhs_t) < b.mod.type_store.types.len
			&& b.mod.type_store.types[lhs_t].kind == .ptr_t {
			ptr_val = lhs
			int_val = rhs
			is_ptr_arith = true
		} else if rhs_t > 0 && int(rhs_t) < b.mod.type_store.types.len
			&& b.mod.type_store.types[rhs_t].kind == .ptr_t {
			ptr_val = rhs
			int_val = lhs
			is_ptr_arith = true
		}
		if is_ptr_arith {
			ptr_type := b.mod.values[ptr_val].typ
			if expr.op == .minus {
				// ptr - int: negate the integer, then GEP
				int_val = b.mod.add_instr(.sub, b.cur_block, b.mod.values[int_val].typ, [
					b.mod.get_or_add_const(b.mod.values[int_val].typ, '0'),
					int_val,
				])
			}
			return b.mod.add_instr(.get_element_ptr, b.cur_block, ptr_type, [
				ptr_val,
				int_val,
			])
		}
	}

	// Check if operands are float type to use float opcodes.
	// When one operand is float and the other is int, promote the int to float.
	mut lhs_v := lhs
	mut rhs_v := rhs
	lhs_type := b.mod.values[lhs_v].typ
	rhs_type_id := b.mod.values[rhs_v].typ
	lhs_is_float := lhs_type > 0 && int(lhs_type) < b.mod.type_store.types.len
		&& b.mod.type_store.types[lhs_type].kind == .float_t
	rhs_is_float := rhs_type_id > 0 && int(rhs_type_id) < b.mod.type_store.types.len
		&& b.mod.type_store.types[rhs_type_id].kind == .float_t
	is_float := lhs_is_float || rhs_is_float
	if is_float {
		if lhs_is_float && !rhs_is_float {
			// Promote RHS int to float: use uitofp for unsigned types, sitofp for signed
			rhs_unsigned := rhs_type_id > 0 && int(rhs_type_id) < b.mod.type_store.types.len
				&& b.mod.type_store.types[rhs_type_id].is_unsigned
			conv_op := if rhs_unsigned { OpCode.uitofp } else { OpCode.sitofp }
			rhs_v = b.mod.add_instr(conv_op, b.cur_block, lhs_type, [rhs_v])
		} else if rhs_is_float && !lhs_is_float {
			// Promote LHS int to float: use uitofp for unsigned types, sitofp for signed
			lhs_unsigned := lhs_type > 0 && int(lhs_type) < b.mod.type_store.types.len
				&& b.mod.type_store.types[lhs_type].is_unsigned
			conv_op := if lhs_unsigned { OpCode.uitofp } else { OpCode.sitofp }
			lhs_v = b.mod.add_instr(conv_op, b.cur_block, rhs_type_id, [lhs_v])
		}
	}

	mut forced_result_type := TypeID(0)
	if !is_float && expr.op == .minus {
		lhs_t := b.mod.values[lhs_v].typ
		rhs_t := b.mod.values[rhs_v].typ
		if lhs_t > 0 && rhs_t > 0 && int(lhs_t) < b.mod.type_store.types.len
			&& int(rhs_t) < b.mod.type_store.types.len {
			lhs_info := b.mod.type_store.types[lhs_t]
			rhs_info := b.mod.type_store.types[rhs_t]
			if lhs_info.kind == .int_t && rhs_info.kind == .int_t && lhs_info.width < 32
				&& rhs_info.width < 32 {
				int_t := b.mod.type_store.get_int(32)
				if lhs_t != int_t {
					lhs_v = b.mod.add_instr(if lhs_info.is_unsigned { .zext } else { .sext },
						b.cur_block, int_t, [lhs_v])
				}
				if rhs_t != int_t {
					rhs_v = b.mod.add_instr(if rhs_info.is_unsigned { .zext } else { .sext },
						b.cur_block, int_t, [rhs_v])
				}
				forced_result_type = int_t
			}
		}
	}

	op := if is_float {
		match expr.op {
			.plus { OpCode.fadd }
			.minus { OpCode.fsub }
			.mul { OpCode.fmul }
			.div { OpCode.fdiv }
			.mod { OpCode.frem }
			// Comparisons use the same opcodes - the arm64 codegen
			// already checks float type for eq/ne/lt/gt/le/ge
			.eq { OpCode.eq }
			.ne { OpCode.ne }
			.lt { OpCode.lt }
			.gt { OpCode.gt }
			.le { OpCode.le }
			.ge { OpCode.ge }
			else { OpCode.fadd }
		}
	} else {
		is_unsigned := lhs_type > 0 && int(lhs_type) < b.mod.type_store.types.len
			&& b.mod.type_store.types[lhs_type].is_unsigned
		match expr.op {
			.plus {
				OpCode.add
			}
			.minus {
				OpCode.sub
			}
			.mul {
				OpCode.mul
			}
			.div {
				if is_unsigned {
					OpCode.udiv
				} else {
					OpCode.sdiv
				}
			}
			.mod {
				if is_unsigned {
					OpCode.urem
				} else {
					OpCode.srem
				}
			}
			.eq {
				OpCode.eq
			}
			.ne {
				OpCode.ne
			}
			.lt {
				if is_unsigned {
					OpCode.ult
				} else {
					OpCode.lt
				}
			}
			.gt {
				if is_unsigned {
					OpCode.ugt
				} else {
					OpCode.gt
				}
			}
			.le {
				if is_unsigned {
					OpCode.ule
				} else {
					OpCode.le
				}
			}
			.ge {
				if is_unsigned {
					OpCode.uge
				} else {
					OpCode.ge
				}
			}
			.amp {
				OpCode.and_
			}
			.pipe {
				OpCode.or_
			}
			.xor {
				OpCode.xor
			}
			.left_shift {
				OpCode.shl
			}
			.right_shift {
				if is_unsigned {
					OpCode.lshr
				} else {
					OpCode.ashr
				}
			}
			else {
				OpCode.add
			}
		}
	}

	// If expr_type() returned a struct/aggregate type for an arithmetic/bitwise operation,
	// it's a stale checker position (e.g., transformer-reconstructed expressions from .map()).
	// Fall back to the LHS operand type which is always correct for arithmetic.
	mut final_type := result_type
	if forced_result_type != 0 {
		final_type = forced_result_type
	}
	if final_type > 0 && int(final_type) < b.mod.type_store.types.len {
		kind := b.mod.type_store.types[final_type].kind
		if kind == .struct_t || kind == .array_t {
			final_type = b.mod.values[lhs_v].typ
		}
	}
	// Widen result type to match operands: if either operand is wider than the
	// result type (e.g., lhs is i64 but type checker assigned i32 to the expression),
	// use the wider operand type to avoid truncation.
	if final_type > 0 && int(final_type) < b.mod.type_store.types.len {
		ft := b.mod.type_store.types[final_type]
		if ft.kind == .int_t {
			lhs_t := b.mod.values[lhs_v].typ
			rhs_t := b.mod.values[rhs_v].typ
			if lhs_t > 0 && int(lhs_t) < b.mod.type_store.types.len {
				lt := b.mod.type_store.types[lhs_t]
				if lt.kind == .int_t && lt.width > ft.width {
					final_type = lhs_t
				}
			}
			if rhs_t > 0 && int(rhs_t) < b.mod.type_store.types.len {
				rt := b.mod.type_store.types[rhs_t]
				if rt.kind == .int_t && rt.width > ft.width {
					final_type = rhs_t
				}
			}
		}
	}
	return b.mod.add_instr(op, b.cur_block, final_type, [lhs_v, rhs_v])
}

fn (mut b Builder) build_prefix(expr ast.PrefixExpr) ValueID {
	// Special case: &InitExpr (heap/pointer to struct init)
	// Build the struct via alloca+GEP+store but return the pointer instead of loading
	if expr.op == .amp && expr.expr is ast.InitExpr {
		return b.build_init_expr_ptr(expr.expr)
	}

	// Special case: &T(expr) → bitcast to *T (pointer type cast)
	// In V, &u8(val) means "cast val to type &u8", NOT "address-of (u8 cast of val)".
	// The parser interprets &u8(val) as PrefixExpr(.amp, CastExpr/CallOrCastExpr)
	// but the V semantic is always a pointer type cast.
	// Handle both pointer and integer sources (int→ptr is needed for &u8(u64_val)).
	if expr.op == .amp && expr.expr is ast.CastExpr {
		cast_expr := expr.expr as ast.CastExpr
		inner_val := b.build_expr(cast_expr.expr)
		if !b.valid_value_id(inner_val) {
			return inner_val
		}
		inner_type := b.mod.values[inner_val].typ
		if inner_type > 0 && int(inner_type) < b.mod.type_store.types.len {
			inner_t := b.mod.type_store.types[inner_type]
			if inner_t.kind == .ptr_t || inner_t.kind == .int_t {
				target_elem := b.ast_type_to_ssa(cast_expr.typ)
				ptr_type := b.mod.type_store.get_ptr(target_elem)
				return b.mod.add_instr(.bitcast, b.cur_block, ptr_type, [inner_val])
			}
		}
	}
	// Same for &CallOrCastExpr (parser uses CallOrCastExpr when it cannot distinguish
	// between a function call and a type cast, e.g. u8(expr))
	if expr.op == .amp && expr.expr is ast.CallOrCastExpr {
		coce := expr.expr as ast.CallOrCastExpr
		inner_val := b.build_expr(coce.expr)
		if !b.valid_value_id(inner_val) {
			return inner_val
		}
		inner_type := b.mod.values[inner_val].typ
		if inner_type > 0 && int(inner_type) < b.mod.type_store.types.len {
			inner_t := b.mod.type_store.types[inner_type]
			if inner_t.kind == .ptr_t || inner_t.kind == .int_t {
				target_elem := b.ast_type_to_ssa(coce.lhs)
				ptr_type := b.mod.type_store.get_ptr(target_elem)
				return b.mod.add_instr(.bitcast, b.cur_block, ptr_type, [inner_val])
			}
		}
	}
	// Handle &&T(expr) — nested ampersand pointer-type cast pattern.
	// Parser creates PrefixExpr(.amp, PrefixExpr(.amp, CallOrCastExpr/CastExpr(T, expr)))
	// for &&T(expr), meaning "cast expr to type **T", not "address-of address-of cast".
	if expr.op == .amp && expr.expr is ast.PrefixExpr {
		inner_prefix := expr.expr as ast.PrefixExpr
		if inner_prefix.op == .amp {
			if inner_prefix.expr is ast.CallOrCastExpr {
				coce := inner_prefix.expr as ast.CallOrCastExpr
				inner_val := b.build_expr(coce.expr)
				if !b.valid_value_id(inner_val) {
					return inner_val
				}
				inner_type := b.mod.values[inner_val].typ
				if inner_type > 0 && int(inner_type) < b.mod.type_store.types.len {
					inner_t := b.mod.type_store.types[inner_type]
					if inner_t.kind == .ptr_t || inner_t.kind == .int_t {
						// &&T(expr): cast to **T = ptr(ptr(T))
						target_elem := b.ast_type_to_ssa(coce.lhs)
						ptr_type := b.mod.type_store.get_ptr(target_elem)
						ptr_ptr_type := b.mod.type_store.get_ptr(ptr_type)
						return b.mod.add_instr(.bitcast, b.cur_block, ptr_ptr_type, [
							inner_val,
						])
					}
				}
			} else if inner_prefix.expr is ast.CastExpr {
				cast_expr := inner_prefix.expr as ast.CastExpr
				inner_val := b.build_expr(cast_expr.expr)
				if !b.valid_value_id(inner_val) {
					return inner_val
				}
				inner_type := b.mod.values[inner_val].typ
				if inner_type > 0 && int(inner_type) < b.mod.type_store.types.len {
					inner_t := b.mod.type_store.types[inner_type]
					if inner_t.kind == .ptr_t || inner_t.kind == .int_t {
						target_elem := b.ast_type_to_ssa(cast_expr.typ)
						ptr_type := b.mod.type_store.get_ptr(target_elem)
						ptr_ptr_type := b.mod.type_store.get_ptr(ptr_type)
						return b.mod.add_instr(.bitcast, b.cur_block, ptr_ptr_type, [
							inner_val,
						])
					}
				}
			}
		}
	}

	// Special case: negation of float literal → create negated constant directly.
	// This handles -0.0 correctly (fsub(0,0) = +0.0 per IEEE 754, but -0.0 has sign bit set).
	if expr.op == .minus && expr.expr is ast.BasicLiteral {
		lit := expr.expr as ast.BasicLiteral
		is_float_lit := lit.value.contains('.')
			|| (!lit.value.starts_with('0x') && !lit.value.starts_with('0X')
			&& (lit.value.contains('e') || lit.value.contains('E')))
		if is_float_lit {
			neg_str := '-' + lit.value
			float_type := b.mod.type_store.get_float(64)
			return b.mod.get_or_add_const(float_type, neg_str)
		}
	}

	if expr.op == .amp {
		addr := b.build_addr(expr.expr)
		if addr != 0 {
			// Sumtype `_data` stores escape the current scope. Materialize a heap copy
			// instead of passing through the address of stack-backed data.
			if b.in_sumtype_data {
				if heap_ptr := b.heap_copy_from_address(addr) {
					return heap_ptr
				}
			}
			return addr
		}
		val := b.build_expr(expr.expr)
		if !b.valid_value_id(val) {
			return val
		}
		// No addressable location (e.g. function call return value) –
		// For function references, &fn_name is just the function pointer
		// itself — no extra indirection needed.
		if b.mod.values[val].kind == .func_ref {
			return val
		}
		// For sumtype boxing, the pointee must outlive the wrapping scope.
		if b.in_sumtype_data {
			if heap_ptr := b.heap_copy_value(val) {
				return heap_ptr
			}
		}
		// For struct types, use heap allocation so the pointer survives
		// the current scope (needed for sum type boxing where _data
		// must outlive the wrapping function).
		// For scalars, use stack alloca (they're typically short-lived).
		val_type := b.mod.values[val].typ
		if val_type > 0 && int(val_type) < b.mod.type_store.types.len {
			ptr_type := b.mod.type_store.get_ptr(val_type)
			typ_info := b.mod.type_store.types[val_type]
			if typ_info.kind == .struct_t {
				// Heap-allocate struct values to ensure pointer validity
				heap_ptr := b.mod.add_instr(.heap_alloc, b.cur_block, ptr_type, []ValueID{})
				b.mod.add_instr(.store, b.cur_block, 0, [val, heap_ptr])
				return heap_ptr
			}
			alloca := b.mod.add_instr(.alloca, b.cur_block, ptr_type, []ValueID{})
			b.mod.add_instr(.store, b.cur_block, 0, [val, alloca])
			return alloca
		}
		return val
	}

	val := b.build_expr(expr.expr)
	if !b.valid_value_id(val) {
		return val
	}

	match expr.op {
		.minus {
			val_type := b.mod.values[val].typ
			is_float := val_type > 0 && int(val_type) < b.mod.type_store.types.len
				&& b.mod.type_store.types[val_type].kind == .float_t
			if is_float {
				// Float negation at runtime: XOR with sign bit mask.
				// Use integer type for XOR since it's a bit operation.
				i64_type := b.mod.type_store.get_int(64)
				sign_mask := b.mod.get_or_add_const(i64_type, '0x8000000000000000')
				int_val := b.mod.add_instr(.bitcast, b.cur_block, i64_type, [val])
				xored := b.mod.add_instr(.xor, b.cur_block, i64_type, [int_val, sign_mask])
				return b.mod.add_instr(.bitcast, b.cur_block, val_type, [xored])
			}
			if val_type <= 0 {
				return val
			}
			zero := b.mod.get_or_add_const(val_type, '0')
			return b.mod.add_instr(.sub, b.cur_block, val_type, [zero, val])
		}
		.not {
			// Logical NOT: !x → (x == 0)
			// Returns 1 if x is 0, 0 if x is non-zero
			val_type := b.mod.values[val].typ
			if val_type <= 0 {
				return val
			}
			zero := b.mod.get_or_add_const(val_type, '0')
			return b.mod.add_instr(.eq, b.cur_block, b.mod.type_store.get_int(1), [
				val,
				zero,
			])
		}
		.bit_not {
			val_type := b.mod.values[val].typ
			if val_type <= 0 {
				return val
			}
			neg_one := b.mod.get_or_add_const(val_type, '-1')
			return b.mod.add_instr(.xor, b.cur_block, val_type, [val, neg_one])
		}
		.mul {
			// Dereference: load from pointer
			val_type := b.mod.values[val].typ
			if val_type > 0 && int(val_type) < b.mod.type_store.types.len {
				typ := b.mod.type_store.types[val_type]
				if typ.kind == .ptr_t && typ.elem_type > 0 {
					result := b.mod.add_instr(.load, b.cur_block, typ.elem_type, [val])
					b.track_array_value_elem_type_from_checked_expr(result, ast.Expr(expr))
					return result
				}
			}
			return val
		}
		else {
			return val
		}
	}
}

fn (mut b Builder) build_call(expr ast.CallExpr) ValueID {
	if expr.args.len == 0 && expr.lhs is ast.SelectorExpr {
		sel := expr.lhs as ast.SelectorExpr
		if sel.lhs is ast.Ident && sel.lhs.name == 'C' && sel.rhs.name == 'mach_task_self' {
			u32_t := b.mod.type_store.get_uint(32)
			glob := b.mod.add_external_global('mach_task_self_', u32_t)
			b.global_refs['mach_task_self_'] = glob
			return b.mod.add_instr(.load, b.cur_block, u32_t, [glob])
		}
	}
	if expr.args.len == 1 {
		lhs_name := expr.lhs.name()
		if lhs_name != '' && lhs_name !in b.fn_index {
			if target_type := b.struct_types[lhs_name] {
				return b.build_cast_expr_to_type_id(expr.args[0], target_type)
			}
			qualified_lhs := '${b.cur_module}__${lhs_name}'
			if qualified_lhs !in b.fn_index {
				if target_type := b.struct_types[qualified_lhs] {
					return b.build_cast_expr_to_type_id(expr.args[0], target_type)
				}
			}
		}
	}
	return b.build_call_resolved(b.resolve_call_name(expr), expr)
}

fn (mut b Builder) call_param_type(fn_name string, param_idx int) ?TypeID {
	if param_idx < 0 {
		return none
	}
	fn_idx := b.fn_index[fn_name] or { return none }
	params := b.mod.funcs[fn_idx].params
	if param_idx >= params.len {
		return none
	}
	param_val := params[param_idx]
	if param_val <= 0 || param_val >= b.mod.values.len {
		return none
	}
	return b.mod.values[param_val].typ
}

fn (mut b Builder) call_param_count(fn_name string) int {
	fn_idx := b.fn_index[fn_name] or { return -1 }
	return b.mod.funcs[fn_idx].params.len
}

fn selector_receiver_path(expr ast.Expr) string {
	return match expr {
		ast.Ident {
			expr.name
		}
		ast.SelectorExpr {
			lhs := selector_receiver_path(expr.lhs)
			if lhs == '' {
				''
			} else {
				'${lhs}.${expr.rhs.name}'
			}
		}
		ast.ParenExpr {
			selector_receiver_path(expr.expr)
		}
		else {
			''
		}
	}
}

fn selector_receiver_path_from_flat(c ast.Cursor) string {
	return match c.kind() {
		.expr_ident {
			c.name()
		}
		.expr_selector {
			lhs := selector_receiver_path_from_flat(c.edge(0))
			rhs := c.edge(1).name()
			if lhs == '' || rhs == '' {
				''
			} else {
				'${lhs}.${rhs}'
			}
		}
		.expr_paren {
			selector_receiver_path_from_flat(c.edge(0))
		}
		else {
			''
		}
	}
}

fn (mut b Builder) build_transformed_interface_receiver_value(expr ast.Expr) ValueID {
	if expr is ast.Ident {
		if ptr := b.vars[expr.name] {
			ptr_typ := b.mod.values[ptr].typ
			if b.valid_type_id(ptr_typ) {
				elem_typ := b.mod.type_store.types[ptr_typ].elem_type
				if elem_typ > 0 {
					return b.mod.add_instr(.load, b.cur_block, elem_typ, [ptr])
				}
			}
		}
	}
	return b.build_expr(expr)
}

fn (mut b Builder) build_transformed_interface_receiver_value_from_flat(c ast.Cursor) ValueID {
	if c.kind() == .expr_ident {
		if ptr := b.vars[c.name()] {
			ptr_typ := b.mod.values[ptr].typ
			if b.valid_type_id(ptr_typ) {
				elem_typ := b.mod.type_store.types[ptr_typ].elem_type
				if elem_typ > 0 {
					return b.mod.add_instr(.load, b.cur_block, elem_typ, [ptr])
				}
			}
		}
	}
	return b.build_expr_from_flat(c)
}

fn (mut b Builder) build_mut_ptr_param_value_for_type(name string, expected_type TypeID) ?ValueID {
	if name !in b.mut_ptr_params || expected_type == 0 {
		return none
	}
	ptr := b.vars[name] or { return none }
	ptr_typ := b.mod.values[ptr].typ
	if !b.valid_type_id(ptr_typ) {
		return none
	}
	elem_typ := b.mod.type_store.types[ptr_typ].elem_type
	if elem_typ == expected_type {
		return b.mod.add_instr(.load, b.cur_block, elem_typ, [ptr])
	}
	return none
}

fn (mut b Builder) should_skip_transformed_interface_object_arg(fn_name string, receiver_expr ast.Expr, call_arg_count int, first_arg ast.Expr) bool {
	if call_arg_count == 0 || !b.selector_receiver_is_interface(receiver_expr) {
		return false
	}
	param_count := b.call_param_count(fn_name)
	if param_count < 0 || call_arg_count != param_count {
		return false
	}
	if first_arg is ast.SelectorExpr {
		receiver_path := selector_receiver_path(receiver_expr)
		object_path := selector_receiver_path(first_arg.lhs)
		return first_arg.rhs.name == '_object' && b.selector_receiver_is_interface(first_arg.lhs)
			&& receiver_path != '' && receiver_path == object_path
	}
	return false
}

fn (mut b Builder) should_skip_transformed_interface_object_arg_from_flat(fn_name string, receiver_c ast.Cursor, call_arg_count int, first_arg_c ast.Cursor) bool {
	if call_arg_count == 0 || !b.selector_receiver_is_interface_from_flat(receiver_c) {
		return false
	}
	param_count := b.call_param_count(fn_name)
	if param_count < 0 || call_arg_count != param_count {
		return false
	}
	if first_arg_c.kind() == .expr_selector {
		receiver_path := selector_receiver_path_from_flat(receiver_c)
		object_path := selector_receiver_path_from_flat(first_arg_c.edge(0))
		return first_arg_c.edge(1).name() == '_object'
			&& b.selector_receiver_is_interface_from_flat(first_arg_c.edge(0))
			&& receiver_path != '' && receiver_path == object_path
	}
	return false
}

fn (mut b Builder) call_param_array_elem_type(fn_name string, param_idx int) ?TypeID {
	if param_idx < 0 {
		return none
	}
	param_elem_types := b.fn_param_array_elem_types[fn_name] or { return none }
	if param_idx >= param_elem_types.len {
		return none
	}
	elem_type := param_elem_types[param_idx]
	if elem_type == 0 {
		return none
	}
	return elem_type
}

fn (mut b Builder) array_elem_type_from_sizeof_arg(arg ast.Expr) TypeID {
	match arg {
		ast.KeywordOperator {
			if arg.op == .key_sizeof && arg.exprs.len > 0 {
				return b.array_elem_type_from_sizeof_operand(arg.exprs[0])
			}
		}
		else {}
	}

	return 0
}

// array_elem_type_from_sizeof_arg_from_flat (s227) is the cursor mirror of
// array_elem_type_from_sizeof_arg. KeywordOperator flat encoding: op in aux,
// the operand expressions are direct child edges, so exprs[0] = edge(0).
fn (mut b Builder) array_elem_type_from_sizeof_arg_from_flat(arg_c ast.Cursor) TypeID {
	if arg_c.kind() == .expr_keyword_operator
		&& unsafe { token.Token(int(arg_c.aux())) } == .key_sizeof && arg_c.edge_count() > 0 {
		return b.array_elem_type_from_sizeof_operand_from_flat(arg_c.edge(0))
	}
	return 0
}

fn (mut b Builder) array_elem_type_from_sizeof_operand(expr ast.Expr) TypeID {
	match expr {
		ast.Ident {
			return b.ident_type_to_ssa(expr.name)
		}
		ast.SelectorExpr {
			lhs_name := expr.lhs.name()
			if lhs_name != '' && expr.rhs.name != '' {
				return b.named_type_to_ssa('${lhs_name}__${expr.rhs.name}')
			}
		}
		ast.Type {
			return b.ast_type_to_ssa(expr)
		}
		else {}
	}

	return 0
}

// array_elem_type_from_sizeof_operand_from_flat (s227) is the cursor mirror of
// array_elem_type_from_sizeof_operand. The Ident arm reads name(); the
// SelectorExpr arm reproduces `named_type_to_ssa('${lhs}__${rhs}')` (note: a
// non-Ident selector lhs yields lhs_name=='' here, which short-circuits to 0 —
// bit-identical because the AST's `${lhs.name()}__${rhs}` could only name a type
// containing a '.', which never exists, so it resolves to 0 too). The ast.Type
// arm matches the contiguous .typ_* FlatNodeKind block and delegates to
// ast_type_to_ssa_from_flat; any other expr kind returns 0 like the AST `else`.
fn (mut b Builder) array_elem_type_from_sizeof_operand_from_flat(expr_c ast.Cursor) TypeID {
	kind := expr_c.kind()
	match kind {
		.expr_ident {
			return b.ident_type_to_ssa(expr_c.name())
		}
		.expr_selector {
			lhs_c := expr_c.edge(0)
			rhs_c := expr_c.edge(1)
			lhs_name := if lhs_c.kind() == .expr_ident { lhs_c.name() } else { '' }
			rhs_name := if rhs_c.kind() == .expr_ident { rhs_c.name() } else { '' }
			if lhs_name != '' && rhs_name != '' {
				return b.named_type_to_ssa('${lhs_name}__${rhs_name}')
			}
		}
		else {
			if int(kind) >= int(ast.FlatNodeKind.typ_anon_struct)
				&& int(kind) <= int(ast.FlatNodeKind.typ_tuple) {
				return b.ast_type_to_ssa_from_flat(expr_c)
			}
		}
	}

	return 0
}

// is_new_array_sizeof_fn (s227) is the shared name predicate for the
// __new_array* / new_array_from* builtins that carry an explicit `sizeof(elem)`
// argument in slot 2. Both array_elem_type_from_new_array_call (AST) and its
// cursor variant consult it so the name list lives in exactly one place.
fn (b &Builder) is_new_array_sizeof_fn(fn_name string) bool {
	return fn_name in ['__new_array', 'builtin____new_array', '__new_array_noscan',
		'builtin____new_array_noscan', '__new_array_with_default',
		'builtin____new_array_with_default', '__new_array_with_default_noscan',
		'builtin____new_array_with_default_noscan', '__new_array_with_multi_default',
		'builtin____new_array_with_multi_default', '__new_array_with_multi_default_noscan',
		'builtin____new_array_with_multi_default_noscan', '__new_array_with_array_default',
		'builtin____new_array_with_array_default', '__new_array_with_array_default_noscan',
		'builtin____new_array_with_array_default_noscan', 'new_array_from_c_array',
		'builtin__new_array_from_c_array', 'new_array_from_c_array_noscan',
		'builtin__new_array_from_c_array_noscan', 'new_array_from_array_and_c_array',
		'builtin__new_array_from_array_and_c_array']
}

fn (mut b Builder) array_elem_type_from_new_array_call(fn_name string, expr ast.CallExpr) TypeID {
	if expr.args.len < 3 {
		return 0
	}
	if b.is_new_array_sizeof_fn(fn_name) {
		return b.array_elem_type_from_sizeof_arg(expr.args[2])
	}
	return 0
}

// array_elem_type_from_new_array_call_from_flat (s227) is the cursor mirror of
// array_elem_type_from_new_array_call. `c` is the call cursor; args[2] is edge 3
// (edge0=lhs, edge1=arg0, edge2=arg1, edge3=arg2). Returns 0 for any non-matching
// name, so it is safe to call unconditionally on the result of every call.
fn (mut b Builder) array_elem_type_from_new_array_call_from_flat(fn_name string, c ast.Cursor) TypeID {
	if c.edge_count() - 1 < 3 {
		return 0
	}
	if b.is_new_array_sizeof_fn(fn_name) {
		return b.array_elem_type_from_sizeof_arg_from_flat(c.edge(3))
	}
	return 0
}

fn (mut b Builder) build_call_arg(fn_name string, param_idx int, arg ast.Expr) ValueID {
	old_hint := b.array_literal_elem_type_hint
	old_as_element_buffer := b.array_literal_as_element_buffer
	is_map_init_buffer_arg := param_idx in [7, 8]
		&& fn_name in ['new_map_init_noscan_value', 'builtin__new_map_init_noscan_value']
	if elem_type := b.call_param_array_elem_type(fn_name, param_idx) {
		b.array_literal_elem_type_hint = elem_type
	}
	if param_idx == 1
		&& fn_name in ['array__push_noscan', 'builtin__array__push_noscan', 'builtin__array_push_noscan'] {
		b.array_literal_as_element_buffer = true
	}
	if param_idx == 3
		&& fn_name in ['new_array_from_c_array', 'builtin__new_array_from_c_array', 'new_array_from_c_array_noscan', 'builtin__new_array_from_c_array_noscan'] {
		b.array_literal_as_element_buffer = true
	}
	if is_map_init_buffer_arg {
		b.array_literal_as_element_buffer = true
	}
	if expected_type := b.call_param_type(fn_name, param_idx) {
		if arg is ast.Ident {
			if val := b.build_mut_ptr_param_value_for_type(arg.name, expected_type) {
				b.array_literal_elem_type_hint = old_hint
				b.array_literal_as_element_buffer = old_as_element_buffer
				return val
			}
		}
	}
	mut val := b.build_expr(arg)
	b.array_literal_elem_type_hint = old_hint
	b.array_literal_as_element_buffer = old_as_element_buffer
	if is_map_init_buffer_arg {
		val = b.dynamic_array_data_pointer_if_value(val)
	}
	return val
}

fn (mut b Builder) dynamic_array_data_pointer_if_value(val ValueID) ValueID {
	if !b.valid_value_id(val) || b.get_array_type() == 0 {
		return val
	}
	if b.mod.values[val].typ != b.get_array_type() {
		return val
	}
	i8_t := b.mod.type_store.get_int(8)
	void_ptr := b.mod.type_store.get_ptr(i8_t)
	idx0 := b.mod.get_or_add_const(b.mod.type_store.get_int(32), '0')
	return b.mod.add_instr(.extractvalue, b.cur_block, void_ptr, [val, idx0])
}

// build_call_arg_from_flat (s225) is the cursor-native mirror of build_call_arg:
// it sets the array-literal elem-type hint from the param type, builds the arg
// expression via build_expr_from_flat, then restores the hint. Bit-identical to
// build_call_arg(fn_name, param_idx, decode_expr(arg_c)) because build_expr_from_flat
// produces the same ValueID as build_expr(decode_expr(arg_c)) (the migration invariant).
fn (mut b Builder) build_call_arg_from_flat(fn_name string, param_idx int, arg_c ast.Cursor) ValueID {
	old_hint := b.array_literal_elem_type_hint
	old_as_element_buffer := b.array_literal_as_element_buffer
	is_map_init_buffer_arg := param_idx in [7, 8]
		&& fn_name in ['new_map_init_noscan_value', 'builtin__new_map_init_noscan_value']
	if elem_type := b.call_param_array_elem_type(fn_name, param_idx) {
		b.array_literal_elem_type_hint = elem_type
	}
	if param_idx == 1
		&& fn_name in ['array__push_noscan', 'builtin__array__push_noscan', 'builtin__array_push_noscan'] {
		b.array_literal_as_element_buffer = true
	}
	if param_idx == 3
		&& fn_name in ['new_array_from_c_array', 'builtin__new_array_from_c_array', 'new_array_from_c_array_noscan', 'builtin__new_array_from_c_array_noscan'] {
		b.array_literal_as_element_buffer = true
	}
	if is_map_init_buffer_arg {
		b.array_literal_as_element_buffer = true
	}
	if expected_type := b.call_param_type(fn_name, param_idx) {
		if arg_c.kind() == .expr_ident {
			if val := b.build_mut_ptr_param_value_for_type(arg_c.name(), expected_type) {
				b.array_literal_elem_type_hint = old_hint
				b.array_literal_as_element_buffer = old_as_element_buffer
				return val
			}
		}
	}
	mut val := b.build_expr_from_flat(arg_c)
	b.array_literal_elem_type_hint = old_hint
	b.array_literal_as_element_buffer = old_as_element_buffer
	if is_map_init_buffer_arg {
		val = b.dynamic_array_data_pointer_if_value(val)
	}
	return val
}

// build_call_resolved (s218) is the post-resolve body of `build_call`,
// taking the already-resolved `fn_name` as a parameter. The top-of-`build_call`
// struct-cast shortcut runs before resolve, so it stays in the public wrapper;
// every other branch (SelectorExpr metadata, second cast shortcut, fnptr-field,
// arg build, auto-deref/ref, indirect call, .call emit) lives here unchanged.
// `build_call_from_flat`'s Ident-lhs path computes fn_name cursor-natively via
// `resolve_call_name_ident_from_flat` and dispatches here directly, avoiding
// the redundant `resolve_call_name(expr)` re-run inside `build_call`.
fn (mut b Builder) build_call_resolved(fn_name_in string, expr ast.CallExpr) ValueID {
	mut fn_name := fn_name_in
	mut module_call_name := ''
	mut is_static_method_call := false
	mut checked_selector_method := ''
	if expr.lhs is ast.SelectorExpr {
		sel := expr.lhs as ast.SelectorExpr
		module_call_name = b.selector_module_name(sel) or { '' }
		if static_method := b.resolve_static_method_name(sel) {
			is_static_method_call = static_method == fn_name
		}
		if module_call_name == '' && !is_static_method_call {
			if typed_method := b.resolve_method_name_from_checked_type(sel.lhs, sel.rhs.name) {
				checked_selector_method = typed_method
			}
		}
	}
	method_resolved_from_checked_type := checked_selector_method != ''
		&& checked_selector_method == fn_name

	// Check if this is a type cast disguised as a call (e.g., IError(ptr)).
	// If the name is a struct type and not a registered function, treat as cast.
	if fn_name !in b.fn_index && expr.args.len == 1 {
		// Try to find a struct type matching this name
		if target_type := b.struct_types[fn_name] {
			return b.build_cast_expr_to_type_id(expr.args[0], target_type)
		}
		// Also try with module prefix
		qualified := '${b.cur_module}__${fn_name}'
		if target_type := b.struct_types[qualified] {
			return b.build_cast_expr_to_type_id(expr.args[0], target_type)
		}
		if target_type := b.call_lhs_type_to_ssa(expr.lhs) {
			return b.build_cast_expr_to_type_id(expr.args[0], target_type)
		}
	}

	if ssa_is_string_str_fn_name(fn_name) && expr.args.len == 1 {
		arg_type := b.expr_type(expr.args[0])
		if b.is_string_struct_type(arg_type) {
			return b.build_expr(expr.args[0])
		}
	}

	mut ret_type := b.expr_type(ast.Expr(expr))
	// If the function is registered, always use its declared return type.
	// This is critical for void functions like push_noscan: the checker may
	// annotate the transformed call expression with the type of the original
	// expression (e.g., array struct for `arr << val`), but the actual
	// function returns void. Using the wrong return type causes the ARM64
	// backend to emit incorrect struct-return ABI handling.
	if fn_name in b.fn_index {
		fn_idx := b.fn_index[fn_name]
		ret_type = b.mod.funcs[fn_idx].typ
	}
	// Check if this is a function pointer field call (e.g., m.hash_fn(pkey))
	// rather than a method call. If the selector field matches a struct field name
	// and the resolved function name is not in fn_index, it's a function pointer call.
	if expr.lhs is ast.SelectorExpr {
		sel := expr.lhs as ast.SelectorExpr
		if module_call_name == '' && fn_name !in b.fn_index {
			field_name := sel.rhs.name
			mut is_fnptr_field_call := b.is_struct_field(sel.lhs, field_name)
			// Interface method calls are transformed to selector-based function-pointer
			// calls (e.g. `i.msg(i._object)`), but the receiver type is `Interface`,
			// so `is_struct_field` alone returns false.
			if !is_fnptr_field_call && b.selector_receiver_is_interface(sel.lhs)
				&& b.env != unsafe { nil } {
				sel_pos := sel.pos
				if sel_pos.is_valid() {
					if sel_type := b.env.get_expr_type(sel_pos.id) {
						if sel_type is types.FnType {
							is_fnptr_field_call = true
						}
					}
				}
			}
			if is_fnptr_field_call {
				// Build the selector expression to get the function pointer value
				fn_ptr := b.build_selector(sel)
				// Recover return type for function-pointer field calls from checker
				// metadata. The transformed AST can lose direct call expression typing,
				// which would otherwise fall back to i64 and break ABI lowering.
				mut call_ret := ret_type
				if b.env != unsafe { nil } {
					lhs_pos := sel.pos
					if lhs_pos.is_valid() {
						if field_type := b.env.get_expr_type(lhs_pos.id) {
							unwrapped := b.unwrap_alias_type(field_type)
							if unwrapped is types.FnType {
								if fn_ret := unwrapped.get_return_type() {
									call_ret = b.type_to_ssa(fn_ret)
								}
							}
						}
					}
				}
				// Build arguments (no receiver - this is a field access, not a method call)
				mut fnptr_args := []ValueID{}
				for arg in expr.args {
					if arg is ast.ModifierExpr && arg.kind == .key_mut {
						addr := b.build_addr(arg.expr)
						if addr != 0 {
							fnptr_args << addr
						} else {
							fnptr_args << b.build_expr(arg)
						}
					} else {
						fnptr_args << b.build_expr(arg)
					}
				}
				mut operands := []ValueID{cap: fnptr_args.len + 1}
				operands << fn_ptr
				operands << fnptr_args
				return b.mod.add_instr(.call_indirect, b.cur_block, call_ret, operands)
			}
		}
	}

	// Build arguments
	mut args := []ValueID{}
	// For method calls, add receiver as first arg
	if expr.lhs is ast.SelectorExpr {
		sel := expr.lhs as ast.SelectorExpr
		// Check if this is a method call (not a module function call)
		if module_call_name == '' && !is_static_method_call {
			// Check if method expects pointer receiver (mut receiver)
			mut expects_ptr := false
			if fn_name in b.fn_index {
				fn_idx := b.fn_index[fn_name]
				if b.mod.funcs[fn_idx].params.len > 0 {
					param_type := b.mod.values[b.mod.funcs[fn_idx].params[0]].typ
					if param_type < b.mod.type_store.types.len
						&& b.mod.type_store.types[param_type].kind == .ptr_t {
						expects_ptr = true
					}
				}
			}
			mut receiver := if b.selector_receiver_is_interface(sel.lhs) {
				b.build_transformed_interface_receiver_value(sel.lhs)
			} else if expects_ptr {
				receiver_val := b.build_expr(sel.lhs)
				receiver_typ := b.mod.values[receiver_val].typ
				expected_receiver_typ := b.call_param_type(fn_name, 0) or { TypeID(0) }
				if expected_receiver_typ != 0 && receiver_typ == expected_receiver_typ {
					receiver_val
				} else {
					// Mut receiver: pass address (pointer to struct/array).
					addr := b.build_addr(sel.lhs)
					if addr != 0 {
						// build_addr for an Ident returns the alloca.
						// For a mut receiver variable, the alloca is ptr(ptr(Struct)),
						// storing the struct pointer. We need to load from the alloca
						// to get the actual struct pointer ptr(Struct).
						addr_typ := b.mod.values[addr].typ
						if addr_typ < b.mod.type_store.types.len
							&& b.mod.type_store.types[addr_typ].kind == .ptr_t {
							inner := b.mod.type_store.types[addr_typ].elem_type
							if inner < b.mod.type_store.types.len
								&& b.mod.type_store.types[inner].kind == .ptr_t {
								pointee := b.mod.type_store.types[inner].elem_type
								if pointee < b.mod.type_store.types.len
									&& b.mod.type_store.types[pointee].kind == .struct_t {
									// addr is ptr(ptr(Struct)) — load to get ptr(Struct)
									b.mod.add_instr(.load, b.cur_block, inner, [addr])
								} else {
									addr
								}
							} else {
								addr
							}
						} else {
							addr
						}
					} else {
						receiver_val
					}
				}
			} else {
				b.build_expr(sel.lhs)
			}
			// Auto-deref pointer receivers: if receiver is a pointer to a struct
			// but the method expects a value receiver, load through the pointer
			if !expects_ptr {
				recv_typ := b.mod.values[receiver].typ
				if recv_typ < b.mod.type_store.types.len
					&& b.mod.type_store.types[recv_typ].kind == .ptr_t {
					pointee := b.mod.type_store.types[recv_typ].elem_type
					if pointee < b.mod.type_store.types.len
						&& b.mod.type_store.types[pointee].kind == .struct_t {
						receiver = b.mod.add_instr(.load, b.cur_block, pointee, [
							receiver,
						])
					}
				}
			}
			if !method_resolved_from_checked_type {
				if actual_method := b.resolve_method_name_for_value(receiver, sel.rhs.name) {
					if actual_method != fn_name {
						fn_name = actual_method
						if fn_idx := b.fn_index[fn_name] {
							ret_type = b.mod.funcs[fn_idx].typ
						}
					}
				}
			}
			args << receiver
		}
	}
	// Determine parameter types to decide if we should pass addresses for mut receivers.
	// Read from the function signature directly instead of cloning the params
	// array; this path is used while compiling the compiler itself, so it must
	// not depend on dynamic-array element inference for local bookkeeping.
	skip_first_transformed_interface_arg := if expr.lhs is ast.SelectorExpr && expr.args.len > 0 {
		b.should_skip_transformed_interface_object_arg(fn_name, expr.lhs.lhs, expr.args.len,
			expr.args[0])
	} else {
		false
	}
	if skip_first_transformed_interface_arg && args.len > 0 && expr.args[0] is ast.SelectorExpr {
		first_arg := expr.args[0] as ast.SelectorExpr
		args[0] = b.build_transformed_interface_receiver_value(first_arg.lhs)
	}
	for arg_idx, arg in expr.args {
		if skip_first_transformed_interface_arg && arg_idx == 0 {
			continue
		}
		// For mut arguments, pass the address (pointer) instead of the value.
		// But if the value is already a pointer (e.g., &FileSet field or parameter),
		// pass it directly instead of creating an extra level of indirection.
		if arg is ast.ModifierExpr && arg.kind == .key_mut {
			// Check if the parameter expects ptr(struct)
			param_idx := args.len
			mut param_wants_ptr_to_struct := false
			if param_type := b.call_param_type(fn_name, param_idx) {
				if param_type < b.mod.type_store.types.len
					&& b.mod.type_store.types[param_type].kind == .ptr_t {
					pointee := b.mod.type_store.types[param_type].elem_type
					if pointee < b.mod.type_store.types.len
						&& b.mod.type_store.types[pointee].kind == .struct_t {
						param_wants_ptr_to_struct = true
					}
				}
			}
			if param_wants_ptr_to_struct {
				// Check if the value is already a pointer — if so, pass it directly
				val := b.build_expr(arg.expr)
				val_type := b.mod.values[val].typ
				val_is_ptr := val_type < b.mod.type_store.types.len
					&& b.mod.type_store.types[val_type].kind == .ptr_t
				if val_is_ptr {
					args << val
				} else {
					// Value type: take its address
					addr := b.build_addr(arg.expr)
					if addr != 0 {
						args << addr
					} else {
						alloca_type := b.mod.type_store.get_ptr(val_type)
						tmp_alloca := b.mod.add_instr(.alloca, b.cur_block, alloca_type,
							[]ValueID{})
						b.mod.add_instr(.store, b.cur_block, 0, [val, tmp_alloca])
						args << tmp_alloca
					}
				}
			} else {
				// Check if the parameter type is already a pointer (e.g., `mut buf &u8`).
				// If so, the function takes a plain pointer, not pointer-to-pointer,
				// so evaluate the expression directly instead of taking its address.
				param_idx2 := args.len
				mut param_is_already_ptr := false
				if pt := b.call_param_type(fn_name, param_idx2) {
					if pt < b.mod.type_store.types.len && b.mod.type_store.types[pt].kind == .ptr_t {
						pointee2 := b.mod.type_store.types[pt].elem_type
						// Only skip addr-of if pointee is NOT a struct (struct mut params need addr-of)
						if pointee2 < b.mod.type_store.types.len
							&& b.mod.type_store.types[pointee2].kind != .struct_t {
							param_is_already_ptr = true
						}
					}
				}
				if param_is_already_ptr
					|| (arg.expr is ast.PrefixExpr && (arg.expr as ast.PrefixExpr).op == .amp) {
					// Parameter already expects a pointer value (not pointer-to-value),
					// or argument explicitly provides a pointer with &.
					args << b.build_expr(arg.expr)
				} else {
					addr := b.build_addr(arg.expr)
					if addr != 0 {
						args << addr
					} else {
						val := b.build_expr(arg.expr)
						val_type := b.mod.values[val].typ
						alloca_type := b.mod.type_store.get_ptr(val_type)
						tmp_alloca := b.mod.add_instr(.alloca, b.cur_block, alloca_type,
							[]ValueID{})
						b.mod.add_instr(.store, b.cur_block, 0, [val, tmp_alloca])
						args << tmp_alloca
					}
				}
			}
		} else {
			// Use args.len as param index (accounts for receiver already in args)
			param_idx := args.len
			mut param_wants_struct_ptr := false
			if param_type := b.call_param_type(fn_name, param_idx) {
				// Only use build_addr for ptr(struct_t) params (mut receivers/params),
				// NOT for ptr(int_t), ptr(ptr_t) etc. (normal pointer params like C.puts)
				if param_type < b.mod.type_store.types.len
					&& b.mod.type_store.types[param_type].kind == .ptr_t {
					pointee := b.mod.type_store.types[param_type].elem_type
					if pointee < b.mod.type_store.types.len
						&& b.mod.type_store.types[pointee].kind == .struct_t {
						param_wants_struct_ptr = true
					}
				}
			}
			// For args where the function expects ptr(struct) (mut receiver/param),
			// try to pass the original variable address instead of a copy
			if param_wants_struct_ptr && (arg is ast.Ident || arg is ast.SelectorExpr
				|| arg is ast.IndexExpr || arg is ast.ParenExpr) {
				// First try build_expr: if it already returns ptr(struct), use it directly
				val := b.build_expr(arg)
				val_typ := b.mod.values[val].typ
				val_is_ptr := val_typ < b.mod.type_store.types.len
					&& b.mod.type_store.types[val_typ].kind == .ptr_t
				if val_is_ptr {
					// Already a pointer (e.g., mut receiver re-passing its own pointer)
					args << val
				} else {
					// Value type (local mut variable): use build_addr to get the alloca
					addr := b.build_addr(arg)
					if addr != 0 {
						args << addr
					} else {
						args << val
					}
				}
			} else {
				args << b.build_call_arg(fn_name, param_idx, arg)
			}
		}
	}

	// Auto-deref/ref arguments to match function parameter types
	if fn_name in b.fn_index {
		for ai := 0; ai < args.len; ai++ {
			param_type := b.call_param_type(fn_name, ai) or { continue }
			mut arg_type := b.mod.values[args[ai]].typ
			if arg_type < b.mod.type_store.types.len && param_type < b.mod.type_store.types.len {
				mut arg_kind := b.mod.type_store.types[arg_type].kind
				param_kind := b.mod.type_store.types[param_type].kind
				// Variant value passed to a sumtype parameter: materialize the canonical
				// sumtype wrapper so callees can read _tag/_data reliably.
				if arg_kind == .struct_t && param_kind == .struct_t && arg_type != param_type
					&& b.ssa_type_is_sumtype(param_type) {
					if wrapped := b.wrap_value_for_sumtype_target(args[ai], param_type) {
						args[ai] = wrapped
						arg_type = param_type
						arg_kind = .struct_t
					}
				}
				// Pointer arg but value param: auto-deref
				// Only auto-deref when the pointee is a struct and the
				// parameter expects that struct value. Do NOT deref raw
				// pointers (ptr(i8)/voidptr) when the param is a plain
				// int type (e.g., i64 from variadic params).
				if arg_kind == .ptr_t && param_kind == .struct_t {
					pointee := b.mod.type_store.types[arg_type].elem_type
					if pointee == param_type {
						args[ai] = b.mod.add_instr(.load, b.cur_block, pointee, [
							args[ai],
						])
					}
				}
				// Value arg but pointer param: auto-ref (alloca + store + pass pointer)
				if arg_kind == .struct_t && param_kind == .ptr_t {
					ptr_type := b.mod.type_store.get_ptr(arg_type)
					alloca := b.mod.add_instr(.alloca, b.cur_block, ptr_type, []ValueID{})
					b.mod.add_instr(.store, b.cur_block, 0, [args[ai], alloca])
					args[ai] = alloca
				}
				if arg_kind == .array_t && param_kind == .ptr_t {
					pointee := b.mod.type_store.types[param_type].elem_type
					if pointee == arg_type {
						ptr_type := b.mod.type_store.get_ptr(arg_type)
						alloca := b.mod.add_instr(.alloca, b.cur_block, ptr_type, []ValueID{})
						b.mod.add_instr(.store, b.cur_block, 0, [args[ai], alloca])
						args[ai] = alloca
					}
				}
				// Int arg but float param: auto-convert (sitofp)
				if arg_kind == .int_t && param_kind == .float_t {
					args[ai] = b.mod.add_instr(.sitofp, b.cur_block, param_type, [
						args[ai],
					])
				}
				// Scalar arg but voidptr param: auto-ref for builtin methods
				// (array insert/prepend, map delete/set/get, new_array_with_default, etc. take voidptr = "pointer to element")
				i8_t := b.mod.type_store.get_int(8)
				voidptr_t := b.mod.type_store.get_ptr(i8_t)
				if param_type == voidptr_t && arg_kind in [.int_t, .float_t]
					&& (fn_name.contains('array__') || fn_name.contains('map__')
					|| fn_name.contains('new_array')) {
					ptr_type := b.mod.type_store.get_ptr(arg_type)
					alloca := b.mod.add_instr(.alloca, b.cur_block, ptr_type, []ValueID{})
					b.mod.add_instr(.store, b.cur_block, 0, [args[ai], alloca])
					args[ai] = alloca
				}
			}
		}
	}

	// Check if fn_name is a local variable holding a function pointer
	// (e.g., `cfn(s)` where cfn is a parameter of type `fn(string) string`)
	if fn_name in b.vars {
		// The ret_type from expr_type may be wrong — it uses the expression position,
		// which for fn-by-name .map() expansion points to the function identifier
		// (typed as FnType → ptr(i8)) instead of the call return type.
		// Extract the actual return type from the FnType.
		mut call_ret := ret_type
		if expr.lhs is ast.Ident {
			lhs_pos := expr.lhs.pos
			if lhs_pos.is_valid() && b.env != unsafe { nil } {
				if var_type := b.env.get_expr_type(lhs_pos.id) {
					unwrapped := b.unwrap_alias_type(var_type)
					if unwrapped is types.FnType {
						if fn_ret := unwrapped.get_return_type() {
							call_ret = b.type_to_ssa(fn_ret)
						}
					}
				}
			}
		}
		fn_ptr := b.build_ident(ast.Ident{ name: fn_name })
		mut indirect_operands := []ValueID{cap: args.len + 1}
		indirect_operands << fn_ptr
		indirect_operands << args
		return b.mod.add_instr(.call_indirect, b.cur_block, call_ret, indirect_operands)
	}

	// Check if fn_name is a global variable holding a function pointer
	// (e.g., `__live_hot_fn(args)` where __live_hot_fn is a global voidptr)
	if glob_id := b.find_global(fn_name) {
		glob_typ := b.mod.values[glob_id].typ
		elem_typ := b.mod.type_store.types[glob_typ].elem_type
		fn_ptr := b.mod.add_instr(.load, b.cur_block, elem_typ, [glob_id])
		mut indirect_operands := []ValueID{cap: args.len + 1}
		indirect_operands << fn_ptr
		indirect_operands << args
		return b.mod.add_instr(.call_indirect, b.cur_block, ret_type, indirect_operands)
	}

	if fn_name in ['array__slice', 'array__slice_ni', 'builtin__array__slice',
		'builtin__array__slice_ni'] {
		ret_type = b.get_array_type()
	}
	fn_ref := b.get_or_create_fn_ref(fn_name, ret_type)
	mut operands := []ValueID{cap: args.len + 1}
	operands << fn_ref
	operands << args

	call_val := b.mod.add_instr(.call, b.cur_block, ret_type, operands)

	new_array_elem_type := b.array_elem_type_from_new_array_call(fn_name, expr)
	if new_array_elem_type != 0 {
		b.array_value_elem_types[call_val] = new_array_elem_type
	}
	if fn_name in ['array__slice', 'array__slice_ni', 'builtin__array__slice', 'builtin__array__slice_ni']
		&& expr.args.len > 0 {
		elem_type := b.infer_array_elem_type_from_expr(expr.args[0])
		if elem_type != 0 {
			b.array_value_elem_types[call_val] = elem_type
		}
	}
	if fn_name in ['array__clone', 'array__clone_to_depth', 'builtin__array__clone',
		'builtin__array__clone_to_depth'] {
		elem_type := b.infer_array_elem_type_from_receiver(expr)
		if elem_type != 0 {
			b.array_value_elem_types[call_val] = elem_type
		}
	}

	// array__first/last/pop/pop_left return voidptr (pointer to element).
	// Dereference the result to get the actual element value.
	if fn_name in ['array__first', 'array__last', 'array__pop', 'array__pop_left',
		'builtin__array__first', 'builtin__array__last', 'builtin__array__pop',
		'builtin__array__pop_left'] {
		elem_type := b.infer_array_elem_type_from_receiver(expr)
		if elem_type != 0 {
			elem_ptr := b.mod.type_store.get_ptr(elem_type)
			typed_ptr := b.mod.add_instr(.bitcast, b.cur_block, elem_ptr, [call_val])
			return b.mod.add_instr(.load, b.cur_block, elem_type, [typed_ptr])
		}
	}

	return call_val
}

// build_call_resolved_from_flat (s225) is the cursor-native body of build_call,
// mirroring build_call_resolved line-for-line but consuming the call cursor `c`
// (edge0 = lhs, edge1..n = args) directly. The lhs is decoded once because the
// SelectorExpr-metadata path (selector_module_name / resolve_static_method_name /
// resolve_method_name_from_checked_type / is_struct_field / build_selector /
// resolve_method_name_for_value / call_lhs_type_to_ssa) and the receiver build
// (build_addr/build_expr on sel.lhs) all pattern-match on ast.SelectorExpr — that
// single decode is identical to the lhs build_call_resolved would have received.
// Every value argument is built from its cursor via build_expr_from_flat /
// build_addr_from_flat / build_call_arg_from_flat, so the per-arg decode_expr loop
// that build_call_from_flat previously ran is gone. The array-elem-type inference
// tail still needs the original ast.CallExpr (it reads args[0]/args[2] and the
// receiver), so it is reconstructed lazily and ONLY for the precise set of array
// builtins that consult it — the common call path reconstructs nothing.
fn (mut b Builder) build_call_resolved_from_flat(fn_name_in string, c ast.Cursor) ValueID {
	lhs_c := c.edge(0)
	lhs_kind := lhs_c.kind()
	is_selector := lhs_kind == .expr_selector
	// s229: the SelectorExpr metadata, fnptr-field and receiver paths are now all
	// cursor-native (selector_module_name_from_flat / resolve_static_method_name_from_flat
	// / resolve_method_name_from_checked_type_from_flat / is_struct_field_from_flat /
	// selector_receiver_is_interface_from_flat / build_selector_from_flat /
	// build_addr_from_flat / build_expr_from_flat), so the lhs is NEVER decoded —
	// build_call_resolved_from_flat no longer calls decode_expr at all. For a
	// SelectorExpr lhs, `sel_lhs_c` is its receiver edge and `rhs_name` its rhs
	// Ident name (only read under `is_selector`).
	sel_lhs_c := lhs_c.edge(0)
	rhs_name := if is_selector && lhs_c.edge(1).kind() == .expr_ident {
		lhs_c.edge(1).name()
	} else {
		''
	}
	n_edges := c.edge_count()
	n_args := if c.kind() == .expr_call_or_cast && c.edge(1).kind() == .expr_empty {
		0
	} else {
		n_edges - 1
	}
	mut fn_name := fn_name_in
	mut module_call_name := ''
	mut is_static_method_call := false
	mut checked_selector_method := ''
	if is_selector {
		module_call_name = b.selector_module_name_from_flat(lhs_c) or { '' }
		if static_method := b.resolve_static_method_name_from_flat(lhs_c) {
			is_static_method_call = static_method == fn_name
		}
		if module_call_name == '' && !is_static_method_call {
			if typed_method := b.resolve_method_name_from_checked_type_from_flat(sel_lhs_c,
				rhs_name)
			{
				checked_selector_method = typed_method
			}
		}
	}
	method_resolved_from_checked_type := checked_selector_method != ''
		&& checked_selector_method == fn_name

	// Type cast disguised as a call (e.g. IError(ptr)). build_call_from_flat's
	// fast paths already cover n_args==1 before dispatching here, but replicate
	// the legacy shortcut so this body is correct for any caller; the conditions
	// match build_call_resolved exactly, using the first arg cursor c.edge(1).
	if fn_name !in b.fn_index && n_args == 1 {
		if target_type := b.struct_types[fn_name] {
			return b.build_cast_expr_to_type_id_from_flat(c.edge(1), target_type)
		}
		qualified := '${b.cur_module}__${fn_name}'
		if target_type := b.struct_types[qualified] {
			return b.build_cast_expr_to_type_id_from_flat(c.edge(1), target_type)
		}
		if target_type := b.call_lhs_type_to_ssa_from_flat(lhs_c) {
			return b.build_cast_expr_to_type_id_from_flat(c.edge(1), target_type)
		}
	}

	if ssa_is_string_str_fn_name(fn_name) && n_args == 1 {
		arg_c := c.edge(1)
		arg_type := b.expr_type_from_flat(arg_c)
		if b.is_string_struct_type(arg_type) {
			return b.build_expr_from_flat(arg_c)
		}
	}

	mut ret_type := b.expr_type_from_flat(c)
	if fn_name in b.fn_index {
		fn_idx := b.fn_index[fn_name]
		ret_type = b.mod.funcs[fn_idx].typ
	}
	// Function-pointer field call (e.g. m.hash_fn(pkey)) rather than a method call.
	if is_selector {
		if module_call_name == '' && fn_name !in b.fn_index {
			field_name := rhs_name
			mut is_fnptr_field_call := b.is_struct_field_from_flat(sel_lhs_c, field_name)
			if !is_fnptr_field_call && b.selector_receiver_is_interface_from_flat(sel_lhs_c)
				&& b.env != unsafe { nil } {
				sel_pos := lhs_c.pos()
				if sel_pos.is_valid() {
					if sel_type := b.env.get_expr_type(sel_pos.id) {
						if sel_type is types.FnType {
							is_fnptr_field_call = true
						}
					}
				}
			}
			if is_fnptr_field_call {
				fn_ptr := b.build_selector_from_flat(lhs_c)
				mut call_ret := ret_type
				if b.env != unsafe { nil } {
					lhs_pos := lhs_c.pos()
					if lhs_pos.is_valid() {
						if field_type := b.env.get_expr_type(lhs_pos.id) {
							unwrapped := b.unwrap_alias_type(field_type)
							if unwrapped is types.FnType {
								if fn_ret := unwrapped.get_return_type() {
									call_ret = b.type_to_ssa(fn_ret)
								}
							}
						}
					}
				}
				// Build arguments (no receiver - this is a field access, not a method call)
				mut fnptr_args := []ValueID{}
				for arg_i in 0 .. n_args {
					arg_c := c.edge(1 + arg_i)
					if arg_c.kind() == .expr_modifier
						&& unsafe { token.Token(int(arg_c.aux())) } == .key_mut {
						addr := b.build_addr_from_flat(arg_c.edge(0))
						if addr != 0 {
							fnptr_args << addr
						} else {
							fnptr_args << b.build_expr_from_flat(arg_c)
						}
					} else {
						fnptr_args << b.build_expr_from_flat(arg_c)
					}
				}
				mut operands := []ValueID{cap: fnptr_args.len + 1}
				operands << fn_ptr
				operands << fnptr_args
				return b.mod.add_instr(.call_indirect, b.cur_block, call_ret, operands)
			}
		}
	}

	// Build arguments
	mut args := []ValueID{}
	// For method calls, add receiver as first arg
	if is_selector {
		if module_call_name == '' && !is_static_method_call {
			mut expects_ptr := false
			if fn_name in b.fn_index {
				fn_idx := b.fn_index[fn_name]
				if b.mod.funcs[fn_idx].params.len > 0 {
					param_type := b.mod.values[b.mod.funcs[fn_idx].params[0]].typ
					if param_type < b.mod.type_store.types.len
						&& b.mod.type_store.types[param_type].kind == .ptr_t {
						expects_ptr = true
					}
				}
			}
			mut receiver := if b.selector_receiver_is_interface_from_flat(sel_lhs_c) {
				b.build_transformed_interface_receiver_value_from_flat(sel_lhs_c)
			} else if expects_ptr {
				addr := b.build_addr_from_flat(sel_lhs_c)
				if addr != 0 {
					addr_typ := b.mod.values[addr].typ
					if addr_typ < b.mod.type_store.types.len
						&& b.mod.type_store.types[addr_typ].kind == .ptr_t {
						inner := b.mod.type_store.types[addr_typ].elem_type
						if inner < b.mod.type_store.types.len
							&& b.mod.type_store.types[inner].kind == .ptr_t {
							pointee := b.mod.type_store.types[inner].elem_type
							if pointee < b.mod.type_store.types.len
								&& b.mod.type_store.types[pointee].kind == .struct_t {
								b.mod.add_instr(.load, b.cur_block, inner, [addr])
							} else {
								addr
							}
						} else {
							addr
						}
					} else {
						addr
					}
				} else {
					b.build_expr_from_flat(sel_lhs_c)
				}
			} else {
				b.build_expr_from_flat(sel_lhs_c)
			}
			if !expects_ptr {
				recv_typ := b.mod.values[receiver].typ
				if recv_typ < b.mod.type_store.types.len
					&& b.mod.type_store.types[recv_typ].kind == .ptr_t {
					pointee := b.mod.type_store.types[recv_typ].elem_type
					if pointee < b.mod.type_store.types.len
						&& b.mod.type_store.types[pointee].kind == .struct_t {
						receiver = b.mod.add_instr(.load, b.cur_block, pointee, [
							receiver,
						])
					}
				}
			}
			if !method_resolved_from_checked_type {
				if actual_method := b.resolve_method_name_for_value(receiver, rhs_name) {
					if actual_method != fn_name {
						fn_name = actual_method
						if fn_idx := b.fn_index[fn_name] {
							ret_type = b.mod.funcs[fn_idx].typ
						}
					}
				}
			}
			args << receiver
		}
	}
	skip_first_transformed_interface_arg := if is_selector && n_args > 0 {
		b.should_skip_transformed_interface_object_arg_from_flat(fn_name, sel_lhs_c, n_args,
			c.edge(1))
	} else {
		false
	}
	if skip_first_transformed_interface_arg && args.len > 0 && c.edge(1).kind() == .expr_selector {
		args[0] = b.build_transformed_interface_receiver_value_from_flat(c.edge(1).edge(0))
	}
	for arg_i in 0 .. n_args {
		if skip_first_transformed_interface_arg && arg_i == 0 {
			continue
		}
		arg_c := c.edge(1 + arg_i)
		// For mut arguments, pass the address (pointer) instead of the value.
		if arg_c.kind() == .expr_modifier && unsafe { token.Token(int(arg_c.aux())) } == .key_mut {
			inner_c := arg_c.edge(0)
			param_idx := args.len
			mut param_wants_ptr_to_struct := false
			if param_type := b.call_param_type(fn_name, param_idx) {
				if param_type < b.mod.type_store.types.len
					&& b.mod.type_store.types[param_type].kind == .ptr_t {
					pointee := b.mod.type_store.types[param_type].elem_type
					if pointee < b.mod.type_store.types.len
						&& b.mod.type_store.types[pointee].kind == .struct_t {
						param_wants_ptr_to_struct = true
					}
				}
			}
			if param_wants_ptr_to_struct {
				val := b.build_expr_from_flat(inner_c)
				val_type := b.mod.values[val].typ
				val_is_ptr := val_type < b.mod.type_store.types.len
					&& b.mod.type_store.types[val_type].kind == .ptr_t
				if val_is_ptr {
					args << val
				} else {
					addr := b.build_addr_from_flat(inner_c)
					if addr != 0 {
						args << addr
					} else {
						alloca_type := b.mod.type_store.get_ptr(val_type)
						tmp_alloca := b.mod.add_instr(.alloca, b.cur_block, alloca_type,
							[]ValueID{})
						b.mod.add_instr(.store, b.cur_block, 0, [val, tmp_alloca])
						args << tmp_alloca
					}
				}
			} else {
				param_idx2 := args.len
				mut param_is_already_ptr := false
				if pt := b.call_param_type(fn_name, param_idx2) {
					if pt < b.mod.type_store.types.len && b.mod.type_store.types[pt].kind == .ptr_t {
						pointee2 := b.mod.type_store.types[pt].elem_type
						if pointee2 < b.mod.type_store.types.len
							&& b.mod.type_store.types[pointee2].kind != .struct_t {
							param_is_already_ptr = true
						}
					}
				}
				if param_is_already_ptr || (inner_c.kind() == .expr_prefix
					&& unsafe { token.Token(int(inner_c.aux())) } == .amp) {
					args << b.build_expr_from_flat(inner_c)
				} else {
					addr := b.build_addr_from_flat(inner_c)
					if addr != 0 {
						args << addr
					} else {
						val := b.build_expr_from_flat(inner_c)
						val_type := b.mod.values[val].typ
						alloca_type := b.mod.type_store.get_ptr(val_type)
						tmp_alloca := b.mod.add_instr(.alloca, b.cur_block, alloca_type,
							[]ValueID{})
						b.mod.add_instr(.store, b.cur_block, 0, [val, tmp_alloca])
						args << tmp_alloca
					}
				}
			}
		} else {
			param_idx := args.len
			mut param_wants_struct_ptr := false
			if param_type := b.call_param_type(fn_name, param_idx) {
				if param_type < b.mod.type_store.types.len
					&& b.mod.type_store.types[param_type].kind == .ptr_t {
					pointee := b.mod.type_store.types[param_type].elem_type
					if pointee < b.mod.type_store.types.len
						&& b.mod.type_store.types[pointee].kind == .struct_t {
						param_wants_struct_ptr = true
					}
				}
			}
			if param_wants_struct_ptr
				&& arg_c.kind() in [.expr_ident, .expr_selector, .expr_index, .expr_paren] {
				val := b.build_expr_from_flat(arg_c)
				val_typ := b.mod.values[val].typ
				val_is_ptr := val_typ < b.mod.type_store.types.len
					&& b.mod.type_store.types[val_typ].kind == .ptr_t
				if val_is_ptr {
					args << val
				} else {
					addr := b.build_addr_from_flat(arg_c)
					if addr != 0 {
						args << addr
					} else {
						args << val
					}
				}
			} else {
				args << b.build_call_arg_from_flat(fn_name, param_idx, arg_c)
			}
		}
	}

	// Auto-deref/ref arguments to match function parameter types
	if fn_name in b.fn_index {
		for ai := 0; ai < args.len; ai++ {
			param_type := b.call_param_type(fn_name, ai) or { continue }
			arg_type := b.mod.values[args[ai]].typ
			if arg_type < b.mod.type_store.types.len && param_type < b.mod.type_store.types.len {
				arg_kind := b.mod.type_store.types[arg_type].kind
				param_kind := b.mod.type_store.types[param_type].kind
				if arg_kind == .ptr_t && param_kind == .struct_t {
					pointee := b.mod.type_store.types[arg_type].elem_type
					if pointee == param_type {
						args[ai] = b.mod.add_instr(.load, b.cur_block, pointee, [
							args[ai],
						])
					}
				}
				if arg_kind == .struct_t && param_kind == .ptr_t {
					ptr_type := b.mod.type_store.get_ptr(arg_type)
					alloca := b.mod.add_instr(.alloca, b.cur_block, ptr_type, []ValueID{})
					b.mod.add_instr(.store, b.cur_block, 0, [args[ai], alloca])
					args[ai] = alloca
				}
				if arg_kind == .array_t && param_kind == .ptr_t {
					pointee := b.mod.type_store.types[param_type].elem_type
					if pointee == arg_type {
						ptr_type := b.mod.type_store.get_ptr(arg_type)
						alloca := b.mod.add_instr(.alloca, b.cur_block, ptr_type, []ValueID{})
						b.mod.add_instr(.store, b.cur_block, 0, [args[ai], alloca])
						args[ai] = alloca
					}
				}
				if arg_kind == .int_t && param_kind == .float_t {
					args[ai] = b.mod.add_instr(.sitofp, b.cur_block, param_type, [
						args[ai],
					])
				}
				i8_t := b.mod.type_store.get_int(8)
				voidptr_t := b.mod.type_store.get_ptr(i8_t)
				if param_type == voidptr_t && arg_kind in [.int_t, .float_t]
					&& (fn_name.contains('array__') || fn_name.contains('map__')
					|| fn_name.contains('new_array')) {
					ptr_type := b.mod.type_store.get_ptr(arg_type)
					alloca := b.mod.add_instr(.alloca, b.cur_block, ptr_type, []ValueID{})
					b.mod.add_instr(.store, b.cur_block, 0, [args[ai], alloca])
					args[ai] = alloca
				}
			}
		}
	}

	// Check if fn_name is a local variable holding a function pointer.
	if fn_name in b.vars {
		mut call_ret := ret_type
		if lhs_kind == .expr_ident {
			lhs_pos := lhs_c.pos()
			if lhs_pos.is_valid() && b.env != unsafe { nil } {
				if var_type := b.env.get_expr_type(lhs_pos.id) {
					unwrapped := b.unwrap_alias_type(var_type)
					if unwrapped is types.FnType {
						if fn_ret := unwrapped.get_return_type() {
							call_ret = b.type_to_ssa(fn_ret)
						}
					}
				}
			}
		}
		fn_ptr := b.build_ident(ast.Ident{ name: fn_name })
		mut indirect_operands := []ValueID{cap: args.len + 1}
		indirect_operands << fn_ptr
		indirect_operands << args
		return b.mod.add_instr(.call_indirect, b.cur_block, call_ret, indirect_operands)
	}

	// Check if fn_name is a global variable holding a function pointer.
	if glob_id := b.find_global(fn_name) {
		glob_typ := b.mod.values[glob_id].typ
		elem_typ := b.mod.type_store.types[glob_typ].elem_type
		fn_ptr := b.mod.add_instr(.load, b.cur_block, elem_typ, [glob_id])
		mut indirect_operands := []ValueID{cap: args.len + 1}
		indirect_operands << fn_ptr
		indirect_operands << args
		return b.mod.add_instr(.call_indirect, b.cur_block, ret_type, indirect_operands)
	}

	if fn_name in ['array__slice', 'array__slice_ni', 'builtin__array__slice',
		'builtin__array__slice_ni'] {
		ret_type = b.get_array_type()
	}
	fn_ref := b.get_or_create_fn_ref(fn_name, ret_type)
	mut operands := []ValueID{cap: args.len + 1}
	operands << fn_ref
	operands << args

	call_val := b.mod.add_instr(.call, b.cur_block, ret_type, operands)

	// Array-elem-type inference tail — fully cursor-native (s226 + s227). Every
	// branch resolves element types straight from cursors; no argument is ever
	// decoded. array_elem_type_from_new_array_call_from_flat returns 0 for every
	// non-new_array name, so it runs unconditionally exactly like the AST path.
	new_array_elem_type := b.array_elem_type_from_new_array_call_from_flat(fn_name, c)
	if new_array_elem_type != 0 {
		b.array_value_elem_types[call_val] = new_array_elem_type
	}
	if fn_name in ['array__slice', 'array__slice_ni', 'builtin__array__slice', 'builtin__array__slice_ni']
		&& n_args > 0 {
		elem_type := b.infer_array_elem_type_from_expr_from_flat(c.edge(1))
		if elem_type != 0 {
			b.array_value_elem_types[call_val] = elem_type
		}
	}
	if fn_name in ['array__clone', 'array__clone_to_depth', 'builtin__array__clone',
		'builtin__array__clone_to_depth'] {
		elem_type := b.infer_array_elem_type_from_receiver_from_flat(c)
		if elem_type != 0 {
			b.array_value_elem_types[call_val] = elem_type
		}
	}
	if fn_name in ['array__first', 'array__last', 'array__pop', 'array__pop_left',
		'builtin__array__first', 'builtin__array__last', 'builtin__array__pop',
		'builtin__array__pop_left'] {
		elem_type := b.infer_array_elem_type_from_receiver_from_flat(c)
		if elem_type != 0 {
			elem_ptr := b.mod.type_store.get_ptr(elem_type)
			typed_ptr := b.mod.add_instr(.bitcast, b.cur_block, elem_ptr, [call_val])
			return b.mod.add_instr(.load, b.cur_block, elem_type, [typed_ptr])
		}
	}

	return call_val
}

// infer_array_elem_type_from_receiver infers the element type of an array
// receiver for array methods like first/last/pop.
fn (mut b Builder) infer_array_elem_type_from_receiver(expr ast.CallExpr) TypeID {
	if b.env == unsafe { nil } {
		return 0
	}
	if expr.lhs is ast.SelectorExpr {
		sel := expr.lhs as ast.SelectorExpr
		elem_type := b.infer_array_elem_type_from_expr(sel.lhs)
		if elem_type != 0 {
			return elem_type
		}
	}
	if expr.args.len > 0 {
		elem_type := b.infer_array_elem_type_from_expr(expr.args[0])
		if elem_type != 0 {
			return elem_type
		}
	}
	return 0
}

fn (mut b Builder) infer_array_elem_type_from_expr(receiver ast.Expr) TypeID {
	if arr_typ := b.get_checked_expr_type(receiver) {
		inferred := b.unwrap_to_array_elem_ssa(arr_typ)
		if inferred != 0 {
			return inferred
		}
	}
	match receiver {
		ast.Ident {
			if elem_type := b.array_elem_types[receiver.name] {
				return elem_type
			}
		}
		ast.ModifierExpr {
			return b.infer_array_elem_type_from_expr(receiver.expr)
		}
		ast.ParenExpr {
			return b.infer_array_elem_type_from_expr(receiver.expr)
		}
		ast.PrefixExpr {
			if receiver.op == .amp {
				return b.infer_array_elem_type_from_expr(receiver.expr)
			}
		}
		else {}
	}

	return 0
}

// infer_array_elem_type_from_expr_from_flat (s226) is the cursor-native mirror
// of infer_array_elem_type_from_expr. get_checked_expr_type_from_flat reads the
// same pos.id the decoded receiver would carry, so the checked-type fast path is
// bit-identical; the Ident/Modifier/Paren/Prefix arms read name()/edge(0) exactly
// as the AST match reads receiver.name/receiver.expr.
fn (mut b Builder) infer_array_elem_type_from_expr_from_flat(receiver_c ast.Cursor) TypeID {
	if arr_typ := b.get_checked_expr_type_from_flat(receiver_c) {
		inferred := b.unwrap_to_array_elem_ssa(arr_typ)
		if inferred != 0 {
			return inferred
		}
	}
	match receiver_c.kind() {
		.expr_ident {
			if elem_type := b.array_elem_types[receiver_c.name()] {
				return elem_type
			}
		}
		.expr_modifier {
			return b.infer_array_elem_type_from_expr_from_flat(receiver_c.edge(0))
		}
		.expr_paren {
			return b.infer_array_elem_type_from_expr_from_flat(receiver_c.edge(0))
		}
		.expr_prefix {
			if unsafe { token.Token(int(receiver_c.aux())) } == .amp {
				return b.infer_array_elem_type_from_expr_from_flat(receiver_c.edge(0))
			}
		}
		else {}
	}

	return 0
}

// infer_array_elem_type_from_receiver_from_flat (s226) is the cursor-native
// mirror of infer_array_elem_type_from_receiver. `c` is the call cursor
// (edge0 = lhs, edge1 = first arg). For a SelectorExpr lhs, the receiver is the
// selector's own lhs = lhs_c.edge(0); the first arg is c.edge(1).
fn (mut b Builder) infer_array_elem_type_from_receiver_from_flat(c ast.Cursor) TypeID {
	if b.env == unsafe { nil } {
		return 0
	}
	lhs_c := c.edge(0)
	if lhs_c.kind() == .expr_selector {
		elem_type := b.infer_array_elem_type_from_expr_from_flat(lhs_c.edge(0))
		if elem_type != 0 {
			return elem_type
		}
	}
	if c.edge_count() > 1 {
		elem_type := b.infer_array_elem_type_from_expr_from_flat(c.edge(1))
		if elem_type != 0 {
			return elem_type
		}
	}
	return 0
}

fn (mut b Builder) selector_module_name(sel ast.SelectorExpr) ?string {
	if sel.lhs !is ast.Ident {
		return none
	}
	mod_ident := sel.lhs as ast.Ident
	if mod_ident.name == 'C' {
		return 'C'
	}
	if b.env != unsafe { nil } {
		if scope := b.env.get_scope(b.cur_module) {
			if obj := scope.lookup_parent(mod_ident.name, 0) {
				if obj is types.Module {
					return obj.name.replace('.', '_')
				}
				return none
			}
		}
	}
	qualified := '${mod_ident.name}__${sel.rhs.name}'
	if qualified in b.fn_index {
		return mod_ident.name
	}
	return none
}

fn (mut b Builder) is_module_name(expr ast.Expr) bool {
	if expr is ast.Ident {
		if b.env != unsafe { nil } {
			if scope := b.env.get_scope(b.cur_module) {
				if obj := scope.lookup_parent(expr.name, 0) {
					return obj is types.Module
				}
			}
		}
	}
	return false
}

fn (mut b Builder) resolve_static_method_name(sel ast.SelectorExpr) ?string {
	receiver_type := b.static_receiver_type_name(sel.lhs) or { return none }
	method_name := '${receiver_type}__${sel.rhs.name}'
	if method_name in b.fn_index {
		return method_name
	}
	builtin_method := 'builtin__${method_name}'
	if builtin_method in b.fn_index {
		return builtin_method
	}
	if receiver_type.contains('__') {
		short_receiver := receiver_type.all_after_last('__')
		short_method := '${short_receiver}__${sel.rhs.name}'
		if short_method in b.fn_index {
			return short_method
		}
		builtin_short_method := 'builtin__${short_method}'
		if builtin_short_method in b.fn_index {
			return builtin_short_method
		}
	}
	return none
}

fn (mut b Builder) static_receiver_type_name(expr ast.Expr) ?string {
	match expr {
		ast.Ident {
			if !looks_like_type_name(expr.name) {
				return none
			}
			if b.cur_module != '' && b.cur_module != 'main' {
				qualified := '${b.cur_module}__${expr.name}'
				if b.known_type_name(qualified) {
					return qualified
				}
			}
			if b.known_type_name(expr.name) {
				return expr.name
			}
			if b.cur_module != '' && b.cur_module != 'main' {
				return '${b.cur_module}__${expr.name}'
			}
			return expr.name
		}
		ast.SelectorExpr {
			if expr.lhs is ast.Ident {
				mod_name := (expr.lhs as ast.Ident).name.replace('.', '_')
				if !looks_like_type_name(expr.rhs.name) {
					return none
				}
				qualified := '${mod_name}__${expr.rhs.name}'
				if b.known_type_name(qualified) {
					return qualified
				}
				return qualified
			}
			return none
		}
		else {
			return none
		}
	}
}

fn looks_like_type_name(name string) bool {
	return name.len > 0 && name[0] >= `A` && name[0] <= `Z`
}

fn is_builtin_cast_type_name(name string) bool {
	return name in ['int', 'i8', 'i16', 'i32', 'i64', 'u8', 'u16', 'u32', 'u64', 'f32', 'f64',
		'bool', 'byte', 'char', 'rune', 'usize', 'isize', 'string', 'byteptr', 'charptr', 'voidptr']
}

fn (mut b Builder) known_type_name(name string) bool {
	if name in b.struct_types || b.is_enum_type(name) {
		return true
	}
	if b.env != unsafe { nil } {
		mut lookup_module := b.cur_module
		mut lookup_name := name
		if idx := name.index('__') {
			lookup_module = name[..idx]
			if last_idx := name.last_index('__') {
				lookup_name = name[last_idx + 2..]
			}
		}
		if scope := b.env.get_scope(lookup_module) {
			if obj := scope.lookup_parent(lookup_name, 0) {
				return obj is types.Type
			}
		}
	}
	return false
}

fn (mut b Builder) cast_lhs_receiver_type_name(lhs ast.Expr) ?string {
	match lhs {
		ast.Type {
			name := b.type_id_to_receiver_name(b.ast_type_to_ssa(lhs))
			if name != 'unknown' {
				return name
			}
		}
		ast.Ident {
			if is_builtin_cast_type_name(lhs.name) {
				return lhs.name
			}
			if lhs.name in b.fn_index && !b.known_type_name(lhs.name) {
				return none
			}
			if b.cur_module != '' && b.cur_module != 'main' {
				qualified := '${b.cur_module}__${lhs.name}'
				if b.known_type_name(qualified) {
					return qualified
				}
			}
			if b.known_type_name(lhs.name) {
				return lhs.name
			}
		}
		ast.SelectorExpr {
			return b.static_receiver_type_name(lhs)
		}
		ast.ParenExpr {
			return b.cast_lhs_receiver_type_name(lhs.expr)
		}
		ast.ModifierExpr {
			return b.cast_lhs_receiver_type_name(lhs.expr)
		}
		ast.PrefixExpr {
			return b.cast_lhs_receiver_type_name(lhs.expr)
		}
		else {}
	}

	return none
}

// receiver_type_name_from_cast_type_from_flat (s220) cursor mirror of
// `receiver_type_name_from_cast_type`. Cursor argument points at the type
// expression (the `typ` of a CastExpr — `c.edge(0)` for `.expr_cast`).
// Resolves through `ast_type_to_ssa_from_flat` + ptr-unwrap + `type_id_to_receiver_name`.
fn (mut b Builder) receiver_type_name_from_cast_type_from_flat(c ast.Cursor) string {
	mut typ_id := b.ast_type_to_ssa_from_flat(c)
	if typ_id > 0 && int(typ_id) < b.mod.type_store.types.len {
		ssa_typ := b.mod.type_store.types[typ_id]
		if ssa_typ.kind == .ptr_t && ssa_typ.elem_type != 0 {
			typ_id = ssa_typ.elem_type
		}
	}
	return b.type_id_to_receiver_name(typ_id)
}

// cast_lhs_receiver_type_name_from_flat (s220) cursor mirror of
// `cast_lhs_receiver_type_name`. Receives the lhs cursor. Mirrors the
// legacy match arms: Ident (builtin cast / known type / module-qualified),
// SelectorExpr (delegate to static_receiver_type_name_from_flat), ParenExpr
// / ModifierExpr / PrefixExpr (recurse on edge(0)). For complex type kinds
// (ArrayType, FnType, etc. — encoded as their own .expr_* kinds), falls
// through to `ast_type_to_ssa_from_flat` + `type_id_to_receiver_name`.
fn (mut b Builder) cast_lhs_receiver_type_name_from_flat(c ast.Cursor) ?string {
	match c.kind() {
		.expr_ident {
			name := c.name()
			if is_builtin_cast_type_name(name) {
				return name
			}
			if name in b.fn_index && !b.known_type_name(name) {
				return none
			}
			if b.cur_module != '' && b.cur_module != 'main' {
				qualified := '${b.cur_module}__${name}'
				if b.known_type_name(qualified) {
					return qualified
				}
			}
			if b.known_type_name(name) {
				return name
			}
		}
		.expr_selector {
			return b.static_receiver_type_name_from_flat(c)
		}
		.expr_paren, .expr_modifier {
			return b.cast_lhs_receiver_type_name_from_flat(c.edge(0))
		}
		.expr_prefix {
			return b.cast_lhs_receiver_type_name_from_flat(c.edge(0))
		}
		else {
			name := b.receiver_type_name_from_cast_type_from_flat(c)
			if name != 'unknown' {
				return name
			}
		}
	}

	return none
}

// explicit_cast_receiver_type_name_from_flat (s220) cursor mirror of
// `explicit_cast_receiver_type_name`. Recognises explicit cast patterns at
// the receiver position: `Type(x).method()`, `*Type(p).method()`,
// `Type(x) /* CallOrCast */.method()`, plus paren/modifier wrappers.
// Recurses through PrefixExpr/ParenExpr/ModifierExpr via edge(0); CastExpr
// extracts the type via `receiver_type_name_from_cast_type_from_flat(edge(0))`;
// CallOrCastExpr delegates the lhs to `cast_lhs_receiver_type_name_from_flat`.
fn (mut b Builder) explicit_cast_receiver_type_name_from_flat(c ast.Cursor) ?string {
	match c.kind() {
		.expr_cast {
			name := b.receiver_type_name_from_cast_type_from_flat(c.edge(0))
			if name != 'unknown' {
				return name
			}
		}
		.expr_prefix {
			op := unsafe { token.Token(int(c.aux())) }
			inner := c.edge(0)
			if op == .mul {
				if inner.kind() == .expr_cast {
					name := b.receiver_type_name_from_cast_type_from_flat(inner.edge(0))
					if name != 'unknown' {
						return name
					}
				}
			}
			return b.explicit_cast_receiver_type_name_from_flat(inner)
		}
		.expr_call_or_cast {
			inner_expr := c.edge(1)
			if inner_expr.kind() == .expr_empty {
				return none
			}
			return b.cast_lhs_receiver_type_name_from_flat(c.edge(0))
		}
		.expr_paren, .expr_modifier {
			return b.explicit_cast_receiver_type_name_from_flat(c.edge(0))
		}
		else {}
	}

	return none
}

// resolve_method_name_from_checked_type_from_flat (s220) cursor mirror of
// `resolve_method_name_from_checked_type`. Receiver cursor → either an
// explicit-cast-derived receiver type (e.g., `Type(x).method()`) or env's
// checked type at the receiver position. Delegates to the type-only helper
// `resolve_method_name_for_type` (no AST) for the env path.
fn (mut b Builder) resolve_method_name_from_checked_type_from_flat(receiver_c ast.Cursor, method_name string) ?string {
	if receiver_type_name := b.explicit_cast_receiver_type_name_from_flat(receiver_c) {
		if method := b.resolve_existing_method_name(receiver_type_name, method_name) {
			return method
		}
	}
	if b.env == unsafe { nil } {
		return none
	}
	receiver_type := b.get_checked_expr_type_from_flat(receiver_c) or { return none }
	return b.resolve_method_name_for_type(receiver_type, method_name)
}

fn (mut b Builder) explicit_cast_receiver_type_name(receiver ast.Expr) ?string {
	match receiver {
		ast.CastExpr {
			name := b.receiver_type_name_from_cast_type(receiver.typ)
			if name != 'unknown' {
				return name
			}
		}
		ast.PrefixExpr {
			if receiver.op == .mul {
				if receiver.expr is ast.CastExpr {
					name := b.receiver_type_name_from_cast_type((receiver.expr as ast.CastExpr).typ)
					if name != 'unknown' {
						return name
					}
				}
			}
			return b.explicit_cast_receiver_type_name(receiver.expr)
		}
		ast.CallOrCastExpr {
			if receiver.expr is ast.EmptyExpr {
				return none
			}
			return b.cast_lhs_receiver_type_name(receiver.lhs)
		}
		ast.ParenExpr {
			return b.explicit_cast_receiver_type_name(receiver.expr)
		}
		ast.ModifierExpr {
			return b.explicit_cast_receiver_type_name(receiver.expr)
		}
		else {}
	}

	return none
}

fn (mut b Builder) receiver_type_name_from_cast_type(typ ast.Expr) string {
	mut typ_id := b.ast_type_to_ssa(typ)
	if typ_id > 0 && int(typ_id) < b.mod.type_store.types.len {
		ssa_typ := b.mod.type_store.types[typ_id]
		if ssa_typ.kind == .ptr_t && ssa_typ.elem_type != 0 {
			typ_id = ssa_typ.elem_type
		}
	}
	return b.type_id_to_receiver_name(typ_id)
}

fn (mut b Builder) resolve_method_name_from_checked_type(receiver ast.Expr, method_name string) ?string {
	if receiver_type_name := b.explicit_cast_receiver_type_name(receiver) {
		if method := b.resolve_existing_method_name(receiver_type_name, method_name) {
			return method
		}
	}
	if b.env == unsafe { nil } {
		return none
	}
	receiver_type := b.get_checked_expr_type(receiver) or { return none }
	return b.resolve_method_name_for_type(receiver_type, method_name)
}

fn (mut b Builder) resolve_method_name_for_type(typ types.Type, method_name string) ?string {
	if !types.type_has_valid_payload(typ) {
		return none
	}
	match typ {
		types.Pointer {
			return b.resolve_method_name_for_type(typ.base_type, method_name)
		}
		types.Alias {
			alias_name := if typ.name != '' { typ.name } else { b.types_type_c_name(typ) }
			if method := b.resolve_existing_method_name(alias_name, method_name) {
				return method
			}
			base := b.resolve_alias_base_type(typ) or { return none }
			return b.resolve_method_name_for_type(base, method_name)
		}
		types.Array {
			elem_name := b.types_type_c_name(typ.elem_type)
			byte_elem_name := if elem_name == 'i8' { 'u8' } else { elem_name }
			if byte_elem_name == 'u8' && method_name != 'free' {
				if method := b.resolve_existing_method_name('strings__Builder', method_name) {
					return method
				}
			}
			if method := b.resolve_existing_method_name('Array_${byte_elem_name}', method_name) {
				return method
			}
			return b.resolve_existing_method_name('array', method_name)
		}
		types.ArrayFixed {
			elem_name := b.types_type_c_name(typ.elem_type)
			array_fixed_name := 'Array_fixed_${elem_name}_${typ.len}'
			if method := b.resolve_existing_method_name(array_fixed_name, method_name) {
				return method
			}
			return b.resolve_existing_method_name('array', method_name)
		}
		types.Map {
			return b.resolve_existing_method_name('map', method_name)
		}
		types.String {
			return b.resolve_existing_method_name('string', method_name)
		}
		types.Struct, types.Enum, types.Interface, types.SumType, types.Primitive, types.Char,
		types.Rune, types.ISize, types.USize {
			type_name := b.types_type_c_name(typ)
			return b.resolve_existing_method_name(type_name, method_name)
		}
		else {
			return none
		}
	}
}

fn (b &Builder) resolve_existing_method_name(type_name string, method_name string) ?string {
	candidate := '${type_name}__${method_name}'
	if candidate in b.fn_index {
		return candidate
	}
	builtin_candidate := 'builtin__${candidate}'
	if builtin_candidate in b.fn_index {
		return builtin_candidate
	}
	return none
}

fn (mut b Builder) resolve_method_name_for_value(val ValueID, method_name string) ?string {
	if val <= 0 || val >= b.mod.values.len {
		return none
	}
	mut typ_id := b.mod.values[val].typ
	if typ_id > 0 && int(typ_id) < b.mod.type_store.types.len {
		typ := b.mod.type_store.types[typ_id]
		if typ.kind == .ptr_t && typ.elem_type != 0 {
			typ_id = typ.elem_type
		}
	}
	receiver_type := b.type_id_to_receiver_name(typ_id)
	if receiver_type == 'unknown' {
		return none
	}
	return b.resolve_existing_method_name(receiver_type, method_name)
}

// resolve_call_name_for_ident_name (s216) is the pure-string body extracted
// from the Ident arm of `resolve_call_name`. It takes a raw name (typically
// `expr.lhs.name` from an `ast.Ident`, or `c.name()` from a `.expr_ident`
// cursor) and runs the cascade of fn_index lookups + V1/builtin/Array_/i8 →
// u8 / C__ / operator-method remap rules. Both `resolve_call_name` and
// `resolve_call_name_ident_from_flat` (s216) delegate here so they stay
// bit-identical. No AST traversal; pure string + map lookups.
fn (b &Builder) resolve_call_name_for_ident_name(name string) string {
	if local_fn := b.current_module_fn_name(name) {
		return local_fn
	}
	if imported := b.selective_import_fn_name(name) {
		return imported
	}
	// Check if it's a known function
	if name in b.fn_index {
		return name
	}
	qualified := '${b.cur_module}__${name}'
	if qualified in b.fn_index {
		return qualified
	}
	// Try builtin-qualified (transformer remaps builtin__X to X)
	builtin_qualified := 'builtin__${name}'
	if builtin_qualified in b.fn_index {
		return builtin_qualified
	}
	// For names with module prefix (e.g., v__error), try replacing
	// the module prefix with builtin__
	if idx := name.index('__') {
		suffix := name[idx + 2..]
		alt := 'builtin__${suffix}'
		if alt in b.fn_index {
			return alt
		}
	}
	// Resolve operator method names: the transformer generates e.g.
	// 'string__plus' but the SSA builder registers 'builtin__string__+'
	// Map transformer names to operator symbols.
	op_map := {
		'plus':     '+'
		'op_plus':  '+'
		'minus':    '-'
		'op_minus': '-'
		'mult':     '*'
		'op_mul':   '*'
		'div':      '/'
		'op_div':   '/'
		'mod':      '%'
		'op_mod':   '%'
		'eq':       '=='
		'op_eq':    '=='
		'ne':       '!='
		'op_ne':    '!='
		'lt':       '<'
		'op_lt':    '<'
		'gt':       '>'
		'op_gt':    '>'
		'le':       '<='
		'op_le':    '<='
		'ge':       '>='
		'op_ge':    '>='
	}
	if idx2 := name.last_index('__') {
		method_part := name[idx2 + 2..]
		if op_sym := op_map[method_part] {
			type_part := name[..idx2]
			op_name := '${type_part}__${op_sym}'
			if op_name in b.fn_index {
				return op_name
			}
			op_builtin := 'builtin__${op_name}'
			if op_builtin in b.fn_index {
				return op_builtin
			}
		}
	}
	// Self-hosted ARM64 builds can lower byte helper calls through the
	// signed 8-bit prefix even though the builtin methods live on `u8`.
	if name.starts_with('i8__') {
		byte_name := 'u8__${name['i8__'.len..]}'
		if byte_name in b.fn_index {
			return byte_name
		}
		builtin_byte_name := 'builtin__${byte_name}'
		if builtin_byte_name in b.fn_index {
			return builtin_byte_name
		}
	}
	if name.starts_with('builtin__i8__') {
		byte_name := 'builtin__u8__${name['builtin__i8__'.len..]}'
		if byte_name in b.fn_index {
			return byte_name
		}
	}
	// C functions: strip C__ prefix for direct C interop
	if name.starts_with('C__') {
		return name[3..]
	}
	// Transformer generates Array_module__Type__method (V1 naming) but SSA
	// registers as module__Array_Type__method. Remap:
	// Array_ast__Attribute__has → ast__Array_Attribute__has
	if name.starts_with('Array_') {
		rest := name[6..] // after "Array_"
		if mod_end := rest.index('__') {
			mod_name := rest[..mod_end]
			after_mod := rest[mod_end + 2..]
			remap := '${mod_name}__Array_${after_mod}'
			if remap in b.fn_index {
				return remap
			}
		}
	}
	// V1 C backend naming uses single underscore for type_method (e.g., array_push_noscan),
	// but SSA builder uses double underscore (array__push_noscan).
	// Try converting known type prefixes from single to double underscore.
	v1_type_prefixes := ['array_', 'string_', 'int_', 'i8_', 'i16_', 'i64_', 'u8_', 'u16_', 'u32_',
		'u64_', 'f32_', 'f64_', 'bool_', 'map_', 'rune_', 'char_']
	// Check bare name (e.g., "array_push_noscan") and builtin-prefixed
	for prefix in v1_type_prefixes {
		// Check bare name
		if name.starts_with(prefix) {
			type_name := prefix[..prefix.len - 1]
			method_part := name[prefix.len..]
			double_name := '${type_name}__${method_part}'
			if double_name in b.fn_index {
				return double_name
			}
			double_builtin := 'builtin__${double_name}'
			if double_builtin in b.fn_index {
				return double_builtin
			}
		}
		// Check builtin__type_method
		bp := 'builtin__${prefix}'
		if name.starts_with(bp) {
			type_name := prefix[..prefix.len - 1]
			method_part := name[bp.len..]
			double_name := 'builtin__${type_name}__${method_part}'
			if double_name in b.fn_index {
				return double_name
			}
		}
	}
	return name
}

// resolve_call_name_ident_from_flat (s216) cursor-native equivalent of
// `resolve_call_name`'s Ident arm. Used by future build_call_from_flat ports
// to compute fn_name without decoding the lhs. Delegates to
// `resolve_call_name_for_ident_name` for bit-identity with the AST path.
fn (b &Builder) resolve_call_name_ident_from_flat(c ast.Cursor) string {
	if c.kind() != .expr_ident {
		return ''
	}
	return b.resolve_call_name_for_ident_name(c.name())
}

fn (mut b Builder) resolve_call_name(expr ast.CallExpr) string {
	match expr.lhs {
		ast.Ident {
			return b.resolve_call_name_for_ident_name(expr.lhs.name)
		}
		ast.SelectorExpr {
			sel := expr.lhs as ast.SelectorExpr
			if static_method := b.resolve_static_method_name(sel) {
				return static_method
			}
			if mod_name := b.selector_module_name(sel) {
				// Module function call: module.fn()
				// C functions: C.puts() → just 'puts' for direct C interop
				if mod_name == 'C' {
					return sel.rhs.name
				}
				return b.resolve_module_call_fn_name(mod_name, sel.rhs.name)
			}
			if typed_method := b.resolve_method_name_from_checked_type(sel.lhs, sel.rhs.name) {
				return typed_method
			}
			return b.resolve_call_name_selector_method_cascade(b.get_receiver_type_name(sel.lhs),
				sel.rhs.name)
		}
		else {
			return 'unknown_fn'
		}
	}
}

// resolve_call_name_from_flat (s223) cursor mirror of `resolve_call_name`.
// `c` is the call's lhs cursor (NOT the CallExpr — `resolve_call_name` only
// consults `expr.lhs`, never `expr.args`). Mirrors the two-arm match:
//  - `.expr_ident` → `resolve_call_name_ident_from_flat` (s216), which
//    delegates to the shared `resolve_call_name_for_ident_name`.
//  - `.expr_selector` → static-method check (s219) → module-call check
//    (existing `selector_module_name_from_flat`; C functions strip to bare
//    rhs name; module functions return `mod__method` whether or not
//    fn_index hit) → checked-type method check (s220) → fall through to
//    `resolve_call_name_selector_method_cascade` (s222) with the
//    cursor-native `get_receiver_type_name_from_flat` (s221) result.
//  - Any other kind → `'unknown_fn'` (matches legacy else arm).
// All sub-helpers are cursor-native; the legacy AST path delegates to the
// same shared helpers (s216 + s222 + s219 + s220 + s221), so this is
// bit-identical with `resolve_call_name(ast.CallExpr{lhs: decoded_lhs, ...})`.
fn (mut b Builder) resolve_call_name_from_flat(c ast.Cursor) string {
	match c.kind() {
		.expr_ident {
			return b.resolve_call_name_ident_from_flat(c)
		}
		.expr_selector {
			if static_method := b.resolve_static_method_name_from_flat(c) {
				return static_method
			}
			rhs := c.edge(1)
			rhs_name := if rhs.kind() == .expr_ident { rhs.name() } else { '' }
			if mod_name := b.selector_module_name_from_flat(c) {
				if mod_name == 'C' {
					return rhs_name
				}
				return b.resolve_module_call_fn_name(mod_name, rhs_name)
			}
			if typed_method := b.resolve_method_name_from_checked_type_from_flat(c.edge(0),
				rhs_name)
			{
				return typed_method
			}
			return b.resolve_call_name_selector_method_cascade(b.get_receiver_type_name_from_flat(c.edge(0)),
				rhs_name)
		}
		else {
			return 'unknown_fn'
		}
	}
}

// resolve_call_name_selector_method_cascade (s222) is the pure-string body
// extracted from the SelectorExpr arm of `resolve_call_name`, covering
// everything after `get_receiver_type_name` returns. Takes the resolved
// receiver-type name and the method-part (`sel.rhs.name`) and runs the full
// cascade of fn_index lookups: pointer-suffix strip → `Type__method` →
// `builtin__Type__method` → `cur_module__Type__method` → struct_types alias
// scan → Array_T patterns (Array_u8 → strings.Builder; Array_ast__T →
// ast__Array_T) → i8/Array_i8 fallbacks → i64/time.Duration → void/voidptr
// → array base (Array_string, Array_rune, strings.Builder) → Option/Result
// → final fn_index suffix scan. No AST traversal; pure string + map lookups.
// Both `resolve_call_name`'s SelectorExpr arm and the future cursor-native
// SelectorExpr dispatcher (`resolve_call_name_from_flat`) delegate here so
// they stay bit-identical.
fn (mut b Builder) resolve_call_name_selector_method_cascade(receiver_type_in string, method_part string) string {
	mut receiver_type := receiver_type_in
	for receiver_type.ends_with('*') {
		receiver_type = receiver_type[..receiver_type.len - 1]
	}
	method_name := '${receiver_type}__${method_part}'
	if method_name in b.fn_index {
		return method_name
	}
	builtin_method := 'builtin__${method_name}'
	if builtin_method in b.fn_index {
		return builtin_method
	}
	if b.cur_module != '' && b.cur_module != 'main' {
		mod_method := '${b.cur_module}__${method_name}'
		if mod_method in b.fn_index {
			return mod_method
		}
	}
	if type_id := b.struct_types[receiver_type] {
		for alt_name, alt_id in b.struct_types {
			if alt_id == type_id && alt_name != receiver_type {
				alt_method := '${alt_name}__${method_part}'
				if alt_method in b.fn_index {
					return alt_method
				}
				alt_builtin := 'builtin__${alt_method}'
				if alt_builtin in b.fn_index {
					return alt_builtin
				}
			}
		}
	}
	if receiver_type.starts_with('Array_') {
		array_method := 'array__${method_part}'
		if array_method in b.fn_index {
			return array_method
		}
		builtin_array := 'builtin__array__${method_part}'
		if builtin_array in b.fn_index {
			return builtin_array
		}
		if receiver_type == 'Array_u8' {
			builder_method := 'strings__Builder__${method_part}'
			if builder_method in b.fn_index {
				return builder_method
			}
			builtin_builder := 'builtin__strings__Builder__${method_part}'
			if builtin_builder in b.fn_index {
				return builtin_builder
			}
		}
		spec_method := 'builtin__${method_name}'
		if spec_method in b.fn_index {
			return spec_method
		}
		if receiver_type.starts_with('Array_ast__') {
			inner := receiver_type.replace('Array_ast__', 'Array_')
			ast_method := 'ast__${inner}__${method_part}'
			if ast_method in b.fn_index {
				return ast_method
			}
		}
	}
	if receiver_type == 'i8' {
		byte_method := 'u8__${method_part}'
		if byte_method in b.fn_index {
			return byte_method
		}
		builtin_byte_method := 'builtin__${byte_method}'
		if builtin_byte_method in b.fn_index {
			return builtin_byte_method
		}
	}
	if receiver_type == 'Array_i8' {
		if method_part != 'free' {
			builder_method := 'strings__Builder__${method_part}'
			if builder_method in b.fn_index {
				return builder_method
			}
			builtin_builder_method := 'builtin__${builder_method}'
			if builtin_builder_method in b.fn_index {
				return builtin_builder_method
			}
		}
		byte_array_method := 'Array_u8__${method_part}'
		if byte_array_method in b.fn_index {
			return byte_array_method
		}
		builtin_byte_array_method := 'builtin__${byte_array_method}'
		if builtin_byte_array_method in b.fn_index {
			return builtin_byte_array_method
		}
	}
	if receiver_type == 'i64' {
		dur_method := 'time__Duration__${method_part}'
		if dur_method in b.fn_index {
			return dur_method
		}
	}
	if receiver_type == 'void' || receiver_type == 'voidptr' {
		voidptr_method := 'builtin__voidptr__${method_part}'
		if voidptr_method in b.fn_index {
			return voidptr_method
		}
	}
	if receiver_type == 'array' {
		arr_string_method := 'builtin__Array_string__${method_part}'
		if arr_string_method in b.fn_index {
			return arr_string_method
		}
		arr_rune_method := 'builtin__Array_rune__${method_part}'
		if arr_rune_method in b.fn_index {
			return arr_rune_method
		}
		if method_part != 'free' {
			builder_method := 'strings__Builder__${method_part}'
			if builder_method in b.fn_index {
				return builder_method
			}
		}
	}
	if receiver_type.contains('Option') || receiver_type.contains('Result') {
		base_method := 'types__Type__${method_part}'
		if base_method in b.fn_index {
			return base_method
		}
	}
	if receiver_type == 'unknown' || method_name !in b.fn_index {
		method_suffix := '__${method_part}'
		mut best_match := ''
		for fn_name, _ in b.fn_index {
			if fn_name.ends_with(method_suffix) {
				if b.cur_module != '' && fn_name.starts_with('${b.cur_module}__') {
					return fn_name
				}
				if best_match == '' {
					best_match = fn_name
				}
			}
		}
		if best_match != '' {
			return best_match
		}
	}
	return method_name
}

// get_receiver_type_name_from_flat (s221) cursor mirror of
// `get_receiver_type_name`. Resolves a method-call receiver expression to a
// C-style receiver type name (`'rune'`, `'int'`, `'array'`, struct names, …).
// Branches:
//  - BasicLiteral kind dispatch (char→rune, number→int/f64, bool keywords)
//  - StringLiteral / StringInterLiteral → 'string'
//  - ParenExpr → recurse on edge(0)
//  - UnsafeExpr → recurse on last edge if .stmt_expr
//  - explicit_cast_receiver_type_name_from_flat (s220)
//  - get_checked_expr_type_from_flat → types_type_c_name
//  - CastExpr direct type, PrefixExpr .mul + .expr_cast pointee
//  - Ident: vars-table lookup → type_id_to_receiver_name; fallback to name
//  - CallExpr / CallOrCastExpr: resolve via resolve_call_name_ident_from_flat
//    (s216) for Ident lhs; for SelectorExpr / other lhs kinds, decode the
//    lhs cursor and call legacy `resolve_call_name` (args empty — the
//    resolver does not consult them). resolve_call_name's SelectorExpr arm
//    is not yet cursor-native; that's pending s222+.
//  - SelectorExpr: lhs checked type + struct field lookup
fn (mut b Builder) get_receiver_type_name_from_flat(c ast.Cursor) string {
	kind := c.kind()
	if kind == .expr_basic_literal {
		lit_kind := unsafe { token.Token(int(c.aux())) }
		if lit_kind == .char {
			return 'rune'
		}
		if lit_kind == .number {
			if c.name().contains('.') {
				return 'f64'
			}
			return 'int'
		}
		if lit_kind == .key_true || lit_kind == .key_false {
			return 'bool'
		}
	}
	if kind == .expr_string || kind == .expr_string_inter {
		return 'string'
	}
	if kind == .expr_paren {
		return b.get_receiver_type_name_from_flat(c.edge(0))
	}
	if kind == .expr_unsafe {
		n := c.edge_count()
		if n > 0 {
			last := c.edge(n - 1)
			if last.kind() == .stmt_expr {
				return b.get_receiver_type_name_from_flat(last.edge(0))
			}
		}
	}
	if receiver_type_name := b.explicit_cast_receiver_type_name_from_flat(c) {
		return receiver_type_name
	}
	if typ := b.get_checked_expr_type_from_flat(c) {
		return b.types_type_c_name(typ)
	}
	if kind == .expr_cast {
		name := b.type_id_to_receiver_name(b.ast_type_to_ssa_from_flat(c.edge(0)))
		if name != 'unknown' {
			return name
		}
	}
	if kind == .expr_prefix {
		op := unsafe { token.Token(int(c.aux())) }
		inner := c.edge(0)
		if op == .mul && inner.kind() == .expr_cast {
			target_type := b.ast_type_to_ssa_from_flat(inner.edge(0))
			if target_type > 0 && int(target_type) < b.mod.type_store.types.len {
				target_info := b.mod.type_store.types[target_type]
				if target_info.kind == .ptr_t && target_info.elem_type != 0 {
					name := b.type_id_to_receiver_name(target_info.elem_type)
					if name != 'unknown' {
						return name
					}
				}
			}
		}
	}
	if kind == .expr_ident {
		name := c.name()
		if var_id := b.vars[name] {
			mut var_type := b.mod.values[var_id].typ
			if var_type != 0 {
				ts_type := b.mod.type_store.types[int(var_type)]
				if ts_type.kind == .ptr_t && ts_type.elem_type != 0 {
					var_type = ts_type.elem_type
				}
				rname := b.type_id_to_receiver_name(var_type)
				if rname != 'unknown' {
					return rname
				}
			}
		}
		return name
	}
	if kind == .expr_call || kind == .expr_call_or_cast {
		call_fn_name := b.resolve_call_name_from_flat(c.edge(0))
		if call_fn_name in b.fn_index {
			fn_idx := b.fn_index[call_fn_name]
			ret_typ := b.mod.funcs[fn_idx].typ
			if ret_typ != 0 {
				return b.type_id_to_receiver_name(ret_typ)
			}
		}
	}
	if kind == .expr_selector {
		if lhs_typ := b.get_checked_expr_type_from_flat(c.edge(0)) {
			if lhs_typ is types.Struct {
				rhs := c.edge(1)
				if rhs.kind() == .expr_ident {
					rhs_name := rhs.name()
					for fi in 0 .. lhs_typ.fields.len {
						if lhs_typ.fields[fi].name == rhs_name {
							return b.types_type_c_name(lhs_typ.fields[fi].typ)
						}
					}
				}
			}
		}
	}
	return 'unknown'
}

fn (mut b Builder) get_receiver_type_name(expr ast.Expr) string {
	// For literals used as method receivers (e.g., `A`.length_in_bytes(), 65.str()),
	// the env type at the literal's position may be the method's function type
	// rather than the literal's value type. Handle these cases before env lookup.
	if expr is ast.BasicLiteral {
		if expr.kind == .char {
			return 'rune'
		}
		if expr.kind == .number {
			if expr.value.contains('.') {
				return 'f64'
			}
			return 'int'
		}
		if expr.kind == .key_true || expr.kind == .key_false {
			return 'bool'
		}
	}
	if expr is ast.StringLiteral || expr is ast.StringInterLiteral {
		return 'string'
	}
	if expr is ast.ParenExpr {
		return b.get_receiver_type_name(expr.expr)
	}
	if expr is ast.UnsafeExpr {
		if expr.stmts.len > 0 {
			last := expr.stmts[expr.stmts.len - 1]
			if last is ast.ExprStmt {
				return b.get_receiver_type_name(last.expr)
			}
		}
	}
	if receiver_type_name := b.explicit_cast_receiver_type_name(expr) {
		return receiver_type_name
	}
	if typ := b.get_checked_expr_type(expr) {
		return b.types_type_c_name(typ)
	}
	if expr is ast.CastExpr {
		name := b.type_id_to_receiver_name(b.ast_type_to_ssa(expr.typ))
		if name != 'unknown' {
			return name
		}
	}
	if expr is ast.PrefixExpr {
		if expr.op == .mul && expr.expr is ast.CastExpr {
			cast_expr := expr.expr as ast.CastExpr
			target_type := b.ast_type_to_ssa(cast_expr.typ)
			if target_type > 0 && int(target_type) < b.mod.type_store.types.len {
				target_info := b.mod.type_store.types[target_type]
				if target_info.kind == .ptr_t && target_info.elem_type != 0 {
					name := b.type_id_to_receiver_name(target_info.elem_type)
					if name != 'unknown' {
						return name
					}
				}
			}
		}
	}
	if expr is ast.Ident {
		// Try to get the type from the SSA variable's alloca type.
		// This is more reliable than env.get_expr_type in ARM64-compiled binaries
		// where the type checker's expression type cache may have corrupt entries.
		if var_id := b.vars[expr.name] {
			mut var_type := b.mod.values[var_id].typ
			// Alloca types are ptr(T), unwrap the pointer to get base type
			if var_type != 0 {
				ts_type := b.mod.type_store.types[int(var_type)]
				if ts_type.kind == .ptr_t && ts_type.elem_type != 0 {
					var_type = ts_type.elem_type
				}
				name := b.type_id_to_receiver_name(var_type)
				if name != 'unknown' {
					return name
				}
			}
		}
		return expr.name
	}
	// For CallExpr/CallOrCastExpr, try to infer receiver type from the called
	// function's return type. This is needed when env.get_expr_type fails (e.g.,
	// in ARM64-compiled binaries where the checker's type store may be unreliable).
	if expr is ast.CallExpr {
		call_fn_name := b.resolve_call_name(expr)
		if call_fn_name in b.fn_index {
			fn_idx := b.fn_index[call_fn_name]
			ret_typ := b.mod.funcs[fn_idx].typ
			if ret_typ != 0 {
				return b.type_id_to_receiver_name(ret_typ)
			}
		}
	}
	if expr is ast.CallOrCastExpr {
		// Try resolving as CallExpr for receiver type inference
		call_expr := ast.CallExpr{
			lhs:  expr.lhs
			args: if expr.expr is ast.EmptyExpr { []ast.Expr{} } else { [expr.expr] }
		}
		call_fn_name := b.resolve_call_name(call_expr)
		if call_fn_name in b.fn_index {
			fn_idx := b.fn_index[call_fn_name]
			ret_typ := b.mod.funcs[fn_idx].typ
			if ret_typ != 0 {
				return b.type_id_to_receiver_name(ret_typ)
			}
		}
	}
	// For SelectorExpr (e.g., obj.field), try to resolve the field's type
	// by looking up the LHS type and then the field type in struct definitions.
	if expr is ast.SelectorExpr {
		if lhs_typ := b.get_checked_expr_type(expr.lhs) {
			// If the LHS is a struct, look up the field type
			if lhs_typ is types.Struct {
				for fi in 0 .. lhs_typ.fields.len {
					if lhs_typ.fields[fi].name == expr.rhs.name {
						return b.types_type_c_name(lhs_typ.fields[fi].typ)
					}
				}
			}
		}
	}
	return 'unknown'
}

// type_id_to_receiver_name converts an SSA TypeID to a C-style receiver name
// for method resolution (e.g., 'string', 'array', 'int', struct names).
fn (mut b Builder) type_id_to_receiver_name(typ TypeID) string {
	// Check against known struct types (reverse lookup)
	for name, id in b.struct_types {
		if id == typ {
			return name
		}
	}
	if int(typ) >= 0 {
		if name := b.mod.c_struct_names[int(typ)] {
			return name
		}
	}
	// Check the SSA type kind for primitives
	if int(typ) >= 0 && int(typ) < b.mod.type_store.types.len {
		t := b.mod.type_store.types[int(typ)]
		match t.kind {
			.int_t {
				if t.is_unsigned {
					return match t.width {
						8 { 'u8' }
						16 { 'u16' }
						64 { 'u64' }
						else { 'u32' }
					}
				}
				return match t.width {
					8 { 'i8' }
					16 { 'i16' }
					64 { 'i64' }
					else { 'int' }
				}
			}
			.float_t {
				return if t.width == 32 { 'f32' } else { 'f64' }
			}
			.ptr_t {
				return 'unknown'
			}
			.array_t {
				elem_name := b.type_id_to_receiver_name(t.elem_type)
				if elem_name != 'unknown' {
					return 'Array_${elem_name}'
				}
				return 'array'
			}
			else {
				return 'unknown'
			}
		}
	}
	return 'unknown'
}

fn (mut b Builder) build_selector(expr ast.SelectorExpr) ValueID {
	// Check for enum shorthand: .field in certain contexts
	if expr.lhs is ast.EmptyExpr || (expr.lhs is ast.Ident && expr.lhs.name == '') {
		// Enum shorthand — try to look up a resolved name
		field_name := expr.rhs.name
		field_symbol := enum_field_symbol_name(field_name)
		// Collect all matching enum values (not just the first)
		suffix := '__${field_symbol}'
		mut match_keys := []string{}
		mut match_vals := []int{}
		for key, val in b.enum_values {
			if key.ends_with(suffix) {
				match_keys << key
				match_vals << val
			}
		}
		if match_keys.len == 1 {
			return b.mod.get_or_add_const(b.mod.type_store.get_int(32), match_vals[0].str())
		}
		if match_keys.len > 1 {
			// Disambiguate: prefer enum from current module
			if b.cur_module != '' {
				if b.cur_module == 'main' {
					// Main-module enums are registered without module prefix (`Enum__field`).
					// Prefer those over similarly-named fields from imported modules.
					for i, mk in match_keys {
						if mk.split('__').len == 2 {
							return b.mod.get_or_add_const(b.mod.type_store.get_int(32),
								match_vals[i].str())
						}
					}
				} else {
					for i, mk in match_keys {
						if mk.starts_with('${b.cur_module}__') {
							return b.mod.get_or_add_const(b.mod.type_store.get_int(32),
								match_vals[i].str())
						}
					}
				}
			}
			// Fallback: use the first match
			return b.mod.get_or_add_const(b.mod.type_store.get_int(32), match_vals[0].str())
		}
		return b.mod.get_or_add_const(b.mod.type_store.get_int(32), '0')
	}

	// Check for qualified enum access: EnumType.value
	if expr.lhs is ast.Ident {
		// Try: EnumType__value (local) or module__EnumType__value (qualified)
		enum_key := '${expr.lhs.name}__${enum_field_symbol_name(expr.rhs.name)}'
		if enum_key in b.enum_values {
			return b.mod.get_or_add_const(b.mod.type_store.get_int(32),
				b.enum_values[enum_key].str())
		}
		qualified_key := '${b.cur_module}__${enum_key}'
		if qualified_key in b.enum_values {
			return b.mod.get_or_add_const(b.mod.type_store.get_int(32),
				b.enum_values[qualified_key].str())
		}
	}

	// C constant/global access: C.SEEK_END, C.stdout, C.stderr, etc.
	if expr.lhs is ast.Ident && expr.lhs.name == 'C' {
		c_name := expr.rhs.name
		if win_const := b.build_windows_c_macro_const(c_name) {
			return win_const
		}
		// Well-known C preprocessor constants — emit inline integer values
		// since the native backend cannot resolve C macros.
		c_const_val := match c_name {
			'SEEK_SET' { '0' }
			'SEEK_CUR' { '1' }
			'SEEK_END' { '2' }
			'EOF' { '-1' }
			'NULL' { '0' }
			'O_RDONLY' { '0' }
			'O_WRONLY' { '1' }
			'O_RDWR' { '2' }
			'O_CREAT' { '512' }
			'O_TRUNC' { '1024' }
			'O_EXCL' { '2048' }
			'O_APPEND' { '8' }
			'S_IRUSR' { '256' }
			'S_IWUSR' { '128' }
			'S_IXUSR' { '64' }
			'S_IREAD' { '256' }
			'S_IWRITE' { '128' }
			'S_IEXEC' { '64' }
			'PROT_READ' { '1' }
			'PROT_WRITE' { '2' }
			'SIGTERM' { '15' }
			'SIGKILL' { '9' }
			'SIGINT' { '2' }
			'STDIN_FILENO' { '0' }
			'STDOUT_FILENO' { '1' }
			'STDERR_FILENO' { '2' }
			'DT_DIR' { '4' }
			'DT_REG' { '8' }
			'DT_LNK' { '10' }
			'DT_UNKNOWN' { '0' }
			'ENOENT' { '2' }
			'EXIT_SUCCESS' { '0' }
			'EXIT_FAILURE' { '1' }
			// Termios
			'ICANON' { '256' }
			'ECHO' { '8' }
			'TCSANOW' { '0' }
			'ISIG' { '128' }
			'IEXTEN' { '1024' }
			'TOSTOP' { '4194304' }
			// ioctl
			'TIOCGWINSZ' { '1074295912' }
			// Time
			'CLOCK_MONOTONIC' { '6' }
			'CLOCK_REALTIME' { '0' }
			// Mach
			'KERN_SUCCESS' { '0' }
			'TASK_BASIC_INFO' { '18' }
			'MACH_TASK_BASIC_INFO_COUNT' { '12' }
			'HOST_VM_INFO64' { '4' }
			'HOST_VM_INFO64_COUNT' { '40' }
			// System
			'_SC_PAGESIZE' { '29' }
			'_SC_NPROCESSORS_ONLN' { '58' }
			'_SC_PHYS_PAGES' { '200' }
			// Errno
			'EINTR' { '4' }
			'EINVAL' { '22' }
			'EAGAIN' { '35' }
			'EWOULDBLOCK' { '35' }
			'EINPROGRESS' { '36' }
			'EACCES' { '13' }
			'EFAULT' { '14' }
			'EBUSY' { '16' }
			'ETIMEDOUT' { '60' }
			// Stat mode bits (macOS)
			'S_IFBLK' { '24576' }
			'S_IFCHR' { '8192' }
			'S_IFDIR' { '16384' }
			'S_IFIFO' { '4096' }
			'S_IFLNK' { '40960' }
			'S_IFMT' { '61440' }
			'S_IFREG' { '32768' }
			'S_IFSOCK' { '49152' }
			'S_IRGRP' { '32' }
			'S_IROTH' { '4' }
			'S_IWGRP' { '16' }
			'S_IWOTH' { '2' }
			'S_IXGRP' { '8' }
			'S_IXOTH' { '1' }
			// Ptrace
			'PT_DETACH' { '11' }
			'PT_TRACE_ME' { '0' }
			// Signals
			'SIG_ERR' { '-1' }
			'SIG_BLOCK' { '1' }
			'SIG_UNBLOCK' { '2' }
			'SIG_SETMASK' { '3' }
			'SIGCONT' { '19' }
			'SIGSTOP' { '17' }
			// Wait
			'WNOHANG' { '1' }
			// I/O buffering
			'_IOFBF' { '0' }
			'_IOLBF' { '1' }
			'_IONBF' { '2' }
			// File flags
			'O_NONBLOCK' { '4' }
			'O_CLOEXEC' { '16777216' }
			else { '' }
		}

		if c_const_val.len > 0 {
			return b.mod.get_or_add_const(b.mod.type_store.get_int(32), c_const_val)
		}
		// float.h exposes these as preprocessor macros, not linkable data symbols.
		// Lower them directly so native backends do not emit _FLT_EPSILON/_DBL_EPSILON.
		if c_name == 'FLT_EPSILON' {
			return b.mod.get_or_add_const(b.mod.type_store.get_float(32), '1.19209290e-07')
		}
		if c_name == 'DBL_EPSILON' {
			return b.mod.get_or_add_const(b.mod.type_store.get_float(64), '2.2204460492503131e-16')
		}
		// _wyp: wyhash secret array from wyhash.h. Our wyhash stub uses
		// hardcoded constants, so this just needs to be a valid pointer.
		if c_name == '_wyp' {
			i64_t := b.mod.type_store.get_int(64)
			return b.mod.get_or_add_const(i64_t, '0')
		}
		// errno is not portable linkable data on targets with TLS errno.
		// Darwin exposes __error(); glibc/LSB expose __errno_location();
		// Windows CRT exposes _errno().
		if c_name == 'errno' {
			i32_t := b.mod.type_store.get_int(32)
			errno_addr := b.build_c_errno_storage_addr()
			return b.mod.add_instr(.load, b.cur_block, i32_t, [errno_addr])
		}
		// Map C standard I/O streams to macOS-specific symbol names only for macOS.
		target_name := if b.is_macos_target() {
			match c_name {
				'stdout' { '__stdoutp' }
				'stderr' { '__stderrp' }
				'stdin' { '__stdinp' }
				else { c_name }
			}
		} else {
			c_name
		}

		i8_t := b.mod.type_store.get_int(8)
		ptr_t := b.mod.type_store.get_ptr(i8_t)
		if c_name in ['stdout', 'stderr', 'stdin'] {
			// These are C `FILE*` variables. macOS exposes them as libSystem
			// `__stdoutp` / `__stdinp` / `__stderrp`; other targets use the
			// standard names. Reading the V expression `C.stdout` must yield the
			// FILE* value, not the address of the variable.
			glob := b.mod.add_external_global(target_name, ptr_t)
			b.global_refs[target_name] = glob
			return b.mod.add_instr(.load, b.cur_block, ptr_t, [glob])
		}

		// Not a known constant — emit as a global reference (e.g. C.stdout, C.stderr)
		glob := b.mod.add_value_node(.global, ptr_t, target_name, 0)
		b.global_refs[target_name] = glob
		return glob
	}

	// Module-qualified constant/global access: os.args, pref.Backend, etc.
	// When LHS is a module name, resolve module__field as a constant or global.
	if expr.lhs is ast.Ident {
		mut mod_name := ''
		if resolved_mod := b.selector_module_name(expr) {
			mod_name = resolved_mod
		} else if b.env == unsafe { nil } {
			mod_name = expr.lhs.name.replace('.', '_')
		}
		if mod_name != '' {
			qualified := ssa_module_storage_name(mod_name, expr.rhs.name)
			// Try as float constant (inline as f64)
			if fval := b.float_const_values[qualified] {
				return b.mod.get_or_add_const(b.mod.type_store.get_float(64), fval)
			}
			// Try as compile-time constant
			if qualified in b.const_values {
				ct := if qualified in b.const_value_types {
					b.const_value_types[qualified]
				} else {
					b.mod.type_store.get_int(64)
				}
				return b.mod.get_or_add_const(ct, b.const_values[qualified].str())
			}
			// Try as string constant
			if qualified in b.string_const_values {
				return b.build_string_literal(ast.StringLiteral{
					kind:  .v
					value: b.string_const_values[qualified]
				})
			}
			// Try as constant array global (return pointer directly for indexing)
			if qualified in b.const_array_globals {
				if glob_id := b.find_global(qualified) {
					return glob_id
				}
			}
			// Try as global variable (runtime-initialized constants like os.args)
			if glob_id := b.find_global(qualified) {
				glob_typ := b.mod.values[glob_id].typ
				elem_typ := b.mod.type_store.types[glob_typ].elem_type
				return b.mod.add_instr(.load, b.cur_block, elem_typ, [glob_id])
			}
		}
	}

	// Check for const array global .len access — return compile-time element count
	if expr.rhs.name == 'len' && expr.lhs is ast.Ident {
		lhs_name := expr.lhs.name
		if count := b.const_array_elem_count[lhs_name] {
			return b.mod.get_or_add_const(b.mod.type_store.get_int(64), count.str())
		}
		qualified := '${b.cur_module}__${lhs_name}'
		if count := b.const_array_elem_count[qualified] {
			return b.mod.get_or_add_const(b.mod.type_store.get_int(64), count.str())
		}
		builtin_name := 'builtin__${lhs_name}'
		if count := b.const_array_elem_count[builtin_name] {
			return b.mod.get_or_add_const(b.mod.type_store.get_int(64), count.str())
		}
	}

	// Prefer address-based selector lowering when the receiver is addressable.
	// This avoids materializing large struct values for expressions like
	// `items[i].field`; only the selected field is loaded.
	skip_addr_selector := expr.lhs is ast.Ident
		&& b.ident_has_inline_const_value((expr.lhs as ast.Ident).name)
	if !skip_addr_selector {
		if selector_addr := b.build_selector_addr(expr) {
			addr_type := b.mod.values[selector_addr.addr].typ
			if addr_type > 0 && int(addr_type) < b.mod.type_store.types.len {
				ptr_type := b.mod.type_store.types[addr_type]
				if ptr_type.kind == .ptr_t && ptr_type.elem_type > 0 {
					field_val := b.mod.add_instr(.load, b.cur_block, ptr_type.elem_type, [
						selector_addr.addr,
					])
					if ptr_type.elem_type == b.get_array_type() {
						b.track_array_value_elem_type_from_selector(expr, field_val,
							selector_addr.base_type)
					}
					return field_val
				}
			}
		}
	}

	// Use extractvalue for struct field access
	mut base := b.build_expr(expr.lhs)
	if field_val := b.build_sumtype_variant_selector_value(base,
		b.selector_lhs_variant_type_id(expr.lhs), expr.rhs.name)
	{
		if b.mod.values[field_val].typ == b.get_array_type() {
			b.track_array_value_elem_type_from_selector(expr, field_val, b.mod.values[base].typ)
		}
		return field_val
	}
	// Check for fixed-size array .len access — return compile-time constant
	base_typ_raw := b.mod.values[base].typ
	if base_typ_raw > 0 && int(base_typ_raw) < b.mod.type_store.types.len {
		mut check_typ := b.mod.type_store.types[base_typ_raw]
		// Also handle ptr(array_t) — e.g. when fixed array is behind a pointer
		if check_typ.kind == .ptr_t && check_typ.elem_type > 0
			&& int(check_typ.elem_type) < b.mod.type_store.types.len {
			check_typ = b.mod.type_store.types[check_typ.elem_type]
		}
		if check_typ.kind == .array_t && expr.rhs.name == 'len' {
			return b.mod.get_or_add_const(b.mod.type_store.get_int(64), check_typ.len.str())
		}
	}
	// If base is a pointer to struct (mut param or heap alloc), auto-deref
	base_typ := b.mod.values[base].typ
	if base_typ > 0 && int(base_typ) < b.mod.type_store.types.len
		&& b.mod.type_store.types[base_typ].kind == .ptr_t {
		pointee := b.mod.type_store.types[base_typ].elem_type
		if pointee > 0 && int(pointee) < b.mod.type_store.types.len
			&& b.mod.type_store.types[pointee].kind == .struct_t {
			field_idx := b.field_index(expr, base)
			if field_idx < 0 {
				return 0
			}
			mut result_type := TypeID(0)
			pointee_typ := b.mod.type_store.types[pointee]
			if field_idx >= 0 && field_idx < pointee_typ.fields.len {
				result_type = pointee_typ.fields[field_idx]
			}
			if result_type == 0 {
				result_type = b.expr_type(ast.Expr(expr))
			}
			if result_type <= 0 || int(result_type) >= b.mod.type_store.types.len {
				return 0
			}
			field_ptr_type := b.mod.type_store.get_ptr(result_type)
			field_ptr := b.mod.add_instr(.get_element_ptr, b.cur_block, field_ptr_type, [
				base,
				b.mod.get_or_add_const(b.mod.type_store.get_int(32), field_idx.str()),
			])
			field_val := b.mod.add_instr(.load, b.cur_block, result_type, [field_ptr])
			if result_type == b.get_array_type() {
				elem_type := b.struct_field_array_elem_type(pointee, expr.rhs.name)
				if elem_type != 0 {
					b.array_value_elem_types[field_val] = elem_type
				} else if checked_field_type := b.checked_struct_field_type_from_ssa(pointee,
					expr.rhs.name)
				{
					checked_elem_type := b.unwrap_to_array_elem_ssa(checked_field_type)
					if checked_elem_type != 0 {
						b.array_value_elem_types[field_val] = checked_elem_type
					}
				}
			}
			return field_val
		}
	}
	// Determine result type: prefer SSA struct field type for struct bases,
	// fall back to type environment.
	// The type environment may have the smartcast variant type (e.g., int) for a
	// sumtype field access, while the SSA struct has the correct field type.
	mut field_idx := -1
	mut result_type := TypeID(0)
	actual_base_type := b.mod.values[base].typ
	if actual_base_type > 0 && int(actual_base_type) < b.mod.type_store.types.len {
		typ := b.mod.type_store.types[actual_base_type]
		if typ.kind == .struct_t {
			for i, name in typ.field_names {
				if name == expr.rhs.name {
					field_idx = i
					result_type = typ.fields[i]
					break
				}
			}
			if field_idx < 0 && b.ssa_type_is_sumtype(actual_base_type)
				&& !expr.rhs.name.starts_with('arg') {
				return 0
			}
		}
	}
	if field_idx < 0 {
		field_idx = b.field_index(expr, base)
	}
	if field_idx < 0 {
		return 0
	}
	if result_type == 0 {
		result_type = b.expr_type(ast.Expr(expr))
	}
	field_val := b.mod.add_instr(.extractvalue, b.cur_block, result_type, [base,
		b.mod.get_or_add_const(b.mod.type_store.get_int(32), field_idx.str())])
	if result_type == b.get_array_type() {
		b.track_array_value_elem_type_from_selector(expr, field_val, actual_base_type)
	}
	return field_val
}

fn (mut b Builder) build_selector_addr(expr ast.SelectorExpr) ?SelectorAddr {
	base := b.build_addr(expr.lhs)
	if base == 0 {
		return none
	}
	base_type := b.mod.values[base].typ
	if base_type <= 0 || int(base_type) >= b.mod.type_store.types.len {
		return none
	}
	ptr_type := b.mod.type_store.types[base_type]
	if ptr_type.kind != .ptr_t || ptr_type.elem_type <= 0
		|| int(ptr_type.elem_type) >= b.mod.type_store.types.len {
		return none
	}
	struct_type := b.mod.type_store.types[ptr_type.elem_type]
	if struct_type.kind != .struct_t {
		return none
	}
	mut field_idx := -1
	for i, name in struct_type.field_names {
		if name == expr.rhs.name {
			field_idx = i
			break
		}
	}
	if field_idx < 0 && b.ssa_type_is_sumtype(ptr_type.elem_type) {
		return none
	}
	if field_idx < 0 {
		field_idx = b.field_index(expr, base)
	}
	if field_idx < 0 || field_idx >= struct_type.fields.len {
		return none
	}
	field_type := struct_type.fields[field_idx]
	field_ptr_type := b.mod.type_store.get_ptr(field_type)
	idx_val := b.mod.get_or_add_const(b.mod.type_store.get_int(32), field_idx.str())
	addr := b.mod.add_instr(.get_element_ptr, b.cur_block, field_ptr_type, [base, idx_val])
	return SelectorAddr{
		addr:      addr
		base_type: ptr_type.elem_type
	}
}

fn (mut b Builder) build_sumtype_variant_selector_value(base ValueID, variant_type TypeID, field_name string) ?ValueID {
	if base <= 0 || base >= b.mod.values.len || field_name == '' {
		return none
	}
	base_type := b.mod.values[base].typ
	if !b.ssa_type_is_sumtype(base_type) {
		return none
	}
	if variant_type <= 0 || variant_type == base_type
		|| int(variant_type) >= b.mod.type_store.types.len {
		return none
	}
	variant_info := b.mod.type_store.types[variant_type]
	if variant_info.kind != .struct_t {
		return none
	}
	mut field_idx := -1
	mut field_type := TypeID(0)
	for i, name in variant_info.field_names {
		if name == field_name {
			field_idx = i
			field_type = variant_info.fields[i]
			break
		}
	}
	if field_idx < 0 || field_type <= 0 {
		return none
	}
	i32_t := b.mod.type_store.get_int(32)
	i64_t := b.mod.type_store.get_int(64)
	data_idx := b.mod.get_or_add_const(i32_t, '1')
	data_word := b.mod.add_instr(.extractvalue, b.cur_block, i64_t, [base, data_idx])
	variant_ptr_type := b.mod.type_store.get_ptr(variant_type)
	data_ptr := b.cast_value_to_type(data_word, variant_ptr_type)
	field_idx_val := b.mod.get_or_add_const(i32_t, field_idx.str())
	field_ptr_type := b.mod.type_store.get_ptr(field_type)
	field_ptr := b.mod.add_instr(.get_element_ptr, b.cur_block, field_ptr_type, [
		data_ptr,
		field_idx_val,
	])
	return b.mod.add_instr(.load, b.cur_block, field_type, [field_ptr])
}

fn (mut b Builder) track_array_value_elem_type_from_selector(expr ast.SelectorExpr, field_val ValueID, base_type TypeID) {
	elem_type := b.struct_field_array_elem_type(base_type, expr.rhs.name)
	if elem_type != 0 {
		b.array_value_elem_types[field_val] = elem_type
		return
	}
	if checked_field_type := b.checked_struct_field_type_from_ssa(base_type, expr.rhs.name) {
		checked_elem_type := b.unwrap_to_array_elem_ssa(checked_field_type)
		if checked_elem_type != 0 {
			b.array_value_elem_types[field_val] = checked_elem_type
			return
		}
	}
	selector_elem_type := b.array_elem_type_from_selector_expr(expr)
	if selector_elem_type != 0 {
		b.array_value_elem_types[field_val] = selector_elem_type
		return
	}
	if checked_expr_type := b.get_checked_expr_type(ast.Expr(expr)) {
		expr_elem_type := b.unwrap_to_array_elem_ssa(checked_expr_type)
		if expr_elem_type != 0 {
			b.array_value_elem_types[field_val] = expr_elem_type
		}
	}
}

fn (mut b Builder) array_elem_type_from_selector_expr(expr ast.SelectorExpr) TypeID {
	if lhs_type := b.get_checked_expr_type(expr.lhs) {
		st := b.unwrap_to_struct(lhs_type)
		if st.name != '' {
			for fi in 0 .. st.fields.len {
				if st.fields[fi].name == expr.rhs.name {
					return b.unwrap_to_array_elem_ssa(st.fields[fi].typ)
				}
			}
		}
	}
	return 0
}

fn (mut b Builder) field_index(expr ast.SelectorExpr, base ValueID) int {
	rhs_name := expr.rhs.name
	// Prefer the SSA struct layout, because the selected index is used to build
	// backend addresses. The checker type can be a stale placeholder while the
	// SSA type already contains the concrete field order.
	base_type_id := b.mod.values[base].typ
	if base_type_id > 0 && int(base_type_id) < b.mod.type_store.types.len {
		mut ssa_typ := b.mod.type_store.types[base_type_id]
		// Dereference pointer(s) to get to the struct type
		for ssa_typ.kind == .ptr_t && ssa_typ.elem_type > 0
			&& int(ssa_typ.elem_type) < b.mod.type_store.types.len {
			ssa_typ = b.mod.type_store.types[ssa_typ.elem_type]
		}
		if ssa_typ.kind == .struct_t {
			for i, name in ssa_typ.field_names {
				if name == rhs_name {
					return i
				}
			}
		}
	}
	if typ := b.get_checked_expr_type(expr.lhs) {
		st := b.unwrap_to_struct(typ)
		if st.name != '' {
			for i in 0 .. st.fields.len {
				if st.fields[i].name == rhs_name {
					return i
				}
			}
		}
	}
	// Tuple field access: the transformer generates `_tuple_tN.arg0`, `.arg1`, etc.
	// Tuples have no named fields in the SSA type, so parse the index from `argN`.
	if rhs_name.starts_with('arg') {
		idx_str := rhs_name[3..]
		if idx_str.len > 0 && idx_str[0] >= `0` && idx_str[0] <= `9` {
			return int(parse_const_int_literal(idx_str))
		}
	}
	return 0
}

// is_struct_field checks whether a field name exists as a function-typed struct
// field of the receiver expression's type. Used to distinguish function pointer
// field calls (e.g., m.hash_fn(pkey)) from method calls.
fn (mut b Builder) is_struct_field(receiver_expr ast.Expr, field_name string) bool {
	// Try type environment first
	if typ := b.get_checked_expr_type(receiver_expr) {
		st := b.unwrap_to_struct(typ)
		if st.name != '' {
			for fi in 0 .. st.fields.len {
				if st.fields[fi].name == field_name {
					return b.is_fn_type(st.fields[fi].typ)
				}
			}
		}
	}
	return false
}

// is_struct_field_from_flat (s229) is the cursor mirror of is_struct_field.
// get_checked_expr_type_from_flat reads the same pos.id as the decoded receiver,
// so the struct-field lookup is bit-identical.
fn (mut b Builder) is_struct_field_from_flat(receiver_c ast.Cursor, field_name string) bool {
	if typ := b.get_checked_expr_type_from_flat(receiver_c) {
		st := b.unwrap_to_struct(typ)
		if st.name != '' {
			for fi in 0 .. st.fields.len {
				if st.fields[fi].name == field_name {
					return b.is_fn_type(st.fields[fi].typ)
				}
			}
		}
	}
	return false
}

fn (b &Builder) is_fn_type(t types.Type) bool {
	unwrapped := b.unwrap_alias_type(t)
	match unwrapped {
		types.FnType {
			return true
		}
		else {
			return false
		}
	}
}

fn (mut b Builder) selector_receiver_is_interface(receiver_expr ast.Expr) bool {
	if typ := b.get_checked_expr_type(receiver_expr) {
		return b.is_interface_type(typ)
	}
	return false
}

// selector_receiver_is_interface_from_flat (s229) is the cursor mirror of
// selector_receiver_is_interface.
fn (mut b Builder) selector_receiver_is_interface_from_flat(receiver_c ast.Cursor) bool {
	if typ := b.get_checked_expr_type_from_flat(receiver_c) {
		return b.is_interface_type(typ)
	}
	return false
}

fn (b &Builder) is_interface_type(t types.Type) bool {
	match t {
		types.Interface {
			return true
		}
		types.Pointer {
			return b.is_interface_type(t.base_type)
		}
		types.Alias {
			base := b.resolve_alias_base_type(t) or { return false }
			return b.is_interface_type(base)
		}
		else {
			return false
		}
	}
}

fn (mut b Builder) unwrap_to_struct(t types.Type) types.Struct {
	match t {
		types.Struct {
			return t
		}
		types.Pointer {
			return b.unwrap_to_struct(t.base_type)
		}
		types.Alias {
			base := b.resolve_alias_base_type(t) or { return types.Struct{} }
			return b.unwrap_to_struct(base)
		}
		types.Array, types.ArrayFixed {
			if builtin_array := b.lookup_checked_type_by_name('array') {
				if builtin_array is types.Struct {
					return builtin_array
				}
			}
			return types.Struct{}
		}
		types.Map {
			// Map is a builtin struct type. Look up the 'map' struct from
			// the type checker's scope to get its fields (hash_fn, key_eq_fn, etc).
			if b.env != unsafe { nil } {
				if scope := b.env.get_scope('builtin') {
					if obj := scope.lookup_parent('map', 0) {
						obj_type := obj.typ()
						if obj_type is types.Struct {
							return obj_type
						}
					}
				}
			}
			return types.Struct{}
		}
		else {
			name := types.type_name(t)
			if name != '' {
				if live_type := b.lookup_checked_type_by_name(name) {
					if live_type is types.Struct {
						return live_type
					}
					if types.type_name(live_type) != name {
						return b.unwrap_to_struct(live_type)
					}
				}
			}
			return types.Struct{}
		}
	}
}

// unwrap_to_array_elem_ssa unwraps Pointer and Alias types to find an Array type,
// converts its elem_type to SSA TypeID, and returns it. Returns 0 if not found.
fn (mut b Builder) unwrap_to_array_elem_ssa(t types.Type) TypeID {
	match t {
		types.Array {
			return b.type_to_ssa(t.elem_type)
		}
		types.Pointer {
			return b.unwrap_to_array_elem_ssa(t.base_type)
		}
		types.Alias {
			base := b.resolve_alias_base_type(t) or { return 0 }
			return b.unwrap_to_array_elem_ssa(base)
		}
		else {
			return 0
		}
	}
}

fn (mut b Builder) checked_struct_field_type_from_ssa(base_type_id TypeID, field_name string) ?types.Type {
	if base_type_id <= 0 || int(base_type_id) >= b.mod.type_store.types.len {
		return none
	}
	mut type_id := base_type_id
	for type_id > 0 && int(type_id) < b.mod.type_store.types.len {
		typ := b.mod.type_store.types[type_id]
		if typ.kind == .ptr_t && typ.elem_type > 0 {
			type_id = typ.elem_type
			continue
		}
		break
	}
	type_name := b.mod.c_struct_names[int(type_id)] or { return none }
	checked_type := b.lookup_checked_type_by_name(type_name) or { return none }
	st := b.unwrap_to_struct(checked_type)
	if st.name == '' {
		return none
	}
	for fi in 0 .. st.fields.len {
		if st.fields[fi].name == field_name {
			return st.fields[fi].typ
		}
	}
	return none
}

fn (mut b Builder) infer_dynamic_array_index_type(expr ast.IndexExpr, base_val ValueID, current_type TypeID) TypeID {
	i64_t := b.mod.type_store.get_int(64)
	mut result_type := current_type
	if result_type == i64_t {
		idx_pos := expr.pos
		if b.env != unsafe { nil } && idx_pos.id != 0 {
			if idx_typ := b.env.get_expr_type(idx_pos.id) {
				inferred := b.type_to_ssa(idx_typ)
				if inferred != 0 && inferred != i64_t {
					result_type = inferred
				}
			}
		}
	}
	if result_type == i64_t {
		if arr_typ := b.get_checked_expr_type(expr.lhs) {
			inferred := b.unwrap_to_array_elem_ssa(arr_typ)
			if inferred != 0 {
				result_type = inferred
			}
		}
	}
	lhs_expr := b.unwrap_index_lhs_expr(expr.lhs)
	if result_type == i64_t && lhs_expr !is ast.EmptyExpr {
		if arr_typ := b.get_checked_expr_type(lhs_expr) {
			inferred := b.unwrap_to_array_elem_ssa(arr_typ)
			if inferred != 0 {
				result_type = inferred
			}
		}
	}
	if result_type == i64_t && lhs_expr is ast.SelectorExpr {
		inferred := b.array_elem_type_from_selector_expr(lhs_expr as ast.SelectorExpr)
		if inferred != 0 {
			result_type = inferred
		}
	}
	if result_type == i64_t {
		if lhs_expr is ast.CallExpr {
			call_lhs := lhs_expr as ast.CallExpr
			if call_lhs.lhs is ast.Ident {
				call_name := (call_lhs.lhs as ast.Ident).name
				if call_name in ['array__slice', 'array__slice_ni'] && call_lhs.args.len >= 1 {
					if arr_typ := b.get_checked_expr_type(call_lhs.args[0]) {
						inferred := b.unwrap_to_array_elem_ssa(arr_typ)
						if inferred != 0 {
							result_type = inferred
						}
					}
				}
			}
		}
	}
	if result_type == i64_t {
		if lhs_expr is ast.Ident {
			if elem_t := b.array_elem_types[lhs_expr.name] {
				result_type = elem_t
			}
		}
	}
	if result_type == i64_t {
		if lhs_expr is ast.Ident {
			if typ := b.checked_local_type(lhs_expr.name) {
				inferred := b.unwrap_to_array_elem_ssa(typ)
				if inferred != 0 {
					result_type = inferred
				}
			}
		}
	}
	if result_type == i64_t {
		if elem_t := b.array_value_elem_types[base_val] {
			result_type = elem_t
		}
	}
	return result_type
}

fn (mut b Builder) finish_index_value(expr ast.IndexExpr, val ValueID) ValueID {
	b.track_array_value_elem_type_from_checked_expr(val, ast.Expr(expr))
	return val
}

fn (mut b Builder) unwrap_index_lhs_expr(expr ast.Expr) ast.Expr {
	mut cur := expr
	for _ in 0 .. 8 {
		match cur {
			ast.ParenExpr {
				cur = cur.expr
				continue
			}
			ast.ModifierExpr {
				cur = cur.expr
				continue
			}
			else {}
		}

		break
	}
	return cur
}

fn (mut b Builder) build_index(expr ast.IndexExpr) ValueID {
	mut base_val := b.build_expr(expr.lhs)
	index := b.build_expr(expr.expr)
	mut result_type := b.expr_type(ast.Expr(expr))

	base_type_id := b.mod.values[base_val].typ
	array_type := b.get_array_type()

	// If base is a pointer to a dynamic array (mut []T param), deref first
	if array_type != 0 && base_type_id != array_type {
		if base_type_id < b.mod.type_store.types.len {
			base_typ := b.mod.type_store.types[base_type_id]
			if base_typ.kind == .ptr_t && base_typ.elem_type == array_type {
				base_val = b.mod.add_instr(.load, b.cur_block, array_type, [base_val])
			}
		}
	}

	// Re-check base type after potential deref
	base_type_id2 := b.mod.values[base_val].typ

	// Check if base is a dynamic array (array struct) — need to access .data field
	if array_type != 0 && base_type_id2 == array_type {
		result_type = b.infer_dynamic_array_index_type(expr, base_val, result_type)
		// Extract .data field (index 0), cast to element pointer, then GEP
		i8_t := b.mod.type_store.get_int(8)
		void_ptr := b.mod.type_store.get_ptr(i8_t)
		data_ptr := b.mod.add_instr(.extractvalue, b.cur_block, void_ptr, [base_val,
			b.mod.get_or_add_const(b.mod.type_store.get_int(32), '0')])
		// Cast void* to element*
		elem_ptr_type := b.mod.type_store.get_ptr(result_type)
		typed_ptr := b.mod.add_instr(.bitcast, b.cur_block, elem_ptr_type, [data_ptr])
		// GEP to the element
		elem_addr := b.mod.add_instr(.get_element_ptr, b.cur_block, elem_ptr_type, [
			typed_ptr,
			index,
		])
		// Load the element
		result := b.mod.add_instr(.load, b.cur_block, result_type, [elem_addr])
		return b.finish_index_value(expr, result)
	}

	// Check if base is a string struct — index into .str (field 0) data pointer
	str_type := b.get_string_type()
	if str_type != 0 && base_type_id2 == str_type {
		// Extract .str field (field 0) — pointer to u8 data
		// Use unsigned u8 type so byte comparisons (>= 0x80) work correctly
		u8_t := b.mod.type_store.get_uint(8)
		u8_ptr := b.mod.type_store.get_ptr(u8_t)
		data_ptr := b.mod.add_instr(.extractvalue, b.cur_block, u8_ptr, [base_val,
			b.mod.get_or_add_const(b.mod.type_store.get_int(32), '0')])
		// GEP to the byte at index (scale = 1 for u8)
		elem_addr := b.mod.add_instr(.get_element_ptr, b.cur_block, u8_ptr, [
			data_ptr,
			index,
		])
		// Load the byte as unsigned u8
		result := b.mod.add_instr(.load, b.cur_block, u8_t, [elem_addr])
		return b.finish_index_value(expr, result)
	}

	// Handle fixed-size array values (from [1,2,3]! load): base is array_t, not ptr_t.
	// Trace back through the load instruction to find the original alloca pointer,
	// then use the pointer-based GEP+load path.
	if base_type_id > 0 && base_type_id < b.mod.type_store.types.len {
		base_typ := b.mod.type_store.types[base_type_id]
		if base_typ.kind == .array_t && base_typ.elem_type != 0 {
			base_value := b.mod.values[base_val]
			if base_value.kind == .instruction {
				instr := b.mod.instrs[base_value.index]
				if instr.op == .load && instr.operands.len > 0 {
					// Found the original alloca pointer — use it for GEP+load
					alloca_ptr := instr.operands[0]
					elem_type := base_typ.elem_type
					elem_ptr_type := b.mod.type_store.get_ptr(elem_type)
					elem_addr := b.mod.add_instr(.get_element_ptr, b.cur_block, elem_ptr_type, [
						alloca_ptr,
						index,
					])
					result := b.mod.add_instr(.load, b.cur_block, elem_type, [
						elem_addr,
					])
					return b.finish_index_value(expr, result)
				}
				if instr.op == .extractvalue && instr.operands.len >= 2 {
					// Base is extractvalue(struct_val, field_idx) producing an array_t.
					// This happens for union/struct field access like u.b[i] where b is [4]u8.
					// The extractvalue produces a VALUE, not a pointer, so GEP can't work on it.
					// Fix: trace back to the struct's alloca, GEP to the field, then index.
					struct_val := instr.operands[0]
					field_idx_val := instr.operands[1]
					struct_value := b.mod.values[struct_val]
					if struct_value.kind == .instruction {
						struct_instr := b.mod.instrs[struct_value.index]
						if struct_instr.op == .load && struct_instr.operands.len > 0 {
							// struct_val came from load(alloca_ptr)
							struct_ptr := struct_instr.operands[0]
							// GEP to the array field within the struct
							arr_ptr_type := b.mod.type_store.get_ptr(base_type_id)
							field_addr := b.mod.add_instr(.get_element_ptr, b.cur_block,
								arr_ptr_type, [struct_ptr, field_idx_val])
							// GEP to the element within the array
							elem_type := base_typ.elem_type
							elem_ptr_type := b.mod.type_store.get_ptr(elem_type)
							elem_addr := b.mod.add_instr(.get_element_ptr, b.cur_block,
								elem_ptr_type, [field_addr, index])
							result := b.mod.add_instr(.load, b.cur_block, elem_type, [
								elem_addr,
							])
							return b.finish_index_value(expr, result)
						}
					}
				}
			}
		}
	}

	// Check if base is a pointer (e.g., from alloca for fixed-size array literal)
	// For ptr(T), element type is T — use that instead of expr_type fallback
	if base_type_id < b.mod.type_store.types.len {
		base_typ := b.mod.type_store.types[base_type_id]
		if base_typ.kind == .ptr_t && base_typ.elem_type != 0 {
			mut elem_type := base_typ.elem_type
			// If pointed-to type is a fixed-size array (array_t), extract its element type.
			// ptr(array(i32, 5))[i] should load an i32, not an array(i32, 5).
			if elem_type < b.mod.type_store.types.len {
				inner_typ := b.mod.type_store.types[elem_type]
				if inner_typ.kind == .array_t && inner_typ.elem_type != 0 {
					elem_type = inner_typ.elem_type
				}
			}
			// GEP to the element address, then load
			elem_ptr_type := b.mod.type_store.get_ptr(elem_type)
			elem_addr := b.mod.add_instr(.get_element_ptr, b.cur_block, elem_ptr_type, [
				base_val,
				index,
			])
			result := b.mod.add_instr(.load, b.cur_block, elem_type, [elem_addr])
			return b.finish_index_value(expr, result)
		}
	}

	result := b.mod.add_instr(.get_element_ptr, b.cur_block, result_type, [base_val, index])
	return b.finish_index_value(expr, result)
}

// infer_if_expr_type tries to infer the result type of a transformer-generated
// IfExpr that has no position annotation (so expr_type returns i64 fallback).
// Checks both then and else branches for type information.
fn (mut b Builder) infer_if_expr_type(node ast.IfExpr, i64_t TypeID) TypeID {
	// Try to infer from branch expressions — recursively walk the entire
	// if-else-if chain so we check ALL branches (not just the first two).
	// This is essential for match expressions lowered to if-else-if chains
	// where the type-bearing expression may be in a deeply nested branch.
	mut branches := [][]ast.Stmt{cap: 8}
	branches << node.stmts
	mut cur_else := node.else_expr
	for {
		if cur_else is ast.IfExpr {
			else_if := cur_else as ast.IfExpr
			branches << else_if.stmts
			cur_else = else_if.else_expr
		} else {
			break
		}
	}
	for branch_stmts in branches {
		if branch_stmts.len == 0 {
			continue
		}
		last := branch_stmts[branch_stmts.len - 1]
		if last !is ast.ExprStmt {
			continue
		}
		// Unwrap PrefixExpr (e.g., -f has the same type as f)
		mut expr := (last as ast.ExprStmt).expr
		for expr is ast.PrefixExpr {
			expr = (expr as ast.PrefixExpr).expr
		}
		if expr is ast.StringLiteral || expr is ast.StringInterLiteral {
			str_type := b.get_string_type()
			if str_type != 0 {
				return str_type
			}
		} else if expr is ast.Ident {
			ident_name := (expr as ast.Ident).name
			if alloca_id := b.vars[ident_name] {
				alloca_val := b.mod.values[alloca_id]
				if alloca_val.typ > 0 && alloca_val.typ < b.mod.type_store.types.len {
					alloca_typ := b.mod.type_store.types[alloca_val.typ]
					if alloca_typ.kind == .ptr_t && alloca_typ.elem_type > 0
						&& alloca_typ.elem_type < b.mod.type_store.types.len {
						elem := b.mod.type_store.types[alloca_typ.elem_type]
						if elem.kind == .struct_t || elem.kind == .float_t {
							return alloca_typ.elem_type
						}
					}
				}
			}
		} else if expr is ast.BasicLiteral {
			bl := expr as ast.BasicLiteral
			if bl.kind == .number && bl.value.contains('.') {
				return b.mod.type_store.get_float(64)
			}
		} else if expr is ast.InitExpr {
			// Sum type init: check the struct type of the InitExpr
			inferred := b.expr_type(expr)
			if inferred != i64_t && inferred != 0 {
				return inferred
			}
			// Try to find the sum type from the field names (_tag, _data)
			init := expr as ast.InitExpr
			if init.fields.len >= 2 {
				for fi in init.fields {
					if fi.name == '_tag' || fi.name == '_data' {
						// This is a sum type init — look up the type from the typ expr
						// (e.g., Ident{name: 'Expr'} → 'ast__Expr' in struct_types)
						type_name := init.typ.name()
						if type_name.len > 0 {
							if tid := b.struct_types[type_name] {
								return tid
							}
							qualified2 := '${b.cur_module}__${type_name}'
							if tid := b.struct_types[qualified2] {
								return tid
							}
							for sname, sid in b.struct_types {
								if sname.ends_with('__${type_name}') {
									return sid
								}
							}
						}
						break
					}
				}
			}
		} else if expr is ast.CallExpr {
			// Check call return type from registered functions
			call_expr := expr as ast.CallExpr
			fn_name := b.resolve_call_name(call_expr)
			if fn_name in b.fn_index {
				fn_idx := b.fn_index[fn_name]
				fn_ret := b.mod.funcs[fn_idx].typ
				if fn_ret != 0 && fn_ret != i64_t {
					return fn_ret
				}
			}
			// Also try expr_type
			inferred := b.expr_type(expr)
			if inferred != i64_t && inferred != 0 {
				return inferred
			}
		} else {
			inferred := b.expr_type(expr)
			if inferred != i64_t && inferred != 0 {
				return inferred
			}
		}
	}
	return i64_t
}

// infer_if_expr_type_from_flat (s232) is the cursor-native mirror of
// infer_if_expr_type. `c` is the IfExpr cursor. It walks the else-if chain
// (edge1 = else_expr) collecting each branch's stmts (edge2..n), inspects the
// last ExprStmt's value (unwrapping PrefixExpr), and infers a type per kind.
// All lookups go through cursor accessors / pos.id, so it is bit-identical to
// the AST helper — including the InitExpr typ-name case: a non-Ident typ
// (SelectorExpr `mod.Type` or a `.typ_*` node) stringifies with a '.' or to
// 'Type', neither of which matches the `__`-joined struct_types keys, so both
// the AST `init.typ.name()` lookups and the cursor's `else -> ''` skip miss
// identically. No decode_expr.
fn (mut b Builder) infer_if_expr_type_from_flat(c ast.Cursor, i64_t TypeID) TypeID {
	// Collect each branch's IfExpr cursor by walking the else-if chain.
	mut branch_cursors := []ast.Cursor{cap: 8}
	branch_cursors << c
	mut cur_else := c.edge(1)
	for cur_else.kind() == .expr_if {
		branch_cursors << cur_else
		cur_else = cur_else.edge(1)
	}
	for bc in branch_cursors {
		n_stmts := bc.edge_count() - 2
		if n_stmts == 0 {
			continue
		}
		last_c := bc.edge(2 + n_stmts - 1)
		if last_c.kind() != .stmt_expr {
			continue
		}
		// Unwrap PrefixExpr (e.g., -f has the same type as f).
		mut expr_c := last_c.edge(0)
		for expr_c.kind() == .expr_prefix {
			expr_c = expr_c.edge(0)
		}
		kind := expr_c.kind()
		if kind == .expr_string || kind == .expr_string_inter {
			str_type := b.get_string_type()
			if str_type != 0 {
				return str_type
			}
		} else if kind == .expr_ident {
			ident_name := expr_c.name()
			if alloca_id := b.vars[ident_name] {
				alloca_val := b.mod.values[alloca_id]
				if alloca_val.typ > 0 && alloca_val.typ < b.mod.type_store.types.len {
					alloca_typ := b.mod.type_store.types[alloca_val.typ]
					if alloca_typ.kind == .ptr_t && alloca_typ.elem_type > 0
						&& alloca_typ.elem_type < b.mod.type_store.types.len {
						elem := b.mod.type_store.types[alloca_typ.elem_type]
						if elem.kind == .struct_t || elem.kind == .float_t {
							return alloca_typ.elem_type
						}
					}
				}
			}
		} else if kind == .expr_basic_literal {
			lit_kind := unsafe { token.Token(int(expr_c.aux())) }
			if lit_kind == .number && expr_c.name().contains('.') {
				return b.mod.type_store.get_float(64)
			}
		} else if kind == .expr_init {
			// Sum type init: check the struct type of the InitExpr.
			inferred := b.expr_type_from_flat(expr_c)
			if inferred != i64_t && inferred != 0 {
				return inferred
			}
			// expr_init flat: edge0=typ, edge1..n = .aux_field_init (name in name_id).
			n_fields := expr_c.edge_count() - 1
			if n_fields >= 2 {
				mut has_tag_or_data := false
				for fi := 0; fi < n_fields; fi++ {
					fname := expr_c.edge(1 + fi).name()
					if fname == '_tag' || fname == '_data' {
						has_tag_or_data = true
						break
					}
				}
				if has_tag_or_data {
					typ_c := expr_c.edge(0)
					type_name := if typ_c.kind() == .expr_ident { typ_c.name() } else { '' }
					if type_name.len > 0 {
						if tid := b.struct_types[type_name] {
							return tid
						}
						qualified2 := '${b.cur_module}__${type_name}'
						if tid := b.struct_types[qualified2] {
							return tid
						}
						for sname, sid in b.struct_types {
							if sname.ends_with('__${type_name}') {
								return sid
							}
						}
					}
				}
			}
		} else if kind == .expr_call {
			// Check call return type from registered functions.
			fn_name := b.resolve_call_name_from_flat(expr_c.edge(0))
			if fn_name in b.fn_index {
				fn_idx := b.fn_index[fn_name]
				fn_ret := b.mod.funcs[fn_idx].typ
				if fn_ret != 0 && fn_ret != i64_t {
					return fn_ret
				}
			}
			inferred := b.expr_type_from_flat(expr_c)
			if inferred != i64_t && inferred != 0 {
				return inferred
			}
		} else {
			inferred := b.expr_type_from_flat(expr_c)
			if inferred != i64_t && inferred != 0 {
				return inferred
			}
		}
	}
	return i64_t
}

fn (mut b Builder) build_if_expr(node ast.IfExpr) ValueID {
	// If used as expression, returns a value
	mut result_type := b.expr_type(ast.Expr(node))
	// If result_type is i64 (fallback), try to infer from branch contents.
	// This handles match-on-string expressions where the transformer creates
	// IfExpr chains without position IDs, so expr_type returns i64 fallback.
	i64_t := b.mod.type_store.get_int(64)
	if result_type == i64_t {
		result_type = b.infer_if_expr_type(node, i64_t)
	}

	then_block := b.mod.add_block(b.cur_func, 'ifx_then')
	merge_block := b.mod.add_block(b.cur_func, 'ifx_merge')
	has_else := node.else_expr !is ast.EmptyExpr
	else_block := if has_else {
		b.mod.add_block(b.cur_func, 'ifx_else')
	} else {
		merge_block
	}

	cond_block := b.cur_block
	cond := b.build_expr(node.cond)
	b.mod.add_instr(.br, b.cur_block, 0,
		[cond, b.mod.blocks[then_block].val_id, b.mod.blocks[else_block].val_id])
	b.add_edge(b.cur_block, then_block)
	b.add_edge(b.cur_block, else_block)

	// Then
	b.cur_block = then_block
	then_smartcasts := b.smartcast_variant_type_ids_from_condition(node.cond)
	mut saved_local_smartcasts := b.local_smartcasts.clone()
	b.apply_local_smartcasts(then_smartcasts)
	mut then_val := ValueID(0)
	if node.stmts.len > 0 {
		for i := 0; i < node.stmts.len - 1; i++ {
			b.build_stmt(node.stmts[i])
		}
		last := node.stmts[node.stmts.len - 1]
		if last is ast.ExprStmt {
			then_val = b.build_expr(last.expr)
		} else {
			b.build_stmt(last)
		}
	}
	b.local_smartcasts = saved_local_smartcasts.move()
	then_end_block := b.cur_block
	mut then_reaches_merge := false
	if !b.block_has_terminator(b.cur_block) {
		b.mod.add_instr(.jmp, b.cur_block, 0, [b.mod.blocks[merge_block].val_id])
		b.add_edge(b.cur_block, merge_block)
		then_reaches_merge = true
	}

	// Else
	mut else_val := ValueID(0)
	mut else_end_block := cond_block
	mut else_reaches_merge := !has_else
	if has_else {
		b.cur_block = else_block
		if node.else_expr is ast.IfExpr {
			else_if := node.else_expr as ast.IfExpr
			if else_if.cond is ast.EmptyExpr {
				// Pure else
				if else_if.stmts.len > 0 {
					for i := 0; i < else_if.stmts.len - 1; i++ {
						b.build_stmt(else_if.stmts[i])
					}
					last := else_if.stmts[else_if.stmts.len - 1]
					if last is ast.ExprStmt {
						else_val = b.build_expr(last.expr)
					} else {
						b.build_stmt(last)
					}
				}
			} else {
				else_val = b.build_if_expr(else_if)
			}
		} else {
			else_val = b.build_expr(node.else_expr)
		}
		else_end_block = b.cur_block
		if !b.block_has_terminator(b.cur_block) {
			b.mod.add_instr(.jmp, b.cur_block, 0, [b.mod.blocks[merge_block].val_id])
			b.add_edge(b.cur_block, merge_block)
			else_reaches_merge = true
		}
	}
	// Merge block: use phi to select result (no alloca/store/load)
	b.cur_block = merge_block
	if b.mod.blocks[merge_block].preds.len == 0 {
		b.mod.add_instr(.unreachable, b.cur_block, 0, []ValueID{})
		return b.mod.get_or_add_const(result_type, '0')
	}

	mut phi_type := result_type
	if then_reaches_merge && else_reaches_merge && then_val != 0 && else_val != 0 {
		then_type := b.mod.values[then_val].typ
		else_type := b.mod.values[else_val].typ
		if then_type != 0 && then_type == else_type {
			phi_type = then_type
		}
	}
	if phi_type == 0 || phi_type == i64_t {
		if then_reaches_merge && then_val != 0 {
			phi_type = b.mod.values[then_val].typ
		}
		if (phi_type == 0 || phi_type == i64_t) && else_reaches_merge && else_val != 0 {
			phi_type = b.mod.values[else_val].typ
		}
		if phi_type == 0 {
			phi_type = i64_t
		}
	}

	zero := b.mod.get_or_add_const(phi_type, '0')
	mut phi_operands := []ValueID{cap: 4}
	if then_reaches_merge {
		then_result := if then_val != 0 { then_val } else { zero }
		phi_operands << then_result
		phi_operands << b.mod.blocks[then_end_block].val_id
	}
	if else_reaches_merge {
		else_result := if else_val != 0 { else_val } else { zero }
		phi_operands << else_result
		phi_operands << b.mod.blocks[else_end_block].val_id
	}
	if phi_operands.len == 0 {
		b.mod.add_instr(.unreachable, b.cur_block, 0, []ValueID{})
		return zero
	}
	return b.mod.add_instr(.phi, merge_block, phi_type, phi_operands)
}

fn match_expr_bool_literal(expr ast.Expr) ?bool {
	match expr {
		ast.BasicLiteral {
			if expr.kind == .key_true {
				return true
			}
			if expr.kind == .key_false {
				return false
			}
		}
		ast.Keyword {
			if expr.tok == .key_true {
				return true
			}
			if expr.tok == .key_false {
				return false
			}
		}
		ast.ParenExpr {
			return match_expr_bool_literal(expr.expr)
		}
		else {}
	}

	return none
}

fn match_bool_branch_cond_expr(conds []ast.Expr, match_value bool) ast.Expr {
	mut branch_cond := ast.Expr(ast.empty_expr)
	for cond in conds {
		mut single_cond := cond
		if !match_value {
			single_cond = ast.Expr(ast.PrefixExpr{
				op:   .not
				expr: cond
				pos:  cond.pos()
			})
		}
		if branch_cond is ast.EmptyExpr {
			branch_cond = single_cond
		} else {
			branch_cond = ast.Expr(ast.InfixExpr{
				op:  .logical_or
				lhs: branch_cond
				rhs: single_cond
				pos: cond.pos()
			})
		}
	}
	return branch_cond
}

fn match_expr_bool_literal_from_flat(c ast.Cursor) ?bool {
	match c.kind() {
		.expr_basic_literal {
			kind := unsafe { token.Token(int(c.aux())) }
			if kind == .key_true {
				return true
			}
			if kind == .key_false {
				return false
			}
		}
		.expr_keyword {
			tok := unsafe { token.Token(int(c.aux())) }
			if tok == .key_true {
				return true
			}
			if tok == .key_false {
				return false
			}
		}
		.expr_paren {
			return match_expr_bool_literal_from_flat(c.edge(0))
		}
		else {}
	}

	return none
}

fn (mut b Builder) build_bool_match_branch_cond_from_flat(conds ast.CursorList, match_value bool) ValueID {
	bool_type := b.mod.type_store.get_int(1)
	mut branch_cond := ValueID(0)
	for i in 0 .. conds.len() {
		mut single_cond := b.build_expr_from_flat(conds.at(i))
		if !match_value {
			single_type := if b.valid_value_id(single_cond) {
				b.mod.values[single_cond].typ
			} else {
				bool_type
			}
			zero := b.mod.get_or_add_const(single_type, '0')
			single_cond = b.mod.add_instr(.eq, b.cur_block, bool_type, [single_cond, zero])
		}
		if branch_cond == 0 {
			branch_cond = single_cond
		} else {
			branch_cond = b.mod.add_instr(.or_, b.cur_block, bool_type, [branch_cond, single_cond])
		}
	}
	if branch_cond == 0 {
		return b.mod.get_or_add_const(bool_type, '0')
	}
	return branch_cond
}

fn (mut b Builder) build_match_branch_value(branch ast.MatchBranch) ValueID {
	if branch.stmts.len == 0 {
		return 0
	}
	for i := 0; i < branch.stmts.len - 1; i++ {
		b.build_stmt(branch.stmts[i])
	}
	last := branch.stmts[branch.stmts.len - 1]
	if last is ast.ExprStmt {
		return b.build_expr(last.expr)
	}
	b.build_stmt(last)
	return 0
}

fn (mut b Builder) build_match_branch_value_from_flat(branch ast.Cursor) ValueID {
	stmts := branch.list_at(1)
	if stmts.len() == 0 {
		return 0
	}
	for i := 0; i < stmts.len() - 1; i++ {
		b.build_stmt_from_flat(stmts.at(i))
	}
	last := stmts.at(stmts.len() - 1)
	if last.kind() == .stmt_expr {
		return b.build_expr_from_flat(last.edge(0))
	}
	b.build_stmt_from_flat(last)
	return 0
}

fn (mut b Builder) match_expr_phi_type(incoming []MatchExprIncoming, fallback TypeID, i64_t TypeID) TypeID {
	mut phi_type := fallback
	if phi_type == 0 || phi_type == i64_t {
		for item in incoming {
			if item.value == 0 || item.value >= b.mod.values.len {
				continue
			}
			value_type := b.mod.values[item.value].typ
			if value_type != 0 {
				phi_type = value_type
				break
			}
		}
	}
	if phi_type == 0 {
		phi_type = i64_t
	}
	return phi_type
}

fn (mut b Builder) build_bool_match_expr(expr ast.MatchExpr, match_value bool) ValueID {
	if expr.branches.len == 0 {
		return b.mod.get_or_add_const(b.mod.type_store.get_int(32), '0')
	}

	i64_t := b.mod.type_store.get_int(64)
	result_type := b.expr_type(ast.Expr(expr))
	merge_block := b.mod.add_block(b.cur_func, 'matchx_merge')
	mut incoming := []MatchExprIncoming{}
	saved_local_smartcasts := b.local_smartcasts.clone()
	mut reached_else := false

	for branch in expr.branches {
		b.local_smartcasts = saved_local_smartcasts.clone()
		if branch.cond.len == 0 {
			value := b.build_match_branch_value(branch)
			end_block := b.cur_block
			if !b.block_has_terminator(b.cur_block) {
				b.mod.add_instr(.jmp, b.cur_block, 0, [b.mod.blocks[merge_block].val_id])
				b.add_edge(b.cur_block, merge_block)
				incoming << MatchExprIncoming{
					value: value
					block: end_block
				}
			}
			reached_else = true
			break
		}

		cond_expr := match_bool_branch_cond_expr(branch.cond, match_value)
		cond_val := b.build_expr(cond_expr)
		then_block := b.mod.add_block(b.cur_func, 'matchx_branch')
		next_block := b.mod.add_block(b.cur_func, 'matchx_next')
		b.mod.add_instr(.br, b.cur_block, 0,
			[cond_val, b.mod.blocks[then_block].val_id, b.mod.blocks[next_block].val_id])
		b.add_edge(b.cur_block, then_block)
		b.add_edge(b.cur_block, next_block)

		b.cur_block = then_block
		b.apply_local_smartcasts(b.smartcast_variant_type_ids_from_condition(cond_expr))
		value := b.build_match_branch_value(branch)
		end_block := b.cur_block
		if !b.block_has_terminator(b.cur_block) {
			b.mod.add_instr(.jmp, b.cur_block, 0, [b.mod.blocks[merge_block].val_id])
			b.add_edge(b.cur_block, merge_block)
			incoming << MatchExprIncoming{
				value: value
				block: end_block
			}
		}
		b.cur_block = next_block
	}

	b.local_smartcasts = saved_local_smartcasts.clone()
	if !reached_else && !b.block_has_terminator(b.cur_block) {
		end_block := b.cur_block
		b.mod.add_instr(.jmp, b.cur_block, 0, [b.mod.blocks[merge_block].val_id])
		b.add_edge(b.cur_block, merge_block)
		incoming << MatchExprIncoming{
			value: 0
			block: end_block
		}
	}

	b.cur_block = merge_block
	if b.mod.blocks[merge_block].preds.len == 0 {
		b.mod.add_instr(.unreachable, b.cur_block, 0, []ValueID{})
		return b.mod.get_or_add_const(result_type, '0')
	}

	phi_type := b.match_expr_phi_type(incoming, result_type, i64_t)
	zero := b.mod.get_or_add_const(phi_type, '0')
	mut phi_operands := []ValueID{cap: incoming.len * 2}
	for item in incoming {
		result := if item.value != 0 { item.value } else { zero }
		phi_operands << result
		phi_operands << b.mod.blocks[item.block].val_id
	}
	if phi_operands.len == 0 {
		b.mod.add_instr(.unreachable, b.cur_block, 0, []ValueID{})
		return zero
	}
	return b.mod.add_instr(.phi, merge_block, phi_type, phi_operands)
}

fn (mut b Builder) build_bool_match_expr_from_flat(c ast.Cursor, match_value bool) ValueID {
	if c.edge_count() <= 1 {
		return b.mod.get_or_add_const(b.mod.type_store.get_int(32), '0')
	}

	i64_t := b.mod.type_store.get_int(64)
	result_type := b.expr_type_from_flat(c)
	merge_block := b.mod.add_block(b.cur_func, 'matchx_merge')
	mut incoming := []MatchExprIncoming{}
	saved_local_smartcasts := b.local_smartcasts.clone()
	mut reached_else := false

	for i in 1 .. c.edge_count() {
		branch := c.edge(i)
		conds := branch.list_at(0)
		b.local_smartcasts = saved_local_smartcasts.clone()
		if conds.len() == 0 {
			value := b.build_match_branch_value_from_flat(branch)
			end_block := b.cur_block
			if !b.block_has_terminator(b.cur_block) {
				b.mod.add_instr(.jmp, b.cur_block, 0, [b.mod.blocks[merge_block].val_id])
				b.add_edge(b.cur_block, merge_block)
				incoming << MatchExprIncoming{
					value: value
					block: end_block
				}
			}
			reached_else = true
			break
		}

		cond_val := b.build_bool_match_branch_cond_from_flat(conds, match_value)
		then_block := b.mod.add_block(b.cur_func, 'matchx_branch')
		next_block := b.mod.add_block(b.cur_func, 'matchx_next')
		b.mod.add_instr(.br, b.cur_block, 0,
			[cond_val, b.mod.blocks[then_block].val_id, b.mod.blocks[next_block].val_id])
		b.add_edge(b.cur_block, then_block)
		b.add_edge(b.cur_block, next_block)

		b.cur_block = then_block
		if match_value && conds.len() == 1 {
			b.apply_local_smartcasts(b.smartcast_variant_type_ids_from_condition_from_flat(conds.at(0)))
		}
		value := b.build_match_branch_value_from_flat(branch)
		end_block := b.cur_block
		if !b.block_has_terminator(b.cur_block) {
			b.mod.add_instr(.jmp, b.cur_block, 0, [b.mod.blocks[merge_block].val_id])
			b.add_edge(b.cur_block, merge_block)
			incoming << MatchExprIncoming{
				value: value
				block: end_block
			}
		}
		b.cur_block = next_block
	}

	b.local_smartcasts = saved_local_smartcasts.clone()
	if !reached_else && !b.block_has_terminator(b.cur_block) {
		end_block := b.cur_block
		b.mod.add_instr(.jmp, b.cur_block, 0, [b.mod.blocks[merge_block].val_id])
		b.add_edge(b.cur_block, merge_block)
		incoming << MatchExprIncoming{
			value: 0
			block: end_block
		}
	}

	return b.build_match_phi(merge_block, incoming, result_type, i64_t)
}

fn (mut b Builder) build_match_phi(merge_block BlockID, incoming []MatchExprIncoming, result_type TypeID, i64_t TypeID) ValueID {
	b.cur_block = merge_block
	if b.mod.blocks[merge_block].preds.len == 0 {
		b.mod.add_instr(.unreachable, b.cur_block, 0, []ValueID{})
		return b.mod.get_or_add_const(result_type, '0')
	}

	phi_type := b.match_expr_phi_type(incoming, result_type, i64_t)
	zero := b.mod.get_or_add_const(phi_type, '0')
	mut phi_operands := []ValueID{cap: incoming.len * 2}
	for item in incoming {
		result := if item.value != 0 { item.value } else { zero }
		phi_operands << result
		phi_operands << b.mod.blocks[item.block].val_id
	}
	if phi_operands.len == 0 {
		b.mod.add_instr(.unreachable, b.cur_block, 0, []ValueID{})
		return zero
	}
	return b.mod.add_instr(.phi, merge_block, phi_type, phi_operands)
}

fn sumtype_variant_index_arg_is_type_shape(expr ast.Expr) bool {
	return match expr {
		ast.Ident, ast.SelectorExpr, ast.GenericArgs, ast.GenericArgOrIndexExpr, ast.Type {
			true
		}
		else {
			false
		}
	}
}

fn sumtype_variant_index_arg_is_type_shape_from_flat(expr ast.Cursor) bool {
	return match expr.kind() {
		.expr_ident, .expr_selector, .typ_array, .typ_array_fixed, .typ_map, .typ_pointer,
		.typ_generic, .typ_option, .typ_result, .expr_generic_args, .expr_generic_arg_or_index {
			true
		}
		else {
			false
		}
	}
}

fn (b &Builder) sumtype_variant_generic_candidate_names(base ast.Expr, params []ast.Expr) []string {
	mut names := []string{}
	generic_name := b.generic_type_name(base, params)
	base_name := b.generic_type_part_name(base)
	if generic_name != '' {
		names << generic_name
	}
	if base_name != '' && base_name !in names {
		names << base_name
	}
	return names
}

fn (b &Builder) sumtype_variant_generic_candidate_names_from_flat(base ast.Cursor, params []ast.Cursor) []string {
	mut names := []string{}
	base_name := b.generic_type_part_name_from_flat(base)
	mut parts := []string{cap: params.len}
	for param in params {
		part := b.generic_type_arg_part_name_from_flat(param)
		if part == '' {
			return []string{}
		}
		parts << part
	}
	generic_name := generic_type_name_from_parts(base_name, parts)
	if generic_name != '' {
		names << generic_name
	}
	if base_name != '' && base_name !in names {
		names << base_name
	}
	return names
}

fn (b &Builder) sumtype_variant_candidate_names(expr ast.Expr) []string {
	match expr {
		ast.ParenExpr {
			return b.sumtype_variant_candidate_names(expr.expr)
		}
		ast.Ident {
			if expr.name == '' {
				return []string{}
			}
			return [expr.name]
		}
		ast.SelectorExpr {
			mut selector_names := []string{}
			if expr.rhs.name != '' {
				if expr.lhs is ast.Ident {
					lhs_name := (expr.lhs as ast.Ident).name
					if lhs_name != '' {
						selector_names << b.generic_selector_type_part_name(expr)
						selector_names << '${lhs_name}__${expr.rhs.name}'
						selector_names << '${lhs_name}.${expr.rhs.name}'
					}
				}
				selector_names << expr.rhs.name
			}
			return selector_names
		}
		ast.GenericType {
			return b.sumtype_variant_generic_candidate_names(expr.name, expr.params)
		}
		ast.GenericArgs {
			return b.sumtype_variant_generic_candidate_names(expr.lhs, expr.args)
		}
		ast.GenericArgOrIndexExpr {
			if !sumtype_variant_index_arg_is_type_shape(expr.expr) {
				return []string{}
			}
			return b.sumtype_variant_generic_candidate_names(expr.lhs, [expr.expr])
		}
		ast.IndexExpr {
			if !sumtype_variant_index_arg_is_type_shape(expr.expr) {
				return []string{}
			}
			return b.sumtype_variant_generic_candidate_names(expr.lhs, [expr.expr])
		}
		ast.InitExpr {
			return b.sumtype_variant_candidate_names(expr.typ)
		}
		ast.CallOrCastExpr {
			return b.sumtype_variant_candidate_names(expr.lhs)
		}
		ast.CastExpr {
			return b.sumtype_variant_candidate_names(expr.typ)
		}
		else {
			return []string{}
		}
	}
}

fn (b &Builder) sumtype_variant_candidate_names_from_flat(expr ast.Cursor) []string {
	match expr.kind() {
		.expr_paren {
			return b.sumtype_variant_candidate_names_from_flat(expr.edge(0))
		}
		.expr_ident {
			name := expr.name()
			if name == '' {
				return []string{}
			}
			return [name]
		}
		.expr_selector {
			rhs := expr.edge(1)
			rhs_name := if rhs.kind() == .expr_ident { rhs.name() } else { '' }
			if rhs_name == '' {
				return []string{}
			}
			mut selector_names := []string{}
			lhs := expr.edge(0)
			if lhs.kind() == .expr_ident {
				lhs_name := lhs.name()
				if lhs_name != '' {
					selector_names << b.generic_selector_type_part_name_from_flat(lhs_name,
						rhs_name)
					selector_names << '${lhs_name}__${rhs_name}'
					selector_names << '${lhs_name}.${rhs_name}'
				}
			}
			selector_names << rhs_name
			return selector_names
		}
		.typ_generic {
			mut params := []ast.Cursor{cap: expr.edge_count() - 1}
			for i in 1 .. expr.edge_count() {
				params << expr.edge(i)
			}
			return b.sumtype_variant_generic_candidate_names_from_flat(expr.edge(0), params)
		}
		.expr_generic_args {
			mut params := []ast.Cursor{cap: expr.edge_count() - 1}
			for i in 1 .. expr.edge_count() {
				params << expr.edge(i)
			}
			return b.sumtype_variant_generic_candidate_names_from_flat(expr.edge(0), params)
		}
		.expr_generic_arg_or_index {
			index_expr := expr.edge(1)
			if !sumtype_variant_index_arg_is_type_shape_from_flat(index_expr) {
				return []string{}
			}
			return b.sumtype_variant_generic_candidate_names_from_flat(expr.edge(0), [
				index_expr,
			])
		}
		.expr_index {
			index_expr := expr.edge(1)
			if !sumtype_variant_index_arg_is_type_shape_from_flat(index_expr) {
				return []string{}
			}
			return b.sumtype_variant_generic_candidate_names_from_flat(expr.edge(0), [
				index_expr,
			])
		}
		.expr_init {
			return b.sumtype_variant_candidate_names_from_flat(expr.edge(0))
		}
		.expr_call_or_cast {
			return b.sumtype_variant_candidate_names_from_flat(expr.edge(0))
		}
		.expr_cast {
			return b.sumtype_variant_candidate_names_from_flat(expr.edge(0))
		}
		else {
			return []string{}
		}
	}
}

fn (mut b Builder) sumtype_variant_type_for_name(sumtype_type TypeID, candidate string) ?TypeID {
	if candidate == '' || b.env == unsafe { nil } {
		return none
	}
	sumtype_name := b.mod.c_struct_names[sumtype_type] or { return none }
	candidate_norm := candidate.replace('.', '__')
	sumtype_info := b.checked_sumtype_for_ssa_name(sumtype_name) or { return none }
	for variant in sumtype_info.get_variants() {
		variant_name := variant.name()
		variant_norm := variant_name.replace('.', '__')
		if ssa_sumtype_variant_name_matches(variant_norm, candidate_norm) {
			if id := b.struct_types[candidate_norm] {
				return id
			}
			if !candidate_norm.contains('__') {
				if last_dunder := sumtype_name.last_index('__') {
					qualified_candidate := '${sumtype_name[..last_dunder]}__${candidate_norm}'
					if id := b.struct_types[qualified_candidate] {
						return id
					}
				}
			}
			return b.type_to_ssa(variant)
		}
	}
	return none
}

fn (mut b Builder) sumtype_variant_type_for_expr(sumtype_type TypeID, expr ast.Expr) ?TypeID {
	candidates := b.sumtype_variant_candidate_names(expr)
	for candidate in candidates {
		if variant_type := b.sumtype_variant_type_for_name(sumtype_type, candidate) {
			return variant_type
		}
	}
	variant_type := b.smartcast_rhs_variant_type_id(expr, true)
	if b.valid_type_id(variant_type) && variant_type != sumtype_type {
		if _ := b.sumtype_variant_tag(sumtype_type, variant_type) {
			return variant_type
		}
	}
	return none
}

fn (mut b Builder) sumtype_variant_type_for_expr_from_flat(sumtype_type TypeID, expr ast.Cursor) ?TypeID {
	candidates := b.sumtype_variant_candidate_names_from_flat(expr)
	for candidate in candidates {
		if variant_type := b.sumtype_variant_type_for_name(sumtype_type, candidate) {
			return variant_type
		}
	}
	variant_type := b.smartcast_rhs_variant_type_id_from_flat(expr, true)
	if b.valid_type_id(variant_type) && variant_type != sumtype_type {
		if _ := b.sumtype_variant_tag(sumtype_type, variant_type) {
			return variant_type
		}
	}
	return none
}

fn (mut b Builder) match_sumtype_branch_info(sumtype_type TypeID, cond ast.Expr) ?MatchSumtypeBranchInfo {
	if cond is ast.ParenExpr {
		return b.match_sumtype_branch_info(sumtype_type, cond.expr)
	}
	variant_type := b.sumtype_variant_type_for_expr(sumtype_type, cond) or { return none }
	if !b.valid_type_id(variant_type) || variant_type == sumtype_type {
		return none
	}
	tag := b.sumtype_variant_tag(sumtype_type, variant_type) or { return none }
	return MatchSumtypeBranchInfo{
		tag:          tag
		variant_type: variant_type
	}
}

fn (mut b Builder) match_sumtype_branch_info_from_flat(sumtype_type TypeID, cond ast.Cursor) ?MatchSumtypeBranchInfo {
	if cond.kind() == .expr_paren {
		return b.match_sumtype_branch_info_from_flat(sumtype_type, cond.edge(0))
	}
	variant_type := b.sumtype_variant_type_for_expr_from_flat(sumtype_type, cond) or { return none }
	if !b.valid_type_id(variant_type) || variant_type == sumtype_type {
		return none
	}
	tag := b.sumtype_variant_tag(sumtype_type, variant_type) or { return none }
	return MatchSumtypeBranchInfo{
		tag:          tag
		variant_type: variant_type
	}
}

fn (mut b Builder) match_sumtype_branch_infos(sumtype_type TypeID, conds []ast.Expr) []MatchSumtypeBranchInfo {
	mut infos := []MatchSumtypeBranchInfo{}
	for cond in conds {
		info := b.match_sumtype_branch_info(sumtype_type, cond) or { continue }
		infos << info
	}
	return infos
}

fn (mut b Builder) match_sumtype_branch_infos_from_flat(sumtype_type TypeID, conds ast.CursorList) []MatchSumtypeBranchInfo {
	mut infos := []MatchSumtypeBranchInfo{}
	for i in 0 .. conds.len() {
		info := b.match_sumtype_branch_info_from_flat(sumtype_type, conds.at(i)) or { continue }
		infos << info
	}
	return infos
}

fn (mut b Builder) build_sumtype_match_condition(tag_val ValueID, infos []MatchSumtypeBranchInfo) ValueID {
	bool_type := b.mod.type_store.get_int(1)
	if !b.valid_value_id(tag_val) || infos.len == 0 {
		return b.mod.get_or_add_const(bool_type, '0')
	}
	tag_type := b.mod.values[tag_val].typ
	mut cond_val := ValueID(0)
	for info in infos {
		tag_const := b.mod.get_or_add_const(tag_type, info.tag.str())
		eq_val := b.mod.add_instr(.eq, b.cur_block, bool_type, [tag_val, tag_const])
		if cond_val == 0 {
			cond_val = eq_val
		} else {
			cond_val = b.mod.add_instr(.or_, b.cur_block, bool_type, [cond_val, eq_val])
		}
	}
	return cond_val
}

fn (mut b Builder) apply_sumtype_match_smartcast(subject ast.Expr, infos []MatchSumtypeBranchInfo) {
	if infos.len != 1 {
		return
	}
	expr_name := b.smartcast_expr_name(subject)
	if expr_name != '' {
		b.local_smartcasts[expr_name] = infos[0].variant_type
	}
}

fn (mut b Builder) apply_sumtype_match_smartcast_from_flat(subject ast.Cursor, infos []MatchSumtypeBranchInfo) {
	if infos.len != 1 {
		return
	}
	expr_name := b.smartcast_expr_name_from_flat(subject)
	if expr_name != '' {
		b.local_smartcasts[expr_name] = infos[0].variant_type
	}
}

fn (mut b Builder) build_sumtype_match_expr(expr ast.MatchExpr, subject ValueID, subject_type TypeID) ValueID {
	if expr.branches.len == 0 {
		return b.mod.get_or_add_const(b.mod.type_store.get_int(32), '0')
	}
	i64_t := b.mod.type_store.get_int(64)
	result_type := b.expr_type(ast.Expr(expr))
	type_info := b.mod.type_store.types[subject_type]
	tag_type := if type_info.fields.len > 0 { type_info.fields[0] } else { i64_t }
	tag_idx := b.mod.get_or_add_const(b.mod.type_store.get_int(32), '0')
	tag_val := b.mod.add_instr(.extractvalue, b.cur_block, tag_type, [subject, tag_idx])
	merge_block := b.mod.add_block(b.cur_func, 'matchx_merge')
	mut incoming := []MatchExprIncoming{}
	saved_local_smartcasts := b.local_smartcasts.clone()
	mut reached_else := false

	for branch in expr.branches {
		b.local_smartcasts = saved_local_smartcasts.clone()
		if branch.cond.len == 0 {
			value := b.build_match_branch_value(branch)
			end_block := b.cur_block
			if !b.block_has_terminator(b.cur_block) {
				b.mod.add_instr(.jmp, b.cur_block, 0, [b.mod.blocks[merge_block].val_id])
				b.add_edge(b.cur_block, merge_block)
				incoming << MatchExprIncoming{
					value: value
					block: end_block
				}
			}
			reached_else = true
			break
		}

		infos := b.match_sumtype_branch_infos(subject_type, branch.cond)
		cond_val := b.build_sumtype_match_condition(tag_val, infos)
		then_block := b.mod.add_block(b.cur_func, 'matchx_branch')
		next_block := b.mod.add_block(b.cur_func, 'matchx_next')
		b.mod.add_instr(.br, b.cur_block, 0,
			[cond_val, b.mod.blocks[then_block].val_id, b.mod.blocks[next_block].val_id])
		b.add_edge(b.cur_block, then_block)
		b.add_edge(b.cur_block, next_block)

		b.cur_block = then_block
		b.apply_sumtype_match_smartcast(expr.expr, infos)
		value := b.build_match_branch_value(branch)
		end_block := b.cur_block
		if !b.block_has_terminator(b.cur_block) {
			b.mod.add_instr(.jmp, b.cur_block, 0, [b.mod.blocks[merge_block].val_id])
			b.add_edge(b.cur_block, merge_block)
			incoming << MatchExprIncoming{
				value: value
				block: end_block
			}
		}
		b.cur_block = next_block
	}

	b.local_smartcasts = saved_local_smartcasts.clone()
	if !reached_else && !b.block_has_terminator(b.cur_block) {
		end_block := b.cur_block
		b.mod.add_instr(.jmp, b.cur_block, 0, [b.mod.blocks[merge_block].val_id])
		b.add_edge(b.cur_block, merge_block)
		incoming << MatchExprIncoming{
			value: 0
			block: end_block
		}
	}

	return b.build_match_phi(merge_block, incoming, result_type, i64_t)
}

fn (mut b Builder) build_sumtype_match_expr_from_flat(c ast.Cursor, subject ValueID, subject_type TypeID) ValueID {
	if c.edge_count() <= 1 {
		return b.mod.get_or_add_const(b.mod.type_store.get_int(32), '0')
	}
	i64_t := b.mod.type_store.get_int(64)
	result_type := b.expr_type_from_flat(c)
	type_info := b.mod.type_store.types[subject_type]
	tag_type := if type_info.fields.len > 0 { type_info.fields[0] } else { i64_t }
	tag_idx := b.mod.get_or_add_const(b.mod.type_store.get_int(32), '0')
	tag_val := b.mod.add_instr(.extractvalue, b.cur_block, tag_type, [subject, tag_idx])
	merge_block := b.mod.add_block(b.cur_func, 'matchx_merge')
	mut incoming := []MatchExprIncoming{}
	saved_local_smartcasts := b.local_smartcasts.clone()
	mut reached_else := false
	subject_c := c.edge(0)

	for i in 1 .. c.edge_count() {
		branch := c.edge(i)
		conds := branch.list_at(0)
		b.local_smartcasts = saved_local_smartcasts.clone()
		if conds.len() == 0 {
			value := b.build_match_branch_value_from_flat(branch)
			end_block := b.cur_block
			if !b.block_has_terminator(b.cur_block) {
				b.mod.add_instr(.jmp, b.cur_block, 0, [b.mod.blocks[merge_block].val_id])
				b.add_edge(b.cur_block, merge_block)
				incoming << MatchExprIncoming{
					value: value
					block: end_block
				}
			}
			reached_else = true
			break
		}

		infos := b.match_sumtype_branch_infos_from_flat(subject_type, conds)
		cond_val := b.build_sumtype_match_condition(tag_val, infos)
		then_block := b.mod.add_block(b.cur_func, 'matchx_branch')
		next_block := b.mod.add_block(b.cur_func, 'matchx_next')
		b.mod.add_instr(.br, b.cur_block, 0,
			[cond_val, b.mod.blocks[then_block].val_id, b.mod.blocks[next_block].val_id])
		b.add_edge(b.cur_block, then_block)
		b.add_edge(b.cur_block, next_block)

		b.cur_block = then_block
		b.apply_sumtype_match_smartcast_from_flat(subject_c, infos)
		value := b.build_match_branch_value_from_flat(branch)
		end_block := b.cur_block
		if !b.block_has_terminator(b.cur_block) {
			b.mod.add_instr(.jmp, b.cur_block, 0, [b.mod.blocks[merge_block].val_id])
			b.add_edge(b.cur_block, merge_block)
			incoming << MatchExprIncoming{
				value: value
				block: end_block
			}
		}
		b.cur_block = next_block
	}

	b.local_smartcasts = saved_local_smartcasts.clone()
	if !reached_else && !b.block_has_terminator(b.cur_block) {
		end_block := b.cur_block
		b.mod.add_instr(.jmp, b.cur_block, 0, [b.mod.blocks[merge_block].val_id])
		b.add_edge(b.cur_block, merge_block)
		incoming << MatchExprIncoming{
			value: 0
			block: end_block
		}
	}

	return b.build_match_phi(merge_block, incoming, result_type, i64_t)
}

fn (mut b Builder) build_match_expr(expr ast.MatchExpr) ValueID {
	if match_value := match_expr_bool_literal(expr.expr) {
		return b.build_bool_match_expr(expr, match_value)
	}
	subject := b.build_expr(expr.expr)
	if b.valid_value_id(subject) {
		subject_type := b.mod.values[subject].typ
		if b.ssa_type_is_sumtype(subject_type) {
			return b.build_sumtype_match_expr(expr, subject, subject_type)
		}
	}
	return b.mod.get_or_add_const(b.mod.type_store.get_int(32), '0')
}

fn (mut b Builder) build_match_expr_from_flat(c ast.Cursor) ValueID {
	subject_c := c.edge(0)
	if match_value := match_expr_bool_literal_from_flat(subject_c) {
		return b.build_bool_match_expr_from_flat(c, match_value)
	}
	subject := b.build_expr_from_flat(subject_c)
	if b.valid_value_id(subject) {
		subject_type := b.mod.values[subject].typ
		if b.ssa_type_is_sumtype(subject_type) {
			return b.build_sumtype_match_expr_from_flat(c, subject, subject_type)
		}
	}
	return b.mod.get_or_add_const(b.mod.type_store.get_int(32), '0')
}

// build_if_expr_from_flat (s231, s232) is the cursor-native port of
// build_if_expr. IfExpr flat encoding: (.expr_if, [edge0=cond, edge1=else_expr,
// edge2..n=stmts]). Cond/stmts/else-chain/phi are built from cursors via
// build_expr_from_flat / build_stmt_from_flat / smartcast_variant_type_ids_from_condition_from_flat
// (s230) and recursion through this function; the i64-fallback type inference
// uses infer_if_expr_type_from_flat (s232). Fully decode-free.
fn (mut b Builder) build_if_expr_from_flat(c ast.Cursor) ValueID {
	cond_c := c.edge(0)
	else_c := c.edge(1)
	n_stmts := c.edge_count() - 2

	// If used as expression, returns a value
	mut result_type := b.expr_type_from_flat(c)
	i64_t := b.mod.type_store.get_int(64)
	if result_type == i64_t {
		// s232: cursor-native inference — no decode.
		result_type = b.infer_if_expr_type_from_flat(c, i64_t)
	}

	then_block := b.mod.add_block(b.cur_func, 'ifx_then')
	merge_block := b.mod.add_block(b.cur_func, 'ifx_merge')
	has_else := else_c.kind() != .expr_empty
	else_block := if has_else {
		b.mod.add_block(b.cur_func, 'ifx_else')
	} else {
		merge_block
	}

	cond_block := b.cur_block
	cond := b.build_expr_from_flat(cond_c)
	b.mod.add_instr(.br, b.cur_block, 0,
		[cond, b.mod.blocks[then_block].val_id, b.mod.blocks[else_block].val_id])
	b.add_edge(b.cur_block, then_block)
	b.add_edge(b.cur_block, else_block)

	// Then
	b.cur_block = then_block
	then_smartcasts := b.smartcast_variant_type_ids_from_condition_from_flat(cond_c)
	mut saved_local_smartcasts := b.local_smartcasts.clone()
	b.apply_local_smartcasts(then_smartcasts)
	mut then_val := ValueID(0)
	if n_stmts > 0 {
		for i := 0; i < n_stmts - 1; i++ {
			b.build_stmt_from_flat(c.edge(2 + i))
		}
		last_c := c.edge(2 + n_stmts - 1)
		if last_c.kind() == .stmt_expr {
			then_val = b.build_expr_from_flat(last_c.edge(0))
		} else {
			b.build_stmt_from_flat(last_c)
		}
	}
	b.local_smartcasts = saved_local_smartcasts.move()
	then_end_block := b.cur_block
	mut then_reaches_merge := false
	if !b.block_has_terminator(b.cur_block) {
		b.mod.add_instr(.jmp, b.cur_block, 0, [b.mod.blocks[merge_block].val_id])
		b.add_edge(b.cur_block, merge_block)
		then_reaches_merge = true
	}

	// Else
	mut else_val := ValueID(0)
	mut else_end_block := cond_block
	mut else_reaches_merge := !has_else
	if has_else {
		b.cur_block = else_block
		if else_c.kind() == .expr_if {
			else_cond_c := else_c.edge(0)
			if else_cond_c.kind() == .expr_empty {
				// Pure else
				else_n_stmts := else_c.edge_count() - 2
				if else_n_stmts > 0 {
					for i := 0; i < else_n_stmts - 1; i++ {
						b.build_stmt_from_flat(else_c.edge(2 + i))
					}
					last_c := else_c.edge(2 + else_n_stmts - 1)
					if last_c.kind() == .stmt_expr {
						else_val = b.build_expr_from_flat(last_c.edge(0))
					} else {
						b.build_stmt_from_flat(last_c)
					}
				}
			} else {
				else_val = b.build_if_expr_from_flat(else_c)
			}
		} else {
			else_val = b.build_expr_from_flat(else_c)
		}
		else_end_block = b.cur_block
		if !b.block_has_terminator(b.cur_block) {
			b.mod.add_instr(.jmp, b.cur_block, 0, [b.mod.blocks[merge_block].val_id])
			b.add_edge(b.cur_block, merge_block)
			else_reaches_merge = true
		}
	}
	// Merge block: use phi to select result (no alloca/store/load)
	b.cur_block = merge_block
	if b.mod.blocks[merge_block].preds.len == 0 {
		b.mod.add_instr(.unreachable, b.cur_block, 0, []ValueID{})
		return b.mod.get_or_add_const(result_type, '0')
	}

	mut phi_type := result_type
	if then_reaches_merge && else_reaches_merge && then_val != 0 && else_val != 0 {
		then_type := b.mod.values[then_val].typ
		else_type := b.mod.values[else_val].typ
		if then_type != 0 && then_type == else_type {
			phi_type = then_type
		}
	}
	if phi_type == 0 || phi_type == i64_t {
		if then_reaches_merge && then_val != 0 {
			phi_type = b.mod.values[then_val].typ
		}
		if (phi_type == 0 || phi_type == i64_t) && else_reaches_merge && else_val != 0 {
			phi_type = b.mod.values[else_val].typ
		}
		if phi_type == 0 {
			phi_type = i64_t
		}
	}

	zero := b.mod.get_or_add_const(phi_type, '0')
	mut phi_operands := []ValueID{cap: 4}
	if then_reaches_merge {
		then_result := if then_val != 0 { then_val } else { zero }
		phi_operands << then_result
		phi_operands << b.mod.blocks[then_end_block].val_id
	}
	if else_reaches_merge {
		else_result := if else_val != 0 { else_val } else { zero }
		phi_operands << else_result
		phi_operands << b.mod.blocks[else_end_block].val_id
	}
	if phi_operands.len == 0 {
		b.mod.add_instr(.unreachable, b.cur_block, 0, []ValueID{})
		return zero
	}
	return b.mod.add_instr(.phi, merge_block, phi_type, phi_operands)
}

fn (mut b Builder) build_array_init_expr(expr ast.ArrayInitExpr) ValueID {
	array_elem_hint := b.array_literal_elem_type_hint
	b.array_literal_elem_type_hint = 0
	// If the array init has elements, alloca a fixed-size array on the stack,
	// store each element, and return the pointer.
	if expr.exprs.len > 0 {
		mut elem_vals := []ValueID{cap: expr.exprs.len}
		for e in expr.exprs {
			if !builder_expr_ok(e) {
				elem_vals << ValueID(0)
				continue
			}
			elem_vals << b.build_expr(e)
		}
		mut elem_type := b.mod.type_store.get_int(32) // default int
		// Try to get the declared element type from the array type annotation.
		// This is important when the value type is narrower than the element type
		// (e.g., pushing u8 into []rune where rune is i32).
		mut has_declared_type := false
		if builder_expr_ok(expr.typ) && expr.typ is ast.Type {
			expr_typ := expr.typ as ast.Type
			match expr_typ {
				ast.ArrayType {
					declared_elem := b.ast_type_to_ssa(expr_typ.elem_type)
					if declared_elem > 0 {
						elem_type = declared_elem
						has_declared_type = true
					}
				}
				ast.ArrayFixedType {
					declared_elem := b.ast_type_to_ssa(expr_typ.elem_type)
					if declared_elem > 0 {
						elem_type = declared_elem
						has_declared_type = true
					}
				}
				else {}
			}
		}
		if array_elem_hint != 0 {
			elem_type = array_elem_hint
			has_declared_type = true
		}
		if !has_declared_type {
			if checked_type := b.get_checked_expr_type(ast.Expr(expr)) {
				checked_base := b.unwrap_alias_type(checked_type)
				match checked_base {
					types.Array {
						checked_elem := b.type_to_ssa(checked_base.elem_type)
						if checked_elem > 0 {
							elem_type = checked_elem
							has_declared_type = true
						}
					}
					types.ArrayFixed {
						checked_elem := b.type_to_ssa(checked_base.elem_type)
						if checked_elem > 0 {
							elem_type = checked_elem
							has_declared_type = true
						}
					}
					else {}
				}
			}
		}
		if !has_declared_type && elem_vals.len > 0 {
			for i, val in elem_vals {
				if b.array_literal_value_type_can_infer_elem(expr.exprs[i], val) {
					elem_type = b.mod.values[val].typ
					break
				}
			}
		}
		if !b.valid_type_id(elem_type) {
			elem_type = b.mod.type_store.get_int(32)
		}
		if elem_vals.len > 0 {
			zero := b.mod.get_or_add_const(elem_type, '0')
			for i, val in elem_vals {
				if !b.valid_value_id(val) {
					elem_vals[i] = zero
				}
			}
		}
		// Convert element values to match the declared/inferred element type.
		// This handles: int→wider int (zext), int→float (sitofp), f64→f32 (trunc).
		{
			elem_kind := b.mod.type_store.types[elem_type].kind
			elem_width := b.mod.type_store.types[elem_type].width
			for i, val in elem_vals {
				val_type := b.mod.values[val].typ
				if val_type == elem_type {
					continue
				}
				if wrapped := b.wrap_value_for_sumtype_target(val, elem_type) {
					elem_vals[i] = wrapped
					continue
				}
				val_kind := b.mod.type_store.types[val_type].kind
				val_width := b.mod.type_store.types[val_type].width
				if val_kind == .int_t && elem_kind == .float_t {
					// int → float (e.g., 15 in []f32 → f32(15.0))
					elem_vals[i] = b.mod.add_instr(.sitofp, b.cur_block, elem_type, [
						val,
					])
				} else if val_kind == .float_t && elem_kind == .float_t && val_width > elem_width {
					// f64 → f32 narrowing
					elem_vals[i] = b.mod.add_instr(.trunc, b.cur_block, elem_type, [
						val,
					])
				} else if val_kind == .float_t && elem_kind == .float_t && val_width < elem_width {
					// f32 → f64 widening
					elem_vals[i] = b.mod.add_instr(.zext, b.cur_block, elem_type, [
						val,
					])
				} else if val_kind == .int_t && elem_kind == .int_t && val_width > 0
					&& val_width < elem_width {
					// int → wider int (e.g., u8 in []rune)
					elem_vals[i] = b.mod.add_instr(.zext, b.cur_block, elem_type, [
						val,
					])
				}
			}
		}
		// Allocate fixed-size array on stack and store each element.
		// Use array_t so that GEP uses element-size scaling (not struct-field offsets).
		arr_fixed_type := b.mod.type_store.get_array(elem_type, elem_vals.len)
		ptr_type := b.mod.type_store.get_ptr(arr_fixed_type)
		alloca := b.mod.add_instr(.alloca, b.cur_block, ptr_type, []ValueID{})
		// Store each element via GEP + store.
		// GEP result type is ptr(elem_type) - a pointer to one element, not the whole array.
		elem_ptr_type := b.mod.type_store.get_ptr(elem_type)
		i32_t := b.mod.type_store.get_int(32)
		for i, val in elem_vals {
			idx := b.mod.get_or_add_const(i32_t, i.str())
			gep := b.mod.add_instr(.get_element_ptr, b.cur_block, elem_ptr_type, [
				alloca,
				idx,
			])
			b.mod.add_instr(.store, b.cur_block, 0, [val, gep])
		}
		// For fixed arrays ([1, 2]!), return the array VALUE (not the pointer).
		// This ensures variables store actual array data so that:
		// 1. &a gives the address of the data (the variable alloca), enabling memcmp
		// 2. a == b compares values, not pointer addresses
		// The ARM64 backend handles array_t values > 8 bytes via memcpy-style
		// load/store, similar to struct_t handling.
		// build_index handles indexing into array_t values by tracing back to
		// the original alloca pointer.
		mut is_fixed := false
		if builder_expr_ok(expr.len) && expr.len is ast.PostfixExpr {
			postfix := expr.len as ast.PostfixExpr
			if postfix.op == .not {
				is_fixed = true
			}
		}
		if !is_fixed && builder_expr_ok(expr.typ) && expr.typ is ast.Type
			&& expr.typ is ast.ArrayFixedType {
			is_fixed = true
		}
		if is_fixed {
			return b.mod.add_instr(.load, b.cur_block, arr_fixed_type, [alloca])
		}
		if b.array_literal_as_element_buffer {
			return alloca
		}
		return b.build_dynamic_array_from_element_buffer(elem_type, elem_vals, alloca)
	}

	// Check if this is a fixed-size array type (e.g., [5]u8{}).
	// These need stack allocation via alloca, not a dynamic array struct.
	if builder_expr_ok(expr.typ) && expr.typ is ast.Type {
		expr_typ := expr.typ as ast.Type
		match expr_typ {
			ast.ArrayFixedType {
				elem_type := b.ast_type_to_ssa(expr_typ.elem_type)
				arr_len := if builder_expr_ok(expr_typ.len) && expr_typ.len is ast.BasicLiteral {
					int(parse_const_int_literal(expr_typ.len.value))
				} else if builder_expr_ok(expr_typ.len) && expr_typ.len is ast.Ident {
					b.resolve_const_int(expr_typ.len.name)
				} else {
					0
				}
				if arr_len > 0 {
					arr_fixed_type := b.mod.type_store.get_array(elem_type, arr_len)
					ptr_type := b.mod.type_store.get_ptr(arr_fixed_type)
					alloca := b.mod.add_instr(.alloca, b.cur_block, ptr_type, []ValueID{})
					if !b.native_backend_bulk_zero_alloca
						|| arr_len <= fixed_array_empty_literal_element_store_threshold {
						zero := b.mod.get_or_add_const(elem_type, '0')
						elem_ptr_type := b.mod.type_store.get_ptr(elem_type)
						i32_t := b.mod.type_store.get_int(32)
						for i in 0 .. arr_len {
							idx := b.mod.get_or_add_const(i32_t, i.str())
							gep := b.mod.add_instr(.get_element_ptr, b.cur_block, elem_ptr_type, [
								alloca,
								idx,
							])
							b.mod.add_instr(.store, b.cur_block, 0, [zero, gep])
						}
					}
					return b.mod.add_instr(.load, b.cur_block, arr_fixed_type, [
						alloca,
					])
				}
			}
			else {}
		}
	}

	if builder_expr_ok(expr.typ) && expr.typ is ast.Type && expr.init is ast.EmptyExpr
		&& expr.update_expr is ast.EmptyExpr {
		expr_typ := expr.typ as ast.Type
		if expr_typ is ast.ArrayType {
			elem_type := if array_elem_hint != 0 {
				array_elem_hint
			} else {
				b.ast_type_to_ssa(expr_typ.elem_type)
			}
			len_val := if expr.len is ast.EmptyExpr {
				ValueID(0)
			} else {
				b.build_expr(expr.len)
			}
			cap_val := if expr.cap is ast.EmptyExpr {
				ValueID(0)
			} else {
				b.build_expr(expr.cap)
			}
			return b.build_empty_dynamic_array(elem_type, len_val, cap_val)
		}
	}

	// Empty dynamic array with len/cap - these should have been
	// transformed to __new_array_with_default_noscan calls by the transformer.
	// Return zero-initialized array struct as fallback.
	arr_type := b.get_array_type()
	return b.mod.get_or_add_const(arr_type, '0')
}

fn (mut b Builder) zero_struct_init_field_value(field_type TypeID) ValueID {
	if b.is_wrapper_type(field_type) {
		return b.build_wrapper_value(field_type, false, 0, false)
	}
	return b.mod.get_or_add_const(field_type, '0')
}

fn (mut b Builder) coerce_struct_init_field_storage_value(field_val ValueID, field_type TypeID) ValueID {
	if field_val <= 0 || field_val >= b.mod.values.len {
		return field_val
	}
	mut coerced := field_val
	val_typ_id := b.mod.values[coerced].typ
	if val_typ_id > 0 && val_typ_id < b.mod.type_store.types.len && field_type > 0
		&& field_type < b.mod.type_store.types.len {
		val_typ := b.mod.type_store.types[val_typ_id]
		fld_typ := b.mod.type_store.types[field_type]
		if val_typ.kind == .ptr_t && fld_typ.kind == .struct_t {
			coerced = b.mod.add_instr(.load, b.cur_block, field_type, [coerced])
		}
	}
	return b.coerce_struct_sumtype_field_value(coerced, field_type)
}

fn (mut b Builder) build_struct_init_field_expr_value(value_expr ast.Expr, field_type TypeID, is_sumtype_data bool) ValueID {
	if is_sumtype_data {
		b.in_sumtype_data = true
	}
	mut field_val := ValueID(0)
	if b.ssa_type_is_sumtype(field_type) {
		addr := b.build_addr(value_expr)
		if wrapped := b.wrap_address_for_sumtype_target(addr, field_type) {
			field_val = wrapped
		}
	}
	if field_val == 0 {
		field_val = b.build_expr(value_expr)
	}
	if is_sumtype_data {
		b.in_sumtype_data = false
	}
	if b.is_wrapper_type(field_type) {
		field_val = b.coerce_wrapper_value(value_expr, field_val, field_type)
	}
	return b.coerce_struct_init_field_storage_value(field_val, field_type)
}

fn (mut b Builder) build_struct_init_field_expr_value_from_flat(value_c ast.Cursor, field_type TypeID, is_sumtype_data bool) ValueID {
	if is_sumtype_data {
		b.in_sumtype_data = true
	}
	mut field_val := ValueID(0)
	if b.ssa_type_is_sumtype(field_type) {
		addr := b.build_addr_from_flat(value_c)
		if wrapped := b.wrap_address_for_sumtype_target(addr, field_type) {
			field_val = wrapped
		}
	}
	if field_val == 0 {
		field_val = b.build_expr_from_flat(value_c)
	}
	if is_sumtype_data {
		b.in_sumtype_data = false
	}
	if b.is_wrapper_type(field_type) {
		field_val = b.coerce_wrapper_value_from_flat(value_c, field_val, field_type)
	}
	return b.coerce_struct_init_field_storage_value(field_val, field_type)
}

fn (b &Builder) embedded_span_for_positional_init(struct_type TypeID, source_type TypeID, target_idx int) ?EmbeddedFieldSpan {
	if source_type <= 0 || source_type >= b.mod.type_store.types.len {
		return none
	}
	spans := b.struct_embedded_spans[int(struct_type)] or { return none }
	for span in spans {
		if span.start != target_idx || span.source_type != source_type || span.len <= 0 {
			continue
		}
		if span.start + span.len > b.mod.type_store.types[struct_type].fields.len {
			continue
		}
		return span
	}
	return none
}

fn (b &Builder) flat_embedded_default_field_name_is_redundant(struct_type TypeID, field_name string) bool {
	if !field_name.contains('.') {
		return false
	}
	embedded_type_name := field_name.all_before_last('.')
	embedded_field_name := field_name.all_after_last('.')
	spans := b.struct_embedded_spans[int(struct_type)] or { return false }
	for span in spans {
		if span.source_type <= 0 || span.source_type >= b.mod.type_store.types.len {
			continue
		}
		source_name := b.mod.c_struct_names[span.source_type] or { '' }
		if embedded_type_name != source_name.all_after_last('__') {
			continue
		}
		source_info := b.mod.type_store.types[span.source_type]
		for known_field_name in source_info.field_names {
			if embedded_field_name == known_field_name {
				return true
			}
		}
	}
	return false
}

fn (b &Builder) flat_init_has_positional_fields_with_embedded_defaults(c ast.Cursor, struct_type TypeID, nfields int) bool {
	if nfields == 0 {
		return false
	}
	mut saw_positional := false
	for i in 0 .. nfields {
		field_name := c.edge(1 + i).name()
		if field_name.len == 0 {
			saw_positional = true
			continue
		}
		if saw_positional
			&& b.flat_embedded_default_field_name_is_redundant(struct_type, field_name) {
			continue
		}
		return false
	}
	return saw_positional
}

fn (mut b Builder) append_embedded_struct_init_value(mut field_vals []ValueID, field_val ValueID, span EmbeddedFieldSpan) bool {
	if field_val <= 0 || field_val >= b.mod.values.len {
		return false
	}
	val_type := b.mod.values[field_val].typ
	i32_t := b.mod.type_store.get_int(32)
	source_info := b.mod.type_store.types[span.source_type]
	if val_type == span.source_type {
		for i in 0 .. span.len {
			source_field_type := source_info.fields[i]
			field_vals << b.mod.add_instr(.extractvalue, b.cur_block, source_field_type, [
				field_val,
				b.mod.get_or_add_const(i32_t, i.str()),
			])
		}
		return true
	}
	if val_type <= 0 || val_type >= b.mod.type_store.types.len {
		return false
	}
	val_type_info := b.mod.type_store.types[val_type]
	if val_type_info.kind != .ptr_t || val_type_info.elem_type != span.source_type {
		return false
	}
	for i in 0 .. span.len {
		source_field_type := source_info.fields[i]
		idx_val := b.mod.get_or_add_const(i32_t, i.str())
		field_ptr_type := b.mod.type_store.get_ptr(source_field_type)
		field_ptr := b.mod.add_instr(.get_element_ptr, b.cur_block, field_ptr_type, [
			field_val,
			idx_val,
		])
		field_vals << b.mod.add_instr(.load, b.cur_block, source_field_type, [
			field_ptr,
		])
	}
	return true
}

fn (mut b Builder) embedded_init_selector_call_name(lhs_name string, rhs_name string) string {
	if lhs_name == '' || rhs_name == '' || !ssa_string_ok(lhs_name) || !ssa_string_ok(rhs_name) {
		return ''
	}
	if resolved_mod := b.module_import_aliases[lhs_name] {
		qualified := imported_symbol_fn_name(resolved_mod, rhs_name)
		if qualified in b.fn_index {
			return qualified
		}
	}
	qualified := '${lhs_name}__${rhs_name}'
	if qualified in b.fn_index {
		return qualified
	}
	if resolved_mod := b.selector_module_name_for_ident(lhs_name) {
		resolved_qualified := imported_symbol_fn_name(resolved_mod, rhs_name)
		if resolved_qualified in b.fn_index {
			return resolved_qualified
		}
	}
	return ''
}

fn (mut b Builder) embedded_init_call_return_type_from_name(fn_name string) TypeID {
	if fn_idx := b.fn_index[fn_name] {
		ret_type := b.mod.funcs[fn_idx].typ
		if b.valid_type_id(ret_type) {
			return ret_type
		}
	}
	return TypeID(0)
}

fn (mut b Builder) embedded_init_build_resolved_call(fn_name string, args []ast.Expr) ValueID {
	call_expr := ast.CallExpr{
		lhs:  ast.Ident{
			name: fn_name
		}
		args: args
	}
	return b.build_call_resolved(fn_name, call_expr)
}

fn (mut b Builder) embedded_init_build_source_expr_value(value_expr ast.Expr) ValueID {
	if value_expr is ast.CallExpr {
		if value_expr.lhs is ast.SelectorExpr {
			sel := value_expr.lhs as ast.SelectorExpr
			if sel.lhs is ast.Ident {
				lhs_name := (sel.lhs as ast.Ident).name
				fn_name := b.embedded_init_selector_call_name(lhs_name, sel.rhs.name)
				if fn_name != '' {
					return b.embedded_init_build_resolved_call(fn_name, value_expr.args)
				}
			}
		}
	} else if value_expr is ast.CallOrCastExpr {
		if value_expr.lhs is ast.SelectorExpr {
			sel := value_expr.lhs as ast.SelectorExpr
			if sel.lhs is ast.Ident {
				lhs_name := (sel.lhs as ast.Ident).name
				fn_name := b.embedded_init_selector_call_name(lhs_name, sel.rhs.name)
				if fn_name != '' {
					args := if value_expr.expr is ast.EmptyExpr {
						[]ast.Expr{}
					} else {
						[value_expr.expr]
					}
					return b.embedded_init_build_resolved_call(fn_name, args)
				}
			}
		}
	}
	return b.build_expr(value_expr)
}

fn (mut b Builder) embedded_init_build_resolved_call_from_flat(fn_name string, c ast.Cursor) ValueID {
	ret_type := b.embedded_init_call_return_type_from_name(fn_name)
	if ret_type == 0 {
		return b.build_expr_from_flat(c)
	}
	n_edges := c.edge_count()
	n_args := if c.kind() == .expr_call_or_cast && n_edges > 1 && c.edge(1).kind() == .expr_empty {
		0
	} else {
		n_edges - 1
	}
	mut args := []ValueID{cap: n_args}
	for arg_i in 0 .. n_args {
		args << b.build_call_arg_from_flat(fn_name, arg_i, c.edge(1 + arg_i))
	}
	fn_ref := b.get_or_create_fn_ref(fn_name, ret_type)
	mut operands := []ValueID{cap: args.len + 1}
	operands << fn_ref
	operands << args
	return b.mod.add_instr(.call, b.cur_block, ret_type, operands)
}

fn (mut b Builder) embedded_init_build_source_expr_value_from_flat(value_c ast.Cursor) ValueID {
	if value_c.kind() in [.expr_call, .expr_call_or_cast] && value_c.edge_count() > 0 {
		lhs_c := value_c.edge(0)
		if lhs_c.kind() == .expr_selector {
			lhs_expr := lhs_c.edge(0)
			rhs_expr := lhs_c.edge(1)
			if lhs_expr.kind() == .expr_ident && rhs_expr.kind() == .expr_ident {
				fn_name := b.embedded_init_selector_call_name(lhs_expr.name(), rhs_expr.name())
				if fn_name != '' {
					return b.embedded_init_build_resolved_call_from_flat(fn_name, value_c)
				}
			}
		}
	}
	return b.build_expr_from_flat(value_c)
}

fn (mut b Builder) struct_init_plan_source_type(value_expr ast.Expr) TypeID {
	if value_expr is ast.CallExpr {
		if value_expr.lhs is ast.SelectorExpr {
			sel := value_expr.lhs as ast.SelectorExpr
			if sel.lhs is ast.Ident {
				lhs_name := (sel.lhs as ast.Ident).name
				fn_name := b.embedded_init_selector_call_name(lhs_name, sel.rhs.name)
				if fn_name != '' {
					ret_type := b.embedded_init_call_return_type_from_name(fn_name)
					if ret_type != 0 {
						return ret_type
					}
				}
			}
		}
		fn_name := b.resolve_call_name(value_expr)
		ret_type := b.embedded_init_call_return_type_from_name(fn_name)
		if ret_type != 0 {
			return ret_type
		}
	} else if value_expr is ast.CallOrCastExpr {
		if value_expr.lhs is ast.SelectorExpr {
			sel := value_expr.lhs as ast.SelectorExpr
			if sel.lhs is ast.Ident {
				lhs_name := (sel.lhs as ast.Ident).name
				fn_name := b.embedded_init_selector_call_name(lhs_name, sel.rhs.name)
				if fn_name != '' {
					ret_type := b.embedded_init_call_return_type_from_name(fn_name)
					if ret_type != 0 {
						return ret_type
					}
				}
			}
		}
		call_expr := ast.CallExpr{
			lhs:  value_expr.lhs
			args: if value_expr.expr is ast.EmptyExpr { []ast.Expr{} } else { [value_expr.expr] }
		}
		fn_name := b.resolve_call_name(call_expr)
		ret_type := b.embedded_init_call_return_type_from_name(fn_name)
		if ret_type != 0 {
			return ret_type
		}
	}
	return b.expr_type(value_expr)
}

fn (mut b Builder) struct_init_plan_source_type_from_flat(value_c ast.Cursor) TypeID {
	if value_c.kind() in [.expr_call, .expr_call_or_cast] && value_c.edge_count() > 0 {
		lhs_c := value_c.edge(0)
		if lhs_c.kind() == .expr_selector {
			lhs_expr := lhs_c.edge(0)
			rhs_expr := lhs_c.edge(1)
			if lhs_expr.kind() == .expr_ident && rhs_expr.kind() == .expr_ident {
				fn_name := b.embedded_init_selector_call_name(lhs_expr.name(), rhs_expr.name())
				if fn_name != '' {
					ret_type := b.embedded_init_call_return_type_from_name(fn_name)
					if ret_type != 0 {
						return ret_type
					}
				}
			}
		}
		fn_name := b.resolve_call_name_from_flat(lhs_c)
		ret_type := b.embedded_init_call_return_type_from_name(fn_name)
		if ret_type != 0 {
			return ret_type
		}
		if value_c.kind() == .expr_call_or_cast && lhs_c.kind() == .expr_ident {
			fn_name2 := b.resolve_call_name_ident_from_flat(lhs_c)
			ret_type2 := b.embedded_init_call_return_type_from_name(fn_name2)
			if ret_type2 != 0 {
				return ret_type2
			}
		}
	}
	return b.expr_type_from_flat(value_c)
}

fn (mut b Builder) plan_positional_flattened_init_expr_values(expr ast.InitExpr, struct_type TypeID, typ_info Type) ?[]StructInitFieldPlan {
	if expr.fields.len == 0 || typ_info.is_union || typ_info.fields.len == 0 {
		return none
	}
	mut plans := []StructInitFieldPlan{cap: expr.fields.len}
	mut target_idx := 0
	mut expanded_any := false
	for source_idx, field in expr.fields {
		if target_idx >= typ_info.fields.len {
			return none
		}
		source_type := b.struct_init_plan_source_type(field.value)
		if span := b.embedded_span_for_positional_init(struct_type, source_type, target_idx) {
			plans << StructInitFieldPlan{
				source_idx:  source_idx
				target_idx:  target_idx
				expand_len:  span.len
				source_type: source_type
			}
			target_idx += span.len
			expanded_any = true
			continue
		}
		plans << StructInitFieldPlan{
			source_idx:  source_idx
			target_idx:  target_idx
			expand_len:  0
			source_type: source_type
		}
		target_idx++
	}
	if !expanded_any {
		return none
	}
	return plans
}

fn (mut b Builder) plan_positional_flattened_init_expr_values_from_flat(c ast.Cursor, struct_type TypeID, typ_info Type, nfields int) ?[]StructInitFieldPlan {
	if nfields == 0 || typ_info.is_union || typ_info.fields.len == 0 {
		return none
	}
	mut plans := []StructInitFieldPlan{cap: nfields}
	mut target_idx := 0
	mut expanded_any := false
	for source_idx in 0 .. nfields {
		field_c := c.edge(1 + source_idx)
		field_name := field_c.name()
		if field_name.len > 0 {
			if expanded_any
				&& b.flat_embedded_default_field_name_is_redundant(struct_type, field_name) {
				continue
			}
			return none
		}
		if target_idx >= typ_info.fields.len {
			return none
		}
		value_c := field_c.edge(0)
		source_type := b.struct_init_plan_source_type_from_flat(value_c)
		if span := b.embedded_span_for_positional_init(struct_type, source_type, target_idx) {
			plans << StructInitFieldPlan{
				source_idx:  source_idx
				target_idx:  target_idx
				expand_len:  span.len
				source_type: source_type
			}
			target_idx += span.len
			expanded_any = true
			continue
		}
		plans << StructInitFieldPlan{
			source_idx:  source_idx
			target_idx:  target_idx
			expand_len:  0
			source_type: source_type
		}
		target_idx++
	}
	if !expanded_any {
		return none
	}
	return plans
}

fn (mut b Builder) collect_positional_flattened_init_expr_values(expr ast.InitExpr, struct_type TypeID, typ_info Type, plans []StructInitFieldPlan) ?[]ValueID {
	mut field_vals := []ValueID{cap: typ_info.fields.len}
	for plan in plans {
		field := expr.fields[plan.source_idx]
		if plan.expand_len > 0 {
			span := b.embedded_span_for_positional_init(struct_type, plan.source_type,
				plan.target_idx) or { return none }
			field_val := b.embedded_init_build_source_expr_value(field.value)
			if !b.append_embedded_struct_init_value(mut field_vals, field_val, span) {
				return none
			}
			continue
		}
		field_type := typ_info.fields[plan.target_idx]
		field_vals << b.build_struct_init_field_expr_value(field.value, field_type, false)
	}
	for field_vals.len < typ_info.fields.len {
		field_vals << b.zero_struct_init_field_value(typ_info.fields[field_vals.len])
	}
	return field_vals
}

fn (mut b Builder) collect_positional_flattened_init_expr_values_from_flat(c ast.Cursor, struct_type TypeID, typ_info Type, plans []StructInitFieldPlan) ?[]ValueID {
	mut field_vals := []ValueID{cap: typ_info.fields.len}
	for plan in plans {
		field_c := c.edge(1 + plan.source_idx)
		value_c := field_c.edge(0)
		if plan.expand_len > 0 {
			span := b.embedded_span_for_positional_init(struct_type, plan.source_type,
				plan.target_idx) or { return none }
			field_val := b.embedded_init_build_source_expr_value_from_flat(value_c)
			if !b.append_embedded_struct_init_value(mut field_vals, field_val, span) {
				return none
			}
			continue
		}
		field_type := typ_info.fields[plan.target_idx]
		field_vals << b.build_struct_init_field_expr_value_from_flat(value_c, field_type, false)
	}
	for field_vals.len < typ_info.fields.len {
		field_vals << b.zero_struct_init_field_value(typ_info.fields[field_vals.len])
	}
	return field_vals
}

fn (mut b Builder) collect_init_expr_values(expr ast.InitExpr) (TypeID, []ValueID) {
	// Resolve the struct type
	mut struct_type := b.ast_type_to_ssa(expr.typ)
	if struct_type == b.mod.type_store.get_int(64) {
		env_type := b.expr_type(ast.Expr(expr))
		if env_type != b.mod.type_store.get_int(64) {
			struct_type = env_type
		}
	}

	// Get the type info for field name lookup
	typ_info := b.mod.type_store.types[struct_type]
	num_fields := typ_info.field_names.len

	if num_fields == 0 {
		return struct_type, []ValueID{}
	}

	// Build field values in declaration order
	mut field_vals := []ValueID{cap: num_fields}
	mut initialized_fields := map[string]int{} // field name -> index in expr.fields

	// Check if this is positional initialization (all field names empty)
	mut is_positional := expr.fields.len > 0
	for field in expr.fields {
		if field.name.len > 0 {
			is_positional = false
			break
		}
	}

	if is_positional {
		if flattened_plan := b.plan_positional_flattened_init_expr_values(expr, struct_type,
			typ_info)
		{
			if flattened_values := b.collect_positional_flattened_init_expr_values(expr,
				struct_type, typ_info, flattened_plan)
			{
				return struct_type, flattened_values
			}
			panic('SSA builder: failed to expand embedded positional struct init')
		}
	}

	// Map explicit field inits by name
	// Handle sumtype _data._variant fields by mapping to _data
	if is_positional {
		// Positional init: match by index using struct field names
		for fi, field in expr.fields {
			if fi < num_fields {
				initialized_fields[typ_info.field_names[fi]] = fi
			}
			_ = field
		}
	} else {
		for fi, field in expr.fields {
			fname := if field.name.starts_with('_data.') {
				'_data'
			} else {
				field.name
			}
			initialized_fields[fname] = fi
		}
	}

	mut field_limit := num_fields
	if typ_info.is_union {
		field_limit = 0
		for fi, fname in typ_info.field_names {
			if _ := initialized_fields[fname] {
				field_limit = fi + 1
			}
		}
	}

	for fi in 0 .. field_limit {
		fname := typ_info.field_names[fi]
		field_type := if fi < typ_info.fields.len {
			typ_info.fields[fi]
		} else {
			b.mod.type_store.get_int(64)
		}

		if idx := initialized_fields[fname] {
			if idx >= 0 && idx < expr.fields.len {
				// Set flag when building _data field of sum type init
				// so build_prefix heap-allocates &struct_local values
				is_sumtype_data := fname == '_data' && typ_info.field_names.len == 2
					&& typ_info.field_names[0] == '_tag'
				if is_sumtype_data {
					b.in_sumtype_data = true
				}
				mut field_val := ValueID(0)
				if b.ssa_type_is_sumtype(field_type) {
					addr := b.build_addr(expr.fields[idx].value)
					if wrapped := b.wrap_address_for_sumtype_target(addr, field_type) {
						field_val = wrapped
					}
				}
				if field_val == 0 {
					field_val = b.build_expr(expr.fields[idx].value)
				}
				if is_sumtype_data {
					b.in_sumtype_data = false
				}
				if b.is_wrapper_type(field_type) {
					field_val = b.coerce_wrapper_value(expr.fields[idx].value, field_val,
						field_type)
				}
				// When a struct field init calls a function that returns a pointer
				// (e.g., TypeStore.new() returns &TypeStore), but the field expects
				// a value type, we need to load (dereference) the pointer.
				val_typ_id := b.mod.values[field_val].typ
				if val_typ_id > 0 && val_typ_id < b.mod.type_store.types.len && field_type > 0
					&& field_type < b.mod.type_store.types.len {
					val_typ := b.mod.type_store.types[val_typ_id]
					fld_typ := b.mod.type_store.types[field_type]
					if val_typ.kind == .ptr_t && fld_typ.kind == .struct_t {
						field_val = b.mod.add_instr(.load, b.cur_block, field_type, [
							field_val,
						])
					}
				}
				field_val = b.coerce_struct_sumtype_field_value(field_val, field_type)
				field_vals << field_val
			} else {
				field_vals << b.mod.get_or_add_const(field_type, '0')
			}
		} else {
			// Zero-initialize unset fields
			field_vals << b.zero_struct_init_field_value(field_type)
		}
	}

	return struct_type, field_vals
}

// collect_init_expr_values_from_flat is the cursor-native counterpart of
// `collect_init_expr_values`. InitExpr flat encoding: edge(0)=typ,
// edge(1..)=aux_field_init cursors (name via `name()`, value at edge(0)).
fn (mut b Builder) collect_init_expr_values_from_flat(c ast.Cursor) (TypeID, []ValueID) {
	typ_c := c.edge(0)
	mut struct_type := b.ast_type_to_ssa_from_flat(typ_c)
	if struct_type == b.mod.type_store.get_int(64) {
		env_type := b.expr_type_from_flat(c)
		if env_type != b.mod.type_store.get_int(64) {
			struct_type = env_type
		}
	}

	typ_info := b.mod.type_store.types[struct_type]
	num_fields := typ_info.field_names.len

	if num_fields == 0 {
		return struct_type, []ValueID{}
	}
	nfields := c.edge_count() - 1

	mut field_vals := []ValueID{cap: num_fields}
	mut initialized_fields := map[string]int{}

	mut is_positional := nfields > 0
	for i in 0 .. nfields {
		field_c := c.edge(1 + i)
		if field_c.name().len > 0 {
			is_positional = false
			break
		}
	}

	if is_positional
		|| b.flat_init_has_positional_fields_with_embedded_defaults(c, struct_type, nfields) {
		if flattened_plan := b.plan_positional_flattened_init_expr_values_from_flat(c, struct_type,
			typ_info, nfields)
		{
			if flattened_values := b.collect_positional_flattened_init_expr_values_from_flat(c,
				struct_type, typ_info, flattened_plan)
			{
				return struct_type, flattened_values
			}
			panic('SSA builder: failed to expand embedded positional struct init')
		}
	}

	if is_positional {
		for fi in 0 .. nfields {
			if fi < num_fields {
				initialized_fields[typ_info.field_names[fi]] = fi
			}
		}
	} else {
		for fi in 0 .. nfields {
			field_c := c.edge(1 + fi)
			raw_name := field_c.name()
			fname := if raw_name.starts_with('_data.') {
				'_data'
			} else {
				raw_name
			}
			initialized_fields[fname] = fi
		}
	}

	mut field_limit := num_fields
	if typ_info.is_union {
		field_limit = 0
		for fi, fname in typ_info.field_names {
			if _ := initialized_fields[fname] {
				field_limit = fi + 1
			}
		}
	}

	for fi in 0 .. field_limit {
		fname := typ_info.field_names[fi]
		field_type := if fi < typ_info.fields.len {
			typ_info.fields[fi]
		} else {
			b.mod.type_store.get_int(64)
		}

		if idx := initialized_fields[fname] {
			if idx >= 0 && idx < nfields {
				is_sumtype_data := fname == '_data' && typ_info.field_names.len == 2
					&& typ_info.field_names[0] == '_tag'
				if is_sumtype_data {
					b.in_sumtype_data = true
				}
				field_c := c.edge(1 + idx)
				value_c := field_c.edge(0)
				mut field_val := ValueID(0)
				if b.ssa_type_is_sumtype(field_type) {
					addr := b.build_addr_from_flat(value_c)
					if wrapped := b.wrap_address_for_sumtype_target(addr, field_type) {
						field_val = wrapped
					}
				}
				if field_val == 0 {
					field_val = b.build_expr_from_flat(value_c)
				}
				if is_sumtype_data {
					b.in_sumtype_data = false
				}
				if b.is_wrapper_type(field_type) {
					field_val = b.coerce_wrapper_value_from_flat(value_c, field_val, field_type)
				}
				val_typ_id := b.mod.values[field_val].typ
				if val_typ_id > 0 && val_typ_id < b.mod.type_store.types.len && field_type > 0
					&& field_type < b.mod.type_store.types.len {
					val_typ := b.mod.type_store.types[val_typ_id]
					fld_typ := b.mod.type_store.types[field_type]
					if val_typ.kind == .ptr_t && fld_typ.kind == .struct_t {
						field_val = b.mod.add_instr(.load, b.cur_block, field_type, [
							field_val,
						])
					}
				}
				field_val = b.coerce_struct_sumtype_field_value(field_val, field_type)
				field_vals << field_val
			} else {
				field_vals << b.mod.get_or_add_const(field_type, '0')
			}
		} else {
			field_vals << b.zero_struct_init_field_value(field_type)
		}
	}

	return struct_type, field_vals
}

fn (mut b Builder) coerce_struct_sumtype_field_value(field_val ValueID, field_type TypeID) ValueID {
	if field_val <= 0 || field_val >= b.mod.values.len || !b.ssa_type_is_sumtype(field_type) {
		return field_val
	}
	if b.mod.values[field_val].typ == field_type {
		return field_val
	}
	if wrapped := b.wrap_value_for_sumtype_target(field_val, field_type) {
		return wrapped
	}
	return field_val
}

// build_init_expr_ptr_from_flat is the cursor-native counterpart of
// `build_init_expr_ptr` for the `&Struct{...}` heap-allocation path used by
// `build_prefix_from_flat` when `op == .amp && inner_kind == .expr_init`.
fn (mut b Builder) build_init_expr_ptr_from_flat(c ast.Cursor) ValueID {
	struct_type, field_vals := b.collect_init_expr_values_from_flat(c)
	if field_vals.len == 0 {
		// s213 follow-up: was routing through build_expr_from_flat (one extra
		// dispatch hop); now calls build_init_from_flat directly since it's
		// cursor-native as of s213.
		struct_val := b.build_init_from_flat(c)
		val_type := b.mod.values[struct_val].typ
		ptr_type := b.mod.type_store.get_ptr(val_type)
		heap_ptr := b.mod.add_instr(.heap_alloc, b.cur_block, ptr_type, []ValueID{})
		b.mod.add_instr(.store, b.cur_block, 0, [struct_val, heap_ptr])
		return heap_ptr
	}
	ptr_type := b.mod.type_store.get_ptr(struct_type)
	heap_ptr := b.mod.add_instr(.heap_alloc, b.cur_block, ptr_type, []ValueID{})
	int32_t := b.mod.type_store.get_int(32)
	typ_info := b.mod.type_store.types[struct_type]
	for fi, field_val in field_vals {
		if fi >= typ_info.fields.len {
			continue
		}
		field_type := typ_info.fields[fi]
		field_ptr_type := b.mod.type_store.get_ptr(field_type)
		idx_val := b.mod.get_or_add_const(int32_t, fi.str())
		field_ptr := b.mod.add_instr(.get_element_ptr, b.cur_block, field_ptr_type, [
			heap_ptr,
			idx_val,
		])
		b.mod.add_instr(.store, b.cur_block, 0, [field_val, field_ptr])
	}
	return heap_ptr
}

fn (mut b Builder) build_init_expr(expr ast.InitExpr) ValueID {
	// Struct initialization: Type{ field: value, ... }
	// Uses struct_init opcode: keeps aggregates as SSA values for better optimization.
	// Lowered to alloca/GEP/store only when address is taken (build_init_expr_ptr).
	struct_type, field_vals := b.collect_init_expr_values(expr)
	if field_vals.len == 0 {
		// Not a known struct or has no fields: fall back to zero constant
		return b.mod.get_or_add_const(struct_type, '0')
	}
	return b.mod.add_instr(.struct_init, b.cur_block, struct_type, field_vals)
}

// build_init_expr_ptr: like build_init_expr but returns the pointer (for &Point{...}).
// This is the "heap allocation" case — allocates on heap via malloc.
fn (mut b Builder) build_init_expr_ptr(expr ast.InitExpr) ValueID {
	struct_type, field_vals := b.collect_init_expr_values(expr)
	if field_vals.len == 0 {
		struct_val := b.build_init_expr(expr)
		val_type := b.mod.values[struct_val].typ
		ptr_type := b.mod.type_store.get_ptr(val_type)
		heap_ptr := b.mod.add_instr(.heap_alloc, b.cur_block, ptr_type, []ValueID{})
		b.mod.add_instr(.store, b.cur_block, 0, [struct_val, heap_ptr])
		return heap_ptr
	}
	ptr_type := b.mod.type_store.get_ptr(struct_type)
	// Avoid aggregate heap stores for &Struct{...}; they are still unreliable
	// in the native arm64 path for larger compiler structs.
	heap_ptr := b.mod.add_instr(.heap_alloc, b.cur_block, ptr_type, []ValueID{})
	int32_t := b.mod.type_store.get_int(32)
	typ_info := b.mod.type_store.types[struct_type]
	for fi, field_val in field_vals {
		if fi >= typ_info.fields.len {
			continue
		}
		field_type := typ_info.fields[fi]
		field_ptr_type := b.mod.type_store.get_ptr(field_type)
		idx_val := b.mod.get_or_add_const(int32_t, fi.str())
		field_ptr := b.mod.add_instr(.get_element_ptr, b.cur_block, field_ptr_type, [
			heap_ptr,
			idx_val,
		])
		b.mod.add_instr(.store, b.cur_block, 0, [field_val, field_ptr])
	}
	return heap_ptr
}

fn (mut b Builder) heap_copy_from_address(addr ValueID) ?ValueID {
	if addr <= 0 || addr >= b.mod.values.len {
		return none
	}
	addr_type_id := b.mod.values[addr].typ
	if addr_type_id <= 0 || int(addr_type_id) >= b.mod.type_store.types.len {
		return none
	}
	addr_type := b.mod.type_store.types[addr_type_id]
	if addr_type.kind != .ptr_t || addr_type.elem_type <= 0
		|| int(addr_type.elem_type) >= b.mod.type_store.types.len {
		return none
	}
	elem_type := addr_type.elem_type
	elem_info := b.mod.type_store.types[elem_type]
	heap_ptr := b.mod.add_instr(.heap_alloc, b.cur_block, addr_type_id, []ValueID{})
	if elem_info.kind == .struct_t && elem_info.fields.len > 0 {
		int32_t := b.mod.type_store.get_int(32)
		for fi, field_type in elem_info.fields {
			field_ptr_type := b.mod.type_store.get_ptr(field_type)
			idx_val := b.mod.get_or_add_const(int32_t, fi.str())
			src_fptr := b.mod.add_instr(.get_element_ptr, b.cur_block, field_ptr_type, [
				addr,
				idx_val,
			])
			fval := b.mod.add_instr(.load, b.cur_block, field_type, [src_fptr])
			dst_fptr := b.mod.add_instr(.get_element_ptr, b.cur_block, field_ptr_type, [
				heap_ptr,
				idx_val,
			])
			b.mod.add_instr(.store, b.cur_block, 0, [fval, dst_fptr])
		}
	} else {
		loaded := b.mod.add_instr(.load, b.cur_block, elem_type, [addr])
		b.mod.add_instr(.store, b.cur_block, 0, [loaded, heap_ptr])
	}
	return heap_ptr
}

fn (mut b Builder) heap_copy_value(val ValueID) ?ValueID {
	if val <= 0 || val >= b.mod.values.len {
		return none
	}
	val_type := b.mod.values[val].typ
	if val_type <= 0 || int(val_type) >= b.mod.type_store.types.len {
		return none
	}
	ptr_type := b.mod.type_store.get_ptr(val_type)
	heap_ptr := b.mod.add_instr(.heap_alloc, b.cur_block, ptr_type, []ValueID{})
	typ_info := b.mod.type_store.types[val_type]
	if typ_info.kind == .struct_t && typ_info.fields.len > 0 {
		int32_t := b.mod.type_store.get_int(32)
		for fi, field_type in typ_info.fields {
			field_ptr_type := b.mod.type_store.get_ptr(field_type)
			idx_val := b.mod.get_or_add_const(int32_t, fi.str())
			field_val := b.mod.add_instr(.extractvalue, b.cur_block, field_type, [val, idx_val])
			field_ptr := b.mod.add_instr(.get_element_ptr, b.cur_block, field_ptr_type, [
				heap_ptr,
				idx_val,
			])
			b.mod.add_instr(.store, b.cur_block, 0, [field_val, field_ptr])
		}
	} else {
		b.mod.add_instr(.store, b.cur_block, 0, [val, heap_ptr])
	}
	return heap_ptr
}

fn ssa_type_name_matches(actual string, expected string) bool {
	if actual == expected {
		return true
	}
	actual_short := if actual.contains('__') { actual.all_after_last('__') } else { actual }
	expected_short := if expected.contains('__') { expected.all_after_last('__') } else { expected }
	return actual_short == expected_short
}

fn ssa_generic_base_name(name string) string {
	return if name.contains('_T_') { name.all_before('_T_') } else { name }
}

fn ssa_sumtype_variant_name_matches(actual string, expected string) bool {
	if ssa_type_name_matches(actual, expected) {
		return true
	}
	actual_base := ssa_generic_base_name(actual)
	expected_base := ssa_generic_base_name(expected)
	return ssa_type_name_matches(actual_base, expected)
		|| ssa_type_name_matches(actual, expected_base)
		|| ssa_type_name_matches(actual_base, expected_base)
}

fn (mut b Builder) sumtype_variants_for_type(type_id TypeID) []string {
	if b.env == unsafe { nil } {
		return []string{}
	}
	sumtype_name := b.mod.c_struct_names[type_id] or { return []string{} }
	sumtype_info := b.checked_sumtype_for_ssa_name(sumtype_name) or { return []string{} }
	mut variants := []string{cap: sumtype_info.get_variants().len}
	for variant in sumtype_info.get_variants() {
		variants << variant.name()
	}
	return variants
}

fn (mut b Builder) sumtype_variant_tag(sumtype_type TypeID, variant_type TypeID) ?int {
	variant_name := b.mod.c_struct_names[variant_type] or { return none }
	variants := b.sumtype_variants_for_type(sumtype_type)
	for i, variant in variants {
		if ssa_sumtype_variant_name_matches(variant_name, variant) {
			return i
		}
	}
	return none
}

fn (mut b Builder) wrap_value_for_sumtype_target(val ValueID, target_type TypeID) ?ValueID {
	if val <= 0 || val >= b.mod.values.len || target_type <= 0
		|| int(target_type) >= b.mod.type_store.types.len {
		return none
	}
	target_info := b.mod.type_store.types[target_type]
	if target_info.kind != .struct_t || target_info.field_names.len < 2
		|| target_info.field_names[0] != '_tag' || target_info.field_names[1] != '_data' {
		return none
	}
	val_type := b.mod.values[val].typ
	if val_type == target_type || val_type <= 0 || int(val_type) >= b.mod.type_store.types.len {
		return none
	}
	tag := b.sumtype_variant_tag(target_type, val_type) or { return none }
	i64_t := b.mod.type_store.get_int(64)
	tag_val := b.mod.get_or_add_const(i64_t, tag.str())
	val_info := b.mod.type_store.types[val_type]
	data_val := if val_info.kind in [.struct_t, .array_t] {
		heap_ptr := b.heap_copy_value(val) or { return none }
		b.cast_value_to_type(heap_ptr, i64_t)
	} else {
		b.cast_value_to_type(val, i64_t)
	}
	return b.mod.add_instr(.struct_init, b.cur_block, target_type, [tag_val, data_val])
}

fn (mut b Builder) wrap_address_for_sumtype_target(addr ValueID, target_type TypeID) ?ValueID {
	if addr <= 0 || addr >= b.mod.values.len || target_type <= 0
		|| int(target_type) >= b.mod.type_store.types.len {
		return none
	}
	target_info := b.mod.type_store.types[target_type]
	if target_info.kind != .struct_t || target_info.field_names.len < 2
		|| target_info.field_names[0] != '_tag' || target_info.field_names[1] != '_data' {
		return none
	}
	addr_type_id := b.mod.values[addr].typ
	if addr_type_id <= 0 || int(addr_type_id) >= b.mod.type_store.types.len {
		return none
	}
	addr_type := b.mod.type_store.types[addr_type_id]
	if addr_type.kind != .ptr_t || addr_type.elem_type <= 0
		|| int(addr_type.elem_type) >= b.mod.type_store.types.len {
		return none
	}
	val_type := addr_type.elem_type
	val_info := b.mod.type_store.types[val_type]
	if val_info.kind !in [.struct_t, .array_t] {
		return none
	}
	tag := b.sumtype_variant_tag(target_type, val_type) or { return none }
	heap_ptr := b.heap_copy_from_address(addr) or { return none }
	i64_t := b.mod.type_store.get_int(64)
	tag_val := b.mod.get_or_add_const(i64_t, tag.str())
	data_val := b.cast_value_to_type(heap_ptr, i64_t)
	return b.mod.add_instr(.struct_init, b.cur_block, target_type, [tag_val, data_val])
}

fn (mut b Builder) build_cast_expr_to_type(type_expr ast.Expr, value_expr ast.Expr) ValueID {
	target_type := b.ast_type_to_ssa(type_expr)
	return b.build_cast_expr_to_type_id(value_expr, target_type)
}

fn (b &Builder) is_mut_ptr_param_expr(expr ast.Expr) bool {
	match expr {
		ast.Ident {
			return expr.name in b.mut_ptr_params
		}
		ast.ParenExpr, ast.ModifierExpr {
			return b.is_mut_ptr_param_expr(expr.expr)
		}
		else {
			return false
		}
	}
}

fn (b &Builder) is_mut_ptr_param_cursor(c ast.Cursor) bool {
	match c.kind() {
		.expr_ident {
			return c.name() in b.mut_ptr_params
		}
		.expr_paren, .expr_modifier {
			if c.edge_count() == 0 {
				return false
			}
			return b.is_mut_ptr_param_cursor(c.edge(0))
		}
		else {
			return false
		}
	}
}

fn (mut b Builder) cast_forwarded_mut_param_addr_to_pointer(addr ValueID, target_type TypeID) ?ValueID {
	if !b.valid_value_id(addr) || !b.valid_type_id(target_type) {
		return none
	}
	target_typ := b.mod.type_store.types[target_type]
	if target_typ.kind != .ptr_t {
		return none
	}
	addr_type := b.mod.values[addr].typ
	if !b.valid_type_id(addr_type) {
		return none
	}
	addr_typ := b.mod.type_store.types[addr_type]
	if addr_typ.kind != .ptr_t && addr_typ.kind != .int_t {
		return none
	}
	return b.build_cast_value_to_type(addr, target_type)
}

fn (mut b Builder) build_cast_expr_to_type_id(value_expr ast.Expr, target_type TypeID) ValueID {
	addr := b.build_addr(value_expr)
	if addr != 0 {
		if wrapped := b.wrap_address_for_sumtype_target(addr, target_type) {
			return wrapped
		}
		if b.is_mut_ptr_param_expr(value_expr) {
			if ptr_cast := b.cast_forwarded_mut_param_addr_to_pointer(addr, target_type) {
				return ptr_cast
			}
		}
	}
	val := b.build_expr(value_expr)
	return b.build_cast_value_to_type(val, target_type)
}

fn (mut b Builder) build_cast_value_to_type(val ValueID, target_type TypeID) ValueID {
	if target_type == 0 || val <= 0 || val >= b.mod.values.len {
		return val
	}
	if b.mod.values[val].typ == target_type {
		return val
	}
	if wrapped := b.wrap_value_for_sumtype_target(val, target_type) {
		return wrapped
	}
	return b.cast_value_to_type(val, target_type)
}

fn (mut b Builder) build_cast(expr ast.CastExpr) ValueID {
	if b.type_expr_is_ierror_interface(expr.typ) {
		if tag := b.ierror_tag_for_expr(expr.expr) {
			return tag
		}
	}
	target_type := b.ast_type_to_ssa(expr.typ)
	addr := b.build_addr(expr.expr)
	if addr != 0 {
		if wrapped := b.wrap_address_for_sumtype_target(addr, target_type) {
			return wrapped
		}
		if b.is_mut_ptr_param_expr(expr.expr) {
			if ptr_cast := b.cast_forwarded_mut_param_addr_to_pointer(addr, target_type) {
				return ptr_cast
			}
		}
	}
	val := b.build_expr(expr.expr)
	return b.build_cast_value_to_type(val, target_type)
}

fn (mut b Builder) build_call_or_cast(expr ast.CallOrCastExpr) ValueID {
	// CallOrCastExpr is produced by the parser when it can't distinguish
	// between a function call and a type cast, e.g., int(x) or voidptr(p).
	// After type checking, the transformer usually resolves these, but some
	// may remain as casts. Treat as cast: convert expr to target type.
	if expr.lhs is ast.Ident {
		name := expr.lhs.name
		fn_name := b.resolve_call_name_for_ident_name(name)
		if fn_name in b.fn_index && !is_builtin_cast_type_name(name) && !b.known_type_name(name) {
			args := if expr.expr is ast.EmptyExpr { []ast.Expr{} } else { [expr.expr] }
			return b.build_call_resolved(fn_name, ast.CallExpr{
				lhs:  expr.lhs
				args: args
				pos:  expr.pos
			})
		}
	}
	if expr.lhs is ast.SelectorExpr {
		sel := expr.lhs as ast.SelectorExpr
		args := if expr.expr is ast.EmptyExpr { []ast.Expr{} } else { [expr.expr] }
		call_expr := ast.CallExpr{
			lhs:  expr.lhs
			args: args
			pos:  expr.pos
		}
		fn_name := b.resolve_call_name(call_expr)
		if mod_name := b.selector_module_name(sel) {
			if (mod_name == 'C' || fn_name in b.fn_index)
				&& !b.call_or_cast_selector_should_remain_cast(sel, fn_name) {
				return b.build_call_resolved(fn_name, call_expr)
			}
		}
		if fn_name in b.fn_index && !b.call_or_cast_selector_should_remain_cast(sel, fn_name) {
			return b.build_call_resolved(fn_name, call_expr)
		}
	}
	target_type := b.ast_type_to_ssa(expr.lhs)
	if b.type_expr_is_ierror_interface(expr.lhs) {
		if tag := b.ierror_tag_for_expr(expr.expr) {
			return tag
		}
	}
	addr := b.build_addr(expr.expr)
	if addr != 0 {
		if wrapped := b.wrap_address_for_sumtype_target(addr, target_type) {
			return wrapped
		}
		if b.is_mut_ptr_param_expr(expr.expr) {
			if ptr_cast := b.cast_forwarded_mut_param_addr_to_pointer(addr, target_type) {
				return ptr_cast
			}
		}
	}
	val := b.build_expr(expr.expr)
	return b.build_cast_value_to_type(val, target_type)
}

fn (b &Builder) get_checked_expr_type(expr ast.Expr) ?types.Type {
	if b.env != unsafe { nil } && builder_expr_ok(expr) {
		pos := expr.pos()
		if pos.id != 0 {
			if typ := b.env.get_expr_type(pos.id) {
				return typ
			}
		}
	}
	return none
}

fn (b &Builder) unwrap_alias_type(t types.Type) types.Type {
	match t {
		types.Alias {
			base := b.resolve_alias_base_type(t) or { return t }
			return b.unwrap_alias_type(base)
		}
		else {
			name := types.type_name(t)
			if name != '' {
				if live_type := b.lookup_checked_type_by_name(name) {
					if live_type is types.Alias {
						return b.unwrap_alias_type(live_type)
					}
					if types.type_name(live_type) != name {
						return b.unwrap_alias_type(live_type)
					}
				}
			}
			return t
		}
	}
}

fn (b &Builder) resolve_alias_base_type(alias types.Alias) ?types.Type {
	if ssa_string_ok(alias.name) && alias.name != '' {
		if live_type := b.lookup_checked_type_by_name(alias.name) {
			if live_type is types.Alias {
				if types.type_has_valid_payload(live_type.base_type) {
					return live_type.base_type
				}
			} else {
				return live_type
			}
		}
	}
	if types.type_has_valid_payload(alias.base_type) {
		return alias.base_type
	}
	return none
}

fn (b &Builder) lookup_checked_type_in_module(module_name string, type_name string) ?types.Type {
	if module_name == '' || type_name == '' || b.env == unsafe { nil } {
		return none
	}
	if scope := b.env.get_scope(module_name) {
		if typ := scope.lookup_type_parent(type_name, 0) {
			return typ
		}
		if obj := scope.lookup_parent(type_name, 0) {
			match obj {
				types.Type {
					return obj
				}
				types.TypeObject {
					return obj.typ
				}
				else {}
			}
		}
	}
	return none
}

fn (b &Builder) checked_module_names_for_ssa_module(module_name string) []string {
	mut candidates := []string{}
	if module_name == '' || b.env == unsafe { nil } {
		return candidates
	}
	for import_alias, imported_module in b.module_import_aliases {
		alias_matches := module_name_to_ssa_name(import_alias) == module_name
		imported_matches := module_name_to_ssa_name(imported_module) == module_name
		if alias_matches || imported_matches {
			if imported_module !in candidates {
				candidates << imported_module
			}
		}
	}
	if candidates.len > 0 {
		return candidates
	}
	if module_name_to_ssa_name(b.cur_module) == module_name {
		candidates << b.cur_module
	}
	return candidates
}

fn (b &Builder) lookup_checked_type_by_name(name string) ?types.Type {
	if name == '' || !ssa_string_ok(name) || b.env == unsafe { nil } {
		return none
	}
	if typ := b.lookup_checked_type_in_module(b.cur_module, name) {
		return typ
	}
	if name.contains('.') {
		dot_idx := name.last_index('.') or { -1 }
		mod_name := name[..dot_idx]
		type_name := name[dot_idx + 1..]
		if typ := b.lookup_checked_type_in_module(mod_name, type_name) {
			return typ
		}
	}
	if name.contains('__') {
		dunder_idx := name.last_index('__') or { -1 }
		type_name := name[dunder_idx + 2..]
		ssa_mod_name := name[..dunder_idx]
		checked_module_names := b.checked_module_names_for_ssa_module(ssa_mod_name)
		if checked_module_names.len > 1 {
			return none
		}
		if checked_module_names.len == 1 {
			checked_module_name := checked_module_names[0]
			if typ := b.lookup_checked_type_in_module(checked_module_name, type_name) {
				return typ
			}
			if checked_module_name.contains('.') {
				leaf_module_name := checked_module_name.all_after_last('.')
				if typ := b.lookup_checked_type_in_module(leaf_module_name, type_name) {
					return typ
				}
			}
		}
		short_name := name.all_after_last('__')
		if typ := b.lookup_checked_type_in_module(b.cur_module, short_name) {
			return typ
		}
	}
	for try_mod in ['main', 'builtin'] {
		if try_mod == b.cur_module {
			continue
		}
		if typ := b.lookup_checked_type_in_module(try_mod, name) {
			return typ
		}
	}
	return none
}

fn (mut b Builder) call_lhs_type_to_ssa(lhs ast.Expr) ?TypeID {
	name := match lhs {
		ast.Ident {
			lhs.name
		}
		ast.SelectorExpr {
			lhs.name().replace('.', '__')
		}
		else {
			''
		}
	}

	if name == '' {
		return none
	}
	if typ := b.lookup_checked_type_by_name(name) {
		type_id := b.type_to_ssa(typ)
		if type_id > 0 {
			return type_id
		}
	}
	if b.cur_module != '' && !name.contains('__') {
		if typ := b.lookup_checked_type_by_name('${b.cur_module}__${name}') {
			type_id := b.type_to_ssa(typ)
			if type_id > 0 {
				return type_id
			}
		}
	}
	return none
}

fn (mut b Builder) cast_value_to_type(val ValueID, target_type TypeID) ValueID {
	if target_type <= 0 || val <= 0 || val >= b.mod.values.len
		|| int(target_type) >= b.mod.type_store.types.len {
		return val
	}
	src_type := b.mod.values[val].typ
	if src_type <= 0 || int(src_type) >= b.mod.type_store.types.len {
		return val
	}
	if src_type == target_type || src_type == 0 {
		return val
	}
	src := b.mod.type_store.types[src_type]
	dst := b.mod.type_store.types[target_type]

	if src.kind == .int_t && dst.kind == .int_t {
		if src.width < dst.width {
			if src.is_unsigned {
				return b.mod.add_instr(.zext, b.cur_block, target_type, [val])
			}
			return b.mod.add_instr(.sext, b.cur_block, target_type, [val])
		} else if src.width > dst.width {
			return b.mod.add_instr(.trunc, b.cur_block, target_type, [val])
		}
		if src.is_unsigned != dst.is_unsigned {
			return b.mod.add_instr(.bitcast, b.cur_block, target_type, [val])
		}
		return val
	} else if src.kind == .int_t && dst.kind == .float_t {
		if src.is_unsigned {
			return b.mod.add_instr(.uitofp, b.cur_block, target_type, [val])
		}
		return b.mod.add_instr(.sitofp, b.cur_block, target_type, [val])
	} else if src.kind == .float_t && dst.kind == .int_t {
		if dst.is_unsigned {
			return b.mod.add_instr(.fptoui, b.cur_block, target_type, [val])
		}
		return b.mod.add_instr(.fptosi, b.cur_block, target_type, [val])
	} else if src.kind == .float_t && dst.kind == .float_t {
		if src.width > dst.width {
			return b.mod.add_instr(.trunc, b.cur_block, target_type, [val])
		}
		if b.mod.values[val].kind == .constant {
			return b.mod.get_or_add_const(target_type, b.mod.values[val].name)
		}
		return b.mod.add_instr(.zext, b.cur_block, target_type, [val])
	} else if src.kind == .ptr_t && dst.kind == .ptr_t {
		return b.mod.add_instr(.bitcast, b.cur_block, target_type, [val])
	} else if src.kind == .int_t && dst.kind == .ptr_t {
		return b.mod.add_instr(.bitcast, b.cur_block, target_type, [val])
	} else if src.kind == .ptr_t && dst.kind == .int_t {
		return b.mod.add_instr(.bitcast, b.cur_block, target_type, [val])
	}

	return b.mod.add_instr(.bitcast, b.cur_block, target_type, [val])
}

fn (mut b Builder) build_sumtype_as_cast(sum_val ValueID, target_type TypeID) ValueID {
	if sum_val <= 0 || sum_val >= b.mod.values.len || target_type == 0 {
		return sum_val
	}
	if b.mod.values[sum_val].typ == target_type {
		return sum_val
	}
	i64_t := b.mod.type_store.get_int(64)
	data_idx := b.mod.get_or_add_const(b.mod.type_store.get_int(32), '1')
	data_word := b.mod.add_instr(.extractvalue, b.cur_block, i64_t, [sum_val, data_idx])
	target_info := b.mod.type_store.types[target_type]
	if target_info.kind in [.struct_t, .array_t] {
		target_ptr_type := b.mod.type_store.get_ptr(target_type)
		data_ptr := b.cast_value_to_type(data_word, target_ptr_type)
		return b.mod.add_instr(.load, b.cur_block, target_type, [data_ptr])
	}
	return b.cast_value_to_type(data_word, target_type)
}

fn (mut b Builder) ssa_type_is_sumtype(type_id TypeID) bool {
	if type_id <= 0 || int(type_id) >= b.mod.type_store.types.len {
		return false
	}
	info := b.mod.type_store.types[type_id]
	return info.kind == .struct_t && info.field_names.len >= 2 && info.field_names[0] == '_tag'
		&& info.field_names[1] == '_data'
}

fn (mut b Builder) build_as_cast(expr ast.AsCastExpr) ValueID {
	val := b.build_expr(expr.expr)
	target_type := b.ast_type_to_ssa(expr.typ)
	if target_type == 0 || b.mod.values[val].typ == target_type {
		return val
	}
	if b.ssa_type_is_sumtype(b.mod.values[val].typ) {
		return b.build_sumtype_as_cast(val, target_type)
	}
	src_checked_type := b.get_checked_expr_type(expr.expr) or { return val }
	match b.unwrap_alias_type(src_checked_type) {
		types.SumType {
			return b.build_sumtype_as_cast(val, target_type)
		}
		else {
			return val
		}
	}
}

fn (mut b Builder) build_keyword(kw ast.Keyword) ValueID {
	match kw.tok {
		.key_true {
			return b.mod.get_or_add_const(b.mod.type_store.get_int(1), '1')
		}
		.key_false {
			return b.mod.get_or_add_const(b.mod.type_store.get_int(1), '0')
		}
		.key_nil {
			i8_t := b.mod.type_store.get_int(8)
			ptr_t := b.mod.type_store.get_ptr(i8_t)
			return b.mod.get_or_add_const(ptr_t, '0')
		}
		else {
			return b.mod.get_or_add_const(b.mod.type_store.get_int(32), '0')
		}
	}
}

fn (mut b Builder) build_keyword_operator(kw ast.KeywordOperator) ValueID {
	match kw.op {
		.key_sizeof {
			if kw.exprs.len > 0 {
				size := b.sizeof_value(kw.exprs[0])
				if size > 0 {
					return b.mod.get_or_add_const(b.mod.type_store.get_int(32), size.str())
				}
			}
			// Fallback: sizeof(int) = 4
			return b.mod.get_or_add_const(b.mod.type_store.get_int(32), '4')
		}
		.key_go {
			// `go expr()` - launch a goroutine via the GMP scheduler.
			// NOTE: The transformer normally lowers `go` to a regular call to
			// goroutines__goroutine_create, so this path is a fallback.
			if kw.exprs.len > 0 {
				return b.build_go_or_spawn(kw.exprs[0], .go_call)
			}
			return b.mod.get_or_add_const(b.mod.type_store.get_int(64), '0')
		}
		.key_spawn {
			// `spawn expr()` - launch an OS thread.
			if kw.exprs.len > 0 {
				return b.build_go_or_spawn(kw.exprs[0], .spawn_call)
			}
			return b.mod.get_or_add_const(b.mod.type_store.get_int(64), '0')
		}
		.key_dump {
			if kw.exprs.len > 0 {
				return b.build_expr(kw.exprs[0])
			}
			return b.mod.get_or_add_const(b.mod.type_store.get_int(64), '0')
		}
		else {
			return b.mod.get_or_add_const(b.mod.type_store.get_int(64), '0')
		}
	}
}

// build_go_or_spawn emits an SSA instruction for `go fn_call()` or `spawn fn_call()`.
// For `go`: emits go_call which the C backend translates to goroutines__goroutine_create().
// For `spawn`: emits spawn_call which the C backend translates to pthread_create().
fn (mut b Builder) build_go_or_spawn(expr ast.Expr, opcode OpCode) ValueID {
	// The expression should be a function call
	if expr is ast.CallExpr {
		call := expr as ast.CallExpr
		mut operands := []ValueID{}

		// First operand: the function reference
		fn_val := b.build_expr(call.lhs)
		operands << fn_val

		// Remaining operands: the arguments
		for arg in call.args {
			arg_val := b.build_expr(arg)
			operands << arg_val
		}

		return b.mod.add_instr(opcode, b.cur_block, TypeID(0), operands)
	}
	// Fallback: just build the expression (shouldn't happen for well-formed code)
	return b.build_expr(expr)
}

// build_go_or_spawn_from_flat mirrors `build_go_or_spawn` without rehydrating
// the operand. CallExpr flat encoding is `(.expr_call, ..., [edge0=lhs,
// edge1..n=args])`. For the non-call fallback we delegate to
// `build_expr_from_flat` directly.
fn (mut b Builder) build_go_or_spawn_from_flat(c ast.Cursor, opcode OpCode) ValueID {
	if c.kind() == .expr_call {
		mut operands := []ValueID{}
		fn_val := b.build_expr_from_flat(c.edge(0))
		operands << fn_val
		for i in 1 .. c.edge_count() {
			arg_val := b.build_expr_from_flat(c.edge(i))
			operands << arg_val
		}
		return b.mod.add_instr(opcode, b.cur_block, TypeID(0), operands)
	}
	return b.build_expr_from_flat(c)
}

fn (b &Builder) sizeof_ident_name(name string) int {
	return match name {
		'int', 'i32', 'u32', 'f32' {
			4
		}
		'i8', 'u8', 'byte', 'bool' {
			1
		}
		'i16', 'u16' {
			2
		}
		'i64', 'u64', 'f64', 'isize', 'usize', 'voidptr', 'byteptr', 'charptr' {
			8
		}
		'rune' {
			4
		}
		else {
			// Array_* and Map_* are mangled names for []T and map[K]V
			// which are always the builtin array/map struct types
			if size := b.sizeof_type_alias_name(name) {
				return size
			}
			if name.starts_with('Array_fixed_') {
				return b.fixed_array_size_from_name(name)
			}
			if name.starts_with('Array_') {
				if tid := b.struct_types['array'] {
					return b.type_byte_size(tid)
				}
				return 32
			}
			if name.starts_with('Map_') {
				if tid := b.struct_types['map'] {
					return b.type_byte_size(tid)
				}
				return 120
			}
			// Look up struct type (try unqualified, then module-prefixed)
			if tid := b.struct_types[name] {
				return b.type_byte_size(tid)
			}
			// Try with current module prefix (e.g., Object → types__Object)
			if b.cur_module != '' && b.cur_module != 'main' {
				qualified := '${b.cur_module}__${name}'
				if tid := b.struct_types[qualified] {
					return b.type_byte_size(tid)
				}
			}
			// Try all registered structs as fallback
			for sname, tid in b.struct_types {
				if sname.ends_with('__${name}') {
					return b.type_byte_size(tid)
				}
			}
			// Resolve type aliases (e.g. ValueID = int → sizeof = 4)
			if b.env != unsafe { nil } {
				short_mod := if b.cur_module.contains('_') {
					b.cur_module.all_after_last('_')
				} else {
					b.cur_module
				}
				scopes_to_try := [short_mod, b.cur_module, 'builtin', 'main']
				for scope_name in scopes_to_try {
					if scope := b.env.get_scope(scope_name) {
						if obj := scope.lookup(name) {
							resolved := obj.typ()
							return b.sizeof_from_checked_type(resolved)
						}
					}
				}
				if name.contains('__') {
					stripped := name.all_after_last('__')
					for scope_name in scopes_to_try {
						if scope := b.env.get_scope(scope_name) {
							if obj := scope.lookup(stripped) {
								resolved := obj.typ()
								return b.sizeof_from_checked_type(resolved)
							}
						}
					}
				}
			}
			8 // pointer size fallback
		}
	}
}

fn (b &Builder) sizeof_value(expr ast.Expr) int {
	match expr {
		ast.Ident {
			return b.sizeof_ident_name(expr.name)
		}
		ast.Type {
			match expr {
				ast.ArrayType {
					if tid := b.struct_types['array'] {
						return b.type_byte_size(tid)
					}
					return 48 // approximate
				}
				ast.ArrayFixedType {
					elem_size := b.sizeof_value(expr.elem_type)
					arr_len := if expr.len is ast.BasicLiteral {
						int(parse_const_int_literal(expr.len.value))
					} else if expr.len is ast.Ident {
						b.resolve_const_int(expr.len.name)
					} else {
						0
					}
					if elem_size > 0 && arr_len > 0 {
						return elem_size * arr_len
					}
					return 8
				}
				ast.MapType {
					if tid := b.struct_types['map'] {
						return b.type_byte_size(tid)
					}
					return 120 // approximate
				}
				else {
					return 8
				}
			}
		}
		ast.SelectorExpr {
			lhs_name := expr.lhs.name()
			rhs_name := expr.rhs.name
			if lhs_name.len > 0 && rhs_name.len > 0 {
				qualified := '${lhs_name}__${rhs_name}'
				if size := b.sizeof_type_alias_name(qualified) {
					return size
				}
				if tid := b.struct_types[qualified] {
					return b.type_byte_size(tid)
				}
				// Resolve type aliases via the type environment (e.g. ssa.ValueID = int)
				if b.env != unsafe { nil } {
					// Scope keys use dots (e.g. 'v2.ssa'), lhs_name may already be dot-separated
					if scope := b.env.get_scope(lhs_name) {
						if obj := scope.lookup(rhs_name) {
							resolved := obj.typ()
							return b.sizeof_from_checked_type(resolved)
						}
					}
				}
			}
			return 8
		}
		else {
			return 8
		}
	}
}

// sizeof_value_from_flat mirrors `sizeof_value` but consumes a cursor instead
// of a rehydrated `ast.Expr`. Same dispatch shape: idents (incl. mangled
// Array_/Map_ names) flow through `sizeof_ident_name`; `.typ_array` /
// `.typ_array_fixed` / `.typ_map` cover type-position operands; `.expr_selector`
// covers `mod.Type` form. Anything else falls through to 8.
fn (b &Builder) sizeof_value_from_flat(c ast.Cursor) int {
	match c.kind() {
		.expr_ident {
			return b.sizeof_ident_name(c.name())
		}
		.typ_array {
			if tid := b.struct_types['array'] {
				return b.type_byte_size(tid)
			}
			return 48
		}
		.typ_array_fixed {
			len_c := c.edge(0)
			elem_c := c.edge(1)
			elem_size := b.sizeof_value_from_flat(elem_c)
			arr_len := if len_c.kind() == .expr_basic_literal {
				int(parse_const_int_literal(len_c.name()))
			} else if len_c.kind() == .expr_ident {
				b.resolve_const_int(len_c.name())
			} else {
				0
			}
			if elem_size > 0 && arr_len > 0 {
				return elem_size * arr_len
			}
			return 8
		}
		.typ_map {
			if tid := b.struct_types['map'] {
				return b.type_byte_size(tid)
			}
			return 120
		}
		.expr_selector {
			lhs_c := c.edge(0)
			rhs_c := c.edge(1)
			if lhs_c.kind() == .expr_ident && rhs_c.kind() == .expr_ident {
				lhs_name := lhs_c.name()
				rhs_name := rhs_c.name()
				if lhs_name.len > 0 && rhs_name.len > 0 {
					qualified := '${lhs_name}__${rhs_name}'
					if size := b.sizeof_type_alias_name(qualified) {
						return size
					}
					if tid := b.struct_types[qualified] {
						return b.type_byte_size(tid)
					}
					if b.env != unsafe { nil } {
						if scope := b.env.get_scope(lhs_name) {
							if obj := scope.lookup(rhs_name) {
								resolved := obj.typ()
								return b.sizeof_from_checked_type(resolved)
							}
						}
					}
				}
			}
			return 8
		}
		else {
			return 8
		}
	}
}

fn (b &Builder) fixed_array_size_from_name(name string) int {
	if !name.starts_with('Array_fixed_') {
		return 8
	}
	payload := name['Array_fixed_'.len..]
	arr_len := payload.all_after_last('_').int()
	if arr_len <= 0 {
		return 8
	}
	elem_name := payload.all_before_last('_')
	return b.type_name_byte_size(elem_name) * arr_len
}

fn (b &Builder) type_name_byte_size(name string) int {
	match name {
		'int', 'i32', 'u32', 'f32', 'rune' {
			return 4
		}
		'i8', 'u8', 'byte', 'bool' {
			return 1
		}
		'i16', 'u16' {
			return 2
		}
		'i64', 'u64', 'f64', 'isize', 'usize', 'voidptr', 'byteptr', 'charptr' {
			return 8
		}
		'string' {
			return 16
		}
		else {
			if size := b.sizeof_type_alias_name(name) {
				return size
			}
			if name.starts_with('Array_fixed_') {
				return b.fixed_array_size_from_name(name)
			}
			if name.starts_with('Array_') {
				if tid := b.struct_types['array'] {
					return b.type_byte_size(tid)
				}
				return 32
			}
			if name.starts_with('Map_') {
				if tid := b.struct_types['map'] {
					return b.type_byte_size(tid)
				}
				return 120
			}
			if tid := b.struct_types[name] {
				return b.type_byte_size(tid)
			}
			return 8
		}
	}
}

// sizeof_from_checked_type returns the byte size of a type-checker Type.
// Recursively unwraps aliases to get the base type size.
fn (b &Builder) sizeof_from_checked_type(t types.Type) int {
	if !types.type_has_valid_payload(t) {
		return 8
	}
	match t {
		types.Alias {
			base := b.resolve_alias_base_type(t) or { return 8 }
			return b.sizeof_from_checked_type(base)
		}
		types.Primitive {
			if t.size > 0 {
				return int(t.size + 7) / 8 // convert bits to bytes
			}
			// size=0 means platform-dependent int (always 4 bytes on arm64/x64)
			return 4
		}
		types.Pointer {
			return 8
		}
		types.Array {
			if tid := b.struct_types['array'] {
				return b.type_byte_size(tid)
			}
			return 32
		}
		types.ArrayFixed {
			elem_size := b.sizeof_from_checked_type(t.elem_type)
			if elem_size > 0 && t.len > 0 {
				return elem_size * t.len
			}
			return 8
		}
		types.Map {
			if tid := b.struct_types['map'] {
				return b.type_byte_size(tid)
			}
			return 120
		}
		types.Struct {
			// Look up the struct in the SSA type registry
			if tid := b.struct_types[t.name] {
				return b.type_byte_size(tid)
			}
			// Try module-qualified
			if b.cur_module != '' {
				qualified := '${b.cur_module}__${t.name}'
				if tid := b.struct_types[qualified] {
					return b.type_byte_size(tid)
				}
			}
			return 8
		}
		types.String {
			return 16
		}
		types.Enum {
			return 4
		}
		else {
			name := types.type_name(t)
			if name != '' {
				if live_type := b.lookup_checked_type_by_name(name) {
					if live_type is types.Alias {
						return b.sizeof_from_checked_type(live_type)
					}
					if types.type_name(live_type) != name {
						return b.sizeof_from_checked_type(live_type)
					}
				}
				return b.type_name_byte_size(name)
			}
			return 8
		}
	}
}

fn (b &Builder) type_byte_size(tid TypeID) int {
	mut visiting := map[TypeID]bool{}
	return b.type_byte_size_with_seen(tid, mut visiting)
}

fn (b &Builder) type_byte_size_with_seen(tid TypeID, mut visiting map[TypeID]bool) int {
	if tid == 0 || tid >= b.mod.type_store.types.len {
		return 0
	}
	if visiting[tid] {
		return 8
	}
	visiting[tid] = true
	typ := b.mod.type_store.types[tid]
	size := match typ.kind {
		.void_t {
			0
		}
		.int_t {
			if typ.width <= 8 {
				1
			} else if typ.width <= 16 {
				2
			} else if typ.width <= 32 {
				4
			} else {
				8
			}
		}
		.float_t {
			if typ.width <= 32 { 4 } else { 8 }
		}
		.ptr_t {
			8
		}
		.struct_t {
			if typ.is_union {
				// Union: size = max(field_sizes)
				mut max_size := 0
				for field in typ.fields {
					fs := b.type_byte_size_with_seen(field, mut visiting)
					if fs > max_size {
						max_size = fs
					}
				}
				max_size
			} else {
				mut struct_size := 0
				mut max_align := 1
				for field in typ.fields {
					fs := b.type_byte_size_with_seen(field, mut visiting)
					// Align to field size (simplified alignment)
					align := if fs >= 8 {
						8
					} else if fs >= 4 {
						4
					} else if fs >= 2 {
						2
					} else {
						1
					}
					if align > max_align {
						max_align = align
					}
					rem := struct_size % align
					if rem != 0 {
						struct_size += align - rem
					}
					struct_size += fs
				}
				// Add tail padding: align struct size to its largest field alignment
				if max_align > 1 {
					rem := struct_size % max_align
					if rem != 0 {
						struct_size += max_align - rem
					}
				}
				struct_size
			}
		}
		.array_t {
			b.type_byte_size_with_seen(typ.elem_type, mut visiting) * typ.len
		}
		.func_t {
			8
		}
		.label_t {
			0
		}
		.metadata_t {
			0
		}
	}

	visiting.delete(tid)
	return size
}

fn (mut b Builder) build_postfix(expr ast.PostfixExpr) ValueID {
	if expr.op in [.not, .question] {
		wrapped_val := b.build_expr(expr.expr)
		return b.build_unwrapped_postfix(expr, wrapped_val)
	}
	// expr++ or expr--
	if expr.expr is ast.Ident {
		if ptr := b.vars[expr.expr.name] {
			ptr_typ := b.mod.values[ptr].typ
			elem_typ := b.mod.type_store.types[ptr_typ].elem_type
			loaded := b.mod.add_instr(.load, b.cur_block, elem_typ, [ptr])
			is_float := elem_typ > 0 && int(elem_typ) < b.mod.type_store.types.len
				&& b.mod.type_store.types[elem_typ].kind == .float_t
			one := if is_float {
				b.mod.get_or_add_const(elem_typ, '1.0')
			} else {
				b.mod.get_or_add_const(elem_typ, '1')
			}
			op := if is_float {
				if expr.op == .inc { OpCode.fadd } else { OpCode.fsub }
			} else {
				if expr.op == .inc { OpCode.add } else { OpCode.sub }
			}
			result := b.mod.add_instr(op, b.cur_block, elem_typ, [loaded, one])
			b.mod.add_instr(.store, b.cur_block, 0, [result, ptr])
			return loaded // postfix returns old value
		}
	}
	// Handle SelectorExpr and IndexExpr: obj.field++ or arr[i]++
	if expr.expr is ast.SelectorExpr || expr.expr is ast.IndexExpr {
		ptr := b.build_addr(expr.expr)
		if ptr != 0 {
			ptr_typ := b.mod.values[ptr].typ
			elem_typ := if ptr_typ < b.mod.type_store.types.len {
				b.mod.type_store.types[ptr_typ].elem_type
			} else {
				b.mod.type_store.get_int(32)
			}
			loaded := b.mod.add_instr(.load, b.cur_block, elem_typ, [ptr])
			is_float := elem_typ > 0 && int(elem_typ) < b.mod.type_store.types.len
				&& b.mod.type_store.types[elem_typ].kind == .float_t
			one := if is_float {
				b.mod.get_or_add_const(elem_typ, '1.0')
			} else {
				b.mod.get_or_add_const(elem_typ, '1')
			}
			op := if is_float {
				if expr.op == .inc { OpCode.fadd } else { OpCode.fsub }
			} else {
				if expr.op == .inc { OpCode.add } else { OpCode.sub }
			}
			result := b.mod.add_instr(op, b.cur_block, elem_typ, [loaded, one])
			b.mod.add_instr(.store, b.cur_block, 0, [result, ptr])
			return loaded // postfix returns old value
		}
	}
	return b.build_expr(expr.expr)
}

// --- Address computation ---

fn (mut b Builder) build_addr(expr ast.Expr) ValueID {
	match expr {
		ast.Ident {
			if expr.name in b.vars {
				ptr := b.vars[expr.name]
				// For mut pointer params (e.g., mut buf &u8), the alloca stores ptr(ptr(T)).
				// When used as a mut arg to another function (build_addr call),
				// return the loaded value (ptr(ptr(T))) instead of the alloca (ptr(ptr(ptr(T)))).
				// This forwards the same indirection level instead of adding another layer.
				if expr.name in b.mut_ptr_params {
					ptr_typ := b.mod.values[ptr].typ
					if b.valid_type_id(ptr_typ) {
						elem_typ := b.mod.type_store.types[ptr_typ].elem_type
						if elem_typ > 0 {
							return b.mod.add_instr(.load, b.cur_block, elem_typ, [ptr])
						}
					}
					return ptr
				}
				return ptr
			}
			if glob_id := b.find_global_ident(expr.name) {
				return glob_id
			}
			return 0
		}
		ast.SelectorExpr {
			if expr.lhs is ast.Ident && expr.lhs.name == 'C' && expr.rhs.name == 'errno' {
				return b.build_c_errno_storage_addr()
			}
			if expr.lhs is ast.Ident {
				if mod_name := b.selector_module_name(expr) {
					qualified := ssa_module_storage_name(mod_name, expr.rhs.name)
					if glob_id := b.find_global(qualified) {
						return glob_id
					}
				}
			}
			// Get address of base (not the loaded value)
			mut base := b.build_addr(expr.lhs)
			if base == 0 {
				return 0
			}
			// If base is ptr-to-ptr-to-struct (mut receiver), load to get the struct pointer
			base_typ := b.mod.values[base].typ
			if b.valid_type_id(base_typ) && b.mod.type_store.types[base_typ].kind == .ptr_t {
				inner := b.mod.type_store.types[base_typ].elem_type
				if b.valid_type_id(inner) && b.mod.type_store.types[inner].kind == .ptr_t {
					pointee := b.mod.type_store.types[inner].elem_type
					if b.valid_type_id(pointee) && b.mod.type_store.types[pointee].kind == .struct_t {
						// Load from alloca to get the struct pointer
						base = b.mod.add_instr(.load, b.cur_block, inner, [base])
					}
				}
			}
			fi := b.field_index(expr, base)
			if fi < 0 {
				return 0
			}
			idx_val := b.mod.get_or_add_const(b.mod.type_store.get_int(32), fi.str())
			// Prefer SSA struct field type over expr_type for correct pointer sizing.
			// Synthetic selectors (e.g. _or_t1.data) may have expr_type=voidptr from
			// the transformer, but the actual struct field type (e.g. string) is needed
			// so the ARM64 store correctly identifies struct destinations.
			mut result_type := TypeID(0)
			base_typ2 := b.mod.values[base].typ
			if b.valid_type_id(base_typ2) {
				bt2 := b.mod.type_store.types[base_typ2]
				if bt2.kind == .ptr_t && b.valid_type_id(bt2.elem_type) {
					struct_typ := b.mod.type_store.types[bt2.elem_type]
					if struct_typ.kind == .struct_t && fi >= 0 && fi < struct_typ.fields.len {
						result_type = struct_typ.fields[fi]
					}
				}
			}
			if result_type == 0 {
				result_type = b.expr_type(ast.Expr(expr))
			}
			return b.mod.add_instr(.get_element_ptr, b.cur_block,
				b.mod.type_store.get_ptr(result_type), [base, idx_val])
		}
		ast.ParenExpr {
			return b.build_addr(expr.expr)
		}
		ast.PrefixExpr {
			if expr.op == .mul {
				// Dereference: addr of (*ptr) is just ptr itself
				return b.build_expr(expr.expr)
			}
			if expr.op == .amp {
				// Address-of: addr of (&x) — evaluate x's address
				return b.build_addr(expr.expr)
			}
			return 0
		}
		ast.CastExpr {
			// Cast preserves pointer value — evaluate the inner expression
			return b.build_expr(expr)
		}
		ast.IndexExpr {
			// Try address-based access first for fixed-size arrays and struct fields.
			// This avoids loading large values (e.g., [256]char in C.dirent.d_name)
			// and instead computes pointer + GEP.
			base_addr := b.build_addr(expr.lhs)
			if base_addr != 0 {
				addr_typ_id := b.mod.values[base_addr].typ
				if b.valid_type_id(addr_typ_id) {
					addr_typ := b.mod.type_store.types[addr_typ_id]
					if addr_typ.kind == .ptr_t {
						pointee := addr_typ.elem_type
						if b.valid_type_id(pointee) {
							pointee_typ := b.mod.type_store.types[pointee]
							// For pointer to fixed-size array: GEP into the array elements
							if pointee_typ.kind == .array_t && pointee_typ.elem_type > 0 {
								index := b.build_expr(expr.expr)
								elem_ptr_type := b.mod.type_store.get_ptr(pointee_typ.elem_type)
								return b.mod.add_instr(.get_element_ptr, b.cur_block,
									elem_ptr_type, [base_addr, index])
							}
						}
					}
				}
			}
			mut base2 := b.build_expr(expr.lhs)
			index := b.build_expr(expr.expr)
			mut result_type := b.expr_type(ast.Expr(expr))
			// For dynamic arrays, extract .data pointer first (mirrors build_index logic)
			base_type_id := b.mod.values[base2].typ
			array_type := b.get_array_type()
			// If base is ptr(array) (mut []T param), deref to get the array struct
			if array_type != 0 && base_type_id != array_type && b.valid_type_id(base_type_id) {
				btyp := b.mod.type_store.types[base_type_id]
				if btyp.kind == .ptr_t && btyp.elem_type == array_type {
					base2 = b.mod.add_instr(.load, b.cur_block, array_type, [base2])
				}
			}
			base_type_id2 := b.mod.values[base2].typ
			if array_type != 0 && base_type_id2 == array_type {
				result_type = b.infer_dynamic_array_index_type(expr, base2, result_type)
				// Extract .data field (index 0), cast to element pointer, then GEP
				i8_t := b.mod.type_store.get_int(8)
				void_ptr := b.mod.type_store.get_ptr(i8_t)
				data_ptr := b.mod.add_instr(.extractvalue, b.cur_block, void_ptr, [
					base2,
					b.mod.get_or_add_const(b.mod.type_store.get_int(32), '0'),
				])
				elem_ptr_type := b.mod.type_store.get_ptr(result_type)
				typed_ptr := b.mod.add_instr(.bitcast, b.cur_block, elem_ptr_type, [
					data_ptr,
				])
				return b.mod.add_instr(.get_element_ptr, b.cur_block, elem_ptr_type, [
					typed_ptr,
					index,
				])
			}
			// For pointers (fixed-size arrays), GEP directly
			if b.valid_type_id(base_type_id) {
				base_typ := b.mod.type_store.types[base_type_id]
				if base_typ.kind == .ptr_t && base_typ.elem_type > 0 {
					mut elem_type := base_typ.elem_type
					if b.valid_type_id(elem_type) {
						inner_typ := b.mod.type_store.types[elem_type]
						if inner_typ.kind == .array_t && inner_typ.elem_type > 0 {
							elem_type = inner_typ.elem_type
						}
					}
					elem_ptr_type := b.mod.type_store.get_ptr(elem_type)
					return b.mod.add_instr(.get_element_ptr, b.cur_block, elem_ptr_type, [
						base2,
						index,
					])
				}
			}
			return b.mod.add_instr(.get_element_ptr, b.cur_block,
				b.mod.type_store.get_ptr(result_type), [base2, index])
		}
		else {
			return 0
		}
	}
}

fn (mut b Builder) build_addr_from_flat(c ast.Cursor) ValueID {
	match c.kind() {
		.expr_ident {
			name := c.name()
			if name in b.vars {
				ptr := b.vars[name]
				if name in b.mut_ptr_params {
					ptr_typ := b.mod.values[ptr].typ
					if b.valid_type_id(ptr_typ) {
						elem_typ := b.mod.type_store.types[ptr_typ].elem_type
						if elem_typ > 0 {
							return b.mod.add_instr(.load, b.cur_block, elem_typ, [ptr])
						}
					}
					return ptr
				}
				return ptr
			}
			if glob_id := b.find_global_ident(name) {
				return glob_id
			}
			return 0
		}
		.expr_selector {
			lhs_c := c.edge(0)
			rhs_c := c.edge(1)
			if lhs_c.kind() == .expr_ident && lhs_c.name() == 'C' && rhs_c.name() == 'errno' {
				return b.build_c_errno_storage_addr()
			}
			if lhs_c.kind() == .expr_ident {
				if mod_name := b.selector_module_name_from_flat(c) {
					qualified := ssa_module_storage_name(mod_name, rhs_c.name())
					if glob_id := b.find_global(qualified) {
						return glob_id
					}
				}
			}
			mut base := b.build_addr_from_flat(lhs_c)
			if base == 0 {
				return 0
			}
			base_typ := b.mod.values[base].typ
			if b.valid_type_id(base_typ) && b.mod.type_store.types[base_typ].kind == .ptr_t {
				inner := b.mod.type_store.types[base_typ].elem_type
				if b.valid_type_id(inner) && b.mod.type_store.types[inner].kind == .ptr_t {
					pointee := b.mod.type_store.types[inner].elem_type
					if b.valid_type_id(pointee) && b.mod.type_store.types[pointee].kind == .struct_t {
						base = b.mod.add_instr(.load, b.cur_block, inner, [base])
					}
				}
			}
			fi := b.field_index_from_flat(c, base)
			if fi < 0 {
				return 0
			}
			idx_val := b.mod.get_or_add_const(b.mod.type_store.get_int(32), fi.str())
			mut result_type := TypeID(0)
			base_typ2 := b.mod.values[base].typ
			if b.valid_type_id(base_typ2) {
				bt2 := b.mod.type_store.types[base_typ2]
				if bt2.kind == .ptr_t && b.valid_type_id(bt2.elem_type) {
					struct_typ := b.mod.type_store.types[bt2.elem_type]
					if struct_typ.kind == .struct_t && fi >= 0 && fi < struct_typ.fields.len {
						result_type = struct_typ.fields[fi]
					}
				}
			}
			if result_type == 0 {
				result_type = b.expr_type_from_flat(c)
			}
			return b.mod.add_instr(.get_element_ptr, b.cur_block,
				b.mod.type_store.get_ptr(result_type), [base, idx_val])
		}
		.expr_paren {
			return b.build_addr_from_flat(c.edge(0))
		}
		.expr_prefix {
			op := unsafe { token.Token(int(c.aux())) }
			if op == .mul {
				return b.build_expr_from_flat(c.edge(0))
			}
			if op == .amp {
				return b.build_addr_from_flat(c.edge(0))
			}
			return 0
		}
		.expr_cast {
			return b.build_expr_from_flat(c)
		}
		.expr_index {
			lhs_c := c.edge(0)
			index_c := c.edge(1)
			base_addr := b.build_addr_from_flat(lhs_c)
			if base_addr != 0 {
				addr_typ_id := b.mod.values[base_addr].typ
				if b.valid_type_id(addr_typ_id) {
					addr_typ := b.mod.type_store.types[addr_typ_id]
					if addr_typ.kind == .ptr_t {
						pointee := addr_typ.elem_type
						if b.valid_type_id(pointee) {
							pointee_typ := b.mod.type_store.types[pointee]
							if pointee_typ.kind == .array_t && pointee_typ.elem_type > 0 {
								index := b.build_expr_from_flat(index_c)
								elem_ptr_type := b.mod.type_store.get_ptr(pointee_typ.elem_type)
								return b.mod.add_instr(.get_element_ptr, b.cur_block,
									elem_ptr_type, [base_addr, index])
							}
						}
					}
				}
			}
			mut base2 := b.build_expr_from_flat(lhs_c)
			index := b.build_expr_from_flat(index_c)
			mut result_type := b.expr_type_from_flat(c)
			base_type_id := b.mod.values[base2].typ
			array_type := b.get_array_type()
			if array_type != 0 && base_type_id != array_type && b.valid_type_id(base_type_id) {
				btyp := b.mod.type_store.types[base_type_id]
				if btyp.kind == .ptr_t && btyp.elem_type == array_type {
					base2 = b.mod.add_instr(.load, b.cur_block, array_type, [base2])
				}
			}
			base_type_id2 := b.mod.values[base2].typ
			if array_type != 0 && base_type_id2 == array_type {
				result_type = b.infer_dynamic_array_index_type_from_flat(c, base2, result_type)
				i8_t := b.mod.type_store.get_int(8)
				void_ptr := b.mod.type_store.get_ptr(i8_t)
				data_ptr := b.mod.add_instr(.extractvalue, b.cur_block, void_ptr, [
					base2,
					b.mod.get_or_add_const(b.mod.type_store.get_int(32), '0'),
				])
				elem_ptr_type := b.mod.type_store.get_ptr(result_type)
				typed_ptr := b.mod.add_instr(.bitcast, b.cur_block, elem_ptr_type, [
					data_ptr,
				])
				return b.mod.add_instr(.get_element_ptr, b.cur_block, elem_ptr_type, [
					typed_ptr,
					index,
				])
			}
			if b.valid_type_id(base_type_id) {
				base_typ := b.mod.type_store.types[base_type_id]
				if base_typ.kind == .ptr_t && base_typ.elem_type > 0 {
					mut elem_type := base_typ.elem_type
					if b.valid_type_id(elem_type) {
						inner_typ := b.mod.type_store.types[elem_type]
						if inner_typ.kind == .array_t && inner_typ.elem_type > 0 {
							elem_type = inner_typ.elem_type
						}
					}
					elem_ptr_type := b.mod.type_store.get_ptr(elem_type)
					return b.mod.add_instr(.get_element_ptr, b.cur_block, elem_ptr_type, [
						base2,
						index,
					])
				}
			}
			return b.mod.add_instr(.get_element_ptr, b.cur_block,
				b.mod.type_store.get_ptr(result_type), [base2, index])
		}
		else {
			return 0
		}
	}
}

// --- Helpers ---

fn (mut b Builder) add_edge(from BlockID, to BlockID) {
	b.mod.block_add_succ(from, to)
	b.mod.block_add_pred(to, from)
}

fn (mut b Builder) get_or_create_fn_ref(name string, _typ TypeID) ValueID {
	if name in b.fn_refs {
		return b.fn_refs[name]
	}
	// If the function is not registered (no FnDecl found) and its name ends with __str,
	// generate a minimal stub that returns an empty string. This handles cases where
	// the transformer failed to generate an auto str function (e.g., enum str functions
	// whose type couldn't be resolved during string interpolation processing).
	if name !in b.fn_index && name.ends_with('__str') {
		str_type := b.get_string_type()
		if str_type != 0 {
			fn_idx := b.mod.new_function(name, str_type, []TypeID{})
			b.fn_index[name] = fn_idx
			save_func := b.cur_func
			save_block := b.cur_block
			b.cur_func = fn_idx
			entry := b.mod.add_block(fn_idx, 'entry')
			b.cur_block = entry
			empty_str := b.mod.add_value_node(.string_literal, str_type, '', 0)
			b.mod.add_instr(.ret, b.cur_block, 0, [empty_str])
			b.cur_func = save_func
			b.cur_block = save_block
		}
	}
	if name !in b.fn_index && name.starts_with('Map_') && name.ends_with('_map_eq') {
		b.generate_map_eq_stub(name)
	}
	// Always use pointer type for function references so that when stored/loaded
	// through alloca (e.g. function pointer parameters), the full 8-byte
	// address is preserved.
	fn_ptr_type := b.mod.type_store.get_ptr(b.mod.type_store.get_int(8))
	ref := b.mod.add_value_node(.func_ref, fn_ptr_type, name, 0)
	b.fn_refs[name] = ref
	return ref
}

fn (mut b Builder) build_fn_literal(expr ast.FnLiteral) ValueID {
	if expr.captured_vars.len > 0 {
		return b.get_or_create_fn_ref(unsupported_captured_fn_literal_symbol, 0)
	}

	// Generate unique name for this anonymous function
	anon_name := '_anon_fn_${b.anon_fn_counter}'
	b.anon_fn_counter++

	// Determine return type
	ret_type := b.ast_type_to_ssa(expr.typ.return_type)

	// Register the function
	func_idx := b.mod.new_function(anon_name, ret_type, []TypeID{})
	b.fn_index[anon_name] = func_idx

	// Save current builder state
	saved_func := b.cur_func
	saved_block := b.cur_block
	mut saved_vars := b.vars.clone()
	mut saved_mut_ptr_params := b.mut_ptr_params.clone()
	mut saved_local_smartcasts := b.local_smartcasts.clone()
	saved_loop_stack := b.loop_stack.clone()

	// Switch to building the anonymous function
	b.cur_func = func_idx
	b.vars = map[string]ValueID{}
	b.mut_ptr_params = map[string]bool{}
	b.local_smartcasts = map[string]TypeID{}
	b.loop_stack = []LoopInfo{}

	// Create entry block
	entry := b.mod.add_block(func_idx, 'entry')
	b.cur_block = entry

	// Add parameters
	for param in expr.typ.params {
		param_type := b.ast_type_to_ssa(param.typ)
		actual_type := if param.is_mut && !(param_type < b.mod.type_store.types.len
			&& b.mod.type_store.types[param_type].kind == .ptr_t) {
			b.mod.type_store.get_ptr(param_type)
		} else {
			param_type
		}
		param_val := b.mod.add_value_node(.argument, actual_type, param.name, 0)
		b.mod.func_add_param(func_idx, param_val)
		// Alloca + store
		alloca := b.mod.add_instr(.alloca, entry, b.mod.type_store.get_ptr(actual_type),
			[]ValueID{})
		b.mod.add_instr(.store, entry, 0, [param_val, alloca])
		b.vars[param.name] = alloca
		if param.is_mut && !(param_type < b.mod.type_store.types.len
			&& b.mod.type_store.types[param_type].kind == .ptr_t) {
			b.mut_ptr_params[param.name] = true
		}
	}

	// Build body
	b.build_stmts(expr.stmts)

	// If no terminator, add implicit return
	if !b.block_has_terminator(b.cur_block) {
		b.mod.add_instr(.ret, b.cur_block, 0, []ValueID{})
	}

	// Restore builder state
	b.cur_func = saved_func
	b.cur_block = saved_block
	b.vars = saved_vars.move()
	b.mut_ptr_params = saved_mut_ptr_params.move()
	b.local_smartcasts = saved_local_smartcasts.move()
	b.loop_stack = saved_loop_stack

	// Return a function reference to the anonymous function.
	// Use pointer type (ptr(i8)) so that when stored in variables via alloca,
	// the full 8-byte function address is preserved (not truncated to i32).
	fn_ptr_type := b.mod.type_store.get_ptr(b.mod.type_store.get_int(8))
	fn_ref := b.mod.add_value_node(.func_ref, fn_ptr_type, anon_name, 0)
	b.fn_refs[anon_name] = fn_ref
	return fn_ref
}

// generate_array_eq_stub creates a synthetic `array__eq` function that compares two arrays.
// The transformer generates `array__eq(arr1, arr2)` for `arr1 == arr2`.
// Implementation: compare len fields, then compare data.
// For string elements (element_size == 16), does element-wise string__eq comparison.
// For other types, uses memcmp.
fn (mut b Builder) generate_array_eq_stub() {
	if 'array__eq' in b.fn_index {
		return
	}
	i32_t := b.mod.type_store.get_int(32)
	i1_t := b.mod.type_store.get_int(1)
	i8_t := b.mod.type_store.get_int(8)
	ptr_t := b.mod.type_store.get_ptr(i8_t)
	array_t := if 'array' in b.struct_types { b.struct_types['array'] } else { return }
	string_t := b.get_string_type()

	// Register function: array__eq(a: array, b: array) -> bool
	func_idx := b.mod.new_function('array__eq', i1_t, []TypeID{})
	b.fn_index['array__eq'] = func_idx
	entry := b.mod.add_block(func_idx, 'entry')

	// Add parameters
	param_a := b.mod.add_value_node(.argument, array_t, 'a', 0)
	param_b := b.mod.add_value_node(.argument, array_t, 'b', 0)
	b.mod.func_add_param(func_idx, param_a)
	b.mod.func_add_param(func_idx, param_b)

	// Alloca + store params
	alloca_a := b.mod.add_instr(.alloca, entry, b.mod.type_store.get_ptr(array_t), []ValueID{})
	b.mod.add_instr(.store, entry, 0, [param_a, alloca_a])
	alloca_b := b.mod.add_instr(.alloca, entry, b.mod.type_store.get_ptr(array_t), []ValueID{})
	b.mod.add_instr(.store, entry, 0, [param_b, alloca_b])

	// Extract len fields (field index 2 in array struct: data=0, offset=1, len=2)
	val_a := b.mod.add_instr(.load, entry, array_t, [alloca_a])
	val_b := b.mod.add_instr(.load, entry, array_t, [alloca_b])
	idx2 := b.mod.get_or_add_const(i32_t, '2')
	len_a := b.mod.add_instr(.extractvalue, entry, i32_t, [val_a, idx2])
	len_b := b.mod.add_instr(.extractvalue, entry, i32_t, [val_b, idx2])

	// Compare lengths
	len_eq := b.mod.add_instr(.eq, entry, i1_t, [len_a, len_b])

	// If lengths differ, return false
	check_elem_block := b.mod.add_block(func_idx, 'check_elem')
	ret_false_block := b.mod.add_block(func_idx, 'ret_false')
	ret_true_block := b.mod.add_block(func_idx, 'ret_true')
	b.mod.add_instr(.br, entry, 0,
		[len_eq, b.mod.blocks[check_elem_block].val_id, b.mod.blocks[ret_false_block].val_id])

	// ret_false: return 0
	zero_i1 := b.mod.get_or_add_const(i1_t, '0')
	b.mod.add_instr(.ret, ret_false_block, 0, [zero_i1])

	// ret_true: return 1
	one_i1 := b.mod.get_or_add_const(i1_t, '1')
	b.mod.add_instr(.ret, ret_true_block, 0, [one_i1])

	// check_elem block: extract data pointers and element_size, branch based on elem type
	val_a2 := b.mod.add_instr(.load, check_elem_block, array_t, [alloca_a])
	val_b2 := b.mod.add_instr(.load, check_elem_block, array_t, [alloca_b])
	idx0 := b.mod.get_or_add_const(i32_t, '0')
	data_a := b.mod.add_instr(.extractvalue, check_elem_block, ptr_t, [val_a2, idx0])
	data_b := b.mod.add_instr(.extractvalue, check_elem_block, ptr_t, [val_b2, idx0])
	idx5 := b.mod.get_or_add_const(i32_t, '5')
	elem_size := b.mod.add_instr(.extractvalue, check_elem_block, i32_t, [val_a2, idx5])

	// Store data pointers and elem_size for use in both paths
	alloca_data_a := b.mod.add_instr(.alloca, check_elem_block, b.mod.type_store.get_ptr(ptr_t),
		[]ValueID{})
	b.mod.add_instr(.store, check_elem_block, 0, [data_a, alloca_data_a])
	alloca_data_b := b.mod.add_instr(.alloca, check_elem_block, b.mod.type_store.get_ptr(ptr_t),
		[]ValueID{})
	b.mod.add_instr(.store, check_elem_block, 0, [data_b, alloca_data_b])
	alloca_esz := b.mod.add_instr(.alloca, check_elem_block, b.mod.type_store.get_ptr(i32_t),
		[]ValueID{})
	b.mod.add_instr(.store, check_elem_block, 0, [elem_size, alloca_esz])
	alloca_len := b.mod.add_instr(.alloca, check_elem_block, b.mod.type_store.get_ptr(i32_t),
		[]ValueID{})
	b.mod.add_instr(.store, check_elem_block, 0, [len_a, alloca_len])

	// Check if element_size == sizeof(string) for string comparison
	// sizeof(string) = 16 on most platforms (ptr + int + int)
	string_size := b.mod.get_or_add_const(i32_t, '16')
	is_string := b.mod.add_instr(.eq, check_elem_block, i1_t, [elem_size, string_size])

	str_loop_header := b.mod.add_block(func_idx, 'str_loop_header')
	check_array_block := b.mod.add_block(func_idx, 'check_array')
	b.mod.add_instr(.br, check_elem_block, 0, [is_string, b.mod.blocks[str_loop_header].val_id,
		b.mod.blocks[check_array_block].val_id])

	// --- Check if element_size == sizeof(array) for nested array comparison ---
	// sizeof(array) = 32 on arm64 (ptr=8 + 5*i32=20, padded to 8-byte align = 32)
	array_size := b.mod.get_or_add_const(i32_t, '32')
	is_array := b.mod.add_instr(.eq, check_array_block, i1_t, [elem_size, array_size])

	arr_loop_header := b.mod.add_block(func_idx, 'arr_loop_header')
	check_map_block := b.mod.add_block(func_idx, 'check_map')
	b.mod.add_instr(.br, check_array_block, 0, [is_array, b.mod.blocks[arr_loop_header].val_id,
		b.mod.blocks[check_map_block].val_id])

	// --- Check if element_size == sizeof(map) for map element comparison ---
	map_size_val := if map_t := b.struct_types['map'] {
		map_sz := b.type_byte_size(map_t)
		b.mod.get_or_add_const(i32_t, '${map_sz}')
	} else {
		b.mod.get_or_add_const(i32_t, '120')
	}
	is_map := b.mod.add_instr(.eq, check_map_block, i1_t, [elem_size, map_size_val])

	map_loop_header := b.mod.add_block(func_idx, 'map_loop_header')
	memcmp_block := b.mod.add_block(func_idx, 'memcmp')
	b.mod.add_instr(.br, check_map_block, 0,
		[is_map, b.mod.blocks[map_loop_header].val_id, b.mod.blocks[memcmp_block].val_id])

	// --- String element comparison loop ---
	// Loop header: i stored in alloca, check if i < len
	alloca_i := b.mod.add_instr(.alloca, str_loop_header, b.mod.type_store.get_ptr(i32_t),
		[]ValueID{})
	zero_i32 := b.mod.get_or_add_const(i32_t, '0')
	b.mod.add_instr(.store, str_loop_header, 0, [zero_i32, alloca_i])

	str_loop_cond := b.mod.add_block(func_idx, 'str_loop_cond')
	b.mod.add_instr(.br, str_loop_header, 0,
		[one_i1, b.mod.blocks[str_loop_cond].val_id, b.mod.blocks[str_loop_cond].val_id])

	// Loop condition: load i, compare with len
	cur_i := b.mod.add_instr(.load, str_loop_cond, i32_t, [alloca_i])
	cur_len := b.mod.add_instr(.load, str_loop_cond, i32_t, [alloca_len])
	i_lt_len := b.mod.add_instr(.lt, str_loop_cond, i1_t, [cur_i, cur_len])

	str_loop_body := b.mod.add_block(func_idx, 'str_loop_body')
	b.mod.add_instr(.br, str_loop_cond, 0,
		[i_lt_len, b.mod.blocks[str_loop_body].val_id, b.mod.blocks[ret_true_block].val_id])

	// Loop body: compare strings at index i
	cur_data_a := b.mod.add_instr(.load, str_loop_body, ptr_t, [alloca_data_a])
	cur_data_b := b.mod.add_instr(.load, str_loop_body, ptr_t, [alloca_data_b])
	cur_i2 := b.mod.add_instr(.load, str_loop_body, i32_t, [alloca_i])

	// Compute byte offset: i * 16 (sizeof string)
	byte_off := b.mod.add_instr(.mul, str_loop_body, i32_t, [cur_i2, string_size])

	// Get pointers to string[i] in each array
	str_ptr_a := b.mod.add_instr(.get_element_ptr, str_loop_body, ptr_t, [cur_data_a, byte_off])
	str_ptr_b := b.mod.add_instr(.get_element_ptr, str_loop_body, ptr_t, [cur_data_b, byte_off])

	// Cast to string pointer and load string structs
	str_ptr_type := b.mod.type_store.get_ptr(string_t)
	typed_str_a := b.mod.add_instr(.bitcast, str_loop_body, str_ptr_type, [str_ptr_a])
	typed_str_b := b.mod.add_instr(.bitcast, str_loop_body, str_ptr_type, [str_ptr_b])
	str_a := b.mod.add_instr(.load, str_loop_body, string_t, [typed_str_a])
	str_b := b.mod.add_instr(.load, str_loop_body, string_t, [typed_str_b])

	// Call string__==(str_a, str_b)
	str_eq_ref := b.get_or_create_fn_ref('builtin__string__==', i1_t)
	str_eq := b.mod.add_instr(.call, str_loop_body, i1_t, [str_eq_ref, str_a, str_b])

	// If not equal, return false
	str_loop_inc := b.mod.add_block(func_idx, 'str_loop_inc')
	b.mod.add_instr(.br, str_loop_body, 0,
		[str_eq, b.mod.blocks[str_loop_inc].val_id, b.mod.blocks[ret_false_block].val_id])

	// Loop increment: i++
	one_i32 := b.mod.get_or_add_const(i32_t, '1')
	next_i := b.mod.add_instr(.add, str_loop_inc, i32_t, [cur_i2, one_i32])
	b.mod.add_instr(.store, str_loop_inc, 0, [next_i, alloca_i])
	b.mod.add_instr(.br, str_loop_inc, 0,
		[one_i1, b.mod.blocks[str_loop_cond].val_id, b.mod.blocks[str_loop_cond].val_id])

	// --- Nested array element comparison loop (recursive array__eq) ---
	alloca_ai := b.mod.add_instr(.alloca, arr_loop_header, b.mod.type_store.get_ptr(i32_t),
		[]ValueID{})
	b.mod.add_instr(.store, arr_loop_header, 0, [zero_i32, alloca_ai])

	arr_loop_cond := b.mod.add_block(func_idx, 'arr_loop_cond')
	b.mod.add_instr(.br, arr_loop_header, 0,
		[one_i1, b.mod.blocks[arr_loop_cond].val_id, b.mod.blocks[arr_loop_cond].val_id])

	// Loop condition: load i, compare with len
	acur_i := b.mod.add_instr(.load, arr_loop_cond, i32_t, [alloca_ai])
	acur_len := b.mod.add_instr(.load, arr_loop_cond, i32_t, [alloca_len])
	ai_lt_len := b.mod.add_instr(.lt, arr_loop_cond, i1_t, [acur_i, acur_len])

	arr_loop_body := b.mod.add_block(func_idx, 'arr_loop_body')
	b.mod.add_instr(.br, arr_loop_cond, 0,
		[ai_lt_len, b.mod.blocks[arr_loop_body].val_id, b.mod.blocks[ret_true_block].val_id])

	// Loop body: compare array elements at index i
	acur_data_a := b.mod.add_instr(.load, arr_loop_body, ptr_t, [alloca_data_a])
	acur_data_b := b.mod.add_instr(.load, arr_loop_body, ptr_t, [alloca_data_b])
	acur_i2 := b.mod.add_instr(.load, arr_loop_body, i32_t, [alloca_ai])

	// Compute byte offset: i * 32 (sizeof array)
	abyte_off := b.mod.add_instr(.mul, arr_loop_body, i32_t, [acur_i2, array_size])

	// Get pointers to array[i] in each array
	arr_ptr_a := b.mod.add_instr(.get_element_ptr, arr_loop_body, ptr_t, [acur_data_a, abyte_off])
	arr_ptr_b := b.mod.add_instr(.get_element_ptr, arr_loop_body, ptr_t, [acur_data_b, abyte_off])

	// Cast to array pointer and load array structs
	arr_ptr_type := b.mod.type_store.get_ptr(array_t)
	typed_arr_a := b.mod.add_instr(.bitcast, arr_loop_body, arr_ptr_type, [arr_ptr_a])
	typed_arr_b := b.mod.add_instr(.bitcast, arr_loop_body, arr_ptr_type, [arr_ptr_b])
	elem_a := b.mod.add_instr(.load, arr_loop_body, array_t, [typed_arr_a])
	elem_b := b.mod.add_instr(.load, arr_loop_body, array_t, [typed_arr_b])

	// Recursive call: array__eq(elem_a, elem_b)
	arr_eq_ref := b.get_or_create_fn_ref('array__eq', i1_t)
	arr_eq := b.mod.add_instr(.call, arr_loop_body, i1_t, [arr_eq_ref, elem_a, elem_b])

	// If not equal, return false
	arr_loop_inc := b.mod.add_block(func_idx, 'arr_loop_inc')
	b.mod.add_instr(.br, arr_loop_body, 0,
		[arr_eq, b.mod.blocks[arr_loop_inc].val_id, b.mod.blocks[ret_false_block].val_id])

	// Loop increment: i++
	anext_i := b.mod.add_instr(.add, arr_loop_inc, i32_t, [acur_i2, one_i32])
	b.mod.add_instr(.store, arr_loop_inc, 0, [anext_i, alloca_ai])
	b.mod.add_instr(.br, arr_loop_inc, 0,
		[one_i1, b.mod.blocks[arr_loop_cond].val_id, b.mod.blocks[arr_loop_cond].val_id])

	// --- Map element comparison loop (call map_map_eq for each element) ---
	alloca_mi := b.mod.add_instr(.alloca, map_loop_header, b.mod.type_store.get_ptr(i32_t),
		[]ValueID{})
	b.mod.add_instr(.store, map_loop_header, 0, [zero_i32, alloca_mi])

	map_loop_cond := b.mod.add_block(func_idx, 'map_loop_cond')
	b.mod.add_instr(.br, map_loop_header, 0,
		[one_i1, b.mod.blocks[map_loop_cond].val_id, b.mod.blocks[map_loop_cond].val_id])

	// Loop condition: load i, compare with len
	mcur_i := b.mod.add_instr(.load, map_loop_cond, i32_t, [alloca_mi])
	mcur_len := b.mod.add_instr(.load, map_loop_cond, i32_t, [alloca_len])
	mi_lt_len := b.mod.add_instr(.lt, map_loop_cond, i1_t, [mcur_i, mcur_len])

	map_loop_body := b.mod.add_block(func_idx, 'map_loop_body')
	b.mod.add_instr(.br, map_loop_cond, 0,
		[mi_lt_len, b.mod.blocks[map_loop_body].val_id, b.mod.blocks[ret_true_block].val_id])

	// Loop body: compare map elements at index i
	mcur_data_a := b.mod.add_instr(.load, map_loop_body, ptr_t, [alloca_data_a])
	mcur_data_b := b.mod.add_instr(.load, map_loop_body, ptr_t, [alloca_data_b])
	mcur_i2 := b.mod.add_instr(.load, map_loop_body, i32_t, [alloca_mi])

	// Compute byte offset: i * sizeof(map)
	mbyte_off := b.mod.add_instr(.mul, map_loop_body, i32_t, [mcur_i2, map_size_val])

	// Get pointers to map[i] in each array
	map_ptr_a := b.mod.add_instr(.get_element_ptr, map_loop_body, ptr_t, [mcur_data_a, mbyte_off])
	map_ptr_b := b.mod.add_instr(.get_element_ptr, map_loop_body, ptr_t, [mcur_data_b, mbyte_off])

	// Load map structs
	map_t2 := b.struct_types['map'] or { b.mod.type_store.get_int(64) }
	map_ptr_type := b.mod.type_store.get_ptr(map_t2)
	typed_map_a := b.mod.add_instr(.bitcast, map_loop_body, map_ptr_type, [map_ptr_a])
	typed_map_b := b.mod.add_instr(.bitcast, map_loop_body, map_ptr_type, [map_ptr_b])
	elem_map_a := b.mod.add_instr(.load, map_loop_body, map_t2, [typed_map_a])
	elem_map_b := b.mod.add_instr(.load, map_loop_body, map_t2, [typed_map_b])

	// Call map_map_eq(map_a, map_b)
	map_eq_ref := b.get_or_create_fn_ref('builtin__map_map_eq', i1_t)
	map_eq := b.mod.add_instr(.call, map_loop_body, i1_t, [map_eq_ref, elem_map_a, elem_map_b])

	// If not equal, return false
	map_loop_inc := b.mod.add_block(func_idx, 'map_loop_inc')
	b.mod.add_instr(.br, map_loop_body, 0,
		[map_eq, b.mod.blocks[map_loop_inc].val_id, b.mod.blocks[ret_false_block].val_id])

	// Loop increment: i++
	mnext_i := b.mod.add_instr(.add, map_loop_inc, i32_t, [mcur_i2, one_i32])
	b.mod.add_instr(.store, map_loop_inc, 0, [mnext_i, alloca_mi])
	b.mod.add_instr(.br, map_loop_inc, 0,
		[one_i1, b.mod.blocks[map_loop_cond].val_id, b.mod.blocks[map_loop_cond].val_id])

	// --- memcmp fallback block ---
	ld_a := b.mod.add_instr(.load, memcmp_block, ptr_t, [alloca_data_a])
	ld_b := b.mod.add_instr(.load, memcmp_block, ptr_t, [alloca_data_b])
	ld_esz := b.mod.add_instr(.load, memcmp_block, i32_t, [alloca_esz])
	ld_len := b.mod.add_instr(.load, memcmp_block, i32_t, [alloca_len])
	total_size := b.mod.add_instr(.mul, memcmp_block, i32_t, [ld_len, ld_esz])

	// Call C.memcmp(data_a, data_b, total_size)
	memcmp_ref := b.get_or_create_fn_ref('memcmp', i32_t)
	cmp_result := b.mod.add_instr(.call, memcmp_block, i32_t, [memcmp_ref, ld_a, ld_b, total_size])

	// Return cmp_result == 0
	is_eq := b.mod.add_instr(.eq, memcmp_block, i1_t, [cmp_result, zero_i32])
	b.mod.add_instr(.ret, memcmp_block, 0, [is_eq])
}

fn parse_map_eq_value_type_name(name string) string {
	if !name.starts_with('Map_') || !name.ends_with('_map_eq') {
		return ''
	}
	inner := name['Map_'.len..name.len - '_map_eq'.len]
	_, value_type := parse_map_kv_type_names(inner)
	return value_type
}

fn parse_map_kv_type_names(kv_str string) (string, string) {
	simple_types := ['string', 'int', 'i8', 'i16', 'i32', 'i64', 'u8', 'u16', 'u32', 'u64', 'f32',
		'f64', 'bool', 'rune', 'voidptr', 'charptr', 'byteptr']
	for st in simple_types {
		prefix := '${st}_'
		if kv_str.starts_with(prefix) {
			return st, kv_str[prefix.len..]
		}
	}
	last_idx := kv_str.last_index('_') or { return '', '' }
	if last_idx > 0 && last_idx < kv_str.len - 1 {
		return kv_str[..last_idx], kv_str[last_idx + 1..]
	}
	return '', ''
}

fn (b &Builder) struct_field_index(type_id TypeID, name string, fallback int) int {
	if type_id > 0 && type_id < b.mod.type_store.types.len {
		typ := b.mod.type_store.types[type_id]
		for i, field_name in typ.field_names {
			if field_name == name {
				return i
			}
		}
	}
	return fallback
}

fn (mut b Builder) generate_map_eq_stub(name string) {
	if name in b.fn_index {
		return
	}
	value_type_name := parse_map_eq_value_type_name(name)
	if value_type_name == '' {
		return
	}
	map_t := b.struct_types['map'] or { return }
	map_info := b.mod.type_store.types[map_t]
	i1_t := b.mod.type_store.get_int(1)
	i32_t := b.mod.type_store.get_int(32)
	ptr_t := b.mod.type_store.get_ptr(b.mod.type_store.get_int(8))

	func_idx := b.mod.new_function(name, i1_t, []TypeID{})
	b.fn_index[name] = func_idx
	entry := b.mod.add_block(func_idx, 'entry')
	ret_false_block := b.mod.add_block(func_idx, 'ret_false')
	ret_true_block := b.mod.add_block(func_idx, 'ret_true')
	loop_init_block := b.mod.add_block(func_idx, 'loop_init')
	loop_cond_block := b.mod.add_block(func_idx, 'loop_cond')
	loop_body_block := b.mod.add_block(func_idx, 'loop_body')
	loop_has_block := b.mod.add_block(func_idx, 'loop_has')
	loop_compare_block := b.mod.add_block(func_idx, 'loop_compare')
	loop_inc_block := b.mod.add_block(func_idx, 'loop_inc')

	param_a := b.mod.add_value_node(.argument, map_t, 'a', 0)
	param_b := b.mod.add_value_node(.argument, map_t, 'b', 0)
	b.mod.func_add_param(func_idx, param_a)
	b.mod.func_add_param(func_idx, param_b)

	alloca_a := b.mod.add_instr(.alloca, entry, b.mod.type_store.get_ptr(map_t), []ValueID{})
	b.mod.add_instr(.store, entry, 0, [param_a, alloca_a])
	alloca_b := b.mod.add_instr(.alloca, entry, b.mod.type_store.get_ptr(map_t), []ValueID{})
	b.mod.add_instr(.store, entry, 0, [param_b, alloca_b])

	zero_i1 := b.mod.get_or_add_const(i1_t, '0')
	one_i1 := b.mod.get_or_add_const(i1_t, '1')
	b.mod.add_instr(.ret, ret_false_block, 0, [zero_i1])
	b.mod.add_instr(.ret, ret_true_block, 0, [one_i1])

	len_idx := b.struct_field_index(map_t, 'len', 13)
	len_typ := if len_idx >= 0 && len_idx < map_info.fields.len {
		map_info.fields[len_idx]
	} else {
		i32_t
	}
	key_values_idx := b.struct_field_index(map_t, 'key_values', 5)
	key_values_typ := if key_values_idx >= 0 && key_values_idx < map_info.fields.len {
		map_info.fields[key_values_idx]
	} else {
		0
	}
	value_bytes_idx := b.struct_field_index(map_t, 'value_bytes', 1)
	value_bytes_typ := if value_bytes_idx >= 0 && value_bytes_idx < map_info.fields.len {
		map_info.fields[value_bytes_idx]
	} else {
		i32_t
	}
	if key_values_typ == 0 {
		return
	}
	key_values_info := b.mod.type_store.types[key_values_typ]
	kv_len_idx := b.struct_field_index(key_values_typ, 'len', 3)
	kv_len_typ := if kv_len_idx >= 0 && kv_len_idx < key_values_info.fields.len {
		key_values_info.fields[kv_len_idx]
	} else {
		i32_t
	}

	map_a0 := b.mod.add_instr(.load, entry, map_t, [alloca_a])
	map_b0 := b.mod.add_instr(.load, entry, map_t, [alloca_b])
	idx_len := b.mod.get_or_add_const(i32_t, '${len_idx}')
	len_a := b.mod.add_instr(.extractvalue, entry, len_typ, [map_a0, idx_len])
	len_b := b.mod.add_instr(.extractvalue, entry, len_typ, [map_b0, idx_len])
	len_eq := b.mod.add_instr(.eq, entry, i1_t, [len_a, len_b])

	idx_kv := b.mod.get_or_add_const(i32_t, '${key_values_idx}')
	kv_a := b.mod.add_instr(.extractvalue, entry, key_values_typ, [map_a0, idx_kv])
	alloca_kv_a := b.mod.add_instr(.alloca, entry, b.mod.type_store.get_ptr(key_values_typ),
		[]ValueID{})
	b.mod.add_instr(.store, entry, 0, [kv_a, alloca_kv_a])

	idx_value_bytes := b.mod.get_or_add_const(i32_t, '${value_bytes_idx}')
	value_bytes := b.mod.add_instr(.extractvalue, entry, value_bytes_typ, [map_a0, idx_value_bytes])
	b.mod.add_instr(.br, entry, 0,
		[len_eq, b.mod.blocks[loop_init_block].val_id, b.mod.blocks[ret_false_block].val_id])

	alloca_i := b.mod.add_instr(.alloca, loop_init_block, b.mod.type_store.get_ptr(kv_len_typ),
		[]ValueID{})
	zero_i := b.mod.get_or_add_const(kv_len_typ, '0')
	b.mod.add_instr(.store, loop_init_block, 0, [zero_i, alloca_i])
	b.mod.add_instr(.br, loop_init_block, 0,
		[one_i1, b.mod.blocks[loop_cond_block].val_id, b.mod.blocks[loop_cond_block].val_id])

	cur_i := b.mod.add_instr(.load, loop_cond_block, kv_len_typ, [alloca_i])
	idx_kv_len := b.mod.get_or_add_const(i32_t, '${kv_len_idx}')
	kv_len := b.mod.add_instr(.extractvalue, loop_cond_block, kv_len_typ, [kv_a, idx_kv_len])
	i_lt_len := b.mod.add_instr(.lt, loop_cond_block, i1_t, [cur_i, kv_len])
	b.mod.add_instr(.br, loop_cond_block, 0,
		[i_lt_len, b.mod.blocks[loop_body_block].val_id, b.mod.blocks[ret_true_block].val_id])

	i_body := b.mod.add_instr(.load, loop_body_block, kv_len_typ, [alloca_i])
	has_index_ref := b.get_or_create_fn_ref('builtin__DenseArray__has_index', i1_t)
	has_index := b.mod.add_instr(.call, loop_body_block, i1_t, [has_index_ref, alloca_kv_a, i_body])
	b.mod.add_instr(.br, loop_body_block, 0,
		[has_index, b.mod.blocks[loop_has_block].val_id, b.mod.blocks[loop_inc_block].val_id])

	i_has := b.mod.add_instr(.load, loop_has_block, kv_len_typ, [alloca_i])
	key_ref := b.get_or_create_fn_ref('builtin__DenseArray__key', ptr_t)
	key := b.mod.add_instr(.call, loop_has_block, ptr_t, [key_ref, alloca_kv_a, i_has])
	exists_ref := b.get_or_create_fn_ref('builtin__map__exists', i1_t)
	exists := b.mod.add_instr(.call, loop_has_block, i1_t, [exists_ref, alloca_b, key])
	b.mod.add_instr(.br, loop_has_block, 0,
		[exists, b.mod.blocks[loop_compare_block].val_id, b.mod.blocks[ret_false_block].val_id])

	i_cmp := b.mod.add_instr(.load, loop_compare_block, kv_len_typ, [alloca_i])
	value_ref := b.get_or_create_fn_ref('builtin__DenseArray__value', ptr_t)
	va := b.mod.add_instr(.call, loop_compare_block, ptr_t, [value_ref, alloca_kv_a, i_cmp])
	get_ref := b.get_or_create_fn_ref('builtin__map__get', ptr_t)
	vb := b.mod.add_instr(.call, loop_compare_block, ptr_t, [get_ref, alloca_b, key, va])
	b.emit_map_value_name_eq_branch(func_idx, loop_compare_block, va, vb, value_type_name,
		value_bytes, loop_inc_block, ret_false_block)

	i_inc := b.mod.add_instr(.load, loop_inc_block, kv_len_typ, [alloca_i])
	one_i := b.mod.get_or_add_const(kv_len_typ, '1')
	next_i := b.mod.add_instr(.add, loop_inc_block, kv_len_typ, [i_inc, one_i])
	b.mod.add_instr(.store, loop_inc_block, 0, [next_i, alloca_i])
	b.mod.add_instr(.br, loop_inc_block, 0,
		[one_i1, b.mod.blocks[loop_cond_block].val_id, b.mod.blocks[loop_cond_block].val_id])
}

fn (mut b Builder) emit_map_value_name_eq_branch(func_idx int, block BlockID, va_ptr ValueID, vb_ptr ValueID, value_type_name string, value_size ValueID, true_block BlockID, false_block BlockID) {
	if value_type_name == 'string' {
		b.emit_ptr_string_eq_branch(block, va_ptr, vb_ptr, true_block, false_block)
		return
	}
	if value_type_name.starts_with('Array_') && !value_type_name.starts_with('Array_fixed_') {
		b.emit_ptr_array_eq_branch(block, va_ptr, vb_ptr, true_block, false_block)
		return
	}
	if value_type_name.starts_with('Map_') {
		b.emit_ptr_map_eq_branch(block, va_ptr, vb_ptr, value_type_name, true_block, false_block)
		return
	}
	if st := b.lookup_checked_struct_type_by_c_name(value_type_name) {
		b.emit_ptr_checked_value_eq_branch(func_idx, block, va_ptr, vb_ptr, types.Type(st),
			true_block, false_block)
		return
	}
	b.emit_memcmp_ptr_eq_branch(block, va_ptr, vb_ptr, value_size, true_block, false_block)
}

fn (mut b Builder) emit_ptr_string_eq_branch(block BlockID, lhs_ptr ValueID, rhs_ptr ValueID, true_block BlockID, false_block BlockID) {
	str_t := b.get_string_type()
	str_ptr_t := b.mod.type_store.get_ptr(str_t)
	lhs_typed := b.mod.add_instr(.bitcast, block, str_ptr_t, [lhs_ptr])
	rhs_typed := b.mod.add_instr(.bitcast, block, str_ptr_t, [rhs_ptr])
	lhs := b.mod.add_instr(.load, block, str_t, [lhs_typed])
	rhs := b.mod.add_instr(.load, block, str_t, [rhs_typed])
	b.emit_string_value_eq_branch(block, lhs, rhs, true_block, false_block)
}

fn (mut b Builder) emit_string_value_eq_branch(block BlockID, lhs ValueID, rhs ValueID, true_block BlockID, false_block BlockID) {
	i1_t := b.mod.type_store.get_int(1)
	eq_ref := b.get_or_create_fn_ref('builtin__string__==', i1_t)
	eq := b.mod.add_instr(.call, block, i1_t, [eq_ref, lhs, rhs])
	b.mod.add_instr(.br, block, 0,
		[eq, b.mod.blocks[true_block].val_id, b.mod.blocks[false_block].val_id])
}

fn (mut b Builder) emit_ptr_array_eq_branch(block BlockID, lhs_ptr ValueID, rhs_ptr ValueID, true_block BlockID, false_block BlockID) {
	array_t := b.get_array_type()
	array_ptr_t := b.mod.type_store.get_ptr(array_t)
	lhs_typed := b.mod.add_instr(.bitcast, block, array_ptr_t, [lhs_ptr])
	rhs_typed := b.mod.add_instr(.bitcast, block, array_ptr_t, [rhs_ptr])
	lhs := b.mod.add_instr(.load, block, array_t, [lhs_typed])
	rhs := b.mod.add_instr(.load, block, array_t, [rhs_typed])
	b.emit_array_value_eq_branch(block, lhs, rhs, true_block, false_block)
}

fn (mut b Builder) emit_array_value_eq_branch(block BlockID, lhs ValueID, rhs ValueID, true_block BlockID, false_block BlockID) {
	i1_t := b.mod.type_store.get_int(1)
	eq_ref := b.get_or_create_fn_ref('array__eq', i1_t)
	eq := b.mod.add_instr(.call, block, i1_t, [eq_ref, lhs, rhs])
	b.mod.add_instr(.br, block, 0,
		[eq, b.mod.blocks[true_block].val_id, b.mod.blocks[false_block].val_id])
}

fn (mut b Builder) emit_ptr_map_eq_branch(block BlockID, lhs_ptr ValueID, rhs_ptr ValueID, map_type_name string, true_block BlockID, false_block BlockID) {
	map_t := b.struct_types['map'] or { return }
	map_ptr_t := b.mod.type_store.get_ptr(map_t)
	lhs_typed := b.mod.add_instr(.bitcast, block, map_ptr_t, [lhs_ptr])
	rhs_typed := b.mod.add_instr(.bitcast, block, map_ptr_t, [rhs_ptr])
	lhs := b.mod.add_instr(.load, block, map_t, [lhs_typed])
	rhs := b.mod.add_instr(.load, block, map_t, [rhs_typed])
	b.emit_map_value_eq_branch(block, lhs, rhs, map_type_name, true_block, false_block)
}

fn (mut b Builder) emit_map_value_eq_branch(block BlockID, lhs ValueID, rhs ValueID, map_type_name string, true_block BlockID, false_block BlockID) {
	i1_t := b.mod.type_store.get_int(1)
	eq_ref := b.get_or_create_fn_ref('${map_type_name}_map_eq', i1_t)
	eq := b.mod.add_instr(.call, block, i1_t, [eq_ref, lhs, rhs])
	b.mod.add_instr(.br, block, 0,
		[eq, b.mod.blocks[true_block].val_id, b.mod.blocks[false_block].val_id])
}

fn (mut b Builder) emit_memcmp_ptr_eq_branch(block BlockID, lhs_ptr ValueID, rhs_ptr ValueID, size ValueID, true_block BlockID, false_block BlockID) {
	i1_t := b.mod.type_store.get_int(1)
	i32_t := b.mod.type_store.get_int(32)
	memcmp_ref := b.get_or_create_fn_ref('memcmp', i32_t)
	cmp := b.mod.add_instr(.call, block, i32_t, [memcmp_ref, lhs_ptr, rhs_ptr, size])
	zero := b.mod.get_or_add_const(i32_t, '0')
	eq := b.mod.add_instr(.eq, block, i1_t, [cmp, zero])
	b.mod.add_instr(.br, block, 0,
		[eq, b.mod.blocks[true_block].val_id, b.mod.blocks[false_block].val_id])
}

fn (mut b Builder) emit_ptr_checked_value_eq_branch(func_idx int, block BlockID, lhs_ptr ValueID, rhs_ptr ValueID, typ types.Type, true_block BlockID, false_block BlockID) {
	if !types.type_has_valid_payload(typ) {
		size := b.mod.get_or_add_const(b.mod.type_store.get_int(32), '8')
		b.emit_memcmp_ptr_eq_branch(block, lhs_ptr, rhs_ptr, size, true_block, false_block)
		return
	}
	if typ is types.Alias {
		base := b.resolve_alias_base_type(typ) or {
			size := b.mod.get_or_add_const(b.mod.type_store.get_int(32),
				'${b.sizeof_from_checked_type(typ)}')
			b.emit_memcmp_ptr_eq_branch(block, lhs_ptr, rhs_ptr, size, true_block, false_block)
			return
		}
		b.emit_ptr_checked_value_eq_branch(func_idx, block, lhs_ptr, rhs_ptr, base, true_block,
			false_block)
		return
	}
	if typ is types.ArrayFixed {
		size := b.mod.get_or_add_const(b.mod.type_store.get_int(32),
			'${b.sizeof_from_checked_type(typ)}')
		b.emit_memcmp_ptr_eq_branch(block, lhs_ptr, rhs_ptr, size, true_block, false_block)
		return
	}
	typ_id := b.type_to_ssa(typ)
	if typ_id <= 0 || typ_id >= b.mod.type_store.types.len {
		size := b.mod.get_or_add_const(b.mod.type_store.get_int(32),
			'${b.sizeof_from_checked_type(typ)}')
		b.emit_memcmp_ptr_eq_branch(block, lhs_ptr, rhs_ptr, size, true_block, false_block)
		return
	}
	ptr_typ := b.mod.type_store.get_ptr(typ_id)
	lhs_typed := b.mod.add_instr(.bitcast, block, ptr_typ, [lhs_ptr])
	rhs_typed := b.mod.add_instr(.bitcast, block, ptr_typ, [rhs_ptr])
	lhs := b.mod.add_instr(.load, block, typ_id, [lhs_typed])
	rhs := b.mod.add_instr(.load, block, typ_id, [rhs_typed])
	b.emit_checked_value_eq_branch(func_idx, block, lhs, rhs, typ, true_block, false_block)
}

fn (mut b Builder) emit_ssa_value_eq_branch(block BlockID, lhs ValueID, rhs ValueID, true_block BlockID, false_block BlockID) {
	i1_t := b.mod.type_store.get_int(1)
	if lhs > 0 && lhs < b.mod.values.len {
		typ_id := b.mod.values[lhs].typ
		if typ_id > 0 && typ_id < b.mod.type_store.types.len {
			ssa_typ := b.mod.type_store.types[typ_id]
			if ssa_typ.kind in [.int_t, .float_t, .ptr_t] {
				eq := b.mod.add_instr(.eq, block, i1_t, [lhs, rhs])
				b.mod.add_instr(.br, block, 0,
					[eq, b.mod.blocks[true_block].val_id, b.mod.blocks[false_block].val_id])
				return
			}
			if ssa_typ.kind in [.struct_t, .array_t] {
				ptr_typ := b.mod.type_store.get_ptr(typ_id)
				lhs_tmp := b.mod.add_instr(.alloca, block, ptr_typ, []ValueID{})
				rhs_tmp := b.mod.add_instr(.alloca, block, ptr_typ, []ValueID{})
				b.mod.add_instr(.store, block, 0, [lhs, lhs_tmp])
				b.mod.add_instr(.store, block, 0, [rhs, rhs_tmp])
				size := b.mod.get_or_add_const(b.mod.type_store.get_int(32),
					'${b.type_byte_size(typ_id)}')
				b.emit_memcmp_ptr_eq_branch(block, lhs_tmp, rhs_tmp, size, true_block, false_block)
				return
			}
		}
	}
	zero := b.mod.get_or_add_const(i1_t, '0')
	b.mod.add_instr(.br, block, 0,
		[zero, b.mod.blocks[false_block].val_id, b.mod.blocks[false_block].val_id])
}

fn (mut b Builder) emit_checked_value_eq_branch(func_idx int, block BlockID, lhs ValueID, rhs ValueID, typ types.Type, true_block BlockID, false_block BlockID) {
	if !types.type_has_valid_payload(typ) {
		b.emit_ssa_value_eq_branch(block, lhs, rhs, true_block, false_block)
		return
	}
	match typ {
		types.Alias {
			base := b.resolve_alias_base_type(typ) or {
				b.emit_ssa_value_eq_branch(block, lhs, rhs, true_block, false_block)
				return
			}
			b.emit_checked_value_eq_branch(func_idx, block, lhs, rhs, base, true_block, false_block)
		}
		types.String {
			b.emit_string_value_eq_branch(block, lhs, rhs, true_block, false_block)
		}
		types.Array {
			b.emit_array_value_eq_branch(block, lhs, rhs, true_block, false_block)
		}
		types.Map {
			map_type_name := b.checked_type_c_name(types.Type(typ))
			if map_type_name.starts_with('Map_') {
				b.emit_map_value_eq_branch(block, lhs, rhs, map_type_name, true_block, false_block)
			} else {
				i1_t := b.mod.type_store.get_int(1)
				eq_ref := b.get_or_create_fn_ref('builtin__map_map_eq', i1_t)
				eq := b.mod.add_instr(.call, block, i1_t, [eq_ref, lhs, rhs])
				b.mod.add_instr(.br, block, 0,
					[eq, b.mod.blocks[true_block].val_id, b.mod.blocks[false_block].val_id])
			}
		}
		types.Struct {
			b.emit_struct_value_eq_branch(func_idx, block, lhs, rhs, typ, true_block, false_block)
		}
		types.ArrayFixed {
			typ_id := b.type_to_ssa(types.Type(typ))
			ptr_typ := b.mod.type_store.get_ptr(typ_id)
			lhs_tmp := b.mod.add_instr(.alloca, block, ptr_typ, []ValueID{})
			rhs_tmp := b.mod.add_instr(.alloca, block, ptr_typ, []ValueID{})
			b.mod.add_instr(.store, block, 0, [lhs, lhs_tmp])
			b.mod.add_instr(.store, block, 0, [rhs, rhs_tmp])
			size := b.mod.get_or_add_const(b.mod.type_store.get_int(32),
				'${b.sizeof_from_checked_type(typ)}')
			b.emit_memcmp_ptr_eq_branch(block, lhs_tmp, rhs_tmp, size, true_block, false_block)
		}
		else {
			typ_id := b.type_to_ssa(typ)
			i1_t := b.mod.type_store.get_int(1)
			if typ_id > 0 && typ_id < b.mod.type_store.types.len {
				eq := b.mod.add_instr(.eq, block, i1_t, [lhs, rhs])
				b.mod.add_instr(.br, block, 0,
					[eq, b.mod.blocks[true_block].val_id, b.mod.blocks[false_block].val_id])
			} else {
				zero := b.mod.get_or_add_const(i1_t, '0')
				b.mod.add_instr(.br, block, 0,
					[zero, b.mod.blocks[false_block].val_id, b.mod.blocks[false_block].val_id])
			}
		}
	}
}

fn (mut b Builder) emit_struct_value_eq_branch(func_idx int, block BlockID, lhs ValueID, rhs ValueID, st types.Struct, true_block BlockID, false_block BlockID) {
	st_id := b.type_to_ssa(types.Type(st))
	if st_id <= 0 || st_id >= b.mod.type_store.types.len {
		b.mod.add_instr(.br, block, 0, [
			b.mod.get_or_add_const(b.mod.type_store.get_int(1), '0'),
			b.mod.blocks[false_block].val_id,
			b.mod.blocks[false_block].val_id,
		])
		return
	}
	st_info := b.mod.type_store.types[st_id]
	if st.fields.len == 0 || st_info.fields.len == 0 {
		b.mod.add_instr(.br, block, 0, [
			b.mod.get_or_add_const(b.mod.type_store.get_int(1), '1'),
			b.mod.blocks[true_block].val_id,
			b.mod.blocks[true_block].val_id,
		])
		return
	}
	i32_t := b.mod.type_store.get_int(32)
	mut cur_block := block
	for i in 0 .. st.fields.len {
		if i >= st_info.fields.len {
			break
		}
		next_block := b.mod.add_block(func_idx, 'struct_eq_next')
		idx := b.mod.get_or_add_const(i32_t, '${i}')
		field_typ := st_info.fields[i]
		lhs_field := b.mod.add_instr(.extractvalue, cur_block, field_typ, [lhs, idx])
		rhs_field := b.mod.add_instr(.extractvalue, cur_block, field_typ, [rhs, idx])
		b.emit_checked_value_eq_branch(func_idx, cur_block, lhs_field, rhs_field, st.fields[i].typ,
			next_block, false_block)
		cur_block = next_block
	}
	b.mod.add_instr(.br, cur_block, 0, [
		b.mod.get_or_add_const(b.mod.type_store.get_int(1), '1'),
		b.mod.blocks[true_block].val_id,
		b.mod.blocks[true_block].val_id,
	])
}

fn (b &Builder) checked_type_c_name(typ types.Type) string {
	if !types.type_has_valid_payload(typ) {
		return ''
	}
	match typ {
		types.Alias {
			if typ.name != '' {
				return typ.name
			}
			base := b.resolve_alias_base_type(typ) or { return '' }
			return b.checked_type_c_name(base)
		}
		types.Primitive {
			if typ.props.has(.boolean) {
				return 'bool'
			}
			if typ.props.has(.float) {
				return if typ.size == 32 { 'f32' } else { 'f64' }
			}
			if typ.props.has(.unsigned) {
				return match typ.size {
					8 { 'u8' }
					16 { 'u16' }
					32 { 'u32' }
					64 { 'u64' }
					else { 'int' }
				}
			}
			return match typ.size {
				8 { 'i8' }
				16 { 'i16' }
				32, 0 { 'int' }
				64 { 'i64' }
				else { 'int' }
			}
		}
		types.String {
			return 'string'
		}
		types.Char {
			return 'char'
		}
		types.Rune {
			return 'rune'
		}
		types.Pointer {
			base_name := b.checked_type_c_name(typ.base_type)
			if base_name == 'char' {
				return 'charptr'
			}
			if base_name == 'u8' {
				return 'byteptr'
			}
			return '${base_name}ptr'
		}
		types.Array {
			return 'Array_${b.checked_type_c_name(typ.elem_type)}'
		}
		types.ArrayFixed {
			return 'Array_fixed_${b.checked_type_c_name(typ.elem_type)}_${typ.len}'
		}
		types.Map {
			return 'Map_${b.checked_type_c_name(typ.key_type)}_${b.checked_type_c_name(typ.value_type)}'
		}
		types.Struct {
			return typ.name
		}
		types.Enum {
			return typ.name
		}
		else {
			return typ.name()
		}
	}
}

fn (b &Builder) lookup_checked_struct_type_by_c_name(c_name string) ?types.Struct {
	if b.env == unsafe { nil } {
		return none
	}
	mut mod_name := ''
	mut struct_name := c_name
	if idx := c_name.index('__') {
		mod_name = c_name[..idx]
		struct_name = c_name[idx + 2..]
	}
	if mod_name != '' {
		if scope := b.env.get_scope(mod_name) {
			if obj := scope.lookup_parent(struct_name, 0) {
				typ := obj.typ()
				if typ is types.Struct {
					return typ
				}
			}
		}
	}
	for try_mod in [b.cur_module, 'main', 'builtin'] {
		if try_mod == '' {
			continue
		}
		if scope := b.env.get_scope(try_mod) {
			if obj := scope.lookup_parent(struct_name, 0) {
				typ := obj.typ()
				if typ is types.Struct {
					return typ
				}
			}
		}
	}
	scopes := b.env.snapshot_scopes()
	for _, scope in scopes {
		if obj := scope.lookup_parent(struct_name, 0) {
			typ := obj.typ()
			if typ is types.Struct {
				return typ
			}
		}
	}
	return none
}

fn (mut b Builder) generate_string_plus_two_stub() {
	fn_name := 'builtin__string__plus_two'
	if fn_name in b.fn_index {
		fn_idx := b.fn_index[fn_name]
		b.mod.func_set_c_extern(fn_idx, false)
		b.generate_string_plus_two_body(fn_idx)
		return
	}
	string_t := b.get_string_type()
	if string_t == 0 {
		return
	}
	func_idx := b.mod.new_function(fn_name, string_t, []TypeID{})
	b.fn_index[fn_name] = func_idx
	b.generate_string_plus_two_body(func_idx)
}

fn (mut b Builder) generate_string_plus_two_body(func_idx int) {
	string_t := b.get_string_type()
	if string_t == 0 {
		return
	}
	b.mod.func_clear_blocks(func_idx)
	b.mod.func_clear_params(func_idx)

	entry := b.mod.add_block(func_idx, 'entry')
	param_a := b.mod.add_value_node(.argument, string_t, 'a', 0)
	param_b := b.mod.add_value_node(.argument, string_t, 'b', 0)
	param_c := b.mod.add_value_node(.argument, string_t, 'c', 0)
	b.mod.func_add_param(func_idx, param_a)
	b.mod.func_add_param(func_idx, param_b)
	b.mod.func_add_param(func_idx, param_c)

	plus_fn := b.get_or_create_fn_ref('builtin__string__+', string_t)
	ab := b.mod.add_instr(.call, entry, string_t, [plus_fn, param_a, param_b])
	abc := b.mod.add_instr(.call, entry, string_t, [plus_fn, ab, param_c])
	b.mod.add_instr(.ret, entry, 0, [abc])
}

// generate_wymix_stub creates a synthetic `_wymix` function for the native backend.
// C._wymix is defined in wyhash.h (C only), and rand.wyrand uses it directly.
fn (mut b Builder) generate_wymix_stub() {
	if '_wymix' in b.fn_index {
		fn_idx := b.fn_index['_wymix']
		b.mod.func_set_c_extern(fn_idx, false)
		b.generate_wymix_body(fn_idx)
		return
	}
	i64_t := b.mod.type_store.get_int(64)
	func_idx := b.mod.new_function('_wymix', i64_t, []TypeID{})
	b.fn_index['_wymix'] = func_idx
	b.generate_wymix_body(func_idx)
}

fn (mut b Builder) generate_wymix_body(func_idx int) {
	i64_t := b.mod.type_store.get_int(64)
	b.mod.func_clear_blocks(func_idx)
	entry := b.mod.add_block(func_idx, 'entry')

	param_a := b.mod.add_value_node(.argument, i64_t, 'a', 0)
	param_b := b.mod.add_value_node(.argument, i64_t, 'b', 0)
	b.mod.func_clear_params(func_idx)
	b.mod.func_add_param(func_idx, param_a)
	b.mod.func_add_param(func_idx, param_b)

	result := b.wymix_inline(entry, param_a, param_b)
	b.mod.add_instr(.ret, entry, 0, [result])
}

// generate_wyhash64_stub creates a synthetic `wyhash64` function for the native backend.
// C.wyhash64 is defined in wyhash.h (C only), so the native backend needs a V implementation.
// wyhash64(a, b) = wymum(a ^ wyp0, b ^ wyp1)
// This calls hash__wymum which is the pure-V implementation.
fn (mut b Builder) generate_wyhash64_stub() {
	// Replace the empty stub created by the linker for C.wyhash64
	if 'wyhash64' in b.fn_index {
		// Already registered from .c.v declaration, need to replace body
		fn_idx := b.fn_index['wyhash64']
		b.mod.func_set_c_extern(fn_idx, false) // Override: we provide the body
		b.generate_wyhash64_body(fn_idx)
		return
	}
	i64_t := b.mod.type_store.get_int(64)
	func_idx := b.mod.new_function('wyhash64', i64_t, []TypeID{})
	b.fn_index['wyhash64'] = func_idx
	b.generate_wyhash64_body(func_idx)
}

fn (mut b Builder) generate_wyhash64_body(func_idx int) {
	i64_t := b.mod.type_store.get_int(64)

	// Clear existing blocks if any
	b.mod.func_clear_blocks(func_idx)

	entry := b.mod.add_block(func_idx, 'entry')

	// Parameters: a: u64, b: u64
	param_a := b.mod.add_value_node(.argument, i64_t, 'a', 0)
	param_b := b.mod.add_value_node(.argument, i64_t, 'b', 0)
	b.mod.func_clear_params(func_idx)
	b.mod.func_add_param(func_idx, param_a)
	b.mod.func_add_param(func_idx, param_b)

	// wyp0 = 0x2d358dccaa6c78a5
	// wyp1 = 0x8bb84b93962eacc9
	wyp0 := b.mod.get_or_add_const(i64_t, '3257665815644502181')
	wyp1 := b.mod.get_or_add_const(i64_t, '10067880064238660809')

	// x = a ^ wyp0
	x := b.mod.add_instr(.xor, entry, i64_t, [param_a, wyp0])
	// y = b ^ wyp1
	y := b.mod.add_instr(.xor, entry, i64_t, [param_b, wyp1])

	// Implement wymum(x, y) inline:
	// wymum returns hi(x*y) ^ lo(x*y) using 32-bit multiply approach
	// lo = x * y (truncated to 64 bits)
	lo := b.mod.add_instr(.mul, entry, i64_t, [x, y])

	// For hi: use the 32-bit decomposition
	// mask32 = 0xFFFFFFFF
	mask32 := b.mod.get_or_add_const(i64_t, '4294967295')
	c32 := b.mod.get_or_add_const(i64_t, '32')

	// x0 = x & mask32
	x0 := b.mod.add_instr(.and_, entry, i64_t, [x, mask32])
	// x1 = x >> 32
	x1 := b.mod.add_instr(.lshr, entry, i64_t, [x, c32])
	// y0 = y & mask32
	y0 := b.mod.add_instr(.and_, entry, i64_t, [y, mask32])
	// y1 = y >> 32
	y1 := b.mod.add_instr(.lshr, entry, i64_t, [y, c32])

	// w0 = x0 * y0
	w0 := b.mod.add_instr(.mul, entry, i64_t, [x0, y0])
	// t = x1 * y0 + (w0 >> 32)
	x1y0 := b.mod.add_instr(.mul, entry, i64_t, [x1, y0])
	w0_hi := b.mod.add_instr(.lshr, entry, i64_t, [w0, c32])
	t := b.mod.add_instr(.add, entry, i64_t, [x1y0, w0_hi])
	// w1 = (t & mask32) + x0 * y1
	t_lo := b.mod.add_instr(.and_, entry, i64_t, [t, mask32])
	x0y1 := b.mod.add_instr(.mul, entry, i64_t, [x0, y1])
	w1 := b.mod.add_instr(.add, entry, i64_t, [t_lo, x0y1])
	// w2 = t >> 32
	w2 := b.mod.add_instr(.lshr, entry, i64_t, [t, c32])
	// hi = x1 * y1 + w2 + (w1 >> 32)
	x1y1 := b.mod.add_instr(.mul, entry, i64_t, [x1, y1])
	w1_hi := b.mod.add_instr(.lshr, entry, i64_t, [w1, c32])
	hi_tmp := b.mod.add_instr(.add, entry, i64_t, [x1y1, w2])
	hi := b.mod.add_instr(.add, entry, i64_t, [hi_tmp, w1_hi])

	// result = hi ^ lo
	result := b.mod.add_instr(.xor, entry, i64_t, [hi, lo])
	b.mod.add_instr(.ret, entry, 0, [result])
}

// generate_wyhash_stub creates a synthetic `wyhash` function for the native backend.
// C.wyhash(key, len, seed, secret) is defined in wyhash.h (C only).
// The native backend needs a V implementation.
// This implements the wyhash v4 algorithm for strings up to 16 bytes (most common map keys)
// and a simplified fallback for longer strings.
fn (mut b Builder) generate_wyhash_stub() {
	// Replace the empty stub created by the linker for C.wyhash
	if 'wyhash' in b.fn_index {
		fn_idx := b.fn_index['wyhash']
		b.mod.func_set_c_extern(fn_idx, false) // Override: we provide the body
		b.generate_wyhash_body(fn_idx)
		return
	}
	i64_t := b.mod.type_store.get_int(64)
	func_idx := b.mod.new_function('wyhash', i64_t, []TypeID{})
	b.fn_index['wyhash'] = func_idx
	b.generate_wyhash_body(func_idx)
}

// Helper: add inline wymum_pair instructions that produce both lo and hi of 128-bit multiply.
// Returns (lo_val, hi_val).
fn (mut b Builder) wymum_pair_inline(block_id int, a ValueID, b_val ValueID) (ValueID, ValueID) {
	i64_t := b.mod.type_store.get_int(64)
	mask32 := b.mod.get_or_add_const(i64_t, '4294967295')
	c32 := b.mod.get_or_add_const(i64_t, '32')

	// lo = a * b (truncated to 64 bits)
	lo := b.mod.add_instr(.mul, block_id, i64_t, [a, b_val])

	// hi via 32-bit decomposition
	x0 := b.mod.add_instr(.and_, block_id, i64_t, [a, mask32])
	x1 := b.mod.add_instr(.lshr, block_id, i64_t, [a, c32])
	y0 := b.mod.add_instr(.and_, block_id, i64_t, [b_val, mask32])
	y1 := b.mod.add_instr(.lshr, block_id, i64_t, [b_val, c32])
	w0 := b.mod.add_instr(.mul, block_id, i64_t, [x0, y0])
	x1y0 := b.mod.add_instr(.mul, block_id, i64_t, [x1, y0])
	w0_hi := b.mod.add_instr(.lshr, block_id, i64_t, [w0, c32])
	t := b.mod.add_instr(.add, block_id, i64_t, [x1y0, w0_hi])
	t_lo := b.mod.add_instr(.and_, block_id, i64_t, [t, mask32])
	x0y1 := b.mod.add_instr(.mul, block_id, i64_t, [x0, y1])
	w1 := b.mod.add_instr(.add, block_id, i64_t, [t_lo, x0y1])
	w2 := b.mod.add_instr(.lshr, block_id, i64_t, [t, c32])
	x1y1 := b.mod.add_instr(.mul, block_id, i64_t, [x1, y1])
	w1_hi := b.mod.add_instr(.lshr, block_id, i64_t, [w1, c32])
	hi_tmp := b.mod.add_instr(.add, block_id, i64_t, [x1y1, w2])
	hi := b.mod.add_instr(.add, block_id, i64_t, [hi_tmp, w1_hi])

	return lo, hi
}

// Helper: add inline wymix instructions: wymix(a, b) = wymum(a, b) returns hi ^ lo
fn (mut b Builder) wymix_inline(block_id int, a ValueID, b_val ValueID) ValueID {
	i64_t := b.mod.type_store.get_int(64)
	lo, hi := b.wymum_pair_inline(block_id, a, b_val)
	return b.mod.add_instr(.xor, block_id, i64_t, [hi, lo])
}

fn (mut b Builder) generate_wyhash_body(func_idx int) {
	i8_t := b.mod.type_store.get_int(8)
	i32_t := b.mod.type_store.get_int(32)
	i64_t := b.mod.type_store.get_int(64)
	i1_t := b.mod.type_store.get_int(1)
	ptr_u8 := b.mod.type_store.get_ptr(i8_t)
	ptr_u32 := b.mod.type_store.get_ptr(i32_t)
	ptr_u64 := b.mod.type_store.get_ptr(i64_t)

	// Clear existing blocks if any
	b.mod.func_clear_blocks(func_idx)

	entry := b.mod.add_block(func_idx, 'entry')

	// Parameters: key: *u8, len: u64, seed: u64, secret: *u64
	param_key := b.mod.add_value_node(.argument, ptr_u8, 'key', 0)
	param_len := b.mod.add_value_node(.argument, i64_t, 'len', 0)
	param_seed := b.mod.add_value_node(.argument, i64_t, 'seed', 0)
	param_secret := b.mod.add_value_node(.argument, ptr_u64, 'secret', 0)
	b.mod.func_clear_params(func_idx)
	b.mod.func_add_param(func_idx, param_key)
	b.mod.func_add_param(func_idx, param_len)
	b.mod.func_add_param(func_idx, param_seed)
	b.mod.func_add_param(func_idx, param_secret)

	// Hardcoded wyp constants (same as wyhash.h _wyp[4])
	wyp0 := b.mod.get_or_add_const(i64_t, '3257665815644502181') // 0x2d358dccaa6c78a5
	wyp1 := b.mod.get_or_add_const(i64_t, '10067880064238660809') // 0x8bb84b93962eacc9

	// wyhash hardens zero-padded inputs by folding len into the initial seed mix.
	seed_xor_s0 := b.mod.add_instr(.xor, entry, i64_t, [param_seed, wyp0])
	seed_xor_s0_len := b.mod.add_instr(.xor, entry, i64_t, [seed_xor_s0, param_len])
	seed_mix := b.wymix_inline(entry, seed_xor_s0_len, wyp1)
	seed_init := b.mod.add_instr(.xor, entry, i64_t, [param_seed, seed_mix])

	// Allocas for a, b, and seed (results from branches, seed updated in long path)
	alloca_a := b.mod.add_instr(.alloca, entry, ptr_u64, []ValueID{})
	alloca_b := b.mod.add_instr(.alloca, entry, ptr_u64, []ValueID{})
	alloca_seed := b.mod.add_instr(.alloca, entry, ptr_u64, []ValueID{})

	// Constants
	zero_64 := b.mod.get_or_add_const(i64_t, '0')
	one_64 := b.mod.get_or_add_const(i64_t, '1')
	two_64 := b.mod.get_or_add_const(i64_t, '2')
	three_64 := b.mod.get_or_add_const(i64_t, '3')
	four_64 := b.mod.get_or_add_const(i64_t, '4')
	eight_64 := b.mod.get_or_add_const(i64_t, '8')
	sixteen_64 := b.mod.get_or_add_const(i64_t, '16')
	c32 := b.mod.get_or_add_const(i64_t, '32')

	// Store defaults
	b.mod.add_instr(.store, entry, 0, [zero_64, alloca_a])
	b.mod.add_instr(.store, entry, 0, [zero_64, alloca_b])
	b.mod.add_instr(.store, entry, 0, [seed_init, alloca_seed])

	// Branch: len <= 16?
	len_le_16 := b.mod.add_instr(.le, entry, i1_t, [param_len, sixteen_64])

	blk_short := b.mod.add_block(func_idx, 'short')
	blk_long := b.mod.add_block(func_idx, 'long')
	blk_final := b.mod.add_block(func_idx, 'final')

	b.mod.add_instr(.br, entry, 0,
		[len_le_16, b.mod.blocks[blk_short].val_id, b.mod.blocks[blk_long].val_id])

	// === SHORT PATH (len <= 16) ===
	// Branch: len >= 4?
	len_ge_4 := b.mod.add_instr(.ge, blk_short, i1_t, [param_len, four_64])

	blk_short_4_16 := b.mod.add_block(func_idx, 'short_4_16')
	blk_short_0_3 := b.mod.add_block(func_idx, 'short_0_3')

	b.mod.add_instr(.br, blk_short, 0,
		[len_ge_4, b.mod.blocks[blk_short_4_16].val_id, b.mod.blocks[blk_short_0_3].val_id])

	// --- SHORT 4-16 block ---
	// a = (_wyr4(p) << 32) | _wyr4(p + ((len>>3)<<2))
	// b = (_wyr4(p+len-4) << 32) | _wyr4(p+len-4-((len>>3)<<2))

	// _wyr4(p) = load u32 from p, zero-extended to u64
	p_as_u32 := b.mod.add_instr(.bitcast, blk_short_4_16, ptr_u32, [param_key])
	wyr4_0 := b.mod.add_instr(.load, blk_short_4_16, i32_t, [p_as_u32])
	wyr4_0_64 := b.mod.add_instr(.zext, blk_short_4_16, i64_t, [wyr4_0])

	// off1 = (len >> 3) << 2
	len_shr3 := b.mod.add_instr(.lshr, blk_short_4_16, i64_t, [param_len, three_64])
	off1 := b.mod.add_instr(.shl, blk_short_4_16, i64_t, [len_shr3, two_64])

	// _wyr4(p + off1)
	p_off1 := b.mod.add_instr(.add, blk_short_4_16, ptr_u8, [param_key, off1])
	p_off1_u32 := b.mod.add_instr(.bitcast, blk_short_4_16, ptr_u32, [p_off1])
	wyr4_1 := b.mod.add_instr(.load, blk_short_4_16, i32_t, [p_off1_u32])
	wyr4_1_64 := b.mod.add_instr(.zext, blk_short_4_16, i64_t, [wyr4_1])

	// a = (wyr4_0 << 32) | wyr4_1
	wyr4_0_shifted := b.mod.add_instr(.shl, blk_short_4_16, i64_t, [wyr4_0_64, c32])
	a_val := b.mod.add_instr(.or_, blk_short_4_16, i64_t, [wyr4_0_shifted, wyr4_1_64])

	// _wyr4(p + len - 4)
	len_m4 := b.mod.add_instr(.sub, blk_short_4_16, i64_t, [param_len, four_64])
	p_lm4 := b.mod.add_instr(.add, blk_short_4_16, ptr_u8, [param_key, len_m4])
	p_lm4_u32 := b.mod.add_instr(.bitcast, blk_short_4_16, ptr_u32, [p_lm4])
	wyr4_2 := b.mod.add_instr(.load, blk_short_4_16, i32_t, [p_lm4_u32])
	wyr4_2_64 := b.mod.add_instr(.zext, blk_short_4_16, i64_t, [wyr4_2])

	// _wyr4(p + len - 4 - off1)
	lm4_moff1 := b.mod.add_instr(.sub, blk_short_4_16, i64_t, [len_m4, off1])
	p_lm4_moff1 := b.mod.add_instr(.add, blk_short_4_16, ptr_u8, [param_key, lm4_moff1])
	p_lm4_moff1_u32 := b.mod.add_instr(.bitcast, blk_short_4_16, ptr_u32, [
		p_lm4_moff1,
	])
	wyr4_3 := b.mod.add_instr(.load, blk_short_4_16, i32_t, [p_lm4_moff1_u32])
	wyr4_3_64 := b.mod.add_instr(.zext, blk_short_4_16, i64_t, [wyr4_3])

	// b = (wyr4_2 << 32) | wyr4_3
	wyr4_2_shifted := b.mod.add_instr(.shl, blk_short_4_16, i64_t, [wyr4_2_64, c32])
	b_val := b.mod.add_instr(.or_, blk_short_4_16, i64_t, [wyr4_2_shifted, wyr4_3_64])

	b.mod.add_instr(.store, blk_short_4_16, 0, [a_val, alloca_a])
	b.mod.add_instr(.store, blk_short_4_16, 0, [b_val, alloca_b])
	b.mod.add_instr(.jmp, blk_short_4_16, 0, [b.mod.blocks[blk_final].val_id])

	// --- SHORT 0-3 block ---
	// Branch: len > 0?
	len_gt_0 := b.mod.add_instr(.gt, blk_short_0_3, i1_t, [param_len, zero_64])
	blk_wyr3 := b.mod.add_block(func_idx, 'wyr3')
	b.mod.add_instr(.br, blk_short_0_3, 0,
		[len_gt_0, b.mod.blocks[blk_wyr3].val_id, b.mod.blocks[blk_final].val_id]) // len==0: a=0, b=0 already stored

	// --- WYR3 block (len 1-3) ---
	// a = _wyr3(p, len) = (p[0] << 16) | (p[len>>1] << 8) | p[len-1]
	// b = 0 (already stored)
	byte_0 := b.mod.add_instr(.load, blk_wyr3, i8_t, [param_key])
	byte_0_64 := b.mod.add_instr(.zext, blk_wyr3, i64_t, [byte_0])
	byte_0_shifted := b.mod.add_instr(.shl, blk_wyr3, i64_t, [byte_0_64, sixteen_64])

	// p[len >> 1]
	len_shr1 := b.mod.add_instr(.lshr, blk_wyr3, i64_t, [param_len, one_64])
	p_mid := b.mod.add_instr(.add, blk_wyr3, ptr_u8, [param_key, len_shr1])
	byte_mid := b.mod.add_instr(.load, blk_wyr3, i8_t, [p_mid])
	byte_mid_64 := b.mod.add_instr(.zext, blk_wyr3, i64_t, [byte_mid])
	byte_mid_shifted := b.mod.add_instr(.shl, blk_wyr3, i64_t, [byte_mid_64, eight_64])

	// p[len - 1]
	len_m1 := b.mod.add_instr(.sub, blk_wyr3, i64_t, [param_len, one_64])
	p_last := b.mod.add_instr(.add, blk_wyr3, ptr_u8, [param_key, len_m1])
	byte_last := b.mod.add_instr(.load, blk_wyr3, i8_t, [p_last])
	byte_last_64 := b.mod.add_instr(.zext, blk_wyr3, i64_t, [byte_last])

	// a = byte_0_shifted | byte_mid_shifted | byte_last
	a_tmp := b.mod.add_instr(.or_, blk_wyr3, i64_t, [byte_0_shifted, byte_mid_shifted])
	a_wyr3 := b.mod.add_instr(.or_, blk_wyr3, i64_t, [a_tmp, byte_last_64])

	b.mod.add_instr(.store, blk_wyr3, 0, [a_wyr3, alloca_a])
	b.mod.add_instr(.jmp, blk_wyr3, 0, [b.mod.blocks[blk_final].val_id])

	// === LONG PATH (len > 16) ===
	// For len > 16, use a simplified approach for native backend:
	// One round of mixing with first 16 bytes, then use last 16 bytes as a/b.
	// This matches wyhash for 17-48 byte strings (one mixing round).

	// _wyr8(p) = load u64 from p
	p_as_u64 := b.mod.add_instr(.bitcast, blk_long, ptr_u64, [param_key])
	wyr8_first := b.mod.add_instr(.load, blk_long, i64_t, [p_as_u64])

	// _wyr8(p + 8)
	p_plus_8 := b.mod.add_instr(.add, blk_long, ptr_u8, [param_key, eight_64])
	p_plus_8_u64 := b.mod.add_instr(.bitcast, blk_long, ptr_u64, [p_plus_8])
	wyr8_second := b.mod.add_instr(.load, blk_long, i64_t, [p_plus_8_u64])

	// _wyr8(p + len - 16)
	len_m16 := b.mod.add_instr(.sub, blk_long, i64_t, [param_len, sixteen_64])
	p_end_16 := b.mod.add_instr(.add, blk_long, ptr_u8, [param_key, len_m16])
	p_end_16_u64 := b.mod.add_instr(.bitcast, blk_long, ptr_u64, [p_end_16])
	wyr8_end_16 := b.mod.add_instr(.load, blk_long, i64_t, [p_end_16_u64])

	// _wyr8(p + len - 8)
	len_m8 := b.mod.add_instr(.sub, blk_long, i64_t, [param_len, eight_64])
	p_end_8 := b.mod.add_instr(.add, blk_long, ptr_u8, [param_key, len_m8])
	p_end_8_u64 := b.mod.add_instr(.bitcast, blk_long, ptr_u64, [p_end_8])
	wyr8_end_8 := b.mod.add_instr(.load, blk_long, i64_t, [p_end_8_u64])

	// seed = wymix(wyr8(p) ^ secret[1], wyr8(p+8) ^ seed)
	mix_a := b.mod.add_instr(.xor, blk_long, i64_t, [wyr8_first, wyp1])
	seed_cur := b.mod.add_instr(.load, blk_long, i64_t, [alloca_seed])
	mix_b := b.mod.add_instr(.xor, blk_long, i64_t, [wyr8_second, seed_cur])
	seed_long := b.wymix_inline(blk_long, mix_a, mix_b)

	// Store updated seed, a, and b
	b.mod.add_instr(.store, blk_long, 0, [seed_long, alloca_seed])
	b.mod.add_instr(.store, blk_long, 0, [wyr8_end_16, alloca_a])
	b.mod.add_instr(.store, blk_long, 0, [wyr8_end_8, alloca_b])
	b.mod.add_instr(.jmp, blk_long, 0, [b.mod.blocks[blk_final].val_id])

	// === FINAL block ===
	// a ^= secret[1]; b ^= seed;
	// _wymum(&a, &b);  // (a, b) = (lo(a*b), hi(a*b))
	// return _wymix(a ^ secret[0] ^ len, b ^ secret[1])

	final_a := b.mod.add_instr(.load, blk_final, i64_t, [alloca_a])
	final_b := b.mod.add_instr(.load, blk_final, i64_t, [alloca_b])
	final_seed := b.mod.add_instr(.load, blk_final, i64_t, [alloca_seed])

	final_a_xor := b.mod.add_instr(.xor, blk_final, i64_t, [final_a, wyp1])
	final_b_xor := b.mod.add_instr(.xor, blk_final, i64_t, [final_b, final_seed])

	// _wymum(&a, &b): gives (lo, hi) of 128-bit product
	mum_lo, mum_hi := b.wymum_pair_inline(blk_final, final_a_xor, final_b_xor)

	// return _wymix(lo ^ secret[0] ^ len, hi ^ secret[1])
	lo_xor_s0 := b.mod.add_instr(.xor, blk_final, i64_t, [mum_lo, wyp0])
	lo_xor_s0_len := b.mod.add_instr(.xor, blk_final, i64_t, [lo_xor_s0, param_len])
	hi_xor_s1 := b.mod.add_instr(.xor, blk_final, i64_t, [mum_hi, wyp1])

	final_result := b.wymix_inline(blk_final, lo_xor_s0_len, hi_xor_s1)
	b.mod.add_instr(.ret, blk_final, 0, [final_result])
}

// generate_ierror_stubs creates stub implementations for IError interface methods.
// The native backend doesn't use IError interface dispatch — errors are simple int values.
// These stubs return safe defaults so that unreachable IError method calls don't crash.
fn (mut b Builder) generate_ierror_stubs() {
	i64_t := b.mod.type_store.get_int(64)
	i32_t := b.mod.type_store.get_int(32)
	str_t := b.get_string_type()

	// IError__msg: returns empty string {ptr=0, len=0}
	if 'IError__msg' !in b.fn_index {
		func_idx := b.mod.new_function('IError__msg', str_t, []TypeID{})
		b.fn_index['IError__msg'] = func_idx
		entry := b.mod.add_block(func_idx, 'entry')
		zero_i64 := b.mod.get_or_add_const(i64_t, '0')
		zero_i32 := b.mod.get_or_add_const(i32_t, '0')
		// Build empty string: start with undef, insert ptr=0 at [0], len=0 at [1]
		undef_str := b.mod.get_or_add_const(str_t, 'undef')
		idx0 := b.mod.get_or_add_const(i32_t, '0')
		idx1 := b.mod.get_or_add_const(i32_t, '1')
		s1 := b.mod.add_instr(.insertvalue, entry, str_t, [undef_str, zero_i64, idx0])
		s2 := b.mod.add_instr(.insertvalue, entry, str_t, [s1, zero_i32, idx1])
		b.mod.add_instr(.ret, entry, 0, [s2])
	}

	// IError__code: returns 0
	if 'IError__code' !in b.fn_index {
		func_idx := b.mod.new_function('IError__code', i32_t, []TypeID{})
		b.fn_index['IError__code'] = func_idx
		entry := b.mod.add_block(func_idx, 'entry')
		zero := b.mod.get_or_add_const(i32_t, '0')
		b.mod.add_instr(.ret, entry, 0, [zero])
	}

	// IError__type_name: returns empty string
	if 'IError__type_name' !in b.fn_index {
		func_idx := b.mod.new_function('IError__type_name', str_t, []TypeID{})
		b.fn_index['IError__type_name'] = func_idx
		entry := b.mod.add_block(func_idx, 'entry')
		zero_i64 := b.mod.get_or_add_const(i64_t, '0')
		zero_i32 := b.mod.get_or_add_const(i32_t, '0')
		undef_str := b.mod.get_or_add_const(str_t, 'undef')
		idx0 := b.mod.get_or_add_const(i32_t, '0')
		idx1 := b.mod.get_or_add_const(i32_t, '1')
		s1 := b.mod.add_instr(.insertvalue, entry, str_t, [undef_str, zero_i64, idx0])
		s2 := b.mod.add_instr(.insertvalue, entry, str_t, [s1, zero_i32, idx1])
		b.mod.add_instr(.ret, entry, 0, [s2])
	}
}

fn (mut b Builder) generate_fd_macro_stubs() {
	// FD_ZERO, FD_SET, FD_ISSET are C macros, not linkable functions.
	// Generate inline SSA implementations for the native backend.
	i32_t := b.mod.type_store.get_int(32)
	i8_t := b.mod.type_store.get_int(8)
	ptr_i8 := b.mod.type_store.get_ptr(i8_t)
	ptr_i32 := b.mod.type_store.get_ptr(i32_t)

	// FD_ZERO(fdset *) => zero out 32 i32 words (128 bytes)
	if fd_zero_name := b.find_fd_fn('FD_ZERO') {
		func_idx := b.fn_index[fd_zero_name]
		b.mod.func_set_c_extern(func_idx, false)
		entry := b.mod.add_block(func_idx, 'entry')
		fdset := b.mod.add_value_node(.argument, ptr_i8, 'fdset', 0)
		zero := b.mod.get_or_add_const(i32_t, '0')
		// Zero 32 words (128 bytes) by storing 0 to each i32 slot
		for w in 0 .. 32 {
			off := b.mod.get_or_add_const(i32_t, '${w * 4}')
			ptr := b.mod.add_instr(.get_element_ptr, entry, ptr_i32, [fdset, off])
			b.mod.add_instr(.store, entry, 0, [zero, ptr])
		}
		b.mod.add_instr(.ret, entry, 0, [])
	}

	// FD_SET(fd int, fdset *) => fdset[fd/32] |= (1 << (fd%32))
	if fd_set_name := b.find_fd_fn('FD_SET') {
		func_idx := b.fn_index[fd_set_name]
		b.mod.func_set_c_extern(func_idx, false)
		entry := b.mod.add_block(func_idx, 'entry')
		fd_arg := b.mod.add_value_node(.argument, i32_t, 'fd', 0)
		fdset_arg := b.mod.add_value_node(.argument, ptr_i8, 'fdset', 1)
		c32 := b.mod.get_or_add_const(i32_t, '32')
		c1 := b.mod.get_or_add_const(i32_t, '1')
		c4 := b.mod.get_or_add_const(i32_t, '4')
		idx := b.mod.add_instr(.sdiv, entry, i32_t, [fd_arg, c32])
		bit_pos := b.mod.add_instr(.srem, entry, i32_t, [fd_arg, c32])
		mask := b.mod.add_instr(.shl, entry, i32_t, [c1, bit_pos])
		byte_off := b.mod.add_instr(.mul, entry, i32_t, [idx, c4])
		elem_ptr := b.mod.add_instr(.get_element_ptr, entry, ptr_i32, [fdset_arg, byte_off])
		old_val := b.mod.add_instr(.load, entry, i32_t, [elem_ptr])
		new_val := b.mod.add_instr(.or_, entry, i32_t, [old_val, mask])
		b.mod.add_instr(.store, entry, 0, [elem_ptr, new_val])
		b.mod.add_instr(.ret, entry, 0, [])
	}

	// FD_ISSET(fd int, fdset *) => return (fdset[fd/32] >> (fd%32)) & 1
	if fd_isset_name := b.find_fd_fn('FD_ISSET') {
		func_idx := b.fn_index[fd_isset_name]
		b.mod.func_set_c_extern(func_idx, false)
		entry := b.mod.add_block(func_idx, 'entry')
		fd_arg := b.mod.add_value_node(.argument, i32_t, 'fd', 0)
		fdset_arg := b.mod.add_value_node(.argument, ptr_i8, 'fdset', 1)
		c32 := b.mod.get_or_add_const(i32_t, '32')
		c1 := b.mod.get_or_add_const(i32_t, '1')
		c4 := b.mod.get_or_add_const(i32_t, '4')
		idx := b.mod.add_instr(.sdiv, entry, i32_t, [fd_arg, c32])
		bit_pos := b.mod.add_instr(.srem, entry, i32_t, [fd_arg, c32])
		byte_off := b.mod.add_instr(.mul, entry, i32_t, [idx, c4])
		elem_ptr := b.mod.add_instr(.get_element_ptr, entry, ptr_i32, [fdset_arg, byte_off])
		old_val := b.mod.add_instr(.load, entry, i32_t, [elem_ptr])
		shifted := b.mod.add_instr(.lshr, entry, i32_t, [old_val, bit_pos])
		result := b.mod.add_instr(.and_, entry, i32_t, [shifted, c1])
		b.mod.add_instr(.ret, entry, 0, [result])
	}
}

fn (b &Builder) find_fd_fn(name string) ?string {
	if name in b.fn_index {
		return name
	}
	return none
}

// generate_vinit creates a _vinit function that initializes dynamic array constants.
// Dynamic arrays ([]T) can't be fully serialized to the data segment because their
// struct contains a pointer to data. This function sets up the array struct fields
// (data, offset, len, cap, flags, element_size) at program startup.
pub fn (mut b Builder) generate_vinit() {
	fn_idx := b.mod.new_function('_vinit', 0, [])
	b.mod.func_set_c_extern(fn_idx, false) // Override: we provide the body
	entry := b.mod.add_block(fn_idx, 'entry')

	arr_t := b.get_array_type()
	if arr_t == 0 || b.dyn_const_arrays.len == 0 {
		// No dynamic array constants — emit empty function
		b.mod.add_instr(.ret, entry, 0, [])
		return
	}

	i8_t := b.mod.type_store.get_int(8)
	i32_t := b.mod.type_store.get_int(32)
	i64_t := b.mod.type_store.get_int(64)
	ptr_t := b.mod.type_store.get_ptr(i8_t)

	for dca in b.dyn_const_arrays {
		// Get global pointers
		arr_glob := b.find_global(dca.arr_global_name) or { continue }
		data_glob := b.find_global(dca.data_global_name) or { continue }
		// Cast data global pointer to voidptr
		data_ptr := b.mod.add_instr(.bitcast, entry, ptr_t, [data_glob])
		// Build array struct: {data, offset, len, cap, flags, element_size}
		// field 0: data (voidptr) — cast to i64 for insertvalue
		data_i64 := b.mod.add_instr(.bitcast, entry, i64_t, [data_ptr])
		// field 1: offset = 0
		zero_i32 := b.mod.get_or_add_const(i32_t, '0')
		// field 2: len
		len_val := b.mod.get_or_add_const(i32_t, '${dca.elem_count}')
		// field 3: cap = len
		// field 4: flags = 0
		// field 5: element_size
		esz_val := b.mod.get_or_add_const(i32_t, '${dca.elem_size}')

		idx0 := b.mod.get_or_add_const(i32_t, '0')
		idx1 := b.mod.get_or_add_const(i32_t, '1')
		idx2 := b.mod.get_or_add_const(i32_t, '2')
		idx3 := b.mod.get_or_add_const(i32_t, '3')
		idx4 := b.mod.get_or_add_const(i32_t, '4')
		idx5 := b.mod.get_or_add_const(i32_t, '5')

		undef := b.mod.get_or_add_const(arr_t, 'undef')
		s0 := b.mod.add_instr(.insertvalue, entry, arr_t, [undef, data_i64, idx0])
		s1 := b.mod.add_instr(.insertvalue, entry, arr_t, [s0, zero_i32, idx1])
		s2 := b.mod.add_instr(.insertvalue, entry, arr_t, [s1, len_val, idx2])
		s3 := b.mod.add_instr(.insertvalue, entry, arr_t, [s2, len_val, idx3])
		s4 := b.mod.add_instr(.insertvalue, entry, arr_t, [s3, zero_i32, idx4])
		s5 := b.mod.add_instr(.insertvalue, entry, arr_t, [s4, esz_val, idx5])

		b.mod.add_instr(.store, entry, 0, [s5, arr_glob])
	}

	b.mod.add_instr(.ret, entry, 0, [])
}
