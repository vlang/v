module types

import v3.flat

fn tarr1(a Type) []Type {
	mut r := []Type{}
	r << a
	return r
}

fn tarr2(a Type, b Type) []Type {
	mut r := []Type{}
	r << a
	r << b
	return r
}

fn tarr3(a Type, b Type, c Type) []Type {
	mut r := []Type{}
	r << a
	r << b
	r << c
	return r
}

fn unknown_type(reason string) Type {
	return Type(Unknown{
		reason: reason
	})
}

pub struct TypeError {
pub:
	msg  string
	kind TypeErrorKind
	node flat.NodeId
}

pub enum TypeErrorKind {
	unknown_ident
	unknown_type
	unknown_fn
	unknown_field
	cannot_index
	if_branch_mismatch
	assignment_mismatch
	return_mismatch
	call_arg_mismatch
	condition_mismatch
	unhandled_node
	unsupported_generic
}

struct CallInfo {
	name         string
	params       []Type
	return_type  Type
	has_receiver bool
	is_variadic  bool
	params_known bool
}

struct LocalBinding {
	name string
	typ  Type
}

struct TypeCache {
mut:
	parse_enabled bool
	parse_entries map[string]Type
	c_entries     map[string]string
}

@[heap]
pub struct TypeChecker {
pub mut:
	a                          &flat.FlatAst = unsafe { nil }
	fn_ret_types               map[string]Type
	fn_param_types             map[string][]Type
	fn_variadic                map[string]bool
	structs                    map[string][]StructField
	unions                     map[string]bool
	type_aliases               map[string]string
	sum_types                  map[string][]string
	enum_names                 map[string]bool
	enum_fields                map[string][]string
	flag_enums                 map[string]bool
	interface_names            map[string]bool
	interface_fields           map[string][]StructField
	interface_embeds           map[string][]string
	interface_abstract_methods map[string][]string // iface -> abstract (declared) method names

	c_globals                     map[string]Type
	const_types                   map[string]Type
	const_exprs                   map[string]flat.NodeId
	const_modules                 map[string]string
	const_suffixes                map[string]string // dot-suffix -> full const key (O(1) lookup; '' if ambiguous)
	imports                       map[string]string // alias -> short module name
	file_imports                  map[string]string
	file_modules                  map[string]string
	file_scope                    &Scope = unsafe { nil }
	cur_scope                     &Scope = unsafe { nil }
	scope_pool                    []&Scope
	scope_pool_index              int
	has_builtins                  bool
	cur_module                    string
	cur_file                      string
	errors                        []TypeError
	resolved_call_names           []string // node_id -> resolved function name
	resolved_call_set             []bool
	expr_type_values              []Type // node_id -> complex/contextual resolved type
	expr_type_set                 []bool
	checking_nodes                []bool
	diagnose_unknown_calls        bool
	reject_unlowered_map_mutation bool
	diagnostic_files              map[string]bool
	cur_fn_ret_type               Type = Type(void_)
	smartcasts                    map[string]Type
mut:
	type_cache &TypeCache = unsafe { nil }
}

pub fn TypeChecker.new(a &flat.FlatAst) TypeChecker {
	fs := new_scope(unsafe { nil })
	return TypeChecker{
		a:                          a
		fn_ret_types:               map[string]Type{}
		fn_param_types:             map[string][]Type{}
		fn_variadic:                map[string]bool{}
		structs:                    map[string][]StructField{}
		unions:                     map[string]bool{}
		type_aliases:               map[string]string{}
		sum_types:                  map[string][]string{}
		enum_names:                 map[string]bool{}
		enum_fields:                map[string][]string{}
		flag_enums:                 map[string]bool{}
		interface_names:            map[string]bool{}
		interface_fields:           map[string][]StructField{}
		interface_embeds:           map[string][]string{}
		interface_abstract_methods: map[string][]string{}
		c_globals:                  map[string]Type{}
		const_types:                map[string]Type{}
		const_exprs:                map[string]flat.NodeId{}
		const_modules:              map[string]string{}
		imports:                    map[string]string{}
		file_imports:               map[string]string{}
		file_modules:               map[string]string{}
		file_scope:                 fs
		cur_scope:                  fs
		resolved_call_names:        []string{len: a.nodes.len}
		resolved_call_set:          []bool{len: a.nodes.len}
		expr_type_values:           []Type{len: a.nodes.len, init: Type(void_)}
		expr_type_set:              []bool{len: a.nodes.len}
		checking_nodes:             []bool{len: a.nodes.len}
		diagnostic_files:           map[string]bool{}
		smartcasts:                 map[string]Type{}
		type_cache:                 &TypeCache{
			parse_entries: map[string]Type{}
			c_entries:     map[string]string{}
		}
	}
}

fn (mut tc TypeChecker) reset_node_caches(n int) {
	tc.resolved_call_names = []string{len: n}
	tc.resolved_call_set = []bool{len: n}
	tc.expr_type_values = []Type{len: n, init: Type(void_)}
	tc.expr_type_set = []bool{len: n}
	tc.checking_nodes = []bool{len: n}
}

pub fn (mut tc TypeChecker) push_scope() {
	tc.cur_scope = tc.reuse_scope(tc.cur_scope)
}

pub fn (mut tc TypeChecker) pop_scope() {
	if tc.cur_scope == unsafe { nil } {
		return
	}
	parent := tc.cur_scope.parent
	if parent == unsafe { nil } {
		return
	}
	if tc.scope_pool_index > 0 && tc.cur_scope == tc.scope_pool[tc.scope_pool_index - 1] {
		tc.scope_pool_index--
	}
	tc.cur_scope = parent
}

fn (mut tc TypeChecker) reuse_scope(parent &Scope) &Scope {
	if tc.scope_pool_index < tc.scope_pool.len {
		mut scope := tc.scope_pool[tc.scope_pool_index]
		scope.reset(parent)
		tc.scope_pool_index++
		return scope
	}
	scope := new_scope(parent)
	tc.scope_pool << scope
	tc.scope_pool_index++
	return scope
}

fn (mut tc TypeChecker) record_error(kind TypeErrorKind, msg string, node flat.NodeId) {
	if !tc.should_diagnose(node) {
		return
	}
	tc.errors << TypeError{
		msg:  msg
		kind: kind
		node: node
	}
}

pub fn (mut tc TypeChecker) collect(a &flat.FlatAst) {
	tc.a = a
	tc.file_scope = new_scope(unsafe { nil })
	tc.cur_scope = tc.file_scope
	tc.scope_pool_index = 0
	tc.reset_node_caches(a.nodes.len)
	tc.type_cache = &TypeCache{
		parse_entries: map[string]Type{}
		c_entries:     map[string]string{}
	}
	for node in a.nodes {
		if node.kind == .struct_decl && node.value == 'string' {
			tc.has_builtins = true
			break
		}
	}
	// Pass 1: collect type-level names (aliases, enums, sum types)
	for node in a.nodes {
		match node.kind {
			.file {
				tc.enter_file(node.value)
			}
			.module_decl {
				tc.enter_module(node.value)
			}
			.import_decl {
				mod := if node.value.contains('.') {
					node.value.all_after_last('.')
				} else {
					node.value
				}
				tc.imports[node.typ] = mod
				tc.file_imports[file_import_key(tc.cur_file, node.typ)] = mod
			}
			.enum_decl {
				qn := tc.qualify_name(node.value)
				tc.enum_names[qn] = true
				mut fields := []string{}
				for i in 0 .. node.children_count {
					f := a.child_node(&node, i)
					if f.kind == .enum_field {
						fields << f.value
					}
				}
				tc.enum_fields[qn] = fields
				if node.typ == 'flag' {
					tc.flag_enums[qn] = true
				}
			}
			.struct_decl {
				qname := tc.qualify_name(node.value)
				if qname !in tc.structs {
					tc.structs[qname] = []StructField{}
				}
				if node.typ.contains('union') {
					tc.unions[qname] = true
				}
			}
			.type_decl {
				if node.children_count > 0 {
					mut variants := []string{}
					for i in 0 .. node.children_count {
						v := a.child_node(&node, i)
						variants << tc.qualify_name(v.value)
					}
					tc.sum_types[tc.qualify_name(node.value)] = variants
				} else if node.typ.len > 0 {
					tc.type_aliases[tc.qualify_name(node.value)] = tc.qualify_type_text(node.typ)
					if tc.cur_module in ['', 'main', 'builtin'] && node.value !in tc.type_aliases {
						tc.type_aliases[node.value] = tc.qualify_type_text(node.typ)
					}
				}
			}
			.interface_decl {
				tc.interface_names[tc.qualify_name(node.value)] = true
			}
			else {}
		}
	}
	// Pass 2: collect struct fields, function signatures (type aliases now available)
	tc.type_cache.parse_enabled = true
	tc.cur_module = ''
	for node in a.nodes {
		match node.kind {
			.file {
				tc.enter_file(node.value)
			}
			.module_decl {
				tc.enter_module(node.value)
			}
			.fn_decl {
				qname := tc.qualify_fn_name(node.value)
				ret_type := tc.parse_type(node.typ)
				mut ptypes := []Type{}
				mut is_variadic := false
				for i in 0 .. node.children_count {
					child := a.child_node(&node, i)
					if child.kind == .param {
						if child.typ.starts_with('...') {
							is_variadic = true
						}
						ptypes << tc.parse_type(child.typ)
					}
				}
				tc.register_fn_signature(qname, ret_type, ptypes, is_variadic)
				if tc.cur_module in ['', 'main', 'builtin'] && qname != node.value
					&& node.value !in tc.fn_param_types {
					tc.register_fn_signature(node.value, ret_type, ptypes, is_variadic)
				}
			}
			.struct_decl {
				mut fields := []StructField{}
				for i in 0 .. node.children_count {
					f := a.child_node(&node, i)
					if f.kind != .field_decl {
						continue
					}
					fields << StructField{
						name: f.value
						typ:  tc.parse_type(f.typ)
					}
				}
				qname := tc.qualify_name(node.value)
				tc.structs[qname] = fields
				if node.typ.contains('union') {
					tc.unions[qname] = true
				}
			}
			.c_fn_decl {
				ret_type := tc.parse_type(node.typ)
				mut ptypes := []Type{}
				mut is_variadic := false
				for i in 0 .. node.children_count {
					child := a.child_node(&node, i)
					if child.kind == .param {
						if child.typ.starts_with('...') {
							is_variadic = true
						}
						ptypes << tc.parse_type(child.typ)
					}
				}
				tc.register_fn_signature(node.value, ret_type, ptypes, is_variadic)
			}
			.interface_decl {
				iface_name := tc.qualify_name(node.value)
				for i in 0 .. node.children_count {
					f := a.child_node(&node, i)
					if f.kind != .interface_field {
						continue
					}
					if f.op == .dot {
						mname := '${iface_name}.${f.value}'
						mut absm := tc.interface_abstract_methods[iface_name] or { []string{} }
						absm << f.value
						tc.interface_abstract_methods[iface_name] = absm
						tc.fn_ret_types[mname] = tc.parse_type(f.typ)
						mut ptypes := []Type{}
						mut is_variadic := false
						ptypes << Type(Pointer{
							base_type: Type(Interface{
								name: iface_name
							})
						})
						for j in 0 .. f.children_count {
							child := a.child_node(f, j)
							if child.kind == .param {
								if child.typ.starts_with('...') {
									is_variadic = true
								}
								ptypes << tc.parse_type(child.typ)
							}
						}
						tc.fn_param_types[mname] = ptypes
						tc.fn_variadic[mname] = is_variadic
					} else if f.typ.len > 0 {
						mut fields := tc.interface_fields[iface_name] or { []StructField{} }
						fields << StructField{
							name: f.value
							typ:  tc.parse_type(f.typ)
						}
						tc.interface_fields[iface_name] = fields
					} else if f.value.len > 0 {
						mut embeds := tc.interface_embeds[iface_name] or { []string{} }
						embeds << tc.qualify_name(f.value)
						tc.interface_embeds[iface_name] = embeds
					}
				}
			}
			.global_decl {
				for i in 0 .. node.children_count {
					f := a.child_node(&node, i)
					if f.value.len > 0 && f.value.starts_with('C.') {
						tc.c_globals[f.value] = tc.parse_type(f.typ)
					} else if f.value.len > 0 {
						mut ft := tc.parse_type(f.typ)
						if ft is Void && f.children_count > 0 {
							ft = tc.resolve_type(a.child(f, 0))
						}
						qname := tc.qualify_name(f.value)
						tc.file_scope.insert(qname, ft)
					}
				}
			}
			.const_decl {
				for i in 0 .. node.children_count {
					f := a.child_node(&node, i)
					if f.kind == .const_field && f.children_count > 0 {
						qname := tc.qualify_name(f.value)
						tc.const_types[qname] = unknown_type('pending const `${qname}`')
						tc.const_exprs[qname] = a.child(f, 0)
						tc.const_modules[qname] = tc.cur_module
					} else if f.kind == .const_field && f.typ.len > 0 {
						qname := tc.qualify_name(f.value)
						tc.const_types[qname] = tc.parse_type(f.typ)
					}
				}
			}
			else {}
		}
	}
	tc.resolve_const_types()
	tc.build_const_suffixes()
}

// build_const_suffixes maps every dot-delimited suffix of each const key to that
// key, so qualified const lookups resolve in O(1) instead of scanning all consts
// (with per-iteration string building) on every selector/ident in module code.
fn (mut tc TypeChecker) build_const_suffixes() {
	for key, _ in tc.const_types {
		mut i := 0
		for i < key.len {
			if key[i] == `.` {
				suffix := key[i + 1..]
				if existing := tc.const_suffixes[suffix] {
					if existing != key {
						tc.const_suffixes[suffix] = ''
					}
				} else {
					tc.const_suffixes[suffix] = key
				}
			}
			i++
		}
	}
}

// const_key_for_suffix returns the const key matching `.${name}` as a suffix,
// equivalent to scanning const_types for `key.ends_with('.' + name)` but O(1).
fn (tc &TypeChecker) const_key_for_suffix(name string) ?string {
	if key := tc.const_suffixes[name] {
		if key.len > 0 {
			return key
		}
	}
	return none
}

fn (mut tc TypeChecker) resolve_const_types() {
	if tc.const_exprs.len == 0 {
		return
	}
	saved_module := tc.cur_module
	for _ in 0 .. tc.const_exprs.len {
		mut changed := false
		for name, expr_id in tc.const_exprs {
			tc.cur_module = tc.const_modules[name] or { '' }
			mut new_type := tc.resolve_type(expr_id)
			new_type = tc.const_type_from_initializer(name, new_type)
			if new_type is Unknown {
				continue
			}
			expr := tc.a.nodes[int(expr_id)]
			if new_type is ArrayFixed && expr.kind == .call {
				new_type = Type(Array{
					elem_type: new_type.elem_type
				})
			}
			old_type := tc.const_types[name] or { Type(void_) }
			if old_type.name() != new_type.name() {
				tc.const_types[name] = new_type
				changed = true
			}
		}
		if !changed {
			break
		}
	}
	tc.cur_module = saved_module
}

fn (tc &TypeChecker) const_type_from_initializer(name string, typ Type) Type {
	if typ !is Unknown {
		return typ
	}
	expr_id := tc.const_exprs[name] or { return typ }
	expr := tc.a.nodes[int(expr_id)]
	if expr.kind != .call || expr.children_count == 0 {
		return typ
	}
	fn_node := tc.a.child_node(&expr, 0)
	if fn_node.kind != .ident || fn_node.value.len == 0 {
		return typ
	}
	mod_name := tc.const_modules[name] or { '' }
	mut candidates := []string{}
	if mod_name.len > 0 && mod_name != 'main' && mod_name != 'builtin' {
		candidates << '${mod_name}.${fn_node.value}'
	}
	candidates << fn_node.value
	candidates << c_name(fn_node.value)
	for candidate in candidates {
		if ret := tc.fn_ret_types[candidate] {
			return ret
		}
	}
	if fn_node.value == 'new_keywords_matcher_trie' {
		type_name := if mod_name.len > 0 && mod_name != 'main' && mod_name != 'builtin' {
			'${mod_name}.KeywordsMatcherTrie'
		} else {
			'KeywordsMatcherTrie'
		}
		return Type(Struct{
			name: type_name
		})
	}
	return typ
}

pub fn (tc &TypeChecker) qualify_fn_name(name string) string {
	if tc.cur_module.len == 0 || tc.cur_module == 'main' || tc.cur_module == 'builtin' {
		return name
	}
	return '${tc.cur_module}.${name}'
}

pub fn (tc &TypeChecker) qualify_name(name string) string {
	if tc.cur_module.len == 0 || tc.cur_module == 'main' || tc.cur_module == 'builtin' {
		return name
	}
	if name.starts_with('[]') {
		return '[]' + tc.qualify_name(name[2..])
	}
	if name.starts_with('[') {
		idx := name.index_u8(`]`)
		if idx > 0 {
			return name[..idx + 1] + tc.qualify_name(name[idx + 1..])
		}
	}
	if name.starts_with('map[') {
		bracket_end := find_matching_bracket(name, 3)
		key_str := name[4..bracket_end]
		val_str := name[bracket_end + 1..]
		return 'map[${tc.qualify_name(key_str)}]${tc.qualify_name(val_str)}'
	}
	if name.starts_with('&') {
		return '&' + tc.qualify_name(name[1..])
	}
	if name.starts_with('?') {
		return '?' + tc.qualify_name(name[1..])
	}
	if name.contains('.') {
		return name
	}
	if is_builtin_type_name(name) {
		return name
	}
	return tc.cur_module + '.' + name
}

fn (tc &TypeChecker) qualify_type_text(typ string) string {
	clean := typ.trim_space()
	if clean.len == 0 {
		return typ
	}
	if clean.starts_with('&') {
		return '&' + tc.qualify_type_text(clean[1..])
	}
	if clean.starts_with('mut ') {
		inner := tc.qualify_type_text(clean[4..])
		if inner.starts_with('&') {
			return inner
		}
		return '&' + inner
	}
	if clean.starts_with('shared ') {
		return 'shared ' + tc.qualify_type_text(clean[7..])
	}
	if clean.starts_with('atomic ') {
		return 'atomic ' + tc.qualify_type_text(clean[7..])
	}
	if clean.starts_with('?') {
		return '?' + tc.qualify_type_text(clean[1..])
	}
	if clean.starts_with('!') {
		return '!' + tc.qualify_type_text(clean[1..])
	}
	if clean.starts_with('...') {
		return '...' + tc.qualify_type_text(clean[3..])
	}
	if clean.starts_with('[]') {
		return '[]' + tc.qualify_type_text(clean[2..])
	}
	if clean.starts_with('map[') {
		bracket_end := find_matching_bracket(clean, 3)
		if bracket_end < clean.len {
			key := tc.qualify_type_text(clean[4..bracket_end])
			val := tc.qualify_type_text(clean[bracket_end + 1..])
			return 'map[${key}]${val}'
		}
	}
	if clean.starts_with('[') {
		bracket_end := find_matching_bracket(clean, 0)
		if bracket_end < clean.len {
			return clean[..bracket_end + 1] + tc.qualify_type_text(clean[bracket_end + 1..])
		}
	}
	if clean.starts_with('(') && clean.ends_with(')') && clean.contains(',') {
		mut parts := []string{}
		for part in split_params(clean[1..clean.len - 1]) {
			parts << tc.qualify_type_text(part)
		}
		return '(' + parts.join(', ') + ')'
	}
	if clean.starts_with('fn(') || clean.starts_with('fn (') {
		return tc.qualify_fn_type_text(clean)
	}
	bracket := clean.index_u8(`[`)
	if bracket > 0 {
		bracket_end := find_matching_bracket(clean, bracket)
		if bracket_end < clean.len {
			mut parts := []string{}
			for part in split_params(clean[bracket + 1..bracket_end]) {
				parts << tc.qualify_type_text(part)
			}
			return tc.qualify_name(clean[..bracket]) + '[' + parts.join(', ') + ']' +
				clean[bracket_end + 1..]
		}
	}
	return tc.qualify_name(clean)
}

fn (tc &TypeChecker) qualify_fn_type_text(typ string) string {
	params_start := typ.index_u8(`(`) + 1
	mut depth := 1
	mut params_end := params_start
	for params_end < typ.len {
		if typ[params_end] == `(` {
			depth++
		} else if typ[params_end] == `)` {
			depth--
			if depth == 0 {
				break
			}
		}
		params_end++
	}
	params_str := typ[params_start..params_end]
	mut params := []string{}
	if params_str.trim_space().len > 0 {
		for part in split_params(params_str) {
			params << tc.qualify_type_text(normalize_fn_type_param_text(part))
		}
	}
	ret_str := typ[params_end + 1..].trim_space()
	if ret_str.len > 0 {
		return 'fn(${params.join(', ')}) ${tc.qualify_type_text(ret_str)}'
	}
	return 'fn(${params.join(', ')})'
}

fn file_import_key(file string, alias string) string {
	return '${file}\n${alias}'
}

fn (mut tc TypeChecker) enter_file(file string) {
	tc.cur_file = file
	tc.cur_module = tc.file_modules[file] or { '' }
}

fn (mut tc TypeChecker) enter_module(name string) {
	tc.cur_module = name
	if tc.cur_file.len > 0 {
		tc.file_modules[tc.cur_file] = name
	}
}

fn (tc &TypeChecker) resolve_import_alias(alias string) ?string {
	if mod := tc.file_imports[file_import_key(tc.cur_file, alias)] {
		return mod
	}
	return none
}

fn (tc &TypeChecker) has_active_import(alias string) bool {
	return file_import_key(tc.cur_file, alias) in tc.file_imports
}

fn (mut tc TypeChecker) register_fn_signature(name string, ret_type Type, params []Type, is_variadic bool) {
	tc.register_fn_name_alias(name, ret_type, params, is_variadic)
	lowered_name := c_name(name)
	if lowered_name != name {
		tc.register_fn_name_alias(lowered_name, ret_type, params, is_variadic)
	}
	if name.ends_with('.str') {
		receiver := name.all_before_last('.')
		legacy_name := '${receiver}_str'
		if !legacy_name.contains('.') {
			tc.register_fn_name_alias(legacy_name, ret_type, params, is_variadic)
		}
	}
}

fn (mut tc TypeChecker) register_fn_name_alias(name string, ret_type Type, params []Type, is_variadic bool) {
	tc.fn_ret_types[name] = ret_type
	tc.fn_param_types[name] = params.clone()
	tc.fn_variadic[name] = is_variadic
}

// annotate_types performs a scope-aware walk over every function body, tracking
// local variable types as they are declared, and records complex/contextual
// expression types. This mirrors what the v2 transformer relies on: the type
// checker runs BEFORE the transformer and publishes per-expression types, so the
// transformer can own type-dependent lowering (string ops, `in` membership, ...)
// instead of the backend.
//
// It uses a single flat scope per function (an over-approximation: a local stays
// visible after its block ends), which is harmless for type lookup since variable
// names are effectively unique within a function.
pub fn (mut tc TypeChecker) annotate_types() {
	tc.cur_module = ''
	for node in tc.a.nodes {
		if node.kind == .file {
			tc.enter_file(node.value)
		} else if node.kind == .module_decl {
			tc.enter_module(node.value)
		} else if node.kind == .fn_decl {
			tc.cur_scope = tc.file_scope
			tc.push_scope()
			for pi in 0 .. node.children_count {
				p := tc.a.child_node(&node, pi)
				if p.kind == .param && p.value.len > 0 {
					tc.cur_scope.insert(p.value, tc.parse_type(p.typ))
				}
			}
			for i in 0 .. node.children_count {
				child := tc.a.child_node(&node, i)
				if child.kind != .param {
					tc.annotate_node(tc.a.child(&node, i))
				}
			}
			tc.pop_scope()
		}
	}
}

fn (mut tc TypeChecker) annotate_node(id flat.NodeId) {
	if int(id) < 0 {
		return
	}
	node := tc.a.nodes[int(id)]
	match node.kind {
		.decl_assign {
			// children are interleaved pairs [lhs0, rhs0, lhs1, rhs1, ...].
			// Multi-return (`a, b := f()`) yields an odd count; we only insert
			// locals for clean pairs and skip MultiReturn rhs values.
			mut i := 0
			for i + 1 < node.children_count {
				lhs_id := tc.a.child(&node, i)
				rhs_id := tc.a.child(&node, i + 1)
				tc.annotate_node(rhs_id)
				lhs := tc.a.nodes[int(lhs_id)]
				if lhs.kind == .ident && lhs.value.len > 0 {
					mut typ := Type(void_)
					if node.children_count == 2 && node.typ.len > 0 {
						typ = tc.parse_type(node.typ)
					} else {
						typ = tc.resolve_type(rhs_id)
					}
					if typ !is MultiReturn && typ !is Void {
						tc.cur_scope.insert(lhs.value, typ)
						tc.remember_expr_type(lhs_id, typ)
					}
				}
				i += 2
			}
			return
		}
		.for_in_stmt {
			tc.annotate_for_in(id, node)
			return
		}
		else {}
	}

	tc.remember_expr_type(id, tc.resolve_type(id))
	for i in 0 .. node.children_count {
		tc.annotate_node(tc.a.child(&node, i))
	}
}

fn (mut tc TypeChecker) annotate_for_in(_id flat.NodeId, node flat.Node) {
	header := node.value.int()
	if header < 3 || node.children_count < 3 {
		return
	}
	key_id := tc.a.child(&node, 0)
	val_id := tc.a.child(&node, 1)
	container_id := tc.a.child(&node, 2)
	tc.annotate_node(container_id)
	has_val := int(val_id) >= 0
	if header == 4 {
		tc.insert_loop_var(key_id, Type(int_))
		tc.annotate_node(tc.a.child(&node, 3))
	} else {
		clean := unwrap_pointer(tc.resolve_type(container_id))
		if clean is Array {
			if has_val {
				tc.insert_loop_var(key_id, Type(int_))
				tc.insert_loop_var(val_id, clean.elem_type)
			} else {
				tc.insert_loop_var(key_id, clean.elem_type)
			}
		} else if clean is ArrayFixed {
			if has_val {
				tc.insert_loop_var(key_id, Type(int_))
				tc.insert_loop_var(val_id, clean.elem_type)
			} else {
				tc.insert_loop_var(key_id, clean.elem_type)
			}
		} else if clean is Map {
			if has_val {
				tc.insert_loop_var(key_id, clean.key_type)
				tc.insert_loop_var(val_id, clean.value_type)
			} else {
				tc.insert_loop_var(key_id, clean.value_type)
			}
		} else if clean is String {
			if has_val {
				tc.insert_loop_var(key_id, Type(int_))
				tc.insert_loop_var(val_id, Type(u8_))
			} else {
				tc.insert_loop_var(key_id, Type(u8_))
			}
		} else {
			container := tc.a.nodes[int(container_id)]
			if container.kind == .range {
				tc.insert_loop_var(key_id, Type(int_))
			}
		}
	}
	for i in header .. node.children_count {
		tc.annotate_node(tc.a.child(&node, i))
	}
}

fn (mut tc TypeChecker) insert_loop_var(id flat.NodeId, typ Type) {
	if int(id) < 0 {
		return
	}
	v := tc.a.nodes[int(id)]
	if v.kind == .ident && v.value.len > 0 {
		tc.cur_scope.insert(v.value, typ)
		tc.remember_expr_type(id, typ)
	}
}

// expr_type returns the resolved type recorded for a node during annotate_types.
pub fn (tc &TypeChecker) expr_type(id flat.NodeId) ?Type {
	if int(id) >= 0 {
		node := tc.a.nodes[int(id)]
		if node.kind == .call && node.typ.len > 0 {
			return tc.parse_type(node.typ)
		}
	}
	if t := tc.resolved_call_type(id) {
		return t
	}
	if t := tc.cached_expr_type(id) {
		return t
	}
	return none
}

fn (tc &TypeChecker) resolved_call_type(id flat.NodeId) ?Type {
	if int(id) < 0 {
		return none
	}
	node := tc.a.nodes[int(id)]
	if node.kind != .call {
		return none
	}
	if name := tc.cached_resolved_call(id) {
		if t := tc.fn_ret_types[name] {
			return t
		}
	}
	return none
}

fn (tc &TypeChecker) cached_expr_type(id flat.NodeId) ?Type {
	idx := int(id)
	if idx >= 0 && idx < tc.expr_type_set.len && tc.expr_type_set[idx] {
		return tc.expr_type_values[idx]
	}
	return none
}

fn (tc &TypeChecker) cached_resolved_call(id flat.NodeId) ?string {
	idx := int(id)
	if idx >= 0 && idx < tc.resolved_call_set.len && tc.resolved_call_set[idx] {
		return tc.resolved_call_names[idx]
	}
	return none
}

// resolved_call_name returns the checker-resolved function name for a call node.
pub fn (tc &TypeChecker) resolved_call_name(id flat.NodeId) ?string {
	return tc.cached_resolved_call(id)
}

fn (mut tc TypeChecker) remember_resolved_call(id flat.NodeId, name string) {
	idx := int(id)
	if idx < 0 {
		return
	}
	if idx >= tc.resolved_call_names.len {
		tc.reset_node_caches(tc.a.nodes.len)
	}
	if idx < tc.resolved_call_names.len {
		tc.resolved_call_names[idx] = name
		tc.resolved_call_set[idx] = true
	}
}

// register_synth_type records the type of a generated or transformed node.
pub fn (mut tc TypeChecker) register_synth_type(id flat.NodeId, typ Type) {
	tc.remember_expr_type(id, typ)
}

fn (mut tc TypeChecker) remember_expr_type(id flat.NodeId, typ Type) {
	if int(id) < 0 {
		return
	}
	kind := if int(id) < tc.a.nodes.len { tc.a.nodes[int(id)].kind } else { flat.NodeKind.empty }
	if should_cache_expr_type(kind, typ) {
		idx := int(id)
		if idx >= tc.expr_type_values.len {
			tc.reset_node_caches(tc.a.nodes.len)
		}
		if idx < tc.expr_type_values.len {
			tc.expr_type_values[idx] = typ
			tc.expr_type_set[idx] = true
		}
	}
}

fn should_cache_expr_type(kind flat.NodeKind, typ Type) bool {
	if typ is Void || typ is Unknown {
		return false
	}
	if typ is Array || typ is ArrayFixed || typ is Map || typ is Pointer || typ is FnType
		|| typ is OptionType || typ is ResultType || typ is Struct || typ is Interface
		|| typ is Enum || typ is SumType || typ is Alias || typ is MultiReturn {
		return true
	}
	kind_id := int(kind)
	return kind_id != 1 && kind_id != 2 && kind_id != 3 && kind_id != 4 && kind_id != 5
		&& kind_id != 28
}

pub fn (mut tc TypeChecker) check_semantics() {
	tc.cur_module = ''
	tc.cur_file = ''
	for i, node in tc.a.nodes {
		match node.kind {
			.file {
				tc.enter_file(node.value)
			}
			.module_decl {
				tc.enter_module(node.value)
			}
			.struct_decl {
				tc.check_decl_type_strings(flat.NodeId(i), node)
				tc.check_struct_field_defaults(node)
			}
			.type_decl, .interface_decl {
				tc.check_decl_type_strings(flat.NodeId(i), node)
			}
			.enum_decl {
				tc.check_enum_field_values(node)
			}
			.const_decl {
				tc.check_const_field_values(node)
			}
			.fn_decl {
				tc.check_decl_type_strings(flat.NodeId(i), node)
				tc.cur_fn_ret_type = tc.parse_type(node.typ)
				tc.push_scope()
				for pi in 0 .. node.children_count {
					p := tc.a.child_node(&node, pi)
					if p.kind == .param && p.value.len > 0 {
						tc.cur_scope.insert(p.value, tc.parse_type(p.typ))
					}
				}
				tc.check_fn_body(node)
				is_disabled_stub := node.value in tc.a.disabled_fns
				if tc.cur_fn_ret_type !is Void && !tc.fn_body_definitely_returns(node)
					&& !is_disabled_stub && tc.should_diagnose(flat.NodeId(i)) {
					tc.record_error(.return_mismatch,
						'missing return at end of function `${node.value}`; expected `${tc.cur_fn_ret_type.name()}`',
						flat.NodeId(i))
				}
				tc.pop_scope()
				tc.cur_fn_ret_type = Type(void_)
			}
			else {}
		}

		_ = i
	}
}

fn (mut tc TypeChecker) check_fn_body(node flat.Node) {
	for i in 0 .. node.children_count {
		child_id := tc.a.child(&node, i)
		child := tc.a.child_node(&node, i)
		if child.kind == .param {
			continue
		}
		tc.check_node(child_id)
	}
}

fn (mut tc TypeChecker) check_decl_type_strings(node_id flat.NodeId, node flat.Node) {
	if !(node.kind == .struct_decl && node.typ.contains('union')) {
		tc.check_type_string_for_unsupported_generics(node.typ, node_id)
	}
	for i in 0 .. node.children_count {
		child_id := tc.a.child(&node, i)
		if int(child_id) < 0 {
			continue
		}
		child := tc.a.nodes[int(child_id)]
		tc.check_type_string_for_unsupported_generics(child.typ, child_id)
		if node.kind == .type_decl && child.value.len > 0 {
			tc.check_type_string_for_unsupported_generics(child.value, child_id)
		}
		for j in 0 .. child.children_count {
			grandchild_id := tc.a.child(&child, j)
			if int(grandchild_id) < 0 {
				continue
			}
			grandchild := tc.a.nodes[int(grandchild_id)]
			tc.check_type_string_for_unsupported_generics(grandchild.typ, grandchild_id)
		}
	}
}

fn (mut tc TypeChecker) check_type_string_for_unsupported_generics(typ string, node_id flat.NodeId) {
	clean := typ.trim_space()
	if clean.len == 0 {
		return
	}
	if clean.starts_with('&') || clean.starts_with('?') || clean.starts_with('!') {
		tc.check_type_string_for_unsupported_generics(clean[1..], node_id)
		return
	}
	if clean.starts_with('shared ') {
		tc.check_type_string_for_unsupported_generics(clean[7..], node_id)
		return
	}
	if clean.starts_with('...') {
		tc.check_type_string_for_unsupported_generics(clean[3..], node_id)
		return
	}
	if clean.starts_with('[]') {
		tc.check_type_string_for_unsupported_generics(clean[2..], node_id)
		return
	}
	if clean.starts_with('map[') {
		bracket_end := find_matching_bracket(clean, 3)
		if bracket_end < clean.len {
			tc.check_type_string_for_unsupported_generics(clean[4..bracket_end], node_id)
			tc.check_type_string_for_unsupported_generics(clean[bracket_end + 1..], node_id)
		}
		return
	}
	if clean.starts_with('[') {
		idx := clean.index_u8(`]`)
		if idx > 0 {
			tc.check_type_string_for_unsupported_generics(clean[idx + 1..], node_id)
		}
		return
	}
	if clean.starts_with('(') && clean.ends_with(')') {
		for part in split_params(clean[1..clean.len - 1]) {
			tc.check_type_string_for_unsupported_generics(part, node_id)
		}
		return
	}
	if clean.starts_with('fn(') || clean.starts_with('fn (') {
		tc.check_fn_type_string_for_unsupported_generics(clean, node_id)
		return
	}
	if generic_type_application(clean) {
		tc.record_error(.unsupported_generic, 'unsupported generic type application `${clean}`',
			node_id)
		return
	}
	if is_bare_generic_param(clean) && !tc.type_name_known(clean) {
		tc.record_error(.unsupported_generic, 'unsupported generic type parameter `${clean}`',
			node_id)
		return
	}
	if should_check_named_type(clean) && !tc.type_name_known(clean) {
		tc.record_error(.unknown_type, 'unknown type `${clean}`', node_id)
	}
}

fn (mut tc TypeChecker) check_fn_type_string_for_unsupported_generics(typ string, node_id flat.NodeId) {
	params_start := typ.index_u8(`(`) + 1
	mut depth := 1
	mut params_end := params_start
	for params_end < typ.len {
		if typ[params_end] == `(` {
			depth++
		} else if typ[params_end] == `)` {
			depth--
			if depth == 0 {
				break
			}
		}
		params_end++
	}
	if params_end >= typ.len {
		return
	}
	for part in split_params(typ[params_start..params_end]) {
		trimmed := part.trim_space()
		parts := trimmed.split(' ')
		param_type := if parts.len >= 2 { parts[parts.len - 1] } else { trimmed }
		tc.check_type_string_for_unsupported_generics(param_type, node_id)
	}
	ret := typ[params_end + 1..].trim_space()
	tc.check_type_string_for_unsupported_generics(ret, node_id)
}

fn generic_type_application(typ string) bool {
	if typ.starts_with('[') || !typ.contains('[') {
		return false
	}
	bracket := typ.index_u8(`[`)
	bracket_end := typ.index_u8(`]`)
	if bracket <= 0 || bracket_end <= bracket {
		return false
	}
	inner := typ[bracket + 1..bracket_end].trim_space()
	return !is_decimal_int_literal(inner)
}

fn is_decimal_int_literal(s string) bool {
	if s.len == 0 {
		return false
	}
	for i in 0 .. s.len {
		if s[i] < `0` || s[i] > `9` {
			return false
		}
	}
	return true
}

fn is_bare_generic_param(typ string) bool {
	return typ.len == 1 && typ[0] >= `A` && typ[0] <= `Z`
}

fn (tc &TypeChecker) type_name_known(typ string) bool {
	if is_builtin_type_name(typ) || typ == 'unknown' || typ.starts_with('C.') {
		return true
	}
	qtyp := tc.qualify_name(typ)
	return typ in tc.type_aliases || qtyp in tc.type_aliases || typ in tc.structs
		|| qtyp in tc.structs || typ in tc.interface_names || qtyp in tc.interface_names
		|| typ in tc.enum_names || qtyp in tc.enum_names || typ in tc.sum_types
		|| qtyp in tc.sum_types
}

fn should_check_named_type(typ string) bool {
	if typ.len == 0 {
		return false
	}
	for i in 0 .. typ.len {
		c := typ[i]
		if !((c >= `a` && c <= `z`) || (c >= `A` && c <= `Z`)
			|| (c >= `0` && c <= `9`) || c == `_` || c == `.`) {
			return false
		}
	}
	return true
}

fn (mut tc TypeChecker) check_struct_field_defaults(node flat.Node) {
	for i in 0 .. node.children_count {
		field := tc.a.child_node(&node, i)
		if field.kind != .field_decl || field.children_count == 0 {
			continue
		}
		default_id := tc.a.child(field, 0)
		expected := tc.parse_type(field.typ)
		tc.check_node(default_id)
		actual := tc.resolve_expr(default_id, expected)
		if !tc.type_compatible(actual, expected) {
			tc.type_mismatch(.assignment_mismatch,
				'cannot initialize field `${field.value}` with `${actual.name()}`; expected `${expected.name()}`',
				default_id)
		}
	}
}

fn (mut tc TypeChecker) check_enum_field_values(node flat.Node) {
	for i in 0 .. node.children_count {
		field := tc.a.child_node(&node, i)
		if field.kind != .enum_field || field.children_count == 0 {
			continue
		}
		value_id := tc.a.child(field, 0)
		tc.check_node(value_id)
		value_type := tc.resolve_type(value_id)
		if value_type is Unknown {
			continue
		}
		if !value_type.is_integer() {
			tc.type_mismatch(.assignment_mismatch,
				'enum field `${field.value}` value must be integer, not `${value_type.name()}`',
				value_id)
		}
	}
}

fn (mut tc TypeChecker) check_const_field_values(node flat.Node) {
	for i in 0 .. node.children_count {
		field := tc.a.child_node(&node, i)
		if field.kind != .const_field || field.children_count == 0 {
			continue
		}
		tc.check_node(tc.a.child(field, 0))
	}
}

fn (tc &TypeChecker) fn_body_definitely_returns(node flat.Node) bool {
	for i in 0 .. node.children_count {
		child_id := tc.a.child(&node, i)
		child := tc.a.child_node(&node, i)
		if child.kind == .param {
			continue
		}
		if tc.stmt_definitely_returns(child_id) {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) valid_node_id(id flat.NodeId) bool {
	return int(id) >= 0 && tc.a != unsafe { nil } && int(id) < tc.a.nodes.len
}

fn (tc &TypeChecker) stmt_definitely_returns(id flat.NodeId) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.nodes[int(id)]
	match node.kind {
		.return_stmt {
			return true
		}
		.block {
			for i in 0 .. node.children_count {
				if tc.stmt_definitely_returns(tc.a.child(&node, i)) {
					return true
				}
			}
			return false
		}
		.if_expr {
			if node.children_count < 3 {
				return false
			}
			return tc.stmt_definitely_returns(tc.a.child(&node, 1))
				&& tc.stmt_definitely_returns(tc.a.child(&node, 2))
		}
		.match_stmt {
			if node.children_count < 2 {
				return false
			}
			mut has_else := false
			for i in 1 .. node.children_count {
				branch := tc.a.child_node(&node, i)
				if branch.kind != .match_branch {
					return false
				}
				if branch.value == 'else' {
					has_else = true
				}
				if !tc.match_branch_definitely_returns(branch) {
					return false
				}
			}
			return has_else
		}
		else {
			return false
		}
	}
}

fn (tc &TypeChecker) match_branch_definitely_returns(branch &flat.Node) bool {
	body_start := if branch.value == 'else' { 0 } else { branch.value.int() }
	for i in body_start .. branch.children_count {
		if tc.stmt_definitely_returns(tc.a.child(branch, i)) {
			return true
		}
	}
	return false
}

fn node_kind_id(node flat.Node) int {
	mut kind_id := node.kind_id
	if kind_id == 0 && int(node.kind) != 0 {
		kind_id = int(node.kind)
	}
	return kind_id
}

fn (mut tc TypeChecker) check_node(id flat.NodeId) {
	idx := int(id)
	if idx < 0 {
		return
	}
	if idx >= tc.checking_nodes.len {
		tc.reset_node_caches(tc.a.nodes.len)
	}
	if idx < tc.checking_nodes.len {
		if tc.checking_nodes[idx] {
			return
		}
		tc.checking_nodes[idx] = true
		defer {
			tc.checking_nodes[idx] = false
		}
	}
	node := tc.a.nodes[idx]
	kind_id := node_kind_id(node)
	if kind_id == 1 || kind_id == 2 || kind_id == 3 || kind_id == 4 || kind_id == 5 || kind_id == 28
		|| kind_id == 29 {
		return
	}
	if kind_id == 45 {
		tc.check_block(node)
		return
	}
	if kind_id == 46 {
		tc.check_for_stmt(node)
		return
	}
	if kind_id == 47 {
		tc.check_for_in_stmt(node)
		return
	}
	if kind_id == 41 {
		tc.check_decl_assign(id, node)
		return
	}
	if kind_id == 40 || kind_id == 42 || kind_id == 43 {
		tc.check_assign(id, node)
		return
	}
	if kind_id == 44 {
		tc.check_return(id, node)
		return
	}
	if kind_id == 12 {
		tc.check_call(id, node)
		return
	}
	if kind_id == 21 {
		tc.check_fn_literal(node)
		return
	}
	if kind_id == 32 {
		tc.check_lambda_expr(node)
		return
	}
	if kind_id == 15 {
		tc.check_if_expr(id, node)
		return
	}
	if kind_id == 22 {
		tc.check_or_expr(node)
		return
	}
	if kind_id == 50 {
		tc.check_match_stmt(id, node)
		return
	}
	if kind_id == 37 {
		tc.check_is_expr(id, node)
		return
	}
	if kind_id == 10 {
		tc.check_postfix(id, node)
		return
	}
	if kind_id == 16 || kind_id == 26 {
		tc.check_struct_init(id, node)
		return
	}
	if kind_id == 13 {
		tc.check_selector(id, node)
		return
	}
	if kind_id == 14 {
		tc.check_index(id, node)
		return
	}
	if kind_id == 7 {
		tc.check_ident(id, node)
		return
	}

	for i in 0 .. node.children_count {
		tc.check_node(tc.a.child(&node, i))
	}
}

fn (mut tc TypeChecker) check_or_expr(node flat.Node) {
	if node.children_count == 0 {
		return
	}
	tc.check_node(tc.a.child(&node, 0))
	if node.children_count < 2 || node.value in ['!', '?'] {
		return
	}
	tc.push_scope()
	tc.cur_scope.insert('err', tc.parse_type('IError'))
	tc.check_node(tc.a.child(&node, 1))
	tc.pop_scope()
}

fn (mut tc TypeChecker) check_fn_literal(node flat.Node) {
	saved_ret := tc.cur_fn_ret_type
	tc.cur_fn_ret_type = tc.parse_type(node.typ)
	tc.push_scope()
	for i in 0 .. node.children_count {
		child := tc.a.child_node(&node, i)
		if child.kind == .param && child.value.len > 0 {
			tc.cur_scope.insert(child.value, tc.parse_type(child.typ))
		}
	}
	for i in 0 .. node.children_count {
		child_id := tc.a.child(&node, i)
		child := tc.a.nodes[int(child_id)]
		if child.kind == .param || child.kind == .ident {
			continue
		}
		tc.check_node(child_id)
	}
	tc.pop_scope()
	tc.cur_fn_ret_type = saved_ret
}

fn (mut tc TypeChecker) check_lambda_expr(node flat.Node) {
	if node.children_count == 0 {
		return
	}
	tc.push_scope()
	for i in 0 .. node.children_count - 1 {
		child := tc.a.child_node(&node, i)
		if child.kind == .ident && child.value.len > 0 {
			tc.cur_scope.insert(child.value, unknown_type('lambda parameter `${child.value}`'))
		}
	}
	tc.check_node(tc.a.child(&node, node.children_count - 1))
	tc.pop_scope()
}

fn (mut tc TypeChecker) check_block(node flat.Node) {
	tc.push_scope()
	for i in 0 .. node.children_count {
		tc.check_node(tc.a.child(&node, i))
	}
	tc.pop_scope()
}

fn (mut tc TypeChecker) check_for_stmt(node flat.Node) {
	tc.push_scope()
	if node.children_count > 0 {
		init_id := tc.a.child(&node, 0)
		if int(init_id) >= 0 {
			tc.check_node(init_id)
		}
	}
	if node.children_count > 1 {
		cond_id := tc.a.child(&node, 1)
		if int(cond_id) >= 0 {
			tc.check_bool_condition(cond_id)
		}
	}
	if node.children_count > 2 {
		post_id := tc.a.child(&node, 2)
		if int(post_id) >= 0 {
			tc.check_node(post_id)
		}
	}
	for i in 3 .. node.children_count {
		tc.check_node(tc.a.child(&node, i))
	}
	tc.pop_scope()
}

fn (mut tc TypeChecker) check_for_in_stmt(node flat.Node) {
	header := node.value.int()
	if header < 3 || node.children_count < 3 {
		return
	}
	tc.push_scope()
	key_id := tc.a.child(&node, 0)
	val_id := tc.a.child(&node, 1)
	container_id := tc.a.child(&node, 2)
	tc.check_node(container_id)
	has_val := int(val_id) >= 0
	if header == 4 {
		tc.insert_loop_var(key_id, Type(int_))
		tc.check_node(tc.a.child(&node, 3))
	} else {
		clean := unwrap_pointer(tc.resolve_type(container_id))
		if clean is Array {
			if has_val {
				tc.insert_loop_var(key_id, Type(int_))
				tc.insert_loop_var(val_id, clean.elem_type)
			} else {
				tc.insert_loop_var(key_id, clean.elem_type)
			}
		} else if clean is ArrayFixed {
			if has_val {
				tc.insert_loop_var(key_id, Type(int_))
				tc.insert_loop_var(val_id, clean.elem_type)
			} else {
				tc.insert_loop_var(key_id, clean.elem_type)
			}
		} else if clean is Map {
			if has_val {
				tc.insert_loop_var(key_id, clean.key_type)
				tc.insert_loop_var(val_id, clean.value_type)
			} else {
				tc.insert_loop_var(key_id, clean.value_type)
			}
		} else if clean is String {
			if has_val {
				tc.insert_loop_var(key_id, Type(int_))
				tc.insert_loop_var(val_id, Type(u8_))
			} else {
				tc.insert_loop_var(key_id, Type(u8_))
			}
		} else {
			container := tc.a.nodes[int(container_id)]
			if container.kind == .range {
				tc.insert_loop_var(key_id, Type(int_))
			} else if tc.should_diagnose(container_id) {
				tc.record_error(.cannot_index, 'cannot iterate over `${clean.name()}`',
					container_id)
			}
		}
	}
	for i in header .. node.children_count {
		tc.check_node(tc.a.child(&node, i))
	}
	tc.pop_scope()
}

fn (mut tc TypeChecker) check_decl_assign(id flat.NodeId, node flat.Node) {
	if node.children_count == 0 {
		return
	}
	if tc.check_multi_return_decl_assign(id, node) {
		return
	}
	mut i := 0
	for i + 1 < node.children_count {
		lhs_id := tc.a.child(&node, i)
		rhs_id := tc.a.child(&node, i + 1)
		tc.check_node(rhs_id)
		mut rhs_type := tc.resolve_type(rhs_id)
		mut expected := rhs_type
		if node.children_count == 2 && node.typ.len > 0 {
			expected = tc.parse_type(node.typ)
			rhs_type = tc.resolve_expr(rhs_id, expected)
			if !tc.type_compatible(rhs_type, expected) {
				tc.type_mismatch(.assignment_mismatch,
					'cannot assign `${rhs_type.name()}` to `${expected.name()}`', id)
			}
		}
		tc.insert_decl_lhs(lhs_id, expected)
		i += 2
	}
}

fn (mut tc TypeChecker) check_multi_return_decl_assign(id flat.NodeId, node flat.Node) bool {
	if node.children_count < 3 {
		return false
	}
	rhs_id := tc.a.child(&node, 1)
	rhs_type := tc.resolve_type(rhs_id)
	rhs_type_name := rhs_type.name()
	if rhs_type is MultiReturn {
		tc.check_node(rhs_id)
		lhs_ids := tc.multi_assign_lhs_ids(node)
		if lhs_ids.len != rhs_type.types.len {
			if tc.should_diagnose(id) {
				tc.record_error(.assignment_mismatch,
					'multi-return assignment mismatch: ${lhs_ids.len} variables but `${rhs_type_name}` has ${rhs_type.types.len} values',
					id)
			}
			return true
		}
		for i, lhs_id in lhs_ids {
			tc.insert_decl_lhs(lhs_id, rhs_type.types[i])
		}
		return true
	}
	return false
}

fn (tc &TypeChecker) multi_assign_lhs_ids(node flat.Node) []flat.NodeId {
	mut lhs_ids := []flat.NodeId{}
	if node.children_count > 0 {
		lhs_ids << tc.a.child(&node, 0)
	}
	for i in 2 .. node.children_count {
		lhs_ids << tc.a.child(&node, i)
	}
	return lhs_ids
}

fn (mut tc TypeChecker) insert_decl_lhs(lhs_id flat.NodeId, typ Type) {
	if int(lhs_id) < 0 || typ is Void {
		return
	}
	lhs := tc.a.nodes[int(lhs_id)]
	if lhs.kind == .ident && lhs.value.len > 0 {
		tc.cur_scope.insert(lhs.value, typ)
		tc.register_synth_type(lhs_id, typ)
	}
}

fn (mut tc TypeChecker) check_assign(id flat.NodeId, node flat.Node) {
	if node.children_count < 2 {
		return
	}
	if node.kind == .index_assign && tc.reject_unlowered_map_mutation
		&& tc.index_assign_lhs_is_map(node) {
		if tc.should_diagnose(id) {
			tc.record_error(.assignment_mismatch,
				'internal compiler error: unlowered map index assignment reached post-transform checker',
				id)
		}
		for i := 1; i < node.children_count; i += 2 {
			tc.check_node(tc.a.child(&node, i))
		}
		return
	}
	if tc.check_multi_return_assign(id, node) {
		return
	}
	mut i := 0
	for i + 1 < node.children_count {
		lhs_id := tc.a.child(&node, i)
		rhs_id := tc.a.child(&node, i + 1)
		lhs_type := tc.resolve_lvalue_type(lhs_id)
		tc.check_node(rhs_id)
		rhs_type := tc.resolve_expr(rhs_id, lhs_type)
		if !tc.type_compatible(rhs_type, lhs_type) {
			tc.type_mismatch(.assignment_mismatch,
				'cannot assign `${rhs_type.name()}` to `${lhs_type.name()}`', id)
		}
		i += 2
	}
}

fn (tc &TypeChecker) index_assign_lhs_is_map(node flat.Node) bool {
	if node.children_count == 0 {
		return false
	}
	lhs_id := tc.a.child(&node, 0)
	if int(lhs_id) < 0 {
		return false
	}
	lhs := tc.a.nodes[int(lhs_id)]
	if lhs.kind != .index || lhs.children_count < 2 {
		return false
	}
	base_type := unwrap_pointer(tc.resolve_type(tc.a.child(&lhs, 0)))
	return base_type is Map
}

fn (mut tc TypeChecker) check_multi_return_assign(id flat.NodeId, node flat.Node) bool {
	if node.children_count < 3 {
		return false
	}
	rhs_id := tc.a.child(&node, 1)
	rhs_type := tc.resolve_type(rhs_id)
	rhs_type_name := rhs_type.name()
	if rhs_type is MultiReturn {
		tc.check_node(rhs_id)
		lhs_ids := tc.multi_assign_lhs_ids(node)
		if lhs_ids.len != rhs_type.types.len {
			if tc.should_diagnose(id) {
				tc.record_error(.assignment_mismatch,
					'multi-return assignment mismatch: ${lhs_ids.len} variables but `${rhs_type_name}` has ${rhs_type.types.len} values',
					id)
			}
			return true
		}
		for i, lhs_id in lhs_ids {
			lhs_type := tc.resolve_lvalue_type(lhs_id)
			if !tc.type_compatible(rhs_type.types[i], lhs_type) {
				tc.type_mismatch(.assignment_mismatch,
					'cannot assign `${rhs_type.types[i].name()}` to `${lhs_type.name()}`', id)
			}
		}
		return true
	}
	return false
}

fn (mut tc TypeChecker) check_postfix(id flat.NodeId, node flat.Node) {
	if node.children_count == 0 {
		return
	}
	child_id := tc.a.child(&node, 0)
	tc.check_node(child_id)
	child := tc.a.nodes[int(child_id)]
	if child.kind == .index && child.children_count >= 2 {
		base_type := unwrap_pointer(tc.resolve_type(tc.a.child(&child, 0)))
		if base_type is Map && node.op in [.inc, .dec] && tc.reject_unlowered_map_mutation
			&& tc.should_diagnose(id) {
			tc.record_error(.assignment_mismatch,
				'internal compiler error: unlowered map index postfix mutation reached post-transform checker',
				id)
		}
	}
}

fn (mut tc TypeChecker) resolve_lvalue_type(lhs_id flat.NodeId) Type {
	if int(lhs_id) < 0 {
		return Type(void_)
	}
	lhs := tc.a.nodes[int(lhs_id)]
	if lhs.kind == .ident {
		if typ := tc.cur_scope.lookup(lhs.value) {
			return typ
		}
		if typ := tc.file_scope.lookup(lhs.value) {
			return typ
		}
		if tc.should_diagnose(lhs_id) && lhs.value != '_' {
			tc.record_error(.unknown_ident, 'unknown identifier `${lhs.value}`', lhs_id)
		}
		return unknown_type('unknown identifier `${lhs.value}`')
	}
	if lhs.kind == .selector {
		tc.check_selector(lhs_id, lhs)
		return tc.resolve_type(lhs_id)
	}
	if lhs.kind == .index {
		tc.check_index(lhs_id, lhs)
		return tc.resolve_type(lhs_id)
	}
	return tc.resolve_type(lhs_id)
}

fn (mut tc TypeChecker) check_return(id flat.NodeId, node flat.Node) {
	expected := tc.cur_fn_ret_type
	if expected is Void {
		if node.children_count > 0 && tc.should_diagnose(id) {
			tc.record_error(.return_mismatch, 'void function should not return a value', id)
		}
		for i in 0 .. node.children_count {
			tc.check_node(tc.a.child(&node, i))
		}
		return
	}
	if node.children_count == 0 {
		if tc.should_diagnose(id) {
			tc.record_error(.return_mismatch, 'missing return value of type `${expected.name()}`',
				id)
		}
		return
	}
	if expected is MultiReturn {
		if node.children_count != expected.types.len {
			if tc.should_diagnose(id) {
				tc.record_error(.return_mismatch,
					'return value count mismatch: expected ${expected.types.len}, got ${node.children_count}',
					id)
			}
			return
		}
		for i in 0 .. node.children_count {
			child_id := tc.a.child(&node, i)
			tc.check_node(child_id)
			actual := tc.resolve_expr(child_id, expected.types[i])
			if !tc.type_compatible(actual, expected.types[i]) {
				tc.type_mismatch(.return_mismatch,
					'cannot return `${actual.name()}` as `${expected.types[i].name()}`', id)
			}
		}
		return
	}
	if node.children_count != 1 {
		if tc.should_diagnose(id) {
			tc.record_error(.return_mismatch,
				'return value count mismatch: expected 1, got ${node.children_count}', id)
		}
		return
	}
	child_id := tc.a.child(&node, 0)
	tc.check_node(child_id)
	actual := tc.resolve_expr(child_id, expected)
	if !tc.type_compatible(actual, expected) {
		tc.type_mismatch(.return_mismatch,
			'cannot return `${actual.name()}` as `${expected.name()}`', id)
	}
}

fn (mut tc TypeChecker) check_call(id flat.NodeId, node flat.Node) {
	if info := tc.resolve_call_info(id, node) {
		if info.name.len > 0 && !is_array_dsl_call_name(info.name) {
			tc.remember_resolved_call(id, info.name)
		}
		if info.return_type !is Void && info.return_type !is Unknown {
			tc.remember_expr_type(id, info.return_type)
		}
		tc.check_call_arg_types(id, node, info)
		return
	}
	if tc.should_diagnose(id) && !tc.is_known_call(node) {
		tc.record_error(.unknown_fn, 'unknown function `${tc.call_display_name(node)}`', id)
	}
	for i in 1 .. node.children_count {
		tc.check_node(tc.call_arg_value(tc.a.child(&node, i)))
	}
}

fn (tc &TypeChecker) should_diagnose(id flat.NodeId) bool {
	if int(id) < 0 || int(id) < tc.a.user_code_start {
		return false
	}
	if int(id) < tc.a.nodes.len && !tc.a.nodes[int(id)].pos.is_valid() && !tc.diagnose_unknown_calls {
		return false
	}
	if tc.diagnostic_files.len == 0 {
		return true
	}
	return tc.cur_file in tc.diagnostic_files
}

fn (tc &TypeChecker) should_diagnose_unknown_call(id flat.NodeId) bool {
	return tc.diagnose_unknown_calls && tc.should_diagnose(id)
}

fn (mut tc TypeChecker) resolve_call_info(_id flat.NodeId, node flat.Node) ?CallInfo {
	if node.children_count == 0 {
		return none
	}
	fn_node := tc.a.child_node(&node, 0)
	if fn_node.kind == .selector {
		base_id := tc.a.child(fn_node, 0)
		base_node := tc.a.nodes[int(base_id)]
		if base_node.kind == .ident && base_node.value == 'C' {
			return none
		}
		if base_node.kind == .ident {
			if resolved_mod := tc.resolve_import_alias(base_node.value) {
				mod_name := '${resolved_mod}.${fn_node.value}'
				if mod_name in tc.fn_ret_types {
					return tc.call_info(mod_name, false)
				}
			}
			qbase := tc.qualify_name(base_node.value)
			static_name := '${qbase}.${fn_node.value}'
			if static_name in tc.fn_ret_types && (qbase in tc.structs
				|| qbase in tc.enum_names || qbase in tc.sum_types
				|| qbase in tc.interface_names) {
				return tc.call_info(static_name, false)
			}
			if fn_node.value == 'zero' && qbase in tc.flag_enums {
				return CallInfo{
					name:         ''
					params:       []Type{}
					return_type:  Type(Enum{
						name:    qbase
						is_flag: true
					})
					params_known: true
				}
			}
		} else if base_node.kind == .selector {
			if method_name := tc.module_const_receiver_method_name(base_node, fn_node.value) {
				return tc.call_info(method_name, true)
			}
			inner := tc.a.child_node(base_node, 0)
			if inner.kind == .ident {
				mod_name := tc.resolve_import_alias(inner.value) or { inner.value }
				full_name := '${mod_name}.${base_node.value}.${fn_node.value}'
				if full_name in tc.fn_ret_types {
					return tc.call_info(full_name, false)
				}
			}
		}
		if fn_typ := tc.selector_fn_type(fn_node) {
			return CallInfo{
				name:         ''
				params:       fn_typ.params.clone()
				return_type:  fn_typ.return_type
				params_known: true
			}
		}
		base_type := tc.resolve_type(base_id)
		clean := unwrap_pointer(base_type)
		if clean is Channel && fn_node.value == 'close' {
			return CallInfo{
				name:         'chan.close'
				params:       tarr1(base_type)
				return_type:  Type(void_)
				has_receiver: true
				params_known: true
			}
		}
		if clean is Array && fn_node.value == 'clone' {
			return CallInfo{
				name:         'array_clone'
				params:       tarr1(base_type)
				return_type:  base_type
				has_receiver: true
				params_known: true
			}
		}
		if clean is Map && fn_node.value == 'clone' {
			return CallInfo{
				name:         ''
				params:       tarr1(base_type)
				return_type:  base_type
				has_receiver: true
				params_known: true
			}
		}
		if clean is Array {
			match fn_node.value {
				'first', 'last', 'pop' {
					return CallInfo{
						name:         ''
						params:       tarr1(base_type)
						return_type:  clean.elem_type
						has_receiver: true
						params_known: true
					}
				}
				'contains' {
					return CallInfo{
						name:         ''
						params:       tarr2(base_type, clean.elem_type)
						return_type:  Type(bool_)
						has_receiver: true
						params_known: true
					}
				}
				'index', 'last_index' {
					return CallInfo{
						name:         ''
						params:       tarr2(base_type, clean.elem_type)
						return_type:  Type(int_)
						has_receiver: true
						params_known: true
					}
				}
				'repeat' {
					return CallInfo{
						name:         'array.repeat_to_depth'
						params:       tarr2(base_type, Type(int_))
						return_type:  base_type
						has_receiver: true
						params_known: true
					}
				}
				'repeat_to_depth' {
					return CallInfo{
						name:         'array.repeat_to_depth'
						params:       tarr3(base_type, Type(int_), Type(int_))
						return_type:  base_type
						has_receiver: true
						params_known: true
					}
				}
				'delete' {
					return CallInfo{
						name:         ''
						params:       tarr2(Type(Pointer{
							base_type: base_type
						}), Type(int_))
						return_type:  Type(void_)
						has_receiver: true
						params_known: true
					}
				}
				'delete_last', 'clear' {
					return CallInfo{
						name:         ''
						params:       tarr1(Type(Pointer{
							base_type: base_type
						}))
						return_type:  Type(void_)
						has_receiver: true
						params_known: true
					}
				}
				'filter' {
					return CallInfo{
						name:         'array.filter'
						params:       tarr2(base_type, Type(bool_))
						return_type:  base_type
						has_receiver: true
						params_known: true
					}
				}
				'map' {
					elem_type := tc.array_map_return_elem_type(node)
					return CallInfo{
						name:         'array.map'
						params:       tarr2(base_type, elem_type)
						return_type:  Type(Array{
							elem_type: elem_type
						})
						has_receiver: true
						params_known: true
					}
				}
				'any', 'all' {
					return CallInfo{
						name:         'array.${fn_node.value}'
						params:       tarr2(base_type, Type(bool_))
						return_type:  Type(bool_)
						has_receiver: true
						params_known: true
					}
				}
				'count' {
					return CallInfo{
						name:         'array.count'
						params:       tarr2(base_type, Type(bool_))
						return_type:  Type(int_)
						has_receiver: true
						params_known: true
					}
				}
				'sort' {
					mut params := tarr1(Type(Pointer{
						base_type: base_type
					}))
					if call_explicit_arg_count(node) > 0 {
						params << Type(bool_)
					}
					return CallInfo{
						name:         'array.sort'
						params:       params
						return_type:  Type(void_)
						has_receiver: true
						params_known: true
					}
				}
				'sorted' {
					mut params := tarr1(base_type)
					if call_explicit_arg_count(node) > 0 {
						params << Type(bool_)
					}
					return CallInfo{
						name:         'array.sorted'
						params:       params
						return_type:  base_type
						has_receiver: true
						params_known: true
					}
				}
				else {}
			}
		}
		type_name := resolve_type_name_for_method(clean)
		if type_name.len > 0 {
			mname := '${type_name}.${fn_node.value}'
			if mname in tc.fn_ret_types {
				return tc.call_info(mname, true)
			}
		}
		if clean is SumType {
			mname := '${clean.name}.${fn_node.value}'
			if mname in tc.fn_ret_types {
				return tc.call_info(mname, true)
			}
		}
		if clean is Enum {
			if clean.is_flag && fn_node.value in ['has', 'all'] {
				return CallInfo{
					name:         ''
					params:       tarr2(base_type, base_type)
					return_type:  Type(bool_)
					has_receiver: true
					params_known: true
				}
			}
			if fn_node.value == 'str' {
				return CallInfo{
					name:         '${clean.name}.str'
					params:       tarr1(base_type)
					return_type:  Type(string_)
					has_receiver: true
					params_known: true
				}
			}
			mname := '${clean.name}.${fn_node.value}'
			if mname in tc.fn_ret_types {
				return tc.call_info(mname, true)
			}
		}
		return none
	}
	if fn_node.kind == .ident {
		if fn_node.value == 'error' || fn_node.value == 'error_with_code' {
			mut params := []Type{}
			if fn_node.value == 'error_with_code' {
				params = tarr2(Type(string_), Type(int_))
			} else {
				params = tarr1(Type(string_))
			}
			return CallInfo{
				name:         fn_node.value
				params:       params
				return_type:  tc.parse_type('IError')
				params_known: true
			}
		}
		if typ := tc.cur_scope.lookup(fn_node.value) {
			if fn_typ := fn_type_from_type(typ) {
				return CallInfo{
					name:         ''
					params:       fn_typ.params
					return_type:  fn_typ.return_type
					params_known: true
				}
			}
		}
		qfn := tc.qualify_fn_name(fn_node.value)
		if qfn in tc.fn_ret_types {
			return tc.call_info(qfn, false)
		}
		if fn_node.value in tc.fn_ret_types {
			return tc.call_info(fn_node.value, false)
		}
	}
	return none
}

fn (tc &TypeChecker) call_info(name string, has_receiver bool) CallInfo {
	mut params := []Type{}
	mut params_known := false
	if p := tc.fn_param_types[name] {
		params = p.clone()
		params_known = true
	}
	if is_print_style_fn_name(name) && params.len == 1
		&& print_style_param_accepts_string(params[0]) {
		params[0] = unknown_type('print argument')
	}
	return CallInfo{
		name:         name
		params:       params
		return_type:  tc.fn_ret_types[name] or { unknown_type('unknown return type for `${name}`') }
		has_receiver: has_receiver
		is_variadic:  tc.fn_variadic[name] or { false }
		params_known: params_known
	}
}

fn is_print_style_fn_name(name string) bool {
	return name in ['print', 'println', 'eprint', 'eprintln', 'builtin.print', 'builtin.println',
		'builtin.eprint', 'builtin.eprintln']
}

fn print_style_param_accepts_string(typ Type) bool {
	mut clean := typ
	for _ in 0 .. 8 {
		if clean is Alias {
			clean = clean.base_type
			continue
		}
		break
	}
	return clean is String
}

fn (mut tc TypeChecker) check_call_arg_types(id flat.NodeId, node flat.Node, info CallInfo) {
	if node.children_count == 0 {
		return
	}
	if info.name.starts_with('map.') {
		for i in 1 .. node.children_count {
			tc.check_node(tc.call_arg_value(tc.a.child(&node, i)))
		}
		return
	}
	if !info.params_known {
		for i in 1 .. node.children_count {
			tc.check_node(tc.call_arg_value(tc.a.child(&node, i)))
		}
		return
	}
	// `@[params]` struct args: trailing `key: value` args collapse into one struct argument.
	// field_init args only appear for this syntax, so they are a reliable signal.
	mut field_init_args := 0
	for i in 1 .. node.children_count {
		if tc.a.child_node(&node, i).kind == .field_init {
			field_init_args++
		}
	}
	collapsed := if field_init_args > 0 { 1 } else { 0 }
	recv_extra := if info.has_receiver { 1 } else { 0 }
	actual_count := node.children_count - 1 - field_init_args + collapsed + recv_extra
	min_count := tc.min_required_arg_count(info)
	if actual_count < min_count || (!info.is_variadic && actual_count > info.params.len) {
		if tc.should_diagnose(id) {
			tc.record_error(.call_arg_mismatch,
				'argument count mismatch for `${tc.call_display_name(node)}`: expected ${info.params.len}, got ${actual_count}',
				id)
		}
		for i in 1 .. node.children_count {
			tc.check_node(tc.call_arg_value(tc.a.child(&node, i)))
		}
		return
	}
	if info.has_receiver && info.params.len > 0 {
		fn_node := tc.a.child_node(&node, 0)
		recv_id := tc.a.child(fn_node, 0)
		tc.check_node(recv_id)
		recv_type := tc.resolve_expr(recv_id, info.params[0])
		if !tc.receiver_compatible(recv_type, info.params[0]) {
			tc.type_mismatch(.call_arg_mismatch,
				'cannot use receiver `${recv_type.name()}` as `${info.params[0].name()}`', id)
		}
	}
	for i in 1 .. node.children_count {
		arg_id := tc.call_arg_value(tc.a.child(&node, i))
		// field_init args are fields of the collapsed `@[params]` struct, not positional params
		if tc.a.child_node(&node, i).kind == .field_init {
			tc.check_node(arg_id)
			continue
		}
		param_idx := if info.has_receiver { i } else { i - 1 }
		has_dsl_scope := tc.call_arg_needs_array_dsl_scope(info.name, param_idx)
		if has_dsl_scope {
			tc.push_array_dsl_scope(node, info.name)
			tc.check_node(arg_id)
		} else {
			tc.check_node(arg_id)
		}
		if param_idx >= info.params.len {
			if info.is_variadic && info.params.len > 0 {
				variadic_raw := info.params[info.params.len - 1]
				if variadic_raw is Array {
					elem_type := array_elem_type(variadic_raw)
					actual := tc.resolve_expr(arg_id, elem_type)
					if !tc.receiver_compatible(actual, elem_type) {
						tc.type_mismatch(.call_arg_mismatch, 'cannot use `${actual.name()}` as argument ${
							param_idx + 1} to `${tc.call_display_name(node)}`; expected `${elem_type.name()}`',
							id)
					}
				}
			}
			if has_dsl_scope {
				tc.pop_scope()
			}
			continue
		}
		mut expected := info.params[param_idx]
		if tc.is_zero_literal(arg_id) && is_fn_pointer_type(expected) {
			if has_dsl_scope {
				tc.pop_scope()
			}
			continue
		}
		expected_raw := expected
		if info.is_variadic && param_idx == info.params.len - 1 && expected_raw is Array {
			elem_type := array_elem_type(expected_raw)
			actual := tc.resolve_expr(arg_id, expected)
			actual_name := actual.name()
			expected_name := '[]${elem_type.name()}'
			actual_raw := actual
			if actual is Array {
				if !tc.receiver_compatible(actual_raw, expected) {
					tc.type_mismatch(.call_arg_mismatch, 'cannot use `${actual_name}` as argument ${
						param_idx + 1} to `${tc.call_display_name(node)}`; expected `${expected_name}`',
						id)
				}
				if has_dsl_scope {
					tc.pop_scope()
				}
				continue
			}
			expected = elem_type
		}
		mut actual := Type(void_)
		if has_dsl_scope {
			actual = tc.resolve_expr(arg_id, expected)
			tc.pop_scope()
		} else {
			actual = tc.resolve_expr(arg_id, expected)
		}
		if !tc.receiver_compatible(actual, expected) {
			tc.type_mismatch(.call_arg_mismatch, 'cannot use `${actual.name()}` as argument ${
				param_idx + 1} to `${tc.call_display_name(node)}`; expected `${expected.name()}`',
				id)
		}
	}
}

fn (mut tc TypeChecker) array_map_return_elem_type(node flat.Node) Type {
	if node.children_count < 2 {
		return Type(void_)
	}
	tc.push_array_dsl_scope(node, 'array.map')
	arg_id := tc.call_arg_value(tc.a.child(&node, 1))
	elem_type := tc.resolve_type(arg_id)
	tc.pop_scope()
	if elem_type is Void || elem_type is Unknown {
		return Type(void_)
	}
	return elem_type
}

fn (tc &TypeChecker) min_required_arg_count(info CallInfo) int {
	if info.is_variadic && info.params.len > 0 {
		return info.params.len - 1
	}
	mut n := info.params.len
	for n > 0 {
		param := info.params[n - 1]
		if param is Struct && param.name.ends_with('Params') {
			n--
			continue
		}
		break
	}
	return n
}

fn (tc &TypeChecker) call_arg_needs_array_dsl_scope(name string, param_idx int) bool {
	return param_idx == 1 && is_array_dsl_call_name(name)
}

fn is_array_dsl_call_name(name string) bool {
	return name in ['array.filter', 'array.any', 'array.all', 'array.count', 'array.map',
		'array.sort', 'array.sorted']
}

fn call_explicit_arg_count(node flat.Node) int {
	if node.children_count <= 1 {
		return 0
	}
	mut n := 0
	for i in 1 .. node.children_count {
		if int(node.children_start) + i < 0 {
			continue
		}
		n++
	}
	return n
}

fn (mut tc TypeChecker) push_array_dsl_scope(node flat.Node, name string) {
	tc.push_scope()
	arr := tc.call_receiver_array_type(node) or { return }
	if name in ['array.sort', 'array.sorted'] {
		tc.cur_scope.insert('a', arr.elem_type)
		tc.cur_scope.insert('b', arr.elem_type)
		return
	}
	tc.cur_scope.insert('it', arr.elem_type)
}

fn (tc &TypeChecker) call_receiver_array_type(node flat.Node) ?Array {
	if node.children_count == 0 {
		return none
	}
	fn_node := tc.a.child_node(&node, 0)
	if fn_node.kind != .selector || fn_node.children_count == 0 {
		return none
	}
	base_type := unwrap_pointer(tc.resolve_type(tc.a.child(fn_node, 0)))
	if base_type is Array {
		return base_type
	}
	return none
}

fn (tc &TypeChecker) call_arg_value(id flat.NodeId) flat.NodeId {
	if int(id) < 0 {
		return id
	}
	node := tc.a.nodes[int(id)]
	if node.kind == .field_init && node.children_count > 0 {
		return tc.a.child(&node, 0)
	}
	return id
}

fn (tc &TypeChecker) receiver_compatible(actual Type, expected Type) bool {
	if tc.type_compatible(actual, expected) {
		return true
	}
	if expected is Pointer {
		return tc.type_compatible(actual, expected.base_type)
	}
	if actual is Pointer {
		return tc.type_compatible(actual.base_type, expected)
	}
	return false
}

fn (tc &TypeChecker) is_zero_literal(id flat.NodeId) bool {
	if int(id) < 0 {
		return false
	}
	node := tc.a.nodes[int(id)]
	return node.kind == .int_literal && node.value == '0'
}

fn is_fn_pointer_type(typ Type) bool {
	clean0 := typ
	mut clean := clean0
	if clean0 is Alias {
		clean = clean0.base_type
	}
	return clean is FnType
}

fn fn_type_from_type(typ Type) ?FnType {
	if typ is FnType {
		return typ
	}
	if typ is Alias {
		return fn_type_from_type(typ.base_type)
	}
	return none
}

fn (tc &TypeChecker) selector_fn_type(node flat.Node) ?FnType {
	if node.children_count == 0 {
		return none
	}
	if !valid_string_data(node.value) {
		return none
	}
	base_id := tc.a.child(&node, 0)
	base_type := tc.selector_fn_base_type(base_id) or { return none }
	clean0 := unwrap_pointer(base_type)
	mut clean := clean0
	if clean0 is Alias {
		clean = clean0.base_type
	}
	if clean is Struct {
		fields := tc.structs[clean.name] or { return none }
		for f in fields {
			if !valid_string_data(f.name) {
				continue
			}
			if f.name == node.value {
				return fn_type_from_type(f.typ)
			}
		}
	}
	if clean is Interface {
		if typ := tc.interface_field_type(clean.name, node.value) {
			return fn_type_from_type(typ)
		}
	}
	return none
}

fn (tc &TypeChecker) selector_fn_base_type(base_id flat.NodeId) ?Type {
	if typ := tc.cached_expr_type(base_id) {
		return typ
	}
	if int(base_id) < 0 {
		return none
	}
	base_node := tc.a.nodes[int(base_id)]
	if base_node.typ.len > 0 && base_node.typ != 'unknown' {
		return tc.parse_type(base_node.typ)
	}
	if base_node.kind == .call {
		if typ := tc.resolved_call_type(base_id) {
			return typ
		}
		if typ := tc.direct_call_return_type(base_node) {
			return typ
		}
		return none
	}
	return tc.resolve_type(base_id)
}

fn (tc &TypeChecker) direct_call_return_type(node flat.Node) ?Type {
	if node.children_count == 0 {
		return none
	}
	fn_node := tc.a.child_node(&node, 0)
	if fn_node.kind == .ident {
		qfn := tc.qualify_fn_name(fn_node.value)
		if typ := tc.fn_ret_types[qfn] {
			return typ
		}
		if typ := tc.fn_ret_types[fn_node.value] {
			return typ
		}
		return none
	}
	if fn_node.kind != .selector || fn_node.children_count == 0 {
		return none
	}
	base_node := tc.a.child_node(fn_node, 0)
	if base_node.kind == .ident {
		if resolved := tc.resolve_import_alias(base_node.value) {
			mod_name := '${resolved}.${fn_node.value}'
			if typ := tc.fn_ret_types[mod_name] {
				return typ
			}
		}
		qbase := tc.qualify_name(base_node.value)
		static_name := '${qbase}.${fn_node.value}'
		if static_name in tc.fn_ret_types && (qbase in tc.structs
			|| qbase in tc.enum_names || qbase in tc.sum_types
			|| qbase in tc.interface_names) {
			return tc.fn_ret_types[static_name] or { none }
		}
		return none
	}
	if base_node.kind == .selector {
		inner := tc.a.child_node(base_node, 0)
		if inner.kind == .ident {
			mod_name := tc.resolve_import_alias(inner.value) or { inner.value }
			full_name := '${mod_name}.${base_node.value}.${fn_node.value}'
			if typ := tc.fn_ret_types[full_name] {
				return typ
			}
		}
	}
	return none
}

fn (tc &TypeChecker) module_const_receiver_method_name(base_node flat.Node, method string) ?string {
	if base_node.kind != .selector || base_node.children_count == 0 || method.len == 0 {
		return none
	}
	inner := tc.a.child_node(&base_node, 0)
	if inner.kind != .ident {
		return none
	}
	mod_name := tc.resolve_import_alias(inner.value) or { inner.value }
	const_name := '${mod_name}.${base_node.value}'
	mut const_type := tc.const_types[const_name] or { Type(Unknown{}) }
	const_type = tc.const_type_from_initializer(const_name, const_type)
	if const_type is Unknown && base_node.value == 'scanner_matcher' {
		const_type = Type(Struct{
			name: '${mod_name}.KeywordsMatcherTrie'
		})
	}
	clean := unwrap_pointer(const_type)
	type_name := resolve_type_name_for_method(clean)
	if type_name.len == 0 {
		return none
	}
	method_name := '${type_name}.${method}'
	if method_name in tc.fn_ret_types {
		return method_name
	}
	return none
}

fn valid_string_data(s string) bool {
	if s.len == 0 {
		return true
	}
	ptr := unsafe { u64(voidptr(s.str)) }
	return ptr >= 4096 && ptr < 281474976710656 && s.len < 1048576
}

fn clone_smartcasts(src map[string]Type) map[string]Type {
	mut dst := map[string]Type{}
	for key, typ in src {
		if valid_string_data(key) {
			dst[key] = typ
		}
	}
	return dst
}

fn array_elem_type(arr Array) Type {
	return arr.elem_type
}

fn fixed_array_elem_type(arr ArrayFixed) Type {
	return arr.elem_type
}

fn map_value_type(m Map) Type {
	return m.value_type
}

fn pointer_base_type(p Pointer) Type {
	return p.base_type
}

fn fn_param_type(f FnType, idx int) Type {
	return f.params[idx]
}

fn (tc &TypeChecker) is_known_call(node flat.Node) bool {
	if node.children_count == 0 {
		return true
	}
	if node.typ.len > 0 {
		return true
	}
	fn_node := tc.a.child_node(&node, 0)
	if fn_node.kind == .selector {
		base_node := tc.a.child_node(fn_node, 0)
		if base_node.kind == .ident {
			if base_node.value == 'C' {
				return true
			}
			if resolved_mod := tc.resolve_import_alias(base_node.value) {
				mod_name := '${resolved_mod}.${fn_node.value}'
				if mod_name in tc.fn_ret_types || mod_name in tc.sum_types || mod_name in tc.structs
					|| mod_name in tc.enum_names {
					return true
				}
			}
			if base_node.value in tc.structs || base_node.value in tc.enum_names {
				qname := tc.qualify_name(base_node.value)
				if '${qname}.${fn_node.value}' in tc.fn_ret_types {
					return true
				}
			} else {
				qname := tc.qualify_name(base_node.value)
				if qname in tc.structs || qname in tc.enum_names {
					if '${qname}.${fn_node.value}' in tc.fn_ret_types {
						return true
					}
				}
			}
		} else if base_node.kind == .selector {
			inner := tc.a.child_node(base_node, 0)
			if inner.kind == .ident {
				mod_name := tc.resolve_import_alias(inner.value) or { inner.value }
				if '${mod_name}.${base_node.value}.${fn_node.value}' in tc.fn_ret_types {
					return true
				}
			}
		}
		if _ := tc.selector_fn_type(fn_node) {
			return true
		}
		base_type := tc.resolve_type(tc.a.child(fn_node, 0))
		clean_type := unwrap_pointer(base_type)
		if clean_type is Array || clean_type is ArrayFixed || clean_type is Map {
			return true
		}
		if clean_type is String {
			return 'string.${fn_node.value}' in tc.fn_ret_types
		}
		if clean_type is Alias {
			mname := '${clean_type.name}.${fn_node.value}'
			if mname in tc.fn_ret_types {
				return true
			}
			base_name := resolve_type_name_for_method(clean_type.base_type)
			if base_name.len > 0 {
				return '${base_name}.${fn_node.value}' in tc.fn_ret_types
			}
		}
		if clean_type is Struct {
			return '${clean_type.name}.${fn_node.value}' in tc.fn_ret_types
		}
		if clean_type is Interface {
			return '${clean_type.name}.${fn_node.value}' in tc.fn_ret_types
		}
		if clean_type is SumType {
			return '${clean_type.name}.${fn_node.value}' in tc.fn_ret_types
		}
		if clean_type is Enum {
			if fn_node.value == 'str' {
				return true
			}
			return '${clean_type.name}.${fn_node.value}' in tc.fn_ret_types
		}
		if clean_type is Primitive {
			mname := '${prim_c_type_from(clean_type.props, clean_type.size)}.${fn_node.value}'
			return mname in tc.fn_ret_types
		}
		return false
	}
	if fn_node.kind == .ident {
		if typ := tc.cur_scope.lookup(fn_node.value) {
			return typ is FnType
		}
		qfn := tc.qualify_fn_name(fn_node.value)
		if qfn in tc.fn_ret_types || fn_node.value in tc.fn_ret_types {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) call_display_name(node flat.Node) string {
	if node.children_count == 0 {
		return '<missing>'
	}
	fn_node := tc.a.child_node(&node, 0)
	if fn_node.kind == .ident {
		return fn_node.value
	}
	if fn_node.kind == .selector && fn_node.children_count > 0 {
		base := tc.a.child_node(fn_node, 0)
		if base.value.len > 0 {
			return '${base.value}.${fn_node.value}'
		}
	}
	return fn_node.value
}

fn (mut tc TypeChecker) check_if_expr(id flat.NodeId, node flat.Node) {
	if node.children_count < 2 {
		return
	}
	cond_id := tc.a.child(&node, 0)
	guard_bindings := tc.check_condition(cond_id)
	smartcasts := tc.extract_smartcasts(cond_id)
	then_id := tc.a.child(&node, 1)
	saved_smartcasts := clone_smartcasts(tc.smartcasts)
	for sc in smartcasts {
		if valid_string_data(sc.name) {
			tc.smartcasts[sc.name] = sc.typ
		}
	}
	tc.push_scope()
	for binding in guard_bindings {
		tc.cur_scope.insert(binding.name, binding.typ)
	}
	tc.check_node(then_id)
	then_type := tc.branch_tail_type(then_id)
	tc.pop_scope()
	tc.smartcasts = clone_smartcasts(saved_smartcasts)
	mut else_type := Type(void_)
	if node.children_count > 2 {
		else_id := tc.a.child(&node, 2)
		tc.check_node(else_id)
		else_type = tc.branch_tail_type(else_id)
	}
	if then_type !is Void && else_type !is Void {
		if tc.branch_has_value_tail(then_id) && tc.branch_has_value_tail(tc.a.child(&node, 2))
			&& !tc.type_compatible(then_type, else_type)
			&& !tc.type_compatible(else_type, then_type) {
			if tc.should_diagnose(id) {
				tc.record_error(.if_branch_mismatch,
					'if-expression branch type mismatch: then `${then_type.name()}` vs else `${else_type.name()}`',
					id)
			}
		}
	}
}

fn (tc &TypeChecker) branch_has_value_tail(id flat.NodeId) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.nodes[int(id)]
	if node.kind == .block {
		if node.children_count == 0 {
			return false
		}
		last_id := tc.a.child(&node, node.children_count - 1)
		if !tc.valid_node_id(last_id) {
			return false
		}
		last := tc.a.nodes[int(last_id)]
		return last.kind == .expr_stmt
	}
	if node.kind == .match_branch {
		body_start := if node.value == 'else' { 0 } else { node.value.int() }
		if node.children_count <= body_start {
			return false
		}
		last_id := tc.a.child(&node, node.children_count - 1)
		if !tc.valid_node_id(last_id) {
			return false
		}
		last := tc.a.nodes[int(last_id)]
		return last.kind == .expr_stmt
	}
	return node.kind !in [.assign, .decl_assign, .selector_assign, .index_assign, .return_stmt,
		.block]
}

fn (mut tc TypeChecker) check_condition(cond_id flat.NodeId) []LocalBinding {
	if int(cond_id) < 0 {
		return []LocalBinding{}
	}
	cond := tc.a.nodes[int(cond_id)]
	if cond.kind == .decl_assign {
		return tc.check_if_guard(cond_id, cond)
	}
	if cond.kind == .infix && cond.op == .logical_and && cond.children_count >= 2 {
		lhs_id := tc.a.child(&cond, 0)
		rhs_id := tc.a.child(&cond, 1)
		tc.check_bool_condition(lhs_id)
		saved_smartcasts := clone_smartcasts(tc.smartcasts)
		for sc in tc.extract_smartcasts(lhs_id) {
			if valid_string_data(sc.name) {
				tc.smartcasts[sc.name] = sc.typ
			}
		}
		rhs_bindings := tc.check_condition(rhs_id)
		tc.smartcasts = clone_smartcasts(saved_smartcasts)
		return rhs_bindings
	}
	tc.check_bool_condition(cond_id)
	return []LocalBinding{}
}

fn (mut tc TypeChecker) check_bool_condition(cond_id flat.NodeId) {
	tc.check_node(cond_id)
	cond_type := tc.resolve_type(cond_id)
	if !tc.type_compatible(cond_type, Type(bool_)) && tc.should_diagnose(cond_id) {
		tc.record_error(.condition_mismatch,
			'if condition must be `bool`, not `${cond_type.name()}`', cond_id)
	}
}

fn (mut tc TypeChecker) check_if_guard(id flat.NodeId, node flat.Node) []LocalBinding {
	if node.children_count < 2 {
		return []LocalBinding{}
	}
	lhs_id := tc.a.child(&node, 0)
	rhs_id := tc.a.child(&node, 1)
	tc.check_node(rhs_id)
	rhs_type := tc.resolve_type(rhs_id)
	mut payload := Type(void_)
	if rhs_type is OptionType {
		payload = rhs_type.base_type
	} else if rhs_type is ResultType {
		payload = rhs_type.base_type
	} else {
		rhs := tc.a.nodes[int(rhs_id)]
		if rhs.kind == .index && rhs.children_count > 0 {
			base_type := unwrap_pointer(tc.resolve_type(tc.a.child(&rhs, 0)))
			if base_type is Map {
				payload = base_type.value_type
			}
		}
	}
	if payload is Void {
		if tc.should_diagnose(id) {
			tc.record_error(.condition_mismatch,
				'if guard expression must be optional or result, not `${rhs_type.name()}`', id)
		}
	}
	lhs := tc.a.nodes[int(lhs_id)]
	if lhs.kind == .ident && lhs.value.len > 0 && payload !is Void {
		mut result := []LocalBinding{}
		result << LocalBinding{
			name: lhs.value
			typ:  payload
		}
		return result
	}
	return []LocalBinding{}
}

fn (mut tc TypeChecker) check_match_stmt(_id flat.NodeId, node flat.Node) {
	if node.children_count == 0 {
		return
	}
	subject_id := tc.a.child(&node, 0)
	tc.check_node(subject_id)
	subject_key := tc.expr_key(subject_id)
	subject_type := unwrap_pointer(tc.resolve_type(subject_id))
	for i in 1 .. node.children_count {
		branch_id := tc.a.child(&node, i)
		branch := tc.a.child_node(&node, i)
		if branch.kind != .match_branch {
			tc.check_node(branch_id)
			continue
		}
		n_conds := if branch.value == 'else' { 0 } else { branch.value.int() }
		if subject_type is SumType {
			for j in 0 .. n_conds {
				cond := tc.a.node(tc.a.child(branch, j))
				if pattern := tc.match_type_pattern(cond) {
					if !tc.sum_has_variant(subject_type.name, pattern) {
						tc.record_error(.condition_mismatch,
							'`${pattern}` is not a variant of sum type `${subject_type.name}`', tc.a.child(branch,
							j))
					}
				}
			}
		}
		saved_smartcasts := clone_smartcasts(tc.smartcasts)
		if subject_key.len > 0 && valid_string_data(subject_key) && n_conds == 1
			&& branch.children_count > 0 {
			cond_id := tc.a.child(branch, 0)
			cond := tc.a.node(cond_id)
			if pattern := tc.match_type_pattern(cond) {
				tc.smartcasts[subject_key] = tc.parse_type(pattern)
			}
		}
		tc.push_scope()
		for j in n_conds .. branch.children_count {
			tc.check_node(tc.a.child(branch, j))
		}
		tc.pop_scope()
		tc.smartcasts = clone_smartcasts(saved_smartcasts)
	}
}

fn (mut tc TypeChecker) check_is_expr(id flat.NodeId, node flat.Node) {
	if node.children_count == 0 {
		return
	}
	expr_id := tc.a.child(&node, 0)
	tc.check_node(expr_id)
	expr_type := unwrap_pointer(tc.resolve_type(expr_id))
	if expr_type is SumType {
		if node.value.len > 0 && !tc.sum_has_variant(expr_type.name, node.value)
			&& tc.should_diagnose(id) {
			tc.record_error(.condition_mismatch,
				'`${node.value}` is not a variant of sum type `${expr_type.name}`', id)
		}
		return
	}
	if expr_type is Interface || expr_type is Unknown {
		return
	}
	if tc.should_diagnose(id) {
		tc.record_error(.condition_mismatch,
			'`is` can only be used with sum type or interface values, not `${expr_type.name()}`',
			id)
	}
}

fn (tc &TypeChecker) branch_tail_type(id flat.NodeId) Type {
	if !tc.valid_node_id(id) {
		return Type(void_)
	}
	node := tc.a.nodes[int(id)]
	if node.kind == .if_expr {
		return tc.if_expr_tail_type(id)
	}
	if node.kind == .block {
		if node.children_count == 0 {
			return Type(void_)
		}
		last_id := tc.a.child(&node, node.children_count - 1)
		if !tc.valid_node_id(last_id) {
			return Type(void_)
		}
		last := tc.a.nodes[int(last_id)]
		if last.kind == .expr_stmt && last.children_count > 0 {
			return tc.resolve_type(tc.a.child(&last, 0))
		}
		return tc.resolve_type(last_id)
	}
	if node.kind == .match_branch {
		body_start := if node.value == 'else' { 0 } else { node.value.int() }
		if node.children_count <= body_start {
			return Type(void_)
		}
		last_id := tc.a.child(&node, node.children_count - 1)
		if !tc.valid_node_id(last_id) {
			return Type(void_)
		}
		last := tc.a.nodes[int(last_id)]
		if last.kind == .expr_stmt && last.children_count > 0 {
			return tc.resolve_type(tc.a.child(&last, 0))
		}
		return tc.resolve_type(last_id)
	}
	return tc.resolve_type(id)
}

fn (tc &TypeChecker) if_expr_tail_type(id flat.NodeId) Type {
	mut cur_id := id
	mut result := Type(void_)
	for tc.valid_node_id(cur_id) {
		node := tc.a.nodes[int(cur_id)]
		if node.kind != .if_expr {
			return choose_if_tail_type(result, tc.branch_tail_type(cur_id))
		}
		if node.children_count > 1 {
			then_type := tc.branch_tail_type(tc.a.child(&node, 1))
			result = choose_if_tail_type(result, then_type)
		}
		if node.children_count <= 2 {
			return result
		}
		else_id := tc.a.child(&node, 2)
		if !tc.valid_node_id(else_id) {
			return result
		}
		else_node := tc.a.nodes[int(else_id)]
		if else_node.kind == .if_expr {
			cur_id = else_id
			continue
		}
		else_type := tc.branch_tail_type(else_id)
		return choose_if_tail_type(result, else_type)
	}
	return result
}

fn choose_if_tail_type(current Type, next Type) Type {
	if current is Void {
		return next
	}
	if next is Void {
		return current
	}
	if current !is Primitive {
		return current
	}
	if next !is Primitive {
		return next
	}
	return current
}

fn (tc &TypeChecker) extract_smartcasts(cond_id flat.NodeId) []LocalBinding {
	if int(cond_id) < 0 {
		return []LocalBinding{}
	}
	cond := tc.a.nodes[int(cond_id)]
	if cond.kind == .is_expr && cond.children_count > 0 {
		expr_id := tc.a.child(&cond, 0)
		key := tc.expr_key(expr_id)
		if key.len > 0 && valid_string_data(key) && cond.value.len > 0 {
			mut result := []LocalBinding{}
			result << LocalBinding{
				name: key
				typ:  tc.parse_type(cond.value)
			}
			return result
		}
	}
	if cond.kind == .infix && cond.op == .logical_and && cond.children_count >= 2 {
		mut result := tc.extract_smartcasts(tc.a.child(&cond, 0))
		result << tc.extract_smartcasts(tc.a.child(&cond, 1))
		return result
	}
	return []LocalBinding{}
}

fn (mut tc TypeChecker) check_struct_init(id flat.NodeId, node flat.Node) {
	init_type := tc.parse_type(node.value)
	if init_type is Struct {
		fields := tc.structs[init_type.name] or { []StructField{} }
		for i in 0 .. node.children_count {
			field_id := tc.a.child(&node, i)
			field := tc.a.nodes[int(field_id)]
			if field.kind != .field_init || field.children_count == 0 {
				tc.check_node(field_id)
				continue
			}
			value_id := tc.a.child(&field, 0)
			mut expected := Type(void_)
			if field.value.len > 0 {
				mut found := false
				for f in fields {
					if f.name == field.value {
						expected = f.typ
						found = true
						break
					}
				}
				if !found && tc.should_diagnose(field_id) && fields.len > 0 {
					tc.record_error(.unknown_field,
						'unknown field `${field.value}` in `${init_type.name}`', field_id)
				}
			} else if i < fields.len {
				expected = fields[i].typ
			}
			tc.check_node(value_id)
			if expected !is Void {
				actual := tc.resolve_expr(value_id, expected)
				if !tc.type_compatible(actual, expected) {
					tc.type_mismatch(.assignment_mismatch,
						'cannot initialize field `${field.value}` with `${actual.name()}`; expected `${expected.name()}`',
						field_id)
				}
			}
		}
		return
	}
	for i in 0 .. node.children_count {
		tc.check_node(tc.a.child(&node, i))
	}
	_ = id
}

fn (mut tc TypeChecker) check_selector(id flat.NodeId, node flat.Node) {
	if node.children_count == 0 {
		return
	}
	base_id := tc.a.child(&node, 0)
	base := tc.a.nodes[int(base_id)]
	if tc.is_namespace_selector(node, base) {
		return
	}
	tc.check_node(base_id)
	base_type := tc.resolve_type(base_id)
	tc.register_synth_type(base_id, base_type)
	if typ := tc.selector_type(id, node) {
		tc.register_synth_type(id, typ)
		return
	} else {
		if tc.should_diagnose(id) {
			if base_type is Unknown {
				return
			}
			tc.record_error(.unknown_field,
				'unknown field `${node.value}` on `${base_type.name()}`', id)
		}
	}
}

fn (tc &TypeChecker) is_namespace_selector(node flat.Node, base flat.Node) bool {
	if base.kind != .ident {
		return false
	}
	if base.value == 'C' || tc.has_active_import(base.value) {
		return true
	}
	qbase := tc.qualify_name(base.value)
	if qbase in tc.structs || qbase in tc.enum_names || qbase in tc.sum_types
		|| qbase in tc.interface_names {
		return true
	}
	qname := '${qbase}.${node.value}'
	return qname in tc.const_types || qname in tc.fn_ret_types || qname in tc.enum_names
}

fn (tc &TypeChecker) selector_type(_id flat.NodeId, node flat.Node) ?Type {
	if node.children_count == 0 {
		return none
	}
	if typ := tc.enum_selector_type(&node) {
		return typ
	}
	base_id := tc.a.child(&node, 0)
	base_type := tc.resolve_type(base_id)
	clean0 := unwrap_pointer(base_type)
	mut clean := clean0
	if clean0 is Alias {
		clean = clean0.base_type
	}
	clean_name := clean.name()
	if typ := option_result_selector_type(clean, node.value) {
		return typ
	}
	if node.value == 'len' {
		if clean is Array || clean is Map || clean is String || clean is ArrayFixed {
			return Type(int_)
		}
	}
	if clean is Channel && node.value == 'closed' {
		return Type(bool_)
	}
	if clean is Struct {
		if fields := tc.structs[clean_name] {
			for f in fields {
				if f.name == node.value {
					return f.typ
				}
			}
		}
	}
	if clean is Interface {
		if typ := tc.interface_field_type(clean.name, node.value) {
			return typ
		}
	}
	if clean is MultiReturn {
		if typ := multi_return_selector_type(clean, node.value) {
			return typ
		}
	}
	if clean_name == 'IError' || clean_name.ends_with('.IError') {
		if node.value == 'message' {
			return Type(String{})
		}
		if node.value == '_object' {
			return tc.parse_type('voidptr')
		}
	}
	if clean is SumType {
		if typ := tc.lowered_sum_selector_type(clean, node.value) {
			return typ
		}
		if typ := tc.sum_shared_field_type(clean, node.value) {
			return typ
		}
	}
	if clean is Array || clean is Map || clean is String {
		sname := if clean is Array {
			'array'
		} else if clean is Map {
			'map'
		} else {
			'string'
		}
		if fields := tc.structs[sname] {
			for f in fields {
				if f.name == node.value {
					return f.typ
				}
			}
		}
	}
	return none
}

fn multi_return_selector_type(typ MultiReturn, field string) ?Type {
	if !field.starts_with('arg') || field.len <= 3 {
		return none
	}
	idx_str := field[3..]
	idx := idx_str.int()
	if idx_str != idx.str() || idx < 0 || idx >= typ.types.len {
		return none
	}
	return typ.types[idx]
}

fn (tc &TypeChecker) lowered_sum_selector_type(sum SumType, field string) ?Type {
	if field == 'typ' {
		return Type(int_)
	}
	variants := tc.sum_types[sum.name] or { return none }
	for variant in variants {
		short := if variant.contains('.') { variant.all_after_last('.') } else { variant }
		if field == variant || field == short || field == c_name(variant) {
			return tc.parse_type(variant)
		}
	}
	return none
}

fn (tc &TypeChecker) sum_shared_field_type(sum SumType, field string) ?Type {
	variants := tc.sum_types[sum.name] or { return none }
	if variants.len == 0 {
		return none
	}
	mut has_common := false
	mut common_typ := Type(void_)
	for variant in variants {
		variant_field := tc.struct_field_type(variant, field) or { return none }
		if !has_common {
			common_typ = variant_field
			has_common = true
			continue
		}
		if !tc.type_compatible(variant_field, common_typ)
			|| !tc.type_compatible(common_typ, variant_field) {
			return none
		}
	}
	return common_typ
}

fn option_result_selector_type(typ Type, field string) ?Type {
	if typ is OptionType {
		if field == 'ok' {
			return Type(bool_)
		}
		if field == 'value' {
			return typ.base_type
		}
	}
	if typ is ResultType {
		if field == 'ok' {
			return Type(bool_)
		}
		if field == 'value' {
			return typ.base_type
		}
	}
	return none
}

fn (mut tc TypeChecker) check_index(id flat.NodeId, node flat.Node) {
	if node.children_count == 0 {
		return
	}
	base_id := tc.a.child(&node, 0)
	tc.check_node(base_id)
	base_type_raw := tc.resolve_type(base_id)
	base_type0 := unwrap_pointer(base_type_raw)
	mut base_type := base_type0
	if base_type0 is Alias {
		base_type = base_type0.base_type
	}
	if node.value == 'range' {
		if !(base_type is Array || base_type is ArrayFixed || base_type is String)
			&& tc.should_diagnose(id) {
			tc.record_error(.cannot_index, 'cannot slice `${base_type.name()}`', id)
		}
		for i in 1 .. node.children_count {
			bound_id := tc.a.child(&node, i)
			if int(bound_id) >= 0 {
				bound := tc.a.nodes[int(bound_id)]
				if bound.kind == .empty {
					continue
				}
				tc.check_node(bound_id)
				bound_type := tc.resolve_type(bound_id)
				if bound_type !is Unknown && !bound_type.is_integer() {
					tc.type_mismatch(.cannot_index,
						'slice bound must be integer, not `${bound_type.name()}`', bound_id)
				}
			}
		}
		tc.register_synth_type(id, tc.resolve_index_type(node))
		return
	}
	if node.children_count > 1 {
		index_id := tc.a.child(&node, 1)
		tc.check_node(index_id)
		if base_type is Map {
			actual_key := tc.resolve_expr(index_id, base_type.key_type)
			if !tc.type_compatible(actual_key, base_type.key_type) {
				tc.type_mismatch(.cannot_index,
					'map key must be `${base_type.key_type.name()}`, not `${actual_key.name()}`',
					index_id)
			}
			tc.register_synth_type(id, base_type.value_type)
			return
		}
		index_type := tc.resolve_type(index_id)
		if index_type !is Unknown && !index_type.is_integer() {
			tc.type_mismatch(.cannot_index, 'index must be integer, not `${index_type.name()}`',
				index_id)
		}
	}
	if !(base_type is Array || base_type is ArrayFixed || base_type is String
		|| base_type is Map || base_type is Unknown || base_type_raw is Pointer)
		&& tc.should_diagnose(id) {
		tc.record_error(.cannot_index, 'cannot index `${base_type.name()}`', id)
	}
	tc.register_synth_type(id, tc.resolve_index_type(node))
}

fn (mut tc TypeChecker) check_ident(id flat.NodeId, node flat.Node) {
	if node.value.len == 0 || node.value == '_' {
		return
	}
	if typ := tc.cur_scope.lookup(node.value) {
		tc.register_synth_type(id, typ)
		return
	}
	if typ := tc.file_scope.lookup(node.value) {
		tc.register_synth_type(id, typ)
		return
	}
	qname := tc.qualify_name(node.value)
	if typ := tc.const_types[qname] {
		tc.register_synth_type(id, typ)
		return
	}
	if typ := tc.const_types[node.value] {
		tc.register_synth_type(id, typ)
		return
	}
	if typ := tc.fn_value_type(node.value) {
		tc.register_synth_type(id, typ)
		return
	}
	if node.value in tc.fn_ret_types || tc.qualify_fn_name(node.value) in tc.fn_ret_types {
		return
	}
	if tc.has_active_import(node.value) || qname in tc.structs || qname in tc.enum_names
		|| qname in tc.sum_types || qname in tc.interface_names {
		return
	}
	if tc.should_diagnose(id) {
		tc.record_error(.unknown_ident, 'unknown identifier `${node.value}`', id)
	}
}

fn (mut tc TypeChecker) resolve_expr(id flat.NodeId, expected Type) Type {
	if int(id) < 0 {
		return unknown_type('missing expression')
	}
	expected_raw := expected
	node := tc.a.nodes[int(id)]
	if node.kind == .field_init && node.children_count > 0 {
		return tc.resolve_expr(tc.a.child(&node, 0), expected)
	}
	if node.kind == .none_expr {
		if expected is OptionType || expected is ResultType || is_ierror_type(expected) {
			tc.register_synth_type(id, expected)
			return expected
		}
	}
	if node.kind == .enum_val && expected is Enum {
		if tc.enum_value_matches(node.value, expected.name) {
			tc.register_synth_type(id, expected_raw)
			return expected_raw
		}
		tc.type_mismatch(.assignment_mismatch,
			'unknown enum field `${node.value}` for `${expected.name}`', id)
		return Type(int_)
	}
	if node.kind == .array_literal {
		if expected is Array && node.children_count == 0 {
			tc.register_synth_type(id, expected_raw)
			return expected_raw
		}
		if expected is ArrayFixed && node.children_count == 0 {
			tc.register_synth_type(id, expected_raw)
			return expected_raw
		}
	}
	if node.kind == .ident && expected is FnType {
		if tc.fn_value_matches(node.value, expected_raw) {
			tc.register_synth_type(id, expected_raw)
			return expected_raw
		}
	}
	if node.kind == .fn_literal || node.kind == .lambda_expr {
		if _ := fn_type_from_type(expected) {
			tc.register_synth_type(id, expected_raw)
			return expected_raw
		}
	}
	actual := tc.resolve_type(id)
	if tc.type_compatible(actual, expected) {
		if actual.name() == expected.name() {
			tc.register_synth_type(id, expected)
			return expected
		}
		if expected is OptionType || expected is ResultType {
			if actual is OptionType || actual is ResultType {
				tc.register_synth_type(id, expected_raw)
				return expected_raw
			}
			return actual
		}
		if expected is SumType || expected is Enum {
			tc.register_synth_type(id, expected_raw)
			return expected_raw
		}
	}
	return actual
}

fn (tc &TypeChecker) fn_value_matches(name string, expected Type) bool {
	if expected is FnType {
		actual := tc.fn_value_type(name) or { return false }
		if actual is FnType {
			if actual.params.len != expected.params.len {
				return false
			}
			for i in 0 .. actual.params.len {
				actual_param := fn_param_type(actual, i)
				expected_param := fn_param_type(expected, i)
				if !tc.type_compatible(actual_param, expected_param) {
					return false
				}
			}
			return tc.type_compatible(actual.return_type, expected.return_type)
		}
	}
	return false
}

fn (tc &TypeChecker) fn_value_type(name string) ?Type {
	qfn := tc.qualify_fn_name(name)
	if qfn in tc.fn_ret_types {
		return tc.fn_type_from_key(qfn)
	}
	if name in tc.fn_ret_types {
		return tc.fn_type_from_key(name)
	}
	return none
}

fn (tc &TypeChecker) fn_type_from_key(key string) ?Type {
	params := tc.fn_param_types[key] or { return none }
	ret := tc.fn_ret_types[key] or { return none }
	return Type(FnType{
		params:      params.clone()
		return_type: ret
	})
}

fn (tc &TypeChecker) enum_value_matches(value string, enum_name string) bool {
	if value.contains('.') {
		prefix := value.all_before_last('.')
		field := value.all_after_last('.')
		if prefix != enum_name && short_type_name(prefix) != short_type_name(enum_name) {
			return false
		}
		return tc.enum_has_field(enum_name, field)
	}
	return tc.enum_has_field(enum_name, value)
}

fn (tc &TypeChecker) enum_has_field(enum_name string, field string) bool {
	fields := tc.enum_fields[enum_name] or { return false }
	return field in fields
}

fn (tc &TypeChecker) resolve_enum_name(name string) ?string {
	if name in tc.enum_names {
		return name
	}
	qname := tc.qualify_name(name)
	if qname in tc.enum_names {
		return qname
	}
	return none
}

fn (tc &TypeChecker) enum_selector_type(node &flat.Node) ?Type {
	if node.kind != .selector || node.children_count == 0 {
		return none
	}
	base := tc.a.child_node(node, 0)
	mut enum_name := ''
	if base.kind == .ident {
		enum_name = tc.resolve_enum_name(base.value) or { '' }
	} else if base.kind == .selector && base.children_count > 0 {
		inner := tc.a.child_node(base, 0)
		if inner.kind == .ident {
			mod_name := tc.resolve_import_alias(inner.value) or { inner.value }
			enum_name = tc.resolve_enum_name('${mod_name}.${base.value}') or { '' }
		}
	}
	if enum_name.len == 0 || !tc.enum_has_field(enum_name, node.value) {
		return none
	}
	return Type(Enum{
		name:    enum_name
		is_flag: enum_name in tc.flag_enums
	})
}

fn (tc &TypeChecker) type_compatible(actual Type, expected Type) bool {
	actual_raw := actual
	expected_raw := expected
	if actual.name() == expected.name() {
		return true
	}
	if actual is Unknown || expected is Unknown {
		return true
	}
	if actual is Alias && expected is Alias {
		return tc.type_compatible(actual.base_type, expected.base_type)
	}
	if actual is Alias {
		return tc.type_compatible(actual.base_type, expected)
	}
	if expected is Alias {
		return tc.type_compatible(actual, expected.base_type)
	}
	if expected is String && is_ierror_type(actual) {
		return true
	}
	if actual is Array && is_runtime_array_type(expected) {
		return true
	}
	if is_runtime_array_type(actual) && expected is Array {
		return true
	}
	if actual is None {
		return expected is OptionType || expected is ResultType
	}
	if expected is OptionType {
		if actual is OptionType {
			return tc.type_compatible(actual.base_type, expected.base_type)
		}
		return tc.type_compatible(actual, expected.base_type)
	}
	if expected is ResultType {
		if is_ierror_type(actual) {
			return true
		}
		if actual is ResultType {
			return tc.type_compatible(actual.base_type, expected.base_type)
		}
		return tc.type_compatible(actual, expected.base_type)
	}
	if expected is SumType {
		return tc.type_matches_sum(actual_raw, expected_raw)
	}
	if expected is Interface {
		return tc.type_implements_interface(actual, expected)
	}
	if actual is Interface {
		if expected is Interface {
			return tc.interface_implements_interface(actual.name, expected.name)
		}
		return false
	}
	if expected is Primitive {
		if actual is Primitive {
			if expected.props.has(.boolean) || actual.props.has(.boolean) {
				return expected.props.has(.boolean) && actual.props.has(.boolean)
			}
			if expected.props.has(.float) && actual.props.has(.integer) {
				return true
			}
			if expected.props.has(.integer) && actual.props.has(.integer) {
				return true
			}
			if expected.props.has(.float) && actual.props.has(.float) {
				return true
			}
		}
		if expected.props.has(.integer) && actual.is_integer() {
			return true
		}
	}
	if actual is Primitive && actual.props.has(.integer) && expected.is_integer() {
		return true
	}
	if expected is String {
		return actual is String || is_ierror_type(actual)
	}
	if expected is Char {
		return actual is Char || actual.name() == 'u8'
	}
	if expected is Pointer {
		if actual is Nil {
			return true
		}
		if actual is Pointer {
			if expected.base_type is Void || actual.base_type is Void {
				return true
			}
			return tc.type_compatible(actual.base_type, expected.base_type)
		}
	}
	if expected is Array {
		if actual is Array {
			return tc.type_compatible(actual.elem_type, expected.elem_type)
		}
		if actual is ArrayFixed {
			return tc.type_compatible(actual.elem_type, expected.elem_type)
		}
	}
	if expected is ArrayFixed {
		if actual is ArrayFixed {
			return tc.fixed_array_lengths_compatible(actual, expected)
				&& tc.type_compatible(actual.elem_type, expected.elem_type)
		}
	}
	if expected is Channel {
		if actual is Channel {
			return tc.type_compatible(actual.elem_type, expected.elem_type)
		}
	}
	if expected is Map {
		if actual is Map {
			return tc.type_compatible(actual.key_type, expected.key_type)
				&& tc.type_compatible(actual.value_type, expected.value_type)
		}
	}
	if expected is FnType {
		if actual is FnType {
			if actual.params.len != expected.params.len {
				return false
			}
			for i in 0 .. actual.params.len {
				actual_param := fn_param_type(actual, i)
				expected_param := fn_param_type(expected, i)
				if !tc.type_compatible(actual_param, expected_param) {
					return false
				}
			}
			return tc.type_compatible(actual.return_type, expected.return_type)
		}
	}
	return false
}

fn is_ierror_type(t Type) bool {
	if t is Alias {
		return t.name == 'IError' || t.name.ends_with('.IError') || is_ierror_type(t.base_type)
	}
	if t is Pointer {
		return is_ierror_type(t.base_type)
	}
	if t is Struct {
		return t.name == 'IError' || t.name.ends_with('.IError')
	}
	if t is Interface {
		return t.name == 'IError' || t.name.ends_with('.IError')
	}
	return false
}

fn is_runtime_array_type(t Type) bool {
	if t is Alias {
		return is_runtime_array_type(t.base_type)
	}
	if t is Struct {
		return t.name == 'array'
	}
	return false
}

fn (tc &TypeChecker) fixed_array_lengths_compatible(actual ArrayFixed, expected ArrayFixed) bool {
	if actual.len > 0 && expected.len > 0 {
		return actual.len == expected.len
	}
	actual_len := tc.fixed_array_len_value(actual) or {
		return actual.len_expr == expected.len_expr
	}
	expected_len := tc.fixed_array_len_value(expected) or {
		return actual.len_expr == expected.len_expr
	}
	return actual_len == expected_len
}

fn (tc &TypeChecker) fixed_array_len_value(arr ArrayFixed) ?int {
	if arr.len > 0 {
		return arr.len
	}
	if arr.len_expr.len == 0 {
		return none
	}
	return tc.const_int_value(arr.len_expr, []string{})
}

fn (tc &TypeChecker) const_int_value(name string, seen []string) ?int {
	if name in seen {
		return none
	}
	mut candidates := []string{}
	candidates << name
	qname := tc.qualify_name(name)
	if qname != name {
		candidates << qname
	}
	for key in candidates {
		if expr_id := tc.const_exprs[key] {
			mut next_seen := seen.clone()
			next_seen << key
			return tc.const_int_expr(expr_id, next_seen)
		}
	}
	if is_decimal_int_literal(name) {
		return name.int()
	}
	return none
}

fn (tc &TypeChecker) const_int_expr(id flat.NodeId, seen []string) ?int {
	if int(id) < 0 {
		return none
	}
	node := tc.a.nodes[int(id)]
	match node.kind {
		.int_literal {
			if is_decimal_int_literal(node.value) {
				return node.value.int()
			}
		}
		.ident {
			return tc.const_int_value(node.value, seen)
		}
		.paren {
			if node.children_count > 0 {
				return tc.const_int_expr(tc.a.child(&node, 0), seen)
			}
		}
		.prefix {
			if node.children_count == 0 {
				return none
			}
			value := tc.const_int_expr(tc.a.child(&node, 0), seen) or { return none }
			return match node.op {
				.minus { -value }
				.plus { value }
				else { none }
			}
		}
		.infix {
			if node.children_count < 2 {
				return none
			}
			left := tc.const_int_expr(tc.a.child(&node, 0), seen) or { return none }
			right := tc.const_int_expr(tc.a.child(&node, 1), seen) or { return none }
			match node.op {
				.plus {
					return left + right
				}
				.minus {
					return left - right
				}
				.mul {
					return left * right
				}
				.div {
					if right == 0 {
						return none
					}
					return left / right
				}
				.mod {
					if right == 0 {
						return none
					}
					return left % right
				}
				else {
					return none
				}
			}
		}
		else {}
	}

	return none
}

fn (tc &TypeChecker) type_implements_interface(actual Type, expected Interface) bool {
	clean := unwrap_pointer(actual)
	if clean is Unknown {
		return true
	}
	if clean is Interface {
		return tc.interface_implements_interface(clean.name, expected.name)
	}
	concrete_name := method_type_name(clean)
	if concrete_name.len == 0 {
		return false
	}
	return tc.named_type_implements_interface(concrete_name, expected.name)
}

fn (tc &TypeChecker) interface_implements_interface(actual_name string, expected_name string) bool {
	if actual_name == expected_name {
		return true
	}
	for method in tc.interface_method_names(expected_name) {
		actual_key := '${actual_name}.${method}'
		expected_key := '${expected_name}.${method}'
		if actual_key !in tc.fn_param_types
			|| !tc.method_signature_compatible(actual_key, expected_key) {
			return false
		}
	}
	for field in tc.interface_field_list(expected_name) {
		actual_field := tc.interface_field_type(actual_name, field.name) or { return false }
		if !tc.type_compatible(actual_field, field.typ)
			|| !tc.type_compatible(field.typ, actual_field) {
			return false
		}
	}
	return true
}

pub fn (tc &TypeChecker) named_type_implements_interface(concrete_name string, iface_name string) bool {
	// Only the abstract (declared) methods must be provided by the concrete type.
	// Methods defined directly on the interface (default implementations) are
	// inherited and need not be reimplemented.
	for method in tc.interface_abstract_method_names(iface_name) {
		concrete_key := '${concrete_name}.${method}'
		if concrete_key !in tc.fn_param_types {
			return false
		}
		expected_key := '${iface_name}.${method}'
		if !tc.method_signature_compatible(concrete_key, expected_key) {
			return false
		}
	}
	for field in tc.interface_field_list(iface_name) {
		actual_field := tc.struct_field_type(concrete_name, field.name) or { return false }
		if !tc.type_compatible(actual_field, field.typ)
			|| !tc.type_compatible(field.typ, actual_field) {
			return false
		}
	}
	return true
}

fn (tc &TypeChecker) interface_method_names(iface_name string) []string {
	mut seen := map[string]bool{}
	return tc.interface_method_names_inner(iface_name, mut seen)
}

// interface_abstract_method_names returns the methods an implementer must provide:
// the interface's own declared (abstract) methods plus those of any embedded
// interfaces. Default methods defined directly on the interface are excluded.
pub fn (tc &TypeChecker) interface_abstract_method_names(iface_name string) []string {
	mut seen := map[string]bool{}
	return tc.interface_abstract_method_names_inner(iface_name, mut seen)
}

fn (tc &TypeChecker) interface_abstract_method_names_inner(iface_name string, mut seen map[string]bool) []string {
	if iface_name in seen {
		return []string{}
	}
	seen[iface_name] = true
	mut methods := []string{}
	for embed in tc.interface_embeds[iface_name] or { []string{} } {
		for method in tc.interface_abstract_method_names_inner(embed, mut seen) {
			if method !in methods {
				methods << method
			}
		}
	}
	for method in tc.interface_abstract_methods[iface_name] or { []string{} } {
		if method !in methods {
			methods << method
		}
	}
	return methods
}

fn (tc &TypeChecker) interface_method_names_inner(iface_name string, mut seen map[string]bool) []string {
	if iface_name in seen {
		return []string{}
	}
	seen[iface_name] = true
	mut methods := []string{}
	for embed in tc.interface_embeds[iface_name] or { []string{} } {
		for method in tc.interface_method_names_inner(embed, mut seen) {
			if method !in methods {
				methods << method
			}
		}
	}
	prefix := '${iface_name}.'
	for key, _ in tc.fn_ret_types {
		if key.starts_with(prefix) {
			method := key[prefix.len..]
			if method !in methods {
				methods << method
			}
		}
	}
	return methods
}

fn (tc &TypeChecker) interface_field_list(iface_name string) []StructField {
	mut seen := map[string]bool{}
	return tc.interface_field_list_inner(iface_name, mut seen)
}

fn (tc &TypeChecker) interface_field_list_inner(iface_name string, mut seen map[string]bool) []StructField {
	if iface_name in seen {
		return []StructField{}
	}
	seen[iface_name] = true
	mut fields := []StructField{}
	for embed in tc.interface_embeds[iface_name] or { []string{} } {
		fields << tc.interface_field_list_inner(embed, mut seen)
	}
	for field in tc.interface_fields[iface_name] or { []StructField{} } {
		fields << field
	}
	return fields
}

fn (tc &TypeChecker) interface_field_type(iface_name string, field_name string) ?Type {
	for field in tc.interface_field_list(iface_name) {
		if field.name == field_name {
			return field.typ
		}
	}
	return none
}

fn (tc &TypeChecker) struct_field_type(struct_name string, field_name string) ?Type {
	for field in tc.structs[struct_name] or { []StructField{} } {
		if field.name == field_name {
			return field.typ
		}
	}
	return none
}

fn (tc &TypeChecker) method_signature_compatible(actual_key string, expected_key string) bool {
	actual_params := tc.fn_param_types[actual_key] or { return false }
	expected_params := tc.fn_param_types[expected_key] or { return false }
	if actual_params.len != expected_params.len {
		return false
	}
	for i in 1 .. actual_params.len {
		if !tc.type_compatible(actual_params[i], expected_params[i])
			|| !tc.type_compatible(expected_params[i], actual_params[i]) {
			return false
		}
	}
	actual_ret := tc.fn_ret_types[actual_key] or { Type(void_) }
	expected_ret := tc.fn_ret_types[expected_key] or { Type(void_) }
	return tc.type_compatible(actual_ret, expected_ret)
}

fn method_type_name(t Type) string {
	if t is Alias {
		return t.name
	}
	if t is Struct {
		return t.name
	}
	if t is Interface {
		return t.name
	}
	if t is SumType {
		return t.name
	}
	if t is Enum {
		return t.name
	}
	if t is String {
		return 'string'
	}
	if t is Primitive {
		return prim_c_type_from(t.props, t.size)
	}
	if t is ISize {
		return 'isize'
	}
	if t is USize {
		return 'usize'
	}
	if t is Rune {
		return 'rune'
	}
	return ''
}

fn (tc &TypeChecker) type_matches_sum(actual Type, expected Type) bool {
	if expected is SumType {
		actual_name := actual.name()
		return tc.sum_has_variant(expected.name, actual_name)
	}
	return false
}

fn (tc &TypeChecker) sum_has_variant(sum_name string, variant_name string) bool {
	variants := tc.sum_types[sum_name] or { return false }
	variant_short := short_type_name(variant_name)
	for variant in variants {
		if variant == variant_name || short_type_name(variant) == variant_short {
			return true
		}
	}
	qvariant := tc.qualify_name(variant_name)
	if qvariant != variant_name {
		for variant in variants {
			if variant == qvariant || short_type_name(variant) == variant_short {
				return true
			}
		}
	}
	return false
}

fn (tc &TypeChecker) match_type_pattern(node &flat.Node) ?string {
	if node.kind == .ident {
		return node.value
	}
	if node.kind == .selector && node.children_count > 0 {
		base := tc.a.child_node(node, 0)
		if base.kind == .ident {
			return '${base.value}.${node.value}'
		}
	}
	return none
}

fn short_type_name(name string) string {
	if name.contains('.') {
		return name.all_after_last('.')
	}
	return name
}

fn (mut tc TypeChecker) type_mismatch(kind TypeErrorKind, msg string, node flat.NodeId) {
	if tc.should_diagnose(node) {
		tc.record_error(kind, msg, node)
	}
}

fn (tc &TypeChecker) expr_key(id flat.NodeId) string {
	if int(id) < 0 {
		return ''
	}
	node := tc.a.nodes[int(id)]
	if node.kind == .ident {
		if valid_string_data(node.value) {
			return node.value
		}
		return ''
	}
	if node.kind == .selector && node.children_count > 0 {
		base := tc.expr_key(tc.a.child(&node, 0))
		if base.len > 0 && node.value.len > 0 && valid_string_data(node.value) {
			return '${base}.${node.value}'
		}
	}
	return ''
}

fn (tc &TypeChecker) smartcast_type(id flat.NodeId) ?Type {
	key := tc.expr_key(id)
	if key.len == 0 {
		return none
	}
	if !valid_string_data(key) {
		return none
	}
	if typ := tc.smartcasts[key] {
		return typ
	}
	return none
}

// parse_type converts a V type string (from parser) to a structured Type.
pub fn (tc &TypeChecker) parse_type(typ string) Type {
	if tc.type_cache != unsafe { nil } && tc.type_cache.parse_enabled {
		key := tc.cur_module + '\n' + typ
		mut cache := unsafe { tc.type_cache }
		if cached := cache.parse_entries[key] {
			return cached
		}
		result := tc.parse_type_uncached(typ)
		cache.parse_entries[key] = result
		return result
	}
	return tc.parse_type_uncached(typ)
}

fn (tc &TypeChecker) parse_type_uncached(typ string) Type {
	if typ.len == 0 {
		return Type(void_)
	}
	if is_generic_placeholder_type(typ) {
		return Type(int_)
	}
	if typ.starts_with('&') {
		return Type(Pointer{
			base_type: tc.parse_type(typ[1..])
		})
	}
	if typ.starts_with('mut ') {
		return Type(Pointer{
			base_type: tc.parse_type(typ[4..])
		})
	}
	if typ.starts_with('shared ') {
		return tc.parse_type(typ[7..])
	}
	if typ.starts_with('atomic ') {
		return tc.parse_type(typ[7..])
	}
	if typ.starts_with('chan ') {
		return Type(Channel{
			elem_type: tc.parse_type(typ[5..])
		})
	}
	if typ == 'chan' {
		return Type(Channel{
			elem_type: Type(void_)
		})
	}
	if typ.starts_with('?') {
		return Type(OptionType{
			base_type: tc.parse_type(typ[1..])
		})
	}
	if typ.starts_with('!') {
		return Type(ResultType{
			base_type: tc.parse_type(typ[1..])
		})
	}
	if typ.starts_with('...') {
		return Type(Array{
			elem_type: tc.parse_type(typ[3..])
		})
	}
	if typ.starts_with('[]') {
		return Type(Array{
			elem_type: tc.parse_type(typ[2..])
		})
	}
	if typ.starts_with('map[') {
		bracket_end := find_matching_bracket(typ, 3)
		if bracket_end >= typ.len {
			return Type(Unknown{
				reason: 'malformed map type'
			})
		}
		key_str := typ[4..bracket_end]
		val_str := typ[bracket_end + 1..]
		return Type(Map{
			key_type:   tc.parse_type(key_str)
			value_type: tc.parse_type(val_str)
		})
	}
	if typ.starts_with('[') {
		idx := typ.index_u8(`]`)
		if idx > 0 {
			len_text := typ[1..idx].trim_space()
			return Type(ArrayFixed{
				elem_type: tc.parse_type(typ[idx + 1..])
				len:       if is_decimal_int_literal(len_text) { len_text.int() } else { 0 }
				len_expr:  if is_decimal_int_literal(len_text) { '' } else { len_text }
			})
		}
	}
	if typ.starts_with('(') && typ.contains(',') {
		inner := typ[1..typ.len - 1]
		parts := split_params(inner)
		mut types := []Type{}
		for p in parts {
			types << tc.parse_type(p.trim_space())
		}
		return Type(MultiReturn{
			types: types
		})
	}
	if typ.starts_with('fn(') || typ.starts_with('fn (') {
		return tc.parse_fn_type(typ)
	}
	qtyp := tc.qualify_name(typ)
	if typ == 'array' && tc.has_builtins && typ in tc.structs {
		return Type(Struct{
			name: typ
		})
	}
	if typ == 'map' && tc.has_builtins {
		return Type(Struct{
			name: typ
		})
	}
	if typ == 'array' && tc.has_builtins {
		return Type(Struct{
			name: typ
		})
	}
	if is_builtin_type_name(typ) {
		return builtin_type_value(typ)
	}
	if typ == 'unknown' {
		return Type(Unknown{
			reason: 'unknown'
		})
	}
	if typ.starts_with('C.') {
		return Type(Struct{
			name: typ
		})
	}
	if qtyp in tc.type_aliases {
		return Type(Alias{
			name:      qtyp
			base_type: tc.parse_type(tc.type_aliases[qtyp])
		})
	}
	if qtyp in tc.structs {
		return Type(Struct{
			name: qtyp
		})
	}
	if typ in tc.type_aliases {
		return Type(Alias{
			name:      typ
			base_type: tc.parse_type(tc.type_aliases[typ])
		})
	}
	if typ in tc.interface_names {
		return Type(Interface{
			name: typ
		})
	}
	if qtyp in tc.interface_names {
		return Type(Interface{
			name: qtyp
		})
	}
	if typ in tc.structs {
		return Type(Struct{
			name: typ
		})
	}
	if qtyp in tc.structs {
		return Type(Struct{
			name: qtyp
		})
	}
	if typ in tc.flag_enums {
		return Type(Enum{
			name:    typ
			is_flag: true
		})
	}
	if qtyp in tc.flag_enums {
		return Type(Enum{
			name:    qtyp
			is_flag: true
		})
	}
	if typ in tc.enum_names {
		return Type(Enum{
			name: typ
		})
	}
	if qtyp in tc.enum_names {
		return Type(Enum{
			name: qtyp
		})
	}
	if typ in tc.sum_types {
		return Type(SumType{
			name: typ
		})
	}
	if qtyp in tc.sum_types {
		return Type(SumType{
			name: qtyp
		})
	}
	if !typ.contains('.') {
		if resolved := tc.unique_qualified_type_name(typ) {
			if resolved in tc.type_aliases {
				return Type(Alias{
					name:      resolved
					base_type: tc.parse_type(tc.type_aliases[resolved])
				})
			}
			if resolved in tc.structs {
				return Type(Struct{
					name: resolved
				})
			}
			if resolved in tc.interface_names {
				return Type(Interface{
					name: resolved
				})
			}
			if resolved in tc.flag_enums {
				return Type(Enum{
					name:    resolved
					is_flag: true
				})
			}
			if resolved in tc.enum_names {
				return Type(Enum{
					name: resolved
				})
			}
			if resolved in tc.sum_types {
				return Type(SumType{
					name: resolved
				})
			}
		}
	}
	if generic_type_application(typ) {
		bracket := typ.index_u8(`[`)
		base := typ[..bracket]
		qbase := tc.qualify_name(base)
		if qbase in tc.type_aliases {
			return Type(Alias{
				name:      qbase
				base_type: tc.parse_type(tc.type_aliases[qbase])
			})
		}
		if qbase in tc.structs {
			return Type(Struct{
				name: qbase
			})
		}
		if qbase in tc.interface_names {
			return Type(Interface{
				name: qbase
			})
		}
		if qbase in tc.sum_types {
			return Type(SumType{
				name: qbase
			})
		}
		if base in tc.type_aliases {
			return Type(Alias{
				name:      base
				base_type: tc.parse_type(tc.type_aliases[base])
			})
		}
		if base in tc.structs {
			return Type(Struct{
				name: base
			})
		}
		if base in tc.interface_names {
			return Type(Interface{
				name: base
			})
		}
		if base in tc.sum_types {
			return Type(SumType{
				name: base
			})
		}
	}
	if typ.contains('[') && !typ.starts_with('[') {
		bracket := typ.index_u8(`[`)
		bracket_end := typ.index_u8(`]`)
		if bracket_end > bracket {
			len_text := typ[bracket + 1..bracket_end].trim_space()
			return Type(ArrayFixed{
				elem_type: tc.parse_type(typ[..bracket])
				len:       if is_decimal_int_literal(len_text) { len_text.int() } else { 0 }
				len_expr:  if is_decimal_int_literal(len_text) { '' } else { len_text }
			})
		}
	}
	if qtyp != typ {
		return Type(Struct{
			name: qtyp
		})
	}
	return Type(Struct{
		name: typ
	})
}

fn (tc &TypeChecker) unique_qualified_type_name(short_name string) ?string {
	if short_name.len == 0 {
		return none
	}
	mut found := ''
	for name, _ in tc.type_aliases {
		if name.all_after_last('.') == short_name {
			if found.len > 0 && found != name {
				return none
			}
			found = name
		}
	}
	for name, _ in tc.structs {
		if name.all_after_last('.') == short_name {
			if found.len > 0 && found != name {
				return none
			}
			found = name
		}
	}
	for name, _ in tc.interface_names {
		if name.all_after_last('.') == short_name {
			if found.len > 0 && found != name {
				return none
			}
			found = name
		}
	}
	for name, _ in tc.enum_names {
		if name.all_after_last('.') == short_name {
			if found.len > 0 && found != name {
				return none
			}
			found = name
		}
	}
	for name, _ in tc.sum_types {
		if name.all_after_last('.') == short_name {
			if found.len > 0 && found != name {
				return none
			}
			found = name
		}
	}
	if found.len == 0 {
		return none
	}
	return found
}

fn is_generic_placeholder_type(typ string) bool {
	if typ.contains('.') {
		last := typ.all_after_last('.')
		return is_generic_placeholder_type(last)
	}
	return typ in ['T', 'U', 'V', 'K']
}

fn (tc &TypeChecker) parse_fn_type(typ string) Type {
	params_start := typ.index_u8(`(`) + 1
	mut depth := 1
	mut params_end := params_start
	for params_end < typ.len {
		if typ[params_end] == `(` {
			depth++
		} else if typ[params_end] == `)` {
			depth--
			if depth == 0 {
				break
			}
		}
		params_end++
	}
	params_str := typ[params_start..params_end]
	ret_str := typ[params_end + 1..].trim_left(' ')
	mut params := []Type{}
	if params_str.len > 0 {
		param_parts := split_params(params_str)
		for p in param_parts {
			trimmed := p.trim_space()
			param_type := normalize_fn_type_param_text(trimmed)
			params << tc.parse_type(param_type)
		}
	}
	mut ret_type := Type(Void{})
	if ret_str.len > 0 {
		ret_type = tc.parse_type(ret_str)
	}
	return Type(FnType{
		params:      params
		return_type: ret_type
	})
}

pub fn (tc &TypeChecker) resolve_type(id flat.NodeId) Type {
	if int(id) < 0 {
		return unknown_type('missing node')
	}
	node := tc.a.nodes[int(id)]
	kind_id := node_kind_id(node)
	if kind_id == 1 {
		return Type(int_)
	}
	if kind_id == 2 {
		return Type(f64_)
	}
	if kind_id == 3 {
		return Type(bool_)
	}
	if kind_id == 4 {
		return Type(u8_)
	}
	if kind_id == 5 || kind_id == 6 {
		return Type(string_)
	}
	if kind_id == 28 {
		return Type(voidptr_)
	}
	if kind_id == 29 {
		return Type(OptionType{
			base_type: Type(void_)
		})
	}
	if kind_id == 21 {
		return tc.fn_literal_type(node)
	}
	if kind_id == 32 {
		return tc.lambda_expr_type(node)
	}
	if kind_id == 12 && node.typ.len > 0 {
		return tc.parse_type(node.typ)
	}
	if t := tc.resolved_call_type(id) {
		return t
	}
	if kind_id == 22 {
		inner := tc.resolve_type(tc.a.child(&node, 0))
		if inner is OptionType {
			return inner.base_type
		}
		if inner is ResultType {
			return inner.base_type
		}
		return inner
	}
	if smart_type := tc.smartcast_type(id) {
		return smart_type
	}
	if kind_id == 14 {
		return tc.resolve_index_type(node)
	}
	if kind_id != 7 && kind_id != 8 && kind_id != 13 && !(tc.smartcasts.len > 0
		&& (kind_id == 7 || kind_id == 13)) {
		if typ := tc.cached_expr_type(id) {
			return typ
		}
	}
	if node.typ.len > 0 && node.typ != 'unknown' {
		return tc.parse_type(node.typ)
	}
	match node.kind {
		.int_literal {
			return Type(int_)
		}
		.float_literal {
			return Type(f64_)
		}
		.bool_literal {
			return Type(bool_)
		}
		.char_literal {
			return Type(u8_)
		}
		.string_literal, .string_interp {
			return Type(string_)
		}
		.nil_literal {
			return Type(voidptr_)
		}
		.none_expr {
			return Type(OptionType{
				base_type: Type(void_)
			})
		}
		.enum_val {
			return Type(int_)
		}
		.ident {
			if smart_type := tc.smartcast_type(id) {
				return smart_type
			}
			if typ := tc.cur_scope.lookup(node.value) {
				return typ
			}
			qname := tc.qualify_name(node.value)
			if qname in tc.const_types {
				return tc.const_types[qname] or { unknown_type('unknown const `${qname}`') }
			}
			if node.value in tc.const_types {
				return tc.const_types[node.value] or {
					unknown_type('unknown const `${node.value}`')
				}
			}
			if typ := tc.fn_value_type(node.value) {
				return typ
			}
			return unknown_type('unknown identifier `${node.value}`')
		}
		.call {
			fn_node := tc.a.child_node(&node, 0)
			if fn_node.kind == .ident {
				if typ := tc.cur_scope.lookup(fn_node.value) {
					if typ is FnType {
						return typ.return_type
					}
				}
			}
			if fn_node.kind == .selector {
				base_node := tc.a.child_node(fn_node, 0)
				if base_node.kind == .ident && base_node.value == 'C' {
					c_fn_name := 'C.${fn_node.value}'
					if c_fn_name in tc.fn_ret_types {
						return tc.fn_ret_types[c_fn_name] or {
							unknown_type('unknown return type for `${c_fn_name}`')
						}
					}
					if fn_node.value in tc.fn_ret_types {
						return tc.fn_ret_types[fn_node.value] or {
							unknown_type('unknown return type for `${fn_node.value}`')
						}
					}
					return Type(Struct{
						name: c_fn_name
					})
				}
				if base_node.kind == .ident {
					if resolved := tc.resolve_import_alias(base_node.value) {
						mod_name := '${resolved}.${fn_node.value}'
						if mod_name in tc.fn_ret_types {
							return tc.fn_ret_types[mod_name] or {
								unknown_type('unknown return type for `${mod_name}`')
							}
						}
						if mod_name in tc.sum_types {
							return Type(SumType{
								name: mod_name
							})
						}
						if mod_name in tc.structs {
							return Type(Struct{
								name: mod_name
							})
						}
						if mod_name in tc.enum_names {
							return Type(Enum{
								name: mod_name
							})
						}
					}
					if base_node.value in tc.structs || base_node.value in tc.enum_names {
						qname := tc.qualify_name(base_node.value)
						sname := '${qname}.${fn_node.value}'
						if sname in tc.fn_ret_types {
							return tc.fn_ret_types[sname] or {
								unknown_type('unknown return type for `${sname}`')
							}
						}
					} else {
						qname := tc.qualify_name(base_node.value)
						if qname in tc.structs || qname in tc.enum_names {
							sname := '${qname}.${fn_node.value}'
							if sname in tc.fn_ret_types {
								return tc.fn_ret_types[sname] or {
									unknown_type('unknown return type for `${sname}`')
								}
							}
						}
					}
				} else if base_node.kind == .selector {
					inner := tc.a.child_node(base_node, 0)
					if inner.kind == .ident {
						mod_name := tc.resolve_import_alias(inner.value) or { inner.value }
						full_name := '${mod_name}.${base_node.value}.${fn_node.value}'
						if full_name in tc.fn_ret_types {
							return tc.fn_ret_types[full_name] or {
								unknown_type('unknown return type for `${full_name}`')
							}
						}
					}
				}
				if fn_typ := tc.selector_fn_type(fn_node) {
					return fn_typ.return_type
				}
				base_type := tc.resolve_type(tc.a.child(fn_node, 0))
				clean_type := unwrap_pointer(base_type)
				if clean_type is Array {
					if fn_node.value == 'clone' || fn_node.value == 'reverse' {
						return base_type
					}
					if fn_node.value == 'filter' || fn_node.value == 'sorted' {
						return base_type
					}
					if fn_node.value in ['any', 'all'] {
						return Type(bool_)
					}
					if fn_node.value == 'count' {
						return Type(int_)
					}
					if fn_node.value == 'sort' {
						return Type(void_)
					}
					if fn_node.value == 'last' || fn_node.value == 'first' || fn_node.value == 'pop' {
						return array_elem_type(clean_type)
					}
					if fn_node.value == 'contains' {
						return Type(bool_)
					}
					if fn_node.value == 'repeat' || fn_node.value == 'repeat_to_depth' {
						return base_type
					}
					if fn_node.value == 'index' {
						return Type(int_)
					}
					if fn_node.value == 'join' || fn_node.value == 'str' {
						return Type(string_)
					}
					if fn_node.value == 'clone' {
						return base_type
					}
					elem_type := array_elem_type(clean_type)
					elem_name := elem_type.name()
					mut short_elem := elem_name
					mut mod_prefix := ''
					if elem_name.contains('.') {
						short_elem = elem_name.all_after_last('.')
						mod_prefix = elem_name.all_before_last('.')
					}
					arr_mname1 := '[]${short_elem}.${fn_node.value}'
					if mod_prefix.len > 0 {
						arr_mkey := '${mod_prefix}.${arr_mname1}'
						if arr_mkey in tc.fn_ret_types {
							return tc.fn_ret_types[arr_mkey] or {
								unknown_type('unknown return type for `${arr_mkey}`')
							}
						}
					}
					if arr_mname1 in tc.fn_ret_types {
						return tc.fn_ret_types[arr_mname1] or {
							unknown_type('unknown return type for `${arr_mname1}`')
						}
					}
					return unknown_type('unknown array method `${fn_node.value}`')
				}
				if clean_type is Map {
					if fn_node.value == 'clone' {
						return base_type
					}
					return unknown_type('unknown map method `${fn_node.value}`')
				}
				if clean_type is String {
					mname := 'string.${fn_node.value}'
					if mname in tc.fn_ret_types {
						return tc.fn_ret_types[mname] or {
							unknown_type('unknown return type for `${mname}`')
						}
					}
				}
				if (clean_type is Void || clean_type is Primitive)
					&& fn_node.value in ['vstring', 'vstring_with_len'] {
					return Type(string_)
				}
				if clean_type is Alias {
					mname := '${clean_type.name}.${fn_node.value}'
					if mname in tc.fn_ret_types {
						return tc.fn_ret_types[mname] or {
							unknown_type('unknown return type for `${mname}`')
						}
					}
					base_name := resolve_type_name_for_method(clean_type.base_type)
					if base_name.len > 0 {
						base_mname := '${base_name}.${fn_node.value}'
						if base_mname in tc.fn_ret_types {
							return tc.fn_ret_types[base_mname] or {
								unknown_type('unknown return type for `${base_mname}`')
							}
						}
					}
				}
				if clean_type is Struct {
					mname := '${clean_type.name}.${fn_node.value}'
					if mname in tc.fn_ret_types {
						return tc.fn_ret_types[mname] or {
							unknown_type('unknown return type for `${mname}`')
						}
					}
				}
				if clean_type is Interface {
					mname := '${clean_type.name}.${fn_node.value}'
					if mname in tc.fn_ret_types {
						return tc.fn_ret_types[mname] or {
							unknown_type('unknown return type for `${mname}`')
						}
					}
				}
				if clean_type is SumType {
					mname := '${clean_type.name}.${fn_node.value}'
					if mname in tc.fn_ret_types {
						return tc.fn_ret_types[mname] or {
							unknown_type('unknown return type for `${mname}`')
						}
					}
				}
				if clean_type is Enum {
					if fn_node.value == 'str' {
						return Type(string_)
					}
					mname := '${clean_type.name}.${fn_node.value}'
					if mname in tc.fn_ret_types {
						return tc.fn_ret_types[mname] or {
							unknown_type('unknown return type for `${mname}`')
						}
					}
				}
				if clean_type is Primitive {
					mname := '${prim_c_type_from(clean_type.props, clean_type.size)}.${fn_node.value}'
					if mname in tc.fn_ret_types {
						return tc.fn_ret_types[mname] or {
							unknown_type('unknown return type for `${mname}`')
						}
					}
				}
			}
			qfn := tc.qualify_fn_name(fn_node.value)
			if qfn in tc.fn_ret_types {
				return tc.fn_ret_types[qfn] or { unknown_type('unknown return type for `${qfn}`') }
			}
			if fn_node.value in tc.fn_ret_types {
				return tc.fn_ret_types[fn_node.value] or {
					unknown_type('unknown return type for `${fn_node.value}`')
				}
			}
			if node.typ.len > 0 {
				return tc.parse_type(node.typ)
			}
			$if debug {
				eprintln('warning: unknown fn return type `${fn_node.value}`')
			}
			return unknown_type('unknown function `${fn_node.value}`')
		}
		.infix {
			if node.op in [.eq, .ne, .lt, .gt, .le, .ge, .logical_and, .logical_or] {
				return Type(bool_)
			}
			lt := tc.resolve_type(tc.a.child(&node, 0))
			lt_raw := lt
			if lt is String {
				return lt_raw
			}
			rt := tc.resolve_type(tc.a.child(&node, 1))
			rt_raw := rt
			if rt is String {
				return rt_raw
			}
			if lt.is_float() || rt.is_float() {
				return Type(f64_)
			}
			return lt
		}
		.prefix {
			if node.typ.len > 0 {
				return tc.parse_type(node.typ)
			}
			if node.op == .amp {
				inner := tc.resolve_type(tc.a.child(&node, 0))
				return Type(Pointer{
					base_type: inner
				})
			}
			if node.op == .mul {
				inner := tc.resolve_type(tc.a.child(&node, 0))
				if inner is Pointer {
					return inner.base_type
				}
				return inner
			}
			if node.op == .arrow {
				inner := tc.resolve_type(tc.a.child(&node, 0))
				if inner is Channel {
					return inner.elem_type
				}
			}
			return tc.resolve_type(tc.a.child(&node, 0))
		}
		.paren {
			return tc.resolve_type(tc.a.child(&node, 0))
		}
		.or_expr {
			inner := tc.resolve_type(tc.a.child(&node, 0))
			if inner is OptionType {
				return inner.base_type
			}
			if inner is ResultType {
				return inner.base_type
			}
			return inner
		}
		.struct_init {
			return tc.parse_type(node.value)
		}
		.assoc {
			if node.value.len > 0 {
				return tc.parse_type(node.value)
			}
			if node.children_count > 0 {
				return tc.resolve_type(tc.a.child(&node, 0))
			}
			return unknown_type('missing assoc base')
		}
		.sizeof_expr {
			return Type(USize{})
		}
		.cast_expr {
			return tc.parse_type(node.value)
		}
		.selector {
			if smart_type := tc.smartcast_type(id) {
				return smart_type
			}
			if typ := tc.enum_selector_type(&node) {
				return typ
			}
			base_node := tc.a.child_node(&node, 0)
			if base_node.kind == .ident {
				if gt := tc.file_scope.lookup(node.value) {
					if gt !is Unknown {
						return gt
					}
				}
				resolved := tc.resolve_import_alias(base_node.value) or { base_node.value }
				qname := '${resolved}.${node.value}'
				if qname.starts_with('C.') {
					if gt := tc.c_globals[qname] {
						return gt
					}
				}
				if qname in tc.const_types {
					typ := tc.const_types[qname] or { unknown_type('unknown const `${qname}`') }
					return tc.const_type_from_initializer(qname, typ)
				}
				if key := tc.const_key_for_suffix(qname) {
					typ := tc.const_types[key] or { unknown_type('unknown const `${key}`') }
					return tc.const_type_from_initializer(key, typ)
				}
			}
			base_type := tc.resolve_type(tc.a.child(&node, 0))
			clean0 := unwrap_pointer(base_type)
			mut clean := clean0
			if clean0 is Alias {
				clean = clean0.base_type
			}
			if typ := option_result_selector_type(clean, node.value) {
				return typ
			}
			if node.value == 'len' {
				if clean is Array || clean is Map || clean is String || clean is ArrayFixed {
					return Type(int_)
				}
			}
			if clean is Struct {
				if clean.name in tc.structs {
					for f in tc.structs[clean.name] {
						if f.name == node.value {
							return f.typ
						}
					}
				}
			}
			if clean is Interface {
				if typ := tc.interface_field_type(clean.name, node.value) {
					return typ
				}
			}
			if clean is MultiReturn {
				if typ := multi_return_selector_type(clean, node.value) {
					return typ
				}
			}
			if clean is SumType {
				if typ := tc.lowered_sum_selector_type(clean, node.value) {
					return typ
				}
				if typ := tc.sum_shared_field_type(clean, node.value) {
					return typ
				}
			}
			if clean is Array || clean is Map || clean is String {
				sname := if clean is Array {
					'array'
				} else if clean is Map {
					'map'
				} else {
					'string'
				}
				if sname in tc.structs {
					for f in tc.structs[sname] {
						if f.name == node.value {
							return f.typ
						}
					}
				}
			}
			if clean is Primitive && base_node.kind == .selector {
				vname := base_node.value.replace('__', '.')
				if vname in tc.structs {
					for f in tc.structs[vname] {
						if f.name == node.value {
							return f.typ
						}
					}
				}
			}
			return unknown_type('unknown selector `${node.value}`')
		}
		.array_literal {
			if node.children_count > 0 {
				elem_type := tc.resolve_type(tc.a.child(&node, 0))
				return Type(ArrayFixed{
					elem_type: elem_type
					len:       node.children_count
				})
			}
			return Type(ArrayFixed{
				elem_type: Type(int_)
				len:       0
			})
		}
		.index {
			return tc.resolve_index_type(node)
		}
		.array_init {
			t := tc.parse_type(node.value)
			raw_t := t
			if t is ArrayFixed {
				return raw_t
			}
			return Type(Array{
				elem_type: t
			})
		}
		.map_init {
			if node.value.len > 0 {
				return tc.parse_type(node.value)
			}
			if node.children_count >= 2 {
				key_type := tc.resolve_type(tc.a.child(&node, 0))
				mut value_type := tc.resolve_type(tc.a.child(&node, 1))
				if value_type is ArrayFixed {
					value_type = Type(Array{
						elem_type: value_type.elem_type
					})
				}
				return Type(Map{
					key_type:   key_type
					value_type: value_type
				})
			}
			return Type(Map{
				key_type:   Type(string_)
				value_type: Type(int_)
			})
		}
		.if_expr {
			return tc.if_expr_tail_type(id)
		}
		.match_stmt {
			for i in 1 .. node.children_count {
				t := tc.branch_tail_type(tc.a.child(&node, i))
				if t !is Void {
					return t
				}
			}
			return Type(void_)
		}
		.in_expr {
			return Type(bool_)
		}
		.block {
			if node.children_count > 0 {
				last_id := tc.a.child(&node, node.children_count - 1)
				last := tc.a.nodes[int(last_id)]
				if last.kind == .expr_stmt {
					return tc.resolve_type(tc.a.child(&last, 0))
				}
				return tc.resolve_type(last_id)
			}
			return Type(void_)
		}
		.as_expr {
			return tc.parse_type(node.value)
		}
		.is_expr {
			return Type(bool_)
		}
		else {
			$if debug {
				eprintln('warning: unhandled node kind .${node.kind} in resolve_type')
			}
			return unknown_type('unhandled node kind .${node.kind}')
		}
	}
}

fn (tc &TypeChecker) fn_literal_type(node flat.Node) Type {
	mut params := []Type{}
	for i in 0 .. node.children_count {
		child := tc.a.child_node(&node, i)
		if child.kind == .param {
			params << tc.parse_type(child.typ)
		}
	}
	return Type(FnType{
		params:      params
		return_type: tc.parse_type(node.typ)
	})
}

fn (tc &TypeChecker) lambda_expr_type(node flat.Node) Type {
	mut params := []Type{}
	if node.children_count > 0 {
		for _ in 0 .. node.children_count - 1 {
			params << unknown_type('lambda parameter')
		}
	}
	ret_type := if node.children_count > 0 {
		tc.resolve_type(tc.a.child(&node, node.children_count - 1))
	} else {
		Type(void_)
	}
	return Type(FnType{
		params:      params
		return_type: ret_type
	})
}

fn (tc &TypeChecker) resolve_index_type(node flat.Node) Type {
	base_type0 := tc.resolve_type(tc.a.child(&node, 0))
	mut base_type := base_type0
	if base_type0 is Alias {
		base_type = base_type0.base_type
	}
	base_type_raw := base_type
	if node.value == 'range' {
		if base_type is Array {
			return base_type_raw
		}
		return Type(string_)
	}
	if base_type is Map {
		return map_value_type(base_type)
	}
	if base_type is Array {
		return array_elem_type(base_type)
	}
	if base_type is ArrayFixed {
		return fixed_array_elem_type(base_type)
	}
	if base_type is Pointer {
		inner0 := pointer_base_type(base_type)
		mut inner := inner0
		if inner0 is Alias {
			inner = inner0.base_type
		}
		if inner is Array {
			return array_elem_type(inner)
		}
		if inner is ArrayFixed {
			return fixed_array_elem_type(inner)
		}
		return inner
	}
	if base_type is String {
		return Type(u8_)
	}
	return unknown_type('cannot index `${base_type.name()}`')
}

pub fn (tc &TypeChecker) c_type(t Type) string {
	if t is Pointer || t is FnType || t is Struct || t is Interface || t is SumType || t is Alias
		|| t is MultiReturn || t is ArrayFixed {
		if tc.type_cache != unsafe { nil } {
			key := t.name()
			mut cache := unsafe { tc.type_cache }
			if cached := cache.c_entries[key] {
				return cached
			}
			result := tc.c_type_uncached(t)
			cache.c_entries[key] = result
			return result
		}
	}
	return tc.c_type_uncached(t)
}

fn (tc &TypeChecker) c_type_uncached(t Type) string {
	if t is Void {
		return 'void'
	}
	if t is Unknown {
		return 'int'
	}
	if t is Nil {
		return 'void*'
	}
	if t is None {
		return 'Optional'
	}
	if t is String {
		return 'string'
	}
	if t is Char {
		return 'char'
	}
	if t is Rune {
		return 'i32'
	}
	if t is ISize {
		return 'ptrdiff_t'
	}
	if t is USize {
		return 'size_t'
	}
	if t is Primitive {
		return prim_c_type_from(t.props, t.size)
	}
	if t is Array {
		return 'Array'
	}
	if t is ArrayFixed {
		return if tc.has_builtins { 'array' } else { 'Array' }
	}
	if t is Channel {
		return 'chan'
	}
	if t is Map {
		return 'map'
	}
	if t is Pointer {
		return tc.c_type(t.base_type) + '*'
	}
	if t is FnType {
		ret := if t.return_type is Void { 'void' } else { tc.c_type(t.return_type) }
		if t.params.len == 0 {
			return 'fn_ptr:${ret}|void'
		}
		mut params := []string{}
		for i in 0 .. t.params.len {
			params << tc.c_type(fn_param_type(t, i))
		}
		return 'fn_ptr:${ret}|${params.join(', ')}'
	}
	if t is OptionType {
		return 'Optional'
	}
	if t is ResultType {
		return 'Optional'
	}
	if t is Struct {
		if t.name.starts_with('C.') {
			raw := t.name[2..]
			if raw.len > 0 && raw[0] >= `a` && raw[0] <= `z` && !raw.ends_with('_t') {
				return 'struct ${raw}'
			}
			return raw
		}
		return c_name(t.name)
	}
	if t is Interface {
		return c_name(t.name)
	}
	if t is Enum {
		return 'int'
	}
	if t is SumType {
		return c_name(t.name)
	}
	if t is Alias {
		return tc.c_type(t.base_type)
	}
	if t is MultiReturn {
		mut parts := []string{}
		for ty in t.types {
			parts << c_type_name_part(tc.c_type(ty))
		}
		return 'multi_return_${parts.join('_')}'
	}
	return 'int'
}

fn c_type_name_part(s string) string {
	return s.replace('*', 'ptr').replace(' ', '_').replace(',', '_').replace('|', '_').replace(':',
		'_')
}

fn resolve_type_name_for_method(t Type) string {
	if t is Alias {
		return t.name
	}
	if t is Struct {
		return t.name
	}
	if t is Interface {
		return t.name
	}
	if t is String {
		return 'string'
	}
	if t is Array {
		return 'array'
	}
	if t is Map {
		return 'map'
	}
	if t is Primitive {
		return prim_c_type_from(t.props, t.size)
	}
	return ''
}

fn prim_c_type_from(props Properties, size u8) string {
	if props.has(.boolean) {
		return 'bool'
	}
	if props.has(.integer) {
		if props.has(.unsigned) {
			return match size {
				8 { 'u8' }
				16 { 'u16' }
				32 { 'u32' }
				64 { 'u64' }
				else { 'u${size}' }
			}
		}
		return match size {
			0 { 'int' }
			8 { 'i8' }
			16 { 'i16' }
			32 { 'i32' }
			64 { 'i64' }
			else { 'i${size}' }
		}
	}
	if props.has(.float) {
		return match size {
			32 { 'float' }
			64 { 'double' }
			else { 'double' }
		}
	}
	return 'int'
}

fn prim_c_type(p Primitive) string {
	if p.props.has(.boolean) {
		return 'bool'
	}
	if p.props.has(.integer) {
		if p.props.has(.unsigned) {
			return match p.size {
				8 { 'u8' }
				16 { 'u16' }
				32 { 'u32' }
				64 { 'u64' }
				else { 'u${p.size}' }
			}
		}
		return match p.size {
			0 { 'int' }
			8 { 'i8' }
			16 { 'i16' }
			32 { 'i32' }
			64 { 'i64' }
			else { 'i${p.size}' }
		}
	}
	if p.props.has(.float) {
		return match p.size {
			32 { 'float' }
			64 { 'double' }
			else { 'double' }
		}
	}
	return 'int'
}

fn find_matching_bracket(s string, start int) int {
	mut depth := 1
	for i := start + 1; i < s.len; i++ {
		if s[i] == `[` {
			depth++
		}
		if s[i] == `]` {
			depth--
			if depth == 0 {
				return i
			}
		}
	}
	return s.len
}

fn split_params(s string) []string {
	mut parts := []string{}
	mut depth := 0
	mut start := 0
	for i := 0; i < s.len; i++ {
		match s[i] {
			`(`, `[` {
				depth++
			}
			`)`, `]` {
				depth--
			}
			`,` {
				if depth == 0 {
					parts << s[start..i]
					start = i + 1
				}
			}
			else {}
		}
	}
	if start < s.len {
		parts << s[start..]
	}
	return parts
}

fn normalize_fn_type_param_text(param string) string {
	mut text := param.trim_space()
	mut is_mut := false
	if text.starts_with('mut ') {
		is_mut = true
		text = text[4..].trim_space()
	}
	space := top_level_space_index(text)
	if space > 0 {
		head := text[..space].trim_space()
		tail := text[space + 1..].trim_space()
		if fn_type_param_head_is_name(head, tail) {
			text = tail
		}
	}
	if is_mut && text.len > 0 && !text.starts_with('&') {
		return '&' + text
	}
	return text
}

fn top_level_space_index(s string) int {
	mut depth := 0
	for i := 0; i < s.len; i++ {
		match s[i] {
			`(`, `[` {
				depth++
			}
			`)`, `]` {
				depth--
			}
			` ` {
				if depth == 0 {
					return i
				}
			}
			else {}
		}
	}
	return -1
}

fn fn_type_param_head_is_name(head string, tail string) bool {
	if head.len == 0 || tail.len == 0 {
		return false
	}
	if head.starts_with('fn') || head.starts_with('&') || head.starts_with('[') {
		return false
	}
	if head in ['shared', 'atomic', 'chan', 'thread', 'map'] || head.contains('.') {
		return false
	}
	if is_builtin_type_name(head) {
		return false
	}
	return (head[0] >= `a` && head[0] <= `z`) || head[0] == `_`
}

const c_reserved_words = ['auto', 'break', 'case', 'char', 'const', 'continue', 'copy', 'default',
	'do', 'double', 'else', 'enum', 'extern', 'float', 'for', 'goto', 'if', 'inline', 'int', 'long',
	'register', 'restrict', 'return', 'short', 'signed', 'sizeof', 'static', 'struct', 'switch',
	'typedef', 'union', 'unsigned', 'void', 'volatile', 'while']

fn c_name(name string) string {
	if name.starts_with('C.') {
		return name[2..]
	}
	if name == 'malloc' {
		return 'v_malloc'
	}
	n := name.replace('[]', 'Array_').replace('.-', '__minus').replace('.+', '__plus').replace('.==',
		'__eq').replace('.!=', '__ne').replace('.<=', '__le').replace('.>=', '__ge').replace('.<',
		'__lt').replace('.>', '__gt').replace('&', 'ptr').replace('[', '_').replace(']', '').replace(',', '_').replace(' ', '_').replace('.', '__')
	if n in c_reserved_words {
		return 'v_${n}'
	}
	return n
}
