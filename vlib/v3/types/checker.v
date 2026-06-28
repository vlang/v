module types

import v3.flat

const export_c_reserved_words = {
	'auto':     true
	'break':    true
	'case':     true
	'char':     true
	'const':    true
	'continue': true
	'default':  true
	'do':       true
	'double':   true
	'else':     true
	'enum':     true
	'extern':   true
	'float':    true
	'for':      true
	'goto':     true
	'if':       true
	'inline':   true
	'int':      true
	'long':     true
	'register': true
	'restrict': true
	'return':   true
	'short':    true
	'signed':   true
	'sizeof':   true
	'static':   true
	'struct':   true
	'switch':   true
	'typedef':  true
	'union':    true
	'unsigned': true
	'void':     true
	'volatile': true
	'while':    true
}

const export_v3_reserved_c_symbols = {
	'i8':            true
	'i16':           true
	'i32':           true
	'i64':           true
	'u8':            true
	'byte':          true
	'u16':           true
	'u32':           true
	'u64':           true
	'bool':          true
	'voidptr':       true
	'int_literal':   true
	'float_literal': true
	'chan':          true
	'string':        true
	'array':         true
	'Array':         true
	'map':           true
	'mapnode':       true
	'DenseArray':    true
	'SortedMap':     true
	'Optional':      true
	'IError':        true
	'true':          true
	'false':         true
	'elem_size':     true
	'c_name':        true
}

const export_c_libc_collision_symbols = {
	'rint':  true
	'y0':    true
	'y1':    true
	'yn':    true
	'j0':    true
	'j1':    true
	'jn':    true
	'drem':  true
	'scalb': true
}

// tarr1 supports tarr1 handling for types.
fn tarr1(a Type) []Type {
	mut r := []Type{}
	r << a
	return r
}

// tarr2 supports tarr2 handling for types.
fn tarr2(a Type, b Type) []Type {
	mut r := []Type{}
	r << a
	r << b
	return r
}

// tarr3 supports tarr3 handling for types.
fn tarr3(a Type, b Type, c Type) []Type {
	mut r := []Type{}
	r << a
	r << b
	r << c
	return r
}

// unknown_type supports unknown type handling for types.
fn unknown_type(reason string) Type {
	return Type(Unknown{
		reason: reason
	})
}

// TypeError represents type error data used by types.
pub struct TypeError {
pub:
	msg  string
	kind TypeErrorKind
	node flat.NodeId
}

// TypeErrorKind lists type error kind values used by types.
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

// CallInfo stores call info metadata used by types.
pub struct CallInfo {
pub:
	name                 string
	params               []Type
	return_type          Type
	has_receiver         bool
	is_variadic          bool
	is_c_variadic        bool
	params_known         bool
	has_implicit_veb_ctx bool
}

// LocalBinding represents local binding data used by types.
struct LocalBinding {
	name string
	typ  Type
}

// TypeCache represents type cache data used by types.
struct TypeCache {
mut:
	parse_enabled bool
	parse_entries map[string]Type
	c_entries     map[string]string
}

// TypeChecker represents type checker data used by types.
@[heap]
pub struct TypeChecker {
pub mut:
	a                      &flat.FlatAst = unsafe { nil }
	fn_ret_types           map[string]Type
	fn_param_types         map[string][]Type
	fn_ret_type_texts      map[string]string   // generic struct method key -> original return type text (e.g. `Box[T].clone` -> `Box[T]`)
	fn_param_type_texts    map[string][]string // generic struct method key -> original param type texts (receiver first)
	fn_type_files          map[string]string
	fn_type_modules        map[string]string
	fn_generic_params      map[string][]string
	fn_variadic            map[string]bool
	c_variadic_fns         map[string]bool
	fn_implicit_veb_ctx    map[string]bool
	structs                map[string][]StructField
	struct_generic_params  map[string][]string // generic struct base name -> type-param names (e.g. Vec4 -> [T])
	struct_field_c_abi_fns map[string]string
	// concrete `Box[int].method` -> substituted CallInfo for a method *value* on a
	// generic receiver. The open `Box[T].method` registration is gone by cgen time, so
	// the checker stashes the resolved signature here for gen_method_value_closure.
	generic_method_value_info  map[string]CallInfo
	params_structs             map[string]bool
	unions                     map[string]bool
	type_aliases               map[string]string
	type_alias_c_abi_fns       map[string]string
	sum_types                  map[string][]string
	enum_names                 map[string]bool
	enum_fields                map[string][]string
	flag_enums                 map[string]bool
	interface_names            map[string]bool
	interface_fields           map[string][]StructField
	interface_embeds           map[string][]string
	interface_abstract_methods map[string][]string // iface -> abstract (declared) method names

	c_globals               map[string]Type
	const_types             map[string]Type
	const_exprs             map[string]flat.NodeId
	const_modules           map[string]string
	const_files             map[string]string
	const_suffixes          map[string]string // dot-suffix -> full const key (O(1) lookup; '' if ambiguous)
	imports                 map[string]string // alias -> short module name
	file_imports            map[string]string
	file_selective_imports  map[string][]string
	file_modules            map[string]string
	file_scope              &Scope = unsafe { nil }
	cur_scope               &Scope = unsafe { nil }
	scope_pool              []&Scope
	scope_pool_index        int
	has_builtins            bool
	cur_module              string
	cur_file                string
	errors                  []TypeError
	resolved_call_names     []string // node_id -> resolved function name
	resolved_call_set       []bool
	resolved_fn_value_names []string // node_id -> resolved function value name
	resolved_fn_value_set   []bool
	// Methods used as *values* (`recv.method` passed as a callback), recorded per enclosing
	// function during semantic checking — which has full scope/type info, runs before
	// markused, and (unlike a call) routes a value-context selector through check_selector.
	// markused seeds these (keeping the wrapper-only method out of the dead-code pruner)
	// only when their enclosing function is reachable.
	method_values_by_fn map[int][]string // enclosing fn node id -> method-value `Type.method` keys
	// Local variables bound to a method value (`cb := c.report`) in the current function.
	// Such an alias shares the same per-site static receiver slot as the bare selector, so it
	// escapes (and corrupts other live callbacks) just like `return c.report`; the escape
	// checks below treat a reference to one of these locals as a method value. Reset per fn.
	method_value_locals map[string]bool
	// Scope depth at which each method-value local was marked, so a reassignment to a
	// non-method value only clears the marker when it dominates later uses (same-or-shallower
	// scope); a reassignment in a deeper conditional/loop scope keeps the maybe-method marker.
	method_value_local_depth      map[string]int
	cur_fn_node_id                int = -1
	expr_type_values              []Type // node_id -> complex/contextual resolved type
	expr_type_set                 []bool
	checking_nodes                []bool
	diagnose_unknown_calls        bool
	reject_unlowered_map_mutation bool
	reject_unsupported_generics   bool
	diagnostic_files              map[string]bool
	cur_fn_ret_type               Type = Type(void_)
	smartcasts                    map[string]Type
	selfhost                      bool
mut:
	type_cache &TypeCache = unsafe { nil }
}

// new creates a TypeChecker value for types.
pub fn TypeChecker.new(a &flat.FlatAst) TypeChecker {
	fs := new_scope(unsafe { nil })
	return TypeChecker{
		a:                          a
		fn_ret_types:               map[string]Type{}
		fn_param_types:             map[string][]Type{}
		fn_ret_type_texts:          map[string]string{}
		fn_param_type_texts:        map[string][]string{}
		fn_type_files:              map[string]string{}
		fn_type_modules:            map[string]string{}
		fn_generic_params:          map[string][]string{}
		fn_variadic:                map[string]bool{}
		c_variadic_fns:             map[string]bool{}
		fn_implicit_veb_ctx:        map[string]bool{}
		structs:                    map[string][]StructField{}
		struct_generic_params:      map[string][]string{}
		struct_field_c_abi_fns:     map[string]string{}
		generic_method_value_info:  map[string]CallInfo{}
		params_structs:             map[string]bool{}
		unions:                     map[string]bool{}
		type_aliases:               map[string]string{}
		type_alias_c_abi_fns:       map[string]string{}
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
		const_files:                map[string]string{}
		const_suffixes:             map[string]string{}
		imports:                    map[string]string{}
		file_imports:               map[string]string{}
		file_selective_imports:     map[string][]string{}
		file_modules:               map[string]string{}
		file_scope:                 fs
		cur_scope:                  fs
		resolved_call_names:        []string{len: a.nodes.len}
		resolved_call_set:          []bool{len: a.nodes.len}
		resolved_fn_value_names:    []string{len: a.nodes.len}
		resolved_fn_value_set:      []bool{len: a.nodes.len}
		method_values_by_fn:        map[int][]string{}
		method_value_locals:        map[string]bool{}
		method_value_local_depth:   map[string]int{}
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

// fork_for_parallel_transform returns a TypeChecker that shares all of `tc`'s
// read-only data (semantic maps and node-indexed cache arrays, which the transform
// pass only reads) but owns a fresh, private `type_cache` and a private AST view.
// During transform the only hidden mutation a TypeChecker performs through its `&`
// receiver is memoization into `type_cache` (parse_type / c_type); giving each
// worker its own cache makes concurrent use race-free without cloning the large
// semantic maps. `ast` must be the worker's own (cloned) FlatAst so that any
// expr_type lookup on a freshly-created node id indexes a valid array.
pub fn (tc &TypeChecker) fork_for_parallel_transform(ast &flat.FlatAst) &TypeChecker {
	mut forked := *tc
	forked.a = ast
	forked.type_cache = &TypeCache{
		parse_enabled: if tc.type_cache != unsafe { nil } {
			tc.type_cache.parse_enabled
		} else {
			false
		}
		parse_entries: map[string]Type{}
		c_entries:     map[string]string{}
	}
	return &forked
}

// reset_node_caches updates reset node caches state for types.
fn (mut tc TypeChecker) reset_node_caches(n int) {
	tc.resolved_call_names = []string{len: n}
	tc.resolved_call_set = []bool{len: n}
	tc.resolved_fn_value_names = []string{len: n}
	tc.resolved_fn_value_set = []bool{len: n}
	tc.expr_type_values = []Type{len: n, init: Type(void_)}
	tc.expr_type_set = []bool{len: n}
	tc.checking_nodes = []bool{len: n}
}

fn (mut tc TypeChecker) extend_node_caches(n int) {
	if n <= tc.resolved_call_names.len && n <= tc.resolved_fn_value_names.len
		&& n <= tc.expr_type_values.len && n <= tc.checking_nodes.len {
		return
	}
	extend_string_cache(mut tc.resolved_call_names, n)
	extend_bool_cache(mut tc.resolved_call_set, n)
	extend_string_cache(mut tc.resolved_fn_value_names, n)
	extend_bool_cache(mut tc.resolved_fn_value_set, n)
	extend_type_cache(mut tc.expr_type_values, n)
	extend_bool_cache(mut tc.expr_type_set, n)
	extend_bool_cache(mut tc.checking_nodes, n)
}

fn extend_string_cache(mut values []string, n int) {
	if n > values.len {
		values << []string{len: n - values.len}
	}
}

fn extend_bool_cache(mut values []bool, n int) {
	if n > values.len {
		values << []bool{len: n - values.len}
	}
}

fn extend_type_cache(mut values []Type, n int) {
	if n > values.len {
		values << []Type{len: n - values.len, init: Type(void_)}
	}
}

// push_scope updates push scope state for TypeChecker.
pub fn (mut tc TypeChecker) push_scope() {
	tc.cur_scope = tc.reuse_scope(tc.cur_scope)
}

// pop_scope updates pop scope state for TypeChecker.
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

// reuse_scope supports reuse scope handling for TypeChecker.
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

// record_error supports record error handling for TypeChecker.
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

fn (mut tc TypeChecker) record_error_unfiltered(kind TypeErrorKind, msg string, node flat.NodeId) {
	tc.errors << TypeError{
		msg:  msg
		kind: kind
		node: node
	}
}

fn (mut tc TypeChecker) record_unsupported_generic(msg string, node flat.NodeId) {
	if !tc.should_diagnose_unsupported_generic(node) {
		return
	}
	tc.errors << TypeError{
		msg:  msg
		kind: .unsupported_generic
		node: node
	}
}

// collect supports collect handling for TypeChecker.
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
				tc.imports[node.typ] = node.value
				tc.file_imports[file_import_key(tc.cur_file, node.typ)] = node.value
				tc.register_selective_imports(node)
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
				if node.generic_params.len > 0 {
					tc.struct_generic_params[qname] = node.generic_params.clone()
					if qname != node.value {
						tc.struct_generic_params[node.value] = node.generic_params.clone()
					}
				}
				if node.typ.contains('union') {
					tc.unions[qname] = true
				}
				if node.typ.contains('params') {
					tc.params_structs[qname] = true
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
					qname := tc.qualify_name(node.value)
					tc.type_aliases[qname] = tc.qualify_type_text(node.typ)
					if c_abi_fn := tc.c_abi_fn_ptr_type_from_text(node.typ) {
						tc.type_alias_c_abi_fns[qname] = c_abi_fn
					}
					if tc.cur_module in ['', 'main', 'builtin'] && node.value !in tc.type_aliases {
						tc.type_aliases[node.value] = tc.qualify_type_text(node.typ)
						if c_abi_fn := tc.c_abi_fn_ptr_type_from_text(node.typ) {
							tc.type_alias_c_abi_fns[node.value] = c_abi_fn
						}
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
				mut param_texts := []string{}
				mut is_variadic := false
				for i in 0 .. node.children_count {
					child := a.child_node(&node, i)
					if child.kind == .param {
						if child.typ.starts_with('...') {
							is_variadic = true
						}
						ptypes << tc.parse_type(child.typ)
						param_texts << child.typ
					}
				}
				needs_ctx := tc.fn_needs_implicit_veb_ctx(node)
				ptypes = tc.fn_param_types_with_implicit_veb_ctx(node, ptypes)
				tc.register_fn_signature(qname, ret_type, ptypes, is_variadic, needs_ctx)
				if tc.cur_module in ['', 'main', 'builtin'] && qname != node.value
					&& node.value !in tc.fn_param_types {
					tc.register_fn_signature(node.value, ret_type, ptypes, is_variadic, needs_ctx)
				}
				// A generic struct method (`Box[T].clone`) keeps its original signature
				// TEXT: the parsed types collapse a non-concrete `Box[T]` to the bare base,
				// so a concrete call must re-substitute the type arguments from the text to
				// recover applications like the receiver type in the return position
				// (`Box[T]` -> `Box[int]`). Only such methods carry `[` in their key.
				if node.generic_params.len > 0 || node.value.contains('[') {
					for name in [qname, node.value] {
						tc.fn_ret_type_texts[name] = node.typ
						tc.fn_param_type_texts[name] = param_texts.clone()
						tc.fn_type_files[name] = tc.cur_file
						tc.fn_type_modules[name] = tc.cur_module
						if node.generic_params.len > 0 {
							tc.fn_generic_params[name] = node.generic_params.clone()
						}
					}
				}
			}
			.struct_decl {
				mut fields := []StructField{}
				mut field_c_abi_fns := map[string]string{}
				for i in 0 .. node.children_count {
					f := a.child_node(&node, i)
					if f.kind != .field_decl {
						continue
					}
					field_typ := if f.typ.len > 0 { f.typ } else { f.value }
					mut typ := tc.parse_type(field_typ)
					if f.value == field_typ {
						typ = tc.resolve_known_field_type(field_typ, typ)
					}
					if c_abi_fn := tc.c_abi_fn_ptr_type_for_type_text(field_typ) {
						field_c_abi_fns[f.value] = c_abi_fn
					}
					fields << StructField{
						name: f.value
						typ:  typ
					}
				}
				qname := tc.qualify_name(node.value)
				tc.structs[qname] = fields
				for field_name, c_abi_fn in field_c_abi_fns {
					tc.struct_field_c_abi_fns[struct_field_c_abi_key(qname, field_name)] = c_abi_fn
				}
				if node.typ.contains('union') {
					tc.unions[qname] = true
				}
				if node.typ.contains('params') {
					tc.params_structs[qname] = true
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
				tc.register_fn_signature(node.value, ret_type, ptypes, is_variadic, false)
				if is_variadic {
					tc.register_c_variadic_fn(node.value)
				}
				if !node.value.starts_with('C.') {
					tc.register_fn_signature('C.${node.value}', ret_type, ptypes, is_variadic,
						false)
					if is_variadic {
						tc.register_c_variadic_fn('C.${node.value}')
					}
				}
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
						tc.const_files[qname] = tc.cur_file
					} else if f.kind == .const_field && f.typ.len > 0 {
						qname := tc.qualify_name(f.value)
						tc.const_types[qname] = tc.parse_type(f.typ)
						tc.const_modules[qname] = tc.cur_module
						tc.const_files[qname] = tc.cur_file
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

// resolve_const_types resolves resolve const types information for types.
fn (mut tc TypeChecker) resolve_const_types() {
	if tc.const_exprs.len == 0 {
		return
	}
	saved_module := tc.cur_module
	saved_file := tc.cur_file
	for _ in 0 .. tc.const_exprs.len {
		mut changed := false
		for name, expr_id in tc.const_exprs {
			tc.cur_module = tc.const_modules[name] or { '' }
			tc.cur_file = tc.const_files[name] or { '' }
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
	tc.cur_file = saved_file
}

// const_type_from_initializer converts const type from initializer data for types.
fn (tc &TypeChecker) const_type_from_initializer(name string, typ Type) Type {
	if typ !is Unknown {
		return typ
	}
	expr_id := tc.const_exprs[name] or { return typ }
	if int(expr_id) < 0 || int(expr_id) >= tc.a.nodes.len {
		return typ
	}
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

fn (tc &TypeChecker) const_type_for_name(name string) ?Type {
	qname := tc.qualify_name(name)
	if typ := tc.const_types[qname] {
		return tc.const_type_from_initializer(qname, typ)
	}
	if typ := tc.const_types[name] {
		return tc.const_type_from_initializer(name, typ)
	}
	return none
}

fn (tc &TypeChecker) const_type_for_selector(node flat.Node) ?Type {
	if node.kind != .selector || node.children_count == 0 {
		return none
	}
	base_node := tc.a.child_node(&node, 0)
	if base_node.kind != .ident {
		return none
	}
	resolved := tc.resolve_import_alias(base_node.value) or { base_node.value }
	qname := '${resolved}.${node.value}'
	if typ := tc.const_types[qname] {
		return tc.const_type_from_initializer(qname, typ)
	}
	if key := tc.const_key_for_suffix(qname) {
		typ := tc.const_types[key] or { unknown_type('unknown const `${key}`') }
		return tc.const_type_from_initializer(key, typ)
	}
	return none
}

// qualify_fn_name supports qualify fn name handling for TypeChecker.
pub fn (tc &TypeChecker) qualify_fn_name(name string) string {
	if tc.cur_module.len == 0 || tc.cur_module == 'main' || tc.cur_module == 'builtin' {
		return name
	}
	return '${tc.cur_module}.${name}'
}

// qualify_name supports qualify name handling for TypeChecker.
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
	if name.starts_with('!') {
		return '!' + tc.qualify_name(name[1..])
	}
	if name.contains('.') {
		return tc.resolve_imported_type_text(name)
	}
	if is_builtin_type_name(name) {
		return name
	}
	return tc.cur_module + '.' + name
}

// qualify_type_text supports qualify type text handling for TypeChecker.
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

// qualify_fn_type_text supports qualify fn type text handling for TypeChecker.
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

// file_import_key supports file import key handling for types.
fn file_import_key(file string, alias string) string {
	return '${file}\n${alias}'
}

// enter_file supports enter file handling for TypeChecker.
fn (mut tc TypeChecker) enter_file(file string) {
	tc.cur_file = file
	tc.cur_module = tc.file_modules[file] or { '' }
}

// enter_module supports enter module handling for TypeChecker.
fn (mut tc TypeChecker) enter_module(name string) {
	tc.cur_module = name
	if tc.cur_file.len > 0 {
		tc.file_modules[tc.cur_file] = name
	}
}

fn (mut tc TypeChecker) register_selective_imports(node flat.Node) {
	if node.children_count == 0 {
		return
	}
	for i in 0 .. node.children_count {
		child_id := tc.a.child(&node, i)
		child := tc.a.nodes[int(child_id)]
		if child.kind != .ident {
			continue
		}
		key := file_import_key(tc.cur_file, child.value)
		if key in tc.file_selective_imports {
			tc.file_selective_imports[key] = []string{}
			tc.record_error_unfiltered(.unknown_fn, 'ambiguous selective import `${child.value}`',
				child_id)
			continue
		}
		mut candidates := []string{}
		path_name := '${node.value}.${child.value}'
		if path_name !in candidates {
			candidates << path_name
		}
		alias_name := '${node.typ}.${child.value}'
		if alias_name != path_name && alias_name !in candidates {
			candidates << alias_name
		}
		tc.file_selective_imports[key] = candidates
	}
}

// resolve_import_alias resolves resolve import alias information for types.
fn (tc &TypeChecker) resolve_import_alias(alias string) ?string {
	if mod := tc.file_imports[file_import_key(tc.cur_file, alias)] {
		return mod
	}
	return none
}

fn (tc &TypeChecker) resolve_selective_import_symbol(name string) ?string {
	candidates := tc.file_selective_imports[file_import_key(tc.cur_file, name)] or { return none }
	for candidate in candidates {
		if tc.fn_signature_known(candidate) {
			return candidate
		}
	}
	return none
}

fn (tc &TypeChecker) resolve_selective_import_type_symbol(name string) ?string {
	candidates := tc.file_selective_imports[file_import_key(tc.cur_file, name)] or { return none }
	for candidate in candidates {
		if tc.type_symbol_known(candidate) {
			return candidate
		}
	}
	return none
}

fn (tc &TypeChecker) type_symbol_known(name string) bool {
	return name in tc.type_aliases || name in tc.structs || name in tc.interface_names
		|| name in tc.flag_enums || name in tc.enum_names || name in tc.sum_types
}

fn (tc &TypeChecker) type_from_known_symbol(name string) ?Type {
	if name in tc.type_aliases {
		return Type(Alias{
			name:      name
			base_type: tc.parse_type(tc.type_aliases[name])
		})
	}
	if name in tc.structs {
		return Type(Struct{
			name: name
		})
	}
	if name in tc.interface_names {
		return Type(Interface{
			name: name
		})
	}
	if name in tc.flag_enums {
		return Type(Enum{
			name:    name
			is_flag: true
		})
	}
	if name in tc.enum_names {
		return Type(Enum{
			name: name
		})
	}
	if name in tc.sum_types {
		return Type(SumType{
			name: name
		})
	}
	return none
}

fn (tc &TypeChecker) selective_import_symbol_is_ambiguous(name string) bool {
	candidates := tc.file_selective_imports[file_import_key(tc.cur_file, name)] or { return false }
	return candidates.len == 0
}

fn (tc &TypeChecker) resolve_imported_type_text(typ string) string {
	if !typ.contains('.') || typ.starts_with('C.') {
		return typ
	}
	dot := typ.index_u8(`.`)
	if dot <= 0 {
		return typ
	}
	alias := typ[..dot]
	if resolved := tc.resolve_import_alias(alias) {
		if resolved != alias {
			return resolved + typ[dot..]
		}
	}
	return typ
}

// has_active_import reports whether has active import applies in types.
fn (tc &TypeChecker) has_active_import(alias string) bool {
	return file_import_key(tc.cur_file, alias) in tc.file_imports
}

// register_fn_signature updates register fn signature state for types.
fn (mut tc TypeChecker) register_fn_signature(name string, ret_type Type, params []Type, is_variadic bool, implicit_veb_ctx bool) {
	tc.register_fn_name_alias(name, ret_type, params, is_variadic, implicit_veb_ctx)
	lowered_name := c_name(name)
	if lowered_name != name {
		tc.register_fn_name_alias(lowered_name, ret_type, params, is_variadic, implicit_veb_ctx)
	}
	if name.ends_with('.str') {
		receiver := name.all_before_last('.')
		legacy_name := '${receiver}_str'
		if !legacy_name.contains('.') {
			tc.register_fn_name_alias(legacy_name, ret_type, params, is_variadic, implicit_veb_ctx)
		}
	}
}

// register_fn_name_alias updates register fn name alias state for types.
fn (mut tc TypeChecker) register_fn_name_alias(name string, ret_type Type, params []Type, is_variadic bool, implicit_veb_ctx bool) {
	tc.fn_ret_types[name] = ret_type
	tc.fn_param_types[name] = params.clone()
	tc.fn_variadic[name] = is_variadic
	tc.fn_implicit_veb_ctx[name] = implicit_veb_ctx
}

fn (mut tc TypeChecker) register_c_variadic_fn(name string) {
	if name.len == 0 {
		return
	}
	tc.c_variadic_fns[name] = true
	lowered_name := c_name(name)
	if lowered_name != name {
		tc.c_variadic_fns[lowered_name] = true
	}
	if !name.starts_with('C.') {
		tc.c_variadic_fns['C.${name}'] = true
	}
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
	tc.annotate_types_with_used(map[string]bool{})
}

// annotate_types_with_used annotates only functions that can be emitted when
// `used_fns` is non-empty. This mirrors transform/cgen pruning and avoids
// resolving types in dead, untransformed function bodies after markused.
pub fn (mut tc TypeChecker) annotate_types_with_used(used_fns map[string]bool) {
	tc.extend_node_caches(tc.a.nodes.len)
	tc.cur_module = ''
	for node in tc.a.nodes {
		if node.kind == .file {
			tc.enter_file(node.value)
		} else if node.kind == .module_decl {
			tc.enter_module(node.value)
		} else if node.kind == .fn_decl {
			if !tc.should_annotate_fn(node, used_fns) {
				continue
			}
			tc.cur_scope = tc.file_scope
			tc.push_scope()
			for pi in 0 .. node.children_count {
				p := tc.a.child_node(&node, pi)
				if p.kind == .param && p.value.len > 0 {
					tc.cur_scope.insert(p.value, tc.parse_type(p.typ))
				}
			}
			tc.insert_implicit_veb_ctx(node)
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

fn (tc &TypeChecker) should_annotate_fn(node flat.Node, used_fns map[string]bool) bool {
	if used_fns.len == 0 {
		return true
	}
	qname := checker_qualified_fn_name(tc.cur_module, node.value)
	if qname in tc.a.export_fn_names || tc.fn_needs_implicit_veb_ctx(node) {
		return true
	}
	if node.value in used_fns {
		return true
	}
	if qname in used_fns {
		return true
	}
	cname := c_name(qname)
	if cname != qname && cname in used_fns {
		return true
	}
	return false
}

fn checker_qualified_fn_name(mod string, name string) string {
	if mod.len == 0 || mod == 'main' || mod == 'builtin' {
		return name
	}
	return '${mod}.${name}'
}

// annotate_node supports annotate node handling for TypeChecker.
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
						tc.annotate_expected_expr(rhs_id, typ)
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
		.assign, .selector_assign, .index_assign {
			tc.annotate_assign_expected_exprs(node)
		}
		.struct_init {
			tc.annotate_struct_init_expected_exprs(node)
		}
		.call {
			tc.annotate_call_expected_exprs(id, node)
		}
		else {}
	}

	tc.remember_expr_type(id, tc.resolve_type(id))
	for i in 0 .. node.children_count {
		tc.annotate_node(tc.a.child(&node, i))
	}
}

fn (mut tc TypeChecker) annotate_expected_expr(id flat.NodeId, expected Type) {
	if _ := fn_type_from_type(expected) {
		_ = tc.resolve_expr(id, expected)
	}
}

fn (mut tc TypeChecker) annotate_assign_expected_exprs(node flat.Node) {
	if node.children_count < 2 {
		return
	}
	mut i := 0
	for i + 1 < node.children_count {
		lhs_id := tc.a.child(&node, i)
		rhs_id := tc.a.child(&node, i + 1)
		lhs_type := tc.resolve_lvalue_type(lhs_id)
		tc.annotate_expected_expr(rhs_id, lhs_type)
		i += 2
	}
}

fn (mut tc TypeChecker) annotate_struct_init_expected_exprs(node flat.Node) {
	init_type := tc.parse_type(node.value)
	init_struct := struct_type_from_type(init_type) or { return }
	init_name := init_struct.name
	fields := tc.structs[init_name] or { []StructField{} }
	for i in 0 .. node.children_count {
		field := tc.a.child_node(&node, i)
		if field.kind != .field_init || field.children_count == 0 {
			continue
		}
		value_id := tc.a.child(field, 0)
		mut expected := Type(void_)
		if field.value.len > 0 {
			expected = tc.struct_field_type(init_name, field.value) or { Type(void_) }
		} else if i < fields.len {
			expected = fields[i].typ
		}
		tc.annotate_expected_expr(value_id, expected)
	}
}

fn (mut tc TypeChecker) annotate_call_expected_exprs(id flat.NodeId, node flat.Node) {
	info0 := tc.resolve_call_info(id, node) or { return }
	info := tc.specialized_plain_generic_call_info(node, info0)
	if info.name.len > 0 && !is_array_dsl_call_name(info.name) {
		tc.remember_resolved_call(id, info.name)
	}
	if !info.params_known || info.params.len == 0 {
		return
	}
	mut field_init_args := 0
	for i in 1 .. node.children_count {
		if tc.a.child_node(&node, i).kind == .field_init {
			field_init_args++
		}
	}
	collapsed := if field_init_args > 0 { 1 } else { 0 }
	recv_extra := if info.has_receiver { 1 } else { 0 }
	actual_count := node.children_count - 1 - field_init_args + collapsed + recv_extra
	ctx_count := if info.has_implicit_veb_ctx { 1 } else { 0 }
	ctx_omitted := ctx_count > 0 && actual_count < info.params.len
	for i in 1 .. node.children_count {
		raw_arg := tc.a.child_node(&node, i)
		arg_id := tc.call_arg_value(tc.a.child(&node, i))
		if raw_arg.kind == .field_init {
			tc.annotate_params_field_expected_expr(arg_id, raw_arg.value, info)
			continue
		}
		arg_shift := if ctx_omitted { ctx_count } else { 0 }
		param_idx := (if info.has_receiver { i } else { i - 1 }) + arg_shift
		if info.is_c_variadic && param_idx >= c_variadic_fixed_param_count(info) {
			continue
		}
		if param_idx >= info.params.len {
			continue
		}
		expected := tc.call_arg_expected_type(info, param_idx)
		tc.annotate_expected_expr(arg_id, expected)
	}
}

fn (tc &TypeChecker) call_arg_expected_type(info CallInfo, param_idx int) Type {
	expected := info.params[param_idx]
	if info.is_variadic && param_idx == info.params.len - 1 && expected is Array {
		return array_elem_type(expected)
	}
	return expected
}

fn (mut tc TypeChecker) annotate_params_field_expected_expr(arg_id flat.NodeId, field_name string, info CallInfo) {
	if field_name.len == 0 {
		return
	}
	for param in info.params {
		clean := unwrap_pointer(param)
		if clean is Struct {
			if expected := tc.struct_field_type(clean.name, field_name) {
				tc.annotate_expected_expr(arg_id, expected)
				return
			}
		}
	}
}

// annotate_for_in supports annotate for in handling for TypeChecker.
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
		} else if elem_type := iterator_for_in_elem_type(clean) {
			if has_val {
				tc.insert_loop_var(key_id, Type(int_))
				tc.insert_loop_var(val_id, elem_type)
			} else {
				tc.insert_loop_var(key_id, elem_type)
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

fn iterator_for_in_elem_type(typ Type) ?Type {
	name := typ.name()
	if name == 'RunesIterator' || name == 'builtin.RunesIterator' {
		return Type(rune_)
	}
	return none
}

// insert_loop_var updates insert loop var state for types.
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
		if node.kind == .call && node.typ.len > 0 && node.typ !in ['int', 'array', 'map', 'unknown'] {
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

// resolved_call_type supports resolved call type handling for TypeChecker.
fn (tc &TypeChecker) resolved_call_type(id flat.NodeId) ?Type {
	if int(id) < 0 {
		return none
	}
	node := tc.a.nodes[int(id)]
	if node.kind != .call {
		return none
	}
	if t := tc.cached_expr_type(id) {
		return t
	}
	if name := tc.cached_resolved_call(id) {
		if t := tc.fn_ret_types[name] {
			return t
		}
	}
	return none
}

// cached_expr_type supports cached expr type handling for TypeChecker.
fn (tc &TypeChecker) cached_expr_type(id flat.NodeId) ?Type {
	idx := int(id)
	if idx >= 0 && idx < tc.expr_type_set.len && tc.expr_type_set[idx] {
		return tc.expr_type_values[idx]
	}
	return none
}

// cached_resolved_call supports cached resolved call handling for TypeChecker.
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

// resolved_fn_value_name returns the checker-resolved function name for a function value node.
pub fn (tc &TypeChecker) resolved_fn_value_name(id flat.NodeId) ?string {
	idx := int(id)
	if idx >= 0 && idx < tc.resolved_fn_value_set.len && tc.resolved_fn_value_set[idx] {
		return tc.resolved_fn_value_names[idx]
	}
	return none
}

// resolve_fn_value_name_for_expected resolves and records a function value in an expected FnType context.
fn (mut tc TypeChecker) resolve_fn_value_name_for_expected(id flat.NodeId, expected Type) ?string {
	if name := tc.resolved_fn_value_name(id) {
		return name
	}
	if int(id) < 0 || int(id) >= tc.a.nodes.len {
		return none
	}
	node := tc.a.nodes[int(id)]
	if tc.fn_value_shadowed_by_value(node) {
		return none
	}
	key := tc.fn_value_match_key(node, expected) or { return none }
	tc.remember_resolved_fn_value_chain(id, key)
	return key
}

// remember_resolved_call supports remember resolved call handling for TypeChecker.
fn (mut tc TypeChecker) remember_resolved_call(id flat.NodeId, name string) {
	idx := int(id)
	if idx < 0 {
		return
	}
	if idx >= tc.resolved_call_names.len {
		tc.extend_node_caches(tc.a.nodes.len)
	}
	if idx < tc.resolved_call_names.len {
		tc.resolved_call_names[idx] = name
		tc.resolved_call_set[idx] = true
	}
}

// remember_resolved_fn_value records the exact function declaration used by a function value.
fn (mut tc TypeChecker) remember_resolved_fn_value(id flat.NodeId, name string) {
	idx := int(id)
	if idx < 0 {
		return
	}
	if idx >= tc.resolved_fn_value_names.len {
		tc.extend_node_caches(tc.a.nodes.len)
	}
	if idx < tc.resolved_fn_value_names.len {
		tc.resolved_fn_value_names[idx] = name
		tc.resolved_fn_value_set[idx] = true
	}
}

fn (mut tc TypeChecker) remember_resolved_fn_value_chain(id flat.NodeId, name string) {
	tc.remember_resolved_fn_value(id, name)
	if int(id) < 0 || int(id) >= tc.a.nodes.len {
		return
	}
	node := tc.a.nodes[int(id)]
	if node.kind in [.cast_expr, .paren, .expr_stmt] && node.children_count > 0 {
		tc.remember_resolved_fn_value_chain(tc.a.child(&node, 0), name)
	}
}

// register_synth_type records the type of a generated or transformed node.
pub fn (mut tc TypeChecker) register_synth_type(id flat.NodeId, typ Type) {
	tc.remember_expr_type(id, typ)
}

// remember_expr_type supports remember expr type handling for TypeChecker.
fn (mut tc TypeChecker) remember_expr_type(id flat.NodeId, typ Type) {
	if int(id) < 0 {
		return
	}
	kind := if int(id) < tc.a.nodes.len { tc.a.nodes[int(id)].kind } else { flat.NodeKind.empty }
	if should_cache_expr_type(kind, typ) {
		idx := int(id)
		if idx >= tc.expr_type_values.len {
			tc.extend_node_caches(tc.a.nodes.len)
		}
		if idx < tc.expr_type_values.len {
			tc.expr_type_values[idx] = typ
			tc.expr_type_set[idx] = true
		}
	}
}

// should_cache_expr_type reports whether should cache expr type applies in types.
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

// check_semantics validates check semantics state for types.
pub fn (mut tc TypeChecker) check_semantics() {
	tc.check_export_attrs()
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
				tc.cur_fn_node_id = i
				tc.method_value_locals = map[string]bool{}
				tc.method_value_local_depth = map[string]int{}
				tc.push_scope()
				for pi in 0 .. node.children_count {
					p := tc.a.child_node(&node, pi)
					if p.kind == .param && p.value.len > 0 {
						tc.cur_scope.insert(p.value, tc.parse_type(p.typ))
					}
				}
				tc.insert_implicit_veb_ctx(node)
				tc.check_fn_body(node)
				tc.cur_fn_node_id = -1
				is_disabled_stub := node.value in tc.a.disabled_fns
				if !type_allows_implicit_return(tc.cur_fn_ret_type)
					&& !tc.fn_body_definitely_returns(node) && !is_disabled_stub
					&& tc.should_diagnose(flat.NodeId(i)) {
					tc.record_error(.return_mismatch,
						'missing return at end of function `${node.value}`; expected `${tc.cur_fn_ret_type.name()}`',
						flat.NodeId(i))
				}
				tc.pop_scope()
				tc.cur_fn_ret_type = Type(void_)
			}
			.c_fn_decl {
				if tc.reject_unsupported_generics {
					tc.check_decl_type_strings(flat.NodeId(i), node)
				}
			}
			else {}
		}

		_ = i
	}
}

fn (mut tc TypeChecker) check_export_attrs() {
	mut natural_symbols := map[string]string{}
	synthetic_main_reserved := tc.has_synthetic_c_entry_main()
	mut cur_module := ''
	for node in tc.a.nodes {
		match node.kind {
			.file {
				tc.enter_file(node.value)
				cur_module = tc.cur_module
			}
			.module_decl {
				cur_module = node.value
				tc.enter_module(node.value)
			}
			.fn_decl {
				qname := export_qualified_fn_name(cur_module, node.value)
				natural_symbol := export_natural_c_symbol(cur_module, node.value)
				natural_symbols[natural_symbol] = qname
			}
			else {}
		}
	}
	mut export_symbols := map[string]string{}
	cur_module = ''
	for i, node in tc.a.nodes {
		match node.kind {
			.file {
				tc.enter_file(node.value)
				cur_module = tc.cur_module
			}
			.module_decl {
				cur_module = node.value
				tc.enter_module(node.value)
			}
			.fn_decl {
				qname := export_qualified_fn_name(cur_module, node.value)
				export_name := tc.a.export_fn_names[qname] or { continue }
				if export_name.len == 0 {
					tc.record_error_unfiltered(.unsupported_generic,
						'empty export name for `${qname}`', flat.NodeId(i))
					continue
				}
				if !is_valid_export_c_name(export_name) {
					tc.record_error_unfiltered(.unsupported_generic,
						'invalid export name `${export_name}` for `${qname}`', flat.NodeId(i))
				}
				if synthetic_main_reserved && export_name == 'main' {
					tc.record_error_unfiltered(.unsupported_generic,
						'export name `main` for `${qname}` collides with synthetic entry point `main`',
						flat.NodeId(i))
				}
				if node.generic_params.len > 0 {
					tc.record_error_unfiltered(.unsupported_generic,
						'generic function `${qname}` cannot be exported', flat.NodeId(i))
				}
				for pi in 0 .. node.children_count {
					p := tc.a.child_node(&node, pi)
					if p.kind == .param && (p.value.len == 0 || p.typ.len == 0) {
						tc.record_error_unfiltered(.unsupported_generic,
							'exported function `${qname}` must name all parameters', flat.NodeId(i))
					}
				}
				if existing := export_symbols[export_name] {
					if existing != qname {
						tc.record_error_unfiltered(.unsupported_generic,
							'duplicate export name `${export_name}` for `${qname}` and `${existing}`',
							flat.NodeId(i))
					}
				} else {
					export_symbols[export_name] = qname
				}
				if existing := natural_symbols[export_name] {
					tc.record_error_unfiltered(.unsupported_generic,
						'export name `${export_name}` for `${qname}` collides with `${existing}`',
						flat.NodeId(i))
				}
			}
			else {}
		}
	}
}

fn (tc &TypeChecker) has_synthetic_c_entry_main() bool {
	if tc.has_c_test_harness_main() {
		return true
	}
	if tc.has_main_module_fn_main() {
		return false
	}
	return tc.has_c_top_level_main()
}

fn (tc &TypeChecker) has_main_module_fn_main() bool {
	mut cur_module := ''
	for node in tc.a.nodes {
		match node.kind {
			.file {
				cur_module = ''
			}
			.module_decl {
				cur_module = node.value
			}
			.fn_decl {
				if node.value == 'main' && (cur_module.len == 0 || cur_module == 'main') {
					return true
				}
			}
			else {}
		}
	}
	return false
}

fn (tc &TypeChecker) has_c_test_harness_main() bool {
	for file_idx, file_node in tc.a.nodes {
		if file_idx < tc.a.user_code_start || file_node.kind != .file || file_node.value.len == 0 {
			continue
		}
		if !tc.is_selected_input_file(file_node.value) {
			continue
		}
		module_name := tc.top_level_file_module_name(file_node)
		if is_c_backend_test_file(file_node.value)
			&& (module_name.len == 0 || module_name == 'main') {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) has_c_top_level_main() bool {
	for file_idx, file_node in tc.a.nodes {
		if !tc.should_emit_c_top_level_file(file_idx, file_node) {
			continue
		}
		for i in 0 .. file_node.children_count {
			child_id := tc.a.child(&file_node, i)
			if int(child_id) < tc.a.user_code_start {
				continue
			}
			if tc.is_c_top_level_stmt(child_id) {
				return true
			}
		}
	}
	return false
}

fn (tc &TypeChecker) should_emit_c_top_level_file(file_idx int, file_node flat.Node) bool {
	if file_idx < tc.a.user_code_start || file_node.kind != .file || file_node.children_count == 0 {
		return false
	}
	module_name := tc.top_level_file_module_name(file_node)
	return module_name.len == 0 || module_name == 'main'
}

fn (tc &TypeChecker) top_level_file_module_name(file_node flat.Node) string {
	if module_name := tc.file_modules[file_node.value] {
		return module_name
	}
	for i in 0 .. file_node.children_count {
		child := tc.a.child_node(&file_node, i)
		if child.kind == .module_decl {
			return child.value
		}
	}
	return ''
}

fn (tc &TypeChecker) is_c_top_level_stmt(id flat.NodeId) bool {
	if int(id) < 0 {
		return false
	}
	node := tc.a.nodes[int(id)]
	return match node.kind {
		.expr_stmt, .assign, .decl_assign, .selector_assign, .index_assign, .for_stmt,
		.for_in_stmt, .if_expr, .match_stmt, .assert_stmt, .defer_stmt {
			true
		}
		.block, .comptime_if {
			for i in 0 .. node.children_count {
				if tc.is_c_top_level_stmt(tc.a.child(&node, i)) {
					return true
				}
			}
			false
		}
		else {
			false
		}
	}
}

fn (tc &TypeChecker) is_selected_input_file(file string) bool {
	return tc.diagnostic_files.len == 0 || tc.diagnostic_files[file]
}

fn is_c_backend_test_file(path string) bool {
	file := path.all_after_last('/').all_after_last('\\')
	if file.ends_with('_test.v') || file.ends_with('_test.c.v') {
		return true
	}
	if !file.ends_with('.v') {
		return false
	}
	base := file[..file.len - 2]
	if !base.contains('.') {
		return false
	}
	return base.all_after_last('.') == 'c' && base.all_before_last('.').ends_with('_test')
}

fn export_qualified_fn_name(module_name string, name string) string {
	if module_name.len == 0 || module_name == 'main' || module_name == 'builtin' {
		return name
	}
	return '${module_name}.${name}'
}

fn export_natural_c_symbol(module_name string, name string) string {
	if module_name == 'builtin' && name == 'free' {
		return 'v_free'
	}
	if module_name.len > 0 && module_name != 'main' && module_name != 'builtin' {
		return c_name('${module_name}.${name}')
	}
	if name == 'free' {
		return 'v_free'
	}
	if name == 'exit' {
		return 'v_exit'
	}
	if name in export_c_libc_collision_symbols {
		return 'v_${name}'
	}
	return c_name(name)
}

fn is_valid_export_c_name(name string) bool {
	if name.len == 0 {
		return false
	}
	if name in export_c_reserved_words {
		return false
	}
	if name in export_v3_reserved_c_symbols {
		return false
	}
	first := name[0]
	if !((first >= `a` && first <= `z`) || (first >= `A` && first <= `Z`) || first == `_`) {
		return false
	}
	for i in 1 .. name.len {
		c := name[i]
		if (c >= `a` && c <= `z`) || (c >= `A` && c <= `Z`) || (c >= `0` && c <= `9`) || c == `_` {
			continue
		}
		return false
	}
	return true
}

fn (mut tc TypeChecker) insert_implicit_veb_ctx(node flat.Node) {
	if !tc.fn_needs_implicit_veb_ctx(node) {
		return
	}
	tc.cur_scope.insert('ctx', tc.implicit_veb_ctx_type())
}

fn (tc &TypeChecker) fn_param_types_with_implicit_veb_ctx(node flat.Node, params []Type) []Type {
	if !tc.fn_needs_implicit_veb_ctx(node) {
		return params
	}
	insert_idx := tc.fn_implicit_veb_ctx_insert_index(node)
	mut result := []Type{cap: params.len + 1}
	for i, param in params {
		if i == insert_idx {
			result << tc.implicit_veb_ctx_type()
		}
		result << param
	}
	if insert_idx >= params.len {
		result << tc.implicit_veb_ctx_type()
	}
	return result
}

fn (tc &TypeChecker) implicit_veb_ctx_type() Type {
	return tc.parse_type('mut Context')
}

fn (tc &TypeChecker) fn_needs_implicit_veb_ctx(node flat.Node) bool {
	return tc.fn_returns_veb_result(node) && tc.fn_has_receiver_param(node)
		&& !tc.fn_receiver_type_is_context(node) && !tc.fn_has_param(node, 'ctx')
		&& tc.type_name_known_in_current_module('Context')
}

fn (tc &TypeChecker) fn_implicit_veb_ctx_insert_index(node flat.Node) int {
	if tc.fn_has_receiver_param(node) {
		return 1
	}
	return 0
}

fn (tc &TypeChecker) fn_has_receiver_param(node flat.Node) bool {
	if !node.value.contains('.') || node.children_count == 0 {
		return false
	}
	first := tc.a.child_node(&node, 0)
	if first.kind != .param || first.typ.len == 0 {
		return false
	}
	receiver := node.value.all_before_last('.').all_after_last('.')
	param_type := first.typ.trim_left('&').all_after_last('.')
	return receiver == param_type
}

fn (tc &TypeChecker) fn_receiver_type_is_context(node flat.Node) bool {
	if !tc.fn_has_receiver_param(node) {
		return false
	}
	first := tc.a.child_node(&node, 0)
	return first.typ.trim_left('&').all_after_last('.') == 'Context'
}

fn (tc &TypeChecker) fn_has_param(node flat.Node, name string) bool {
	for i in 0 .. node.children_count {
		p := tc.a.child_node(&node, i)
		if p.kind == .param && p.value == name {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) fn_returns_veb_result(node flat.Node) bool {
	if node.typ == 'veb.Result' {
		return true
	}
	ret := tc.parse_type(node.typ)
	return ret.name() == 'veb.Result'
}

// check_fn_body validates check fn body state for types.
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

// check_decl_type_strings validates check decl type strings state for types.
fn (mut tc TypeChecker) check_decl_type_strings(node_id flat.NodeId, node flat.Node) {
	generic_params := tc.infer_decl_generic_params(node)
	if node.kind == .struct_decl {
		if node.typ.contains('generic') && tc.reject_unsupported_generics {
			tc.record_unsupported_generic('unsupported generic struct `${node.value}`', node_id)
		}
	} else {
		if node.generic_params.len > 0 && tc.reject_unsupported_generics {
			tc.record_unsupported_generic('unsupported generic declaration `${node.value}`',
				node_id)
		}
		tc.check_type_string_for_unsupported_generics(node.typ, node_id, generic_params)
	}
	for i in 0 .. node.children_count {
		child_id := tc.a.child(&node, i)
		if int(child_id) < 0 {
			continue
		}
		child := tc.a.nodes[int(child_id)]
		tc.check_type_string_for_unsupported_generics(child.typ, child_id, generic_params)
		if node.kind == .type_decl && child.value.len > 0 {
			tc.check_type_string_for_unsupported_generics(child.value, child_id, generic_params)
		}
		for j in 0 .. child.children_count {
			grandchild_id := tc.a.child(&child, j)
			if int(grandchild_id) < 0 {
				continue
			}
			grandchild := tc.a.nodes[int(grandchild_id)]
			tc.check_type_string_for_unsupported_generics(grandchild.typ, grandchild_id,
				generic_params)
		}
	}
}

fn (tc &TypeChecker) infer_decl_generic_params(node flat.Node) map[string]bool {
	mut params := map[string]bool{}
	for name in node.generic_params {
		params[name] = true
	}
	tc.collect_generic_receiver_params(node, mut params)
	return params
}

fn (tc &TypeChecker) collect_generic_receiver_params(node flat.Node, mut params map[string]bool) {
	if node.kind != .fn_decl && node.kind != .c_fn_decl {
		return
	}
	if !node.value.contains('.') {
		return
	}
	if node.children_count == 0 {
		return
	}
	receiver_id := tc.a.child(&node, 0)
	if int(receiver_id) < 0 {
		return
	}
	receiver := tc.a.nodes[int(receiver_id)]
	if receiver.kind != .param {
		return
	}
	receiver_type := receiver.typ.trim_left('&')
	if receiver_type != node.value.all_before_last('.') {
		return
	}
	mut counts := map[string]int{}
	if !tc.collect_generic_param_candidates(receiver.typ, mut counts) {
		return
	}
	for name, _ in counts {
		params[name] = true
	}
}

fn (tc &TypeChecker) collect_generic_param_candidates(typ string, mut counts map[string]int) bool {
	clean := typ.trim_space()
	if clean.len == 0 {
		return false
	}
	if clean.starts_with('&') || clean.starts_with('?') || clean.starts_with('!') {
		return tc.collect_generic_param_candidates(clean[1..], mut counts)
	}
	if clean.starts_with('shared ') {
		return tc.collect_generic_param_candidates(clean[7..], mut counts)
	}
	if clean.starts_with('...') {
		return tc.collect_generic_param_candidates(clean[3..], mut counts)
	}
	if clean.starts_with('[]') {
		return tc.collect_generic_param_candidates(clean[2..], mut counts)
	}
	if clean.starts_with('map[') {
		mut found_context := false
		bracket_end := find_matching_bracket(clean, 3)
		if bracket_end < clean.len {
			if tc.collect_generic_param_candidates(clean[4..bracket_end], mut counts) {
				found_context = true
			}
			if tc.collect_generic_param_candidates(clean[bracket_end + 1..], mut counts) {
				found_context = true
			}
		}
		return found_context
	}
	if clean.starts_with('[') {
		idx := clean.index_u8(`]`)
		if idx > 0 {
			return tc.collect_generic_param_candidates(clean[idx + 1..], mut counts)
		}
		return false
	}
	if clean.starts_with('(') && clean.ends_with(')') {
		mut found_context := false
		for part in split_params(clean[1..clean.len - 1]) {
			if tc.collect_generic_param_candidates(part, mut counts) {
				found_context = true
			}
		}
		return found_context
	}
	if clean.starts_with('fn(') || clean.starts_with('fn (') {
		mut found_context := false
		params_start := clean.index_u8(`(`) + 1
		mut depth := 1
		mut params_end := params_start
		for params_end < clean.len {
			if clean[params_end] == `(` {
				depth++
			} else if clean[params_end] == `)` {
				depth--
				if depth == 0 {
					break
				}
			}
			params_end++
		}
		if params_end < clean.len {
			for part in split_params(clean[params_start..params_end]) {
				trimmed := part.trim_space()
				parts := trimmed.split(' ')
				param_type := if parts.len >= 2 { parts[parts.len - 1] } else { trimmed }
				if tc.collect_generic_param_candidates(param_type, mut counts) {
					found_context = true
				}
			}
			if tc.collect_generic_param_candidates(clean[params_end + 1..], mut counts) {
				found_context = true
			}
		}
		return found_context
	}
	if generic_type_application(clean) {
		bracket := clean.index_u8(`[`)
		bracket_end := find_matching_bracket(clean, bracket)
		if bracket_end < clean.len {
			for part in split_params(clean[bracket + 1..bracket_end]) {
				tc.collect_generic_param_candidates(part, mut counts)
			}
		}
		return true
	}
	if is_bare_generic_param(clean) && !tc.type_name_known(clean) {
		counts[clean] = (counts[clean] or { 0 }) + 1
	}
	return false
}

// check_type_string_for_unsupported_generics
// validates helper state for types.
fn (mut tc TypeChecker) check_type_string_for_unsupported_generics(typ string, node_id flat.NodeId, generic_params map[string]bool) {
	clean := typ.trim_space()
	if clean.len == 0 {
		return
	}
	if clean in ['generic', 'params', 'union'] {
		return
	}
	if clean.starts_with('&') || clean.starts_with('?') || clean.starts_with('!') {
		tc.check_type_string_for_unsupported_generics(clean[1..], node_id, generic_params)
		return
	}
	if clean.starts_with('shared ') {
		tc.check_type_string_for_unsupported_generics(clean[7..], node_id, generic_params)
		return
	}
	if clean == 'thread' || clean == 'chan' {
		return
	}
	if clean.starts_with('thread ') {
		// `thread T` is a thread handle; only its element type T needs checking.
		tc.check_type_string_for_unsupported_generics(clean[7..], node_id, generic_params)
		return
	}
	if clean.starts_with('chan ') {
		tc.check_type_string_for_unsupported_generics(clean[5..], node_id, generic_params)
		return
	}
	if clean.starts_with('...') {
		tc.check_type_string_for_unsupported_generics(clean[3..], node_id, generic_params)
		return
	}
	if clean.starts_with('[]') {
		tc.check_type_string_for_unsupported_generics(clean[2..], node_id, generic_params)
		return
	}
	if clean.starts_with('map[') {
		bracket_end := find_matching_bracket(clean, 3)
		if bracket_end < clean.len {
			tc.check_type_string_for_unsupported_generics(clean[4..bracket_end], node_id,
				generic_params)
			tc.check_type_string_for_unsupported_generics(clean[bracket_end + 1..], node_id,
				generic_params)
		}
		return
	}
	if clean.starts_with('[') {
		idx := clean.index_u8(`]`)
		if idx > 0 {
			tc.check_type_string_for_unsupported_generics(clean[idx + 1..], node_id, generic_params)
		}
		return
	}
	if clean.starts_with('(') && clean.ends_with(')') {
		for part in split_params(clean[1..clean.len - 1]) {
			tc.check_type_string_for_unsupported_generics(part, node_id, generic_params)
		}
		return
	}
	if clean.starts_with('fn(') || clean.starts_with('fn (') {
		tc.check_fn_type_string_for_unsupported_generics(clean, node_id, generic_params)
		return
	}
	if generic_type_application(clean) {
		if tc.reject_unsupported_generics {
			tc.record_unsupported_generic('unsupported generic type application `${clean}`',
				node_id)
			return
		}
		bracket := clean.index_u8(`[`)
		bracket_end := find_matching_bracket(clean, bracket)
		base := clean[..bracket].trim_space()
		if should_check_named_type(base) && !tc.type_name_known(base) {
			tc.record_error(.unknown_type, 'unknown type `${base}`', node_id)
		}
		if bracket_end < clean.len {
			for part in split_params(clean[bracket + 1..bracket_end]) {
				tc.check_type_string_for_unsupported_generics(part, node_id, generic_params)
			}
		}
		return
	}
	if is_bare_generic_param(clean) && !tc.type_name_known(clean) {
		if tc.reject_unsupported_generics {
			tc.record_unsupported_generic('unsupported generic type parameter `${clean}`', node_id)
			return
		}
		if clean in generic_params {
			return
		}
	}
	if should_check_named_type(clean) && !tc.type_name_known(clean) {
		tc.record_error(.unknown_type, 'unknown type `${clean}`', node_id)
	}
}

// check_fn_type_string_for_unsupported_generics
// validates helper state for types.
fn (mut tc TypeChecker) check_fn_type_string_for_unsupported_generics(typ string, node_id flat.NodeId, generic_params map[string]bool) {
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
		tc.check_type_string_for_unsupported_generics(param_type, node_id, generic_params)
	}
	ret := typ[params_end + 1..].trim_space()
	tc.check_type_string_for_unsupported_generics(ret, node_id, generic_params)
}

// generic_type_application supports generic type application handling for types.
fn generic_type_application(typ string) bool {
	_, _, ok := generic_type_application_parts(typ)
	return ok
}

fn (tc &TypeChecker) generic_args_are_concrete(args []string) bool {
	for arg in args {
		if tc.type_text_has_generic_placeholder(arg) {
			return false
		}
	}
	return true
}

fn (tc &TypeChecker) type_text_has_generic_placeholder(typ string) bool {
	clean := typ.trim_space()
	if is_bare_generic_param(clean) {
		return !tc.is_known_type_text(clean)
	}
	if clean.starts_with('&') {
		return tc.type_text_has_generic_placeholder(clean[1..])
	}
	if clean.starts_with('mut ') {
		return tc.type_text_has_generic_placeholder(clean[4..])
	}
	if clean.starts_with('?') || clean.starts_with('!') {
		return tc.type_text_has_generic_placeholder(clean[1..])
	}
	if clean.starts_with('...') {
		return tc.type_text_has_generic_placeholder(clean[3..])
	}
	if clean.starts_with('[]') {
		return tc.type_text_has_generic_placeholder(clean[2..])
	}
	if clean.starts_with('map[') {
		bracket_end := find_matching_bracket(clean, 3)
		if bracket_end < clean.len {
			return tc.type_text_has_generic_placeholder(clean[4..bracket_end])
				|| tc.type_text_has_generic_placeholder(clean[bracket_end + 1..])
		}
	}
	if clean.starts_with('[') {
		bracket_end := find_matching_bracket(clean, 0)
		if bracket_end < clean.len {
			return tc.type_text_has_generic_placeholder(clean[bracket_end + 1..])
		}
	}
	_, args, ok := generic_type_application_parts(clean)
	if ok {
		for arg in args {
			if tc.type_text_has_generic_placeholder(arg) {
				return true
			}
		}
	}
	return false
}

fn generic_type_application_parts(typ string) (string, []string, bool) {
	if typ.starts_with('[') || !typ.contains('[') {
		return '', []string{}, false
	}
	bracket := typ.index_u8(`[`)
	bracket_end := find_matching_bracket(typ, bracket)
	if bracket <= 0 || bracket_end <= bracket {
		return '', []string{}, false
	}
	inner := typ[bracket + 1..bracket_end].trim_space()
	if is_fixed_array_len_text(inner) {
		return '', []string{}, false
	}
	return typ[..bracket], split_params(inner), true
}

// is_fixed_array_len_text reports whether a postfix `Base[inner]` bracket holds a fixed-array
// length rather than a generic type argument. `ArrayFixed.name()` renders the length as a decimal
// (`u8[16]`), a non-decimal literal (`u8[0x10]`), or the source length expression (`u8[segs + 1]`);
// a generic argument is always a type, never a number or arithmetic expression. Recognising all
// three keeps such a postfix name parsing as a fixed array (e.g. when `[]thread T.wait()` recovers
// the spawned return type) instead of a bogus generic application.
fn is_fixed_array_len_text(inner string) bool {
	s := inner.trim_space()
	if s.len == 0 {
		return false
	}
	// A fixed-array length is a single integer expression; a comma means the brackets hold a
	// generic argument LIST (`Pair[int, &Node]`), not a length. Without this an `&` (or `-`)
	// that merely leads a later pointer type argument would be read as a length operator.
	if s.contains(',') {
		return false
	}
	if v_int_literal_value(s) != none {
		return true
	}
	for i in 0 .. s.len {
		c := s[i]
		if c in [`+`, `*`, `/`, `%`, `|`, `^`, `<`, `>`] {
			return true
		}
		// A leading `-`/`&` is a negative literal / pointer-type argument; elsewhere they are the
		// subtraction / bitwise-and operators of a length expression.
		if (c == `-` || c == `&`) && i > 0 {
			return true
		}
	}
	return false
}

// is_decimal_int_literal reports whether is decimal int literal applies in types.
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

// v_int_literal_value parses a complete V integer literal — decimal, hex (`0x`), octal
// (`0o`), or binary (`0b`), with optional `_` digit separators — to its value. Returns
// none when `s` is not a whole integer literal (a const name, an expression, etc.), so
// const-length folding accepts `0xF & 6` / `[0b1100 >> 1]int`, not just decimal text.
fn v_int_literal_value(s string) ?int {
	if s.len == 0 {
		return none
	}
	t := s.replace('_', '')
	if t.len == 0 {
		return none
	}
	mut base := 10
	mut digits := t
	if t.len >= 2 && t[0] == `0` {
		c := t[1]
		if c == `x` || c == `X` {
			base = 16
			digits = t[2..]
		} else if c == `o` || c == `O` {
			base = 8
			digits = t[2..]
		} else if c == `b` || c == `B` {
			base = 2
			digits = t[2..]
		}
	}
	if digits.len == 0 {
		return none
	}
	mut value := 0
	for ch in digits {
		mut d := 0
		if ch >= `0` && ch <= `9` {
			d = int(ch - `0`)
		} else if ch >= `a` && ch <= `f` {
			d = int(ch - `a`) + 10
		} else if ch >= `A` && ch <= `F` {
			d = int(ch - `A`) + 10
		} else {
			return none
		}
		if d >= base {
			return none
		}
		value = value * base + d
	}
	return value
}

// is_bare_generic_param reports whether is bare generic param applies in types.
fn is_bare_generic_param(typ string) bool {
	return typ.len == 1 && typ[0] >= `A` && typ[0] <= `Z`
}

fn generic_param_index(name string) int {
	return match name {
		'T', 'A', 'K', 'X' { 0 }
		'U', 'B', 'V', 'Y' { 1 }
		'C', 'W', 'Z' { 2 }
		else { 0 }
	}
}

fn generic_placeholder_from_unknown(typ Unknown) ?string {
	start := typ.reason.index_u8(`\``)
	if start < 0 {
		return none
	}
	end := typ.reason[start + 1..].index_u8(`\``)
	if end < 0 {
		return none
	}
	name := typ.reason[start + 1..start + 1 + end]
	if !is_bare_generic_param(name) {
		return none
	}
	return name
}

fn (tc &TypeChecker) resolve_known_field_type(type_name string, fallback Type) Type {
	qname := tc.qualify_name(type_name)
	if qname in tc.structs {
		return Type(Struct{
			name: qname
		})
	}
	if type_name in tc.structs {
		return Type(Struct{
			name: type_name
		})
	}
	if qname in tc.interface_names {
		return Type(Interface{
			name: qname
		})
	}
	if type_name in tc.interface_names {
		return Type(Interface{
			name: type_name
		})
	}
	if qname in tc.type_aliases {
		return Type(Alias{
			name:      qname
			base_type: tc.parse_type(tc.type_aliases[qname])
		})
	}
	if type_name in tc.type_aliases {
		return Type(Alias{
			name:      type_name
			base_type: tc.parse_type(tc.type_aliases[type_name])
		})
	}
	return fallback
}

// type_name_known returns type name known data for TypeChecker.
fn (tc &TypeChecker) type_name_known(typ string) bool {
	if is_builtin_type_name(typ) || typ == 'unknown' || typ.starts_with('C.') {
		return true
	}
	qtyp := tc.qualify_name(typ)
	if !typ.contains('.') {
		if resolved := tc.resolve_selective_import_type_symbol(typ) {
			return tc.type_symbol_known(resolved)
		}
	}
	return typ in tc.type_aliases || qtyp in tc.type_aliases || typ in tc.structs
		|| qtyp in tc.structs || typ in tc.interface_names || qtyp in tc.interface_names
		|| typ in tc.enum_names || qtyp in tc.enum_names || typ in tc.sum_types
		|| qtyp in tc.sum_types
}

fn (tc &TypeChecker) type_name_known_in_current_module(typ string) bool {
	qtyp := tc.qualify_name(typ)
	return qtyp in tc.type_aliases || qtyp in tc.structs || qtyp in tc.interface_names
		|| qtyp in tc.enum_names || qtyp in tc.sum_types
}

// should_check_named_type reports whether should check named type applies in types.
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

// check_struct_field_defaults validates check struct field defaults state for types.
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

// check_enum_field_values validates check enum field values state for types.
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

// check_const_field_values validates check const field values state for types.
fn (mut tc TypeChecker) check_const_field_values(node flat.Node) {
	for i in 0 .. node.children_count {
		field := tc.a.child_node(&node, i)
		if field.kind != .const_field || field.children_count == 0 {
			continue
		}
		tc.check_node(tc.a.child(field, 0))
	}
}

// fn_body_definitely_returns supports fn body definitely returns handling for TypeChecker.
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

fn type_allows_implicit_return(typ Type) bool {
	if typ is Void {
		return true
	}
	if typ is OptionType {
		return typ.base_type is Void
	}
	if typ is ResultType {
		return typ.base_type is Void
	}
	return false
}

// valid_node_id supports valid node id handling for TypeChecker.
fn (tc &TypeChecker) valid_node_id(id flat.NodeId) bool {
	return int(id) >= 0 && tc.a != unsafe { nil } && int(id) < tc.a.nodes.len
}

// stmt_definitely_returns supports stmt definitely returns handling for TypeChecker.
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
			return has_else || tc.match_without_else_exhaustive_enum_returns(node)
		}
		else {
			return false
		}
	}
}

// match_covers_all_enum_variants reports whether a `match` over an enum subject
// lists every variant of that enum (so it is exhaustive without an `else`).
fn (tc &TypeChecker) match_covers_all_enum_variants(node flat.Node) bool {
	if node.children_count < 2 {
		return false
	}
	subject_type := unwrap_pointer(tc.resolve_type(tc.a.child(&node, 0)))
	mut enum_name := ''
	if subject_type is Enum {
		enum_name = subject_type.name
	} else {
		return false
	}
	// A `[flag]` enum value can hold combined or zero bits (`.read | .write`, `0`) that
	// no single-field branch covers, so listing every field is NOT exhaustive — such a
	// match needs an explicit `else`.
	if enum_name in tc.flag_enums {
		return false
	}
	all_fields := tc.enum_fields[enum_name] or { return false }
	if all_fields.len == 0 {
		return false
	}
	mut covered := map[string]bool{}
	for i in 1 .. node.children_count {
		branch := tc.a.child_node(&node, i)
		if branch.kind != .match_branch {
			return false
		}
		if branch.value == 'else' {
			return true
		}
		n_conds := branch.value.int()
		for j in 0 .. n_conds {
			cond := tc.a.child_node(branch, j)
			if cond.kind == .enum_val {
				covered[cond.value.all_after_last('.')] = true
			}
		}
	}
	for f in all_fields {
		if f !in covered {
			return false
		}
	}
	return true
}

// match_branch_definitely_returns
// supports helper handling in types.
fn (tc &TypeChecker) match_branch_definitely_returns(branch &flat.Node) bool {
	body_start := if branch.value == 'else' { 0 } else { branch.value.int() }
	for i in body_start .. branch.children_count {
		if tc.stmt_definitely_returns(tc.a.child(branch, i)) {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) match_without_else_exhaustive_enum_returns(node flat.Node) bool {
	subject_type := unwrap_pointer(tc.resolve_type(tc.a.child(&node, 0)))
	if subject_type is Enum {
		enum_name := tc.resolve_enum_name(subject_type.name) or { subject_type.name }
		if subject_type.is_flag || enum_name in tc.flag_enums {
			return false
		}
		fields := tc.enum_fields[enum_name] or { return false }
		if fields.len == 0 {
			return false
		}
		mut covered := map[string]bool{}
		for i in 1 .. node.children_count {
			branch := tc.a.child_node(&node, i)
			if branch.kind != .match_branch || branch.value == 'else' {
				return false
			}
			n_conds := branch.value.int()
			if n_conds <= 0 || n_conds > branch.children_count {
				return false
			}
			for j in 0 .. n_conds {
				cond := tc.a.child_node(branch, j)
				field := tc.match_enum_condition_field(cond, enum_name) or { return false }
				covered[field] = true
			}
		}
		for field in fields {
			if field !in covered {
				return false
			}
		}
		return true
	}
	return false
}

fn (tc &TypeChecker) match_enum_condition_field(cond &flat.Node, enum_name string) ?string {
	match cond.kind {
		.enum_val {
			field := cond.value.all_after_last('.')
			if tc.enum_value_matches(cond.value, enum_name) {
				return field
			}
		}
		.selector {
			if typ := tc.enum_selector_type(cond) {
				if typ is Enum {
					cond_enum_name := tc.resolve_enum_name(typ.name) or { typ.name }
					if cond_enum_name == enum_name && tc.enum_has_field(enum_name, cond.value) {
						return cond.value
					}
				}
			}
		}
		else {}
	}

	return none
}

// node_kind_id supports node kind id handling for types.
fn node_kind_id(node flat.Node) int {
	mut kind_id := node.kind_id
	if kind_id == 0 && int(node.kind) != 0 {
		kind_id = int(node.kind)
	}
	return kind_id
}

// check_node validates check node state for types.
fn (mut tc TypeChecker) check_node(id flat.NodeId) {
	idx := int(id)
	if idx < 0 {
		return
	}
	if idx >= tc.checking_nodes.len {
		tc.extend_node_caches(tc.a.nodes.len)
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
	if node.kind == .comptime_if {
		tc.check_comptime_if(id, node)
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
	if node.kind == .array_init {
		tc.check_array_init(node)
		return
	}
	if node.kind == .select_stmt {
		tc.check_select_stmt(node)
		return
	}
	// A method value stored in a container escapes the single-use guarantee of its per-site
	// static receiver, so reject `[obj.method]` / `arr << obj.method` / `{'k': obj.method}`.
	if node.kind == .array_literal {
		for i in 0 .. node.children_count {
			tc.reject_stored_method_value(tc.a.child(&node, i))
			tc.reject_stored_capturing_fn_literal(tc.a.child(&node, i))
		}
	} else if node.kind == .map_init {
		// children alternate key, value, key, value, ...; check the value positions.
		for j := 1; j < node.children_count; j += 2 {
			tc.reject_stored_method_value(tc.a.child(&node, j))
			tc.reject_stored_capturing_fn_literal(tc.a.child(&node, j))
		}
	} else if node.kind == .infix && node.op == .left_shift && node.children_count >= 2 {
		if unwrap_pointer(tc.resolve_type(tc.a.child(&node, 0))) is Array {
			tc.reject_stored_method_value(tc.a.child(&node, 1))
			tc.reject_stored_capturing_fn_literal(tc.a.child(&node, 1))
		}
	}

	for i in 0 .. node.children_count {
		tc.check_node(tc.a.child(&node, i))
	}
}

fn (mut tc TypeChecker) check_comptime_if(_id flat.NodeId, node flat.Node) {
	take_then := tc.comptime_type_condition_value(node.value) or { return }
	branch_index := if take_then { 0 } else { 1 }
	if branch_index >= node.children_count {
		return
	}
	tc.check_node(tc.a.child(&node, branch_index))
}

fn (mut tc TypeChecker) comptime_type_condition_value(cond string) ?bool {
	clean := comptime_condition_strip_outer_parens(cond)
	if clean == 'true' {
		return true
	}
	if clean == 'false' {
		return false
	}
	or_idx := comptime_condition_top_level_index(clean, '||')
	if or_idx >= 0 {
		left := tc.comptime_type_condition_value(clean[..or_idx]) or { return none }
		if left {
			return true
		}
		return tc.comptime_type_condition_value(clean[or_idx + 2..])
	}
	and_idx := comptime_condition_top_level_index(clean, '&&')
	if and_idx >= 0 {
		left := tc.comptime_type_condition_value(clean[..and_idx]) or { return none }
		if !left {
			return false
		}
		return tc.comptime_type_condition_value(clean[and_idx + 2..])
	}
	for op in [' !is ', ' is '] {
		op_idx := comptime_condition_top_level_index(clean, op)
		if op_idx >= 0 {
			left := clean[..op_idx].trim_space()
			right := clean[op_idx + op.len..].trim_space()
			matches := tc.comptime_type_matches(left, right) or { return none }
			return if op == ' is ' { matches } else { !matches }
		}
	}
	if clean.starts_with('!') {
		value := tc.comptime_type_condition_value(clean[1..]) or { return none }
		return !value
	}
	return none
}

fn (mut tc TypeChecker) comptime_type_matches(actual string, expected string) ?bool {
	clean_actual := actual.trim_space()
	clean_expected := expected.trim_space()
	if clean_actual.len == 0 || clean_expected.len == 0
		|| (is_bare_generic_param(clean_actual) && !tc.type_name_known(clean_actual)) {
		return none
	}
	actual_type := tc.comptime_type_match_type(clean_actual)
	normalized := actual_type.name()
	match clean_expected {
		'$array' {
			return actual_type is Array || actual_type is ArrayFixed
		}
		'$map' {
			return actual_type is Map
		}
		'$function' {
			return actual_type is FnType
		}
		'$option' {
			return actual_type is OptionType
		}
		'$int' {
			return actual_type.is_integer()
		}
		'$float' {
			return actual_type.is_float()
		}
		'$struct' {
			return normalized in tc.structs
		}
		'$enum' {
			return normalized in tc.enum_names
		}
		'$alias' {
			return clean_actual in tc.type_aliases
				|| tc.qualify_name(clean_actual) in tc.type_aliases
		}
		'$sumtype' {
			return normalized in tc.sum_types
		}
		'$interface' {
			return normalized in tc.interface_names
		}
		else {}
	}

	expected_type := tc.comptime_type_match_type(clean_expected)
	return normalized == expected_type.name()
}

fn (mut tc TypeChecker) comptime_type_match_type(type_text string) Type {
	typ := tc.parse_type(type_text)
	if typ is Alias {
		return typ.base_type
	}
	return typ
}

fn comptime_condition_matching_paren(s string, start int) int {
	mut paren_depth := 0
	mut bracket_depth := 0
	for i in start .. s.len {
		match s[i] {
			`(` {
				paren_depth++
			}
			`)` {
				paren_depth--
				if paren_depth == 0 && bracket_depth == 0 {
					return i
				}
			}
			`[` {
				bracket_depth++
			}
			`]` {
				bracket_depth--
			}
			else {}
		}
	}
	return s.len
}

fn comptime_condition_strip_outer_parens(cond string) string {
	mut clean := cond.trim_space()
	for clean.len >= 2 && clean.starts_with('(') {
		end := comptime_condition_matching_paren(clean, 0)
		if end != clean.len - 1 {
			break
		}
		clean = clean[1..clean.len - 1].trim_space()
	}
	return clean
}

fn comptime_condition_top_level_index(s string, needle string) int {
	if needle.len == 0 || s.len < needle.len {
		return -1
	}
	mut paren_depth := 0
	mut bracket_depth := 0
	for i := 0; i <= s.len - needle.len; i++ {
		match s[i] {
			`(` {
				paren_depth++
			}
			`)` {
				if paren_depth > 0 {
					paren_depth--
				}
			}
			`[` {
				bracket_depth++
			}
			`]` {
				if bracket_depth > 0 {
					bracket_depth--
				}
			}
			else {}
		}

		if paren_depth == 0 && bracket_depth == 0 && s[i..].starts_with(needle) {
			return i
		}
	}
	return -1
}

// check_select_stmt validates a `select { ... }` statement. A receive branch
// `val := <-ch` binds `val` (the channel's element type) in the branch body's
// scope; other branches (sends, bare conditions, `else`) are checked as-is.
fn (mut tc TypeChecker) check_select_stmt(node flat.Node) {
	for i in 0 .. node.children_count {
		branch_id := tc.a.child(&node, i)
		if !tc.valid_node_id(branch_id) {
			continue
		}
		branch := tc.a.nodes[int(branch_id)]
		if branch.kind != .select_branch {
			tc.check_node(branch_id)
			continue
		}
		tc.push_scope()
		mut body_start := 0
		if branch.value == 'recv' && branch.children_count >= 2 {
			// children[0] = bound var ident, children[1] = `<-ch` receive expr.
			var_id := tc.a.child(&branch, 0)
			recv_id := tc.a.child(&branch, 1)
			tc.check_node(recv_id)
			elem_type := tc.resolve_type(recv_id)
			if tc.valid_node_id(var_id) {
				var_node := tc.a.nodes[int(var_id)]
				if var_node.kind == .ident && var_node.value.len > 0 {
					tc.cur_scope.insert(var_node.value, elem_type)
				}
			}
			body_start = 2
		}
		for j in body_start .. branch.children_count {
			tc.check_node(tc.a.child(&branch, j))
		}
		tc.pop_scope()
	}
}

// check_array_init validates an `[]T{len: ..., init: ...}` initializer. The `init:`
// expression may reference the magic `index` variable (the current element index),
// so it is checked in a scope where `index` is bound to an int.
fn (mut tc TypeChecker) check_array_init(node flat.Node) {
	for i in 0 .. node.children_count {
		child_id := tc.a.child(&node, i)
		child := tc.a.nodes[int(child_id)]
		if child.kind == .field_init && child.value == 'init' {
			if child.children_count > 0 {
				tc.reject_stored_method_value(tc.a.child(&child, 0))
				tc.reject_stored_capturing_fn_literal(tc.a.child(&child, 0))
			}
			tc.push_scope()
			tc.cur_scope.insert('index', Type(int_))
			tc.check_node(child_id)
			tc.pop_scope()
		} else {
			tc.check_node(child_id)
		}
	}
}

// check_or_expr validates check or expr state for types.
fn (mut tc TypeChecker) check_or_expr(node flat.Node) {
	if node.children_count == 0 {
		return
	}
	inner_id := tc.a.child(&node, 0)
	tc.check_node(inner_id)
	if node.children_count < 2 || node.value in ['!', '?'] {
		return
	}
	tc.push_scope()
	tc.cur_scope.insert('err', tc.parse_type('IError'))
	tc.check_node(tc.a.child(&node, 1))
	tc.pop_scope()
}

// check_fn_literal validates check fn literal state for types.
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

// check_lambda_expr validates check lambda expr state for types.
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

// check_block validates check block state for types.
fn (mut tc TypeChecker) check_block(node flat.Node) {
	tc.push_scope()
	for i in 0 .. node.children_count {
		tc.check_node(tc.a.child(&node, i))
	}
	tc.pop_scope()
}

// check_for_stmt validates check for stmt state for types.
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

// check_for_in_stmt validates check for in stmt state for types.
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
		} else if elem_type := iterator_for_in_elem_type(clean) {
			if has_val {
				tc.insert_loop_var(key_id, Type(int_))
				tc.insert_loop_var(val_id, elem_type)
			} else {
				tc.insert_loop_var(key_id, elem_type)
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

// check_decl_assign validates check decl assign state for types.
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
		mut rhs_type := tc.decl_assign_inferred_type(rhs_id)
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
		tc.track_method_value_local(lhs_id, rhs_id)
		tc.reject_stored_capturing_fn_literal(rhs_id)
		i += 2
	}
}

// cur_scope_depth returns the number of enclosing scopes (the current scope's parent-chain
// length), used to tell a dominating top-level reassignment from one nested in a branch/loop.
fn (tc &TypeChecker) cur_scope_depth() int {
	mut d := 0
	mut s := tc.cur_scope
	for s != unsafe { nil } {
		d++
		s = s.parent
	}
	return d
}

// track_method_value_local records (or clears) a local variable bound to a method value, so a
// later `return cb` / `arr << cb` aliasing the same per-site static receiver is rejected as an
// escape just like the bare `return c.report`.
fn (mut tc TypeChecker) track_method_value_local(lhs_id flat.NodeId, rhs_id flat.NodeId) {
	if int(lhs_id) < 0 {
		return
	}
	lhs := tc.a.nodes[int(lhs_id)]
	if lhs.kind != .ident || lhs.value.len == 0 || lhs.value == '_' {
		return
	}
	if tc.expr_is_method_value(rhs_id) {
		tc.method_value_locals[lhs.value] = true
		tc.method_value_local_depth[lhs.value] = tc.cur_scope_depth()
	} else if lhs.value in tc.method_value_locals {
		// Reassigned to a non-method-value. Only clear the marker when this reassignment
		// dominates later uses — at the same or a shallower scope than where the local was
		// marked. A reassignment in a deeper conditional/loop scope does not run on every path
		// (`mut cb := c.report; if x { cb = plain }; return cb`), so the local may still hold the
		// method value; keep the maybe-method marker and let the later escape be rejected.
		marked_depth := tc.method_value_local_depth[lhs.value] or { 0 }
		if tc.cur_scope_depth() <= marked_depth {
			tc.method_value_locals.delete(lhs.value)
			tc.method_value_local_depth.delete(lhs.value)
		}
	}
}

fn (mut tc TypeChecker) decl_assign_inferred_type(rhs_id flat.NodeId) Type {
	if int(rhs_id) < 0 || int(rhs_id) >= tc.a.nodes.len {
		return unknown_type('missing declaration initializer')
	}
	rhs := tc.a.nodes[int(rhs_id)]
	if rhs.kind == .cast_expr && rhs.value.len > 0 {
		typ := tc.parse_type(rhs.value)
		if typ is Alias {
			return typ
		}
	}
	if typ := tc.infer_fn_value_decl_type(rhs_id) {
		return typ
	}
	return tc.resolve_type(rhs_id)
}

fn (mut tc TypeChecker) infer_fn_value_decl_type(rhs_id flat.NodeId) ?Type {
	if int(rhs_id) < 0 || int(rhs_id) >= tc.a.nodes.len {
		return none
	}
	rhs := tc.a.nodes[int(rhs_id)]
	if tc.fn_value_shadowed_by_value(rhs) {
		return none
	}
	key := tc.fn_value_key(rhs) or { return none }
	typ := tc.fn_type_from_key(key) or { return none }
	tc.remember_resolved_fn_value_chain(rhs_id, key)
	tc.register_synth_type(rhs_id, typ)
	return typ
}

fn (tc &TypeChecker) fn_value_shadowed_by_value(node flat.Node) bool {
	match node.kind {
		.ident {
			return tc.name_bound_as_value(node.value)
		}
		.selector {
			return tc.selector_base_bound_as_value(node)
		}
		.cast_expr, .paren, .expr_stmt {
			if node.children_count == 0 {
				return false
			}
			return tc.fn_value_shadowed_by_value(tc.a.child_node(&node, 0))
		}
		else {
			return false
		}
	}
}

// lvalue_is_local_var reports whether an assignment target is safe to receive a method value:
// the blank discard `_` (stores nothing) or a plain function-local variable bound under its bare
// name in the current scope. Non-local storage (a struct field `h.cb`, an array/map element
// `cbs[i]`, or a module-level global, which lives in file_scope under its qualified name and so
// misses a bare lookup) is not. A method value may alias a local (tracked for a later escape) but
// must not be stored into anything that outlives the call site.
fn (tc &TypeChecker) lvalue_is_local_var(lhs_id flat.NodeId) bool {
	if int(lhs_id) < 0 {
		return false
	}
	lhs := tc.a.nodes[int(lhs_id)]
	if lhs.kind != .ident || lhs.value.len == 0 {
		return false
	}
	if lhs.value == '_' {
		return true
	}
	return tc.cur_scope.lookup(lhs.value) != none
}

fn (tc &TypeChecker) selector_base_bound_as_value(node flat.Node) bool {
	if node.children_count == 0 {
		return false
	}
	base := tc.a.child_node(&node, 0)
	match base.kind {
		.ident {
			return tc.name_bound_as_value(base.value)
		}
		.selector {
			return tc.selector_base_bound_as_value(base)
		}
		.cast_expr, .paren, .expr_stmt {
			if base.children_count == 0 {
				return false
			}
			return tc.fn_value_shadowed_by_value(tc.a.child_node(base, 0))
		}
		else {
			return false
		}
	}
}

fn (tc &TypeChecker) name_bound_as_value(name string) bool {
	if name.len == 0 {
		return false
	}
	if typ := tc.cur_scope.lookup(name) {
		return typ !is Void
	}
	if typ := tc.file_scope.lookup(name) {
		return typ !is Void
	}
	return false
}

// check_multi_return_decl_assign validates check multi return decl assign state for types.
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

// multi_assign_lhs_ids supports multi assign lhs ids handling for TypeChecker.
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

// insert_decl_lhs updates insert decl lhs state for types.
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

// check_assign validates check assign state for types.
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
		if node.kind in [.assign, .selector_assign, .index_assign] {
			if tc.expr_is_method_value(rhs_id) && !tc.lvalue_is_local_var(lhs_id) {
				// Storing a method value into a struct field (`h.cb = ..`), an array/map element
				// (`cbs[i] = ..`), or a global lets it outlive the per-site static `_mvctx_N`
				// receiver slot, which the next evaluation of the same site overwrites — so every
				// stored callback would use the last receiver. Reject it like the other escapes.
				tc.reject_stored_method_value(rhs_id)
			} else {
				tc.track_method_value_local(lhs_id, rhs_id)
			}
			tc.reject_stored_capturing_fn_literal(rhs_id)
		}
		i += 2
	}
}

// index_assign_lhs_is_map supports index assign lhs is map handling for TypeChecker.
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

// check_multi_return_assign validates check multi return assign state for types.
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

// check_postfix validates check postfix state for types.
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

// resolve_lvalue_type resolves resolve lvalue type information for types.
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

// check_return validates check return state for types.
fn (mut tc TypeChecker) check_return(id flat.NodeId, node flat.Node) {
	// A returned method value escapes the function, where its per-site static receiver
	// can't keep multiple returned callbacks distinct (a factory `fn bind(c) fn () int {
	// return c.report }`); reject it rather than emitting invalid C.
	for i in 0 .. node.children_count {
		tc.reject_stored_method_value(tc.a.child(&node, i))
		tc.reject_stored_capturing_fn_literal(tc.a.child(&node, i))
	}
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
		if type_allows_implicit_return(expected) {
			return
		}
		if tc.should_diagnose(id) {
			tc.record_error(.return_mismatch, 'missing return value of type `${expected.name()}`',
				id)
		}
		return
	}
	if multi := multi_return_payload_type(expected) {
		if node.children_count == 1 {
			child_id := tc.a.child(&node, 0)
			tc.check_node(child_id)
			actual := tc.resolve_expr(child_id, expected)
			if tc.type_compatible(actual, expected) {
				return
			}
		}
		if node.children_count != multi.types.len {
			if tc.should_diagnose(id) {
				tc.record_error(.return_mismatch,
					'return value count mismatch: expected ${multi.types.len}, got ${node.children_count}',
					id)
			}
			return
		}
		for i in 0 .. node.children_count {
			child_id := tc.a.child(&node, i)
			tc.check_node(child_id)
			actual := tc.resolve_expr(child_id, multi.types[i])
			if !tc.type_compatible(actual, multi.types[i]) {
				tc.type_mismatch(.return_mismatch,
					'cannot return `${actual.name()}` as `${multi.types[i].name()}`', id)
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
	if !tc.return_type_compatible(actual, expected) {
		tc.type_mismatch(.return_mismatch,
			'cannot return `${actual.name()}` as `${expected.name()}`', id)
	}
}

fn (tc &TypeChecker) return_type_compatible(actual Type, expected Type) bool {
	return tc.type_compatible(actual, expected)
}

fn contextual_payload_type(typ Type) ?Type {
	if typ is OptionType {
		if typ.base_type is Void {
			return none
		}
		return typ.base_type
	}
	if typ is ResultType {
		if typ.base_type is Void {
			return none
		}
		return typ.base_type
	}
	return none
}

fn multi_return_payload_type(typ Type) ?MultiReturn {
	if typ is MultiReturn {
		return typ
	}
	if typ is OptionType {
		base := typ.base_type
		if base is MultiReturn {
			return base
		}
	}
	if typ is ResultType {
		base := typ.base_type
		if base is MultiReturn {
			return base
		}
	}
	return none
}

// check_call validates check call state for types.
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
	if tc.is_unsupported_hex_call(node) {
		if tc.should_diagnose(id) {
			tc.record_error(.unknown_fn, 'unknown function `${tc.call_display_name(node)}`', id)
		}
		return
	}
	if tc.call_has_ambiguous_selective_import(node) {
		tc.record_error(.unknown_fn, 'ambiguous selective import `${tc.call_display_name(node)}`',
			id)
		return
	}
	if tc.should_diagnose(id) && !tc.is_known_call(node) {
		tc.record_error(.unknown_fn, 'unknown function `${tc.call_display_name(node)}`', id)
	}
	for i in 1 .. node.children_count {
		tc.check_node(tc.call_arg_value(tc.a.child(&node, i)))
	}
}

// should_diagnose reports whether should diagnose applies in types.
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

fn (tc &TypeChecker) should_diagnose_unsupported_generic(id flat.NodeId) bool {
	if tc.should_diagnose(id) {
		return true
	}
	if int(id) < 0 || int(id) < tc.a.user_code_start {
		return false
	}
	if tc.diagnostic_files.len == 0 {
		return false
	}
	return tc.diagnostic_files['generic:' + tc.cur_file]
}

// should_diagnose_unknown_call reports whether should diagnose unknown call applies in types.
fn (tc &TypeChecker) should_diagnose_unknown_call(id flat.NodeId) bool {
	return tc.diagnose_unknown_calls && tc.should_diagnose(id)
}

// resolve_call_info resolves resolve call info information for types.
fn (mut tc TypeChecker) resolve_call_info(id flat.NodeId, node flat.Node) ?CallInfo {
	if node.children_count == 0 {
		return none
	}
	fn_node := tc.a.child_node(&node, 0)
	if info := tc.resolve_generic_call_info(id, fn_node) {
		return info
	}
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
			if base_node.value == tc.cur_module {
				mod_name := '${tc.cur_module}.${fn_node.value}'
				if mod_name in tc.fn_ret_types {
					return tc.call_info(mod_name, false)
				}
			}
			qbase := tc.qualify_name(base_node.value)
			static_name := '${qbase}.${fn_node.value}'
			if static_name in tc.fn_ret_types && (qbase in tc.structs
				|| qbase in tc.enum_names || qbase in tc.sum_types
				|| qbase in tc.interface_names || qbase in tc.type_aliases) {
				// `qbase in tc.type_aliases` covers static methods on a type alias,
				// e.g. `fn SimdFloat4.new()` for `type SimdFloat4 = vec.Vec4[f32]`.
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
		if info := tc.pointer_builtin_method_call_info(base_type, fn_node.value) {
			return info
		}
		if clean is Channel && fn_node.value == 'close' {
			return CallInfo{
				name:         'chan.close'
				params:       tarr1(base_type)
				return_type:  Type(void_)
				has_receiver: true
				params_known: true
			}
		}
		if clean is Array {
			for mname in exact_array_receiver_method_candidates(clean, fn_node.value, tc.cur_module) {
				if mname in tc.fn_ret_types {
					return tc.call_info(mname, true)
				}
			}
		}
		if clean is Array && fn_node.value in ['clone', 'reverse'] {
			return CallInfo{
				name:         'array.${fn_node.value}'
				params:       tarr1(base_type)
				return_type:  base_type
				has_receiver: true
				params_known: true
			}
		}
		if clean is Map {
			match fn_node.value {
				'clone' {
					return CallInfo{
						name:         ''
						params:       tarr1(base_type)
						return_type:  base_type
						has_receiver: true
						params_known: true
					}
				}
				'keys' {
					return CallInfo{
						name:         'map.keys'
						params:       tarr1(base_type)
						return_type:  Type(Array{
							elem_type: clean.key_type
						})
						has_receiver: true
						params_known: true
					}
				}
				'values' {
					return CallInfo{
						name:         'map.values'
						params:       tarr1(base_type)
						return_type:  Type(Array{
							elem_type: clean.value_type
						})
						has_receiver: true
						params_known: true
					}
				}
				else {}
			}
		}
		if clean is Array {
			match fn_node.value {
				'first', 'last', 'pop', 'pop_left' {
					return CallInfo{
						name:         ''
						params:       tarr1(base_type)
						return_type:  clean.elem_type
						has_receiver: true
						params_known: true
					}
				}
				'contains' {
					elem_type := tc.array_contains_elem_type(base_node, clean)
					return CallInfo{
						name:         ''
						params:       tarr2(base_type, elem_type)
						return_type:  Type(bool_)
						has_receiver: true
						params_known: true
					}
				}
				'join' {
					return CallInfo{
						name:         'array.join'
						params:       tarr2(base_type, Type(String{}))
						return_type:  Type(String{})
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
				'hex' {
					if tc.is_builtin_hex_receiver(base_type) {
						return CallInfo{
							name:         '[]u8.hex'
							params:       tarr1(base_type)
							return_type:  Type(string_)
							has_receiver: true
							params_known: true
						}
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
				'insert', 'prepend' {
					params := if fn_node.value == 'insert' {
						tarr3(base_type, Type(int_), clean.elem_type)
					} else {
						tarr2(base_type, clean.elem_type)
					}
					return CallInfo{
						name:         'array.${fn_node.value}'
						params:       params
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
				'sort_with_compare' {
					return CallInfo{
						name:         'array.sort_with_compare'
						params:       [
							Type(Pointer{
								base_type: base_type
							}),
							Type(FnType{
								params:      [
									Type(Pointer{
										base_type: clean.elem_type
									}),
									Type(Pointer{
										base_type: clean.elem_type
									}),
								]
								return_type: Type(int_)
							}),
						]
						return_type:  Type(void_)
						has_receiver: true
						params_known: true
					}
				}
				'sorted_with_compare' {
					return CallInfo{
						name:         'array.sorted_with_compare'
						params:       [base_type,
							Type(FnType{
								params:      [
									Type(Pointer{
										base_type: clean.elem_type
									}),
									Type(Pointer{
										base_type: clean.elem_type
									}),
								]
								return_type: Type(int_)
							})]
						return_type:  base_type
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
			if fn_node.value == 'str' && (clean is Primitive || clean is Char || clean is Rune) {
				return CallInfo{
					name:         ''
					params:       tarr1(base_type)
					return_type:  Type(string_)
					has_receiver: true
					params_known: true
				}
			}
			for mname in receiver_method_name_candidates(clean, fn_node.value, tc.cur_module) {
				if mname in tc.fn_ret_types {
					if !tc.method_can_be_called_on_receiver(base_type, fn_node.value, mname) {
						continue
					}
					if clean_map := map_type_from_receiver(clean) {
						if info := tc.map_builtin_call_info(base_type, clean_map, fn_node.value,
							mname)
						{
							return info
						}
					}
					return tc.call_info(mname, true)
				}
			}
			if fixed_array := fixed_array_type_from_receiver(clean) {
				if fn_node.value == 'pointers' {
					if info := tc.fixed_array_dynamic_receiver_call_info(base_type, fixed_array,
						fn_node.value)
					{
						return info
					}
					if base_type !is Pointer && !tc.expr_can_take_address(base_id) {
						tc.record_error(.call_arg_mismatch,
							'fixed array receiver for `pointers` must be addressable', id)
					}
					if info := tc.fixed_array_pointers_call_info(base_type) {
						return info
					}
				}
			}
			if info := tc.embedded_method_call_info(type_name, fn_node.value) {
				return info
			}
			if info := tc.resolve_generic_struct_method(type_name, fn_node.value) {
				return info
			}
			if fn_node.value == 'use' && clean is Struct
				&& tc.struct_has_middleware_receiver(type_name) {
				return CallInfo{
					name:         '${type_name}.use'
					params:       []Type{}
					return_type:  Type(void_)
					has_receiver: true
					params_known: false
				}
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
		if fn_node.value == 'str' {
			return CallInfo{
				name:         ''
				params:       tarr1(base_type)
				return_type:  Type(string_)
				has_receiver: true
				params_known: true
			}
		}
		if fn_node.value == 'hex' && tc.is_builtin_hex_receiver(base_type) {
			return CallInfo{
				name:         ''
				params:       tarr1(base_type)
				return_type:  Type(string_)
				has_receiver: true
				params_known: true
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
		if fn_node.value == 'malloc' {
			return CallInfo{
				name:         'malloc'
				params:       tarr1(Type(ISize{}))
				return_type:  Type(Pointer{
					base_type: Type(u8_)
				})
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
		if imported_name := tc.resolve_selective_import_symbol(fn_node.value) {
			return tc.call_info(imported_name, false)
		}
		if fn_node.value in tc.fn_ret_types {
			return tc.call_info(fn_node.value, false)
		}
		if fn_node.value in ['print', 'println', 'eprint', 'eprintln', 'panic'] {
			return CallInfo{
				name:         fn_node.value
				params:       []Type{}
				return_type:  Type(void_)
				params_known: false
			}
		}
	}
	return none
}

fn (tc &TypeChecker) is_builtin_hex_receiver(typ Type) bool {
	if tc.type_is_pointer_receiver(typ) {
		return false
	}
	clean := typ
	if clean is Alias {
		return tc.is_builtin_hex_receiver(clean.base_type)
	}
	if clean is Array {
		return is_byte_type(clean.elem_type)
	}
	if clean is Primitive {
		return prim_c_type_from(clean.props, clean.size) in ['u8', 'i8', 'u16', 'i16', 'u32', 'int',
			'u64', 'i64']
	}
	return clean is Rune || clean is Char
}

fn (tc &TypeChecker) type_is_pointer_receiver(typ Type) bool {
	if typ is Pointer {
		return true
	}
	if typ is Alias {
		return tc.type_is_pointer_receiver(typ.base_type)
	}
	return false
}

fn (tc &TypeChecker) method_can_be_called_on_receiver(receiver Type, method string, method_name string) bool {
	if method != 'hex' || !tc.type_is_pointer_receiver(receiver) {
		return true
	}
	params := tc.fn_param_types[method_name] or { return false }
	if params.len == 0 {
		return false
	}
	return tc.type_is_pointer_receiver(params[0])
}

fn (tc &TypeChecker) receiver_expr_is_pointer(id flat.NodeId) bool {
	if int(id) < 0 {
		return false
	}
	node := tc.a.nodes[int(id)]
	if node.kind == .ident {
		if typ := tc.cur_scope.lookup(node.value) {
			return tc.type_is_pointer_receiver(typ)
		}
	}
	if node.typ.starts_with('&') {
		return true
	}
	return tc.type_is_pointer_receiver(tc.resolve_type(id))
}

fn is_byte_type(typ Type) bool {
	if typ is Alias {
		return is_byte_type(typ.base_type)
	}
	return typ is Primitive && typ.props.has(.integer) && typ.props.has(.unsigned) && typ.size == 8
}

fn (mut tc TypeChecker) resolve_generic_call_info(id flat.NodeId, fn_node flat.Node) ?CallInfo {
	if fn_node.kind != .index || fn_node.children_count < 2 || fn_node.value == 'range' {
		return none
	}
	base_id := tc.a.child(&fn_node, 0)
	type_args := tc.generic_call_type_arg_names(fn_node)
	if type_args.len == 0 {
		return none
	}
	base_node := tc.a.nodes[int(base_id)]
	if base_node.kind == .selector && base_node.children_count > 0 {
		recv_id := tc.a.child(&base_node, 0)
		base_type := tc.resolve_type(recv_id)
		clean := unwrap_pointer(base_type)
		type_name := resolve_type_name_for_method(clean)
		if type_name.len > 0 {
			call_name := '${type_name}.${base_node.value}'
			if call_name in tc.fn_ret_types {
				if tc.explicit_generic_arg_count_mismatch(call_name, type_args, id) {
					return tc.failed_explicit_generic_call_info(call_name)
				}
				if info := tc.explicit_generic_call_info(call_name, true, type_args) {
					return info
				}
				return tc.call_info(call_name, true)
			}
		}
	}
	call_name := tc.generic_call_base_name(base_node) or { return none }
	if is_veb_run_at_call_name(call_name) {
		return CallInfo{
			name:         call_name
			params:       []Type{}
			return_type:  Type(ResultType{
				base_type: Type(void_)
			})
			params_known: false
		}
	}
	if call_name !in tc.fn_ret_types {
		return none
	}
	if is_decode_call_name(call_name) {
		return CallInfo{
			name:          call_name
			params:        tc.fn_param_types[call_name] or { []Type{} }
			return_type:   Type(ResultType{
				base_type: tc.parse_type(type_args[0])
			})
			has_receiver:  false
			is_variadic:   tc.fn_variadic[call_name] or { false }
			is_c_variadic: tc.c_variadic_fns[call_name] or { false }
			params_known:  call_name in tc.fn_param_types
		}
	}
	if tc.explicit_generic_arg_count_mismatch(call_name, type_args, id) {
		return tc.failed_explicit_generic_call_info(call_name)
	}
	if info := tc.explicit_generic_call_info(call_name, false, type_args) {
		return info
	}
	return tc.call_info(call_name, false)
}

fn (mut tc TypeChecker) explicit_generic_arg_count_mismatch(name string, type_args []string, id flat.NodeId) bool {
	generic_params := tc.fn_generic_params[name] or { return false }
	if type_args.len == generic_params.len {
		return false
	}
	if tc.should_diagnose(id) {
		tc.record_error(.call_arg_mismatch,
			'generic argument count mismatch for `${name}`: expected ${generic_params.len}, got ${type_args.len}',
			id)
	}
	return true
}

fn (tc &TypeChecker) failed_explicit_generic_call_info(name string) CallInfo {
	return CallInfo{
		name:         name
		params:       []Type{}
		return_type:  unknown_type('invalid explicit generic call `${name}`')
		params_known: false
	}
}

fn (mut tc TypeChecker) explicit_generic_call_info(name string, has_receiver bool, type_args []string) ?CallInfo {
	generic_params := tc.fn_generic_params[name] or { return none }
	param_texts := tc.fn_param_type_texts[name] or { return none }
	if generic_params.len == 0 || type_args.len != generic_params.len {
		return none
	}
	mut concrete_args := []string{cap: generic_params.len}
	for i in 0 .. generic_params.len {
		concrete_args << tc.qualify_type_text(type_args[i])
	}
	mut sub_params := []Type{}
	for param_text in param_texts {
		sub_params << tc.parse_fn_signature_type(name, subst_generic_text(param_text,
			concrete_args, generic_params))
	}
	ret_text := tc.fn_ret_type_texts[name] or { '' }
	sub_ret := if ret_text.len > 0 {
		tc.parse_fn_signature_type(name,
			subst_generic_text(ret_text, concrete_args, generic_params))
	} else {
		tc.fn_ret_types[name] or { Type(void_) }
	}
	return CallInfo{
		name:                 name
		params:               sub_params
		return_type:          sub_ret
		has_receiver:         has_receiver
		is_variadic:          tc.fn_variadic[name] or { false }
		is_c_variadic:        tc.c_variadic_fns[name] or { false }
		params_known:         true
		has_implicit_veb_ctx: tc.fn_implicit_veb_ctx[name] or { false }
	}
}

fn is_decode_call_name(name string) bool {
	return name in ['json.decode', 'json2.decode']
}

fn is_veb_run_at_call_name(name string) bool {
	return name == 'veb.run_at'
}

fn (tc &TypeChecker) generic_call_base_name(base_node flat.Node) ?string {
	if base_node.kind == .ident {
		qfn := tc.qualify_fn_name(base_node.value)
		if qfn in tc.fn_ret_types {
			return qfn
		}
		if imported_name := tc.resolve_selective_import_symbol(base_node.value) {
			return imported_name
		}
		if base_node.value in tc.fn_ret_types {
			return base_node.value
		}
		return none
	}
	if base_node.kind == .selector && base_node.children_count > 0 {
		inner := tc.a.child_node(&base_node, 0)
		if inner.kind == .ident {
			mod_name := tc.resolve_import_alias(inner.value) or { inner.value }
			full_name := '${mod_name}.${base_node.value}'
			if is_veb_run_at_call_name(full_name) {
				return full_name
			}
			if full_name in tc.fn_ret_types {
				return full_name
			}
		}
	}
	return none
}

fn (tc &TypeChecker) generic_call_type_arg_names(index_node flat.Node) []string {
	if index_node.kind != .index || index_node.children_count < 2 || index_node.value == 'range' {
		return []string{}
	}
	mut args := []string{}
	for i in 1 .. index_node.children_count {
		arg := tc.generic_call_type_arg_name(tc.a.child(&index_node, i))
		if arg.len == 0 {
			return []string{}
		}
		args << arg
	}
	return args
}

fn (tc &TypeChecker) generic_call_type_arg_name(id flat.NodeId) string {
	if int(id) < 0 {
		return ''
	}
	node := tc.a.nodes[int(id)]
	match node.kind {
		.ident {
			return node.value
		}
		.selector {
			if node.children_count == 0 {
				return node.value
			}
			base := tc.generic_call_type_arg_name(tc.a.child(&node, 0))
			if base.len == 0 {
				return node.value
			}
			return '${base}.${node.value}'
		}
		.index {
			if node.children_count < 2 || node.value == 'range' {
				return ''
			}
			base := tc.generic_call_type_arg_name(tc.a.child(&node, 0))
			if base.len == 0 {
				return ''
			}
			mut args := []string{}
			for i in 1 .. node.children_count {
				arg := tc.generic_call_type_arg_name(tc.a.child(&node, i))
				if arg.len == 0 {
					return ''
				}
				args << arg
			}
			return '${base}[${args.join(', ')}]'
		}
		.array_init {
			if node.value.len > 0 {
				return '[]${node.value}'
			}
			return ''
		}
		.map_init {
			return node.value
		}
		.prefix {
			if node.children_count == 0 {
				return ''
			}
			child := tc.generic_call_type_arg_name(tc.a.child(&node, 0))
			if child.len == 0 {
				return ''
			}
			if node.op == .amp {
				return '&${child}'
			}
			return child
		}
		else {
			return ''
		}
	}
}

// call_info updates call info state for TypeChecker.
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
		name:                 name
		params:               params
		return_type:          tc.fn_ret_types[name] or {
			unknown_type('unknown return type for `${name}`')
		}
		has_receiver:         has_receiver
		is_variadic:          tc.fn_variadic[name] or { false }
		is_c_variadic:        tc.c_variadic_fns[name] or { false }
		params_known:         params_known
		has_implicit_veb_ctx: tc.fn_implicit_veb_ctx[name] or { false }
	}
}

fn map_type_from_receiver(t Type) ?Map {
	if t is Map {
		return t
	}
	if t is Alias {
		return map_type_from_receiver(t.base_type)
	}
	return none
}

fn fixed_array_type_from_receiver(t Type) ?ArrayFixed {
	if t is ArrayFixed {
		return t
	}
	if t is Alias {
		return fixed_array_type_from_receiver(t.base_type)
	}
	return none
}

fn (tc &TypeChecker) fixed_array_dynamic_receiver_call_info(base_type Type, arr ArrayFixed, method string) ?CallInfo {
	array_type := Array{
		elem_type: arr.elem_type
	}
	mut candidates := []string{}
	push_receiver_method_candidate(mut candidates,
		'${resolve_type_name_for_method(Type(array_type))}.${method}')
	append_array_receiver_method_candidates(mut candidates, array_type, method, tc.cur_module)
	for mname in candidates {
		if mname !in tc.fn_ret_types {
			continue
		}
		info := tc.call_info(mname, true)
		mut params := info.params.clone()
		if params.len > 0 {
			params[0] = base_type
		}
		return CallInfo{
			name:                 info.name
			params:               params
			return_type:          info.return_type
			has_receiver:         info.has_receiver
			is_variadic:          info.is_variadic
			is_c_variadic:        info.is_c_variadic
			params_known:         info.params_known
			has_implicit_veb_ctx: info.has_implicit_veb_ctx
		}
	}
	return none
}

fn (tc &TypeChecker) fixed_array_pointers_call_info(base_type Type) ?CallInfo {
	mname := 'array.pointers'
	return_type := tc.fn_ret_types[mname] or { return none }
	return CallInfo{
		name:         mname
		params:       tarr1(base_type)
		return_type:  return_type
		has_receiver: true
		params_known: true
	}
}

fn (tc &TypeChecker) expr_can_take_address(id flat.NodeId) bool {
	if int(id) < 0 {
		return false
	}
	node := tc.a.nodes[int(id)]
	match node.kind {
		.ident {
			return true
		}
		.index {
			if node.value == 'range' {
				return false
			}
			if node.children_count > 0 {
				base_id := tc.a.child(&node, 0)
				base_type0 := unwrap_pointer(tc.resolve_type(base_id))
				mut base_type := base_type0
				if base_type0 is Alias {
					base_type = base_type0.base_type
				}
				if base_type is Map {
					return false
				}
			}
			return node.children_count > 0 && tc.expr_can_take_address(tc.a.child(&node, 0))
		}
		.selector {
			if node.children_count == 0 {
				return false
			}
			return tc.expr_can_take_address(tc.a.child(&node, 0))
		}
		.prefix {
			return node.op == .mul
		}
		.paren {
			if node.children_count == 0 {
				return false
			}
			return tc.expr_can_take_address(tc.a.child(&node, 0))
		}
		else {
			return false
		}
	}
}

fn (tc &TypeChecker) map_builtin_call_info(base_type Type, m Map, method string, mname string) ?CallInfo {
	if !checker_is_raw_collection_method_name(mname, 'map.') {
		return none
	}
	params := match method {
		'delete' {
			tarr2(base_type, m.key_type)
		}
		'clear', 'free', 'keys', 'values' {
			tarr1(base_type)
		}
		'move' {
			tarr1(base_type)
		}
		'reserve' {
			tarr2(base_type, Type(u32_))
		}
		else {
			return none
		}
	}

	return_type := match method {
		'keys' {
			Type(Array{
				elem_type: m.key_type
			})
		}
		'values' {
			Type(Array{
				elem_type: m.value_type
			})
		}
		'move' {
			Type(m)
		}
		else {
			Type(void_)
		}
	}

	return CallInfo{
		name:         mname
		params:       params
		return_type:  return_type
		has_receiver: true
		params_known: true
	}
}

fn checker_is_raw_collection_method_name(name string, prefix string) bool {
	if !name.starts_with(prefix) {
		return false
	}
	rest := name[prefix.len..]
	return rest.len > 0 && !rest.contains('.')
}

// is_print_style_fn_name reports whether is print style fn name applies in types.
fn is_print_style_fn_name(name string) bool {
	return name in ['print', 'println', 'eprint', 'eprintln', 'builtin.print', 'builtin.println',
		'builtin.eprint', 'builtin.eprintln']
}

// print_style_param_accepts_string updates print style param accepts string state for types.
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

// array_insert_prepend_many_arg_compatible reports whether an insert/prepend
// value argument is a many-element operand for the receiver array.
fn (tc &TypeChecker) array_insert_prepend_many_arg_compatible(info CallInfo, param_idx int, actual Type, elem_type Type) bool {
	if info.name !in ['array.insert', 'array.prepend'] {
		return false
	}
	if (info.name == 'array.insert' && param_idx != 2)
		|| (info.name == 'array.prepend' && param_idx != 1) {
		return false
	}
	mut clean := actual
	for _ in 0 .. 8 {
		if clean is Alias {
			clean = clean.base_type
			continue
		}
		break
	}
	if clean is Array {
		return tc.receiver_compatible(clean.elem_type, elem_type)
	}
	if clean is ArrayFixed {
		return tc.receiver_compatible(clean.elem_type, elem_type)
	}
	return false
}

// check_call_arg_types validates check call arg types state for types.
fn (mut tc TypeChecker) check_call_arg_types(id flat.NodeId, node flat.Node, info0 CallInfo) {
	info := tc.specialized_plain_generic_call_info(node, info0)
	if node.children_count == 0 {
		return
	}
	if info.name in ['map.keys', 'map.values'] {
		for i in 1 .. node.children_count {
			tc.check_node(tc.call_arg_value(tc.a.child(&node, i)))
		}
		arg_count := node.children_count - 1
		if arg_count != 0 && tc.should_diagnose(id) {
			tc.record_error(.call_arg_mismatch,
				'argument count mismatch for `${tc.call_display_name(node)}`: expected 0, got ${arg_count}',
				id)
		}
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
	// A hidden veb `Context` parameter may be supplied implicitly from the
	// enclosing handler instead of by the caller, so accept argument counts both
	// with the ctx (route dispatch) and without it (handler delegation).
	ctx_count := if info.has_implicit_veb_ctx { 1 } else { 0 }
	ctx_omitted := ctx_count > 0 && actual_count < info.params.len
	min_count := tc.min_required_arg_count(info) - ctx_count
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
		if !tc.receiver_compatible(recv_type, info.params[0])
			&& !tc.receiver_embeds(recv_type, info.params[0]) {
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
		// When the caller omitted the implicit veb `Context` parameter, skip it
		// (it is inserted right after the receiver) while mapping the caller's
		// positional arguments to the callee's params.
		arg_shift := if ctx_omitted { ctx_count } else { 0 }
		param_idx := (if info.has_receiver { i } else { i - 1 }) + arg_shift
		has_dsl_scope := tc.call_arg_needs_array_dsl_scope(info.name, param_idx)
		if has_dsl_scope {
			tc.push_array_dsl_scope(node, info.name)
			tc.check_node(arg_id)
		} else {
			tc.check_node(arg_id)
		}
		if info.is_c_variadic && param_idx >= c_variadic_fixed_param_count(info) {
			if has_dsl_scope {
				tc.pop_scope()
			}
			continue
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
			actual := tc.resolve_expr(arg_id, elem_type)
			actual_name := actual.name()
			expected_name := elem_type.name()
			actual_raw := actual
			if actual is Array {
				if !tc.receiver_compatible(actual_raw, elem_type)
					&& !tc.receiver_compatible(actual_raw, expected) {
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
			if tc.array_insert_prepend_many_arg_compatible(info, param_idx, actual, expected) {
				continue
			}
			if tc.array_dsl_fn_arg_compatible(node, info, param_idx, actual) {
				continue
			}
			if tc.is_os_args_contains_call(node) && param_idx == 1 && actual is String {
				continue
			}
			tc.type_mismatch(.call_arg_mismatch, 'cannot use `${actual.name()}` as argument ${
				param_idx + 1} to `${tc.call_display_name(node)}`; expected `${expected.name()}`',
				id)
		}
	}
}

fn (mut tc TypeChecker) specialized_plain_generic_call_info(node flat.Node, info CallInfo) CallInfo {
	generic_params := tc.fn_generic_params[info.name] or { return info }
	param_texts := tc.fn_param_type_texts[info.name] or { return info }
	if generic_params.len == 0 || node.children_count <= 1 {
		return info
	}
	mut inferred := map[string]string{}
	mut first_param_idx := 0
	if info.has_receiver && param_texts.len > 0 {
		fn_node := tc.a.child_node(&node, 0)
		if fn_node.kind == .selector && fn_node.children_count > 0 {
			recv_id := tc.a.child(fn_node, 0)
			actual := tc.resolve_type(recv_id)
			tc.infer_generic_type_text_from_type(param_texts[0], actual, generic_params, mut
				inferred)
			first_param_idx = 1
		}
	}
	for param_idx in first_param_idx .. param_texts.len {
		arg_idx := param_idx - first_param_idx + 1
		if arg_idx >= node.children_count {
			break
		}
		arg_id := tc.call_arg_value(tc.a.child(&node, arg_idx))
		actual := tc.resolve_type(arg_id)
		tc.infer_generic_type_text_from_type(param_texts[param_idx], actual, generic_params, mut
			inferred)
	}
	mut concrete_args := []string{cap: generic_params.len}
	for param in generic_params {
		arg := inferred[param] or { return info }
		concrete_args << arg
	}
	mut sub_params := []Type{}
	for param_text in param_texts {
		sub_params << tc.parse_fn_signature_type(info.name, subst_generic_text(param_text,
			concrete_args, generic_params))
	}
	ret_text := tc.fn_ret_type_texts[info.name] or { '' }
	sub_ret := if ret_text.len > 0 {
		tc.parse_fn_signature_type(info.name, subst_generic_text(ret_text, concrete_args,
			generic_params))
	} else {
		info.return_type
	}
	return CallInfo{
		name:                 info.name
		params:               sub_params
		return_type:          sub_ret
		has_receiver:         info.has_receiver
		is_variadic:          info.is_variadic
		is_c_variadic:        info.is_c_variadic
		params_known:         true
		has_implicit_veb_ctx: info.has_implicit_veb_ctx
	}
}

fn (tc &TypeChecker) parse_fn_signature_type(name string, typ string) Type {
	decl_file := tc.fn_type_files[name] or { return tc.parse_type(typ) }
	mut scoped := *tc
	scoped.cur_file = decl_file
	scoped.cur_module = tc.fn_type_modules[name] or {
		tc.file_modules[decl_file] or { tc.cur_module }
	}
	scoped.type_cache = &TypeCache{
		parse_enabled: if tc.type_cache != unsafe { nil } {
			tc.type_cache.parse_enabled
		} else {
			false
		}
		parse_entries: map[string]Type{}
		c_entries:     map[string]string{}
	}
	return scoped.parse_type(typ)
}

fn (mut tc TypeChecker) infer_generic_type_text_from_type(param_text string, actual Type, generic_params []string, mut inferred map[string]string) {
	clean := param_text.trim_space()
	if clean.len == 0 {
		return
	}
	if clean.starts_with('&') {
		if actual is Pointer {
			tc.infer_generic_type_text_from_type(clean[1..], actual.base_type, generic_params, mut
				inferred)
		} else {
			tc.infer_generic_type_text_from_type(clean[1..], actual, generic_params, mut inferred)
		}
		return
	}
	if clean.starts_with('mut ') {
		tc.infer_generic_type_text_from_type(clean[4..], actual, generic_params, mut inferred)
		return
	}
	if clean.starts_with('...') {
		if actual is Array {
			tc.infer_generic_type_text_from_type(clean[3..], actual.elem_type, generic_params, mut
				inferred)
		}
		return
	}
	if clean.starts_with('[]') {
		if actual is Array {
			tc.infer_generic_type_text_from_type(clean[2..], actual.elem_type, generic_params, mut
				inferred)
		}
		return
	}
	if clean.starts_with('?') {
		if actual is OptionType {
			tc.infer_generic_type_text_from_type(clean[1..], actual.base_type, generic_params, mut
				inferred)
		}
		return
	}
	if clean.starts_with('!') {
		if actual is ResultType {
			tc.infer_generic_type_text_from_type(clean[1..], actual.base_type, generic_params, mut
				inferred)
		}
		return
	}
	if clean.starts_with('fn(') || clean.starts_with('fn (') {
		if actual is FnType {
			tc.infer_generic_fn_type_text_from_type(clean, actual, generic_params, mut inferred)
		}
		return
	}
	for param in generic_params {
		if clean == param && param !in inferred {
			inferred[param] = actual.name()
			return
		}
	}
}

fn (mut tc TypeChecker) infer_generic_fn_type_text_from_type(param_text string, actual FnType, generic_params []string, mut inferred map[string]string) {
	params_start := param_text.index_u8(`(`) + 1
	mut depth := 1
	mut params_end := params_start
	for params_end < param_text.len {
		if param_text[params_end] == `(` {
			depth++
		} else if param_text[params_end] == `)` {
			depth--
			if depth == 0 {
				break
			}
		}
		params_end++
	}
	if params_end >= param_text.len {
		return
	}
	parts := split_params(param_text[params_start..params_end])
	for i, part in parts {
		if i >= actual.params.len {
			break
		}
		tc.infer_generic_type_text_from_type(normalize_fn_type_param_text(part), fn_param_type(actual,
			i), generic_params, mut inferred)
	}
	ret := param_text[params_end + 1..].trim_space()
	if ret.len > 0 {
		tc.infer_generic_type_text_from_type(ret, actual.return_type, generic_params, mut inferred)
	}
}

// array_map_return_elem_type supports array map return elem type handling for TypeChecker.
fn (mut tc TypeChecker) array_map_return_elem_type(node flat.Node) Type {
	if node.children_count < 2 {
		return Type(void_)
	}
	tc.push_array_dsl_scope(node, 'array.map')
	arg_id := tc.call_arg_value(tc.a.child(&node, 1))
	tc.check_node(arg_id)
	elem_type := tc.resolve_type(arg_id)
	tc.pop_scope()
	if fn_typ := fn_type_from_type(elem_type) {
		return fn_typ.return_type
	}
	if elem_type is Void || elem_type is Unknown {
		return Type(void_)
	}
	return elem_type
}

fn (tc &TypeChecker) array_contains_elem_type(base_node flat.Node, array_type Array) Type {
	if base_node.kind == .selector && base_node.value == 'args' && base_node.children_count > 0 {
		parent := tc.a.child_node(&base_node, 0)
		if parent.kind == .ident && parent.value == 'os' {
			return Type(String{})
		}
	}
	return array_type.elem_type
}

fn (tc &TypeChecker) is_os_args_contains_call(node flat.Node) bool {
	if node.children_count == 0 {
		return false
	}
	fn_node := tc.a.child_node(&node, 0)
	if fn_node.kind != .selector || fn_node.value != 'contains' || fn_node.children_count == 0 {
		return false
	}
	base_node := tc.a.child_node(fn_node, 0)
	if base_node.kind != .selector || base_node.value != 'args' || base_node.children_count == 0 {
		return false
	}
	parent := tc.a.child_node(base_node, 0)
	return parent.kind == .ident && parent.value == 'os'
}

fn (tc &TypeChecker) pointer_builtin_method_call_info(base_type Type, method string) ?CallInfo {
	receiver := pointer_builtin_receiver_name(base_type)
	if receiver.len == 0 {
		return none
	}
	if receiver in ['charptr', 'byteptr'] && method in ['vstring', 'vstring_with_len'] {
		mut params := tarr1(base_type)
		if method == 'vstring_with_len' {
			params << Type(int_)
		}
		return CallInfo{
			name:         '${receiver}.${method}'
			params:       params
			return_type:  Type(string_)
			has_receiver: true
			params_known: true
		}
	}
	if receiver in ['byteptr', 'voidptr'] && method == 'vbytes' {
		return CallInfo{
			name:         '${receiver}.${method}'
			params:       [base_type, Type(int_)]
			return_type:  Type(Array{
				elem_type: Type(u8_)
			})
			has_receiver: true
			params_known: true
		}
	}
	return none
}

fn pointer_builtin_receiver_name(typ Type) string {
	if typ is Alias {
		if typ.name in ['charptr', 'byteptr', 'voidptr'] {
			return typ.name
		}
		return pointer_builtin_receiver_name(typ.base_type)
	}
	if typ is Pointer {
		base := typ.base_type
		if base is Alias {
			if base.name == 'byte' {
				return 'byteptr'
			}
			return pointer_builtin_receiver_name(base)
		}
		if base is Char {
			return 'charptr'
		}
		if base is Void {
			return 'voidptr'
		}
		if base is Primitive && prim_name(base) == 'u8' {
			return 'byteptr'
		}
	}
	return ''
}

// min_required_arg_count supports min required arg count handling for TypeChecker.
fn (tc &TypeChecker) min_required_arg_count(info CallInfo) int {
	if info.is_variadic && info.params.len > 0 {
		return info.params.len - 1
	}
	mut n := info.params.len
	for n > 0 {
		param := info.params[n - 1]
		if tc.is_params_struct_type(param) {
			n--
			continue
		}
		break
	}
	return n
}

fn c_variadic_fixed_param_count(info CallInfo) int {
	if info.is_c_variadic && info.params.len > 0 && info.params[info.params.len - 1] is Array {
		return info.params.len - 1
	}
	return info.params.len
}

fn (tc &TypeChecker) is_params_struct_type(typ Type) bool {
	if typ is Struct {
		if typ.name in tc.params_structs {
			return true
		}
		qname := tc.qualify_name(typ.name)
		return qname in tc.params_structs
	}
	if typ is Alias {
		return tc.is_params_struct_type(typ.base_type)
	}
	return false
}

// call_arg_needs_array_dsl_scope updates call arg needs array dsl scope state for TypeChecker.
fn (tc &TypeChecker) call_arg_needs_array_dsl_scope(name string, param_idx int) bool {
	return param_idx == 1 && is_array_dsl_call_name(name)
}

fn (tc &TypeChecker) array_dsl_fn_arg_compatible(node flat.Node, info CallInfo, param_idx int, actual Type) bool {
	if param_idx != 1 || info.name !in ['array.filter', 'array.map'] {
		return false
	}
	fn_typ := fn_type_from_type(actual) or { return false }
	if fn_typ.params.len != 1 {
		return false
	}
	arr := tc.call_receiver_array_type(node) or { return false }
	param := fn_param_type(fn_typ, 0)
	if !tc.receiver_compatible(param, arr.elem_type)
		&& !tc.receiver_compatible(arr.elem_type, param) {
		return false
	}
	if info.name == 'array.filter' {
		return tc.type_compatible(fn_typ.return_type, Type(bool_))
	}
	return true
}

// is_array_dsl_call_name reports whether is array dsl call name applies in types.
fn is_array_dsl_call_name(name string) bool {
	return name in ['array.filter', 'array.any', 'array.all', 'array.count', 'array.map',
		'array.sort', 'array.sorted']
}

// call_explicit_arg_count updates call explicit arg count state for types.
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

// push_array_dsl_scope updates push array dsl scope state for TypeChecker.
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

// call_receiver_array_type updates call receiver array type state for TypeChecker.
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

// call_arg_value updates call arg value state for TypeChecker.
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

// receiver_compatible supports receiver compatible handling for TypeChecker.
fn (tc &TypeChecker) receiver_compatible(actual Type, expected Type) bool {
	if tc.type_compatible(actual, expected) {
		return true
	}
	// Check the generic-base relaxation before the pointer fallbacks: it unwraps
	// pointers itself, so a bare `&Box{...}` literal still matches an expected
	// `&Box[int]` (while `&Box[string]` vs `&Box[int]` stays rejected).
	if tc.generic_receiver_base_match(actual, expected) {
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

// generic_receiver_base_match relaxes compatibility between two same-base generic
// struct types when at least one side is the open/bare generic form — a bare
// `Vec4{...}` literal specializing to the expected `Vec4[f32]`, or a concrete
// `Vec4[f32]` value matching the open `Vec4` / `Vec4[T]` method-receiver form.
//
// It deliberately does NOT relax two *different concrete* instantiations. Because
// `receiver_compatible` is also used for ordinary call arguments, field inits,
// array literals, and expected-type propagation, conflating them would let
// `Box[string]` satisfy an expected `Box[int]` and emit incompatible C structs.
fn (tc &TypeChecker) generic_receiver_base_match(actual Type, expected Type) bool {
	// Both sides must share pointer shape before unwrapping: a `&Box{...}` value must
	// not satisfy an expected `Box[int]` value. The C argument path only
	// auto-dereferences when the actual/expected type names match exactly, so the bare
	// `Box` vs `Box[int]` mismatch would otherwise be emitted as a pointer where a value
	// is required. (A pointer receiver on a value-receiver method is handled by
	// receiver_compatible's pointer fallbacks, not here.)
	if (actual is Pointer) != (expected is Pointer) {
		return false
	}
	a_full := unwrap_pointer(actual).name()
	e_full := unwrap_pointer(expected).name()
	a := strip_generic_args_name(a_full)
	if a.len == 0 || a != strip_generic_args_name(e_full) {
		return false
	}
	if a !in tc.struct_generic_params {
		return false
	}
	if a_full == e_full {
		return false
	}
	// Reject two *different concrete* instantiations (`Box[string]` vs `Box[int]`),
	// which produce incompatible C structs. Relax only when at least one side is
	// the open/bare generic form: a bare `Box{...}` literal specializing to the
	// expected `Box[int]`, or a concrete `Box[int]` value matching the open
	// `Box`/`Box[T]` method-receiver form.
	if tc.is_concrete_generic_instance(a_full) && tc.is_concrete_generic_instance(e_full) {
		return false
	}
	return true
}

// is_concrete_generic_instance reports whether `name` is a fully concrete generic
// instantiation (e.g. `Box[int]`), as opposed to the bare base (`Box`) or an open
// parameter form (`Box[T]`).
fn (tc &TypeChecker) is_concrete_generic_instance(name string) bool {
	_, args, ok := generic_type_application_parts(name)
	if !ok {
		return false
	}
	return tc.generic_args_are_concrete(args)
}

// bare_generic_literal_adopts reports whether a struct literal written as the bare
// generic base (`Box{...}`, no type args) should adopt the concrete `expected`
// instance (`Box[int]`, optionally behind a pointer). The base short-names must match
// and the base must be a known generic struct, so a non-generic same-named struct is
// left to ordinary checking.
fn (tc &TypeChecker) bare_generic_literal_adopts(lit_value string, expected Type) bool {
	if lit_value.len == 0 || lit_value.contains('[') {
		return false
	}
	e_base, _, e_ok := generic_type_application_parts(unwrap_pointer(expected).name())
	if !e_ok || e_base.all_after_last('.') != lit_value.all_after_last('.') {
		return false
	}
	return e_base in tc.struct_generic_params
		|| e_base.all_after_last('.') in tc.struct_generic_params
}

// generic_literal_fields_compatible checks a bare generic struct literal's named
// field initializers against the expected concrete instantiation (`Box[int]`),
// substituting the struct's type parameters into each field's declared type. It
// returns false only on a *definite* mismatch (e.g. `Box{v: 'str'}` for `Box[int]`),
// so a clearly-unrelated literal yields a clean checker error instead of adopting the
// type and emitting broken C; unresolvable fields stay lenient.
fn (mut tc TypeChecker) generic_literal_fields_compatible(node flat.Node, expected Type) bool {
	e_base, e_args, e_ok := generic_type_application_parts(unwrap_pointer(expected).name())
	if !e_ok {
		return true
	}
	params := tc.struct_generic_params[e_base] or {
		tc.struct_generic_params[e_base.all_after_last('.')] or { return true }
	}
	if params.len != e_args.len {
		return true
	}
	fields := tc.structs[e_base] or { tc.structs[e_base.all_after_last('.')] or { return true } }
	for i in 0 .. node.children_count {
		fi := tc.a.child_node(&node, i)
		if fi.kind != .field_init || fi.children_count == 0 {
			continue
		}
		mut decl_typ := Type(void_)
		mut found := false
		if fi.value.len > 0 {
			// Named initializer (`Box{v: 'x'}`): match by field name.
			for f in fields {
				if f.name == fi.value {
					decl_typ = f.typ
					found = true
					break
				}
			}
		} else if i < fields.len {
			// Positional initializer (`Box{'x'}`): the parser emits a `field_init` with
			// an empty name, so match by field order like `check_struct_init` does.
			decl_typ = fields[i].typ
			found = true
		}
		if !found {
			continue
		}
		sub := tc.substitute_generic_type(decl_typ, e_args, params)
		if sub is Unknown || sub is Void {
			continue
		}
		actual := tc.resolve_expr(tc.a.child(fi, 0), sub)
		if !tc.receiver_compatible(actual, sub) {
			return false
		}
	}
	return true
}

// strip_generic_args_name returns the base name of a generic instance type
// (`Box[int]` -> `Box`); array/map types (leading `[`) yield the name unchanged.
fn strip_generic_args_name(name string) string {
	bracket := name.index_u8(`[`)
	if bracket <= 0 {
		return name
	}
	return name[..bracket]
}

// is_zero_literal reports whether is zero literal applies in types.
fn (tc &TypeChecker) is_zero_literal(id flat.NodeId) bool {
	if int(id) < 0 {
		return false
	}
	node := tc.a.nodes[int(id)]
	return node.kind == .int_literal && node.value == '0'
}

// is_fn_pointer_type reports whether is fn pointer type applies in types.
fn is_fn_pointer_type(typ Type) bool {
	clean0 := typ
	mut clean := clean0
	if clean0 is Alias {
		clean = clean0.base_type
	}
	return clean is FnType
}

// fn_type_from_type converts fn type from type data for types.
fn fn_type_from_type(typ Type) ?FnType {
	if typ is FnType {
		return typ
	}
	if typ is Alias {
		return fn_type_from_type(typ.base_type)
	}
	return none
}

fn struct_type_from_type(typ Type) ?Struct {
	if typ is Struct {
		return typ
	}
	if typ is Alias {
		return struct_type_from_type(typ.base_type)
	}
	return none
}

// selector_fn_type supports selector fn type handling for TypeChecker.
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
		if typ := tc.struct_field_type(clean.name, node.value) {
			return fn_type_from_type(typ)
		}
	}
	if clean is Interface {
		if typ := tc.interface_field_type(clean.name, node.value) {
			return fn_type_from_type(typ)
		}
	}
	return none
}

fn (tc &TypeChecker) method_value_type(receiver_name string, method string) ?Type {
	method_name := '${receiver_name}.${method}'
	mut ret_type := tc.fn_ret_types[method_name] or { Type(void_) }
	mut params := tc.fn_param_types[method_name] or { []Type{} }
	if method_name !in tc.fn_ret_types && method_name !in tc.fn_param_types {
		// A concrete generic receiver (`Box[int]`) has its methods registered under the
		// open key (`Box[T].method`); resolve and substitute so a method *value* on a
		// generic struct is typed instead of reported as an unknown field.
		ci := tc.resolve_generic_struct_method(receiver_name, method) or { return none }
		ret_type = ci.return_type
		params = ci.params.clone()
	}
	mut bound_params := []Type{}
	if params.len > 1 {
		bound_params = params[1..].clone()
	}
	return Type(FnType{
		params:      bound_params
		return_type: ret_type
	})
}

// selector_fn_base_type supports selector fn base type handling for TypeChecker.
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

// direct_call_return_type supports direct call return type handling for TypeChecker.
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

// module_const_receiver_method_name supports module_const_receiver_method_name handling in types.
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
	for method_name in receiver_method_name_candidates(clean, method, mod_name) {
		if method_name in tc.fn_ret_types {
			return method_name
		}
	}
	return none
}

// valid_string_data supports valid string data handling for types.
fn valid_string_data(s string) bool {
	if s.len == 0 {
		return true
	}
	ptr := unsafe { u64(voidptr(s.str)) }
	return ptr >= 4096 && ptr < 281474976710656 && s.len < 1048576
}

// clone_smartcasts supports clone smartcasts handling for types.
fn clone_smartcasts(src map[string]Type) map[string]Type {
	mut dst := map[string]Type{}
	for key, typ in src {
		if valid_string_data(key) {
			dst[key] = typ
		}
	}
	return dst
}

// array_elem_type supports array elem type handling for types.
fn array_elem_type(arr Array) Type {
	return arr.elem_type
}

// array_like_elem_type returns the element type of an `Array` or `ArrayFixed`.
fn array_like_elem_type(t Type) ?Type {
	if t is Array {
		return t.elem_type
	}
	if t is ArrayFixed {
		return t.elem_type
	}
	return none
}

// if_branch_types_compatible reports whether two if-expression branch types are
// compatible. Bare array literals (`[a, b, c]`) resolve to a fixed `T[n]`, but V
// treats them as dynamic `[]T`; two *literal* branches with compatible element
// types must therefore not be flagged as a mismatch merely because their lengths
// differ. The length-agnostic relaxation is limited to literal tails: genuine
// fixed-array values keep their length (handled by `type_compatible` above), so
// e.g. `[2]int` vs `[3]int` branches still mismatch.
fn (tc &TypeChecker) if_branch_types_compatible(a Type, b Type, a_is_array_lit bool, b_is_array_lit bool) bool {
	if tc.type_compatible(a, b) || tc.type_compatible(b, a) {
		return true
	}
	if !a_is_array_lit || !b_is_array_lit {
		return false
	}
	a_elem := array_like_elem_type(a) or { return false }
	b_elem := array_like_elem_type(b) or { return false }
	return tc.type_compatible(a_elem, b_elem) || tc.type_compatible(b_elem, a_elem)
}

// branch_tail_is_array_literal reports whether a branch's value tail is a bare
// array literal (`[a, b, c]`) — directly, or through a const whose initializer is
// one. V types such values as dynamic `[]T` regardless of element count, so they
// must not constrain if-branch length compatibility. Explicit fixed-array
// initializers (`[N]T{...}`, parsed as `.array_init`) are genuine fixed arrays and
// keep their length.
fn (tc &TypeChecker) branch_tail_is_array_literal(id flat.NodeId) bool {
	return tc.expr_is_bare_array_literal(tc.branch_tail_expr_id(id))
}

// expr_is_bare_array_literal reports whether `id` is a bare `[a, b, c]` literal,
// directly or through a single const reference.
fn (tc &TypeChecker) expr_is_bare_array_literal(id flat.NodeId) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.nodes[int(id)]
	if node.kind == .array_literal {
		return true
	}
	if node.kind == .ident {
		for cand in [tc.qualify_name(node.value), node.value] {
			if expr_id := tc.const_exprs[cand] {
				if tc.valid_node_id(expr_id) {
					return tc.a.nodes[int(expr_id)].kind == .array_literal
				}
			}
		}
	}
	return false
}

// fixed_array_elem_type supports fixed array elem type handling for types.
fn fixed_array_elem_type(arr ArrayFixed) Type {
	return arr.elem_type
}

// map_value_type supports map value type handling for types.
fn map_value_type(m Map) Type {
	return m.value_type
}

// pointer_base_type supports pointer base type handling for types.
fn pointer_base_type(p Pointer) Type {
	return p.base_type
}

// fn_param_type supports fn param type handling for types.
fn fn_param_type(f FnType, idx int) Type {
	return f.params[idx]
}

// is_known_call reports whether is known call applies in types.
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
		if fn_node.value == 'hex' && tc.type_is_pointer_receiver(base_type) {
			return false
		}
		clean_type := unwrap_pointer(base_type)
		if clean_type is Array || clean_type is ArrayFixed || clean_type is Map {
			if fn_node.value == 'hex' {
				return tc.is_builtin_hex_receiver(base_type)
			}
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
				for base_mname in receiver_method_name_candidates(clean_type.base_type,
					fn_node.value, tc.cur_module) {
					if base_mname in tc.fn_ret_types {
						return true
					}
				}
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
		if _ := tc.resolve_selective_import_symbol(fn_node.value) {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) is_unsupported_hex_call(node flat.Node) bool {
	if node.children_count == 0 {
		return false
	}
	fn_node := tc.a.child_node(&node, 0)
	if fn_node.kind != .selector || fn_node.value != 'hex' || fn_node.children_count == 0 {
		return false
	}
	base_id := tc.a.child(fn_node, 0)
	if tc.receiver_expr_is_pointer(base_id) {
		return true
	}
	base_type := tc.resolve_type(base_id)
	return !tc.is_builtin_hex_receiver(base_type)
}

fn (tc &TypeChecker) call_has_ambiguous_selective_import(node flat.Node) bool {
	if node.children_count == 0 {
		return false
	}
	fn_node := tc.a.child_node(&node, 0)
	if fn_node.kind == .index && fn_node.children_count > 0 {
		base := tc.a.child_node(fn_node, 0)
		return base.kind == .ident && tc.selective_import_symbol_is_ambiguous(base.value)
	}
	return fn_node.kind == .ident && tc.selective_import_symbol_is_ambiguous(fn_node.value)
}

// call_display_name updates call display name state for TypeChecker.
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

// check_if_expr validates check if expr state for types.
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
		else_id := tc.a.child(&node, 2)
		if tc.branch_has_value_tail(then_id) && tc.branch_has_value_tail(else_id)
			&& !tc.if_branch_types_compatible(then_type, else_type, tc.branch_tail_is_array_literal(then_id), tc.branch_tail_is_array_literal(else_id)) {
			if tc.should_diagnose(id) {
				tc.record_error(.if_branch_mismatch,
					'if-expression branch type mismatch: then `${then_type.name()}` vs else `${else_type.name()}`',
					id)
			}
		}
	}
}

// branch_has_value_tail converts branch has value tail data for types.
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

// check_condition validates check condition state for types.
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

// check_bool_condition validates check bool condition state for types.
fn (mut tc TypeChecker) check_bool_condition(cond_id flat.NodeId) {
	tc.check_node(cond_id)
	cond_type := tc.resolve_type(cond_id)
	if !tc.type_compatible(cond_type, Type(bool_)) && tc.should_diagnose(cond_id) {
		tc.record_error(.condition_mismatch,
			'if condition must be `bool`, not `${cond_type.name()}`', cond_id)
	}
}

// check_if_guard validates check if guard state for types.
fn (mut tc TypeChecker) check_if_guard(id flat.NodeId, node flat.Node) []LocalBinding {
	if node.children_count < 2 {
		return []LocalBinding{}
	}
	lhs_id := tc.a.child(&node, 0)
	rhs_id := tc.a.child(&node, 1)
	tc.check_node(rhs_id)
	rhs_type := tc.resolve_type(rhs_id)
	mut payload := Type(void_)
	is_optional_result := rhs_type is OptionType || rhs_type is ResultType
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
	if payload is Void && !is_optional_result {
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

// check_match_stmt validates check match stmt state for types.
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
				if tc.type_symbol_known(pattern) {
					tc.smartcasts[subject_key] = tc.parse_type(pattern)
				}
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

// check_is_expr validates check is expr state for types.
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

// branch_tail_type supports branch tail type handling for TypeChecker.
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

// if_expr_tail_type supports if expr tail type handling for TypeChecker.
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

// choose_if_tail_type supports choose if tail type handling for types.
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

// branch_tail_expr_id returns the value-producing tail expression of a branch
// body (a `block` or `match_branch`), unwrapping a trailing `expr_stmt`. Returns
// `empty_node` when the branch has no expression tail.
fn (tc &TypeChecker) branch_tail_expr_id(id flat.NodeId) flat.NodeId {
	if !tc.valid_node_id(id) {
		return flat.empty_node
	}
	node := tc.a.nodes[int(id)]
	mut last_id := flat.empty_node
	if node.kind == .block {
		if node.children_count == 0 {
			return flat.empty_node
		}
		last_id = tc.a.child(&node, node.children_count - 1)
	} else if node.kind == .match_branch {
		body_start := if node.value == 'else' { 0 } else { node.value.int() }
		if node.children_count <= body_start {
			return flat.empty_node
		}
		last_id = tc.a.child(&node, node.children_count - 1)
	} else {
		return id
	}
	if !tc.valid_node_id(last_id) {
		return flat.empty_node
	}
	last := tc.a.nodes[int(last_id)]
	if last.kind == .expr_stmt {
		if last.children_count > 0 {
			return tc.a.child(&last, 0)
		}
		return flat.empty_node
	}
	return last_id
}

// branches_compatible_with propagates `expected` into every value-producing tail
// of a match/if expression (so context-dependent tails such as enum shorthand
// `.foo`, `none`, or fn literals type against it instead of defaulting to e.g.
// `int`). Returns true when the node is a match/if expression and every branch
// tail is compatible with `expected`.
fn (mut tc TypeChecker) branches_compatible_with(id flat.NodeId, expected Type) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.nodes[int(id)]
	if node.kind == .match_stmt {
		mut saw_branch := false
		for i in 1 .. node.children_count {
			branch_id := tc.a.child(&node, i)
			if !tc.valid_node_id(branch_id) {
				continue
			}
			if tc.a.nodes[int(branch_id)].kind != .match_branch {
				continue
			}
			tail := tc.branch_tail_expr_id(branch_id)
			if !tc.valid_node_id(tail) {
				return false
			}
			saw_branch = true
			actual := tc.resolve_expr(tail, expected)
			if !tc.receiver_compatible(actual, expected) {
				return false
			}
		}
		return saw_branch
	}
	if node.kind == .if_expr {
		// A value if-expression needs an else branch (child 2). children: cond,
		// then-block, else (block or nested if_expr).
		if node.children_count <= 2 {
			return false
		}
		then_tail := tc.branch_tail_expr_id(tc.a.child(&node, 1))
		if !tc.valid_node_id(then_tail) {
			return false
		}
		then_actual := tc.resolve_expr(then_tail, expected)
		if !tc.receiver_compatible(then_actual, expected) {
			return false
		}
		else_id := tc.a.child(&node, 2)
		if !tc.valid_node_id(else_id) {
			return false
		}
		if tc.a.nodes[int(else_id)].kind == .if_expr {
			return tc.branches_compatible_with(else_id, expected)
		}
		else_tail := tc.branch_tail_expr_id(else_id)
		if !tc.valid_node_id(else_tail) {
			return false
		}
		else_actual := tc.resolve_expr(else_tail, expected)
		return tc.receiver_compatible(else_actual, expected)
	}
	return false
}

// extract_smartcasts supports extract smartcasts handling for TypeChecker.
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

// check_struct_init validates check struct init state for types.
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
			// A method value stored in a struct field escapes the evaluation site (several
			// instances from the same `Foo{cb: obj.method}` site would share one receiver).
			tc.reject_stored_method_value(value_id)
			tc.reject_stored_capturing_fn_literal(value_id)
			mut expected := Type(void_)
			if field.value.len > 0 {
				mut found := false
				if typ := tc.struct_field_type(init_type.name, field.value) {
					expected = typ
					found = true
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

// expr_is_method_value reports whether `id` is a selector that resolves to a *method
// value* — a struct method used as a value (`obj.draw`), not a field access or a method
// call. cgen backs such a value with a per-evaluation-site static receiver slot, which is
// only safe for an immediately-consumed callback; it cannot hold several live instances
// from the same site at once (see gen_method_value_closure).
fn (tc &TypeChecker) expr_is_method_value(id flat.NodeId) bool {
	if int(id) < 0 {
		return false
	}
	node := tc.a.nodes[int(id)]
	// A local bound to a method value (`cb := c.report`) carries the same escape hazard as the
	// bare selector, so a reference to it counts as a method value here too.
	if node.kind == .ident {
		return node.value in tc.method_value_locals
	}
	if node.kind != .selector || node.children_count == 0 {
		return false
	}
	clean := unwrap_pointer(tc.resolve_type(tc.a.child(&node, 0)))
	if clean !is Struct {
		return false
	}
	sname := (clean as Struct).name
	if tc.struct_field_type(sname, node.value) != none {
		return false
	}
	if '${sname}.${node.value}' in tc.fn_param_types {
		return true
	}
	if _ := tc.resolve_generic_struct_method(sname, node.value) {
		return true
	}
	return false
}

// reject_stored_method_value reports a clear error when a method value escapes its
// evaluation site — stored in an array/map/struct field or returned. The per-site static
// receiver slot cannot keep several live instances distinct, so a factory like
// `fn bind(c Counter) fn () int { return c.report }` (or storage in a loop) would make
// every escaped callback use the last receiver; without this the value also reaches cgen
// and emits C referencing an unsupported helper. Pass method values directly as a
// callback argument instead (a real closure capture is not yet supported).
fn (mut tc TypeChecker) reject_stored_method_value(id flat.NodeId) {
	if tc.expr_is_method_value(id) && tc.should_diagnose(id) {
		tc.record_error(.assignment_mismatch,
			'a method value (`obj.method`) cannot escape its call site (no closure capture); pass it directly as a callback argument',
			id)
	}
}

fn (tc &TypeChecker) expr_is_capturing_fn_literal_value(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= tc.a.nodes.len {
		return false
	}
	node := tc.a.nodes[int(id)]
	match node.kind {
		.fn_literal {
			return tc.fn_literal_has_captures(node)
		}
		.cast_expr, .paren, .expr_stmt {
			if node.children_count == 0 {
				return false
			}
			return tc.expr_is_capturing_fn_literal_value(tc.a.child(&node, 0))
		}
		else {
			return false
		}
	}
}

fn (tc &TypeChecker) fn_literal_has_captures(node flat.Node) bool {
	if node.kind != .fn_literal {
		return false
	}
	for i in 0 .. node.children_count {
		child := tc.a.child_node(&node, i)
		if child.kind == .ident && child.value.len > 0 {
			return true
		}
	}
	return false
}

fn (mut tc TypeChecker) reject_stored_capturing_fn_literal(id flat.NodeId) {
	if tc.expr_is_capturing_fn_literal_value(id) && tc.should_diagnose(id) {
		tc.record_error(.assignment_mismatch,
			'a capturing fn literal cannot be stored or returned (no closure capture); pass it directly as a callback argument',
			id)
	}
}

// check_selector validates check selector state for types.
fn (mut tc TypeChecker) check_selector(id flat.NodeId, node flat.Node) {
	if node.children_count == 0 {
		return
	}
	base_id := tc.a.child(&node, 0)
	base := tc.a.nodes[int(base_id)]
	if tc.is_namespace_selector(node, base) {
		if typ := tc.enum_selector_type(&node) {
			tc.register_synth_type(id, typ)
		}
		return
	}
	tc.check_node(base_id)
	base_type := tc.resolve_type(base_id)
	tc.register_synth_type(base_id, base_type)
	// A value-context selector whose name is a method (not a field) of a struct
	// receiver is a method value; record the concrete `Type.method` so it survives
	// dead-code elimination (cgen emits a wrapper that calls it).
	clean_recv := unwrap_pointer(base_type)
	if clean_recv is Struct {
		if tc.struct_field_type(clean_recv.name, node.value) == none {
			mut mkey := '${clean_recv.name}.${node.value}'
			if mkey !in tc.fn_param_types {
				// Generic receiver methods are registered under the open key
				// (`Box[T].method`); mark that one reachable for the wrapper, and stash
				// the substituted signature for cgen (the open form is gone by cgen time).
				if ci := tc.resolve_generic_struct_method(clean_recv.name, node.value) {
					tc.generic_method_value_info['${clean_recv.name}.${node.value}'] = ci
					mkey = ci.name
				}
			}
			if mkey in tc.fn_param_types && tc.cur_fn_node_id >= 0 {
				// Record per enclosing function so markused marks it only when that
				// function is reachable; over-marking an unreachable method value can pull
				// in (and fail to compile) an otherwise-unused specialization. A method
				// value can only appear inside a function body — escaping to a const/global
				// is rejected elsewhere — so a non-fn context needs no recording here.
				tc.method_values_by_fn[tc.cur_fn_node_id] << mkey
				// Also record the concrete instance key (`Box[int].report`) — distinct from the
				// open key (`Box[T].report`) above — so monomorphize can gate a generic method's
				// specialization on *this* instance's method value being reachable (it shares the
				// open key with every other instance, e.g. `Box[Pair]`).
				concrete_mkey := '${clean_recv.name}.${node.value}'
				if concrete_mkey != mkey {
					tc.method_values_by_fn[tc.cur_fn_node_id] << concrete_mkey
				}
			}
		}
	}
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

// is_namespace_selector reports whether is namespace selector applies in types.
fn (tc &TypeChecker) is_namespace_selector(node flat.Node, base flat.Node) bool {
	if base.kind != .ident {
		return false
	}
	if base.value == 'C' || tc.has_active_import(base.value) {
		return true
	}
	if resolved := tc.resolve_selective_import_type_symbol(base.value) {
		if resolved in tc.structs || resolved in tc.enum_names || resolved in tc.flag_enums
			|| resolved in tc.sum_types || resolved in tc.interface_names {
			return true
		}
	}
	qbase := tc.qualify_name(base.value)
	if qbase in tc.structs || qbase in tc.enum_names || qbase in tc.sum_types
		|| qbase in tc.interface_names {
		return true
	}
	qname := '${qbase}.${node.value}'
	return qname in tc.const_types || qname in tc.fn_ret_types || qname in tc.enum_names
}

// selector_type supports selector type handling for TypeChecker.
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
		if typ := tc.struct_field_type(clean_name, node.value) {
			return typ
		}
		if typ := tc.method_value_type(clean_name, node.value) {
			return typ
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

// multi_return_selector_type supports multi return selector type handling for types.
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

// lowered_sum_selector_type supports lowered sum selector type handling for TypeChecker.
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

// sum_shared_field_type supports sum shared field type handling for TypeChecker.
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

// option_result_selector_type supports option result selector type handling for types.
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

// check_index validates check index state for types.
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
	if node.children_count > 2 {
		for i in 2 .. node.children_count {
			tc.check_node(tc.a.child(&node, i))
		}
		tc.record_error(.cannot_index,
			'index expression accepts one index, got ${node.children_count - 1}', id)
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

// check_ident validates check ident state for types.
fn (mut tc TypeChecker) check_ident(id flat.NodeId, node flat.Node) {
	if node.value.len == 0 || node.value == '_' {
		return
	}
	if is_bare_generic_param(node.value) {
		tc.register_synth_type(id, unknown_type('generic placeholder `${node.value}`'))
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
	if node.value == 'err' {
		tc.register_synth_type(id, tc.parse_type('IError'))
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
	if tc.selective_import_symbol_is_ambiguous(node.value) {
		tc.register_synth_type(id, unknown_type('ambiguous selective import `${node.value}`'))
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

// resolve_expr resolves resolve expr information for types.
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
	if payload := contextual_payload_type(expected) {
		if payload is Array || payload is ArrayFixed {
			actual := tc.resolve_expr(id, payload)
			if tc.type_compatible(actual, payload) {
				return actual
			}
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
	// A bare generic struct literal (`Box{...}` / `&Box{...}`) adopts a matching concrete
	// expected instance (`Box[int]` / `&Box[int]`), so `fn make() Box[int] { return
	// Box{...} }` and bare literals passed/assigned where a concrete instance is expected
	// type-check and carry the concrete type into codegen. A *value* literal only adopts a
	// *value* expectation: `bare_generic_literal_adopts` unwraps the pointer, so without
	// the `expected !is Pointer` guard `return Box{...}` would be accepted for an expected
	// `&Box[int]`, and cgen would emit a `Box_int` value where a `Box_int*` is required.
	// The pointer case is the `prefix .amp` (`&Box{...}`) path below.
	if node.kind == .struct_init && expected !is Pointer
		&& tc.bare_generic_literal_adopts(node.value, expected)
		&& tc.generic_literal_fields_compatible(node, expected) {
		tc.register_synth_type(id, expected_raw)
		return expected_raw
	}
	if node.kind == .prefix && node.op == .amp && node.children_count == 1 && expected is Pointer {
		child := tc.a.nodes[int(tc.a.child(&node, 0))]
		if child.kind == .struct_init && tc.bare_generic_literal_adopts(child.value, expected)
			&& tc.generic_literal_fields_compatible(child, expected) {
			tc.register_synth_type(id, expected_raw)
			return expected_raw
		}
	}
	if node.kind == .postfix && node.children_count == 1 {
		child_id := tc.a.child(&node, 0)
		child := tc.a.nodes[int(child_id)]
		if node.op == .not && child.kind == .array_literal {
			actual := tc.resolve_expr(child_id, expected)
			if tc.receiver_compatible(actual, expected) {
				tc.register_synth_type(id, expected_raw)
				return expected_raw
			}
		}
	}
	if node.kind == .array_literal {
		mut elem_expected := Type(void_)
		mut expected_is_array := false
		if expected is Array {
			elem_expected = array_elem_type(expected)
			expected_is_array = true
		} else if expected is ArrayFixed {
			elem_expected = fixed_array_elem_type(expected)
			expected_is_array = true
		}
		if expected_is_array {
			// Empty literal `[]` simply adopts the expected array type.
			if node.children_count == 0 {
				tc.register_synth_type(id, expected_raw)
				return expected_raw
			}
			// Non-empty literal: propagate the expected element type down to each
			// element so context-dependent elements (enum shorthand `[.foo]`, `none`,
			// fn literals) type against it instead of defaulting to a fixed `int[N]`.
			// Adopt the expected array type when every element fits.
			mut all_ok := true
			for i in 0 .. node.children_count {
				child_id := tc.a.child(&node, i)
				elem_actual := tc.resolve_expr(child_id, elem_expected)
				if !tc.receiver_compatible(elem_actual, elem_expected) {
					all_ok = false
					break
				}
			}
			// A fixed-array expectation additionally requires the literal to have
			// exactly the expected number of elements; otherwise `[1, 2]` would be
			// accepted as e.g. `[4]int` and the C backend would copy/read past the
			// compound literal. Element-type propagation above still happens either
			// way; only the type adoption is gated. Unresolvable const lengths stay
			// lenient (we cannot verify them here).
			if all_ok && expected is ArrayFixed {
				if expected_len := tc.fixed_array_len_value(expected) {
					if node.children_count != expected_len {
						all_ok = false
					}
				}
			}
			if all_ok {
				tc.register_synth_type(id, expected_raw)
				return expected_raw
			}
		}
	}
	if node.kind == .match_stmt || node.kind == .if_expr {
		// Match/if used as a value expression: propagate the expected type into
		// each branch tail so enum-shorthand / `none` / fn-literal tails type
		// against it (e.g. `return match s { 'a' { .foo } ... }` with an enum
		// return type), then adopt the expected type when every branch fits.
		if expected !is Void && expected !is Unknown && tc.branches_compatible_with(id, expected) {
			tc.register_synth_type(id, expected_raw)
			return expected_raw
		}
	}
	if _ := fn_type_from_type(expected) {
		if _ := tc.resolve_fn_value_name_for_expected(id, expected_raw) {
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
	if tc.expr_is_unsafe_nil(id) {
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

// fn_value_match_key returns the single exact function declaration key accepted for a function value.
fn (tc &TypeChecker) fn_value_match_key(node flat.Node, expected Type) ?string {
	expected_fn := fn_type_from_type(expected) or { return none }
	key := tc.fn_value_key(node) or { return none }
	actual := tc.fn_type_from_key(key) or { return none }
	if actual is FnType {
		if actual.params.len != expected_fn.params.len {
			return none
		}
		for i in 0 .. actual.params.len {
			actual_param := fn_param_type(actual, i)
			expected_param := fn_param_type(expected_fn, i)
			if !tc.fn_param_compatible(actual_param, expected_param) {
				return none
			}
		}
		if tc.fn_return_compatible(actual.return_type, expected_fn.return_type) {
			return key
		}
	}
	return none
}

// fn_value_key resolves a function value expression to one exact function declaration key.
fn (tc &TypeChecker) fn_value_key(node flat.Node) ?string {
	if node.kind == .ident {
		return tc.ident_fn_value_key(node.value)
	}
	if node.kind == .selector {
		return tc.selector_fn_value_key(node)
	}
	if node.kind in [.cast_expr, .paren, .expr_stmt] && node.children_count > 0 {
		return tc.fn_value_key(tc.a.child_node(&node, 0))
	}
	return none
}

fn (tc &TypeChecker) ident_fn_value_key(name string) ?string {
	qfn := tc.qualify_fn_name(name)
	if tc.fn_signature_known(qfn) {
		return qfn
	}
	if imported_name := tc.resolve_selective_import_symbol(name) {
		return imported_name
	}
	if tc.fn_signature_known(name) {
		return name
	}
	return none
}

fn (tc &TypeChecker) selector_fn_value_key(node flat.Node) ?string {
	if node.children_count == 0 || !valid_string_data(node.value) {
		return none
	}
	base := tc.a.child_node(&node, 0)
	if base.kind == .ident {
		if base.value == 'C' {
			key := 'C.${node.value}'
			if tc.fn_signature_known(key) {
				return key
			}
			return none
		}
		if resolved_mod := tc.resolve_import_alias(base.value) {
			key := '${resolved_mod}.${node.value}'
			if tc.fn_signature_known(key) {
				return key
			}
			return none
		}
		qbase := tc.qualify_name(base.value)
		key := '${qbase}.${node.value}'
		if tc.fn_signature_known(key) && (qbase in tc.structs
			|| qbase in tc.enum_names || qbase in tc.sum_types
			|| qbase in tc.interface_names) {
			return key
		}
		return none
	}
	if base.kind == .selector && base.children_count > 0 {
		inner := tc.a.child_node(base, 0)
		if inner.kind == .ident {
			mod_name := tc.resolve_import_alias(inner.value) or { inner.value }
			key := '${mod_name}.${base.value}.${node.value}'
			if tc.fn_signature_known(key) {
				return key
			}
		}
	}
	return none
}

fn (tc &TypeChecker) fn_signature_known(key string) bool {
	return key in tc.fn_ret_types && key in tc.fn_param_types
}

fn (tc &TypeChecker) expr_is_unsafe_nil(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= tc.a.nodes.len {
		return false
	}
	node := tc.a.nodes[int(id)]
	if node.kind != .block {
		return false
	}
	return tc.expr_tail_is_nil(id)
}

fn (tc &TypeChecker) expr_tail_is_nil(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= tc.a.nodes.len {
		return false
	}
	node := tc.a.nodes[int(id)]
	match node.kind {
		.nil_literal {
			return true
		}
		.expr_stmt, .paren {
			if node.children_count == 0 {
				return false
			}
			return tc.expr_tail_is_nil(tc.a.child(&node, 0))
		}
		.block {
			if node.children_count == 0 {
				return false
			}
			return tc.expr_tail_is_nil(tc.a.child(&node, node.children_count - 1))
		}
		else {
			return false
		}
	}
}

// fn_value_type supports fn value type handling for TypeChecker.
fn (tc &TypeChecker) fn_value_type(name string) ?Type {
	qfn := tc.qualify_fn_name(name)
	if qfn in tc.fn_ret_types {
		return tc.fn_type_from_key(qfn)
	}
	if imported_name := tc.resolve_selective_import_symbol(name) {
		return tc.fn_type_from_key(imported_name)
	}
	if name in tc.fn_ret_types {
		return tc.fn_type_from_key(name)
	}
	return none
}

// fn_type_from_key converts fn type from key data for types.
fn (tc &TypeChecker) fn_type_from_key(key string) ?Type {
	params := tc.fn_param_types[key] or { return none }
	ret := tc.fn_ret_types[key] or { return none }
	return Type(FnType{
		params:      params.clone()
		return_type: ret
	})
}

// struct_field_c_abi_fn_ptr_type returns the C ABI function-pointer type for a struct field.
pub fn (tc &TypeChecker) struct_field_c_abi_fn_ptr_type(struct_name string, field_name string) ?string {
	key := struct_field_c_abi_key(struct_name, field_name)
	if typ := tc.struct_field_c_abi_fns[key] {
		return typ
	}
	return none
}

// enum_value_matches supports enum value matches handling for TypeChecker.
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

// enum_has_field converts enum has field data for types.
fn (tc &TypeChecker) enum_has_field(enum_name string, field string) bool {
	fields := tc.enum_fields[enum_name] or { return false }
	return field in fields
}

// resolve_enum_name resolves resolve enum name information for types.
fn (tc &TypeChecker) resolve_enum_name(name string) ?string {
	if name in tc.enum_names {
		return name
	}
	qname := tc.qualify_name(name)
	if qname in tc.enum_names {
		return qname
	}
	if !name.contains('.') {
		if resolved := tc.resolve_selective_import_type_symbol(name) {
			if resolved in tc.enum_names || resolved in tc.flag_enums {
				return resolved
			}
		}
	}
	return none
}

// enum_selector_type supports enum selector type handling for TypeChecker.
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

// type_compatible returns type compatible data for TypeChecker.
fn (tc &TypeChecker) type_compatible(actual Type, expected Type) bool {
	actual_raw := actual
	expected_raw := expected
	if actual.name() == expected.name() {
		return true
	}
	if actual is Unknown || expected is Unknown {
		return true
	}
	if type_contains_unknown(actual) || type_contains_unknown(expected) {
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
	if actual is Array && expected is Array && actual.elem_type is Void {
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
		if is_ierror_type(actual) || tc.type_embeds_error(actual) {
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
				if !tc.fn_param_compatible(actual_param, expected_param) {
					return false
				}
			}
			return tc.fn_return_compatible(actual.return_type, expected.return_type)
		}
	}
	return false
}

fn type_contains_unknown(typ Type) bool {
	if typ is Unknown {
		return true
	}
	if typ is Alias {
		return type_contains_unknown(typ.base_type)
	}
	if typ is Array {
		return type_contains_unknown(typ.elem_type)
	}
	if typ is ArrayFixed {
		return type_contains_unknown(typ.elem_type)
	}
	if typ is Map {
		return type_contains_unknown(typ.key_type) || type_contains_unknown(typ.value_type)
	}
	if typ is Pointer {
		return type_contains_unknown(typ.base_type)
	}
	if typ is OptionType {
		return type_contains_unknown(typ.base_type)
	}
	if typ is ResultType {
		return type_contains_unknown(typ.base_type)
	}
	if typ is FnType {
		for param in typ.params {
			if type_contains_unknown(param) {
				return true
			}
		}
		return type_contains_unknown(typ.return_type)
	}
	if typ is MultiReturn {
		for part in typ.types {
			if type_contains_unknown(part) {
				return true
			}
		}
	}
	return false
}

fn (tc &TypeChecker) fn_param_compatible(actual Type, expected Type) bool {
	if actual is Unknown || expected is Unknown {
		return false
	}
	if tc.c_type(actual) == tc.c_type(expected) {
		return true
	}
	return fn_param_can_cast_userdata_param(actual, expected)
}

fn (tc &TypeChecker) fn_return_compatible(actual Type, expected Type) bool {
	if actual.name() == expected.name() {
		return true
	}
	return fn_return_canonical_type_name(actual) == fn_return_canonical_type_name(expected)
}

fn fn_param_can_cast_userdata_param(actual Type, expected Type) bool {
	return (fn_param_is_voidptr_type(expected) && fn_param_is_nonvoid_pointer_type(actual))
		|| (fn_param_is_nonvoid_pointer_type(expected) && fn_param_is_voidptr_type(actual))
}

fn fn_param_is_voidptr_type(typ Type) bool {
	clean := fn_param_unalias_type(typ)
	if clean is Pointer {
		base := fn_param_unalias_type(clean.base_type)
		return base is Void
	}
	return false
}

fn fn_param_is_nonvoid_pointer_type(typ Type) bool {
	clean := fn_param_unalias_type(typ)
	if clean is Pointer {
		base := fn_param_unalias_type(clean.base_type)
		return base !is Void
	}
	return false
}

fn fn_param_unalias_type(typ Type) Type {
	if typ is Alias {
		return fn_param_unalias_type(typ.base_type)
	}
	return typ
}

fn fn_return_canonical_type_name(typ Type) string {
	if typ is Alias {
		return fn_return_canonical_type_name(typ.base_type)
	}
	return typ.name()
}

// is_ierror_type reports whether is ierror type applies in types.
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

fn (tc &TypeChecker) type_embeds_error(t Type) bool {
	clean := unwrap_pointer(t)
	if clean is Alias {
		return tc.type_embeds_error(clean.base_type)
	}
	if clean is Struct {
		struct_name := clean.name
		if struct_name == 'Error' || struct_name.ends_with('.Error') {
			return true
		}
		return tc.receiver_embeds(clean, Type(Struct{
			name: 'Error'
		}))
	}
	return false
}

// is_runtime_array_type reports whether is runtime array type applies in types.
fn is_runtime_array_type(t Type) bool {
	if t is Alias {
		return is_runtime_array_type(t.base_type)
	}
	if t is Struct {
		return t.name == 'array'
	}
	return false
}

// fixed_array_lengths_compatible supports fixed array lengths compatible handling for TypeChecker.
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

// fixed_array_len_value returns the evaluated fixed-array length when it can be resolved.
pub fn (tc &TypeChecker) fixed_array_len_value(arr ArrayFixed) ?int {
	if arr.len > 0 {
		return arr.len
	}
	if arr.len_expr.len == 0 {
		return none
	}
	return tc.const_int_value(arr.len_expr, []string{})
}

// const_expr_paren_wraps_whole reports whether `s` is a single parenthesised group that
// encloses the entire string (`(a + b)`), as opposed to one that only covers part of it
// (`(a) + (b)`), so a const length wrapped in redundant parentheses can be unwrapped.
fn const_expr_paren_wraps_whole(s string) bool {
	if s.len < 2 || s[0] != `(` || s[s.len - 1] != `)` {
		return false
	}
	mut depth := 0
	for i := 0; i < s.len; i++ {
		if s[i] == `(` {
			depth++
		} else if s[i] == `)` {
			depth--
			if depth == 0 {
				return i == s.len - 1
			}
		}
	}
	return false
}

// const_int_value supports const int value handling for TypeChecker.
pub fn (tc &TypeChecker) const_int_value(name string, seen []string) ?int {
	return tc.const_int_value_in_module(name, tc.cur_module, seen)
}

// const_int_value_in_module supports const int value handling for a specific module.
pub fn (tc &TypeChecker) const_int_value_in_module(name string, module_name string, seen []string) ?int {
	if name in seen {
		return none
	}
	mut candidates := []string{}
	candidates << name
	if module_name.len > 0 && module_name != 'main' && module_name != 'builtin'
		&& !name.contains('.') {
		candidates << '${module_name}.${name}'
	}
	qname := tc.qualify_name(name)
	if qname != name {
		candidates << qname
	}
	if key := tc.const_key_for_suffix(name) {
		candidates << key
	}
	for key in candidates {
		if key in seen {
			continue
		}
		if expr_id := tc.const_exprs[key] {
			mut next_seen := seen.clone()
			next_seen << key
			const_module := tc.const_modules[key] or { module_name }
			return tc.const_int_expr(expr_id, const_module, next_seen)
		}
	}
	if v := v_int_literal_value(name) {
		return v
	}
	// Simple const arithmetic in string form, e.g. a fixed-array size `[SEGS + 1]`,
	// `[SEGS+1]`, `[segs / 2]`, `[segs % 4]` or `[2 * (segs + 1)]`. A length wrapped
	// wholly in parentheses is the inner expression, so strip the outer pair and
	// re-evaluate. Otherwise split on the rightmost operator of the lowest precedence
	// level present (`+ -`, then `* / %`) that sits OUTSIDE any parentheses, so a nested
	// operator (the `+` inside `2 * (segs + 1)`) is not chosen; precedence and left
	// associativity hold and each side is trimmed and resolved recursively. A leading `-`
	// (unary) leaves an empty lhs and is skipped.
	expr := name.trim_space()
	if const_expr_paren_wraps_whole(expr) {
		return tc.const_int_value(expr[1..expr.len - 1].trim_space(), seen)
	}
	// Operators grouped by precedence level, lowest first, MATCHING the v3 parser's binding
	// power (token.left_binding_power) so a length text recovered from source folds to the
	// same value as the AST const evaluator below: `|` < `^` < `<< >> >>>` < `+ -` <
	// `* / % &`. The parser keeps shifts below additive operators (so `arr << a + b` appends
	// `a + b`), hence `1 << 2 + 1` groups as `1 << (2 + 1)` — split on `<<` before `+` here
	// too, not the reverse. Split on the rightmost top-level operator of the lowest level
	// present; longer operators match first (`>>>` before `>>`, two-char before one) and
	// `idx + op.len` skips the operator.
	for level in [['|'], ['^'], ['<<', '>>', '>>>'], ['+', '-'],
		['*', '/', '%', '&']] {
		mut idx := -1
		mut op := ''
		mut depth := 0
		mut i := 0
		for i < expr.len {
			ch := expr[i..i + 1]
			if ch == '(' {
				depth++
				i++
				continue
			}
			if ch == ')' {
				depth--
				i++
				continue
			}
			if depth == 0 {
				three := if i + 3 <= expr.len { expr[i..i + 3] } else { '' }
				if three.len == 3 && three in level {
					idx = i
					op = three
					i += 3
					continue
				}
				two := if i + 2 <= expr.len { expr[i..i + 2] } else { '' }
				if two.len == 2 && two in level {
					idx = i
					op = two
					i += 2
					continue
				}
				if ch in level {
					idx = i
					op = ch
				}
			}
			i++
		}
		if idx <= 0 {
			continue
		}
		lhs := expr[..idx].trim_space()
		rhs := expr[idx + op.len..].trim_space()
		if lhs.len == 0 || rhs.len == 0 {
			continue
		}
		l := tc.const_int_value_in_module(lhs, module_name, seen) or { return none }
		r := tc.const_int_value_in_module(rhs, module_name, seen) or { return none }
		if (op == '/' || op == '%') && r == 0 {
			return none
		}
		if (op == '<<' || op == '>>' || op == '>>>') && (r < 0 || r >= 64) {
			return none
		}
		return match op {
			'+' { l + r }
			'-' { l - r }
			'*' { l * r }
			'/' { l / r }
			'%' { l % r }
			'|' { l | r }
			'^' { l ^ r }
			'&' { l & r }
			'<<' { l << r }
			'>>' { l >> r }
			else { int(u64(l) >> r) }
		}
	}
	return none
}

// const_int_expr supports const int expr handling for TypeChecker.
fn (tc &TypeChecker) const_int_expr(id flat.NodeId, module_name string, seen []string) ?int {
	if int(id) < 0 {
		return none
	}
	node := tc.a.nodes[int(id)]
	match node.kind {
		.int_literal {
			if v := v_int_literal_value(node.value) {
				return v
			}
		}
		.ident {
			return tc.const_int_value_in_module(node.value, module_name, seen)
		}
		.paren {
			if node.children_count > 0 {
				return tc.const_int_expr(tc.a.child(&node, 0), module_name, seen)
			}
		}
		.prefix {
			if node.children_count == 0 {
				return none
			}
			value := tc.const_int_expr(tc.a.child(&node, 0), module_name, seen) or { return none }
			return match node.op {
				.minus { -value }
				.plus { value }
				.bit_not { ~value }
				else { none }
			}
		}
		.infix {
			if node.children_count < 2 {
				return none
			}
			left := tc.const_int_expr(tc.a.child(&node, 0), module_name, seen) or { return none }
			right := tc.const_int_expr(tc.a.child(&node, 1), module_name, seen) or { return none }
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
				.amp {
					return left & right
				}
				.pipe {
					return left | right
				}
				.xor {
					return left ^ right
				}
				.left_shift {
					if right < 0 || right >= 64 {
						return none
					}
					return left << right
				}
				.right_shift {
					if right < 0 || right >= 64 {
						return none
					}
					return left >> right
				}
				.right_shift_unsigned {
					if right < 0 || right >= 64 {
						return none
					}
					return int(u64(left) >> right)
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

// type_implements_interface returns type implements interface data for TypeChecker.
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

// interface_implements_interface supports interface implements interface handling for TypeChecker.
fn (tc &TypeChecker) interface_implements_interface(actual_name string, expected_name string) bool {
	if actual_name == expected_name {
		return true
	}
	for method in tc.interface_method_names(expected_name) {
		actual_key := tc.interface_method_signature_key(actual_name, method) or {
			'${actual_name}.${method}'
		}
		expected_key := tc.interface_method_signature_key(expected_name, method) or {
			'${expected_name}.${method}'
		}
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

// named_type_implements_interface
// supports helper handling in types.
pub fn (tc &TypeChecker) named_type_implements_interface(concrete_name string, iface_name string) bool {
	// Only the abstract (declared) methods must be provided by the concrete type.
	// Methods defined directly on the interface (default implementations) are
	// inherited and need not be reimplemented.
	for method in tc.interface_abstract_method_names(iface_name) {
		concrete_key := '${concrete_name}.${method}'
		if concrete_key !in tc.fn_param_types {
			return false
		}
		expected_key := tc.interface_method_signature_key(iface_name, method) or {
			'${iface_name}.${method}'
		}
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

// interface_method_names supports interface method names handling for TypeChecker.
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

// interface_abstract_method_names_inner supports interface_abstract_method_names_inner handling.
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

// interface_method_names_inner supports interface method names inner handling for TypeChecker.
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

pub fn (tc &TypeChecker) interface_method_signature_key(iface_name string, method string) ?string {
	key := '${iface_name}.${method}'
	if key in tc.fn_ret_types || key in tc.fn_param_types {
		return key
	}
	for embed in tc.interface_embeds[iface_name] or { []string{} } {
		if found := tc.interface_method_signature_key(embed, method) {
			return found
		}
	}
	return none
}

// interface_field_list supports interface field list handling for TypeChecker.
fn (tc &TypeChecker) interface_field_list(iface_name string) []StructField {
	mut seen := map[string]bool{}
	return tc.interface_field_list_inner(iface_name, mut seen)
}

// interface_field_list_inner supports interface field list inner handling for TypeChecker.
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

// interface_field_type supports interface field type handling for TypeChecker.
fn (tc &TypeChecker) interface_field_type(iface_name string, field_name string) ?Type {
	for field in tc.interface_field_list(iface_name) {
		if field.name == field_name {
			return field.typ
		}
	}
	return none
}

// struct_field_type supports struct field type handling for TypeChecker.
fn (tc &TypeChecker) struct_field_type(struct_name string, field_name string) ?Type {
	mut seen := map[string]bool{}
	return tc.struct_field_type_inner(struct_name, field_name, mut seen)
}

fn (tc &TypeChecker) struct_field_type_inner(struct_name string, field_name string, mut seen map[string]bool) ?Type {
	base_name, generic_args, is_generic := generic_type_application_parts(struct_name)
	lookup_name := if is_generic { base_name } else { struct_name }
	if lookup_name in seen {
		return none
	}
	seen[lookup_name] = true
	for field in tc.structs[lookup_name] or { []StructField{} } {
		if field.name == field_name {
			if is_generic {
				return tc.substitute_generic_type(field.typ, generic_args, tc.struct_generic_params[base_name] or {
					[]string{}
				})
			}
			return field.typ
		}
		mut embedded_type := embedded_field_type(field) or { continue }
		embedded_type = if is_generic {
			tc.substitute_generic_type(embedded_type, generic_args, tc.struct_generic_params[base_name] or {
				[]string{}
			})
		} else {
			embedded_type
		}
		embedded_name := method_type_name(unwrap_pointer(embedded_type))
		if embedded_name.len == 0 {
			continue
		}
		if typ := tc.struct_field_type_inner(embedded_name, field_name, mut seen) {
			return typ
		}
	}
	return none
}

// substitute_generic_type replaces generic placeholders in `typ` with the concrete
// `args`. When `param_names` (the struct/fn's declared type parameters, e.g.
// `['L', 'R']`) is provided, a placeholder is matched to its arg by its declared
// position — so `Pair[L, R]`'s `R` resolves to `args[1]`, not the letter-based
// `generic_param_index` guess (which maps any unrecognised name to 0). The
// letter-based fallback is kept only for callers that have no declared names.
fn (tc &TypeChecker) substitute_generic_type(typ Type, args []string, param_names []string) Type {
	if args.len == 0 {
		return typ
	}
	if typ is Unknown {
		if name := generic_placeholder_from_unknown(typ) {
			mut idx := if param_names.len > 0 { param_names.index(name) } else { -1 }
			if idx < 0 {
				idx = generic_param_index(name)
			}
			if idx >= 0 && idx < args.len {
				return tc.parse_type(args[idx].trim_space())
			}
		}
		return typ
	}
	if typ is Array {
		return Type(Array{
			elem_type: tc.substitute_generic_type(typ.elem_type, args, param_names)
		})
	}
	if typ is ArrayFixed {
		return Type(ArrayFixed{
			elem_type: tc.substitute_generic_type(typ.elem_type, args, param_names)
			len:       typ.len
			len_expr:  typ.len_expr
		})
	}
	if typ is Map {
		return Type(Map{
			key_type:   tc.substitute_generic_type(typ.key_type, args, param_names)
			value_type: tc.substitute_generic_type(typ.value_type, args, param_names)
		})
	}
	if typ is Pointer {
		return Type(Pointer{
			base_type: tc.substitute_generic_type(typ.base_type, args, param_names)
		})
	}
	if typ is OptionType {
		return Type(OptionType{
			base_type: tc.substitute_generic_type(typ.base_type, args, param_names)
		})
	}
	if typ is ResultType {
		return Type(ResultType{
			base_type: tc.substitute_generic_type(typ.base_type, args, param_names)
		})
	}
	if typ is FnType {
		mut params := []Type{}
		for param in typ.params {
			params << tc.substitute_generic_type(param, args, param_names)
		}
		return Type(FnType{
			params:      params
			return_type: tc.substitute_generic_type(typ.return_type, args, param_names)
		})
	}
	if typ is MultiReturn {
		mut parts := []Type{}
		for part in typ.types {
			parts << tc.substitute_generic_type(part, args, param_names)
		}
		return Type(MultiReturn{
			types: parts
		})
	}
	return typ
}

fn (tc &TypeChecker) embedded_method_call_info(struct_name string, method string) ?CallInfo {
	mut seen := map[string]bool{}
	return tc.embedded_method_call_info_inner(struct_name, method, mut seen)
}

fn (tc &TypeChecker) embedded_method_call_info_inner(struct_name string, method string, mut seen map[string]bool) ?CallInfo {
	if seen[struct_name] {
		return none
	}
	seen[struct_name] = true
	for field in tc.structs[struct_name] or { []StructField{} } {
		embedded_type := embedded_field_type(field) or { continue }
		receiver := method_type_name(unwrap_pointer(embedded_type))
		if receiver.len == 0 {
			continue
		}
		mut method_names := ['${receiver}.${method}']
		base_name, _, is_generic := generic_type_application_parts(receiver)
		if is_generic {
			method_names << '${base_name}.${method}'
		}
		for method_name in method_names {
			if method_name in tc.fn_ret_types {
				info := tc.call_info(method_name, true)
				return CallInfo{
					name:          info.name
					params:        info.params
					return_type:   info.return_type
					has_receiver:  info.has_receiver
					is_variadic:   info.is_variadic
					is_c_variadic: info.is_c_variadic
					params_known:  if receiver.contains('[') { false } else { info.params_known }
				}
			}
		}
		if info := tc.embedded_method_call_info_inner(receiver, method, mut seen) {
			return info
		}
	}
	return none
}

fn (tc &TypeChecker) struct_has_middleware_receiver(struct_name string) bool {
	if is_middleware_type_name(struct_name) {
		return true
	}
	for field in tc.structs[struct_name] or { []StructField{} } {
		embedded_type := embedded_field_type(field) or { continue }
		embedded_name := method_type_name(unwrap_pointer(embedded_type))
		if is_middleware_type_name(embedded_name) {
			return true
		}
	}
	return false
}

fn is_middleware_type_name(name string) bool {
	base := if name.contains('[') { name.all_before('[') } else { name }
	return base == 'veb.Middleware'
}

fn (tc &TypeChecker) receiver_embeds(actual Type, expected Type) bool {
	actual_name := method_type_name(unwrap_pointer(actual))
	expected_name := method_type_name(unwrap_pointer(expected))
	if actual_name.len == 0 || expected_name.len == 0 {
		return false
	}
	mut seen := map[string]bool{}
	return tc.receiver_embeds_inner(actual_name, expected_name, mut seen)
}

fn (tc &TypeChecker) receiver_embeds_inner(actual_name string, expected_name string, mut seen map[string]bool) bool {
	if seen[actual_name] {
		return false
	}
	seen[actual_name] = true
	for field in tc.structs[actual_name] or { []StructField{} } {
		embedded_type := embedded_field_type(field) or { continue }
		embedded_name := method_type_name(unwrap_pointer(embedded_type))
		if embedded_name == expected_name {
			return true
		}
		if tc.receiver_embeds_inner(embedded_name, expected_name, mut seen) {
			return true
		}
	}
	return false
}

fn embedded_field_type(field StructField) ?Type {
	field_type_name := method_type_name(unwrap_pointer(field.typ))
	if field_type_name.len == 0 {
		return none
	}
	mut names := [field_type_name]
	base_name, _, is_generic := generic_type_application_parts(field_type_name)
	if is_generic {
		names << base_name
	}
	for name in names {
		short_name := if name.contains('.') { name.all_after_last('.') } else { name }
		if field.name == name || field.name == short_name {
			return field.typ
		}
	}
	return none
}

// method_signature_compatible supports method signature compatible handling for TypeChecker.
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

// method_type_name supports method type name handling for types.
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
		return prim_name(t)
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

// type_matches_sum returns type matches sum data for TypeChecker.
fn (tc &TypeChecker) type_matches_sum(actual Type, expected Type) bool {
	if expected is SumType {
		actual_name := actual.name()
		return tc.sum_has_variant(expected.name, actual_name)
	}
	return false
}

// sum_has_variant converts sum has variant data for types.
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

// match_type_pattern supports match type pattern handling for TypeChecker.
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

// short_type_name supports short type name handling for types.
fn short_type_name(name string) string {
	if name.contains('.') {
		return name.all_after_last('.')
	}
	return name
}

// type_mismatch returns type mismatch data for TypeChecker.
fn (mut tc TypeChecker) type_mismatch(kind TypeErrorKind, msg string, node flat.NodeId) {
	if tc.should_diagnose(node) {
		tc.record_error(kind, msg, node)
	}
}

// expr_key supports expr key handling for TypeChecker.
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

// smartcast_type supports smartcast type handling for TypeChecker.
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
		key := tc.cur_file + '\n' + tc.cur_module + '\n' + typ
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

// parse_type_uncached reads parse type uncached input for types.
fn (tc &TypeChecker) parse_type_uncached(typ string) Type {
	if typ.len == 0 {
		return Type(void_)
	}
	if is_generic_placeholder_type(typ) && !tc.is_known_type_text(typ) {
		return unknown_type('generic placeholder `${typ}`')
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
	if typ.starts_with('thread ') || typ == 'thread' {
		// A thread handle. The element type (the spawned fn's return type) is kept
		// in the struct name (`thread T`) so `array_of_threads.wait()` can recover
		// `[]T`. The handle itself lowers to `void*` in C (see c_type).
		return Type(Struct{
			name: typ
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
		if resolved := tc.resolve_selective_import_type_symbol(typ) {
			if resolved_type := tc.type_from_known_symbol(resolved) {
				return resolved_type
			}
		}
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
		generic_suffix := typ[bracket..]
		_, generic_args, _ := generic_type_application_parts(typ)
		is_concrete_generic := tc.generic_args_are_concrete(generic_args)
		qbase := tc.qualify_name(base)
		if qbase in tc.type_aliases {
			return Type(Alias{
				name:      qbase
				base_type: tc.parse_type(tc.type_aliases[qbase])
			})
		}
		if qbase in tc.structs {
			return Type(Struct{
				name: if is_concrete_generic { qbase + generic_suffix } else { qbase }
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
		if !base.contains('.') {
			if resolved := tc.resolve_selective_import_type_symbol(base) {
				if resolved in tc.type_aliases {
					return Type(Alias{
						name:      resolved
						base_type: tc.parse_type(tc.type_aliases[resolved])
					})
				}
				if resolved in tc.structs {
					return Type(Struct{
						name: if is_concrete_generic { resolved + generic_suffix } else { resolved }
					})
				}
				if resolved in tc.interface_names {
					return Type(Interface{
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
		if base in tc.type_aliases {
			return Type(Alias{
				name:      base
				base_type: tc.parse_type(tc.type_aliases[base])
			})
		}
		if base in tc.structs {
			return Type(Struct{
				name: if is_concrete_generic { typ } else { base }
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
		if is_concrete_generic && !is_builtin_type_name(base) {
			// A concrete generic instance (`Vec4[f32]`) is a monomorphized struct, even
			// when the generic base decl has been erased after monomorphization. It is
			// never a fixed array, so don't fall through to the `[N]T` handler below.
			// Qualify an imported base (`Vec4` -> `vec.Vec4`) so its c_type matches the
			// materialized struct (`vec__Vec4_f32`) everywhere it appears. A builtin base
			// (`int[seg_count]`) cannot be a generic application, so let it fall through to
			// the fixed-array handler — its bracket is a const/expression length.
			mut full := typ
			if !base.contains('.') {
				if resolved := tc.unique_qualified_type_name(base) {
					full = resolved + generic_suffix
				}
			}
			return Type(Struct{
				name: full
			})
		}
	}
	if is_generic_placeholder_type(typ) {
		return unknown_type('generic type parameter `${typ}`')
	}
	if typ.contains('[') && !typ.starts_with('[') {
		// Postfix fixed-array name (`ArrayFixed.name()`): the element comes first and
		// each dimension is appended, so the OUTERMOST dimension is the trailing `[N]`
		// (`int[3][2]` is `[2][3]int`). Split on the last bracket pair so a nested fixed
		// array recovers the outer length and recurses into the inner element, instead of
		// taking the first `[N]` and dropping the rest. For a single dimension the last
		// and first brackets coincide, so this matches the previous behaviour.
		bracket := typ.last_index_u8(`[`)
		bracket_end := typ.last_index_u8(`]`)
		if bracket >= 0 && bracket_end > bracket {
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

fn (tc &TypeChecker) is_known_type_text(typ string) bool {
	qtyp := tc.qualify_name(typ)
	if !typ.contains('.') {
		if resolved := tc.resolve_selective_import_type_symbol(typ) {
			return tc.type_symbol_known(resolved)
		}
	}
	return typ in tc.structs || qtyp in tc.structs || typ in tc.interface_names
		|| qtyp in tc.interface_names || typ in tc.enum_names || qtyp in tc.enum_names
		|| typ in tc.sum_types || qtyp in tc.sum_types || typ in tc.type_aliases
		|| qtyp in tc.type_aliases
}

// unique_qualified_type_name supports unique qualified type name handling for TypeChecker.
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

// is_generic_placeholder_type reports whether is generic placeholder type applies in types.
fn is_generic_placeholder_type(typ string) bool {
	if typ.contains('.') {
		last := typ.all_after_last('.')
		return is_generic_placeholder_type(last)
	}
	return is_bare_generic_param(typ)
}

// parse_fn_type reads parse fn type input for types.
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

fn (tc &TypeChecker) c_abi_fn_ptr_type_from_text(typ string) ?string {
	clean := typ.trim_space()
	if !clean.starts_with('fn(') && !clean.starts_with('fn (') {
		return none
	}
	params_start := clean.index_u8(`(`) + 1
	mut depth := 1
	mut params_end := params_start
	for params_end < clean.len {
		if clean[params_end] == `(` {
			depth++
		} else if clean[params_end] == `)` {
			depth--
			if depth == 0 {
				break
			}
		}
		params_end++
	}
	if params_end >= clean.len {
		return none
	}
	params_str := clean[params_start..params_end]
	ret_str := clean[params_end + 1..].trim_left(' ')
	mut params := []string{}
	mut has_c_abi_param := false
	if params_str.trim_space().len > 0 {
		for part in split_params(params_str) {
			ct, is_c_abi := tc.c_abi_fn_param_type(part)
			params << ct
			if is_c_abi {
				has_c_abi_param = true
			}
		}
	}
	if !has_c_abi_param {
		return none
	}
	ret_type := if ret_str.len > 0 { tc.parse_type(ret_str) } else { Type(Void{}) }
	ret_ct := tc.fn_ptr_return_c_type(ret_type)
	params_ct := if params.len == 0 { 'void' } else { params.join(', ') }
	return 'fn_ptr:${ret_ct}|${params_ct}'
}

fn (tc &TypeChecker) c_abi_fn_ptr_type_for_type_text(typ string) ?string {
	mut seen := map[string]bool{}
	return tc.c_abi_fn_ptr_type_for_type_text_inner(typ.trim_space(), mut seen)
}

fn (tc &TypeChecker) c_abi_fn_ptr_type_for_type_text_inner(typ string, mut seen map[string]bool) ?string {
	if typ.len == 0 || seen[typ] {
		return none
	}
	seen[typ] = true
	if c_abi_fn := tc.c_abi_fn_ptr_type_from_text(typ) {
		return c_abi_fn
	}
	for name in [tc.qualify_name(typ), typ] {
		if name.len == 0 {
			continue
		}
		if c_abi_fn := tc.type_alias_c_abi_fns[name] {
			return c_abi_fn
		}
		if target := tc.type_aliases[name] {
			if c_abi_fn := tc.c_abi_fn_ptr_type_for_type_text_inner(target, mut seen) {
				return c_abi_fn
			}
		}
	}
	return none
}

fn (tc &TypeChecker) c_abi_fn_param_type(param string) (string, bool) {
	clean := param.trim_space()
	param_type := normalize_fn_type_param_text(clean)
	if c_abi_fn_param_name(clean).starts_with('const_') && param_type.starts_with('&') {
		base_type := tc.parse_type(param_type[1..])
		return 'const ${tc.c_type(base_type)}*', true
	}
	if param_type.starts_with('&') {
		if ct := tc.c_abi_alias_pointer_param_c_type(param_type[1..]) {
			if clean.starts_with('mut ') {
				return '${ct}*', true
			}
			return 'const ${ct}*', true
		}
	}
	return tc.c_type(tc.parse_type(param_type)), false
}

fn (tc &TypeChecker) c_abi_alias_pointer_param_c_type(typ string) ?string {
	t := tc.parse_type(typ)
	if t is Alias {
		base := c_abi_alias_c_base_type(t.base_type) or { return none }
		return tc.c_type(base)
	}
	return none
}

fn c_abi_alias_c_base_type(t Type) ?Type {
	if t is Alias {
		return c_abi_alias_c_base_type(t.base_type)
	}
	if t is Struct && t.name.starts_with('C.') {
		return t
	}
	return none
}

fn c_abi_fn_param_name(param string) string {
	mut text := param.trim_space()
	if text.starts_with('mut ') {
		text = text[4..].trim_space()
	}
	space := top_level_space_index(text)
	if space <= 0 {
		return ''
	}
	head := text[..space].trim_space()
	tail := text[space + 1..].trim_space()
	if fn_type_param_head_is_name(head, tail) {
		return head
	}
	return ''
}

fn struct_field_c_abi_key(struct_name string, field_name string) string {
	return '${struct_name}\n${field_name}'
}

// resolve_type resolves resolve type information for types.
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
	if kind_id == 12 && node.typ.len > 0 && node.typ !in ['int', 'array', 'map', 'unknown'] {
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
	if node.kind == .selector {
		if typ := tc.const_type_for_selector(node) {
			return typ
		}
	}
	if node.typ.len > 0 && node.typ != 'unknown' && !(kind_id == 12
		&& node.typ in ['int', 'array', 'map']) {
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
			return Type(rune_)
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
			if is_bare_generic_param(node.value) {
				return unknown_type('generic placeholder `${node.value}`')
			}
			if smart_type := tc.smartcast_type(id) {
				return smart_type
			}
			if typ := tc.cur_scope.lookup(node.value) {
				return typ
			}
			if typ := tc.file_scope.lookup(node.value) {
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
			if tc.selective_import_symbol_is_ambiguous(node.value) {
				return unknown_type('ambiguous selective import `${node.value}`')
			}
			if node.value == 'err' {
				return tc.parse_type('IError')
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
					if fn_node.value == 'map' {
						return Type(Array{
							elem_type: Type(Unknown{
								reason: 'array.map'
							})
						})
					}
					if fn_node.value == 'wait' {
						// `[]thread T`.wait() joins all threads and returns `[]T`. A bare
						// `[]thread` (threads with no return value) joins to `void`. Any
						// other array element type is not a thread and `.wait()` is
						// unsupported, so reject it rather than mis-typing the call as the
						// receiver array (which would emit invalid C joining non-handles).
						elem := array_elem_type(clean_type)
						if elem is Struct {
							if elem.name == 'thread' {
								return Type(void_)
							}
							if elem.name.starts_with('thread ') {
								return Type(Array{
									elem_type: tc.parse_type(elem.name[7..])
								})
							}
						}
						return unknown_type('`.wait()` requires an array of threads')
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
					array_mname := 'array.${fn_node.value}'
					if array_mname in tc.fn_ret_types {
						return tc.fn_ret_types[array_mname] or {
							unknown_type('unknown return type for `${array_mname}`')
						}
					}
					return unknown_type('unknown array method `${fn_node.value}`')
				}
				if clean_type is Map {
					if fn_node.value == 'clone' {
						return base_type
					}
					if fn_node.value in ['delete', 'clear', 'free'] {
						return Type(void_)
					}
					if fn_node.value == 'keys' {
						return Type(Array{
							elem_type: clean_type.key_type
						})
					}
					if fn_node.value == 'values' {
						return Type(Array{
							elem_type: clean_type.value_type
						})
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
				if fn_node.value == 'str'
					&& (clean_type is Primitive || clean_type is Char || clean_type is Rune) {
					return Type(string_)
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
						for base_mname in receiver_method_name_candidates(clean_type.base_type,
							fn_node.value, tc.cur_module) {
							if base_mname in tc.fn_ret_types {
								return tc.fn_ret_types[base_mname] or {
									unknown_type('unknown return type for `${base_mname}`')
								}
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
					// A method on a concrete generic instance (`Box[int].clone`) is
					// registered under the open form (`Box[T].clone`); resolve it so the
					// call types as the substituted return (`Box[int]`) rather than the
					// bare base the collapsed open signature would yield.
					if ci := tc.resolve_generic_struct_method(clean_type.name, fn_node.value) {
						return ci.return_type
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
			rt := tc.resolve_type(tc.a.child(&node, 1))
			rt_raw := rt
			if operator_ret := tc.infix_operator_return_type(node.op, lt, rt) {
				return operator_ret
			}
			if lt is String {
				return lt_raw
			}
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
		.offsetof_expr {
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
				if base_node.value == 'os' && node.value == 'args' {
					return Type(Array{
						elem_type: Type(String{})
					})
				}
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
				if typ := tc.struct_field_type(clean.name, node.value) {
					return typ
				}
				if typ := tc.method_value_type(clean.name, node.value) {
					return typ
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
				return Type(Array{
					elem_type: elem_type
				})
			}
			return Type(Array{
				elem_type: Type(int_)
			})
		}
		.postfix {
			if node.children_count == 0 {
				return unknown_type('missing postfix expression')
			}
			child_id := tc.a.child(&node, 0)
			child := tc.a.nodes[int(child_id)]
			if node.op == .not && child.kind == .array_literal {
				elem_type := if child.children_count > 0 {
					tc.resolve_type(tc.a.child(&child, 0))
				} else {
					Type(int_)
				}
				return Type(ArrayFixed{
					elem_type: elem_type
					len:       child.children_count
				})
			}
			return tc.resolve_type(child_id)
		}
		.index {
			return tc.resolve_index_type(node)
		}
		.array_init {
			if node.typ.len > 0 {
				return tc.parse_type(node.typ)
			}
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

// fn_literal_type supports fn literal type handling for TypeChecker.
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

// lambda_expr_type supports lambda expr type handling for TypeChecker.
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

// resolve_index_type resolves resolve index type information for types.
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
		if base_type is ArrayFixed {
			return Type(Array{
				elem_type: fixed_array_elem_type(base_type)
			})
		}
		if base_type is Pointer {
			inner0 := pointer_base_type(base_type)
			mut inner := inner0
			if inner0 is Alias {
				inner = inner0.base_type
			}
			if inner is ArrayFixed {
				return Type(Array{
					elem_type: fixed_array_elem_type(inner)
				})
			}
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
		if inner is Map {
			// `mut m map[K]V` params resolve to a pointer-to-map; indexing still
			// yields the value type V, not the whole map.
			return map_value_type(inner)
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

// c_type supports c type handling for TypeChecker.
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

// c_type_uncached supports c type uncached handling for TypeChecker.
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
		return prim_c_type(t)
	}
	if t is Array {
		return 'Array'
	}
	if t is ArrayFixed {
		// The typedef name preserves the source length text while
		// the emitted C dimension is folded separately by fixed_array_len_value.
		len_text := if t.len_expr.len > 0 { t.len_expr } else { t.len.str() }
		len_name := c_type_name_part(len_text)
		return 'Array_fixed_${c_type_name_part(tc.c_type(t.elem_type))}_${len_name}'
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
		ret := tc.fn_ptr_return_c_type(t.return_type)
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
		if t.name == 'thread' || t.name.ends_with('.thread') || t.name.starts_with('thread ') {
			return 'void*'
		}
		if t.name.starts_with('C.') {
			raw := t.name[2..]
			if raw.ends_with('_s')
				|| (raw.len > 0 && raw[0] >= `a` && raw[0] <= `z` && !raw.ends_with('_t')) {
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
		// Follow the alias chain iteratively. A self-referential / cyclic alias (whose
		// base resolves back to itself) would otherwise recurse forever here — the cache
		// is only populated after the recursive call returns — overflowing the stack.
		mut cur := Type(t)
		for _ in 0 .. 1000 {
			if cur is Alias {
				cur = cur.base_type
			} else {
				return tc.c_type(cur)
			}
		}
		return 'void*'
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

fn (tc &TypeChecker) fn_ptr_return_c_type(t Type) string {
	if t is Void {
		return 'void'
	}
	if t is OptionType {
		return tc.optional_c_type_name(t.base_type)
	}
	if t is ResultType {
		return tc.optional_c_type_name(t.base_type)
	}
	return tc.c_type(t)
}

fn (tc &TypeChecker) optional_c_type_name(base_type Type) string {
	if base_type is Void {
		return 'Optional'
	}
	inner_ct := tc.c_type(base_type)
	if inner_ct == 'int' {
		return 'Optional'
	}
	return 'Optional_${inner_ct.replace('*', 'ptr').replace(' ', '_')}'
}

// c_type_name_part turns a C type or length expression into a fragment that is safe
// to embed inside a C identifier: `*` becomes `ptr` (so pointer payloads stay
// distinguishable), and every other character that is not a letter, digit, or `_`
// becomes `_`. This keeps const-expression fixed-array lengths (e.g. `segs + 1`) and
// pointer return types (`Foo*`) from producing invalid identifiers such as
// `Array_fixed_f32_segs_+_1` or `__v_thread_arr_wait_Foo*`.
pub fn c_type_name_part(s string) string {
	mut b := []u8{cap: s.len + 2}
	for i := 0; i < s.len; i++ {
		c := s[i]
		if c == `*` {
			b << `p`
			b << `t`
			b << `r`
		} else if (c >= `a` && c <= `z`) || (c >= `A` && c <= `Z`)
			|| (c >= `0` && c <= `9`) || c == `_` {
			b << c
		} else {
			b << `_`
		}
	}
	return b.bytestr()
}

// resolve_type_name_for_method resolves resolve type name for method information for types.
// resolve_generic_struct_method resolves a method call on a generic-struct
// instance (e.g. `Vec4[f32].r_sqrt`). The method is registered against the
// generic form (`Vec4[T].r_sqrt`); this maps the instance's concrete type
// arguments onto the generic parameters and substitutes them into the method's
// signature, so the pre-transform checker accepts the call. The transformer's
// monomorphize pass later materialises the concrete method body.
pub fn (tc &TypeChecker) resolve_generic_struct_method(type_name string, method string) ?CallInfo {
	bracket := type_name.index_u8(`[`)
	if bracket <= 0 || !type_name.ends_with(']') {
		return none
	}
	base := type_name[..bracket]
	args_str := type_name[bracket + 1..type_name.len - 1]
	mut concrete_args := []string{}
	for a in split_generic_arg_list(args_str) {
		concrete_args << a.trim_space()
	}
	params := tc.struct_generic_params[base] or { return none }
	if params.len == 0 || params.len != concrete_args.len {
		return none
	}
	generic_key := '${base}[${params.join(', ')}].${method}'
	ret := tc.fn_ret_types[generic_key] or { return none }
	// Prefer substituting the original signature TEXT: parsing `Box[T]` collapses the
	// non-concrete application to the bare `Box`, so substituting the already-parsed type
	// cannot recover `Box[int]`. Re-substituting the text and re-parsing does.
	mut sub_ret := tc.substitute_generic_type(ret, concrete_args, params)
	if ret_text := tc.fn_ret_type_texts[generic_key] {
		sub_ret = tc.parse_fn_signature_type(generic_key, subst_generic_text(ret_text,
			concrete_args, params))
	}
	mut sub_params := []Type{}
	if param_texts := tc.fn_param_type_texts[generic_key] {
		for pt in param_texts {
			sub_params << tc.parse_fn_signature_type(generic_key, subst_generic_text(pt,
				concrete_args, params))
		}
	} else if ptypes := tc.fn_param_types[generic_key] {
		for pt in ptypes {
			sub_params << tc.substitute_generic_type(pt, concrete_args, params)
		}
	}
	return CallInfo{
		name:         generic_key
		params:       sub_params
		return_type:  sub_ret
		has_receiver: true
		is_variadic:  tc.fn_variadic[generic_key] or { false }
		params_known: true
	}
}

// subst_generic_text textually substitutes the generic parameter names `params` with the
// concrete argument texts `args` inside a type text, preserving the generic application
// form so a method signature mentioning the receiver type (`Box[T]`) becomes the concrete
// instance (`Box[int]`) when re-parsed, instead of collapsing to the bare base. Prefix and
// container forms (`&`, `mut`, `?`, `!`, `...`, `[]`, `map[`, `[N]`) recurse into the
// element type; a bare parameter name is replaced with its argument.
fn subst_generic_text(typ string, args []string, params []string) string {
	clean := typ.trim_space()
	if clean.len == 0 || args.len == 0 || params.len != args.len {
		return clean
	}
	if clean.starts_with('&') {
		return '&' + subst_generic_text(clean[1..], args, params)
	}
	if clean.starts_with('mut ') {
		return 'mut ' + subst_generic_text(clean[4..], args, params)
	}
	if clean.starts_with('?') {
		return '?' + subst_generic_text(clean[1..], args, params)
	}
	if clean.starts_with('!') {
		return '!' + subst_generic_text(clean[1..], args, params)
	}
	if clean.starts_with('...') {
		return '...' + subst_generic_text(clean[3..], args, params)
	}
	if clean.starts_with('[]') {
		return '[]' + subst_generic_text(clean[2..], args, params)
	}
	if clean.starts_with('map[') {
		bracket_end := find_matching_bracket(clean, 3)
		if bracket_end < clean.len {
			key := subst_generic_text(clean[4..bracket_end], args, params)
			val := subst_generic_text(clean[bracket_end + 1..], args, params)
			return 'map[${key}]${val}'
		}
	}
	if clean.starts_with('[') {
		bracket_end := find_matching_bracket(clean, 0)
		if bracket_end < clean.len {
			return clean[..bracket_end + 1] + subst_generic_text(clean[bracket_end +
				1..], args, params)
		}
	}
	if clean.starts_with('fn(') || clean.starts_with('fn (') {
		// A function-type parameter (`fn (T) int`) carries the generic params in its own
		// signature; substitute each parameter and the return type so a `Box[string].apply`
		// callback is expected as `fn (string) int`, not the unsubstituted `fn (T) int`.
		params_start := clean.index_u8(`(`) + 1
		mut depth := 1
		mut params_end := params_start
		for params_end < clean.len {
			if clean[params_end] == `(` {
				depth++
			} else if clean[params_end] == `)` {
				depth--
				if depth == 0 {
					break
				}
			}
			params_end++
		}
		if params_end < clean.len {
			mut fn_parts := []string{}
			params_str := clean[params_start..params_end]
			if params_str.trim_space().len > 0 {
				for part in split_params(params_str) {
					fn_parts << subst_generic_text(normalize_fn_type_param_text(part), args, params)
				}
			}
			ret_str := clean[params_end + 1..].trim_space()
			if ret_str.len > 0 {
				return 'fn(${fn_parts.join(', ')}) ${subst_generic_text(ret_str, args, params)}'
			}
			return 'fn(${fn_parts.join(', ')})'
		}
	}
	bracket := clean.index_u8(`[`)
	if bracket > 0 {
		bracket_end := find_matching_bracket(clean, bracket)
		if bracket_end < clean.len {
			mut parts := []string{}
			for part in split_params(clean[bracket + 1..bracket_end]) {
				parts << subst_generic_text(part, args, params)
			}
			return clean[..bracket] + '[' + parts.join(', ') + ']' + clean[bracket_end + 1..]
		}
	}
	for i, p in params {
		if clean == p {
			return args[i]
		}
	}
	return clean
}

// split_generic_arg_list splits a comma-separated generic argument list at the
// top bracket level (so nested types like `map[int]string` stay intact).
fn split_generic_arg_list(s string) []string {
	mut parts := []string{}
	mut depth := 0
	mut start := 0
	for i := 0; i < s.len; i++ {
		c := s[i]
		if c == `[` || c == `(` {
			depth++
		} else if c == `]` || c == `)` {
			depth--
		} else if c == `,` && depth == 0 {
			parts << s[start..i]
			start = i + 1
		}
	}
	parts << s[start..]
	return parts
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
		return '[]${nested_type_name(t.elem_type)}'
	}
	if t is ArrayFixed {
		mut len_text := t.len.str()
		if t.len_expr.len > 0 {
			len_text = t.len_expr
		}
		return '${nested_type_name(t.elem_type)}[${len_text}]'
	}
	if t is Map {
		return 'map[${nested_type_name(t.key_type)}]${nested_type_name(t.value_type)}'
	}
	if t is Primitive {
		return prim_name(t)
	}
	return ''
}

fn receiver_type_name_variant(t Type, fixed_array_prefix bool, shorten_modules bool) string {
	if t is Alias {
		return receiver_leaf_type_name(t.name, shorten_modules)
	}
	if t is Struct {
		return receiver_leaf_type_name(t.name, shorten_modules)
	}
	if t is Interface {
		return receiver_leaf_type_name(t.name, shorten_modules)
	}
	if t is SumType {
		return receiver_leaf_type_name(t.name, shorten_modules)
	}
	if t is Enum {
		return receiver_leaf_type_name(t.name, shorten_modules)
	}
	if t is String {
		return 'string'
	}
	if t is Array {
		return '[]${receiver_type_name_variant(t.elem_type, fixed_array_prefix, shorten_modules)}'
	}
	if t is ArrayFixed {
		mut len_text := t.len.str()
		if t.len_expr.len > 0 {
			len_text = t.len_expr
		}
		elem := receiver_type_name_variant(t.elem_type, fixed_array_prefix, shorten_modules)
		if fixed_array_prefix {
			return '[${len_text}]${elem}'
		}
		return '${elem}[${len_text}]'
	}
	if t is Map {
		key := receiver_type_name_variant(t.key_type, fixed_array_prefix, shorten_modules)
		value := receiver_type_name_variant(t.value_type, fixed_array_prefix, shorten_modules)
		return 'map[${key}]${value}'
	}
	if t is Primitive {
		return prim_c_type_from(t.props, t.size)
	}
	return receiver_leaf_type_name(t.name(), shorten_modules)
}

fn receiver_leaf_type_name(name string, shorten_modules bool) string {
	if shorten_modules && name.contains('.') {
		return name.all_after_last('.')
	}
	return name
}

fn receiver_type_name_variants(t Type) []string {
	mut names := []string{}
	push_receiver_method_candidate(mut names, receiver_type_name_variant(t, false, false))
	push_receiver_method_candidate(mut names, receiver_type_name_variant(t, false, true))
	push_receiver_method_candidate(mut names, receiver_type_name_variant(t, true, false))
	push_receiver_method_candidate(mut names, receiver_type_name_variant(t, true, true))
	return names
}

fn receiver_type_module_names(t Type) []string {
	mut names := []string{}
	if t is Array {
		for name in receiver_type_module_names(t.elem_type) {
			push_receiver_method_candidate(mut names, name)
		}
	} else if t is ArrayFixed {
		for name in receiver_type_module_names(t.elem_type) {
			push_receiver_method_candidate(mut names, name)
		}
	} else if t is Map {
		for name in receiver_type_module_names(t.key_type) {
			push_receiver_method_candidate(mut names, name)
		}
		for name in receiver_type_module_names(t.value_type) {
			push_receiver_method_candidate(mut names, name)
		}
	} else {
		name := t.name()
		if name.contains('.') {
			push_receiver_method_candidate(mut names, name.all_before_last('.'))
		}
	}
	return names
}

fn push_receiver_method_candidate(mut names []string, name string) {
	if name.len > 0 && name !in names {
		names << name
	}
}

fn module_can_prefix_collection_receiver(module_name string) bool {
	return module_name.len > 0 && module_name != 'main' && module_name != 'builtin'
}

fn exact_array_receiver_method_candidates(t Array, method string, module_name string) []string {
	mut names := []string{}
	append_array_receiver_method_candidates(mut names, t, method, module_name)
	return names
}

fn append_array_receiver_method_candidates(mut names []string, t Array, method string, module_name string) {
	elem_types := receiver_type_name_variants(t.elem_type)
	if elem_types.len == 0 {
		return
	}
	for elem_type in elem_types {
		push_receiver_method_candidate(mut names, '[]${elem_type}.${method}')
	}
	mut module_names := receiver_type_module_names(t.elem_type)
	if module_can_prefix_collection_receiver(module_name) {
		push_receiver_method_candidate(mut module_names, module_name)
	}
	for mod_name in module_names {
		for elem_type in elem_types {
			push_receiver_method_candidate(mut names, '${mod_name}.[]${elem_type}.${method}')
		}
	}
}

fn append_map_receiver_method_candidates(mut names []string, t Map, method string, module_name string) {
	key_types := receiver_type_name_variants(t.key_type)
	value_types := receiver_type_name_variants(t.value_type)
	if key_types.len == 0 || value_types.len == 0 {
		return
	}
	mut map_types := []string{}
	for key_type in key_types {
		for value_type in value_types {
			push_receiver_method_candidate(mut map_types, 'map[${key_type}]${value_type}')
		}
	}
	for map_type in map_types {
		push_receiver_method_candidate(mut names, '${map_type}.${method}')
	}
	mut module_names := []string{}
	if module_can_prefix_collection_receiver(module_name) {
		push_receiver_method_candidate(mut module_names, module_name)
	}
	for mod_name in receiver_type_module_names(t.key_type) {
		push_receiver_method_candidate(mut module_names, mod_name)
	}
	for mod_name in receiver_type_module_names(t.value_type) {
		push_receiver_method_candidate(mut module_names, mod_name)
	}
	for mod_name in module_names {
		for map_type in map_types {
			push_receiver_method_candidate(mut names, '${mod_name}.${map_type}.${method}')
		}
	}
}

fn receiver_method_name_candidates(t Type, method string, module_name string) []string {
	mut names := []string{}
	type_name := resolve_type_name_for_method(t)
	if type_name.len > 0 {
		push_receiver_method_candidate(mut names, '${type_name}.${method}')
	}
	mut clean := t
	if clean is Alias {
		clean = clean.base_type
	}
	if clean is Array {
		append_array_receiver_method_candidates(mut names, clean, method, module_name)
		array_name := 'array.${method}'
		push_receiver_method_candidate(mut names, array_name)
	}
	if clean is Map {
		append_map_receiver_method_candidates(mut names, clean, method, module_name)
		map_name := 'map.${method}'
		push_receiver_method_candidate(mut names, map_name)
	}
	return names
}

fn (tc &TypeChecker) infix_operator_return_type(op flat.Op, lhs Type, rhs Type) ?Type {
	op_name := infix_operator_name(op) or { return none }
	lhs_name := resolve_type_name_for_method(unwrap_pointer(lhs))
	if lhs_name.len == 0 {
		return none
	}
	method_name := '${lhs_name}.${op_name}'
	for candidate in [method_name, c_name(method_name)] {
		ret := tc.fn_ret_types[candidate] or { continue }
		params := tc.fn_param_types[candidate] or { continue }
		if params.len < 2 {
			continue
		}
		if !tc.receiver_compatible(lhs, params[0]) {
			continue
		}
		if !tc.type_compatible(rhs, params[1]) {
			continue
		}
		return ret
	}
	return none
}

fn infix_operator_name(op flat.Op) ?string {
	match op {
		.plus { return '+' }
		.minus { return '-' }
		.mul { return '*' }
		.div { return '/' }
		.mod { return '%' }
		.pipe { return '|' }
		.xor { return '^' }
		.left_shift { return '<<' }
		.right_shift { return '>>' }
		.right_shift_unsigned { return '>>>' }
		else {}
	}

	return none
}

// prim_c_type_from supports prim c type from handling for types.
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

// prim_c_type supports prim c type handling for types.
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

// find_matching_bracket resolves find matching bracket information for types.
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

// split_params supports split params handling for types.
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

// normalize_fn_type_param_text transforms normalize fn type param text data for types.
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

// top_level_space_index supports top level space index handling for types.
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

// fn_type_param_head_is_name supports fn type param head is name handling for types.
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

// c_name converts c name data for types.
fn c_name(name string) string {
	if name.starts_with('C.') {
		return name[2..]
	}
	if name == 'malloc' {
		return 'v_malloc'
	}
	if name == 'int_str' {
		return 'int__str'
	}
	n := name.replace('[]', 'Array_').replace('.-', '__minus').replace('.+', '__plus').replace('.==',
		'__eq').replace('.!=', '__ne').replace('.<=', '__le').replace('.>=', '__ge').replace('.<',
		'__lt').replace('.>', '__gt').replace('.*', '__mul').replace('./', '__div').replace('.%',
		'__mod').replace('.&', '__and').replace('.|', '__or').replace('.^', '__xor').replace('.<<',
		'__left_shift').replace('.>>', '__right_shift').replace('&', 'ptr').replace('[', '_').replace(']', '').replace(',', '_').replace(' ', '_').replace('.', '__')
	if n in c_reserved_words {
		return 'v_${n}'
	}
	return n
}
