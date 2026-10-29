module markused

import time
import os
import v.flat
import v.gen.c.naming
import v.pref
import v.types

const trace_markused = false
const min_eager_markused_bodies = 4096
const markused_source_type_ierror = flat.source_type_id('IError')
const markused_source_type_builtin_ierror = flat.source_type_id('builtin.IError')
const markused_source_type_i128 = flat.source_type_id('i128')
const markused_source_type_u128 = flat.source_type_id('u128')
const markused_source_type_float_literal = flat.source_type_id('float literal')
const markused_source_type_closure_runtime = flat.source_type_id('__v3_builtin_closure_runtime')
const markused_source_type_veb_result = flat.source_type_id('veb.Result')

// library_seeds are the functions that every program keeps, whatever it calls.
const library_seeds = ['time.Time.new', 'Time.new', 'gen_expr_lvalue', 'c.gen_expr_lvalue', 'gen_assign',
	'c.gen_assign']

// parallel_callback_seeds are the callbacks that the compiler hands to its workers.
const parallel_callback_seeds = ['c.FlatGen.gen_fn_items_scoped_batches',
	'markused.CallCollector.collect_bodies_scoped_batches',
	'parser.Parser.precollect_parallel_comptime_consts', 'types.TypeChecker.check_scoped_batches',
	'driver.compare_print_notices', 'pref.detect_vroot', 'pref.detect_vexe', 'v.pref.detect_vroot',
	'v.pref.detect_vexe', 'types.compare_type_errors', 'types.compare_type_notices',
	'types.TypeChecker.result_return_uses_multi_tail', 'sync.Semaphore.timed_wait',
	'sync.Semaphore.destroy']

// core_runtime_seeds and lowered_runtime_seeds are the runtime functions that the
// code generator calls for the constructs of the language. No call in a program
// leads to them, so every program keeps them.
const core_runtime_seeds = ['__new_array', '__new_array_noscan', 'array.get', 'array.push',
	'map_hash_int_4', 'map_hash_int_8', 'map_hash_int_16', 'map_eq_int_4', 'map_eq_int_8',
	'map_eq_int_16', 'map_clone_int_4', 'map_clone_int_8', 'map_clone_int_16', 'map_free_nop']

const lowered_runtime_seeds = ['new_array_from_c_array_aligned', 'new_array_from_c_array',
	'new_array_from_c_array_noscan', 'array.set', 'array.push_many', 'array.insert', 'array.insert_many',
	'array.prepend', 'array.reverse', 'array.slice', 'array.slice_ni', 'string.substr_ni',
	'array.pop_left', 'array.clone', 'array.delete', 'array.ensure_cap', 'string.==', 'string.<',
	'string.free', 'string.all_before', 'string.all_before_last', 'string.all_after',
	'string.all_after_last', 'string.substr', 'string__substr', 'u8.vstring', 'u8.vstring_with_len',
	'u8.vbytes', 'charptr.vstring', 'charptr.vstring_with_len', 'byteptr.vstring',
	'byteptr.vstring_with_len', 'byteptr.vbytes', 'voidptr.vbytes', '[]rune.string', 'map.set',
	'map.exists', 'map.get', 'map.get_check', 'map.get_and_set', 'map.delete', 'map.clone', 'map.clear',
	'map.keys', 'map.values', 'map.reserve', 'map_map_eq', 'memdup', 'memdup_align',
	'strings.Builder.write_ptr', 'strings.Builder.write_runes', 'strings.Builder.free',
	'strconv.format_int', 'strconv.format_uint', 'strconv.Dec32.get_string_32',
	'strconv.Dec64.get_string_64', 'bool.str', 'int.str', 'u64.str', 'rune.str', 'string.+', 'ptr_str',
	'os.join_path_single', 'panic', 'u8.is_letter', 'u8.is_capital', 'string.is_capital',
	'string.to_lower_ascii', 'rune.to_lower', 'Array_u8__bytestr', 'Array_u8__hex', 'data_to_hex_string',
	'map_hash_string', 'map_hash_int_1', 'map_hash_int_2', 'map_hash_int_16', 'map_eq_string',
	'map_eq_int_1', 'map_eq_int_2', 'map_eq_int_16', 'map_clone_string', 'map_clone_int_1',
	'map_clone_int_2', 'map_clone_int_16', 'map_free_string', '[]string.join', 'Array_string__join',
	'embed_file.Decoder.decompress', 'embed_file.join_chunks', 'exit', 'v_exit']

// float_runtime_seeds format the floats of a program that can have them.
const float_runtime_seeds = ['f32.str', 'f64.str', 'strconv__f32_to_str_l', 'strconv__f64_to_str_l']

const ownership_runtime_seeds = ['array.free', 'array__free', 'map.free', 'map__free', 'free']

const int_str_runtime_seeds = ['i8.str', 'i16.str', 'i32.str', 'i64.str']

// The helpers that follow are kept for a construct that enqueue_detected_runtime_helpers
// finds anywhere in a program, in a function that is reached or not.
const optional_runtime_helpers = ['IError.str', 'error', 'error_with_code']

const string_interp_runtime_helpers = ['strings.new_builder', 'strings.Builder.write_string',
	'strings.Builder.str', 'string_plus_many']

const string_membership_runtime_helpers = ['string__contains', 'string__contains_u8']

const map_snapshot_runtime_helpers = ['map.clone', 'map__clone', 'map.free', 'map__free']

// string_method_runtime_helpers are the methods of `string` that are kept when a
// call of that name is found, whatever its receiver is.
const string_method_runtime_helpers = ['trim_space', 'trim_space_left', 'trim_space_right', 'to_upper',
	'to_upper_ascii', 'to_lower', 'to_lower_ascii', 'contains', 'count']

// single_runtime_helpers are the rest of what enqueue_detected_runtime_helpers and
// mark_used keep by name for a construct.
const single_runtime_helpers = ['array.delete_last', 'string__plus', 'new_map', 'f32.eq_epsilon',
	'malloc_uncollectable', 'escape_default_string', 'join_path_single', 'RunesIterator.next']

// seeded_fn_names returns the functions that mark_used keeps without a call that
// leads to them, by the names it looks them up with: those that every program
// keeps, and those that a program keeps for a construct found anywhere in it.
// The channel and `shared` helpers of `sync` are not among them.
pub fn seeded_fn_names() []string {
	mut names := library_seeds.clone()
	names << parallel_callback_seeds
	names << core_runtime_seeds
	names << lowered_runtime_seeds
	names << float_runtime_seeds
	names << ownership_runtime_seeds
	names << int_str_runtime_seeds
	names << optional_runtime_helpers
	names << string_interp_runtime_helpers
	names << string_membership_runtime_helpers
	names << map_snapshot_runtime_helpers
	names << string_method_runtime_helpers
	names << single_runtime_helpers
	return names
}

// mark_used updates mark used state for markused.
pub fn mark_used(a &flat.FlatAst, tc &types.TypeChecker) map[string]bool {
	used, _ := mark_used_with_test_files(a, tc, map[string]bool{}, map[string]bool{}, false, true, false, true, unsafe { nil }, unsafe { nil }, false, unsafe { nil }, false)
	return used
}

pub fn mark_used_for_tests(a &flat.FlatAst, tc &types.TypeChecker, test_files []string) map[string]bool {
	used, _ := mark_used_for_tests_with_generic_usage(a, tc, test_files)
	return used
}

// mark_used_with_generic_usage also reports whether reachable code uses a generic
// function, struct, or sum type and therefore requires monomorphization.
pub fn mark_used_with_generic_usage(a &flat.FlatAst, tc &types.TypeChecker) (map[string]bool, bool) {
	return mark_used_with_test_files(a, tc, map[string]bool{}, map[string]bool{}, false, true, false, true, unsafe { nil }, unsafe { nil }, false, unsafe { nil }, false)
}

// mark_used_with_generic_usage_cached is mark_used_with_generic_usage that keeps
// its declaration indexes in `decl_cache`, so a later pass over the same
// declarations (after semantic checks of more library bodies) reuses them.
pub fn mark_used_with_generic_usage_cached(a &flat.FlatAst, tc &types.TypeChecker, decl_cache &MarkusedDeclCache) (map[string]bool, bool) {
	return mark_used_with_test_files(a, tc, map[string]bool{}, map[string]bool{}, false, true, false, true, unsafe { nil }, unsafe { nil }, false, decl_cache, false)
}

// mark_used_with_generic_usage_full_runtime_cached is the decl_cache variant of
// mark_used_with_generic_usage_full_runtime.
pub fn mark_used_with_generic_usage_full_runtime_cached(a &flat.FlatAst, tc &types.TypeChecker, decl_cache &MarkusedDeclCache) (map[string]bool, bool) {
	return mark_used_with_test_files(a, tc, map[string]bool{}, map[string]bool{}, false, true, false, false, unsafe { nil }, unsafe { nil }, false, decl_cache, false)
}

// mark_used_with_generic_usage_full_runtime disables the literal-output shortcut so
// compatibility fixtures retain helpers referenced by the complete builtin runtime.
pub fn mark_used_with_generic_usage_full_runtime(a &flat.FlatAst, tc &types.TypeChecker) (map[string]bool, bool) {
	return mark_used_with_test_files(a, tc, map[string]bool{}, map[string]bool{}, false, true, false, false, unsafe { nil }, unsafe { nil }, false, unsafe { nil }, false)
}

// mark_used_without_generic_detection is the self-host variant for inputs whose caller
// already guarantees that monomorphization is unnecessary.
pub fn mark_used_without_generic_detection(a &flat.FlatAst, tc &types.TypeChecker) map[string]bool {
	used, _ := mark_used_with_test_files(a, tc, map[string]bool{}, map[string]bool{}, false, false, false, true, unsafe { nil }, unsafe { nil }, false, unsafe { nil }, false)
	return used
}

// mark_used_without_generic_detection_prepared consumes declaration indexes
// built concurrently with checking for the no-generic self-host path.
pub fn mark_used_without_generic_detection_prepared(a &flat.FlatAst, tc &types.TypeChecker, mut prepared PreparedMarkusedDecls) map[string]bool {
	used, _ := mark_used_with_test_files(a, tc, map[string]bool{}, map[string]bool{}, false, false, false, true, prepared, unsafe { nil }, false, unsafe { nil }, false)
	return used
}

// mark_used_for_tests_with_generic_usage is the test-file-rooted variant of
// mark_used_with_generic_usage.
pub fn mark_used_for_tests_with_generic_usage(a &flat.FlatAst, tc &types.TypeChecker, test_files []string) (map[string]bool, bool) {
	mut file_map := map[string]bool{}
	for file in test_files {
		file_map[file] = true
	}
	return mark_used_with_test_files(a, tc, file_map, map[string]bool{}, false, true, false, true, unsafe { nil }, unsafe { nil }, false, unsafe { nil }, false)
}

// mark_used_for_native_tests_with_generic_usage roots test hooks and discovers
// runtime helpers only in reachable bodies, avoiding unused unsupported native code.
pub fn mark_used_for_native_tests_with_generic_usage(a &flat.FlatAst, tc &types.TypeChecker, test_files []string) (map[string]bool, bool) {
	mut file_map := map[string]bool{}
	for file in test_files {
		file_map[file] = true
	}
	return mark_used_with_test_files(a, tc, file_map, map[string]bool{}, false, true, false, true, unsafe { nil }, unsafe { nil }, false, unsafe { nil }, true)
}

// mark_all_used_with_generic_usage roots every concrete function while preserving
// ordinary generic reachability and test harness roots.
pub fn mark_all_used_with_generic_usage(a &flat.FlatAst, tc &types.TypeChecker, test_files []string) (map[string]bool, bool) {
	mut file_map := map[string]bool{}
	for file in test_files {
		file_map[file] = true
	}
	return mark_used_with_test_files(a, tc, file_map, map[string]bool{}, false, true, true, true, unsafe { nil }, unsafe { nil }, false, unsafe { nil }, false)
}

// mark_used_for_cache roots every concrete function in modules being built for the object cache.
pub fn mark_used_for_cache(a &flat.FlatAst, tc &types.TypeChecker, test_files []string, source_modules map[string]bool) map[string]bool {
	used, _ := mark_used_for_cache_with_generic_usage(a, tc, test_files, source_modules)
	return used
}

// mark_used_for_cache_with_generic_usage roots cached module functions and reports whether
// their reachable code requires monomorphization.
pub fn mark_used_for_cache_with_generic_usage(a &flat.FlatAst, tc &types.TypeChecker, test_files []string, source_modules map[string]bool) (map[string]bool, bool) {
	mut file_map := map[string]bool{}
	for file in test_files {
		file_map[file] = true
	}
	return mark_used_with_test_files(a, tc, file_map, source_modules, true, true, false, true, unsafe { nil }, unsafe { nil }, false, unsafe { nil }, false)
}

// mark_used_for_cache_without_generic_detection is the self-host cache variant for inputs
// whose caller already guarantees that monomorphization is unnecessary.
pub fn mark_used_for_cache_without_generic_detection(a &flat.FlatAst, tc &types.TypeChecker, test_files []string, source_modules map[string]bool) map[string]bool {
	mut file_map := map[string]bool{}
	for file in test_files {
		file_map[file] = true
	}
	used, _ := mark_used_with_test_files(a, tc, file_map, source_modules, true, false, false, true, unsafe { nil }, unsafe { nil }, false, unsafe { nil }, false)
	return used
}

// mark_used_checking_library_bodies checks each reached concrete library body before
// collecting its calls, retaining the current reachability queue between frontiers.
// The checker and reachability results belong to the caller's compilation arena.
pub fn mark_used_checking_library_bodies(a &flat.FlatAst, mut tc types.TypeChecker, parallel bool) (map[string]bool, bool) {
	tc.library_body_convergence_complete = false
	return mark_used_with_test_files(a, &tc, map[string]bool{}, map[string]bool{}, false, true,
		false, true, unsafe { nil }, unsafe { voidptr(&tc) }, parallel, unsafe { nil }, false)
}

// reachable_const_exprs returns const initializer expressions referenced by the supplied
// post-transform AST roots, including transitive const-to-const references.
pub fn reachable_const_exprs(a &flat.FlatAst, tc &types.TypeChecker, root_ids []flat.NodeId, root_modules []string, root_files []string) []flat.NodeId {
	mut import_contexts := []map[string]string{cap: 64}
	import_contexts << map[string]string{}
	mut import_context_by_file := map[string]int{}
	for node_idx in tc.top_level_idx {
		node := a.nodes[node_idx]
		if node.kind != .file {
			continue
		}
		import_context_by_file[node.value] = import_contexts.len
		import_contexts << markused_top_level_file_imports(a, node)
	}
	mut const_decls := map[string]ConstDeclInfo{}
	mut const_suffixes := map[string]bool{}
	for name, expr_id in tc.const_exprs {
		file := tc.const_files[name] or { '' }
		const_decls[name] = ConstDeclInfo{
			expr_id:        expr_id
			module:         tc.const_modules[name] or { '' }
			import_context: import_context_by_file[file] or { 0 }
		}
		add_candidate_suffix(mut const_suffixes, name)
	}
	collector := CallCollector{
		a:                       a
		tc:                      tc
		fn_decls:                map[string]FnDeclInfo{}
		fn_suffixes:             map[string]bool{}
		struct_decls:            map[u32]StructDeclInfo{}
		const_decls:             const_decls
		const_suffixes:          const_suffixes
		const_suffixes_complete: true
		import_contexts:         import_contexts
		selective_alias_targets: map[u32][]u32{}
		iface_param_gate:        map[string]bool{}
		generic_type_bases:      map[u32]bool{}
	}
	mut pending := []string{cap: 16}
	for i, root_id in root_ids {
		if i >= root_modules.len || i >= root_files.len {
			break
		}
		root := a.node(root_id)
		context := import_context_by_file[root_files[i]] or { 0 }
		collector.collect_initializer_expr_refs(root, root_modules[i], collector.imports(context), mut pending)
	}
	mut result := []flat.NodeId{cap: pending.len}
	mut seen_names := map[string]bool{}
	mut seen_exprs := map[int]bool{}
	mut qi := 0
	for qi < pending.len {
		name := pending[qi]
		qi++
		if name in seen_names {
			continue
		}
		seen_names[name] = true
		info := const_decls[name] or { continue }
		expr_idx := int(info.expr_id)
		if expr_idx in seen_exprs {
			continue
		}
		seen_exprs[expr_idx] = true
		result << info.expr_id
		collector.collect_initializer_expr_refs(a.node(info.expr_id), info.module, collector.imports(info.import_context), mut pending)
	}
	return result
}

// MarkusedDeclScan holds the declaration indexes that one reachability pass
// derives from the top-level AST and the checker's declaration tables.
struct MarkusedDeclScan {
mut:
	fn_decls                     map[string]FnDeclInfo
	fn_decl_lists                map[string][]FnDeclInfo
	struct_decls                 map[u32]StructDeclInfo
	const_decls                  map[string]ConstDeclInfo
	fn_name_suffixes             map[string]bool
	const_name_suffixes          map[string]bool
	suffix_map                   map[string][]string
	import_contexts              []map[string]string
	body_ids                     []int
	body_modules                 []string
	body_import_contexts         []int
	cache_roots                  []string
	cached_header_enum_str_roots []u32
	c_interface_roots            []string
	marked_roots                 []string
	convergence_index_complete   bool
	fn_count                     int
	fn_with_dot                  int
	contains2_total              int
}

// build_markused_decl_scan indexes every top-level declaration. Its result is a
// function of the AST, `tc.top_level_idx`, and the checker's file, module and
// function-file tables only.
fn build_markused_decl_scan(a &flat.FlatAst, tc &types.TypeChecker, cache_mode bool, cache_modules map[string]bool) MarkusedDeclScan {
	mut result := MarkusedDeclScan{
		fn_decls:                   map[string]FnDeclInfo{}
		fn_decl_lists:              map[string][]FnDeclInfo{}
		struct_decls:               map[u32]StructDeclInfo{}
		const_decls:                map[string]ConstDeclInfo{}
		fn_name_suffixes:           map[string]bool{}
		const_name_suffixes:        map[string]bool{}
		suffix_map:                 map[string][]string{}
		import_contexts:            []map[string]string{cap: 256}
		body_ids:                   []int{cap: 8192}
		body_modules:               []string{cap: 8192}
		body_import_contexts:       []int{cap: 8192}
		convergence_index_complete: true
	}
	result.import_contexts << map[string]string{}
	// The checker already knows the declaration cardinalities. Reserve once here
	// instead of repeatedly growing the alias-heavy reachability indexes.
	result.fn_decls.reserve(u32(tc.fn_ret_types.len))
	result.fn_decl_lists.reserve(u32(tc.fn_ret_types.len))
	result.struct_decls.reserve(u32(tc.structs.len * 2))
	result.const_decls.reserve(u32(tc.const_types.len))
	result.fn_name_suffixes.reserve(u32(tc.fn_ret_types.len))
	result.const_name_suffixes.reserve(u32(tc.const_types.len))
	result.suffix_map.reserve(u32(tc.fn_ret_types.len / 2))
	mut cur_module := ''
	mut cur_file := ''
	mut import_context_by_file := map[string]int{}
	mut cur_import_context := 0
	// Body-call metadata feeds the reachability BFS's discovery of lowering-introduced
	// implicit helpers, which is required on every path (not just generics detection).
	collect_body_metadata := true
	for node_idx in tc.top_level_idx {
		node := a.nodes[node_idx]
		if node.kind == .file {
			cur_file = node.value
			cur_module = ''
			result.import_contexts << markused_top_level_file_imports(a, node)
			cur_import_context = result.import_contexts.len - 1
			import_context_by_file[cur_file] = cur_import_context
			continue
		}
		if node.kind == .module_decl {
			cur_module = node.value
			continue
		}
		if node.kind == .import_decl {
			if cur_import_context >= 0 && cur_import_context < result.import_contexts.len {
				result.import_contexts[cur_import_context][node.typ_text()] = node.value
			}
			continue
		}
		decl_file := if source_file := a.source_files[node.pos.id] {
			source_file.name
		} else {
			cur_file
		}
		decl_module := tc.file_modules[decl_file] or { cur_module }
		decl_import_context := import_context_by_file[decl_file] or { cur_import_context }
		if node.kind == .enum_decl && decl_file.ends_with('.vh') {
			result.cached_header_enum_str_roots << markused_qualify_source_type(decl_module, node.source_value_id())
		}
		if node.kind == .struct_decl {
			name_id := node.source_value_id()
			full_name := markused_qualify_source_type(decl_module, name_id)
			info := StructDeclInfo{
				node_id:        flat.NodeId(node_idx)
				module:         decl_module
				import_context: decl_import_context
			}
			result.struct_decls[full_name] = info
			if name_id !in result.struct_decls {
				result.struct_decls[name_id] = info
			}
			continue
		}
		if node.kind == .const_decl || node.kind == .global_decl {
			for i in 0 .. node.children_count {
				field_id := a.child(&node, i)
				field := a.node(field_id)
				if field.kind != .const_field || field.children_count == 0 {
					continue
				}
				info := ConstDeclInfo{
					expr_id:        a.child(field, 0)
					module:         decl_module
					import_context: decl_import_context
				}
				result.const_decls[field.value] = info
				add_candidate_suffix(mut result.const_name_suffixes, field.value)
				full_name := qualify_fn(decl_module, field.value)
				if full_name != field.value {
					result.const_decls[full_name] = info
					add_candidate_suffix(mut result.const_name_suffixes, full_name)
				}
			}
			continue
		}
		if node.kind == .fn_decl || node.kind == .c_fn_decl {
			decl_qname := qualify_fn(decl_module, node.value)
			fn_decl_file := tc.fn_type_files[decl_qname] or { decl_file }
			fn_decl_module := tc.fn_type_modules[decl_qname] or { decl_module }
			if fn_decl_module != decl_module {
				result.convergence_index_complete = false
			}
			fn_decl_import_context := import_context_by_file[fn_decl_file] or {
				decl_import_context
			}
			if collect_body_metadata {
				result.body_ids << node_idx
				result.body_modules << fn_decl_module
				result.body_import_contexts << fn_decl_import_context
			}
			has_dot := node.value.index_u8(`.`) >= 0
			can_suffix_match := !markused_fn_decl_is_generic_template(node, a)
			if trace_markused {
				result.fn_count++
				if has_dot {
					result.fn_with_dot++
					if result.fn_with_dot <= 5 {
						eprintln('  fn with dot: "${node.value}"')
					}
				}
			}
			info := FnDeclInfo{
				node_id:           flat.NodeId(node_idx)
				module:            fn_decl_module
				import_context:    fn_decl_import_context
				positional_params: markused_positional_param_count(a, &node)
			}
			add_fn_decl_info(mut result.fn_decls, mut result.fn_decl_lists, node.value, info)
			if can_suffix_match {
				add_candidate_suffix(mut result.fn_name_suffixes, node.value)
			}
			lowered_name := markused_c_name(node.value)
			if lowered_name != node.value {
				add_fn_decl_info(mut result.fn_decls, mut result.fn_decl_lists, lowered_name, info)
				if can_suffix_match {
					add_candidate_suffix(mut result.fn_name_suffixes, lowered_name)
				}
			}
			qname := qualify_fn(fn_decl_module, node.value)
			if markused_fn_has_attribute(a, node_idx, 'markused') {
				result.marked_roots << qname
			}
			// Cached headers retain bodies only when the warm pass must recreate
			// generated specializations or closure support symbols. Root those bodies
			// even though their ordinary caller can live entirely in another cached
			// object and therefore be invisible to the program AST.
			cached_header_body := fn_decl_file.ends_with('.vh') && !node.is_mut
			if cache_mode && node.kind == .fn_decl && node.generic_params().len == 0
				&& (cache_modules[fn_decl_module] || cached_header_body) {
				result.cache_roots << qname
			}
			if qname != node.value {
				add_fn_decl_info(mut result.fn_decls, mut result.fn_decl_lists, qname, info)
				if can_suffix_match {
					add_candidate_suffix(mut result.fn_name_suffixes, qname)
				}
				lowered_qname := markused_c_name(qname)
				if lowered_qname != qname {
					add_fn_decl_info(mut result.fn_decls, mut result.fn_decl_lists, lowered_qname, info)
					if can_suffix_match {
						add_candidate_suffix(mut result.fn_name_suffixes, lowered_qname)
					}
				}
			}
			// Build result.suffix_map entries
			if has_dot && can_suffix_match {
				if trace_markused {
					result.contains2_total++
				}
				short := node.value.all_after_last('.')
				add_suffix_candidate(mut result.suffix_map, short, node.value)
				if qname != node.value {
					add_suffix_candidate(mut result.suffix_map, short, qname)
				}
			}
			if qname != node.value && qname.index_u8(`.`) >= 0 && can_suffix_match {
				short := qname.all_after_last('.')
				add_suffix_candidate(mut result.suffix_map, short, qname)
			}
			if fn_decl_module == 'c' && fn_decl_file.ends_with('/gen/c/interface.v') {
				result.c_interface_roots << node.value
				if qname != node.value {
					result.c_interface_roots << qname
				}
				lowered_qname := markused_c_name(qname)
				if lowered_qname != qname {
					result.c_interface_roots << lowered_qname
				}
			}
		}
	}
	return result
}

// MarkusedDeclCache lets a later reachability pass reuse the declaration scan
// of an earlier one (see mark_used_with_generic_usage_cached). The indexes live
// in the cache's own arena until `release`.
@[heap]
pub struct MarkusedDeclCache {
mut:
	scope               voidptr
	ready               bool
	ast                 voidptr
	node_count          int
	children_count      int
	source_files_len    int
	file_modules_len    int
	fn_type_files_len   int
	fn_type_modules_len int
	top_level           []i32
	scan                MarkusedDeclScan
}

// new_markused_decl_cache returns an empty declaration-scan cache.
pub fn new_markused_decl_cache() &MarkusedDeclCache {
	return &MarkusedDeclCache{}
}

// release frees the cached declaration indexes. The caller must have copied
// every reachability result that can name their strings.
pub fn (mut cache MarkusedDeclCache) release() {
	scope := cache.scope
	cache.scope = unsafe { nil }
	// `matches` checks this first; the stale index headers are never read.
	cache.ready = false
	markused_worker_scope_free(scope)
}

// matches reports whether the scan inputs are unchanged: no node, child or
// top-level declaration was added, and no file module or function file was
// registered. Semantic body checks never rewrite those inputs in place.
@[direct_array_access]
fn (cache &MarkusedDeclCache) matches(a &flat.FlatAst, tc &types.TypeChecker) bool {
	if !cache.ready || cache.ast != voidptr(a) || cache.node_count != a.nodes.len
		|| cache.children_count != a.children.len || cache.source_files_len != a.source_files.len
		|| cache.file_modules_len != tc.file_modules.len
		|| cache.fn_type_files_len != tc.fn_type_files.len
		|| cache.fn_type_modules_len != tc.fn_type_modules.len
		|| cache.top_level.len != tc.top_level_idx.len {
		return false
	}
	for i, idx in cache.top_level {
		if tc.top_level_idx[i] != idx {
			return false
		}
	}
	return true
}

// markused_decl_scan returns the declaration indexes for this pass, reusing or
// filling `decl_cache` when the caller supplied one.
fn markused_decl_scan(a &flat.FlatAst, tc &types.TypeChecker, cache_mode bool, cache_modules map[string]bool, decl_cache &MarkusedDeclCache) MarkusedDeclScan {
	if isnil(decl_cache) || cache_mode || cache_modules.len > 0 {
		return build_markused_decl_scan(a, tc, cache_mode, cache_modules)
	}
	mut cache := unsafe { decl_cache }
	if cache.matches(a, tc) {
		return cache.scan
	}
	if cache.ready {
		// The declarations changed under an earlier pass. Its results were
		// already copied out of the cache arena; index this pass afresh.
		cache.release()
	}
	scope := markused_worker_scope_begin(true)
	cache.scan = build_markused_decl_scan(a, tc, false, map[string]bool{})
	cache.top_level = tc.top_level_idx.clone()
	markused_worker_scope_leave(scope)
	cache.scope = scope
	cache.ast = voidptr(a)
	cache.node_count = a.nodes.len
	cache.children_count = a.children.len
	cache.source_files_len = a.source_files.len
	cache.file_modules_len = tc.file_modules.len
	cache.fn_type_files_len = tc.fn_type_files.len
	cache.fn_type_modules_len = tc.fn_type_modules.len
	cache.ready = true
	return cache.scan
}

@[direct_array_access]
fn mark_used_with_test_files(a &flat.FlatAst, tc &types.TypeChecker, test_files map[string]bool, cache_modules map[string]bool, cache_mode bool, detect_generics bool, all_functions bool, allow_trivial_literal_output bool, prepared &PreparedMarkusedDecls, library_body_checker voidptr, library_check_parallel bool, decl_cache &MarkusedDeclCache, reachable_test_helpers bool) (map[string]bool, bool) {
	mut mu_sw := time.new_stopwatch()
	trivial_literal_output := allow_trivial_literal_output && !cache_mode && cache_modules.len == 0
		&& test_files.len == 0 && !all_functions
		&& is_trivial_literal_output_program(a, tc.diagnostic_files)
	// An exact literal-output program has no user expressions or declarations that
	// can instantiate a generic. Skip generic indexes and per-node generic checks,
	// just as the known non-generic self-host path does. A mark-all build also emits
	// library bodies, whose generic calls still need specialization.
	detect_reachable_generics := detect_generics && !trivial_literal_output
	// Full-runtime fixtures, C tests, module caches, and self-host builds retain
	// the conservative whole-program scan. Ordinary and native test builds discover
	// expression helpers while following their reachable function bodies.
	reachable_runtime_helpers := detect_reachable_generics && allow_trivial_literal_output
		&& !cache_mode && cache_modules.len == 0
		&& (test_files.len == 0 || reachable_test_helpers) && !all_functions
	use_prepared := !isnil(prepared) && prepared.ready && !detect_reachable_generics && !cache_mode
		&& !all_functions
	// Runtime-helper detection reads completed semantic types. Start it here,
	// after checking, and overlap it with the declaration scan + precollect.
	// Its enqueue requests are replayed in scan order at the seeds step below.
	markused_parallel_allowed := !isnil(a.worker_pool) || tc.scoped_parallel_workers_enabled()
	rt_scan_parallel := markused_parallel_allowed && par_markused_seeds_enabled()
		&& !trivial_literal_output && !reachable_runtime_helpers
	mut rt_scan := &RtHelpersScanArgs{
		a:  a
		tc: if rt_scan_parallel { tc.fork_for_parallel_transform(a) } else { tc }
	}
	rt_scan.enabled = rt_scan_parallel
	mut rt_scan_threads := []thread{cap: 1}
	if rt_scan_parallel {
		rt_scan_threads << spawn markused_rt_helpers_thread(mut rt_scan)
	}
	mut import_contexts := []map[string]string{}
	mut fn_decls := map[string]FnDeclInfo{}
	mut fn_decl_lists := map[string][]FnDeclInfo{}
	mut struct_decls := map[u32]StructDeclInfo{}
	mut const_decls := map[string]ConstDeclInfo{}
	mut fn_name_suffixes := map[string]bool{}
	mut const_name_suffixes := map[string]bool{}
	mut generic_type_bases := map[u32]bool{}
	if detect_reachable_generics {
		for node_idx in 0 .. a.nodes.len {
			// Read the node in place: the loop copies only the few declarations.
			node := unsafe { &a.nodes[node_idx] }
			if node.kind in [.struct_decl, .type_decl] && node.generic_params().len > 0 {
				generic_type_bases[node.source_value_id()] = true
			}
		}
	}

	// Reverse index: short name (after last '.') -> list of full qualified names
	mut suffix_map := map[string][]string{}

	// Every fn_decl/c_fn_decl node (and its module), in AST order: the worklist
	// for the parallel body-call precollection below.
	mut body_ids := []int{}
	mut body_modules := []string{}
	mut body_import_contexts := []int{}
	mut cache_roots := []string{}
	mut cached_header_enum_str_roots := []u32{}
	mut c_interface_roots := []string{}
	mut marked_roots := []string{}
	if use_prepared {
		// These indexes remain owned by the retained helper arena until the
		// prepared wrapper clones the result and releases that arena.
		unsafe {
			fn_decls = prepared.fn_decls
			fn_decl_lists = prepared.fn_decl_lists
			struct_decls = prepared.struct_decls
			const_decls = prepared.const_decls
			fn_name_suffixes = prepared.fn_name_suffixes
			const_name_suffixes = prepared.const_name_suffixes
			suffix_map = prepared.suffix_map
			import_contexts = prepared.import_contexts
			marked_roots = prepared.marked_roots
			cached_header_enum_str_roots = prepared.cached_header_enum_str_roots
			c_interface_roots = prepared.c_interface_roots
			body_ids = prepared.body_ids
			body_modules = prepared.body_modules
			body_import_contexts = prepared.body_import_contexts
		}
	}

	mut convergence_index_complete := !use_prepared
	convergence_node_count := a.nodes.len
	convergence_top_level_size := tc.top_level_idx.len
	mut fn_count := 0
	mut fn_with_dot := 0
	mut contains2_total := 0
	if !use_prepared {
		scan := markused_decl_scan(a, tc, cache_mode, cache_modules, decl_cache)
		// The scan's indexes are read-only below; share them instead of copying.
		unsafe {
			fn_decls = scan.fn_decls
			fn_decl_lists = scan.fn_decl_lists
			struct_decls = scan.struct_decls
			const_decls = scan.const_decls
			fn_name_suffixes = scan.fn_name_suffixes
			const_name_suffixes = scan.const_name_suffixes
			suffix_map = scan.suffix_map
			import_contexts = scan.import_contexts
			body_ids = scan.body_ids
			body_modules = scan.body_modules
			body_import_contexts = scan.body_import_contexts
			cache_roots = scan.cache_roots
			cached_header_enum_str_roots = scan.cached_header_enum_str_roots
			c_interface_roots = scan.c_interface_roots
			marked_roots = scan.marked_roots
		}
		convergence_index_complete = scan.convergence_index_complete
		fn_count = scan.fn_count
		fn_with_dot = scan.fn_with_dot
		contains2_total = scan.contains2_total
	}

	// BFS from main
	mut used := map[string]bool{}
	used.reserve(u32(fn_decls.len))
	mut queue := []string{cap: fn_decls.len}
	reachable_modules := if use_prepared {
		map[string]bool{}
	} else {
		markused_reachable_modules(a, tc)
	}
	queue << 'main'
	used['main'] = true
	if cache_mode {
		// Cached bodies can call an interface without exposing that call in their
		// declaration header. Keep the program's implementations and their callees.
		for iface_name_source_id in tc.interface_names.keys() {
			iface_name := flat.source_type_name(iface_name_source_id)
			iface_id := iface_name_source_id
			for method in tc.interface_abstract_method_names(iface_id) {
				key := tc.interface_method_signature_key(iface_id, method) or { continue }
				if (tc.fn_type_files[key] or { '' }).ends_with('.vh') {
					enqueue('${iface_name}.${method}', mut used, mut queue)
				}
			}
		}
		// Cached IError dispatchers name every implementer's methods, even when
		// their callers have no body in the warm AST. Cgen emits these methods,
		// so retain their transformations and the helpers their bodies call.
		for impl_source_id in tc.ierror_impl_names() {
			for method in ['msg', 'code'] {
				for alias in interface_implementer_method_aliases(impl_source_id, method, tc) {
					enqueue(alias, mut used, mut queue)
				}
			}
		}
	}
	$if ownership ? {
		// Ownership cleanup and clone lowering emits direct C references to these
		// process-wide IError sentinels. They have no source-AST reference for the
		// ordinary markused traversal to discover, so root their const declarations.
		for seed in ['builtin.none__', 'builtin.error_sentinel'] {
			enqueue(seed, mut used, mut queue)
		}
		// The `_result` destructor names every IError implementer's destructor, with no
		// source-AST call site to follow. The branch below emits it too, so root them here.
		ierror_destructor := if tc.autofree_mode { 'free' } else { 'drop' }
		for impl_source_id_2 in tc.ierror_impl_names() {
			for alias in interface_implementer_method_aliases(impl_source_id_2, ierror_destructor, tc) {
				enqueue(alias, mut used, mut queue)
			}
		}
	}
	enqueue_main_module_roots(fn_decls, mut used, mut queue)
	if use_prepared {
		for root in prepared.auto_roots {
			enqueue(root, mut used, mut queue)
		}
	} else {
		enqueue_auto_roots(a, fn_decls, reachable_modules, mut used, mut queue)
	}
	enqueue_implicit_global_container_roots(a, tc, fn_decls, fn_decl_lists, mut used, mut queue)
	for root in marked_roots {
		enqueue(root, mut used, mut queue)
	}
	// Overflow calls are introduced by C generation after reachability has been
	// computed. The synthetic `builtin.overflow` import exists only for
	// `-check-overflow` and `-check-casts`, so its declarations are the signal to retain the helper
	// bodies that those generated calls need.
	if 'builtin.overflow.add_i8' in fn_decls || 'overflow.add_i8' in fn_decls {
		for op in ['add', 'sub', 'mul', 'shl', 'shr'] {
			for typ in ['i8', 'u8', 'i16', 'u16', 'i32', 'u32', 'i64', 'u64'] {
				enqueue('builtin.overflow.${op}_${typ}', mut used, mut queue)
				enqueue('overflow.${op}_${typ}', mut used, mut queue)
			}
		}
		for op in ['neg', 'div', 'mod'] {
			for typ in ['i8', 'i16', 'i32', 'i64'] {
				enqueue('builtin.overflow.${op}_${typ}', mut used, mut queue)
				enqueue('overflow.${op}_${typ}', mut used, mut queue)
			}
		}
		for name in ['cast_overflow_signed', 'cast_overflow_unsigned'] {
			enqueue('builtin.overflow.${name}', mut used, mut queue)
			enqueue('overflow.${name}', mut used, mut queue)
		}
	}
	// Interface dispatchers are generated after reachability has been computed.
	// When the program can load V shared libraries, they forward type tags they do
	// not implement to the libraries' dispatchers through this lookup.
	if 'dl.interface_export_find' in fn_decls {
		enqueue('dl.interface_export_find', mut used, mut queue)
	}
	// Exported functions are externally reachable even when the input has no V
	// entry point (for example `-is_o` modules called from C).
	enqueue_export_roots(a, tc, mut used, mut queue)
	if trivial_literal_output {
		// Bounds-checking helpers are part of the generated C prelude even when the
		// user program only prints a literal. Their diagnostics stringify indexes
		// and release intermediate concatenations after markused has run.
		for seed in ['strconv.format_int', 'string.free', 'string__free'] {
			enqueue(seed, mut used, mut queue)
		}
		// Linux backtraces add strings.Builder to the otherwise minimal runtime.
		// Its C bodies and map diagnostics contain calls lowered after markused.
		if 'strings.Builder.str' in fn_decls || 'Builder.str' in fn_decls {
			for seed in ['strconv.format_uint', 'u8.ascii_str', 'byteptr.vstring_with_len', 'array.push',
				'array__push', 'array.free', 'array__free', 'int.str'] {
				enqueue(seed, mut used, mut queue)
			}
		}
	}
	if !trivial_literal_output {
		enqueue_veb_handler_roots(a, tc, mut used, mut queue)
		enqueue_test_file_roots(a, test_files, mut used, mut queue)
		for seed in library_seeds {
			queue << seed
			used[seed] = true
		}
		// Parallel compiler callbacks and channel runtime helpers contain calls that
		// are only selected after markused (by prealloc/worker lowering). Keep their
		// concrete callees available for self-hosted compiler builds.
		for seed in parallel_callback_seeds {
			queue << seed
			used[seed] = true
		}
		for seed in core_runtime_seeds {
			if reachable_runtime_helpers && markused_is_map_runtime_seed(seed) {
				continue
			}
			queue << seed
			used[seed] = true
		}
		mut runtime_seeds := lowered_runtime_seeds.clone()
		if !tc.nofloat && !reachable_runtime_helpers {
			runtime_seeds << float_runtime_seeds
		}
		for seed in runtime_seeds {
			if reachable_runtime_helpers && markused_is_map_runtime_seed(seed) {
				continue
			}
			// Array mutation/reversal methods are reached from explicit source calls.
			// Reached selectors retain their helpers below, including batch lowering.
			if reachable_runtime_helpers && seed in ['array.insert', 'array.insert_many',
				'array.prepend', 'array.reverse', 'array.pop_left', 'array.delete'] {
				continue
			}
			// These utilities have source callers; they are never synthesized by lowering.
			if reachable_runtime_helpers && seed in ['string.all_before', 'string.all_before_last',
				'string.all_after', 'string.all_after_last', 'u8.is_letter', 'u8.is_capital',
				'string.is_capital', 'string.to_lower_ascii', 'rune.to_lower'] {
				continue
			}
			// Conversions need their bodies only when a reached selector names them.
			if reachable_runtime_helpers && seed in ['[]rune.string', 'Array_u8__bytestr',
				'Array_u8__hex', 'data_to_hex_string'] {
				continue
			}
			// Float conversion methods have source callers. Ordinary reachability
			// keeps them when a reached expression actually needs float formatting.
			if reachable_runtime_helpers
				&& seed in ['strconv.Dec32.get_string_32', 'strconv.Dec64.get_string_64'] {
				continue
			}
			queue << seed
			used[seed] = true
		}
		if !reachable_runtime_helpers {
			queue << 'array.delete_last'
			used['array.delete_last'] = true
		}
		// Ownership cleanup is synthesized after markused. Its array/map destructors
		// therefore have no AST call sites for the collector to follow. This also
		// applies to drop-before-reassignment, which is not part of the exit snapshots.
		for helper in ownership_runtime_seeds {
			if reachable_runtime_helpers && markused_is_map_runtime_seed(helper) {
				continue
			}
			enqueue(helper, mut used, mut queue)
		}
		for type_name_id in tc.ownership_drop_type_names() {
			type_name := flat.source_type_name(type_name_id)
			method := if tc.autofree_mode { '${type_name}.free' } else { '${type_name}.drop' }
			enqueue(method, mut used, mut queue)
			lowered := markused_c_name(method)
			if lowered != method {
				enqueue(lowered, mut used, mut queue)
			}
		}
		for seed in int_str_runtime_seeds {
			queue << seed
			used[seed] = true
		}
	}
	// Cached module objects can call generated enum stringifiers from bodies that
	// are intentionally absent from their public headers. Keep those program-level
	// helpers even when no visible caller remains in the warm-cache AST.
	for type_name in cached_header_enum_str_roots {
		enqueue_enum_str_method(type_name, '', tc, mut used, mut queue)
	}
	for name in cache_roots {
		enqueue(name, mut used, mut queue)
	}
	for name in c_interface_roots {
		enqueue(name, mut used, mut queue)
	}
	if markused_has_node_kind(a, .debugger_stmt) {
		enqueue('debug.Debugger.interact', mut used, mut queue)
		enqueue_debugger_custom_str_methods(tc, mut used, mut queue)
	}
	// Trace calls are injected by Cgen after AST reachability has been computed,
	// so retain their two runtime entry points whenever the debug module exists.
	enqueue('debug.before_call_hook', mut used, mut queue)
	enqueue('debug.after_call_hook', mut used, mut queue)
	if all_functions {
		for index, node_idx in body_ids {
			node := a.nodes[node_idx]
			if node.kind != .fn_decl || markused_fn_decl_is_generic_template(node, a) {
				continue
			}
			enqueue(qualify_fn(body_modules[index], node.value), mut used, mut queue)
		}
	}

	if trace_markused {
		eprintln('markused: fn_count:')
		eprintln(fn_count.str())
		eprintln('fn_with_dot:')
		eprintln(fn_with_dot.str())
		eprintln('contains2_total:')
		eprintln(contains2_total.str())
		eprintln('markused: main in fn_decls: ${'main' in fn_decls}')
		eprintln('markused: fn_decls count: ${fn_decls.len}')
		eprintln('markused: suffix_map count: ${suffix_map.len}')
		mut total_suffix_entries := 0
		for _, vals in suffix_map {
			total_suffix_entries += vals.len
		}
		eprintln('total suffix_map entries (sum of array lens):')
		eprintln(total_suffix_entries.str())
	}

	mut suffix_hits := 0
	// mut suffix_misses := 0
	mut in_cg := 0
	mut not_in_cg := 0
	mut total_callees := 0
	has_entry_main := markused_has_entry_main_indexed(a, tc)
	auto_str_skipped_fields := if reachable_runtime_helpers || !has_entry_main {
		markused_auto_str_skipped_fields(a)
	} else {
		map[u32]&MarkusedSkippedFields{}
	}
	collector := CallCollector{
		a:                     a
		tc:                    tc
		receiver_method_cache: &MarkusedMethodCache{}
		local_walk_scratch:    &MarkusedLocalWalkScratch{}
		decl_name_cache:       &DeclNameCache{}

		source_type_cache:                &SourceTypeCache{}
		default_calls:                    &StructDefaultCalls{}
		fn_decls:                         fn_decls
		fn_suffixes:                      fn_name_suffixes
		struct_decls:                     struct_decls
		const_decls:                      const_decls
		const_suffixes:                   const_name_suffixes
		const_suffixes_complete:          true
		import_contexts:                  import_contexts
		selective_alias_targets:          if detect_reachable_generics {
			markused_selective_alias_targets(tc)
		} else {
			map[u32][]u32{}
		}
		iface_param_gate:                 if detect_reachable_generics {
			markused_interface_param_gate(tc)
		} else {
			map[string]bool{}
		}
		generic_type_bases:               generic_type_bases
		generic_calls:                    if detect_reachable_generics {
			markused_generic_calls(a, tc, fn_decls)
		} else {
			unsafe { nil }
		}
		auto_str_skipped_fields:          auto_str_skipped_fields
		detect_generics:                  detect_reachable_generics
		body_checker_edges_authoritative: use_prepared && !detect_reachable_generics
	}
	// Precollect every body's call/initializer-ref lists up front (across
	// threads when available): the BFS below then only does the cheap
	// name-resolution work. Collecting inline used to serialize ~80% of
	// markused's time inside the BFS loop. Keep skipped library bodies out of
	// the eager pass, and collect reached bodies inline when there are no workers.
	if tc.verbose {
		eprintln('  [ttime] mu decl scan       ${f64(mu_sw.elapsed().microseconds()) / 1000.0:7.2f} ms')
		mu_sw.restart()
	}
	mut body_index := map[int]int{}
	mut body_results := []BodyCalls{}
	// Frontier checking discovers bodies on demand; eagerly collecting checked bodies
	// repeats work and retains call lists for declarations the queue never reaches.
	if library_body_checker == unsafe { nil } && body_ids.len >= min_eager_markused_bodies
		&& (!use_prepared || markused_can_parallel_precollect(&collector))
		&& (!tc.skips_library_bodies || a.worker_count() > 0) {
		mut checked_ids := []int{}
		mut checked_modules := []string{}
		mut checked_import_contexts := []int{}
		if tc.skips_library_bodies {
			for i, id in body_ids {
				node := a.node(flat.NodeId(id))
				file := if source := a.source_files[node.pos.id] { source.name } else { '' }
				if tc.skips_library_body_in_file(*node, file, body_modules[i]) {
					continue
				}
				checked_ids << id
				checked_modules << body_modules[i]
				checked_import_contexts << body_import_contexts[i]
			}
		}
		collect_ids := if tc.skips_library_bodies { checked_ids } else { body_ids }
		collect_modules := if tc.skips_library_bodies { checked_modules } else { body_modules }
		collect_import_contexts := if tc.skips_library_bodies {
			checked_import_contexts
		} else {
			body_import_contexts
		}
		for i, id in collect_ids {
			body_index[id] = i
		}
		body_results = precollect_body_calls(collector, collect_ids, collect_modules, collect_import_contexts)
	}
	if tc.verbose {
		eprintln('  [ttime] mu precollect      ${f64(mu_sw.elapsed().microseconds()) / 1000.0:7.2f} ms (bodies: ${body_ids.len})')
		mu_sw.restart()
	}
	needs_closure_runtime := if reachable_runtime_helpers {
		markused_has_closure_runtime_import(a)
	} else if use_prepared && prepared.needs_closure_runtime {
		true
	} else {
		markused_program_needs_closure_runtime(a, tc)
	}
	mut reachable_helper_scan := RuntimeHelpersScan{}
	mut runtime_helper_seen := []bool{}
	mut runtime_helper_stack := []flat.NodeId{cap: 128}
	mut runtime_helper_nodes := MarkusedNodeList{ nodes: []int{cap: 128} }
	if reachable_runtime_helpers {
		reachable_helper_scan.select_array_methods = true
		reachable_helper_scan.select_source_utilities = true
		reachable_helper_scan.select_map_runtime = true
		reachable_helper_scan.select_closure_runtime = true
		reachable_helper_scan.auto_str_skipped_fields = auto_str_skipped_fields
		if !needs_closure_runtime {
			reachable_helper_scan.deferred_closure_mask = []bool{len: a.nodes.len}
			for idx in tc.top_level_idx {
				node := a.nodes[idx]
				if markused_is_builtin_closure_global(a, tc, node) {
					runtime_helper_subtree(a, flat.NodeId(idx), mut reachable_helper_scan.deferred_closure_mask,
						mut runtime_helper_stack, mut runtime_helper_nodes)
					reachable_helper_scan.deferred_closure_nodes << runtime_helper_nodes.nodes[..runtime_helper_nodes.size]
				}
			}
		}
		body_nodes := runtime_helper_body_mask(a, body_ids)
		reachable_helper_scan.enqueue_nodes(a, tc, []int{}, body_nodes, '', map[string]string{}, mut used, mut queue)
		runtime_helper_seen = []bool{len: a.nodes.len}
	} else if !rt_scan_parallel && !trivial_literal_output {
		enqueue_detected_runtime_helpers(a, tc, mut used, mut queue)
	}
	if !trivial_literal_output && needs_closure_runtime {
		enqueue('closure.closure_init', mut used, mut queue)
		enqueue('closure.closure_create_with_data', mut used, mut queue)
		enqueue('closure.closure_try_destroy', mut used, mut queue)
	}
	if !trivial_literal_output {
		enqueue_function_value_selectors(a, collector, fn_decls, has_entry_main, mut used, mut queue)
	}
	// Methods used as values (`recv.method` passed as a callback) are reachable only
	// through a wrapper cgen generates later. The checker records them per enclosing
	// function in `method_values_by_fn`; they are seeded inside the BFS below (only when
	// that function is reached), so an unreachable function's method value never forces an
	// otherwise-unused specialization to be transformed/emitted.
	mut uses_generics := enqueue_initializer_calls(a, collector, fn_decls, fn_decl_lists, mut used, mut
		queue, false)
	uses_generics = enqueue_top_level_calls(a, collector, fn_decls, fn_decl_lists, has_entry_main, mut
		used, mut queue, uses_generics)
	// Interface method values and other roots can enter the queue as abstract
	// dispatch keys that have no function declaration of their own. Seed their
	// concrete implementations before the declaration-driven BFS skips those keys.
	enqueue_used_interface_dispatch_implementers(tc, mut used, mut queue, mut reachable_helper_scan.interface_dispatch)
	// Interface dispatch reachability: calling an interface method `Foo.m` may
	// dispatch to any concrete `T.m` for a type `T` that implements `Foo`. Those
	// concrete methods are only referenced from the generated dispatch switch, so
	// without this they would be pruned and produce undefined-symbol errors.
	mut iface_impls := map[u32][]u32{}
	mut checked_iface_impls := map[u32]bool{}
	iface_impls.reserve(u32(tc.interface_names.len * 2 + 16))
	checked_iface_impls.reserve(u32(tc.interface_names.len * 2 + 16))
	mut processed_nodes := []bool{len: a.nodes.len}
	mut processed_initializer_refs := map[string]bool{}
	processed_initializer_refs.reserve(u32(const_decls.len + 16))
	mut calls := []string{cap: 128}
	mut initializer_calls := []string{cap: 32}
	mut initializer_refs := []string{cap: 32}
	// Reuse one epoch table across the BFS instead of allocating and reserving
	// a fresh map for every reachable body. A callee is unique within a body
	// when its stored epoch matches the current body epoch.
	mut call_seen_epoch := map[string]u32{}
	call_seen_epoch.reserve(32_768)
	mut call_epoch := u32(0)
	// Global per-callee dedup for the body-independent parts of callee
	// processing. The same callee name (`println`, `array.get`, ...) appears in
	// thousands of bodies; its decl-alias expansion depends only on `fn_decls`
	// (fixed during the BFS) and the interface-dispatch tail only on
	// (module, callee), so each needs to run once. enqueue() is idempotent, so
	// skipping a repeat performs no fewer markings.
	mut safe_alias_done := map[string]bool{}
	mut iface_tail_done := map[string]bool{}
	// Qualified callees whose whole processing above is independent of the
	// calling module, once it ran for one caller: a declared function marked
	// directly, whose alias and interface tails are done. Any later call of one
	// only repeats marks that enqueue() ignores. Generic detection keeps per-module
	// state for these, so it does not use this.
	mut settled_callees := map[string]bool{}
	settle_callees := !collector.detect_generics && !detect_reachable_generics
	mut processed_module_calls := map[string]&ModuleCalls{}
	mut call_signature_count := tc.fn_ret_types.len
	safe_alias_done.reserve(u32(fn_decls.len))
	iface_tail_done.reserve(u32(fn_decls.len))
	if tc.verbose {
		eprintln('  [ttime] mu seeds           ${f64(mu_sw.elapsed().microseconds()) / 1000.0:7.2f} ms')
		mu_sw.restart()
	}
	mut qi := 0
	mut pending_library_names := []string{cap: 32}
	mut pending_library_ids := []int{cap: 32}
	mut pending_library_nodes := []bool{len: if library_body_checker != unsafe { nil } {
		a.nodes.len
	} else {
		0
	}}
	mut library_frontier := &types.LibraryBodyFrontier(unsafe { nil })
	mut check_library_frontiers := library_body_checker != unsafe { nil }
	mut runtime_helpers_pending := rt_scan_parallel
	for qi < queue.len || runtime_helpers_pending || pending_library_names.len > 0
		|| check_library_frontiers {
		if qi >= queue.len && pending_library_names.len > 0 {
			// The public entry point supplies its mutable checker; other callers
			// leave this pointer nil and retain the existing reachability behavior.
			mut checker := unsafe { &types.TypeChecker(library_body_checker) }
			if isnil(library_frontier) {
				library_frontier = checker.prepare_library_body_frontier()
			}
			reached := checker.check_library_body_frontier_nodes(library_frontier, pending_library_ids,
				library_check_parallel) or {
				mut checking_used := used.clone()
				for pending in pending_library_names {
					checking_used[pending] = true
				}
				checker.check_reached_library_bodies(checking_used, library_check_parallel)
			}
			if checker.errors.len > 0 {
				// Complete the conservative batch before reporting errors, as the
				// convergence path does, including bodies in a later frontier.
				error_used, error_uses_generics := mark_used_with_generic_usage(a, checker)
				checker.check_reached_library_bodies(error_used, library_check_parallel)
				return error_used, uses_generics || error_uses_generics
			}
			if reached == 0 {
				// Incomplete declaration/file metadata keeps the original collector
				// fallback instead of postponing a body indefinitely.
				check_library_frontiers = false
			}
			queue << pending_library_names
			for id in pending_library_ids {
				pending_library_nodes[id] = false
			}
			pending_library_ids.clear()
			pending_library_names.clear()
			continue
		}
		if qi >= queue.len && !runtime_helpers_pending && check_library_frontiers {
			// The convergence check also accepts ambiguous short declaration names.
			// These bodies have no exact reachable queue entry. Complete their
			// semantic checks without introducing new reachability roots.
			mut checker := unsafe { &types.TypeChecker(library_body_checker) }
			use_decl_index := convergence_index_complete && !isnil(library_frontier)
				&& a.nodes.len == convergence_node_count
				&& tc.top_level_idx.len == convergence_top_level_size
			remaining := if use_decl_index {
				unchecked_used_library_nodes(a, tc, fn_decl_lists, used)
			} else {
				checker.reached_library_body_nodes(used)
			}
			if remaining.len > 0 {
				node_count := a.nodes.len
				top_level_size := tc.top_level_idx.len
				mut recheck_reachability := false
				for id in remaining {
					if id >= processed_nodes.len || processed_nodes[id] {
						recheck_reachability = true
					}
				}
				if use_decl_index {
					_ = checker.check_library_body_frontier_nodes(library_frontier, remaining,
						library_check_parallel) or {
						checker.check_reached_library_bodies(used, library_check_parallel)
					}
				} else {
					checker.check_reached_library_bodies(used, library_check_parallel)
				}
				// Checking an unreached body only publishes its own expression types
				// and call edges. Existing reached bodies retain their analysis. New
				// arena nodes or declarations require the conservative fresh pass.
				if recheck_reachability || a.nodes.len != node_count
					|| tc.top_level_idx.len != top_level_size || checker.errors.len > 0 {
					return mark_used_with_generic_usage(a, checker)
				}
			}
			checker.library_body_convergence_complete = true
			check_library_frontiers = false
			continue
		}
		if qi >= queue.len {
			// Runtime-helper scanning starts only after semantic checking. Let the
			// ordinary reachability roots keep the caller busy while it runs, then
			// append its roots before allowing the fixed-point queue to finish.
			rt_scan_threads[0].wait()
			for helper_name in rt_scan.queue {
				// Clone into the master arena: the scan scope is freed right below.
				enqueue(helper_name.clone(), mut used, mut queue)
			}
			markused_worker_scope_free(rt_scan.scope)
			runtime_helpers_pending = false
			continue
		}
		name := queue[qi]
		qi++
		if reachable_runtime_helpers && name in ['closure.closure_init', 'closure__closure_init']
			&& !reachable_helper_scan.closure_globals_scanned {
			reachable_helper_scan.closure_globals_scanned = true
			if reachable_helper_scan.deferred_closure_nodes.len > 0 {
				reachable_helper_scan.enqueue_nodes(a, tc, reachable_helper_scan.deferred_closure_nodes,
					[]bool{}, 'closure', map[string]string{}, mut used, mut queue)
			}
		}
		if reachable_runtime_helpers && !reachable_helper_scan.map_runtime_enqueued
			&& (name == 'new_map' || markused_is_map_runtime_seed(name)) {
			reachable_helper_scan.enqueue_map_runtime(mut used, mut queue)
		}
		prev_len := queue.len
		fn_infos := fn_decl_infos_for_queue_name(name, fn_decl_lists, a, detect_reachable_generics)
		if fn_infos.len == 0 {
			not_in_cg++
			if trace_markused && qi <= 10 {
				eprintln('BFS qi=${qi.str()} name="${name}" in_cg=false')
			}
			continue
		}
		for fn_info in fn_infos {
			if trace_markused && qi <= 10 {
				eprintln('BFS qi=${qi.str()} name="${name}" in_cg=true')
			}
			in_cg++
			node_key := int(fn_info.node_id)
			if node_key < 0 || node_key >= processed_nodes.len {
				continue
			}
			if processed_nodes[node_key] {
				continue
			}
			if check_library_frontiers {
				node := a.node(fn_info.node_id)
				file := if source := a.source_files[node.pos.id] { source.name } else { '' }
				module_name := tc.file_modules[file] or { fn_info.module }
				if tc.skips_library_body_in_file(node, file, module_name) {
					if !pending_library_nodes[node_key] {
						pending_library_nodes[node_key] = true
						pending_library_ids << node_key
						pending_library_names << qualify_fn(module_name, node.value)
					}
					continue
				}
			}
			processed_nodes[node_key] = true
			if reachable_runtime_helpers {
				runtime_helper_subtree(a, fn_info.node_id, mut runtime_helper_seen, mut runtime_helper_stack, mut runtime_helper_nodes)
				if runtime_helper_nodes.size > 0 {
					reachable_helper_scan.enqueue_nodes(a, tc, runtime_helper_nodes.nodes[..runtime_helper_nodes.size], []bool{}, fn_info.module, collector.imports(fn_info.import_context), mut used, mut queue)
				}
			}
			// This function is reachable, so any methods it uses as *values* (recorded by
			// the checker per enclosing function) are reachable too -- mark them so they
			// survive pruning (cgen emits a wrapper that calls them).
			if mvs := tc.method_values_by_fn[node_key] {
				enqueue('closure.closure_init', mut used, mut queue)
				enqueue('closure.closure_create_with_data', mut used, mut queue)
				enqueue('closure.closure_try_destroy', mut used, mut queue)
				for mkey in mvs {
					enqueue(mkey, mut used, mut queue)
					lowered_mv := markused_c_name(mkey)
					if lowered_mv != mkey {
						enqueue(lowered_mv, mut used, mut queue)
					}
					mv_short := mkey.all_after_last('.')
					if cands := suffix_map[mv_short] {
						for cand in cands {
							if cand == mkey || cand.ends_with('.${mkey}') {
								enqueue(cand, mut used, mut queue)
							}
						}
					}
				}
			}
			calls.clear()
			initializer_refs.clear()
			// Direct source-level call and function-value edges are authoritative:
			// the checker recorded them with the exact file/import/type context.
			// The body collector below remains responsible for implicit helpers
			// introduced by lowering and for unresolved compatibility fallbacks.
			for dependency in tc.direct_dependency_ids(node_key) {
				calls << tc.frozen_symbol_name(dependency)
			}
			// The checker's dependency edges only cover source-level calls. Implicit
			// helpers introduced by lowering (array bounds checks, `array__get`,
			// `v_ni_index`, ...) are discovered solely by the body collector, so it must
			// run for correctness regardless of generics detection; only the generics
			// bookkeeping below is gated.
			if body_i := body_index[node_key] {
				body := body_results[body_i]
				calls << body.calls
				initializer_refs << body.refs
				if detect_reachable_generics {
					uses_generics = uses_generics || body.uses_generics
				}
			} else {
				// Not precollected (no eager pass, or not part of that decl set) —
				// collect inline.
				node := a.node(fn_info.node_id)
				body := collector.collect_body_with_generic_detection(node, fn_info.module, collector.imports(fn_info.import_context), detect_reachable_generics && !uses_generics)
				calls << body.calls
				initializer_refs << body.refs
				if detect_reachable_generics {
					uses_generics = uses_generics || body.uses_generics
				}
			}
			mut unique_call_count := calls.len
			if detect_reachable_generics {
				// The body collector can add compatibility fallbacks beside checker
				// edges. Self-host uses only the checker's already-deduplicated symbol
				// ids, so it can skip this per-callee hash pass entirely.
				call_epoch++
				unique_call_count = 0
				for call in calls {
					if call_seen_epoch[call] == call_epoch {
						continue
					}
					call_seen_epoch[call] = call_epoch
					calls[unique_call_count] = call
					unique_call_count++
				}
			}
			for initializer_ref in initializer_refs {
				enqueue_initializer_ref_calls(a, collector, fn_decls, fn_decl_lists, initializer_ref, mut
					processed_initializer_refs, mut initializer_calls, mut used, mut queue)
			}
			total_callees += unique_call_count
			// Complete callee resolution depends only on its name and caller
			// module. A short-name fallback suppressed by a qualified sibling
			// remains uncached: another body may need its broader candidate set.
			if call_signature_count != tc.fn_ret_types.len {
				processed_module_calls.clear()
				call_signature_count = tc.fn_ret_types.len
			}
			mut module_calls := processed_module_calls[fn_info.module] or {
				fresh := &ModuleCalls{}
				processed_module_calls[fn_info.module] = fresh
				fresh
			}
			for call_idx in 0 .. unique_call_count {
				callee := calls[call_idx]
				if settle_callees && settled_callees[callee] {
					continue
				}
				if module_calls.names[callee] {
					continue
				}
				if !valid_symbol_name(callee) {
					module_calls.names[callee] = true
					continue
				}
				if collector.detect_generics
					&& collector.is_generic_index_overload_call(callee, fn_info.module) {
					module_calls.names[callee] = true
					enqueue(callee, mut used, mut queue)
					lowered := markused_c_name(callee)
					if lowered != callee {
						enqueue(lowered, mut used, mut queue)
					}
					uses_generics = true
					continue
				}
				if markused_generated_c_helper_name(callee) {
					module_calls.names[callee] = true
					for alias in markused_generated_c_helper_aliases(callee) {
						enqueue(alias, mut used, mut queue)
					}
					continue
				}
				mut found_direct := false
				// Marked as a declared function, independently of the caller.
				mut found_declared := false
				if callee_info := fn_decls[callee] {
					found_direct = true
					found_declared = true
					if enqueue(callee, mut used, mut queue) {
						if trace_markused && qi == 1 {
							eprintln('main: all_fns hit: "${callee}"')
						}
					}
					if !safe_alias_done[callee] {
						safe_alias_done[callee] = true
						add_safe_decl_alias(callee, callee_info, fn_info.module, fn_decl_lists, a, mut
							used, mut queue)
					}
				} else if callee in tc.fn_ret_types {
					found_direct = true
					found_declared = true
					if enqueue(callee, mut used, mut queue) {
						if trace_markused && qi == 1 {
							eprintln('main: resolved hit: "${callee}"')
						}
					}
				} else if markused_is_interface_dispatch_call(callee, fn_info.module, tc) {
					found_direct = true
					enqueue(callee, mut used, mut queue)
					lowered := markused_c_name(callee)
					if lowered != callee {
						enqueue(lowered, mut used, mut queue)
					}
				}
				if !found_direct {
					short := callee.all_after_last('.')
					qcallee := qualify_fn(fn_info.module, callee)
					if qcallee == callee || call_seen_epoch[qcallee] != call_epoch {
						module_calls.names[callee] = true
						if suffix_candidates := suffix_map[short] {
							for candidate in suffix_candidates {
								if candidate in fn_decls || candidate in tc.fn_ret_types {
									if enqueue(candidate, mut used, mut queue) {
										suffix_hits++
									}
								}
							}
						}
					}
				} else {
					module_calls.names[callee] = true
				}
				if !detect_reachable_generics {
					module_calls.names[callee] = true
				}
				// Qualified call names resolve independently of the caller's module.
				// Reuse the call itself as the cache key and avoid allocating the
				// composite module key on the overwhelmingly common exact path.
				iface_tail_key := if callee.index_u8(`.`) >= 0 {
					callee
				} else {
					'${fn_info.module}\x01${callee}'
				}
				if settle_callees && found_declared && iface_tail_key == callee {
					settled_callees[callee] = true
				}
				if !iface_tail_done[iface_tail_key] {
					iface_tail_done[iface_tail_key] = true
					mut iface_callee := callee
					if normalized := interface_dispatch_dotted_name(callee, tc) {
						iface_callee = normalized
					}
					if iface_callee.contains('.') {
						recv := flat.source_type_id(iface_callee.all_before_last('.'))
						method := iface_callee.all_after_last('.')
						ensure_iface_impls(recv, fn_info.module, tc, mut iface_impls, mut checked_iface_impls)
						mut recv_iface := u32(0)
						if iface_name := interface_name_for_receiver(recv, fn_info.module, tc) {
							recv_iface = iface_name
							// Keep the dispatch stub (`Iface__method`) the transform will
							// call; the call site may name the interface through an alias
							// (`Expr.name` for `type Expr = Node`), which cgen's used-fn
							// filter does not resolve back to the interface key.
							enqueue('${flat.source_type_name(iface_name)}.${method}', mut used, mut queue)
						}
						if impls := iface_impls[recv] {
							for impl in impls {
								impl_method := tc.concrete_method_signature_key(impl, method) or {
									'${flat.source_type_name(impl)}.${method}'
								}
								enqueue(impl_method, mut used, mut queue)
								short_impl := '${impl_method.all_before_last('.').all_after_last('.')}.${method}'
								if short_impl != impl_method {
									enqueue(short_impl, mut used, mut queue)
								}
								if recv_iface != 0 {
									enqueue_implicit_interface_str_helpers_for_impl(recv_iface, method, impl, tc, mut used, mut queue)
								}
							}
						}
					}
				}
			}
		}
		new_added := queue.len - prev_len
		if trace_markused && qi <= 10 {
			eprintln('  -> added ${new_added.str()} new entries, queue now ${queue.len.str()}')
		}
	}
	if collector.detect_generics && !uses_generics {
		uses_generics = collector.emitted_type_declarations_use_generics()
	}
	if trace_markused {
		eprintln('total_callees:')
		eprintln(total_callees.str())
		eprintln('markused: in_cg:')
		eprintln(in_cg.str())
		eprintln('not_in_cg:')
		eprintln(not_in_cg.str())
		eprintln('queue.len:')
		eprintln(queue.len.str())
		eprintln('markused: suffix_hits:')
		eprintln(suffix_hits.str())
		eprintln('markused: total used: ${used.len}')
	}
	if tc.verbose {
		eprintln('  [ttime] mu bfs+tail        ${f64(mu_sw.elapsed().microseconds()) / 1000.0:7.2f} ms')
	}
	return used, uses_generics
}

// InterfaceDispatchScan remembers the dispatch keys of every abstract interface method,
// and which methods already had their implementers enqueued. The checker is fixed during
// markused and `used` only grows, so the keys are built once per scan.
struct InterfaceDispatchScan {
mut:
	processed            map[string]bool
	groups               []InterfaceDispatchGroup
	ierror_keys          []string
	built                bool
	ierror_keys_enqueued bool
	// used.len after the last call that found nothing new; an unchanged len means an unchanged set.
	settled_used_len int = -1
	// Every dispatch key of every method. A method can only become reached
	// through one of these names entering `used`.
	dispatch_keys map[string]bool
	// used.len and queue.len when the last full scan started, or -1 before it.
	scanned_used_len  int = -1
	scanned_queue_len int
}

struct InterfaceDispatchGroup {
	name_id   u32
	is_ierror bool
mut:
	methods []InterfaceDispatchMethod
	pending int
}

struct InterfaceDispatchMethod {
	name      string
	key       string
	c_key     string
	short_key string
mut:
	done bool
}

fn (mut scan InterfaceDispatchScan) build(tc &types.TypeChecker) {
	scan.built = true
	for iface_name_id in tc.interface_names.keys() {
		iface_name := flat.source_type_name(iface_name_id)
		is_ierror := markused_is_ierror_interface_name(iface_name_id)
		if is_ierror {
			for method in ['msg', 'code'] {
				dispatch_key := '${iface_name}.${method}'
				scan.ierror_keys << dispatch_key
				scan.ierror_keys << markused_c_name(dispatch_key)
			}
		}
		methods := tc.interface_abstract_method_names(iface_name_id)
		if methods.len == 0 {
			continue
		}
		short_name := iface_name.all_after_last('.')
		mut group := InterfaceDispatchGroup{
			name_id:   iface_name_id
			is_ierror: is_ierror
			methods:   []InterfaceDispatchMethod{cap: methods.len}
			pending:   methods.len
		}
		for method in methods {
			dispatch_key := '${iface_name}.${method}'
			dispatch_method := InterfaceDispatchMethod{
				name:      method
				key:       dispatch_key
				c_key:     markused_c_name(dispatch_key)
				short_key: '${short_name}.${method}'
			}
			scan.dispatch_keys[dispatch_method.key] = true
			scan.dispatch_keys[dispatch_method.c_key] = true
			scan.dispatch_keys[dispatch_method.short_key] = true
			group.methods << dispatch_method
		}
		scan.groups << group
	}
}

// Each used dispatch method expands a fixed declaration graph during markused.
// Remember completed methods while still discovering methods reached by later bodies.
fn enqueue_used_interface_dispatch_implementers(tc &types.TypeChecker, mut used map[string]bool, mut queue []string, mut scan InterfaceDispatchScan) bool {
	if !scan.built {
		scan.build(tc)
	}
	start_len := used.len
	if start_len == scan.settled_used_len {
		return false
	}
	// A scan finds a method only once one of its dispatch keys is in `used`;
	// keys that were there when the last scan started were all seen by it. When
	// every name added since was also queued, as `enqueue` does, look for a
	// dispatch key among those queued names before scanning every method again.
	if scan.scanned_used_len >= 0
		&& start_len - scan.scanned_used_len == queue.len - scan.scanned_queue_len {
		mut reached_dispatch := false
		for i in scan.scanned_queue_len .. queue.len {
			if queue[i] in scan.dispatch_keys {
				reached_dispatch = true
				break
			}
		}
		if !reached_dispatch {
			// What a full scan from here would have found: nothing to add.
			scan.settled_used_len = start_len
			scan.scanned_used_len = start_len
			scan.scanned_queue_len = queue.len
			return false
		}
	}
	scan.scanned_used_len = start_len
	scan.scanned_queue_len = queue.len
	mut added := false
	for mut group in scan.groups {
		if group.pending == 0 {
			continue
		}
		mut used_methods := []int{}
		for i, method in group.methods {
			if method.done || scan.processed[method.key] {
				continue
			}
			if method.key in used || method.c_key in used || method.short_key in used {
				used_methods << i
			}
		}
		if used_methods.len == 0 {
			continue
		}
		impls := if group.is_ierror {
			tc.ierror_impl_names()
		} else {
			markused_interface_impl_names(group.name_id, tc)
		}
		if impls.len == 0 {
			continue
		}
		for i in used_methods {
			method_name := group.methods[i].name
			for impl in impls {
				for alias in interface_implementer_method_aliases(impl, method_name, tc) {
					if enqueue(alias, mut used, mut queue) {
						added = true
					}
				}
				enqueue_implicit_interface_str_helpers_for_impl(group.name_id, method_name,
					impl, tc, mut used, mut queue)
			}
			scan.processed[group.methods[i].key] = true
			group.methods[i].done = true
			group.pending--
		}
	}
	scan.settled_used_len = if used.len == start_len { start_len } else { -1 }
	return added
}

fn markused_interface_impl_names(iface_name_id u32, tc &types.TypeChecker) []u32 {
	return tc.pre_transform_interface_impl_names(iface_name_id) or {
		tc.interface_impl_names(iface_name_id)
	}
}

fn interface_implementer_method_aliases(impl_id u32, method string, tc &types.TypeChecker) []string {
	impl_method := tc.concrete_method_signature_key(impl_id, method) or { '${flat.source_type_name(impl_id)}.${method}' }
	mut aliases := [impl_method]
	lowered := markused_c_name(impl_method)
	if lowered != impl_method {
		aliases << lowered
	}
	short_impl := '${impl_method.all_before_last('.').all_after_last('.')}.${method}'
	if short_impl != impl_method {
		aliases << short_impl
	}
	return aliases
}

fn markused_is_interface_dispatch_call(name string, cur_module string, tc &types.TypeChecker) bool {
	if !name.contains('.') {
		return false
	}
	recv := name.all_before_last('.')
	method := name.all_after_last('.')
	iface_name := interface_name_for_receiver(flat.source_type_id(recv), cur_module, tc) or { return false }
	return tc.interface_method_signature_key(iface_name, method) != none
}

// ensure_iface_impls records source spelling implementers in the checker's stable order.
fn ensure_iface_impls(recv_id u32, cur_module string, tc &types.TypeChecker, mut iface_impls map[u32][]u32, mut checked map[u32]bool) {
	if recv_id == 0 {
		return
	}
	iface_name := interface_name_for_receiver(recv_id, cur_module, tc) or { return }
	if iface_name in checked {
		return
	}
	checked[iface_name] = true
	if recv_id != iface_name {
		checked[recv_id] = true
	}
	mut impls := []u32{}
	if markused_is_ierror_interface_name(iface_name) {
		impls = tc.ierror_impl_names()
	} else {
		// Structs plus alias implementers, from the same list cgen assigns
		// dispatch ids over. These are source spelling IDs, not dispatch ids.
		impls = markused_interface_impl_names(iface_name, tc)
	}
	iface_impls[recv_id] = impls
	if iface_name != recv_id {
		iface_impls[iface_name] = impls
		checked[iface_name] = true
	}
}

fn interface_name_for_receiver(recv_id u32, cur_module string, tc &types.TypeChecker) ?u32 {
	if recv_id in tc.interface_names {
		return recv_id
	}
	if target_id := tc.type_aliases.type_id(recv_id) {
		if target_id in tc.interface_names {
			return target_id
		}
		qtarget := tc.qualify_type_name(target_id)
		if qtarget in tc.interface_names {
			return qtarget
		}
	}
	if flat.source_type_shape(recv_id).has(.dotted) {
		return none
	}
	if cur_module.len > 0 && cur_module != 'main' && cur_module != 'builtin' {
		qname := markused_qualify_source_type(cur_module, recv_id)
		if qname in tc.interface_names {
			return qname
		}
		if target_id := tc.type_aliases.type_id(qname) {
			if target_id in tc.interface_names {
				return target_id
			}
			qtarget := tc.qualify_type_name(target_id)
			if qtarget in tc.interface_names {
				return qtarget
			}
		}
	}
	return none
}

fn interface_dispatch_dotted_name(name string, tc &types.TypeChecker) ?string {
	if name.contains('.') {
		return name
	}
	if !name.contains('__') {
		return none
	}
	recv_c := name.all_before_last('__')
	method := name.all_after_last('__')
	if recv_c.len == 0 || method.len == 0 {
		return none
	}
	recv := recv_c.replace('__', '.')
	recv_id := flat.source_type_existing_id(recv) or { return none }
	if recv_id in tc.interface_names {
		return '${recv}.${method}'
	}
	if target_source_id := tc.type_aliases.type_id(recv_id) {
		if target_source_id in tc.interface_names {
			return '${flat.source_type_name(target_source_id)}.${method}'
		}
		qtarget_id := tc.qualify_type_name(target_source_id)
		if qtarget_id in tc.interface_names {
			return '${flat.source_type_name(qtarget_id)}.${method}'
		}
	}
	return none
}

fn markused_is_ierror_interface_name(name_id u32) bool {
	return name_id == markused_source_type_ierror || name_id == markused_source_type_builtin_ierror
}

// add_suffix_candidate updates add suffix candidate state for markused.
fn add_suffix_candidate(mut suffix_map map[string][]string, short string, name string) {
	if !valid_symbol_name(short) || !valid_symbol_name(name) {
		return
	}
	mut candidates := suffix_map[short] or { []string{} }
	candidates << name
	suffix_map[short] = candidates
}

fn add_candidate_suffix(mut suffixes map[string]bool, name string) {
	if !valid_symbol_name(name) {
		return
	}
	suffixes[name.all_after_last('.')] = true
}

fn markused_fn_decl_is_generic_template(node flat.Node, a &flat.FlatAst) bool {
	if node.generic_params().len > 0 || node.value.contains('[')
		|| flat.source_type_query(node.typ).has(.generic_word) {
		return true
	}
	for i in 0 .. node.children_count {
		child := a.child_node(&node, i)
		if child.kind == .param && flat.source_type_query(child.typ).has(.generic_word) {
			return true
		}
	}
	return false
}

fn add_fn_decl_info(mut fn_decls map[string]FnDeclInfo, mut fn_decl_lists map[string][]FnDeclInfo, name string, info FnDeclInfo) {
	fn_decls[name] = info
	mut infos := fn_decl_lists[name] or { []FnDeclInfo{} }
	infos << info
	fn_decl_lists[name] = infos
}

fn fn_decl_infos_for_queue_name(name string, fn_decl_lists map[string][]FnDeclInfo, a &flat.FlatAst, allow_generic_specialization bool) []FnDeclInfo {
	infos := fn_decl_lists[name] or {
		if !allow_generic_specialization {
			return []FnDeclInfo{}
		}
		return fn_decl_infos_for_generic_specialization_name(name, fn_decl_lists, a)
	}
	if infos.len <= 1 {
		return infos
	}
	mut exact_infos := []FnDeclInfo{}
	for info in infos {
		node := a.node(info.node_id)
		if fn_decl_key_is_exact_for_info(name, node.value, info.module) {
			exact_infos << info
		}
	}
	return exact_infos
}

fn fn_decl_infos_for_generic_specialization_name(name string, fn_decl_lists map[string][]FnDeclInfo, a &flat.FlatAst) []FnDeclInfo {
	for candidate in markused_generic_specialization_decl_keys(name) {
		infos := fn_decl_lists[candidate] or { continue }
		mut generic_infos := []FnDeclInfo{}
		for info in infos {
			node := a.node(info.node_id)
			if markused_fn_decl_is_generic_template(node, a) {
				generic_infos << info
			}
		}
		if generic_infos.len > 0 {
			return generic_infos
		}
	}

	return []FnDeclInfo{}
}

fn markused_generic_specialization_decl_keys(name string) []string {
	mut keys := []string{}
	if name.contains('__') {
		parts := name.split('__')
		for i in 0 .. parts.len - 1 {
			markused_add_generic_specialization_keys(parts, i, mut keys)
		}
		return keys
	}
	markused_add_generic_specialization_key(name, mut keys)
	return keys
}

fn markused_add_generic_specialization_keys(parts []string, index int, mut keys []string) {
	segment := parts[index]
	if !segment.contains('_') {
		return
	}
	mut base := segment
	for base.contains('_') {
		base = base.all_before_last('_')
		if base.len == 0 {
			return
		}
		mut candidate_parts := parts.clone()
		candidate_parts[index] = base
		markused_push_unique_key(candidate_parts.join('__'), mut keys)
	}
}

fn markused_add_generic_specialization_key(name string, mut keys []string) {
	if !name.contains('_') {
		return
	}
	mut base := name
	for base.contains('_') {
		base = base.all_before_last('_')
		if base.len == 0 {
			return
		}
		markused_push_unique_key(base, mut keys)
	}
}

fn markused_push_unique_key(key string, mut keys []string) {
	if key.len == 0 || key in keys {
		return
	}
	keys << key
}

fn fn_decl_key_is_exact_for_info(name string, decl_name string, module_name string) bool {
	qname := qualify_fn(module_name, decl_name)
	if name == qname {
		return true
	}
	lowered := markused_c_name(qname)
	return lowered != qname && name == lowered
}

// add_safe_decl_alias enqueues the qualified and C names of the declaration that
// `callee`, a call in `caller_module`, stands for when the call names it by a key
// that is not its own, as the short name `Parser.expr` of `othermod.Parser.expr`.
fn add_safe_decl_alias(callee string, callee_info FnDeclInfo, caller_module string, fn_decl_lists map[string][]FnDeclInfo, a &flat.FlatAst, mut used map[string]bool, mut queue []string) {
	info := markused_alias_decl_info(callee, callee_info, caller_module, fn_decl_lists)
	alias := a.node(info.node_id).value
	if fn_decl_key_is_exact_for_info(callee, alias, info.module) {
		return
	}
	mut aliases := []string{cap: 3}
	aliases << alias
	qalias := qualify_fn(info.module, alias)
	if qalias != alias {
		aliases << qalias
	}
	lowered := markused_c_name(qalias)
	if lowered != qalias {
		aliases << lowered
	}
	for candidate in aliases {
		if candidate != callee {
			enqueue(candidate, mut used, mut queue)
		}
	}
}

// markused_alias_decl_info returns the declaration that `callee`, called in
// `caller_module`, means among those registered under that key: the one of the
// caller's own module, where there is one. `fallback`, the declaration registered
// last, can be a homonym of another module: `Parser.expr` names a method of `main`
// and, by its short name, one of an imported module that declares a `Parser` too.
fn markused_alias_decl_info(callee string, fallback FnDeclInfo, caller_module string, fn_decl_lists map[string][]FnDeclInfo) FnDeclInfo {
	infos := fn_decl_lists[callee] or { return fallback }
	if infos.len > 1 {
		for info in infos {
			if markused_is_same_module(info.module, caller_module) {
				return info
			}
		}
	}
	return fallback
}

fn markused_is_same_module(a string, b string) bool {
	return a == b || (a in ['', 'main'] && b in ['', 'main'])
}

fn markused_is_unqualified_receiver_method_name(name string) bool {
	if !name.contains('.') || name.all_before('.').contains('.') {
		return false
	}
	receiver := name.all_before('.')
	return receiver.len > 0 && receiver[0] >= `A` && receiver[0] <= `Z`
}

// valid_symbol_name supports valid symbol name handling for markused.
fn valid_symbol_name(name string) bool {
	return name.len > 0 && name.len < 512
}

fn markused_generated_c_helper_name(name string) bool {
	return name in ['string__plus', 'v3_c_lit', 'array__get', 'sum_type_index',
		'sum_type_index_resolved', 'FlatGen.sum_type_index', 'FlatGen.sum_type_index_resolved',
		'c.FlatGen.sum_type_index', 'c.FlatGen.sum_type_index_resolved',
		'v.gen.c.FlatGen.sum_type_index', 'v.gen.c.FlatGen.sum_type_index_resolved',
		'c__FlatGen__sum_type_index', 'c__FlatGen__sum_type_index_resolved']
}

fn markused_generated_c_helper_aliases(name string) []string {
	if name.contains('sum_type_index_resolved') {
		return markused_unique_helper_aliases([
			name,
			'sum_type_index_resolved',
			'FlatGen.sum_type_index_resolved',
			'c.FlatGen.sum_type_index_resolved',
			'v.gen.c.FlatGen.sum_type_index_resolved',
			'c__FlatGen__sum_type_index_resolved',
		])
	}
	if name.contains('sum_type_index') {
		return markused_unique_helper_aliases([
			name,
			'sum_type_index',
			'FlatGen.sum_type_index',
			'c.FlatGen.sum_type_index',
			'v.gen.c.FlatGen.sum_type_index',
			'c__FlatGen__sum_type_index',
		])
	}
	return [name]
}

fn markused_unique_helper_aliases(names []string) []string {
	mut result := []string{cap: names.len}
	for name in names {
		if name.len > 0 && name !in result {
			result << name
		}
	}
	return result
}

// markused_clone_bool_map returns a value clone even when the source is passed
// from a `mut map` parameter. v3 self-host cgen otherwise infers a `map*` local
// for `param.clone()` in a few recursive markused scanners.
fn markused_clone_bool_map(src map[string]bool) map[string]bool {
	return src.clone()
}

fn markused_clone_local_types(src flat.TypeTextBindings) flat.TypeTextBindings {
	return src.clone()
}

fn markused_decl_type_snapshot(src flat.TypeTextBindings, lhs_count int) flat.TypeTextBindings {
	if lhs_count == 1 {
		return src
	}
	return src.clone()
}

// enqueue_initializer_calls supports enqueue initializer calls handling for markused.
fn enqueue_initializer_calls(a &flat.FlatAst, collector CallCollector, fn_decls map[string]FnDeclInfo, fn_decl_lists map[string][]FnDeclInfo, mut used map[string]bool, mut queue []string, initial_uses_generics bool) bool {
	mut cur_module := ''
	mut imports := map[string]string{}
	mut calls := []string{cap: 32}
	mut uses_generics := initial_uses_generics
	for node_idx in collector.tc.top_level_idx {
		node := a.nodes[node_idx]
		match node.kind {
			.file {
				cur_module = ''
				imports = markused_top_level_file_imports(a, node)
			}
			.module_decl {
				cur_module = node.value
			}
			.import_decl {
				imports[node.typ_text()] = node.value
			}
			.const_decl, .global_decl {
				for i in 0 .. node.children_count {
					field := a.child_node(&node, i)
					if field.children_count == 0 {
						continue
					}
					calls.clear()
					if uses_generics || !collector.detect_generics {
						collector.collect_calls(field, cur_module, imports, '', u32(0), mut calls)
					} else {
						uses_generics = collector.collect_calls_with_generic_usage(field, cur_module, imports, '', u32(0), mut calls)
					}
					for callee in calls {
						enqueue_initializer_callee(callee, cur_module, fn_decls, fn_decl_lists, a, mut
							used, mut queue)
					}
				}
			}
			else {}
		}
	}
	return uses_generics
}

fn enqueue_initializer_ref_calls(a &flat.FlatAst, collector CallCollector, fn_decls map[string]FnDeclInfo, fn_decl_lists map[string][]FnDeclInfo, initializer_ref string, mut processed map[string]bool, mut calls []string, mut used map[string]bool, mut queue []string) {
	if initializer_ref in processed {
		return
	}
	info := collector.const_decls[initializer_ref] or { return }
	processed[initializer_ref] = true
	calls.clear()
	collector.collect_calls(a.node(info.expr_id), info.module, collector.imports(info.import_context), '', u32(0), mut calls)
	for callee in calls {
		enqueue_initializer_callee(callee, info.module, fn_decls, fn_decl_lists, a, mut used, mut
			queue)
	}
}

fn enqueue_initializer_callee(callee string, caller_module string, fn_decls map[string]FnDeclInfo, fn_decl_lists map[string][]FnDeclInfo, a &flat.FlatAst, mut used map[string]bool, mut queue []string) {
	if !valid_symbol_name(callee) {
		return
	}
	if callee_info := fn_decls[callee] {
		enqueue(callee, mut used, mut queue)
		add_safe_decl_alias(callee, callee_info, caller_module, fn_decl_lists, a, mut used, mut
			queue)
	} else {
		enqueue(callee, mut used, mut queue)
	}
}

fn enqueue_implicit_global_container_roots(a &flat.FlatAst, tc &types.TypeChecker, fn_decls map[string]FnDeclInfo, fn_decl_lists map[string][]FnDeclInfo, mut used map[string]bool, mut queue []string) {
	mut needs_array := false
	mut needs_map := false
	for node_idx in tc.top_level_idx {
		node := a.nodes[node_idx]
		if node.kind != .global_decl {
			continue
		}
		for i in 0 .. node.children_count {
			field := a.child_node(&node, i)
			if field.children_count > 0 {
				continue
			}
			mut typ := tc.parse_type(field.typ)
			for _ in 0 .. 32 {
				if typ is types.Alias {
					typ = typ.base_type
					continue
				}
				break
			}
			if typ is types.Array {
				needs_array = true
			} else if typ is types.Map {
				needs_map = true
			}
		}
	}
	if needs_array {
		enqueue_initializer_callee('__new_array', 'builtin', fn_decls, fn_decl_lists, a, mut used, mut
			queue)
	}
	if needs_map {
		for helper in ['new_map', 'map_hash_string', 'map_eq_string', 'map_clone_string',
			'map_free_string', 'map_hash_int_1', 'map_hash_int_2', 'map_hash_int_4', 'map_hash_int_8',
			'map_hash_int_16', 'map_eq_int_1', 'map_eq_int_2', 'map_eq_int_4', 'map_eq_int_8',
			'map_eq_int_16', 'map_clone_int_1', 'map_clone_int_2', 'map_clone_int_4', 'map_clone_int_8',
			'map_clone_int_16', 'map_free_nop'] {
			enqueue_initializer_callee(helper, 'builtin', fn_decls, fn_decl_lists, a, mut used, mut
				queue)
		}
	}
}

fn markused_module_has_reachable_initializer(module_name string, reachable_modules map[string]bool) bool {
	if module_name.len == 0 {
		return '' in reachable_modules || 'main' in reachable_modules
	}
	return module_name in reachable_modules
}

fn markused_reachable_modules(a &flat.FlatAst, tc &types.TypeChecker) map[string]bool {
	mut module_imports := map[string][]string{}
	mut roots := []string{}
	mut has_user_root := false
	file_modules := markused_top_level_file_modules(a, tc)
	for file_idx in tc.top_level_idx {
		file_node := &a.nodes[file_idx]
		if file_node.kind != .file {
			continue
		}
		module_name := file_modules[file_node.value] or {
			markused_top_level_file_module_name(a, file_node)
		}
		if file_idx >= a.user_code_start
			&& (!markused_file_is_vlib(file_node.value) || !has_user_root) {
			has_user_root = true
			roots << module_name
			if module_name.len == 0 {
				roots << 'main'
			}
		}
		mut imports := module_imports[module_name] or { []string{} }
		if file_idx >= a.user_code_start || !markused_file_is_test(file_node.value) {
			for i in 0 .. file_node.children_count {
				child := a.child_node(file_node, i)
				if child.kind == .import_decl && child.value.len > 0 {
					imports << child.value
				}
			}
		}
		module_imports[module_name] = imports
	}
	roots << ''
	roots << 'main'
	roots << 'builtin'
	mut reachable := map[string]bool{}
	mut queue := []string{}
	for root in roots {
		if root in reachable {
			continue
		}
		reachable[root] = true
		queue << root
	}
	for qi := 0; qi < queue.len; qi++ {
		module_name := queue[qi]
		for imported in module_imports[module_name] or { []string{} } {
			if imported in reachable {
				continue
			}
			reachable[imported] = true
			queue << imported
		}
	}
	return reachable
}

fn markused_top_level_file_modules(a &flat.FlatAst, tc &types.TypeChecker) map[string]string {
	mut modules := map[string]string{}
	mut current_file := ''
	for node_idx in tc.top_level_idx {
		node := a.nodes[node_idx]
		match node.kind {
			.file {
				current_file = node.value
			}
			.module_decl {
				if current_file.len > 0 {
					modules[current_file] = node.value
				}
			}
			else {}
		}
	}
	return modules
}

fn markused_file_is_vlib(file string) bool {
	return file.contains('/vlib/') || file.starts_with('vlib/') || file.contains('\\vlib\\')
		|| file.starts_with('vlib\\')
}

// markused_file_is_test defers to pref, so every spelling the driver treats as a test
// (including architecture-qualified ones such as `foo_test.arm64.v`) is treated the same here.
// pref reports `_test.js.v` as a test for no backend while V3 has no JS backend, so that
// spelling is still matched directly.
fn markused_file_is_test(file string) bool {
	return pref.is_test_file_for_backend(file, 'c') || file.ends_with('_test.js.v')
}

fn enqueue_top_level_calls(a &flat.FlatAst, collector CallCollector, fn_decls map[string]FnDeclInfo, fn_decl_lists map[string][]FnDeclInfo, has_entry_main bool, mut used map[string]bool, mut queue []string, initial_uses_generics bool) bool {
	if has_entry_main {
		return initial_uses_generics
	}
	mut calls := []string{cap: 32}
	mut uses_generics := initial_uses_generics
	for file_idx, file_node in a.nodes {
		if !markused_should_scan_top_level_file(a, file_idx, file_node) {
			continue
		}
		module_name := markused_top_level_file_module_name(a, file_node)
		file_imports := markused_top_level_file_imports(a, file_node)
		mut local_values := map[string]bool{}
		mut local_types := collector.new_local_types()
		for i in 0 .. file_node.children_count {
			child_id := a.child(&file_node, i)
			if int(child_id) < a.user_code_start {
				continue
			}
			child := a.node(child_id)
			if !markused_is_top_level_stmt(child) {
				continue
			}
			calls.clear()
			collector.collect_top_level_stmt_calls(child_id, module_name, file_imports, mut local_values, mut local_types, mut calls)
			if collector.detect_generics && !uses_generics
				&& collector.node_tree_uses_generics(child_id, module_name, file_imports) {
				uses_generics = true
			}
			for callee in calls {
				if callee_info := fn_decls[callee] {
					enqueue(callee, mut used, mut queue)
					add_safe_decl_alias(callee, callee_info, module_name, fn_decl_lists, a, mut used, mut
						queue)
				} else {
					enqueue(callee, mut used, mut queue)
				}
			}
		}
	}
	return uses_generics
}

fn markused_has_entry_main_indexed(a &flat.FlatAst, tc &types.TypeChecker) bool {
	mut cur_module := ''
	for node_idx in tc.top_level_idx {
		node := a.nodes[node_idx]
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

fn markused_has_entry_main(a &flat.FlatAst) bool {
	mut cur_module := ''
	for node in a.nodes {
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

fn markused_should_scan_top_level_file(a &flat.FlatAst, file_idx int, file_node flat.Node) bool {
	if file_idx < a.user_code_start || file_node.kind != .file || file_node.children_count == 0 {
		return false
	}
	module_name := markused_top_level_file_module_name(a, file_node)
	return module_name.len == 0 || module_name == 'main'
}

fn markused_top_level_file_module_name(a &flat.FlatAst, file_node flat.Node) string {
	for i in 0 .. file_node.children_count {
		child := a.child_node(&file_node, i)
		if child.kind == .module_decl {
			return child.value
		}
	}
	return ''
}

fn markused_top_level_file_imports(a &flat.FlatAst, file_node flat.Node) map[string]string {
	mut imports := map[string]string{}
	for i in 0 .. file_node.children_count {
		child := a.child_node(&file_node, i)
		if child.kind == .import_decl {
			imports[child.typ_text()] = child.value
		}
	}
	return imports
}

fn markused_is_top_level_stmt(node flat.Node) bool {
	return match node.kind {
		.expr_stmt, .assign, .decl_assign, .global_decl, .selector_assign, .index_assign,
		.for_stmt, .for_in_stmt, .if_expr, .match_stmt, .assert_stmt, .defer_stmt, .block {
			true
		}
		else {
			false
		}
	}
}

// enqueue supports enqueue handling for markused.
struct RtHelpersScanArgs {
	a  &flat.FlatAst
	tc &types.TypeChecker
mut:
	enabled bool
	queue   []string
	scope   voidptr
}

fn markused_rt_helpers_thread(mut args RtHelpersScanArgs) {
	if !args.enabled {
		return
	}
	// Everything the scan allocates lives in a detached scope: the spawn
	// wrapper frees the thread's own arena at exit, so results must survive in
	// a scope the master frees after replaying (see fixed_storage_scan_thread).
	scope := markused_worker_scope_begin(true)
	// A fresh local `used` makes enqueue_detected_runtime_helpers collect every
	// name it would have enqueued (already-seeded names are skipped again when
	// the master replays the queue through enqueue, which is idempotent).
	mut used := map[string]bool{}
	enqueue_detected_runtime_helpers(args.a, args.tc, mut used, mut args.queue)
	args.scope = scope
	markused_worker_scope_leave(scope)
}

fn par_markused_seeds_enabled() bool {
	$if v3_no_parallel ? {
		// The runtime-helper scan thread queries a checker fork beside the main
		// thread; a serial build keeps it on the main thread.
		return false
	}
	return os.getenv('V3_NO_PAR_MU_SEEDS') == ''
}

fn enqueue(name string, mut used map[string]bool, mut queue []string) bool {
	if !valid_symbol_name(name) {
		return false
	}
	if name in used {
		return false
	}
	used[name] = true
	queue << name
	return true
}

// FnDeclInfo stores fn decl info metadata used by markused.
struct FnDeclInfo {
	node_id        flat.NodeId
	module         string
	import_context int
	// positional_params counts the declared params a call passes positionally;
	// -1 when it was not computed with the declaration.
	positional_params int = -1
}

// StructDeclInfo stores struct decl info metadata used by markused.
struct StructDeclInfo {
	node_id        flat.NodeId
	module         string
	import_context int
}

// ConstDeclInfo stores const decl info metadata used by markused.
struct ConstDeclInfo {
	expr_id        flat.NodeId
	module         string
	import_context int
}

// PreparedMarkusedDecls owns the declaration indexes that can be built from
// the immutable post-collect AST while semantic body checking uses the other
// cores.
@[heap]
pub struct PreparedMarkusedDecls {
mut:
	fn_decls                     map[string]FnDeclInfo
	fn_decl_lists                map[string][]FnDeclInfo
	struct_decls                 map[u32]StructDeclInfo
	const_decls                  map[string]ConstDeclInfo
	fn_name_suffixes             map[string]bool
	const_name_suffixes          map[string]bool
	suffix_map                   map[string][]string
	import_contexts              []map[string]string
	marked_roots                 []string
	cached_header_enum_str_roots []u32
	c_interface_roots            []string
	auto_roots                   []string
	body_ids                     []int
	body_modules                 []string
	body_import_contexts         []int
	needs_closure_runtime        bool
	scope                        voidptr
	ready                        bool
}

// prepare_markused_declarations builds self-host declaration indexes on a
// helper-local arena. It intentionally performs no semantic/body queries.
pub fn prepare_markused_declarations(a &flat.FlatAst, tc &types.TypeChecker, enabled bool) &PreparedMarkusedDecls {
	mut prepared := &PreparedMarkusedDecls{}
	if !enabled {
		return prepared
	}
	scope := markused_worker_scope_begin(true)
	prepared = build_prepared_markused_declarations(a, tc)
	prepared.scope = scope
	prepared.ready = true
	markused_worker_scope_leave(scope)
	return prepared
}

// release frees the helper-local preparation arena after the caller has
// published the used-name map into its compilation arena.
pub fn (mut prepared PreparedMarkusedDecls) release() {
	scope := prepared.scope
	prepared.scope = unsafe { nil }
	prepared.ready = false
	if scope != unsafe { nil } {
		// `prepared` itself belongs to this arena, so the final write must happen
		// before releasing it.
		markused_worker_scope_free(scope)
	}
}

fn build_prepared_markused_declarations(a &flat.FlatAst, tc &types.TypeChecker) &PreparedMarkusedDecls {
	mut result := &PreparedMarkusedDecls{
		fn_decls:             map[string]FnDeclInfo{}
		fn_decl_lists:        map[string][]FnDeclInfo{}
		struct_decls:         map[u32]StructDeclInfo{}
		const_decls:          map[string]ConstDeclInfo{}
		fn_name_suffixes:     map[string]bool{}
		const_name_suffixes:  map[string]bool{}
		suffix_map:           map[string][]string{}
		import_contexts:      []map[string]string{cap: 256}
		body_ids:             []int{cap: 8192}
		body_modules:         []string{cap: 8192}
		body_import_contexts: []int{cap: 8192}
	}
	result.import_contexts << map[string]string{}
	result.fn_decls.reserve(u32(tc.fn_ret_types.len))
	result.fn_decl_lists.reserve(u32(tc.fn_ret_types.len))
	result.struct_decls.reserve(u32(tc.structs.len * 2))
	result.const_decls.reserve(u32(tc.const_types.len))
	result.fn_name_suffixes.reserve(u32(tc.fn_ret_types.len))
	result.const_name_suffixes.reserve(u32(tc.const_types.len))
	result.suffix_map.reserve(u32(tc.fn_ret_types.len / 2))
	mut import_context_by_file := map[string]int{}
	mut cur_file := ''
	mut cur_module := ''
	mut cur_import_context := 0
	for node_idx in tc.top_level_idx {
		node := a.nodes[node_idx]
		if node.kind == .file {
			cur_file = node.value
			cur_module = ''
			result.import_contexts << markused_top_level_file_imports(a, node)
			cur_import_context = result.import_contexts.len - 1
			import_context_by_file[cur_file] = cur_import_context
			continue
		}
		if node.kind == .module_decl {
			cur_module = node.value
			continue
		}
		if node.kind == .import_decl {
			if cur_import_context >= 0 && cur_import_context < result.import_contexts.len {
				result.import_contexts[cur_import_context][node.typ_text()] = node.value
			}
			continue
		}
		decl_file := if source_file := a.source_files[node.pos.id] {
			source_file.name
		} else {
			cur_file
		}
		decl_module := tc.file_modules[decl_file] or { cur_module }
		decl_import_context := import_context_by_file[decl_file] or { cur_import_context }
		if node.kind == .enum_decl && decl_file.ends_with('.vh') {
			result.cached_header_enum_str_roots << markused_qualify_source_type(decl_module, node.source_value_id())
		}
		if node.kind == .struct_decl {
			name_id := node.source_value_id()
			full_name := markused_qualify_source_type(decl_module, name_id)
			info := StructDeclInfo{
				node_id:        flat.NodeId(node_idx)
				module:         decl_module
				import_context: decl_import_context
			}
			result.struct_decls[full_name] = info
			if name_id !in result.struct_decls {
				result.struct_decls[name_id] = info
			}
			continue
		}
		if node.kind == .const_decl || node.kind == .global_decl {
			for i in 0 .. node.children_count {
				field_id := a.child(&node, i)
				field := a.node(field_id)
				if field.kind != .const_field || field.children_count == 0 {
					continue
				}
				info := ConstDeclInfo{
					expr_id:        a.child(field, 0)
					module:         decl_module
					import_context: decl_import_context
				}
				result.const_decls[field.value] = info
				add_candidate_suffix(mut result.const_name_suffixes, field.value)
				full_name := qualify_fn(decl_module, field.value)
				if full_name != field.value {
					result.const_decls[full_name] = info
					add_candidate_suffix(mut result.const_name_suffixes, full_name)
				}
			}
			continue
		}
		if node.kind != .fn_decl && node.kind != .c_fn_decl {
			continue
		}
		decl_qname := qualify_fn(decl_module, node.value)
		fn_decl_file := tc.fn_type_files[decl_qname] or { decl_file }
		fn_decl_module := tc.fn_type_modules[decl_qname] or { decl_module }
		fn_decl_import_context := import_context_by_file[fn_decl_file] or { decl_import_context }
		result.body_ids << node_idx
		result.body_modules << fn_decl_module
		result.body_import_contexts << fn_decl_import_context
		has_dot := node.value.index_u8(`.`) >= 0
		can_suffix_match := !markused_fn_decl_is_generic_template(node, a)
		info := FnDeclInfo{
			node_id:           flat.NodeId(node_idx)
			module:            fn_decl_module
			import_context:    fn_decl_import_context
			positional_params: markused_positional_param_count(a, &node)
		}
		add_fn_decl_info(mut result.fn_decls, mut result.fn_decl_lists, node.value, info)
		if can_suffix_match {
			add_candidate_suffix(mut result.fn_name_suffixes, node.value)
		}
		lowered_name := markused_c_name(node.value)
		if lowered_name != node.value {
			add_fn_decl_info(mut result.fn_decls, mut result.fn_decl_lists, lowered_name, info)
			if can_suffix_match {
				add_candidate_suffix(mut result.fn_name_suffixes, lowered_name)
			}
		}
		qname := qualify_fn(fn_decl_module, node.value)
		if markused_fn_has_attribute(a, node_idx, 'markused') {
			result.marked_roots << qname
		}
		if qname != node.value {
			add_fn_decl_info(mut result.fn_decls, mut result.fn_decl_lists, qname, info)
			if can_suffix_match {
				add_candidate_suffix(mut result.fn_name_suffixes, qname)
			}
			lowered_qname := markused_c_name(qname)
			if lowered_qname != qname {
				add_fn_decl_info(mut result.fn_decls, mut result.fn_decl_lists, lowered_qname, info)
				if can_suffix_match {
					add_candidate_suffix(mut result.fn_name_suffixes, lowered_qname)
				}
			}
		}
		if has_dot && can_suffix_match {
			short := node.value.all_after_last('.')
			add_suffix_candidate(mut result.suffix_map, short, node.value)
			if qname != node.value {
				add_suffix_candidate(mut result.suffix_map, short, qname)
			}
		}
		if qname != node.value && qname.index_u8(`.`) >= 0 && can_suffix_match {
			add_suffix_candidate(mut result.suffix_map, qname.all_after_last('.'), qname)
		}
		if fn_decl_module == 'c' && fn_decl_file.ends_with('/gen/c/interface.v') {
			result.c_interface_roots << node.value
			if qname != node.value {
				result.c_interface_roots << qname
			}
			lowered_qname := markused_c_name(qname)
			if lowered_qname != qname {
				result.c_interface_roots << lowered_qname
			}
		}
	}
	reachable_modules := markused_reachable_modules(a, tc)
	mut auto_used := map[string]bool{}
	enqueue_auto_roots(a, result.fn_decls, reachable_modules, mut auto_used, mut result.auto_roots)
	result.needs_closure_runtime = markused_syntax_needs_closure_runtime(a)
	return result
}

fn markused_syntax_needs_closure_runtime(a &flat.FlatAst) bool {
	for idx, node in a.nodes {
		if (node.kind == .import_decl
			&& (node.value == 'builtin.closure' || node.typ == markused_source_type_closure_runtime))
			|| node.kind == .fn_literal
			|| (idx >= a.user_code_start && node.kind == .lambda_expr) {
			return true
		}
	}
	return false
}

// CallCollector represents call collector data used by markused.
struct CallCollector {
	a                     &flat.FlatAst             = unsafe { nil }
	tc                    &types.TypeChecker        = unsafe { nil }
	receiver_method_cache &MarkusedMethodCache      = unsafe { nil }
	local_walk_scratch    &MarkusedLocalWalkScratch = unsafe { nil }
	decl_name_cache       &DeclNameCache            = unsafe { nil }

	source_type_cache &SourceTypeCache    = unsafe { nil }
	default_calls     &StructDefaultCalls = unsafe { nil }
	fn_decls          map[string]FnDeclInfo
	fn_suffixes       map[string]bool
	struct_decls      map[u32]StructDeclInfo
	const_decls       map[string]ConstDeclInfo
	const_suffixes    map[string]bool
	// True only when the declaration scan indexed every constant/global key.
	// Manually constructed collectors can retain the ordinary candidate lookup.
	const_suffixes_complete bool
	import_contexts         []map[string]string
	// Unqualified selective-import symbol -> all known alias targets. This is
	// global because generic detection only needs to know whether any target
	// requires monomorphization.
	selective_alias_targets map[u32][]u32
	// Gate set for collect_interface_boxed_generic_methods: every fn_param_types
	// key with at least one interface-typed parameter, plus its post-'.' and
	// post-'__' short spellings (see markused_interface_param_gate). Lets the
	// per-call-node scan bail out with a couple of map probes instead of building
	// signature-candidate spellings (c_name/qualify allocations) for every call.
	iface_param_gate map[string]bool
	// Includes scoped local declarations, which are intentionally absent from
	// the checker's top-level generic declaration maps.
	generic_type_bases map[u32]bool
	generic_calls      &GenericCalls = unsafe { nil }
	// Shared read-only metadata for every script stringification in this pass.
	auto_str_skipped_fields map[u32]&MarkusedSkippedFields
	// Self-host builds are known not to need monomorphization, so they can omit
	// generic-only reachability probes while preserving ordinary call collection.
	local_ident_visibility           map[int]bool
	local_ident_types                flat.NodeTypeTextBindings
	local_type_texts                 &flat.TypeTextTable = &flat.TypeTextTable{}
	checked_local_names              map[string]bool
	checked_sum_types                map[string]types.TypeId
	has_local_ident_visibility       bool
	detect_generics                  bool
	body_checker_edges_authoritative bool
}

fn (c &CallCollector) imports(context int) map[string]string {
	if context >= 0 && context < c.import_contexts.len {
		return c.import_contexts[context]
	}
	return c.import_contexts[0]
}

// markused_interface_param_gate builds CallCollector.iface_param_gate. Any raw
// callee spelling whose expanded signature candidates could hit an
// interface-param signature is covered by the key itself, its post-dot short
// name, or its post-'__' short name (candidate expansion only prepends module
// qualifiers or applies c_name, both of which preserve the final segment).
fn markused_interface_param_gate(tc &types.TypeChecker) map[string]bool {
	mut gate := map[string]bool{}
	for name, params in tc.fn_param_types {
		mut has_iface := false
		for p in params {
			if types.unwrap_pointer(p) is types.Interface {
				has_iface = true
				break
			}
		}
		if !has_iface {
			continue
		}
		gate[name] = true
		short_dot := name.all_after_last('.')
		if short_dot != name {
			gate[short_dot] = true
		}
		if name.contains('__') {
			short_us := name.all_after_last('__')
			if short_us != name {
				gate[short_us] = true
			}
		}
	}
	return gate
}

fn markused_selective_alias_targets(tc &types.TypeChecker) map[u32][]u32 {
	mut result := map[u32][]u32{}
	for _, info in tc.selective_import_files {
		for name_id, candidates in info.selective_imports {
			for candidate_id in candidates {
				target := tc.type_aliases.type_id(candidate_id) or { continue }
				mut targets := result[name_id]
				if target !in targets {
					targets << target
					result[name_id] = targets
				}
			}
		}
	}
	return result
}

// may_target_interface_params reports whether `name` (a raw callee spelling)
// could expand to a signature in iface_param_gate.
fn (c &CallCollector) may_target_interface_params(name string) bool {
	if name.len == 0 || c.iface_param_gate.len == 0 {
		return false
	}
	if name in c.iface_param_gate {
		return true
	}
	dot := name.last_index_u8(`.`)
	if dot < 0 {
		return false
	}
	// A view of the name after the last dot, not a copy: the map lookup only
	// reads `len` bytes of it.
	short := unsafe { tos(name.str + dot + 1, name.len - dot - 1) }
	return short in c.iface_param_gate
}

// enqueue_auto_roots supports enqueue auto roots handling for markused.
fn enqueue_auto_roots(a &flat.FlatAst, fn_decls map[string]FnDeclInfo, reachable_modules map[string]bool, mut used map[string]bool, mut queue []string) {
	for name, info in fn_decls {
		if !is_auto_root_fn(name) {
			continue
		}
		if name.all_after_last('.') == 'cleanup'
			&& markused_fn_has_receiver_param(a, a.node(info.node_id)) {
			continue
		}
		if !markused_module_has_reachable_initializer(info.module, reachable_modules) {
			continue
		}
		enqueue(name, mut used, mut queue)
		short_name := name.all_after_last('.')
		if short_name != name {
			enqueue(short_name, mut used, mut queue)
		}
		qualified_name := qualify_fn(info.module, short_name)
		if qualified_name != name {
			enqueue(qualified_name, mut used, mut queue)
		}
	}
}

fn enqueue_export_roots(a &flat.FlatAst, tc &types.TypeChecker, mut used map[string]bool, mut queue []string) {
	if a.export_fn_names.len == 0 {
		return
	}
	mut cur_module := ''
	for node_idx in tc.top_level_idx {
		node := a.nodes[node_idx]
		match node.kind {
			.file {
				cur_module = ''
			}
			.module_decl {
				cur_module = node.value
			}
			.fn_decl {
				qname := qualify_fn(cur_module, node.value)
				if qname in a.export_fn_names {
					enqueue(qname, mut used, mut queue)
				}
			}
			else {}
		}
	}
}

fn enqueue_veb_handler_roots(a &flat.FlatAst, tc &types.TypeChecker, mut used map[string]bool, mut queue []string) {
	// Most programs, including compiler self-hosts, do not import veb. Avoid
	// parsing every function return type when its Result type is absent.
	if !markused_type_name_known_in_module(tc, 'veb', flat.source_type_id('Result')) {
		return
	}
	mut cur_module := ''
	for node_idx in tc.top_level_idx {
		node := a.nodes[node_idx]
		match node.kind {
			.file {
				cur_module = ''
			}
			.module_decl {
				cur_module = node.value
			}
			.fn_decl {
				if markused_fn_needs_implicit_veb_ctx(a, tc, cur_module, node) {
					enqueue(qualify_fn(cur_module, node.value), mut used, mut queue)
				}
			}
			else {}
		}
	}
}

fn markused_fn_needs_implicit_veb_ctx(a &flat.FlatAst, tc &types.TypeChecker, cur_module string, node flat.Node) bool {
	return markused_fn_returns_veb_result(tc, node) && markused_fn_has_receiver_param(a, node)
		&& !markused_fn_receiver_type_is_context(a, node)
		&& markused_type_name_known_in_module(tc, cur_module, flat.source_type_id('Context'))
}

fn markused_fn_returns_veb_result(tc &types.TypeChecker, node flat.Node) bool {
	if node.typ == markused_source_type_veb_result {
		return true
	}
	ret := tc.parse_type(node.typ)
	return ret.source_type_id() == markused_source_type_veb_result
}

fn markused_fn_has_receiver_param(a &flat.FlatAst, node flat.Node) bool {
	if !node.value.contains('.') || node.children_count == 0 {
		return false
	}
	first := a.child_node(&node, 0)
	if first.kind != .param || first.typ == 0 {
		return false
	}
	receiver := flat.source_type_syntax(flat.source_type_syntax(node.source_value_id()).qualifier()).unqualified()
	param_type := flat.source_type_syntax(flat.source_type_syntax(first.typ).pointer_raw_base()).unqualified()
	return receiver == param_type
}

fn markused_fn_receiver_type_is_context(a &flat.FlatAst, node flat.Node) bool {
	if !markused_fn_has_receiver_param(a, node) {
		return false
	}
	first := a.child_node(&node, 0)
	return flat.source_type_syntax(flat.source_type_syntax(first.typ).pointer_raw_base()).unqualified() == flat.source_type_id('Context')
}

fn markused_type_name_known_in_module(tc &types.TypeChecker, module_name string, typ u32) bool {
	qtyp_id := markused_qualify_source_type(module_name, typ)
	return tc.type_aliases.contains(qtyp_id) || qtyp_id in tc.structs || qtyp_id in tc.interface_names
		|| qtyp_id in tc.enum_names || tc.sum_types.contains(qtyp_id)
}

fn enqueue_test_file_roots(a &flat.FlatAst, test_files map[string]bool, mut used map[string]bool, mut queue []string) {
	if test_files.len == 0 {
		return
	}
	for file_idx, file_node in a.nodes {
		if !is_user_test_file_node(a, file_idx, file_node, test_files) {
			continue
		}
		module_name := test_file_module_name(a, file_node)
		mut decl_ids := []flat.NodeId{}
		markused_collect_test_harness_decl_ids(a, file_node, mut decl_ids)
		for child_id in decl_ids {
			child := a.node(child_id)
			if is_test_harness_root_name(child.value) {
				enqueue(qualify_fn(module_name, child.value), mut used, mut queue)
			}
		}
	}
}

fn markused_collect_test_harness_decl_ids(a &flat.FlatAst, node flat.Node, mut ids []flat.NodeId) {
	if node.kind != .file && node.kind != .block {
		return
	}
	for i in 0 .. node.children_count {
		child_id := a.child(&node, i)
		if int(child_id) < a.user_code_start {
			continue
		}
		child := a.node(child_id)
		if child.kind == .fn_decl {
			ids << child_id
		} else if child.kind == .block {
			markused_collect_test_harness_decl_ids(a, child, mut ids)
		}
	}
}

fn is_user_test_file_node(a &flat.FlatAst, file_idx int, file_node flat.Node, test_files map[string]bool) bool {
	if file_idx < a.user_code_start || file_node.kind != .file || file_node.children_count == 0 {
		return false
	}
	return test_files[file_node.value]
}

fn test_file_module_name(a &flat.FlatAst, file_node flat.Node) string {
	for i in 0 .. file_node.children_count {
		child := a.child_node(&file_node, i)
		if child.kind == .module_decl {
			return child.value
		}
	}
	return ''
}

fn is_test_harness_root_name(name string) bool {
	return name.starts_with('test_')
		|| name in ['testsuite_begin', 'testsuite_end', 'before_each', 'after_each']
}

// is_trivial_literal_output_program reports whether the selected source consists
// only of literal print calls. It does not depend on semantic collection, so the
// driver can select its non-generic check path before checking function bodies.
pub fn is_trivial_literal_output_program(a &flat.FlatAst, diagnostic_files map[string]bool) bool {
	if diagnostic_files.len != 1 {
		return false
	}
	mut stack := []flat.NodeId{}
	file_ids := if a.file_node_ids.len > 0 && !a.file_index_incomplete {
		a.file_node_ids
	} else {
		mut ids := []i32{}
		for i, node in a.nodes {
			if node.kind == .file {
				ids << i
			}
		}
		ids
	}
	for id in file_ids {
		node := a.nodes[id]
		if node.kind != .file || node.value !in diagnostic_files {
			continue
		}
		for i in 0 .. node.children_count {
			stack << a.child(&node, i)
		}
	}
	if stack.len == 0 {
		return false
	}
	mut output_calls := 0
	mut main_fns := 0
	for stack.len > 0 {
		id := stack.pop()
		if int(id) < 0 {
			continue
		}
		node := a.node(id)
		match node.kind {
			.module_decl, .block, .expr_stmt, .string_literal {}
			.fn_decl {
				if node.value != 'main' || main_fns > 0 {
					return false
				}
				main_fns++
			}
			.call {
				if node.children_count != 2 {
					return false
				}
				callee := a.child_node(node, 0)
				arg := a.child_node(node, 1)
				if callee.kind != .ident
					|| callee.value !in ['print', 'println', 'eprint', 'eprintln'] || arg.kind != .string_literal {
					return false
				}
				output_calls++
			}
			.ident {
				if node.value !in ['print', 'println', 'eprint', 'eprintln'] {
					return false
				}
			}
			else {
				return false
			}
		}
		for i in 0 .. node.children_count {
			stack << a.child(node, i)
		}
	}
	return output_calls > 0
}

// enqueue_main_module_roots supports enqueue main module roots handling for markused.
fn enqueue_main_module_roots(fn_decls map[string]FnDeclInfo, mut used map[string]bool, mut queue []string) {
	for name, info in fn_decls {
		if info.module != 'main' || name != 'main' {
			continue
		}
		enqueue(name, mut used, mut queue)
	}
}

// is_auto_root_fn reports whether is auto root fn applies in markused.
fn is_auto_root_fn(name string) bool {
	// Compare the text after the last `.` in place: every declaration name is
	// tested, and only a few end in one of these.
	short_len := name.len - name.last_index_u8(`.`) - 1
	return (short_len == 4 && name.ends_with('init'))
		|| (short_len == 12 && name.ends_with('builtin_init'))
		|| (short_len == 7 && name.ends_with('cleanup'))
}

fn markused_fn_has_attribute(a &flat.FlatAst, node_idx int, name string) bool {
	attr_idx := node_idx + 1
	if attr_idx < 0 || attr_idx >= a.nodes.len {
		return false
	}
	attr := a.nodes[attr_idx]
	if attr.kind != .directive || attr.value != '@attributes:${node_idx}' {
		return false
	}
	return name in attr.generic_params()
}

@[flag]
enum RuntimeHelperGroups {
	optional
	interpolation
	string_plus
	membership
	new_map
	map_snapshot
	channel
	channel_select
	channel_string
	float_epsilon
	shared
}

@[heap]
struct MarkusedTypeResults {
mut:
	entries map[u32]int
}

@[heap]
struct MarkusedSkippedFields {
mut:
	fields map[string]bool
}

// RuntimeHelpersScan shares helper flags and type-query caches across reached bodies.
struct RuntimeHelpersScan {
mut:
	enqueued_groups                 RuntimeHelperGroups
	needs_optional_helpers          bool
	needs_string_interp_helpers     bool
	needs_string_plus_helper        bool
	needs_string_membership_helpers bool
	needs_new_map                   bool
	needs_map_iteration_snapshot    bool
	needs_channel_helpers           bool
	needs_channel_select_helpers    bool
	needs_channel_str_helpers       bool
	needs_f32_eq_epsilon            bool
	needs_ierror_equality_dispatch  bool
	needs_shared_runtime            bool
	select_array_methods            bool
	select_source_utilities         bool
	select_map_runtime              bool
	needs_map_runtime               bool
	map_runtime_enqueued            bool
	select_closure_runtime          bool
	closure_globals_scanned         bool
	closure_runtime_enqueued        bool
	deferred_closure_mask           []bool
	deferred_closure_nodes          []int
	direct_call_callees             []bool
	assignment_targets              []bool

	channel_stringify_cache    map[string]&MarkusedTypeResults
	ierror_equality_cache      map[u32]int
	interface_dispatch         InterfaceDispatchScan
	map_type_cache             map[u32]int
	auto_str_skipped_fields    map[u32]&MarkusedSkippedFields
	stringification_seen       map[string]&StringifiedTypes
	stringification_node_count int
	stringification_fn_count   int
	initialized                bool
}

fn enqueue_detected_runtime_helpers(a &flat.FlatAst, tc &types.TypeChecker, mut used map[string]bool, mut queue []string) {
	mut scan := RuntimeHelpersScan{ auto_str_skipped_fields: markused_auto_str_skipped_fields(a) }
	scan.enqueue_nodes(a, tc, []int{}, []bool{}, '', map[string]string{}, mut used, mut queue)
}

// runtime_helper_subtree appends each node of a reached body once, including nested
// expressions introduced by checking. These need not be contiguous in the arena.
@[direct_array_access]
fn runtime_helper_subtree(a &flat.FlatAst, root flat.NodeId, mut seen []bool, mut stack []flat.NodeId, mut node_ids MarkusedNodeList) {
	node_ids.size = 0
	if seen.len < a.nodes.len {
		seen << []bool{len: a.nodes.len - seen.len}
	}
	mut walker := MarkusedNodeStack{ nodes: stack }
	walker.push(root)
	for walker.size > 0 {
		id := int(walker.pop())
		if id < 0 || id >= a.nodes.len || seen[id] {
			continue
		}
		seen[id] = true
		node_ids.push(id)
		node := &a.nodes[id]
		for i in 0 .. node.children_count {
			walker.push(a.child(node, i))
		}
	}
	stack = walker.nodes
}

// Source nodes are stored before their parents. Propagate the union of body
// roots in arena order without materializing each body's traversal list. A
// forward edge introduced by checking uses the ordinary graph walk, so the
// mask also covers rewritten, shared and noncontiguous subtrees.
@[direct_array_access]
fn runtime_helper_body_mask(a &flat.FlatAst, body_ids []int) []bool {
	mut seen := []bool{len: a.nodes.len}
	for id in body_ids {
		if id >= 0 && id < seen.len {
			seen[id] = true
		}
	}
	mut stack := []flat.NodeId{}
	mut nodes := MarkusedNodeList{}
	for next := a.nodes.len; next > 0; next-- {
		id := next - 1
		if !seen[id] {
			continue
		}
		node := &a.nodes[id]
		for i in 0 .. node.children_count {
			child := int(a.child(node, i))
			if child < 0 || child >= seen.len || seen[child] {
				continue
			}
			if child < id {
				seen[child] = true
			} else {
				runtime_helper_subtree(a, flat.NodeId(child), mut seen, mut stack, mut nodes)
			}
		}
	}
	return seen
}

// Traversal outputs retain their high-water length too. Consumers borrow only
// the live prefix until the next synchronous scan resets size.
struct MarkusedNodeList {
mut:
	nodes []int
	size  int
}

@[direct_array_access; inline]
fn (mut list MarkusedNodeList) push(id int) {
	if list.size < list.nodes.len {
		list.nodes[list.size] = id
	} else {
		list.nodes << id
	}
	list.size++
}

// The private traversal stack retains its high-water length so push/pop do not
// repeatedly inspect slice ownership. Only size entries are live.
struct MarkusedNodeStack {
mut:
	nodes []flat.NodeId
	size  int
}

@[direct_array_access; inline]
fn (mut stack MarkusedNodeStack) push(id flat.NodeId) {
	if stack.size < stack.nodes.len {
		stack.nodes[stack.size] = id
	} else {
		stack.nodes << id
	}
	stack.size++
}

@[direct_array_access; inline]
fn (mut stack MarkusedNodeStack) pop() flat.NodeId {
	stack.size--
	return stack.nodes[stack.size]
}

@[direct_array_access]
fn (mut scan RuntimeHelpersScan) enqueue_nodes(a &flat.FlatAst, tc &types.TypeChecker, node_ids []int, body_nodes []bool, initial_module string, initial_imports map[string]string, mut used map[string]bool, mut queue []string) {
	mut cur_module := initial_module
	// Borrow the caller's imports until a declaration requires a private copy.
	mut imports := unsafe { initial_imports }
	mut imports_owned := false
	if !scan.initialized {
		scan.initialized = true
		if scan.select_closure_runtime {
			scan.direct_call_callees = []bool{len: a.nodes.len}
			scan.assignment_targets = []bool{len: a.nodes.len}
			for call_idx in 0 .. a.nodes.len {
				call := unsafe { &a.nodes[call_idx] }
				if call.kind in [.assign, .selector_assign, .index_assign]
					&& call.children_count > 0 {
					mut target := a.child(call, 0)
					for a.node(target).kind == .paren && a.node(target).children_count > 0 {
						target = a.child(a.node(target), 0)
					}
					if int(target) >= 0 && int(target) < a.nodes.len {
						scan.assignment_targets[int(target)] = true
					}
				}
				if call.kind == .call && call.children_count > 0 {
					callee := a.child(call, 0)
					if int(callee) >= 0 && int(callee) < a.nodes.len {
						scan.direct_call_callees[int(callee)] = true
					}
					callee_node := a.node(callee)
					if callee_node.kind == .index && callee_node.children_count > 0 {
						base := int(a.child(callee_node, 0))
						if base >= 0 && base < a.nodes.len {
							scan.direct_call_callees[base] = true
						}
					}
				}
			}
		}
		for _, shared_params in tc.fn_shared_params {
			if shared_params.any(it) {
				scan.needs_shared_runtime = true
				break
			}
		}
	}
	node_count := if node_ids.len > 0 { node_ids.len } else { a.nodes.len }
	for scan_idx in 0 .. node_count {
		node_idx := if node_ids.len > 0 { node_ids[scan_idx] } else { scan_idx }
		node := &a.nodes[node_idx]
		if node_ids.len == 0 && scan.deferred_closure_mask.len > 0
			&& scan.deferred_closure_mask[node_idx] {
			continue
		}
		// Signatures and generated type helpers are emitted independently of a
		// function body. Keep those declarations even when its expressions are skipped.
		if body_nodes.len > 0 && body_nodes[node_idx]
			&& node.kind !in [.fn_decl, .c_fn_decl, .param, .field_decl, .field_init, .const_field,
				.struct_decl, .interface_decl, .type_decl, .enum_decl] {
			continue
		}
		if scan.select_array_methods && node.kind == .selector
			&& node.value in ['insert', 'insert_many', 'prepend', 'reverse', 'pop_left', 'delete',
				'delete_last'] {
			// Cgen intrinsic methods and method values need these bodies even when
			// call collection has no ordinary callee. Unknown receivers stay conservative.
			enqueue('array.${node.value}', mut used, mut queue)
			if node.value in ['insert', 'prepend'] {
				// Inserting an array or fixed array lowers to this helper after markused.
				enqueue('array.insert_many', mut used, mut queue)
			}
		}
		if scan.select_source_utilities && node.kind == .selector {
			match node.value {
				'all_before', 'all_before_last', 'all_after', 'all_after_last', 'is_capital',
				'to_lower_ascii' {
					enqueue('string.${node.value}', mut used, mut queue)
				}
				else {}
			}
			if node.value in ['is_letter', 'is_capital'] {
				enqueue('u8.${node.value}', mut used, mut queue)
			}
			if node.value == 'string' {
				enqueue('[]rune.string', mut used, mut queue)
			}
			if node.value == 'bytestr' {
				enqueue('Array_u8__bytestr', mut used, mut queue)
			}
			if node.value == 'hex' {
				enqueue('Array_u8__hex', mut used, mut queue)
			}
			if node.value == 'to_lower' {
				enqueue('rune.to_lower', mut used, mut queue)
			}
		}
		if scan.select_closure_runtime && !scan.closure_runtime_enqueued {
			mut needs_closure := node.kind in [.fn_literal, .lambda_expr]
			if node.kind == .selector && node.children_count > 0
				&& (node_idx >= scan.direct_call_callees.len || !scan.direct_call_callees[node_idx])
				&& (node_idx >= scan.assignment_targets.len || !scan.assignment_targets[node_idx]) {
				base := a.child_node(node, 0)
				if !(base.kind == .ident && (base.value in ['C', 'JS'] || base.value in imports)) {
					needs_closure = tc.expr_is_method_value(flat.NodeId(node_idx))
						|| base.kind in [.string_literal, .int_literal, .float_literal, .char_literal]
					if !needs_closure {
						// Enum namespaces have no receiver value. Their checked selector
						// type distinguishes them from unresolved generic method values.
						selector_type := tc.expr_type(flat.NodeId(node_idx)) or {
							types.Type(types.Unknown{})
						}
						needs_closure = types.unalias_type(selector_type) !is types.Enum
							&& tc.resolve_type(a.child(node, 0)) is types.Unknown
					}
				}
			}
			if needs_closure {
				scan.closure_runtime_enqueued = true
				for helper in ['closure.closure_init', 'closure.closure_create_with_data',
					'closure.closure_try_destroy'] {
					enqueue(helper, mut used, mut queue)
				}
			}
		}
		if scan.select_map_runtime && !scan.map_runtime_enqueued && !scan.needs_map_runtime
			&& node.kind == .global_decl {
			// Global struct defaults are emitted even when main never reads them.
			for i in 0 .. node.children_count {
				field := a.child_node(node, i)
				global_name := if cur_module in ['', 'main', 'builtin'] || field.value.contains('.') {
					field.value
				} else {
					'${cur_module}.${field.value}'
				}
				global_type := if field.value.starts_with('C.') {
					tc.c_globals[field.value] or { *tc.parse_type(field.typ) }
				} else {
					tc.file_scope.lookup(global_name) or { *tc.parse_type(field.typ) }
				}
				if scan.type_needs_map_runtime(global_type, tc) {
					scan.needs_map_runtime = true
					break
				}
			}
		}
		if scan.select_map_runtime && !scan.map_runtime_enqueued && !scan.needs_map_runtime
			&& node.kind in [.ident, .selector, .call, .struct_init, .array_init, .cast_expr, .as_expr,
				.prefix, .index] {
			if typ := tc.expr_type(flat.NodeId(node_idx)) {
				scan.needs_map_runtime = scan.type_needs_map_runtime(typ, tc)
			} else if node.kind in [.struct_init, .array_init, .cast_expr, .as_expr] {
				scan.needs_map_runtime = scan.type_needs_map_runtime(tc.resolve_type(flat.NodeId(node_idx)), tc)
			}
		}
		if node.typ != 0 {
			if !scan.needs_channel_helpers && markused_type_text_is_channel(node.typ) {
				scan.needs_channel_helpers = true
			}
			if !scan.needs_shared_runtime && markused_type_text_needs_shared_runtime(node.typ) {
				scan.needs_shared_runtime = true
			}
		}
		if !scan.needs_shared_runtime && node.kind == .decl_assign
			&& (node.value == 'shared' || node.value.starts_with('shared:')) {
			scan.needs_shared_runtime = true
		}
		match node.kind {
			.file {
				cur_module = ''
				imports = map[string]string{}
				imports_owned = true
			}
			.module_decl {
				cur_module = node.value
			}
			.import_decl {
				// Reached bodies only read their file's imports. Clone lazily for
				// a scan that actually encounters a declaration and updates them.
				if !imports_owned {
					imports = imports.clone()
					imports_owned = true
				}
				imports[node.typ_text()] = node.value
			}
			.fn_decl {
				if !scan.needs_optional_helpers && type_string_needs_optional_helpers(node.typ) {
					scan.needs_optional_helpers = true
				}
			}
			.param, .field_decl, .field_init, .const_field {
				if !scan.needs_optional_helpers && type_string_needs_optional_helpers(node.typ) {
					scan.needs_optional_helpers = true
				}
			}
			.none_expr {
				scan.needs_optional_helpers = true
			}
			.or_expr {
				scan.needs_optional_helpers = true
				if !scan.needs_new_map && node.children_count > 0 {
					expr_id := a.child(node, 0)
					expr_type := tc.expr_type(expr_id) or { tc.resolve_type(expr_id) }
					if type_needs_zero_map(expr_type) {
						scan.needs_new_map = true
					}
				}
			}
			.call {
				if node.children_count > 0 {
					fn_node := a.child_node(node, 0)
					if fn_node.kind == .selector && fn_node.value == 'str'
						&& fn_node.children_count > 0 {
						base_id := a.child(fn_node, 0)
						if !scan.needs_channel_str_helpers
							&& markused_expr_stringifies_channel(tc, base_id, cur_module, mut scan.channel_stringify_cache) {
							scan.needs_channel_str_helpers = true
						}
						scan.enqueue_stringification(base_id, cur_module, tc, mut used, mut queue)
					}
					if fn_node.kind == .ident
						&& (fn_node.value == 'error' || fn_node.value == 'error_with_code') {
						scan.needs_optional_helpers = true
					}
					if fn_node.kind == .ident && fn_node.value == 'flag_default_value' {
						enqueue('escape_default_string', mut used, mut queue)
					}
					if fn_node.kind == .selector && fn_node.value in string_method_runtime_helpers {
						enqueue('string.${fn_node.value}', mut used, mut queue)
					}
					if markused_call_lowers_to_join_path_single(a, fn_node, imports, tc.resolved_call_name(flat.NodeId(node_idx)) or { '' }) {
						enqueue('join_path_single', mut used, mut queue)
						enqueue('os.join_path_single', mut used, mut queue)
					}
					if fn_node.kind == .selector && fn_node.value == 'runes_iterator' {
						enqueue('RunesIterator.next', mut used, mut queue)
					}
					if fn_node.kind == .ident
						&& fn_node.value in ['print', 'println', 'eprint', 'eprintln']
						&& node.children_count >= 2 {
						if !scan.needs_channel_str_helpers
							&& markused_expr_stringifies_channel(tc, a.child(node, 1), cur_module, mut scan.channel_stringify_cache) {
							scan.needs_channel_str_helpers = true
						}
						scan.enqueue_stringification(a.child(node, 1), cur_module, tc, mut used, mut queue)
					}
				}
			}
			.select_stmt {
				scan.needs_channel_helpers = true
				scan.needs_channel_select_helpers = true
			}
			.struct_init {
				if flat.source_type_shape(node.source_value_id()).has(.channel_type) {
					scan.needs_channel_helpers = true
				}
			}
			.lock_expr {
				scan.needs_shared_runtime = true
			}
			.infix {
				if node.op == .arrow {
					scan.needs_channel_helpers = true
				}
				if node.op in [.eq, .ne] {
					if !tc.nofloat && !scan.needs_f32_eq_epsilon
						&& markused_infix_needs_f32_eq_epsilon(a, tc, node) {
						scan.needs_f32_eq_epsilon = true
					}
					if !scan.needs_ierror_equality_dispatch && node.children_count >= 2 {
						lhs_type := tc.resolve_type(a.child(node, 0))
						rhs_type := tc.resolve_type(a.child(node, 1))
						scan.needs_ierror_equality_dispatch =
							markused_type_equality_uses_ierror(lhs_type, tc, mut scan.ierror_equality_cache)
								|| markused_type_equality_uses_ierror(rhs_type, tc, mut scan.ierror_equality_cache)
					}
				}
			}
			.prefix {
				if node.op == .arrow {
					scan.needs_channel_helpers = true
				}
			}
			.string_interp {
				scan.needs_string_interp_helpers = true
				scan.needs_string_plus_helper = true
				for i in 0 .. node.children_count {
					mut part_id := a.child(node, i)
					part := a.node(part_id)
					if part.kind == .directive && part.value == 'string_interp_format'
						&& part.children_count > 0 {
						if part.typ_text() == 'p' {
							continue
						}
						part_id = a.child(part, 0)
					}
					if !scan.needs_channel_str_helpers
						&& markused_expr_stringifies_channel(tc, part_id, cur_module, mut scan.channel_stringify_cache) {
						scan.needs_channel_str_helpers = true
					}
					scan.enqueue_stringification(part_id, cur_module, tc, mut used, mut queue)
				}
			}
			.assign, .index_assign {
				if !scan.needs_string_plus_helper && node.op == .plus_assign
					&& node.children_count == 2 {
					lhs_id := a.child(node, 0)
					rhs_id := a.child(node, 1)
					rhs := a.node(rhs_id)
					lhs_type := markused_membership_container_type(tc, tc.resolve_type(lhs_id))
					rhs_type := markused_membership_container_type(tc, tc.resolve_type(rhs_id))
					if lhs_type == flat.source_type_string || rhs_type == flat.source_type_string
						|| rhs.kind in [.string_literal, .string_interp] {
						scan.needs_string_plus_helper = true
					}
				}
			}
			.assert_stmt {
				if node.children_count > 0 {
					condition := a.child_node(node, 0)
					if condition.kind == .infix && condition.children_count >= 2
						&& condition.op !in [.logical_and, .logical_or] {
						for operand in 0 .. 2 {
							scan.enqueue_stringification(a.child(condition, operand), cur_module, tc, mut used, mut queue)
						}
					}
				}
			}
			.in_expr {
				if node.children_count >= 2 {
					lhs_id := a.child(node, 0)
					if !scan.needs_ierror_equality_dispatch {
						scan.needs_ierror_equality_dispatch = markused_type_equality_uses_ierror(tc.resolve_type(lhs_id), tc, mut scan.ierror_equality_cache)
					}
					if !scan.needs_string_membership_helpers {
						rhs_id := a.child(node, 1)
						rhs_type := markused_membership_container_type(tc, tc.resolve_type(rhs_id))
						if rhs_type == flat.source_type_string {
							scan.needs_string_membership_helpers = true
						}
					}
				}
			}
			.map_init {
				scan.needs_new_map = true
			}
			.for_in_stmt {
				if node.value.int() == 3 && node.children_count > 2 {
					container_id := a.child(node, 2)
					container_type := tc.resolve_type(container_id)
					if types.unwrap_pointer(container_type) is types.Map {
						scan.needs_map_iteration_snapshot = true
					}
					if info := tc.iterator_for_in_next_call_info(container_type) {
						enqueue(info.name, mut used, mut queue)
						scan.needs_optional_helpers = true
					}
				}
			}
			.enum_decl {
				// A `[flag]` enum's synthesized `<Enum>__autostr` helper (emitted
				// unconditionally in cgen) builds its `Enum{.a | .b}` string with
				// `string__plus`. markused never walks that generated body, so without
				// seeding the helper here it can be pruned when the program has no other
				// string concatenation, leaving the autostr calling an undefined
				// `string__plus`.
				if node.typ_text() == 'flag' {
					scan.needs_string_plus_helper = true
				}
			}
			else {}
		}
	}
	if scan.needs_optional_helpers && !scan.enqueued_groups.has(.optional) {
		scan.enqueued_groups.set(.optional)
		for helper in optional_runtime_helpers {
			enqueue(helper, mut used, mut queue)
		}
	}
	if scan.needs_string_interp_helpers && !scan.enqueued_groups.has(.interpolation) {
		scan.enqueued_groups.set(.interpolation)
		for helper in string_interp_runtime_helpers {
			enqueue(helper, mut used, mut queue)
		}
	}
	if scan.needs_string_plus_helper && !scan.enqueued_groups.has(.string_plus) {
		scan.enqueued_groups.set(.string_plus)
		enqueue('string__plus', mut used, mut queue)
	}
	if scan.needs_string_membership_helpers && !scan.enqueued_groups.has(.membership) {
		scan.enqueued_groups.set(.membership)
		for helper in string_membership_runtime_helpers {
			enqueue(helper, mut used, mut queue)
		}
	}
	if scan.needs_new_map && !scan.enqueued_groups.has(.new_map) {
		scan.enqueued_groups.set(.new_map)
		enqueue('new_map', mut used, mut queue)
		enqueue('new_map_aligned', mut used, mut queue)
	}
	if scan.select_map_runtime && (scan.needs_new_map || scan.needs_map_runtime)
		&& !scan.map_runtime_enqueued {
		scan.enqueue_map_runtime(mut used, mut queue)
	}
	if scan.needs_map_iteration_snapshot && !scan.enqueued_groups.has(.map_snapshot) {
		scan.enqueued_groups.set(.map_snapshot)
		for helper in map_snapshot_runtime_helpers {
			enqueue(helper, mut used, mut queue)
		}
	}
	if scan.needs_channel_helpers && !scan.enqueued_groups.has(.channel) {
		scan.enqueued_groups.set(.channel)
		for helper in ['sync.new_channel_st', 'sync.Channel.push', 'sync.Channel.pop',
			'sync.Channel.close', 'sync.Channel.len', 'sync.Channel.closed',
			'sync.Channel.try_push_priv', 'sync.Channel.closed_error', 'new_channel_st', 'Channel.push',
			'Channel.pop', 'Channel.close', 'Channel.len', 'Channel.closed', 'Channel.try_push_priv',
			'Channel.closed_error'] {
			enqueue(helper, mut used, mut queue)
		}
	}
	if scan.needs_channel_select_helpers && !scan.enqueued_groups.has(.channel_select) {
		scan.enqueued_groups.set(.channel_select)
		for helper in ['sync.channel_select', 'sync.channel_select_lang', 'channel_select',
			'channel_select_lang', 'rand.init', 'array.free', 'array__free', 'time.sleep', 'time__sleep'] {
			enqueue(helper, mut used, mut queue)
		}
	}
	if scan.needs_channel_str_helpers && !scan.enqueued_groups.has(.channel_string) {
		scan.enqueued_groups.set(.channel_string)
		for helper in ['string__plus', 'int.str', 'int__str'] {
			enqueue(helper, mut used, mut queue)
		}
	}
	if scan.needs_f32_eq_epsilon && !scan.enqueued_groups.has(.float_epsilon) {
		scan.enqueued_groups.set(.float_epsilon)
		enqueue('f32.eq_epsilon', mut used, mut queue)
		enqueue('f32__eq_epsilon', mut used, mut queue)
	}
	if scan.needs_ierror_equality_dispatch {
		enqueue_ierror_equality_dispatch_helpers(tc, mut used, mut queue, mut scan.interface_dispatch)
	}
	if scan.needs_shared_runtime && !scan.enqueued_groups.has(.shared) {
		scan.enqueued_groups.set(.shared)
		enqueue('malloc_uncollectable', mut used, mut queue)
		for helper in ['sync.cpanic', 'sync.cpanic_errno', 'sync.should_be_zero', 'sync.RwMutex.init',
			'sync.RwMutex.lazy_init', 'sync.RwMutex.lock', 'sync.RwMutex.unlock', 'sync.RwMutex.rlock',
			'sync.RwMutex.runlock', 'cpanic', 'cpanic_errno', 'should_be_zero', 'RwMutex.init',
			'RwMutex.lazy_init', 'RwMutex.lock', 'RwMutex.unlock', 'RwMutex.rlock', 'RwMutex.runlock'] {
			enqueue(helper, mut used, mut queue)
		}
	}
}

fn markused_program_needs_closure_runtime(a &flat.FlatAst, tc &types.TypeChecker) bool {
	if markused_syntax_needs_closure_runtime(a) {
		return true
	}
	mut call_callees := map[int]bool{}
	for node in a.nodes {
		if node.kind == .call && node.children_count > 0 {
			callee_id := a.child(&node, 0)
			call_callees[int(callee_id)] = true
			callee_node := a.node(callee_id)
			if callee_node.kind == .index && callee_node.children_count > 0 {
				call_callees[int(a.child(callee_node, 0))] = true
			}
			mut unwrapped_id := callee_id
			mut was_wrapped := false
			for int(unwrapped_id) >= 0 && int(unwrapped_id) < a.nodes.len {
				callee := a.nodes[int(unwrapped_id)]
				if callee.kind !in [.paren, .cast_expr, .expr_stmt] || callee.children_count != 1 {
					if was_wrapped && callee.kind == .selector {
						return true
					}
					break
				}
				was_wrapped = true
				unwrapped_id = a.child(&callee, 0)
			}
		}
	}
	for idx, node in a.nodes {
		if node.kind == .selector && node.children_count > 0 && idx !in call_callees {
			base := a.child_node(&node, 0)
			if base.kind in [.string_literal, .int_literal, .float_literal, .char_literal]
				|| tc.expr_is_method_value(flat.NodeId(idx)) {
				return true
			}
		}
	}
	for idx, node in a.nodes {
		if idx >= a.user_code_start && node.kind == .lambda_expr {
			// Lambda captures are inferred during transform, after markused has run.
			// Conservatively keep the runtime reachable for source/import lambdas, while
			// ignoring implementation lambdas parsed as part of builtin itself.
			return true
		}
		if node.kind != .fn_literal {
			continue
		}
		for i in 0 .. node.children_count {
			if a.child_node(&node, i).kind == .ident {
				return true
			}
		}
	}
	return false
}

fn markused_is_builtin_closure_global(a &flat.FlatAst, tc &types.TypeChecker, node flat.Node) bool {
	if node.kind != .global_decl {
		return false
	}
	file := a.source_files[node.pos.id] or { return false }
	return tc.file_modules[file.name] == 'closure'
		&& os.dir(file.name).replace('\\', '/').ends_with('vlib/builtin/closure')
}

fn enqueue_ierror_equality_dispatch_helpers(tc &types.TypeChecker, mut used map[string]bool, mut queue []string, mut scan InterfaceDispatchScan) {
	if !scan.built {
		scan.build(tc)
	}
	// `used` only grows, so the IError dispatch keys stay enqueued once added.
	if !scan.ierror_keys_enqueued {
		scan.ierror_keys_enqueued = true
		for key in scan.ierror_keys {
			enqueue(key, mut used, mut queue)
		}
	}
	enqueue_used_interface_dispatch_implementers(tc, mut used, mut queue, mut scan)
}

fn markused_type_equality_uses_ierror(typ types.Type, tc &types.TypeChecker, mut cache map[u32]int) bool {
	match typ {
		types.Primitive, types.String, types.Char, types.Rune, types.ISize, types.USize,
		types.Void, types.Nil, types.None {
			return false
		}
		else {}
	}
	key := typ.source_type_id()
	if cached := cache[key] {
		return cached == 1
	}
	cache[key] = 2
	result := markused_type_equality_uses_ierror_uncached(typ, tc, mut cache)
	cache[key] = if result { 1 } else { -1 }
	return result
}

fn markused_type_equality_uses_ierror_uncached(typ types.Type, tc &types.TypeChecker, mut cache map[u32]int) bool {
	match typ {
		types.Alias {
			return markused_type_equality_uses_ierror(typ.base_type, tc, mut cache)
		}
		types.OptionType {
			return markused_type_equality_uses_ierror(typ.base_type, tc, mut cache)
		}
		types.ResultType {
			return markused_type_equality_uses_ierror(typ.base_type, tc, mut cache)
		}
		types.Array {
			return markused_type_equality_uses_ierror(typ.elem_type, tc, mut cache)
		}
		types.ArrayFixed {
			return markused_type_equality_uses_ierror(typ.elem_type, tc, mut cache)
		}
		types.Map {
			return markused_type_equality_uses_ierror(typ.key_type, tc, mut cache)
				|| markused_type_equality_uses_ierror(typ.value_type, tc, mut cache)
		}
		types.Struct {
			for field in markused_struct_fields(typ.source_type_id(), tc) {
				if markused_type_equality_uses_ierror(field.typ, tc, mut cache) {
					return true
				}
			}
		}
		types.SumType {
			for variant in markused_sum_variants(typ.source_type_id(), tc) {
				if markused_type_equality_uses_ierror(tc.parse_type(variant), tc, mut cache) {
					return true
				}
			}
		}
		types.Interface {
			if markused_is_ierror_interface_name(typ.source_type_id()) {
				return true
			}
			for concrete in markused_interface_impl_names(typ.source_type_id(), tc) {
				if markused_type_equality_uses_ierror(tc.parse_type(concrete), tc, mut cache) {
					return true
				}
			}
		}
		types.MultiReturn {
			for item in typ.types {
				if markused_type_equality_uses_ierror(item, tc, mut cache) {
					return true
				}
			}
		}
		else {}
	}

	return false
}

fn markused_is_map_runtime_seed(name string) bool {
	return name.starts_with('map.') || name.starts_with('map_')
}

fn (mut scan RuntimeHelpersScan) enqueue_map_runtime(mut used map[string]bool, mut queue []string) {
	scan.map_runtime_enqueued = true
	// Container initialization, indexing and ownership lowering can introduce
	// calls after markused. Retain the existing map runtime group as one unit.
	enqueue('new_map', mut used, mut queue)
	for seeds in [core_runtime_seeds, lowered_runtime_seeds, ownership_runtime_seeds] {
		for seed in seeds {
			if markused_is_map_runtime_seed(seed) {
				enqueue(seed, mut used, mut queue)
			}
		}
	}
}

fn (mut scan RuntimeHelpersScan) type_needs_map_runtime(typ types.Type, tc &types.TypeChecker) bool {
	match typ {
		types.Primitive, types.String, types.Char, types.Rune, types.ISize, types.USize, types.Void,
		types.Nil, types.None {
			return false
		}
		else {}
	}
	if typ is types.Unknown {
		// Markused precedes monomorphization. A reached generic value may
		// become a map even when its placeholder has no concrete type yet.
		return typ.generic
	}
	key := typ.source_type_id()
	if cached := scan.map_type_cache[key] {
		return cached == 1
	}
	scan.map_type_cache[key] = 2
	mut needed := false
	match typ {
		types.Map {
			needed = true
		}
		types.Alias, types.Pointer, types.OptionType, types.ResultType {
			needed = scan.type_needs_map_runtime(typ.base_type, tc)
		}
		types.Array, types.ArrayFixed, types.Channel {
			needed = scan.type_needs_map_runtime(typ.elem_type, tc)
		}
		types.Struct {
			for field in tc.struct_fields_for_type(typ.source_type_id()) {
				if scan.type_needs_map_runtime(field.typ, tc) {
					needed = true
					break
				}
			}
		}
		types.SumType {
			for variant in markused_sum_variants(typ.source_type_id(), tc) {
				if scan.type_needs_map_runtime(tc.parse_type(variant), tc) {
					needed = true
					break
				}
			}
		}
		types.Interface {
			for impl in markused_interface_impl_names(typ.source_type_id(), tc) {
				if scan.type_needs_map_runtime(tc.parse_type(impl), tc) {
					needed = true
					break
				}
			}
		}
		types.FnType {
			needed = scan.type_needs_map_runtime(typ.return_type, tc)
			for param in typ.params {
				if scan.type_needs_map_runtime(param.typ, tc) {
					needed = true
					break
				}
			}
		}
		types.MultiReturn {
			for part in typ.types {
				if scan.type_needs_map_runtime(part, tc) {
					needed = true
					break
				}
			}
		}
		else {}
	}
	scan.map_type_cache[key] = if needed { 1 } else { -1 }
	return needed
}

fn markused_type_text_needs_shared_runtime(type_id u32) bool {
	return flat.source_type_query(type_id).has(.shared_modifier)
}

fn markused_infix_needs_f32_eq_epsilon(a &flat.FlatAst, tc &types.TypeChecker, node flat.Node) bool {
	if node.children_count < 2 {
		return false
	}
	return markused_type_is_f32(tc.resolve_type(a.child(&node, 0)))
		|| markused_type_is_f32(tc.resolve_type(a.child(&node, 1)))
}

fn markused_type_is_f32(typ types.Type) bool {
	if typ is types.Alias {
		return markused_type_is_f32(typ.base_type)
	}
	return typ.source_type_id() == flat.source_type_f32
}

fn markused_expr_stringifies_channel(tc &types.TypeChecker, id flat.NodeId, cur_module string, mut cache map[string]&MarkusedTypeResults) bool {
	typ := tc.expr_type(id) or { tc.resolve_type(id) }
	return markused_type_stringifies_channel(typ, cur_module, tc, mut cache)
}

fn markused_type_stringifies_channel(typ types.Type, cur_module string, tc &types.TypeChecker, mut cache map[string]&MarkusedTypeResults) bool {
	match typ {
		types.Primitive, types.String, types.Char, types.Rune, types.ISize, types.USize,
		types.Void, types.Nil, types.None {
			return false
		}
		else {}
	}
	mut module_cache := cache[cur_module] or {
		fresh := &MarkusedTypeResults{}
		cache[cur_module] = fresh
		fresh
	}
	key := typ.source_type_id()
	if cached := module_cache.entries[key] {
		return cached == 1
	}
	module_cache.entries[key] = 2
	result := markused_type_stringifies_channel_uncached(typ, cur_module, tc, mut cache)
	module_cache.entries[key] = if result { 1 } else { -1 }
	return result
}

fn markused_type_stringifies_channel_uncached(typ types.Type, cur_module string, tc &types.TypeChecker, mut cache map[string]&MarkusedTypeResults) bool {
	match typ {
		types.Channel {
			return true
		}
		types.Alias {
			if markused_type_has_custom_str(typ.source_type_id(), cur_module, tc) {
				return false
			}
			return markused_type_stringifies_channel(typ.base_type, cur_module, tc, mut cache)
		}
		types.Pointer {
			return markused_type_stringifies_channel(typ.base_type, cur_module, tc, mut cache)
		}
		types.OptionType {
			return markused_type_stringifies_channel(typ.base_type, cur_module, tc, mut cache)
		}
		types.ResultType {
			return markused_type_stringifies_channel(typ.base_type, cur_module, tc, mut cache)
		}
		types.Array {
			return markused_type_stringifies_channel(typ.elem_type, cur_module, tc, mut cache)
		}
		types.ArrayFixed {
			return markused_type_stringifies_channel(typ.elem_type, cur_module, tc, mut cache)
		}
		types.Map {
			return markused_type_stringifies_channel(typ.key_type, cur_module, tc, mut cache)
				|| markused_type_stringifies_channel(typ.value_type, cur_module, tc, mut cache)
		}
		types.Struct {
			if markused_type_has_custom_str(typ.source_type_id(), cur_module, tc) {
				return false
			}
			if flat.source_type_syntax(typ.source_type_id()).channel_fragment() {
				return true
			}
			for field in markused_struct_fields(typ.source_type_id(), tc) {
				if markused_type_stringifies_channel(field.typ, cur_module, tc, mut cache) {
					return true
				}
			}
		}
		types.SumType {
			if markused_type_has_custom_str(typ.source_type_id(), cur_module, tc) {
				return false
			}
			for variant in markused_sum_variants(typ.source_type_id(), tc) {
				if markused_type_stringifies_channel(tc.parse_type(variant), cur_module, tc, mut cache) {
					return true
				}
			}
		}
		types.Interface {
			if 'str' in tc.interface_abstract_method_names(typ.source_type_id()) {
				return false
			}
			for concrete in markused_interface_impl_names(typ.source_type_id(), tc) {
				if markused_type_stringifies_channel(tc.parse_type(concrete), cur_module, tc, mut cache) {
					return true
				}
			}
		}
		types.MultiReturn {
			for item in typ.types {
				if markused_type_stringifies_channel(item, cur_module, tc, mut cache) {
					return true
				}
			}
		}
		else {}
	}

	return false
}

fn markused_type_has_custom_str(name_id u32, cur_module string, tc &types.TypeChecker) bool {
	for candidate_id in stringification_type_candidates(name_id, cur_module) {
		candidate := flat.source_type_name(candidate_id)
		if '${candidate}.str' in tc.fn_ret_types
			|| '${markused_c_name(candidate)}__str' in tc.fn_ret_types {
			return true
		}
	}
	for candidate_id in generic_stringification_type_candidates(name_id, cur_module, tc) {
		candidate := flat.source_type_name(candidate_id)
		if '${candidate}.str' in tc.fn_ret_types
			|| '${markused_c_name(candidate)}__str' in tc.fn_ret_types {
			return true
		}
	}
	return false
}

// enqueue_debugger_custom_str_methods retains formatters that debugger scope rendering may call.
// Those calls are synthesized by cgen after ordinary AST reachability has been computed.
fn enqueue_debugger_custom_str_methods(tc &types.TypeChecker, mut used map[string]bool, mut queue []string) {
	for method, ret_type in tc.fn_ret_types {
		if method.all_after_last('.') != 'str' || ret_type.source_type_id() != flat.source_type_string {
			continue
		}
		params := tc.fn_param_types[method] or { continue }
		if params.len != 1 {
			continue
		}
		enqueue(method, mut used, mut queue)
		lowered := markused_c_name(method)
		if lowered != method {
			enqueue(lowered, mut used, mut queue)
		}
	}
}

fn markused_struct_fields(name_id u32, tc &types.TypeChecker) []types.StructField {
	mut candidates := [name_id]
	base_name := types.generic_base_type_id(name_id)
	if base_name != name_id {
		candidates << base_name
	}
	qualified := tc.qualify_type_name(name_id)
	if qualified !in candidates {
		candidates << qualified
	}
	if flat.source_type_shape(name_id).has(.dotted) {
		short_name := flat.source_type_syntax(name_id).unqualified()
		if short_name !in candidates {
			candidates << short_name
		}
	}
	for candidate in candidates {
		if fields := tc.structs[candidate] {
			return fields
		}
	}
	return []types.StructField{}
}

fn markused_sum_variants(name_id u32, tc &types.TypeChecker) []u32 {
	mut candidates := [name_id]
	qualified := tc.qualify_type_name(name_id)
	if qualified !in candidates {
		candidates << qualified
	}
	if flat.source_type_shape(name_id).has(.dotted) {
		short_name := flat.source_type_syntax(name_id).unqualified()
		if short_name !in candidates {
			candidates << short_name
		}
	}
	for candidate in candidates {
		if variants := tc.sum_types.type_ids(candidate) {
			return variants.clone()
		}
	}
	return []u32{}
}

fn markused_type_text_is_channel(type_id u32) bool {
	return flat.source_type_query(type_id).has(.channel)
}

fn markused_call_lowers_to_join_path_single(a &flat.FlatAst, fn_node flat.Node, imports map[string]string, resolved_call string) bool {
	if resolved_call.len > 0 {
		return resolved_call == 'os.join_path'
	}
	if fn_node.kind == .ident {
		return fn_node.value == 'os.join_path'
	}
	if fn_node.kind != .selector || fn_node.value != 'join_path' || fn_node.children_count == 0 {
		return false
	}
	base_id := a.child(&fn_node, 0)
	if int(base_id) < 0 {
		return false
	}
	base := a.node(base_id)
	if base.kind != .ident {
		return false
	}
	return base.value == 'os' || imports[base.value] == 'os'
}

fn markused_type_name_lowers_to_map_str(type_name_id u32, tc &types.TypeChecker) bool {
	if flat.source_type_shape(markused_clean_map_type(type_name_id)).has(.map_type) {
		return true
	}
	return markused_type_lowers_to_map_str(tc.parse_type(type_name_id))
}

fn markused_type_lowers_to_map_str(typ0 types.Type) bool {
	mut typ := typ0
	for _ in 0 .. 8 {
		if typ is types.Alias {
			typ = typ.base_type
			continue
		}
		if typ is types.Pointer {
			typ = typ.base_type
			continue
		}
		break
	}
	return typ is types.Map || flat.source_type_shape(markused_clean_map_type(typ.source_type_id())).has(.map_type)
}

fn markused_auto_str_skipped_fields(a &flat.FlatAst) map[u32]&MarkusedSkippedFields {
	mut skipped := map[u32]&MarkusedSkippedFields{}
	mut module_name := 'main'
	for node_idx in 0 .. a.nodes.len {
		// Read the node in place: most nodes are neither modules nor structs.
		node := unsafe { &a.nodes[node_idx] }
		if node.kind == .module_decl {
			module_name = if node.value.len > 0 { node.value } else { 'main' }
			continue
		}
		if node.kind != .struct_decl {
			continue
		}
		qualified := if module_name in ['', 'main', 'builtin'] {
			node.value
		} else {
			'${module_name}.${node.value}'
		}
		for i in 0 .. node.children_count {
			field := a.child_node(node, i)
			if field.kind != .field_decl {
				continue
			}
			params := field.generic_params()
			if params.len < 2 {
				continue
			}
			for attr in params[1..] {
				parts := attr.split_nth(':', 2)
				if parts.len == 2 && parts[0].trim_space() == 'str'
					&& parts[1].trim_space().trim('\'"') == 'skip' {
					markused_add_skipped_field(mut skipped, flat.source_type_id(qualified), field.value)
					markused_add_skipped_field(mut skipped, node.source_value_id(), field.value)
					break
				}
			}
		}
	}
	return skipped
}

// markused_has_node_kind reports whether the AST has a node of `kind`, reading
// the nodes in place instead of copying each one as `a.nodes.any()` does.
fn markused_has_node_kind(a &flat.FlatAst, kind flat.NodeKind) bool {
	for node_idx in 0 .. a.nodes.len {
		if unsafe { a.nodes[node_idx].kind } == kind {
			return true
		}
	}
	return false
}

// markused_has_closure_runtime_import reports whether a file imports the closure
// runtime other than through the import that every program gets.
fn markused_has_closure_runtime_import(a &flat.FlatAst) bool {
	for node_idx in 0 .. a.nodes.len {
		node := unsafe { &a.nodes[node_idx] }
		if node.kind == .import_decl && node.value == 'builtin.closure'
			&& node.typ != markused_source_type_closure_runtime {
			return true
		}
	}
	return false
}

fn markused_add_skipped_field(mut skipped map[u32]&MarkusedSkippedFields, owner u32, field string) {
	mut entry := skipped[owner] or {
		fresh := &MarkusedSkippedFields{}
		skipped[owner] = fresh
		fresh
	}
	entry.fields[field] = true
}

fn markused_auto_str_field_is_skipped(type_name_id u32, field_name string, skipped map[u32]&MarkusedSkippedFields) bool {
	base, _, is_generic := markused_generic_app_parts(type_name_id)
	lookup := if is_generic { base } else { type_name_id }
	if entry := skipped[lookup] {
		if entry.fields[field_name] {
			return true
		}
	}
	if !flat.source_type_shape(lookup).has(.dotted) {
		return false
	}
	short := flat.source_type_syntax(lookup).unqualified()
	if short != lookup {
		if entry := skipped[short] {
			return entry.fields[field_name]
		}
	}
	return false
}

struct StringifiedTypes {
mut:
	seen map[u32]bool
}

// Every scan appends to one monotonically growing reachability queue. Once a
// type's formatters have been queued in a module, another expression of that
// type needs no dependency traversal. Recheck after generated declarations grow.
fn (mut scan RuntimeHelpersScan) enqueue_stringification(expr_id flat.NodeId, cur_module string, tc &types.TypeChecker, mut used map[string]bool, mut queue []string) {
	if scan.stringification_node_count != tc.a.nodes.len
		|| scan.stringification_fn_count != tc.fn_ret_types.len {
		scan.stringification_seen.clear()
		scan.stringification_node_count = tc.a.nodes.len
		scan.stringification_fn_count = tc.fn_ret_types.len
	}
	mut memo := scan.stringification_seen[cur_module] or {
		fresh := &StringifiedTypes{}
		scan.stringification_seen[cur_module] = fresh
		fresh
	}
	typ := tc.expr_type(expr_id) or { tc.resolve_type(expr_id) }
	enqueue_stringified_type_dependencies(typ, cur_module, tc, scan.auto_str_skipped_fields,
		mut used, mut queue, mut memo.seen)
}

// enqueue_stringified_custom_str_method retains custom str methods needed by direct and nested
// automatic stringification. The checker supplies concretely substituted generic field types.
fn enqueue_stringified_custom_str_method(expr_id flat.NodeId, cur_module string, tc &types.TypeChecker, skipped_fields map[u32]&MarkusedSkippedFields, mut used map[string]bool, mut queue []string) {
	typ := tc.expr_type(expr_id) or { tc.resolve_type(expr_id) }
	mut seen := map[u32]bool{}
	enqueue_stringified_type_dependencies(typ, cur_module, tc, skipped_fields, mut used,
		mut queue, mut seen)
}

fn enqueue_stringified_type_dependencies(typ types.Type, cur_module string, tc &types.TypeChecker, skipped_fields map[u32]&MarkusedSkippedFields, mut used map[string]bool, mut queue []string, mut seen map[u32]bool) {
	type_name_id := typ.source_type_id()
	if type_name_id != 0 {
		if seen[type_name_id] {
			return
		}
		seen[type_name_id] = true
	}
	match typ {
		types.Alias {
			if enqueue_structlike_str_method(type_name_id, cur_module, tc, mut used, mut queue) {
				return
			}
			enqueue_stringified_type_dependencies(typ.base_type, cur_module, tc, skipped_fields,
				mut used, mut queue, mut seen)
		}
		types.OptionType {
			enqueue_stringified_type_dependencies(typ.base_type, cur_module, tc, skipped_fields,
				mut used, mut queue, mut seen)
		}
		types.ResultType {
			enqueue_stringified_type_dependencies(typ.base_type, cur_module, tc, skipped_fields,
				mut used, mut queue, mut seen)
		}
		types.Pointer {
			base := *typ.base_type
			if base is types.Struct || base is types.SumType || base is types.Interface
				|| base is types.Enum || base is types.Alias || base is types.Map
				|| base is types.Array || base is types.ArrayFixed {
				enqueue_stringified_type_dependencies(base, cur_module, tc, skipped_fields,
					mut used, mut queue, mut seen)
			}
		}
		types.Primitive, types.Rune, types.Char, types.ISize, types.USize, types.String {
			enqueue_stringified_primitive_helpers(type_name_id, mut used, mut queue)
		}
		types.Enum {
			enqueue_enum_str_method(type_name_id, cur_module, tc, mut used, mut queue)
		}
		types.Struct {
			if enqueue_structlike_str_method(type_name_id, cur_module, tc, mut used, mut queue) {
				return
			}
			for field in tc.struct_fields_for_type(type_name_id) {
				if markused_auto_str_field_is_skipped(type_name_id, field.name, skipped_fields) {
					continue
				}
				enqueue_stringified_type_dependencies(field.typ, cur_module, tc, skipped_fields,
					mut used, mut queue, mut seen)
			}
		}
		types.SumType {
			if enqueue_structlike_str_method(type_name_id, cur_module, tc, mut used, mut queue) {
				return
			}
			for variant in markused_sum_variants(type_name_id, tc) {
				enqueue_stringified_type_dependencies(tc.parse_type(variant), cur_module, tc,
					skipped_fields, mut used, mut queue, mut seen)
			}
		}
		types.Interface {
			enqueue_interface_str_methods(type_name_id, tc, mut used, mut queue)
		}
		types.Array {
			if !enqueue_structlike_str_method(type_name_id, cur_module, tc, mut used, mut queue) {
				enqueue_stringified_type_dependencies(typ.elem_type, cur_module, tc,
					skipped_fields, mut used, mut queue, mut seen)
			}
		}
		types.ArrayFixed {
			enqueue_stringified_type_dependencies(typ.elem_type, cur_module, tc, skipped_fields,
				mut used, mut queue, mut seen)
		}
		types.Map {
			// The generated map formatter has runtime branches for every scalar
			// kind, so even an integer map needs its float conversion callee.
			enqueue_implicit_map_str_helpers(mut used, mut queue)
			enqueue_stringified_type_dependencies(typ.key_type, cur_module, tc, skipped_fields,
				mut used, mut queue, mut seen)
			enqueue_stringified_type_dependencies(typ.value_type, cur_module, tc, skipped_fields,
				mut used, mut queue, mut seen)
		}
		else {}
	}
}

fn enqueue_interface_str_methods(iface_name_id u32, tc &types.TypeChecker, mut used map[string]bool, mut queue []string) {
	enqueue('${flat.source_type_name(iface_name_id)}.str', mut used, mut queue)
	for impl in markused_interface_impl_names(iface_name_id, tc) {
		impl_name := flat.source_type_name(impl)
		method := '${impl_name}.str'
		enqueue(method, mut used, mut queue)
		lowered := markused_c_name(method)
		if lowered != method {
			enqueue(lowered, mut used, mut queue)
		}
		short := '${flat.source_type_name(flat.source_type_syntax(impl).unqualified())}.str'
		if short != method {
			enqueue(short, mut used, mut queue)
		}
		enqueue_implicit_interface_str_helpers_for_impl(iface_name_id, 'str', impl, tc, mut used, mut queue)
	}
}

fn enqueue_implicit_interface_str_helpers_for_impl(iface_name_id u32, method string, impl_id u32, tc &types.TypeChecker, mut used map[string]bool, mut queue []string) {
	if method != 'str' || !markused_interface_method_is_plain_str(iface_name_id, tc) {
		return
	}
	method_key := tc.concrete_method_signature_key(impl_id, method) or { '${flat.source_type_name(impl_id)}.${method}' }
	if method_key in tc.fn_param_types || markused_c_name(method_key) in tc.fn_param_types {
		return
	}
	enqueue_implicit_interface_str_helpers(tc.parse_type(impl_id), tc, mut used, mut queue)
}

fn enqueue_implicit_interface_str_helpers(typ types.Type, tc &types.TypeChecker, mut used map[string]bool, mut queue []string) {
	mut seen := map[u32]bool{}
	enqueue_implicit_interface_str_helpers_inner(typ, tc, mut used, mut queue, mut seen)
}

fn markused_interface_method_is_plain_str(iface_name_id u32, tc &types.TypeChecker) bool {
	key := tc.interface_method_signature_key(iface_name_id, 'str') or { return false }
	ret := tc.fn_ret_types[key] or { return false }
	if ret.source_type_id() != flat.source_type_string {
		return false
	}
	params := tc.fn_param_types[key] or { return false }
	return params.len == 1
}

fn enqueue_implicit_interface_str_helpers_inner(typ types.Type, tc &types.TypeChecker, mut used map[string]bool, mut queue []string, mut seen map[u32]bool) {
	enqueue('string__plus', mut used, mut queue)
	typ_name_id := typ.source_type_id()
	if typ_name_id in seen {
		return
	}
	seen[typ_name_id] = true
	match typ {
		types.Unknown {
			// Before monomorphization an implicit interface implementer can still
			// expose its declaration's generic field type. Keep both float widths
			// until the concrete field selected by the boxer is available.
			if !tc.nofloat && typ.generic {
				enqueue_stringified_primitive_helpers(flat.source_type_f32, mut used, mut queue)
				enqueue_stringified_primitive_helpers(flat.source_type_f64, mut used, mut queue)
			}
		}
		types.Alias {
			if enqueue_structlike_str_method(typ_name_id, '', tc, mut used, mut queue) {
				return
			}
			enqueue_implicit_interface_str_helpers_inner(typ.base_type, tc, mut used, mut queue, mut seen)
		}
		types.Pointer, types.OptionType, types.ResultType {
			enqueue_implicit_interface_str_helpers_inner(typ.base_type, tc, mut used, mut queue, mut seen)
		}
		types.Primitive, types.Rune, types.Char, types.ISize, types.USize, types.String {
			enqueue_stringified_primitive_helpers(types.Type(typ).source_type_id(), mut used, mut queue)
		}
		types.Enum {
			enqueue('${markused_c_name(typ.name())}__autostr', mut used, mut queue)
		}
		types.Array {
			enqueue_implicit_interface_str_helpers_inner(typ.elem_type, tc, mut used, mut queue, mut seen)
		}
		types.ArrayFixed {
			enqueue_implicit_interface_str_helpers_inner(typ.elem_type, tc, mut used, mut queue, mut seen)
		}
		types.Map {
			for helper in ['i64.str', 'i64__str', 'u64.str', 'u64__str', 'f64.str', 'f64__str',
				'rune.str', 'rune__str'] {
				enqueue(helper, mut used, mut queue)
			}
			enqueue_implicit_interface_str_helpers_inner(typ.key_type, tc, mut used, mut queue, mut seen)
			enqueue_implicit_interface_str_helpers_inner(typ.value_type, tc, mut used, mut queue, mut seen)
		}
		types.Struct {
			if enqueue_structlike_str_method(typ_name_id, '', tc, mut used, mut queue) {
				return
			}
			for field in tc.struct_fields_for_type(typ.source_type_id()) {
				enqueue_implicit_interface_str_helpers_inner(field.typ, tc, mut used, mut queue, mut seen)
			}
		}
		types.SumType {
			if enqueue_structlike_str_method(typ_name_id, '', tc, mut used, mut queue) {
				return
			}
			for variant in markused_sum_variants(typ.source_type_id(), tc) {
				enqueue_implicit_interface_str_helpers_inner(tc.parse_type(variant), tc, mut used,
					mut queue, mut seen)
			}
		}
		types.Interface {
			for impl in markused_interface_impl_names(typ.source_type_id(), tc) {
				enqueue_implicit_interface_str_helpers_inner(tc.parse_type(impl), tc, mut used,
					mut queue, mut seen)
			}
		}
		else {}
	}
}

fn enqueue_implicit_interface_str_dispatch_helpers(iface_name_id u32, tc &types.TypeChecker, mut used map[string]bool, mut queue []string) {
	if !tc.interface_accepts_implicit_str(iface_name_id) {
		return
	}
	enqueue('string__plus', mut used, mut queue)
	for impl in markused_interface_impl_names(iface_name_id, tc) {
		if !tc.type_has_implicit_str_method(impl) {
			continue
		}
		mut seen := map[u32]bool{}
		enqueue_implicit_str_type_helpers(tc.parse_type(impl), tc, mut used, mut queue, mut seen)
	}
}

fn enqueue_implicit_str_type_helpers(typ types.Type, tc &types.TypeChecker, mut used map[string]bool, mut queue []string, mut seen map[u32]bool) {
	key_id := typ.source_type_id()
	if key_id != 0 {
		if seen[key_id] {
			return
		}
		seen[key_id] = true
		enqueue_structlike_str_method(key_id, '', tc, mut used, mut queue)
	}
	match typ {
		types.Alias {
			enqueue_implicit_str_type_helpers(typ.base_type, tc, mut used, mut queue, mut seen)
		}
		types.Pointer {
			enqueue_implicit_str_type_helpers(typ.base_type, tc, mut used, mut queue, mut seen)
		}
		types.Primitive, types.Rune, types.Char, types.ISize, types.USize {
			enqueue_stringified_primitive_helpers(key_id, mut used, mut queue)
		}
		types.Enum {
			enqueue_enum_str_method(key_id, '', tc, mut used, mut queue)
		}
		types.Struct {
			for field in tc.structs[key_id] or { []types.StructField{} } {
				enqueue_implicit_str_type_helpers(field.typ, tc, mut used, mut queue, mut seen)
			}
		}
		types.Array {
			enqueue('string__plus', mut used, mut queue)
			enqueue('array.get', mut used, mut queue)
			enqueue('array__get', mut used, mut queue)
			enqueue_implicit_str_type_helpers(typ.elem_type, tc, mut used, mut queue, mut seen)
		}
		types.ArrayFixed {
			enqueue('string__plus', mut used, mut queue)
			enqueue_implicit_str_type_helpers(typ.elem_type, tc, mut used, mut queue, mut seen)
		}
		types.OptionType {
			enqueue('string__plus', mut used, mut queue)
			enqueue_implicit_str_type_helpers(typ.base_type, tc, mut used, mut queue, mut seen)
		}
		types.ResultType {
			enqueue('string__plus', mut used, mut queue)
			enqueue_implicit_str_type_helpers(typ.base_type, tc, mut used, mut queue, mut seen)
		}
		types.Map {
			enqueue_implicit_map_str_helpers(mut used, mut queue)
			enqueue_implicit_str_type_helpers(typ.key_type, tc, mut used, mut queue, mut seen)
			enqueue_implicit_str_type_helpers(typ.value_type, tc, mut used, mut queue, mut seen)
		}
		else {}
	}
}

fn enqueue_implicit_map_str_helpers(mut used map[string]bool, mut queue []string) {
	enqueue('string__plus', mut used, mut queue)
	for primitive in ['i64', 'u64', 'f64', 'rune'] {
		enqueue_stringified_primitive_helpers(flat.source_type_id(primitive), mut used, mut queue)
	}
}

fn enqueue_stringified_primitive_helpers(type_name_id u32, mut used map[string]bool, mut queue []string) {
	if type_name_id == flat.source_type_bool {
		enqueue('bool.str', mut used, mut queue)
		enqueue(markused_c_name('bool.str'), mut used, mut queue)
	} else if type_name_id == flat.source_type_rune || type_name_id == flat.source_type_char {
		enqueue('rune.str', mut used, mut queue)
		enqueue(markused_c_name('rune.str'), mut used, mut queue)
	} else if type_name_id == flat.source_type_int || type_name_id == flat.source_type_i8 || type_name_id == flat.source_type_i16 || type_name_id == flat.source_type_i32 || type_name_id == flat.source_type_i64 {
		enqueue('${flat.source_type_name(type_name_id)}.str', mut used, mut queue)
		enqueue(markused_c_name('${flat.source_type_name(type_name_id)}.str'), mut used, mut queue)
		enqueue('i64.str', mut used, mut queue)
		enqueue(markused_c_name('i64.str'), mut used, mut queue)
		enqueue('strconv__format_int', mut used, mut queue)
	} else if type_name_id == flat.source_type_isize {
		enqueue('i64.str', mut used, mut queue)
		enqueue(markused_c_name('i64.str'), mut used, mut queue)
		enqueue('strconv__format_int', mut used, mut queue)
	} else if type_name_id == flat.source_type_u8 || type_name_id == flat.source_type_u16 || type_name_id == flat.source_type_u32 || type_name_id == flat.source_type_uint {
		enqueue('u64.str', mut used, mut queue)
		enqueue(markused_c_name('u64.str'), mut used, mut queue)
		enqueue('strconv__format_uint', mut used, mut queue)
	} else if type_name_id == flat.source_type_u64 {
		enqueue('u64.str', mut used, mut queue)
		enqueue(markused_c_name('u64.str'), mut used, mut queue)
		enqueue('strconv__format_uint', mut used, mut queue)
	} else if type_name_id == markused_source_type_u128 {
		enqueue('u128.str', mut used, mut queue)
		enqueue(markused_c_name('u128.str'), mut used, mut queue)
		enqueue('u128.str_base', mut used, mut queue)
		enqueue(markused_c_name('u128.str_base'), mut used, mut queue)
		enqueue('u128.char_str', mut used, mut queue)
		enqueue(markused_c_name('u128.char_str'), mut used, mut queue)
	} else if type_name_id == markused_source_type_i128 {
		enqueue('i128.str', mut used, mut queue)
		enqueue(markused_c_name('i128.str'), mut used, mut queue)
		// A signed value formats in another base from its bit pattern, which is
		// the unsigned method's job.
		enqueue('u128.str_base', mut used, mut queue)
		enqueue(markused_c_name('u128.str_base'), mut used, mut queue)
		enqueue('i128.char_str', mut used, mut queue)
		enqueue(markused_c_name('i128.char_str'), mut used, mut queue)
	} else if type_name_id == flat.source_type_f32 {
		enqueue('f32.str', mut used, mut queue)
		enqueue(markused_c_name('f32.str'), mut used, mut queue)
		enqueue('f64.str', mut used, mut queue)
		enqueue(markused_c_name('f64.str'), mut used, mut queue)
		enqueue('strconv__f32_to_str_l', mut used, mut queue)
	} else if type_name_id == flat.source_type_f64 || type_name_id == markused_source_type_float_literal {
		enqueue('f64.str', mut used, mut queue)
		enqueue(markused_c_name('f64.str'), mut used, mut queue)
		enqueue('strconv__f64_to_str_l', mut used, mut queue)
	} else {
	}
}

// enqueue_enum_str_method supports enqueue enum str method handling for markused.
fn enqueue_enum_str_method(type_name_id u32, cur_module string, tc &types.TypeChecker, mut used map[string]bool, mut queue []string) {
	mut has_custom_method := false
	for candidate_id in stringification_type_candidates(type_name_id, cur_module) {
		candidate := flat.source_type_name(candidate_id)
		method := '${candidate}.str'
		if method in tc.fn_ret_types {
			has_custom_method = true
			enqueue(method, mut used, mut queue)
			lowered := markused_c_name(method)
			if lowered != method {
				enqueue(lowered, mut used, mut queue)
			}
		}
	}
	if has_custom_method {
		return
	}
	parsed := tc.parse_type(type_name_id)
	if parsed is types.Enum {
		enqueue('${markused_c_name(parsed.name())}__autostr', mut used, mut queue)
	}
}

// enqueue_structlike_str_method supports enqueue structlike str method handling for markused.

fn enqueue_structlike_str_method(type_name_id u32, cur_module string, tc &types.TypeChecker, mut used map[string]bool, mut queue []string) bool {
	mut found := false
	for candidate in stringification_type_candidates(type_name_id, cur_module) {
		found = enqueue_structlike_str_candidate(candidate, tc, mut used, mut queue) || found
	}
	for candidate in generic_stringification_type_candidates(type_name_id, cur_module, tc) {
		found = enqueue_structlike_str_candidate(candidate, tc, mut used, mut queue) || found
	}
	if info := tc.resolve_generic_struct_method(type_name_id, 'str') {
		found = true
		enqueue(info.name, mut used, mut queue)
		lowered := markused_c_name(info.name)
		if lowered != info.name {
			enqueue(lowered, mut used, mut queue)
		}
	}
	if method := tc.concrete_method_signature_key(type_name_id, 'str') {
		found = true
		enqueue(method, mut used, mut queue)
		lowered := markused_c_name(method)
		if lowered != method {
			enqueue(lowered, mut used, mut queue)
		}
	}
	return found
}

fn enqueue_structlike_str_candidate(candidate_id u32, tc &types.TypeChecker, mut used map[string]bool, mut queue []string) bool {
	candidate := flat.source_type_name(candidate_id)
	mut found := false
	lowered := '${markused_c_name(candidate)}__str'
	if lowered in tc.fn_ret_types {
		found = true
		enqueue(lowered, mut used, mut queue)
	}
	method := '${candidate}.str'
	if method in tc.fn_ret_types {
		found = true
		enqueue(method, mut used, mut queue)
	}
	return found
}

// stringification_type_candidates supports stringification type candidates handling for markused.
fn stringification_type_candidates(type_name_id u32, cur_module string) []u32 {
	if type_name_id == 0 {
		return []u32{}
	}
	mut candidates := []u32{cap: 2}
	candidates << type_name_id
	if !flat.source_type_shape(type_name_id).has(.dotted) && cur_module.len > 0 && cur_module != 'main'
		&& cur_module != 'builtin' {
		candidates << markused_qualify_source_type(cur_module, type_name_id)
	}
	return candidates
}

fn generic_stringification_type_candidates(type_name_id u32, cur_module string, tc &types.TypeChecker) []u32 {
	base, args, ok := markused_generic_app_parts(type_name_id)
	if !ok || args.len == 0 {
		return []u32{}
	}
	mut candidates := []u32{}
	for base_candidate in stringification_type_candidates(base, cur_module) {
		params := generic_stringification_params(base_candidate, tc) or { continue }
		if params.len != args.len {
			continue
		}
		mut parts := [base_candidate]
		parts << params
		candidates << flat.source_type_composition(.generic, parts)
	}
	return candidates
}

fn generic_stringification_params(base_candidate_id u32, tc &types.TypeChecker) ?[]u32 {
	if params := tc.struct_generic_params[base_candidate_id] {
		return params
	}
	if params := tc.sum_generic_params[base_candidate_id] {
		return params
	}
	return none
}

fn markused_generic_app_parts(typ_id u32) (u32, []u32, bool) {
	clean := markused_trimmed_source_type(typ_id)
	shape := flat.source_type_shape(clean)
	if shape.has(.function_compact) || shape.has(.function_spaced) || !shape.has(.bracketed) || shape.has(.leading_bracket) {
		return u32(0), []u32{}, false
	}
	syntax := flat.source_type_syntax(clean)
	if !syntax.bracket_closed() { return u32(0), []u32{}, false }
	return syntax.generic_base(), syntax.bracket_generic_parts(), true
}

@[direct_array_access]
fn (c &CallCollector) node_tree_uses_generics(root flat.NodeId, cur_module string, imports map[string]string) bool {
	mut stack := [root]
	for stack.len > 0 {
		id := stack.pop()
		if int(id) < 0 || int(id) >= c.a.nodes.len {
			continue
		}
		node := &c.a.nodes[int(id)]
		if c.node_uses_generics(node, cur_module, imports) {
			return true
		}
		if node.kind == .call {
			if name := c.tc.resolved_call_name(id) {
				if c.generic_fn_name_is_known(name, cur_module) {
					return true
				}
			}
		}
		for i in 0 .. node.children_count {
			child_id := c.a.child(node, i)
			if int(child_id) >= 0 {
				stack << child_id
			}
		}
	}
	return false
}

fn (c &CallCollector) node_uses_generics(node &flat.Node, cur_module string, imports map[string]string) bool {
	if c.type_text_uses_generics(node.typ, cur_module, imports) {
		return true
	}
	if node.kind == .string_literal && node.children_count == 1
		&& node.value in ['__v3_comptime_zero', '__v3_comptime_new'] {
		target := c.a.child_node(node, 0)
		return c.type_text_uses_generics(target.source_value_id(), cur_module, imports)
			|| c.type_text_uses_generics(target.typ, cur_module, imports)
	}
	if node.kind !in [.struct_init, .array_init, .map_init, .cast_expr, .as_expr, .sizeof_expr,
		.offsetof_expr, .typeof_expr, .is_expr] {
		return false
	}
	if c.type_text_uses_generics(node.source_value_id(), cur_module, imports) {
		return true
	}
	params := node.generic_params()
	return !flat.source_type_shape(node.source_value_id()).has(.bracketed) && params.len > 0
		&& c.type_text_uses_generics(flat.source_type_id('${node.value_text()}[${params.join(', ')}]'), cur_module, imports)
}

fn (c &CallCollector) type_text_uses_generics(typ_id u32, cur_module string, imports map[string]string) bool {
	if typ_id == 0 {
		return false
	}
	return c.type_text_uses_generics_depth(markused_trimmed_source_type(typ_id), cur_module, imports, 0)
}

fn (c &CallCollector) type_text_uses_generics_depth(typ_id u32, cur_module string, imports map[string]string, depth int) bool {
	// A valid alias chain cannot visit more declarations than these tables contain.
	if typ_id == 0 || depth > c.tc.type_aliases.ids.len + c.selective_alias_targets.len {
		return false
	}
	syntax := flat.source_type_syntax(typ_id)
	for head in syntax.markused_application_heads() {
		if c.generic_type_base_is_known(head, cur_module, imports) { return true }
	}
	if !syntax.contains_capital() { return false }
	clean := markused_trimmed_source_type(markused_unwrapped_source_type(typ_id))
	if target := c.generic_alias_target(clean, cur_module, imports) {
		return c.type_text_uses_generics_depth(markused_trimmed_source_type(target), cur_module, imports, depth + 1)
	}
	if c.selective_alias_uses_generics(clean, cur_module, imports, depth + 1) { return true }
	// Wrapped aliases are not whole type strings, so inspect each named component.
	for candidate in flat.source_type_syntax(clean).markused_generic_components() {
		if target := c.generic_alias_target(candidate, cur_module, imports) {
			if c.type_text_uses_generics_depth(markused_trimmed_source_type(target), cur_module, imports, depth + 1) {
				return true
			}
		} else if c.selective_alias_uses_generics(candidate, cur_module, imports, depth + 1) {
			return true
		}
	}
	return false
}

fn (c &CallCollector) struct_fields_use_generics(name_id u32, cur_module string, imports map[string]string, depth int) bool {
	info := c.struct_decl_info_with_imports(name_id, cur_module, imports) or { return false }
	decl := c.a.node(info.node_id)
	qualified_name := markused_qualify_source_type(info.module, decl.source_value_id())
	fields := c.tc.structs[qualified_name] or { return false }
	field_module := if info.module.len > 0 { info.module } else { cur_module }
	field_imports := c.imports(info.import_context)
	for field in fields {
		if c.type_text_uses_generics_depth(field.typ.source_type_id(), field_module, field_imports, depth) {
			return true
		}
	}
	return false
}

fn (c &CallCollector) emitted_type_declarations_use_generics() bool {
	for name_source_id, fields in c.tc.structs {
		if name_source_id in c.tc.struct_generic_params {
			continue
		}
		for field in fields {
			if c.parsed_type_uses_concrete_generics(field.typ, 0) {
				return true
			}
		}
	}
	for name_source_id, variants_id in c.tc.sum_types.ids {
		if name_source_id in c.tc.sum_generic_params {
			continue
		}
		for variant in variants_id {
			if c.parsed_type_uses_concrete_generics(c.tc.parse_type(variant), 0) {
				return true
			}
		}
	}
	for _, fields in c.tc.interface_fields {
		for field in fields {
			if c.parsed_type_uses_concrete_generics(field.typ, 0) {
				return true
			}
		}
	}
	mut cur_module := ''
	mut imports := map[string]string{}
	for node_idx in c.tc.top_level_idx {
		node := c.a.nodes[node_idx]
		match node.kind {
			.file {
				cur_module = ''
				imports = markused_top_level_file_imports(c.a, node)
			}
			.module_decl {
				cur_module = node.value
			}
			.import_decl {
				imports[node.typ_text()] = node.value
			}
			.const_decl, .global_decl {
				for i in 0 .. node.children_count {
					if c.node_tree_uses_generics(c.a.child(&node, i), cur_module, imports) {
						return true
					}
				}
			}
			else {}
		}
	}
	return false
}

fn (c &CallCollector) parsed_type_uses_concrete_generics(typ types.Type, depth int) bool {
	if depth > 16 {
		return false
	}
	match typ {
		types.Array {
			return c.parsed_type_uses_concrete_generics(typ.elem_type, depth + 1)
		}
		types.ArrayFixed {
			return c.parsed_type_uses_concrete_generics(typ.elem_type, depth + 1)
		}
		types.Channel {
			return c.parsed_type_uses_concrete_generics(typ.elem_type, depth + 1)
		}
		types.Map {
			return c.parsed_type_uses_concrete_generics(typ.key_type, depth + 1)
				|| c.parsed_type_uses_concrete_generics(typ.value_type, depth + 1)
		}
		types.Pointer {
			return c.parsed_type_uses_concrete_generics(typ.base_type, depth + 1)
		}
		types.FnType {
			for parameter in typ.params {
				param := parameter.typ
				if c.parsed_type_uses_concrete_generics(param, depth + 1) {
					return true
				}
			}
			return c.parsed_type_uses_concrete_generics(typ.return_type, depth + 1)
		}
		types.OptionType {
			return c.parsed_type_uses_concrete_generics(typ.base_type, depth + 1)
		}
		types.ResultType {
			return c.parsed_type_uses_concrete_generics(typ.base_type, depth + 1)
		}
		types.Alias {
			return c.parsed_type_uses_concrete_generics(typ.base_type, depth + 1)
		}
		types.Struct {
			return c.named_type_uses_concrete_generics(typ.source_type_id())
		}
		types.Interface {
			return c.named_type_uses_concrete_generics(typ.source_type_id())
		}
		types.SumType {
			return c.named_type_uses_concrete_generics(typ.source_type_id())
		}
		types.MultiReturn {
			for item in typ.types {
				if c.parsed_type_uses_concrete_generics(item, depth + 1) {
					return true
				}
			}
		}
		else {}
	}

	return false
}

fn (c &CallCollector) named_type_uses_concrete_generics(name_id u32) bool {
	base_name_id := types.generic_base_type_id(name_id)
	if base_name_id == name_id { return false }
	return base_name_id in c.tc.struct_generic_params || base_name_id in c.tc.sum_generic_params
}

fn (c &CallCollector) struct_decl_info_with_imports(name_id u32, cur_module string, imports map[string]string) ?StructDeclInfo {
	if info := c.struct_decl_info(name_id, cur_module) { return info }
	if !flat.source_type_shape(name_id).has(.dotted) { return none }
	syntax := flat.source_type_syntax(name_id)
	if module_name := imports[flat.source_type_name(syntax.first_dot_prefix())] {
		resolved := flat.source_type_composition_two(.qualified, flat.source_type_id(module_name), syntax.first_dot_suffix())
		return c.struct_decl_info(resolved, module_name)
	}
	return none
}

fn (c &CallCollector) generic_type_base_is_known(base_id u32, cur_module string, imports map[string]string) bool {
	if base_id in c.generic_type_bases || base_id in c.tc.struct_generic_params || base_id in c.tc.sum_generic_params {
		return true
	}
	if !flat.source_type_shape(base_id).has(.dotted) {
		qualified := markused_qualify_source_type(cur_module, base_id)
		return qualified in c.tc.struct_generic_params || qualified in c.tc.sum_generic_params
	}
	syntax := flat.source_type_syntax(base_id)
	if module_name := imports[flat.source_type_name(syntax.first_dot_prefix())] {
		resolved := flat.source_type_composition_two(.qualified, flat.source_type_id(module_name), syntax.first_dot_suffix())
		return resolved in c.tc.struct_generic_params || resolved in c.tc.sum_generic_params
	}
	return false
}

fn (c &CallCollector) generic_alias_target(name_id u32, cur_module string, imports map[string]string) ?u32 {
	if target := c.tc.type_aliases.type_id(name_id) { return target }
	if !flat.source_type_shape(name_id).has(.dotted) {
		return c.tc.type_aliases.type_id(markused_qualify_source_type(cur_module, name_id))
	}
	syntax := flat.source_type_syntax(name_id)
	if module_name := imports[flat.source_type_name(syntax.first_dot_prefix())] {
		resolved := flat.source_type_composition_two(.qualified, flat.source_type_id(module_name), syntax.first_dot_suffix())
		return c.tc.type_aliases.type_id(resolved)
	}
	return none
}

fn (c &CallCollector) selective_alias_uses_generics(name_id u32, cur_module string, imports map[string]string, depth int) bool {
	if c.selective_alias_targets.len == 0 || flat.source_type_shape(name_id).has(.dotted) {
		return false
	}
	targets := c.selective_alias_targets[name_id] or { return false }
	for target in targets {
		if c.type_text_uses_generics_depth(markused_trimmed_source_type(target), cur_module, imports, depth) {
			return true
		}
	}
	return false
}

// generic_fn_value_name returns the generic function that the index `node`
// names with its type arguments without calling it: `f[int]` of the current
// module, a selectively imported `f[int]`, or `mod.f[int]`. Monomorphization
// specializes such a value, so it is a generic use even without a generic call.
fn (c &CallCollector) generic_fn_value_name(node &flat.Node, base &flat.Node, cur_module string, imports map[string]string, local_values map[string]bool) ?string {
	if base.kind == .ident {
		if base.value.len == 0 || base.value in local_values {
			return none
		}
		if c.generic_fn_name_is_known(base.value, cur_module) {
			qname := qualify_fn(cur_module, base.value)
			return if c.is_known_fn_name(qname) { qname } else { base.value }
		}
		source := c.a.source_files[node.pos.id] or { return none }
		for candidate_id in c.tc.file_selective_import_ids_by_name(source.name, base.value) or {
			return none
		} {
			// Generic functions are keyed by declaration names.
			candidate := flat.source_type_name(candidate_id)
			if candidate in c.tc.fn_generic_params {
				return candidate
			}
		}
		return none
	}
	if base.kind != .selector || base.children_count == 0 {
		return none
	}
	qualifier := c.a.child_node(base, 0)
	if qualifier.kind != .ident || qualifier.value in local_values {
		return none
	}
	module_name := imports[qualifier.value] or { return none }
	name := '${module_name}.${base.value}'
	if name in c.tc.fn_generic_params {
		return name
	}
	return none
}

fn (c &CallCollector) generic_fn_name_is_known(name string, cur_module string) bool {
	if name in c.tc.fn_generic_params || c.generic_receiver_method_name_is_known(name) {
		return true
	}
	qname := qualify_fn(cur_module, name)
	return qname != name && (qname in c.tc.fn_generic_params
		|| c.generic_receiver_method_name_is_known(qname))
}

fn (c &CallCollector) generic_receiver_method_name_is_known(name string) bool {
	if name.index_u8(`[`) < 0 {
		return false
	}
	if name !in c.tc.fn_ret_types && !c.tc.fn_ret_type_texts.contains(name) {
		return false
	}
	receiver := flat.source_type_syntax(flat.source_type_id(name)).qualifier()
	_, args, is_generic := markused_generic_app_parts(receiver)
	return is_generic && flat.source_type_query(receiver).has(.bracket_suffix) && args.len > 0
}

// enqueue_function_value_selectors supports enqueue function value selectors handling for markused.
fn enqueue_function_value_selectors(a &flat.FlatAst, collector CallCollector, fn_decls map[string]FnDeclInfo, has_entry_main bool, mut used map[string]bool, mut queue []string) {
	if has_entry_main {
		enqueue_function_value_selectors_with_entry_main(a, collector, fn_decls, mut used, mut queue)
		return
	}
	ignored_top_level_nodes := markused_ignored_top_level_nodes(a)
	ignored_fn_decl_nodes := markused_ignored_fn_decl_nodes(a)
	shadowed_value_idents := markused_shadowed_value_idents(a)
	for node_idx, node in a.nodes {
		if node_idx < ignored_fn_decl_nodes.len && ignored_fn_decl_nodes[node_idx] {
			continue
		}
		if node_idx < ignored_top_level_nodes.len && ignored_top_level_nodes[node_idx] {
			continue
		}
		if node.kind == .ident && node.value.len > 0 {
			node_id := flat.NodeId(node_idx)
			is_shadowed := node_idx < shadowed_value_idents.len && shadowed_value_idents[node_idx]
			if is_shadowed {
				continue
			}
			if resolved := collector.tc.resolved_fn_value_name(node_id) {
				enqueue(resolved, mut used, mut queue)
				continue
			}
			if node.value in fn_decls && collector.node_is_fn_value(node_id) {
				enqueue(node.value, mut used, mut queue)
			}
			continue
		}
		if node.kind == .selector && node.children_count > 0 && node.value.len > 0 {
			base_id := a.child(&node, 0)
			if int(base_id) >= 0 && int(base_id) < shadowed_value_idents.len
				&& shadowed_value_idents[int(base_id)] {
				continue
			}
			for name in collector.fn_value_selector_names(&node, '', map[string]string{}) {
				if name in fn_decls {
					enqueue(name, mut used, mut queue)
				}
			}
		}
	}
}

fn enqueue_function_value_selectors_with_entry_main(a &flat.FlatAst, collector CallCollector, fn_decls map[string]FnDeclInfo, mut used map[string]bool, mut queue []string) {
	for file_idx in 0 .. a.nodes.len {
		// Only file nodes matter; read the others in place instead of copying them.
		file_node := unsafe { &a.nodes[file_idx] }
		if file_node.kind != .file {
			continue
		}
		for i in 0 .. file_node.children_count {
			child_id := a.child(file_node, i)
			child := a.node(child_id)
			if child.kind == .fn_decl {
				continue
			}
			if file_idx >= a.user_code_start && int(child_id) >= a.user_code_start
				&& markused_is_top_level_stmt(child) {
				continue
			}
			enqueue_function_value_selectors_in_node(a, collector, fn_decls, child_id, mut used, mut queue)
		}
	}
}

fn enqueue_function_value_selectors_in_node(a &flat.FlatAst, collector CallCollector, fn_decls map[string]FnDeclInfo, id flat.NodeId, mut used map[string]bool, mut queue []string) {
	if int(id) < 0 || int(id) >= a.nodes.len {
		return
	}
	node := a.node(id)
	// Struct field defaults are collected when an initializer is reachable, like bodies.
	if node.kind in [.fn_decl, .struct_decl] {
		return
	}
	if node.kind == .ident && node.value.len > 0 {
		if resolved := collector.tc.resolved_fn_value_name(id) {
			enqueue(resolved, mut used, mut queue)
			return
		}
		if node.value in fn_decls && collector.node_is_fn_value(id) {
			enqueue(node.value, mut used, mut queue)
		}
		return
	}
	if node.kind == .selector && node.children_count > 0 && node.value.len > 0 {
		for name in collector.fn_value_selector_names(node, '', map[string]string{}) {
			if name in fn_decls {
				enqueue(name, mut used, mut queue)
			}
		}
	}
	for i in 0 .. node.children_count {
		enqueue_function_value_selectors_in_node(a, collector, fn_decls, a.child(node, i), mut used, mut queue)
	}
}

fn markused_ignored_fn_decl_nodes(a &flat.FlatAst) []bool {
	mut ignored := []bool{len: a.nodes.len}
	for node_idx, node in a.nodes {
		if node.kind == .fn_decl {
			markused_mark_node_subtree(a, flat.NodeId(node_idx), mut ignored)
		}
	}
	return ignored
}

fn markused_ignored_top_level_nodes(a &flat.FlatAst) []bool {
	if !markused_has_entry_main(a) {
		return []bool{}
	}
	mut ignored := []bool{len: a.nodes.len}
	for file_idx, file_node in a.nodes {
		if !markused_should_scan_top_level_file(a, file_idx, file_node) {
			continue
		}
		for i in 0 .. file_node.children_count {
			child_id := a.child(&file_node, i)
			if int(child_id) < a.user_code_start {
				continue
			}
			child := a.node(child_id)
			if !markused_is_top_level_stmt(child) {
				continue
			}
			markused_mark_node_subtree(a, child_id, mut ignored)
		}
	}
	return ignored
}

fn markused_shadowed_value_idents(a &flat.FlatAst) []bool {
	mut shadowed := []bool{len: a.nodes.len}
	for file_idx, file_node in a.nodes {
		if !markused_should_scan_top_level_file(a, file_idx, file_node) {
			continue
		}
		mut local_values := map[string]bool{}
		for i in 0 .. file_node.children_count {
			child_id := a.child(&file_node, i)
			if int(child_id) < a.user_code_start {
				continue
			}
			child := a.node(child_id)
			if !markused_is_top_level_stmt(child) {
				continue
			}
			if local_values.len > 0 {
				markused_mark_shadowed_idents(a, child, local_values, mut shadowed)
			}
			markused_mark_top_level_lhs_idents(a, child, mut shadowed)
			markused_add_top_level_lhs_names(a, child, mut local_values)
		}
	}
	return shadowed
}

fn markused_mark_shadowed_idents(a &flat.FlatAst, node &flat.Node, local_values map[string]bool, mut shadowed []bool) {
	if local_values.len == 0 {
		return
	}
	mut stack := []flat.NodeId{cap: int(node.children_count)}
	for i in 0 .. node.children_count {
		child_id := a.child(node, i)
		if int(child_id) >= 0 {
			stack << child_id
		}
	}
	for stack.len > 0 {
		id := stack.pop()
		idx := int(id)
		if idx < 0 || idx >= shadowed.len {
			continue
		}
		child := a.node(id)
		if child.kind == .ident && child.value in local_values {
			shadowed[idx] = true
		}
		for i in 0 .. child.children_count {
			next_id := a.child(child, i)
			if int(next_id) >= 0 {
				stack << next_id
			}
		}
	}
}

fn markused_mark_top_level_lhs_idents(a &flat.FlatAst, node &flat.Node, mut shadowed []bool) {
	if node.kind != .decl_assign {
		return
	}
	mut i := 0
	for i + 1 < node.children_count {
		lhs_id := a.child(node, i)
		idx := int(lhs_id)
		if idx >= 0 && idx < shadowed.len {
			lhs := a.node(lhs_id)
			if lhs.kind == .ident {
				shadowed[idx] = true
			}
		}
		i += 2
	}
}

fn markused_add_top_level_lhs_names(a &flat.FlatAst, node &flat.Node, mut local_values map[string]bool) {
	match node.kind {
		.decl_assign, .assign {
			mut i := 0
			for i + 1 < node.children_count {
				lhs_id := a.child(node, i)
				if int(lhs_id) >= 0 {
					lhs := a.node(lhs_id)
					if lhs.kind == .ident && lhs.value.len > 0 {
						local_values[lhs.value] = true
					}
				}
				i += 2
			}
		}
		.global_decl {
			for i in 0 .. node.children_count {
				field := a.child_node(node, i)
				if field.value.len > 0 {
					local_values[field.value] = true
				}
			}
		}
		else {}
	}
}

fn markused_mark_node_subtree(a &flat.FlatAst, root flat.NodeId, mut marked []bool) {
	mut stack := [root]
	for stack.len > 0 {
		id := stack.pop()
		idx := int(id)
		if idx < 0 || idx >= marked.len || marked[idx] {
			continue
		}
		marked[idx] = true
		node := a.node(id)
		for i in 0 .. node.children_count {
			child_id := a.child(node, i)
			if int(child_id) >= 0 {
				stack << child_id
			}
		}
	}
}

// type_string_needs_optional_helpers returns type string needs optional helpers data for markused.
fn type_string_needs_optional_helpers(type_id u32) bool {
	shape := flat.source_type_shape(type_id)
	return shape.has(.optional)
}

// type_needs_zero_map returns type needs zero map data for markused.
fn type_needs_zero_map(typ types.Type) bool {
	mut clean := typ
	for _ in 0 .. 8 {
		if clean is types.Alias {
			clean = clean.base_type
			continue
		}
		if clean is types.OptionType {
			clean = clean.base_type
			continue
		}
		if clean is types.ResultType {
			clean = clean.base_type
			continue
		}
		break
	}
	return clean is types.Map
}

// markused_membership_container_type
// supports helper handling in markused.
fn markused_membership_container_type(tc &types.TypeChecker, typ types.Type) u32 {
	mut clean_id := markused_trimmed_source_type(typ.source_type_id())
	for {
		shape := flat.source_type_shape(clean_id)
		if shape.has(.shared_type) {
			clean_id = markused_trimmed_source_type(flat.source_type_syntax(clean_id).raw_base())
			continue
		}
		if shape.has(.pointer) {
			clean_id = markused_trimmed_source_type(flat.source_type_syntax(clean_id).raw_base())
			continue
		}
		if alias_id := tc.type_aliases.type_id(clean_id) {
			alias := markused_trimmed_source_type(alias_id)
			if alias == clean_id {
				break
			}
			clean_id = alias
			continue
		}
		break
	}
	return clean_id
}

// qualify_fn supports qualify fn handling for markused.
fn qualify_fn(mod string, name string) string {
	if mod.len == 0 || mod == 'main' || mod == 'builtin' {
		return name
	}
	return '${mod}.${name}'
}

// markused_qualify_source_type preserves IDs when qualify_fn leaves a source spelling unchanged.
fn markused_qualify_source_type(mod string, name_id u32) u32 {
	if mod.len == 0 || mod == 'main' || mod == 'builtin' {
		return name_id
	}
	return flat.source_type_composition_two(.qualified, flat.source_type_id(mod), name_id)
}

// receiver_info supports receiver info handling for markused.
fn receiver_info(a &flat.FlatAst, node &flat.Node) (string, u32) {
	mut receiver_struct := u32(0)
	if node.value.contains('.') {
		receiver_struct = flat.source_type_syntax(node.source_value_id()).qualifier()
	}
	for pi in 0 .. node.children_count {
		pc := a.child_node(node, pi)
		if pc.kind == .param {
			if receiver_struct != 0 {
				return pc.value, receiver_struct
			}
			clean_type := flat.source_type_syntax(pc.typ).pointer_raw_base()
			if pc.value.len > 0 && flat.source_type_syntax(clean_type).first_capital() {
				return pc.value, if flat.source_type_shape(pc.typ).has(.pointer) {
					clean_type
				} else {
					pc.typ
				}
			}
		}
	}
	if receiver_struct == 0 {
		return '', u32(0)
	}
	return '', receiver_struct
}

fn (c &CallCollector) collect_interface_boxed_methods(call &flat.Node, resolved_call string, cur_module string, imports map[string]string, local_values map[string]bool, mut calls []string) {
	if call.children_count < 2 {
		return
	}
	// Cheap pre-check on the raw callee spellings before building the full
	// candidate list: this pass only matters for the few signatures that take
	// an interface-typed parameter.
	if !c.call_may_target_interface_params(call, resolved_call) {
		return
	}
	signatures := c.call_signature_candidates(call, resolved_call, cur_module, imports, local_values)
	if signatures.len == 0 {
		return
	}
	callee := c.a.child_node(call, 0)
	param_offset := c.call_receiver_param_offset(callee, imports, local_values)
	for signature in signatures {
		params := c.tc.fn_param_types[signature] or { continue }
		for arg_i := 1; arg_i < call.children_count; arg_i++ {
			param_i := arg_i - 1 + param_offset
			if param_i < 0 || param_i >= params.len {
				continue
			}
			expected := types.unwrap_pointer(params[param_i])
			if expected !is types.Interface {
				continue
			}
			arg_id := markused_generic_call_arg_value(c.a, c.a.child(call, arg_i))
			actual := types.unwrap_pointer(c.tc.resolve_type(arg_id))
			actual_name_source_id := resolve_type_name(actual)

			c.add_interface_boxed_methods(expected.source_type_id(), actual_name_source_id, mut calls)
		}
	}
}

// call_may_target_interface_params mirrors call_signature_candidates' raw name
// extraction (resolved call name, ident callee, selector method name) against
// the precomputed interface-param gate.
fn (c &CallCollector) call_may_target_interface_params(call &flat.Node, resolved_call string) bool {
	if c.may_target_interface_params(resolved_call) {
		return true
	}
	if call.children_count == 0 {
		return false
	}
	callee := c.a.child_node(call, 0)
	if callee.kind in [.ident, .selector] {
		return c.may_target_interface_params(callee.value_text())
	}
	return false
}

fn (c &CallCollector) call_signature_candidates(call &flat.Node, resolved_call string, cur_module string, imports map[string]string, local_values map[string]bool) []string {
	mut names := []string{}
	markused_add_call_signature_candidate(resolved_call, cur_module, mut names)
	if call.children_count == 0 {
		return names
	}
	callee := c.a.child_node(call, 0)
	if callee.kind == .ident && callee.value_text().len > 0 && callee.value_text() !in local_values {
		markused_add_call_signature_candidate(callee.value_text(), cur_module, mut names)
		return names
	}
	if callee.kind == .selector && callee.value_text().len > 0 && callee.children_count > 0 {
		base := c.a.child_node(callee, 0)
		if base.kind == .ident && base.value_text().len > 0 {
			if imported := imports[base.value_text()] {
				markused_add_call_signature_candidate('${imported}.${callee.value_text()}', cur_module, mut names)
			}
		}
	}
	return names
}

fn markused_add_call_signature_candidate(name string, cur_module string, mut names []string) {
	if name.len == 0 {
		return
	}
	markused_push_unique_key(name, mut names)
	lowered := markused_c_name(name)
	if lowered != name {
		markused_push_unique_key(lowered, mut names)
	}
	qname := qualify_fn(cur_module, name)
	if qname != name {
		markused_push_unique_key(qname, mut names)
		lowered_qname := markused_c_name(qname)
		if lowered_qname != qname {
			markused_push_unique_key(lowered_qname, mut names)
		}
	}
}

fn (c &CallCollector) call_receiver_param_offset(callee &flat.Node, imports map[string]string, local_values map[string]bool) int {
	if callee.kind != .selector || callee.children_count == 0 {
		return 0
	}
	base := c.a.child_node(callee, 0)
	if base.kind != .ident || base.value.len == 0 {
		return 1
	}
	if base.value in imports && base.value !in local_values {
		return 0
	}
	if base.value in local_values {
		return 1
	}
	if base.value[0] >= `A` && base.value[0] <= `Z` {
		return 0
	}
	return 0
}

fn (c &CallCollector) add_interface_boxed_methods(iface_name_id u32, actual_name_id u32, mut calls []string) {
	iface_name := flat.source_type_name(iface_name_id)
	actual_name := flat.source_type_name(actual_name_id)
	if iface_name_id == 0 || actual_name_id == 0 {
		return
	}
	if !c.tc.named_type_implements_interface(actual_name_id, iface_name_id) {
		return
	}
	for method in c.tc.interface_abstract_method_names(iface_name_id) {
		calls << '${iface_name}.${method}'
		if info := c.tc.resolve_generic_struct_method(actual_name_id, method) {
			c.add_typed_receiver_method_name(info.name, mut calls)
		}
		if concrete_method := c.tc.concrete_method_signature_key(actual_name_id, method) {
			c.add_typed_receiver_method_name(concrete_method, mut calls)
		}
		c.add_typed_receiver_method_name('${actual_name}.${method}', mut calls)
	}
}

fn (c &CallCollector) collect_interface_boxed_return_methods(node &flat.Node, mut calls []string) {
	return_type := types.unwrap_pointer(c.tc.fn_ret_types[node.value] or { return })
	if return_type !is types.Interface {
		return
	}
	mut stack := []flat.NodeId{cap: int(node.children_count)}
	for i in 0 .. node.children_count {
		child_id := c.a.child(node, i)
		if int(child_id) >= 0 {
			stack << child_id
		}
	}
	for stack.len > 0 {
		id := stack.pop()
		child := c.a.node(id)
		if child.kind == .fn_decl {
			continue
		}
		if child.kind == .return_stmt {
			for i in 0 .. child.children_count {
				value_id := c.a.child(child, i)
				actual := types.unwrap_pointer(c.tc.resolve_type(value_id))
				c.add_interface_boxed_methods(return_type.source_type_id(), resolve_type_name(actual), mut calls)
			}
			continue
		}
		for i in 0 .. child.children_count {
			child_id := c.a.child(child, i)
			if int(child_id) >= 0 {
				stack << child_id
			}
		}
	}
}

struct ModuleCalls {
mut:
	names map[string]bool
}

// The declaration scan already indexes the raw, qualified and C spellings
// accepted by library-body convergence. Probe reached names instead of
// regenerating those spellings for every still-unused library declaration.
fn unchecked_used_library_nodes(a &flat.FlatAst, tc &types.TypeChecker, declarations map[string][]FnDeclInfo, used map[string]bool) []int {
	mut seen := []bool{len: a.nodes.len}
	mut result := []int{}
	for name, _ in used {
		infos := declarations[name] or { continue }
		for info in infos {
			id := int(info.node_id)
			if id < 0 || id >= seen.len || seen[id] { continue }
			seen[id] = true
			node := a.node(info.node_id)
			file := if source := a.source_files[node.pos.id] { source.name } else { '' }
			module_name := tc.file_modules[file] or { info.module }
			if tc.skips_library_body_in_file(node, file, module_name) {
				result << id
			}
		}
	}
	return result
}

// BodyCalls holds the precollected call and initializer-ref names of one
// fn_decl body (see precollect_body_calls).
struct BodyCalls {
mut:
	calls         []string
	refs          []string
	uses_generics bool
}

// collect_body runs the full per-body analysis (local values, calls,
// initializer refs) for one fn_decl node. It only reads shared state — the
// checker memoizes exclusively into its private type_cache — so it can run on
// worker threads against a forked TypeChecker.
fn (c &CallCollector) collect_body(node &flat.Node, cur_module string, imports map[string]string) BodyCalls {
	return c.collect_body_with_generic_detection(node, cur_module, imports, c.detect_generics)
}

// Once the pass knows it needs monomorphization, bodies only collect their
// dependency edges. Generic alias/operator helper discovery remains enabled
// on the collector independently of this reporting-only flag.
fn (c &CallCollector) collect_body_with_generic_detection(node &flat.Node, cur_module string, imports map[string]string, detect_generics bool) BodyCalls {
	receiver_name, receiver_struct := receiver_info(c.a, node)
	local_values, has_inference_binders := c.local_value_names(node)
	visibility_names := c.local_value_visibility_names(local_values, cur_module, imports)
	needs_visibility := visibility_names.len > 0
	mut local_types := c.new_local_types()
	mut local_ident_types := c.new_ident_types()
	mut checked_local_names := map[string]bool{}
	mut checked_sum_types := map[string]types.TypeId{}
	mut inferred_visible_idents := map[int]bool{}
	if checked := c.checked_local_bindings(node, cur_module, imports, local_values) {
		local_types = checked.type_names
		checked_local_names = checked.ident_names
		checked_sum_types = checked.sum_types
		if needs_visibility {
			inferred_visible_idents = markused_visible_local_idents(c.a, node, visibility_names)
		}
	} else {
		_, local_types, local_ident_types, inferred_visible_idents = c.local_value_info_for_names(node, cur_module, imports, local_values, visibility_names, has_inference_binders)
	}
	if receiver_name.len > 0 && receiver_struct != 0 {
		local_types.set_id(receiver_name, receiver_struct)
	}
	visible_local_idents := if needs_visibility {
		inferred_visible_idents
	} else {
		map[int]bool{}
	}
	mut result := BodyCalls{
		calls:         []string{cap: 32}
		refs:          []string{cap: 8}
		uses_generics: detect_generics && (c.generic_fn_name_is_known(node.value, cur_module)
			|| c.type_text_uses_generics(node.source_value_id(), cur_module, imports)
			|| c.node_uses_generics(node, cur_module, imports))
	}
	scoped := CallCollector{
		...c
		local_ident_visibility:     visible_local_idents
		local_ident_types:          local_ident_types
		checked_local_names:        checked_local_names
		checked_sum_types:          checked_sum_types
		has_local_ident_visibility: true
	}
	result.uses_generics = scoped.collect_calls_with_locals_and_generics(node, cur_module, imports, receiver_name, receiver_struct, local_values, local_types, visible_local_idents, c.body_checker_edges_authoritative, detect_generics, result.uses_generics, mut result.calls, mut result.refs, true)
	c.collect_interface_boxed_return_methods(node, mut result.calls)
	return result
}

// collect_bodies_range fills results[start..end] for the given body list; the
// per-worker unit of the parallel precollection (also the serial fallback).
@[direct_array_access]
fn (c &CallCollector) collect_bodies_range(body_ids []int, body_modules []string, body_import_contexts []int, start int, end int, mut results []BodyCalls) {
	for i in start .. end {
		node := c.a.node(flat.NodeId(body_ids[i]))
		results[i] = c.collect_body(node, body_modules[i], c.imports(body_import_contexts[i]))
	}
}

// fork_with_tc returns a copy of this collector bound to a worker's forked
// TypeChecker. The lookup maps are shared read-only.
fn (c &CallCollector) fork_with_tc(wtc &types.TypeChecker) CallCollector {
	return CallCollector{
		a:                     c.a
		tc:                    wtc
		receiver_method_cache: &MarkusedMethodCache{}
		local_walk_scratch:    &MarkusedLocalWalkScratch{}
		decl_name_cache:       &DeclNameCache{}

		source_type_cache:                &SourceTypeCache{}
		default_calls:                    &StructDefaultCalls{}
		fn_decls:                         c.fn_decls
		fn_suffixes:                      c.fn_suffixes
		struct_decls:                     c.struct_decls
		const_decls:                      c.const_decls
		const_suffixes:                   c.const_suffixes
		const_suffixes_complete:          c.const_suffixes_complete
		import_contexts:                  c.import_contexts
		selective_alias_targets:          c.selective_alias_targets
		iface_param_gate:                 c.iface_param_gate
		generic_type_bases:               c.generic_type_bases
		generic_calls:                    c.generic_calls
		auto_str_skipped_fields:          c.auto_str_skipped_fields
		detect_generics:                  c.detect_generics
		body_checker_edges_authoritative: c.body_checker_edges_authoritative
	}
}

// collect_calls updates collect calls state for markused.
fn (c &CallCollector) collect_calls(node &flat.Node, cur_module string, imports map[string]string, receiver_name string, receiver_struct u32, mut calls []string) {
	local_values, local_types, local_ident_types, inferred_visible_idents := c.local_value_info_with_visibility(node, cur_module, imports)
	visible_local_idents := if c.local_values_need_visibility(local_values, cur_module, imports) {
		inferred_visible_idents
	} else {
		map[int]bool{}
	}
	scoped := CallCollector{
		...c
		local_ident_visibility:     visible_local_idents
		local_ident_types:          local_ident_types
		has_local_ident_visibility: true
	}
	scoped.collect_calls_with_locals(node, cur_module, imports, receiver_name, receiver_struct, local_values, local_types, visible_local_idents, mut calls)
}

fn (c &CallCollector) collect_calls_with_generic_usage(node &flat.Node, cur_module string, imports map[string]string, receiver_name string, receiver_struct u32, mut calls []string) bool {
	local_values, local_types, local_ident_types, inferred_visible_idents := c.local_value_info_with_visibility(node, cur_module, imports)
	visible_local_idents := if c.local_values_need_visibility(local_values, cur_module, imports) {
		inferred_visible_idents
	} else {
		map[int]bool{}
	}
	uses_generics := c.node_uses_generics(node, cur_module, imports)
	mut no_refs := []string{}
	scoped := CallCollector{
		...c
		local_ident_visibility:     visible_local_idents
		local_ident_types:          local_ident_types
		has_local_ident_visibility: true
	}
	return scoped.collect_calls_with_locals_and_generics(node, cur_module, imports, receiver_name, receiver_struct, local_values, local_types, visible_local_idents, false, true, uses_generics, mut calls, mut no_refs, false)
}

// collect_calls_with_locals is collect_calls with the per-body local-value
// analysis precomputed by the caller, so the BFS can share one analysis between
// collect_calls and collect_initializer_refs instead of walking each function
// subtree twice more.
fn (c &CallCollector) collect_calls_with_locals(node &flat.Node, cur_module string, imports map[string]string, receiver_name string, receiver_struct u32, local_values map[string]bool, local_types flat.TypeTextBindings, visible_local_idents map[int]bool, mut calls []string) {
	uses_generics := false
	mut no_refs := []string{}
	_ = c.collect_calls_with_locals_and_generics(node, cur_module, imports, receiver_name, receiver_struct, local_values, local_types, visible_local_idents, false, false, uses_generics, mut calls, mut no_refs, false)
}

@[direct_array_access]
fn (c &CallCollector) collect_calls_with_locals_and_generics(node &flat.Node, cur_module string, imports map[string]string, receiver_name string, receiver_struct u32, local_values map[string]bool, local_types flat.TypeTextBindings, visible_local_idents map[int]bool, source_edges_authoritative bool, detect_generics bool, initial_uses_generics bool, mut calls []string, mut initializer_refs []string, collect_initializer_refs bool) bool {
	mut uses_generics := initial_uses_generics
	if cur_module == 'ast' && node.value == 'TypeSymbol.find_method_with_generic_parent' {
		c.add_typed_receiver_method_name('ast.Table.find_structured_receiver_method', mut calls)
	}
	mut stack := MarkusedNodeStack{ nodes: []flat.NodeId{cap: int(node.children_count)} }
	for i in 0 .. node.children_count {
		child_id := c.a.child(node, i)
		if int(child_id) >= 0 {
			stack.push(child_id)
		}
	}
	mut no_ctor_literals := []flat.NodeId{}
	if literal_id := c.array_literal_without_ctor(node, cur_module) {
		no_ctor_literals << literal_id
	}
	for stack.size > 0 {
		raw_child_id := stack.pop()
		collect_initializer_ref := collect_initializer_refs && int(raw_child_id) >= 0
		child_id := if int(raw_child_id) >= 0 {
			raw_child_id
		} else {
			flat.NodeId(-int(raw_child_id) - 1)
		}
		child := &c.a.nodes[int(child_id)]
		if child.kind in [.postfix, .in_expr] {
			if literal_id := c.array_literal_without_ctor(child, cur_module) {
				no_ctor_literals << literal_id
			}
		}
		if detect_generics && !uses_generics && c.node_uses_generics(child, cur_module, imports) {
			uses_generics = true
		}
		if detect_generics && !uses_generics && child.kind == .struct_init
			&& c.generic_type_base_is_known(child.source_value_id(), cur_module, imports)
			&& c.parsed_type_uses_concrete_generics(c.tc.resolve_type(child_id), 0) {
			uses_generics = true
		}
		match child.kind {
			.ident {
				name_is_local := markused_ident_is_visible_local(child_id, child.value_text(), local_values, visible_local_idents)
				if collect_initializer_ref && !name_is_local {
					c.add_initializer_ref_candidates(child.value_text(), cur_module, imports, mut initializer_refs)
				}
				if !source_edges_authoritative {
					c.collect_fn_value_ident(child_id, child.value_text(), cur_module, imports, name_is_local, mut calls)
				}
			}
			.selector {
				if collect_initializer_ref && !c.selector_base_is_local(child, local_values)
					&& child.children_count > 0 {
					base := c.a.child_node(child, 0)
					if base.kind == .ident && base.value_text().len > 0 && child.value_text().len > 0 {
						c.add_initializer_ref_candidates('${base.value_text()}.${child.value_text()}', cur_module, imports, mut initializer_refs)
					}
				}
				if !source_edges_authoritative {
					c.collect_fn_value_selector(child_id, child, cur_module, imports, local_values, mut calls)
				}
			}
			.call {
				mut resolved_call := ''
				if resolved := c.tc.resolved_call_name(child_id) {
					resolved_call = resolved
				}
				if detect_generics && !uses_generics
					&& c.generic_fn_name_is_known(resolved_call, cur_module) {
					uses_generics = true
				}
				c.collect_interface_boxed_methods(child, resolved_call, cur_module, imports, local_values, mut calls)
				c.collect_lowered_join_path_single(child, resolved_call, imports, local_values, mut calls)
				if !source_edges_authoritative && child.children_count > 0 {
					callee_id := c.a.child(child, 0)
					if int(callee_id) >= 0 {
						mut callee := &c.a.nodes[int(callee_id)]
						if detect_generics && !uses_generics && callee.kind == .index
							&& callee.children_count > 0 && callee.value_text() != 'range' {
							base_name := c.qualified_expr_name(c.a.child(callee, 0))
							if c.generic_type_base_is_known(flat.source_type_id(base_name), cur_module, imports) {
								uses_generics = true
							}
						}
						if callee.kind == .paren && callee.children_count > 0 {
							inner_id := c.a.child(callee, 0)
							if int(inner_id) >= 0 {
								inner := &c.a.nodes[int(inner_id)]
								if inner.kind == .ident || inner.kind == .selector {
									callee = inner
								}
							}
						}
						// A generic call `f[T](...)` has an `.index` callee whose base is the
						// (possibly module-qualified) function name. The checker usually records
						// a `resolved_call` for it, but a generic call inside a still-generic
						// template body can lose that annotation (notably under parallel check),
						// leaving only the `.index` node. Unwrap to the base function so markused
						// still follows the call — but only when the base actually names a known
						// function, so a real value index `arr[i](...)` is left alone.
						if callee.kind == .index && callee.children_count > 0 {
							base_id := c.a.child(callee, 0)
							if int(base_id) >= 0 {
								base := &c.a.nodes[int(base_id)]
								if base.kind == .selector {
									callee = base
								} else if base.kind == .ident && base.value_text().len > 0
									&& base.value_text() !in local_values && (c.is_known_fn_name(base.value_text())
									|| c.is_known_fn_name(qualify_fn(cur_module, base.value_text()))) {
									callee = base
								}
							}
						}
						if callee.kind == .ident && callee.value_text().len > 0 {
							if resolved_call.len > 0 {
								calls << resolved_call
							}
							if callee.value_text() in ['print', 'println', 'eprint', 'eprintln']
								&& child.children_count >= 2 {
								arg_id := c.a.child(child, 1)
								if c.expr_lowers_to_map_str(arg_id, local_values, local_types) {
									calls << 'string__plus'
								}
							}
							if callee.value_text() !in local_values {
								qcallee := qualify_fn(cur_module, callee.value_text())
								if qcallee != callee.value_text() && c.is_known_fn_name(qcallee) {
									calls << qcallee
								} else {
									calls << callee.value_text()
									if qcallee != callee.value_text() {
										calls << qcallee
									}
								}
							}
						} else if callee.kind == .selector && callee.value_text().len > 0 {
							mut has_exact_selector_call := false
							if callee.children_count > 0 {
								base_id := c.a.child(callee, 0)
								if int(base_id) >= 0 {
									if detect_generics && !uses_generics
										&& c.node_tree_uses_generics(base_id, cur_module, imports) {
										uses_generics = true
									}
									base := &c.a.nodes[int(base_id)]
									base_is_local_value := base.kind == .ident
										&& base.value_text() in local_values
									if callee.value_text() == 'str'
										&& c.expr_lowers_to_map_str(base_id, local_values, local_types) {
										calls << 'string__plus'
									}
									if base_is_local_value {
										has_exact_selector_call = c.collect_typed_receiver_method(base_id, callee.value_text(), cur_module, imports, local_values, local_types, mut calls)
										if declared_type_source_id := local_types.type_id(base.value_text()) {
											c.collect_sum_variant_receiver_methods_for_type_name(declared_type_source_id, callee.value_text(), cur_module, mut calls)
										}
									}
									if !has_exact_selector_call && !base_is_local_value {
										has_exact_selector_call = c.collect_checker_selected_call(resolved_call, mut calls)
									}
									if !has_exact_selector_call {
										has_exact_selector_call = c.collect_sum_variant_receiver_methods(base_id, callee.value_text(), cur_module, mut calls)
									}
									if !has_exact_selector_call && !base_is_local_value {
										has_exact_selector_call = c.collect_typed_receiver_method(base_id, callee.value_text(), cur_module, imports, local_values, local_types, mut calls)
									}
									if !has_exact_selector_call && base.kind == .ident
										&& base.value_text() in imports && base.value_text() !in local_values {
										calls << imports[base.value_text()] + '.' + callee.value_text()
										has_exact_selector_call = true
									}
									if !has_exact_selector_call && resolved_call.len > 0 {
										calls << resolved_call
										has_exact_selector_call = true
									}
									if !has_exact_selector_call && base.kind == .ident
										&& base.value_text() in local_values && !(receiver_name.len > 0
										&& base.value_text() == receiver_name) {
										has_exact_selector_call = true
									}
									if !has_exact_selector_call {
										if base.kind == .ident && base.value_text().len > 0 {
											if receiver_name.len > 0 && base.value_text() == receiver_name {
												calls << flat.source_type_name(receiver_struct) + '.' + callee.value_text()
												qrecv := qualify_fn(cur_module, flat.source_type_name(receiver_struct) + '.' + callee.value_text())
												if qrecv != flat.source_type_name(receiver_struct) + '.' + callee.value_text() {
													calls << qrecv
												}
											}
											mod_name := if base.value_text() in imports {
												imports[base.value_text()]
											} else {
												base.value_text()
											}
											calls << mod_name + '.' + callee.value_text()
											calls << qualify_fn(cur_module, base.value_text() + '.' + callee.value_text())
											if base.value_text().len > 0 && base.value_text()[0] >= `A`
												&& base.value_text()[0] <= `Z` {
												named_type := c.tc.parse_type(base.source_value_id())
												named_type_name_source_id := resolve_type_name(named_type)
												named_type_name := flat.source_type_name(named_type_name_source_id)
												if named_type_name_source_id != 0 {
													calls << named_type_name + '.' + callee.value_text()
												}
											}
										} else if base.kind == .selector && base.children_count > 0 {
											inner_id := c.a.child(base, 0)
											if int(inner_id) >= 0 {
												inner := &c.a.nodes[int(inner_id)]
												if inner.kind == .ident && inner.value_text().len > 0 {
													mod_name := if inner.value_text() in imports {
														imports[inner.value_text()]
													} else {
														inner.value_text()
													}
													calls << mod_name + '.' + base.value_text() + '.' + callee.value_text()
												}
											}
										}
									}
								}
							}
							if !has_exact_selector_call {
								calls << callee.value_text()
							}
						} else if resolved_call.len > 0 {
							calls << resolved_call
						}
					}
				} else if !source_edges_authoritative && resolved_call.len > 0 {
					calls << resolved_call
				}
				if c.detect_generics {
					c.collect_generic_alias_operator_usage(child, resolved_call, cur_module, imports, local_values, local_types, mut calls)
				}
				c.collect_omitted_params_default_calls(child, resolved_call, cur_module, imports, mut calls)
			}
			.prefix {
				if child.op == .amp && child.children_count > 0 {
					inner_id := c.a.child(child, 0)
					if int(inner_id) >= 0 {
						inner := &c.a.nodes[int(inner_id)]
						if inner.kind == .struct_init {
							calls << 'memdup'
						}
					}
				}
			}
			.string_interp {
				calls << 'string_plus_many'
			}
			.for_in_stmt {
				if child.value.int() == 3 && child.children_count > 2 {
					container_id := c.a.child(child, 2)
					type_name_source_id_2 := c.top_level_expr_type_name(container_id, cur_module, imports,
						local_values, local_types, true)

					mut container_type := types.unwrap_pointer(c.tc.parse_canonical_type(type_name_source_id_2))
					for container_type is types.Alias {
						container_type = types.unwrap_pointer(container_type.base_type)
					}
					if container_type is types.Array {
						// Cgen introduces array_get for iteration after markused. Literal
						// output programs can reach these loops through the builtin runtime.
						calls << 'array.get'
						calls << 'array__get'
					}
				}
			}
			.index {
				if child.value != 'range' && child.children_count >= 2 {
					base_id := c.a.child(child, 0)
					base := c.a.node(base_id)
					if generic_fn := c.generic_fn_value_name(child, base, cur_module, imports, local_values) {
						calls << generic_fn
						uses_generics = true
					} else if base.kind == .selector {
						// A generic method named on a value with its type arguments,
						// `h.first[int]`: the method the check resolved it to (see
						// types.TypeChecker.check_generic_method_value).
						if name := c.tc.resolved_call_name(child_id) {
							if (c.tc.fn_generic_params[name] or { []u32{} }).len > 0 {
								calls << name
								uses_generics = true
							}
						}
					}
				}
				c.collect_index_operator_method(child_id, '[]', cur_module, imports, local_values, local_types, mut calls)
				c.collect_index_overload_getter_method(child, cur_module, local_types, mut calls)
			}
			.assign, .selector_assign, .index_assign {
				if resolved := c.tc.resolved_call_name(child_id) {
					c.add_operator_call_name(resolved, mut calls)
				}
				if child.kind == .index_assign && child.children_count > 0 {
					lhs_id := c.a.child(child, 0)
					c.collect_index_operator_method(lhs_id, '[]=', cur_module, imports, local_values, local_types, mut calls)
					if child.op != .assign {
						c.collect_index_operator_method(lhs_id, '[]', cur_module, imports, local_values, local_types, mut calls)
						c.collect_index_compound_update_helpers(child, cur_module, local_types, mut calls)
					}
				}
				c.collect_assign_operator_call(child, cur_module, local_types, mut calls)
			}
			.infix {
				if resolved := c.tc.resolved_call_name(child_id) {
					c.add_operator_call_name(resolved, mut calls)
				}
				if child.op == .plus {
					calls << 'string__plus'
				}
				if child.children_count >= 2 {
					lhs_id := c.a.child(child, 0)
					rhs_id := c.a.child(child, 1)
					if child.op in [.dot, .amp] {
						lhs_name := c.qualified_expr_name(lhs_id)
						if lhs_name.len > 0 {
							rhs := &c.a.nodes[int(rhs_id)]
							if rhs.kind == .call && rhs.children_count > 0 {
								fn_node := c.a.child_node(rhs, 0)
								if fn_node.kind == .ident && fn_node.value.len > 0 {
									mod_name := if lhs_name in imports {
										imports[lhs_name]
									} else {
										lhs_name
									}
									calls << mod_name + '.' + fn_node.value
									calls << qualify_fn(cur_module, lhs_name + '.' + fn_node.value)
								}
							}
						}
					}
					if int(lhs_id) >= 0 {
						c.collect_struct_operator_call(lhs_id, child.op, cur_module, local_types, mut calls)
					}
				}
			}
			.or_expr {
				if child.children_count > 0 {
					expr_id := c.a.child(child, 0)
					c.collect_zero_struct_default_calls(c.node_type(expr_id), cur_module, imports, mut calls)
				}
			}
			.struct_init {
				c.collect_struct_default_calls(child, cur_module, imports, mut calls)
			}
			.array_init {
				c.collect_array_default_calls(child, cur_module, imports, mut calls)
			}
			.array_literal {
				// Cgen lowers a runtime array literal to one of these constructors after
				// markused, so the literal has no call edge of its own. Literal-output
				// programs skip the runtime seeds, yet still reach such literals in
				// builtin (the Linux backtrace runs `addr2line` with an argument array).
				if child.children_count > 0 && child_id !in no_ctor_literals {
					calls << 'new_array_from_c_array'
					if child.children_count == 1 {
						calls << 'new_array_from_c_array_noscan'
					}
				}
			}
			else {}
		}

		if child.children_count > 0 {
			mut j := int(child.children_count) - 1
			for j >= 0 {
				if child.kind == .call && j == 0 {
					if callee_expr_id := c.call_callee_expr_to_collect(child) {
						stack.push(if collect_initializer_refs {
							flat.NodeId(-int(callee_expr_id) - 1)
						} else {
							callee_expr_id
						})
					}
					j--
					continue
				}
				if child.kind == .decl_assign && j % 2 == 0 {
					j--
					continue
				}
				next_id := c.a.child(child, j)
				if int(next_id) >= 0 {
					stack.push(if collect_initializer_refs && !collect_initializer_ref {
						flat.NodeId(-int(next_id) - 1)
					} else {
						next_id
					})
				}
				j--
			}
		}
	}
	return uses_generics
}

fn (c &CallCollector) call_callee_expr_to_collect(node &flat.Node) ?flat.NodeId {
	if node.kind != .call || node.children_count == 0 {
		return none
	}
	callee_id := c.a.child(node, 0)
	if int(callee_id) < 0 {
		return none
	}
	callee := c.a.node(callee_id)
	if callee.kind == .selector {
		if callee.children_count == 0 {
			return none
		}
		base_id := c.a.child(callee, 0)
		if int(base_id) >= 0 && c.expr_contains_call_or_index(base_id) {
			return base_id
		}
		return none
	}
	if callee.kind == .ident {
		return none
	}
	if c.expr_contains_call(callee_id) {
		return callee_id
	}
	return none
}

fn (c &CallCollector) expr_contains_call_or_index(id flat.NodeId) bool {
	if int(id) < 0 {
		return false
	}
	root := c.a.node(id)
	if root.kind in [.call, .index] {
		return true
	}
	if root.children_count == 0 {
		return false
	}
	mut stack := MarkusedNodeStack{ nodes: [id], size: 1 }
	for stack.size > 0 {
		cur_id := stack.pop()
		if int(cur_id) < 0 {
			continue
		}
		node := c.a.node(cur_id)
		if node.kind in [.call, .index] {
			return true
		}
		for i in 0 .. node.children_count {
			stack.push(c.a.child(node, i))
		}
	}
	return false
}

fn (c &CallCollector) expr_contains_call(id flat.NodeId) bool {
	if int(id) < 0 {
		return false
	}
	root := c.a.node(id)
	if root.kind == .call {
		return true
	}
	if root.children_count == 0 {
		return false
	}
	mut stack := MarkusedNodeStack{ nodes: [id], size: 1 }
	for stack.size > 0 {
		cur_id := stack.pop()
		node := c.a.node(cur_id)
		if node.kind == .call {
			return true
		}
		for i in 0 .. node.children_count {
			child_id := c.a.child(node, i)
			if int(child_id) >= 0 {
				stack.push(child_id)
			}
		}
	}
	return false
}

fn (c &CallCollector) collect_initializer_refs(node &flat.Node, cur_module string, imports map[string]string, mut refs []string) {
	local_values, _ := markused_local_value_names(c.a, node)
	visible_local_idents := if c.local_values_need_visibility(local_values, cur_module, imports) {
		markused_visible_local_idents(c.a, node, local_values)
	} else {
		map[int]bool{}
	}
	c.collect_initializer_refs_with_locals(node, cur_module, imports, local_values, visible_local_idents, mut refs)
}

fn (c &CallCollector) collect_initializer_expr_refs(node &flat.Node, cur_module string, imports map[string]string, mut refs []string) {
	match node.kind {
		.ident {
			c.add_initializer_ref_candidates(node.value_text(), cur_module, imports, mut refs)
		}
		.selector {
			if node.children_count > 0 {
				base := c.a.child_node(node, 0)
				if base.kind == .ident && base.value_text().len > 0 && node.value_text().len > 0 {
					c.add_initializer_ref_candidates('${base.value_text()}.${node.value_text()}', cur_module, imports, mut refs)
				}
			}
		}
		else {}
	}
	c.collect_initializer_refs(node, cur_module, imports, mut refs)
}

// collect_initializer_refs_with_locals is collect_initializer_refs with the
// per-body local-value analysis precomputed by the caller (see
// collect_calls_with_locals).
fn (c &CallCollector) collect_initializer_refs_with_locals(node &flat.Node, cur_module string, imports map[string]string, local_values map[string]bool, visible_local_idents map[int]bool, mut refs []string) {
	mut stack := []flat.NodeId{cap: int(node.children_count)}
	for i in 0 .. node.children_count {
		child_id := c.a.child(node, i)
		if int(child_id) >= 0 {
			stack << child_id
		}
	}
	for stack.len > 0 {
		child_id := stack.pop()
		child := &c.a.nodes[int(child_id)]
		match child.kind {
			.ident {
				if !markused_ident_is_visible_local(child_id, child.value_text(), local_values, visible_local_idents) {
					c.add_initializer_ref_candidates(child.value_text(), cur_module, imports, mut refs)
				}
			}
			.selector {
				if !c.selector_base_is_local(child, local_values) && child.children_count > 0 {
					base := c.a.child_node(child, 0)
					if base.kind == .ident && base.value_text().len > 0 && child.value_text().len > 0 {
						c.add_initializer_ref_candidates('${base.value_text()}.${child.value_text()}', cur_module, imports, mut refs)
					}
				}
			}
			else {}
		}

		if child.children_count > 0 {
			mut j := int(child.children_count) - 1
			for j >= 0 {
				if child.kind == .call && j == 0 {
					j--
					continue
				}
				if child.kind == .decl_assign && j % 2 == 0 {
					j--
					continue
				}
				next_id := c.a.child(child, j)
				if int(next_id) >= 0 {
					stack << next_id
				}
				j--
			}
		}
	}
}

fn (c &CallCollector) add_initializer_ref_candidates(name string, cur_module string, imports map[string]string, mut refs []string) {
	if name.len == 0 || c.const_decls.len == 0 {
		return
	}
	// Bare candidates only gain a module prefix, preserving their suffix.
	// A complete negative index therefore avoids allocating candidate strings.
	if c.const_suffixes_complete && name.index_u8(`.`) < 0 && !c.const_suffixes[name] {
		return
	}
	for candidate in c.value_name_candidates(name, cur_module, imports) {
		if candidate in c.const_decls {
			markused_push_receiver_candidate(mut refs, candidate)
		}
	}
}

fn (c &CallCollector) local_value_info(node &flat.Node, cur_module string, imports map[string]string) (map[string]bool, flat.TypeTextBindings, flat.NodeTypeTextBindings) {
	names, type_names, ident_types, _ := c.local_value_info_with_visibility(node, cur_module,
		imports)
	return names, type_names, ident_types
}

struct CheckedLocalBindings {
mut:
	type_names  flat.TypeTextBindings
	ident_names map[string]bool
	sum_types   map[string]types.TypeId
}

// Checked bindings preserve the checker's lexical scopes without rebuilding
// them. Sum receivers retain their declared type through narrowing. Nominal
// aliases and unresolved generics retain the inference path, whose source
// spellings carry information beyond a checked runtime type.
@[direct_array_access]
fn (c &CallCollector) checked_local_bindings(root &flat.Node, cur_module string, imports map[string]string, names map[string]bool) ?CheckedLocalBindings {
	if root.kind != .fn_decl || c.generic_fn_name_is_known(root.value, cur_module) {
		return none
	}
	params := c.tc.fn_param_types[qualify_fn(cur_module, root.value)] or { return none }
	for typ in params {
		if !markused_checked_local_type(typ) {
			return none
		}
	}
	mut result := CheckedLocalBindings{
		type_names:  c.new_local_types()
		ident_names: names
	}
	mut ident_names_owned := false
	mut bindings := map[string]bool{}
	mut param_index := 0
	for i in 0 .. root.children_count {
		param := c.a.child_node(root, i)
		if param.kind != .param || param.value.len == 0 { continue }
		bindings[param.value] = true
		result.type_names.set_id(param.value, u32(0))
		if param_index < params.len && markused_direct_sum_type(params[param_index]) {
			result.sum_types[param.value] = c.tc.semantic_type_id(params[param_index]) or { return none }
			result.type_names.set_id(param.value, c.tc.type_name_by_id(result.sum_types[param.value]))
		}
		param_index++
	}
	if param_index != params.len { return none }
	mut first_local_read := flat.empty_node
	mut scratch := c.local_walk_scratch
	mut stack := MarkusedNodeStack{
		nodes: if !isnil(scratch) {
			scratch.nodes
		} else {
			[]flat.NodeId{cap: int(root.children_count)}
		}
	}
	defer {
		if !isnil(scratch) { scratch.nodes = stack.nodes }
	}
	mut loop_names := map[string]bool{}
	for i in 0 .. root.children_count {
		id := c.a.child(root, i)
		if int(id) >= 0 {
			stack.push(id)
		}
	}
	for stack.size > 0 {
		raw_id := stack.pop()
		outer := int(raw_id) >= 0
		id := if outer { raw_id } else { flat.NodeId(-int(raw_id) - 1) }
		node := c.a.node(id)
		if node.kind in [.fn_decl, .c_fn_decl] {
			continue
		}
		if node.kind in [.lambda_expr, .fn_literal] {
			return none
		}
		if node.kind == .for_in_stmt {
			for i in 0 .. 2 {
				binding_id := c.a.child(node, i)
				binding := c.a.node(binding_id)
				if int(binding_id) < 0 || binding.kind != .ident
					|| binding.value in ['', '_'] {
					continue
				}
				typ := c.tc.expr_type(binding_id) or { return none }
				if !markused_checked_local_type(typ) {
					return none
				}
				if markused_direct_sum_type(typ) {
					if binding.value in bindings { return none }
					result.sum_types[binding.value] = c.tc.semantic_type_id(typ) or { return none }
				} else if binding.value in result.sum_types {
					return none
				}
				bindings[binding.value] = true
				loop_names[binding.value] = true
				if !result.ident_names[binding.value] {
					if !ident_names_owned {
						result.ident_names = result.ident_names.clone()
						ident_names_owned = true
					}
					result.ident_names[binding.value] = true
				}
			}
		}
		if node.kind == .call {
			if name := c.tc.resolved_call_name(id) {
				if c.generic_fn_name_is_known(name, cur_module) {
					return none
				}
			}
		}
		if node.kind == .decl_assign {
			if node.typ != 0 {
				if _ := c.contextual_alias_type_name(node.typ, cur_module, imports) {
					return none
				}
			}
			lhs_count := markused_assign_lhs_count(node)
			rhs_count := int(node.children_count) - lhs_count
			for i in 0 .. lhs_count {
				lhs_index := if i < rhs_count { i * 2 } else { rhs_count + i }
				lhs_id := c.a.child(node, lhs_index)
				lhs := c.a.node(lhs_id)
				if lhs.kind != .ident || lhs.value in ['', '_'] {
					continue
				}
				typ := c.tc.expr_type(lhs_id) or { return none }
				if !markused_checked_local_type(typ) {
					return none
				}
				if markused_direct_sum_type(typ) {
					if lhs.value in bindings { return none }
					result.sum_types[lhs.value] = c.tc.semantic_type_id(typ) or { return none }
				} else if lhs.value in result.sum_types {
					return none
				}
				bindings[lhs.value] = true
				if outer {
					if result.type_names.contains(lhs.value) {
						return none
					}
					result.type_names.set_id(lhs.value, if declared := result.sum_types[lhs.value] {
						c.tc.type_name_by_id(declared)
					} else {
						u32(0)
					})
				}
			}
			for i in 0 .. rhs_count {
				rhs_index := if i < lhs_count { i * 2 + 1 } else { lhs_count + i }
				if _ := c.syntax_alias_expr_type(c.a.child(node, rhs_index), cur_module, imports) {
					return none
				}
			}
		}
		if node.kind == .ident && (node.value in names || node.value in loop_names) {
			if int(first_local_read) < 0 { first_local_read = id }
			typ := c.tc.expr_type(id) or { return none }
			if !markused_checked_local_type(typ) {
				return none
			}
		}
		for i in 0 .. node.children_count {
			child_id := c.a.child(node, i)
			if int(child_id) < 0 {
				continue
			}
			child := c.a.node(child_id)
			inner := child.kind in [.block, .if_expr, .match_stmt, .match_branch, .select_stmt,
				.select_branch, .for_stmt, .for_in_stmt, .fn_literal, .comptime_if, .comptime_for,
				.defer_stmt, .lock_expr] || node.kind == .decl_assign
			stack.push(if outer && !inner {
				child_id
			} else {
				flat.NodeId(-int(child_id) - 1)
			})
		}
	}
	// Manually constructed checker views without an interner retain the
	// spelling-based fallback. Checked bodies otherwise format only queried types.
	if int(first_local_read) >= 0 {
		if _ := c.tc.checked_expr_type_id(first_local_read) {
		} else {
			return none
		}
	}
	return result
}

// Intern only receiver types actually queried by call collection. Validation
// reads the recorded semantic types directly instead of interning every local
// identifier in a body that may subsequently require the inference fallback.
fn (c &CallCollector) checked_local_type_id(id flat.NodeId) ?types.TypeId {
	if c.checked_local_names.len == 0 || c.a.node(id).value !in c.checked_local_names {
		return none
	}
	if c.checked_sum_types.len > 0 {
		if declared := c.checked_sum_types[c.a.node(id).value] {
			return declared
		}
	}
	return c.tc.checked_expr_type_id(id)
}

fn (c &CallCollector) checked_local_type_name(id flat.NodeId) ?u32 {
	type_id := c.checked_local_type_id(id) or { return none }
	return c.tc.type_name_by_id(type_id)
}

fn markused_checked_local_type(typ types.Type) bool {
	match typ {
		types.Unknown, types.Alias, types.Interface { return false }
		types.Pointer, types.OptionType, types.ResultType {
			return markused_checked_local_type(typ.base_type)
		}
		types.Array, types.ArrayFixed, types.Channel {
			return markused_checked_local_type(typ.elem_type)
		}
		types.Map {
			return markused_checked_local_type(typ.key_type)
				&& markused_checked_local_type(typ.value_type)
		}
		types.FnType {
			if !markused_checked_local_type(typ.return_type) {
				return false
			}
			for param in typ.params {
				if !markused_checked_local_type(param.typ) {
					return false
				}
			}
		}
		types.MultiReturn {
			for part in typ.types {
				if !markused_checked_local_type(part) {
					return false
				}
			}
		}
		else {}
	}
	return true
}

fn markused_direct_sum_type(typ types.Type) bool {
	match typ {
		types.SumType { return true }
		types.Pointer, types.OptionType, types.ResultType {
			return markused_direct_sum_type(typ.base_type)
		}
		else { return false }
	}
}

@[direct_array_access]
fn (c &CallCollector) local_value_info_with_visibility(node &flat.Node, cur_module string, imports map[string]string) (map[string]bool, flat.TypeTextBindings, flat.NodeTypeTextBindings, map[int]bool) {
	names, has_inference_binders := c.local_value_names(node)
	visibility_names := c.local_value_visibility_names(names, cur_module, imports)
	return c.local_value_info_for_names(node, cur_module, imports, names, visibility_names, has_inference_binders)
}

@[direct_array_access]
fn (c &CallCollector) local_value_info_for_names(node &flat.Node, cur_module string, imports map[string]string, names map[string]bool, visibility_names map[string]bool, has_inference_binders bool) (map[string]bool, flat.TypeTextBindings, flat.NodeTypeTextBindings, map[int]bool) {
	mut type_names := c.new_local_types()
	mut ident_types := c.new_ident_types()
	// Scope visibility only distinguishes local names that can also name a
	// function or constant. The caller already identified those names.
	visible_local_idents := if visibility_names.len > 0 {
		markused_visible_local_idents(c.a, node, visibility_names)
	} else {
		map[int]bool{}
	}
	// Initializer expressions without declarations, parameters or loop/lambda
	// bindings cannot add any local types during the inference walk.
	if names.len == 0 && !has_inference_binders {
		return names, type_names, ident_types, visible_local_idents
	}
	c.set_local_fn_param_types(node, cur_module, imports, mut type_names)
	// Infer calls using the locals visible at each identifier, not declarations
	// collected later in the body or in a different block. The call walk reuses
	// this same visibility map when local names collide with declarations.
	scoped := CallCollector{
		...c
		local_ident_visibility:     visible_local_idents
		has_local_ident_visibility: true
	}
	scoped.infer_local_type_bindings(node, cur_module, imports, names, mut type_names,
		mut ident_types, true)
	return names, type_names, ident_types, visible_local_idents
}

fn (c &CallCollector) local_value_names(node &flat.Node) (map[string]bool, bool) {
	if isnil(c.local_walk_scratch) {
		return markused_local_value_names(c.a, node)
	}
	mut scratch := c.local_walk_scratch
	return markused_local_value_names_with_scratch(c.a, node, mut scratch)
}

@[direct_array_access]
fn markused_local_value_names(a &flat.FlatAst, node &flat.Node) (map[string]bool, bool) {
	mut scratch := MarkusedLocalWalkScratch{}
	return markused_local_value_names_with_scratch(a, node, mut scratch)
}

@[heap]
struct MarkusedLocalWalkScratch {
mut:
	nodes []flat.NodeId
}

@[direct_array_access]
fn markused_local_value_names_with_scratch(a &flat.FlatAst, node &flat.Node, mut buffer MarkusedLocalWalkScratch) (map[string]bool, bool) {
	mut names := map[string]bool{}
	mut has_inference_binders := false
	mut stack := MarkusedNodeStack{ nodes: buffer.nodes }
	defer { buffer.nodes = stack.nodes }
	for i in 0 .. node.children_count {
		child_id := a.child(node, i)
		if int(child_id) >= 0 {
			stack.push(child_id)
		}
	}
	for stack.size > 0 {
		id := stack.pop()
		child := a.node(id)
		if child.kind in [.for_in_stmt, .lambda_expr] {
			has_inference_binders = true
		}
		if child.kind == .param && child.value.len > 0 {
			names[child.value] = true
		} else if child.kind == .lambda_expr {
			for i in 0 .. child.children_count - 1 {
				param := a.child_node(child, i)
				if param.kind == .ident && param.value.len > 0 {
					names[param.value] = true
				}
			}
		} else if child.kind == .decl_assign {
			lhs_count := markused_assign_lhs_count(child)
			rhs_count := int(child.children_count) - lhs_count
			for i in 0 .. lhs_count {
				idx := if i < rhs_count { i * 2 } else { rhs_count + i }
				lhs := a.child_node(child, idx)
				if lhs.kind == .ident && lhs.value.len > 0 {
					names[lhs.value] = true
				}
			}
		}
		for i in 0 .. child.children_count {
			next_id := a.child(child, i)
			if int(next_id) >= 0 {
				stack.push(next_id)
			}
		}
	}
	return names, has_inference_binders
}

fn markused_assign_lhs_count(node &flat.Node) int {
	if node.value.is_int() {
		count := node.value.int()
		if count > 0 && count <= int(node.children_count) {
			return count
		}
	}
	if node.children_count <= 2 {
		return if node.children_count > 0 { 1 } else { 0 }
	}
	return int(node.children_count) - 1
}

struct LocalTypeChange {
	name    string
	typ     u32
	present bool
}

struct LocalNameChange {
	name    string
	present bool
}

struct LocalNameState {
mut:
	names   map[string]bool
	owned   bool
	changes []LocalNameChange
}

fn (mut state LocalNameState) restore(mark int) {
	for state.changes.len > mark {
		change := state.changes.pop()
		if change.present {
			state.names[change.name] = false
		} else {
			state.names.delete(change.name)
		}
	}
}

// markused_resolve_imported_source_type keeps a source spelling numeric across import resolution.
fn markused_resolve_imported_source_type(typ_id u32, imports map[string]string) u32 {
	if !flat.source_type_shape(typ_id).has(.dotted) { return typ_id }
	syntax := flat.source_type_syntax(typ_id)
	if module_name := imports[flat.source_type_name(syntax.qualifier())] {
		return flat.source_type_composition_two(.qualified, flat.source_type_id(module_name), syntax.unqualified())
	}
	return typ_id
}

fn markused_set_local_type(name string, typ_id u32, mut type_names flat.TypeTextBindings, mut changes []LocalTypeChange) {
	if previous := type_names.type_id(name) {
		if previous == typ_id { return }
		changes << LocalTypeChange{ name: name, typ: previous, present: true }
	} else {
		changes << LocalTypeChange{ name: name }
	}
	type_names.set_id(name, typ_id)
}

fn markused_restore_local_types(mark int, mut type_names flat.TypeTextBindings, mut changes []LocalTypeChange) {
	for changes.len > mark {
		change := changes.pop()
		if change.present {
			type_names.set_id(change.name, change.typ)
		} else {
			type_names.delete(change.name)
		}
	}
}

fn (c &CallCollector) infer_local_type_bindings(node &flat.Node, cur_module string, imports map[string]string, names map[string]bool, mut type_names flat.TypeTextBindings, mut ident_types flat.NodeTypeTextBindings, root bool) {
	mut changes := []LocalTypeChange{}
	mut name_state := LocalNameState{ names: names }
	c.infer_local_type_bindings_scoped(node, cur_module, imports, mut name_state, mut type_names,
		mut ident_types, root, mut changes)
}

@[direct_array_access]
fn (c &CallCollector) infer_local_type_bindings_scoped(node &flat.Node, cur_module string, imports map[string]string, mut name_state LocalNameState, mut type_names flat.TypeTextBindings, mut ident_types flat.NodeTypeTextBindings, root bool, mut changes []LocalTypeChange) {
	for i in 0 .. node.children_count {
		id := c.a.child(node, i)
		if int(id) < 0 {
			continue
		}
		child := c.a.node(id)
		if child.kind in [.fn_decl, .c_fn_decl] {
			continue
		}
		if child.kind == .for_in_stmt {
			mark := changes.len
			name_mark := name_state.changes.len
			// Share the outer name set until a loop actually adds a name. Once
			// copied, one private map serves every nested loop in this inference.
			if child.children_count >= 3 {
				for binding_index in 0 .. 2 {
					binding := c.a.child_node(child, binding_index)
					if binding.kind != .ident || binding.value in ['', '_'] || name_state.names[binding.value] {
						continue
					}
					if !name_state.owned {
						name_state.names = name_state.names.clone()
						name_state.owned = true
					}
					name_state.changes << LocalNameChange{
						name:    binding.value
						present: binding.value in name_state.names
					}
				}
			}
			// Registration changes only the key and value bindings. Journal them,
			// then restore every loop-local change when this scope finishes.
			for binding_index in 0 .. 2 {
				binding := c.a.child_node(child, binding_index)
				if binding.kind != .ident || binding.value in ['', '_'] { continue }
				if previous := type_names.type_id(binding.value) {
					changes << LocalTypeChange{ name: binding.value, typ: previous, present: true }
				} else {
					changes << LocalTypeChange{ name: binding.value }
				}
			}
			c.register_top_level_for_in_vars(child, cur_module, imports, mut name_state.names,
				mut type_names)
			c.infer_local_type_bindings_scoped(child, cur_module, imports, mut name_state,
				mut type_names, mut ident_types, false, mut changes)
			markused_restore_local_types(mark, mut type_names, mut changes)
			name_state.restore(name_mark)
			continue
		}
		if child.kind == .lambda_expr {
			mut nested_types := type_names.clone()
			c.seed_lambda_param_types(id, child, node, cur_module, imports, type_names,
				mut nested_types)
			c.infer_local_type_bindings(child, cur_module, imports, name_state.names, mut nested_types,
				mut ident_types, false)
			continue
		}
		if child.kind in [.block, .if_expr, .match_stmt, .match_branch, .select_stmt, .select_branch,
			.for_stmt, .fn_literal, .comptime_if, .comptime_for, .defer_stmt, .lock_expr] {
			if root && child.kind == .block {
				c.infer_local_type_bindings_scoped(child, cur_module, imports, mut name_state, mut type_names,
					mut ident_types, false, mut changes)
			} else {
				// Keep only changed bindings, rather than cloning all outer locals
				// for every branch and nested block.
				mark := changes.len
				c.infer_local_type_bindings_scoped(child, cur_module, imports, mut name_state, mut type_names,
					mut ident_types, false, mut changes)
				markused_restore_local_types(mark, mut type_names, mut changes)
			}
			continue
		}
		if child.kind == .param && child.value.len > 0 && child.typ != 0 {
			if !root || !type_names.contains(child.value) {
				markused_set_local_type(child.value, markused_resolve_imported_source_type(child.typ, imports), mut type_names, mut changes)
			}
			continue
		}
		if child.kind == .decl_assign {
			lhs_count := markused_assign_lhs_count(child)
			rhs_count := int(child.children_count) - lhs_count
			for j in 0 .. rhs_count {
				rhs_idx := if j < lhs_count { j * 2 + 1 } else { lhs_count + j }
				rhs_id := c.a.child(child, rhs_idx)
				if int(rhs_id) >= 0 {
					rhs := c.a.node(rhs_id)
					if rhs.children_count > 0 {
						mark := changes.len
						c.infer_local_type_bindings_scoped(rhs, cur_module, imports, mut name_state,
							mut type_names, mut ident_types, false, mut changes)
						markused_restore_local_types(mark, mut type_names, mut changes)
					}
					if rhs.kind == .ident {
						if typ_source_id := type_names.type_id(rhs.value) {
							ident_types.set_id(int(rhs_id), typ_source_id)
						}
					}
				}
			}
			// All RHS bindings have been restored. Multiple LHS bindings still
			// need a snapshot so an earlier write cannot affect later RHS types.
			pre_types := markused_decl_type_snapshot(type_names, lhs_count)
			mut parts := []u32{}
			if rhs_count == 1 && lhs_count > 1 {
				rhs_id := c.a.child(child, 1)
				tuple := c.top_level_decl_rhs_type_name(rhs_id, cur_module, imports, name_state.names, pre_types)
				tuple_syntax := flat.source_type_syntax(tuple)
				if tuple_syntax.tuple_like() { parts = tuple_syntax.tuple_components() }
			}
			for j in 0 .. lhs_count {
				lhs_idx := if j < rhs_count { j * 2 } else { rhs_count + j }
				lhs := c.a.child_node(child, lhs_idx)
				if lhs.kind != .ident || lhs.value.len == 0 || lhs.value == '_' {
					continue
				}
				if parts.len == lhs_count {
					markused_set_local_type(lhs.value, parts[j], mut type_names, mut changes)
					continue
				}
				if j >= rhs_count || (rhs_count == 1 && lhs_count > 1) {
					continue
				}
				rhs_id := c.a.child(child, j * 2 + 1)
				if int(rhs_id) < 0 {
					continue
				}
				type_name := if lhs_count == 1 && child.typ != 0 {
					c.local_decl_type_name(child.typ, rhs_id, cur_module, imports, pre_types)
				} else {
					c.top_level_decl_rhs_type_name(rhs_id, cur_module, imports, name_state.names, pre_types)
				}
				if type_name != 0 {
					markused_set_local_type(lhs.value, type_name, mut type_names, mut changes)
				}
			}
			continue
		}
		if child.kind == .ident {
			if typ_source_id := type_names.type_id(child.value) {
				ident_types.set_id(int(id), typ_source_id)
			}
		}
		if child.children_count > 0 {
			c.infer_local_type_bindings_scoped(child, cur_module, imports, mut name_state, mut type_names,
				mut ident_types, false, mut changes)
		}
	}
}

fn (c &CallCollector) seed_lambda_param_types(id flat.NodeId, lambda &flat.Node, parent &flat.Node, cur_module string, imports map[string]string, outer_types flat.TypeTextBindings, mut nested_types flat.TypeTextBindings) {
	mut param_types := []u32{}
	if fn_type := c.tc.expr_type(id) {
		if fn_type is types.FnType {
			for parameter in fn_type.params {
				param := parameter.typ
				param_types << if markused_type_has_unknown(param) {
					u32(0)
				} else {
					param.source_type_id()
				}
			}
		}
	}
	if parent.kind == .call && parent.children_count > 1 {
		callee := c.a.child_node(parent, 0)
		if callee.kind == .selector && callee.value in ['map', 'filter', 'any', 'all', 'count'] && callee.children_count > 0 {
			receiver_id := c.a.child(callee, 0)
			if elem := c.top_level_for_in_elem_type_name(receiver_id, cur_module, imports, map[string]bool{}, outer_types) {
				if param_types.len == 0 {
					param_types << elem
				} else if param_types[0] == 0 {
					param_types[0] = elem
				}
			}
		}
	}
	for i in 0 .. lambda.children_count - 1 {
		param := c.a.child_node(lambda, i)
		if param.kind != .ident || param.value.len == 0 { continue }
		mut typ := if i < param_types.len { param_types[i] } else { u32(0) }
		if typ == 0 && param.typ != 0 {
			typ = markused_resolve_imported_source_type(param.typ, imports)
		}
		if typ != 0 {
			nested_types.set_id(param.value, typ)
		} else {
			nested_types.delete(param.value)
		}
	}
}

const markused_max_inline_fn_params = 16

// set_local_fn_param_types binds each named, typed parameter of a fn declaration.
// A checked signature with one entry per named parameter supplies the types
// (the last of duplicate names wins); otherwise the source spelling is resolved
// through the file's imports.
@[direct_array_access]
fn (c &CallCollector) set_local_fn_param_types(node &flat.Node, cur_module string, imports map[string]string, mut type_names flat.TypeTextBindings) {
	mut named := [markused_max_inline_fn_params]flat.NodeId{}
	mut named_count := 0
	for i in 0 .. node.children_count {
		child_id := c.a.child(node, i)
		child := c.a.node(child_id)
		if child.kind == .param && child.value.len > 0 {
			if named_count < markused_max_inline_fn_params {
				named[named_count] = child_id
			}
			named_count++
		}
	}
	if named_count == 0 {
		return
	}
	if named_count > markused_max_inline_fn_params {
		c.set_many_local_fn_param_types(node, cur_module, imports, named_count, mut type_names)
		return
	}
	params := c.local_fn_signature_param_types(node.value, cur_module, named_count) or {
		for k in 0 .. named_count {
			param := c.a.node(named[k])
			if param.typ != 0 {
				type_names.set_id(param.value, markused_resolve_imported_source_type(param.typ,
					imports))
			}
		}
		return
	}
	// Intern every signature type in order before binding, as a full name table would.
	mut ids := [markused_max_inline_fn_params]u32{}
	for k in 0 .. named_count {
		ids[k] = params[k].source_type_id()
	}
	for k in 0 .. named_count {
		param := c.a.node(named[k])
		if param.typ == 0 {
			continue
		}
		mut last := k
		for j := named_count - 1; j > k; j-- {
			if c.a.node(named[j]).value == param.value {
				last = j
				break
			}
		}
		type_names.set_id(param.value, ids[last])
	}
}

// set_many_local_fn_param_types is set_local_fn_param_types for declarations
// with more parameters than fit its inline buffers.
@[direct_array_access]
fn (c &CallCollector) set_many_local_fn_param_types(node &flat.Node, cur_module string, imports map[string]string, named_count int, mut type_names flat.TypeTextBindings) {
	mut param_types := c.new_local_types()
	if params := c.local_fn_signature_param_types(node.value, cur_module, named_count) {
		mut k := 0
		for i in 0 .. node.children_count {
			child := c.a.child_node(node, i)
			if child.kind == .param && child.value.len > 0 {
				param_types.set_id(child.value, params[k].source_type_id())
				k++
			}
		}
	}
	for i in 0 .. node.children_count {
		param := c.a.child_node(node, i)
		if param.kind == .param && param.value.len > 0 && param.typ != 0 {
			type_names.set_id(param.value, param_types.type_id(param.value) or {
				markused_resolve_imported_source_type(param.typ, imports)
			})
		}
	}
}

// local_fn_signature_param_types returns the checked parameter types of fn
// `name` when they have `param_count` entries. It probes the spellings of
// markused_fn_signature_name_candidates in the same order, deriving a C name
// only after the spellings before it missed.
fn (c &CallCollector) local_fn_signature_param_types(name string, cur_module string, param_count int) ?[]types.Type {
	module_first := cur_module.len > 0 && cur_module != 'main' && cur_module != 'builtin'
	first := if module_first { '${cur_module}.${name}' } else { name }
	if params := c.fn_param_types_with_count(first, param_count) {
		return params
	}
	first_c := markused_c_name(first)
	if first_c != first {
		if params := c.fn_param_types_with_count(first_c, param_count) {
			return params
		}
	}
	if !module_first {
		// The qualified spelling equals `name` outside modules.
		return none
	}
	if params := c.fn_param_types_with_count(name, param_count) {
		return params
	}
	name_c := markused_c_name(name)
	if name_c != name && name_c != first && name_c != first_c {
		if params := c.fn_param_types_with_count(name_c, param_count) {
			return params
		}
	}
	return none
}

@[inline]
fn (c &CallCollector) fn_param_types_with_count(key string, param_count int) ?[]types.Type {
	if key.len == 0 {
		return none
	}
	params := c.tc.fn_param_types[key] or { return none }
	if params.len != param_count {
		return none
	}
	return params
}

fn (c &CallCollector) local_decl_type_name(declared_id u32, rhs_id flat.NodeId, cur_module string, imports map[string]string, local_types flat.TypeTextBindings) u32 {
	if contextual_alias_source_id := c.contextual_alias_type_name(declared_id, cur_module, imports) {
		return contextual_alias_source_id
	}
	declared_type := c.tc.parse_canonical_type(declared_id)
	if declared_type is types.Alias {
		return declared_type.source_type_id()
	}
	rhs_name_source_id := c.top_level_decl_rhs_type_name(rhs_id, cur_module, imports, map[string]bool{}, local_types)

	if rhs_name_source_id == 0 {
		return declared_id
	}
	rhs_type := c.tc.parse_canonical_type(rhs_name_source_id)
	if rhs_type is types.Alias && markused_alias_base_matches_type(c.tc, rhs_type, declared_type) {
		return rhs_type.source_type_id()
	}
	return declared_id
}

// contextual_alias_type_name preserves the owner module of an alias written
// unqualified in a declaration. Markused workers analyze bodies from many
// modules with one checker view, so parsing that source spelling through the
// worker's last module would attach an unrelated module to the alias.
fn (c &CallCollector) contextual_alias_type_name(type_text_id u32, cur_module string, imports map[string]string) ?u32 {
	clean := markused_trimmed_source_type(type_text_id)
	shape := flat.source_type_shape(clean)
	for i, wrapper in [flat.SourceTypeShape.mutable_type, .shared_type, .atomic_type, .variadic,
		.array_type, .pointer, .option_type, .result_type] {
		if shape.has(wrapper) {
			inner := c.contextual_alias_type_name(flat.source_type_syntax(clean).raw_base(), cur_module, imports) or { return none }
			kind := [flat.SourceTypeComposition.mutable_parameter, .shared_type, .atomic_type,
				.variadic_parameter, .array, .pointer, .option, .result][i]
			return flat.source_type_composition_one(kind, inner)
		}
	}
	return c.alias_type_name_from_id(clean, cur_module, imports)
}

fn markused_fn_signature_name_candidates(name string, cur_module string) []string {
	dotted_name := qualify_fn(cur_module, name)
	mut names := []string{}
	if cur_module.len > 0 && cur_module != 'main' && cur_module != 'builtin' {
		names << dotted_name
		names << markused_c_name(dotted_name)
		names << name
		names << markused_c_name(name)
	} else {
		names << name
		names << markused_c_name(name)
		names << dotted_name
		names << markused_c_name(dotted_name)
	}
	mut deduped := []string{cap: names.len}
	for item in names {
		if item.len > 0 && item !in deduped {
			deduped << item
		}
	}
	return deduped
}

fn (c &CallCollector) local_values_need_visibility(local_values map[string]bool, cur_module string, imports map[string]string) bool {
	for name, _ in local_values {
		if c.name_may_reference_fn(name, cur_module, imports) {
			return true
		}
	}
	return false
}

fn (c &CallCollector) local_value_visibility_names(local_values map[string]bool, cur_module string, imports map[string]string) map[string]bool {
	mut names := map[string]bool{}
	for name, _ in local_values {
		if c.name_may_reference_fn(name, cur_module, imports) {
			names[name] = true
		}
	}
	return names
}

fn markused_visible_local_idents(a &flat.FlatAst, root &flat.Node, local_values map[string]bool) map[int]bool {
	mut visible_ids := map[int]bool{}
	if local_values.len == 0 {
		return visible_ids
	}
	mut locals := map[string]int{}
	mut local_stack := []string{}
	if root.kind == .fn_decl {
		for i in 0 .. root.children_count {
			child := a.child_node(root, i)
			if child.kind == .param && child.value in local_values {
				markused_push_visible_local(child.value, mut locals, mut local_stack)
			}
		}
	}
	markused_collect_visible_local_idents(a, root, local_values, mut locals, mut local_stack, mut visible_ids)
	return visible_ids
}

fn markused_ident_is_visible_local(id flat.NodeId, name string, local_values map[string]bool, visible_local_idents map[int]bool) bool {
	// The integer node-id probe is cheaper than hashing the name and rejects
	// most identifiers, so it runs first; the result is the same conjunction.
	return name.len > 0 && visible_local_idents.len > 0 && int(id) in visible_local_idents
		&& name in local_values
}

fn markused_push_visible_local(name string, mut locals map[string]int, mut local_stack []string) {
	if name.len == 0 {
		return
	}
	locals[name] = (locals[name] or { 0 }) + 1
	local_stack << name
}

fn markused_restore_visible_local_scope(mark int, mut locals map[string]int, mut local_stack []string) {
	for local_stack.len > mark {
		name := local_stack.pop()
		count := locals[name] or { 0 }
		if count <= 1 {
			locals.delete(name)
		} else {
			locals[name] = count - 1
		}
	}
}

// markused_local_visible reports whether `name` is a visible local binding. Only
// names of `local_values` are ever pushed onto `locals`, so a positive count
// alone proves both, with one hash instead of two.
fn markused_local_visible(name string, _ map[string]bool, locals map[string]int) bool {
	return name.len > 0 && locals.len > 0 && (locals[name] or { 0 }) > 0
}

@[direct_array_access]
fn markused_collect_visible_local_idents(a &flat.FlatAst, node &flat.Node, local_values map[string]bool, mut locals map[string]int, mut local_stack []string, mut visible_ids map[int]bool) {
	for i in 0 .. node.children_count {
		child_id := a.child(node, i)
		if int(child_id) < 0 {
			continue
		}
		child := a.node(child_id)
		match child.kind {
			.fn_decl, .c_fn_decl, .fn_literal {
				continue
			}
			.lambda_expr {
				scope_mark := local_stack.len
				for j in 0 .. child.children_count - 1 {
					param := a.child_node(child, j)
					if param.kind == .ident && param.value in local_values {
						markused_push_visible_local(param.value, mut locals, mut local_stack)
					}
				}
				if child.children_count > 0 {
					body := a.child_node(child, int(child.children_count) - 1)
					markused_collect_visible_local_idents(a, body, local_values, mut locals,
						mut local_stack, mut visible_ids)
				}
				markused_restore_visible_local_scope(scope_mark, mut locals, mut local_stack)
			}
			.block, .if_expr, .match_stmt, .for_stmt, .for_in_stmt {
				scope_mark := local_stack.len
				markused_collect_visible_local_idents(a, child, local_values, mut locals, mut local_stack, mut visible_ids)
				markused_restore_visible_local_scope(scope_mark, mut locals, mut local_stack)
			}
			.decl_assign {
				mut i2 := 1
				for i2 < child.children_count {
					rhs_id := a.child(child, i2)
					if int(rhs_id) >= 0 {
						markused_mark_visible_local_ident(a, rhs_id, local_values, locals, mut visible_ids)
						rhs := a.node(rhs_id)
						if rhs.children_count > 0 {
							markused_collect_visible_local_idents(a, rhs, local_values, mut locals, mut local_stack, mut visible_ids)
						}
					}
					i2 += 2
				}
				i2 = 0
				for i2 + 1 < child.children_count {
					lhs_id := a.child(child, i2)
					lhs := a.node(lhs_id)
					if lhs.kind == .ident && markused_local_visible(lhs.value, local_values, locals) {
						visible_ids[int(lhs_id)] = true
					}
					if lhs.kind == .ident && lhs.value in local_values {
						markused_push_visible_local(lhs.value, mut locals, mut local_stack)
					}
					i2 += 2
				}
			}
			else {
				if child.kind == .ident && markused_local_visible(child.value, local_values, locals) {
					visible_ids[int(child_id)] = true
				}
				if child.children_count > 0 {
					markused_collect_visible_local_idents(a, child, local_values, mut locals, mut local_stack, mut visible_ids)
				}
			}
		}
	}
}

fn markused_mark_visible_local_ident(a &flat.FlatAst, id flat.NodeId, local_values map[string]bool, locals map[string]int, mut visible_ids map[int]bool) {
	node := a.node(id)
	if node.kind == .ident && markused_local_visible(node.value, local_values, locals) {
		visible_ids[int(id)] = true
	}
}

fn (c &CallCollector) top_level_decl_rhs_type_name(rhs_id flat.NodeId, cur_module string, imports map[string]string, local_values map[string]bool, local_types flat.TypeTextBindings) u32 {
	rhs := c.a.node(rhs_id)
	if rhs.kind == .struct_init && rhs.source_value_id() != 0 {
		resolved := markused_resolve_imported_source_type(rhs.source_value_id(), imports)
		struct_type := c.struct_lookup_name(resolved, cur_module)
		if struct_type != 0 {
			return struct_type
		}
		if c.receiver_is_generic_struct_application(resolved, cur_module) {
			return resolved
		}
		// An alias-to-struct literal (`Alias{...}` where `type Alias = Base`) is not
		// itself a struct key; keep the alias name so a later method call on the var
		// resolves `Alias.method` (methods are registered on the alias).
		if c.tc.type_aliases.contains(resolved)
			|| markused_resolve_imported_source_type(rhs.source_value_id(), imports) in c.tc.type_aliases.ids {
			return resolved
		}
		return struct_type
	}
	type_name := c.top_level_expr_type_name(rhs_id, cur_module, imports, local_values, local_types, false)
	if type_name != 0 {
		struct_type := c.struct_lookup_name(type_name, cur_module)
		if struct_type != 0 {
			return struct_type
		}
		return type_name
	}
	return u32(0)
}

fn (c &CallCollector) collect_top_level_stmt_calls(id flat.NodeId, cur_module string, imports map[string]string, mut local_values map[string]bool, mut local_types flat.TypeTextBindings, mut calls []string) {
	if int(id) < 0 {
		return
	}
	node := c.a.node(id)
	match node.kind {
		.decl_assign, .assign {
			c.collect_top_level_assign_calls(node, cur_module, imports, mut local_values, mut local_types, mut calls)
		}
		.global_decl {
			c.collect_top_level_global_decl_calls(node, cur_module, imports, mut local_values, mut local_types, mut calls)
		}
		.block, .for_stmt {
			mut nested_values := markused_clone_bool_map(local_values)
			mut nested_types := markused_clone_local_types(local_types)
			c.collect_top_level_child_calls(node, cur_module, imports, mut nested_values, mut nested_types, mut calls)
		}
		.for_in_stmt {
			mut nested_values := markused_clone_bool_map(local_values)
			mut nested_types := markused_clone_local_types(local_types)
			// Register the loop variable's element type so a method call on it in the
			// body (`for a in addrs { a.family() }`) marks that method used. The checker
			// types the loop var, but this simplified top-level collector must too.
			c.register_top_level_for_in_vars(node, cur_module, imports, mut nested_values, mut nested_types)
			c.collect_top_level_child_calls(node, cur_module, imports, mut nested_values, mut nested_types, mut calls)
		}
		.if_expr, .match_stmt {
			c.collect_top_level_branch_calls(node, cur_module, imports, local_values, local_types, mut calls)
		}
		else {
			c.collect_top_level_expr_calls(id, cur_module, imports, local_values, local_types, mut calls)
		}
	}
}

fn (c &CallCollector) collect_top_level_child_calls(node &flat.Node, cur_module string, imports map[string]string, mut local_values map[string]bool, mut local_types flat.TypeTextBindings, mut calls []string) {
	for i in 0 .. node.children_count {
		child_id := c.a.child(node, i)
		if int(child_id) >= 0 {
			c.collect_top_level_stmt_calls(child_id, cur_module, imports, mut local_values, mut local_types, mut calls)
		}
	}
}

fn (c &CallCollector) collect_top_level_branch_calls(node &flat.Node, cur_module string, imports map[string]string, local_values map[string]bool, local_types flat.TypeTextBindings, mut calls []string) {
	for i in 0 .. node.children_count {
		child_id := c.a.child(node, i)
		if int(child_id) < 0 {
			continue
		}
		mut branch_values := local_values.clone()
		mut branch_types := local_types.clone()
		c.collect_top_level_stmt_calls(child_id, cur_module, imports, mut branch_values, mut branch_types, mut calls)
	}
}

fn (c &CallCollector) collect_top_level_assign_calls(node &flat.Node, cur_module string, imports map[string]string, mut local_values map[string]bool, mut local_types flat.TypeTextBindings, mut calls []string) {
	if node.kind !in [.decl_assign, .assign] {
		return
	}
	mut i := 0
	pre_values := markused_clone_bool_map(local_values)
	pre_types := markused_clone_local_types(local_types)
	for i + 1 < node.children_count {
		rhs_id := c.a.child(node, i + 1)
		if int(rhs_id) >= 0 {
			mut rhs_values := markused_clone_bool_map(pre_values)
			mut rhs_types := markused_clone_local_types(pre_types)
			c.collect_top_level_stmt_calls(rhs_id, cur_module, imports, mut rhs_values, mut rhs_types, mut calls)
		}
		i += 2
	}
	i = 0
	for i + 1 < node.children_count {
		lhs_id := c.a.child(node, i)
		rhs_id := c.a.child(node, i + 1)
		if int(lhs_id) >= 0 && int(rhs_id) >= 0 {
			lhs := c.a.node(lhs_id)
			if lhs.kind == .ident && lhs.value.len > 0 {
				local_values[lhs.value] = true
				type_name_source_id_4 := c.top_level_decl_rhs_type_name(rhs_id, cur_module, imports, pre_values, pre_types)
				if type_name_source_id_4 != 0 {
					local_types.set_id(lhs.value, type_name_source_id_4)
				}
			}
		}
		i += 2
	}
}

fn (c &CallCollector) collect_top_level_global_decl_calls(node &flat.Node, cur_module string, imports map[string]string, mut local_values map[string]bool, mut local_types flat.TypeTextBindings, mut calls []string) {
	if node.kind != .global_decl {
		return
	}
	pre_values := markused_clone_bool_map(local_values)
	pre_types := markused_clone_local_types(local_types)
	for i in 0 .. node.children_count {
		field := c.a.child_node(node, i)
		if field.children_count == 0 {
			continue
		}
		expr_id := c.a.child(field, 0)
		if int(expr_id) >= 0 {
			mut rhs_values := markused_clone_bool_map(pre_values)
			mut rhs_types := markused_clone_local_types(pre_types)
			c.collect_top_level_stmt_calls(expr_id, cur_module, imports, mut rhs_values, mut rhs_types, mut calls)
		}
	}
	for i in 0 .. node.children_count {
		field := c.a.child_node(node, i)
		if field.value.len == 0 {
			continue
		}
		local_values[field.value] = true
		if field.children_count > 0 {
			expr_id := c.a.child(field, 0)
			type_name_source_id_5 := c.top_level_decl_rhs_type_name(expr_id, cur_module, imports, pre_values, pre_types)
			if type_name_source_id_5 != 0 {
				local_types.set_id(field.value, type_name_source_id_5)
			}
		}
	}
}

fn (c &CallCollector) collect_top_level_expr_calls(id flat.NodeId, cur_module string, imports map[string]string, local_values map[string]bool, local_types flat.TypeTextBindings, mut calls []string) {
	if int(id) < 0 {
		return
	}
	mut stack := [id]
	for stack.len > 0 {
		child_id := stack.pop()
		child := c.a.node(child_id)
		match child.kind {
			.ident {
				if child.value_text() !in local_values {
					c.collect_fn_value_ident(child_id, child.value_text(), cur_module, imports, false, mut calls)
				}
			}
			.selector {
				if c.selector_is_enum_value(child, cur_module, imports, local_values) {
					// Enum fields are values even when a method has the same name.
				} else if c.collect_interface_method_value_selector(child, mut calls) {
					// handled
				} else if !c.top_level_selector_base_is_local(child, local_values) {
					c.collect_fn_value_selector(child_id, child, cur_module, imports, local_values, mut calls)
				} else if child.children_count > 0 {
					// A method value on a local/global receiver (`app.frame` passed as a
					// callback field, e.g. `gg.new_context(frame_fn: app.frame)`). Mark the
					// receiver's method so its body is emitted for the generated
					// method-value wrapper; a plain field access resolves to no method and
					// is left untouched.
					base_id := c.a.child(child, 0)
					c.collect_top_level_typed_receiver_method(base_id, child.value_text(), cur_module, imports, local_values, local_types, mut calls)
				}
			}
			.call {
				c.collect_top_level_call(child_id, child, cur_module, imports, local_values, local_types, mut calls)
			}
			.prefix {
				if child.op == .amp && child.children_count > 0 {
					inner_id := c.a.child(child, 0)
					if int(inner_id) >= 0 {
						inner := c.a.node(inner_id)
						if inner.kind == .struct_init {
							calls << 'memdup'
						}
					}
				}
			}
			.string_interp {
				calls << 'string_plus_many'
				for i in 0 .. child.children_count {
					mut part_id := c.a.child(child, i)
					part := c.a.node(part_id)
					if part.kind == .directive && part.value == 'string_interp_format'
						&& part.children_count > 0 {
						if part.typ_text() == 'p' {
							continue
						}
						part_id = c.a.child(part, 0)
					}
					c.collect_top_level_stringification_calls(part_id, cur_module, imports,
						local_values, local_types, mut calls)
				}
			}
			.assign, .selector_assign, .index_assign {
				c.collect_assign_operator_call(child, cur_module, local_types, mut calls)
			}
			.index {
				c.collect_index_overload_getter_method(child, cur_module, local_types, mut calls)
			}
			.infix {
				if child.op == .plus {
					calls << 'string__plus'
				}
				if child.children_count >= 2 {
					lhs_id := c.a.child(child, 0)
					rhs_id := c.a.child(child, 1)
					if child.op in [.dot, .amp] {
						lhs_name := c.qualified_expr_name(lhs_id)
						if lhs_name.len > 0 {
							rhs := c.a.node(rhs_id)
							if rhs.kind == .call && rhs.children_count > 0 {
								fn_node := c.a.child_node(rhs, 0)
								if fn_node.kind == .ident && fn_node.value.len > 0 {
									mod_name := if lhs_name in imports {
										imports[lhs_name]
									} else {
										lhs_name
									}
									calls << mod_name + '.' + fn_node.value
									calls << qualify_fn(cur_module, lhs_name + '.' + fn_node.value)
								}
							}
						}
					}
					if int(lhs_id) >= 0 {
						c.collect_struct_operator_call(lhs_id, child.op, cur_module, local_types, mut calls)
					}
				}
			}
			.or_expr {
				if child.children_count > 0 {
					expr_id := c.a.child(child, 0)
					c.collect_zero_struct_default_calls(c.node_type(expr_id), cur_module, imports, mut calls)
				}
			}
			.struct_init {
				c.collect_struct_default_calls(child, cur_module, imports, mut calls)
			}
			.array_init {
				c.collect_array_default_calls(child, cur_module, imports, mut calls)
			}
			else {}
		}

		if child.children_count > 0 {
			mut j := int(child.children_count) - 1
			for j >= 0 {
				next_id := c.a.child(child, j)
				if int(next_id) >= 0 {
					stack << next_id
				}
				j--
			}
		}
	}
}

fn (c &CallCollector) top_level_selector_base_is_local(node flat.Node, local_values map[string]bool) bool {
	return c.selector_base_is_local(&node, local_values)
}

fn (c &CallCollector) selector_base_is_local(node &flat.Node, local_values map[string]bool) bool {
	if node.children_count == 0 {
		return false
	}
	base_id := c.a.child(node, 0)
	if int(base_id) < 0 {
		return false
	}
	base := c.a.node(base_id)
	return base.kind == .ident && base.value_text() in local_values
}

fn (c &CallCollector) expr_lowers_to_map_str(id flat.NodeId, local_values map[string]bool, local_types flat.TypeTextBindings) bool {
	if int(id) < 0 {
		return false
	}
	if markused_type_lowers_to_map_str(c.node_type(id)) {
		return true
	}
	expr := c.a.node(id)
	if expr.kind == .ident && expr.value.len > 0 && expr.value in local_values {
		if type_name_source_id := local_types.type_id(expr.value) {
			return markused_type_name_lowers_to_map_str(type_name_source_id, c.tc)
		}
	}
	return false
}

fn (c &CallCollector) top_level_expr_lowers_to_map_str(id flat.NodeId, cur_module string, imports map[string]string, local_values map[string]bool, local_types flat.TypeTextBindings) bool {
	if int(id) < 0 {
		return false
	}
	if markused_type_lowers_to_map_str(c.node_type(id)) {
		return true
	}
	type_name := c.top_level_expr_type_name(id, cur_module, imports, local_values, local_types, false)
	return type_name != 0 && markused_type_name_lowers_to_map_str(type_name, c.tc)
}

// collect_top_level_stringification_calls resolves script values with their own
// imports and local bindings, before the checker has cached their expression types.
fn (c &CallCollector) collect_top_level_stringification_calls(id flat.NodeId, cur_module string, imports map[string]string, local_values map[string]bool, local_types flat.TypeTextBindings, mut calls []string) {
	type_name_source_id_6 := c.top_level_expr_type_name(id, cur_module, imports, local_values, local_types, false)
	if type_name_source_id_6 == 0 {
		return
	}
	typ := c.tc.parse_canonical_type(type_name_source_id_6)
	mut used := map[string]bool{}
	mut seen := map[u32]bool{}
	enqueue_stringified_type_dependencies(typ, cur_module, c.tc, c.auto_str_skipped_fields,
		mut used, mut calls, mut seen)
}

fn (c &CallCollector) collect_top_level_call(call_id flat.NodeId, call &flat.Node, cur_module string, imports map[string]string, local_values map[string]bool, local_types flat.TypeTextBindings, mut calls []string) {
	mut resolved_call := ''
	if resolved := c.tc.resolved_call_name(call_id) {
		resolved_call = resolved
	}
	c.collect_lowered_join_path_single(call, resolved_call, imports, local_values, mut calls)
	if call.children_count == 0 {
		if resolved_call.len > 0 {
			calls << resolved_call
		}
		return
	}
	callee_id := c.a.child(call, 0)
	if int(callee_id) < 0 {
		return
	}
	callee := c.a.node(callee_id)
	if callee.kind == .ident && callee.value_text().len > 0 {
		if resolved_call.len > 0 {
			calls << resolved_call
		}
		if callee.value_text() in ['print', 'println', 'eprint', 'eprintln'] && call.children_count >= 2 {
			arg_id := c.a.child(call, 1)
			c.collect_top_level_stringification_calls(arg_id, cur_module, imports, local_values,
				local_types, mut calls)
			if c.top_level_expr_lowers_to_map_str(arg_id, cur_module, imports, local_values, local_types) {
				calls << 'string__plus'
			}
		}
		if callee.value_text() !in local_values {
			calls << callee.value_text()
			qcallee := qualify_fn(cur_module, callee.value_text())
			if qcallee != callee.value_text() {
				calls << qcallee
			}
		}
	} else if callee.kind == .selector && callee.value_text().len > 0 {
		c.collect_top_level_selector_call(callee, callee.value_text(), resolved_call, cur_module, imports, local_values, local_types, mut calls)
	} else if resolved_call.len > 0 {
		calls << resolved_call
	}
	if c.detect_generics {
		c.collect_generic_alias_operator_usage(call, resolved_call, cur_module, imports, local_values, local_types, mut calls)
	}
	for ci in 1 .. call.children_count {
		arg_id := c.a.child(call, ci)
		if int(arg_id) >= 0 {
			arg := c.a.node(arg_id)
			if arg.kind == .ident && arg.value.len > 0 {
				if arg.value !in local_values {
					c.collect_fn_value_ident(arg_id, arg.value, cur_module, imports, false, mut calls)
				}
			}
		}
	}
}

struct GenericCalls {
	names     map[string]bool
	nodes     int
	functions int
}

fn markused_generic_calls(a &flat.FlatAst, tc &types.TypeChecker, fn_decls map[string]FnDeclInfo) &GenericCalls {
	mut names := map[string]bool{}
	for name, _ in tc.fn_generic_params {
		names[name] = true
		names[name.all_after_last('.')] = true
	}
	mut node_states := []u8{len: a.nodes.len}
	for name, info in fn_decls {
		id := int(info.node_id)
		if id < 0 || id >= node_states.len { continue }
		if node_states[id] == 0 {
			node := a.node(info.node_id)
			node_states[id] = if node.kind == .fn_decl && node.generic_params().len > 0 {
				2
			} else {
				1
			}
		}
		if node_states[id] == 2 {
			names[name] = true
			names[name.all_after_last('.')] = true
		}
	}
	return &GenericCalls{ names: names, nodes: a.nodes.len, functions: tc.fn_generic_params.len }
}

fn (c &CallCollector) generic_call_possible(call &flat.Node, resolved_call string, local_values map[string]bool) bool {
	gate := c.generic_calls
	if isnil(gate) || gate.nodes != c.a.nodes.len || gate.functions != c.tc.fn_generic_params.len {
		return true
	}
	if resolved_call in gate.names || resolved_call.contains('_T_') {
		return true
	}
	mut callee := c.a.child_node(call, 0)
	if callee.kind == .index && callee.children_count > 0 && callee.value != 'range' {
		callee = c.a.child_node(callee, 0)
	}
	if callee.kind == .ident {
		return callee.value_text() !in local_values && (callee.value_text().all_after_last('.') in gate.names
			|| callee.value_text().contains('_T_'))
	}
	if callee.kind == .selector && callee.children_count > 0 {
		base := c.a.child_node(callee, 0)
		return base.kind == .ident && base.value_text() !in local_values
			&& (callee.value_text().all_after_last('.') in gate.names || callee.value_text().contains('_T_'))
	}
	return false
}

fn (c &CallCollector) collect_generic_alias_operator_usage(call &flat.Node, resolved_call string, cur_module string, imports map[string]string, local_values map[string]bool, local_types flat.TypeTextBindings, mut calls []string) {
	if call.children_count == 0 || !c.generic_call_possible(call, resolved_call, local_values) {
		return
	}
	for candidate in c.generic_alias_operator_call_candidates(call, resolved_call, cur_module, imports, local_values) {
		info := c.fn_decls[candidate] or { continue }
		fn_node := c.a.node(info.node_id)
		generic_params := c.generic_fn_param_names(candidate, fn_node)
		param_texts_source_ids := c.generic_fn_param_type_texts(candidate, fn_node).clone()

		inferred := c.infer_alias_generic_args(call, candidate, param_texts_source_ids.clone(), generic_params,
			cur_module, imports, local_values, local_types)
		if inferred.len == 0 {
			continue
		}
		specialized_locals := c.generic_alias_local_type_names(fn_node, info.module, imports, inferred)
		c.collect_generic_body_alias_operator_calls(fn_node, info.module, specialized_locals, mut calls)
	}
}

fn (c &CallCollector) generic_alias_operator_call_candidates(call &flat.Node, resolved_call string, cur_module string, imports map[string]string, local_values map[string]bool) []string {
	mut names := []string{}
	c.add_generic_alias_candidate(resolved_call, mut names)
	mut callee := c.a.child_node(call, 0)
	if callee.kind == .index && callee.children_count > 0 && callee.value != 'range' {
		callee = c.a.child_node(callee, 0)
	}
	if callee.kind == .ident && callee.value_text().len > 0 && callee.value_text() !in local_values {
		c.add_generic_alias_candidate(callee.value_text(), mut names)
		c.add_generic_alias_candidate(qualify_fn(cur_module, callee.value_text()), mut names)
	} else if callee.kind == .selector && callee.value_text().len > 0 && callee.children_count > 0 {
		base := c.a.child_node(callee, 0)
		if base.kind == .ident && base.value_text().len > 0 && base.value_text() !in local_values {
			if imported := imports[base.value_text()] {
				c.add_generic_alias_candidate('${imported}.${callee.value_text()}', mut names)
			}
			c.add_generic_alias_candidate('${base.value_text()}.${callee.value_text()}', mut names)
			c.add_generic_alias_candidate(qualify_fn(cur_module, '${base.value_text()}.${callee.value_text()}'), mut names)
		}
	}
	return names
}

fn (c &CallCollector) add_generic_alias_candidate(name string, mut names []string) {
	if name.len == 0 {
		return
	}
	if c.generic_candidate_is_generic(name) {
		if name !in names {
			names << name
		}
		return
	}
	if marker := name.index('_T_') {
		base := name[..marker]
		if c.generic_candidate_is_generic(base) && base !in names {
			names << base
		}
	}
}

fn (c &CallCollector) generic_candidate_is_generic(name string) bool {
	if name in c.tc.fn_generic_params {
		return true
	}
	if info := c.fn_decls[name] {
		node := c.a.node(info.node_id)
		return node.kind == .fn_decl && node.generic_params().len > 0
	}
	return false
}

fn (c &CallCollector) generic_fn_param_names(name string, fn_node &flat.Node) []u32 {
	if params := c.tc.fn_generic_params[name] {
		return params
	}
	return types.generic_param_ids(fn_node.generic_params())
}

fn (c &CallCollector) generic_fn_param_type_texts(name string, fn_node &flat.Node) []u32 {
	if param_texts_source_ids := c.tc.fn_param_type_texts.type_ids(name) {
		return param_texts_source_ids.clone()
	}
	mut result := []u32{}
	for i in 0 .. fn_node.children_count {
		child := c.a.child_node(fn_node, i)
		if child.kind == .param {
			result << child.typ
		}
	}
	return result
}

fn (c &CallCollector) infer_alias_generic_args(call &flat.Node, fn_name string, param_texts_ids []u32, generic_params []u32, cur_module string, imports map[string]string, local_values map[string]bool, local_types flat.TypeTextBindings) map[u32]u32 {
	mut inferred := map[u32]u32{}
	if call.children_count == 0 || param_texts_ids.len == 0 || generic_params.len == 0 {
		return inferred
	}
	mut param_idx := 0
	callee := c.a.child_node(call, 0)
	if callee.kind == .selector && callee.children_count > 0 {
		if actual := c.alias_aware_expr_type(c.a.child(callee, 0), cur_module, imports, local_values, local_types) {
			markused_infer_alias_generic_type(param_texts_ids[0], actual, generic_params, mut inferred)
			param_idx = 1
		}
	}
	for arg_i in 1 .. call.children_count {
		if param_idx >= param_texts_ids.len {
			break
		}
		raw_arg_id := c.a.child(call, arg_i)
		raw_arg := c.a.node(raw_arg_id)
		if raw_arg.kind == .field_init && raw_arg.children_count > 0 {
			field_pattern_source_id := c.generic_factory_short_struct_field_type_text(fn_name,
				param_texts_ids[param_idx], raw_arg.value, cur_module)

			if field_pattern_source_id != 0 {
				c.infer_alias_generic_argument(fn_name, field_pattern_source_id, c.a.child(raw_arg, 0),
					generic_params, cur_module, imports, local_values, local_types, mut inferred)
			}
			continue
		}
		c.infer_alias_generic_argument(fn_name, param_texts_ids[param_idx], raw_arg_id,
			generic_params, cur_module, imports, local_values, local_types, mut inferred)
		param_idx++
	}
	return inferred
}

fn (c &CallCollector) infer_alias_generic_argument(fn_name string, pattern_id u32, arg_id flat.NodeId, generic_params []u32, cur_module string, imports map[string]string, local_values map[string]bool, local_types flat.TypeTextBindings, mut inferred map[u32]u32) {
	arg := c.a.node(arg_id)
	if flat.source_type_shape(pattern_id).has(.variadic) && arg.kind == .prefix && arg.value == '...'
		&& arg.children_count > 0 {
		c.infer_alias_generic_argument(fn_name, flat.source_type_composition_one(.array, flat.source_type_syntax(pattern_id).raw_base()), c.a.child(arg, 0),
			generic_params, cur_module, imports, local_values, local_types, mut inferred)
		return
	}
	if actual := c.alias_aware_expr_type(arg_id, cur_module, imports, local_values, local_types) {
		markused_infer_alias_generic_type(pattern_id, actual, generic_params, mut inferred)
	}
	if inferred.len < generic_params.len {
		actual_text_source_id := c.top_level_expr_type_name(arg_id, cur_module, imports, local_values,
			local_types, false)
		if actual_text_source_id != 0 && actual_text_source_id != flat.source_type_unknown {
			markused_infer_generic_type_text(pattern_id, actual_text_source_id, generic_params, mut inferred)
		}
	}
	if inferred.len < generic_params.len {
		if actual := c.alias_aware_expr_type(arg_id, cur_module, imports, local_values, local_types) {
			for generic, concrete in c.tc.infer_generic_reachability_type_args(fn_name,
				pattern_id, actual, generic_params) {
				if generic !in inferred && concrete != flat.source_type_unknown
					&& concrete != flat.source_type_generic {
					inferred[generic] = concrete
				}
			}
		}
	}
}

fn (c &CallCollector) generic_factory_short_struct_field_type_text(fn_name string, param_text_id u32, field_name string, cur_module string) u32 {
	base, args, is_generic := markused_generic_app_parts(param_text_id)
	if !is_generic || field_name.len == 0 {
		return u32(0)
	}
	mut decl_module := c.tc.fn_type_modules[fn_name] or { cur_module }
	mut decl_imports := map[string]string{}
	if decl := c.fn_decls[fn_name] {
		decl_module = decl.module
		if c.import_contexts.len > 0 {
			decl_imports = c.imports(decl.import_context)
		}
	}
	resolved_base := markused_resolve_imported_source_type(base, decl_imports)
	mut candidates := []u32{}
	if !flat.source_type_shape(resolved_base).has(.dotted) && decl_module !in ['', 'main', 'builtin'] {
		candidates << flat.source_type_composition_two(.qualified, flat.source_type_id(decl_module), resolved_base)
	}
	candidates << resolved_base
	for candidate in candidates {
		mut field_type := u32(0)
		if info := c.struct_decl_info(candidate, decl_module) {
			decl := c.a.node(info.node_id)
			for i in 0 .. decl.children_count {
				field := c.a.child_node(decl, i)
				if field.kind == .field_decl && field.value == field_name {
					field_type = field.typ
					break
				}
			}
		}
		if field_type == 0 {
			for field in c.tc.struct_fields_for_type(candidate) {
				if field.name == field_name {
					field_type = field.typ.source_type_id()
					break
				}
			}
		}
		if field_type == 0 {
			continue
		}
		params := c.tc.struct_generic_params[candidate] or {
			c.tc.struct_generic_params[base] or { []u32{} }
		}
		return types.subst_generic_text(field_type, args, params)
	}
	return u32(0)
}

fn (c &CallCollector) alias_aware_expr_type(id flat.NodeId, cur_module string, imports map[string]string, local_values map[string]bool, local_types flat.TypeTextBindings) ?types.Type {
	if int(id) < 0 {
		return none
	}
	if typ := c.syntax_alias_expr_type(id, cur_module, imports) {
		return typ
	}
	type_name_source_id_7 := c.top_level_expr_type_name(id, cur_module, imports, local_values, local_types, false)
	if type_name_source_id_7 != 0 {
		typ := c.tc.parse_type(type_name_source_id_7)
		if typ !is types.Unknown && typ !is types.Void {
			return *typ
		}
	}
	typ := c.node_type(id)
	if typ !is types.Unknown && typ !is types.Void {
		return typ
	}
	return none
}

fn (c &CallCollector) syntax_alias_expr_type(id flat.NodeId, cur_module string, imports map[string]string) ?types.Type {
	if int(id) < 0 {
		return none
	}
	node := c.a.node(id)
	match node.kind {
		.paren {
			if node.children_count > 0 {
				return c.syntax_alias_expr_type(c.a.child(node, 0), cur_module, imports)
			}
		}
		.prefix {
			if node.op == .amp && node.children_count > 0 {
				inner := c.syntax_alias_expr_type(c.a.child(node, 0), cur_module, imports) or {
					return none
				}
				return types.Type(types.Pointer{
					base_type: types.new_type_reference(types.Type(inner))
				})
			}
		}
		.array_literal {
			for i in 0 .. node.children_count {
				elem_id := c.a.child(node, i)
				elem_type := c.syntax_alias_expr_type(elem_id, cur_module, imports) or { continue }
				return types.Type(types.Array{
					elem_type: types.new_type_reference(types.Type(elem_type))
				})
			}
		}
		.call {
			if node.children_count > 0 {
				callee := c.a.child_node(node, 0)
				if alias_type := c.alias_type_from_callee(callee, cur_module, imports) {
					return alias_type
				}
			}
		}
		.cast_expr {
			if node.source_value_id() != 0 {
				if contextual_alias_source_id := c.contextual_alias_type_name(node.source_value_id(), cur_module, imports) {
					return c.tc.parse_canonical_type(contextual_alias_source_id)
				}
				if alias_type := c.alias_type_from_name(node.source_value_id(), cur_module, imports) {
					return alias_type
				}
			}
		}
		else {}
	}

	return none
}

fn (c &CallCollector) alias_type_from_callee(callee &flat.Node, cur_module string, imports map[string]string) ?types.Type {
	if c.tc.type_aliases.ids.len == 0 { return none }
	if callee.kind == .ident && callee.source_value_id() != 0 {
		return c.alias_type_from_name(callee.source_value_id(), cur_module, imports)
	}
	if callee.kind == .selector && callee.source_value_id() != 0 && callee.children_count > 0 {
		base := c.a.child_node(callee, 0)
		if base.kind == .ident && base.source_value_id() != 0 {
			if imported := imports[base.value_text()] {
				candidate := flat.source_type_composition_two(.qualified, flat.source_type_id(imported), callee.source_value_id())
				if alias_type := c.alias_type_from_name(candidate, cur_module, imports) {
					return alias_type
				}
			}
			candidate := flat.source_type_composition_two(.qualified, base.source_value_id(), callee.source_value_id())
			return c.alias_type_from_name(candidate, cur_module, imports)
		}
	}
	return none
}

fn (c &CallCollector) alias_type_from_name(name_id u32, cur_module string, imports map[string]string) ?types.Type {
	alias_id := c.alias_type_name_from_id(name_id, cur_module, imports) or { return none }
	return c.tc.parse_canonical_type(alias_id)
}

// alias_type_name_from_id checks candidate spelling identities in their original order.
fn (c &CallCollector) alias_type_name_from_id(name u32, cur_module string, imports map[string]string) ?u32 {
	if c.tc.type_aliases.ids.len == 0 { return none }
	resolved := markused_resolve_imported_source_type(name, imports)
	if resolved != 0 && c.tc.type_aliases.contains(resolved) {
		return resolved
	}
	if name != resolved && name != 0 && c.tc.type_aliases.contains(name) {
		return name
	}
	if cur_module.len > 0 && cur_module != 'main' && cur_module != 'builtin' {
		module_id := flat.source_type_id(cur_module)
		qualified := flat.source_type_composition_two(.qualified, module_id, resolved)
		if c.tc.type_aliases.contains(qualified) { return qualified }
		if name != resolved {
			qualified_name := flat.source_type_composition_two(.qualified, module_id, name)
			if c.tc.type_aliases.contains(qualified_name) {
				return qualified_name
			}
		}
	}
	return none
}

fn markused_alias_type_candidates(resolved u32, name u32, cur_module string) []u32 {
	mut candidates := []u32{cap: 4}
	candidates << resolved
	candidates << name
	if cur_module.len > 0 && cur_module != 'main' && cur_module != 'builtin' {
		candidates << flat.source_type_composition_two(.qualified, flat.source_type_id(cur_module), resolved)
		candidates << flat.source_type_composition_two(.qualified, flat.source_type_id(cur_module), name)
	}
	mut deduped := []u32{cap: candidates.len}
	for candidate in candidates {
		if candidate != 0 && candidate !in deduped {
			deduped << candidate
		}
	}
	return deduped
}

fn markused_generic_call_arg_value(a &flat.FlatAst, id flat.NodeId) flat.NodeId {
	if int(id) < 0 {
		return id
	}
	node := a.node(id)
	if node.kind == .field_init && node.children_count > 0 {
		return a.child(node, 0)
	}
	return id
}

fn markused_infer_alias_generic_type(param_id u32, actual types.Type, generic_params []u32, mut inferred map[u32]u32) {
	markused_infer_alias_generic_type_inner(param_id, actual, generic_params, mut inferred)
}

fn markused_infer_alias_generic_type_inner(param_id u32, actual types.Type, generic_params []u32, mut inferred map[u32]u32) {
	clean := markused_trimmed_source_type(param_id)
	if clean == 0 { return }
	shape := flat.source_type_shape(clean)
	if shape.has(.mutable_type) || shape.has(.shared_type) || shape.has(.variadic) {
		markused_infer_alias_generic_type_inner(flat.source_type_syntax(clean).raw_base(), actual, generic_params, mut inferred)
		return
	}
	if shape.has(.pointer) {
		child := flat.source_type_syntax(clean).raw_base()
		if actual is types.Pointer {
			markused_infer_alias_generic_type_inner(child, actual.base_type, generic_params, mut inferred)
		} else {
			markused_infer_alias_generic_type_inner(child, actual, generic_params, mut inferred)
		}
		return
	}
	if shape.has(.array_type) {
		actual_clean := types.unalias_type(types.unwrap_pointer(types.unalias_type(actual)))
		if actual_clean is types.Array {
			markused_infer_alias_generic_type_inner(flat.source_type_syntax(clean).raw_base(), actual_clean.elem_type, generic_params, mut inferred)
		} else if actual_clean is types.ArrayFixed {
			markused_infer_alias_generic_type_inner(flat.source_type_syntax(clean).raw_base(), actual_clean.elem_type, generic_params, mut inferred)
		}
		return
	}
	if shape.has(.leading_bracket) {
		syntax := flat.source_type_syntax(clean)
		actual_clean := types.unalias_type(types.unwrap_pointer(types.unalias_type(actual)))
		if syntax.bracket_closed() && actual_clean is types.ArrayFixed {
			markused_infer_alias_generic_type_inner(syntax.bracket_suffix(), actual_clean.elem_type, generic_params, mut inferred)
		}
		return
	}
	if shape.has(.channel_type) {
		actual_clean := types.unalias_type(types.unwrap_pointer(types.unalias_type(actual)))
		if actual_clean is types.Channel {
			markused_infer_alias_generic_type_inner(flat.source_type_syntax(clean).raw_base(), actual_clean.elem_type, generic_params, mut inferred)
		}
		return
	}
	if shape.has(.option_type) {
		child := flat.source_type_syntax(clean).raw_base()
		if actual is types.OptionType {
			markused_infer_alias_generic_type_inner(child, actual.base_type, generic_params, mut inferred)
		} else {
			markused_infer_alias_generic_type_inner(child, actual, generic_params, mut inferred)
		}
		return
	}
	if shape.has(.result_type) {
		if actual is types.ResultType {
			markused_infer_alias_generic_type_inner(flat.source_type_syntax(clean).raw_base(), actual.base_type, generic_params, mut inferred)
		}
		return
	}
	if shape.has(.map_type) {
		syntax := flat.source_type_syntax(clean)
		actual_clean := types.unalias_type(types.unwrap_pointer(types.unalias_type(actual)))
		if syntax.bracket_closed() && actual_clean is types.Map {
			markused_infer_alias_generic_type_inner(syntax.bracket_raw_inner(), actual_clean.key_type, generic_params, mut inferred)
			markused_infer_alias_generic_type_inner(syntax.bracket_suffix(), actual_clean.value_type, generic_params, mut inferred)
		}
		return
	}
	if clean in generic_params && !markused_type_has_unknown(actual) {
		if clean !in inferred {
			type_name := resolve_type_name(actual)
			if type_name != 0 { inferred[clean] = type_name }
		}
	}
}

fn markused_infer_generic_type_text(pattern_id u32, actual_id u32, generic_params []u32, mut inferred map[u32]u32) {
	markused_infer_generic_type_inner(pattern_id, actual_id, generic_params, mut inferred)
}

fn markused_infer_generic_type_inner(pattern_id u32, actual_id u32, generic_params []u32, mut inferred map[u32]u32) {
	clean := markused_trimmed_source_type(pattern_id)
	value := markused_trimmed_source_type(actual_id)
	if clean == 0 || value == 0 { return }
	if clean in generic_params {
		if clean !in inferred { inferred[clean] = value }
		return
	}
	shape := flat.source_type_shape(clean)
	value_shape := flat.source_type_shape(value)
	for wrapper in [flat.SourceTypeShape.mutable_type, .shared_type, .atomic_type, .variadic,
		.array_type, .channel_type, .thread_type, .option_type, .result_type, .pointer] {
		if shape.has(wrapper) {
			child := flat.source_type_syntax(clean).raw_base()
			if value_shape.has(wrapper) {
				markused_infer_generic_type_inner(child, flat.source_type_syntax(value).raw_base(), generic_params, mut inferred)
			} else if wrapper in [.option_type, .variadic, .mutable_type, .shared_type, .atomic_type] {
				markused_infer_generic_type_inner(child, value, generic_params, mut inferred)
			} else if wrapper == .array_type && value_shape.has(.leading_bracket) {
				syntax := flat.source_type_syntax(value)
				if syntax.bracket_closed() {
					markused_infer_generic_type_inner(child, syntax.bracket_suffix(), generic_params, mut inferred)
				}
			}
			return
		}
	}
	if shape.has(.map_type) && value_shape.has(.map_type) {
		pattern := flat.source_type_syntax(clean)
		actual := flat.source_type_syntax(value)
		if pattern.bracket_closed() && actual.bracket_closed() {
			markused_infer_generic_type_inner(pattern.bracket_raw_inner(), actual.bracket_raw_inner(), generic_params, mut inferred)
			markused_infer_generic_type_inner(pattern.bracket_suffix(), actual.bracket_suffix(), generic_params, mut inferred)
		}
		return
	}
	if shape.has(.leading_bracket) && value_shape.has(.leading_bracket) {
		pattern := flat.source_type_syntax(clean)
		actual := flat.source_type_syntax(value)
		if pattern.bracket_closed() && actual.bracket_closed() && pattern.bracket_raw_inner() == actual.bracket_raw_inner() {
			markused_infer_generic_type_inner(pattern.bracket_suffix(), actual.bracket_suffix(), generic_params, mut inferred)
		}
		return
	}
	pattern_base, pattern_args, pattern_generic := markused_generic_app_parts(clean)
	actual_base, actual_args, actual_generic := markused_generic_app_parts(value)
	if pattern_generic && actual_generic && pattern_args.len == actual_args.len
		&& flat.source_type_syntax(pattern_base).unqualified() == flat.source_type_syntax(actual_base).unqualified() {
		for i, part in pattern_args {
			markused_infer_generic_type_inner(part, actual_args[i], generic_params, mut inferred)
		}
	}
}

fn (c &CallCollector) generic_alias_local_type_names(fn_node &flat.Node, cur_module string, imports map[string]string, inferred map[u32]u32) flat.TypeTextBindings {
	mut result := c.new_local_types()
	mut stack := []flat.NodeId{cap: int(fn_node.children_count)}
	for i in 0 .. fn_node.children_count {
		child_id := c.a.child(fn_node, i)
		if int(child_id) >= 0 {
			child := c.a.node(child_id)
			if child.kind == .param && child.value.len > 0 && child.typ != 0 {
				result.set_id(child.value, markused_resolve_imported_source_type(markused_substitute_alias_generics(child.typ, inferred), imports))
			}
			stack << child_id
		}
	}
	for stack.len > 0 {
		id := stack.pop()
		child := c.a.node(id)
		if child.kind == .param && child.value.len > 0 && child.typ != 0 {
			result.set_id(child.value, markused_resolve_imported_source_type(markused_substitute_alias_generics(child.typ, inferred), imports))
		} else if child.kind == .decl_assign {
			mut i := 0
			for i + 1 < child.children_count {
				lhs_id := c.a.child(child, i)
				rhs_id := c.a.child(child, i + 1)
				if int(lhs_id) >= 0 && int(rhs_id) >= 0 {
					lhs := c.a.node(lhs_id)
					if lhs.kind == .ident && lhs.value.len > 0 {
						type_name := if child.children_count == 2 && child.typ != 0 {
							c.local_decl_type_name(markused_substitute_alias_generics(child.typ, inferred), rhs_id, cur_module, imports, result)
						} else {
							c.top_level_decl_rhs_type_name(rhs_id, cur_module, imports, map[string]bool{}, result)
						}
						if type_name != 0 {
							result.set_id(lhs.value, type_name)
						}
					}
				}
				i += 2
			}
		}
		for i in 0 .. child.children_count {
			next_id := c.a.child(child, i)
			if int(next_id) >= 0 {
				stack << next_id
			}
		}
	}
	return result
}

fn markused_substitute_alias_generics(type_id u32, inferred map[u32]u32) u32 {
	clean := markused_trimmed_source_type(type_id)
	if inferred.len > 0 {
		if replacement := inferred[clean] { return replacement }
	}
	shape := flat.source_type_shape(clean)
	for i, wrapper in [flat.SourceTypeShape.pointer, .mutable_type, .array_type, .variadic,
		.option_type, .result_type] {
		if shape.has(wrapper) {
			kind := [flat.SourceTypeComposition.pointer, .mutable_parameter, .array,
				.variadic_parameter, .option, .result][i]
			return flat.source_type_composition_one(kind, markused_substitute_alias_generics(flat.source_type_syntax(clean).raw_base(), inferred))
		}
	}
	if shape.has(.map_type) {
		syntax := flat.source_type_syntax(clean)
		if syntax.bracket_closed() {
			return flat.source_type_composition_two(.map, markused_substitute_alias_generics(syntax.bracket_raw_inner(), inferred), markused_substitute_alias_generics(syntax.bracket_suffix(), inferred))
		}
	}
	return clean
}

fn (c &CallCollector) collect_generic_body_alias_operator_calls(fn_node &flat.Node, cur_module string, local_types flat.TypeTextBindings, mut calls []string) {
	mut stack := []flat.NodeId{cap: int(fn_node.children_count)}
	for i in 0 .. fn_node.children_count {
		child_id := c.a.child(fn_node, i)
		if int(child_id) >= 0 {
			stack << child_id
		}
	}
	for stack.len > 0 {
		child_id := stack.pop()
		child := c.a.node(child_id)
		match child.kind {
			.assign, .selector_assign, .index_assign {
				c.collect_assign_operator_call(child, cur_module, local_types, mut calls)
			}
			.infix {
				if child.children_count >= 2 {
					lhs_id := c.a.child(child, 0)
					if int(lhs_id) >= 0 {
						c.collect_struct_operator_call(lhs_id, child.op, cur_module, local_types, mut calls)
					}
				}
			}
			else {}
		}

		for i in 0 .. child.children_count {
			next_id := c.a.child(child, i)
			if int(next_id) >= 0 {
				stack << next_id
			}
		}
	}
}

fn (c &CallCollector) collect_top_level_selector_call(callee &flat.Node, method string, resolved_call string, cur_module string, imports map[string]string, local_values map[string]bool, local_types flat.TypeTextBindings, mut calls []string) {
	mut has_exact_selector_call := false
	if callee.children_count > 0 {
		base_id := c.a.child(callee, 0)
		if int(base_id) >= 0 {
			base := c.a.node(base_id)
			base_is_local_value := base.kind == .ident && base.value_text() in local_values
			if method == 'str'
				&& c.top_level_expr_lowers_to_map_str(base_id, cur_module, imports, local_values, local_types) {
				calls << 'string__plus'
			}
			if base_is_local_value {
				has_exact_selector_call = c.collect_top_level_typed_receiver_method(base_id, method, cur_module, imports, local_values, local_types, mut calls)
				if declared_type_source_id := local_types.type_id(base.value_text()) {
					c.collect_sum_variant_receiver_methods_for_type_name(declared_type_source_id, method, cur_module, mut calls)
				}
			}
			if !has_exact_selector_call && !base_is_local_value {
				has_exact_selector_call = c.collect_checker_selected_call(resolved_call, mut calls)
			}
			if !has_exact_selector_call {
				has_exact_selector_call = c.collect_top_level_typed_receiver_method(base_id, method, cur_module, imports, local_values, local_types, mut calls)
			}
			if !has_exact_selector_call {
				has_exact_selector_call = c.collect_sum_variant_receiver_methods(base_id, method, cur_module, mut calls)
			}
			if !has_exact_selector_call && base.kind == .ident && base.value_text() in imports
				&& base.value_text() !in local_values {
				calls << imports[base.value_text()] + '.' + method
				has_exact_selector_call = true
			}
			if !has_exact_selector_call && resolved_call.len > 0 {
				calls << resolved_call
				has_exact_selector_call = true
			}
			if !has_exact_selector_call {
				c.collect_top_level_selector_fallback(base, method, cur_module, imports, local_values, mut calls)
			}
		}
	}
}

fn (c &CallCollector) collect_checker_selected_call(resolved_call string, mut calls []string) bool {
	if resolved_call.len == 0 || markused_is_builtin_collection_resolved_call(resolved_call)
		|| !c.is_known_fn_name(resolved_call) {
		return false
	}
	c.add_typed_receiver_method_name(resolved_call, mut calls)
	return true
}

fn markused_is_builtin_collection_resolved_call(name string) bool {
	return name.len == 0 || markused_is_raw_collection_method_name(name, 'array.')
		|| name == 'array_clone' || markused_is_runtime_collection_helper_name(name)
		|| markused_is_raw_collection_method_name(name, 'map.')
}

fn markused_is_raw_collection_method_name(name string, prefix string) bool {
	if !name.starts_with(prefix) {
		return false
	}
	rest := name[prefix.len..]
	return rest.len > 0 && !rest.contains('.')
}

fn markused_is_runtime_collection_helper_name(name string) bool {
	return name in ['array__clone', 'array__reverse', 'array__reverse_in_place', 'array__prepend',
		'array__insert', 'array__push_many', 'array__needs_unique_shift', 'map__delete', 'map__move',
		'map__reserve', 'map__keys', 'map__values', 'map__clear', 'map__free', 'map__get',
		'map__get_check', 'map__get_key_check', 'map__exists', 'map__set']
}

fn (c &CallCollector) collect_top_level_selector_fallback(base &flat.Node, method string, cur_module string, imports map[string]string, local_values map[string]bool, mut calls []string) {
	if base.kind == .ident && base.value_text().len > 0 {
		if base.value_text() in local_values {
			return
		}
		if base.value_text() in imports {
			return
		}
		mod_name := base.value_text()
		calls << mod_name + '.' + method
		calls << qualify_fn(cur_module, base.value_text() + '.' + method)
		if base.value_text().len > 0 && base.value_text()[0] >= `A` && base.value_text()[0] <= `Z` {
			named_type := c.tc.parse_type(base.source_value_id())
			named_type_name_source_id_2 := resolve_type_name(named_type)
			named_type_name := flat.source_type_name(named_type_name_source_id_2)
			if named_type_name_source_id_2 != 0 {
				calls << named_type_name + '.' + method
			}
		}
	} else if base.kind == .selector && base.children_count > 0 {
		inner_id := c.a.child(base, 0)
		if int(inner_id) >= 0 {
			inner := c.a.node(inner_id)
			if inner.kind == .ident && inner.value_text().len > 0 {
				mod_name := if inner.value_text() in imports {
					imports[inner.value_text()]
				} else {
					inner.value_text()
				}
				calls << mod_name + '.' + base.value_text() + '.' + method
			}
		}
	}
}

fn (c &CallCollector) collect_lowered_join_path_single(call &flat.Node, resolved_call string, imports map[string]string, local_values map[string]bool, mut calls []string) {
	if call.children_count <= 2 { return }
	callee_id := c.a.child(call, 0)
	if int(callee_id) < 0 { return }
	callee := c.a.node(callee_id)
	if (callee.kind == .ident && local_values[callee.value])
		|| (callee.kind == .selector && c.selector_base_is_local(callee, local_values)) {
		return
	}
	if !markused_call_lowers_to_join_path_single(c.a, *callee, imports, resolved_call) {
		return
	}
	for i in 1 .. call.children_count {
		arg := c.a.child_node(call, i)
		if arg.kind == .prefix && arg.value == '...' { return }
	}
	calls << 'join_path_single'
	calls << 'os.join_path_single'
}

fn (c &CallCollector) collect_top_level_typed_receiver_method(base_id flat.NodeId, method string, cur_module string, imports map[string]string, local_values map[string]bool, local_types flat.TypeTextBindings, mut calls []string) bool {
	type_name_source_id_9 := c.top_level_receiver_type_name(base_id, cur_module, imports, local_values, local_types)
	if type_name_source_id_9 == 0 {
		return false
	}
	if method_name := c.typed_receiver_method_name(type_name_source_id_9, method, cur_module) {
		c.add_typed_receiver_method_name(method_name, mut calls)
		return true
	}
	return c.flag_enum_intrinsic_receiver_method(type_name_source_id_9, method, cur_module)
}

fn (c &CallCollector) top_level_receiver_type_name(base_id flat.NodeId, cur_module string, imports map[string]string, local_values map[string]bool, local_types flat.TypeTextBindings) u32 {
	base := c.a.node(base_id)
	if base.kind == .ident {
		if scoped_type_source_id := c.local_ident_types.type_id(int(base_id)) {
			return scoped_type_source_id
		}
		if type_name_source_id := c.checked_local_type_name(base_id) {
			type_name_source_id_23 := type_name_source_id

			return type_name_source_id_23
		}
	}
	type_name := if base.kind == .call {
		c.top_level_expr_type_name(base_id, cur_module, imports, local_values, local_types, false)
	} else {
		resolve_type_name(c.node_type(base_id))
	}
	if type_name != 0 {
		struct_type := c.struct_lookup_name(type_name, cur_module)
		if struct_type != 0 {
			return struct_type
		}
		return type_name
	}
	if base.kind == .ident && base.value.len > 0 {
		if local_type_source_id := local_types.type_id(base.value) {
			return local_type_source_id
		}
		if base.value in local_values {
			return u32(0)
		}
		if base.value in imports {
			return u32(0)
		}
	}
	if base.kind == .or_expr {
		or_type_name_source_id := c.top_level_or_expr_base_type_name(base_id, cur_module, imports, local_values, local_types)

		if or_type_name_source_id != 0 {
			return or_type_name_source_id
		}
	}
	// A field-access receiver (`app.best.load()`): resolve the base struct's field
	// type so a method on it (including one declared on a type alias) is marked used.
	// The checker keeps the declared field type name (e.g. the alias `HighScore`).
	if base.kind == .selector && base.value.len > 0 && base.children_count > 0 {
		inner_id := c.a.child(base, 0)
		if int(inner_id) >= 0 {
			inner_type_source_id := c.top_level_receiver_type_name(inner_id, cur_module, imports, local_values, local_types)
			if inner_type_source_id != 0 {
				struct_name := c.struct_lookup_name(inner_type_source_id, cur_module)
				lookup := if struct_name != 0 { struct_name } else { inner_type_source_id }
				if field_type_source_id := c.tc.struct_field_type_name(lookup, base.value) {
					if field_type_source_id != 0 {
						return field_type_source_id
					}
				}
			}
		}
	}
	if base.kind == .ident && base.value.len > 0 {
		return c.value_type_name(base.value, cur_module, imports)
	}
	if base.kind == .selector {
		name := c.qualified_expr_name(base_id)
		if name.len > 0 {
			return c.value_type_name(name, cur_module, imports)
		}
	}
	return u32(0)
}

fn (c &CallCollector) top_level_expr_type_name(id flat.NodeId, cur_module string, imports map[string]string, local_values map[string]bool, local_types flat.TypeTextBindings, unwrap_optional_result bool) u32 {
	node := c.a.node(id)
	if node.kind == .ident {
		if scoped_type_source_id := c.local_ident_types.type_id(int(id)) {
			return scoped_type_source_id
		}
		if type_name_source_id := c.checked_local_type_name(id) {
			type_name_source_id_24 := type_name_source_id

			return type_name_source_id_24
		}
		if local_type_source_id := local_types.type_id(node.value) {
			return local_type_source_id
		}
	}
	if node.kind == .index {
		if elem_type_source_id := c.top_level_index_elem_type_name(id, cur_module, imports, local_values, local_types) {
			return elem_type_source_id
		}
	}
	if alias_type := c.syntax_alias_expr_type(id, cur_module, imports) {
		return alias_type.source_type_id()
	}
	if node.kind == .call {
		if resolved := c.tc.resolved_call_name(id) {
			if c.generic_fn_name_is_known(resolved, cur_module) {
				specialized_source_id := c.top_level_call_return_type_name(id, cur_module, imports,
					local_values, local_types, unwrap_optional_result)

				if specialized_source_id != 0 {
					return specialized_source_id
				}
			}
		}
	}
	typ := c.node_type(id)
	if type_name_source_id := markused_type_name(typ, unwrap_optional_result) {
		type_name_source_id_25 := type_name_source_id

		return type_name_source_id_25
	}
	match node.kind {
		.call {
			return c.top_level_call_return_type_name(id, cur_module, imports, local_values, local_types, unwrap_optional_result)
		}
		.or_expr {
			return c.top_level_or_expr_base_type_name(id, cur_module, imports, local_values, local_types)
		}
		.ident {
			return local_types.type_id(node.value) or { u32(0) }
		}
		else {}
	}

	return u32(0)
}

// register_top_level_for_in_vars records the loop variable(s) of a top-level
// `for .. in ..` as locals and gives the value variable the container's element
// type, so a method call on it in the body is marked used.
fn (c &CallCollector) register_top_level_for_in_vars(node &flat.Node, cur_module string, imports map[string]string, mut local_values map[string]bool, mut local_types flat.TypeTextBindings) {
	if node.children_count < 3 {
		return
	}
	header := node.value.int()
	key_id := c.a.child(node, 0)
	val_id := c.a.child(node, 1)
	container_id := c.a.child(node, 2)
	// Checked loop bindings already supply the value type. Resolve the key
	// against the outer bindings before writing either loop variable; this
	// path needs no copies of the surrounding maps.
	key_binding := c.a.node(key_id)
	second_binding := c.a.node(val_id)
	second_is_named := int(val_id) >= 0 && second_binding.kind == .ident
		&& second_binding.value.len > 0
	cached_value_id := if second_is_named { val_id } else { key_id }
	cached_value_name := if second_is_named { second_binding.value } else { key_binding.value }
	if (second_is_named || key_binding.kind == .ident) && cached_value_name.len > 0
		&& cached_value_name != '_' {
		if cached := c.tc.expr_type(cached_value_id) {
			if !markused_type_has_unknown(cached) {
				if name_source_id := markused_type_name(cached, false) {
					if name_source_id != flat.source_type_generic {
						if second_is_named && key_binding.kind == .ident
							&& key_binding.value.len > 0 && key_binding.value != '_' {
							key_type_source_id := c.top_level_for_in_key_type_name(container_id, cur_module,
								imports, local_values, local_types)
							if !local_values[key_binding.value] {
								local_values[key_binding.value] = true
							}
							if key_type_source_id != 0 {
								local_types.set_id(key_binding.value, key_type_source_id)
							}
						}
						if !local_values[cached_value_name] {
							local_values[cached_value_name] = true
						}
						local_types.set_id(cached_value_name, name_source_id)
						return
					}
				}
			}
		}
	}
	container_values := local_values.clone()
	container_types := local_types.clone()
	key_node := c.a.node(key_id)
	has_second := int(val_id) >= 0 && c.a.node(val_id).kind == .ident
		&& c.a.node(val_id).value.len > 0
	mut value_var := ''
	if has_second {
		value_var = c.a.node(val_id).value
		if key_node.kind == .ident && key_node.value.len > 0 && key_node.value != '_' {
			if !local_values[key_node.value] { local_values[key_node.value] = true }
			key_type_source_id_2 := c.top_level_for_in_key_type_name(container_id, cur_module, imports,
				container_values, container_types)
			if key_type_source_id_2 != 0 {
				local_types.set_id(key_node.value, key_type_source_id_2)
			}
		}
	} else if key_node.kind == .ident && key_node.value.len > 0 {
		value_var = key_node.value
	}
	if value_var.len == 0 || value_var == '_' {
		return
	}
	if !local_values[value_var] { local_values[value_var] = true }
	value_id := if has_second { val_id } else { key_id }
	if cached := c.tc.expr_type(value_id) {
		if !markused_type_has_unknown(cached) {
			if name_source_id := markused_type_name(cached, false) {
				if name_source_id != flat.source_type_generic {
					local_types.set_id(value_var, name_source_id)
					return
				}
			}
		}
	}
	container := c.a.node(container_id)
	if header == 4 || container.kind == .range {
		cached := c.node_type(key_id)
		if types.unalias_type(cached).is_integer() {
			local_types.set_id(value_var, cached.source_type_id())
			return
		}
		low := if header == 4 { container_id } else { c.a.child(container, 0) }
		high := if header == 4 { c.a.child(node, 3) } else { c.a.child(container, 1) }
		low_literal := c.range_endpoint_is_literal(low, cur_module, imports, container_types,
			0)
		candidates := if low_literal { [high, low] } else { [low, high] }
		local_types.set_id(value_var, flat.source_type_int)
		for candidate in candidates {
			type_name_source_id_10 := c.top_level_expr_type_name(candidate, cur_module, imports,
				container_values, container_types, false)

			if types.unalias_type(c.tc.parse_canonical_type(type_name_source_id_10)).is_integer() {
				local_types.set_id(value_var, type_name_source_id_10)
				break
			}
		}
		return
	}
	container_is_ref := container.kind == .prefix && container.op == .amp
	iterable_id := if container_is_ref && container.children_count > 0 {
		c.a.child(container, 0)
	} else {
		container_id
	}
	container_type_source_id := c.top_level_expr_type_name(iterable_id, cur_module, imports,
		container_values, container_types, false)
	if elem_source_id := c.top_level_for_in_elem_type_name(iterable_id, cur_module, imports,
		container_values, container_types) {
		if elem_source_id != 0 {
			mut value_type := elem_source_id
			collection := types.unalias_type(types.unwrap_pointer(c.tc.parse_canonical_type(container_type_source_id)))
			if (node.op == .amp || container_is_ref || flat.source_type_shape(container_type_source_id).has(.pointer))
				&& (collection is types.Array || collection is types.ArrayFixed || collection is types.Map) {
				clean_elem := types.unalias_type(c.tc.parse_canonical_type(elem_source_id))
				if clean_elem !is types.Pointer && clean_elem !is types.OptionType {
					value_type = flat.source_type_composition_one(.pointer, elem_source_id)
				}
			}
			local_types.set_id(value_var, value_type)
		}
		return
	}
	if container_type_source_id == 0 {
		return
	}
	if info := c.tc.iterator_for_in_next_call_info_text(container_type_source_id) {
		return_text_source_id := c.generic_factory_return_type_text(info.name, cur_module, false)

		specialized := c.tc.specialize_generic_factory_return(return_text_source_id, info.name, container_type_source_id, []u32{})
		if flat.source_type_shape(specialized).has(.option_type) {
			local_types.set_id(value_var, flat.source_type_syntax(specialized).raw_base())
			return
		}
		if info.return_type is types.OptionType {
			if elem_source_id := markused_type_name(info.return_type.base_type, false) {
				local_types.set_id(value_var, elem_source_id)
			}
		}
	}
}

fn (c &CallCollector) range_endpoint_is_literal(id flat.NodeId, cur_module string, imports map[string]string, local_types flat.TypeTextBindings, depth int) bool {
	if int(id) < 0 || depth >= 32 { return false }
	node := c.a.node(id)
	if node.kind in [.int_literal, .float_literal, .char_literal] { return true }
	if node.kind == .ident && !local_types.contains(node.value) {
		for candidate in c.value_name_candidates(node.value, cur_module, imports) {
			if expr_id := c.tc.const_exprs[candidate] {
				return c.range_endpoint_is_literal(expr_id, cur_module, imports, local_types, depth + 1)
			}
		}
	}
	return false
}

fn (c &CallCollector) top_level_for_in_key_type_name(container_id flat.NodeId, cur_module string, imports map[string]string, local_values map[string]bool, local_types flat.TypeTextBindings) u32 {
	type_name_id := markused_unwrapped_source_type(c.top_level_expr_type_name(container_id, cur_module, imports, local_values,
		local_types, true))
	if flat.source_type_shape(type_name_id).has(.map_type) {
		syntax := flat.source_type_syntax(type_name_id)
		if syntax.bracket_closed() { return syntax.bracket_raw_inner() }
	}
	container := types.unwrap_pointer(c.tc.parse_canonical_type(type_name_id))
	if container is types.Map {
		return container.key_type.source_type_id()
	}
	return flat.source_type_int
}

fn (c &CallCollector) top_level_for_in_elem_type_name(container_id flat.NodeId, cur_module string, imports map[string]string, local_values map[string]bool, local_types flat.TypeTextBindings) ?u32 {
	type_name_id := markused_unwrapped_source_type(c.top_level_expr_type_name(container_id, cur_module, imports, local_values, local_types, true))
	if type_name_id == 0 {
		return none
	}
	if type_name_id == flat.source_type_string { return flat.source_type_u8 }
	shape := flat.source_type_shape(type_name_id)
	if shape.has(.array_type) { return flat.source_type_syntax(type_name_id).raw_base() }
	if shape.has(.map_type) || shape.has(.leading_bracket) {
		syntax := flat.source_type_syntax(type_name_id)
		if syntax.bracket_closed() && syntax.bracket_suffix() != 0 {
			return syntax.bracket_suffix()
		}
	}
	ct := types.unwrap_pointer(c.tc.parse_canonical_type(type_name_id))
	if ct is types.Array {
		return ct.elem_type.source_type_id()
	}
	if ct is types.ArrayFixed {
		return ct.elem_type.source_type_id()
	}
	if ct is types.Map {
		return ct.value_type.source_type_id()
	}
	return none
}

fn (c &CallCollector) top_level_index_elem_type_name(id flat.NodeId, cur_module string, imports map[string]string, local_values map[string]bool, local_types flat.TypeTextBindings) ?u32 {
	node := c.a.node(id)
	if node.children_count == 0 {
		return none
	}
	base_type_name_source_id := c.top_level_expr_type_name(c.a.child(node, 0), cur_module, imports, local_values, local_types, false)

	if base_type_name_source_id == 0 {
		return none
	}
	base_type := types.unwrap_pointer(c.tc.parse_canonical_type(base_type_name_source_id))
	if base_type is types.Array {
		return base_type.elem_type.source_type_id()
	}
	if base_type is types.ArrayFixed {
		return base_type.elem_type.source_type_id()
	}
	if base_type is types.Map {
		return base_type.value_type.source_type_id()
	}
	return none
}

fn (c &CallCollector) top_level_call_return_type_name(call_id flat.NodeId, cur_module string, imports map[string]string, local_values map[string]bool, local_types flat.TypeTextBindings, unwrap_optional_result bool) u32 {
	call := c.a.node(call_id)
	if call.kind != .call || call.children_count == 0 {
		return u32(0)
	}
	callee_id := c.a.child(call, 0)
	if int(callee_id) < 0 {
		return u32(0)
	}
	mut callee := c.a.node(callee_id)
	if callee.kind == .index && callee.value != 'range' && callee.children_count > 0 {
		mut receiver_type := u32(0)
		factory := c.a.child_node(callee, 0)
		if factory.kind == .selector && factory.children_count > 0 {
			receiver_id := c.a.child(factory, 0)
			raw_receiver_source_id := c.top_level_receiver_type_name(receiver_id, cur_module, imports, local_values, local_types)

			receiver_type = c.generic_factory_qualified_type_text(raw_receiver_source_id, receiver_id, cur_module, imports)
		}
		if resolved := c.tc.resolved_call_name(call_id) {
			if c.generic_fn_name_is_known(resolved, cur_module) {
				return_type_source_id := c.generic_factory_return_type_name(callee, resolved, cur_module, imports, unwrap_optional_result, receiver_type)

				if return_type_source_id != 0 {
					return return_type_source_id
				}
			}
		}
		base_id := c.a.child(callee, 0)
		name := c.qualified_expr_name(base_id)
		resolved := markused_resolve_imported_name(name, imports)
		mut root_id := base_id
		for c.a.node(root_id).kind == .selector && c.a.node(root_id).children_count > 0 {
			root_id = c.a.child(c.a.node(root_id), 0)
		}
		shadowed := if c.has_local_ident_visibility {
			markused_ident_is_visible_local(root_id, name.all_before('.'), local_values, c.local_ident_visibility)
		} else {
			name.all_before('.') in local_values
		}
		if !shadowed && c.generic_fn_name_is_known(resolved, cur_module) {
			return c.generic_factory_return_type_name(callee, resolved, cur_module, imports, unwrap_optional_result, receiver_type)
		}
	}
	if callee.kind in [.ident, .selector] {
		if resolved := c.tc.resolved_call_name(call_id) {
			if c.generic_fn_name_is_known(resolved, cur_module) {
				if inferred_source_id := c.inferred_generic_factory_return_type_name(call_id, call, resolved,
					cur_module, imports, local_values, local_types, unwrap_optional_result) {
					return inferred_source_id
				}
			}
		}
	}
	if callee.kind == .selector && callee.value.len > 0 && callee.children_count > 0 {
		base_id := c.a.child(callee, 0)
		base := c.a.node(base_id)
		type_name_source_id_26 := c.top_level_receiver_type_name(base_id, cur_module, imports, local_values, local_types)
		if type_name_source_id_26 != 0 {
			if method_name := c.typed_receiver_method_name(type_name_source_id_26, callee.value, cur_module) {
				return c.fn_return_type_name(method_name, unwrap_optional_result)
			}
		}
		if base.kind == .ident && base.value in imports {
			return c.fn_return_type_name('${imports[base.value]}.${callee.value}', unwrap_optional_result)
		}
	}
	if callee.kind == .ident && callee.value.len > 0 {
		name := qualify_fn(cur_module, callee.value)
		type_name_source_id_27 := c.fn_return_type_name(name, unwrap_optional_result)
		if type_name_source_id_27 != 0 {
			return type_name_source_id_27
		}
		return c.fn_return_type_name(callee.value, unwrap_optional_result)
	}
	return u32(0)
}

fn (c &CallCollector) generic_factory_return_type_name(index &flat.Node, name string, cur_module string, imports map[string]string, unwrap_optional_result bool, receiver_type_id u32) u32 {
	for candidate in markused_fn_signature_name_candidates(name, cur_module) {
		if candidate !in c.tc.fn_generic_params
			&& !c.generic_receiver_method_name_is_known(candidate) {
			continue
		}
		return_type_source_id_2 := c.generic_factory_return_type_text(candidate, cur_module,
			unwrap_optional_result)

		if return_type_source_id_2 == 0 { continue }
		arg_count := int(index.children_count) - 1
		if arg_count <= 0 { return return_type_source_id_2 }
		mut args := []u32{cap: arg_count}
		for i in 0 .. arg_count {
			arg_id := c.a.child(index, i + 1)
			arg_source_id := c.generic_factory_type_arg(arg_id)

			if arg_source_id == 0 { return return_type_source_id_2 }
			args << c.generic_factory_qualified_type_text(arg_source_id, arg_id, cur_module, imports)
		}
		return c.tc.specialize_generic_factory_return(return_type_source_id_2, candidate, receiver_type_id, args)
	}
	return u32(0)
}

fn (c &CallCollector) inferred_generic_factory_return_type_name(call_id flat.NodeId, call &flat.Node, name string, cur_module string, imports map[string]string, local_values map[string]bool, local_types flat.TypeTextBindings, unwrap_optional_result bool) ?u32 {
	mut receiver_type := u32(0)
	callee := c.a.child_node(call, 0)
	if callee.kind == .selector && callee.children_count > 0 {
		receiver_id := c.a.child(callee, 0)
		raw_receiver_source_id := c.top_level_receiver_type_name(receiver_id, cur_module, imports,
			local_values, local_types)

		receiver_type = c.generic_factory_qualified_type_text(raw_receiver_source_id, receiver_id,
			cur_module, imports)
	}
	for candidate in markused_fn_signature_name_candidates(name, cur_module) {
		generic_params := c.tc.fn_generic_params[candidate] or { []u32{} }
		if generic_params.len == 0 {
			if receiver_type != 0 && c.generic_receiver_method_name_is_known(candidate) {
				return_type_source_id_3 := c.generic_factory_return_type_text(candidate, cur_module,
					unwrap_optional_result)

				if return_type_source_id_3 != 0 {
					return c.tc.specialize_generic_factory_return(return_type_source_id_3, candidate,
						receiver_type, []u32{})
				}
			}
			continue
		}
		mut param_texts := []u32{}
		if known := c.tc.fn_param_type_texts.type_ids(candidate) {
			param_texts = known.clone()
		}
		if param_texts.len == 0 {
			if decl := c.fn_decls[candidate] {
				param_texts = c.generic_fn_param_type_texts(candidate, c.a.node(decl.node_id))
			}
		}
		if param_texts.len == 0 {
			continue
		}
		inferred := c.infer_alias_generic_args(call, candidate, param_texts, generic_params, cur_module,
			imports, local_values, local_types)
		mut args := []u32{cap: generic_params.len}
		mut complete := true
		for param in generic_params {
			if actual_source_id := inferred[param] {
				args << c.generic_factory_qualified_type_text(actual_source_id, call_id, cur_module,
					imports)
			} else {
				complete = false
				break
			}
		}
		if !complete {
			continue
		}
		return_type_source_id_4 := c.generic_factory_return_type_text(candidate, cur_module,
			unwrap_optional_result)

		if return_type_source_id_4 == 0 {
			continue
		}
		return c.tc.specialize_generic_factory_return(return_type_source_id_4, candidate, receiver_type,
			args)
	}
	return none
}

fn (c &CallCollector) generic_factory_return_type_text(candidate string, cur_module string, unwrap_optional_result bool) u32 {
	mut return_type := c.fn_return_type_name(candidate, unwrap_optional_result)
	semantic_has_placeholder := if semantic_type := c.fn_return_type_for_name(candidate) {
		if return_type == 0 && semantic_type is types.MultiReturn {
			return_type = semantic_type.source_type_id()
		}
		markused_type_has_unknown(semantic_type)
	} else {
		false
	}
	if return_type == 0 || semantic_has_placeholder {
		if signature_text_source_id := c.tc.fn_ret_type_texts.type_id(candidate) {
			return_type = c.generic_factory_signature_type_text(signature_text_source_id, candidate,
				cur_module)
			if unwrap_optional_result {
				if flat.source_type_shape(return_type).has(.optional) {
					return_type = flat.source_type_syntax(return_type).raw_base()
				}
			}
		}
	}
	return return_type
}

fn (c &CallCollector) generic_factory_signature_type_text(signature_text_id u32, candidate string, cur_module string) u32 {
	mut decl_module := c.tc.fn_type_modules[candidate] or { cur_module }
	mut decl_id := flat.empty_node
	mut decl_imports := map[string]string{}
	if decl := c.fn_decls[candidate] {
		decl_module = decl.module
		decl_id = decl.node_id
		if c.import_contexts.len > 0 {
			decl_imports = c.imports(decl.import_context)
		}
	} else if decl_module == cur_module && candidate.contains('.') {
		prefix := candidate.all_before('.')
		if !prefix.contains('[') {
			decl_module = prefix
		}
	}
	return c.generic_factory_qualified_type_text(signature_text_id, decl_id, decl_module,
		decl_imports)
}

fn markused_type_has_unknown(typ types.Type) bool {
	return match typ {
		types.Unknown { true }
		types.Array, types.ArrayFixed, types.Channel { markused_type_has_unknown(typ.elem_type) }
		types.Pointer, types.OptionType, types.ResultType, types.Alias {
			markused_type_has_unknown(typ.base_type)
		}
		types.Map {
			markused_type_has_unknown(typ.key_type) || markused_type_has_unknown(typ.value_type)
		}
		types.FnType {
			mut found := markused_type_has_unknown(typ.return_type)
			for parameter in typ.params {
				param := parameter.typ
				found = found || markused_type_has_unknown(param)
			}
			found
		}
		types.MultiReturn {
			mut found := false
			for part in typ.types {
				found = found || markused_type_has_unknown(part)
			}
			found
		}
		else { false }
	}
}

fn (c &CallCollector) generic_factory_type_arg(id flat.NodeId) u32 {
	if int(id) < 0 { return u32(0) }
	node := c.a.node(id)
	match node.kind {
		.ident { return node.source_value_id() }
		.selector { return flat.source_type_id(c.qualified_expr_name(id)) }
		.index {
			if node.children_count < 2 || node.value == 'range' { return u32(0) }
			base := c.generic_factory_type_arg(c.a.child(node, 0))
			mut args := [base]
			for i in 1 .. node.children_count {
				arg_source_id_2 := c.generic_factory_type_arg(c.a.child(node, i))
				if arg_source_id_2 == 0 { return u32(0) }
				args << arg_source_id_2
			}
			return flat.source_type_composition(.generic, args)
		}
		.array_init {
			return if node.typ != 0 {
				node.typ
			} else if flat.source_type_shape(node.source_value_id()).has(.leading_bracket) {
				node.source_value_id()
			} else {
				flat.source_type_composition_one(.array, node.source_value_id())
			}
		}
		.map_init, .struct_init, .struct_decl { return node.source_value_id() }
		.prefix {
			if node.children_count == 0 { return u32(0) }
			inner_source_id := c.generic_factory_type_arg(c.a.child(node, 0))
			return if node.op == .amp {
				flat.source_type_composition_one(.pointer, inner_source_id)
			} else {
				inner_source_id
			}
		}
		else { return u32(0) }
	}
}

fn (c &CallCollector) top_level_or_expr_base_type_name(id flat.NodeId, cur_module string, imports map[string]string, local_values map[string]bool, local_types flat.TypeTextBindings) u32 {
	node := c.a.node(id)
	if node.kind != .or_expr || node.children_count == 0 {
		return u32(0)
	}
	return c.top_level_expr_type_name(c.a.child(node, 0), cur_module, imports, local_values, local_types, true)
}

fn (c &CallCollector) fn_return_type_name(name string, unwrap_optional_result bool) u32 {
	if typ := c.tc.fn_ret_types[name] {
		return markused_type_name_or_empty(typ, unwrap_optional_result)
	}
	lowered := markused_c_name(name)
	if lowered != name {
		if typ := c.tc.fn_ret_types[lowered] {
			return markused_type_name_or_empty(typ, unwrap_optional_result)
		}
	}
	return u32(0)
}

fn (c &CallCollector) fn_return_type_for_name(name string) ?types.Type {
	if typ := c.tc.fn_ret_types[name] {
		return typ
	}
	lowered := markused_c_name(name)
	if lowered != name {
		if typ := c.tc.fn_ret_types[lowered] {
			return typ
		}
	}
	return none
}

fn (c &CallCollector) fn_param_types_for_name(name string) ?[]types.Type {
	if params := c.tc.fn_param_types[name] {
		return params
	}
	lowered := markused_c_name(name)
	if lowered != name {
		if params := c.tc.fn_param_types[lowered] {
			return params
		}
	}
	return none
}

fn markused_index_overload_compound_type_is_string(typ types.Type) bool {
	clean := if typ is types.Alias { *typ.base_type } else { typ }
	return clean is types.String
}

// collect_struct_operator_call updates collect struct operator call state for markused.
fn (c &CallCollector) collect_struct_operator_call(lhs_id flat.NodeId, op flat.Op, cur_module string, local_types flat.TypeTextBindings, mut calls []string) {
	// Logical/bitwise operators cannot dispatch to a struct overload. Avoid
	// resolving their operand types merely to discover that after the lookup.
	op_name := markused_struct_operator_symbol(op) or { return }
	mut possible := c.has_receiver_method(op_name) || c.fn_suffixes[op_name]
	if !possible {
		match op {
			.gt, .ge, .le { possible = c.has_receiver_method('<') || c.fn_suffixes['<'] }
			.ne { possible = c.has_receiver_method('==') || c.fn_suffixes['=='] }
			else {}
		}
	}
	if !possible {
		return
	}
	lhs_type := c.operator_lhs_type(lhs_id, local_types)
	c.collect_struct_operator_call_for_type(lhs_type, op, cur_module, mut calls)
}

fn (c &CallCollector) collect_struct_operator_call_for_type(lhs_type types.Type, op flat.Op, cur_module string, mut calls []string) {
	for receiver_source_id_2 in c.struct_operator_receivers_for_call(lhs_type, cur_module) {
		method_name := c.struct_operator_call_name(receiver_source_id_2, op) or { continue }
		c.add_operator_call_name(method_name, mut calls)
		return
	}
}

fn (c &CallCollector) collect_index_compound_update_helpers(node &flat.Node, cur_module string, local_types flat.TypeTextBindings, mut calls []string) {
	op := markused_assign_operator_symbol(node.op) or { return }
	if node.children_count < 2 {
		return
	}
	lhs_id := c.a.child(node, 0)
	lhs_type := c.index_compound_value_type(lhs_id, local_types)
	if op == .plus && markused_type_is_string_like(lhs_type) {
		calls << 'string__plus'
	}
	c.collect_struct_operator_call_for_type(lhs_type, op, cur_module, mut calls)
}

fn (c &CallCollector) index_compound_value_type(lhs_id flat.NodeId, local_types flat.TypeTextBindings) types.Type {
	if int(lhs_id) < 0 {
		return types.Type(types.void_)
	}
	lhs := c.a.node(lhs_id)
	if lhs.kind == .index && lhs.children_count >= 1 {
		base_id := c.a.child(lhs, 0)
		base_type := c.node_type(base_id)
		if getter := c.tc.index_operator_call_info(base_type, '[]') {
			return getter.return_type
		}
	}
	return c.operator_lhs_type(lhs_id, local_types)
}

fn markused_type_is_string_like(typ types.Type) bool {
	if typ is types.String {
		return true
	}
	if typ is types.Alias {
		return markused_type_is_string_like(typ.base_type)
	}
	return false
}

// collect_assign_operator_call adds operator overloads used through assignment operators.
fn (c &CallCollector) collect_assign_operator_call(node &flat.Node, cur_module string, local_types flat.TypeTextBindings, mut calls []string) {
	if node.kind == .index_assign {
		c.collect_index_overload_assignment_methods(node, cur_module, local_types, mut calls)
	}
	op := markused_assign_operator_symbol(node.op) or { return }
	mut i := 0
	for i + 1 < node.children_count {
		lhs_id := c.a.child(node, i)
		if int(lhs_id) >= 0 {
			c.collect_struct_operator_call(lhs_id, op, cur_module, local_types, mut calls)
		}
		i += 2
	}
}

fn (c &CallCollector) collect_index_overload_assignment_methods(node &flat.Node, cur_module string, local_types flat.TypeTextBindings, mut calls []string) {
	if !c.has_receiver_method('[]=') && (node.op == .assign || !c.has_receiver_method('[]')) {
		return
	}
	if node.children_count < 2 {
		return
	}
	lhs_id := c.a.child(node, 0)
	if int(lhs_id) < 0 {
		return
	}
	lhs := c.a.node(lhs_id)
	if lhs.kind != .index || lhs.children_count == 0 {
		return
	}
	base_id := c.a.child(lhs, 0)
	base_type := c.operator_lhs_type(base_id, local_types)
	for receiver_source_id_3 in c.struct_operator_receivers_for_call(base_type, cur_module) {
		c.collect_index_overload_method(receiver_source_id_3, '[]=', cur_module, mut calls)
		if node.op != .assign {
			c.collect_index_overload_method(receiver_source_id_3, '[]', cur_module, mut calls)
			c.collect_index_overload_compound_helpers(receiver_source_id_3, node.op, cur_module, mut calls)
		}
	}
	if c.detect_generics {
		if receiver_source_id := c.generic_index_overload_receiver(base_type, cur_module) {
			c.collect_index_overload_method(receiver_source_id, '[]=', cur_module, mut calls)
			if node.op != .assign {
				c.collect_index_overload_method(receiver_source_id, '[]', cur_module, mut calls)
				c.collect_index_overload_compound_helpers(receiver_source_id, node.op, cur_module, mut calls)
			}
		}
	}
}

fn (c &CallCollector) collect_index_overload_compound_helpers(receiver u32, op flat.Op, cur_module string, mut calls []string) {
	compound_op := markused_assign_operator_symbol(op) or { return }
	setter_name := c.index_overload_method_name(receiver, '[]=', cur_module) or { return }
	getter_name := c.index_overload_method_name(receiver, '[]', cur_module) or { return }
	getter_return := c.fn_return_type_for_name(getter_name) or { return }
	setter_params := c.fn_param_types_for_name(setter_name) or { return }
	if compound_op == .plus && (markused_index_overload_compound_type_is_string(getter_return)
		|| (setter_params.len > 2
			&& markused_index_overload_compound_type_is_string(setter_params[2]))) {
		calls << 'string__plus'
		return
	}
	if setter_params.len > 2 {
		c.collect_struct_operator_call_for_type(getter_return, compound_op, cur_module, mut calls)
	}
}

fn (c &CallCollector) collect_index_overload_getter_method(node &flat.Node, cur_module string, local_types flat.TypeTextBindings, mut calls []string) {
	if !c.has_receiver_method('[]') {
		return
	}
	if node.children_count == 0 {
		return
	}
	base_id := c.a.child(node, 0)
	base_type := c.operator_lhs_type(base_id, local_types)
	for receiver_source_id_4 in c.struct_operator_receivers_for_call(base_type, cur_module) {
		c.collect_index_overload_method(receiver_source_id_4, '[]', cur_module, mut calls)
	}
	if c.detect_generics {
		if receiver_source_id := c.generic_index_overload_receiver(base_type, cur_module) {
			c.collect_index_overload_method(receiver_source_id, '[]', cur_module, mut calls)
		}
	}
}

fn (c &CallCollector) collect_index_overload_method(receiver_id u32, method string, cur_module string, mut calls []string) {
	if method_name := c.index_overload_method_name(receiver_id, method, cur_module) {
		c.add_typed_receiver_method_name(method_name, mut calls)
	}
}

fn (c &CallCollector) index_overload_method_name(receiver_id u32, method string, cur_module string) ?string {
	if method_name := c.typed_receiver_method_name(receiver_id, method, cur_module) {
		return method_name
	}
	if c.detect_generics && c.receiver_is_generic_struct_application(receiver_id, cur_module) {
		return '${flat.source_type_name(receiver_id)}.${method}'
	}
	return none
}

fn (c &CallCollector) is_generic_index_overload_call(name string, cur_module string) bool {
	method := name.all_after_last('.')
	if method !in ['[]', '[]='] {
		return false
	}
	receiver := name.all_before_last('.')
	return c.receiver_is_generic_struct_application(flat.source_type_id(receiver), cur_module)
}

fn (c &CallCollector) generic_index_overload_receiver(typ types.Type, cur_module string) ?u32 {
	clean := types.unwrap_pointer(typ)
	receiver_source_id := resolve_type_name(clean)

	if c.receiver_is_generic_struct_application(receiver_source_id, cur_module) {
		return receiver_source_id
	}
	return none
}

fn (c &CallCollector) receiver_is_generic_struct_application(receiver_id u32, cur_module string) bool {
	clean := markused_clean_receiver_type_name(receiver_id)
	shape := flat.source_type_shape(clean)
	if !shape.has(.bracketed) || shape.has(.leading_bracket) || !flat.source_type_query(clean).has(.bracket_suffix) {
		return false
	}
	base := flat.source_type_syntax(clean).generic_base()
	if base in c.tc.struct_generic_params { return true }
	short_base := flat.source_type_syntax(base).unqualified()
	if short_base != base && short_base in c.tc.struct_generic_params {
		return true
	}
	if !flat.source_type_shape(base).has(.dotted) && cur_module.len > 0 && cur_module != 'main' && cur_module != 'builtin' {
		return flat.source_type_composition_two(.qualified, flat.source_type_id(cur_module), base) in c.tc.struct_generic_params
	}
	return false
}

fn (c &CallCollector) operator_lhs_type(lhs_id flat.NodeId, local_types flat.TypeTextBindings) types.Type {
	if int(lhs_id) >= 0 {
		lhs := c.a.node(lhs_id)
		if lhs.kind == .ident {
			if lhs.value in c.checked_local_names && lhs.value !in c.checked_sum_types {
				if recorded := c.tc.expr_type(lhs_id) {
					return recorded
				}
			}
			if local_type_source_id := local_types.type_id(lhs.value) {
				typ := c.tc.parse_canonical_type(local_type_source_id)
				if typ !is types.Unknown && typ !is types.Void {
					return typ
				}
			}
		}
		if lhs.kind == .index && lhs.children_count > 0 {
			base_type := types.unwrap_pointer(c.operator_lhs_type(c.a.child(lhs, 0), local_types))
			return match base_type {
				types.Array { *base_type.elem_type }
				types.ArrayFixed { *base_type.elem_type }
				types.Map { *base_type.value_type }
				else { base_type }
			}
		}
	}
	return c.node_type(lhs_id)
}

fn (c &CallCollector) struct_operator_receivers_for_call(lhs_type types.Type, cur_module string) []u32 {
	clean := types.unwrap_pointer(lhs_type)
	// Primitive scalar types have no struct declarations. String stays on the
	// lookup path because builtin.string declares its own operators.
	match clean {
		types.Primitive, types.Char, types.Rune, types.ISize, types.USize, types.Void,
		types.Nil, types.None {
			return []u32{}
		}
		else {}
	}
	lhs_name_source_id := resolve_type_name(clean)
	mut receivers := []u32{}
	if clean is types.Alias {
		markused_add_operator_receiver_candidates(lhs_name_source_id, cur_module, mut receivers)
		return receivers
	}
	struct_type := c.struct_lookup_name(lhs_name_source_id, cur_module)
	if struct_type != 0 {
		receivers << struct_type
	}
	return receivers
}

fn markused_add_operator_receiver_candidates(type_name_id u32, cur_module string, mut receivers []u32) {
	if type_name_id == 0 {
		return
	}
	receivers << type_name_id
	if !flat.source_type_shape(type_name_id).has(.dotted) && cur_module.len > 0 && cur_module != 'main'
		&& cur_module != 'builtin' {
		receivers << markused_qualify_source_type(cur_module, type_name_id)
	}
}

fn markused_alias_base_matches_type(tc &types.TypeChecker, alias_type types.Alias, typ types.Type) bool {
	base := types.unwrap_pointer(alias_type.base_type)
	clean := types.unwrap_pointer(typ)
	if base.source_type_id() == clean.source_type_id()
		|| flat.source_type_syntax(base.source_type_id()).unqualified() == flat.source_type_syntax(clean.source_type_id()).unqualified() {
		return true
	}
	return tc.c_type(base) == tc.c_type(clean)
}

// struct_operator_call_name supports struct operator call name handling for CallCollector.
fn (c &CallCollector) struct_operator_call_name(struct_type_id u32, op flat.Op) ?string {
	if op_name := markused_struct_operator_symbol(op) {
		if method_name := c.struct_operator_fn_name(struct_type_id, op_name) {
			return method_name
		}
	}
	match op {
		.gt, .ge, .le {
			if method_name := c.struct_operator_fn_name(struct_type_id, '<') {
				return method_name
			}
		}
		.ne {
			if method_name := c.struct_operator_fn_name(struct_type_id, '==') {
				return method_name
			}
		}
		else {}
	}

	return none
}

// markused_assign_operator_symbol maps assignment operators to their binary operator.
fn markused_assign_operator_symbol(op flat.Op) ?flat.Op {
	match op {
		.plus_assign {
			return .plus
		}
		.minus_assign {
			return .minus
		}
		.mul_assign {
			return .mul
		}
		.power_assign {
			return .power
		}
		.div_assign {
			return .div
		}
		.mod_assign {
			return .mod
		}
		else {}
	}

	return none
}

// markused_struct_operator_symbol supports markused struct operator symbol handling for markused.
fn markused_struct_operator_symbol(op flat.Op) ?string {
	match op {
		.plus {
			return '+'
		}
		.minus {
			return '-'
		}
		.mul {
			return '*'
		}
		.power {
			return '**'
		}
		.div {
			return '/'
		}
		.mod {
			return '%'
		}
		.eq {
			return '=='
		}
		.ne {
			return '!='
		}
		.lt {
			return '<'
		}
		.gt {
			return '>'
		}
		.le {
			return '<='
		}
		.ge {
			return '>='
		}
		else {}
	}

	return none
}

// struct_operator_fn_name supports struct operator fn name handling for CallCollector.
fn (c &CallCollector) struct_operator_fn_name(struct_type_id u32, op_name string) ?string {
	struct_type := flat.source_type_name(struct_type_id)
	method_name := '${struct_type}.${op_name}'
	if c.is_known_fn_name(method_name) {
		return method_name
	}
	cmethod_name := markused_c_name(method_name)
	if c.is_known_fn_name(cmethod_name) {
		return cmethod_name
	}
	return none
}

// is_known_fn_name reports whether is known fn name applies in markused.
fn (c &CallCollector) is_known_fn_name(name string) bool {
	return name in c.fn_decls || name in c.tc.fn_ret_types || name in c.tc.fn_param_types
}

// struct_lookup_name supports struct lookup name handling for CallCollector.
fn (c &CallCollector) struct_lookup_name(type_name_id u32, cur_module string) u32 {
	if type_name_id == 0 { return u32(0) }
	if flat.source_type_shape(type_name_id).has(.dotted) {
		if type_name_id in c.struct_decls { return type_name_id }
		syntax := flat.source_type_syntax(type_name_id)
		type_mod := flat.source_type_name(syntax.qualifier())
		if info := c.struct_decls[syntax.unqualified()] {
			if info.module == type_mod { return syntax.unqualified() }
		}
		if info := c.struct_decls[type_name_id] {
			if info.module == type_mod { return type_name_id }
		}
		return u32(0)
	}
	if info := c.struct_decls[type_name_id] {
		if info.module.len == 0 || info.module == cur_module { return type_name_id }
	}
	if cur_module.len > 0 && cur_module != 'main' && cur_module != 'builtin' {
		qualified := flat.source_type_composition_two(.qualified, flat.source_type_id(cur_module), type_name_id)
		if qualified in c.struct_decls { return qualified }
	}
	return u32(0)
}

// add_operator_call_name updates add operator call name state for CallCollector.
fn (c &CallCollector) add_operator_call_name(method_name string, mut calls []string) {
	calls << method_name
	lowered := markused_c_name(method_name)
	if lowered != method_name {
		calls << lowered
	}
}

// qualified_expr_name supports qualified expr name handling for CallCollector.
fn (c &CallCollector) qualified_expr_name(id flat.NodeId) string {
	if int(id) < 0 {
		return ''
	}
	node := c.a.nodes[int(id)]
	match node.kind {
		.ident {
			return node.value_text()
		}
		.selector {
			if node.children_count == 0 {
				return node.value_text()
			}
			base := c.qualified_expr_name(c.a.child(&node, 0))
			if base.len == 0 {
				return node.value_text()
			}
			return base + '.' + node.value_text()
		}
		else {
			return ''
		}
	}
}

// collect_fn_value_ident updates collect fn value ident state for markused.
fn (c &CallCollector) collect_fn_value_ident(id flat.NodeId, name string, cur_module string, imports map[string]string, is_local_value bool, mut calls []string) {
	if is_local_value {
		return
	}
	if resolved := c.tc.resolved_fn_value_name(id) {
		calls << resolved
		return
	}
	if name.len == 0 || !c.name_may_reference_fn(name, cur_module, imports) {
		return
	}
	has_fn_decl := c.name_has_fn_decl(name, cur_module, imports)
	if !has_fn_decl && !c.node_is_fn_value(id) {
		return
	}
	c.add_fn_value_candidates(name, cur_module, imports, mut calls)
	c.add_const_alias_candidates(name, cur_module, imports, mut calls)
}

// collect_fn_value_selector updates collect fn value selector state for markused.
@[direct_array_access]
fn (c &CallCollector) collect_fn_value_selector(id flat.NodeId, node &flat.Node, cur_module string, imports map[string]string, local_values map[string]bool, mut calls []string) {
	if resolved := c.tc.resolved_fn_value_name(id) {
		if c.selector_is_enum_value(node, cur_module, imports, local_values) {
			return
		}
		calls << resolved
		return
	}
	// A field access can share its name with a receiver method. Only apply the
	// typed-selector fallback when checking established that the selector itself
	// is a function value; otherwise the fallback over-marks unrelated methods.
	if !c.node_is_fn_value(id) {
		return
	}
	if c.selector_is_enum_value(node, cur_module, imports, local_values) {
		return
	}
	if c.collect_interface_method_value_selector(node, mut calls) {
		return
	}
	if c.detect_generics
		&& c.collect_generic_method_value_selector(node, cur_module, imports, mut calls) {
		return
	}
	for name in c.fn_value_selector_names(node, cur_module, imports) {
		if !c.name_may_reference_fn(name, cur_module, imports) {
			continue
		}
		has_fn_decl := c.name_has_fn_decl(name, cur_module, imports)
		if !has_fn_decl && !c.node_is_fn_value(id) {
			continue
		}
		c.add_fn_value_candidates(name, cur_module, imports, mut calls)
		c.add_const_alias_candidates(name, cur_module, imports, mut calls)
	}
}

fn (c &CallCollector) selector_is_enum_value(node &flat.Node, cur_module string, imports map[string]string, local_values map[string]bool) bool {
	if node.kind != .selector || node.children_count == 0 || node.value.len == 0 {
		return false
	}
	base_id := c.a.child(node, 0)
	base := c.a.node(base_id)
	if base.kind == .ident && base.value in local_values {
		return false
	}
	base_name := c.qualified_expr_name(base_id)
	if base_name.len == 0 {
		return false
	}
	resolved_name := markused_resolve_imported_source_type(flat.source_type_id(base_name), imports)
	mut candidates := [resolved_name]
	qualified_name := markused_qualify_source_type(cur_module, resolved_name)
	if qualified_name != resolved_name {
		candidates << qualified_name
	}
	for candidate in candidates {
		mut enum_name := candidate
		for _ in 0 .. 16 {
			if fields := c.tc.enum_fields[enum_name] {
				return node.value in fields
			}
			target := c.tc.type_aliases.type_id(enum_name) or { break }
			if target == enum_name {
				break
			}
			enum_name = target
		}
	}
	return false
}

fn (c &CallCollector) collect_generic_method_value_selector(node &flat.Node, cur_module string, imports map[string]string, mut calls []string) bool {
	// A generic receiver method value is resolved to its open declaration name,
	// but monomorphization also needs the concrete receiver application in the
	// reachable set so it emits the wrapper's target specialization.
	mut has_generic_method_value := false
	for name in c.fn_value_selector_names(node, cur_module, imports) {
		if !name.contains('.') {
			continue
		}
		// Selector candidates are declaration symbols; split the receiver at this lookup boundary.
		owner_id := flat.source_type_id(name.all_before_last('.'))
		method := name.all_after_last('.')
		if info := c.tc.generic_method_value_info[owner_id][method] {
			calls << name
			calls << info.name
			has_generic_method_value = true
		}
	}
	return has_generic_method_value
}

fn (c &CallCollector) fn_value_selector_names(node &flat.Node, cur_module string, imports map[string]string) []string {
	if node.children_count == 0 || node.value.len == 0 {
		return []string{}
	}
	base_id := c.a.child(node, 0)
	base := c.a.node(base_id)
	mut names := []string{}
	if base.kind == .ident && base.value_text().len > 0 {
		markused_push_fn_value_selector_name(mut names, '${base.value_text()}.${node.value}')
		if base.value_text() in imports {
			markused_push_fn_value_selector_name(mut names, '${imports[base.value_text()]}.${node.value}')
		}
	}
	mut type_names := [resolve_type_name(c.node_type(base_id))]
	if base.typ != 0 {
		annotated_name_source_id := resolve_type_name(c.tc.parse_type(base.typ))
		if annotated_name_source_id != 0 && annotated_name_source_id !in type_names {
			type_names << annotated_name_source_id
		}
	}
	for type_name in type_names {
		if info := c.tc.generic_method_value_info[type_name][node.value] {
			concrete_method := '${flat.source_type_name(type_name)}.${node.value}'
			markused_push_fn_value_selector_name(mut names, concrete_method)
			markused_push_fn_value_selector_name(mut names, info.name)
		}
		if method_name := c.typed_receiver_method_name(type_name, node.value, cur_module) {
			markused_push_fn_value_selector_name(mut names, method_name)
		}
	}
	return names
}

fn markused_push_fn_value_selector_name(mut names []string, name string) {
	if name.len > 0 && name !in names {
		names << name
	}
}

@[direct_array_access]
fn (c &CallCollector) collect_interface_method_value_selector(node &flat.Node, mut calls []string) bool {
	if node.children_count == 0 || node.value.len == 0 {
		return false
	}
	base_id := c.a.child(node, 0)
	mut base_type := types.unwrap_pointer(c.node_type(base_id))
	if base_type is types.Alias {
		base_type = base_type.base_type
	}
	if base_type !is types.Interface {
		return false
	}
	iface := base_type as types.Interface
	if _ := c.tc.interface_method_signature_key(iface.source_type_id(), node.value) {
		calls << '${iface.name()}.${node.value}'
		return true
	}
	return false
}

// name_may_reference_fn returns name may reference fn data for CallCollector.
fn (c &CallCollector) name_may_reference_fn(name string, cur_module string, imports map[string]string) bool {
	return c.name_has_candidate_decl(name, cur_module, imports, true)
}

// name_has_fn_decl returns name has fn decl data for CallCollector.
fn (c &CallCollector) name_has_fn_decl(name string, cur_module string, imports map[string]string) bool {
	return c.name_has_candidate_decl(name, cur_module, imports, false)
}

// name_has_candidate_decl returns name has candidate decl data for CallCollector.
struct DeclNameResults {
mut:
	functions map[string]bool
	values    map[string]bool
}

struct DeclNameCache {
mut:
	modules map[string]&DeclNameResults
	// markused_c_name results of the receiver-method names this collector
	// spells; that function depends on its argument only.
	c_names map[string]string
}

fn (c &CallCollector) name_has_candidate_decl(name string, cur_module string, imports map[string]string, include_consts bool) bool {
	// Bare local names cannot depend on import aliases. These declaration
	// indexes stay immutable for this collector, including its worker forks.
	if name.index_u8(`.`) >= 0 || isnil(c.decl_name_cache) {
		return c.name_has_candidate_decl_uncached(name, cur_module, imports, include_consts)
	}
	if !c.fn_suffixes[name] && !(include_consts && c.const_suffixes[name]) { return false }
	mut cache := c.decl_name_cache
	mut module_results := cache.modules[cur_module] or {
		fresh := &DeclNameResults{}
		cache.modules[cur_module] = fresh
		fresh
	}
	if include_consts {
		if result := module_results.values[name] { return result }
	} else {
		if result := module_results.functions[name] { return result }
	}
	result := c.name_has_candidate_decl_uncached(name, cur_module, imports, include_consts)
	if include_consts {
		module_results.values[name] = result
	} else {
		module_results.functions[name] = result
	}
	return result
}

fn (c &CallCollector) name_has_candidate_decl_uncached(name string, cur_module string, imports map[string]string, include_consts bool) bool {
	has_dot := name.index_u8(`.`) >= 0
	if !has_dot && !c.fn_suffixes[name] && !(include_consts && c.const_suffixes[name]) {
		return false
	}
	if c.candidate_matches_decl(name, include_consts) {
		return true
	}
	qname := qualify_fn(cur_module, name)
	if qname != name && c.candidate_matches_decl(qname, include_consts) {
		return true
	}
	if has_dot {
		base := name.all_before_last('.')
		member := name.all_after_last('.')
		if base in imports {
			imported_name := imports[base] + '.' + member
			if c.candidate_matches_decl(imported_name, include_consts) {
				return true
			}
		}
	}
	return false
}

// candidate_matches_decl supports candidate matches decl handling for CallCollector.
fn (c &CallCollector) candidate_matches_decl(candidate string, include_consts bool) bool {
	if candidate in c.fn_decls {
		return true
	}
	return include_consts && candidate in c.const_decls
}

// node_is_fn_value supports node is fn value handling for CallCollector.
fn (c &CallCollector) node_is_fn_value(id flat.NodeId) bool {
	expr_type := c.node_type(id)
	return expr_type is types.FnType
}

// add_fn_value_candidates updates add fn value candidates state for CallCollector.
fn (c &CallCollector) add_fn_value_candidates(name string, cur_module string, imports map[string]string, mut calls []string) {
	if name.len == 0 {
		return
	}
	calls << name
	qname := qualify_fn(cur_module, name)
	if qname != name {
		calls << qname
	}
	if name.contains('.') {
		base := name.all_before_last('.')
		member := name.all_after_last('.')
		if base in imports {
			calls << imports[base] + '.' + member
		}
	}
}

// add_const_alias_candidates converts add const alias candidates data for markused.
fn (c &CallCollector) add_const_alias_candidates(name string, cur_module string, imports map[string]string, mut calls []string) {
	c.add_const_alias_candidate(name, imports, mut calls)
	qname := qualify_fn(cur_module, name)
	if qname != name {
		c.add_const_alias_candidate(qname, imports, mut calls)
	}
	if name.contains('.') {
		base := name.all_before_last('.')
		member := name.all_after_last('.')
		if base in imports {
			c.add_const_alias_candidate(imports[base] + '.' + member, imports, mut calls)
		}
	}
}

// add_const_alias_candidate converts add const alias candidate data for markused.
fn (c &CallCollector) add_const_alias_candidate(const_name string, imports map[string]string, mut calls []string) {
	info := c.const_decls[const_name] or { return }
	expr := c.a.node(info.expr_id)
	match expr.kind {
		.ident {
			c.add_fn_value_candidates(expr.value_text(), info.module, imports, mut calls)
		}
		.selector {
			if expr.children_count > 0 {
				base := c.a.child_node(expr, 0)
				if base.kind == .ident && base.value_text().len > 0 && expr.value_text().len > 0 {
					c.add_fn_value_candidates('${base.value_text()}.${expr.value_text()}', info.module, imports, mut calls)
					if base.value_text() in imports {
						c.add_fn_value_candidates('${imports[base.value_text()]}.${expr.value_text()}', info.module, imports, mut calls)
					}
				}
			}
		}
		else {}
	}
}

// node_type supports node type handling for CallCollector.
fn (c &CallCollector) node_type(id flat.NodeId) types.Type {
	if t := c.tc.expr_type(id) {
		return t
	}
	return c.tc.resolve_type(id)
}

fn (c &CallCollector) collect_typed_receiver_method(base_id flat.NodeId, method string, cur_module string, imports map[string]string, local_values map[string]bool, local_types flat.TypeTextBindings, mut calls []string) bool {
	base := c.a.node(base_id)
	if base.kind == .ident && !c.local_ident_types.contains(int(base_id)) {
		if type_id := c.checked_local_type_id(base_id) {
			if method_name := c.typed_receiver_method_id(type_id, method, cur_module) {
				c.add_typed_receiver_method_name(method_name, mut calls)
				return true
			}
			value := c.tc.type_by_id(type_id) or { return false }
			clean := types.unalias_type(types.unwrap_pointer(value))
			return clean is types.Enum && clean.is_flag
				&& method in ['has', 'all', 'set', 'clear', 'toggle', 'set_all', 'clear_all', 'is_empty']
		}
	}
	type_name_source_id_11 := c.receiver_type_name(base_id, cur_module, imports, local_values, local_types)
	if type_name_source_id_11 == 0 {
		return false
	}
	if method_name := c.typed_receiver_method_name(type_name_source_id_11, method, cur_module) {
		c.add_typed_receiver_method_name(method_name, mut calls)
		return true
	}
	return c.flag_enum_intrinsic_receiver_method(type_name_source_id_11, method, cur_module)
}

fn (c &CallCollector) flag_enum_intrinsic_receiver_method(type_name_id u32, method string, cur_module string) bool {
	if method !in ['has', 'all', 'set', 'clear', 'toggle', 'set_all', 'clear_all', 'is_empty'] {
		return false
	}
	clean_id := markused_clean_receiver_type_name(type_name_id)
	mut candidates := [clean_id]
	qualified := markused_qualify_source_type(cur_module, clean_id)
	if qualified != clean_id {
		candidates << qualified
	}
	for candidate in candidates {
		parsed := types.unalias_type(types.unwrap_pointer(c.tc.parse_type(candidate)))
		if parsed is types.Enum && parsed.is_flag {
			// Flag-enum receiver methods are lowered by the compiler and do not have a
			// source declaration to retain. Treat the typed call as resolved so the
			// same-name fallback cannot retain an unrelated method such as `map.set`.
			return true
		}
	}
	return false
}

fn (c &CallCollector) collect_sum_variant_receiver_methods(base_id flat.NodeId, method string, cur_module string, mut calls []string) bool {
	mut base_type := types.unwrap_pointer(c.node_type(base_id))
	if base_type is types.Alias {
		base_type = base_type.base_type
	}
	if base_type !is types.SumType {
		return false
	}
	sum_name := (base_type as types.SumType).source_type_id()
	return c.collect_sum_variant_receiver_methods_for_type_name(sum_name, method, cur_module, mut calls)
}

fn (c &CallCollector) collect_sum_variant_receiver_methods_for_type_name(type_name_id u32, method string, cur_module string, mut calls []string) bool {
	mut clean_id := markused_clean_receiver_type_name(type_name_id)
	for flat.source_type_shape(clean_id).has(.optional) {
		clean_id = markused_trimmed_source_type(flat.source_type_syntax(clean_id).raw_base())
	}
	mut sum_names := [clean_id]
	qualified := markused_qualify_source_type(cur_module, clean_id)
	if qualified != clean_id {
		sum_names << qualified
	}
	mut variants := []u32{}
	for sum_name in sum_names {
		if known_source_ids := c.tc.sum_types.type_ids(sum_name) {
			variants = known_source_ids.clone()
			break
		}
	}
	if variants.len == 0 {
		return false
	}
	mut added := false
	for variant in variants {
		if candidate := c.typed_receiver_method_name(variant, method, cur_module) {
			c.add_typed_receiver_method_name(candidate, mut calls)
			added = true
		}
	}
	return added
}

fn (c &CallCollector) collect_index_operator_method(index_id flat.NodeId, method string, cur_module string, imports map[string]string, local_values map[string]bool, local_types flat.TypeTextBindings, mut calls []string) bool {
	if !c.has_receiver_method(method) {
		return false
	}
	if int(index_id) < 0 {
		return false
	}
	index := c.a.node(index_id)
	if index.kind != .index || index.value == 'range' || index.children_count < 2 {
		return false
	}
	base_id := c.a.child(index, 0)
	if int(base_id) < 0 {
		return false
	}
	return c.collect_typed_receiver_method(base_id, method, cur_module, imports, local_values, local_types, mut calls)
}

fn (c &CallCollector) has_receiver_method(method string) bool {
	// Use the same complete declaration index as checker receiver-method queries.
	// Incomplete manually built views and the compatibility switch keep the scan.
	return !c.tc.method_suffix_prescreen || c.tc.top_level_idx.len == 0
		|| method in c.tc.receiver_method_suffix_index
		|| '@${method}' in c.tc.receiver_method_suffix_index
}

fn (c &CallCollector) receiver_type_name(base_id flat.NodeId, cur_module string, imports map[string]string, local_values map[string]bool, local_types flat.TypeTextBindings) u32 {
	base := c.a.node(base_id)
	if base.kind == .ident {
		if scoped_type_source_id := c.local_ident_types.type_id(int(base_id)) {
			return scoped_type_source_id
		}
		if type_name_source_id := c.checked_local_type_name(base_id) {
			type_name_source_id_29 := type_name_source_id

			return type_name_source_id_29
		}
	}
	if base.kind == .struct_init && base.source_value_id() != 0 {
		return markused_resolve_imported_source_type(base.source_value_id(), imports)
	}
	if base.kind == .call {
		type_name_source_id_30 := c.top_level_call_return_type_name(base_id, cur_module, imports, local_values, local_types, false)
		if type_name_source_id_30 != 0 {
			return type_name_source_id_30
		}
	}
	// Resolve a method receiver reached through a field selector (`a.flags.set()`)
	// from the declared field type before consulting the checker's erased runtime
	// type. Without this, an ArrayFlags field can look like `array`, and the suffix
	// fallback can retain unrelated same-named methods such as `map.set`.
	if base.kind == .selector && base.children_count > 0 {
		inner_id := c.a.child(base, 0)
		if int(inner_id) >= 0 {
			inner_type_source_id_2 := c.receiver_type_name(inner_id, cur_module, imports, local_values, local_types)
			if inner_type_source_id_2 != 0 {
				struct_name := c.struct_lookup_name(inner_type_source_id_2, cur_module)
				lookup := if struct_name != 0 { struct_name } else { inner_type_source_id_2 }
				if field_type_source_id := c.tc.struct_field_type_name(lookup, base.value_text()) {
					if field_type_source_id != 0 {
						return field_type_source_id
					}
				}
			}
		}
	}
	base_type := c.node_type(base_id)
	type_name_source_id_31 := resolve_type_name(base_type)
	if type_name_source_id_31 != 0 {
		return type_name_source_id_31
	}
	if base.kind == .index {
		if elem_type_source_id := c.top_level_index_elem_type_name(base_id, cur_module, imports, local_values, local_types) {
			return elem_type_source_id
		}
	}
	if base.kind == .ident && base.value_text().len > 0 {
		if local_type_source_id := local_types.type_id(base.value_text()) {
			return local_type_source_id
		}
	}
	if base.kind == .ident && base.value_text().len > 0 {
		if base.value_text() in local_values {
			return u32(0)
		}
		return c.value_type_name(base.value_text(), cur_module, imports)
	}
	if base.kind == .selector {
		name := c.qualified_expr_name(base_id)
		if name.len > 0 {
			return c.value_type_name(name, cur_module, imports)
		}
	}
	return u32(0)
}

struct MarkusedModuleMethods {
mut:
	method_ids       map[string]u32
	semantic_methods map[u64]string
	source_methods   map[u64]string
}

struct MarkusedMethodCache {
mut:
	modules         map[string]&MarkusedModuleMethods
	node_count      int
	signature_count int
}

fn (c &CallCollector) receiver_method_module(cur_module string) &MarkusedModuleMethods {
	mut cache := c.receiver_method_cache
	// A late body check may publish generated declarations. Keep failures and
	// successes valid for the same declaration snapshot as the original memo.
	if cache.node_count != c.a.nodes.len || cache.signature_count != c.tc.fn_ret_types.len {
		cache.modules.clear()
		cache.node_count = c.a.nodes.len
		cache.signature_count = c.tc.fn_ret_types.len
	}
	return cache.modules[cur_module] or {
		fresh := &MarkusedModuleMethods{}
		cache.modules[cur_module] = fresh
		fresh
	}
}

fn (mut module_methods MarkusedModuleMethods) method_id(method string) u32 {
	return module_methods.method_ids[method] or {
		fresh := u32(module_methods.method_ids.len + 1)
		module_methods.method_ids[method] = fresh
		fresh
	}
}

fn (c &CallCollector) typed_receiver_method_id(type_id types.TypeId, method string, cur_module string) ?string {
	if isnil(c.receiver_method_cache) {
		return c.typed_receiver_method_name_uncached(c.tc.type_name_by_id(type_id), method, cur_module)
	}
	mut module_methods := c.receiver_method_module(cur_module)
	key := (u64(type_id) << 32) | u64(module_methods.method_id(method))
	if name := module_methods.semantic_methods[key] {
		if name.len == 0 { return none }
		return name
	}
	// A semantic receiver only crosses the spelling boundary on a cache miss.
	name := c.typed_receiver_method_name_uncached(c.tc.type_name_by_id(type_id), method, cur_module) or {
		module_methods.semantic_methods[key] = ''
		return none
	}
	module_methods.semantic_methods[key] = name
	return name
}

fn (c &CallCollector) typed_receiver_method_name(type_name_id u32, method string, cur_module string) ?string {
	if isnil(c.receiver_method_cache) {
		return c.typed_receiver_method_name_uncached(type_name_id, method, cur_module)
	}
	mut module_methods := c.receiver_method_module(cur_module)
	key := (u64(type_name_id) << 32) | u64(module_methods.method_id(method))
	if name := module_methods.source_methods[key] {
		if name.len == 0 { return none }
		return name
	}
	name := c.typed_receiver_method_name_uncached(type_name_id, method, cur_module) or {
		module_methods.source_methods[key] = ''
		return none
	}
	module_methods.source_methods[key] = name
	return name
}

fn (c &CallCollector) typed_receiver_method_name_uncached(type_name_id u32, method string, cur_module string) ?string {
	clean_id := markused_clean_receiver_type_name(type_name_id)
	clean_type_name := flat.source_type_name(clean_id)
	if clean_id == 0 || method.len == 0 {
		return none
	}
	mut candidates := []string{cap: 4}
	if flat.source_type_shape(clean_id).has(.map_type) {
		candidates << markused_map_receiver_method_candidates(clean_id, method, cur_module)
	} else if flat.source_type_shape(clean_id).has(.array_type) {
		candidates << markused_array_receiver_method_candidates(clean_id, method, cur_module)
	} else if flat.source_type_shape(clean_id).has(.dotted) {
		candidates << '${clean_type_name}.${method}'
		unqualified_type := markused_unqualified_receiver_type_name(clean_id)
		if unqualified_type != clean_id {
			candidates << '${flat.source_type_name(unqualified_type)}.${method}'
		}
	} else {
		if cur_module.len > 0 && cur_module != 'main' && cur_module != 'builtin' {
			candidates << '${cur_module}.${clean_type_name}.${method}'
		}
		candidates << '${clean_type_name}.${method}'
	}
	for candidate in candidates {
		if c.is_known_fn_name(candidate) {
			return candidate
		}
		lowered := c.cached_c_name(candidate)
		if lowered != candidate && c.is_known_fn_name(lowered) {
			return lowered
		}
	}
	return c.collection_alias_receiver_method_name(clean_id, method, cur_module)
}

fn (c &CallCollector) collection_alias_receiver_method_name(type_name_id u32, method string, cur_module string) ?string {
	mut current := type_name_id
	mut owner := cur_module
	mut aliases := []u32{}
	for _ in 0 .. 16 {
		qualified_id := if flat.source_type_shape(current).has(.dotted) {
			current
		} else {
			markused_qualify_source_type(owner, current)
		}
		alias_id := if c.tc.type_aliases.contains(qualified_id) {
			qualified_id
		} else if c.tc.type_aliases.contains(current) {
			current
		} else {
			return none
		}
		if alias_id in aliases || (c.tc.type_alias_generic_params[alias_id] or { []u32{} }).len > 0 {
			return none
		}
		aliases << alias_id
		owner = c.tc.type_alias_modules[alias_id] or { owner }
		target_id := markused_clean_receiver_type_name(c.tc.type_aliases.value_id(alias_id))
		target_shape := flat.source_type_shape(target_id)
		if !target_shape.has(.array_type) && !target_shape.has(.map_type) {
			current = target_id
			continue
		}
		// Only collection aliases inherit these runtime methods. Keep each alias's
		// own methods ahead of the element-specific and generic implementations.
		for alias in aliases {
			candidate := '${flat.source_type_name(alias)}.${method}'
			if c.is_known_fn_name(candidate) {
				return candidate
			}
			lowered := markused_c_name(candidate)
			if lowered != candidate && c.is_known_fn_name(lowered) {
				return lowered
			}
		}
		parsed := types.unalias_type(types.unwrap_all_pointers(types.unalias_type(c.tc.parse_canonical_type(alias_id))))
		mut candidates := []string{}
		if parsed is types.Array {
			candidates << markused_array_receiver_method_candidates(parsed.source_type_id(), method, owner)
			candidates << 'array.${method}'
		} else if parsed is types.Map {
			generic := 'map.${method}'
			for candidate in markused_map_receiver_method_candidates(parsed.source_type_id(), method, owner) {
				if candidate != generic {
					candidates << candidate
				}
			}
			candidates << generic
		} else {
			return none
		}
		for candidate in candidates {
			if c.is_known_fn_name(candidate) {
				return candidate
			}
			lowered := markused_c_name(candidate)
			if lowered != candidate && c.is_known_fn_name(lowered) {
				return lowered
			}
		}
		return none
	}
	return none
}

fn markused_clean_receiver_type_name(type_id u32) u32 {
	mut clean := markused_trimmed_source_type(type_id)
	for {
		shape := flat.source_type_shape(clean)
		if !shape.has(.pointer) && !shape.has(.mutable_type) && !shape.has(.shared_type) {
			return clean
		}
		clean = markused_trimmed_source_type(flat.source_type_syntax(clean).raw_base())
	}
}

fn markused_can_prefix_collection_receiver(cur_module string) bool {
	return cur_module.len > 0 && cur_module != 'main' && cur_module != 'builtin'
}

fn markused_push_receiver_candidate(mut candidates []string, candidate string) {
	if candidate.len > 0 && candidate !in candidates {
		candidates << candidate
	}
}

fn markused_array_receiver_method_candidates(receiver_type_id u32, method string, cur_module string) []string {
	mut clean := markused_trimmed_source_type(receiver_type_id)
	for flat.source_type_shape(clean).has(.pointer) {
		clean = markused_trimmed_source_type(flat.source_type_syntax(clean).raw_base())
	}
	mut candidates := []string{}
	markused_push_receiver_candidate(mut candidates, '${flat.source_type_name(clean)}.${method}')
	if !flat.source_type_shape(clean).has(.array_type) { return candidates }
	elem := flat.source_type_syntax(clean).raw_base()
	if elem == 0 { return candidates }
	elem_syntax := flat.source_type_syntax(elem)
	short_elem := flat.source_type_name(elem_syntax.unqualified())
	markused_push_receiver_candidate(mut candidates, '[]${short_elem}.${method}')
	if flat.source_type_shape(elem).has(.dotted) {
		markused_push_receiver_candidate(mut candidates, '${flat.source_type_name(elem_syntax.qualifier())}.[]${short_elem}.${method}')
	} else if markused_can_prefix_collection_receiver(cur_module) {
		markused_push_receiver_candidate(mut candidates, '${cur_module}.[]${short_elem}.${method}')
	}
	return candidates
}

fn markused_map_receiver_method_candidates(receiver_type_id u32, method string, cur_module string) []string {
	clean_id := markused_clean_map_type(receiver_type_id)
	clean_type := flat.source_type_name(clean_id)
	key_type := markused_map_key_type(clean_id)
	value_type := markused_map_value_type(clean_id)
	mut candidates := []string{}
	candidates << '${clean_type}.${method}'
	candidates << 'map.${method}'
	if key_type == 0 || value_type == 0 {
		markused_push_receiver_candidate(mut candidates, '${clean_type}.${method}')
		return candidates
	}
	key_types := markused_receiver_type_text_variants(key_type)
	value_types := markused_receiver_type_text_variants(value_type)
	mut map_types := []u32{}
	for key in key_types {
		for value in value_types {
			markused_push_receiver_type_candidate(mut map_types, flat.source_type_composition_two(.map, key, value))
		}
	}
	for map_type in map_types {
		markused_push_receiver_candidate(mut candidates, '${flat.source_type_name(map_type)}.${method}')
	}
	mut module_names := []string{}
	if markused_can_prefix_collection_receiver(cur_module) {
		markused_push_receiver_candidate(mut module_names, cur_module)
	}
	for mod_name in markused_receiver_type_text_module_names(key_type) {
		markused_push_receiver_candidate(mut module_names, mod_name)
	}
	for mod_name in markused_receiver_type_text_module_names(value_type) {
		markused_push_receiver_candidate(mut module_names, mod_name)
	}
	for mod_name in module_names {
		for map_type in map_types {
			markused_push_receiver_candidate(mut candidates, '${mod_name}.${flat.source_type_name(map_type)}.${method}')
		}
	}
	return candidates
}

fn markused_push_receiver_type_candidate(mut candidates []u32, candidate u32) {
	if candidate != 0 && candidate !in candidates {
		candidates << candidate
	}
}

fn markused_receiver_type_text_variants(type_id u32) []u32 {
	clean := markused_trimmed_source_type(type_id)
	mut names := []u32{}
	markused_push_receiver_type_candidate(mut names, clean)
	markused_push_receiver_type_candidate(mut names, markused_receiver_type_short_spelling(clean))
	if markused_type_is_fixed_array(clean) {
		source := markused_receiver_type_text_source_fixed_spelling(clean)
		markused_push_receiver_type_candidate(mut names, source)
		markused_push_receiver_type_candidate(mut names, markused_receiver_type_short_spelling(source))
	}
	return names
}

fn markused_receiver_type_text_source_fixed_spelling(type_id u32) u32 {
	clean := markused_trimmed_source_type(type_id)
	if clean == 0 || flat.source_type_shape(clean).has(.leading_bracket) || !markused_type_is_fixed_array(clean) {
		return clean
	}
	syntax := flat.source_type_syntax(clean)
	if syntax.postfix_base() == 0 || syntax.postfix_dims().len == 0 { return clean }
	mut source := syntax.postfix_base()
	for i := syntax.postfix_dims().len; i > 0; i-- {
		source = flat.source_type_composition_two(.fixed_array, syntax.postfix_dims()[i - 1], source)
	}
	return source
}

// markused_receiver_type_short_spelling retains types whose member spelling is unchanged.
fn markused_receiver_type_short_spelling(type_id u32) u32 {
	if !flat.source_type_shape(type_id).has(.dotted) { return type_id }
	return markused_receiver_type_short_spelling_inner(type_id)
}

fn markused_receiver_type_short_spelling_inner(type_id u32) u32 {
	clean := markused_trimmed_source_type(type_id)
	shape := flat.source_type_shape(clean)
	if shape.has(.array_type) {
		return flat.source_type_composition_one(.array, markused_receiver_type_short_spelling_inner(flat.source_type_syntax(clean).raw_base()))
	}
	if shape.has(.leading_bracket) {
		syntax := flat.source_type_syntax(clean)
		if syntax.bracket_closed() {
			return flat.source_type_composition_two(.concat, syntax.bracket_prefix(), markused_receiver_type_short_spelling_inner(syntax.bracket_suffix()))
		}
	}
	if shape.has(.map_type) {
		syntax := flat.source_type_syntax(clean)
		if syntax.bracket_closed() {
			return flat.source_type_composition_two(.map, markused_receiver_type_short_spelling_inner(syntax.bracket_raw_inner()), markused_receiver_type_short_spelling_inner(syntax.bracket_suffix()))
		}
	}
	return if shape.has(.dotted) { flat.source_type_syntax(clean).unqualified() } else { clean }
}

fn markused_receiver_type_text_module_names(type_id u32) []string {
	clean := markused_trimmed_source_type(type_id)
	shape := flat.source_type_shape(clean)
	if shape.has(.array_type) {
		return markused_receiver_type_text_module_names(flat.source_type_syntax(clean).raw_base())
	}
	if shape.has(.leading_bracket) {
		syntax := flat.source_type_syntax(clean)
		if syntax.bracket_closed() {
			return markused_receiver_type_text_module_names(syntax.bracket_suffix())
		}
	}
	mut names := []string{}
	if shape.has(.map_type) {
		for name in markused_receiver_type_text_module_names(markused_map_key_type(clean)) {
			markused_push_receiver_candidate(mut names, name)
		}
		for name in markused_receiver_type_text_module_names(markused_map_value_type(clean)) {
			markused_push_receiver_candidate(mut names, name)
		}
		return names
	}
	if markused_type_is_fixed_array(clean) {
		return markused_receiver_type_text_module_names(markused_fixed_array_elem_type(clean))
	}
	if shape.has(.dotted) {
		markused_push_receiver_candidate(mut names, flat.source_type_name(flat.source_type_syntax(clean).qualifier()))
	}
	return names
}

fn markused_clean_map_type(receiver_id u32) u32 {
	mut clean := markused_trimmed_source_type(receiver_id)
	for {
		shape := flat.source_type_shape(clean)
		if !shape.has(.shared_type) && !shape.has(.pointer) { return clean }
		clean = markused_trimmed_source_type(flat.source_type_syntax(clean).raw_base())
	}
}

fn markused_type_is_fixed_array(type_id u32) bool {
	clean := markused_trimmed_source_type(type_id)
	shape := flat.source_type_shape(clean)
	if clean == 0 || shape.has(.array_type) || shape.has(.map_type) { return false }
	syntax := flat.source_type_syntax(clean)
	if shape.has(.leading_bracket) { return syntax.bracket_closed() }
	if !shape.has(.bracketed) || !flat.source_type_query(clean).has(.bracket_suffix) {
		return false
	}
	return syntax.first_bracket_length() != 0 && !flat.source_type_syntax(syntax.first_bracket_length()).contains_comma()
}

fn markused_fixed_array_elem_type(type_id u32) u32 {
	syntax := flat.source_type_syntax(type_id)
	if flat.source_type_shape(type_id).has(.leading_bracket) { return syntax.bracket_suffix() }
	return syntax.generic_base()
}

fn markused_map_key_type(type_id u32) u32 {
	if !flat.source_type_shape(type_id).has(.map_type) { return u32(0) }
	return flat.source_type_syntax(type_id).bracket_source_inner()
}

fn markused_map_value_type(type_id u32) u32 {
	if !flat.source_type_shape(type_id).has(.map_type) { return u32(0) }
	return flat.source_type_syntax(type_id).bracket_suffix()
}

fn markused_unqualified_receiver_type_name(type_id u32) u32 {
	syntax := flat.source_type_syntax(type_id)
	if !flat.source_type_shape(type_id).has(.map_type) { return syntax.unqualified() }
	if !syntax.first_bracket_closed() { return type_id }
	key := flat.source_type_syntax(syntax.first_bracket_raw_inner()).unqualified()
	value := flat.source_type_syntax(syntax.first_bracket_base()).unqualified()
	return flat.source_type_composition_two(.map, key, value)
}

fn (c &CallCollector) add_typed_receiver_method_name(method_name string, mut calls []string) {
	calls << method_name
	lowered := c.cached_c_name(method_name)
	if lowered != method_name {
		calls << lowered
	}
}

// cached_c_name memoizes markused_c_name for the method names that repeat across
// the bodies of one pass. The cache belongs to this collector (each worker fork
// has its own), like the declaration-name results beside it.
fn (c &CallCollector) cached_c_name(name string) string {
	if isnil(c.decl_name_cache) {
		return markused_c_name(name)
	}
	mut cache := unsafe { c.decl_name_cache }
	if lowered := cache.c_names[name] {
		return lowered
	}
	lowered := markused_c_name(name)
	cache.c_names[name] = lowered
	return lowered
}

fn (c &CallCollector) value_type_name(name string, cur_module string, imports map[string]string) u32 {
	for candidate in c.value_name_candidates(name, cur_module, imports) {
		if typ := c.tc.file_scope.lookup(candidate) {
			type_name_source_id_32 := resolve_type_name(typ)
			if type_name_source_id_32 != 0 {
				return type_name_source_id_32
			}
		}
		if typ := c.tc.const_types[candidate] {
			type_name_source_id_33 := resolve_type_name(typ)
			if type_name_source_id_33 != 0 {
				return type_name_source_id_33
			}
		}
		if info := c.const_decls[candidate] {
			type_name_source_id_34 := c.const_initializer_type_name(info)
			if type_name_source_id_34 != 0 {
				return type_name_source_id_34
			}
		}
	}
	return u32(0)
}

fn (c &CallCollector) const_initializer_type_name(info ConstDeclInfo) u32 {
	expr := c.a.node(info.expr_id)
	if expr.kind == .struct_init && expr.source_value_id() != 0 {
		resolved := markused_resolve_imported_source_type(expr.source_value_id(), c.imports(info.import_context))
		return c.struct_lookup_name(resolved, info.module)
	}
	type_name_source_id_12 := resolve_type_name(c.node_type(info.expr_id))
	if type_name_source_id_12 != 0 {
		return type_name_source_id_12
	}
	return u32(0)
}

fn (c &CallCollector) value_name_candidates(name string, cur_module string, imports map[string]string) []string {
	mut candidates := []string{cap: 3}
	if !isnil(c.tc) {
		if key := c.tc.shadowed_builtin_const_key_in_module(name, cur_module) {
			candidates << key
		}
	}
	qname := qualify_fn(cur_module, name)
	candidates << qname
	if qname != name {
		candidates << name
	}
	if name.contains('.') {
		base := name.all_before_last('.')
		member := name.all_after_last('.')
		if base == 'main' && cur_module in ['', 'main'] {
			candidates << member
		}
		if base in imports {
			candidates << '${imports[base]}.${member}'
		}
	}
	return candidates
}

// Resolve qualified identifiers throughout a type without changing its container syntax.
fn (c &CallCollector) generic_factory_qualified_type_text(text_id u32, id flat.NodeId, cur_module string, imports map[string]string) u32 {
	return c.qualified_type_text_preserving_params(text_id, id, cur_module, imports, []u32{})
}

fn (c &CallCollector) qualified_type_text_preserving_params(text_id u32, id flat.NodeId, cur_module string, imports map[string]string, generic_params []u32) u32 {
	syntax := flat.source_type_syntax(text_id)
	param_ids := generic_params
	mut parts := []u32{cap: syntax.symbol_parts().len}
	mut changed := false
	for i, part in syntax.symbol_parts() {
		mut qualified := part
		if syntax.symbol_is_name()[i] {
			qualified = markused_resolve_imported_source_type(part, imports)
			if qualified !in param_ids {
				qualified = c.tc.qualify_type_name_at(qualified, id, cur_module)
			}
		}
		changed = changed || qualified != part
		parts << qualified
	}
	return if changed { flat.source_type_composition(.concat, parts) } else { text_id }
}

fn markused_resolve_imported_name(name string, imports map[string]string) string {
	if !name.contains('.') {
		return name
	}
	base := name.all_before_last('.')
	member := name.all_after_last('.')
	if base in imports {
		return '${imports[base]}.${member}'
	}
	return name
}

// collect_zero_struct_default_calls updates collect zero struct default calls state for markused.
fn (c &CallCollector) collect_zero_struct_default_calls(typ types.Type, cur_module string, imports map[string]string, mut calls []string) {
	type_name_source_id_13 := zero_value_struct_type_name(typ)
	if type_name_source_id_13 == 0 {
		return
	}
	c.collect_struct_default_calls_for_type(type_name_source_id_13, cur_module, imports, mut calls)
}

// zero_value_struct_type_name supports zero value struct type name handling for markused.
fn zero_value_struct_type_name(typ types.Type) u32 {
	mut clean := typ
	for _ in 0 .. 8 {
		if clean is types.Alias {
			clean = clean.base_type
			continue
		}
		if clean is types.OptionType {
			clean = clean.base_type
			continue
		}
		if clean is types.ResultType {
			clean = clean.base_type
			continue
		}
		break
	}
	if clean is types.Struct {
		return clean.source_type_id()
	}
	return u32(0)
}

// collect_struct_default_calls updates collect struct default calls state for markused.
fn (c &CallCollector) collect_struct_default_calls(init &flat.Node, cur_module string, imports map[string]string, mut calls []string) {
	init_type := c.value_type_in_source(init.source_value_id(), init, imports)
	if init_type is types.Array || init_type is types.ArrayFixed {
		c.collect_array_default_calls(init, cur_module, imports, mut calls)
		return
	}
	info := c.value_struct_decl_info(init_type, cur_module, imports) or { return }
	mut set_fields := map[string]bool{}
	for i in 0 .. init.children_count {
		field := c.a.child_node(init, i)
		if field.kind == .field_init {
			set_fields[field.value] = true
		}
	}
	c.collect_struct_default_calls_from_info(info, value_struct_default_type_name(init_type), set_fields, mut calls)
}

// markused_positional_param_count counts the declared positional params of a fn
// decl. Params precede the body statements; the receiver (op == .dot) is the
// selector base, not a positional argument.
fn markused_positional_param_count(a &flat.FlatAst, fn_node &flat.Node) int {
	mut npos := 0
	for i in 0 .. fn_node.children_count {
		p := a.child_node(fn_node, i)
		if p.kind != .param {
			break
		}
		if p.op != .dot {
			npos++
		}
	}
	return npos
}

// collect_omitted_params_default_calls marks the field-initializer calls of a
// trailing struct parameter that the call site omitted (e.g. `zstd.compress(data)`
// leaving out its `@[params] CompressParams` argument). The transformer synthesizes
// that default argument only AFTER markused runs, so a field default that calls a
// function (`compression_level int = default_c_level()`) would otherwise leave that
// function unmarked and undefined at link.
fn (c &CallCollector) collect_omitted_params_default_calls(call &flat.Node, callee_name string, cur_module string, imports map[string]string, mut calls []string) {
	if call.children_count == 0 {
		return
	}
	// The checker records a `resolved_call` for most calls, but a call inside a
	// still-generic template body can lack it; derive the qualified callee name
	// from the call node in that case.
	mut name := callee_name
	if name.len == 0 {
		callee := c.a.child_node(call, 0)
		if callee.kind == .selector && callee.children_count > 0 {
			base := c.a.child_node(callee, 0)
			if base.kind == .ident && base.value.len > 0 {
				mod := imports[base.value] or { base.value }
				name = '${mod}.${callee.value}'
			}
		} else if callee.kind == .ident && callee.value.len > 0 {
			name = qualify_fn(cur_module, callee.value)
		}
	}
	if name.len == 0 {
		return
	}
	info := c.fn_decls[name] or {
		// `name` is `callee_name` unless the latter was empty; skip the repeated miss.
		if name == callee_name {
			return
		}
		c.fn_decls[callee_name] or { return }
	}
	fn_node := c.a.node(info.node_id)
	npos := if info.positional_params >= 0 {
		info.positional_params
	} else {
		markused_positional_param_count(c.a, fn_node)
	}
	if npos == 0 {
		return
	}
	// Count only positional arguments. Trailing named params
	// (`compress(data, level: 3)`) are `field_init` children of the call, not
	// positional arguments; counting them would make the trailing params struct
	// look fully provided and skip collecting its omitted field defaults.
	// Most calls provide every param; stop counting as soon as they do.
	mut provided := 0
	for i in 1 .. int(call.children_count) {
		arg := c.a.child_node(call, i)
		if arg.kind == .field_init {
			continue
		}
		provided++
		if provided >= npos {
			return
		}
	}
	c.collect_omitted_param_types_default_calls(fn_node, info, provided, mut calls)
}

// collect_omitted_param_types_default_calls visits the param types at positional
// index >= `provided`, the params a call omitted. It is kept out of line so the
// common fully-provided call stays cheap.
@[noinline]
fn (c &CallCollector) collect_omitted_param_types_default_calls(fn_node &flat.Node, info FnDeclInfo, provided int, mut calls []string) {
	imports := c.imports(info.import_context)
	mut param_index := 0
	for i in 0 .. fn_node.children_count {
		p := c.a.child_node(fn_node, i)
		if p.kind != .param {
			break
		}
		if p.op == .dot {
			continue
		}
		if param_index >= provided {
			c.collect_struct_default_calls_for_type(p.typ, info.module, imports, mut calls)
		}
		param_index++
	}
}

// collect_struct_default_calls_for_type supports collect_struct_default_calls_for_type handling.
fn (c &CallCollector) collect_struct_default_calls_for_type(type_name_id u32, cur_module string, imports map[string]string, mut calls []string) {
	info := c.struct_decl_info_with_imports(types.generic_base_type_id(type_name_id), cur_module, imports) or {
		return
	}
	c.collect_struct_default_calls_from_info(info, type_name_id, map[string]bool{}, mut calls)
}

// struct_decl_info supports struct decl info handling for CallCollector.
fn (c &CallCollector) struct_decl_info(type_name_id u32, cur_module string) ?StructDeclInfo {
	struct_name := c.struct_lookup_name(type_name_id, cur_module)
	if struct_name == 0 {
		return none
	}
	if info := c.struct_decls[struct_name] {
		return info
	}
	return none
}

struct StructDefaultCalls {
mut:
	calls     map[string][]string
	active    map[string]bool
	nodes     int
	functions int
}

// collect_struct_default_calls_from_info supports collect_struct_default_calls_from_info handling.
fn (c &CallCollector) collect_struct_default_calls_from_info(info StructDeclInfo, struct_name_id u32, provided map[string]bool, mut calls []string) {
	if !isnil(c.default_calls) {
		mut cache := c.default_calls
		if cache.nodes != c.a.nodes.len || cache.functions != c.tc.fn_ret_types.len {
			cache.calls.clear()
			cache.active.clear()
			cache.nodes = c.a.nodes.len
			cache.functions = c.tc.fn_ret_types.len
		}
		mut fields := provided.keys()
		fields.sort()
		key := '${int(info.node_id)}\x01${struct_name_id}\x01${fields.join('\x01')}'
		if collected := cache.calls[key] {
			calls << collected
			return
		}
		if cache.active[key] {
			// Reentrant defaults share their caller's cycle markers, as the
			// ordinary traversal does, instead of starting an isolated memo.
			mut active := map[u64]bool{}
			c.collect_struct_default_calls_from_info_guarded(info, struct_name_id, provided,
				mut active, mut calls)
			return
		}
		// Compute independently of another initializer's recursion markers. The
		// specialization and explicitly supplied fields determine these edges.
		mut collected := []string{}
		mut active := map[u64]bool{}
		cache.active[key] = true
		c.collect_struct_default_calls_from_info_guarded(info, struct_name_id, provided,
			mut active, mut collected)
		cache.active.delete(key)
		cache.calls[key] = collected
		calls << collected
		return
	}
	mut active_defaults := map[u64]bool{}
	c.collect_struct_default_calls_from_info_guarded(info, struct_name_id, provided, mut active_defaults, mut calls)
}

fn (c &CallCollector) collect_struct_default_calls_from_info_guarded(info StructDeclInfo, struct_name_id u32, provided map[string]bool, mut active_defaults map[u64]bool, mut calls []string) {
	node := c.a.node(info.node_id)
	imports := c.imports(info.import_context)
	_, generic_args, _ := markused_generic_app_parts(struct_name_id)
	generic_params := types.generic_param_ids(node.generic_params())
	for i in 0 .. node.children_count {
		field_id := c.a.child(node, i)
		field := c.a.node(field_id)
		if field.kind != .field_decl || field.value in provided {
			continue
		}
		active_key := (u64(u32(field_id)) << 32) | u64(struct_name_id)
		if active_defaults[active_key] {
			continue
		}
		if field.children_count == 0 {
			// A struct field without a default of its own is initialized with that
			// struct's defaults, which the transformer only expands after markused.
			active_defaults[active_key] = true
			field_type := c.specialized_struct_default_type(field.typ, field_id, info.module, imports, generic_args, generic_params)
			c.collect_value_struct_default_calls(field_type, info.module, imports, mut active_defaults, mut calls)
			active_defaults.delete(active_key)
			continue
		}
		active_defaults[active_key] = true
		default := c.a.child_node(field, 0)
		if default.kind == .struct_init {
			default_type := c.specialized_struct_default_type(default.source_value_id(), c.a.child(field, 0), info.module, imports, generic_args, generic_params)
			if default_type is types.Struct {
				if default_info := c.value_struct_decl_info(default_type, info.module, imports) {
					mut nested_provided := map[string]bool{}
					for j in 0 .. default.children_count {
						explicit_field := c.a.child_node(default, j)
						if explicit_field.kind == .field_init {
							nested_provided[explicit_field.value] = true
						}
						c.collect_calls(explicit_field, info.module, imports, '', u32(0), mut calls)
					}
					c.collect_struct_default_calls_from_info_guarded(default_info, value_struct_default_type_name(default_type), nested_provided, mut active_defaults, mut calls)
					active_defaults.delete(active_key)
					continue
				}
			}
		}
		c.collect_calls(field, info.module, imports, '', u32(0), mut calls)
		active_defaults.delete(active_key)
	}
}

// specialized_struct_default_type substitutes arguments without rebasing their module ownership.
fn (c &CallCollector) specialized_struct_default_type(type_text_id u32, id flat.NodeId, cur_module string, imports map[string]string, generic_args_ids []u32, generic_params []u32) types.Type {
	if generic_args_ids.len == 0 || generic_args_ids.len != generic_params.len {
		return c.value_type_in_source(type_text_id, c.a.node(id), imports)
	}
	// Qualify declaration-owned types before inserting semantic arguments from the caller.
	declared_type_id := c.qualified_type_text_preserving_params(type_text_id, id, cur_module, imports, generic_params)
	substituted_type_id := types.subst_generic_text(declared_type_id, generic_args_ids.clone(), generic_params)
	// Canonical bare argument names belong to main; protect them from an imported declaration's scope.
	qualified_type_source_id := c.generic_factory_qualified_type_text(substituted_type_id, id, 'main', map[string]string{})

	// All names are semantic now; no declaration-file import may capture them.
	return *types.unalias_type(c.tc.parse_resolution_type_in_file(qualified_type_source_id, ''))
}

struct SourceTypes {
mut:
	types map[u32]types.TypeId
}

struct SourceTypeCache {
mut:
	files     map[int]&SourceTypes
	nodes     int
	functions int
	aliases   int
	structs   int
}

// markused_trimmed_source_type preserves already-trimmed source spelling identities.
fn markused_trimmed_source_type(type_id u32) u32 {
	if !flat.source_type_shape(type_id).has(.padded) { return type_id }
	return flat.source_type_syntax(type_id).trimmed()
}

// markused_unwrapped_source_type removes the leading reference/optional spelling syntax.
fn markused_unwrapped_source_type(type_id u32) u32 {
	if !flat.source_type_shape(type_id).has(.wrapped) { return type_id }
	return flat.source_type_syntax(type_id).source_wrapper_base()
}

// value_type_in_source resolves a value's semantic type in its source context.
fn (c &CallCollector) value_type_in_source(type_text_id u32, node &flat.Node, imports map[string]string) types.Type {
	if !isnil(c.source_type_cache) {
		if source := c.a.source_files[node.pos.id] {
			mut cache := c.source_type_cache
			if cache.nodes != c.a.nodes.len || cache.functions != c.tc.fn_ret_types.len
				|| cache.aliases != c.tc.type_aliases.ids.len || cache.structs != c.tc.structs.len {
				cache.files.clear()
				cache.nodes = c.a.nodes.len
				cache.functions = c.tc.fn_ret_types.len
				cache.aliases = c.tc.type_aliases.ids.len
				cache.structs = c.tc.structs.len
			}
			mut file := cache.files[node.pos.id] or {
				fresh := &SourceTypes{}
				cache.files[node.pos.id] = fresh
				fresh
			}
			text_id := type_text_id
			if type_id := file.types[text_id] {
				if typ := c.tc.type_by_id(type_id) { return typ }
			}
			typ := types.unalias_type(c.tc.parse_resolution_type_in_file(markused_trimmed_source_type(type_text_id), source.name))
			if !markused_type_has_unknown(typ) {
				if type_id := c.tc.semantic_type_id(typ) {
					file.types[text_id] = type_id
				}
			}
			return *typ
		}
	}
	clean_id := markused_trimmed_source_type(type_text_id)
	return *types.unalias_type(if source_file := c.a.source_files[node.pos.id] {
		c.tc.parse_resolution_type_in_file(clean_id, source_file.name)
	} else {
		c.tc.parse_canonical_type(markused_resolve_imported_source_type(clean_id, imports))
	})
}

// value_struct_decl_info resolves structs initialized by a semantic value type.
fn (c &CallCollector) value_struct_decl_info(typ types.Type, cur_module string, imports map[string]string) ?StructDeclInfo {
	struct_name_source_id := value_struct_default_type_name(typ)
	if struct_name_source_id == 0 {
		return none
	}
	base_name_id := types.generic_base_type_id(struct_name_source_id)
	// Semantic bare names belong to their registered module, even when substituted
	// into an imported generic declaration with a same-named struct of its own.
	owner := if !flat.source_type_shape(base_name_id).has(.dotted) {
		c.tc.struct_modules[base_name_id] or { cur_module }
	} else {
		cur_module
	}
	return c.struct_decl_info_with_imports(base_name_id, owner, imports)
}

// value_struct_default_type_name preserves the specialization initialized by a value type.
fn value_struct_default_type_name(typ types.Type) u32 {
	mut value_type := types.unalias_type(typ)
	// Fixed arrays initialize their elements; dynamic containers remain empty.
	for value_type is types.ArrayFixed {
		value_type = types.unalias_type(value_type.elem_type)
	}
	// Preserve reference, option and result wrappers while unwrapping aliases.
	if value_type !is types.Struct {
		return u32(0)
	}
	return value_type.source_type_id()
}

// collect_value_struct_default_calls collects field defaults for struct values.
// References, options, dynamic containers and function types have no implicit struct defaults.
fn (c &CallCollector) collect_value_struct_default_calls(typ types.Type, cur_module string, imports map[string]string, mut active_defaults map[u64]bool, mut calls []string) {
	info := c.value_struct_decl_info(typ, cur_module, imports) or { return }
	struct_name_id := value_struct_default_type_name(typ)
	// A default can create more values of its own struct (`next []Node = []Node{len: 1}`),
	// so expand each specialization once per collected body. `@` starts no symbol name.
	expanded_marker := '@markused.struct_defaults:${int(info.node_id)}:${struct_name_id}'
	if expanded_marker in calls {
		return
	}
	calls << expanded_marker
	c.collect_struct_default_calls_from_info_guarded(info, struct_name_id, map[string]bool{}, mut active_defaults, mut calls)
}

// array_literal_without_ctor returns the array literal operand of `node` that cgen
// emits as C data or comparisons, without a runtime array constructor: `[..]!`,
// `x in [..]`, and constant tables that are stored as fixed C arrays.
fn (c &CallCollector) array_literal_without_ctor(node &flat.Node, cur_module string) ?flat.NodeId {
	operand := match node.kind {
		.postfix {
			if node.op == .not { 0 } else { -1 }
		}
		.in_expr {
			1
		}
		.const_field {
			typ := c.tc.const_types[qualify_fn(cur_module, node.value)] or { return none }
			if typ is types.ArrayFixed { 0 } else { -1 }
		}
		else {
			-1
		}
	}
	if operand < 0 || operand >= int(node.children_count) {
		return none
	}
	id := c.a.child(node, operand)
	if int(id) < 0 || c.a.node(id).kind != .array_literal {
		return none
	}
	return id
}

fn (c &CallCollector) collect_array_default_calls(node &flat.Node, cur_module string, imports map[string]string, mut calls []string) {
	mut has_len := false
	for i in 0 .. node.children_count {
		field := c.a.child_node(node, i)
		if field.kind != .field_init {
			continue
		}
		if field.value == 'init' {
			// The explicit initializer is traversed separately, including its defaults.
			return
		}
		if field.value == 'len' {
			has_len = true
		}
	}
	typ_id := if node.typ != 0 { node.typ } else { node.source_value_id() }
	array_type := c.value_type_in_source(typ_id, node, imports)
	element_type := if array_type is types.Array {
		if !has_len {
			// Empty and capacity-only dynamic arrays do not initialize elements.
			return
		}
		*array_type.elem_type
	} else if array_type is types.ArrayFixed {
		*array_type.elem_type
	} else {
		if (flat.source_type_shape(typ_id).has(.array_type) || !flat.source_type_shape(typ_id).has(.leading_bracket)) && !has_len {
			return
		}
		c.value_type_in_source(array_init_element_type_text(node), node, imports)
	}
	mut element_defaults := map[u64]bool{}
	c.collect_value_struct_default_calls(element_type, cur_module, imports, mut element_defaults, mut calls)
}

// array_init_element_type_text returns the value type initialized by an array literal.
// Fixed-array elements initialize their contents too, but nested dynamic arrays remain empty.
fn array_init_element_type_text(node &flat.Node) u32 {
	mut typ := if node.typ != 0 { node.typ } else { node.source_value_id() }
	// Strip the literal's outer dimension once; further dynamic dimensions are containers.
	if node.typ != 0 && flat.source_type_shape(typ).has(.leading_bracket) {
		syntax := flat.source_type_syntax(typ)
		if !syntax.bracket_closed() { return u32(0) }
		typ = syntax.bracket_suffix()
	}
	for flat.source_type_shape(typ).has(.leading_bracket) && !flat.source_type_shape(typ).has(.array_type) {
		syntax := flat.source_type_syntax(typ)
		if !syntax.bracket_closed() { return u32(0) }
		typ = syntax.bracket_suffix()
	}
	return typ
}

// resolve_type_name resolves resolve type name information for markused.
fn resolve_type_name(t types.Type) u32 {
	if t is types.Alias {
		return t.source_type_id()
	} else if t is types.Struct {
		return t.source_type_id()
	} else if t is types.Interface {
		return t.source_type_id()
	} else if t is types.SumType {
		return t.source_type_id()
	} else if t is types.Enum {
		return t.source_type_id()
	} else if t is types.String {
		return flat.source_type_string
	} else if t is types.Array {
		return t.source_type_id()
	} else if t is types.ArrayFixed {
		return flat.source_type_fixed_array(t.len, t.len_expr, t.elem_type.source_type_id(), true)
	} else if t is types.Map {
		return t.source_type_id()
	} else if t is types.Pointer {
		return resolve_type_name(t.base_type)
	} else if t is types.Primitive {
		props := int(t.props)
		sz := int(t.size)
		if props & 1 > 0 { return flat.source_type_bool }
		if props & 4 > 0 {
			if props & 8 > 0 {
				return match sz {
					8 { flat.source_type_u8 }
					16 { flat.source_type_u16 }
					32 { flat.source_type_u32 }
					64 { flat.source_type_u64 }
					else { flat.source_type_int }
				}
			}
			return match sz {
				0 { flat.source_type_int }
				8 { flat.source_type_i8 }
				16 { flat.source_type_i16 }
				32 { flat.source_type_i32 }
				64 { flat.source_type_i64 }
				else { flat.source_type_int }
			}
		}
		if props & 2 > 0 {
			return match sz {
				32 { flat.source_type_f32 }
				64 { flat.source_type_f64 }
				else { flat.source_type_f64 }
			}
		}
		return flat.source_type_int
	} else if t is types.ISize {
		return flat.source_type_isize
	} else if t is types.USize {
		return flat.source_type_uint
	} else if t is types.Rune {
		return flat.source_type_rune
	}
	return 0
}

fn markused_type_name(t types.Type, unwrap_optional_result bool) ?u32 {
	if unwrap_optional_result {
		if t is types.OptionType {
			name_source_id_4 := resolve_type_name(t.base_type)
			if name_source_id_4 != 0 {
				return name_source_id_4
			}
		}
		if t is types.ResultType {
			name_source_id_5 := resolve_type_name(t.base_type)
			if name_source_id_5 != 0 {
				return name_source_id_5
			}
		}
	}
	name_source_id_6 := resolve_type_name(t)
	if name_source_id_6 == 0 {
		return none
	}
	return name_source_id_6
}

fn markused_type_name_or_empty(t types.Type, unwrap_optional_result bool) u32 {
	return markused_type_name(t, unwrap_optional_result) or { u32(0) }
}

// markused_c_name returns the C identifier used for a V symbol or type name.
fn markused_c_name(name string) string {
	if naming.c_name_needs_internal_namespace(name) {
		return naming.c_name(name)
	}
	if name.starts_with('C.') {
		return name[2..]
	}
	if name == 'malloc' {
		return 'v_malloc'
	}
	if name == 'exit' {
		return 'v_exit'
	}
	if naming.is_plain_identifier(name) {
		return name
	}
	return naming.sanitize(name)
}
