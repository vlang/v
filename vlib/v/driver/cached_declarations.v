module driver

import os
import v.flat
import v.modulecache
import v.tempname
import v.types

// A build that reads a module from its cached interface gets every function of the
// module as a declaration, and each stage of the compiler then registers, indexes
// and visits all of them: for `builtin` and the modules it brings along that is
// some sixteen hundred functions, of which a small program calls a few dozen. The
// code of those functions is in the object of the module, so a declaration that
// nothing in the build can name is of no use to it. This file takes such
// declarations out of the AST before the stages run.
//
// What is taken out are plain functions and C function declarations, never
// methods: a method is found through its receiver type, by interfaces, by
// operators and by `str`, and none of those spells its name. A plain function is
// reached by its name only, from the program, from a function body that an
// interface carries, or from the code that the compiler generates. The names of
// the first two are in the AST. Those of the third are cached_runtime_function_names,
// cached_module_function_names and the names that markused seeds, and when
// generated C still names a function that was taken out, the build starts again
// with every declaration and records the name, so that the next build keeps it.
//
// A program that asks for its functions at run time, through `v.reflection`, is
// one where every declaration can be seen: nothing is taken out of it.

// cached_runtime_function_names are the plain functions of `builtin`, of its
// closure runtime and of `strconv`, `strings`, `hash` and `math.bits` that a stage
// of the compiler spells in a string literal: the functions that generated code
// can call without a call in the program. `cached_declarations_test.v` works the
// list out from the sources again and names what is missing here.
const cached_runtime_function_names = ['__as_cast', '__new_array', '__new_array_noscan', 'arguments',
	'array_sort_move', 'atoi', 'autostr_addr_pop', 'autostr_addr_type_in_stack',
	'autostr_addr_type_push', 'builtin_init', 'closure_create_with_data', 'closure_data', 'closure_init',
	'closure_try_destroy', 'common_parse_int', 'common_parse_uint', 'common_parse_uint2', 'copy',
	'cstring_to_vstring', 'data_to_hex_string', 'drop_owned', 'drop_owned_v3_interface', 'eprint',
	'eprintln', 'error', 'error_with_code', 'exit', 'fabs', 'fast_string_eq', 'format_int',
	'format_uint', 'free', 'gc_runtime_init', 'is_digit', 'is_space', 'isnil', 'malloc', 'malloc_noscan',
	'malloc_uncollectable', 'map_clone_int_1', 'map_clone_int_16', 'map_clone_int_2', 'map_clone_int_4',
	'map_clone_int_8', 'map_clone_string', 'map_eq_int_1', 'map_eq_int_16', 'map_eq_int_2',
	'map_eq_int_4', 'map_eq_int_8', 'map_eq_string', 'map_free_nop', 'map_free_string', 'map_hash_int_1',
	'map_hash_int_16', 'map_hash_int_2', 'map_hash_int_4', 'map_hash_int_8', 'map_hash_string',
	'map_map_eq', 'memdup', 'memdup_align', 'memdup_noscan', 'min', 'new_array_from_c_array',
	'new_array_from_c_array_no_alloc', 'new_array_from_c_array_noscan', 'new_builder', 'new_map',
	'panic', 'panic_debug', 'panic_frame_done', 'panic_frame_pop', 'panic_frame_push',
	'panic_frame_relink', 'panic_frames_reset', 'panic_recover_frame', 'parse_int', 'parse_uint',
	'parser', 'print', 'println', 'ptr_str', 'quote', 'recover', 'repeat', 'string_plus_many',
	'tos2', 'v_fixed_index', 'v_fixed_index_i64', 'v_fixed_index_u64', 'v_gettid', 'vcalloc',
	'vgc_scan_range']

// cached_module_function_names are the plain functions of the other modules of
// vlib that a stage of the compiler spells in a string literal, with the name of
// their module: `dl.interface_export_find`, which the generator looks for and
// calls where a program can load a shared library, and leaves out without a word
// where it is not declared. Every function of the same name is kept, in whatever
// module it is. `cached_declarations_test.v` works this list out again as well.
const cached_module_function_names = ['c.gen_expr_lvalue', 'debug.after_call_hook',
	'debug.before_call_hook', 'dl.interface_export_find', 'driver.compare_print_notices',
	'embed_file.join_chunks', 'json2.decode', 'json2.encode', 'math.abs', 'math.fmod', 'math.min',
	'naming.type_name_part', 'orm.v_sql_query_data_add', 'orm.v_sql_query_data_parentheses', 'os.exists',
	'os.exists_in_system_path', 'os.file_ext', 'os.file_name', 'os.find_abs_path_of_executable',
	'os.getpid', 'os.is_abs_path', 'os.is_dir', 'os.is_drive_rooted', 'os.is_executable', 'os.is_file',
	'os.is_normal_path', 'os.is_unc_path', 'os.join_path', 'os.join_path_single',
	'os.kind_of_existing_path', 'os.ls', 'os.mkdir', 'os.mkdir_all', 'os.posix_get_error_msg',
	'os.read_bytes', 'os.read_file', 'os.real_path', 'os.walk', 'os.win_volume_len', 'overflow.add_i8',
	'pref.detect_vexe', 'pref.detect_vroot', 'rand.init', 'sync.channel_select',
	'sync.channel_select_lang', 'sync.cpanic', 'sync.cpanic_errno', 'sync.new_channel_st',
	'sync.should_be_zero', 'time.now', 'time.sleep', 'time.ticks', 'time.vpc_now', 'time.vpc_now_darwin',
	'types.compare_type_errors', 'types.compare_type_notices', 'veb.run_at']

// module_lifecycle_function_names are the functions that a program calls for each
// of its modules that has them, without a call in its source.
const module_lifecycle_function_names = ['init', 'cleanup']

// kept_cached_functions_file holds the functions that a build found generated C to
// name after it had taken their declarations out. It lies in the directory of the
// module cache, which belongs to one compiler and one configuration.
const kept_cached_functions_file = 'kept_cached_functions'

const v3_internal_all_cached_declarations_flag = '-v3-internal-all-cached-declarations'
const v3_internal_checker_notices_printed_flag = '-v3-internal-checker-notices-printed'

// V3PrunedDeclarations are the functions of cached interfaces that a build took
// out of its AST. `names[i]` is a V function that `modules[i]` declares; the C
// functions are in `short` only, with the names of the V functions.
struct V3PrunedDeclarations {
mut:
	count   int
	names   []string
	modules []string
	short   map[string]bool
}

// v3_cached_function_is_prunable reports whether `node`, a declaration of a cached
// interface, is one that only its name leads to: a C function, or a V function
// without a receiver whose body is in the object of its module.
fn v3_cached_function_is_prunable(a &flat.FlatAst, node &flat.Node) bool {
	// A name with a dot is that of a method: a static one has no receiver.
	if node.kind !in [.fn_decl, .c_fn_decl] || node.is_static_type_method()
		|| node.value.contains('.') {
		return false
	}
	if node.kind == .c_fn_decl {
		return true
	}
	if !node.is_mut {
		return false
	}
	for i in 0 .. node.children_count {
		child := a.child_node(node, i)
		if child.kind != .param || child.op == .dot {
			return false
		}
	}
	return true
}

// v3_program_lists_its_functions reports whether a program of `a` can ask at run
// time which functions it has: the generator fills the tables of `v.reflection`
// from the declarations of the AST.
fn v3_program_lists_its_functions(a &flat.FlatAst) bool {
	for node in a.nodes {
		if node.kind in [.module_decl, .import_decl]
			&& (node.value == 'reflection' || node.value.ends_with('.reflection')) {
			return true
		}
	}
	return false
}

// prune_unreferenced_cached_functions takes the functions of cached interfaces that
// nothing in `a` names, and that are not among `keep`, out of `a`: their nodes and
// the nodes of their attributes become empty ones, which every stage passes over.
// A name counts wherever a node spells it, as an identifier, a callee, a field or
// a qualified name, so a function that only shares its name with something that is
// used stays in. Call it after every file is parsed and before the checker
// collects declarations.
fn prune_unreferenced_cached_functions(mut a flat.FlatAst, keep []string) V3PrunedDeclarations {
	mut pruned := V3PrunedDeclarations{}
	if a.cached_header_sources.len == 0 || v3_program_lists_its_functions(a) {
		return pruned
	}
	// A function that is exported, or that its module marks as used, is one that
	// something outside the build reaches: its declaration stays.
	mut rooted := []bool{len: a.nodes.len}
	for node in a.nodes {
		if node.kind == .directive && node.value.starts_with('@attributes:') {
			decl_id := node.value['@attributes:'.len..].int()
			if decl_id >= 0 && decl_id < rooted.len {
				for attribute in node.generic_params() {
					if attribute.all_before(':').trim_space() in ['export', 'markused'] {
						rooted[decl_id] = true
					}
				}
			}
		}
	}
	// The signature of a candidate names nothing that a build could need it for.
	mut in_candidate := []bool{len: a.nodes.len}
	mut candidates := []int{}
	mut candidate_modules := []string{}
	for node in a.nodes {
		if node.kind != .file || node.value !in a.cached_header_sources {
			continue
		}
		mut module_name := ''
		for i in 0 .. node.children_count {
			child_id := a.child(&node, i)
			child := a.node(child_id)
			if child.kind == .module_decl {
				module_name = child.value
			} else if !rooted[int(child_id)] && v3_cached_function_is_prunable(a, child) {
				in_candidate[int(child_id)] = true
				candidates << int(child_id)
				candidate_modules << module_name
				for j in 0 .. child.children_count {
					in_candidate[int(a.child(child, j))] = true
				}
			}
		}
	}
	if candidates.len == 0 {
		return pruned
	}
	mut named := map[string]bool{}
	for name in keep {
		named[name] = true
		if name.contains('.') {
			named[name.all_after_last('.')] = true
		}
	}
	for idx, node in a.nodes {
		if in_candidate[idx] || node.value.len == 0 || node.kind in [.string_literal, .int_literal,
			.float_literal, .char_literal, .file, .directive, .empty] {
			continue
		}
		named[node.value] = true
		if node.value.contains('.') {
			named[node.value.all_after_last('.')] = true
		}
	}
	mut taken_out := []bool{len: a.nodes.len}
	for i, id in candidates {
		node := a.nodes[id]
		short := node.value.all_after_last('.')
		if named[node.value] || named[short] {
			continue
		}
		a.nodes[id].kind = .empty
		taken_out[id] = true
		pruned.count++
		pruned.short[short] = true
		// The prototype of a C function is that of its header: the generator has
		// none to give, and cannot miss it.
		if node.kind == .fn_decl {
			pruned.names << node.value
			pruned.modules << candidate_modules[i]
		}
	}
	if pruned.count == 0 {
		return pruned
	}
	// The attributes of a declaration are a node of their own that names it.
	for idx, node in a.nodes {
		if node.kind == .directive && node.value.starts_with('@attributes:') {
			decl_id := node.value['@attributes:'.len..].int()
			if decl_id >= 0 && decl_id < taken_out.len && taken_out[decl_id] {
				a.nodes[idx].kind = .empty
			}
		}
	}
	return pruned
}

// v3_kept_cached_functions returns the names that earlier builds recorded with
// v3_remember_kept_cached_functions.
fn v3_kept_cached_functions(manager &modulecache.Manager) []string {
	content := os.read_file(os.join_path_single(manager.dir, kept_cached_functions_file)) or {
		return []string{}
	}
	return content.split_into_lines().filter(it.len > 0)
}

// v3_remember_kept_cached_functions adds `names` to the functions that the builds
// of this module cache keep the declarations of.
fn v3_remember_kept_cached_functions(manager &modulecache.Manager, names []string) {
	if names.len == 0 || !manager.ensure_dir() {
		return
	}
	mut kept := v3_kept_cached_functions(manager)
	for name in names {
		if name !in kept && !name.contains_any(' \t\r\n') {
			kept << name
		}
	}
	kept.sort()
	path := os.join_path_single(manager.dir, kept_cached_functions_file)
	tmp := '${path}.tmp.${tempname.unique_token()}'
	os.write_file(tmp, kept.join('\n') + '\n') or {
		os.rm(tmp) or {}
		return
	}
	os.mv(tmp, path) or { os.rm(tmp) or {} }
}

// v3_pruned_functions_named_in returns the functions that were taken out and that
// `text`, a message of the checker or of the C compiler, spells.
fn v3_pruned_functions_named_in(text string, pruned &V3PrunedDeclarations) []string {
	mut found := []string{}
	if pruned.count == 0 {
		return found
	}
	mut i := 0
	for i < text.len {
		c := text[i]
		if !(c.is_letter() || c == `_`) {
			i++
			continue
		}
		start := i
		for i < text.len && (text[i].is_letter() || text[i].is_digit() || text[i] == `_`) {
			i++
		}
		word := text[start..i]
		// The C name of a function of a module starts with the name of the module.
		short := if word.contains('__') { word.all_after_last('__') } else { word }
		for candidate in [word, short] {
			if candidate.len > 0 && pruned.short[candidate] && candidate !in found {
				found << candidate
			}
		}
	}
	return found
}

// restart_v3_with_all_cached_declarations starts the build again with every
// declaration of the cached interfaces. `learned` are the functions that this build
// should have kept; `notices_printed` says that the checker has printed its notices
// and warnings, which the build that follows must not print again.
fn restart_v3_with_all_cached_declarations(manager &modulecache.Manager, reason string, learned []string, notices_printed bool) {
	v3_remember_kept_cached_functions(manager, learned)
	if os.getenv('V3_CACHE_TRACE') != '' {
		eprintln('  V3 cached declarations: starting again with all of them: ${reason}')
	}
	mut args := [v3_internal_all_cached_declarations_flag]
	if notices_printed {
		args << v3_internal_checker_notices_printed_flag
	}
	restart_v3_with_args(args)
}

// restart_v3_for_errors_of_pruned_build starts a build that left functions out
// again with every declaration when a stage reports errors, before any of them is
// printed. What a stage says about a program can depend on the declarations that it
// knows, as the names that an error suggests do: the build that reports errors is
// one that has them all. The functions that `messages` spell are kept from then on.
fn restart_v3_for_errors_of_pruned_build(manager &modulecache.Manager, messages []string, pruned &V3PrunedDeclarations, notices_printed bool) {
	if pruned.count == 0 || messages.len == 0 {
		return
	}
	mut named := []string{}
	for message in messages {
		for name in v3_pruned_functions_named_in(message, pruned) {
			if name !in named {
				named << name
			}
		}
	}
	restart_v3_with_all_cached_declarations(manager, 'a stage reports errors', named, notices_printed)
}

// restart_v3_for_checker_errors_of_pruned_build is
// restart_v3_for_errors_of_pruned_build for the errors of the checker.
fn restart_v3_for_checker_errors_of_pruned_build(manager &modulecache.Manager, errors []types.TypeError, pruned &V3PrunedDeclarations, notices_printed bool) {
	if pruned.count == 0 || errors.len == 0 {
		return
	}
	mut messages := []string{cap: errors.len}
	for error in errors {
		messages << error.msg
		messages << error.details
	}
	restart_v3_for_errors_of_pruned_build(manager, messages, pruned, notices_printed)
}

// v3_c_output_reports_source_error reports whether `output`, what a C compiler
// printed, has an error at a position in a source: `file:line: error` or
// `file:line:column: error`. An error of the link has no such position, and no
// declaration that a build left out can be its cause.
fn v3_c_output_reports_source_error(output string) bool {
	mut pos := 0
	for {
		found := output.index_after_(': error', pos)
		if found < 0 {
			return false
		}
		if found > 0 && output[found - 1].is_digit() {
			return true
		}
		pos = found + 1
	}
	return false
}
