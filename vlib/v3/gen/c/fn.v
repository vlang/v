module c

import strings
import v3.flat
import v3.types

// Match the previous spawned pthread stack size so recursive workers and
// functions with large stack locals do not regress at spawn sites.
const spawn_thread_stack_size = 8 * 1024 * 1024

struct TestHarnessFn {
	name   string
	c_name string
	ret    types.Type
}

struct TestHarnessHooks {
mut:
	testsuite_begin string
	testsuite_end   string
	before_each     string
	after_each      string
}

struct TopLevelStmt {
	id     flat.NodeId
	file   string
	module string
}

struct GenericMethodCandidate {
	name   string
	ret    types.Type
	params []types.Type
}

struct SpawnPackedArg {
	field_ct    string
	assign_expr string
	call_expr   string
	copy_array  bool
}

// FlatFnGenItem represents one top-level function selected for C emission.
struct FlatFnGenItem {
	node_id                   flat.NodeId
	file                      string
	module                    string
	c_name                    string
	cost                      int
	is_program_specialization bool
}

struct FlatFnGenCandidate {
	preferred_name string
	item           FlatFnGenItem
}

// gen_fns emits fns output for c.
fn (mut g FlatGen) gen_fns() {
	g.gen_fn_items(g.ensure_fn_gen_items())
}

fn (mut g FlatGen) ensure_fn_gen_items() []FlatFnGenItem {
	if g.fn_gen_items.len == 0 {
		g.fn_gen_items = g.collect_fn_gen_items()
	}
	return g.fn_gen_items
}

// collect_fn_gen_items updates collect fn gen items state for c.
fn (mut g FlatGen) collect_fn_gen_items() []FlatFnGenItem {
	mut candidates := []FlatFnGenCandidate{}
	mut preferred_fns := map[string]int{}
	mut ranks := map[string]int{}
	mut program_specializations := map[string]bool{}
	mut preferred_program_specializations := map[string]bool{}
	mut cur_module := ''
	mut cur_file := ''
	// Defer cost/prep until after preferred-file and emission filtering. When
	// the parallel path asks for prep, that walk also collects C-extern refs.
	prep := g.want_parallel_prep
	for i in 0 .. g.a.nodes.len {
		node := g.a.nodes[i]
		kind_id := node_kind_id(node)
		if kind_id == 77 {
			cur_file = node.value
			g.tc.cur_file = cur_file
			cur_module = ''
			g.tc.cur_module = cur_module
			continue
		}
		if kind_id == 73 {
			cur_module = node.value
			g.tc.cur_file = cur_file
			g.tc.cur_module = cur_module
			continue
		}

		if kind_id == 61 {
			if !g.should_emit_fn_node_in_module(node, i, cur_module, cur_file) {
				continue
			}
			preferred_name := g.qualified_fn_name_in_module_c(cur_module, node.value)
			if g.is_program_specialization_fn_node(node, i, cur_module) {
				program_specializations[preferred_name] = true
			}
			rank := c_backend_fn_file_rank(cur_file)
			is_program_specialization := program_specializations[preferred_name]
			if preferred_name !in preferred_fns || rank > ranks[preferred_name]
				|| (rank == ranks[preferred_name] && is_program_specialization
				&& !preferred_program_specializations[preferred_name]) {
				preferred_fns[preferred_name] = i
				ranks[preferred_name] = rank
				preferred_program_specializations[preferred_name] = is_program_specialization
			}
			candidates << FlatFnGenCandidate{
				preferred_name: preferred_name
				item:           FlatFnGenItem{
					node_id: flat.NodeId(i)
					file:    cur_file
					module:  cur_module
					c_name:  g.fn_c_name_in_module(cur_module, node.value)
				}
			}
		}
	}
	mut items := []FlatFnGenItem{cap: candidates.len}
	mut prep_stack := []flat.NodeId{cap: 256}
	mut prep_type_text_cache := map[string]bool{}
	for candidate in candidates {
		if preferred_idx := preferred_fns[candidate.preferred_name] {
			if preferred_idx != int(candidate.item.node_id) {
				continue
			}
		}
		item := candidate.item
		qfn := item.c_name
		if g.emitted_fn_contains(qfn) {
			continue
		}
		g.emitted_fns[qfn] = true
		cost := if prep {
			if item.file != g.tc.cur_file || item.module != g.tc.cur_module {
				prep_type_text_cache.clear()
			}
			g.tc.cur_file = item.file
			g.tc.cur_module = item.module
			g.fn_item_cost_and_prep(item.node_id, mut prep_stack, mut prep_type_text_cache)
		} else {
			flat_fn_gen_item_cost(g.a, item.node_id)
		}
		items << FlatFnGenItem{
			node_id:                   item.node_id
			file:                      item.file
			module:                    item.module
			c_name:                    item.c_name
			cost:                      cost
			is_program_specialization: program_specializations[candidate.preferred_name]
		}
	}
	return items
}

fn flat_fn_gen_item_cost(a &flat.FlatAst, node_id flat.NodeId) int {
	mut cost := 0
	mut stack := [node_id]
	for stack.len > 0 {
		id := stack.pop()
		idx := int(id)
		if idx < 0 || idx >= a.nodes.len {
			continue
		}
		node := a.nodes[idx]
		cost++
		for i in 0 .. node.children_count {
			child_id := a.children[node.children_start + i]
			if int(child_id) >= 0 {
				stack << child_id
			}
		}
	}
	return cost
}

// gen_fn_items emits fn items output for c.
fn (mut g FlatGen) gen_fn_items(items []FlatFnGenItem) {
	for item in items {
		if int(item.node_id) < 0 || int(item.node_id) >= g.a.nodes.len {
			continue
		}
		g.tc.cur_file = item.file
		g.tc.cur_module = item.module
		node := g.a.nodes[int(item.node_id)]
		is_anon_fn := node.value.starts_with('__anon_fn_') || node.value.contains('.__anon_fn_')
		is_program_fn := item.is_program_specialization
			|| g.is_program_specialization_fn_node(node, int(item.node_id), item.module)
			|| (is_anon_fn && item.module in ['', 'main']) || g.test_files[item.file]
			|| g.cache_program_files[item.file]
		if node.is_mut && item.file.ends_with('.vh') && !is_program_fn {
			continue
		}
		if g.cache_split {
			// Generic templates are source-parsed even when their module object is
			// cached. Their program-specific concrete specializations belong to the
			// main translation unit, not to the source-stable module object.
			module_name := if is_program_fn || item.module.len == 0 {
				'main'
			} else {
				item.module
			}
			g.writeln('/* V3CACHE_MODULE ${module_name} */')
		}
		g.gen_fn_in_module(node, item.module)
	}
}

fn c_backend_fn_file_rank(file string) int {
	if file.ends_with('.c.v') {
		return 1
	}
	return 0
}

fn (mut g FlatGen) gen_synthetic_main_after_fns() {
	if g.test_files.len > 0 {
		if g.cache_split {
			g.writeln('/* V3CACHE_MODULE main */')
		}
		g.gen_test_main()
		return
	}
	if g.has_entry_main() {
		return
	}
	top_level_stmts := g.top_level_stmts()
	if top_level_stmts.len > 0 {
		if g.cache_split {
			g.writeln('/* V3CACHE_MODULE main */')
		}
		g.gen_top_level_main(top_level_stmts)
	}
}

fn (g &FlatGen) has_entry_main() bool {
	mut cur_module := ''
	for node in g.a.nodes {
		kind_id := node_kind_id(node)
		if kind_id == 77 {
			cur_module = ''
			continue
		}
		if kind_id == 73 {
			cur_module = node.value
			continue
		}
		if kind_id == 61 && node.value == 'main' && (cur_module.len == 0 || cur_module == 'main') {
			return true
		}
	}
	return false
}

fn (g &FlatGen) top_level_stmts() []TopLevelStmt {
	mut stmts := []TopLevelStmt{}
	for file_idx, file_node in g.a.nodes {
		if !g.should_emit_top_level_file(file_idx, file_node) {
			continue
		}
		module_name := g.top_level_file_module_name(file_node)
		for i in 0 .. file_node.children_count {
			child_id := g.a.child(&file_node, i)
			if int(child_id) < g.a.user_code_start {
				continue
			}
			if g.cgen_is_top_level_stmt(child_id) {
				stmts << TopLevelStmt{
					id:     child_id
					file:   file_node.value
					module: if module_name.len == 0 { 'main' } else { module_name }
				}
			}
		}
	}
	return stmts
}

fn (g &FlatGen) should_emit_top_level_file(file_idx int, file_node flat.Node) bool {
	if file_idx < g.a.user_code_start || file_node.kind != .file || file_node.children_count == 0 {
		return false
	}
	module_name := g.top_level_file_module_name(file_node)
	return module_name.len == 0 || module_name == 'main'
}

fn (g &FlatGen) top_level_file_module_name(file_node flat.Node) string {
	for i in 0 .. file_node.children_count {
		child := g.a.child_node(&file_node, i)
		if child.kind == .module_decl {
			return child.value
		}
	}
	return ''
}

fn (g &FlatGen) cgen_is_top_level_stmt(id flat.NodeId) bool {
	if int(id) < 0 {
		return false
	}
	node := g.a.nodes[int(id)]
	return match node.kind {
		.expr_stmt, .assign, .decl_assign, .selector_assign, .index_assign, .for_stmt,
		.for_in_stmt, .if_expr, .assert_stmt, .defer_stmt {
			true
		}
		.block, .comptime_if {
			for i in 0 .. node.children_count {
				if g.cgen_is_top_level_stmt(g.a.child(&node, i)) {
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

// should_emit_fn_node reports whether should emit fn node applies in c.
fn (mut g FlatGen) should_emit_fn_node(node flat.Node, node_index int) bool {
	return g.should_emit_fn_node_in_module(node, node_index, g.tc.cur_module, g.tc.cur_file)
}

// should_emit_fn_node_in_module reports whether should emit fn node in module applies in c.
fn (mut g FlatGen) should_emit_fn_node_in_module(node flat.Node, node_index int, module_name string, file_name string) bool {
	qfn := g.qualified_fn_name_in_module_c(module_name, node.value)
	if g.should_rename_user_main_for_tests(module_name, node.value) {
		return true
	}
	if module_name == 'builtin' && node.value == 'exit' {
		return true
	}
	if module_name == 'sync' && g.needs_shared_runtime
		&& node.value in ['cpanic', 'cpanic_errno', 'should_be_zero', 'RwMutex.init', 'RwMutex.lazy_init', 'RwMutex.lock', 'RwMutex.unlock', 'RwMutex.rlock', 'RwMutex.runlock'] {
		return true
	}
	// `array.pointers` is emitted as an intrinsic at call sites; the raw
	// builtin body has an erased `array` receiver and generates invalid C.
	if module_name == 'builtin' && node.value == 'array.pointers' {
		return false
	}
	// `&u8.vbytes` call sites are canonicalized to `byteptr.vbytes`; emitting the
	// raw helper would duplicate the ABI under a misleading `u8__vbytes` name.
	if module_name == 'builtin' && node.value == 'u8.vbytes' {
		return false
	}
	if node.value.starts_with('__anon_fn_') || qfn.contains('__anon_fn_') {
		return true
	}
	if node.generic_params.len == 0 && cgen_is_operator_overload_fn(node.value)
		&& g.test_files[file_name] {
		return true
	}
	if g.should_emit_ierror_method(node.value, qfn) {
		return true
	}
	// Every specialization materialized from the combined program/module-cache
	// graph is a concrete body needed by either main or one of the cached objects.
	if g.is_program_specialization_fn_node(node, node_index, module_name) {
		return true
	}
	if g.has_used_fn_filter() && !g.used_fn_contains_in_module(node.value, module_name) {
		return false
	}
	return true
}

fn (g &FlatGen) is_program_specialization_fn_node(node flat.Node, node_index int, module_name string) bool {
	if g.a.specialized_fn_nodes[node_index] {
		return true
	}
	qfn := g.qualified_fn_name_in_module_c(module_name, node.value)
	return node.value in g.tc.specialized_generic_fns || qfn in g.tc.specialized_generic_fns
		|| g.cname(node.value) in g.tc.specialized_generic_fns
		|| g.cname(qfn) in g.tc.specialized_generic_fns
}

fn cgen_is_operator_overload_fn(name string) bool {
	if !name.contains('.') {
		return false
	}
	method := name.all_after_last('.')
	return method in ['+', '-', '*', '/', '%', '==', '!=', '<', '>', '<=', '>=']
}

fn is_generated_fn_after_markused(name string) bool {
	return name.starts_with('__anon_fn_')
}

// used_fn_contains reports whether used fn contains applies in c.
fn (g &FlatGen) used_fn_contains(name string) bool {
	if name.len == 0 {
		return false
	}
	return g.used_fns[name]
}

fn (g &FlatGen) used_fn_contains_in_module(name string, module_name string) bool {
	if module_name.len > 0 && module_name != 'main' && module_name != 'builtin'
		&& name.starts_with('${module_name}.') {
		if g.used_fn_contains(name) {
			return true
		}
		cfn := g.cname(name)
		if cfn != name && g.used_fn_contains(cfn) {
			return true
		}
	}
	dfn := dotted_fn_name_in_module(module_name, name)
	qfn := g.qualified_fn_name_in_module_c(module_name, name)
	if g.used_fn_contains(dfn) || g.used_fn_contains(qfn) {
		return true
	}
	if g.used_fn_contains(g.cname(dfn)) || g.used_fn_contains(g.cname(qfn)) {
		return true
	}
	if module_name.len == 0 || module_name == 'main' || module_name == 'builtin' {
		cfn := g.cname(name)
		return g.used_fn_contains(name) || g.used_fn_contains(cfn)
	}
	return false
}

// has_used_fn_filter reports whether has used fn filter applies in c.
fn (g &FlatGen) has_used_fn_filter() bool {
	return g.used_fns.len > 0 && g.used_fn_contains('main')
}

// emitted_fn_contains reports whether emitted fn contains applies in c.
fn (g &FlatGen) emitted_fn_contains(name string) bool {
	return name.len > 0 && g.emitted_fns[name]
}

fn generic_method_candidate_key(receiver string, method string) string {
	return '${receiver}\n${method}'
}

fn (mut g FlatGen) precompute_generic_method_candidate_index() {
	g.generic_method_candidates.clear()
	for name, ret in g.tc.fn_ret_types {
		if !name.contains('[') || !name.contains('.') {
			continue
		}
		method := name.all_after_last('.')
		receiver := name.all_before_last('.')
		if method.len == 0 || !receiver.contains('[') {
			continue
		}
		base_receiver := receiver.all_before('[')
		if base_receiver.len == 0 {
			continue
		}
		candidate := GenericMethodCandidate{
			name:   name
			ret:    ret
			params: g.tc.fn_param_types[name] or { []types.Type{} }
		}
		g.add_generic_method_candidate(base_receiver, method, candidate)
		short_receiver := base_receiver.all_after_last('.')
		if short_receiver != base_receiver {
			g.add_generic_method_candidate(short_receiver, method, candidate)
		}
	}
}

fn (mut g FlatGen) add_generic_method_candidate(receiver string, method string, candidate GenericMethodCandidate) {
	key := generic_method_candidate_key(receiver, method)
	mut candidates := g.generic_method_candidates[key] or { []GenericMethodCandidate{} }
	candidates << candidate
	g.generic_method_candidates[key] = candidates
}

// qualified_fn_name supports qualified fn name handling for FlatGen.
fn (g &FlatGen) qualified_fn_name(name string) string {
	return g.qualified_fn_name_in_module_c(g.tc.cur_module, name)
}

fn (g &FlatGen) export_fn_name_in_module(module_name string, name string) ?string {
	qname := dotted_fn_name_in_module(module_name, name)
	if export_name := g.a.export_fn_names[qname] {
		return export_name
	}
	if (module_name.len == 0 || module_name == 'main' || module_name == 'builtin')
		&& name in g.a.export_fn_names {
		return g.a.export_fn_names[name]
	}
	return none
}

// qualified_fn_name_in_module supports qualified fn name in module handling for c.
// qualified_fn_name_in_module_c is the memoizing FlatGen variant of
// qualified_fn_name_in_module (the c_name cache absorbs the sanitize cost;
// asked ~46k times per build on the call-emission path).
fn (g &FlatGen) qualified_fn_name_in_module_c(module_name string, name string) string {
	if module_name == 'builtin' && name == 'free' {
		return 'v_free'
	}
	if name.starts_with('__v3_sum_eq_') {
		return g.cname(name)
	}
	if module_name.len > 0 && module_name != 'main' && module_name != 'builtin' {
		return g.cname('${module_name}.${name}')
	}
	if name == 'free' {
		return 'v_free'
	}
	return g.cname(name)
}

fn qualified_fn_name_in_module(module_name string, name string) string {
	if module_name == 'builtin' && name == 'free' {
		return 'v_free'
	}
	if name.starts_with('__v3_sum_eq_') {
		return c_name(name)
	}
	if module_name.len > 0 && module_name != 'main' && module_name != 'builtin' {
		return c_name('${module_name}.${name}')
	}
	if name == 'free' {
		return 'v_free'
	}
	return c_name(name)
}

fn is_main_fn_in_main_module(module_name string, name string) bool {
	return name == 'main' && (module_name.len == 0 || module_name == 'main')
}

fn (g &FlatGen) should_rename_user_main_for_tests(module_name string, name string) bool {
	return g.test_files.len > 0 && is_main_fn_in_main_module(module_name, name)
}

fn (g &FlatGen) fn_c_name_in_module(module_name string, name string) string {
	if g.should_rename_user_main_for_tests(module_name, name) {
		return g.test_user_main_c_name()
	}
	return g.qualified_fn_name_in_module_c(module_name, name)
}

fn (g &FlatGen) test_user_main_c_name() string {
	base := 'main__user_main'
	if !g.c_fn_symbol_exists(base) {
		return base
	}
	limit := g.a.nodes.len + 2
	for idx in 1 .. limit {
		candidate := '${base}_${idx}'
		if !g.c_fn_symbol_exists(candidate) {
			return candidate
		}
	}
	return '${base}_${limit}'
}

fn (g &FlatGen) c_fn_symbol_exists(candidate string) bool {
	mut cur_module := ''
	for node in g.a.nodes {
		kind_id := node_kind_id(node)
		if kind_id == 77 {
			cur_module = ''
			continue
		}
		if kind_id == 73 {
			cur_module = node.value
			continue
		}
		if kind_id != 61 {
			continue
		}
		if g.qualified_fn_name_in_module_c(cur_module, node.value) == candidate {
			return true
		}
		if export_name := g.export_fn_name_in_module(cur_module, node.value) {
			if export_name == candidate {
				return true
			}
		}
	}
	return false
}

// direct_call_name supports direct call name handling for FlatGen.
fn (mut g FlatGen) direct_call_name(name string) string {
	if compat_name := g.libc_compat_call_name(name) {
		return compat_name
	}
	if g.test_files.len > 0 && (name == 'main' || name == 'main.main') {
		return g.test_user_main_c_name()
	}
	if name == 'free' {
		return 'v_free'
	}
	if name == 'int_str' {
		return 'int__str'
	}
	if name == 'bool_str' {
		return 'bool__str'
	}
	if name == 'char.vstring' {
		return 'charptr__vstring'
	}
	if name == 'char.vstring_with_len' {
		return 'charptr__vstring_with_len'
	}
	return g.cname(name)
}

fn (mut g FlatGen) direct_call_name_for_call(id flat.NodeId, name string) string {
	if g.test_files.len > 0 && (name == 'main' || name == 'main.main') {
		if resolved := g.tc.resolved_call_name(id) {
			if resolved == 'main' || resolved == 'main.main' {
				return g.test_user_main_c_name()
			}
		}
		return g.cname(name)
	}
	if alias := g.flattened_generic_method_short_alias(name) {
		return g.cname(alias)
	}
	if specialized := g.specialized_generic_method_name_for_call_with_arg_count(id, name, -1) {
		return g.cname(specialized)
	}
	return g.direct_call_name(name)
}

fn (mut g FlatGen) direct_call_name_for_call_node(id flat.NodeId, node flat.Node, name string) string {
	if specialized := g.specialized_generic_method_name_for_call_args(node, name,
		int(node.children_count) - 1)
	{
		return g.cname(specialized)
	}
	return g.direct_call_name_for_call(id, name)
}

fn (g &FlatGen) flattened_generic_method_short_alias(name string) ?string {
	if !name.contains('.') {
		return none
	}
	receiver := name.all_before_last('.')
	method := name.all_after_last('.')
	if receiver.len == 0 || method.len == 0 {
		return none
	}
	for short_receiver in cgen_flattened_generic_receiver_short_variants(receiver) {
		candidate := '${short_receiver}.${method}'
		if candidate in g.tc.fn_param_types || candidate in g.tc.fn_ret_types {
			return candidate
		}
	}
	return none
}

fn (mut g FlatGen) libc_compat_call_name(name string) ?string {
	if name.starts_with('C.') {
		cfn := g.cname(name)
		wide_cfn := c_winapi_wide_export_name(cfn)
		if wide_cfn != cfn {
			return wide_cfn
		}
	}
	// `builtin.v_gettid()` reaches libc through `C.gettid()` on Linux/glibc, but
	// that symbol is not declared by all usable C header sets. Route it through a
	// tiny runtime compatibility helper instead of emitting an undeclared call.
	if name == 'C.gettid' {
		g.libc_compat_fns['gettid'] = true
		return 'v3_gettid'
	}
	if name in ['C.v_filelock_lock', 'C.v_filelock_unlock'] {
		g.libc_compat_fns['filelock'] = true
		return g.cname(name)
	}
	return none
}

fn (mut g FlatGen) preseed_libc_compat_fns() {
	refs := g.c_extern_referenced_symbols()
	if refs['C.gettid'] || refs['gettid'] {
		g.libc_compat_fns['gettid'] = true
	}
	if refs['C.v_filelock_lock'] || refs['C.v_filelock_unlock'] || refs['v_filelock_lock']
		|| refs['v_filelock_unlock'] {
		g.libc_compat_fns['filelock'] = true
	}
}

fn (g &FlatGen) test_user_main_fn_value_c_name(id flat.NodeId, node flat.Node) ?string {
	if g.test_files.len == 0 || node.kind != .ident || node.value != 'main' {
		return none
	}
	looked_up := g.tc.cur_scope.lookup(node.value) or { types.Type(types.void_) }
	if looked_up !is types.Void {
		return none
	}
	if resolved := g.tc.resolved_fn_value_name(id) {
		if resolved == 'main' || resolved == 'main.main' {
			return g.test_user_main_c_name()
		}
		return none
	}
	if g.usable_expr_type(id) is types.FnType {
		return g.test_user_main_c_name()
	}
	return none
}

fn (g &FlatGen) import_alias_module(alias string) ?string {
	if alias.len == 0 {
		return none
	}
	if g.tc != unsafe { nil } && g.tc.cur_file.len > 0 {
		key := g.tc.cur_file + '\n' + alias
		if mod := g.tc.file_imports[key] {
			return mod
		}
	}
	if mod := g.modules[alias] {
		if !mod.contains('.') && mod == alias {
			return none
		}
		return mod
	}
	return none
}

fn (g &FlatGen) selector_base_module(name string) ?string {
	if name.len == 0 {
		return none
	}
	if g.tc != unsafe { nil } && g.tc.cur_file.len > 0 {
		key := g.tc.cur_file + '\n' + name
		if mod := g.tc.file_imports[key] {
			return mod
		}
	}
	if mod := g.modules[name] {
		return mod
	}
	if mod := g.tc.imports[name] {
		return mod
	}
	return none
}

fn (g &FlatGen) selector_base_is_module(name string) bool {
	if _ := g.selector_base_module(name) {
		return true
	}
	return false
}

fn (g &FlatGen) selector_base_is_value(name string) bool {
	if name.len == 0 {
		return false
	}
	if g.tc != unsafe { nil } && g.tc.cur_scope != unsafe { nil } {
		if typ := g.tc.cur_scope.lookup(name) {
			if typ !is types.Void {
				return true
			}
		}
	}
	if _ := g.current_param_type(name) {
		return true
	}
	if _ := g.global_type_for_ident(name) {
		return true
	}
	return false
}

fn (g &FlatGen) has_import_alias(alias string) bool {
	if _ := g.import_alias_module(alias) {
		return true
	}
	return false
}

// dotted_fn_name supports dotted fn name handling for FlatGen.
fn (g &FlatGen) dotted_fn_name(name string) string {
	return dotted_fn_name_in_module(g.tc.cur_module, name)
}

// dotted_fn_name_in_module supports dotted fn name in module handling for c.
fn dotted_fn_name_in_module(module_name string, name string) string {
	if module_name.len > 0 && module_name != 'main' && module_name != 'builtin' {
		return '${module_name}.${name}'
	}
	return name
}

// qualify_name_in_module supports qualify name in module handling for c.
fn qualify_name_in_module(module_name string, name string) string {
	if module_name.len == 0 || module_name == 'main' || module_name == 'builtin' {
		return name
	}
	if name.contains('.') {
		return name
	}
	return '${module_name}.${name}'
}

// gen_fn emits fn output for c.
fn (mut g FlatGen) gen_fn(node flat.Node) {
	g.gen_fn_in_module(node, g.tc.cur_module)
}

fn (mut g FlatGen) write_method_c_name(id flat.NodeId, node flat.Node, method_name string) {
	g.write(g.cname(g.method_call_name_for_call(id, node, method_name)))
}

fn (mut g FlatGen) gen_channel_close_call(base_id flat.NodeId, node flat.Node) {
	g.write('sync__Channel__close(')
	if g.channel_close_receiver_needs_deref(base_id) {
		g.write('*(')
		g.gen_expr(base_id)
		g.write(')')
	} else {
		g.write('(sync__Channel*)(')
		g.gen_expr(base_id)
		g.write(')')
	}
	g.write(', ')
	g.gen_channel_close_errors(node)
	g.write(')')
}

fn (mut g FlatGen) gen_channel_close_errors(node flat.Node) {
	if node.children_count <= 1 {
		g.write('array_new(sizeof(IError), 0, 0)')
		return
	}
	count := node.children_count - 1
	ierror_type := g.tc.parse_type('IError')
	g.write('new_array_from_c_array(${count}, ${count}, sizeof(IError), (IError[]){')
	for i in 1 .. node.children_count {
		if i > 1 {
			g.write(', ')
		}
		g.gen_expr_with_expected_type(g.a.child(&node, i), ierror_type)
	}
	g.write('})')
}

fn (mut g FlatGen) gen_channel_try_call(node flat.Node, fn_node flat.Node) bool {
	if fn_node.kind != .selector || fn_node.children_count == 0
		|| fn_node.value !in ['try_push', 'try_pop'] || node.children_count < 2 {
		return false
	}
	base_id := g.a.child(&fn_node, 0)
	base_type := concrete_receiver_type(g.usable_expr_type(base_id))
	if base_type !is types.Channel {
		return false
	}
	channel_type := base_type as types.Channel
	arg_id := g.a.child(&node, 1)
	if fn_node.value == 'try_push' {
		elem_ct := g.value_c_type(channel_type.elem_type)
		if fixed := array_fixed_type(channel_type.elem_type) {
			tmp := g.tmp_count
			g.tmp_count++
			tmp_name := '_try_push_${tmp}'
			src := g.fixed_array_copy_source_string(arg_id, types.Type(fixed))
			g.write('({ ${elem_ct} ${tmp_name}; memmove(${tmp_name}, ${src}, sizeof(${tmp_name})); sync__Channel__try_push(')
			g.gen_channel_try_receiver(base_id)
			g.write(', &${tmp_name}); })')
			return true
		}
		g.write('sync__Channel__try_push(')
		g.gen_channel_try_receiver(base_id)
		g.write(', &(${elem_ct}[]){')
		g.gen_expr_with_expected_type(arg_id, channel_type.elem_type)
		g.write('})')
		return true
	}
	g.write('sync__Channel__try_pop(')
	g.gen_channel_try_receiver(base_id)
	g.write(', ')
	g.gen_channel_try_pop_arg(arg_id)
	g.write(')')
	return true
}

fn (mut g FlatGen) gen_channel_try_receiver(base_id flat.NodeId) {
	if g.channel_close_receiver_needs_deref(base_id) {
		g.write('*(')
		g.gen_expr(base_id)
		g.write(')')
		return
	}
	g.gen_expr(base_id)
}

fn (mut g FlatGen) gen_channel_try_pop_arg(arg_id flat.NodeId) {
	arg_node := g.a.nodes[int(arg_id)]
	if arg_node.kind == .prefix && arg_node.op == .amp {
		g.gen_expr(arg_id)
		return
	}
	if g.usable_expr_type(arg_id) is types.Pointer {
		g.gen_expr(arg_id)
		return
	}
	g.write('&')
	g.gen_expr(arg_id)
}

fn (mut g FlatGen) gen_compiler_default_free_call(fn_node flat.Node, resolved_target_name string) bool {
	if fn_node.kind != .selector || fn_node.value != 'free' || fn_node.children_count == 0 {
		return false
	}
	if resolved_target_name.len > 0 && resolved_target_name !in ['free', 'builtin.free'] {
		return false
	}
	base_id := g.a.child(&fn_node, 0)
	base_type := g.usable_expr_type(base_id)
	if base_type is types.Void || base_type is types.Unknown {
		return false
	}
	if resolved_target_name in ['free', 'builtin.free'] && cgen_type_is_pointer_like(base_type) {
		g.write('free(')
		g.gen_expr(base_id)
		g.write(')')
		return true
	}
	clean := concrete_receiver_type(base_type)
	if _ := array_like_type(clean) {
		return false
	}
	if clean is types.Map || clean is types.String {
		return false
	}
	if g.receiver_has_method(base_type, 'free') {
		return false
	}
	g.write('((void)0)')
	return true
}

fn cgen_type_is_pointer_like(t types.Type) bool {
	if t is types.Pointer {
		return true
	}
	if t is types.Alias {
		if t.name in ['charptr', 'byteptr', 'voidptr'] {
			return true
		}
		return cgen_type_is_pointer_like(t.base_type)
	}
	return false
}

fn (g &FlatGen) receiver_has_method(base_type types.Type, method string) bool {
	mut names := []string{}
	raw := types.unwrap_pointer(base_type)
	if raw_name := receiver_method_type_name(raw) {
		names << raw_name
	}
	clean := concrete_receiver_type(base_type)
	if clean_name := receiver_method_type_name(clean) {
		if clean_name !in names {
			names << clean_name
		}
	}
	for name in names {
		if g.receiver_method_registered(name, method) {
			return true
		}
		for alias, target in g.tc.type_aliases {
			if target == name {
				alias_method := '${alias}.${method}'
				if g.fn_key_registered(alias_method) {
					return true
				}
			}
		}
	}
	return false
}

fn (g &FlatGen) receiver_method_registered(type_name string, method string) bool {
	if type_name.len == 0 {
		return false
	}
	mut receivers := []string{}
	for receiver in [type_name, type_name.all_after_last('.'),
		g.tc.qualify_name(type_name)] {
		if receiver.len > 0 && receiver !in receivers {
			receivers << receiver
		}
	}
	for receiver in receivers {
		if resolved := g.tc.concrete_method_signature_key(receiver, method) {
			if g.fn_key_registered(resolved) {
				return true
			}
		}
		resolved := g.resolve_method_name(receiver, method)
		if resolved.len > 0 && g.fn_key_registered(resolved) {
			return true
		}
		if g.fn_key_registered('${receiver}.${method}') {
			return true
		}
	}
	return false
}

fn (g &FlatGen) fn_key_registered(name string) bool {
	if name.len == 0 {
		return false
	}
	if name in g.tc.fn_param_types || name in g.tc.fn_ret_types || name in g.fn_decl_param_types
		|| name in g.fn_decl_ret_types || name in g.fn_decl_mut_receivers {
		return true
	}
	cname := g.cname(name)
	return cname in g.tc.fn_param_types || cname in g.tc.fn_ret_types
		|| cname in g.fn_decl_param_types || cname in g.fn_decl_ret_types
		|| cname in g.fn_decl_mut_receivers
}

fn receiver_method_type_name(t types.Type) ?string {
	name := t.name()
	if name.len == 0 {
		return none
	}
	return name
}

fn (g &FlatGen) channel_close_receiver_needs_deref(base_id flat.NodeId) bool {
	base_type := g.tc.resolve_type(base_id)
	return base_type is types.Pointer
		&& cgen_is_channel_close_receiver_type(types.unwrap_pointer(base_type))
}

fn (g &FlatGen) method_call_name_for_call(id flat.NodeId, node flat.Node, method_name string) string {
	if concrete := g.concrete_generic_method_name_from_call_receiver(node, method_name) {
		return concrete
	}
	if specialized := g.specialized_generic_method_name_for_call_args(node, method_name,
		int(node.children_count))
	{
		return specialized
	}
	if specialized := g.specialized_generic_method_name_for_call_with_arg_count(id, method_name,
		int(node.children_count))
	{
		return specialized
	}
	return method_name
}

fn (g &FlatGen) concrete_generic_method_name_from_call_receiver(node flat.Node, method_name string) ?string {
	if !method_name.contains('.') || node.children_count == 0 {
		return none
	}
	fn_node := g.a.child_node(&node, 0)
	if fn_node.kind != .selector || fn_node.children_count == 0 {
		return none
	}
	method := method_name.all_after_last('.')
	if method.len == 0 {
		return none
	}
	receiver_type := types.unwrap_pointer(g.usable_expr_type(g.a.child(fn_node, 0)))
	receiver_name := receiver_type.name()
	if receiver_name.contains('[') && receiver_name.contains(']') {
		if resolved := g.resolve_concrete_generic_method_name(receiver_name, method) {
			return resolved
		}
	}
	for receiver in cgen_flattened_generic_receiver_short_variants(receiver_name) {
		candidate := '${receiver}.${method}'
		if candidate in g.tc.fn_param_types || candidate in g.tc.fn_ret_types {
			return candidate
		}
	}
	if receiver_name.contains('_') {
		return g.method_name_by_receiver_param_type(receiver_type, method)
	}
	return none
}

fn (g &FlatGen) method_name_by_receiver_param_type(receiver_type types.Type, method string) ?string {
	clean_receiver := concrete_receiver_type(receiver_type)
	receiver_ct := g.tc.c_type(clean_receiver)
	mut candidates := []string{}
	for name, params in g.fn_decl_param_types {
		if !name.contains('.') || name.contains('__') || name.all_after_last('.') != method
			|| params.len == 0 {
			continue
		}
		param_receiver := concrete_receiver_type(params[0])
		if g.tc.c_type(param_receiver) != receiver_ct
			&& !g.type_names_match(param_receiver, clean_receiver) {
			continue
		}
		candidates << name
	}
	if candidates.len == 0 {
		return none
	}
	candidates.sort()
	mut best := candidates[0]
	mut best_score := g.receiver_param_method_candidate_score(best)
	for candidate in candidates[1..] {
		score := g.receiver_param_method_candidate_score(candidate)
		if score > best_score {
			best = candidate
			best_score = score
		}
	}
	return best
}

fn (g &FlatGen) receiver_param_method_candidate_score(name string) int {
	mut score := 0
	if g.tc.cur_module.len > 0 && g.tc.cur_module != 'main' && g.tc.cur_module != 'builtin'
		&& name.starts_with('${g.tc.cur_module}.') {
		score += 100
	}
	if name.contains('_') {
		score += 10
	}
	if !name.contains('[') {
		score += 5
	}
	return score
}

fn (g &FlatGen) specialized_generic_method_name_for_call_with_arg_count(id flat.NodeId, method_name string, arg_count int) ?string {
	if !method_name.contains('.') {
		return none
	}
	receiver := method_name.all_before_last('.')
	method := method_name.all_after_last('.')
	if receiver.len == 0 || method.len == 0 {
		return none
	}
	call_ret := g.call_default_return_type(id)
	if type_arg := generic_method_type_arg_from_return(call_ret) {
		for arg in [type_arg, type_arg.all_after_last('.')] {
			for candidate in ['${receiver}[${arg}].${method}',
				'${receiver.all_after_last('.')}[${arg}].${method}'] {
				if candidate in g.tc.fn_param_types || candidate in g.tc.fn_ret_types {
					return candidate
				}
			}
		}
	}
	for lookup_receiver in [receiver, receiver.all_after_last('.')] {
		candidates := g.generic_method_candidates[generic_method_candidate_key(lookup_receiver,
			method)] or { continue }
		for candidate in candidates {
			if arg_count >= 0 && candidate.params.len != arg_count {
				continue
			}
			if g.type_names_match(call_ret, candidate.ret)
				|| call_ret.name() == candidate.ret.name() {
				return candidate.name
			}
		}
		if lookup_receiver == receiver.all_after_last('.') {
			break
		}
	}
	return none
}

fn (g &FlatGen) specialized_generic_method_name_for_call_args(node flat.Node, method_name string, arg_count int) ?string {
	if !method_name.contains('.') {
		return none
	}
	receiver := method_name.all_before_last('.')
	method := method_name.all_after_last('.')
	if receiver.len == 0 || method.len == 0 {
		return none
	}
	mut best := ''
	mut best_score := 0
	mut best_preference := -1
	mut ambiguous := false
	for lookup_receiver in [receiver, receiver.all_after_last('.')] {
		candidates := g.generic_method_candidates[generic_method_candidate_key(lookup_receiver,
			method)] or { continue }
		for candidate in candidates {
			if arg_count >= 0 && candidate.params.len != arg_count {
				continue
			}
			score := g.generic_method_candidate_arg_score(node, candidate)
			if score <= 0 {
				continue
			}
			preference := generic_method_candidate_receiver_preference(candidate.name, receiver)
			if score > best_score || (score == best_score && preference > best_preference) {
				best = candidate.name
				best_score = score
				best_preference = preference
				ambiguous = false
			} else if score == best_score && preference == best_preference && candidate.name != best {
				ambiguous = true
			}
		}
		if lookup_receiver == receiver.all_after_last('.') {
			break
		}
	}
	if best_score > 0 && !ambiguous {
		return best
	}
	return none
}

fn generic_method_candidate_receiver_preference(candidate_name string, receiver string) int {
	candidate_receiver := candidate_name.all_before_last('.')
	if candidate_receiver.len == 0 {
		return 0
	}
	if candidate_receiver == receiver {
		return 30
	}
	base, _, ok := shared_generic_app_parts(candidate_receiver)
	if ok {
		if base == receiver {
			return 25
		}
		if base == receiver.all_after_last('.') {
			return 15
		}
	}
	if candidate_receiver == receiver.all_after_last('.') {
		return 10
	}
	return 0
}

fn (g &FlatGen) generic_method_candidate_arg_score(node flat.Node, candidate GenericMethodCandidate) int {
	params := candidate.params
	explicit_start := g.generic_method_candidate_explicit_arg_start(node, params.len) or {
		return -1
	}
	mut score := 0
	// The receiver usually does not carry method-level generic arguments. Concrete
	// generic receivers are handled before this fallback.
	for i in 1 .. params.len {
		arg_child_idx := explicit_start + i - 1
		if arg_child_idx >= int(node.children_count) {
			return -1
		}
		arg_id := g.a.child(&node, arg_child_idx)
		arg_node := g.a.nodes[int(arg_id)]
		arg_type := g.const_type_for_arg_node(arg_node) or { g.usable_expr_type(arg_id) }
		arg_score := g.generic_method_param_arg_score(params[i], arg_type, arg_node)
		if arg_score < 0 {
			return -1
		}
		score += arg_score
	}
	score += g.generic_method_candidate_type_arg_score(node, candidate.name, explicit_start)
	return score
}

fn (g &FlatGen) generic_method_candidate_explicit_arg_start(node flat.Node, params_len int) ?int {
	if node.children_count == 0 || params_len == 0 {
		return none
	}
	if params_len == int(node.children_count) {
		return 1
	}
	if params_len == int(node.children_count) - 1 {
		return 2
	}
	return none
}

fn (g &FlatGen) generic_method_candidate_type_arg_score(node flat.Node, candidate_name string, explicit_start int) int {
	if !candidate_name.contains('.') {
		return 0
	}
	receiver := candidate_name.all_before_last('.')
	_, type_args, ok := shared_generic_app_parts(receiver)
	if !ok || type_args.len == 0 {
		return 0
	}
	mut score := 0
	for i in explicit_start .. node.children_count {
		arg_id := g.a.child(&node, i)
		arg_node := g.a.nodes[int(arg_id)]
		actual := types.unwrap_pointer(g.const_type_for_arg_node(arg_node) or {
			g.usable_expr_type(arg_id)
		})
		for type_arg in type_args {
			if g.generic_method_type_arg_matches_actual(type_arg, actual) {
				score += 30
			}
		}
	}
	return score
}

fn (g &FlatGen) generic_method_type_arg_matches_actual(type_arg string, actual types.Type) bool {
	clean := trimmed_space(type_arg)
	if clean.len == 0 || actual is types.Unknown || actual is types.Void {
		return false
	}
	expected := g.tc.parse_type(clean)
	if !decl_annotation_is_unusable(expected, clean)
		&& g.generic_method_arg_types_match(actual, expected) {
		return true
	}
	actual_name := actual.name()
	return actual_name == clean || actual_name.all_after_last('.') == clean.all_after_last('.')
}

fn (g &FlatGen) generic_method_param_arg_score(param types.Type, actual types.Type, arg_node flat.Node) int {
	if param is types.Unknown || actual is types.Unknown || param is types.Void
		|| actual is types.Void {
		return 0
	}
	if g.generic_method_arg_types_match(actual, param) {
		return 20
	}
	if param is types.Pointer {
		if actual is types.Pointer {
			if g.generic_method_arg_types_match(actual.base_type, param.base_type) {
				return 20
			}
		} else if g.generic_method_arg_types_match(actual, param.base_type) {
			return 20
		}
	}
	if actual is types.Pointer && g.generic_method_arg_types_match(actual.base_type, param) {
		return 12
	}
	clean_param := types.unwrap_pointer(param)
	if (arg_node.kind == .int_literal || arg_node.kind == .float_literal)
		&& g.types_numeric_compatible(actual, clean_param) {
		return 1
	}
	if actual is types.Primitive && clean_param is types.Primitive
		&& g.types_numeric_compatible(actual, clean_param) {
		return 1
	}
	return 0
}

fn (g &FlatGen) generic_method_arg_types_match(actual types.Type, expected types.Type) bool {
	if g.type_names_match(actual, expected) {
		return true
	}
	if actual is types.Alias && g.generic_method_arg_types_match(actual.base_type, expected) {
		return true
	}
	if expected is types.Alias && g.generic_method_arg_types_match(actual, expected.base_type) {
		return true
	}
	return false
}

fn generic_method_type_arg_from_return(ret types.Type) ?string {
	if ret is types.ResultType {
		name := ret.base_type.name()
		if name.len > 0 && name != 'void' {
			return name
		}
	}
	if ret is types.OptionType {
		name := ret.base_type.name()
		if name.len > 0 && name != 'void' {
			return name
		}
	}
	return none
}

// static_method_fn_name resolves `Type.method(...)` static calls where `Type` is a
// named type, struct, enum, sum type or type alias (e.g. `SimdFloat4.new` for
// `type SimdFloat4 = vec.Vec4[f32]`). Returns the fn key, or none if `type_ident`
// is not a type or has no such static method.
fn (g &FlatGen) static_method_fn_name(type_ident string, method string) ?string {
	qtype := g.tc.qualify_name(type_ident)
	is_type := type_ident in g.tc.type_aliases || qtype in g.tc.type_aliases
		|| type_ident in g.tc.structs || qtype in g.tc.structs || type_ident in g.tc.enum_names
		|| qtype in g.tc.enum_names || type_ident in g.tc.sum_types || qtype in g.tc.sum_types
	if !is_type {
		return none
	}
	// Prefer the module-qualified key: a static method defined in the current (or the
	// type's) module is emitted under its qualified C name (`game__Animation__load`),
	// so the call must resolve to the same qualified key even though an unqualified
	// alias (`Animation.load`) may also be registered.
	qdirect := '${qtype}.${method}'
	if qtype != type_ident && (qdirect in g.tc.fn_ret_types || qdirect in g.tc.fn_param_types) {
		return qdirect
	}
	direct := '${type_ident}.${method}'
	if direct in g.tc.fn_ret_types || direct in g.tc.fn_param_types {
		return direct
	}
	if qdirect in g.tc.fn_ret_types || qdirect in g.tc.fn_param_types {
		return qdirect
	}
	return none
}

fn (g &FlatGen) resolve_method_name(type_name string, method string) string {
	direct := '${type_name}.${method}'
	if direct in g.tc.fn_param_types || direct in g.tc.fn_ret_types {
		return direct
	}
	if generic_method := g.resolve_concrete_generic_method_name(type_name, method) {
		return generic_method
	}
	for receiver in cgen_flattened_generic_receiver_short_variants(type_name) {
		candidate := '${receiver}.${method}'
		if candidate in g.tc.fn_param_types || candidate in g.tc.fn_ret_types {
			return candidate
		}
	}
	if g.tc.cur_module.len > 0 && g.tc.cur_module != 'main' && g.tc.cur_module != 'builtin'
		&& !type_name.contains('.') {
		qualified := '${g.tc.cur_module}.${type_name}.${method}'
		if qualified in g.tc.fn_param_types || qualified in g.tc.fn_ret_types {
			return qualified
		}
	}
	return ''
}

fn (g &FlatGen) resolve_concrete_generic_method_name(type_name string, method string) ?string {
	base, args, ok := shared_generic_app_parts(type_name)
	if !ok || args.len == 0 {
		return none
	}
	for suffix in generic_receiver_type_suffix_variants(args) {
		mut candidates := ['${base}_${suffix}.${method}']
		if base.contains('.') {
			base_mod := base.all_before_last('.')
			base_short := base.all_after_last('.')
			candidates << '${base_short}_${suffix}.${method}'
			candidates << '${base_mod}.${base_short}_${suffix}.${method}'
		}
		for candidate in candidates {
			if candidate in g.tc.fn_param_types || candidate in g.tc.fn_ret_types {
				return candidate
			}
		}
	}
	return none
}

fn generic_receiver_type_suffixes(args []string) string {
	variants := generic_receiver_type_suffix_variants(args)
	if variants.len > 0 {
		return variants[0]
	}
	return ''
}

fn generic_receiver_type_suffix_variants(args []string) []string {
	mut raw_parts := []string{cap: args.len}
	mut parts := []string{cap: args.len}
	for arg in args {
		raw := generic_receiver_type_arg_short(arg).replace('[]', 'Array_').replace('&', 'ptr_')
		raw_parts << raw
		parts << c_name(raw)
	}
	mut variants := []string{}
	codegen_push_unique(mut variants, raw_parts.join('_'))
	codegen_push_unique(mut variants, parts.join('_'))
	return variants
}

fn generic_receiver_type_arg_short(type_arg string) string {
	clean := trimmed_space(type_arg)
	if fixed := generic_receiver_fixed_array_type_arg_short(clean) {
		return fixed
	}
	if clean.contains('(') || clean.contains(' ') {
		return sanitize_generic_receiver_type_fragment(clean)
	}
	base, args, ok := shared_generic_app_parts(clean)
	if ok {
		mut parts := [generic_receiver_type_arg_short(base)]
		for arg in args {
			parts << generic_receiver_type_arg_short(arg)
		}
		return parts.join('_')
	}
	if clean.contains('.') {
		return clean.all_after_last('.')
	}
	return clean
}

fn generic_receiver_fixed_array_type_arg_short(type_arg string) ?string {
	clean := trimmed_space(type_arg)
	if !clean.starts_with('[') {
		return none
	}
	close_idx := clean.index_u8(`]`)
	if close_idx <= 1 || close_idx + 1 >= clean.len {
		return none
	}
	len_text := trimmed_space(clean[1..close_idx])
	elem_text := clean[close_idx + 1..].trim_space()
	if len_text.len == 0 || elem_text.len == 0 {
		return none
	}
	elem := generic_receiver_type_arg_short(elem_text)
	return '${elem}_${len_text}'
}

fn sanitize_generic_receiver_type_fragment(typ string) string {
	mut out := []u8{}
	mut prev_us := false
	mut i := 0
	for i < typ.len {
		c := typ[i]
		if (c >= `A` && c <= `Z`) || (c >= `a` && c <= `z`) || (c >= `0` && c <= `9`) {
			out << c
			prev_us = false
			i++
		} else if c == `[` && i + 1 < typ.len && typ[i + 1] == `]` {
			for ch in 'Array_'.bytes() {
				out << ch
			}
			prev_us = false
			i += 2
		} else if c == `&` {
			for ch in 'ptr_'.bytes() {
				out << ch
			}
			prev_us = false
			i++
		} else if c == `.` {
			for out.len > 0 {
				last := out[out.len - 1]
				if (last >= `A` && last <= `Z`) || (last >= `a` && last <= `z`)
					|| (last >= `0` && last <= `9`) {
					out.delete_last()
				} else {
					break
				}
			}
			i++
		} else {
			if !prev_us {
				out << `_`
				prev_us = true
			}
			i++
		}
	}
	mut s := out.bytestr()
	for s.starts_with('_') {
		s = s[1..]
	}
	for s.ends_with('_') {
		s = s[..s.len - 1]
	}
	return s
}

fn cgen_flattened_generic_receiver_short_variants(receiver_type string) []string {
	clean := trimmed_space(receiver_type)
	if clean.len == 0 || !clean.contains('__') || !clean.contains('_') {
		return []string{}
	}
	module_name := if clean.contains('.') { clean.all_before_last('.') } else { '' }
	leaf := if clean.contains('.') { clean.all_after_last('.') } else { clean }
	parts := cgen_flattened_generic_receiver_leaf_parts(leaf)
	mut changed := false
	mut short_parts := []string{cap: parts.len}
	for part in parts {
		if part.contains('__') {
			short_parts << part.all_after_last('__')
			changed = true
		} else {
			short_parts << part
		}
	}
	if !changed {
		return []string{}
	}
	short_leaf := short_parts.join('_')
	mut variants := [short_leaf]
	if module_name.len > 0 {
		variants << '${module_name}.${short_leaf}'
	}
	return variants
}

fn cgen_flattened_generic_receiver_leaf_parts(leaf string) []string {
	mut parts := []string{}
	mut start := 0
	mut i := 0
	for i < leaf.len {
		if leaf[i] == `_` {
			if i + 1 < leaf.len && leaf[i + 1] == `_` {
				i += 2
				continue
			}
			parts << leaf[start..i]
			i++
			start = i
			continue
		}
		i++
	}
	parts << leaf[start..]
	return parts
}

fn c_string_pointer_base_arg(base types.Type) bool {
	clean := if base is types.Alias { base.base_type } else { base }
	if clean is types.Char {
		return true
	}
	return clean is types.Primitive && clean.name() == 'u8'
}

fn (g &FlatGen) c_char_literal_arg(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= g.a.nodes.len {
		return false
	}
	node := g.a.nodes[int(id)]
	if node.kind == .paren && node.children_count > 0 {
		return g.c_char_literal_arg(g.a.child(&node, 0))
	}
	return node.kind == .char_literal && node.value.starts_with('c:')
}

fn (g &FlatGen) c_string_pointer_arg(arg_node flat.Node, expected types.Type) bool {
	if expected !is types.Pointer {
		return false
	}
	if !c_string_pointer_base_arg(types.unwrap_pointer(expected)) {
		return false
	}
	if arg_node.kind == .char_literal {
		return arg_node.value.starts_with('c:')
	}
	if arg_node.kind == .ident {
		const_name := g.const_ref_name(arg_node.value)
		if const_name.len == 0 {
			return false
		}
		if const_id := g.const_vals[const_name] {
			const_node := g.a.nodes[int(const_id)]
			return const_node.kind == .char_literal && const_node.value.starts_with('c:')
		}
	}
	return false
}

fn (g &FlatGen) arg_is_null_pointer_literal(arg_id flat.NodeId, arg_node flat.Node) bool {
	return g.expr_is_nil_value(arg_id)
		|| (arg_node.kind == .int_literal && (arg_node.value == '0' || arg_node.value.len == 0))
		|| (arg_node.kind == .selector && arg_node.value == 'NULL' && arg_node.children_count > 0
		&& g.a.child_node(&arg_node, 0).kind == .ident && g.a.child_node(&arg_node, 0).value == 'C')
}

fn (mut g FlatGen) gen_pointer_builtin_method_call(node flat.Node, fn_node &flat.Node, base_type types.Type) bool {
	receiver := pointer_builtin_receiver_name_for_c(base_type)
	if receiver.len == 0 {
		return false
	}
	method := fn_node.value
	if receiver in ['charptr', 'byteptr'] && method in ['vstring', 'vstring_with_len'] {
		g.write(g.cname('${receiver}.${method}'))
		g.write('(')
		g.gen_expr(g.a.child(fn_node, 0))
		for i in 1 .. node.children_count {
			g.write(', ')
			g.gen_expr(g.a.child(&node, i))
		}
		g.write(')')
		return true
	}
	if receiver in ['byteptr', 'voidptr'] && method == 'vbytes' {
		g.write(g.cname('${receiver}.${method}'))
		g.write('(')
		g.gen_expr(g.a.child(fn_node, 0))
		for i in 1 .. node.children_count {
			g.write(', ')
			g.gen_expr(g.a.child(&node, i))
		}
		g.write(')')
		return true
	}
	return false
}

fn pointer_builtin_receiver_name_for_c(typ types.Type) string {
	if typ is types.Alias {
		if typ.name in ['charptr', 'byteptr', 'voidptr'] {
			return typ.name
		}
		return pointer_builtin_receiver_name_for_c(typ.base_type)
	}
	if typ is types.Pointer {
		base := typ.base_type
		if base is types.Alias {
			if base.name == 'byte' {
				return 'byteptr'
			}
			return pointer_builtin_receiver_name_for_c(base)
		}
		if base is types.Char {
			return 'charptr'
		}
		if base is types.Void {
			return 'voidptr'
		}
		if base is types.Primitive && base.name() == 'u8' {
			return 'byteptr'
		}
	}
	if typ is types.Char {
		return 'charptr'
	}
	if typ is types.Void {
		return 'voidptr'
	}
	if typ is types.Primitive && typ.name() == 'u8' {
		return 'byteptr'
	}
	return ''
}

fn (mut g FlatGen) gen_special_c_callback_arg(fn_name string, arg_idx int, arg_id flat.NodeId, expected_param types.Type) bool {
	clean_name := fn_name.trim_string_left('C.').all_after_last('.')
	if clean_name == 'mbedtls_ssl_conf_sni' && arg_idx == 1 {
		g.write('(int (*)(void *, mbedtls_ssl_context *, const unsigned char *, size_t))')
		g.gen_expr(arg_id)
		return true
	}
	// Only convert to `(void*)` for an actual C-callback slot. A V `fn (...) ...`
	// parameter is generated as a `_fn_ptr_*` typedef and must receive the function
	// pointer directly: `(void*)foo` is an object-pointer-to-function-pointer cast that
	// strict C rejects and that is not portable across ABIs. C functions (and loosely
	// typed `voidptr` slots) still need it, because C uses the header prototype.
	// `fn_type_from` is alias-aware, so a `type Cb = fn ()` parameter is recognised too.
	if _ := fn_type_from(expected_param) {
		return false
	}
	// A V function passed by name to a C function (a callback) must be cast: the V
	// declaration's parameter type (often `voidptr`) is ignored by C in favour of the
	// real header prototype, so the bare name trips -Wincompatible-function-pointer-types.
	// `(void*)` converts cleanly to any function-pointer parameter.
	if int(arg_id) >= 0 {
		arg_node := g.a.nodes[int(arg_id)]
		if arg_node.kind == .ident {
			looked_up := g.tc.cur_scope.lookup(arg_node.value) or { types.Type(types.void_) }
			if looked_up is types.Void {
				call_name := g.call_key(arg_id, arg_node.value)
				fn_key := if call_name in g.tc.fn_ret_types {
					call_name
				} else if arg_node.value in g.tc.fn_ret_types {
					arg_node.value
				} else {
					''
				}
				if fn_key.len > 0 && !fn_key.starts_with('C.') {
					g.write('(void*)')
					g.write(g.cname(fn_key))
					return true
				}
			}
		}
	}
	return false
}

fn (mut g FlatGen) spawn_wrapper_decls() {
	for def in g.spawn_wrapper_defs {
		g.writeln(def)
	}
	if g.spawn_wrapper_defs.len > 0 {
		g.writeln('')
	}
}

// expr_is_addressable reports whether an expression denotes a stable lvalue whose address
// outlives the enclosing statement expression — a variable, a field/index access reaching one,
// or a dereference. Rvalues (struct literals, calls, ...) only have temporary storage, so their
// address must not be captured in a method value's static receiver slot.
fn (g &FlatGen) expr_is_addressable(id flat.NodeId) bool {
	if int(id) < 0 {
		return false
	}
	node := g.a.nodes[int(id)]
	return match node.kind {
		.ident {
			true
		}
		.index {
			node.value != 'range' && node.children_count > 0
				&& g.expr_is_addressable(g.a.child(&node, 0))
		}
		.selector {
			node.children_count > 0 && g.expr_is_addressable(g.a.child(&node, 0))
		}
		.prefix {
			node.op == .mul
		}
		.paren {
			node.children_count > 0 && g.expr_is_addressable(g.a.child(&node, 0))
		}
		else {
			false
		}
	}
}

fn (mut g FlatGen) gen_mut_sum_lvalue_arg(arg_id flat.NodeId, expected types.Type) bool {
	mut lvalue_id := arg_id
	if int(arg_id) >= 0 && int(arg_id) < g.a.nodes.len {
		arg_node := g.a.nodes[int(arg_id)]
		if arg_node.kind == .prefix && arg_node.op == .amp && arg_node.children_count > 0 {
			lvalue_id = g.a.child(&arg_node, 0)
		}
	}
	base0 := if expected is types.Pointer {
		expected.base_type
	} else {
		return false
	}
	base := if base0 is types.Alias { base0.base_type } else { base0 }
	if base !is types.SumType {
		return false
	}
	if !g.expr_is_addressable(lvalue_id) {
		return false
	}
	actual0 := g.tc.resolve_type(lvalue_id)
	if actual0 is types.Pointer {
		return false
	}
	storage0 := if declared := g.selector_declared_type(lvalue_id) { declared } else { actual0 }
	storage := if storage0 is types.Alias { storage0.base_type } else { storage0 }
	if storage !is types.SumType || !g.type_names_match(storage, base) {
		return false
	}
	g.write('&')
	if !g.gen_sum_storage_lvalue_arg(lvalue_id) {
		g.gen_expr(lvalue_id)
	}
	return true
}

fn (mut g FlatGen) gen_sum_storage_lvalue_arg(arg_id flat.NodeId) bool {
	if int(arg_id) < 0 || int(arg_id) >= g.a.nodes.len {
		return false
	}
	node := g.a.nodes[int(arg_id)]
	if node.kind != .selector || node.children_count == 0 {
		return false
	}
	if _ := g.selector_declared_type(arg_id) {
		// handled below
	} else {
		return false
	}
	base_id := g.a.child(&node, 0)
	base := g.a.nodes[int(base_id)]
	needs_paren := base.kind !in [.ident, .selector]
	if needs_paren {
		g.write('(')
	}
	g.gen_expr(base_id)
	if needs_paren {
		g.write(')')
	}
	mut is_ptr := false
	if base.kind == .ident {
		if typ := g.tc.cur_scope.lookup(base.value) {
			is_ptr = typ is types.Pointer
		}
	} else if base.kind == .selector {
		if declared := g.selector_declared_type(base_id) {
			is_ptr = declared is types.Pointer
		} else {
			resolved := g.tc.resolve_type(base_id)
			is_ptr = resolved is types.Pointer
		}
	} else {
		resolved := g.tc.resolve_type(base_id)
		is_ptr = resolved is types.Pointer
	}
	if node.op == .arrow || is_ptr {
		g.write('->')
	} else {
		g.write('.')
	}
	g.write(g.cname(node.value))
	return true
}

// gen_method_value_closure handles a method used as a *value* (e.g. `game.draw`
// passed where a `fn ()` callback is expected) rather than called. A plain struct
// field access can't represent the bound receiver, so it binds the receiver into a
// per-site context global and yields a wrapper function that invokes the method on
// it. Returns false when the selector is an ordinary field access (handled normally).
//
// LIMITATION: the captured receiver lives in a single per-site global, so it is
// only valid while no later evaluation of the *same* selector site overwrites it.
// That covers the supported cases — passing/storing a method value and invoking it
// before the site is re-evaluated (immediate callbacks, a single stored callback).
// It does NOT support keeping several live method values from one site with
// different receivers (e.g. building an array of `obj.method` in a loop): they all
// share the global and would observe the last-bound receiver. A correct general
// form needs a real per-closure captured environment, which the v3 backend has no
// ABI for yet (anonymous fns are lifted without true capture). Such escaping method
// values should be reworked (e.g. capture into an explicit struct + free fn) until
// closure support lands.
fn (mut g FlatGen) gen_method_value_closure(base_id flat.NodeId, base_type types.Type, method string) bool {
	clean := types.unwrap_pointer(base_type)
	if clean !is types.Struct {
		return false
	}
	struct_name := (clean as types.Struct).name
	// A real field shadows any same-named method: that's a field access, not a value.
	if _ := g.struct_field_type(struct_name, method) {
		return false
	}
	method_key := g.resolve_method_name(struct_name, method)
	mut params := []types.Type{}
	mut ret := types.Type(types.void_)
	mut cname := ''
	if method_key.len > 0 {
		params = g.tc.fn_param_types[method_key] or { return false }
		ret = g.tc.fn_ret_types[method_key] or { types.Type(types.void_) }
		cname = g.cname(method_key)
	} else if ci := g.tc.generic_method_value_info['${struct_name}.${method}'] {
		// Generic receiver (`Box[int]`): the open `Box[T].get` registration is gone by
		// cgen, so use the substituted params/return the checker stashed, plus the
		// monomorphised C name `g.cname('Box[int].get')` == `Box_int__get`.
		params = ci.params.clone()
		// The receiver param is the un-substituted open `Box[T]`; replace it with the
		// concrete receiver type (keeping the method's pointer-ness) so its C name
		// resolves to `Box_int` rather than the template `Box`.
		if params.len > 0 {
			recv_concrete := types.unwrap_pointer(base_type)
			params[0] = if params[0] is types.Pointer {
				types.Type(types.Pointer{
					base_type: recv_concrete
				})
			} else {
				recv_concrete
			}
		}
		ret = ci.return_type
		cname = g.cname('${struct_name}.${method}')
	} else {
		return false
	}
	if params.len == 0 {
		return false
	}
	recv_is_ptr := params[0] is types.Pointer
	recv_ct := g.tc.c_type(params[0])
	// Use the method's ABI return type (matching `cname`'s signature and the callback
	// fn-pointer typedef): an option/result is `Optional_T`, a fixed array its
	// `_v_ret_*` wrapper — not the bare `c_type` (`Optional`/`Array_fixed_*`).
	ret_ct := g.fn_return_type_name(ret)
	idx := g.tmp_count
	g.tmp_count++
	ctx_name := '_mvctx_${idx}'
	wrap_name := '_mvwrap_${idx}'
	mut wparams := []string{}
	mut call_args := [ctx_name]
	for i in 1 .. params.len {
		pt := g.tc.c_type(params[i])
		wparams << '${pt} a${i}'
		call_args << 'a${i}'
	}
	wparam_str := if wparams.len == 0 { 'void' } else { wparams.join(', ') }
	g.spawn_wrapper_defs << 'static ${recv_ct} ${ctx_name};'
	ret_prefix := if ret_ct == 'void' { '' } else { 'return ' }
	g.spawn_wrapper_defs << 'static ${ret_ct} ${wrap_name}(${wparam_str}) { ${ret_prefix}${cname}(${call_args.join(', ')}); }'
	base_is_ptr := base_type is types.Pointer
	// Set the context global to the receiver, then yield the wrapper as the value.
	if recv_is_ptr && !base_is_ptr && !g.expr_is_addressable(base_id) {
		// Pointer receiver bound to an rvalue base (`Foo{..}.tick`, `make_foo().tick`): taking
		// `&(rvalue)` captures a temporary that dies with the enclosing statement expression,
		// before the callback runs. Copy the receiver into a durable static slot and point at it.
		val_ct := g.tc.c_type(types.unwrap_pointer(params[0]))
		g.spawn_wrapper_defs << 'static ${val_ct} ${ctx_name}_recv;'
		g.write('({ ${ctx_name}_recv = ')
		g.gen_expr(base_id)
		g.write('; ${ctx_name} = &${ctx_name}_recv')
	} else {
		g.write('({ ${ctx_name} = ')
		if recv_is_ptr && !base_is_ptr {
			g.write('&(')
			g.gen_expr(base_id)
			g.write(')')
		} else if !recv_is_ptr && base_is_ptr {
			g.write('*(')
			g.gen_expr(base_id)
			g.write(')')
		} else {
			g.gen_expr(base_id)
		}
	}
	// Yield the wrapper as the callback value. A V `fn (...) ...` parameter is a
	// `_fn_ptr_*` typedef and needs the wrapper cast to that function-pointer type; only
	// a C / `voidptr` callback slot gets the object-pointer `(void*)` cast (which strict
	// C rejects for function pointers). `fn_type_from` is alias-aware, so a method value
	// passed through a `type Cb = fn ()` parameter (an `Alias`, not a bare `FnType`) is
	// recognised too and cast to the function-pointer typedef rather than `(void*)`.
	if fnt := fn_type_from(g.expected_expr_type) {
		fnptr_ct := g.value_c_type(fnt)
		g.write('; (${fnptr_ct})${wrap_name}; })')
	} else {
		g.write('; (void*)${wrap_name}; })')
	}
	return true
}

fn (mut g FlatGen) callback_wrapper_decls() {
	for def in g.callback_wrapper_defs {
		g.writeln(def)
	}
	if g.callback_wrapper_defs.len > 0 {
		g.writeln('')
	}
}

fn (mut g FlatGen) gen_spawn_expr(node flat.Node) {
	if node.children_count == 0 {
		g.write('(void*)0')
		return
	}
	call_id := g.a.child(&node, 0)
	call_node := g.a.nodes[int(call_id)]
	if call_node.kind != .call || call_node.children_count == 0 {
		g.write('(void*)0')
		return
	}
	fn_node := g.a.child_node(&call_node, 0)
	// The spawned call's return type: heap-captured by the wrapper so a later
	// `[]thread T .wait()` can recover the value (void callees just return NULL).
	// Use the callee's ABI return type, not the bare value type: an option/result
	// return is `Optional_T` (not the generic `Optional`, whose payload is `int`) and a
	// fixed-array return is its `_v_ret_*` wrapper struct. The wrapper mallocs and
	// assigns this type, and `[]thread T.wait()` must read back the same layout.
	ret_ct := g.fn_return_type_name(g.tc.resolve_type(call_id))
	mut wrapper := ''
	mut arg_expr := 'NULL'
	if fn_node.kind == .ident {
		call_key := g.call_key(call_id, fn_node.value)
		looked_up := g.tc.cur_scope.lookup(fn_node.value) or { types.Type(types.void_) }
		if fn_type := fn_type_from(looked_up) {
			if fn_type.params.len == int(call_node.children_count) - 1 {
				mut packed_args := []SpawnPackedArg{}
				for i, pt in fn_type.params {
					arg_id := g.a.child(&call_node, i + 1)
					expected_ct := g.spawn_arg_c_type(pt)
					packed_args << g.spawn_packed_arg_for_param(arg_id, pt, expected_ct, i)
				}
				g.emit_fn_value_spawn_expr(call_id, fn_node, fn_type, packed_args, ret_ct)
				return
			}
		}
		cfn := if looked_up !is types.Void && fn_type_from(looked_up) != none {
			g.cname(fn_node.value)
		} else if call_key in g.tc.fn_ret_types || call_key in g.tc.fn_param_types {
			g.direct_call_name_for_call(call_id, call_key)
		} else {
			g.cname(fn_node.value)
		}
		if call_node.children_count == 1 {
			wrapper = g.ensure_noarg_spawn_wrapper(cfn, ret_ct)
		} else {
			// `spawn work(a, b)` packs the arguments into a heap struct so the
			// spawned thread receives them, instead of silently dropping them.
			param_types := g.param_types_for(call_key, fn_node.value)
			if param_types.len > 0 && param_types.len == int(call_node.children_count) - 1 {
				mut packed_args := []SpawnPackedArg{}
				for i, pt in param_types {
					arg_id := g.a.child(&call_node, i + 1)
					packed_args << g.spawn_packed_arg_for_call_param(call_key, arg_id, pt, i)
				}
				g.emit_args_spawn_expr(cfn, packed_args, ret_ct)
				return
			}
		}
	} else if fn_node.kind == .selector && fn_node.children_count > 0 {
		base_id := g.a.child(fn_node, 0)
		base_type := g.receiver_base_type(base_id)
		clean_type := concrete_receiver_type(base_type)
		method_name := g.resolved_method_name_for_spawn(clean_type, fn_node.value)
		if method_name.len > 0 {
			param_types := g.param_types_for(method_name, fn_node.value)
			if param_types.len > 0 {
				receiver_type := param_types[0]
				receiver_ct := g.tc.c_type(receiver_type)
				if call_node.children_count == 1 {
					if receiver_type is types.Pointer {
						if base_type is types.Pointer {
							wrapper = g.ensure_receiver_spawn_wrapper(g.cname(method_name),
								receiver_ct, ret_ct)
							base_expr := g.expr_to_string(base_id)
							arg_expr = '(${receiver_ct})(${base_expr})'
						} else if g.expr_is_addressable(base_id) {
							wrapper = g.ensure_receiver_spawn_wrapper(g.cname(method_name),
								receiver_ct, ret_ct)
							base_expr := g.expr_to_string(base_id)
							arg_expr = '(${receiver_ct})(&(${base_expr}))'
						} else {
							receiver_value := g.spawn_packed_arg_for_call_param(method_name,
								base_id, receiver_type, 0)
							g.emit_args_spawn_expr(g.cname(method_name), [
								receiver_value,
							], ret_ct)
							return
						}
					} else {
						// Casting the void* thread argument straight to a struct type
						// is invalid C, so copy the value receiver into the heap arg
						// struct and dispatch through the argument-packing path.
						receiver_value := g.spawn_packed_arg_for_call_param(method_name, base_id,
							receiver_type, 0)
						g.emit_args_spawn_expr(g.cname(method_name), [receiver_value], ret_ct)
						return
					}
				} else if param_types.len == int(call_node.children_count) {
					// `spawn recv.method(a, b)` packs the receiver and arguments
					// into a heap struct rather than dropping the call.
					receiver_arg := g.spawn_packed_arg_for_call_param(method_name, base_id,
						receiver_type, 0)
					mut packed_args := [receiver_arg]
					for i in 1 .. param_types.len {
						arg_id := g.a.child(&call_node, i)
						packed_args << g.spawn_packed_arg_for_call_param(method_name, arg_id,
							param_types[i], i)
					}
					g.emit_args_spawn_expr(g.cname(method_name), packed_args, ret_ct)
					return
				}
			}
		}
	} else if fn_type := fn_type_from(g.tc.resolve_type(g.a.child(&call_node, 0))) {
		if fn_type.params.len == int(call_node.children_count) - 1 {
			mut packed_args := []SpawnPackedArg{}
			for i, pt in fn_type.params {
				arg_id := g.a.child(&call_node, i + 1)
				mut expected_ct := g.tc.c_type(pt)
				if expected_ct.starts_with('fn_ptr:') {
					expected_ct = g.resolve_fn_ptr_type(expected_ct)
				}
				packed_args << g.spawn_packed_arg_for_param(arg_id, pt, expected_ct, i)
			}
			g.emit_fn_value_spawn_expr(call_id, g.a.child_node(&call_node, 0), fn_type,
				packed_args, ret_ct)
			return
		}
	}
	if wrapper.len == 0 {
		g.write('(void*)0')
		return
	}
	tmp := g.tmp_count
	g.tmp_count++
	g.write('({ pthread_t _t${tmp}; pthread_attr_t _a${tmp}; pthread_attr_init(&_a${tmp}); ')
	g.write('pthread_attr_setstacksize(&_a${tmp}, ${spawn_thread_stack_size}); ')
	g.write('int _r${tmp} = pthread_create(&_t${tmp}, &_a${tmp}, ${wrapper}, (void*)(${arg_expr})); ')
	g.write('pthread_attr_destroy(&_a${tmp}); (void)_r${tmp}; (void*)_t${tmp}; })')
}

// spawn_wrapper_body builds the thread-wrapper statement that invokes the spawned
// call and returns its result as a `void*`. When the callee returns a value, the
// result is heap-copied so `[]thread T .wait()` can recover it (the wait fn frees
// it); a void callee returns NULL. `post` runs after the call (e.g. `free(p);`).
fn spawn_wrapper_body(call_expr string, ret_ct string, post string) string {
	if ret_ct == 'void' || ret_ct.len == 0 {
		return '${call_expr}; ${post}return NULL;'
	}
	return '${ret_ct}* __tr = (${ret_ct}*)malloc(sizeof(${ret_ct})); *__tr = ${call_expr}; ${post}return (void*)__tr;'
}

fn (mut g FlatGen) ensure_noarg_spawn_wrapper(cfn string, ret_ct string) string {
	key := 'noarg|${cfn}'
	if name := g.spawn_wrapper_names[key] {
		return name
	}
	name := g.cname('${cfn}_thread_wrapper')
	g.spawn_wrapper_names[key] = name
	body := spawn_wrapper_body('${cfn}()', ret_ct, '')
	g.spawn_wrapper_defs << 'static void* ${name}(void* arg) { (void)arg; ${body} }'
	return name
}

fn (mut g FlatGen) ensure_receiver_spawn_wrapper(cfn string, receiver_ct string, ret_ct string) string {
	key := 'receiver|${cfn}|${receiver_ct}'
	if name := g.spawn_wrapper_names[key] {
		return name
	}
	name := g.cname('${cfn}_thread_wrapper')
	g.spawn_wrapper_names[key] = name
	body := spawn_wrapper_body('${cfn}((${receiver_ct})arg)', ret_ct, '')
	g.spawn_wrapper_defs << 'static void* ${name}(void* arg) { ${body} }'
	return name
}

// ensure_args_spawn_wrapper registers a heap-arg struct plus a thread wrapper
// that unpacks the struct, calls the function with all arguments, and frees the
// struct. Pointer rvalues are stored by value in the heap struct and passed as
// `&p->field`, so the wrapper shape is part of the cache key.
fn (mut g FlatGen) ensure_args_spawn_wrapper(cfn string, args []SpawnPackedArg, ret_ct string) (string, string) {
	signature := spawn_packed_args_signature(args)
	mut struct_name := g.cname('${cfn}_thread_args')
	mut wrapper_name := g.cname('${cfn}_args_thread_wrapper')
	if !spawn_packed_args_are_direct(args) {
		suffix := spawn_packed_args_name_suffix(args)
		struct_name = g.cname('${cfn}_thread_args_${suffix}')
		wrapper_name = g.cname('${cfn}_args_thread_wrapper_${suffix}')
	}
	key := 'args|${cfn}|${signature}'
	if name := g.spawn_wrapper_names[key] {
		return name, struct_name
	}
	g.spawn_wrapper_names[key] = wrapper_name
	mut fields := ''
	mut call_args := []string{}
	for i, arg in args {
		fields += '${arg.field_ct} a${i}; '
		call_args << arg.call_expr
	}
	g.spawn_wrapper_defs << 'typedef struct { ${fields}} ${struct_name};'
	body := spawn_wrapper_body('${cfn}(${call_args.join(', ')})', ret_ct, 'free(p); ')
	g.spawn_wrapper_defs << 'static void* ${wrapper_name}(void* arg) { ${struct_name}* p = (${struct_name}*)arg; ${body} }'
	return wrapper_name, struct_name
}

// emit_args_spawn_expr writes a statement-expression that heap-allocates the arg
// struct, populates it, and starts the thread on the packing wrapper.
fn (mut g FlatGen) emit_args_spawn_expr(cfn string, args []SpawnPackedArg, ret_ct string) {
	wrapper, struct_name := g.ensure_args_spawn_wrapper(cfn, args, ret_ct)
	tmp := g.tmp_count
	g.tmp_count++
	g.write('({ ${struct_name}* _sa${tmp} = (${struct_name}*)malloc(sizeof(${struct_name})); ')
	for i, arg in args {
		g.write_spawn_packed_arg_init(tmp, i, arg)
	}
	g.write('pthread_t _t${tmp}; pthread_attr_t _at${tmp}; pthread_attr_init(&_at${tmp}); ')
	g.write('pthread_attr_setstacksize(&_at${tmp}, ${spawn_thread_stack_size}); ')
	g.write('int _r${tmp} = pthread_create(&_t${tmp}, &_at${tmp}, ${wrapper}, (void*)_sa${tmp}); ')
	g.write('pthread_attr_destroy(&_at${tmp}); (void)_r${tmp}; (void*)_t${tmp}; })')
}

fn (mut g FlatGen) ensure_fn_value_spawn_wrapper(fn_ct string, args []SpawnPackedArg, ret_ct string) (string, string) {
	signature := spawn_packed_args_signature(args)
	suffix :=
		'${fn_ct}_${spawn_packed_args_name_suffix(args)}'.replace('*', 'ptr').replace(' ', '_')
	struct_name := g.cname('fn_value_thread_args_${suffix}')
	wrapper_name := g.cname('fn_value_args_thread_wrapper_${suffix}')
	key := 'fnvalue|${fn_ct}|${ret_ct}|${signature}'
	if name := g.spawn_wrapper_names[key] {
		return name, struct_name
	}
	g.spawn_wrapper_names[key] = wrapper_name
	mut fields := '${fn_ct} f; '
	mut call_args := []string{}
	for i, arg in args {
		fields += '${arg.field_ct} a${i}; '
		call_args << arg.call_expr
	}
	g.spawn_wrapper_defs << 'typedef struct { ${fields}} ${struct_name};'
	body := spawn_wrapper_body('p->f(${call_args.join(', ')})', ret_ct, 'free(p); ')
	g.spawn_wrapper_defs << 'static void* ${wrapper_name}(void* arg) { ${struct_name}* p = (${struct_name}*)arg; ${body} }'
	return wrapper_name, struct_name
}

fn (mut g FlatGen) emit_fn_value_spawn_expr(call_id flat.NodeId, fn_node flat.Node, fn_type types.FnType, args []SpawnPackedArg, ret_ct string) {
	fn_ct := g.value_c_type(fn_type)
	wrapper, struct_name := g.ensure_fn_value_spawn_wrapper(fn_ct, args, ret_ct)
	tmp := g.tmp_count
	g.tmp_count++
	g.write('({ ${struct_name}* _sa${tmp} = (${struct_name}*)malloc(sizeof(${struct_name})); ')
	g.write('_sa${tmp}->f = ')
	g.gen_expr_with_expected_type(g.a.child(&g.a.nodes[int(call_id)], 0), fn_type)
	g.write('; ')
	for i, arg in args {
		g.write_spawn_packed_arg_init(tmp, i, arg)
	}
	g.write('pthread_t _t${tmp}; pthread_attr_t _at${tmp}; pthread_attr_init(&_at${tmp}); ')
	g.write('pthread_attr_setstacksize(&_at${tmp}, ${spawn_thread_stack_size}); ')
	g.write('int _r${tmp} = pthread_create(&_t${tmp}, &_at${tmp}, ${wrapper}, (void*)_sa${tmp}); ')
	g.write('pthread_attr_destroy(&_at${tmp}); (void)_r${tmp}; (void*)_t${tmp}; })')
	_ = fn_node
}

fn (mut g FlatGen) write_spawn_packed_arg_init(tmp int, idx int, arg SpawnPackedArg) {
	if arg.copy_array {
		g.write('memmove(_sa${tmp}->a${idx}, ${arg.assign_expr}, sizeof(_sa${tmp}->a${idx})); ')
		return
	}
	g.write('_sa${tmp}->a${idx} = ${arg.assign_expr}; ')
}

fn (g &FlatGen) shared_local_arg_c_expr(arg_id flat.NodeId) ?string {
	if int(arg_id) < 0 || int(arg_id) >= g.a.nodes.len {
		return none
	}
	arg := g.a.nodes[int(arg_id)]
	if arg.kind == .paren && arg.children_count > 0 {
		return g.shared_local_arg_c_expr(g.a.child(&arg, 0))
	}
	if arg.kind == .prefix && arg.value == 'shared' && arg.children_count > 0 {
		return g.shared_local_arg_c_expr(g.a.child(&arg, 0))
	}
	if arg.kind == .ident && g.local_storage_is_shared(arg.value) {
		return g.cname(arg.value)
	}
	return none
}

fn (mut g FlatGen) shared_arg_storage_c_expr(arg_id flat.NodeId) ?string {
	if expr := g.shared_local_arg_c_expr(arg_id) {
		return expr
	}
	orig := g.sb
	orig_line_start := g.line_start
	g.sb = strings.new_builder(64)
	g.line_start = false
	ok := g.gen_shared_storage_expr(arg_id)
	result := g.sb.str()
	g.sb = orig
	g.line_start = orig_line_start
	if !ok || result.len == 0 {
		return none
	}
	return result
}

fn (mut g FlatGen) spawn_packed_arg_for_call_param(fn_name string, arg_id flat.NodeId, expected types.Type, field_idx int) SpawnPackedArg {
	if g.fn_param_is_shared_for_call(field_idx, fn_name, '', '', '') {
		if expr := g.shared_arg_storage_c_expr(arg_id) {
			inner := g.shared_qualify_type_text(expected.name(), g.tc.cur_module)
			wrapper_ct := '${g.shared_wrapper_c_name(inner)}*'
			return SpawnPackedArg{
				field_ct:    wrapper_ct
				assign_expr: expr
				call_expr:   'p->a${field_idx}'
			}
		}
	}
	expected_ct := g.spawn_arg_c_type(expected)
	return g.spawn_packed_arg_for_param(arg_id, expected, expected_ct, field_idx)
}

fn (mut g FlatGen) spawn_arg_c_type(expected types.Type) string {
	return g.value_c_type(expected)
}

fn (mut g FlatGen) spawn_packed_arg_for_param(arg_id flat.NodeId, expected types.Type, expected_ct string, field_idx int) SpawnPackedArg {
	if fixed := array_fixed_type(expected) {
		return SpawnPackedArg{
			field_ct:    expected_ct
			assign_expr: g.fixed_array_copy_source_string(arg_id, types.Type(fixed))
			call_expr:   'p->a${field_idx}'
			copy_array:  true
		}
	}
	if spawn_c_type_is_pointer(expected_ct) {
		arg_node := g.a.nodes[int(arg_id)]
		if child_id := g.spawn_materialized_pointer_rvalue_arg(arg_node) {
			value_type := types.unwrap_pointer(expected)
			return SpawnPackedArg{
				field_ct:    g.tc.c_type(value_type)
				assign_expr: g.expr_to_string_with_expected_type(child_id, value_type)
				call_expr:   '&p->a${field_idx}'
			}
		}
		if child_id := g.addressed_rvalue_arg(arg_node) {
			value_type := types.unwrap_pointer(expected)
			return SpawnPackedArg{
				field_ct:    g.tc.c_type(value_type)
				assign_expr: g.expr_to_string_with_expected_type(child_id, value_type)
				call_expr:   '&p->a${field_idx}'
			}
		}
		if child_id := g.spawn_stack_address_value(arg_id) {
			value_type := types.unwrap_pointer(expected)
			return SpawnPackedArg{
				field_ct:    g.tc.c_type(value_type)
				assign_expr: g.expr_to_string_with_expected_type(child_id, value_type)
				call_expr:   '&p->a${field_idx}'
			}
		}
		if g.spawn_arg_expr_is_pointer_value(arg_id) {
			return SpawnPackedArg{
				field_ct:    expected_ct
				assign_expr: g.expr_to_string(arg_id)
				call_expr:   'p->a${field_idx}'
			}
		}
		if g.expr_is_addressable(arg_id) {
			expr := g.expr_to_string(arg_id)
			return SpawnPackedArg{
				field_ct:    expected_ct
				assign_expr: '&${expr}'
				call_expr:   'p->a${field_idx}'
			}
		}
		value_type := types.unwrap_pointer(expected)
		return SpawnPackedArg{
			field_ct:    g.tc.c_type(value_type)
			assign_expr: g.expr_to_string_with_expected_type(arg_id, value_type)
			call_expr:   '&p->a${field_idx}'
		}
	}
	return SpawnPackedArg{
		field_ct:    expected_ct
		assign_expr: g.expr_to_string_with_expected_type(arg_id, expected)
		call_expr:   'p->a${field_idx}'
	}
}

// spawn_stack_address_value finds `&local` (also through parentheses) when local is a value
// binding. The spawned wrapper must own a copy in its heap argument block; storing the stack
// address itself lets loop iterations reuse the slot before the thread reads it.
fn (g &FlatGen) spawn_stack_address_value(id flat.NodeId) ?flat.NodeId {
	if int(id) < 0 || int(id) >= g.a.nodes.len {
		return none
	}
	node := g.a.node(id)
	if node.kind == .paren && node.children_count > 0 {
		return g.spawn_stack_address_value(g.a.child(node, 0))
	}
	if node.kind != .prefix || node.op != .amp || node.children_count == 0 {
		return none
	}
	child_id := g.a.child(node, 0)
	child := g.a.node(child_id)
	if child.kind != .ident || node.is_mut || child.is_mut {
		return none
	}
	local_type := g.local_ident_type(child.value) or { return none }
	if local_type is types.Pointer {
		return none
	}
	return child_id
}

fn spawn_packed_args_signature(args []SpawnPackedArg) string {
	mut parts := []string{}
	for i, arg in args {
		parts << '${i}:${arg.field_ct}:${arg.call_expr}'
	}
	return parts.join('|')
}

fn spawn_packed_args_are_direct(args []SpawnPackedArg) bool {
	for i, arg in args {
		if arg.call_expr != 'p->a${i}' {
			return false
		}
	}
	return true
}

fn spawn_packed_args_name_suffix(args []SpawnPackedArg) string {
	mut parts := []string{}
	for i, arg in args {
		field := arg.field_ct.replace('*', 'ptr').replace(' ', '_')
		mode := if arg.call_expr == 'p->a${i}' { 'value' } else { 'addr' }
		parts << '${field}_${mode}'
	}
	return parts.join('_')
}

fn spawn_c_type_is_pointer(ct string) bool {
	return trimmed_space(ct).ends_with('*')
}

fn (mut g FlatGen) shared_param_c_type(raw_typ string) ?string {
	inner := shared_inner_type_text(raw_typ) or { return none }
	qualified := g.shared_qualify_type_text(inner, g.tc.cur_module)
	return '${g.shared_wrapper_c_name(qualified)}*'
}

fn (g &FlatGen) spawn_arg_expr_is_pointer_value(arg_id flat.NodeId) bool {
	if int(arg_id) < 0 || int(arg_id) >= g.a.nodes.len {
		return false
	}
	node := g.a.nodes[int(arg_id)]
	if node.kind == .prefix && node.op == .amp {
		if node.children_count == 0 {
			return false
		}
		return g.expr_is_addressable(g.a.child(&node, 0))
	}
	if node.kind == .ident {
		if typ := g.current_param_type(node.value) {
			return typ is types.Pointer
		}
	}
	if node.kind == .call {
		if fname := g.tc.resolved_call_name(arg_id) {
			ret_type := g.tc.fn_ret_types[fname] or { return false }
			return ret_type is types.Pointer
		}
		return false
	}
	return g.tc.resolve_type(arg_id) is types.Pointer
}

fn (g &FlatGen) spawn_materialized_pointer_rvalue_arg(arg_node flat.Node) ?flat.NodeId {
	if arg_node.kind != .prefix || arg_node.op != .amp || arg_node.children_count == 0 {
		return none
	}
	child_id := g.a.child(&arg_node, 0)
	if int(child_id) < 0 || int(child_id) >= g.a.nodes.len {
		return none
	}
	child := g.a.nodes[int(child_id)]
	if child.kind == .ident && child.value.starts_with('__ptr_arg_') {
		return child_id
	}
	return none
}

fn (mut g FlatGen) gen_thread_wait_call(fn_node &flat.Node) bool {
	if fn_node.value != 'wait' || fn_node.children_count == 0 {
		return false
	}
	base_id := g.a.child(fn_node, 0)
	base_type0 := g.usable_expr_type(base_id)
	base_type := if base_type0 is types.Unknown || base_type0 is types.Void {
		g.tc.resolve_type(base_id)
	} else {
		base_type0
	}
	clean_type := types.unwrap_pointer(base_type)
	if clean_type !is types.Struct {
		return false
	}
	thread_struct := clean_type as types.Struct
	thread_name := trimmed_space(thread_struct.name)
	mut ret_name := ''
	if thread_name == 'thread' || thread_name.ends_with('.thread') {
		ret_name = ''
	} else if thread_name.starts_with('thread ') {
		ret_name = trimmed_space(thread_name[7..])
	} else {
		return false
	}
	tmp := g.tmp_count
	g.tmp_count++
	res_name := '__twres${tmp}'
	g.write('({ void* ${res_name} = NULL; pthread_join((pthread_t)(')
	g.gen_expr(base_id)
	g.write('), &${res_name}); ')
	if ret_name.len == 0 {
		g.write('if (${res_name}) free(${res_name}); })')
		return true
	}
	ret_ct := g.fn_return_type_name(g.tc.parse_type(ret_name))
	val_name := '__twval${tmp}'
	g.write('${ret_ct} ${val_name}; if (${res_name}) { ${val_name} = *((${ret_ct}*)${res_name}); free(${res_name}); } else { memset(&${val_name}, 0, sizeof(${val_name})); } ${val_name}; })')
	return true
}

fn (g &FlatGen) resolved_method_name_for_spawn(clean_type types.Type, method string) string {
	mut type_name := clean_type.name()
	if clean_type is types.Struct {
		type_name = clean_type.name
	}
	method_name := '${type_name}.${method}'
	if method_name in g.tc.fn_param_types {
		return method_name
	}
	for alias, target in g.tc.type_aliases {
		if target == type_name {
			alias_method := '${alias}.${method}'
			if alias_method in g.tc.fn_param_types {
				return alias_method
			}
		}
	}
	return ''
}

// gen_fn_in_module emits fn in module output for c.
fn (mut g FlatGen) gen_fn_in_module(node flat.Node, module_name string) {
	g.tc.cur_module = module_name
	g.cur_fn_name = node.value
	g.ownership_return_index = 0
	g.ownership_seen_return_sources = map[string]bool{}
	g.ownership_propagation_index = 0
	g.ownership_loop_control_index = 0
	g.ownership_loop_iteration_index = 0
	g.ownership_scope_index = 0
	g.cur_return_drops = []types.OwnershipDropEntry{}
	g.loop_depth = 0
	g.loop_label_depths = map[string]int{}
	mut prelude_scan := g.collect_fn_prelude_scan(node)
	g.goto_label_lock_scopes = prelude_scan.goto_label_lock_scopes.move()
	g.pending_loop_label = ''
	old_ierror_stack_pointer_aliases := g.ierror_stack_pointer_aliases.clone()
	g.ierror_stack_pointer_aliases = []map[string]bool{}
	old_local_pointer_storage_by_owner := g.local_pointer_storage_by_owner.clone()
	g.local_pointer_storage_by_owner = map[string]bool{}
	old_local_c_type_by_owner := g.local_c_type_by_owner.clone()
	g.local_c_type_by_owner = map[string]string{}
	old_local_shared_storage_by_owner := g.local_shared_storage_by_owner.clone()
	g.local_shared_storage_by_owner = map[string]bool{}
	g.push_scope()
	g.defers = []flat.NodeId{}
	g.fn_defers = []flat.NodeId{}
	g.fn_defer_counts = map[int]string{}
	g.defer_capture_names = []string{}
	g.defer_capture_types = map[string]types.Type{}
	g.set_cur_fn_ret(types.Type(types.void_))
	old_param_names := g.cur_param_names.clone()
	old_param_type_values := g.cur_param_type_values.clone()
	old_param_types := g.cur_param_types.clone()
	old_concrete_optional_params := g.cur_concrete_optional_params.clone()
	old_mut_params := g.cur_mut_params.clone()
	old_mut_param_owners := g.cur_mut_param_owners.clone()
	g.cur_param_names = []string{}
	g.cur_param_type_values = []types.Type{}
	g.cur_param_types = map[string]types.Type{}
	g.cur_concrete_optional_params = map[string]bool{}
	g.cur_mut_params = map[string]bool{}
	g.cur_mut_param_owners = map[string]types.ScopeBindingOwner{}
	typed_params := g.fn_node_param_types(node, module_name)
	concrete_optional_params := g.is_specialized_generic_fn_node(node)
	mut param_idx := 0
	for i in 0 .. node.children_count {
		param_id := g.a.child(&node, i)
		p := g.a.node(param_id)
		if p.kind == .param {
			decl_param_type := g.tc.parse_type(p.typ)
			param_type := if !concrete_optional_params && p.typ.len > 0
				&& !decl_annotation_is_unusable(decl_param_type, p.typ) {
				decl_param_type
			} else if param_idx < typed_params.len {
				typed_params[param_idx]
			} else {
				decl_param_type
			}
			param_idx++
			if p.value.len > 0 {
				g.cur_param_names << p.value
				g.cur_param_type_values << param_type
				g.cur_param_types[p.value] = param_type
				owner := g.tc.cur_scope.insert_with_owner(p.value, param_type)
				if shared_ct := g.shared_param_c_type(p.typ) {
					g.declare_local_c_type(owner, shared_ct)
					g.declare_local_pointer_storage(owner, true)
					g.declare_local_shared_storage(owner, true)
				}
				if p.is_mut {
					g.cur_mut_params[p.value] = true
					g.cur_mut_param_owners[p.value] = owner
				}
				if concrete_optional_params && type_is_optional_result(param_type) {
					g.cur_concrete_optional_params[p.value] = true
				}
			}
		}
	}
	g.insert_cur_implicit_veb_ctx_param(node)
	g.prepare_function_defers(prelude_scan.defer_ids)
	is_entry_main := is_main_fn_in_main_module(module_name, node.value) && g.test_files.len == 0
	if is_entry_main {
		g.writeln('int main(int argc, char** argv) {')
		if g.has_builtins {
			g.writeln('\tg_main_argc = argc;')
			g.writeln('\tg_main_argv = argv;')
		}
		g.gen_compiler_vexe_env_setup()
		if g.const_runtime_inits.len > 0 || g.runtime_inits.len > 0 || g.module_init_fns.len > 0
			|| g.global_inits.len > 0 {
			g.writeln('\t_vinit();')
		}
	} else {
		ret_type := g.fn_node_return_type(node, module_name)
		g.set_cur_fn_ret(ret_type)
		g.write(g.fn_return_type_name(ret_type))
		g.write(' ')
		g.write(g.fn_c_name_in_module(module_name, node.value))
		g.write('(')
		g.write_fn_node_params(node)
		g.writeln(') {')
	}
	g.indent++
	g.gen_function_defer_prelude()

	for i in 0 .. node.children_count {
		id := g.a.child(&node, i)
		child := g.a.node(id)
		if child.kind != .param {
			g.tc.cur_module = module_name
			g.gen_node(id)
		}
	}
	g.gen_all_defers()
	g.gen_ownership_drops(g.tc.ownership_drop_entries_at_fn_exit(qualify_name_in_module(module_name,
		node.value)))
	if is_entry_main {
		g.writeln('return 0;')
	} else if g.cur_fn_ret_is_optional {
		ct := g.optional_type_name(g.cur_fn_ret)
		g.writeln('return (${ct}){.ok = true};')
	}
	g.indent--
	g.writeln('}')
	g.writeln('')
	if !is_entry_main {
		g.gen_export_wrapper_for_fn(node, module_name)
	}
	g.cur_param_names = old_param_names.clone()
	g.cur_param_type_values = old_param_type_values.clone()
	g.cur_param_types = old_param_types.clone()
	g.cur_concrete_optional_params = old_concrete_optional_params.clone()
	g.cur_mut_params = old_mut_params.clone()
	g.cur_mut_param_owners = old_mut_param_owners.clone()
	g.loop_depth = 0
	g.loop_label_depths = map[string]int{}
	g.goto_label_lock_scopes = map[string][]int{}
	g.pending_loop_label = ''
	g.pop_scope()
	g.ierror_stack_pointer_aliases = old_ierror_stack_pointer_aliases.clone()
	g.local_pointer_storage_by_owner = old_local_pointer_storage_by_owner.clone()
	g.local_c_type_by_owner = old_local_c_type_by_owner.clone()
	g.local_shared_storage_by_owner = old_local_shared_storage_by_owner.clone()
}

fn (mut g FlatGen) gen_export_wrapper_for_fn(node flat.Node, module_name string) {
	export_name := g.export_fn_name_in_module(module_name, node.value) or { return }
	canonical_name := g.fn_c_name_in_module(module_name, node.value)
	ret_type := g.fn_node_return_type(node, module_name)
	ret_ct := g.fn_return_type_name(ret_type)
	g.write(ret_ct)
	g.write(' ')
	g.write(export_name)
	g.write('(')
	g.write_fn_node_params(node)
	g.writeln(') {')
	g.indent++
	args := g.export_wrapper_arg_names(node)
	call := '${canonical_name}(${args.join(', ')})'
	if ret_type is types.Void {
		g.writeln('${call};')
	} else {
		g.writeln('return ${call};')
	}
	g.indent--
	g.writeln('}')
	g.writeln('')
}

fn (mut g FlatGen) export_wrapper_arg_names(node flat.Node) []string {
	mut args := []string{}
	needs_implicit_ctx := g.fn_needs_implicit_veb_ctx(node)
	insert_implicit_ctx_after_first := needs_implicit_ctx && g.fn_has_receiver_param(node)
	mut written := 0
	mut implicit_ctx_written := false
	for i in 0 .. node.children_count {
		param_id := g.a.child(&node, i)
		p := g.a.node(param_id)
		if p.kind != .param {
			continue
		}
		param_name := if p.value == '_' { '_${written}' } else { g.cname(p.value) }
		args << param_name
		written++
		if insert_implicit_ctx_after_first && !implicit_ctx_written {
			args << 'ctx'
			written++
			implicit_ctx_written = true
		}
	}
	if needs_implicit_ctx && !implicit_ctx_written {
		args << 'ctx'
	}
	return args
}

fn (mut g FlatGen) gen_top_level_main(stmts []TopLevelStmt) {
	old_tc_file := g.tc.cur_file
	old_tc_module := g.tc.cur_module
	g.tc.cur_module = 'main'
	old_fn_name := g.cur_fn_name
	g.cur_fn_name = 'main'
	g.loop_depth = 0
	g.loop_label_depths = map[string]int{}
	mut prelude_scan := g.collect_top_level_prelude_scan(stmts)
	g.goto_label_lock_scopes = prelude_scan.goto_label_lock_scopes.move()
	g.pending_loop_label = ''
	old_ierror_stack_pointer_aliases := g.ierror_stack_pointer_aliases.clone()
	g.ierror_stack_pointer_aliases = []map[string]bool{}
	old_local_pointer_storage_by_owner := g.local_pointer_storage_by_owner.clone()
	g.local_pointer_storage_by_owner = map[string]bool{}
	old_local_c_type_by_owner := g.local_c_type_by_owner.clone()
	g.local_c_type_by_owner = map[string]string{}
	old_local_shared_storage_by_owner := g.local_shared_storage_by_owner.clone()
	g.local_shared_storage_by_owner = map[string]bool{}
	g.push_scope()
	g.defers = []flat.NodeId{}
	g.fn_defers = []flat.NodeId{}
	g.fn_defer_counts = map[int]string{}
	g.defer_capture_names = []string{}
	g.defer_capture_types = map[string]types.Type{}
	g.set_cur_fn_ret(types.Type(types.void_))
	old_param_names := g.cur_param_names.clone()
	old_param_type_values := g.cur_param_type_values.clone()
	old_param_types := g.cur_param_types.clone()
	old_concrete_optional_params := g.cur_concrete_optional_params.clone()
	old_mut_params := g.cur_mut_params.clone()
	old_mut_param_owners := g.cur_mut_param_owners.clone()
	g.cur_param_names = []string{}
	g.cur_param_type_values = []types.Type{}
	g.cur_param_types = map[string]types.Type{}
	g.cur_concrete_optional_params = map[string]bool{}
	g.cur_mut_params = map[string]bool{}
	g.cur_mut_param_owners = map[string]types.ScopeBindingOwner{}
	g.prepare_function_defers(prelude_scan.defer_ids)
	g.writeln('int main(int argc, char** argv) {')
	if g.has_builtins {
		g.writeln('\tg_main_argc = argc;')
		g.writeln('\tg_main_argv = argv;')
	}
	g.gen_compiler_vexe_env_setup()
	if g.const_runtime_inits.len > 0 || g.runtime_inits.len > 0 || g.module_init_fns.len > 0
		|| g.global_inits.len > 0 {
		g.writeln('\t_vinit();')
	}
	g.indent++
	g.gen_function_defer_prelude()
	for stmt in stmts {
		g.tc.cur_file = stmt.file
		g.tc.cur_module = stmt.module
		g.gen_top_level_main_stmt(stmt.id)
	}
	g.gen_all_defers()
	g.writeln('return 0;')
	g.indent--
	g.writeln('}')
	g.writeln('')
	g.cur_param_names = old_param_names.clone()
	g.cur_param_type_values = old_param_type_values.clone()
	g.cur_param_types = old_param_types.clone()
	g.cur_concrete_optional_params = old_concrete_optional_params.clone()
	g.cur_mut_params = old_mut_params.clone()
	g.cur_mut_param_owners = old_mut_param_owners.clone()
	g.cur_fn_name = old_fn_name
	g.loop_depth = 0
	g.loop_label_depths = map[string]int{}
	g.goto_label_lock_scopes = map[string][]int{}
	g.pending_loop_label = ''
	g.tc.cur_file = old_tc_file
	g.tc.cur_module = old_tc_module
	g.pop_scope()
	g.ierror_stack_pointer_aliases = old_ierror_stack_pointer_aliases.clone()
	g.local_pointer_storage_by_owner = old_local_pointer_storage_by_owner.clone()
	g.local_c_type_by_owner = old_local_c_type_by_owner.clone()
	g.local_shared_storage_by_owner = old_local_shared_storage_by_owner.clone()
}

fn (mut g FlatGen) gen_top_level_main_stmt(id flat.NodeId) {
	if int(id) < 0 || int(id) >= g.a.nodes.len {
		return
	}
	node := g.a.nodes[int(id)]
	if node.kind in [.block, .comptime_if] {
		for i in 0 .. node.children_count {
			child_id := g.a.child(&node, i)
			if g.cgen_is_top_level_stmt(child_id) {
				g.gen_top_level_main_stmt(child_id)
			}
		}
		return
	}
	g.gen_node(id)
}

fn (mut g FlatGen) gen_test_main() {
	tests, hooks := g.test_harness_fns()
	g.tc.cur_module = 'main'
	g.writeln('int main(int argc, char** argv) {')
	if g.has_builtins {
		g.writeln('\tg_main_argc = argc;')
		g.writeln('\tg_main_argv = argv;')
	}
	g.gen_compiler_vexe_env_setup()
	if g.const_runtime_inits.len > 0 || g.runtime_inits.len > 0 || g.module_init_fns.len > 0
		|| g.global_inits.len > 0 {
		g.writeln('\t_vinit();')
	}
	g.indent++
	if hooks.testsuite_begin.len > 0 {
		g.writeln('${hooks.testsuite_begin}();')
	}
	for idx, test_fn in tests {
		if hooks.before_each.len > 0 {
			g.writeln('${hooks.before_each}();')
		}
		g.gen_test_fn_call(test_fn, hooks, idx)
		if hooks.after_each.len > 0 {
			g.writeln('${hooks.after_each}();')
		}
	}
	if hooks.testsuite_end.len > 0 {
		g.writeln('${hooks.testsuite_end}();')
	}
	g.writeln('return 0;')
	g.indent--
	g.writeln('}')
	g.writeln('')
}

fn (mut g FlatGen) gen_test_fn_call(test_fn TestHarnessFn, hooks TestHarnessHooks, idx int) {
	if test_fn.ret is types.OptionType || test_fn.ret is types.ResultType {
		ct := g.optional_type_name(test_fn.ret)
		tmp_name := '__test_opt_${idx}'
		g.writeln('${ct} ${tmp_name} = ${test_fn.c_name}();')
		g.writeln('if (!${tmp_name}.ok) {')
		g.indent++
		g.writeln('v3_eprint_lit("test failed: ${c_escape(test_fn.name)}\\n");')
		if hooks.after_each.len > 0 {
			g.writeln('${hooks.after_each}();')
		}
		if hooks.testsuite_end.len > 0 {
			g.writeln('${hooks.testsuite_end}();')
		}
		g.writeln('return 1;')
		g.indent--
		g.writeln('}')
		return
	}
	g.writeln('${test_fn.c_name}();')
}

fn (g &FlatGen) test_harness_fns() ([]TestHarnessFn, TestHarnessHooks) {
	mut tests := []TestHarnessFn{}
	mut hooks := TestHarnessHooks{}
	for file_idx, file_node in g.a.nodes {
		if !g.is_user_test_file_node(file_idx, file_node) {
			continue
		}
		module_name := g.test_file_module_name(file_node)
		mut decl_ids := []flat.NodeId{}
		g.collect_test_harness_decl_ids(file_node, mut decl_ids)
		for child_id in decl_ids {
			child := g.a.nodes[int(child_id)]
			cname := g.qualified_fn_name_in_module_c(module_name, child.value)
			match child.value {
				'testsuite_begin' {
					if hooks.testsuite_begin.len == 0 && g.is_supported_test_hook_decl(child) {
						hooks.testsuite_begin = cname
					}
				}
				'testsuite_end' {
					if hooks.testsuite_end.len == 0 && g.is_supported_test_hook_decl(child) {
						hooks.testsuite_end = cname
					}
				}
				'before_each' {
					if hooks.before_each.len == 0 && g.is_supported_test_hook_decl(child) {
						hooks.before_each = cname
					}
				}
				'after_each' {
					if hooks.after_each.len == 0 && g.is_supported_test_hook_decl(child) {
						hooks.after_each = cname
					}
				}
				else {
					if child.value.starts_with('test_') && g.is_supported_test_fn_decl(child) {
						tests << TestHarnessFn{
							name:   child.value
							c_name: cname
							ret:    g.tc.parse_type(child.typ)
						}
					}
				}
			}
		}
	}
	return tests, hooks
}

fn (g &FlatGen) collect_test_harness_decl_ids(node flat.Node, mut ids []flat.NodeId) {
	if node.kind != .file && node.kind != .block {
		return
	}
	for i in 0 .. node.children_count {
		child_id := g.a.child(&node, i)
		if int(child_id) < g.a.user_code_start {
			continue
		}
		child := g.a.nodes[int(child_id)]
		if child.kind == .fn_decl {
			ids << child_id
		} else if child.kind == .block {
			g.collect_test_harness_decl_ids(child, mut ids)
		}
	}
}

fn (g &FlatGen) is_supported_test_fn_decl(node flat.Node) bool {
	if node.generic_params.len > 0 {
		return false
	}
	if g.test_fn_param_count(node) != 0 {
		return false
	}
	return test_harness_fn_return_supported(g.tc.parse_type(node.typ))
}

fn (g &FlatGen) is_supported_test_hook_decl(node flat.Node) bool {
	if node.generic_params.len > 0 {
		return false
	}
	return g.test_fn_param_count(node) == 0 && g.tc.parse_type(node.typ) is types.Void
}

fn (g &FlatGen) test_fn_param_count(node flat.Node) int {
	mut count := 0
	for i in 0 .. node.children_count {
		child := g.a.child_node(&node, i)
		if child.kind == .param {
			count++
		}
	}
	return count
}

fn test_harness_fn_return_supported(ret types.Type) bool {
	return ret is types.Void || ret is types.OptionType || ret is types.ResultType
}

fn (g &FlatGen) is_user_test_file_node(file_idx int, file_node flat.Node) bool {
	if file_idx < g.a.user_code_start || file_node.kind != .file || file_node.children_count == 0 {
		return false
	}
	return g.test_files[file_node.value]
}

fn (g &FlatGen) test_file_module_name(file_node flat.Node) string {
	for i in 0 .. file_node.children_count {
		child := g.a.child_node(&file_node, i)
		if child.kind == .module_decl {
			return child.value
		}
	}
	return ''
}

// collect_function_defer_ids updates collect function defer ids state for c.
fn (mut g FlatGen) collect_function_defer_ids(node flat.Node) []flat.NodeId {
	mut ids := []flat.NodeId{}
	for i in 0 .. node.children_count {
		g.collect_function_defer_ids_from(g.a.child(&node, i), mut ids)
	}
	return ids
}

// collect_function_defer_ids_from updates collect function defer ids from state for c.
fn (mut g FlatGen) collect_function_defer_ids_from(id flat.NodeId, mut ids []flat.NodeId) {
	if !g.valid_node_id(id) {
		return
	}
	node := g.a.nodes[int(id)]
	if node.kind == .fn_decl || node.kind == .c_fn_decl || node.kind == .fn_literal {
		return
	}
	if node.kind == .defer_stmt && node.value == 'function' {
		ids << id
		return
	}
	for i in 0 .. node.children_count {
		g.collect_function_defer_ids_from(g.a.child(&node, i), mut ids)
	}
}

// prepare_function_defers supports prepare function defers handling for FlatGen.
fn (mut g FlatGen) prepare_function_defers(fn_defer_ids []flat.NodeId) {
	for idx, defer_id in fn_defer_ids {
		g.fn_defer_counts[int(defer_id)] = '${g.cname(g.cur_fn_name)}_defer_${idx}_count'
		defer_node := g.a.nodes[int(defer_id)]
		if defer_node.children_count > 0 {
			g.collect_function_defer_captures(g.a.child(&defer_node, 0))
		}
	}
}

// collect_function_defer_captures updates collect function defer captures state for c.
fn (mut g FlatGen) collect_function_defer_captures(id flat.NodeId) {
	if !g.valid_node_id(id) {
		return
	}
	node := g.a.nodes[int(id)]
	if node.kind == .fn_decl || node.kind == .c_fn_decl || node.kind == .fn_literal {
		return
	}
	if node.kind == .ident {
		g.add_function_defer_capture(id, node.value)
	}
	for i in 0 .. node.children_count {
		g.collect_function_defer_captures(g.a.child(&node, i))
	}
}

// add_function_defer_capture updates add function defer capture state for FlatGen.
fn (mut g FlatGen) add_function_defer_capture(id flat.NodeId, name string) {
	if name.len == 0 || name == '_' || name in g.cur_param_names || g.has_import_alias(name)
		|| name in g.global_modules || name in g.defer_capture_types {
		return
	}
	typ := g.usable_expr_type(id)
	if typ is types.Void || typ is types.Unknown || typ is types.FnType {
		return
	}
	ct := g.value_c_type(typ)
	if ct.len == 0 || ct == 'void' || ct.starts_with('fn_ptr:') {
		return
	}
	g.defer_capture_names << name
	g.defer_capture_types[name] = typ
}

// gen_function_defer_prelude emits function defer prelude output for c.
fn (mut g FlatGen) gen_function_defer_prelude() {
	for _, count_name in g.fn_defer_counts {
		g.writeln('int ${count_name} = 0;')
	}
	for name in g.defer_capture_names {
		typ := g.defer_capture_types[name] or { continue }
		ct := g.value_c_type(typ)
		g.write('${ct} ${g.cname(name)} = ')
		g.gen_default_value_for_type(typ)
		g.writeln(';')
		g.tc.cur_scope.insert(name, typ)
	}
}

// set_cur_fn_ret updates set cur fn ret state for c.
fn (mut g FlatGen) set_cur_fn_ret(ret_type types.Type) {
	g.cur_fn_ret = ret_type
	g.cur_fn_ret_is_optional = false
	g.cur_fn_ret_base = types.Type(types.void_)
	if ret_type is types.OptionType {
		g.cur_fn_ret_is_optional = true
		g.cur_fn_ret_base = ret_type.base_type
	} else if ret_type is types.ResultType {
		g.cur_fn_ret_is_optional = true
		g.cur_fn_ret_base = ret_type.base_type
	}
}

// gen_compiler_vexe_env_setup emits compiler vexe env setup output for c.
fn (mut g FlatGen) gen_compiler_vexe_env_setup() {
	if g.compiler_vexe.len == 0 && g.compiler_vroot.len == 0 {
		return
	}
	root := c_escape(g.compiler_vroot)
	mut runtime_vexe := g.compiler_vexe
	if runtime_vexe.len > 0 && g.compiler_vroot.len > 0 {
		clean_root := g.compiler_vroot.replace('\\', '/').trim_right('/')
		clean_vexe := runtime_vexe.replace('\\', '/')
		if clean_root.len > 0 && !(clean_vexe == '${clean_root}/v'
			|| clean_vexe.starts_with('${clean_root}/')) {
			runtime_vexe = '${g.compiler_vroot}/v'
		}
	}
	g.writeln('\tif (getenv("VEXE") == NULL || getenv("VEXE")[0] == 0) {')
	if runtime_vexe.len > 0 {
		vexe := c_escape(runtime_vexe)
		g.writeln('\t\tconst char* v3_vexe = "${vexe}";')
	} else {
		g.writeln('\t\tconst char* v3_arg0 = argc > 0 ? argv[0] : "v";')
		g.writeln("\t\tconst char* v3_base = strrchr(v3_arg0, '/');")
		g.writeln('\t\tv3_base = v3_base == NULL ? v3_arg0 : v3_base + 1;')
		g.writeln('\t\tif (v3_base[0] == 0) v3_base = "v";')
		g.writeln('\t\tconst char* v3_checkout_root = "${root}";')
		g.writeln('\t\tchar v3_checkout_vexe[4096];')
		g.writeln('\t\tsnprintf(v3_checkout_vexe, sizeof(v3_checkout_vexe), "%s/%s", v3_checkout_root, v3_base);')
		g.writeln('\t\tif (access(v3_checkout_vexe, F_OK) != 0) snprintf(v3_checkout_vexe, sizeof(v3_checkout_vexe), "%s/v", v3_checkout_root);')
		g.writeln('\t\tchar v3_src_real[4096];')
		g.writeln('\t\tchar* v3_src_real_result = realpath(v3_arg0, v3_src_real);')
		g.writeln('\t\tconst char* v3_vexe = v3_src_real_result != NULL ? v3_src_real : v3_arg0;')
		g.writeln('\t\tif (access(v3_checkout_vexe, F_OK) == 0) v3_vexe = v3_checkout_vexe;')
	}
	g.writeln('\t\tif (v3_vexe[0] != 0) {')
	g.writeln('#ifdef _WIN32')
	g.writeln('\t\t\t_putenv_s("VEXE", v3_vexe);')
	g.writeln('#else')
	g.writeln('\t\t\tsetenv("VEXE", v3_vexe, 1);')
	g.writeln('#endif')
	g.writeln('\t\t}')
	g.writeln('\t}')
}

// gen_defers emits defers output for c.
fn (mut g FlatGen) gen_defers() {
	g.gen_defers_from(0)
}

// gen_all_defers emits all defers output for c.
fn (mut g FlatGen) gen_all_defers() {
	g.gen_defers()
	g.gen_fn_defers()
}

// gen_defers_from emits defers from output for c.
fn (mut g FlatGen) gen_defers_from(start int) {
	g.gen_defers_range(start, g.defers.len)
}

fn (mut g FlatGen) gen_defers_range(start int, end int) {
	if g.defers.len == 0 {
		return
	}
	mut defer_start := start
	if defer_start < 0 {
		defer_start = 0
	}
	mut defer_end := end
	if defer_end > g.defers.len {
		defer_end = g.defers.len
	}
	if defer_start >= defer_end {
		return
	}
	mut i := defer_end
	for i > defer_start {
		i--
		defer_body := g.a.nodes[int(g.defers[i])]
		g.writeln('{')
		g.indent++
		for j in 0 .. defer_body.children_count {
			g.gen_node(g.a.child(&defer_body, j))
		}
		g.indent--
		g.writeln('}')
	}
}

// gen_fn_defers emits fn defers output for c.
fn (mut g FlatGen) gen_fn_defers() {
	if g.fn_defers.len == 0 {
		return
	}
	mut i := g.fn_defers.len
	for i > 0 {
		i--
		defer_id := g.fn_defers[i]
		defer_node := g.a.nodes[int(defer_id)]
		defer_body := g.a.nodes[int(g.a.child(&defer_node, 0))]
		count_name := g.fn_defer_counts[int(defer_id)] or { '0' }
		iter_name := '${count_name}_i'
		g.writeln('for (int ${iter_name} = 0; ${iter_name} < ${count_name}; ${iter_name}++) {')
		g.indent++
		for j in 0 .. defer_body.children_count {
			g.gen_node(g.a.child(&defer_body, j))
		}
		g.indent--
		g.writeln('}')
	}
}

// trim_defers transforms trim defers data for c.
fn (mut g FlatGen) trim_defers(start int) {
	if start >= g.defers.len {
		return
	}
	g.defers = g.defers[..start].clone()
}

// gen_ierror_from_error_call converts gen ierror from error call data for c.
fn (mut g FlatGen) gen_ierror_from_error_call(node flat.Node) {
	fn_node := g.a.child_node(&node, 0)
	g.write('(IError){._typ = 0, ._object = NULL, .message = ')
	if node.children_count > 1 {
		g.gen_expr(g.a.child(&node, 1))
	} else {
		g.write('_S("")')
	}
	g.write(', .code = ')
	if fn_node.value == 'error_with_code' && node.children_count > 2 {
		g.gen_expr(g.a.child(&node, 2))
	} else {
		g.write('0')
	}
	g.write('}')
}

// gen_optional_error_from_call converts gen optional error from call data for c.
fn (mut g FlatGen) gen_optional_error_from_call(ct string, node flat.Node) {
	g.write('(${ct}){.ok = false, .err = ')
	g.gen_ierror_from_error_call(node)
	g.write('}')
}

// gen_call emits call output for c.
fn (mut g FlatGen) gen_call(id flat.NodeId, node flat.Node) {
	fn_node := g.a.child_node(&node, 0)
	target_name := g.call_target_name(g.a.child(&node, 0))
	fn_name := if fn_node.kind == .selector && fn_node.value in ['error', 'error_with_code'] {
		target_name
	} else {
		fn_node.value
	}
	if target_name == 'array.pointers' || fn_name == 'array.pointers' {
		if node.children_count > 1 {
			arg_id := g.a.child(&node, 1)
			g.gen_array_pointers_expr(arg_id, g.tc.resolve_type(arg_id) is types.Pointer)
		} else {
			g.write('array_new(sizeof(voidptr), 0, 0)')
		}
		return
	}
	resolved_target_name := g.tc.resolved_call_name(id) or { '' }
	if node.children_count >= 2 && (fn_name == 'sync__Channel__close'
		|| target_name == 'sync__Channel__close'
		|| target_name == 'C.sync__Channel__close'
		|| resolved_target_name == 'sync__Channel__close') {
		g.write('sync__Channel__close(')
		g.gen_expr(g.a.child(&node, 1))
		if node.children_count > 2 {
			g.write(', ')
			g.gen_expr(g.a.child(&node, 2))
		} else {
			g.write(', array_new(sizeof(IError), 0, 0)')
		}
		g.write(')')
		return
	}
	if resolved_target_name == 'chan.close' && fn_node.kind == .selector {
		g.gen_channel_close_call(g.a.child(fn_node, 0), node)
		return
	}
	if fn_node.kind == .selector && g.gen_channel_try_call(node, fn_node) {
		return
	}
	if fn_node.kind == .selector && g.gen_compiler_default_free_call(fn_node, resolved_target_name) {
		return
	}
	if resolved_target_name in ['free', 'builtin.free'] && node.children_count == 2 {
		arg_id := g.a.child(&node, 1)
		arg_type := g.usable_expr_type(arg_id)
		clean_type := types.unwrap_pointer(arg_type)
		if _ := array_like_type(clean_type) {
			g.write('array__free(')
			if arg_type !is types.Pointer {
				g.write('&')
			}
			g.gen_expr(arg_id)
			g.write(')')
			return
		}
	}
	if g.is_json_decode_call(id, target_name) {
		if !g.is_json_decode_self_call(target_name, resolved_target_name)
			&& g.gen_json_decode_call(node) {
			return
		}
		g.gen_default_value_for_type(g.json_decode_result_type_for_call(node) or {
			g.call_default_return_type(id)
		})
		return
	}
	if target_name == 'json.encode' || resolved_target_name == 'json.encode'
		|| (g.tc.cur_module == 'json' && target_name == 'encode') {
		// Old `json` module only; `json2`/`x.json2` are pure V (see
		// is_json_decode_target_name).
		if g.gen_json_encode_call(node) {
			return
		}
	}
	if g.is_veb_json_result_call(fn_node) {
		g.gen_default_value_for_type(g.call_default_return_type(id))
		return
	}
	if target_name == 'veb.run_at' {
		g.gen_default_value_for_type(g.call_default_return_type(id))
		return
	}
	if g.is_missing_middleware_use_call(fn_node) {
		g.write('0')
		return
	}
	if fn_node.kind == .selector && fn_node.value == 'str' {
		base_type := g.tc.resolve_type(g.a.child(fn_node, 0))
		clean_type := concrete_receiver_type(base_type)
		if clean_type is types.Enum {
			if _ := g.enum_receiver_method_name(clean_type, fn_node.value) {
				// Let normal method call generation handle custom enum str methods.
			} else {
				g.gen_enum_str_call(fn_node, clean_type)
				return
			}
		}
	}
	if fn_node.kind == .selector && fn_node.value == 'close' {
		base_id := g.a.child(fn_node, 0)
		base_type := g.tc.resolve_type(base_id)
		if cgen_is_channel_close_receiver_type(base_type) {
			g.gen_channel_close_call(base_id, node)
			return
		}
	}
	if g.gen_flag_enum_from_call(fn_node, node) {
		return
	}
	if target_name.starts_with('C.') {
		g.write(g.direct_call_name_for_call(id, target_name))
		g.write('(')
		g.gen_call_args(target_name, node, 1)
		g.write(')')
		return
	}
	if g.gen_transformed_method_ident_call(id, node, fn_node) {
		return
	}
	if const_target := g.const_fn_call_target_name(fn_node) {
		emitted_target := g.direct_call_name_for_call(id, const_target)
		g.write(emitted_target)
		g.write('(')
		g.gen_call_args(emitted_target, node, 1)
		g.write(')')
		return
	}
	if resolved_module_call := g.selector_module_call_name(fn_node, node) {
		call_args_name := if specialized := g.specialized_generic_plain_fn_name_for_call(id, node,
			resolved_module_call)
		{
			specialized
		} else {
			resolved_module_call
		}
		emitted_name := g.direct_call_name_for_call_node(id, node, call_args_name)
		g.write(emitted_name)
		g.write('(')
		g.gen_call_args(call_args_name, node, g.selector_module_call_arg_start(fn_node, node))
		g.write(')')
		return
	}
	if resolved_module_call := g.target_module_call_name(target_name, node) {
		call_args_name := if specialized := g.specialized_generic_plain_fn_name_for_call(id, node,
			resolved_module_call)
		{
			specialized
		} else {
			resolved_module_call
		}
		emitted_name := g.direct_call_name_for_call_node(id, node, call_args_name)
		g.write(emitted_name)
		g.write('(')
		g.gen_call_args(call_args_name, node, g.target_module_call_arg_start(target_name, node))
		g.write(')')
		return
	}
	if resolved := g.tc.resolved_call_name(id) {
		if !(fn_node.kind == .ident && g.resolved_name_is_generic_plain(resolved)
			&& g.plain_concrete_fn_name_shadows_generic(fn_node.value) && resolved != fn_node.value)
			&& g.selector_call_can_emit_direct(resolved, node) {
			emitted_resolved := g.direct_call_name_for_call_node(id, node, resolved)
			g.write(emitted_resolved)
			g.write('(')
			g.gen_call_args(emitted_resolved, node, 1)
			g.write(')')
			return
		}
	}
	match fn_name {
		'array_new' {
			g.write('array_new(')
			for i in 1 .. node.children_count {
				if i > 1 {
					g.write(', ')
				}
				arg_id := g.a.child(&node, i)
				arg_node := g.a.nodes[int(arg_id)]
				if i == 1 {
					arg_expr := g.expr_to_string(arg_id)
					if raw_sizeof := raw_sizeof_arg_value(arg_expr) {
						if raw_sizeof_needs_normalization(raw_sizeof) {
							g.write('sizeof(${g.sizeof_target(raw_sizeof)})')
						} else {
							g.write(arg_expr)
						}
					} else {
						g.write(arg_expr)
					}
				} else if arg_node.kind == .sizeof_expr {
					g.write('sizeof(${g.sizeof_target(arg_node.value)})')
				} else if raw_sizeof := raw_sizeof_arg_value(arg_node.value) {
					if raw_sizeof_needs_normalization(raw_sizeof) {
						g.write('sizeof(${g.sizeof_target(raw_sizeof)})')
					} else {
						g.gen_expr(arg_id)
					}
				} else {
					g.gen_expr(arg_id)
				}
			}
			g.write(')')
			return
		}
		'new_map' {
			if node.typ.starts_with('map[') {
				map_type := g.tc.parse_type(node.typ)
				if map_type is types.Map {
					g.write_new_map(map_type.key_type, map_type.value_type)
					return
				}
			}
			g.write('new_map(')
			g.gen_call_args(fn_name, node, 1)
			g.write(')')
			return
		}
		'panic' {
			g.write('panic(')
			if node.children_count > 1 {
				arg_id := g.a.child(&node, 1)
				arg_type := g.tc.resolve_type(arg_id)
				clean_arg_type := types.unwrap_pointer(arg_type)
				if g.is_ierror_type_name(clean_arg_type.name()) {
					g.write('IError__str(')
					arg_expr := g.expr_to_string(arg_id)
					if arg_expr.starts_with('&') {
						g.write(arg_expr[1..])
					} else {
						g.write(arg_expr)
					}
					g.write(')')
				} else {
					g.gen_expr(arg_id)
				}
			}
			g.write(')')
			return
		}
		'error' {
			if g.cur_fn_ret_is_optional {
				ct := g.optional_type_name(g.cur_fn_ret)
				g.gen_optional_error_from_call(ct, node)
			} else {
				g.gen_ierror_from_error_call(node)
			}
			return
		}
		'error_with_code' {
			if g.cur_fn_ret_is_optional {
				ct := g.optional_type_name(g.cur_fn_ret)
				g.gen_optional_error_from_call(ct, node)
			} else {
				g.gen_ierror_from_error_call(node)
			}
			return
		}
		else {
			mut is_method := false
			mut is_c_call := false
			mut method_name := ''
			mut base_id := flat.NodeId(0)
			mut emitted_callee_name := ''
			if fn_node.kind == .selector {
				base := g.a.child_node(fn_node, 0)
				base_is_local := if base.kind == .ident {
					g.selector_base_is_value(base.value)
				} else {
					false
				}
				if base.kind == .ident && base.value == 'C' {
					g.write(g.direct_call_name('C.${fn_node.value}'))
					is_c_call = true
				} else if g.is_flag_enum_method(fn_node) {
					g.gen_flag_enum_call(node)
					return
				} else if base.kind == .ident && !base_is_local && g.has_import_alias(base.value) {
					mod := g.import_alias_module(base.value) or { '' }
					full_name := '${mod}.${fn_node.value}'
					if full_name in g.tc.type_aliases || full_name in g.tc.structs
						|| full_name in g.tc.enum_names || full_name in g.tc.sum_types {
						target_type := g.tc.parse_type(full_name)
						ct := g.tc.c_type(target_type)
						if target_type is types.SumType && node.children_count > 1 {
							g.gen_sum_cast_expr(target_type, g.a.child(&node, 1))
						} else {
							g.write('(${ct})(')
							for i in 1 .. node.children_count {
								if i > 1 {
									g.write(', ')
								}
								g.gen_expr(g.a.child(&node, i))
							}
							g.write(')')
						}
						return
					}
					g.write(g.cname(full_name))
					g.write('(')
					g.gen_call_args(full_name, node, 1)
					g.write(')')
					return
				} else if base.kind == .ident && !base_is_local
					&& g.static_method_fn_name(base.value, fn_node.value) != none {
					// `Type.method(...)` where the base ident names a (possibly
					// imported) type, not a value — e.g. `Animation.load(path)` inside
					// the type's own module. Resolve to the module-qualified static fn.
					static_fn := g.static_method_fn_name(base.value, fn_node.value) or { '' }
					g.write(g.direct_call_name(static_fn))
					g.write('(')
					// Lower the arguments through the ordinary call path so a static method
					// parameter that needs coercion — option/result wrapping, fn-pointer alias
					// callbacks, sum variants, fixed-array decay, variadic/`@[params]` handling —
					// is emitted against its expected type, not as the raw expression.
					g.gen_call_args(static_fn, node, 1)
					g.write(')')
					return
				} else if base.kind == .selector {
					inner := g.a.child_node(base, 0)
					inner_is_local := if inner.kind == .ident {
						(g.tc.cur_scope.lookup(inner.value) or { types.Type(types.void_) }) !is types.Void
					} else {
						false
					}
					if inner.kind == .ident && !inner_is_local {
						if mod := g.import_alias_module(inner.value) {
							full_name := '${mod}.${base.value}.${fn_node.value}'
							g.write(g.cname(full_name))
							g.write('(')
							g.gen_call_args(full_name, node, 1)
							g.write(')')
							return
						}
					}
					{
						base_type := g.usable_expr_type(g.a.child(fn_node, 0))
						clean_type := concrete_receiver_type(base_type)
						if g.gen_thread_wait_call(fn_node) {
							return
						}
						if g.gen_interface_method_call(node, fn_node, base_type) {
							return
						}
						if g.gen_fn_field_call(node, fn_node, base_type) {
							return
						}
						if arr := array_like_type(clean_type) {
							g.gen_array_method_call(node, fn_node, arr)
							return
						}
						if clean_type is types.ArrayFixed && fn_node.value == 'bytestr' {
							g.write('u8__vstring_with_len((u8*)')
							g.gen_expr(g.a.child(fn_node, 0))
							len_expr := g.fixed_array_len_value(clean_type)
							g.write(', ${len_expr})')
							return
						}
						if g.gen_pointer_builtin_method_call(node, fn_node, base_type) {
							return
						}
						if clean_type is types.Map {
							if fn_node.value == 'str' {
								g.gen_map_str_expr(g.a.child(fn_node, 0), base_type)
								return
							} else if fn_node.value == 'clone' {
								g.write('map__clone(')
								g.gen_map_ref_arg(g.a.child(fn_node, 0), base_type)
								g.write(')')
								return
							} else if fn_node.value in ['keys', 'values', 'delete', 'clear', 'free',
								'move', 'reserve'] {
								panic('map method `${fn_node.value}` should be lowered by v3 transform')
							}
						}
						if clean_type is types.String {
							if fn_node.value == 'to_owned' {
								g.write('string__clone(')
								g.gen_expr(g.a.child(fn_node, 0))
								g.write(')')
								return
							}
							method_name = 'string.${fn_node.value}'
							if method_name in g.tc.fn_param_types {
								is_method = true
								base_id = g.a.child(fn_node, 0)
								g.write_method_c_name(id, node, method_name)
							} else {
								g.write('string__${fn_node.value}(')
								g.gen_expr(g.a.child(fn_node, 0))
								for i in 1 .. node.children_count {
									g.write(', ')
									g.gen_expr(g.a.child(&node, i))
								}
								g.write(')')
								return
							}
						}
						if !is_method && (clean_type is types.Primitive
							|| clean_type is types.ISize || clean_type is types.USize
							|| clean_type is types.Rune) {
							tname := clean_type.name()
							prim_method := '${tname}.${fn_node.value}'
							if prim_method in g.tc.fn_param_types {
								is_method = true
								base_id = g.a.child(fn_node, 0)
								g.write(g.cname(prim_method))
							} else {
								mut prim_found := false
								if alias_method := g.find_alias_method(tname, fn_node.value) {
									is_method = true
									prim_found = true
									base_id = g.a.child(fn_node, 0)
									g.write(g.cname(alias_method))
								}
								if !prim_found {
									alt_name := g.find_prim_method(fn_node.value)
									if alt_name.len > 0 {
										is_method = true
										base_id = g.a.child(fn_node, 0)
										g.write(alt_name)
									}
								}
							}
						}
						if !is_method {
							mut struct_name := clean_type.name()
							if clean_type is types.Struct {
								struct_name = clean_type.name
							}
							method_name = g.resolve_method_name(struct_name, fn_node.value)
							if method_name.len == 0 {
								for alias, target in g.tc.type_aliases {
									if target == struct_name {
										alias_method := '${alias}.${fn_node.value}'
										if alias_method in g.tc.fn_param_types {
											method_name = alias_method
											break
										}
									}
								}
							}
							if method_name.len > 0 && method_name in g.tc.fn_param_types {
								is_method = true
								base_id = g.a.child(fn_node, 0)
								g.write_method_c_name(id, node, method_name)
							} else {
								str_method := 'string.${fn_node.value}'
								if str_method in g.tc.fn_param_types {
									is_method = true
									method_name = str_method
									base_id = g.a.child(fn_node, 0)
									g.write(g.cname(str_method))
								} else if embedded_method := g.embedded_method_name_for_type(clean_type,
									fn_node.value)
								{
									is_method = true
									method_name = embedded_method
									base_id = g.a.child(fn_node, 0)
									g.write(g.cname(embedded_method))
								} else if struct_name.len > 0 {
									is_method = true
									base_id = g.a.child(fn_node, 0)
									fallback_method := '${struct_name}.${fn_node.value}'
									method_name = fallback_method
									g.write_method_c_name(id, node, fallback_method)
								} else {
									g.gen_expr(g.a.child(&node, 0))
								}
							}
						}
					}
				} else if base.kind == .ident
					&& (base.value in g.tc.structs || base.value in g.tc.enum_names || g.tc.qualify_name(base.value) in g.tc.structs
					|| g.tc.qualify_name(base.value) in g.tc.enum_names) {
					qname := if base.value in g.tc.structs || base.value in g.tc.enum_names {
						base.value
					} else {
						g.tc.qualify_name(base.value)
					}
					static_name := '${qname}.${fn_node.value}'
					g.write(g.cname(static_name))
					g.write('(')
					for i in 1 .. node.children_count {
						if i > 1 {
							g.write(', ')
						}
						g.gen_expr(g.a.child(&node, i))
					}
					g.write(')')
					return
				} else {
					base_type := g.usable_expr_type(g.a.child(fn_node, 0))
					clean_type := concrete_receiver_type(base_type)
					if cgen_is_channel_close_receiver_type(clean_type) && fn_node.value == 'close' {
						g.gen_channel_close_call(g.a.child(fn_node, 0), node)
						return
					}
					if g.gen_thread_wait_call(fn_node) {
						return
					}
					if g.gen_interface_method_call(node, fn_node, base_type) {
						return
					}
					if g.gen_fn_field_call(node, fn_node, base_type) {
						return
					}
					if arr := array_like_type(clean_type) {
						g.gen_array_method_call(node, fn_node, arr)
						return
					}
					if clean_type is types.ArrayFixed && fn_node.value == 'bytestr' {
						g.write('u8__vstring_with_len((u8*)')
						g.gen_expr(g.a.child(fn_node, 0))
						len_expr := g.fixed_array_len_value(clean_type)
						g.write(', ${len_expr})')
						return
					}
					if g.gen_pointer_builtin_method_call(node, fn_node, base_type) {
						return
					}
					if clean_type is types.Map {
						if fn_node.value == 'str' {
							g.gen_map_str_expr(g.a.child(fn_node, 0), base_type)
							return
						} else if fn_node.value == 'clone' {
							g.write('map__clone(')
							g.gen_map_ref_arg(g.a.child(fn_node, 0), base_type)
							g.write(')')
							return
						} else if fn_node.value in ['keys', 'values', 'delete', 'clear', 'free',
							'move', 'reserve'] {
							panic('map method `${fn_node.value}` should be lowered by v3 transform')
						}
					}
					if clean_type is types.String {
						if fn_node.value == 'to_owned' {
							g.write('string__clone(')
							g.gen_expr(g.a.child(fn_node, 0))
							g.write(')')
							return
						}
						method_name = 'string.${fn_node.value}'
						if method_name in g.tc.fn_param_types {
							is_method = true
							base_id = g.a.child(fn_node, 0)
							g.write_method_c_name(id, node, method_name)
						} else {
							g.write('string__${fn_node.value}(')
							g.gen_expr(g.a.child(fn_node, 0))
							for i in 1 .. node.children_count {
								g.write(', ')
								g.gen_expr(g.a.child(&node, i))
							}
							g.write(')')
							return
						}
					}
					if !is_method && (clean_type is types.Void || clean_type is types.Primitive)
						&& fn_node.value in ['vstring', 'vstring_with_len'] {
						g.write('u8__${fn_node.value}((u8*)')
						g.gen_expr(g.a.child(fn_node, 0))
						for i in 1 .. node.children_count {
							g.write(', ')
							g.gen_expr(g.a.child(&node, i))
						}
						g.write(')')
						return
					}
					if !is_method && clean_type is types.Struct && clean_type.name == 'IError' {
						if fn_node.value == 'msg' {
							g.gen_expr(g.a.child(fn_node, 0))
							g.write('.message')
							return
						} else if fn_node.value == 'code' {
							g.gen_expr(g.a.child(fn_node, 0))
							g.write('.code')
							return
						}
					}
					if !is_method && clean_type is types.Struct && clean_type.name == 'array'
						&& fn_node.value == 'free' {
						g.write('array__free(')
						if base_type is types.Pointer {
							g.gen_expr(g.a.child(fn_node, 0))
						} else {
							g.write('&')
							g.gen_expr(g.a.child(fn_node, 0))
						}
						g.write(')')
						return
					}
					if !is_method && (clean_type is types.Primitive
						|| clean_type is types.ISize || clean_type is types.USize
						|| clean_type is types.Rune) {
						tname := clean_type.name()
						prim_method := '${tname}.${fn_node.value}'
						if prim_method in g.tc.fn_param_types {
							is_method = true
							base_id = g.a.child(fn_node, 0)
							g.write(g.cname(prim_method))
						} else {
							mut prim_found := false
							if alias_method := g.find_alias_method(tname, fn_node.value) {
								is_method = true
								prim_found = true
								base_id = g.a.child(fn_node, 0)
								g.write(g.cname(alias_method))
							}
							if !prim_found {
								alt_name := g.find_prim_method(fn_node.value)
								if alt_name.len > 0 {
									is_method = true
									base_id = g.a.child(fn_node, 0)
									g.write(alt_name)
								}
							}
						}
					}
					if !is_method {
						// Static method on a named type / type alias (e.g. `SimdFloat4.new(...)`,
						// where `SimdFloat4` names a type, not a value). The base ident resolves to
						// no value type, so handle it before the instance-method fallback.
						base_node_s := g.a.child_node(fn_node, 0)
						if base_node_s.kind == .ident {
							if static_fn := g.static_method_fn_name(base_node_s.value,
								fn_node.value)
							{
								g.write(g.direct_call_name(static_fn))
								g.write('(')
								for i in 1 .. node.children_count {
									if i > 1 {
										g.write(', ')
									}
									g.gen_expr(g.a.child(&node, i))
								}
								g.write(')')
								return
							}
						}
					}
					if !is_method {
						mut struct_name := clean_type.name()
						if clean_type is types.Struct {
							struct_name = clean_type.name
						}
						method_name = g.resolve_method_name(struct_name, fn_node.value)
						if method_name.len == 0 {
							for alias, target in g.tc.type_aliases {
								if target == struct_name {
									alias_method := '${alias}.${fn_node.value}'
									if alias_method in g.tc.fn_param_types {
										method_name = alias_method
										break
									}
								}
							}
						}
						if method_name.len > 0 && method_name in g.tc.fn_param_types {
							is_method = true
							base_id = g.a.child(fn_node, 0)
							g.write_method_c_name(id, node, method_name)
						} else {
							str_method := 'string.${fn_node.value}'
							if str_method in g.tc.fn_param_types {
								is_method = true
								method_name = str_method
								base_id = g.a.child(fn_node, 0)
								g.write(g.cname(str_method))
							} else if embedded_method := g.embedded_method_name_for_type(clean_type,
								fn_node.value)
							{
								is_method = true
								method_name = embedded_method
								base_id = g.a.child(fn_node, 0)
								g.write(g.cname(embedded_method))
							} else if struct_name.len > 0 {
								is_method = true
								base_id = g.a.child(fn_node, 0)
								fallback_method := '${struct_name}.${fn_node.value}'
								method_name = fallback_method
								g.write_method_c_name(id, node, fallback_method)
							} else {
								g.gen_expr(g.a.child(&node, 0))
							}
						}
					}
					// !is_method
				}
			} else {
				fn_id := g.a.child(&node, 0)
				fn_ident := g.a.nodes[int(fn_id)]
				if fn_ident.kind == .ident {
					qname := g.tc.qualify_name(fn_ident.value)
					if fn_ident.value in g.tc.type_aliases || qname in g.tc.type_aliases
						|| fn_ident.value in g.tc.structs || qname in g.tc.structs
						|| fn_ident.value in g.tc.enum_names || qname in g.tc.enum_names
						|| fn_ident.value in g.tc.sum_types || qname in g.tc.sum_types {
						type_name := if fn_ident.value in g.tc.type_aliases
							|| fn_ident.value in g.tc.structs || fn_ident.value in g.tc.enum_names
							|| fn_ident.value in g.tc.sum_types {
							fn_ident.value
						} else {
							qname
						}
						target_type := g.tc.parse_type(type_name)
						ct := g.tc.c_type(target_type)
						if target_type is types.SumType && node.children_count > 1 {
							g.gen_sum_cast_expr(target_type, g.a.child(&node, 1))
						} else {
							g.write('(${ct})(')
							for i in 1 .. node.children_count {
								if i > 1 {
									g.write(', ')
								}
								g.gen_expr(g.a.child(&node, i))
							}
							g.write(')')
						}
						return
					}
					call_key := g.call_key(id, fn_ident.value)
					looked_up := g.tc.cur_scope.lookup(fn_ident.value) or {
						types.Type(types.void_)
					}
					if looked_up !is types.Void && fn_type_from(looked_up) != none {
						emitted_callee_name = g.cname(fn_ident.value)
						g.write(emitted_callee_name)
					} else if specialized := g.specialized_generic_plain_fn_name_for_call(id, node,
						fn_ident.value)
					{
						emitted_callee_name = g.direct_call_name_for_call_node(id, node,
							specialized)
						g.write(emitted_callee_name)
					} else if g.resolved_name_is_generic_plain(call_key)
						&& g.plain_concrete_fn_name_shadows_generic(fn_ident.value) {
						emitted_callee_name = g.cname(fn_ident.value)
						g.write(emitted_callee_name)
					} else if call_key in g.tc.fn_ret_types || call_key in g.tc.fn_param_types {
						emitted_callee_name = g.direct_call_name_for_call_node(id, node, call_key)
						g.write(emitted_callee_name)
					} else if specialized := g.specialized_generic_method_name_for_call_with_arg_count(id,
						fn_ident.value, -1)
					{
						emitted_callee_name = g.cname(specialized)
						g.write(emitted_callee_name)
					} else {
						emitted_callee_name = g.cname(fn_ident.value)
						g.write(emitted_callee_name)
					}
				} else {
					needs_callee_parens := fn_ident.kind !in [.ident, .selector]
					if needs_callee_parens {
						g.write('(')
					}
					g.gen_expr(fn_id)
					if needs_callee_parens {
						g.write(')')
					}
				}
			}
			g.write('(')
			actual_fn := if is_method {
				g.method_call_name_for_call(id, node, method_name)
			} else if target_name.contains('.') {
				g.call_key(id, target_name)
			} else {
				g.call_key(id, fn_name)
			}
			mut param_types := g.param_types_for(actual_fn, fn_name)
			if emitted_callee_name.len > 0 {
				emitted_param_types := g.param_types_for(emitted_callee_name, emitted_callee_name)
				if emitted_param_types.len > 0 {
					param_types = emitted_param_types.clone()
				}
			}
			if !is_method {
				if fn_value_params := g.fn_value_call_param_types(g.a.child(&node, 0)) {
					param_types = fn_value_params.clone()
				}
			}
			callee_uses_specialized_generic_abi :=
				g.call_callee_uses_specialized_generic_abi(g.a.child(&node, 0))
			concrete_optional_args := g.call_uses_concrete_optional_params(actual_fn)
				|| g.call_uses_concrete_optional_params(fn_name)
				|| g.call_uses_concrete_optional_params(target_name)
				|| g.call_uses_concrete_optional_params(emitted_callee_name)
				|| (callee_uses_specialized_generic_abi && params_have_optional_result(param_types))
			if param_types.len == 0 && !is_method {
				// Calling through a function-typed value (a generic `f F` parameter or a
				// local fn variable): the callee is not a named function, so recover the
				// parameter types from the value's own function type. This lets `mut` /
				// pointer arguments receive their `&` exactly as a direct call would.
				if ft := fn_type_from(g.tc.resolve_type(g.a.child(&node, 0))) {
					param_types = ft.params.clone()
				}
			}
			mut arg_start := 1
			if is_method {
				base_type := g.receiver_base_type(base_id)
				wants_ptr := (param_types.len > 0 && param_types[0] is types.Pointer)
					|| g.method_decl_receiver_wants_ptr(actual_fn, method_name, fn_name)
					|| g.method_receiver_is_mut(actual_fn)
					|| g.method_receiver_is_mut(g.cname(actual_fn))
					|| g.method_receiver_is_mut(method_name)
					|| g.method_receiver_is_mut(g.cname(method_name))
					|| g.mut_receiver_arg_wants_addr(actual_fn, base_id)
				receiver_type_name := g.type_lookup_name(base_type)
				method_short := method_name.all_after_last('.').all_after_last('__')
				atomic_receiver_wants_ptr := method_name.starts_with('stdatomic.AtomicVal_')
					|| method_name.starts_with('stdatomic.AtomicVal.')
					|| method_name.starts_with('AtomicVal_')
					|| method_name.starts_with('AtomicVal.')
					|| (receiver_type_name.contains('AtomicVal')
					&& method_short in ['load', 'store', 'add', 'sub', 'swap', 'compare_and_swap'])
				receiver_wants_ptr := wants_ptr || atomic_receiver_wants_ptr
					|| g.method_receiver_is_mut(method_name)
				receiver_wants_shared := g.fn_param_is_shared_for_call(0, actual_fn,
					emitted_callee_name, method_name, fn_name)
				if receiver_wants_shared && (g.gen_shared_local_receiver_arg(base_id)
					|| g.gen_shared_storage_expr(base_id)) {
					arg_start = 1
				} else if param_types.len > 0
					&& g.gen_embedded_method_receiver(base_id, base_type, param_types[0], receiver_wants_ptr) {
					arg_start = 1
				} else if g.gen_current_mut_param_method_receiver(base_id, receiver_wants_ptr) {
					arg_start = 1
				} else {
					mut is_ptr_base := base_type is types.Pointer
						|| g.receiver_ident_storage_is_pointer(base_id)
					base_node := g.a.nodes[int(base_id)]
					if base_node.kind == .ident && g.local_storage_is_shared(base_node.value)
						&& !receiver_wants_shared {
						is_ptr_base = false
					}
					if receiver_wants_ptr && base_node.kind == .ident && base_type !is types.Pointer
						&& !g.receiver_ident_storage_is_pointer(base_id) {
						is_ptr_base = false
					}
					// A `string.*` method always takes its receiver by value. If the
					// receiver mis-resolves to a char pointer (e.g. a `const x =
					// os.getenv(..)` whose type was inferred as `&char`), the actual
					// storage is still a `string`, so do not dereference it.
					if is_ptr_base && method_name.starts_with('string.')
						&& base_type is types.Pointer && base_type.base_type is types.Char {
						is_ptr_base = false
					}
					if receiver_wants_ptr && !is_ptr_base {
						g.write('&')
					} else if !receiver_wants_ptr && is_ptr_base {
						g.write('*')
					}
					g.gen_expr(base_id)
					arg_start = 1
				}
			}
			num_call_args := node.children_count - arg_start
			is_c_variadic_fn := is_c_call && (g.tc.c_variadic_fns[actual_fn] or { false })
			is_variadic_fn := !is_method && !is_c_variadic_fn && ((g.tc.fn_variadic[actual_fn] or {
				false
			}) || g.fn_decl_is_variadic(actual_fn, fn_name))
			variadic_idx := if is_variadic_fn && param_types.len > 0
				&& param_types[param_types.len - 1] is types.Array {
				param_types.len - 1
			} else {
				-1
			}
			typed_param_count := if is_c_variadic_fn && param_types.len > 0
				&& param_types[param_types.len - 1] is types.Array {
				param_types.len - 1
			} else {
				param_types.len
			}
			// A veb handler whose hidden `Context` parameter (param index 1, right
			// after the receiver) was omitted by the caller forwards the enclosing
			// handler's `ctx` in that slot so the remaining explicit arguments still
			// line up with their parameters.
			expected_non_ctx := if is_method { param_types.len - 1 } else { param_types.len }
			forward_ctx := param_types.len > 1 && g.is_implicit_veb_ctx_param(param_types[1])
				&& num_call_args < expected_non_ctx && g.cur_scope_has_ctx()
			if forward_ctx && is_method {
				g.write(', ctx')
			}
			mut emitted_arg_count := 0
			for i in arg_start .. node.children_count {
				mut arg_idx := if is_method { i } else { i - 1 }
				if forward_ctx && (is_method || i - arg_start >= 1) {
					arg_idx++
				}
				arg_id := g.a.child(&node, i)
				if int(arg_id) < 0 {
					continue
				}
				arg_node := g.a.nodes[int(arg_id)]
				if param_types.len > 0 && !is_c_variadic_fn && variadic_idx < 0
					&& arg_idx >= typed_param_count {
					continue
				}
				if is_method || emitted_arg_count > 0 {
					g.write(', ')
				}
				emitted_arg_count++
				if arg_node.kind == .field_init && variadic_idx >= 0 && arg_idx == variadic_idx {
					variadic_type := param_types[variadic_idx]
					if variadic_type is types.Array {
						if variadic_type.elem_type is types.Struct {
							c_elem := g.tc.c_type(variadic_type.elem_type)
							g.write('new_array_from_c_array(1, 1, sizeof(${c_elem}), (${c_elem}[]){')
							g.gen_params_struct_arg(variadic_type.elem_type, node, i)
							g.write('})')
							break
						}
					}
				}
				if arg_node.kind == .field_init {
					// `@[params]` struct argument: trailing `key: value` args form a struct literal
					ptyp := if arg_idx < typed_param_count {
						param_types[arg_idx]
					} else {
						types.Type(types.void_)
					}
					g.gen_params_struct_arg(ptyp, node, i)
					break
				}
				if arg_node.kind == .sizeof_expr {
					g.write('sizeof(${g.sizeof_target(arg_node.value)})')
					continue
				}
				if !is_c_call && arg_idx < typed_param_count {
					arg_param_is_shared := g.fn_param_is_shared_for_call(arg_idx, actual_fn,
						target_name, emitted_callee_name, fn_name)
					if arg_param_is_shared && (g.gen_shared_local_receiver_arg(arg_id)
						|| g.gen_shared_storage_expr(arg_id)) {
						continue
					}
				}
				if !is_c_call && arg_idx < typed_param_count
					&& param_types[arg_idx] !is types.Pointer
					&& g.gen_addressed_byvalue_arg(arg_node, param_types[arg_idx]) {
					continue
				}
				if !is_c_call && arg_idx < param_types.len
					&& g.gen_fixed_array_pointer_lvalue_arg(arg_id, param_types[arg_idx]) {
					continue
				}
				if fixed := array_fixed_type(g.tc.resolve_type(arg_id)) {
					g.gen_fixed_array_data_arg(arg_id, fixed)
					continue
				}
				if !is_c_call && arg_idx < typed_param_count {
					if fixed := array_fixed_type(param_types[arg_idx]) {
						g.gen_fixed_array_data_arg(arg_id, fixed)
						continue
					}
				}
				if !is_c_call && arg_idx < typed_param_count
					&& g.gen_pointer_arg_from_array_literal(arg_node, param_types[arg_idx]) {
					continue
				}
				cb_param := if arg_idx >= 0 && arg_idx < typed_param_count {
					param_types[arg_idx]
				} else {
					types.Type(types.void_)
				}
				if g.gen_special_c_callback_arg(target_name, arg_idx, arg_id, cb_param) {
					continue
				}
				if variadic_idx >= 0 && arg_idx == variadic_idx {
					variadic_type := param_types[variadic_idx]
					if variadic_type is types.Array {
						if spread_id := g.spread_arg_child(arg_node) {
							g.gen_expr_with_expected_type(spread_id, variadic_type)
							continue
						}
						if num_call_args > param_types.len {
							c_elem := g.tc.c_type(variadic_type.elem_type)
							count := num_call_args - variadic_idx
							g.write('new_array_from_c_array(${count}, ${count}, sizeof(${c_elem}), (${c_elem}[]){')
							is_voidptr_variadic := variadic_elem_is_voidptr(variadic_type.elem_type)
							for j in i .. node.children_count {
								if j > i {
									g.write(', ')
								}
								if is_voidptr_variadic {
									g.gen_voidptr_variadic_arg(g.a.child(&node, j))
								} else {
									g.gen_expr_with_expected_type(g.a.child(&node, j),
										variadic_type.elem_type)
								}
							}
							g.write('})')
							break
						}
						arg_type := g.tc.resolve_type(arg_id)
						if arg_type !is types.Array || arg_node.kind == .struct_init {
							c_elem := g.tc.c_type(variadic_type.elem_type)
							g.write('new_array_from_c_array(1, 1, sizeof(${c_elem}), (${c_elem}[]){')
							if variadic_elem_is_voidptr(variadic_type.elem_type) {
								g.gen_voidptr_variadic_arg(arg_id)
							} else {
								g.gen_expr_with_expected_type(arg_id, variadic_type.elem_type)
							}
							g.write('})')
							continue
						}
					}
				}
				mut needs_addr := false
				if !is_c_call && arg_idx < typed_param_count
					&& param_types[arg_idx] is types.Pointer && !(arg_node.kind == .prefix
					&& arg_node.op == .amp) && !g.arg_is_null_pointer_literal(arg_id, arg_node) {
					arg_type := g.usable_expr_type(arg_id)
					arg_is_pointer_param := arg_node.kind == .ident && (g.current_param_type(arg_node.value) or {
						types.Type(types.void_)
					}) is types.Pointer
					arg_is_pointer_global := arg_node.kind == .ident && (g.global_type_for_ident(arg_node.value) or {
						types.Type(types.void_)
					}) is types.Pointer
					value_local_mut_receiver := arg_idx == 0 && (g.method_receiver_is_mut(fn_name)
						|| g.method_receiver_is_mut(g.direct_call_name(fn_name)))
						&& arg_node.kind == .ident && !g.local_storage_is_pointer(arg_node.value)
						&& !arg_is_pointer_param && !arg_is_pointer_global
						&& arg_type !is types.Pointer
					if (arg_type !is types.Pointer || value_local_mut_receiver)
						&& !g.c_string_pointer_arg(arg_node, param_types[arg_idx]) {
						needs_addr = !(arg_node.kind == .ident
							&& (g.local_storage_is_pointer(arg_node.value) || arg_is_pointer_global))
					}
				}
				if !is_c_call && !needs_addr && arg_idx == 0
					&& (g.mut_receiver_arg_wants_addr(actual_fn, arg_id)
					|| g.mut_receiver_arg_wants_addr(emitted_callee_name, arg_id)
					|| g.mut_receiver_arg_wants_addr(fn_name, arg_id)) {
					needs_addr = true
				}
				if !is_c_call && arg_idx < typed_param_count {
					pt := param_types[arg_idx]
					if pt is types.Enum {
						g.expected_enum = pt.name
					}
				}
				if !is_c_call && arg_idx < typed_param_count
					&& g.gen_mut_sum_lvalue_arg(arg_id, param_types[arg_idx]) {
					g.expected_enum = ''
					continue
				}
				if !is_c_call && arg_idx < typed_param_count {
					if child_id := g.addressed_rvalue_arg(arg_node) {
						pt := param_types[arg_idx]
						if g.gen_addressed_rvalue_arg(child_id, pt) {
							g.expected_enum = ''
							continue
						}
					}
				}
				is_rvalue := arg_node.kind == .call
					|| (arg_node.kind == .index && arg_node.value == 'range')
					|| g.arg_is_const_ident(arg_node)
				if needs_addr && g.arg_is_const_ident(arg_node) {
					pt := param_types[arg_idx]
					ct := g.tc.c_type(types.unwrap_pointer(pt))
					g.write('(${ct}[]){')
					g.gen_expr_with_expected_type(arg_id, types.unwrap_pointer(pt))
					g.write('}')
				} else if needs_addr && is_rvalue {
					pt := param_types[arg_idx]
					ct := g.tc.c_type(types.unwrap_pointer(pt))
					if g.c_typedef_nil_call(arg_id) {
						g.write('NULL')
					} else {
						g.write('&((${ct}[]){')
						g.gen_expr_with_expected_type(arg_id, types.unwrap_pointer(pt))
						g.write('})[0]')
					}
				} else if needs_addr && g.gen_mut_sum_lvalue_arg(arg_id, param_types[arg_idx]) {
					// handled
				} else {
					if needs_addr {
						g.write('&')
					}
					emitted_variant := !needs_addr && !is_c_call && arg_idx < typed_param_count
						&& g.gen_sum_variant_arg(arg_id, param_types[arg_idx])
					if !emitted_variant {
						if !is_c_call && arg_idx < typed_param_count
							&& g.gen_optional_arg_with_abi(arg_id, param_types[arg_idx], concrete_optional_args) {
							// handled
						} else if !is_c_call && arg_idx < typed_param_count
							&& g.gen_pointer_backed_param_arg(arg_id, param_types[arg_idx]) {
							// handled
						} else if !is_c_call && arg_idx < typed_param_count {
							g.gen_expr_with_expected_type(arg_id, param_types[arg_idx])
						} else {
							g.gen_expr(arg_id)
						}
					}
				}
				g.expected_enum = ''
				// A no-arg delegation leaves the forwarded ctx as the final argument;
				// emit it here, right after the receiver, for the lowered free call.
				if forward_ctx && !is_method && i - arg_start == 0 {
					g.write(', ctx')
				}
			}
			// Count the forwarded ctx (if any) as already supplied.
			actual_args := emitted_arg_count + (if forward_ctx { 1 } else { 0 })
			expected_args := if is_method {
				param_types.len - 1
			} else {
				param_types.len
			}
			if !is_c_call && expected_args > 0 && actual_args < expected_args {
				for pi in actual_args .. expected_args {
					if is_method || pi > 0 {
						g.write(', ')
					}
					pidx := if is_method { pi + 1 } else { pi }
					pt := param_types[pidx]
					// The implicit veb `Context` parameter is supplied from the
					// enclosing handler's `ctx`, not a zero/default value.
					if g.is_implicit_veb_ctx_param(pt) && g.cur_scope_has_ctx() {
						g.write('ctx')
					} else {
						g.gen_default_value_for_type(pt)
					}
				}
			}
			g.write(')')
		}
	}
}

// receiver_base_type supports receiver base type handling for FlatGen.
fn (g &FlatGen) receiver_base_type(base_id flat.NodeId) types.Type {
	if int(base_id) < 0 {
		return types.Type(types.void_)
	}
	base := g.a.nodes[int(base_id)]
	if base.kind == .ident {
		if typ := g.tc.cur_scope.lookup(base.value) {
			return typ
		}
		if typ := g.current_param_type(base.value) {
			return typ
		}
		if typ := g.current_param_map_type(base.value) {
			return typ
		}
		if typ := g.global_type_for_ident(base.value) {
			return typ
		}
	}
	return g.tc.resolve_type(base_id)
}

fn (g &FlatGen) global_type_for_ident(name string) ?types.Type {
	if typ := g.global_types[name] {
		return typ
	}
	if mod := g.global_modules[name] {
		qname := qualify_name_in_module(mod, name)
		if typ := g.global_types[qname] {
			return typ
		}
	}
	qname := qualify_name_in_module(g.tc.cur_module, name)
	if qname != name {
		if typ := g.global_types[qname] {
			return typ
		}
	}
	return none
}

fn (g &FlatGen) const_ident_type(name string) ?types.Type {
	if typ := g.tc.const_types[name] {
		return typ
	}
	qname := qualify_name_in_module(g.tc.cur_module, name)
	if qname != name {
		if typ := g.tc.const_types[qname] {
			return typ
		}
	}
	if key := g.tc.const_suffixes[name] {
		if key.len > 0 {
			if typ := g.tc.const_types[key] {
				return typ
			}
		}
	}
	return none
}

fn (g &FlatGen) const_type_for_arg_node(node flat.Node) ?types.Type {
	if node.kind == .ident {
		return g.const_ident_type(node.value)
	}
	if node.kind == .selector && node.children_count > 0 {
		base := g.a.child_node(&node, 0)
		if base.kind != .ident {
			return none
		}
		mod := g.import_alias_module(base.value) or { base.value }
		for key in ['${mod}.${node.value}', '${base.value}.${node.value}'] {
			if typ := g.tc.const_types[key] {
				return typ
			}
		}
	}
	return none
}

fn (mut g FlatGen) gen_current_mut_param_method_receiver(base_id flat.NodeId, wants_ptr bool) bool {
	if !wants_ptr {
		return false
	}
	if int(base_id) < 0 || int(base_id) >= g.a.nodes.len {
		return false
	}
	base := g.a.nodes[int(base_id)]
	if base.kind != .ident || !g.current_param_is_mut(base.value) {
		return false
	}
	g.write(g.cname(base.value))
	return true
}

fn (mut g FlatGen) gen_pointer_backed_param_arg(arg_id flat.NodeId, expected types.Type) bool {
	if int(arg_id) < 0 || int(arg_id) >= g.a.nodes.len {
		return false
	}
	arg := g.a.nodes[int(arg_id)]
	if arg.kind != .ident {
		return false
	}
	param_type := g.current_param_type(arg.value) or { return false }
	if expected is types.Pointer {
		expected_ptr := expected as types.Pointer
		if param_type is types.Pointer {
			if !g.type_names_match(param_type.base_type, expected_ptr.base_type) {
				return false
			}
			g.write(g.cname(arg.value))
			return true
		}
	}
	return false
}

fn (mut g FlatGen) method_decl_receiver_wants_ptr(actual_fn string, method_name string, fallback string) bool {
	for name in [actual_fn, g.cname(actual_fn), method_name, g.cname(method_name), fallback,
		g.cname(fallback)] {
		params := g.param_types_from_decl(name, fallback)
		if params.len > 0 {
			return params[0] is types.Pointer
		}
	}
	return false
}

fn (g &FlatGen) method_receiver_is_mut(method_name string) bool {
	return g.fn_decl_mut_receivers[method_name] or { false }
}

fn (mut g FlatGen) mut_receiver_arg_wants_addr(fn_name string, arg_id flat.NodeId) bool {
	if int(arg_id) < 0 || int(arg_id) >= g.a.nodes.len {
		return false
	}
	arg_node := g.a.nodes[int(arg_id)]
	if arg_node.kind != .ident {
		return false
	}
	if g.local_storage_is_shared(arg_node.value) {
		return !g.fn_param_is_shared_for_call(0, fn_name, '', '', '')
			&& g.fn_first_param_is_mut_receiver(fn_name)
	}
	if g.local_storage_is_pointer(arg_node.value) {
		return false
	}
	arg_is_pointer_param := (g.current_param_type(arg_node.value) or { types.Type(types.void_) }) is types.Pointer
	if arg_is_pointer_param {
		return false
	}
	arg_type := g.usable_expr_type(arg_id)
	if arg_type is types.Pointer {
		return false
	}
	return g.fn_first_param_is_mut_receiver(fn_name)
}

// fn_first_param_is_mut_receiver reports whether a call target's first
// parameter is a mut receiver/pointer. Pure per fn name given the fixed
// signature tables, and asked once per call site — memoized.
fn (mut g FlatGen) fn_first_param_is_mut_receiver(fn_name string) bool {
	if !isnil(g.mut_recv_facts) {
		if cached := g.mut_recv_facts.entries[fn_name] {
			return cached > 0
		}
	}
	result := g.fn_first_param_is_mut_receiver_uncached(fn_name)
	if !isnil(g.mut_recv_facts) {
		mut cache := g.mut_recv_facts
		cache.entries[fn_name] = if result { i8(1) } else { i8(-1) }
	}
	return result
}

fn (mut g FlatGen) fn_first_param_is_mut_receiver_uncached(fn_name string) bool {
	mut names := []string{}
	if fn_name.contains('.') || fn_name.contains('__') {
		names << fn_name
		names << g.cname(fn_name)
	}
	direct_name := g.direct_call_name(fn_name)
	if direct_name != fn_name && direct_name != g.cname(fn_name) {
		names << direct_name
		names << g.cname(direct_name)
	}
	for name in names {
		if params := g.fn_decl_param_types[name] {
			if params.len > 0 {
				return params[0] is types.Pointer
			}
		}
	}
	for name in names {
		if g.method_receiver_is_mut(name) {
			return true
		}
	}
	return false
}

fn (g &FlatGen) c_style_mut_receiver_arg_wants_addr(fn_name string, arg_id flat.NodeId) bool {
	if !fn_name.contains('__') {
		return false
	}
	arg_node := g.a.nodes[int(arg_id)]
	if arg_node.kind != .ident || g.local_storage_is_pointer(arg_node.value) {
		return false
	}
	arg_is_pointer_param := (g.current_param_type(arg_node.value) or { types.Type(types.void_) }) is types.Pointer
	if arg_is_pointer_param {
		return false
	}
	arg_type := g.usable_expr_type(arg_id)
	if arg_type is types.Pointer {
		return false
	}
	receiver_ct := g.tc.c_type(types.unwrap_pointer(arg_type))
	short_receiver := flattened_generic_struct_c_type_short_name(receiver_ct)
	if short_receiver.len == 0 || !fn_name.contains(short_receiver) {
		return false
	}
	method := fn_name.all_after_last('__')
	if method.len == 0 {
		return false
	}
	for name, is_mut in g.fn_decl_mut_receivers {
		if !is_mut {
			continue
		}
		if !name.contains(short_receiver) {
			continue
		}
		if name == method || name.ends_with('.${method}') || name.ends_with('__${method}') {
			return true
		}
	}
	return false
}

fn concrete_receiver_type(base_type types.Type) types.Type {
	clean_type := types.unwrap_pointer(base_type)
	if clean_type is types.Alias {
		return clean_type.base_type
	}
	return clean_type
}

fn cgen_is_channel_close_receiver_type(base_type types.Type) bool {
	clean_type := concrete_receiver_type(base_type)
	if clean_type is types.Channel {
		return true
	}
	mut name := clean_type.name().trim_space()
	for name.starts_with('&') {
		name = name[1..].trim_space()
	}
	return name == 'chan' || name.starts_with('chan ')
}

fn (g &FlatGen) receiver_storage_type(base_id flat.NodeId) ?types.Type {
	if int(base_id) < 0 || int(base_id) >= g.a.nodes.len {
		return none
	}
	node := g.a.nodes[int(base_id)]
	if node.kind == .selector && node.children_count > 0 {
		parent_type := g.tc.resolve_type(g.a.child(&node, 0))
		return g.field_type(parent_type, node.value)
	}
	return none
}

fn (g &FlatGen) receiver_ident_storage_is_pointer(base_id flat.NodeId) bool {
	if int(base_id) < 0 || int(base_id) >= g.a.nodes.len {
		return false
	}
	node := g.a.nodes[int(base_id)]
	return node.kind == .ident && g.local_storage_is_pointer(node.value)
}

fn (g &FlatGen) receiver_needs_address(base_id flat.NodeId, base_type types.Type) bool {
	if int(base_id) >= 0 && int(base_id) < g.a.nodes.len {
		node := g.a.nodes[int(base_id)]
		if node.kind == .ident && g.local_storage_is_pointer(node.value) {
			return false
		}
		if node.kind == .selector {
			if storage_type := g.receiver_storage_type(base_id) {
				return storage_type !is types.Pointer
			}
		}
	}
	return base_type !is types.Pointer
}

fn (g &FlatGen) interface_receiver_needs_address(base_id flat.NodeId, base_type types.Type) bool {
	if int(base_id) >= 0 && int(base_id) < g.a.nodes.len {
		node := g.a.nodes[int(base_id)]
		if node.kind == .ident {
			if typ := g.tc.cur_scope.lookup(node.value) {
				if typ is types.Pointer {
					clean := types.unwrap_pointer(typ)
					if clean is types.Interface {
						return false
					}
				}
			}
		}
	}
	actual := g.usable_expr_type(base_id)
	if actual is types.Pointer {
		clean := types.unwrap_pointer(actual)
		if clean is types.Interface {
			return false
		}
	}
	return g.receiver_needs_address(base_id, base_type)
}

fn (mut g FlatGen) gen_interface_method_call(node flat.Node, fn_node flat.Node, base_type types.Type) bool {
	clean0 := types.unwrap_pointer(base_type)
	mut clean := clean0
	if clean0 is types.Alias {
		clean = clean0.base_type
	}
	if clean !is types.Interface {
		return false
	}
	iface := clean as types.Interface
	if g.is_ierror_type_name(iface.name) {
		return false
	}
	if iface.name !in g.interfaces {
		return false
	}
	method_name := '${iface.name}.${fn_node.value}'
	param_types := g.interface_method_param_types(method_name) or { []types.Type{} }
	base_id := g.a.child(fn_node, 0)
	g.write(g.cname(method_name))
	g.write('(')
	if g.interface_receiver_needs_address(base_id, base_type) {
		g.write('&')
	}
	g.gen_expr(base_id)
	mut emitted_arg_count := 0
	for i in 1 .. node.children_count {
		arg_id := g.a.child(&node, i)
		if int(arg_id) < 0 {
			continue
		}
		arg_node := g.a.nodes[int(arg_id)]
		g.write(', ')
		emitted_arg_count++
		param_idx := i
		if param_idx < param_types.len {
			if g.gen_fixed_array_pointer_lvalue_arg(arg_id, param_types[param_idx]) {
				continue
			}
			if fixed := array_fixed_type(param_types[param_idx]) {
				g.gen_fixed_array_data_arg(arg_id, fixed)
				continue
			}
			if g.gen_pointer_arg_from_array_literal(arg_node, param_types[param_idx]) {
				continue
			}
			if g.gen_mut_sum_lvalue_arg(arg_id, param_types[param_idx]) {
				continue
			}
			if child_id := g.addressed_rvalue_arg(arg_node) {
				pt := param_types[param_idx]
				if g.gen_addressed_rvalue_arg(child_id, pt) {
					continue
				}
			}
			emitted_variant := g.gen_sum_variant_arg(arg_id, param_types[param_idx])
			if !emitted_variant {
				if g.gen_optional_arg(arg_id, param_types[param_idx]) {
					// handled
				} else {
					g.gen_expr_with_expected_type(arg_id, param_types[param_idx])
				}
			}
		} else {
			g.gen_expr(arg_id)
		}
	}
	expected_args := if param_types.len > 0 { param_types.len - 1 } else { 0 }
	if emitted_arg_count < expected_args {
		for pi in emitted_arg_count .. expected_args {
			g.write(', ')
			g.gen_default_value_for_type(param_types[pi + 1])
		}
	}
	g.write(')')
	return true
}

fn (g &FlatGen) call_target_name(id flat.NodeId) string {
	if int(id) < 0 || int(id) >= g.a.nodes.len {
		return ''
	}
	node := g.a.nodes[int(id)]
	match node.kind {
		.ident {
			if target := g.const_fn_call_target_name(node) {
				return target
			}
			return node.value
		}
		.selector {
			if target := g.const_fn_call_target_name(node) {
				return target
			}
			if node.children_count == 0 {
				return node.value
			}
			base := g.a.child_node(&node, 0)
			if base.kind == .ident {
				if mod := g.import_alias_module(base.value) {
					return '${mod}.${node.value}'
				}
				return '${base.value}.${node.value}'
			}
			return node.value
		}
		.index {
			if node.children_count > 0 {
				return g.call_target_name(g.a.child(&node, 0))
			}
			return node.value
		}
		else {
			return node.value
		}
	}
}

fn (g &FlatGen) const_fn_call_target_name(node flat.Node) ?string {
	key := g.const_key_for_call_target(node) or { return none }
	expr_id := g.tc.const_exprs[key] or { return none }
	if int(expr_id) < 0 || int(expr_id) >= g.a.nodes.len {
		return none
	}
	expr := g.a.nodes[int(expr_id)]
	match expr.kind {
		.ident, .selector, .index {
			target := g.call_target_name(expr_id)
			if target in g.tc.fn_ret_types || target in g.tc.fn_param_types
				|| g.cname(target) in g.tc.fn_ret_types || g.cname(target) in g.tc.fn_param_types {
				return target
			}
			typ := if node.kind == .selector {
				g.tc.selector_const_type(node) or { g.tc.const_types[key] or { return none } }
			} else {
				g.tc.const_types[key] or { return none }
			}
			if typ.name().starts_with('fn ') {
				return target
			}
			return none
		}
		else {
			return none
		}
	}
}

fn (g &FlatGen) const_key_for_call_target(node flat.Node) ?string {
	name := g.call_target_key(node)
	if name.len == 0 {
		return none
	}
	if name in g.tc.const_types {
		return name
	}
	if key := g.tc.const_suffixes[name] {
		if key.len > 0 {
			return key
		}
	}
	return none
}

fn (g &FlatGen) call_target_key(node flat.Node) string {
	match node.kind {
		.ident {
			if node.value.len == 0 {
				return ''
			}
			if g.tc.cur_module.len > 0 && g.tc.cur_module != 'main' && g.tc.cur_module != 'builtin' {
				qname := '${g.tc.cur_module}.${node.value}'
				if qname in g.tc.const_types {
					return qname
				}
			}
			return node.value
		}
		.selector {
			if node.children_count == 0 {
				return node.value
			}
			base := g.a.child_node(&node, 0)
			if base.kind != .ident {
				return ''
			}
			mod := g.import_alias_module(base.value) or { base.value }
			qname := '${mod}.${node.value}'
			if qname in g.tc.const_types {
				return qname
			}
			return '${base.value}.${node.value}'
		}
		.index {
			if node.children_count > 0 {
				return g.call_target_key(g.a.child_node(&node, 0))
			}
			return node.value
		}
		else {
			return ''
		}
	}
}

fn (g &FlatGen) call_has_selector_name(id flat.NodeId, name string) bool {
	if int(id) < 0 || int(id) >= g.a.nodes.len {
		return false
	}
	node := g.a.nodes[int(id)]
	if node.kind == .selector && node.value == name {
		return true
	}
	for i in 0 .. node.children_count {
		if g.call_has_selector_name(g.a.child(&node, i), name) {
			return true
		}
	}
	return false
}

fn (g &FlatGen) is_json_decode_target_name(target string) bool {
	// Only the old C-magic `json` module uses the cgen shortcut. `json2`/`x.json2`
	// are pure V and must compile through normal codegen, so they are not matched
	// here (matching them would inject cJSON calls and hijack json2's own
	// internal `encode`/`decode` functions).
	return target == 'json.decode' || (target == 'decode' && g.tc.cur_module == 'json')
}

fn (g &FlatGen) is_json_decode_call(id flat.NodeId, target string) bool {
	if resolved := g.tc.resolved_call_name(id) {
		if g.is_json_decode_target_name(resolved) {
			return true
		}
	}
	return g.is_json_decode_target_name(target)
}

fn (g &FlatGen) is_json_decode_self_call(target string, resolved string) bool {
	return g.tc.cur_module == 'json' && target == 'decode'
		&& resolved in ['', 'decode', 'json.decode']
}

fn (mut g FlatGen) gen_json_encode_call(node flat.Node) bool {
	if node.children_count < 2 {
		return false
	}
	arg_id := g.a.child(&node, 1)
	typ := types.unwrap_pointer(g.usable_expr_type(arg_id))
	expr := g.expr_to_string_with_expected_type(arg_id, typ)
	// Bind the argument to a single temporary so it is evaluated exactly once,
	// no matter how many times the field/enum expansion references it.
	tmp := g.tmp_name()
	encoded := g.json_encode_value_c_expr(typ, tmp) or { return false }
	g.write('({ ${g.value_c_type(typ)} ${tmp} = ${expr}; ${encoded}; })')
	return true
}

fn (mut g FlatGen) json_encode_value_c_expr(typ types.Type, expr string) ?string {
	clean := if typ is types.Alias { typ.base_type } else { typ }
	if clean is types.Enum {
		if cast := g.json_enum_number_cast(clean.name) {
			// `@[json_as_number]` enum: encode the numeric backing value, not the label.
			num_fn := if cast == 'u64' { 'u64__str' } else { 'i64__str' }
			return '${num_fn}((${cast})(${expr}))'
		}
		names, labels := g.json_enum_labels(clean.name)
		if names.len == 0 {
			return none
		}
		mut result := ''
		for i := names.len - 1; i >= 0; i-- {
			name := names[i]
			label := labels[name] or { name }
			sid := g.intern_string(label)
			encoded_label := 'v3_json_encode_string(_str_${sid})'
			value := g.enum_value_expr_for_type(clean.name, name) or {
				'${g.cname(clean.name)}__${g.cname(name)}'
			}
			if result.len == 0 {
				result = encoded_label
			} else {
				result = '((${expr}) == ${value} ? ${encoded_label} : ${result})'
			}
		}
		return result
	}
	if clean is types.String {
		// Escape the contents with a length-aware escaper (quotes, backslashes and
		// control characters), preserving embedded NUL bytes; cJSON_CreateString is
		// C-NUL-terminated and would truncate the V string.
		return 'v3_json_encode_string(${expr})'
	}
	if clean is types.Primitive {
		if clean.props.has(.boolean) {
			true_sid := g.intern_string('true')
			false_sid := g.intern_string('false')
			return '((bool)(${expr}) ? _str_${true_sid} : _str_${false_sid})'
		}
		if clean.props.has(.integer) {
			if clean.props.has(.unsigned) {
				return 'u64__str((u64)(${expr}))'
			}
			return 'i64__str((i64)(${expr}))'
		}
		if clean.props.has(.float) {
			return 'f64__str((double)(${expr}))'
		}
		return none
	}
	if clean is types.Struct {
		// The shortcut keys the wire format off the raw field name, so any struct
		// carrying json field attributes (`@[json: 'x']`, `@[skip]`, ...) would be
		// serialized with the wrong keys. Decline those and let normal codegen run.
		if g.json_struct_has_field_attrs(clean.name) {
			return none
		}
		fields := g.tc.structs[clean.name] or { return none }
		open := g.intern_string('{')
		close := g.intern_string('}')
		mut result := '_str_${open}'
		for i, field in fields {
			separator := if i == 0 { '' } else { ',' }
			prefix := g.intern_string('${separator}"${field.name}":')
			field_expr := '(${expr}).${g.cname(field.name)}'
			encoded := g.json_encode_value_c_expr(field.typ, field_expr) or { return none }
			result = 'string__plus(string__plus(${result}, _str_${prefix}), ${encoded})'
		}
		return 'string__plus(${result}, _str_${close})'
	}
	return none
}

fn (mut g FlatGen) gen_json_decode_call(node flat.Node) bool {
	if node.children_count < 3 {
		return false
	}
	ret_type := g.json_decode_result_type_for_call(node) or { return false }
	mut result_base := types.Type(types.void_)
	if ret_type is types.ResultType {
		result_base = ret_type.base_type
	} else {
		return false
	}

	base := types.unwrap_pointer(result_base)
	if base !is types.Struct {
		return false
	}
	struct_type := base as types.Struct
	fields := g.tc.structs[struct_type.name] or { return false }
	// The shortcut can only faithfully decode a fixed set of field types and
	// ignores json field attributes. Decline anything else so the result stays
	// an error instead of silently succeeding with dropped/renamed/rounded data.
	if g.json_struct_has_decode_field_attrs(struct_type.name) {
		return false
	}
	// A field with a default initializer (`n int = 5`) must keep that default when the
	// JSON omits it, but the fast path would zero it; decline such structs so the
	// normal decoder preserves the default instead of silently changing the value.
	if g.json_struct_has_field_default(struct_type.name) {
		return false
	}
	mut needs_exact_integer := false
	for field in fields {
		if !g.json_decode_value_supported(field.typ, 0) {
			return false
		}
		if g.json_decode_value_needs_exact_integer(field.typ, 0) {
			needs_exact_integer = true
		}
	}
	json_id := g.a.child(&node, 2)
	json_name := g.tmp_name()
	root_name := g.tmp_name()
	out_name := g.tmp_name()
	opt_ct := g.optional_type_name(ret_type)
	struct_ct := g.value_c_type(struct_type)
	g.write('({ string ${json_name} = ')
	g.gen_expr_with_expected_type(json_id, types.Type(types.string_))
	g.write('; cJSON* ${root_name} = cJSON_ParseWithLength((char*)${json_name}.str, (size_t)${json_name}.len); ')
	if needs_exact_integer {
		g.write('v3_json_preserve_number_tokens(${json_name}.str, ${json_name}.len, ${root_name}); ')
	}
	// A struct only decodes successfully from a JSON object; `null`, arrays,
	// strings and numbers must remain an error rather than a zero-valued struct.
	// A present field must also have the expected cJSON type, otherwise the decode
	// fails instead of silently substituting a default (e.g. `{"n":"bad"}` for int).
	mut checks := []string{}
	for field in fields {
		checks << g.json_decode_field_valid_expr(root_name, field)
	}
	valid_cond := if checks.len > 0 { checks.join(' && ') } else { 'true' }
	g.write('${opt_ct} ${out_name} = (${opt_ct}){0}; if (${root_name} != NULL) { if (cJSON_IsObject(${root_name})) { if (${valid_cond}) { ')
	g.write('${out_name}.ok = true; ${out_name}.value = (${struct_ct}){')
	for i, field in fields {
		if i > 0 {
			g.write(', ')
		}
		g.write('.${g.cname(field.name)} = ')
		g.gen_json_decode_field_expr(root_name, struct_type.name, field)
	}
	g.write('}; } } cJSON_Delete(${root_name}); } ${out_name}; })')
	return true
}

// json_decode_field_valid_expr returns a C boolean expression that is true when the
// JSON value for `field` is absent (defaulted) or present with the cJSON type the
// fast-path decoder can faithfully read. A present wrong-typed value fails the decode.
fn (mut g FlatGen) json_decode_field_valid_expr(root_name string, field types.StructField) string {
	item := if g.json_decode_struct_field_is_embedded(field) {
		root_name
	} else {
		'cJSON_GetObjectItemCaseSensitive(${root_name}, "${field.name}")'
	}
	return g.json_decode_value_valid_expr(item, field.typ)
}

fn (mut g FlatGen) json_decode_value_valid_expr(item string, typ types.Type) string {
	clean := if typ is types.Alias { typ.base_type } else { typ }
	// Mirror the json module: string fields accept strings and stringify objects/arrays,
	// bool fields require booleans, and numeric/enum fields tolerate wrong-typed or
	// unknown values by falling back to a default.
	if clean is types.String {
		return '(${item} == NULL || cJSON_IsString(${item}) || cJSON_IsObject(${item}) || cJSON_IsArray(${item}))'
	}
	if clean is types.Primitive && clean.props.has(.boolean) {
		return '(${item} == NULL || cJSON_IsBool(${item}))'
	}
	if clean is types.Array {
		item_name := g.tmp_name()
		elem_name := g.tmp_name()
		valid_name := g.tmp_name()
		elem_valid := g.json_decode_value_valid_expr(elem_name, clean.elem_type)
		return '({ cJSON* ${item_name} = ${item}; bool ${valid_name} = (${item_name} == NULL || cJSON_IsArray(${item_name})); if (${valid_name} && ${item_name} != NULL) { cJSON* ${elem_name} = NULL; cJSON_ArrayForEach(${elem_name}, ${item_name}) { if (!(${elem_valid})) { ${valid_name} = false; break; } } } ${valid_name}; })'
	}
	if clean is types.Pointer {
		inner := g.json_decode_value_valid_expr(item, clean.base_type)
		return '(${item} == NULL || cJSON_IsNull(${item}) || ${inner})'
	}
	if clean is types.Struct {
		fields := g.tc.structs[clean.name] or { return 'false' }
		mut checks := []string{cap: fields.len}
		for field in fields {
			field_item := if g.json_decode_struct_field_is_embedded(field) {
				item
			} else {
				'cJSON_GetObjectItemCaseSensitive(${item}, "${field.name}")'
			}
			checks << g.json_decode_value_valid_expr(field_item, field.typ)
		}
		children_valid := if checks.len > 0 { checks.join(' && ') } else { 'true' }
		return '(${item} == NULL || (cJSON_IsObject(${item}) && ${children_valid}))'
	}
	return 'true'
}

// json_decode_value_supported reports whether the fast-path decoder can preserve `typ`.
// The depth guard also prevents recursive structures from making this preflight recurse forever.
fn (g &FlatGen) json_decode_value_supported(typ types.Type, depth int) bool {
	if depth > 12 {
		return false
	}
	clean := if typ is types.Alias { typ.base_type } else { typ }
	if clean is types.String {
		return true
	}
	if clean is types.Enum {
		names, _ := g.json_enum_labels(clean.name)
		return names.len > 0
	}
	if clean is types.Primitive {
		return true
	}
	if clean is types.Array {
		return g.json_decode_value_supported(clean.elem_type, depth + 1)
	}
	if clean is types.Pointer {
		return g.json_decode_value_supported(clean.base_type, depth + 1)
	}
	if clean is types.Struct {
		if g.json_struct_has_decode_field_attrs(clean.name)
			|| g.json_struct_has_field_default(clean.name) {
			return false
		}
		fields := g.tc.structs[clean.name] or { return false }
		for field in fields {
			if !g.json_decode_value_supported(field.typ, depth + 1) {
				return false
			}
		}
		return true
	}
	return false
}

fn (g &FlatGen) json_decode_value_needs_exact_integer(typ types.Type, depth int) bool {
	if depth > 12 {
		return false
	}
	clean := if typ is types.Alias { typ.base_type } else { typ }
	if clean is types.Primitive {
		return clean.props.has(.integer) && clean.size == 64
	}
	if clean is types.Array {
		return g.json_decode_value_needs_exact_integer(clean.elem_type, depth + 1)
	}
	if clean is types.Pointer {
		return g.json_decode_value_needs_exact_integer(clean.base_type, depth + 1)
	}
	if clean is types.Struct {
		fields := g.tc.structs[clean.name] or { return false }
		for field in fields {
			if g.json_decode_value_needs_exact_integer(field.typ, depth + 1) {
				return true
			}
		}
	}
	return false
}

fn (mut g FlatGen) gen_json_decode_field_expr(root_name string, struct_name string, field types.StructField) {
	item := if g.json_decode_struct_field_is_embedded(field) {
		root_name
	} else {
		'cJSON_GetObjectItemCaseSensitive(${root_name}, "${field.name}")'
	}
	clean := if field.typ is types.Alias { field.typ.base_type } else { field.typ }
	if clean is types.Pointer {
		if info, default_id := g.json_struct_field_default_expr(struct_name, field.name) {
			default_node := g.a.node(default_id)
			if default_node.kind != .nil_literal {
				item_name := g.tmp_name()
				out_name := g.tmp_name()
				field_ct := g.value_c_type(field.typ)
				g.write('({ cJSON* ${item_name} = ${item}; ${field_ct} ${out_name}; if (${item_name} == NULL) { ${out_name} = ')
				old_module := g.tc.cur_module
				old_file := g.tc.cur_file
				g.tc.cur_module = info.module
				g.tc.cur_file = info.file
				g.gen_struct_field_expr_for_field(default_id, info.full_name, field.name, field.typ)
				g.tc.cur_module = old_module
				g.tc.cur_file = old_file
				g.write('; } else { ${out_name} = ')
				g.gen_json_decode_value_expr(item_name, field.typ)
				g.write('; } ${out_name}; })')
				return
			}
		}
	}
	g.gen_json_decode_value_expr(item, field.typ)
}

fn (g &FlatGen) json_struct_field_default_expr(struct_name string, field_name string) ?(StructDeclInfo, flat.NodeId) {
	info := g.find_struct_decl(json_struct_decl_name(struct_name)) or { return none }
	for i in 0 .. info.node.children_count {
		field := g.a.child_node(&info.node, i)
		if field.kind == .field_decl && field.value == field_name && field.children_count > 0 {
			return info, g.a.child(field, 0)
		}
	}
	return none
}

fn (mut g FlatGen) gen_json_decode_value_expr(item string, typ types.Type) {
	clean := if typ is types.Alias { typ.base_type } else { typ }
	if clean is types.String {
		empty := g.intern_string('')
		item_name := g.tmp_name()
		out_name := g.tmp_name()
		raw_name := g.tmp_name()
		g.write('({ cJSON* ${item_name} = ${item}; string ${out_name} = _str_${empty}; if (${item_name} != NULL && cJSON_IsString(${item_name}) && ${item_name}->valuestring != NULL) { ${out_name} = tos_clone((u8*)${item_name}->valuestring); } else if (${item_name} != NULL && (cJSON_IsObject(${item_name}) || cJSON_IsArray(${item_name}))) { char* ${raw_name} = cJSON_PrintUnformatted(${item_name}); if (${raw_name} != NULL) { ${out_name} = tos_clone((u8*)${raw_name}); cJSON_free(${raw_name}); } } ${out_name}; })')
		return
	}
	if clean is types.Enum {
		names, labels := g.json_enum_labels(clean.name)
		if names.len == 0 {
			g.write('0')
			return
		}
		default_value := g.enum_value_expr_for_type(clean.name, names[0]) or {
			'${g.cname(clean.name)}__${g.cname(names[0])}'
		}
		if _ := g.json_enum_number_cast(clean.name) {
			// `@[json_as_number]`: the JSON value is a number, not a label string.
			g.write('(${item} != NULL ? (${g.value_c_type(clean)})${item}->valuedouble : ${default_value})')
			return
		}
		mut result := default_value
		for i := names.len - 1; i >= 0; i-- {
			name := names[i]
			label := labels[name] or { name }
			value := g.enum_value_expr_for_type(clean.name, name) or {
				'${g.cname(clean.name)}__${g.cname(name)}'
			}
			mut comparisons := [
				'strcmp(${item}->valuestring, "${c_escape(name)}") == 0',
			]
			if label != name {
				comparisons << 'strcmp(${item}->valuestring, "${c_escape(label)}") == 0'
			}
			matches := comparisons.join(' || ')
			result = '(${item} != NULL && ${item}->valuestring != NULL && (${matches}) ? ${value} : ${result})'
		}
		g.write(result)
		return
	}
	if clean is types.Primitive {
		if clean.props.has(.boolean) {
			// cJSON records booleans via the node type (cJSON_True/cJSON_False),
			// not in valuedouble, so read them with cJSON_IsTrue.
			g.write('(${item} != NULL ? (bool)cJSON_IsTrue(${item}) : 0)')
			return
		}
		if clean.props.has(.integer) && clean.size == 64 {
			// cJSON's double cannot exactly represent every i64/u64. The fast-path
			// setup preserves the original number token in valuestring; use it for
			// decimal integers and retain valuedouble for fractional/exponent forms.
			item_name := g.tmp_name()
			raw_name := g.tmp_name()
			scan_name := g.tmp_name()
			is_decimal_name := g.tmp_name()
			is_negative_name := g.tmp_name()
			magnitude_name := g.tmp_name()
			limit_name := g.tmp_name()
			digit_name := g.tmp_name()
			out_name := g.tmp_name()
			ct := g.value_c_type(clean)
			limit_expr := if clean.props.has(.unsigned) {
				'18446744073709551615ULL'
			} else {
				'(${is_negative_name} ? 9223372036854775808ULL : 9223372036854775807ULL)'
			}
			value_expr := if clean.props.has(.unsigned) {
				'(${is_negative_name} ? (u64)0 - ${magnitude_name} : ${magnitude_name})'
			} else {
				'(${is_negative_name} ? (${magnitude_name} == 9223372036854775808ULL ? (i64)(-9223372036854775807LL - 1LL) : -(i64)${magnitude_name}) : (i64)${magnitude_name})'
			}
			g.write('({ cJSON* ${item_name} = ${item}; const char* ${raw_name} = ${item_name} != NULL ? ${item_name}->valuestring : NULL; const char* ${scan_name} = ${raw_name}; bool ${is_decimal_name} = ${scan_name} != NULL; bool ${is_negative_name} = false; if (${is_decimal_name} && (*${scan_name} == \'-\' || *${scan_name} == \'+\')) { ${is_negative_name} = *${scan_name} == \'-\'; ${scan_name}++; } if (${is_decimal_name} && *${scan_name} == \'\\0\') { ${is_decimal_name} = false; } u64 ${magnitude_name} = 0; u64 ${limit_name} = ${limit_expr}; while (${is_decimal_name} && *${scan_name} != \'\\0\') { if (*${scan_name} < \'0\' || *${scan_name} > \'9\') { ${is_decimal_name} = false; break; } u64 ${digit_name} = (u64)(*${scan_name} - \'0\'); if (${magnitude_name} > (${limit_name} - ${digit_name}) / 10) { ${is_decimal_name} = false; break; } ${magnitude_name} = ${magnitude_name} * 10 + ${digit_name}; ${scan_name}++; } ${ct} ${out_name} = ${item_name} != NULL ? (${ct})${item_name}->valuedouble : 0; if (${is_decimal_name}) { ${out_name} = (${ct})${value_expr}; } ${out_name}; })')
			return
		}
		g.write('(${item} != NULL ? (${g.value_c_type(clean)})${item}->valuedouble : 0)')
		return
	}
	if clean is types.Array {
		item_name := g.tmp_name()
		out_name := g.tmp_name()
		elem_name := g.tmp_name()
		value_name := g.tmp_name()
		elem_ct := g.value_c_type(clean.elem_type)
		g.write('({ cJSON* ${item_name} = ${item}; Array ${out_name} = array_new(sizeof(${elem_ct}), 0, (${item_name} != NULL && cJSON_IsArray(${item_name})) ? cJSON_GetArraySize(${item_name}) : 0); if (${item_name} != NULL && cJSON_IsArray(${item_name})) { cJSON* ${elem_name} = NULL; cJSON_ArrayForEach(${elem_name}, ${item_name}) { ${elem_ct} ${value_name} = ')
		g.gen_json_decode_value_expr(elem_name, clean.elem_type)
		g.write('; array_push(&${out_name}, &${value_name}); } } ${out_name}; })')
		return
	}
	if clean is types.Pointer {
		item_name := g.tmp_name()
		out_name := g.tmp_name()
		value_name := g.tmp_name()
		base_ct := g.value_c_type(clean.base_type)
		g.write('({ cJSON* ${item_name} = ${item}; ${base_ct}* ${out_name} = NULL; if (${item_name} != NULL && !cJSON_IsNull(${item_name})) { ${base_ct} ${value_name} = ')
		g.gen_json_decode_value_expr(item_name, clean.base_type)
		g.write('; ${out_name} = (${base_ct}*)memdup(&${value_name}, sizeof(${base_ct})); } ${out_name}; })')
		return
	}
	if clean is types.Struct {
		fields := g.tc.structs[clean.name] or {
			g.gen_default_value_for_type(typ)
			return
		}
		g.write('(${g.value_c_type(clean)}){')
		for i, field in fields {
			if i > 0 {
				g.write(', ')
			}
			g.write('.${g.cname(field.name)} = ')
			g.gen_json_decode_field_expr(item, clean.name, field)
		}
		g.write('}')
		return
	}
	g.gen_default_value_for_type(typ)
}

fn (g &FlatGen) json_decode_struct_field_is_embedded(field types.StructField) bool {
	mut field_type := field.typ
	if field_type is types.Alias {
		field_type = field_type.base_type
	}
	field_type = types.unwrap_pointer(field_type)
	if field_type is types.Struct {
		return field.name == field_type.name.all_after_last('.')
	}
	return false
}

// json_struct_has_field_default reports whether `struct_name` has a default initializer
// that the fast-path decoder cannot preserve. Pointer defaults are handled separately
// by gen_json_decode_field_expr; other defaults still require the full decoder.
fn (g &FlatGen) json_struct_has_field_default(struct_name string) bool {
	decl_name := json_struct_decl_name(struct_name)
	mut cur_module := ''
	for node in g.a.nodes {
		if node.kind == .module_decl {
			cur_module = node.value
			continue
		}
		if node.kind != .struct_decl {
			continue
		}
		qualified := if cur_module.len > 0 && cur_module !in ['main', 'builtin'] {
			'${cur_module}.${node.value}'
		} else {
			node.value
		}
		if decl_name != node.value && decl_name != qualified {
			continue
		}
		for i in 0 .. node.children_count {
			field := g.a.child_node(&node, i)
			if field.kind != .field_decl || field.children_count == 0 {
				continue
			}
			if field.typ.trim_space().starts_with('&') {
				// Explicit nil defaults use the pointer zero value. Non-nil pointer
				// defaults are emitted by gen_json_decode_field_expr when the key is
				// absent, so both pointer forms remain safe on the shortcut.
				continue
			}
			return true
		}
		return false
	}
	return false
}

fn json_struct_decl_name(name string) string {
	bracket := name.index_u8(`[`)
	if bracket <= 0 {
		return name
	}
	return name[..bracket]
}

// json_struct_has_field_attrs reports whether any field of `struct_name` carries
// attributes (`@[json: 'x']`, `@[skip]`, `@[required]`, ...). Field attributes are
// stored in `field_decl.generic_params` with index 0 holding the mut/pub flags and
// any further entries being attributes.
fn (g &FlatGen) json_struct_has_field_attrs(struct_name string) bool {
	return g.json_struct_has_disallowed_field_attrs(struct_name, []string{})
}

fn (g &FlatGen) json_struct_has_decode_field_attrs(struct_name string) bool {
	// `omitempty` changes encoding only. It is safe to ignore while decoding.
	return g.json_struct_has_disallowed_field_attrs(struct_name, ['omitempty'])
}

fn (g &FlatGen) json_struct_has_disallowed_field_attrs(struct_name string, allowed []string) bool {
	decl_name := json_struct_decl_name(struct_name)
	mut cur_module := ''
	for node in g.a.nodes {
		if node.kind == .module_decl {
			cur_module = node.value
			continue
		}
		if node.kind != .struct_decl {
			continue
		}
		qualified := if cur_module.len > 0 && cur_module !in ['main', 'builtin'] {
			'${cur_module}.${node.value}'
		} else {
			node.value
		}
		if decl_name != node.value && decl_name != qualified {
			continue
		}
		for i in 0 .. node.children_count {
			field := g.a.child_node(&node, i)
			if field.kind != .field_decl {
				continue
			}
			if field.generic_params.len > 1 {
				for attr in field.generic_params[1..] {
					name := attr.all_before(':').trim_space()
					if name !in allowed {
						return true
					}
				}
			}
		}
		return false
	}
	return false
}

// json_enum_number_cast returns the integer cast (`i64`/`u64`) to use when an enum is
// declared `@[json_as_number]`, so json.encode emits its numeric value rather than a
// quoted label; returns none for a plain enum.
fn (g &FlatGen) json_enum_number_cast(enum_name string) ?string {
	mut cur_module := ''
	for node in g.a.nodes {
		if node.kind == .module_decl {
			cur_module = node.value
			continue
		}
		if node.kind != .enum_decl {
			continue
		}
		qualified := if cur_module.len > 0 && cur_module !in ['main', 'builtin'] {
			'${cur_module}.${node.value}'
		} else {
			node.value
		}
		if enum_name != node.value && enum_name != qualified {
			continue
		}
		if 'json_as_number' !in node.generic_params {
			return none
		}
		backing := if node.generic_params.len > 0 { node.generic_params[0] } else { '' }
		return if backing in ['u8', 'byte', 'u16', 'u32', 'u64', 'usize'] {
			'u64'
		} else {
			'i64'
		}
	}
	return none
}

fn (g &FlatGen) json_enum_labels(enum_name string) ([]string, map[string]string) {
	mut names := []string{}
	mut labels := map[string]string{}
	mut cur_module := ''
	for node in g.a.nodes {
		if node.kind == .module_decl {
			cur_module = node.value
			continue
		}
		if node.kind != .enum_decl {
			continue
		}
		qualified := if cur_module.len > 0 && cur_module !in ['main', 'builtin'] {
			'${cur_module}.${node.value}'
		} else {
			node.value
		}
		if enum_name != node.value && enum_name != qualified {
			continue
		}
		for i in 0 .. node.children_count {
			field := g.a.child_node(&node, i)
			names << field.value
			for attr in field.generic_params {
				if attr.starts_with('json:') {
					labels[field.value] = json_enum_attr_label(attr.all_after(':'))
				}
			}
		}
		break
	}
	return names, labels
}

fn json_enum_attr_label(raw_value string) string {
	mut value := raw_value.trim_space()
	mut is_raw := false
	if value.len >= 3 && value[0] == `r` && value[1] in [`'`, `"`]
		&& value[value.len - 1] == value[1] {
		is_raw = true
		value = value[1..]
	}
	if value.len < 2 || value[0] !in [`'`, `"`] || value[value.len - 1] != value[0] {
		return value
	}
	inner := value[1..value.len - 1]
	if is_raw || !inner.contains('\\') {
		return inner
	}
	mut out := strings.new_builder(inner.len)
	mut i := 0
	for i < inner.len {
		if inner[i] != `\\` || i + 1 >= inner.len {
			out.write_u8(inner[i])
			i++
			continue
		}
		next := inner[i + 1]
		hex_len := match next {
			`x` { 2 }
			`u` { 4 }
			`U` { 8 }
			else { 0 }
		}

		if hex_len > 0 && i + 2 + hex_len <= inner.len {
			if code := json_enum_attr_hex(inner, i + 2, hex_len) {
				if next == `x` {
					out.write_u8(u8(code))
				} else {
					out.write_rune(rune(code))
				}
				i += 2 + hex_len
				continue
			}
		}
		match next {
			`n` {
				out.write_u8(`\n`)
			}
			`t` {
				out.write_u8(`\t`)
			}
			`r` {
				out.write_u8(`\r`)
			}
			`\\` {
				out.write_u8(`\\`)
			}
			`'` {
				out.write_u8(`'`)
			}
			`"` {
				out.write_u8(`"`)
			}
			`$` {
				out.write_u8(`$`)
			}
			`0` {
				out.write_u8(0)
			}
			`a` {
				out.write_u8(7)
			}
			`b` {
				out.write_u8(8)
			}
			`f` {
				out.write_u8(12)
			}
			`v` {
				out.write_u8(11)
			}
			else {
				out.write_u8(`\\`)
				out.write_u8(next)
			}
		}

		i += 2
	}
	return out.str()
}

fn json_enum_attr_hex(value string, start int, count int) ?u32 {
	mut code := u32(0)
	for i in 0 .. count {
		ch := value[start + i]
		digit := if ch >= `0` && ch <= `9` {
			int(ch - `0`)
		} else if ch >= `a` && ch <= `f` {
			int(ch - `a`) + 10
		} else if ch >= `A` && ch <= `F` {
			int(ch - `A`) + 10
		} else {
			return none
		}
		code = (code << 4) | u32(digit)
	}
	return code
}

fn (g &FlatGen) is_veb_json_result_call(fn_node flat.Node) bool {
	if fn_node.kind == .ident {
		if fn_node.value in ['veb.Context.json', 'veb.Context.json_pretty'] {
			return true
		}
		if fn_node.value !in ['Context.json', 'Context.json_pretty'] {
			return false
		}
		if fn_node.value in g.tc.fn_param_types || fn_node.value in g.tc.fn_ret_types {
			return false
		}
		return g.type_name_known_in_current_module('Context')
			&& g.type_embeds_veb_context(g.tc.parse_type('Context'))
	}
	if fn_node.kind != .selector || fn_node.children_count == 0 {
		return false
	}
	if fn_node.value !in ['json', 'json_pretty'] {
		return false
	}
	receiver_id := g.a.child(fn_node, 0)
	receiver_type := types.unwrap_pointer(g.tc.resolve_type(receiver_id))
	if receiver_type is types.Struct {
		receiver_name := receiver_type.name
		if receiver_name != 'veb.Context' {
			method_name := '${receiver_name}.${fn_node.value}'
			if method_name in g.tc.fn_param_types || method_name in g.tc.fn_ret_types {
				return false
			}
		}
	}
	if embedded_method := g.embedded_method_name_for_type(receiver_type, fn_node.value) {
		return embedded_method in ['veb.Context.json', 'veb.Context.json_pretty']
	}
	return g.is_veb_context_receiver(receiver_id)
}

fn (g &FlatGen) is_veb_context_receiver(id flat.NodeId) bool {
	typ := g.tc.resolve_type(id)
	return g.type_embeds_veb_context(typ)
}

fn (g &FlatGen) type_embeds_veb_context(typ types.Type) bool {
	clean := types.unwrap_pointer(typ)
	if clean is types.Alias {
		return g.type_embeds_veb_context(clean.base_type)
	}
	if clean !is types.Struct {
		return false
	}
	struct_name := clean.name()
	if struct_name == 'veb.Context' {
		return true
	}
	fields := g.struct_fields_for_type(struct_name) or { return false }
	for field in fields {
		embedded_type_name := g.embedded_field_type_name(field)
		if embedded_type_name == 'veb.Context' {
			return true
		}
		if embedded_type_name.len > 0
			&& g.type_embeds_veb_context(g.tc.parse_type(embedded_type_name)) {
			return true
		}
	}
	return false
}

fn (g &FlatGen) is_missing_middleware_use_call(fn_node flat.Node) bool {
	if fn_node.kind == .ident {
		if !fn_node.value.ends_with('.use') {
			return false
		}
		if fn_node.value in g.tc.fn_param_types || fn_node.value in g.tc.fn_ret_types {
			return false
		}
		receiver := fn_node.value.all_before_last('.')
		return g.struct_has_middleware_receiver(receiver)
	}
	if fn_node.kind != .selector || fn_node.value != 'use' || fn_node.children_count == 0 {
		return false
	}
	base_type := g.tc.resolve_type(g.a.child(fn_node, 0))
	clean_type := types.unwrap_pointer(base_type)
	if clean_type !is types.Struct {
		return false
	}
	method_name := '${clean_type.name()}.use'
	if method_name in g.tc.fn_param_types || method_name in g.tc.fn_ret_types {
		return false
	}
	return g.struct_has_middleware_receiver(clean_type.name())
}

fn (g &FlatGen) struct_has_middleware_receiver(type_name string) bool {
	if is_middleware_type_name(type_name) {
		return true
	}
	fields := g.struct_fields_for_type(type_name) or { return false }
	for field in fields {
		embedded_type_name := g.embedded_field_type_name(field)
		if is_middleware_type_name(embedded_type_name) {
			return true
		}
	}
	return false
}

fn is_middleware_type_name(name string) bool {
	base := if name.contains('[') { name.all_before('[') } else { name }
	return base == 'veb.Middleware'
}

fn (g &FlatGen) call_default_return_type(id flat.NodeId) types.Type {
	if g.expected_expr_type is types.OptionType || g.expected_expr_type is types.ResultType {
		return g.expected_expr_type
	}
	if g.expected_expr_type is types.Struct && g.expected_expr_type.name.starts_with('Optional') {
		return g.expected_expr_type
	}
	if g.expected_expr_type is types.String {
		return g.expected_expr_type
	}
	if typ := g.tc.expr_type(id) {
		if typ !is types.Unknown && typ !is types.Void {
			return typ
		}
	}
	return g.tc.resolve_type(id)
}

fn (g &FlatGen) json_decode_result_type_for_call(node flat.Node) ?types.Type {
	if node.children_count == 0 {
		return none
	}
	if ret_type := g.json_decode_result_type(g.a.child(&node, 0)) {
		return ret_type
	}
	if node.children_count < 2 {
		return none
	}
	type_name := g.json_decode_type_arg_name(g.a.child(&node, 1))
	if type_name.len == 0 {
		return none
	}
	return types.Type(types.ResultType{
		base_type: g.tc.parse_type(type_name)
	})
}

fn (g &FlatGen) json_decode_result_type(callee_id flat.NodeId) ?types.Type {
	if int(callee_id) < 0 || int(callee_id) >= g.a.nodes.len {
		return none
	}
	callee := g.a.nodes[int(callee_id)]
	if callee.kind != .index || callee.children_count < 2 {
		return none
	}
	arg := g.a.child_node(&callee, 1)
	mut type_name := ''
	if arg.typ.len > 0 {
		type_name = arg.typ
	} else if arg.kind == .array_init && arg.value.len > 0 {
		type_name = '[]${arg.value}'
	} else if arg.value.len > 0 {
		type_name = arg.value
	}
	if type_name.len == 0 {
		return none
	}
	return types.Type(types.ResultType{
		base_type: g.tc.parse_type(type_name)
	})
}

fn (g &FlatGen) json_decode_type_arg_name(id flat.NodeId) string {
	if int(id) < 0 || int(id) >= g.a.nodes.len {
		return ''
	}
	node := g.a.nodes[int(id)]
	match node.kind {
		.ident {
			return node.value
		}
		.selector {
			if node.children_count == 0 {
				return node.value
			}
			base := g.json_decode_type_arg_name(g.a.child(&node, 0))
			if base.len == 0 {
				return node.value
			}
			return '${base}.${node.value}'
		}
		.index {
			if node.children_count < 2 || node.value == 'range' {
				return ''
			}
			base := g.json_decode_type_arg_name(g.a.child(&node, 0))
			if base.len == 0 {
				return ''
			}
			mut args := []string{}
			for i in 1 .. node.children_count {
				arg := g.json_decode_type_arg_name(g.a.child(&node, i))
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
		.struct_decl {
			return node.value
		}
		.prefix {
			if node.children_count == 0 {
				return ''
			}
			child := g.json_decode_type_arg_name(g.a.child(&node, 0))
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

fn (g &FlatGen) embedded_method_name_for_type(base_type types.Type, method string) ?string {
	type_name := g.type_lookup_name(base_type)
	if type_name.len == 0 || method.len == 0 {
		return none
	}
	return g.embedded_method_name_for_struct(type_name, method)
}

fn (g &FlatGen) embedded_method_name_for_struct(type_name string, method string) ?string {
	fields := g.struct_fields_for_type(type_name) or { return none }
	for field in fields {
		embedded_type_name := g.embedded_field_type_name(field)
		if embedded_type_name.len == 0 {
			continue
		}
		method_name := '${embedded_type_name}.${method}'
		if method_name in g.tc.fn_param_types {
			return method_name
		}
		if found := g.embedded_method_name_for_struct(embedded_type_name, method) {
			return found
		}
	}
	return none
}

fn (mut g FlatGen) gen_embedded_method_receiver(base_id flat.NodeId, base_type types.Type, expected_type types.Type, wants_ptr bool) bool {
	path := g.embedded_receiver_path_for_expected(base_type, expected_type) or { return false }
	target_is_ptr := path[path.len - 1].typ is types.Pointer
	if wants_ptr && !target_is_ptr {
		g.write('&')
	} else if !wants_ptr && target_is_ptr {
		g.write('*')
	}
	needs_paren := g.a.nodes[int(base_id)].kind !in [.ident, .selector]
	if needs_paren {
		g.write('(')
	}
	g.gen_expr(base_id)
	if needs_paren {
		g.write(')')
	}
	mut access_is_ptr := base_type is types.Pointer
	for field in path {
		op := if access_is_ptr { '->' } else { '.' }
		g.write('${op}${c_field_name(field.name)}')
		access_is_ptr = field.typ is types.Pointer
	}
	return true
}

fn (g &FlatGen) embedded_receiver_field_for_expected(base_type types.Type, expected_type types.Type) ?types.StructField {
	path := g.embedded_receiver_path_for_expected(base_type, expected_type) or { return none }
	if path.len == 0 {
		return none
	}
	return path[0]
}

fn (g &FlatGen) embedded_receiver_path_for_expected(base_type types.Type, expected_type types.Type) ?[]types.StructField {
	base_name := g.type_lookup_name(base_type)
	expected_name := g.type_lookup_name(expected_type)
	if base_name.len == 0 || expected_name.len == 0 {
		return none
	}
	mut seen := map[string]bool{}
	return g.embedded_receiver_path_for_expected_name(base_name, expected_name, mut seen)
}

fn (g &FlatGen) embedded_receiver_path_for_expected_name(base_name string, expected_name string, mut seen map[string]bool) ?[]types.StructField {
	if seen[base_name] {
		return none
	}
	seen[base_name] = true
	for field in g.struct_embedded_fields(base_name) {
		embedded_type_name := g.embedded_field_type_name(field)
		if embedded_type_name == expected_name {
			return [field]
		}
		if nested := g.embedded_receiver_path_for_expected_name(embedded_type_name, expected_name, mut
			seen)
		{
			mut path := [field]
			path << nested
			return path
		}
	}
	return none
}

// current_param_type returns current param type data for FlatGen.
fn (g &FlatGen) current_param_type(name string) ?types.Type {
	if g.cur_param_types.len == 0 {
		return none
	}
	if g.current_mut_param_binding_is_shadowed(name) {
		return none
	}
	return g.cur_param_types[name] or { none }
}

fn (g &FlatGen) current_param_map_type(name string) ?types.Type {
	if g.cur_param_types.len == 0 {
		return none
	}
	if g.current_mut_param_binding_is_shadowed(name) {
		return none
	}
	if typ := g.cur_param_types[name] {
		return typ
	}
	return none
}

fn (g &FlatGen) call_uses_concrete_optional_params(name string) bool {
	if name in g.concrete_optional_abi_fns {
		return true
	}
	return g.name_uses_specialized_generic_abi(name)
}

fn (g &FlatGen) name_uses_specialized_generic_abi(name string) bool {
	if name.contains('[') && name.contains(']') {
		return true
	}
	if _ := g.generic_receiver_method_call_info(name) {
		return true
	}
	if name in g.tc.specialized_generic_fns {
		return true
	}
	return false
}

fn (g &FlatGen) call_callee_uses_specialized_generic_abi(callee_id flat.NodeId) bool {
	if int(callee_id) < 0 || int(callee_id) >= g.a.nodes.len {
		return false
	}
	node := g.a.nodes[int(callee_id)]
	if node.kind == .index && node.children_count > 0 {
		base_id := g.a.child(&node, 0)
		base_name := g.call_target_name(base_id)
		return g.name_uses_specialized_generic_abi(base_name) || g.generic_fn_base_known(base_name)
	}
	name := g.call_target_name(callee_id)
	return g.name_uses_specialized_generic_abi(name)
}

fn (g &FlatGen) generic_fn_base_known(base string) bool {
	mut candidates := []string{cap: 8}
	candidates << base
	if base.starts_with('main.') {
		candidates << base.all_after('main.')
	}
	if !base.contains('.') {
		candidates << 'main.${base}'
	}
	if !base.contains('.') && g.tc.cur_module.len > 0 && g.tc.cur_module !in ['', 'main', 'builtin'] {
		candidates << '${g.tc.cur_module}.${base}'
	}
	short := base.all_after_last('.')
	if short != base {
		candidates << short
	}
	if base.contains('__') {
		dotted := base.replace('__', '.')
		candidates << dotted
		candidates << dotted.all_after_last('.')
		if !dotted.contains('.') {
			candidates << 'main.${dotted}'
		}
	}
	for candidate in candidates {
		if candidate.len == 0 {
			continue
		}
		if candidate in g.tc.fn_generic_params || candidate in g.tc.fn_param_type_texts
			|| candidate in g.tc.fn_ret_type_texts {
			return true
		}
	}
	return false
}

fn type_is_optional_result(t types.Type) bool {
	return t is types.OptionType || t is types.ResultType
}

fn params_have_optional_result(params []types.Type) bool {
	for param in params {
		if type_is_optional_result(param) {
			return true
		}
	}
	return false
}

fn (mut g FlatGen) precompute_concrete_optional_abi_fns() {
	mut cur_module := ''
	mut cur_file := ''
	for node in g.a.nodes {
		kind_id := node_kind_id(node)
		if kind_id == 77 {
			cur_file = node.value
			g.tc.cur_file = cur_file
			cur_module = ''
			g.tc.cur_module = cur_module
			continue
		}
		if kind_id == 73 {
			cur_module = node.value
			g.tc.cur_file = cur_file
			g.tc.cur_module = cur_module
			continue
		}
		if kind_id != 61 || !g.is_specialized_generic_fn_node(node) {
			continue
		}
		params := g.fn_node_param_types_or_decl(node, cur_module)
		if !params_have_optional_result(params) {
			continue
		}
		g.register_concrete_optional_abi_fn(cur_module, node.value)
	}
}

fn (mut g FlatGen) fn_node_param_types_or_decl(node flat.Node, module_name string) []types.Type {
	params := g.fn_node_param_types(node, module_name)
	if params.len > 0 {
		return params
	}
	mut result := []types.Type{}
	for i in 0 .. node.children_count {
		child := g.a.child_node(&node, i)
		if child.kind == .param {
			result << g.tc.parse_type(child.typ)
		}
	}
	return result
}

fn (mut g FlatGen) register_concrete_optional_abi_fn(module_name string, name string) {
	for candidate in concrete_optional_abi_fn_name_candidates(module_name, name, g.fn_c_name_in_module(module_name,
		name)) {
		if candidate.len > 0 {
			g.concrete_optional_abi_fns[candidate] = true
		}
	}
}

fn concrete_optional_abi_fn_name_candidates(module_name string, name string, emitted string) []string {
	qname := qualify_name_in_module(module_name, name)
	mut candidates := []string{cap: 8}
	candidates << name
	candidates << c_name(name)
	candidates << qname
	candidates << c_name(qname)
	candidates << emitted
	mut deduped := []string{cap: candidates.len}
	for candidate in candidates {
		if candidate.len > 0 && candidate !in deduped {
			deduped << candidate
		}
	}
	return deduped
}

fn (g &FlatGen) concrete_optional_param_type_for_expr(id flat.NodeId) ?types.Type {
	if int(id) < 0 || int(id) >= g.a.nodes.len {
		return none
	}
	node := g.a.nodes[int(id)]
	if node.kind == .paren && node.children_count > 0 {
		return g.concrete_optional_param_type_for_expr(g.a.child(&node, 0))
	}
	if node.kind != .ident || node.value.len == 0 {
		return none
	}
	if !(g.cur_concrete_optional_params[node.value] or { false }) {
		return none
	}
	param_type := g.current_param_type(node.value) or { return none }
	if type_is_optional_result(param_type) {
		return param_type
	}
	return none
}

fn (g &FlatGen) optional_source_type_for_expr(id flat.NodeId, typ types.Type) types.Type {
	if type_is_optional_result(typ) {
		if param_type := g.concrete_optional_param_type_for_expr(id) {
			return param_type
		}
	}
	return typ
}

fn (mut g FlatGen) optional_type_name_for_expr(id flat.NodeId, typ types.Type) string {
	if param_type := g.concrete_optional_param_type_for_expr(id) {
		return g.concrete_optional_type_name(param_type)
	}
	if int(id) >= 0 && int(id) < g.a.nodes.len {
		node := g.a.nodes[int(id)]
		if node.kind == .ident {
			if local_ct := g.local_storage_c_type(node.value) {
				if local_ct == 'Optional' || local_ct.starts_with('Optional_') {
					return local_ct
				}
			}
		}
		if node.kind == .call && node.children_count > 0
			&& g.call_callee_uses_specialized_generic_abi(g.a.child(&node, 0)) {
			declared := g.declared_call_return_type(id)
			if declared is types.OptionType || declared is types.ResultType {
				return g.optional_type_name(declared)
			}
		}
	}
	return g.optional_type_name(typ)
}

// current_param_is_mut returns true when a current param originated from `mut name T`
// and the identifier still resolves to that parameter (not a shadowing local).
fn (g &FlatGen) current_param_is_mut(name string) bool {
	if g.cur_mut_params.len == 0 {
		return false
	}
	if !(g.cur_mut_params[name] or { false }) {
		return false
	}
	owner := g.cur_mut_param_owners[name] or { return false }
	if g.tc == unsafe { nil } || g.tc.cur_scope == unsafe { nil } {
		return false
	}
	return g.tc.cur_scope.nearest_binding_owned_by(name, owner)
}

fn (g &FlatGen) current_mut_param_binding_is_shadowed(name string) bool {
	if g.cur_mut_params.len == 0 {
		return false
	}
	if !(g.cur_mut_params[name] or { false }) {
		return false
	}
	owner := g.cur_mut_param_owners[name] or { return false }
	if g.tc == unsafe { nil } || g.tc.cur_scope == unsafe { nil } {
		return false
	}
	return !g.tc.cur_scope.nearest_binding_owned_by(name, owner)
}

// gen_enum_str_call emits an explicit `enum_val.str()` (with no user-defined `str`)
// by routing to the compiler-synthesized `<Enum>__autostr`, so it matches `${enum}`
// interpolation exactly — including `[flag]` enums' `Enum{.a | .b}` form, which the
// old inline single-value ternary chain could not render.
fn (mut g FlatGen) gen_enum_str_call(fn_node &flat.Node, enum_type types.Enum) {
	mut name := enum_type.name
	if name.starts_with('main.') {
		name = name[5..]
	}
	g.write('${g.cname(name)}__autostr(')
	g.gen_expr(g.a.child(fn_node, 0))
	g.write(')')
}

// enum_receiver_method_name supports enum receiver method name handling for FlatGen.
fn (g &FlatGen) enum_receiver_method_name(enum_type types.Enum, method string) ?string {
	name := enum_type.name
	direct := '${name}.${method}'
	if direct in g.tc.fn_param_types {
		return direct
	}
	if name.contains('.') {
		return none
	}
	for candidate, _ in g.tc.fn_param_types {
		if candidate.ends_with('.${direct}') {
			return candidate
		}
	}
	return none
}

// gen_fn_field_call emits fn field call output for c.
fn (mut g FlatGen) gen_fn_field_call(node flat.Node, fn_node &flat.Node, base_type types.Type) bool {
	fn_type := g.fn_field_type(base_type, fn_node.value) or { return false }
	base_id := g.a.child(fn_node, 0)
	base := g.a.nodes[int(base_id)]
	needs_paren := base.kind !in [.ident, .selector, .call]
	if needs_paren {
		g.write('(')
	}
	g.gen_expr(base_id)
	if needs_paren {
		g.write(')')
	}
	if base_type is types.Pointer {
		g.write('->')
	} else {
		g.write('.')
	}
	g.write(g.cname(fn_node.value))
	g.write('(')
	for i in 1 .. node.children_count {
		if i > 1 {
			g.write(', ')
		}
		arg_id := g.a.child(&node, i)
		arg_idx := i - 1
		if arg_idx < fn_type.params.len {
			g.gen_arg_for_expected_type(arg_id, fn_type.params[arg_idx])
		} else {
			g.gen_expr(arg_id)
		}
	}
	g.write(')')
	return true
}

// call_key updates call key state for FlatGen.
fn (g &FlatGen) call_key(id flat.NodeId, name string) string {
	if name.contains('.') {
		normalized := g.normalize_call_key(name)
		if normalized in g.tc.fn_param_types || normalized in g.tc.fn_ret_types {
			return normalized
		}
	}
	if resolved := g.tc.resolved_call_name(id) {
		if resolved_call_matches_target(resolved, name) {
			return g.normalize_call_key(resolved)
		}
	}
	return g.normalize_call_key(name)
}

fn resolved_call_matches_target(resolved string, target string) bool {
	if resolved.len == 0 || target.len == 0 {
		return false
	}
	if resolved == target || c_name(resolved) == target || resolved == c_name(target) {
		return true
	}
	resolved_short := resolved.all_after_last('.')
	target_short := target.all_after_last('.')
	return resolved_short == target_short || c_name(resolved_short) == target_short
		|| resolved_short == c_name(target_short)
}

// normalize_call_key transforms normalize call key data for c.
fn (g &FlatGen) normalize_call_key(name string) string {
	if name.starts_with('main.') {
		short_name := name.all_after_last('.')
		if short_name in g.tc.fn_param_types || short_name in g.tc.fn_ret_types {
			return short_name
		}
	}
	if !name.contains('.') && g.tc.cur_module.len > 0 && g.tc.cur_module != 'main'
		&& g.tc.cur_module != 'builtin' {
		local := '${g.tc.cur_module}.${name}'
		if local in g.tc.fn_param_types || local in g.tc.fn_ret_types {
			return local
		}
	}
	if name in g.tc.fn_param_types || name in g.tc.fn_ret_types {
		return name
	}
	if imported := g.selective_import_call_key(name) {
		return imported
	}
	qname := g.tc.qualify_fn_name(name)
	if qname in g.tc.fn_param_types || qname in g.tc.fn_ret_types {
		return qname
	}
	for _, mod_name in g.tc.imports {
		imported := '${mod_name}.${name}'
		if imported in g.tc.fn_param_types || imported in g.tc.fn_ret_types {
			return imported
		}
	}
	return qname
}

fn (g &FlatGen) selective_import_call_key(name string) ?string {
	if name.contains('.') {
		return none
	}
	mut resolved := []string{}
	if g.tc.cur_file.len > 0 {
		if candidates := g.tc.file_selective_imports['${g.tc.cur_file}\n${name}'] {
			for candidate in candidates {
				if (candidate in g.tc.fn_param_types || candidate in g.tc.fn_ret_types)
					&& candidate !in resolved {
					resolved << candidate
				}
			}
		}
	}
	if resolved.len == 0 {
		suffix := '\n${name}'
		for key, candidates in g.tc.file_selective_imports {
			if !key.ends_with(suffix) {
				continue
			}
			for candidate in candidates {
				if (candidate in g.tc.fn_param_types || candidate in g.tc.fn_ret_types)
					&& candidate !in resolved {
					resolved << candidate
				}
			}
		}
	}
	if resolved.len == 1 {
		return resolved[0]
	}
	return none
}

// param_types_for supports param types for handling for FlatGen.
// param_types_for resolves the parameter types of a called function. It is invoked once
// per call site during codegen, and the slow path below scans every known function, so
// results are memoized: without this, generic/monomorphized call names that miss the
// direct lookups re-scan (and copy) the whole function table on every call (O(n^2)).
// fn_decl_is_variadic resolves the variadic flag for a call target using the
// same key candidates as param_types_for, so import-alias and C-name call
// sites (e.g. `http.new_header` for module `net.http`) resolve consistently.
fn (g &FlatGen) fn_decl_is_variadic(name string, fallback string) bool {
	if name.contains('__') {
		dotted_name := name.replace('__', '.')
		if v := g.fn_decl_variadic[dotted_name] {
			return v
		}
		if v := g.import_resolved_fn_decl_variadic(dotted_name) {
			return v
		}
		if v := g.unique_short_fn_decl_variadic(dotted_name) {
			return v
		}
	}
	for candidate in [name, fallback] {
		if !candidate.contains('.') && !candidate.contains('__') {
			if v := g.local_or_unique_short_fn_decl_variadic(candidate) {
				return v
			}
			continue
		}
		if v := g.import_resolved_fn_decl_variadic(candidate) {
			return v
		}
		if v := g.fn_decl_variadic[candidate] {
			return v
		}
		if candidate.starts_with('main.') {
			if v := g.unique_short_fn_decl_variadic(candidate) {
				return v
			}
		}
	}
	if name.contains('.') {
		if v := g.unique_short_fn_decl_variadic(name) {
			return v
		}
	}
	return false
}

fn (g &FlatGen) import_resolved_fn_decl_variadic(name string) ?bool {
	if !name.contains('.') {
		return none
	}
	alias := name.all_before('.')
	module_name := if g.tc != unsafe { nil } {
		if g.tc.cur_file.len == 0 {
			return none
		}
		g.tc.file_imports['${g.tc.cur_file}\n${alias}'] or { return none }
	} else {
		g.import_alias_module(alias) or { return none }
	}
	resolved_name := '${module_name}.${name.all_after('.')}'
	if v := g.fn_decl_variadic[resolved_name] {
		return v
	}
	return none
}

fn (g &FlatGen) local_or_unique_short_fn_decl_variadic(name string) ?bool {
	if g.tc != unsafe { nil } {
		module_key := fn_decl_module_key(g.tc.cur_module, name)
		if v := g.fn_decl_variadic[module_key] {
			return v
		}
	}
	return g.unique_short_fn_decl_variadic(name)
}

fn (g &FlatGen) unique_short_fn_decl_variadic(name string) ?bool {
	short_name := name.all_after_last('.')
	if g.fn_decl_variadic_short_counts[short_name] != 1 {
		return none
	}
	if v := g.fn_decl_variadic[short_name] {
		return v
	}
	return none
}

fn (mut g FlatGen) param_types_for(name string, fallback string) []types.Type {
	cache_key := '${name}\x01${fallback}'
	if cached := g.param_types_cache[cache_key] {
		return cached
	}
	result := g.param_types_for_uncached(name, fallback)
	g.param_types_cache[cache_key] = result
	return result
}

fn (mut g FlatGen) param_types_for_uncached(name string, fallback string) []types.Type {
	if name.contains('__') {
		dotted_name := name.replace('__', '.')
		dotted_decl_types := g.param_types_from_decl(dotted_name, dotted_name)
		for candidate in [dotted_name, dotted_name.all_after_last('.')] {
			if params := g.tc.fn_param_types[candidate] {
				return merge_decl_pointer_param_abi(params, dotted_decl_types)
			}
		}
		if dotted_decl_types.len > 0 {
			return dotted_decl_types
		}
	}
	decl_types := g.param_types_from_decl(name, fallback)
	for candidate in [name, fallback] {
		if params := g.tc.fn_param_types[candidate] {
			return merge_decl_pointer_param_abi(params, decl_types)
		}
		if candidate.starts_with('main.') {
			short_name := candidate.all_after_last('.')
			if params := g.tc.fn_param_types[short_name] {
				return merge_decl_pointer_param_abi(params, decl_types)
			}
		}
	}
	if decl_types.len > 0 {
		return decl_types
	}
	if name.contains('__') {
		short_name := name.all_after_last('__')
		if params := g.param_types_by_short[short_name] {
			return merge_decl_pointer_param_abi(params, decl_types)
		}
	}
	if generic_params := g.generic_receiver_method_param_types(name) {
		return generic_params
	}
	if interface_types := g.interface_method_param_types(name) {
		return interface_types
	}
	if name.contains('.') {
		// O(1) lookup via the precomputed short-name index instead of scanning the whole
		// function table on every (cache-missing) call — this fallback ran ~3000× and was
		// a top cgen self-time cost (each scan is O(functions)).
		short_name := name.all_after_last('.')
		if params := g.param_types_by_short[short_name] {
			return params
		}
	}
	return []types.Type{}
}

fn merge_decl_pointer_param_abi(params []types.Type, decl_types []types.Type) []types.Type {
	if params.len == 0 {
		return params
	}
	if params.len != decl_types.len {
		return if decl_types.len > params.len { decl_types } else { params }
	}
	mut merged := params.clone()
	mut changed := false
	for i, decl_type in decl_types {
		if decl_type is types.Unknown || decl_type is types.Void {
			continue
		}
		if decl_type.name() != merged[i].name() {
			merged[i] = decl_type
			changed = true
		}
	}
	return if changed { merged } else { params }
}

fn (g &FlatGen) generic_receiver_method_param_types(name string) ?[]types.Type {
	if !name.contains('.') || !name.contains('[') || !name.contains(']') {
		return none
	}
	receiver := name.all_before_last('.')
	method := name.all_after_last('.')
	info := g.tc.resolve_generic_struct_method(receiver, method) or { return none }
	return info.params.clone()
}

// precompute_param_type_index builds short-name -> param-types, preserving the fallback's
// priority (fn_decl_param_types first, then the checker's fn_param_types; first match wins).
fn (mut g FlatGen) precompute_param_type_index() {
	for name, params in g.fn_decl_param_types {
		if name.contains('.') {
			short := name.all_after_last('.')
			if short !in g.param_types_by_short {
				g.param_types_by_short[short] = params
			}
		}
	}
	for name, params in g.tc.fn_param_types {
		if name.contains('.') {
			short := name.all_after_last('.')
			if short !in g.param_types_by_short {
				g.param_types_by_short[short] = params
			}
		}
	}
}

fn (g &FlatGen) interface_method_param_types(name string) ?[]types.Type {
	if !name.contains('.') {
		return none
	}
	iface_name := name.all_before_last('.')
	if iface_name !in g.interfaces {
		return none
	}
	method := name.all_after_last('.')
	if iface_name == 'IError' && method == 'str' {
		return none
	}
	decl_key := g.interface_method_signature_key(iface_name, method) or { return none }
	decl_params := g.tc.fn_param_types[decl_key] or { return none }
	mut params := []types.Type{cap: decl_params.len}
	params << types.Type(types.Pointer{
		base_type: types.Type(types.Interface{
			name: iface_name
		})
	})
	if decl_params.len > 1 {
		for i in 1 .. decl_params.len {
			params << decl_params[i]
		}
	}
	return params
}

// param_types_from_decl converts param types from decl data for c.
fn (mut g FlatGen) param_types_from_decl(name string, fallback string) []types.Type {
	if name.contains('.') {
		if ptypes := g.fn_decl_param_types[name] {
			return ptypes
		}
	} else {
		for candidate in [fallback, name] {
			if ptypes := g.fn_decl_param_types[candidate] {
				return ptypes
			}
		}
	}
	return []types.Type{}
}

// short_receiver_method_name supports short receiver method name handling for c.
fn short_receiver_method_name(name string) string {
	if !name.contains('.') {
		return ''
	}
	receiver := name.all_before_last('.')
	if !receiver.contains('.') {
		return ''
	}
	return '${receiver.all_after_last('.')}.${name.all_after_last('.')}'
}

// gen_arg_for_expected_type emits arg for expected type output for c.
fn (mut g FlatGen) gen_arg_for_expected_type(arg_id flat.NodeId, expected types.Type) {
	arg_node := g.a.nodes[int(arg_id)]
	if g.gen_mut_sum_lvalue_arg(arg_id, expected) {
		return
	}
	mut needs_addr := false
	if expected is types.Pointer && !(arg_node.kind == .prefix && arg_node.op == .amp)
		&& !g.arg_is_null_pointer_literal(arg_id, arg_node) {
		arg_type := g.usable_expr_type(arg_id)
		if arg_type !is types.Pointer {
			needs_addr = true
		}
	}
	if needs_addr {
		if g.arg_is_const_ident(arg_node) {
			ct := g.tc.c_type(types.unwrap_pointer(expected))
			g.write('(${ct}[]){')
			g.gen_expr_with_expected_type(arg_id, types.unwrap_pointer(expected))
			g.write('}')
			return
		}
		is_rvalue := arg_node.kind == .call
			|| (arg_node.kind == .index && arg_node.value == 'range')
		if is_rvalue {
			ct := g.tc.c_type(types.unwrap_pointer(expected))
			g.write('({${ct} _t${g.tmp_count} = ')
			g.gen_expr_with_expected_type(arg_id, types.unwrap_pointer(expected))
			g.write('; &_t${g.tmp_count};})')
			g.tmp_count++
			return
		}
		if g.gen_mut_sum_lvalue_arg(arg_id, expected) {
			return
		}
		g.write('&')
	}
	if !needs_addr && g.gen_sum_variant_arg(arg_id, expected) {
		return
	}
	if !needs_addr && g.gen_optional_arg(arg_id, expected) {
		return
	}
	g.gen_expr_with_expected_type(arg_id, expected)
}

fn (mut g FlatGen) gen_callback_fn_value_for_expected_type(arg_id flat.NodeId, expected types.Type) bool {
	return g.gen_callback_fn_value_for_expected_c_abi(arg_id, expected, '')
}

fn (mut g FlatGen) gen_callback_fn_value_for_expected_c_abi(arg_id flat.NodeId, expected types.Type, expected_c_abi string) bool {
	expected_fn := fn_type_from(expected) or { return false }
	actual_name := g.callback_fn_value_name(arg_id, expected) or { return false }
	actual_fn := g.callback_fn_value_type(actual_name) or { return false }
	wrapper := g.ensure_callback_userdata_wrapper(actual_name, actual_fn, expected_fn,
		expected_c_abi) or { return false }
	g.write(wrapper)
	return true
}

fn (mut g FlatGen) gen_callback_fn_value_for_field_c_abi(arg_id flat.NodeId, expected types.Type, expected_c_abi string) bool {
	if expected_c_abi.len == 0 {
		return false
	}
	if g.gen_callback_fn_value_for_expected_c_abi(arg_id, expected, expected_c_abi) {
		return true
	}
	if call_name := g.callback_direct_fn_value_name_for_c_abi(arg_id, expected, expected_c_abi) {
		g.write(g.callback_c_fn_name(call_name))
		return true
	}
	if _ := g.callback_fn_value_name(arg_id, expected) {
		g.gen_expr(arg_id)
		return true
	}
	return false
}

fn (mut g FlatGen) callback_fn_value_name(id flat.NodeId, expected types.Type) ?string {
	if name := g.tc.resolved_fn_value_name(id) {
		return name
	}
	if int(id) < 0 || int(id) >= g.a.nodes.len {
		return none
	}
	node := g.a.nodes[int(id)]
	if node.kind in [.cast_expr, .paren, .expr_stmt] && node.children_count > 0 {
		return g.callback_fn_value_name(g.a.child(&node, 0), expected)
	}
	return none
}

fn (mut g FlatGen) callback_direct_fn_value_name(id flat.NodeId, expected types.Type) ?string {
	return g.callback_direct_fn_value_name_for_c_abi(id, expected, '')
}

fn (mut g FlatGen) callback_direct_fn_value_name_for_c_abi(id flat.NodeId, expected types.Type, expected_c_abi string) ?string {
	expected_fn := fn_type_from(expected) or { return none }
	actual_name := g.callback_fn_value_name(id, expected) or {
		g.direct_callback_ident_name(id) or { return none }
	}
	actual_fn := g.callback_fn_value_type(actual_name) or { return none }
	if !g.callback_fn_types_direct_compatible(actual_fn, expected_fn, expected_c_abi) {
		return none
	}
	return actual_name
}

fn (g &FlatGen) direct_callback_ident_name(id flat.NodeId) ?string {
	if int(id) < 0 || int(id) >= g.a.nodes.len {
		return none
	}
	node := g.a.nodes[int(id)]
	if node.kind in [.cast_expr, .paren, .expr_stmt] && node.children_count > 0 {
		return g.direct_callback_ident_name(g.a.child(&node, 0))
	}
	if node.kind == .selector && node.children_count > 0 {
		base := g.a.child_node(&node, 0)
		if base.kind == .ident {
			looked_up := g.tc.cur_scope.lookup(base.value) or { types.Type(types.void_) }
			if looked_up !is types.Void {
				return none
			}
			name := '${base.value}.${node.value}'
			if name in g.tc.fn_param_types && name in g.tc.fn_ret_types {
				return name
			}
			qname := '${g.tc.cur_module}.${name}'
			if qname in g.tc.fn_param_types && qname in g.tc.fn_ret_types {
				return qname
			}
		}
	}
	if node.kind != .ident || node.value.len == 0 {
		return none
	}
	looked_up := g.tc.cur_scope.lookup(node.value) or { types.Type(types.void_) }
	if looked_up !is types.Void {
		return none
	}
	call_key := g.call_key(id, node.value)
	if call_key in g.tc.fn_param_types && call_key in g.tc.fn_ret_types {
		return call_key
	}
	if node.value in g.tc.fn_param_types && node.value in g.tc.fn_ret_types {
		return node.value
	}
	return none
}

fn (mut g FlatGen) ident_fn_value_c_name(id flat.NodeId, node flat.Node) ?string {
	if node.kind != .ident || node.value.len == 0 {
		return none
	}
	looked_up := g.tc.cur_scope.lookup(node.value) or { types.Type(types.void_) }
	if looked_up !is types.Void {
		return none
	}
	call_key := g.call_key(id, node.value)
	if call_key in g.tc.fn_param_types && call_key in g.tc.fn_ret_types {
		return g.direct_call_name_for_call(id, call_key)
	}
	if node.value in g.tc.fn_param_types && node.value in g.tc.fn_ret_types {
		return g.direct_call_name_for_call(id, node.value)
	}
	return none
}

fn (mut g FlatGen) callback_fn_value_type(name string) ?types.FnType {
	params := g.tc.fn_param_types[name] or { return none }
	ret := g.tc.fn_ret_types[name] or { return none }
	return types.FnType{
		params:      params.clone()
		return_type: ret
	}
}

fn (mut g FlatGen) callback_fn_types_direct_compatible(actual types.FnType, expected types.FnType, expected_c_abi string) bool {
	if actual.params.len != expected.params.len {
		return false
	}
	if g.callback_c_type(actual.return_type) != g.callback_expected_return_c_type(expected.return_type,
		expected_c_abi) {
		return false
	}
	for i in 0 .. expected.params.len {
		if g.callback_c_type(fn_type_param(actual, i)) != g.callback_expected_param_c_type(expected,
			i, expected_c_abi) {
			return false
		}
	}
	return true
}

fn (mut g FlatGen) ensure_callback_userdata_wrapper(actual_name string, actual types.FnType, expected types.FnType, expected_c_abi string) ?string {
	if actual.params.len != expected.params.len {
		return none
	}
	actual_ret_ct := g.callback_c_type(actual.return_type)
	expected_ret_ct := g.callback_expected_return_c_type(expected.return_type, expected_c_abi)
	if actual_ret_ct != expected_ret_ct {
		return none
	}
	mut needs_wrapper := false
	mut param_decls := []string{}
	mut call_args := []string{}
	for i in 0 .. expected.params.len {
		expected_param := fn_type_param(expected, i)
		actual_param := fn_type_param(actual, i)
		expected_ct := g.callback_expected_param_c_type(expected, i, expected_c_abi)
		actual_ct := g.callback_c_type(actual_param)
		param_decls << '${expected_ct} arg${i}'
		if actual_ct == expected_ct {
			call_args << 'arg${i}'
			continue
		}
		if g.callback_can_cast_userdata_param(actual_param, expected_param) {
			call_args << '(${actual_ct})arg${i}'
			needs_wrapper = true
			continue
		}
		if callback_can_cast_const_abi_param(actual_ct, expected_ct) {
			call_args << '(${actual_ct})arg${i}'
			needs_wrapper = true
			continue
		}
		return none
	}
	if !needs_wrapper {
		return none
	}
	actual_c_name := g.callback_c_fn_name(actual_name)
	expected_key := if expected_c_abi.len > 0 {
		expected_c_abi
	} else {
		g.callback_fn_type_key(expected)
	}
	key := '${actual_c_name}|${g.callback_fn_type_key(actual)}|${expected_key}'
	if name := g.callback_wrapper_names[key] {
		return name
	}
	name := g.cname('${actual_c_name}_callback_adapter_${callback_stable_key_hash(key)}')
	g.callback_wrapper_names[key] = name
	params := if param_decls.len == 0 { 'void' } else { param_decls.join(', ') }
	call := '${actual_c_name}(${call_args.join(', ')})'
	body := if expected_ret_ct == 'void' {
		'static void ${name}(${params}) { ${call}; }'
	} else {
		'static ${expected_ret_ct} ${name}(${params}) { return ${call}; }'
	}
	g.callback_wrapper_defs << body
	return name
}

fn (mut g FlatGen) callback_expected_return_c_type(typ types.Type, expected_c_abi string) string {
	if expected_c_abi.len > 0 {
		ret, _ := fn_ptr_typedef_parts(expected_c_abi)
		return trimmed_space(ret)
	}
	return g.callback_c_type(typ)
}

fn (mut g FlatGen) callback_expected_param_c_type(expected types.FnType, idx int, expected_c_abi string) string {
	if expected_c_abi.len > 0 {
		params := callback_fn_ptr_param_c_types(expected_c_abi)
		if idx < params.len {
			return params[idx]
		}
	}
	return g.callback_c_type(fn_type_param(expected, idx))
}

fn callback_fn_ptr_param_c_types(encoded string) []string {
	_, params := fn_ptr_typedef_parts(encoded)
	clean := trimmed_space(params)
	if clean.len == 0 || clean == 'void' {
		return []string{}
	}
	mut out := []string{}
	for param in clean.split(',') {
		out << trimmed_space(param)
	}
	return out
}

fn callback_can_cast_const_abi_param(actual_ct string, expected_ct string) bool {
	actual := trimmed_space(actual_ct)
	expected := trimmed_space(expected_ct)
	if !expected.starts_with('const ') || !expected.ends_with('*') {
		return false
	}
	return actual == expected['const '.len..].trim_space()
}

fn (mut g FlatGen) callback_c_type(typ types.Type) string {
	if typ is types.Void {
		return 'void'
	}
	mut ct := if typ is types.OptionType || typ is types.ResultType {
		g.optional_type_name(typ)
	} else {
		g.tc.c_type(typ)
	}
	if ct.starts_with('fn_ptr:') {
		ct = g.resolve_fn_ptr_type(ct)
	}
	return ct
}

fn (mut g FlatGen) callback_fn_type_key(typ types.FnType) string {
	mut parts := []string{}
	for i in 0 .. typ.params.len {
		parts << g.callback_c_type(fn_type_param(typ, i))
	}
	return '${g.callback_c_type(typ.return_type)}|${parts.join(',')}'
}

fn callback_stable_key_hash(key string) string {
	mut hash := u64(1469598103934665603)
	for b in key.bytes() {
		hash ^= u64(b)
		hash *= u64(1099511628211)
	}
	return '${hash}'
}

fn (g &FlatGen) callback_can_cast_userdata_param(actual types.Type, expected types.Type) bool {
	return (callback_is_voidptr_type(expected) && callback_is_nonvoid_pointer_type(actual))
		|| (callback_is_nonvoid_pointer_type(expected) && callback_is_voidptr_type(actual))
}

fn callback_is_voidptr_type(typ types.Type) bool {
	clean := callback_unalias_type(typ)
	if clean is types.Pointer {
		base := callback_unalias_type(clean.base_type)
		return base is types.Void
	}
	return false
}

fn callback_is_nonvoid_pointer_type(typ types.Type) bool {
	clean := callback_unalias_type(typ)
	if clean is types.Pointer {
		base := callback_unalias_type(clean.base_type)
		return base !is types.Void
	}
	return false
}

fn callback_unalias_type(typ types.Type) types.Type {
	if typ is types.Alias {
		return callback_unalias_type(typ.base_type)
	}
	return typ
}

fn (g &FlatGen) callback_c_fn_name(name string) string {
	if name.starts_with('C.') {
		return g.cname(name)
	}
	if g.test_files.len > 0 && (name == 'main' || name == 'main.main') {
		return g.test_user_main_c_name()
	}
	if name.starts_with('main.') {
		return g.cname(name.all_after_last('.'))
	}
	return g.cname(name)
}

fn fn_type_param(typ types.FnType, idx int) types.Type {
	return typ.params[idx]
}

// gen_optional_arg emits optional arg output for c.
fn (mut g FlatGen) gen_optional_arg(arg_id flat.NodeId, expected types.Type) bool {
	return g.gen_optional_arg_with_abi(arg_id, expected, false)
}

fn (mut g FlatGen) gen_optional_arg_with_abi(arg_id flat.NodeId, expected types.Type, concrete_abi bool) bool {
	mut base_type := types.Type(types.void_)
	if expected is types.OptionType {
		base_type = expected.base_type
	} else if expected is types.ResultType {
		base_type = expected.base_type
	} else {
		return false
	}
	if g.expr_is_optional_literal(arg_id, expected) {
		collapsed := g.collapsed_optional_literal(arg_id, expected)
		if concrete_abi {
			ct := g.concrete_optional_type_name(expected)
			if value_id := g.optional_literal_value_id(collapsed) {
				if base_type is types.Void {
					g.write('(${ct}){.ok = true}')
				} else {
					g.write('(${ct}){.ok = true, .value = ')
					g.gen_expr_with_expected_type(value_id, base_type)
					g.write('}')
				}
				return true
			}
			g.write('(${ct}){.ok = false')
			if err_id := g.optional_literal_err_id(collapsed) {
				g.write(', .err = ')
				g.gen_expr(err_id)
			}
			g.write('}')
			return true
		}
		g.gen_expr_with_expected_type(collapsed, expected)
		return true
	}
	arg_node := g.a.nodes[int(arg_id)]
	if concrete_abi && arg_node.kind == .none_expr {
		ct := g.concrete_optional_type_name(expected)
		g.write('(${ct}){.ok = false}')
		return true
	}
	arg_type := g.usable_expr_type(arg_id)
	if arg_type is types.OptionType || arg_type is types.ResultType {
		if concrete_abi {
			g.gen_concrete_optional_arg_from_optional_expr(arg_id, arg_type, expected, base_type)
			return true
		}
		if g.type_names_match(arg_type, expected) {
			g.gen_expr_with_expected_type(arg_id, expected)
			return true
		}
		if arg_node.kind == .none_expr || g.expr_really_returns_optional(arg_id) {
			g.gen_expr_with_expected_type(arg_id, expected)
			return true
		}
	}
	ct := if concrete_abi {
		g.concrete_optional_type_name(expected)
	} else {
		g.optional_type_name(expected)
	}
	if base_type is types.Void {
		g.write('(${ct}){.ok = true}')
		return true
	}
	g.write('(${ct}){.ok = true, .value = ')
	g.gen_expr_with_expected_type(arg_id, base_type)
	g.write('}')
	return true
}

fn (mut g FlatGen) gen_concrete_optional_arg_from_optional_expr(arg_id flat.NodeId, arg_type types.Type, expected types.Type, base_type types.Type) {
	dest_ct := g.concrete_optional_type_name(expected)
	source_ct := g.optional_type_name_for_expr(arg_id, arg_type)
	if source_ct == dest_ct {
		g.gen_expr_with_expected_type(arg_id, expected)
		return
	}
	tmp := g.tmp_count
	g.tmp_count++
	g.write('({ ${source_ct} _opt${tmp} = ')
	g.gen_expr_with_expected_type(arg_id, arg_type)
	g.write('; _opt${tmp}.ok ? (${dest_ct}){.ok = true')
	if base_type !is types.Void {
		g.write(', .value = _opt${tmp}.value')
	}
	g.write('} : (${dest_ct}){.ok = false, .err = _opt${tmp}.err}; })')
}

// expr_is_optional_literal supports expr is optional literal handling for FlatGen.
fn (mut g FlatGen) expr_is_optional_literal(id flat.NodeId, expected types.Type) bool {
	if int(id) < 0 {
		return false
	}
	node := g.a.nodes[int(id)]
	if node.kind != .struct_init && node.kind != .cast_expr {
		return false
	}
	return node.value.starts_with('?') || node.value.starts_with('!')
		|| node.value == g.optional_type_name(expected) || node.value.starts_with('Optional')
}

// collapsed_optional_literal supports collapsed optional literal handling for FlatGen.
fn (mut g FlatGen) collapsed_optional_literal(id flat.NodeId, expected types.Type) flat.NodeId {
	mut current := id
	for _ in 0 .. 4 {
		value_id := g.optional_literal_value_id(current) or { break }
		if !g.expr_is_optional_literal(value_id, expected) {
			break
		}
		current = value_id
	}
	return current
}

// optional_literal_value_id supports optional literal value id handling for FlatGen.
fn (g &FlatGen) optional_literal_value_id(id flat.NodeId) ?flat.NodeId {
	if int(id) < 0 {
		return none
	}
	node := g.a.nodes[int(id)]
	if node.kind != .struct_init && node.kind != .cast_expr {
		return none
	}
	for i in 0 .. node.children_count {
		field := g.a.child_node(&node, i)
		if field.kind == .field_init && field.value == 'value' && field.children_count > 0 {
			return g.a.child(field, 0)
		}
	}
	return none
}

fn (g &FlatGen) optional_literal_err_id(id flat.NodeId) ?flat.NodeId {
	if int(id) < 0 {
		return none
	}
	node := g.a.nodes[int(id)]
	if node.kind != .struct_init && node.kind != .cast_expr {
		return none
	}
	for i in 0 .. node.children_count {
		field := g.a.child_node(&node, i)
		if field.kind == .field_init && field.value == 'err' && field.children_count > 0 {
			return g.a.child(field, 0)
		}
	}
	return none
}

// fn_field_type supports fn field type handling for FlatGen.
fn (g &FlatGen) fn_field_type(base_type types.Type, field_name string) ?types.FnType {
	field_type := g.field_type(base_type, field_name) or { return none }
	return fn_type_from(field_type)
}

// field_type supports field type handling for FlatGen.
fn (g &FlatGen) field_type(base_type types.Type, field_name string) ?types.Type {
	clean0 := types.unwrap_pointer(base_type)
	mut clean := clean0
	if clean0 is types.Alias {
		clean = clean0.base_type
	}
	mut struct_name := ''
	if clean is types.Struct {
		struct_name = clean.name
	} else if clean is types.Array {
		struct_name = 'array'
	} else if clean is types.Map {
		struct_name = 'map'
	} else if clean is types.String {
		struct_name = 'string'
	}
	if struct_name.len == 0 {
		return none
	}
	fields := g.tc.structs[struct_name] or { return none }
	for field in fields {
		if field.name == field_name {
			return field.typ
		}
	}
	return none
}

// fn_type_from supports fn type from handling for c.
fn fn_type_from(t types.Type) ?types.FnType {
	if t is types.FnType {
		return t
	}
	if t is types.Alias {
		return fn_type_from(t.base_type)
	}
	return none
}

fn (mut g FlatGen) specialized_generic_plain_fn_name_for_call(id flat.NodeId, node flat.Node, name string) ?string {
	if name.len == 0 || name.contains('[') || node.children_count == 0 {
		return none
	}
	// A transformed receiver call is an already-resolved qualified concrete
	// function. Do not reinterpret its short method name as an unrelated plain
	// generic (for example `sync.WaitGroup.add` versus a user `add[T]`).
	if name.contains('.') && g.concrete_fn_return_known(name) {
		return none
	}
	if g.plain_concrete_fn_name_shadows_generic(name) {
		return none
	}
	base := g.generic_plain_fn_base_for_call(id, name) or { return none }
	generic_params := g.tc.fn_generic_params[base] or { return none }
	if generic_params.len == 0 {
		return none
	}
	param_texts := g.tc.fn_param_type_texts[base] or { return none }
	mut inferred := map[string]string{}
	for i, param_text in param_texts {
		arg_idx := i + 1
		if arg_idx >= int(node.children_count) {
			break
		}
		arg_type := g.generic_call_arg_type_text(g.a.child(&node, arg_idx))
		if arg_type.len > 0 {
			infer_codegen_generic_type_args(param_text, arg_type, mut inferred)
		}
	}
	if ret_text := g.tc.fn_ret_type_texts[base] {
		ret_type := g.call_default_return_type(id).name()
		if ret_type.len > 0 {
			infer_codegen_generic_type_args(ret_text, ret_type, mut inferred)
		}
	}
	mut args := []string{cap: generic_params.len}
	for param in generic_params {
		arg := inferred[param] or { return none }
		if codegen_generic_arg_is_unresolved(arg) {
			return none
		}
		args << arg
	}
	if args.len == 0 {
		return none
	}
	for candidate in g.specialized_generic_plain_fn_candidates(base, args) {
		if candidate in g.tc.fn_param_types || candidate in g.tc.fn_ret_types {
			return candidate
		}
	}
	return none
}

fn (g &FlatGen) plain_concrete_fn_name_shadows_generic(name string) bool {
	if name.len == 0 || name.contains('.') {
		return false
	}
	if g.non_generic_fn_decl_exists_in_module(name, g.tc.cur_module) {
		return true
	}
	if name in g.tc.fn_generic_params {
		return false
	}
	qname := g.tc.qualify_fn_name(name)
	if qname != name {
		if qname in g.tc.fn_generic_params {
			return false
		}
		if g.concrete_fn_return_known(qname) {
			return true
		}
	}
	if g.concrete_fn_return_known(name) {
		return true
	}
	return qname != name && g.concrete_fn_return_known(qname)
}

// precompute_non_generic_fn_index builds the lookup consumed by
// non_generic_fn_decl_exists_in_module. It mirrors that scan exactly: module
// starts at 'main' and is only advanced by `.module_decl` nodes (not reset per
// file), so the attribution matches the previous per-call behavior byte for byte.
fn (mut g FlatGen) precompute_non_generic_fn_index() {
	mut cur_module := 'main'
	for node in g.a.nodes {
		match node.kind {
			.module_decl {
				cur_module = node.value
			}
			.fn_decl {
				if node.generic_params.len == 0 && !node.typ.contains('generic') {
					g.non_generic_fn_names_by_module['${cur_module}\x01${node.value}'] = true
				}
			}
			else {}
		}
	}
}

fn (g &FlatGen) non_generic_fn_decl_exists_in_module(name string, module_name string) bool {
	if name.len == 0 || name.contains('.') {
		return false
	}
	return '${module_name}\x01${name}' in g.non_generic_fn_names_by_module
}

// precompute_generic_fn_key_index builds the lookups consumed by
// generic_plain_fn_base_for_call: every tc.fn_generic_params key indexed by its
// short (post-dot) name and by its c_name spelling, plus its position in the
// map's iteration order so multi-match resolution below replays the original
// full-map scan byte for byte.
fn (mut g FlatGen) precompute_generic_fn_key_index() {
	mut ordinal := 0
	for key, _ in g.tc.fn_generic_params {
		g.generic_fn_keys_by_short[key.all_after_last('.')] << key
		g.generic_fn_keys_by_cname[g.cname(key)] << key
		g.generic_fn_key_ordinal[key] = ordinal
		ordinal++
	}
}

// generic_fn_keys_matching returns the fn_generic_params keys whose short name
// is `short` or whose c_name is `name`, in the params map's iteration order.
fn (g &FlatGen) generic_fn_keys_matching(short string, name string) []string {
	by_short := g.generic_fn_keys_by_short[short] or { []string{} }
	by_cname := g.generic_fn_keys_by_cname[name] or { []string{} }
	if by_cname.len == 0 {
		return by_short
	}
	if by_short.len == 0 {
		return by_cname
	}
	// Merge the two ordinal-sorted bucket lists, dropping keys present in both.
	mut merged := []string{cap: by_short.len + by_cname.len}
	mut i := 0
	mut j := 0
	for i < by_short.len || j < by_cname.len {
		if i >= by_short.len {
			merged << by_cname[j]
			j++
			continue
		}
		if j >= by_cname.len {
			merged << by_short[i]
			i++
			continue
		}
		oi := g.generic_fn_key_ordinal[by_short[i]]
		oj := g.generic_fn_key_ordinal[by_cname[j]]
		if oi == oj {
			merged << by_short[i]
			i++
			j++
		} else if oi < oj {
			merged << by_short[i]
			i++
		} else {
			merged << by_cname[j]
			j++
		}
	}
	return merged
}

fn (g &FlatGen) concrete_fn_return_known(name string) bool {
	if name in g.tc.fn_generic_params {
		return false
	}
	if ret := g.fn_decl_ret_types[name] {
		return ret !is types.Unknown
	}
	ret := g.tc.fn_ret_types[name] or { return false }
	return ret !is types.Unknown
}

fn (g &FlatGen) resolved_name_is_generic_plain(name string) bool {
	if name in g.tc.fn_generic_params {
		return true
	}
	if !name.contains('_T_') {
		return false
	}
	base := name.all_before('_T_')
	return base in g.tc.fn_generic_params || base.replace('__', '.') in g.tc.fn_generic_params
}

fn (g &FlatGen) generic_plain_fn_base_for_call(id flat.NodeId, name string) ?string {
	mut candidates := []string{}
	if resolved := g.tc.resolved_call_name(id) {
		if resolved_call_matches_target(resolved, name) {
			candidates << resolved
		}
	}
	if !name.contains('.') && g.tc.cur_module.len > 0 && g.tc.cur_module !in ['', 'main', 'builtin'] {
		candidates << '${g.tc.cur_module}.${name}'
	}
	candidates << name
	if name.contains('__') {
		candidates << name.replace('__', '.')
	}
	normalized := g.normalize_call_key(name)
	candidates << normalized
	short := name.all_after_last('.')
	mut found := ''
	for candidate in candidates {
		if base := g.known_generic_plain_fn_base(candidate) {
			return base
		}
	}
	// `key == short || key.ends_with('.${short}')` is exactly "key's post-dot
	// short name equals short", so the precomputed buckets cover the old
	// full-map scan (see precompute_generic_fn_key_index).
	cur_module_prefix := '${g.tc.cur_module}.'
	for key in g.generic_fn_keys_matching(short, name) {
		if key.starts_with(cur_module_prefix) {
			return key
		}
		if found.len > 0 && found != key {
			return none
		}
		found = key
	}
	if found.len > 0 {
		return found
	}
	return none
}

fn (g &FlatGen) known_generic_plain_fn_base(name string) ?string {
	if name.len == 0 {
		return none
	}
	if name in g.tc.fn_generic_params {
		if !name.contains('.') {
			if module_name := g.tc.fn_type_modules[name] {
				if module_name.len > 0 && module_name !in ['main', 'builtin'] {
					qualified := '${module_name}.${name}'
					if qualified in g.tc.fn_generic_params {
						return qualified
					}
				}
			}
		}
		return name
	}
	if name.starts_with('main.') {
		short := name.all_after_last('.')
		if short in g.tc.fn_generic_params {
			return short
		}
	}
	if name.contains('__') {
		dotted := name.replace('__', '.')
		if dotted in g.tc.fn_generic_params {
			return dotted
		}
	}
	return none
}

fn (g &FlatGen) generic_call_arg_type_text(id flat.NodeId) string {
	if int(id) < 0 || int(id) >= g.a.nodes.len {
		return ''
	}
	node := g.a.nodes[int(id)]
	if node.kind == .ident {
		typ := g.usable_expr_type(id)
		name := typ.name()
		if codegen_type_text_is_usable_for_generic_inference(name) {
			return name
		}
	}
	if node.kind == .call {
		if ret_type := g.generic_call_return_type_text_for_inference(id, node) {
			return ret_type
		}
		if ret_type := g.registered_call_return_type_text(node) {
			return ret_type
		}
	}
	if codegen_type_text_is_usable_for_generic_inference(node.typ) {
		return node.typ
	}
	typ := g.usable_expr_type(id)
	name := typ.name()
	if codegen_type_text_is_usable_for_generic_inference(name) {
		return name
	}
	return ''
}

fn (g &FlatGen) generic_call_return_type_text_for_inference(id flat.NodeId, node flat.Node) ?string {
	if node.kind != .call || node.children_count == 0 {
		return none
	}
	target := g.call_target_name(g.a.child(&node, 0))
	base := g.generic_plain_fn_base_for_call(id, target) or { return none }
	params := g.tc.fn_generic_params[base] or { return none }
	if params.len == 0 {
		return none
	}
	param_texts := g.tc.fn_param_type_texts[base] or { return none }
	mut inferred := map[string]string{}
	for i, param_text in param_texts {
		arg_idx := i + 1
		if arg_idx >= int(node.children_count) {
			break
		}
		arg_type := g.generic_call_arg_type_text(g.a.child(&node, arg_idx))
		if arg_type.len > 0 {
			infer_codegen_generic_type_args(param_text, arg_type, mut inferred)
		}
	}
	mut args := []string{cap: params.len}
	for param in params {
		arg := inferred[param] or { return none }
		if codegen_generic_arg_is_unresolved(arg) {
			return none
		}
		args << arg
	}
	ret_text := g.tc.fn_ret_type_texts[base] or { node.typ }
	if ret_text.len == 0 {
		return none
	}
	ret := substitute_shared_generic_type_text(ret_text, params, args)
	if codegen_type_text_is_usable_for_generic_inference(ret) {
		return ret
	}
	return none
}

fn (g &FlatGen) registered_call_return_type_text(node flat.Node) ?string {
	if node.kind != .call || node.children_count == 0 {
		return none
	}
	if typ := g.specialized_receiver_call_return_type_text(node) {
		return typ
	}
	target := g.call_target_name(g.a.child(&node, 0))
	for candidate in [target, g.normalize_call_key(target), g.cname(target)] {
		if ret := g.fn_decl_ret_types[candidate] {
			name := ret.name()
			if codegen_type_text_is_usable_for_generic_inference(name) {
				return name
			}
		}
		if ret := g.tc.fn_ret_types[candidate] {
			name := ret.name()
			if codegen_type_text_is_usable_for_generic_inference(name) {
				return name
			}
		}
	}
	return none
}

fn (g &FlatGen) specialized_receiver_call_return_type_text(node flat.Node) ?string {
	target := g.call_target_name(g.a.child(&node, 0))
	if !target.contains('.') {
		return none
	}
	receiver := target.all_before_last('.')
	method := target.all_after_last('.')
	if receiver.len == 0 || method.len == 0 {
		return none
	}
	mod_name := if receiver.contains('.') { receiver.all_before_last('.') } else { '' }
	short_receiver := receiver.all_after_last('.')
	for i := short_receiver.len - 1; i >= 0; i-- {
		if short_receiver[i] != `_` {
			continue
		}
		base_short := short_receiver[..i]
		suffix := short_receiver[i + 1..]
		if base_short.len == 0 || suffix.len == 0 {
			continue
		}
		base := if mod_name.len > 0 { '${mod_name}.${base_short}' } else { base_short }
		arg := codegen_generic_type_arg_from_suffix(suffix)
		if arg.len == 0 {
			continue
		}
		return '${base}[${arg}]'
	}
	return none
}

fn codegen_generic_type_arg_from_suffix(suffix string) string {
	clean := trimmed_space(suffix)
	return match clean {
		'string' { 'string' }
		'bool' { 'bool' }
		'int' { 'int' }
		'u8' { 'u8' }
		'u16' { 'u16' }
		'u32' { 'u32' }
		'u64' { 'u64' }
		'i8' { 'i8' }
		'i16' { 'i16' }
		'i32' { 'i32' }
		'i64' { 'i64' }
		'f32' { 'f32' }
		'f64' { 'f64' }
		else { clean.replace('__', '.') }
	}
}

fn codegen_type_text_is_usable_for_generic_inference(typ string) bool {
	clean := trimmed_space(typ)
	if clean.len == 0 || clean in ['void', 'unknown', 'generic'] {
		return false
	}
	return !codegen_generic_arg_is_unresolved(clean)
}

fn (mut g FlatGen) specialized_generic_plain_fn_candidates(base string, args []string) []string {
	mut suffixes := codegen_generic_type_suffix_variants(args)
	mut bases := []string{}
	bases << base
	if base.contains('.') {
		bases << base.all_after_last('.')
	} else if module_name := g.tc.fn_type_modules[base] {
		if module_name.len > 0 && module_name !in ['main', 'builtin'] {
			bases.insert(0, '${module_name}.${base}')
		}
	} else if g.tc.cur_module.len > 0 && g.tc.cur_module !in ['', 'main', 'builtin'] {
		bases << '${g.tc.cur_module}.${base}'
	}
	mut result := []string{}
	for suffix in suffixes {
		for fn_base in bases {
			if fn_base.len == 0 || suffix.len == 0 {
				continue
			}
			codegen_specialized_receiver_fn_candidates(mut result, fn_base, suffix)
			spec := '${fn_base}_T_${suffix}'
			codegen_push_unique(mut result, spec)
			codegen_push_unique(mut result, g.cname(spec))
		}
	}
	return result
}

fn codegen_specialized_receiver_fn_candidates(mut result []string, fn_base string, suffix string) {
	if !fn_base.contains('.') {
		return
	}
	receiver := fn_base.all_before_last('.')
	method := fn_base.all_after_last('.')
	if receiver.len == 0 || method.len == 0 {
		return
	}
	spec := '${receiver}_${suffix}.${method}'
	codegen_push_unique(mut result, spec)
	codegen_push_unique(mut result, c_name(spec))
	if receiver.contains('.') {
		mod_name := receiver.all_before_last('.')
		short_receiver := receiver.all_after_last('.')
		short_spec := '${short_receiver}_${suffix}.${method}'
		qualified_short_spec := '${mod_name}.${short_receiver}_${suffix}.${method}'
		codegen_push_unique(mut result, short_spec)
		codegen_push_unique(mut result, c_name(short_spec))
		codegen_push_unique(mut result, qualified_short_spec)
		codegen_push_unique(mut result, c_name(qualified_short_spec))
	}
}

fn codegen_generic_type_suffix_variants(args []string) []string {
	mut suffixes := []string{}
	mut short_raw_parts := []string{cap: args.len}
	mut short_parts := []string{cap: args.len}
	mut full_parts := []string{cap: args.len}
	for arg in args {
		clean := trimmed_space(arg)
		short_raw := generic_receiver_type_arg_short(clean).replace('[]', 'Array_').replace('&',
			'ptr_')
		short_raw_parts << short_raw
		short_parts << c_name(short_raw)
		full_parts << c_name(clean.replace('[]', 'Array_').replace('&', 'ptr_'))
	}
	codegen_push_unique(mut suffixes, short_raw_parts.join('_'))
	codegen_push_unique(mut suffixes, short_parts.join('_'))
	codegen_push_unique(mut suffixes, full_parts.join('_'))
	return suffixes
}

fn codegen_push_unique(mut values []string, value string) {
	if value.len > 0 && value !in values {
		values << value
	}
}

fn infer_codegen_generic_type_args(param_type string, arg_type string, mut inferred map[string]string) {
	param := trimmed_space(param_type)
	arg := trimmed_space(arg_type)
	if param.len == 0 || arg.len == 0 || arg in ['unknown', 'generic'] {
		return
	}
	if codegen_generic_placeholder_name(param) {
		if param !in inferred {
			inferred[param] = arg
		}
		return
	}
	if param.starts_with('&') {
		infer_codegen_generic_type_args(param[1..], arg.trim_left('&'), mut inferred)
		return
	}
	if param.starts_with('mut ') {
		infer_codegen_generic_type_args(param[4..], arg.trim_left('&'), mut inferred)
		return
	}
	if param.starts_with('...') {
		infer_codegen_generic_type_args(param[3..], arg.trim_left('[]'), mut inferred)
		return
	}
	if param.starts_with('[]') && arg.starts_with('[]') {
		infer_codegen_generic_type_args(param[2..], arg[2..], mut inferred)
		return
	}
	if (param.starts_with('?') && arg.starts_with('?'))
		|| (param.starts_with('!') && arg.starts_with('!')) {
		infer_codegen_generic_type_args(param[1..], arg[1..], mut inferred)
		return
	}
	if param.starts_with('map[') && arg.starts_with('map[') {
		p_end := shared_generic_matching_bracket(param, 3)
		a_end := shared_generic_matching_bracket(arg, 3)
		if p_end < param.len && a_end < arg.len {
			infer_codegen_generic_type_args(param[4..p_end], arg[4..a_end], mut inferred)
			infer_codegen_generic_type_args(param[p_end + 1..], arg[a_end + 1..], mut inferred)
		}
		return
	}
	p_base, p_args, p_ok := shared_generic_app_parts(param)
	if p_ok {
		a_base, a_args, a_ok := shared_generic_app_parts(arg)
		if a_ok && p_base.all_after_last('.') == a_base.all_after_last('.') {
			for i, p_arg in p_args {
				if i < a_args.len {
					infer_codegen_generic_type_args(p_arg, a_args[i], mut inferred)
				}
			}
		}
	}
}

fn codegen_generic_placeholder_name(name string) bool {
	clean := trimmed_space(name)
	if clean.len == 0 || clean.contains('.') || clean.contains('[') || clean.contains(']')
		|| clean.contains(' ') {
		return false
	}
	if clean.len == 1 {
		return clean[0] >= `A` && clean[0] <= `Z`
	}
	return clean[0] >= `A` && clean[0] <= `Z` && clean[1] >= `0` && clean[1] <= `9`
}

fn codegen_generic_arg_is_unresolved(arg string) bool {
	clean := trimmed_space(arg)
	if clean.len == 0 {
		return true
	}
	if codegen_generic_placeholder_name(clean) {
		return true
	}
	if clean.starts_with('&') || clean.starts_with('?') || clean.starts_with('!') {
		return codegen_generic_arg_is_unresolved(clean[1..])
	}
	if clean.starts_with('mut ') {
		return codegen_generic_arg_is_unresolved(clean[4..])
	}
	if clean.starts_with('...') {
		return codegen_generic_arg_is_unresolved(clean[3..])
	}
	if clean.starts_with('[]') {
		return codegen_generic_arg_is_unresolved(clean[2..])
	}
	if clean.starts_with('map[') {
		bracket_end := shared_generic_matching_bracket(clean, 3)
		if bracket_end < clean.len {
			return codegen_generic_arg_is_unresolved(clean[4..bracket_end])
				|| codegen_generic_arg_is_unresolved(clean[bracket_end + 1..])
		}
	}
	_, nested_args, ok := shared_generic_app_parts(clean)
	if ok {
		for nested in nested_args {
			if codegen_generic_arg_is_unresolved(nested) {
				return true
			}
		}
	}
	return false
}

fn (g &FlatGen) fn_value_call_param_types(callee_id flat.NodeId) ?[]types.Type {
	if int(callee_id) < 0 {
		return none
	}
	node := g.a.nodes[int(callee_id)]
	if node.kind == .ident {
		if typ := g.cur_param_types[node.value] {
			if ft := fn_type_from(typ) {
				return ft.params.clone()
			}
		}
	}
	if node.typ.len > 0 {
		if ft := fn_type_from(g.tc.parse_type(node.typ)) {
			return ft.params.clone()
		}
	}
	if node.kind != .ident {
		if ft := fn_type_from(g.tc.resolve_type(callee_id)) {
			return ft.params.clone()
		}
	}
	return none
}

fn (g &FlatGen) selector_call_can_emit_direct(resolved string, node flat.Node) bool {
	if resolved.len == 0 || node.children_count == 0 {
		return false
	}
	fn_node := g.a.child_node(&node, 0)
	if fn_node.kind != .selector {
		return false
	}
	params := g.tc.fn_param_types[resolved] or {
		if resolved in g.tc.fn_ret_types {
			return node.children_count == 1
		}
		return false
	}
	return params.len == node.children_count - 1
}

fn (g &FlatGen) selector_module_call_name(fn_node flat.Node, node flat.Node) ?string {
	if fn_node.kind != .selector || fn_node.children_count == 0 {
		return none
	}
	base := g.a.child_node(&fn_node, 0)
	if base.kind != .ident {
		return none
	}
	if g.selector_base_is_value(base.value) {
		return none
	}
	mod_name := g.selector_base_module(base.value) or {
		if base.value == g.tc.cur_module {
			base.value
		} else {
			''
		}
	}
	if mod_name.len == 0 {
		return none
	}
	call_name := '${mod_name}.${fn_node.value}'
	arg_start := g.selector_module_call_arg_start(fn_node, node)
	params := g.tc.fn_param_types[call_name] or {
		if call_name in g.tc.fn_ret_types && node.children_count == arg_start {
			return call_name
		}
		return none
	}
	if g.module_call_arg_count_matches(call_name, params, node.children_count - arg_start) {
		return call_name
	}
	return none
}

fn (g &FlatGen) module_call_arg_count_matches(fn_name string, params []types.Type, supplied int) bool {
	if params.len == supplied {
		return true
	}
	if !(g.tc.fn_variadic[fn_name] or { false }) && !(g.tc.c_variadic_fns[fn_name] or { false }) {
		return false
	}
	if params.len == 0 {
		return supplied == 0
	}
	min_args := if params[params.len - 1] is types.Array { params.len - 1 } else { params.len }
	return supplied >= min_args
}

fn (g &FlatGen) selector_module_call_arg_start(fn_node flat.Node, node flat.Node) int {
	if fn_node.kind == .selector && fn_node.children_count > 0 && node.children_count > 1 {
		base := g.a.child_node(&fn_node, 0)
		first_arg := g.a.child_node(&node, 1)
		if base.kind == .ident && first_arg.kind == .ident && first_arg.value == base.value {
			return 2
		}
	}
	return 1
}

fn (g &FlatGen) target_module_call_name(target string, node flat.Node) ?string {
	if !target.contains('.') {
		return none
	}
	if node.children_count > 0 {
		fn_node := g.a.child_node(&node, 0)
		if fn_node.kind == .selector && fn_node.children_count > 0 {
			original_base := g.a.child_node(fn_node, 0)
			if original_base.kind == .ident && g.selector_base_is_value(original_base.value) {
				return none
			}
		}
	}
	base := target.all_before_last('.')
	method := target.all_after_last('.')
	if base.len == 0 || method.len == 0 {
		return none
	}
	if typ := g.tc.cur_scope.lookup(base) {
		if typ !is types.Void {
			return none
		}
	}
	static_name := '${base}.${method}'
	if static_name in g.tc.fn_param_types || static_name in g.tc.fn_ret_types
		|| static_name in g.tc.fn_generic_params {
		arg_start := g.target_module_call_arg_start(target, node)
		params := g.tc.fn_param_types[static_name] or {
			if static_name in g.tc.fn_ret_types && node.children_count == arg_start {
				return static_name
			}
			return none
		}
		if g.module_call_arg_count_matches(static_name, params, node.children_count - arg_start) {
			return static_name
		}
	}
	mod_name := g.selector_base_module(base) or {
		if base == g.tc.cur_module {
			base
		} else {
			''
		}
	}
	if mod_name.len == 0 {
		return none
	}
	call_name := '${mod_name}.${method}'
	arg_start := g.target_module_call_arg_start(target, node)
	params := g.tc.fn_param_types[call_name] or {
		if call_name in g.tc.fn_ret_types && node.children_count == arg_start {
			return call_name
		}
		return none
	}
	if g.module_call_arg_count_matches(call_name, params, node.children_count - arg_start) {
		return call_name
	}
	return none
}

fn (g &FlatGen) target_module_call_arg_start(target string, node flat.Node) int {
	if !target.contains('.') || node.children_count <= 1 {
		return 1
	}
	base := target.all_before_last('.')
	first_arg := g.a.child_node(&node, 1)
	if first_arg.kind == .ident && first_arg.value == base {
		return 2
	}
	return 1
}

// gen_call_args emits call args output for c.
fn (mut g FlatGen) gen_call_args(fn_name string, node flat.Node, start int) {
	callee_name := if node.children_count > 0 {
		g.call_target_name(g.a.child(&node, 0))
	} else {
		''
	}
	is_c_call := fn_name.starts_with('C.') || callee_name.starts_with('C.')
	mut param_types := g.param_types_for(fn_name, fn_name.all_after_last('.'))
	// Retry with the bare method name only when the qualified name is genuinely unresolved —
	// not when it is a known function that simply takes no parameters. The short-name index
	// matches any same-named function, so retrying a real 0-param fn (e.g. a static method
	// `Type.build()`) would adopt an unrelated `build`'s parameters and append phantom args.
	if !is_c_call && param_types.len == 0 && fn_name.contains('.')
		&& fn_name !in g.tc.fn_param_types && g.cname(fn_name) !in g.tc.fn_param_types {
		param_types = g.param_types_for(fn_name.all_after_last('.'), fn_name.all_after_last('.'))
	}
	callee_uses_specialized_generic_abi := if node.children_count > 0 {
		g.call_callee_uses_specialized_generic_abi(g.a.child(&node, 0))
	} else {
		false
	}
	concrete_optional_args := g.call_uses_concrete_optional_params(fn_name)
		|| g.call_uses_concrete_optional_params(g.direct_call_name(fn_name))
		|| g.call_uses_concrete_optional_params(callee_name)
		|| g.call_uses_concrete_optional_params(g.direct_call_name(callee_name))
		|| (callee_uses_specialized_generic_abi && params_have_optional_result(param_types))
	is_c_variadic_fn := g.tc.c_variadic_fns[fn_name] or { false }
	is_variadic_fn := !is_c_variadic_fn && ((g.tc.fn_variadic[fn_name] or { false })
		|| g.fn_decl_is_variadic(fn_name, callee_name))
	variadic_idx := if is_variadic_fn && param_types.len > 0
		&& param_types[param_types.len - 1] is types.Array {
		param_types.len - 1
	} else {
		-1
	}
	typed_param_count := if is_c_variadic_fn && param_types.len > 0
		&& param_types[param_types.len - 1] is types.Array {
		param_types.len - 1
	} else {
		param_types.len
	}
	num_args := node.children_count - start
	is_variadic := variadic_idx >= 0 && num_args > param_types.len
	for i in start .. node.children_count {
		arg_idx := i - start
		arg_id := g.a.child(&node, i)
		arg_node := g.a.nodes[int(arg_id)]
		if param_types.len > 0 && !is_c_variadic_fn && variadic_idx < 0
			&& arg_idx >= typed_param_count {
			continue
		}
		if i > start {
			g.write(', ')
		}
		if arg_node.kind == .field_init && variadic_idx >= 0 && arg_idx == variadic_idx {
			variadic_type := param_types[variadic_idx]
			if variadic_type is types.Array {
				if variadic_type.elem_type is types.Struct {
					c_elem := g.tc.c_type(variadic_type.elem_type)
					g.write('new_array_from_c_array(1, 1, sizeof(${c_elem}), (${c_elem}[]){')
					g.gen_params_struct_arg(variadic_type.elem_type, node, i)
					g.write('})')
					break
				}
			}
		}
		if arg_node.kind == .field_init {
			// `@[params]` struct argument: trailing `key: value` args form a struct literal
			ptyp := if arg_idx < typed_param_count {
				param_types[arg_idx]
			} else {
				types.Type(types.void_)
			}
			g.gen_params_struct_arg(ptyp, node, i)
			break
		}
		if !is_c_call && arg_idx == 0 && start == 1 && arg_node.kind == .ident
			&& (g.mut_receiver_arg_wants_addr(fn_name, arg_id)
			|| g.mut_receiver_arg_wants_addr(callee_name, arg_id)) {
			g.write('&')
			g.gen_expr(arg_id)
			continue
		}
		if arg_idx < typed_param_count {
			arg_param_is_shared :=
				g.fn_param_is_shared_for_call(arg_idx, fn_name, callee_name, '', '')
			if arg_param_is_shared
				&& (g.gen_shared_local_receiver_arg(arg_id) || g.gen_shared_storage_expr(arg_id)) {
				continue
			}
		}
		if arg_node.kind == .sizeof_expr {
			g.write('sizeof(${g.sizeof_target(arg_node.value)})')
			continue
		}
		if g.gen_ierror_str_arg(fn_name, callee_name, arg_idx, arg_id) {
			continue
		}
		if arg_idx < typed_param_count && param_types[arg_idx] !is types.Pointer
			&& g.gen_addressed_byvalue_arg(arg_node, param_types[arg_idx]) {
			continue
		}
		if arg_idx < typed_param_count
			&& g.gen_fixed_array_pointer_lvalue_arg(arg_id, param_types[arg_idx]) {
			continue
		}
		if fixed := array_fixed_type(g.tc.resolve_type(arg_id)) {
			g.gen_fixed_array_data_arg(arg_id, fixed)
			continue
		}
		if arg_idx < typed_param_count {
			if fixed := array_fixed_type(param_types[arg_idx]) {
				g.gen_fixed_array_data_arg(arg_id, fixed)
				continue
			}
		}
		if arg_idx < typed_param_count
			&& g.gen_pointer_arg_from_array_literal(arg_node, param_types[arg_idx]) {
			continue
		}
		if arg_idx < typed_param_count && g.gen_mut_sum_lvalue_arg(arg_id, param_types[arg_idx]) {
			continue
		}
		cb_param := if arg_idx >= 0 && arg_idx < typed_param_count {
			param_types[arg_idx]
		} else {
			types.Type(types.void_)
		}
		if g.gen_special_c_callback_arg(fn_name, arg_idx, arg_id, cb_param) {
			continue
		}
		if is_c_call {
			if g.c_char_literal_arg(arg_id) {
				if arg_idx < typed_param_count {
					g.gen_expr_with_expected_type(arg_id, param_types[arg_idx])
				} else {
					old_expected := g.expected_expr_type
					g.expected_expr_type = types.Type(types.void_)
					g.gen_expr(arg_id)
					g.expected_expr_type = old_expected
				}
			} else {
				g.gen_expr(arg_id)
			}
			continue
		}
		if variadic_idx >= 0 && arg_idx == variadic_idx {
			variadic_type := param_types[variadic_idx]
			if variadic_type is types.Array {
				if spread_id := g.spread_arg_child(arg_node) {
					g.gen_expr_with_expected_type(spread_id, variadic_type)
					continue
				}
			}
		}
		if is_variadic && arg_idx == variadic_idx {
			variadic_type := param_types[variadic_idx]
			if variadic_type is types.Array {
				c_elem := g.tc.c_type(variadic_type.elem_type)
				count := num_args - variadic_idx
				g.write('new_array_from_c_array(${count}, ${count}, sizeof(${c_elem}), (${c_elem}[]){')
				is_voidptr_variadic := variadic_elem_is_voidptr(variadic_type.elem_type)
				for j in i .. node.children_count {
					if j > i {
						g.write(', ')
					}
					if is_voidptr_variadic {
						g.gen_voidptr_variadic_arg(g.a.child(&node, j))
					} else {
						g.gen_expr_with_expected_type(g.a.child(&node, j), variadic_type.elem_type)
					}
				}
				g.write('})')
			}
			break
		}
		if variadic_idx >= 0 && arg_idx == variadic_idx && num_args == param_types.len {
			arg_type := g.tc.resolve_type(arg_id)
			// A struct literal can never itself be the variadic array; the checker
			// propagates the expected `[]T` onto the node, masking its own type.
			if arg_type !is types.Array || arg_node.kind == .struct_init {
				variadic_type := param_types[variadic_idx]
				if variadic_type is types.Array {
					c_elem := g.tc.c_type(variadic_type.elem_type)
					g.write('new_array_from_c_array(1, 1, sizeof(${c_elem}), (${c_elem}[]){')
					if variadic_elem_is_voidptr(variadic_type.elem_type) {
						g.gen_voidptr_variadic_arg(arg_id)
					} else {
						g.gen_expr_with_expected_type(arg_id, variadic_type.elem_type)
					}
					g.write('})')
					continue
				}
			}
		}
		mut needs_addr := false
		if arg_idx < typed_param_count && param_types[arg_idx] is types.Pointer
			&& !(arg_node.kind == .prefix && arg_node.op == .amp)
			&& !g.arg_is_null_pointer_literal(arg_id, arg_node) {
			arg_type := g.usable_expr_type(arg_id)
			arg_is_shared_local := arg_node.kind == .ident
				&& g.local_storage_is_shared(arg_node.value)
			arg_param_is_shared :=
				g.fn_param_is_shared_for_call(arg_idx, fn_name, callee_name, '', '')
			arg_is_pointer_param := arg_node.kind == .ident && (g.current_param_type(arg_node.value) or {
				types.Type(types.void_)
			}) is types.Pointer
			arg_is_pointer_global := arg_node.kind == .ident && (g.global_type_for_ident(arg_node.value) or {
				types.Type(types.void_)
			}) is types.Pointer
			value_local_mut_receiver := arg_idx == 0 && (g.method_receiver_is_mut(fn_name)
				|| g.method_receiver_is_mut(g.direct_call_name(fn_name))) && arg_node.kind == .ident
				&& !g.local_storage_is_pointer(arg_node.value) && !arg_is_pointer_param
				&& !arg_is_pointer_global && arg_type !is types.Pointer
			if arg_is_shared_local && !arg_param_is_shared
				&& !g.c_string_pointer_arg(arg_node, param_types[arg_idx]) {
				needs_addr = true
			} else if (arg_type !is types.Pointer || value_local_mut_receiver)
				&& !g.c_string_pointer_arg(arg_node, param_types[arg_idx]) {
				needs_addr = !(arg_node.kind == .ident
					&& (g.local_storage_is_pointer(arg_node.value) || arg_is_pointer_global))
			}
		}
		is_rvalue := arg_node.kind == .call
			|| (arg_node.kind == .index && arg_node.value == 'range')
			|| g.arg_is_const_ident(arg_node)
		if needs_addr && g.arg_is_const_ident(arg_node) {
			pt := param_types[arg_idx]
			ct := g.tc.c_type(types.unwrap_pointer(pt))
			g.write('(${ct}[]){')
			g.gen_expr_with_expected_type(arg_id, types.unwrap_pointer(pt))
			g.write('}')
		} else if needs_addr && is_rvalue {
			pt := param_types[arg_idx]
			ct := g.tc.c_type(types.unwrap_pointer(pt))
			g.write('&((${ct}[]){')
			g.gen_expr_with_expected_type(arg_id, types.unwrap_pointer(pt))
			g.write('})[0]')
		} else if needs_addr && g.gen_mut_sum_lvalue_arg(arg_id, param_types[arg_idx]) {
			// handled
		} else {
			if arg_idx < typed_param_count {
				if param_types[arg_idx] !is types.Pointer {
					if child_id := g.addressed_byvalue_arg(arg_node) {
						g.gen_expr_with_expected_type(child_id, param_types[arg_idx])
						continue
					}
				}
				if child_id := g.addressed_rvalue_arg(arg_node) {
					pt := param_types[arg_idx]
					if g.gen_addressed_rvalue_arg(child_id, pt) {
						continue
					}
				}
			}
			if needs_addr {
				g.write('&')
			}
			emitted_variant := !needs_addr && arg_idx < typed_param_count
				&& g.gen_sum_variant_arg(arg_id, param_types[arg_idx])
			if !emitted_variant {
				if arg_idx < typed_param_count
					&& g.gen_optional_arg_with_abi(arg_id, param_types[arg_idx], concrete_optional_args) {
					// handled
				} else if arg_idx < typed_param_count
					&& g.gen_pointer_backed_param_arg(arg_id, param_types[arg_idx]) {
					// handled
				} else if arg_idx < typed_param_count && param_types[arg_idx] is types.Struct
					&& arg_node.kind == .struct_init {
					g.gen_struct_init(arg_node)
				} else if arg_idx < typed_param_count {
					g.gen_expr_with_expected_type(arg_id, param_types[arg_idx])
				} else {
					g.gen_expr(arg_id)
				}
			}
		}
		if variadic_idx >= 0 && num_args == variadic_idx {
			if node.children_count > start {
				g.write(', ')
			}
			variadic_type := param_types[variadic_idx]
			if variadic_type is types.Array {
				c_elem := g.tc.c_type(variadic_type.elem_type)
				g.write('new_array_from_c_array(0, 0, sizeof(${c_elem}), (${c_elem}[]){0})')
			}
		}
	}
	num_provided := node.children_count - start
	if !is_c_call && num_provided < typed_param_count {
		for i in num_provided .. typed_param_count {
			if num_provided > 0 || i > num_provided {
				g.write(', ')
			}
			g.gen_default_value_for_type(param_types[i])
		}
	}
}

fn (mut g FlatGen) gen_transformed_method_ident_call(id flat.NodeId, node flat.Node, fn_node flat.Node) bool {
	if fn_node.kind != .ident || node.children_count < 2 || !fn_node.value.contains('.') {
		return false
	}
	receiver_id := g.a.child(&node, 1)
	emitted_name := g.direct_call_name_for_call_node(id, node, fn_node.value)
	if !g.mut_receiver_arg_wants_addr(fn_node.value, receiver_id)
		&& !g.mut_receiver_arg_wants_addr(emitted_name, receiver_id) {
		return false
	}
	g.write(emitted_name)
	g.write('(&')
	g.gen_expr(receiver_id)
	mut params := g.param_types_for(emitted_name, emitted_name)
	if params.len == 0 {
		params = g.param_types_for(fn_node.value, fn_node.value.all_after_last('.'))
	}
	for i in 2 .. node.children_count {
		g.write(', ')
		arg_id := g.a.child(&node, i)
		param_idx := i - 1
		if param_idx < params.len {
			g.gen_arg_for_expected_type(arg_id, params[param_idx])
		} else {
			g.gen_expr(arg_id)
		}
	}
	g.write(')')
	return true
}

fn (mut g FlatGen) gen_ierror_str_arg(fn_name string, callee_name string, arg_idx int, arg_id flat.NodeId) bool {
	if arg_idx != 0 {
		return false
	}
	arg_type := g.usable_expr_type(arg_id)
	clean_type := types.unwrap_pointer(arg_type)
	if !g.is_ierror_type_name(clean_type.name()) {
		return false
	}
	is_ierror_str := g.cname(fn_name) == 'IError__str' || g.cname(callee_name) == 'IError__str'
		|| fn_name == 'IError.str' || callee_name == 'IError.str'
	if !is_ierror_str && fn_name != 'str' {
		return false
	}
	arg_node := g.a.nodes[int(arg_id)]
	if arg_node.kind == .prefix && arg_node.op == .amp {
		g.gen_expr(g.a.child(&arg_node, 0))
		return true
	}
	if arg_type is types.Pointer {
		g.write('*')
	}
	g.gen_expr(arg_id)
	return true
}

fn variadic_elem_is_voidptr(typ types.Type) bool {
	if typ is types.Pointer {
		return typ.base_type is types.Void
	}
	return false
}

fn (mut g FlatGen) gen_voidptr_variadic_arg(arg_id flat.NodeId) {
	actual := g.tc.resolve_type(arg_id)
	if voidptr_variadic_type_passes_direct(actual) {
		g.write('(voidptr)(')
		g.gen_expr_with_expected_type(arg_id, actual)
		g.write(')')
		return
	}
	storage_ct := g.voidptr_variadic_storage_c_type(actual)
	g.write('(voidptr)&((${storage_ct}[]){')
	g.gen_expr_with_expected_type(arg_id, actual)
	g.write('}[0])')
}

fn voidptr_variadic_type_passes_direct(typ types.Type) bool {
	if typ is types.Alias {
		return voidptr_variadic_type_passes_direct(typ.base_type)
	}
	return typ is types.Pointer || typ is types.Nil
}

fn (g &FlatGen) voidptr_variadic_storage_c_type(actual types.Type) string {
	mut clean := actual
	for _ in 0 .. 8 {
		if clean is types.Alias {
			clean = clean.base_type
			continue
		}
		break
	}
	if clean is types.Char {
		return 'int'
	}
	if clean is types.Primitive {
		if clean.props.has(.integer) && clean.size < 32 {
			return 'int'
		}
		if clean.props.has(.float) && clean.size == 32 {
			return 'double'
		}
	}
	return g.tc.c_type(clean)
}

fn raw_sizeof_arg_value(value string) ?string {
	clean := trimmed_space(value)
	if !clean.starts_with('sizeof(') || !clean.ends_with(')') {
		return none
	}
	return clean['sizeof('.len..clean.len - 1].trim_space()
}

fn raw_sizeof_needs_normalization(value string) bool {
	return value.starts_with('fn_ptr:') || value.starts_with('Array_fixed_')
}

// is_flag_enum_method reports whether is flag enum method applies in c.
fn (g &FlatGen) is_flag_enum_method(fn_node &flat.Node) bool {
	if fn_node.kind != .selector {
		return false
	}
	method := fn_node.value
	if method !in ['has', 'all', 'set', 'clear', 'toggle', 'set_all', 'clear_all', 'is_empty'] {
		return false
	}
	base_type := g.tc.resolve_type(g.a.child(fn_node, 0))
	clean := types.unwrap_pointer(base_type)
	if clean is types.Enum {
		return clean.is_flag || clean.name in g.tc.flag_enums
	} else if clean is types.Primitive {
		return clean.props.has(.integer)
	} else if clean is types.Unknown {
		return true
	}
	return false
}

// gen_flag_enum_call emits flag enum call output for c.
fn (mut g FlatGen) gen_flag_enum_call(node flat.Node) {
	fn_node := g.a.child_node(&node, 0)
	method := fn_node.value
	base_id := g.a.child(fn_node, 0)
	base_type := types.unwrap_pointer(g.tc.resolve_type(base_id))
	match method {
		'has' {
			g.write('((')
			g.gen_expr(base_id)
			g.write(' & ')
			if node.children_count > 1 {
				g.gen_flag_enum_arg(g.a.child(&node, 1), base_type)
			}
			g.write(') != 0)')
		}
		'all' {
			g.write('((')
			g.gen_expr(base_id)
			g.write(' & (')
			if node.children_count > 1 {
				g.gen_flag_enum_arg(g.a.child(&node, 1), base_type)
			}
			g.write(')) == (')
			if node.children_count > 1 {
				g.gen_flag_enum_arg(g.a.child(&node, 1), base_type)
			}
			g.write('))')
		}
		'set' {
			g.gen_expr(base_id)
			g.write(' |= ')
			if node.children_count > 1 {
				g.gen_flag_enum_arg(g.a.child(&node, 1), base_type)
			}
		}
		'clear' {
			g.gen_expr(base_id)
			g.write(' &= ~(')
			if node.children_count > 1 {
				g.gen_flag_enum_arg(g.a.child(&node, 1), base_type)
			}
			g.write(')')
		}
		'toggle' {
			g.gen_expr(base_id)
			g.write(' ^= ')
			if node.children_count > 1 {
				g.gen_flag_enum_arg(g.a.child(&node, 1), base_type)
			}
		}
		'set_all' {
			g.gen_expr(base_id)
			g.write(' = ${g.flag_enum_mask_expr(base_type.name())}')
		}
		'clear_all' {
			g.gen_expr(base_id)
			g.write(' = 0')
		}
		'is_empty' {
			g.write('(')
			g.gen_expr(base_id)
			g.write(' == 0)')
		}
		else {}
	}
}

fn (mut g FlatGen) gen_flag_enum_from_call(fn_node flat.Node, node flat.Node) bool {
	if fn_node.kind != .selector || fn_node.value != 'from' || fn_node.children_count == 0
		|| node.children_count < 2 {
		return false
	}
	base := g.a.child_node(&fn_node, 0)
	if base.kind != .ident {
		return false
	}
	mut enum_name := ''
	if base.value in g.tc.flag_enums {
		enum_name = base.value
	} else {
		qbase := g.tc.qualify_name(base.value)
		if qbase in g.tc.flag_enums {
			enum_name = qbase
		}
	}
	if enum_name.len == 0 {
		return false
	}
	enum_info := types.Enum{
		name:    enum_name
		is_flag: true
	}
	enum_type := types.Type(enum_info)
	ct := g.optional_type_name(types.Type(types.OptionType{
		base_type: enum_type
	}))
	value_ct := g.enum_value_c_type(enum_info)
	storage_ct := g.enum_storage_c_type(enum_info)
	mask := g.flag_enum_mask_expr(enum_name)
	arg := g.expr_to_string(g.a.child(&node, 1))
	value_tmp := g.tmp_name()
	ok_tmp := g.tmp_name()
	g.write('({ u64 ${value_tmp} = (u64)(${arg}); bool ${ok_tmp} = ((${value_tmp} & ~((u64)${mask})) == 0); (${ct}){.ok = ${ok_tmp}, .value = (${value_ct})(${ok_tmp} ? (${storage_ct})${value_tmp} : (${storage_ct})0)}; })')
	return true
}

fn (g &FlatGen) flag_enum_mask_expr(enum_name string) string {
	if _ := g.enum_backing_info(enum_name) {
		fields := g.enum_fields_for_type(enum_name) or { return '0' }
		mut parts := []string{cap: fields.len}
		for field in fields {
			if expr := g.enum_value_expr_for_type(enum_name, field) {
				parts << expr
			}
		}
		if parts.len == 0 {
			return '0'
		}
		return '(${parts.join(' | ')})'
	}
	return '${g.flag_enum_mask(enum_name)}'
}

fn (g &FlatGen) flag_enum_mask(enum_name string) int {
	mut mask := 0
	prefix := '${enum_name}.'
	for key, value in g.enum_vals {
		if key.starts_with(prefix) {
			mask |= value
		}
	}
	return mask
}

// gen_flag_enum_arg emits flag enum arg output for c.
fn (mut g FlatGen) gen_flag_enum_arg(arg_id flat.NodeId, base_type types.Type) {
	if base_type is types.Enum {
		g.gen_expr_with_expected_type(arg_id, base_type)
	} else {
		g.gen_expr(arg_id)
	}
}

// is_generic_type reports whether is generic type applies in c.
fn is_generic_type(typ string) bool {
	t := typ.trim_left('&?!')
	return t.len == 1 && t[0] >= `A` && t[0] <= `Z`
}

// has_generic_params reports whether has generic params applies in c.
fn (g &FlatGen) has_generic_params(node flat.Node) bool {
	for i in 0 .. node.children_count {
		child := g.a.child_node(&node, i)
		if child.kind == .param && is_generic_type(child.typ) {
			return true
		}
	}
	return is_generic_type(node.typ)
}

fn (g &FlatGen) addressed_rvalue_arg(arg_node flat.Node) ?flat.NodeId {
	if arg_node.kind != .prefix || arg_node.op != .amp || arg_node.children_count == 0 {
		return none
	}
	child_id := g.a.child(&arg_node, 0)
	child := g.a.nodes[int(child_id)]
	if child.kind == .call || (child.kind == .index && child.value == 'range') {
		return child_id
	}
	return none
}

fn (g &FlatGen) addressed_byvalue_arg(arg_node flat.Node) ?flat.NodeId {
	if arg_node.kind != .prefix || arg_node.op != .amp || arg_node.children_count == 0 {
		return none
	}
	child_id := g.a.child(&arg_node, 0)
	child := g.a.nodes[int(child_id)]
	if child.kind in [.struct_init, .cast_expr, .call]
		|| (child.kind == .index && child.value == 'range') {
		return child_id
	}
	return none
}

fn (mut g FlatGen) gen_addressed_byvalue_arg(arg_node flat.Node, expected types.Type) bool {
	child_id := g.addressed_byvalue_arg(arg_node) or { return false }
	child := g.a.nodes[int(child_id)]
	if child.kind == .struct_init {
		g.gen_struct_init(child)
	} else {
		g.gen_expr_with_expected_type(child_id, expected)
	}
	return true
}

fn (mut g FlatGen) gen_addressed_rvalue_arg(child_id flat.NodeId, pt types.Type) bool {
	if pt !is types.Pointer {
		return false
	}
	if g.c_typedef_nil_call(child_id) {
		g.write('NULL')
		return true
	}
	ct := g.tc.c_type(types.unwrap_pointer(pt))
	g.write('&((${ct}[]){')
	g.gen_expr_with_expected_type(child_id, types.unwrap_pointer(pt))
	g.write('})[0]')
	return true
}

fn (g &FlatGen) c_typedef_nil_call(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= g.a.nodes.len {
		return false
	}
	node := g.a.nodes[int(id)]
	if node.kind != .call || node.children_count != 2 || g.c_typedef_cast_call_name(node).len == 0 {
		return false
	}
	arg := g.a.child_node(&node, 1)
	return arg.kind == .nil_literal
}

fn (g &FlatGen) spread_arg_child(arg_node flat.Node) ?flat.NodeId {
	if arg_node.kind == .prefix && arg_node.value == '...' && arg_node.children_count > 0 {
		return g.a.child(&arg_node, 0)
	}
	return none
}

fn (g &FlatGen) arg_is_const_ident(arg_node flat.Node) bool {
	if arg_node.kind != .ident || arg_node.value.len == 0 {
		return false
	}
	looked_up := g.tc.cur_scope.lookup(arg_node.value) or { types.Type(types.void_) }
	if looked_up !is types.Void {
		return false
	}
	if g.const_ref_name(arg_node.value).len > 0 {
		return true
	}
	if _ := g.const_ident_type(arg_node.value) {
		return true
	}
	return false
}

// find_prim_method resolves find prim method information for c.
fn (g &FlatGen) find_prim_method(method string) string {
	if 'u8.${method}' in g.tc.fn_param_types {
		return g.cname('u8.${method}')
	}
	if 'int.${method}' in g.tc.fn_param_types {
		return g.cname('int.${method}')
	}
	if 'i64.${method}' in g.tc.fn_param_types {
		return g.cname('i64.${method}')
	}
	if 'u32.${method}' in g.tc.fn_param_types {
		return g.cname('u32.${method}')
	}
	if 'u64.${method}' in g.tc.fn_param_types {
		return g.cname('u64.${method}')
	}
	return ''
}

// find_alias_method converts find alias method data for c.
fn (g &FlatGen) find_alias_method(target string, method string) ?string {
	mut fallback := ''
	for alias, alias_target in g.tc.type_aliases {
		if alias_target != target {
			continue
		}
		alias_method := '${alias}.${method}'
		if alias_method !in g.tc.fn_param_types {
			if alias.contains('.') {
				short_method := '${alias.all_after_last('.')}.${method}'
				if short_method in g.tc.fn_param_types {
					return alias_method
				}
			}
			continue
		}
		if alias.contains('.') {
			return alias_method
		}
		if fallback.len == 0 {
			fallback = alias_method
		}
	}
	if fallback.len > 0 {
		return fallback
	}
	return none
}

// gen_sum_variant_arg emits sum variant arg output for c.
fn (mut g FlatGen) gen_sum_variant_arg(arg_id flat.NodeId, expected types.Type) bool {
	actual0 := types.unwrap_pointer(g.tc.resolve_type(arg_id))
	mut actual := actual0
	if actual0 is types.Alias {
		actual = actual0.base_type
	}
	expected0 := expected
	mut expected_type := expected0
	if expected0 is types.Alias {
		expected_type = expected0.base_type
	}
	if expected_type is types.SumType {
		return false
	}
	if actual !is types.SumType {
		return false
	}
	sum_type := actual as types.SumType
	sum_name := sum_type.name
	variant := g.resolve_variant(sum_name, expected_type.name())
	variants := g.tc.sum_types[sum_name] or { return false }
	if variant !in variants {
		return false
	}
	is_ptr_arg := g.tc.resolve_type(arg_id) is types.Pointer
	is_ref_variant := g.variant_references_sum(variant, sum_name)
	if is_ref_variant {
		g.write('(*')
	}
	g.gen_expr(arg_id)
	if is_ptr_arg {
		g.write('->')
	} else {
		g.write('.')
	}
	g.write(g.sum_field_name(variant))
	if is_ref_variant {
		g.write(')')
	}
	return true
}

// forward_decls supports forward decls handling for FlatGen.
fn (mut g FlatGen) forward_decls() {
	mut forwarded := map[string]bool{}
	for item in g.ensure_fn_gen_items() {
		node := g.a.nodes[int(item.node_id)]
		if is_main_fn_in_main_module(item.module, node.value) && g.test_files.len == 0 {
			continue
		}
		qfn := item.c_name
		if forwarded[qfn] {
			continue
		}
		forwarded[qfn] = true
		g.tc.cur_file = item.file
		g.tc.cur_module = item.module
		ret_type := g.fn_node_return_type(node, item.module)
		g.write(g.fn_return_type_name(ret_type))
		g.write(' ')
		g.write(qfn)
		g.write('(')
		g.write_fn_node_params(node)
		g.writeln(');')
		if export_name := g.export_fn_name_in_module(item.module, node.value) {
			if !forwarded[export_name] {
				forwarded[export_name] = true
				g.write(g.fn_return_type_name(ret_type))
				g.write(' ')
				g.write(export_name)
				g.write('(')
				g.write_fn_node_params(node)
				g.writeln(');')
			}
		}
	}
	g.writeln('')
}

fn (mut g FlatGen) cached_header_forward_decls() {
	mut cur_file := ''
	mut cur_module := ''
	mut forwarded := map[string]bool{}
	for node in g.a.nodes {
		if node.kind == .file {
			cur_file = node.value
			cur_module = ''
			continue
		}
		if node.kind == .module_decl {
			cur_module = node.value
			continue
		}
		if node.kind != .fn_decl || !node.is_mut || !cur_file.ends_with('.vh') {
			continue
		}
		qfn := g.fn_c_name_in_module(cur_module, node.value)
		if forwarded[qfn] {
			continue
		}
		forwarded[qfn] = true
		g.tc.cur_file = cur_file
		g.tc.cur_module = cur_module
		ret_type := g.fn_node_return_type(node, cur_module)
		g.write(g.fn_return_type_name(ret_type))
		g.write(' ')
		g.write(qfn)
		g.write('(')
		g.write_fn_node_params(node)
		g.writeln(');')
	}
	if forwarded.len > 0 {
		g.writeln('')
	}
}

fn (mut g FlatGen) c_extern_forward_decls() {
	mut cur_module := ''
	mut cur_file := ''
	mut decls := map[string]string{}
	mut names := []string{}
	referenced_c_externs := g.c_extern_referenced_symbols()
	// file/module/c_fn_decl nodes only occur at the top level: iterate the
	// checker's top-level index for the range it covers, then scan only the
	// transform-appended tail.
	use_idx := !isnil(g.tc) && g.tc.top_level_idx.len > 0
	idx_count := if use_idx { g.tc.top_level_idx.len } else { 0 }
	tail_start := if use_idx { g.tc.top_level_idx_nodes_len } else { 0 }
	total := idx_count + (g.a.nodes.len - tail_start)
	for k in 0 .. total {
		i := if k < idx_count { g.tc.top_level_idx[k] } else { tail_start + (k - idx_count) }
		node := g.a.nodes[i]
		kind_id := node_kind_id(node)
		if kind_id == 77 {
			cur_file = node.value
			cur_module = ''
			g.tc.cur_file = cur_file
			g.tc.cur_module = cur_module
			continue
		}
		if kind_id == 73 {
			cur_module = node.value
			g.tc.cur_file = cur_file
			g.tc.cur_module = cur_module
			continue
		}
		if kind_id != 76 {
			continue
		}
		raw_name := if node.value.starts_with('C.') { node.value } else { 'C.${node.value}' }
		raw_cfn := g.cname(raw_name)
		cfn := c_winapi_wide_export_name(raw_cfn)
		shared_runtime_extern := g.needs_shared_runtime && cfn in c_shared_runtime_extern_symbols
		if g.has_used_fn_filter() && !(g.spawn_wrapper_defs.len > 0
			&& cfn in c_spawn_runtime_extern_symbols) && !shared_runtime_extern
			&& !g.used_fn_contains(raw_name) && !g.used_fn_contains(raw_cfn)
			&& !g.used_fn_contains(cfn) && !referenced_c_externs[raw_name]
			&& !referenced_c_externs[raw_cfn] && !referenced_c_externs[cfn] {
			continue
		}
		if !g.should_emit_c_extern_decl(cfn) {
			continue
		}
		if cfn == 'syscall' {
			g.libc_compat_fns[c_libc_compat_syscall_decl_key] = true
		}
		g.tc.cur_file = cur_file
		g.tc.cur_module = cur_module
		if cfn !in decls {
			names << cfn
		}
		decls[cfn] = g.c_extern_decl_line(node, cfn)
	}
	names.sort()
	for name in names {
		if name == 'task_info' || name == 'mach_task_self' {
			g.writeln('#ifndef __APPLE__')
			g.writeln(decls[name])
			g.writeln('#endif')
		} else {
			g.writeln(decls[name])
		}
	}
	if names.len > 0 {
		g.writeln('')
	}
}

fn (mut g FlatGen) c_extern_referenced_symbols() map[string]bool {
	if g.c_extern_refs_ready {
		return g.c_extern_refs
	}
	mut refs := map[string]bool{}
	for item in g.ensure_fn_gen_items() {
		g.collect_c_extern_referenced_symbols_from_node(item.node_id, mut refs)
	}
	if g.test_files.len == 0 && !g.has_entry_main() {
		for stmt in g.top_level_stmts() {
			g.collect_c_extern_referenced_symbols_from_node(stmt.id, mut refs)
		}
	}
	g.c_extern_refs = refs.move()
	g.c_extern_refs_ready = true
	return g.c_extern_refs
}

fn (g &FlatGen) collect_c_extern_referenced_symbols_from_node(id flat.NodeId, mut refs map[string]bool) {
	if int(id) < 0 || int(id) >= g.a.nodes.len {
		return
	}
	node := g.a.node(id)
	g.collect_c_extern_ref_from_node_into(node, mut refs)
	for i in 0 .. node.children_count {
		g.collect_c_extern_referenced_symbols_from_node(g.a.child(node, i), mut refs)
	}
}

fn (mut g FlatGen) collect_c_extern_ref_from_node(node flat.Node) {
	g.collect_c_extern_ref_from_node_into(node, mut g.c_extern_refs)
}

fn (g &FlatGen) collect_c_extern_ref_from_node_into(node flat.Node, mut refs map[string]bool) {
	if node.kind == .selector && node.children_count > 0 && node.value.len > 0 {
		base_id := g.a.child(node, 0)
		if int(base_id) >= 0 {
			base := g.a.node(base_id)
			if base.kind == .ident && base.value == 'C' {
				raw_name := 'C.${node.value}'
				raw_cfn := g.cname(raw_name)
				refs[raw_name] = true
				refs[raw_cfn] = true
				refs[c_winapi_wide_export_name(raw_cfn)] = true
			}
		}
	}
}

fn (mut g FlatGen) preseed_c_extern_fn_ptr_types() {
	for i in 0 .. g.a.nodes.len {
		node := g.a.nodes[i]
		if node_kind_id(node) != 76 {
			continue
		}
		ret_type := g.tc.parse_type(node.typ)
		g.preseed_fn_ptr_type(ret_type)
		for j in 0 .. node.children_count {
			param_id := g.a.child(&node, j)
			p := g.a.node(param_id)
			if p.kind != .param {
				continue
			}
			raw_typ := if p.typ.len > 0 { p.typ } else { p.value }
			if raw_typ.len == 0 || raw_typ.starts_with('...') {
				continue
			}
			g.preseed_fn_ptr_type(g.tc.parse_type(raw_typ))
		}
	}
}

const c_spawn_runtime_extern_symbols = {
	'pthread_attr_destroy':      true
	'pthread_attr_init':         true
	'pthread_attr_setstacksize': true
	'pthread_create':            true
	'pthread_join':              true
}

const c_shared_runtime_extern_symbols = {
	'pthread_rwlock_rdlock':         true
	'pthread_rwlock_wrlock':         true
	'pthread_rwlock_unlock':         true
	'pthread_rwlockattr_init':       true
	'pthread_rwlockattr_setkind_np': true
	'pthread_rwlock_init':           true
	'pthread_rwlockattr_destroy':    true
}

fn (g &FlatGen) should_emit_c_extern_decl(cfn string) bool {
	if cfn.contains('.') {
		return false
	}
	if g.cache_split && cfn in c_cache_system_header_declared_fns {
		return false
	}
	if cfn in c_preamble_declared_extern_symbols {
		if g.needs_shared_runtime && cfn in c_shared_runtime_extern_symbols {
			return true
		}
		return false
	}
	if cfn in c_libc_compat_extern_symbols {
		return false
	}
	if cfn in c_static_helper_symbols {
		return false
	}
	if cfn in g.inlined_c_fns {
		return false
	}
	if cfn in g.inlined_c_declared_fns {
		return false
	}
	return true
}

const c_preamble_declared_extern_symbols = {
	'_exit':                         true
	'_dyld_get_image_name':          true
	'__errno':                       true
	'__errno_location':              true
	'__error':                       true
	'_errno':                        true
	'abort':                         true
	'access':                        true
	'atexit':                        true
	'ceil':                          true
	'ceilf':                         true
	'close':                         true
	'cos':                           true
	'dup2':                          true
	'execlp':                        true
	'execvp':                        true
	'fabs':                          true
	'fcntl':                         true
	'floor':                         true
	'floorf':                        true
	'fmod':                          true
	'fork':                          true
	'getenv':                        true
	'ldexp':                         true
	'memcmp':                        true
	'memcpy':                        true
	'memmove':                       true
	'memset':                        true
	'open':                          true
	'pipe':                          true
	'pow':                           true
	'pthread_attr_destroy':          true
	'pthread_attr_init':             true
	'pthread_cond_broadcast':        true
	'pthread_cond_destroy':          true
	'pthread_cond_init':             true
	'pthread_cond_signal':           true
	'pthread_cond_wait':             true
	'pthread_create':                true
	'pthread_detach':                true
	'pthread_join':                  true
	'pthread_mutex_destroy':         true
	'pthread_mutex_init':            true
	'pthread_mutex_lock':            true
	'pthread_mutex_unlock':          true
	'malloc':                        true
	'calloc':                        true
	'realloc':                       true
	'free':                          true
	'clock':                         true
	'fprintf':                       true
	'fflush':                        true
	'qsort_r':                       true
	'mktime':                        true
	'localtime':                     true
	'utime':                         true
	'stat':                          true
	'fopen':                         true
	'freopen':                       true
	'fclose':                        true
	'fread':                         true
	'fwrite':                        true
	'fseek':                         true
	'ftell':                         true
	'remove':                        true
	'rename':                        true
	'time':                          true
	'fileno':                        true
	'ftruncate':                     true
	'pthread_rwlock_destroy':        true
	'pthread_rwlock_init':           true
	'pthread_rwlock_rdlock':         true
	'pthread_rwlock_tryrdlock':      true
	'pthread_rwlock_trywrlock':      true
	'pthread_rwlock_unlock':         true
	'pthread_rwlock_wrlock':         true
	'pthread_rwlockattr_destroy':    true
	'pthread_rwlockattr_init':       true
	'pthread_rwlockattr_setkind_np': true
	'read':                          true
	'realpath':                      true
	'setenv':                        true
	'signal':                        true
	'snprintf':                      true
	'sqrt':                          true
	'strcmp':                        true
	'strlen':                        true
	'strncmp':                       true
	'strncpy':                       true
	'strrchr':                       true
	'strstr':                        true
}

const c_libc_compat_extern_symbols = {
	'gettid': true
}

const c_libc_compat_syscall_decl_key = 'syscall_decl'

const c_static_helper_symbols = {
	'EV_SET':                              true
	'FD_ISSET':                            true
	'FD_SET':                              true
	'FD_ZERO':                             true
	'WEXITSTATUS':                         true
	'WIFEXITED':                           true
	'WIFSIGNALED':                         true
	'WTERMSIG':                            true
	'access':                              true
	'atexit':                              true
	'atomic_compare_exchange_strong_byte': true
	'atomic_compare_exchange_strong_ptr':  true
	'atomic_compare_exchange_strong_u16':  true
	'atomic_compare_exchange_strong_u32':  true
	'atomic_compare_exchange_strong_u64':  true
	'atomic_compare_exchange_weak_byte':   true
	'atomic_compare_exchange_weak_ptr':    true
	'atomic_compare_exchange_weak_u16':    true
	'atomic_compare_exchange_weak_u32':    true
	'atomic_compare_exchange_weak_u64':    true
	'atomic_exchange_byte':                true
	'atomic_exchange_ptr':                 true
	'atomic_exchange_u16':                 true
	'atomic_exchange_u32':                 true
	'atomic_exchange_u64':                 true
	'atomic_fetch_add_byte':               true
	'atomic_fetch_add_ptr':                true
	'atomic_fetch_add_u16':                true
	'atomic_fetch_add_u32':                true
	'atomic_fetch_add_u64':                true
	'atomic_fetch_sub_byte':               true
	'atomic_fetch_sub_ptr':                true
	'atomic_fetch_sub_u16':                true
	'atomic_fetch_sub_u32':                true
	'atomic_fetch_sub_u64':                true
	'atomic_load_byte':                    true
	'atomic_load_ptr':                     true
	'atomic_load_u16':                     true
	'atomic_load_u32':                     true
	'atomic_load_u64':                     true
	'atomic_store_byte':                   true
	'atomic_store_ptr':                    true
	'atomic_store_u16':                    true
	'atomic_store_u32':                    true
	'atomic_store_u64':                    true
	'close':                               true
	'cpu_relax':                           true
	'dup2':                                true
	'execlp':                              true
	'execvp':                              true
	'fcntl':                               true
	'fork':                                true
	'getenv':                              true
	'open':                                true
	'pipe':                                true
	'posix_spawn':                         true
	'posix_spawnp':                        true
	'read':                                true
	'realpath':                            true
	'setenv':                              true
	'signal':                              true
	'snprintf':                            true
	'strrchr':                             true
	'strstr':                              true
	'v_filelock_lock':                     true
	'v_filelock_unlock':                   true
	'v_os_exec_capture_start':             true
	'v_os_execute_capture_start':          true
	'vschannel_cleanup':                   true
	'vschannel_init':                      true
	'v_prealloc_atomic_add_i32':           true
	'v_prealloc_atomic_cas_i32':           true
	'v_prealloc_atomic_load_i32':          true
	'v_prealloc_atomic_store_i32':         true
	'v_signal_with_handler_cast':          true
	'wyhash':                              true
	'wyhash64':                            true
	'_exit':                               true
	'_wymix':                              true
	'_vcleanup':                           true
	'_vinit':                              true
}

fn (mut g FlatGen) c_extern_decl_line(node flat.Node, cfn string) string {
	mut sb := strings.new_builder(96)
	ret_type := g.tc.parse_type(node.typ)
	sb.write_string(g.fn_return_type_name(ret_type))
	sb.write_string(' ')
	call_conv := c_extern_calling_convention(cfn)
	if call_conv.len > 0 {
		sb.write_string(call_conv)
		sb.write_u8(` `)
	}
	sb.write_string(cfn)
	sb.write_u8(`(`)
	sb.write_string(g.c_extern_decl_params(node))
	sb.write_string(');')
	return sb.str()
}

fn c_extern_calling_convention(cfn string) string {
	if cfn in c_winapi_extern_symbols {
		return 'WINAPI'
	}
	return ''
}

fn c_winapi_wide_export_name(cfn string) string {
	match cfn {
		'CopyFile' { return 'CopyFileW' }
		'CreateDirectory' { return 'CreateDirectoryW' }
		'CreateFile' { return 'CreateFileW' }
		'CreateWindowEx' { return 'CreateWindowExW' }
		'DefWindowProc' { return 'DefWindowProcW' }
		'FindFirstFile' { return 'FindFirstFileW' }
		'FindNextFile' { return 'FindNextFileW' }
		'GetCommandLine' { return 'GetCommandLineW' }
		'GetFullPathName' { return 'GetFullPathNameW' }
		'GetLongPathName' { return 'GetLongPathNameW' }
		'GetModuleFileName' { return 'GetModuleFileNameW' }
		'LoadLibrary' { return 'LoadLibraryW' }
		'ReadConsole' { return 'ReadConsoleW' }
		'RegOpenKeyEx' { return 'RegOpenKeyExW' }
		'RegQueryValueEx' { return 'RegQueryValueExW' }
		'RegSetValueEx' { return 'RegSetValueExW' }
		'RegisterClassEx' { return 'RegisterClassExW' }
		'RemoveDirectory' { return 'RemoveDirectoryW' }
		'SendMessageTimeout' { return 'SendMessageTimeoutW' }
		'SetConsoleTitle' { return 'SetConsoleTitleW' }
		'WriteConsole' { return 'WriteConsoleW' }
		else { return cfn }
	}
}

const c_winapi_extern_symbols = {
	'AddVectoredExceptionHandler':   true
	'CaptureStackBackTrace':         true
	'CloseClipboard':                true
	'CloseHandle':                   true
	'CopyFile':                      true
	'CopyFileW':                     true
	'CreateDirectory':               true
	'CreateDirectoryW':              true
	'CreateFile':                    true
	'CreateFileW':                   true
	'CreateHardLinkW':               true
	'CreatePipe':                    true
	'CreateProcessW':                true
	'CreateSymbolicLinkW':           true
	'CreateWindowEx':                true
	'CreateWindowExW':               true
	'DefWindowProc':                 true
	'DefWindowProcW':                true
	'DeleteFileW':                   true
	'DestroyWindow':                 true
	'EmptyClipboard':                true
	'ExpandEnvironmentStringsW':     true
	'FindClose':                     true
	'FindFirstFile':                 true
	'FindFirstFileW':                true
	'FindNextFile':                  true
	'FindNextFileW':                 true
	'FormatMessageW':                true
	'FreeEnvironmentStringsW':       true
	'GenerateConsoleCtrlEvent':      true
	'GetClipboardData':              true
	'GetClipboardOwner':             true
	'GetCommandLine':                true
	'GetCommandLineW':               true
	'GetComputerNameW':              true
	'GetConsoleMode':                true
	'GetConsoleScreenBufferInfo':    true
	'GetCurrentDirectoryW':          true
	'GetCurrentProcess':             true
	'GetCurrentThreadId':            true
	'GetEnvironmentStringsW':        true
	'GetExitCodeProcess':            true
	'GetFinalPathNameByHandleW':     true
	'GetFileAttributesW':            true
	'GetFileSizeEx':                 true
	'GetFileType':                   true
	'GetFullPathName':               true
	'GetFullPathNameW':              true
	'GetLastError':                  true
	'GetLongPathName':               true
	'GetLongPathNameW':              true
	'GetModuleFileName':             true
	'GetModuleFileNameW':            true
	'GetModuleHandleA':              true
	'GetNumberOfConsoleInputEvents': true
	'GetProcAddress':                true
	'GetProcessHeap':                true
	'GetShortPathNameW':             true
	'GetStdHandle':                  true
	'GetSystemTimeAsFileTime':       true
	'GetTempFileNameW':              true
	'GetTempPathW':                  true
	'GetTickCount':                  true
	'GetUserNameW':                  true
	'GlobalAlloc':                   true
	'GlobalFree':                    true
	'GlobalLock':                    true
	'GlobalUnlock':                  true
	'HeapAlloc':                     true
	'HeapFree':                      true
	'InitializeConditionVariable':   true
	'IsDebuggerPresent':             true
	'LoadLibrary':                   true
	'LoadLibraryW':                  true
	'LocalFree':                     true
	'MoveFileW':                     true
	'MultiByteToWideChar':           true
	'OpenClipboard':                 true
	'PeekNamedPipe':                 true
	'ReadConsole':                   true
	'ReadConsoleInput':              true
	'ReadConsoleW':                  true
	'ReadFile':                      true
	'RegCloseKey':                   true
	'RegOpenKeyEx':                  true
	'RegOpenKeyExW':                 true
	'RegQueryValueEx':               true
	'RegQueryValueExW':              true
	'RegSetValueEx':                 true
	'RegSetValueExW':                true
	'RegisterClassEx':               true
	'RegisterClassExW':              true
	'RemoveDirectory':               true
	'RemoveDirectoryW':              true
	'ScrollConsoleScreenBuffer':     true
	'SetClipboardData':              true
	'SetConsoleCursorPosition':      true
	'SetConsoleMode':                true
	'SetConsoleTitle':               true
	'SetConsoleTitleW':              true
	'SetCurrentDirectoryW':          true
	'SetEndOfFile':                  true
	'SetFileAttributesW':            true
	'SetFilePointerEx':              true
	'SetHandleInformation':          true
	'SetLastError':                  true
	'SetUnhandledExceptionFilter':   true
	'Sleep':                         true
	'SleepConditionVariableSRW':     true
	'SendMessageTimeout':            true
	'SendMessageTimeoutW':           true
	'SymCleanup':                    true
	'SymFromAddr':                   true
	'SymGetLineFromAddr64':          true
	'SymInitialize':                 true
	'SymSetOptions':                 true
	'TerminateProcess':              true
	'TlsAlloc':                      true
	'TlsFree':                       true
	'TlsGetValue':                   true
	'TlsSetValue':                   true
	'TryAcquireSRWLockExclusive':    true
	'TryAcquireSRWLockShared':       true
	'VirtualAlloc':                  true
	'VirtualFree':                   true
	'VirtualProtect':                true
	'WSAAddressToStringA':           true
	'WaitForSingleObject':           true
	'WakeConditionVariable':         true
	'WriteConsole':                  true
	'WriteConsoleW':                 true
	'WriteFile':                     true
}

fn (mut g FlatGen) c_extern_decl_params(node flat.Node) string {
	if node.children_count == 0 {
		return 'void'
	}
	mut parts := []string{}
	for i in 0 .. node.children_count {
		param_id := g.a.child(&node, i)
		p := g.a.node(param_id)
		if p.kind != .param {
			continue
		}
		raw_typ := if p.typ.len > 0 { p.typ } else { p.value }
		if raw_typ.len == 0 {
			continue
		}
		if raw_typ.starts_with('...') {
			if parts.len > 0 {
				parts << '...'
			}
			continue
		}
		pt := g.tc.parse_type(raw_typ)
		mut ct := g.c_extern_param_c_type(pt)
		if ct.starts_with('fn_ptr:') {
			ct = g.resolve_fn_ptr_type(ct)
		}
		if c_extern_param_needs_const_prefix(p.value, raw_typ, pt) {
			ct = 'const ${ct}'
		}
		if p.typ.len > 0 && p.value.len > 0 {
			param_name := if p.value == '_' { '_${parts.len}' } else { g.cname(p.value) }
			parts << '${ct} ${param_name}'
		} else {
			parts << ct
		}
	}
	if parts.len == 0 {
		return 'void'
	}
	return parts.join(', ')
}

fn (mut g FlatGen) c_extern_param_c_type(pt types.Type) string {
	if pt is types.OptionType || pt is types.ResultType {
		return g.optional_type_name(pt)
	}
	if pt is types.Pointer {
		base := pt.base_type
		if base is types.Struct && base.name.starts_with('C.') {
			base_ct := g.tc.c_type(base)
			if base_ct.starts_with('struct ') {
				return '${base_ct}*'
			}
		}
	}
	return g.tc.c_type(pt)
}

fn c_extern_param_needs_const_prefix(param_name string, raw_typ string, pt types.Type) bool {
	return param_name.starts_with('const_') && !trimmed_space(raw_typ).starts_with('mut ')
		&& pt is types.Pointer
}

fn (mut g FlatGen) insert_cur_implicit_veb_ctx_param(node flat.Node) {
	if !g.fn_needs_implicit_veb_ctx(node) {
		return
	}
	insert_idx := g.fn_implicit_veb_ctx_insert_index(node)
	ctx_type := g.implicit_veb_ctx_type()
	mut names := []string{cap: g.cur_param_names.len + 1}
	mut type_values := []types.Type{cap: g.cur_param_type_values.len + 1}
	for i, name in g.cur_param_names {
		if i == insert_idx {
			names << 'ctx'
			type_values << ctx_type
		}
		names << name
		type_values << g.cur_param_type_values[i]
	}
	if insert_idx >= g.cur_param_names.len {
		names << 'ctx'
		type_values << ctx_type
	}
	g.cur_param_names = names
	g.cur_param_type_values = type_values
	g.cur_param_types['ctx'] = ctx_type
	g.tc.cur_scope.insert('ctx', ctx_type)
}

fn (mut g FlatGen) fn_param_types_with_implicit_veb_ctx(node flat.Node, params []types.Type) []types.Type {
	if !g.fn_needs_implicit_veb_ctx(node) {
		return params
	}
	insert_idx := g.fn_implicit_veb_ctx_insert_index(node)
	ctx_type := g.implicit_veb_ctx_type()
	mut result := []types.Type{cap: params.len + 1}
	for i, param in params {
		if i == insert_idx {
			result << ctx_type
		}
		result << param
	}
	if insert_idx >= params.len {
		result << ctx_type
	}
	return result
}

fn (mut g FlatGen) fn_shared_params_with_implicit_veb_ctx(node flat.Node, flags []bool) []bool {
	if !g.fn_needs_implicit_veb_ctx(node) {
		return flags
	}
	insert_idx := g.fn_implicit_veb_ctx_insert_index(node)
	mut result := []bool{cap: flags.len + 1}
	for i, flag in flags {
		if i == insert_idx {
			result << false
		}
		result << flag
	}
	if insert_idx >= flags.len {
		result << false
	}
	return result
}

fn (g &FlatGen) fn_param_is_shared(fn_name string, idx int) bool {
	if !g.has_shared_params || idx < 0 || fn_name.len == 0 {
		return false
	}
	if flags := g.fn_shared_params_resolved[fn_name] {
		return idx < flags.len && flags[idx]
	}
	candidates := [
		fn_name,
		g.cname(fn_name),
		fn_name.all_after_last('.'),
		g.cname(fn_name.all_after_last('.')),
	]
	for candidate in candidates {
		flags := g.fn_decl_shared_params[candidate] or { continue }
		// The first present entry is authoritative: an exact-name (possibly
		// all-false) entry must stop the short-name fallback from matching an
		// unrelated declaration.
		return idx < flags.len && flags[idx]
	}
	return false
}

fn (g &FlatGen) fn_param_shared_exact(fn_name string, idx int) ?bool {
	if fn_name.len == 0 {
		return none
	}
	if flags := g.fn_shared_params_resolved[fn_name] {
		return idx < flags.len && flags[idx]
	}
	cname := g.cname(fn_name)
	if cname != fn_name {
		if flags := g.fn_shared_params_resolved[cname] {
			return idx < flags.len && flags[idx]
		}
	}
	return none
}

fn (g &FlatGen) fn_param_is_shared_for_call(idx int, name1 string, name2 string, name3 string, name4 string) bool {
	if !g.has_shared_params || idx < 0 {
		return false
	}
	if flag := g.fn_param_shared_exact(name1, idx) {
		return flag
	}
	if flag := g.fn_param_shared_exact(name2, idx) {
		return flag
	}
	if flag := g.fn_param_shared_exact(name3, idx) {
		return flag
	}
	if flag := g.fn_param_shared_exact(name4, idx) {
		return flag
	}
	return g.fn_param_is_shared(name1, idx) || g.fn_param_is_shared(name2, idx)
		|| g.fn_param_is_shared(name3, idx) || g.fn_param_is_shared(name4, idx)
}

fn (mut g FlatGen) precompute_shared_param_index() {
	if !g.has_shared_params {
		return
	}
	for name, flags in g.fn_decl_shared_params {
		// The name's own entry is authoritative (mirrors fn_param_is_shared's
		// first-present-candidate rule); merging short-name variants here
		// would smear another declaration's shared flags onto this one.
		// Store empty/all-false results too. Presence in this map is what turns
		// the very hot call-site query into one lookup.
		g.fn_shared_params_resolved[name] = flags.clone()
	}
}

fn (mut g FlatGen) gen_shared_local_receiver_arg(base_id flat.NodeId) bool {
	if int(base_id) < 0 || int(base_id) >= g.a.nodes.len {
		return false
	}
	base := g.a.nodes[int(base_id)]
	if base.kind == .paren && base.children_count > 0 {
		return g.gen_shared_local_receiver_arg(g.a.child(&base, 0))
	}
	if base.kind == .prefix && base.value == 'shared' && base.children_count > 0 {
		return g.gen_shared_local_receiver_arg(g.a.child(&base, 0))
	}
	if base.kind != .ident || !g.local_storage_is_shared(base.value) {
		return false
	}
	g.write(g.cname(base.value))
	return true
}

fn (mut g FlatGen) fn_needs_implicit_veb_ctx(node flat.Node) bool {
	return g.fn_returns_veb_result(node) && g.fn_has_receiver_param(node)
		&& !g.fn_receiver_type_is_context(node) && !g.fn_has_param(node, 'ctx')
		&& g.type_name_known_in_current_module('Context')
}

// is_implicit_veb_ctx_param reports whether a callee parameter is the hidden
// veb `Context` pointer that callers do not supply explicitly.
fn (g &FlatGen) is_implicit_veb_ctx_param(pt types.Type) bool {
	if pt is types.Pointer {
		return pt.base_type.name().all_after_last('.') == 'Context'
	}
	return false
}

// cur_scope_has_ctx reports whether the current function exposes a `ctx`
// variable (the implicit veb context) that can be forwarded to delegated calls.
fn (g &FlatGen) cur_scope_has_ctx() bool {
	if _ := g.tc.cur_scope.lookup('ctx') {
		return true
	}
	return false
}

fn (g &FlatGen) type_name_known_in_current_module(name string) bool {
	qname := g.tc.qualify_name(name)
	return qname in g.struct_decl_infos || qname in g.tc.type_aliases || qname in g.tc.enum_names
		|| qname in g.tc.sum_types || qname in g.tc.interface_names
}

fn (g &FlatGen) type_name_known(name string) bool {
	if types.is_builtin_type_name(name) || name in ['C', 'JS'] {
		return true
	}
	qname := g.tc.qualify_name(name)
	return name in g.struct_decl_infos || qname in g.struct_decl_infos || name in g.tc.type_aliases
		|| qname in g.tc.type_aliases || name in g.tc.structs || qname in g.tc.structs
		|| name in g.tc.enum_names || qname in g.tc.enum_names || name in g.tc.sum_types
		|| qname in g.tc.sum_types || name in g.tc.interface_names || qname in g.tc.interface_names
}

fn (mut g FlatGen) fn_returns_veb_result(node flat.Node) bool {
	if node.typ == 'veb.Result' {
		return true
	}
	ret := g.tc.parse_type(node.typ)
	return ret.name() == 'veb.Result'
}

fn (g &FlatGen) fn_has_param(node flat.Node, name string) bool {
	for i in 0 .. node.children_count {
		p := g.a.child_node(&node, i)
		if p.kind == .param && p.value == name {
			return true
		}
	}
	return false
}

fn (g &FlatGen) fn_implicit_veb_ctx_insert_index(node flat.Node) int {
	if g.fn_has_receiver_param(node) {
		return 1
	}
	return 0
}

fn (g &FlatGen) fn_has_receiver_param(node flat.Node) bool {
	if !node.value.contains('.') || node.children_count == 0 {
		return false
	}
	first := g.a.child_node(&node, 0)
	if first.kind != .param || first.typ.len == 0 {
		return false
	}
	receiver := node.value.all_before_last('.').all_after_last('.')
	param_type := first.typ.trim_left('&').all_after_last('.')
	return receiver == param_type
}

fn (g &FlatGen) fn_receiver_type_is_context(node flat.Node) bool {
	if !g.fn_has_receiver_param(node) {
		return false
	}
	first := g.a.child_node(&node, 0)
	return first.typ.trim_left('&').all_after_last('.') == 'Context'
}

fn (mut g FlatGen) implicit_veb_ctx_type() types.Type {
	return g.tc.parse_type('mut Context')
}

fn (mut g FlatGen) fn_node_return_type(node flat.Node, module_name string) types.Type {
	if rt := g.fn_node_return_type_from_signatures(node, module_name) {
		return rt
	}
	if info := g.generic_receiver_method_call_info(node.value) {
		return info.return_type
	}
	return g.tc.parse_type(node.typ)
}

fn (mut g FlatGen) fn_node_return_type_from_signatures(node flat.Node, module_name string) ?types.Type {
	dotted_name := dotted_fn_name_in_module(module_name, node.value)
	cname := g.fn_c_name_in_module(module_name, node.value)
	if module_name.len > 0 && module_name != 'main' && module_name != 'builtin' {
		if rt := g.tc.fn_ret_types[dotted_name] {
			return rt
		}
		c_dotted_name := g.cname(dotted_name)
		if c_dotted_name != dotted_name {
			if rt := g.tc.fn_ret_types[c_dotted_name] {
				return rt
			}
		}
		if rt := g.tc.fn_ret_types[cname] {
			return rt
		}
		if rt := g.tc.fn_ret_types[node.value] {
			return rt
		}
		c_value := g.cname(node.value)
		if c_value != node.value {
			if rt := g.tc.fn_ret_types[c_value] {
				return rt
			}
		}
		return none
	}
	if rt := g.tc.fn_ret_types[node.value] {
		return rt
	}
	c_value := g.cname(node.value)
	if c_value != node.value {
		if rt := g.tc.fn_ret_types[c_value] {
			return rt
		}
	}
	if dotted_name != node.value {
		if rt := g.tc.fn_ret_types[dotted_name] {
			return rt
		}
		c_dotted_name := g.cname(dotted_name)
		if c_dotted_name != dotted_name {
			if rt := g.tc.fn_ret_types[c_dotted_name] {
				return rt
			}
		}
	}
	if cname != node.value && cname != c_value {
		if rt := g.tc.fn_ret_types[cname] {
			return rt
		}
	}
	return none
}

fn (mut g FlatGen) fn_node_param_types(node flat.Node, module_name string) []types.Type {
	if g.fn_needs_implicit_veb_ctx(node) {
		return []types.Type{}
	}
	mut explicit_params := 0
	for i in 0 .. node.children_count {
		if g.a.child_node(&node, i).kind == .param {
			explicit_params++
		}
	}
	// The module-scoped key first: a method name (`Recv.method`) is dotted but
	// not module-qualified, and two modules declaring the same receiver/method
	// pair must not share an entry.
	if params := g.fn_decl_param_types[fn_decl_module_key(module_name, node.value)] {
		if params.len == explicit_params {
			return params
		}
	}
	if !node.value.contains('.') {
		full_name := qualify_name_in_module(module_name, node.value)
		if params := g.fn_decl_param_types[full_name] {
			if params.len == explicit_params {
				return params
			}
		}
	}
	if params := g.fn_node_param_types_from_signatures(node, module_name, explicit_params) {
		return params
	}
	if info := g.generic_receiver_method_call_info(node.value) {
		if info.params.len == explicit_params {
			return info.params.clone()
		}
	}
	return []types.Type{}
}

fn (mut g FlatGen) fn_node_param_types_from_signatures(node flat.Node, module_name string, explicit_params int) ?[]types.Type {
	dotted_name := dotted_fn_name_in_module(module_name, node.value)
	cname := g.fn_c_name_in_module(module_name, node.value)
	if module_name.len > 0 && module_name != 'main' && module_name != 'builtin' {
		if params := g.matching_fn_param_types(dotted_name, explicit_params) {
			return params
		}
		c_dotted_name := g.cname(dotted_name)
		if c_dotted_name != dotted_name {
			if params := g.matching_fn_param_types(c_dotted_name, explicit_params) {
				return params
			}
		}
		if params := g.matching_fn_param_types(cname, explicit_params) {
			return params
		}
		if params := g.matching_fn_param_types(node.value, explicit_params) {
			return params
		}
		c_value := g.cname(node.value)
		if c_value != node.value {
			if params := g.matching_fn_param_types(c_value, explicit_params) {
				return params
			}
		}
		return none
	}
	if params := g.matching_fn_param_types(node.value, explicit_params) {
		return params
	}
	c_value := g.cname(node.value)
	if c_value != node.value {
		if params := g.matching_fn_param_types(c_value, explicit_params) {
			return params
		}
	}
	if dotted_name != node.value {
		if params := g.matching_fn_param_types(dotted_name, explicit_params) {
			return params
		}
		c_dotted_name := g.cname(dotted_name)
		if c_dotted_name != dotted_name {
			if params := g.matching_fn_param_types(c_dotted_name, explicit_params) {
				return params
			}
		}
	}
	if cname != node.value && cname != c_value {
		if params := g.matching_fn_param_types(cname, explicit_params) {
			return params
		}
	}
	return none
}

fn (g &FlatGen) matching_fn_param_types(name string, explicit_params int) ?[]types.Type {
	if name.len == 0 {
		return none
	}
	params := g.tc.fn_param_types[name] or { return none }
	if params.len != explicit_params {
		return none
	}
	return params
}

fn (g &FlatGen) generic_receiver_method_call_info(name string) ?types.CallInfo {
	if !name.contains('.') {
		return none
	}
	receiver := name.all_before_last('.')
	if !receiver.contains('[') || !receiver.contains(']') {
		return none
	}
	return g.tc.resolve_generic_struct_method(receiver, name.all_after_last('.'))
}

fn (mut g FlatGen) fn_node_signature_names(node flat.Node, module_name string) []string {
	dotted_name := dotted_fn_name_in_module(module_name, node.value)
	cname := g.fn_c_name_in_module(module_name, node.value)
	mut names := []string{}
	if module_name.len > 0 && module_name != 'main' && module_name != 'builtin' {
		names << dotted_name
		names << g.cname(dotted_name)
		names << cname
		names << node.value
		names << g.cname(node.value)
	} else {
		names << node.value
		names << g.cname(node.value)
		names << dotted_name
		names << g.cname(dotted_name)
		names << cname
	}
	mut deduped := []string{cap: names.len}
	for name in names {
		if name.len > 0 && name !in deduped {
			deduped << name
		}
	}
	return deduped
}

// write_fn_node_params writes fn node params output for c.
fn (mut g FlatGen) write_fn_node_params(node flat.Node) {
	mut params_len := 0
	for i in 0 .. node.children_count {
		if g.a.child_node(&node, i).kind == .param {
			params_len++
		}
	}
	needs_implicit_ctx := g.fn_needs_implicit_veb_ctx(node)
	if needs_implicit_ctx {
		params_len++
	}
	if params_len == 0 {
		g.write('void')
		return
	}
	mut written := 0
	mut param_idx := 0
	mut implicit_ctx_written := false
	insert_implicit_ctx_after_first := needs_implicit_ctx && g.fn_has_receiver_param(node)
	typed_params := g.fn_node_param_types(node, g.tc.cur_module)
	concrete_optional_params := g.is_specialized_generic_fn_node(node)
	for i in 0 .. node.children_count {
		param_id := g.a.child(&node, i)
		p := g.a.node(param_id)
		if p.kind != .param {
			continue
		}
		pt := if param_idx < typed_params.len {
			typed_params[param_idx]
		} else {
			g.tc.parse_type(p.typ)
		}
		param_idx++
		if concrete_optional_params && type_is_optional_result(pt) && p.value.len > 0 {
			g.cur_concrete_optional_params[p.value] = true
		}
		ct := if shared_ct := g.shared_param_c_type(p.typ) {
			shared_ct
		} else if concrete_optional_params && (pt is types.OptionType || pt is types.ResultType) {
			g.concrete_optional_type_name(pt)
		} else if pt is types.ArrayFixed {
			'${g.fixed_array_elem_c_type(pt.elem_type)}*'
		} else if pt is types.OptionType || pt is types.ResultType {
			g.optional_type_name(pt)
		} else {
			g.tc.c_type(pt)
		}
		if ct.starts_with('fn_ptr:') {
			g.write(g.resolve_fn_ptr_type(ct))
		} else {
			g.write(ct)
		}
		if p.value.len > 0 {
			g.write(' ')
			param_name := if p.value == '_' { '_${written}' } else { g.cname(p.value) }
			g.write(param_name)
		}
		written++
		if insert_implicit_ctx_after_first && !implicit_ctx_written {
			if written < params_len {
				g.write(', ')
			}
			g.write_implicit_veb_ctx_param()
			written++
			implicit_ctx_written = true
		}
		if written < params_len {
			g.write(', ')
		}
	}
	if needs_implicit_ctx && !implicit_ctx_written {
		g.write_implicit_veb_ctx_param()
	}
}

fn (g &FlatGen) is_specialized_generic_fn_node(node flat.Node) bool {
	return g.name_uses_specialized_generic_abi(node.value)
}

fn (mut g FlatGen) concrete_optional_type_name(t types.Type) string {
	mut base_type := types.Type(types.void_)
	if t is types.OptionType {
		base_type = t.base_type
	} else if t is types.ResultType {
		base_type = t.base_type
	} else {
		return g.tc.c_type(t)
	}
	if base_type is types.Void {
		return 'Optional'
	}
	if g.type_contains_generic_placeholder(base_type) {
		return 'Optional'
	}
	mut inner_ct := g.value_c_type(base_type)
	if inner_ct.starts_with('fn_ptr:') {
		inner_ct = g.resolve_fn_ptr_type(inner_ct)
	}
	safe_name := inner_ct.replace('*', 'ptr').replace(' ', '_')
	opt_name := 'Optional_${safe_name}'
	g.needed_optional_types[opt_name] = inner_ct
	return opt_name
}

fn (mut g FlatGen) write_implicit_veb_ctx_param() {
	pt := g.implicit_veb_ctx_type()
	g.write(g.tc.c_type(pt))
	g.write(' ctx')
}

// write_c_fn_node_params writes c fn node params output for c.
fn (mut g FlatGen) write_c_fn_node_params(node flat.Node) {
	if node.children_count == 0 {
		g.write('void')
		return
	}
	mut written := 0
	for i in 0 .. node.children_count {
		param_id := g.a.child(&node, i)
		p := g.a.node(param_id)
		if p.kind != .param {
			continue
		}
		raw_typ := if p.typ.len > 0 { p.typ } else { p.value }
		if raw_typ.len == 0 {
			continue
		}
		pt := g.tc.parse_type(raw_typ)
		ct := if pt is types.OptionType || pt is types.ResultType {
			g.optional_type_name(pt)
		} else {
			g.tc.c_type(pt)
		}
		if written > 0 {
			g.write(', ')
		}
		if ct.starts_with('fn_ptr:') {
			g.write(g.resolve_fn_ptr_type(ct))
		} else {
			g.write(ct)
		}
		if p.typ.len > 0 && p.value.len > 0 {
			g.write(' ')
			param_name := if p.value == '_' { '_${written}' } else { g.cname(p.value) }
			g.write(param_name)
		}
		written++
	}
	if written == 0 {
		g.write('void')
	}
}

// fn_ptr_typedefs supports fn ptr typedefs handling for FlatGen.
fn (mut g FlatGen) fn_ptr_typedefs() {
	// The emitted set persists on g so a second (post-region) call emits only
	// typedefs the body workers registered beyond the pre-seeded set.
	start_len := g.emitted_fn_ptr_typedefs.len
	for {
		mut pending_encoded := []string{}
		mut pending_name := []string{}
		for encoded, name in g.fn_ptr_types {
			if g.emitted_fn_ptr_typedefs[encoded] {
				continue
			}
			pending_encoded << encoded
			pending_name << name
		}
		if pending_encoded.len == 0 {
			break
		}
		for i in 0 .. pending_encoded.len {
			g.emit_fn_ptr_typedef(pending_encoded[i], pending_name[i], mut
				g.emitted_fn_ptr_typedefs)
		}
	}
	if g.emitted_fn_ptr_typedefs.len > start_len {
		g.writeln('')
	}
}

fn (mut g FlatGen) emit_fn_ptr_typedef(encoded string, name string, mut emitted map[string]bool) {
	if emitted[encoded] {
		return
	}
	emitted[encoded] = true
	ret, params := fn_ptr_typedef_parts(encoded)
	ret_ct := g.fn_ptr_return_ct(g.fn_ptr_typedef_type(ret, mut emitted))
	params_ct := g.fn_ptr_typedef_params(params, mut emitted)
	g.writeln('typedef ${ret_ct} (*${name})(${params_ct});')
}

fn fn_ptr_typedef_parts(encoded string) (string, string) {
	payload := if encoded.starts_with('fn_ptr:') { encoded['fn_ptr:'.len..] } else { encoded }
	if payload.starts_with('fn_ptr:') {
		first_pipe_idx := payload.index('|') or { return payload, 'void' }
		rest := payload[first_pipe_idx + 1..]
		second_pipe_idx := rest.index('|') or { return payload, 'void' }
		split_idx := first_pipe_idx + 1 + second_pipe_idx
		return payload[..split_idx], payload[split_idx + 1..]
	}
	pipe_idx := payload.index('|') or { return payload, 'void' }
	return payload[..pipe_idx], payload[pipe_idx + 1..]
}

fn (mut g FlatGen) fn_ptr_typedef_params(params string, mut emitted map[string]bool) string {
	clean := trimmed_space(params)
	if clean.len == 0 || clean == 'void' {
		return 'void'
	}
	mut out := []string{}
	for param in clean.split(',') {
		out << g.fn_ptr_typedef_type(param, mut emitted)
	}
	return out.join(', ')
}

fn (mut g FlatGen) fn_ptr_typedef_type(typ string, mut emitted map[string]bool) string {
	mut clean := trimmed_space(typ)
	if clean.len == 0 {
		return 'void'
	}
	if clean.starts_with('fn_ptr:') {
		clean = fn_ptr_typedef_normalized(clean)
		name := g.resolve_fn_ptr_type(clean)
		g.emit_fn_ptr_typedef(clean, name, mut emitted)
		return name
	}
	if clean == 'Optional' {
		return 'struct Optional'
	}
	if clean.starts_with('Optional_') {
		return 'struct ${clean}'
	}
	if tagged := fn_ptr_typedef_generic_placeholder_struct_tag(clean) {
		return tagged
	}
	if g.fn_ptr_typedef_is_generic_placeholder(clean) {
		return 'int'
	}
	return clean
}

fn fn_ptr_typedef_normalized(typ string) string {
	clean := trimmed_space(typ)
	if !clean.starts_with('fn_ptr:') {
		return clean
	}
	payload := clean['fn_ptr:'.len..]
	if payload.contains('|') {
		return clean
	}
	return 'fn_ptr:${payload}|void'
}

fn (g &FlatGen) fn_ptr_typedef_is_generic_placeholder(typ string) bool {
	mut clean := trimmed_space(typ)
	for clean.ends_with('*') {
		clean = clean[..clean.len - 1].trim_space()
	}
	if clean.starts_with('struct ') {
		clean = clean['struct '.len..].trim_space()
	}
	if g.type_name_known(clean)
		|| (clean.contains('__') && g.type_name_known(clean.replace('__', '.'))) {
		return false
	}
	short := if clean.contains('__') {
		clean.all_after_last('__')
	} else if clean.contains('.') {
		clean.all_after_last('.')
	} else {
		clean
	}
	if !codegen_generic_placeholder_name(short) {
		return false
	}
	if g.type_name_known(short) {
		return false
	}
	return true
}

fn fn_ptr_typedef_generic_placeholder_struct_tag(typ string) ?string {
	mut clean := trimmed_space(typ)
	mut ptr_suffix := ''
	for clean.ends_with('*') {
		clean = clean[..clean.len - 1].trim_space()
		ptr_suffix += '*'
	}
	if clean.starts_with('struct ') {
		clean = clean['struct '.len..].trim_space()
	}
	segment := if clean.contains('__') {
		clean.all_after_last('__')
	} else {
		clean
	}
	if !segment.contains('_') {
		return none
	}
	suffix := segment.all_after_last('_')
	if suffix.len == 1 && suffix[0] >= `A` && suffix[0] <= `Z` {
		return 'struct ${clean}${ptr_suffix}'
	}
	return none
}

// multi_return_forward_decls forward-declares every multi-return struct that the
// generated C can reference by name. It must run before fn_ptr_typedefs(), because
// a function-pointer typedef may name a multi-return as its (by-value) return type
// — and a `typedef RET (*fp)(...)` only needs RET's tag declared, not its full
// layout. The full struct bodies are emitted later by multi_return_typedefs(),
// after the member struct definitions they depend on are available.
fn (mut g FlatGen) multi_return_forward_decls() {
	mut emitted := map[string]bool{}
	g.walk_multi_return_typedefs(mut emitted, true)
	// Also cover multi-returns reachable only as a fn-pointer return type: parallel
	// cgen preseeds fn-ptr types that the serial path never materializes, so their
	// return multi-returns may not appear among the function/expression types above.
	for encoded, _ in g.fn_ptr_types {
		ret, _ := fn_ptr_typedef_parts(encoded)
		if ret.starts_with('multi_return_') && ret !in emitted {
			emitted[ret] = true
			g.writeln('typedef struct ${ret} ${ret};')
		}
	}
	if emitted.len > 0 {
		g.writeln('')
	}
}

// multi_return_typedefs emits the full struct definitions for multi-return types.
fn (mut g FlatGen) multi_return_typedefs() {
	mut emitted := map[string]bool{}
	g.walk_multi_return_typedefs(mut emitted, false)
	if emitted.len > 0 {
		g.writeln('')
	}
}

// walk_multi_return_typedefs visits every multi-return type reachable from a
// function return type or an expression type and emits it via emit_multi_return_typedef.
// Shared by the forward-declaration and full-definition passes so both see the same
// set in the same (deterministic) order.
fn (mut g FlatGen) walk_multi_return_typedefs(mut emitted map[string]bool, forward_only bool) {
	if !g.multi_return_types_ready {
		g.collect_multi_return_types()
	}
	for typ in g.multi_return_types {
		g.emit_concrete_multi_return_typedef(typ, mut emitted, forward_only)
	}
}

fn (mut g FlatGen) collect_multi_return_types() {
	for _, ret in g.tc.fn_ret_types {
		g.collect_concrete_multi_return_type(ret)
	}
	for _, params in g.tc.fn_param_types {
		for param in params {
			g.collect_concrete_multi_return_type(param)
		}
	}
	for _, fields in g.tc.structs {
		for field in fields {
			g.collect_concrete_multi_return_type(field.typ)
		}
	}
	for _, fields in g.tc.interface_fields {
		for field in fields {
			g.collect_concrete_multi_return_type(field.typ)
		}
	}
	for _, typ in g.tc.c_globals {
		g.collect_concrete_multi_return_type(typ)
	}
	for _, typ in g.tc.const_types {
		g.collect_concrete_multi_return_type(typ)
	}
	for idx, is_set in g.tc.expr_type_set {
		if !is_set || idx >= g.tc.expr_type_values.len {
			continue
		}
		g.collect_concrete_multi_return_type(g.tc.expr_type_values[idx])
	}
	mut cur_module := ''
	mut cur_file := ''
	for node in g.a.nodes {
		kind_id := node_kind_id(node)
		if kind_id == 77 {
			cur_file = node.value
			g.tc.cur_file = cur_file
			cur_module = ''
			g.tc.cur_module = cur_module
			continue
		}
		if kind_id == 73 {
			cur_module = node.value
			g.tc.cur_file = cur_file
			g.tc.cur_module = cur_module
			continue
		}
		g.tc.cur_file = cur_file
		g.tc.cur_module = cur_module
		// emit_multi_return_typedef only acts on (optionally `?`/`!`-wrapped) multi-return
		// types, whose string form always begins with `(`. Skip parse_type for everything
		// else — this ran on every node's type (~hundreds of thousands of parse_type calls).
		typ := node.typ
		if typ.len > 0 && (typ[0] == `(` || ((typ[0] == `?` || typ[0] == `!`) && typ.len > 1
			&& typ[1] == `(`)) {
			g.collect_concrete_multi_return_type(g.tc.parse_type(typ))
		}
	}
	g.multi_return_types_ready = true
}

fn (mut g FlatGen) collect_concrete_multi_return_type(typ types.Type) {
	if g.type_contains_generic_placeholder(typ) {
		return
	}
	if typ is types.OptionType {
		g.collect_concrete_multi_return_type(typ.base_type)
		return
	}
	if typ is types.ResultType {
		g.collect_concrete_multi_return_type(typ.base_type)
		return
	}
	if typ is types.MultiReturn {
		name := g.multi_return_c_type_name(typ)
		if name !in g.multi_return_type_names {
			g.multi_return_type_names[name] = true
			g.multi_return_types << types.Type(typ)
		}
	}
}

fn (mut g FlatGen) emit_concrete_multi_return_typedef(ret types.Type, mut emitted map[string]bool, forward_only bool) {
	if g.type_contains_generic_placeholder(ret) {
		return
	}
	g.emit_multi_return_typedef(ret, mut emitted, forward_only)
}

// emit_multi_return_typedef emits one multi-return type: a forward declaration
// (`typedef struct NAME NAME;`) when forward_only, otherwise the full struct body
// (`struct NAME { ... };`). The two forms are paired — the forward decl provides the
// typedef name, the body completes the tagged struct.
fn (mut g FlatGen) emit_multi_return_typedef(ret types.Type, mut emitted map[string]bool, forward_only bool) {
	if ret is types.OptionType {
		g.emit_multi_return_typedef(ret.base_type, mut emitted, forward_only)
		return
	}
	if ret is types.ResultType {
		g.emit_multi_return_typedef(ret.base_type, mut emitted, forward_only)
		return
	}
	if ret is types.MultiReturn {
		name := g.multi_return_c_type_name(ret)
		if name in emitted {
			return
		}
		emitted[name] = true
		if forward_only {
			for typ in ret.types {
				ct := g.value_c_type(typ)
				if ct.starts_with('fn_ptr:') {
					g.resolve_fn_ptr_type(ct)
				}
			}
			g.writeln('typedef struct ${name} ${name};')
		} else {
			g.emit_multi_return_field_option_typedefs(ret)
			g.writeln('struct ${name} {')
			for i, typ in ret.types {
				mut ct := g.multi_return_field_c_type(typ)
				if ct.starts_with('fn_ptr:') {
					ct = g.resolve_fn_ptr_type(ct)
				}
				g.writeln('\t${ct} arg${i};')
			}
			g.writeln('};')
		}
	}
}

fn (mut g FlatGen) emit_multi_return_field_option_typedefs(ret types.MultiReturn) {
	for typ in ret.types {
		if typ is types.OptionType || typ is types.ResultType {
			opt_name := g.optional_type_name(typ)
			if val_type := g.needed_optional_types[opt_name] {
				g.emit_optional_typedef(opt_name, val_type)
			}
		}
	}
}

// resolve_fn_ptr_type resolves resolve fn ptr type information for c.
fn (mut g FlatGen) resolve_fn_ptr_type(typ string) string {
	if typ in g.fn_ptr_types {
		return g.fn_ptr_types[typ]
	}
	name := fn_ptr_type_name(typ)
	g.fn_ptr_types[typ] = name
	return name
}

fn fn_ptr_type_name(typ string) string {
	mut h := u64(1469598103934665603)
	for c in typ.bytes() {
		h = (h ^ u64(c)) * u64(1099511628211)
	}
	return '_fn_ptr_${h.hex()}'
}
