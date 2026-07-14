module c

import strings
import v3.flat
import v3.types

const direct_optional_forward_return_value = '__direct_optional_forward'
const optional_success_return_value = '__optional_success_return'
const transformed_return_value_prefix = '__transformed_return:'
const pending_loop_label_marker = '__v_pending_loop_label:'

// gen_expr_lvalue emits expr lvalue output for c.
fn gen_expr_lvalue(mut g FlatGen, id flat.NodeId) {
	node := g.a.nodes[int(id)]
	if node.kind == .index {
		base_id := g.a.child(&node, 0)
		base_type := g.usable_expr_type(base_id)
		if base_type is types.Map {
			c_key := g.map_key_temp_c_type(base_type.key_type)
			c_val := g.tc.c_type(base_type.value_type)
			g.write('(*(${c_val}*)map__get_or_set(&')
			g.gen_expr(base_id)
			g.write(', &(${c_key}[]){')
			g.gen_expr(g.a.child(&node, 1))
			g.write('}, ')
			g.gen_default_value_addr_for_type(base_type.value_type)
			g.write('))')
			return
		}
	}
	g.gen_expr(id)
}

fn (mut g FlatGen) gen_split_array_append_expr_stmt(node flat.Node) bool {
	if node.kind != .infix || node.op != .pipe || node.children_count < 2 {
		return false
	}
	append_id := g.a.child(&node, 0)
	append := g.a.nodes[int(append_id)]
	if append.kind != .infix || append.op != .left_shift || append.children_count < 2 {
		return false
	}
	lhs_id := g.a.child(&append, 0)
	lhs_arr_type := types.unwrap_pointer(g.tc.resolve_type(lhs_id))
	lhs_arr := array_like_type(lhs_arr_type) or { return false }
	lhs_is_ptr := g.tc.resolve_type(lhs_id) is types.Pointer
	amp := if lhs_is_ptr { '' } else { '&' }
	c_elem := g.tc.c_type(lhs_arr.elem_type)
	g.write('array_push(${amp}')
	gen_expr_lvalue(mut g, lhs_id)
	g.write(', &(${c_elem}[]){(')
	g.gen_expr(g.a.child(&append, 1))
	g.write(' ${g.op_str(node.op)} ')
	g.gen_expr(g.a.child(&node, 1))
	g.writeln(')});')
	return true
}

fn (mut g FlatGen) gen_lock_mutex_addr(lock_id flat.NodeId) {
	lock_node := g.a.nodes[int(lock_id)]
	if lock_node.kind == .ident && g.local_storage_is_shared(lock_node.value) {
		g.write('(uintptr_t)&')
		g.write(g.cname(lock_node.value))
		g.write('->mtx')
		return
	}
	g.write('(uintptr_t)&')
	if !g.gen_shared_storage_expr(lock_id) {
		g.gen_expr(lock_id)
	}
	g.write('->mtx')
}

fn (mut g FlatGen) gen_sort_lock_mutexes(mutexes_var string, lock_count int) {
	if lock_count < 2 {
		return
	}
	if lock_count == 2 {
		g.writeln('if (${mutexes_var}[0] > ${mutexes_var}[1]) {')
		g.indent++
		g.writeln('uintptr_t ${mutexes_var}_tmp = ${mutexes_var}[0];')
		g.writeln('${mutexes_var}[0] = ${mutexes_var}[1];')
		g.writeln('${mutexes_var}[1] = ${mutexes_var}_tmp;')
		g.indent--
		g.writeln('}')
		return
	}
	g.writeln('for (int ${mutexes_var}_i = 1; ${mutexes_var}_i < ${lock_count}; ${mutexes_var}_i++) {')
	g.indent++
	g.writeln('uintptr_t ${mutexes_var}_key = ${mutexes_var}[${mutexes_var}_i];')
	g.writeln('int ${mutexes_var}_j = ${mutexes_var}_i - 1;')
	g.writeln('while (${mutexes_var}_j >= 0 && ${mutexes_var}[${mutexes_var}_j] > ${mutexes_var}_key) {')
	g.indent++
	g.writeln('${mutexes_var}[${mutexes_var}_j + 1] = ${mutexes_var}[${mutexes_var}_j];')
	g.writeln('${mutexes_var}_j--;')
	g.indent--
	g.writeln('}')
	g.writeln('${mutexes_var}[${mutexes_var}_j + 1] = ${mutexes_var}_key;')
	g.indent--
	g.writeln('}')
}

fn (mut g FlatGen) gen_lock_enter(scope_id int, node flat.Node) ?ActiveLock {
	lock_count := int(node.children_count) - 1
	if lock_count <= 0 {
		return none
	}
	lock_fn := if node.value == 'rlock' { 'sync__RwMutex__rlock' } else { 'sync__RwMutex__lock' }
	unlock_fn := if node.value == 'rlock' {
		'sync__RwMutex__runlock'
	} else {
		'sync__RwMutex__unlock'
	}
	mutexes_var := g.tmp_name()
	g.writeln('uintptr_t ${mutexes_var}[${lock_count}];')
	for i in 0 .. lock_count {
		lock_id := g.a.child(&node, i)
		g.write('${mutexes_var}[${i}] = ')
		g.gen_lock_mutex_addr(lock_id)
		g.writeln(';')
	}
	g.gen_sort_lock_mutexes(mutexes_var, lock_count)
	g.writeln('for (int ${mutexes_var}_i = 0; ${mutexes_var}_i < ${lock_count}; ${mutexes_var}_i++) {')
	g.indent++
	g.writeln('if (${mutexes_var}_i > 0 && ${mutexes_var}[${mutexes_var}_i] == ${mutexes_var}[${mutexes_var}_i - 1]) continue;')
	g.writeln('${lock_fn}((sync__RwMutex*)${mutexes_var}[${mutexes_var}_i]);')
	g.indent--
	g.writeln('}')
	return ActiveLock{
		mutexes_var: mutexes_var
		lock_count:  lock_count
		unlock_fn:   unlock_fn
		scope_id:    scope_id
		loop_depth:  g.loop_depth
		defer_depth: g.defers.len
	}
}

fn (mut g FlatGen) gen_lock_leave(active ActiveLock) {
	if active.lock_count <= 0 {
		return
	}
	if active.lock_count == 1 {
		g.writeln('${active.unlock_fn}((sync__RwMutex*)${active.mutexes_var}[0]);')
		return
	}
	g.writeln('for (int ${active.mutexes_var}_i = ${active.lock_count - 1}; ${active.mutexes_var}_i >= 0; ${active.mutexes_var}_i--) {')
	g.indent++
	g.writeln('if (${active.mutexes_var}_i > 0 && ${active.mutexes_var}[${active.mutexes_var}_i] == ${active.mutexes_var}[${active.mutexes_var}_i - 1]) continue;')
	g.writeln('${active.unlock_fn}((sync__RwMutex*)${active.mutexes_var}[${active.mutexes_var}_i]);')
	g.indent--
	g.writeln('}')
}

fn (mut g FlatGen) gen_lock_scope_cleanup(active ActiveLock, defer_end int) int {
	mut defer_start := active.defer_depth
	if defer_start < 0 {
		defer_start = 0
	}
	if defer_start > defer_end {
		defer_start = defer_end
	}
	g.gen_defers_range(defer_start, defer_end)
	g.gen_lock_leave(active)
	return defer_start
}

fn (mut g FlatGen) gen_active_lock_leaves() {
	mut i := g.active_locks.len - 1
	for i >= 0 {
		g.gen_lock_leave(g.active_locks[i])
		i--
	}
}

fn (g &FlatGen) branch_target_loop_depth(label string) int {
	if label.len == 0 {
		return g.loop_depth
	}
	return g.loop_label_depths[label] or { g.loop_depth }
}

fn (mut g FlatGen) gen_branch_lock_cleanup(label string) {
	target_depth := g.branch_target_loop_depth(label)
	mut defer_end := g.defers.len
	mut i := g.active_locks.len - 1
	for i >= 0 {
		active := g.active_locks[i]
		if active.loop_depth >= target_depth {
			defer_end = g.gen_lock_scope_cleanup(active, defer_end)
		}
		i--
	}
}

struct FnPreludeScan {
mut:
	defer_ids              []flat.NodeId
	lock_scopes            []int
	goto_label_lock_scopes map[string][]int
}

fn new_fn_prelude_scan() FnPreludeScan {
	return FnPreludeScan{
		defer_ids:              []flat.NodeId{}
		lock_scopes:            []int{}
		goto_label_lock_scopes: map[string][]int{}
	}
}

fn (g &FlatGen) collect_fn_prelude_scan(node flat.Node) FnPreludeScan {
	mut scan := new_fn_prelude_scan()
	start := node.children_start
	end := start + int(node.children_count)
	if start >= 0 && end <= g.a.children.len {
		for i in start .. end {
			// The child range is checked above; avoid per-child bounds checks in this hot walk.
			g.collect_prelude_scan_from(unsafe { g.a.children[i] }, mut scan, true)
		}
	}
	return scan
}

fn (g &FlatGen) collect_top_level_prelude_scan(stmts []TopLevelStmt) FnPreludeScan {
	mut scan := new_fn_prelude_scan()
	for stmt in stmts {
		g.collect_prelude_scan_from(stmt.id, mut scan, true)
	}
	return scan
}

fn (g &FlatGen) collect_prelude_scan_from(id flat.NodeId, mut scan FnPreludeScan, collect_defers bool) {
	if int(id) < 0 || int(id) >= g.a.nodes.len {
		return
	}
	node := g.a.nodes[int(id)]
	mut child_collect_defers := collect_defers
	if collect_defers
		&& (node.kind == .fn_decl || node.kind == .c_fn_decl || node.kind == .fn_literal) {
		child_collect_defers = false
	}
	if collect_defers && node.kind == .defer_stmt && node.value == 'function' {
		scan.defer_ids << id
		child_collect_defers = false
	}
	mut pushed_lock := false
	if node.kind == .lock_expr {
		scan.lock_scopes << int(id)
		pushed_lock = true
	}
	if node.kind == .label_stmt && node.value.len > 0 {
		scan.goto_label_lock_scopes[node.value] = scan.lock_scopes.clone()
	}
	start := node.children_start
	end := start + int(node.children_count)
	if start >= 0 && end <= g.a.children.len {
		for i in start .. end {
			// The child range is checked above; avoid per-child bounds checks in this hot walk.
			g.collect_prelude_scan_from(unsafe { g.a.children[i] }, mut scan, child_collect_defers)
		}
	}
	if pushed_lock {
		scan.lock_scopes.delete_last()
	}
}

fn (g &FlatGen) active_lock_scope_ids() []int {
	mut scopes := []int{cap: g.active_locks.len}
	for active in g.active_locks {
		scopes << active.scope_id
	}
	return scopes
}

fn (g &FlatGen) goto_target_lock_scopes(label string) []int {
	if label.len == 0 {
		return g.active_lock_scope_ids()
	}
	return g.goto_label_lock_scopes[label] or { []int{} }
}

fn (mut g FlatGen) gen_goto_lock_leaves(label string) bool {
	target_scopes := g.goto_target_lock_scopes(label)
	active_scopes := g.active_lock_scope_ids()
	for i, target_scope in target_scopes {
		if i >= active_scopes.len || active_scopes[i] != target_scope {
			g.writeln('#error goto into a different lock scope is not supported')
			return false
		}
	}
	mut i := g.active_locks.len - 1
	mut defer_end := g.defers.len
	for i >= 0 {
		if i >= target_scopes.len {
			defer_end = g.gen_lock_scope_cleanup(g.active_locks[i], defer_end)
		}
		i--
	}
	return true
}

fn (mut g FlatGen) gen_return_cleanup() {
	if g.active_locks.len == 0 {
		g.gen_all_defers()
		g.gen_current_return_ownership_drops()
		return
	}
	mut defer_end := g.defers.len
	mut i := g.active_locks.len - 1
	for i >= 0 {
		active := g.active_locks[i]
		defer_end = g.gen_lock_scope_cleanup(active, defer_end)
		i--
	}
	g.gen_defers_range(0, defer_end)
	g.gen_fn_defers()
	g.gen_current_return_ownership_drops()
}

fn (mut g FlatGen) gen_current_return_ownership_drops() {
	g.gen_ownership_drops(g.cur_return_drops)
}

fn (mut g FlatGen) take_return_ownership_drops() []types.OwnershipDropEntry {
	fn_name := qualify_name_in_module(g.tc.cur_module, g.cur_fn_name)
	entries := g.tc.ownership_drop_entries_at_return(fn_name, g.ownership_return_index)
	g.ownership_return_index++
	return entries
}

fn transformed_return_source_id(value string) ?flat.NodeId {
	if !value.starts_with(transformed_return_value_prefix) {
		return none
	}
	text := value[transformed_return_value_prefix.len..]
	if text.len == 0 {
		return none
	}
	return flat.NodeId(text.int())
}

fn (mut g FlatGen) take_return_node_ownership_drops(id flat.NodeId) []types.OwnershipDropEntry {
	fn_name := qualify_name_in_module(g.tc.cur_module, g.cur_fn_name)
	return g.tc.ownership_drop_entries_at_return_node(fn_name, id)
}

fn (mut g FlatGen) take_transformed_return_ownership_drops(source_id flat.NodeId) []types.OwnershipDropEntry {
	source_key := int(source_id).str()
	if source_key !in g.ownership_seen_return_sources {
		g.ownership_seen_return_sources[source_key] = true
		g.ownership_return_index++
	}
	return g.take_return_node_ownership_drops(source_id)
}

fn (mut g FlatGen) gen_propagation_return_cleanup() {
	entries := g.take_propagation_ownership_drops()
	old_drops := g.cur_return_drops.clone()
	g.cur_return_drops = entries
	g.gen_return_cleanup()
	g.cur_return_drops = old_drops
}

fn (mut g FlatGen) take_propagation_ownership_drops() []types.OwnershipDropEntry {
	fn_name := qualify_name_in_module(g.tc.cur_module, g.cur_fn_name)
	entries := g.tc.ownership_drop_entries_at_propagation(fn_name, g.ownership_propagation_index)
	g.ownership_propagation_index++
	return entries
}

fn (mut g FlatGen) take_return_stmt_ownership_drops(node flat.Node) []types.OwnershipDropEntry {
	mut entries := []types.OwnershipDropEntry{}
	if source_id := transformed_return_source_id(node.value) {
		entries = g.take_transformed_return_ownership_drops(source_id)
	} else if node.typ.len == 0 {
		entries = g.take_return_ownership_drops()
	} else if node.typ[0] !in [`!`, `?`] {
		entries = []types.OwnershipDropEntry{}
	} else if node.value == direct_optional_forward_return_value
		|| node.value == optional_success_return_value
		|| g.return_stmt_is_explicit_optional_failure(node) {
		entries = g.take_return_ownership_drops()
	} else {
		entries = g.take_propagation_ownership_drops()
	}
	if g.pending_return_scope_drops.len == 0 {
		return entries
	}
	mut combined := g.pending_return_scope_drops.clone()
	for entry in entries {
		combined << entry
	}
	g.pending_return_scope_drops = []types.OwnershipDropEntry{}
	return combined
}

fn (g &FlatGen) return_stmt_is_explicit_optional_failure(node flat.Node) bool {
	if node.children_count != 1 {
		return false
	}
	ret_id := g.a.child(&node, 0)
	if int(ret_id) < 0 || int(ret_id) >= g.a.nodes.len {
		return false
	}
	ret_node := g.a.nodes[int(ret_id)]
	if ret_node.kind == .none_expr {
		return true
	}
	if ret_node.kind != .call || ret_node.children_count == 0 {
		return false
	}
	fn_n := g.a.child_node(&ret_node, 0)
	return fn_n.value == 'error' || fn_n.value == 'error_with_code'
}

fn (mut g FlatGen) gen_scope_ownership_drops() {
	g.gen_ownership_drops(g.take_scope_ownership_drops())
}

fn (mut g FlatGen) take_scope_ownership_drops() []types.OwnershipDropEntry {
	fn_name := qualify_name_in_module(g.tc.cur_module, g.cur_fn_name)
	entries := g.tc.ownership_drop_entries_at_scope_exit(fn_name, g.ownership_scope_index)
	g.ownership_scope_index++
	return entries
}

fn (mut g FlatGen) gen_loop_control_ownership_drops() {
	fn_name := qualify_name_in_module(g.tc.cur_module, g.cur_fn_name)
	entries := g.tc.ownership_drop_entries_at_loop_control(fn_name, g.ownership_loop_control_index)
	g.ownership_loop_control_index++
	g.gen_ownership_drops(entries)
}

fn (mut g FlatGen) gen_loop_iteration_ownership_drops() {
	fn_name := qualify_name_in_module(g.tc.cur_module, g.cur_fn_name)
	entries := g.tc.ownership_drop_entries_at_loop_iteration(fn_name,
		g.ownership_loop_iteration_index)
	g.ownership_loop_iteration_index++
	g.gen_ownership_drops(entries)
}

fn (mut g FlatGen) gen_ownership_drops(entries []types.OwnershipDropEntry) {
	for entry in entries {
		method := g.resolve_method_name(entry.type_name, 'drop')
		if method.len == 0 {
			g.writeln('#error missing generated Drop method for ${entry.type_name}')
			continue
		}
		cname := g.cname(entry.name)
		if entry.optional_wrapper {
			g.writeln('if (${cname}.ok) {')
			g.indent++
			g.writeln('${g.cname(method)}(&${cname}.value);')
			g.indent--
			g.writeln('}')
			continue
		}
		g.writeln('${g.cname(method)}(&${cname});')
	}
}

fn (mut g FlatGen) gen_lock_stmt(id flat.NodeId, node flat.Node) {
	active := g.gen_lock_enter(int(id), node) or {
		g.gen_lock_body_stmt(node)
		return
	}
	g.active_locks << active
	g.gen_lock_body_stmt(node)
	g.active_locks.delete_last()
	g.gen_lock_leave(active)
}

fn (mut g FlatGen) gen_lock_body_stmt(node flat.Node) {
	if node.children_count > 0 {
		body_id := g.a.child(&node, node.children_count - 1)
		if int(body_id) >= 0 {
			body := g.a.nodes[int(body_id)]
			if body.kind == .block {
				g.gen_node(body_id)
			} else if body.kind == .expr_stmt {
				g.gen_node(body_id)
			} else {
				g.gen_expr(body_id)
				g.writeln(';')
			}
		}
	}
}

fn (g &FlatGen) lock_expr_result_type(node flat.Node) types.Type {
	if node.typ.len > 0 {
		typ := g.tc.parse_type(node.typ)
		if typ !is types.Unknown && typ !is types.Void {
			return typ
		}
	}
	if node.children_count == 0 {
		return types.Type(types.void_)
	}
	body_id := g.a.child(&node, node.children_count - 1)
	if int(body_id) < 0 {
		return types.Type(types.void_)
	}
	body := g.a.nodes[int(body_id)]
	if body.kind == .block && body.children_count > 0 {
		last_id := g.a.child(&body, body.children_count - 1)
		last := g.a.nodes[int(last_id)]
		if last.kind == .expr_stmt && last.children_count > 0 {
			return g.usable_expr_type(g.a.child(&last, 0))
		}
		return g.usable_expr_type(last_id)
	}
	if body.kind == .expr_stmt && body.children_count > 0 {
		return g.usable_expr_type(g.a.child(&body, 0))
	}
	return g.usable_expr_type(body_id)
}

fn (mut g FlatGen) gen_lock_expr_result_assign(tmp string, result_type types.Type, id flat.NodeId) {
	if int(id) < 0 || int(id) >= g.a.nodes.len {
		return
	}
	node := g.a.nodes[int(id)]
	if result_type is types.MultiReturn {
		if g.gen_lock_expr_multi_return_assign(tmp, result_type, id, node) {
			return
		}
	}
	if node.kind == .expr_stmt && node.children_count > 0 {
		g.write('${tmp} = ')
		g.gen_expr_with_expected_type(g.a.child(&node, 0), result_type)
		g.writeln(';')
		return
	}
	if node.kind == .block {
		if node.children_count == 0 {
			return
		}
		last_idx := int(node.children_count) - 1
		for i in 0 .. last_idx {
			g.gen_node(g.a.child(&node, i))
		}
		g.gen_lock_expr_result_assign(tmp, result_type, g.a.child(&node, last_idx))
		return
	}
	if g.is_expr_kind(node.kind) {
		g.write('${tmp} = ')
		g.gen_expr_with_expected_type(id, result_type)
		g.writeln(';')
		return
	}
	g.gen_node(id)
}

fn (mut g FlatGen) gen_lock_expr_multi_return_assign(tmp string, result_type types.MultiReturn, _id flat.NodeId, node flat.Node) bool {
	if node.kind == .block {
		if _ := g.multi_return_tail_parts(&node, result_type.types.len) {
			g.write('${tmp} = ')
			_ = g.gen_multi_return_block_expr(&node, result_type)
			g.writeln(';')
			return true
		}
		if _ := g.multi_return_value_exprs_in_block(&node, result_type.types.len) {
			g.write('${tmp} = ({')
			_ = g.gen_multi_return_block_field_assigns(tmp, result_type, &node)
			g.write('${tmp};})')
			g.writeln(';')
			return true
		}
		return false
	}
	if node.kind != .expr_stmt || node.children_count != result_type.types.len {
		return false
	}
	for i in 0 .. node.children_count {
		value_id := g.a.child(&node, i)
		field := '${tmp}.arg${i}'
		if fixed := array_fixed_type(result_type.types[i]) {
			g.gen_fixed_array_copy_from_node(field, value_id, fixed)
			continue
		}
		g.write('${field} = ')
		g.gen_expr_with_expected_type(value_id, result_type.types[i])
		g.writeln(';')
	}
	return true
}

fn (mut g FlatGen) gen_multi_return_block_field_assigns(tmp string, result_type types.MultiReturn, block &flat.Node) bool {
	value_ids := g.multi_return_value_exprs_in_block(block, result_type.types.len) or {
		return false
	}
	mut value_idx := 0
	for i in 0 .. block.children_count {
		child_id := g.a.child(block, i)
		child := g.a.nodes[int(child_id)]
		if child.kind == .expr_stmt && child.children_count > 0 {
			mut consumed := false
			for j in 0 .. child.children_count {
				expr_id := g.a.child(&child, j)
				if value_idx < value_ids.len && expr_id == value_ids[value_idx] {
					field := '${tmp}.arg${value_idx}'
					if fixed := array_fixed_type(result_type.types[value_idx]) {
						g.gen_fixed_array_copy_from_node(field, expr_id, fixed)
					} else {
						g.write('${field} = ')
						g.gen_expr_with_expected_type(expr_id, result_type.types[value_idx])
						g.writeln(';')
					}
					value_idx++
					consumed = true
				}
			}
			if consumed {
				continue
			}
		}
		g.gen_node(child_id)
	}
	return value_idx == result_type.types.len
}

fn (g &FlatGen) multi_return_value_exprs_in_block(block &flat.Node, count int) ?[]flat.NodeId {
	if count <= 0 {
		return none
	}
	mut values := []flat.NodeId{cap: count}
	for i in 0 .. block.children_count {
		child_id := g.a.child(block, i)
		child := g.a.nodes[int(child_id)]
		if child.kind != .expr_stmt || child.children_count == 0 {
			continue
		}
		for j in 0 .. child.children_count {
			expr_id := g.a.child(&child, j)
			typ := g.usable_expr_type(expr_id)
			if typ is types.Void || typ is types.Unknown {
				continue
			}
			values << expr_id
		}
	}
	if values.len != count {
		return none
	}
	return values
}

fn (mut g FlatGen) gen_lock_expr(id flat.NodeId, node flat.Node) {
	result_type := if g.expected_expr_type is types.MultiReturn {
		types.Type(g.expected_expr_type)
	} else {
		g.tc.expr_type(id) or { g.lock_expr_result_type(node) }
	}
	if result_type is types.Void || result_type is types.Unknown {
		g.write('({')
		active := g.gen_lock_enter(int(id), node) or {
			g.gen_lock_body_stmt(node)
			g.write('0;})')
			return
		}
		g.active_locks << active
		g.gen_lock_body_stmt(node)
		g.active_locks.delete_last()
		g.gen_lock_leave(active)
		g.write('0;})')
		return
	}
	ct := g.value_c_type(result_type)
	tmp := g.tmp_name()
	g.write('({ ${ct} ${tmp};')
	active := g.gen_lock_enter(int(id), node) or {
		if node.children_count > 0 {
			body_id := g.a.child(&node, node.children_count - 1)
			if int(body_id) >= 0 {
				g.gen_lock_expr_result_assign(tmp, result_type, body_id)
			}
		}
		g.write('${tmp};})')
		return
	}
	g.active_locks << active
	if node.children_count > 0 {
		body_id := g.a.child(&node, node.children_count - 1)
		if int(body_id) >= 0 {
			body := g.a.nodes[int(body_id)]
			if body.kind == .block {
				defer_start := g.defers.len
				last_idx := int(body.children_count) - 1
				for i in 0 .. last_idx {
					g.gen_node(g.a.child(&body, i))
				}
				if last_idx >= 0 {
					last_id := g.a.child(&body, last_idx)
					g.gen_lock_expr_result_assign(tmp, result_type, last_id)
				}
				g.gen_defers_from(defer_start)
				g.trim_defers(defer_start)
			} else {
				g.gen_lock_expr_result_assign(tmp, result_type, body_id)
			}
		}
	}
	g.active_locks.delete_last()
	g.gen_lock_leave(active)
	g.write('${tmp};})')
}

struct FlatSelectCase {
	branch_id  flat.NodeId
	channel_id flat.NodeId
	value_id   flat.NodeId = flat.empty_node
	lhs_id     flat.NodeId = flat.empty_node
	body_start int
	is_push    bool
	is_decl    bool
}

fn (g &FlatGen) select_channel_elem_type(channel_id flat.NodeId, value_id flat.NodeId) types.Type {
	channel_type := concrete_receiver_type(g.usable_expr_type(channel_id))
	if channel_type is types.Channel {
		return channel_type.elem_type
	}
	if int(value_id) >= 0 {
		return g.usable_expr_type(value_id)
	}
	return types.Unknown{}
}

fn (mut g FlatGen) gen_select(id flat.NodeId, node flat.Node, is_expr bool) {
	mut cases := []FlatSelectCase{}
	mut exception_branch := flat.empty_node
	mut timeout_id := flat.empty_node
	for i in 0 .. node.children_count {
		branch_id := g.a.child(&node, i)
		if int(branch_id) < 0 || int(branch_id) >= g.a.nodes.len {
			continue
		}
		branch := g.a.nodes[int(branch_id)]
		if branch.kind != .select_branch || branch.children_count == 0 {
			if branch.value == 'else' {
				exception_branch = branch_id
			}
			continue
		}
		if branch.value == 'else' {
			exception_branch = branch_id
			continue
		}
		first_id := g.a.child(&branch, 0)
		first := g.a.nodes[int(first_id)]
		if first.kind == .infix && first.op == .arrow && first.children_count >= 2 {
			cases << FlatSelectCase{
				branch_id:  branch_id
				channel_id: g.a.child(&first, 0)
				value_id:   g.a.child(&first, 1)
				body_start: 1
				is_push:    true
			}
			continue
		}
		if branch.value in ['recv', 'recv_assign'] && branch.children_count >= 2 {
			second_id := g.a.child(&branch, 1)
			second := g.a.nodes[int(second_id)]
			if second.kind == .prefix && second.op == .arrow && second.children_count > 0 {
				cases << FlatSelectCase{
					branch_id:  branch_id
					channel_id: g.a.child(&second, 0)
					value_id:   second_id
					lhs_id:     first_id
					body_start: 2
					is_decl:    branch.value == 'recv'
				}
				continue
			}
		}
		if first.kind == .prefix && first.op == .arrow && first.children_count > 0 {
			cases << FlatSelectCase{
				branch_id:  branch_id
				channel_id: g.a.child(&first, 0)
				value_id:   first_id
				body_start: 1
			}
			continue
		}
		exception_branch = branch_id
		timeout_id = first_id
	}

	if is_expr {
		g.write('({')
		g.writeln('')
	}
	mut temps := []string{cap: cases.len}
	mut elem_types := []types.Type{cap: cases.len}
	for select_case in cases {
		elem_type := g.select_channel_elem_type(select_case.channel_id, select_case.value_id)
		elem_types << elem_type
		ct := g.value_c_type(elem_type)
		tmp := g.tmp_name()
		temps << tmp
		if select_case.is_push {
			g.write('${ct} ${tmp} = ')
			g.gen_expr_with_expected_type(select_case.value_id, elem_type)
			g.writeln(';')
		} else {
			g.writeln('${ct} ${tmp} = (${ct}){0};')
		}
	}
	select_result := g.tmp_name()
	if cases.len == 0 && int(timeout_id) >= 0 {
		g.write('time__sleep(')
		g.gen_expr(timeout_id)
		g.writeln(');')
		g.writeln('int ${select_result} = -1;')
	} else {
		channels := g.tmp_name()
		directions := g.tmp_name()
		objects := g.tmp_name()
		if cases.len == 0 {
			g.writeln('Array ${channels} = array_new(sizeof(sync__Channel*), 0, 0);')
			g.writeln('Array ${directions} = array_new(sizeof(int), 0, 0);')
			g.writeln('Array ${objects} = array_new(sizeof(void*), 0, 0);')
		} else {
			g.write('Array ${channels} = new_array_from_c_array(${cases.len}, ${cases.len}, sizeof(sync__Channel*), (sync__Channel*[]){')
			for i, select_case in cases {
				if i > 0 {
					g.write(', ')
				}
				g.write('(sync__Channel*)(')
				g.gen_channel_try_receiver(select_case.channel_id)
				g.write(')')
			}
			g.writeln('});')
			g.write('Array ${directions} = new_array_from_c_array(${cases.len}, ${cases.len}, sizeof(int), (int[]){')
			for i, select_case in cases {
				if i > 0 {
					g.write(', ')
				}
				g.write(if select_case.is_push { '1' } else { '0' })
			}
			g.writeln('});')
			g.write('Array ${objects} = new_array_from_c_array(${cases.len}, ${cases.len}, sizeof(void*), (void*[]){')
			for i, tmp in temps {
				if i > 0 {
					g.write(', ')
				}
				g.write('(void*)&${tmp}')
			}
			g.writeln('});')
		}
		select_fn := if is_expr { 'sync__channel_select' } else { 'sync__channel_select_lang' }
		g.write('int ${select_result} = ${select_fn}(&${channels}, ${directions}, &${objects}, ')
		if int(timeout_id) >= 0 {
			g.gen_expr(timeout_id)
		} else if int(exception_branch) >= 0 {
			g.write('-1')
		} else {
			g.write('((i64)9223372036854775807LL)')
		}
		g.writeln(');')
		g.writeln('array__free(&${objects});')
		g.writeln('array__free(&${directions});')
		g.writeln('array__free(&${channels});')
	}

	for i, select_case in cases {
		if i == 0 {
			g.writeln('if (${select_result} == ${i}) {')
		} else {
			g.writeln('else if (${select_result} == ${i}) {')
		}
		g.indent++
		g.push_scope()
		defer_start := g.defers.len
		if !select_case.is_push && int(select_case.lhs_id) >= 0 {
			lhs := g.a.nodes[int(select_case.lhs_id)]
			if lhs.kind != .ident || lhs.value != '_' {
				if select_case.is_decl {
					ct := g.value_c_type(elem_types[i])
					g.write('${ct} ')
					gen_expr_lvalue(mut g, select_case.lhs_id)
					g.writeln(' = ${temps[i]};')
					if lhs.kind == .ident {
						owner := g.tc.cur_scope.insert_with_owner(lhs.value, elem_types[i])
						g.track_local_pointer_storage_decl(lhs, owner, elem_types[i], ct)
					}
				} else {
					gen_expr_lvalue(mut g, select_case.lhs_id)
					g.write(' = ')
					lhs_type := g.usable_expr_type(select_case.lhs_id)
					expected := g.assign_rhs_expected_type(select_case.lhs_id, lhs_type)
					g.gen_select_receive_value(temps[i], elem_types[i], expected)
					g.writeln(';')
				}
			}
		}
		branch := g.a.nodes[int(select_case.branch_id)]
		for j in select_case.body_start .. branch.children_count {
			g.gen_node(g.a.child(&branch, j))
		}
		g.gen_defers_from(defer_start)
		g.gen_scope_ownership_drops()
		g.trim_defers(defer_start)
		g.pop_scope()
		g.indent--
		g.writeln('}')
	}
	if int(exception_branch) >= 0 {
		exception_test := if int(timeout_id) < 0 && !is_expr {
			'${select_result} == -1 || ${select_result} == -2'
		} else {
			'${select_result} == -1'
		}
		if cases.len == 0 {
			g.writeln('if (${exception_test}) {')
		} else {
			g.writeln('else if (${exception_test}) {')
		}
		g.indent++
		g.push_scope()
		defer_start := g.defers.len
		branch := g.a.nodes[int(exception_branch)]
		body_start := if branch.value == 'else' { 0 } else { 1 }
		for j in body_start .. branch.children_count {
			g.gen_node(g.a.child(&branch, j))
		}
		g.gen_defers_from(defer_start)
		g.gen_scope_ownership_drops()
		g.trim_defers(defer_start)
		g.pop_scope()
		g.indent--
		g.writeln('}')
	}
	if is_expr {
		g.writeln('${select_result} != -2; })')
	}
	_ = id
}

fn (mut g FlatGen) gen_select_receive_value(expr string, actual types.Type, expected types.Type) {
	expected_base := select_receive_unalias_type(expected)
	actual_base := select_receive_unalias_type(actual)
	if expected_base is types.OptionType || expected_base is types.ResultType {
		if actual_base is types.OptionType || actual_base is types.ResultType {
			expected_payload := if expected_base is types.OptionType {
				expected_base.base_type
			} else {
				(expected_base as types.ResultType).base_type
			}
			actual_payload := if actual_base is types.OptionType {
				actual_base.base_type
			} else {
				(actual_base as types.ResultType).base_type
			}
			expected_ct := g.optional_type_name(expected)
			actual_ct := g.optional_type_name(actual)
			if expected_ct == actual_ct {
				g.write(expr)
				return
			}
			g.write('((${expr}).ok ? (${expected_ct}){.ok = true')
			if expected_payload !is types.Void {
				g.write(', .value = ')
				g.gen_select_receive_value('(${expr}).value', actual_payload, expected_payload)
			}
			g.write('} : (${expected_ct}){.ok = false, .err = (${expr}).err})')
			return
		}
		base_type := if expected_base is types.OptionType {
			expected_base.base_type
		} else {
			(expected_base as types.ResultType).base_type
		}
		ct := g.optional_type_name(expected)
		if base_type is types.Void {
			g.write('(${ct}){.ok = true}')
			return
		}
		g.write('(${ct}){.ok = true, .value = ')
		g.gen_select_receive_value(expr, actual, base_type)
		g.write('}')
		return
	}
	if expected !is types.Pointer && actual is types.Pointer
		&& g.type_names_match(actual.base_type, expected) {
		g.write('*${expr}')
		return
	}
	if g.gen_select_receive_array_value(expr, actual_base, expected_base) {
		return
	}
	if g.gen_select_receive_map_value(expr, actual_base, expected_base) {
		return
	}
	if g.gen_select_receive_interface_value(expr, actual_base, expected_base) {
		return
	}
	if g.gen_select_receive_sum_value(expr, actual_base, expected_base) {
		return
	}
	g.write(expr)
}

fn select_receive_unalias_type(typ types.Type) types.Type {
	if typ is types.Alias {
		return select_receive_unalias_type(typ.base_type)
	}
	return typ
}

fn (mut g FlatGen) gen_select_receive_array_value(expr string, actual types.Type, expected types.Type) bool {
	if expected !is types.Array {
		return false
	}
	expected_array := expected as types.Array
	expected_elem := select_receive_unalias_type(expected_array.elem_type)
	mut actual_elem := types.Type(types.void_)
	mut source := ''
	mut source_len := ''
	mut source_value := ''
	mut free_source := false
	if actual is types.Array {
		actual_elem = select_receive_unalias_type(actual.elem_type)
		if actual_elem.name() == expected_elem.name() {
			return false
		}
		source = g.tmp_name()
		source_len = '${source}.len'
		actual_elem_ct := g.value_c_type(actual_elem)
		source_value = '*(${actual_elem_ct}*)array_get(${source}, '
		free_source = true
	} else if actual is types.ArrayFixed {
		actual_elem = select_receive_unalias_type(actual.elem_type)
		source_len = g.fixed_array_len_value(actual)
		source_value = '${expr}['
	} else {
		return false
	}
	out := g.tmp_name()
	idx := g.tmp_name()
	expected_elem_ct := g.value_c_type(expected_elem)
	if free_source {
		g.write('({ Array ${source} = ${expr}; ')
	} else {
		g.write('({ ')
	}
	g.write('Array ${out} = __new_array(${source_len}, ${source_len}, sizeof(${expected_elem_ct})); ')
	g.write('for (int ${idx} = 0; ${idx} < ${source_len}; ${idx}++) { ((${expected_elem_ct}*)${out}.data)[${idx}] = ')
	if free_source {
		g.gen_select_receive_value('${source_value}${idx})', actual_elem, expected_elem)
	} else {
		g.gen_select_receive_value('${source_value}${idx}]', actual_elem, expected_elem)
	}
	g.write('; } ')
	if free_source {
		g.write('array__free(&${source}); ')
	}
	g.write('${out}; })')
	return true
}

fn (mut g FlatGen) gen_select_receive_map_value(expr string, actual types.Type, expected types.Type) bool {
	if actual !is types.Map || expected !is types.Map {
		return false
	}
	actual_map := actual as types.Map
	expected_map := expected as types.Map
	actual_key := select_receive_unalias_type(actual_map.key_type)
	expected_key := select_receive_unalias_type(expected_map.key_type)
	actual_value := select_receive_unalias_type(actual_map.value_type)
	expected_value := select_receive_unalias_type(expected_map.value_type)
	if actual_key.name() == expected_key.name() && actual_value.name() == expected_value.name() {
		return false
	}
	source := g.tmp_name()
	out := g.tmp_name()
	keys := g.tmp_name()
	idx := g.tmp_name()
	source_key := g.tmp_name()
	key := g.tmp_name()
	source_value := g.tmp_name()
	value := g.tmp_name()
	actual_key_ct := g.map_key_temp_c_type(actual_key)
	expected_key_ct := g.map_key_temp_c_type(expected_key)
	actual_value_ct := g.value_c_type(actual_value)
	expected_value_ct := g.value_c_type(expected_value)
	g.write('({ map ${source} = ${expr}; map ${out} = ')
	g.write_new_map(expected_key, expected_value)
	g.write('; Array ${keys} = map__keys(&${source}); ')
	g.write('for (int ${idx} = 0; ${idx} < ${keys}.len; ${idx}++) { ')
	g.write('${actual_key_ct} ${source_key} = *(${actual_key_ct}*)array_get(${keys}, ${idx}); ')
	g.write('${actual_value_ct} ${source_value} = *(${actual_value_ct}*)map__get_check(&${source}, &${source_key}); ')
	g.write('${expected_key_ct} ${key} = ')
	g.gen_select_receive_value(source_key, actual_key, expected_key)
	g.write('; ${expected_value_ct} ${value} = ')
	g.gen_select_receive_value(source_value, actual_value, expected_value)
	g.write('; map__set(&${out}, &${key}, &${value}); ${source}.free_fn(&${source_key}); } ')
	g.write('array__free(&${keys}); map__free(&${source}); ${out}; })')
	return true
}

fn (mut g FlatGen) gen_select_receive_interface_value(expr string, actual types.Type, expected types.Type) bool {
	iface_type := if expected is types.Alias { expected.base_type } else { expected }
	if iface_type !is types.Interface {
		return false
	}
	iface := iface_type as types.Interface
	actual_clean := if actual is types.Pointer { actual.base_type } else { actual }
	actual_base := actual_clean
	if actual_base is types.Interface {
		return false
	}
	concrete_name := actual_base.name()
	if concrete_name.len == 0 {
		return false
	}
	type_id := g.iface_type_id_for_concrete(iface.name, actual_clean)
	ct := g.tc.c_type(iface)
	concrete_ct := g.tc.c_type(actual_base)
	if g.is_ierror_type_name(iface.name) {
		empty_sid := g.intern_string('')
		object := if actual is types.Pointer {
			expr
		} else {
			'memdup(&${expr}, sizeof(${concrete_ct}))'
		}
		g.write('(${ct}){._typ = ${type_id}, ._object = ${object}, .message = _str_${empty_sid}, .code = 0}')
		return true
	}
	fields := g.tc.interface_fields[iface.name] or { []types.StructField{} }
	if fields.len > 0 {
		tmp := g.tmp_count
		g.tmp_count++
		if actual is types.Pointer {
			g.write('({ ${concrete_ct}* _iface${tmp} = ${expr}; (${ct}){._typ = ${type_id}, ._object = _iface${tmp}')
			for field in fields {
				g.write(', .${g.cname(field.name)} = _iface${tmp}->${g.cname(field.name)}')
			}
		} else {
			g.write('({ ${concrete_ct} _iface${tmp} = ${expr}; (${ct}){._typ = ${type_id}, ._object = memdup(&_iface${tmp}, sizeof(${concrete_ct}))')
			for field in fields {
				g.write(', .${g.cname(field.name)} = _iface${tmp}.${g.cname(field.name)}')
			}
		}
		g.write('}; })')
		return true
	}
	g.write('(${ct}){._typ = ${type_id}, ._object = ')
	if actual is types.Pointer {
		g.write(expr)
	} else {
		g.write('memdup(&${expr}, sizeof(${concrete_ct}))')
	}
	g.write('}')
	return true
}

fn (mut g FlatGen) gen_select_receive_sum_value(expr string, actual types.Type, expected types.Type) bool {
	sum_type0 := if expected is types.Alias { expected.base_type } else { expected }
	if sum_type0 !is types.SumType {
		return false
	}
	raw_actual := actual
	actual_base := if raw_actual is types.Alias { raw_actual.base_type } else { raw_actual }
	if actual_base is types.SumType {
		return false
	}
	sum_type := sum_type0 as types.SumType
	sum_name := g.resolve_sum_name(sum_type.name)
	variants := g.tc.sum_types[sum_name] or { return false }
	mut actual_type := raw_actual
	mut variant := g.resolve_variant(sum_name, actual_type.name())
	if variant !in variants {
		actual_type = actual_base
		variant = g.resolve_variant(sum_name, actual_type.name())
	}
	if variant !in variants {
		return false
	}
	variant_type := g.tc.parse_type(variant)
	inner_ct := g.value_c_type(variant_type)
	ct := g.tc.c_type(sum_type0)
	idx := g.sum_type_index(sum_name, variant)
	field := g.sum_field_name(variant)
	g.write('(${ct}){.typ = ${idx}, .${field} = ')
	if actual_type is types.Pointer && g.type_names_match(actual_type.base_type, variant_type) {
		g.write(expr)
	} else {
		g.write('(${inner_ct}*)memdup(&${expr}, sizeof(${inner_ct}))')
	}
	g.write('}')
	return true
}

// gen_node emits node output for c.
fn (mut g FlatGen) gen_node(id flat.NodeId) {
	if int(id) < 0 || int(id) >= g.a.nodes.len {
		return
	}
	node := g.a.nodes[int(id)]
	if node.kind !in [.label_stmt, .for_stmt, .for_in_stmt] {
		g.pending_loop_label = ''
	}
	g.in_return = false
	match node.kind {
		.fn_decl, .c_fn_decl, .struct_decl, .type_decl, .enum_decl, .interface_decl {
			return
		}
		.expr_stmt {
			child_id := g.a.child(&node, 0)
			if int(child_id) < 0 || int(child_id) >= g.a.nodes.len {
				return
			}
			child := g.a.nodes[int(child_id)]
			if child.kind == .select_stmt {
				g.gen_select(child_id, child, false)
				return
			}
			if g.is_runtime_array_flags_stmt(child_id) {
				return
			}
			if child.kind == .or_expr {
				g.gen_or_expr_stmt(child)
				return
			} else if g.gen_split_array_append_expr_stmt(child) {
				return
			} else if child.kind == .infix && child.op == .left_shift {
				lhs_id := g.a.child(&child, 0)
				if child.value == 'push_many' {
					g.gen_array_push_many_stmt(lhs_id, g.a.child(&child, 1))
				} else if child.value == 'push' {
					push_rhs_id := g.a.child(&child, 1)
					mut c_elem := if child.typ.len > 0 {
						g.tc.c_type(g.tc.parse_type(child.typ))
					} else {
						'string'
					}
					lhs_arr_type := types.unwrap_pointer(g.usable_expr_type(lhs_id))
					if lhs_arr := array_like_type(lhs_arr_type) {
						push_rhs_clean := types.unwrap_pointer(g.usable_expr_type(push_rhs_id))
						if rhs_arr := array_like_type(push_rhs_clean) {
							if g.tc.c_type(lhs_arr.elem_type) !in ['array', 'Array']
								&& g.tc.c_type(lhs_arr.elem_type) == g.tc.c_type(rhs_arr.elem_type) {
								g.gen_array_push_many_stmt(lhs_id, push_rhs_id)
								return
							}
						} else if rhs_fixed := array_fixed_type(push_rhs_clean) {
							if g.tc.c_type(lhs_arr.elem_type) !in ['array', 'Array']
								&& g.tc.c_type(lhs_arr.elem_type) == g.tc.c_type(rhs_fixed.elem_type) {
								g.gen_array_push_many_stmt(lhs_id, push_rhs_id)
								return
							}
						}
						c_elem = g.tc.c_type(lhs_arr.elem_type)
					}
					lhs_is_ptr := g.tc.resolve_type(lhs_id) is types.Pointer
					amp := if lhs_is_ptr { '' } else { '&' }
					g.write('array_push(${amp}')
					gen_expr_lvalue(mut g, lhs_id)
					g.write(', &(${c_elem}[]){')
					if lhs_arr := array_like_type(lhs_arr_type) {
						g.gen_expr_with_expected_type(push_rhs_id, lhs_arr.elem_type)
					} else {
						g.gen_expr(push_rhs_id)
					}
					g.writeln('});')
				} else {
					lhs_type := g.usable_expr_type(lhs_id)
					clean := types.unwrap_pointer(lhs_type)
					if lhs_arr := array_like_type(clean) {
						rhs_id := g.a.child(&child, 1)
						rhs_type := g.usable_expr_type(rhs_id)
						rhs_clean := types.unwrap_pointer(rhs_type)
						if rhs_arr := array_like_type(rhs_clean) {
							if g.tc.c_type(lhs_arr.elem_type) !in ['array', 'Array']
								&& g.tc.c_type(lhs_arr.elem_type) == g.tc.c_type(rhs_arr.elem_type) {
								g.gen_array_push_many_stmt(lhs_id, rhs_id)
							} else {
								c_elem := g.tc.c_type(lhs_arr.elem_type)
								lhs_is_ptr := g.tc.resolve_type(lhs_id) is types.Pointer
								amp := if lhs_is_ptr { '' } else { '&' }
								g.write('array_push(${amp}')
								gen_expr_lvalue(mut g, lhs_id)
								g.write(', &(${c_elem}[]){')
								g.gen_expr_with_expected_type(rhs_id, lhs_arr.elem_type)
								g.writeln('});')
							}
						} else if rhs_fixed := array_fixed_type(rhs_clean) {
							if g.tc.c_type(lhs_arr.elem_type) !in ['array', 'Array']
								&& g.tc.c_type(lhs_arr.elem_type) == g.tc.c_type(rhs_fixed.elem_type) {
								g.gen_array_push_many_stmt(lhs_id, rhs_id)
							} else {
								c_elem := g.tc.c_type(lhs_arr.elem_type)
								lhs_is_ptr := g.tc.resolve_type(lhs_id) is types.Pointer
								amp := if lhs_is_ptr { '' } else { '&' }
								g.write('array_push(${amp}')
								gen_expr_lvalue(mut g, lhs_id)
								g.write(', &(${c_elem}[]){')
								g.gen_expr_with_expected_type(rhs_id, lhs_arr.elem_type)
								g.writeln('});')
							}
						} else {
							c_elem := g.tc.c_type(lhs_arr.elem_type)
							lhs_is_ptr := g.tc.resolve_type(lhs_id) is types.Pointer
							amp := if lhs_is_ptr { '' } else { '&' }
							g.write('array_push(${amp}')
							gen_expr_lvalue(mut g, lhs_id)
							g.write(', &(${c_elem}[]){')
							g.gen_expr(rhs_id)
							g.writeln('});')
						}
					} else {
						g.gen_expr(child_id)
						g.writeln(';')
					}
				}
			} else {
				g.track_ierror_array_push_call_alias(child)
				g.gen_expr(child_id)
				g.writeln(';')
			}
		}
		.decl_assign {
			g.gen_decl_assign(node)
		}
		.assign, .selector_assign {
			g.gen_assign(node)
		}
		.index_assign {
			g.gen_index_assign(node)
		}
		.return_stmt {
			g.in_return = true
			old_return_node_id := g.cur_return_node_id
			old_return_drops := g.cur_return_drops.clone()
			g.cur_return_node_id = int(id)
			g.cur_return_drops = g.take_return_stmt_ownership_drops(node)
			defer {
				g.cur_return_node_id = old_return_node_id
				g.cur_return_drops = old_return_drops
			}
			if g.cur_fn_ret is types.Enum {
				g.expected_enum = g.cur_fn_ret.name
			}
			if node.children_count > 0
				&& (g.has_pending_defers() || g.active_locks.len > 0 || g.cur_return_drops.len > 0) {
				g.gen_return_with_defers(node)
				g.expected_enum = ''
				return
			}
			g.gen_return_cleanup()
			if node.children_count > 0 {
				ret_id := g.a.child(&node, 0)
				if int(ret_id) < 0 || int(ret_id) >= g.a.nodes.len {
					g.gen_default_return_stmt()
					g.expected_enum = ''
					return
				}
				if g.is_noreturn_call(ret_id) {
					g.gen_noreturn_return(ret_id)
					g.expected_enum = ''
					return
				}
				ret_node := g.a.nodes[int(ret_id)]
				if ret_node.kind == .call {
					fn_n := g.a.child_node(&ret_node, 0)
					if fn_n.value == 'error' || fn_n.value == 'error_with_code' {
						if g.cur_fn_ret_is_optional {
							ct := g.optional_type_name(g.cur_fn_ret)
							g.write('return ')
							g.gen_optional_error_from_call(ct, ret_node)
							g.writeln(';')
						} else {
							g.write('return ')
							g.gen_expr(ret_id)
							g.writeln(';')
						}
						return
					}
				}
				if g.cur_fn_ret_is_optional {
					ct := g.optional_type_name(g.cur_fn_ret)
					base := g.cur_fn_ret_base
					if g.expr_is_optional_literal(ret_id, g.cur_fn_ret) {
						g.write('return ')
						g.gen_expr(ret_id)
						g.writeln(';')
						return
					}
					if base is types.MultiReturn && node.children_count > 1 {
						base_ct := g.value_c_type(base)
						if g.return_children_all_none(node) {
							g.writeln('return (${ct}){.ok = false};')
							return
						}
						if g.multi_return_types_have_fixed_array(base.types) {
							g.write('return ({ ${ct} __opt = {.ok = true}; ')
							tmp := g.gen_multi_return_temp(base_ct, base.types, node)
							g.writeln('__opt.value = ${tmp}; __opt; });')
							return
						}
						g.write('return (${ct}){.ok = true, .value = (${base_ct}){')
						for i in 0 .. node.children_count {
							if i > 0 {
								g.write(', ')
							}
							child_id := g.a.child(&node, i)
							if i < base.types.len {
								g.gen_expr_with_expected_type(child_id, base.types[i])
							} else {
								g.gen_expr(child_id)
							}
						}
						g.writeln('}};')
						return
					}
					if base is types.MultiReturn {
						expr_type := g.usable_expr_type(ret_id)
						if ret_node.kind in [.block, .if_expr] || expr_type is types.MultiReturn {
							g.write('return (${ct}){.ok = true, .value = ')
							g.gen_expr_with_expected_type(ret_id, base)
							g.writeln('};')
							return
						}
					}
					if ret_node.kind == .none_expr {
						g.writeln('return (${ct}){.ok = false};')
						return
					}
					if base is types.Void {
						raw_expr_type := g.tc.resolve_type(ret_id)
						expr_type := g.usable_expr_type(ret_id)
						call_ret_type := g.local_fn_call_return_type(ret_id, ret_node)
						decl_ret_type := g.declared_call_return_type(ret_id)
						if g.optional_result_matches_base(raw_expr_type, base)
							|| g.optional_result_matches_base(expr_type, base)
							|| g.optional_result_matches_base(call_ret_type, base)
							|| g.optional_result_matches_base(decl_ret_type, base) {
							g.write('return ')
							g.gen_expr(ret_id)
							g.writeln(';')
							return
						}
						if g.cur_fn_ret is types.ResultType {
							if err := g.result_error_from_expr_string(ret_id) {
								g.writeln('return (${ct}){.ok = false, .err = ${err}};')
								return
							}
						}
						g.writeln('return (${ct}){.ok = false};')
					} else if fixed := array_fixed_type(base) {
						if g.cur_fn_ret is types.ResultType {
							if result_err := g.result_error_from_expr_string(ret_id) {
								g.writeln('return (${ct}){.ok = false, .err = ${result_err}};')
								return
							}
						}
						// The optional's `.value` is a fixed-array member, which can't be set
						// in the compound literal; build via a temp + memcpy.
						g.write('return ({ ${ct} __opt = {.ok = true}; memcpy(__opt.value, ')
						g.gen_fixed_array_copy_source(ret_id, types.Type(fixed))
						g.writeln(', sizeof(__opt.value)); __opt; });')
					} else {
						raw_expr_type := g.tc.resolve_type(ret_id)
						expr_type := g.usable_expr_type(ret_id)
						call_ret_type := g.local_fn_call_return_type(ret_id, ret_node)
						decl_ret_type := g.declared_call_return_type(ret_id)
						if g.optional_result_matches_base(raw_expr_type, base)
							|| g.optional_result_matches_base(expr_type, base)
							|| g.optional_result_matches_base(call_ret_type, base)
							|| g.optional_result_matches_base(decl_ret_type, base) {
							g.write('return ')
							g.gen_expr(ret_id)
							g.writeln(';')
						} else {
							mut expr_value_type := expr_type
							if expr_type is types.OptionType {
								expr_value_type = expr_type.base_type
							} else if expr_type is types.ResultType {
								expr_value_type = expr_type.base_type
							}
							base_ct := g.value_c_type(base)
							expr_ct := g.tc.c_type(expr_value_type)
							struct_init_ct := if ret_node.kind == .struct_init {
								g.struct_init_c_type_name(ret_node.value)
							} else {
								''
							}
							expr_value_name := expr_value_type.name()
							is_alias_value := expr_value_type is types.Alias
								|| expr_value_name in g.tc.type_aliases
								|| g.tc.qualify_name(expr_value_name) in g.tc.type_aliases
							pointer_value_expr := g.pointer_value_return_expr_string(ret_id, base) or {
								''
							}
							if expr_ct != base_ct && struct_init_ct != base_ct
								&& pointer_value_expr.len == 0 && !is_alias_value
								&& ret_node.kind !in [.cast_expr, .as_expr]
								&& !g.type_names_match(expr_value_type, base)
								&& !g.expr_is_nil_pointer_payload(ret_id, base)
								&& !g.type_can_wrap_as_sum(expr_value_type, base)
								&& !g.type_can_wrap_as_ierror_payload(expr_value_type, base)
								&& !g.types_numeric_compatible(expr_value_type, base)
								&& !g.array_abi_types_match(expr_value_type, base)
								&& !g.or_value_temp_matches_array_return(ret_node, base)
								&& !g.call_constructs_type(ret_id, base)
								&& !g.clone_call_matches_base(ret_node, base)
								&& expr_value_type !is types.Primitive
								&& expr_value_type !is types.Unknown {
								if g.cur_fn_ret is types.ResultType {
									if result_err := g.result_error_from_expr_string(ret_id) {
										g.writeln('return (${ct}){.ok = false, .err = ${result_err}};')
										return
									}
								}
								g.writeln('return (${ct}){.ok = false};')
							} else {
								g.write('return (${ct}){.ok = true, .value = ')
								if pointer_value_expr.len > 0 {
									g.write(pointer_value_expr)
								} else if !g.gen_heap_local_address_expr(ret_id, base) {
									g.gen_expr_with_expected_type(ret_id, base)
								}
								g.writeln('};')
							}
						}
					}
				} else if g.cur_fn_ret is types.MultiReturn {
					if node.children_count > 1 {
						ct := g.tc.c_type(g.cur_fn_ret)
						ret_types := g.cur_fn_ret.types
						if g.multi_return_types_have_fixed_array(ret_types) {
							g.gen_multi_return_temp_return(ct, ret_types, node)
						} else {
							g.write('return (${ct}){')
							for i in 0 .. node.children_count {
								if i > 0 {
									g.write(', ')
								}
								child_id := g.a.child(&node, i)
								if i < ret_types.len
									&& g.gen_pointer_value_return_expr(child_id, ret_types[i]) {
								} else if i < ret_types.len {
									g.gen_expr_with_expected_type(child_id, ret_types[i])
								} else {
									g.gen_expr(child_id)
								}
							}
							g.writeln('};')
						}
					} else {
						g.write('return ')
						g.gen_expr_with_expected_type(ret_id, g.cur_fn_ret)
						g.writeln(';')
					}
				} else if ret_node.kind == .assoc {
					g.gen_return_assoc(ret_node)
				} else if ret_fixed := array_fixed_type(g.cur_fn_ret) {
					if g.tc.c_type(ret_fixed) in g.fixed_array_ret_wrappers {
						g.write('return ')
						g.gen_fixed_array_return_wrap(ret_fixed, ret_id)
						g.writeln(';')
					} else {
						g.write('return ')
						g.gen_expr_with_expected_type(ret_id, g.cur_fn_ret)
						g.writeln(';')
					}
				} else {
					g.write('return ')
					// Most interface returns are already boxed by the transform pass into
					// a `(Iface){._typ = N, ._object = ...}` literal, in which case
					// gen_interface_value_expr is a no-op (the value is already an
					// interface) and we emit it directly. IError is intentionally left
					// unboxed by the transform, so box the concrete error here. Never emit
					// a zeroed `(Iface){0}` — that drops `_typ`/`_object` and makes every
					// dispatch through the returned interface panic as "not implemented".
					if g.cur_fn_ret is types.Interface {
						if !g.gen_interface_value_expr(ret_id, g.cur_fn_ret) {
							g.gen_expr(ret_id)
						}
					} else if g.gen_pointer_value_return_expr(ret_id, g.cur_fn_ret) {
					} else if !g.gen_heap_local_address_expr(ret_id, g.cur_fn_ret)
						&& !g.gen_sum_constructor_call_with_expected_type(ret_id, ret_node, g.cur_fn_ret) {
						g.gen_expr_with_expected_type(ret_id, g.cur_fn_ret)
					}
					g.writeln(';')
				}
			} else {
				g.gen_default_return_stmt()
			}
			g.expected_enum = ''
		}
		.defer_stmt {
			if node.value == 'function' {
				if count_name := g.fn_defer_counts[int(id)] {
					g.writeln('${count_name}++;')
				}
				g.fn_defers << id
			} else {
				g.defers << g.a.child(&node, 0)
			}
		}
		.for_stmt {
			g.gen_for(node)
		}
		.for_in_stmt {
			g.gen_for_in(node)
		}
		.lock_expr {
			g.gen_lock_stmt(id, node)
		}
		.select_stmt {
			g.gen_select(id, node, false)
		}
		.break_stmt {
			g.gen_branch_lock_cleanup(node.value)
			g.gen_loop_control_ownership_drops()
			if node.value.len > 0 {
				g.writeln('goto ${g.cname(node.value)}_break;')
			} else {
				g.writeln('break;')
			}
		}
		.continue_stmt {
			g.gen_branch_lock_cleanup(node.value)
			g.gen_loop_control_ownership_drops()
			if node.value.len > 0 {
				g.writeln('${g.labelled_continue_skip_drops_var(node.value)} = true;')
				g.writeln('goto ${g.cname(node.value)}_continue;')
			} else {
				g.writeln('continue;')
			}
		}
		.block {
			g.writeln('{')
			g.push_scope()
			defer_start := g.defers.len
			g.indent++
			for i in 0 .. node.children_count {
				g.gen_node(g.a.child(&node, i))
			}
			g.gen_defers_from(defer_start)
			g.gen_scope_ownership_drops()
			g.trim_defers(defer_start)
			g.indent--
			g.pop_scope()
			g.writeln('}')
		}
		.if_expr {
			g.gen_if(node)
		}
		.assert_stmt {
			g.write('if (!(')
			g.gen_expr(g.a.child(&node, 0))
			g.writeln(')) {')
			g.indent++
			g.writeln('v3_eprint_lit("assert failed\\n");')
			g.writeln('exit(1);')
			g.indent--
			g.writeln('}')
		}
		.goto_stmt {
			if g.gen_goto_lock_leaves(node.value) {
				g.writeln('goto ${g.cname(node.value)};')
			}
		}
		.label_stmt {
			if node.value.starts_with(pending_loop_label_marker) {
				g.pending_loop_label = node.value[pending_loop_label_marker.len..]
				return
			}
			old_indent := g.indent
			g.indent = 0
			g.writeln('${g.cname(node.value)}: ;')
			g.indent = old_indent
			g.pending_loop_label = node.value
		}
		.empty, .asm_stmt {}
		else {
			// NOTE: match_stmt is intentionally absent — the transformer lowers every
			// match into an if/else-if chain (see transform.lower_match_stmts), so the
			// backend never sees one. Match lowering lives in the transformer, not here.
			eprintln('gen_node: unsupported node kind: ${node.kind}')
		}
	}
}

// has_pending_defers reports whether has pending defers applies in c.
fn (g &FlatGen) has_pending_defers() bool {
	return g.defers.len > 0 || g.fn_defers.len > 0
}

fn (mut g FlatGen) gen_pointer_value_return_expr(ret_id flat.NodeId, expected types.Type) bool {
	return g.write_pointer_value_return_expr(ret_id, expected)
}

fn (mut g FlatGen) pointer_value_return_expr_string(ret_id flat.NodeId, expected types.Type) ?string {
	orig := g.sb
	orig_line_start := g.line_start
	g.sb = strings.new_builder(64)
	g.line_start = false
	if !g.write_pointer_value_return_expr(ret_id, expected) {
		g.sb = orig
		g.line_start = orig_line_start
		return none
	}
	result := g.sb.str()
	g.sb = orig
	g.line_start = orig_line_start
	return result
}

fn (mut g FlatGen) write_pointer_value_return_expr(ret_id flat.NodeId, expected types.Type) bool {
	source_id := g.pointer_value_return_source_id(ret_id)
	actual := g.usable_expr_type(source_id)
	expected0 := if expected is types.Alias { expected.base_type } else { expected }
	if expected0 is types.Pointer {
		return false
	}
	if actual is types.Pointer {
		if g.type_names_match(actual.base_type, expected0) {
			g.write('*(')
			g.gen_expr(source_id)
			g.write(')')
			return true
		}
	}
	if pointer_value_type_names_match(actual.name(), expected0.name()) {
		g.write('*(')
		g.gen_expr(source_id)
		g.write(')')
		return true
	}
	return false
}

fn (g &FlatGen) pointer_value_return_source_id(id flat.NodeId) flat.NodeId {
	if int(id) < 0 || int(id) >= g.a.nodes.len {
		return id
	}
	node := g.a.nodes[int(id)]
	match node.kind {
		.expr_stmt, .paren {
			if node.children_count > 0 {
				return g.pointer_value_return_source_id(g.a.child(&node, 0))
			}
		}
		.block {
			if node.children_count == 1 {
				return g.pointer_value_return_source_id(g.a.child(&node, 0))
			}
		}
		else {}
	}

	return id
}

fn pointer_value_type_names_match(actual string, expected string) bool {
	if !actual.starts_with('&') {
		return false
	}
	clean_actual := actual[1..]
	if clean_actual == expected {
		return true
	}
	return clean_actual.all_after_last('.') == expected.all_after_last('.')
}

fn (g &FlatGen) return_children_all_none(node flat.Node) bool {
	if node.children_count == 0 {
		return false
	}
	for i in 0 .. node.children_count {
		child_id := g.a.child(&node, i)
		if int(child_id) < 0 || int(child_id) >= g.a.nodes.len {
			return false
		}
		child := g.a.nodes[int(child_id)]
		if child.kind != .none_expr {
			return false
		}
	}
	return true
}

// gen_return_with_defers emits return with defers output for c.
fn (mut g FlatGen) gen_return_with_defers(node flat.Node) {
	ret_id := g.a.child(&node, 0)
	if int(ret_id) < 0 || int(ret_id) >= g.a.nodes.len {
		g.gen_return_cleanup()
		g.gen_default_return_stmt()
		return
	}
	if g.is_noreturn_call(ret_id) {
		g.gen_noreturn_return(ret_id)
		return
	}
	ret_node := g.a.nodes[int(ret_id)]
	if ret_node.kind == .assoc {
		tmp := g.tmp_name()
		g.gen_assoc_return_tmp(ret_node, tmp)
		g.gen_return_cleanup()
		g.writeln('return ${tmp};')
		return
	}
	if ret_fixed := array_fixed_type(g.cur_fn_ret) {
		if g.tc.c_type(ret_fixed) in g.fixed_array_ret_wrappers {
			wrapper := fixed_array_ret_wrapper_name(g.tc.c_type(ret_fixed))
			tmp := g.tmp_name()
			g.write('${wrapper} ${tmp} = ')
			g.gen_fixed_array_return_wrap(ret_fixed, ret_id)
			g.writeln(';')
			g.gen_return_cleanup()
			g.writeln('return ${tmp};')
			return
		}
	}
	ct := g.return_c_type()
	if g.cur_fn_ret is types.MultiReturn && node.children_count > 1 {
		ret_types := g.cur_fn_ret.types
		if g.multi_return_types_have_fixed_array(ret_types) {
			tmp := g.gen_multi_return_temp(ct, ret_types, node)
			g.gen_return_cleanup()
			g.writeln('return ${tmp};')
			return
		}
	}
	expr := g.return_expr_string(node, ret_id, ret_node, ct)
	tmp := g.tmp_name()
	g.writeln('${ct} ${tmp} = ${expr};')
	g.gen_return_cleanup()
	g.writeln('return ${tmp};')
}

// gen_fixed_array_return_wrap emits a fixed-array return value wrapped in its
// return-wrapper struct: `({ Wrapper __fa_ret; memcpy(__fa_ret.ret_arr, <expr>,
// sizeof(...)); __fa_ret; })`. C cannot return raw arrays, so the array is copied
// into the wrapper's `ret_arr` field and the struct is returned by value.
fn (mut g FlatGen) gen_fixed_array_return_wrap(ret_type types.Type, ret_id flat.NodeId) {
	wrapper := fixed_array_ret_wrapper_name(g.tc.c_type(ret_type))
	g.write('({ ${wrapper} __fa_ret; memcpy(__fa_ret.ret_arr, ')
	g.gen_fixed_array_copy_source(ret_id, ret_type)
	g.write(', sizeof(__fa_ret.ret_arr)); __fa_ret; })')
}

fn (mut g FlatGen) gen_noreturn_return(ret_id flat.NodeId) {
	g.gen_expr(ret_id)
	g.writeln(';')
	g.gen_noreturn_default_return_stmt()
}

fn (mut g FlatGen) gen_noreturn_default_return_stmt() {
	if g.cur_fn_ret_is_optional || g.cur_fn_name == 'main' || g.cur_fn_ret is types.Void {
		g.gen_default_return_stmt()
		return
	}
	value_ct := g.optional_type_name(g.cur_fn_ret)
	abi_ct := g.fn_return_type_name(g.cur_fn_ret)
	if abi_ct == value_ct {
		g.gen_default_return_stmt()
		return
	}
	if value_ct.starts_with('fn_ptr:') {
		g.writeln('return (${abi_ct})0;')
		return
	}
	g.writeln('return (${abi_ct}){0};')
}

fn (mut g FlatGen) gen_default_return_stmt() {
	if g.cur_fn_ret_is_optional {
		ct := g.optional_type_name(g.cur_fn_ret)
		g.writeln('return (${ct}){.ok = true};')
	} else if g.cur_fn_name == 'main' {
		g.writeln('return 0;')
	} else if g.cur_fn_ret is types.Void {
		g.writeln('return;')
	} else {
		g.write('return ')
		g.gen_default_value_for_type(g.cur_fn_ret)
		g.writeln(';')
	}
}

// return_c_type supports return c type handling for FlatGen.
fn (mut g FlatGen) return_c_type() string {
	if g.cur_fn_ret_is_optional {
		return g.optional_type_name(g.cur_fn_ret)
	}
	if g.cur_fn_ret is types.MultiReturn {
		return g.value_c_type(g.cur_fn_ret)
	}
	return g.tc.c_type(g.cur_fn_ret)
}

// local_ident_type returns the type of an identifier that is local to the
// currently emitted function body.
fn (g &FlatGen) local_ident_type(name string) ?types.Type {
	if typ := g.current_param_type(name) {
		return typ
	}
	if typ := g.current_param_map_type(name) {
		return typ
	}
	if g.cur_scope_has_local_name(name) {
		if typ := g.tc.cur_scope.lookup(name) {
			if typ !is types.Void {
				return typ
			}
		}
	}
	return none
}

// heap_local_address_expr returns a heap-copy expression for `&local` when the
// surrounding return type is a pointer. V permits local address escapes; C needs
// the local value copied out of the stack frame before returning.
fn (mut g FlatGen) heap_local_address_expr(ret_id flat.NodeId, expected types.Type) ?string {
	if int(ret_id) < 0 || int(ret_id) >= g.a.nodes.len {
		return none
	}
	node := g.a.nodes[int(ret_id)]
	if node.kind != .prefix || node.op != .amp || node.children_count == 0 {
		return none
	}
	if expected !is types.Pointer {
		return none
	}
	ptr := expected as types.Pointer
	child_id := g.a.child(&node, 0)
	child := g.a.nodes[int(child_id)]
	if child.kind != .ident || child.value.len == 0 {
		return none
	}
	local_type := g.local_ident_type(child.value) or { return none }
	base_type := ptr.base_type
	local_ct := g.tc.c_type(local_type)
	base_ct := g.tc.c_type(base_type)
	if base_ct.len == 0 || base_ct == 'void' {
		return none
	}
	if local_ct != base_ct && !g.type_names_match(local_type, base_type) {
		return none
	}
	local_expr := g.expr_to_string(child_id)
	if g.current_param_is_mut(child.value) {
		return local_expr
	}
	return '(${base_ct}*)memdup(&${local_expr}, sizeof(${base_ct}))'
}

fn (mut g FlatGen) gen_heap_local_address_expr(ret_id flat.NodeId, expected types.Type) bool {
	if expr := g.heap_local_address_expr(ret_id, expected) {
		g.write(expr)
		return true
	}
	return false
}

// return_expr_string supports return expr string handling for FlatGen.
fn (mut g FlatGen) return_expr_string(node flat.Node, ret_id flat.NodeId, ret_node flat.Node, ct string) string {
	if ret_node.kind == .call {
		fn_n := g.a.child_node(&ret_node, 0)
		if fn_n.value == 'error' || fn_n.value == 'error_with_code' {
			if g.cur_fn_ret_is_optional {
				return g.optional_error_from_call_string(ct, ret_node)
			}
			return g.expr_to_string(ret_id)
		}
	}
	if g.cur_fn_ret_is_optional {
		base := g.cur_fn_ret_base
		if g.expr_is_optional_literal(ret_id, g.cur_fn_ret) {
			return g.expr_to_string(ret_id)
		}
		if base is types.MultiReturn && node.children_count > 1 {
			base_ct := g.value_c_type(base)
			if g.return_children_all_none(node) {
				return '(${ct}){.ok = false}'
			}
			mut parts := []string{cap: int(node.children_count)}
			for i in 0 .. node.children_count {
				child_id := g.a.child(&node, i)
				if i < base.types.len {
					parts << g.expr_to_string_with_expected_type(child_id, base.types[i])
				} else {
					parts << g.expr_to_string(child_id)
				}
			}
			return '(${ct}){.ok = true, .value = (${base_ct}){${parts.join(', ')}}}'
		}
		if base is types.MultiReturn {
			expr_type := g.usable_expr_type(ret_id)
			if ret_node.kind in [.block, .if_expr] || expr_type is types.MultiReturn {
				return '(${ct}){.ok = true, .value = ${g.expr_to_string_with_expected_type(ret_id,
					base)}}'
			}
		}
		if ret_node.kind == .none_expr {
			return '(${ct}){.ok = false}'
		}
		if base is types.Void {
			raw_expr_type := g.tc.resolve_type(ret_id)
			expr_type := g.usable_expr_type(ret_id)
			call_ret_type := g.local_fn_call_return_type(ret_id, ret_node)
			decl_ret_type := g.declared_call_return_type(ret_id)
			if g.optional_result_matches_base(raw_expr_type, base)
				|| g.optional_result_matches_base(expr_type, base)
				|| g.optional_result_matches_base(call_ret_type, base)
				|| g.optional_result_matches_base(decl_ret_type, base) {
				return g.expr_to_string(ret_id)
			}
			if g.cur_fn_ret is types.ResultType {
				if err := g.result_error_from_expr_string(ret_id) {
					return '(${ct}){.ok = false, .err = ${err}}'
				}
			}
			return '(${ct}){.ok = false}'
		}
		if _ := array_fixed_type(base) {
			// The optional's `.value` is a fixed-array member, which can't be set in a compound
			// literal; build via a temp + memcpy (mirrors the direct return path) so a deferred
			// return saves the array value instead of dropping it to `{.ok = false}`.
			src := g.fixed_array_copy_source_string(ret_id, base)
			return '({ ${ct} __opt = {.ok = true}; memcpy(__opt.value, ${src}, sizeof(__opt.value)); __opt; })'
		}
		raw_expr_type := g.tc.resolve_type(ret_id)
		expr_type := g.usable_expr_type(ret_id)
		call_ret_type := g.local_fn_call_return_type(ret_id, ret_node)
		decl_ret_type := g.declared_call_return_type(ret_id)
		if g.optional_result_matches_base(raw_expr_type, base)
			|| g.optional_result_matches_base(expr_type, base)
			|| g.optional_result_matches_base(call_ret_type, base)
			|| g.optional_result_matches_base(decl_ret_type, base) {
			return g.expr_to_string(ret_id)
		}
		mut expr_value_type := expr_type
		if expr_type is types.OptionType {
			expr_value_type = expr_type.base_type
		} else if expr_type is types.ResultType {
			expr_value_type = expr_type.base_type
		}
		base_ct := g.value_c_type(base)
		expr_ct := g.tc.c_type(expr_value_type)
		struct_init_ct := if ret_node.kind == .struct_init {
			g.struct_init_c_type_name(ret_node.value)
		} else {
			''
		}
		pointer_value_expr := g.pointer_value_return_expr_string(ret_id, base) or { '' }
		if expr_ct != base_ct && struct_init_ct != base_ct && pointer_value_expr.len == 0
			&& !g.type_names_match(expr_value_type, base)
			&& !g.expr_is_nil_pointer_payload(ret_id, base)
			&& !g.type_can_wrap_as_sum(expr_value_type, base)
			&& !g.type_can_wrap_as_ierror_payload(expr_value_type, base)
			&& !g.types_numeric_compatible(expr_value_type, base)
			&& !g.array_abi_types_match(expr_value_type, base)
			&& !g.or_value_temp_matches_array_return(ret_node, base)
			&& !g.call_constructs_type(ret_id, base) && !g.clone_call_matches_base(ret_node, base)
			&& expr_value_type !is types.Primitive && expr_value_type !is types.Unknown {
			if g.cur_fn_ret is types.ResultType {
				if err := g.result_error_from_expr_string(ret_id) {
					return '(${ct}){.ok = false, .err = ${err}}'
				}
			}
			return '(${ct}){.ok = false}'
		}
		if pointer_value_expr.len > 0 {
			return '(${ct}){.ok = true, .value = ${pointer_value_expr}}'
		}
		value := g.heap_local_address_expr(ret_id, base) or {
			g.expr_to_string_with_expected_type(ret_id, base)
		}
		return '(${ct}){.ok = true, .value = ${value}}'
	}
	if g.cur_fn_ret is types.MultiReturn {
		if node.children_count > 1 {
			ret_types := g.cur_fn_ret.types
			mut parts := []string{cap: int(node.children_count)}
			for i in 0 .. node.children_count {
				child_id := g.a.child(&node, i)
				if i < ret_types.len {
					if expr := g.pointer_value_return_expr_string(child_id, ret_types[i]) {
						parts << expr
					} else {
						parts << g.expr_to_string_with_expected_type(child_id, ret_types[i])
					}
				} else {
					parts << g.expr_to_string(child_id)
				}
			}
			return '(${ct}){${parts.join(', ')}}'
		}
		return g.expr_to_string_with_expected_type(ret_id, g.cur_fn_ret)
	}
	if g.cur_fn_ret is types.Interface {
		// Box the concrete value the same way the direct return path does, so a deferred return
		// preserves `_typ`/`_object` instead of zeroing the interface.
		return g.interface_value_to_string(ret_id, g.cur_fn_ret)
	}
	if expr := g.heap_local_address_expr(ret_id, g.cur_fn_ret) {
		return expr
	}
	if expr := g.sum_constructor_return_expr_string(ret_id, ret_node, g.cur_fn_ret) {
		return expr
	}
	return g.expr_to_string_with_expected_type(ret_id, g.cur_fn_ret)
}

fn (mut g FlatGen) sum_constructor_return_expr_string(ret_id flat.NodeId, ret_node flat.Node, expected types.Type) ?string {
	sum_type0 := if expected is types.Alias { expected.base_type } else { expected }
	if sum_type0 !is types.SumType || ret_node.kind != .call || ret_node.children_count < 2 {
		return none
	}
	sum_type := sum_type0 as types.SumType
	callee := g.a.child_node(&ret_node, 0)
	if !g.call_callee_names_sum_base(callee, sum_type.name) {
		return none
	}
	return g.expr_to_string_with_expected_type(ret_id, expected)
}

fn (g &FlatGen) type_can_return_as_ierror(typ types.Type) bool {
	clean := types.unwrap_pointer(typ)
	if clean is types.Alias {
		return g.type_can_return_as_ierror(clean.base_type)
	}
	if clean is types.Interface {
		return g.is_ierror_type_name(clean.name)
	}
	if clean is types.Struct {
		if g.is_ierror_type_name(clean.name) {
			return true
		}
		if g.tc.named_type_implements_interface(clean.name, 'IError') {
			return true
		}
		return g.struct_type_embeds_error(clean.name)
	}
	return false
}

fn (g &FlatGen) struct_type_embeds_error(type_name string) bool {
	if type_name == 'Error' || type_name.ends_with('.Error') {
		return true
	}
	for field in g.struct_embedded_fields(type_name) {
		embedded_type_name := g.embedded_field_type_name(field)
		if embedded_type_name.len == 0 {
			continue
		}
		if g.struct_type_embeds_error(embedded_type_name) {
			return true
		}
	}
	return false
}

fn (mut g FlatGen) result_error_from_expr_string(id flat.NodeId) ?string {
	if err := g.ierror_from_expr_string(id) {
		return err
	}
	expr_type := g.usable_expr_type(id)
	mut expr_value_type := expr_type
	if expr_type is types.OptionType {
		expr_value_type = expr_type.base_type
	} else if expr_type is types.ResultType {
		expr_value_type = expr_type.base_type
	}
	if g.is_ierror_type_name(expr_value_type.name()) {
		return g.expr_to_string(id)
	}
	if !g.type_can_return_as_ierror(expr_value_type) {
		return none
	}
	iface := g.ierror_interface_name() or { return none }
	return g.interface_value_to_string(id, types.Type(types.Interface{
		name: iface
	}))
}

fn (g &FlatGen) local_fn_call_return_type(call_id flat.NodeId, call_node flat.Node) types.Type {
	if call_node.kind != .call || call_node.children_count == 0 {
		return types.Type(types.void_)
	}
	mut node_type := types.Type(types.void_)
	// Monomorphization rewrites a generic call and records its concrete return
	// on the call node. The checker resolution still names the open template,
	// so prefer the rewritten annotation over that stale `!T`/`?T` signature.
	if call_node.typ.len > 0 {
		node_type = g.tc.parse_type(call_node.typ)
		if node_type is types.OptionType || node_type is types.ResultType {
			return node_type
		}
	}
	if name := g.tc.resolved_call_name(call_id) {
		if ret := g.tc.fn_ret_types[name] {
			return ret
		}
	}
	fn_node := g.a.child_node(&call_node, 0)
	if fn_node.kind == .selector {
		if ret := g.selector_call_return_type(fn_node) {
			return ret
		}
		return node_type
	}
	if fn_node.kind != .ident {
		return node_type
	}
	if ret := g.tc.fn_ret_types[fn_node.value] {
		return ret
	}
	cfn := g.cname(fn_node.value)
	if cfn != fn_node.value {
		if ret := g.tc.fn_ret_types[cfn] {
			return ret
		}
	}
	if ret := g.fn_decl_return_type_for_call_name(fn_node.value) {
		return ret
	}
	if typ := g.tc.cur_scope.lookup(fn_node.value) {
		return fn_type_return_type(typ)
	}
	typ := g.tc.resolve_type(g.a.child(&call_node, 0))
	ret_type := fn_type_return_type(typ)
	if ret_type !is types.Void {
		return ret_type
	}
	return node_type
}

// declared_call_return_type returns the *declared* return type of a (possibly
// lowered) call's target function, preserving type aliases. Method calls are
// lowered to ident calls (`Recv__method(recv, ...)`) before codegen, and the
// call node's own `.typ` annotation has aliases resolved away (e.g. `?NodeId`
// becomes `?int`), which makes the optional C type name appear to differ from
// the callee's signature. The declared type read from `fn_ret_types`/the fn decl
// keeps the alias, so propagating `return call()` is recognised as valid.
fn (g &FlatGen) declared_call_return_type(call_id flat.NodeId) types.Type {
	if int(call_id) < 0 {
		return types.Type(types.void_)
	}
	call_node := g.a.nodes[int(call_id)]
	if call_node.kind != .call || call_node.children_count == 0 {
		return types.Type(types.void_)
	}
	fn_node := g.a.child_node(&call_node, 0)
	if fn_node.kind == .selector {
		if ret := g.selector_call_return_type(fn_node) {
			return ret
		}
	} else if fn_node.kind == .ident {
		if ret := g.tc.fn_ret_types[fn_node.value] {
			return ret
		}
		cfn := g.cname(fn_node.value)
		if cfn != fn_node.value {
			if ret := g.tc.fn_ret_types[cfn] {
				return ret
			}
		}
		if ret := g.fn_decl_return_type_for_call_name(fn_node.value) {
			return ret
		}
	}
	// Indirect call through an fn-pointer value (local var, param, or struct field
	// like `h.f()`): the target isn't a declared function/method, so resolve its type
	// and read the fn type's return. Unwrap alias (`type MakeArr = fn () [2]string`)
	// and pointer layers first. Lets fixed-array-returning callbacks unwrap `.ret_arr`
	// at the call site whether reached through a local, a param, or a struct field.
	mut callee_t := types.unwrap_pointer(g.tc.resolve_type(g.a.child(&call_node, 0)))
	for callee_t is types.Alias {
		callee_t = types.unwrap_pointer((callee_t as types.Alias).base_type)
	}
	if callee_t is types.FnType {
		return callee_t.return_type
	}
	return types.Type(types.void_)
}

fn (g &FlatGen) selector_call_return_type(fn_node flat.Node) ?types.Type {
	if fn_node.children_count == 0 || fn_node.value.len == 0 {
		return none
	}
	base_id := g.a.child(&fn_node, 0)
	base_node := g.a.nodes[int(base_id)]
	// A selector whose base names a type or an imported module is not a receiver method but a
	// static method (`Type.make()`) or import-qualified function (`mod.make()`); the base ident
	// has no value type, so resolve it the same way gen_call does and read the declared return
	// type. Without this a fixed-array such call's `_v_ret_*` wrapper is never unwrapped.
	if base_node.kind == .ident && base_node.value.len > 0 {
		base_is_local := g.tc.cur_scope.lookup(base_node.value) or { types.Type(types.void_) } !is types.Void
		if !base_is_local {
			if static_fn := g.static_method_fn_name(base_node.value, fn_node.value) {
				if ret := g.tc.fn_ret_types[static_fn] {
					return ret
				}
			}
			if mod := g.import_alias_module(base_node.value) {
				if ret := g.tc.fn_ret_types['${mod}.${fn_node.value}'] {
					return ret
				}
			}
		}
	}
	base_type0 := g.usable_expr_type(base_id)
	base_type := if base_type0 is types.Unknown || base_type0 is types.Void {
		g.tc.resolve_type(base_id)
	} else {
		base_type0
	}
	clean_type := types.unwrap_pointer(base_type)
	if fn_node.value == 'clone' && (clean_type is types.Array || clean_type is types.Map) {
		return base_type
	}
	mut receiver_name := clean_type.name()
	if clean_type is types.Struct {
		receiver_name = clean_type.name
	} else if clean_type is types.Interface {
		receiver_name = clean_type.name
	} else if clean_type is types.Alias {
		receiver_name = clean_type.name
	}
	if receiver_name.len == 0 {
		return none
	}
	if decl_key := g.interface_method_signature_key(receiver_name, fn_node.value) {
		if ret := g.tc.fn_ret_types[decl_key] {
			return ret
		}
	}
	method_name := '${receiver_name}.${fn_node.value}'
	if ret := g.tc.fn_ret_types[method_name] {
		return ret
	}
	if receiver_name.contains('.') {
		short_name := receiver_name.all_after_last('.')
		short_method := '${short_name}.${fn_node.value}'
		if ret := g.tc.fn_ret_types[short_method] {
			return ret
		}
	}
	return none
}

fn (g &FlatGen) fn_decl_return_type_for_call_name(name string) ?types.Type {
	if name.len == 0 {
		return none
	}
	// Indexed in collect_gen_info (register_fn_decl_ret_type); previously this scanned
	// every AST node per call (O(n^2)) and re-mangled each decl name with c_name.
	if rt := g.fn_decl_ret_types[name] {
		return rt
	}
	cname := g.cname(name)
	if cname != name {
		if rt := g.fn_decl_ret_types[cname] {
			return rt
		}
	}
	return none
}

fn fn_type_return_type(typ types.Type) types.Type {
	if typ is types.FnType {
		return typ.return_type
	}
	if typ is types.Alias {
		return fn_type_return_type(typ.base_type)
	}
	return types.Type(types.void_)
}

// optional_error_from_call_string converts optional error from call string data for c.
fn (mut g FlatGen) optional_error_from_call_string(ct string, node flat.Node) string {
	orig := g.sb
	orig_line_start := g.line_start
	g.sb = strings.new_builder(64)
	g.line_start = true
	g.gen_optional_error_from_call(ct, node)
	result := g.sb.str()
	g.sb = orig
	g.line_start = orig_line_start
	return result
}

// expr_really_returns_optional supports expr really returns optional handling for FlatGen.
fn (g &FlatGen) expr_really_returns_optional(id flat.NodeId) bool {
	if int(id) < 0 {
		return false
	}
	node := g.a.nodes[int(id)]
	if node.kind == .none_expr {
		return true
	}
	if node.kind == .call {
		if fname := g.tc.resolved_call_name(id) {
			ret_type := g.tc.fn_ret_types[fname] or { return false }
			return ret_type is types.OptionType || ret_type is types.ResultType
		}
	}
	return false
}

// optional_result_matches_base supports optional result matches base handling for FlatGen.
fn (g &FlatGen) optional_result_matches_base(expr_type types.Type, base types.Type) bool {
	mut inner := types.Type(types.void_)
	if expr_type is types.OptionType {
		inner = expr_type.base_type
	} else if expr_type is types.ResultType {
		inner = expr_type.base_type
	} else {
		return false
	}
	if g.type_names_match(inner, base) {
		return true
	}
	// Aliases keep their declared name (e.g. `[]NodeId`) while `resolve_type` collapses
	// them to the underlying type (`[]int`), so a structural name comparison spuriously
	// fails. What actually matters for `return <call>;` is whether the C optional type
	// emitted for the call equals the one this function returns — compare those instead.
	return g.option_c_name_for_base(inner) == g.option_c_name_for_base(base)
}

fn (g &FlatGen) clone_call_matches_base(call_node flat.Node, base types.Type) bool {
	mut node := call_node
	for node.kind in [.expr_stmt, .paren] && node.children_count > 0 {
		node = *g.a.child_node(&node, 0)
	}
	if node.kind != .call || node.children_count == 0 {
		return false
	}
	fn_node := g.a.child_node(&node, 0)
	if fn_node.kind == .ident {
		base_ct := g.tc.c_type(types.unwrap_pointer(base))
		return (fn_node.value == 'array__clone' && base_ct == 'Array')
			|| (fn_node.value == 'map__clone' && base_ct == 'map')
	}
	if fn_node.kind != .selector || fn_node.value != 'clone' || fn_node.children_count == 0 {
		return false
	}
	base_id := g.a.child(fn_node, 0)
	receiver_type0 := g.usable_expr_type(base_id)
	receiver_type := if receiver_type0 is types.Unknown || receiver_type0 is types.Void {
		g.tc.resolve_type(base_id)
	} else {
		receiver_type0
	}
	clean_receiver := types.unwrap_pointer(receiver_type)
	clean_base := types.unwrap_pointer(base)
	if g.type_names_match(clean_receiver, clean_base) {
		return true
	}
	receiver_ct0 := g.tc.c_type(clean_receiver)
	base_ct0 := g.tc.c_type(clean_base)
	if receiver_ct0.len > 0 && base_ct0.len > 0 && receiver_ct0 == base_ct0 {
		return true
	}
	receiver := if clean_receiver is types.Alias {
		clean_receiver.base_type
	} else {
		clean_receiver
	}
	expected := if clean_base is types.Alias {
		clean_base.base_type
	} else {
		clean_base
	}
	if expected is types.Array || expected is types.Map {
		receiver_ct := g.tc.c_type(receiver)
		expected_ct := g.tc.c_type(expected)
		if receiver_ct.len > 0 && receiver_ct == expected_ct {
			return true
		}
	}
	if receiver is types.Array && expected is types.Array {
		return g.type_names_match(receiver.elem_type, expected.elem_type)
	}
	if receiver is types.Map && expected is types.Map {
		return g.type_names_match(receiver.key_type, expected.key_type)
			&& g.type_names_match(receiver.value_type, expected.value_type)
	}
	return false
}

// option_c_name_for_base returns the C optional type name used for a `?base`/`!base`
// value, mirroring optional_type_name without its side effects.
fn (g &FlatGen) option_c_name_for_base(base types.Type) string {
	if base is types.Void {
		return 'Optional'
	}
	inner_ct := g.tc.c_type(base)
	if inner_ct == 'int' {
		return 'Optional'
	}
	return 'Optional_' + inner_ct.replace('*', 'ptr').replace(' ', '_')
}

fn (g &FlatGen) expr_is_nil_pointer_payload(id flat.NodeId, base types.Type) bool {
	if !g.type_accepts_nil_pointer(base) {
		return false
	}
	return g.expr_is_nil_value(id)
}

fn (g &FlatGen) type_accepts_nil_pointer(typ types.Type) bool {
	if typ is types.Pointer {
		return true
	}
	if typ is types.Alias {
		return g.type_accepts_nil_pointer(typ.base_type)
	}
	return false
}

fn (g &FlatGen) expr_is_nil_value(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= g.a.nodes.len {
		return false
	}
	node := g.a.nodes[int(id)]
	match node.kind {
		.nil_literal {
			return true
		}
		.expr_stmt {
			if node.children_count == 0 {
				return false
			}
			return g.expr_is_nil_value(g.a.child(&node, 0))
		}
		.block {
			if node.children_count == 0 {
				return false
			}
			return g.expr_is_nil_value(g.a.child(&node, node.children_count - 1))
		}
		.cast_expr, .as_expr {
			if node.children_count == 0 {
				return false
			}
			return g.expr_is_nil_value(g.a.child(&node, 0))
		}
		else {
			return false
		}
	}
}

// usable_expr_type supports usable expr type handling for FlatGen.
fn (g &FlatGen) usable_expr_type(id flat.NodeId) types.Type {
	if int(id) >= 0 && int(id) < g.a.nodes.len {
		node := g.a.nodes[int(id)]
		if node.kind in [.expr_stmt, .paren] && node.children_count > 0 {
			return g.usable_expr_type(g.a.child(&node, 0))
		}
		if node.kind == .block && node.children_count > 0 {
			return g.usable_expr_type(g.a.child(&node, node.children_count - 1))
		}
		if node.kind == .lock_expr {
			return g.tc.expr_type(id) or { g.lock_expr_result_type(node) }
		}
		if node.kind in [.as_expr, .cast_expr] && node.value.len > 0 {
			target_type := g.tc.parse_type(node.value)
			if !decl_annotation_is_unusable(target_type, node.value) {
				return target_type
			}
		}
		if node.kind == .ident {
			if typ := g.current_param_map_type(node.value) {
				return typ
			}
			if typ := g.tc.cur_scope.lookup(node.value) {
				if typ !is types.Void {
					return typ
				}
			}
			if typ := g.global_type_for_ident(node.value) {
				return typ
			}
			if typ := g.const_ident_type(node.value) {
				return typ
			}
		}
		if node.kind == .selector && node.children_count > 0 {
			base_type0 := g.usable_expr_type(g.a.child(&node, 0))
			base_type := types.unwrap_pointer(base_type0)
			if base_type is types.Struct {
				if typ := g.usable_struct_field_type(base_type.name, node.value) {
					return typ
				}
				if typ := g.checker_struct_field_type(base_type.name, node.value) {
					return typ
				}
			}
			if base_type is types.Alias {
				if base_type.base_type is types.Struct {
					if typ := g.usable_struct_field_type(base_type.base_type.name, node.value) {
						return typ
					}
					if typ := g.checker_struct_field_type(base_type.base_type.name, node.value) {
						return typ
					}
				}
			}
			if typ := g.sum_shared_field_type(base_type0, node.value) {
				return typ
			}
		}
		if node.kind == .index && node.children_count > 0 {
			base_type0 := g.usable_expr_type(g.a.child(&node, 0))
			base_type := types.unwrap_pointer(base_type0)
			is_slice := node.value == 'range'
				|| (node.children_count > 1 && g.a.child_node(&node, 1).kind == .range)
			if is_slice {
				if base_type is types.Array {
					return base_type
				}
				if base_type is types.ArrayFixed {
					return types.Type(types.Array{
						elem_type: base_type.elem_type
					})
				}
				if base_type is types.String {
					return types.Type(types.String{})
				}
			}
			if base_type is types.Array {
				return base_type.elem_type
			}
			if base_type is types.ArrayFixed {
				return base_type.elem_type
			}
			if base_type is types.Map {
				return base_type.value_type
			}
			if base_type is types.String {
				return types.Type(types.u8_)
			}
		}
		if node.kind == .call && node.children_count > 0 {
			if node.typ.len > 0 {
				node_type := g.tc.parse_type(node.typ)
				if !decl_annotation_is_unusable(node_type, node.typ) {
					return node_type
				}
			}
			fn_node := g.a.child_node(&node, 0)
			if node.typ.len > 0 && node.typ !in ['int', 'array', 'map', 'unknown'] {
				typ := g.tc.parse_type(node.typ)
				if !decl_annotation_is_unusable(typ, node.typ) && typ !is types.Unknown
					&& typ !is types.Void {
					return typ
				}
			}
			if map_return_type := g.array_map_call_return_type(node, fn_node) {
				return map_return_type
			}
			if arr_return_type := g.array_method_call_return_type(fn_node) {
				return arr_return_type
			}
			if fn_node.kind == .ident {
				if typ := g.tc.cur_scope.lookup(fn_node.value) {
					ret := fn_type_return_type(typ)
					if ret !is types.Unknown && ret !is types.Void {
						return ret
					}
				}
				if ret := g.fn_decl_return_type_for_call_name(fn_node.value) {
					if ret !is types.Unknown && ret !is types.Void {
						return ret
					}
				}
			}
		}
	}
	if typ := g.tc.expr_type(id) {
		if typ !is types.Unknown && typ !is types.Void {
			return typ
		}
	}
	return g.tc.resolve_type(id)
}

fn (g &FlatGen) array_method_call_return_type(fn_node &flat.Node) ?types.Type {
	if fn_node.kind != .selector || fn_node.children_count == 0 {
		return none
	}
	receiver_type := types.unwrap_pointer(g.usable_expr_type(g.a.child(fn_node, 0)))
	receiver_arr := array_like_type(receiver_type) or { return none }
	match fn_node.value {
		'first', 'last', 'pop', 'pop_left' {
			return receiver_arr.elem_type
		}
		'filter', 'clone', 'reverse', 'sort', 'sorted', 'repeat', 'repeat_to_depth' {
			return types.Type(receiver_arr)
		}
		else {
			return none
		}
	}
}

fn (g &FlatGen) array_map_call_return_type(node flat.Node, fn_node &flat.Node) ?types.Type {
	if fn_node.kind != .selector || fn_node.value != 'map' || fn_node.children_count == 0
		|| node.children_count < 2 {
		return none
	}
	receiver_type := types.unwrap_pointer(g.usable_expr_type(g.a.child(fn_node, 0)))
	if _ := array_like_type(receiver_type) {
		map_expr_id := g.a.child(&node, 1)
		elem_type := g.usable_expr_type(map_expr_id)
		if elem_type !is types.Unknown && elem_type !is types.Void {
			return types.Type(types.Array{
				elem_type: elem_type
			})
		}
	}
	return none
}

// type_names_match returns type names match data for FlatGen.
fn (g &FlatGen) type_names_match(a types.Type, b types.Type) bool {
	a_name := a.name()
	b_name := b.name()
	if a_name.len == 0 || b_name.len == 0 {
		return false
	}
	if a_name == b_name {
		return true
	}
	return a_name.all_after_last('.') == b_name.all_after_last('.')
}

fn (g &FlatGen) array_abi_types_match(a types.Type, b types.Type) bool {
	a0 := if a is types.Alias { a.base_type } else { a }
	b0 := if b is types.Alias { b.base_type } else { b }
	if a0 is types.Array && b0 is types.Array {
		return true
	}
	if a0 is types.Array {
		return generated_array_type_name(b0.name())
	}
	if b0 is types.Array {
		return generated_array_type_name(a0.name())
	}
	return false
}

fn generated_array_type_name(name string) bool {
	clean := name.all_after_last('.')
	return clean.starts_with('Array_') && !clean.starts_with('Array_fixed_')
}

fn (g &FlatGen) or_value_temp_matches_array_return(node flat.Node, expected types.Type) bool {
	expected0 := if expected is types.Alias { expected.base_type } else { expected }
	return expected0 is types.Array && node.kind == .ident && node.value.starts_with('__or_val_')
}

// type_can_wrap_as_sum returns type can wrap as sum data for FlatGen.
fn (g &FlatGen) type_can_wrap_as_sum(actual types.Type, expected types.Type) bool {
	expected0 := if expected is types.Alias { expected.base_type } else { expected }
	if expected0 !is types.SumType {
		return false
	}
	actual0 := if actual is types.Alias { actual.base_type } else { actual }
	if actual0 is types.SumType {
		return false
	}
	sum_type := expected0 as types.SumType
	sum_name := g.resolve_sum_name(sum_type.name)
	variant := g.resolve_variant(sum_name, actual0.name())
	variants := g.tc.sum_types[sum_name] or { return false }
	return variant in variants
}

fn (g &FlatGen) type_can_wrap_as_ierror_payload(actual types.Type, expected types.Type) bool {
	expected0 := if expected is types.Alias { expected.base_type } else { expected }
	if expected0 !is types.Interface || !g.is_ierror_type_name(expected0.name()) {
		return false
	}
	actual_base := g.ierror_payload_concrete_type(actual)
	if actual_base is types.Interface {
		return false
	}
	concrete := actual_base.name()
	if concrete.len == 0 {
		return false
	}
	return g.type_can_box_as_ierror(concrete)
}

// types_numeric_compatible supports types numeric compatible handling for FlatGen.
fn (g &FlatGen) types_numeric_compatible(a types.Type, b types.Type) bool {
	_ = g
	return (a.is_integer() || a.is_float()) && (b.is_integer() || b.is_float())
}

// call_constructs_type updates call constructs type state for FlatGen.
fn (g &FlatGen) call_constructs_type(id flat.NodeId, target types.Type) bool {
	if int(id) < 0 {
		return false
	}
	node := g.a.nodes[int(id)]
	if node.kind != .call || node.children_count == 0 {
		return false
	}
	fn_node := g.a.child_node(&node, 0)
	if fn_node.kind != .ident {
		return false
	}
	target_name := target.name()
	if target_name.len == 0 {
		return false
	}
	short_target := target_name.all_after_last('.')
	return fn_node.value == target_name || fn_node.value == short_target
}

// is_runtime_array_flags_stmt reports whether is runtime array flags stmt applies in c.
fn (g &FlatGen) is_runtime_array_flags_stmt(id flat.NodeId) bool {
	if int(id) < 0 {
		return false
	}
	node := g.a.nodes[int(id)]
	if node.kind != .call || node.children_count == 0 {
		return false
	}
	fn_node := g.a.child_node(&node, 0)
	if fn_node.kind != .selector || fn_node.value !in ['set', 'clear']
		|| fn_node.children_count == 0 {
		return false
	}
	flags_node := g.a.child_node(fn_node, 0)
	if flags_node.kind != .selector || flags_node.value != 'flags' || flags_node.children_count == 0 {
		return false
	}
	owner_id := g.a.child(flags_node, 0)
	owner_type := types.unwrap_pointer(g.tc.resolve_type(owner_id))
	return owner_type is types.Array || owner_type.name() == 'strings.Builder'
}

fn (g &FlatGen) multi_return_expr_type(id flat.NodeId) ?types.MultiReturn {
	rtype := g.tc.resolve_type(id)
	if rtype is types.MultiReturn {
		return rtype
	}
	utype := g.usable_expr_type(id)
	if utype is types.MultiReturn {
		return utype
	}
	return none
}

fn (g &FlatGen) multi_return_expr_type_for_lhs_count(id flat.NodeId, count int) ?types.MultiReturn {
	if int(id) >= 0 && int(id) < g.a.nodes.len {
		node := g.a.nodes[int(id)]
		if node.kind == .lock_expr {
			if multi := g.lock_expr_multi_return_type(node, count) {
				return multi
			}
		}
	}
	return g.multi_return_expr_type(id)
}

fn (g &FlatGen) lock_expr_multi_return_type(node flat.Node, count int) ?types.MultiReturn {
	if count <= 0 || node.children_count == 0 {
		return none
	}
	body_id := g.a.child(&node, node.children_count - 1)
	if int(body_id) < 0 || int(body_id) >= g.a.nodes.len {
		return none
	}
	body := g.a.nodes[int(body_id)]
	if body.kind != .block {
		return none
	}
	parts := g.multi_return_tail_parts(&body, count) or { return none }
	mut ret_types := []types.Type{cap: parts.values.len}
	for value_id in parts.values {
		ret_types << g.usable_expr_type(value_id)
	}
	return types.MultiReturn{
		types: ret_types
	}
}

// gen_decl_assign emits decl assign output for c.
fn (mut g FlatGen) gen_decl_assign(node flat.Node) {
	if node.children_count >= 3 {
		if _ := g.multi_return_expr_type_for_lhs_count(g.a.child(&node, 1), node.children_count - 1) {
			g.gen_multi_return_decl(node)
			return
		}
	}
	mut bad_decl_child := node.children_count % 2 == 1
	for i in 0 .. node.children_count {
		if int(g.a.child(&node, i)) < 0 {
			bad_decl_child = true
		}
	}
	if bad_decl_child {
		mut parts := []string{}
		for i in 0 .. node.children_count {
			child_id := g.a.child(&node, i)
			if int(child_id) < 0 {
				parts << '${i}:empty'
			} else {
				child := g.a.nodes[int(child_id)]
				parts << '${i}:${child.kind}:${child.value}:${child.typ}'
			}
		}
		panic('internal error: odd decl_assign in ${g.cur_fn_name}: count=${node.children_count} typ=${node.typ} value=${node.value} children=${parts.join('|')}')
	}
	decl_prefix := if node.value == 'static' { 'static ' } else { '' }
	decl_is_shared := decl_assign_is_shared_marker(node.value)
	mut i := 0
	for i < node.children_count {
		lhs_id := g.a.child(&node, i)
		rhs_id := g.a.child(&node, i + 1)
		lhs := g.a.nodes[int(lhs_id)]
		rhs := g.a.nodes[int(rhs_id)]
		lhs_is_defer_capture := lhs.kind == .ident && lhs.value in g.defer_capture_types
		g.track_ierror_stack_pointer_alias(lhs, rhs)
		if decl_is_shared && lhs.kind == .ident {
			shared_type := g.usable_expr_type(rhs_id)
			if shared_type !is types.Unknown && shared_type !is types.Void {
				g.gen_shared_local_decl(lhs_id, rhs_id, shared_type, decl_prefix,
					lhs_is_defer_capture)
				i += 2
				continue
			}
		}
		if rhs.kind in [.cast_expr, .as_expr] && rhs.children_count > 0 {
			target_type := g.tc.parse_type(rhs.value)
			mut fixed := types.ArrayFixed{}
			mut has_fixed := false
			if direct_fixed := array_fixed_type(target_type) {
				fixed = direct_fixed
				has_fixed = true
			} else if alias_fixed := g.fixed_array_type_from_alias_text(rhs.value) {
				fixed = alias_fixed
				has_fixed = true
			}
			if has_fixed {
				lhs_str := g.decl_lhs_str(lhs_id)
				if !lhs_is_defer_capture {
					c_elem, dims := g.fixed_array_decl_parts(fixed)
					g.writeln('${decl_prefix}${c_elem} ${lhs_str}${dims};')
				}
				g.gen_fixed_array_copy_from_node(lhs_str, g.a.child(&rhs, 0), fixed)
				if lhs.kind == .ident {
					owner := g.tc.cur_scope.insert_with_owner(lhs.value, target_type)
					g.track_local_pointer_storage_decl(lhs, owner, target_type, '')
				}
				i += 2
				continue
			}
		}
		if rhs.kind == .array_literal {
			mut rhs_v_type := g.tc.resolve_type(rhs_id)
			if rhs.typ.len > 0 {
				annotated_type := g.tc.parse_type(rhs.typ)
				if !(rhs.children_count == 0 && annotated_type is types.ArrayFixed
					&& rhs_v_type is types.Array) {
					rhs_v_type = annotated_type
				}
			}
			if node.typ.len > 0 {
				decl_type := g.tc.parse_type(node.typ)
				if _ := array_like_type(decl_type) {
					rhs_v_type = decl_type
				} else if _ := array_fixed_type(decl_type) {
					rhs_v_type = decl_type
				}
			}
			if lhs.typ.len > 0 {
				lhs_decl_type := g.tc.parse_type(lhs.typ)
				if _ := array_like_type(lhs_decl_type) {
					rhs_v_type = lhs_decl_type
				} else if _ := array_fixed_type(lhs_decl_type) {
					rhs_v_type = lhs_decl_type
				}
			}
			if lhs.kind == .ident {
				lhs_type := g.usable_expr_type(lhs_id)
				if _ := array_like_type(lhs_type) {
					rhs_v_type = lhs_type
				} else if _ := array_fixed_type(lhs_type) {
					rhs_v_type = lhs_type
				}
			}
			if fixed := array_fixed_type(rhs_v_type) {
				lhs_str := g.decl_lhs_str(lhs_id)
				if !lhs_is_defer_capture {
					c_elem, dims := g.fixed_array_decl_parts(fixed)
					g.writeln('${decl_prefix}${c_elem} ${lhs_str}${dims};')
				}
				g.gen_fixed_array_copy_from_node(lhs_str, rhs_id, fixed)
				if lhs.kind == .ident {
					owner := g.tc.cur_scope.insert_with_owner(lhs.value, rhs_v_type)
					g.track_local_pointer_storage_decl(lhs, owner, rhs_v_type, '')
				}
				i += 2
				continue
			}
			elem_type := if rhs_arr := array_like_type(rhs_v_type) {
				rhs_arr.elem_type
			} else if rhs.children_count > 0 {
				g.tc.resolve_type(g.a.child(&rhs, 0))
			} else {
				types.Type(types.int_)
			}
			if !lhs_is_defer_capture {
				g.write('${decl_prefix}Array ')
			}
			g.gen_decl_lhs(lhs_id)
			g.write(' = ')
			g.gen_array_literal_value(rhs, elem_type)
			g.writeln(';')
			if lhs.kind == .ident {
				arr_type := types.Type(types.Array{
					elem_type: elem_type
				})
				owner := g.tc.cur_scope.insert_with_owner(lhs.value, arr_type)
				g.track_local_pointer_storage_decl(lhs, owner, arr_type, 'Array')
			}
		} else if rhs.kind == .or_expr {
			g.gen_decl_or_expr(lhs, rhs)
		} else if rhs.kind == .array_init {
			raw_init_type := g.tc.parse_type(rhs.value)
			mut init_type := raw_init_type
			mut resolved_init_type := g.tc.resolve_type(rhs_id)
			if node.typ.len > 0 {
				decl_type := g.tc.parse_type(node.typ)
				if arr := array_like_type(decl_type) {
					resolved_init_type = decl_type
					init_type = arr.elem_type
				}
			}
			if lhs.typ.len > 0 {
				lhs_decl_type := g.tc.parse_type(lhs.typ)
				if arr := array_like_type(lhs_decl_type) {
					resolved_init_type = lhs_decl_type
					init_type = arr.elem_type
				}
			}
			is_dynamic_array_init := resolved_init_type is types.Array || rhs.typ.starts_with('[]')
				|| node.typ.starts_with('[]') || lhs.typ.starts_with('[]')
			if init_type is types.ArrayFixed && !is_dynamic_array_init {
				g.gen_fixed_array_zero_init_decl(lhs_id, init_type, decl_prefix,
					lhs_is_defer_capture)
				if lhs.kind == .ident {
					owner := g.tc.cur_scope.insert_with_owner(lhs.value, raw_init_type)
					g.track_local_pointer_storage_decl(lhs, owner, raw_init_type, '')
				}
			} else {
				c_elem := g.value_sizeof_target(init_type)
				mut init_len := '0'
				mut init_cap := '0'
				mut init_val := ''
				for j in 0 .. rhs.children_count {
					child := g.a.child_node(&rhs, j)
					if child.kind == .field_init {
						if child.value == 'len' {
							init_len = g.expr_to_string(g.a.child(child, 0))
						} else if child.value == 'cap' {
							init_cap = g.expr_to_string(g.a.child(child, 0))
						} else if child.value == 'init' {
							init_val = g.expr_to_string(g.a.child(child, 0))
						}
					}
				}
				lhs_str := g.decl_lhs_str(lhs_id)
				if lhs_is_defer_capture {
					g.writeln('${lhs_str} = array_new(sizeof(${c_elem}), ${init_len}, ${init_cap});')
				} else {
					g.writeln('${decl_prefix}Array ${lhs_str} = array_new(sizeof(${c_elem}), ${init_len}, ${init_cap});')
				}
				if init_val.len > 0 {
					g.writeln('for (int _ai = 0; _ai < ${lhs_str}.len; _ai++) ((${c_elem}*)${lhs_str}.data)[_ai] = ${init_val};')
				}
				if lhs.kind == .ident {
					// Assign (not cast-in-if-expr): `types.Type(smartcast_mut)` in an
					// if-expression arm is miscompiled by the bootstrap compiler and
					// boxes the value with a wrong sum tag.
					mut arr_type := types.Type(types.Array{
						elem_type: init_type
					})
					if resolved_init_type is types.Array {
						arr_type = resolved_init_type
					}
					owner := g.tc.cur_scope.insert_with_owner(lhs.value, arr_type)
					g.track_local_pointer_storage_decl(lhs, owner, arr_type, 'Array')
				}
			}
		} else if init_type := g.fixed_array_zero_init_block_type(rhs) {
			g.gen_fixed_array_zero_init_decl(lhs_id, init_type, decl_prefix, lhs_is_defer_capture)
			if lhs.kind == .ident {
				owner := g.tc.cur_scope.insert_with_owner(lhs.value, init_type)
				g.track_local_pointer_storage_decl(lhs, owner, init_type, '')
			}
		} else if rhs.kind == .map_init {
			v_type := g.tc.resolve_type(rhs_id)
			c_typ := g.tc.c_type(v_type)
			if !lhs_is_defer_capture {
				g.write('${decl_prefix}${c_typ} ')
			}
			g.gen_decl_lhs(lhs_id)
			g.write(' = ')
			g.gen_expr_with_expected_type(rhs_id, v_type)
			g.writeln(';')
			if lhs.kind == .ident {
				owner := g.tc.cur_scope.insert_with_owner(lhs.value, v_type)
				g.track_local_pointer_storage_decl(lhs, owner, v_type, c_typ)
			}
		} else {
			mut v_type := if node.typ.len > 0 {
				decl_type := g.tc.parse_type(node.typ)
				if decl_type is types.Struct && decl_type.name == 'array' {
					g.usable_expr_type(rhs_id)
				} else if sum_field_type := g.selector_sum_shared_field_type(rhs_id) {
					sum_field_type
				} else if decl_annotation_is_unusable(decl_type, node.typ) {
					rhs_type := g.decl_rhs_fallback_type(rhs_id, rhs)
					if rhs_type is types.Unknown {
						types.Type(decl_type)
					} else {
						rhs_type
					}
				} else {
					decl_type
				}
			} else if rhs.kind == .if_expr {
				g.if_expr_type(&rhs)
			} else {
				g.usable_expr_type(rhs_id)
			}
			if rhs.kind == .struct_init
				&& (v_type is types.OptionType || v_type is types.ResultType) {
				rhs_type := g.usable_expr_type(rhs_id)
				if rhs_type !is types.OptionType && rhs_type !is types.ResultType
					&& rhs_type !is types.Unknown && rhs_type !is types.Void {
					v_type = rhs_type
				}
			}
			if rhs.kind == .lock_expr && v_type !is types.MultiReturn {
				lock_type := g.usable_expr_type(rhs_id)
				if lock_type !is types.Unknown && lock_type !is types.Void {
					v_type = lock_type
				}
			}
			if rhs.kind == .prefix && rhs.op == .amp && rhs.children_count > 0
				&& v_type !is types.Pointer {
				v_type = types.Type(types.Pointer{
					base_type: g.usable_expr_type(g.a.child(&rhs, 0))
				})
			}
			if fixed := g.to_fixed_size_call_fixed_type(rhs_id) {
				v_type = types.Type(fixed)
			}
			if fixed := array_fixed_type(v_type) {
				if g.fixed_array_decl_is_unusable(fixed) {
					rhs_type := g.decl_rhs_fallback_type(rhs_id, rhs)
					if rhs_type !is types.Unknown {
						v_type = rhs_type
					}
				}
			}
			v_type = g.optional_source_type_for_expr(rhs_id, v_type)
			v_type = g.preserve_specialized_alias_decl_type(rhs_id, rhs, v_type)
			if fixed := array_fixed_type(v_type) {
				lhs_str := g.decl_lhs_str(lhs_id)
				if !lhs_is_defer_capture {
					c_elem, dims := g.fixed_array_decl_parts(fixed)
					g.writeln('${decl_prefix}${c_elem} ${lhs_str}${dims};')
				}
				g.gen_fixed_array_copy_from_node(lhs_str, rhs_id, fixed)
				if lhs.kind == .ident {
					owner := g.tc.cur_scope.insert_with_owner(lhs.value, v_type)
					g.track_local_pointer_storage_decl(lhs, owner, v_type, '')
				}
				i += 2
				continue
			}
			ct0 := if v_type is types.MultiReturn {
				g.value_c_type(v_type)
			} else if rhs.kind == .struct_init
				&& g.struct_init_decl_type_is_bare_generic_instance(rhs, v_type) {
				g.tc.c_type(v_type)
			} else if rhs.kind == .struct_init {
				g.struct_init_c_type_name(rhs.value)
			} else if v_type is types.Enum {
				g.value_c_type(v_type)
			} else {
				g.tc.c_type(v_type)
			}
			ct := if v_type is types.OptionType || v_type is types.ResultType {
				g.optional_type_name_for_expr(rhs_id, v_type)
			} else {
				ct0
			}
			if ct.starts_with('fn_ptr:') {
				fp_name := g.resolve_fn_ptr_type(ct)
				if !lhs_is_defer_capture {
					g.write('${decl_prefix}${fp_name} ')
				}
			} else {
				if !lhs_is_defer_capture {
					g.write('${decl_prefix}${ct} ')
				}
			}
			g.gen_decl_lhs(lhs_id)
			g.write(' = ')
			g.gen_decl_init_expr(rhs_id, rhs, v_type, ct, !lhs_is_defer_capture)
			g.writeln(';')
			if lhs.kind == .ident {
				owner := g.tc.cur_scope.insert_with_owner(lhs.value, v_type)
				g.track_local_pointer_storage_decl(lhs, owner, v_type, ct)
			}
		}
		i += 2
	}
}

fn (mut g FlatGen) gen_shared_local_decl(lhs_id flat.NodeId, rhs_id flat.NodeId, inner_type types.Type, decl_prefix string, lhs_is_defer_capture bool) {
	lhs := g.a.nodes[int(lhs_id)]
	value_type := shared_local_value_type(inner_type)
	inner := g.shared_qualify_type_text(value_type.name(), g.tc.cur_module)
	wrapper := g.shared_wrapper_c_name(inner)
	if !lhs_is_defer_capture {
		g.write('${decl_prefix}${wrapper}* ')
	}
	if fixed := array_fixed_type(value_type) {
		lhs_str := g.decl_lhs_str(lhs_id)
		g.gen_decl_lhs(lhs_id)
		g.writeln(' = (${wrapper}*)__dup${wrapper}(&(${wrapper}){.mtx = {0}}, sizeof(${wrapper}));')
		src := g.fixed_array_copy_source_string(rhs_id, types.Type(fixed))
		g.writeln('memmove(${lhs_str}->val, ${src}, sizeof(${lhs_str}->val));')
		if lhs.kind == .ident {
			owner := g.tc.cur_scope.insert_with_owner(lhs.value, value_type)
			g.track_local_pointer_storage_decl(lhs, owner, value_type, '${wrapper}*')
			g.declare_local_shared_storage(owner, true)
		}
		return
	}
	g.gen_decl_lhs(lhs_id)
	g.write(' = (${wrapper}*)__dup${wrapper}(&(${wrapper}){.mtx = {0}, .val = ')
	if inner_type is types.Pointer {
		if g.a.nodes[int(rhs_id)].kind == .ident {
			g.write('*')
			g.gen_expr_with_expected_type(rhs_id, inner_type)
		} else {
			g.write('*(')
			g.gen_expr_with_expected_type(rhs_id, inner_type)
			g.write(')')
		}
	} else {
		g.gen_expr_with_expected_type(rhs_id, value_type)
	}
	g.writeln('}, sizeof(${wrapper}));')
	if lhs.kind == .ident {
		owner := g.tc.cur_scope.insert_with_owner(lhs.value, value_type)
		g.track_local_pointer_storage_decl(lhs, owner, value_type, '${wrapper}*')
		g.declare_local_shared_storage(owner, true)
	}
}

fn (mut g FlatGen) track_ierror_stack_pointer_alias(lhs flat.Node, rhs flat.Node) {
	if lhs.kind != .ident || lhs.value.len == 0 {
		return
	}
	needs_copy := g.ierror_pointer_payload_expr_needs_heap_copy(rhs)
		|| g.ierror_array_get_pointer_alias_needs_copy(rhs)
	g.declare_ierror_pointer_alias(lhs.value, needs_copy)
}

fn (mut g FlatGen) track_local_pointer_storage_decl(lhs flat.Node, owner types.ScopeBindingOwner, typ types.Type, c_type string) {
	if lhs.kind != .ident || lhs.value.len == 0 || lhs.value == '_' {
		return
	}
	g.declare_local_c_type(owner, c_type)
	g.declare_local_pointer_storage(owner,
		typ is types.Pointer || c_type_is_pointer_storage(c_type))
}

fn c_type_is_pointer_storage(c_type string) bool {
	clean := trimmed_space(c_type)
	return clean.len > 0 && !clean.starts_with('fn_ptr:') && clean.ends_with('*')
}

fn (mut g FlatGen) track_ierror_stack_pointer_alias_assign(lhs flat.Node, rhs flat.Node) {
	if lhs.kind != .ident || lhs.value.len == 0 {
		return
	}
	g.assign_ierror_pointer_alias(lhs.value, g.ierror_pointer_payload_expr_needs_heap_copy(rhs)
		|| g.ierror_array_get_pointer_alias_needs_copy(rhs))
}

fn (mut g FlatGen) track_ierror_array_push_call_alias(node flat.Node) {
	if node.kind != .call || node.children_count < 3 {
		return
	}
	target := g.call_target_name(g.a.child(&node, 0))
	if target !in ['array_push', 'array__push'] {
		return
	}
	array_arg := g.a.nodes[int(g.a.child(&node, 1))]
	array_name := g.ierror_pointer_alias_name_from_addr(array_arg) or { return }
	value_arg := g.a.nodes[int(g.a.child(&node, 2))]
	needs_copy := g.ierror_array_push_value_needs_copy(value_arg)
	if needs_copy {
		g.assign_ierror_pointer_alias(array_name, true)
	}
}

fn (g &FlatGen) ierror_array_push_value_needs_copy(node flat.Node) bool {
	clean := g.ierror_pointer_payload_unwrapped_node(node)
	if clean.kind == .prefix && clean.op == .amp && clean.children_count > 0 {
		child := g.ierror_pointer_payload_unwrapped_node(g.a.nodes[int(g.a.child(&clean, 0))])
		if child.kind == .ident && g.ierror_pointer_alias_needs_copy(child.value) {
			return true
		}
	}
	return g.ierror_pointer_payload_expr_needs_heap_copy(clean)
}

fn (g &FlatGen) selector_sum_shared_field_type(id flat.NodeId) ?types.Type {
	if int(id) < 0 || int(id) >= g.a.nodes.len {
		return none
	}
	node := g.a.nodes[int(id)]
	if node.kind != .selector || node.children_count == 0 {
		return none
	}
	base_type0 := g.usable_expr_type(g.a.child(&node, 0))
	return g.sum_shared_field_type(base_type0, node.value)
}

fn (g &FlatGen) decl_rhs_fallback_type(rhs_id flat.NodeId, rhs flat.Node) types.Type {
	if rhs.kind == .index && rhs.value.len > 0 {
		index_type := g.tc.parse_type(rhs.value)
		if !decl_annotation_is_unusable(index_type, rhs.value) {
			return index_type
		}
	}
	if alias_type := g.index_rhs_alias_elem_type(rhs) {
		return alias_type
	}
	if rhs.typ.len > 0 {
		rhs_type := g.tc.parse_type(rhs.typ)
		if !decl_annotation_is_unusable(rhs_type, rhs.typ) {
			return rhs_type
		}
	}
	if rhs.kind == .index && rhs.children_count > 0 {
		base_type := types.unwrap_pointer(g.usable_expr_type(g.a.child(&rhs, 0)))
		if base_type is types.Array {
			return base_type.elem_type
		}
		if base_type is types.ArrayFixed {
			return base_type.elem_type
		}
		if base_type is types.Map {
			return base_type.value_type
		}
	}
	return g.usable_expr_type(rhs_id)
}

fn (g &FlatGen) index_rhs_alias_elem_type(rhs flat.Node) ?types.Type {
	if rhs.kind != .index || rhs.children_count == 0 {
		return none
	}
	base_type := types.unwrap_pointer(g.usable_expr_type(g.a.child(&rhs, 0)))
	if arr := array_like_type(base_type) {
		if arr.elem_type is types.Alias {
			return arr.elem_type
		}
	}
	if base_type is types.Map {
		if base_type.value_type is types.Alias {
			return base_type.value_type
		}
	}
	return none
}

fn (g &FlatGen) preserve_specialized_alias_decl_type(rhs_id flat.NodeId, rhs flat.Node, v_type types.Type) types.Type {
	if v_type is types.Alias || !g.name_uses_specialized_generic_abi(g.cur_fn_name) {
		return v_type
	}
	rhs_type := g.decl_rhs_fallback_type(rhs_id, rhs)
	if rhs_type is types.Alias && g.alias_base_matches_type(rhs_type, v_type) {
		return rhs_type
	}
	return v_type
}

fn (g &FlatGen) struct_init_decl_type_is_bare_generic_instance(rhs flat.Node, v_type types.Type) bool {
	if rhs.kind != .struct_init || rhs.value.contains('[') {
		return false
	}
	type_name := v_type.name()
	if !type_name.contains('[') {
		return false
	}
	return rhs.value.all_after_last('.') == type_name.all_before('[').all_after_last('.')
}

fn (g &FlatGen) alias_base_matches_type(alias_type types.Alias, typ types.Type) bool {
	base := types.unwrap_pointer(alias_type.base_type)
	clean := types.unwrap_pointer(typ)
	if g.type_names_match(base, clean) {
		return true
	}
	return g.tc.c_type(base) == g.tc.c_type(clean)
}

fn (g &FlatGen) usable_struct_field_type(type_name string, field_name string) ?types.Type {
	typ := g.struct_field_type(type_name, field_name) or { return none }
	if field_type_needs_checker_authority(typ.name()) {
		if checker_typ := g.checker_struct_field_type(type_name, field_name) {
			return checker_typ
		}
	}
	return typ
}

fn (g &FlatGen) checker_struct_field_type(type_name string, field_name string) ?types.Type {
	mut names := []string{}
	if type_name.len > 0 {
		names << type_name
	}
	if info := g.find_struct_decl(type_name) {
		names << info.full_name
	}
	if type_name.contains('.') {
		names << type_name.all_after_last('.')
	} else {
		qname := g.tc.qualify_name(type_name)
		if qname != type_name {
			names << qname
		}
	}
	for name in names {
		raw := g.tc.struct_field_type_name(name, field_name) or { continue }
		typ := g.tc.parse_type(raw)
		if typ !is types.Unknown && typ !is types.Void {
			return typ
		}
	}
	return none
}

fn field_type_needs_checker_authority(raw string) bool {
	clean_raw := raw.replace(' ', '')
	return clean_raw in ['Option', 'Optional', 'Result'] || clean_raw.starts_with('Option_')
		|| clean_raw.starts_with('Optional_') || clean_raw.starts_with('Result_')
}

fn (mut g FlatGen) fixed_array_decl_is_unusable(typ types.ArrayFixed) bool {
	c_elem, _ := g.fixed_array_decl_parts(typ)
	return c_elem == 'void'
}

fn decl_annotation_is_unusable(typ types.Type, raw string) bool {
	if typ is types.ArrayFixed && fixed_array_contains_void(typ) {
		return true
	}
	clean_raw := raw.replace(' ', '')
	if field_type_needs_checker_authority(clean_raw) {
		return true
	}
	return clean_raw == 'void' || clean_raw.starts_with('void[') || clean_raw.ends_with(']void')
		|| clean_raw.contains(']void[')
}

fn fixed_array_contains_void(typ types.Type) bool {
	if typ is types.ArrayFixed {
		return fixed_array_contains_void(typ.elem_type)
	}
	return typ is types.Void
}

fn (mut g FlatGen) gen_fixed_array_zero_init_decl(lhs_id flat.NodeId, init_type types.ArrayFixed, decl_prefix string, lhs_is_defer_capture bool) {
	c_elem, dims := g.fixed_array_decl_parts(init_type)
	lhs_str := g.decl_lhs_str(lhs_id)
	if g.fixed_array_len_is_zero(init_type) {
		if !lhs_is_defer_capture {
			g.writeln('${decl_prefix}${c_elem} ${lhs_str}${dims};')
		}
	} else if lhs_is_defer_capture {
		g.writeln('memset(${lhs_str}, 0, sizeof(${lhs_str}));')
	} else {
		g.writeln('${decl_prefix}${c_elem} ${lhs_str}${dims} = {0};')
	}
}

fn (mut g FlatGen) fixed_array_zero_init_block_type(node flat.Node) ?types.ArrayFixed {
	if node.kind != .block || node.children_count != 1 {
		return none
	}
	stmt_id := g.a.child(&node, 0)
	if int(stmt_id) < 0 {
		return none
	}
	stmt := g.a.nodes[int(stmt_id)]
	expr_id := if stmt.kind == .expr_stmt && stmt.children_count == 1 {
		g.a.child(&stmt, 0)
	} else {
		stmt_id
	}
	if int(expr_id) < 0 {
		return none
	}
	expr := g.a.nodes[int(expr_id)]
	if expr.kind != .array_init || expr.children_count != 0 {
		return none
	}
	typ := g.tc.parse_type(expr.value)
	if typ is types.ArrayFixed {
		return typ
	}
	return none
}

// gen_decl_init_expr emits decl init expr output for c.
fn (mut g FlatGen) gen_decl_init_expr(rhs_id flat.NodeId, rhs flat.Node, v_type types.Type, c_type string, is_declaration bool) {
	if rhs.kind == .int_literal && rhs.value == '0' && g.is_aggregate_zero_init_type(v_type, c_type) {
		if is_declaration {
			g.write('{0}')
		} else {
			g.write('(${c_type}){0}')
		}
		return
	}
	g.gen_expr_with_expected_type(rhs_id, v_type)
}

// gen_multi_return_decl emits multi return decl output for c.
fn (g &FlatGen) multi_return_types_have_fixed_array(ret_types []types.Type) bool {
	for typ in ret_types {
		if _ := array_fixed_type(typ) {
			return true
		}
	}
	return false
}

fn (mut g FlatGen) gen_multi_return_temp(ct string, ret_types []types.Type, node flat.Node) string {
	tmp := g.tmp_name()
	g.writeln('${ct} ${tmp};')
	for i in 0 .. node.children_count {
		field := '${tmp}.arg${i}'
		child_id := g.a.child(&node, i)
		if i < ret_types.len {
			if fixed := array_fixed_type(ret_types[i]) {
				g.gen_fixed_array_copy_from_node(field, child_id, fixed)
				continue
			}
			g.write('${field} = ')
			g.gen_expr_with_expected_type(child_id, ret_types[i])
			g.writeln(';')
			continue
		}
		g.write('${field} = ')
		g.gen_expr(child_id)
		g.writeln(';')
	}
	return tmp
}

fn (mut g FlatGen) gen_multi_return_temp_return(ct string, ret_types []types.Type, node flat.Node) {
	tmp := g.gen_multi_return_temp(ct, ret_types, node)
	g.writeln('return ${tmp};')
}

fn (mut g FlatGen) gen_fixed_array_copy_from_node(dst string, rhs_id flat.NodeId, fixed types.ArrayFixed) {
	g.write('memmove(${dst}, ')
	g.gen_fixed_array_data_arg(rhs_id, fixed)
	g.writeln(', sizeof(${dst}));')
}

fn (mut g FlatGen) gen_multi_return_decl(node flat.Node) {
	rhs_id := g.a.child(&node, 1)
	rhs_multi := g.multi_return_expr_type_for_lhs_count(rhs_id, node.children_count - 1) or {
		return
	}
	rhs_type := types.Type(rhs_multi)
	ct := g.value_c_type(rhs_type)
	tmp := g.tmp_name()
	if or_rhs := g.unwrap_or_expr_node(rhs_id) {
		g.gen_multi_return_or_rhs(or_rhs, rhs_multi, ct, tmp)
	} else {
		g.write('${ct} ${tmp} = ')
		g.gen_expr_with_expected_type(rhs_id, rhs_type)
		g.writeln(';')
	}
	num_lhs := node.children_count - 1
	multi_types := rhs_multi.types.clone()
	for j in 0 .. num_lhs {
		lhs_idx := if j == 0 { 0 } else { j + 1 }
		lhs_id := g.a.child(&node, lhs_idx)
		lhs := g.a.nodes[int(lhs_id)]
		if lhs.kind == .ident && lhs.value == '_' {
			continue
		}
		field_type := if j < multi_types.len {
			g.value_c_type(multi_types[j])
		} else {
			'int'
		}
		lhs_name := g.cname(lhs.value)
		if j < multi_types.len {
			if fixed := array_fixed_type(multi_types[j]) {
				c_elem, dims := g.fixed_array_decl_parts(fixed)
				g.writeln('${c_elem} ${lhs_name}${dims};')
				g.writeln('memmove(${lhs_name}, ${tmp}.arg${j}, sizeof(${lhs_name}));')
				if lhs.kind == .ident {
					owner := g.tc.cur_scope.insert_with_owner(lhs.value, multi_types[j])
					g.track_local_pointer_storage_decl(lhs, owner, multi_types[j], '')
				}
				continue
			}
		}
		g.writeln('${field_type} ${lhs_name} = ${tmp}.arg${j};')
		if lhs.kind == .ident {
			inner := if j < multi_types.len {
				multi_types[j]
			} else {
				types.Type(types.int_)
			}
			owner := g.tc.cur_scope.insert_with_owner(lhs.value, inner)
			g.track_local_pointer_storage_decl(lhs, owner, inner, field_type)
		}
	}
}

fn (g &FlatGen) unwrap_or_expr_node(id flat.NodeId) ?flat.Node {
	if int(id) < 0 || int(id) >= g.a.nodes.len {
		return none
	}
	node := g.a.nodes[int(id)]
	if node.kind == .or_expr {
		return node
	}
	if node.kind in [.paren, .expr_stmt] && node.children_count > 0 {
		return g.unwrap_or_expr_node(g.a.child(&node, 0))
	}
	if node.kind == .block && node.children_count == 1 {
		return g.unwrap_or_expr_node(g.a.child(&node, 0))
	}
	return none
}

fn (mut g FlatGen) gen_multi_return_or_rhs(or_node flat.Node, rhs_multi types.MultiReturn, ct string, tmp string) {
	if or_node.children_count < 2 {
		g.writeln('${ct} ${tmp} = (${ct}){};')
		return
	}
	expr_id := g.a.child(&or_node, 0)
	or_body_id := g.a.child(&or_node, 1)
	or_body := g.a.nodes[int(or_body_id)]
	expr_node := g.a.nodes[int(expr_id)]
	expr_type := g.optional_source_type_for_expr(expr_id, g.or_expr_source_type(expr_id, expr_node))
	opt_ct := g.optional_type_name_for_expr(expr_id, expr_type)
	opt_tmp := g.tmp_name()
	g.write('${opt_ct} ${opt_tmp} = ')
	g.gen_expr_with_expected_type(expr_id, expr_type)
	g.writeln(';')
	g.writeln('${ct} ${tmp} = (${ct}){};')
	g.writeln('if (${opt_tmp}.ok) {')
	g.indent++
	g.writeln('${tmp} = ${opt_tmp}.value;')
	g.indent--
	g.writeln('} else {')
	g.push_scope()
	g.tc.cur_scope.insert('err', types.Type(types.Struct{
		name: 'IError'
	}))
	g.indent++
	g.writeln('IError err = ${opt_tmp}.err;')
	if or_node.value == '!' || or_node.value == '?' {
		if g.cur_fn_ret_is_optional {
			fn_opt_ct := g.optional_type_name(g.cur_fn_ret)
			g.gen_propagation_return_cleanup()
			g.writeln('return (${fn_opt_ct}){.ok = false, .err = err};')
		} else {
			g.writeln('panic(IError__str(err));')
		}
	} else {
		g.gen_or_body_value(or_body, tmp, types.Type(rhs_multi))
		g.gen_scope_ownership_drops()
	}
	g.indent--
	g.pop_scope()
	g.writeln('}')
}

// gen_assign emits assign output for c.
fn (mut g FlatGen) gen_assign(node flat.Node) {
	if node.children_count >= 3 {
		rhs_id := g.a.child(&node, 1)
		if _ := g.multi_return_expr_type_for_lhs_count(rhs_id, node.children_count - 1) {
			g.gen_multi_return_assign(node)
			return
		}
	}
	mut i := 0
	for i < node.children_count {
		lhs := g.a.nodes[int(g.a.child(&node, i))]
		rhs_for_alias := g.a.nodes[int(g.a.child(&node, i + 1))]
		if node.op == .assign {
			g.track_ierror_stack_pointer_alias_assign(lhs, rhs_for_alias)
		}
		if lhs.kind == .ident && lhs.value == '_' {
			g.write('(void)(')
			g.gen_expr(g.a.child(&node, i + 1))
			g.writeln(');')
		} else if node.op == .left_shift_assign && lhs.kind == .ident
			&& node.value in ['push', 'push_many'] {
			if node.value == 'push_many' {
				g.gen_array_push_many_stmt(g.a.child(&node, i), g.a.child(&node, i + 1))
			} else if node.value == 'push' {
				lhs_id := g.a.child(&node, i)
				rhs_id := g.a.child(&node, i + 1)
				lhs_arr_type := types.unwrap_pointer(g.usable_expr_type(lhs_id))
				if lhs_arr := array_like_type(lhs_arr_type) {
					push_rhs_clean := types.unwrap_pointer(g.usable_expr_type(rhs_id))
					if rhs_arr := array_like_type(push_rhs_clean) {
						if g.tc.c_type(lhs_arr.elem_type) !in ['array', 'Array']
							&& g.tc.c_type(lhs_arr.elem_type) == g.tc.c_type(rhs_arr.elem_type) {
							g.gen_array_push_many_stmt(lhs_id, rhs_id)
							i += 2
							continue
						}
					} else if rhs_fixed := array_fixed_type(push_rhs_clean) {
						if g.tc.c_type(lhs_arr.elem_type) !in ['array', 'Array']
							&& g.tc.c_type(lhs_arr.elem_type) == g.tc.c_type(rhs_fixed.elem_type) {
							g.gen_array_push_many_stmt(lhs_id, rhs_id)
							i += 2
							continue
						}
					}
					lhs_is_ptr := g.tc.resolve_type(g.a.child(&node, i)) is types.Pointer
					amp := if lhs_is_ptr { '' } else { '&' }
					c_elem := g.tc.c_type(lhs_arr.elem_type)
					g.write('array_push(${amp}${g.cname(lhs.value)}, &(${c_elem}[]){')
					g.gen_expr_with_expected_type(g.a.child(&node, i + 1), lhs_arr.elem_type)
					g.writeln('});')
				} else {
					// Array appends are annotated by the transformer; an un-annotated
					// `<<=` here is the integer bit-shift-assign operator.
					g.gen_expr(g.a.child(&node, i))
					g.write(' <<= ')
					g.gen_expr(g.a.child(&node, i + 1))
					g.writeln(';')
				}
			} else {
				g.gen_expr(g.a.child(&node, i))
				g.write(' <<= ')
				g.gen_expr(g.a.child(&node, i + 1))
				g.writeln(';')
			}
		} else {
			rhs_id := g.a.child(&node, i + 1)
			rhs_node := g.a.nodes[int(rhs_id)]
			if rhs_node.kind == .or_expr {
				g.gen_assign_or_expr(node, i, rhs_node)
				i += 2
				continue
			}
			lhs_id := g.a.child(&node, i)
			if rhs_node.kind == .array_literal {
				lhs_type := types.unwrap_pointer(g.tc.resolve_type(lhs_id))
				if lhs_type is types.ArrayFixed {
					// A fixed-array field/var can't be `=`-assigned an array literal (which
					// lowers to a dynamic `Array`); memcpy the element bytes instead.
					g.write('memcpy(')
					g.gen_expr(lhs_id)
					g.write(', ')
					g.gen_fixed_array_data_arg(rhs_id, lhs_type)
					g.write(', sizeof(')
					g.gen_expr(lhs_id)
					g.writeln('));')
					i += 2
					continue
				}
				elem_type := if lhs_arr := array_like_type(lhs_type) {
					lhs_arr.elem_type
				} else if rhs_arr := array_like_type(g.usable_expr_type(rhs_id)) {
					rhs_arr.elem_type
				} else if rhs_node.children_count > 0 {
					g.tc.resolve_type(g.a.child(&rhs_node, 0))
				} else {
					types.Type(types.int_)
				}
				g.gen_expr(g.a.child(&node, i))
				g.write(' = ')
				g.gen_array_literal_value(rhs_node, elem_type)
				g.writeln(';')
			} else {
				lhs_type := g.usable_expr_type(lhs_id)
				rhs_type := g.usable_expr_type(rhs_id)
				if node.op == .assign {
					if lhs_fixed := array_fixed_type(lhs_type) {
						if _ := array_fixed_type(rhs_type) {
							dst := g.expr_to_string(lhs_id)
							g.writeln('memmove(${dst}, ${g.expr_to_string(rhs_id)}, sizeof(${dst}));')
							i += 2
							continue
						} else if rhs_node.kind == .array_init || rhs_node.kind == .array_literal
							|| rhs_node.kind == .postfix {
							dst := g.expr_to_string(lhs_id)
							g.write('memmove(${dst}, ')
							g.gen_fixed_array_data_arg(rhs_id, lhs_fixed)
							g.writeln(', sizeof(${dst}));')
							i += 2
							continue
						}
					}
				}
				if node.op == .plus_assign && (lhs_type is types.String || rhs_type is types.String) {
					g.gen_expr(g.a.child(&node, i))
					g.write(' = string__plus(')
					g.gen_expr(g.a.child(&node, i))
					g.write(', ')
					g.gen_expr(rhs_id)
					g.writeln(');')
					i += 2
					continue
				}
				if method_name := g.assign_struct_operator_method(lhs_type, node.op) {
					g.gen_expr(lhs_id)
					g.write(' = ${g.cname(method_name)}(')
					g.gen_expr(lhs_id)
					g.write(', ')
					g.gen_expr(rhs_id)
					g.writeln(');')
					i += 2
					continue
				}
				if lhs_type is types.Enum {
					g.expected_enum = lhs_type.name
				}
				if node.op == .right_shift_unsigned_assign {
					// `x >>>= y` -> `x = (T)((UT)(x) >> (y))` (logical shift).
					lhs_assign_id := g.a.child(&node, i)
					if g.a.nodes[int(lhs_assign_id)].kind == .ident {
						mut lhs_text := g.expr_to_string(lhs_assign_id)
						if g.assign_lhs_needs_deref(lhs_assign_id, lhs_type, rhs_type, node.op) {
							lhs_text = '*${lhs_text}'
						}
						g.write('${lhs_text} = ')
						g.gen_unsigned_right_shift_from_text(lhs_text, rhs_id, lhs_type)
						g.writeln(';')
					} else {
						// Compound assignment evaluates its lvalue once; spill the
						// address so `arr[next()] >>>= 1` runs the index expr once.
						lhs_ct := g.value_c_type(lhs_type)
						addr_tmp := g.tmp_name()
						g.write('{ ${lhs_ct}* ${addr_tmp} = &(')
						g.gen_expr(lhs_assign_id)
						g.write('); *${addr_tmp} = ')
						g.gen_unsigned_right_shift_from_text('*${addr_tmp}', rhs_id, lhs_type)
						g.writeln('; }')
					}
					g.expected_enum = ''
					i += 2
					continue
				}
				if g.assign_lhs_needs_deref(g.a.child(&node, i), lhs_type, rhs_type, node.op) {
					g.write('*')
				}
				g.gen_expr(g.a.child(&node, i))
				g.write(' ${g.op_str(node.op)} ')
				rhs_expected_type := g.assign_rhs_expected_type(lhs_id, lhs_type)
				if _ := fn_type_from(rhs_expected_type) {
					if c_abi_fn := g.assign_lhs_c_abi_fn_ptr_type(lhs_id) {
						if g.gen_callback_fn_value_for_field_c_abi(rhs_id, rhs_expected_type,
							c_abi_fn)
						{
							g.writeln(';')
							g.expected_enum = ''
							i += 2
							continue
						}
					}
					g.gen_expr_with_expected_type(rhs_id, rhs_expected_type)
				} else {
					g.gen_expr_with_expected_type(rhs_id, rhs_expected_type)
				}
				g.writeln(';')
				g.expected_enum = ''
			}
		}
		i += 2
	}
}

fn (g &FlatGen) assign_lhs_c_abi_fn_ptr_type(lhs_id flat.NodeId) ?string {
	if int(lhs_id) < 0 || int(lhs_id) >= g.a.nodes.len {
		return none
	}
	lhs := g.a.nodes[int(lhs_id)]
	if lhs.kind != .selector || lhs.children_count == 0 || lhs.value.len == 0 {
		return none
	}
	base_id := g.a.child(&lhs, 0)
	base_type := types.unwrap_pointer(g.usable_expr_type(base_id))
	mut clean := base_type
	if base_type is types.Alias {
		clean = base_type.base_type
	}
	if clean is types.Struct {
		return g.struct_field_c_abi_fn_ptr_type(clean.name, lhs.value)
	}
	return none
}

fn (g &FlatGen) assign_struct_operator_method(lhs_type types.Type, op flat.Op) ?string {
	clean := types.unwrap_pointer(lhs_type)
	if clean !is types.Struct && clean !is types.Alias {
		return none
	}
	op_symbol := assign_struct_operator_symbol(op) or { return none }
	for receiver in g.assign_struct_operator_receivers(clean.name()) {
		method_name := '${receiver}.${op_symbol}'
		if method_name in g.tc.fn_param_types || method_name in g.tc.fn_ret_types {
			return method_name
		}
		cmethod_name := g.cname(method_name)
		if cmethod_name in g.tc.fn_param_types || cmethod_name in g.tc.fn_ret_types {
			return cmethod_name
		}
	}
	return none
}

fn (g &FlatGen) assign_struct_operator_receivers(type_name string) []string {
	mut receivers := []string{cap: 2}
	if type_name.len == 0 {
		return receivers
	}
	receivers << type_name
	if !type_name.contains('.') && g.tc.cur_module.len > 0 && g.tc.cur_module != 'main'
		&& g.tc.cur_module != 'builtin' {
		receivers << '${g.tc.cur_module}.${type_name}'
	}
	return receivers
}

fn assign_struct_operator_symbol(op flat.Op) ?string {
	match op {
		.plus_assign { return '+' }
		.minus_assign { return '-' }
		.mul_assign { return '*' }
		.div_assign { return '/' }
		.mod_assign { return '%' }
		else {}
	}

	return none
}

fn (g &FlatGen) assign_rhs_expected_type(lhs_id flat.NodeId, lhs_type types.Type) types.Type {
	lhs := g.a.nodes[int(lhs_id)]
	if lhs.kind == .ident && g.current_param_is_mut(lhs.value) {
		if lhs_type is types.Pointer {
			return lhs_type.base_type
		}
	}
	return lhs_type
}

// assign_lhs_needs_deref supports assign lhs needs deref handling for FlatGen.
fn (g &FlatGen) assign_lhs_needs_deref(lhs_id flat.NodeId, lhs_type types.Type, rhs_type types.Type, op flat.Op) bool {
	lhs := g.a.nodes[int(lhs_id)]
	if lhs.kind != .ident {
		return false
	}
	if g.current_param_is_mut(lhs.value) {
		return true
	}
	if op != .assign {
		return false
	}
	if lhs_type is types.Pointer {
		return lhs_type.base_type.name() == rhs_type.name()
	}
	return false
}

// gen_multi_return_assign emits multi return assign output for c.
fn (mut g FlatGen) gen_multi_return_assign(node flat.Node) {
	rhs_id := g.a.child(&node, 1)
	rhs_multi := g.multi_return_expr_type_for_lhs_count(rhs_id, node.children_count - 1) or {
		return
	}
	rhs_type := types.Type(rhs_multi)
	ct := g.value_c_type(rhs_type)
	tmp := g.tmp_name()
	g.write('${ct} ${tmp} = ')
	g.gen_expr_with_expected_type(rhs_id, rhs_type)
	g.writeln(';')
	num_lhs := node.children_count - 1
	multi_types := rhs_multi.types.clone()
	for j in 0 .. num_lhs {
		lhs_idx := if j == 0 { 0 } else { j + 1 }
		lhs_id := g.a.child(&node, lhs_idx)
		lhs := g.a.nodes[int(lhs_id)]
		if lhs.kind == .ident && lhs.value == '_' {
			continue
		}
		if j < multi_types.len {
			if _ := array_fixed_type(multi_types[j]) {
				dst := g.expr_to_string(lhs_id)
				g.writeln('memmove(${dst}, ${tmp}.arg${j}, sizeof(${dst}));')
				continue
			}
		}
		gen_expr_lvalue(mut g, lhs_id)
		g.writeln(' = ${tmp}.arg${j};')
	}
}

// gen_decl_lhs emits decl lhs output for c.
fn (mut g FlatGen) gen_decl_lhs(id flat.NodeId) {
	node := g.a.nodes[int(id)]
	if node.kind == .ident {
		if node.value == '_' {
			g.write('__discard_${int(id)}')
			return
		}
		g.write(g.cname(node.value))
	} else {
		g.gen_expr(id)
	}
}

// decl_lhs_str supports decl lhs str handling for FlatGen.
fn (mut g FlatGen) decl_lhs_str(id flat.NodeId) string {
	node := g.a.nodes[int(id)]
	if node.kind == .ident {
		if node.value == '_' {
			return '__discard_${int(id)}'
		}
		return g.cname(node.value)
	}
	return g.expr_to_string(id)
}

// gen_assign_or_expr emits assign or expr output for c.
fn (mut g FlatGen) gen_assign_or_expr(node flat.Node, lhs_idx int, or_node flat.Node) {
	expr_id := g.a.child(&or_node, 0)
	or_body_id := g.a.child(&or_node, 1)
	or_body := g.a.nodes[int(or_body_id)]
	expr_node := g.a.nodes[int(expr_id)]
	tmp := g.tmp_name()
	expr_type := g.optional_source_type_for_expr(expr_id, g.or_expr_source_type(expr_id, expr_node))
	opt_ct := g.optional_type_name_for_expr(expr_id, expr_type)
	g.write('${opt_ct} ${tmp} = ')
	g.gen_expr(expr_id)
	g.writeln(';')
	g.writeln('if (${tmp}.ok) {')
	g.indent++
	g.gen_expr(g.a.child(&node, lhs_idx))
	g.writeln(' = ${tmp}.value;')
	g.indent--
	g.writeln('} else {')
	g.push_scope()
	g.tc.cur_scope.insert('err', types.Type(types.Struct{
		name: 'IError'
	}))
	g.indent++
	g.writeln('IError err = ${tmp}.err;')
	if or_node.value == '!' || or_node.value == '?' {
		if g.cur_fn_ret_is_optional {
			fn_opt_ct := g.optional_type_name(g.cur_fn_ret)
			g.gen_propagation_return_cleanup()
			g.writeln('return (${fn_opt_ct}){.ok = false, .err = err};')
		} else {
			g.writeln('panic(IError__str(err));')
		}
	} else {
		for j in 0 .. or_body.children_count {
			child_id := g.a.child(&or_body, j)
			g.gen_node(child_id)
		}
		g.gen_scope_ownership_drops()
	}
	g.indent--
	g.pop_scope()
	g.writeln('}')
}

// gen_decl_or_expr emits decl or expr output for c.
fn (mut g FlatGen) gen_decl_or_expr(lhs flat.Node, or_node flat.Node) {
	expr_id := g.a.child(&or_node, 0)
	or_body_id := g.a.child(&or_node, 1)
	or_body := g.a.nodes[int(or_body_id)]
	expr_node := g.a.nodes[int(expr_id)]
	if expr_node.kind == .index {
		base_type := g.tc.resolve_type(g.a.child(&expr_node, 0))
		clean := types.unwrap_pointer(base_type)
		if clean is types.Map {
			g.gen_decl_or_map_index(lhs, expr_node, clean, or_body)
			return
		}
	}
	tmp := g.tmp_name()
	expr_type := g.optional_source_type_for_expr(expr_id, g.or_expr_source_type(expr_id, expr_node))
	opt_ct := g.optional_type_name_for_expr(expr_id, expr_type)
	val_ct0, val_type := g.optional_value_ct(expr_type)
	val_ct := if val_type is types.MultiReturn {
		g.optional_payload_c_type(val_type)
	} else {
		val_ct0
	}
	owner := g.tc.cur_scope.insert_with_owner(lhs.value, val_type)
	g.track_local_pointer_storage_decl(lhs, owner, val_type, val_ct)
	g.write('${opt_ct} ${tmp} = ')
	g.gen_expr_with_expected_type(expr_id, expr_type)
	g.writeln(';')
	g.writeln('${val_ct} ${g.cname(lhs.value)};')
	g.writeln('if (${tmp}.ok) {')
	g.indent++
	g.writeln('${g.cname(lhs.value)} = ${tmp}.value;')
	g.indent--
	g.writeln('} else {')
	g.push_scope()
	g.tc.cur_scope.insert('err', types.Type(types.Struct{
		name: 'IError'
	}))
	g.indent++
	g.writeln('IError err = ${tmp}.err;')
	if or_node.value == '!' || or_node.value == '?' {
		if g.cur_fn_ret_is_optional {
			fn_opt_ct := g.optional_type_name(g.cur_fn_ret)
			g.gen_propagation_return_cleanup()
			g.writeln('return (${fn_opt_ct}){.ok = false, .err = err};')
		} else {
			g.writeln('panic(IError__str(err));')
		}
	} else {
		if or_body.children_count > 0 {
			for i in 0 .. or_body.children_count {
				child_id := g.a.child(&or_body, i)
				child := g.a.nodes[int(child_id)]
				if i == or_body.children_count - 1 && child.kind == .expr_stmt {
					inner_id := g.a.child(&child, 0)
					if g.is_noreturn_call(inner_id) {
						g.gen_node(child_id)
					} else {
						g.write('${g.cname(lhs.value)} = ')
						g.gen_expr(g.a.child(&child, 0))
						g.writeln(';')
					}
				} else {
					g.gen_node(child_id)
				}
			}
		}
		g.gen_scope_ownership_drops()
	}
	g.indent--
	g.pop_scope()
	g.writeln('}')
}

// gen_decl_or_map_index emits decl or map index output for c.
fn (mut g FlatGen) gen_decl_or_map_index(lhs flat.Node, expr_node flat.Node, m types.Map, or_body flat.Node) {
	tmp := g.tmp_name()
	c_val := g.tc.c_type(m.value_type)
	c_key := g.map_key_temp_c_type(m.key_type)
	owner := g.tc.cur_scope.insert_with_owner(lhs.value, m.value_type)
	g.track_local_pointer_storage_decl(lhs, owner, m.value_type, c_val)
	g.write('void* ${tmp} = map__get_check(&')
	g.gen_expr(g.a.child(&expr_node, 0))
	g.write(', &(${c_key}[]){')
	g.gen_expr(g.a.child(&expr_node, 1))
	g.writeln('});')
	g.writeln('${c_val} ${g.cname(lhs.value)};')
	g.writeln('if (${tmp}) {')
	g.indent++
	g.writeln('${g.cname(lhs.value)} = *(${c_val}*)${tmp};')
	g.indent--
	g.writeln('} else {')
	g.indent++
	if or_body.children_count > 0 {
		for i in 0 .. or_body.children_count {
			child_id := g.a.child(&or_body, i)
			child := g.a.nodes[int(child_id)]
			if i == or_body.children_count - 1 && child.kind == .expr_stmt {
				inner_id := g.a.child(&child, 0)
				if g.is_noreturn_call(inner_id) {
					g.gen_node(child_id)
				} else {
					g.write('${g.cname(lhs.value)} = ')
					g.gen_expr(g.a.child(&child, 0))
					g.writeln(';')
				}
			} else {
				g.gen_node(child_id)
			}
		}
	}
	g.gen_scope_ownership_drops()
	g.indent--
	g.writeln('}')
}

fn (g &FlatGen) is_json_decode_call_expr(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= g.a.nodes.len {
		return false
	}
	node := g.a.nodes[int(id)]
	if node.kind == .call && node.children_count > 0 {
		target := g.call_target_name(g.a.child(&node, 0))
		if g.is_json_decode_call(id, target) {
			return true
		}
	}
	for i in 0 .. node.children_count {
		if g.is_json_decode_call_expr(g.a.child(&node, i)) {
			return true
		}
	}
	return false
}

fn (g &FlatGen) noreturn_call_id(id flat.NodeId) ?flat.NodeId {
	if int(id) < 0 || int(id) >= g.a.nodes.len {
		return none
	}
	node := g.a.nodes[int(id)]
	match node.kind {
		.expr_stmt, .paren {
			if node.children_count == 0 {
				return none
			}
			return g.noreturn_call_id(g.a.child(&node, 0))
		}
		.call {
			if node.children_count == 0 {
				return none
			}
			return id
		}
		else {
			return none
		}
	}
}

// is_noreturn_call reports whether is noreturn call applies in c.
fn (g &FlatGen) is_noreturn_call(id flat.NodeId) bool {
	call_id := g.noreturn_call_id(id) or { return false }
	return g.tc.resolved_call_never_returns(call_id)
}

// tmp_name supports tmp name handling for FlatGen.
fn (mut g FlatGen) tmp_name() string {
	g.tmp_count++
	return '_t${g.tmp_count}'
}

// gen_or_expr emits or expr output for c.
fn (mut g FlatGen) gen_or_expr(node flat.Node) {
	expr_id := g.a.child(&node, 0)
	or_body_id := g.a.child(&node, 1)
	or_body := g.a.nodes[int(or_body_id)]
	expr_node := g.a.nodes[int(expr_id)]
	if expr_node.kind == .index {
		base_type := g.tc.resolve_type(g.a.child(&expr_node, 0))
		clean := types.unwrap_pointer(base_type)
		if clean is types.Map {
			g.gen_or_map_index(expr_node, clean, or_body)
			return
		}
	}
	tmp := g.tmp_name()
	expr_type := g.optional_source_type_for_expr(expr_id, g.or_expr_source_type(expr_id, expr_node))
	opt_ct := g.optional_type_name_for_expr(expr_id, expr_type)
	no_value := if expr_type is types.OptionType {
		expr_type.base_type is types.Void
	} else if expr_type is types.ResultType {
		expr_type.base_type is types.Void
	} else {
		false
	}
	val_ct0, val_type := g.optional_value_ct(expr_type)
	val_ct := if val_type is types.MultiReturn {
		g.optional_payload_c_type(val_type)
	} else {
		val_ct0
	}
	if no_value {
		g.write('({${opt_ct} ${tmp} = ')
		g.gen_expr_with_expected_type(expr_id, expr_type)
		g.write('; if (!${tmp}.ok) { IError err = ${tmp}.err; (void)err; ')
		g.push_scope()
		g.tc.cur_scope.insert('err', g.tc.parse_type('IError'))
		if node.value == '!' || node.value == '?' {
			if g.cur_fn_ret_is_optional {
				fn_opt_ct := g.optional_type_name(g.cur_fn_ret)
				g.gen_propagation_return_cleanup()
				g.write('return (${fn_opt_ct}){.ok = false, .err = err};')
			} else {
				g.write('panic(IError__str(err));')
			}
		} else {
			for i in 0 .. or_body.children_count {
				g.gen_node(g.a.child(&or_body, i))
			}
			g.gen_scope_ownership_drops()
		}
		g.pop_scope()
		g.write(' } 0;})')
		return
	}
	val := g.tmp_name()
	g.write('({${opt_ct} ${tmp} = ')
	g.gen_expr_with_expected_type(expr_id, expr_type)
	g.write('; ${val_ct} ${val}; if (${tmp}.ok) { ${val} = ${tmp}.value; } else { IError err = ${tmp}.err; (void)err; ')
	// Bind `err` (IError) in a *temporary* cgen scope so the or-body's own string
	// interpolations and selector accesses resolve `err`'s type correctly (without this
	// an `${err}` inside the or-body falls back to `int__str(err)`). The scope is popped
	// afterwards so an outer local named `err` keeps its real type — e.g.
	// `err := 1; _ := maybe() or { 0 }; println('${err}')` must still see `err` as int.
	g.push_scope()
	g.tc.cur_scope.insert('err', g.tc.parse_type('IError'))
	g.gen_or_body_value(or_body, val, val_type)
	g.gen_scope_ownership_drops()
	g.pop_scope()
	g.write(' } ${val};})')
}

fn (g &FlatGen) or_expr_source_type(expr_id flat.NodeId, expr_node flat.Node) types.Type {
	if ret_type := g.json_decode_call_expr_result_type(expr_id) {
		return ret_type
	}
	if expr_node.kind == .call {
		local_type := g.local_fn_call_return_type(expr_id, expr_node)
		if local_type is types.OptionType || local_type is types.ResultType {
			return local_type
		}
		decl_type := g.declared_call_return_type(expr_id)
		if decl_type is types.OptionType || decl_type is types.ResultType {
			return decl_type
		}
	}
	return g.usable_expr_type(expr_id)
}

fn (g &FlatGen) json_decode_call_expr_result_type(id flat.NodeId) ?types.Type {
	if int(id) < 0 || int(id) >= g.a.nodes.len {
		return none
	}
	node := g.a.nodes[int(id)]
	if node.kind == .paren && node.children_count == 1 {
		return g.json_decode_call_expr_result_type(g.a.child(&node, 0))
	}
	if node.kind != .call || node.children_count == 0 {
		return none
	}
	callee_id := g.a.child(&node, 0)
	target := g.call_target_name(callee_id)
	if !g.is_json_decode_call(id, target) {
		return none
	}
	return g.json_decode_result_type_for_call(node)
}

// gen_or_body emits or body output for c.
fn (mut g FlatGen) gen_or_body(or_body flat.Node) {
	if or_body.children_count == 1 {
		last_id := g.a.child(&or_body, or_body.children_count - 1)
		last := g.a.nodes[int(last_id)]
		if last.kind == .expr_stmt {
			g.gen_expr(g.a.child(&last, 0))
		} else {
			g.gen_expr(last_id)
		}
	} else {
		g.write('({')
		for i in 0 .. or_body.children_count {
			child_id := g.a.child(&or_body, i)
			child := g.a.nodes[int(child_id)]
			if i == or_body.children_count - 1 && child.kind == .expr_stmt {
				g.gen_expr(g.a.child(&child, 0))
				g.write(';')
			} else {
				g.gen_node(child_id)
			}
		}
		g.write('})')
	}
}

fn (mut g FlatGen) gen_or_body_value(or_body flat.Node, value_name string, value_type types.Type) {
	if value_type is types.MultiReturn {
		if g.gen_or_body_multi_return_value(or_body, value_name, value_type) {
			return
		}
	}
	for i in 0 .. or_body.children_count {
		child_id := g.a.child(&or_body, i)
		child := g.a.nodes[int(child_id)]
		is_last := i == or_body.children_count - 1
		if is_last && child.kind == .expr_stmt {
			expr_id := g.a.child(&child, 0)
			if g.expr_is_error_call(expr_id) && g.cur_fn_ret_is_optional {
				fn_opt_ct := g.optional_type_name(g.cur_fn_ret)
				g.gen_propagation_return_cleanup()
				g.write('return ')
				g.gen_optional_error_from_call(fn_opt_ct, g.a.nodes[int(expr_id)])
				g.write(';')
			} else if g.is_noreturn_call(expr_id) || g.tc.resolve_type(expr_id) is types.Void {
				// A diverging/void or-body tail (e.g. `panic(..)`/`exit(..)`) yields no
				// value; emit it as a bare statement instead of assigning void.
				g.gen_expr(expr_id)
				g.write(';')
			} else {
				g.write('${value_name} = ')
				g.gen_expr_with_expected_type(expr_id, value_type)
				g.write(';')
			}
		} else {
			g.gen_node(child_id)
		}
	}
}

fn (mut g FlatGen) gen_or_body_multi_return_value(or_body flat.Node, value_name string, value_type types.MultiReturn) bool {
	if g.gen_multi_return_block_field_assigns(value_name, value_type, &or_body) {
		return true
	}
	if or_body.kind == .expr_stmt && or_body.children_count == value_type.types.len {
		for i in 0 .. or_body.children_count {
			field := '${value_name}.arg${i}'
			expr_id := g.a.child(&or_body, i)
			g.write('${field} = ')
			g.gen_expr_with_expected_type(expr_id, value_type.types[i])
			g.writeln(';')
		}
		return true
	}
	if or_body.children_count == 1 {
		child_id := g.a.child(&or_body, 0)
		if int(child_id) >= 0 && int(child_id) < g.a.nodes.len {
			child := g.a.nodes[int(child_id)]
			if child.kind in [.block, .expr_stmt, .paren] {
				return g.gen_or_body_multi_return_value(child, value_name, value_type)
			}
		}
	}
	return false
}

fn (g &FlatGen) expr_is_error_call(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= g.a.nodes.len {
		return false
	}
	node := g.a.nodes[int(id)]
	if node.kind != .call || node.children_count == 0 {
		return false
	}
	fn_node := g.a.child_node(&node, 0)
	return fn_node.kind == .ident
		&& (fn_node.value == 'error' || fn_node.value == 'error_with_code')
}

// gen_or_map_index emits or map index output for c.
fn (mut g FlatGen) gen_or_map_index(expr_node flat.Node, m types.Map, or_body flat.Node) {
	tmp := g.tmp_name()
	c_val := g.tc.c_type(m.value_type)
	c_key := g.map_key_temp_c_type(m.key_type)
	val := g.tmp_name()
	g.write('({void* ${tmp} = map__get_check(&')
	g.gen_expr(g.a.child(&expr_node, 0))
	g.write(', &(${c_key}[]){')
	g.gen_expr(g.a.child(&expr_node, 1))
	g.write('}); ${c_val} ${val}; if (${tmp}) { ${val} = *(${c_val}*)${tmp}; } else { ')
	g.gen_or_body_value(or_body, val, m.value_type)
	g.gen_scope_ownership_drops()
	g.write(' } ${val};})')
}

// gen_or_expr_stmt emits or expr stmt output for c.
fn (mut g FlatGen) gen_or_expr_stmt(node flat.Node) {
	expr_id := g.a.child(&node, 0)
	or_body_id := g.a.child(&node, 1)
	or_body := g.a.nodes[int(or_body_id)]
	tmp := g.tmp_name()
	expr_type := g.optional_source_type_for_expr(expr_id, g.tc.resolve_type(expr_id))
	opt_ct := g.optional_type_name_for_expr(expr_id, expr_type)
	g.writeln('${opt_ct} ${tmp} = ')
	g.gen_expr_with_expected_type(expr_id, expr_type)
	g.writeln(';')
	g.writeln('if (!${tmp}.ok) {')
	g.push_scope()
	g.tc.cur_scope.insert('err', types.Type(types.Struct{
		name: 'IError'
	}))
	g.indent++
	g.writeln('IError err = ${tmp}.err;')
	if node.value == '!' || node.value == '?' {
		if g.cur_fn_ret_is_optional {
			fn_opt_ct := g.optional_type_name(g.cur_fn_ret)
			g.gen_propagation_return_cleanup()
			g.writeln('return (${fn_opt_ct}){.ok = false, .err = err};')
		} else {
			g.writeln('panic(IError__str(err));')
		}
	} else {
		for i in 0 .. or_body.children_count {
			g.gen_node(g.a.child(&or_body, i))
		}
		g.gen_scope_ownership_drops()
	}
	g.indent--
	g.pop_scope()
	g.writeln('}')
}
