module types

import v3.flat

enum RecursiveStrMutationEffect {
	none
	value
	shared
	rebind
}

struct RecursiveStrParamEffect {
	kind         RecursiveStrMutationEffect
	source_param string
}

struct RecursiveStrConditionalParamFlow {
	fallthrough     RecursiveStrParamEffect
	terminal        []RecursiveStrParamEffect
	has_fallthrough bool
}

struct RecursiveStrReturnedParam {
	name  string
	index int
}

struct RecursiveStrBinding {
mut:
	can_recurse             bool
	progressed              bool
	is_recursive_str_method bool
	storage_id              int
	typ_name                string
	elements                []RecursiveStrBinding
	repeated_element        bool
}

struct RecursiveStrEnv {
mut:
	bindings        map[string]RecursiveStrBinding
	next_storage_id int = 2
	pending_goto    string
}

struct RecursiveStrLoopFlow {
	fallthrough []RecursiveStrEnv
	breaks      []RecursiveStrEnv
}

struct RecursiveStrContext {
	fn_id         flat.NodeId
	fn_name       string
	receiver_name string
	receiver_type Type
}

fn (env &RecursiveStrEnv) clone_env() RecursiveStrEnv {
	return RecursiveStrEnv{
		bindings:        env.bindings.clone()
		next_storage_id: env.next_storage_id
		pending_goto:    env.pending_goto
	}
}

fn (mut tc TypeChecker) check_recursive_str_calls(fn_id flat.NodeId, node flat.Node) {
	if !node.value.ends_with('.str') {
		return
	}
	mut receiver := flat.empty_node
	for i in 0 .. node.children_count {
		child_id := tc.a.child(&node, i)
		child := tc.a.node(child_id)
		if child.kind == .param && child.op == .dot {
			receiver = child_id
			break
		}
	}
	if !tc.valid_node_id(receiver) {
		return
	}
	receiver_node := tc.a.node(receiver)
	receiver_type := tc.parse_type(receiver_node.typ)
	mut env := RecursiveStrEnv{}
	env.bindings[receiver_node.value] = RecursiveStrBinding{
		can_recurse: true
		storage_id:  1
		typ_name:    receiver_node.typ
	}
	ctx := RecursiveStrContext{
		fn_id:         fn_id
		fn_name:       node.value
		receiver_name: receiver_node.value
		receiver_type: receiver_type
	}
	tc.recursive_str_process_child_sequence(node, mut env, ctx)
}

fn (mut tc TypeChecker) recursive_str_process_child_sequence(node flat.Node, mut env RecursiveStrEnv, ctx RecursiveStrContext) bool {
	mut i := 0
	for i < int(node.children_count) {
		child_id := tc.a.child(&node, i)
		if tc.a.node(child_id).kind == .param {
			i++
			continue
		}
		if tc.recursive_str_process_stmt(child_id, mut env, ctx) {
			i++
			continue
		}
		if env.pending_goto.len > 0 {
			if target := tc.recursive_str_forward_label_index(node, i, env.pending_goto) {
				env.pending_goto = ''
				i = target + 1
				continue
			}
		}
		return false
	}
	return true
}

fn (tc &TypeChecker) recursive_str_forward_label_index(node flat.Node, after int, label string) ?int {
	for i in after + 1 .. int(node.children_count) {
		child := tc.a.child_node(&node, i)
		if child.kind == .label_stmt && child.value == label {
			return i
		}
	}
	return none
}

fn (mut tc TypeChecker) recursive_str_process_stmt(id flat.NodeId, mut env RecursiveStrEnv, ctx RecursiveStrContext) bool {
	if !tc.valid_node_id(id) {
		return true
	}
	node := tc.a.node(id)
	match node.kind {
		.decl_assign {
			lhs_ids := tc.multi_assign_lhs_ids(*node)
			rhs_count := tc.multi_assign_rhs_count(*node)
			for i, lhs_id in lhs_ids {
				lhs := tc.a.node(lhs_id)
				if lhs.kind != .ident || lhs.value == '_' {
					continue
				}
				rhs_id := if rhs_count == 1 {
					tc.multi_assign_rhs_id(*node, 0)
				} else if i < rhs_count {
					tc.multi_assign_rhs_id(*node, i)
				} else {
					flat.empty_node
				}
				mut binding := tc.recursive_str_eval_expr(rhs_id, mut env, ctx)
				if binding.typ_name.len == 0 && tc.valid_node_id(rhs_id) {
					binding.typ_name = tc.resolve_type(rhs_id).name()
				}
				if binding.storage_id == 0 {
					binding.storage_id = env.next_storage_id
					env.next_storage_id++
				}
				env.bindings[lhs.value] = binding
			}
			return true
		}
		.assign {
			if node.children_count >= 2 {
				lhs_id := tc.a.child(node, 0)
				rhs_id := tc.a.child(node, node.children_count - 1)
				mut binding := tc.recursive_str_eval_expr(rhs_id, mut env, ctx)
				lhs := tc.a.node(lhs_id)
				if lhs.kind == .ident {
					if binding.typ_name.len == 0 {
						binding.typ_name = tc.resolve_type(rhs_id).name()
					}
					if binding.storage_id == 0 {
						binding.storage_id = env.next_storage_id
						env.next_storage_id++
					}
					env.bindings[lhs.value] = binding
				} else {
					tc.recursive_str_apply_mutation(lhs_id, rhs_id, node.op, mut env)
				}
			}
			return true
		}
		.selector_assign, .index_assign {
			if node.children_count >= 2 {
				lhs_id := tc.a.child(node, 0)
				rhs_id := tc.a.child(node, node.children_count - 1)
				tc.recursive_str_eval_expr(rhs_id, mut env, ctx)
				tc.recursive_str_apply_mutation(lhs_id, rhs_id, node.op, mut env)
			}
			return true
		}
		.expr_stmt {
			for i in 0 .. node.children_count {
				tc.recursive_str_eval_expr(tc.a.child(node, i), mut env, ctx)
			}
			return !tc.expr_never_returns(id)
		}
		.return_stmt {
			for i in 0 .. node.children_count {
				tc.recursive_str_eval_expr(tc.a.child(node, i), mut env, ctx)
			}
			return false
		}
		.goto_stmt {
			env.pending_goto = node.value
			return false
		}
		.assert_stmt {
			// Assertions can be removed from production builds, so their mutations
			// cannot establish progress for a later recursive call.
			mut assert_env := env.clone_env()
			for i in 0 .. node.children_count {
				tc.recursive_str_eval_expr(tc.a.child(node, i), mut assert_env, ctx)
			}
			return true
		}
		.defer_stmt {
			// Deferred statements execute after the current path has finished. Check their
			// bodies without letting deferred mutations affect earlier recursive calls.
			mut deferred_env := env.clone_env()
			for i in 0 .. node.children_count {
				tc.recursive_str_process_stmt(tc.a.child(node, i), mut deferred_env, ctx)
			}
			return true
		}
		.block {
			return tc.recursive_str_process_child_sequence(*node, mut env, ctx)
		}
		.if_expr {
			return tc.recursive_str_process_if_stmt(id, mut env, ctx)
		}
		.match_stmt {
			return tc.recursive_str_process_match_stmt(id, mut env, ctx)
		}
		.select_stmt {
			return tc.recursive_str_process_select_stmt(id, mut env, ctx)
		}
		.for_stmt, .for_in_stmt {
			return tc.recursive_str_process_loop_stmt(id, mut env, ctx)
		}
		else {
			tc.recursive_str_eval_expr(id, mut env, ctx)
			return true
		}
	}
}

fn (mut tc TypeChecker) recursive_str_process_select_stmt(id flat.NodeId, mut env RecursiveStrEnv, ctx RecursiveStrContext) bool {
	node := tc.a.node(id)
	base := env.clone_env()
	mut branch_envs := []RecursiveStrEnv{}
	for i in 0 .. node.children_count {
		branch_id := tc.a.child(node, i)
		branch := tc.a.node(branch_id)
		if branch.kind != .select_branch {
			continue
		}
		mut branch_env := base.clone_env()
		mut falls_through := true
		for j in 0 .. branch.children_count {
			if !tc.recursive_str_process_stmt(tc.a.child(branch, j), mut branch_env, ctx) {
				falls_through = false
				break
			}
		}
		if falls_through {
			branch_envs << branch_env
		}
	}
	if branch_envs.len == 0 {
		return false
	}
	env = tc.recursive_str_merge_envs(branch_envs)
	return true
}

fn (mut tc TypeChecker) recursive_str_process_loop_stmt(id flat.NodeId, mut env RecursiveStrEnv, ctx RecursiveStrContext) bool {
	node := tc.a.node(id)
	mut body_start := 0
	if node.kind == .for_stmt {
		if node.children_count < 3 {
			return true
		}
		tc.recursive_str_process_stmt(tc.a.child(node, 0), mut env, ctx)
		tc.recursive_str_eval_condition(tc.a.child(node, 1), mut env, ctx)
		body_start = 3
	} else {
		body_start = if node.value.is_int() { node.value.int() } else { 3 }
		for i in 2 .. int_min(body_start, int(node.children_count)) {
			tc.recursive_str_eval_expr(tc.a.child(node, i), mut env, ctx)
		}
	}
	base := env.clone_env()
	mut flow := tc.recursive_str_process_loop_sequence(*node, body_start, [
		base.clone_env(),
	], ctx)
	if node.kind == .for_stmt {
		mut after_post := []RecursiveStrEnv{}
		for loop_path in flow.fallthrough {
			mut post_env := loop_path.clone_env()
			if tc.recursive_str_process_stmt(tc.a.child(node, 2), mut post_env, ctx) {
				after_post << post_env
			}
		}
		flow = RecursiveStrLoopFlow{
			fallthrough: after_post
			breaks:      flow.breaks
		}
	}
	mut paths := flow.breaks.clone()
	if node.kind != .for_stmt || !tc.recursive_str_loop_guarantees_entry(*node) {
		paths << base
		paths << flow.fallthrough
	}
	if paths.len == 0 {
		return false
	}
	env = tc.recursive_str_merge_envs(paths)
	return true
}

fn (tc &TypeChecker) recursive_str_loop_guarantees_entry(node flat.Node) bool {
	if node.children_count < 2 {
		return false
	}
	condition_id := tc.a.child(&node, 1)
	if tc.a.node(condition_id).kind == .empty {
		return true
	}
	return tc.constant_bool_value(condition_id) or { false }
}

fn (mut tc TypeChecker) recursive_str_process_loop_sequence(node flat.Node, start int, initial []RecursiveStrEnv, ctx RecursiveStrContext) RecursiveStrLoopFlow {
	mut active := initial.clone()
	mut breaks := []RecursiveStrEnv{}
	for i in start .. int(node.children_count) {
		mut next := []RecursiveStrEnv{}
		for loop_path in active {
			flow := tc.recursive_str_process_loop_control_stmt(tc.a.child(&node, i), loop_path, ctx)
			next << flow.fallthrough
			breaks << flow.breaks
		}
		active = next.clone()
		if active.len == 0 {
			break
		}
	}
	return RecursiveStrLoopFlow{
		fallthrough: active
		breaks:      breaks
	}
}

fn (mut tc TypeChecker) recursive_str_process_loop_control_stmt(id flat.NodeId, source RecursiveStrEnv, ctx RecursiveStrContext) RecursiveStrLoopFlow {
	if !tc.valid_node_id(id) {
		return RecursiveStrLoopFlow{
			fallthrough: [source]
		}
	}
	node := tc.a.node(id)
	match node.kind {
		.break_stmt {
			return RecursiveStrLoopFlow{
				breaks: [source]
			}
		}
		.continue_stmt {
			return RecursiveStrLoopFlow{}
		}
		.block {
			return tc.recursive_str_process_loop_sequence(*node, 0, [source], ctx)
		}
		.if_expr {
			return tc.recursive_str_process_loop_if(*node, source, ctx)
		}
		.match_stmt {
			return tc.recursive_str_process_loop_match(*node, source, ctx)
		}
		.select_stmt {
			return tc.recursive_str_process_loop_select(*node, source, ctx)
		}
		else {
			mut env := source.clone_env()
			if tc.recursive_str_process_stmt(id, mut env, ctx) {
				return RecursiveStrLoopFlow{
					fallthrough: [env]
				}
			}
		}
	}
	return RecursiveStrLoopFlow{}
}

fn (mut tc TypeChecker) recursive_str_process_loop_if(node flat.Node, source RecursiveStrEnv, ctx RecursiveStrContext) RecursiveStrLoopFlow {
	mut base := source.clone_env()
	mut fallthrough := []RecursiveStrEnv{}
	mut breaks := []RecursiveStrEnv{}
	mut has_else := false
	mut i := 0
	for i < node.children_count {
		child_id := tc.a.child(&node, i)
		child := tc.a.node(child_id)
		if child.kind == .block {
			has_else = true
			flow := tc.recursive_str_process_loop_sequence(*child, 0, [
				base.clone_env(),
			], ctx)
			fallthrough << flow.fallthrough
			breaks << flow.breaks
			i++
			continue
		}
		mut condition_env := base.clone_env()
		tc.recursive_str_eval_condition(child_id, mut condition_env, ctx)
		if i + 1 < node.children_count {
			block := tc.a.child_node(&node, i + 1)
			if block.kind == .block {
				flow := tc.recursive_str_process_loop_sequence(*block, 0, [
					condition_env.clone_env(),
				], ctx)
				fallthrough << flow.fallthrough
				breaks << flow.breaks
				base = condition_env
				i += 2
				continue
			}
		}
		i++
	}
	if !has_else {
		fallthrough << base
	}
	return RecursiveStrLoopFlow{
		fallthrough: fallthrough
		breaks:      breaks
	}
}

fn (mut tc TypeChecker) recursive_str_process_loop_match(node flat.Node, source RecursiveStrEnv, ctx RecursiveStrContext) RecursiveStrLoopFlow {
	mut base := source.clone_env()
	if node.children_count > 0 {
		tc.recursive_str_eval_expr(tc.a.child(&node, 0), mut base, ctx)
	}
	mut fallthrough := []RecursiveStrEnv{}
	mut breaks := []RecursiveStrEnv{}
	for i in 1 .. node.children_count {
		branch := tc.a.child_node(&node, i)
		if branch.kind != .match_branch {
			continue
		}
		start := if branch.value.is_int() { branch.value.int() } else { 0 }
		flow := tc.recursive_str_process_loop_sequence(*branch, start, [
			base.clone_env(),
		], ctx)
		fallthrough << flow.fallthrough
		breaks << flow.breaks
	}
	if !tc.match_has_else_or_exhaustive_coverage(node) {
		fallthrough << base
	}
	return RecursiveStrLoopFlow{
		fallthrough: fallthrough
		breaks:      breaks
	}
}

fn (mut tc TypeChecker) recursive_str_process_loop_select(node flat.Node, source RecursiveStrEnv, ctx RecursiveStrContext) RecursiveStrLoopFlow {
	mut fallthrough := []RecursiveStrEnv{}
	mut breaks := []RecursiveStrEnv{}
	for i in 0 .. node.children_count {
		branch := tc.a.child_node(&node, i)
		if branch.kind != .select_branch {
			continue
		}
		flow := tc.recursive_str_process_loop_sequence(*branch, 0, [
			source.clone_env(),
		], ctx)
		fallthrough << flow.fallthrough
		breaks << flow.breaks
	}
	return RecursiveStrLoopFlow{
		fallthrough: fallthrough
		breaks:      breaks
	}
}

fn (mut tc TypeChecker) recursive_str_eval_expr(id flat.NodeId, mut env RecursiveStrEnv, ctx RecursiveStrContext) RecursiveStrBinding {
	if !tc.valid_node_id(id) {
		return RecursiveStrBinding{}
	}
	node := tc.a.node(id)
	match node.kind {
		.ident {
			return env.bindings[node.value] or {
				RecursiveStrBinding{
					typ_name: tc.resolve_type(id).name()
				}
			}
		}
		.paren, .prefix, .cast_expr, .as_expr, .dump_expr {
			if node.children_count > 0 {
				return tc.recursive_str_eval_expr(tc.a.child(node, 0), mut env, ctx)
			}
		}
		.selector {
			if node.children_count > 0 {
				receiver_id := tc.a.child(node, 0)
				mut binding := tc.recursive_str_eval_expr(receiver_id, mut env, ctx)
				if node.value == 'str' && binding.can_recurse && tc.expr_is_method_value(id)
					&& tc.recursive_str_method_value_targets_current(id, receiver_id, ctx) {
					binding.is_recursive_str_method = true
					binding.typ_name = tc.resolve_type(id).name()
					return binding
				}
				return RecursiveStrBinding{
					typ_name: tc.resolve_type(id).name()
				}
			}
		}
		.index {
			if node.children_count > 0 {
				base := tc.recursive_str_eval_expr(tc.a.child(node, 0), mut env, ctx)
				for i in 1 .. node.children_count {
					tc.recursive_str_eval_expr(tc.a.child(node, i), mut env, ctx)
				}
				index_id := if node.children_count > 1 {
					tc.a.child(node, 1)
				} else {
					flat.empty_node
				}
				return tc.recursive_str_index_binding(base, index_id, id)
			}
		}
		.array_literal, .array_init {
			return tc.recursive_str_eval_array_expr(id, *node, mut env, ctx)
		}
		.assoc {
			return tc.recursive_str_eval_struct_update(id, *node, mut env, ctx)
		}
		.block {
			return tc.recursive_str_eval_block_value(*node, mut env, ctx)
		}
		.if_expr {
			return tc.recursive_str_eval_if_expr(id, mut env, ctx)
		}
		.match_stmt {
			return tc.recursive_str_eval_match_expr(id, mut env, ctx)
		}
		.select_stmt {
			tc.recursive_str_process_select_stmt(id, mut env, ctx)
		}
		.or_expr {
			if node.children_count == 0 {
				return RecursiveStrBinding{}
			}
			source_result := tc.recursive_str_eval_expr(tc.a.child(node, 0), mut env, ctx)
			if node.children_count < 2 || node.value in ['!', '?'] {
				return source_result
			}
			base := env.clone_env()
			mut fallback_env := base.clone_env()
			fallback_result :=
				tc.recursive_str_eval_expr(tc.a.child(node, 1), mut fallback_env, ctx)
			env = tc.recursive_str_merge_envs([base, fallback_env])
			return tc.recursive_str_merge_bindings([source_result, fallback_result])
		}
		.call {
			return tc.recursive_str_eval_call(id, mut env, ctx)
		}
		.fn_literal, .lambda_expr, .spawn_expr {
			// Creating a closure or starting asynchronous work does not execute its body
			// synchronously before the following recursive call.
		}
		.postfix {
			if node.children_count > 0 {
				target_id := tc.a.child(node, 0)
				tc.recursive_str_apply_mutation(target_id, flat.empty_node, node.op, mut env)
			}
		}
		.infix {
			if node.op in [.logical_or, .logical_and] && node.children_count >= 2 {
				tc.recursive_str_eval_expr(tc.a.child(node, 0), mut env, ctx)
				base := env.clone_env()
				mut rhs_env := base.clone_env()
				tc.recursive_str_eval_expr(tc.a.child(node, 1), mut rhs_env, ctx)
				env = tc.recursive_str_merge_envs([base, rhs_env])
				return RecursiveStrBinding{
					typ_name: tc.resolve_type(id).name()
				}
			}
			for i in 0 .. node.children_count {
				tc.recursive_str_eval_expr(tc.a.child(node, i), mut env, ctx)
			}
			if node.op == .left_shift && node.children_count >= 2 {
				lhs_id := tc.a.child(node, 0)
				if _ := array_type_from_receiver(tc.resolve_type(lhs_id)) {
					tc.recursive_str_apply_mutation(lhs_id, tc.a.child(node, 1), node.op, mut env)
				}
			}
		}
		else {
			for i in 0 .. node.children_count {
				tc.recursive_str_eval_expr(tc.a.child(node, i), mut env, ctx)
			}
		}
	}
	return RecursiveStrBinding{
		typ_name: tc.resolve_type(id).name()
	}
}

fn (mut tc TypeChecker) recursive_str_eval_struct_update(id flat.NodeId, node flat.Node, mut env RecursiveStrEnv, ctx RecursiveStrContext) RecursiveStrBinding {
	if node.children_count == 0 {
		return RecursiveStrBinding{
			typ_name: tc.resolve_type(id).name()
		}
	}
	base_id := tc.a.child(&node, 0)
	mut binding := tc.recursive_str_eval_expr(base_id, mut env, ctx)
	for i in 1 .. node.children_count {
		field := tc.a.child_node(&node, i)
		for j in 0 .. field.children_count {
			tc.recursive_str_eval_expr(tc.a.child(field, j), mut env, ctx)
		}
		if binding.can_recurse && tc.recursive_str_struct_update_field_changes_base(base_id, *field) {
			binding.progressed = true
		}
	}
	binding.typ_name = tc.resolve_type(id).name()
	return binding
}

fn (tc &TypeChecker) recursive_str_struct_update_field_changes_base(base_id flat.NodeId, field flat.Node) bool {
	if field.kind != .field_init || field.children_count == 0 || field.value.len == 0 {
		return true
	}
	value_id := tc.a.child(&field, 0)
	base_text := tc.source_text_for_node(base_id).trim_space()
	value_text := tc.source_text_for_node(value_id).trim_space()
	return value_text != '${base_text}.${field.value}'
}

fn (mut tc TypeChecker) recursive_str_eval_array_expr(id flat.NodeId, node flat.Node, mut env RecursiveStrEnv, ctx RecursiveStrContext) RecursiveStrBinding {
	mut elements := []RecursiveStrBinding{}
	mut repeated_element := false
	for i in 0 .. node.children_count {
		child_id := tc.a.child(&node, i)
		child := tc.a.node(child_id)
		if node.kind == .array_init && child.kind == .field_init {
			if child.value == 'init' && child.children_count > 0 {
				elements = [
					tc.recursive_str_eval_expr(tc.a.child(child, 0), mut env, ctx),
				]
				repeated_element = true
			} else {
				for j in 0 .. child.children_count {
					tc.recursive_str_eval_expr(tc.a.child(child, j), mut env, ctx)
				}
			}
			continue
		}
		elements << tc.recursive_str_eval_expr(child_id, mut env, ctx)
	}
	return RecursiveStrBinding{
		typ_name:         tc.resolve_type(id).name()
		elements:         elements
		repeated_element: repeated_element
	}
}

fn (tc &TypeChecker) recursive_str_index_binding(base RecursiveStrBinding, index_id flat.NodeId, result_id flat.NodeId) RecursiveStrBinding {
	if base.elements.len > 0 {
		if base.repeated_element {
			return base.elements[0]
		}
		if index := tc.recursive_str_constant_index(index_id) {
			if index >= 0 && index < base.elements.len {
				return base.elements[index]
			}
		}
		return tc.recursive_str_merge_bindings(base.elements)
	}
	return RecursiveStrBinding{
		typ_name: tc.resolve_type(result_id).name()
	}
}

fn (tc &TypeChecker) recursive_str_constant_index(id flat.NodeId) ?int {
	if !tc.valid_node_id(id) {
		return none
	}
	node := tc.a.node(id)
	if node.kind == .int_literal && node.value.is_int() {
		return node.value.int()
	}
	if node.kind in [.paren, .cast_expr] && node.children_count > 0 {
		return tc.recursive_str_constant_index(tc.a.child(node, 0))
	}
	return none
}

fn (mut tc TypeChecker) recursive_str_eval_block_value(node flat.Node, mut env RecursiveStrEnv, ctx RecursiveStrContext) RecursiveStrBinding {
	if node.children_count == 0 {
		return RecursiveStrBinding{}
	}
	for i in 0 .. node.children_count {
		child_id := tc.a.child(&node, i)
		child := tc.a.node(child_id)
		if i == node.children_count - 1 && child.kind == .expr_stmt && child.children_count > 0 {
			return tc.recursive_str_eval_expr(tc.a.child(child, child.children_count - 1), mut env,
				ctx)
		}
		if !tc.recursive_str_process_stmt(child_id, mut env, ctx) {
			return RecursiveStrBinding{}
		}
	}
	return RecursiveStrBinding{}
}

fn (mut tc TypeChecker) recursive_str_eval_call(id flat.NodeId, mut env RecursiveStrEnv, ctx RecursiveStrContext) RecursiveStrBinding {
	node := tc.a.node(id)
	if node.children_count == 0 {
		return RecursiveStrBinding{}
	}
	callee := tc.a.child_node(node, 0)
	mut receiver_binding := RecursiveStrBinding{}
	mut receiver_id := flat.empty_node
	if callee.kind == .selector && callee.children_count > 0 {
		receiver_id = tc.a.child(callee, 0)
		receiver_binding = tc.recursive_str_eval_expr(receiver_id, mut env, ctx)
	}
	if callee.kind == .selector && callee.value == 'str' && receiver_binding.can_recurse
		&& !receiver_binding.progressed
		&& tc.recursive_str_call_targets_current(id, receiver_id, ctx) {
		pos := tc.method_call_name_pos(*node, *callee)
		message := 'cannot call `str()` method recursively'
		if !tc.errors.any(it.msg == message && it.pos == pos) {
			tc.record_error_at(.unknown_fn, message, id, pos)
		}
	} else if callee.kind == .ident {
		binding := env.bindings[callee.value] or { RecursiveStrBinding{} }
		if binding.is_recursive_str_method && binding.can_recurse && !binding.progressed {
			message := 'cannot call `str()` method recursively'
			if !tc.errors.any(it.msg == message && it.pos == callee.pos) {
				tc.record_error_at(.unknown_fn, message, id, callee.pos)
			}
		}
	}
	for i in 1 .. node.children_count {
		arg_id := tc.call_arg_value(tc.a.child(node, i))
		tc.recursive_str_eval_expr(arg_id, mut env, ctx)
	}
	tc.recursive_str_apply_call_mutations(id, mut env)
	if mut returned := tc.recursive_str_call_return_binding(id, env) {
		if returned.typ_name.len == 0 {
			returned.typ_name = tc.resolve_type(id).name()
		}
		return returned
	}
	return RecursiveStrBinding{
		typ_name: tc.resolve_type(id).name()
	}
}

fn (tc &TypeChecker) recursive_str_method_value_targets_current(selector_id flat.NodeId, receiver_id flat.NodeId, ctx RecursiveStrContext) bool {
	if resolved := tc.resolved_fn_value_name(selector_id) {
		if resolved != ctx.fn_name && tc.recursive_str_has_concrete_fn_decl(resolved) {
			return false
		}
	}
	if !tc.valid_node_id(receiver_id) {
		return false
	}
	if tc.recursive_str_receiver_is_concrete_match_variant(selector_id, receiver_id, ctx) {
		return false
	}
	actual := unalias_and_unwrap_pointer_type(tc.resolve_type(receiver_id))
	expected := unalias_and_unwrap_pointer_type(ctx.receiver_type)
	if actual is Unknown || expected is Unknown {
		return true
	}
	return actual.name() == expected.name()
}

fn (tc &TypeChecker) recursive_str_call_targets_current(call_id flat.NodeId, receiver_id flat.NodeId, ctx RecursiveStrContext) bool {
	resolved := tc.resolved_call_name(call_id) or { return false }
	if resolved != ctx.fn_name && tc.recursive_str_has_concrete_fn_decl(resolved) {
		return false
	}
	if !tc.valid_node_id(receiver_id) {
		return false
	}
	if tc.recursive_str_receiver_is_concrete_match_variant(call_id, receiver_id, ctx) {
		return false
	}
	actual := unalias_and_unwrap_pointer_type(tc.resolve_type(receiver_id))
	expected := unalias_and_unwrap_pointer_type(ctx.receiver_type)
	if actual is Unknown || expected is Unknown {
		return true
	}
	if actual.name() == expected.name() {
		return true
	}
	return actual is Interface && !tc.recursive_str_has_concrete_fn_decl(resolved)
}

fn (tc &TypeChecker) recursive_str_receiver_is_concrete_match_variant(call_id flat.NodeId, receiver_id flat.NodeId, ctx RecursiveStrContext) bool {
	root := tc.recursive_str_root_ident(receiver_id) or { return false }
	if root != ctx.receiver_name {
		return false
	}
	expected := unalias_and_unwrap_pointer_type(ctx.receiver_type)
	mut current := call_id
	for _ in 0 .. 32 {
		parent_id := tc.direct_parent_id(current)
		if !tc.valid_node_id(parent_id) {
			return false
		}
		parent := tc.a.node(parent_id)
		if parent.kind == .match_branch {
			if parent.value == 'else' {
				return false
			}
			condition_count := if parent.value.is_int() { parent.value.int() } else { 0 }
			for i in 0 .. int_min(condition_count, int(parent.children_count)) {
				pattern := tc.a.child_node(parent, i)
				if pattern.kind != .ident || !tc.type_name_known(pattern.value) {
					continue
				}
				pattern_type := unalias_and_unwrap_pointer_type(tc.parse_type(pattern.value))
				if pattern_type !is Unknown && pattern_type.name() != expected.name() {
					return true
				}
			}
			return false
		}
		if parent.kind in [.fn_decl, .fn_literal, .lambda_expr] {
			return false
		}
		current = parent_id
	}
	return false
}

fn (tc &TypeChecker) recursive_str_has_concrete_fn_decl(name string) bool {
	for idx in tc.top_level_idx {
		node := tc.a.node(flat.NodeId(idx))
		if node.kind != .fn_decl {
			continue
		}
		if node.value == name || checker_qualified_fn_name(tc.cur_module, node.value) == name
			|| node.value == name.trim_string_left('main.') {
			return true
		}
	}
	return false
}

fn (mut tc TypeChecker) recursive_str_process_if_stmt(id flat.NodeId, mut env RecursiveStrEnv, ctx RecursiveStrContext) bool {
	node := tc.a.node(id)
	if node.children_count == 0 {
		return true
	}
	mut base := env.clone_env()
	mut branch_envs := []RecursiveStrEnv{}
	mut has_else := false
	mut i := 0
	for i < node.children_count {
		child_id := tc.a.child(node, i)
		child := tc.a.node(child_id)
		if child.kind == .block {
			has_else = true
			mut branch_env := base.clone_env()
			if tc.recursive_str_process_stmt(child_id, mut branch_env, ctx) {
				branch_envs << branch_env
			}
			i++
			continue
		}
		mut condition_env := base.clone_env()
		tc.recursive_str_eval_condition(child_id, mut condition_env, ctx)
		if i + 1 < node.children_count {
			block_id := tc.a.child(node, i + 1)
			if tc.a.node(block_id).kind == .block {
				mut branch_env := condition_env.clone_env()
				if tc.recursive_str_process_stmt(block_id, mut branch_env, ctx) {
					branch_envs << branch_env
				}
				base = condition_env
				i += 2
				continue
			}
		}
		i++
	}
	if !has_else {
		branch_envs << base
	}
	if branch_envs.len == 0 {
		return false
	}
	env = tc.recursive_str_merge_envs(branch_envs)
	return true
}

fn (mut tc TypeChecker) recursive_str_process_match_stmt(id flat.NodeId, mut env RecursiveStrEnv, ctx RecursiveStrContext) bool {
	node := tc.a.node(id)
	if node.children_count == 0 {
		return true
	}
	tc.recursive_str_eval_expr(tc.a.child(node, 0), mut env, ctx)
	base := env.clone_env()
	mut branch_envs := []RecursiveStrEnv{}
	exhaustive := tc.match_has_else_or_exhaustive_coverage(*node)
	for i in 1 .. node.children_count {
		branch_id := tc.a.child(node, i)
		branch := tc.a.node(branch_id)
		if branch.kind != .match_branch {
			continue
		}
		mut branch_env := base.clone_env()
		if tc.recursive_str_process_match_branch(*branch, mut branch_env, ctx) {
			branch_envs << branch_env
		}
	}
	if !exhaustive {
		branch_envs << base
	}
	if branch_envs.len == 0 {
		return false
	}
	env = tc.recursive_str_merge_envs(branch_envs)
	return true
}

fn (mut tc TypeChecker) recursive_str_process_match_branch(branch flat.Node, mut env RecursiveStrEnv, ctx RecursiveStrContext) bool {
	condition_count := if branch.value.is_int() { branch.value.int() } else { 0 }
	for i in condition_count .. branch.children_count {
		if !tc.recursive_str_process_stmt(tc.a.child(&branch, i), mut env, ctx) {
			return false
		}
	}
	return true
}

fn (mut tc TypeChecker) recursive_str_eval_if_expr(id flat.NodeId, mut env RecursiveStrEnv, ctx RecursiveStrContext) RecursiveStrBinding {
	node := tc.a.node(id)
	mut base := env.clone_env()
	mut branch_envs := []RecursiveStrEnv{}
	mut results := []RecursiveStrBinding{}
	mut i := 0
	for i < node.children_count {
		child_id := tc.a.child(node, i)
		child := tc.a.node(child_id)
		if child.kind == .block {
			mut branch_env := base.clone_env()
			result := tc.recursive_str_eval_block_value(*child, mut branch_env, ctx)
			if result.typ_name.len > 0 || result.can_recurse {
				results << result
				branch_envs << branch_env
			}
			i++
			continue
		}
		mut condition_env := base.clone_env()
		tc.recursive_str_eval_condition(child_id, mut condition_env, ctx)
		if i + 1 < node.children_count {
			block_id := tc.a.child(node, i + 1)
			block := tc.a.node(block_id)
			if block.kind == .block {
				mut branch_env := condition_env.clone_env()
				result := tc.recursive_str_eval_block_value(*block, mut branch_env, ctx)
				if result.typ_name.len > 0 || result.can_recurse {
					results << result
					branch_envs << branch_env
				}
				base = condition_env
				i += 2
				continue
			}
		}
		i++
	}
	if branch_envs.len > 0 {
		env = tc.recursive_str_merge_envs(branch_envs)
	}
	return tc.recursive_str_merge_bindings(results)
}

fn (mut tc TypeChecker) recursive_str_eval_match_expr(id flat.NodeId, mut env RecursiveStrEnv, ctx RecursiveStrContext) RecursiveStrBinding {
	node := tc.a.node(id)
	if node.children_count == 0 {
		return RecursiveStrBinding{}
	}
	tc.recursive_str_eval_expr(tc.a.child(node, 0), mut env, ctx)
	base := env.clone_env()
	mut branch_envs := []RecursiveStrEnv{}
	mut results := []RecursiveStrBinding{}
	for i in 1 .. node.children_count {
		branch := tc.a.child_node(node, i)
		if branch.kind != .match_branch {
			continue
		}
		mut branch_env := base.clone_env()
		condition_count := if branch.value.is_int() { branch.value.int() } else { 0 }
		mut result := RecursiveStrBinding{}
		mut falls := true
		for j in condition_count .. branch.children_count {
			part_id := tc.a.child(branch, j)
			part := tc.a.node(part_id)
			if j == branch.children_count - 1 && part.kind == .expr_stmt && part.children_count > 0 {
				result = tc.recursive_str_eval_expr(tc.a.child(part, part.children_count - 1), mut
					branch_env, ctx)
			} else if !tc.recursive_str_process_stmt(part_id, mut branch_env, ctx) {
				falls = false
				break
			}
		}
		if falls && (result.typ_name.len > 0 || result.can_recurse) {
			results << result
			branch_envs << branch_env
		}
	}
	if branch_envs.len > 0 {
		env = tc.recursive_str_merge_envs(branch_envs)
	}
	return tc.recursive_str_merge_bindings(results)
}

fn (mut tc TypeChecker) recursive_str_eval_condition(id flat.NodeId, mut env RecursiveStrEnv, ctx RecursiveStrContext) {
	if !tc.valid_node_id(id) {
		return
	}
	node := tc.a.node(id)
	if node.kind == .infix && node.op in [.logical_or, .logical_and] && node.children_count >= 2 {
		tc.recursive_str_eval_condition(tc.a.child(node, 0), mut env, ctx)
		mut conditional_env := env.clone_env()
		tc.recursive_str_eval_condition(tc.a.child(node, 1), mut conditional_env, ctx)
		return
	}
	tc.recursive_str_eval_expr(id, mut env, ctx)
}

fn (tc &TypeChecker) recursive_str_merge_envs(envs []RecursiveStrEnv) RecursiveStrEnv {
	if envs.len == 0 {
		return RecursiveStrEnv{}
	}
	mut result := RecursiveStrEnv{
		next_storage_id: envs[0].next_storage_id
	}
	mut names := map[string]bool{}
	for env in envs {
		result.next_storage_id = int_max(result.next_storage_id, env.next_storage_id)
		for name, _ in env.bindings {
			names[name] = true
		}
	}
	for name, _ in names {
		mut bindings := []RecursiveStrBinding{}
		for env in envs {
			if binding := env.bindings[name] {
				bindings << binding
			}
		}
		result.bindings[name] = tc.recursive_str_merge_bindings(bindings)
	}
	return result
}

fn (tc &TypeChecker) recursive_str_merge_bindings(bindings []RecursiveStrBinding) RecursiveStrBinding {
	if bindings.len == 0 {
		return RecursiveStrBinding{}
	}
	mut can_recurse := false
	mut has_unprogressed := false
	mut is_recursive_str_method := false
	mut storage_id := bindings[0].storage_id
	mut typ_name := bindings[0].typ_name
	mut max_elements := 0
	mut repeated_element := true
	for binding in bindings {
		if binding.can_recurse {
			can_recurse = true
			has_unprogressed = has_unprogressed || !binding.progressed
		}
		is_recursive_str_method = is_recursive_str_method || binding.is_recursive_str_method
		if binding.storage_id != storage_id {
			storage_id = 0
		}
		if typ_name.len == 0 {
			typ_name = binding.typ_name
		}
		max_elements = int_max(max_elements, binding.elements.len)
		repeated_element = repeated_element && binding.repeated_element
	}
	mut elements := []RecursiveStrBinding{}
	for i in 0 .. max_elements {
		mut candidates := []RecursiveStrBinding{}
		for binding in bindings {
			if binding.repeated_element && binding.elements.len > 0 {
				candidates << binding.elements[0]
			} else if i < binding.elements.len {
				candidates << binding.elements[i]
			}
		}
		elements << tc.recursive_str_merge_bindings(candidates)
	}
	return RecursiveStrBinding{
		can_recurse:             can_recurse
		progressed:              can_recurse && !has_unprogressed
		is_recursive_str_method: is_recursive_str_method
		storage_id:              storage_id
		typ_name:                typ_name
		elements:                elements
		repeated_element:        repeated_element && elements.len > 0
	}
}

fn (mut tc TypeChecker) recursive_str_apply_mutation(target_id flat.NodeId, rhs_id flat.NodeId, op flat.Op, mut env RecursiveStrEnv) {
	name := tc.recursive_str_root_ident(target_id) or { return }
	if tc.recursive_str_mutation_is_noop(rhs_id, op) {
		return
	}
	if tc.valid_node_id(rhs_id) && op == .assign
		&& tc.source_text_for_node(target_id).trim_space() == tc.source_text_for_node(rhs_id).trim_space() {
		return
	}
	if tc.recursive_str_expr_contains_index(target_id) {
		tc.recursive_str_mark_shared_progress(name, mut env)
	} else {
		tc.recursive_str_mark_value_progress(name, mut env)
	}
}

fn (tc &TypeChecker) recursive_str_mutation_is_noop(rhs_id flat.NodeId, op flat.Op) bool {
	if !tc.valid_node_id(rhs_id) {
		return false
	}
	node := tc.a.node(rhs_id)
	if node.kind in [.paren, .cast_expr, .expr_stmt] && node.children_count > 0 {
		return tc.recursive_str_mutation_is_noop(tc.a.child(node, 0), op)
	}
	if node.kind !in [.int_literal, .float_literal] {
		return false
	}
	if op in [.plus_assign, .minus_assign, .pipe_assign, .xor_assign, .left_shift_assign,
		.right_shift_assign, .right_shift_unsigned_assign] {
		return numeric_literal_is_zero(node.value)
	}
	if op in [.mul_assign, .div_assign, .power_assign] {
		return numeric_literal_is_one(node.value)
	}
	return false
}

fn numeric_literal_is_one(value string) bool {
	mut clean := value.to_lower().replace('_', '')
	for suffix in ['f32', 'f64', 'u8', 'u16', 'u32', 'u64', 'i8', 'i16', 'i32', 'i64'] {
		if clean.ends_with(suffix) {
			clean = clean[..clean.len - suffix.len]
			break
		}
	}
	return clean in ['1', '1.', '1.0', '0x1', '0b1', '0o1']
}

fn (tc &TypeChecker) recursive_str_root_ident(id flat.NodeId) ?string {
	if !tc.valid_node_id(id) {
		return none
	}
	node := tc.a.node(id)
	if node.kind == .ident {
		return node.value
	}
	if node.kind in [.selector, .index, .paren, .prefix, .as_expr, .cast_expr]
		&& node.children_count > 0 {
		return tc.recursive_str_root_ident(tc.a.child(node, 0))
	}
	return none
}

fn (tc &TypeChecker) recursive_str_expr_contains_index(id flat.NodeId) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.node(id)
	if node.kind == .index {
		return true
	}
	for i in 0 .. node.children_count {
		if tc.recursive_str_expr_contains_index(tc.a.child(node, i)) {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) recursive_str_mark_value_progress(name string, mut env RecursiveStrEnv) {
	mut binding := env.bindings[name] or { return }
	if binding.can_recurse {
		binding.progressed = true
		env.bindings[name] = binding
	}
}

fn (tc &TypeChecker) recursive_str_mark_shared_progress(name string, mut env RecursiveStrEnv) {
	binding := env.bindings[name] or { return }
	if binding.storage_id == 0 {
		tc.recursive_str_mark_value_progress(name, mut env)
		return
	}
	for other_name, other_binding in env.bindings {
		mut other := other_binding
		if other.storage_id == binding.storage_id && other.can_recurse {
			other.progressed = true
			env.bindings[other_name] = other
		}
	}
}

fn (tc &TypeChecker) recursive_str_call_param_actuals(call flat.Node, decl flat.Node) map[string]flat.NodeId {
	mut actual_ids := []flat.NodeId{}
	if call.children_count == 0 {
		return map[string]flat.NodeId{}
	}
	callee := tc.a.child_node(&call, 0)
	if callee.kind == .selector && callee.children_count > 0 {
		actual_ids << tc.a.child(callee, 0)
	}
	for i in 1 .. call.children_count {
		actual_ids << tc.call_arg_value(tc.a.child(&call, i))
	}
	mut actuals := map[string]flat.NodeId{}
	mut actual_index := 0
	for i in 0 .. decl.children_count {
		param := tc.a.child_node(&decl, i)
		if param.kind != .param {
			continue
		}
		if actual_index >= actual_ids.len {
			break
		}
		actuals[param.value] = actual_ids[actual_index]
		actual_index++
	}
	return actuals
}

fn (tc &TypeChecker) recursive_str_binding_for_expr(id flat.NodeId, env RecursiveStrEnv) RecursiveStrBinding {
	if !tc.valid_node_id(id) {
		return RecursiveStrBinding{}
	}
	node := tc.a.node(id)
	match node.kind {
		.ident {
			return env.bindings[node.value] or {
				RecursiveStrBinding{
					typ_name: tc.resolve_type(id).name()
				}
			}
		}
		.paren, .prefix, .cast_expr, .as_expr, .dump_expr {
			if node.children_count > 0 {
				return tc.recursive_str_binding_for_expr(tc.a.child(node, 0), env)
			}
		}
		.selector {
			return RecursiveStrBinding{
				typ_name: tc.resolve_type(id).name()
			}
		}
		.index {
			if node.children_count > 0 {
				base := tc.recursive_str_binding_for_expr(tc.a.child(node, 0), env)
				index_id := if node.children_count > 1 {
					tc.a.child(node, 1)
				} else {
					flat.empty_node
				}
				return tc.recursive_str_index_binding(base, index_id, id)
			}
		}
		.array_literal, .array_init {
			mut elements := []RecursiveStrBinding{}
			mut repeated_element := false
			for i in 0 .. node.children_count {
				child_id := tc.a.child(node, i)
				child := tc.a.node(child_id)
				if node.kind == .array_init && child.kind == .field_init {
					if child.value == 'init' && child.children_count > 0 {
						elements = [
							tc.recursive_str_binding_for_expr(tc.a.child(child, 0), env),
						]
						repeated_element = true
					}
					continue
				}
				elements << tc.recursive_str_binding_for_expr(child_id, env)
			}
			return RecursiveStrBinding{
				typ_name:         tc.resolve_type(id).name()
				elements:         elements
				repeated_element: repeated_element
			}
		}
		.assoc {
			if node.children_count > 0 {
				base_id := tc.a.child(node, 0)
				mut binding := tc.recursive_str_binding_for_expr(base_id, env)
				for i in 1 .. node.children_count {
					field := tc.a.child_node(node, i)
					if binding.can_recurse
						&& tc.recursive_str_struct_update_field_changes_base(base_id, *field) {
						binding.progressed = true
					}
				}
				binding.typ_name = tc.resolve_type(id).name()
				return binding
			}
		}
		else {}
	}
	return RecursiveStrBinding{
		typ_name: tc.resolve_type(id).name()
	}
}

fn (tc &TypeChecker) recursive_str_returned_param(decl flat.Node) ?RecursiveStrReturnedParam {
	mut params := map[string]int{}
	mut stack := []flat.NodeId{}
	for i in 0 .. decl.children_count {
		child_id := tc.a.child(&decl, i)
		child := tc.a.node(child_id)
		if child.kind == .param {
			params[child.value] = i
		} else {
			stack << child_id
		}
	}
	mut returned := RecursiveStrReturnedParam{}
	mut has_return := false
	for stack.len > 0 {
		id := stack.pop()
		node := tc.a.node(id)
		if node.kind in [.fn_decl, .fn_literal, .lambda_expr] {
			continue
		}
		if node.kind == .return_stmt {
			if node.children_count != 1 {
				return none
			}
			value_id := tc.a.child(node, 0)
			name := tc.recursive_str_root_ident(value_id) or { return none }
			index := params[name] or { return none }
			if has_return && returned.name != name {
				return none
			}
			returned = RecursiveStrReturnedParam{
				name:  name
				index: index
			}
			has_return = true
			continue
		}
		for i in 0 .. node.children_count {
			stack << tc.a.child(node, i)
		}
	}
	if has_return {
		return returned
	}
	return none
}

fn (mut tc TypeChecker) recursive_str_call_return_binding(call_id flat.NodeId, env RecursiveStrEnv) ?RecursiveStrBinding {
	resolved := tc.resolved_call_name(call_id) or { return none }
	decl_id := tc.recursive_str_fn_decl_id(resolved) or { return none }
	decl := tc.a.node(decl_id)
	returned := tc.recursive_str_returned_param(*decl) or { return none }
	call := tc.a.node(call_id)
	actuals := tc.recursive_str_call_param_actuals(*call, *decl)
	actual_id := actuals[returned.name] or { return none }
	mut binding := tc.recursive_str_binding_for_expr(actual_id, env)
	effect := tc.recursive_str_guaranteed_param_effect(*decl, returned.index)
	match effect.kind {
		.value, .shared {
			if binding.can_recurse {
				binding.progressed = true
			}
		}
		.rebind {
			if source_id := actuals[effect.source_param] {
				binding = tc.recursive_str_binding_for_expr(source_id, env)
			}
		}
		else {}
	}
	return binding
}

fn (mut tc TypeChecker) recursive_str_apply_call_mutations(call_id flat.NodeId, mut env RecursiveStrEnv) {
	resolved := tc.resolved_call_name(call_id) or { return }
	decl_id := tc.recursive_str_fn_decl_id(resolved) or { return }
	decl := tc.a.node(decl_id)
	call := tc.a.node(call_id)
	if call.children_count == 0 {
		return
	}
	actuals := tc.recursive_str_call_param_actuals(*call, *decl)
	mut actual_bindings := map[string]RecursiveStrBinding{}
	for name, actual_id in actuals {
		actual_bindings[name] = tc.recursive_str_binding_for_expr(actual_id, env)
	}
	callee := tc.a.child_node(call, 0)
	mut param_index := 0
	if callee.kind == .selector && callee.children_count > 0 {
		receiver_id := tc.a.child(callee, 0)
		effect := tc.recursive_str_guaranteed_param_effect(*decl, 0)
		source_binding := actual_bindings[effect.source_param] or { RecursiveStrBinding{} }
		tc.recursive_str_apply_effect_to_target(receiver_id, effect, source_binding, mut env)
		param_index = 1
	}
	for i in 1 .. call.children_count {
		arg_id := tc.call_arg_value(tc.a.child(call, i))
		for param_index < decl.children_count && tc.a.child_node(decl, param_index).kind != .param {
			param_index++
		}
		if param_index >= decl.children_count {
			break
		}
		param := tc.a.child_node(decl, param_index)
		if param.is_mut {
			effect := tc.recursive_str_guaranteed_param_effect(*decl, param_index)
			source_binding := actual_bindings[effect.source_param] or { RecursiveStrBinding{} }
			tc.recursive_str_apply_effect_to_target(arg_id, effect, source_binding, mut env)
		}
		param_index++
	}
}

fn (mut tc TypeChecker) recursive_str_apply_effect_to_target(target_id flat.NodeId, effect RecursiveStrParamEffect, source_binding RecursiveStrBinding, mut env RecursiveStrEnv) {
	name := tc.recursive_str_root_ident(target_id) or { return }
	match effect.kind {
		.value {
			tc.recursive_str_mark_value_progress(name, mut env)
		}
		.shared {
			tc.recursive_str_mark_shared_progress(name, mut env)
		}
		.rebind {
			old := env.bindings[name] or { return }
			if effect.source_param.len > 0
				&& (source_binding.can_recurse || source_binding.storage_id != 0) {
				mut rebound := source_binding
				if rebound.typ_name.len == 0 {
					rebound.typ_name = old.typ_name
				}
				env.bindings[name] = rebound
				return
			}
			env.bindings[name] = RecursiveStrBinding{
				storage_id: env.next_storage_id
				typ_name:   old.typ_name
			}
			env.next_storage_id++
		}
		else {}
	}
}

fn (tc &TypeChecker) recursive_str_fn_decl_id(name string) ?flat.NodeId {
	for idx in tc.top_level_idx {
		id := flat.NodeId(idx)
		node := tc.a.node(id)
		if node.kind != .fn_decl {
			continue
		}
		if node.value == name || checker_qualified_fn_name(tc.cur_module, node.value) == name
			|| node.value == name.trim_string_left('main.') {
			return id
		}
	}
	return none
}

fn (mut tc TypeChecker) recursive_str_guaranteed_param_effect(decl flat.Node, param_index int) RecursiveStrParamEffect {
	return tc.recursive_str_guaranteed_param_effect_depth(decl, param_index, 0)
}

fn (mut tc TypeChecker) recursive_str_guaranteed_param_effect_depth(decl flat.Node, param_index int, depth int) RecursiveStrParamEffect {
	if depth >= 32 {
		return RecursiveStrParamEffect{}
	}
	if param_index < 0 || param_index >= decl.children_count {
		return RecursiveStrParamEffect{}
	}
	param := tc.a.child_node(&decl, param_index)
	if param.kind != .param {
		return RecursiveStrParamEffect{}
	}
	mut result := RecursiveStrParamEffect{}
	mut terminal := []RecursiveStrParamEffect{}
	for i in 0 .. decl.children_count {
		child_id := tc.a.child(&decl, i)
		child := tc.a.node(child_id)
		if child.kind == .param {
			continue
		}
		conditional_id := tc.recursive_str_conditional_stmt_id(child_id) or { flat.empty_node }
		if tc.valid_node_id(conditional_id) {
			flow := tc.recursive_str_conditional_param_flow(conditional_id, param.value, depth,
				result)
			terminal << flow.terminal
			if !flow.has_fallthrough {
				return recursive_str_merge_param_effects(terminal)
			}
			result = flow.fallthrough
			continue
		}
		effect := tc.recursive_str_stmt_param_effect(child_id, param.value, depth)
		if effect.kind != .none {
			result = effect
		}
		if tc.recursive_str_stmt_may_return(*child) {
			terminal << result
			return recursive_str_merge_param_effects(terminal)
		}
	}
	if terminal.len == 0 {
		return result
	}
	terminal << result
	return recursive_str_merge_param_effects(terminal)
}

fn (tc &TypeChecker) recursive_str_stmt_may_return(node flat.Node) bool {
	if node.kind == .return_stmt {
		return true
	}
	if node.kind in [.fn_decl, .fn_literal, .lambda_expr] {
		return false
	}
	for i in 0 .. node.children_count {
		if tc.recursive_str_stmt_may_return(*tc.a.child_node(&node, i)) {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) recursive_str_stmt_always_returns(node flat.Node) bool {
	if node.kind == .return_stmt {
		return true
	}
	if node.kind in [.fn_decl, .fn_literal, .lambda_expr] {
		return false
	}
	if node.kind in [.block, .match_branch] {
		for i in 0 .. node.children_count {
			if tc.recursive_str_stmt_always_returns(*tc.a.child_node(&node, i)) {
				return true
			}
		}
		return false
	}
	if node.kind == .if_expr {
		mut blocks := 0
		mut all_return := true
		mut has_else := false
		for i in 0 .. node.children_count {
			child := tc.a.child_node(&node, i)
			if child.kind != .block {
				continue
			}
			blocks++
			all_return = all_return && tc.recursive_str_stmt_always_returns(*child)
			if i == node.children_count - 1 && i % 2 == 0 {
				has_else = true
			}
		}
		return has_else && blocks > 1 && all_return
	}
	if node.kind == .match_stmt {
		if !tc.match_has_else_or_exhaustive_coverage(node) {
			return false
		}
		for i in 1 .. node.children_count {
			branch := tc.a.child_node(&node, i)
			if branch.kind != .match_branch || !tc.recursive_str_stmt_always_returns(*branch) {
				return false
			}
		}
		return true
	}
	return false
}

fn (mut tc TypeChecker) recursive_str_stmt_param_effect(id flat.NodeId, name string, depth int) RecursiveStrParamEffect {
	if !tc.valid_node_id(id) {
		return RecursiveStrParamEffect{}
	}
	node := tc.a.node(id)
	match node.kind {
		.assign {
			if node.children_count >= 2 {
				lhs_id := tc.a.child(node, 0)
				rhs_id := tc.a.child(node, node.children_count - 1)
				lhs := tc.a.node(lhs_id)
				if tc.recursive_str_mutation_is_noop(rhs_id, node.op) {
					return RecursiveStrParamEffect{}
				}
				if lhs.kind == .ident && lhs.value == name {
					if tc.source_text_for_node(lhs_id).trim_space() != tc.source_text_for_node(rhs_id).trim_space() {
						return RecursiveStrParamEffect{
							kind:         .rebind
							source_param: tc.recursive_str_root_ident(rhs_id) or { '' }
						}
					}
					return RecursiveStrParamEffect{}
				}
				if root := tc.recursive_str_root_ident(lhs_id) {
					if root != name {
						return RecursiveStrParamEffect{}
					}
					if tc.source_text_for_node(lhs_id).trim_space() == tc.source_text_for_node(rhs_id).trim_space() {
						return RecursiveStrParamEffect{}
					}
					return if tc.recursive_str_expr_contains_index(lhs_id) {
						RecursiveStrParamEffect{
							kind: .shared
						}
					} else {
						RecursiveStrParamEffect{
							kind: .value
						}
					}
				}
			}
		}
		.selector_assign, .index_assign {
			if node.children_count >= 2 {
				lhs_id := tc.a.child(node, 0)
				rhs_id := tc.a.child(node, node.children_count - 1)
				if tc.recursive_str_mutation_is_noop(rhs_id, node.op) {
					return RecursiveStrParamEffect{}
				}
				if root := tc.recursive_str_root_ident(lhs_id) {
					if root != name {
						return RecursiveStrParamEffect{}
					}
					if tc.source_text_for_node(lhs_id).trim_space() == tc.source_text_for_node(rhs_id).trim_space() {
						return RecursiveStrParamEffect{}
					}
					return if tc.recursive_str_expr_contains_index(lhs_id) {
						RecursiveStrParamEffect{
							kind: .shared
						}
					} else {
						RecursiveStrParamEffect{
							kind: .value
						}
					}
				}
			}
		}
		.postfix {
			if node.children_count > 0 {
				target_id := tc.a.child(node, 0)
				if root := tc.recursive_str_root_ident(target_id) {
					if root != name {
						return RecursiveStrParamEffect{}
					}
					return if tc.recursive_str_expr_contains_index(target_id) {
						RecursiveStrParamEffect{
							kind: .shared
						}
					} else {
						RecursiveStrParamEffect{
							kind: .value
						}
					}
				}
			}
		}
		.infix {
			if node.op == .left_shift && node.children_count >= 2 {
				lhs_id := tc.a.child(node, 0)
				if _ := array_type_from_receiver(tc.resolve_type(lhs_id)) {
					if root := tc.recursive_str_root_ident(lhs_id) {
						if root != name {
							return RecursiveStrParamEffect{}
						}
						return if tc.recursive_str_expr_contains_index(lhs_id) {
							RecursiveStrParamEffect{
								kind: .shared
							}
						} else {
							RecursiveStrParamEffect{
								kind: .value
							}
						}
					}
				}
			}
		}
		.call {
			return tc.recursive_str_call_param_effect(id, name, depth)
		}
		.expr_stmt {
			mut result := RecursiveStrParamEffect{}
			for i in 0 .. node.children_count {
				child_id := tc.a.child(node, i)
				child := tc.a.node(child_id)
				effect := tc.recursive_str_stmt_param_effect(child_id, name, depth)
				if effect.kind != .none {
					result = effect
				}
				if tc.recursive_str_stmt_may_return(*child) {
					return result
				}
			}
			return result
		}
		.block {
			mut result := RecursiveStrParamEffect{}
			mut terminal := []RecursiveStrParamEffect{}
			for i in 0 .. node.children_count {
				child_id := tc.a.child(node, i)
				child := tc.a.node(child_id)
				conditional_id := tc.recursive_str_conditional_stmt_id(child_id) or {
					flat.empty_node
				}
				if tc.valid_node_id(conditional_id) {
					flow := tc.recursive_str_conditional_param_flow(conditional_id, name, depth,
						result)
					terminal << flow.terminal
					if !flow.has_fallthrough {
						return recursive_str_merge_param_effects(terminal)
					}
					result = flow.fallthrough
					continue
				}
				effect := tc.recursive_str_stmt_param_effect(child_id, name, depth)
				if effect.kind != .none {
					result = effect
				}
				if tc.recursive_str_stmt_may_return(*child) {
					terminal << result
					return recursive_str_merge_param_effects(terminal)
				}
			}
			if terminal.len == 0 {
				return result
			}
			terminal << result
			return recursive_str_merge_param_effects(terminal)
		}
		.if_expr {
			flow :=
				tc.recursive_str_conditional_param_flow(id, name, depth, RecursiveStrParamEffect{})
			mut effects := flow.terminal.clone()
			if flow.has_fallthrough {
				effects << flow.fallthrough
			}
			return recursive_str_merge_param_effects(effects)
		}
		.match_stmt {
			mut effects := []RecursiveStrParamEffect{}
			for i in 1 .. node.children_count {
				branch_id := tc.a.child(node, i)
				branch := tc.a.node(branch_id)
				if branch.kind != .match_branch {
					continue
				}
				effects << tc.recursive_str_stmt_param_effect(branch_id, name, depth)
			}
			if tc.match_has_else_or_exhaustive_coverage(*node) && effects.len > 0 {
				return recursive_str_merge_param_effects(effects)
			}
		}
		.match_branch {
			condition_count := if node.value.is_int() { node.value.int() } else { 0 }
			mut result := RecursiveStrParamEffect{}
			for i in condition_count .. node.children_count {
				child_id := tc.a.child(node, i)
				child := tc.a.node(child_id)
				effect := tc.recursive_str_stmt_param_effect(child_id, name, depth)
				if effect.kind != .none {
					result = effect
				}
				if tc.recursive_str_stmt_may_return(*child) {
					return result
				}
			}
			return result
		}
		else {}
	}
	return RecursiveStrParamEffect{}
}

fn (tc &TypeChecker) recursive_str_conditional_stmt_id(id flat.NodeId) ?flat.NodeId {
	if !tc.valid_node_id(id) {
		return none
	}
	node := tc.a.node(id)
	if node.kind == .if_expr {
		return id
	}
	if node.kind == .expr_stmt && node.children_count == 1 {
		child_id := tc.a.child(node, 0)
		if tc.a.node(child_id).kind == .if_expr {
			return child_id
		}
	}
	return none
}

fn (mut tc TypeChecker) recursive_str_conditional_param_flow(id flat.NodeId, name string, depth int, incoming RecursiveStrParamEffect) RecursiveStrConditionalParamFlow {
	node := tc.a.node(id)
	mut terminal := []RecursiveStrParamEffect{}
	mut fallthrough := []RecursiveStrParamEffect{}
	mut has_else := false
	for i in 0 .. node.children_count {
		child_id := tc.a.child(node, i)
		child := tc.a.node(child_id)
		if child.kind != .block {
			continue
		}
		if i == node.children_count - 1 && i % 2 == 0 {
			has_else = true
		}
		effect := tc.recursive_str_stmt_param_effect(child_id, name, depth)
		final_effect := if effect.kind == .none { incoming } else { effect }
		if tc.recursive_str_stmt_may_return(*child) {
			terminal << final_effect
		}
		if !tc.recursive_str_stmt_always_returns(*child) {
			fallthrough << final_effect
		}
	}
	if !has_else {
		fallthrough << incoming
	}
	return RecursiveStrConditionalParamFlow{
		fallthrough:     recursive_str_merge_param_effects(fallthrough)
		terminal:        terminal
		has_fallthrough: fallthrough.len > 0
	}
}

fn (mut tc TypeChecker) recursive_str_call_param_effect(call_id flat.NodeId, name string, depth int) RecursiveStrParamEffect {
	if depth >= 32 {
		return RecursiveStrParamEffect{}
	}
	resolved := tc.resolved_call_name(call_id) or {
		tc.assignment_rhs_call_name(call_id) or { return RecursiveStrParamEffect{} }
	}
	decl_id := tc.recursive_str_fn_decl_id(resolved) or { return RecursiveStrParamEffect{} }
	decl := tc.a.node(decl_id)
	call := tc.a.node(call_id)
	actuals := tc.recursive_str_call_param_actuals(*call, *decl)
	mut result := RecursiveStrParamEffect{}
	for param_index in 0 .. decl.children_count {
		param := tc.a.child_node(decl, param_index)
		if param.kind != .param {
			continue
		}
		actual_id := actuals[param.value] or { continue }
		if tc.recursive_str_root_ident(actual_id) or { continue } != name {
			continue
		}
		mut effect := tc.recursive_str_guaranteed_param_effect_depth(*decl, param_index, depth + 1)
		if effect.kind == .rebind && effect.source_param.len > 0 {
			source_id := actuals[effect.source_param] or { flat.empty_node }
			effect = RecursiveStrParamEffect{
				...effect
				source_param: tc.recursive_str_root_ident(source_id) or { '' }
			}
		}
		if effect.kind != .none {
			result = effect
		}
	}
	return result
}

fn recursive_str_merge_param_effects(effects []RecursiveStrParamEffect) RecursiveStrParamEffect {
	if effects.len == 0 || effects[0].kind == .none {
		return RecursiveStrParamEffect{}
	}
	first := effects[0]
	for effect in effects[1..] {
		if effect.kind != first.kind || effect.source_param != first.source_param {
			return RecursiveStrParamEffect{}
		}
	}
	return first
}
