module types

import strconv
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

struct RecursiveStrParamLoopFlow {
	fallthrough []RecursiveStrParamEffect
	breaks      []RecursiveStrParamEffect
	continues   []RecursiveStrParamEffect
	returns     []RecursiveStrParamEffect
}

struct RecursiveStrReturnedParam {
	name     string
	index    int
	slot     int
	value_id flat.NodeId
}

struct RecursiveStrAssignmentValue {
	binding RecursiveStrBinding
	rhs_id  flat.NodeId
}

enum RecursiveStrAggregateSlotKind {
	index
	field
}

struct RecursiveStrAggregateSlot {
	kind     RecursiveStrAggregateSlotKind
	index_id flat.NodeId
	key      string
}

struct RecursiveStrBinding {
mut:
	can_recurse             bool
	progressed              bool
	nonreversible_progress  bool
	numeric_deltas          map[string]i64
	is_recursive_str_method bool
	storage_id              int
	typ_name                string
	elements                []RecursiveStrBinding
	element_keys            []string
	repeated_element        bool
	closure_ids             []flat.NodeId
	closure_capture_names   []string
	closure_captures        []RecursiveStrBinding
}

struct RecursiveStrEnv {
mut:
	bindings           map[string]RecursiveStrBinding
	next_storage_id    int = 2
	pending_goto       string
	active_closure_ids map[int]bool
	active_helper_ids  map[int]bool
	defer_scopes       [][]flat.NodeId
	known_values       map[string]string
	excluded_values    map[string]map[string]bool
}

struct RecursiveStrLoopFlow {
	fallthrough []RecursiveStrEnv
	breaks      []RecursiveStrEnv
	continues   []RecursiveStrEnv
	returns     []RecursiveStrEnv
}

struct RecursiveStrContext {
	fn_id         flat.NodeId
	fn_name       string
	receiver_name string
	receiver_type Type
}

fn (env &RecursiveStrEnv) clone_env() RecursiveStrEnv {
	mut bindings := env.bindings.clone()
	for name, binding in env.bindings {
		mut cloned := binding
		cloned.numeric_deltas = binding.numeric_deltas.clone()
		bindings[name] = cloned
	}
	mut defer_scopes := [][]flat.NodeId{cap: env.defer_scopes.len}
	for scope in env.defer_scopes {
		defer_scopes << scope.clone()
	}
	mut excluded_values := map[string]map[string]bool{}
	for key, values in env.excluded_values {
		excluded_values[key] = values.clone()
	}
	return RecursiveStrEnv{
		bindings:           bindings
		next_storage_id:    env.next_storage_id
		pending_goto:       env.pending_goto
		active_closure_ids: env.active_closure_ids.clone()
		active_helper_ids:  env.active_helper_ids.clone()
		defer_scopes:       defer_scopes
		known_values:       env.known_values.clone()
		excluded_values:    excluded_values
	}
}

fn recursive_str_push_defer_scope(mut env RecursiveStrEnv) {
	env.defer_scopes << []flat.NodeId{}
}

fn (mut tc TypeChecker) recursive_str_register_defer(id flat.NodeId, node flat.Node, mut env RecursiveStrEnv) {
	if env.defer_scopes.len == 0 {
		recursive_str_push_defer_scope(mut env)
	}
	scope_index := if node.value == 'function' { 0 } else { env.defer_scopes.len - 1 }
	env.defer_scopes[scope_index] << id
}

fn (mut tc TypeChecker) recursive_str_run_current_defer_scope(mut env RecursiveStrEnv, ctx RecursiveStrContext) {
	if env.defer_scopes.len == 0 {
		return
	}
	scope_index := env.defer_scopes.len - 1
	defer_stmts := env.defer_scopes[scope_index].clone()
	env.defer_scopes[scope_index] = []flat.NodeId{}
	for i := defer_stmts.len; i > 0; i-- {
		defer_id := defer_stmts[i - 1]
		if !tc.valid_node_id(defer_id) {
			continue
		}
		defer_node := tc.a.node(defer_id)
		for j in 0 .. defer_node.children_count {
			tc.recursive_str_process_stmt(tc.a.child(defer_node, j), mut env, ctx)
		}
	}
	env.defer_scopes.delete_last()
}

fn (mut tc TypeChecker) recursive_str_close_defer_scope(envs []RecursiveStrEnv, ctx RecursiveStrContext) []RecursiveStrEnv {
	mut closed := []RecursiveStrEnv{cap: envs.len}
	for source in envs {
		mut env := source.clone_env()
		tc.recursive_str_run_current_defer_scope(mut env, ctx)
		closed << env
	}
	return closed
}

fn (mut tc TypeChecker) recursive_str_run_all_defer_scopes(mut env RecursiveStrEnv, ctx RecursiveStrContext) {
	for env.defer_scopes.len > 0 {
		tc.recursive_str_run_current_defer_scope(mut env, ctx)
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
	recursive_str_push_defer_scope(mut env)
	mut i := 0
	mut falls_through := true
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
		falls_through = false
		break
	}
	tc.recursive_str_run_current_defer_scope(mut env, ctx)
	return falls_through
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
			values := tc.recursive_str_assignment_values(*node, lhs_ids.len, mut env, ctx)
			for i, lhs_id in lhs_ids {
				if i >= values.len {
					break
				}
				lhs := tc.a.node(lhs_id)
				value := values[i]
				mut binding := value.binding
				if lhs.kind != .ident || lhs.value == '_' {
					continue
				}
				if binding.typ_name.len == 0 && tc.valid_node_id(value.rhs_id) {
					binding.typ_name = tc.resolve_type(value.rhs_id).name()
				}
				if binding.storage_id == 0 {
					binding.storage_id = env.next_storage_id
					env.next_storage_id++
				}
				fact_value := tc.recursive_str_known_fact_value(value.rhs_id, env) or { '' }
				tc.recursive_str_invalidate_value_facts(lhs_id, mut env)
				env.bindings[lhs.value] = binding
				if fact_value.len > 0 {
					tc.recursive_str_set_value_fact(lhs.value, fact_value, true, mut env)
				}
			}
			return true
		}
		.assign {
			lhs_ids := tc.multi_assign_lhs_ids(*node)
			values := tc.recursive_str_assignment_values(*node, lhs_ids.len, mut env, ctx)
			for i, lhs_id in lhs_ids {
				if i >= values.len {
					break
				}
				value := values[i]
				mut binding := value.binding
				lhs := tc.a.node(lhs_id)
				if lhs.kind == .ident {
					if binding.typ_name.len == 0 {
						binding.typ_name = tc.resolve_type(value.rhs_id).name()
					}
					if binding.storage_id == 0 {
						binding.storage_id = env.next_storage_id
						env.next_storage_id++
					}
					fact_value := tc.recursive_str_known_fact_value(value.rhs_id, env) or { '' }
					tc.recursive_str_invalidate_value_facts(lhs_id, mut env)
					env.bindings[lhs.value] = binding
					if fact_value.len > 0 {
						tc.recursive_str_set_value_fact(lhs.value, fact_value, true, mut env)
					}
				} else {
					if node.op == .assign
						&& tc.recursive_str_replace_aggregate_slot(lhs_id, binding, mut env) {
						continue
					}
					tc.recursive_str_apply_mutation(lhs_id, value.rhs_id, node.op, mut env)
				}
			}
			return true
		}
		.selector_assign, .index_assign {
			if node.children_count >= 2 {
				lhs_id := tc.a.child(node, 0)
				rhs_id := tc.a.child(node, node.children_count - 1)
				rhs_binding := tc.recursive_str_eval_expr(rhs_id, mut env, ctx)
				if node.op == .assign
					&& tc.recursive_str_replace_aggregate_slot(lhs_id, rhs_binding, mut env) {
					return true
				}
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
			if node.children_count > 0 {
				condition_id := tc.a.child(node, 0)
				tc.recursive_str_eval_expr(condition_id, mut assert_env, ctx)
				condition_is_true := tc.constant_bool_value(condition_id) or { false }
				if node.children_count > 1 && !condition_is_true {
					tc.recursive_str_eval_expr(tc.a.child(node, 1), mut assert_env, ctx)
				}
			}
			return true
		}
		.defer_stmt {
			tc.recursive_str_register_defer(id, *node, mut env)
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
		.comptime_if {
			return tc.recursive_str_process_comptime_if_stmt(*node, mut env, ctx)
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

fn (mut tc TypeChecker) recursive_str_process_comptime_if_stmt(node flat.Node, mut env RecursiveStrEnv, ctx RecursiveStrContext) bool {
	if taken := tc.comptime_type_condition_value(node.value) {
		branch_index := if taken { 0 } else { 1 }
		if branch_index >= node.children_count {
			return true
		}
		return tc.recursive_str_process_stmt(tc.a.child(&node, branch_index), mut env, ctx)
	}
	base := env.clone_env()
	mut branch_envs := []RecursiveStrEnv{}
	for i in 0 .. node.children_count {
		mut branch_env := base.clone_env()
		if tc.recursive_str_process_stmt(tc.a.child(&node, i), mut branch_env, ctx) {
			branch_envs << branch_env
		}
	}
	if node.children_count < 2 {
		branch_envs << base
	}
	if branch_envs.len == 0 {
		return false
	}
	env = tc.recursive_str_merge_envs(branch_envs)
	return true
}

fn (mut tc TypeChecker) recursive_str_assignment_values(node flat.Node, lhs_count int, mut env RecursiveStrEnv, ctx RecursiveStrContext) []RecursiveStrAssignmentValue {
	rhs_count := tc.multi_assign_rhs_count(node)
	if rhs_count == 0 {
		return []
	}
	if rhs_count == 1 {
		rhs_id := tc.multi_assign_rhs_id(node, 0)
		result := tc.recursive_str_eval_expr(rhs_id, mut env, ctx)
		if lhs_count > 1 {
			if multi := multi_return_payload_type(tc.resolve_type(rhs_id)) {
				mut values := []RecursiveStrAssignmentValue{cap: lhs_count}
				for i in 0 .. lhs_count {
					mut binding := if i < result.elements.len {
						result.elements[i]
					} else {
						RecursiveStrBinding{}
					}
					if binding.typ_name.len == 0 && i < multi.types.len {
						binding.typ_name = multi.types[i].name()
					}
					values << RecursiveStrAssignmentValue{
						binding: binding
						rhs_id:  rhs_id
					}
				}
				return values
			}
		}
		return [
			RecursiveStrAssignmentValue{
				binding: result
				rhs_id:  rhs_id
			},
		]
	}
	mut values := []RecursiveStrAssignmentValue{cap: int_min(lhs_count, rhs_count)}
	for i in 0 .. int_min(lhs_count, rhs_count) {
		rhs_id := tc.multi_assign_rhs_id(node, i)
		values << RecursiveStrAssignmentValue{
			binding: tc.recursive_str_eval_expr(rhs_id, mut env, ctx)
			rhs_id:  rhs_id
		}
	}
	return values
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
		if tc.recursive_str_process_child_sequence(*branch, mut branch_env, ctx) {
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
	mut container_binding := RecursiveStrBinding{}
	if node.kind == .for_stmt {
		if node.children_count < 3 {
			return true
		}
		tc.recursive_str_process_stmt(tc.a.child(node, 0), mut env, ctx)
		tc.recursive_str_eval_condition(tc.a.child(node, 1), mut env, ctx)
		body_start = 3
	} else {
		body_start = if node.value.is_int() { node.value.int() } else { 3 }
		if node.children_count > 2 {
			container_binding = tc.recursive_str_eval_expr(tc.a.child(node, 2), mut env, ctx)
		}
		for i in 3 .. int_min(body_start, int(node.children_count)) {
			tc.recursive_str_eval_expr(tc.a.child(node, i), mut env, ctx)
		}
	}
	base := env.clone_env()
	mut loop_base := base.clone_env()
	if node.kind == .for_in_stmt {
		tc.recursive_str_bind_for_in_vars(*node, container_binding, mut loop_base)
	}
	mut flow := tc.recursive_str_process_loop_sequence(*node, body_start, [
		loop_base,
	], ctx)
	for return_path in flow.returns {
		mut return_env := return_path.clone_env()
		tc.recursive_str_run_all_defer_scopes(mut return_env, ctx)
	}
	if node.kind == .for_stmt {
		mut after_post := []RecursiveStrEnv{}
		mut post_paths := flow.fallthrough.clone()
		post_paths << flow.continues
		for loop_path in post_paths {
			mut post_env := loop_path.clone_env()
			if tc.recursive_str_process_stmt(tc.a.child(node, 2), mut post_env, ctx) {
				after_post << post_env
			}
		}
		flow = RecursiveStrLoopFlow{
			fallthrough: after_post
			breaks:      flow.breaks
			returns:     flow.returns
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

fn (tc &TypeChecker) recursive_str_bind_for_in_vars(node flat.Node, container RecursiveStrBinding, mut env RecursiveStrEnv) {
	if node.children_count < 3 {
		return
	}
	key_id := tc.a.child(&node, 0)
	value_id := tc.a.child(&node, 1)
	key := tc.a.node(key_id)
	value := tc.a.node(value_id)
	mut target_id := key_id
	mut target_name := key.value
	if value.kind == .ident && value.value != '_' {
		target_id = value_id
		target_name = value.value
	} else if key.kind != .ident || key.value == '_' {
		return
	} else if unalias_and_unwrap_pointer_type(tc.resolve_type(tc.a.child(&node, 2))) is Map {
		return
	}
	mut binding := if container.elements.len == 0 {
		RecursiveStrBinding{}
	} else if container.repeated_element {
		container.elements[0]
	} else {
		tc.recursive_str_merge_bindings(container.elements)
	}
	if binding.typ_name.len == 0 {
		binding.typ_name = tc.resolve_type(target_id).name()
	}
	env.bindings[target_name] = binding
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
	mut active := []RecursiveStrEnv{cap: initial.len}
	for initial_env in initial {
		mut scoped_env := initial_env.clone_env()
		recursive_str_push_defer_scope(mut scoped_env)
		active << scoped_env
	}
	mut breaks := []RecursiveStrEnv{}
	mut continues := []RecursiveStrEnv{}
	mut returns := []RecursiveStrEnv{}
	for i in start .. int(node.children_count) {
		mut next := []RecursiveStrEnv{}
		for loop_path in active {
			flow := tc.recursive_str_process_loop_control_stmt(tc.a.child(&node, i), loop_path, ctx)
			next << flow.fallthrough
			breaks << flow.breaks
			continues << flow.continues
			returns << flow.returns
		}
		active = next.clone()
		if active.len == 0 {
			break
		}
	}
	return RecursiveStrLoopFlow{
		fallthrough: tc.recursive_str_close_defer_scope(active, ctx)
		breaks:      tc.recursive_str_close_defer_scope(breaks, ctx)
		continues:   tc.recursive_str_close_defer_scope(continues, ctx)
		returns:     tc.recursive_str_close_defer_scope(returns, ctx)
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
			return RecursiveStrLoopFlow{
				continues: [source]
			}
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
			if node.kind == .return_stmt || (node.kind == .expr_stmt && tc.expr_never_returns(id)) {
				return RecursiveStrLoopFlow{
					returns: [env]
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
	mut continues := []RecursiveStrEnv{}
	mut returns := []RecursiveStrEnv{}
	mut has_else := false
	mut has_guaranteed_branch := false
	mut i := 0
	for i < node.children_count {
		child_id := tc.a.child(node, i)
		child := tc.a.node(child_id)
		if child.kind == .block {
			has_else = true
			flow := tc.recursive_str_process_loop_sequence(*child, 0, [
				base.clone_env(),
			], ctx)
			fallthrough << flow.fallthrough
			breaks << flow.breaks
			continues << flow.continues
			returns << flow.returns
			i++
			continue
		}
		mut condition_env := base.clone_env()
		tc.recursive_str_eval_condition(child_id, mut condition_env, ctx)
		mut condition_is_true := false
		mut condition_is_false := false
		if value := tc.constant_bool_value(child_id) {
			condition_is_true = value
			condition_is_false = !value
		}
		if i + 1 < node.children_count {
			block := tc.a.child_node(&node, i + 1)
			if block.kind == .block {
				if !condition_is_false {
					mut branch_env := condition_env.clone_env()
					tc.recursive_str_apply_condition_facts(child_id, true, mut branch_env)
					flow := tc.recursive_str_process_loop_sequence(*block, 0, [
						branch_env,
					], ctx)
					fallthrough << flow.fallthrough
					breaks << flow.breaks
					continues << flow.continues
					returns << flow.returns
				}
				base = condition_env
				tc.recursive_str_apply_condition_facts(child_id, false, mut base)
				i += 2
				if condition_is_true {
					has_guaranteed_branch = true
					break
				}
				continue
			}
		}
		i++
	}
	if !has_else && !has_guaranteed_branch {
		fallthrough << base
	}
	return RecursiveStrLoopFlow{
		fallthrough: fallthrough
		breaks:      breaks
		continues:   continues
		returns:     returns
	}
}

fn (mut tc TypeChecker) recursive_str_process_loop_match(node flat.Node, source RecursiveStrEnv, ctx RecursiveStrContext) RecursiveStrLoopFlow {
	mut base := source.clone_env()
	if node.children_count > 0 {
		tc.recursive_str_eval_expr(tc.a.child(&node, 0), mut base, ctx)
	}
	mut fallthrough := []RecursiveStrEnv{}
	mut breaks := []RecursiveStrEnv{}
	mut continues := []RecursiveStrEnv{}
	mut returns := []RecursiveStrEnv{}
	for i in 1 .. node.children_count {
		branch := tc.a.child_node(&node, i)
		if branch.kind != .match_branch {
			continue
		}
		start := if branch.value.is_int() { branch.value.int() } else { 0 }
		mut branch_env := base.clone_env()
		tc.recursive_str_apply_match_branch_progress(tc.a.child(&node, 0), *branch, mut branch_env,
			ctx)
		flow := tc.recursive_str_process_loop_sequence(*branch, start, [
			branch_env,
		], ctx)
		fallthrough << flow.fallthrough
		breaks << flow.breaks
		continues << flow.continues
		returns << flow.returns
	}
	if !tc.match_has_else_or_exhaustive_coverage(node) {
		fallthrough << base
	}
	return RecursiveStrLoopFlow{
		fallthrough: fallthrough
		breaks:      breaks
		continues:   continues
		returns:     returns
	}
}

fn (mut tc TypeChecker) recursive_str_process_loop_select(node flat.Node, source RecursiveStrEnv, ctx RecursiveStrContext) RecursiveStrLoopFlow {
	mut fallthrough := []RecursiveStrEnv{}
	mut breaks := []RecursiveStrEnv{}
	mut continues := []RecursiveStrEnv{}
	mut returns := []RecursiveStrEnv{}
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
		continues << flow.continues
		returns << flow.returns
	}
	return RecursiveStrLoopFlow{
		fallthrough: fallthrough
		breaks:      breaks
		continues:   continues
		returns:     returns
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
		.paren, .cast_expr, .as_expr {
			if node.children_count > 0 {
				mut binding := tc.recursive_str_eval_expr(tc.a.child(node, 0), mut env, ctx)
				if node.kind == .cast_expr && binding.can_recurse {
					target := unwrap_all_pointers(tc.resolve_type(id))
					receiver := unwrap_all_pointers(ctx.receiver_type)
					if target !is Unknown && target !is Interface && receiver !is Unknown
						&& target.name() != receiver.name() {
						binding.progressed = true
						binding.nonreversible_progress = true
						binding.numeric_deltas = map[string]i64{}
					}
				}
				return binding
			}
		}
		.dump_expr {
			if node.children_count > 0 {
				child_id := tc.a.child(node, 0)
				binding := tc.recursive_str_eval_expr(child_id, mut env, ctx)
				if !tc.suppress_dump_output
					&& recursive_str_binding_has_unprogressed_receiver(binding) {
					pos := tc.a.node(child_id).pos
					message := 'cannot call `str()` method recursively'
					if !tc.errors.any(it.msg == message && it.pos == pos) {
						tc.record_error_at(.unknown_fn, message, child_id, pos)
					}
				}
				return binding
			}
		}
		.prefix {
			if node.children_count > 0 {
				child_id := tc.a.child(node, 0)
				if node.op == .arrow {
					return tc.recursive_str_eval_channel_receive(id, child_id, mut env, ctx)
				}
				return tc.recursive_str_eval_expr(child_id, mut env, ctx)
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
				if field_binding := tc.recursive_str_named_element_binding(binding,
					recursive_str_struct_field_key(node.value), id)
				{
					return field_binding
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
				return tc.recursive_str_index_binding(base, index_id, id, env)
			}
		}
		.array_literal, .array_init {
			return tc.recursive_str_eval_array_expr(id, *node, mut env, ctx)
		}
		.map_init {
			return tc.recursive_str_eval_map_expr(id, *node, mut env, ctx)
		}
		.struct_init {
			return tc.recursive_str_eval_struct_init(id, *node, mut env, ctx)
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
		.comptime_if {
			return tc.recursive_str_eval_comptime_if_expr(id, *node, mut env, ctx)
		}
		.string_interp {
			for i in 0 .. node.children_count {
				part_id := tc.a.child(node, i)
				part := tc.a.node(part_id)
				expr_id := if part.kind == .directive && part.value == 'string_interp_format'
					&& part.children_count > 0 {
					tc.a.child(part, 0)
				} else {
					part_id
				}
				binding := tc.recursive_str_eval_expr(expr_id, mut env, ctx)
				if !recursive_str_binding_has_unprogressed_receiver(binding) {
					continue
				}
				pos := tc.string_interpolation_expr_pos(expr_id)
				message := 'cannot call `str()` method recursively'
				if !tc.errors.any(it.msg == message && it.pos == pos) {
					tc.record_error_at(.unknown_fn, message, expr_id, pos)
				}
			}
			return RecursiveStrBinding{
				typ_name: tc.resolve_type(id).name()
			}
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
		.fn_literal, .lambda_expr {
			// Keep the closure body for a later invocation without treating its creation as
			// synchronous execution.
			mut capture_names := []string{}
			for name, binding in env.bindings {
				if recursive_str_binding_has_provenance(binding) {
					capture_names << name
				}
			}
			capture_names.sort()
			mut captures := []RecursiveStrBinding{cap: capture_names.len}
			for name in capture_names {
				captures << env.bindings[name]
			}
			return RecursiveStrBinding{
				typ_name:              tc.resolve_type(id).name()
				closure_ids:           [id]
				closure_capture_names: capture_names
				closure_captures:      captures
			}
		}
		.spawn_expr {
			// Starting asynchronous work does not execute its body synchronously before the
			// following recursive call, but calls inside it still need recursion diagnostics.
			mut spawned_env := env.clone_env()
			for i in 0 .. node.children_count {
				tc.recursive_str_eval_expr(tc.a.child(node, i), mut spawned_env, ctx)
			}
		}
		.postfix {
			if node.children_count > 0 {
				target_id := tc.a.child(node, 0)
				tc.recursive_str_apply_mutation(target_id, flat.empty_node, node.op, mut env)
			}
		}
		.infix {
			if node.op in [.logical_or, .logical_and] && node.children_count >= 2 {
				lhs_id := tc.a.child(node, 0)
				tc.recursive_str_eval_expr(lhs_id, mut env, ctx)
				if lhs := tc.constant_bool_value(lhs_id) {
					if (node.op == .logical_and && !lhs) || (node.op == .logical_or && lhs) {
						return RecursiveStrBinding{
							typ_name: tc.resolve_type(id).name()
						}
					}
					tc.recursive_str_eval_expr(tc.a.child(node, 1), mut env, ctx)
					return RecursiveStrBinding{
						typ_name: tc.resolve_type(id).name()
					}
				}
				base := env.clone_env()
				mut rhs_env := base.clone_env()
				tc.recursive_str_eval_expr(tc.a.child(node, 1), mut rhs_env, ctx)
				env = tc.recursive_str_merge_envs([base, rhs_env])
				return RecursiveStrBinding{
					typ_name: tc.resolve_type(id).name()
				}
			}
			mut operand_bindings := []RecursiveStrBinding{cap: int(node.children_count)}
			for i in 0 .. node.children_count {
				operand_bindings << tc.recursive_str_eval_expr(tc.a.child(node, i), mut env, ctx)
			}
			if node.op == .arrow && node.children_count >= 2 {
				tc.recursive_str_apply_channel_send(tc.a.child(node, 0), operand_bindings[1], mut
					env)
				return RecursiveStrBinding{
					typ_name: tc.resolve_type(id).name()
				}
			}
			if node.op == .left_shift && node.children_count >= 2 {
				lhs_id := tc.a.child(node, 0)
				if _ := array_type_from_receiver(tc.resolve_type(lhs_id)) {
					if tc.recursive_str_apply_array_append(lhs_id, tc.a.child(node, 1),
						operand_bindings[1], mut env)
					{
						return RecursiveStrBinding{
							typ_name: tc.resolve_type(id).name()
						}
					}
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

fn (mut tc TypeChecker) recursive_str_eval_struct_init(id flat.NodeId, node flat.Node, mut env RecursiveStrEnv, ctx RecursiveStrContext) RecursiveStrBinding {
	target_type := tc.resolve_type(id)
	mut aggregate := RecursiveStrBinding{
		typ_name: target_type.name()
	}
	target_struct := struct_type_from_type(target_type) or {
		for i in 0 .. node.children_count {
			field := tc.a.child_node(&node, i)
			for j in 0 .. field.children_count {
				tc.recursive_str_eval_expr(tc.a.child(field, j), mut env, ctx)
			}
		}
		return aggregate
	}
	fields := tc.struct_fields_for_init(target_struct.name)
	for i in 0 .. node.children_count {
		field := tc.a.child_node(&node, i)
		mut field_binding := RecursiveStrBinding{}
		for j in 0 .. field.children_count {
			field_binding = tc.recursive_str_eval_expr(tc.a.child(field, j), mut env, ctx)
		}
		if field.kind == .field_init && field.children_count == 1 {
			field_name := if field.value.len > 0 {
				field.value
			} else if i < fields.len {
				fields[i].name
			} else {
				''
			}
			if field_name.len > 0 {
				aggregate.element_keys << recursive_str_struct_field_key(field_name)
				aggregate.elements << field_binding
			}
		}
	}
	if node.children_count != fields.len {
		return aggregate
	}
	if fields.len == 0 {
		mut binding := env.bindings[ctx.receiver_name] or { return aggregate }
		if tc.type_compatible(ctx.receiver_type, target_type)
			&& tc.type_compatible(target_type, ctx.receiver_type) {
			binding.typ_name = target_type.name()
			return binding
		}
		return aggregate
	}
	mut receiver_text := ''
	mut receiver_binding := RecursiveStrBinding{}
	mut supplied := map[string]bool{}
	for i in 0 .. node.children_count {
		field := tc.a.child_node(&node, i)
		if field.kind != .field_init || field.children_count != 1 {
			return aggregate
		}
		field_name := if field.value.len > 0 { field.value } else { fields[i].name }
		if field_name in supplied {
			return aggregate
		}
		supplied[field_name] = true
		value_id := tc.a.child(field, 0)
		receiver_id := tc.recursive_str_struct_field_copy_receiver(value_id, field_name) or {
			return aggregate
		}
		current_text := tc.source_text_for_node(receiver_id).trim_space()
		if receiver_text.len == 0 {
			receiver_text = current_text
			receiver_binding = tc.recursive_str_binding_for_expr(receiver_id, env)
			receiver_type := tc.resolve_type(receiver_id)
			if !receiver_binding.can_recurse || !tc.type_compatible(receiver_type, target_type)
				|| !tc.type_compatible(target_type, receiver_type) {
				return aggregate
			}
		} else if current_text != receiver_text {
			return aggregate
		}
	}
	for field in fields {
		if field.name !in supplied {
			return aggregate
		}
	}
	receiver_binding.typ_name = target_type.name()
	return receiver_binding
}

fn (tc &TypeChecker) recursive_str_struct_field_copy_receiver(id flat.NodeId, field_name string) ?flat.NodeId {
	if !tc.valid_node_id(id) {
		return none
	}
	node := tc.a.node(id)
	if node.kind in [.paren, .cast_expr, .as_expr, .dump_expr] && node.children_count > 0 {
		return tc.recursive_str_struct_field_copy_receiver(tc.a.child(node, 0), field_name)
	}
	if node.kind == .selector && node.value == field_name && node.children_count > 0 {
		return tc.a.child(node, 0)
	}
	return none
}

fn (mut tc TypeChecker) recursive_str_eval_comptime_if_expr(id flat.NodeId, node flat.Node, mut env RecursiveStrEnv, ctx RecursiveStrContext) RecursiveStrBinding {
	if taken := tc.comptime_type_condition_value(node.value) {
		branch_index := if taken { 0 } else { 1 }
		if branch_index >= node.children_count {
			return RecursiveStrBinding{
				typ_name: tc.resolve_type(id).name()
			}
		}
		return tc.recursive_str_eval_expr(tc.a.child(&node, branch_index), mut env, ctx)
	}
	base := env.clone_env()
	mut branch_envs := []RecursiveStrEnv{}
	mut results := []RecursiveStrBinding{}
	for i in 0 .. node.children_count {
		mut branch_env := base.clone_env()
		results << tc.recursive_str_eval_expr(tc.a.child(&node, i), mut branch_env, ctx)
		branch_envs << branch_env
	}
	if node.children_count < 2 {
		branch_envs << base
	}
	env = tc.recursive_str_merge_envs(branch_envs)
	return tc.recursive_str_merge_bindings(results)
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
		if binding.can_recurse
			&& tc.recursive_str_struct_update_field_changes_base(base_id, *field, env) {
			binding.progressed = true
			binding.nonreversible_progress = true
			binding.numeric_deltas = map[string]i64{}
		}
	}
	binding.typ_name = tc.resolve_type(id).name()
	return binding
}

fn (tc &TypeChecker) recursive_str_struct_update_field_changes_base(base_id flat.NodeId, field flat.Node, env RecursiveStrEnv) bool {
	if field.kind != .field_init || field.children_count == 0 || field.value.len == 0 {
		return false
	}
	value_id := tc.a.child(&field, 0)
	if tc.recursive_str_expr_matches_struct_field(value_id, base_id, field.value) {
		return false
	}
	if value := tc.recursive_str_literal_fact_value(value_id) {
		if base_key := tc.recursive_str_value_expr_key(base_id) {
			field_key := '${base_key}.${field.value}'
			if known := env.known_values[field_key] {
				return known != value
			}
			if excluded := env.excluded_values[field_key] {
				return excluded[value]
			}
		}
	}
	value := tc.a.node(value_id)
	if value.kind != .infix || value.children_count < 2 {
		return false
	}
	lhs_id := tc.a.child(value, 0)
	rhs_id := tc.a.child(value, 1)
	if value.op in [.plus, .minus]
		&& tc.recursive_str_expr_matches_struct_field(lhs_id, base_id, field.value) {
		rhs := tc.a.node(rhs_id)
		return rhs.kind in [.int_literal, .float_literal] && !numeric_literal_is_zero(rhs.value)
	}
	if value.op == .plus && tc.recursive_str_expr_matches_struct_field(rhs_id, base_id, field.value) {
		lhs := tc.a.node(lhs_id)
		return lhs.kind in [.int_literal, .float_literal] && !numeric_literal_is_zero(lhs.value)
	}
	return false
}

fn (tc &TypeChecker) recursive_str_expr_matches_struct_field(id flat.NodeId, base_id flat.NodeId, field_name string) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.node(id)
	if node.kind == .paren && node.children_count > 0 {
		return tc.recursive_str_expr_matches_struct_field(tc.a.child(node, 0), base_id, field_name)
	}
	if node.kind != .selector || node.value != field_name || node.children_count == 0 {
		return false
	}
	receiver_id := tc.a.child(node, 0)
	return tc.source_text_for_node(receiver_id).trim_space() == tc.source_text_for_node(base_id).trim_space()
}

fn (mut tc TypeChecker) recursive_str_eval_map_expr(id flat.NodeId, node flat.Node, mut env RecursiveStrEnv, ctx RecursiveStrContext) RecursiveStrBinding {
	mut elements := []RecursiveStrBinding{}
	mut element_keys := []string{}
	mut i := 0
	for i < int(node.children_count) {
		key_id := tc.a.child(&node, i)
		tc.recursive_str_eval_expr(key_id, mut env, ctx)
		if i + 1 >= node.children_count {
			break
		}
		value_id := tc.a.child(&node, i + 1)
		element_keys << tc.recursive_str_constant_map_key(key_id) or { '' }
		elements << tc.recursive_str_eval_expr(value_id, mut env, ctx)
		i += 2
	}
	return RecursiveStrBinding{
		typ_name:     tc.resolve_type(id).name()
		elements:     elements
		element_keys: element_keys
	}
}

fn (mut tc TypeChecker) recursive_str_eval_array_expr(id flat.NodeId, node flat.Node, mut env RecursiveStrEnv, ctx RecursiveStrContext) RecursiveStrBinding {
	mut elements := []RecursiveStrBinding{}
	mut repeated_element := false
	is_empty := tc.recursive_str_array_init_is_empty(node, env)
	for i in 0 .. node.children_count {
		child_id := tc.a.child(&node, i)
		child := tc.a.node(child_id)
		if node.kind == .array_init && child.kind == .field_init {
			if child.value == 'init' && child.children_count > 0 {
				if is_empty {
					continue
				}
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

fn (tc &TypeChecker) recursive_str_array_init_is_empty(node flat.Node, env RecursiveStrEnv) bool {
	if node.kind != .array_init {
		return false
	}
	for i in 0 .. node.children_count {
		field := tc.a.child_node(&node, i)
		if field.kind != .field_init || field.value != 'len' || field.children_count == 0 {
			continue
		}
		if length := tc.recursive_str_resolved_constant_index(tc.a.child(field, 0), env) {
			return length == 0
		}
		return false
	}
	return false
}

fn (tc &TypeChecker) recursive_str_index_binding(base RecursiveStrBinding, index_id flat.NodeId, result_id flat.NodeId, env RecursiveStrEnv) RecursiveStrBinding {
	if base.elements.len > 0 {
		result := tc.a.node(result_id)
		if result.kind == .index && result.value == 'range' {
			return tc.recursive_str_slice_binding(base, *result, result_id, env)
		}
		if base.element_keys.len == base.elements.len && '' !in base.element_keys {
			if key := tc.recursive_str_constant_map_key(index_id) {
				for i := base.element_keys.len - 1; i >= 0; i-- {
					if base.element_keys[i] == key {
						return base.elements[i]
					}
				}
			}
			return tc.recursive_str_merge_bindings(base.elements)
		}
		if base.repeated_element {
			return base.elements[0]
		}
		if index := tc.recursive_str_resolved_constant_index(index_id, env) {
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

fn (tc &TypeChecker) recursive_str_slice_binding(base RecursiveStrBinding, node flat.Node, result_id flat.NodeId, env RecursiveStrEnv) RecursiveStrBinding {
	mut result := base
	result.typ_name = tc.resolve_type(result_id).name()
	result.element_keys = []string{}
	mut low := 0
	mut high := base.elements.len
	mut high_known := !base.repeated_element
	if node.children_count > 1 {
		low_id := tc.a.child(&node, 1)
		if tc.a.node(low_id).kind != .empty {
			low = tc.recursive_str_resolved_constant_index(low_id, env) or { return result }
		}
	}
	if node.children_count > 2 {
		high = tc.recursive_str_resolved_constant_index(tc.a.child(&node, 2), env) or {
			return result
		}
		high_known = true
	}
	if base.repeated_element {
		if high_known && low == high {
			result.elements = []RecursiveStrBinding{}
			result.repeated_element = false
		}
		return result
	}
	if low < 0 || high < low || high > base.elements.len {
		return result
	}
	result.elements = base.elements[low..high].clone()
	return result
}

fn (tc &TypeChecker) recursive_str_named_element_binding(base RecursiveStrBinding, key string, result_id flat.NodeId) ?RecursiveStrBinding {
	index := recursive_str_named_element_index(base, key) or { return none }
	mut binding := base.elements[index]
	if binding.typ_name.len == 0 {
		binding.typ_name = tc.resolve_type(result_id).name()
	}
	return binding
}

fn recursive_str_named_element_index(base RecursiveStrBinding, key string) ?int {
	if base.element_keys.len != base.elements.len {
		return none
	}
	for i := base.element_keys.len; i > 0; i-- {
		if base.element_keys[i - 1] == key {
			return i - 1
		}
	}
	return none
}

fn recursive_str_struct_field_key(name string) string {
	return 'field:${name}'
}

fn (tc &TypeChecker) recursive_str_constant_map_key(id flat.NodeId) ?string {
	if !tc.valid_node_id(id) {
		return none
	}
	node := tc.a.node(id)
	if node.kind in [.paren, .cast_expr] && node.children_count > 0 {
		return tc.recursive_str_constant_map_key(tc.a.child(node, 0))
	}
	return match node.kind {
		.string_literal { 'string:${node.value}' }
		.char_literal { 'char:${node.value}' }
		.int_literal { 'int:${node.value.to_lower().replace('_', '')}' }
		.bool_literal { 'bool:${node.value}' }
		.enum_val { 'enum:${node.value}' }
		else { none }
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

fn (tc &TypeChecker) recursive_str_resolved_constant_index(id flat.NodeId, env RecursiveStrEnv) ?int {
	if index := tc.recursive_str_constant_index(id) {
		return index
	}
	value := tc.recursive_str_known_fact_value(id, env) or { return none }
	if !value.starts_with('int:') || !value[4..].is_int() {
		return none
	}
	return value[4..].int()
}

fn (mut tc TypeChecker) recursive_str_eval_block_value(node flat.Node, mut env RecursiveStrEnv, ctx RecursiveStrContext) RecursiveStrBinding {
	recursive_str_push_defer_scope(mut env)
	mut result := RecursiveStrBinding{}
	if node.children_count == 0 {
		tc.recursive_str_run_current_defer_scope(mut env, ctx)
		return result
	}
	for i in 0 .. node.children_count {
		child_id := tc.a.child(&node, i)
		child := tc.a.node(child_id)
		if i == node.children_count - 1 && child.kind == .expr_stmt && child.children_count > 0 {
			result = tc.recursive_str_eval_expr(tc.a.child(child, child.children_count - 1), mut
				env, ctx)
			break
		}
		if !tc.recursive_str_process_stmt(child_id, mut env, ctx) {
			break
		}
	}
	tc.recursive_str_run_current_defer_scope(mut env, ctx)
	return result
}

fn (mut tc TypeChecker) recursive_str_eval_call(id flat.NodeId, mut env RecursiveStrEnv, ctx RecursiveStrContext) RecursiveStrBinding {
	node := tc.a.node(id)
	if node.children_count == 0 {
		return RecursiveStrBinding{}
	}
	callee := tc.a.child_node(node, 0)
	mut receiver_binding := RecursiveStrBinding{}
	mut callee_binding := RecursiveStrBinding{}
	mut receiver_id := flat.empty_node
	if callee.kind == .selector && callee.children_count > 0 {
		receiver_id = tc.a.child(callee, 0)
		receiver_binding = tc.recursive_str_eval_expr(receiver_id, mut env, ctx)
		if field_binding := tc.recursive_str_named_element_binding(receiver_binding,
			recursive_str_struct_field_key(callee.value), tc.a.child(node, 0))
		{
			callee_binding = field_binding
		}
	} else if callee.kind == .ident {
		callee_binding = env.bindings[callee.value] or { RecursiveStrBinding{} }
	} else {
		callee_binding = tc.recursive_str_eval_expr(tc.a.child(node, 0), mut env, ctx)
	}
	direct_recursive_receiver := receiver_binding.can_recurse && !receiver_binding.progressed
		&& tc.recursive_str_call_targets_current(id, receiver_id, receiver_binding, env.active_helper_ids.len > 0, ctx)
	nested_recursive_receiver := tc.recursive_str_call_formats_nested_aggregate(id, receiver_id,
		receiver_binding)
	if callee.kind == .selector && callee.value == 'str'
		&& (direct_recursive_receiver || nested_recursive_receiver) {
		pos := tc.method_call_name_pos(*node, *callee)
		message := 'cannot call `str()` method recursively'
		if !tc.errors.any(it.msg == message && it.pos == pos) {
			tc.record_error_at(.unknown_fn, message, id, pos)
		}
	} else if callee_binding.is_recursive_str_method && callee_binding.can_recurse
		&& !callee_binding.progressed {
		message := 'cannot call `str()` method recursively'
		if !tc.errors.any(it.msg == message && it.pos == callee.pos) {
			tc.record_error_at(.unknown_fn, message, id, callee.pos)
		}
	}
	mut arg_bindings := []RecursiveStrBinding{}
	for i in 1 .. node.children_count {
		arg_id := tc.call_arg_value(tc.a.child(node, i))
		arg_bindings << tc.recursive_str_eval_expr(arg_id, mut env, ctx)
	}
	if tc.recursive_str_is_builtin_print_call(id, *callee) {
		for i, binding in arg_bindings {
			if !recursive_str_binding_has_unprogressed_receiver(binding) {
				continue
			}
			arg_id := tc.call_arg_value(tc.a.child(node, i + 1))
			pos := tc.a.node(arg_id).pos
			message := 'cannot call `str()` method recursively'
			if !tc.errors.any(it.msg == message && it.pos == pos) {
				tc.record_error_at(.unknown_fn, message, id, pos)
			}
		}
	}
	if result := tc.recursive_str_apply_builtin_array_mutator(id, *node, *callee, receiver_id,
		receiver_binding, arg_bindings, mut env)
	{
		return result
	}
	tc.recursive_str_eval_invoked_helper(id, receiver_binding, arg_bindings, env, ctx)
	tc.recursive_str_eval_builtin_array_callback(id, *callee, receiver_id, receiver_binding,
		arg_bindings, mut env, ctx)
	tc.recursive_str_apply_call_mutations(id, mut env)
	for closure_id in callee_binding.closure_ids {
		tc.recursive_str_eval_invoked_closure(closure_id, callee_binding, arg_bindings, mut env,
			ctx)
	}
	if tc.recursive_str_call_preserves_aggregate_elements(id, *callee, receiver_binding) {
		mut returned := receiver_binding
		returned.typ_name = tc.resolve_type(id).name()
		return returned
	}
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

fn (mut tc TypeChecker) recursive_str_eval_builtin_array_callback(call_id flat.NodeId, callee flat.Node, receiver_id flat.NodeId, receiver RecursiveStrBinding, args []RecursiveStrBinding, mut env RecursiveStrEnv, ctx RecursiveStrContext) {
	if callee.kind != .selector || callee.value != 'map' || args.len == 0
		|| receiver.elements.len == 0 || !tc.valid_node_id(receiver_id) {
		return
	}
	_ := array_type_from_receiver(tc.resolve_type(receiver_id)) or { return }
	if resolved := tc.resolved_call_name(call_id) {
		if resolved.len > 0 && resolved != 'array.map' {
			return
		}
	}
	callback := args[0]
	if callback.closure_ids.len == 0 {
		return
	}
	for element in receiver.elements {
		for closure_id in callback.closure_ids {
			tc.recursive_str_eval_invoked_closure(closure_id, callback, [element], mut env, ctx)
		}
	}
}

fn (tc &TypeChecker) recursive_str_is_builtin_print_call(id flat.NodeId, callee flat.Node) bool {
	if callee.kind != .ident || callee.value !in ['print', 'println', 'eprint', 'eprintln'] {
		return false
	}
	if resolved := tc.resolved_call_name(id) {
		return resolved.len == 0 || is_print_style_fn_name(resolved)
	}
	return true
}

fn (tc &TypeChecker) recursive_str_call_formats_nested_aggregate(call_id flat.NodeId, receiver_id flat.NodeId, receiver RecursiveStrBinding) bool {
	if !recursive_str_binding_has_unprogressed_receiver(receiver) {
		return false
	}
	if resolved := tc.resolved_call_name(call_id) {
		if tc.recursive_str_has_concrete_fn_decl(resolved) {
			return false
		}
	}
	receiver_type := unalias_and_unwrap_pointer_type(tc.resolve_type(receiver_id))
	return receiver_type is Array || receiver_type is ArrayFixed || receiver_type is Map
}

fn (tc &TypeChecker) recursive_str_call_preserves_aggregate_elements(id flat.NodeId, callee flat.Node, receiver RecursiveStrBinding) bool {
	if receiver.elements.len == 0 || callee.kind != .selector || callee.value != 'clone' {
		return false
	}
	if resolved := tc.resolved_call_name(id) {
		return resolved in ['array.clone', 'map.clone']
	}
	return callee.children_count > 0
		&& unalias_type(unwrap_pointer(tc.resolve_type(tc.a.child(&callee, 0)))) is Map
}

fn (mut tc TypeChecker) recursive_str_apply_builtin_array_mutator(call_id flat.NodeId, call flat.Node, callee flat.Node, receiver_id flat.NodeId, receiver RecursiveStrBinding, args []RecursiveStrBinding, mut env RecursiveStrEnv) ?RecursiveStrBinding {
	if callee.kind != .selector || !tc.valid_node_id(receiver_id) {
		return none
	}
	_ := array_type_from_receiver(tc.resolve_type(receiver_id)) or { return none }
	if resolved := tc.resolved_call_name(call_id) {
		if resolved.len > 0 && !checker_is_raw_collection_method_name(resolved, 'array.') {
			return none
		}
	}
	method := callee.value
	if method !in ['clear', 'delete', 'delete_many', 'delete_last', 'drop', 'insert', 'pop',
		'pop_left', 'prepend', 'reverse_in_place', 'sort', 'sort_with_compare', 'trim'] {
		return none
	}
	mut binding := receiver
	binding.elements = receiver.elements.clone()
	binding.element_keys = []string{}
	mut result := RecursiveStrBinding{
		typ_name: tc.resolve_type(call_id).name()
	}
	match method {
		'clear' {
			binding.elements = []RecursiveStrBinding{}
			binding.repeated_element = false
		}
		'delete' {
			index_id := tc.recursive_str_call_arg_id(call, 0) or { return result }
			tc.recursive_str_delete_array_elements(mut binding, index_id, flat.empty_node)
		}
		'delete_many' {
			index_id := tc.recursive_str_call_arg_id(call, 0) or { return result }
			count_id := tc.recursive_str_call_arg_id(call, 1) or { return result }
			tc.recursive_str_delete_array_elements(mut binding, index_id, count_id)
		}
		'delete_last' {
			if !binding.repeated_element && binding.elements.len > 0 {
				binding.elements.delete_last()
			}
		}
		'drop' {
			count_id := tc.recursive_str_call_arg_id(call, 0) or { return result }
			tc.recursive_str_drop_array_elements(mut binding, count_id)
		}
		'insert', 'prepend' {
			index_id := if method == 'insert' {
				tc.recursive_str_call_arg_id(call, 0) or { return result }
			} else {
				flat.empty_node
			}
			value_arg := if method == 'insert' { 1 } else { 0 }
			value_id := tc.recursive_str_call_arg_id(call, value_arg) or { return result }
			value := if value_arg < args.len { args[value_arg] } else { RecursiveStrBinding{} }
			tc.recursive_str_insert_array_elements(mut binding, index_id, value_id, value)
		}
		'pop', 'pop_left' {
			if binding.elements.len > 0 {
				index := if method == 'pop_left' { 0 } else { binding.elements.len - 1 }
				result = if binding.repeated_element {
					binding.elements[0]
				} else {
					binding.elements[index]
				}
				if !binding.repeated_element {
					binding.elements.delete(index)
				}
				if result.typ_name.len == 0 {
					result.typ_name = tc.resolve_type(call_id).name()
				}
			}
		}
		'reverse_in_place' {
			if !binding.repeated_element {
				binding.elements.reverse_in_place()
			}
		}
		'sort', 'sort_with_compare' {
			tc.recursive_str_make_array_order_conservative(mut binding)
		}
		'trim' {
			index_id := tc.recursive_str_call_arg_id(call, 0) or { return result }
			tc.recursive_str_trim_array_elements(mut binding, index_id)
		}
		else {}
	}
	tc.recursive_str_invalidate_value_facts(receiver_id, mut env)
	if tc.a.node(receiver_id).kind == .ident {
		name := tc.a.node(receiver_id).value
		env.bindings[name] = binding
	} else {
		tc.recursive_str_replace_aggregate_slot(receiver_id, binding, mut env)
	}
	return result
}

fn (tc &TypeChecker) recursive_str_call_arg_id(call flat.Node, index int) ?flat.NodeId {
	child_index := index + 1
	if child_index < 1 || child_index >= call.children_count {
		return none
	}
	return tc.call_arg_value(tc.a.child(&call, child_index))
}

fn (tc &TypeChecker) recursive_str_delete_array_elements(mut binding RecursiveStrBinding, index_id flat.NodeId, count_id flat.NodeId) {
	if binding.elements.len == 0 || binding.repeated_element {
		return
	}
	index := tc.recursive_str_constant_index(index_id) or {
		tc.recursive_str_make_array_order_conservative(mut binding)
		return
	}
	count := if tc.valid_node_id(count_id) {
		tc.recursive_str_constant_index(count_id) or {
			tc.recursive_str_make_array_order_conservative(mut binding)
			return
		}
	} else {
		1
	}
	if index < 0 || count < 0 || index > binding.elements.len
		|| index + count > binding.elements.len {
		return
	}
	for _ in 0 .. count {
		binding.elements.delete(index)
	}
}

fn (tc &TypeChecker) recursive_str_drop_array_elements(mut binding RecursiveStrBinding, count_id flat.NodeId) {
	if binding.elements.len == 0 || binding.repeated_element {
		return
	}
	count := tc.recursive_str_constant_index(count_id) or {
		tc.recursive_str_make_array_order_conservative(mut binding)
		return
	}
	if count <= 0 {
		return
	}
	if count >= binding.elements.len {
		binding.elements = []RecursiveStrBinding{}
		return
	}
	binding.elements = binding.elements[count..].clone()
}

fn (tc &TypeChecker) recursive_str_insert_array_elements(mut binding RecursiveStrBinding, index_id flat.NodeId, value_id flat.NodeId, value RecursiveStrBinding) {
	value_type := unalias_type(tc.resolve_type(value_id))
	mut inserted := if value_type is Array || value_type is ArrayFixed {
		value.elements.clone()
	} else {
		[value]
	}
	if binding.repeated_element {
		inserted << binding.elements
		binding.elements = [tc.recursive_str_merge_bindings(inserted)]
		return
	}
	index := if tc.valid_node_id(index_id) {
		tc.recursive_str_constant_index(index_id) or {
			inserted << binding.elements
			binding.elements = inserted
			tc.recursive_str_make_array_order_conservative(mut binding)
			return
		}
	} else {
		0
	}
	if index < 0 || index > binding.elements.len {
		return
	}
	mut elements := []RecursiveStrBinding{cap: binding.elements.len + inserted.len}
	elements << binding.elements[..index]
	elements << inserted
	elements << binding.elements[index..]
	binding.elements = elements
}

fn (tc &TypeChecker) recursive_str_trim_array_elements(mut binding RecursiveStrBinding, index_id flat.NodeId) {
	if binding.elements.len == 0 || binding.repeated_element {
		return
	}
	index := tc.recursive_str_constant_index(index_id) or {
		tc.recursive_str_make_array_order_conservative(mut binding)
		return
	}
	if index >= 0 && index < binding.elements.len {
		binding.elements = binding.elements[..index].clone()
	}
}

fn (tc &TypeChecker) recursive_str_make_array_order_conservative(mut binding RecursiveStrBinding) {
	if binding.elements.len == 0 || binding.repeated_element {
		return
	}
	binding.elements = [tc.recursive_str_merge_bindings(binding.elements)]
	binding.repeated_element = true
}

fn (mut tc TypeChecker) recursive_str_eval_invoked_helper(call_id flat.NodeId, receiver RecursiveStrBinding, args []RecursiveStrBinding, env RecursiveStrEnv, ctx RecursiveStrContext) {
	resolved := tc.resolved_call_name(call_id) or { return }
	decl_id := tc.recursive_str_fn_decl_id(resolved) or { return }
	if decl_id == ctx.fn_id || env.active_helper_ids[int(decl_id)] {
		return
	}
	decl := tc.a.node(decl_id)
	call := tc.a.node(call_id)
	mut helper_env := RecursiveStrEnv{
		next_storage_id:    env.next_storage_id
		active_closure_ids: env.active_closure_ids.clone()
		active_helper_ids:  env.active_helper_ids.clone()
	}
	helper_env.active_helper_ids[int(decl_id)] = true
	mut has_provenance := false
	mut actual_index := 0
	callee := tc.a.child_node(call, 0)
	mut actual_bindings := args.clone()
	if tc.recursive_str_decl_has_receiver(*decl) && callee.kind == .selector
		&& callee.children_count > 0 {
		actual_bindings.prepend(receiver)
	}
	for i in 0 .. decl.children_count {
		param := tc.a.child_node(decl, i)
		if param.kind != .param || actual_index >= actual_bindings.len {
			continue
		}
		binding := actual_bindings[actual_index]
		helper_env.bindings[param.value] = binding
		has_provenance = has_provenance || recursive_str_binding_has_provenance(binding)
		actual_index++
	}
	if !has_provenance {
		return
	}
	tc.recursive_str_process_child_sequence(*decl, mut helper_env, ctx)
}

fn (mut tc TypeChecker) recursive_str_eval_invoked_closure(id flat.NodeId, closure RecursiveStrBinding, args []RecursiveStrBinding, mut env RecursiveStrEnv, ctx RecursiveStrContext) {
	if !tc.valid_node_id(id) || env.active_closure_ids[int(id)] {
		return
	}
	node := tc.a.node(id)
	if node.kind !in [.fn_literal, .lambda_expr] {
		return
	}
	mut closure_env := env.clone_env()
	closure_env.active_closure_ids[int(id)] = true
	for i, name in closure.closure_capture_names {
		if i < closure.closure_captures.len {
			closure_env.bindings[name] = closure.closure_captures[i]
		}
	}
	mut arg_index := 0
	if node.kind == .lambda_expr {
		for i in 0 .. int(node.children_count) - 1 {
			param := tc.a.child_node(node, i)
			if param.kind != .ident || param.value.len == 0 {
				continue
			}
			closure_env.bindings[param.value] = if arg_index < args.len {
				args[arg_index]
			} else {
				RecursiveStrBinding{
					typ_name: tc.resolve_type(tc.a.child(node, i)).name()
				}
			}
			arg_index++
		}
		if node.children_count > 0 {
			tc.recursive_str_eval_expr(tc.a.child(node, node.children_count - 1), mut closure_env,
				ctx)
		}
		return
	}
	for i in 0 .. node.children_count {
		param_id := tc.a.child(node, i)
		param := tc.a.node(param_id)
		if param.kind != .param || param.value.len == 0 {
			continue
		}
		closure_env.bindings[param.value] = if arg_index < args.len {
			args[arg_index]
		} else {
			RecursiveStrBinding{
				typ_name: tc.resolve_type(param_id).name()
			}
		}
		arg_index++
	}
	tc.recursive_str_process_child_sequence(*node, mut closure_env, ctx)
}

fn (tc &TypeChecker) recursive_str_method_value_targets_current(selector_id flat.NodeId, receiver_id flat.NodeId, ctx RecursiveStrContext) bool {
	resolved := tc.resolved_fn_value_name(selector_id) or { '' }
	if resolved != ctx.fn_name && tc.recursive_str_has_concrete_fn_decl(resolved) {
		return false
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
	if actual.name() == expected.name() {
		return true
	}
	return actual is Interface && !tc.recursive_str_has_concrete_fn_decl(resolved)
}

fn (tc &TypeChecker) recursive_str_call_targets_current(call_id flat.NodeId, receiver_id flat.NodeId, receiver RecursiveStrBinding, allow_unresolved bool, ctx RecursiveStrContext) bool {
	resolved := tc.resolved_call_name(call_id) or {
		if !allow_unresolved {
			return false
		}
		actual := unwrap_all_pointers(tc.parse_type(receiver.typ_name))
		expected := unwrap_all_pointers(ctx.receiver_type)
		return actual !is Unknown && expected !is Unknown && actual.name() == expected.name()
	}
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
				pattern_name := tc.match_type_pattern(pattern) or { continue }
				pattern_type := unalias_and_unwrap_pointer_type(tc.parse_type(pattern_name))
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
	_ := tc.recursive_str_fn_decl_id(name) or { return false }
	return true
}

fn (mut tc TypeChecker) recursive_str_process_if_stmt(id flat.NodeId, mut env RecursiveStrEnv, ctx RecursiveStrContext) bool {
	node := tc.a.node(id)
	if node.children_count == 0 {
		return true
	}
	mut base := env.clone_env()
	mut branch_envs := []RecursiveStrEnv{}
	mut has_else := false
	mut has_guaranteed_branch := false
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
		mut condition_is_true := false
		mut condition_is_false := false
		if value := tc.constant_bool_value(child_id) {
			condition_is_true = value
			condition_is_false = !value
		}
		if i + 1 < node.children_count {
			block_id := tc.a.child(node, i + 1)
			if tc.a.node(block_id).kind == .block {
				if !condition_is_false {
					mut branch_env := condition_env.clone_env()
					tc.recursive_str_apply_condition_facts(child_id, true, mut branch_env)
					if tc.recursive_str_process_stmt(block_id, mut branch_env, ctx) {
						branch_envs << branch_env
					}
				}
				base = condition_env
				tc.recursive_str_apply_condition_facts(child_id, false, mut base)
				i += 2
				if condition_is_true {
					has_guaranteed_branch = true
					break
				}
				continue
			}
		}
		i++
	}
	if !has_else && !has_guaranteed_branch {
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
		tc.recursive_str_apply_match_branch_progress(tc.a.child(node, 0), *branch, mut branch_env,
			ctx)
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
	recursive_str_push_defer_scope(mut env)
	condition_count := if branch.value.is_int() { branch.value.int() } else { 0 }
	mut falls_through := true
	for i in condition_count .. branch.children_count {
		if !tc.recursive_str_process_stmt(tc.a.child(&branch, i), mut env, ctx) {
			falls_through = false
			break
		}
	}
	tc.recursive_str_run_current_defer_scope(mut env, ctx)
	return falls_through
}

fn (tc &TypeChecker) recursive_str_apply_match_branch_progress(subject_id flat.NodeId, branch flat.Node, mut env RecursiveStrEnv, ctx RecursiveStrContext) {
	if !tc.valid_node_id(subject_id) || branch.kind != .match_branch || !branch.value.is_int() {
		return
	}
	subject := tc.a.node(subject_id)
	if subject.kind != .ident {
		return
	}
	mut binding := env.bindings[subject.value] or { return }
	if !binding.can_recurse || binding.progressed {
		return
	}
	condition_count := branch.value.int()
	if condition_count == 0 || condition_count > branch.children_count {
		return
	}
	subject_type := unalias_type(unwrap_pointer(tc.resolve_type(subject_id)))
	receiver_type := unalias_type(unwrap_pointer(ctx.receiver_type))
	for i in 0 .. condition_count {
		condition := tc.a.child_node(&branch, i)
		pattern := tc.match_type_pattern(*condition) or { return }
		smartcast_text := if subject_type is SumType {
			tc.sum_variant_type_for_pattern(subject_type.name, pattern) or { return }
		} else if is_ierror_type(subject_type) {
			tc.resolve_ierror_match_pattern(pattern) or { return }
		} else if subject_type is Interface {
			tc.resolve_interface_match_pattern(pattern) or { return }
		} else {
			return
		}
		smartcast_type := unalias_type(unwrap_pointer(tc.parse_type(smartcast_text)))
		if smartcast_type.name() == receiver_type.name() {
			return
		}
	}
	// Every condition narrows the receiver to a distinct variant. Formatting
	// that variant cannot dispatch back to the sum/interface str method.
	binding.progressed = true
	binding.nonreversible_progress = true
	binding.numeric_deltas = map[string]i64{}
	env.bindings[subject.value] = binding
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
		mut condition_is_true := false
		mut condition_is_false := false
		if value := tc.constant_bool_value(child_id) {
			condition_is_true = value
			condition_is_false = !value
		}
		if i + 1 < node.children_count {
			block_id := tc.a.child(node, i + 1)
			block := tc.a.node(block_id)
			if block.kind == .block {
				if !condition_is_false {
					mut branch_env := condition_env.clone_env()
					tc.recursive_str_apply_condition_facts(child_id, true, mut branch_env)
					result := tc.recursive_str_eval_block_value(*block, mut branch_env, ctx)
					if result.typ_name.len > 0 || result.can_recurse {
						results << result
						branch_envs << branch_env
					}
				}
				base = condition_env
				tc.recursive_str_apply_condition_facts(child_id, false, mut base)
				i += 2
				if condition_is_true {
					break
				}
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
		tc.recursive_str_apply_match_branch_progress(tc.a.child(node, 0), *branch, mut branch_env,
			ctx)
		recursive_str_push_defer_scope(mut branch_env)
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
		tc.recursive_str_run_current_defer_scope(mut branch_env, ctx)
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
		lhs_id := tc.a.child(node, 0)
		tc.recursive_str_eval_condition(lhs_id, mut env, ctx)
		if lhs := tc.constant_bool_value(lhs_id) {
			if (node.op == .logical_and && !lhs) || (node.op == .logical_or && lhs) {
				return
			}
			tc.recursive_str_eval_condition(tc.a.child(node, 1), mut env, ctx)
			return
		}
		mut conditional_env := env.clone_env()
		tc.recursive_str_eval_condition(tc.a.child(node, 1), mut conditional_env, ctx)
		return
	}
	tc.recursive_str_eval_expr(id, mut env, ctx)
}

fn (tc &TypeChecker) recursive_str_apply_condition_facts(id flat.NodeId, truth bool, mut env RecursiveStrEnv) {
	if !tc.valid_node_id(id) {
		return
	}
	node := tc.a.node(id)
	if node.kind == .paren && node.children_count > 0 {
		tc.recursive_str_apply_condition_facts(tc.a.child(node, 0), truth, mut env)
		return
	}
	if node.kind == .prefix && node.op == .not && node.children_count > 0 {
		tc.recursive_str_apply_condition_facts(tc.a.child(node, 0), !truth, mut env)
		return
	}
	if node.kind == .infix && node.children_count >= 2 {
		if node.op == .logical_and && truth {
			tc.recursive_str_apply_condition_facts(tc.a.child(node, 0), true, mut env)
			tc.recursive_str_apply_condition_facts(tc.a.child(node, 1), true, mut env)
			return
		}
		if node.op == .logical_or && !truth {
			tc.recursive_str_apply_condition_facts(tc.a.child(node, 0), false, mut env)
			tc.recursive_str_apply_condition_facts(tc.a.child(node, 1), false, mut env)
			return
		}
		if node.op in [.eq, .ne] {
			lhs_id := tc.a.child(node, 0)
			rhs_id := tc.a.child(node, 1)
			equal := if node.op == .eq { truth } else { !truth }
			if key := tc.recursive_str_condition_field_key(lhs_id) {
				if value := tc.recursive_str_literal_fact_value(rhs_id) {
					tc.recursive_str_set_value_fact(key, value, equal, mut env)
					return
				}
			}
			if key := tc.recursive_str_condition_field_key(rhs_id) {
				if value := tc.recursive_str_literal_fact_value(lhs_id) {
					tc.recursive_str_set_value_fact(key, value, equal, mut env)
				}
			}
			return
		}
	}
	if node.kind == .selector && tc.resolve_type(id).name() == 'bool' {
		if key := tc.recursive_str_condition_field_key(id) {
			tc.recursive_str_set_value_fact(key, 'bool:${truth}', true, mut env)
		}
	}
}

fn (tc &TypeChecker) recursive_str_condition_field_key(id flat.NodeId) ?string {
	if !tc.valid_node_id(id) {
		return none
	}
	node := tc.a.node(id)
	if node.kind == .paren && node.children_count > 0 {
		return tc.recursive_str_condition_field_key(tc.a.child(node, 0))
	}
	if node.kind != .selector {
		return none
	}
	return tc.recursive_str_value_expr_key(id)
}

fn (tc &TypeChecker) recursive_str_value_expr_key(id flat.NodeId) ?string {
	if !tc.valid_node_id(id) {
		return none
	}
	node := tc.a.node(id)
	if node.kind in [.paren, .cast_expr, .as_expr] && node.children_count > 0 {
		return tc.recursive_str_value_expr_key(tc.a.child(node, 0))
	}
	if node.kind == .ident {
		return node.value
	}
	if node.kind == .selector && node.children_count > 0 {
		base := tc.recursive_str_value_expr_key(tc.a.child(node, 0)) or { return none }
		return '${base}.${node.value}'
	}
	return none
}

fn (tc &TypeChecker) recursive_str_literal_fact_value(id flat.NodeId) ?string {
	if !tc.valid_node_id(id) {
		return none
	}
	node := tc.a.node(id)
	if node.kind in [.paren, .cast_expr, .as_expr] && node.children_count > 0 {
		return tc.recursive_str_literal_fact_value(tc.a.child(node, 0))
	}
	return match node.kind {
		.bool_literal {
			'bool:${node.value}'
		}
		.enum_val {
			'enum:${node.value}'
		}
		.int_literal {
			value := numeric_literal_i64(node.value) or { return none }
			if value < min_int || value > max_int {
				none
			} else {
				'int:${value}'
			}
		}
		else {
			none
		}
	}
}

fn (tc &TypeChecker) recursive_str_known_fact_value(id flat.NodeId, env RecursiveStrEnv) ?string {
	if value := tc.recursive_str_literal_fact_value(id) {
		return value
	}
	key := tc.recursive_str_value_expr_key(id) or { return none }
	return env.known_values[key] or { none }
}

fn (tc &TypeChecker) recursive_str_set_value_fact(key string, value string, equal bool, mut env RecursiveStrEnv) {
	if key.len == 0 || value.len == 0 {
		return
	}
	if equal {
		env.known_values[key] = value
		env.excluded_values.delete(key)
		return
	}
	if key in env.known_values {
		return
	}
	mut excluded := if key in env.excluded_values {
		env.excluded_values[key].clone()
	} else {
		map[string]bool{}
	}
	excluded[value] = true
	env.excluded_values[key] = excluded.move()
}

fn (tc &TypeChecker) recursive_str_invalidate_value_facts(id flat.NodeId, mut env RecursiveStrEnv) {
	key := tc.recursive_str_value_expr_key(id) or { return }
	for fact_key in env.known_values.keys() {
		if fact_key == key || fact_key.starts_with('${key}.') || key.starts_with('${fact_key}.') {
			env.known_values.delete(fact_key)
		}
	}
	for fact_key in env.excluded_values.keys() {
		if fact_key == key || fact_key.starts_with('${key}.') || key.starts_with('${fact_key}.') {
			env.excluded_values.delete(fact_key)
		}
	}
}

fn (tc &TypeChecker) recursive_str_merge_envs(envs []RecursiveStrEnv) RecursiveStrEnv {
	if envs.len == 0 {
		return RecursiveStrEnv{}
	}
	mut result := RecursiveStrEnv{
		next_storage_id:    envs[0].next_storage_id
		active_closure_ids: envs[0].active_closure_ids.clone()
		active_helper_ids:  envs[0].active_helper_ids.clone()
	}
	mut names := map[string]bool{}
	mut max_defer_scopes := 0
	for env in envs {
		result.next_storage_id = int_max(result.next_storage_id, env.next_storage_id)
		max_defer_scopes = int_max(max_defer_scopes, env.defer_scopes.len)
		for name, _ in env.bindings {
			names[name] = true
		}
		for id, active in env.active_closure_ids {
			if active {
				result.active_closure_ids[id] = true
			}
		}
		for id, active in env.active_helper_ids {
			if active {
				result.active_helper_ids[id] = true
			}
		}
	}
	for scope_index in 0 .. max_defer_scopes {
		mut defer_stmts := []flat.NodeId{}
		for env in envs {
			if scope_index >= env.defer_scopes.len {
				continue
			}
			for defer_id in env.defer_scopes[scope_index] {
				if defer_id !in defer_stmts {
					defer_stmts << defer_id
				}
			}
		}
		result.defer_scopes << defer_stmts
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
	for key, value in envs[0].known_values {
		mut is_shared := true
		for env in envs[1..] {
			if env.known_values[key] != value {
				is_shared = false
				break
			}
		}
		if is_shared {
			result.known_values[key] = value
		}
	}
	for key, values in envs[0].excluded_values {
		mut shared_values := map[string]bool{}
		for value, _ in values {
			mut is_shared := true
			for env in envs[1..] {
				if key !in env.excluded_values {
					is_shared = false
					break
				}
				if !env.excluded_values[key][value] {
					is_shared = false
					break
				}
			}
			if is_shared {
				shared_values[value] = true
			}
		}
		if shared_values.len > 0 {
			result.excluded_values[key] = shared_values.move()
		}
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
	mut element_keys := bindings[0].element_keys.clone()
	mut repeated_element := true
	mut closure_ids := []flat.NodeId{}
	mut capture_candidates := map[string][]RecursiveStrBinding{}
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
		if binding.element_keys != element_keys {
			element_keys = []string{}
		}
		repeated_element = repeated_element && binding.repeated_element
		for closure_id in binding.closure_ids {
			if closure_id !in closure_ids {
				closure_ids << closure_id
			}
		}
		for i, name in binding.closure_capture_names {
			if i >= binding.closure_captures.len {
				continue
			}
			mut candidates := capture_candidates[name] or { []RecursiveStrBinding{} }
			candidates << binding.closure_captures[i]
			capture_candidates[name] = candidates
		}
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
	mut closure_capture_names := capture_candidates.keys()
	closure_capture_names.sort()
	mut closure_captures := []RecursiveStrBinding{cap: closure_capture_names.len}
	for name in closure_capture_names {
		closure_captures << tc.recursive_str_merge_bindings(capture_candidates[name])
	}
	progressed := can_recurse && !has_unprogressed
	mut nonreversible_progress := false
	mut numeric_deltas := map[string]i64{}
	if progressed {
		mut first := true
		mut can_preserve_deltas := true
		for binding in bindings {
			if !binding.can_recurse {
				continue
			}
			if binding.nonreversible_progress
				|| (binding.progressed && binding.numeric_deltas.len == 0) {
				can_preserve_deltas = false
				break
			}
			if first {
				numeric_deltas = binding.numeric_deltas.clone()
				first = false
			} else if !recursive_str_numeric_deltas_equal(numeric_deltas, binding.numeric_deltas) {
				can_preserve_deltas = false
				break
			}
		}
		if !can_preserve_deltas || numeric_deltas.len == 0 {
			nonreversible_progress = true
			numeric_deltas = map[string]i64{}
		}
	}
	return RecursiveStrBinding{
		can_recurse:             can_recurse
		progressed:              progressed
		nonreversible_progress:  nonreversible_progress
		numeric_deltas:          numeric_deltas
		is_recursive_str_method: is_recursive_str_method
		storage_id:              storage_id
		typ_name:                typ_name
		elements:                elements
		element_keys:            element_keys
		repeated_element:        repeated_element && elements.len > 0
		closure_ids:             closure_ids
		closure_capture_names:   closure_capture_names
		closure_captures:        closure_captures
	}
}

fn recursive_str_numeric_deltas_equal(left map[string]i64, right map[string]i64) bool {
	if left.len != right.len {
		return false
	}
	for key, value in left {
		if right[key] != value {
			return false
		}
	}
	return true
}

fn recursive_str_binding_has_provenance(binding RecursiveStrBinding) bool {
	if binding.can_recurse || binding.is_recursive_str_method || binding.closure_ids.len > 0 {
		return true
	}
	return binding.elements.any(recursive_str_binding_has_provenance(it))
}

fn recursive_str_binding_has_unprogressed_receiver(binding RecursiveStrBinding) bool {
	if binding.can_recurse && !binding.progressed {
		return true
	}
	return binding.elements.any(recursive_str_binding_has_unprogressed_receiver(it))
}

fn (mut tc TypeChecker) recursive_str_eval_channel_receive(result_id flat.NodeId, channel_id flat.NodeId, mut env RecursiveStrEnv, ctx RecursiveStrContext) RecursiveStrBinding {
	tc.recursive_str_eval_expr(channel_id, mut env, ctx)
	if unalias_and_unwrap_pointer_type(tc.resolve_type(channel_id)) !is Channel {
		return RecursiveStrBinding{
			typ_name: tc.resolve_type(result_id).name()
		}
	}
	name := tc.recursive_str_root_ident(channel_id) or {
		return RecursiveStrBinding{
			typ_name: tc.resolve_type(result_id).name()
		}
	}
	channel := env.bindings[name] or {
		return RecursiveStrBinding{
			typ_name: tc.resolve_type(result_id).name()
		}
	}
	if channel.elements.len == 0 {
		return RecursiveStrBinding{
			typ_name: tc.resolve_type(result_id).name()
		}
	}
	mut payload := channel.elements[0]
	if payload.typ_name.len == 0 {
		payload.typ_name = tc.resolve_type(result_id).name()
	}
	tc.recursive_str_set_channel_payloads(name, channel.elements[1..].clone(), mut env)
	return payload
}

fn (tc &TypeChecker) recursive_str_apply_channel_send(channel_id flat.NodeId, payload RecursiveStrBinding, mut env RecursiveStrEnv) bool {
	if unalias_and_unwrap_pointer_type(tc.resolve_type(channel_id)) !is Channel {
		return false
	}
	name := tc.recursive_str_root_ident(channel_id) or { return false }
	channel := env.bindings[name] or { return false }
	mut payloads := channel.elements.clone()
	payloads << payload
	tc.recursive_str_set_channel_payloads(name, payloads, mut env)
	return true
}

fn (tc &TypeChecker) recursive_str_set_channel_payloads(name string, payloads []RecursiveStrBinding, mut env RecursiveStrEnv) {
	source := env.bindings[name] or { return }
	names := env.bindings.keys()
	for alias in names {
		mut binding := env.bindings[alias]
		if alias != name && (source.storage_id == 0 || binding.storage_id != source.storage_id) {
			continue
		}
		binding.elements = payloads.clone()
		binding.element_keys = []string{}
		binding.repeated_element = false
		env.bindings[alias] = binding
	}
}

fn (tc &TypeChecker) recursive_str_apply_array_append(lhs_id flat.NodeId, rhs_id flat.NodeId, rhs RecursiveStrBinding, mut env RecursiveStrEnv) bool {
	if !tc.valid_node_id(lhs_id) || !tc.valid_node_id(rhs_id) {
		return false
	}
	lhs := tc.a.node(lhs_id)
	if lhs.kind != .ident {
		return false
	}
	mut binding := env.bindings[lhs.value] or { return false }
	if binding.repeated_element || binding.element_keys.len > 0 {
		return false
	}
	binding.elements = binding.elements.clone()
	rhs_type := unalias_type(tc.resolve_type(rhs_id))
	if rhs_type is Array || rhs_type is ArrayFixed {
		binding.elements << rhs.elements
	} else {
		binding.elements << rhs
	}
	tc.recursive_str_invalidate_value_facts(lhs_id, mut env)
	env.bindings[lhs.value] = binding
	return true
}

fn (tc &TypeChecker) recursive_str_replace_aggregate_slot(target_id flat.NodeId, rhs RecursiveStrBinding, mut env RecursiveStrEnv) bool {
	name := tc.recursive_str_root_ident(target_id) or { return false }
	path := tc.recursive_str_aggregate_slot_path(target_id) or { return false }
	if path.len == 0 {
		return false
	}
	mut binding := env.bindings[name] or { return false }
	if !tc.recursive_str_replace_aggregate_binding_slot(path, 0, rhs, mut binding) {
		return false
	}
	tc.recursive_str_invalidate_value_facts(target_id, mut env)
	env.bindings[name] = binding
	return true
}

fn (tc &TypeChecker) recursive_str_aggregate_slot_path(id flat.NodeId) ?[]RecursiveStrAggregateSlot {
	if !tc.valid_node_id(id) {
		return none
	}
	node := tc.a.node(id)
	if node.kind == .ident {
		return []RecursiveStrAggregateSlot{}
	}
	if node.kind in [.paren, .prefix, .as_expr, .cast_expr] && node.children_count > 0 {
		return tc.recursive_str_aggregate_slot_path(tc.a.child(node, 0))
	}
	if node.kind == .selector && node.children_count > 0 {
		mut path := tc.recursive_str_aggregate_slot_path(tc.a.child(node, 0)) or { return none }
		path << RecursiveStrAggregateSlot{
			kind: .field
			key:  recursive_str_struct_field_key(node.value)
		}
		return path
	}
	if node.kind == .index && node.children_count >= 2 {
		mut path := tc.recursive_str_aggregate_slot_path(tc.a.child(node, 0)) or { return none }
		path << RecursiveStrAggregateSlot{
			kind:     .index
			index_id: tc.a.child(node, 1)
		}
		return path
	}
	return none
}

fn (tc &TypeChecker) recursive_str_replace_aggregate_binding_slot(path []RecursiveStrAggregateSlot, depth int, rhs RecursiveStrBinding, mut binding RecursiveStrBinding) bool {
	if depth >= path.len {
		binding = rhs
		return true
	}
	slot := path[depth]
	element_index := match slot.kind {
		.index { tc.recursive_str_tracked_element_index(binding, slot.index_id) or { return false } }
		.field { recursive_str_named_element_index(binding, slot.key) or { return false } }
	}
	binding.elements = binding.elements.clone()
	mut element := binding.elements[element_index]
	if !tc.recursive_str_replace_aggregate_binding_slot(path, depth + 1, rhs, mut element) {
		return false
	}
	binding.elements[element_index] = element
	return true
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
	tc.recursive_str_invalidate_value_facts(target_id, mut env)
	if tc.recursive_str_expr_contains_index(target_id)
		&& tc.recursive_str_apply_indexed_mutation(name, target_id, rhs_id, op, mut env) {
		return
	}
	if delta := tc.recursive_str_numeric_mutation_delta(rhs_id, op) {
		if tc.recursive_str_expr_contains_index(target_id) {
			tc.recursive_str_mark_shared_progress(name, mut env)
		} else {
			target := tc.recursive_str_mutation_target_key(target_id)
			tc.recursive_str_apply_numeric_delta(name, target, delta, mut env)
		}
		return
	}
	if tc.recursive_str_expr_contains_index(target_id) {
		tc.recursive_str_mark_shared_progress(name, mut env)
	} else {
		tc.recursive_str_mark_value_progress(name, mut env)
	}
}

fn (mut tc TypeChecker) recursive_str_apply_indexed_mutation(name string, target_id flat.NodeId, rhs_id flat.NodeId, op flat.Op, mut env RecursiveStrEnv) bool {
	indexes := tc.recursive_str_index_path(target_id) or { return false }
	if indexes.len == 0 {
		return false
	}
	mut binding := env.bindings[name] or { return false }
	if !tc.recursive_str_apply_indexed_binding_mutation(indexes, 0, target_id, rhs_id, op, mut
		binding) {
		return false
	}
	env.bindings[name] = binding
	return true
}

fn (tc &TypeChecker) recursive_str_index_path(id flat.NodeId) ?[]flat.NodeId {
	if !tc.valid_node_id(id) {
		return none
	}
	node := tc.a.node(id)
	if node.kind == .ident {
		return []flat.NodeId{}
	}
	if node.kind in [.selector, .paren, .prefix, .as_expr, .cast_expr] && node.children_count > 0 {
		return tc.recursive_str_index_path(tc.a.child(node, 0))
	}
	if node.kind == .index && node.children_count >= 2 {
		mut indexes := tc.recursive_str_index_path(tc.a.child(node, 0)) or { return none }
		indexes << tc.a.child(node, 1)
		return indexes
	}
	return none
}

fn (mut tc TypeChecker) recursive_str_apply_indexed_binding_mutation(indexes []flat.NodeId, depth int, target_id flat.NodeId, rhs_id flat.NodeId, op flat.Op, mut binding RecursiveStrBinding) bool {
	if depth >= indexes.len {
		if delta := tc.recursive_str_numeric_mutation_delta(rhs_id, op) {
			target := tc.recursive_str_mutation_target_key(target_id)
			return tc.recursive_str_apply_numeric_delta_to_binding(target, delta, mut binding)
		}
		return recursive_str_mark_binding_value_progress(mut binding)
	}
	element_index := tc.recursive_str_tracked_element_index(binding, indexes[depth]) or {
		return false
	}
	binding.elements = binding.elements.clone()
	mut element := binding.elements[element_index]
	if !tc.recursive_str_apply_indexed_binding_mutation(indexes, depth + 1, target_id, rhs_id, op, mut
		element) {
		return false
	}
	binding.elements[element_index] = element
	return true
}

fn (tc &TypeChecker) recursive_str_tracked_element_index(binding RecursiveStrBinding, index_id flat.NodeId) ?int {
	if binding.elements.len == 0 || binding.repeated_element {
		return none
	}
	if binding.element_keys.len == binding.elements.len && '' !in binding.element_keys {
		key := tc.recursive_str_constant_map_key(index_id) or { return none }
		for i := binding.element_keys.len - 1; i >= 0; i-- {
			if binding.element_keys[i] == key {
				return i
			}
		}
		return none
	}
	index := tc.recursive_str_constant_index(index_id) or { return none }
	if index < 0 || index >= binding.elements.len {
		return none
	}
	return index
}

fn (tc &TypeChecker) recursive_str_numeric_mutation_delta(rhs_id flat.NodeId, op flat.Op) ?i64 {
	if op == .inc {
		return i64(1)
	}
	if op == .dec {
		return i64(-1)
	}
	if op !in [.plus_assign, .minus_assign] || !tc.valid_node_id(rhs_id) {
		return none
	}
	node := tc.a.node(rhs_id)
	if node.kind in [.paren, .cast_expr, .expr_stmt] && node.children_count > 0 {
		return tc.recursive_str_numeric_mutation_delta(tc.a.child(node, 0), op)
	}
	if node.kind != .int_literal {
		return none
	}
	value := numeric_literal_i64(node.value) or { return none }
	if op == .minus_assign {
		if value == min_i64 {
			return none
		}
		return -value
	}
	return value
}

fn numeric_literal_i64(value string) ?i64 {
	mut clean := value.to_lower().replace('_', '')
	for suffix in ['u8', 'u16', 'u32', 'u64', 'i8', 'i16', 'i32', 'i64'] {
		if clean.ends_with(suffix) {
			clean = clean[..clean.len - suffix.len]
			break
		}
	}
	parsed := strconv.parse_int(clean, 0, 64) or { return none }
	return parsed
}

fn (tc &TypeChecker) recursive_str_mutation_target_key(id flat.NodeId) string {
	return tc.source_text_for_node(id).trim_space().replace(' ', '').replace('\t', '').replace('\n',
		'').replace('\r', '')
}

fn (tc &TypeChecker) recursive_str_apply_numeric_delta(name string, target string, delta i64, mut env RecursiveStrEnv) {
	mut binding := env.bindings[name] or { return }
	if !tc.recursive_str_apply_numeric_delta_to_binding(target, delta, mut binding) {
		return
	}
	env.bindings[name] = binding
}

fn (tc &TypeChecker) recursive_str_apply_numeric_delta_to_binding(target string, delta i64, mut binding RecursiveStrBinding) bool {
	if delta == 0 {
		return true
	}
	if !binding.can_recurse {
		return false
	}
	if binding.nonreversible_progress {
		return true
	}
	if target.len == 0 {
		binding.progressed = true
		binding.nonreversible_progress = true
		binding.numeric_deltas = map[string]i64{}
		return true
	}
	binding.numeric_deltas = binding.numeric_deltas.clone()
	current := binding.numeric_deltas[target]
	if (delta > 0 && current > max_i64 - delta) || (delta < 0 && current < min_i64 - delta) {
		binding.progressed = true
		binding.nonreversible_progress = true
		binding.numeric_deltas = map[string]i64{}
		return true
	}
	next := current + delta
	if next == 0 {
		binding.numeric_deltas.delete(target)
	} else {
		binding.numeric_deltas[target] = next
	}
	binding.progressed = binding.numeric_deltas.len > 0
	return true
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
	if recursive_str_mark_binding_value_progress(mut binding) {
		env.bindings[name] = binding
	}
}

fn recursive_str_mark_binding_value_progress(mut binding RecursiveStrBinding) bool {
	if !binding.can_recurse {
		return false
	}
	binding.progressed = true
	binding.nonreversible_progress = true
	binding.numeric_deltas = map[string]i64{}
	return true
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
			other.nonreversible_progress = true
			other.numeric_deltas = map[string]i64{}
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
	if tc.recursive_str_decl_has_receiver(decl) && callee.kind == .selector
		&& callee.children_count > 0 {
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

fn (tc &TypeChecker) recursive_str_decl_has_receiver(decl flat.Node) bool {
	if decl.children_count == 0 {
		return false
	}
	first := tc.a.child_node(&decl, 0)
	return first.kind == .param && first.op == .dot
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
			if node.children_count > 0 {
				base := tc.recursive_str_binding_for_expr(tc.a.child(node, 0), env)
				if field_binding := tc.recursive_str_named_element_binding(base,
					recursive_str_struct_field_key(node.value), id)
				{
					return field_binding
				}
			}
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
				return tc.recursive_str_index_binding(base, index_id, id, env)
			}
		}
		.array_literal, .array_init {
			mut elements := []RecursiveStrBinding{}
			mut repeated_element := false
			is_empty := tc.recursive_str_array_init_is_empty(*node, env)
			for i in 0 .. node.children_count {
				child_id := tc.a.child(node, i)
				child := tc.a.node(child_id)
				if node.kind == .array_init && child.kind == .field_init {
					if child.value == 'init' && child.children_count > 0 {
						if is_empty {
							continue
						}
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
		.map_init {
			mut elements := []RecursiveStrBinding{}
			mut element_keys := []string{}
			mut i := 0
			for i < int(node.children_count) {
				key_id := tc.a.child(node, i)
				if i + 1 >= node.children_count {
					break
				}
				element_keys << tc.recursive_str_constant_map_key(key_id) or { '' }
				elements << tc.recursive_str_binding_for_expr(tc.a.child(node, i + 1), env)
				i += 2
			}
			return RecursiveStrBinding{
				typ_name:     tc.resolve_type(id).name()
				elements:     elements
				element_keys: element_keys
			}
		}
		.struct_init {
			return tc.recursive_str_struct_binding_for_expr(id, *node, env)
		}
		.assoc {
			if node.children_count > 0 {
				base_id := tc.a.child(node, 0)
				mut binding := tc.recursive_str_binding_for_expr(base_id, env)
				for i in 1 .. node.children_count {
					field := tc.a.child_node(node, i)
					if binding.can_recurse
						&& tc.recursive_str_struct_update_field_changes_base(base_id, *field, env) {
						binding.progressed = true
						binding.nonreversible_progress = true
						binding.numeric_deltas = map[string]i64{}
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

fn (tc &TypeChecker) recursive_str_struct_binding_for_expr(id flat.NodeId, node flat.Node, env RecursiveStrEnv) RecursiveStrBinding {
	target_type := tc.resolve_type(id)
	mut aggregate := RecursiveStrBinding{
		typ_name: target_type.name()
	}
	target_struct := struct_type_from_type(target_type) or { return aggregate }
	fields := tc.struct_fields_for_init(target_struct.name)
	for i in 0 .. node.children_count {
		field := tc.a.child_node(&node, i)
		if field.kind != .field_init || field.children_count != 1 {
			continue
		}
		field_name := if field.value.len > 0 {
			field.value
		} else if i < fields.len {
			fields[i].name
		} else {
			''
		}
		if field_name.len == 0 {
			continue
		}
		aggregate.element_keys << recursive_str_struct_field_key(field_name)
		aggregate.elements << tc.recursive_str_binding_for_expr(tc.a.child(field, 0), env)
	}
	if fields.len == 0 || node.children_count != fields.len {
		return aggregate
	}
	mut receiver_text := ''
	mut receiver_binding := RecursiveStrBinding{}
	mut supplied := map[string]bool{}
	for i in 0 .. node.children_count {
		field := tc.a.child_node(&node, i)
		if field.kind != .field_init || field.children_count != 1 {
			return aggregate
		}
		field_name := if field.value.len > 0 { field.value } else { fields[i].name }
		if field_name in supplied {
			return aggregate
		}
		supplied[field_name] = true
		receiver_id := tc.recursive_str_struct_field_copy_receiver(tc.a.child(field, 0), field_name) or {
			return aggregate
		}
		current_text := tc.source_text_for_node(receiver_id).trim_space()
		if receiver_text.len == 0 {
			receiver_text = current_text
			receiver_binding = tc.recursive_str_binding_for_expr(receiver_id, env)
			receiver_type := tc.resolve_type(receiver_id)
			if !receiver_binding.can_recurse || !tc.type_compatible(receiver_type, target_type)
				|| !tc.type_compatible(target_type, receiver_type) {
				return aggregate
			}
		} else if current_text != receiver_text {
			return aggregate
		}
	}
	for field in fields {
		if field.name !in supplied {
			return aggregate
		}
	}
	receiver_binding.typ_name = target_type.name()
	return receiver_binding
}

fn (tc &TypeChecker) recursive_str_returned_params(decl flat.Node) ?[]RecursiveStrReturnedParam {
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
	mut returned := []RecursiveStrReturnedParam{}
	for stack.len > 0 {
		id := stack.pop()
		node := tc.a.node(id)
		if node.kind in [.fn_decl, .fn_literal, .lambda_expr] {
			continue
		}
		if node.kind == .return_stmt {
			for slot in 0 .. node.children_count {
				value_id := tc.a.child(node, slot)
				name := tc.recursive_str_returned_param_root(value_id) or { '' }
				index := params[name] or { -1 }
				returned << RecursiveStrReturnedParam{
					name:     name
					index:    index
					slot:     slot
					value_id: value_id
				}
			}
			continue
		}
		for i in 0 .. node.children_count {
			stack << tc.a.child(node, i)
		}
	}
	if returned.len > 0 {
		return returned
	}
	return none
}

fn (tc &TypeChecker) recursive_str_returned_param_root(id flat.NodeId) ?string {
	if !tc.valid_node_id(id) {
		return none
	}
	node := tc.a.node(id)
	if node.kind in [.assoc, .paren, .cast_expr, .as_expr, .dump_expr] && node.children_count > 0 {
		return tc.recursive_str_returned_param_root(tc.a.child(node, 0))
	}
	return tc.recursive_str_root_ident(id)
}

fn (tc &TypeChecker) recursive_str_direct_returned_ident(id flat.NodeId) ?string {
	if !tc.valid_node_id(id) {
		return none
	}
	node := tc.a.node(id)
	if node.kind == .ident {
		return node.value
	}
	if node.kind in [.paren, .cast_expr, .as_expr, .dump_expr] && node.children_count > 0 {
		return tc.recursive_str_direct_returned_ident(tc.a.child(node, 0))
	}
	return none
}

fn (mut tc TypeChecker) recursive_str_call_return_binding(call_id flat.NodeId, env RecursiveStrEnv) ?RecursiveStrBinding {
	resolved := tc.resolved_call_name(call_id) or { return none }
	decl_id := tc.recursive_str_fn_decl_id(resolved) or { return none }
	decl := tc.a.node(decl_id)
	returned := tc.recursive_str_returned_params(*decl) or { return none }
	call := tc.a.node(call_id)
	actuals := tc.recursive_str_call_param_actuals(*call, *decl)
	mut return_env := env.clone_env()
	for name, actual_id in actuals {
		return_env.bindings[name] = tc.recursive_str_binding_for_expr(actual_id, env)
	}
	return_type := tc.resolve_type(call_id)
	slot_count := if multi := multi_return_payload_type(return_type) {
		multi.types.len
	} else {
		1
	}
	mut slot_bindings := [][]RecursiveStrBinding{len: slot_count}
	for returned_param in returned {
		if returned_param.slot >= slot_bindings.len {
			continue
		}
		mut binding := tc.recursive_str_binding_for_expr(returned_param.value_id, return_env)
		if returned_param.index >= 0
			&& tc.recursive_str_direct_returned_ident(returned_param.value_id) != none {
			effect := tc.recursive_str_guaranteed_param_effect(*decl, returned_param.index)
			match effect.kind {
				.value, .shared {
					if binding.can_recurse {
						binding.progressed = true
						binding.nonreversible_progress = true
						binding.numeric_deltas = map[string]i64{}
					}
				}
				.rebind {
					binding = return_env.bindings[effect.source_param] or { binding }
				}
				else {}
			}
		}
		slot_bindings[returned_param.slot] << binding
	}
	if slot_count == 1 {
		if slot_bindings[0].len == 0 {
			return none
		}
		return tc.recursive_str_merge_bindings(slot_bindings[0])
	}
	multi := multi_return_payload_type(return_type) or { return none }
	mut elements := []RecursiveStrBinding{cap: slot_count}
	for i in 0 .. slot_count {
		mut binding := tc.recursive_str_merge_bindings(slot_bindings[i])
		if binding.typ_name.len == 0 {
			binding.typ_name = multi.types[i].name()
		}
		elements << binding
	}
	return RecursiveStrBinding{
		typ_name: return_type.name()
		elements: elements
	}
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
	if tc.recursive_str_decl_has_receiver(*decl) && callee.kind == .selector
		&& callee.children_count > 0 {
		receiver_id := tc.a.child(callee, 0)
		receiver_param := tc.a.child_node(decl, 0)
		if receiver_param.is_mut {
			tc.recursive_str_invalidate_value_facts(receiver_id, mut env)
		}
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
			tc.recursive_str_invalidate_value_facts(arg_id, mut env)
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
	short_name := name.all_after_last('.')
	if idx := tc.fn_decl_short_name_ids[short_name] {
		id := flat.NodeId(idx)
		if tc.recursive_str_fn_decl_matches(*tc.a.node(id), name) {
			return id
		}
	}
	// Colliding short names are uncommon. Preserve declaration-order semantics
	// for them with the old scan only when the indexed first declaration did
	// not match the requested qualified name.
	for idx in tc.top_level_idx {
		id := flat.NodeId(idx)
		node := tc.a.node(id)
		if node.kind != .fn_decl {
			continue
		}
		if tc.recursive_str_fn_decl_matches(*node, name) {
			return id
		}
	}
	return none
}

fn (tc &TypeChecker) recursive_str_fn_decl_matches(node flat.Node, name string) bool {
	if node.value == name || node.value == name.trim_string_left('main.') {
		return true
	}
	module_name := if source_file := tc.a.source_files[node.pos.id] {
		tc.file_modules[source_file.name] or { tc.cur_module }
	} else {
		tc.cur_module
	}
	return checker_qualified_fn_name(module_name, node.value) == name
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
		.for_stmt {
			return tc.recursive_str_loop_param_effect(*node, name, depth)
		}
		else {}
	}
	return RecursiveStrParamEffect{}
}

fn (mut tc TypeChecker) recursive_str_loop_param_effect(node flat.Node, name string, depth int) RecursiveStrParamEffect {
	if !tc.recursive_str_loop_guarantees_entry(node) || node.children_count < 3 {
		return RecursiveStrParamEffect{}
	}
	flow := tc.recursive_str_param_loop_sequence(node, 3, [RecursiveStrParamEffect{}], name, depth)
	if flow.fallthrough.len > 0 || flow.continues.len > 0 || flow.returns.len > 0 {
		return RecursiveStrParamEffect{}
	}
	return recursive_str_merge_param_effects(flow.breaks)
}

fn (mut tc TypeChecker) recursive_str_param_loop_sequence(node flat.Node, start int, initial []RecursiveStrParamEffect, name string, depth int) RecursiveStrParamLoopFlow {
	mut active := initial.clone()
	mut breaks := []RecursiveStrParamEffect{}
	mut continues := []RecursiveStrParamEffect{}
	mut returns := []RecursiveStrParamEffect{}
	for i in start .. int(node.children_count) {
		mut next := []RecursiveStrParamEffect{}
		for source in active {
			flow := tc.recursive_str_param_loop_control_stmt(tc.a.child(&node, i), source, name,
				depth)
			next << flow.fallthrough
			breaks << flow.breaks
			continues << flow.continues
			returns << flow.returns
		}
		active = next.clone()
		if active.len == 0 {
			break
		}
	}
	return RecursiveStrParamLoopFlow{
		fallthrough: active
		breaks:      breaks
		continues:   continues
		returns:     returns
	}
}

fn (mut tc TypeChecker) recursive_str_param_loop_control_stmt(id flat.NodeId, source RecursiveStrParamEffect, name string, depth int) RecursiveStrParamLoopFlow {
	if !tc.valid_node_id(id) {
		return RecursiveStrParamLoopFlow{
			fallthrough: [source]
		}
	}
	node := tc.a.node(id)
	match node.kind {
		.break_stmt {
			return RecursiveStrParamLoopFlow{
				breaks: [source]
			}
		}
		.continue_stmt {
			return RecursiveStrParamLoopFlow{
				continues: [source]
			}
		}
		.return_stmt {
			return RecursiveStrParamLoopFlow{
				returns: [source]
			}
		}
		.block {
			return tc.recursive_str_param_loop_sequence(*node, 0, [source], name, depth)
		}
		.if_expr {
			return tc.recursive_str_param_loop_if(*node, source, name, depth)
		}
		.match_stmt {
			return tc.recursive_str_param_loop_match(*node, source, name, depth)
		}
		.select_stmt {
			return tc.recursive_str_param_loop_select(*node, source, name, depth)
		}
		else {
			effect := tc.recursive_str_stmt_param_effect(id, name, depth)
			final_effect := if effect.kind == .none { source } else { effect }
			if node.kind == .expr_stmt && tc.expr_never_returns(id) {
				return RecursiveStrParamLoopFlow{
					returns: [final_effect]
				}
			}
			return RecursiveStrParamLoopFlow{
				fallthrough: [final_effect]
			}
		}
	}
}

fn (mut tc TypeChecker) recursive_str_param_loop_if(node flat.Node, source RecursiveStrParamEffect, name string, depth int) RecursiveStrParamLoopFlow {
	mut fallthrough := []RecursiveStrParamEffect{}
	mut breaks := []RecursiveStrParamEffect{}
	mut continues := []RecursiveStrParamEffect{}
	mut returns := []RecursiveStrParamEffect{}
	mut has_else := false
	mut has_guaranteed_branch := false
	mut i := 0
	for i < node.children_count {
		child_id := tc.a.child(&node, i)
		child := tc.a.node(child_id)
		if child.kind == .block {
			has_else = true
			flow := tc.recursive_str_param_loop_sequence(*child, 0, [source], name, depth)
			fallthrough << flow.fallthrough
			breaks << flow.breaks
			continues << flow.continues
			returns << flow.returns
			break
		}
		mut condition_is_true := false
		mut condition_is_false := false
		if value := tc.constant_bool_value(child_id) {
			condition_is_true = value
			condition_is_false = !value
		}
		if i + 1 >= node.children_count {
			i++
			continue
		}
		block := tc.a.child_node(&node, i + 1)
		if block.kind != .block {
			i++
			continue
		}
		if !condition_is_false {
			flow := tc.recursive_str_param_loop_sequence(*block, 0, [source], name, depth)
			fallthrough << flow.fallthrough
			breaks << flow.breaks
			continues << flow.continues
			returns << flow.returns
		}
		i += 2
		if condition_is_true {
			has_guaranteed_branch = true
			break
		}
	}
	if !has_else && !has_guaranteed_branch {
		fallthrough << source
	}
	return RecursiveStrParamLoopFlow{
		fallthrough: fallthrough
		breaks:      breaks
		continues:   continues
		returns:     returns
	}
}

fn (mut tc TypeChecker) recursive_str_param_loop_match(node flat.Node, source RecursiveStrParamEffect, name string, depth int) RecursiveStrParamLoopFlow {
	mut fallthrough := []RecursiveStrParamEffect{}
	mut breaks := []RecursiveStrParamEffect{}
	mut continues := []RecursiveStrParamEffect{}
	mut returns := []RecursiveStrParamEffect{}
	for i in 1 .. node.children_count {
		branch := tc.a.child_node(&node, i)
		if branch.kind != .match_branch {
			continue
		}
		start := if branch.value.is_int() { branch.value.int() } else { 0 }
		flow := tc.recursive_str_param_loop_sequence(*branch, start, [source], name, depth)
		fallthrough << flow.fallthrough
		breaks << flow.breaks
		continues << flow.continues
		returns << flow.returns
	}
	if !tc.match_has_else_or_exhaustive_coverage(node) {
		fallthrough << source
	}
	return RecursiveStrParamLoopFlow{
		fallthrough: fallthrough
		breaks:      breaks
		continues:   continues
		returns:     returns
	}
}

fn (mut tc TypeChecker) recursive_str_param_loop_select(node flat.Node, source RecursiveStrParamEffect, name string, depth int) RecursiveStrParamLoopFlow {
	mut fallthrough := []RecursiveStrParamEffect{}
	mut breaks := []RecursiveStrParamEffect{}
	mut continues := []RecursiveStrParamEffect{}
	mut returns := []RecursiveStrParamEffect{}
	for i in 0 .. node.children_count {
		branch := tc.a.child_node(&node, i)
		if branch.kind != .select_branch {
			continue
		}
		flow := tc.recursive_str_param_loop_sequence(*branch, 0, [source], name, depth)
		fallthrough << flow.fallthrough
		breaks << flow.breaks
		continues << flow.continues
		returns << flow.returns
	}
	return RecursiveStrParamLoopFlow{
		fallthrough: fallthrough
		breaks:      breaks
		continues:   continues
		returns:     returns
	}
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
	mut has_guaranteed_branch := false
	mut i := 0
	for i < node.children_count {
		child_id := tc.a.child(node, i)
		child := tc.a.node(child_id)
		if child.kind == .block {
			has_else = true
			effect := tc.recursive_str_stmt_param_effect(child_id, name, depth)
			final_effect := if effect.kind == .none { incoming } else { effect }
			if tc.recursive_str_stmt_may_return(*child) {
				terminal << final_effect
			}
			if !tc.recursive_str_stmt_always_returns(*child) {
				fallthrough << final_effect
			}
			break
		}
		mut condition_is_true := false
		mut condition_is_false := false
		if value := tc.constant_bool_value(child_id) {
			condition_is_true = value
			condition_is_false = !value
		}
		if i + 1 >= node.children_count {
			i++
			continue
		}
		block_id := tc.a.child(node, i + 1)
		block := tc.a.node(block_id)
		if block.kind != .block {
			i++
			continue
		}
		if !condition_is_false {
			effect := tc.recursive_str_stmt_param_effect(block_id, name, depth)
			final_effect := if effect.kind == .none { incoming } else { effect }
			if tc.recursive_str_stmt_may_return(*block) {
				terminal << final_effect
			}
			if !tc.recursive_str_stmt_always_returns(*block) {
				fallthrough << final_effect
			}
		}
		i += 2
		if condition_is_true {
			has_guaranteed_branch = true
			break
		}
	}
	if !has_else && !has_guaranteed_branch {
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
