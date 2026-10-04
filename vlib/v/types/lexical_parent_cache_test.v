module types

import os
import v.flat
import v.parser
import v.pref

fn lexical_parent_cache_children(mut a flat.FlatAst, kind flat.NodeKind, children []flat.NodeId) flat.NodeId {
	start := a.begin_children()
	for child in children {
		a.add_child(child)
	}
	return a.add_node(flat.Node{
		kind:           kind
		children_start: start
		children_count: children.len
	})
}

fn lexical_parent_cache_condition(mut a flat.FlatAst, name string, pattern string) flat.NodeId {
	subject := a.add_val(.ident, name)
	start := a.begin_children()
	a.add_child(subject)
	return a.add_node(flat.Node{
		kind:           .is_expr
		value:          pattern
		children_start: start
		children_count: 1
	})
}

fn test_lexical_parent_cache_preserves_empty_checker_and_invalid_id_handling() {
	mut tc := TypeChecker{}
	tc.expr_type_set = [true]
	tc.invalidate_checked_expr_type(-1)
	assert tc.expr_type_set[0]
	tc.invalidate_checked_expr_type(0)
	assert !tc.expr_type_set[0]
	parent, child, depth := tc.lexical_parent_link(flat.empty_node)
	assert parent == flat.empty_node
	assert child == flat.empty_node
	assert depth == 1
	assert isnil(tc.lexical_parent_memo)
}

fn test_lexical_parent_cache_preserves_if_else_and_for_branch_children() {
	mut a := flat.FlatAst.new()
	then_value := a.add_val(.ident, 'value')
	else_value := a.add_val(.ident, 'value')
	loop_value := a.add_val(.ident, 'value')
	negated_else_value := a.add_val(.ident, 'value')
	mut nested := then_value
	// If narrowing is not subject to the match walk's 64-parent limit.
	for _ in 0 .. 80 {
		nested = lexical_parent_cache_children(mut a, .paren, [nested])
	}
	then_body := lexical_parent_cache_children(mut a, .block, [nested])
	else_body := lexical_parent_cache_children(mut a, .block, [else_value])
	cond := lexical_parent_cache_condition(mut a, 'value', 'int')
	if_node := lexical_parent_cache_children(mut a, .if_expr, [cond, then_body, else_body])
	loop_condition := lexical_parent_cache_condition(mut a, 'value', 'string')
	init := a.add(.empty)
	post := a.add(.empty)
	loop_body := lexical_parent_cache_children(mut a, .block, [loop_value])
	loop := lexical_parent_cache_children(mut a, .for_stmt, [init, loop_condition, post, loop_body])
	negated_condition := lexical_parent_cache_condition(mut a, 'value', 'int')
	negation := lexical_parent_cache_children(mut a, .prefix, [negated_condition])
	a.nodes[int(negation)].op = .not
	negated_then := a.add(.block)
	negated_else := lexical_parent_cache_children(mut a, .block, [negated_else_value])
	negated_if := lexical_parent_cache_children(mut a, .if_expr, [negation, negated_then, negated_else])
	outer_body := lexical_parent_cache_children(mut a, .block, [if_node, loop, negated_if])
	lexical_parent_cache_children(mut a, .fn_decl, [outer_body])
	mut tc := TypeChecker.new(&a)
	tc.build_direct_parent_index(&a)
	tc.sum_types['Value'] = ['int', 'string']
	tc.cur_scope.insert('value', Type(SumType{ name: 'Value' }))
	for cached in [true, true, false, true] {
		tc.cache_lexical_parents = cached
		assert tc.lexical_smartcast_type_in_parents(then_value, 'value', false)? == Type(int_)
		assert tc.lexical_smartcast_type_in_parents(else_value, 'value', false) == none
		assert tc.lexical_smartcast_type_in_parents(negated_else_value, 'value', false)? == Type(int_)
		assert tc.lexical_smartcast_type_in_parents(loop_value, 'value', false)? == Type(String{})
		assert tc.lexical_smartcast_type_in_parents(then_value, 'value', true) == none
	}
	parent, child, depth := tc.lexical_parent_link(then_value)
	assert parent == if_node
	assert child == then_body
	assert depth == 82
}

fn test_lexical_parent_cache_rechecks_condition_and_preceding_writes() {
	mut a := flat.FlatAst.new()
	value := a.add_val(.ident, 'value')
	lhs := a.add_val(.ident, 'value')
	rhs := a.add_val(.int_literal, '1')
	write := lexical_parent_cache_children(mut a, .block, [lhs, rhs])
	body := lexical_parent_cache_children(mut a, .block, [write, value])
	cond := lexical_parent_cache_condition(mut a, 'value', 'int')
	if_node := lexical_parent_cache_children(mut a, .if_expr, [cond, body])
	lexical_parent_cache_children(mut a, .fn_decl, [if_node])
	mut tc := TypeChecker.new(&a)
	tc.build_direct_parent_index(&a)
	tc.sum_types['Value'] = ['int', 'string']
	tc.cur_scope.insert('value', Type(SumType{ name: 'Value' }))
	assert tc.lexical_smartcast_type_in_parents(value, 'value', false)? == Type(int_)
	// Neither the type result nor write analysis is stored in the topology memo.
	a.nodes[int(cond)].value = 'string'
	assert tc.lexical_smartcast_type_in_parents(value, 'value', false)? == Type(String{})
	a.nodes[int(write)].kind = .assign
	assert tc.lexical_smartcast_type_in_parents(value, 'value', false) == none
	tc.cache_lexical_parents = false
	assert tc.lexical_smartcast_type_in_parents(value, 'value', false) == none
	// Rewriting an ordinary ancestor into a function boundary invalidates links,
	// even when no body-local resolve memo was armed.
	a.nodes[int(write)].kind = .block
	tc.cache_lexical_parents = true
	assert tc.lexical_smartcast_type_in_parents(value, 'value', false)? == Type(String{})
	a.nodes[int(body)].kind = .fn_literal
	tc.invalidate_checked_expr_type(int(body))
	assert tc.lexical_smartcast_type_in_parents(value, 'value', false) == none
}

fn test_lexical_parent_cache_forks_own_slots_and_phase_resets_drop_storage() {
	mut a := flat.FlatAst.new()
	value := a.add_val(.ident, 'value')
	parent := lexical_parent_cache_children(mut a, .fn_literal, [value])
	mut tc := TypeChecker.new(&a)
	tc.build_direct_parent_index(&a)
	linked, _, _ := tc.lexical_parent_link(value)
	assert linked == parent
	mut forked := tc.fork_program_view(&a, map[int][]SymbolId{})
	assert isnil(forked.lexical_parent_memo)
	fork_parent, _, _ := forked.lexical_parent_link(value)
	assert fork_parent == parent
	assert voidptr(forked.lexical_parent_memo) != voidptr(tc.lexical_parent_memo)
	fork_generation := forked.lexical_parent_memo.generation
	tc.invalidate_lexical_parent_memo()
	assert forked.lexical_parent_memo.generation == fork_generation
	tc.set_fresh_type_cache(true)
	assert isnil(tc.lexical_parent_memo)
	tc.lexical_parent_link(value)
	tc.reset_body_resolve_memo()
	assert isnil(tc.lexical_parent_memo)
	forked.set_fresh_type_cache_based_on(&tc, true)
	assert isnil(forked.lexical_parent_memo)
}

fn test_lexical_parent_cache_matches_uncached_reachability_and_diagnostics() {
	path := os.join_path(os.vtmp_dir(), 'v3_lexical_parent_differential_${os.getpid()}.v')
	os.write_file(path, 'module main
struct First { value int }
struct Second { value string }
type Value = First | Second
fn (item First) leaf() int { return item.value }
fn inspect(value Value) int {
 if value is First {
  if true { return value.leaf() }
 } else { return 0 }
 return 1
}
fn loop(value Value) {
 for value is First { _ := value.leaf(); break }
}
fn main() {
 value := Value(First{})
 _ := inspect(value)
 loop(value)
 unknown_symbol()
}
')!
	defer { os.rm(path) or {} }
	mut expected_names := []string{}
	mut expected_errors := []TypeError{}
	for cached in [false, true, true] {
		mut p := parser.Parser.new(pref.new_preferences())
		mut a := p.parse_file(path)
		assert p.diagnostics.len == 0, p.diagnostics.str()
		mut tc := TypeChecker.new(a)
		tc.cache_lexical_parents = cached
		tc.diagnose_unknown_calls = true
		tc.diagnostic_files[path] = true
		tc.collect(a)
		tc.collect_selected_file_called_fns()
		mut actual_names := tc.selected_file_called_fns.keys()
		actual_names.sort()
		assert actual_names.len >= 3, actual_names.str()
		tc.check_semantics_opt(false)
		assert tc.errors.any(it.msg.contains('unknown function') && it.msg.contains('unknown_symbol')), tc.errors.str()
		if !cached {
			expected_names = actual_names.clone()
			expected_errors = tc.errors.clone()
		} else {
			assert actual_names == expected_names
			assert tc.errors == expected_errors
			assert !isnil(tc.lexical_parent_memo)
		}
	}
}
