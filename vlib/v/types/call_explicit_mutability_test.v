module types

import os
import v.flat
import v.parser
import v.pref

fn test_receiver_queries_preserve_explicit_mutability_diagnostics() {
	cases := [
		'fn take(xs []int) {}\nfn main() { xs := [1]; take(xs) }',
		'fn take(mut xs []int) {}\nfn main() { mut xs := [1]; take(mut xs) }',
		'fn take(mut xs []int) {}\nfn main() { mut xs := [1]; take(xs) }',
		'fn take(xs []int) {}\nfn main() { mut xs := [1]; take(mut xs) }',
		'fn take(mut xs []int) {}\nfn main() { xs := [1]; take(mut xs) }',
		'fn take(p &int) {}\nfn main() { x := 1; take(&x) }',
		'fn take(mut p &int) {}\nfn main() { mut x := 1; mut p := &x; take(mut p) }',
		'fn take(mut p &int) {}\nfn main() { mut x := 1; mut p := &x; take(p) }',
		'type IntPtr = &int\nfn take(mut p IntPtr) {}\nfn main() { mut x := 1; mut p := IntPtr(&x); take(mut p) }',
		'type IntPtr = &int\nfn take(mut p IntPtr) {}\nfn main() { mut x := 1; mut p := IntPtr(&x); take(p) }',
		'fn take[T](mut xs T) {}\nfn main() { mut xs := [1]; take[[]int](mut xs) }',
		'fn take[T](mut xs T) {}\nfn main() { mut xs := [1]; take[[]int](xs) }',
		'struct Box { value int }\nfn (mut b Box) take(mut xs []int) {}\nfn main() { mut b := Box{}; mut xs := [1]; b.take(mut xs) }',
		'struct Box { value int }\nfn (mut b Box) take(mut xs []int) {}\nfn main() { mut b := Box{}; mut xs := [1]; b.take(xs) }',
		'struct Box { value int }\nfn (b Box) take(xs []int) {}\nfn main() { b := Box{}; xs := [1]; b.take(xs) }',
		'fn take(mut xs []int) {}\nfn main() { mut xs := [1]; callback := take; callback(mut xs) }',
		'fn take(mut xs []int) {}\nfn main() { mut xs := [1]; callback := take; callback(xs) }',
		'fn main() { mut xs := [1]; callback := fn (mut values []int) {}; callback(mut xs) }',
		'fn main() { mut xs := [1]; callback := fn (mut values []int) {}; callback(xs) }',
		'struct Holder { callback fn (mut []int) }\nfn take(mut xs []int) {}\nfn main() { h := Holder{callback: take}; mut xs := [1]; h.callback(mut xs) }',
		'struct Holder { callback fn (mut []int) }\nfn take(mut xs []int) {}\nfn main() { h := Holder{callback: take}; mut xs := [1]; h.callback(xs) }',
		'fn take(xs []int) {}\nfn main() { take([1]); take([]); take(if true { [2] } else { [] }) }',
	]
	for index, source in cases {
		for disabled in [false, true] {
			for scoped in [false, true] {
				for trusted in [false, true] {
					path := os.join_path(os.vtmp_dir(), 'call_explicit_mutability_${index}.v')
					os.write_file(path, 'module main\n' + source + '\n')!
					mut p := parser.Parser.new(pref.new_preferences())
					a := p.parse_file(path)
					assert p.diagnostics.len == 0, '${index}: ${p.diagnostics}'
					mut tc := TypeChecker.new(a)
					tc.scope_parallel_check_workers = scoped
					tc.disable_explicit_mutability = disabled
					tc.collect(a)
					_ = tc.check_semantics_opt(false)
					tc.trust_checked_expr_types = trusted
					tc.annotate_types()
					os.rm(path) or {}
					expected_errors := if disabled {
						[0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
					} else {
						[0, 0, 2, 1, 1, 0, 0, 2, 0, 2, 0, 2, 0, 1, 0, 0, 2, 0, 2, 0, 1, 0]
					}
					expected_notices := if disabled {
						[1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 2, 2, 2]
					} else {
						[1, 1, 2, 1, 1, 1, 1, 2, 1, 2, 1, 2, 1, 1, 1, 1, 2, 0, 1, 2, 2, 2]
					}
					assert tc.errors.len == expected_errors[index], '${index}: ${tc.errors}'
					assert tc.notices.len == expected_notices[index], '${index}: ${tc.notices}'
					println('EXPLICIT_CASE:${index}:${disabled}:${scoped}:${trusted}:${tc.errors.len}:${tc.notices.len}')
					for err in tc.errors {
						println('EXPLICIT_ERROR:${err.kind}:${int(err.node)}:${err.pos.offset}:${err.pos.end}:${err.msg}:${err.details.join('|')}')
					}
					for notice in tc.notices {
						println('EXPLICIT_NOTICE:${notice.kind}:${int(notice.node)}:${notice.pos.offset}:${notice.pos.end}:${notice.msg}:${notice.details.join('|')}')
					}
					for id, node in a.nodes {
						if typ := tc.expr_type(flat.NodeId(id)) {
							println('EXPLICIT_TYPE:${index}:${disabled}:${scoped}:${trusted}:${id}:${node.kind}:${typ.name()}')
						}
						if node.typ != 0 {
							println('EXPLICIT_TEXT:${index}:${disabled}:${scoped}:${trusted}:${id}:${flat.source_type_name(node.typ)}')
						}
						if node.kind == .call {
							println('EXPLICIT_CALL:${index}:${disabled}:${scoped}:${trusted}:${id}:${tc.cached_resolved_call(flat.NodeId(id)) or { '' }}')
						}
					}
				}
			}
		}
	}
}

fn test_receiver_identity_queries_cover_selector_and_lowered_calls() {
	mut a := flat.FlatAst.new()
	receiver := a.add_val(.ident, 'receiver')
	other := a.add_val(.ident, 'other')
	selector_start := a.begin_children()
	a.add_child(receiver)
	selector := a.add_node(flat.Node{
		kind:           .selector
		value:          'update'
		children_start: selector_start
		children_count: 1
	})
	call_start := a.begin_children()
	a.add_child(selector)
	a.add_child(receiver)
	call := a.add_node(flat.Node{
		kind:           .call
		children_start: call_start
		children_count: 2
	})
	callee := a.add_val(.ident, 'update')
	lowered_start := a.begin_children()
	a.add_child(callee)
	a.add_child(receiver)
	lowered := a.add_node(flat.Node{
		kind:           .call
		children_start: lowered_start
		children_count: 2
	})
	tc := TypeChecker.new(&a)
	assert tc.call_arg_is_callee_receiver(*a.node(call), receiver)
	assert !tc.call_arg_is_callee_receiver(*a.node(call), other)
	assert !tc.call_arg_is_callee_receiver(*a.node(lowered), receiver)
	assert !tc.call_arg_is_callee_receiver(flat.Node{}, flat.empty_node)
	box := Type(Struct{ name_id: flat.source_type_id('mod.Box') })
	info := CallInfo{ name: 'mod.Box.update' }
	assert tc.call_arg_is_lowered_method_receiver(*a.node(lowered), info, 0, box)
	assert !tc.call_arg_is_lowered_method_receiver(*a.node(lowered), info, 1, box)
	assert !tc.call_arg_is_lowered_method_receiver(*a.node(call), info, 0, box)
	assert !tc.call_arg_is_lowered_method_receiver(*a.node(lowered), CallInfo{}, 0, box)
}
