module types

import os
import v.flat
import v.parser
import v.pref

fn test_mutable_pointer_lookup_guard_keeps_value_and_slot_arguments() {
	cases := [
		'fn take(p &int) {}\nfn main() { x := 1; take(&x) }',
		'fn take(mut p &int) {}\nfn main() { mut x := 1; mut p := &x; take(mut p) }',
		'type IntPtr = &int\nfn take(mut p IntPtr) {}\nfn main() { mut x := 1; mut p := IntPtr(&x); take(mut p) }',
		'fn take(mut x []int) {}\nfn main() { mut x := [1]; take(mut x) }',
		'fn take[T](mut x T) {}\nfn main() { mut x := [1]; take[[]int](mut x) }',
		'struct Box { value int }\nfn (mut b Box) take(mut x []int) {}\nfn main() { mut b := Box{}; mut x := [1]; b.take(mut x) }',
	]
	mut slot_count := 0
	mut immutable_count := 0
	for index, source in cases {
		for scoped in [false, true] {
			for trusted in [false, true] {
				path := os.join_path(os.vtmp_dir(), 'call_mut_pointer_${index}.v')
				os.write_file(path, 'module main\n' + source + '\n')!
				mut p := parser.Parser.new(pref.new_preferences())
				a := p.parse_file(path)
				assert p.diagnostics.len == 0, '${index}: ${p.diagnostics}'
				mut tc := TypeChecker.new(a)
				tc.scope_parallel_check_workers = scoped
				tc.collect(a)
				_ = tc.check_semantics_opt(false)
				tc.trust_checked_expr_types = trusted
				tc.annotate_types()
				os.rm(path) or {}
				assert tc.errors.len == 0, '${index}: ${tc.errors}'
				println('SLOT_CASE:${index}:${scoped}:${trusted}:${tc.errors.len}:${tc.notices.len}')
				for notice in tc.notices {
					println('SLOT_NOTICE:${notice.kind}:${int(notice.node)}:${notice.pos.offset}:${notice.pos.end}:${notice.msg}:${notice.details.join('|')}')
				}
				for id, node in a.nodes {
					if typ := tc.expr_type(flat.NodeId(id)) {
						println('SLOT_TYPE:${index}:${scoped}:${trusted}:${id}:${node.kind}:${typ.name()}')
					}
					if node.kind != .call { continue }
					if info := tc.resolve_call_info(flat.NodeId(id), node) {
						for param_idx in 0 .. info.params.len {
							is_mut := tc.call_param_is_mut(info, param_idx)
							requires_slot := tc.call_param_requires_mut_pointer_slot(info, param_idx)
							if requires_slot { slot_count++ }
							if !is_mut {
								immutable_count++
								assert !requires_slot
							}
							println('SLOT_PARAM:${index}:${scoped}:${trusted}:${id}:${param_idx}:${is_mut}:${requires_slot}')
						}
					}
				}
			}
		}
	}
	assert slot_count > 0
	assert immutable_count > 0
}
