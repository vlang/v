module types

import os
import v.flat
import v.parser
import v.pref

fn test_selector_call_info_follows_same_size_smartcast_changes() {
	path := os.join_path(os.vtmp_dir(), 'selector_call_memo_${os.getpid()}.v')
	os.write_file(path, 'struct First { callback fn (int) int }
struct Second { callback fn (string) string }
fn probe(x First) { x.callback(1) }
') or { panic(err) }
	defer { os.rm(path) or {} }
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut call_id := flat.empty_node
	for i, node in a.nodes {
		if node.kind == .call {
			call_id = flat.NodeId(i)
		}
	}
	assert call_id != flat.empty_node
	for memoized in [false, true] {
		mut tc := TypeChecker.new(a)
		tc.collect(a)
		tc.cur_module = 'main'
		tc.cur_file = path
		tc.memo_call_info = memoized
		tc.arm_body_resolve_memo(0, a.nodes.len - 1)
		first := *tc.parse_type(flat.source_type_id('First'))
		second := *tc.parse_type(flat.source_type_id('Second'))
		tc.cur_scope.insert('x', first)
		for typ in [first, second, first] {
			tc.smartcasts['x'] = typ
			assert tc.smartcasts.len == 1
			call := *a.node(call_id)
			assert tc.call_has_smartcast_receiver(call)
			info := tc.resolve_call_info(call_id, call) or { panic('missing callback signature') }
			expected := if typ == first { builtin_int_type } else { builtin_string_type }
			assert info.params_known
			assert info.params == [expected]
			assert info.return_type == expected
		}
	}
}
