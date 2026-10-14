module parser

import os
import v.pref

fn test_parenthesized_multiline_condition_keeps_outer_infix() {
	conditions := [
		'(one == 1 // one\n || two == 2) // two\n && three == 3',
		'(\n one == 1 // one\n || two == 2\n) // two\n && three == 3',
		'(\n one == 1\n || two == 2\n)\n && three == 3',
		'(\n one == 1\n || two == 2 // before close\n)\n && three == 3',
	]
	path := os.join_path(os.vtmp_dir(), 'parenthesized_multiline_${os.getpid()}.v')
	defer { os.rm(path) or {} }
	for condition in conditions {
		os.write_file(path, 'fn f(one int, two int, three int) { if ${condition} { println(1) } }\n')!
		for is_fmt in [false, true] {
			mut prefs := pref.new_preferences()
			prefs.is_fmt = is_fmt
			mut p := Parser.new(prefs)
			a := p.parse_file(path)
			assert p.diagnostics.len == 0, '${condition}: ${p.diagnostics}'
			mut if_count := 0
			for node in a.nodes {
				if node.kind != .if_expr {
					continue
				}
				if_count++
				cond := a.node(a.children[node.children_start])
				assert cond.kind == .infix
				assert cond.op == .logical_and
				left := a.node(a.children[cond.children_start])
				assert left.kind == .paren
				inner := a.node(a.children[left.children_start])
				assert inner.kind == .infix
				assert inner.op == .logical_or
			}
			assert if_count == 1
		}
	}
}
