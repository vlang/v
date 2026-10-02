module types

import os
import v.parser
import v.pref

struct ReachabilityCalleeCase {
	name     string
	source   string
	expected []string
}

fn test_resolved_call_callees_preserve_selected_function_reachability() {
	cases := [
		ReachabilityCalleeCase{
			name:     'direct'
			source:   'import dep\nfn main() { dep.direct() }'
			expected: ['dep.direct']
		},
		ReachabilityCalleeCase{
			name:     'local_receiver'
			source:   'import dep\nfn main() { item := dep.Item{}; _ := item.method() }'
			expected: ['dep.Item.method', 'dep.inner']
		},
		ReachabilityCalleeCase{
			name:     'callback'
			source:   'import dep\nfn apply(cb fn () int) int { return cb() }\nfn main() { _ := apply(dep.callback) }'
			expected: ['apply', 'dep.callback', 'dep.direct']
		},
		ReachabilityCalleeCase{
			name:     'module_callback'
			source:   'import dep\nfn main() { _ := dep.consume(dep.callback) }'
			expected: ['dep.callback', 'dep.consume', 'dep.direct']
		},
		ReachabilityCalleeCase{
			name:     'bound_method'
			source:   'import dep\nfn apply(cb fn () int) int { return cb() }\nfn main() { item := dep.Item{}; _ := apply(item.method) }'
			expected: ['apply', 'dep.Item.method', 'dep.inner']
		},
		ReachabilityCalleeCase{
			name:     'nested_receiver'
			source:   'import dep\nfn main() { _ := dep.make().method() }'
			expected: ['dep.Item.method', 'dep.direct', 'dep.inner', 'dep.make']
		},
		ReachabilityCalleeCase{
			name:     'local_callback'
			source:   'import dep\nfn main() { callback := dep.callback; _ := callback() }'
			expected: ['dep.callback', 'dep.direct']
		},
		ReachabilityCalleeCase{
			name:   'shadowed_import'
			source: 'import dep { callback }\nfn main() { callback := fn () int { return 2 }; _ := callback() }'
		},
		ReachabilityCalleeCase{
			name:     'shadowed_parameter'
			source:   'import dep { callback }\nfn apply(callback fn () int) int { return callback() }\nfn main() { _ := apply(fn () int { return 2 }) }'
			expected: ['apply']
		},
		ReachabilityCalleeCase{
			name:     'fn_field'
			source:   'import dep\nstruct Holder { callback fn () int }\nfn main() { holder := Holder{ callback: dep.callback }; _ := holder.callback() }'
			expected: ['dep.callback', 'dep.direct']
		},
		ReachabilityCalleeCase{
			name:     'unresolved_call'
			source:   'import dep\nfn main() { unknown(dep.callback) }'
			expected: ['dep.callback', 'dep.direct']
		},
	]
	root := os.join_path(os.vtmp_dir(), 'v3_reachability_callee_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	dep := os.join_path(root, 'dep.v')
	main := os.join_path(root, 'main.v')
	os.write_file(dep, 'module dep
pub struct Item { value int }
pub fn direct() {}
pub fn inner() {}
pub fn make() Item { direct(); return Item{} }
pub fn (item Item) method() int { inner(); return item.value }
pub fn callback() int { direct(); return 1 }
pub fn consume(cb fn () int) int { return cb() }
pub fn unused() {}
')!
	for case in cases {
		os.write_file(main, 'module main\n${case.source}\n')!
		mut p := parser.Parser.new(pref.new_preferences())
		a := p.parse_files([dep, main])
		assert p.diagnostics.len == 0, '${case.name}: ${p.diagnostics}'
		mut tc := TypeChecker.new(a)
		tc.diagnostic_files[main] = true
		tc.collect(a)
		tc.collect_selected_file_called_fns()
		mut actual := tc.selected_file_called_fns.keys()
		actual.sort()
		assert actual == case.expected, '${case.name}: ${actual}'
	}
}
