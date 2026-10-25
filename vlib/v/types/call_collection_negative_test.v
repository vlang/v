module types

import os
import v.parser
import v.pref

fn test_collection_call_negative_guards_preserve_diagnostics_and_annotations() {
	cases := [
		'fn main() { mut a := [1, 2]; a.insert(1, 3); a.prepend(0); _ = a.clone(); _ = a.contains(2); a.clear() }',
		'fn main() { mut a := [[1]]; a.insert(0, []); a.prepend([]); _ = a.map(it.len) }',
		'fn main() { mut a := [1, 2]!; _ = a.clone(); _ = a.first(); _ = a.contains(1) }',
		'fn main() { mut m := {"a": 1}; _ = m.clone(); _ = m.keys(); _ = m.values(); _ = m.move() }',
		'struct Cell {}\nfn (c Cell) insert(value string) {}\nfn (c Cell) prepend(value string) {}\nfn (c Cell) keys(value int) {}\nfn main() { c := Cell{}; c.insert("ok"); c.prepend("ok"); c.keys(1) }',
		'type Values = []int\nfn (v Values) insert(value string) {}\nfn main() { mut a := Values([1]); a.insert("ok"); a.prepend(2); _ = a.clone() }',
		'type Values = map[string]int\nfn (v Values) keys(value int) int { return value }\nfn main() { a := Values({"a": 1}); _ = a.keys(1); _ = a.values() }',
		'struct Cell { apply fn (int) int }\nfn identity(x int) int { return x }\nfn main() { c := Cell{apply: identity}; _ = c.apply(1) }',
		'fn main() { mut a := [1]; a.insert("bad", "bad"); a.prepend("bad"); _ = a.clone(1); mut m := {"a": 1}; _ = m.keys(1) }',
		'struct Cell[T] { value T }\nfn (c Cell[T]) get(other T) T { return other }\nfn main() { c := Cell[int]{1}; _ = c.get(2) }',
	]
	for index, source in cases {
		for scoped in [false, true] {
			path := os.join_path(os.vtmp_dir(), 'collection_negative_${index}_${os.getpid()}.v')
			os.write_file(path, 'module main\n' + source + '\n')!
			mut p := parser.Parser.new(pref.new_preferences())
			a := p.parse_file(path)
			assert p.diagnostics.len == 0, '${index}: ${p.diagnostics}'
			mut tc := TypeChecker.new(a)
			tc.scope_parallel_check_workers = scoped
			tc.collect(a)
			_ = tc.check_semantics_opt(false)
			os.rm(path) or {}
			if index < 5 {
				assert tc.errors.len == 0, '${index}: ${tc.errors}'
			}
			println('COLLECTION_CASE:${index}:${scoped}:${tc.errors.len}:${tc.notices.len}')
			for err in tc.errors {
				println('COLLECTION_ERROR:${err.kind}:${int(err.node)}:${err.pos.offset}:${err.pos.end}:${err.msg}:${err.details.join('|')}')
			}
			for notice in tc.notices {
				println('COLLECTION_NOTICE:${notice.kind}:${int(notice.node)}:${notice.pos.offset}:${notice.pos.end}:${notice.msg}:${notice.details.join('|')}')
			}
			for id, node in a.nodes {
				if node.kind != .call { continue }
				for arg_index in 1 .. node.children_count {
					typ := tc.builtin_array_annotation_expected_type(node, arg_index)
					println('COLLECTION_EXPECTED:${index}:${scoped}:${id}:${arg_index}:${typ.name()}')
				}
			}
		}
	}
}
