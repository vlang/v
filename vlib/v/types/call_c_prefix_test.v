module types

import os
import v.flat
import v.parser
import v.pref

fn test_c_call_prefix_guard_preserves_external_and_ordinary_calls() {
	cases := [
		'fn C.abs(value int) int\nfn main() { _ = C.abs(1) }',
		'fn main() { C.missing(1) }',
		'fn accept(a int, b int, c string) {}\nfn main() { accept(1, 2, "ok") }',
		'fn pair() (int, string) { return 1, "ok" }\nfn accept(a int, b string) {}\nfn main() { accept(pair()) }',
		'fn pair() (int, int) { return 1, 2 }\nfn accept(a string, b int, c int) {}\nfn main() { accept("ok", pair()) }',
		'fn accept(a int, values ...int) {}\nfn main() { values := [2, 3]; accept(1, 2, 3); accept(1, ...values) }',
		'fn pair() (int, int) { return 1, 2 }\nfn accept(values ...int) {}\nfn main() { accept(pair()) }',
		'fn main() { println([1, 2]); eprintln("ok"); print(1) }',
		'fn accept(a []int, b []string) {}\nfn main() { accept([], []); accept(if true { [1] } else { [] }, []) }',
		'fn accept(a fn (int) int, b []int) {}\nfn main() { accept(fn (v int) int { return v + 1 }, []) }',
		'struct Box[T] { value T }\nfn accept(a Box[int], b int) {}\nfn main() { accept(Box{value: 1}, 2) }',
		'@[params]\nstruct Options { value int }\nfn accept(a int, options Options) {}\nfn main() { accept(1, value: 2); accept(1) }',
		'fn accept(a int, b ?int) {}\nfn main() { accept(1); accept(1, none) }',
		'fn main() { mut a := [[1]]; a.insert(0, []); a.prepend([]); _ = a.map(it.len) }',
		'fn accept(a int, b int) {}\nfn main() { accept(...[1, 2]) }',
		'fn pair() (int, int) { return 1, 2 }\nfn accept(a int, b int) {}\nfn main() { accept(pair(), 3); accept(1, "bad"); accept() }',
		'fn generic[T](a T, b []T) {}\nfn main() { generic(1, []); generic[int](2, [3]) }',
		'struct Holder { callback fn (int, []int) }\nfn accept(a int, b []int) {}\nfn main() { h := Holder{callback: accept}; h.callback(1, []) }',
		'fn empty[T]() {}\nfn main() { empty(); empty[int](); }',
	]
	for index, source in cases {
		for scoped in [false, true] {
			for trusted in [false, true] {
				path := os.join_path(os.vtmp_dir(), 'call_c_prefix_${index}.v')
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
				expected_errors := [0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 3, 1, 0, 1]
				assert tc.errors.len == expected_errors[index], '${index}: ${tc.errors}'
				println('CQUERY_CASE:${index}:${scoped}:${trusted}:${tc.errors.len}:${tc.notices.len}')
				for err in tc.errors {
					println('CQUERY_ERROR:${err.kind}:${int(err.node)}:${err.pos.offset}:${err.pos.end}:${err.msg}:${err.details.join('|')}')
				}
				for notice in tc.notices {
					println('CQUERY_NOTICE:${notice.kind}:${int(notice.node)}:${notice.pos.offset}:${notice.pos.end}:${notice.msg}:${notice.details.join('|')}')
				}
				for id, node in a.nodes {
					if typ := tc.expr_type(flat.NodeId(id)) {
						println('CQUERY_TYPE:${index}:${scoped}:${trusted}:${id}:${node.kind}:${typ.name()}')
					}
					if node.typ != 0 {
						println('CQUERY_TEXT:${index}:${scoped}:${trusted}:${id}:${flat.source_type_name(node.typ)}')
					}
					if node.kind == .call {
						println('CQUERY_CALL:${index}:${scoped}:${trusted}:${id}:${tc.cached_resolved_call(flat.NodeId(id)) or { '' }}')
					}
				}
			}
		}
	}
}
