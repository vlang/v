module types

import os
import v.parser
import v.pref

fn test_integer_queries_preserve_call_types_and_diagnostics() {
	cases := [
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
		'type Count = int\nfn accept(a Count, b u8, c i64, d rune) {}\nfn main() { accept(Count(2), 3, 4, `a`) }',
		'const amount = 3\nconst negative = -1\nfn accept(a u8, b u16, c int) {}\nfn main() { accept(amount, 4, negative); accept(-1, 2, 3); accept(negative, 2, 3) }',
		'struct Item { value int }\nfn accept(a u32, b uint) {}\nfn main() { size := sizeof(Item); accept(size, size); accept(300, 4) }',
		'fn text() string { return "ok" }\nfn ints() []int { return [1, 2] }\nfn accept(a string, b []int, c map[string]int, d f64, e bool) {}\nfn main() { accept(text(), ints(), {"key": 1}, 2, true) }',
		'struct Item { value int }\ninterface Named { value int }\ntype Choice = Item | string\nfn accept(a Item, b Named, c Choice, d ?int) {}\nfn main() { item := Item{value: 2}; accept(item, item, item, none) }',
		'fn accept(a fn (int) int, b &int, c voidptr) {}\nfn identity(value int) int { return value }\nfn main() { value := 1; accept(identity, &value, unsafe { nil }) }',
		'fn accept(a f32, b f64) {}\nfn main() { accept(1, 2); accept(3.5, 4.5) }',
		'fn accept(a u8, b i8) {}\nfn main() { accept(256, 128); accept(-1, -129) }',
	]
	for index, source in cases {
		for scoped in [false, true] {
			for trusted in [false, true] {
				path := os.join_path(os.vtmp_dir(), 'call_integer_query_${index}.v')
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
				expected_errors := [0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 3, 1, 0, 1, 0, 2, 0,
					0, 0, 0, 0, 1]
				assert tc.errors.len == expected_errors[index], '${index}: ${tc.errors}'
			}
		}
	}
}
