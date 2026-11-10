module c

import os
import v.parser
import v.pref
import v.types

fn test_assoc_codegen_without_transform_preserves_spread_evaluation() {
	path := os.join_path(os.vtmp_dir(), 'assoc_codegen_${os.getpid()}.v')
	os.write_file(path, 'struct Config { width int }
struct Target { token int = unused_default() }
struct CopiedTarget {
	width int
	token int
}
fn unused_default() int { return 17 }
fn make_config() Config { return Config{width: 31} }
fn make_token() int { return 9 }
fn main() {
	_ = Target{...make_config(), token: make_token()}
	_ = CopiedTarget{...make_config(), token: make_token()}
	_ = Config{...make_config(), width: make_token()}
}
')!
	defer { os.rm(path) or {} }
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	tc.check_semantics()
	assert tc.errors.len == 0, tc.errors.str()
	mut checked := 0
	for node in a.nodes {
		if node.kind != .assoc {
			continue
		}
		for heap in [false, true] {
			mut g := FlatGen.new()
			g.a = a
			g.tc = &tc
			if heap {
				g.gen_heap_assoc_expr(node)
			} else {
				g.gen_assoc_expr(node)
			}
			generated := g.sb.str()
			assert generated.count('make_config(') == 1, generated
			assert generated.count('make_token(') == 1, generated
			source_index := generated.index('make_config(') or { panic(generated) }
			field_index := generated.index('make_token(') or { panic(generated) }
			assert source_index < field_index, generated
			assert !generated.contains('unused_default('), generated
			if node.value_text().all_after_last('.') == 'Target' {
				assert generated.contains('(void)('), generated
				assert generated.contains('){0};'), generated
			} else {
				assert !generated.contains('(void)('), generated
			}
			checked++
		}
	}
	assert checked == 6
}
