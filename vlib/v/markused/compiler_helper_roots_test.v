module markused

import os
import v.parser
import v.pref
import v.types

fn test_compiler_helpers_follow_direct_calls_and_worker_function_values() ! {
	root := os.join_path(os.vtmp_dir(), 'compiler_helper_roots_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	for spec in [['c', 'FlatGen', 'gen_fn_items_scoped_batches'],
		['markused', 'CallCollector', 'collect_bodies_scoped_batches'],
		['parser', 'Parser', 'precollect_parallel_comptime_consts'],
		['types', 'TypeChecker', 'check_scoped_batches'],
		['types', 'TypeChecker', 'result_return_uses_multi_tail']] {
		mod_name, receiver, method := spec[0], spec[1], spec[2]
		dependency := os.join_path(root, '${mod_name}.v')
		os.write_file(dependency, 'module ${mod_name}
pub struct ${receiver} {}
pub fn (value ${receiver}) ${method}() int { return helper() }
pub fn gen_expr_lvalue() int { return gen_assign() }
fn gen_assign() int { return helper() }
fn helper() int { return 42 }
')!
		for reached in [false, true] {
			body := if reached {
				'item := ${mod_name}.${receiver}{}\ncallback := item.${method}\n_ := callback()\n_ := ${mod_name}.gen_expr_lvalue()'
			} else {
				'_ := ${mod_name}.${receiver}{}'
			}
			main_path := os.join_path(root, 'main.v')
			os.write_file(main_path, 'module main\nimport ${mod_name}\nfn main() { ${body} }\n')!
			mut p := parser.Parser.new(pref.new_preferences())
			a := p.parse_files([main_path, dependency])
			assert p.diagnostics.len == 0, p.diagnostics.str()
			mut tc := types.TypeChecker.new(a)
			tc.collect(a)
			tc.check_semantics()
			assert tc.errors.len == 0, tc.errors.str()
			used := mark_used(a, &tc)
			for name in ['${receiver}.${method}', 'gen_expr_lvalue', 'gen_assign', 'helper'] {
				assert used['${mod_name}.${name}'] == reached, '${mod_name}.${name}: ${reached}'
			}
		}
	}
}
