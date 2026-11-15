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

fn test_loop_callback_fields_do_not_retain_same_named_functions() ! {
	root := os.join_path(os.vtmp_dir(), 'callback_field_roots_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	main_path := os.join_path(root, 'main.v')
	worker_path := os.join_path(root, 'workers.v')
	driver_path := os.join_path(root, 'driver.v')
	os.write_file(driver_path, 'module driver
pub const flag = 1
pub fn run() { unused_helper() }
fn unused_helper() {}
')!
	os.write_file(main_path, 'import workers
import driver
fn actual_callback() {}
fn main() {
	mut pool := workers.Pool{}
	pool.run([workers.Task{run: actual_callback}])
	_ := driver.flag
}
')!
	for body in ['task.run()', 'callback := task.run; callback()'] {
		os.write_file(worker_path, 'module workers
pub struct Task { pub: run fn () }
pub struct Pool {}
pub fn (mut pool Pool) run(tasks []Task) {
	for task in tasks { ${body} }
}
')!
		mut p := parser.Parser.new(pref.new_preferences())
		a := p.parse_files([main_path, worker_path, driver_path])
		assert p.diagnostics.len == 0, p.diagnostics.str()
		mut tc := types.TypeChecker.new(a)
		tc.collect(a)
		tc.check_semantics()
		assert tc.errors.len == 0, tc.errors.str()
		used := mark_used(a, &tc)
		assert used['workers.Pool.run']
		assert used['actual_callback']
		assert !used['driver.run'], body
		assert !used['driver.unused_helper'], body
	}
}

fn test_compiler_interface_helpers_follow_calls_in_both_decl_scans() ! {
	root := os.join_path(os.vtmp_dir(), 'interface_helper_roots_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'gen', 'c'))!
	defer { os.rmdir_all(root) or {} }
	main_path := os.join_path(root, 'main.v')
	interface_path := os.join_path(root, 'gen', 'c', 'interface.v')
	os.write_file(interface_path, 'module c
pub const flag = 1
pub struct FlatGen {}
pub fn (g FlatGen) emit_interface() int { return interface_helper() }
fn interface_helper() int { return 42 }
')!
	for reached in [false, true] {
		body := if reached { '_ := c.FlatGen{}.emit_interface()' } else { '_ := c.flag' }
		os.write_file(main_path, 'import c\nfn main() { ${body} }\n')!
		mut p := parser.Parser.new(pref.new_preferences())
		a := p.parse_files([main_path, interface_path])
		assert p.diagnostics.len == 0, p.diagnostics.str()
		mut tc := types.TypeChecker.new(a)
		tc.collect(a)
		tc.check_semantics()
		assert tc.errors.len == 0, tc.errors.str()
		// Keep the real source location explicit for both declaration indexes.
		tc.fn_type_files['c.FlatGen.emit_interface'] = interface_path
		tc.fn_type_files['c.interface_helper'] = interface_path
		used := mark_used(a, &tc)
		assert used['c.FlatGen.emit_interface'] == reached
		assert used['c.interface_helper'] == reached
		mut prepared := prepare_markused_declarations(a, &tc, true)
		prepared_used := mark_used_without_generic_detection_prepared(a, &tc, mut prepared)
		assert prepared_used['c.FlatGen.emit_interface'] == reached
		assert prepared_used['c.interface_helper'] == reached
		prepared.release()
	}
}
