module types

import os
import strings
import v.flat
import v.parser
import v.pref
import v.workers

fn selected_reachability_test_body(next string) string {
	return '
 item := make()
 _ := item.method()
 stored := callback_fn
 _ := stored()
 _ := consume(item.method)
 local := fn () int { inner(); return 1 }
 _ := consume(local)
 for entry in [item] { _ := entry.method() }
 { callback_fn := fn () int { return 2 }; _ := callback_fn() }
 _ := callback()
 ${next}()
'
}

fn test_parallel_selected_function_closure_matches_serial() {
	root := os.join_path(os.vtmp_dir(), 'v3_parallel_reachability_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	dep := os.join_path(root, 'dep.v')
	main := os.join_path(root, 'main.v')
	mut dep_source := strings.new_builder(16384)
	dep_source.write_string('module dep
pub struct Item { value int }
pub struct ErrorItem {}
pub fn (item ErrorItem) error_method() { error_leaf() }
pub fn error_leaf() {}
pub fn inner() {}
pub fn make() Item { inner(); return Item{} }
pub fn (item Item) method() int { leaf(); return item.value }
pub fn callback_fn() int { leaf(); return 1 }
pub fn callback() int { unused(); return 0 }
pub fn consume(cb fn () int) int { return cb() }
pub fn seed() int { inner(); return 1 }
pub fn leaf() { cycle() }
pub fn cycle() { leaf() }
pub fn unused() {}
pub fn duplicate() { first_winner() }
pub fn duplicate() { losing_body() }
pub fn first_winner() {}
pub fn losing_body() {}
')
	mut main_source := strings.new_builder(4096)
	main_source.write_string('module main\nimport dep\nfn main() {\n')
	mut expected := ['dep.ErrorItem.error_method', 'dep.error_leaf', 'dep.Item.method',
		'dep.callback_fn', 'dep.consume', 'dep.cycle', 'dep.duplicate', 'dep.first_winner', 'dep.inner',
		'dep.leaf', 'dep.make', 'dep.seed']
	for i in 0 .. 80 {
		error_receiver := if i == 0 {
			'error_item := ErrorItem{}\n_ := error_item.error_method()\n'
		} else {
			''
		}
		dep_source.write_string('pub fn first_${i}(callback fn () int) {${error_receiver}${selected_reachability_test_body('second_${i}')}}\n')
		dep_source.write_string('pub fn second_${i}() { ${selected_reachability_test_body('duplicate').replace('_ := callback()', '')} }\n')
		main_source.write_string('dep.first_${i}(dep.seed)\n')
		expected << 'dep.first_${i}'
		expected << 'dep.second_${i}'
	}
	main_source.write_string('}\n')
	os.write_file(dep, dep_source.str())!
	os.write_file(main, main_source.str())!
	expected.sort()
	mut p := parser.Parser.new(pref.new_preferences())
	mut a := p.parse_files([dep, main])
	assert p.diagnostics.len == 0, '${p.diagnostics}'
	mut pool := workers.new(3)
	defer { pool.close() }
	a.worker_pool = pool
	mut error_receiver := flat.NodeId(-1)
	for i, node in a.nodes {
		if node.kind == .selector && node.value == 'error_method' {
			error_receiver = a.child(a.node(flat.NodeId(i)), 0)
			break
		}
	}
	assert int(error_receiver) >= 0
	for with_error in [false, true] {
		mut serial := TypeChecker.new(a)
		serial.diagnostic_files[main] = true
		serial.collect(a)
		if with_error {
			// Existing unknown-ident diagnostics make resolve_type return void.
			// The reachability fork must see the same prior diagnostic as its owner.
			serial.errors << TypeError{
				kind: .unknown_ident
				node: error_receiver
			}
		}
		serial.collect_selected_file_called_fns()
		mut serial_names := serial.selected_file_called_fns.keys()
		serial_names.sort()
		case_expected := if with_error {
			expected.filter(it !in ['dep.ErrorItem.error_method', 'dep.error_leaf'])
		} else {
			expected
		}
		assert serial_names == case_expected, '${serial_names}'
		assert serial.selected_file_worklist.len == 0
		for attempt in 0 .. 4 {
			mut parallel := TypeChecker.new(a)
			parallel.diagnostic_files[main] = true
			parallel.collect(a)
			parallel.errors = serial.errors.clone()
			if attempt % 2 == 0 {
				// Exercise the disposable walker used by scoped compiler checks too.
				parallel.set_fresh_type_cache_based_on(serial, false)
			}
			parallel.building_v_fast = true
			parallel.scope_parallel_check_workers = true
			before := pool.tasks_run()
			parallel.collect_selected_file_called_fns()
			if !checker_serial_only() {
				assert pool.tasks_run() > before
			}
			mut actual := parallel.selected_file_called_fns.keys()
			actual.sort()
			assert actual == serial_names, '${actual}'
			assert parallel.selected_file_worklist.len == 0
			assert parallel.errors.len == serial.errors.len
		}
	}
}
