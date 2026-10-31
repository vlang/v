module types

import os
import v.flat
import v.parser
import v.pref

fn promoted_method_return_fixture() !string {
	root := os.join_path(os.vtmp_dir(), 'v3_promoted_method_return_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'model'))!
	os.mkdir_all(os.join_path(root, 'dimensions'))!
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'promoted_return' }")!
	os.write_file(os.join_path(root, 'dimensions', 'dimensions.v'), 'module dimensions
pub struct Canvas { pub: width f32 = 30 }
')!
	os.write_file(os.join_path(root, 'model', 'model.v'), 'module model
import dimensions as size
pub struct Base {}
pub fn (b Base) canvas() size.Canvas { return size.Canvas{} }
pub struct Nested { Base }
pub struct Holder[T] { pub: value T }
pub fn (h Holder[T]) get() T { return h.value }
')!
	os.write_file(os.join_path(root, 'main.v'), 'module main
import model
import dimensions
struct App { model.Nested }
struct GenericApp { model.Holder[dimensions.Canvas] }
struct WideCanvas { width f64 = 7.5 }
struct DirectApp { model.Base }
fn (a DirectApp) canvas() WideCanvas { return WideCanvas{} }
fn canvas() string { return "unrelated function" }
fn accept[T](value T) T { return value }
fn probe(a App, b GenericApp, c DirectApp) {
	assert accept[f32](f32(0.5) * a.canvas().width) == f32(15)
	assert accept[f32](b.get().width) == f32(30)
	assert accept[f64](c.canvas().width) == f64(7.5)
}
fn main() {
	probe(App{}, GenericApp{Holder: model.Holder[dimensions.Canvas]{value: dimensions.Canvas{}}}, DirectApp{})
	assert canvas() == "unrelated function"
}
')!
	return root
}

fn test_promoted_method_result_is_resolved_before_body_checking() {
	root := promoted_method_return_fixture()!
	defer { os.rmdir_all(root) or {} }
	main_path := os.join_path(root, 'main.v')
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_files([main_path, os.join_path(root, 'model', 'model.v'),
		os.join_path(root, 'dimensions', 'dimensions.v')])
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	tc.cur_file = main_path
	tc.cur_module = 'main'
	tc.push_scope()
	for name, typ in {
		'a': 'App'
		'b': 'GenericApp'
		'c': 'DirectApp'
	} {
		tc.cur_scope.insert(name, *tc.parse_type(flat.source_type_id(typ)))
	}
	mut fields := 0
	for idx, node in a.nodes {
		if node.kind != .selector || node.value != 'width' || node.children_count == 0 {
			continue
		}
		call := a.child_node(&node, 0)
		if call.kind != .call { continue }
		callee := a.child_node(call, 0)
		receiver := a.child_node(callee, 0)
		expected := if receiver.value == 'c' { 'f64' } else { 'f32' }
		assert tc.resolve_type(flat.NodeId(idx)).name() == expected, receiver.value
		fields++
	}
	assert fields == 3
	tc.pop_scope()
}

fn test_promoted_method_results_keep_native_generic_argument_widths() {
	root := promoted_method_return_fixture()!
	defer { os.rmdir_all(root) or {} }
	result := os.exec([@VEXE, '-no-retry-compilation', 'run', root])
	assert result.exit_code == 0, result.output
}
