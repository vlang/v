import os

fn test_foreign_generic_arguments_keep_their_declaring_module() {
	root := os.join_path(os.vtmp_dir(), 'foreign_generic_identity_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'ck'))!
	os.mkdir_all(os.join_path(root, 'rt'))!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'foreign_generic_identity' }")!
	os.write_file(os.join_path(root, 'main.v'), 'import ck
fn main() { ck.check() }
')!
	os.write_file(os.join_path(root, 'rt', 'rt.v'), 'module rt
pub struct Type { other int }
@[heap]
pub struct Cell[T] { pub mut: value T }
')!
	os.write_file(os.join_path(root, 'ck', 'ck.v'), 'module ck
import rt
pub struct Type { pub: name string }
pub fn check() {
    empty := []&Type{cap: 2}
    empty_cell := &rt.Cell[[]&Type]{value: empty}
    assert empty_cell.value.cap == 2
    assert typeof(empty_cell.value).name == "[]&ck.Type"
    item := &Type{name: "caller"}
    values := [item]
    cell := &rt.Cell[[]&Type]{value: values}
    assert cell.value.len == 1
    assert cell.value[0].name == "caller"
    assert cell.value[0] == item
    mapping := rt.Cell[map[string]&Type]{value: {"key": item}}
    assert mapping.value["key"]!.name == "caller"
}
')!
	result := os.exec([@VEXE, '-cc', 'clang', '-gc', 'none', '-no-retry-compilation', 'run',
		os.join_path(root, 'main.v')])
	assert result.exit_code == 0, result.output
}
