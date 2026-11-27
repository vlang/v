import os

fn test_imported_enum_defaults_in_fixed_arrays_with_local_name_collisions() {
	root := os.join_path(os.temp_dir(), 'v3_imported_enum_array_${os.getpid()}')
	dep_dir := os.join_path(root, 'dep')
	main_path := os.join_path(root, 'main.v')
	output_path := os.join_path(root, 'program')
	defer {
		os.rmdir_all(root) or {}
	}
	os.mkdir_all(dep_dir) or { panic(err) }
	os.write_file(os.join_path(dep_dir, 'dep.v'), 'module dep
pub enum Kind {
	first = 5
	second = 11
}
pub struct Item {
pub mut:
	kind Kind = Kind.first
}
pub type KindAlias = Kind
pub struct AliasItem {
pub mut:
	kind KindAlias = KindAlias.second
}
') or { panic(err) }
	os.write_file(main_path, 'module main
import dep
enum Kind {
	first = 41
	second = 73
}
type KindAlias = Kind
fn main() {
	assert int(Kind.first) == 41
	items := [2]dep.Item{}
	assert items[0].kind == dep.Kind.first
	assert items[1].kind == dep.Kind.first
	assert int(items[0].kind) == 5
	assert int(items[1].kind) == 5

	assert int(KindAlias.second) == 73
	alias_items := [2]dep.AliasItem{}
	assert alias_items[0].kind == dep.KindAlias.second
	assert alias_items[1].kind == dep.KindAlias.second
	assert int(alias_items[0].kind) == 11
	assert int(alias_items[1].kind) == 11
}
') or { panic(err) }
	compile := os.exec([@VEXE, '-path', '${root}' + '|@vlib|@vmodules', '-o', output_path, main_path])
	assert compile.exit_code == 0, compile.output
	run := os.exec([output_path])
	assert run.exit_code == 0, run.output
}
