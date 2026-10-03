module c

import os

fn test_interface_bearing_union_construction_clears_all_storage() {
	root := os.join_path(os.vtmp_dir(), 'union_storage_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source_path := os.join_path(root, 'main.v')
	c_path := os.join_path(root, 'main.c')
	os.write_file(source_path, 'interface Any {}
struct Item { value int }
union Bad { f f64; a Any }
union BadBits { f f64; bad Bad }
fn make_value(number f64) Bad {
 bits := BadBits{f: number}
 return unsafe { bits.bad }
}
fn make_heap(number f64) &Bad { return &Bad{f: number} }
fn main() {
 println(make_value(2.0))
 println(make_heap(3.0))
 println(Bad{a: Item{value: 42}})
}
')!
	result := os.exec([@VEXE, '-new-compiler', '-o', c_path, source_path])
	assert result.exit_code == 0, result.output
	source := os.read_file(c_path)!
	for function in ['make_value(double number) {', 'make_heap(double number) {'] {
		body := source.all_after(function).all_before('\n}')
		assert body.contains('memset('), body
		assert body.contains(', 0, sizeof('), body
		assert body.index('memset(') or { -1 } < body.index('.f = number') or { -1 }, body
	}
}
