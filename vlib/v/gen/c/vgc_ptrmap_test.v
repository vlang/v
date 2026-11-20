module main

import os

fn test_vgc_struct_literal_emits_ptrmap() {
	root := os.join_path(os.vtmp_dir(), 'vgc_ptrmap_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'input.v')
	c_path := os.join_path(root, 'output.c')
	os.write_file(path, "struct Child { v int }
struct Holder {
	name  string
	child &Child
	value int
}
fn main() {
	h := &Holder{ name: 'x', child: &Child{ v: 1 }, value: 2 }
	println(h.value)
}
")!
	shared_v3 := os.getenv('V3_TEST_UNIT_SHARED_V3')
	mut args := if shared_v3 != '' { [shared_v3] } else { [@VEXE, '-new-compiler'] }
	args << ['-gc', 'vgc', '-o', c_path, path]
	compiled := os.exec(args)
	assert compiled.exit_code == 0, compiled.output
	generated := os.read_file(c_path)!
	assert generated.contains('vgc_memdup_typed(&(main__Holder){'), generated
	assert generated.contains('offsetof(main__Holder, name)'), generated
	assert generated.contains('offsetof(main__Holder, child)'), generated
	assert generated.contains('(u8)2)'), generated
}
