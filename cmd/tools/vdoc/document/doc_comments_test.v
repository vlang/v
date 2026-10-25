module document

import os

fn test_a_trailing_comment_is_not_the_doc_comment_of_the_next_declaration() {
	dir := os.join_path(os.vtmp_dir(), 'vdoc_trailing_comments_${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir)!
	defer { os.rmdir_all(dir) or {} }
	path := os.join_path(dir, 'sizes.v')
	os.write_file(path, 'module sizes // the sizes module\n\n// min_size is the smallest size.\npub const min_size = 5 // 5 MiB\npub const max_size = 50 // 50 MiB\n\n// big is big.\npub fn big() {} // trailing\npub fn small() {}\n')!
	d := generate(dir, true, true, .auto)!
	consts := d.contents['Constants']!.children
	assert consts.map(it.merge_comments().trim_space()) == [
		'min_size is the smallest size.',
		'',
	]
	assert d.contents['big']!.merge_comments().trim_space() == 'big is big.'
	assert d.contents['small']!.comments == []
	assert !d.head.merge_comments().contains('the sizes module')
}

fn test_follows_code() {
	source := 'const a = 1 // one\n\t// two\n'
	assert follows_code(source, source.index('// one') or { 0 })
	assert !follows_code(source, source.index('// two') or { 0 })
	assert !follows_code('// first', 0)
}

fn test_a_type_declared_after_its_methods_is_documented() {
	dir := os.join_path(os.vtmp_dir(), 'vdoc_type_after_methods_${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir)!
	defer { os.rmdir_all(dir) or {} }
	os.write_file(os.join_path(dir, 'a_methods.v'), 'module any\n\npub fn (a Any) str() string {\n\treturn ""\n}\n')!
	os.write_file(os.join_path(dir, 'b_types.v'), 'module any\n\n// Any is any value.\npub type Any = int | string\n')!
	d := generate(dir, true, true, .auto)!
	any := d.contents['Any']!
	assert any.content == 'pub type Any = int | string'
	assert any.merge_comments().trim_space() == 'Any is any value.'
	assert any.children.map(it.name) == ['str']
}
