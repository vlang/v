import os

fn test_generic_struct_literal_with_qualified_type_argument() {
	root := os.join_path(os.vtmp_dir(), 'v3_generic_struct_literal_qualified_type_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(os.join_path(root, 'record')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'record', 'record.v'), 'module record

pub struct Collection {}
') or { panic(err) }
	os.write_file(os.join_path(root, 'main.v'), 'module main

import record

struct List[T] {
	items []T
}

fn collection_list() !List[record.Collection] {
	return List[record.Collection]{}
}

fn main() {
	list := collection_list()!
	assert list.items.len == 0
}
') or { panic(err) }
	result := os.execute('${os.quoted_path(@VEXE)} -new-compiler -nocache run ${os.quoted_path(root)}')
	assert result.exit_code == 0, result.output
}
