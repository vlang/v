module types

import os

fn test_private_return_type_still_hides_private_fields_and_its_name() {
	root := os.join_path(os.vtmp_dir(), 'v3_private_return_fields_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'opaque'))!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'opaque', 'opaque.v'), 'module opaque
struct Record {
 secret int
pub:
 value int
}
pub fn make_record() Record { return Record{secret: 1, value: 2} }
')!
	path := os.join_path(root, 'main.v')
	os.write_file(path, 'import opaque
fn main() {
 value := opaque.make_record()
 println(value.secret)
 _ := opaque.Record{}
}
')!
	result := os.execute('${os.quoted_path(@VEXE)} -check ${os.quoted_path(path)}')
	assert result.exit_code != 0, result.output
	assert result.output.contains('private'), result.output
	assert result.output.contains('value.secret'), result.output
	assert result.output.contains('opaque.Record'), result.output
}
