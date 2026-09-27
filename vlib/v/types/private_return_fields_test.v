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

fn test_private_method_value_on_private_return_type_is_rejected() {
	root := os.join_path(os.vtmp_dir(), 'v3_private_return_method_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'opaque'))!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'opaque', 'opaque.v'), 'module opaque
struct Record {}
pub fn make_record() Record { return Record{} }
fn (r Record) private_method() int { return 1 }
')!
	path := os.join_path(root, 'main.v')
	os.write_file(path, 'import opaque
fn main() {
 callback := opaque.make_record().private_method
 println(callback())
}
')!
	result := os.execute('${os.quoted_path(@VEXE)} -check ${os.quoted_path(path)}')
	assert result.exit_code != 0, result.output
	assert result.output.contains('Record.private_method` is private'), result.output
}

fn test_private_generic_method_value_on_private_return_type_is_rejected() {
	root := os.join_path(os.vtmp_dir(), 'v3_private_generic_return_method_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'opaque'))!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'opaque', 'opaque.v'), 'module opaque
struct Record[T] { value T }
pub fn make_record[T](value T) Record[T] { return Record[T]{value: value} }
fn (r Record[T]) private_method() T { return r.value }
')!
	path := os.join_path(root, 'main.v')
	os.write_file(path, 'import opaque\nfn main() {\n callback := opaque.make_record[int](1).private_method\n println(callback())\n}\n')!
	result := os.execute('${os.quoted_path(@VEXE)} -check ${os.quoted_path(path)}')
	assert result.exit_code != 0, result.output
	assert result.output.contains('Record[int].private_method` is private'), result.output
}

fn test_private_method_value_through_double_pointer_is_rejected() {
	root := os.join_path(os.vtmp_dir(), 'v3_private_double_pointer_method_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'opaque'))!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'opaque', 'opaque.v'), 'module opaque\nstruct Record {}\npub fn make_record() Record { return Record{} }\nfn (r Record) private_method() int { return 1 }\n')!
	path := os.join_path(root, 'main.v')
	os.write_file(path, 'import opaque\nfn main() {\n record := opaque.make_record()\n p := &record\n pp := &p\n callback := pp.private_method\n println(callback())\n}\n')!
	result := os.execute('${os.quoted_path(@VEXE)} -check ${os.quoted_path(path)}')
	assert result.exit_code != 0, result.output
	assert result.output.contains('Record.private_method` is private'), result.output
}

fn test_private_embedded_method_on_private_return_type_is_rejected() {
	root := os.join_path(os.vtmp_dir(), 'v3_private_embedded_method_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'opaque'))!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'opaque', 'opaque.v'), 'module opaque\nstruct Inner {}\nfn (i Inner) private_method() int { return 1 }\nstruct Outer { Inner }\npub fn make_outer() Outer { return Outer{} }\n')!
	path := os.join_path(root, 'main.v')
	os.write_file(path, 'import opaque\nfn main() { println(opaque.make_outer().private_method()) }\n')!
	result := os.execute('${os.quoted_path(@VEXE)} -check ${os.quoted_path(path)}')
	assert result.exit_code != 0, result.output
	assert result.output.contains('private_method` is private'), result.output
}

fn test_direct_private_method_wins_over_public_embedded_namesake() {
	root := os.join_path(os.vtmp_dir(), 'v3_private_direct_method_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'opaque'))!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'opaque', 'opaque.v'), 'module opaque\npub struct Inner {}\npub fn (i Inner) value() int { return 1 }\nstruct Outer { Inner }\nfn (o Outer) value() int { return 2 }\npub fn make_outer() Outer { return Outer{} }\n')!
	path := os.join_path(root, 'main.v')
	os.write_file(path, 'import opaque\nfn main() { println(opaque.make_outer().value()) }\n')!
	result := os.execute('${os.quoted_path(@VEXE)} -check ${os.quoted_path(path)}')
	assert result.exit_code != 0, result.output
	assert result.output.contains('value` is private'), result.output
}

fn test_alias_private_method_wins_over_public_embedded_namesake() {
	root := os.join_path(os.vtmp_dir(), 'v3_private_alias_method_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'opaque'))!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'opaque', 'opaque.v'), 'module opaque\npub struct Inner {}\npub fn (i Inner) value() int { return 1 }\npub struct Outer { Inner }\npub type Alias = Outer\nfn (a Alias) value() int { return 2 }\npub fn make_alias() Alias { return Alias(Outer{}) }\n')!
	path := os.join_path(root, 'main.v')
	os.write_file(path, 'import opaque\nfn main() { println(opaque.make_alias().value()) }\n')!
	result := os.execute('${os.quoted_path(@VEXE)} -new-compiler -check ${os.quoted_path(path)}')
	assert result.exit_code != 0, result.output
	assert result.output.contains('value` is private'), result.output
}

fn test_private_generic_method_wins_over_public_embedded_namesake() {
	root := os.join_path(os.vtmp_dir(), 'v3_private_generic_direct_method_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'opaque'))!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'opaque', 'opaque.v'), 'module opaque\npub struct Inner {}\npub fn (i Inner) value() int { return 1 }\nstruct Outer[T] {\n Inner\n item T\n}\nfn (o Outer[T]) value() T { return o.item }\npub fn make_outer[T](item T) Outer[T] { return Outer[T]{item: item} }\n')!
	path := os.join_path(root, 'main.v')
	os.write_file(path, 'import opaque\nfn main() { println(opaque.make_outer[int](2).value()) }\n')!
	result := os.execute('${os.quoted_path(@VEXE)} -new-compiler -check ${os.quoted_path(path)}')
	assert result.exit_code != 0, result.output
	assert result.output.contains('value` is private'), result.output
}
