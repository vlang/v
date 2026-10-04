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
	result := os.exec([@VEXE, '-check', path])
	assert result.exit_code != 0, result.output
	assert result.output.contains('private'), result.output
	assert result.output.contains('value.secret'), result.output
	assert result.output.contains('opaque.Record'), result.output
}

fn test_private_return_type_requires_explicit_str() {
	root := os.join_path(os.vtmp_dir(), 'v3_private_return_str_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'opaque'))!
	defer { os.rmdir_all(root) or {} }
	module_path := os.join_path(root, 'opaque', 'opaque.v')
	os.write_file(module_path, 'module opaque\nstruct Record { secret int }\npub fn make_record() Record { return Record{secret: 7} }\n')!
	path := os.join_path(root, 'main.v')
	os.write_file(path, 'import opaque\nfn main() { println(opaque.make_record().str()) }\n')!
	result := os.exec([@VEXE, '-new-compiler', '-check', path])
	assert result.exit_code != 0, result.output
	assert result.output.contains('cannot stringify private type'), result.output
	os.write_file(module_path, 'module opaque\nstruct Record { secret int }\npub fn make_record() Record { return Record{secret: 7} }\nfn (r Record) str() string { return "record" }\n')!
	result_with_private_method := os.exec([@VEXE, '-new-compiler', '-check', path])
	assert result_with_private_method.exit_code != 0, result_with_private_method.output
	assert result_with_private_method.output.contains('str` is private'), result_with_private_method.output
	os.write_file(module_path, 'module opaque\nstruct Record { secret int }\npub fn make_record() Record { return Record{secret: 7} }\npub fn (r Record) str() string { return "record" }\n')!
	result_with_method := os.exec([@VEXE, '-new-compiler', '-check', path])
	assert result_with_method.exit_code == 0, result_with_method.output
}

fn test_public_outer_implicit_str_over_embedded_private_str() {
	root := os.join_path(os.vtmp_dir(), 'v3_embedded_private_str_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'opaque'))!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'opaque', 'opaque.v'), 'module opaque\nstruct Inner {}\nfn (i Inner) str() string { return "inner" }\npub struct Outer { Inner }\n')!
	path := os.join_path(root, 'main.v')
	os.write_file(path, 'import opaque\nfn main() { println(opaque.Outer{}.str()) }\n')!
	result := os.exec([@VEXE, '-new-compiler', '-check', path])
	assert result.exit_code == 0, result.output
}

fn test_private_return_type_rejects_embedded_str_override() {
	root := os.join_path(os.vtmp_dir(), 'v3_private_embedded_str_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'opaque'))!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'opaque', 'opaque.v'), 'module opaque\nstruct Inner {}\npub fn (i Inner) str() string { return "inner" }\nstruct Outer { Inner; secret int }\npub fn make_outer() Outer { return Outer{secret: 7} }\n')!
	path := os.join_path(root, 'main.v')
	os.write_file(path, 'import opaque\nfn main() { println(opaque.make_outer().str()) }\n')!
	result := os.exec([@VEXE, '-new-compiler', '-check', path])
	assert result.exit_code != 0, result.output
	assert result.output.contains('cannot stringify private type'), result.output
}

fn test_private_alias_method_value_through_double_pointer_is_rejected() {
	root := os.join_path(os.vtmp_dir(), 'v3_private_alias_double_pointer_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'opaque'))!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'opaque', 'opaque.v'), 'module opaque\npub struct Record {}\npub type Alias = Record\nfn (r Record) hidden() int { return 1 }\npub fn make_alias() Alias { return Alias(Record{}) }\n')!
	path := os.join_path(root, 'main.v')
	os.write_file(path, 'import opaque\nfn main() {\n value := opaque.make_alias()\n p := &value\n pp := &p\n callback := pp.hidden\n println(callback())\n}\n')!
	result := os.exec([@VEXE, '-new-compiler', '-check', path])
	assert result.exit_code != 0, result.output
	assert result.output.contains('hidden` is private'), result.output
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
	result := os.exec([@VEXE, '-check', path])
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
	result := os.exec([@VEXE, '-check', path])
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
	result := os.exec([@VEXE, '-check', path])
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
	result := os.exec([@VEXE, '-check', path])
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
	result := os.exec([@VEXE, '-check', path])
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
	result := os.exec([@VEXE, '-new-compiler', '-check', path])
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
	result := os.exec([@VEXE, '-new-compiler', '-check', path])
	assert result.exit_code != 0, result.output
	assert result.output.contains('value` is private'), result.output
}
