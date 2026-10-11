module c

import os

fn test_sumtype_rejects_infinite_layout_and_mutable_variant_conversion() {
	sources := [
		'type Tree = int | Branch
struct Branch { child Tree }
fn main() {}',
		'type Tree = int | Branch
struct Branch { children [2]Tree }
fn main() {}',
		'struct Item {}
type Value = int | Item
fn update(mut value Value) {}
fn main() { mut item := Item{}
 update(mut item) }',
		'struct Item {}
type Value = int | Item
struct Holder { values []&Value }
fn main() { holder := Holder{values: [&Item{}]}
 println(holder.values.len) }',
	]
	expected := [
		'cannot be defined recursively',
		'cannot be defined recursively',
		'sum type reference requires',
		'invalid array element',
	]
	path := os.join_path(os.vtmp_dir(), 'sumtype_invalid_${os.getpid()}.v')
	defer { os.rm(path) or {} }
	for i, source in sources {
		os.write_file(path, source)!
		command := '${os.quoted_path(@VEXE)} -new-compiler -check ${os.quoted_path(path)}'
		result := os.execute(command)
		assert result.exit_code != 0, source
		assert result.output.contains(expected[i]), result.output
	}
}

fn test_sumtype_generated_construction_has_no_payload_allocation() {
	path := os.join_path(os.vtmp_dir(), 'sumtype_codegen_${os.getpid()}.v')
	output := path + '.c'
	defer {
		os.rm(path) or {}
		os.rm(output) or {}
	}
	source := 'struct Pair { x int
 y int }
type Value = int | Pair
@[noinline]
fn pack(x int) Value { return Pair{x, x + 1} }
fn main() { println(pack(3)) }'
	os.write_file(path, source)!
	command := '${os.quoted_path(@VEXE)} -new-compiler -o ${os.quoted_path(output)} ${os.quoted_path(path)}'
	result := os.execute(command)
	assert result.exit_code == 0, result.output
	generated := os.read_file(output)!
	body := generated.all_after('__attribute__((noinline)) Value pack(').all_before('\n}')
	assert body.contains('.typ = ')
	assert body.contains('.Pair = ')
	assert !body.contains('memdup')
	assert !body.contains('malloc')
	assert !body.contains('memcpy')
	assert !generated.contains('_pointer_variant_is_owned')
}

fn test_sumtype_reference_field_keeps_qualified_storage() {
	dir := os.join_path(os.vtmp_dir(), 'sumtype_reference_${os.getpid()}')
	os.mkdir_all(os.join_path(dir, 'model'))!
	defer { os.rmdir_all(dir) or {} }
	os.write_file(os.join_path(dir, 'v.mod'), "Module { name: 'sum_reference' }")!
	model := 'module model
pub type Value = int | string
pub struct Entry {
pub:
 typ &Value
}
pub fn keep(typ &Value) Entry { return Entry{typ: typ} }'
	program := 'import model
fn main() {
 value := model.Value(42)
 entry := model.keep(&value)
 assert voidptr(entry.typ) == voidptr(&value)
 assert *entry.typ == value
}'
	os.write_file(os.join_path(dir, 'model', 'model.v'), model)!
	path := os.join_path(dir, 'main.v')
	os.write_file(path, program)!
	command := '${os.quoted_path(@VEXE)} -new-compiler run ${os.quoted_path(path)}'
	result := os.execute(command)
	assert result.exit_code == 0, result.output
}
