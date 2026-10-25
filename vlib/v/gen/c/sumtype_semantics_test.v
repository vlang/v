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
	body := sumtype_test_function_body(generated, 'pack')
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

fn sumtype_test_function_body(generated string, name string) string {
	for line in generated.split_into_lines() {
		if line.contains('${name}(') && line.ends_with(' {') {
			return generated.all_after(line).all_before('\n}')
		}
	}
	assert false, 'missing generated function ${name}'
	return ''
}

fn test_sumtype_pointer_variants_keep_references_and_escaping_local_storage() {
	dir := os.join_path(os.vtmp_dir(), 'sumtype_pointer_storage_${os.getpid()}')
	os.mkdir_all(dir)!
	defer { os.rmdir_all(dir) or {} }
	path := os.join_path(dir, 'main.v')
	output := os.join_path(dir, 'main.c')
	source := '@[aligned: 64]
struct Payload { mut: value int }
type PointerPayload = int | &Payload
@[noinline]
fn from_local() PointerPayload {
 local := Payload{value: 17}
 return PointerPayload(&local)
}
@[noinline]
fn from_mut(mut value Payload) PointerPayload { return PointerPayload(&value) }
fn borrow(value &Payload) &Payload { return value }
@[noinline]
fn from_borrowed(value &Payload) PointerPayload { return PointerPayload(borrow(value)) }
fn main() {
 local := from_local()
 if local is &Payload {
  assert local.value == 17
  assert uint(local) % 64 == 0
 } else { assert false }
 mut original := Payload{value: 23}
 reference := from_mut(mut original)
 original.value = 99
 if reference is &Payload {
  assert voidptr(reference) == voidptr(&original)
  assert reference.value == 99
  assert uint(reference) % 64 == 0
 } else { assert false }
 borrowed := from_borrowed(&original)
 original.value = 101
 if borrowed is &Payload {
  assert borrowed.value == 101
 } else { assert false }
}'
	os.write_file(path, source)!
	generated_result := os.exec([@VEXE, '-new-compiler', '-nocache', '-o', output, path])
	assert generated_result.exit_code == 0, generated_result.output
	generated := os.read_file(output)!
	local := sumtype_test_function_body(generated, 'from_local')
	assert local.contains('v3_aligned_memdup('), local
	assert local.contains(', 64)') || local.contains('__alignof__('), local
	for name in ['from_mut', 'from_borrowed'] {
		body := sumtype_test_function_body(generated, name)
		assert !body.contains('memdup'), body
		assert !body.contains('._pointer_variant_is_owned = true'), body
	}
	run := os.exec([@VEXE, '-new-compiler', '-nocache', 'run', path])
	assert run.exit_code == 0, run.output
}

fn test_sumtype_zero_reassignment_keeps_scalar_variant() {
	dir := os.join_path(os.vtmp_dir(), 'sumtype_zero_${os.getpid()}')
	os.mkdir_all(dir)!
	defer { os.rmdir_all(dir) or {} }
	path := os.join_path(dir, 'main.v')
	os.write_file(path, 'struct Item { value int }
type Choice = int | &Item
type WideChoice = i64 | &Item
fn main() {
 mut value := Choice(&Item{value: 1})
 assert value is &Item
 value = Choice(0)
 assert value is int
 assert (value as int) == 0
 value = &Item{value: 2}
 assert value is &Item
 value = 0
 assert value is int
 assert (value as int) == 0
 mut wide := WideChoice(&Item{value: 3})
 assert wide is &Item
 wide = WideChoice(i64(0))
 assert wide is i64
 assert (wide as i64) == 0
 null := Choice(unsafe { &Item(nil) })
 assert null is &Item
 assert voidptr(null as &Item) == unsafe { nil }
}')!
	result := os.exec([@VEXE, '-new-compiler', '-nocache', '-no-closures', 'run', path])
	assert result.exit_code == 0, result.output
}
