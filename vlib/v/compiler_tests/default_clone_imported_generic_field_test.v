// vtest ownership
import os

fn test_default_clone_qualifies_imported_fields_and_reads_heap_receivers_once() {
	root := os.join_path(os.vtmp_dir(), 'default_clone_imported_generic_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'leaf'))!
	os.mkdir_all(os.join_path(root, 'wrapper'))!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'default_clone_imported_generic' }")!
	os.write_file(os.join_path(root, 'leaf', 'leaf.v'), 'module leaf

pub struct Box[T] implements IClone, Drop {
pub mut:
	values []T
	clones &int
	drops &int
}

pub fn (value &Box[T]) clone() Box[T] {
	unsafe { *value.clones += 1 }
	return Box[T]{
		values: value.values.clone()
		clones: value.clones
		drops: value.drops
	}
}

pub fn (mut value Box[T]) drop() {
	unsafe {
		*value.drops += 1
		value.values.free()
	}
	value.values = []T{}
}
')!
	os.write_file(os.join_path(root, 'wrapper', 'wrapper.v'), 'module wrapper

import leaf

struct Payload implements IClone {
mut:
	value int
}

pub struct Config implements IClone {
mut:
	inner leaf.Box[Payload]
}

pub struct Holder implements IClone {
mut:
	config Config
}

pub fn new(clones &int, drops &int) Config {
	return Config{
		inner: leaf.Box[Payload]{
			values: [Payload{value: 7}]
			clones: clones
			drops: drops
		}
	}
}

fn Holder.new(config &Config) Holder {
	return Holder{config: config.clone()}
}

pub struct Factory[T] {
	token T
}

pub fn factory[T](token T) Factory[T] {
	return Factory[T]{token: token}
}

pub fn (factory &Factory[T]) build(config &Config) Holder {
	return Holder.new(config)
}

pub fn copy(config &Config) Config {
	return *config
}

pub fn (mut holder Holder) set(value int) {
	holder.config.inner.values[0].value = value
}

pub fn (config &Config) value() int {
	return config.inner.values[0].value
}

pub fn (holder &Holder) value() int {
	return holder.config.value()
}
')!
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'import wrapper

fn main() {
	mut clones := 0
	mut drops := 0
	{
		original := wrapper.new(&clones, &drops)
		factory := wrapper.factory(1)
		{
			mut holder := factory.build(&original)
			assert clones == 1
			assert holder.value() == 7
			holder.set(9)
			assert holder.value() == 9
			assert original.value() == 7
		}
		assert drops == 1
		{
			copied := wrapper.copy(&original)
			assert clones == 2
			assert copied.value() == 7
		}
		assert drops == 2
		assert original.value() == 7
	}
	assert drops == 3
	check_heap_receivers()
}
')!
	os.write_file(os.join_path(root, 'heap_receivers.v'), '@[heap]
struct HeapValue implements IClone, Drop {
mut:
 bytes []u8
 drops &int
}

fn (mut value HeapValue) drop() {
 unsafe {
  *value.drops += 1
  value.bytes.free()
 }
 value.bytes = []u8{}
}

struct PromotedValue implements IClone, Drop {
mut:
 bytes []u8
 drops &int
}

fn (mut value PromotedValue) drop() {
 unsafe {
  *value.drops += 1
  value.bytes.free()
 }
 value.bytes = []u8{}
}

fn (mut value PromotedValue) reference() &PromotedValue {
 return value
}

fn clone_mutable(mut value HeapValue) HeapValue {
 return value.clone()
}

fn check_heap_receivers() {
 mut drops := 0
 {
  value := HeapValue{bytes: [u8(1), 2], drops: &drops}
  mut cloned := value.clone()
  assert cloned.bytes == [u8(1), 2]
  assert unsafe { uint(cloned.bytes.data) != uint(value.bytes.data) }
  cloned.bytes[0] = 3
  assert value.bytes[0] == 1
 }
 assert drops == 2
 {
  mut value := HeapValue{bytes: [u8(4), 5], drops: &drops}
  first := (value).clone()
  assert first.bytes == [u8(4), 5]
  assert unsafe { uint(first.bytes.data) != uint(value.bytes.data) }
  third := clone_mutable(mut value)
  assert third.bytes == [u8(4), 5]
  assert unsafe { uint(third.bytes.data) != uint(value.bytes.data) }
  pointer := &value
  second := pointer.clone()
  assert second.bytes == [u8(4), 5]
  assert unsafe { uint(second.bytes.data) != uint(value.bytes.data) }
 }
 assert drops == 6
 {
  mut value := PromotedValue{bytes: [u8(6), 7], drops: &drops}
  pointer := value.reference()
  assert unsafe { uint(pointer.bytes.data) == uint(value.bytes.data) }
  mut cloned := value.clone()
  assert cloned.bytes == [u8(6), 7]
  assert unsafe { uint(cloned.bytes.data) != uint(value.bytes.data) }
  cloned.bytes[0] = 8
  assert pointer.bytes[0] == 6
 }
 assert drops == 8
}
')!
	for mode in [['-no-parallel'], []string{}] {
		output := os.join_path(root, 'program_${mode.len}')
		compile := os.exec([@VEXE, '-no-memory-limit', '-nocache', '-ownership', '-d', 'ownership',
			'-cc', 'clang', ...mode, '-o', output, root])
		assert compile.exit_code == 0, '${mode}: ${compile.output}'
		run := os.exec([output])
		assert run.exit_code == 0, '${mode}: ${run.output}'
	}
}
