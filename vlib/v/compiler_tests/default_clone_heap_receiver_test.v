import os

fn test_default_clone_reads_heap_receivers_once() {
	root := os.join_path(os.vtmp_dir(), 'default_clone_heap_receiver_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, '@[heap]
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

fn main() {
 mut drops := 0
 {
  value := HeapValue{bytes: [u8(1), 2], drops: &drops}
  mut cloned := value.clone()
  assert cloned.bytes == [u8(1), 2]
  assert unsafe { usize(cloned.bytes.data) != usize(value.bytes.data) }
  cloned.bytes[0] = 3
  assert value.bytes[0] == 1
 }
 assert drops == 2
 {
  mut value := HeapValue{bytes: [u8(4), 5], drops: &drops}
  first := (value).clone()
  assert first.bytes == [u8(4), 5]
  assert unsafe { usize(first.bytes.data) != usize(value.bytes.data) }
  third := clone_mutable(mut value)
  assert third.bytes == [u8(4), 5]
  assert unsafe { usize(third.bytes.data) != usize(value.bytes.data) }
  pointer := &value
  second := pointer.clone()
  assert second.bytes == [u8(4), 5]
  assert unsafe { usize(second.bytes.data) != usize(value.bytes.data) }
 }
 assert drops == 6
 {
  mut value := PromotedValue{bytes: [u8(6), 7], drops: &drops}
  pointer := value.reference()
  assert unsafe { usize(pointer.bytes.data) == usize(value.bytes.data) }
  mut cloned := value.clone()
  assert cloned.bytes == [u8(6), 7]
  assert unsafe { usize(cloned.bytes.data) != usize(value.bytes.data) }
  cloned.bytes[0] = 8
  assert pointer.bytes[0] == 6
 }
 assert drops == 8
}
')!
	for mode in ['-no-parallel', ''] {
		output := os.join_path(root, 'program_${if mode.len > 0 { 'serial' } else { 'parallel' }}')
		compile := os.execute('${os.quoted_path(@VEXE)} -new-compiler -no-memory-limit -nocache -ownership -cc clang ${mode} -o ${os.quoted_path(output)} ${os.quoted_path(source)}')
		assert compile.exit_code == 0, '${mode}: ${compile.output}'
		run := os.execute(os.quoted_path(output))
		assert run.exit_code == 0, '${mode}: ${run.output}'
	}
}
