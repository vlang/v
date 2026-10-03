import os

fn test_borrowed_array_range_references_preserve_the_owner() {
	root := os.join_path(os.vtmp_dir(), 'borrowed_array_range_reference_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, '@[has_globals]
module main

__global global_bytes = [u8(5), 6, 7]

struct Buffer implements IClone {
mut:
 bytes []u8
}

fn (value &^a Buffer) buffer[^a](start int, end int) &^a []u8 {
 return &value.bytes[start..end]
}

fn (mut value ^a Buffer) free_buffer[^a](start int) &^a []u8 {
 return &value.bytes[start..]
}

fn forwarded[^a](value &^a Buffer, start int, end int) &^a []u8 {
 return value.buffer(start, end)
}

fn global_view(start int, end int) &[]u8 {
 return unsafe { &global_bytes[start..end] }
}

fn heap_view() &[]u8 {
 owner := &Buffer{bytes: [u8(4), 5, 6]}
 return owner.buffer(1, 3)
}

fn (value &Buffer) copy() []u8 {
 return value.bytes[1..3]
}

fn fill(mut bytes []u8, value u8) {
 bytes[0] = value
}

fn disturb_stack(seed int) int {
 values := [seed, seed + 1, seed + 2, seed + 3]!
 return values[0] + values[3]
}

fn main() {
 mut owner := Buffer{bytes: [u8(1), 2, 3, 4]}
 address := unsafe { usize(owner.bytes.data) }
 {
  mut first := owner.free_buffer(1)
  assert unsafe { usize(first.data) == address + 1 }
  assert first.len == 3
  assert disturb_stack(5) == 13
  {
   mut second := owner.free_buffer(0)
   assert unsafe { usize(second.data) == address }
   fill(mut second, 8)
   assert owner.bytes[0] == 8
   assert second.len == 4
  }
  assert owner.bytes[0] == 8
  fill(mut first, 9)
  assert owner.bytes[1] == 9
  assert first.len == 3
 }
 assert owner.bytes == [u8(8), 9, 3, 4]
 assert unsafe { usize(owner.bytes.data) == address }
 {
  read := forwarded(owner, 1, 3)
  zero := owner.buffer(0, 2)
  empty := owner.buffer(2, 2)
  assert disturb_stack(9) == 21
  assert read.len == 2
  assert zero.len == 2
  assert empty.len == 0
  assert unsafe { usize(read.data) == address + 1 }
  assert unsafe { usize(zero.data) == address }
  assert unsafe { usize(empty.data) == address + 2 }
  assert unsafe { (*read)[0] == 9 }
  assert unsafe { (*zero)[0] == 8 }
 }
 assert owner.bytes == [u8(8), 9, 3, 4]
 global_address := unsafe { usize(global_bytes.data) }
 global_range := global_view(1, 3)
 assert unsafe { usize(global_range.data) == global_address + 1 }
 assert unsafe { (*global_range)[0] == 6 }
 heap_range := heap_view()
 assert disturb_stack(12) == 27
 assert unsafe { (*heap_range)[0] == 5 }
 assert heap_range.len == 2
 mut copied := owner.copy()
 assert unsafe { usize(copied.data) != address + 1 }
 copied[0] = 7
 assert owner.bytes[1] == 9
 assert copied[0] == 7
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

fn test_borrowed_array_and_string_ranges_cannot_escape_local_owners() {
	root := os.join_path(os.vtmp_dir(), 'borrowed_range_local_owner_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	cases := [
		'fn escaped[^a]() &^a []u8 { values := [u8(1), 2]; return &values[1..] }',
		'fn escaped[^a](values []u8) &^a []u8 { return &values[1..] }',
		'fn parameter_reference(mut values []u8) &[]u8 { return &values }
fn escaped() &[]u8 { mut values := [u8(1), 2]; return parameter_reference(mut values) }',
		'fn parameter_reference(mut values []u8) &[]u8 { return &values }
fn forwarded(mut values []u8) &[]u8 { return parameter_reference(mut values) }
fn escaped() &[]u8 { mut values := [u8(1), 2]; return forwarded(mut values) }',
		'fn parameter_reference(mut values []u8) &[]u8 { return &values }
struct Holder { mut: values []u8 }
fn escaped() &[]u8 { mut holder := Holder{values: [u8(1), 2]}; return parameter_reference(mut holder.values) }',
		'fn parameter_reference(mut values []u8) &[]u8 { return &values }
fn escaped() &[]u8 { mut values := [u8(1), 2]; reference := parameter_reference(mut values); return reference }',
		'fn parameter_reference(mut values []u8) &[]u8 { return &values }
fn escaped(choose bool) &[]u8 { return if choose { mut local := [u8(1), 2]; parameter_reference(mut local) } else { mut local := [u8(3), 4]; parameter_reference(mut local) } }',
		'fn choose_view(mut values []string, whole bool) &[]string { values_ref := &values; if whole { return &values }; return &(*values_ref)[1..] }
fn escaped() &[]string { mut owner := ["first".to_owned(), "second".to_owned()]; return choose_view(mut owner[..], false) }',
		'fn choose_view(mut values []string, whole bool) &[]string { values_ref := &values; if whole { return &values }; view := &(*values_ref)[1..]; return view }
fn escaped() &[]string { mut owner := ["first".to_owned(), "second".to_owned()]; return choose_view(mut owner[..], false) }',
		'struct Resource implements IClone, Drop { value int }
fn (mut value Resource) drop() {}
fn parameter_reference(mut values []Resource) &[]Resource { return &values }
fn escaped() &[]Resource { mut values := [Resource{1}, Resource{2}]; return parameter_reference(mut values) }',
		'fn escaped[^a]() &^a []u8 { values := [u8(1), 2]; view := &values[1..]; return view }',
		'fn escaped[^a]() &^a []u8 { values := [u8(1), 2]; return &(values[1..]) }',
		'fn escaped[^a](choose bool) &^a []u8 { values := [u8(1), 2]; return if choose { &values[1..] } else { &values[..] } }',
		'struct Resource implements IClone, Drop { value int }
fn (mut value Resource) drop() {}
fn escaped[^a]() &^a []Resource { values := [Resource{1}, Resource{2}]; return &values[1..] }',
		'struct Resource implements IClone, Drop { value int }
fn (mut value Resource) drop() {}
fn escaped[^a](values []Resource) &^a []Resource { return &values[1..] }',
		'fn escaped[^a]() &^a string { value := "ab".repeat(3); return &value[1..] }',
		'fn escaped[^a](value string) &^a string { return &value[1..] }',
		'fn escaped(choose bool) &string { return if choose { local := "abcdef".to_owned(); &local[1..] } else { local := "ghijkl".to_owned(); &local[1..] } }',
		'fn escaped(choose bool) &string { return match choose { true { local := "abcdef".to_owned(); &local[1..] } false { local := "ghijkl".to_owned(); &local[1..] } } }',
	]
	for code in cases {
		os.write_file(source, code + '\nfn main() {}\n')!
		for mode in ['-no-parallel', ''] {
			compile := os.execute('${os.quoted_path(@VEXE)} -new-compiler -no-memory-limit -nocache -ownership ${mode} -check ${os.quoted_path(source)}')
			assert compile.exit_code != 0, '${mode}: ${code}: ${compile.output}'
			assert compile.output.contains('cannot return a reference to local storage'), '${mode}: ${code}: ${compile.output}'
		}
	}
}

fn test_borrowed_string_range_headers_survive_returns_and_loop_iterations() {
	root := os.join_path(os.vtmp_dir(), 'borrowed_string_range_reference_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'fn split[^a](text &^a string) []&^a string {
  mut result := []&^a string{}
  for start in [0, 4] {
   end := if start == 0 { 4 } else { text.len }
   result << &(*text)[start..end]
  }
  return result
 }
 fn view[^a](text &^a string, start int, end int) &^a string {
  return &((*text)[start..end])
 }
 fn disturb_stack(seed int) int {
  values := [seed, seed + 1, seed + 2, seed + 3]!
  return values[0] + values[3]
 }
 fn main() {
  text := "abc\\nxyz".to_owned()
  address := unsafe { usize(text.str) }
  {
   parts := split(text)
   first := view(text, 0, 4)
   full := view(text, 0, text.len)
   last := view(text, 4, text.len)
   empty := view(text, 2, 2)
   assert disturb_stack(3) == 9
   assert *parts[0] == "abc\\n"
   assert *parts[1] == "xyz"
   assert unsafe { usize(parts[0].str) == address }
   assert unsafe { usize(parts[1].str) == address + 4 }
   assert voidptr(parts[0]) != voidptr(parts[1])
   assert *full == text
   assert unsafe { usize(full.str) == address }
   assert *first == "abc\\n"
   assert *last == "xyz"
   assert empty.len == 0
   assert unsafe { usize(empty.str) == address + 2 }
  }
  assert text == "abc\\nxyz"
  assert unsafe { usize(text.str) == address }
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

fn test_borrowed_string_ranges_keep_source_loans_live() {
	root := os.join_path(os.vtmp_dir(), 'borrowed_string_range_loan_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	for mutation in ['text = "replacement".to_owned()', 'take(text)'] {
		os.write_file(source, 'fn take(value string) { _ = value.to_owned() }
fn main() {
 mut text := "original".to_owned()
 view := &text[1..]
 ' + mutation + '\n assert *view == "riginal"
}
')!
		for mode in ['-no-parallel', ''] {
			compile := os.execute('${os.quoted_path(@VEXE)} -new-compiler -no-memory-limit -nocache -ownership ${mode} -check ${os.quoted_path(source)}')
			assert compile.exit_code != 0, '${mode}: ${mutation}: ${compile.output}'
			assert compile.output.contains('borrow'), '${mode}: ${mutation}: ${compile.output}'
		}
	}
}

fn test_borrowed_string_ranges_preserve_bounds_checks_and_evaluation_order() {
	root := os.join_path(os.vtmp_dir(), 'borrowed_string_range_bounds_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'import os
fn main() {
 start := os.args[1].int()
 end := os.args[2].int()
 text := "abcd".to_owned()
 view := &text[start..end]
 assert view.len == end - start
}
')!
	for mode in ['-no-parallel', ''] {
		output := os.join_path(root, 'bounds_${if mode.len > 0 { 'serial' } else { 'parallel' }}')
		compile := os.execute('${os.quoted_path(@VEXE)} -new-compiler -no-memory-limit -nocache -ownership -cc clang ${mode} -o ${os.quoted_path(output)} ${os.quoted_path(source)}')
		assert compile.exit_code == 0, '${mode}: ${compile.output}'
		for bounds in [[-1, 2], [0, 5], [3, 2], [5, 5], [0, -1]] {
			start, end := bounds[0], bounds[1]
			run := os.execute('${os.quoted_path(output)} ${start} ${end}')
			assert run.exit_code != 0, '${mode}: ${bounds}: ${run.output}'
			assert run.output.contains('substr(${start}, ${end}) out of bounds (len=4) s=abcd'), '${mode}: ${bounds}: ${run.output}'
		}
		run := os.execute('${os.quoted_path(output)} 1 3')
		assert run.exit_code == 0, '${mode}: ${run.output}'
	}
	os.write_file(source, '@[has_globals]
module main
__global calls = 0
__global text = "abcdef"
fn source() &string { assert calls == 0; calls++; return &text }
fn start() int { assert calls == 1; calls++; return 1 }
fn end() int { assert calls == 2; calls++; return 4 }
fn main() { view := &(*source())[start()..end()]; assert calls == 3; assert *view == "bcd"; assert unsafe { view.str == text.str + 1 } }
')!
	for mode in ['-no-parallel', ''] {
		output := os.join_path(root, 'ordered_${if mode.len > 0 { 'serial' } else { 'parallel' }}')
		compile := os.execute('${os.quoted_path(@VEXE)} -new-compiler -no-memory-limit -nocache -ownership -cc clang ${mode} -o ${os.quoted_path(output)} ${os.quoted_path(source)}')
		assert compile.exit_code == 0, '${mode}: ${compile.output}'
		run := os.execute(os.quoted_path(output))
		assert run.exit_code == 0, '${mode}: ${run.output}'
	}
}

fn test_explicit_mutable_array_parameter_addresses_update_the_caller_header() {
	root := os.join_path(os.vtmp_dir(), 'borrowed_mutable_array_header_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'fn fill(mut dst []u8) {
 dst_ptr := &dst
 callback := fn [dst_ptr] () {
  unsafe { (*dst_ptr)[0] = u8(9) }
  for value in 0 .. 100 { unsafe { (*dst_ptr) << u8(value) } }
 }
 callback()
 assert dst.len == 101
 assert dst[0] == 9
}
fn parameter_reference[^a](mut values ^a []u8) &^a []u8 { return &values }
struct Resource implements IClone, Drop { value int }
fn (mut value Resource) drop() {}
fn resource_reference[^a](mut values ^a []Resource) &^a []Resource { return &values }
fn fill_slice(mut dst []u8) {
 dst_ptr := &dst
 callback := fn [dst_ptr] () { unsafe { (*dst_ptr)[0] = u8(8) } }
 callback()
}

fn main() {
 mut dst := [u8(1)]
 {
  reference := parameter_reference(mut dst)
  assert voidptr(reference) == voidptr(&dst)
  assert unsafe { reference.data == dst.data }
 }
 fill(mut dst)
 assert dst.len == 101
 assert dst[0] == 9
 assert dst[100] == 99
 fill_slice(mut dst[1..2])
 assert dst[1] == 8
 mut fixed := [u8(1), 2]!
 fill_slice(mut fixed[..])
 assert fixed[0] == 8
 mut resources := [Resource{1}]
 {
  view := resource_reference(mut resources)
  assert voidptr(view) == voidptr(&resources)
  assert unsafe { view.data == resources.data }
  assert unsafe { (*view)[0].value == 1 }
 }
 assert resources[0].value == 1
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

fn test_mixed_mutable_array_reference_returns_keep_source_loans_live() {
	root := os.join_path(os.vtmp_dir(), 'borrowed_array_range_loan_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	for argument in ['owner', 'owner[..]'] {
		for mutation in ['drop_owned(owner)', 'owner = ["replacement".to_owned()]'] {
			os.write_file(source, 'fn choose_view(mut values []string, whole bool) &[]string {
 if whole { return &values }
 values_ref := &values
 return &(*values_ref)[1..]
}
fn main() {
 mut owner := ["first".to_owned(), "second".to_owned()]
 result := choose_view(mut ' + argument + ', false)
 ' + mutation + '\n assert (*result)[0] == "second"
}
')!
			for mode in ['-no-parallel', ''] {
				compile := os.execute('${os.quoted_path(@VEXE)} -new-compiler -no-memory-limit -nocache -ownership ${mode} -check ${os.quoted_path(source)}')
				assert compile.exit_code != 0, '${mode}: ${argument}: ${mutation}: ${compile.output}'
				assert compile.output.contains('borrowed by `result`'), '${mode}: ${argument}: ${mutation}: ${compile.output}'
			}
		}
	}
}
