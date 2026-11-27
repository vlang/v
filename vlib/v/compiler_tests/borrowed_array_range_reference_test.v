// vtest ownership
import os

const borrowed_range_compiler_args = [@VEXE, '-no-memory-limit', '-nocache', '-ownership']

struct BorrowedRangeRejection {
	source   string
	expected string
}

fn test_borrowed_array_references_preserve_owners_and_parameter_headers() {
	root := os.join_path(os.vtmp_dir(), 'borrowed_array_references_${os.getpid()}')
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

fn fill_range(mut bytes []u8, value u8) {
 bytes[0] = value
}

fn disturb_stack(seed int) int {
 values := [seed, seed + 1, seed + 2, seed + 3]!
 return values[0] + values[3]
}

fn check_range_owner() {
 mut owner := Buffer{bytes: [u8(1), 2, 3, 4]}
 address := unsafe { uint(owner.bytes.data) }
 {
  mut first := owner.free_buffer(1)
  assert unsafe { uint(first.data) == address + 1 }
  assert first.len == 3
  assert disturb_stack(5) == 13
  {
   mut second := owner.free_buffer(0)
   assert unsafe { uint(second.data) == address }
   fill_range(mut second, 8)
   assert owner.bytes[0] == 8
   assert second.len == 4
  }
  assert owner.bytes[0] == 8
  fill_range(mut first, 9)
  assert owner.bytes[1] == 9
  assert first.len == 3
 }
 assert owner.bytes == [u8(8), 9, 3, 4]
 assert unsafe { uint(owner.bytes.data) == address }
 {
  read := forwarded(owner, 1, 3)
  zero := owner.buffer(0, 2)
  empty := owner.buffer(2, 2)
  assert disturb_stack(9) == 21
  assert read.len == 2
  assert zero.len == 2
  assert empty.len == 0
  assert unsafe { uint(read.data) == address + 1 }
  assert unsafe { uint(zero.data) == address }
  assert unsafe { uint(empty.data) == address + 2 }
  assert unsafe { (*read)[0] == 9 }
  assert unsafe { (*zero)[0] == 8 }
 }
 assert owner.bytes == [u8(8), 9, 3, 4]
 global_address := unsafe { uint(global_bytes.data) }
 global_range := global_view(1, 3)
 assert unsafe { uint(global_range.data) == global_address + 1 }
 assert unsafe { (*global_range)[0] == 6 }
 heap_range := heap_view()
 assert disturb_stack(12) == 27
 assert unsafe { (*heap_range)[0] == 5 }
 assert heap_range.len == 2
 mut copied := owner.copy()
 assert unsafe { uint(copied.data) != address + 1 }
 copied[0] = 7
 assert owner.bytes[1] == 9
 assert copied[0] == 7
}

fn fill(mut dst []u8) {
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

fn check_parameter_header() {
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

fn main() {
 check_range_owner()
 println("range owner")
 check_parameter_header()
 println("parameter header")
}
')!
	for mode in [['-no-parallel'], []string{}] {
		name := if mode.len > 0 { 'program_serial' } else { 'program_parallel' }
		output := os.join_path(root, name + $if windows { '.exe' } $else { '' })
		compile := os.exec([...borrowed_range_compiler_args, '-cc', 'clang', ...mode, '-o', output,
			source])
		assert compile.exit_code == 0, '${mode}: ${compile.output}'
		run := os.exec([output])
		assert run.exit_code == 0, '${mode}: ${run.output}'
		assert run.output.trim_space().split_into_lines() == ['range owner', 'parameter header'], '${mode}: ${run.output}'
	}
}

fn test_borrowed_array_and_string_ranges_cannot_escape_local_owners() {
	root := os.join_path(os.vtmp_dir(), 'borrowed_range_local_owner_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
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
	check_borrowed_range_rejections(root, cases.map(BorrowedRangeRejection{
		source:   it + '\nfn main() {}\n'
		expected: 'cannot return a reference to local storage'
	}))!
}

fn check_borrowed_range_rejections(root string, cases []BorrowedRangeRejection) ! {
	modes := [['-no-parallel'], []string{}]
	// Custom flags can change import resolution or limit diagnostics.
	if os.getenv('VFLAGS').trim_space() !in ['', '-gc none'] {
		source := os.join_path(root, 'main.v')
		for case in cases {
			os.write_file(source, case.source)!
			for mode in modes {
				compile := os.exec([...borrowed_range_compiler_args, ...mode, '-check', source])
				assert compile.exit_code != 0, '${mode}: ${case.source}\n${compile.output}'
				assert compile.output.contains(case.expected), '${mode}: ${case.source}\n${compile.output}'
			}
		}
		return
	}
	// Keep the original fixture declarations isolated in separate modules.
	mut imports := []string{cap: cases.len}
	for i, case in cases {
		name := 'range_case_${i}'
		dir := os.join_path(root, name)
		os.mkdir(dir)!
		os.write_file(os.join_path(dir, '${name}.v'), 'module ${name}\n\n${case.source}')!
		imports << 'import ${name} as _'
	}
	os.write_file(os.join_path(root, 'main.v'), 'module main\n\n${imports.join('\n')}\n\nfn main() {}\n')!
	for mode in modes {
		compile := os.exec([...borrowed_range_compiler_args, ...mode, '-check', root])
		assert compile.exit_code != 0, '${mode}: ${compile.output}'
		diagnostics := compile.output.split_into_lines().filter(it.contains('error: '))
		for i, case in cases {
			assert diagnostics.any(it.contains('range_case_${i}.v:')
				&& it.all_after('error: ').contains(case.expected)), '${mode}: ${case.source}\n${compile.output}'
		}
	}
}

fn test_borrowed_string_ranges_preserve_headers_bounds_and_evaluation_order() {
	root := os.join_path(os.vtmp_dir(), 'borrowed_string_ranges_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, '@[has_globals]
module main

import os

__global calls = 0
__global ordered_text = "abcdef"
fn source() &string { assert calls == 0; calls++; return &ordered_text }
fn ordered_start() int { assert calls == 1; calls++; return 1 }
fn ordered_end() int { assert calls == 2; calls++; return 4 }

fn split[^a](text &^a string) []&^a string {
 mut result := []&^a string{}
 for start in [0, 4] {
  end := if start == 0 { 4 } else { text.len }
  result << &(*text)[start..end]
 }
 return result
}
fn header_view[^a](text &^a string, start int, end int) &^a string {
 return &((*text)[start..end])
}
fn disturb_stack(seed int) int {
 values := [seed, seed + 1, seed + 2, seed + 3]!
 return values[0] + values[3]
}
fn check_headers() {
 text := "abc\\nxyz".to_owned()
 address := unsafe { uint(text.str) }
 {
  parts := split(text)
  first := header_view(text, 0, 4)
  full := header_view(text, 0, text.len)
  last := header_view(text, 4, text.len)
  empty := header_view(text, 2, 2)
  assert disturb_stack(3) == 9
  assert *parts[0] == "abc\\n"
  assert *parts[1] == "xyz"
  assert unsafe { uint(parts[0].str) == address }
  assert unsafe { uint(parts[1].str) == address + 4 }
  assert voidptr(parts[0]) != voidptr(parts[1])
  assert *full == text
  assert unsafe { uint(full.str) == address }
  assert *first == "abc\\n"
  assert *last == "xyz"
  assert empty.len == 0
  assert unsafe { uint(empty.str) == address + 2 }
 }
 assert text == "abc\\nxyz"
 assert unsafe { uint(text.str) == address }
}

fn check_bounds() {
 start := os.args[1].int()
 end := os.args[2].int()
 text := "abcd".to_owned()
 view := &text[start..end]
 assert view.len == end - start
}

fn check_evaluation_order() {
 view := &(*source())[ordered_start()..ordered_end()]
 assert calls == 3
 assert *view == "bcd"
 assert unsafe { view.str == ordered_text.str + 1 }
}

fn main() {
 if os.args.len == 1 {
  check_headers()
  println("headers")
  check_evaluation_order()
  println("evaluation order")
 } else {
  check_bounds()
  println("bounds")
 }
}
')!
	for mode in [['-no-parallel'], []string{}] {
		name := if mode.len > 0 { 'program_serial' } else { 'program_parallel' }
		output := os.join_path(root, name + $if windows { '.exe' } $else { '' })
		compile := os.exec([...borrowed_range_compiler_args, '-cc', 'clang', ...mode, '-o', output,
			source])
		assert compile.exit_code == 0, '${mode}: ${compile.output}'
		run := os.exec([output])
		assert run.exit_code == 0, '${mode}: ${run.output}'
		assert run.output.trim_space().split_into_lines() == ['headers', 'evaluation order'], '${mode}: ${run.output}'
		valid := os.exec([output, '1', '3'])
		assert valid.exit_code == 0, '${mode}: ${valid.output}'
		assert valid.output.trim_space() == 'bounds', '${mode}: ${valid.output}'
		for bounds in [[-1, 2], [0, 5], [3, 2], [5, 5], [0, -1]] {
			start, end := bounds[0], bounds[1]
			invalid := os.exec([output, start.str(), end.str()])
			assert invalid.exit_code != 0, '${mode}: ${bounds}: ${invalid.output}'
			assert invalid.output.contains('substr(${start}, ${end}) out of bounds (len=4) s=abcd'), '${mode}: ${bounds}: ${invalid.output}'
		}
	}
}

fn test_borrowed_string_ranges_keep_source_loans_live() {
	root := os.join_path(os.vtmp_dir(), 'borrowed_string_range_loan_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	mut cases := []BorrowedRangeRejection{}
	for mutation, expected in {
		'text = "replacement".to_owned()': 'cannot assign to `text` because `text[1]` is borrowed by `view`'
		'take(text)':                      'cannot move `text` because `text[1]` is borrowed by `view`'
	} {
		fixture := 'fn take(value string) { _ = value.to_owned() }
fn main() {
 mut text := "original".to_owned()
 view := &text[1..]
 ' + mutation + '\n assert *view == "riginal"
}
'
		cases << BorrowedRangeRejection{fixture, expected}
	}
	check_borrowed_range_rejections(root, cases)!
}

fn test_mixed_mutable_array_reference_returns_keep_source_loans_live() {
	root := os.join_path(os.vtmp_dir(), 'borrowed_array_range_loan_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	mut cases := []BorrowedRangeRejection{}
	for argument in ['owner', 'owner[..]'] {
		for mutation in ['drop_owned(owner)', 'owner = ["replacement".to_owned()]'] {
			fixture := 'fn choose_view(mut values []string, whole bool) &[]string {
 if whole { return &values }
 values_ref := &values
 return &(*values_ref)[1..]
}
fn main() {
 mut owner := ["first".to_owned(), "second".to_owned()]
 result := choose_view(mut ' + argument + ', false)
 ' + mutation + '\n assert (*result)[0] == "second"
}
'
			cases << BorrowedRangeRejection{fixture, 'borrowed by `result`'}
		}
	}
	check_borrowed_range_rejections(root, cases)!
}
