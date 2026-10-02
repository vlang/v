import os

fn test_ownership_string_views_do_not_create_implicit_owners() {
	root := os.join_path(os.vtmp_dir(), 'ownership_string_views_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'import os
import encoding.utf8

fn read_view(value string) int {
	return value.len
}

fn normalize_view(value string) string {
	return value
}

fn independent_copy(value &string) string {
	return *value
}

struct Holder {
	name string
}

struct Payload implements IClone {
mut:
	values []int
}

fn (value &Payload) clone() Payload {
	return Payload{values: value.values.clone()}
}

fn path_view[^a](path &^a string) string {
	return normalize_view(*path)
}

fn escaped_view() (string, voidptr) {
	local := "owned local".to_owned()
	ptr := &local
	return normalize_view(*ptr), unsafe { voidptr(local.str) }
}

fn escaped_unsafe_view() (string, voidptr) {
	local := "owned local".to_owned()
	return unsafe { local.substr_unsafe(1, 5) }, unsafe { voidptr(local.str + 1) }
}

fn main() {
	text := "prefix/filename".to_owned()
	mut view := text[7..]
	assert read_view(view) == 8
	assert utf8.validate_str(view)
	assert view.to_owned() == "filename"
	view = view[..4]
	assert read_view(view) == 4
	assert view == "file"
	assert text == "prefix/filename"
	path := path_view(&text)
	assert path == text
	assert unsafe { path.str != text.str }
	assert read_view(path) == text.len
	ptr := &text
	normalized := normalize_view(*ptr)
	assert read_view(normalized) == text.len
	assert normalized == text
	dereferenced := *ptr
	assert unsafe { dereferenced.str == text.str }
	assert read_view(dereferenced) == text.len
	assert dereferenced == text
	unsafe_view := unsafe { text.substr_unsafe(7, text.len) }
	assert unsafe { unsafe_view.str == text.str + 7 }
	assert read_view(unsafe_view) == 8
	assert unsafe_view == "filename"
	independent := independent_copy(ptr)
	assert unsafe { independent.str != text.str }
	assert read_view(independent) == text.len
	assert text == "prefix/filename"
	stored := Holder{name: *ptr}
	assert unsafe { stored.name.str != text.str }
	assert stored.name == text
	payload := Payload{values: [1, 2]}
	payload_ptr := &payload
	mut payload_copy := *payload_ptr
	payload_copy.values[0] = 9
	assert payload.values[0] == 1
	escaped, source_ptr := escaped_view()
	assert escaped == "owned local"
	assert unsafe { voidptr(escaped.str) != source_ptr }
	escaped_unsafe, unsafe_source_ptr := escaped_unsafe_view()
	assert escaped_unsafe == "wned"
	assert unsafe { voidptr(escaped_unsafe.str) != unsafe_source_ptr }
	owner := "independent".to_owned()
	independent_slice := owner[..3]
	assert read_view(owner) == 11
	assert independent_slice == "ind"
	assert os.join_path(view, "child") == "file/child"
	assert view == "file"
	println("ok")
}
')!
	for mode in ['-no-parallel', ''] {
		out := os.execute('${os.quoted_path(@VEXE)} -new-compiler -nocache -ownership -d ownership -cc clang ${mode} run ${os.quoted_path(source)}')
		assert out.exit_code == 0, '${mode}: ${out.output}'
		assert out.output.trim_space() == 'ok', out.output
	}
}

fn test_ownership_string_views_still_require_explicit_owned_copies_to_move() {
	root := os.join_path(os.vtmp_dir(), 'ownership_string_view_moves_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	for creation in ['"abcd".to_owned()', '"abcd"[1..].to_owned()', '(*ptr).to_owned()',
		'original.clone()'] {
		os.write_file(source, 'fn consume(value string) { _ = value }
fn main() {
	original := "abcd".to_owned()
	ptr := &original
	owned := ${creation}
	consume(owned)
	println(owned)
}
')!
		out := os.execute('${os.quoted_path(@VEXE)} -new-compiler -nocache -ownership -d ownership -check ${os.quoted_path(source)}')
		assert out.exit_code != 0, '${creation}: ${out.output}'
		assert out.output.contains('use of moved value: `owned`'), '${creation}: ${out.output}'
	}
}

fn test_ownership_string_views_keep_owner_borrow_until_view_expires() {
	root := os.join_path(os.vtmp_dir(), 'ownership_string_view_borrows_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	for expr in ['*ptr', 'unsafe { original.substr_unsafe(0, 2) }', 'identity(*ptr)'] {
		for invalid_action, expected in {
			'consume(original)':                   'cannot move `original` because it is borrowed'
			'original = "replacement".to_owned()': 'cannot assign to `original` because it is borrowed'
		} {
			os.write_file(source, 'fn consume(value string) { _ = value }
fn identity(value string) string { return value }
fn main() {
	mut original := "abcd".to_owned()
	mut view := ""
	{
		ptr := &original
		view = ${expr}
	}
	${invalid_action}
	assert view.len > 0
}
')!
			out := os.execute('${os.quoted_path(@VEXE)} -new-compiler -nocache -ownership -d ownership -check ${os.quoted_path(source)}')
			assert out.exit_code != 0, '${expr}: ${out.output}'
			assert out.output.contains(expected), '${expr}: ${out.output}'
		}
	}
}

fn test_ownership_string_views_are_copied_at_owning_call_boundaries() {
	root := os.join_path(os.vtmp_dir(), 'ownership_string_view_mixed_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	fixture := 'fn identity(value string) string { return value }
fn read(value string) (int, voidptr) {
	return value.len, unsafe { voidptr(value.str) }
}
fn read_many(values ...string) voidptr {
	return unsafe { voidptr(values[0].str) }
}
fn escape() (string, voidptr) {
	local := "local owner".to_owned()
	ptr := &local
	return identity(*ptr), unsafe { voidptr(local.str) }
}
fn main() {
	OWNED_CALLS_START
	local := "second owner".to_owned()
	ptr := &local
	borrowed := *ptr
	alias := borrowed
	view_len, read_ptr := read(alias)
	assert view_len == local.len
	assert unsafe { read_ptr != voidptr(local.str) }
	assert borrowed == "second owner"
	assert alias == "second owner"
	variadic_ptr := read_many(alias)
	assert unsafe { variadic_ptr != voidptr(local.str) }
	assert alias == "second owner"
	conditional_len, conditional_ptr := read(if borrowed.len > 0 { borrowed } else { "empty" })
	assert conditional_len == local.len
	assert unsafe { conditional_ptr != voidptr(local.str) }
	assert borrowed == "second owner"
	view := identity(*ptr)
	assert unsafe { view.str != local.str }
	assert view == "second owner"
	assert local == "second owner"
	escaped, original := escape()
	assert escaped == "local owner"
	assert unsafe { voidptr(escaped.str) != original }
	literal_len, _ := read("literal")
	assert literal_len == 7
	slice := local[..3]
	slice_len, slice_ptr := read(slice)
	assert slice_len == 3
	assert unsafe { slice_ptr != voidptr(slice.str) }
	assert slice == "sec"
	assert local == "second owner"
	OWNED_CALLS_END
	println("ok")
}
'
	owned_calls := 'taken := identity("owned argument".to_owned())
	assert taken == "owned argument"
	owned_len, _ := read("owned reader".to_owned())
	assert owned_len == 12
	_ = read_many("owned variadic".to_owned())'
	for first in [true, false] {
		os.write_file(source, fixture.replace('OWNED_CALLS_START', if first {
			owned_calls
		} else {
			''
		}).replace('OWNED_CALLS_END', if first { '' } else { owned_calls }))!
		for mode in ['-no-parallel', ''] {
			out := os.execute('${os.quoted_path(@VEXE)} -new-compiler -nocache -ownership -d ownership -cc clang ${mode} run ${os.quoted_path(source)}')
			assert out.exit_code == 0, '${mode}: ${out.output}'
			assert out.output.trim_space() == 'ok', out.output
		}
	}
}

fn test_ownership_string_view_results_from_owning_calls_are_owned() {
	root := os.join_path(os.vtmp_dir(), 'ownership_string_view_result_moves_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'fn identity(value string) string { return value }
fn consume(value string) { _ = value }
fn main() {
	owned := identity("owned argument".to_owned())
	assert owned.len > 0
	original := "borrowed argument".to_owned()
	ptr := &original
	result := identity(*ptr)
	consume(result)
	println(result)
}
')!
	for mode in ['-no-parallel', ''] {
		out := os.execute('${os.quoted_path(@VEXE)} -new-compiler -nocache -ownership -d ownership ${mode} -check ${os.quoted_path(source)}')
		assert out.exit_code != 0, '${mode}: ${out.output}'
		assert out.output.contains('use of moved value: `result`'), '${mode}: ${out.output}'
	}
}

fn test_ownership_string_call_copies_preserve_owned_argument_moves() {
	root := os.join_path(os.vtmp_dir(), 'ownership_string_call_copy_moves_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	for next in ['consume(owned)', 'consume(if owned.len > 0 { owned } else { "fallback" })'] {
		os.write_file(source, 'fn consume(value string) { _ = value }
fn main() {
	owned := "owned argument".to_owned()
	consume(owned)
	${next}
}
')!
		for mode in ['-no-parallel', ''] {
			out := os.execute('${os.quoted_path(@VEXE)} -new-compiler -nocache -ownership -d ownership ${mode} -check ${os.quoted_path(source)}')
			assert out.exit_code != 0, '${mode}: ${out.output}'
			assert out.output.contains('use of moved value: `owned`'), '${mode}: ${out.output}'
		}
	}
}
