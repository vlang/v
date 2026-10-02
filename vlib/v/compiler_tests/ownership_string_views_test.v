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
