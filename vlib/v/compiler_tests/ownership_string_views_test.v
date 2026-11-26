import os
import json2

struct OwnershipStringViewRejection {
	body                    string
	expected                string
	standalone_declarations ?string
}

struct OwnershipStringViewDiagnostic {
	file     string
	severity string
	message  string
}

const ownership_string_view_rejection_args = [@VEXE, '-nocache', '-ownership', '-d', 'ownership']
const ownership_string_returned_fallback_body = '	mut original := "fallback".to_owned()
	mut result := "".to_owned()
	{
		view := original.substr_unsafe(1, 4)
		result = "abc".substr_or(0, 4, view)
		assert view == "all"
		assert unsafe { result.str != view.str }
	}
	original = "replacement".to_owned()'

fn check_ownership_string_view_rejections(root string, declarations string, cases []OwnershipStringViewRejection, modes [][]string) ! {
	main := os.join_path(root, 'main.v')
	// Custom flags can limit diagnostics or change how a project is resolved.
	if os.getenv('VFLAGS').trim_space() !in ['', '-gc none'] {
		for case in cases {
			case_declarations := case.standalone_declarations or { declarations }
			os.write_file(main, case_declarations + 'fn main() {\n${case.body}\n}\n')!
			for mode in modes {
				out := os.exec([...ownership_string_view_rejection_args, ...mode, '-check', main])
				assert out.exit_code != 0, '${mode}: ${case.body}\n${out.output}'
				assert out.output.contains(case.expected), '${mode}: ${case.body}\n${out.output}'
			}
		}
		return
	}
	mut calls := []string{cap: cases.len}
	for i, case in cases {
		name := 'view_case_${i}'
		os.write_file(os.join_path(root, '${name}.v'), 'module main\n\nfn ${name}() {\n${case.body}\n}\n')!
		calls << '\t${name}()'
	}
	os.write_file(main, 'module main\n\n' + declarations + 'fn main() {\n${calls.join('\n')}\n}\n')!
	for mode in modes {
		out := os.exec([...ownership_string_view_rejection_args, ...mode, '-json-errors', '-check',
			root])
		assert out.exit_code != 0, '${mode}: ${out.output}'
		mut diagnostics := []OwnershipStringViewDiagnostic{}
		for line in out.output.split_into_lines() {
			if line.starts_with('{') {
				diagnostics << json2.decode[OwnershipStringViewDiagnostic](line)!
			}
		}
		for i, case in cases {
			filename := 'view_case_${i}.v'
			assert diagnostics.any(it.severity == 'error' && os.file_name(it.file) == filename
				&& it.message.contains(case.expected)), '${mode}: ${case.body}\n${out.output}'
		}
	}
}

fn test_ownership_string_views_and_returned_fallbacks_preserve_storage() {
	root := os.join_path(os.vtmp_dir(), 'ownership_string_views_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	views := 'import os
import encoding.utf8

fn read_view(value &string) int {
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
	assert read_view(&view) == 8
	assert utf8.validate_str(view.clone())
	assert view.to_owned() == "filename"
	view = view[..4]
	assert read_view(&view) == 4
	assert view == "file"
	assert text == "prefix/filename"
	path := path_view(&text)
	assert path == text
	assert unsafe { path.str != text.str }
	assert read_view(&path) == text.len
	ptr := &text
	normalized := normalize_view(*ptr)
	assert read_view(&normalized) == text.len
	assert normalized == text
	dereferenced := *ptr
	assert unsafe { dereferenced.str == text.str }
	assert read_view(&dereferenced) == text.len
	assert dereferenced == text
	unsafe_view := unsafe { text.substr_unsafe(7, text.len) }
	assert unsafe { unsafe_view.str == text.str + 7 }
	assert read_view(&unsafe_view) == 8
	assert unsafe_view == "filename"
	independent := independent_copy(ptr)
	assert unsafe { independent.str != text.str }
	assert read_view(&independent) == text.len
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
	assert read_view(&owner) == 11
	assert independent_slice == "ind"
	assert os.join_path(view, "child") == "file/child"
	assert view == "file"
	println("ok")
	CHECK_FALLBACK
}
'
	fallback := '
fn check_cloned_returned_fallback() {
${ownership_string_returned_fallback_body}
	assert result == "all"
	println("fallback")
}
'
	for mode in [['-no-parallel'], []string{}] {
		// The default-mode fixtures exercise their original string cleanup separately.
		serial := mode.len > 0
		check := if serial { '\tcheck_cloned_returned_fallback()\n' } else { '' }
		extra := if serial { fallback } else { '' }
		os.write_file(source, views.replace('\tCHECK_FALLBACK\n', check) + extra)!
		out := os.exec([@VEXE, '-nocache', '-ownership', '-d', 'ownership', '-cc', 'clang', ...mode,
			'run', source])
		assert out.exit_code == 0, '${mode}: ${out.output}'
		expected := if serial { ['ok', 'fallback'] } else { ['ok'] }
		assert out.output.trim_space().split_into_lines() == expected, out.output
	}
}

fn test_ownership_string_views_still_require_explicit_owned_copies_to_move() {
	root := os.join_path(os.vtmp_dir(), 'ownership_string_view_moves_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	mut cases := []OwnershipStringViewRejection{}
	for creation in ['"abcd".to_owned()', '"abcd"[1..].to_owned()', '(*ptr).to_owned()',
		'original.clone()'] {
		body := '
	original := "abcd".to_owned()
	ptr := &original
	owned := ${creation}
	consume(owned)
	println(owned)'
		cases << OwnershipStringViewRejection{ body: body, expected: 'use of moved value: `owned`' }
	}
	check_ownership_string_view_rejections(root, 'fn consume(value string) { _ = value }\n',
		cases, [[]string{}])!
}

fn test_ownership_string_views_keep_owner_borrow_until_view_expires() {
	root := os.join_path(os.vtmp_dir(), 'ownership_string_view_borrows_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	mut cases := []OwnershipStringViewRejection{}
	for expr in ['*ptr', 'unsafe { original.substr_unsafe(0, 2) }', 'identity(*ptr)'] {
		for invalid_action, expected in {
			'consume(original)':                   'cannot move `original` because it is borrowed'
			'original = "replacement".to_owned()': 'cannot assign to `original` because it is borrowed'
		} {
			body := '
	mut original := "abcd".to_owned()
	mut view := ""
	{
		ptr := &original
		view = ${expr}
	}
	${invalid_action}
	assert view.len > 0'
			cases << OwnershipStringViewRejection{ body: body, expected: expected }
		}
	}
	check_ownership_string_view_rejections(root, 'fn consume(value string) { _ = value }\nfn identity(value string) string { return value }\n',
		cases, [[]string{}])!
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
fn read_spread(values ...string) (int, voidptr) {
	return values[0].len, unsafe { voidptr(values[0].str) }
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
	spread_len, spread_ptr := read_spread(alias)
	assert spread_len == local.len
	assert unsafe { spread_ptr != voidptr(local.str) }
	assert alias == "second owner"
	conditional_len, conditional_ptr := read(if borrowed.len > 0 { borrowed } else { "empty" })
	assert conditional_len == local.len
	assert unsafe { conditional_ptr != voidptr(local.str) }
	assert borrowed == "second owner"
	match_len, match_ptr := read(match borrowed.len {
		0 { "empty" }
		else { borrowed }
	})
	assert match_len == local.len
	assert unsafe { match_ptr != voidptr(local.str) }
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
	slice_len, slice_ptr := read(slice.clone())
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
	_ = read_many("owned variadic".to_owned())
	owned_args := ["owned argument".to_owned()]
	array_len, _ := read_spread(...owned_args)
	assert array_len == 14'
	for first in [true, false] {
		os.write_file(source, fixture.replace('OWNED_CALLS_START', if first {
			owned_calls
		} else {
			''
		}).replace('OWNED_CALLS_END', if first { '' } else { owned_calls }))!
		for mode in ['-no-parallel', ''] {
			out := os.execute('${os.quoted_path(@VEXE)} -nocache -ownership -d ownership -cc clang ${mode} run ${os.quoted_path(source)}')
			assert out.exit_code == 0, '${mode}: ${out.output}'
			assert out.output.trim_space() == 'ok', out.output
		}
	}
}

fn test_ownership_string_calls_preserve_move_semantics() {
	root := os.join_path(os.vtmp_dir(), 'ownership_string_call_moves_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	identity := 'fn identity(value string) string { return value }\n'
	consume := 'fn consume(value string) { _ = value }\n'
	consume_many := 'fn consume_many(values ...string) int { return values.len }\n'
	first_value := 'fn first(values ...string) string { return values[0] }\n'
	user_method := 'struct Source {}
fn (source Source) substr_unsafe() string { return "independent".to_owned() }
'
	declarations := identity + consume + consume_many + first_value + user_method
	moved_result := 'use of moved value: `result`'
	moved_owned := 'use of moved value: `owned`'
	mut cases := [
		OwnershipStringViewRejection{'
	owned := identity("owned argument".to_owned())
	assert owned.len > 0
	original := "borrowed argument".to_owned()
	ptr := &original
	result := identity(*ptr)
	consume(result)
	println(result)', moved_result, identity + consume},
		OwnershipStringViewRejection{'
	owned := ["owned argument".to_owned()]
	assert consume_many(...owned) == 1
	println(owned.len)', moved_owned, consume_many},
		OwnershipStringViewRejection{'
	owned := ["owned argument".to_owned()]
	result := first(...owned)
	consume(result)
	println(result)', moved_result, first_value + consume},
		OwnershipStringViewRejection{'
	result := Source{}.substr_unsafe()
	consume(result)
	println(result)', moved_result, user_method + consume},
	]

	for first in ['owned', 'if owned.len > 0 { owned } else { "fallback" }'] {
		for next in ['consume(owned)', 'consume(if owned.len > 0 { owned } else { "fallback" })'] {
			body := '
	owned := "owned argument".to_owned()
	consume(${first})
	${next}'
			cases << OwnershipStringViewRejection{body, moved_owned, consume}
		}
	}
	check_ownership_string_view_rejections(root, declarations, cases, [
		['-no-parallel'],
		[]string{},
	])!
}

fn test_ownership_dereferenced_pointer_calls_retain_string_owner_loans() {
	root := os.join_path(os.vtmp_dir(), 'ownership_pointer_call_view_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	mut cases := []OwnershipStringViewRejection{}
	for expression in ['*pointer(&original)', '*forward(pointer(&original))'] {
		for action, diagnostic in {
			'original = "replacement".to_owned()': 'cannot assign to `original` because it is borrowed'
			'consume(original)':                   'cannot move `original` because it is borrowed'
		} {
			body := '
	mut original := "owned text".to_owned()
	view := ${expression}
	${action}
	assert view == "owned text"'
			cases << OwnershipStringViewRejection{ body: body, expected: diagnostic }
		}
	}
	check_ownership_string_view_rejections(root, 'fn pointer(value &string) &string { return value }\nfn forward(value &string) &string { return value }\nfn consume(value string) { _ = value }\n',
		cases, [['-no-parallel'], []string{}])!
}

fn test_ownership_cloned_returned_fallback_detaches_source_loan() {
	root := os.join_path(os.vtmp_dir(), 'ownership_returned_fallback_loan_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	fixture := 'fn consume(value string) { _ = value }
fn main() {
${ownership_string_returned_fallback_body}
	LAST_USE
}
'
	os.write_file(source, fixture.replace('LAST_USE', 'assert result == "all"'))!
	success := os.exec([...ownership_string_view_rejection_args, '-cc', 'clang', 'run', source])
	assert !success.output.contains('cannot assign to `original`'), success.output
	assert success.exit_code == 0, success.output
	os.write_file(source, fixture.replace('LAST_USE', 'consume(result)\n\tprintln(result)'))!
	for mode in [['-no-parallel'], []string{}] {
		out := os.exec([...ownership_string_view_rejection_args, ...mode, '-check', source])
		assert !out.output.contains('cannot assign to `original`'), out.output
		assert out.exit_code != 0, out.output
		assert out.output.contains('use of moved value: `result`'), out.output
	}
}
