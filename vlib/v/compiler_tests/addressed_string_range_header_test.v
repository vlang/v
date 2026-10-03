import os

fn test_addressed_string_range_headers_keep_their_type_and_values() {
	root := os.join_path(os.vtmp_dir(), 'addressed_string_range_header_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, '@[has_globals]
module main

__global source_calls = 0
__global start_calls = 0
__global end_calls = 0

fn tail(text &string) &string {
 return &(*text)[1..]
}

fn gated_tail(text &string) &string {
 return &(*text)#[1..]
}

fn range(text &string, start int, end int) &string {
 return &((*text)[start..end])
}

fn gated_range(text &string, start int, end int) &string {
 return &((*text)#[start..end])
}

fn counted_source(text &string) &string {
 source_calls++
 return text
}

fn counted_start() int {
 start_calls++
 return 1
}

fn counted_end() int {
 end_calls++
 return 4
}

fn disturb_stack(seed int) int {
 values := [seed, seed + 1, seed + 2, seed + 3]!
 return values[0] + values[3]
}

fn main() {
 text := $if ownership ? { "abcdef".to_owned() } $else { "abcdef".clone() }
 first := tail(text)
 second := tail(text)
 gated := gated_tail(text)
 bounded := range(text, 2, 5)
 gated_bounded := gated_range(text, -4, -1)
 empty := range(text, 2, 2)
 assert disturb_stack(8) == 19
 assert *first == "bcdef"
 assert *second == "bcdef"
 assert voidptr(first) != voidptr(second)
 assert *gated == "bcdef"
 assert *bounded == "cde"
 assert *gated_bounded == "cde"
 assert empty.len == 0
 $if ownership ? {
  assert unsafe { first.str == text.str + 1 }
  assert unsafe { bounded.str == text.str + 2 }
 }
 ordered := &(*counted_source(text))[counted_start()..counted_end()]
 assert *ordered == "bcd"
 assert source_calls == 1
 assert start_calls == 1
 assert end_calls == 1
 ordered_gated := &(*counted_source(text))#[counted_start()..counted_end()]
 assert *ordered_gated == "bcd"
 assert source_calls == 2
 assert start_calls == 2
 assert end_calls == 2
 counted_tail := &(*counted_source(text))[counted_start()..]
 assert *counted_tail == "bcdef"
 assert source_calls == 3
 assert start_calls == 3
 assert end_calls == 2
 counted_gated_tail := &(*counted_source(text))#[counted_start()..]
 assert *counted_gated_tail == "bcdef"
 assert source_calls == 4
 assert start_calls == 4
 assert end_calls == 2
 assert text == "abcdef"
}
')!
	for ownership in ['', '-ownership'] {
		for mode in ['-no-parallel', ''] {
			output := os.join_path(root, 'program_${if ownership.len > 0 {
				'ownership'
			} else {
				'regular'
			}}_${if mode.len > 0 { 'serial' } else { 'parallel' }}')
			compile := os.execute('${os.quoted_path(@VEXE)} -new-compiler -no-memory-limit -nocache -cc clang -gc none ${ownership} ${mode} -o ${os.quoted_path(output)} ${os.quoted_path(source)}')
			assert compile.exit_code == 0, '${ownership} ${mode}: ${compile.output}'
			run := os.execute(os.quoted_path(output))
			assert run.exit_code == 0, '${ownership} ${mode}: ${run.output}'
		}
	}
}

fn test_ownership_addressed_string_ranges_reject_temporary_sources() {
	root := os.join_path(os.vtmp_dir(), 'addressed_string_range_temporary_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	for expression in [
		'&"abc".repeat(3)[1..]',
		'&("abc".repeat(3)[1..])',
		'&("abc".to_owned())[1..]',
		'&("abc".clone())[1..]',
		'&make_text()[1..]',
		'&("prefix" + make_text())[1..]',
		'&"text: \${make_text()}"[1..]',
		'&"abc".repeat(3).substr_unsafe(0, 6)[1..]',
		'&(if true { make_text() } else { "literal" })[1..]',
		'&(Text(make_text()))[1..]',
		'&(maybe_text() or { "literal" })[1..]',
		'&make_holder().text[1..]',
		'&make_array()[0][1..]',
		'&([make_text()])[0][1..]',
		'&([make_text()]!)[0][1..]',
		'&(Holder{text: make_text()}).text[1..]',
		'&"abc".repeat(3)[1..][1..]',
	] {
		os.write_file(source, 'type Text = string
struct Holder { text string }
fn make_text() string { return "abcabcabc".to_owned() }
fn maybe_text() ?string { return make_text() }
fn make_holder() Holder { return Holder{text: make_text()} }
fn make_array() []string { return [make_text()] }
fn escaped() &string {
	return ${expression}
}
fn main() {
	for _ in 0 .. 3 {
		view := ${expression}
		println(*view)
	}
}
')!
		for mode in ['-no-parallel', ''] {
			// Rejection cases cannot own their backing allocation, so they are never run.
			out := os.execute('${os.quoted_path(@VEXE)} -new-compiler -no-memory-limit -nocache -ownership -gc none -cc clang ${mode} -check ${os.quoted_path(source)}')
			assert out.exit_code != 0, '${expression}: ${out.output}'
			assert out.output.contains('cannot take an addressed string range from a temporary value'), '${expression}: ${out.output}'
		}
	}
}

fn test_ownership_addressed_string_ranges_keep_retained_sources() {
	root := os.join_path(os.vtmp_dir(), 'addressed_string_range_retained_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'type Text = string
struct Holder { text string }
fn reference(text &string) &string { return text }
fn caller(text &string) &string { return &(*reference(text))[1..] }
fn literal() &string { return &"abcdef"[1..] }
fn main() {
	copied := "abc".repeat(3)[1..]
	assert copied == "bcabcabc"
	owner := "abcdef".to_owned()
	view := &owner[1..]
	assert *view == "bcdef"
	assert unsafe { view.str == owner.str + 1 }
	assert *caller(&owner) == "bcdef"
	assert *literal() == "bcdef"
	borrowed := &owner.substr_unsafe(0, 4)[1..]
	assert *borrowed == "bcd"
	assert unsafe { borrowed.str == owner.str + 1 }
	aliased := Text("abcdef".to_owned())
	assert *(&aliased[1..]) == "bcdef"
	holder := Holder{text: "abcdef".to_owned()}
	assert *(&holder.text[1..]) == "bcdef"
	texts := ["abcdef".to_owned()]
	assert *(&texts[0][1..]) == "bcdef"
	assert *(&(Text("abcdef"))[1..]) == "bcdef"
	assert owner == "abcdef"
}
')!
	for mode in ['-no-parallel', ''] {
		out := os.execute('${os.quoted_path(@VEXE)} -new-compiler -no-memory-limit -nocache -ownership -gc none -cc clang ${mode} run ${os.quoted_path(source)}')
		assert out.exit_code == 0, '${mode}: ${out.output}'
	}
}
