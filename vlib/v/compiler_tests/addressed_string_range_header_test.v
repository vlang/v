// vtest ownership
import os
import json2

const addressed_range_compiler_args = [@VEXE, '-no-memory-limit', '-nocache', '-cc', 'clang', '-gc',
	'none']

fn test_addressed_string_ranges_keep_headers_and_retained_sources() {
	root := os.join_path(os.vtmp_dir(), 'addressed_string_range_header_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	headers := '@[has_globals]
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
'
	retained := 'type Text = string
struct Holder { text string }
fn reference(text &string) &string { return text }
fn caller(text &string) &string { return &(*reference(text))[1..] }
fn literal() &string { return &"abcdef"[1..] }
fn check_retained_sources() {
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
'
	for ownership in [false, true] {
		code := if ownership {
			headers.replace_once('fn main() {\n', 'fn check_headers() {\n') + '\n' + retained + '
fn main() {
	check_headers()
	println("headers")
	check_retained_sources()
	println("retained sources")
}
'
		} else {
			headers
		}
		os.write_file(source, code)!
		kind := if ownership { 'ownership' } else { 'regular' }
		flags := if ownership { ['-ownership'] } else { []string{} }
		for mode in [['-no-parallel'], []string{}] {
			mode_name := if mode.len > 0 { 'serial' } else { 'parallel' }
			name := 'program_${kind}_${mode_name}'
			output := os.join_path(root, name + $if windows { '.exe' } $else { '' })
			compile := os.exec([...addressed_range_compiler_args, ...flags, ...mode, '-o', output,
				source])
			assert compile.exit_code == 0, '${kind} ${mode}: ${compile.output}'
			run := os.exec([output])
			assert run.exit_code == 0, '${kind} ${mode}: ${run.output}'
			if ownership {
				assert run.output.trim_space().split_into_lines() == ['headers', 'retained sources'], '${kind} ${mode}: ${run.output}'
			}
		}
	}
}

const addressed_range_rejection_error = 'cannot take an addressed string range from a temporary value'

struct AddressedRangeRejectionDiagnostic {
	file     string
	severity string
	message  string
}

fn addressed_range_rejection_fixture(expression string) string {
	return 'type Text = string
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
'
}

fn test_ownership_addressed_string_ranges_reject_temporary_sources() {
	root := os.join_path(os.vtmp_dir(), 'addressed_string_range_temporary_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	expressions := [
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
	]
	// Custom flags can change import resolution or limit diagnostics.
	if os.getenv('VFLAGS').trim_space() in ['', '-gc none'] {
		check_addressed_range_rejections_batched(root, expressions)!
		return
	}
	source := os.join_path(root, 'main.v')
	for expression in expressions {
		os.write_file(source, addressed_range_rejection_fixture(expression))!
		for mode in [['-no-parallel'], []string{}] {
			out := os.exec([...addressed_range_compiler_args, '-ownership', ...mode, '-check',
				source])
			assert out.exit_code != 0, '${expression}: ${out.output}'
			assert out.output.contains(addressed_range_rejection_error), '${expression}: ${out.output}'
		}
	}
}

fn check_addressed_range_rejections_batched(root string, expressions []string) ! {
	mut imports := []string{cap: expressions.len}
	for i, expression in expressions {
		name := 'range_case_${i}'
		dir := os.join_path(root, name)
		os.mkdir(dir)!
		os.write_file(os.join_path(dir, '${name}.v'), 'module ${name}\n\n' +
			addressed_range_rejection_fixture(expression))!
		imports << 'import ${name} as _'
	}
	os.write_file(os.join_path(root, 'main.v'), 'module main\n\n${imports.join('\n')}\n\nfn main() {}\n')!
	for mode in [['-no-parallel'], []string{}] {
		// Each fixture can report two errors, so use JSON to avoid the text diagnostic limit.
		out := os.exec([...addressed_range_compiler_args, '-ownership', ...mode, '-json-errors',
			'-check', root])
		assert out.exit_code != 0, '${mode}: ${out.output}'
		mut rejected := map[string]bool{}
		for line in out.output.split_into_lines() {
			if !line.starts_with('{') {
				continue
			}
			diagnostic := json2.decode[AddressedRangeRejectionDiagnostic](line)!
			if diagnostic.severity == 'error' && diagnostic.message.contains(addressed_range_rejection_error) {
				rejected[os.file_name(diagnostic.file)] = true
			}
		}
		for i, expression in expressions {
			assert 'range_case_${i}.v' in rejected, '${mode}: ${expression}: ${out.output}'
		}
	}
}
