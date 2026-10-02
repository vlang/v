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
