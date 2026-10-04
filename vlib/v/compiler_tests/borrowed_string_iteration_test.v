import os

fn test_borrowed_strings_iterate_bytes_and_evaluate_the_source_once() {
	root := os.join_path(os.vtmp_dir(), 'borrowed_string_iteration_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'type Text = string

struct Holder {
 text &string
}

fn has_meta(pattern &string) bool {
 for ch in pattern {
  if ch == `[` { return true }
 }
 return false
}

fn bytes(value &string) []u8 {
 mut result := []u8{}
 for i, ch in (value) {
  assert i == result.len
  result << ch
 }
 return result
}

fn alias_sum(value &Text) int {
 mut total := 0
 for ch in value { total += int(ch) }
 return total
}

fn counted(value &string, calls &int) &string {
 unsafe { *calls += 1 }
 return value
}

fn main() {
 pattern := $if ownership ? { "[abc]".to_owned() } $else { "[abc]".clone() }
 assert has_meta(pattern)
 assert bytes(pattern) == pattern.bytes()
 holder := Holder{text: &pattern}
 mut field_bytes := []u8{}
 for ch in holder.text { field_bytes << ch }
 assert field_bytes == pattern.bytes()
 empty := ""
 assert bytes(empty).len == 0
 utf8 := "é€"
 assert bytes(utf8) == [u8(0xc3), 0xa9, 0xe2, 0x82, 0xac]
 alias := Text("ab")
 assert alias[1] == `b`
 assert alias_sum(alias) == int(`a`) + int(`b`)
 mut calls := 0
 mut call_bytes := []u8{}
 for i, ch in counted(pattern, &calls) {
  assert i == call_bytes.len
  call_bytes << ch
 }
 assert calls == 1
 assert call_bytes == pattern.bytes()
 assert pattern == "[abc]"
}
')!
	for ownership in ['', '-ownership'] {
		for mode in ['-no-parallel', ''] {
			output := os.join_path(root, 'program_${if ownership.len > 0 {
				'ownership'
			} else {
				'regular'
			}}_${if mode.len > 0 { 'serial' } else { 'parallel' }}')
			compile := os.execute('${os.quoted_path(@VEXE)} -new-compiler -no-memory-limit -nocache -cc clang ${ownership} ${mode} -o ${os.quoted_path(output)} ${os.quoted_path(source)}')
			assert compile.exit_code == 0, '${ownership} ${mode}: ${compile.output}'
			run := os.execute(os.quoted_path(output))
			assert run.exit_code == 0, '${ownership} ${mode}: ${run.output}'
		}
	}
}
