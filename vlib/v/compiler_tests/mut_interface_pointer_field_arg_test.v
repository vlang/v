// vtest ownership
import os

fn test_borrowed_string_iteration_and_mut_interface_pointer_field_call() {
	root := os.join_path(os.vtmp_dir(), 'mut_interface_pointer_field_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'import io

struct Source {
mut:
 pos int
}

fn (mut src Source) read(mut buf []u8) !int {
 if src.pos > 0 { return io.Eof{} }
 src.pos++
 buf[0] = `x`
 return 1
}

struct Holder {
mut:
 reader &io.Reader
}

fn consume(mut reader &io.Reader) !int {
 mut buf := []u8{len: 1}
 n := reader.read(mut buf)!
 assert buf[0] == `x`
 return n
}

fn replace(mut reader &io.Reader, replacement &io.Reader) {
 reader = replacement
}

fn (mut holder Holder) replace(replacement &io.Reader) {
 replace(mut holder.reader, replacement)
}

fn (mut holder Holder) consume() !int {
 return consume(mut holder.reader)!
}

fn main() {
 mut src := Source{}
 mut reader := io.Reader(&src)
 mut holder := Holder{reader: &reader}
 assert holder.consume()! == 1
 assert src.pos == 1
 mut second := Source{}
 mut replacement := io.Reader(&second)
 holder.replace(&replacement)
 assert voidptr(holder.reader) == voidptr(&replacement)
 assert holder.consume()! == 1
 assert src.pos == 1
 assert second.pos == 1
 check_borrowed_string_iteration()
}
')!
	os.write_file(os.join_path(root, 'string_iteration.v'), 'type Text = string

struct TextHolder {
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

fn check_borrowed_string_iteration() {
 pattern := $if ownership ? { "[abc]".to_owned() } $else { "[abc]".clone() }
 assert has_meta(pattern)
 assert bytes(pattern) == pattern.bytes()
 holder := TextHolder{text: &pattern}
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
	for mode in [['-no-parallel'], []string{}] {
		for ownership in [[]string{}, ['-ownership', '-d', 'ownership']] {
			output := os.join_path(root, 'program_${mode.len}_${ownership.len}')
			compile := os.exec([@VEXE, '-no-memory-limit', '-nocache', '-cc', 'clang', ...mode,
				...ownership, '-o', output, root])
			assert compile.exit_code == 0, '${mode} ${ownership}: ${compile.output}'
			run := os.exec([output])
			assert run.exit_code == 0, '${mode} ${ownership}: ${run.output}'
		}
	}
}
