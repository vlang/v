import os

fn test_mut_interface_pointer_field_call_keeps_the_pointer_slot() {
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
}
')!
	for mode in ['-no-parallel', ''] {
		for ownership in ['', '-ownership -d ownership'] {
			output := os.join_path(root, 'program_${if mode.len > 0 { 'serial' } else { 'parallel' }}_${if ownership.len > 0 {
				'ownership'
			} else {
				'ordinary'
			}}')
			compile := os.execute('${os.quoted_path(@VEXE)} -new-compiler -no-memory-limit -nocache -cc clang ${mode} ${ownership} -o ${os.quoted_path(output)} ${os.quoted_path(source)}')
			assert compile.exit_code == 0, '${mode} ${ownership}: ${compile.output}'
			run := os.execute(os.quoted_path(output))
			assert run.exit_code == 0, '${mode} ${ownership}: ${run.output}'
		}
	}
}
