import os

const vexe = @VEXE

fn build(name string, source string) os.Result {
	root := os.join_path(os.vtmp_dir(), 'array_method_element_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	file := os.join_path(root, '${name}.v')
	os.write_file(file, source) or { panic(err) }
	return os.exec([vexe, '-o', os.join_path(root, name), file])
}

fn test_bytestr_and_join_need_their_element_type() {
	for name, source in {
		'runes':   'fn main() {\n\tprintln([`a`, `b`].bytestr())\n}\n'
		'mapped':  'fn main() {\n\tprintln([true].map(if it { `1` } else { `0` }).bytestr())\n}\n'
		'ints':    'fn main() {\n\tprintln([1, 2].join(","))\n}\n'
		'generic': 'fn pick[T](a []T) string {\n\treturn a.bytestr()\n}\n\nfn main() {\n\tprintln(pick([1]))\n}\n'
	} {
		result := build(name, source)
		assert result.exit_code != 0, name
		assert result.output.contains('bytestr') || result.output.contains('join'), result.output
		assert !result.output.contains('C compilation error'), result.output
	}
}

fn test_bytestr_and_join_keep_working_on_their_element_type() {
	for name, source in {
		'bytes':          'fn main() {\n\tprintln([u8(97)].bytestr())\n}\n'
		'strings':        'fn main() {\n\tprintln(["a", "b"].join(","))\n}\n'
		'alias':          'type Byte = u8\n\nfn main() {\n\tprintln([Byte(97)].map(u8(it)).bytestr())\n}\n'
		'generic_bytes':  'fn pick[T](a []T) string {\n\treturn a.bytestr()\n}\n\nfn main() {\n\tprintln(pick([u8(97)]))\n}\n'
		'generic_string': 'fn pick[T](a []T) string {\n\treturn a.join(",")\n}\n\nfn main() {\n\tprintln(pick(["a"]))\n}\n'
	} {
		result := build(name, source)
		assert result.exit_code == 0, result.output
	}
}
