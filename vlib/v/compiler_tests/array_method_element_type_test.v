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
		'runes':              'fn main() {\n\tprintln([`a`, `b`].bytestr())\n}\n'
		'mapped':             'fn main() {\n\tprintln([true].map(if it { `1` } else { `0` }).bytestr())\n}\n'
		'ints':               'fn main() {\n\tprintln([1, 2].join(","))\n}\n'
		'generic':            'fn pick[T](a []T) string {\n\treturn a.bytestr()\n}\n\nfn main() {\n\tprintln(pick([1]))\n}\n'
		'generic_join':       'fn pick[T](a []T) string {\n\treturn a.join(",")\n}\n\nfn main() {\n\tprintln(pick([1]))\n}\n'
		'generic_join_array': 'fn pick[T](a T) string {\n\treturn a.join(",")\n}\n\nfn main() {\n\tprintln(pick([1, 2]))\n}\n'
		'generic_join_field': 'struct S[T] {\n\titems []T\n}\n\nfn (s S[T]) all() string {\n\treturn s.items.join(",")\n}\n\nfn main() {\n\tprintln(S[int]{\n\t\titems: [1]\n\t}.all())\n}\n'
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
		'generic_alias':  'type Str = string\n\nfn pick[T](a []T) string {\n\treturn a.join(",")\n}\n\nfn main() {\n\tprintln(pick([Str("a")]))\n}\n'
	} {
		result := build(name, source)
		assert result.exit_code == 0, result.output
	}
}

fn test_generic_join_calls_the_method_of_an_array_alias() {
	source := 'type Ints = []int\n\nfn (a Ints) join(sep string) string {\n\treturn "ints" + sep\n}\n\nfn pick[T](a T) string {\n\treturn a.join(",")\n}\n\nfn main() {\n\tprintln(pick(Ints([1])))\n}\n'
	result := build('generic_alias_method', source)
	assert result.exit_code == 0, result.output
	root := os.join_path(os.vtmp_dir(), 'array_method_element_${os.getpid()}')
	run := os.exec([os.join_path(root, 'generic_alias_method')])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ints,'
}

type JoinValue = f64 | int | string

fn join_identity[T](a []T) string {
	return a.map(it).join('')
}

fn join_identity_variadic[T](args ...T) string {
	return args.map(it).join('')
}

fn join_strings[T](a []T) string {
	return a.map(it.str()).join(',')
}

fn join_strings_variadic[T](args ...T) string {
	return args.map(it.str()).join(',')
}

fn test_generic_map_join_uses_the_mapped_element_type() {
	assert join_identity(['a', 'b']) == 'ab'
	assert join_identity_variadic('a', 'b') == 'ab'
	assert join_strings([1, 2]) == '1,2'
	assert join_strings([1.5, 2.5]) == '1.5,2.5'
	assert join_strings(['a', 'b']) == 'a,b'
	assert join_strings([JoinValue(1), JoinValue(1.5), JoinValue('a')]) == "JoinValue(1),JoinValue(1.5),JoinValue('a')"
	assert join_strings_variadic(1, 2) == '1,2'
	assert join_strings_variadic(1.5, 2.5) == '1.5,2.5'
	assert join_strings_variadic('a', 'b') == 'a,b'
	assert join_strings_variadic[JoinValue](1, 1.5, 'a') == "JoinValue(1),JoinValue(1.5),JoinValue('a')"
}
