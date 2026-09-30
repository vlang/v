module types

import os

fn c_char_pointer_check(name string, files map[string]string) os.Result {
	root := os.join_path(os.vtmp_dir(), 'c_char_pointer_${name}_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	for file, source in files {
		os.write_file(os.join_path(root, file), source) or { panic(err) }
	}
	return os.execute('${os.quoted_path(@VEXE)} -check ${os.quoted_path(root)}')
}

fn test_c_calls_accept_character_pointers_of_the_same_depth() {
	result := c_char_pointer_check('args', {
		'main.v': 'module main
fn C.strtol(s &char, endptr &&char, base i32) i64
fn main() {
	mut buf := [8]i8{}
	signed := unsafe { &buf[0] }
	unsigned := &u8(signed)
	mut end := &i8(unsafe { nil })
	_ = C.strlen(signed)
	_ = C.strlen(unsigned)
	_ = C.strtol(signed, &end, 10)
}
'
	})
	assert result.exit_code == 0, result.output
}

fn test_c_calls_keep_rejecting_other_pointers() {
	result := c_char_pointer_check('other', {
		'main.v': 'module main
fn main() {
	mut words := [4]i16{}
	mut buf := [8]i8{}
	signed := unsafe { &buf[0] }
	_ = C.strlen(unsafe { &words[0] })
	_ = C.strlen(&signed)
}
'
	})
	assert result.exit_code != 0, result.output
	assert result.output.contains('cannot use `&i16` as `&char` in argument 1 to `C.strlen`'), result.output
	assert result.output.contains('cannot use `&&i8` as `&char` in argument 1 to `C.strlen`'), result.output
}

fn test_translated_code_stores_character_pointer_results() {
	source := "struct Holder {
mut:
	data &i8 = unsafe { nil }
}
fn store(mut holder Holder, text &i8) {
	holder.data = C.strdup(text)
}
fn main() {
	mut holder := Holder{}
	store(mut holder, &i8(c'x'))
}
"
	translated := c_char_pointer_check('translated', {
		'main.v': '@[translated]\nmodule main\n' + source
	})
	assert translated.exit_code == 0, translated.output
	plain := c_char_pointer_check('plain', {
		'main.v': 'module main\n' + source
	})
	assert plain.exit_code != 0, plain.output
	assert plain.output.contains('cannot assign to `holder.data`: expected `&i8`, not `&char`'), plain.output
}
