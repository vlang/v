module types

import os

fn c_char_pointer_check(name string, files map[string]string) os.Result {
	root := os.join_path(os.vtmp_dir(), 'c_char_pointer_${name}_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	for file, source in files {
		os.write_file(os.join_path(root, file), source) or { panic(err) }
	}
	return os.exec([@VEXE, '-new-compiler', '-check', root])
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

fn test_translated_character_pointers_in_typed_value_contexts() {
	source := "type SignedChars = &i8
struct Holder { data SignedChars }
fn duplicate(text &i8) &i8 { return C.strdup(text) }
fn accept(text &i8) { _ = text }
fn accept_pointer_slot(slot &&i8) { _ = slot }
fn main() {
	text := C.strdup(c'x')
	holder := Holder{data: text}
	accept(text)
	accept_pointer_slot(&text)
	values := [&i8(c'x'), text]
	mut appended := []&i8{}
	appended << text
	mapping := {'x': &i8(c'x'), 'y': text}
	_ = duplicate(&i8(c'x'))
	_ = holder
	_ = values
	_ = mapping
}
"
	translated := c_char_pointer_check('translated_values', {
		'main.c.v': '@[translated]\nmodule main\n' + source
	})
	assert translated.exit_code == 0, translated.output
	plain := c_char_pointer_check('plain_values', {
		'translated.v': '@[translated]\nmodule main\nfn translated() {}\n'
		'main.c.v':     'module main\n' + source
	})
	assert plain.exit_code != 0, plain.output
	assert plain.output.contains('cannot use `&char` as type `&i8` in return argument'), plain.output
	assert plain.output.contains('field `data`: expected `SignedChars`, not `&char`'), plain.output
	assert plain.output.contains('cannot use `&char` as `&i8` in argument 1 to `accept`'), plain.output
}

fn test_translated_character_pointer_values_keep_depth_and_base_checks() {
	result := c_char_pointer_check('translated_other_values', {
		'main.v': '@[translated]
module main
fn different_depth(text &char) &&i8 { return text }
fn different_base(text &char) &i16 { return text }
fn different_wrapper(text ?&char) ?&i8 { return text }
fn main() {}
'
	})
	assert result.exit_code != 0, result.output
	assert result.output.contains('cannot use `&char` as type `&&i8` in return argument'), result.output
	assert result.output.contains('cannot use `&char` as type `&i16` in return argument'), result.output
	assert result.output.contains('cannot use `?&char` as type `?&i8` in return argument'), result.output
}
