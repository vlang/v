module types

import os
import v.parser
import v.pref

const private_field_lib_source = 'module lib

pub struct Inner {
	hidden int
pub:
	shown int
}

pub struct Box {
	Inner
	private int
	cb      fn () int = default_cb
mut:
	secret int
pub:
	readable int
pub mut:
	writable int
}

pub type BoxAlias = Box

pub type BoxRef = &Box

fn default_cb() int {
	return 1
}

pub fn new_box() Box {
	return Box{}
}

pub fn (b Box) sum() int {
	return b.private + b.secret + b.readable + b.writable + b.hidden + b.shown
}
'

const private_field_main_source = 'module main

import lib

struct Local {
	lib.Inner
	own int
}

type LocalBox = lib.Box

fn main() {
	mut b := lib.new_box()
	println(b.private)
	println(b.secret)
	b.secret = 1
	println(b.hidden)
	ptr := &b
	println(ptr.secret)
	cb := b.cb
	alias := lib.BoxAlias(b)
	println(alias.secret)
	println(b.readable + b.writable + b.shown + b.sum() + b.cb() + cb())
	b.writable = 2
	local := Local{}
	println(local.own + local.hidden + local.shown)
	local_box := LocalBox(b)
	println(local_box.readable)
	ref := unsafe { lib.BoxRef(&b) }
	println(ref.secret + ref.readable)
}
'

fn private_field_errors() ![]string {
	root := os.join_path(os.vtmp_dir(), 'v3_private_field_visibility_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'lib'))!
	defer {
		os.rmdir_all(root) or {}
	}
	lib_path := os.join_path(root, 'lib', 'lib.v')
	main_path := os.join_path(root, 'main.v')
	os.write_file(lib_path, private_field_lib_source)!
	os.write_file(main_path, private_field_main_source)!
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_files([lib_path, main_path])
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	tc.check_semantics_opt(false)
	mut errors := []string{}
	for err in tc.errors {
		source := if err.file.ends_with('main.v') {
			private_field_main_source
		} else {
			private_field_lib_source
		}
		line := source[..err.pos.offset].count('\n') + 1
		errors << '${os.file_name(err.file)}:${line}: ${err.msg}'
	}
	return errors
}

// https://github.com/vlang/v/issues/28827
fn test_private_struct_fields_of_other_modules_are_not_public() {
	errors := private_field_errors()!
	assert errors == [
		'main.v:14: field `lib.Box.private` is not public',
		'main.v:15: field `lib.Box.secret` is not public',
		'main.v:16: field `lib.Box.secret` is not public',
		'main.v:17: field `lib.Box.hidden` is not public',
		'main.v:19: field `lib.Box.secret` is not public',
		'main.v:20: field `lib.Box.cb` is not public',
		'main.v:22: field `lib.BoxAlias.secret` is not public',
		'main.v:30: field `lib.BoxRef.secret` is not public',
	], errors.str()
}
