module parser

import os
import v3.pref
import v3.token

fn test_local_type_index_keeps_nested_block_names_separate() {
	source := 'struct Top {}
fn run() {
	_ := "struct Quoted {}"
	/* union Comment {} */
	_ := Outer{}
	struct Outer {}
	if true {
		_ := Inner{}
		union Inner { value int }
	}
	callback := fn () {
		struct Captured {}
	}
}
'
	mut p := Parser.new(pref.new_preferences())
	mut fs := token.FileSet.new()
	file := fs.add_file('local_types.v', source.len)
	p.s.init(file, source)
	p.index_local_type_declarations()
	outer := (source.index('fn run() ') or { panic(err) }) + 'fn run() '.len
	inner := (source.index('if true ') or { panic(err) }) + 'if true '.len
	callback := (source.index('fn () ') or { panic(err) }) + 'fn () '.len
	assert p.local_type_decls_by_block.len == 3
	assert p.local_type_decls_by_block[outer] == ['Outer']
	assert p.local_type_decls_by_block[inner] == ['Inner']
	assert p.local_type_decls_by_block[callback] == ['Captured']
}

fn test_local_type_index_is_reset_between_files() {
	root := os.join_path(os.vtmp_dir(), 'v3_local_type_index_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	mut p := Parser.new(pref.new_preferences())
	for name in ['First', 'Other'] {
		path := os.join_path(root, '${name}.v')
		os.write_file(path, 'fn run() {\n _ := ${name}{}\n struct ${name} {}\n}\n')!
		p.parse_file(path)
		assert p.diagnostics.len == 0, p.diagnostics.str()
		assert p.local_type_decls_by_block[9] == [name]
	}
}
