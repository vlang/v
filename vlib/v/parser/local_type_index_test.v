module parser

import os
import v.flat
import v.pref
import v.token

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
	outer := (source.index('fn run() ') or { panic('missing function marker') }) + 'fn run() '.len
	inner := (source.index('if true ') or { panic('missing branch marker') }) + 'if true '.len
	callback := (source.index('fn () ') or { panic('missing callback marker') }) + 'fn () '.len
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

fn test_file_type_index_keeps_incremental_current_file_declarations() {
	mut p := Parser.new(pref.new_preferences())
	p.reset_file_state('file_type_index.v')
	for i, kind in [flat.NodeKind.struct_decl, .type_decl, .interface_decl, .enum_decl] {
		name := 'FileTypeIndex${i}'
		assert !p.file_declares_type_name(name)
		p.a.add_node(flat.Node{
			kind:  kind
			value: name
			pos:   token.Pos{ id: p.cur_file_id }
		})
		assert p.file_declares_type_name(name)
		assert p.file_type_names[flat.source_type_id(name)]
	}
	p.a.add_node(flat.Node{
		kind:  .struct_decl
		value: 'FileTypeIndexOtherFile'
		pos:   token.Pos{ id: p.cur_file_id + 1 }
	})
	assert !p.file_declares_type_name('FileTypeIndexOtherFile')
	p.reset_file_state('file_type_index_next.v')
	assert !p.file_declares_type_name('FileTypeIndex0')
	assert p.file_type_names.len == 0
}

fn test_file_type_index_does_not_intern_failed_lookup() {
	mut p := Parser.new(pref.new_preferences())
	missing := '__file_type_index_never_declared_or_interned__'
	if _ := flat.source_type_existing_id(missing) {
		assert false, 'failed lookup fixture was already interned'
	}
	assert !p.file_declares_type_name(missing)
	if _ := flat.source_type_existing_id(missing) {
		assert false, 'failed type-name lookup retained its spelling'
	}
}
