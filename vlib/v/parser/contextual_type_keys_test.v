module parser

import os
import v.flat
import v.pref
import v.token

fn test_translated_sizeof_type_keys_preserve_exact_context() {
	mut p := Parser.new(pref.new_preferences())
	name := '__contextual_sizeof_type__'
	name_id := flat.source_type_id(name)
	p.cur_file = '/tmp/contextual_sizeof/first.v'
	p.cur_module = ''
	key := p.translated_sizeof_type_key(name_id)
	context_id := flat.source_type_id('${os.dir(p.cur_file)}\x00${p.cur_module}')
	assert key == ((u64(context_id) << 32) | u64(name_id))
	p.translated_sizeof_type_names[key] = true
	assert p.translated_sizeof_has_type(name)
	p.cur_file = '/tmp/contextual_sizeof/second.v'
	assert p.translated_sizeof_type_key(name_id) == key
	assert p.translated_sizeof_has_type(name)
	p.cur_file = '/tmp/contextual_sizeof_other/first.v'
	assert p.translated_sizeof_type_key(name_id) != key
	assert !p.translated_sizeof_has_type(name)
	p.cur_file = '/tmp/contextual_sizeof/first.v'
	p.cur_module = 'main'
	main_key := p.translated_sizeof_type_key(name_id)
	assert main_key != key
	assert !p.translated_sizeof_has_type(name)
	p.cur_module = 'other'
	assert p.translated_sizeof_type_key(name_id) != key
	assert p.translated_sizeof_type_key(name_id) != main_key
	assert !p.translated_sizeof_has_type(name)
	p.cur_module = ''
	assert p.translated_sizeof_type_key(flat.source_type_id('__contextual_sizeof_other_type__')) != key
	assert !p.translated_sizeof_has_type('__contextual_sizeof_other_type__')
}

fn test_translated_sizeof_type_lookup_does_not_intern_missing_names() {
	mut p := Parser.new(pref.new_preferences())
	p.cur_file = '/tmp/contextual_sizeof_lookup/main.v'
	p.cur_module = 'main'
	missing := '__contextual_sizeof_never_declared_or_interned__'
	assert flat.source_type_existing_id(missing) == none
	assert !p.translated_sizeof_has_type(missing)
	assert flat.source_type_existing_id(missing) == none
	assert p.translated_sizeof_type_names.len == 0
}

fn test_parser_type_context_cache_releases_file_and_module_views() {
	mut p := Parser.new(pref.new_preferences())
	p.cur_file = '/tmp/contextual_sizeof_release/first.v'
	p.cur_module = 'main'
	name_id := flat.source_type_id('ContextualReleasedType')
	key := p.translated_sizeof_type_key(name_id)
	p.translated_sizeof_type_names[key] = true
	p.parse_batch_paths = [p.cur_file, '/tmp/contextual_sizeof_release/second.v']
	p.reset_file_state('/tmp/contextual_sizeof_release/second.v')
	assert p.type_declaration_context_file == ''
	assert p.type_declaration_context_module == ''
	assert p.type_declaration_context == 0
	p.cur_module = 'main'
	assert p.translated_sizeof_type_key(name_id) == key
	p.release_source_storage()
	assert p.type_declaration_context_file == ''
	assert p.type_declaration_context_module == ''
	assert p.type_declaration_context == 0
	assert p.translated_sizeof_type_names[key]
}

fn test_parsed_type_keys_keep_incremental_file_and_module_context() {
	mut p := Parser.new(pref.new_preferences())
	p.cur_file = '/tmp/contextual_parsed/lookup.v'
	p.cur_module = 'main'
	p.a.add_node(flat.Node{ kind: .file, value: '/tmp/contextual_parsed/first.v' })
	p.a.add_node(flat.Node{ kind: .module_decl, value: 'main' })
	p.a.add_node(flat.Node{ kind: .struct_decl, value: 'ParsedContextFirst' })
	assert p.parsed_module_declares_type('ParsedContextFirst')
	indexed := p.parsed_type_decls_end
	assert indexed == p.a.nodes.len
	p.a.add_node(flat.Node{ kind: .type_decl, value: 'ParsedContextSecond' })
	assert p.parsed_module_declares_type('ParsedContextSecond')
	assert p.parsed_type_decls_end == indexed + 1
	p.a.add_node(flat.Node{ kind: .file, value: '/tmp/contextual_parsed_other/other.v' })
	p.a.add_node(flat.Node{ kind: .module_decl, value: 'main' })
	p.a.add_node(flat.Node{ kind: .enum_decl, value: 'ParsedContextOtherDirectory' })
	assert !p.parsed_module_declares_type('ParsedContextOtherDirectory')
	p.cur_file = '/tmp/contextual_parsed_other/lookup.v'
	assert p.parsed_module_declares_type('ParsedContextOtherDirectory')
	assert !p.parsed_module_declares_type('ParsedContextFirst')
	p.a.add_node(flat.Node{ kind: .module_decl, value: 'other' })
	p.a.add_node(flat.Node{ kind: .interface_decl, value: 'ParsedContextOtherModule' })
	assert !p.parsed_module_declares_type('ParsedContextOtherModule')
	p.cur_module = 'other'
	assert p.parsed_module_declares_type('ParsedContextOtherModule')
	assert !p.parsed_module_declares_type('ParsedContextOtherDirectory')
}

fn test_translated_sizeof_shared_cache_retains_numeric_type_keys() {
	root := os.join_path(os.vtmp_dir(), 'contextual_sizeof_shared_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'sibling.v')
	source := 'module main\ntype ContextualSharedType = int\n'
	os.write_file(path, source)!
	shared := TranslatedSizeofShared.new()
	mut p := Parser.new(pref.new_preferences())
	p.cur_file = os.join_path(root, 'first.v')
	p.cur_module = 'main'
	p.translated_sizeof_shared = shared
	mut files := token.FileSet.new()
	file := files.add_file(p.cur_file, 0)
	p.s.init(file, '')
	p.scan_translated_sizeof_sibling(path)
	key := p.translated_sizeof_type_key(flat.source_type_id('ContextualSharedType'))
	assert p.translated_sizeof_type_names[key]
	entry := shared.files['${path}\x00main'] or { panic('missing shared sizeof entry') }
	assert entry.ready
	assert entry.types == [key]
	// A ready cache entry must supply the same numeric keys to another parser.
	mut sibling := Parser.new(pref.new_preferences())
	sibling.cur_file = os.join_path(root, 'second.v')
	sibling.cur_module = 'main'
	sibling.translated_sizeof_shared = shared
	sibling.scan_translated_sizeof_sibling(path)
	assert sibling.translated_sizeof_type_names.keys() == [key]
	assert sibling.translated_sizeof_has_type('ContextualSharedType')
}
