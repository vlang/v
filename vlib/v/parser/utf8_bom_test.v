module parser

import os
import v.pref

fn test_leading_utf8_bom_preserves_source_and_spans() {
	path := os.join_path(os.vtmp_dir(), 'utf8_bom_${os.getpid()}.v')
	defer { os.rm(path) or {} }
	bom := '\xef\xbb\xbf'
	source := bom + "module main\n\nfn main() {\n println('hello')\n}\n"
	os.write_file(path, source)!
	for is_fmt in [false, true] {
		mut prefs := pref.new_preferences()
		prefs.is_fmt = is_fmt
		mut p := Parser.new(prefs)
		a := p.parse_file(path)
		assert p.diagnostics.len == 0, p.diagnostics.str()
		assert a.source_buffers.last() == source
		modules := a.nodes.filter(it.kind == .module_decl)
		assert modules.len == 1
		assert modules[0].value == 'main'
		assert modules[0].pos.offset == bom.len + 7
		assert source[modules[0].pos.offset..modules[0].pos.end] == 'main'
		functions := a.nodes.filter(it.kind == .fn_decl)
		assert functions.len == 1
		assert functions[0].value == 'main'
	}
}

fn test_only_leading_utf8_bom_is_ignored() {
	path := os.join_path(os.vtmp_dir(), 'utf8_bom_position_${os.getpid()}.v')
	defer { os.rm(path) or {} }
	bom := '\xef\xbb\xbf'
	for source in ['module main\n' + bom + '\nfn main() {}\n',
		bom + bom + 'module main\nfn main() {}\n'] {
		os.write_file(path, source)!
		mut p := Parser.new(pref.new_preferences())
		p.parse_file(path)
		assert p.diagnostics.any(it.message == 'invalid character `${bom}`'), p.diagnostics.str()
	}
	// Short files must not read past the buffer while checking the BOM.
	for source in ['', '\xef', '\xef\xbb', bom] {
		os.write_file(path, source)!
		assert read_source_file_raw(path)! == source
		mut p := Parser.new(pref.new_preferences())
		p.parse_file(path)
		if source == '' || source == bom {
			assert p.diagnostics.len == 0, p.diagnostics.str()
		} else {
			assert p.diagnostics.len > 0
		}
	}
}
