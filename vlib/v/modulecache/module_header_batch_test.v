module modulecache

import os
import v.flat
import v.parser
import v.pref
import v.types

fn test_batch_headers_preserve_module_boundaries_and_implicit_imports() ! {
	root := os.join_path(os.vtmp_dir(), 'v3_batch_headers_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	first := os.join_path(root, 'first.v')
	second := os.join_path(root, 'second.v')
	third := os.join_path(root, 'third.v')
	os.write_file(first, 'module first
@[inline]
pub fn identity[T](x T) T { return x }
const later = earlier + 1
')!
	os.write_file(second, 'module second
@[deprecated: "old"]
pub fn value() int { return 7 }
')!
	os.write_file(third, 'module first
const earlier = 2
pub struct Marker { value int }
')!
	mut p := parser.Parser.new(pref.new_preferences())
	mut a := p.parse_files([first, second, third])
	assert p.diagnostics.len == 0, p.diagnostics.str()
	// The resolver splices implicit imports into the stream, outside file children.
	a.add_node(flat.Node{ kind: .import_decl, value: 'sync', typ: flat.source_type_id('sync') })
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	names := ['first', 'second']
	order := ['first.earlier', 'first.later']
	headers := module_headers_with_const_order(a, &tc, names, root, map[string]string{}, order)
	assert headers.len == 2
	for name in names {
		assert headers[name] == module_header_with_const_order(a, &tc, name, root, map[string]string{}, order)
	}
	assert headers['first'].contains('@[inline]')
	assert headers['first'].contains('identity[T]')
	assert headers['first'].contains('Marker')
	assert headers['first'].contains('import sync')
	assert !headers['second'].contains('sync')
	assert !headers['first'].contains('deprecated')
	assert headers['second'].contains('deprecated')
	assert !headers['second'].contains('Marker')
	earliest := headers['first'].index('earlier') or { panic(headers['first']) }
	latest := headers['first'].index('later') or { panic(headers['first']) }
	assert earliest < latest
}

fn test_header_selects_a_symbol_that_two_files_import_once() ! {
	root := os.join_path(os.vtmp_dir(), 'v3_header_selective_imports_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	first := os.join_path(root, 'first.v')
	second := os.join_path(root, 'second.v')
	os.write_file(first, 'module joined
import strings { new_builder }
pub fn one() string { mut b := new_builder(1) return b.str() }
')!
	os.write_file(second, 'module joined
import strings as s { new_builder, Builder }
pub fn two() Builder { return new_builder(2) }
')!
	mut p := parser.Parser.new(pref.new_preferences())
	mut a := p.parse_files([first, second])
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	header := module_header_with_const_order(a, &tc, 'joined', root, map[string]string{}, []string{})
	imports := header.split_into_lines().filter(it.starts_with('import '))
	assert imports == ['import strings { new_builder }', 'import strings as s { Builder }'], header
}
