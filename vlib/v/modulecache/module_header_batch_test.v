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

fn test_header_const_order_keeps_stored_values_in_stable_declaration_slots() ! {
	root := os.join_path(os.vtmp_dir(), 'v3_header_const_slots_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'stable.v')
	os.write_file(source, 'module stable
pub const startup = stored_value()
pub fn stored_value() []int { return [7] }
pub const later = read_earlier() + 1
pub fn between() {}
pub const fixed = [u8(1), 2]!
const (
	mixed_store = [u8(3), 4]!
	mixed_value = 19
)
pub const earlier = 41
pub fn read_earlier() int { return earlier }
')!
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(source)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	// Rebuilding a dependency exposes helper bodies that warm headers omit.
	// Only the ranks of values whose initializers are omitted may change here.
	first_order := ['stable.startup', 'stable.earlier', 'stable.mixed_value', 'stable.fixed',
		'stable.later', 'stable.mixed_store']
	second_order := ['stable.mixed_store', 'stable.fixed', 'stable.earlier', 'stable.mixed_value',
		'stable.later', 'stable.startup']
	first := module_header_with_const_order(a, &tc, 'stable', root, map[string]string{}, first_order)
	second := module_header_with_const_order(a, &tc, 'stable', root, map[string]string{}, second_order)
	assert first == second, 'stored constant ranks changed the declaration header'
	assert header_signature(first) == header_signature(second)
	assert first.contains('pub const startup []int'), first
	assert first.contains('pub const fixed [2]u8'), first
	// A primitive expression retains its helper call in the header. Its required
	// initialization order must still survive when the helper body is omitted.
	assert first.contains('pub const later = read_earlier() + 1'), first
	earlier := first.index('pub const earlier = 41') or { panic(first) }
	later := first.index('pub const later = ') or { panic(first) }
	assert earlier < later
	assert !first.contains('return earlier'), first
	header_path := os.join_path(root, 'stable.vh')
	os.write_file(header_path, first)!
	mut warm_parser := parser.Parser.new(pref.new_preferences())
	warm_ast := warm_parser.parse_file(header_path)
	assert warm_parser.diagnostics.len == 0, warm_parser.diagnostics.str()
	mut warm_tc := types.TypeChecker.new(warm_ast)
	warm_tc.collect(warm_ast)
	warm := module_header_with_const_order(warm_ast, &warm_tc, 'stable', root,
		map[string]string{}, ['stable.earlier', 'stable.mixed_value', 'stable.later'])
	assert warm == first, 'typed cached constants changed the reserialized header'
	assert header_signature(first.replace('earlier = 41', 'earlier = 43')) != header_signature(first)
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

fn test_header_preserves_first_nominal_declarations_across_files() ! {
	root := os.join_path(os.vtmp_dir(), 'v3_header_nominal_dedup_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	first := os.join_path(root, 'first.v')
	second := os.join_path(root, 'second.v')
	os.write_file(first, 'module joined
pub struct Record { first_field int }
pub enum Choice { first_choice }
pub type Number = int
pub interface Reader { first_method() int }
')!
	os.write_file(second, 'module joined
pub struct Record { second_field string }
pub enum Choice { second_choice }
pub type Number = string
pub interface Reader { second_method() string }
')!
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_files([first, second])
	assert p.diagnostics.len == 0, p.diagnostics.str()
	tc := types.TypeChecker.new(a)
	header := module_header_with_const_order(a, &tc, 'joined', root, map[string]string{}, []string{})
	for declaration in ['struct Record', 'enum Choice', 'type Number', 'interface Reader'] {
		assert header.count(declaration) == 1, header
	}
	assert header.contains('first_field int'), header
	assert header.contains('first_choice'), header
	assert header.contains('type Number = int'), header
	assert header.contains('first_method() int'), header
	assert !header.contains('second_'), header
	assert !header.contains('type Number = string'), header
	struct_pos := header.index('struct Record') or { panic(header) }
	enum_pos := header.index('enum Choice') or { panic(header) }
	type_pos := header.index('type Number') or { panic(header) }
	interface_pos := header.index('interface Reader') or { panic(header) }
	assert struct_pos < enum_pos && enum_pos < type_pos && type_pos < interface_pos, header
}

fn test_nominal_declaration_keys_preserve_kind_and_exact_name() {
	mut seen := map[u64]bool{}
	for kind in [flat.NodeKind.struct_decl, .enum_decl, .type_decl, .interface_decl] {
		for name in ['', 'Shared', 'other.Shared'] {
			node := flat.Node{ kind: kind, value: name }
			key := nominal_decl_key(node)
			assert key != 0
			assert key !in seen
			seen[key] = true
			assert nominal_decl_key(flat.Node{ kind: kind, value: name.clone() }) == key
		}
	}
	assert nominal_decl_key(flat.Node{ kind: .fn_decl, value: 'Shared' }) == 0
	assert nominal_decl_key(flat.Node{ kind: .import_decl, value: 'Shared' }) == 0
}
