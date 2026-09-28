module parser

import os
import v.pref

fn test_translated_expressions_and_file_scope() {
	root := os.join_path(os.vtmp_dir(), 'translated_expressions_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	mut p := Parser.new(pref.new_preferences())
	source := 'const translated_regs = [3, 12, 13]!
fn main() {
 value := true
 if if value { true } else { false } { println(13 - -5) }
 regs := [3, 12, 13]!
 _ = sizeof(translated_regs) / sizeof(translated_regs[0])
 i := 0
 if !value {
  for i = 0; i < sizeof(regs) / sizeof(regs[0]); i++ { println(regs[i]) }
 } else { println(0) }
 mut values := [1, 2]
 ptr := &values[0]
 dst := &values[1]
 ch := *ptr++
 *dst++ = ch
 *dst++ += ch
 result := i++
  + 2
 _ = result
}
'
	translated := os.join_path(root, 'translated.v')
	os.write_file(translated, '@[translated]\nmodule main\n' + source)!
	p.parse_file(translated)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	ordinary := os.join_path(root, 'ordinary.v')
	os.write_file(ordinary, 'module main\nfn main() {\nif if true {}\nprintln(13 - -5)\n}\n')!
	p.parse_file(ordinary)
	assert p.diagnostics.any(it.message.contains('did you write `if` twice'))
	assert p.diagnostics.any(it.message == 'invalid expression: unexpected token `-`')
}

fn test_translated_sizeof_constant_in_later_file() {
	root := os.join_path(os.vtmp_dir(), 'translated_sizeof_later_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'a.v'), '@[translated]
module main
fn main() {
 assert sizeof(later_regs) == sizeof([3]int)
 assert sizeof(LaterRegs) == sizeof([3]int)
 assert sizeof(LocalRegs) == sizeof([3]int)
 assert sizeof(my_type) == sizeof(int)
}

type my_type = int
const LocalRegs = [3, 12, 13]!
')!
	os.write_file(os.join_path(root, 'z.v'), '@[translated]
module main
const later_regs = [3, 12, 13]!
const LaterRegs = [3, 12, 13]!
')!
	// Cross the dispatch threshold so the first pass also exercises worker batches.
	for name in ['padding_a.v', 'padding_b.v'] {
		os.write_file(os.join_path(root, name), 'module main\n//' + ' '.repeat(70000) + '\n')!
	}
	for flags in ['', '-no-parallel'] {
		result := os.execute('${os.quoted_path(@VEXE)} ${flags} -check ${os.quoted_path(root)}')
		assert result.exit_code == 0, result.output
	}
}

fn test_translated_sizeof_globals_declared_after_use() {
	root := os.join_path(os.vtmp_dir(), 'translated_sizeof_globals_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'a.v'), '@[translated]
module main
fn main() {
 assert sizeof(LocalRegs[0]) == sizeof(int)
 assert sizeof(LocalRegs) == sizeof([2]int)
 assert sizeof(LaterRegs[0]) == sizeof(int)
 assert sizeof(LaterRegs) == sizeof([3]int)
 assert sizeof(GroupedRegs[0][0]) == sizeof(int)
 assert sizeof(GroupedRegs[0]) == sizeof([2]int)
 assert sizeof(ConditionalRegs[0]) == sizeof(int)
 assert sizeof(DisabledRegs) == sizeof(int)
}
__global LocalRegs = [1, 2]!
type DisabledRegs = int
@[if false]
__global DisabledRegs = [1, 2]!
')!
	os.write_file(os.join_path(root, 'z.v'), '@[translated]
module main
__global LaterRegs = [1, 2, 3]!
__global (
 UnusedValue = 3
 GroupedRegs [1][2]int = [[1, 2]!]!
)
$if feature ? {
 __global ConditionalRegs = [1, 2]!
} $else {
 __global (
  ConditionalRegs [3]int = [1, 2, 3]!
 )
}
')!
	for name in ['padding_a.v', 'padding_b.v'] {
		os.write_file(os.join_path(root, name), 'module main\n//' + ' '.repeat(70000) + '\n')!
	}
	for flags in ['', '-d feature', '-no-parallel', '-no-parallel -d feature'] {
		result := os.execute('${os.quoted_path(@VEXE)} ${flags} -enable-globals run ${os.quoted_path(root)}')
		assert result.exit_code == 0, result.output
	}
}

fn test_translated_sizeof_ignores_excluded_sibling_files() {
	root := os.join_path(os.vtmp_dir(), 'translated_sizeof_selected_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), '@[translated]\nmodule main\nfn main() { assert sizeof(Item) > 0 }\n')!
	os.write_file(os.join_path(root, 'item_notd_feature.v'), '@[translated]\nmodule main\ntype Item = int\n')!
	os.write_file(os.join_path(root, 'item_d_feature.v'), '@[translated]\nmodule main\nconst Item = [3, 4]!\n')!
	for flags in ['', '-d feature'] {
		result := os.execute('${os.quoted_path(@VEXE)} ${flags} run ${os.quoted_path(root)}')
		assert result.exit_code == 0, result.output
	}
}

fn test_translated_sizeof_ignores_unparsed_siblings() {
	root := os.join_path(os.vtmp_dir(), 'translated_sizeof_single_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	main_file := os.join_path(root, 'main.v')
	os.write_file(main_file, '@[translated]\nmodule main\ntype Item = int\nfn main() { assert sizeof(Item) == sizeof(int) }\n')!
	os.write_file(os.join_path(root, 'unused.v'), '@[translated]\nmodule main\nconst Item = [3, 4]!\n')!
	for flags in ['', '-no-parallel'] {
		result := os.execute('${os.quoted_path(@VEXE)} ${flags} run ${os.quoted_path(main_file)}')
		assert result.exit_code == 0, result.output
	}
}

fn test_translated_sizeof_later_active_comptime_constants() {
	root := os.join_path(os.vtmp_dir(), 'translated_sizeof_comptime_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	main_file := os.join_path(root, 'main.v')
	os.write_file(main_file, '@[translated]
module main
type Item = int
fn main() {
 assert sizeof(Regs) == sizeof([2]int)
 assert sizeof(NestedRegs) == sizeof([3]int)
 assert sizeof(Item) == sizeof(int)
}
$if feature ? {
 const Regs = [1, 2]!
 $if true {
  const NestedRegs = [1, 2, 3]!
 }
} $else $if false {
 const Item = [1, 2]!
} $else {
 const Regs = [3, 4]!
 $if false {
  const Item = [1, 2]!
 } $else {
  const NestedRegs = [4, 5, 6]!
 }
}
')!
	for flags in ['', '-d feature', '-no-parallel', '-no-parallel -d feature'] {
		result := os.execute('${os.quoted_path(@VEXE)} ${flags} run ${os.quoted_path(main_file)}')
		assert result.exit_code == 0, result.output
	}
}

fn test_translated_sizeof_ignores_constants_from_other_modules() {
	root := os.join_path(os.vtmp_dir(), 'translated_sizeof_modules_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	dependency := os.join_path(root, 'dependency.v')
	main_file := os.join_path(root, 'main.v')
	os.write_file(dependency, 'module dependency\npub const item = [1, 2]!\n')!
	os.write_file(main_file, '@[translated]\nmodule main\ntype item = int\nfn main() { assert sizeof(item) == sizeof(int) }\n')!
	mut p := Parser.new(pref.new_preferences())
	p.parse_file(dependency)
	p.parse_file(main_file)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	sizes := p.a.nodes.filter(it.kind == .sizeof_expr)
	assert sizes.len == 2
	assert sizes[0].value == 'item'
	assert sizes[0].children_count == 0
}

fn test_translated_sizeof_later_lowercase_type_declarations() {
	root := os.join_path(os.vtmp_dir(), 'translated_sizeof_lowercase_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	use_types := '@[translated]
module main
fn main() {
 assert sizeof(c_record) > 0
 assert sizeof(c_enum) > 0
 assert sizeof(c_interface) > 0
 assert sizeof(c_union) > 0
}
'
	definitions := '
struct c_record { value int }
enum c_enum { zero }
interface c_interface { value() int }
union c_union { first int second i64 }
'
	main_file := os.join_path(root, 'a.v')
	os.write_file(main_file, use_types + definitions)!
	mut p := Parser.new(pref.new_preferences())
	p.parse_file(main_file)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	sizes := p.a.nodes.filter(it.kind == .sizeof_expr)
	assert sizes.len == 4
	assert sizes.all(it.children_count == 0)
	result := os.execute('${os.quoted_path(@VEXE)} run ${os.quoted_path(main_file)}')
	assert result.exit_code == 0, result.output
	os.write_file(main_file, use_types)!
	os.write_file(os.join_path(root, 'z.v'), '@[translated]\nmodule main\n' + definitions)!
	for name in ['padding_a.v', 'padding_b.v'] {
		os.write_file(os.join_path(root, name), 'module main\n//' + ' '.repeat(70000) + '\n')!
	}
	for flags in ['', '-no-parallel'] {
		batch := os.execute('${os.quoted_path(@VEXE)} ${flags} run ${os.quoted_path(root)}')
		assert batch.exit_code == 0, batch.output
	}
}

fn test_translated_postfix_before_complex_dereference_assignments() {
	root := os.join_path(os.vtmp_dir(), 'translated_deref_assignment_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	for target in ['*state.dst++', '*state.dst[0]++', '*(state.dst)++', '*targets[0]++',
		'*get_target()++', '*(*cursor)++', '*dst'] {
		main_file := os.join_path(root, 'main.v')
		os.write_file(main_file, '@[translated]\nmodule main\nfn main() {\nch := *src++\n${target} = ch\n}\n')!
		mut p := Parser.new(pref.new_preferences())
		p.parse_file(main_file)
		assert p.diagnostics.len == 0, '${target}: ${p.diagnostics}'
	}
}

fn test_translated_sizeof_function_local_types_keep_their_scope() {
	root := os.join_path(os.vtmp_dir(), 'translated_sizeof_local_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'main.v')
	os.write_file(path, '@[translated]
module main
const c_record = [1, 2]!
fn main() {
 assert sizeof(c_record) > 0
 struct c_record { value int }
 assert sizeof(c_record) > 0
 if true {
  assert sizeof(c_union) > 0
  union c_union { first int second i64 }
 }
 other()
}
fn other() {
 c_union := [1, 2]!
 assert sizeof(c_union) == sizeof([2]int)
 assert sizeof(c_record) == sizeof([2]int)
}
')!
	mut p := Parser.new(pref.new_preferences())
	p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	sizes := p.a.nodes.filter(it.kind == .sizeof_expr)
	assert sizes[0].value.contains('c_record@local@')
	assert sizes[1].value == sizes[0].value
	assert sizes[2].value.contains('c_union@local@')
	assert sizes[3].children_count == 1
	assert sizes[5].children_count == 1
	result := os.execute('${os.quoted_path(@VEXE)} run ${os.quoted_path(path)}')
	assert result.exit_code == 0, result.output
}

fn test_translated_sizeof_declaration_cache_is_scoped_and_refreshed() {
	root := os.join_path(os.vtmp_dir(), 'translated_sizeof_cache_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	mut p := Parser.new(pref.new_preferences())
	for module_index in 0 .. 2 {
		dir := os.join_path(root, 'module_${module_index}')
		os.mkdir_all(dir)!
		mut paths := []string{}
		for i in 0 .. 8 {
			path := os.join_path(dir, 'use_${i}.v')
			os.write_file(path, '@[translated]\nmodule main\nfn use_${i}() { _ = sizeof(c_value) }\n')!
			paths << path
		}
		declaration := os.join_path(dir, 'declaration.v')
		paths << declaration
		for is_type in [true, false] {
			os.write_file(declaration, if is_type {
				'module main\nstruct c_value { value int }\n'
			} else {
				'module main\nconst c_value = 7\n'
			})!
			start := p.a.nodes.len
			p.parse_files(paths)
			assert p.diagnostics.len == 0, p.diagnostics.str()
			sizes := p.a.nodes[start..].filter(it.kind == .sizeof_expr)
			assert sizes.len == 8
			assert sizes.all((it.children_count == 0) == is_type)
			assert p.translated_sizeof_scanned_modules.len == 1
		}
	}
}

fn test_translated_sizeof_filters_conditional_declaration_attributes() {
	root := os.join_path(os.vtmp_dir(), 'translated_sizeof_attributes_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'main.v')
	os.write_file(path, '@[translated]
module main
fn main() {
 $if feature ? {
  assert sizeof(Item) == sizeof([2]int)
 } $else {
  assert sizeof(Item) == sizeof(int)
 }
}
@[if feature ?]
const Item = [1, 2]!
@[if !feature ?]
type Item = int
')!
	for flags in ['', '-d feature', '-no-parallel', '-no-parallel -d feature'] {
		result := os.execute('${os.quoted_path(@VEXE)} ${flags} run ${os.quoted_path(path)}')
		assert result.exit_code == 0, result.output
	}
}

fn test_translated_sizeof_indexes_constants_in_deferred_metadata_branches() {
	root := os.join_path(os.vtmp_dir(), 'translated_sizeof_deferred_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'main.v')
	for condition in ['sizeof(int) > 0', 'int is $int'] {
		os.write_file(path, '@[translated]
module main
fn main() {
 assert sizeof(Regs) == sizeof([2]int)
 assert sizeof(Item) == sizeof(int)
 assert sizeof(lower_item) == sizeof(int)
}
$if ${condition} {
 const Regs = [1, 2]!
 type Item = int
 type lower_item = int
} $else {
 const OtherRegs = [1, 2, 3]!
 const Item = [1, 2, 3]!
 const lower_item = [1, 2, 3]!
}
')!
		for flags in ['', '-no-parallel'] {
			result := os.execute('${os.quoted_path(@VEXE)} ${flags} run ${os.quoted_path(path)}')
			assert result.exit_code == 0, result.output
		}
	}
	os.write_file(path, '@[translated]
module main
fn main() { assert sizeof(Regs) == sizeof([2]int) }
$if sizeof(int) == 0 {
 const OtherRegs = [1, 2, 3]!
} $else {
 const Regs = [1, 2]!
}
')!
	result := os.execute('${os.quoted_path(@VEXE)} run ${os.quoted_path(path)}')
	assert result.exit_code == 0, result.output
}

fn test_translated_sizeof_qualified_constants_and_types() {
	root := os.join_path(os.vtmp_dir(), 'translated_sizeof_qualified_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'values'))!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'sizeof_qualified' }")!
	os.write_file(os.join_path(root, 'values', 'values.v'), '@[translated]
module values
pub const regs = [1, 2]!
pub const Regs = [1, 2, 3]!
pub type count = u16
pub type Count = u32
pub struct Box[T] { value T }
$if int is $int { pub const Chosen = [1, 2]! } $else { pub type Chosen = int }
')!
	os.write_file(os.join_path(root, 'main.v'), '@[translated]
module main
import values as foo
const count_bytes = sizeof(foo.Count)
const regs_bytes = sizeof(foo.regs)
fn main() {
	_ = foo.Box[u64]{}
 assert sizeof(foo.regs) == sizeof([2]int)
 assert sizeof(foo.Regs) == sizeof([3]int)
 assert sizeof(foo.Chosen) == sizeof([2]int)
 assert sizeof(foo.regs[0]) == sizeof(int)
 assert sizeof(foo.count) == sizeof(u16)
 assert sizeof(foo.Count) == sizeof(u32)
 assert sizeof(foo.Box[u64]) == sizeof(u64)
 assert count_bytes == sizeof(u32)
 assert regs_bytes == sizeof([2]int)
}

')!
	for flags in ['', '-no-parallel'] {
		result := os.execute('${os.quoted_path(@VEXE)} ${flags} run ${os.quoted_path(root)}')
		assert result.exit_code == 0, result.output
	}
}

fn test_translated_sizeof_selects_deferred_constant_or_type() {
	root := os.join_path(os.vtmp_dir(), 'translated_sizeof_selected_kind_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'main.v')
	for condition in ['int is $int', 'int is $float'] {
		for const_in_then in [true, false] {
			then_decl := if const_in_then { 'const Item = [1, 2]!' } else { 'type Item = int' }
			else_decl := if const_in_then { 'type Item = int' } else { 'const Item = [1, 2]!' }
			expected := if (condition == 'int is $int') == const_in_then {
				'[2]int'
			} else {
				'int'
			}
			os.write_file(path, '@[translated]
module main
const chosen_size = sizeof(Item)
fn check(int string) { assert sizeof(Item) == chosen_size; assert sizeof(int) == sizeof(string) }
fn main() {
 assert sizeof(Item) == sizeof(${expected})
 assert chosen_size == sizeof(${expected})
 check("")
}
$if ${condition} { ${then_decl} } $else { ${else_decl} }
')!
			for flags in ['', '-no-parallel'] {
				result := os.execute('${os.quoted_path(@VEXE)} ${flags} run ${os.quoted_path(path)}')
				assert result.exit_code == 0, result.output
			}
		}
	}
}

fn test_translated_sizeof_deferred_constant_compound_operands() {
	root := os.join_path(os.vtmp_dir(), 'translated_sizeof_compound_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'main.v')
	for condition in ['int is $int', 'int is $float'] {
		then_decl := if condition == 'int is $int' {
			'const Item = i64(1)'
		} else {
			'type Item = u8'
		}
		else_decl := if condition == 'int is $int' {
			'type Item = u8'
		} else {
			'const Item = i64(1)'
		}
		os.write_file(path, '@[translated]
module main
fn main() {
 assert sizeof(Item + 0) == sizeof(i64)
 assert sizeof(Item * 2) == sizeof(i64)
}
$if ${condition} { ${then_decl} } $else { ${else_decl} }
')!
		for flags in ['', '-no-parallel'] {
			result := os.execute('${os.quoted_path(@VEXE)} ${flags} run ${os.quoted_path(path)}')
			assert result.exit_code == 0, result.output
		}
	}
}

fn test_translated_sizeof_selects_deferred_global_or_type() {
	root := os.join_path(os.vtmp_dir(), 'translated_sizeof_global_kind_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'main.v')
	for condition in ['int is $int', 'int is $float'] {
		for global_in_then in [true, false] {
			then_decl := if global_in_then { '__global Item = [2]i64{}' } else { 'type Item = u8' }
			else_decl := if global_in_then { 'type Item = u8' } else { '__global Item = [2]i64{}' }
			expected := if (condition == 'int is $int') == global_in_then { '[2]i64' } else { 'u8' }
			os.write_file(path, '@[translated]
module main
const chosen_size = sizeof(Item)
fn check(int string) { assert sizeof(Item) == chosen_size; assert sizeof(int) == sizeof(string) }
fn main() {
 assert sizeof(Item) == sizeof(${expected})
 assert chosen_size == sizeof(${expected})
 check("")
}
$if ${condition} { ${then_decl} } $else { ${else_decl} }
')!
			for flags in ['', '-no-parallel'] {
				result := os.execute('${os.quoted_path(@VEXE)} ${flags} -enable-globals run ${os.quoted_path(path)}')
				assert result.exit_code == 0, result.output
			}
		}
	}
}
