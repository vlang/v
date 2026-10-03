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
		result := os.exec([@VEXE, ...(os.split_args(flags) or { panic(err) }), '-check', root])
		assert result.exit_code == 0, result.output
	}
}

fn test_translated_sizeof_cross_output_matches_selected_declarations() {
	root := os.join_path(os.vtmp_dir(), 'translated_sizeof_cross_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'main.v')
	os.write_file(path, '@[translated]
module main
fn main() {
 assert sizeof(ConstOnLinux) == expected_const_linux
 assert sizeof(ConstOnOther) == expected_const_other
 assert sizeof(GlobalOnLinux) == expected_global_linux
 assert sizeof(GlobalOnOther) == expected_global_other
}
$if linux {
 const ConstOnLinux = [2]u64{}
 type ConstOnOther = u8
 __global GlobalOnLinux = [3]u64{}
 type GlobalOnOther = u16
 const expected_const_linux = 16
 const expected_const_other = 1
 const expected_global_linux = 24
 const expected_global_other = 2
} $else {
 type ConstOnLinux = u8
 const ConstOnOther = [2]u64{}
 type GlobalOnLinux = u16
 __global GlobalOnOther = [3]u64{}
 const expected_const_linux = 1
 const expected_const_other = 16
 const expected_global_linux = 2
 const expected_global_other = 24
}
')!
	for target in ['macos', 'linux'] {
		mut prefs := pref.new_preferences()
		prefs.output_cross_c = true
		prefs.target = pref.target_from(target, pref.host_arch())!
		mut p := Parser.new(prefs)
		a := p.parse_file(path)
		assert p.diagnostics.len == 0, p.diagnostics.str()
		// Portable output keeps target guards for directives and statements, but
		// top-level declarations still select one branch before C generation.
		assert !a.nodes.any(it.kind == .comptime_if)
		sizes := a.nodes.filter(it.kind == .sizeof_expr)
		assert sizes.len == 4
		for i, name in ['ConstOnLinux', 'ConstOnOther', 'GlobalOnLinux', 'GlobalOnOther'] {
			is_value := (target == 'linux') == (i % 2 == 0)
			assert sizes[i].children_count == if is_value { 1 } else { 0 }
			if is_value {
				assert a.nodes[int(a.child(&sizes[i], 0))].value == name
				assert !a.nodes.any(it.kind == .type_decl && it.value == name)
			} else {
				assert sizes[i].value == name
				assert a.nodes.any(it.kind == .type_decl && it.value == name)
			}
		}
		out := os.join_path(root, '${target}.c')
		result := os.exec([@VEXE, '-enable-globals', '-gc', 'none', '-cross', '-os', '${target}',
			'-o', '${out}', path])
		assert result.exit_code == 0, result.output
		c_code := os.read_file(out)!
		selected := if target == 'linux' { 'Linux' } else { 'Other' }
		inactive := if target == 'linux' { 'Other' } else { 'Linux' }
		assert c_code.contains('sizeof(main__ConstOn${selected})')
		assert c_code.contains('(sizeof(GlobalOn${selected})')
		assert !c_code.contains('sizeof(main__ConstOn${inactive})')
		assert !c_code.contains('(sizeof(GlobalOn${inactive})')
		assert c_code.contains('sizeof(u8)')
		assert c_code.contains('sizeof(u16)')
	}
	result := os.exec([@VEXE, '-enable-globals', '-gc', 'none', '-os', 'cross', 'run', path])
	assert result.exit_code == 0, result.output
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
		result := os.exec([@VEXE, ...(os.split_args(flags) or { panic(err) }), '-enable-globals',
			'run', root])
		assert result.exit_code == 0, result.output
	}
}

fn test_translated_sizeof_typed_only_constants() {
	root := os.join_path(os.vtmp_dir(), 'translated_sizeof_headers_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'sizeof_headers' }")!
	os.write_file(os.join_path(root, 'header.h'), '#include <stdint.h>
#define Earlier ((uint16_t)0)
#define Foo ((intptr_t)0)
#define Grouped ((uint64_t[2]){0, 0})
#define Callback ((intptr_t (*)(intptr_t))0)
')!
	main_file := os.join_path(root, 'a.c.v')
	source := '@[translated]
module main
#include "@VMODROOT/header.h"
const Earlier u16
type Disabled = int
fn main() {
 assert sizeof(Earlier) == sizeof(u16)
 assert sizeof(Foo) == sizeof(int)
 assert sizeof(Grouped) == sizeof([2]u64)
 assert sizeof(Callback) == sizeof(fn (int) int)
 assert sizeof(Mixed) == sizeof([2]int)
 assert sizeof(Disabled) == sizeof(int)
}
'
	declarations := '
const Foo int
const (
 Grouped [2]u64
 Callback fn (int) int
 Mixed = [1, 2]!
)
@[if false]
const Disabled [2]int
'
	os.write_file(main_file, source + declarations)!
	mut p := Parser.new(pref.new_preferences())
	p.parse_file(main_file)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	sizes := p.a.nodes.filter(it.kind == .sizeof_expr)
	assert sizes.len == 12
	for i in [0, 2, 4, 6, 8] {
		assert sizes[i].children_count == 1
	}
	assert sizes[10].children_count == 0
	single := os.exec([@VEXE, 'run', main_file])
	assert single.exit_code == 0, single.output
	os.write_file(main_file, source)!
	os.write_file(os.join_path(root, 'z.c.v'), '@[translated]\nmodule main\n' + declarations)!
	for name in ['padding_a.v', 'padding_b.v'] {
		os.write_file(os.join_path(root, name), 'module main\n//' + ' '.repeat(70000) + '\n')!
	}
	for flags in ['', '-no-parallel'] {
		result := os.exec([@VEXE, ...(os.split_args(flags) or { panic(err) }), 'run', root])
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
		result := os.exec([@VEXE, ...(os.split_args(flags) or { panic(err) }), 'run', root])
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
		result := os.exec([@VEXE, ...(os.split_args(flags) or { panic(err) }), 'run', main_file])
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
		result := os.exec([@VEXE, ...(os.split_args(flags) or { panic(err) }), 'run', main_file])
		assert result.exit_code == 0, result.output
	}
}

fn test_translated_sizeof_top_level_comptime_matches() {
	// `@FILE` is the resolved path, e.g. /private/tmp instead of /tmp on macOS.
	root := os.join_path(os.real_path(os.vtmp_dir()), 'translated_sizeof_match_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	main_file := os.join_path(root, 'a.v')
	source := '@[translated]
module main
fn main() {
 assert sizeof(Regs) == sizeof([2]int)
 assert sizeof(NestedRegs) == sizeof([3]int)
 assert sizeof(Choice) == sizeof([1]int)
 assert sizeof(Fallback) == sizeof([4]int)
 assert sizeof(DeferredRegs) == sizeof([5]int)
 assert sizeof(EnabledRegs) == sizeof([2]int)
 assert sizeof(FileRegs) == sizeof([2]int)
 assert sizeof(Item) == sizeof(int)
}
'
	declarations := '
type Item = int
const chosen = "active"
$match @OS {
 "unsupported" { const Item = [1, 2]! }
 @OS {
  const enabled = true
  const Regs = [1, 2]!
  $if true {
   $match "nested" {
    "unused" { const Item = [1, 2]! }
    "nested" { const NestedRegs = [1, 2, 3]! }
   }
  }
 }
 $else {
  const Item = [1, 2]!
  fn unused() { println(@FILE) }
 }
}
$if enabled { const EnabledRegs = [1, 2]! }
$match chosen {
 "unused" { const Item = [1, 2]! }
 "active", "also" { const Choice = [1]! }
 $else { const Item = [1, 2]! }
}
$match false {
 true { const Item = [1, 2]! }
 $else { const Fallback = [1, 2, 3, 4]! }
}
$match int {
 int { const DeferredRegs = [1, 2, 3, 4, 5]! }
 $else { const OtherRegs = [6, 7]! }
}
$match @FILE {
 "__DECL_FILE__" { const FileRegs = [1, 2]! }
 $else { type FileRegs = int }
}
'
	os.write_file(main_file, source + declarations.replace('__DECL_FILE__',
		main_file.replace('\\', '\\\\').replace('"', '\\"')))!
	mut p := Parser.new(pref.new_preferences())
	p.parse_file(main_file)
	assert !p.diagnostics.any(it.severity == 'error:'), p.diagnostics.str()
	sizes := p.a.nodes.filter(it.kind == .sizeof_expr)
	assert sizes.len == 16
	for i in [0, 2, 4, 6, 8, 10] {
		assert sizes[i].children_count == 1
	}
	assert !p.translated_sizeof_const_names[p.translated_sizeof_declaration_key('Item')]
	assert p.translated_sizeof_const_names[p.translated_sizeof_declaration_key('OtherRegs')]
	single := os.exec([@VEXE, 'run', main_file])
	assert single.exit_code == 0, single.output
	os.write_file(main_file, source)!
	later_file := os.join_path(root, 'z.v')
	os.write_file(later_file, '@[translated]\nmodule main\n' + declarations.replace('__DECL_FILE__',
		later_file.replace('\\', '\\\\').replace('"', '\\"')))!
	for name in ['padding_a.v', 'padding_b.v'] {
		os.write_file(os.join_path(root, name), 'module main\n//' + ' '.repeat(70000) + '\n')!
	}
	for flags in ['', '-no-parallel'] {
		result := os.exec([@VEXE, ...(os.split_args(flags) or { panic(err) }), 'run', root])
		assert result.exit_code == 0, result.output
	}
}

fn test_translated_sizeof_location_dependent_comptime_conditions() {
	// `@FILE` is the resolved path, e.g. /private/tmp instead of /tmp on macOS.
	root := os.join_path(os.real_path(os.vtmp_dir()), 'translated_sizeof_location_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	main_file := os.join_path(root, 'a.v')
	later_file := os.join_path(root, 'z.v')
	source := '@[translated]
module main
fn main() {
 assert sizeof(FileValue) == sizeof([2]u64)
 assert sizeof(FileType) == sizeof(u8)
 assert sizeof(FunctionValue) == sizeof([3]u16)
 assert sizeof(LineValue) == sizeof([3]u32)
 assert sizeof(FollowingValue) == sizeof([4]u16)
}
'
	declarations := '
$if @FILE == "__DECL_FILE__" {
 const FileValue = [2]u64{}
 type FileType = u8
 const enabled = true
} $else {
 type FileValue = u8
 const FileType = [2]u64{}
 const enabled = false
}
$if @FN == "" {
 const FunctionValue = [3]u16{}
} $else { type FunctionValue = u8 }
$if @LINE == "__LINE__" {
 __global LineValue = [3]u32{}
} $else { type LineValue = u8 }
$if enabled {
 const FollowingValue = [4]u16{}
} $else { type FollowingValue = u8 }
'
	for name in ['padding_a.v', 'padding_b.v'] {
		os.write_file(os.join_path(root, name), 'module main\n//' + ' '.repeat(70000) + '\n')!
	}
	for later in [false, true] {
		declaration_file := if later { later_file } else { main_file }
		prefix := if later { '@[translated]\nmodule main\n' } else { source }
		mut contents := prefix + declarations.replace('__DECL_FILE__',
			declaration_file.replace('\\', '\\\\').replace('"', '\\"'))
		line_start := contents.index('\$if @LINE') or { panic('missing line condition') }
		line := contents[..line_start].count('\n') + 1
		contents = contents.replace('__LINE__', line.str())
		os.write_file(main_file, if later { source } else { contents })!
		os.write_file(later_file, if later { contents } else { 'module main\n' })!
		for flags in ['', '-no-parallel'] {
			result := os.exec([@VEXE, ...(os.split_args(flags) or { panic(err) }), '-enable-globals',
				'run', root])
			assert result.exit_code == 0, result.output
		}
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

fn test_translated_sizeof_ignores_globals_from_other_modules() {
	root := os.join_path(os.vtmp_dir(), 'translated_sizeof_global_modules_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'dependency'))!
	os.mkdir_all(os.join_path(root, 'values'))!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'sizeof_global_modules' }")!
	dependency := os.join_path(root, 'dependency', 'dependency.v')
	values := os.join_path(root, 'values', 'values.v')
	main_file := os.join_path(root, 'main.v')
	os.write_file(dependency, '@[translated]
module dependency
__global Item = [1, 2]!
__global foo = 1
pub fn touch() {}
')!
	os.write_file(values, 'module values\npub type Count = u32\n')!
	os.write_file(main_file, '@[translated]
module main
import dependency
import values as foo
type Item = int
fn main() {
 dependency.touch()
 assert sizeof(Item) == sizeof(int)
 assert sizeof(foo.Count) == sizeof(u32)
}
')!
	mut p := Parser.new(pref.new_preferences())
	p.parse_file(dependency)
	p.parse_file(values)
	p.parse_file(main_file)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	sizes := p.a.nodes.filter(it.kind == .sizeof_expr)
	assert sizes.len == 4
	assert sizes[0].value == 'Item'
	assert sizes[0].children_count == 0
	assert sizes[2].value == 'foo.Count'
	for flags in ['', '-no-parallel'] {
		result := os.exec([@VEXE, ...(os.split_args(flags) or { panic(err) }), '-enable-globals',
			'run', root])
		assert result.exit_code == 0, result.output
	}
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
 assert sizeof(c_box[int]) == sizeof(int)
 assert sizeof(c_box[[2]int]) == sizeof([2]int)
}
'
	definitions := '
struct c_record { value int }
enum c_enum { zero }
interface c_interface { value() int }
union c_union { first int second i64 }
struct c_box[T] { value T }
'
	main_file := os.join_path(root, 'a.v')
	os.write_file(main_file, use_types + definitions)!
	mut p := Parser.new(pref.new_preferences())
	p.parse_file(main_file)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	sizes := p.a.nodes.filter(it.kind == .sizeof_expr)
	assert sizes.len == 8
	assert sizes.all(it.children_count == 0)
	result := os.exec([@VEXE, 'run', main_file])
	assert result.exit_code == 0, result.output
	os.write_file(main_file, use_types)!
	os.write_file(os.join_path(root, 'z.v'), '@[translated]\nmodule main\n' + definitions)!
	for name in ['padding_a.v', 'padding_b.v'] {
		os.write_file(os.join_path(root, name), 'module main\n//' + ' '.repeat(70000) + '\n')!
	}
	for flags in ['', '-no-parallel'] {
		batch := os.exec([@VEXE, ...(os.split_args(flags) or { panic(err) }), 'run', root])
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
	result := os.exec([@VEXE, 'run', path])
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
		result := os.exec([@VEXE, ...(os.split_args(flags) or { panic(err) }), 'run', path])
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
			result := os.exec([@VEXE, ...(os.split_args(flags) or { panic(err) }), 'run', path])
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
	result := os.exec([@VEXE, 'run', path])
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
		result := os.exec([@VEXE, ...(os.split_args(flags) or { panic(err) }), 'run', root])
		assert result.exit_code == 0, result.output
	}
}

fn test_translated_sizeof_enum_member_operands() {
	root := os.join_path(os.vtmp_dir(), 'translated_sizeof_enum_members_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'definitions'))!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'sizeof_enum_members' }")!
	os.write_file(os.join_path(root, 'definitions', 'color.v'), 'module definitions
pub enum Imported as u32 { red }
pub struct Record { value i64 }
')!
	main_path := os.join_path(root, 'a.v')
	later_path := os.join_path(root, 'z.v')
	declarations := 'enum Color as u16 { red }
type Alias = Color
type color = Color
@[flag]
enum Permission { read write }
struct Record { red [2]u8 }
'
	source := '@[translated]
module main
import definitions as imported
const color_size = sizeof(Color.red)
fn shadow(color Record) { assert sizeof(color.red) == sizeof([2]u8) }
fn main() {
 assert sizeof(Color.red) == sizeof(Color)
 assert color_size == sizeof(Color)
 assert sizeof(Alias.red) == sizeof(Color)
 assert sizeof(color.red) == sizeof(Color)
 assert sizeof(Permission.read | Permission.write) == sizeof(Permission)
 assert sizeof(imported.Imported.red) == sizeof(imported.Imported)
 assert sizeof(imported.Record) == sizeof(i64)
 shadow(Record{})
}
'
	for name in ['padding_a.v', 'padding_b.v'] {
		os.write_file(os.join_path(root, name), 'module main\n//' + ' '.repeat(70000) + '\n')!
	}
	for later in [false, true] {
		os.write_file(main_path, source + if later { '' } else { declarations })!
		os.write_file(later_path, '@[translated]\nmodule main\n' + if later {
			declarations
		} else {
			''
		})!
		for flags in ['', '-no-parallel'] {
			result := os.exec([@VEXE, ...(os.split_args(flags) or { panic(err) }), 'run', root])
			assert result.exit_code == 0, result.output
		}
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
				result := os.exec([@VEXE, ...(os.split_args(flags) or { panic(err) }), 'run', path])
				assert result.exit_code == 0, result.output
			}
		}
	}
}

fn test_translated_sizeof_selects_deferred_header_constant_or_type() {
	root := os.join_path(os.vtmp_dir(), 'translated_sizeof_header_kind_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'sizeof_header_kind' }")!
	os.write_file(os.join_path(root, 'header.h'), '#include <stdint.h>
#define Item ((int64_t)0)
#define Pair ((int64_t[2]){0, 0})
')!
	path := os.join_path(root, 'a.c.v')
	later := os.join_path(root, 'z.c.v')
	for name in ['padding_a.v', 'padding_b.v'] {
		os.write_file(os.join_path(root, name), 'module main\n//' + ' '.repeat(70000) + '\n')!
	}
	for condition in ['int is $int', 'int is $float'] {
		for const_in_then in [true, false] {
			constant := 'const Item i64\nconst (\n Pair [2]i64\n)'
			alias := 'type Item = u8\ntype Pair = u16'
			then_decl := if const_in_then { constant } else { alias }
			else_decl := if const_in_then { alias } else { constant }
			selected := (condition == 'int is $int') == const_in_then
			expected_item := if selected { 'i64' } else { 'u8' }
			expected_pair := if selected { '[2]i64' } else { 'u16' }
			source := '@[translated]
module main
#include "@VMODROOT/header.h"
const chosen_size = sizeof(Item)
fn main() {
 assert sizeof(Item) == sizeof(${expected_item})
 assert chosen_size == sizeof(${expected_item})
 assert sizeof(Pair) == sizeof(${expected_pair})
}
'
			declarations := '\n\$if ${condition} {\n${then_decl}\n} \$else {\n${else_decl}\n}\n'
			os.write_file(path, source + declarations)!
			os.write_file(later, 'module main\n')!
			for flags in ['', '-no-parallel'] {
				result := os.exec([@VEXE, ...(os.split_args(flags) or { panic(err) }), 'run', root])
				assert result.exit_code == 0, result.output
			}
			os.write_file(path, source)!
			os.write_file(later, '@[translated]\nmodule main\n' + declarations)!
			for flags in ['', '-no-parallel'] {
				result := os.exec([@VEXE, ...(os.split_args(flags) or { panic(err) }), 'run', root])
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
			result := os.exec([@VEXE, ...(os.split_args(flags) or { panic(err) }), 'run', path])
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
				result := os.exec([@VEXE, ...(os.split_args(flags) or { panic(err) }), '-enable-globals',
					'run', path])
				assert result.exit_code == 0, result.output
			}
		}
	}
}

fn test_shared_sizeof_scan_preserves_deferred_sibling_constant_context() {
	root := os.join_path(os.vtmp_dir(), 'translated_sizeof_shared_context_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'sibling.v')
	os.write_file(path, 'module main\n$if enabled {\n const Choice = [2]u64{}\n} $else {\n type Choice = u8\n}\n')!
	shared := TranslatedSizeofShared.new()
	for share in [false, true, true] {
		mut p := Parser.new(pref.new_preferences())
		p.cur_module = 'main'
		if share { p.translated_sizeof_shared = shared }
		enabled_key := p.translated_sizeof_declaration_key('enabled')
		p.translated_sizeof_const_names[enabled_key] = true
		assert p.comptime_value('enabled') == none
		p.scan_translated_sizeof_sibling(path)
		choice_key := p.translated_sizeof_declaration_key('Choice')
		assert p.translated_sizeof_const_names[enabled_key]
		assert p.translated_sizeof_const_names[choice_key]
		assert p.translated_sizeof_type_names[choice_key]
	}
}
