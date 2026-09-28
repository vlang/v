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
