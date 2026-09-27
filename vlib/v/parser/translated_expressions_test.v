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
