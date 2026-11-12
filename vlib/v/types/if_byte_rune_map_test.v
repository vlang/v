module types

import os

fn test_byte_mapper_character_branches_keep_byte_array_type() {
	root := os.join_path(os.vtmp_dir(), 'byte_mapper_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'fn main() {
 name := "hello-world".bytes().map(if it.is_alnum() || it == `_` { it } else { `_` }).bytestr()
 assert name == "hello_world"
 reverse := "hello-world".bytes().map(if it == `-` { `_` } else { it }).bytestr()
 assert reverse == "hello_world"
 runes := [rune(0x100)].map(if it == 0x100 { it } else { `x` })
 assert typeof(runes).name == "[]rune"
 assert runes[0] == 0x100
 wide := "x".bytes().map(if it == `x` { it } else { `Ā` })
 assert typeof(wide).name == "[]rune"
 explicit := rune(0x100)
 mixed := "x".bytes().map(if it == `x` { it } else { explicit })
 assert typeof(mixed).name == "[]rune"
}')!
	result := os.exec([@VEXE, '-no-retry-compilation', 'run', source])
	assert result.exit_code == 0, result.output
}
