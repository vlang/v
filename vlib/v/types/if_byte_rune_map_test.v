module types

import os

fn test_byte_mapper_character_branches_keep_byte_array_type() {
	root := os.join_path(os.vtmp_dir(), 'byte_mapper_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'type Byte = u8
fn identity[T](value T) T { return value }
fn main() {
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
 b := u8(0)
 mut c := if b == 0 { `ÿ` } else { b }
 assert typeof(c).name == "u8"
 assert sizeof(c) == 1
 c++
 assert c == 0
 assert identity(c) == c
 mut other := if b != 0 { b } else { `ÿ` }
 other++
 assert identity(other) == other
 value := if b == 0 { value := `ÿ`; value } else { b }
 assert typeof(value).name == "rune"
 byte_value := if b == 0 { ignored := 1; assert ignored == 1; `ÿ` } else { b }
 assert sizeof(byte_value) == 1
 assert identity(byte_value) == byte_value
 mut aliased := if b == 0 { `ÿ` } else { Byte(b) }
 assert sizeof(aliased) == 1
 aliased++
 assert identity(aliased) == aliased
}')!
	result := os.exec([@VEXE, '-no-retry-compilation', 'run', source])
	assert result.exit_code == 0, result.output
}
