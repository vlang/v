module c

import os

fn test_translated_typed_only_constants_use_header_symbols_for_size_and_values() {
	root := os.join_path(os.vtmp_dir(), 'translated_const_symbols_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'translated_const_symbols' }")!
	os.write_file(os.join_path(root, 'header.h'), '#include <stdint.h>
#define Earlier ((uint16_t)7)
#define Later ((uint64_t)11)
#define Grouped ((uint64_t[2]){3, 4})
#define Callback ((intptr_t (*)(intptr_t))0)
')!
	source := os.join_path(root, 'main.c.v')
	os.write_file(source, '@[translated]
module main
#include "@VMODROOT/header.h"
const Earlier u16
fn main() {
 assert sizeof(Earlier) == sizeof(u16)
 assert sizeof(Later) == sizeof(u64)
 assert sizeof(Grouped) == sizeof([2]u64)
 assert sizeof(Callback) == sizeof(fn (int) int)
 assert sizeof(ordinary) == sizeof([2]int)
 assert Earlier == 7
 assert Later == 11
 assert ordinary[1] == 6
}
const Later u64
const (
 Grouped [2]u64
 Callback fn (int) int
 ordinary = [5, 6]!
)
')!
	result := os.exec([@VEXE, '-b', 'c', 'run', source])
	assert result.exit_code == 0, result.output
}
