module types

import os

fn test_opaque_c_scalar_typedef_casts_to_numbers_and_runes() {
	root := os.join_path(os.vtmp_dir(), 'opaque_c_scalar_casts_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.c.v')
	os.write_file(source, '#include <wchar.h>
@[typedef]
struct C.wchar_t {}
type Character = C.wchar_t
fn main() {
	r := rune(65)
	value := unsafe { *(&Character(&r)) }
	assert rune(value) == r
	assert u64(value) == 65
	assert f64(value) == 65.0
	raw := unsafe { *(&C.wchar_t(&r)) }
	assert rune(raw) == r
}
')!
	for flags in ['', '-no-parallel -nocache'] {
		result := os.exec([@VEXE, '-new-compiler', ...(os.split_args(flags) or { panic(err) }),
			'run', source])
		assert result.exit_code == 0, result.output
	}
}

fn test_c_struct_typedef_with_fields_cannot_cast_to_numbers_or_runes() {
	root := os.join_path(os.vtmp_dir(), 'c_struct_numeric_casts_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.c.v')
	os.write_file(source, '@[typedef]
struct C.Record {
	value int
}
fn main() {
	value := C.Record{value: 65}
	_ = rune(value)
	_ = int(value)
}
')!
	for flags in ['', '-no-parallel -nocache'] {
		result := os.exec([@VEXE, '-new-compiler', ...(os.split_args(flags) or { panic(err) }),
			'-check', source])
		assert result.exit_code != 0, result.output
		assert result.output.contains('cannot cast struct `C.Record` to `rune`'), result.output
		assert result.output.contains('cannot cast type `C.Record` to `int`'), result.output
	}
}
