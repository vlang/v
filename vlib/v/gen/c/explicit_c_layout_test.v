module c

import os
import v.cmdexec

fn test_explicit_native_layout_survives_unrelated_module_headers() ! {
	root := os.join_path(os.vtmp_dir(), 'explicit_c_layout_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'unrelated.h'), '#define UNRELATED_HEADER 1\n')!
	os.write_file(os.join_path(root, 'native.c'), 'typedef struct NativeRecord {
 int count;
 float y0;
 double value;
} NativeRecord;
int native_record_verify(const NativeRecord* record) {
 return record->count == 7 && record->y0 == 2.5f && record->value == 9.0;
}
')!
	source := os.join_path(root, 'main.c.v')
	os.write_file(source, '#include "unrelated.h"
#flag @VMODROOT/native.c
@[c_extern; typedef]
struct C.NativeRecord {
 count i32
 y0 f32
 value f64
}
@[c_extern]
fn C.native_record_verify(const_record &C.NativeRecord) i32
fn main() {
 record := C.NativeRecord{count: 7, y0: 2.5, value: 9.0}
 assert C.native_record_verify(&record) == 1
 println("OK")
}
')!
	os.write_file(os.join_path(root, 'v.mod'), 'Module { name: "explicit_c_layout" }\n')!
	for flags in [[]string{}, ['-cstrict']] {
		mut args := ['-nocache', '-cc', 'clang', '-gc', 'none', '-no-retry-compilation']
		args << flags
		args << ['run', source]
		result := cmdexec.run(@VEXE, args)
		assert result.exit_code == 0, result.output
		assert result.output == 'OK\n', result.output
	}
}
