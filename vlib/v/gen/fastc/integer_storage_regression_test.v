module fastc

import os
import v.cmdexec

fn test_fastc_uint_global_is_initialized_before_runtime_constants() {
	root := os.join_path(os.vtmp_dir(), 'fastc_uint_global_storage_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), r'module main
__global configured = uint(7)
const initial_value = read_configured()
fn read_configured() uint { return configured }
fn main() {
 assert initial_value == 7
 assert configured == 7
}
') or { panic(err) }
	program := os.join_path(root, 'program')
	built := cmdexec.run(@VEXE, ['-b', 'fastc', '-enable-globals', '-o', program, root])
	assert built.exit_code == 0, built.output
	ran := cmdexec.run(program, [])
	assert ran.exit_code == 0, ran.output
}

fn test_fastc_signed_interpolation_preserves_native_and_canonical_integer_widths() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_integer_interpolation_storage_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'native.h'), r'#include <stddef.h>
#include <stdint.h>
static ptrdiff_t wide_positive(void) {
#if PTRDIFF_MAX > INT32_MAX
 return (ptrdiff_t)INT64_C(4294967297);
#else
 return (ptrdiff_t)1234567;
#endif
}
static ptrdiff_t wide_negative(void) { return -wide_positive(); }
') or { panic(err) }
	os.write_file(os.join_path(root, 'main.c.v'), r'module main
#insert "native.h"
fn C.wide_positive() C.ptrdiff_t
fn C.wide_negative() C.ptrdiff_t
fn main() {
 positive := C.wide_positive()
 negative := C.wide_negative()
 positive_text := if sizeof(voidptr) > 4 { "4294967297" } else { "1234567" }
 negative_text := "-" + positive_text
 signed_positive_text := "+" + positive_text
 assert "${positive}" == positive_text
 assert "${negative}" == negative_text
 assert "${positive:+}" == signed_positive_text
 assert "${negative:+}" == negative_text
 canonical_positive := int(positive)
 canonical_negative := int(negative)
 canonical_unsigned := uint(positive)
 assert "${canonical_positive}" == positive_text
 assert "${canonical_negative}" == negative_text
 assert "${canonical_unsigned}" == positive_text
 assert "${canonical_positive:+}" == signed_positive_text
 assert "${canonical_negative:+}" == negative_text
 assert "${canonical_unsigned:+}" == signed_positive_text
}
') or { panic(err) }
	program := os.join_path(root, 'program')
	built := cmdexec.run(@VEXE, ['-b', 'fastc', '-o', program, root])
	assert built.exit_code == 0, built.output
	ran := cmdexec.run(program, [])
	assert ran.exit_code == 0, ran.output
}
