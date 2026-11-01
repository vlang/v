import os

fn test_c_signature_types_can_be_used_as_native_casts() {
	root := os.join_path(os.vtmp_dir(), 'c_signature_typedef_cast_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'native.h'), '#include <stdint.h>
typedef uint32_t DeviceId;
typedef uint16_t AudioFormat;
typedef uint32_t ReturnOnly;
typedef uint32_t Wide;
typedef uint16_t Narrow;
typedef enum { blend_alpha = 1, blend_add = 2 } BlendMode;
typedef struct { uint32_t value; } NativeGUID;
typedef NativeGUID DeviceGUID;
static uint32_t take_device(DeviceId value) { return value; }
static uint16_t take_format(AudioFormat value) { return value; }
static ReturnOnly make_device(void) { return 29; }
static Wide make_wide(void) { return 65543; }
static uint16_t take_narrow(Narrow value) { return value; }
static uint32_t take_blend(BlendMode value) { return value; }
static NativeGUID make_guid(void) { NativeGUID guid = { 11 }; return guid; }
static uint32_t take_guid(DeviceGUID guid) { return guid.value; }
static uint32_t ordinary_call(uint32_t value) { return value + 1; }
')!
	source := os.join_path(root, 'main.c.v')
	os.write_file(source, '#flag -I @DIR
#include "native.h"
type DeviceID = u32
enum Blend {
	alpha = 1
	add = 2
}
fn C.take_device(value C.DeviceId) u32
fn main() {
	device := C.DeviceId(DeviceID(7))
	assert C.take_device(device) == 7
	assert C.take_format(C.AudioFormat(u16(0x8010))) == 0x8010
	assert u32(C.ReturnOnly(23)) == 23
	assert u32(C.make_device()) == 29
	assert u32(C.Narrow(C.make_wide())) == 7
	assert C.take_narrow(C.Narrow(C.make_wide())) == 7
	assert C.take_blend(C.BlendMode(Blend.add)) == 2
	assert C.take_guid(C.DeviceGUID(C.make_guid())) == 11
	assert C.ordinary_call(41) == 42
}
fn C.take_format(value C.AudioFormat) u16
fn C.make_device() C.ReturnOnly
fn C.make_wide() C.Wide
fn C.take_narrow(value C.Narrow) u16
fn C.take_blend(value C.BlendMode) u32
fn C.make_guid() C.NativeGUID
fn C.take_guid(value C.DeviceGUID) u32
fn C.ordinary_call(value u32) u32
')!
	for flags in [[]string{}, ['-no-parallel', '-nocache']] {
		result := os.exec([@VEXE, '-new-compiler', '-no-retry-compilation', '-cc', @CCOMPILER,
			...flags, 'run', source])
		assert result.exit_code == 0, result.output
	}
	c_path := os.join_path(root, 'main.c')
	generated := os.exec([@VEXE, '-new-compiler', '-no-retry-compilation', '-o', c_path, source])
	assert generated.exit_code == 0, generated.output
	c_source := os.read_file(c_path)!
	start := c_source.index('((DeviceGUID[1]){') or { panic(c_source) }
	end := c_source[start..].index('})[0]') or { panic(c_source) }
	native_expr := c_source[start..start + end + '})[0]'.len]
	assert native_expr == '((DeviceGUID[1]){make_guid()})[0]', native_expr
	assert !c_source.contains('(DeviceGUID)(make_guid())'), c_source
	// Compile the emitted native expression as strict C, without the V runtime's
	// unrelated compiler extensions. This catches casts to aggregate typedefs.
	if os.file_name(@CCOMPILER) in ['gcc', 'clang', 'cc'] {
		strict_source := os.join_path(root, 'strict.c')
		strict_exe := os.join_path(root, 'strict')
		os.write_file(strict_source, '#include "native.h"\nint main(void) { return take_guid(${native_expr}) != 11; }\n')!
		strict_compile := os.exec([@CCOMPILER, '-std=c11', '-pedantic-errors', '-o', strict_exe,
			strict_source])
		assert strict_compile.exit_code == 0, strict_compile.output
		strict_run := os.exec([strict_exe])
		assert strict_run.exit_code == 0, strict_run.output
	}
	windows_c_path := os.join_path(root, 'windows.c')
	windows_generated := os.exec([@VEXE, '-new-compiler', '-no-retry-compilation', '-os', 'windows',
		'-cc', 'msvc', '-gc', 'none', '-o', windows_c_path, source])
	assert windows_generated.exit_code == 0, windows_generated.output
	windows_c := os.read_file(windows_c_path)!
	assert windows_c.contains(native_expr), windows_c
	assert !windows_c.contains('(DeviceGUID)(make_guid())'), windows_c
}

fn test_undeclared_c_calls_are_not_mistaken_for_typedef_casts() {
	root := os.join_path(os.vtmp_dir(), 'c_signature_unknown_call_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.c.v')
	os.write_file(source, 'fn C.take_device(value C.DeviceId) u32
fn main() {
	C.MissingType(u32(7))
}
')!
	result := os.exec([@VEXE, '-new-compiler', '-no-retry-compilation', '-check', source])
	assert result.exit_code != 0, result.output
	assert result.output.contains('unknown C function: `C.MissingType`'), result.output
}
