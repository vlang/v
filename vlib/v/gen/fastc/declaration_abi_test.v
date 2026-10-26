module fastc

import os
import v.cmdexec
import v.pref

fn declaration_abi_run_c(name string, source string) {
	root := os.join_path(os.vtmp_dir(), 'fastc_declaration_abi_${name}_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'main.c')
	program := os.join_path(root, 'program')
	os.write_file(path, source) or { panic(err) }
	build := cmdexec.run('cc', ['-std=gnu11', '-Werror=incompatible-pointer-types', '-o', program,
		path])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
}

fn declaration_abi_generate(name string, source string, prefs &pref.Preferences) string {
	root := os.join_path(os.vtmp_dir(), 'fastc_declaration_source_${name}_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), source) or { panic(err) }
	mut scoped := *prefs
	scoped.building_v = true
	scoped.backend = 'fastc'
	scoped.ccompiler = 'clang'
	scoped.user_defines = ['gc_none', 'no_backtrace', 'v3_backend']
	return generate_files([root], &scoped) or { panic(err) }
}

fn test_fastc_exports_preserve_c_integer_abi_and_internal_v_width() {
	$if !macos && !linux {
		return
	}
	mut prefs := pref.new_preferences()
	prefs.vroot = @VEXEROOT
	source := declaration_abi_generate('exports', r'module main
@[c_extern]
fn C.native_classify(value i32) i32
@[c_extern]
fn C.verify_exports()
@[export: "native_classify"]
fn classify(value int) int {
	return if value < 0 { -7 } else { 9 }
}
@[export: "wide_identity"]
fn wide_identity(value i64) i64 { return value }
fn main() { C.verify_exports() }
', prefs)
	declaration_abi_run_c('exports', source + r'
void verify_exports(void) {
	if (native_classify(-1) != -7 || native_classify(1) != 9) abort();
	if (wide_identity((i64)0x100000005) != (i64)0x100000005) abort();
}
')
}

fn test_fastc_cinit_grouped_volatile_exports_and_struct_layout() {
	$if !macos && !linux {
		return
	}
	mut prefs := pref.new_preferences()
	prefs.vroot = @VEXEROOT
	prefs.enable_globals = true
	section := $if macos { '__DATA,__fastc' } $else { '.fastc' }
	source := declaration_abi_generate('layout', r'module main
@[packed]
struct Packet {
	code u8
	value u32
}
@[aligned: 64]
struct Cacheline { value u64 }
struct Request {
	id [2]u64 = [u64(0x1234), u64(0xabcd)]!
	revision u64
}
@[_linker_section: "SECTION"]
@[cinit]
@[export: "boot_request"]
__global (
	volatile request = Request{ revision: 2 }
)
@[cinit]
@[export: "request_magic"]
__global (
	volatile magic = [u64(0xf6b8f4b39de7d1ae), u64(0xfab91a6940fcb9cf)]!
)
@[c_extern]
fn C.verify_layout()
fn main() { C.verify_layout() }
'.replace('SECTION', section), prefs)
	fastc_set_compact_v3_type_names(true)
	packet_type := fastc_c_declared_type_name(fastc_type_key('main', 'Packet'))
	cacheline_type := fastc_c_declared_type_name(fastc_type_key('main', 'Cacheline'))
	request_type := fastc_c_declared_type_name(fastc_type_key('main', 'Request'))
	fastc_set_compact_v3_type_names(false)
	assert source.contains('section("${section}")'), source
	assert source.contains('volatile ${request_type} boot_request'), source
	assert !source.contains('\tboot_request = '), source
	assert !source.contains('\trequest_magic = '), source
	declaration_abi_run_c('layout', source + r'
void verify_layout(void) {
	if (sizeof(PACKET_TYPE) != 5 || offsetof(PACKET_TYPE, value) != 1) abort();
	if (_Alignof(CACHELINE_TYPE) != 64 || sizeof(CACHELINE_TYPE) != 64) abort();
	if (boot_request.revision != 2 || boot_request.id[0] != 0x1234 || boot_request.id[1] != 0xabcd) abort();
	if (request_magic[0] != (u64)0xf6b8f4b39de7d1aeULL || request_magic[1] != (u64)0xfab91a6940fcb9cfULL) abort();
}
'.replace('PACKET_TYPE', packet_type).replace('CACHELINE_TYPE', cacheline_type))
}

fn test_fastc_grouped_global_function_pointers_preserve_option_results() {
	$if !macos && !linux {
		return
	}
	mut prefs := pref.new_preferences()
	prefs.vroot = @VEXEROOT
	prefs.enable_globals = true
	source := declaration_abi_generate('global_callbacks', r'module main
__global (
	marker [2]u64 = [u64(11), u64(22)]!
	volatile callback fn (int) ?int = unsafe { nil }
	checks int
)
@[cinit]
__global (volatile boot_callback fn (int) ?int = unsafe { nil })
fn probe(value int) ?int {
	if value < 0 { return none }
	return value + 7
}
fn call_probe(value int) int {
	if callback == unsafe { nil } { return -1 }
	return callback(value) or { -2 }
}
fn main() {
	assert call_probe(1) == -1
	assert boot_callback == unsafe { nil }
	callback = probe
	boot_callback = probe
	assert (boot_callback(35) or { -1 }) == 42
	assert call_probe(35) == 42
	assert call_probe(-1) == -2
	assert marker[0] == 11 && marker[1] == 22
	checks = call_probe(35)
	assert checks == 42
}
', prefs)
	assert source.contains('Option (*volatile callback)'), source
	assert source.contains('Option (*volatile boot_callback)'), source
	declaration_abi_run_c('global_callbacks', source + r'
_Static_assert(__builtin_types_compatible_p(__typeof__(&callback), Option (*volatile *)(i64)), "callback pointer storage must be volatile");
_Static_assert(__builtin_types_compatible_p(__typeof__(&boot_callback), Option (*volatile *)(i64)), "cinit callback pointer storage must be volatile");
')
}

fn test_fastc_volatile_struct_fields_keep_qualified_storage() {
	$if !macos && !linux {
		return
	}
	mut prefs := pref.new_preferences()
	prefs.vroot = @VEXEROOT
	source := declaration_abi_generate('volatile_field', r'module main
struct SecretByte {
mut:
	volatile value u8
}
@[c_extern]
fn C.verify_secret(secret &SecretByte)
fn main() {
	mut secret := SecretByte{ value: 42 }
	secret.value = 0
	C.verify_secret(&secret)
}
', prefs)
	secret_type := fastc_c_declared_type_name(fastc_type_key('main', 'SecretByte'))
	assert source.contains('volatile u8 value;'), source
	declaration_abi_run_c('volatile_field', source + r'
void verify_secret(SECRET_TYPE *secret) {
	volatile u8 *storage = &secret->value;
	if (*storage != 0) abort();
}
'.replace('SECRET_TYPE', secret_type))
}

fn test_fastc_target_globals_initialize_only_when_vinit_is_called() {
	$if !macos && !linux {
		return
	}
	mut prefs := pref.new_preferences()
	prefs.target_libc_headers = true
	definition := fastc_target_startup_definition(prefs, true)
	declaration_abi_run_c('startup', '#include <stddef.h>\n#include <stdlib.h>\n' + r'
static int allocations_ready;
static int initialized;
void v_fastc_init_globals(void) {
	if (!allocations_ready) abort();
	initialized = 42;
}
' + definition + r'
int main(void) {
	if (initialized != 0) abort();
	allocations_ready = 1;
	_vinit(0, NULL);
	if (initialized != 42) abort();
	return 0;
}
')
}

fn test_fastc_target_exports_match_unqualified_target_header_prototypes() {
	$if !macos && !linux {
		return
	}
	mut prefs := pref.new_preferences()
	prefs.vroot = @VEXEROOT
	prefs.target_libc_headers = true
	source := declaration_abi_generate('target_constness', r'module main
fn C.native_copy(destination voidptr, const_source voidptr, count uint) voidptr
@[c_extern]
fn C.verify_copy()
@[export: "native_copy"]
fn copy(destination voidptr, source voidptr, count uint) voidptr {
	if count != 0 {
		unsafe {
			mut dest := &u8(destination)
			src := &u8(source)
			*dest = *src
		}
	}
	return destination
}

fn main() { C.verify_copy() }
', prefs)
	// Vinix's VINIX_V_RUNTIME declarations intentionally omit const on V definitions.
	contract := '#include <stddef.h>\nvoid *native_copy(void *, void *, size_t);\n'
	declaration_abi_run_c('target_constness', contract + source + r'
void verify_copy(void) {
	u8 input = 42, output = 0;
	if (native_copy(&output, &input, 1) != &output || output != 42) abort();
}
')
}

fn test_fastc_target_native_prototypes_match_exported_write_constness_and_abi() {
	$if !macos && !linux {
		return
	}
	mut prefs := pref.new_preferences()
	prefs.vroot = @VEXEROOT
	prefs.target_libc_headers = true
	source := declaration_abi_generate('target_write', r'module main
fn C.write(fd i32, const_buf voidptr, count uint) C.ptrdiff_t
@[export: "write"]
fn kernel_write(fd int, buf voidptr, count uint) int {
	if fd < 0 { return -7 }
	if count == 0 { return 0 }
	ptr := unsafe { &u8(buf) }
	return int(ptr[0]) + int(count)
}
fn main() {
	mut value := u8(40)
	assert C.write(-1, voidptr(&value), 2) == -7
	assert C.write(1, voidptr(&value), 2) == 42
}
', prefs)
	assert source.contains('extern ptrdiff_t write(i32, voidptr, usize);'), source
	assert !source.contains('extern ptrdiff_t write(i32, const void*'), source
	declaration_abi_run_c('target_write', source)
}

fn test_fastc_interpolates_native_pointer_differences() {
	$if !macos && !linux {
		return
	}
	mut prefs := pref.new_preferences()
	prefs.vroot = @VEXEROOT
	program := r"module main
@[c_extern]
fn C.native_difference() C.ptrdiff_t
fn main() {
	difference := C.native_difference()
	assert '${difference}' == '-15'
	assert '${difference:05d}' == '-0015'
}
"
	for selfhost in [false, true] {
		prefs.target_libc_headers = selfhost
		source := if selfhost {
			declaration_abi_generate('ptrdiff', program, prefs)
		} else {
			generate(program, 'ptrdiff.c.v', prefs) or { panic(err) }
		}
		assert source.contains('extern ptrdiff_t native_difference('), source
		declaration_abi_run_c('ptrdiff_${selfhost}', '#include <stddef.h>\n' + source + '\nptrdiff_t native_difference(void) { return -15; }\n')
	}
}
