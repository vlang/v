module arm64

import os
import v.parser
import v.pref
import v.ssa
import v.types

fn test_native_startup_initializes_globals_before_module_init() {
	$if macos && arm64 {
		run_native_runtime_fixture('startup', 'module main
fn C.exit(int)
struct StartupValues { first i64 second i64 third i64 }
__global startup_text = "initialized"
__global startup_values = StartupValues{first: 3, second: 5, third: 7}
__global startup_numbers = [11, 13]
__global startup_number = 17
__global startup_bytes = [128]u8{}
fn init() {
	startup_number += 19
	startup_bytes[0] = u8(7)
	startup_bytes[127] = u8(9)
}
fn main() {
	if startup_text != "initialized" { C.exit(1) }
	values := startup_values
	if values.first != 3 || values.second != 5 || values.third != 7 { C.exit(2) }
	if startup_numbers.len != 2 || startup_numbers[0] != 11 || startup_numbers[1] != 13 { C.exit(3) }
	if startup_number != 36 { C.exit(4) }
	if startup_bytes[0] != 7 || startup_bytes[127] != 9 { C.exit(5) }
}
')
	}
}

fn test_native_module_globals_and_initializers_follow_dependencies() {
	$if macos && arm64 {
		result := native_runtime_fixture_result('module_globals', [
			'module main
import parent
import child
fn C.exit(int)
struct LocalChild { shared int }
__global shared = 100
fn init() { shared += 1 }
fn main() {
	if shared != 101 { C.exit(1) }
	if parent.value() != 15 { C.exit(2) }
	if child.value() != 5 { C.exit(3) }
	child := LocalChild{shared: 72}
	if child.shared != 72 { C.exit(4) }
}
',
			'module parent
import child
__global shared = child.value()
fn init() { shared += 10 }
pub fn value() int { return shared }
',
			'module child
__global shared = 2
fn init() { shared += 3 }
pub fn value() int { return shared }
',
		])
		assert result.exit_code == 0, result.output
	}
}

fn test_native_builtin_initialization_precedes_module_globals_and_init() {
	$if macos && arm64 {
		result := native_runtime_fixture_result('builtin_startup', [
			'module main
import dependent
fn C.exit(int)
__global main_phase = startup_phase()
fn init() { main_phase += dependent.value() }
fn main() {
	if main_phase != 31 { C.exit(1) }
	if dependent.value() != 22 { C.exit(2) }
}
',
			'module dependent
__global dependent_phase = startup_phase()
fn init() { dependent_phase += 13 }
pub fn value() int { return dependent_phase }
',
			'module builtin
__global native_startup_phase = 4
fn builtin_init() { native_startup_phase += 5 }
pub fn startup_phase() int { return native_startup_phase }
',
		])
		assert result.exit_code == 0, result.output
	}
}

fn test_native_panic_preserves_its_message() {
	$if macos && arm64 {
		result := native_runtime_fixture_result('panic', ['module main
fn main() { panic("native panic message") }
'])
		assert result.exit_code == 1
		assert result.output.contains('native panic message')
	}
}

fn test_native_literal_pointer_store_preserves_adjacent_canary() {
	$if macos && arm64 {
		mut m := ssa.Module.new()
		i8_type := m.type_store.get_int(8)
		i32_type := m.type_store.get_int(32)
		i64_type := m.type_store.get_int(64)
		ptr_type := m.type_store.get_ptr(i8_type)
		pair_type := m.type_store.get_tuple([ptr_type, i64_type])
		global := m.add_global('literal_and_canary', pair_type)
		main_id := m.new_function('main', i32_type)
		entry := m.add_block(main_id, 'entry')
		zero := m.get_or_add_const(i64_type, '0')
		eight := m.get_or_add_const(i64_type, '8')
		canary := m.get_or_add_const(i64_type, '123456789')
		canary_ptr := m.add_instr(.get_element_ptr, entry, m.type_store.get_ptr(i64_type),
			[global, eight])
		m.add_instr(.store, entry, ssa.TypeID(0), [canary, canary_ptr])
		literal := m.add_value(.string_literal, ptr_type, '%.*f', 0)
		literal_ptr := m.add_instr(.get_element_ptr, entry, m.type_store.get_ptr(ptr_type),
			[global, zero])
		m.add_instr(.store, entry, ssa.TypeID(0), [literal, literal_ptr])
		value := m.add_instr(.load, entry, i64_type, [canary_ptr])
		changed := m.add_instr(.ne, entry, i32_type, [value, canary])
		m.add_instr(.ret, entry, ssa.TypeID(0), [changed])
		output := os.join_path(os.vtmp_dir(), 'arm64_literal_canary_${os.getpid()}')
		defer {
			os.rm(output) or {}
		}
		mut g := Gen.new(m)
		g.gen()
		g.write_and_link(output)
		result := os.exec([output])
		assert result.exit_code == 0, result.output
	}
}

fn test_native_filelock_headers_preserve_modes_ranges_and_unlocking() {
	$if macos && arm64 {
		run_native_runtime_fixture('filelock', 'module main
fn C.exit(int)
fn C.alarm(u32) u32
fn C.malloc(usize) voidptr
fn C.free(voidptr)
fn C.memcpy(voidptr, voidptr, usize) voidptr
fn C.mkstemp(voidptr) i32
fn C.unlink(voidptr) i32
fn C.close(i32) i32
fn C.fork() i32
fn C.waitpid(i32, &i32, i32) i32
fn C.v_filelock_lock(i32, i32, i32, u64, u64) i32
fn C.v_filelock_unlock(i32, u64, u64) i32
fn check_child(pid i32) {
	mut status := i32(0)
	if C.waitpid(pid, &status, 0) != pid || status != 0 { C.exit(10) }
}
fn main() {
	C.alarm(5)
	name := "/tmp/v-native-filelock-XXXXXX"
	path := C.malloc(128)
	C.memcpy(path, name.str, usize(name.len + 1))
	fd := C.mkstemp(path)
	if fd < 0 { C.exit(1) }
	if C.v_filelock_lock(fd, 1, 1, 2, 3) != 0 { C.exit(2) }
	child := C.fork()
	if child < 0 { C.exit(3) }
	if child == 0 {
		if C.v_filelock_lock(fd, 0, 1, 2, 1) == 0 { C.exit(4) }
		if C.v_filelock_lock(fd, 1, 1, 6, 1) != 0 { C.exit(5) }
		C.exit(0)
	}
	check_child(child)
	if C.v_filelock_unlock(fd, 2, 3) != 0 { C.exit(6) }
	if C.v_filelock_lock(fd, 0, 0, 2, 3) != 0 { C.exit(7) }
	reader := C.fork()
	if reader < 0 { C.exit(8) }
	if reader == 0 {
		if C.v_filelock_lock(fd, 0, 1, 2, 1) != 0 { C.exit(11) }
		if C.v_filelock_lock(fd, 1, 1, 2, 1) == 0 { C.exit(12) }
		C.exit(0)
	}
	check_child(reader)
	C.v_filelock_unlock(fd, 2, 3)
	C.close(fd)
	C.unlink(path)
	C.free(path)
	C.alarm(0)
}
')
	}
}

fn test_native_flat_payload_header_helpers_preserve_pointer_identity() {
	$if macos && arm64 {
		run_native_runtime_fixture('payload_pointer', 'module main
fn C.exit(int)
fn C.malloc(usize) voidptr
fn C.free(voidptr)
fn C.v_flat_payload_ptr_get(voidptr, usize) voidptr
fn C.v_flat_payload_ptr_set(voidptr, usize, voidptr)
fn main() {
	base := C.malloc(24)
	C.v_flat_payload_ptr_set(base, 0, unsafe { nil })
	C.v_flat_payload_ptr_set(base, 1, unsafe { nil })
	mut payload := i64(73)
	C.v_flat_payload_ptr_set(base, 2, &payload)
	if C.v_flat_payload_ptr_get(base, 0) != unsafe { nil } { C.exit(1) }
	pointer := &i64(C.v_flat_payload_ptr_get(base, 2))
	if pointer != &payload || unsafe { *pointer } != 73 { C.exit(2) }
	unsafe { *pointer = 91 }
	if payload != 91 { C.exit(3) }
	C.free(base)
}
')
	}
}

fn test_native_stdio_globals_use_the_libc_stream() {
	$if macos && arm64 {
		result := native_runtime_fixture_result('stdio', ['module main
struct C.FILE {}
__global C.stderr &C.FILE
fn C.fputs(voidptr, &C.FILE) i32
fn C.fflush(&C.FILE) i32
fn main() {
	message := "native stdio marker"
	C.fputs(message.str, C.stderr)
	C.fflush(C.stderr)
}
'])
		assert result.exit_code == 0, result.output
		assert result.output.contains('native stdio marker')
	}
}

fn test_native_c_string_literals_preserve_stdio_format_and_variadic_arguments() {
	$if macos && arm64 {
		result := native_runtime_fixture_result('c_string_stdio', ["module main
struct C.FILE {}
__global C.stderr &C.FILE
fn C.exit(int)
fn C.alarm(u32) u32
fn C.fprintf(&C.FILE, &u8, ...voidptr) i32
fn C.fputs(&u8, &C.FILE) i32
fn C.fflush(&C.FILE) i32
fn first(value &u8) u8 { return unsafe { value[0] } }
fn main() {
	C.alarm(3)
	if C.fputs(c'fixed\\n', C.stderr) < 0 { C.exit(4) }
	if C.fprintf(C.stderr, c'one %d\\n', 7) != 6 { C.exit(1) }
	if C.fprintf(C.stderr, c'%s %d\\n', c'ptr', 9) != 6 { C.exit(2) }
	if first(c'A') != `A` { C.exit(3) }
	C.fflush(C.stderr)
	C.alarm(0)
}
"])
		assert result.exit_code == 0, result.output
		assert result.output == 'fixed\none 7\nptr 9\n', result.output
	}
}

fn test_native_startup_preserves_the_process_arguments() {
	$if macos && arm64 {
		run_native_runtime_fixture('arguments', 'module main
fn C.exit(int)
__global g_main_argc = int(0)
__global g_main_argv = unsafe { nil }
fn arguments() []string { return []string{} }
fn main() {
	if g_main_argc != 1 { C.exit(2) }
	if g_main_argv == unsafe { nil } { C.exit(3) }
	args := arguments()
	if args.len != 1 { C.exit(4) }
	if args[0].len == 0 { C.exit(5) }
}
')
	}
}

fn test_native_map_equality_handles_order_values_missing_keys_and_empty_maps() {
	$if macos && arm64 {
		run_native_runtime_fixture('map_equality', 'module main
fn C.exit(int)
fn map_map_eq(a map[string]int, b map[string]int) bool { return false }
fn main() {
	left := map[string]int{"first": 11, "second": 22}
	right := map[string]int{"second": 22, "first": 11}
	if !map_map_eq(left, right) { C.exit(1) }
	if map_map_eq(left, map[string]int{"first": 11, "second": 23}) { C.exit(2) }
	if map_map_eq(left, map[string]int{"first": 11, "other": 22}) { C.exit(3) }
	if map_map_eq(left, map[string]int{"first": 11}) { C.exit(4) }
	empty := map[string]int{}
	if !map_map_eq(empty, map[string]int{}) { C.exit(5) }
	if map_map_eq(empty, left) { C.exit(6) }
}
')
	}
}

fn test_native_string_ordering_preserves_signed_libc_comparisons() {
	$if macos && arm64 {
		run_native_runtime_fixture('string_ordering', 'module main
fn C.exit(int)
fn main() {
	if !("alpha" < "beta") { C.exit(1) }
	if "beta" < "alpha" { C.exit(2) }
	if "same" < "same" { C.exit(3) }
	if !("prefix" < "prefix suffix") { C.exit(4) }
	if "prefix suffix" < "prefix" { C.exit(5) }
}
')
	}
}

fn test_native_backtrace_capture_honors_skipped_frames() {
	$if macos && arm64 {
		run_native_runtime_fixture('backtrace', 'module main
fn C.exit(int)
fn print_backtrace_skipping_top_frames(skipframes int) bool { return false }
fn main() {
	if print_backtrace_skipping_top_frames(1000) { C.exit(1) }
}
')
	}
}

fn test_native_map_get_and_set_inserts_once_and_returns_writable_storage() {
	$if macos && arm64 {
		run_native_runtime_fixture('map_get_and_set', 'module main
fn C.exit(int)
fn map__get_and_set(m &map[string]i64, key voidptr, fallback voidptr) voidptr { return 0 }
fn main() {
	mut values := map[string]i64{}
	key := "new"
	fallback := i64(3)
	slot := &i64(map__get_and_set(&values, &key, &fallback))
	unsafe { *slot += 4 }
	if values["new"] != 7 { C.exit(1) }
	replacement := i64(99)
	same := &i64(map__get_and_set(&values, &key, &replacement))
	if same != slot || values["new"] != 7 || values.len != 1 { C.exit(2) }
}
')
	}
}

fn run_native_runtime_fixture(name string, source string) {
	result := native_runtime_fixture_result(name, [source])
	assert result.exit_code == 0, '${name}: ${result.output}'
}

fn native_runtime_fixture_result(name string, sources []string) os.Result {
	mut paths := []string{}
	for i, source in sources {
		path := os.join_path(os.vtmp_dir(), 'arm64_runtime_${name}_${os.getpid()}_${i}.v')
		os.write_file(path, source) or { panic(err) }
		paths << path
	}
	output := paths[0].all_before_last('.')
	defer {
		for path in paths {
			os.rm(path) or {}
		}
		os.rm(output) or {}
	}
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_files(paths)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	tc.annotate_types()
	assert tc.errors.len == 0, tc.errors.str()
	m := ssa.build_with_used(a, map[string]bool{}, tc)
	mut g := Gen.new(m)
	g.gen()
	g.write_and_link(output)
	return os.exec([output])
}
