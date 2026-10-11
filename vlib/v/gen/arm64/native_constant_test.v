module arm64

import os
import v.parser
import v.pref
import v.ssa
import v.transform
import v.types

fn test_native_fixed_array_lengths_fold_in_runtime_constant_initializers() {
	$if macos && arm64 {
		result := native_constant_fixture('fixed_array_lengths', [
			r'module templates
pub const bytes = [u8(0x11), 0x00, 0xfe, 0x5c, 0x30, 0x00, 0xfe, 0x58, 0x00, 0x02, 0x1f, 0xd6]!
const padded = if 2 * u32(sizeof(voidptr)) > u32(bytes.len) {
    2 * u32(sizeof(voidptr))
} else {
    u32(bytes.len) + u32(sizeof(voidptr)) - 1
}
pub const slot_size = int(padded & ~(u32(sizeof(voidptr)) - 1))
pub fn verify() bool {
    local := [u8(65), 66, 67]!
    return bytes.len == 12 && local.len == 3 && slot_size == 16
}
',
			r'module main
import templates
fn C.exit(int)
fn main() {
    if !templates.verify() { C.exit(1) }
    if templates.bytes.len != 12 || templates.slot_size != 16 { C.exit(2) }
}
',
		])
		assert result.exit_code == 0, 'exit ${result.exit_code}: ${result.output}'
	}
}

fn test_native_runtime_constants_initialize_once_after_their_dependencies() {
	$if macos && arm64 {
		result := native_constant_fixture('dependencies', [r'module main
fn C.exit(int)
struct Pair { value int }
__global calls = 0
const from_function = read_later()
const outer = next_value(later)
const later = next_value(10)
const pair = Pair{value: next_value(20)}
const values = [next_value(30), next_value(40)]
const extent = 1 << 2
const bytes = [extent]u8{}
__global snapshot = outer
fn next_value(seed int) int {
    calls += 1
    return seed + calls
}
fn read_later() int { return later }
fn init() {
    if calls != 5 { C.exit(10 + calls) }
    if snapshot != 13 { C.exit(40 + snapshot) }
    calls += 100
}
fn main() {
    if from_function != 11 || later != 11 || outer != 13 { C.exit(2) }
    if pair.value != 23 || values[0] != 34 || values[1] != 45 { C.exit(3) }
    if from_function != 11 || pair.value != 23 || values[0] != 34 { C.exit(4) }
    if calls != 105 || snapshot != 13 || sizeof(bytes) != 4 { C.exit(5) }
}
'])
		assert result.exit_code == 0, 'exit ${result.exit_code}: ${result.output}'
	}
}

fn test_native_darwin_clock_and_stopwatch_advance_without_unsigned_underflow() {
	$if macos && arm64 {
		darwin := os.read_file(os.join_path(@VMODROOT, 'vlib', 'time', 'time_darwin.c.v')) or {
			panic(err)
		}
		stopwatch := os.read_file(os.join_path(@VMODROOT, 'vlib', 'time', 'stopwatch.v')) or {
			panic(err)
		}
		time_source := darwin.all_before('// darwin_now') + stopwatch.all_after('module time') +
			'\npub type Duration = i64\npub fn sys_mono_now() u64 { return sys_mono_now_darwin() }\n'
		result := native_constant_fixture('clock', [time_source,
			r'module main
import time
fn C.exit(int)
fn C.alarm(u32) u32
fn C.usleep(u32) int
fn main() {
    C.alarm(10)
    watch := time.new_stopwatch(time.StopWatchOptions{auto_start: true})
    C.usleep(20000)
    elapsed := i64(watch.elapsed())
    if elapsed < 1000000 || elapsed > 2000000000 { C.exit(1) }
    first := time.sys_mono_now()
    C.usleep(10000)
    second := time.sys_mono_now()
    if second <= first || second - first > 2000000000 { C.exit(2) }
    if first > 60000000000 || second > 60000000000 { C.exit(3) }
}
'])
		assert result.exit_code == 0, 'exit ${result.exit_code}: ${result.output}'
	}
}

fn test_native_compiler_string_array_constants_reserve_full_headers_and_preserve_libc_calls() {
	$if macos && arm64 {
		builder := os.read_file(os.join_path(@VMODROOT, 'vlib', 'v', 'ssa', 'builder.v')) or {
			panic(err)
		}
		linker := os.read_file(os.join_path(@VMODROOT, 'vlib', 'v', 'gen', 'arm64', 'linker.v')) or {
			panic(err)
		}
		bench_names := builder.all_after('const bench_runtime_stub_names =').all_before('// Builder stores')
		external_names := linker.all_after('const force_external_syms =').all_before('\n\n')
		result := native_constant_fixture('compiler_arrays', [
			'module ssa\nconst bench_runtime_stub_names =' + bench_names +
				'pub fn verify() bool { return bench_runtime_stub_names.len == 9 && bench_runtime_stub_names[0] == "current_rss_kb" && bench_runtime_stub_names[8] == "v.bench.linux_rss_kb" }\n',
			'module arm64\nconst force_external_syms =' + external_names +
				'pub fn verify() bool { return force_external_syms.len > 100 && force_external_syms[0] == "_malloc" && force_external_syms[1] == "_free" }\n',
			r'module main
import ssa
import arm64
fn C.exit(int)
fn C.strlen(&u8) usize
__global canary = u64(305419896)
const neighboring = ["first", "second", "third"]
fn main() {
    if !ssa.verify() || !arm64.verify() { C.exit(1) }
    if neighboring.len != 3 || neighboring[2] != "third" { C.exit(2) }
    if canary != 305419896 || C.strlen(c"guard") != 5 { C.exit(3) }
}
',
		])
		assert result.exit_code == 0, 'exit ${result.exit_code}: ${result.output}'
	}
}

fn native_constant_fixture(name string, sources []string) os.Result {
	mut paths := []string{}
	for index, source in sources {
		path := os.join_path(os.vtmp_dir(), 'arm64_constant_${name}_${os.getpid()}_${index}.v')
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
	mut a := p.parse_files(paths)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	m := if name == 'without_checker' {
		ssa.build(a)
	} else {
		mut tc := types.TypeChecker.new(a)
		tc.collect(a)
		tc.annotate_types()
		assert tc.errors.len == 0, tc.errors.str()
		transform.transform(mut a, tc)
		ssa.build_with_used(a, map[string]bool{}, tc)
	}
	if name == 'compiler_arrays' {
		for global_name in ['__const_ssa__bench_runtime_stub_names',
			'__const_arm64__force_external_syms', '__const_neighboring'] {
			globals := m.globals.filter(it.name == global_name)
			assert globals.len == 1, global_name
			assert m.type_size(globals[0].typ) == 32, global_name
		}
	}
	mut g := Gen.new(m)
	g.gen()
	g.write_and_link(output)
	return os.exec([output])
}

fn test_native_string_array_constant_indexing_without_a_type_checker() {
	$if macos && arm64 {
		result := native_constant_fixture('without_checker', [r'module main
fn C.exit(int)
const words = ["first", "second", "third"]
fn main() {
    if words.len != 3 || words[0] != "first" { C.exit(1) }
    if words[1] != "second" || words[2] != "third" { C.exit(2) }
    if words.len != 3 || words[2] != "third" { C.exit(3) }
}
'])
		assert result.exit_code == 0, 'exit ${result.exit_code}: ${result.output}'
	}
}
