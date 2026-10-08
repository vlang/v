module arm64

import os
import v.parser
import v.pref
import v.ssa
import v.transform
import v.types

fn test_native_poll_detects_closed_capture_pipes_and_invalid_descriptors() {
	$if macos && arm64 {
		result := native_poll_sync_fixture('poll', [r'module main
fn C.exit(int)
fn C.alarm(u32) u32
fn C.pipe(&i32) i32
fn C.close(i32) i32
fn C.read(i32, voidptr, usize) isize
struct C.pollfd { fd int events i16 revents i16 }
fn C.poll(&C.pollfd, u64, int) int
fn main() {
    C.alarm(5)
    if sizeof(C.pollfd) != 8 { C.exit(6) }
    mut signed := i16(-1234)
    if sizeof(signed) != 2 || i64(signed) != -1234 { C.exit(7) }
    mut fds := [2]i32{}
    if C.pipe(&fds[0]) != 0 { C.exit(1) }
    C.close(fds[1])
    mut descriptor := C.pollfd{fd: fds[0], events: i16(C.POLLIN)}
    if C.poll(&descriptor, 1, 0) != 1 { C.exit(2) }
    mut byte := u8(0)
    if C.read(fds[0], &byte, 1) != 0 { C.exit(3) }
    C.close(fds[0])
    descriptor.revents = 0
    if C.poll(&descriptor, 1, 0) != 1 { C.exit(4) }
    if descriptor.revents & i16(C.POLLNVAL) == 0 { C.exit(5) }
    C.alarm(0)
}
'])
		assert result.exit_code == 0, 'exit ${result.exit_code}: ${result.output}'
	}
}

fn test_native_semaphore_timeout_returns_and_monitor_thread_terminates() {
	$if macos && arm64 {
		darwin := os.read_file(os.join_path(@VMODROOT, 'vlib', 'sync', 'sync_darwin.c.v')) or {
			panic(err)
		}
		timing := os.read_file(os.join_path(@VMODROOT, 'vlib', 'sync', 'timing_nix.c.v')) or {
			panic(err)
		}
		common := os.read_file(os.join_path(@VMODROOT, 'vlib', 'sync', 'sync.c.v')) or {
			panic(err)
		}
		sync_source := darwin.all_before('// new_mutex creates') +
			'\n' + common.all_after('module sync').all_before('// SpinLock is') +
			'\n' + timing.all_after('module sync') +
			'\n// init initialises the Semaphore' + darwin.all_after('// init initialises the Semaphore').all_before('// wait will') +
			'\n// timed_wait is similar' + darwin.all_after('// timed_wait is similar') +
			'\npub fn monotonic_now() i64 { return sync_mono_now() }\n'
		result := native_poll_sync_fixture('semaphore', [sync_source,
			r'module main
import sync
fn C.exit(int)
fn C.alarm(u32) u32
fn C.usleep(u32) int
fn C.malloc(usize) voidptr
fn C.memcpy(voidptr, voidptr, usize) voidptr
fn memdup(source voidptr, size isize) voidptr {
    result := C.malloc(usize(size))
    return C.memcpy(result, source, usize(size))
}
fn monitor(sem &sync.Semaphore) int {
    mut wake := unsafe { &sync.Semaphore(voidptr(sem)) }
    mut ticks := 0
    for {
        if wake.timed_wait(10000000) { return ticks }
        ticks++
    }
    return ticks
}
fn main() {
    C.alarm(5)
    mut sem := sync.Semaphore{}
    sem.init(0)
    started := sync.monotonic_now()
    if sem.timed_wait(20000000) { C.exit(1) }
    elapsed := sync.monotonic_now() - started
    if elapsed < 1000000 || elapsed > 1000000000 { C.exit(2) }
    sem.post()
    if !sem.timed_wait(1000000) { C.exit(3) }
    worker := spawn monitor(&sem)
    C.usleep(25000)
    sem.post()
    if worker.wait() < 1 { C.exit(4) }
    sem.destroy()
    C.alarm(0)
}
'])
		assert result.exit_code == 0, 'exit ${result.exit_code}: ${result.output}'
	}
}

fn test_native_directory_listing_skips_dot_entries_before_recursive_cleanup() {
	$if macos && arm64 {
		root := os.join_path(os.vtmp_dir(), 'arm64_directory_${os.getpid()}')
		empty := os.join_path(root, 'empty')
		target := os.join_path(root, 'target')
		nested := os.join_path(target, 'nested')
		guard := os.join_path(root, 'outside_guard')
		os.mkdir_all(empty)!
		os.mkdir_all(nested)!
		os.write_file(guard, 'outside sentinel')!
		os.write_file(os.join_path(target, '.hidden'), 'hidden')!
		os.write_file(os.join_path(target, '..prefix'), 'prefix')!
		os.write_file(os.join_path(nested, 'child'), 'child')!
		defer {
			os.rmdir_all(root) or {}
		}
		result := native_poll_sync_fixture('directory', [
			r'module os
pub fn ls(path string) ![]string { return []string{} }
pub fn is_dir(path string) bool { return false }
pub fn is_link(path string) bool { return false }
',
			'module main
import os
fn C.exit(int)
fn C.alarm(u32) u32
fn C.unlink(&u8) int
fn C.rmdir(&u8) int
fn entries(path string) []string {
    items := os.ls(path) or { C.exit(1); return []string{} }
    for item in items {
        if item == "." || item == ".." { C.exit(2) }
    }
    return items
}
fn cleanup(path string) {
    for item in entries(path) {
        child := path + "/" + item
        if os.is_dir(child) && !os.is_link(child) {
            cleanup(child)
        } else if C.unlink(child.str) != 0 { C.exit(3) }
    }
    if C.rmdir(path.str) != 0 { C.exit(4) }
}
fn main() {
    C.alarm(5)
    if entries("${empty}").len != 0 { C.exit(5) }
    items := entries("${target}")
    if items.len != 3 { C.exit(6) }
    mut hidden := false
    mut prefix := false
    mut directory := false
    for item in items {
        if item == ".hidden" { hidden = true }
        if item == "..prefix" { prefix = true }
        if item == "nested" { directory = true }
    }
    if !hidden || !prefix || !directory { C.exit(7) }
    cleanup("${target}")
    cleanup("${empty}")
    C.alarm(0)
}
',
		])
		assert result.exit_code == 0, 'exit ${result.exit_code}: ${result.output}'
		assert !os.exists(target)
		assert !os.exists(empty)
		assert os.read_file(guard)! == 'outside sentinel'
	}
}

fn native_poll_sync_fixture(name string, sources []string) os.Result {
	mut paths := []string{}
	for index, source in sources {
		path := os.join_path(os.vtmp_dir(), 'arm64_poll_sync_${name}_${os.getpid()}_${index}.v')
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
	mut preferences := pref.new_preferences()
	preferences.backend = 'arm64'
	mut p := parser.Parser.new(preferences)
	mut a := p.parse_files(paths)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	tc.annotate_types()
	assert tc.errors.len == 0, tc.errors.str()
	transform.transform(mut a, tc)
	m := ssa.build_with_used(a, map[string]bool{}, tc)
	if name == 'poll' {
		mut found := false
		for index, typ in m.type_store.types {
			if typ.kind == .struct_t && typ.field_names == ['fd', 'events', 'revents'] {
				poll_type := ssa.TypeID(index)
				assert m.type_size(poll_type) == 8
				assert m.struct_field_offset(poll_type, 1) == 4
				assert m.struct_field_offset(poll_type, 2) == 6
				assert m.type_store.types[typ.fields[1]].width == 16
				assert !m.type_store.types[typ.fields[1]].is_unsigned
				found = true
			}
		}
		assert found
	}
	mut g := Gen.new(m)
	g.gen()
	g.write_and_link(output)
	return os.exec([output])
}
