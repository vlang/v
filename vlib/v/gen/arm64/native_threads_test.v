module arm64

import os
import v.flat
import v.parser
import v.pref
import v.ssa
import v.types

fn test_native_spawn_overlaps_parent_and_returns_aggregate() {
	$if macos && arm64 {
		run_native_thread_fixture('aggregate', 'module main
fn C.exit(int)
fn C.alarm(u32) u32
fn C.pipe(voidptr) i32
fn C.read(i32, voidptr, usize) isize
fn C.write(i32, voidptr, usize) isize
fn C.close(i32) i32
fn C.pthread_self() u64
fn C.v3_pthread_is_current(u64) int
struct Payload { first i64 second i64 third i64 }
fn work(fd i32, parent u64) Payload {
    if C.pthread_self() == parent { C.exit(1) }
    if C.v3_pthread_is_current(parent) != 0 { C.exit(6) }
    if C.v3_pthread_is_current(C.pthread_self()) == 0 { C.exit(7) }
    mut marker := u8(0)
    if C.read(fd, &marker, 1) != 1 { C.exit(2) }
    return Payload{first: marker, second: 8, third: 9}
}
fn main() {
    C.alarm(5)
    if C.v3_pthread_is_current(C.pthread_self()) == 0 { C.exit(8) }
    mut fds := [i32(0), i32(0)]!
    if C.pipe(&fds[0]) != 0 { C.exit(3) }
    job := spawn work(fds[0], C.pthread_self())
    marker := u8(7)
    if C.write(fds[1], &marker, 1) != 1 { C.exit(4) }
    payload := job.wait()
    if payload.first != 7 || payload.second != 8 || payload.third != 9 { C.exit(5) }
    C.close(fds[0])
    C.close(fds[1])
    C.alarm(0)
}
', false)
	}
}

fn test_native_detached_spawn_performs_side_effect() {
	$if macos && arm64 {
		run_native_thread_fixture('detached', 'module main
fn C.exit(int)
fn C.alarm(u32) u32
fn C.pipe(voidptr) i32
fn C.read(i32, voidptr, usize) isize
fn C.write(i32, voidptr, usize) isize
fn C.close(i32) i32
fn C.pthread_self() u64
fn send(fd i32, parent u64) {
    if C.pthread_self() == parent { C.exit(1) }
    marker := u8(93)
    if C.write(fd, &marker, 1) != 1 { C.exit(2) }
}
fn main() {
    C.alarm(5)
    mut fds := [i32(0), i32(0)]!
    if C.pipe(&fds[0]) != 0 { C.exit(3) }
    spawn send(fds[1], C.pthread_self())
    mut marker := u8(0)
    if C.read(fds[0], &marker, 1) != 1 || marker != 93 { C.exit(4) }
    C.close(fds[0])
    C.close(fds[1])
    C.alarm(0)
}
', true)
	}
}

fn test_native_spawn_calls_indirect_function_values() {
	$if macos && arm64 {
		run_native_thread_fixture('callback', 'module main
fn C.exit(int)
fn add(value i64) i64 { return value + 35 }
fn launch(callback fn (i64) i64) i64 {
    job := spawn callback(7)
    return job.wait()
}

fn main() { if launch(add) != 42 { C.exit(1) } }
', false)
	}
}

fn run_native_thread_fixture(name string, source string, detached bool) {
	path := os.join_path(os.vtmp_dir(), 'arm64_thread_${name}_${os.getpid()}.v')
	output := path.all_before_last('.')
	defer {
		os.rm(path) or {}
		os.rm(output) or {}
	}
	os.write_file(path, source) or { panic(err) }
	mut p := parser.Parser.new(pref.new_preferences())
	mut a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	tc.annotate_types()
	assert tc.errors.len == 0, tc.errors.str()
	if detached {
		for i, node in a.nodes {
			if node.kind == .spawn_expr {
				a.nodes[i].flags |= flat.node_flag_detached_spawn
			}
		}
	}
	m := ssa.build_with_used(a, map[string]bool{}, tc)
	mut g := Gen.new(m)
	g.gen()
	g.write_and_link(output)
	result := os.exec([output])
	assert result.exit_code == 0, result.output
}
