module driver

import os
import time
import v.cmdexec

fn test_precise_boehm_counts_each_small_allocation_after_warmup() {
	$if !(linux || macos) {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'v precise gc ${os.getpid()}_${time.now().unix_nano()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or { panic(err) }
	}
	// Avoid inheriting flags from the runner, including a V1 fallback.
	keys := ['VFLAGS', 'VOSARGS', 'V_MACOS_V3_NO_FALLBACK']
	mut old_values := map[string]string{}
	for key in keys {
		if value := os.getenv_opt(key) {
			old_values[key] = value
		}
	}
	defer {
		for key in keys {
			if value := old_values[key] {
				os.setenv(key, value, true)
			} else {
				os.unsetenv(key)
			}
		}
	}
	os.unsetenv('VFLAGS')
	os.unsetenv('VOSARGS')
	os.setenv('V_MACOS_V3_NO_FALLBACK', '1', true)
	vexe := if os.base(@VEXE) in ['v1_fallback', 'v1_fallback.exe'] {
		os.join_path(@VEXEROOT, 'v')
	} else {
		@VEXE
	}
	source := os.join_path(root, 'precise_gc.c.v')
	os.write_file(source, 'module main
#include <gc.h>
struct C.GC_prof_stats_s {
 bytes_allocd_since_gc uint
 allocd_bytes_before_gc uint
 gc_no uint
}
fn C.GC_get_prof_stats(&C.GC_prof_stats_s, uint) uint
fn allocated_bytes() u64 {
 mut stats := C.GC_prof_stats_s{}
 C.GC_get_prof_stats(&stats, sizeof(C.GC_prof_stats_s))
 return u64(stats.allocd_bytes_before_gc + stats.bytes_allocd_since_gc)
}
fn main() {
 mut retained := [][]u8{cap: 256}
 for _ in 0 .. 128 {
  retained << []u8{len: 16}
 }
 for i in 0 .. 20 {
  before := allocated_bytes()
  mut data := []u8{len: 16}
  data[0] = u8(i)
  after := allocated_bytes()
  if after < before || after - before < 16 { panic("GC counter missed an allocation") }
  retained << data
 }
 if retained.len != 148 { panic("wrong retained allocation count") }
 for i in 0 .. 20 {
  if retained[128 + i][0] != u8(i) { panic("retained allocation changed") }
 }
}
')!
	for production in [false, true] {
		flags_path := os.join_path(root, 'flags_${production}.txt')
		output := os.join_path(root, 'precise_${production}')
		mut args := ['-new-compiler', '-no-retry-compilation', '-b', 'c', '-d',
			'no_gc_thread_local_alloc', '-dump-c-flags', flags_path, '-o', output]
		if production {
			args << '-prod'
		}
		args << source
		compiled := cmdexec.run_with_timeout(vexe, args, 120_000)
		assert compiled.exit_code == 0, compiled.output
		flags := os.read_file(flags_path)!
		assert flags.contains('gc_o_'), flags
		assert !flags.contains('libgc.a'), flags
		assert !flags.contains('libgc.dylib'), flags
		assert !flags.contains('THREAD_LOCAL_ALLOC'), flags
		assert flags.contains('GC_THREADS=1'), flags
		run := cmdexec.run_with_timeout(output, [], 15_000)
		assert run.exit_code == 0, run.output
	}
}
