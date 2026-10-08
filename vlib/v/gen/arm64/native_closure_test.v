module arm64

import os
import v.parser
import v.pref
import v.ssa
import v.transform
import v.types

fn test_native_spawn_noreturn_worker_preserves_parent_execution() {
	$if macos && arm64 {
		for building_v in [false, true] {
			path := os.join_path(os.vtmp_dir(), 'arm64_spawn_noreturn_${building_v}_${os.getpid()}.v')
			output := path.all_before_last('.')
			defer {
				os.rm(path) or {}
				os.rm(output) or {}
			}
			os.write_file(path, 'module main
fn C.exit(int)
fn C.alarm(u32) u32
fn C.malloc(usize) voidptr
fn C.memcpy(voidptr, voidptr, usize) voidptr
fn memdup(source voidptr, size isize) voidptr {
    destination := C.malloc(usize(size))
    return C.memcpy(destination, source, usize(size))
}
@[noreturn]
fn C.pthread_exit(voidptr)
struct Payload {
    values [64]u64
    label string
}
@[noreturn]
fn leave_worker(payload Payload, marker &int) {
    if payload.values[0] != 11 || payload.values[63] != 74 || payload.label != "retained" {
        C.exit(2)
    }
    unsafe { *marker = 41 }
    C.pthread_exit(voidptr(unsafe { nil }))
}
fn main() {
    C.alarm(5)
    mut marker := 0
    mut values := [64]u64{}
    for i in 0 .. values.len { values[i] = u64(i + 11) }
    payload := Payload{values: values, label: "retained"}
    job := spawn leave_worker(payload, &marker)
    println("parent survived spawn")
    job.wait()
    if marker != 41 { C.exit(1) }
    C.alarm(0)
}
')!
			mut preferences := pref.new_preferences()
			preferences.backend = 'arm64'
			mut p := parser.Parser.new(preferences)
			mut a := p.parse_file(path)
			assert p.diagnostics.len == 0, p.diagnostics.str()
			mut tc := types.TypeChecker.new(a)
			tc.building_v_fast = building_v
			tc.collect(a)
			if !building_v {
				tc.annotate_types()
			}
			assert tc.errors.len == 0, tc.errors.str()
			if building_v {
				_, _, errors := transform.transform_with_used_opt_config_scoped_workers_checked(mut a,
					tc, map[string]bool{}, false, true, false, true)
				assert errors.len == 0, errors.str()
			} else {
				transform.transform(mut a, tc)
			}
			m := ssa.build_with_used(a, map[string]bool{}, tc)
			mut g := Gen.new(m)
			g.gen()
			g.write_and_link(output)
			result := os.exec(['env', 'MallocScribble=1', output])
			assert result.exit_code == 0, 'building_v=${building_v}: ${result.exit_code}: ${result.output}'
			assert result.output.contains('parent survived spawn'), result.output
		}
	}
}

fn test_native_capturing_closures_preserve_context_and_call_abi() {
	$if !macos || !arm64 {
		return
	}
	path := os.join_path(os.vtmp_dir(), 'arm64_closure_${os.getpid()}.v')
	output := path.all_before_last('.')
	test_compiler := output + '_compiler'
	defer {
		os.rm(path) or {}
		os.rm(output) or {}
		os.rm(test_compiler) or {}
	}
	os.write_file(path, 'module main
struct CaptureResult {
    total int
    label string
}
fn make_callback(offset int, label string) fn (int) CaptureResult {
    return fn [offset, label] (value int) CaptureResult {
        return CaptureResult{offset + value, label}
    }
}
fn main() {
    first := make_callback(7, "first")
    second := make_callback(11, "second")
    a := first(5)
    b := second(2)
    assert a.total == 12 && a.label == "first"
    assert b.total == 13 && b.label == "second"
    assert first(1).total == 8
    job := spawn first(3)
    c := job.wait()
    assert c.total == 10 && c.label == "first"
    scale := 2.5
    multiply := fn [scale] (value f64) f64 { return scale * value }
    assert multiply(4.0) == 10.0
}
')!
	compiler := os.getenv_opt('VEXE') or { @VEXE }
	mut compiled := os.exec([compiler, '-gc', 'none', '-b', 'arm64', '-o', output, path])
	if compiled.output.contains('ARM64 support is not compiled into this executable') {
		// C-only bootstrap compilers omit ARM64 dispatch; build the backend for this regression.
		bootstrap := os.exec([compiler, '-gc', 'none', '-d', 'skip_fastc', '-compile-backend',
			'arm64', '-o', test_compiler, os.join_path(@VEXEROOT, 'vlib', 'v', 'v.v')])
		assert bootstrap.exit_code == 0, bootstrap.output
		compiled = os.exec([test_compiler, '-gc', 'none', '-b', 'arm64', '-o', output, path])
	}
	assert compiled.exit_code == 0, compiled.output
	assert os.exists(output), compiled.output
	result := os.exec([output])
	assert result.exit_code == 0, result.output
}
