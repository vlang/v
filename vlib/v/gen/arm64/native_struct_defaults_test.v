module arm64

import os
import v.parser
import v.pref
import v.ssa
import v.transform
import v.types

fn test_native_struct_defaults_include_omitted_mkdir_params_and_explicit_overrides() {
	$if !macos || !arm64 {
		return
	}
	decl := os.read_file(@VEXEROOT + '/vlib/os/os.v') or { panic(err) }
	mkdir_params := decl.all_after('@[params]\npub struct MkdirParams {').all_before('\n}')
	module_source := 'module defaults\n@[params]\npub struct MkdirParams {' + mkdir_params + '\n}\n' + r'
fn C.mkdir(&char, u32) int
pub fn mkdir(path string, params MkdirParams) int {
    return C.mkdir(path.str, params.mode)
}
const default_count = 37
fn initial_count() int { return default_count }
pub struct Inner {
pub:
    count int = initial_count()
    name string = "default"
}
pub type Value = Empty | Pointer
pub struct Empty { dummy_ u8 }
pub struct Pointer {
pub:
    child Value
    count int = 53
}
pub struct Outer {
pub:
    inner Inner
    enabled bool = true
    mode u32 = 0o600
    environment ?string = "VDIFF_CMD"
    missing ?int = none
    recursive Pointer
}
'
	for building_v in [false, true] {
		path := os.join_path(os.vtmp_dir(), 'arm64_struct_defaults_${building_v}_${os.getpid()}.v')
		module_path := path.all_before_last('.') + '_defaults.v'
		output := path.all_before_last('.')
		directory := output + '_directory'
		defer {
			os.rm(path) or {}
			os.rm(module_path) or {}
			os.rm(output) or {}
			os.rmdir(directory) or {}
		}
		source := 'module main\nimport defaults\nfn C.exit(int)\n' + r'
fn C.malloc(usize) voidptr
fn C.memcpy(voidptr, voidptr, usize) voidptr
fn memdup(source voidptr, size isize) voidptr {
    destination := C.malloc(usize(size))
    C.memcpy(destination, source, usize(size))
    return destination
}
fn main() {
' +
			'if defaults.mkdir("${directory}") != 0 { C.exit(1) }\n' + r'
    default_count := 99
    _ = default_count
    stack := defaults.Outer{}
    if stack.inner.count != 37 || stack.inner.name != "default" || !stack.enabled || stack.mode != u32(0o600) { C.exit(2) }
    heap := &defaults.Outer{}
    if heap.inner.count != 37 || heap.inner.name != "default" || !heap.enabled || heap.mode != u32(0o600) { C.exit(3) }
    if environment := stack.environment {
        if environment != "VDIFF_CMD" { C.exit(6) }
    } else { C.exit(7) }
    if stack.missing != none { C.exit(8) }
    if environment := heap.environment {
        if environment != "VDIFF_CMD" { C.exit(9) }
    } else { C.exit(10) }
    if heap.missing != none { C.exit(11) }
    if stack.recursive.count != 53 || heap.recursive.count != 53 { C.exit(15) }
    _ = stack.recursive.child
    stack_override := defaults.Outer{inner: defaults.Inner{count: 41}, enabled: false, mode: 0o700, environment: none}
    if stack_override.inner.count != 41 || stack_override.inner.name != "default" || stack_override.enabled || stack_override.mode != u32(0o700) { C.exit(4) }
    if stack_override.environment != none { C.exit(12) }
    heap_override := &defaults.Outer{inner: defaults.Inner{count: 43}, enabled: false, mode: 0o750, environment: "OTHER"}
    if heap_override.inner.count != 43 || heap_override.inner.name != "default" || heap_override.enabled || heap_override.mode != u32(0o750) { C.exit(5) }
    if environment := heap_override.environment {
        if environment != "OTHER" { C.exit(13) }
    } else { C.exit(14) }
}
'
		os.write_file(path, source) or { panic(err) }
		os.write_file(module_path, module_source) or { panic(err) }
		mut preferences := pref.new_preferences()
		preferences.backend = 'arm64'
		mut p := parser.Parser.new(preferences)
		mut a := p.parse_files([path, module_path])
		assert p.diagnostics.len == 0, p.diagnostics.str()
		mut tc := types.TypeChecker.new(a)
		tc.building_v_fast = building_v
		tc.collect(a)
		if !building_v { tc.annotate_types() }
		assert tc.errors.len == 0, tc.errors.str()
		if building_v {
			_, _, errors := transform.transform_with_used_opt_config_scoped_workers_checked(mut a, tc, map[string]bool{}, false, true, false, true)
			assert errors.len == 0, errors.str()
		} else {
			transform.transform(mut a, tc)
		}
		m := ssa.build_with_used(a, map[string]bool{}, tc)
		mut g := Gen.new(m)
		g.gen()
		g.write_and_link(output)
		result := os.exec([output])
		assert result.exit_code == 0, 'exit ${result.exit_code}: ${result.output}'
		directory_stat := os.stat(directory) or { panic(err) }
		assert directory_stat.mode & 0o700 == 0o700
	}
}
