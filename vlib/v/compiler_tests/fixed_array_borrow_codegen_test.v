module main

import os

fn fixed_array_borrow_body(source string, function string) string {
	for line in source.split_into_lines() {
		if line.contains(' ${function}(') && line.ends_with(' {') {
			return source.all_after(line).all_before('\n}')
		}
	}
	panic('missing generated function ${function}')
}

fn test_fixed_array_scalar_read_write_callees_borrow_stack_storage() {
	root := os.join_path(os.vtmp_dir(), 'fixed_array_borrow_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source_path := os.join_path(root, 'main.v')
	c_path := os.join_path(root, 'main.c')
	os.write_file(source_path, 'struct Slice { start int; len int }
struct Params {
mut:
 n int
 vals [8]Slice
}
fn set_params(mut p Params, i int) { p.vals[i & 7] = Slice{i, 1}; p.n++ }
fn read_params(p &Params, i int) int { return p.vals[i & 7].start + p.n }
fn (mut p Params) set(i int) { p.vals[i & 7] = Slice{i, 1}; p.n++ }
fn params_by_mut(i int) int { mut p := Params{}; set_params(mut p, i); return p.n }
fn params_by_ref(i int) int { p := Params{}; return read_params(&p, i) }
fn params_by_unsafe_ref(i int) int {
 p := Params{}
 ptr := unsafe { &p }
 return read_params(ptr, i)
}
fn params_by_method(i int) int { mut p := Params{}; p.set(i); return p.n }
fn retain(p &Params) []Slice { return p.vals[..] }
fn params_retained(i int) []Slice { mut p := Params{}; p.vals[0] = Slice{i, 1}; return retain(&p) }
fn retain_pointer(p &Params) &Slice { return &p.vals[0] }
fn params_retained_pointer(i int) &Slice {
 mut p := Params{}
 p.vals[0] = Slice{i, 1}
 return retain_pointer(&p)
}
fn forward(p &Params, cb fn (&Params) int) int { return cb(p) }
fn params_forwarded(i int) int {
 p := Params{}
 return forward(&p, fn (p &Params) int { return p.n }) + i
}
fn main() {
 assert params_by_mut(3) == 1
 assert params_by_ref(3) == 0
 assert params_by_unsafe_ref(3) == 0
 assert params_by_method(3) == 1
 assert params_retained(7)[0].start == 7
 assert params_retained_pointer(8).start == 8
 assert params_forwarded(9) == 9
 println("OK")
}
')!
	generated := os.exec([@VEXE, '-new-compiler', '-gc', 'none', '-o', c_path, source_path])
	assert generated.exit_code == 0, generated.output
	source := os.read_file(c_path)!
	for name in ['params_by_mut', 'params_by_ref', 'params_by_unsafe_ref', 'params_by_method'] {
		body := fixed_array_borrow_body(source, name)
		assert body.len > 0, name
		assert !body.contains('memdup('), body
	}
	for name in ['params_retained', 'params_retained_pointer', 'params_forwarded'] {
		body := fixed_array_borrow_body(source, name)
		assert body.contains('memdup('), body
	}
	run := os.exec([@VEXE, '-new-compiler', '-gc', 'none', 'run', source_path])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'OK', run.output
	warnings := os.exec([@VEXE, '-new-compiler', '-warn-about-allocs', '-o', c_path, source_path])
	assert warnings.exit_code == 0, warnings.output
	assert warnings.output.contains('local moved to the heap: its fixed array storage may escape'), warnings.output
}

fn test_fixed_array_borrow_summary_does_not_follow_shadowed_function_names() {
	root := os.join_path(os.vtmp_dir(), 'fixed_array_borrow_shadow_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source_path := os.join_path(root, 'main.v')
	c_path := os.join_path(root, 'main.c')
	os.write_file(source_path, 'struct Payload {
mut:
 n int
 values [8]int
}
__global retained = &Payload(unsafe { nil })
fn borrow(p &Payload) int { return p.n }
fn retain(p &Payload) int { retained = p; return p.n }
fn shadowed_local(i int) int {
 borrow := retain
 mut p := Payload{n: i}
 p.values[0] = i + 1
 return borrow(&p)
}
fn shadowed_nested(i int) int {
 if i > 0 {
  borrow := retain
  mut p := Payload{n: i}
  p.values[0] = i + 1
  return borrow(&p)
 }
 return 0
}
fn shadowed_parameter(borrow fn (&Payload) int, i int) int {
 mut p := Payload{n: i}
 p.values[0] = i + 1
 return borrow(&p)
}
fn after_sibling_shadow(i int) int {
 if i < 0 {
  borrow := retain
  _ = borrow(retained)
 }
 p := Payload{n: i}
 return borrow(&p)
}
fn main() {
 assert shadowed_local(10) == 10
 assert retained.values[0] == 11
 assert shadowed_nested(20) == 20
 assert retained.values[0] == 21
 assert shadowed_parameter(retain, 30) == 30
 assert retained.values[0] == 31
 assert after_sibling_shadow(40) == 40
 println("OK")
}
')!
	generated := os.exec([@VEXE, '-new-compiler', '-no-retry-compilation', '-enable-globals', '-gc',
		'none', '-o', c_path, source_path])
	assert generated.exit_code == 0, generated.output
	source := os.read_file(c_path)!
	for name in ['shadowed_local', 'shadowed_nested', 'shadowed_parameter'] {
		body := fixed_array_borrow_body(source, name)
		assert body.contains('memdup('), body
	}
	assert !fixed_array_borrow_body(source, 'after_sibling_shadow').contains('memdup(')
	mut binary_path := os.join_path(root, 'main')
	$if windows {
		binary_path += '.exe'
	}
	compiled := os.exec([@VEXE, '-new-compiler', '-no-retry-compilation', '-enable-globals', '-cc',
		'clang', '-gc', 'none', '-o', binary_path, source_path])
	assert compiled.exit_code == 0, compiled.output
	run := os.exec([binary_path])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'OK', run.output
}
