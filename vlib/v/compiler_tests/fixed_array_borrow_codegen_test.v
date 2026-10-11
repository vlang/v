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

fn test_element_addresses_handed_to_borrowing_pointer_params_stay_on_the_stack() {
	root := os.join_path(os.vtmp_dir(), 'fixed_array_borrow_elem_${os.getpid()}')
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
__global retained_byte = &u8(unsafe { nil })
fn read_byte(data &u8, i int) u8 { return unsafe { data[i] } }
fn sum_bytes(data &u8, n int) int {
 mut s := 0
 for i in 0 .. n { s += int(read_byte(data, i)) }
 return s
}
fn fill_and_sum(mut rec [16]u8) int { rec[1] = 7; return sum_bytes(&rec[0], 16) }
fn fill_forward(mut rec [16]u8) int { return fill_and_sum(mut rec) }
fn elem_borrowed() int { mut rec := [16]u8{}; return fill_forward(mut rec) }
fn keep_byte(data &u8) { retained_byte = unsafe { data } }
fn store_elem(mut rec [16]u8) { keep_byte(&rec[0]) }
fn elem_stored() u8 { mut rec := [16]u8{}; store_elem(mut rec); return rec[0] }
fn give_back(data &u8) &u8 { return unsafe { data } }
fn return_elem(mut rec [16]u8) &u8 { return give_back(&rec[0]) }
fn elem_returned() &u8 { mut rec := [16]u8{}; return return_elem(mut rec) }
fn retain_slice(mut p Params) []Slice { return p.vals[..] }
fn forward_to_retainer(mut p Params) []Slice { return retain_slice(mut p) }
fn forwarded_retained(i int) []Slice {
 mut p := Params{}
 p.vals[0] = Slice{i, 1}
 return forward_to_retainer(mut p)
}
fn recurse(mut rec [16]u8, depth int) int {
 if depth == 0 { return int(rec[0]) }
 return recurse(mut rec, depth - 1)
}
fn recursive_caller() int { mut rec := [16]u8{}; return recurse(mut rec, 3) }
fn main() {
 assert elem_borrowed() == 7
 assert elem_stored() == 0
 assert unsafe { *elem_returned() } == 0
 assert forwarded_retained(5)[0].start == 5
 assert recursive_caller() == 0
 println("OK")
}
')!
	generated := os.exec([@VEXE, '-new-compiler', '-no-retry-compilation', '-enable-globals', '-gc',
		'none', '-o', c_path, source_path])
	assert generated.exit_code == 0, generated.output
	source := os.read_file(c_path)!
	// the element address reaches only callees that read through it, two calls deep
	assert !fixed_array_borrow_body(source, 'elem_borrowed').contains('memdup(')
	// stored, returned, forwarded to a retainer, or through a recursion not yet known to borrow
	for name in ['elem_stored', 'elem_returned', 'forwarded_retained', 'recursive_caller'] {
		body := fixed_array_borrow_body(source, name)
		assert body.contains('memdup('), body
	}
	run := os.exec([@VEXE, '-new-compiler', '-no-retry-compilation', '-enable-globals', '-gc',
		'none', 'run', source_path])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'OK', run.output
}

fn test_a_generic_caller_uses_the_summary_of_a_callee_it_reaches_first() {
	root := os.join_path(os.vtmp_dir(), 'fixed_array_borrow_generic_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source_path := os.join_path(root, 'main.v')
	c_path := os.join_path(root, 'main.c')
	os.write_file(source_path, 'struct Frame {
mut:
 id u32
 data [8]u8
}
fn fill(mut f Frame, id u32) { f.id = id; f.data[0] = u8(id) }
fn keep(mut f Frame) []u8 { return f.data[..] }
fn generic_borrowed[T](x T) u32 { _ = x; mut f := Frame{}; fill(mut f, 3); return f.id + u32(f.data[0]) }
fn generic_retained[T](x T) []u8 { _ = x; mut f := Frame{}; fill(mut f, 4); return keep(mut f) }
fn main() {
 assert generic_borrowed(1) == 6
 assert generic_retained(1)[0] == 4
 println("OK")
}
')!
	generated := os.exec([@VEXE, '-new-compiler', '-no-retry-compilation', '-gc', 'none', '-o',
		c_path, source_path])
	assert generated.exit_code == 0, generated.output
	source := os.read_file(c_path)!
	// specialized during monomorphization, before the callee's body is transformed
	assert !fixed_array_borrow_body(source, 'generic_borrowed_T_v_int').contains('memdup(')
	assert fixed_array_borrow_body(source, 'generic_retained_T_v_int').contains('memdup(')
	run := os.exec([@VEXE, '-new-compiler', '-no-retry-compilation', '-gc', 'none', 'run', source_path])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'OK', run.output
}

fn test_elements_passed_by_mut_or_copied_by_value_stay_on_the_stack() {
	root := os.join_path(os.vtmp_dir(), 'fixed_array_borrow_elem_mut_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source_path := os.join_path(root, 'main.v')
	c_path := os.join_path(root, 'main.c')
	os.write_file(source_path, 'type Gf = [4]i64
interface Sizer { size() int }
struct Box { g Gf }
fn (b Box) size() int { return int(b.g[0]) }
__global kept_ptr = voidptr(unsafe { nil })
__global kept_gf = &Gf(unsafe { nil })
__global kept_sizer = Sizer(Box{})
fn set_gf(mut r Gf, v i64) { for i in 0 .. 4 { r[i] = v } }
fn sum_gf(a Gf) i64 { return a[0] + a[1] + a[2] + a[3] }
fn keep_gf(mut r Gf) { kept_gf = unsafe { &r } }
fn keep_ptr(p voidptr) { kept_ptr = p }
fn keep_sizer(s Sizer) { kept_sizer = s }
fn pt_fill(mut p [2]Gf) i64 {
 set_gf(mut p[0], 1)
 set_gf(mut p[1], 2)
 return sum_gf(p[0]) + sum_gf(p[1])
}
fn by_elem_and_copy() i64 { mut p := [2]Gf{}; return pt_fill(mut p) }
fn pt_keep(mut p [2]Gf) { keep_gf(mut p[0]) }
fn elem_retained() i64 { mut p := [2]Gf{}; pt_keep(mut p); return p[0][0] }
fn rec_keep_ptr(mut rec [8]u8) { keep_ptr(rec) }
fn voidptr_retained() u8 { mut rec := [8]u8{}; rec_keep_ptr(mut rec); return rec[0] }
fn box_keep(mut b Box) { keep_sizer(b) }
fn interface_retained() int { mut b := Box{}; box_keep(mut b); return b.size() }
fn main() {
 assert by_elem_and_copy() == 12
 assert elem_retained() == 0
 assert voidptr_retained() == 0
 assert interface_retained() == 0
 println("OK")
}
')!
	generated := os.exec([@VEXE, '-new-compiler', '-no-retry-compilation', '-enable-globals', '-gc',
		'none', '-o', c_path, source_path])
	assert generated.exit_code == 0, generated.output
	source := os.read_file(c_path)!
	// an element handed by mut to a borrowing callee, and elements copied into value parameters
	assert !fixed_array_borrow_body(source, 'by_elem_and_copy').contains('memdup(')
	// kept by the callee, or passed where the parameter takes the address (voidptr, interface)
	for name in ['elem_retained', 'voidptr_retained', 'interface_retained'] {
		body := fixed_array_borrow_body(source, name)
		assert body.contains('memdup('), body
	}
	run := os.exec([@VEXE, '-new-compiler', '-no-retry-compilation', '-enable-globals', '-gc',
		'none', 'run', source_path])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'OK', run.output
}

fn test_an_index_through_a_method_is_a_call_that_receives_the_storage() {
	root := os.join_path(os.vtmp_dir(), 'fixed_array_borrow_index_method_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source_path := os.join_path(root, 'main.v')
	c_path := os.join_path(root, 'main.c')
	os.write_file(source_path, 'type Gf = [4]i64
__global kept_gf = &Gf(unsafe { nil })
fn (g &Gf) [] (_ int) i64 { kept_gf = unsafe { g }; return 7 }
fn take(x i64) i64 { return x }
fn gf_copy(mut p Gf) i64 { return take(p[1]) }
fn copy_retained() i64 { mut g := Gf{}; return gf_copy(mut g) }
fn gf_scalar(mut p Gf) i64 { return p[1] + 1 }
fn scalar_retained() i64 { mut g := Gf{}; return gf_scalar(mut g) }
fn main() {
 assert copy_retained() == 7
 assert scalar_retained() == 8
 println("OK")
}
')!
	generated := os.exec([@VEXE, '-new-compiler', '-no-retry-compilation', '-enable-globals', '-gc',
		'none', '-o', c_path, source_path])
	assert generated.exit_code == 0, generated.output
	source := os.read_file(c_path)!
	// `p[1]` calls the method, which keeps its receiver: the caller's array must outlive the frame
	for name in ['copy_retained', 'scalar_retained'] {
		body := fixed_array_borrow_body(source, name)
		assert body.contains('memdup('), body
	}
	run := os.exec([@VEXE, '-new-compiler', '-no-retry-compilation', '-enable-globals', '-gc',
		'none', 'run', source_path])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'OK', run.output
}
