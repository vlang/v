module types

import os
import v.flat
import v.parser
import v.pref

fn test_declared_c_globals_keep_their_types_in_both_selector_paths() {
	path := os.join_path(os.vtmp_dir(), 'v3_c_global_selector_${os.getpid()}.c.v')
	os.write_file(path, 'module main
type Pixel = u8
@[c_extern]
__global (
	C.I_VideoBuffer &Pixel
	C.VIDEO_BUFFERS [2]&Pixel
	C.video_buffer &Pixel
	C.IMAGE_SCALE f64
)
fn main() {
	_ = C.I_VideoBuffer
	_ = C.VIDEO_BUFFERS
	_ = C.video_buffer
	_ = C.IMAGE_SCALE
}
')!
	defer { os.rm(path) or {} }
	for fast in [false, true] {
		mut p := parser.Parser.new(pref.new_preferences())
		a := p.parse_file(path)
		assert p.diagnostics.len == 0, p.diagnostics.str()
		mut tc := TypeChecker.new(a)
		tc.collect(a)
		tc.valid_resolution_fast = fast
		mut checked := 0
		for i, node in a.nodes {
			if node.kind != .selector || node.children_count == 0 {
				continue
			}
			base := a.child_node(&node, 0)
			if base.kind != .ident || base.value != 'C' {
				continue
			}
			expected := tc.c_globals['C.${node.value}'] or { panic('missing declared global') }
			id := flat.NodeId(i)
			assert tc.resolve_type(id) == expected
			tc.check_selector(id, node)
			assert tc.errors.len == 0, tc.errors.str()
			assert tc.resolve_type(id) == expected, 'fast=${fast}: C.${node.value}'
			checked++
		}
		assert checked == 4
	}
}

fn test_declared_uppercase_c_pointer_assignments_and_indexing() {
	path := os.join_path(os.vtmp_dir(), 'v3_c_global_pointer_${os.getpid()}.c.v')
	os.write_file(path, '@[translated]
module main
type Pixel = u8
@[c_extern]
__global C.I_VideoBuffer &Pixel
fn main() {
	mut pointer := &Pixel(unsafe { nil })
	pointer = C.I_VideoBuffer
	_ = voidptr(C.I_VideoBuffer + u32(1))
	unsafe { pointer[0] = 1 }
	unsafe { C.I_VideoBuffer[1] = 2 }
}
')!
	defer { os.rm(path) or {} }
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	_ = tc.check_semantics_opt(false)
	assert tc.errors.len == 0, tc.errors.str()
}

fn test_declared_uppercase_c_pointer_is_not_an_integer_macro() {
	path := os.join_path(os.vtmp_dir(), 'v3_c_global_pointer_mismatch_${os.getpid()}.c.v')
	os.write_file(path, 'module main
type Pixel = u8
@[c_extern]
__global C.I_VideoBuffer &Pixel
fn consume(value i32) {}
fn main() { consume(C.I_VideoBuffer) }
')!
	defer { os.rm(path) or {} }
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	_ = tc.check_semantics_opt(false)
	assert tc.errors.any(it.msg.contains('&Pixel') && it.msg.contains('i32')), tc.errors.str()
}
