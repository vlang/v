module types

import os
import v.flat
import v.parser
import v.pref

// A selector can be checked again by a later pass, outside the `unsafe_depth` of
// the block it is in. Writing a union's fields inside `unsafe {}` must still not
// be reported as reading a union field outside `unsafe`.
fn test_union_field_inside_unsafe_block_checked_out_of_context() {
	path := os.join_path(os.vtmp_dir(), 'v3_union_unsafe_block_${os.getpid()}.v')
	os.write_file(path, 'module main
struct RwCmd {
mut:
	nsid u32
}
union CmdPrivate {
mut:
	rw  RwCmd
	raw u64
}
struct Command {
mut:
	private CmdPrivate
}
__global cmd = Command{}
fn main() {
	unsafe {
		cmd.private.rw.nsid = u32(1)
	}
}
')!
	defer {
		os.rm(path) or {}
	}
	mut prefs := pref.new_preferences()
	mut p := parser.Parser.new(prefs)
	a := p.parse_file(path)
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	mut found := false
	for i, node in a.nodes {
		if node.kind == .selector && node.value == 'rw' {
			found = true
			tc.check_node(flat.NodeId(i))
		}
	}
	assert found
	assert !tc.notices.any(it.msg.contains('reading a union field')), tc.notices.str()
	assert !tc.errors.any(it.msg.contains('reading a union field')), tc.errors.str()
}

// The same for a cast from voidptr, which outside `unsafe` is warned about.
fn test_voidptr_cast_inside_unsafe_block_checked_out_of_context() {
	path := os.join_path(os.vtmp_dir(), 'v3_voidptr_cast_unsafe_block_${os.getpid()}.v')
	os.write_file(path, 'module main
__global refcounts = voidptr(0)
fn main() {
	unsafe {
		(&u32(refcounts))[0] = 1
	}
}
')!
	defer {
		os.rm(path) or {}
	}
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	mut found := false
	for i, node in a.nodes {
		if node.kind == .cast_expr && node.value == '&u32' {
			found = true
			tc.check_node(flat.NodeId(i))
		}
	}
	assert found
	assert !tc.notices.any(it.msg.contains('casting voidptr')), tc.notices.str()
	assert !tc.errors.any(it.msg.contains('casting voidptr')), tc.errors.str()
}
