import os
import v3.flat
import v3.gen.c as cgen
import v3.markused
import v3.parser
import v3.pref
import v3.transform
import v3.types

// gen_c_for_source emits c for source output for v3 tests.
fn gen_c_for_source(name string, source string) string {
	src := os.join_path(os.temp_dir(), 'v3_${name}.v')
	os.write_file(src, source) or { panic(err) }
	prefs := pref.new_preferences()
	mut p := parser.Parser.new(prefs)
	mut a := p.parse_file(src)
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	tc.diagnose_unknown_calls = true
	tc.diagnostic_files[src] = true
	tc.check_semantics()
	assert tc.errors.len == 0, tc.errors.str()
	transform.transform(mut a, &tc)
	tc.annotate_types()
	used_fns := markused.mark_used(a, tc)
	mut g := cgen.FlatGen.new()
	return g.gen_with_used_options(a, used_fns, &tc, true)
}

// gen_c_for_sources emits c for sources output for v3 tests.
fn gen_c_for_sources(name string, files map[string]string) string {
	root := os.join_path(os.temp_dir(), 'v3_${name}')
	if os.exists(root) {
		os.rmdir_all(root) or { panic(err) }
	}
	os.mkdir_all(root) or { panic(err) }
	mut paths := []string{}
	mut rels := files.keys()
	rels.sort()
	for rel in rels {
		path := os.join_path(root, rel)
		os.mkdir_all(os.dir(path)) or { panic(err) }
		os.write_file(path, files[rel]) or { panic(err) }
		paths << path
	}
	prefs := pref.new_preferences()
	mut p := parser.Parser.new(prefs)
	mut a := p.parse_files(paths)
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	tc.diagnose_unknown_calls = true
	for path in paths {
		tc.diagnostic_files[path] = true
	}
	tc.check_semantics()
	assert tc.errors.len == 0, tc.errors.str()
	transform.transform(mut a, &tc)
	tc.annotate_types()
	used_fns := markused.mark_used(a, tc)
	mut g := cgen.FlatGen.new()
	return g.gen_with_used_options(a, used_fns, &tc, true)
}

// gen_c_for_source_with_scalar_zero_decl supports gen_c_for_source_with_scalar_zero_decl handling.
fn gen_c_for_source_with_scalar_zero_decl(name string, source string, decl_name string, decl_type string) string {
	src := os.join_path(os.temp_dir(), 'v3_${name}.v')
	os.write_file(src, source) or { panic(err) }
	prefs := pref.new_preferences()
	mut p := parser.Parser.new(prefs)
	mut a := p.parse_file(src)
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	tc.diagnose_unknown_calls = true
	tc.diagnostic_files[src] = true
	tc.check_semantics()
	assert tc.errors.len == 0, tc.errors.str()
	transform.transform(mut a, &tc)
	tc.annotate_types()
	zero_id := a.add_node(flat.Node{
		kind:  .int_literal
		value: '0'
	})
	mut patched := false
	for i, node in a.nodes {
		if node.kind != .decl_assign || node.children_count != 2 {
			continue
		}
		lhs_id := a.child(&node, 0)
		lhs := a.nodes[int(lhs_id)]
		if lhs.kind == .ident && lhs.value == decl_name {
			a.nodes[i].typ = decl_type
			a.children[int(node.children_start) + 1] = zero_id
			patched = true
		}
	}
	assert patched
	used_fns := markused.mark_used(a, tc)
	mut g := cgen.FlatGen.new()
	return g.gen_with_used_options(a, used_fns, &tc, true)
}

// test_c_global_pointer_arg_is_not_addressed_again validates this v3 regression case.
fn test_c_global_pointer_arg_is_not_addressed_again() {
	c_code := gen_c_for_source('c_global_stdout_arg', '__global C.stdout &C.FILE

fn set_stream_unbuffered(stream &C.FILE) {}

fn main() {
	set_stream_unbuffered(C.stdout)
}
')
	assert c_code.contains('set_stream_unbuffered(stdout);')
	assert !c_code.contains('set_stream_unbuffered(&stdout);')
}

// test_mut_static_local_decl_codegen validates this v3 regression case.
fn test_mut_static_local_decl_codegen() {
	c_code := gen_c_for_source('static_local_decl', 'fn next_value() int {
	mut static x := 0
	x++
	return x
}

fn main() {
	_ := next_value()
}
')
	assert c_code.contains('static int x = 0;')
}

// test_aggregate_globals_are_brace_zero_initialized validates this v3 regression case.
fn test_aggregate_globals_are_brace_zero_initialized() {
	c_code := gen_c_for_source('aggregate_global_zero_init', 'const zero_len = 1 - 1

struct Box {
	value int
}

struct ZeroLeading {
	z [0]int
	value int
}

struct NestedZeroLeading {
	zero ZeroLeading
	value int
}

struct ZeroLeadingConst {
	z [zero_len]int
	value int
}

__global (
	names []string
	lookup map[string]int
	box Box
	empty [0]int
	slots [2]int
	zero ZeroLeading
	nested NestedZeroLeading
	zero_slots [2]ZeroLeading
	const_empty [zero_len]int
	const_zero ZeroLeadingConst
	const_zero_slots [2]ZeroLeadingConst
)

fn main() {}
')
	assert c_code.contains('Array names = {0};')
	assert c_code.contains('map lookup = {0};')
	assert c_code.contains('Box box = {0};')
	assert c_code.contains('int empty[0];')
	assert c_code.contains('int slots[2] = {0};')
	assert c_code.contains('\nZeroLeading zero;\n')
	assert c_code.contains('\nNestedZeroLeading nested;\n')
	assert c_code.contains('\nZeroLeading zero_slots[2];\n')
	assert c_code.contains('\nint const_empty[0];\n')
	assert c_code.contains('\nZeroLeadingConst const_zero;\n')
	assert c_code.contains('\nZeroLeadingConst const_zero_slots[2];\n')
	assert !c_code.contains('Array names = 0;')
	assert !c_code.contains('map lookup = 0;')
	assert !c_code.contains('Box box = 0;')
	assert !c_code.contains('int empty[0] = {0};')
	assert !c_code.contains('ZeroLeading zero = {0};')
	assert !c_code.contains('NestedZeroLeading nested = {0};')
	assert !c_code.contains('ZeroLeading zero_slots[2] = {0};')
	assert !c_code.contains('\nint const_empty[0] = {0};\n')
	assert !c_code.contains('ZeroLeadingConst const_zero = {0};')
	assert !c_code.contains('ZeroLeadingConst const_zero_slots[2] = {0};')
}

// test_imported_zero_length_leading_field_global_uses_decl_only validates this v3 regression case.
fn test_imported_zero_length_leading_field_global_uses_decl_only() {
	c_code := gen_c_for_sources('imported_zero_leading_global', {
		'main.v':      'module main

import moda

__global imported moda.ImportedZero
__global imported_slots [2]moda.ImportedZero

fn main() {}
'
		'moda/moda.v': 'module moda

const zero_len = 1 - 1

pub struct ImportedZero {
	z [zero_len]int
	value int
}
'
	})
	assert c_code.contains('\nmoda__ImportedZero imported;\n')
	assert c_code.contains('\nmoda__ImportedZero imported_slots[2];\n')
	assert !c_code.contains('moda__ImportedZero imported = {0};')
	assert !c_code.contains('moda__ImportedZero imported_slots[2] = {0};')
}

// test_aggregate_decl_with_scalar_zero_uses_brace_initializer validates this v3 regression case.
fn test_aggregate_decl_with_scalar_zero_uses_brace_initializer() {
	c_code := gen_c_for_source_with_scalar_zero_decl('aggregate_decl_scalar_zero', 'struct Box {
	value int
}

fn main() {
	box := Box{}
	_ = box.value
}
',
		'box', 'Box')
	assert c_code.contains('Box box = {0};'), c_code
	assert !c_code.contains('Box box = 0;')
}

// test_defer_capture_aggregate_decl_with_scalar_zero_uses_compound_literal
// validates this v3 regression case.
fn test_defer_capture_aggregate_decl_with_scalar_zero_uses_compound_literal() {
	c_code := gen_c_for_source_with_scalar_zero_decl('defer_capture_aggregate_decl_scalar_zero', 'struct Box {
	value int
}

fn main() {
	box := Box{}
	defer(fn) {
		_ = box.value
	}
}
',
		'box', 'Box')
	assert c_code.contains('box = (Box){0};'), c_code
	assert !c_code.contains('box = {0};')
}
