import os
import v3.gen.c as cgen
import v3.markused
import v3.parser
import v3.pref
import v3.transform
import v3.types

fn gen_c_for_c_global_source(name string, source string) string {
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

fn gen_c_for_c_global_sources(name string, files map[string]string) string {
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
	tc.diagnostic_files[os.join_path(root, 'main.v')] = true
	tc.check_semantics()
	assert tc.errors.len == 0, tc.errors.str()
	transform.transform(mut a, &tc)
	tc.annotate_types()
	used_fns := markused.mark_used(a, tc)
	mut g := cgen.FlatGen.new()
	return g.gen_with_used_options(a, used_fns, &tc, true)
}

fn test_v_owned_global_with_c_struct_type_gets_storage() {
	c_code := gen_c_for_c_global_source('v_owned_c_struct_global', 'struct C.NativeDesc {
mut:
	value int
}

__global owned C.NativeDesc
__global C.stdout &C.FILE

fn set_stream_unbuffered(stream &C.FILE) {}

fn set_owned() {
	owned = C.NativeDesc{
		value: 7
	}
}

fn owned_score() int {
	return owned.value
}

fn main() {
	set_owned()
	_ := owned_score()
	set_stream_unbuffered(C.stdout)
}
')
	assert c_code.contains('\nNativeDesc owned = {0};'), c_code
	assert c_code.contains('owned = (NativeDesc){'), c_code
	assert c_code.contains('.value = 7'), c_code
	assert c_code.contains('return owned.value;'), c_code
	assert c_code.contains('set_stream_unbuffered(stdout);'), c_code
	assert !c_code.contains('C__stdout'), c_code
	assert !c_code.contains('\nFILE* stdout'), c_code
}

fn test_module_v_owned_global_with_c_struct_type_gets_qualified_storage() {
	c_code := gen_c_for_c_global_sources('v_owned_c_struct_global_module', {
		'main.v':      'module main

import moda

fn main() {
	moda.set_owned()
	_ := moda.owned_score()
}
'
		'moda/moda.v': 'module moda

struct C.NativeDesc {
mut:
	value int
}

__global owned C.NativeDesc
__global C.stdout &C.FILE

fn set_stream_unbuffered(stream &C.FILE) {}

pub fn set_owned() {
	owned = C.NativeDesc{
		value: 7
	}
}

pub fn owned_score() int {
	set_stream_unbuffered(C.stdout)
	return owned.value
}
'
	})
	assert c_code.contains('\nNativeDesc moda__owned = {0};'), c_code
	assert c_code.contains('moda__owned = (NativeDesc){'), c_code
	assert c_code.contains('.value = 7'), c_code
	assert c_code.contains('return moda__owned.value;'), c_code
	assert !c_code.contains('\nNativeDesc owned = {0};'), c_code
	assert c_code.contains('set_stream_unbuffered(stdout);'), c_code
	assert !c_code.contains('C__stdout'), c_code
	assert !c_code.contains('\nFILE* stdout'), c_code
}
