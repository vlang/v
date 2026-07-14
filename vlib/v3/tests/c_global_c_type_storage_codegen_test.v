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

fn test_c_pointer_cast_selector_uses_cast_field_value() {
	c_code := gen_c_for_c_global_source('c_pointer_cast_selector', 'module main

@[typedef]
struct C.log__Logger {
mut:
	_object voidptr
}

fn raw_object(logger &C.log__Logger) voidptr {
	pobject := &C.log__Logger(logger)._object
	return pobject
}

fn main() {
	logger := &C.log__Logger(voidptr(0))
	_ := raw_object(logger)
}
')
	assert c_code.contains('void* pobject = ((struct log__Logger*)logger)->_object;')
	assert !c_code.contains('void** pobject')
	assert !c_code.contains('__addr_')
}

fn test_mut_parameter_address_uses_lowered_pointer() {
	c_code := gen_c_for_c_global_source('mut_parameter_address', 'module main

struct Reader {
mut:
	pos int
}

struct ReaderBox {
mut:
	rdr &Reader
}

fn ReaderBox.new(rdr &Reader) ReaderBox {
	return ReaderBox{
		rdr: rdr
	}
}

fn wrap(mut read_from Reader) ReaderBox {
	return ReaderBox.new(&read_from)
}

fn main() {
	mut reader := Reader{}
	_ := wrap(mut reader)
}
')
	assert c_code.contains('ReaderBox__new(read_from)'), c_code
	assert !c_code.contains('ReaderBox__new(&read_from)'), c_code
}

fn test_mut_parameter_address_boxed_as_interface_uses_concrete_pointer() {
	c_code := gen_c_for_c_global_source('mut_parameter_address_interface', 'module main

interface Reader {
	read() int
}

struct FileReader {}

fn (r FileReader) read() int {
	return 1
}

struct ReaderBox {
mut:
	rdr &Reader
}

fn ReaderBox.new(rdr &Reader) ReaderBox {
	return ReaderBox{
		rdr: rdr
	}
}

fn wrap(mut read_from FileReader) ReaderBox {
	return ReaderBox.new(&read_from)
}

fn main() {
	mut reader := FileReader{}
	_ := wrap(mut reader)
}
')
	assert c_code.contains('._object = read_from'), c_code
	assert c_code.contains('(Reader){._typ = '), c_code
	assert !c_code.contains('FileReader** __iface_src_'), c_code
}

fn test_global_pointer_interface_arg_does_not_take_global_address() {
	c_code := gen_c_for_c_global_source('global_pointer_interface_arg', 'module main

interface Logger {
mut:
	free()
}

struct Log {}

fn (mut l Log) free() {}

__global default_logger &Logger = &Logger(Log{})

fn deinit() {
	free_logger(default_logger)
}

fn free_logger(logger &Logger) {
	logger.free()
}

fn main() {
	deinit()
}
')
	assert c_code.contains('Logger* default_logger;'), c_code
	assert c_code.contains('free_logger(default_logger);'), c_code
	assert !c_code.contains('free_logger(&default_logger);'), c_code
}

fn test_mut_parameter_pointer_return_stays_pointer() {
	c_code := gen_c_for_c_global_source('mut_parameter_pointer_return', 'module main

struct Builder {
mut:
	value int
}

fn identity(mut builder Builder) &Builder {
	builder.value = 1
	return builder
}

fn main() {
	mut builder := Builder{}
	_ := identity(mut builder)
}
')
	assert c_code.contains('Builder* identity(Builder* builder)'), c_code
	assert c_code.contains('return builder;'), c_code
	assert !c_code.contains('return *(builder);'), c_code
}

fn test_pointer_struct_field_from_mut_parameter_stays_pointer() {
	c_code := gen_c_for_c_global_source('mut_parameter_pointer_struct_field', 'module main

struct Reader {
mut:
	pos int
}

struct Holder {
mut:
	rdr &Reader
}

fn Holder.new(mut rdr Reader) Holder {
	return Holder{
		rdr: rdr
	}
}

fn main() {
	mut rdr := Reader{}
	_ := Holder.new(mut rdr)
}
')
	assert c_code.contains('.rdr = rdr'), c_code
	assert !c_code.contains('.rdr = *(rdr)'), c_code
}

fn test_pointer_struct_field_from_selector_address_stays_pointer() {
	c_code := gen_c_for_c_global_source('selector_address_pointer_struct_field', 'module main

struct Config {}

struct Searcher {
mut:
	config Config
}

struct Holder {
mut:
	config &Config
}

fn Holder.new(searcher &Searcher) Holder {
	return Holder{
		config: &searcher.config
	}
}

fn main() {
	searcher := Searcher{}
	_ := Holder.new(&searcher)
}
')
	assert c_code.contains('.config = &searcher->config'), c_code
	assert !c_code.contains('.config = *(&searcher->config)'), c_code
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
