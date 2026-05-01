module cleanc

import os
import v2.parser
import v2.pref as vpref
import v2.token
import v2.transformer
import v2.types

fn generate_c_for_generic_interface_cast_test(code string) string {
	tmp_file := '/tmp/v2_generic_interface_cast_codegen_${os.getpid()}.v'
	os.write_file(tmp_file, code) or { panic('failed to write temp file') }
	defer {
		os.rm(tmp_file) or {}
	}
	prefs := &vpref.Preferences{
		backend:     .cleanc
		no_parallel: true
	}
	mut file_set := token.FileSet.new()
	mut par := parser.Parser.new(prefs)
	files := par.parse_files([tmp_file], mut file_set)
	env := types.Environment.new()
	mut checker := types.Checker.new(prefs, file_set, env)
	checker.check_files(files)
	mut trans := transformer.Transformer.new_with_pref(files, env, prefs)
	mut gen := Gen.new_with_env_and_pref(trans.transform_files(files), env, prefs)
	return gen.gen()
}

fn test_generate_c_for_generic_interface_cast() {
	out := generate_c_for_generic_interface_cast_test('
struct ByteSet {}

struct Caps {}

interface Matcher[C] {
	non_matching_bytes[^a]() ?&^a ByteSet
}

struct MyMatcher {
	set ByteSet
}

fn (m &^a MyMatcher) non_matching_bytes[^a]() ?&^a ByteSet {
	return &m.set
}

fn main() {
	m := MyMatcher{}
	mi := Matcher[Caps](&m)
	if bs := mi.non_matching_bytes() {
		_ = bs
	}
}
')
	assert out.contains('Matcher mi = ((Matcher){._object = (void*)(&m),')
	assert out.contains('.non_matching_bytes = (_option_ByteSetptr (*)(void*))MyMatcher__non_matching_bytes')
}

fn test_generate_c_for_generic_fn_callback_with_ref_arg() {
	out := generate_c_for_generic_interface_cast_test('
struct Caps {
	x int
}

fn call_with_ref[T](x T, f fn (&T) bool) bool {
	return f(&x)
}

fn main() {
	x := Caps{3}
	ok := call_with_ref[Caps](x, fn (y &Caps) bool {
		return y.x == 3
	})
	assert ok
}
')
	assert out.contains('bool call_with_ref_T_Caps(Caps x, bool (*f)(Caps*)) {')
	assert !out.contains('void (*f)(Caps*)')
	assert out.contains('bool ok = call_with_ref_T_Caps(x, _anon_fn_0);')
}
