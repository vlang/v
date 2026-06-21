import os
import v3.gen.c as cgen
import v3.markused
import v3.parser
import v3.pref
import v3.transform
import v3.types

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
