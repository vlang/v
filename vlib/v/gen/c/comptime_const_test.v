module c

import os
import v.markused
import v.parser
import v.pref
import v.transform
import v.types

fn test_comptime_values_are_static_without_initialization_code() {
	path := os.join_path(os.vtmp_dir(), 'static_comptime_${os.getpid()}.v')
	os.write_file(path, '@[comptime]\nfn table() [3]u32 { mut a := [3]u32{} for i in 0 .. 3 { a[i] = u32(i * i) } return a }\nconst squares = table()\n@[comptime]\nfn label() string { return "hello" + " world" }\nconst name = label()\n@[comptime]\nfn word() u32 { return 0xffffffff }\nconst value = word()\nfn main() { assert squares[2] == 4 assert name.len == 11 }')!
	defer { os.rm(path) or {} }
	mut p := parser.Parser.new(pref.new_preferences())
	mut a := p.parse_file(path)
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	tc.check_semantics()
	assert tc.errors.len == 0, tc.errors.str()
	assert tc.comptime_const_values.len == 3, tc.comptime_const_values.keys().str()
	transform.transform(mut a, &tc)
	tc.annotate_types()
	used, _ := markused.mark_all_used_with_generic_usage(a, tc, [])
	mut g := FlatGen.new()
	generated := g.gen_with_used_options(a, used, &tc, true)
	assert generated.contains('const u32 main__squares[3] = {0LL, 1LL, 4LL};'), generated.all_after('string label(void);')
	assert generated.contains('const string main__name = {(u8*)"hello world", 11, 1};'), generated.all_after('string label(void);')
	assert generated.contains('#define main__value ((u32)(4294967295LL))'), generated.all_after('string label(void);')
	assert !generated.contains('memmove(main__squares,'), generated.all_after('string label(void);')
	assert !generated.contains('main__name = label('), generated.all_after('string label(void);')
	for casts in [false, true] {
		mut checked := FlatGen.new()
		checked.set_check_overflow(!casts)
		checked.set_check_casts(casts)
		_ = checked.gen_with_used_options(a, used, &tc, true)
		assert checked.output_error.contains('required evaluation with -check-overflow or -check-casts'), checked.output_error
	}
}
