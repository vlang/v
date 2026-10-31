module c

fn check_libc_variant_merge(headerless string, headers string) string {
	merged := merge_libc_variants(headerless, headers)
	assert libc_variant_source(merged, true) == headerless.trim_right('\n')
	assert libc_variant_source(merged, false) == headers.trim_right('\n')
	return merged
}

fn test_identical_variants_are_not_guarded() {
	src := '#include <x.h>\nint a;\n#if X\nint b;\n#endif\n'
	merged := check_libc_variant_merge(src, src)
	assert merged == headerless_libc_prologue + src
}

fn test_a_declaration_of_one_variant_is_guarded() {
	merged := check_libc_variant_merge('int a;\nint own(void);\nint b;\n', 'int a;\nint b;\n')
	assert merged.contains('#if ${headerless_libc_macro}\nint own(void);\n#endif\n')
	merged2 := check_libc_variant_merge('int a;\nint b;\n', 'int a;\n#include <own.h>\nint b;\n')
	assert merged2.contains('#if !${headerless_libc_macro}\n#include <own.h>\n#endif\n')
}

fn test_a_differing_conditional_is_guarded_as_a_whole() {
	// Only the `#if` lines differ, but a guard around them alone would take the
	// `#endif` of the file's block for its own.
	merged := check_libc_variant_merge('a\n#if P\nb\n#endif\nc\n', 'a\n#if Q\nb\n#endif\nc\n')
	assert merged.contains('#if ${headerless_libc_macro}\n#if P\nb\n#endif\n#else\n#if Q\nb\n#endif\n#endif\n')
}

fn test_a_branch_of_one_variant_takes_its_whole_block() {
	check_libc_variant_merge('#if X\na\n#endif\nz\n', '#if X\na\n#elif Y\nb\n#endif\nz\n')
	check_libc_variant_merge('x\n#ifdef X\n#if Y\na\n#else\nb\n#endif\n#endif\n', 'x\n#ifdef X\n#if Y\na\n#else\nc\n#endif\n#endif\n')
	check_libc_variant_merge('\t#if A\n\tx;\n\t#endif\n', '\t#if B\n\tx;\n\t#endif\n')
}

fn test_line_continuations_and_comments_are_not_split() {
	headerless := '#define M(x) \\\n\tfoo(x)\n/* one\ntwo */\nint a;\n'
	headers := '#define M(x) \\\n\tbar(x)\n/* one\nthree */\nint a;\n'
	merged := check_libc_variant_merge(headerless, headers)
	assert merged.contains('#if ${headerless_libc_macro}\n#define M(x) \\\n\tfoo(x)\n/* one\ntwo */\n#else\n')
	assert c_preprocessor_units('char* s = "/*";\nint a;\n') == ['char* s = "/*";\n', 'int a;\n']
	assert c_preprocessor_units('char c = \'"\';\nint a; // /*\nint b;') == [
		'char c = \'"\';\n',
		'int a; // /*\n',
		'int b;',
	]
}

fn test_reordered_and_repeated_lines_merge() {
	check_libc_variant_merge('a\nb\nc\nd\ne\n', 'a\nd\nc\nb\ne\n')
	check_libc_variant_merge('#if Z\n#endif\n#if A\nx\n#endif\n', '#if Z\n#endif\n#if B\nx\ny\n#endif\n')
	check_libc_variant_merge('', 'int a;\n')
	check_libc_variant_merge('int a;', 'int a;\nint b;')
}
