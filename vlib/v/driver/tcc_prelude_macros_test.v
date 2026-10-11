module driver

fn test_directives_are_the_lines_that_start_with_a_hash_outside_comments() {
	text := '#include <a.h>\n  # define  ONE 1 /* one */\n/*\n#define COMMENTED 1\n*/\n#if defined(A) && \\\n    B > 2 // both\nint x; # none\n#endif\nconst char *s = "/*";\n#undef ONE\n'
	directives := v3_c_directives(text)
	assert directives.map(it.name) == ['include', 'define', 'if', 'endif', 'undef']
	assert directives[1].rest.trim_space() == 'ONE 1'
	assert directives[2].rest.contains('defined(A) &&') && directives[2].rest.contains('B > 2')
	assert !directives[2].rest.contains('both')
}

fn test_macros_of_a_prelude_are_those_that_its_text_asks_about_and_defines() {
	mut macros := V3PreludeMacros{}
	macros.add('#ifdef __builtin_offsetof\n#elif defined(OTHER) && VALUE > 1\n#endif\n#undef GONE\n#define PLAIN 1\n#define CALL(a, b) a b\n#define LOG(fmt, args...) printf(fmt, args)\n#define ANY(...) f(__VA_ARGS__)\n#define SPLIT(a, \\\n  rest ...) a rest\nint x;\n')
	assert macros.names_to_test() == ['ANY', 'CALL', 'GONE', 'LOG', 'OTHER', 'PLAIN', 'SPLIT',
		'VALUE', '__builtin_offsetof']
	assert macros.named_variadic == {
		'LOG':   ['args']
		'SPLIT': ['rest']
	}
	assert macros.fixed == {
		'CALL': ['b']
	}
}

fn test_variable_arguments_are_put_back_into_the_definitions_that_tcc_prints() {
	mut macros := V3PreludeMacros{}
	macros.add('#define LOG(fmt, args...) printf(fmt, args)\n#define SPLIT(rest...) rest\n#define CALL(a, b) a b\n')
	restored, unusable := macros.with_variable_arguments('#define ANY(__VA_ARGS__) f(__VA_ARGS__)\n#define FMT(fmt,__VA_ARGS__) g(fmt, ## __VA_ARGS__)\n#define LOG(fmt,args) printf(fmt, args)\n#define SPLIT(rest) rest\n#define CALL(a,b) a b\n#define OBJECT (a,__VA_ARGS__)\n#undef ANY\n#define NONE() 1\n')
	assert unusable == ''
	assert restored == '#define ANY(...) f(__VA_ARGS__)\n#define FMT(fmt,...) g(fmt, ## __VA_ARGS__)\n#define LOG(fmt,args...) printf(fmt, args)\n#define SPLIT(rest...) rest\n#define CALL(a,b) a b\n#define OBJECT (a,__VA_ARGS__)\n#undef ANY\n#define NONE() 1\n'
	// What comes out of it is what goes in, when TinyCC prints it again.
	again, _ := macros.with_variable_arguments(restored.replace('(...)', '(__VA_ARGS__)').replace('fmt,...',
		'fmt,__VA_ARGS__').replace('args...', 'args').replace('rest...', 'rest'))
	assert again == restored
	// A macro that the text defines in both ways cannot be told.
	macros.add('#define LOG(fmt, args) plain(fmt, args)\n')
	_, both := macros.with_variable_arguments('#define LOG(fmt,args) printf(fmt, args)\n')
	assert both.contains('`LOG`')
}

fn test_probes_tell_which_names_the_compiler_and_the_preprocessor_define() {
	names := ['A', 'B', 'C']
	assert v3_tcc_preprocessor_probe(names) == '#ifdef A\n${v3_tcc_prelude_defined_probe} 0\n#endif\n#ifdef B\n${v3_tcc_prelude_defined_probe} 1\n#endif\n#ifdef C\n${v3_tcc_prelude_defined_probe} 2\n#endif\n'
	// What the preprocessor leaves of the probe is no C of the prelude.
	form := v3_tcc_split_preprocessed('# 1 "verify.c"\n${v3_tcc_prelude_defined_probe} 1\nint x;\n')
	assert form.defined == [1] && form.code == 'int x;\n'
	assert v3_tcc_compiler_probe(names, [1]) == '#ifdef A\n#error ${v3_tcc_prelude_differs_probe} A\n#endif\n#ifndef B\n#error ${v3_tcc_prelude_differs_probe} B\n#endif\n#ifdef C\n#error ${v3_tcc_prelude_differs_probe} C\n#endif\n'
	differing := v3_tcc_probe_difference('probe.c:5: error: #error ${v3_tcc_prelude_differs_probe} __builtin_offsetof\n') or {
		''
	}
	assert differing == '__builtin_offsetof'
	other := v3_tcc_probe_difference('probe.c:5: error: include file not found\n') or { '' }
	assert other == ''
}

fn test_names_that_a_macro_of_a_condition_expands_to_are_asked_about_too() {
	asked, expanded := v3_c_condition_names('defined(A) && B(C) > 1 || defined D && !E')
	assert asked == ['A', 'D'] && expanded == ['B', 'C', 'E']
	// What a macro expands to is no question by itself, and neither is it when
	// a condition asks whether the macro is defined.
	mut used := V3PreludeMacros{}
	used.add('#define offsetof(t, f) __builtin_offsetof(t, f)\n#ifdef OTHER\n#endif\n#ifndef offsetof\n#endif\n#if defined(offsetof)\n#endif\n')
	assert used.names_to_test() == ['OTHER', 'offsetof']
	// It is one when a condition expands the macro, however far away.
	mut asked_far := V3PreludeMacros{}
	asked_far.add('#define HAS_OFFSETOF defined(__builtin_offsetof)\n#define HAS_IT (HAS_OFFSETOF && LEVEL)\n')
	asked_far.add('#if HAS_IT\nint yes;\n#endif\n')
	assert asked_far.names_to_test() == ['HAS_IT', 'HAS_OFFSETOF', 'LEVEL', '__builtin_offsetof']
	// A macro that names itself ends the search.
	mut circular := V3PreludeMacros{}
	circular.add('#define A B\n#define B A\n#if A\n#endif\n')
	assert circular.names_to_test() == ['A', 'B']
}
