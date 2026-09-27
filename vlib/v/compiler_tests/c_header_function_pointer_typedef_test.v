import os
import rand

const vexe = @VEXE

// A C header that the program includes declares its own types, and V has to
// leave the function-pointer typedefs in it alone. `struct Name` does not name
// such a typedef, so declaring one is a redefinition with a different type and
// the C compiler rejects the whole build with
// `typedef redefinition with different types`.
fn test_included_header_function_pointer_typedef_is_not_redeclared_as_a_struct() {
	root := os.join_path(os.vtmp_dir(), 'v3_c_header_fn_typedef_${os.getpid()}_${rand.ulid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'fn_typedef_probe' }\n") or {
		panic(err)
	}
	// The wrapped typedef matters as much as the single line one: the header in
	// vlib/x/multiwindow/testdata wraps most of its declarations. The definition
	// at the end is what makes V treat the header as a native source and scan the
	// declarations above it.
	os.write_file(os.join_path(root, 'probe.h'), 'typedef int (*ProbeSingleLineFn)(void *, int);\ntypedef int (*ProbeWrappedFn)(void *,\n\tint);\ntypedef struct ProbeRecordTag { int a; } ProbeRecord;\nstatic int probe_answer(void) { return 7; }\n') or {
		panic(err)
	}
	source := os.join_path(root, 'm.v')
	os.write_file(source, 'module main\n\n#include "@VMODROOT/probe.h"\n\nfn C.probe_answer() int\n\nfn main() {\n\tprintln(C.probe_answer())\n}\n') or {
		panic(err)
	}
	out := os.join_path(root, 'out.c')
	result := os.execute('${os.quoted_path(vexe)} -o ${os.quoted_path(out)} ${os.quoted_path(source)}')
	assert result.exit_code == 0, result.output
	c_code := os.read_file(out) or { panic(err) }
	for name in ['ProbeSingleLineFn', 'ProbeWrappedFn'] {
		assert !c_code.contains('struct ${name} {'), '`${name}` is a function pointer typedef, but V gave it a struct body'
		assert !c_code.contains('typedef struct ${name} ${name};'), '`${name}` is a function pointer typedef, but V re-declared it as a struct'
	}
}
