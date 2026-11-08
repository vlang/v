module fastc

import os
import v.pref
import v.cmdexec

fn test_conditional_native_includes_preserve_parallel_units() ! {
	root := os.join_path(os.real_path(os.vtmp_dir()), 'fastc_conditional_include_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'value.h'), '#define CONDITIONAL_VALUE 42\n')!
	os.write_file(os.join_path(root, 'helper.c.v'), r'module main
#if defined(__TINYC__)
#include "@DIR/value.h"
#else
#include "@DIR/value.h"
#endif
fn helper() int { return C.CONDITIONAL_VALUE }
')!
	os.write_file(os.join_path(root, 'main.v'), 'fn main() { assert helper() == 42 }\n')!
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	prefs.ccompiler = 'tinyc'
	prefs.backend = 'fastc'
	generation := generate_files_with_source_paths([root], prefs)!
	assert generation.units.unit_starts.len > 2
	program := os.join_path(root, 'program')
	built := cmdexec.run(@VEXE, ['-b', 'fastc', '-cc', 'tcc', '-nocache', '-o', program, root])
	assert built.exit_code == 0, built.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert !fastc_conditional_directives_only(['#if 1\nint value;\n#endif\n'])
	assert !fastc_conditional_directives_only(['#if 1\n#define VALUE 42\nint value;\n#endif\n'])
	// Macro-only blocks are shared like the unconditional directives hoisted to the head.
	assert fastc_conditional_directives_only(['#if 1\n#define VALUE 42\n#else\n#undef VALUE\n#endif\n'])
}

// A conditional block that defines an implementation macro before an include keeps one
// owner: a copy in every unit would define the header's functions in each.
fn test_conditional_implementation_include_keeps_one_owner() ! {
	assert !fastc_conditional_directives_only(['#if 1\n#define VALUE_IMPLEMENTATION\n#include "value.h"\n#endif\n'])
	assert fastc_conditional_directives_only(['#if 1\n#include "value.h"\n#endif\n'])
	root := os.join_path(os.real_path(os.vtmp_dir()), 'fastc_conditional_implementation_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'value.h'), 'int conditional_value(void);
#ifdef VALUE_IMPLEMENTATION
int conditional_value(void) { return 42; }
#endif
')!
	os.write_file(os.join_path(root, 'helper.c.v'), r'module main
#if 1
#define VALUE_IMPLEMENTATION
#include "@DIR/value.h"
#endif
fn C.conditional_value() int
fn helper() int { return C.conditional_value() }
')!
	os.write_file(os.join_path(root, 'main.v'), 'fn main() { assert helper() == 42 }\n')!
	previous_jobs := os.getenv_opt('VJOBS')
	os.setenv('VJOBS', '4', true)
	defer {
		if jobs := previous_jobs {
			os.setenv('VJOBS', jobs, true)
		} else {
			os.unsetenv('VJOBS')
		}
	}
	program := os.join_path(root, 'program')
	built := cmdexec.run(@VEXE, ['-b', 'fastc', '-cc', 'tcc', '-nocache', '-o', program, root])
	assert built.exit_code == 0, built.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
}
