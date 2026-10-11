module driver

import os
import time
import v.modulecache

// A build that links cached modules gives TinyCC the headers of its program unit
// in preprocessed form. These tests cover how the unit is divided and what the
// preprocessed form is valid for.

fn test_prelude_ends_after_the_last_include_and_outside_any_group() {
	unit := '#define A 1
#include <stdio.h>
int x;
#if defined(_WIN32)
#include <windows.h>
#else
#include <unistd.h>
#endif
typedef int late;
int main(void) { return 0; }
'
	end := v3_tcc_prelude_end(unit)
	assert unit[..end].ends_with('#include <unistd.h>\n#endif\n')
	assert unit[end..].starts_with('typedef int late;')
	// An include at the top level ends the prelude on its own line.
	flat := '#include <a.h>\nint x;\n#include <b.h>\nint y;\n'
	assert flat[..v3_tcc_prelude_end(flat)] == '#include <a.h>\nint x;\n#include <b.h>\n'
}

fn test_prelude_is_not_taken_from_a_unit_that_it_cannot_be_cut_from() {
	// Nothing to set aside.
	assert v3_tcc_prelude_end('int main(void) { return 0; }\n') == 0
	// Nothing would be left.
	assert v3_tcc_prelude_end('#include <stdio.h>\n') == 0
	// The group of the last include is never closed.
	assert v3_tcc_prelude_end('#if X\n#include <a.h>\nint x;\n') == 0
	// More groups are closed than opened.
	assert v3_tcc_prelude_end('#include <a.h>\n#endif\nint x;\n') == 0
}

fn test_prelude_ignores_directives_in_comments_and_continued_lines() {
	commented := '#include <a.h>\n/* a comment\n#include <b.h>\n#if 0\n*/\nint x;\n'
	assert commented[..v3_tcc_prelude_end(commented)] == '#include <a.h>\n'
	continued := '#include <a.h>\n#define M(x) \\\n#include <c.h>\nint x;\n'
	assert continued[..v3_tcc_prelude_end(continued)] == '#include <a.h>\n'
}

fn test_preprocess_args_are_the_compile_args_without_output_inputs_and_link_options() {
	args := ['-std=gnu11', '-fPIC', '-B/tcc/lib', '-I/tcc/lib/include', '-L/tcc/lib', '-w',
		'-Werror=implicit-function-declaration', '-bt25', '-o', 'out', 'src.c', '/cache/builtin.o',
		'-DGC_THREADS=1', '-I', '/gc/include', '/tcc/lib/libgc.dylib', '-Wl,-rpath,/tcc/lib', '-ldl',
		'-lpthread', '-L', '/usr/local/lib', '-framework', 'Cocoa']
	assert v3_tcc_preprocess_args(args, 'src.c') == ['-std=gnu11', '-fPIC', '-B/tcc/lib',
		'-I/tcc/lib/include', '-w', '-Werror=implicit-function-declaration', '-DGC_THREADS=1',
		'-I', '/gc/include']
	first, system, own := v3_tcc_include_dirs(v3_tcc_preprocess_args(args, 'src.c'))
	assert first == ['/tcc/lib/include', '/gc/include']
	assert system == []
	assert own == ['/tcc/lib/include']
	first_only, system_only, own_only := v3_tcc_include_dirs(['-B/tcc/lib', '-I/a', '-isystem',
		'/sys', '-isystem/other', '-I', '/b'])
	assert first_only == ['/a', '/b']
	assert system_only == ['/sys', '/other']
	assert own_only == ['/tcc/lib/include']
}

fn test_has_include_names_are_found() {
	source := '#if __has_include(<wchar.h>)
#include <wchar.h>
#endif
#if defined(__has_include) && __has_include( "local/config.h" )
#endif
#if __has_include_next(<stdint.h>)
#endif
int __has_include_is_not_asked_here;
'
	assert v3_has_include_names(source) == ['wchar.h', 'local/config.h', 'stdint.h']
}

fn test_first_missing_path_is_the_first_component_that_is_absent() {
	root := os.join_path(os.vtmp_dir(), 'v3_driver_tcc_prelude_paths_${os.getpid()}')
	os.rmdir_all(root) or {}
	defer {
		os.rmdir_all(root) or {}
	}
	os.mkdir_all(os.join_path(root, 'sys'))!
	os.write_file(os.join_path(root, 'sys', 'wait.h'), '')!
	assert v3_first_missing_path(root, 'sys/wait.h') == ''
	assert v3_first_missing_path(root, 'sys/time.h') == os.join_path(root, 'sys', 'time.h')
	assert v3_first_missing_path(root, 'mach/mach_time.h') == os.join_path(root, 'mach')
	assert v3_first_missing_path(os.join_path(root, 'absent'), 'a/b.h') == os.join_path(root,
		'absent')
}

fn test_preprocessed_files_are_those_of_the_line_markers() {
	build_dir := '/build/dir'
	preprocessed := '# 1 "${v3_tcc_prelude_input_name}"\n# 1 "<command line>" 1\n# 1 "/usr/include/a.h" 1\nint a;\n# 1 "generated.h" 1\nint g;\n# 1 "../shared/b.h" 1\n# 2 "/usr/include/a.h" 2\n'
	files, of_the_build, relative := v3_preprocessed_files(preprocessed, build_dir)
	// The preprocessor names a file below the directory that it runs in relative to
	// it: such a file is one that the build made for itself. One that it names
	// relative to that directory and that is elsewhere is a file by where the
	// output goes.
	assert files.sorted() == ['/build/shared/b.h', '/usr/include/a.h']
	assert of_the_build == '/build/dir/generated.h'
	assert relative
	plain, none_of_the_build, plain_relative := v3_preprocessed_files('# 1 "/usr/include/a.h" 1\n',
		build_dir)
	assert plain == ['/usr/include/a.h'] && none_of_the_build == ''
	assert !plain_relative
}

fn test_prelude_stamp_is_valid_while_its_inputs_are_what_they_were() {
	root := os.join_path(os.vtmp_dir(), 'v3_driver_tcc_prelude_stamp_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	header := os.join_path(root, 'a.h')
	absent := os.join_path(root, 'b.h')
	os.write_file(header, 'int a;\n')!
	identity := modulecache.file_metadata_signature(header)
	// Inputs that cannot all be told leave no stamp.
	assert v3_tcc_prelude_stamp('key', V3HeaderInputs{
		files:      [header]
		identities: [identity]
		unknown:    'a header appeared'
	}, '') == none
	assert v3_tcc_prelude_stamp('key', V3HeaderInputs{ files: [header] }, '') == none
	if identity == '' {
		// This file system cannot tell a later edit apart: nothing is kept for it.
		return
	}
	stamp := v3_tcc_prelude_stamp('key', V3HeaderInputs{
		files:      [header]
		identities: [identity]
		missing:    [absent]
	}, '') or {
		assert false, 'a header with an identity can be recorded'
		return
	}
	record := v3_read_tcc_prelude_stamp(stamp, 'key') or {
		assert false, 'the stamp is read back'
		return
	}
	assert record.unusable == '' && !record.inputs.mentions_time
	assert record.inputs.files == [header] && record.inputs.identities == [identity]
	assert record.inputs.missing == [absent]
	assert v3_tcc_prelude_inputs_are_unchanged(&record.inputs)
	// Why a preprocessed form cannot be used holds as long as the inputs do, and
	// so does what the headers say of the time.
	unusable := v3_tcc_prelude_stamp('key', V3HeaderInputs{
		files:         [header]
		identities:    [identity]
		missing:       [absent]
		mentions_time: true
	}, 'its macros change\nthe C') or {
		assert false, 'a prelude that cannot be used can be recorded'
		return
	}
	unusable_record := v3_read_tcc_prelude_stamp(unusable, 'key') or {
		assert false, 'the stamp is read back'
		return
	}
	assert unusable_record.unusable == 'its macros change the C'
	assert unusable_record.inputs.mentions_time
	assert unusable_record.inputs.files == [header]
	assert v3_read_tcc_prelude_stamp(stamp, 'other key') == none
	assert v3_read_tcc_prelude_stamp(stamp.all_before_last('complete=1'), 'key') == none
	assert v3_read_tcc_prelude_stamp(stamp.replace('missing=', 'unknown='), 'key') == none
	// A header that appears where none was changes what an include finds.
	os.write_file(absent, 'int b;\n')!
	assert !v3_tcc_prelude_inputs_are_unchanged(&record.inputs)
	os.rm(absent)!
	assert v3_tcc_prelude_inputs_are_unchanged(&record.inputs)
	// So does a header that is another file than it was.
	os.rm(header)!
	os.write_file(header, 'long a;\n')!
	assert !v3_tcc_prelude_inputs_are_unchanged(&record.inputs)
}

fn test_build_time_macros_keep_a_prelude_out_of_the_cache() {
	assert v3_tcc_source_has_build_time_macros('const char* built = __DATE__;\n')
	assert v3_tcc_source_has_build_time_macros('int n = __COUNTER__;\n')
	assert !v3_tcc_source_has_build_time_macros('#include <stdio.h>\nint line = __LINE__;\n')
}

fn test_preprocessed_prelude_is_its_c_followed_by_its_definitions() {
	preprocessed := '# 1 "${v3_tcc_prelude_input_name}"
# 1 "<command line>" 1
#define __TINYC__ 928
#define __BASE_FILE__ "${v3_tcc_prelude_input_name}"
# 1 "${v3_tcc_prelude_input_name}" 2
# 1 "/usr/include/next.h" 1
static int next(int n) { return n; }
#define next(n) next((n) + 1)

#pragma pack(push, 1)
static inline int value(void) { return next((1) + 1); }
  #undef unused
# 2 "${v3_tcc_prelude_input_name}" 2
${v3_tcc_prelude_counter_probe} 0
'
	form := v3_tcc_split_preprocessed(preprocessed)
	assert form.unusable == ''
	// The macros that the compiler defines itself are not those of the prelude.
	assert form.code == 'static int next(int n) { return n; }\n#pragma pack(push, 1)\nstatic inline int value(void) { return next((1) + 1); }\n'
	assert form.definitions == '#define next(n) next((n) + 1)\n#undef unused\n'
	assert form.text() == form.code + form.definitions
	// The form that a preprocessor makes of the form is the same when no macro
	// expands in it again.
	same := v3_tcc_split_preprocessed('# 1 "verify.c"\n# 1 "<command line>" 1\n#define __TINYC__ 928\n# 1 "verify.c" 2\n# 1 "/cache/prelude.i" 1\nstatic int next(int n) { return n; }\n#pragma pack(push, 1)\nstatic inline int value(void)   { return next((1) + 1); }\n#define next(n) next((n) + 1)\n#undef unused\n')
	assert same.unusable == ''
	assert v3_tcc_preprocessed_forms_match(&form, &same)
	again := V3TccPreprocessed{
		code:        form.code.replace('next((1) + 1)', 'next(((1) + 1) + 1)')
		definitions: form.definitions
	}
	assert !v3_tcc_preprocessed_forms_match(&form, &again)
	other_definitions := V3TccPreprocessed{
		code:        form.code
		definitions: '#define next(n) next((n) + 1)\n'
	}
	assert !v3_tcc_preprocessed_forms_match(&form, &other_definitions)
}

fn test_preprocessed_prelude_cannot_be_used_for_what_depends_on_the_compilation() {
	marker := '# 1 "/usr/include/a.h" 1\n'
	assert v3_tcc_split_preprocessed(marker + 'int a;\n${v3_tcc_prelude_counter_probe} 0\n').unusable == ''
	assert v3_tcc_split_preprocessed(marker + 'int a_0;\n${v3_tcc_prelude_counter_probe} 1\n').unusable.contains('__COUNTER__')
	assert v3_tcc_split_preprocessed(marker +
		'const char *file = "${v3_tcc_prelude_input_name}";\n').unusable.contains('name of the file')
	assert v3_tcc_split_preprocessed(marker + 'const char *built = "Oct 10 2026";\n').unusable.contains('time')
	assert v3_tcc_split_preprocessed(marker + 'const char *built = "Jan  1 2026" " " "09:08:07";\n').unusable.contains('time')
	assert v3_tcc_split_preprocessed(marker + 'char quote = \'"\'; const char *at = "01:02:03";\n').unusable.contains('time')
	assert v3_tcc_split_preprocessed(marker + 'const char *text = "October 10"; char c = \'"\';\n').unusable == ''
	assert v3_tcc_split_preprocessed(marker + '#pragma push_macro("N")\n').unusable.contains('directive')
	assert v3_tcc_split_preprocessed(marker + '#include_next <a.h>\n').unusable.contains('directive')
}

fn test_c_tokens_text_keeps_what_separates_tokens_and_what_literals_hold() {
	assert v3_c_tokens_text('  int\ta ;\n\n int  b;') == 'int a ; int b;'
	assert v3_c_tokens_text('a + +b') != v3_c_tokens_text('a ++b')
	assert v3_c_tokens_text('puts("a  b");') == 'puts("a  b");'
	assert v3_c_tokens_text('puts("a  \\"  b");  x') == 'puts("a  \\"  b"); x'
	assert v3_c_tokens_text("char c = ' ';  char d = '\\'';") == "char c = ' '; char d = '\\'';"
	assert v3_c_tokens_text('#define F(x) x') != v3_c_tokens_text('#define F (x) x')
}

fn test_include_directories_of_the_environment_come_after_those_of_the_command() {
	saved := ['CPATH', 'C_INCLUDE_PATH'].map(os.getenv_opt(it))
	defer {
		for i, name in ['CPATH', 'C_INCLUDE_PATH'] {
			if value := saved[i] {
				os.setenv(name, value, true)
			} else {
				os.unsetenv(name)
			}
		}
	}
	os.setenv('CPATH', ['/env/first', '', '/env/second', '/env/first'].join(os.path_delimiter),
		true)
	assert v3_environment_dirs('CPATH') == ['/env/first', '/env/second']
	os.unsetenv('CPATH')
	assert v3_environment_dirs('CPATH') == []
	assert v3_tcc_absolute_include_dirs(['/a', 'inc', '../shared', '/a'], '/build/dir') == [
		'/a',
		'/build/dir/inc',
		'/build/shared',
	]
	// TinyCC searches the directories of `-I`, of CPATH, of `-isystem`, of
	// C_INCLUDE_PATH and its own, in that order, and then those of the system.
	os.setenv('CPATH', '/env/cpath', true)
	os.setenv('C_INCLUDE_PATH', '/env/c_include', true)
	manager := modulecache.new_manager(os.join_path(os.vtmp_dir(), 'v3_driver_tcc_search_${os.getpid()}'),
		'salt', false, '', '')
	search := v3_tcc_include_search(&manager, '/nonexistent/tcc', ['-B/tcc/lib', '-isystem', '/sys',
		'-I/option'], '/build/dir')
	assert search.dirs == ['/option', '/env/cpath', '/sys', '/env/c_include', '/tcc/lib/include']
	assert search.frameworks == []
	assert search.absent == []
}

fn test_the_rest_of_a_unit_keeps_its_lines_after_the_preprocessed_prelude() {
	source := '#include <a.h>\n#define ONE 1\n#include <b.h>\nint line = __LINE__;\n'
	end := v3_tcc_prelude_end(source)
	assert source[end..] == 'int line = __LINE__;\n'
	assert v3_tcc_prelude_unit('/cache/form.i', source, end) == '#include "/cache/form.i"\n#line 4\nint line = __LINE__;\n'
}

fn test_a_prelude_that_a_unit_does_not_compile_with_is_not_used_again() {
	dir := os.join_path(os.vtmp_dir(), 'v3_tcc_prelude_reject_${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir)!
	defer {
		os.rmdir_all(dir) or {}
	}
	form := os.join_path(dir, 'tcc_prelude_key.i')
	stamp := form + '.stamp'
	os.write_file(form, 'int x;\n')!
	prelude := V3TccPrelude{
		form:  form
		stamp: stamp
		key:   'key'
	}
	prelude.reject('TinyCC does not compile a unit that reads it: src.c:9: error')
	record := v3_read_tcc_prelude_stamp(os.read_file(stamp)!, 'key') or { panic('no record') }
	assert record.unusable.contains('does not compile a unit')
	assert !os.exists(form)
	// A form that is gone was not read: nothing is said of it.
	os.rm(stamp)!
	prelude.reject('again')
	assert !os.exists(stamp)
}

fn test_a_date_or_a_time_of_day_in_a_string_is_what_a_time_macro_leaves() {
	assert v3_c_has_time_literal('const char *s = "\\"Oct 11 2026\\"";')
	assert v3_c_has_time_literal('const char *s = "built Jan  1 2026";')
	assert v3_c_has_time_literal('const char *s = "Sun Oct 11 08:11:38 2026";')
	assert !v3_c_has_time_literal('const char *s = "October 11, 2026 at 8:11"; int t = 12:34:56;')
	// A `'` between the digits of a number opens no character literal.
	assert v3_c_has_time_literal('int n = 1\'000; const char *t = "08:11:38";')
}

fn test_the_first_error_of_a_compiler_is_not_where_a_file_was_included_from() {
	assert v3_first_c_error_line('In file included from src.c:1:\n/cache/form.i:9: error: ; expected\n') == '/cache/form.i:9: error: ; expected'
	assert v3_first_c_error_line('tcc: file not found\nmore\n') == 'tcc: file not found'
}
