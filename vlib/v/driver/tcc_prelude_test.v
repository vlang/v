module driver

import os
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
	first, later := v3_tcc_include_dirs(v3_tcc_preprocess_args(args, 'src.c'))
	assert first == ['/tcc/lib/include', '/gc/include']
	// TinyCC's own headers are named by `-I` here, and are searched in that place.
	assert later == []
	first_only, later_only := v3_tcc_include_dirs(['-B/tcc/lib', '-I/a', '-isystem', '/sys',
		'-isystem/other', '-I', '/b'])
	assert first_only == ['/a', '/b']
	assert later_only == ['/tcc/lib/include', '/sys', '/other']
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

fn test_prelude_inputs_are_the_files_read_and_the_places_a_file_could_appear() {
	root := os.join_path(os.vtmp_dir(), 'v3_driver_tcc_prelude_inputs_${os.getpid()}')
	os.rmdir_all(root) or {}
	defer {
		os.rmdir_all(root) or {}
	}
	first := os.join_path(root, 'first')
	second := os.join_path(root, 'second')
	os.mkdir_all(os.join_path(second, 'sys'))!
	os.mkdir_all(first)!
	header := os.join_path(second, 'sys', 'wait.h')
	os.write_file(header, '#if __has_include(<optional.h>)\n#endif\n')!
	preprocessed := '# 1 "prelude.c"\n# 1 "${header}" 1\nint wait(void);\n# 2 "prelude.c" 2\n'
	inputs := v3_tcc_prelude_inputs(preprocessed, '#include <sys/wait.h>\n', [first, second],
		[]string{})
	assert inputs.files == [header]
	// `sys/wait.h` would be found in the first directory if `sys` appeared there,
	// and `optional.h` in either.
	assert inputs.missing == [os.join_path(first, 'optional.h'), os.join_path(first, 'sys'),
		os.join_path(second, 'optional.h')]
	assert v3_strip_preprocessor_line_markers(preprocessed) == 'int wait(void);\n'
	// A directory that is searched after the one of the header cannot hide it.
	reversed := v3_tcc_prelude_inputs(preprocessed, '', [second, first], []string{})
	assert reversed.missing == [os.join_path(first, 'optional.h'), os.join_path(second, 'optional.h')]
	// The place of a directory that is no `-I` one is not known: it counts as both.
	unordered := v3_tcc_prelude_inputs(preprocessed, '', []string{}, [second, first])
	assert os.join_path(first, 'sys') in unordered.missing
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
	if modulecache.file_metadata_signature(header) == '' {
		// This file system cannot tell a later edit apart: nothing is kept for it.
		assert v3_tcc_prelude_stamp('key', V3TccPreludeInputs{ files: [header] }) == none
		return
	}
	stamp := v3_tcc_prelude_stamp('key', V3TccPreludeInputs{
		files:   [header]
		missing: [absent]
	}) or {
		assert false, 'a header with an identity can be recorded'
		return
	}
	assert v3_tcc_prelude_stamp_is_valid(stamp, 'key')
	assert !v3_tcc_prelude_stamp_is_valid(stamp, 'other key')
	assert !v3_tcc_prelude_stamp_is_valid(stamp.all_before_last('complete=1'), 'key')
	// A header that appears where none was changes what an include finds.
	os.write_file(absent, 'int b;\n')!
	assert !v3_tcc_prelude_stamp_is_valid(stamp, 'key')
	os.rm(absent)!
	assert v3_tcc_prelude_stamp_is_valid(stamp, 'key')
	// So does a header that is another file than it was.
	os.rm(header)!
	os.write_file(header, 'long a;\n')!
	assert !v3_tcc_prelude_stamp_is_valid(stamp, 'key')
}

fn test_build_time_macros_keep_a_prelude_out_of_the_cache() {
	assert v3_tcc_source_has_build_time_macros('const char* built = __DATE__;\n')
	assert v3_tcc_source_has_build_time_macros('int n = __COUNTER__;\n')
	assert !v3_tcc_source_has_build_time_macros('#include <stdio.h>\nint line = __LINE__;\n')
}
