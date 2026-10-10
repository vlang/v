module driver

import os
import time
import v.modulecache

fn test_compiled_sources_are_the_operands_that_a_compiler_knows_as_sources() {
	assert v3_compiled_sources(['-O2', '-o', 'out', 'src.c', '/cache/module.o', '-I', '/inc/extra.c',
		'-lm', '-x', 'objective-c', 'main.m', '-x', 'none', '-MF', 'deps.d', '-include', '/inc/force.h',
		'/lib/libfoo.a']) == ['src.c', 'main.m']
	assert v3_compiled_sources(['-o', 'out', '/cache/main.o', '/cache/os.o', '-lm']) == []
}

fn test_dependency_file_names_what_a_compiler_read() {
	text := 'out: src.c /usr/include/stdio.h \\\n /usr/include/with\\ space.h \\\n  /usr/include/hash\\#mark.h /usr/include/dollar$$.h\n\n/usr/include/stdio.h:\n'
	assert v3_parse_dependency_file(text) == ['src.c', '/usr/include/stdio.h',
		'/usr/include/with space.h', '/usr/include/hash#mark.h', '/usr/include/dollar$.h']
	assert v3_parse_dependency_file('') == []
	assert v3_parse_dependency_file('out.o: \\\r\n a.c b.h\r\n') == ['a.c', 'b.h']
}

fn test_include_search_is_read_from_what_a_compiler_prints() {
	gcc := 'ignoring nonexistent directory "/usr/local/include/x86_64-linux-gnu"
ignoring duplicate directory "/usr/include"
#include "..." search starts here:
 /quoted
#include <...> search starts here:
 /first
 /usr/lib/gcc/x86_64-linux-gnu/13/include
 /usr/local/include
 /usr/include
End of search list.
# 0 "/dev/null"
'
	search := v3_parse_include_search(gcc) or {
		assert false, 'the search list of GCC is not read'
		return
	}
	assert search.dirs == ['/quoted', '/first', '/usr/lib/gcc/x86_64-linux-gnu/13/include',
		'/usr/local/include', '/usr/include']
	assert search.absent == ['/usr/local/include/x86_64-linux-gnu']
	clang := '#include "..." search starts here:
#include <...> search starts here:
 /usr/local/include
 /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include
 /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/System/Library/Frameworks (framework directory)
End of search list.
'
	frameworks := v3_parse_include_search(clang) or {
		assert false, 'the search list of Clang is not read'
		return
	}
	assert frameworks.dirs == ['/usr/local/include',
		'/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include']
	// An answer without its end is none.
	assert v3_parse_include_search('#include <...> search starts here:\n /usr/include\n') == none
	assert v3_include_search_args(['-O2', '-o', 'out', 'src.c', '-I/a', '-I', '/b', '-isystem',
		'/c', '-lm', '-std=gnu11', '-m64', '-nostdinc', '-Wl,-s', '-D', 'X=1']) == [
		'-I/a',
		'-I',
		'/b',
		'-isystem',
		'/c',
		'-std=gnu11',
		'-m64',
		'-nostdinc',
	]
}

fn test_headers_of_a_tcc_unit_are_inputs_of_its_executable() {
	mut inputs := V3ProgramLinkInputs{
		taken:      true
		files:      ['/lib/libm.a']
		identities: ['m']
	}
	// A unit that includes nothing has no headers.
	inputs.add_tcc_headers(&V3TccPrelude{})
	assert inputs.unknown == '' && inputs.files == ['/lib/libm.a']
	inputs.add_tcc_headers(&V3TccPrelude{
		has_headers: true
		stamp:       'format=x\nkey=k\nunusable=why\nfile=/usr/include/stdio.h\tidentity of stdio\nfile=/lib/libm.a\tother\nmissing=/first/stdio.h\ncomplete=1\n'
	})
	assert inputs.unknown == ''
	assert inputs.files == ['/lib/libm.a', '/usr/include/stdio.h']
	assert inputs.identities == ['m', 'identity of stdio']
	assert inputs.missing == ['/first/stdio.h']
	// Headers that cannot be told from changed ones leave no executable behind.
	inputs.add_tcc_headers(&V3TccPrelude{
		has_headers: true
	})
	assert inputs.unknown.contains('headers')
}

fn test_headers_that_a_compiler_read_are_inputs_of_its_executable() {
	$if windows {
		return
	}
	compiler := os.find_abs_path_of_executable('cc') or { return }
	root := os.join_path(os.vtmp_dir(), 'v3_driver_compile_inputs_${os.getpid()}')
	os.rmdir_all(root) or {}
	defer {
		os.rmdir_all(root) or {}
	}
	build_dir := os.join_path(root, 'build')
	first := os.join_path(root, 'first')
	second := os.join_path(root, 'second')
	absent := os.join_path(root, 'absent')
	for dir in [build_dir, first, os.join_path(second, 'sub')] {
		os.mkdir_all(dir)!
	}
	header := os.join_path(second, 'sub', 'answer.h')
	os.write_file(header, '#if __has_include(<optional_answer.h>)\n#endif\nstatic inline int answer(void) { return 41; }\n')!
	unit := '#include <sub/answer.h>\nint main(void) { return answer() - 41; }\n'
	os.write_file(os.join_path(build_dir, 'src.c'), unit)!
	manager := modulecache.new_manager(os.join_path(root, 'cache'), 'salt', true, '', '')
	args := ['-I${first}', '-I', second, '-I${absent}', '-o', 'out', 'src.c', '-MD', '-MF',
		v3_program_dependency_file]
	compiled := os.exec([compiler, '-I${first}', '-I', second, '-I${absent}', '-o',
		os.join_path(build_dir, 'out'), os.join_path(build_dir, 'src.c'), '-MD', '-MF',
		os.join_path(build_dir, v3_program_dependency_file)])
	assert compiled.exit_code == 0, compiled.output
	if modulecache.file_metadata_signature(header) == '' {
		// This file system cannot tell a later edit apart: nothing is kept for it.
		return
	}
	later := time.utc().unix() + 10
	mut inputs := V3ProgramLinkInputs{
		taken: true
	}
	inputs.add_compiler_headers(&manager, compiler, args, build_dir, unit, later)
	assert inputs.unknown == ''
	assert header in inputs.files
	assert inputs.identities[inputs.files.index(header)] == modulecache.file_metadata_signature(header)
	// The source of the unit is in the directory of the build: it is no input.
	assert inputs.files.all(!it.starts_with(build_dir + '/'))
	// The header would be found in the first directory if it appeared there, and so
	// would the one that it asks about; the directory that is not there may appear.
	for candidate in [os.join_path(first, 'sub'), os.join_path(first, 'optional_answer.h'),
		os.join_path(second, 'optional_answer.h'), absent] {
		assert candidate in inputs.missing, candidate
	}
	// The compiler is asked once where it searches.
	assert os.ls(manager.dir)!.filter(it.starts_with('include_search_')).len == 1
	again := v3_include_search(&manager, compiler, args, build_dir) or {
		assert false, 'the answer of the compiler is not kept'
		return
	}
	assert first in again.dirs && second in again.dirs && absent in again.absent
	assert again.dirs.index(first) < again.dirs.index(second)
	// A header that was written when the compiler had started may not be the one
	// that it read.
	mut early := V3ProgramLinkInputs{
		taken: true
	}
	early.add_compiler_headers(&manager, compiler, args, build_dir, unit, time.utc().unix() - 10)
	assert early.unknown.contains('answer.h')
	// More than one source: the compiler writes down the headers of the last one.
	mut several := V3ProgramLinkInputs{
		taken: true
	}
	several.add_compiler_headers(&manager, compiler, ['-o', 'out', 'src.c', 'other.c'], build_dir,
		unit, later)
	assert several.unknown.contains('more than one source')
	// A command that only links read no header.
	mut linked := V3ProgramLinkInputs{
		taken: true
	}
	linked.add_compiler_headers(&manager, compiler, ['-o', 'out', '/cache/main.o'], build_dir,
		'', later)
	assert linked.unknown == '' && linked.files == []
}
