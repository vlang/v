module driver

import os
import time
import v.modulecache

fn link_inputs_fixture(name string) string {
	root := os.join_path(os.vtmp_dir(), 'v3_driver_${name}_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	return root
}

fn test_program_link_inputs_are_the_files_and_library_candidates_of_a_link() {
	root := link_inputs_fixture('program_link_inputs')
	defer {
		os.rmdir_all(root) or {}
	}
	first := os.join_path(root, 'first')
	second := os.join_path(root, 'second')
	linker := os.join_path(root, 'linker')
	system := os.join_path(root, 'system')
	for dir in [first, second, linker, system] {
		os.mkdir_all(dir)!
	}
	object := os.join_path(root, 'module.o')
	archive := os.join_path(second, 'libfoo.a')
	runtime := os.join_path(linker, 'libruntime.a')
	system_library := os.join_path(system, 'libbar.so')
	for file in [object, archive, runtime, os.join_path(linker, 'notes.txt')] {
		os.write_file(file, '!<arch>\nx')!
	}
	os.write_file(system_library, '\x7fELF\x00')!
	gone := os.join_path(root, 'gone.o')
	absent_dir := os.join_path(root, 'absent')
	inputs := v3_program_link_inputs(['-std=gnu11', '-o', 'out', 'src.c', object, gone, '-L${first}',
		'-L', second, '-lfoo', '-l', 'bar', '-I${root}', '-Wl,-rpath,${root}', '-L${absent_dir}'],
		linker, [system], '')
	assert inputs.taken && inputs.unknown == ''
	assert inputs.files == [runtime, object, archive, system_library].sorted()
	assert inputs.identities == inputs.files.map(modulecache.file_metadata_signature(it))
	// The link reads `libfoo.a` of the second directory because no library of that
	// name is in the first, or before it in the second; `libbar.so` of the system
	// because none is in a directory of `-L`.
	for candidate in [gone, os.join_path(first, 'libfoo.dylib'), os.join_path(first, 'libfoo.a'),
		os.join_path(second, 'libfoo.dylib'), os.join_path(first, 'libbar.a'),
		os.join_path(second, 'libbar.so'), os.join_path(system, 'libfoo.a'), absent_dir] {
		assert candidate in inputs.missing, candidate
	}
	assert archive !in inputs.missing
	// What is relative to the build directory is made anew by every build.
	assert inputs.files.all(os.is_abs_path(it)) && inputs.missing.all(os.is_abs_path(it))
}

fn test_program_link_inputs_include_the_files_of_linker_options() {
	root := link_inputs_fixture('program_link_options')
	defer {
		os.rmdir_all(root) or {}
	}
	names := ['direct.a', 'whole.a', 'forced.a', 'xlinker.a', 'loaded.a', 'symbols.txt', 'version.map',
		'exact.a']
	for name in names {
		os.write_file(os.join_path(root, name), '!<arch>\n${name}')!
	}
	path := fn [root] (name string) string {
		return os.join_path(root, name)
	}
	inputs := v3_program_link_inputs(['-o', 'out', 'src.c', '-Wl,${path('direct.a')}',
		'-Wl,--whole-archive,${path('whole.a')},--no-whole-archive',
		'-Wl,-force_load,${path('forced.a')}', '-Xlinker', path('xlinker.a'), '-force_load',
		path('loaded.a'), '-exported_symbols_list', path('symbols.txt'),
		'-Wl,--version-script=${path('version.map')}', '-L${root}', '-l:exact.a', '-Wl,-rpath,${root}',
		'-Wl,-dead_strip'], '', []string{}, '')
	assert inputs.unknown == ''
	assert inputs.files == names.map(path(it)).sorted()
	assert inputs.missing == []
}

fn test_program_link_inputs_are_unknown_for_what_cannot_be_followed() {
	root := link_inputs_fixture('program_link_unknown')
	defer {
		os.rmdir_all(root) or {}
	}
	thin := os.join_path(root, 'libthin.a')
	os.write_file(thin, '!<thin>\n//                                              10        `\nmember.o/\n')!
	assert v3_program_link_inputs(['-o', 'out', 'src.c', thin], '', []string{}, '').unknown.contains('thin archive')
	assert v3_program_link_inputs(['-o', 'out', 'src.c', '-L${root}', '-lthin'], '', []string{}, '').unknown.contains('thin archive')
	assert v3_program_link_inputs(['-o', 'out', 'src.c', '-Wl,${thin}'], '', []string{}, '').unknown.contains('thin archive')
	response := os.join_path(root, 'link.rsp')
	os.write_file(response, thin)!
	assert v3_program_link_inputs(['-o', 'out', 'src.c', '@${response}'], '', []string{}, '').unknown.len > 0
	assert v3_program_link_inputs(['-o', 'out', 'src.c', '-filelist', response], '', []string{}, '').unknown.len > 0
	assert v3_program_link_inputs(['-o', 'out', 'src.c', '-Wl,-filelist,${response}'], '',
		[]string{}, '').unknown.len > 0
	// A script that does more than name its inputs.
	script := os.join_path(root, 'libscript.so')
	os.write_file(script, 'SEARCH_DIR(/somewhere)\nGROUP ( libother.so.1 )\n')!
	assert v3_program_link_inputs(['-o', 'out', 'src.c', script], '', []string{}, '').unknown.contains('linker script')
}

fn test_program_link_inputs_follow_a_linker_script() {
	root := link_inputs_fixture('program_link_script')
	defer {
		os.rmdir_all(root) or {}
	}
	real_library := os.join_path(root, 'libreal.so.6')
	extra := os.join_path(root, 'libextra.a')
	needed := os.join_path(root, 'libneeded.so.1')
	os.write_file(real_library, '\x7fELF\x00')!
	os.write_file(extra, '!<arch>\nextra')!
	os.write_file(needed, '\x7fELF\x00')!
	script := os.join_path(root, 'libscripted.so')
	os.write_file(script, '/* GNU ld script */\nOUTPUT_FORMAT(elf64-x86-64)\nGROUP ( ${real_library} -lextra AS_NEEDED ( libneeded.so.1 ) )\n')!
	inputs := v3_program_link_inputs(['-o', 'out', 'src.c', '-L${root}', '-lscripted'], '',
		[]string{}, '')
	assert inputs.unknown == ''
	assert inputs.files == [script, real_library, extra, needed].sorted()
	// A thin archive behind a script is found too.
	os.write_file(extra, '!<thin>\n')!
	assert v3_program_link_inputs(['-o', 'out', 'src.c', '-L${root}', '-lscripted'], '', []string{}, '').unknown.contains('thin archive')
}

fn test_program_link_inputs_read_what_goes_to_the_linker_as_one_command() {
	root := link_inputs_fixture('program_link_forwarded')
	defer {
		os.rmdir_all(root) or {}
	}
	libs := os.join_path(root, 'libs')
	os.mkdir_all(libs)!
	archive := os.join_path(libs, 'libanswer.a')
	os.write_file(archive, '!<arch>\nanswer')!
	// An option and its value can each be handed over on its own.
	for args in [
		['-Wl,-L,${libs},-lanswer'],
		['-Wl,-L', '-Wl,${libs}', '-Wl,-l,answer'],
		['-Xlinker', '-L', '-Xlinker', libs, '-Xlinker', '-lanswer'],
		['-Wl,--library-path=${libs},--library=answer'],
		['-Wl,--library-path,${libs},--library,answer'],
		['-Wl,-L${libs}', '-Wl,-weak-lanswer'],
	] {
		mut command := ['-o', 'out', 'src.c']
		command << args
		inputs := v3_program_link_inputs(command, '', []string{}, '')
		assert inputs.unknown == '', args.str()
		assert inputs.files == [archive], args.str()
		assert os.join_path(libs, 'libanswer.dylib') in inputs.missing, args.str()
	}
	// The value of an option that names no input is none, though it is a file.
	named := v3_program_link_inputs(['-o', 'out', 'src.c', '-Wl,-soname,${archive}',
		'-Wl,-rpath,${libs}', '-Wl,-install_name,${archive}', '-Wl,-z,now', '-Wl,-znow',
		'-Wl,--build-id=sha1', '-Wl,-stack_size,0x4000000', '-Wl,-platform_version,macos,11.0,14.0',
		'-Wl,--as-needed', '-Wl,-dead_strip', '-Wl,-undefined,dynamic_lookup', '-Wl,--exclude-libs,ALL',
		'-Wl,-export_dynamic', '-Wl,-O1', '-Wl,-melf_x86_64'], '', []string{}, '')
	assert named.unknown == ''
	assert named.files == []
	// An option that is not known may read anything, and so may one that moves
	// the place where libraries are looked for.
	for args in [
		['-Wl,--no-such-option'],
		['-Wl,--sysroot=${root}'],
		['-Wl,-syslibroot,${root}'],
		['-Xlinker', '--made-up=1'],
		['-Wl,-exotic,${archive}'],
		['-Wl,@${archive}'],
	] {
		mut command := ['-o', 'out', 'src.c']
		command << args
		unknown := v3_program_link_inputs(command, '', []string{}, '')
		assert unknown.reason().len > 0, args.str()
		// What an option of the linker reads is known when the linker tells it; the
		// arguments of a file are those of the compiler as well.
		assert (unknown.unknown_option.len > 0) == !args[0].contains('@'), args.str()
	}
	// The options of the compiler itself are many, and none of them is the linker's.
	assert v3_program_link_inputs(['-O2', '-fwrapv', '--no-such-option', '-o', 'out', 'src.c'],
		'', []string{}, '').unknown == ''
	// A relative path is one of the directory of the build, unless it leaves it.
	assert v3_program_link_inputs(['-o', 'out', 'src.c', 'module.o', '-Lcache', '-Wl,cache/a.o'],
		'', []string{}, '').unknown == ''
	assert v3_program_link_inputs(['-o', 'out', 'src.c', '-L../libs', '-lanswer'], '', []string{}, '').unknown.len > 0
	assert v3_program_link_inputs(['-o', 'out', 'src.c', '../libs/libanswer.a'], '', []string{}, '').unknown.len > 0
	assert v3_program_link_inputs(['-o', 'out', 'src.c', '-Wl,../libs/libanswer.a'], '',
		[]string{}, '').unknown.len > 0
}

fn test_program_link_inputs_give_an_option_the_value_that_follows_it_in_the_command() {
	root := link_inputs_fixture('program_link_values')
	defer {
		os.rmdir_all(root) or {}
	}
	libs := os.join_path(root, 'libs')
	os.mkdir_all(libs)!
	archive := os.join_path(libs, 'libanswer.a')
	os.write_file(archive, '!<arch>\nanswer')!
	symbols := os.join_path(root, 'symbols.map')
	os.write_file(symbols, '{ global: answer; };\n')!
	// The value of an option of the linker can be an argument of the compiler
	// driver, which hands it over in its place; an option whose value is attached
	// to it takes none.
	for args in [
		['-Wl,-rpath', '/opt/lib', '-Wl,-L,${libs},-lanswer'],
		['-Wl,-rpath', libs, '-Wl,-L,${libs},-lanswer'],
		['-Wl,--sort-common', '-Wl,-L${libs}', '-lanswer'],
		['-Wl,--icf=all', '-Wl,--sort-common=descending', '-L', libs, '-lanswer'],
		['-Wl,-soname', 'libout.so', '-L${libs}', '-Wl,-lanswer'],
		['-Wl,-R${libs}', '-Wl,-R', '-Wl,${libs}', '-L${libs}', '-lanswer'],
	] {
		mut command := ['-o', 'out', 'src.c']
		command << args
		inputs := v3_program_link_inputs(command, '', []string{}, '')
		assert inputs.unknown == '', args.str()
		assert inputs.files == [archive], args.str()
	}
	split := v3_program_link_inputs(['-o', 'out', 'src.c', '-Wl,--version-script', symbols,
		'-L${libs}', '-lanswer'], '', []string{}, '')
	assert split.unknown == ''
	assert split.files == [archive, symbols].sorted()
	// A macro that the compiler is given a value for names no file, and neither
	// does a value that is no absolute path.
	defined := v3_program_link_inputs(['-o', 'out', 'src.c', '-DASSETS=../assets', '-DROOT=${archive}',
		'-std=gnu11', '--sysroot=${root}', '-fprofile-use=${symbols}', '-march=native'], '',
		[]string{}, '')
	assert defined.unknown == ''
	assert defined.files == [symbols]
}

fn test_program_link_inputs_find_what_a_relative_path_names_outside_the_build() {
	root := link_inputs_fixture('program_link_relative')
	defer {
		os.rmdir_all(root) or {}
	}
	// The command runs in a directory that the build made beside the output.
	build_dir := os.join_path(root, 'out.v3cc')
	libs := os.join_path(root, 'libs')
	os.mkdir_all(build_dir)!
	os.mkdir_all(libs)!
	archive := os.join_path(libs, 'libanswer.a')
	profile := os.join_path(root, 'default.profdata')
	os.write_file(archive, '!<arch>\nanswer')!
	os.write_file(profile, 'profile')!
	inputs := v3_program_link_inputs(['-o', 'out', 'src.c', 'cache/module.o', '-L../libs', '-lanswer',
		'-fprofile-use=../default.profdata', '-fdebug-prefix-map=../src=.', '-iframework',
		'../Frameworks', '-Wl,-rpath,../libs', '-I../include'], '', []string{}, build_dir)
	assert inputs.unknown == ''
	assert inputs.files == [profile, archive].sorted()
	// A file that is not there now may be there for the next build.
	assert os.join_path(libs, 'libanswer.dylib') in inputs.missing
	direct := v3_program_link_inputs(['-o', 'out', 'src.c', '../libs/libanswer.a',
		'-Wl,../libs/libanswer.a'],
		'', []string{}, build_dir)
	assert direct.unknown == ''
	assert direct.files == [archive]
	// Where the directory of the build is not known, such a path names nothing
	// that can be followed.
	assert v3_program_link_inputs(['-o', 'out', 'src.c', '-fprofile-use=../default.profdata'],
		'', []string{}, '').unknown.len > 0
}

fn test_program_link_inputs_tell_what_only_the_place_of_the_output_names() {
	root := link_inputs_fixture('program_link_output_place')
	defer {
		os.rmdir_all(root) or {}
	}
	build_dir := os.join_path(root, 'out.v3cc')
	frameworks := os.join_path(root, 'Frameworks', 'Kit.framework')
	os.mkdir_all(build_dir)!
	os.mkdir_all(frameworks)!
	os.write_file(os.join_path(frameworks, 'Kit'), '!<arch>\nkit')!
	// A relative path that leaves the directory of the build names a file by
	// where the output goes: the executable must be kept for that place.
	relative := v3_program_link_inputs(['-o', 'out', 'src.c', '-F../Frameworks', '-framework',
		'Kit'],
		'', []string{}, build_dir)
	assert relative.unknown == ''
	assert relative.files == [os.join_path(frameworks, 'Kit')]
	assert os.join_path(frameworks, 'Kit.tbd') in relative.missing
	assert relative.relative_paths
	absolute := v3_program_link_inputs(['-o', 'out', 'src.c', '-F${os.dir(frameworks)}', '-framework',
		'Kit'], '', []string{}, build_dir)
	assert absolute.files == relative.files
	assert !absolute.relative_paths
	assert v3_flags_name_relative_paths(['-O2', '-L../libs'])
	assert v3_flags_name_relative_paths(['-fprofile-use=../../p.profdata', '-lm'])
	assert v3_flags_name_relative_paths(['-I', '..'])
	// A `..` that ends a part of a path, wherever it is in a list.
	assert v3_flags_name_relative_paths(['..:/usr/include'])
	assert v3_flags_name_relative_paths(['/usr/include:..:/opt/include'])
	assert v3_flags_name_relative_paths(['-Wp,-I..,-DX'])
	assert v3_flags_name_relative_paths(['-I..'])
	assert !v3_flags_name_relative_paths(['-L/abs/libs', '-lm', '-Wl,-rpath,/x', '-DV=1..2x',
		'-DE=a...b', '-DR=0..9'])
	// A specs file tells the driver what to link with: nothing is kept of that.
	for args in [['-specs', '/abs/my.specs'], ['-specs=/abs/my.specs'], ['--specs=/abs/my.specs']] {
		mut command := ['-o', 'out', 'src.c']
		command << args
		assert v3_program_link_inputs(command, '', []string{}, build_dir).unknown.contains('specs'), args.str()
	}
	// What an option that is not known is given may be a file that it reads.
	list := os.join_path(root, 'symbols.list')
	os.write_file(list, '_answer\n')!
	given := v3_program_link_inputs(['-o', 'out', 'src.c', '-Wl,--made-up-list=${list}'], '',
		[]string{}, build_dir)
	assert given.unknown == '' && given.unknown_option.len > 0
	assert given.files == [list]
}

fn test_an_argument_of_the_driver_is_told_from_one_that_it_hands_to_the_linker() {
	assert v3_compiler_option_values('-iframework') == 1
	assert v3_compiler_option_values('-sectcreate') == 3
	assert v3_compiler_option_values('-L') == 1
	assert v3_compiler_option_values('-O2') == 0
	kind, value := v3_attached_linker_option('-L/dir')
	assert kind == .library_dir && value == '/dir'
	for arg, expected in {
		'-lanswer':      V3AttachedOption.library
		'-weak-lanswer': V3AttachedOption.library
		'-F/frameworks': V3AttachedOption.framework_dir
		'-R/run':        V3AttachedOption.runtime_path
		'-T/script.ld':  V3AttachedOption.script
		'-Ttext':        V3AttachedOption.no_option
		'-znow':         V3AttachedOption.value
		'--library=x':   V3AttachedOption.no_option
		'-exported':     V3AttachedOption.no_option
		'/abs/libfoo.a': V3AttachedOption.no_option
	} {
		found, _ := v3_attached_linker_option(arg)
		assert found == expected, arg
	}
}

fn test_program_link_inputs_follow_a_linker_script_of_any_name() {
	root := link_inputs_fixture('program_link_script_name')
	defer {
		os.rmdir_all(root) or {}
	}
	archive := os.join_path(root, 'libanswer.a')
	os.write_file(archive, '!<arch>\nanswer')!
	script := os.join_path(root, 'answer.ld')
	os.write_file(script, 'INPUT ( ${archive} )\n')!
	for args in [
		[script],
		['-Wl,${script}'],
		['-Xlinker', script],
		['-T', script],
		['-T${script}'],
		['-Wl,-T,${script}'],
		['-Wl,-T${script}'],
		['-Wl,--script=${script}'],
	] {
		mut command := ['-o', 'out', 'src.c']
		command << args
		inputs := v3_program_link_inputs(command, '', []string{}, '')
		assert inputs.unknown == '', args.str()
		assert inputs.files == [archive, script].sorted(), args.str()
	}
	// A script that lays the program out does more than name inputs.
	layout := os.join_path(root, 'layout.x')
	os.write_file(layout, 'SECTIONS { . = 0x10000; }\n')!
	assert v3_program_link_inputs(['-o', 'out', 'src.c', layout], '', []string{}, '').unknown.contains('linker script')
	assert v3_program_link_inputs(['-o', 'out', 'src.c', '-Wl,-T,${layout}'], '', []string{}, '').unknown.contains('linker script')
	// A list of symbols is read as it is, and a source is compiled.
	symbols := os.join_path(root, 'symbols.txt')
	source := os.join_path(root, 'extra.c')
	os.write_file(symbols, '_answer\n')!
	os.write_file(source, 'int extra(void) { return 1; }\n')!
	plain := v3_program_link_inputs(['-o', 'out', 'src.c', source, '-Wl,--version-script=${symbols}',
		'-exported_symbols_list', symbols, '-Wl,-exported_symbols_list,${symbols}'], '',
		[]string{}, '')
	assert plain.unknown == ''
	assert plain.files == [source, symbols].sorted()
	// A text stub of a library names what the library exports, not other files.
	stub := os.join_path(root, 'libstub.tbd')
	os.write_file(stub, '--- !tapi-tbd\ntbd-version: 4\n')!
	stubbed := v3_program_link_inputs(['-o', 'out', 'src.c', stub], '', []string{}, '')
	assert stubbed.unknown == '' && stubbed.files == [stub]
}

fn test_environment_of_a_link_is_part_of_what_identifies_its_executable() {
	saved := os.getenv_opt('LIBRARY_PATH')
	defer {
		if value := saved {
			os.setenv('LIBRARY_PATH', value, true)
		} else {
			os.unsetenv('LIBRARY_PATH')
		}
	}
	signature := fn () string {
		return v3_program_executable_link_signature(['-lanswer'], false, []string{})
	}
	os.setenv('LIBRARY_PATH', '/first', true)
	first := signature()
	assert first == signature()
	os.setenv('LIBRARY_PATH', '/second', true)
	assert signature() != first
	os.setenv('LIBRARY_PATH', '/first${os.path_delimiter}/second', true)
	ordered := signature()
	os.setenv('LIBRARY_PATH', '/second${os.path_delimiter}/first', true)
	assert signature() != ordered
	os.unsetenv('LIBRARY_PATH')
	assert signature() != first
	// What a compiler or a linker searches or writes into its output by.
	for name in ['LIBRARY_PATH', 'CPATH', 'C_INCLUDE_PATH', 'LD_RUN_PATH', 'SDKROOT',
		'MACOSX_DEPLOYMENT_TARGET'] {
		assert name in v3_link_environment_names
	}
}

fn test_link_search_args_are_those_that_move_the_libraries_of_a_compiler() {
	assert v3_link_search_args(['-O2', '-o', 'out', 'src.c', '--sysroot=/sdk', '-isysroot', '/sdk2',
		'-m32', '-B/tools', '-lfoo', '-static', '-target', 'x86_64-linux-gnu', '-Wl,-s', '-I/inc']) == [
		'--sysroot=/sdk',
		'-isysroot',
		'/sdk2',
		'-m32',
		'-B/tools',
		'-static',
		'-target',
		'x86_64-linux-gnu',
	]
}

fn test_linker_report_is_read_in_both_of_its_forms() {
	root := link_inputs_fixture('linker_report_forms')
	defer {
		os.rmdir_all(root) or {}
	}
	info := os.join_path(root, 'dependency_info')
	mut bytes := []u8{}
	for kind, name in {
		u8(0x00): '@(#)PROGRAM:ld PROJECT:ld-1267'
		u8(0x10): '/sdk/usr/lib/libSystem.tbd'
		u8(0x11): '/first/libanswer.a'
		u8(0x40): 'out'
	} {
		bytes << kind
		bytes << name.bytes()
		bytes << 0
	}
	bytes << u8(0x10)
	bytes << '/libs/libanswer.a'.bytes()
	bytes << 0
	bytes << u8(0x10)
	bytes << '/sdk/usr/lib/libSystem.tbd'.bytes()
	bytes << 0
	os.write_bytes(info, bytes)!
	found, not_found := v3_read_linker_report(.dependency_info, info) or {
		assert false, 'the report of Apple ld is not read'
		return
	}
	assert found == ['/sdk/usr/lib/libSystem.tbd', '/libs/libanswer.a']
	assert not_found == ['/first/libanswer.a']
	// A report that is cut off, or that is not there, is none.
	os.write_bytes(info, bytes[..bytes.len - 3])!
	assert v3_read_linker_report(.dependency_info, info) == none
	assert v3_read_linker_report(.dependency_info, info + '.absent') == none
	assert v3_read_linker_report(.no_report, info) == none
	rule := os.join_path(root, 'dependency_file')
	os.write_file(rule, 'out: /usr/lib/crt1.o /tmp/cc1.o \\\n /libs/libanswer.a /usr/lib/libc.so\n\n/usr/lib/crt1.o:\n\n/tmp/cc1.o:\n')!
	read, none_missing := v3_read_linker_report(.dependency_file, rule) or {
		assert false, 'the report of GNU ld is not read'
		return
	}
	assert read == ['/usr/lib/crt1.o', '/tmp/cc1.o', '/libs/libanswer.a', '/usr/lib/libc.so']
	assert none_missing == []
	// The report is named relative to the directory that the linker runs in: a
	// compiler driver takes an argument for the linker apart at each comma, and
	// the path of a directory can hold one.
	assert v3_linker_report_args(.dependency_info) == [
		'-Wl,-dependency_info,${v3_linker_report_file}',
	]
	assert v3_linker_report_args(.dependency_file) == [
		'-Wl,--dependency-file=${v3_linker_report_file}',
	]
	assert v3_linker_report_args(.no_report) == []
	// A command that asks its compiler or its linker for a file besides the
	// executable gets it from a build that compiles and links: no executable is
	// kept of it. The driver reads what a command names before it adds anything.
	for args in [
		'-Wl,-dependency_info,/tmp/user.bin',
		'-Xlinker -dependency_info -Xlinker /tmp/user.bin',
		'-Wl,--dependency-file=/tmp/user.bin',
		'-Wl,--dependency-file,/tmp/user.bin',
		'-Wl,-map,/tmp/out.map',
		'-Wl,-Map=/tmp/out.map',
		'-MD -MF /tmp/deps.d',
		'-MMD',
		'--coverage',
		'-save-temps=obj',
		'-ftime-trace',
		'-fdump-tree-all',
		'-Wp,-MD,/tmp/deps.d',
		'-Wp,-DX,-MMD,/tmp/deps.d',
		'-index-store-path /tmp/index',
		'-fsave-optimization-record',
		'-foptimization-record-file=/tmp/out.opt.yaml',
		'-Wl,--reproduce=/tmp/link.tar',
		'-Wl,--pdb=/tmp/out.pdb',
	] {
		mut command := ['-o', 'out', 'src.c']
		command << args.split(' ')
		assert v3_program_link_inputs(command, '', []string{}, '/build').unknown.contains('writes a file'), args
	}
	for args in ['-Wl,--as-needed -L/tmp -lm', '-O2 -MT x -o dependency_info', '-Wp,-DMD=1,-I/tmp'] {
		mut command := ['-o', 'out', 'src.c']
		command << args.split(' ')
		assert v3_program_link_inputs(command, '', []string{}, '/build').unknown == '', args
	}
	// A linker that does not know the option says which one.
	assert v3_output_names_unknown_option(.dependency_file, "/usr/bin/ld: unrecognized option '--dependency-file=x'\ncollect2: error: ld returned 1")
	assert v3_output_names_unknown_option(.dependency_info, 'ld: unknown options: -dependency_info')
	assert !v3_output_names_unknown_option(.dependency_file, 'ld: cannot open dependency file x: No space left on device')
	assert !v3_output_names_unknown_option(.dependency_file, 'ld: unknown options: -dependency_info')
	assert !v3_output_names_unknown_option(.no_report, 'ld: unknown options: -dependency_info')
	// The link of a build runs in the language of the user: a linker that fails
	// over the option names it in any of them.
	assert v3_output_names_linker_report(.dependency_file, "/usr/bin/ld: nicht erkannte Option '--dependency-file=x'")
	assert v3_output_names_linker_report(.dependency_info, 'ld: unknown options: -dependency_info')
	assert !v3_output_names_linker_report(.dependency_file, 'ld: undefined reference to `answer`')
	assert !v3_output_names_linker_report(.no_report, 'ld: unknown options: -dependency_info')
	// What takes the libraries of a program away is no part of the question.
	assert v3_linker_probe_args(['-fuse-ld=lld', '-static', '-nostdlib', '-m64']) == [
		'-fuse-ld=lld',
		'-m64',
	]
}

fn test_linker_report_adds_what_the_linker_found_by_itself() {
	root := link_inputs_fixture('linker_report_inputs')
	defer {
		os.rmdir_all(root) or {}
	}
	build_dir := os.join_path(root, 'build')
	os.mkdir_all(build_dir)!
	named := os.join_path(root, 'libnamed.a')
	implicit := os.join_path(root, 'libimplicit.tbd')
	made := os.join_path(build_dir, 'src.o')
	gone := os.join_path(root, 'temporary.o')
	absent := os.join_path(root, 'first', 'libnamed.a')
	for file in [named, implicit, made] {
		os.write_file(file, '!<arch>\nx')!
	}
	if modulecache.file_metadata_signature(named) == '' {
		// This file system cannot tell a later edit apart: nothing is kept for it.
		return
	}
	write_report := fn [build_dir] (found []string, not_found []string) {
		mut bytes := []u8{}
		for name in found {
			bytes << u8(0x10)
			bytes << name.bytes()
			bytes << 0
		}
		for name in not_found {
			bytes << u8(0x11)
			bytes << name.bytes()
			bytes << 0
		}
		os.write_bytes(os.join_path(build_dir, v3_linker_report_file), bytes) or { panic(err) }
	}
	write_report([named, implicit, made, gone, 'relative.o'], [absent])
	later := time.utc().unix() + 10
	new_inputs := fn [named] () V3ProgramLinkInputs {
		return V3ProgramLinkInputs{
			taken:          true
			files:          [named]
			identities:     ['identity from before the link']
			unknown_option: 'the linker option `-no_order_inits` is not one whose inputs are known'
		}
	}
	report_file := os.join_path(build_dir, v3_linker_report_file)
	mut inputs := new_inputs()
	inputs.add_linker_report(.dependency_info, report_file, build_dir, later, []string{})
	assert inputs.unknown == ''
	// The linker told where it found nothing: the option that is not known here
	// read nothing that the report does not hold.
	assert inputs.unknown_option == ''
	assert inputs.reason() == ''
	// A file that the command names keeps what it was before the link; one that
	// the linker found by itself is the file that it is now; what the command made
	// or removed is no input.
	assert inputs.files == [named, implicit]
	assert inputs.identities == ['identity from before the link',
		modulecache.file_metadata_signature(implicit)]
	assert inputs.missing == [absent]
	// A rule of `make` tells no path where nothing was: what the option reads
	// besides is not known.
	os.write_file(os.join_path(build_dir, v3_linker_report_file), 'out: ${named} ${implicit}\n')!
	mut ruled := new_inputs()
	ruled.add_linker_report(.dependency_file, report_file, build_dir, later, []string{})
	assert ruled.unknown == ''
	assert ruled.files == [named, implicit]
	assert ruled.unknown_option.len > 0
	// A file that the linker found and that is as new as the link may not be the
	// one that it read.
	write_report([named, implicit], []string{})
	mut early := new_inputs()
	early.add_linker_report(.dependency_info, report_file, build_dir, time.utc().unix() - 10,
		[]string{})
	assert early.unknown.contains('libimplicit.tbd')
	// The linker names what it found through a relative directory relative to the
	// directory that it runs in: such a file is one by where the output goes.
	write_report([named, '../libimplicit.tbd', 'src.o'], ['../first/libnamed.a', 'first/libnamed.a'])
	mut relative := new_inputs()
	relative.add_linker_report(.dependency_info, report_file, build_dir, later, []string{})
	assert relative.unknown == ''
	assert relative.files == [named, implicit]
	assert relative.missing == [absent]
	assert relative.relative_paths
	assert !inputs.relative_paths
	// What the linker takes from deep inside an SDK, and where it finds nothing in
	// one, comes and goes with the files that it names there.
	sdk := os.join_path(root, 'sdk')
	member := os.join_path(sdk, 'usr', 'lib', 'system', 'libsystem_c.tbd')
	system := os.join_path(sdk, 'usr', 'lib', 'libSystem.tbd')
	os.mkdir_all(os.dir(member))!
	os.write_file(member, '--- !tapi-tbd')!
	os.write_file(system, '--- !tapi-tbd')!
	write_report([named, system, member], [absent, os.join_path(sdk, 'usr', 'lib', 'libcache.dylib')])
	mut with_sdk := new_inputs()
	with_sdk.add_linker_report(.dependency_info, report_file, build_dir, later, [sdk])
	assert with_sdk.unknown == ''
	assert with_sdk.files == [named, system]
	assert with_sdk.missing == [absent]
	// A linker that was asked and wrote nothing leaves no executable behind.
	os.rm(report_file)!
	mut silent := new_inputs()
	silent.add_linker_report(.dependency_info, report_file, build_dir, later, []string{})
	assert silent.unknown.contains('did not tell')
	mut unasked := new_inputs()
	unasked.add_linker_report(.no_report, report_file, build_dir, later, []string{})
	assert unasked.unknown == ''
	assert unasked.unknown_option.len > 0
}

fn test_linker_is_asked_once_which_report_it_gives() {
	$if windows {
		return
	}
	root := link_inputs_fixture('linker_report_probe')
	defer {
		os.rmdir_all(root) or {}
	}
	build_dir := os.join_path(root, 'build')
	os.mkdir_all(build_dir)!
	manager := modulecache.new_manager(os.join_path(root, 'cache'), 'salt', true, '', '')
	answers := fn [manager] () []string {
		mut kept := []string{}
		for name in os.ls(manager.dir) or { []string{} } {
			if name.starts_with('linker_report_') {
				kept << os.read_file(os.join_path(manager.dir, name)) or { '' }
			}
		}
		kept.sort()
		return kept
	}
	// A linker that writes the report: a compiler that writes it in its place, and
	// that leaves a line for each time that it runs.
	writing := os.join_path(root, 'writing-cc')
	runs := os.join_path(root, 'runs')
	os.write_file(writing, '#!/bin/sh\necho run >> ${runs}\necho report > ${v3_linker_report_file}\nexit 0\n')!
	os.chmod(writing, 0o755)!
	expected := if os.user_os() == 'macos' {
		V3LinkerReport.dependency_info
	} else {
		V3LinkerReport.dependency_file
	}
	assert v3_linker_report(&manager, writing, []string{}, os.user_os(), build_dir) == expected
	assert answers() == ['yes\n']
	// Nothing of the question is left in the directory of the build.
	assert os.ls(build_dir)! == []
	// The answer is read back: the compiler is not run again.
	assert v3_linker_report(&manager, writing, []string{}, os.user_os(), build_dir) == expected
	assert os.read_lines(runs)!.len == 1
	// A linker that does not know the option says so, and is not asked again.
	refusing := os.join_path(root, 'refusing-cc')
	os.write_file(refusing, '#!/bin/sh\necho "ld: unknown options: -dependency_info" >&2\necho "ld: unrecognized option --dependency-file" >&2\nexit 1\n')!
	os.chmod(refusing, 0o755)!
	assert v3_linker_report(&manager, refusing, []string{}, os.user_os(), build_dir) == .no_report
	assert answers() == ['no\n', 'yes\n']
	// A compiler that fails for another reason is asked the next time.
	failing := os.join_path(root, 'failing-cc')
	os.write_file(failing, '#!/bin/sh\necho "ld: cannot open dependency file: No space left on device" >&2\nexit 1\n')!
	os.chmod(failing, 0o755)!
	assert v3_linker_report(&manager, failing, []string{}, os.user_os(), build_dir) == .no_report
	assert answers() == ['no\n', 'yes\n']
	// A link that fails over the option takes a kept answer back.
	v3_forget_linker_report(&manager, writing, []string{}, os.user_os())
	assert answers() == ['no\n', 'no\n']
	assert v3_linker_report(&manager, writing, []string{}, os.user_os(), build_dir) == .no_report
	assert os.read_lines(runs)!.len == 1
	// The linker of the system gives the report that it is asked for, or none.
	if compiler := os.find_abs_path_of_executable('cc') {
		real := v3_linker_report(&manager, compiler, []string{}, os.user_os(), build_dir)
		assert real in [expected, .no_report]
		$if macos {
			assert real == .dependency_info
		}
		assert os.ls(build_dir)! == []
	}
}

fn test_library_search_dirs_are_read_from_what_a_compiler_prints() {
	gcc := 'install: /usr/lib/gcc/x86_64-linux-gnu/13/\nprograms: =/usr/bin\nlibraries: =/usr/lib/gcc/x86_64-linux-gnu/13/:/usr/lib/x86_64-linux-gnu/:/lib/\n'
	assert v3_parse_library_search_dirs(gcc) == ['/usr/lib/gcc/x86_64-linux-gnu/13/',
		'/usr/lib/x86_64-linux-gnu/', '/lib/']
	tcc := 'install: /tcc/lib\ninclude:\n  /tcc/lib/include\n  /usr/include\nlibraries:\n  thirdparty/tcc/lib\n  /usr/local/lib\n  /usr/lib\nlibtcc1:\n  /tcc/lib/libtcc1.a\n'
	assert v3_parse_library_search_dirs(tcc) == ['thirdparty/tcc/lib', '/usr/local/lib', '/usr/lib']
	assert v3_parse_library_search_dirs('') == []
}

fn test_files_keep_identities_only_while_they_are_the_files_that_were_read() {
	root := os.join_path(os.vtmp_dir(), 'v3_driver_file_identities_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'fn main() {}\n')!
	identities := [modulecache.file_metadata_signature(source)]
	if identities[0] == '' {
		// This file system cannot tell a later edit apart: nothing is kept for it.
		assert !v3_files_keep_identities([source], identities)
		return
	}
	assert v3_files_keep_identities([source], identities)
	assert !v3_files_keep_identities([source, source], identities)
	assert !v3_files_keep_identities([source], [''])
	os.write_file(source, 'fn main() { println(1) }\n')!
	assert !v3_files_keep_identities([source], identities)
}

fn test_program_executable_link_signature_tells_builds_apart() {
	base := v3_program_executable_link_signature(['-lm'], false, ['strict=false'])
	assert base == v3_program_executable_link_signature(['-lm'], false, ['strict=false'])
	assert base != v3_program_executable_link_signature(['-lm', '-lz'], false, ['strict=false'])
	assert base != v3_program_executable_link_signature(['-lm'], true, ['strict=false'])
	assert base != v3_program_executable_link_signature(['-lm'], false, ['strict=true'])
}

fn test_a_command_that_names_the_time_of_the_compilation_is_asked_about_it() {
	named := v3_program_link_inputs(['-o', 'out', 'src.c', '-DBUILT=__TIME__'], '', []string{},
		'/build')
	assert named.unknown == '' && named.reason().contains('names a macro for the time')
	part := v3_program_link_inputs(['-o', 'out', 'src.c', '-DHEAD=__TI', '-DTEXT="ME__"'],
		'', []string{}, '/build')
	assert part.reason() == '' && part.time_parts == ['__TI']
	plain := v3_program_link_inputs(['-o', 'out', 'src.c', '-DTIMEOUT=3'], '', []string{},
		'/build')
	assert plain.reason() == '' && plain.time_parts == []
	// An argument is a text of its own: what opens a comment in one hides nothing
	// of the next.
	apart := v3_program_link_inputs(['-o', 'out', 'src.c', '-DNOTE=/*x', '-DHEAD=__TI', '-DTAIL=ME__'],
		'', []string{}, '/build')
	assert apart.time_parts == ['ME__', '__TI']
}

fn test_a_variable_that_makes_the_compiler_write_a_file_leaves_no_executable() {
	saved := os.getenv_opt('DEPENDENCIES_OUTPUT')
	defer {
		if value := saved {
			os.setenv('DEPENDENCIES_OUTPUT', value, true)
		} else {
			os.unsetenv('DEPENDENCIES_OUTPUT')
		}
	}
	os.unsetenv('DEPENDENCIES_OUTPUT')
	assert v3_program_link_inputs(['-o', 'out', 'src.c'], '', []string{}, '/build').unknown == ''
	os.setenv('DEPENDENCIES_OUTPUT', '/tmp/deps.d', true)
	assert v3_program_link_inputs(['-o', 'out', 'src.c'], '', []string{}, '/build').unknown.contains('`DEPENDENCIES_OUTPUT` makes the compiler write a file')
}

fn test_library_search_of_a_compiler_is_asked_again_in_another_environment() {
	$if windows {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'v3_library_search_env_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	manager := modulecache.new_manager(os.join_path(root, 'cache'), 'salt', true, '', '')
	// A compiler that searches where a variable of its environment sends it, as
	// GCC does with `GCC_EXEC_PREFIX`, and that leaves a line for each run.
	compiler := os.join_path(root, 'searching-cc')
	runs := os.join_path(root, 'runs')
	os.write_file(compiler, '#!/bin/sh\necho run >> ${runs}\necho "libraries: =\${GCC_EXEC_PREFIX}lib"\n')!
	os.chmod(compiler, 0o755)!
	saved := os.getenv_opt('GCC_EXEC_PREFIX')
	defer {
		if value := saved {
			os.setenv('GCC_EXEC_PREFIX', value, true)
		} else {
			os.unsetenv('GCC_EXEC_PREFIX')
		}
	}
	os.setenv('GCC_EXEC_PREFIX', '/first/', true)
	assert '/first/lib' in v3_default_link_library_dirs(&manager, compiler, []string{}, '', '')
	assert '/first/lib' in v3_default_link_library_dirs(&manager, compiler, []string{}, '', '')
	assert os.read_lines(runs)!.len == 1
	os.setenv('GCC_EXEC_PREFIX', '/second/', true)
	second := v3_default_link_library_dirs(&manager, compiler, []string{}, '', '')
	assert '/second/lib' in second && '/first/lib' !in second
	assert os.read_lines(runs)!.len == 2
}
