module driver

import os
import v.cmdexec
import v.modulecache

// The executable of a program is kept for the next build of the same inputs (see
// modulecache.Manager.valid_program_executable). What the build generates is part
// of the identity of that executable; what the link reads besides is found here,
// from the command that links: the files it names, the libraries that it looks up,
// and what those refer to. A command with an input that cannot be told leaves no
// executable behind for a later build.

// V3ProgramLinkInputs is what the link of a program reads besides the C of the
// build. `identities[i]` is the metadata of `files[i]` from before the link.
struct V3ProgramLinkInputs {
mut:
	taken      bool
	files      []string
	identities []string
	// The paths where a file that is not there would be read instead of, or along
	// with, one of the files.
	missing []string
	// Why the inputs of the command cannot all be told, or '' when they can.
	unknown string
	// An option of the linker that is not known, or '': what it makes the linker
	// read is not known from the command, and is when the linker tells it itself.
	unknown_option string
	// Whether an input is named by a path that is relative to the directory of the
	// build and leaves it: what it names depends on where the output goes.
	relative_paths bool
	// The parts of the name of a macro for the time of the compilation that the
	// command or generated unit has: see v3_c_time_name_parts.
	time_parts []string
	// Why a possible time dependency prevents executable reuse, or ''.
	time_question string
}

// reason returns why no executable can be kept of a link with these inputs, or ''.
fn (inputs &V3ProgramLinkInputs) reason() string {
	if inputs.unknown.len > 0 {
		return inputs.unknown
	}
	if inputs.time_question.len > 0 {
		return inputs.time_question
	}
	return inputs.unknown_option
}

// inputs_can_be_told reports whether nothing of the command stands in the way of
// telling what it reads: what the compiler read is worth finding out then.
fn (inputs &V3ProgramLinkInputs) inputs_can_be_told() bool {
	return inputs.unknown.len == 0 && inputs.unknown_option.len == 0
}

// V3LinkCommand is the part of a link command that names files and libraries.
struct V3LinkCommand {
mut:
	// The files that the linker reads as inputs: objects, archives, libraries, and
	// linker scripts, which name more of them.
	files []string
	// The files that the command reads as they are: sources, lists of symbols.
	plain_files    []string
	library_dirs   []string
	libraries      []string
	framework_dirs []string
	frameworks     []string
	// The directory in which the command runs, or '' when it is not known.
	build_dir      string
	unknown        string
	unknown_option string
	// Whether a relative path that leaves the directory of the build was followed.
	relative_paths bool
}

// v3_compiler_options_with_value are the options of a C compiler driver whose next
// argument is a value that names no input of the link.
const v3_compiler_options_with_value = ['-o', '-x', '-arch', '-target', '-u', '-e', '-install_name',
	'-current_version', '-compatibility_version', '-MF', '-MT', '-MQ', '-D', '-U', '-Xclang',
	'-Xassembler', '-Xpreprocessor', '-mllvm', '-z', '-stack_size', '-rpath', '-I', '-isystem',
	'-iquote', '-idirafter', '-isysroot', '-syslibroot', '-B', '-undefined', '-pagezero_size',
	'-image_base', '-init', '-exported_symbol', '-unexported_symbol', '--param', '-G',
	'-allowable_client', '-client_name', '-umbrella', '-sub_library', '-sub_umbrella',
	'-multiply_defined', '-iprefix', '-iwithprefix', '-iwithprefixbefore', '-imultilib', '-dumpbase',
	'-dumpdir', '-aux-info', '-A', '-iframework', '--sysroot', '-include', '-imacros']

// v3_compiled_source_suffixes are the endings by which a C compiler driver knows a
// file that it compiles. A file of any other name goes to the linker.
const v3_compiled_source_suffixes = ['.c', '.m', '.mm', '.M', '.cc', '.cp', '.cxx', '.cpp', '.c++',
	'.C', '.i', '.ii', '.mi', '.mii', '.s', '.S', '.sx', '.h', '.hh', '.hpp', '.H', '.ll', '.bc']

// The options of a linker, by their names without the dashes that start them: GNU
// ld takes one of its long options with one dash or with two.

// v3_linker_flag_options take no value, or one that is attached to them with `=`,
// and name no input.
const v3_linker_flag_options = ['sort-common', 'icf', 's', 'S', 'x', 'X', 'E', 'i', 'r', 'g', 'n',
	'N', 'q', 't', 'w', 'v', 'V', 'M', 'd', 'dc', 'dp', 'O0', 'O1', 'O2', 'O3', '(', ')', 'strip-all',
	'strip-debug', 'discard-all', 'discard-locals', 'export-dynamic', 'no-export-dynamic', 'gc-sections',
	'no-gc-sections', 'as-needed', 'no-as-needed', 'whole-archive', 'no-whole-archive', 'start-group',
	'end-group', 'push-state', 'pop-state', 'Bstatic', 'Bdynamic', 'Bsymbolic', 'Bsymbolic-functions',
	'Bshareable', 'static', 'dn', 'dy', 'non_shared', 'call_shared', 'shared', 'pie', 'pic-executable',
	'no-pie', 'nostdlib', 'no-undefined', 'allow-multiple-definition', 'allow-shlib-undefined',
	'no-allow-shlib-undefined', 'build-id', 'eh-frame-hdr', 'no-eh-frame-hdr', 'fatal-warnings',
	'no-fatal-warnings', 'relax', 'no-relax', 'warn-common', 'warn-once', 'verbose', 'stats', 'trace',
	'print-map', 'print-gc-sections', 'no-print-gc-sections', 'emit-relocs', 'relocatable', 'omagic',
	'nmagic', 'no-omagic', 'enable-new-dtags', 'disable-new-dtags', 'copy-dt-needed-entries',
	'no-copy-dt-needed-entries', 'add-needed', 'no-add-needed', 'demangle', 'no-demangle',
	'no-keep-memory', 'reduce-memory-overheads', 'cref', 'check-sections', 'no-check-sections',
	'combreloc', 'nocombreloc', 'ld-generated-unwind-info', 'no-ld-generated-unwind-info',
	'warn-shared-textrel', 'warn-unresolved-symbols', 'error-unresolved-symbols', 'no-warn-rwx-segments',
	'warn-rwx-segments', 'no-warn-execstack', 'warn-execstack', 'no-warn-mismatch', 'color-diagnostics',
	'no-color-diagnostics', 'threads', 'no-threads', 'undefined-version', 'no-undefined-version',
	'rosegment', 'no-rosegment', 'no-dynamic-linker', 'print-memory-usage', 'dead_strip',
	'dead_strip_dylibs', 'no_pie', 'search_paths_first', 'search_dylibs_first', 'dynamic', 'dylib',
	'bundle', 'execute', 'no_uuid', 'random_uuid', 'export_dynamic', 'all_load', 'ObjC',
	'no_compact_unwind', 'no_warn_duplicate_libraries', 'no_deduplicate', 'deduplicate',
	'adhoc_codesign', 'no_adhoc_codesign', 'bind_at_load', 'flat_namespace', 'twolevel_namespace',
	'headerpad_max_install_names', 'application_extension', 'no_application_extension', 'fixup_chains',
	'no_fixup_chains', 'ld_classic', 'ld_new', 'fatal_warnings', 'no_function_starts', 'function_starts',
	'no_data_const', 'data_const', 'no_objc_category_merging', 'mark_dead_strippable_dylib',
	'no_implicit_dylibs', 'no_zero_fill_sections', 'merge_zero_fill_sections', 'no_branch_islands',
	'no_eh_labels', 'warn_commons', 'warn_weak_exports', 'prebind', 'noprebind', 'no_weak_imports',
	'arch_errors_fatal', 'keep_private_externs', 'data_in_code_info', 'no_data_in_code_info',
	'reproducible']

// v3_linker_value_options maps an option to the number of arguments that follow it
// and that name no file for the linker to read.
const v3_linker_value_options = {
	'o':                           1
	'output':                      1
	'e':                           1
	'entry':                       1
	'u':                           1
	'undefined':                   1
	'z':                           1
	'm':                           1
	'y':                           1
	'trace-symbol':                1
	'h':                           1
	'soname':                      1
	'rpath':                       1
	'rpath-link':                  1
	'defsym':                      1
	'hash-style':                  1
	'sort-section':                1
	'compress-debug-sections':     1
	'dynamic-linker':              1
	'I':                           1
	'init':                        1
	'fini':                        1
	'a':                           1
	'A':                           1
	'architecture':                1
	'b':                           1
	'format':                      1
	'oformat':                     1
	'G':                           1
	'gpsize':                      1
	'unresolved-symbols':          1
	'wrap':                        1
	'exclude-libs':                1
	'exclude-symbols':             1
	'require-defined':             1
	'image-base':                  1
	'Ttext':                       1
	'Tbss':                        1
	'Tdata':                       1
	'Ttext-segment':               1
	'Trodata-segment':             1
	'Tldata-segment':              1
	'section-start':               1
	'stack':                       1
	'subsystem':                   1
	'thread-count':                1
	'spare-dynamic-tags':          1
	'auxiliary':                   1
	'f':                           1
	'filter':                      1
	'audit':                       1
	'depaudit':                    1
	'pack-dyn-relocs':             1
	'export-dynamic-symbol':       1
	'orphan-handling':             1
	'arch':                        1
	'install_name':                1
	'current_version':             1
	'compatibility_version':       1
	'stack_size':                  1
	'stack_addr':                  1
	'pagezero_size':               1
	'headerpad':                   1
	'image_base':                  1
	'seg1addr':                    1
	'macosx_version_min':          1
	'macos_version_min':           1
	'platform_version':            3
	'sdk_version':                 1
	'exported_symbol':             1
	'unexported_symbol':           1
	'U':                           1
	'alias':                       2
	'sectalign':                   3
	'segprot':                     3
	'segaddr':                     2
	'rename_section':              4
	'rename_segment':              2
	'umbrella':                    1
	'sub_library':                 1
	'sub_umbrella':                1
	'allowable_client':            1
	'client_name':                 1
	'dylib_install_name':          1
	'dylib_current_version':       1
	'dylib_compatibility_version': 1
	'multiply_defined':            1
	'commons':                     1
	'weak_reference_mismatches':   1
	'objc_abi_version':            1
	'final_output':                1
	'oso_prefix':                  1
	'add_empty_section':           2
	'dyld_env':                    1
	'mllvm':                       1
	'mcpu':                        1
}

// v3_linker_output_options make the linker write a file besides the executable: a
// map, a list of what it read, an object, an import library, a trace. A build
// that takes a kept executable links nothing, and would not write it. The list
// holds the options that are known to do so; a linker has more than anyone lists.
const v3_linker_output_options = ['Map', 'map', 'dependency-file', 'dependency_info', 'object_path_lto',
	'out-implib', 'output-def', 'pdb', 'reproduce', 'time-trace', 'print-archive-stats',
	'print-symbol-order', 'lto-obj-path', 'thinlto-index-only', 'save-temps', 'dot', 'debug_snapshot',
	'snapshot_dir', 'bitcode_symbol_map']

// v3_compiler_output_options make a compiler driver write a file besides what it
// compiles: the headers that it read, its temporaries, data for a profile.
const v3_compiler_output_options = ['-MD', '-MMD', '-MF', '-MJ', '-M', '-MM', '-save-temps',
	'--coverage', '-ftest-coverage', '-fprofile-arcs', '-gsplit-dwarf', '-ftime-trace', '-fstack-usage',
	'-fcallgraph-info', '-aux-info', '--serialize-diagnostics', '-serialize-diagnostics',
	'-index-store-path', '-fsave-optimization-record', '-save-stats']

// v3_compiler_output_option_prefixes start the options of v3_compiler_output_options
// that hold their value.
const v3_compiler_output_option_prefixes = ['-save-temps=', '-ftime-trace=', '-fdump-',
	'-fsave-optimization-record=', '-foptimization-record-file=', '-fopt-info', '-save-stats=',
	'-fcallgraph-info=']

// v3_output_environment_names are the variables that make a compiler driver write a
// file besides what it compiles, as an option of v3_compiler_output_options does.
const v3_output_environment_names = ['DEPENDENCIES_OUTPUT', 'SUNPRO_DEPENDENCIES', 'CC_PRINT_OPTIONS',
	'CC_PRINT_HEADERS', 'CC_LOG_DIAGNOSTICS']

// v3_option_writes_a_file reports whether `arg`, an argument of a compiler driver,
// is known to make it write a file besides what it compiles. `-Wp,` hands the
// preprocessor its options: `-Wp,-MD,<file>` writes what it read.
fn v3_option_writes_a_file(arg string) bool {
	if arg in v3_compiler_output_options || v3_compiler_output_option_prefixes.any(arg.starts_with(it)) {
		return true
	}
	if arg.starts_with('-Wp,') {
		for option in arg[4..].split(',') {
			if option in ['-MD', '-MMD', '-MF', '-M', '-MM', '-MJ'] {
				return true
			}
		}
	}
	return false
}

// v3_linker_plain_file_options take a file that the linker reads as it is: a list
// of symbols, an order, a plugin.
const v3_linker_plain_file_options = ['version-script', 'dynamic-list', 'export-dynamic-symbol-list',
	'retain-symbols-file', 'symbol-ordering-file', 'call-graph-ordering-file', 'just-symbols',
	'plugin', 'exported_symbols_list', 'unexported_symbols_list', 'reexported_symbols_list',
	'order_file', 'interposable_list', 'alias_list', 'dtrace', 'lto_library', 'bundle_loader',
	'add_ast_path']

// v3_linker_input_file_options take a file that the linker reads like one that the
// command names without an option: a library, an archive, a linker script.
const v3_linker_input_file_options = ['T', 'script', 'dT', 'default-script', 'force_load', 'load_hidden',
	'reexport_library', 'weak_library', 'needed_library', 'upward_library', 'lazy_library',
	'merge_library']

const v3_linker_framework_options = ['framework', 'weak_framework', 'needed_framework',
	'reexport_framework', 'upward_framework', 'lazy_framework']

// v3_linker_library_prefixes start an option that names a library with it.
const v3_linker_library_prefixes = ['-weak-l', '-reexport-l', '-needed-l', '-upward-l', '-hidden-l',
	'-lazy-l', '-merge-l']

// v3_compiler_options_for_the_linker maps the options that a C compiler driver
// hands to the linker as they are, with the arguments that follow them, to the
// number of those arguments.
const v3_compiler_options_for_the_linker = {
	'-L':                       1
	'-l':                       1
	'-F':                       1
	'-T':                       1
	'-framework':               1
	'-weak_framework':          1
	'-force_load':              1
	'-weak_library':            1
	'-reexport_library':        1
	'-bundle_loader':           1
	'-exported_symbols_list':   1
	'-unexported_symbols_list': 1
	'-order_file':              1
	'-filelist':                1
	'-dylib_file':              1
	'-sectcreate':              3
}

// v3_compiler_option_values returns the number of arguments after the option `arg`
// of a C compiler driver that belong to it, and are no arguments of their own.
fn v3_compiler_option_values(arg string) int {
	if arg in v3_compiler_options_with_value || c_flag_consumes_next_operand(arg) {
		return 1
	}
	return v3_compiler_options_for_the_linker[arg] or { 0 }
}

// V3LinkerArgument is one argument of the command of the linker, as the command of
// the compiler driver gives it: `forwarded` says that it was handed over with
// `-Wl,` or `-Xlinker`, and may be any option that a linker has.
struct V3LinkerArgument {
	text      string
	forwarded bool
}

// add_file records `path`, which the command reads: `as_input` says that the linker
// reads it as an object, an archive, a library or a linker script, and not as it
// is. A relative path is one of the directory in which the command runs. The
// build makes that directory for itself and removes it, so nothing of another
// build is in it: only a path that leaves it names a file to follow.
fn (mut c V3LinkCommand) add_file(path string, as_input bool) {
	resolved := c.path_outside_the_build(path) or { return }
	if os.is_dir(resolved) {
		return
	}
	if as_input {
		c.files << resolved
	} else {
		c.plain_files << resolved
	}
}

// path_outside_the_build returns `path` as an absolute one, or none for a path
// that is empty or in the directory of the build. Where a relative path leaves a
// directory that is not known, the inputs of the command are not known either.
fn (mut c V3LinkCommand) path_outside_the_build(path string) ?string {
	if path.len == 0 {
		return none
	}
	if os.is_abs_path(path) {
		return path
	}
	if !v3_relative_path_leaves_its_directory(path) {
		return none
	}
	if c.build_dir.len == 0 {
		c.unknown = '`${path}` is relative to the directory of the build, and not in it'
		return none
	}
	c.relative_paths = true
	return os.norm_path(os.join_path(c.build_dir, path))
}

// v3_relative_path_leaves_its_directory reports whether a relative path names
// something outside the directory that it is relative to.
fn v3_relative_path_leaves_its_directory(path string) bool {
	return path == '..' || path.starts_with('../') || path.contains('/../') || path.ends_with('/..')
}

// add_linker_option records the option `name` of a linker, given without the
// dashes that start it, and returns the number of arguments after it that belong to
// it, or none when it is no option that is known by that name. `value` is what
// the argument itself gives the option after `=`, and `rest` are the arguments
// that follow in the command of the linker.
fn (mut c V3LinkCommand) add_linker_option(name string, value string, has_value bool, rest []V3LinkerArgument) ?int {
	taken := if has_value { 0 } else { 1 }
	operand := if has_value {
		value
	} else if rest.len > 0 {
		rest[0].text
	} else {
		''
	}
	if name in v3_linker_flag_options {
		return 0
	}
	if name in v3_linker_output_options {
		c.unknown = 'the linker option `-${name}` writes a file besides the executable'
		return taken
	}
	if name in v3_linker_value_options {
		return if has_value { 0 } else { v3_linker_value_options[name] }
	}
	if name in v3_linker_plain_file_options {
		c.add_file(operand, false)
		return taken
	}
	if name in v3_linker_input_file_options {
		c.add_file(operand, true)
		return taken
	}
	if name in v3_linker_framework_options {
		c.frameworks << operand
		return taken
	}
	match name {
		'L', 'library-path' {
			c.library_dirs << operand
			return taken
		}
		'l', 'library' {
			c.libraries << operand
			return taken
		}
		'F' {
			c.framework_dirs << operand
			return taken
		}
		'R' {
			// A directory for the run-time search path, or a file to take symbols of.
			c.add_file(operand, false)
			return taken
		}
		'sectcreate' {
			if rest.len > 2 {
				c.add_file(rest[2].text, false)
			}
			return 3
		}
		'filelist', 'dylib_file' {
			c.unknown = 'the linker option `-${name}` reads files that are named elsewhere'
			return taken
		}
		else {}
	}
	return none
}

// V3AttachedOption is what an option of a linker with its value attached to it
// names, as `-L/dir` does.
enum V3AttachedOption {
	no_option
	library
	library_dir
	framework_dir
	runtime_path
	script
	value
}

// v3_attached_linker_option tells what the argument `arg` of a linker names when
// it is an option with its value attached, and returns the value with it.
fn v3_attached_linker_option(arg string) (V3AttachedOption, string) {
	if arg.starts_with('--') || arg.len < 3 || arg[0] != `-` {
		return V3AttachedOption.no_option, ''
	}
	if library_prefix := v3_linker_library_prefix(arg) {
		return V3AttachedOption.library, arg[library_prefix.len..]
	}
	if arg.starts_with('-L') {
		return V3AttachedOption.library_dir, arg[2..].trim_space()
	}
	if arg.starts_with('-l') {
		return V3AttachedOption.library, arg[2..].trim_space()
	}
	if arg.starts_with('-F') {
		return V3AttachedOption.framework_dir, arg[2..]
	}
	if arg.starts_with('-R') {
		return V3AttachedOption.runtime_path, arg[2..]
	}
	if arg.starts_with('-T') && !arg.contains('=') && arg[1..] !in v3_linker_value_options {
		return V3AttachedOption.script, arg[2..]
	}
	if arg[1] in [`z`, `m`, `O`] || (arg[1] == `G` && arg[2].is_digit()) {
		// `-znow`, `-melf_x86_64`, `-G0`: a value that names no input.
		return V3AttachedOption.value, arg[2..]
	}
	return V3AttachedOption.no_option, ''
}

// add_attached_linker_option records an option of a linker that has its value
// attached to it, and reports whether `arg` is one.
fn (mut c V3LinkCommand) add_attached_linker_option(arg string) bool {
	kind, value := v3_attached_linker_option(arg)
	match kind {
		.no_option {
			return false
		}
		.library {
			c.libraries << value
		}
		.library_dir {
			c.library_dirs << value
		}
		.framework_dir {
			c.framework_dirs << value
		}
		.runtime_path {
			// A directory for the run-time search path, or a file to take symbols of.
			c.add_file(value, false)
		}
		.script {
			c.add_file(value, true)
		}
		.value {}
	}
	return true
}

// add_linker_arguments reads the command of the linker. It is one command: an
// option that takes a value takes it from the argument that follows, whether the
// compiler driver was given the two together, apart, or the value as an argument
// of its own (`-Wl,-L,/dir`, `-Xlinker -L -Xlinker /dir`, `-Wl,-rpath /dir`). An
// option that was handed over to the linker and that is not known is one whose
// inputs are not known either.
fn (mut c V3LinkCommand) add_linker_arguments(linker_args []V3LinkerArgument) {
	mut i := 0
	for i < linker_args.len {
		argument := linker_args[i]
		arg := argument.text
		i++
		if arg.len == 0 {
			continue
		}
		if arg[0] == `@` {
			c.unknown = 'the arguments of `${arg}` are in a file'
			continue
		}
		if arg[0] != `-` || arg == '-' {
			// The linker reads a file that it is given, whatever its name is. What
			// the compiler driver names, is not there, and has no name of a library
			// is what the command writes.
			if argument.forwarded || !os.is_abs_path(arg) || os.is_file(arg)
				|| v3_path_is_link_input(arg) {
				c.add_file(arg, true)
			}
			continue
		}
		mut name := arg.trim_left('-')
		mut value := ''
		mut has_value := false
		if equals := name.index('=') {
			value = name[equals + 1..]
			name = name[..equals]
			has_value = true
		}
		if taken := c.add_linker_option(name, value, has_value, linker_args[i..]) {
			i += taken
		} else if !c.add_attached_linker_option(arg) && argument.forwarded {
			c.unknown_option = 'the linker option `${arg}` is not one whose inputs are known'
			if has_value {
				// What it is given may be a file that it reads as it is.
				c.add_file(value, false)
			}
		}
	}
}

fn v3_linker_library_prefix(arg string) ?string {
	for prefix in v3_linker_library_prefixes {
		if arg.starts_with(prefix) && arg.len > prefix.len {
			return prefix
		}
	}
	return none
}

// v3_parse_link_command reads the arguments of a command that compiles and links a
// program for what the link reads. The compiler driver compiles a file that it
// knows as a source by its name, and hands every other file to the linker,
// whatever its name ends with, in the place that it has in the command, along
// with what `-Wl,` and `-Xlinker` hand over and with the options that it takes for
// the linker itself.
fn v3_parse_link_command(args []string, build_dir string) V3LinkCommand {
	mut command := V3LinkCommand{
		build_dir: build_dir
	}
	mut linker_args := []V3LinkerArgument{}
	mut i := 0
	for i < args.len {
		arg := args[i].trim_space()
		i++
		if arg.len == 0 {
			continue
		}
		if arg == '-Xlinker' {
			if i < args.len {
				linker_args << V3LinkerArgument{args[i].trim_space(), true}
				i++
			}
			continue
		}
		if arg.starts_with('-Wl,') {
			for part in arg[4..].split(',') {
				linker_args << V3LinkerArgument{part, true}
			}
			continue
		}
		if arg[0] == `@` {
			command.unknown = 'the arguments of `${arg}` are in a file'
			continue
		}
		if arg[0] != `-` || arg == '-' {
			if os.file_ext(arg) in v3_compiled_source_suffixes {
				command.add_file(arg, false)
			} else {
				linker_args << V3LinkerArgument{arg, false}
			}
			continue
		}
		if v3_option_writes_a_file(arg) {
			// A build that takes a kept executable compiles nothing, and would
			// not write it.
			command.unknown = 'the option `${arg}` writes a file besides the executable'
			i += v3_compiler_option_values(arg)
			continue
		}
		if arg == '-specs' || arg.starts_with('-specs=') || arg.starts_with('--specs=') {
			// A specs file tells the driver what to compile and link with.
			command.unknown = 'the option `${arg}` changes what the compiler driver links'
			if arg == '-specs' {
				i++
			}
			continue
		}
		if values := v3_compiler_options_for_the_linker[arg] {
			linker_args << V3LinkerArgument{arg, false}
			for _ in 0 .. values {
				if i < args.len {
					linker_args << V3LinkerArgument{args[i].trim_space(), false}
					i++
				}
			}
			continue
		}
		values := v3_compiler_option_values(arg)
		if values > 0 {
			i += values
			continue
		}
		kind, _ := v3_attached_linker_option(arg)
		if kind in [.library, .library_dir, .framework_dir, .script] {
			// The driver hands these to the linker as they are.
			linker_args << V3LinkerArgument{arg, false}
		} else if arg.contains('=') && !arg.starts_with('-D') && !arg.starts_with('-U') {
			// `-fprofile-use=/path`, `--sysroot=/path`: a file is an input, and the
			// linker reads none of these as one of its own. The value of a macro
			// names none.
			command.add_file(arg.all_after('='), false)
		}
	}
	command.add_linker_arguments(linker_args)
	return command
}

// v3_link_library_names returns the file names that a linker gives the library
// `name` of `-l`: `-l:file` names the file itself.
fn v3_link_library_names(name string) []string {
	if name.starts_with(':') {
		return [name[1..]]
	}
	return ['lib${name}.dylib', 'lib${name}.tbd', 'lib${name}.so', 'lib${name}.a']
}

const v3_archive_magic = '!<arch>\n'
const v3_thin_archive_magic = '!<thin>\n'

// v3_read_file_start returns up to `limit` bytes from the start of a file.
fn v3_read_file_start(path string, limit int) []u8 {
	mut file := os.open(path) or { return []u8{} }
	defer {
		file.close()
	}
	mut buffer := []u8{len: limit}
	count := file.read(mut buffer) or { return []u8{} }
	return buffer[..count]
}

// v3_linker_script_words returns the words of a linker script: its keywords, the
// files and libraries that it names, and its parentheses, without comments.
fn v3_linker_script_words(text string) []string {
	mut words := []string{}
	mut i := 0
	for i < text.len {
		c := text[i]
		if c == `/` && i + 1 < text.len && text[i + 1] == `*` {
			end := text.index_after_('*/', i + 2)
			i = if end < 0 { text.len } else { end + 2 }
		} else if c in [` `, `\t`, `\r`, `\n`, `,`] {
			i++
		} else if c in [`(`, `)`] {
			words << c.ascii_str()
			i++
		} else {
			start := i
			for i < text.len && text[i] !in [` `, `\t`, `\r`, `\n`, `,`, `(`, `)`] {
				i++
			}
			words << text[start..i]
		}
	}
	return words
}

// v3_link_file_is_binary reports whether `start`, the first bytes of a file that a
// linker reads, are those of an object, an archive or a library in a binary format,
// or of a text stub of a library, and not those of a linker script.
fn v3_link_file_is_binary(start []u8) bool {
	if start.len == 0 || start.any(it == 0) {
		return true
	}
	text := start.bytestr()
	// An archive starts with text, and so does a stub of a library of an SDK.
	return text.starts_with(v3_archive_magic) || text.starts_with('---')
		|| text.starts_with('BC\xc0\xde')
}

// expand_link_file adds what the link reads through `path` to `command`. An object,
// an archive or a library in a binary format is read as it is. A thin archive holds
// the names of its members, which are other files, and a linker script names the
// files and libraries to read in its place: the first cannot be followed here, the
// second can when it does nothing but name them. What a file is shows in what it
// holds: a linker reads a script whatever its name ends with.
fn (mut command V3LinkCommand) expand_link_file(path string) {
	start := v3_read_file_start(path, 4096)
	if start.len >= v3_thin_archive_magic.len
		&& start[..v3_thin_archive_magic.len].bytestr() == v3_thin_archive_magic {
		command.unknown = '`${path}` is a thin archive: its members are other files'
		return
	}
	if v3_link_file_is_binary(start) {
		return
	}
	// Text where an object or a library is expected is a linker script, as glibc
	// has for libc.
	text := os.read_file(path) or { return }
	words := v3_linker_script_words(text)
	mut i := 0
	for i < words.len {
		word := words[i]
		i++
		if word in ['GROUP', 'INPUT', 'AS_NEEDED', '(', ')'] {
			continue
		}
		if word in ['OUTPUT_FORMAT', 'OUTPUT_ARCH'] {
			// Their arguments name a format, not a file.
			for i < words.len && words[i] != ')' {
				i++
			}
			continue
		}
		if word.starts_with('-l') && word.len > 2 {
			command.libraries << word[2..]
		} else if os.is_abs_path(word) {
			command.files << word
		} else if word.contains('.') && !word.contains('=') && word.bytes().all(it.is_letter()
			|| it.is_digit() || it in [`.`, `_`, `-`, `+`]) {
			// A file that is looked up like a library.
			command.libraries << ':${word}'
		} else {
			command.unknown = 'the linker script `${path}` does more than name its inputs: `${word}`'
			return
		}
	}
}

// v3_program_link_inputs returns what the command `args` reads to link a program.
// `linker_dir` is the directory of the archives and objects that TinyCC links
// without being asked to, and `default_library_dirs` are the directories that the
// linker searches after those of `-L`.
//
// A library of `-l` is recorded with every name that a linker gives it in every
// directory of the search: the file where there is one, the path where there is
// none, and the directory itself where that is not there. A library that is
// installed, removed or replaced in any of them changes what the link reads.
fn v3_program_link_inputs(args []string, linker_dir string, default_library_dirs []string, build_dir string) V3ProgramLinkInputs {
	mut command := v3_parse_link_command(args, build_dir)
	mut files := map[string]bool{}
	mut missing := map[string]bool{}
	mut expanded := map[string]bool{}
	mut resolved_libraries := map[string]bool{}
	if linker_dir.len > 0 {
		for name in os.ls(linker_dir) or { []string{} } {
			if name.ends_with('.a') || name.ends_with('.o') {
				command.files << os.join_path_single(linker_dir, name)
			}
		}
	}
	for given in command.framework_dirs.clone() {
		// A directory that is relative is one of the build: see add_file.
		dir := command.path_outside_the_build(given) or { continue }
		for framework in command.frameworks {
			for name in [framework, '${framework}.tbd'] {
				command.files << os.join_path(dir, '${framework}.framework', name)
			}
		}
	}
	for path in command.plain_files {
		if os.is_file(path) {
			files[path] = true
		} else {
			missing[path] = true
		}
	}
	// A linker script adds files and libraries, which are read like the others.
	mut settled := false
	for _ in 0 .. 16 {
		mut library_dirs := []string{}
		for given in command.library_dirs.clone() {
			// A directory that is relative is one of the build: see add_file.
			dir := command.path_outside_the_build(given) or { continue }
			if dir !in library_dirs {
				library_dirs << dir
			}
		}
		for dir in default_library_dirs {
			if dir !in library_dirs {
				library_dirs << dir
			}
		}
		for library in command.libraries {
			if resolved_libraries[library] {
				continue
			}
			resolved_libraries[library] = true
			for dir in library_dirs {
				if !os.is_dir(dir) {
					missing[dir] = true
					continue
				}
				for name in v3_link_library_names(library) {
					command.files << os.join_path_single(dir, name)
				}
			}
		}
		mut added := false
		for path in command.files {
			if expanded[path] {
				continue
			}
			expanded[path] = true
			added = true
			if os.is_file(path) {
				files[path] = true
				command.expand_link_file(path)
			} else {
				missing[path] = true
			}
		}
		if !added || command.unknown.len > 0 {
			settled = true
			break
		}
	}
	if !settled {
		command.unknown = 'the linker scripts of the command name each other without end'
	}
	mut inputs := V3ProgramLinkInputs{
		taken:          true
		files:          files.keys()
		missing:        missing.keys()
		unknown:        command.unknown
		unknown_option: command.unknown_option
		relative_paths: command.relative_paths
	}
	inputs.files.sort()
	inputs.missing.sort()
	for name in v3_output_environment_names {
		if os.getenv(name).len > 0 && inputs.unknown.len == 0 {
			inputs.unknown = 'the variable `${name}` makes the compiler write a file besides the executable'
		}
	}
	// A command can define a macro as the time of the compilation, or as a part
	// of the name of such a macro: `-DBUILT=__TIME__`. Each argument is a text of
	// its own.
	mut words := args.clone()
	words << os.getenv('CCC_OVERRIDE_OPTIONS')
	for word in words {
		if v3_c_mentions_compile_time(word) {
			inputs.time_question = 'the command names a macro for the time of the compilation'
		}
		for part in v3_c_time_name_parts(word) {
			if part !in inputs.time_parts {
				inputs.time_parts << part
			}
		}
	}
	inputs.time_parts.sort()
	for path in inputs.files {
		identity := modulecache.file_metadata_signature(path)
		if identity.len == 0 && inputs.unknown.len == 0 {
			inputs.unknown = '`${path}` cannot be told apart from a changed file'
		}
		inputs.identities << identity
	}
	return inputs
}

// v3_parse_library_search_dirs reads the directories of libraries from what a C
// compiler prints for `-print-search-dirs`: `libraries: =/a:/b` for GCC and Clang,
// a `libraries:` line followed by one directory a line for TinyCC.
fn v3_parse_library_search_dirs(output string) []string {
	mut dirs := []string{}
	mut in_libraries := false
	for line in output.split_into_lines() {
		if line.starts_with('libraries:') {
			in_libraries = true
			rest := line['libraries:'.len..].trim_space().trim_left('=')
			for dir in rest.split(os.path_delimiter) {
				if dir.len > 0 {
					dirs << dir
				}
			}
			continue
		}
		if !line.starts_with(' ') {
			in_libraries = false
		} else if in_libraries {
			dirs << line.trim_space()
		}
	}
	return dirs
}

// v3_default_link_library_dirs returns the directories that `linker` searches for
// a library of `-l` after those of `-L`: those of LIBRARY_PATH, those that it
// reports itself, and those of the system. What a compiler reports is asked once
// for a module cache, which belongs to one compiler: the answer is kept in it.
// Only directories with an absolute path are of use: TinyCC gives its own relative
// to the directory that it runs in.
fn v3_default_link_library_dirs(manager &modulecache.Manager, linker string, base_args []string, sdk_root string, build_dir string) []string {
	mut candidates := []string{}
	for dir in os.getenv('LIBRARY_PATH').split(os.path_delimiter) {
		// A relative directory is one of the directory that the linker runs in.
		if dir.len > 0 && !os.is_abs_path(dir) && build_dir.len > 0
			&& v3_relative_path_leaves_its_directory(dir) {
			candidates << os.norm_path(os.join_path(build_dir, dir))
		} else {
			candidates << dir
		}
	}
	// A compiler driver answers by its environment as well: GCC adds the
	// directories of `LIBRARY_PATH` to what it prints, and finds its own by
	// `GCC_EXEC_PREFIX`.
	record := os.join_path_single(manager.dir, 'link_library_dirs_${c_hash_bytes(u64(1469598103934665603), [
		'v3-link-library-dirs-3',
		os.real_path(linker),
		v3_cache_file_identity(linker),
		base_args.join('\n'),
		v3_link_environment_signature(),
	].join('\x01').bytes()).hex()}')
	reported := os.read_file(record) or {
		mut query := base_args.clone()
		query << '-print-search-dirs'
		// GCC names what it prints in the language of the user.
		result := v3_run_in_c_locale(linker, query, '')
		answer := if result.exit_code == 0 {
			v3_parse_library_search_dirs(result.output).join('\n') + '\n'
		} else {
			''
		}
		// A query that failed may answer the next time.
		if answer.len > 0 && manager.ensure_dir() {
			os.write_file(record, answer) or {}
		}
		answer
	}
	candidates << reported.split_into_lines()
	if sdk_root.len > 0 {
		candidates << os.join_path(sdk_root, 'usr', 'lib')
		candidates << os.join_path(sdk_root, 'usr', 'local', 'lib')
	}
	candidates << ['/usr/local/lib', '/usr/lib', '/lib']
	mut dirs := []string{}
	for candidate in candidates {
		if candidate.len == 0 || !os.is_abs_path(candidate) {
			continue
		}
		dir := os.norm_path(candidate)
		if dir !in dirs {
			dirs << dir
		}
	}
	return dirs
}

// v3_link_environment_names are the variables of the environment that a C compiler
// driver, its preprocessor or its linker reads for what it searches, reads or
// writes into its output. TinyCC reads the first three.
const v3_link_environment_names = ['LIBRARY_PATH', 'CPATH', 'C_INCLUDE_PATH', 'OBJC_INCLUDE_PATH',
	'COMPILER_PATH', 'GCC_EXEC_PREFIX', 'LD_RUN_PATH', 'LD_LIBRARY_PATH', 'GNUTARGET', 'LDEMULATION',
	'SDKROOT', 'MACOSX_DEPLOYMENT_TARGET', 'DEVELOPER_DIR', 'TOOLCHAINS', 'CCC_OVERRIDE_OPTIONS',
	'SOURCE_DATE_EPOCH', 'ZERO_AR_DATE']

// v3_link_environment_signature returns the part of the environment that decides,
// along with its arguments, what a command that compiles and links a program
// reads and makes. It belongs to the identity of an executable that a later build
// may take in place of running the command: the files that the command read can
// all be as they were while the environment sends it to others.
fn v3_link_environment_signature() string {
	mut parts := []string{cap: v3_link_environment_names.len}
	for name in v3_link_environment_names {
		if value := os.getenv_opt(name) {
			parts << '${name}=${value}'
		}
	}
	return parts.join('\x00')
}

// v3_link_search_args returns the arguments of a command that links with a C
// compiler driver which change where the driver looks for libraries: they belong
// to the question that v3_default_link_library_dirs asks it.
fn v3_link_search_args(args []string) []string {
	mut search := []string{}
	mut i := 0
	for i < args.len {
		arg := args[i].trim_space()
		i++
		values := v3_compiler_option_values(arg)
		if values > 0 {
			if arg in ['-isysroot', '--sysroot', '-target', '-arch', '-B'] && i < args.len {
				search << [arg, args[i].trim_space()]
			}
			i += values
			continue
		}
		if arg.starts_with('--sysroot=') || arg.starts_with('--target=') || arg.starts_with('--ld-path=')
			|| (arg.starts_with('-B') && !arg.starts_with('-Bs') && !arg.starts_with('-Bd'))
			|| arg.starts_with('-m') || arg.starts_with('-stdlib=') || arg.starts_with('-fuse-ld=')
			|| arg in ['-static', '-static-pie', '-nostdlib'] {
			search << arg
		}
	}
	return search
}

// v3_run_in_c_locale runs `program` like cmdexec.run_in_merged, with the messages
// of the program in the language that this compiler reads them in: GCC prints
// where it searches in the language of the user.
fn v3_run_in_c_locale(program string, args []string, work_folder string) os.Result {
	return cmdexec.run_in_merged_with_env(program, args, work_folder, {
		'LC_ALL':   'C'
		'LANGUAGE': 'C'
	})
}

// A linker can tell what it read: every file, and with Apple's linker every path
// where it looked for one in vain. That is the link itself, and not what the
// command seems to ask for, so it holds the libraries and the start files that a
// compiler driver adds by itself, and whatever an option that is not known here
// makes the linker read.

// v3_linker_report_file is where the linker writes what it read, in the directory
// of the build.
const v3_linker_report_file = 'v3_program_link.report'

// V3LinkerReport is the kind of account that a linker gives of its inputs.
enum V3LinkerReport {
	// It gives none that is known here.
	no_report
	// `-dependency_info` of Apple's linker and of LLD for Mach-O: the files that it
	// read and the paths where it found none.
	dependency_info
	// `--dependency-file` of GNU ld, gold, LLD and mold: the files that it read, as
	// a rule of `make`.
	dependency_file
}

// v3_linker_report_args returns the arguments that make the linker of a compiler
// driver write its report to v3_linker_report_file in the directory that the
// command runs in. The name is relative: a compiler driver takes an argument for
// the linker apart at each comma, and the path of a directory can hold one.
fn v3_linker_report_args(report V3LinkerReport) []string {
	return match report {
		.no_report { []string{} }
		.dependency_info { ['-Wl,-dependency_info,${v3_linker_report_file}'] }
		.dependency_file { ['-Wl,--dependency-file=${v3_linker_report_file}'] }
	}
}

// v3_linker_probe_args returns the arguments among `search_args` that choose a
// linker: an empty program is linked with those, and not with the ones that
// take its libraries or its start files away.
fn v3_linker_probe_args(search_args []string) []string {
	return search_args.filter(it !in ['-static', '-static-pie', '-nostdlib'])
}

// v3_output_names_unknown_option reports whether `output`, what a command that
// links printed, says that the linker does not know the option of `report`.
fn v3_output_names_unknown_option(report V3LinkerReport, output string) bool {
	option := match report {
		.no_report { return false }
		.dependency_info { 'dependency_info' }
		.dependency_file { 'dependency-file' }
	}
	for line in output.split_into_lines() {
		if line.contains(option) && (line.contains('unrecognized') || line.contains('unknown')
			|| line.contains('unsupported') || line.contains('invalid option')) {
			return true
		}
	}
	return false
}

// v3_output_names_linker_report reports whether `output`, what a command that
// links printed, names the option of `report` at all. A linker that fails over an
// option names it, in whatever language it speaks.
fn v3_output_names_linker_report(report V3LinkerReport, output string) bool {
	return match report {
		.no_report { false }
		.dependency_info { output.contains('dependency_info') }
		.dependency_file { output.contains('dependency-file') }
	}
}

// v3_linker_report_record returns the file of the module cache that keeps which
// report the linker of `compiler` gives. What chooses the linker is part of its
// name: the compiler, the arguments that choose one, the variables that GCC finds
// its programs by, and those by which the compiler of an Apple toolchain, which
// is one file for all of them, finds the toolchain to run. A linker that is
// another one all the same is found out by the link that fails over the option:
// see v3_forget_linker_report.
fn v3_linker_report_record(manager &modulecache.Manager, compiler string, search_args []string, target_os string) string {
	identity := ['v3-linker-report-4', os.real_path(compiler), v3_cache_file_identity(compiler),
		target_os, v3_linker_probe_args(search_args).join('\n'), os.getenv('COMPILER_PATH'),
		os.getenv('GCC_EXEC_PREFIX'), os.getenv('DEVELOPER_DIR'), os.getenv('TOOLCHAINS'),
		os.getenv('SDKROOT')]
	return os.join_path_single(manager.dir, 'linker_report_${c_hash_bytes(u64(1469598103934665603), identity.join('\x00').bytes()).hex()}')
}

// v3_forget_linker_report records that the linker of `compiler` gives no report
// after all: a command that asked for one failed over the option.
fn v3_forget_linker_report(manager &modulecache.Manager, compiler string, search_args []string, target_os string) {
	if manager.ensure_dir() {
		os.write_file(v3_linker_report_record(manager, compiler, search_args, target_os),
			'no\n') or {}
	}
}

// v3_linker_report tells which report the linker of `compiler` gives when it runs
// with `search_args`, the arguments that choose a linker and its libraries. It
// finds out by linking an empty program in `cc_dir`, once for a module cache: the
// answer is kept there. A linker that says that it does not know the option is
// not asked again; a link that fails for another reason is.
fn v3_linker_report(manager &modulecache.Manager, compiler string, search_args []string, target_os string, cc_dir string) V3LinkerReport {
	candidate := if target_os == 'macos' {
		V3LinkerReport.dependency_info
	} else {
		V3LinkerReport.dependency_file
	}
	record := v3_linker_report_record(manager, compiler, search_args, target_os)
	if kept := os.read_file(record) {
		if kept == 'yes\n' {
			return candidate
		}
		if kept == 'no\n' {
			return .no_report
		}
	}
	source := os.join_path_single(cc_dir, 'v3_link_probe.c')
	output := os.join_path_single(cc_dir, 'v3_link_probe.out')
	report := os.join_path_single(cc_dir, v3_linker_report_file)
	defer {
		os.rm(source) or {}
		os.rm(output) or {}
		os.rm(report) or {}
	}
	os.write_file(source, 'int main(void) { return 0; }\n') or { return .no_report }
	mut args := v3_linker_probe_args(search_args)
	args << v3_linker_report_args(candidate)
	args << ['-o', os.file_name(output), os.file_name(source)]
	result := v3_run_in_c_locale(compiler, args, cc_dir)
	mut answer := ''
	if result.exit_code == 0 && os.file_size(report) > 0 {
		answer = 'yes\n'
	} else if v3_output_names_unknown_option(candidate, result.output) {
		answer = 'no\n'
	}
	if answer.len > 0 && manager.ensure_dir() {
		os.write_file(record, answer) or {}
	}
	return if answer == 'yes\n' { candidate } else { V3LinkerReport.no_report }
}

// v3_read_linker_report returns the files that a linker read and the paths where
// it found none, from the report that it wrote to `path`, or none when there is no
// report to read.
fn v3_read_linker_report(report V3LinkerReport, path string) ?([]string, []string) {
	mut found := []string{}
	mut not_found := []string{}
	match report {
		.no_report {
			return none
		}
		.dependency_file {
			text := os.read_file(path) or { return none }
			found = v3_parse_dependency_file(text)
		}
		.dependency_info {
			// A record is a byte that tells its kind and a path that ends with a
			// zero byte: 0x10 a file that was read, 0x11 a path where none was, 0x40
			// the output, 0x00 the version of the linker.
			bytes := os.read_bytes(path) or { return none }
			mut seen := map[string]bool{}
			mut i := 0
			for i < bytes.len {
				kind := bytes[i]
				mut end := i + 1
				for end < bytes.len && bytes[end] != 0 {
					end++
				}
				if end >= bytes.len {
					return none
				}
				name := bytes[i + 1..end].bytestr()
				i = end + 1
				if kind !in [u8(0x10), 0x11] || seen[name] {
					continue
				}
				seen[name] = true
				if kind == 0x10 {
					found << name
				} else {
					not_found << name
				}
			}
		}
	}
	if found.len == 0 {
		return none
	}
	return found, not_found
}

// add_linker_report adds what the linker says that it read to the inputs of the
// executable, from the report that it wrote to `path` when it ran in `cc_dir`. A
// file that the command names has the metadata that it had before the link; one
// that the linker found by itself gets the metadata that it has now, and must be
// older than `before`, a time in seconds from before the command started. What is
// no longer there was made and removed by the command. `system_roots` are
// directories that only an update of the system changes, as an SDK is: the files
// that the linker takes from deep inside one, and the places in one where it
// found nothing, come and go with the files that it names there, and are not
// recorded one by one.
//
// A report that tells where the linker found nothing shows what an option that is
// not known here made it look for: such an option is no reason to keep no
// executable then.
fn (mut inputs V3ProgramLinkInputs) add_linker_report(report V3LinkerReport, path string, cc_dir string, before i64, system_roots []string) {
	if report == .no_report || inputs.unknown.len > 0 {
		return
	}
	found, not_found := v3_read_linker_report(report, path) or {
		inputs.unknown = 'the linker did not tell what it read'
		return
	}
	mut known := map[string]bool{}
	for file in inputs.files {
		known[file] = true
	}
	for name in found {
		file := inputs.reported_path(name, cc_dir) or { continue }
		if known[file] || !os.exists(file) || v3_is_deep_in_a_system_root(file, system_roots) {
			continue
		}
		known[file] = true
		identity := v3_file_identity_from_before(file, before) or {
			inputs.unknown = '`${file}` cannot be told apart from a changed file'
			return
		}
		inputs.files << file
		inputs.identities << identity
	}
	mut absent := map[string]bool{}
	for file in inputs.missing {
		absent[file] = true
	}
	for name in not_found {
		file := inputs.reported_path(name, cc_dir) or { continue }
		if absent[file] || file.contains_any('\n') || system_roots.any(file.starts_with(it + '/')) {
			continue
		}
		absent[file] = true
		inputs.missing << file
	}
	if report == .dependency_info {
		inputs.unknown_option = ''
	}
}

// reported_path returns a path of a linker report as an absolute one, or none for
// a file of the directory of the build. The linker names what it found through a
// relative directory relative to the directory that it runs in.
fn (mut inputs V3ProgramLinkInputs) reported_path(name string, cc_dir string) ?string {
	if name.len == 0 {
		return none
	}
	if os.is_abs_path(name) {
		return if name.starts_with(cc_dir + '/') { none } else { name }
	}
	if !v3_relative_path_leaves_its_directory(name) {
		return none
	}
	inputs.relative_paths = true
	return os.norm_path(os.join_path(cc_dir, name))
}

// v3_is_deep_in_a_system_root reports whether `path` is a file that a linker takes
// from a system root without being asked for it by name: a member of
// `usr/lib/system`, which the library of the system is made of.
fn v3_is_deep_in_a_system_root(path string, system_roots []string) bool {
	for root in system_roots {
		if path.starts_with(os.join_path(root, 'usr', 'lib', 'system') + '/') {
			return true
		}
	}
	return false
}
