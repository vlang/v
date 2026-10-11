module driver

import os
import v.cmdexec
import v.modulecache

// The executable of a program is kept for the next build of the same inputs (see
// modulecache.Manager.valid_program_executable and program_link_inputs.v). A build
// that takes it does not compile the C of the program again, so what the C compiler
// read for that C is an input like what the linker read: the headers of the C
// library and of the libraries that the program uses. They are no part of the C
// that V generates, and a header that an installer replaces, or one that appears
// where an `#include` finds it first, changes what the same C compiles to.
//
// TinyCC tells what it read in the line markers of what it preprocesses: the record
// that tcc_prelude.v keeps of the headers of a unit holds it. A compiler driver
// writes it down while it compiles, when it is asked to with `-MD`.

// v3_program_dependency_file is where the C compiler writes the files that it read
// for the program, in the directory of the build.
const v3_program_dependency_file = 'v3_program_unit.d'

// v3_compiled_sources returns the files that the compiler driver of the command
// `args` compiles: the arguments that are no options, no values of options, and
// that it knows as sources by their names.
fn v3_compiled_sources(args []string) []string {
	mut sources := []string{}
	mut i := 0
	for i < args.len {
		arg := args[i].trim_space()
		i++
		if arg.len == 0 {
			continue
		}
		if arg[0] == `-` {
			i += v3_compiler_option_values(arg)
			continue
		}
		ext := os.file_ext(arg)
		if ext in v3_compiled_source_suffixes && ext !in ['.h', '.hh', '.hpp', '.H'] {
			sources << arg
		}
	}
	return sources
}

// v3_parse_dependency_file returns the files that the rules of `text` name as what
// their targets are made of: `text` is what a C compiler writes for `make` with
// `-MD`. A space, a `#` or a backslash of a name has a backslash before it, a `$`
// is written twice, and a backslash at the end of a line continues the rule.
fn v3_parse_dependency_file(text string) []string {
	mut files := []string{}
	mut word := []u8{}
	mut in_target := true
	mut i := 0
	for i < text.len {
		c := text[i]
		if c == `\\` && i + 1 < text.len {
			next := text[i + 1]
			if next == `\n` {
				i += 2
				continue
			}
			if next == `\r` && i + 2 < text.len && text[i + 2] == `\n` {
				i += 3
				continue
			}
			if next in [` `, `#`, `\\`, `:`] {
				word << next
				i += 2
				continue
			}
		}
		if c == `$` && i + 1 < text.len && text[i + 1] == `$` {
			word << `$`
			i += 2
			continue
		}
		if c == `:` && in_target && (i + 1 >= text.len || text[i + 1] in [` `, `\t`, `\n`, `\r`]) {
			// What was read so far names the target.
			word.clear()
			in_target = false
			i++
			continue
		}
		if c in [` `, `\t`, `\n`, `\r`] {
			if word.len > 0 {
				if !in_target {
					name := word.bytestr()
					if name !in files {
						files << name
					}
				}
				word.clear()
			}
			if c == `\n` {
				in_target = true
			}
			i++
			continue
		}
		word << c
		i++
	}
	if word.len > 0 && !in_target {
		name := word.bytestr()
		if name !in files {
			files << name
		}
	}
	return files
}

// v3_parse_include_search reads what GCC and Clang print for `-E -v`: the
// directories between `#include "..." search starts here:` and
// `End of search list.`, those for `#include "..."` first, and before them the
// ones that they leave out because they are not there.
fn v3_parse_include_search(output string) ?V3IncludeSearch {
	mut search := V3IncludeSearch{}
	mut listing := false
	mut ended := false
	for line in output.split_into_lines() {
		if line.starts_with('ignoring nonexistent directory "') {
			dir := line.all_after('"').all_before_last('"')
			if dir.len > 0 && dir !in search.absent {
				search.absent << dir
			}
			continue
		}
		if line.starts_with('#include "..." search starts here:')
			|| line.starts_with('#include <...> search starts here:') {
			listing = true
			continue
		}
		if line.starts_with('End of search list.') {
			ended = true
			break
		}
		if listing && line.starts_with(' ') {
			mut dir := line.trim_space()
			framework := dir.ends_with(' (framework directory)')
			if framework {
				dir = dir.all_before_last(' (framework directory)')
			}
			if dir.len == 0 || dir in search.dirs {
				continue
			}
			search.dirs << dir
			if framework {
				search.frameworks << dir
			}
		}
	}
	if !ended {
		return none
	}
	return search
}

// v3_include_search_args returns the arguments of a command that compiles with a C
// compiler driver which decide where the driver looks for an included file.
fn v3_include_search_args(args []string) []string {
	mut search := []string{}
	mut i := 0
	for i < args.len {
		arg := args[i].trim_space()
		i++
		if arg in ['-I', '-isystem', '-iquote', '-idirafter', '-isysroot', '--sysroot', '-target',
			'-arch', '-B', '-iprefix', '-iwithprefix', '-iwithprefixbefore', '-imultilib', '-iframework',
			'-F'] {
			if i < args.len {
				search << [arg, args[i].trim_space()]
				i++
			}
			continue
		}
		values := v3_compiler_option_values(arg)
		if values > 0 {
			i += values
			continue
		}
		if arg.starts_with('-I') || arg.starts_with('-isystem') || arg.starts_with('-iquote')
			|| arg.starts_with('-idirafter') || arg.starts_with('-isysroot')
			|| arg.starts_with('--sysroot=') || arg.starts_with('--target=')
			|| arg.starts_with('-B') || arg.starts_with('-m') || arg.starts_with('-stdlib=')
			|| arg.starts_with('-std=') || arg.starts_with('-F') || arg.starts_with('-iframework')
			|| arg in ['-nostdinc', '-nostdlibinc', '-nobuiltininc', '-ffreestanding', '-pthread'] {
			search << arg
		}
	}
	return search
}

// v3_include_search returns where `compiler` looks for an included file when it
// compiles with `args` in the directory `cc_dir`. It asks the compiler, once for a
// module cache, a set of arguments and an environment: the answer is kept there for
// as long as the directories that it names are there, and those that it leaves out
// are not.
fn v3_include_search(manager &modulecache.Manager, compiler string, args []string, cc_dir string) ?V3IncludeSearch {
	search_args := v3_include_search_args(args)
	mut identity := ['v3-include-search-2', os.real_path(compiler), v3_cache_file_identity(compiler),
		('objective-c' in args).str()]
	identity << search_args
	for name in v3_link_environment_names {
		identity << '${name}=${os.getenv(name)}'
	}
	record := os.join_path_single(manager.dir, 'include_search_${c_hash_bytes(u64(1469598103934665603), identity.join('\n').bytes()).hex()}')
	if kept := os.read_file(record) {
		if search := v3_parse_include_search(kept) {
			absolute := v3_absolute_include_search(search, cc_dir)
			if absolute.dirs.all(os.is_dir(it)) && absolute.absent.all(!os.exists(it)) {
				return absolute
			}
		}
	}
	mut query := search_args.clone()
	query << ['-x', if 'objective-c' in args { 'objective-c' } else { 'c' }, '-E', '-v',
		os.path_devnull]
	// The compiler says where it searches on its standard error, in the language
	// of the user unless it is asked for another.
	result := v3_run_in_c_locale(compiler, query, cc_dir)
	if result.exit_code != 0 {
		return none
	}
	search := v3_parse_include_search(result.output)?
	if manager.ensure_dir() {
		os.write_file(record, result.output) or {}
	}
	return v3_absolute_include_search(search, cc_dir)
}

// v3_absolute_include_search returns `search` with the directories that are
// relative to the directory of the build as absolute ones.
fn v3_absolute_include_search(search V3IncludeSearch, cc_dir string) V3IncludeSearch {
	return V3IncludeSearch{
		dirs:       v3_tcc_absolute_include_dirs(search.dirs, cc_dir)
		frameworks: v3_tcc_absolute_include_dirs(search.frameworks, cc_dir)
		absent:     v3_tcc_absolute_include_dirs(search.absent, cc_dir)
	}
}

// add_header_inputs adds what the compilation of the program depends on to the
// inputs of its executable: the headers that the C compiler read, each with the
// metadata that it had when it was read, and the places where it would have found
// another. An executable is kept of a compilation whose inputs can all be told,
// and that gives the same at another time.
fn (mut inputs V3ProgramLinkInputs) add_header_inputs(headers &V3HeaderInputs) {
	if inputs.unknown.len > 0 {
		return
	}
	if headers.unknown.len > 0 {
		inputs.unknown = headers.unknown
		return
	}
	// Whether a C that names a macro for the time of its compilation asks for
	// it is told by what the preprocessor makes of it: see settle_time. A part of
	// such a name can come from the command, from a header and from the unit.
	if headers.mentions_time && inputs.time_question.len == 0 {
		inputs.time_question = 'the C of the program names a macro for the time of its compilation'
	}
	for part in headers.time_parts {
		if part !in inputs.time_parts {
			inputs.time_parts << part
		}
	}
	if inputs.time_question.len == 0 && v3_time_names_can_be_built(inputs.time_parts) {
		inputs.time_question = 'the C of the program can put the name of a macro for the time of its compilation together of ${inputs.time_parts.join(', ')}'
	}
	if headers.relative_paths {
		inputs.relative_paths = true
	}
	for i, file in headers.files {
		if file !in inputs.files {
			inputs.files << file
			inputs.identities << headers.identities[i]
		}
	}
	for path in headers.missing {
		if path !in inputs.missing {
			inputs.missing << path
		}
	}
}

// v3_program_time_file is where the unit of a program is preprocessed to, in the
// directory of the build, to tell whether it asks for the time.
const v3_program_time_file = 'v3_program_unit.time.i'

// v3_time_question_args returns the command `args`, which compiles and links the
// unit of a program with a compiler driver, as one that preprocesses the unit to
// v3_program_time_file. A warning is no error in it: of what the command links,
// a driver that preprocesses uses nothing, and says so.
fn v3_time_question_args(args []string) []string {
	mut question := []string{cap: args.len + 5}
	mut i := 0
	for i < args.len {
		arg := args[i].trim_space()
		if arg in ['-o', '-MF', '-MT', '-MQ'] {
			i += 2
		} else if arg in ['-MD', '-MMD'] {
			i++
		} else {
			question << args[i]
			i++
		}
	}
	question << ['-Wno-error', '-w', '-E', '-o', v3_program_time_file]
	return question
}

// v3_preprocessed_c_asks_for_time reports whether the C that the command `args`
// of `compiler` preprocesses to v3_program_time_file, in `cc_dir`, holds what a
// macro for the time of the compilation expands to: a string with a date or a
// time of day in it. A string that the C spells out so looks the same. It returns
// none for a command that fails, which tells nothing.
fn v3_preprocessed_c_asks_for_time(compiler string, args []string, cc_dir string) ?bool {
	output := os.join_path_single(cc_dir, v3_program_time_file)
	os.rm(output) or {}
	defer {
		os.rm(output) or {}
	}
	if cmdexec.run_in(compiler, args, cc_dir).exit_code != 0 {
		return none
	}
	preprocessed := os.read_file(output) or { return none }
	return v3_c_has_time_literal(preprocessed)
}

// add_compiled_headers adds the headers that a C compiler read when it ran a
// command in `cc_dir`, which made it write them to v3_program_dependency_file, or
// says why they are not known. `unit` is the C of the program, `search` where the
// compiler looks for an included file, and `before` a time, in seconds, from
// before the command started.
fn (mut inputs V3ProgramLinkInputs) add_compiled_headers(manager &modulecache.Manager, cc_dir string, unit string, search V3IncludeSearch, before i64) {
	if inputs.unknown.len > 0 {
		return
	}
	text := os.read_file(os.join_path_single(cc_dir, v3_program_dependency_file)) or {
		inputs.unknown = 'the compiler did not tell which headers it read'
		return
	}
	mut read := []string{}
	mut relative_paths := false
	for name in v3_parse_dependency_file(text) {
		path := if os.is_abs_path(name) { name } else { os.norm_path(os.join_path(cc_dir, name)) }
		// What is in the directory of the build is made by the build, and what the
		// linker reads is an input of the link.
		if !path.starts_with(cc_dir + '/') && !v3_path_is_link_input(path) && path !in read {
			read << path
			if !os.is_abs_path(name) {
				relative_paths = true
			}
		}
	}
	mut headers := v3_kept_header_inputs(manager, read, unit, search, before)
	if relative_paths {
		headers.relative_paths = true
	}
	inputs.add_header_inputs(&headers)
}

// compiler_tells_its_headers reports whether a compiler driver that compiles
// `sources` in one command writes down every header that it reads when it is
// asked to: it writes those of one source, so a command that compiles several is
// told by the last of them only. `inputs` gets the reason when it does not.
fn (mut inputs V3ProgramLinkInputs) compiler_tells_its_headers(sources []string) bool {
	if sources.len > 1 && inputs.unknown.len == 0 {
		inputs.unknown = 'the command compiles more than one source: ${sources.join(' ')}'
	}
	return sources.len == 1 && inputs.unknown.len == 0
}
