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
			if arg in v3_compiler_options_with_value
				|| arg in ['-L', '-l', '-F', '-framework', '-weak_framework', '-Xlinker', '-T',
					'-include', '-imacros', '-force_load'] {
				i++
			}
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

// V3IncludeSearch is where a C compiler looks for an included file: `dirs` in the
// order of the search, those for `#include "..."` before the others, and `absent`
// the directories that it was told to search and that are not there.
struct V3IncludeSearch {
mut:
	dirs   []string
	absent []string
}

// v3_parse_include_search reads what GCC and Clang print for `-E -v`: the
// directories between `#include "..." search starts here:` and
// `End of search list.`, and before them the ones that they leave out because
// they are not there. A directory of frameworks holds no header by its name.
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
			dir := line.trim_space()
			if dir.ends_with('(framework directory)') || dir.len == 0 {
				continue
			}
			if dir !in search.dirs {
				search.dirs << dir
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
		} else if arg.starts_with('-I') || arg.starts_with('-isystem') || arg.starts_with('-iquote')
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
	mut identity := ['v3-include-search-1', os.real_path(compiler), v3_cache_file_identity(compiler),
		('objective-c' in args).str()]
	identity << search_args
	for name in v3_link_environment_names {
		identity << '${name}=${os.getenv(name)}'
	}
	record := os.join_path_single(manager.dir, 'include_search_${c_hash_bytes(u64(1469598103934665603), identity.join('\n').bytes()).hex()}')
	if kept := os.read_file(record) {
		if search := v3_parse_include_search(kept) {
			if search.dirs.all(os.is_dir(it)) && search.absent.all(!os.exists(it)) {
				return v3_absolute_include_search(search, cc_dir)
			}
		}
	}
	mut query := search_args.clone()
	query << ['-x', if 'objective-c' in args { 'objective-c' } else { 'c' }, '-E', '-v',
		os.path_devnull]
	// The compiler says where it searches on its standard error.
	result := cmdexec.run_in_merged(compiler, query, cc_dir)
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
		dirs:   v3_tcc_absolute_include_dirs(search.dirs, cc_dir)
		absent: v3_tcc_absolute_include_dirs(search.absent, cc_dir)
	}
}

// add_headers adds the headers that the C compiler read for the program, and the
// places where it would have found another, to the inputs of the executable.
// `identities[i]` is the metadata of `files[i]`, which was that of the file when
// the compiler read it.
fn (mut inputs V3ProgramLinkInputs) add_headers(files []string, identities []string, missing []string) {
	for i, file in files {
		if file !in inputs.files {
			inputs.files << file
			inputs.identities << identities[i]
		}
	}
	for path in missing {
		if path !in inputs.missing {
			inputs.missing << path
		}
	}
}

// add_tcc_headers adds the headers of a program unit that TinyCC compiled, as
// v3_tcc_prelude found them before the compilation.
fn (mut inputs V3ProgramLinkInputs) add_tcc_headers(prelude &V3TccPrelude) {
	if !prelude.has_headers || inputs.unknown.len > 0 {
		return
	}
	if prelude.stamp.len == 0 {
		inputs.unknown = 'the headers of the program cannot be told apart from changed ones'
		return
	}
	mut files := []string{}
	mut identities := []string{}
	mut missing := []string{}
	for line in prelude.stamp.split_into_lines() {
		if line.starts_with('file=') {
			tab := line.last_index_u8(`\t`)
			if tab > 'file='.len {
				files << line['file='.len..tab]
				identities << line[tab + 1..]
			}
		} else if line.starts_with('missing=') {
			missing << line['missing='.len..]
		}
	}
	inputs.add_headers(files, identities, missing)
}

// add_compiler_headers adds the headers that the compiler driver `compiler` read
// when it ran the command `args` in `cc_dir`, which made it write them to
// v3_program_dependency_file, or says why they are not known. `unit` is the C of
// the program, and `before` a time, in seconds, from before the command started:
// the metadata of a header is taken after the compiler has read it, so one that
// was written or put in place at `before` or later may not be the one that was
// read.
fn (mut inputs V3ProgramLinkInputs) add_compiler_headers(manager &modulecache.Manager, compiler string, args []string, cc_dir string, unit string, before i64) {
	if inputs.unknown.len > 0 {
		return
	}
	sources := v3_compiled_sources(args)
	if sources.len == 0 {
		// The command links what earlier commands compiled.
		return
	}
	if sources.len > 1 {
		inputs.unknown = 'the command compiles more than one source: ${sources.join(' ')}'
		return
	}
	text := os.read_file(os.join_path_single(cc_dir, v3_program_dependency_file)) or {
		inputs.unknown = 'the compiler did not tell which headers it read'
		return
	}
	mut read := []string{}
	for name in v3_parse_dependency_file(text) {
		path := if os.is_abs_path(name) { name } else { os.norm_path(os.join_path(cc_dir, name)) }
		// What is in the directory of the build is made by the build.
		if !path.starts_with(cc_dir + '/') && path !in read {
			read << path
		}
	}
	search := v3_include_search(manager, compiler, args, cc_dir) or {
		inputs.unknown = 'the compiler did not tell where it looks for headers'
		return
	}
	headers := v3_header_inputs(read, unit, search.dirs, []string{})
	mut identities := []string{cap: headers.files.len}
	for path in headers.files {
		identity := modulecache.file_metadata_signature(path)
		attributes := os.stat(path) or {
			inputs.unknown = '`${path}` cannot be told apart from a changed file'
			return
		}
		if identity.len == 0 || attributes.mtime >= before || attributes.ctime >= before {
			inputs.unknown = '`${path}` cannot be told apart from a changed file'
			return
		}
		identities << identity
	}
	mut missing := headers.missing.clone()
	for dir in search.absent {
		if dir !in missing {
			missing << dir
		}
	}
	inputs.add_headers(headers.files, identities, missing)
}
