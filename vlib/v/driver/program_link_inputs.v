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
}

// V3LinkCommand is the part of a link command that names files and libraries.
struct V3LinkCommand {
mut:
	files          []string
	library_dirs   []string
	libraries      []string
	framework_dirs []string
	frameworks     []string
	unknown        string
}

// options_with_operand are the options of a C compiler driver whose next argument
// is a value that names no input of the link.
const v3_link_options_with_value = ['-o', '-x', '-arch', '-target', '-u', '-e', '-install_name',
	'-current_version', '-compatibility_version', '-MF', '-MT', '-MQ', '-D', '-U', '-Xclang',
	'-Xassembler', '-Xpreprocessor', '-mllvm', '-z', '-stack_size', '-rpath', '-I', '-isystem',
	'-iquote', '-idirafter', '-isysroot', '-syslibroot', '-B']

// add_operand records an argument that is no option. One that is the absolute
// path of a file is an input of the link, whatever its name ends with: an object,
// an archive, a library, a linker script, a list of symbols. A relative one is in
// the directory of the build, which the build made itself.
fn (mut c V3LinkCommand) add_operand(operand string) {
	if operand.len == 0 {
		return
	}
	if operand[0] == `@` {
		c.unknown = 'the arguments of `${operand}` are in a file'
		return
	}
	if os.is_abs_path(operand) && !os.is_dir(operand)
		&& (os.is_file(operand) || v3_path_is_link_input(operand)) {
		c.files << operand
	}
}

// add_linker_option records one option that goes to the linker as it is.
fn (mut c V3LinkCommand) add_linker_option(option string) {
	if option.starts_with('-L') && option.len > 2 {
		c.library_dirs << option[2..]
	} else if option.starts_with('-l') && option.len > 2 {
		c.libraries << option[2..]
	} else if option in ['-filelist', '--just-symbols', '-R'] {
		c.unknown = 'the linker option `${option}` reads files that are named elsewhere'
	} else if option.starts_with('-') {
		// `--version-script=/path`, `-force_load=/path`: the value may be a file.
		if option.contains('=') {
			c.add_operand(option.all_after('='))
		}
	} else {
		c.add_operand(option)
	}
}

// v3_parse_link_command reads the arguments of a command that compiles and links a
// program for what the link reads.
fn v3_parse_link_command(args []string) V3LinkCommand {
	mut command := V3LinkCommand{}
	mut i := 0
	for i < args.len {
		arg := args[i].trim_space()
		i++
		if arg.len == 0 {
			continue
		}
		if arg in ['-L', '-l', '-F', '-framework', '-weak_framework', '-Xlinker'] {
			if i >= args.len {
				continue
			}
			operand := args[i].trim_space()
			i++
			match arg {
				'-L' { command.library_dirs << operand }
				'-l' { command.libraries << operand }
				'-F' { command.framework_dirs << operand }
				'-Xlinker' { command.add_linker_option(operand) }
				else { command.frameworks << operand }
			}
			continue
		}
		if arg in v3_link_options_with_value {
			i++
			continue
		}
		if arg.starts_with('-Wl,') {
			for option in arg[4..].split(',') {
				command.add_linker_option(option)
			}
		} else if arg.starts_with('-L') {
			command.library_dirs << arg[2..].trim_space()
		} else if arg.starts_with('-weak-l') {
			command.libraries << arg['-weak-l'.len..]
		} else if arg.starts_with('-l') {
			command.libraries << arg[2..].trim_space()
		} else if arg.starts_with('-F') && arg.len > 2 {
			command.framework_dirs << arg[2..]
		} else if arg == '-filelist' {
			command.unknown = 'the option `-filelist` reads files that are named elsewhere'
		} else if arg.starts_with('-') {
			// `-fprofile-use=/path`, `--sysroot=/path`: only a file is an input.
			if arg.contains('=') {
				command.add_operand(arg.all_after('='))
			}
		} else {
			command.add_operand(arg)
		}
	}
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

// expand_link_file adds what the link reads through `path` to `command`. An archive
// or a library in a binary format is read as it is. A thin archive holds the names
// of its members, which are other files, and a linker script names the files and
// libraries to read in its place: the first cannot be followed here, the second can
// when it does nothing but name them.
fn (mut command V3LinkCommand) expand_link_file(path string) {
	if !(path.ends_with('.a') || path.ends_with('.so') || path.contains('.so.')) {
		return
	}
	start := v3_read_file_start(path, 4096)
	if start.len >= v3_thin_archive_magic.len
		&& start[..v3_thin_archive_magic.len].bytestr() == v3_thin_archive_magic {
		command.unknown = '`${path}` is a thin archive: its members are other files'
		return
	}
	if start.len == 0 || start.any(it == 0) || start.bytestr().starts_with(v3_archive_magic) {
		return
	}
	// Text where a library is expected is a linker script, as glibc has for libc.
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
fn v3_program_link_inputs(args []string, linker_dir string, default_library_dirs []string) V3ProgramLinkInputs {
	mut command := v3_parse_link_command(args)
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
	for framework in command.frameworks {
		for dir in command.framework_dirs {
			for name in [framework, '${framework}.tbd'] {
				command.files << os.join_path(dir, '${framework}.framework', name)
			}
		}
	}
	// A linker script adds files and libraries, which are read like the others.
	for _ in 0 .. 16 {
		mut library_dirs := command.library_dirs.clone()
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
			break
		}
	}
	mut inputs := V3ProgramLinkInputs{
		taken:   true
		files:   files.keys()
		missing: missing.keys()
		unknown: command.unknown
	}
	inputs.files.sort()
	inputs.missing.sort()
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
fn v3_default_link_library_dirs(manager &modulecache.Manager, linker string, base_args []string, sdk_root string) []string {
	mut candidates := []string{}
	for dir in os.getenv('LIBRARY_PATH').split(os.path_delimiter) {
		candidates << dir
	}
	record := os.join_path_single(manager.dir, 'link_library_dirs_${c_hash_bytes(u64(1469598103934665603), '${os.real_path(linker)}\n${v3_cache_file_identity(linker)}\n${base_args.join('\n')}'.bytes()).hex()}')
	reported := os.read_file(record) or {
		mut query := base_args.clone()
		query << '-print-search-dirs'
		result := cmdexec.run(linker, query)
		answer := if result.exit_code == 0 {
			v3_parse_library_search_dirs(result.output).join('\n') + '\n'
		} else {
			'\n'
		}
		if manager.ensure_dir() {
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
