module driver

import os
import strings
import time
import v.cmdexec
import v.modulecache
import v.tempname

// The C of a program starts with the headers of the C library and of the modules
// that it links. TinyCC has no precompiled headers and reads them for every build:
// megabytes for a program unit of a hundred kilobytes. A build that links cached
// module objects keeps that part of its unit in preprocessed form instead, with the
// macro definitions that the rest of the unit may use, and compiles the same C.
//
// The preprocessed form is the C that the preprocessor made of the headers,
// followed by every definition of a macro that it met. The C comes first, where
// none of those macros is defined: it is expanded already, and a macro that names
// itself in what it expands to would be expanded in it again. Whether a compiler
// that reads the form sees the same C in it is checked by preprocessing the form:
// it must come out as it went in. A prelude that does not, or whose headers ask
// for the moment or the place of a compilation, is compiled as it is.

const v3_tcc_prelude_format = 'v3-tcc-prelude-3'

// v3_tcc_prelude_input_name is the name under which the preprocessor reads a
// prelude: where it shows in what comes out, a header asked for the name of the
// file that is compiled.
const v3_tcc_prelude_input_name = 'v3_tcc_prelude_input.c'

// v3_tcc_prelude_counter_probe follows a prelude through the preprocessor to tell
// how many times its headers took a value of `__COUNTER__`.
const v3_tcc_prelude_counter_probe = '__v3_tcc_prelude_counter'

// v3_tcc_prelude_end returns the length of the part of `source` that holds every
// `#include` of it and ends outside any conditional group, or 0 when `source` has
// no such part to set aside: nothing before the end may leave a group open, and
// what follows it must not include a file.
fn v3_tcc_prelude_end(source string) int {
	mut depth := 0
	mut end := 0
	mut pending := false
	mut in_comment := false
	mut continued := false
	mut pos := 0
	for pos < source.len {
		mut line_end := source.index_after_('\n', pos)
		if line_end < 0 {
			line_end = source.len
		}
		line := source[pos..line_end]
		next := if line_end < source.len { line_end + 1 } else { source.len }
		was_continued := continued
		continued = line.len > 0 && line[line.len - 1] == `\\`
		if in_comment {
			if line.contains('*/') {
				in_comment = false
			}
			pos = next
			continue
		}
		if !was_continued {
			text := line.trim_left(' \t')
			if text.len > 0 && text[0] == `#` {
				directive := text[1..].trim_left(' \t')
				if directive.starts_with('if') {
					depth++
				} else if directive.starts_with('endif') {
					depth--
					if depth < 0 {
						return 0
					}
				} else if directive.starts_with('include') {
					pending = true
				}
			}
		}
		if comment := line.last_index('/*') {
			if !line[comment..].contains('*/') {
				in_comment = true
			}
		}
		if pending && depth == 0 && !continued {
			end = next
			pending = false
		}
		pos = next
	}
	if pending || depth != 0 || end >= source.len {
		return 0
	}
	return end
}

// v3_tcc_preprocess_args returns the arguments of a TinyCC compile-and-link
// command that decide how it preprocesses its source: everything but the output,
// the inputs and the options of the link.
fn v3_tcc_preprocess_args(tcc_args []string, source_name string) []string {
	mut args := []string{cap: tcc_args.len}
	mut i := 0
	for i < tcc_args.len {
		arg := tcc_args[i]
		clean := arg.trim_space()
		i++
		if clean in ['-o', '-framework', '-rpath', '-L', '-l'] {
			i++
			continue
		}
		if clean == source_name || clean in ['-c', '-shared', '-rdynamic', '-static']
			|| clean.starts_with('-l') || clean.starts_with('-L') || clean.starts_with('-Wl,')
			|| clean.starts_with('-bt') || (!clean.starts_with('-') && v3_path_is_link_input(clean)) {
			continue
		}
		args << arg
	}
	return args
}

// v3_tcc_include_dirs returns the directories that `args` make TinyCC search for
// an included file: those of `-I`, which it searches first and in the order they
// are given, and the others, its own headers among them, which come after them.
fn v3_tcc_include_dirs(args []string) ([]string, []string) {
	mut first := []string{}
	mut later := []string{}
	mut i := 0
	for i < args.len {
		clean := args[i].trim_space()
		i++
		if clean == '-I' || clean == '-isystem' {
			if i < args.len {
				dir := args[i].trim_space()
				i++
				if clean == '-I' && dir !in first {
					first << dir
				} else if clean == '-isystem' && dir !in later {
					later << dir
				}
			}
		} else if clean.starts_with('-isystem') {
			dir := clean['-isystem'.len..].trim_space()
			if dir !in later {
				later << dir
			}
		} else if clean.starts_with('-I') {
			dir := clean[2..].trim_space()
			if dir.len > 0 && dir !in first {
				first << dir
			}
		} else if clean.starts_with('-B') {
			dir := os.join_path_single(clean[2..].trim_space(), 'include')
			if dir !in later {
				later << dir
			}
		}
	}
	return first, later.filter(it !in first)
}

// v3_tcc_default_include_dirs returns the absolute directories that TinyCC searches
// for an included file without being told to. Those that it gives relative to the
// directory it runs in are below the directory of one build, where nothing is.
fn v3_tcc_default_include_dirs(tcc_path string, tcc_args []string, cc_dir string) []string {
	mut args := tcc_args.filter(it.trim_space().starts_with('-B'))
	args << '-print-search-dirs'
	result := cmdexec.run_in(tcc_path, args, cc_dir)
	mut dirs := []string{}
	if result.exit_code != 0 {
		return dirs
	}
	mut in_include := false
	for line in result.output.split_into_lines() {
		if !line.starts_with(' ') {
			in_include = line.trim_space() == 'include:'
			continue
		}
		dir := line.trim_space()
		if in_include && os.is_abs_path(dir) && dir !in dirs {
			dirs << dir
		}
	}
	return dirs
}

// v3_tcc_source_has_build_time_macros reports whether `source` spells a macro whose
// value is that of the moment or of the place where it is expanded.
fn v3_tcc_source_has_build_time_macros(source string) bool {
	return source.contains('__DATE__') || source.contains('__TIME__')
		|| source.contains('__TIMESTAMP__') || source.contains('__COUNTER__')
}

// v3_first_missing_path returns the shortest prefix of `path` below `root` that
// does not exist, or '' when `path` exists. Where a file is not, one directory on
// the way to it is the first that is not, and nothing below it can appear while
// that one stays absent.
fn v3_first_missing_path(root string, relative string) string {
	if !os.exists(root) {
		return root
	}
	mut current := root
	for part in relative.split('/') {
		if part.len == 0 {
			continue
		}
		current = os.join_path_single(current, part)
		if !os.exists(current) {
			return current
		}
	}
	return ''
}

// v3_has_include_names returns the names that `source` asks `__has_include` and
// `__has_include_next` about.
fn v3_has_include_names(source string) []string {
	mut names := []string{}
	mut pos := 0
	for {
		found := source.index_after_('__has_include', pos)
		if found < 0 {
			break
		}
		pos = found + '__has_include'.len
		mut open := pos
		if source[open..].starts_with('_next') {
			open += '_next'.len
		}
		for open < source.len && source[open] in [` `, `\t`] {
			open++
		}
		if open >= source.len || source[open] != `(` {
			continue
		}
		open++
		for open < source.len && source[open] in [` `, `\t`] {
			open++
		}
		if open >= source.len || source[open] !in [`<`, `"`] {
			continue
		}
		close := if source[open] == `<` { `>` } else { `"` }
		mut end := open + 1
		for end < source.len && source[end] != close && source[end] != `\n` {
			end++
		}
		if end < source.len && source[end] == close && end > open + 1 {
			name := source[open + 1..end]
			if name !in names {
				names << name
			}
		}
	}
	return names
}

// V3TccPreludeInputs is what the preprocessed form of a prelude depends on besides
// its text and the command: the files that the preprocessor read, and the paths
// where a file that is not there would be found instead of, or in addition to, one
// of them.
struct V3TccPreludeInputs {
mut:
	files   []string
	missing []string
	// A file that the build made for itself and that the prelude includes: it is
	// another one in the next build, under the same name.
	of_the_build string
}

// v3_preprocessed_files returns the files that the line markers of preprocessed C
// name, which are all that the preprocessor read, and one of them that the build
// made for itself in `build_dir`, if there is one.
fn v3_preprocessed_files(preprocessed string, build_dir string) ([]string, string) {
	mut files := map[string]bool{}
	mut of_the_build := ''
	mut pos := 0
	for pos < preprocessed.len {
		mut line_end := preprocessed.index_after_('\n', pos)
		if line_end < 0 {
			line_end = preprocessed.len
		}
		if line_end - pos > 4 && preprocessed[pos] == `#` && preprocessed[pos + 1] == ` `
			&& preprocessed[pos + 2].is_digit() {
			line := preprocessed[pos..line_end]
			open := line.index_u8(`"`)
			close := line.last_index_u8(`"`)
			if open > 0 && close > open + 1 {
				mut path := line[open + 1..close]
				// The preprocessor names a file that it found through a relative
				// directory relative to the directory that it runs in.
				if !os.is_abs_path(path) && build_dir.len > 0 && path != v3_tcc_prelude_input_name
					&& !path.starts_with('<') {
					path = os.norm_path(os.join_path(build_dir, path))
				}
				if build_dir.len > 0 && path.starts_with(build_dir + '/') {
					of_the_build = path
				} else if os.is_abs_path(path) {
					files[path] = true
				}
			}
		}
		pos = line_end + 1
	}
	return files.keys(), of_the_build
}

// v3_tcc_prelude_inputs works the inputs out from the output of the preprocessor,
// whose line markers name every file that it read: see v3_header_inputs.
fn v3_tcc_prelude_inputs(preprocessed string, prelude string, first_dirs []string, later_dirs []string, build_dir string) V3TccPreludeInputs {
	read, of_the_build := v3_preprocessed_files(preprocessed, build_dir)
	mut inputs := v3_header_inputs(read, prelude, first_dirs, later_dirs)
	inputs.of_the_build = of_the_build
	return inputs
}

// v3_header_inputs returns what the C of a unit depends on besides its text and
// the command that compiles it, given the files that the compiler `read` for it.
// `first_dirs` are the include directories that are searched first and in that
// order, `later_dirs` those that come after them, in an order that is not known
// here. A file of the name of one that was read would be found instead of it in
// any directory that is searched before the one it is in: for each such directory,
// the path where it is not is an input. So is each directory for a name that
// `unit` or one of the files asks `__has_include` about.
fn v3_header_inputs(read []string, unit string, first_dirs []string, later_dirs []string) V3TccPreludeInputs {
	mut include_dirs := first_dirs.clone()
	include_dirs << later_dirs
	mut files := map[string]bool{}
	for path in read {
		files[path] = true
	}
	mut missing := map[string]bool{}
	mut asked := v3_has_include_names(unit)
	for path in read {
		text := os.read_file(path) or { '' }
		if text.contains('__has_include') {
			for name in v3_has_include_names(text) {
				if name !in asked {
					asked << name
				}
			}
		}
		for dir in include_dirs {
			if !path.starts_with(dir + '/') {
				continue
			}
			relative := path[dir.len + 1..]
			position := first_dirs.index(dir)
			for other_position, other in include_dirs {
				// A directory of `-I` is searched before those given after it, and
				// before every directory that is no `-I` one.
				if other == dir || (position >= 0 && other_position > position) {
					continue
				}
				absent := v3_first_missing_path(other, relative)
				if absent.len > 0 {
					missing[absent] = true
				}
			}
		}
	}
	for name in asked {
		for dir in include_dirs {
			absent := v3_first_missing_path(dir, name)
			if absent.len > 0 {
				missing[absent] = true
			} else {
				files[os.join_path_single(dir, name)] = true
			}
		}
	}
	mut inputs := V3TccPreludeInputs{
		files:   files.keys()
		missing: missing.keys()
	}
	inputs.files.sort()
	inputs.missing.sort()
	return inputs
}

// v3_tcc_environment_include_dirs returns the directories of CPATH, which TinyCC
// searches after those of `-I` and before every other, in the order they are
// given. It does not tell them when it is asked where it searches.
fn v3_tcc_environment_include_dirs() []string {
	mut dirs := []string{}
	for dir in os.getenv('CPATH').split(os.path_delimiter) {
		if dir.len > 0 && dir !in dirs {
			dirs << dir
		}
	}
	return dirs
}

// v3_tcc_absolute_include_dirs returns `dirs` as TinyCC reads them when it runs in
// `build_dir`: a relative one is below that directory.
fn v3_tcc_absolute_include_dirs(dirs []string, build_dir string) []string {
	mut absolute := []string{cap: dirs.len}
	for dir in dirs {
		path := if os.is_abs_path(dir) { dir } else { os.norm_path(os.join_path(build_dir, dir)) }
		if path !in absolute {
			absolute << path
		}
	}
	return absolute
}

// V3TccPreprocessed is what a preprocessor made of a prelude: the C, and the
// definitions of macros in the order it met them. `unusable` says why the two
// cannot stand for the prelude, or is '' when they can.
struct V3TccPreprocessed {
mut:
	code        string
	definitions string
	unusable    string
}

// text returns the form in which a unit includes the preprocessed prelude.
fn (p &V3TccPreprocessed) text() string {
	return p.code + p.definitions
}

// v3_preprocessor_marker_file returns the file that `line` names when it is a line
// marker of preprocessed C, `# <line> "<file>"`.
fn v3_preprocessor_marker_file(line string) ?string {
	if line.len < 3 || line[0] != `#` || line[1] != ` ` || !line[2].is_digit() {
		return none
	}
	open := line.index_u8(`"`)
	close := line.last_index_u8(`"`)
	if open < 0 || close <= open {
		return ''
	}
	return line[open + 1..close]
}

// v3_tcc_split_preprocessed takes the output of `tcc -E -dD` apart. The macros
// that the compiler defines before it reads anything, which it prints as those of
// a file `<command line>`, are left out: it defines them again for the unit that
// includes the form. The line markers are left out as well: they are a tenth of
// the output, and what they say is of no use to a unit that exists for one build.
fn v3_tcc_split_preprocessed(preprocessed string) V3TccPreprocessed {
	mut code := strings.new_builder(preprocessed.len)
	mut definitions := strings.new_builder(preprocessed.len / 4)
	mut result := V3TccPreprocessed{}
	mut in_command_line := false
	mut pos := 0
	for pos < preprocessed.len {
		mut line_end := preprocessed.index_after_('\n', pos)
		if line_end < 0 {
			line_end = preprocessed.len
		}
		line := preprocessed[pos..line_end]
		pos = line_end + 1
		if file := v3_preprocessor_marker_file(line) {
			in_command_line = file == '<command line>'
			continue
		}
		if in_command_line {
			continue
		}
		text := line.trim_left(' \t')
		if text.len == 0 {
			continue
		}
		if text[0] != `#` {
			if text.starts_with(v3_tcc_prelude_counter_probe) {
				if text[v3_tcc_prelude_counter_probe.len..].trim_space() != '0' {
					result.unusable = 'its headers count with __COUNTER__'
				}
				continue
			}
			code.write_string(line)
			code.write_u8(`\n`)
			continue
		}
		directive := text[1..].trim_left(' \t')
		if directive.starts_with('define') || directive.starts_with('undef') {
			definitions.write_string(text)
			definitions.write_u8(`\n`)
		} else if directive.starts_with('pragma') && !directive.contains('push_macro')
			&& !directive.contains('pop_macro') {
			code.write_string(line)
			code.write_u8(`\n`)
		} else {
			result.unusable = 'a directive that cannot be moved: `${text}`'
		}
	}
	result.code = code.str()
	result.definitions = definitions.str()
	if result.unusable.len == 0 && result.code.contains('"${v3_tcc_prelude_input_name}"') {
		result.unusable = 'its headers ask for the name of the file that is compiled'
	}
	if result.unusable.len == 0 && v3_c_has_time_literal(result.code) {
		result.unusable = 'its headers ask for the time of the compilation'
	}
	return result
}

// v3_c_has_time_literal reports whether `code` has a string literal that looks
// like what `__DATE__`, `__TIME__` or `__TIMESTAMP__` expand to: `"Jan  1 2026"`,
// or one with `12:34:56` in it.
fn v3_c_has_time_literal(code string) bool {
	mut i := 0
	for i < code.len {
		if code[i] == `'` {
			// A character literal can hold a quotation mark.
			i++
			for i < code.len && code[i] != `'` && code[i] != `\n` {
				i += if code[i] == `\\` { 2 } else { 1 }
			}
			i++
			continue
		}
		if code[i] != `"` {
			i++
			continue
		}
		start := i + 1
		i++
		for i < code.len && code[i] != `"` && code[i] != `\n` {
			i += if code[i] == `\\` { 2 } else { 1 }
		}
		if i > code.len {
			i = code.len
		}
		literal := code[start..i]
		i++
		if literal.len == 11 && literal[0].is_capital() && literal[1].is_letter()
			&& literal[2].is_letter() && literal[3] == ` ` && literal[6] == ` `
			&& literal[5].is_digit() && literal[7..].bytes().all(it.is_digit()) {
			return true
		}
		for j := 0; j + 8 <= literal.len; j++ {
			if literal[j].is_digit() && literal[j + 1].is_digit() && literal[j + 2] == `:`
				&& literal[j + 3].is_digit() && literal[j + 4].is_digit() && literal[j + 5] == `:`
				&& literal[j + 6].is_digit() && literal[j + 7].is_digit() {
				return true
			}
		}
	}
	return false
}

// v3_c_tokens_text returns `text` with every run of white space outside a string
// or character literal as one space, and none at its start: two texts that a
// preprocessor printed hold the same tokens when this is the same for both.
fn v3_c_tokens_text(text string) string {
	mut out := strings.new_builder(text.len)
	mut pending_space := false
	mut quote := u8(0)
	mut i := 0
	for i < text.len {
		c := text[i]
		if quote != 0 {
			out.write_u8(c)
			if c == `\\` && i + 1 < text.len {
				out.write_u8(text[i + 1])
				i += 2
				continue
			}
			if c == quote || c == `\n` {
				quote = 0
			}
			i++
			continue
		}
		if c in [` `, `\t`, `\r`, `\n`, `\f`, `\v`] {
			pending_space = true
			i++
			continue
		}
		if pending_space && out.len > 0 {
			out.write_u8(` `)
		}
		pending_space = false
		if c in [`"`, `'`] {
			quote = c
		}
		out.write_u8(c)
		i++
	}
	return out.str()
}

// v3_tcc_preprocessed_forms_match reports whether `second`, what the preprocessor
// made of the form of `first`, holds the same C and the same definitions. The C is
// compared as tokens; a definition is a line, and is compared as one.
fn v3_tcc_preprocessed_forms_match(first &V3TccPreprocessed, second &V3TccPreprocessed) bool {
	if v3_c_tokens_text(first.code) != v3_c_tokens_text(second.code) {
		return false
	}
	first_definitions := first.definitions.split_into_lines()
	second_definitions := second.definitions.split_into_lines()
	if first_definitions.len != second_definitions.len {
		return false
	}
	for i, definition in first_definitions {
		if v3_c_tokens_text(definition) != v3_c_tokens_text(second_definitions[i]) {
			return false
		}
	}
	return true
}

// v3_tcc_prelude_stamp records the inputs of a preprocessed prelude, and in
// `unusable` why the preprocessed form cannot stand for it, when it cannot: that
// holds as long as the inputs stay what they are, and a build need not find it out
// again. `before` is a
// time, in seconds, from before the preprocessor started. The files that it read
// are known only from what it printed, so their metadata is taken after it has
// read them: a file that was written or put in place at `before` or later may not
// be the one that was read, and nothing is recorded then.
fn v3_tcc_prelude_stamp(key string, inputs V3TccPreludeInputs, before i64, unusable string) ?string {
	mut out := strings.new_builder(128 + inputs.files.len * 128 + inputs.missing.len * 96)
	out.writeln('format=${v3_tcc_prelude_format}')
	out.writeln('key=${key}')
	if unusable.len > 0 {
		out.writeln('unusable=${unusable.replace('\n', ' ')}')
	}
	for path in inputs.files {
		metadata := modulecache.file_metadata_signature(path)
		if metadata.len == 0 || path.contains_any('\t\n') {
			return none
		}
		attributes := os.stat(path) or { return none }
		if attributes.mtime >= before || attributes.ctime >= before {
			return none
		}
		out.writeln('file=${path}\t${metadata}')
	}
	for path in inputs.missing {
		if path.contains_any('\n') {
			return none
		}
		out.writeln('missing=${path}')
	}
	out.writeln('complete=1')
	return out.str()
}

fn v3_tcc_prelude_stamp_is_valid(stamp string, key string) bool {
	lines := stamp.split_into_lines()
	if lines.len < 3 || lines[0] != 'format=${v3_tcc_prelude_format}' || lines[1] != 'key=${key}'
		|| lines.last() != 'complete=1' {
		return false
	}
	for line in lines[2..lines.len - 1] {
		if line.starts_with('unusable=') {
			continue
		}
		if line.starts_with('file=') {
			tab := line.last_index_u8(`\t`)
			if tab <= 'file='.len {
				return false
			}
			if modulecache.file_metadata_signature(line['file='.len..tab]) != line[tab + 1..] {
				v3_trace_tcc_prelude('a header changed: ${line['file='.len..tab]}')
				return false
			}
		} else if line.starts_with('missing=') {
			if os.exists(line['missing='.len..]) {
				v3_trace_tcc_prelude('a header appeared: ${line['missing='.len..]}')
				return false
			}
		} else {
			return false
		}
	}
	return true
}

// v3_tcc_prelude_stamp_unusable returns why the prelude of `stamp` is compiled as it
// is, or '' when its preprocessed form is used.
fn v3_tcc_prelude_stamp_unusable(stamp string) string {
	lines := stamp.split_into_lines()
	if lines.len > 2 && lines[2].starts_with('unusable=') {
		return lines[2]['unusable='.len..]
	}
	return ''
}

fn v3_trace_tcc_prelude(message string) {
	if os.getenv('V3_CACHE_TRACE') != '' {
		eprintln('  V3 TinyCC prelude: ${message}')
	}
}

// v3_tcc_prelude_key identifies the preprocessed form of `prelude` for one TinyCC
// and one set of arguments. TinyCC also takes include directories from the
// environment.
fn v3_tcc_prelude_key(prelude string, tcc_path string, preprocess_args []string) string {
	mut hash := u64(1469598103934665603)
	for part in [v3_tcc_prelude_format, os.real_path(tcc_path), v3_cache_file_identity(tcc_path),
		preprocess_args.join('\x00'), os.getenv('CPATH'), os.getenv('C_INCLUDE_PATH'), prelude] {
		hash = c_hash_bytes(hash, part.bytes())
		hash = c_hash_bytes(hash, [u8(0xff)])
	}
	return '${hash.hex()}_${prelude.len}'
}

// V3TccPrelude is what a build knows of the headers of a program unit.
struct V3TccPrelude {
	// The unit with the part that includes its headers replaced by an `#include` of
	// that part in preprocessed form, or '' when the unit is compiled as it is.
	unit string
	// Whether the unit includes a file at all.
	has_headers bool
	// The record of the files that the headers of the unit are and of the places
	// where another would be found: see v3_tcc_prelude_stamp. It is '' when the
	// headers cannot be told from changed ones.
	stamp string
}

// v3_tcc_source_with_cached_prelude returns `source` with the part that includes
// its headers replaced by an `#include` of that part in preprocessed form, which
// it takes from the module cache or puts there. It returns none when `source` has
// no such part, or when the preprocessed form cannot be made, kept or used: the
// build then compiles `source` as it is.
fn v3_tcc_source_with_cached_prelude(manager &modulecache.Manager, source string, tcc_path string, tcc_args []string, source_name string, cc_dir string) ?string {
	prelude := v3_tcc_prelude(manager, source, tcc_path, tcc_args, source_name, cc_dir)
	if prelude.unit.len == 0 {
		return none
	}
	return prelude.unit
}

// v3_tcc_prelude finds out what the headers of the program unit `source` are for
// TinyCC, and returns the unit in the form that reads them preprocessed when that
// form can be used. What it finds is in the module cache for the builds that
// follow, as long as the headers stay what they are.
fn v3_tcc_prelude(manager &modulecache.Manager, source string, tcc_path string, tcc_args []string, source_name string, cc_dir string) V3TccPrelude {
	if !source.contains('#include') {
		return V3TccPrelude{}
	}
	end := v3_tcc_prelude_end(source)
	if end == 0 || !manager.enabled {
		return V3TccPrelude{
			has_headers: true
		}
	}
	unit, stamp := v3_tcc_prelude_of(manager, source, end, tcc_path, tcc_args, source_name, cc_dir)
	return V3TccPrelude{
		unit:        unit
		has_headers: true
		stamp:       stamp
	}
}

// v3_tcc_prelude_of returns the two results of v3_tcc_prelude for a unit whose
// headers end at `end`.
fn v3_tcc_prelude_of(manager &modulecache.Manager, source string, end int, tcc_path string, tcc_args []string, source_name string, cc_dir string) (string, string) {
	prelude := source[..end]
	preprocess_args := v3_tcc_preprocess_args(tcc_args, source_name)
	key := v3_tcc_prelude_key(prelude, tcc_path, preprocess_args)
	cached := os.join_path(manager.dir, 'tcc_prelude_${key}.i')
	stamp_path := cached + '.stamp'
	if stamp := os.read_file(stamp_path) {
		if v3_tcc_prelude_stamp_is_valid(stamp, key) {
			unusable := v3_tcc_prelude_stamp_unusable(stamp)
			if unusable.len > 0 {
				v3_trace_tcc_prelude('not used: ${unusable}')
				return '', stamp
			}
			if os.is_file(cached) {
				return '#include "${c_include_path(cached)}"\n' + source[end..], stamp
			}
		}
	}
	if !manager.ensure_dir() {
		return '', ''
	}
	prelude_file := os.join_path_single(cc_dir, v3_tcc_prelude_input_name)
	output_file := os.join_path_single(cc_dir, 'v3_tcc_prelude_output.i')
	verify_file := os.join_path_single(cc_dir, 'v3_tcc_prelude_verify.c')
	tmp := '${cached}.tmp.${tempname.unique_token()}'
	defer {
		os.rm(prelude_file) or {}
		os.rm(output_file) or {}
		os.rm(verify_file) or {}
		os.rm(tmp) or {}
	}
	// The line after the prelude tells how far its headers counted.
	os.write_file(prelude_file, '${prelude}\n${v3_tcc_prelude_counter_probe} __COUNTER__\n') or {
		return '', ''
	}
	// Whole seconds, and one to spare for a file system that rounds them.
	before_preprocessing := time.utc().unix() - 1
	mut args := preprocess_args.clone()
	args << ['-E', '-dD', '-o', os.file_name(output_file), v3_tcc_prelude_input_name]
	result := cmdexec.run_in(tcc_path, args, cc_dir)
	if result.exit_code != 0 {
		v3_trace_tcc_prelude('not preprocessed: ${result.output.all_before('\n')}')
		return '', ''
	}
	preprocessed := os.read_file(output_file) or { return '', '' }
	mut first_dirs, mut later_dirs := v3_tcc_include_dirs(preprocess_args)
	for dir in v3_tcc_environment_include_dirs() {
		if dir !in first_dirs {
			first_dirs << dir
		}
	}
	for dir in v3_tcc_default_include_dirs(tcc_path, preprocess_args, cc_dir) {
		if dir !in later_dirs {
			later_dirs << dir
		}
	}
	first_dirs = v3_tcc_absolute_include_dirs(first_dirs, cc_dir)
	later_dirs = v3_tcc_absolute_include_dirs(later_dirs, cc_dir).filter(it !in first_dirs)
	inputs := v3_tcc_prelude_inputs(preprocessed, prelude, first_dirs, later_dirs, cc_dir)
	mut form := v3_tcc_split_preprocessed(preprocessed)
	if form.unusable.len == 0 && v3_tcc_source_has_build_time_macros(prelude) {
		form.unusable = 'it asks for the time or the count of the compilation'
	}
	if form.unusable.len == 0 && inputs.of_the_build.len > 0 {
		form.unusable = 'it includes a file that the build made: ${os.file_name(inputs.of_the_build)}'
	}
	if form.unusable.len == 0 {
		// A compiler that reads the form must see the C that the preprocessor made
		// of the prelude: so it does when the form comes out of the preprocessor as
		// it went in.
		os.write_file(tmp, form.text()) or { return '', '' }
		os.write_file(verify_file, '#include "${c_include_path(tmp)}"\n') or { return '', '' }
		mut verify_args := preprocess_args.clone()
		verify_args << ['-E', '-dD', '-o', os.file_name(output_file), os.file_name(verify_file)]
		verified := cmdexec.run_in(tcc_path, verify_args, cc_dir)
		again := if verified.exit_code == 0 { os.read_file(output_file) or { '' } } else { '' }
		if again.len == 0 {
			form.unusable = 'its preprocessed form cannot be preprocessed'
		} else if !v3_tcc_preprocessed_forms_match(&form, v3_tcc_split_preprocessed(again)) {
			form.unusable = 'its macros change the C that they expanded to'
		}
	}
	stamp := v3_tcc_prelude_stamp(key, inputs, before_preprocessing, form.unusable) or {
		v3_trace_tcc_prelude('not kept: a header cannot be told apart from a changed one')
		return '', ''
	}
	// The stamp commits the text: remove the old one before the text is replaced.
	os.rm(stamp_path) or {}
	if form.unusable.len == 0 {
		os.mv(tmp, cached) or { return '', '' }
	} else {
		os.rm(cached) or {}
	}
	stamp_tmp := '${stamp_path}.tmp.${tempname.unique_token()}'
	os.write_file(stamp_tmp, stamp) or {
		os.rm(stamp_tmp) or {}
		return '', ''
	}
	os.mv(stamp_tmp, stamp_path) or {
		os.rm(stamp_tmp) or {}
		return '', ''
	}
	if form.unusable.len > 0 {
		v3_trace_tcc_prelude('not used: ${form.unusable}')
		return '', stamp
	}
	v3_trace_tcc_prelude('preprocessed ${prelude.len} bytes into ${cached}')
	return '#include "${c_include_path(cached)}"\n' + source[end..], stamp
}

// v3_tcc_units_compile_alike reports whether TinyCC makes the same object of two
// forms of a program unit. The check costs two more compilations, and is there for
// tests and for looking into a difference: V3_TCC_PRELUDE_VERIFY=1 asks for it.
fn v3_tcc_units_compile_alike(tcc_path string, tcc_args []string, source_name string, cc_dir string, first string, second string) bool {
	mut args := v3_tcc_preprocess_args(tcc_args, source_name)
	args << ['-c', '-o', 'verify.o', 'verify.c']
	source := os.join_path_single(cc_dir, 'verify.c')
	object := os.join_path_single(cc_dir, 'verify.o')
	defer {
		os.rm(source) or {}
		os.rm(object) or {}
	}
	mut objects := [][]u8{}
	for unit in [first, second] {
		os.write_file(source, unit) or { return false }
		os.rm(object) or {}
		if cmdexec.run_in(tcc_path, args, cc_dir).exit_code != 0 {
			return false
		}
		objects << os.read_bytes(object) or { return false }
	}
	return objects[0] == objects[1]
}
