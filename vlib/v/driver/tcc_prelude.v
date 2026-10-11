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
// itself in what it expands to would be expanded in it again. The rest of the
// unit follows it under the numbers that its lines had.
//
// A form stands for its prelude when a compiler that reads it sees what it would
// see in the prelude. That is checked when the form is made, in three ways:
//
// - The form is preprocessed, and must come out as it went in.
// - What `tcc -E -dD` does not tell is taken from the text of the headers, or the
//   form is not used: see tcc_prelude_macros.v.
// - The unit that asked for the form is compiled with it and without it, and
//   both must give the same object.
//
// A prelude whose form fails one of them, or whose headers ask for the moment or
// the place of a compilation, is compiled as it is. So is, from then on, one with
// whose form a unit does not compile that compiles without it.

const v3_tcc_prelude_format = 'v3-tcc-prelude-5'

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
// an included file, each kind in the order it is given: those of `-I`, which it
// searches first, those of `-isystem`, which come after the ones of the
// environment, and its own headers, which come after all of these.
fn v3_tcc_include_dirs(args []string) ([]string, []string, []string) {
	mut first := []string{}
	mut system := []string{}
	mut own := []string{}
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
				} else if clean == '-isystem' && dir !in system {
					system << dir
				}
			}
		} else if clean.starts_with('-isystem') {
			dir := clean['-isystem'.len..].trim_space()
			if dir !in system {
				system << dir
			}
		} else if clean.starts_with('-I') {
			dir := clean[2..].trim_space()
			if dir.len > 0 && dir !in first {
				first << dir
			}
		} else if clean.starts_with('-B') {
			dir := os.join_path_single(clean[2..].trim_space(), 'include')
			if dir !in own {
				own << dir
			}
		}
	}
	return first, system, own
}

// v3_tcc_default_include_dirs returns the absolute directories that TinyCC searches
// for an included file without being told to. Those that it gives relative to the
// directory it runs in are below the directory of one build, where nothing is.
// What it answers follows from the compiler, its `-B` and C_INCLUDE_PATH: the
// answer is kept in the module cache for those.
fn v3_tcc_default_include_dirs(manager &modulecache.Manager, tcc_path string, tcc_args []string, cc_dir string) []string {
	mut args := tcc_args.filter(it.trim_space().starts_with('-B'))
	record := os.join_path_single(manager.dir, 'tcc_include_dirs_${c_hash_bytes(u64(1469598103934665603), [
		'v3-tcc-include-dirs-1',
		os.real_path(tcc_path),
		v3_cache_file_identity(tcc_path),
		args.join('\n'),
		os.getenv('C_INCLUDE_PATH'),
	].join('\x00').bytes()).hex()}')
	if manager.enabled {
		if kept := os.read_file(record) {
			if kept.ends_with('complete=1\n') {
				return kept.split_into_lines().filter(it != 'complete=1')
			}
		}
	}
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
	if manager.enabled && manager.ensure_dir() {
		mut lines := dirs.clone()
		lines << 'complete=1'
		os.write_file(record, lines.join('\n') + '\n') or {}
	}
	return dirs
}

// v3_tcc_source_has_build_time_macros reports whether `source` spells a macro whose
// value is that of the moment or of the place where it is expanded.
fn v3_tcc_source_has_build_time_macros(source string) bool {
	return v3_c_mentions_compile_time(source) || source.contains('__COUNTER__')
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

// v3_preprocessed_files returns the files that the line markers of preprocessed C
// name, which are all that the preprocessor read, one of them that the build made
// for itself in `build_dir`, if there is one, and whether one of them is named
// relative to that directory and is not in it.
fn v3_preprocessed_files(preprocessed string, build_dir string) ([]string, string, bool) {
	mut files := map[string]bool{}
	mut of_the_build := ''
	mut relative := false
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
				was_relative := !os.is_abs_path(path)
				if was_relative && build_dir.len > 0 && path != v3_tcc_prelude_input_name
					&& !path.starts_with('<') {
					path = os.norm_path(os.join_path(build_dir, path))
				}
				if build_dir.len > 0 && path.starts_with(build_dir + '/') {
					of_the_build = path
				} else if os.is_abs_path(path) {
					files[path] = true
					if was_relative {
						relative = true
					}
				}
			}
		}
		pos = line_end + 1
	}
	return files.keys(), of_the_build, relative
}

// v3_environment_dirs returns the directories that the variable `name` of the
// environment lists, as CPATH and C_INCLUDE_PATH do for a C compiler.
fn v3_environment_dirs(name string) []string {
	mut dirs := []string{}
	for dir in os.getenv(name).split(os.path_delimiter) {
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

// v3_tcc_include_search returns where TinyCC looks for an included file when it
// runs with `args` in `cc_dir`, in the order in which it searches: the directories
// of `-I`, those of CPATH, those of `-isystem`, those of C_INCLUDE_PATH, its own
// headers, and the ones of the system, which it tells when it is asked.
fn v3_tcc_include_search(manager &modulecache.Manager, tcc_path string, args []string, cc_dir string) V3IncludeSearch {
	mut dirs, system_dirs, own_dirs := v3_tcc_include_dirs(args)
	dirs << v3_environment_dirs('CPATH')
	dirs << system_dirs
	dirs << v3_environment_dirs('C_INCLUDE_PATH')
	dirs << own_dirs
	dirs << v3_tcc_default_include_dirs(manager, tcc_path, args, cc_dir)
	return V3IncludeSearch{
		dirs: v3_tcc_absolute_include_dirs(dirs, cc_dir)
	}
}

// V3TccPreprocessed is what a preprocessor made of a prelude: the C, and the
// definitions of macros in the order it met them. `unusable` says why the two
// cannot stand for the prelude, or is '' when they can.
struct V3TccPreprocessed {
mut:
	code        string
	definitions string
	unusable    string
	// The numbers that follow v3_tcc_prelude_defined_probe in the output.
	defined []int
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
			if text.starts_with(v3_tcc_prelude_defined_probe) {
				result.defined << text[v3_tcc_prelude_defined_probe.len..].trim_space().int()
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

// v3_c_has_time_literal reports whether `code` has a string literal with what
// `__DATE__`, `__TIME__` or `__TIMESTAMP__` expand to in it: a date such as
// `Jan  1 2026`, or a time of day such as `12:34:56`.
fn v3_c_has_time_literal(code string) bool {
	mut i := 0
	for i < code.len {
		c := code[i]
		if c == `'` {
			// A character literal can hold a quotation mark.
			i = v3_c_skip_literal(code, i)
			continue
		}
		if c.is_digit() {
			// A number can hold a `'`.
			i = v3_c_skip_number(code, i)
			continue
		}
		if c != `"` {
			i++
			continue
		}
		end := v3_c_skip_literal(code, i)
		literal := code[i + 1..end]
		i = end
		for j := 0; j + 8 <= literal.len; j++ {
			if literal[j].is_digit() && literal[j + 1].is_digit() && literal[j + 2] == `:`
				&& literal[j + 3].is_digit() && literal[j + 4].is_digit() && literal[j + 5] == `:`
				&& literal[j + 6].is_digit() && literal[j + 7].is_digit() {
				return true
			}
		}
		for j := 0; j + 11 <= literal.len; j++ {
			if literal[j].is_capital() && literal[j + 1].is_letter() && literal[j + 2].is_letter()
				&& literal[j + 3] == ` ` && (literal[j + 4] == ` ` || literal[j + 4].is_digit())
				&& literal[j + 5].is_digit() && literal[j + 6] == ` ` && literal[j + 7].is_digit()
				&& literal[j + 8].is_digit() && literal[j + 9].is_digit() && literal[j + 10].is_digit() {
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
// again. Nothing is recorded for inputs that cannot all be told.
fn v3_tcc_prelude_stamp(key string, inputs V3HeaderInputs, unusable string) ?string {
	if inputs.unknown.len > 0 || inputs.files.len != inputs.identities.len {
		return none
	}
	mut out := strings.new_builder(128 + inputs.files.len * 128 + inputs.missing.len * 96)
	out.writeln('format=${v3_tcc_prelude_format}')
	out.writeln('key=${key}')
	if unusable.len > 0 {
		out.writeln('unusable=${unusable.replace('\n', ' ')}')
	}
	out.write_string(v3_header_inputs_text(&inputs))
	out.writeln('complete=1')
	return out.str()
}

// V3TccPreludeRecord is what a stamp says of a prelude: its inputs, and why its
// preprocessed form is not used, or '' when it is.
struct V3TccPreludeRecord {
mut:
	unusable string
	inputs   V3HeaderInputs
}

// v3_read_tcc_prelude_stamp returns what the stamp of the prelude `key` says, or
// none for a stamp of another prelude or of another form.
fn v3_read_tcc_prelude_stamp(stamp string, key string) ?V3TccPreludeRecord {
	lines := stamp.split_into_lines()
	if lines.len < 3 || lines[0] != 'format=${v3_tcc_prelude_format}' || lines[1] != 'key=${key}'
		|| lines.last() != 'complete=1' {
		return none
	}
	mut record := V3TccPreludeRecord{}
	for line in lines[2..lines.len - 1] {
		if line.starts_with('unusable=') {
			record.unusable = line['unusable='.len..]
		} else if !record.inputs.read_line(line) {
			return none
		}
	}
	return record
}

// v3_tcc_prelude_inputs_are_unchanged reports whether every header of `inputs` is
// the file that it was, and no header is where none was.
fn v3_tcc_prelude_inputs_are_unchanged(inputs &V3HeaderInputs) bool {
	for i, path in inputs.files {
		if modulecache.file_metadata_signature(path) != inputs.identities[i] {
			v3_trace_tcc_prelude('a header changed: ${path}')
			return false
		}
	}
	for path in inputs.missing {
		if os.exists(path) {
			v3_trace_tcc_prelude('a header appeared: ${path}')
			return false
		}
	}
	return true
}

fn v3_trace_tcc_prelude(message string) {
	if os.getenv('V3_CACHE_TRACE') != '' {
		eprintln('  V3 TinyCC prelude: ${message}')
	}
}

// v3_tcc_prelude_key identifies the preprocessed form of `prelude` for one TinyCC
// and one set of arguments. TinyCC also takes include directories from the
// environment. `output_dir` is the directory beside which TinyCC runs when the
// prelude, the arguments or the environment name a file relative to it, and ''
// when they do not.
fn v3_tcc_prelude_key(prelude string, tcc_path string, preprocess_args []string, output_dir string) string {
	mut hash := u64(1469598103934665603)
	for part in [v3_tcc_prelude_format, os.real_path(tcc_path), v3_cache_file_identity(tcc_path),
		preprocess_args.join('\x00'), os.getenv('CPATH'), os.getenv('C_INCLUDE_PATH'), output_dir,
		prelude] {
		hash = c_hash_bytes(hash, part.bytes())
		hash = c_hash_bytes(hash, [u8(0xff)])
	}
	return '${hash.hex()}_${prelude.len}'
}

// V3TccPrelude is the unit of a program in the form that reads its headers
// preprocessed, with what that form depends on.
struct V3TccPrelude {
	unit   string
	inputs V3HeaderInputs
	// The preprocessed form, the record of it, and the key of both.
	form  string
	stamp string
	key   string
}

// v3_tcc_prelude_unit returns the unit `source`, whose prelude is its first `end`
// bytes, with the prelude replaced by its preprocessed form in the file `form`.
// The lines after the prelude keep their numbers: `__LINE__` and what TinyCC says
// of a line are those of `source`.
fn v3_tcc_prelude_unit(form string, source string, end int) string {
	return '#include "${c_include_path(form)}"\n#line ${source[..end].count('\n') + 1}\n' +
		source[end..]
}

// v3_first_c_error_line returns the line of `output`, what a C compiler printed,
// that tells its first error, or the first line when none says `error`: a line
// that tells where a file was included from comes before it.
fn v3_first_c_error_line(output string) string {
	for line in output.split_into_lines() {
		if line.contains('error') {
			return line
		}
	}
	return output.all_before('\n')
}

// v3_write_tcc_prelude_stamp puts `stamp` in place of the record at `path`.
fn v3_write_tcc_prelude_stamp(path string, stamp string) bool {
	tmp := '${path}.tmp.${tempname.unique_token()}'
	os.write_file(tmp, stamp) or {
		os.rm(tmp) or {}
		return false
	}
	os.mv(tmp, path) or {
		os.rm(tmp) or {}
		return false
	}
	return true
}

// reject records that the preprocessed form of the prelude cannot stand for it,
// and why: a unit did not compile with it that compiles without it. No build
// reads the form after that, for as long as the headers stay what they are.
fn (p &V3TccPrelude) reject(reason string) {
	// A form that is gone is one that a unit could not read, whatever it holds.
	if p.stamp.len == 0 || !os.is_file(p.form) {
		return
	}
	stamp := v3_tcc_prelude_stamp(p.key, p.inputs, reason) or { return }
	if v3_write_tcc_prelude_stamp(p.stamp, stamp) {
		os.rm(p.form) or {}
		v3_trace_tcc_prelude('not used from now on: ${reason}')
	}
}

// v3_tcc_prelude returns `source`, the unit of a program, with the part that
// includes its headers replaced by an `#include` of that part in preprocessed
// form, which it takes from the module cache or puts there. It returns none when
// `source` has no such part, or when the preprocessed form cannot be made, kept or
// used: the build then compiles `source` as it is.
fn v3_tcc_prelude(manager &modulecache.Manager, source string, tcc_path string, tcc_args []string, source_name string, cc_dir string) ?V3TccPrelude {
	end := v3_tcc_prelude_end(source)
	if end == 0 || !manager.enabled {
		return none
	}
	prelude := source[..end]
	preprocess_args := v3_tcc_preprocess_args(tcc_args, source_name)
	// A relative path that leaves the directory of the build names a file by where
	// the output goes: the form is kept for that place then.
	output_dir := if v3_flags_name_relative_paths(preprocess_args)
		|| v3_flags_name_relative_paths([os.getenv('CPATH'), os.getenv('C_INCLUDE_PATH')])
		|| prelude.contains('"../') || prelude.contains('<../') {
		os.dir(cc_dir)
	} else {
		''
	}
	key := v3_tcc_prelude_key(prelude, tcc_path, preprocess_args, output_dir)
	cached := os.join_path(manager.dir, 'tcc_prelude_${key}.i')
	stamp_path := cached + '.stamp'
	if stamp := os.read_file(stamp_path) {
		if record := v3_read_tcc_prelude_stamp(stamp, key) {
			if v3_tcc_prelude_inputs_are_unchanged(&record.inputs) {
				if record.unusable.len > 0 {
					v3_trace_tcc_prelude('not used: ${record.unusable}')
					return none
				}
				if os.is_file(cached) {
					return V3TccPrelude{
						unit:   v3_tcc_prelude_unit(cached, source, end)
						inputs: record.inputs
						form:   cached
						stamp:  stamp_path
						key:    key
					}
				}
			}
		}
	}
	if !manager.ensure_dir() {
		return none
	}
	prelude_file := os.join_path_single(cc_dir, v3_tcc_prelude_input_name)
	output_file := os.join_path_single(cc_dir, 'v3_tcc_prelude_output.i')
	verify_file := os.join_path_single(cc_dir, 'v3_tcc_prelude_verify.c')
	probe_file := os.join_path_single(cc_dir, 'v3_tcc_prelude_probe.c')
	probe_object := os.join_path_single(cc_dir, 'v3_tcc_prelude_probe.o')
	tmp := '${cached}.tmp.${tempname.unique_token()}'
	defer {
		os.rm(prelude_file) or {}
		os.rm(output_file) or {}
		os.rm(verify_file) or {}
		os.rm(probe_file) or {}
		os.rm(probe_object) or {}
		os.rm(tmp) or {}
	}
	// The line after the prelude tells how far its headers counted.
	os.write_file(prelude_file, '${prelude}\n${v3_tcc_prelude_counter_probe} __COUNTER__\n') or {
		return none
	}
	// Whole seconds, and one to spare for a file system that rounds them.
	before_preprocessing := time.utc().unix() - 1
	mut args := preprocess_args.clone()
	args << ['-E', '-dD', '-o', os.file_name(output_file), v3_tcc_prelude_input_name]
	result := cmdexec.run_in(tcc_path, args, cc_dir)
	if result.exit_code != 0 {
		v3_trace_tcc_prelude('not preprocessed: ${result.output.all_before('\n')}')
		return none
	}
	preprocessed := os.read_file(output_file) or { return none }
	read, of_the_build, relative_paths := v3_preprocessed_files(preprocessed, cc_dir)
	// The headers are read once, for what they make an input of the form and for
	// what they say of their macros.
	mut texts := map[string]string{}
	for path in read {
		texts[path] = os.read_file(path) or { '' }
	}
	mut inputs := v3_inputs_of_headers(read, v3_has_include_names(prelude), v3_tcc_include_search(manager,
		tcc_path, preprocess_args, cc_dir), before_preprocessing, texts)
	inputs.add_time_of(prelude)
	if relative_paths {
		inputs.relative_paths = true
		if output_dir.len == 0 {
			// Nothing is kept of such a prelude: there is no form to make of it.
			v3_trace_tcc_prelude('not kept: a header is named relative to the directory of the output')
			return none
		}
	}
	if inputs.unknown.len > 0 {
		// Nothing can be recorded of headers that cannot all be told: what the
		// form is worth need not be found out.
		v3_trace_tcc_prelude('not kept: ${inputs.unknown}')
		return none
	}
	mut form := v3_tcc_split_preprocessed(preprocessed)
	if form.unusable.len == 0 && v3_tcc_source_has_build_time_macros(prelude) {
		form.unusable = 'it asks for the time or the count of the compilation'
	}
	if form.unusable.len == 0 && of_the_build.len > 0 {
		form.unusable = 'it includes a file that the build made: ${os.file_name(of_the_build)}'
	}
	mut macros := V3PreludeMacros{}
	if form.unusable.len == 0 {
		// What the preprocessor does not print of the macros is in the text.
		macros.add(prelude)
		for path in read {
			macros.add(texts[path])
		}
		form.definitions, form.unusable = macros.with_variable_arguments(form.definitions)
	}
	if form.unusable.len == 0 {
		// A compiler that reads the form must see the C that the preprocessor made
		// of the prelude: so it does when the form comes out of the preprocessor as
		// it went in. A form that TinyCC reports an error in is one that it cannot
		// use. The same run tells which of the names that the headers ask about
		// are macros before the preprocessor has read anything.
		tested := macros.names_to_test()
		os.write_file(tmp, form.text()) or { return none }
		os.write_file(verify_file, '${v3_tcc_preprocessor_probe(tested)}#include "${c_include_path(tmp)}"\n') or {
			return none
		}
		mut verify_args := preprocess_args.clone()
		verify_args << ['-E', '-dD', '-o', os.file_name(output_file), os.file_name(verify_file)]
		verified := cmdexec.run_in(tcc_path, verify_args, cc_dir)
		if verified.exit_code != 0 {
			if !v3_c_output_reports_source_error(verified.output) {
				// TinyCC did not run, was stopped, or could not write: that says
				// nothing of the form, and the next build asks again.
				v3_trace_tcc_prelude('not verified: ${verified.output.all_before('\n')}')
				return none
			}
			form.unusable = 'TinyCC does not read its preprocessed form: ${verified.output.all_before('\n')}'
		} else {
			again_text := os.read_file(output_file) or { return none }
			mut again := v3_tcc_split_preprocessed(again_text)
			again.definitions, _ = macros.with_variable_arguments(again.definitions)
			if !v3_tcc_preprocessed_forms_match(&form, &again) {
				form.unusable = 'its macros change the C that they expanded to'
			} else {
				// The compiler must find the same of those names defined: it
				// defines some macros that the preprocessor does not.
				os.write_file(probe_file, v3_tcc_compiler_probe(tested, again.defined)) or {
					return none
				}
				mut probe_args := preprocess_args.clone()
				probe_args << ['-c', '-o', os.file_name(probe_object), os.file_name(probe_file)]
				probed := cmdexec.run_in(tcc_path, probe_args, cc_dir)
				if probed.exit_code != 0 {
					name := v3_tcc_probe_difference(probed.output) or {
						v3_trace_tcc_prelude('not verified: ${probed.output.all_before('\n')}')
						return none
					}
					form.unusable = 'its headers ask about `${name}`, which TinyCC defines for a compilation and not for the preprocessor, or the other way round'
				}
			}
		}
	}
	// A unit that may ask for the time gives another object every second: what
	// the headers, the rest of the unit and the command say of it tells.
	mut of_the_time := V3HeaderInputs{
		mentions_time: inputs.mentions_time
		time_parts:    inputs.time_parts.clone()
	}
	of_the_time.add_time_of(source[end..])
	for arg in tcc_args {
		of_the_time.add_time_of(arg)
	}
	if form.unusable.len == 0 && !of_the_time.may_ask_for_time()
		&& !source[end..].contains('__COUNTER__') {
		// The unit that asks for the form is what there is to compile: with the
		// form and without it, it must give the same object.
		match v3_tcc_compare_units(tcc_path, tcc_args, source_name, cc_dir, source, v3_tcc_prelude_unit(tmp,
			source, end)) {
			.alike {}
			.differ {
				form.unusable = 'TinyCC makes another object of a unit that reads it'
			}
			.uncompiled {
				// TinyCC does not compile the unit as it is: there is nothing to
				// compare, and the form is kept for what the other checks say.
				v3_trace_tcc_prelude('not compared: TinyCC does not compile the unit as it is')
			}
			.unknown {
				// TinyCC did not run: that says nothing of the form, and the next
				// build asks again.
				v3_trace_tcc_prelude('not verified: the unit was not compiled')
				return none
			}
		}
	}
	stamp := v3_tcc_prelude_stamp(key, inputs, form.unusable) or {
		v3_trace_tcc_prelude('not kept: a header cannot be told apart from a changed one')
		return none
	}
	// The stamp commits the text: remove the old one before the text is replaced.
	os.rm(stamp_path) or {}
	if form.unusable.len == 0 {
		os.mv(tmp, cached) or { return none }
	} else {
		os.rm(cached) or {}
	}
	if !v3_write_tcc_prelude_stamp(stamp_path, stamp) {
		return none
	}
	if form.unusable.len > 0 {
		v3_trace_tcc_prelude('not used: ${form.unusable}')
		return none
	}
	v3_trace_tcc_prelude('preprocessed ${prelude.len} bytes into ${cached}')
	return V3TccPrelude{
		unit:   v3_tcc_prelude_unit(cached, source, end)
		inputs: inputs
		form:   cached
		stamp:  stamp_path
		key:    key
	}
}

// V3UnitsCompared is what compiling a unit in two forms tells of them.
enum V3UnitsCompared {
	alike
	differ
	uncompiled
	unknown
}

// v3_tcc_compare_units tells whether TinyCC makes the same object of `second` as
// of `first`, two forms of a program unit. A `first` that it finds an error in is
// `.uncompiled`, a `second` that it finds one in differs from a `first` that it
// compiles, and nothing is known when TinyCC did not get to compile one of them.
fn v3_tcc_compare_units(tcc_path string, tcc_args []string, source_name string, cc_dir string, first string, second string) V3UnitsCompared {
	mut args := v3_tcc_preprocess_args(tcc_args, source_name)
	args << ['-c', '-o', 'verify.o', 'verify.c']
	source := os.join_path_single(cc_dir, 'verify.c')
	object := os.join_path_single(cc_dir, 'verify.o')
	defer {
		os.rm(source) or {}
		os.rm(object) or {}
	}
	mut objects := [][]u8{}
	for i, unit in [first, second] {
		os.write_file(source, unit) or { return .unknown }
		os.rm(object) or {}
		compiled := cmdexec.run_in(tcc_path, args, cc_dir)
		if compiled.exit_code != 0 {
			if !v3_c_output_reports_source_error(compiled.output) {
				return .unknown
			}
			return if i == 0 { V3UnitsCompared.uncompiled } else { V3UnitsCompared.differ }
		}
		objects << os.read_bytes(object) or { return .unknown }
	}
	return if objects[0] == objects[1] { V3UnitsCompared.alike } else { V3UnitsCompared.differ }
}

// v3_tcc_units_compile_alike reports whether TinyCC makes the same object of two
// forms of a program unit. V3_TCC_PRELUDE_VERIFY=1 asks for it in every build, for
// tests and for looking into a difference.
fn v3_tcc_units_compile_alike(tcc_path string, tcc_args []string, source_name string, cc_dir string, first string, second string) bool {
	return v3_tcc_compare_units(tcc_path, tcc_args, source_name, cc_dir, first, second) == .alike
}
