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

const v3_tcc_prelude_format = 'v3-tcc-prelude-2'

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
}

// v3_tcc_prelude_inputs works the inputs out from the output of the preprocessor,
// whose line markers name every file that it read. `first_dirs` are the include
// directories that are searched first and in that order, `later_dirs` those that
// come after them, in an order that is not known here. A file of the name of one
// that was read would be found instead of it in any directory that is searched
// before the one it is in: for each such directory, the path where it is not is
// an input. So is each directory for a name that the files ask `__has_include`
// about.
fn v3_tcc_prelude_inputs(preprocessed string, prelude string, first_dirs []string, later_dirs []string) V3TccPreludeInputs {
	mut include_dirs := first_dirs.clone()
	include_dirs << later_dirs
	mut files := map[string]bool{}
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
				path := line[open + 1..close]
				if os.is_abs_path(path) {
					files[path] = true
				}
			}
		}
		pos = line_end + 1
	}
	mut missing := map[string]bool{}
	mut asked := v3_has_include_names(prelude)
	for path, _ in files {
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

// v3_strip_preprocessor_line_markers removes the `# <line> "<file>"` lines of
// preprocessed C. They are a tenth of it, and what they say is of no use to a
// program unit that exists for one build.
fn v3_strip_preprocessor_line_markers(preprocessed string) string {
	mut out := strings.new_builder(preprocessed.len)
	mut pos := 0
	for pos < preprocessed.len {
		mut line_end := preprocessed.index_after_('\n', pos)
		if line_end < 0 {
			line_end = preprocessed.len
		}
		if !(line_end - pos > 2 && preprocessed[pos] == `#` && preprocessed[pos + 1] == ` `
			&& preprocessed[pos + 2].is_digit()) {
			out.write_string(preprocessed[pos..line_end])
			out.write_u8(`\n`)
		}
		pos = line_end + 1
	}
	return out.str()
}

// v3_tcc_prelude_stamp records the inputs of a preprocessed prelude. `before` is a
// time, in seconds, from before the preprocessor started. The files that it read
// are known only from what it printed, so their metadata is taken after it has
// read them: a file that was written or put in place at `before` or later may not
// be the one that was read, and nothing is recorded then.
fn v3_tcc_prelude_stamp(key string, inputs V3TccPreludeInputs, before i64) ?string {
	mut out := strings.new_builder(128 + inputs.files.len * 128 + inputs.missing.len * 96)
	out.writeln('format=${v3_tcc_prelude_format}')
	out.writeln('key=${key}')
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

// v3_tcc_source_with_cached_prelude returns `source` with the part that includes
// its headers replaced by an `#include` of that part in preprocessed form, which
// it takes from the module cache or puts there. It returns none when `source` has
// no such part, or when the preprocessed form cannot be made or kept: the build
// then compiles `source` as it is.
fn v3_tcc_source_with_cached_prelude(manager &modulecache.Manager, source string, tcc_path string, tcc_args []string, source_name string, cc_dir string) ?string {
	end := v3_tcc_prelude_end(source)
	if end == 0 || !manager.enabled {
		return none
	}
	prelude := source[..end]
	if v3_tcc_source_has_build_time_macros(prelude) {
		return none
	}
	preprocess_args := v3_tcc_preprocess_args(tcc_args, source_name)
	key := v3_tcc_prelude_key(prelude, tcc_path, preprocess_args)
	cached := os.join_path(manager.dir, 'tcc_prelude_${key}.i')
	stamp_path := cached + '.stamp'
	if stamp := os.read_file(stamp_path) {
		if os.is_file(cached) && v3_tcc_prelude_stamp_is_valid(stamp, key) {
			return '#include "${c_include_path(cached)}"\n' + source[end..]
		}
	}
	if !manager.ensure_dir() {
		return none
	}
	prelude_file := os.join_path_single(cc_dir, 'prelude.c')
	output_file := os.join_path_single(cc_dir, 'prelude.i')
	defer {
		os.rm(prelude_file) or {}
		os.rm(output_file) or {}
	}
	os.write_file(prelude_file, prelude) or { return none }
	// Whole seconds, and one to spare for a file system that rounds them.
	before_preprocessing := time.utc().unix() - 1
	mut args := preprocess_args.clone()
	args << ['-E', '-dD', '-o', 'prelude.i', 'prelude.c']
	result := cmdexec.run_in(tcc_path, args, cc_dir)
	if result.exit_code != 0 {
		v3_trace_tcc_prelude('not preprocessed: ${result.output.all_before('\n')}')
		return none
	}
	preprocessed := os.read_file(output_file) or { return none }
	first_dirs, mut later_dirs := v3_tcc_include_dirs(preprocess_args)
	for dir in v3_tcc_default_include_dirs(tcc_path, preprocess_args, cc_dir) {
		if dir !in first_dirs && dir !in later_dirs {
			later_dirs << dir
		}
	}
	inputs := v3_tcc_prelude_inputs(preprocessed, prelude, first_dirs, later_dirs)
	stamp := v3_tcc_prelude_stamp(key, inputs, before_preprocessing) or {
		v3_trace_tcc_prelude('not kept: a header cannot be told apart from a changed one')
		return none
	}
	// The stamp commits the text: remove the old one before the text is replaced.
	os.rm(stamp_path) or {}
	tmp := '${cached}.tmp.${tempname.unique_token()}'
	os.write_file(tmp, v3_strip_preprocessor_line_markers(preprocessed)) or {
		os.rm(tmp) or {}
		return none
	}
	os.mv(tmp, cached) or {
		os.rm(tmp) or {}
		return none
	}
	stamp_tmp := '${stamp_path}.tmp.${tempname.unique_token()}'
	os.write_file(stamp_tmp, stamp) or {
		os.rm(stamp_tmp) or {}
		return none
	}
	os.mv(stamp_tmp, stamp_path) or {
		os.rm(stamp_tmp) or {}
		return none
	}
	v3_trace_tcc_prelude('preprocessed ${prelude.len} bytes into ${cached}')
	return '#include "${c_include_path(cached)}"\n' + source[end..]
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
