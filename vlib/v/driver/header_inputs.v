module driver

import os
import strings
import v.modulecache
import v.tempname

// Native dependencies are compiler-reported paths and opaque file identities.
// V never reads header contents or preprocessed declarations.

// V3IncludeSearch is where a C compiler looks for an included file: `dirs` in the
// order of the search, `frameworks` those of them that hold frameworks, where
// `<A/B.h>` is `A.framework/Headers/B.h`, and `absent` the directories that it was
// told to search and that are not there.
struct V3IncludeSearch {
mut:
	dirs       []string
	frameworks []string
	absent     []string
}

// V3HeaderInputs is what a compilation depends on besides its unit and its
// command: the `files` that the compiler read, each with the metadata in
// `identities` that it had when it was read, and the `missing` paths where a file
// that is not there would be read instead of, or in addition to, one of them.
// `mentions_time` says that the generated unit spells `__DATE__`, `__TIME__` or
// `__TIMESTAMP__`, so that the compilation may give another result at another
// time, and `time_parts` holds the parts of those names that the unit or command
// has as words of their own: `##` can put a name together of them. `unknown` says
// why the inputs cannot all be told, or is '' when they can.
struct V3HeaderInputs {
mut:
	files         []string
	identities    []string
	missing       []string
	mentions_time bool
	time_parts    []string
	// Whether a file is named by a path that is relative to the directory of the
	// build and leaves it: what it names depends on where the output goes.
	relative_paths bool
	unknown        string
}

// v3_time_macro_names are the macros whose value is that of the moment of the
// compilation, or of the last change of the file that is compiled.
const v3_time_macro_names = ['__DATE__', '__TIME__', '__TIMESTAMP__']

// v3_c_mentions_compile_time reports whether `text` spells a macro whose value is
// that of the moment of the compilation.
fn v3_c_mentions_compile_time(text string) bool {
	return text.contains('__DATE__') || text.contains('__TIME__') || text.contains('__TIMESTAMP__')
}

// v3_c_time_name_parts returns the identifiers of the C `text` that are a part of
// one of v3_time_macro_names and not the whole of it, such as `__TI` and `ME__`,
// in the order of their bytes. The preprocessor makes one name of two with `##`,
// so a text that spells none of the names can still ask for the time: with
// `JOIN(__TI, ME__)`, or with a macro that the command defines as `__TI`. What is
// in a comment, in a string or in a character literal is no identifier.
fn v3_c_time_name_parts(text string) []string {
	mut parts := []string{}
	mut i := 0
	for i < text.len {
		c := text[i]
		if c == `/` {
			after := v3_c_skip_comment(text, i)
			i = if after > i { after } else { i + 1 }
			continue
		}
		if c == `"` || c == `'` {
			i = v3_c_skip_literal(text, i)
			continue
		}
		if c.is_digit() {
			i = v3_c_skip_number(text, i)
			continue
		}
		if c != `_` && !c.is_letter() {
			i++
			continue
		}
		start := i
		mut of_a_name := true
		for i < text.len && (text[i] == `_` || text[i].is_alnum()) {
			if of_a_name && text[i] !in [`_`, `A`, `D`, `E`, `I`, `M`, `P`, `S`, `T`] {
				of_a_name = false
			}
			i++
		}
		if !of_a_name || i - start > 12 {
			continue
		}
		word := text[start..i]
		if word !in parts && v3_time_macro_names.any(it != word && it.contains(word)) {
			parts << word
		}
	}
	parts.sort()
	return parts
}

// v3_time_names_can_be_built reports whether one of v3_time_macro_names is `parts`
// written one after another: whether a C that has those identifiers can paste the
// name together.
fn v3_time_names_can_be_built(parts []string) bool {
	for name in v3_time_macro_names {
		// built[n] says that the first n bytes of the name are parts.
		mut built := []bool{len: name.len + 1}
		built[0] = true
		for end in 1 .. name.len + 1 {
			for part in parts {
				if part.len <= end && built[end - part.len] && name[end - part.len..end] == part {
					built[end] = true
					break
				}
			}
		}
		if built[name.len] {
			return true
		}
	}
	return false
}

// may_ask_for_time reports whether what `inputs` holds of the time can make a C
// ask for it: a macro that it names, or parts that can be pasted to its name.
fn (inputs &V3HeaderInputs) may_ask_for_time() bool {
	return inputs.mentions_time || v3_time_names_can_be_built(inputs.time_parts)
}

// add_time_of takes what `text`, the C of a unit or an argument of a command,
// says of the time of the compilation into `inputs`.
fn (mut inputs V3HeaderInputs) add_time_of(text string) {
	if v3_c_mentions_compile_time(text) {
		inputs.mentions_time = true
	}
	for part in v3_c_time_name_parts(text) {
		if part !in inputs.time_parts {
			inputs.time_parts << part
		}
	}
	inputs.time_parts.sort()
}

// v3_quoted_include_names returns the names that `source` includes in quotation
// marks: the compiler looks for those in the directory of the file that names them
// before it looks anywhere else. Only a line that starts with `#` is looked at.
fn v3_quoted_include_names(source string) []string {
	mut names := []string{}
	mut pos := 0
	for pos < source.len {
		mut start := pos
		for start < source.len && source[start] in [` `, `\t`] {
			start++
		}
		mut line_end := source.index_after_('\n', start)
		if line_end < 0 {
			line_end = source.len
		}
		pos = line_end + 1
		if start >= line_end || source[start] != `#` {
			continue
		}
		// Most directives are no includes: `#if`, `#define`, `#endif`.
		mut word := start + 1
		for word < line_end && source[word] in [` `, `\t`] {
			word++
		}
		if word + 1 >= line_end || source[word] != `i` || source[word + 1] !in [`n`, `m`] {
			continue
		}
		line := source[start..line_end]
		mut argument := v3_include_directive_argument(line) or { '' }
		if argument.len == 0 {
			// `#include_next` and the `#import` of Objective-C name a file as well.
			directive := line[1..].trim_left(' \t')
			for name in ['include_next', 'import'] {
				if directive.starts_with(name) {
					argument = directive[name.len..].trim_left(' \t')
				}
			}
		}
		if argument.len > 2 && argument[0] == `"` {
			close := argument.index_after_('"', 1)
			if close > 1 && argument[1..close] !in names {
				names << argument[1..close]
			}
		}
	}
	return names
}

// v3_path_is_older_than reports whether `relative` below `root`, which is there,
// was there before the time `before`, in seconds: each part of the way to it was
// made, changed and put in place earlier. A file that someone moves into a
// directory keeps the time of its contents and gets a new time of change.
fn v3_path_is_older_than(root string, relative string, before i64) bool {
	mut current := root
	for part in relative.split('/') {
		if part.len == 0 {
			continue
		}
		current = os.join_path_single(current, part)
		attributes := os.stat(current) or { return false }
		if attributes.mtime >= before || attributes.ctime >= before {
			return false
		}
	}
	return true
}

// v3_file_identity_from_before returns the metadata of `path` when that is the
// metadata of the file that a compiler read which started at the time `before`,
// in seconds: the metadata is taken after the compiler has read the file, so a
// file that was written or put in place at `before` or later may not be the one
// that was read.
fn v3_file_identity_from_before(path string, before i64) ?string {
	identity := modulecache.file_metadata_signature(path)
	if identity.len == 0 || path.contains_any('\t\n') {
		return none
	}
	attributes := os.stat(path) or { return none }
	if attributes.mtime >= before || attributes.ctime >= before {
		return none
	}
	return identity
}

// v3_include_name_below returns the name by which a file is included that is at
// `relative` below a directory of the search: the path itself, or for a directory
// of frameworks `A/B.h` for `A.framework/Headers/B.h`. It returns none for a file
// of a framework that no name of that form leads to.
fn v3_include_name_below(relative string, framework bool) ?string {
	if !framework {
		return relative
	}
	parts := relative.split('/')
	if parts.len < 3 || !parts[0].ends_with('.framework') {
		return none
	}
	mut headers := 1
	if parts.len > 4 && parts[1] == 'Versions' {
		headers = 3
	}
	if parts[headers] !in ['Headers', 'PrivateHeaders'] || headers + 1 >= parts.len {
		return none
	}
	mut name := [parts[0].all_before_last('.framework')]
	name << parts[headers + 1..]
	return name.join('/')
}

// v3_include_path_below is the opposite of v3_include_name_below: the path below a
// directory of the search where a file of the name `name` would be found.
fn v3_include_path_below(name string, framework bool) ?string {
	if !framework {
		return name
	}
	slash := name.index_u8(`/`)
	if slash <= 0 || slash + 1 >= name.len {
		return none
	}
	return '${name[..slash]}.framework/Headers/${name[slash + 1..]}'
}

// v3_given_include_dirs returns the absolute directories that `args` tell a C
// compiler to search, as opposed to the ones it searches on its own.
fn v3_given_include_dirs(args []string, build_dir string) []string {
	names := ['-I', '-isystem', '-iquote', '-idirafter', '-iframework', '-F']
	search_args := v3_include_search_args(args)
	mut dirs := []string{}
	mut i := 0
	for i < search_args.len {
		arg := search_args[i]
		i++
		mut dir := ''
		if arg in names {
			if i < search_args.len {
				dir = search_args[i]
				i++
			}
		} else {
			for name in names {
				if arg.starts_with(name) {
					dir = arg[name.len..].trim_space()
					break
				}
			}
		}
		if dir.len > 0 {
			dirs << os.norm_path(if os.is_abs_path(dir) {
				dir
			} else {
				os.join_path(build_dir, dir)
			})
		}
	}
	return dirs
}

// v3_include_quote_dirs returns the directories that a compiler searches first for
// a name in quotation marks: that of each translation unit of `units`, and that of
// each header of `read` that is not one of the compiler's own, in the directories
// of its search that the arguments do not give (`given`). A header of the compiler
// names its own neighbours, which change with the compiler.
fn v3_include_quote_dirs(search V3IncludeSearch, given []string, read []string, units []string) []string {
	own_roots := search.dirs.map(os.norm_path(it)).filter(it !in given)
	mut quote_dirs := map[string]bool{}
	for unit in units {
		quote_dirs[os.dir(os.norm_path(unit))] = true
	}
	for path in read {
		header := os.norm_path(path)
		if !own_roots.any(v3_path_is_below(header, it)) {
			quote_dirs[os.dir(header)] = true
		}
	}
	mut dirs := quote_dirs.keys()
	dirs.sort()
	return dirs
}

// v3_missing_include_candidates returns the paths that have to stay absent for a
// compiler to read the headers `read` again: those of each directory that it
// searches before the one where it found a header, and of the v3_include_quote_dirs
// of the translation units `units` and of the program's own headers, which it
// searches first for a name in quotation marks. A path below a directory that is
// not there is that directory. Zig's Cache.Manifest and Odin's try_cached_build
// check only the files that they recorded, so a new file earlier in a search goes
// unseen there. It returns none when one of the paths holds a file newer than
// `before`, in seconds, from before the compiler started: that file may or may not
// be what it read. V does not open the headers, so it cannot tell what a header
// asks `__has_include` about.
fn v3_missing_include_candidates(search V3IncludeSearch, given []string, read []string, units []string, before i64) ?[]string {
	roots := search.dirs.map(os.norm_path(it))
	frameworks := search.frameworks.map(os.norm_path(it))
	headers := read.map(os.norm_path(it))
	quote_dirs := v3_include_quote_dirs(search, given, read, units)
	mut missing := map[string]bool{}
	for dir in search.absent {
		missing[os.norm_path(dir)] = true
	}
	mut there := map[string]bool{}
	for header in headers {
		mut found := -1
		for i, root in roots {
			if v3_path_is_below(header, root) {
				found = i
				break
			}
		}
		if found < 0 {
			// It was found beside the file that names it, which is searched first.
			continue
		}
		root := roots[found]
		name := v3_include_name_below(header[root.len + 1..], root in frameworks) or { continue }
		mut earlier := quote_dirs.clone()
		earlier << roots[..found]
		for dir in earlier {
			below := v3_include_path_below(name, dir in frameworks) or { continue }
			if os.join_path_single(dir, below) == header {
				continue
			}
			if !v3_note_missing_below(dir, below, before, mut there, mut missing) {
				return none
			}
		}
	}
	mut result := missing.keys()
	result.sort()
	return result
}

// v3_note_missing_below records the first part of the way from `dir` to `below`
// that is not there. It returns false when all of it is, and is recent.
fn v3_note_missing_below(dir string, below string, before i64, mut there map[string]bool, mut missing map[string]bool) bool {
	mut current := dir
	for part in below.split(os.path_separator) {
		current = os.join_path_single(current, part)
		exists := there[current] or {
			found := os.exists(current)
			there[current] = found
			found
		}
		if !exists {
			missing[current] = true
			return true
		}
	}
	return v3_path_is_older_than(dir, below, before)
}

// v3_path_is_below reports whether `path` is inside the directory `dir`.
fn v3_path_is_below(path string, dir string) bool {
	return path.len > dir.len && path.starts_with(dir)
		&& (path[dir.len] == os.path_separator[0] || dir.ends_with(os.path_separator))
}

// v3_header_inputs returns what a compilation of `unit` depends on, given the
// files that the compiler `read` for it, where it looks for an included file, and
// a time `before`, in seconds, from before it started.
//
// A file of the name of one that was read would be read instead of it if it were
// in a directory that is searched earlier: for each such directory, the path
// where it is not is an input. So is the directory of a file for each name that
// the file includes in quotation marks, which is searched first of all, and each
// directory of the search for a name that the unit or a file asks `__has_include`
// about. A file that is in such a place now, and was not there before the
// compiler started, may or may not be what the compiler read: the inputs are not
// known then. One that was there all along is one that the compiler passed over,
// as `#include_next` does.
fn v3_header_inputs(read []string, unit string, before i64) V3HeaderInputs {
	mut inputs := v3_inputs_of_headers(read, v3_has_include_names(unit), before)
	inputs.add_time_of(unit)
	return inputs
}

// v3_inputs_of_headers records compiler-reported paths as opaque files. A
// manifest does not describe missing include candidates or time macros, so it
// cannot certify executable reuse when headers were read.
fn v3_inputs_of_headers(read []string, unit_asks []string, before i64) V3HeaderInputs {
	mut inputs := V3HeaderInputs{
		files: read.clone()
	}
	if read.len > 0 || unit_asks.len > 0 {
		inputs.unknown = 'native headers require C compiler processing'
	}
	for path in inputs.files {
		inputs.identities << v3_file_identity_from_before(path, before) or { '' }
	}
	return inputs
}

fn v3_header_inputs_text(inputs &V3HeaderInputs) string {
	mut out := strings.new_builder(64 + inputs.files.len * 128 + inputs.missing.len * 96)
	if inputs.mentions_time {
		out.writeln('mentions_time=1')
	}
	for part in inputs.time_parts {
		out.writeln('time_part=${part}')
	}
	if inputs.relative_paths {
		out.writeln('relative_paths=1')
	}
	for i, path in inputs.files {
		out.writeln('file=${path}\t${inputs.identities[i]}')
	}
	for path in inputs.missing {
		out.writeln('missing=${path}')
	}
	return out.str()
}

// read_line takes one line of v3_header_inputs_text into `inputs`, and reports
// whether it is one.
fn (mut inputs V3HeaderInputs) read_line(line string) bool {
	if line == 'mentions_time=1' {
		inputs.mentions_time = true
	} else if line.starts_with('time_part=') {
		inputs.time_parts << line['time_part='.len..]
	} else if line == 'relative_paths=1' {
		inputs.relative_paths = true
	} else if line.starts_with('file=') {
		tab := line.last_index_u8(`\t`)
		if tab <= 'file='.len {
			return false
		}
		inputs.files << line['file='.len..tab]
		inputs.identities << line[tab + 1..]
	} else if line.starts_with('missing=') {
		inputs.missing << line['missing='.len..]
	} else {
		return false
	}
	return true
}

// v3_kept_header_inputs is v3_header_inputs with a record in the module cache.
// What the inputs of a compilation are follows from what its files hold, so a
// compilation that read the files of an earlier one, each still the file that it
// was and older than the compilation, with no file where none was, has the inputs
// of that one: they are read back in place of every header.
fn v3_kept_header_inputs(manager &modulecache.Manager, read []string, unit string, search V3IncludeSearch, before i64) V3HeaderInputs {
	mut sorted := read.clone()
	sorted.sort()
	asked := v3_has_include_names(unit)
	key := c_hash_bytes(u64(1469598103934665603), ['v3-header-inputs-3', sorted.join('\n'),
		search.dirs.join('\n'), search.frameworks.join('\n'), search.absent.join('\n'),
		asked.join('\n')].join('\x00').bytes()).hex()
	record := os.join_path_single(manager.dir, 'header_inputs_${key}_${sorted.len}')
	if kept := os.read_file(record) {
		if of_record := v3_header_inputs_of_record(kept, sorted, before) {
			mut inputs := of_record
			inputs.add_time_of(unit)
			return inputs
		}
	}
	mut inputs := v3_inputs_of_headers(sorted, asked, before)
	// What a unit says of the time is no fact of its headers: the record is of
	// the headers alone.
	if inputs.unknown.len == 0 && manager.ensure_dir() {
		tmp := '${record}.tmp.${tempname.unique_token()}'
		if _ := os.write_file(tmp, v3_header_inputs_text(&inputs) + 'complete=1\n') {
			os.mv(tmp, record) or { os.rm(tmp) or {} }
		} else {
			os.rm(tmp) or {}
		}
	}
	inputs.add_time_of(unit)
	return inputs
}

// v3_header_inputs_of_record returns the inputs that `kept` records for a
// compilation that read the files `read`, or none when the record is of other
// files, when a file is not what it was or is as new as the compilation that
// started at `before`, or when a file is where none was.
fn v3_header_inputs_of_record(kept string, read []string, before i64) ?V3HeaderInputs {
	lines := kept.split_into_lines()
	if lines.len == 0 || lines.last() != 'complete=1' {
		return none
	}
	mut inputs := V3HeaderInputs{}
	for line in lines[..lines.len - 1] {
		if !inputs.read_line(line) {
			return none
		}
	}
	mut recorded := map[string]bool{}
	for path in inputs.files {
		recorded[path] = true
	}
	for path in read {
		if !recorded[path] {
			return none
		}
	}
	for i, path in inputs.files {
		identity := v3_file_identity_from_before(path, before) or { return none }
		if identity != inputs.identities[i] {
			return none
		}
	}
	for path in inputs.missing {
		if os.exists(path) {
			return none
		}
	}
	return inputs
}
