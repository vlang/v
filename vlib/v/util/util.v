module util

import os
import strings
import v.ansi

const normalized_workdir = os.wd_at_startup.replace('\\', '/') + '/'

// set_vroot_folder exposes the V executable and marks child tool processes.
pub fn set_vroot_folder(vroot_path string) {
	if os.getenv('VEXE') == '' {
		vname := if os.user_os() == 'windows' { 'v.exe' } else { 'v' }
		os.setenv('VEXE', os.real_path(os.join_path_single(vroot_path, vname)), true)
	}
	os.setenv('VCHILD', 'true', true)
}

// quote_path quotes a path for use in a shell command.
pub fn quote_path(path string) string {
	return os.quoted_path(path)
}

// args_quote_paths quotes paths and joins them into a shell command fragment.
pub fn args_quote_paths(args []string) string {
	return args.map(os.quoted_path(it)).join(' ')
}

// find_all_v_files resolves V files below the supplied files and directories.
pub fn find_all_v_files(roots []string) ![]string {
	mut files := []string{}
	for file in roots {
		if os.is_dir(file) {
			files << os.walk_ext(file, '.v')
			files << os.walk_ext(file, '.vsh')
			continue
		}
		if !file.ends_with('.v') && !file.ends_with('.vv') && !file.ends_with('.vsh') {
			return error('v fmt can only be used on .v files.\nOffending file: "${file}"')
		}
		if !os.exists(file) {
			return error('"${file}" does not exist')
		}
		files << file
	}
	return files
}

// path_styled_for_error_messages returns a stable path for diagnostics.
pub fn path_styled_for_error_messages(path string) string {
	mut real_path := os.real_path(path).replace('\\', '/')
	if os.getenv('VERROR_PATHS') != 'absolute' && real_path.starts_with(normalized_workdir) {
		real_path = real_path[normalized_workdir.len..]
	}
	return real_path
}

// verror prints a tool error and exits.
@[noreturn]
pub fn verror(kind string, message string) {
	eprintln('${kind}: ${message}')
	exit(1)
}

// is_escape_sequence reports whether c is a valid escape sequence denoter.
@[inline]
pub fn is_escape_sequence(c u8) bool {
	return c in [`x`, `u`, `e`, `n`, `r`, `t`, `v`, `a`, `f`, `b`, `\\`, `\``, `$`, `@`, `?`, `{`,
		`}`, `'`, `"`, `U`]
}

// contains_capital reports whether s contains an uppercase character.
pub fn contains_capital(s string) bool {
	for c in s {
		if c.is_capital() {
			return true
		}
	}
	return false
}

// tokenize_c_flag splits a C flag on unquoted whitespace while preserving quotes.
pub fn tokenize_c_flag(value string) []string {
	mut tokens := []string{}
	mut start := -1
	mut quote := u8(0)
	mut escaped := false
	for i, c in value.bytes() {
		if start < 0 {
			if c.is_space() {
				continue
			}
			start = i
		}
		if escaped {
			escaped = false
			continue
		}
		if c == `\\` {
			escaped = true
			continue
		}
		if quote != 0 {
			if c == quote {
				quote = 0
			}
			continue
		}
		if c in [`'`, `"`] {
			quote = c
			continue
		}
		if c.is_space() {
			tokens << value[start..i]
			start = -1
		}
	}
	if start >= 0 {
		tokens << value[start..]
	}
	return tokens
}

// nearest_vmod_root returns the closest directory containing a v.mod for path.
pub fn nearest_vmod_root(path string) ?string {
	mut dir := if path.len == 0 {
		os.getwd()
	} else if os.is_dir(path) {
		path
	} else {
		os.dir(path)
	}
	if dir.len == 0 {
		dir = os.getwd()
	}
	dir = os.real_path(dir)
	for dir.len > 0 {
		if os.is_file(os.join_path_single(dir, 'v.mod')) {
			return dir
		}
		parent := os.dir(dir)
		if parent == dir {
			break
		}
		dir = parent
	}
	return none
}

// githash returns the current seven-character Git commit hash for path.
pub fn githash(path string) !string {
	git_marker := os.join_path(path, '.git')
	mut git_dir := git_marker
	if os.is_file(git_marker) {
		marker := os.read_file(git_marker) or { return error('failed to read `${git_marker}`') }
		if !marker.starts_with('gitdir: ') {
			return error('invalid Git worktree marker `${git_marker}`')
		}
		configured := marker.all_after('gitdir: ').trim_space()
		git_dir = os.real_path(if os.is_abs_path(configured) {
			configured
		} else {
			os.join_path(path, configured)
		})
	}
	head_file := os.join_path(git_dir, 'HEAD')
	if !os.exists(head_file) {
		return error('failed to find `${head_file}`')
	}
	head_content := os.read_file(head_file) or { return error('failed to read `${head_file}`') }
	hash := if head_content.starts_with('ref: ') {
		reference := head_content[5..].trim_space()
		mut revision_path := os.join_path(git_dir, reference)
		if !os.exists(revision_path) {
			common_dir_file := os.join_path(git_dir, 'commondir')
			common_dir := os.read_file(common_dir_file) or {
				return error('failed to find revision `${reference}`')
			}
			revision_path = os.real_path(os.join_path(git_dir, common_dir.trim_space(), reference))
		}
		if !os.exists(revision_path) {
			return error('failed to find revision file `${revision_path}`')
		}
		os.read_file(revision_path) or {
			return error('failed to read revision file `${revision_path}`')
		}
	} else {
		head_content
	}
	return hash[..7] or { error('failed to limit hash `${hash}` to 7 characters') }
}

struct Possibility {
	value  string
	svalue string
mut:
	similarity f32
}

// CalculateSuggestionSimilarityFN compares two suggestion candidates.
pub type CalculateSuggestionSimilarityFN = fn (s1 string, s2 string) f32

struct Suggestion {
mut:
	known                []Possibility
	wanted               string
	swanted              string
	similarity_threshold f32
	similarity_fn        CalculateSuggestionSimilarityFN = strings.dice_coefficient
}

// SuggestionParams configures new_suggestion.
@[params]
pub struct SuggestionParams {
pub mut:
	similarity_threshold f32 = 0.5
	similarity_fn        CalculateSuggestionSimilarityFN = strings.dice_coefficient
}

// new_suggestion creates a diagnostic suggestion from wanted and possibilities.
pub fn new_suggestion(wanted string, possibilities []string, params SuggestionParams) Suggestion {
	mut suggestion := Suggestion{
		known: []Possibility{cap: int(max_suggestions_limit)}
		wanted: wanted
		swanted: short_module_name(wanted)
		similarity_threshold: params.similarity_threshold
		similarity_fn: params.similarity_fn
	}
	suggestion.add_many(possibilities)
	suggestion.sort()
	return suggestion
}

const max_suggestions_limit = $d('max_suggestions_limit', 200)

fn (mut s Suggestion) add(value string) {
	if s.known.len >= max_suggestions_limit || value in [s.wanted, s.swanted] {
		return
	}
	short_value := short_module_name(value)
	if short_value in [s.wanted, s.swanted] {
		return
	}
	similarity := f32(int(s.similarity_fn(s.swanted, short_value) * 1000)) / 1000
	s.known << Possibility{
		value: value
		svalue: short_value
		similarity: similarity
	}
}

fn (mut s Suggestion) add_many(values []string) {
	for value in values {
		if s.known.len >= max_suggestions_limit {
			break
		}
		s.add(value)
	}
}

fn (mut s Suggestion) sort() {
	s.known.sort(a.similarity < b.similarity)
}

// say appends the best suggestion, or a short possibility list, to message.
pub fn (s Suggestion) say(message string) string {
	mut result := message
	mut found := false
	if s.known.len > 0 {
		top := s.known.last()
		if top.similarity > s.similarity_threshold && !top.value.starts_with('[]') {
			result += '.\nDid you mean `${highlight_suggestion(top.value)}`?'
			found = true
		}
	}
	if !found && s.known.len > 0 {
		mut values := s.known.map('`${highlight_suggestion(it.svalue)}`')
		values.sort()
		if values.len == 1 {
			result += '.\n1 possibility: ${values[0]}.'
		} else if values.len < 25 {
			result += '.\n${values.len} possibilities: ' + values.join(', ') + '.'
		}
	}
	return result
}

fn short_module_name(name string) string {
	if !name.contains('.') {
		return name
	}
	values := name.split('.')
	if values.len < 2 {
		return name
	}
	return '${values[values.len - 2]}.${values.last()}'
}

fn highlight_suggestion(message string) string {
	return ansi.bright_blue_stderr(message)
}
