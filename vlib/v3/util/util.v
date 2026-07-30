module util

import os
import strings
import v3.ansi

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

// githash returns the current seven-character Git commit hash for path.
pub fn githash(path string) !string {
	head_file := os.join_path(path, '.git', 'HEAD')
	if !os.exists(head_file) {
		return error('failed to find `${head_file}`')
	}
	head_content := os.read_file(head_file) or { return error('failed to read `${head_file}`') }
	hash := if head_content.starts_with('ref: ') {
		revision_path := os.join_path(path, '.git', head_content[5..].trim_space())
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
		known:                []Possibility{cap: int(max_suggestions_limit)}
		wanted:               wanted
		swanted:              short_module_name(wanted)
		similarity_threshold: params.similarity_threshold
		similarity_fn:        params.similarity_fn
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
		value:      value
		svalue:     short_value
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
