module main

import os

// This file edits a JSON config file without rewriting it.
//
// A round trip through `json.decode` and `json.encode` would reorder every key,
// because V maps are unordered, and would drop anything the decoder does not
// model. So the work here is textual: find the one object that has to grow,
// insert next to its opening brace, and leave every other byte alone.

// The bytes this file scans for, as numbers rather than literals: a backtick
// char literal cannot hold a backslash, and every other error in the file
// cascades from that one.
const dquote = u8(34)
const bslash = u8(92)
const sp = u8(32)

// is_space is JSON whitespace: space, tab, newline, carriage return.
fn is_space(c u8) bool {
	return c == sp || c == 9 || c == 10 || c == 13
}

// object_span returns the byte range of the value of the top-level `key`, where
// the value is an object.
fn object_span(text string, key string) ?(int, int) {
	mut depth := 0
	mut in_string := false
	mut escaped := false
	mut token_start := 0
	mut i := 0
	for i < text.len {
		c := text[i]
		if in_string {
			if escaped {
				escaped = false
			} else if c == bslash {
				escaped = true
			} else if c == dquote {
				in_string = false
				if depth == 1 && text[token_start..i] == key {
					mut j := skip_space(text, i + 1)
					if j < text.len && text[j] == `:` {
						j = skip_space(text, j + 1)
						if j < text.len && text[j] == `{` {
							return j, matching_bracket(text, j)
						}
					}
				}
			}
		} else if c == dquote {
			in_string = true
			token_start = i + 1
		} else if c == `{` {
			depth++
		} else if c == `}` {
			depth--
		}
		i++
	}
	return none
}

// insertion_point is where a new entry goes inside the object at `key`, and the
// text that has to surround it there. Reusing the line and the indentation of
// the entry that is already there is what keeps a hand-arranged file reading
// like something a person wrote.
fn insertion_point(text string, key string) ?Insertion {
	start, end := object_span(text, key) or { return none }
	return insertion_in(text, start, end)
}

// root_insertion is where a new top-level member goes: first in the root object.
fn root_insertion(text string) ?Insertion {
	start := skip_space(text, 0)
	if start >= text.len || text[start] != `{` {
		return none
	}
	return insertion_in(text, start, matching_bracket(text, start))
}

// insertion_in is `insertion_point` for the object that spans start..end.
fn insertion_in(text string, start int, end int) Insertion {
	j := skip_space(text, start + 1)
	if j < end && text[j] == `}` {
		// An empty object has no entry to copy its layout from, so whatever
		// whitespace sits between its braces is replaced by a body indented one
		// step under the line that opens it.
		outer := line_indent(text, start)
		return Insertion{
			pos:    start + 1
			end:    j
			prefix: '\n' + outer + '  '
			suffix: '\n' + outer
		}
	}
	closing := line_start(text, j)
	if closing <= start {
		// The body opens on the brace's own line, so it stays a one-liner.
		return Insertion{
			pos:    start + 1
			end:    start + 1
			prefix: ' '
			suffix: ','
			inline: true
		}
	}
	return Insertion{
		pos:    closing
		end:    closing
		prefix: text[closing..j]
		suffix: ',\n'
	}
}

// line_start is the offset just past the newline before `at`, or 0 when there is
// none.
fn line_start(text string, at int) int {
	mut i := at
	for i > 0 && text[i - 1] != `\n` {
		i--
	}
	return i
}

// line_indent is the leading whitespace of the line `at` sits on, which is the
// indentation a new entry belongs at.
fn line_indent(text string, at int) string {
	line := line_start(text, at)
	mut i := line
	for i < text.len && (text[i] == sp || text[i] == 9) {
		i++
	}
	return text[line..i]
}

// has_entry reports whether `id` is already a key of the object at `key`.
fn has_entry(text string, key string, id string) bool {
	start, end := object_span(text, key) or { return false }
	find_entry(text, start, end, id) or { return false }
	return true
}

// find_entry locates `"id"` at one level inside the object spanning start..end.
// It returns the start of the whole `"id": value` pair, the offset just past the
// value, and the offset to cut to: past a following comma when there is one,
// otherwise past the value so the preceding comma is taken instead.
fn find_entry(text string, start int, end int, id string) ?(int, int, int) {
	mut depth := 0
	mut in_string := false
	mut escaped := false
	mut token_start := 0
	mut i := start
	for i < end {
		c := text[i]
		if in_string {
			if escaped {
				escaped = false
			} else if c == bslash {
				escaped = true
			} else if c == dquote {
				in_string = false
				if depth == 1 && text[token_start..i] == id {
					colon := skip_space(text, i + 1)
					// A string not followed by a colon is a value, not a key, so
					// the search goes on past it.
					if colon < end && text[colon] == `:` {
						entry_start := token_start - 1
						stop := value_end(text, skip_space(text, colon + 1), end)
						k := skip_space(text, stop)
						if k < end && text[k] == `,` {
							return entry_start, stop, skip_space(text, k + 1)
						}
						// The last entry, so the comma before it has to go too.
						return previous_comma(text, entry_start, start), stop, stop
					}
				}
			}
		} else if c == dquote {
			in_string = true
			token_start = i + 1
		} else if c == `{` || c == `[` {
			depth++
		} else if c == `}` || c == `]` {
			depth--
		}
		i++
	}
	return none
}

// value_end returns the offset just past the value that starts at `i`.
fn value_end(text string, i int, end int) int {
	if i >= end {
		return i
	}
	c := text[i]
	if c == `{` || c == `[` {
		return matching_bracket(text, i)
	}
	if c == dquote {
		mut j := i + 1
		for j < end {
			if text[j] == bslash {
				j += 2
				continue
			}
			if text[j] == dquote {
				return j + 1
			}
			j++
		}
		return end
	}
	mut j := i
	for j < end && text[j] != `,` && text[j] != `}` {
		j++
	}
	return j
}

fn matching_bracket(text string, i int) int {
	mut depth := 0
	mut in_string := false
	mut escaped := false
	mut j := i
	for j < text.len {
		c := text[j]
		if in_string {
			if escaped {
				escaped = false
			} else if c == bslash {
				escaped = true
			} else if c == dquote {
				in_string = false
			}
		} else if c == dquote {
			in_string = true
		} else if c == `{` || c == `[` {
			depth++
		} else if c == `}` || c == `]` {
			depth--
			if depth == 0 {
				return j + 1
			}
		}
		j++
	}
	return text.len
}

// previous_comma returns the offset of the comma before `i`, or `i` when the
// entry is the first one and there is nothing to take.
fn previous_comma(text string, i int, floor int) int {
	mut j := i - 1
	for j > floor {
		if text[j] == `,` {
			return j
		}
		j--
	}
	return i
}

fn skip_space(text string, i int) int {
	mut j := i
	for j < text.len && is_space(text[j]) {
		j++
	}
	return j
}

// remove_entry deletes the entry from the client's file. It reports whether
// there was one to delete; an error means there may be one that was left in
// place, and says why.
fn remove_entry(h Harness, path string) !bool {
	if !os.exists(path) {
		return false
	}
	text := os.read_file(path) or { return error('could not read ${path}: ${err.msg()}') }
	// The scan below does not know where comments are, so in a file that has
	// them a commented-out entry looks real and a real one can be cut together
	// with the comment beside it. Such a file is left for the user to edit.
	if !is_plain_json(text) {
		if text.contains(json_string(server_id)) {
			return error('${path} is not plain JSON (comments or trailing commas); not rewriting it. Remove the ${json_string(server_id)} entry by hand.')
		}
		return false
	}
	start, end := object_span(text, h.key) or { return false }
	cut_from, _, cut_to := find_entry(text, start, end, server_id) or { return false }
	write_atomically(path, text[..cut_from] + text[cut_to..]) or {
		return error('could not write ${path}: ${err.msg()}')
	}
	println('${h.label}: removed ${server_id} from ${path}')
	return true
}

// write_atomically replaces the file in one step: the new text goes to a
// temporary file beside it, which is then renamed over it, so a crash or a full
// disk leaves the old config in place rather than half of the new one.
fn write_atomically(path string, text string) ! {
	// A symlinked dotfile stays a symlink: the file it points at is replaced.
	target := if os.exists(path) { os.real_path(path) } else { path }
	tmp := '${target}.vmcp-${os.getpid()}'
	replace_through(tmp, target, text) or {
		os.rm(tmp) or {}
		return err
	}
}

// replace_through writes `text` to `tmp` and moves it over `target`, keeping
// the permissions `target` had.
fn replace_through(tmp string, target string, text string) ! {
	os.write_file(tmp, text)!
	$if !windows {
		if st := os.stat(target) {
			os.chmod(tmp, int(st.mode & 0o7777))!
		}
	}
	os.rename(tmp, target) or {
		// Windows will not rename over an existing file.
		$if windows {
			if os.exists(target) {
				os.mv(tmp, target, overwrite: true)!
				return
			}
		}
		return err
	}
}
