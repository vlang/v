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

// skip_comment returns the offset just past the comment that starts at `at`,
// or `at` when no complete comment starts there. It is only called outside a string.
fn skip_comment(text string, at int) int {
	if at + 1 >= text.len || text[at] != `/` {
		return at
	}
	if text[at + 1] == `/` {
		mut i := at + 2
		for i < text.len && text[i] != `\n` {
			i++
		}
		return i
	}
	if text[at + 1] == `*` {
		mut i := at + 2
		for i + 1 < text.len {
			if text[i] == `*` && text[i + 1] == `/` {
				return i + 2
			}
			i++
		}
		return at
	}
	return at
}

// strip_comments returns the text with every comment removed. This is how a
// commented file is judged: if what is left is plain JSON, the file is one this
// tool can edit, and the comments around the edit are preserved because the
// edit is textual.
fn strip_comments(text string) string {
	mut out := []u8{}
	mut in_string := false
	mut escaped := false
	mut i := 0
	for i < text.len {
		c := text[i]
		if in_string {
			out << c
			if escaped {
				escaped = false
			} else if c == bslash {
				escaped = true
			} else if c == dquote {
				in_string = false
			}
			i++
			continue
		}
		if c == dquote {
			in_string = true
			out << c
			i++
			continue
		}
		if c == `/` {
			end := skip_comment(text, i)
			if end > i {
				// One space keeps whatever was on either side of the comment
				// from ending up joined.
				out << sp
				i = end
				continue
			}
		}
		out << c
		i++
	}
	return out.bytestr()
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
		if !in_string && c == `/` {
			comment_end := skip_comment(text, i)
			if comment_end > i {
				i = comment_end
				continue
			}
		}
		if in_string {
			if escaped {
				escaped = false
			} else if c == bslash {
				escaped = true
			} else if c == dquote {
				in_string = false
				if depth == 1 && text[token_start..i] == key {
					mut j := skip_space_and_comments(text, i + 1)
					if j < text.len && text[j] == `:` {
						j = skip_space_and_comments(text, j + 1)
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
	start := skip_space_and_comments(text, 0)
	if start >= text.len || text[start] != `{` {
		return none
	}
	return insertion_in(text, start, matching_bracket(text, start))
}

// insertion_in is `insertion_point` for the object that spans start..end.
fn insertion_in(text string, start int, end int) Insertion {
	j := skip_space_and_comments(text, start + 1)
	has_leading_comments := j != skip_space(text, start + 1)
	if has_leading_comments {
		outer := line_indent(text, start)
		return Insertion{
			pos:    start + 1
			end:    start + 1
			prefix: '\n' + outer + '  '
			suffix: if j < end && text[j] == `}` { '\n' + outer } else { ',\n' + outer }
		}
	}
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
// value, and a following or preceding comma's offset. When there is no comma,
// the third offset is the entry's start.
fn find_entry(text string, start int, end int, id string) ?(int, int, int) {
	mut depth := 0
	mut in_string := false
	mut escaped := false
	mut token_start := 0
	mut i := start
	for i < end {
		c := text[i]
		if !in_string && c == `/` {
			comment_end := skip_comment(text, i)
			if comment_end > i {
				i = comment_end
				continue
			}
		}
		if in_string {
			if escaped {
				escaped = false
			} else if c == bslash {
				escaped = true
			} else if c == dquote {
				in_string = false
				if depth == 1 && text[token_start..i] == id {
					colon := skip_space_and_comments(text, i + 1)
					// A string not followed by a colon is a value, not a key, so
					// the search goes on past it.
					if colon < end && text[colon] == `:` {
						entry_start := token_start - 1
						stop := value_end(text, skip_space_and_comments(text, colon + 1), end)
						k := skip_space_and_comments(text, stop)
						if k < end && text[k] == `,` {
							return entry_start, stop, k
						}
						// The last entry, so the comma before it has to go too.
						return entry_start, stop, previous_comma(text, entry_start, start)
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
fn value_end(text string, start int, end int) int {
	mut i := start
	// A comment can sit between the colon and the value.
	for i < end && text[i] == `/` {
		comment_end := skip_comment(text, i)
		if comment_end == i {
			break
		}
		i = skip_space(text, comment_end)
	}
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
	for j < end && text[j] != `,` && text[j] != `}` && !is_space(text[j]) && text[j] != `/` {
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
		if !in_string && c == `/` {
			comment_end := skip_comment(text, j)
			if comment_end > j {
				j = comment_end
				continue
			}
		}
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
	mut j := floor
	mut comma := i
	mut depth := 0
	mut in_string := false
	mut escaped := false
	for j < i {
		c := text[j]
		if !in_string && c == `/` {
			end := skip_comment(text, j)
			if end > j {
				j = end
				continue
			}
		}
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
		} else if c == `,` && depth == 1 {
			comma = j
		}
		j++
	}
	return comma
}

fn skip_space(text string, i int) int {
	mut j := i
	for j < text.len && is_space(text[j]) {
		j++
	}
	return j
}

// skip_space_and_comments skips whitespace and any comments, which is what a
// scan needs when a comment can sit between a colon and the value that follows
// it.
fn skip_space_and_comments(text string, i int) int {
	mut j := skip_space(text, i)
	for j < text.len && text[j] == `/` {
		end := skip_comment(text, j)
		if end == j {
			break
		}
		j = skip_space(text, end)
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
	// The scan skips comments, so a commented-out entry is not mistaken for a
	// real one. A file that is not JSON at all is still left alone.
	if !is_editable(text) {
		if text.contains(json_string(server_id)) {
			return error('${path} is not JSON this tool can edit; remove the ${json_string(server_id)} entry by hand.')
		}
		return false
	}
	start, end := object_span(text, h.key) or { return false }
	entry_start, stop, comma := find_entry(text, start, end, server_id) or { return false }
	edited := if is_plain_json(text) {
		// Preserve existing spacing for plain JSON configurations.
		cut_from := if comma < entry_start { comma } else { entry_start }
		cut_to := if comma >= stop { skip_space(text, comma + 1) } else { stop }
		text[..cut_from] + text[cut_to..]
	} else if comma >= stop {
		text[..entry_start] + text[stop..comma] + text[comma + 1..]
	} else if comma < entry_start {
		text[..comma] + text[comma + 1..entry_start] + text[stop..]
	} else {
		text[..entry_start] + text[stop..]
	}
	write_atomically(path, edited) or {
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
