module document

const highlight_keys = ['note:', 'fixme:', 'todo:']
const horizontal_rule_chars = ['-', '=', '*', '_', '~']

// merge_doc_comments merges a contiguous group of documentation comments.
pub fn merge_doc_comments(comments []DocComment) string {
	if comments.len == 0 {
		return ''
	}
	if raw_markdown := merge_raw_markdown_comments(comments) {
		return raw_markdown
	}
	return merge_comment_lines(last_comment_block(comments))
}

// last_comment_block returns the single line comments of the last group of `comments`
// that are on consecutive lines.
fn last_comment_block(comments []DocComment) []DocComment {
	mut block := []DocComment{}
	for i := comments.len - 1; i >= 0; i-- {
		if comments[i].is_multi {
			continue
		}
		block << comments[i]
		if cmt_above := comments[i - 1] {
			if cmt_above.pos.line_nr + 1 < comments[i].pos.line_nr {
				break
			}
		}
	}
	return block.reverse()
}

// merge_comment_lines merges the lines of a group of documentation comments into markdown.
fn merge_comment_lines(comments []DocComment) string {
	mut comment := ''
	mut next_on_newline := true
	mut is_codeblock := false
	mut trimmed_indent := ''
	for cmt in comments.map(it.text) {
		line_loop: for line in cmt.split_into_lines() {
			l_normalized := line.trim_left('\x01')
			l := l_normalized.trim_space()
			last_ends_with_lb := comment.ends_with('\n')
			if l == '' {
				comment += if last_ends_with_lb { '\n' } else { '\n\n' }
				next_on_newline = true
				continue
			}
			has_codeblock_quote := l.starts_with('```')
			if is_codeblock {
				comment += l_normalized.trim_string_left(trimmed_indent) + '\n'
				if has_codeblock_quote {
					is_codeblock = false
				}
				continue
			}
			if has_codeblock_quote {
				if !last_ends_with_lb {
					comment += '\n'
				}
				comment += l + '\n'
				is_codeblock = true
				trimmed_indent = l_normalized.all_before(l)
				next_on_newline = true
				continue
			}
			if l.starts_with('>') {
				if !last_ends_with_lb {
					comment += '\n'
				}
				comment += l + '\n'
				next_on_newline = true
				continue
			}
			is_list := l.len > 1 && ((l[1] == ` ` && l[0] in [`-`, `*`, `+`])
				|| (l.len > 2 && l[2] == ` ` && l[1] == `.` && l[0].is_digit()))
			line_before_spaces := l.before(' ')
			if is_list || (l.starts_with('|') && l.ends_with('|'))
				|| (l.starts_with('#') && line_before_spaces.count('#') == line_before_spaces.len) {
				if comment != '' && !last_ends_with_lb {
					comment += '\n'
				}
				comment += l + '\n'
				next_on_newline = true
				continue
			}
			lower := l.to_lower_ascii()
			mut highlighted := false
			for key in highlight_keys {
				if lower.starts_with(key) {
					comment += '\n\n${key.title()}${l[key.len..]}'
					highlighted = true
					break
				}
			}
			if highlighted {
				continue
			}
			line_no_spaces := l.replace(' ', '')
			for ch in horizontal_rule_chars {
				if line_no_spaces.starts_with(ch.repeat(3))
					&& line_no_spaces.count(ch) == line_no_spaces.len {
					comment += '\n' + l + '\n'
					next_on_newline = true
					continue line_loop
				}
			}
			if !next_on_newline {
				comment += ' '
			}
			comment += l
			next_on_newline = false
		}
	}
	return comment
}

fn merge_raw_markdown_comments(comments []DocComment) ?string {
	if !comments.all(it.is_readme) {
		return none
	}
	mut raw_markdown := []string{}
	for i := comments.len - 1; i >= 0; i-- {
		if comments[i].is_multi {
			continue
		}
		raw_markdown << comments[i].text.trim_left('\x01')
	}
	return raw_markdown.reverse().join('\n')
}
