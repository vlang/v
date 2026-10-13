module shlex

// Byte codes used below, to keep the lexer readable without char-literal
// escaping: 9 tab, 10 LF, 13 CR, 32 space, 34 double quote, 36 dollar,
// 39 single quote, 92 backslash.

// split breaks s into shell-like words: single and double quotes group,
// backslash escapes the next character (newline continues the line), and
// blank input yields no words. An empty quoted string yields one empty word.
// `#` comments are deliberately unsupported (shells disagree on them outside
// interactive use). Unbalanced quotes and a trailing backslash are errors.
// It is the V-idiomatic equivalent of Python's `shlex.split`, and the
// companion to `os.Command` / `os.execute`.
pub fn split(s string) ![]string {
	b := s.bytes()
	mut out := []string{}
	mut cur := []u8{}
	mut quote := u8(0)
	mut has_token := false
	mut i := 0
	for i < b.len {
		c := b[i]
		if quote == 39 {
			// inside single quotes everything is literal
			if c == 39 {
				quote = 0
			} else {
				cur << c
			}
			i++
			continue
		}
		if quote == 34 {
			if c == 34 {
				quote = 0
				i++
				continue
			}
			if c == 92 && i + 1 < b.len {
				n := b[i + 1]
				if n == 36 || n == 34 || n == 92 || n == 10 {
					if n != 10 {
						cur << n
					}
					i += 2
					continue
				}
			}
			cur << c
			i++
			continue
		}
		if c == 32 || c == 9 || c == 10 || c == 13 {
			if has_token {
				out << cur.bytestr()
				cur = []u8{}
				has_token = false
			}
			i++
			continue
		}
		if c == 39 || c == 34 {
			quote = c
			has_token = true
			i++
			continue
		}
		if c == 92 {
			if i + 1 >= b.len {
				return error('shlex.split: trailing backslash')
			}
			i++
			if b[i] == 10 {
				// line continuation: drop both characters
				i++
				continue
			}
			cur << b[i]
			has_token = true
			i++
			continue
		}
		cur << c
		has_token = true
		i++
	}
	if quote != 0 {
		return error('shlex.split: unbalanced quote')
	}
	if has_token {
		out << cur.bytestr()
	}
	return out
}
