module cmdline

// Fetch multiple option by param, e.g.
// args: ['v', '-d', 'aa', '-d', 'bb', '-d', 'cc']
// param: '-d'
// ret: ['aa', 'bb', 'cc']
pub fn options(args []string, param string) []string {
	mut flags := []string{}
	for i, v in args {
		if v == param {
			if i + 1 < args.len {
				flags << args[i + 1]
			}
		}
	}
	return flags
}

// Fetch option by param, e.g.
// args: ['v', '-d', 'aa']
// param: '-d'
// def: ''
// ret: 'aa'
pub fn option(args []string, param string, def string) string {
	mut found := false
	for arg in args {
		if found {
			return arg
		} else if param == arg {
			found = true
		}
	}
	return def
}

// Fetch all options before what params, e.g.
// args: ['-stat', 'test', 'aaa.v']
// what: ['test']
// ret: ['-stat']
pub fn options_before(args []string, what []string) []string {
	mut args_before := []string{}
	for a in args {
		if a in what {
			break
		}
		args_before << a
	}
	return args_before
}

// Fetch all options after what params, e.g.
// args: ['-stat', 'test', 'aaa.v']
// what: ['test']
// ret: ['aaa.v']
pub fn options_after(args []string, what []string) []string {
	mut found := false
	mut args_after := []string{}
	for a in args {
		if a in what {
			found = true
			continue
		}
		if found {
			args_after << a
		}
	}
	return args_after
}

// Fetch all options not start with '-', e.g.
// args: ['-d', 'aa', '--help', 'bb']
// ret: ['aa', 'bb']
pub fn only_non_options(args []string) []string {
	return args.filter(!it.starts_with('-'))
}

// Fetch all options start with '-', e.g.
// args: ['-d', 'aa', '--help', 'bb']
// ret: ['-d', '--help']
pub fn only_options(args []string) []string {
	return args.filter(it.starts_with('-'))
}

// Byte codes used in split below, to keep the lexer readable without
// char-literal escaping: 9 tab, 10 LF, 13 CR, 32 space, 34 double quote,
// 36 dollar, 39 single quote, 92 backslash, 96 backtick.

// split breaks s into shell-like words, e.g.
// s: 'v run "my prog.v" --flag=value'
// ret: ['v', 'run', 'my prog.v', '--flag=value']
// Single and double quotes group, backslash escapes the next character
// (newline continues the line), and blank input yields no words. An empty
// quoted string yields one empty word. `#` comments are deliberately
// unsupported (shells disagree on them outside interactive use). Unbalanced
// quotes and a trailing backslash are errors.
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
				if n == 36 || n == 34 || n == 92 || n == 96 || n == 10 {
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
				return error('cmdline.split: trailing backslash')
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
		return error('cmdline.split: unbalanced quote')
	}
	if has_token {
		out << cur.bytestr()
	}
	return out
}
