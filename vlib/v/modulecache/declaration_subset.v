module modulecache

import strings

// A cached module object is compiled from the declaration header of the whole program
// followed by the module's own code. Most of that header is function prototypes and
// extern variables of other modules that the code never names: 40 to 50 percent of the
// bytes for an `import net.http` build, which the C compiler parses and the cache hashes,
// prunes and copies once for every module.
//
// DeclarationIndex holds the lines of one declaration header, built once, and `subset`
// gives the lines a given module code needs. Only lines that this file can tell, with a
// reason, are one self contained declaration of a function or of a variable are ever left
// out: everything else (preprocessor lines, types, `static` functions, multi-line
// declarations, anything with a brace or a string, anything it is unsure about) stays, in
// its place. A declaration is left out when its name is neither in the module code nor in
// any line that stays: macros and `static` functions of the header that call it keep it.

// DeclarationIndex is a declaration header, split into units (c_source_units: a line of C,
// or all the lines of one generated string literal definition), with the name that each
// droppable unit declares.
pub struct DeclarationIndex {
pub:
	units           []string
	declared        []string        // per unit: the name it declares when it may be left out, else ''
	literal         []string        // per unit: the generated string literal it defines, else ''
	kept            map[string]bool // every identifier of every unit that is never left out
	total_bytes     int
	droppable_bytes int // the declarations and the literal definitions that can go
	// subset copies runs of the joined text, not unit by unit: only the units that can go
	// are looked at, and they are a fraction of all.
	joined  string
	starts  []int // per unit: where it starts in joined
	drop_at []int // the units that can go, in order
}

const droppable_prefix_keywords = ['typedef', 'return', 'else', 'goto', 'case', 'default', 'do',
	'while', 'for', 'if', 'switch', 'break', 'continue', 'sizeof', 'struct', 'union', 'enum', 'inline',
	'static', '__attribute__', '__declspec', '_Static_assert', 'asm', '__asm__']

// new_declaration_index splits `header` and decides, unit by unit, what may be left out.
pub fn new_declaration_index(header string) DeclarationIndex {
	source_units := c_source_units(header)
	mut units := []string{cap: source_units.len}
	mut declared := []string{len: source_units.len}
	mut literal := []string{len: source_units.len}
	mut kept := map[string]bool{}
	mut depth := 0
	mut in_block_comment := false
	mut in_directive := false
	mut droppable_bytes := 0
	for i, unit in source_units {
		line := unit.text
		units << line
		if unit.symbol != '' {
			// Generated string storage: the unit stays when its symbol is named by the code
			// or by a unit that stays. What it names itself stays too (it is rare).
			literal[i] = unit.symbol
			droppable_bytes += line.len + 1
			for symbol in generated_string_symbols(line) {
				if symbol != unit.symbol {
					kept[symbol] = true
				}
			}
			continue
		}
		line_depth := depth
		was_in_code := !in_block_comment && !in_directive
		trimmed := line.trim_space()
		mut is_directive_line := in_directive
		if !in_block_comment && !in_directive && trimmed.starts_with('#') {
			is_directive_line = true
		}
		if is_directive_line {
			in_directive = line.trim_right(' \t\r').ends_with('\\')
			collect_identifiers(line, mut kept)
			continue
		}
		depth, in_block_comment = scan_c_line(line, depth, in_block_comment)
		name := if was_in_code && line_depth == 0 && depth == 0 {
			droppable_declaration_name(line)
		} else {
			''
		}
		if name.len > 0 {
			declared[i] = name
			droppable_bytes += line.len + 1
		} else {
			collect_identifiers(line, mut kept)
		}
	}
	// every unit followed by its newline, so that a unit that goes takes its newline with it
	joined := if units.len == 0 { '' } else { units.join('\n') + '\n' }
	mut starts := []int{len: units.len}
	mut drop_at := []int{}
	mut offset := 0
	for i, unit in units {
		starts[i] = offset
		offset += unit.len + 1
		if declared[i].len > 0 || literal[i].len > 0 {
			drop_at << i
		}
	}
	return DeclarationIndex{
		units:           units
		declared:        declared
		literal:         literal
		kept:            kept
		total_bytes:     header.len
		droppable_bytes: droppable_bytes
		joined:          joined
		starts:          starts
		drop_at:         drop_at
	}
}

// subset returns the header without the declarations and string literal definitions that
// neither `code` nor the units that stay mention. The order of the units that stay is the
// header's.
pub fn (idx &DeclarationIndex) subset(code string) string {
	if idx.drop_at.len == 0 {
		return idx.joined
	}
	mut used := map[string]bool{}
	collect_identifiers(code, mut used)
	mut out := strings.new_builder(idx.total_bytes - idx.droppable_bytes + 1024)
	mut run_start := 0
	for i in idx.drop_at {
		name := idx.declared[i]
		symbol := idx.literal[i]
		goes := (name.len > 0 && !used[name] && !idx.kept[name])
			|| (symbol.len > 0 && !used[symbol] && !idx.kept[symbol])
		if !goes {
			continue
		}
		out.write_string(idx.joined[run_start..idx.starts[i]])
		run_start = idx.starts[i] + idx.units[i].len + 1
	}
	if run_start < idx.joined.len {
		out.write_string(idx.joined[run_start..])
	}
	return out.str()
}

// collect_identifiers adds every identifier of `text` to `into`; comments and strings are
// scanned like code, which only makes a declaration stay that could have gone.
fn collect_identifiers(text string, mut into map[string]bool) {
	mut start := -1
	for i := 0; i <= text.len; i++ {
		is_identifier := i < text.len && (text[i].is_alnum() || text[i] == `_`)
		if is_identifier {
			if start < 0 {
				start = i
			}
			continue
		}
		if start >= 0 {
			if !text[start].is_digit() {
				into[unsafe { text[start..i] }] = true
			}
			start = -1
		}
	}
}

// scan_c_line returns the brace depth and the block comment state after `line`, skipping
// string and character literals and comments.
fn scan_c_line(line string, depth_in int, in_comment_in bool) (int, bool) {
	mut depth := depth_in
	mut in_comment := in_comment_in
	mut i := 0
	for i < line.len {
		c := line[i]
		if in_comment {
			if c == `*` && i + 1 < line.len && line[i + 1] == `/` {
				in_comment = false
				i += 2
				continue
			}
			i++
			continue
		}
		if c == `/` && i + 1 < line.len && line[i + 1] == `*` {
			in_comment = true
			i += 2
			continue
		}
		if c == `/` && i + 1 < line.len && line[i + 1] == `/` {
			break
		}
		if c == `"` || c == `'` {
			quote := c
			i++
			for i < line.len && line[i] != quote {
				if line[i] == `\\` {
					i++
				}
				i++
			}
			i++
			continue
		}
		if c == `{` {
			depth++
		} else if c == `}` {
			depth--
		}
		i++
	}
	return depth, in_comment
}

// droppable_declaration_name returns the name of the function or variable that `line`
// declares when the line is exactly one such declaration and nothing else, else ''. A
// function prototype is `<type words> name(<anything>);` with the type words made of
// identifiers and `*`; an extern variable is `extern <type words> name;` or
// `extern <type words> name[<anything>];`. A line with a brace, a quote, an `=`, a comment
// or a keyword that makes it something else is not one.
fn droppable_declaration_name(line string) string {
	if line.len == 0 || line[0] == ` ` || line[0] == `\t` || line[0] == `/` {
		return ''
	}
	text := line.trim_right(' \t\r')
	if !text.ends_with(';') {
		return ''
	}
	for c in text {
		if c == `{` || c == `}` || c == `"` || c == `'` || c == `=` || c == `#` {
			return ''
		}
	}
	if text.contains('/*') || text.contains('//') {
		return ''
	}
	paren := text.index_u8(`(`)
	if paren < 0 {
		// extern <type words> name;   or   extern <type words> name[...];
		if !text.starts_with('extern ') {
			return ''
		}
		mut end := text.len - 1 // the ';'
		if end > 0 && text[end - 1] == `]` {
			bracket := text.last_index_u8(`[`)
			if bracket < 0 {
				return ''
			}
			end = bracket
		}
		return declared_name_before(text, end)
	}
	// name(<anything>);  with the matching ')' the last character before ';'
	close := matching_paren(text, paren)
	if close != text.len - 2 {
		return ''
	}
	return declared_name_before(text, paren)
}

// matching_paren returns the index of the ')' that closes the '(' at `open`, or -1.
fn matching_paren(text string, open int) int {
	mut depth := 0
	for i := open; i < text.len; i++ {
		if text[i] == `(` {
			depth++
		} else if text[i] == `)` {
			depth--
			if depth == 0 {
				return i
			}
		}
	}
	return -1
}

// declared_name_before returns the identifier that ends just before `end` (spaces
// allowed between) when everything in front of it is type words: identifiers, spaces and
// `*`, at least one, none of them a keyword that starts another kind of line.
fn declared_name_before(text string, end int) string {
	mut e := end
	for e > 0 && text[e - 1] == ` ` {
		e--
	}
	mut s := e
	for s > 0 && (text[s - 1].is_alnum() || text[s - 1] == `_`) {
		s--
	}
	if s >= e || text[s].is_digit() {
		return ''
	}
	prefix := text[..s]
	mut words := 0
	mut in_word := false
	for c in prefix {
		if c.is_alnum() || c == `_` {
			if !in_word {
				words++
				in_word = true
			}
		} else if c == ` ` || c == `*` {
			in_word = false
		} else {
			return ''
		}
	}
	if words == 0 {
		return ''
	}
	for word in prefix.replace('*', ' ').split(' ') {
		if word in droppable_prefix_keywords {
			return ''
		}
	}
	return text[s..e]
}
