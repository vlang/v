module c

import strings

// MSVC's C compiler rejects several GNU C extensions that the generated C relies on.
// msvc_compat_c_source rewrites a complete generated translation unit so `cl` accepts it:
//
// * GNU statement expressions `({ ...; value; })` are hoisted into ordinary statements
//   before the statement that uses them. Their locals are renamed to unique names, so
//   hoisting cannot collide with or shadow other declarations. Operands that are only
//   conditionally evaluated (`&&`, `||`, `?:`, loop conditions, `for` post expressions,
//   `else if` conditions) keep their evaluation semantics through guards and restructured
//   loops.
// * Casts of a struct value to its own struct type (a GNU extension) are dropped.
// * Empty initializers `{}` (C23) become `{0}`.
// * Compound literals used as static initializers become plain brace initializers.
//
// The pass only understands the subset of C that the V C generator emits.

enum MsvcTokKind as u8 {
	ident
	number
	str
	punct
}

struct MsvcTok {
	kind MsvcTokKind
	lead int // start of the whitespace, comments and preprocessor lines before the token
	pos  int
	end  int
}

// MsvcPre is one statement hoisted in front of the statement that needs it.
struct MsvcPre {
	is_decl   bool
	is_static bool
	text      string // the complete statement, including a declaration's initializer
	// A declaration can be split into a hoisted declaration and a guarded assignment:
	decl     string // the declaration without its initializer
	name     string
	init     string
	type_txt string
	dims     string
}

struct MsvcLowered {
mut:
	pre  []MsvcPre
	text string
}

struct MsvcLowerer {
	src string
mut:
	toks       []MsvcTok
	match_idx  []int
	se_prefix  []int
	dirty      []bool
	dirty_text []string
	uid        int
}

const msvc_decl_start_keywords = ['static', 'const', 'volatile', 'extern', 'register', 'struct',
	'union', 'enum', 'unsigned', 'signed', 'short', 'long', 'int', 'char', 'float', 'double', 'void',
	'_Bool', '_Thread_local', '__declspec', '_Atomic', '_Alignas']

const msvc_stmt_keywords = ['if', 'else', 'while', 'for', 'do', 'switch', 'case', 'default', 'return',
	'break', 'continue', 'goto', 'sizeof']

const msvc_assign_ops = ['=', '+=', '-=', '*=', '/=', '%=', '&=', '|=', '^=', '<<=', '>>=']

// msvc_compat_c_source rewrites generated C source so MSVC's `cl` can compile it.
pub fn msvc_compat_c_source(src string) string {
	struct_names, array_names := msvc_type_names(src)
	mut out := strings.new_builder(src.len + src.len / 16)
	mut region_start := 0
	mut body_start := -1
	mut depth := 0
	mut saw_eq := false
	mut last_sig := u8(0)
	mut line_start := true
	mut uid := 0
	mut i := 0
	n := src.len
	for i < n {
		ch := src[i]
		if ch == `\n` {
			line_start = true
			i++
			continue
		}
		if ch == ` ` || ch == `\t` || ch == `\r` || ch == `\f` || ch == `\v` {
			i++
			continue
		}
		if ch == `#` && line_start {
			i = msvc_skip_directive(src, i, n)
			continue
		}
		line_start = false
		if ch == `/` && i + 1 < n && src[i + 1] == `/` {
			for i < n && src[i] != `\n` {
				i++
			}
			continue
		}
		if ch == `/` && i + 1 < n && src[i + 1] == `*` {
			i = msvc_skip_block_comment(src, i, n)
			continue
		}
		if ch == `"` || ch == `'` {
			i = msvc_skip_quoted(src, i, n)
			last_sig = ch
			continue
		}
		if ch == `{` {
			if depth == 0 && last_sig == `)` && !saw_eq {
				out.write_string(msvc_lower_file_scope(src, region_start, i, struct_names,
					array_names))
				body_start = i
			}
			depth++
		} else if ch == `}` {
			depth--
			if depth == 0 && body_start >= 0 {
				body, next_uid := msvc_lower_function_body(src, body_start, i + 1, struct_names,
					uid)
				out.write_string(body)
				uid = next_uid
				region_start = i + 1
				body_start = -1
				saw_eq = false
			}
		} else if ch == `;` && depth == 0 {
			saw_eq = false
		} else if ch == `=` && depth == 0 {
			saw_eq = true
		}
		last_sig = ch
		i++
	}
	if body_start >= 0 {
		// Unbalanced input: keep the unfinished tail unchanged.
		out.write_string(msvc_lower_file_scope(src, region_start, body_start, struct_names,
			array_names))
		out.write_string(src[body_start..])
	} else {
		out.write_string(msvc_lower_file_scope(src, region_start, n, struct_names, array_names))
	}
	return out.str()
}

// msvc_type_names returns the typedef names of struct types (including aliases of them)
// and of fixed array types, as declared at the start of a line.
fn msvc_type_names(src string) (map[string]bool, map[string]bool) {
	mut structs := map[string]bool{}
	mut arrays := map[string]bool{}
	mut aliases := [][]string{}
	mut pos := 0
	for pos < src.len {
		line_end := src.index_after('\n', pos) or { src.len }
		if src[pos] == `t` && line_end - pos > 8 && src[pos..pos + 8] == 'typedef ' {
			line := src[pos + 8..line_end].trim_space()
			if line.ends_with(';') {
				parts := line[..line.len - 1].fields()
				if parts.len == 3 && parts[0] == 'struct' && msvc_is_ident(parts[2]) {
					structs[parts[2]] = true
				} else if parts.len == 2 && msvc_is_ident(parts[0]) {
					if parts[1].ends_with(']') && parts[1].contains('[') {
						name := parts[1].all_before('[')
						if msvc_is_ident(name) {
							arrays[name] = true
						}
					} else if msvc_is_ident(parts[1]) {
						aliases << [parts[0], parts[1]]
					}
				}
			}
		}
		pos = line_end + 1
	}
	// Aliases can refer to other aliases; resolve them until nothing changes.
	mut changed := true
	for changed {
		changed = false
		for alias in aliases {
			if alias[0] in structs && alias[1] !in structs {
				structs[alias[1]] = true
				changed = true
			}
			if alias[0] in arrays && alias[1] !in arrays {
				arrays[alias[1]] = true
				changed = true
			}
		}
	}
	return structs, arrays
}

fn msvc_is_ident(s string) bool {
	if s.len == 0 || !msvc_is_ident_start(s[0]) {
		return false
	}
	for ch in s {
		if !msvc_is_ident_char(ch) {
			return false
		}
	}
	return true
}

@[inline]
fn msvc_is_ident_start(ch u8) bool {
	return (ch >= `a` && ch <= `z`) || (ch >= `A` && ch <= `Z`) || ch == `_` || ch == `$`
}

@[inline]
fn msvc_is_ident_char(ch u8) bool {
	return msvc_is_ident_start(ch) || (ch >= `0` && ch <= `9`)
}

fn msvc_skip_directive(src string, start int, n int) int {
	mut i := start
	for i < n {
		if src[i] == `\\` && i + 1 < n && src[i + 1] == `\n` {
			i += 2
			continue
		}
		if src[i] == `\\` && i + 2 < n && src[i + 1] == `\r` && src[i + 2] == `\n` {
			i += 3
			continue
		}
		if src[i] == `\n` {
			break
		}
		if src[i] == `/` && i + 1 < n && src[i + 1] == `*` {
			i = msvc_skip_block_comment(src, i, n)
			continue
		}
		i++
	}
	return i
}

fn msvc_skip_block_comment(src string, start int, n int) int {
	mut i := start + 2
	for i + 1 < n && !(src[i] == `*` && src[i + 1] == `/`) {
		i++
	}
	return if i + 2 <= n { i + 2 } else { n }
}

fn msvc_skip_quoted(src string, start int, n int) int {
	quote := src[start]
	mut i := start + 1
	for i < n {
		ch := src[i]
		if ch == `\\` {
			i += 2
			continue
		}
		if ch == quote || ch == `\n` {
			return i + 1
		}
		i++
	}
	return n
}

const msvc_puncts3 = ['<<=', '>>=', '...']
const msvc_puncts2 = ['->', '++', '--', '<<', '>>', '<=', '>=', '==', '!=', '&&', '||', '*=', '/=',
	'%=', '+=', '-=', '&=', '^=', '|=', '##']

fn msvc_tokenize(src string, start int, end int) []MsvcTok {
	mut toks := []MsvcTok{cap: (end - start) / 5 + 4}
	mut i := start
	mut lead := start
	mut line_start := i == 0 || src[i - 1] == `\n`
	for i < end {
		ch := src[i]
		if ch == `\n` {
			line_start = true
			i++
			continue
		}
		if ch == ` ` || ch == `\t` || ch == `\r` || ch == `\f` || ch == `\v` {
			i++
			continue
		}
		if ch == `\\` && i + 1 < end && src[i + 1] == `\n` {
			i += 2
			continue
		}
		if ch == `#` && line_start {
			i = msvc_skip_directive(src, i, end)
			continue
		}
		if ch == `/` && i + 1 < end && src[i + 1] == `/` {
			for i < end && src[i] != `\n` {
				i++
			}
			continue
		}
		if ch == `/` && i + 1 < end && src[i + 1] == `*` {
			i = msvc_skip_block_comment(src, i, end)
			continue
		}
		line_start = false
		tok_start := i
		mut kind := MsvcTokKind.punct
		if msvc_is_ident_start(ch) {
			for i < end && msvc_is_ident_char(src[i]) {
				i++
			}
			kind = .ident
			if i < end && (src[i] == `"` || src[i] == `'`)
				&& src[tok_start..i] in ['L', 'u', 'U', 'u8'] {
				i = msvc_skip_quoted(src, i, end)
				kind = .str
			}
		} else if (ch >= `0` && ch <= `9`) || (ch == `.` && i + 1 < end && src[i + 1] >= `0`
			&& src[i + 1] <= `9`) {
			i++
			for i < end {
				c := src[i]
				if msvc_is_ident_char(c) || c == `.` {
					i++
					continue
				}
				if (c == `+` || c == `-`) && src[i - 1] in [`e`, `E`, `p`, `P`] {
					i++
					continue
				}
				break
			}
			kind = .number
		} else if ch == `"` || ch == `'` {
			i = msvc_skip_quoted(src, i, end)
			kind = .str
		} else if i + 3 <= end && src[i..i + 3] in msvc_puncts3 {
			i += 3
		} else if i + 2 <= end && src[i..i + 2] in msvc_puncts2 {
			i += 2
		} else {
			i++
		}
		toks << MsvcTok{
			kind: kind
			lead: lead
			pos:  tok_start
			end:  i
		}
		lead = i
	}
	return toks
}

// msvc_lower_file_scope rewrites the declarations between function bodies: static
// initializers may not use compound literals or empty initializer lists in MSVC.
fn msvc_lower_file_scope(src string, start int, end int, struct_names map[string]bool, array_names map[string]bool) string {
	if start >= end {
		return ''
	}
	region := src[start..end]
	if !region.contains('{') {
		return region
	}
	toks := msvc_tokenize(src, start, end)
	mut dirty := []bool{len: toks.len}
	mut dirty_text := []string{len: toks.len}
	mut any := false
	for k := 0; k < toks.len; k++ {
		t := src[toks[k].pos..toks[k].end]
		if t == '(' && k + 3 < toks.len && k > 0 && toks[k + 1].kind == .ident
			&& src[toks[k + 2].pos..toks[k + 2].end] == ')'
			&& src[toks[k + 3].pos..toks[k + 3].end] == '{' {
			name := src[toks[k + 1].pos..toks[k + 1].end]
			prev := src[toks[k - 1].pos..toks[k - 1].end]
			if (name in struct_names || name in array_names) && prev in ['=', ',', '{'] {
				for d in k .. k + 3 {
					dirty[d] = true
					dirty_text[d] = ''
				}
				any = true
			}
		}
		if t == '{' && k > 0 && k + 1 < toks.len && src[toks[k + 1].pos..toks[k + 1].end] == '}' {
			prev := src[toks[k - 1].pos..toks[k - 1].end]
			if prev in ['=', ',', '{', ')'] {
				dirty[k] = true
				dirty_text[k] = '{0'
				any = true
			}
		}
	}
	if !any {
		return region
	}
	mut sb := strings.new_builder(region.len + 16)
	mut seg_start := start
	for k, tok in toks {
		if dirty[k] {
			sb.write_string(src[seg_start..tok.pos])
			sb.write_string(dirty_text[k])
			seg_start = tok.end
		}
	}
	sb.write_string(src[seg_start..end])
	return sb.str()
}

// msvc_lower_function_body lowers one function body. `uid` numbers the renamed locals;
// the next free number is returned with the body.
fn msvc_lower_function_body(src string, start int, end int, struct_names map[string]bool, uid int) (string, int) {
	toks := msvc_tokenize(src, start, end)
	if toks.len < 2 {
		return src[start..end], uid
	}
	mut l := MsvcLowerer{
		src:  src
		toks: toks
		uid:  uid
	}
	if !l.prepare(struct_names) {
		return src[start..end], uid
	}
	mut sb := strings.new_builder(end - start + (end - start) / 4)
	last := toks.len - 1
	l.emit_range(mut sb, 0, 1, true)
	l.emit_block_items(mut sb, 1, last)
	l.emit_range(mut sb, last, toks.len, true)
	sb.write_string(src[toks[last].end..end])
	return sb.str(), l.uid
}

@[inline]
fn (l &MsvcLowerer) text(k int) string {
	return l.src[l.toks[k].pos..l.toks[k].end]
}

@[inline]
fn (l &MsvcLowerer) is(k int, s string) bool {
	return k >= 0 && k < l.toks.len && l.toks[k].end - l.toks[k].pos == s.len
		&& l.src[l.toks[k].pos..l.toks[k].end] == s
}

@[inline]
fn (l &MsvcLowerer) lead_text(k int) string {
	return l.src[l.toks[k].lead..l.toks[k].pos]
}

fn (mut l MsvcLowerer) new_name(prefix string) string {
	l.uid++
	return '${prefix}__vmsvc${l.uid}'
}

// prepare computes matching brackets, statement-expression starts and the local rewrites.
// It returns false when the body needs no changes.
fn (mut l MsvcLowerer) prepare(struct_names map[string]bool) bool {
	n := l.toks.len
	l.match_idx = []int{len: n, init: -1}
	l.se_prefix = []int{len: n + 1}
	l.dirty = []bool{len: n}
	l.dirty_text = []string{len: n}
	mut stack := []int{cap: 64}
	for k in 0 .. n {
		if l.toks[k].kind != .punct {
			continue
		}
		t := l.text(k)
		if t == '(' || t == '[' || t == '{' {
			stack << k
		} else if t == ')' || t == ']' || t == '}' {
			if stack.len == 0 {
				return false
			}
			open := stack.pop()
			l.match_idx[open] = k
			l.match_idx[k] = open
		}
	}
	if stack.len > 0 {
		return false
	}
	mut changed := false
	mut is_init_brace := []bool{len: n}
	for k in 0 .. n {
		mut se := 0
		if l.is(k, '(') && l.is(k + 1, '{') && l.match_idx[k + 1] == l.match_idx[k] - 1 {
			se = 1
			changed = true
		}
		l.se_prefix[k + 1] = l.se_prefix[k] + se
		if l.toks[k].kind != .punct {
			continue
		}
		if l.is(k, '(') && k + 2 < n && l.toks[k + 1].kind == .ident && l.is(k + 2, ')')
			&& l.text(k + 1) in struct_names && !l.is(k + 3, '{') && l.is_cast_position(k) {
			for d in k .. k + 3 {
				l.dirty[d] = true
				l.dirty_text[d] = ''
			}
			changed = true
		}
		if l.is(k, '{') && k > 0 {
			is_init_brace[k] = l.is_initializer_brace(k, is_init_brace)
			if is_init_brace[k] && l.is(k + 1, '}') {
				l.dirty[k] = true
				l.dirty_text[k] = '{0'
				changed = true
			}
		}
	}
	return changed
}

// is_cast_position reports whether the parenthesized type name at `k` is a cast, not the
// operand of `sizeof` or the argument list of a call.
fn (l &MsvcLowerer) is_cast_position(k int) bool {
	if k == 0 {
		return true
	}
	prev := l.text(k - 1)
	if l.toks[k - 1].kind == .ident {
		return prev in ['return', 'case', 'else', 'do']
	}
	if l.toks[k - 1].kind != .punct {
		return false
	}
	return prev !in [')', ']']
}

fn (l &MsvcLowerer) is_initializer_brace(k int, is_init_brace []bool) bool {
	prev := l.text(k - 1)
	if prev in ['=', ','] {
		return true
	}
	if prev == '{' {
		return is_init_brace[k - 1]
	}
	if prev == ')' {
		open := l.match_idx[k - 1]
		if open <= 0 {
			return false
		}
		before := l.text(open - 1)
		return before !in ['if', 'while', 'for', 'switch']
	}
	return false
}

@[inline]
fn (l &MsvcLowerer) range_has_se(i int, j int) bool {
	return i < j && l.se_prefix[j] - l.se_prefix[i] > 0
}

// emit_range writes tokens [i, j) with their layout, applying the per-token rewrites.
fn (l &MsvcLowerer) emit_range(mut sb strings.Builder, i int, j int, with_lead bool) {
	if i >= j {
		return
	}
	mut seg_start := if with_lead { l.toks[i].lead } else { l.toks[i].pos }
	for k in i .. j {
		if l.dirty[k] {
			sb.write_string(l.src[seg_start..l.toks[k].pos])
			sb.write_string(l.dirty_text[k])
			seg_start = l.toks[k].end
		}
	}
	sb.write_string(l.src[seg_start..l.toks[j - 1].end])
}

fn (l &MsvcLowerer) range_str(i int, j int, with_lead bool) string {
	mut sb := strings.new_builder(64)
	l.emit_range(mut sb, i, j, with_lead)
	return sb.str()
}

// stmt_end returns the index after the statement (or label) that starts at `i`.
fn (l &MsvcLowerer) stmt_end(i int) int {
	n := l.toks.len
	if i >= n {
		return n
	}
	if l.toks[i].kind == .punct {
		if l.is(i, '{') {
			return l.match_idx[i] + 1
		}
		if l.is(i, ';') {
			return i + 1
		}
	} else if l.toks[i].kind == .ident {
		t := l.text(i)
		match t {
			'if' {
				if !l.is(i + 1, '(') {
					return l.scan_to_semicolon(i)
				}
				mut k := l.stmt_end(l.match_idx[i + 1] + 1)
				if l.is(k, 'else') {
					k = l.stmt_end(k + 1)
				}
				return k
			}
			'while', 'for', 'switch' {
				if !l.is(i + 1, '(') {
					return l.scan_to_semicolon(i)
				}
				return l.stmt_end(l.match_idx[i + 1] + 1)
			}
			'do' {
				k := l.stmt_end(i + 1)
				if l.is(k, 'while') && l.is(k + 1, '(') {
					close := l.match_idx[k + 1]
					if l.is(close + 1, ';') {
						return close + 2
					}
					return close + 1
				}
				return k
			}
			'case' {
				mut k := i + 1
				for k < n {
					if l.is(k, '(') || l.is(k, '[') {
						k = l.match_idx[k] + 1
						continue
					}
					if l.is(k, ':') {
						return k + 1
					}
					k++
				}
				return n
			}
			'default' {
				if l.is(i + 1, ':') {
					return i + 2
				}
			}
			else {
				if l.is(i + 1, ':') && t !in msvc_stmt_keywords {
					return i + 2
				}
				if l.is_macro_block_stmt(i) {
					return l.stmt_end(l.match_idx[i + 1] + 1)
				}
			}
		}
	}
	return l.scan_to_semicolon(i)
}

// is_macro_block_stmt reports whether the statement at `i` is a macro that takes a block,
// like `cJSON_ArrayForEach(item, array) { ... }`, which expands to a loop header. C has no
// other statement that starts with a call followed directly by a block.
fn (l &MsvcLowerer) is_macro_block_stmt(i int) bool {
	if l.toks[i].kind != .ident || !l.is(i + 1, '(') || l.text(i) in msvc_stmt_keywords {
		return false
	}
	return l.is(l.match_idx[i + 1] + 1, '{')
}

fn (l &MsvcLowerer) scan_to_semicolon(i int) int {
	n := l.toks.len
	mut k := i
	for k < n {
		if l.toks[k].kind == .punct {
			if l.is(k, '(') || l.is(k, '[') || l.is(k, '{') {
				k = l.match_idx[k] + 1
				continue
			}
			if l.is(k, ';') {
				return k + 1
			}
			if l.is(k, '}') {
				// The enclosing block ends without a terminating semicolon.
				return k
			}
		}
		k++
	}
	return n
}

fn (l &MsvcLowerer) is_label_item(i int) bool {
	if l.toks[i].kind != .ident {
		return false
	}
	t := l.text(i)
	if t == 'case' {
		return true
	}
	if t == 'default' {
		return l.is(i + 1, ':')
	}
	return l.is(i + 1, ':') && t !in msvc_stmt_keywords
}

fn (mut l MsvcLowerer) emit_block_items(mut sb strings.Builder, i int, j int) {
	mut k := i
	mut after_label := false
	for k < j {
		mut e := l.stmt_end(k)
		if e > j || e <= k {
			e = j
		}
		l.emit_stmt(mut sb, k, e, true, after_label)
		after_label = l.is_label_item(k)
		k = e
	}
}

fn (l &MsvcLowerer) pre_text(pre []MsvcPre) string {
	mut parts := []string{cap: pre.len}
	for p in pre {
		parts << p.text
	}
	return parts.join(' ')
}

// write_with_pre writes a statement preceded by its hoisted statements. Outside of a
// block (the body of an `if`, `else` or loop) the result is wrapped into a new block.
fn (l &MsvcLowerer) write_with_pre(mut sb strings.Builder, lead string, pre []MsvcPre, stmt string, in_block bool, after_label bool) {
	sb.write_string(lead)
	if pre.len == 0 {
		sb.write_string(stmt)
		return
	}
	if !in_block {
		sb.write_string('{ ')
	} else if after_label {
		sb.write_string('; ')
	}
	sb.write_string(l.pre_text(pre))
	sb.write_string(' ')
	sb.write_string(stmt.trim_left(' \t'))
	if !in_block {
		sb.write_string(' }')
	}
}

fn (mut l MsvcLowerer) emit_stmt(mut sb strings.Builder, i int, j int, in_block bool, after_label bool) {
	if !l.range_has_se(i, j) {
		l.emit_range(mut sb, i, j, true)
		return
	}
	lead := l.lead_text(i)
	if l.is(i, '{') {
		close := l.match_idx[i]
		sb.write_string(lead)
		sb.write_string('{')
		l.emit_block_items(mut sb, i + 1, close)
		l.emit_range(mut sb, close, close + 1, true)
		if close + 1 < j {
			l.emit_range(mut sb, close + 1, j, true)
		}
		return
	}
	if l.toks[i].kind == .ident {
		match l.text(i) {
			'if' {
				l.emit_if(mut sb, i, j, in_block, after_label)
				return
			}
			'while' {
				l.emit_while(mut sb, i, j, in_block, after_label)
				return
			}
			'do' {
				l.emit_do(mut sb, i, j, in_block, after_label)
				return
			}
			'for' {
				l.emit_for(mut sb, i, j, in_block, after_label)
				return
			}
			'switch' {
				open := i + 1
				close := l.match_idx[open]
				cond := l.lower_expr(open + 1, close)
				mut s := strings.new_builder(64)
				s.write_string('switch')
				s.write_string(l.lead_text(open))
				s.write_string('(')
				s.write_string(l.lead_text(open + 1))
				s.write_string(cond.text)
				s.write_string(l.lead_text(close))
				s.write_string(')')
				l.emit_stmt(mut s, close + 1, j, false, false)
				l.write_with_pre(mut sb, lead, cond.pre, s.str(), in_block, after_label)
				return
			}
			'return' {
				mut end := j
				if l.is(end - 1, ';') {
					end--
				}
				value := l.lower_expr(i + 1, end)
				mut s := strings.new_builder(64)
				s.write_string('return')
				if i + 1 < end {
					s.write_string(l.lead_text(i + 1))
				}
				s.write_string(value.text)
				if end < j {
					l.emit_range(mut s, end, j, true)
				}
				l.write_with_pre(mut sb, lead, value.pre, s.str(), in_block, after_label)
				return
			}
			else {
				if l.is_macro_block_stmt(i) {
					// The macro arguments stay as they are; its block is a loop body.
					close := l.match_idx[i + 1]
					l.emit_range(mut sb, i, close + 1, true)
					l.emit_stmt(mut sb, close + 1, j, false, false)
					return
				}
			}
		}
	}
	mut end := j
	if l.is(end - 1, ';') {
		end--
	}
	value := l.lower_expr(i, end)
	mut s := strings.new_builder(64)
	s.write_string(value.text)
	if end < j {
		l.emit_range(mut s, end, j, true)
	}
	l.write_with_pre(mut sb, lead, value.pre, s.str(), in_block, after_label)
}

fn (mut l MsvcLowerer) emit_if(mut sb strings.Builder, i int, j int, in_block bool, after_label bool) {
	if l.emit_flat_if_chain(mut sb, i, j, in_block, after_label) {
		return
	}
	lead := l.lead_text(i)
	open := i + 1
	close := l.match_idx[open]
	cond := l.lower_expr(open + 1, close)
	mut s := strings.new_builder(128)
	s.write_string('if')
	s.write_string(l.lead_text(open))
	s.write_string('(')
	s.write_string(l.lead_text(open + 1))
	s.write_string(cond.text)
	s.write_string(l.lead_text(close))
	s.write_string(')')
	then_end := l.stmt_end(close + 1)
	l.emit_stmt(mut s, close + 1, then_end, false, false)
	if then_end < j && l.is(then_end, 'else') {
		s.write_string(l.lead_text(then_end))
		s.write_string('else')
		l.emit_stmt(mut s, then_end + 1, j, false, false)
	} else if then_end < j {
		l.emit_range(mut s, then_end, j, true)
	}
	l.write_with_pre(mut sb, lead, cond.pre, s.str(), in_block, after_label)
}

// emit_flat_if_chain lowers an `if`/`else if` chain whose later conditions need hoisted
// statements. Moving each such condition into its `else` branch would nest one block per
// branch, and MSVC rejects blocks nested more than 128 deep, which long `match` chains
// reach. A flag that records the taken branch keeps the chain flat instead.
fn (mut l MsvcLowerer) emit_flat_if_chain(mut sb strings.Builder, i int, j int, in_block bool, after_label bool) bool {
	mut conds := [][]int{}
	mut bodies := [][]int{}
	mut else_start := -1
	mut k := i
	for {
		open := k + 1
		if !l.is(open, '(') {
			return false
		}
		close := l.match_idx[open]
		then_end := l.stmt_end(close + 1)
		conds << [open, close]
		bodies << [close + 1, then_end]
		if then_end < j && l.is(then_end, 'else') {
			if l.is(then_end + 1, 'if') && l.is(then_end + 2, '(') {
				k = then_end + 1
				continue
			}
			else_start = then_end + 1
		}
		break
	}
	mut later_cond_has_se := false
	for cond in conds[1..] {
		if l.range_has_se(cond[0] + 1, cond[1]) {
			later_cond_has_se = true
			break
		}
	}
	if !later_cond_has_se {
		return false
	}
	flag := l.new_name('taken')
	mut s := strings.new_builder(256)
	s.write_string('{ int ${flag} = 0;')
	for idx, cond in conds {
		lowered := l.lower_expr(cond[0] + 1, cond[1])
		if idx > 0 {
			s.write_string(' if (!${flag}) {')
		}
		if lowered.pre.len > 0 {
			s.write_string(' ')
			s.write_string(l.pre_text(lowered.pre))
		}
		s.write_string(' if (${lowered.text}) { ${flag} = 1;')
		l.emit_stmt(mut s, bodies[idx][0], bodies[idx][1], true, false)
		s.write_string(' }')
		if idx > 0 {
			s.write_string(' }')
		}
	}
	if else_start >= 0 {
		s.write_string(' if (!${flag})')
		l.emit_stmt(mut s, else_start, j, false, false)
	}
	s.write_string(' }')
	l.write_with_pre(mut sb, l.lead_text(i), []MsvcPre{}, s.str(), in_block, after_label)
	return true
}

fn (mut l MsvcLowerer) emit_while(mut sb strings.Builder, i int, j int, in_block bool, after_label bool) {
	lead := l.lead_text(i)
	open := i + 1
	close := l.match_idx[open]
	cond := l.lower_expr(open + 1, close)
	mut s := strings.new_builder(128)
	if cond.pre.len == 0 {
		s.write_string('while')
		s.write_string(l.lead_text(open))
		s.write_string('(')
		s.write_string(l.lead_text(open + 1))
		s.write_string(cond.text)
		s.write_string(l.lead_text(close))
		s.write_string(')')
		l.emit_stmt(mut s, close + 1, j, false, false)
	} else {
		// The condition runs statements: evaluate them at the top of every iteration.
		s.write_string('for (;;) { ')
		s.write_string(l.pre_text(cond.pre))
		s.write_string(' if (!(')
		s.write_string(cond.text)
		s.write_string(')) break;')
		l.emit_stmt(mut s, close + 1, j, true, false)
		s.write_string(' }')
	}
	l.write_with_pre(mut sb, lead, []MsvcPre{}, s.str(), in_block, after_label)
}

fn (mut l MsvcLowerer) emit_do(mut sb strings.Builder, i int, j int, in_block bool, after_label bool) {
	lead := l.lead_text(i)
	body_end := l.stmt_end(i + 1)
	if !l.is(body_end, 'while') || !l.is(body_end + 1, '(') {
		l.emit_range(mut sb, i, j, true)
		return
	}
	open := body_end + 1
	close := l.match_idx[open]
	cond := l.lower_expr(open + 1, close)
	mut s := strings.new_builder(128)
	if cond.pre.len == 0 {
		s.write_string('do')
		l.emit_stmt(mut s, i + 1, body_end, false, false)
		l.emit_range(mut s, body_end, open + 1, true)
		s.write_string(l.lead_text(open + 1))
		s.write_string(cond.text)
		l.emit_range(mut s, close, j, true)
	} else {
		// `continue` jumps to the condition, which now runs after a label at the end of
		// the loop body.
		label := l.new_name('cont')
		l.mark_loop_continues(i + 1, body_end, 'goto ${label}')
		s.write_string('for (;;) {')
		l.emit_stmt(mut s, i + 1, body_end, true, false)
		s.write_string(' ${label}: ; ')
		s.write_string(l.pre_text(cond.pre))
		s.write_string(' if (!(')
		s.write_string(cond.text)
		s.write_string(')) break; }')
	}
	l.write_with_pre(mut sb, lead, []MsvcPre{}, s.str(), in_block, after_label)
}

fn (mut l MsvcLowerer) emit_for(mut sb strings.Builder, i int, j int, in_block bool, after_label bool) {
	lead := l.lead_text(i)
	open := i + 1
	close := l.match_idx[open]
	semi1 := l.find_top_level(open + 1, close, ';')
	semi2 := if semi1 >= 0 { l.find_top_level(semi1 + 1, close, ';') } else { -1 }
	if semi1 < 0 || semi2 < 0 {
		l.emit_range(mut sb, i, j, true)
		return
	}
	init := l.lower_expr(open + 1, semi1)
	cond := l.lower_expr(semi1 + 1, semi2)
	post := l.lower_expr(semi2 + 1, close)
	mut s := strings.new_builder(128)
	s.write_string('for')
	s.write_string(l.lead_text(open))
	s.write_string('(')
	if open + 1 < semi1 {
		s.write_string(l.lead_text(open + 1))
	}
	s.write_string(init.text)
	s.write_string(';')
	if cond.pre.len == 0 {
		if semi1 + 1 < semi2 {
			s.write_string(l.lead_text(semi1 + 1))
		}
		s.write_string(cond.text)
	}
	s.write_string(';')
	if post.pre.len == 0 {
		if semi2 + 1 < close {
			s.write_string(l.lead_text(semi2 + 1))
		}
		s.write_string(post.text)
	}
	s.write_string(')')
	if cond.pre.len == 0 && post.pre.len == 0 {
		l.emit_stmt(mut s, close + 1, j, false, false)
	} else {
		s.write_string(' { ')
		if cond.pre.len > 0 {
			s.write_string(l.pre_text(cond.pre))
			s.write_string(' if (!(')
			s.write_string(cond.text)
			s.write_string(')) break;')
		}
		mut label := ''
		if post.pre.len > 0 {
			label = l.new_name('cont')
			l.mark_loop_continues(close + 1, j, 'goto ${label}')
		}
		l.emit_stmt(mut s, close + 1, j, true, false)
		if post.pre.len > 0 {
			s.write_string(' ${label}: ; ')
			s.write_string(l.pre_text(post.pre))
			s.write_string(' ')
			s.write_string(post.text)
			s.write_string(';')
		}
		s.write_string(' }')
	}
	l.write_with_pre(mut sb, lead, init.pre, s.str(), in_block, after_label)
}

// mark_loop_continues rewrites the `continue` statements that belong to the loop whose
// body is [i, j) into `replacement` (a jump to the loop's re-evaluated condition).
fn (mut l MsvcLowerer) mark_loop_continues(i int, j int, replacement string) {
	mut k := i
	for k < j {
		if l.toks[k].kind == .ident {
			t := l.text(k)
			if t == 'continue' {
				l.dirty[k] = true
				l.dirty_text[k] = replacement
			} else if t in ['for', 'while'] && l.is(k + 1, '(') {
				k = l.stmt_end(k)
				continue
			} else if t == 'do' || l.is_macro_block_stmt(k) {
				k = l.stmt_end(k)
				continue
			}
		}
		k++
	}
}

fn (l &MsvcLowerer) find_top_level(i int, j int, s string) int {
	mut k := i
	for k < j {
		if l.toks[k].kind == .punct {
			if l.is(k, '(') || l.is(k, '[') || l.is(k, '{') {
				k = l.match_idx[k] + 1
				continue
			}
			if l.is(k, s) {
				return k
			}
		}
		k++
	}
	return -1
}

fn (l &MsvcLowerer) find_all_top_level(i int, j int, s string) []int {
	mut res := []int{}
	mut k := i
	for k < j {
		if l.toks[k].kind == .punct {
			if l.is(k, '(') || l.is(k, '[') || l.is(k, '{') {
				k = l.match_idx[k] + 1
				continue
			}
			if l.is(k, s) {
				res << k
			}
		}
		k++
	}
	return res
}

// lower_expr lowers the expression tokens [i, j). The returned text omits the layout
// before token `i`; the caller writes it when it wants to preserve the original spacing.
fn (mut l MsvcLowerer) lower_expr(i int, j int) MsvcLowered {
	if i >= j {
		return MsvcLowered{}
	}
	if !l.range_has_se(i, j) {
		return MsvcLowered{
			text: l.range_str(i, j, false)
		}
	}
	commas := l.find_all_top_level(i, j, ',')
	if commas.len > 0 {
		mut res := MsvcLowered{}
		mut sb := strings.new_builder(64)
		mut start := i
		for idx, comma in commas {
			part := l.lower_expr(start, comma)
			res.pre << part.pre
			if idx > 0 {
				sb.write_string(l.lead_text(start))
			}
			sb.write_string(part.text)
			l.emit_range(mut sb, comma, comma + 1, true)
			start = comma + 1
		}
		part := l.lower_expr(start, j)
		res.pre << part.pre
		if start < j {
			sb.write_string(l.lead_text(start))
		}
		sb.write_string(part.text)
		res.text = sb.str()
		return res
	}
	// Find the first top-level assignment operator and conditional operator. Whichever
	// comes first is the outermost operator of the expression.
	mut assign := -1
	mut question := -1
	mut k := i
	for k < j {
		if l.toks[k].kind == .punct {
			if l.is(k, '(') || l.is(k, '[') || l.is(k, '{') {
				k = l.match_idx[k] + 1
				continue
			}
			t := l.text(k)
			if assign < 0 && t in msvc_assign_ops {
				assign = k
			}
			if question < 0 && t == '?' {
				question = k
			}
		}
		if assign >= 0 || question >= 0 {
			break
		}
		k++
	}
	if assign >= 0 {
		lhs := l.lower_expr(i, assign)
		rhs := l.lower_expr(assign + 1, j)
		mut res := MsvcLowered{}
		res.pre << lhs.pre
		res.pre << rhs.pre
		mut sb := strings.new_builder(64)
		sb.write_string(lhs.text)
		l.emit_range(mut sb, assign, assign + 1, true)
		if assign + 1 < j {
			sb.write_string(l.lead_text(assign + 1))
		}
		sb.write_string(rhs.text)
		res.text = sb.str()
		return res
	}
	if question >= 0 {
		return l.lower_conditional(i, question, j)
	}
	ors := l.find_all_top_level(i, j, '||')
	if ors.len > 0 {
		return l.lower_logical(i, j, ors, true)
	}
	ands := l.find_all_top_level(i, j, '&&')
	if ands.len > 0 {
		return l.lower_logical(i, j, ands, false)
	}
	return l.lower_atoms(i, j)
}

// lower_atoms lowers an operand sequence without top-level `,`, assignment, `?:`, `||` or
// `&&` operators. All statement expressions in it are evaluated unconditionally.
fn (mut l MsvcLowerer) lower_atoms(i int, j int) MsvcLowered {
	mut res := MsvcLowered{}
	mut sb := strings.new_builder(64)
	mut k := i
	for k < j {
		is_group := l.toks[k].kind == .punct && (l.is(k, '(') || l.is(k, '[') || l.is(k, '{'))
		if !is_group || !l.range_has_se(k, l.match_idx[k] + 1) {
			if is_group {
				close := l.match_idx[k]
				l.emit_range(mut sb, k, close + 1, k > i)
				k = close + 1
			} else {
				l.emit_range(mut sb, k, k + 1, k > i)
				k++
			}
			continue
		}
		close := l.match_idx[k]
		if k > i {
			sb.write_string(l.lead_text(k))
		}
		if l.is(k, '(') && l.is(k + 1, '{') && l.match_idx[k + 1] == close - 1 {
			se := l.lower_stmt_expr(k)
			res.pre << se.pre
			sb.write_string(se.text)
		} else {
			inner := l.lower_expr(k + 1, close)
			res.pre << inner.pre
			sb.write_string(l.text(k))
			if k + 1 < close {
				sb.write_string(l.lead_text(k + 1))
			}
			sb.write_string(inner.text)
			l.emit_range(mut sb, close, close + 1, true)
		}
		k = close + 1
	}
	res.text = sb.str()
	return res
}

// lower_logical lowers a chain of `||` (or `&&`) operands. Once an operand needs hoisted
// statements, the value so far is stored into a flag, and the operand's statements run
// only when the flag does not already decide the result.
fn (mut l MsvcLowerer) lower_logical(i int, j int, ops []int, is_or bool) MsvcLowered {
	mut res := MsvcLowered{}
	first := l.lower_expr(i, ops[0])
	res.pre << first.pre
	mut acc := first.text
	for idx, op in ops {
		start := op + 1
		end := if idx + 1 < ops.len { ops[idx + 1] } else { j }
		operand := l.lower_expr(start, end)
		if operand.pre.len == 0 {
			acc += l.range_str(op, op + 1, true)
			if start < end {
				acc += l.lead_text(start)
			}
			acc += operand.text
			continue
		}
		flag := l.new_name('cond')
		res.pre << MsvcPre{
			is_decl:  true
			text:     'int ${flag};'
			decl:     'int ${flag}'
			name:     flag
			type_txt: 'int'
		}
		res.pre << MsvcPre{
			text: '${flag} = !!(${acc});'
		}
		guard := if is_or { '!${flag}' } else { flag }
		res.pre << MsvcPre{
			text: 'if (${guard}) { ${l.pre_text(operand.pre)} ${flag} = !!(${operand.text}); }'
		}
		acc = flag
	}
	res.text = acc
	return res
}

// lower_conditional lowers `cond ? a : b`. When a branch needs hoisted statements, they
// run in the matching branch of an `if`; their declarations are hoisted in front of it
// so the branch value can still use them.
fn (mut l MsvcLowerer) lower_conditional(i int, question int, j int) MsvcLowered {
	mut depth := 0
	mut colon := -1
	mut k := question + 1
	for k < j {
		if l.toks[k].kind == .punct {
			if l.is(k, '(') || l.is(k, '[') || l.is(k, '{') {
				k = l.match_idx[k] + 1
				continue
			}
			if l.is(k, '?') {
				depth++
			} else if l.is(k, ':') {
				if depth == 0 {
					colon = k
					break
				}
				depth--
			}
		}
		k++
	}
	if colon < 0 {
		return l.lower_atoms(i, j)
	}
	cond := l.lower_expr(i, question)
	then_part := l.lower_expr(question + 1, colon)
	else_part := l.lower_expr(colon + 1, j)
	mut res := MsvcLowered{}
	res.pre << cond.pre
	if then_part.pre.len == 0 && else_part.pre.len == 0 {
		mut sb := strings.new_builder(64)
		sb.write_string(cond.text)
		l.emit_range(mut sb, question, question + 1, true)
		if question + 1 < colon {
			sb.write_string(l.lead_text(question + 1))
		}
		sb.write_string(then_part.text)
		l.emit_range(mut sb, colon, colon + 1, true)
		if colon + 1 < j {
			sb.write_string(l.lead_text(colon + 1))
		}
		sb.write_string(else_part.text)
		res.text = sb.str()
		return res
	}
	flag := l.new_name('cond')
	res.pre << MsvcPre{
		is_decl:  true
		text:     'int ${flag};'
		decl:     'int ${flag}'
		name:     flag
		type_txt: 'int'
	}
	res.pre << MsvcPre{
		text: '${flag} = !!(${cond.text});'
	}
	res.pre << msvc_hoisted_decls(then_part.pre)
	res.pre << msvc_hoisted_decls(else_part.pre)
	mut guard := 'if (${flag}) { ${msvc_guarded_text(then_part.pre)} }'
	if else_part.pre.len > 0 {
		guard += ' else { ${msvc_guarded_text(else_part.pre)} }'
	}
	res.pre << MsvcPre{
		text: guard
	}
	res.text = '(${flag} ? (${then_part.text}) : (${else_part.text}))'
	return res
}

// msvc_hoisted_decls returns declarations, without their initializers, for the
// declarations among `pre`, so they stay visible after `pre` moves into a guarded block.
fn msvc_hoisted_decls(pre []MsvcPre) []MsvcPre {
	mut res := []MsvcPre{}
	for p in pre {
		if !p.is_decl {
			continue
		}
		if p.is_static || p.decl.len == 0 {
			res << MsvcPre{
				...p
				is_static: true
			}
			continue
		}
		res << MsvcPre{
			is_decl:   true
			is_static: true // already hoisted: nothing to assign in a guard
			text:      '${p.decl};'
			decl:      p.decl
			name:      p.name
		}
	}
	return res
}

// msvc_guarded_text returns `pre` as statements for a guarded block, with declarations
// (hoisted by msvc_hoisted_decls) replaced by assignments of their initializers.
fn msvc_guarded_text(pre []MsvcPre) string {
	mut parts := []string{}
	for p in pre {
		if !p.is_decl {
			parts << p.text
			continue
		}
		if p.is_static || p.decl.len == 0 || p.init.len == 0 {
			continue
		}
		init := p.init.trim_space()
		if init.starts_with('{') {
			if p.dims.len > 0 {
				parts << 'memcpy(${p.name}, (${p.type_txt}${p.dims})${init}, sizeof(${p.name}));'
			} else {
				parts << '${p.name} = (${p.type_txt})${init};'
			}
		} else {
			parts << '${p.name} = ${init};'
		}
	}
	return parts.join(' ')
}

// lower_stmt_expr lowers the statement expression whose `(` is at `k`. Its statements
// become hoisted statements and its last expression statement becomes the value.
fn (mut l MsvcLowerer) lower_stmt_expr(k int) MsvcLowered {
	body_start := k + 2
	body_end := l.match_idx[k + 1]
	mut stmts := [][]int{}
	mut s := body_start
	for s < body_end {
		mut e := l.stmt_end(s)
		if e > body_end || e <= s {
			e = body_end
		}
		stmts << [s, e]
		s = e
	}
	// Rename the declarations of the statement expression's own scope; they become
	// declarations of the enclosing block. `extern` declarations and function
	// prototypes name global symbols, and keep their names.
	for st in stmts {
		if l.is_decl_stmt(st[0], st[1]) && !l.decl_is_extern(st[0], st[1]) {
			for name_idx in l.decl_name_indices(st[0], st[1]) {
				if l.is(name_idx + 1, '(') {
					continue
				}
				l.rename_from(name_idx, body_end, l.text(name_idx))
			}
		}
	}
	mut value_idx := -1
	if stmts.len > 0 {
		last := stmts[stmts.len - 1]
		if l.is_value_stmt(last[0], last[1]) {
			value_idx = stmts.len - 1
		}
	}
	mut res := MsvcLowered{}
	for idx, st in stmts {
		if idx == value_idx {
			break
		}
		if l.is(st[0], ';') {
			continue
		}
		if l.is_decl_stmt(st[0], st[1]) {
			res.pre << l.lower_decl_stmt(st[0], st[1])
			continue
		}
		mut sb := strings.new_builder(64)
		l.emit_stmt(mut sb, st[0], st[1], true, idx > 0 && l.is_label_item(stmts[idx - 1][0]))
		res.pre << MsvcPre{
			text: sb.str().trim_space()
		}
	}
	if value_idx >= 0 {
		st := stmts[value_idx]
		mut end := st[1]
		if l.is(end - 1, ';') {
			end--
		}
		value := l.lower_expr(st[0], end)
		res.pre << value.pre
		res.text = '(${value.text})'
	} else {
		res.text = '((void)0)'
	}
	return res
}

// rename_from renames the identifier `name` from token `from` to `to`, skipping member
// names after `.` and `->`.
fn (mut l MsvcLowerer) rename_from(from int, to int, name string) {
	new_name := l.new_name(name)
	for k in from .. to {
		if l.toks[k].kind != .ident || l.toks[k].end - l.toks[k].pos != name.len
			|| l.text(k) != name {
			continue
		}
		if k > 0 && (l.is(k - 1, '.') || l.is(k - 1, '->')) {
			continue
		}
		l.dirty[k] = true
		l.dirty_text[k] = new_name
	}
}

fn (l &MsvcLowerer) is_value_stmt(i int, j int) bool {
	if i >= j || !l.is(j - 1, ';') || l.is(i, ';') || l.is(i, '{') {
		return false
	}
	if l.toks[i].kind == .ident && l.text(i) in msvc_stmt_keywords {
		return false
	}
	if l.is_label_item(i) {
		return false
	}
	return !l.is_decl_stmt(i, j)
}

fn (l &MsvcLowerer) is_decl_stmt(i int, j int) bool {
	if i >= j || l.toks[i].kind != .ident {
		return false
	}
	t := l.text(i)
	if t in msvc_decl_start_keywords {
		return true
	}
	if t in msvc_stmt_keywords || l.is(i + 1, ':') {
		return false
	}
	mut k := i + 1
	for k < j && (l.is(k, '*') || l.is(k, 'const') || l.is(k, 'volatile')) {
		k++
	}
	if k < j && k > i && l.toks[k].kind == .ident && k + 1 < j {
		next := l.text(k + 1)
		return next in ['=', ';', '[', ',']
	}
	// Function pointer declarator: `ret (*name)(params)`.
	return l.is(i + 1, '(') && l.is(i + 2, '*') && i + 3 < j && l.toks[i + 3].kind == .ident
		&& l.is(i + 4, ')') && l.is(i + 5, '(')
}

// decl_is_extern reports whether the declaration [i, j) is an `extern` declaration.
fn (l &MsvcLowerer) decl_is_extern(i int, j int) bool {
	mut k := i
	for k < j && l.toks[k].kind == .ident && l.text(k) in msvc_decl_start_keywords {
		if l.is(k, 'extern') {
			return true
		}
		k++
	}
	return false
}

// decl_name_indices returns the token indices of the names declared by [i, j).
fn (l &MsvcLowerer) decl_name_indices(i int, j int) []int {
	mut res := []int{}
	mut start := i
	for {
		mut k := start
		mut name_idx := -1
		for k < j {
			if l.is(k, '(') && l.is(k + 1, '*') && k + 2 < j && l.toks[k + 2].kind == .ident
				&& l.is(k + 3, ')') {
				name_idx = k + 2
				k = l.match_idx[k] + 1
				continue
			}
			if l.is(k, '(') || l.is(k, '[') || l.is(k, '{') {
				k = l.match_idx[k] + 1
				continue
			}
			if l.is(k, '=') || l.is(k, ',') || l.is(k, ';') {
				break
			}
			if l.toks[k].kind == .ident && l.text(k) !in msvc_decl_start_keywords {
				name_idx = k
			}
			k++
		}
		if name_idx >= 0 {
			res << name_idx
		}
		if l.is(k, '=') {
			// Skip the initializer.
			for k < j && !l.is(k, ',') && !l.is(k, ';') {
				if l.is(k, '(') || l.is(k, '[') || l.is(k, '{') {
					k = l.match_idx[k] + 1
					continue
				}
				k++
			}
		}
		if k < j && l.is(k, ',') {
			start = k + 1
			continue
		}
		break
	}
	return res
}

// lower_decl_stmt lowers a declaration inside a statement expression into hoisted items.
fn (mut l MsvcLowerer) lower_decl_stmt(i int, j int) []MsvcPre {
	mut end := j
	if l.is(end - 1, ';') {
		end--
	}
	is_static := l.is(i, 'static')
	eq := l.find_top_level(i, end, '=')
	commas := l.find_all_top_level(i, end, ',')
	if eq < 0 || commas.len > 0 || is_static {
		lowered := l.lower_expr(i, end)
		mut pre := lowered.pre.clone()
		names := l.decl_name_indices(i, j)
		decl_name := if names.len == 1 { l.dirty_text[names[0]] } else { '' }
		decl_text := lowered.text + ';'
		mut decl := ''
		if eq < 0 && commas.len == 0 && !is_static && names.len == 1 {
			decl = lowered.text
		}
		pre << MsvcPre{
			is_decl:   true
			is_static: decl.len == 0
			text:      decl_text
			decl:      decl
			name:      decl_name
		}
		return pre
	}
	names := l.decl_name_indices(i, j)
	if names.len != 1 {
		lowered := l.lower_expr(i, end)
		mut pre := lowered.pre.clone()
		pre << MsvcPre{
			is_decl:   true
			is_static: true
			text:      lowered.text + ';'
		}
		return pre
	}
	name_idx := names[0]
	init := l.lower_expr(eq + 1, end)
	declarator := l.range_str(i, eq, false).trim_space()
	// Array dimensions follow the name; the type is everything before it.
	mut dims := ''
	if name_idx + 1 < eq && l.is(name_idx + 1, '[') {
		dims = l.range_str(name_idx + 1, eq, false).trim_space()
	}
	mut type_txt := l.range_str(i, name_idx, false).trim_space()
	mut decl := declarator
	is_fn_ptr := l.is(name_idx - 1, '*') && l.is(name_idx - 2, '(')
	if is_fn_ptr {
		type_txt = ''
	} else if type_txt.starts_with('const ') && !type_txt.contains('*') {
		type_txt = type_txt['const '.len..]
		decl = declarator['const '.len..]
	}
	mut pre := init.pre.clone()
	mut text := strings.new_builder(64)
	text.write_string(declarator)
	text.write_string(' = ')
	text.write_string(init.text.trim_space())
	text.write_string(';')
	pre << MsvcPre{
		is_decl:   true
		is_static: is_fn_ptr && init.text.trim_space().starts_with('{')
		text:      text.str()
		decl:      decl
		name:      l.dirty_text[name_idx]
		init:      init.text
		type_txt:  type_txt
		dims:      dims
	}
	return pre
}
