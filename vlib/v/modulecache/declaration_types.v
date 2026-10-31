module modulecache

import strings

// The types layer of the declaration header subset (see declaration_subset.v for the
// declarations layer). A module object is compiled from the header of the whole program
// followed by the module's code; the layers decide which top level constructs of that
// header a module object does not need.
//
// The header is split into items: a preprocessor directive (with its continuation lines),
// a string literal definition, a whole `struct`/`union`/`enum` definition or typedef, a whole
// `static` function, a prototype or extern variable, a comment or blank line. Every item
// that can be told apart with a reason defines names (the type, the function, the enum
// constants, the macro) and mentions names. An item that a layer allows to go stays only
// when its name is mentioned, transitively:
//
//   roots    the identifiers of the module code, and of every item that never goes
//   closure  a kept item keeps the items that define the names it mentions
//
// Every mention keeps the body of a type, including a mention that is followed by `*`.
// A forward declaration would be enough for a pointer that nothing dereferences, but the
// module code can dereference a pointer whose type it never names: `c->streams.data->len`
// reaches the body of the type of `data` through the field of `c->streams`, and so do
// `make_thing()->a` and `sizeof(*p)`. The identifiers of the code cannot tell. (Keeping
// the bodies of pointed-to types costs about 1 percent of the bytes a module is given.)
// The order of the items that stay is the header's.

// HeaderLayer says how much of a header a module object may be spared. Each layer includes
// the ones before it.
pub enum HeaderLayer {
	declarations // prototypes, extern variables, generated string literals (brief 19)
	types        // + struct, union and enum definitions, typedefs and forward declarations
	macros       // + the macros that V generates for constants
	statics      // + `static` functions: TinyCC emits the unused ones, so the object changes
}

enum ItemKind {
	always // never goes: its identifiers are roots
	blank
	comment // a `//` line: never goes, mentions nothing
	lit
	decl
	fwd
	body
	typedef
	static_fn
	macro
}

struct HeaderItem {
mut:
	kind  ItemKind
	first int // first and last unit of the item
	last  int
	defs  []string
	full  []string // names it mentions
}

// TypedIndex is the closure state of one header, built once. Names are numbered; the
// base state is the closure of the items that never go.
struct TypedIndex {
	items     []HeaderItem
	name_ids  map[string]int
	by_name   [][]int // per name: the items that define it
	item_full [][]int // per item: the names it mentions
	can_go    []bool  // per item: the layer lets it go
	joined    string
	starts    []int // per item: where it starts in joined
	ends      []int // per item: where it ends in joined (after its newline)
	drop_at   []int // the items that can go, in order
	has_paste bool
mut:
	base_need []bool // per name: some root or kept item mentions it
	base_keep []bool // per item
}

const static_function_blockers = ['constructor', 'destructor', 'section', 'naked', 'ifunc', 'alias',
	'weak']

// is_generated_constant_macro tells the macro names that V generates for a constant:
// `<module>__<name>`, lower case first, never an identifier of the system headers.
fn is_generated_constant_macro(name string) bool {
	if name.len < 4 || !(name[0] >= `a` && name[0] <= `z`) {
		return false
	}
	return name.contains('__') && !name.ends_with('__')
}

fn is_ident_token(tok string) bool {
	return tok.len > 0 && (tok[0].is_letter() || tok[0] == `_`)
}

// tokenize_group turns the lines first..last of units into tokens: identifiers, one
// character punctuators, `0` for a number and `"` for a string or character literal.
// Comments are dropped. A directive line inside the group only contributes identifiers.
fn tokenize_group(units []CSourceUnit, first int, last int) ([]string, []string) {
	mut toks := []string{cap: 16}
	mut from_directives := []string{}
	mut in_comment := false
	for u := first; u <= last; u++ {
		line := units[u].text
		if u > first && !in_comment && line.trim_space().starts_with('#') {
			mut tmp := map[string]bool{}
			collect_identifiers(line, mut tmp)
			for k, _ in tmp {
				from_directives << k
			}
			continue
		}
		mut i := 0
		for i < line.len {
			c := line[i]
			if in_comment {
				if c == `*` && i + 1 < line.len && line[i + 1] == `/` {
					in_comment = false
					i += 2
				} else {
					i++
				}
				continue
			}
			if c == ` ` || c == `\t` || c == `\r` {
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
				toks << '"'
				continue
			}
			if c.is_letter() || c == `_` {
				start := i
				for i < line.len && (line[i].is_alnum() || line[i] == `_`) {
					i++
				}
				toks << unsafe { line[start..i] }
				continue
			}
			if c.is_digit() {
				for i < line.len && (line[i].is_alnum() || line[i] == `.`) {
					i++
				}
				toks << '0'
				continue
			}
			toks << unsafe { line[i..i + 1] }
			i++
		}
	}
	return toks, from_directives
}

fn last_index_of(toks []string, tok string) int {
	for i := toks.len - 1; i >= 0; i-- {
		if toks[i] == tok {
			return i
		}
	}
	return -1
}

// enum_constants returns the names that an enum body defines: the first identifier of each
// comma separated entry between the braces at `open` and `close`.
fn enum_constants(toks []string, open int, close int) []string {
	mut names := []string{}
	mut expect := true
	mut nesting := 0
	for k := open + 1; k < close; k++ {
		tok := toks[k]
		if tok == '(' || tok == '[' || tok == '{' {
			nesting++
		} else if tok == ')' || tok == ']' || tok == '}' {
			nesting--
		}
		if expect && nesting == 0 && is_ident_token(tok) {
			names << tok
			expect = false
		} else if tok == ',' && nesting == 0 {
			expect = true
		}
	}
	return names
}

fn push_unique(mut names []string, name string) {
	if name !in names {
		names << name
	}
}

// classify_group says what the group of tokens is, with the names it defines. It returns
// .always whenever the shape is not one that it knows, so a surprise stays.
fn classify_group(toks []string, line_text string, single_line bool) (ItemKind, []string) {
	if toks.len < 2 {
		return .always, []string{}
	}
	last := toks[toks.len - 1]
	match toks[0] {
		'typedef' {
			if toks.len == 5 && (toks[1] == 'struct' || toks[1] == 'union') && last == ';'
				&& is_ident_token(toks[2]) && is_ident_token(toks[3]) {
				mut defs := []string{}
				push_unique(mut defs, toks[3])
				push_unique(mut defs, toks[2])
				return .fwd, defs
			}
			open := toks.index('{')
			if open >= 0 {
				close := last_index_of(toks, '}')
				if close < open || last != ';' || close + 2 != toks.len - 1 {
					return .always, []string{}
				}
				if !is_ident_token(toks[close + 1]) {
					return .always, []string{}
				}
				mut defs := []string{}
				push_unique(mut defs, toks[close + 1])
				if (toks[1] == 'struct' || toks[1] == 'union' || toks[1] == 'enum')
					&& is_ident_token(toks[2]) && toks[3] == '{' {
					push_unique(mut defs, toks[2])
				}
				if toks[1] == 'enum' {
					for name in enum_constants(toks, open, close) {
						push_unique(mut defs, name)
					}
				}
				return .body, defs
			}
			paren := toks.index('(')
			if paren >= 0 {
				mut k := paren + 1
				for k < toks.len && is_ident_token(toks[k]) {
					k++
				}
				if k < toks.len && toks[k] == '*' {
					for k < toks.len && toks[k] == '*' {
						k++
					}
					if k + 1 < toks.len && is_ident_token(toks[k]) && toks[k + 1] == ')' {
						return .typedef, [toks[k]]
					}
				}
				return .always, []string{}
			}
			if last != ';' {
				return .always, []string{}
			}
			mut k := toks.len - 2
			if toks[k] == ']' {
				for k > 0 && toks[k] != '[' {
					k--
				}
				k--
			}
			if k > 0 && is_ident_token(toks[k]) {
				return .typedef, [toks[k]]
			}
			return .always, []string{}
		}
		'struct', 'union' {
			if toks.len == 3 && is_ident_token(toks[1]) && last == ';' {
				return .fwd, [toks[1]]
			}
			if toks.len >= 5 && is_ident_token(toks[1]) && toks[2] == '{' && last == ';'
				&& toks[toks.len - 2] == '}' {
				return .body, [toks[1]]
			}
			return .always, []string{}
		}
		'enum' {
			open := toks.index('{')
			close := last_index_of(toks, '}')
			if open < 0 || close < open || last != ';' || close != toks.len - 2 {
				return .always, []string{}
			}
			mut defs := []string{}
			if open == 2 && is_ident_token(toks[1]) {
				push_unique(mut defs, toks[1])
			}
			for name in enum_constants(toks, open, close) {
				push_unique(mut defs, name)
			}
			return .body, defs
		}
		'static' {
			open := toks.index('{')
			if open < 0 || last != '}' || '=' in toks[..open] {
				return .always, []string{}
			}
			paren := toks.index('(')
			if paren < 1 || paren > open || !is_ident_token(toks[paren - 1]) {
				return .always, []string{}
			}
			for blocker in static_function_blockers {
				if blocker in toks {
					return .always, []string{}
				}
			}
			return .static_fn, [toks[paren - 1]]
		}
		else {
			if single_line {
				name := droppable_declaration_name(line_text)
				if name.len > 0 {
					return .decl, [name]
				}
			}
			return .always, []string{}
		}
	}
}

// collect_mentions returns the identifiers of `toks` that some item defines.
fn collect_mentions(toks []string, defs []string, name_ids map[string]int) []string {
	mut seen := map[string]bool{}
	mut order := []string{}
	for tok in toks {
		if !is_ident_token(tok) || tok !in name_ids || tok in defs || tok in seen {
			continue
		}
		seen[tok] = true
		order << tok
	}
	return order
}

fn layer_lets_go(layer HeaderLayer, kind ItemKind) bool {
	return match kind {
		.lit, .decl { true }
		.fwd, .body, .typedef { int(layer) >= int(HeaderLayer.types) }
		.macro { int(layer) >= int(HeaderLayer.macros) }
		.static_fn { int(layer) >= int(HeaderLayer.statics) }
		else { false }
	}
}

// new_typed_index splits `header` into items and settles the closure of the ones that stay.
fn new_typed_index(header string, layer HeaderLayer) TypedIndex {
	units := c_source_units(header)
	mut items := []HeaderItem{cap: units.len / 2}
	mut group_tokens := [][]string{cap: units.len / 2}
	mut in_block_comment := false
	mut has_paste := false
	mut i := 0
	for i < units.len {
		unit := units[i]
		line := unit.text
		if unit.symbol != '' {
			mut item := HeaderItem{
				kind:  .lit
				first: i
				last:  i
				defs:  [unit.symbol]
			}
			for symbol in generated_string_symbols(line) {
				if symbol != unit.symbol {
					item.full << symbol
				}
			}
			items << item
			group_tokens << []string{}
			i++
			continue
		}
		trimmed := line.trim_space()
		if !in_block_comment && trimmed.starts_with('//') {
			items << HeaderItem{
				kind:  .comment
				first: i
				last:  i
			}
			group_tokens << []string{}
			i++
			continue
		}
		if in_block_comment || trimmed.starts_with('#') || trimmed.starts_with('/*') {
			// a directive, with its continuation lines; a block comment line is kept like one
			mut last := i
			if !in_block_comment && trimmed.starts_with('#') {
				mut continues := line.trim_right(' \t\r').ends_with('\\')
				for continues && last + 1 < units.len {
					last++
					continues = units[last].text.trim_right(' \t\r').ends_with('\\')
				}
			} else {
				_, in_block_comment = scan_c_line(line, 0, in_block_comment)
			}
			if line.contains('##') {
				has_paste = true
			}
			mut item := HeaderItem{
				kind:  .always
				first: i
				last:  last
			}
			mut tmp := map[string]bool{}
			for u := i; u <= last; u++ {
				collect_identifiers(units[u].text, mut tmp)
			}
			if trimmed.starts_with('#') {
				define := directive_defined_name(trimmed)
				if define.len > 0 && is_generated_constant_macro(define) {
					item.kind = .macro
					item.defs = [define]
					tmp.delete(define)
				}
			}
			for k, _ in tmp {
				item.full << k
			}
			items << item
			group_tokens << []string{}
			i = last + 1
			continue
		}
		if trimmed == '' {
			items << HeaderItem{
				kind:  .always
				first: i
				last:  i
			}
			group_tokens << []string{}
			i++
			continue
		}
		// a construct: lines until the braces are closed and it ends with `;` or `}`
		mut depth := 0
		mut j := i
		for j < units.len {
			l := units[j].text
			lt := l.trim_space()
			inside_directive := j > i && lt.starts_with('#')
			if !inside_directive {
				depth, in_block_comment = scan_c_line(l, depth, in_block_comment)
				if depth <= 0 && !in_block_comment {
					t := l.trim_right(' \t\r')
					if t.ends_with(';') || t.ends_with('}') {
						break
					}
				}
			}
			j++
		}
		last := if j >= units.len { units.len - 1 } else { j }
		toks, from_directives := tokenize_group(units, i, last)
		for u := i; u <= last; u++ {
			if units[u].text.contains('##') {
				has_paste = true
			}
		}
		kind, defs := classify_group(toks, line, last == i)
		mut item := HeaderItem{
			kind:  kind
			first: i
			last:  last
			defs:  defs
		}
		if kind == .always {
			mut tmp := map[string]bool{}
			for tok in toks {
				if is_ident_token(tok) {
					tmp[tok] = true
				}
			}
			for k in from_directives {
				tmp[k] = true
			}
			for k, _ in tmp {
				item.full << k
			}
		} else {
			for k in from_directives {
				push_unique(mut item.full, k)
			}
		}
		items << item
		group_tokens << toks
		i = last + 1
	}
	return finish_typed_index(units, mut items, group_tokens, layer, has_paste)
}

// directive_defined_name returns NAME of `#define NAME ...` (or `#define NAME(...)`), else ''.
fn directive_defined_name(trimmed string) string {
	mut rest := trimmed[1..].trim_left(' \t')
	if !rest.starts_with('define') {
		return ''
	}
	rest = rest[6..]
	if rest.len == 0 || (rest[0] != ` ` && rest[0] != `\t`) {
		return ''
	}
	rest = rest.trim_left(' \t')
	mut end := 0
	for end < rest.len && (rest[end].is_alnum() || rest[end] == `_`) {
		end++
	}
	if end == 0 || rest[0].is_digit() {
		return ''
	}
	return rest[..end]
}

fn finish_typed_index(units []CSourceUnit, mut items []HeaderItem, group_tokens [][]string, layer HeaderLayer, has_paste bool) TypedIndex {
	// a `##` anywhere can make a name that no identifier shows: only the declarations layer
	// is sound then
	eff_layer := if has_paste { HeaderLayer.declarations } else { layer }
	mut name_ids := map[string]int{}
	mut by_name := [][]int{}
	mut can_go := []bool{len: items.len}
	for n, item in items {
		can_go[n] = layer_lets_go(eff_layer, item.kind) && item.defs.len > 0
		for name in item.defs {
			id := name_ids[name] or {
				name_ids[name] = by_name.len
				by_name << []int{}
				by_name.len - 1
			}
			by_name[id] << n
		}
	}
	// mentions: only names that some item defines matter
	mut item_full := [][]int{len: items.len}
	for n, mut item in items {
		if item.kind == .always || item.kind == .blank || item.kind == .comment {
			if item.kind != .always {
				continue
			}
			for name in item.full {
				if id := name_ids[name] {
					item_full[n] << id
				}
			}
			continue
		}
		if item.kind == .lit || item.kind == .macro {
			for name in item.full {
				if id := name_ids[name] {
					item_full[n] << id
				}
			}
			continue
		}
		toks := group_tokens[n]
		for name in collect_mentions(toks, item.defs, name_ids) {
			item_full[n] << name_ids[name]
		}
		for name in item.full {
			// identifiers of directives inside the construct
			if id := name_ids[name] {
				if id !in item_full[n] {
					item_full[n] << id
				}
			}
		}
	}
	joined := if units.len == 0 { '' } else { units.map(it.text).join('\n') + '\n' }
	mut unit_start := []int{len: units.len}
	mut offset := 0
	for u, unit in units {
		unit_start[u] = offset
		offset += unit.text.len + 1
	}
	mut starts := []int{len: items.len}
	mut ends := []int{len: items.len}
	mut drop_at := []int{}
	for n, item in items {
		starts[n] = unit_start[item.first]
		ends[n] = unit_start[item.last] + units[item.last].text.len + 1
		if can_go[n] {
			drop_at << n
		}
	}
	mut index := TypedIndex{
		items:     items
		name_ids:  name_ids
		by_name:   by_name
		item_full: item_full
		can_go:    can_go
		base_need: []bool{len: by_name.len}
		base_keep: []bool{len: items.len}
		joined:    joined
		starts:    starts
		ends:      ends
		drop_at:   drop_at
		has_paste: has_paste
	}
	mut need := index.base_need.clone()
	mut keep := index.base_keep.clone()
	mut work := []int{}
	for n, _ in items {
		if !can_go[n] {
			index.take(n, mut need, mut keep, mut work)
		}
	}
	index.settle(mut need, mut keep, mut work)
	index.base_need = need
	index.base_keep = keep
	return index
}

fn raise_need(mut need []bool, mut work []int, name int) {
	if !need[name] {
		need[name] = true
		work << name
	}
}

fn (idx &TypedIndex) take(n int, mut need []bool, mut keep []bool, mut work []int) {
	if keep[n] {
		return
	}
	keep[n] = true
	for name in idx.item_full[n] {
		raise_need(mut need, mut work, name)
	}
}

// settle keeps what the raised names ask for until nothing new is asked.
fn (idx &TypedIndex) settle(mut need []bool, mut keep []bool, mut work []int) {
	for work.len > 0 {
		name := work.pop()
		for n in idx.by_name[name] {
			idx.take(n, mut need, mut keep, mut work)
		}
	}
}

// subset returns the header without the items that neither `code` nor the items that stay
// need.
fn (idx &TypedIndex) subset(code string, total_hint int) string {
	if idx.drop_at.len == 0 || code.contains('##') {
		return idx.joined
	}
	mut need := idx.base_need.clone()
	mut keep := idx.base_keep.clone()
	mut work := []int{cap: 512}
	mut start := -1
	for i := 0; i <= code.len; i++ {
		is_identifier := i < code.len && (code[i].is_alnum() || code[i] == `_`)
		if is_identifier {
			if start < 0 {
				start = i
			}
			continue
		}
		if start >= 0 {
			if !code[start].is_digit() {
				if id := idx.name_ids[unsafe { code[start..i] }] {
					raise_need(mut need, mut work, id)
				}
			}
			start = -1
		}
	}
	idx.settle(mut need, mut keep, mut work)
	mut out := strings.new_builder(total_hint + 1024)
	mut run_start := 0
	for n in idx.drop_at {
		if keep[n] {
			continue
		}
		out.write_string(idx.joined[run_start..idx.starts[n]])
		run_start = idx.ends[n]
	}
	if run_start < idx.joined.len {
		out.write_string(idx.joined[run_start..])
	}
	return out.str()
}
