module builtin

$if linux && (amd64 || arm64) && !v_bare_no_startup ? {
	#include "@DIR/libc_pattern_abi.h"
	#include <regex.h>
	#include <fnmatch.h>
	fn C.v_pattern_match_get(uint, uint, uint)
	fn C.v_pattern_match_set(uint, uint, i64, i64)
	fn C.v_pattern_offset_max() i64

	// AST nodes and the Thompson instruction stream use owned, fallible native
	// allocations. They never retain the compiling caller's allocator scope.
	struct BarePatternNode {
	mut:
		kind    int
		left    int
		right   int
		value   u32
		minimum int
		maximum int
		ranges  int
		count   int
		classes u32
		negate  bool
	}
	struct BarePatternRange {
		first u32
		last  u32
	}
	struct BarePatternInstruction {
	mut:
		kind    int
		next    int
		other   int
		value   u32
		ranges  int
		count   int
		classes u32
		negate  bool
	}
	@[heap]
	struct BarePattern {
	mut:
		nodes          &BarePatternNode = unsafe { nil }
		node_count     int
		node_capacity  int
		ranges         &BarePatternRange = unsafe { nil }
		range_count    int
		range_capacity int
		code           &BarePatternInstruction = unsafe { nil }
		code_count     int
		code_capacity  int
		pattern        string
		position       int
		flags          i32
		groups         int
		closed         [10]bool
		error          i32
		start          int
		unicode        bool
		backrefs       bool
		glob           bool
	}
	// Node/instruction kinds: empty, literal, any, class, concatenate,
	// alternate, repeat, capture, BOL, EOL, backreference, word boundaries.
	const bare_pattern_empty = 0
	const bare_pattern_literal = 1
	const bare_pattern_any = 2
	const bare_pattern_class = 3
	const bare_pattern_concat = 4
	const bare_pattern_alternate = 5
	const bare_pattern_repeat = 6
	const bare_pattern_capture = 7
	const bare_pattern_bol = 8
	const bare_pattern_eol = 9
	const bare_pattern_backref = 10
	const bare_pattern_word_start = 11
	const bare_pattern_word_end = 12
	const bare_pattern_word_boundary = 13
	const bare_pattern_not_boundary = 14
	const bare_pattern_split = 15
	const bare_pattern_save = 16
	const bare_pattern_accept = 17

	fn bare_pattern_grow(pointer voidptr, capacity &int, size uint) voidptr {
		current := unsafe { *capacity }
		if current > max_int / 2 { return unsafe { nil } }
		next := if current == 0 { 32 } else { current * 2 }
		if uint(next) > uint(max_int) / size { return unsafe { nil } }
		grown := unsafe { realloc(pointer, uint(next) * size) }
		if grown != unsafe { nil } { unsafe { *capacity = next } }
		return grown
	}
	fn bare_pattern_add_node(mut re BarePattern, node BarePatternNode) int {
		if re.error != 0 { return -1 }
		if re.node_count == re.node_capacity {
			grown := bare_pattern_grow(re.nodes, &re.node_capacity, sizeof(BarePatternNode))
			if grown == unsafe { nil } {
				re.error = i32(C.REG_ESPACE)
				return -1
			}
			re.nodes = unsafe { &BarePatternNode(grown) }
		}
		index := re.node_count
		unsafe { re.nodes[index] = node }
		re.node_count++
		return index
	}
	fn bare_pattern_add_range(mut re BarePattern, first u32, last u32) {
		if re.error != 0 { return }
		if first > last {
			re.error = i32(C.REG_ERANGE)
			return
		}
		if re.range_count == re.range_capacity {
			grown := bare_pattern_grow(re.ranges, &re.range_capacity, sizeof(BarePatternRange))
			if grown == unsafe { nil } {
				re.error = i32(C.REG_ESPACE)
				return
			}
			re.ranges = unsafe { &BarePatternRange(grown) }
		}
		unsafe { re.ranges[re.range_count] = BarePatternRange{ first: first, last: last } }
		re.range_count++
	}
	fn bare_pattern_emit(mut re BarePattern, instruction BarePatternInstruction) int {
		if re.error != 0 { return -1 }
		if re.code_count == re.code_capacity {
			grown := bare_pattern_grow(re.code, &re.code_capacity, sizeof(BarePatternInstruction))
			if grown == unsafe { nil } {
				re.error = i32(C.REG_ESPACE)
				return -1
			}
			re.code = unsafe { &BarePatternInstruction(grown) }
		}
		index := re.code_count
		unsafe { re.code[index] = instruction }
		re.code_count++
		return index
	}
	fn bare_pattern_character(text string, at int, unicode bool) (u32, int) {
		if at >= text.len { return 0, 0 }
		if !unicode || text[at] < 128 { return u32(text[at]), 1 }
		value, length, code := bare_iconv_decode(BareIconvEncoding{}, unsafe { text.str + at }, uint(text.len - at))
		if code != 0 { return u32(text[at]), 1 }
		return value, int(length)
	}
	fn bare_pattern_take(mut re BarePattern) u32 {
		value, length := bare_pattern_character(re.pattern, re.position, re.unicode)
		re.position += length
		return value
	}
	fn bare_pattern_token(re &BarePattern, character u8) bool {
		if re.position >= re.pattern.len { return false }
		if re.glob { return re.pattern[re.position] == character }
		if re.flags & i32(C.REG_EXTENDED) != 0 { return re.pattern[re.position] == character }
		return re.position + 1 < re.pattern.len && re.pattern[re.position] == `\\` && re.pattern[re.position + 1] == character
	}
	fn bare_pattern_skip_token(mut re BarePattern) {
		re.position += if !re.glob && re.flags & i32(C.REG_EXTENDED) == 0 { 2 } else { 1 }
	}
	fn bare_pattern_bracket_character(mut re BarePattern) (u32, u32, bool) {
		if re.glob && re.flags & i32(C.FNM_NOESCAPE) == 0 && re.pattern[re.position] == `\\` {
			re.position++
			if re.position == re.pattern.len {
				re.error = i32(C.REG_EBRACK)
				return 0, 0, false
			}
			return bare_pattern_take(mut re), 0, false
		}
		if re.position + 1 < re.pattern.len && re.pattern[re.position] == `[` && re.pattern[re.position + 1] in [
			`:`,
			`.`,
			`=`,
		] {
			marker := re.pattern[re.position + 1]
			begin := re.position + 2
			mut end := begin
			for end + 1 < re.pattern.len && !(re.pattern[end] == marker && re.pattern[end + 1] == `]`) {
				end++
			}
			if end + 1 == re.pattern.len {
				re.error = i32(C.REG_EBRACK)
				return 0, 0, false
			}
			re.position = end + 2
			if marker == `:` {
				kind := bare_wide_class_id(re.pattern[begin..end])
				if kind == 0 {
					re.error = i32(C.REG_ECTYPE)
					return 0, 0, false
				}
				return 0, u32(1) << kind, true
			}
			value, length := bare_pattern_character(re.pattern, begin, re.unicode)
			if begin + length != end {
				re.error = i32(C.REG_ECOLLATE)
				return 0, 0, false
			}
			return value, 0, false
		}
		return bare_pattern_take(mut re), 0, false
	}
	fn bare_pattern_bracket(mut re BarePattern) int {
		re.position++
		mut node := BarePatternNode{ kind: bare_pattern_class, ranges: re.range_count }
		if re.position < re.pattern.len && (re.pattern[re.position] == `^` || (re.glob && re.pattern[re.position] == `!`)) {
			node.negate = true
			re.position++
		}
		mut first := true
		for re.position < re.pattern.len && re.error == 0 {
			if re.pattern[re.position] == `]` && !first {
				re.position++
				node.count = re.range_count - node.ranges
				return bare_pattern_add_node(mut re, node)
			}
			first = false
			value, classes, is_class := bare_pattern_bracket_character(mut re)
			if is_class {
				node.classes |= classes
				continue
			}
			if re.position + 1 < re.pattern.len && re.pattern[re.position] == `-` && re.pattern[re.position + 1] != `]` {
				re.position++
				last, _, invalid := bare_pattern_bracket_character(mut re)
				if invalid {
					re.error = i32(C.REG_ERANGE)
					return -1
				}
				bare_pattern_add_range(mut re, value, last)
			} else {
				bare_pattern_add_range(mut re, value, value)
			}
		}
		if re.error == 0 { re.error = i32(C.REG_EBRACK) }
		return -1
	}
	fn bare_pattern_atom(mut re BarePattern, beginning bool) int {
		if re.position >= re.pattern.len { return bare_pattern_add_node(mut re, BarePatternNode{}) }
		if bare_pattern_token(re, `(`) {
			bare_pattern_skip_token(mut re)
			re.groups++
			group := re.groups
			inside := bare_pattern_alternation(mut re)
			if !bare_pattern_token(re, `)`) {
				if re.error == 0 { re.error = i32(C.REG_EPAREN) }
				return -1
			}
			bare_pattern_skip_token(mut re)
			if group < 10 { re.closed[group] = true }
			return bare_pattern_add_node(mut re, BarePatternNode{ kind: bare_pattern_capture, left: inside, value: u32(group) })
		}
		if re.pattern[re.position] == `[` { return bare_pattern_bracket(mut re) }
		if re.pattern[re.position] == `.` {
			re.position++
			return bare_pattern_add_node(mut re, BarePatternNode{ kind: bare_pattern_any })
		}
		if re.pattern[re.position] == `^` && (beginning || re.flags & i32(C.REG_EXTENDED) != 0) {
			re.position++
			return bare_pattern_add_node(mut re, BarePatternNode{ kind: bare_pattern_bol })
		}
		if re.pattern[re.position] == `$` {
			re.position++
			if re.position == re.pattern.len || bare_pattern_token(re, `)`) || bare_pattern_token(re, `|`) || re.flags & i32(C.REG_EXTENDED) != 0 {
				return bare_pattern_add_node(mut re, BarePatternNode{ kind: bare_pattern_eol })
			}
			return bare_pattern_add_node(mut re, BarePatternNode{ kind: bare_pattern_literal, value: `$` })
		}
		if (re.pattern[re.position] == `*` && re.flags & i32(C.REG_EXTENDED) != 0) || bare_pattern_token(re, `+`) || bare_pattern_token(re, `?`) || bare_pattern_token(re, `{`) {
			re.error = i32(C.REG_BADRPT)
			return -1
		}
		if re.pattern[re.position] == `\\` {
			re.position++
			if re.position == re.pattern.len {
				re.error = i32(C.REG_EESCAPE)
				return -1
			}
			value := bare_pattern_take(mut re)
			if value >= `1` && value <= `9` {
				group := int(value - `0`)
				if !re.closed[group] {
					re.error = i32(C.REG_ESUBREG)
					return -1
				}
				re.backrefs = true
				return bare_pattern_add_node(mut re, BarePatternNode{ kind: bare_pattern_backref, value: u32(group) })
			}
			kind := match value {
				`<` { bare_pattern_word_start }
				`>` { bare_pattern_word_end }
				`b` { bare_pattern_word_boundary }
				`B` { bare_pattern_not_boundary }
				else { bare_pattern_literal }
			}
			if value in [`w`, `W`, `s`, `S`] {
				mut node := BarePatternNode{ kind: bare_pattern_class, negate: value == `W` || value == `S`, ranges: re.range_count }
				if value == `w` || value == `W` {
					node.classes = (u32(1) << 1)
					bare_pattern_add_range(mut re, `_`, `_`)
					node.count = 1
				} else {
					node.classes = u32(1) << 10
				}
				return bare_pattern_add_node(mut re, node)
			}
			return bare_pattern_add_node(mut re, BarePatternNode{ kind: kind, value: value })
		}
		return bare_pattern_add_node(mut re, BarePatternNode{ kind: bare_pattern_literal, value: bare_pattern_take(mut re) })
	}
	fn bare_pattern_bound(mut re BarePattern) int {
		mut value := 0
		mut found := false
		for re.position < re.pattern.len && re.pattern[re.position] >= `0` && re.pattern[re.position] <= `9` {
			found = true
			value = value * 10 + int(re.pattern[re.position] - `0`)
			re.position++
			if value > 32767 {
				re.error = i32(C.REG_BADBR)
				return -1
			}
		}
		return if found { value } else { -1 }
	}
	fn bare_pattern_sequence(mut re BarePattern) int {
		mut sequence := -1
		for re.position < re.pattern.len && re.error == 0 && !bare_pattern_token(re, `)`) && !bare_pattern_token(re, `|`) {
			mut atom := bare_pattern_atom(mut re, sequence == -1)
			if re.error != 0 { return -1 }
			mut quantified := false
			basic_anchor := re.flags & i32(C.REG_EXTENDED) == 0 && unsafe { re.nodes[atom].kind } == bare_pattern_bol
			for re.position < re.pattern.len {
				mut minimum := 0
				mut maximum := -1
				if re.pattern[re.position] == `*` {
					// An initial BRE star (including immediately after ^) is literal.
					if basic_anchor { break }
					re.position++
				} else if bare_pattern_token(re, `+`) {
					bare_pattern_skip_token(mut re)
					minimum = 1
				} else if bare_pattern_token(re, `?`) {
					bare_pattern_skip_token(mut re)
					maximum = 1
				} else if bare_pattern_token(re, `{`) {
					bare_pattern_skip_token(mut re)
					minimum = bare_pattern_bound(mut re)
					maximum = minimum
					if re.position < re.pattern.len && re.pattern[re.position] == `,` {
						re.position++
						maximum = bare_pattern_bound(mut re)
					}
					if !bare_pattern_token(re, `}`) {
						if re.error == 0 { re.error = i32(C.REG_EBRACE) }
						return -1
					}
					bare_pattern_skip_token(mut re)
					if minimum < 0 || (maximum >= 0 && maximum < minimum) {
						re.error = i32(C.REG_BADBR)
						return -1
					}
				} else {
					break
				}
				if quantified {
					re.error = i32(C.REG_BADRPT)
					return -1
				}
				quantified = true
				atom = bare_pattern_add_node(mut re, BarePatternNode{ kind: bare_pattern_repeat, left: atom, minimum: minimum, maximum: maximum })
			}
			sequence = if sequence == -1 {
				atom
			} else {
				bare_pattern_add_node(mut re, BarePatternNode{ kind: bare_pattern_concat, left: sequence, right: atom })
			}
		}
		return if sequence == -1 {
			bare_pattern_add_node(mut re, BarePatternNode{})
		} else {
			sequence
		}
	}
	fn bare_pattern_alternation(mut re BarePattern) int {
		mut left := bare_pattern_sequence(mut re)
		for re.error == 0 && bare_pattern_token(re, `|`) {
			bare_pattern_skip_token(mut re)
			right := bare_pattern_sequence(mut re)
			left = bare_pattern_add_node(mut re, BarePatternNode{ kind: bare_pattern_alternate, left: left, right: right })
		}
		return left
	}
	fn bare_pattern_compile_node(mut re BarePattern, index int, continuation int) int {
		if re.error != 0 || index < 0 { return -1 }
		node := unsafe { re.nodes[index] }
		match node.kind {
			bare_pattern_empty { return continuation }
			bare_pattern_concat {
				right := bare_pattern_compile_node(mut re, node.right, continuation)
				return bare_pattern_compile_node(mut re, node.left, right)
			}
			bare_pattern_alternate {
				left := bare_pattern_compile_node(mut re, node.left, continuation)
				right := bare_pattern_compile_node(mut re, node.right, continuation)
				return bare_pattern_emit(mut re, BarePatternInstruction{ kind: bare_pattern_split, next: left, other: right })
			}
			bare_pattern_capture {
				end := bare_pattern_emit(mut re, BarePatternInstruction{ kind: bare_pattern_save, next: continuation, value: node.value * 2 + 1 })
				body := bare_pattern_compile_node(mut re, node.left, end)
				return bare_pattern_emit(mut re, BarePatternInstruction{ kind: bare_pattern_save, next: body, value: node.value * 2 })
			}
			bare_pattern_repeat {
				mut current := continuation
				if node.maximum == -1 {
					split := bare_pattern_emit(mut re, BarePatternInstruction{
						kind:  bare_pattern_split
						other: continuation
						value: if re.glob {
							u32(1)
						} else {
							u32(0)
						}
					})
					body := bare_pattern_compile_node(mut re, node.left, split)
					if split < 0 { return -1 }
					unsafe { re.code[split].next = body }
					current = split
				} else {
					for _ in node.minimum .. node.maximum {
						body := bare_pattern_compile_node(mut re, node.left, current)
						current = bare_pattern_emit(mut re, BarePatternInstruction{ kind: bare_pattern_split, next: body, other: current })
					}
				}
				for _ in 0 .. node.minimum {
					current = bare_pattern_compile_node(mut re, node.left, current)
				}
				return current
			}
			else {
				return bare_pattern_emit(mut re, BarePatternInstruction{ kind: node.kind, next: continuation, value: node.value, ranges: node.ranges, count: node.count, classes: node.classes, negate: node.negate })
			}
		}
	}
	fn bare_pattern_glob(mut re BarePattern) int {
		mut sequence := -1
		for re.position < re.pattern.len && re.error == 0 {
			mut atom := -1
			character := re.pattern[re.position]
			if character == `*` {
				re.position++
				for re.position < re.pattern.len && re.pattern[re.position] == `*` { re.position++ }
				any := bare_pattern_add_node(mut re, BarePatternNode{ kind: bare_pattern_any })
				atom = bare_pattern_add_node(mut re, BarePatternNode{ kind: bare_pattern_repeat, left: any, maximum: -1 })
			} else if character == `?` {
				re.position++
				atom = bare_pattern_add_node(mut re, BarePatternNode{ kind: bare_pattern_any })
			} else if character == `[` {
				saved_position := re.position
				saved_ranges := re.range_count
				atom = bare_pattern_bracket(mut re)
				if re.error != 0 {
					if re.error == i32(C.REG_ESPACE) { return -1 }
					re.error = 0
					re.position = saved_position + 1
					re.range_count = saved_ranges
					atom = bare_pattern_add_node(mut re, BarePatternNode{ kind: bare_pattern_literal, value: `[` })
				}
			} else {
				if character == `\\` && re.flags & i32(C.FNM_NOESCAPE) == 0 {
					re.position++
					if re.position == re.pattern.len {
						re.error = i32(C.REG_EESCAPE)
						return -1
					}
				}
				atom = bare_pattern_add_node(mut re, BarePatternNode{ kind: bare_pattern_literal, value: bare_pattern_take(mut re) })
			}
			sequence = if sequence == -1 {
				atom
			} else {
				bare_pattern_add_node(mut re, BarePatternNode{ kind: bare_pattern_concat, left: sequence, right: atom })
			}
		}
		return if sequence == -1 {
			bare_pattern_add_node(mut re, BarePatternNode{})
		} else {
			sequence
		}
	}
	fn bare_pattern_new(pattern uint, flags i32, glob bool) &BarePattern {
		mut re := unsafe { &BarePattern(__calloc(1, sizeof(BarePattern))) }
		if re == unsafe { nil } { return re }
		re.pattern = unsafe { tos2(&u8(pattern)) }
		re.flags = flags
		re.glob = glob
		re.unicode = bare_wide_utf8(0)
		root := if glob { bare_pattern_glob(mut re) } else { bare_pattern_alternation(mut re) }
		if re.position != re.pattern.len && re.error == 0 { re.error = i32(C.REG_EPAREN) }
		accept := bare_pattern_emit(mut re, BarePatternInstruction{ kind: bare_pattern_accept })
		re.start = bare_pattern_compile_node(mut re, root, accept)
		unsafe { __free(re.nodes) }
		re.nodes = unsafe { nil }
		re.pattern = ''
		return re
	}
	@[export: 'v_pattern_free'; markused]
	fn bare_pattern_free(address uint) {
		if address == 0 { return }
		re := unsafe { &BarePattern(address) }
		unsafe {
			__free(re.nodes)
			__free(re.ranges)
			__free(re.code)
			__free(re)
		}
	}
	@[export: 'v_pattern_compile'; markused]
	fn bare_pattern_compile(pattern uint, flags i32, output uint, groups uint) i32 {
		re := bare_pattern_new(pattern, flags, false)
		if re == unsafe { nil } { return i32(C.REG_ESPACE) }
		if re.error != 0 {
			code := re.error
			bare_pattern_free(uint(re))
			return code
		}
		unsafe {
			*(&uint(output)) = uint(re)
			*(&uint(groups)) = uint(re.groups)
		}
		return 0
	}

	@[heap]
	struct BarePatternThread {
	mut:
		instruction int
		bucket      &BarePatternThread = unsafe { nil }
		visited     &BarePatternThread = unsafe { nil }
		owned       &BarePatternThread = unsafe { nil }
	}
	fn bare_pattern_tags(thread &BarePatternThread) &i64 {
		return unsafe { &i64(uint(thread) + sizeof(BarePatternThread)) }
	}
	fn bare_pattern_schedule(buckets &uint, position int, instruction int, previous &BarePatternThread, groups int) bool {
		mut node := unsafe { &BarePatternThread(__calloc(1, sizeof(BarePatternThread) + uint(groups * 2 + 2) * sizeof(i64))) }
		if node == unsafe { nil } { return false }
		node.instruction = instruction
		tags := bare_pattern_tags(node)
		if previous != unsafe { nil } {
			unsafe { vmemcpy(tags, bare_pattern_tags(previous), (groups * 2 + 2) * 8) }
		} else {
			for i in 0 .. groups * 2 + 2 {
				unsafe { tags[i] = -1 }
			}
			unsafe { tags[0] = i64(position) }
		}
		node.bucket = unsafe { &BarePatternThread(buckets[position]) }
		unsafe { buckets[position] = uint(node) }
		return true
	}
	fn bare_pattern_preferred(left &i64, right &i64, groups int) int {
		for group in 0 .. groups + 1 {
			start := unsafe { left[group * 2] }
			other_start := unsafe { right[group * 2] }
			if start != other_start {
				if start < 0 { return 1 }
				if other_start < 0 { return -1 }
				return if start < other_start { -1 } else { 1 }
			}
			end := unsafe { left[group * 2 + 1] }
			other_end := unsafe { right[group * 2 + 1] }
			if end != other_end { return if end > other_end { -1 } else { 1 } }
		}
		return 0
	}
	fn bare_pattern_word(character u32, unicode bool) bool {
		return character == `_` || (character >= `0` && character <= `9`)
			|| (character >= `a` && character <= `z`) || (character >= `A` && character <= `Z`)
			|| (unicode && bare_wide_letter(character))
	}
	fn bare_pattern_fold(character u32, unicode bool) u32 {
		if character >= `A` && character <= `Z` { return character + 32 }
		return if unicode { u32(rune(character).to_lower()) } else { character }
	}
	fn bare_pattern_class_match(re &BarePattern, code BarePatternInstruction, character u32, fold bool) bool {
		mut found := false
		for i in 0 .. code.count {
			range := unsafe { re.ranges[code.ranges + i] }
			if character >= range.first && character <= range.last {
				found = true
				break
			}
			if fold {
				lower := bare_pattern_fold(character, re.unicode)
				upper := if re.unicode || character < 128 {
					u32(rune(character).to_upper())
				} else {
					character
				}
				if (lower >= range.first && lower <= range.last) || (upper >= range.first && upper <= range.last) {
					found = true
					break
				}
			}
		}
		if !found {
			for kind in 1 .. 13 {
				if code.classes & (u32(1) << kind) == 0 { continue }
				classified := bare_wide_class(character, uint(kind), 0) != 0
					|| (fold && kind == 7 && bare_wide_class(character, 11, 0) != 0)
					|| (fold && kind == 11 && bare_wide_class(character, 7, 0) != 0)
				if classified {
					found = true
					break
				}
			}
		}
		if code.negate { found = !found }
		return found
	}
	fn bare_pattern_glob_dot(re &BarePattern, text string, position int) bool {
		return re.glob && re.flags & i32(C.FNM_PERIOD) != 0 && position < text.len && text[position] == `.`
			&& (position == 0 || (re.flags & i32(C.FNM_PATHNAME) != 0 && text[position - 1] == `/`))
	}
	fn bare_pattern_run(re &BarePattern, text string, matches uint, count uint, flags i32, origin i64, preceding u32) i32 {
		if text.len > max_int / int(sizeof(uint)) - 1 || re.groups > (max_int / 8 - 2) / 2 {
			return i32(C.REG_ESPACE)
		}
		buckets := unsafe { &uint(__calloc(uint(text.len + 1), sizeof(uint))) }
		visited := unsafe { &uint(__calloc(uint(re.code_count), sizeof(uint))) }
		best := unsafe { &i64(__malloc(uint(re.groups * 2 + 2) * 8)) }
		if buckets == unsafe { nil } || visited == unsafe { nil } || best == unsafe { nil } {
			unsafe {
				__free(buckets)
				__free(visited)
				__free(best)
			}
			return i32(C.REG_ESPACE)
		}
		defer {
			for position in 0 .. text.len + 1 {
				mut node := unsafe { &BarePatternThread(buckets[position]) }
				for node != unsafe { nil } {
					next := node.bucket
					unsafe { __free(node) }
					node = next
				}
			}
			unsafe {
				__free(buckets)
				__free(visited)
				__free(best)
			}
		}
		mut found := false
		mut error := i32(0)
		fold := re.flags & (if re.glob { i32(C.FNM_CASEFOLD) } else { i32(C.REG_ICASE) }) != 0
		newline := !re.glob && re.flags & i32(C.REG_NEWLINE) != 0
		mut previous_word := bare_pattern_word(preceding, re.unicode)
		mut character_end := 0
		for position in 0 .. text.len + 1 {
			character, length := bare_pattern_character(text, position, re.unicode)
			boundary := position >= character_end
			if boundary && (!re.glob || position == 0) && !found {
				if !bare_pattern_schedule(buckets, position, re.start, unsafe { nil }, re.groups) {
					return i32(C.REG_ESPACE)
				}
			}
			unsafe { vmemset(visited, 0, re.code_count * int(sizeof(uint))) }
			mut owned := unsafe { &BarePatternThread(nil) }
			for unsafe { buckets[position] } != 0 {
				mut node := unsafe { &BarePatternThread(buckets[position]) }
				unsafe { buckets[position] = uint(node.bucket) }
				node.owned = owned
				owned = node
				pc := node.instruction
				tags := bare_pattern_tags(node)
				mut prior := unsafe { &BarePatternThread(visited[pc]) }
				mut dominated := false
				for prior != unsafe { nil } {
					comparison := bare_pattern_preferred(bare_pattern_tags(prior), tags, re.groups)
					if comparison == 0 || (!re.backrefs && comparison < 0) {
						dominated = true
						break
					}
					if !re.backrefs { break }
					prior = prior.visited
				}
				if dominated { continue }
				node.visited = unsafe { &BarePatternThread(visited[pc]) }
				unsafe { visited[pc] = uint(node) }
				instruction := unsafe { re.code[pc] }
				mut destination := position
				mut proceed := true
				match instruction.kind {
					bare_pattern_accept {
						if re.glob && position != text.len && !(re.flags & i32(C.FNM_LEADING_DIR) != 0 && text[position] == `/`) {
							continue
						}
						unsafe { tags[1] = i64(position) }
						if !found || bare_pattern_preferred(tags, best, re.groups) < 0 {
							unsafe { vmemcpy(best, tags, (re.groups * 2 + 2) * 8) }
							found = true
						}
						continue
					}
					bare_pattern_split {
						if instruction.value == 1 && bare_pattern_glob_dot(re, text, position) {
							continue
						}
						if !bare_pattern_schedule(buckets, position, instruction.other, node, re.groups) {
							error = i32(C.REG_ESPACE)
							break
						}
					}
					bare_pattern_save { unsafe { tags[instruction.value] = i64(position) } }
					bare_pattern_bol {
						proceed = if position == 0 {
							(origin == 0 && flags & i32(C.REG_NOTBOL) == 0) || (newline && preceding == 10)
						} else {
							newline && text[position - 1] == 10
						}
					}
					bare_pattern_eol {
						proceed = (position == text.len && flags & i32(C.REG_NOTEOL) == 0) || (newline && position < text.len && text[position] == 10)
					}
					bare_pattern_word_start, bare_pattern_word_end, bare_pattern_word_boundary,
					bare_pattern_not_boundary {
						current := position < text.len && bare_pattern_word(character, re.unicode)
						proceed = match instruction.kind {
							bare_pattern_word_start { !previous_word && current }
							bare_pattern_word_end { previous_word && !current }
							bare_pattern_word_boundary { previous_word != current }
							else { previous_word == current }
						}
					}
					bare_pattern_backref {
						start := unsafe { tags[instruction.value * 2] }
						end := unsafe { tags[instruction.value * 2 + 1] }
						if start < 0 || end < start { continue }
						mut a := int(start)
						mut b := position
						for a < end {
							left, left_length := bare_pattern_character(text, a, re.unicode)
							right, right_length := bare_pattern_character(text, b, re.unicode)
							if right_length == 0 || (if fold {
								bare_pattern_fold(left, re.unicode) != bare_pattern_fold(right, re.unicode)
							} else {
								left != right
							}) {
								proceed = false
								break
							}
							a += left_length
							b += right_length
						}
						destination = b
					}
					bare_pattern_literal, bare_pattern_any, bare_pattern_class {
						if length == 0 { continue }
						destination += length
						if instruction.kind == bare_pattern_literal {
							proceed = if fold {
								bare_pattern_fold(character, re.unicode) == bare_pattern_fold(instruction.value, re.unicode)
							} else {
								character == instruction.value
							}
						} else {
							if re.glob && (bare_pattern_glob_dot(re, text, position) || (re.flags & i32(C.FNM_PATHNAME) != 0 && character == `/`)) {
								continue
							}
							proceed = instruction.kind == bare_pattern_any || bare_pattern_class_match(re, instruction, character, fold)
							if newline && character == 10 && (instruction.kind == bare_pattern_any || instruction.negate) {
								proceed = false
							}
						}
					}
					else { proceed = false }
				}
				if proceed && !bare_pattern_schedule(buckets, destination, instruction.next, node, re.groups) {
					error = i32(C.REG_ESPACE)
					break
				}
			}
			for owned != unsafe { nil } {
				next := owned.owned
				unsafe { __free(owned) }
				owned = next
			}
			if error != 0 { return error }
			if boundary {
				previous_word = position < text.len && bare_pattern_word(character, re.unicode)
				character_end = position + length
			}
		}
		if !found { return i32(C.REG_NOMATCH) }
		if !re.glob && re.flags & i32(C.REG_NOSUB) == 0 {
			for i := uint(0); i < count; i++ {
				start := if i <= uint(re.groups) { unsafe { best[i * 2] } } else { i64(-1) }
				end := if i <= uint(re.groups) { unsafe { best[i * 2 + 1] } } else { i64(-1) }
				C.v_pattern_match_set(matches, i, if start < 0 { i64(-1) } else { start + origin }, if end < 0 {
					i64(-1)
				} else {
					end + origin
				})
			}
		}
		return 0
	}
	@[export: 'v_pattern_execute'; markused]
	fn bare_pattern_execute(address uint, input uint, count uint, matches uint, flags i32) i32 {
		re := unsafe { &BarePattern(address) }
		mut origin := i64(0)
		mut length := i64(0)
		if flags & i32(C.REG_STARTEND) != 0 {
			if matches == 0 { return i32(C.REG_BADPAT) }
			mut bounds := [2]i64{}
			C.v_pattern_match_get(matches, 0, uint(&bounds[0]))
			if bounds[0] < 0 || bounds[1] < bounds[0] { return i32(C.REG_BADPAT) }
			origin = bounds[0]
			length = bounds[1] - origin
		} else {
			length = i64(bare_string_chr_nul(input, 0) - input)
		}
		if length > C.v_pattern_offset_max() - origin || length > max_int {
			return i32(C.REG_ESPACE)
		}
		text := unsafe { tos(&u8(input + uint(origin)), int(length)) }
		// A bounded search still sees the character immediately before its start.
		mut preceding := u32(0)
		if origin > 0 {
			mut width := 1
			for re.unicode && width < 4 && i64(width) < origin
				&& unsafe { text.str[-width] } & 0xc0 == 0x80 {
				width++
			}
			prefix := unsafe { tos(text.str - width, width) }
			character, decoded := bare_pattern_character(prefix, 0, re.unicode)
			if decoded == width { preceding = character }
		}
		return bare_pattern_run(re, text, matches, count, flags, origin, preceding)
	}
	@[export: 'v_pattern_fnmatch'; markused]
	fn bare_pattern_fnmatch(pattern uint, input uint, flags i32) i32 {
		re := bare_pattern_new(pattern, flags, true)
		if re == unsafe { nil } { return -1 }
		defer { bare_pattern_free(uint(re)) }
		if re.error != 0 {
			return if re.error == i32(C.REG_ESPACE) { i32(-1) } else { i32(C.FNM_NOMATCH) }
		}
		text := unsafe { tos2(&u8(input)) }
		code := bare_pattern_run(re, text, 0, 0, 0, 0, 0)
		return if code == 0 {
			i32(0)
		} else if code == i32(C.REG_NOMATCH) {
			i32(C.FNM_NOMATCH)
		} else {
			i32(-1)
		}
	}
	@[export: 'v_pattern_error'; markused]
	fn bare_pattern_error(code i32, output uint, size uint) uint {
		text := match code {
			0 { 'Success' }
			1 { 'No match' }
			2 { 'Invalid regular expression' }
			3 { 'Invalid collating element' }
			4 { 'Invalid character class name' }
			5 { 'Trailing backslash' }
			6 { 'Invalid back reference' }
			7 { 'Unmatched [' }
			8 { 'Unmatched parenthesis' }
			9 { 'Unmatched repetition brace' }
			10 { 'Invalid repetition count' }
			11 { 'Invalid character range' }
			12 { 'Out of memory' }
			13 { 'Repetition operator without preceding expression' }
			else { 'Unknown regular expression error' }
		}
		bare_string_strlcpy(output, uint(text.str), size)
		return uint(text.len + 1)
	}
}
