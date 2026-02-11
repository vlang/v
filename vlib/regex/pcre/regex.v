/*
regex2 0.9.3 beta (VM Edition)

Copyright (c) 2026 Dario Deledda. All rights reserved.
Use of this source code is governed by an MIT license
that can be found in the LICENSE file.

This file contains regex module based on a Virtual Machine approach.

Features:
 - **Non-recursive VM**: Safe execution without stack overflow on complex patterns
 - UTF8 support
 - Literal characters, '.', '*', '{m,n}'
 - Short quantifiers: '?', '+'
 - Non-greedy quantifiers: '*?', '+?', '??'
 - Nested groups: '()'
 - Named groups: '(?P<name>...)'
 - Non-capturing groups: '(?:...)'
 - Character classes: \w, \W, \d, \D, \s, \S, \a, \A
 - Hex escapes: \xHH, \XHHHH
 - Alternation: '|'
 - Character classes: [abc], [^abc], [a-z]
 - Anchors: '^' (start of string), '$' (end of string)
 - Word boundaries: '\b' (word boundary), '\B' (non-word boundary)
 - Inline Flags:
   - '(?i)' Case insensitive
   - '(?m)' Multiline (anchors match newlines)
   - '(?s)' Dot-all (dot matches newline)

Functions:
 - `compile(pattern) !Regex` -> Compiles a pattern into a Regex object.
 - `(r Regex) find(text) ?Match` -> Finds the first match in a string.
 - `(r Regex) find_all(text) []Match` -> Finds all non-overlapping matches.
 - `(r Regex) find_from(text, start_index) ?Match` -> Finds the first match starting from a specific index.
 - `(r Regex) replace(text, repl) string` -> Finds and replaces the first match.
 - `(r Regex) fullmatch(text string) ?Match` -> Match exact strings.
 - `(r Regex) group_by_name(m Match, name string) string` -> Get text of a named group.
*/

module pcre

import strings
import strconv

/******************************************************************************
*
* Structs & Enums (VM Instructions)
*
******************************************************************************/

// InstType defines the operation code for the VM.
enum InstType {
	match             // Successfully matched the pattern
	char              // Match a specific character
	any               // Match any character (.)
	class             // Match character class ([...])
	split             // Branch execution (used for |, ?, *, +)
	jmp               // Jump to a specific instruction
	save              // Save the current cursor position to a capture group
	assert_start      // ^ (Absolute start)
	assert_end        // $ (Absolute end)
	assert_line_start // ^ (Multiline: start or after \n)
	assert_line_end   // $ (Multiline: end or before \n)
	assert_bound      // \b
	assert_nbound     // \B
	// Optimized built-ins
	match_digit     // \d
	match_non_digit // \D
	match_word      // \w
	match_non_word  // \W
	match_space     // \s
	match_non_space // \S
	match_lower     // \a
	match_upper     // \A
}

// Inst represents a single instruction in the compiled regex program.
struct Inst {
mut:
	typ         InstType
	val         rune   // For 'char'
	target_x    int    // Jump target 1 (primary)
	target_y    int    // Jump target 2 (alternative for split)
	group_idx   int    // For 'save'
	char_class  []rune // For 'class'
	inverted    bool   // For 'class' (negation)
	ignore_case bool
	dot_all     bool // For 'any'
}

// Thread represents the state of a single execution path in the VM.
struct Thread {
	pc       int   // Program Counter
	sp       int   // String Pointer
	captures []int // Captured group positions
}

// Regex holds the compiled bytecode program.
pub struct Regex {
pub:
	pattern      string
	prog         []Inst
	total_groups int
	group_map    map[string]int
}

// Match represents a successful match result.
pub struct Match {
pub:
	text   string
	start  int
	end    int
	groups []string
}

// --- AST Nodes ---

// Quantifier represents a repetition range.
struct Quantifier {
mut:
	min    int
	max    int  // -1 for infinity
	greedy bool // true = greedy, false = lazy
}

// Flags holds the current state of regex options.
struct Flags {
mut:
	ignore_case bool
	multiline   bool
	dot_all     bool
}

// NodeType defines the specific type of an AST node.
enum NodeType {
	chr
	any_char
	group
	alternation
	char_class
	start_of_string
	end_of_string
	word_boundary
	non_word_boundary
	word_char
	non_word_char
	digit
	non_digit
	whitespace
	non_whitespace
	lowercase_char // \a
	uppercase_char // \A
}

// Node represents a component in the regex Abstract Syntax Tree.
struct Node {
mut:
	typ                 NodeType
	chr                 rune
	quant               Quantifier
	nodes               []Node
	alternatives        [][]Node
	group_capture_index int = -1
	char_set            []rune
	inverted            bool
	ignore_case         bool
	multiline           bool
	dot_all             bool
}

/******************************************************************************
*
* Utilities
*
******************************************************************************/

// is_word_char checks if a rune is a word character (alphanumeric or underscore).
fn is_word_char(r rune) bool {
	return (r >= `a` && r <= `z`) || (r >= `A` && r <= `Z`) || (r >= `0` && r <= `9`) || r == `_`
}

// is_whitespace checks if a rune is a whitespace character.
fn is_whitespace(r rune) bool {
	return r in [` `, `\t`, `\n`, `\r`, `\v`, `\f`]
}

// read_rune decodes the next UTF-8 character from the string at the given index.
@[inline]
pub fn read_rune(s string, index int) (rune, int) {
	if index < s.len {
		b := s[index]
		if b < 0x80 {
			return rune(b), 1
		}
	} else {
		return 0, 0
	}

	mut res := 0
	mut ch_len := utf8_char_len(s[index])

	if ch_len == 1 {
		return u16(s[index]), 1
	}
	if ch_len > 1 && ch_len < 5 {
		mut lword := 0
		for i := 0; i < ch_len; i++ {
			lword = int(u32(lword) << 8 | u32(s[index + i]))
		}
		if ch_len == 2 {
			res = (lword & 0x1f00) >> 2 | (lword & 0x3f)
		} else if ch_len == 3 {
			res = (lword & 0x0f0000) >> 4 | (lword & 0x3f00) >> 2 | (lword & 0x3f)
		} else if ch_len == 4 {
			res = ((lword & 0x07000000) >> 6) | ((lword & 0x003f0000) >> 4) | ((lword & 0x00003F00) >> 2) | (lword & 0x0000003f)
		}
	}
	return res, ch_len
}

/******************************************************************************
*
* Compiler (Parser + CodeGen)
*
******************************************************************************/

// compile parses a pattern string and returns a compiled Regex struct.
pub fn compile(pattern string) !Regex {
	start_pos := 0
	start_group_counter := 0
	mut group_map := map[string]int{}
	initial_flags := Flags{
		ignore_case: false
		multiline:   false
		dot_all:     false
	}

	nodes, _, final_group_count := parse_nodes(pattern, start_pos, `\0`, start_group_counter,
		initial_flags, mut group_map)!

	// Ensure root node has quantifier 1,1
	root_node := Node{
		typ:                 .group
		nodes:               nodes
		group_capture_index: -1
		quant:               Quantifier{
			min:    1
			max:    1
			greedy: true
		}
	}

	mut compiler := Compiler{
		prog:      []Inst{}
		group_cnt: final_group_count
	}
	compiler.emit_node(root_node)
	compiler.emit(Inst{ typ: .match })

	return Regex{
		pattern:      pattern
		prog:         compiler.prog
		total_groups: final_group_count
		group_map:    group_map
	}
}

// Compiler manages the state of the bytecode generation process.
struct Compiler {
mut:
	prog      []Inst
	group_cnt int
}

// emit adds an instruction to the program and returns its index.
fn (mut c Compiler) emit(i Inst) int {
	c.prog << i
	return c.prog.len - 1
}

// emit_node generates VM instructions for a given AST node.
fn (mut c Compiler) emit_node(node Node) {
	for _ in 0 .. node.quant.min {
		c.emit_single_node_logic(node)
	}

	if node.quant.max == -1 {
		// Infinite quantifier
		split_idx := c.emit(Inst{ typ: .split })
		start_pc := c.prog.len

		c.emit_single_node_logic(node)

		c.emit(Inst{ typ: .jmp, target_x: split_idx })

		// In this VM, target_x is the first path taken (stack.pop order depends on implementation,
		// but standard here is target_x executed immediately, target_y pushed to stack).
		// Greedy: Prefer matching loop (start_pc) over exit.
		// Lazy: Prefer exit over matching loop.
		if node.quant.greedy {
			c.prog[split_idx].target_x = start_pc // Loop
			c.prog[split_idx].target_y = c.prog.len // Exit
		} else {
			c.prog[split_idx].target_x = c.prog.len // Exit
			c.prog[split_idx].target_y = start_pc // Loop
		}
	} else if node.quant.max > node.quant.min {
		// Finite range
		rem := node.quant.max - node.quant.min
		mut splits := []int{}

		for _ in 0 .. rem {
			idx := c.emit(Inst{ typ: .split })
			match_pc := c.prog.len

			// If greedy, we prefer to match the node (continue execution)
			// If lazy, we prefer to skip the match (jump to end)
			if node.quant.greedy {
				c.prog[idx].target_x = match_pc // Match node
			} else {
				c.prog[idx].target_y = match_pc // Match node (fallback)
			}

			c.emit_single_node_logic(node)
			splits << idx
		}

		end_pc := c.prog.len
		for idx in splits {
			if node.quant.greedy {
				c.prog[idx].target_y = end_pc // Skip match (fallback)
			} else {
				c.prog[idx].target_x = end_pc // Skip match (primary)
			}
		}
	}
}

// emit_single_node_logic handles the core logic for a node type without quantifiers.
fn (mut c Compiler) emit_single_node_logic(node Node) {
	match node.typ {
		.chr {
			c.emit(Inst{ typ: .char, val: node.chr, ignore_case: node.ignore_case })
		}
		.any_char {
			c.emit(Inst{ typ: .any, dot_all: node.dot_all })
		}
		.char_class {
			c.emit(Inst{
				typ:         .class
				char_class:  node.char_set
				inverted:    node.inverted
				ignore_case: node.ignore_case
			})
		}
		.group {
			if node.group_capture_index != -1 {
				c.emit(Inst{ typ: .save, group_idx: node.group_capture_index * 2 })
			}

			for child in node.nodes {
				c.emit_node(child)
			}

			if node.group_capture_index != -1 {
				c.emit(Inst{ typ: .save, group_idx: node.group_capture_index * 2 + 1 })
			}
		}
		.alternation {
			if node.alternatives.len == 0 {
				return
			}

			mut end_jumps := []int{}

			for i := 0; i < node.alternatives.len - 1; i++ {
				split_idx := c.emit(Inst{ typ: .split })
				c.prog[split_idx].target_x = c.prog.len

				for child in node.alternatives[i] {
					c.emit_node(child)
				}

				jmp_idx := c.emit(Inst{ typ: .jmp })
				end_jumps << jmp_idx
				c.prog[split_idx].target_y = c.prog.len
			}

			for child in node.alternatives[node.alternatives.len - 1] {
				c.emit_node(child)
			}

			end_pos := c.prog.len
			for idx in end_jumps {
				c.prog[idx].target_x = end_pos
			}
		}
		.start_of_string {
			if node.multiline {
				c.emit(Inst{ typ: .assert_line_start })
			} else {
				c.emit(Inst{ typ: .assert_start })
			}
		}
		.end_of_string {
			if node.multiline {
				c.emit(Inst{ typ: .assert_line_end })
			} else {
				c.emit(Inst{ typ: .assert_end })
			}
		}
		.word_boundary {
			c.emit(Inst{ typ: .assert_bound })
		}
		.non_word_boundary {
			c.emit(Inst{ typ: .assert_nbound })
		}
		// Map special types to Optimized opcodes
		.digit {
			c.emit(Inst{ typ: .match_digit })
		}
		.non_digit {
			c.emit(Inst{ typ: .match_non_digit })
		}
		.word_char {
			c.emit(Inst{ typ: .match_word })
		}
		.non_word_char {
			c.emit(Inst{ typ: .match_non_word })
		}
		.whitespace {
			c.emit(Inst{ typ: .match_space })
		}
		.non_whitespace {
			c.emit(Inst{ typ: .match_non_space })
		}
		.lowercase_char {
			c.emit(Inst{ typ: .match_lower })
		}
		.uppercase_char {
			c.emit(Inst{ typ: .match_upper })
		}
	}
}

// --- Parser ---

// parse_nodes parses the pattern string into a list of AST nodes.
fn parse_nodes(pattern string, pos_start int, terminator rune, group_counter_start int, passed_flags Flags, mut group_map map[string]int) !([]Node, int, int) {
	mut pos := pos_start
	mut group_counter := group_counter_start
	mut current_flags := passed_flags

	mut alternatives := [][]Node{}
	mut current_sequence := []Node{}

	for pos < pattern.len {
		chr, char_len := read_rune(pattern, pos)

		if chr == terminator {
			pos += char_len
			if alternatives.len > 0 {
				if current_sequence.len == 0 {
					return error('Pattern cannot end with an empty alternative')
				}
				alternatives << current_sequence
				return [
					Node{
						typ:          .alternation
						alternatives: alternatives
						quant:        Quantifier{1, 1, true}
					},
				], pos, group_counter
			}
			return current_sequence, pos, group_counter
		}

		if chr == `|` {
			pos += char_len
			if current_sequence.len == 0 {
				return error('Pattern cannot start with or contain an empty alternative')
			}
			alternatives << current_sequence
			current_sequence = []Node{}
			continue
		}

		mut parsed_nodes := []Node{}
		match chr {
			`^` {
				pos += char_len
				parsed_nodes << Node{
					typ:       .start_of_string
					multiline: current_flags.multiline
				}
			}
			`$` {
				pos += char_len
				parsed_nodes << Node{
					typ:       .end_of_string
					multiline: current_flags.multiline
				}
			}
			`.` {
				pos += char_len
				parsed_nodes << Node{
					typ:     .any_char
					dot_all: current_flags.dot_all
				}
			}
			`*`, `+`, `?`, `{` {
				return error('Quantifier must follow a token')
			}
			`(` {
				pos += char_len
				mut idx := -1
				mut capturing := true
				mut is_flag := false

				// Check extensions
				if pos < pattern.len && pattern[pos] == `?` {
					pos++
					if pos < pattern.len {
						next_char := pattern[pos]

						// Flags: (?i)
						if next_char in [`i`, `m`, `s`] {
							mut f_pos := pos
							for f_pos < pattern.len {
								f_char := pattern[f_pos]
								if f_char == `)` {
									pos = f_pos + 1
									is_flag = true
									break
								}
								match f_char {
									`i` { current_flags.ignore_case = true }
									`m` { current_flags.multiline = true }
									`s` { current_flags.dot_all = true }
									else { return error('Invalid flag') }
								}
								f_pos++
							}
							if is_flag {
								continue
							}
							// Skip loop, next token
						} else if next_char == `:` {
							capturing = false
							pos++
						} else if next_char == `P` {
							pos++ // P
							if pos < pattern.len && pattern[pos] == `<` {
								pos++
								end_name := pattern.index_after('>', pos) or { -1 }
								if end_name != -1 {
									name := pattern[pos..end_name]
									idx = group_counter
									group_map[name] = idx
									pos = end_name + 1
								} else {
									return error('Unclosed group name')
								}
							}
						}
					}
				}

				if capturing {
					if idx == -1 {
						idx = group_counter
					}
					group_counter++
				}

				sub_nodes, new_pos, new_cnt := parse_nodes(pattern, pos, `)`, group_counter,
					current_flags, mut group_map)!
				pos = new_pos
				group_counter = new_cnt

				parsed_nodes << Node{
					typ:                 .group
					nodes:               sub_nodes
					group_capture_index: idx
				}
			}
			`[` {
				pos += char_len
				end_pos := pattern.index_after(']', pos) or { return error('Unclosed char class') }
				content := pattern[pos..end_pos]
				pos = end_pos + 1

				mut inverted := false
				mut set_chars := []rune{}

				mut i := 0
				if content.len > 0 && content[0] == `^` {
					inverted = true
					i++
				}

				// Char class range parsing (simulated)
				for i < content.len {
					c, l := read_rune(content, i)

					// Check for range a-z
					if i + l + 1 < content.len && content[i + l] == `-` {
						end_c, end_l := read_rune(content, i + l + 1)
						// Expand range
						for r := c; r <= end_c; r++ {
							set_chars << r
							if current_flags.ignore_case {
								if r >= `a` && r <= `z` {
									set_chars << r - 32
								}
								if r >= `A` && r <= `Z` {
									set_chars << r + 32
								}
							}
						}
						i += l + 1 + end_l
						continue
					}

					set_chars << c
					if current_flags.ignore_case {
						if c >= `a` && c <= `z` {
							set_chars << c - 32
						}
						if c >= `A` && c <= `Z` {
							set_chars << c + 32
						}
					}
					i += l
				}
				parsed_nodes << Node{
					typ:         .char_class
					char_set:    set_chars
					inverted:    inverted
					ignore_case: current_flags.ignore_case
				}
			}
			`\\` {
				pos += char_len
				esc, el := read_rune(pattern, pos)
				pos += el
				match esc {
					`w` {
						parsed_nodes << Node{
							typ: .word_char
						}
					}
					`W` {
						parsed_nodes << Node{
							typ: .non_word_char
						}
					}
					`d` {
						parsed_nodes << Node{
							typ: .digit
						}
					}
					`D` {
						parsed_nodes << Node{
							typ: .non_digit
						}
					}
					`s` {
						parsed_nodes << Node{
							typ: .whitespace
						}
					}
					`S` {
						parsed_nodes << Node{
							typ: .non_whitespace
						}
					}
					`b` {
						parsed_nodes << Node{
							typ: .word_boundary
						}
					}
					`B` {
						parsed_nodes << Node{
							typ: .non_word_boundary
						}
					}
					`a` {
						parsed_nodes << Node{
							typ: .lowercase_char
						}
					}
					`A` {
						parsed_nodes << Node{
							typ: .uppercase_char
						}
					}
					else {
						parsed_nodes << Node{
							typ:         .chr
							chr:         esc
							ignore_case: current_flags.ignore_case
						}
					}
				}
			}
			else {
				pos += char_len
				parsed_nodes << Node{
					typ:         .chr
					chr:         chr
					ignore_case: current_flags.ignore_case
				}
			}
		}

		if parsed_nodes.len > 0 {
			mut q := Quantifier{1, 1, true}
			if pos < pattern.len {
				peek, pl := read_rune(pattern, pos)
				match peek {
					`*` {
						q = Quantifier{0, -1, true}
						pos += pl
					}
					`+` {
						q = Quantifier{1, -1, true}
						pos += pl
					}
					`?` {
						q = Quantifier{0, 1, true}
						pos += pl
					}
					`{` {
						// Parse {m,n}
						pos += pl
						end_q := pattern.index_after('}', pos) or {
							return error('Unclosed quantifier')
						}
						q_str := pattern[pos..end_q]
						parts := q_str.split(',')
						mut min := 0
						mut max := 0
						if parts.len == 1 {
							min = strconv.atoi(parts[0]) or { 0 }
							max = min
						} else {
							min = if parts[0].len > 0 { strconv.atoi(parts[0]) or { 0 } } else { 0 }
							max = if parts[1].len > 0 {
								strconv.atoi(parts[1]) or { -1 }
							} else {
								-1
							}
						}
						q = Quantifier{min, max, true}
						pos = end_q + 1
					}
					else {}
				}

				// Check for Non-Greedy modifier '?'
				// e.g. *?, +?, ??, {m,n}?
				if pos < pattern.len {
					peek_lazy, pl_lazy := read_rune(pattern, pos)
					if peek_lazy == `?` {
						q.greedy = false
						pos += pl_lazy
					}
				}
			}
			parsed_nodes[parsed_nodes.len - 1].quant = q
		}

		current_sequence << parsed_nodes
	}

	if alternatives.len > 0 {
		if current_sequence.len == 0 {
			return error('Pattern cannot end with an empty alternative')
		}
		alternatives << current_sequence
		return [
			Node{
				typ:          .alternation
				alternatives: alternatives
				quant:        Quantifier{1, 1, true}
			},
		], pos, group_counter
	}

	return current_sequence, pos, group_counter
}

/******************************************************************************
*
* Virtual Machine Executor
*
******************************************************************************/

// vm_match executes the compiled bytecode against the text starting at start_pos.
fn (r Regex) vm_match(text string, start_pos int) ?Match {
	mut stack := []Thread{cap: 32}

	mut captures := []int{len: r.total_groups * 2, init: -1}
	stack << Thread{
		pc:       0
		sp:       start_pos
		captures: captures
	}

	prog := r.prog

	for stack.len > 0 {
		mut t := stack.pop()
		mut pc := t.pc
		mut sp := t.sp
		mut cap := t.captures.clone()

		for {
			if pc >= prog.len {
				break
			}
			inst := unsafe { &prog[pc] }

			match inst.typ {
				.match {
					mut s_groups := []string{len: r.total_groups}
					for i := 0; i < r.total_groups; i++ {
						s := cap[i * 2]
						e := cap[i * 2 + 1]
						if s != -1 && e != -1 && s <= e {
							s_groups[i] = text[s..e]
						}
					}
					return Match{
						text:   text[start_pos..sp]
						start:  start_pos
						end:    sp
						groups: s_groups
					}
				}
				.char {
					if sp >= text.len {
						break
					}
					c, cl := read_rune(text, sp)

					mut matched := false
					if inst.ignore_case {
						mut c1 := c
						mut c2 := inst.val
						if c1 >= `a` && c1 <= `z` {
							c1 -= 32
						}
						if c2 >= `a` && c2 <= `z` {
							c2 -= 32
						}
						matched = (c1 == c2)
					} else {
						matched = (c == inst.val)
					}

					if matched {
						sp += cl
						pc++
					} else {
						break
					}
				}
				.any {
					if sp >= text.len {
						break
					}
					c, cl := read_rune(text, sp)
					if inst.dot_all || c != `\n` {
						sp += cl
						pc++
					} else {
						break
					}
				}
				.match_digit {
					if sp >= text.len {
						break
					}
					c, cl := read_rune(text, sp)
					if c >= `0` && c <= `9` {
						sp += cl
						pc++
					} else {
						break
					}
				}
				.match_non_digit {
					if sp >= text.len {
						break
					}
					c, cl := read_rune(text, sp)
					if !(c >= `0` && c <= `9`) {
						sp += cl
						pc++
					} else {
						break
					}
				}
				.match_word {
					if sp >= text.len {
						break
					}
					c, cl := read_rune(text, sp)
					if is_word_char(c) {
						sp += cl
						pc++
					} else {
						break
					}
				}
				.match_non_word {
					if sp >= text.len {
						break
					}
					c, cl := read_rune(text, sp)
					if !is_word_char(c) {
						sp += cl
						pc++
					} else {
						break
					}
				}
				.match_space {
					if sp >= text.len {
						break
					}
					c, cl := read_rune(text, sp)
					if is_whitespace(c) {
						sp += cl
						pc++
					} else {
						break
					}
				}
				.match_non_space {
					if sp >= text.len {
						break
					}
					c, cl := read_rune(text, sp)
					if !is_whitespace(c) {
						sp += cl
						pc++
					} else {
						break
					}
				}
				.match_lower {
					if sp >= text.len {
						break
					}
					c, cl := read_rune(text, sp)
					if c >= `a` && c <= `z` {
						sp += cl
						pc++
					} else {
						break
					}
				}
				.match_upper {
					if sp >= text.len {
						break
					}
					c, cl := read_rune(text, sp)
					if c >= `A` && c <= `Z` {
						sp += cl
						pc++
					} else {
						break
					}
				}
				.class {
					if sp >= text.len {
						break
					}
					c, cl := read_rune(text, sp)

					mut found := false
					for sc in inst.char_class {
						if sc == c {
							found = true
							break
						}
						if inst.ignore_case {
							if (c >= `a` && c <= `z` && sc == c - 32)
								|| (c >= `A` && c <= `Z` && sc == c + 32) {
								found = true
								break
							}
						}
					}

					if found != inst.inverted {
						sp += cl
						pc++
					} else {
						break
					}
				}
				.split {
					stack << Thread{
						pc:       inst.target_y
						sp:       sp
						captures: cap.clone()
					}
					pc = inst.target_x
				}
				.jmp {
					pc = inst.target_x
				}
				.save {
					if inst.group_idx < cap.len {
						cap[inst.group_idx] = sp
					}
					pc++
				}
				.assert_start {
					if sp == 0 {
						pc++
					} else {
						break
					}
				}
				.assert_end {
					if sp == text.len {
						pc++
					} else {
						break
					}
				}
				.assert_line_start {
					if sp == 0 {
						pc++
					} else {
						// Check previous char for newline
						// (Assumes 1-byte newline for simplicity, works for \n)
						if sp > 0 && text[sp - 1] == `\n` {
							pc++
						} else {
							break
						}
					}
				}
				.assert_line_end {
					if sp == text.len {
						pc++
					} else {
						// Check current char for newline
						if text[sp] == `\n` {
							pc++
						} else {
							break
						}
					}
				}
				.assert_bound, .assert_nbound {
					mut left_word := false
					if sp > 0 {
						lc, _ := read_rune(text, sp - 1)
						left_word = is_word_char(lc)
					}
					mut right_word := false
					if sp < text.len {
						rc, _ := read_rune(text, sp)
						right_word = is_word_char(rc)
					}

					is_b := left_word != right_word
					if (inst.typ == .assert_bound && is_b) || (inst.typ == .assert_nbound && !is_b) {
						pc++
					} else {
						break
					}
				}
			}
		}
	}

	return none
}

// fullmatch checks if the entire input text matches the regex pattern.
pub fn (r Regex) fullmatch(text string) ?Match {
	if res := r.vm_match(text, 0) {
		if res.end == text.len {
			return res
		}
	}
	return none
}

// replace finds the first match in text and replaces it with repl.
// Supports $1, $2, etc. in repl for group substitution.
pub fn (r Regex) replace(text string, repl string) string {
	match_res := r.find(text) or { return text }
	mut sb := strings.new_builder(text.len)
	sb.write_string(text[0..match_res.start])
	mut i := 0
	for i < repl.len {
		if repl[i] == `$` && i + 1 < repl.len && repl[i + 1].is_digit() {
			group_index := repl[i + 1].ascii_str().int() - 1 // $1 -> idx 0
			if group_index >= 0 && group_index < match_res.groups.len {
				sb.write_string(match_res.groups[group_index])
			}
			i += 2
		} else {
			sb.write_byte(repl[i])
			i++
		}
	}
	sb.write_string(text[match_res.end..])
	return sb.str()
}

// group_by_name retrieves the captured text for a named group from a Match.
pub fn (r Regex) group_by_name(m Match, name string) string {
	if idx := r.group_map[name] {
		if idx >= 0 && idx < m.groups.len {
			return m.groups[idx]
		}
	}
	return ''
}

// find scans the string `text` for the first occurrence of a pattern match.
pub fn (r Regex) find(text string) ?Match {
	if r.prog.len > 0 && r.prog[0].typ == .assert_start {
		return r.vm_match(text, 0)
	}

	for i := 0; i <= text.len; i++ {
		if i > 0 && i < text.len && (text[i] & 0xC0) == 0x80 {
			continue
		}

		if res := r.vm_match(text, i) {
			return res
		}
	}
	return none
}

// find_all returns a list of all non-overlapping matches in the string.
pub fn (r Regex) find_all(text string) []Match {
	mut matches := []Match{}
	mut i := 0

	if r.prog.len > 0 && r.prog[0].typ == .assert_start {
		if m := r.vm_match(text, 0) {
			matches << m
		}
		return matches
	}

	for i <= text.len {
		if i > 0 && i < text.len && (text[i] & 0xC0) == 0x80 {
			i++
			continue
		}

		if m := r.vm_match(text, i) {
			matches << m
			if m.end > i {
				i = m.end
			} else {
				i++
			}
		} else {
			i++
		}
	}

	return matches
}

// find_from finds the first match starting search from a specific index.
pub fn (r Regex) find_from(text string, start_index int) ?Match {
	if start_index < 0 || start_index > text.len {
		return none
	}

	for i := start_index; i <= text.len; i++ {
		if i > 0 && i < text.len && (text[i] & 0xC0) == 0x80 {
			continue
		}
		if res := r.vm_match(text, i) {
			return res
		}
	}
	return none
}
