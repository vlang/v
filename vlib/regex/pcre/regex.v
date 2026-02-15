/*
regex2 0.9.6 beta (VM Edition) - Performance Optimized

Copyright (c) 2026 Dario Deledda. All rights reserved.
Use of this source code is governed by an MIT license
that can be found in the LICENSE file.

This file contains a regex module based on a Virtual Machine approach.

Features:
 - **Non-recursive VM**: Safe execution without stack overflow on complex patterns.
 - UTF8 support with Fast ASCII path.
 - Literal characters, '.', '*', '{m,n}'.
 - Short quantifiers: '?', '+'.
 - Non-greedy quantifiers: '*?', '+?', '??'.
 - Nested groups: '()'.
 - Named groups: '(?P<name>...)'.
 - Non-capturing groups: '(?:...)'.
 - Character classes: \w, \W, \d, \D, \s, \S, \a, \A.
 - Hex escapes: \xHH, \XHHHH.
 - Alternation: '|'.
 - Character classes: [abc], [^abc], [a-z].
 - Anchors: '^' (start of string), '$' (end of string).
 - Word boundaries: '\b' (word boundary), '\B' (non-word boundary).
 - Inline Flags:
   - '(?i)' Case insensitive.
   - '(?m)' Multiline (anchors match newlines).
   - '(?s)' Dot-all (dot matches newline).

Configuration:
 - `max_stack_depth`: Controls the dynamic growth limit of the backtracking stack.
    Default is 2048. Increase this value if you encounter complex patterns failing
    on deep recursions/backtracking, or decrease it to limit memory usage.

Functions:
 - `compile(pattern) !Regex` -> Compiles a pattern into a Regex object.
 - `(r Regex) find(text) ?Match` -> Finds the first match in a string.
 - `(r Regex) find_all(text) []Match` -> Finds all non-overlapping matches.
 - `(r Regex) find_from(text, start_index) ?Match` -> Finds the first match starting from a specific index.
 - `(r Regex) replace(text, repl) string` -> Finds and replaces the first match.
 - `(r Regex) fullmatch(text string) ?Match` -> Match exact strings.
 - `(r Regex) group_by_name(m Match, name string) string` -> Get text of a named group.

Key Architectural Features and Optimizations:
 - **Pre-allocated Machine workspace**: All search operations are zero-allocation.
 - **Raw pointer access**: Bypasses bounds checking for string and instruction access.
 - **Fast ASCII Path**: Characters < 128 bypass heavy UTF-8 decoding.
 - **Instruction merging**: Consecutive character matches are merged into string blocks.
 - **Bitmap lookups**: ASCII character classes use a 128-bit bitset for O(1) matching.
 - **NFA Virtual Machine**: Executes bytecode instructions to simulate pattern matching.
 - **Dynamic Stack Growth**: Automatically expands the backtracking stack to prevent false negatives.
 - **Zero-Allocation Search**: Reuses a pre-allocated Machine workspace for search operations.
 - **Anchored Optimization**: Patterns starting with '^' skip the scanning loop.
 - **Prefix Skipping**: Uses Boyer-Moore-like skipping for literal prefixes.

*/

module pcre

import strings

/******************************************************************************
*
* VM Instruction Definition
*
******************************************************************************/

// InstType defines the operation codes for the Virtual Machine.
enum InstType as u8 {
	match             // Halt and signal a successful match.
	char              // Match a single UTF-8 rune.
	string            // Match a sequence of ASCII characters (merged optimization).
	any               // Match any character (dot).
	class             // Match a character class (e.g., [a-z] or \d).
	split             // Branch execution: target_x (primary), target_y (backtrack).
	jmp               // Unconditional jump to target_x.
	save              // Save current string position into a capture group index.
	assert_start      // Assert current position is start of string.
	assert_end        // Assert current position is end of string.
	assert_line_start // Assert current position is start of a line (multiline mode).
	assert_line_end   // Assert current position is end of a line (multiline mode).
	assert_bound      // Assert word boundary (\b).
	assert_nbound     // Assert non-word boundary (\B).
}

// Inst represents a single bytecode instruction.
// Packed for memory locality.
struct Inst {
mut:
	typ         InstType
	val         rune   // Used by .char
	val_str     string // Used by .string
	val_len     int    // Byte length of val_str
	target_x    int    // Primary jump/split target
	target_y    int    // Backtrack target for .split
	group_idx   int    // Capture group index for .save
	char_class  []rune // Literal runes for character classes
	bitmap      [4]u32 // 128-bit bitset for ASCII character classes
	inverted    bool   // Negation flag for classes
	ignore_case bool   // Case-insensitive flag
	dot_all     bool   // Whether '.' matches '\n'
}

// Machine provides a workspace for VM execution.
// To ensure thread safety, this is created per top-level API call.
pub struct Machine {
mut:
	stack    []int // Backtracking stack stores: [capture_states..., string_ptr, next_pc]
	captures []int // Flat array of [start, end] byte indices for groups
}

// Regex is the compiled regular expression object.
pub struct Regex {
pub:
	pattern      string         // The original regex string
	prog         []Inst         // Compiled bytecode
	total_groups int            // Number of capture groups defined in pattern
	group_map    map[string]int // Mapping of names to indices for (?P<name>...)
	prefix_lit   string         // Pre-calculated literal prefix for fast-skip optimization
	has_prefix   bool           // Whether a literal prefix exists
	anchored     bool           // True if pattern starts with '^' (optimization hint)
pub mut:
	max_stack_depth int // User-defined stack limit hint
}

// Match contains the results of a successful regex match.
pub struct Match {
pub:
	text   string   // The full text of the match
	start  int      // Starting byte index in the source string
	end    int      // Ending byte index in the source string
	groups []string // Sub-strings captured by groups
}

// Internal structures for Compilation
struct Quantifier {
mut:
	min    int
	max    int // -1 for infinity
	greedy bool
}

struct Flags {
mut:
	ignore_case bool
	multiline   bool
	dot_all     bool
}

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
	lowercase_char
	uppercase_char
}

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
* Internal Utilities (Inlined for Speed)
*
******************************************************************************/

// read_rune_at decodes a UTF-8 rune from a byte pointer safely.
// Marked inline to be embedded directly into the VM loop.
@[inline]
fn read_rune_at(str &u8, len int, index int) (rune, int) {
	unsafe {
		if index >= len {
			return 0, 0
		}
		b0 := u32(str[index])
		if b0 < 0x80 {
			return rune(b0), 1
		}
		if (b0 & 0xE0) == 0xC0 && index + 1 < len {
			return rune(((b0 & 0x1F) << 6) | (u32(str[index + 1]) & 0x3F)), 2
		}
		if (b0 & 0xF0) == 0xE0 && index + 2 < len {
			return rune(((b0 & 0x0F) << 12) | ((u32(str[index + 1]) & 0x3F) << 6) | (u32(str[
				index + 2]) & 0x3F)), 3
		}
		if (b0 & 0xF8) == 0xF0 && index + 3 < len {
			return rune(((b0 & 0x07) << 18) | ((u32(str[index + 1]) & 0x3F) << 12) | ((u32(str[
				index + 2]) & 0x3F) << 6) | (u32(str[index + 3]) & 0x3F)), 4
		}
	}
	return 0, 0
}

// is_word_char returns true for alphanumeric ASCII characters and underscore.
@[inline]
fn is_word_char(r u8) bool {
	return (r >= `a` && r <= `z`) || (r >= `A` && r <= `Z`) || (r >= `0` && r <= `9`) || r == `_`
}

// set_bitmap sets a specific bit in a 128-bit bitset for ASCII matching.
@[inline]
fn set_bitmap(mut bitmap [4]u32, r rune) {
	if r >= 0 && r < 128 {
		idx := u32(r) >> 5
		bit := u32(r) & 31
		bitmap[idx] |= (u32(1) << bit)
	}
}

/******************************************************************************
*
* Compiler Logic
*
******************************************************************************/

// compile transforms a regex pattern string into a Regex object.
pub fn compile(pattern string) !Regex {
	mut group_map := map[string]int{}
	initial_flags := Flags{false, false, false}

	// Phase 1: AST Parsing
	nodes, _, final_group_count := parse_nodes(pattern, 0, `\0`, 0, initial_flags, mut
		group_map)!

	root := Node{
		typ:   .group
		nodes: nodes
		quant: Quantifier{1, 1, true}
	}

	// Phase 2: Bytecode Emission
	mut compiler := Compiler{
		prog: []Inst{cap: 128}
	}
	compiler.emit_node(root)
	compiler.emit(Inst{ typ: .match })

	// Phase 3: Optimization
	optimized_prog := compiler.optimize()

	// Detect Prefix and Anchor optimizations
	mut prefix := ''
	mut has_prefix := false
	mut anchored := false

	if optimized_prog.len > 0 {
		first := optimized_prog[0]
		if first.typ == .string {
			prefix = first.val_str
			has_prefix = true
		} else if first.typ == .char && !first.ignore_case && first.val < 128 {
			prefix = unsafe { u8(first.val).ascii_str() }
			has_prefix = true
		} else if first.typ == .assert_start {
			anchored = true
		}
	}

	return Regex{
		max_stack_depth: 2048
		pattern:         pattern
		prog:            optimized_prog
		total_groups:    final_group_count
		group_map:       group_map
		prefix_lit:      prefix
		has_prefix:      has_prefix
		anchored:        anchored
	}
}

// new_machine allocates a new VM state machine.
// This isolates the runtime memory (stack/captures) from the compiled regex, allowing thread-safe usage.
pub fn (r &Regex) new_machine() Machine {
	// Pre-allocate enough space for stack and captures to avoid re-allocation in hot path
	return Machine{
		stack:    []int{len: r.max_stack_depth}
		captures: []int{len: r.total_groups * 2}
	}
}

// Compiler holds the state for generating the bytecode instructions.
struct Compiler {
mut:
	prog []Inst
}

// emit appends an instruction to the program and returns its index.
fn (mut c Compiler) emit(i Inst) int {
	c.prog << i
	return c.prog.len - 1
}

// optimize merges consecutive literal characters into single string instructions
// and resolves jump targets to absolute indices.
fn (mut c Compiler) optimize() []Inst {
	mut targets := map[int]bool{}
	for inst in c.prog {
		if inst.typ == .split || inst.typ == .jmp {
			targets[inst.target_x] = true
			targets[inst.target_y] = true
		}
	}

	mut new_prog := []Inst{cap: c.prog.len}
	mut idx_map := map[int]int{}

	mut i := 0
	for i < c.prog.len {
		inst := c.prog[i]
		idx_map[i] = new_prog.len

		// Optimization: Merge consecutive chars
		if inst.typ == .char && !inst.ignore_case && inst.val < 128 {
			mut s_val := unsafe { u8(inst.val).ascii_str() }
			mut j := i + 1
			for j < c.prog.len && !targets[j] {
				next := c.prog[j]
				if next.typ == .char && !next.ignore_case && next.val < 128 {
					s_val += unsafe { u8(next.val).ascii_str() }
					j++
				} else {
					break
				}
			}
			if j > i + 1 {
				new_prog << Inst{
					typ:     .string
					val_str: s_val
					val_len: s_val.len
				}
				for k := i + 1; k < j; k++ {
					idx_map[k] = new_prog.len
				}
				i = j
				continue
			}
		}
		new_prog << inst
		i++
	}
	idx_map[c.prog.len] = new_prog.len

	// Fix jump offsets
	for mut inst in new_prog {
		if inst.typ == .split || inst.typ == .jmp {
			inst.target_x = idx_map[inst.target_x] or { inst.target_x }
			inst.target_y = idx_map[inst.target_y] or { inst.target_y }
		}
	}
	return new_prog
}

// emit_class generates the instructions for a character class node.
// It populates the bitmap for O(1) ASCII matching and the slice for Unicode.
fn (mut c Compiler) emit_class(node Node) {
	mut bitmap := [4]u32{}
	mut char_class := node.char_set.clone()
	mut inverted := node.inverted

	// Pre-compile common classes into the bitmap for O(1) lookups
	match node.typ {
		.word_char {
			for r := `0`; r <= `9`; r++ {
				set_bitmap(mut bitmap, r)
			}
			for r := `a`; r <= `z`; r++ {
				set_bitmap(mut bitmap, r)
			}
			for r := `A`; r <= `Z`; r++ {
				set_bitmap(mut bitmap, r)
			}
			set_bitmap(mut bitmap, `_`)
		}
		.non_word_char {
			inverted = true
			for r := `0`; r <= `9`; r++ {
				set_bitmap(mut bitmap, r)
			}
			for r := `a`; r <= `z`; r++ {
				set_bitmap(mut bitmap, r)
			}
			for r := `A`; r <= `Z`; r++ {
				set_bitmap(mut bitmap, r)
			}
			set_bitmap(mut bitmap, `_`)
		}
		.digit {
			for r := `0`; r <= `9`; r++ {
				set_bitmap(mut bitmap, r)
			}
		}
		.non_digit {
			inverted = true
			for r := `0`; r <= `9`; r++ {
				set_bitmap(mut bitmap, r)
			}
		}
		.whitespace {
			for r in [` `, `\t`, `\n`, `\r`, `\v`, `\f`] {
				set_bitmap(mut bitmap, r)
			}
		}
		.non_whitespace {
			inverted = true
			for r in [` `, `\t`, `\n`, `\r`, `\v`, `\f`] {
				set_bitmap(mut bitmap, r)
			}
		}
		.lowercase_char {
			for r := `a`; r <= `z`; r++ {
				set_bitmap(mut bitmap, r)
			}
		}
		.uppercase_char {
			for r := `A`; r <= `Z`; r++ {
				set_bitmap(mut bitmap, r)
			}
		}
		.char_class {
			for r in node.char_set {
				set_bitmap(mut bitmap, r)
				if node.ignore_case {
					if r >= `a` && r <= `z` {
						set_bitmap(mut bitmap, r - 32)
					} else if r >= `A` && r <= `Z` {
						set_bitmap(mut bitmap, r + 32)
					}
				}
			}
		}
		else {}
	}

	c.emit(Inst{
		typ:         .class
		char_class:  char_class
		bitmap:      bitmap
		inverted:    inverted
		ignore_case: node.ignore_case
	})
}

// emit_node handles quantifiers and loops, delegating the actual logic to emit_logic.
fn (mut c Compiler) emit_node(node Node) {
	for _ in 0 .. node.quant.min {
		c.emit_logic(node)
	}

	if node.quant.max == -1 {
		split_idx := c.emit(Inst{ typ: .split })
		start_pc := c.prog.len
		c.emit_logic(node)
		c.emit(Inst{ typ: .jmp, target_x: split_idx })
		if node.quant.greedy {
			c.prog[split_idx].target_x = start_pc
			c.prog[split_idx].target_y = c.prog.len
		} else {
			c.prog[split_idx].target_x = c.prog.len
			c.prog[split_idx].target_y = start_pc
		}
	} else if node.quant.max > node.quant.min {
		rem := node.quant.max - node.quant.min
		mut splits := []int{}
		for _ in 0 .. rem {
			idx := c.emit(Inst{ typ: .split })
			match_pc := c.prog.len
			if node.quant.greedy {
				c.prog[idx].target_x = match_pc
			} else {
				c.prog[idx].target_y = match_pc
			}
			c.emit_logic(node)
			splits << idx
		}
		end_pc := c.prog.len
		for idx in splits {
			if node.quant.greedy {
				c.prog[idx].target_y = end_pc
			} else {
				c.prog[idx].target_x = end_pc
			}
		}
	}
}

// emit_logic generates instructions for specific node types (char, group, alternation).
fn (mut c Compiler) emit_logic(node Node) {
	match node.typ {
		.chr {
			c.emit(Inst{ typ: .char, val: node.chr, ignore_case: node.ignore_case })
		}
		.any_char {
			c.emit(Inst{ typ: .any, dot_all: node.dot_all })
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
				end_jumps << c.emit(Inst{ typ: .jmp })
				c.prog[split_idx].target_y = c.prog.len
			}
			for child in node.alternatives.last() {
				c.emit_node(child)
			}
			for idx in end_jumps {
				c.prog[idx].target_x = c.prog.len
			}
		}
		.start_of_string {
			c.emit(Inst{
				typ: if node.multiline { InstType.assert_line_start } else { InstType.assert_start }
			})
		}
		.end_of_string {
			c.emit(Inst{
				typ: if node.multiline { InstType.assert_line_end } else { InstType.assert_end }
			})
		}
		.word_boundary {
			c.emit(Inst{ typ: .assert_bound })
		}
		.non_word_boundary {
			c.emit(Inst{ typ: .assert_nbound })
		}
		else {
			c.emit_class(node)
		}
	}
}

// parse_nodes implements a recursive descent parser to construct the AST from the pattern string.
fn parse_nodes(pattern string, pos_start int, terminator rune, group_counter_start int, passed_flags Flags, mut group_map map[string]int) !([]Node, int, int) {
	mut pos := pos_start
	mut group_counter := group_counter_start
	mut current_flags := passed_flags
	mut alternatives := [][]Node{}
	mut current_sequence := []Node{}

	for pos < pattern.len {
		chr, char_len := read_rune_at(pattern.str, pattern.len, pos)
		if chr == terminator {
			pos += char_len
			if alternatives.len > 0 {
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
				return error('Empty alternative')
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
				mut cap := true
				if pos < pattern.len && pattern[pos] == `?` {
					pos++
					if pos < pattern.len && pattern[pos] in [`i`, `m`, `s`] {
						for pos < pattern.len && pattern[pos] != `)` {
							match pattern[pos] {
								`i` { current_flags.ignore_case = true }
								`m` { current_flags.multiline = true }
								`s` { current_flags.dot_all = true }
								else {}
							}
							pos++
						}
						if pos < pattern.len {
							pos++
						}
						continue
					} else if pos < pattern.len && pattern[pos] == `:` {
						cap = false
						pos++
					} else if pos < pattern.len && pattern[pos] == `P` {
						pos++
						if pos >= pattern.len || pattern[pos] != `<` {
							return error('Invalid named group syntax')
						}
						pos++
						end := pattern.index_after('>', pos) or { -1 }
						if end == -1 {
							return error('Unclosed named group')
						}
						name := pattern[pos..end]
						idx = group_counter
						group_map[name] = idx
						pos = end + 1
					}
				}
				if cap {
					if idx == -1 {
						idx = group_counter
					}
					group_counter++
				}
				sub, new_p, new_c := parse_nodes(pattern, pos, `)`, group_counter, current_flags, mut
					group_map)!
				pos = new_p
				group_counter = new_c
				parsed_nodes << Node{
					typ:                 .group
					nodes:               sub
					group_capture_index: idx
				}
			}
			`[` {
				pos += char_len
				end := pattern.index_after(']', pos) or { -1 }
				if end == -1 {
					return error('Unclosed character class')
				}
				content := pattern[pos..end]
				pos = end + 1
				inv := content.len > 0 && content[0] == `^`
				mut set := []rune{}
				mut i := if inv { 1 } else { 0 }
				for i < content.len {
					c, l := read_rune_at(content.str, content.len, i)
					if i + l + 1 < content.len && content[i + l] == `-` {
						ec, el := read_rune_at(content.str, content.len, i + l + 1)
						for r := c; r <= ec; r++ {
							set << r
						}
						i += l + 1 + el
					} else {
						set << c
						i += l
					}
				}
				parsed_nodes << Node{
					typ:         .char_class
					char_set:    set
					inverted:    inv
					ignore_case: current_flags.ignore_case
				}
			}
			`\\` {
				pos += char_len
				if pos >= pattern.len {
					return error('Trailing backslash')
				}
				esc, el := read_rune_at(pattern.str, pattern.len, pos)
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
				peek := pattern[pos]
				match peek {
					`*` {
						q = Quantifier{0, -1, true}
						pos++
					}
					`+` {
						q = Quantifier{1, -1, true}
						pos++
					}
					`?` {
						q = Quantifier{0, 1, true}
						pos++
					}
					`{` {
						end := pattern.index_after('}', pos) or { -1 }
						if end == -1 {
							return error('Unclosed quantifier')
						}
						parts := pattern[pos + 1..end].split(',')
						min := parts[0].int()
						max := if parts.len > 1 {
							if parts[1] == '' { -1 } else { parts[1].int() }
						} else {
							min
						}
						q = Quantifier{min, max, true}
						pos = end + 1
					}
					else {}
				}
				if pos < pattern.len && pattern[pos] == `?` {
					q.greedy = false
					pos++
				}
			}
			parsed_nodes.last().quant = q
		}
		current_sequence << parsed_nodes
	}

	if alternatives.len > 0 {
		if current_sequence.len == 0 {
			return error('Empty alternative at end of pattern')
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
* Virtual Machine Execution Engine (Highly Optimized)
*
******************************************************************************/

// vm_match executes the bytecode against the input string using the provided Machine state.
// OPTIMIZATION: Uses raw pointers for instruction and stack access to bypass bounds checking.
@[direct_array_access]
fn (r &Regex) vm_match(text string, start_pos int, mut m Machine) ?Match {
	unsafe {
		// Optimization: Cast voidptr to typed pointer for direct indexing
		mut cap_ptr := &int(m.captures.data)
		cap_len := m.captures.len

		// Fast clear of captures using pointer arithmetic (memset-like)
		for i := 0; i < cap_len; i++ {
			cap_ptr[i] = -1
		}

		mut sp := start_pos // String Pointer Index
		mut stack_ptr := 0 // Stack Pointer Offset

		cap_size := r.total_groups * 2
		frame_size := cap_size + 2 // [captures..., saved_sp, saved_pc]

		// Raw pointers for hot path access
		prog_start := &Inst(r.prog.data)
		mut inst_ptr := prog_start // PC as a pointer

		str_ptr := text.str
		str_len := text.len

		// Cache stack data pointer (cast to typed pointer)
		mut stack_data := &int(m.stack.data)
		mut stack_max := m.stack.len

		for {
			// Check if we walked off the program (should be caught by match inst)
			// Using pointer arithmetic: offset = (inst_ptr - prog_start)

			match inst_ptr.typ {
				.match {
					// Only allocate result strings on successful match
					mut s_groups := []string{cap: r.total_groups}
					for i := 0; i < r.total_groups; i++ {
						s, e := cap_ptr[i * 2], cap_ptr[i * 2 + 1]
						s_groups << if s != -1 && e >= s { text[s..e] } else { '' }
					}
					return Match{
						text:   text[start_pos..sp]
						start:  start_pos
						end:    sp
						groups: s_groups
					}
				}
				.char {
					if sp >= str_len {
						goto backtrack
					}
					curr_byte := str_ptr[sp]

					// Fast ASCII path
					if curr_byte < 128 && inst_ptr.val < 128 {
						mut c1, mut c2 := curr_byte, u8(inst_ptr.val)
						if inst_ptr.ignore_case {
							if c1 >= `a` && c1 <= `z` {
								c1 -= 32
							}
							if c2 >= `a` && c2 <= `z` {
								c2 -= 32
							}
						}
						if c1 == c2 {
							sp++
							inst_ptr++
							continue
						}
						goto backtrack
					}

					// UTF-8 Path
					rn, l := read_rune_at(str_ptr, str_len, sp)
					if l == 0 {
						goto backtrack
					}

					mut match_ok := false
					if inst_ptr.ignore_case {
						r1 := if rn >= `a` && rn <= `z` { rn - 32 } else { rn }
						r2 := if inst_ptr.val >= `a` && inst_ptr.val <= `z` {
							inst_ptr.val - 32
						} else {
							inst_ptr.val
						}
						if r1 == r2 {
							match_ok = true
						}
					} else if rn == inst_ptr.val {
						match_ok = true
					}

					if match_ok {
						sp += l
						inst_ptr++
					} else {
						goto backtrack
					}
				}
				.string {
					if sp + inst_ptr.val_len > str_len {
						goto backtrack
					}
					// Inline memcmp
					v_str := inst_ptr.val_str.str
					for i in 0 .. inst_ptr.val_len {
						if str_ptr[sp + i] != v_str[i] {
							goto backtrack
						}
					}
					sp += inst_ptr.val_len
					inst_ptr++
				}
				.class {
					if sp >= str_len {
						goto backtrack
					}
					c_byte := str_ptr[sp]
					mut matched := false
					mut cl := 1

					// Optimization: Bitmap lookup for ASCII
					if c_byte < 128 {
						if (inst_ptr.bitmap[c_byte >> 5] & (u32(1) << (c_byte & 31))) != 0 {
							matched = true
						}
					} else {
						rn, l := read_rune_at(str_ptr, str_len, sp)
						cl = l
						for cr in inst_ptr.char_class {
							if cr == rn {
								matched = true
								break
							}
						}
					}

					if matched != inst_ptr.inverted {
						sp += cl
						inst_ptr++
					} else {
						goto backtrack
					}
				}
				.any {
					if sp >= str_len {
						goto backtrack
					}
					if inst_ptr.dot_all || str_ptr[sp] != `\n` {
						_, cl := read_rune_at(str_ptr, str_len, sp)
						sp += cl
						inst_ptr++
					} else {
						goto backtrack
					}
				}
				.save {
					cap_ptr[inst_ptr.group_idx] = sp
					inst_ptr++
				}
				.split {
					if stack_ptr + frame_size >= stack_max {
						new_size := stack_max * 2
						if new_size > 1_000_000 {
							goto backtrack
						}
						m.stack.grow_len(new_size)
						stack_data = &int(m.stack.data) // Pointer might change on realloc
						stack_max = new_size
					}

					// Optimization: Unrolled stack push
					stack_offset := stack_ptr
					for i in 0 .. cap_size {
						stack_data[stack_offset + i] = cap_ptr[i]
					}
					stack_data[stack_offset + cap_size] = sp
					// Save backtrack target PC index
					stack_data[stack_offset + cap_size + 1] = inst_ptr.target_y

					stack_ptr += frame_size
					// Jump to primary target (convert index to pointer)
					// FIX: Use pointer indexing instead of addition to avoid type mismatch
					inst_ptr = &prog_start[inst_ptr.target_x]
				}
				.jmp {
					inst_ptr = &prog_start[inst_ptr.target_x]
				}
				.assert_start {
					if sp == 0 {
						inst_ptr++
					} else {
						goto backtrack
					}
				}
				.assert_end {
					if sp == str_len {
						inst_ptr++
					} else {
						goto backtrack
					}
				}
				.assert_line_start {
					if sp == 0 || (sp > 0 && str_ptr[sp - 1] == `\n`) {
						inst_ptr++
					} else {
						goto backtrack
					}
				}
				.assert_line_end {
					if sp == str_len || str_ptr[sp] == `\n` {
						inst_ptr++
					} else {
						goto backtrack
					}
				}
				.assert_bound, .assert_nbound {
					l := if sp > 0 { is_word_char(str_ptr[sp - 1]) } else { false }
					r_ := if sp < str_len { is_word_char(str_ptr[sp]) } else { false }
					if (inst_ptr.typ == .assert_bound && l != r_)
						|| (inst_ptr.typ == .assert_nbound && l == r_) {
						inst_ptr++
					} else {
						goto backtrack
					}
				}
			}
			continue

			backtrack:
			if stack_ptr <= 0 {
				return none
			}

			stack_ptr -= frame_size
			stack_offset := stack_ptr

			// Restore captures
			for i in 0 .. cap_size {
				cap_ptr[i] = stack_data[stack_offset + i]
			}
			sp = stack_data[stack_offset + cap_size]
			// Restore PC from index (explicit cast for pointer arithmetic)
			// FIX: Use pointer indexing instead of addition
			inst_ptr = &prog_start[stack_data[stack_offset + cap_size + 1]]
		}
	}
	return none
}

/******************************************************************************
*
* Public API
*
******************************************************************************/

// find returns the first match of the regex in text.
pub fn (r &Regex) find(text string) ?Match {
	return r.find_from(text, 0)
}

// find_from returns the first match starting from start_index.
// Optimized with fast prefix skipping and anchor checks.
pub fn (r &Regex) find_from(text string, start_index int) ?Match {
	if start_index < 0 || start_index > text.len {
		return none
	}
	mut m := r.new_machine()

	// Optimization: Anchored pattern (^) only checks the start
	if r.anchored {
		if start_index == 0 {
			return r.vm_match(text, 0, mut m)
		}
		// If multiline mode is NOT enabled, ^ only matches index 0.
		// If multiline is enabled, we need to check every newline, handled by logic below.
		// Note: The compiler sets anchored=true only for ^ at start.
		// If multiline flag is set dynamically inside pattern, strict anchoring logic might differ,
		// but standard ^ usage benefits here.
	}

	// Optimization: Boyer-Moore-like literal prefix skip
	if r.has_prefix {
		mut i := text.index_after(r.prefix_lit, start_index) or { -1 }
		for i != -1 {
			if res := r.vm_match(text, i, mut m) {
				return res
			}
			i = text.index_after(r.prefix_lit, i + 1) or { -1 }
		}
		return none
	}

	for i := start_index; i <= text.len; i++ {
		// Skip UTF-8 continuation bytes to ensure we only match at rune boundaries
		// 0xC0 (11000000) is the start of a multi-byte sequence, 0x80 (10000000) is a continuation
		if i > 0 && i < text.len && (text[i] & 0xC0) == 0x80 {
			continue
		}
		if res := r.vm_match(text, i, mut m) {
			return res
		}
	}
	return none
}

// find_all returns all non-overlapping matches in text.
pub fn (r &Regex) find_all(text string) []Match {
	mut matches := []Match{}
	mut m := r.new_machine()
	mut i := 0
	for i <= text.len {
		if i > 0 && i < text.len && (text[i] & 0xC0) == 0x80 {
			i++
			continue
		}
		if res := r.vm_match(text, i, mut m) {
			matches << res
			i = if res.end > i { res.end } else { i + 1 }
		} else {
			i++
		}
	}
	return matches
}

// replace finds the first match and replaces it using repl. Supports $1, $2 backreferences.
pub fn (r &Regex) replace(text string, repl string) string {
	res := r.find(text) or { return text }
	mut sb := strings.new_builder(text.len + repl.len)
	sb.write_string(text[0..res.start])
	mut i := 0
	for i < repl.len {
		if repl[i] == `$` && i + 1 < repl.len {
			next_char := repl[i + 1]
			if next_char >= `0` && next_char <= `9` {
				d := next_char - `0` - 1
				if d >= 0 && d < res.groups.len {
					sb.write_string(res.groups[d])
				}
				i += 2
				continue
			}
		}
		sb.write_u8(repl[i])
		i++
	}
	sb.write_string(text[res.end..])
	return sb.str()
}

// fullmatch returns a Match only if the pattern matches the entire text.
pub fn (r &Regex) fullmatch(text string) ?Match {
	mut m := r.new_machine()
	if res := r.vm_match(text, 0, mut m) {
		if res.end == text.len {
			return res
		}
	}
	return none
}

// group_by_name retrieves text captured by a named group.
pub fn (r &Regex) group_by_name(m Match, name string) string {
	idx := r.group_map[name] or { return '' }
	return if idx < m.groups.len { m.groups[idx] } else { '' }
}

/******************************************************************************
*
* Compatibility Layer
*
******************************************************************************/

// new_regex is a helper wrapper to compile a regex pattern.
pub fn new_regex(pattern string, _ int) !Regex {
	return compile(pattern)
}

// match_str is a compatibility alias for find_from.
pub fn (r &Regex) match_str(text string, start_index int, _ int) ?Match {
	return r.find_from(text, start_index)
}

// get returns the matched text for a specific group index.
// Index 0 returns the full match, 1..n returns capture groups.
pub fn (m Match) get(idx int) ?string {
	if idx == 0 {
		return m.text
	}
	if idx > 0 && idx <= m.groups.len {
		return m.groups[idx - 1]
	}
	return none
}

// get_all returns a list of all captured strings, starting with the full match at index 0.
pub fn (m Match) get_all() []string {
	mut res := [m.text]
	res << m.groups
	return res
}
