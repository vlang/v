/*
regex2 0.9.4 beta (VM Edition) - Performance Optimized

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

Optimizations:
 - **Pre-allocated Machine workspace**: All search operations are zero-allocation.
 - **Raw pointer access**: Bypasses bounds checking for string and instruction access.
 - **Fast ASCII Path**: Characters < 128 bypass heavy UTF-8 decoding.
 - **Instruction merging**: Consecutive character matches are merged into string blocks.
 - **Bitmap lookups**: ASCII character classes use a 128-bit bitset for O(1) matching.

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

// max_stack_depth defines the limit for the backtracking stack.
// This prevents excessive memory use while allowing complex pattern matching.
const deafault_max_stack_depth = 1024 // default value

/******************************************************************************
*
* Structs & Enums (VM Instructions)
*
******************************************************************************/

// InstType represents the various operation codes executed by the Virtual Machine.
enum InstType as u8 {
	match             // Successful match reached.
	char              // Match a single rune (UTF-8 supported).
	string            // Match a merged sequence of ASCII characters.
	any               // Match any character (dot).
	class             // Match a character class (e.g., [a-z] or \w).
	split             // Create a non-deterministic branch (backtracking point).
	jmp               // Jump to a specific instruction.
	save              // Save the current string position for a capture group.
	assert_start      // Match the start of the entire string.
	assert_end        // Match the end of the entire string.
	assert_line_start // Match the start of a line (multiline mode).
	assert_line_end   // Match the end of a line (multiline mode).
	assert_bound      // Match a word boundary (\b).
	assert_nbound     // Match a non-word boundary (\B).
}

// Inst represents a single Virtual Machine instruction and its associated data.
struct Inst {
mut:
	typ         InstType
	val         rune   // Character value for .char.
	val_str     string // String value for merged .string instructions.
	val_len     int    // Length in bytes of val_str.
	target_x    int    // Primary target for jumps/splits.
	target_y    int    // Secondary target for splits (backtracking path).
	group_idx   int    // Index for save operations.
	char_class  []rune // Explicit runes for character classes.
	bitmap      [4]u32 // 128-bit bitset for fast ASCII class matching.
	inverted    bool   // True if the match should be negated (e.g., [^abc]).
	ignore_case bool   // True if case-insensitive matching is enabled.
	dot_all     bool   // True if '.' should match newlines.
}

// Machine provides a pre-allocated workspace for VM execution.
// It allows the VM to run without allocating memory on every search.
struct Machine {
mut:
	stack    []int // Backtracking stack containing [captures, sp, next_pc].
	captures []int // Indices of capture group start/end positions.
}

// Regex is the compiled representation of a regular expression pattern.
pub struct Regex {
pub:
	pattern      string         // The original pattern string.
	prog         []Inst         // The compiled VM bytecode.
	total_groups int            // Total number of capture groups.
	group_map    map[string]int // Map of group names to their indices.
	prefix_lit   string         // Literal prefix used for fast-skip optimization.
	has_prefix   bool           // True if the pattern starts with a literal.
mut:
	max_stack_depth int     // max stack depth possible during VM run
	machine         Machine // Pre-allocated workspace for the VM.
}

// Match contains the results of a successful regex search.
pub struct Match {
pub:
	text   string   // The full text of the match.
	start  int      // Byte index where the match starts.
	end    int      // Byte index where the match ends.
	groups []string // Text captured by each group.
}

// Quantifier defines the repetition limits for an AST node.
struct Quantifier {
mut:
	min    int  // Minimum occurrences.
	max    int  // Maximum occurrences (-1 for infinity).
	greedy bool // True if it should match as much as possible.
}

// Flags contains the state of inline regex flags like (?i).
struct Flags {
mut:
	ignore_case bool
	multiline   bool
	dot_all     bool
}

// NodeType represents the different kinds of nodes in the Abstract Syntax Tree.
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

// Node is a component of the Abstract Syntax Tree during the compilation phase.
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

// read_rune_at decodes a UTF-8 rune starting at the given index using raw pointers.
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

// is_word_char returns true if the byte is an ASCII alphanumeric character or underscore.
@[inline]
fn is_word_char(r u8) bool {
	return (r >= `a` && r <= `z`) || (r >= `A` && r <= `Z`) || (r >= `0` && r <= `9`) || r == `_`
}

// set_bitmap sets the bit corresponding to the given rune in a 128-bit ASCII bitmap.
fn set_bitmap(mut bitmap [4]u32, r rune) {
	if r >= 0 && r < 128 {
		idx := u32(r) >> 5
		bit := u32(r) & 31
		bitmap[idx] |= (u32(1) << bit)
	}
}

/******************************************************************************
*
* Compiler
*
******************************************************************************/

// compile parses a regex pattern and compiles it into an executable Regex object.
pub fn compile(pattern string) !Regex {
	mut group_map := map[string]int{}
	initial_flags := Flags{false, false, false}

	// Phase 1: Parse string into AST nodes.
	nodes, _, final_group_count := parse_nodes(pattern, 0, `\0`, 0, initial_flags, mut
		group_map)!

	// Wrap in a root group.
	root := Node{
		typ:   .group
		nodes: nodes
		quant: Quantifier{1, 1, true}
	}

	// Phase 2: Emit VM bytecode.
	mut compiler := Compiler{
		prog: []Inst{cap: 128}
	}
	compiler.emit_node(root)
	compiler.emit(Inst{ typ: .match })

	// Phase 3: Optimize bytecode.
	optimized_prog := compiler.optimize()

	// Detect literal prefix for fast-search optimization.
	mut prefix := ''
	mut has_prefix := false
	if optimized_prog.len > 0 {
		if optimized_prog[0].typ == .string {
			prefix = optimized_prog[0].val_str
			has_prefix = true
		} else if optimized_prog[0].typ == .char && !optimized_prog[0].ignore_case
			&& optimized_prog[0].val < 128 {
			prefix = unsafe { u8(optimized_prog[0].val).ascii_str() }
			has_prefix = true
		}
	}

	// Pre-allocate the Machine workspace based on pattern requirements.
	cap_count := final_group_count * 2
	return Regex{
		max_stack_depth: deafault_max_stack_depth
		pattern:         pattern
		prog:            optimized_prog
		total_groups:    final_group_count
		group_map:       group_map
		prefix_lit:      prefix
		has_prefix:      has_prefix
		machine:         Machine{
			stack:    []int{len: deafault_max_stack_depth}
			captures: []int{len: cap_count}
		}
	}
}

// change_stack_depth let to change the stack depth of the VM, Regex strunct r MUST be Mutable
pub fn (mut r Regex) change_stack_depth(depth int) {
	r.max_stack_depth = depth
	r.machine.stack = []int{len: depth}
}

// Compiler handles the transformation of AST nodes into VM instructions.
struct Compiler {
mut:
	prog []Inst
}

// emit appends an instruction to the program and returns its index.
fn (mut c Compiler) emit(i Inst) int {
	c.prog << i
	return c.prog.len - 1
}

// optimize performs peephole optimizations like merging consecutive characters.
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

		// Optimization: Merge consecutive literal Chars into a single String instruction.
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

	// Fix up jump and split targets based on new indices.
	for mut inst in new_prog {
		if inst.typ == .split || inst.typ == .jmp {
			inst.target_x = idx_map[inst.target_x] or { inst.target_x }
			inst.target_y = idx_map[inst.target_y] or { inst.target_y }
		}
	}
	return new_prog
}

// emit_class generates bytecode for character classes using bitmaps for ASCII.
fn (mut c Compiler) emit_class(node Node) {
	mut bitmap := [4]u32{}
	mut char_class := node.char_set.clone()
	mut inverted := node.inverted

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

// emit_node generates instructions for a node, handling repetitions (quantifiers).
fn (mut c Compiler) emit_node(node Node) {
	for _ in 0 .. node.quant.min {
		c.emit_logic(node)
	}

	if node.quant.max == -1 {
		// Infinite repetition (*)
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
		// Range repetition {m, n}
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

// emit_logic translates AST node types into VM instructions.
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

// parse_nodes parses a pattern string into a sequence of AST Nodes.
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
* Virtual Machine Executor
*
******************************************************************************/

// vm_match executes the compiled bytecode against the provided string.
// It uses raw pointers for speed and a pre-allocated stack for zero-allocation matching.
@[direct_array_access]
fn (r &Regex) vm_match(text string, start_pos int) ?Match {
	unsafe {
		mut m := &r.machine
		mut stack := m.stack
		mut captures := m.captures
		// Initialize captures to -1 (no match).
		for i in 0 .. captures.len {
			captures[i] = -1
		}

		mut pc := 0
		mut sp := start_pos
		mut stack_ptr := 0

		cap_size := r.total_groups * 2
		frame_size := cap_size + 2 // stack frame layout: [capture_states..., sp, next_pc]

		// Use pointers to bypass array bounds checking.
		prog_ptr := &Inst(r.prog.data)
		str_ptr := text.str
		str_len := text.len

		for {
			if pc >= r.prog.len {
				goto backtrack
			}

			inst := &prog_ptr[pc]

			match inst.typ {
				.match {
					// Successful match: collect captured groups.
					mut s_groups := []string{cap: r.total_groups}
					for i := 0; i < r.total_groups; i++ {
						s, e := captures[i * 2], captures[i * 2 + 1]
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
					if curr_byte < 128 && inst.val < 128 {
						// Fast ASCII Path
						mut c1, mut c2 := curr_byte, u8(inst.val)
						if inst.ignore_case {
							if c1 >= `a` && c1 <= `z` {
								c1 -= 32
							}
							if c2 >= `a` && c2 <= `z` {
								c2 -= 32
							}
						}
						if c1 == c2 {
							sp++
							pc++
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
					if inst.ignore_case {
						r1 := if rn >= `a` && rn <= `z` { rn - 32 } else { rn }
						r2 := if inst.val >= `a` && inst.val <= `z` {
							inst.val - 32
						} else {
							inst.val
						}
						if r1 == r2 {
							match_ok = true
						}
					} else if rn == inst.val {
						match_ok = true
					}
					if match_ok {
						sp += l
						pc++
					} else {
						goto backtrack
					}
				}
				.string {
					// Optimized multi-byte literal match.
					if sp + inst.val_len > str_len {
						goto backtrack
					}
					for i in 0 .. inst.val_len {
						if str_ptr[sp + i] != inst.val_str.str[i] {
							goto backtrack
						}
					}
					sp += inst.val_len
					pc++
				}
				.class {
					if sp >= str_len {
						goto backtrack
					}
					c_byte := str_ptr[sp]
					mut matched := false
					mut cl := 1
					if c_byte < 128 {
						// Fast Bitmap Lookup for O(1) matching.
						if (inst.bitmap[c_byte >> 5] & (u32(1) << (c_byte & 31))) != 0 {
							matched = true
						}
					} else {
						rn, l := read_rune_at(str_ptr, str_len, sp)
						cl = l
						for cr in inst.char_class {
							if cr == rn {
								matched = true
								break
							}
						}
					}
					if matched != inst.inverted {
						sp += cl
						pc++
					} else {
						goto backtrack
					}
				}
				.any {
					if sp >= str_len {
						goto backtrack
					}
					if inst.dot_all || str_ptr[sp] != `\n` {
						_, cl := read_rune_at(str_ptr, str_len, sp)
						sp += cl
						pc++
					} else {
						goto backtrack
					}
				}
				.save {
					captures[inst.group_idx] = sp
					pc++
				}
				.split {
					// Save state to stack for backtracking.
					if stack_ptr + frame_size >= r.max_stack_depth {
						goto backtrack
					}
					for i in 0 .. cap_size {
						stack[stack_ptr + i] = captures[i]
					}
					stack[stack_ptr + cap_size] = sp
					stack[stack_ptr + cap_size + 1] = inst.target_y
					stack_ptr += frame_size
					pc = inst.target_x
				}
				.jmp {
					pc = inst.target_x
				}
				.assert_start {
					if sp == 0 {
						pc++
					} else {
						goto backtrack
					}
				}
				.assert_end {
					if sp == str_len {
						pc++
					} else {
						goto backtrack
					}
				}
				.assert_line_start {
					if sp == 0 || (sp > 0 && str_ptr[sp - 1] == `\n`) {
						pc++
					} else {
						goto backtrack
					}
				}
				.assert_line_end {
					if sp == str_len || str_ptr[sp] == `\n` {
						pc++
					} else {
						goto backtrack
					}
				}
				.assert_bound, .assert_nbound {
					l := if sp > 0 { is_word_char(str_ptr[sp - 1]) } else { false }
					r_ := if sp < str_len { is_word_char(str_ptr[sp]) } else { false }
					if (inst.typ == .assert_bound && l != r_)
						|| (inst.typ == .assert_nbound && l == r_) {
						pc++
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
			// Pop last saved state.
			stack_ptr -= frame_size
			for i in 0 .. cap_size {
				captures[i] = stack[stack_ptr + i]
			}
			sp = stack[stack_ptr + cap_size]
			pc = stack[stack_ptr + cap_size + 1]
		}
	}
	return none
}

/******************************************************************************
*
* Public API
*
******************************************************************************/

// find returns the first match of the pattern in the given text.
pub fn (r &Regex) find(text string) ?Match {
	return r.find_from(text, 0)
}

// find_from returns the first match of the pattern in the given text, starting from start_index.
pub fn (r &Regex) find_from(text string, start_index int) ?Match {
	if start_index < 0 || start_index > text.len {
		return none
	}

	// Optimization: Skip immediately to the literal prefix if available.
	if r.has_prefix {
		mut i := text.index_after(r.prefix_lit, start_index) or { -1 }
		for i != -1 {
			if res := r.vm_match(text, i) {
				return res
			}
			i = text.index_after(r.prefix_lit, i + 1) or { -1 }
		}
		return none
	}

	// General search: iterate through the string byte-by-byte.
	for i := start_index; i <= text.len; i++ {
		// skip UTF-8 continuation bytes.
		if i > 0 && i < text.len && (text[i] & 0xC0) == 0x80 {
			continue
		}
		if res := r.vm_match(text, i) {
			return res
		}
	}
	return none
}

// find_all returns all non-overlapping matches of the pattern in the given text.
pub fn (r &Regex) find_all(text string) []Match {
	mut matches := []Match{}
	mut i := 0
	for i <= text.len {
		if i > 0 && i < text.len && (text[i] & 0xC0) == 0x80 {
			i++
			continue
		}
		if m := r.vm_match(text, i) {
			matches << m
			i = if m.end > i { m.end } else { i + 1 }
		} else {
			i++
		}
	}
	return matches
}

// replace finds the first match in text and replaces it according to the repl string.
// Supports $n backreferences (e.g., $1 for the first capture group).
pub fn (r &Regex) replace(text string, repl string) string {
	res := r.find(text) or { return text }
	mut sb := strings.new_builder(text.len)
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

// fullmatch returns a match only if the pattern matches the entire text.
pub fn (r &Regex) fullmatch(text string) ?Match {
	if res := r.vm_match(text, 0) {
		if res.end == text.len {
			return res
		}
	}
	return none
}

// group_by_name retrieves the text captured by a named group.
pub fn (r &Regex) group_by_name(m Match, name string) string {
	idx := r.group_map[name] or { return '' }
	return if idx < m.groups.len { m.groups[idx] } else { '' }
}

/******************************************************************************
*
* C PCRE compatibility layer
*
******************************************************************************/

// new_regex is a compatibility alias for compile.
pub fn new_regex(pattern string, _ int) !Regex {
	return compile(pattern)
}

// match_str is a compatibility alias for find_from.
pub fn (r &Regex) match_str(text string, start_index int, _ int) ?Match {
	return r.find_from(text, start_index)
}

// get returns the full match text (idx 0) or the captured group text (idx > 0).
pub fn (m Match) get(idx int) ?string {
	if idx == 0 {
		return m.text
	}
	if idx > 0 && idx <= m.groups.len {
		return m.groups[idx - 1]
	}
	return none
}

// get_all returns a list containing the full match text followed by all capture groups.
pub fn (m Match) get_all() []string {
	mut res := [m.text]
	res << m.groups
	return res
}
