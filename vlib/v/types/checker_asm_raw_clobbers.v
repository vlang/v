module types

import v.flat
import v.token

// inline_asm_raw_gp_registers are the 64-bit general-purpose registers `rax` to `r15` that the
// checks follow. `rsp` is left out because no template should write it, and the APX registers
// `r16` to `r31` are left out too.
const inline_asm_raw_gp_registers = ['rax', 'rbx', 'rcx', 'rdx', 'rsi', 'rdi', 'rbp', 'r8', 'r9',
	'r10', 'r11', 'r12', 'r13', 'r14', 'r15']!

// inline_asm_raw_writes_dst_families are the GNU x86 mnemonics whose last operand, the
// destination, is written. `cmp` and `test` only read their operands.
const inline_asm_raw_writes_dst_families = ['mov', 'add', 'sub', 'and', 'or', 'xor', 'inc', 'dec',
	'neg', 'not', 'shl', 'shr', 'sar', 'lea', 'pop', 'imul']!

// inline_asm_raw_string_families are the GNU x86 string instructions, with the `smov`, `ssto`,
// `slod`, `scmp` and `ssca` spellings GNU as also takes for them. Under a `rep` prefix they
// use `%%rcx` as the repeat count.
const inline_asm_raw_string_families = ['movs', 'stos', 'lods', 'cmps', 'scas', 'ins', 'outs',
	'smov', 'ssto', 'slod', 'scmp', 'ssca']!

// inline_asm_raw_far_transfers start the GNU x86 mnemonics, other than a jump, a loop and a near
// `call`, that leave the template or resume it elsewhere: every form of `ret`, a far jump, call
// or return, an interrupt return, the user interrupt return `uiret`, the FRED returns `eretu` and
// `erets`, `sysenter`, `sysret` and `sysexit`, and `xbegin`, whose abort resumes at its label.
const inline_asm_raw_far_transfers = ['ret', 'lret', 'iret', 'uiret', 'eretu', 'erets', 'sysenter',
	'sysret', 'sysexit', 'ljmp', 'lcall', 'xbegin']!

// inline_asm_raw_flag_families are the GNU x86 mnemonics that set the condition flags.
const inline_asm_raw_flag_families = ['add', 'sub', 'and', 'or', 'xor', 'cmp', 'test', 'inc', 'dec',
	'neg', 'shl', 'shr', 'sar', 'imul', 'mul', 'div', 'idiv']!

// inline_asm_raw_read_prefixes are the only GNU x86 prefix words the clobber check reads past.
// Any other prefix can change what the instruction after it writes.
const inline_asm_raw_read_prefixes = ['lock', 'wait', 'rep', 'repe', 'repz', 'repne', 'repnz']!

// inline_asm_raw_rep_prefixes are the `rep` family: with a string instruction they repeat it
// `rcx` times.
const inline_asm_raw_rep_prefixes = ['rep', 'repe', 'repz', 'repne', 'repnz']!

// inline_asm_raw_lone_prefix_carries_rep reports whether the words of a statement with no operands
// put a `rep` on the next instruction: `rep` words alone, or after one `wait`. See
// vlib/v/types/README.md.
fn inline_asm_raw_lone_prefix_carries_rep(words []string) bool {
	rest := if words.len > 1 && words[0] == 'wait' { words[1..] } else { words }
	return rest.all(it in inline_asm_raw_rep_prefixes)
}

// inline_asm_raw_encoding_suffixes are the GNU as encoding suffixes, after the dot and in lower
// case, that inline_asm_raw_has_encoding_suffix looks for.
const inline_asm_raw_encoding_suffixes = ['s', 'd8', 'd32']

// inline_asm_raw_has_encoding_suffix reports whether an instruction carries one of the GNU as
// encoding suffixes `.s`, `.d8` or `.d32`, in any letter case, such as the `.s` of `movq.s`. Like a
// pseudo prefix, a suffix picks the encoding, so a template that uses one is not checked either.
// Another dotted name, such as a macro called `set.rbx`, is not a suffix.
fn inline_asm_raw_has_encoding_suffix(instruction InlineAsmRawInstruction) bool {
	return !instruction.is_directive && instruction.mnemonic.contains('.')
		&& instruction.mnemonic.all_after_last('.').to_lower_ascii() in inline_asm_raw_encoding_suffixes
}

// inline_asm_raw_has_unread_prefix reports whether an instruction has a prefix that is not in
// inline_asm_raw_read_prefixes, or is such a prefix on a line of its own.
fn inline_asm_raw_has_unread_prefix(instruction InlineAsmRawInstruction) bool {
	for word in instruction.mnemonic.to_lower_ascii().split(' ') {
		if word !in inline_asm_raw_read_prefixes && inline_asm_raw_is_prefix(word) {
			return true
		}
	}
	return false
}

// inline_asm_raw_is_rex_prefix reports whether a lower-case word is one of the REX prefixes of
// binutils' `opcodes/i386-opc.tbl`: `rex` or `rex64`, followed by some of `x`, `y` and `z` in that
// order (`rexz`, `rex64xy`), or `rex.` followed by some of `w`, `r`, `x` and `b` in that order
// (`rex.b`, `rex.wrxb`).
fn inline_asm_raw_is_rex_prefix(word string) bool {
	if word.starts_with('rex.') {
		return word.len > 4 && inline_asm_raw_is_ordered_subset(word[4..], 'wrxb')
	}
	rest := if word.starts_with('rex64') {
		word[5..]
	} else if word.starts_with('rex') {
		word[3..]
	} else {
		return false
	}
	return inline_asm_raw_is_ordered_subset(rest, 'xyz')
}

// inline_asm_raw_is_ordered_subset reports whether every character of `s` is in `letters`, at
// most once and in the order of `letters`.
fn inline_asm_raw_is_ordered_subset(s string, letters string) bool {
	mut next := 0
	for c in s {
		for next < letters.len && letters[next] != c {
			next++
		}
		if next == letters.len {
			return false
		}
		next++
	}
	return true
}

// inline_asm_raw_x86_family maps a register name to the 64-bit general-purpose register that
// contains it, so `%%eax`, `%%AL` and `%%al` all name `rax`. The assembler reads register names
// in any case, so other names are returned in lower case.
fn inline_asm_raw_x86_family(raw_name string) string {
	name := raw_name.to_lower_ascii()
	match name {
		'rax', 'eax', 'ax', 'al', 'ah' { return 'rax' }
		'rbx', 'ebx', 'bx', 'bl', 'bh' { return 'rbx' }
		'rcx', 'ecx', 'cx', 'cl', 'ch' { return 'rcx' }
		'rdx', 'edx', 'dx', 'dl', 'dh' { return 'rdx' }
		'rsi', 'esi', 'si', 'sil' { return 'rsi' }
		'rdi', 'edi', 'di', 'dil' { return 'rdi' }
		'rbp', 'ebp', 'bp', 'bpl' { return 'rbp' }
		else {}
	}
	if name.len >= 2 && name[0] == `r` && name[1].is_digit() {
		// `r8d`, `r8w` and `r8b` are the narrower views of `r8`.
		mut end := name.len
		if name.len >= 3 && name[end - 1] in [`d`, `w`, `b`] {
			end--
		}
		return name[..end]
	}
	return name
}

// inline_asm_raw_register_name names the register of the family `family` that a block for an
// instruction set of `arch_bits` bits uses: `eax` for `rax` on i386.
fn inline_asm_raw_register_name(family string, arch_bits int) string {
	if arch_bits == 32 && family in ['rax', 'rbx', 'rcx', 'rdx', 'rsi', 'rdi', 'rbp'] {
		return 'e' + family[1..]
	}
	return family
}

// inline_asm_raw_clobber_counts reports whether a clobber word declares its register in a block
// for an instruction set of `arch_bits` bits. On i386 a 64-bit name (`rax`, `r8`) does not, as it
// does not for every compiler (see vlib/v/types/README.md).
fn inline_asm_raw_clobber_counts(word string, arch_bits int) bool {
	if arch_bits != 32 {
		return true
	}
	name := word.to_lower_ascii()
	return !(name in ['rax', 'rbx', 'rcx', 'rdx', 'rsi', 'rdi', 'rbp', 'rsp']
		|| (name.len >= 2 && name[0] == `r` && name[1].is_digit()))
}

// inline_asm_raw_base_mnemonic returns the family of a GNU x86 mnemonic, with or without its
// size suffix, or an empty string when the family is not in families. `addq` and `add` share
// the family `add`, while `movsb` is not a `mov` family member.
fn inline_asm_raw_base_mnemonic(name string, families []string) string {
	if name in families {
		return name
	}
	if name.len > 1 && name[name.len - 1] in [`b`, `w`, `l`, `q`]
		&& name[..name.len - 1] in families {
		return name[..name.len - 1]
	}
	return ''
}

// inline_asm_raw_fixed_registers returns the general-purpose registers that the operands of a
// section bind through their single-register constraints, such as `"=a"` for `rax`, and through
// `{rax}`. The `A` pair constraint depends on the operand width; see
// inline_asm_raw_pair_outputs and inline_asm_raw_pair_inputs.
fn inline_asm_raw_fixed_registers(block string, section InlineAsmRange) []string {
	mut registers := []string{}
	for io in inline_asm_ios(block, section) {
		registers << inline_asm_raw_read_constraint(io.constraint).registers
	}
	return registers
}

// InlineAsmRawConstraint is what the text of one operand constraint says about the place of its
// operand, over all its alternatives. See inline_asm_raw_read_constraint.
struct InlineAsmRawConstraint {
mut:
	registers  []string // families of the fixed registers it names, such as `rax` for `a`
	may_sit    []string // families of the registers that a class such as `Q` or `A` may use
	has_pair   bool     // it holds `A`
	is_matched bool     // it names an output by number or by name
	has_other  bool     // it allows a constant, a vector, mask or x87 register, or the flags
	is_free    bool     // it may sit in any general-purpose register, or its address may
}

// inline_asm_raw_other_letters are the constraint letters of a constant, and those of a vector
// (`x`, `v`, `y`), mask (`k`) or x87 (`t`, `u`, `f`) register.
const inline_asm_raw_other_letters = 'inIJKLMNOeZEFGHCsxvyktuf'

// inline_asm_raw_other_names are the two-letter names for the same. See vlib/v/types/README.md.
const inline_asm_raw_other_names = ['Y2', 'YW', 'Yd', 'Yi', 'Yk', 'Ym', 'Yr', 'Yt', 'Yv', 'Yw',
	'Yz', 'Wb', 'Wd', 'We', 'Wf', 'Ws', 'Ww', 'Wz', 'Bz']

// inline_asm_raw_class_registers returns the families that a constraint letter for a limited class of
// registers may use, or none for another letter. See vlib/v/types/README.md.
fn inline_asm_raw_class_registers(letter u8) []string {
	return match letter {
		`A` { ['rax', 'rdx'] }
		`Q` { ['rax', 'rbx', 'rcx', 'rdx'] }
		`R` { ['rax', 'rbx', 'rcx', 'rdx', 'rsi', 'rdi', 'rbp'] }
		`U` { ['rax', 'rcx', 'rdx', 'rsi', 'rdi', 'r8', 'r9', 'r10', 'r11'] }
		else { []string{} }
	}
}

// inline_asm_raw_read_constraint reads an x86 operand constraint. It skips the rest of an alternative
// after `#` and everything after `@`, and takes the letter after a `Y`, `W`, `T`, `B` or `j` with it.
// Every letter that is not listed here counts as free, as do `r`, `q`, `g` and `m`, and so does a
// constraint that names nothing: V binds an operand without a constraint with `r` or `+r`.
fn inline_asm_raw_read_constraint(constraint string) InlineAsmRawConstraint {
	mut result := InlineAsmRawConstraint{}
	mut i := 0
	for i < constraint.len {
		c := constraint[i]
		rest := constraint[i + 1..]
		i++
		match c {
			`=`, `+`, `&`, `%`, `?`, `*`, `!`, `^`, `$`, ` `, `,` {}
			`#` {
				i += rest.all_before(',').len
			}
			`@` {
				result.has_other = true
				break
			}
			`{` {
				name := rest.all_before('}')
				result.registers << inline_asm_raw_x86_family(name)
				i += name.len + 1
			}
			`[` {
				result.is_matched = true
				i += rest.all_before(']').len + 1
			}
			`0`...`9` {
				result.is_matched = true
			}
			`a` {
				result.registers << 'rax'
			}
			`b` {
				result.registers << 'rbx'
			}
			`c` {
				result.registers << 'rcx'
			}
			`d` {
				result.registers << 'rdx'
			}
			`S` {
				result.registers << 'rsi'
			}
			`D` {
				result.registers << 'rdi'
			}
			`A`, `Q`, `R`, `U` {
				result.may_sit << inline_asm_raw_class_registers(c)
				result.has_pair = result.has_pair || c == `A`
			}
			`Y`, `W`, `T`, `B`, `j` {
				if constraint[i - 1..].limit(2) in inline_asm_raw_other_names {
					result.has_other = true
				} else {
					result.is_free = true
				}
				i++
			}
			else {
				if inline_asm_raw_other_letters.contains_u8(c) {
					result.has_other = true
				} else {
					result.is_free = true
				}
			}
		}
	}
	if result.registers.len == 0 && result.may_sit.len == 0 && !result.is_matched && !result.has_other {
		result.is_free = true
	}
	return result
}

// binds_pair_register reports whether a constraint names `rax` or `rdx`: `A`, `a`, `d`, or a braced
// name of either register.
fn (constraint &InlineAsmRawConstraint) binds_pair_register() bool {
	return constraint.has_pair || 'rax' in constraint.registers || 'rdx' in constraint.registers
}

// is_in_changeable reports whether a constraint can only place an input in registers that an output
// or a clobber names too, or in an output that it matches.
fn (constraint &InlineAsmRawConstraint) is_in_changeable(changeable map[string]bool) bool {
	return !constraint.is_free && !constraint.has_other
		&& constraint.registers.all(changeable[it]) && constraint.may_sit.all(changeable[it])
}

// InlineAsmRawPairOperand is an operand whose constraint holds `A` and whose width does not fill
// the `rax`:`rdx` pair, so it cannot reserve both registers.
struct InlineAsmRawPairOperand {
	name       string
	constraint string
	width      int
	start      int
	end        int
}

// inline_asm_raw_pair_outputs returns an output whose constraint holds `A` when it is narrower
// than the `rax`:`rdx` pair and `alone` is set (see `pair_bound` in
// check_inline_asm_raw_clobbers). Such an output cannot reserve both registers, so the template
// may write neither. For every other output whose constraint holds `A`, it declares `rax` and
// `rdx`.
fn (mut tc TypeChecker) inline_asm_raw_pair_outputs(node flat.Node, block string, section InlineAsmRange, arch_bits int, alone bool, mut clobbers map[string]bool) ?InlineAsmRawPairOperand {
	for io_index, io in inline_asm_ios(block, section) {
		if !inline_asm_raw_read_constraint(io.constraint).has_pair {
			continue
		}
		// Outputs come first among the node's children, so the output index is the child index.
		// Every output is an lvalue with a type, so a field or an index expression is resolved too.
		mut width := 0
		if io_index < int(node.children_count) {
			width = tc.inline_asm_att_type_bits(tc.resolve_type(tc.a.child(&node, io_index)),
				arch_bits)
		}
		if alone && width > 0 && width < 2 * arch_bits {
			return InlineAsmRawPairOperand{
				name:       if io.alias.len > 0 { io.alias } else { io.expr }
				constraint: io.constraint
				width:      width
				start:      io.start
				end:        io.end
			}
		}
		clobbers['rax'] = true
		clobbers['rdx'] = true
	}
	return none
}

// inline_asm_raw_pair_inputs returns an input whose constraint holds `A` when it is a variable
// narrower than the pair and `alone` is set, as for inline_asm_raw_pair_outputs. For every other
// input whose constraint holds `A`, it declares `rax` and `rdx`, as a template may change a fixed
// input if it restores it.
fn (mut tc TypeChecker) inline_asm_raw_pair_inputs(node flat.Node, block string, section InlineAsmRange, first_child int, arch_bits int, alone bool, mut clobbers map[string]bool) ?InlineAsmRawPairOperand {
	for io_index, io in inline_asm_ios(block, section) {
		if !inline_asm_raw_read_constraint(io.constraint).has_pair {
			continue
		}
		child_index := first_child + io_index
		mut width := 0
		if child_index < int(node.children_count) {
			child := tc.a.child(&node, child_index)
			if tc.inline_asm_operand_is_variable(child, io.expr) {
				width = tc.inline_asm_att_type_bits(tc.resolve_type(child), arch_bits)
			}
		}
		if alone && width > 0 && width < 2 * arch_bits {
			return InlineAsmRawPairOperand{
				name:       if io.alias.len > 0 { io.alias } else { io.expr }
				constraint: io.constraint
				width:      width
				start:      io.start
				end:        io.end
			}
		}
		clobbers['rax'] = true
		clobbers['rdx'] = true
	}
	return none
}

// InlineAsmRawRestoredRegion spans the instructions from one that saves a register, or the flags,
// on the stack to the one that restores it, both included. `name` is the 64-bit register family
// or `flags`.
struct InlineAsmRawRestoredRegion {
	name  string
	first int
	last  int
}

// inline_asm_raw_saved_register returns the family of the register that a one-operand `push` or
// `pop` moves whole, or an empty string for any other operand. A narrower view does not save the
// whole register.
fn inline_asm_raw_saved_register(instruction InlineAsmRawInstruction, arch_bits int) string {
	if instruction.operands.len != 1 || instruction.operands[0].kind != .register {
		return ''
	}
	name := instruction.operands[0].name
	if inline_asm_att_register_width(name, arch_bits) != arch_bits {
		return ''
	}
	return inline_asm_raw_x86_family(name)
}

// inline_asm_raw_stack_slot_bits returns how many bits a `push` or `pop` named `name`, of the
// family `family`, moves: the width of a general register operand; for an operand reference, its
// width in `ref_bits`, else the size suffix, else 0, as it is not known; for any other operand the size
// suffix, else the width of the instruction set, which an unsuffixed memory or immediate moves.
fn inline_asm_raw_stack_slot_bits(name string, family string, instruction InlineAsmRawInstruction, arch_bits int, ref_bits map[string]int) int {
	suffix := match name[family.len..] {
		'w' { 16 }
		'l' { 32 }
		'q' { 64 }
		else { 0 }
	}
	if instruction.operands.len == 1 {
		operand := instruction.operands[0]
		if operand.kind == .register {
			width := inline_asm_att_register_width(operand.name, arch_bits)
			if width > 0 {
				return width
			}
		} else if operand.kind == .operand_ref {
			return ref_bits[inline_asm_raw_ref_key(operand)] or { suffix }
		}
	}
	return if suffix > 0 { suffix } else { arch_bits }
}

// inline_asm_raw_ref_key names an operand reference with its modifier, as `%w[x]` and `%[x]`
// have different widths.
fn inline_asm_raw_ref_key(operand InlineAsmRawOperand) string {
	return '${operand.modifier}:${operand.name}'
}

// inline_asm_raw_is_stack_pointer reports whether `name` is a view of the stack pointer.
fn inline_asm_raw_is_stack_pointer(name string) bool {
	return name.to_lower_ascii() in ['rsp', 'esp', 'sp', 'spl']
}

// inline_asm_raw_operand_registers returns the registers that an operand names, as a register or
// in an address such as `8(%%rsp,%%rax,4)` or `%%fs:(%%rbx)`, in lower case and without `%`.
fn inline_asm_raw_operand_registers(operand InlineAsmRawOperand) []string {
	if operand.kind == .register {
		return [operand.name.to_lower_ascii()]
	}
	mut registers := []string{}
	if operand.kind != .memory {
		return registers
	}
	for part in operand.text.to_lower_ascii().split('%') {
		name := part.all_before(')').all_before(',').all_before(':').trim_space()
		if name.len > 0 && name[0].is_letter() {
			registers << name
		}
	}
	return registers
}

// inline_asm_raw_operand_touches_stack reports whether an operand names the stack pointer or,
// inside a frame, the frame pointer.
fn inline_asm_raw_operand_touches_stack(operand InlineAsmRawOperand, in_frame bool) bool {
	for name in inline_asm_raw_operand_registers(operand) {
		if inline_asm_raw_is_stack_pointer(name) || (in_frame && inline_asm_raw_x86_family(name) == 'rbp') {
			return true
		}
	}
	return false
}

// inline_asm_raw_is_mov_between reports whether an instruction is a whole-width `mov` from the
// register family `from` to the family `to`, where `rsp` stands for the stack pointer.
fn inline_asm_raw_is_mov_between(name string, instruction InlineAsmRawInstruction, from string, to string, arch_bits int) bool {
	if inline_asm_raw_base_mnemonic(name, ['mov']).len == 0 || instruction.operands.len != 2 {
		return false
	}
	for i, family in [from, to] {
		operand := instruction.operands[i]
		if operand.kind != .register || inline_asm_att_register_width(operand.name, arch_bits) != arch_bits {
			return false
		}
		matches := if family == 'rsp' {
			inline_asm_raw_is_stack_pointer(operand.name)
		} else {
			inline_asm_raw_x86_family(operand.name) == family
		}
		if !matches {
			return false
		}
	}
	return true
}

// inline_asm_raw_names_label reports whether a branch target names a label of the template: a
// numeric local label such as `1f` or `1b`, or one of `labels`. A quoted name is compared whole,
// so `"x@y"` names the label `"x@y":`, and an unquoted one also without a relocation suffix, so
// `getpc@PLT` names `getpc:`.
fn inline_asm_raw_names_label(target string, labels []string) bool {
	text := target.trim_space()
	mut names := []string{}
	if text.len > 1 && text[0] == `"` {
		close := text.index_after('"', 1) or { -1 }
		if close > 0 {
			names << text[1..close]
		}
	} else {
		names << text
		if text.contains('@') {
			names << text.all_before('@')
		}
	}
	for name in names {
		if name.len > 1 && name[name.len - 1] in [`f`, `b`] && name[..name.len - 1].bytes().all(it.is_digit()) {
			return true
		}
		if name in labels {
			return true
		}
	}
	return false
}

// inline_asm_raw_is_symbol reports whether a call target is a plain symbol name, such as `abort`
// or `memcpy@PLT`. A target through a register or memory (`*%%rcx`), relative to the location
// counter (`.+5`), or an expression (`1f+0`) may land inside the template, so it is not.
fn inline_asm_raw_is_symbol(target string) bool {
	mut text := target.trim_space()
	if text.len > 1 && text[0] == `"` && text[text.len - 1] == `"` {
		text = text[1..text.len - 1]
	}
	if text.len == 0 || text == '.' || text[0].is_digit() {
		return false
	}
	return text.bytes().all(it.is_alnum() || it in [`_`, `.`, `$`, `@`])
}

// inline_asm_raw_restored_regions pairs every whole-register `push` with the `pop` that restores
// it, every `pushf` with its `popf`, and every frame, a `push %%rbp` followed by `mov %%rsp,
// %%rbp`, with the `leave`, or the `mov %%rbp, %%rsp` and `pop %%rbp`, that end it, as stacks in
// the order written, so frames may nest. A write inside a region leaves the register or the flags
// as they were. A push or pop of another whole-width operand only moves the stack, and a `call`
// to a function, named by a plain symbol, leaves it as it was when nothing the template pushed is
// still on the stack; the function owns the slots above its return address. Anything else is not
// followed: a call while a pushed value is on the stack, a push or pop of another width, including
// a 16-bit `leavew`, a pop into the stack pointer or a stack address, any other instruction that
// names the stack pointer, or the frame pointer inside a frame, `enter`, `pusha`, `popa`, a
// `leave` outside a frame, a jump or loop, a return, a far transfer or `xbegin` (see
// inline_asm_raw_far_transfers), a 16-bit `callw`, and a call to a label of the template or to a
// target that is not a plain symbol (see inline_asm_raw_is_symbol). For those it returns none,
// and the template is not checked.
fn inline_asm_raw_restored_regions(instructions []InlineAsmRawInstruction, arch_bits int, ref_bits map[string]int) ?[]InlineAsmRawRestoredRegion {
	mut regions := []InlineAsmRawRestoredRegion{}
	mut saved := []string{}
	mut saved_at := []int{}
	// The slots of the saved `rbp` of each open frame, the innermost last.
	mut frames := []int{}
	mut previous_saved_rbp := false
	mut labels := []string{}
	for instruction in instructions {
		for label in instruction.labels {
			labels << label.name
		}
	}
	for index, instruction in instructions {
		if instruction.is_directive || instruction.mnemonic.len == 0 {
			continue
		}
		name := instruction.mnemonic.all_after_last(' ').to_lower_ascii()
		pushed_rbp := previous_saved_rbp
		previous_saved_rbp = false
		in_frame := frames.len > 0
		// Some assemblers take `pushfd` and `popfd` for the i386 `pushfl` and `popfl`.
		flags_push := if name == 'pushfd' {
			'pushf'
		} else {
			inline_asm_raw_base_mnemonic(name, ['pushf'])
		}
		flags_pop := if name == 'popfd' {
			'popf'
		} else {
			inline_asm_raw_base_mnemonic(name, ['popf'])
		}
		push := inline_asm_raw_base_mnemonic(name, ['push'])
		pop := inline_asm_raw_base_mnemonic(name, ['pop'])
		if flags_push.len > 0 || push.len > 0 {
			family := if flags_push.len > 0 { flags_push } else { push }
			if inline_asm_raw_stack_slot_bits(name, family, instruction, arch_bits, ref_bits) != arch_bits {
				return none
			}
			for operand in instruction.operands {
				// A pushed register is only read, but the stack pointer and a stack address move.
				if (operand.kind == .register && inline_asm_raw_is_stack_pointer(operand.name))
					|| (operand.kind == .memory && inline_asm_raw_operand_touches_stack(operand, in_frame)) {
					return none
				}
			}
			value := if flags_push.len > 0 {
				'flags'
			} else {
				inline_asm_raw_saved_register(instruction, arch_bits)
			}
			saved << value
			saved_at << index
			previous_saved_rbp = value == 'rbp'
		} else if flags_pop.len > 0 || pop.len > 0 {
			family := if flags_pop.len > 0 { flags_pop } else { pop }
			if inline_asm_raw_stack_slot_bits(name, family, instruction, arch_bits, ref_bits) != arch_bits {
				return none
			}
			for operand in instruction.operands {
				if (operand.kind == .register && inline_asm_raw_is_stack_pointer(operand.name))
					|| (operand.kind == .memory && inline_asm_raw_operand_touches_stack(operand, in_frame)) {
					return none
				}
			}
			if saved.len == 0 {
				// A pop with nothing pushed in the template restores nothing it saved.
				continue
			}
			value := if flags_pop.len > 0 {
				'flags'
			} else {
				inline_asm_raw_saved_register(instruction, arch_bits)
			}
			top := saved.pop()
			first := saved_at.pop()
			if value.len > 0 && value == top {
				regions << InlineAsmRawRestoredRegion{
					name:  value
					first: first
					last:  index
				}
			} else if in_frame && value == 'rbp' {
				// Another value in the frame pointer leaves the frame unknown.
				return none
			}
			if frames.len > 0 && frames.last() >= saved.len {
				frames.pop()
			}
		} else if name in ['leave', 'leavel', 'leaveq'] {
			if frames.len == 0 {
				return none
			}
			frame := frames.pop()
			regions << InlineAsmRawRestoredRegion{
				name:  'rbp'
				first: saved_at[frame]
				last:  index
			}
			saved.trim(frame)
			saved_at.trim(frame)
		} else if pushed_rbp && inline_asm_raw_is_mov_between(name, instruction, 'rsp', 'rbp', arch_bits) {
			frames << saved.len - 1
		} else if in_frame && inline_asm_raw_is_mov_between(name, instruction, 'rbp', 'rsp', arch_bits) {
			// `mov %%rbp, %%rsp` returns the stack pointer to the saved frame pointer.
			saved.trim(frames.last() + 1)
			saved_at.trim(frames.last() + 1)
		} else if name in ['call', 'callq', 'calll', 'callw'] {
			// A call to a function returns with the stack pointer where it was. A call to a label of the
			// template, such as `call 1f; 1: pop %%rbx`, leaves its return address on the stack, and a
			// target that is not a plain symbol may be such a label. A `callw` pushes a 16-bit return
			// address.
			if name == 'callw' {
				return none
			}
			// The function called owns the slots above its return address: a `stdcall` function, or one
			// that returns a struct on 32-bit x86, pops them with `ret $n`, and any function may change
			// one it takes as an argument. So a slot pushed before the call may not hold its value after.
			if saved.len > 0 {
				return none
			}
			if instruction.operands.len > 0 {
				target := instruction.operands[0].text
				if inline_asm_raw_names_label(target, labels) || !inline_asm_raw_is_symbol(target) {
					return none
				}
			}
		} else if name.starts_with('j') || name.starts_with('loop')
			|| inline_asm_raw_far_transfers.any(name.starts_with(it)) {
			// A jump, a loop, a return or a transaction runs the template out of the order it is
			// written in.
			return none
		} else if name in ['enter', 'enterl', 'enterq', 'enterw', 'pusha', 'pushal', 'pushaw',
			'popa', 'popal', 'popaw', 'leavew'] {
			return none
		} else {
			for operand in instruction.operands {
				if inline_asm_raw_operand_touches_stack(operand, in_frame) {
					return none
				}
			}
		}
	}
	return regions
}

// inline_asm_raw_is_restored reports whether the instruction at `index` is inside a region that
// saves and restores `name`.
fn inline_asm_raw_is_restored(regions []InlineAsmRawRestoredRegion, name string, index int) bool {
	for region in regions {
		if region.name == name && index >= region.first && index <= region.last {
			return true
		}
	}
	return false
}

// inline_asm_raw_mov_place names the place that a whole-width `mov` copies a register to or from:
// a whole general-purpose register other than the stack pointer, by its family; or an operand
// reference, by its modifier and name as written. It returns none for any other operand, such as
// memory, a narrower register, an `mm`, `xmm`, control or debug register, or a symbol.
fn inline_asm_raw_mov_place(operand InlineAsmRawOperand, arch_bits int) ?string {
	match operand.kind {
		.register {
			family := inline_asm_raw_x86_family(operand.name)
			if inline_asm_att_register_width(operand.name, arch_bits) != arch_bits
				|| family !in inline_asm_raw_gp_registers {
				return none
			}
			return 'register:${family}'
		}
		.operand_ref {
			return 'operand:${inline_asm_raw_ref_key(operand)}'
		}
		else {
			return none
		}
	}
}

// InlineAsmRawSavePlaces holds what a block says about the places that inline_asm_raw_mov_place
// names. A template must keep the value of an input.
struct InlineAsmRawSavePlaces {
	changeable map[string]bool // register families that an output or a clobber names
	named      map[string]bool // register families that an output, an input or a clobber names
	input_refs map[string]bool // names and numbers of the inputs to keep
}

// inline_asm_raw_input_refs returns the names and numbers of the inputs whose value the template must
// keep: all but those that InlineAsmRawConstraint.is_in_changeable accepts.
fn inline_asm_raw_input_refs(block string, sections []InlineAsmRange, changeable map[string]bool) map[string]bool {
	mut refs := map[string]bool{}
	if sections.len < 3 {
		return refs
	}
	outputs := inline_asm_ios(block, sections[1]).len
	for index, io in inline_asm_ios(block, sections[2]) {
		if inline_asm_raw_read_constraint(io.constraint).is_in_changeable(changeable) {
			continue
		}
		refs[io.alias] = true
		refs[(outputs + index).str()] = true
	}
	return refs
}

// may_change reports whether a template may leave another value in the place that `operand` names. A
// register that nothing in the block names counts only `with_mov`: the `mov` to it is reported on
// its own, unless a save restores that register too, and an `xchg` is not reported.
fn (places &InlineAsmRawSavePlaces) may_change(operand InlineAsmRawOperand, with_mov bool) bool {
	if operand.kind != .register {
		return !places.input_refs[operand.name]
	}
	family := inline_asm_raw_x86_family(operand.name)
	return places.changeable[family] || (with_mov && !places.named[family])
}

// InlineAsmRawStackSlot is what a `push` in inline_asm_raw_mov_restored put on the stack: the families
// whose value was copied to the pushed place before, and the pushed register, if it is one.
struct InlineAsmRawStackSlot {
	copied []string
	own    []string
}

// inline_asm_raw_mov_restored returns the register families that a template copies whole to another
// place, as inline_asm_raw_mov_place names it, and later copies back whole, directly or through
// further places. A copy is a `mov`, an `xchg`, which is one each way, or a `push` and its `pop`. One
// `xchg` alone restores nothing, and neither does a `push` and a `pop` of the register itself (see
// inline_asm_raw_restored_regions). What is written to a register between two `xchg` ends up in the
// other one. A copy counts only when the place may change (InlineAsmRawSavePlaces). The check does
// not follow whether a place changes in between, nor writes after the copy back, so it reports no
// write to such a register.
fn inline_asm_raw_mov_restored(instructions []InlineAsmRawInstruction, arch_bits int, places &InlineAsmRawSavePlaces) map[string]bool {
	mut saved := map[string][]string{}
	mut restored := map[string]bool{}
	mut stack := []InlineAsmRawStackSlot{}
	// The number of slots below the top of each open frame, the innermost last.
	mut frames := []int{}
	for instruction in instructions {
		if instruction.is_directive {
			continue
		}
		words := instruction.mnemonic.to_lower_ascii().split(' ')
		name := words[words.len - 1]
		if frames.len > 0 && frames.last() > stack.len {
			frames.pop()
		}
		if inline_asm_raw_is_mov_between(name, instruction, 'rsp', 'rbp', arch_bits) {
			frames << stack.len
			continue
		}
		if name.starts_with('leave') || inline_asm_raw_is_mov_between(name, instruction, 'rbp', 'rsp', arch_bits) {
			// `leave` also pops the saved `rbp`.
			if frames.len > 0 {
				stack.trim(frames.last())
			}
			if name.starts_with('leave') && stack.len > 0 {
				stack.pop()
			}
			continue
		}
		if inline_asm_raw_base_mnemonic(name, ['pushf']).len > 0 || name == 'pushfd' {
			stack << InlineAsmRawStackSlot{}
			continue
		}
		if inline_asm_raw_base_mnemonic(name, ['popf']).len > 0 || name == 'popfd' {
			if stack.len > 0 {
				stack.pop()
			}
			continue
		}
		if inline_asm_raw_base_mnemonic(name, ['push']).len > 0 && instruction.operands.len == 1 {
			operand := instruction.operands[0]
			place := inline_asm_raw_mov_place(operand, arch_bits) or { '' }
			stack << InlineAsmRawStackSlot{
				copied: saved[place] or { []string{} }
				own:    if place.starts_with('register:') {
					[inline_asm_raw_x86_family(operand.name)]
				} else {
					[]string{}
				}
			}
			continue
		}
		if inline_asm_raw_base_mnemonic(name, ['pop']).len > 0 && instruction.operands.len == 1 {
			if stack.len == 0 {
				continue
			}
			slot := stack.pop()
			operand := instruction.operands[0]
			place := inline_asm_raw_mov_place(operand, arch_bits) or { continue }
			if operand.kind == .register && inline_asm_raw_x86_family(operand.name) in slot.copied {
				restored[inline_asm_raw_x86_family(operand.name)] = true
			}
			if places.may_change(operand, true) {
				saved[place] = inline_asm_raw_joined(saved[place] or { []string{} }, slot.copied, slot.own)
			}
			continue
		}
		family := inline_asm_raw_base_mnemonic(name, ['mov', 'xchg'])
		if family.len == 0 || instruction.operands.len != 2 {
			continue
		}
		first := instruction.operands[0]
		second := instruction.operands[1]
		mut copies := [[first, second]]
		if family == 'xchg' {
			copies << [second, first]
		}
		// Every copy is read before any is recorded, so an `xchg` does not match itself.
		mut values := [][]string{}
		for copy in copies {
			src_place := inline_asm_raw_mov_place(copy[0], arch_bits) or { '' }
			values << if places.may_change(copy[0], family == 'mov') {
				saved[src_place] or { []string{} }
			} else {
				[]string{}
			}
		}
		for i, copy in copies {
			src_place := inline_asm_raw_mov_place(copy[0], arch_bits) or { continue }
			dst_place := inline_asm_raw_mov_place(copy[1], arch_bits) or { continue }
			if copy[1].kind == .register && inline_asm_raw_x86_family(copy[1].name) in values[i] {
				restored[inline_asm_raw_x86_family(copy[1].name)] = true
			}
			// A copy to the register itself, as in `movq %%rbx, %%rbx`, saves nothing.
			if src_place == dst_place || !places.may_change(copy[1], family == 'mov') {
				continue
			}
			own := if copy[0].kind == .register {
				[inline_asm_raw_x86_family(copy[0].name)]
			} else {
				[]string{}
			}
			saved[dst_place] = inline_asm_raw_joined(saved[dst_place] or { []string{} }, values[i], own)
		}
	}
	return restored
}

// inline_asm_raw_joined returns the families of three lists, each once.
fn inline_asm_raw_joined(first []string, second []string, third []string) []string {
	mut joined := first.clone()
	for list in [second, third] {
		for register in list {
			if register !in joined {
				joined << register
			}
		}
	}
	return joined
}

// inline_asm_raw_mul_writes_dx reports whether a mul, imul, div or idiv is proven to write DX.
// A `b` suffix never does. A `w`, `l` or `q` suffix does, and so does a resolved operand width of
// 16, 32 or 64 bits. An unresolved width proves nothing, so the check stays silent.
fn inline_asm_raw_mul_writes_dx(name string, width int) bool {
	if name.len > 1 && name[name.len - 1] == `b` {
		return false
	}
	if name.len > 1 && name[name.len - 1] in [`w`, `l`, `q`]
		&& name[..name.len - 1] in ['mul', 'imul', 'div', 'idiv'] {
		return true
	}
	return width == 16 || width == 32 || width == 64
}

// inline_asm_raw_writes returns the general-purpose registers, by family, that `instruction`
// writes: its register destination, and the registers that `mul`, `div`, the `cqo` family and,
// with `has_rep`, a string instruction write implicitly. `name` is its lowercase mnemonic.
fn (tc &TypeChecker) inline_asm_raw_writes(instruction InlineAsmRawInstruction, name string, has_rep bool, types map[string]Type, arch_bits int) map[string]bool {
	mut written := map[string]bool{}
	mul_family := inline_asm_raw_base_mnemonic(name, ['mul', 'imul', 'div', 'idiv'])
	if mul_family.len > 0 && instruction.operands.len > 0 && (mul_family == 'div'
		|| mul_family == 'idiv' || instruction.operands.len == 1) {
		written['rax'] = true
		last := instruction.operands[instruction.operands.len - 1]
		width := tc.inline_asm_att_operand_width(last, types, arch_bits)
		if inline_asm_raw_mul_writes_dx(name, width) {
			written['rdx'] = true
		}
	}
	if name in ['cqo', 'cqto', 'cdq', 'cltd', 'cwd', 'cwtd'] {
		written['rdx'] = true
	}
	// With operands, `movsd` and `cmpsd` are the SSE2 scalar move and compare.
	is_string := inline_asm_raw_base_mnemonic(name, inline_asm_raw_string_families).len > 0
		|| (name in ['movsd', 'cmpsd'] && instruction.operands.len == 0)
	if has_rep && is_string {
		written['rcx'] = true
	}
	if dst := inline_asm_raw_written_destination(instruction, name, arch_bits) {
		written[inline_asm_raw_x86_family(dst.name)] = true
	}
	return written
}

// inline_asm_raw_written_destination returns the register operand that `instruction` writes as
// its destination, or none when it writes no register operand.
fn inline_asm_raw_written_destination(instruction InlineAsmRawInstruction, name string, arch_bits int) ?InlineAsmRawOperand {
	if instruction.operands.len == 0 {
		return none
	}
	family := inline_asm_raw_base_mnemonic(name, inline_asm_raw_writes_dst_families)
	is_movx := name.len == 6 && (name.starts_with('movz') || name.starts_with('movs'))
	writes_dst := (family.len > 0 && family != 'imul') || (family == 'imul' && instruction.operands.len > 1)
	dst := instruction.operands[instruction.operands.len - 1]
	if (writes_dst || is_movx) && dst.kind == .register
		&& !inline_asm_raw_changes_nothing(instruction, family, arch_bits) {
		return dst
	}
	return none
}

// inline_asm_raw_integer_value returns the value of an integer constant as GNU as and clang read it
// (`text` without the `$` and blanks): a decimal number, an octal one with a leading zero, a `0x` or
// `0b` one, each after any number of the prefix operators `-`, `+`, `~` and `!` and in any number of
// parentheses. It returns none for anything else, such as a symbol, a character constant, an
// expression with two operands, or a number wider than 64 bits.
fn inline_asm_raw_integer_value(text string) ?u64 {
	// The operators and opening parentheses before the number, outermost first. They are applied
	// innermost first, each `(` taking its `)`; a loop, as the nesting has no limit.
	mut start := 0
	for start < text.len && text[start] in [`-`, `+`, `~`, `!`, `(`] {
		start++
	}
	mut end := start
	for end < text.len && text[end].is_alnum() {
		end++
	}
	mut value := inline_asm_raw_integer_digits(text[start..end])?
	for op in text[..start].reverse() {
		match op {
			`(` {
				if end >= text.len || text[end] != `)` {
					return none
				}
				end++
			}
			`-` {
				value = u64(0) - value
			}
			`~` {
				value = ~value
			}
			`!` {
				value = u64(if value == 0 { 1 } else { 0 })
			}
			else {}
		}
	}
	return if end == text.len { value } else { none }
}

// inline_asm_raw_integer_digits returns the value of the number `digits` of
// inline_asm_raw_integer_value, or none when it is not one.
fn inline_asm_raw_integer_digits(digits string) ?u64 {
	mut base := u64(10)
	mut body := digits
	if digits.len > 2 && digits[0] == `0` && digits[1] in [`x`, `X`] {
		base = 16
		body = digits[2..]
	} else if digits.len > 2 && digits[0] == `0` && digits[1] in [`b`, `B`] {
		base = 2
		body = digits[2..]
	} else if digits.len > 1 && digits[0] == `0` {
		base = 8
		body = digits[1..]
	}
	if body == '' {
		return none
	}
	mut value := u64(0)
	for d in body {
		// `d` is a letter or digit; a letter past `f` gives 16 or more, which no base takes.
		digit := u64(inline_asm_raw_hex_value(d))
		if digit >= base || value > (max_u64 - digit) / base {
			return none
		}
		value = value * base + digit
	}
	return value
}

// inline_asm_raw_lea_keeps reports whether a `lea` source `address` (lower case, without `%` and
// blanks) computes the value of the register `name` of `width` bits: a displacement that reads as
// zero, and the register itself as the base, alone or with `riz` or `eiz` as the index, or as the
// index with no base and a scale of 1. The address register must be at least as wide as `name`, as
// a narrower one is zero-extended, and at least 32 bits wide on amd64. The scale is read by
// inline_asm_raw_integer_value, as the displacement is.
fn inline_asm_raw_lea_keeps(address string, name string, width int, arch_bits int) bool {
	if !address.ends_with(')') {
		return false
	}
	// The address starts at the `(` that matches the last `)`; the scale may hold parentheses.
	mut open := address.len - 1
	mut depth := 0
	for ; open >= 0; open-- {
		if address[open] == `)` {
			depth++
		} else if address[open] == `(` {
			depth--
			if depth == 0 {
				break
			}
		}
	}
	if open < 0 {
		return false
	}
	if open > 0 && inline_asm_raw_integer_value(address[..open]) or { return false } != 0 {
		return false
	}
	parts := address[open + 1..address.len - 1].split(',')
	if parts.len > 3 {
		return false
	}
	base := parts[0]
	index := if parts.len > 1 { parts[1] } else { '' }
	scale := if parts.len > 2 {
		inline_asm_raw_integer_value(parts[2]) or { return false }
	} else {
		1
	}
	register := if index in ['', 'riz', 'eiz'] && (index != '' || parts.len < 3) {
		base
	} else if base == '' && scale == 1 {
		index
	} else {
		return false
	}
	bits := inline_asm_att_register_width(register, arch_bits)
	return inline_asm_raw_x86_family(register) == inline_asm_raw_x86_family(name) && bits >= width
		&& bits >= arch_bits / 2
}

// inline_asm_raw_changes_nothing reports whether an instruction of the family `family` leaves its
// register destination as it was: `mov`, `or` or `and` of the register with itself, `lea` of its own
// value (inline_asm_raw_lea_keeps), an `add`, `sub`, `or` or `xor` of an immediate that is zero at
// the destination's width, an `and` of one that is all ones there, or a shift by a count the CPU
// masks to zero. An immediate is read by inline_asm_raw_integer_value; one it cannot read counts
// as a change. A 32-bit write on amd64 clears the upper half of the register, so it changes it.
fn inline_asm_raw_changes_nothing(instruction InlineAsmRawInstruction, family string, arch_bits int) bool {
	if instruction.operands.len != 2 {
		return false
	}
	src := instruction.operands[0]
	dst := instruction.operands[1]
	width := inline_asm_att_register_width(dst.name, arch_bits)
	if width !in [8, 16, 32, 64] || (width == 32 && arch_bits == 64) {
		return false
	}
	text := src.text.to_lower_ascii().replace_each(['%', '', ' ', '', '\t', ''])
	name := dst.name.to_lower_ascii()
	if family == 'lea' {
		return inline_asm_raw_lea_keeps(text, name, width, arch_bits)
	}
	if src.kind == .register {
		return family in ['mov', 'or', 'and'] && src.name.to_lower_ascii() == name
	}
	if !text.starts_with('$') {
		return false
	}
	value := inline_asm_raw_integer_value(text[1..]) or { return false }
	mask := if width == 64 { max_u64 } else { (u64(1) << width) - 1 }
	return match family {
		'add', 'sub', 'or', 'xor' { value & mask == 0 }
		'and' { value & mask == mask }
		// A count is one byte, which the CPU masks to 6 bits for a 64-bit operand, 5 otherwise.
		'shl', 'shr', 'sar' { value & 0xff & u64(if width == 64 { 63 } else { 31 }) == 0 }
		else { false }
	}
}

// check_inline_asm_raw_clobbers reports a GNU x86 raw template that writes a general-purpose
// register, or sets the flags, without declaring it. It reads the template in order, without
// following jumps, and reports a destination register operand, the registers that `mul`, `div`,
// `cqo` and `rep` write implicitly, and a flag-setting instruction without `cc` or `flags`. It does
// not report other mnemonics, nor flags that inline_asm_raw_restored_regions shows are restored. A
// write to a register that inline_asm_raw_restored_regions shows is restored, or that
// inline_asm_raw_mov_restored returns, is not reported either, unless the compiler may place
// an operand in it (InlineAsmRawConstraint.is_free and may_sit). A template that inline_asm_raw_template_is_linear does not accept, that has a
// prefix inline_asm_raw_has_unread_prefix finds or a suffix inline_asm_raw_has_encoding_suffix
// finds, or that touches the stack in a way inline_asm_raw_restored_regions does not follow, is not
// checked at all.
fn (mut tc TypeChecker) check_inline_asm_raw_clobbers(id flat.NodeId, node flat.Node, block string, base int, sections []InlineAsmRange, arch string, types map[string]Type) {
	template := inline_asm_raw_template(block, sections[0], arch)
	instructions := inline_asm_raw_parse_instructions(&template, [])
	if !inline_asm_raw_template_is_linear(&template, instructions) {
		return
	}
	for instruction in instructions {
		if inline_asm_raw_has_unread_prefix(instruction)
			|| inline_asm_raw_has_encoding_suffix(instruction) {
			return
		}
	}
	arch_bits := inline_asm_att_arch_bits(arch)
	// The width of each operand reference, where its type fixes one, for the stack model.
	mut ref_bits := map[string]int{}
	for instruction in instructions {
		for operand in instruction.operands {
			if operand.kind == .operand_ref {
				width := tc.inline_asm_att_operand_width(operand, types, arch_bits)
				if width > 0 {
					ref_bits[inline_asm_raw_ref_key(operand)] = width
				}
			}
		}
	}
	regions := inline_asm_raw_restored_regions(instructions, arch_bits, ref_bits) or { return }
	// A save and restore keeps a register only when no operand can sit in it: the compiler may give
	// an `r` or `m` operand, or the address of a memory operand, the register the template saves.
	// The restore then overwrites an output there, and an input or an address read after the write
	// has changed. The flags hold no operand.
	mut has_free_operand := false
	mut may_sit := map[string]bool{}
	for s in 1 .. sections.len {
		if s <= 2 {
			for io in inline_asm_ios(block, sections[s]) {
				constraint := inline_asm_raw_read_constraint(io.constraint)
				if constraint.is_free {
					has_free_operand = true
				}
				for register in constraint.may_sit {
					may_sit[register] = true
				}
			}
		}
	}
	mut clobbers := map[string]bool{}
	mut has_cc := false
	// `pair_bound` counts every operand whose constraint names `rax` or `rdx` (`A`, `a`, `d` or a
	// braced name), the `A` operand itself included, and every counted clobber of either. A narrow `A`
	// operand is checked only when it is 1 (`alone`); otherwise the check counts both registers as
	// declared.
	mut pair_bound := 0
	if sections.len > 3 {
		// A clobber may name a register by number, which is not resolved here, with an optional
		// `%` or `#` prefix. Clobbers may be separated by commas as well as blanks.
		for field in block[sections[3].start..sections[3].end].replace(',', ' ').fields() {
			number := field.trim_left('%#')
			if number.len > 0 && number[0].is_digit() {
				return
			}
		}
		for word in inline_asm_words(block, sections[3]) {
			// `flags` is another name of the `cc` clobber.
			if word.text in ['cc', 'flags'] {
				has_cc = true
			}
			if !inline_asm_raw_clobber_counts(word.text, arch_bits) {
				continue
			}
			family := inline_asm_raw_x86_family(word.text)
			clobbers[family] = true
			if family in ['rax', 'rdx'] {
				pair_bound++
			}
		}
	}
	for s in 1 .. sections.len {
		if s <= 2 {
			for io in inline_asm_ios(block, sections[s]) {
				if inline_asm_raw_read_constraint(io.constraint).binds_pair_register() {
					pair_bound++
				}
			}
		}
	}
	if sections.len > 1 {
		for io in inline_asm_ios(block, sections[1]) {
			// A flag output such as `"=@ccz"` declares the flags without a `cc` clobber.
			if io.constraint.contains('@cc') {
				has_cc = true
			}
		}
		for register in inline_asm_raw_fixed_registers(block, sections[1]) {
			clobbers[register] = true
		}
	}
	mut pair := ?InlineAsmRawPairOperand(none)
	if sections.len > 1 {
		pair = tc.inline_asm_raw_pair_outputs(node, block, sections[1], arch_bits, pair_bound == 1, mut
			clobbers)
	}
	// The registers declared so far are those the template may change. The inputs are added below.
	changeable := clobbers.clone()
	if sections.len > 2 {
		// A template may change a fixed input if it restores it, so a write to one is not proven
		// wrong.
		for register in inline_asm_raw_fixed_registers(block, sections[2]) {
			clobbers[register] = true
		}
		outputs := inline_asm_ios(block, sections[1]).len
		if p := tc.inline_asm_raw_pair_inputs(node, block, sections[2], outputs, arch_bits, pair_bound == 1, mut
			clobbers)
		{
			pair = p
		}
	}
	mov_restored := inline_asm_raw_mov_restored(instructions, arch_bits, &InlineAsmRawSavePlaces{
		changeable: changeable
		named:      clobbers
		input_refs: inline_asm_raw_input_refs(block, sections, changeable)
	})
	// `rep; movsb` and a `rep` on a line of its own prefix the next instruction, unless that starts
	// with `wait`. A `rep` that meets a `lock`, in its own statement or on lines of prefixes only
	// before or after it, and a lone `rep wait`, are counted for no instruction, as assemblers
	// disagree on them (see vlib/v/types/README.md).
	mut rep_pending := false
	// A `lock` on lines of prefixes only, waiting for the instruction it prefixes.
	mut lock_pending := false
	for index, instruction in instructions {
		if instruction.is_directive {
			// An alignment directive may emit padding that the prefix applies to; other accepted
			// directives emit nothing, so the prefix still reaches the next instruction.
			directive := instruction.mnemonic.to_lower_ascii().all_before(' ')
			if directive in ['.align', '.p2align', '.balign'] {
				rep_pending = false
			}
			continue
		}
		if instruction.mnemonic.len == 0 {
			continue
		}
		words := instruction.mnemonic.to_lower_ascii().split(' ')
		name := words[words.len - 1]
		locked := lock_pending || 'lock' in words
		has_rep := !locked && ((rep_pending && words[0] != 'wait')
			|| words.any(it in inline_asm_raw_rep_prefixes))
		rep_pending = instruction.operands.len == 0 && inline_asm_raw_lone_prefix_carries_rep(words)
		lock_pending = locked && instruction.operands.len == 0
			&& words.all(inline_asm_raw_is_prefix(it))
		mnemonic_start, mnemonic_end := template.span(instruction.mnemonic_start, instruction.mnemonic_end)
		// A destination operand is reported at its own span, implicit writes at the mnemonic.
		mut written_at := map[string]token.Pos{}
		mut written := tc.inline_asm_raw_writes(instruction, name, has_rep, types, arch_bits)
		if dst := inline_asm_raw_written_destination(instruction, name, arch_bits) {
			start, end := template.span(dst.start, dst.end)
			written_at[inline_asm_raw_x86_family(dst.name)] = token.new_span(node.pos.id, base + start,
				base + end)
		}
		for register in inline_asm_raw_gp_registers {
			if written[register] && !has_free_operand && !may_sit[register]
				&& (inline_asm_raw_is_restored(regions, register, index) || mov_restored[register]) {
				written[register] = false
			}
		}
		if p := pair {
			if (written['rax'] && !clobbers['rax']) || (written['rdx'] && !clobbers['rdx']) {
				// The check does not know which register, if either, holds a narrow `A` operand, so a
				// write to either one is not declared by it. The report states that and suggests no
				// fix: which register is right depends on what the template reads, such as `eax` for
				// `cltd` or `mul`, and the check does not model those reads.
				ax := inline_asm_raw_register_name('rax', arch_bits)
				dx := inline_asm_raw_register_name('rdx', arch_bits)
				shown := if written['rax'] && !clobbers['rax'] { ax } else { dx }
				tc.record_error_at(.compile_error, '`${instruction.mnemonic}` writes `${shown}` in this raw assembly template, but the `${p.constraint}` constraint on the ${p.width}-bit operand `${p.name}` does not reserve both `${ax}` and `${dx}`',
					id, token.new_span(node.pos.id, base + p.start, base + p.end))
				// The operand is reported once; its registers are not reported again.
				clobbers['rax'] = true
				clobbers['rdx'] = true
				pair = none
			}
		}
		for register in inline_asm_raw_gp_registers {
			if written[register] && !clobbers[register] {
				span := if register in written_at {
					written_at[register]
				} else {
					token.new_span(node.pos.id, base + mnemonic_start, base + mnemonic_end)
				}
				shown := inline_asm_raw_register_name(register, arch_bits)
				// GCC refuses an `rbp` clobber beside a frame pointer, so the report suggests a `mov`
				// save, and not `push`, which may overwrite the red zone.
				fix := if register == 'rbp' {
					'copy it with `mov` to a register in the clobbers and back, and bind every operand to a fixed register such as `=d`, or to a constant'
				} else {
					'add `${shown}` to the clobbers'
				}
				tc.record_error_at(.compile_error, '`${instruction.mnemonic}` writes `${shown}` in this raw assembly template, but `${shown}` is neither an output operand nor in the clobber list; ${fix}',
					id, span)
			}
		}
		if inline_asm_raw_base_mnemonic(name, inline_asm_raw_flag_families).len > 0 && !has_cc
			&& !inline_asm_raw_is_restored(regions, 'flags', index) {
			tc.record_error_at(.compile_error, '`${instruction.mnemonic}` sets the condition flags in this raw assembly template, but the clobbers do not list `cc`; add `cc` to the clobbers',
				id, token.new_span(node.pos.id, base + mnemonic_start, base + mnemonic_end))
		}
	}
}
