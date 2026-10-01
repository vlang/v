module types

import v.flat
import v.token
import v.util

// InlineAsmRange is a half open [start, end) span into a masked assembly block.
struct InlineAsmRange {
	start int
	end   int
}

// InlineAsmWord is an identifier found in an assembly block, with its block offsets.
struct InlineAsmWord {
	text  string
	start int
	end   int
}

// InlineAsmIOSpan is one `constraint (expression)` operand, with its block offsets.
struct InlineAsmIOSpan {
	constraint string
	alias      string
	expr       string
	start      int
	end        int
}

// InlineAsmIntelArg is the source-level shape needed for Intel operand-width checks.
// Inline assembly is retained as source by the flat AST, so these lightweight operands
// deliberately preserve only aliases, hard registers, literals, and address expressions.
struct InlineAsmIntelArg {
	text       string
	words      []InlineAsmWord
	is_address bool
	is_literal bool
	literal    int
}

struct InlineAsmIntelInstruction {
	name  string
	args  []InlineAsmIntelArg
	start int
	end   int
}

const asm_intel_flag_cf = u8(1 << 0)
const asm_intel_flag_pf = u8(1 << 1)
const asm_intel_flag_af = u8(1 << 2)
const asm_intel_flag_zf = u8(1 << 3)
const asm_intel_flag_sf = u8(1 << 4)
const asm_intel_flag_of = u8(1 << 5)
const asm_intel_status_flags = asm_intel_flag_cf | asm_intel_flag_pf | asm_intel_flag_af |
	asm_intel_flag_zf | asm_intel_flag_sf | asm_intel_flag_of

// x86 instruction prefixes are written as a separate word, so the mnemonic that can be
// mistaken for an operand is the one after them.
const inline_asm_instruction_prefixes = ['lock', 'rep', 'repe', 'repz', 'repne', 'repnz']!

// Intel memory operands can use these words to select an address, distance or
// explicit operand size. They are syntax, not possible misspellings of registers.
const inline_asm_intel_operand_keywords = ['ptr', 'byte', 'word', 'dword', 'qword', 'tbyte', 'oword',
	'xmmword', 'ymmword', 'zmmword', 'short', 'near', 'far', 'offset', 'rel', 'abs', 'rn', 'rd',
	'ru', 'rz', 'sae']!

// Arm64 uses identifiers for shift and extension operators, condition codes,
// barrier domains, and vector/predicate qualifiers inside otherwise structured operands.
const inline_asm_arm64_operand_keywords = ['lsl', 'lsr', 'asr', 'ror', 'msl', 'uxtb', 'uxth', 'uxtw',
	'uxtx', 'sxtb', 'sxth', 'sxtw', 'sxtx', 'eq', 'ne', 'cs', 'hs', 'cc', 'lo', 'mi', 'pl', 'vs',
	'vc', 'hi', 'ls', 'ge', 'lt', 'gt', 'le', 'al', 'nv', 'sy', 'st', 'ld', 'osh', 'oshst', 'oshld',
	'nsh', 'nshst', 'nshld', 'ish', 'ishst', 'ishld', 'mul', 'vl', 'b', 'h', 's', 'd', 'q', 'z',
	'm', 'sm', 'c', 'j', 'jc', 'fpmr', 'pow2', 'mul3', 'mul4', 'all']!

// check_inline_asm_block reports the assembly diagnostics that only need the block's
// preserved source: unsupported operand constraints in structured `intel` blocks, and
// register names that were probably misspelled.
fn (mut tc TypeChecker) check_inline_asm_block(id flat.NodeId, node flat.Node, source string, start int, end int) {
	if start >= end {
		return
	}
	// Comments become spaces so that block offsets keep matching file offsets.
	block := inline_asm_mask_comments(source[start..end])
	open := inline_asm_index_of(block, 0, block.len, `{`) or { return }
	close := inline_asm_last_index_of(block, open, block.len, `}`) or { return }
	header := util.parse_inline_asm_header(block[..open])
	registers := util.asm_register_names(header.arch)
	sections := inline_asm_section_ranges(block, open + 1, close)
	if sections.len > 1 {
		for index, _ in inline_asm_ios(block, sections[1]) {
			if index < int(node.children_count) {
				tc.check_inline_asm_output_lvalue(tc.a.child(&node, index))
			}
		}
	}
	if header.is_intel && !header.is_raw {
		intel_io_errors_before := tc.errors.len
		mut child_offset := 0
		mut operand_types := map[string]Type{}
		for index in 1 .. 3 {
			if index < sections.len {
				ios := inline_asm_ios(block, sections[index])
				tc.check_inline_asm_intel_ios(id, node, start, ios, index == 1)
				for io_index, io in ios {
					child_index := child_offset + io_index
					if io.alias.len > 0 && inline_asm_is_ident(io.expr)
						&& child_index < int(node.children_count) {
						operand_types[io.alias] = tc.resolve_type(tc.a.child(&node, child_index))
					}
				}
				child_offset += ios.len
			}
		}
		if tc.errors.len == intel_io_errors_before
			&& header.arch in ['amd64', 'x64', 'x86_64', 'i386', 'i486', 'i586', 'i686', 'x86',
				'x86_32', 'ia-32', 'ia32'] && sections.len > 0 {
			tc.check_inline_asm_intel_operand_widths(id, node, source, block, start, sections[0],
				operand_types, registers)
		}
	}
	if registers.len == 0 {
		// Without a register table for this instruction set every name looks unknown.
		return
	}
	if sections.len > 3 {
		tc.check_inline_asm_clobbers(id, node, block, start, sections[3], registers)
	}
	if !header.is_raw && sections.len > 0 {
		mut aliases := map[string]bool{}
		for index in 1 .. 3 {
			if index < sections.len {
				for io in inline_asm_ios(block, sections[index]) {
					if io.alias.len > 0 {
						aliases[io.alias] = true
					}
				}
			}
		}
		if header.is_goto && sections.len > 4 {
			for label in inline_asm_words(block, sections[4]) {
				aliases[label.text] = true
			}
		}
		tc.check_inline_asm_templates(id, node, block, start, sections[0], registers, aliases, header.arch, header.is_intel)
	}
}

fn inline_asm_goto_labels(source string) []string {
	block := inline_asm_mask_comments(source)
	open := inline_asm_index_of(block, 0, block.len, `{`) or { return [] }
	close := inline_asm_last_index_of(block, open, block.len, `}`) or { return [] }
	header := util.parse_inline_asm_header(block[..open])
	if !header.is_goto {
		return []
	}
	sections := inline_asm_section_ranges(block, open + 1, close)
	if sections.len <= 4 {
		return []
	}
	mut labels := []string{}
	for label in inline_asm_words(block, sections[4]) {
		if label.text !in labels {
			labels << label.text
		}
	}
	return labels
}

fn (mut tc TypeChecker) check_inline_asm_output_lvalue(id flat.NodeId) {
	if !tc.expr_can_take_address(id) {
		tc.record_error_at(.assignment_mismatch, 'inline assembly output must be an lvalue', id, tc.a.node(id).pos)
		return
	}
	tc.check_lvalue_mutability(id)
}

// check_inline_asm_intel_ios rejects the operand constraints that a structured `intel`
// block cannot express, because compilers still format those placeholders as AT&T.
fn (mut tc TypeChecker) check_inline_asm_intel_ios(id flat.NodeId, node flat.Node, base int, ios []InlineAsmIOSpan, is_output bool) {
	for io in ios {
		constraint := if io.constraint == '' {
			if is_output { '+r' } else { 'r' }
		} else {
			io.constraint
		}
		if is_output && constraint[0] !in [`=`, `+`] {
			tc.record_error_at(.compile_error, 'output constraint `${constraint}` must start with `=` or `+`', id, token.new_span(node.pos.id, base + io.start, base + io.end))
			continue
		}
		if !is_output && constraint.bytes().any(it in [`=`, `+`, `&`]) {
			tc.record_error_at(.compile_error, 'input constraint `${constraint}` cannot use output modifiers `=`, `+`, or `&`', id, token.new_span(node.pos.id, base + io.start, base + io.end))
			continue
		}
		if constraint.trim_left('=+&%*') == 'r' {
			continue
		}
		tc.record_error_at(.compile_error, 'constraint `${constraint}` is not supported for operands in structured `intel` assembly; use a register-only `r` constraint or a `raw` template with explicit operand modifiers', id, token.new_span(node.pos.id, base + io.start, base + io.end))
	}
}

fn (tc &TypeChecker) inline_asm_intel_operand_width(typ Type) ?int {
	clean := unalias_type(typ)
	return match clean {
		Primitive {
			if clean.props.has(.boolean) {
				8
			} else if clean.props.has(.integer) || clean.props.has(.float) {
				if clean.size == 0 { 32 } else { int(clean.size) }
			} else {
				none
			}
		}
		Char { 8 }
		Rune { 32 }
		ISize, USize, Pointer { platform_int_bits() }
		Enum { tc.inline_asm_enum_backing_width(clean.name) }
		else { none }
	}
}

fn (tc &TypeChecker) inline_asm_enum_backing_width(name string) int {
	for index in tc.top_level_idx {
		decl := tc.a.nodes[index]
		if decl.kind != .enum_decl || decl.value != name.all_after_last('.') {
			continue
		}
		file := tc.a.source_files[decl.pos.id] or { continue }
		module_name := tc.file_modules[file.name] or { '' }
		qualified := qualify_decl_name_in_module(decl.value, module_name)
		if name.contains('.') && qualified != name {
			continue
		}
		if decl.generic_params().len > 0 && decl.generic_params()[0].len > 0 {
			if width := tc.inline_asm_intel_operand_width(tc.parse_type(decl.generic_params()[0])) {
				return width
			}
		}
		break
	}
	return 32
}

fn (tc &TypeChecker) inline_asm_enum_backing_type(name string) Type {
	for index in tc.top_level_idx {
		decl := tc.a.nodes[index]
		if decl.kind != .enum_decl || decl.value != name.all_after_last('.') {
			continue
		}
		file := tc.a.source_files[decl.pos.id] or { continue }
		module_name := tc.file_modules[file.name] or { '' }
		qualified := qualify_decl_name_in_module(decl.value, module_name)
		if name.contains('.') && qualified != name {
			continue
		}
		if decl.generic_params().len > 0 && decl.generic_params()[0].len > 0 {
			return tc.parse_type(decl.generic_params()[0])
		}
		break
	}
	return Type(int_)
}

fn (tc &TypeChecker) inline_asm_intel_type_is_signed(typ Type) bool {
	clean := unalias_type(typ)
	return match clean {
		Primitive { clean.props.has(.integer) && !clean.props.has(.unsigned) }
		Char, Rune, USize, Pointer { false }
		ISize { true }
		Enum { tc.inline_asm_intel_type_is_signed(tc.inline_asm_enum_backing_type(clean.name)) }
		else { false }
	}
}

fn inline_asm_intel_register_width(name string) int {
	register := name.to_lower_ascii()
	if register in ['al', 'ah', 'bl', 'bh', 'cl', 'ch', 'dl', 'dh', 'bpl', 'sil', 'dil', 'spl']
		|| (register.starts_with('r') && register.ends_with('b')) {
		return 8
	}
	if register in ['ax', 'bx', 'cx', 'dx', 'bp', 'si', 'di', 'sp', 'cs', 'ss', 'ds', 'es', 'fs',
		'gs'] || (register.starts_with('r') && register.ends_with('w')) {
		return 16
	}
	if register in ['eax', 'ebx', 'ecx', 'edx', 'ebp', 'esi', 'edi', 'esp', 'eip', 'eiz', 'eflags',
		'mxcsr'] || (register.starts_with('r') && register.ends_with('d')) {
		return 32
	}
	if register.starts_with('xmm') {
		return 128
	}
	if register.starts_with('ymm') {
		return 256
	}
	if register.starts_with('zmm') {
		return 512
	}
	if register.starts_with('mm') || register.starts_with('cr') || register.starts_with('dr')
		|| register.starts_with('r') || register in ['ip', 'flags', 'gdtr', 'idtr', 'tr', 'ldtr'] {
		return 64
	}
	return 0
}

fn inline_asm_intel_parse_instructions(block string, section InlineAsmRange) []InlineAsmIntelInstruction {
	mut instructions := []InlineAsmIntelInstruction{}
	for line in inline_asm_lines(block, section) {
		mut start := inline_asm_skip_leading_label(block, line)
		if start == line.start {
			start = inline_asm_skip_blanks(block, line.start, line.end)
		}
		mut end := line.end
		for end > start && block[end - 1].is_space() {
			end--
		}
		if start >= end || block[start] == `.` {
			continue
		}
		mut mnemonic_end := start
		for mnemonic_end < end && !block[mnemonic_end].is_space() {
			mnemonic_end++
		}
		mut name := block[start..mnemonic_end]
		mut args_start := inline_asm_skip_blanks(block, mnemonic_end, end)
		lower_name := name.to_lower_ascii()
		if lower_name in ['lock', 'rex', 'vex', 'xop'] || lower_name.starts_with('rex.')
			|| lower_name.starts_with('vex.') || lower_name.starts_with('xop.') {
			second_end := inline_asm_skip_mnemonic(block, InlineAsmRange{
				start: args_start
				end:   end
			})
			if second_end > args_start {
				name += ' ' + block[args_start..second_end]
				args_start = inline_asm_skip_blanks(block, second_end, end)
			}
		}
		instructions << InlineAsmIntelInstruction{
			name:  name
			args:  inline_asm_intel_parse_args(block, args_start, end)
			start: start
			end:   end
		}
	}
	return instructions
}

fn inline_asm_intel_parse_args(block string, start int, end int) []InlineAsmIntelArg {
	mut args := []InlineAsmIntelArg{}
	mut arg_start := start
	mut square_depth := 0
	mut paren_depth := 0
	mut i := start
	for i <= end {
		at_end := i == end
		if !at_end && block[i] in [`'`, `"`, `\``] {
			i = inline_asm_skip_quoted_text(block, i, end)
			continue
		}
		if !at_end {
			match block[i] {
				`[` { square_depth++ }
				`]` { square_depth-- }
				`(` { paren_depth++ }
				`)` { paren_depth-- }
				else {}
			}
		}
		if at_end || (block[i] == `,` && square_depth == 0 && paren_depth == 0) {
			mut left := arg_start
			mut right := i
			for left < right && block[left].is_space() {
				left++
			}
			for right > left && block[right - 1].is_space() {
				right--
			}
			if left < right {
				text := block[left..right]
				is_literal := text.bytes().all(it.is_digit())
				args << InlineAsmIntelArg{
					text:       text
					words:      inline_asm_words(block, InlineAsmRange{ start: left, end: right })
					is_address: text.contains('[') && text.contains(']')
					is_literal: is_literal
					literal:    if is_literal { text.int() } else { 0 }
				}
			}
			arg_start = i + 1
		}
		i++
	}
	return args
}

fn inline_asm_intel_global_labels(source string) map[string]bool {
	mut labels := map[string]bool{}
	for line in source.split_into_lines() {
		trimmed := line.trim_space()
		if trimmed.starts_with('.global ') || trimmed.starts_with('.globl ') {
			fields := trimmed.fields()
			if fields.len > 1 {
				labels[fields[1].trim_right(',')] = true
			}
		}
	}
	return labels
}

fn inline_asm_intel_normalized_instruction_name(instruction string) string {
	mut name := instruction.to_lower_ascii()
	for name.contains(' ') {
		prefix := name.all_before(' ')
		if prefix !in ['lock', 'rex', 'vex', 'xop'] && !prefix.starts_with('rex.')
			&& !prefix.starts_with('vex.') && !prefix.starts_with('xop.') {
			break
		}
		name = name.all_after(' ')
	}
	return name
}

fn inline_asm_intel_arg_alias(arg InlineAsmIntelArg, aliases map[string]Type) ?string {
	if !arg.is_address && arg.text in aliases {
		return arg.text
	}
	return none
}

fn inline_asm_intel_arg_has_named_alias(arg InlineAsmIntelArg, aliases map[string]Type) bool {
	return arg.words.any(it.text in aliases)
}

fn (mut tc TypeChecker) check_inline_asm_intel_operand_widths(id flat.NodeId, node flat.Node, source string, block string, base int, section InlineAsmRange, operand_types map[string]Type, registers []string) {
	mut aliases := operand_types.clone()
	for label, _ in inline_asm_template_labels(block, section) {
		aliases.delete(label)
	}
	for label, _ in inline_asm_intel_global_labels(source) {
		aliases.delete(label)
	}
	instructions := inline_asm_intel_parse_instructions(block, section)
	native_width := platform_int_bits()
	for index, instruction in instructions {
		for arg in instruction.args {
			if tc.check_inline_asm_intel_address_widths(id, node, instruction, arg, aliases,
				registers, native_width, base) {
				break
			}
		}
		tc.check_inline_asm_intel_instruction_widths(id, node, instruction, aliases, registers,
			native_width, inline_asm_intel_flags_are_observed_after(instructions, index, native_width),
			base)
	}
}

fn (mut tc TypeChecker) check_inline_asm_intel_address_widths(id flat.NodeId, node flat.Node, instruction InlineAsmIntelInstruction, arg InlineAsmIntelArg, aliases map[string]Type, registers []string, native_width int, base int) bool {
	if !arg.is_address {
		return false
	}
	mut register_words := []InlineAsmWord{}
	for word in arg.words {
		if word.text in aliases || word.text.to_lower_ascii() in registers {
			register_words << word
		}
	}
	if register_words.len > 2 {
		tc.record_inline_asm_intel_error(id, node, instruction,
			'register-valued displacement creates a third address register in structured `intel` assembly; use at most a base and index register, or a `raw intel` block with an explicit address expression',
			base)
		return true
	}
	if register_words.any(it.text.to_lower_ascii() == 'rip')
		&& register_words.any(it.text in aliases) {
		tc.record_inline_asm_intel_error(id, node, instruction,
			'named operands cannot be used as RIP-relative displacements in structured `intel` assembly; use a literal or label displacement, or a `raw intel` block with an explicit operand modifier',
			base)
		return true
	}
	if !inline_asm_intel_arg_has_named_alias(arg, aliases) {
		return false
	}
	name := inline_asm_intel_normalized_instruction_name(instruction.name)
	is_vsib := name in ['vpgatherdd', 'vpgatherdq', 'vpgatherqd', 'vpgatherqq', 'vgatherdps',
		'vgatherdpd', 'vgatherqps', 'vgatherqpd', 'vpscatterdd', 'vpscatterdq', 'vpscatterqd',
		'vpscatterqq', 'vscatterdps', 'vscatterdpd', 'vscatterqps', 'vscatterqpd']
	for word_index, word in register_words {
		if word.text in aliases {
			typ := aliases[word.text] or { continue }
			if width := tc.inline_asm_intel_operand_width(typ) {
				if width != native_width && tc.inline_asm_intel_type_is_signed(typ) {
					tc.record_inline_asm_intel_error(id, node, instruction,
						'address operand `${word.text}` has ${width}-bit signed type `${typ.name()}`, but structured `intel` assembly substitutes a ${native_width}-bit register without sign extension; use a native-width address operand, or a `raw intel` block with an explicit operand modifier',
						base)
					return true
				}
			}
			continue
		}
		register := word.text.to_lower_ascii()
		width := inline_asm_intel_register_width(register)
		if width <= 0 || width == native_width {
			continue
		}
		if is_vsib && word_index == 1
			&& (register.starts_with('xmm') || register.starts_with('ymm')
				|| register.starts_with('zmm')) {
			continue
		}
		tc.record_inline_asm_intel_error(id, node, instruction,
			'hard register `${word.text}` is ${width}-bit, but named operands in the same structured `intel` address expand to ${native_width}-bit registers for the current compilation target; use matching address-register widths, or a `raw intel` block with explicit operand modifiers',
			base)
		return true
	}
	return false
}

fn (mut tc TypeChecker) record_inline_asm_intel_error(id flat.NodeId, node flat.Node, instruction InlineAsmIntelInstruction, message string, base int) {
	tc.record_error_at(.assignment_mismatch, message, id, token.new_span(node.pos.id, base + instruction.start,
		base + instruction.end))
}

fn inline_asm_intel_mnemonic_is_one_of(name string, mnemonics []string) bool {
	if name in mnemonics {
		return true
	}
	return name.len > 1 && name[name.len - 1] in [`b`, `w`, `l`, `q`]
		&& name[..name.len - 1] in mnemonics
}

fn inline_asm_intel_condition_flags(instruction string) u8 {
	name := inline_asm_intel_normalized_instruction_name(instruction)
	mut condition := if name.starts_with('cmov') {
		name[4..]
	} else if name.starts_with('set') {
		name[3..]
	} else if name.starts_with('j') {
		name[1..]
	} else {
		return 0
	}
	if condition.len > 1 && condition[condition.len - 1] in [`b`, `w`, `l`, `q`]
		&& condition[..condition.len - 1] in ['a', 'ae', 'b', 'be', 'c', 'e', 'g', 'ge', 'l', 'le',
			'na', 'nae', 'nb', 'nbe', 'nc', 'ne', 'ng', 'nge', 'nl', 'nle', 'no', 'np', 'ns', 'nz',
			'o', 'p', 'pe', 'po', 's', 'z'] {
		condition = condition[..condition.len - 1]
	}
	return match condition {
		'b', 'c', 'nae', 'ae', 'nb', 'nc' { asm_intel_flag_cf }
		'p', 'pe', 'np', 'po' { asm_intel_flag_pf }
		'e', 'z', 'ne', 'nz' { asm_intel_flag_zf }
		'a', 'nbe', 'be', 'na' { asm_intel_flag_cf | asm_intel_flag_zf }
		's', 'ns' { asm_intel_flag_sf }
		'o', 'no' { asm_intel_flag_of }
		'l', 'nge', 'ge', 'nl' { asm_intel_flag_sf | asm_intel_flag_of }
		'le', 'ng', 'g', 'nle' { asm_intel_flag_zf | asm_intel_flag_sf | asm_intel_flag_of }
		else { u8(0) }
	}
}

fn inline_asm_intel_instruction_read_flags(instruction string) u8 {
	name := inline_asm_intel_normalized_instruction_name(instruction)
	if name == 'cmc' {
		return asm_intel_flag_cf
	}
	if inline_asm_intel_mnemonic_is_one_of(name, ['adc', 'adcx', 'sbb', 'rcl', 'rcr']) {
		return asm_intel_flag_cf
	}
	if inline_asm_intel_mnemonic_is_one_of(name, ['adox']) {
		return asm_intel_flag_of
	}
	condition_flags := inline_asm_intel_condition_flags(name)
	if condition_flags != 0 {
		return condition_flags
	}
	if name == 'lahf' {
		return asm_intel_status_flags & ~asm_intel_flag_of
	}
	if name in ['pushf', 'pushfd', 'pushfq'] {
		return asm_intel_status_flags
	}
	if name.starts_with('loopz') || name.starts_with('loope') || name.starts_with('loopnz')
		|| name.starts_with('loopne') {
		return asm_intel_flag_zf
	}
	return 0
}

fn inline_asm_intel_instruction_set_flags(instruction string) u8 {
	name := inline_asm_intel_normalized_instruction_name(instruction)
	if inline_asm_intel_mnemonic_is_one_of(name, ['adcx']) {
		return asm_intel_flag_cf
	}
	if inline_asm_intel_mnemonic_is_one_of(name, ['adox']) {
		return asm_intel_flag_of
	}
	if inline_asm_intel_mnemonic_is_one_of(name, ['inc', 'dec']) {
		return asm_intel_status_flags & ~asm_intel_flag_cf
	}
	if inline_asm_intel_mnemonic_is_one_of(name, ['and', 'or', 'test', 'xor']) {
		return asm_intel_status_flags & ~asm_intel_flag_af
	}
	if inline_asm_intel_mnemonic_is_one_of(name, ['andn', 'bextr', 'blsi', 'blsmsk', 'blsr', 'bzhi']) {
		return asm_intel_flag_cf | asm_intel_flag_zf | asm_intel_flag_sf | asm_intel_flag_of
	}
	if inline_asm_intel_mnemonic_is_one_of(name, ['bsf', 'bsr']) {
		return asm_intel_flag_zf
	}
	if inline_asm_intel_mnemonic_is_one_of(name, ['bt', 'btc', 'btr', 'bts']) {
		return asm_intel_flag_cf
	}
	if inline_asm_intel_mnemonic_is_one_of(name, ['lzcnt', 'tzcnt']) {
		return asm_intel_flag_cf | asm_intel_flag_zf
	}
	if inline_asm_intel_mnemonic_is_one_of(name, ['imul', 'mul']) {
		return asm_intel_flag_cf | asm_intel_flag_of
	}
	if inline_asm_intel_mnemonic_is_one_of(name, ['popcnt']) {
		return asm_intel_status_flags
	}
	if name in ['clc', 'cmc', 'stc'] {
		return asm_intel_flag_cf
	}
	if name == 'sahf' {
		return asm_intel_status_flags & ~asm_intel_flag_of
	}
	if inline_asm_intel_mnemonic_is_one_of(name, ['add', 'adc', 'cmp', 'cmpxchg', 'neg', 'sbb',
		'sub', 'xadd']) {
		return asm_intel_status_flags
	}
	return 0
}

fn inline_asm_intel_static_shift_count(instruction InlineAsmIntelInstruction, name string, native_width int) int {
	is_double_shift := inline_asm_intel_mnemonic_is_one_of(name, ['shld', 'shrd'])
	if !is_double_shift && instruction.args.len == 1 {
		return 1
	}
	if instruction.args.len > 0 {
		count := instruction.args.last()
		if count.is_literal {
			mut operand_width := native_width
			destination := instruction.args[0]
			if !destination.is_address {
				register_width := inline_asm_intel_register_width(destination.text)
				if register_width > 0 {
					operand_width = register_width
				} else if name.len > 1 && name[name.len - 1] in [`b`, `w`, `l`, `q`] {
					operand_width = match name[name.len - 1] {
						`b` { 8 }
						`w` { 16 }
						`l` { 32 }
						else { 64 }
					}
				}
			}
			count_mask := if operand_width == 64 { 63 } else { 31 }
			return count.literal & count_mask
		}
	}
	return 0
}

fn inline_asm_intel_instruction_overwritten_flags(instruction InlineAsmIntelInstruction, native_width int) u8 {
	name := inline_asm_intel_normalized_instruction_name(instruction.name)
	if name in ['popf', 'popfd', 'popfq'] {
		return asm_intel_status_flags
	}
	if name in ['clc', 'cmc', 'stc'] {
		return asm_intel_flag_cf
	}
	if name == 'sahf' {
		return asm_intel_status_flags & ~asm_intel_flag_of
	}
	if inline_asm_intel_mnemonic_is_one_of(name, ['inc', 'dec']) {
		return asm_intel_status_flags & ~asm_intel_flag_cf
	}
	if inline_asm_intel_mnemonic_is_one_of(name, ['adcx']) {
		return asm_intel_flag_cf
	}
	if inline_asm_intel_mnemonic_is_one_of(name, ['adox']) {
		return asm_intel_flag_of
	}
	if inline_asm_intel_mnemonic_is_one_of(name, ['div', 'idiv']) {
		return asm_intel_status_flags
	}
	shift_count := inline_asm_intel_static_shift_count(instruction, name, native_width)
	if shift_count > 0 {
		if inline_asm_intel_mnemonic_is_one_of(name, ['rol', 'ror', 'rcl', 'rcr']) {
			return asm_intel_flag_cf | if shift_count == 1 { asm_intel_flag_of } else { u8(0) }
		}
		if inline_asm_intel_mnemonic_is_one_of(name, ['sal', 'sar', 'shl', 'shr', 'shld', 'shrd']) {
			return asm_intel_status_flags
		}
	}
	if inline_asm_intel_instruction_set_flags(name) != 0 {
		return asm_intel_status_flags
	}
	return 0
}

fn inline_asm_intel_instruction_changes_control_flow(instruction string) bool {
	name := inline_asm_intel_normalized_instruction_name(instruction)
	return name.starts_with('j') || name == 'ljmp' || name.starts_with('loop')
}

fn inline_asm_intel_flags_are_observed_after(instructions []InlineAsmIntelInstruction, instruction_index int, native_width int) bool {
	mut remaining_flags := inline_asm_intel_instruction_set_flags(instructions[instruction_index].name)
	if remaining_flags == 0 {
		return false
	}
	for index in instruction_index + 1 .. instructions.len {
		if inline_asm_intel_instruction_read_flags(instructions[index].name) & remaining_flags != 0 {
			return true
		}
		if inline_asm_intel_instruction_changes_control_flow(instructions[index].name) {
			return true
		}
		remaining_flags &= asm_intel_status_flags ^
			inline_asm_intel_instruction_overwritten_flags(instructions[index], native_width)
		if remaining_flags == 0 {
			return false
		}
	}
	return false
}

fn inline_asm_intel_extension_move_source_width(instruction string) int {
	return match instruction {
		'movzbw', 'movzbl', 'movzbq', 'movsbw', 'movsbl', 'movsbq' { 8 }
		'movzwl', 'movzwq', 'movswl', 'movswq' { 16 }
		'movsxd', 'movslq' { 32 }
		else { 0 }
	}
}

fn inline_asm_intel_extension_move_destination_width(instruction string) int {
	if instruction == 'movsxd' {
		return 64
	}
	if inline_asm_intel_extension_move_source_width(instruction) == 0 {
		return 0
	}
	return match instruction[instruction.len - 1] {
		`w` { 16 }
		`l` { 32 }
		else { 64 }
	}
}

fn inline_asm_intel_operand_is_data_source(instruction string, operand_index int) bool {
	return match instruction {
		'bt', 'btc', 'btr', 'bts' { false }
		'bextr', 'bzhi', 'rorx', 'shld', 'shrd' { operand_index == 1 }
		'mulx' { operand_index == 2 }
		else { operand_index > 0 }
	}
}

fn inline_asm_intel_crc32_source_width_is_valid(source_width int, native_width int) bool {
	return source_width == 8 || source_width == native_width
		|| (native_width == 32 && source_width == 16)
}

fn (mut tc TypeChecker) check_inline_asm_intel_named_shift_count(id flat.NodeId, node flat.Node, instruction InlineAsmIntelInstruction, aliases map[string]Type, count_index int, cl_allowed bool, base int) bool {
	if instruction.args.len <= count_index {
		return false
	}
	count := instruction.args[count_index]
	alias := inline_asm_intel_arg_alias(count, aliases) or { return false }
	requirement := if cl_allowed { 'an immediate or `cl`' } else { 'an immediate' }
	remedy := if cl_allowed { 'a hard `cl` register' } else { 'a literal count' }
	tc.record_inline_asm_intel_error(id, node, instruction,
		'named shift count `${alias}` expands to a native-width register in structured `intel` assembly, but instruction `${instruction.name}` requires ${requirement}; use ${remedy}, or a `raw intel` block with an explicit operand modifier',
		base)
	return true
}

fn (mut tc TypeChecker) check_inline_asm_intel_narrow_data_aliases(id flat.NodeId, node flat.Node, instruction InlineAsmIntelInstruction, aliases map[string]Type, native_width int, name string, base int) bool {
	for index, arg in instruction.args {
		alias := inline_asm_intel_arg_alias(arg, aliases) or { continue }
		typ := aliases[alias] or { continue }
		width := tc.inline_asm_intel_operand_width(typ) or { continue }
		if width == native_width {
			continue
		}
		is_width_dependent := match name {
			'movbe' { index in [0, 1] }
			'div', 'idiv', 'mul' { index == 0 }
			'imul' { instruction.args.len == 1 && index == 0 }
			'bt', 'btc', 'btr', 'bts' { index == 0 }
			'bzhi', 'rorx', 'sarx', 'shlx', 'shrx', 'lzcnt', 'tzcnt' { index == 1 }
			'shld', 'shrd' { index in [0, 1] }
			'mulx' { index == 2 }
			'crc32' { index == 1 }
			else { false }
		}
		if is_width_dependent {
			tc.record_inline_asm_intel_error(id, node, instruction,
				'named operand `${alias}` has ${width}-bit type `${typ.name()}`, but instruction `${instruction.name}` operates on the ${native_width}-bit register substituted by structured `intel` assembly; use native-width data operands, or a `raw intel` block with explicit operand modifiers',
				base)
			return true
		}
	}
	return false
}

fn (mut tc TypeChecker) check_inline_asm_intel_signed_narrow_sources(id flat.NodeId, node flat.Node, instruction InlineAsmIntelInstruction, aliases map[string]Type, name string, explicit_width int, base int) bool {
	if instruction.args.len < 2 {
		return false
	}
	destination := instruction.args[0]
	mut destination_width := explicit_width
	if destination_width == 0 {
		if alias := inline_asm_intel_arg_alias(destination, aliases) {
			if typ := aliases[alias] {
				destination_width = tc.inline_asm_intel_operand_width(typ) or { 0 }
			}
		} else if !destination.is_address {
			destination_width = inline_asm_intel_register_width(destination.text)
		}
	}
	if destination_width <= 0 {
		return false
	}
	for index, source in instruction.args {
		if !inline_asm_intel_operand_is_data_source(name, index) {
			continue
		}
		alias := inline_asm_intel_arg_alias(source, aliases) or { continue }
		typ := aliases[alias] or { continue }
		source_width := tc.inline_asm_intel_operand_width(typ) or { continue }
		if source_width < destination_width && tc.inline_asm_intel_type_is_signed(typ) {
			tc.record_inline_asm_intel_error(id, node, instruction,
				'named source `${alias}` has ${source_width}-bit signed type `${typ.name()}`, but instruction `${instruction.name}` consumes the wider ${destination_width}-bit register substituted by structured `intel` assembly without sign extension; use operands of matching width, explicitly sign-extend into a hard register, or use a `raw intel` block with an explicit operand modifier',
				base)
			return true
		}
	}
	return false
}

fn (mut tc TypeChecker) check_inline_asm_intel_extension_move_source(id flat.NodeId, node flat.Node, instruction InlineAsmIntelInstruction, aliases map[string]Type, native_width int, source_width int, base int) {
	if instruction.args.len < 2 {
		return
	}
	source := instruction.args[1]
	if alias := inline_asm_intel_arg_alias(source, aliases) {
		tc.record_inline_asm_intel_error(id, node, instruction,
			'named source `${alias}` expands to a ${native_width}-bit register in structured `intel` assembly, but instruction `${instruction.name}` requires a narrower source; use a hard source register of the required width, or a `raw intel` block with an explicit operand modifier',
			base)
		return
	}
	if !source.is_address {
		register_width := inline_asm_intel_register_width(source.text)
		if register_width > 0 {
			is_valid_source := if source_width > 0 {
				register_width == source_width
			} else {
				register_width in [8, 16]
			}
			if !is_valid_source {
				requirement := if source_width > 0 {
					'a source register of ${source_width} bits'
				} else {
					'a source narrower than its ${native_width}-bit named destination'
				}
				tc.record_inline_asm_intel_error(id, node, instruction,
					'hard source register `${source.text}` is ${register_width}-bit, but instruction `${instruction.name}` requires ${requirement}; use a hard source register of the required width, or a `raw intel` block with explicit operand modifiers',
					base)
			}
		}
	}
}

fn (mut tc TypeChecker) check_inline_asm_intel_instruction_widths(id flat.NodeId, node flat.Node, instruction InlineAsmIntelInstruction, aliases map[string]Type, registers []string, native_width int, flags_are_observed bool, base int) {
	mut name := inline_asm_intel_normalized_instruction_name(instruction.name)
	is_movq := name == 'movq'
	extension_source_width := inline_asm_intel_extension_move_source_width(name)
	extension_destination_width := inline_asm_intel_extension_move_destination_width(name)
	is_extension_move := name in ['movsx', 'movsxd', 'movzx'] || extension_source_width > 0
	same_width_instructions := ['mov', 'movbe', 'add', 'adc', 'adcx', 'adox', 'sub', 'sbb', 'and',
		'andn', 'or', 'xor', 'cmp', 'test', 'xchg', 'xadd', 'cmpxchg', 'inc', 'dec', 'neg', 'div',
		'idiv', 'imul', 'mul', 'bsf', 'bsr', 'bt', 'btc', 'btr', 'bts', 'bextr', 'blsi', 'blsmsk',
		'blsr', 'bzhi', 'mulx', 'pdep', 'pext', 'rorx', 'sarx', 'shlx', 'shrx', 'shld', 'shrd',
		'popcnt', 'lzcnt', 'tzcnt', 'crc32']
	width_sensitive_instructions := ['bswap', 'rcl', 'rcr', 'rol', 'ror', 'sal', 'sar', 'shl',
		'shr']
	mut is_same_width := name in same_width_instructions
	mut is_width_sensitive := name in width_sensitive_instructions
	mut explicit_width := 0
	cmov_conditions := ['a', 'ae', 'b', 'be', 'c', 'e', 'g', 'ge', 'l', 'le', 'na', 'nae', 'nb',
		'nbe', 'nc', 'ne', 'ng', 'nge', 'nl', 'nle', 'no', 'np', 'ns', 'nz', 'o', 'p', 'pe', 'po',
		's', 'z']
	is_suffixed_cmov := name.len > 5 && name.starts_with('cmov')
		&& name[name.len - 1] in [`b`, `w`, `l`, `q`]
		&& name[4..name.len - 1] in cmov_conditions
	if !is_same_width && name.len > 1 && name[name.len - 1] in [`b`, `w`, `l`, `q`]
		&& (name[..name.len - 1] in same_width_instructions
			|| name[..name.len - 1] in width_sensitive_instructions || is_suffixed_cmov) {
		explicit_width = match name[name.len - 1] {
			`b` { 8 }
			`w` { 16 }
			`l` { 32 }
			else { 64 }
		}
		name = name[..name.len - 1]
		is_same_width = true
		is_width_sensitive = name in width_sensitive_instructions
	}
	if !is_same_width && !is_width_sensitive && !name.starts_with('cmov') && !is_extension_move {
		return
	}
	is_implicit_width_arithmetic := name in ['div', 'idiv', 'mul']
		|| (name == 'imul' && instruction.args.len == 1)
	if is_implicit_width_arithmetic && explicit_width == 0 && instruction.args.len > 0
		&& instruction.args[0].is_address {
		tc.record_inline_asm_intel_error(id, node, instruction,
			'addressed operand in instruction `${instruction.name}` has no explicit data width in structured `intel` assembly; use an explicitly suffixed instruction, or a `raw intel` block with an explicit operand size',
			base)
		return
	}
	if is_extension_move && instruction.args.len > 1 && instruction.args[1].is_address
		&& name in ['movsx', 'movzx'] {
		tc.record_inline_asm_intel_error(id, node, instruction,
			'addressed source in instruction `${instruction.name}` has no explicit data width in structured `intel` assembly; use a hard source register of the required width, or a `raw intel` block with an explicit source size',
			base)
		return
	}
	if name == 'crc32' && explicit_width == 0 && instruction.args.len > 1
		&& instruction.args[1].is_address {
		tc.record_inline_asm_intel_error(id, node, instruction,
			'addressed source in instruction `${instruction.name}` has no explicit data width in structured `intel` assembly; use an explicitly suffixed instruction, or a `raw intel` block with an explicit source size',
			base)
		return
	}
	mut has_named_alias := false
	for arg in instruction.args {
		if _ := inline_asm_intel_arg_alias(arg, aliases) {
			has_named_alias = true
			break
		}
	}
	if !has_named_alias {
		return
	}
	if extension_destination_width > 0 && instruction.args.len > 0 {
		if alias := inline_asm_intel_arg_alias(instruction.args[0], aliases) {
			if extension_destination_width != native_width {
				tc.record_inline_asm_intel_error(id, node, instruction,
					'instruction `${instruction.name}` selects a ${extension_destination_width}-bit destination, but named destination `${alias}` expands to a ${native_width}-bit register in structured `intel` assembly; use a matching destination width, or a `raw intel` block with an explicit operand modifier',
					base)
				return
			}
		}
	}
	if explicit_width > 0 {
		if name != 'crc32' && explicit_width != native_width {
			tc.record_inline_asm_intel_error(id, node, instruction,
				'instruction `${instruction.name}` selects ${explicit_width}-bit operands, but named operands in structured `intel` assembly expand to ${native_width}-bit registers for the current compilation target; use a matching instruction width, or a `raw intel` block with explicit operand modifiers',
				base)
			return
		}
		if name == 'crc32' && instruction.args.len > 1 {
			source := instruction.args[1]
			if alias := inline_asm_intel_arg_alias(source, aliases) {
				if explicit_width != native_width {
					tc.record_inline_asm_intel_error(id, node, instruction,
						'instruction `${instruction.name}` selects a ${explicit_width}-bit source, but named source `${alias}` expands to a ${native_width}-bit register in structured `intel` assembly; use a matching instruction width, or a `raw intel` block with an explicit operand modifier',
						base)
					return
				}
			}
			if source.is_address
				&& !inline_asm_intel_crc32_source_width_is_valid(explicit_width, native_width) {
				tc.record_inline_asm_intel_error(id, node, instruction,
					'instruction `${instruction.name}` selects a ${explicit_width}-bit memory source, which is incompatible with the ${native_width}-bit named destination in structured `intel` assembly; use a valid CRC32 source width, or a `raw intel` block with explicit operand modifiers',
					base)
				return
			}
		}
	}
	if is_extension_move {
		tc.check_inline_asm_intel_extension_move_source(id, node, instruction, aliases, native_width,
			extension_source_width, base)
		return
	}
	if tc.check_inline_asm_intel_narrow_data_aliases(id, node, instruction, aliases, native_width,
		name, base) {
		return
	}
	if (is_same_width || name.starts_with('cmov'))
		&& tc.check_inline_asm_intel_signed_narrow_sources(id, node, instruction, aliases, name,
			explicit_width, base) {
		return
	}
	if name in ['cmp', 'test'] {
		for arg in instruction.args {
			alias := inline_asm_intel_arg_alias(arg, aliases) or { continue }
			typ := aliases[alias] or { continue }
			width := tc.inline_asm_intel_operand_width(typ) or { continue }
			if width != native_width && tc.inline_asm_intel_type_is_signed(typ) {
				tc.record_inline_asm_intel_error(id, node, instruction,
					'named operand `${alias}` has ${width}-bit signed type `${typ.name()}`, but instruction `${instruction.name}` sets flags from the ${native_width}-bit register substituted by structured `intel` assembly; use native-width operands, or a `raw intel` block with explicit operand modifiers',
					base)
				return
			}
		}
	}
	if flags_are_observed
		&& name in ['add', 'adc', 'adcx', 'adox', 'and', 'andn', 'blsi', 'blsmsk', 'blsr', 'cmp',
			'cmpxchg', 'dec', 'imul', 'inc', 'neg', 'or', 'sbb', 'sub', 'test', 'xadd', 'xor'] {
		for arg in instruction.args {
			alias := inline_asm_intel_arg_alias(arg, aliases) or { continue }
			typ := aliases[alias] or { continue }
			width := tc.inline_asm_intel_operand_width(typ) or { continue }
			if width != native_width {
				tc.record_inline_asm_intel_error(id, node, instruction,
					'named operand `${alias}` has ${width}-bit type `${typ.name()}`, but instruction `${instruction.name}` sets ${native_width}-bit flags that are observed later in this structured `intel` block; use native-width operands, or a `raw intel` block with explicit operand modifiers',
					base)
				return
			}
		}
	}
	if is_width_sensitive {
		if instruction.args.len > 0 {
			if alias := inline_asm_intel_arg_alias(instruction.args[0], aliases) {
				if typ := aliases[alias] {
					if width := tc.inline_asm_intel_operand_width(typ) {
						if width != native_width {
							tc.record_inline_asm_intel_error(id, node, instruction,
								'named destination `${alias}` has ${width}-bit type `${typ.name()}`, but instruction `${instruction.name}` operates on the ${native_width}-bit register substituted by structured `intel` assembly; use a native-width destination, or a `raw intel` block with an explicit operand modifier',
								base)
						}
					}
				}
			}
		}
		tc.check_inline_asm_intel_named_shift_count(id, node, instruction, aliases, 1, true,
			base)
		return
	}
	if name in ['shld', 'shrd']
		&& tc.check_inline_asm_intel_named_shift_count(id, node, instruction, aliases, 2, true,
			base) {
		return
	}
	if name == 'rorx'
		&& tc.check_inline_asm_intel_named_shift_count(id, node, instruction, aliases, 2, false,
			base) {
		return
	}
	for index, arg in instruction.args {
		if arg.is_address || arg.text.to_lower_ascii() !in registers {
			continue
		}
		register := arg.text.to_lower_ascii()
		width := inline_asm_intel_register_width(register)
		if width <= 0 || width == native_width {
			continue
		}
		if name == 'crc32' && index == 1
			&& (width == 8 || (native_width == 32 && width == 16)) {
			continue
		}
		if is_movq && native_width == 64
			&& (register.starts_with('mm') || register.starts_with('xmm')) {
			continue
		}
		if name in ['shld', 'shrd'] && index == 2 && register == 'cl' {
			continue
		}
		if name == 'mov' && register in ['cs', 'ss', 'ds', 'es', 'fs', 'gs']
			&& (index == 1 || (index == 0 && register != 'cs')) {
			continue
		}
		if name == 'mov' && native_width == 32
			&& (register.starts_with('cr') || register.starts_with('dr')) {
			continue
		}
		tc.record_inline_asm_intel_error(id, node, instruction,
			'hard register `${arg.text}` is ${width}-bit, but named operands in structured `intel` assembly expand to ${native_width}-bit registers for the current compilation target; use matching register widths, or a `raw intel` block with explicit operand modifiers',
			base)
	}
}

// check_inline_asm_clobbers reports clobber list entries that name no known register.
fn (mut tc TypeChecker) check_inline_asm_clobbers(id flat.NodeId, node flat.Node, block string, base int, section InlineAsmRange, registers []string) {
	for word in inline_asm_words(block, section) {
		if util.asm_clobber_is_special(word.text) || word.text in registers {
			continue
		}
		mut message := 'unknown clobbered register `${word.text}`'
		if suggestion := util.closest_asm_register(word.text, registers) {
			message += '; did you mean `${suggestion}`?'
		}
		tc.record_error_at(.unknown_ident, message, id, token.new_span(node.pos.id, base + word.start, base + word.end))
	}
}

// check_inline_asm_templates reports operand names that are close enough to a register
// name to be a typo. Anything else can legitimately be a symbol or a label.
fn (mut tc TypeChecker) check_inline_asm_templates(id flat.NodeId, node flat.Node, block string, base int, section InlineAsmRange, registers []string, aliases map[string]bool, arch string, is_intel bool) {
	labels := inline_asm_template_labels(block, section)
	for line in inline_asm_lines(block, section) {
		trimmed := block[line.start..line.end].trim_space()
		if trimmed.len == 0 {
			continue
		}
		instruction_start := inline_asm_skip_leading_label(block, line)
		if instruction_start >= line.end || (instruction_start == line.start
			&& trimmed.starts_with('.')) {
			continue
		}
		instruction_line := InlineAsmRange{
			start: instruction_start
			end:   line.end
		}
		// Only operands can name a register; the mnemonic itself never does.
		mut operand_start := inline_asm_skip_mnemonic(block, instruction_line)
		if block[instruction_start..operand_start].trim_space().to_lower_ascii() in inline_asm_instruction_prefixes {
			operand_start = inline_asm_skip_mnemonic(block, InlineAsmRange{
				start: operand_start
				end:   line.end
			})
		}
		for word in inline_asm_words(block, InlineAsmRange{ start: operand_start, end: line.end }) {
			register_word := if is_intel || arch in ['arm64', 'aarch64'] {
				word.text.to_lower_ascii()
			} else {
				word.text
			}
			if aliases[word.text] || word.text in labels || register_word in registers
				|| inline_asm_directional_numeric_label(register_word)
				|| inline_asm_operand_is_keyword(register_word, arch, is_intel) {
				continue
			}
			suggestion := util.closest_asm_register(register_word, registers) or { continue }
			tc.record_error_at(.unknown_ident, 'unknown register `${word.text}`; did you mean `${suggestion}`?', id, token.new_span(node.pos.id, base + word.start, base + word.end))
		}
	}
}

fn inline_asm_directional_numeric_label(word string) bool {
	return word.len > 1 && word[0] in [`b`, `f`] && word[1..].bytes().all(it.is_digit())
}

fn inline_asm_operand_is_keyword(word string, arch string, is_intel bool) bool {
	if is_intel && word in inline_asm_intel_operand_keywords {
		return true
	}
	if arch !in ['arm64', 'aarch64'] {
		return false
	}
	if word in inline_asm_arm64_operand_keywords {
		return true
	}
	if word in ['c0', 'c1', 'c2', 'c3', 'c4', 'c5', 'c6', 'c7', 'c8', 'c9', 'c10', 'c11', 'c12',
		'c13', 'c14', 'c15'] {
		return true
	}
	return word.starts_with('vl')
		&& word[2..] in ['1', '2', '3', '4', '5', '6', '7', '8', '16', '32', '64', '128', '256']
}

// inline_asm_mask_comments replaces every comment with spaces, keeping newlines and the
// length of source so that offsets into the result stay usable as source offsets.
fn inline_asm_mask_comments(source string) string {
	if !source.contains('/') {
		return source
	}
	mut out := []u8{cap: source.len}
	mut i := 0
	for i < source.len {
		c := source[i]
		if c in [`'`, `"`, `\``] {
			next := inline_asm_skip_quoted_text(source, i, source.len)
			out << source[i..next].bytes()
			i = next
			continue
		}
		comment_end := inline_asm_comment_end(source, i, source.len) or {
			out << c
			i++
			continue
		}
		for masked in i .. comment_end {
			out << if source[masked] == `\n` { `\n` } else { ` ` }
		}
		i = comment_end
	}
	return out.bytestr()
}

// inline_asm_section_ranges splits an assembly block body on the `;` separators that are
// not inside a string or character literal.
fn inline_asm_section_ranges(block string, start int, end int) []InlineAsmRange {
	mut ranges := []InlineAsmRange{}
	mut section_start := start
	mut i := start
	for i < end {
		c := block[i]
		if c in [`'`, `"`, `\``] {
			i = inline_asm_skip_quoted_text(block, i, end)
			continue
		}
		if c == `;` {
			ranges << InlineAsmRange{
				start: section_start
				end:   i
			}
			section_start = i + 1
		}
		i++
	}
	ranges << InlineAsmRange{
		start: section_start
		end:   end
	}
	return ranges
}

// inline_asm_lines splits a section into lines.
fn inline_asm_lines(block string, section InlineAsmRange) []InlineAsmRange {
	mut lines := []InlineAsmRange{}
	mut line_start := section.start
	mut i := section.start
	for i < section.end {
		c := block[i]
		if c in [`'`, `"`, `\``] {
			i = inline_asm_skip_quoted_text(block, i, section.end)
			continue
		}
		if c == `\n` {
			lines << InlineAsmRange{
				start: line_start
				end:   i
			}
			line_start = i + 1
		}
		i++
	}
	lines << InlineAsmRange{
		start: line_start
		end:   section.end
	}
	return lines
}

// inline_asm_skip_mnemonic returns the offset just past the first word of a line.
fn inline_asm_skip_mnemonic(block string, line InlineAsmRange) int {
	mut i := line.start
	for i < line.end && block[i].is_space() {
		i++
	}
	for i < line.end && !block[i].is_space() {
		i++
	}
	return i
}

// inline_asm_skip_leading_label returns the start of an instruction after an optional
// leading local label, or the original line start when there is no label.
fn inline_asm_skip_leading_label(block string, line InlineAsmRange) int {
	mut i := line.start
	for i < line.end && block[i].is_space() {
		i++
	}
	if i < line.end && block[i] == `.` {
		i++
	}
	if i >= line.end || !inline_asm_is_ident_start(block[i]) {
		return line.start
	}
	for i < line.end && inline_asm_is_ident_char(block[i]) {
		i++
	}
	if i >= line.end || block[i] != `:` {
		return line.start
	}
	i++
	for i < line.end && block[i].is_space() {
		i++
	}
	return i
}

// inline_asm_template_labels collects the leading `name:` labels a template section declares.
fn inline_asm_template_labels(block string, section InlineAsmRange) map[string]bool {
	mut labels := map[string]bool{}
	for line in inline_asm_lines(block, section) {
		trimmed := block[line.start..line.end].trim_space()
		colon := trimmed.index_u8(`:`)
		if colon < 0 {
			continue
		}
		name := trimmed[..colon].trim_space().trim_left('.')
		if inline_asm_is_ident(name) {
			labels[name] = true
		}
	}
	return labels
}

// inline_asm_words returns the identifiers in a range, skipping quoted text and anything
// that is part of a number.
fn inline_asm_words(block string, section InlineAsmRange) []InlineAsmWord {
	mut words := []InlineAsmWord{}
	mut i := section.start
	for i < section.end {
		c := block[i]
		if c in [`'`, `"`, `\``] {
			i = inline_asm_skip_quoted_text(block, i, section.end)
			continue
		}
		if c.is_digit() {
			// Skip the whole literal so `0x1f` never looks like the identifier `x1f`.
			for i < section.end && (block[i].is_alnum() || block[i] == `_`) {
				i++
			}
			continue
		}
		if !inline_asm_is_ident_start(c) {
			i++
			continue
		}
		start := i
		for i < section.end && inline_asm_is_ident_char(block[i]) {
			i++
		}
		words << InlineAsmWord{
			text:  block[start..i]
			start: start
			end:   i
		}
	}
	return words
}

// inline_asm_ios parses the `[alias] "constraint" (expression) as alias` operands of an
// input or output section.
fn inline_asm_ios(block string, section InlineAsmRange) []InlineAsmIOSpan {
	mut ios := []InlineAsmIOSpan{}
	mut i := section.start
	for i < section.end {
		i = inline_asm_skip_blanks(block, i, section.end)
		if i >= section.end {
			break
		}
		start := i
		mut alias := ''
		if block[i] == `[` {
			close := inline_asm_index_of(block, i + 1, section.end, `]`) or { break }
			alias = block[i + 1..close].trim_space()
			i = inline_asm_skip_blanks(block, close + 1, section.end)
			if i >= section.end {
				break
			}
		}
		mut constraint := ''
		if block[i] != `(` {
			constraint_start := i
			for i < section.end && !block[i].is_space() && block[i] != `(` {
				i++
			}
			constraint = block[constraint_start..i].trim('"\'')
			i = inline_asm_skip_blanks(block, i, section.end)
		}
		if i >= section.end || block[i] != `(` {
			break
		}
		expr_start := i + 1
		expr_end := inline_asm_matching_paren(block, i, section.end) or { break }
		expr := block[expr_start..expr_end].trim_space()
		i = inline_asm_skip_blanks(block, expr_end + 1, section.end)
		mut end := expr_end + 1
		if i + 2 <= section.end && block[i..i + 2] == 'as'
			&& (i + 2 == section.end || block[i + 2].is_space()) {
			i = inline_asm_skip_blanks(block, i + 2, section.end)
			alias_start := i
			for i < section.end && inline_asm_is_ident_char(block[i]) {
				i++
			}
			if alias.len == 0 {
				alias = block[alias_start..i]
			}
			end = i
		} else if alias.len == 0 && inline_asm_is_ident(expr) {
			alias = expr
		}
		ios << InlineAsmIOSpan{
			constraint: constraint
			alias:      alias
			expr:       expr
			start:      start
			end:        end
		}
	}
	return ios
}

fn inline_asm_skip_blanks(block string, start int, end int) int {
	mut i := start
	for i < end && block[i].is_space() {
		i++
	}
	return i
}

fn inline_asm_skip_quoted_text(source string, start int, end int) int {
	quote := source[start]
	mut i := start + 1
	for i < end {
		if source[i] == `\\` && i + 1 < end {
			i += 2
			continue
		}
		if source[i] == quote {
			return i + 1
		}
		i++
	}
	return end
}

// inline_asm_comment_end returns the offset just past a comment starting at start, or
// none when there is no comment there.
fn inline_asm_comment_end(source string, start int, end int) ?int {
	if start + 1 >= end || source[start] != `/` {
		return none
	}
	if source[start + 1] == `/` {
		mut i := start + 2
		for i < end && source[i] != `\n` {
			i++
		}
		return i
	}
	if source[start + 1] != `*` {
		return none
	}
	mut i := start + 2
	mut depth := 1
	for i < end && depth > 0 {
		if source[i] == `/` && i + 1 < end && source[i + 1] == `*` {
			depth++
			i += 2
			continue
		}
		if source[i] == `*` && i + 1 < end && source[i + 1] == `/` {
			depth--
			i += 2
			continue
		}
		i++
	}
	return i
}

fn inline_asm_matching_paren(block string, open int, end int) ?int {
	mut depth := 0
	mut i := open
	for i < end {
		c := block[i]
		if c in [`'`, `"`, `\``] {
			i = inline_asm_skip_quoted_text(block, i, end)
			continue
		}
		if c == `(` {
			depth++
		} else if c == `)` {
			depth--
			if depth == 0 {
				return i
			}
		}
		i++
	}
	return none
}

fn inline_asm_index_of(block string, start int, end int, needle u8) ?int {
	for i in start .. end {
		if block[i] == needle {
			return i
		}
	}
	return none
}

fn inline_asm_last_index_of(block string, start int, end int, needle u8) ?int {
	for i := end - 1; i >= start; i-- {
		if block[i] == needle {
			return i
		}
	}
	return none
}

fn inline_asm_is_ident_start(c u8) bool {
	return c == `_` || c.is_letter()
}

fn inline_asm_is_ident_char(c u8) bool {
	return inline_asm_is_ident_start(c) || c.is_digit()
}

fn inline_asm_is_ident(source string) bool {
	if source == '' || !inline_asm_is_ident_start(source[0]) {
		return false
	}
	return source[1..].bytes().all(inline_asm_is_ident_char(it))
}
