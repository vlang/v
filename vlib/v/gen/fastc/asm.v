module fastc

import strings
import v.flat
import v.pref
import v.scanner
import v.token

struct FastcAsmOperand {
	constraint     string
	expression     string
	alias          string
	source_offset  int
	source_origins []FastcSourceOrigin
}

struct FastcAsmRenderedOperand {
	source        string
	typ           u32
	address_alias string
}

struct FastcAsmAlias {
	typ           u32
	constraint    string
	address_alias string
}

// parse_asm_statement keeps the line-sensitive assembly template separate from
// its V expressions, which still go through the ordinary expression lowering.
fn (mut g Parser) parse_asm_statement() !bool {
	g.next()
	mut is_volatile := false
	mut is_goto := false
	mut is_raw := false
	mut is_intel := false
	mut arch := ''
	for g.tok != .lcbr && g.tok != .eof {
		match g.token_source() {
			'volatile' { is_volatile = true }
			'goto' { is_goto = true }
			'raw' { is_raw = true }
			'intel' { is_intel = true }
			else {
				if arch != '' { return g.unsupported('assembly modifier `${g.token_source()}`') }
				arch = g.token_source()
			}
		}
		g.next()
	}
	if g.tok != .lcbr { return g.unsupported('unfinished assembly header') }
	body_start := g.s.pos + 1
	g.next()
	mut depth := 1
	for depth > 0 && g.tok != .eof {
		if g.tok == .lcbr { depth++ }
		if g.tok == .rcbr {
			depth--
			if depth == 0 { break }
		}
		g.next()
	}
	if depth != 0 { return g.unsupported('unfinished assembly block') }
	body := fastc_asm_strip_comments(FastcSourceFile{
		path:           g.path
		source:         g.s.src[body_start..g.s.pos]
		source_offset:  g.source_offset + body_start
		source_origins: g.source_origins
	})
	g.next()
	g.consume_statement_end()
	sections := fastc_asm_sections(body)
	if sections.len > 5 { return g.unsupported('assembly operand sections') }
	outputs := if sections.len > 1 {
		fastc_asm_operands(sections[1], true)!
	} else {
		[]FastcAsmOperand{}
	}
	inputs := if sections.len > 2 {
		fastc_asm_operands(sections[2], false)!
	} else {
		[]FastcAsmOperand{}
	}
	clobbers := if sections.len > 3 {
		sections[3].source.replace(',', ' ').fields()
	} else {
		[]string{}
	}
	labels := if sections.len > 4 {
		sections[4].source.replace(',', ' ').fields()
	} else {
		[]string{}
	}
	if labels.len > 0 && !is_goto { return g.unsupported('assembly labels without goto') }
	normalized_arch := pref.normalized_arch(arch)
	if normalized_arch !in ['amd64', 'arm64'] || normalized_arch != g.prefs.target.arch {
		if sections[0].source.trim_space() != '' || outputs.len > 0 || inputs.len > 0 || labels.len > 0 {
			return g.unsupported('assembly architecture `${arch}` for `${g.prefs.target.arch}`')
		}
	}
	if is_intel && normalized_arch != 'amd64' {
		return g.unsupported('Intel assembly on `${arch}`')
	}
	mut reserved_aliases := []string{}
	for operand in outputs { reserved_aliases << operand.alias }
	for operand in inputs { reserved_aliases << operand.alias }
	mut aliases := map[string]FastcAsmAlias{}
	mut operands := []string{}
	mut address_inputs := []string{}
	for operand in outputs {
		rendered := g.render_asm_operand_info(operand, is_intel && !is_raw, reserved_aliases)!
		if operand.alias != '' {
			if operand.alias in aliases {
				return g.unsupported('duplicate assembly operand `${operand.alias}`')
			}
			aliases[operand.alias] = FastcAsmAlias{
				typ:           rendered.typ
				constraint:    operand.constraint
				address_alias: rendered.address_alias
			}
		}
		operands << fastc_asm_rendered_operand_source(operand, rendered)
		if rendered.address_alias != '' {
			address_inputs << '[${rendered.address_alias}] "r" (${rendered.address_alias})'
		}
	}
	output_source := operands.join(', ')
	operands.clear()
	for operand in inputs {
		rendered := g.render_asm_operand_info(operand, is_intel && !is_raw, reserved_aliases)!
		if operand.alias != '' {
			if operand.alias in aliases {
				return g.unsupported('duplicate assembly operand `${operand.alias}`')
			}
			aliases[operand.alias] = FastcAsmAlias{
				typ:           rendered.typ
				constraint:    operand.constraint
				address_alias: rendered.address_alias
			}
		}
		operands << fastc_asm_rendered_operand_source(operand, rendered)
		if rendered.address_alias != '' {
			address_inputs << '[${rendered.address_alias}] "r" (${rendered.address_alias})'
		}
	}
	operands << address_inputs
	input_source := operands.join(', ')
	is_extended := sections.len > 1 || is_goto
	mut template := ''
	if is_raw {
		template = fastc_asm_raw_template(sections[0].source)!
		for label in labels {
			template = template.replace('%l[${label}]', '%l[${fastc_c_identifier(label)}]')
		}
	} else {
		mut lines := []string{}
		for source_line in sections[0].source.split_into_lines() {
			line := source_line.trim_space()
			if line == '' { continue }
			lines << fastc_asm_lower_template(line, normalized_arch, aliases, is_extended, is_intel, labels)
		}
		text := lines.join('\n\t') + if lines.len > 0 { '\n\t' } else { '' }
		template = '"${fastc_asm_c_escape(text)}"'
	}
	if is_intel {
		template = '".intel_syntax noprefix\\n\\t" ${template} ".att_syntax prefix\\n\\t"'
	}
	mut suffix := ''
	if is_extended { suffix += ' : ${output_source} : ${input_source}' }
	if sections.len > 3 || is_goto {
		mut quoted := []string{}
		for clobber in clobbers { quoted << '"${fastc_asm_c_escape(fastc_asm_unquote(clobber))}"' }
		suffix += ' : ${quoted.join(', ')}'
	}
	if is_goto {
		mut named := []string{}
		for label in labels {
			if !fastc_asm_is_ident(label) { return g.unsupported('assembly goto label `${label}`') }
			named << fastc_c_identifier(label)
		}
		suffix += ' : ${named.join(', ')}'
	}
	g.write_line('__asm__${if is_goto { ' goto' } else { '' }}${if is_volatile {
		' volatile'
	} else {
		''
	}} (${template}${suffix});')
	return false
}

fn (mut g Parser) render_asm_operand_info(operand FastcAsmOperand, intel bool, reserved_aliases []string) !FastcAsmRenderedOperand {
	rendered := g.render_asm_operand(operand)!
	typ := g.underlying_alias_type(rendered.typ)
	if !intel || operand.alias == '' || !operand.constraint.contains('m') {
		return FastcAsmRenderedOperand{ source: rendered.source, typ: typ }
	}
	// GNU memory placeholders use the compiler's AT&T address syntax even inside
	// an Intel template. Keep the memory constraint and print its address through
	// an extra register input. Capture the lvalue once for both operands.
	mut address := g.temporary_name('asm_address')
	for (address in reserved_aliases) { address = g.temporary_name('asm_address') }
	g.write_line('__typeof__(&(${rendered.source})) ${address} = &(${rendered.source});')
	return FastcAsmRenderedOperand{
		source:        '(*${address})'
		typ:           typ
		address_alias: address
	}
}

fn (mut g Parser) render_asm_operand(operand FastcAsmOperand) !FastcAsmRenderedOperand {
	source := operand.expression
	previous_scanner := g.s
	previous_tok := g.tok
	previous_lit := g.lit
	previous_expected := g.expected_expression_type
	previous_source_offset := g.source_offset
	previous_source_origins := g.source_origins
	defer {
		g.s = previous_scanner
		g.tok = previous_tok
		g.lit = previous_lit
		g.expected_expression_type = previous_expected
		g.source_offset = previous_source_offset
		g.source_origins = previous_source_origins
	}
	mut scan := scanner.new_scanner(g.prefs, .normal)
	file := token.File.unindexed(g.path, source.len)
	scan.init(file, source)
	g.s = scan
	g.source_offset = operand.source_offset
	g.source_origins = operand.source_origins
	g.expected_expression_type = 0
	g.next()
	mut expression := g.read_expression([token.Token.semicolon, token.Token.eof])!
	g.skip_semicolons()
	if g.tok != .eof { return g.unsupported('assembly operand `${source}`') }
	mut typ := g.last_expression_type
	if fastc_asm_is_ident(source) {
		if local := g.locals[source] {
			if local.is_reference {
				expression = '(*(${expression}))'
				if flat.source_type_name(typ).ends_with('*') {
					typ = flat.source_type_id(flat.source_type_name(typ)[..flat.source_type_name(typ).len - 1])
				}
			}
		}
	}
	return FastcAsmRenderedOperand{ source: expression, typ: typ }
}

fn fastc_asm_operand_source(operand FastcAsmOperand, expression string) string {
	name := if operand.alias == '' { '' } else { '[${operand.alias}] ' }
	return '${name}"${fastc_asm_c_escape(operand.constraint)}" (${expression})'
}

fn fastc_asm_rendered_operand_source(operand FastcAsmOperand, rendered FastcAsmRenderedOperand) string {
	if rendered.address_alias == '' { return fastc_asm_operand_source(operand, rendered.source) }
	// Once a mixed rm operand is printed as memory, do not let the compiler
	// allocate a register and copy its unchanged value over the assembly result.
	mut modifiers := ''
	for c in operand.constraint {
		if c !in [`=`, `+`, `&`, `%`, `!`, `*`, `?`, `#`] { break }
		modifiers += c.ascii_str()
	}
	return fastc_asm_operand_source(FastcAsmOperand{
		constraint: modifiers + 'm'
		alias:      operand.alias
	}, rendered.source)
}

fn fastc_asm_strip_comments(source_file FastcSourceFile) FastcSourceFile {
	source := source_file.source
	mut edits := []FastcSourceEdit{}
	mut i := 0
	mut quote := u8(0)
	for i < source.len {
		c := source[i]
		if quote != 0 {
			if c == `\\` && i + 1 < source.len {
				i++
			} else if c == quote {
				quote = 0
			}
			i++
			continue
		}
		if c in [`'`, `"`, `\``] { quote = c }
		if c == `/` && i + 1 < source.len && source[i + 1] == `/` {
			start := i
			for i < source.len && source[i] != `\n` { i++ }
			edits << FastcSourceEdit{ start: start, end: i }
			continue
		}
		if c == `/` && i + 1 < source.len && source[i + 1] == `*` {
			start := i
			mut newlines := 0
			i += 2
			mut depth := 1
			for i < source.len && depth > 0 {
				if source[i] == `\n` { newlines++ }
				if i + 1 < source.len && source[i..i + 2] == '/*' {
					depth++
					i += 2
				} else if i + 1 < source.len && source[i..i + 2] == '*/' {
					depth--
					i += 2
				} else {
					i++
				}
			}
			edits << FastcSourceEdit{ start: start, end: i, replacement: ' ' + '\n'.repeat(newlines) }
			continue
		}
		i++
	}
	return fastc_apply_mapped_source_edits(source_file, edits)
}

fn fastc_asm_sections(source_file FastcSourceFile) []FastcSourceFile {
	source := source_file.source
	mut parts := []FastcSourceFile{}
	mut start := 0
	mut quote := u8(0)
	mut i := 0
	mut depth := 0
	for i < source.len {
		c := source[i]
		if quote != 0 {
			if c == `\\` {
				i += 2
				continue
			}
			if c == quote { quote = 0 }
		} else if c in [`'`, `"`, `\``] {
			quote = c
		} else if c in [`(`, `[`] {
			depth++
		} else if c in [`)`, `]`] {
			depth--
		} else if c == `;` && depth == 0 {
			parts << FastcSourceFile{
				...source_file
				source:        source[start..i]
				source_offset: source_file.source_offset + start
			}
			start = i + 1
		}
		i++
	}
	parts << FastcSourceFile{
		...source_file
		source:        source[start..]
		source_offset: source_file.source_offset + start
	}
	return parts
}

fn fastc_asm_operands(source_file FastcSourceFile, is_output bool) ![]FastcAsmOperand {
	source := source_file.source
	mut operands := []FastcAsmOperand{}
	mut i := 0
	for i < source.len {
		for i < source.len && (source[i].is_space() || source[i] == `,`) { i++ }
		if i == source.len { break }
		mut alias := ''
		if source[i] == `[` {
			end := source.index_after(']', i + 1) or { return error('fastc: unfinished named assembly operand') }
			alias = source[i + 1..end].trim_space()
			i = end + 1
			for i < source.len && source[i].is_space() { i++ }
		}
		constraint_start := i
		for i < source.len && source[i] != `(` && !source[i].is_space() { i++ }
		constraint := if constraint_start == i {
			if is_output { '+r' } else { 'r' }
		} else {
			fastc_asm_unquote(source[constraint_start..i])
		}
		for i < source.len && source[i].is_space() { i++ }
		if i >= source.len || source[i] != `(` {
			return error('fastc: expected assembly operand expression')
		}
		i++
		mut start := i
		mut depth := 1
		mut quote := u8(0)
		for i < source.len && depth > 0 {
			c := source[i]
			if quote != 0 {
				if c == `\\` {
					i += 2
					continue
				}
				if c == quote { quote = 0 }
			} else if c in [`'`, `"`, `\``] {
				quote = c
			} else if c == `(` {
				depth++
			} else if c == `)` {
				depth--
				if depth == 0 { break }
			}
			i++
		}
		if depth != 0 { return error('fastc: unfinished assembly operand expression') }
		for start < i && source[start].is_space() { start++ }
		expression := source[start..i].trim_space()
		i++
		for i < source.len && source[i].is_space() { i++ }
		if i + 2 <= source.len && source[i..i + 2] == 'as' && (i + 2 == source.len || source[i + 2].is_space()) {
			i += 2
			for i < source.len && source[i].is_space() { i++ }
			alias_start := i
			for i < source.len && fastc_asm_ident_char(source[i]) { i++ }
			alias = source[alias_start..i]
		} else if alias == '' && fastc_asm_is_ident(expression) {
			alias = expression
		}
		if alias != '' && !fastc_asm_is_ident(alias) {
			return error('fastc: invalid assembly operand alias `${alias}`')
		}
		operands << FastcAsmOperand{
			constraint:     constraint
			expression:     expression
			alias:          alias
			source_offset:  source_file.source_offset + start
			source_origins: source_file.source_origins
		}
	}
	return operands
}

fn fastc_asm_raw_template(source string) !string {
	mut templates := []string{}
	mut i := 0
	for i < source.len {
		for i < source.len && source[i].is_space() { i++ }
		if i == source.len { break }
		if source[i] != `"` { return error('fastc: raw assembly requires double-quoted templates') }
		start := i
		i++
		for i < source.len && source[i] != `"` {
			if source[i] == `\\` { i += 2 } else { i++ }
		}
		if i >= source.len { return error('fastc: unfinished raw assembly template') }
		i++
		templates << source[start..i]
	}
	return if templates.len > 0 { templates.join(' ') } else { '""' }
}

fn fastc_asm_lower_template(source string, arch string, aliases map[string]FastcAsmAlias, extended bool, intel bool, labels []string) string {
	if source.ends_with(':') { return source }
	mut start := 0
	// A label can precede an instruction on the same line.
	for start < source.len && !source[start].is_space() && source[start] != `:` { start++ }
	if start < source.len && source[start] == `:` {
		start++
		for start < source.len && source[start].is_space() { start++ }
	} else {
		start = 0
	}
	mut split := start
	for split < source.len && !source[split].is_space() { split++ }
	mut instruction := source[start..split]
	mut rest := source[split..].trim_space()
	if arch == 'amd64' && instruction in ['lock', 'rep', 'repe', 'repne', 'repz', 'repnz'] && rest != '' {
		mut end := 0
		for end < rest.len && !rest[end].is_space() { end++ }
		instruction += ' ' + rest[..end]
		rest = rest[end..].trim_space()
	}
	if rest == '' { return source[..start] + instruction }
	mut operands := fastc_asm_split_operands(rest)
	if arch == 'amd64' && !intel && !instruction.starts_with('.') { operands.reverse_in_place() }
	mut lowered := []string{}
	for operand in operands {
		mut value := fastc_asm_lower_operand(operand, arch, aliases, extended, intel, instruction.starts_with('.'))
		if operand in labels {
			value = '%l[${fastc_c_identifier(operand)}]'
		} else if arch == 'amd64' && !intel && (instruction.starts_with('call') || instruction.starts_with('j')) && (operand in aliases || fastc_asm_x86_register(operand)) {
			value = '*' + value
		}
		if arch == 'amd64' && instruction in ['in', 'inb', 'inw', 'inl', 'out', 'outb', 'outw',
			'outl'] {
			for alias, info in aliases {
				if !info.constraint.contains('m') && info.typ in [
					flat.source_type_int,
					flat.source_type_i64,
					flat.source_type_u64,
					flat.source_type_uint,
					flat.source_type_usize,
					flat.source_type_isize,
				] {
					value = value.replace('%[${alias}]', '%k[${alias}]')
				}
			}
		}
		lowered << value
	}
	return source[..start] + instruction + ' ' + lowered.join(', ')
}

fn fastc_asm_split_operands(source string) []string {
	mut parts := []string{}
	mut start := 0
	mut depth := 0
	mut quote := u8(0)
	for i, c in source {
		if quote != 0 {
			if c == quote { quote = 0 }
		} else if c in [`'`, `"`, `\``] {
			quote = c
		} else if c in [`[`, `(`] {
			depth++
		} else if c in [`]`, `)`] {
			depth--
		} else if c == `,` && depth == 0 {
			parts << source[start..i].trim_space()
			start = i + 1
		}
	}
	parts << source[start..].trim_space()
	return parts
}

fn fastc_asm_lower_operand(source string, arch string, aliases map[string]FastcAsmAlias, extended bool, intel bool, directive bool) string {
	if source.len > 1 && source[0] in [`b`, `f`] && source[1..].bytes().all(it.is_digit()) {
		return source[1..] + source[..1]
	}
	if source.len >= 2 && source[0] in [`'`, `"`] && source[source.len - 1] == source[0] {
		return source[1..source.len - 1]
	}
	if source.len >= 2 && source[0] == `\`` && source[source.len - 1] == `\`` {
		return "'${source[1..source.len - 1]}'"
	}
	if !intel && arch == 'amd64' {
		colon := source.index_u8(`:`)
		if colon > 0 && source[..colon] in ['cs', 'ds', 'es', 'fs', 'gs', 'ss'] {
			address := source[colon + 1..].trim_space()
			if address.starts_with('[') && address.ends_with(']') {
				return '${if extended { '%%' } else { '%' }}${source[..colon]}:${fastc_asm_address(address[1..address.len - 1], aliases, extended)}'
			}
		}
		if source.starts_with('[') && source.ends_with(']') {
			return fastc_asm_address(source[1..source.len - 1], aliases, extended)
		}
	}
	if !intel && fastc_asm_number(source) && !directive {
		return if arch == 'amd64' { '\$${source}' } else { '#${source}' }
	}
	return fastc_asm_atoms(source, arch, aliases, extended, intel)
}

fn fastc_asm_address(source string, aliases map[string]FastcAsmAlias, extended bool) string {
	mut base := ''
	mut index := ''
	mut scale := '1'
	mut displacement := ''
	for raw in source.split('+') {
		part := raw.trim_space()
		if part.contains('*') {
			pair := part.split('*')
			if pair.len == 2 {
				index = fastc_asm_atoms(pair[0].trim_space(), 'amd64', aliases, extended, false)
				scale = pair[1].trim_space()
			}
		} else if fastc_asm_number(part) {
			displacement = part
		} else if base == '' {
			base = fastc_asm_atoms(part, 'amd64', aliases, extended, false)
		} else if part in aliases || fastc_asm_x86_register(part) {
			index = fastc_asm_atoms(part, 'amd64', aliases, extended, false)
		} else {
			displacement = part
		}
	}
	if base == '' && index == '' { return displacement }
	return if index != '' {
		'${displacement}(${base},${index},${scale})'
	} else {
		'${displacement}(${base})'
	}
}

fn fastc_asm_atoms(source string, arch string, aliases map[string]FastcAsmAlias, extended bool, intel bool) string {
	mut out := strings.new_builder(source.len + 8)
	mut i := 0
	for i < source.len {
		c := source[i]
		if fastc_asm_ident_start(c) {
			start := i
			i++
			for i < source.len && fastc_asm_ident_char(source[i]) { i++ }
			word := source[start..i]
			if word in aliases {
				info := aliases[word]
				if intel && info.address_alias != '' {
					size := if source[..start].trim_space().ends_with('ptr') {
						''
					} else {
						fastc_asm_intel_memory_size(info.typ)
					}
					out.write_string('${size}[%V[${info.address_alias}]]')
					continue
				}
				modifier := if intel && fastc_asm_immediate_constraint(info.constraint) {
					'c'
				} else if intel {
					'V'
				} else if arch == 'arm64' && !info.constraint.contains('m') && info.typ in [
					flat.source_type_bool,
					flat.source_type_i8,
					flat.source_type_u8,
					flat.source_type_i16,
					flat.source_type_u16,
					flat.source_type_i32,
					flat.source_type_u32,
					flat.source_type_rune,
				] {
					'w'
				} else {
					''
				}
				out.write_string('%${modifier}[${word}]')
			} else if arch == 'amd64' && !intel && fastc_asm_x86_register(word) {
				out.write_string('${if extended { '%%' } else { '%' }}${word}')
			} else {
				out.write_string(word)
			}
			continue
		}
		if intel && extended && c in [`{`, `|`, `}`] { out.write_u8(`%`) }
		out.write_u8(c)
		i++
	}
	return out.str()
}

fn fastc_asm_immediate_constraint(constraint string) bool {
	return constraint.len > 0 && constraint.bytes().all(it in [`i`, `n`, `I`, `J`, `K`, `L`, `M`,
		`N`, `O`, `P`])
}

fn fastc_asm_intel_memory_size(typ u32) string {
	if fastc_is_pointer_type(typ) { return 'qword ptr ' }
	return match typ {
		flat.source_type_bool, flat.source_type_char, flat.source_type_i8, flat.source_type_u8 {
			'byte ptr '
		}
		flat.source_type_i16, flat.source_type_u16 { 'word ptr ' }
		flat.source_type_i32, flat.source_type_u32, flat.source_type_rune, flat.source_type_f32 {
			'dword ptr '
		}
		flat.source_type_int, flat.source_type_i64, flat.source_type_u64, flat.source_type_isize,
		flat.source_type_uint, flat.source_type_usize, flat.source_type_f64 {
			'qword ptr '
		}
		else { '' }
	}
}

fn fastc_asm_x86_register(name string) bool {
	if name in ['al', 'ah', 'ax', 'eax', 'rax', 'bl', 'bh', 'bx', 'ebx', 'rbx', 'cl', 'ch', 'cx',
		'ecx', 'rcx', 'dl', 'dh', 'dx', 'edx', 'rdx', 'sil', 'si', 'esi', 'rsi', 'dil', 'di', 'edi',
		'rdi', 'spl', 'sp', 'esp', 'rsp', 'bpl', 'bp', 'ebp', 'rbp', 'rip', 'eiz', 'riz', 'cs',
		'ss', 'ds', 'es', 'fs', 'gs', 'st'] {
		return true
	}
	for prefix in ['r', 'xmm', 'ymm', 'zmm', 'mm', 'st', 'cr', 'dr', 'k'] {
		if name.starts_with(prefix) && name.len > prefix.len {
			mut number := name[prefix.len..]
			if prefix == 'r' && number.len > 1 && number[number.len - 1] in [`b`, `w`, `d`] {
				number = number[..number.len - 1]
			}
			if number.bytes().all(it.is_digit()) { return true }
		}
	}
	return false
}

fn fastc_asm_c_escape(source string) string {
	return source.replace('\\', '\\\\').replace('"', '\\"').replace('\n', '\\n').replace('\r', '\\r').replace('\t', '\\t')
}

fn fastc_asm_unquote(source string) string {
	if source.len >= 2 && source[0] in [`'`, `"`] && source[source.len - 1] == source[0] {
		return source[1..source.len - 1]
	}
	return source
}

fn fastc_asm_number(source string) bool {
	if source == '' { return false }
	start := if source[0] in [`+`, `-`] { 1 } else { 0 }
	if start == source.len { return false }
	return if source[start..].starts_with('0x') {
		source.len > start + 2 && source[start + 2..].bytes().all(it.is_hex_digit())
	} else {
		source[start..].bytes().all(it.is_digit())
	}
}

fn fastc_asm_is_ident(source string) bool {
	return source.len > 0 && fastc_asm_ident_start(source[0]) && source[1..].bytes().all(fastc_asm_ident_char(it))
}

fn fastc_asm_ident_start(c u8) bool { return c == `_` || c.is_letter() }

fn fastc_asm_ident_char(c u8) bool { return fastc_asm_ident_start(c) || c.is_digit() }
