module ssa

import strings

// module_to_text renders a module in the versioned text format described in
// ir_spec.md. Text IDs are scoped to the text. Every value is emitted exactly
// once: instructions inline in their blocks, globals in the globals section,
// arguments in their function header, everything else in the values section.
pub fn module_to_text(m &Module) !string {
	mut sb := strings.new_builder(4096)
	sb.write_string('vir ${ir_version}\n')
	os_name := if m.target.os == '' { '-' } else { m.target.os }
	arch_name := if m.target.arch == '' { '-' } else { m.target.arch }
	endian_name := if m.target.endian_little { 'little' } else { 'big' }
	sb.write_string('target os ${os_name} arch ${arch_name} ptr ${m.target.ptr_size} endian ${endian_name}\n')
	for i, t in m.type_store.types {
		sb.write_string('type %t${i} = ${type_to_text(t)!}\n')
	}
	for i in 1 .. m.values.len {
		val := m.values[i]
		if val.kind in [.instruction, .global, .argument] {
			// Emitted with their instruction, global, or function header.
			continue
		}
		match val.kind {
			.constant {
				sb.write_string('const %v${val.id} %t${val.typ} ${quote_text(val.name)}\n')
			}
			.func_ref {
				sb.write_string('funcref %v${val.id} %t${val.typ} ${quote_text(val.name)} fn ${val.index}\n')
			}
			.string_literal {
				sb.write_string('strlit %v${val.id} %t${val.typ} ${quote_text(val.name)}\n')
			}
			.c_string_literal {
				sb.write_string('cstrlit %v${val.id} %t${val.typ} ${quote_text(val.name)}\n')
			}
			else {
				sb.write_string('value %v${val.id} ${value_kind_name(val.kind)} %t${val.typ} ${quote_text(val.name)} idx ${val.index}\n')
			}
		}
	}
	for i, g in m.globals {
		val_id := global_value_id(m, i)!
		mut line := 'global %v${val_id} ${quote_text(g.name)} : %t${g.typ} linkage ${linkage_name(g.linkage)}'
		if g.alignment != 0 {
			line += ' align ${g.alignment}'
		}
		if g.is_constant {
			line += ' constant'
		}
		if g.initial_value != 0 {
			line += ' init ${g.initial_value}'
		}
		if g.initial_data.len > 0 {
			line += ' data ${hex_encode(g.initial_data)}'
		}
		sb.write_string(line + '\n')
	}
	for f in m.funcs {
		mut line := 'func ${f.id} ${quote_text(f.name)} -> %t${f.typ} linkage ${linkage_name(f.linkage)} callconv ${call_conv_name(f.call_conv)}'
		if f.is_variadic {
			line += ' variadic ${f.variadic_start}'
		}
		if f.is_c_extern {
			line += ' c_extern'
		}
		if f.is_prototype {
			line += ' prototype'
		}
		line += ' params ('
		for pi, param_id in f.params {
			if pi > 0 {
				line += ', '
			}
			param := m.values[param_id]
			line += '%v${param.id} ${quote_text(param.name)} : %t${param.typ}'
		}
		line += ')'
		sb.write_string(line + '\n')
		for blk_id in f.blocks {
			blk := m.blocks[blk_id]
			sb.write_string('@b${blk.id} ${quote_text(blk.name)} :\n')
			for val_id in blk.instrs {
				if val_id <= 0 || val_id >= m.values.len {
					return error('ssa: block @b${blk.id} references out of range value ${val_id}')
				}
				val := m.values[val_id]
				if val.kind != .instruction {
					continue
				}
				if val.index < 0 || val.index >= m.instrs.len {
					return error('ssa: value %v${val_id} has invalid instruction index')
				}
				sb.write_string('  ${instruction_to_text(m.instrs[val.index], val_id)!}\n')
			}
		}
	}
	write_detached_instructions(mut sb, m)!
	write_meta_maps(mut sb, m)
	for i, spec in m.asm_specs {
		sb.write_string('  ${asm_spec_to_text(i + 1, spec)}\n')
	}
	return sb.str()
}

// write_detached_instructions emits instructions that are no longer attached
// to any block (typically dead code the optimizer removed from blocks but
// left in the values array). Without this section the text would silently
// drop values and the parser could not rebuild the exact values array.
fn write_detached_instructions(mut sb strings.Builder, m &Module) ! {
	mut attached := map[ValueID]bool{}
	for f in m.funcs {
		for blk_id in f.blocks {
			for val_id in m.blocks[blk_id].instrs {
				attached[val_id] = true
			}
		}
	}
	mut started := false
	for i in 1 .. m.values.len {
		val := m.values[i]
		if val.kind != .instruction || val.id in attached {
			continue
		}
		if !started {
			sb.write_string('detached\n')
			started = true
		}
		if val.index < 0 || val.index >= m.instrs.len {
			return error('ssa: detached value %v${val.id} has invalid instruction index')
		}
		sb.write_string('  ${instruction_to_text(m.instrs[val.index], val.id)!}\n')
	}
}

// global_value_id finds the value id of the global at the given globals index.
fn global_value_id(m &Module, global_idx int) !ValueID {
	for i in 1 .. m.values.len {
		val := m.values[i]
		if val.kind == .global && val.index == global_idx {
			return val.id
		}
	}
	return error('ssa: global ${global_idx} has no value')
}

// type_to_text renders one type in the text format.
fn type_to_text(t Type) !string {
	match t.kind {
		.void_t {
			return 'void'
		}
		.label_t {
			return 'label'
		}
		.metadata_t {
			return 'metadata'
		}
		.int_t {
			prefix := if t.is_unsigned { 'u' } else { 'i' }
			return '${prefix}${t.width}'
		}
		.float_t {
			return 'f${t.width}'
		}
		.ptr_t {
			return 'ptr %t${t.elem_type}'
		}
		.array_t {
			return '[${t.len} x %t${t.elem_type}]'
		}
		.struct_t {
			mut sb := strings.new_builder(64)
			if t.field_names.len == 0 {
				sb.write_string('tuple {')
				for i, field in t.fields {
					if i > 0 {
						sb.write_string(', ')
					}
					sb.write_string('%t${field}')
				}
				sb.write_string('}')
			} else {
				sb.write_string('struct {')
				for i, field in t.fields {
					if i > 0 {
						sb.write_string(', ')
					}
					if i < t.field_names.len {
						sb.write_string('${quote_text(t.field_names[i])} : ')
					}
					sb.write_string('%t${field}')
				}
				sb.write_string('}')
			}
			if t.is_union {
				sb.write_string(' union')
			}
			if t.is_packed {
				sb.write_string(' packed')
			}
			if t.is_c_struct {
				sb.write_string(' c_struct')
			}
			if t.alignment != 0 {
				sb.write_string(' align ${t.alignment}')
			}
			if t.size != 0 {
				sb.write_string(' csize ${t.size}')
			}
			if t.field_offsets.len > 0 {
				sb.write_string(' offsets [')
				for i, off in t.field_offsets {
					if i > 0 {
						sb.write_string(',')
					}
					sb.write_string('${off}')
				}
				sb.write_string(']')
			}
			return sb.str()
		}
		.func_t {
			mut sb := strings.new_builder(64)
			sb.write_string('func (')
			for i, param in t.params {
				if i > 0 {
					sb.write_string(', ')
				}
				sb.write_string('%t${param}')
			}
			sb.write_string(') -> %t${t.ret_type}')
			return sb.str()
		}
	}
}

// instruction_to_text renders one instruction with its defining value id.
fn instruction_to_text(instr Instruction, val_id ValueID) !string {
	mut sb := strings.new_builder(64)
	sb.write_string('%v${val_id} = ${opcode_name(instr.op)!} %t${instr.typ}')
	for oi, op_id in instr.operands {
		if instr.is_block_operand(oi) {
			sb.write_string(', @b${op_id}')
		} else {
			sb.write_string(', %v${op_id}')
		}
	}
	if instr.atomic_ord != .not_atomic {
		sb.write_string(' atomic ${atomic_ordering_name(instr.atomic_ord)}')
	}
	if instr.inline != .none_ {
		sb.write_string(' inline ${inline_hint_name(instr.inline)}')
	}
	if instr.asm_id != 0 {
		sb.write_string(' asm ${instr.asm_id}')
	}
	return sb.str()
}

// write_meta_maps emits the V-specific side tables; empty maps are skipped.
fn write_meta_maps(mut sb strings.Builder, m &Module) {
	if m.c_struct_names.len > 0 {
		keys := sorted_int_keys(m.c_struct_names)
		sb.write_string('meta c_struct_names {')
		for i, k in keys {
			if i > 0 {
				sb.write_string(',')
			}
			sb.write_string(' %t${k} : ${quote_text(m.c_struct_names[k])}')
		}
		sb.write_string(' }\n')
	}
	if m.c_typedef_structs.len > 0 {
		keys := sorted_int_keys(m.c_typedef_structs)
		sb.write_string('meta c_typedef_structs {')
		for i, k in keys {
			if i > 0 {
				sb.write_string(',')
			}
			sb.write_string(' %t${k}')
		}
		sb.write_string(' }\n')
	}
	write_string_map(mut sb, 'export_c_names', m.export_c_names)
	write_string_map(mut sb, 'c_extern_symbols', m.c_extern_symbols)
	write_string_map(mut sb, 'export_aliases', m.export_aliases)
	if m.noinline_fns.len > 0 {
		keys := sorted_string_keys(m.noinline_fns)
		sb.write_string('meta noinline_fns {')
		for i, k in keys {
			if i > 0 {
				sb.write_string(',')
			}
			sb.write_string(' ${quote_text(k)}')
		}
		sb.write_string(' }\n')
	}
}

// write_string_map emits one string-to-string side table when non-empty.
fn write_string_map(mut sb strings.Builder, name string, table map[string]string) {
	if table.len == 0 {
		return
	}
	keys := sorted_string_keys(table)
	sb.write_string('meta ${name} {')
	for i, k in keys {
		if i > 0 {
			sb.write_string(',')
		}
		sb.write_string(' ${quote_text(k)} : ${quote_text(table[k])}')
	}
	sb.write_string(' }\n')
}

// asm_spec_to_text renders one inline-asm spec with its 1-based id.
fn asm_spec_to_text(id int, spec InlineAsmSpec) string {
	mut sb := strings.new_builder(128)
	sb.write_string('asm_spec ${id} arch ${quote_text(spec.arch)}')
	if spec.is_volatile {
		sb.write_string(' volatile')
	}
	if spec.is_intel {
		sb.write_string(' intel')
	}
	if spec.is_raw {
		sb.write_string(' raw')
	}
	sb.write_string(' insts [')
	for i, inst in spec.insts {
		if i > 0 {
			sb.write_string(', ')
		}
		sb.write_string(quote_text(inst))
	}
	sb.write_string('] ios [')
	for i, io in spec.ios {
		if i > 0 {
			sb.write_string(', ')
		}
		sb.write_string('{${quote_text(io.alias)} ${quote_text(io.constraint)} ${io.child_index}}')
	}
	sb.write_string('] clobbered [')
	for i, cl in spec.clobbered {
		if i > 0 {
			sb.write_string(', ')
		}
		sb.write_string(quote_text(cl))
	}
	sb.write_string(']')
	return sb.str()
}

// opcode_name returns the text-form name of an opcode. The match is exhaustive
// on purpose: adding an opcode without a text name fails to compile here.
fn opcode_name(op OpCode) !string {
	return match op {
		.ret { 'ret' }
		.br { 'br' }
		.jmp { 'jmp' }
		.switch_ { 'switch' }
		.unreachable { 'unreachable' }
		.add { 'add' }
		.sub { 'sub' }
		.mul { 'mul' }
		.sdiv { 'sdiv' }
		.srem { 'srem' }
		.udiv { 'udiv' }
		.urem { 'urem' }
		.fadd { 'fadd' }
		.fsub { 'fsub' }
		.fmul { 'fmul' }
		.fdiv { 'fdiv' }
		.frem { 'frem' }
		.shl { 'shl' }
		.ashr { 'ashr' }
		.lshr { 'lshr' }
		.and_ { 'and' }
		.or_ { 'or' }
		.xor { 'xor' }
		.alloca { 'alloca' }
		.load { 'load' }
		.store { 'store' }
		.get_element_ptr { 'gep' }
		.heap_alloc { 'heap_alloc' }
		.fence { 'fence' }
		.atomic_load { 'atomic_load' }
		.atomic_store { 'atomic_store' }
		.atomic_xchg { 'atomic_xchg' }
		.atomic_add { 'atomic_add' }
		.atomic_sub { 'atomic_sub' }
		.cmpxchg { 'cmpxchg' }
		.lt { 'lt' }
		.gt { 'gt' }
		.le { 'le' }
		.ge { 'ge' }
		.ult { 'ult' }
		.ugt { 'ugt' }
		.ule { 'ule' }
		.uge { 'uge' }
		.eq { 'eq' }
		.ne { 'ne' }
		.call { 'call' }
		.call_indirect { 'call_indirect' }
		.call_sret { 'call_sret' }
		.neg { 'neg' }
		.trunc { 'trunc' }
		.sext { 'sext' }
		.zext { 'zext' }
		.fptoui { 'fptoui' }
		.fptosi { 'fptosi' }
		.uitofp { 'uitofp' }
		.sitofp { 'sitofp' }
		.bitcast { 'bitcast' }
		.phi { 'phi' }
		.select { 'select' }
		.inline_asm { 'inline_asm' }
		.assign { 'assign' }
		.inline_string_init { 'inline_string_init' }
		.go_call { 'go_call' }
		.spawn_call { 'spawn_call' }
		.extractvalue { 'extractvalue' }
		.insertvalue { 'insertvalue' }
		.struct_init { 'struct_init' }
	}
}

// opcode_from_name parses a text-form opcode name.
fn opcode_from_name(name string) !OpCode {
	return match name {
		'ret' { OpCode.ret }
		'br' { OpCode.br }
		'jmp' { OpCode.jmp }
		'switch' { OpCode.switch_ }
		'unreachable' { OpCode.unreachable }
		'add' { OpCode.add }
		'sub' { OpCode.sub }
		'mul' { OpCode.mul }
		'sdiv' { OpCode.sdiv }
		'srem' { OpCode.srem }
		'udiv' { OpCode.udiv }
		'urem' { OpCode.urem }
		'fadd' { OpCode.fadd }
		'fsub' { OpCode.fsub }
		'fmul' { OpCode.fmul }
		'fdiv' { OpCode.fdiv }
		'frem' { OpCode.frem }
		'shl' { OpCode.shl }
		'ashr' { OpCode.ashr }
		'lshr' { OpCode.lshr }
		'and' { OpCode.and_ }
		'or' { OpCode.or_ }
		'xor' { OpCode.xor }
		'alloca' { OpCode.alloca }
		'load' { OpCode.load }
		'store' { OpCode.store }
		'gep' { OpCode.get_element_ptr }
		'heap_alloc' { OpCode.heap_alloc }
		'fence' { OpCode.fence }
		'atomic_load' { OpCode.atomic_load }
		'atomic_store' { OpCode.atomic_store }
		'atomic_xchg' { OpCode.atomic_xchg }
		'atomic_add' { OpCode.atomic_add }
		'atomic_sub' { OpCode.atomic_sub }
		'cmpxchg' { OpCode.cmpxchg }
		'lt' { OpCode.lt }
		'gt' { OpCode.gt }
		'le' { OpCode.le }
		'ge' { OpCode.ge }
		'ult' { OpCode.ult }
		'ugt' { OpCode.ugt }
		'ule' { OpCode.ule }
		'uge' { OpCode.uge }
		'eq' { OpCode.eq }
		'ne' { OpCode.ne }
		'call' { OpCode.call }
		'call_indirect' { OpCode.call_indirect }
		'call_sret' { OpCode.call_sret }
		'neg' { OpCode.neg }
		'trunc' { OpCode.trunc }
		'sext' { OpCode.sext }
		'zext' { OpCode.zext }
		'fptoui' { OpCode.fptoui }
		'fptosi' { OpCode.fptosi }
		'uitofp' { OpCode.uitofp }
		'sitofp' { OpCode.sitofp }
		'bitcast' { OpCode.bitcast }
		'phi' { OpCode.phi }
		'select' { OpCode.select }
		'inline_asm' { OpCode.inline_asm }
		'assign' { OpCode.assign }
		'inline_string_init' { OpCode.inline_string_init }
		'go_call' { OpCode.go_call }
		'spawn_call' { OpCode.spawn_call }
		'extractvalue' { OpCode.extractvalue }
		'insertvalue' { OpCode.insertvalue }
		'struct_init' { OpCode.struct_init }
		else { return error('ssa: unknown opcode `${name}`') }
	}
}

// value_kind_name returns the text-form name of a value kind.
fn value_kind_name(kind ValueKind) string {
	return match kind {
		.unknown { 'unknown' }
		.constant { 'constant' }
		.argument { 'argument' }
		.global { 'global' }
		.instruction { 'instruction' }
		.phi_result { 'phi_result' }
		.basic_block { 'basic_block' }
		.string_literal { 'string_literal' }
		.c_string_literal { 'c_string_literal' }
		.func_ref { 'func_ref' }
	}
}

// value_kind_from_name parses a text-form value kind.
fn value_kind_from_name(name string) !ValueKind {
	return match name {
		'unknown' { ValueKind.unknown }
		'constant' { ValueKind.constant }
		'argument' { ValueKind.argument }
		'global' { ValueKind.global }
		'instruction' { ValueKind.instruction }
		'phi_result' { ValueKind.phi_result }
		'basic_block' { ValueKind.basic_block }
		'string_literal' { ValueKind.string_literal }
		'c_string_literal' { ValueKind.c_string_literal }
		'func_ref' { ValueKind.func_ref }
		else { return error('ssa: unknown value kind `${name}`') }
	}
}

// linkage_name returns the text-form name of a linkage.
fn linkage_name(l Linkage) string {
	return match l {
		.external { 'external' }
		.private { 'private' }
		.internal { 'internal' }
	}
}

// linkage_from_name parses a text-form linkage.
fn linkage_from_name(name string) !Linkage {
	return match name {
		'external' { Linkage.external }
		'private' { Linkage.private }
		'internal' { Linkage.internal }
		else { return error('ssa: unknown linkage `${name}`') }
	}
}

// call_conv_name returns the text-form name of a calling convention.
fn call_conv_name(c CallConv) string {
	return match c {
		.c_decl { 'c_decl' }
		.fast_call { 'fast_call' }
		.wasm_std { 'wasm_std' }
	}
}

// call_conv_from_name parses a text-form calling convention.
fn call_conv_from_name(name string) !CallConv {
	return match name {
		'c_decl' { CallConv.c_decl }
		'fast_call' { CallConv.fast_call }
		'wasm_std' { CallConv.wasm_std }
		else { return error('ssa: unknown callconv `${name}`') }
	}
}

// atomic_ordering_name returns the text-form name of an atomic ordering.
fn atomic_ordering_name(o AtomicOrdering) string {
	return match o {
		.not_atomic { 'not_atomic' }
		.unordered { 'unordered' }
		.monotonic { 'monotonic' }
		.acquire { 'acquire' }
		.release { 'release' }
		.acq_rel { 'acq_rel' }
		.seq_cst { 'seq_cst' }
	}
}

// atomic_ordering_from_name parses a text-form atomic ordering.
fn atomic_ordering_from_name(name string) !AtomicOrdering {
	return match name {
		'not_atomic' { AtomicOrdering.not_atomic }
		'unordered' { AtomicOrdering.unordered }
		'monotonic' { AtomicOrdering.monotonic }
		'acquire' { AtomicOrdering.acquire }
		'release' { AtomicOrdering.release }
		'acq_rel' { AtomicOrdering.acq_rel }
		'seq_cst' { AtomicOrdering.seq_cst }
		else { return error('ssa: unknown atomic ordering `${name}`') }
	}
}

// inline_hint_name returns the text-form name of an inline hint.
fn inline_hint_name(h InlineHint) string {
	return match h {
		.none_ { 'none' }
		.always { 'always' }
		.never { 'never' }
		.hint { 'hint' }
	}
}

// inline_hint_from_name parses a text-form inline hint.
fn inline_hint_from_name(name string) !InlineHint {
	return match name {
		'none' { InlineHint.none_ }
		'always' { InlineHint.always }
		'never' { InlineHint.never }
		'hint' { InlineHint.hint }
		else { return error('ssa: unknown inline hint `${name}`') }
	}
}

// quote_text quotes a string for the text format, escaping as needed.
fn quote_text(s string) string {
	mut sb := strings.new_builder(s.len + 2)
	sb.write_string('"')
	for b in s.bytes() {
		match b {
			`"` {
				sb.write_string('\\"')
			}
			`\\` {
				sb.write_string('\\\\')
			}
			`\n` {
				sb.write_string('\\n')
			}
			`\t` {
				sb.write_string('\\t')
			}
			`\r` {
				sb.write_string('\\r')
			}
			else {
				if b < 0x20 || b > 0x7e {
					sb.write_string('\\x')
					sb.write_string(hex_byte(b))
				} else {
					sb.write_string(b.ascii_str())
				}
			}
		}
	}
	sb.write_string('"')
	return sb.str()
}

// hex_byte renders one byte as two lowercase hex digits.
fn hex_byte(b u8) string {
	hex_digits := '0123456789abcdef'
	hi := int(b >> 4)
	lo := int(b & 0x0f)
	return hex_digits[hi..hi + 1] + hex_digits[lo..lo + 1]
}

// hex_encode renders bytes as lowercase hex.
fn hex_encode(data []u8) string {
	mut sb := strings.new_builder(data.len * 2)
	for b in data {
		sb.write_string(hex_byte(b))
	}
	return sb.str()
}

// hex_val parses one hex digit.
fn hex_val(c u8) !u8 {
	if c >= `0` && c <= `9` {
		return c - `0`
	}
	if c >= `a` && c <= `f` {
		return c - `a` + 10
	}
	if c >= `A` && c <= `F` {
		return c - `A` + 10
	}
	return error('ssa: bad hex digit')
}

// hex_decode parses a lowercase or uppercase hex string into bytes.
fn hex_decode(s string) ![]u8 {
	if s.len % 2 != 0 {
		return error('ssa: odd-length hex string')
	}
	mut out := []u8{cap: s.len / 2}
	mut i := 0
	for i < s.len {
		hi := hex_val(s[i])!
		lo := hex_val(s[i + 1])!
		out << (hi << 4) | lo
		i += 2
	}
	return out
}

// sorted_int_keys returns the sorted keys of an int-keyed table so that the
// printed text is deterministic across runs.
fn sorted_int_keys[T](table map[int]T) []int {
	mut keys := table.keys()
	for i in 1 .. keys.len {
		key := keys[i]
		mut j := i - 1
		for j >= 0 && keys[j] > key {
			keys[j + 1] = keys[j]
			j--
		}
		keys[j + 1] = key
	}
	return keys
}

// str_less compares two strings byte-wise.
fn str_less(a string, b string) bool {
	min_len := if a.len < b.len { a.len } else { b.len }
	for i in 0 .. min_len {
		if a[i] != b[i] {
			return a[i] < b[i]
		}
	}
	return a.len < b.len
}

// sorted_string_keys returns the sorted keys of a string-keyed table so that
// the printed text is deterministic across runs.
fn sorted_string_keys[T](table map[string]T) []string {
	mut keys := table.keys()
	for i in 1 .. keys.len {
		key := keys[i]
		mut j := i - 1
		for j >= 0 && str_less(key, keys[j]) {
			keys[j + 1] = keys[j]
			j--
		}
		keys[j + 1] = key
	}
	return keys
}

// TextValueDef is one parsed non-instruction value definition.
struct TextValueDef {
pub mut:
	kind  ValueKind
	typ   TypeID
	name  string
	index int
}

// TextGlobalDef is one parsed global definition.
struct TextGlobalDef {
pub mut:
	val_id   ValueID
	name     string
	typ      TypeID
	linkage  Linkage
	align    int
	is_const bool
	init     i64
	data     []u8
}

// TextParamDef is one parsed function parameter.
struct TextParamDef {
pub mut:
	val_id ValueID
	name   string
	typ    TypeID
}

// TextInstrDef is one parsed instruction.
struct TextInstrDef {
pub mut:
	val_id   ValueID
	op       OpCode
	typ      TypeID
	operands []ValueID
	atomic   AtomicOrdering
	inline   InlineHint
	asm_id   int
}

// TextBlockDef is one parsed basic block with its instructions.
struct TextBlockDef {
pub mut:
	id     BlockID
	name   string
	instrs []TextInstrDef
}

// TextFuncDef is one parsed function with its blocks.
struct TextFuncDef {
pub mut:
	id           int
	name         string
	ret          TypeID
	linkage      Linkage
	callconv     CallConv
	variadic     int
	has_variadic bool
	c_extern     bool
	prototype    bool
	params       []TextParamDef
	blocks       []TextBlockDef
}

// TextModule is the buffered parse result, materialized by ID afterwards so
// that value, block, and instruction arrays match the printed IDs exactly.
struct TextModule {
pub mut:
	target_os         string
	target_arch       string
	target_ptr        int  = 8
	target_le         bool = true
	types             []Type
	value_defs        map[int]TextValueDef
	globals           []TextGlobalDef
	funcs             []TextFuncDef
	c_struct_names    map[int]string
	c_typedef_structs map[int]bool
	export_c_names    map[string]string
	c_extern_symbols  map[string]string
	export_aliases    map[string]string
	noinline_fns      map[string]bool
	asm_specs         []InlineAsmSpec
	max_value_id      int
	detached          []TextInstrDef
}

// module_from_text parses the text format back into a module. Use-lists,
// CFG edges, and caches are rebuilt; source positions stay empty. Types are
// recreated with the same interning calls in the same order, so type IDs,
// value IDs, and block IDs match the printed module exactly.
pub fn module_from_text(src string) !&Module {
	mut tm := TextModule{
		value_defs:        map[int]TextValueDef{}
		c_struct_names:    map[int]string{}
		c_typedef_structs: map[int]bool{}
		export_c_names:    map[string]string{}
		c_extern_symbols:  map[string]string{}
		export_aliases:    map[string]string{}
		noinline_fns:      map[string]bool{}
	}
	lines := src.split_into_lines()
	if lines.len == 0 {
		return error('ssa: empty text module')
	}
	head := tokenize_text_line(lines[0].trim_space())
	if head.len != 2 || head[0] != 'vir' {
		return error('ssa: expected `vir <version>` header')
	}
	version := parse_digits(head[1])!
	if version != ir_version {
		return error('ssa: unsupported text version ${version}, want ${ir_version}')
	}
	mut cur_func := -1
	mut in_detached := false
	for li in 1 .. lines.len {
		line_no := li + 1
		line := lines[li].trim_space()
		if line == '' || line.starts_with(';') {
			continue
		}
		toks := tokenize_text_line(line)
		if toks.len == 0 {
			continue
		}
		match toks[0] {
			'target' {
				parse_text_target(mut tm, toks)!
			}
			'type' {
				parse_text_type(mut tm, toks)!
			}
			'const', 'funcref', 'strlit', 'cstrlit', 'value' {
				parse_text_value(mut tm, toks)!
			}
			'global' {
				parse_text_global(mut tm, toks)!
			}
			'func' {
				tm.funcs << parse_text_func(mut tm, toks)!
				cur_func = tm.funcs.len - 1
			}
			'meta' {
				parse_text_meta(mut tm, toks)!
			}
			'asm_spec' {
				tm.asm_specs << parse_text_asm_spec(toks)!
			}
			'detached' {
				if toks.len != 1 {
					return error('ssa: bad detached marker')
				}
				in_detached = true
			}
			else {
				if toks[0].len > 0 && toks[0][0] == `@` {
					if cur_func < 0 {
						return error('ssa: line ${line_no}: block outside function')
					}
					in_detached = false
					mut f := tm.funcs[cur_func]
					f.blocks << parse_text_block(toks)!
					tm.funcs[cur_func] = f
				} else if toks[0].starts_with('%v') && toks.len > 2 && toks[1] == '=' {
					ins := parse_text_instr(mut tm, toks)!
					if in_detached {
						tm.detached << ins
					} else {
						if cur_func < 0 || tm.funcs[cur_func].blocks.len == 0 {
							return error('ssa: line ${line_no}: instruction outside block')
						}
						mut f := tm.funcs[cur_func]
						last := f.blocks.len - 1
						mut b := f.blocks[last]
						b.instrs << ins
						f.blocks[last] = b
						tm.funcs[cur_func] = f
					}
				} else {
					return error('ssa: line ${line_no}: cannot parse `${line}`')
				}
			}
		}
	}
	return materialize_text_module(tm)!
}

// tokenize_text_line splits a line into tokens. Quoted strings stay intact,
// `,` is a separator, and `: = ( ) { } [ ]` plus `->` are own tokens.
fn tokenize_text_line(s string) []string {
	mut toks := []string{}
	mut i := 0
	for i < s.len {
		c := s[i]
		if c == ` ` || c == `\t` || c == `\r` || c == `,` {
			i++
			continue
		}
		if c == `"` {
			mut j := i + 1
			for j < s.len {
				if s[j] == `\\` {
					j += 2
					continue
				}
				if s[j] == `"` {
					break
				}
				j++
			}
			end := if j < s.len { j + 1 } else { j }
			toks << s[i..end]
			i = end
			continue
		}
		if c == `:` || c == `=` || c == `{` || c == `}` || c == `(` || c == `)`
			|| c == `[` || c == `]` {
			toks << s[i..i + 1]
			i++
			continue
		}
		if c == `-` && i + 1 < s.len && s[i + 1] == `>` {
			toks << '->'
			i += 2
			continue
		}
		mut j := i
		for j < s.len {
			d := s[j]
			if d == ` ` || d == `\t` || d == `\r` || d == `,` || d == `:` || d == `=`
				|| d == `{` || d == `}` || d == `(` || d == `)` || d == `[` || d == `]`
				|| d == `"` {
				break
			}
			j++
		}
		toks << s[i..j]
		i = j
	}
	return toks
}

// unquote_text parses a quoted text string with backslash escapes.
fn unquote_text(s string) !string {
	if s.len < 2 || s[0] != `"` || s[s.len - 1] != `"` {
		return error('ssa: expected quoted string, got `${s}`')
	}
	mut out := []u8{cap: s.len}
	mut i := 1
	for i < s.len - 1 {
		c := s[i]
		if c != `\\` {
			out << c
			i++
			continue
		}
		i++
		if i >= s.len - 1 {
			return error('ssa: dangling escape')
		}
		e := s[i]
		match e {
			`n` {
				out << `\n`
			}
			`t` {
				out << `\t`
			}
			`r` {
				out << `\r`
			}
			`\\` {
				out << `\\`
			}
			`"` {
				out << `"`
			}
			`x` {
				if i + 2 >= s.len - 1 {
					return error('ssa: truncated hex escape')
				}
				hi := hex_val(s[i + 1])!
				lo := hex_val(s[i + 2])!
				out << ((hi << 4) | lo)
				i += 2
			}
			else {
				return error('ssa: bad escape')
			}
		}
		i++
	}
	return out.bytestr()
}

// parse_digits parses a non-negative decimal integer strictly.
fn parse_digits(s string) !int {
	if s.len == 0 {
		return error('ssa: expected integer, got empty string')
	}
	mut res := 0
	for i in 0 .. s.len {
		if s[i] < `0` || s[i] > `9` {
			return error('ssa: bad integer `${s}`')
		}
		res = res * 10 + int(s[i] - `0`)
	}
	return res
}

// parse_i64 parses an optionally negative decimal integer strictly.
fn parse_i64(s string) !i64 {
	if s.len == 0 {
		return error('ssa: expected integer, got empty string')
	}
	mut neg := false
	mut start := 0
	if s[0] == `-` {
		neg = true
		start = 1
	}
	if start >= s.len {
		return error('ssa: bad integer `${s}`')
	}
	mut res := i64(0)
	for i in start .. s.len {
		if s[i] < `0` || s[i] > `9` {
			return error('ssa: bad integer `${s}`')
		}
		res = res * 10 + i64(s[i] - `0`)
	}
	return if neg { -res } else { res }
}

// parse_type_id parses a `%tN` token.
fn parse_type_id(s string) !TypeID {
	if s.len < 3 || s[0] != `%` || s[1] != `t` {
		return error('ssa: bad type id `${s}`')
	}
	return TypeID(parse_digits(s[2..])!)
}

// parse_value_id parses a `%vN` token.
fn parse_value_id(s string) !ValueID {
	if s.len < 3 || s[0] != `%` || s[1] != `v` {
		return error('ssa: bad value id `${s}`')
	}
	return ValueID(parse_digits(s[2..])!)
}

// parse_block_id parses a `@bN` token.
fn parse_block_id(s string) !BlockID {
	if s.len < 3 || s[0] != `@` || s[1] != `b` {
		return error('ssa: bad block id `${s}`')
	}
	return BlockID(parse_digits(s[2..])!)
}

// note_value_id tracks a value id for coverage checking and max sizing.
fn note_value_id(mut tm TextModule, id ValueID) ! {
	n := int(id)
	if n <= 0 {
		return error('ssa: bad value id %v${n}')
	}
	if n in tm.value_defs {
		return error('ssa: duplicate value %v${n}')
	}
	if n > tm.max_value_id {
		tm.max_value_id = n
	}
}

// parse_text_target parses the `target` line.
fn parse_text_target(mut tm TextModule, toks []string) ! {
	// target os <os> arch <arch> ptr <n> endian <little|big>
	if toks.len != 9 || toks[1] != 'os' || toks[3] != 'arch' || toks[5] != 'ptr'
		|| toks[7] != 'endian' {
		return error('ssa: bad target line')
	}
	tm.target_os = if toks[2] == '-' { '' } else { toks[2] }
	tm.target_arch = if toks[4] == '-' { '' } else { toks[4] }
	tm.target_ptr = parse_digits(toks[6])!
	if toks[8] == 'little' {
		tm.target_le = true
	} else if toks[8] == 'big' {
		tm.target_le = false
	} else {
		return error('ssa: bad endianness `${toks[8]}`')
	}
}

// parse_text_type parses a `type %tN = <spec>` line.
fn parse_text_type(mut tm TextModule, toks []string) ! {
	if toks.len < 4 || toks[2] != '=' {
		return error('ssa: bad type line')
	}
	id := parse_type_id(toks[1])!
	if int(id) != tm.types.len {
		return error('ssa: type out of order (%t${int(id)})')
	}
	mut p := TypeTokens{
		toks: toks[3..]
	}
	tm.types << p.parse_type()!
	if p.pos != p.toks.len {
		return error('ssa: trailing tokens in type line')
	}
}

// TypeTokens is a cursor over the tokens of one type specification.
struct TypeTokens {
	toks []string
mut:
	pos int
}

// next returns the next token of a type specification.
fn (mut p TypeTokens) next() !string {
	if p.pos >= p.toks.len {
		return error('ssa: truncated type')
	}
	tok := p.toks[p.pos]
	p.pos++
	return tok
}

// expect consumes one expected token of a type specification.
fn (mut p TypeTokens) expect(want string) ! {
	got := p.next()!
	if got != want {
		return error('ssa: expected `${want}`, got `${got}`')
	}
}

// parse_type parses one type specification.
fn (mut p TypeTokens) parse_type() !Type {
	first := p.next()!
	match first {
		'void' {
			return Type{ kind: .void_t }
		}
		'label' {
			return Type{ kind: .label_t }
		}
		'metadata' {
			return Type{ kind: .metadata_t }
		}
		'ptr' {
			elem := parse_type_id(p.next()!)!
			return Type{ kind: .ptr_t, elem_type: elem }
		}
		'[' {
			length := parse_digits(p.next()!)!
			p.expect('x')!
			elem := parse_type_id(p.next()!)!
			p.expect(']')!
			return Type{ kind: .array_t, elem_type: elem, len: length }
		}
		'tuple' {
			p.expect('{')!
			fields := p.parse_id_list()!
			p.expect('}')!
			return Type{ kind: .struct_t, fields: fields }
		}
		'struct' {
			p.expect('{')!
			mut fields := []TypeID{}
			mut names := []string{}
			for {
				nxt := p.peek()!
				if nxt == '}' {
					break
				}
				name := unquote_text(p.next()!)!
				p.expect(':')!
				fields << parse_type_id(p.next()!)!
				names << name
			}
			p.expect('}')!
			mut is_union := false
			mut is_packed := false
			mut is_c_struct := false
			mut struct_align := 0
			mut struct_csize := 0
			mut struct_offsets := []int{}
			for p.pos < p.toks.len {
				flag := p.next()!
				match flag {
					'union' {
						is_union = true
					}
					'packed' {
						is_packed = true
					}
					'c_struct' {
						is_c_struct = true
					}
					'align' {
						struct_align = parse_digits(p.next()!)!
					}
					'csize' {
						struct_csize = parse_digits(p.next()!)!
					}
					'offsets' {
						p.expect('[')!
						for {
							nxt := p.peek()!
							if nxt == ']' {
								break
							}
							struct_offsets << parse_digits(p.next()!)!
						}
						p.expect(']')!
					}
					else {
						return error('ssa: bad struct flag `${flag}`')
					}
				}
			}
			return Type{
				kind:          .struct_t
				fields:        fields
				field_names:   names
				is_union:      is_union
				is_packed:     is_packed
				is_c_struct:   is_c_struct
				alignment:     struct_align
				size:          struct_csize
				field_offsets: struct_offsets
			}
		}
		'func' {
			p.expect('(')!
			params := p.parse_id_list()!
			p.expect(')')!
			p.expect('->')!
			ret := parse_type_id(p.next()!)!
			return Type{ kind: .func_t, params: params, ret_type: ret }
		}
		else {
			if first.len > 1 {
				if first[0] == `i` {
					return Type{ kind: .int_t, width: parse_digits(first[1..])! }
				}
				if first[0] == `u` {
					return Type{ kind: .int_t, width: parse_digits(first[1..])!, is_unsigned: true }
				}
				if first[0] == `f` {
					return Type{ kind: .float_t, width: parse_digits(first[1..])! }
				}
			}
			return error('ssa: bad type `${first}`')
		}
	}
}

// peek returns the next token of a type specification without consuming it.
fn (mut p TypeTokens) peek() !string {
	if p.pos >= p.toks.len {
		return error('ssa: truncated type')
	}
	return p.toks[p.pos]
}

// parse_id_list parses a whitespace-separated `%tN` list up to a closer.
fn (mut p TypeTokens) parse_id_list() ![]TypeID {
	mut out := []TypeID{}
	for {
		nxt := p.peek()!
		if nxt == ')' || nxt == '}' || nxt == ']' {
			break
		}
		out << parse_type_id(p.next()!)!
	}
	return out
}

// parse_text_value parses one values-section line.
fn parse_text_value(mut tm TextModule, toks []string) ! {
	match toks[0] {
		'const' {
			if toks.len != 4 {
				return error('ssa: bad const line')
			}
			id := parse_value_id(toks[1])!
			note_value_id(mut tm, id)!
			tm.value_defs[int(id)] = TextValueDef{
				kind: .constant
				typ:  parse_type_id(toks[2])!
				name: unquote_text(toks[3])!
			}
		}
		'funcref' {
			if toks.len != 6 || toks[4] != 'fn' {
				return error('ssa: bad funcref line')
			}
			id := parse_value_id(toks[1])!
			note_value_id(mut tm, id)!
			tm.value_defs[int(id)] = TextValueDef{
				kind:  .func_ref
				typ:   parse_type_id(toks[2])!
				name:  unquote_text(toks[3])!
				index: parse_digits(toks[5])!
			}
		}
		'strlit', 'cstrlit' {
			if toks.len != 4 {
				return error('ssa: bad string literal line')
			}
			id := parse_value_id(toks[1])!
			note_value_id(mut tm, id)!
			kind := if toks[0] == 'strlit' {
				ValueKind.string_literal
			} else {
				ValueKind.c_string_literal
			}
			tm.value_defs[int(id)] = TextValueDef{
				kind: kind
				typ:  parse_type_id(toks[2])!
				name: unquote_text(toks[3])!
			}
		}
		'value' {
			if toks.len != 6 || toks[4] != 'idx' {
				return error('ssa: bad value line')
			}
			id := parse_value_id(toks[1])!
			note_value_id(mut tm, id)!
			tm.value_defs[int(id)] = TextValueDef{
				kind:  value_kind_from_name(toks[2])!
				typ:   parse_type_id(toks[3])!
				name:  unquote_text(toks[4])!
				index: parse_digits(toks[5])!
			}
		}
		else {
			return error('ssa: bad value line')
		}
	}
}

// parse_text_global parses one `global` line.
fn parse_text_global(mut tm TextModule, toks []string) ! {
	// global %vN "name" : %tE linkage <l> [align N] [constant] [init N] [data hex]
	if toks.len < 7 || toks[3] != ':' || toks[5] != 'linkage' {
		return error('ssa: bad global line')
	}
	id := parse_value_id(toks[1])!
	note_value_id(mut tm, id)!
	tm.value_defs[int(id)] = TextValueDef{
		kind: .global
		typ:  parse_type_id(toks[4])!
		name: unquote_text(toks[2])!
	}
	mut g := TextGlobalDef{
		val_id:  id
		name:    unquote_text(toks[2])!
		typ:     parse_type_id(toks[4])!
		linkage: linkage_from_name(toks[6])!
	}
	mut i := 7
	for i < toks.len {
		match toks[i] {
			'align' {
				i++
				if i >= toks.len {
					return error('ssa: bad global align')
				}
				g.align = parse_digits(toks[i])!
			}
			'constant' {
				g.is_const = true
			}
			'init' {
				i++
				if i >= toks.len {
					return error('ssa: bad global init')
				}
				g.init = parse_i64(toks[i])!
			}
			'data' {
				i++
				if i >= toks.len {
					return error('ssa: bad global data')
				}
				g.data = hex_decode(toks[i])!
			}
			else {
				return error('ssa: bad global attribute `${toks[i]}`')
			}
		}
		i++
	}
	tm.globals << g
}

// parse_text_func parses one `func` header line and registers its params.
fn parse_text_func(mut tm TextModule, toks []string) !TextFuncDef {
	// func <fid> "name" -> %tR linkage <l> callconv <c> [...] params (...)
	if toks.len < 10 || toks[3] != '->' {
		return error('ssa: bad func header')
	}
	mut f := TextFuncDef{
		id:       parse_digits(toks[1])!
		name:     unquote_text(toks[2])!
		ret:      parse_type_id(toks[4])!
		linkage:  linkage_from_name(toks[6])!
		callconv: call_conv_from_name(toks[8])!
	}
	mut i := 9
	for i < toks.len && toks[i] != 'params' {
		match toks[i] {
			'variadic' {
				i++
				if i >= toks.len {
					return error('ssa: bad variadic marker')
				}
				f.has_variadic = true
				f.variadic = parse_digits(toks[i])!
			}
			'c_extern' {
				f.c_extern = true
			}
			'prototype' {
				f.prototype = true
			}
			else {
				return error('ssa: bad func attribute `${toks[i]}`')
			}
		}
		i++
	}
	if i >= toks.len || toks[i] != 'params' {
		return error('ssa: func header without params')
	}
	i++
	if i >= toks.len || toks[i] != '(' {
		return error('ssa: bad func params')
	}
	i++
	for i < toks.len && toks[i] != ')' {
		// %vN "name" : %tT
		if i + 3 >= toks.len || toks[i + 2] != ':' {
			return error('ssa: bad func param')
		}
		param_id := parse_value_id(toks[i])!
		note_value_id(mut tm, param_id)!
		tm.value_defs[int(param_id)] = TextValueDef{
			kind:  .argument
			typ:   parse_type_id(toks[i + 3])!
			name:  unquote_text(toks[i + 1])!
			index: f.params.len
		}
		f.params << TextParamDef{
			val_id: param_id
			name:   unquote_text(toks[i + 1])!
			typ:    parse_type_id(toks[i + 3])!
		}
		i += 4
	}
	if i >= toks.len {
		return error('ssa: unterminated func params')
	}
	return f
}

// parse_text_block parses one `@bN "name" :` line.
fn parse_text_block(toks []string) !TextBlockDef {
	if toks.len != 3 || toks[2] != ':' {
		return error('ssa: bad block header')
	}
	return TextBlockDef{
		id:   parse_block_id(toks[0])!
		name: unquote_text(toks[1])!
	}
}

// parse_text_instr parses one `%vN = <op> %tT ...` line and registers it.
fn parse_text_instr(mut tm TextModule, toks []string) !TextInstrDef {
	if toks.len < 4 {
		return error('ssa: bad instruction line')
	}
	id := parse_value_id(toks[0])!
	note_value_id(mut tm, id)!
	tm.value_defs[int(id)] = TextValueDef{
		kind:  .instruction
		typ:   parse_type_id(toks[3])!
		index: -1
	}
	mut ins := TextInstrDef{
		val_id: id
		op:     opcode_from_name(toks[2])!
		typ:    parse_type_id(toks[3])!
		atomic: .not_atomic
		inline: .none_
	}
	mut i := 4
	for i < toks.len {
		tok := toks[i]
		if tok == 'atomic' {
			i++
			if i >= toks.len {
				return error('ssa: bad atomic attribute')
			}
			ins.atomic = atomic_ordering_from_name(toks[i])!
		} else if tok == 'inline' {
			i++
			if i >= toks.len {
				return error('ssa: bad inline attribute')
			}
			ins.inline = inline_hint_from_name(toks[i])!
		} else if tok == 'asm' {
			i++
			if i >= toks.len {
				return error('ssa: bad asm attribute')
			}
			ins.asm_id = parse_digits(toks[i])!
		} else if tok.len > 0 && tok[0] == `@` {
			ins.operands << ValueID(parse_block_id(tok)!)
		} else if tok.starts_with('%v') {
			ins.operands << parse_value_id(tok)!
		} else {
			return error('ssa: bad instruction operand `${tok}`')
		}
		i++
	}
	return ins
}

// parse_text_meta parses one `meta <table> { ... }` line.
fn parse_text_meta(mut tm TextModule, toks []string) ! {
	if toks.len < 4 || toks[2] != '{' || toks[toks.len - 1] != '}' {
		return error('ssa: bad meta line')
	}
	body := toks[3..toks.len - 1]
	match toks[1] {
		'c_struct_names' {
			mut i := 0
			for i < body.len {
				if i + 2 >= body.len || body[i + 1] != ':' {
					return error('ssa: bad c_struct_names entry')
				}
				tm.c_struct_names[int(parse_type_id(body[i])!)] = unquote_text(body[i + 2])!
				i += 3
			}
		}
		'c_typedef_structs' {
			for tok in body {
				tm.c_typedef_structs[int(parse_type_id(tok)!)] = true
			}
		}
		'export_c_names' {
			parse_text_string_map(mut tm.export_c_names, body)!
		}
		'c_extern_symbols' {
			parse_text_string_map(mut tm.c_extern_symbols, body)!
		}
		'export_aliases' {
			parse_text_string_map(mut tm.export_aliases, body)!
		}
		'noinline_fns' {
			for tok in body {
				tm.noinline_fns[unquote_text(tok)!] = true
			}
		}
		else {
			return error('ssa: unknown meta table `${toks[1]}`')
		}
	}
}

// parse_text_string_map parses `"k" : "v"` pairs into a string table.
fn parse_text_string_map(mut table map[string]string, body []string) ! {
	mut i := 0
	for i < body.len {
		if i + 2 >= body.len || body[i + 1] != ':' {
			return error('ssa: bad meta string entry')
		}
		table[unquote_text(body[i])!] = unquote_text(body[i + 2])!
		i += 3
	}
}

// parse_text_asm_spec parses one `asm_spec <id> arch ...` line.
fn parse_text_asm_spec(toks []string) !InlineAsmSpec {
	if toks.len < 5 {
		return error('ssa: bad asm_spec line')
	}
	_ = parse_digits(toks[1])!
	if toks[2] != 'arch' {
		return error('ssa: bad asm_spec header')
	}
	mut spec_arch := unquote_text(toks[3])!
	mut spec_volatile := false
	mut spec_intel := false
	mut spec_raw := false
	mut spec_insts := []string{}
	mut spec_ios := []InlineAsmIO{}
	mut spec_clobbered := []string{}
	mut i := 4
	for i < toks.len {
		match toks[i] {
			'volatile' {
				spec_volatile = true
			}
			'intel' {
				spec_intel = true
			}
			'raw' {
				spec_raw = true
			}
			'insts' {
				i++
				if i >= toks.len || toks[i] != '[' {
					return error('ssa: bad asm insts list')
				}
				i++
				for i < toks.len && toks[i] != ']' {
					spec_insts << unquote_text(toks[i])!
					i++
				}
				if i >= toks.len {
					return error('ssa: unterminated asm insts list')
				}
			}
			'ios' {
				i++
				if i >= toks.len || toks[i] != '[' {
					return error('ssa: bad asm ios list')
				}
				i++
				for i < toks.len && toks[i] != ']' {
					if toks[i] != '{' || i + 4 >= toks.len || toks[i + 4] != '}' {
						return error('ssa: bad asm io entry')
					}
					spec_ios << InlineAsmIO{
						alias:       unquote_text(toks[i + 1])!
						constraint:  unquote_text(toks[i + 2])!
						child_index: parse_digits(toks[i + 3])!
					}
					i += 5
				}
				if i >= toks.len {
					return error('ssa: unterminated asm ios list')
				}
			}
			'clobbered' {
				i++
				if i >= toks.len || toks[i] != '[' {
					return error('ssa: bad asm clobbered list')
				}
				i++
				for i < toks.len && toks[i] != ']' {
					spec_clobbered << unquote_text(toks[i])!
					i++
				}
				if i >= toks.len {
					return error('ssa: unterminated asm clobbered list')
				}
			}
			else {
				return error('ssa: bad asm_spec attribute `${toks[i]}`')
			}
		}
		i++
	}
	return InlineAsmSpec{
		arch:        spec_arch
		is_volatile: spec_volatile
		is_intel:    spec_intel
		is_raw:      spec_raw
		insts:       spec_insts
		ios:         spec_ios
		clobbered:   spec_clobbered
	}
}

// materialize_text_module builds a module from the buffered parse result.
// Arrays are filled in ID order so value, block, and type IDs match the text.
fn materialize_text_module(tm TextModule) !&Module {
	mut m := Module.new()
	m.target = TargetData{
		ptr_size:      tm.target_ptr
		endian_little: tm.target_le
		os:            tm.target_os
		arch:          tm.target_arch
	}
	for i, t in tm.types {
		if i == 0 {
			if t.kind != .void_t {
				return error('ssa: type %t0 must be void')
			}
			continue
		}
		id := materialize_text_type(mut m, t)!
		if int(id) != i {
			return error('ssa: type materialized out of order')
		}
	}
	for id in 1 .. tm.max_value_id + 1 {
		if id !in tm.value_defs {
			return error('ssa: value %v${id} has no definition')
		}
		def := tm.value_defs[id]
		new_id := m.add_value(def.kind, def.typ, def.name, def.index)
		if int(new_id) != id {
			return error('ssa: value materialized out of order')
		}
	}
	for g in tm.globals {
		if int(g.val_id) <= 0 || int(g.val_id) >= m.values.len {
			return error('ssa: global value out of range')
		}
		m.globals << GlobalVar{
			name:          g.name
			typ:           g.typ
			linkage:       g.linkage
			alignment:     g.align
			is_constant:   g.is_const
			initial_value: g.init
			initial_data:  g.data
		}
		mut val := m.values[g.val_id]
		if val.kind != .global {
			return error('ssa: global value kind mismatch')
		}
		val.index = m.globals.len - 1
		m.values[g.val_id] = val
	}
	for f in tm.funcs {
		if f.id != m.funcs.len {
			return error('ssa: functions out of order')
		}
		fid := m.new_function(f.name, f.ret)
		mut func := m.funcs[fid]
		func.linkage = f.linkage
		func.call_conv = f.callconv
		func.is_variadic = f.has_variadic
		func.variadic_start = f.variadic
		func.is_c_extern = f.c_extern
		m.funcs[fid] = func
		for p in f.params {
			if int(p.val_id) <= 0 || int(p.val_id) >= m.values.len {
				return error('ssa: param value out of range')
			}
			m.func_add_param(fid, p.val_id)
		}
	}
	for fi, f in tm.funcs {
		for b in f.blocks {
			bid := m.add_block(fi, b.name)
			if int(bid) != int(b.id) {
				return error('ssa: blocks out of order')
			}
		}
		if f.prototype {
			mut func := m.funcs[fi]
			func.is_prototype = true
			m.funcs[fi] = func
		}
	}
	for f in tm.funcs {
		for b in f.blocks {
			for ins in b.instrs {
				append_text_instr(mut m, ins, b.id)!
				mut blk := m.blocks[b.id]
				blk.instrs << ins.val_id
				m.blocks[b.id] = blk
			}
		}
	}
	for ins in tm.detached {
		// Detached instructions keep no block: the values array never compacts,
		// but blocks only reference live instructions.
		append_text_instr(mut m, ins, BlockID(-1))!
	}
	// Validate detached instructions the same way (blocks are never removed
	// from m.blocks, so their block operands stay in range).
	for ins in tm.detached {
		validate_text_instr(m, tm, ins)!
	}
	// Validate type and id references before rebuilding derived state.
	for id in 1 .. m.values.len {
		t := int(m.values[id].typ)
		if t < 0 || t >= m.type_store.types.len {
			return error('ssa: value %v${id} has invalid type')
		}
	}
	for f in tm.funcs {
		if int(f.ret) < 0 || int(f.ret) >= m.type_store.types.len {
			return error('ssa: function has invalid return type')
		}
		for b in f.blocks {
			if int(b.id) < 0 || int(b.id) >= m.blocks.len {
				return error('ssa: function has invalid block')
			}
			for ins in b.instrs {
				validate_text_instr(m, tm, ins)!
			}
		}
	}
	// Rebuild use-lists from operand roles, mirroring add_instr.
	for blk in m.blocks {
		for val_id in blk.instrs {
			if val_id <= 0 || val_id >= m.values.len {
				continue
			}
			val := m.values[val_id]
			if val.kind != .instruction || val.index < 0 || val.index >= m.instrs.len {
				continue
			}
			instr := m.instrs[val.index]
			for oi, op_id in instr.operands {
				if instr.is_value_operand(oi) && op_id > 0 && op_id < m.values.len
					&& m.values[op_id].has_use_list() {
					m.add_value_user(op_id, val_id)
				}
			}
		}
	}
	// Rebuild the constant cache and check for dangling instruction values.
	for id in 1 .. m.values.len {
		val := m.values[id]
		if val.kind == .instruction && val.index < 0 {
			return error('ssa: instruction %v${id} has no body')
		}
		if val.kind == .constant {
			key := '${val.typ}:${val.name}'
			if key !in m.const_cache {
				m.const_cache[key] = val.id
			}
		}
		if val.kind == .func_ref && val.index >= m.funcs.len {
			return error('ssa: funcref %v${id} references missing function')
		}
	}
	// Rebuild CFG edges from terminators.
	for blk in m.blocks {
		for val_id in blk.instrs {
			if val_id <= 0 || val_id >= m.values.len {
				continue
			}
			val := m.values[val_id]
			if val.kind != .instruction || val.index < 0 || val.index >= m.instrs.len {
				continue
			}
			instr := m.instrs[val.index]
			for oi, op_id in instr.operands {
				if instr.is_successor_operand(oi) {
					if op_id < 0 || op_id >= m.blocks.len {
						return error('ssa: instruction has invalid successor')
					}
					m.block_add_succ(blk.id, BlockID(op_id))
					m.block_add_pred(BlockID(op_id), blk.id)
				}
			}
		}
	}
	m.c_struct_names = tm.c_struct_names
	for k in m.c_struct_names.keys() {
		if k < 0 || k >= m.type_store.types.len {
			return error('ssa: c_struct_names references missing type')
		}
	}
	m.c_typedef_structs = tm.c_typedef_structs
	m.export_c_names = tm.export_c_names
	m.c_extern_symbols = tm.c_extern_symbols
	m.export_aliases = tm.export_aliases
	m.noinline_fns = tm.noinline_fns
	for i, spec in tm.asm_specs {
		if spec.insts.len == 0 {
			return error('ssa: asm_spec ${i + 1} has no instructions')
		}
		m.asm_specs << spec
	}
	return m
}

// append_text_instr appends one parsed instruction and links its value.
fn append_text_instr(mut m Module, ins TextInstrDef, block BlockID) ! {
	if int(ins.val_id) <= 0 || int(ins.val_id) >= m.values.len {
		return error('ssa: instruction value out of range')
	}
	idx := m.instrs.len
	m.instrs << Instruction{
		op:         ins.op
		operands:   ins.operands
		block:      block
		typ:        ins.typ
		atomic_ord: ins.atomic
		inline:     ins.inline
		asm_id:     ins.asm_id
	}
	mut val := m.values[ins.val_id]
	if val.kind != .instruction {
		return error('ssa: instruction value kind mismatch')
	}
	val.index = idx
	m.values[ins.val_id] = val
}

// validate_text_instr checks one parsed instruction's type and operand ranges.
fn validate_text_instr(m &Module, tm TextModule, ins TextInstrDef) ! {
	if int(ins.typ) < 0 || int(ins.typ) >= m.type_store.types.len {
		return error('ssa: instruction has invalid type')
	}
	tmp := Instruction{
		op:       ins.op
		operands: ins.operands
	}
	for oi, op_id in ins.operands {
		if tmp.is_value_operand(oi) && (int(op_id) <= 0 || int(op_id) > tm.max_value_id) {
			return error('ssa: instruction has invalid value operand')
		}
		if tmp.is_block_operand(oi) && (int(op_id) < 0 || int(op_id) >= m.blocks.len) {
			return error('ssa: instruction has invalid block operand')
		}
	}
}

// materialize_text_type recreates one type with the same interning call the
// builder used, so type IDs match the printed order exactly.
fn materialize_text_type(mut m Module, t Type) !TypeID {
	match t.kind {
		.int_t {
			if t.is_unsigned {
				return m.type_store.get_uint(t.width)
			}
			return m.type_store.get_int(t.width)
		}
		.float_t {
			return m.type_store.get_float(t.width)
		}
		.ptr_t {
			return m.type_store.get_ptr(t.elem_type)
		}
		.array_t {
			return m.type_store.get_array(t.elem_type, t.len)
		}
		.struct_t {
			if t.field_names.len == 0 {
				return m.type_store.get_tuple(t.fields)
			}
			return m.type_store.register(t)
		}
		else {
			return m.type_store.register(t)
		}
	}
}
