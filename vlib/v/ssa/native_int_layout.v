module ssa

import v.types

// ssa_c_type_from_checker_type preserves C integer widths inside callback signatures.
fn (mut b Builder) ssa_c_type_from_checker_type(typ types.Type) TypeID {
	if typ is types.FnType {
		mut params := []TypeID{cap: typ.params.len}
		for param in typ.params {
			params << b.ssa_c_type_from_checker_type(param.typ)
		}
		return b.m.type_store.register(Type{
			kind:     .func_t
			params:   params
			ret_type: b.ssa_c_type_from_checker_type(typ.return_type)
		})
	}
	if typ is types.Pointer {
		return b.m.type_store.get_ptr(b.ssa_c_type_from_checker_type(typ.base_type))
	}
	if typ is types.Alias { return b.ssa_c_type_from_checker_type(typ.base_type) }
	if typ is types.Primitive { return b.resolve_c_decl_type(primitive_type_name(typ)) }
	return b.ssa_type_from_checker_type(typ)
}

// block_load_native_int reads builtin integer fields using their declared storage width.
fn (mut b Builder) block_load_native_int(block BlockID, address ValueID) ValueID {
	return b.block_instr1(.load, block, b.deref_type(address), address)
}

// materialize_native_string_literals gives widened string literals ordinary aggregate storage.
// The ARM64 literal shortcut packs two i32 fields into one register. A native string
// instead contains two platform-width integers and follows the large-aggregate ABI.
fn (mut b Builder) materialize_native_string_literals() {
	for function_index, function in b.m.funcs {
		if function_index >= b.materialized_functions.len || !b.materialized_functions[function_index] {
			b.materialize_function_string_literals(function)
		}
	}
}

fn (mut b Builder) materialize_function_string_literals(function Function) {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	if function.blocks.len == 0 {
		return
	}
	entry := function.blocks[0]
	original_entry := b.m.blocks[entry].instrs.clone()
	mut replacements := map[ValueID]ValueID{}
	for block in function.blocks {
		original := if block == entry { original_entry } else { b.m.blocks[block].instrs }
		for instruction_id in original {
			value := b.m.values[instruction_id]
			if value.kind != .instruction { continue }
			instruction := b.m.instrs[value.index]
			mut operands := []ValueID{}
			mut changed := false
			for index, operand in instruction.operands {
				if !instruction.is_value_operand(index) || operand <= 0 { continue }
				literal := b.m.values[operand]
				if literal.kind != .string_literal || literal.typ != b.str_type { continue }
				mut replacement := replacements[operand] or { ValueID(0) }
				if replacement == 0 {
					data := b.m.add_value(.string_literal, ptr_i8, literal.name, 0)
					length := b.m.get_or_add_const(b.i64_type, literal.name.len.str())
					replacement = b.emit_make_string(entry, data, length, 1)
					replacements[operand] = replacement
				}
				if !changed { operands = instruction.operands.clone() }
				operands[index] = replacement
				changed = true
			}
			if changed {
				if b.m.track_uses {
					b.m.rewrite_instruction(instruction_id, instruction.op, operands)
				} else {
					b.m.instrs[value.index].operands = operands
				}
			}
		}
	}
	if replacements.len > 0 {
		initializers := b.m.blocks[entry].instrs[original_entry.len..].clone()
		b.m.blocks[entry].instrs = initializers
		b.m.blocks[entry].instrs << original_entry
	}
}

// resolve_c_struct_storage_type keeps C integer array members inline and fixed-width.
// Member types and array lengths resolve in `module_name`, the module that
// declares the C struct, so `[max_iconimages]ImageDesc` in `sokol.sapp` uses
// that module's constant and type.
fn (mut b Builder) resolve_c_struct_storage_type(name u32, module_name string) TypeID {
	previous_module := b.cur_module
	b.cur_module = module_name
	defer {
		b.cur_module = previous_module
	}
	if b.is_fixed_array_type_name(name) {
		element := b.resolve_c_struct_storage_type(b.fixed_array_elem_type_name(name), module_name)
		return b.m.type_store.get_array(element, b.fixed_array_len_text(name).int())
	}
	return b.resolve_c_decl_type(name)
}
