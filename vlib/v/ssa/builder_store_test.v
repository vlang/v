module ssa

import v.ssa.optimize

struct NumericStoreCase {
	from string
	to   string
	op   OpCode
}

fn test_numeric_store_conversions_survive_scalar_promotion() {
	cases := [
		NumericStoreCase{'int', 'f64', .sitofp},
		NumericStoreCase{'u32', 'f64', .uitofp},
		NumericStoreCase{'f32', 'f64', .bitcast},
		NumericStoreCase{'f64', 'f32', .bitcast},
		NumericStoreCase{'i8', 'i64', .sext},
		NumericStoreCase{'u8', 'i64', .zext},
		NumericStoreCase{'i64', 'u8', .trunc},
		NumericStoreCase{'i8', 'u8', .bitcast},
		NumericStoreCase{'f64', 'int', .fptosi},
		NumericStoreCase{'f64', 'u32', .fptoui},
	]
	for path in ['user', 'synthetic', 'field'] {
		for scenario in cases {
			mut m := Module.new()
			m.target = TargetData{ ptr_size: 4 }
			mut b := Builder{
				m:        m
				i64_type: m.type_store.get_int(64)
				i32_type: m.type_store.get_int(32)
				i8_type:  m.type_store.get_int(8)
				u64_type: m.type_store.get_uint(64)
				u32_type: m.type_store.get_uint(32)
				u8_type:  m.type_store.get_uint(8)
				f32_type: m.type_store.get_float(32)
				f64_type: m.type_store.get_float(64)
			}
			from := b.resolve_type(scenario.from)
			to := b.resolve_type(scenario.to)
			func := m.new_function('conversion', to)
			entry := m.add_block(func, 'entry')
			input := m.add_value(.argument, from, 'input', 0)
			m.func_add_param(func, input)
			b.cur_block = entry
			address := b.emit0(.alloca, m.type_store.get_ptr(to))
			store := if path == 'synthetic' {
				// Synthetic emitters must put conversions in their explicit block.
				b.cur_block = -1
				b.block_instr2(.store, entry, b.void_type, input, address)
			} else if path == 'field' {
				value := b.coerce_store_value(input, to)
				b.emit2(.store, b.void_type, value, address)
			} else {
				b.emit2(.store, b.void_type, input, address)
			}
			stored := m.instrs[m.values[store].index].operands[0]
			assert m.values[stored].typ == to, '${path}: ${scenario}'
			assert m.values[stored].kind == .instruction
			conversion := m.instrs[m.values[stored].index]
			assert conversion.op == scenario.op, '${path}: ${scenario}'
			assert conversion.block == entry
			b.cur_block = entry
			loaded := b.emit1(.load, to, address)
			b.emit1(.ret, b.void_type, loaded)
			optimize.optimize(mut m)
			mut returns := 0
			for block in m.funcs[func].blocks {
				for value in m.blocks[block].instrs {
					instruction := m.instrs[m.values[value].index]
					assert instruction.op !in [.alloca, .store, .load]
					if instruction.op == .ret {
						result := m.values[instruction.operands[0]]
						assert result.typ == to, '${path}: ${scenario}'
						assert m.instrs[result.index].op == scenario.op
						returns++
					}
				}
			}
			assert returns == 1
		}
	}
}
