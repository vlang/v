module builder

import v.table

// NOTE: Think about generic struct implementation
// this might not be the best strategy. - joe-c
//
// generic struct instantiations to concrete types
pub fn (b &Builder) generic_struct_insts_to_concrete() {
	for idx, _ in b.table.types {
		mut typ := unsafe { &b.table.types[idx] }
		if typ.kind == .generic_struct_inst {
			info := typ.info as table.GenericStructInst
			parent := b.table.types[info.parent_idx]
			if parent.kind == .placeholder {
				typ.kind = .placeholder
				continue
			}
			mut parent_info := parent.info as table.Struct
			mut fields := parent_info.fields.clone()
			for i, _ in fields {
				mut field := fields[i]
				if field.typ.has_flag(.generic) {
					if parent_info.generic_types.len != info.generic_types.len {
						// TODO: proper error
						panic('generic template mismatch')
					}
					for j, gp in parent_info.generic_types {
						if gp == field.typ {
							field.typ = info.generic_types[j].derive(field.typ).clear_flag(.generic)
							break
						}
					}
				}
				fields[i] = field
			}
			parent_info.generic_types = []
			parent_info.fields = fields
			typ.is_public = true
			typ.kind = .struct_
			typ.info = parent_info
		}
	}
}
