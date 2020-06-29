module builder

import v.table

pub fn (b &Builder) instantiate_generic_structs() {
	for idx, _ in b.table.types {
		mut typ := &b.table.types[idx]
		if typ.kind == .generic_struct_instance {
			info := typ.info as table.GenericStructInstance
			parent := b.table.types[info.parent_idx]
			mut parent_info := *(parent.info as table.Struct)
			mut fields := parent_info.fields.clone()
			for i, _ in parent_info.fields {
				mut field := fields[i]
				if field.typ.has_flag(.generic) {
					if parent_info.generic_types.len != info.generic_types.len {
						// TODO: proper error
						panic('generic template mismatch')
					}
					for j, gp in parent_info.generic_types {
						if gp == field.typ {
							field.typ = info.generic_types[j]
							break
						}
					}
				}
				fields[i] = field
			}
			parent_info.generic_types = []
			typ.is_public = true
			typ.kind = .struct_
			typ.info = {parent_info| fields: fields}
		}
	}
}