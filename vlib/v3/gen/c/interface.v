module c

import v3.types

fn (mut g FlatGen) emit_sum_type(name string) {
	variants := g.tc.sum_types[name]
	g.writeln('struct ${c_name(name)} {')
	g.writeln('\tint typ;')
	g.writeln('\tunion {')
	for v in variants {
		ct := g.tc.c_type(g.tc.parse_type(v))
		field := g.sum_field_name(v)
		if g.variant_references_sum(v, name) {
			g.writeln('\t\t${ct}* ${field};')
		} else {
			g.writeln('\t\t${ct} ${field};')
		}
	}
	g.writeln('\t};')
	g.writeln('};')
	g.writeln('')
}

fn (g &FlatGen) sum_type_contains_struct(sum_name string, struct_name string) bool {
	if sum_name in g.tc.sum_types {
		for v in g.tc.sum_types[sum_name] {
			if v == struct_name {
				return true
			}
		}
	}
	return false
}

fn (g &FlatGen) variant_references_sum(variant string, sum_name string) bool {
	mut visited := map[string]bool{}
	return g.variant_refs_sum_inner(variant, sum_name, mut visited)
}

fn (g &FlatGen) variant_refs_sum_inner(variant string, sum_name string, mut visited map[string]bool) bool {
	if variant == sum_name || variant.all_after_last('.') == sum_name.all_after_last('.') {
		return true
	}
	if variant in visited {
		return false
	}
	visited[variant] = true
	if variant in g.tc.structs {
		for f in g.tc.structs[variant] {
			if g.type_references_sum(f.typ, sum_name, mut visited) {
				return true
			}
		}
	}
	return false
}

fn (g &FlatGen) type_references_sum(typ types.Type, sum_name string, mut visited map[string]bool) bool {
	clean := types.unwrap_pointer(typ)
	if clean is types.Struct && clean.name == sum_name {
		return true
	}
	if clean is types.SumType && clean.name == sum_name {
		return true
	}
	if clean is types.SumType {
		return true
	}
	if clean is types.Struct {
		if g.variant_refs_sum_inner(clean.name, sum_name, mut visited) {
			return true
		}
	}
	if clean is types.Array {
		return g.type_references_sum(clean.elem_type, sum_name, mut visited)
	}
	return false
}

fn (g &FlatGen) resolve_variant(sum_name string, variant string) string {
	if sum_name in g.tc.sum_types {
		for v in g.tc.sum_types[sum_name] {
			if v == variant {
				return variant
			}
		}
		for v in g.tc.sum_types[sum_name] {
			if v.all_after_last('.') == variant {
				return v
			}
		}
	}
	return variant
}

fn (g &FlatGen) sum_field_name(variant string) string {
	if variant.starts_with('[]') {
		return '_Array_${c_name(variant[2..])}'
	}
	if variant.starts_with('map[') {
		return '_Map_${c_name(variant[4..].replace(']', '_'))}'
	}
	return match variant {
		'int' { '_int' }
		'i8' { '_i8' }
		'i16' { '_i16' }
		'i64' { '_i64' }
		'u8', 'byte' { '_u8' }
		'u16' { '_u16' }
		'u32' { '_u32' }
		'u64' { '_u64' }
		'f32' { '_f32' }
		'f64' { '_f64' }
		'bool' { '_bool' }
		'string' { '_string' }
		else { c_name(variant) }
	}
}

fn (mut g FlatGen) register_interface_strings() {
	for iface_name, methods in g.interfaces {
		cn := c_name(iface_name)
		for method in methods {
			g.intern_string('interface method ${cn}.${method} not implemented')
		}
	}
}

fn (mut g FlatGen) interface_method_stubs() {
	for iface_name, methods in g.interfaces {
		cn := c_name(iface_name)
		for method in methods {
			sid := g.intern_string('interface method ${cn}.${method} not implemented')
			mname := '${iface_name}.${method}'
			ret_type := if mname in g.tc.fn_ret_types {
				g.tc.fn_ret_types[mname] or { types.Type(types.int_) }
			} else {
				types.Type(types.int_)
			}
			ct := g.optional_type_name(ret_type)
			if ct == 'void' {
				g.writeln('void ${cn}__${method}() { v_panic(_str_${sid}); }')
			} else {
				g.writeln('${ct} ${cn}__${method}() { v_panic(_str_${sid}); return (${ct}){0}; }')
			}
		}
	}
	if g.interfaces.len > 0 {
		g.writeln('')
	}
}

fn (g &FlatGen) sum_type_index(sum_name string, variant string) int {
	if sum_name in g.tc.sum_types {
		for i, v in g.tc.sum_types[sum_name] {
			if v == variant {
				return i + 1
			}
		}
		for i, v in g.tc.sum_types[sum_name] {
			if v.all_after_last('.') == variant {
				return i + 1
			}
		}
	}
	return 0
}
