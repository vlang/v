module c

const reflection_c_prefix = 'reflection__'

fn (g &FlatGen) has_runtime_reflection() bool {
	for _, module_name in g.tc.file_modules {
		if module_name in ['v.reflection', 'reflection'] {
			return true
		}
	}
	for alias, module_name in g.modules {
		if alias == 'v.reflection' || module_name == 'v.reflection' {
			return true
		}
	}
	return false
}

// reflection_type_id reproduces the stable ids used by V3 comptime typeof metadata.
fn reflection_type_id(type_name string, module_name string) int {
	if builtin_idx := reflection_builtin_type_id(type_name) {
		return builtin_idx
	}
	key := if type_name.contains('.') || module_name in ['', 'builtin'] {
		type_name
	} else {
		'${module_name}.${type_name}'
	}
	mut hash := u64(1469598103934665603)
	for i in 0 .. key.len {
		hash = ((hash ^ u64(key[i])) * 1099511628211) % 2147418111
	}
	mut idx := (int(hash) + 65536) & ~(0xff << 16)
	if idx < 65536 {
		idx |= 1 << 24
	}
	return idx
}

fn reflection_builtin_type_id(name string) ?int {
	return match name {
		'void' { 1 }
		'voidptr' { 2 }
		'byteptr' { 3 }
		'charptr' { 4 }
		'i8' { 5 }
		'i16' { 6 }
		'i32' { 7 }
		'int' { 8 }
		'i64' { 9 }
		'isize' { 10 }
		'u8', 'byte' { 11 }
		'u16' { 12 }
		'u32' { 13 }
		'u64' { 14 }
		'usize' { 15 }
		'f32' { 16 }
		'f64' { 17 }
		'char' { 18 }
		'bool' { 19 }
		'none' { 20 }
		'string' { 21 }
		'rune' { 22 }
		'float literal' { 27 }
		'int literal' { 28 }
		'thread' { 29 }
		'nil' { 31 }
		else { none }
	}
}

fn reflection_c_string(value string) string {
	mut result := ''
	for ch in value {
		match ch {
			`\\` { result += '\\\\' }
			`"` { result += '\\"' }
			`\n` { result += '\\n' }
			`\r` { result += '\\r' }
			`\t` { result += '\\t' }
			else { result += ch.ascii_str() }
		}
	}
	return result
}

fn reflection_short_type_name(name string) string {
	return name.all_after_last('.')
}

struct ReflectionAttributeInfo {
	name    string
	has_arg bool
	arg     string
	kind    int
}

fn reflection_attribute_info(raw string) ReflectionAttributeInfo {
	clean := raw.trim_space()
	colon := clean.index_u8(`:`)
	if colon < 0 {
		return ReflectionAttributeInfo{
			name: clean.trim('\'"')
			kind: if clean.len >= 2 && clean[0] in [`'`, `\"`] { 1 } else { 0 }
		}
	}
	name := clean[..colon].trim_space()
	raw_arg := clean[colon + 1..].trim_space()
	is_string := (raw_arg.len >= 2 && raw_arg[0] in [`'`, `\"`]
		&& raw_arg[raw_arg.len - 1] == raw_arg[0])
		|| (raw_arg.len >= 3 && raw_arg[0] == `r` && raw_arg[1] in [`'`, `\"`]
			&& raw_arg[raw_arg.len - 1] == raw_arg[1])
	arg := if is_string {
		if raw_arg[0] == `r` { raw_arg[2..raw_arg.len - 1] } else { raw_arg[1..raw_arg.len - 1] }
	} else {
		raw_arg
	}
	return ReflectionAttributeInfo{
		name:    name
		has_arg: true
		arg:     arg
		kind:    if is_string {
			1
		} else if raw_arg == 'true' || raw_arg == 'false' {
			3
		} else if reflection_is_integer(raw_arg) {
			2
		} else {
			0
		}
	}
}

fn reflection_is_integer(raw string) bool {
	if raw.len == 0 {
		return false
	}
	start := if raw[0] in [`+`, `-`] { 1 } else { 0 }
	if start == raw.len {
		return false
	}
	for i in start .. raw.len {
		if !raw[i].is_digit() {
			return false
		}
	}
	return true
}

fn reflection_empty_array(c_type string) string {
	return 'array_new(sizeof(${c_type}), 0, 0)'
}

fn reflection_attributes_array(raw_attrs []string) string {
	if raw_attrs.len == 0 {
		return reflection_empty_array('VAttribute')
	}
	mut items := []string{cap: raw_attrs.len}
	for raw in raw_attrs {
		attr := reflection_attribute_info(raw)
		items << '(VAttribute){.name = _S("${reflection_c_string(attr.name)}"), .has_arg = ${attr.has_arg}, .arg = _S("${reflection_c_string(attr.arg)}"), .kind = ${attr.kind}}'
	}
	return 'new_array_from_c_array(${items.len}, ${items.len}, sizeof(VAttribute), (VAttribute[]){${items.join(', ')}})'
}

fn reflection_field_type_id(raw_type string, module_name string) int {
	mut typ := raw_type.trim_space()
	mut flags := 0
	if typ.starts_with('?') {
		flags |= 1 << 24
		typ = typ[1..].trim_space()
	} else if typ.starts_with('!') {
		flags |= 1 << 25
		typ = typ[1..].trim_space()
	}
	if typ.starts_with('shared ') {
		flags |= 1 << 28
		typ = typ[7..].trim_space()
	} else if typ.starts_with('atomic ') {
		flags |= 1 << 29
		typ = typ[7..].trim_space()
	}
	mut indirections := 0
	for typ.starts_with('&') {
		indirections++
		typ = typ[1..].trim_space()
	}
	return reflection_type_id(typ, module_name) | flags | (indirections << 16)
}

fn (g &FlatGen) reflection_struct_fields(info StructDeclInfo) string {
	mut fields := []string{}
	for i in 0 .. info.node.children_count {
		field := g.a.child_node(&info.node, i)
		if field.kind != .field_decl {
			continue
		}
		params := field.generic_params()
		flags := if params.len > 0 { params[0] } else { '' }
		attrs := if params.len > 1 { params[1..] } else { []string{} }
		fields << '(reflection__StructField){.name = _S("${reflection_c_string(field.value)}"), .typ = ${reflection_field_type_id(field.typ, info.module)}, .attrs = ${reflection_attributes_array(attrs)}, .is_pub = ${flags.contains('p')}, .is_mut = ${flags.contains('m')}}'
	}
	if fields.len == 0 {
		return reflection_empty_array('reflection__StructField')
	}
	return 'new_array_from_c_array(${fields.len}, ${fields.len}, sizeof(reflection__StructField), (reflection__StructField[]){${fields.join(', ')}})'
}

fn (g &FlatGen) reflection_struct_info(info StructDeclInfo) string {
	attrs := g.decl_attrs[info.node_id] or { []string{} }
	value := '(reflection__Struct){.parent_idx = 0, .attrs = ${reflection_attributes_array(attrs)}, .fields = ${g.reflection_struct_fields(info)}}'
	return '(reflection__TypeInfo){.typ = 10, ._pointer_variant_is_owned = 1, .reflection__Struct = (reflection__Struct*)memdup(&${value}, sizeof(reflection__Struct))}'
}

// gen_reflection_data registers source modules and structs after g_reflection is initialized.
fn (mut g FlatGen) gen_reflection_data() {
	mut modules := map[string]bool{}
	for _, module_name in g.tc.file_modules {
		if module_name.len > 0 {
			modules[module_name] = true
		}
	}
	mut module_names := modules.keys()
	module_names.sort()
	for module_name in module_names {
		g.writeln('\t${reflection_c_prefix}add_module(_S("${reflection_c_string(module_name)}"));')
	}

	mut names := g.struct_decl_infos.keys()
	names.sort()
	mut seen_ids := map[int]bool{}
	for name in names {
		info := g.struct_decl_infos[name]
		idx := reflection_type_id(info.full_name, info.module)
		if idx in seen_ids {
			continue
		}
		seen_ids[idx] = true
		short_name := reflection_c_string(reflection_short_type_name(info.full_name))
		module_name := reflection_c_string(info.module)
		g.writeln('\t${reflection_c_prefix}add_type_symbol((${reflection_c_prefix}TypeSymbol){.name = _S("${short_name}"), .mod = _S("${module_name}"), .idx = ${idx}, .parent_idx = 0, .language = 0, .kind = 28, .info = ${g.reflection_struct_info(info)}, .methods = array_new(sizeof(${reflection_c_prefix}Function), 0, 0)});')
		g.writeln('\t${reflection_c_prefix}add_type((${reflection_c_prefix}Type){.name = _S("${short_name}"), .idx = ${idx}});')
	}
}
