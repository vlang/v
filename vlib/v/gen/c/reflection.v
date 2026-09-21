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
		g.writeln('\t${reflection_c_prefix}add_type_symbol((${reflection_c_prefix}TypeSymbol){.name = _S("${short_name}"), .mod = _S("${module_name}"), .idx = ${idx}, .parent_idx = 0, .language = 0, .kind = 28, .info = {0}, .methods = array_new(sizeof(${reflection_c_prefix}Function), 0, 0)});')
		g.writeln('\t${reflection_c_prefix}add_type((${reflection_c_prefix}Type){.name = _S("${short_name}"), .idx = ${idx}});')
	}
}
