module c

import v.flat

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

fn reflection_matching_bracket(text string, start int) int {
	if start < 0 || start >= text.len || text[start] != `[` {
		return -1
	}
	mut depth := 0
	for i in start .. text.len {
		if text[i] == `[` {
			depth++
		} else if text[i] == `]` {
			depth--
			if depth == 0 {
				return i
			}
		}
	}
	return -1
}

fn reflection_split_types(text string) []string {
	mut parts := []string{}
	mut start := 0
	mut bracket_depth := 0
	mut paren_depth := 0
	for i in 0 .. text.len {
		match text[i] {
			`[` { bracket_depth++ }
			`]` { bracket_depth-- }
			`(` { paren_depth++ }
			`)` { paren_depth-- }
			`,` {
				if bracket_depth == 0 && paren_depth == 0 {
					parts << text[start..i].trim_space()
					start = i + 1
				}
			}
			else {}
		}
	}
	if start < text.len {
		parts << text[start..].trim_space()
	}
	return parts
}

fn reflection_type_key(raw_type string, module_name string) string {
	mut typ := raw_type.trim_space()
	for typ.starts_with('?') || typ.starts_with('!') || typ.starts_with('&') {
		typ = typ[1..].trim_space()
	}
	for prefix in ['mut ', 'shared ', 'atomic ', '...'] {
		if typ.starts_with(prefix) {
			return prefix + reflection_type_key(typ[prefix.len..], module_name)
		}
	}
	if typ.starts_with('[]') {
		return '[]' + reflection_type_key(typ[2..], module_name)
	}
	if typ.starts_with('map[') {
		close := reflection_matching_bracket(typ, 3)
		if close > 3 && close + 1 < typ.len {
			key := reflection_type_key(typ[4..close], module_name)
			value := reflection_type_key(typ[close + 1..], module_name)
			return 'map[${key}]${value}'
		}
	}
	if typ.starts_with('(') && typ.ends_with(')') {
		parts := reflection_split_types(typ[1..typ.len - 1])
		if parts.len > 1 {
			return '(${parts.map(reflection_type_key(it, module_name)).join(', ')})'
		}
	}
	if typ.ends_with(']') {
		open := typ.index_u8(`[`)
		close := if open >= 0 { reflection_matching_bracket(typ, open) } else { -1 }
		if open > 0 && close == typ.len - 1 {
			base := reflection_type_key(typ[..open], module_name)
			args := reflection_split_types(typ[open + 1..close])
			return '${base}[${args.map(reflection_type_key(it, module_name)).join(', ')}]'
		}
	}
	if reflection_builtin_type_id(typ) != none || typ.contains('.') || module_name in ['', 'builtin'] {
		return typ
	}
	return '${module_name}.${typ}'
}

// reflection_type_id reproduces the stable ids used by V3 comptime typeof metadata.
fn reflection_type_id(type_name string, module_name string) int {
	if builtin_idx := reflection_builtin_type_id(type_name) {
		return builtin_idx
	}
	key := reflection_type_key(type_name, module_name)
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

fn reflection_builtin_type_kind(name string) int {
	return match name {
		'void' { 1 }
		'voidptr' { 2 }
		'byteptr' { 3 }
		'charptr' { 4 }
		'i8' { 5 }
		'i16' { 6 }
		'i32' { 7 }
		'i64' { 8 }
		'int' { 9 }
		'isize' { 10 }
		'u8', 'byte' { 11 }
		'u16' { 12 }
		'u32' { 13 }
		'u64' { 14 }
		'usize' { 15 }
		'f32' { 16 }
		'f64' { 17 }
		'char' { 18 }
		'rune' { 19 }
		'bool' { 20 }
		'none' { 21 }
		'string' { 22 }
		'float literal' { 36 }
		'int literal' { 37 }
		'thread' { 39 }
		else { 0 }
	}
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

fn reflection_string_array(values []string) string {
	if values.len == 0 {
		return reflection_empty_array('string')
	}
	items := values.map('_S("${reflection_c_string(it)}")')
	return 'new_array_from_c_array(${items.len}, ${items.len}, sizeof(string), (string[]){${items.join(', ')}})'
}

fn reflection_type_array(values []string, module_name string) string {
	if values.len == 0 {
		return reflection_empty_array('u32')
	}
	items := values.map(reflection_field_type_id(it, module_name).str())
	return 'new_array_from_c_array(${items.len}, ${items.len}, sizeof(u32), (u32[]){${items.join(', ')}})'
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
	return (reflection_type_id(typ, module_name) & 0xffff) | flags | (indirections * 0x10000)
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

fn (g &FlatGen) reflection_function_args(node flat.Node, is_method bool, module_name string) string {
	mut args := []string{}
	mut param_idx := 0
	for i in 0 .. node.children_count {
		param := g.a.child_node(&node, i)
		if param.kind != .param {
			continue
		}
		if is_method && param_idx == 0 {
			param_idx++
			continue
		}
		args << '(reflection__FunctionArg){.name = _S("${reflection_c_string(param.value)}"), .typ = ${reflection_field_type_id(param.typ, module_name)}, .is_mut = ${param.is_mut}}'
		param_idx++
	}
	if args.len == 0 {
		return reflection_empty_array('reflection__FunctionArg')
	}
	return 'new_array_from_c_array(${args.len}, ${args.len}, sizeof(reflection__FunctionArg), (reflection__FunctionArg[]){${args.join(', ')}})'
}

fn (g &FlatGen) reflection_function(node_id int, node flat.Node) string {
	file := g.a.source_files[node.pos.id] or { unsafe { nil } }
	file_name := if isnil(file) { '' } else { file.name }
	module_name := g.tc.file_modules[file_name] or { 'main' }
	is_method := node.value.contains('.')
	mut receiver_type := 0
	mut is_variadic := false
	mut param_idx := 0
	for i in 0 .. node.children_count {
		param := g.a.child_node(&node, i)
		if param.kind != .param {
			continue
		}
		if is_method && param_idx == 0 {
			receiver_type = reflection_field_type_id(param.typ, module_name)
		}
		if param.typ.starts_with('...') {
			is_variadic = true
		}
		param_idx++
	}
	pos := g.a.source_position(node.pos) or {
		return '(reflection__Function){.mod_name = _S("${reflection_c_string(module_name)}"), .name = _S("${reflection_c_string(node.value.all_after_last('.'))}"), .attrs = ${reflection_attributes_array(g.decl_attrs[node_id] or {
			[]string{}
		})}, .args = ${g.reflection_function_args(node, is_method, module_name)}, .file_idx = ${node.pos.id}, .is_variadic = ${is_variadic}, .return_typ = ${reflection_field_type_id(node.typ, module_name)}, .receiver_typ = ${receiver_type}, .is_pub = ${node.op == .arrow}}'
	}
	return '(reflection__Function){.mod_name = _S("${reflection_c_string(module_name)}"), .name = _S("${reflection_c_string(node.value.all_after_last('.'))}"), .attrs = ${reflection_attributes_array(g.decl_attrs[node_id] or {
		[]string{}
	})}, .args = ${g.reflection_function_args(node, is_method, module_name)}, .file_idx = ${node.pos.id}, .line_start = ${pos.line}, .line_end = ${pos.line}, .is_variadic = ${is_variadic}, .return_typ = ${reflection_field_type_id(node.typ, module_name)}, .receiver_typ = ${receiver_type}, .is_pub = ${node.op == .arrow}}'
}

fn (g &FlatGen) reflection_method_receiver_key(node flat.Node) string {
	if !node.value.contains('.') {
		return ''
	}
	for i in 0 .. node.children_count {
		param := g.a.child_node(&node, i)
		if param.kind != .param {
			continue
		}
		file := g.a.source_files[node.pos.id] or { return '' }
		module_name := g.tc.file_modules[file.name] or { 'main' }
		return reflection_type_key(param.typ, module_name)
	}
	return ''
}

fn (g &FlatGen) reflection_methods(type_key string) string {
	mut methods := []string{}
	mut seen := map[string]bool{}
	for node_id, node in g.a.nodes {
		if node.kind != .fn_decl || !node.pos.is_valid()
			|| g.reflection_method_receiver_key(node) != type_key {
			continue
		}
		key := '${node.value}\n${node.pos.id}\n${node.pos.offset}'
		if seen[key] {
			continue
		}
		seen[key] = true
		methods << g.reflection_function(node_id, node)
	}
	if methods.len == 0 {
		return reflection_empty_array('reflection__Function')
	}
	return 'new_array_from_c_array(${methods.len}, ${methods.len}, sizeof(reflection__Function), (reflection__Function[]){${methods.join(', ')}})'
}

fn reflection_none_info() string {
	value := '(reflection__None){.parent_idx = 0}'
	return '(reflection__TypeInfo){.typ = 9, ._pointer_variant_is_owned = 1, .reflection__None = (reflection__None*)memdup(&${value}, sizeof(reflection__None))}'
}

fn reflection_array_info(type_key string) string {
	mut elem := type_key
	mut dims := 0
	for elem.starts_with('[]') {
		dims++
		elem = elem[2..]
	}
	value := '(reflection__Array){.nr_dims = ${dims}, .elem_type = ${reflection_field_type_id(elem, '')}}'
	return '(reflection__TypeInfo){.typ = 2, ._pointer_variant_is_owned = 1, .reflection__Array = (reflection__Array*)memdup(&${value}, sizeof(reflection__Array))}'
}

fn reflection_map_info(type_key string) string {
	close := reflection_matching_bracket(type_key, 3)
	key := if close > 3 { type_key[4..close] } else { '' }
	value_type := if close > 0 && close + 1 < type_key.len { type_key[close + 1..] } else { '' }
	value := '(reflection__Map){.key_type = ${reflection_field_type_id(key, '')}, .value_type = ${reflection_field_type_id(value_type, '')}}'
	return '(reflection__TypeInfo){.typ = 7, ._pointer_variant_is_owned = 1, .reflection__Map = (reflection__Map*)memdup(&${value}, sizeof(reflection__Map))}'
}

fn reflection_multi_return_info(type_key string) string {
	values := reflection_split_types(type_key[1..type_key.len - 1])
	value := '(reflection__MultiReturn){.types = ${reflection_type_array(values, '')}}'
	return '(reflection__TypeInfo){.typ = 8, ._pointer_variant_is_owned = 1, .reflection__MultiReturn = (reflection__MultiReturn*)memdup(&${value}, sizeof(reflection__MultiReturn))}'
}

fn reflection_alias_info(target string, module_name string) string {
	value := '(reflection__Alias){.parent_idx = ${reflection_field_type_id(target, module_name)}, .language = 0}'
	return '(reflection__TypeInfo){.typ = 1, ._pointer_variant_is_owned = 1, .reflection__Alias = (reflection__Alias*)memdup(&${value}, sizeof(reflection__Alias))}'
}

fn reflection_enum_info(values []string, is_flag bool) string {
	value := '(reflection__Enum){.vals = ${reflection_string_array(values)}, .is_flag = ${is_flag}}'
	return '(reflection__TypeInfo){.typ = 4, ._pointer_variant_is_owned = 1, .reflection__Enum = (reflection__Enum*)memdup(&${value}, sizeof(reflection__Enum))}'
}

fn reflection_sum_type_info(variants []string, module_name string) string {
	value := '(reflection__SumType){.parent_idx = 0, .variants = ${reflection_type_array(variants, module_name)}}'
	return '(reflection__TypeInfo){.typ = 11, ._pointer_variant_is_owned = 1, .reflection__SumType = (reflection__SumType*)memdup(&${value}, sizeof(reflection__SumType))}'
}

fn (g &FlatGen) reflection_interface_methods(name string, module_name string) string {
	methods := g.tc.interface_abstract_methods[name] or { []string{} }
	if methods.len == 0 {
		return reflection_empty_array('reflection__Function')
	}
	mut items := []string{cap: methods.len}
	for method in methods {
		items << '(reflection__Function){.mod_name = _S("${reflection_c_string(module_name)}"), .name = _S("${reflection_c_string(method)}"), .attrs = ${reflection_empty_array('VAttribute')}, .args = ${reflection_empty_array('reflection__FunctionArg')}}'
	}
	return 'new_array_from_c_array(${items.len}, ${items.len}, sizeof(reflection__Function), (reflection__Function[]){${items.join(', ')}})'
}

fn (g &FlatGen) reflection_interface_info(name string, module_name string) string {
	short_name := reflection_short_type_name(name)
	value := '(reflection__Interface){.name = _S("${reflection_c_string(short_name)}"), .methods = ${g.reflection_interface_methods(name, module_name)}, .fields = ${reflection_empty_array('reflection__StructField')}, .is_generic = ${name in g.tc.interface_generic_params}}'
	return '(reflection__TypeInfo){.typ = 6, ._pointer_variant_is_owned = 1, .reflection__Interface = (reflection__Interface*)memdup(&${value}, sizeof(reflection__Interface))}'
}

fn (g &FlatGen) reflection_decl_module(name string, kind flat.NodeKind) string {
	if name.contains('.') {
		return name.all_before_last('.')
	}
	for node in g.a.nodes {
		if node.kind != kind || node.value != name || !node.pos.is_valid() {
			continue
		}
		file := g.a.source_files[node.pos.id] or { continue }
		return g.tc.file_modules[file.name] or { 'main' }
	}
	return 'main'
}

fn reflection_base_type(raw_type string) string {
	mut typ := raw_type.trim_space()
	for typ.starts_with('?') || typ.starts_with('!') || typ.starts_with('&') {
		typ = typ[1..].trim_space()
	}
	for prefix in ['mut ', 'shared ', 'atomic ', '...'] {
		if typ.starts_with(prefix) {
			return reflection_base_type(typ[prefix.len..])
		}
	}
	return typ
}

fn reflection_collect_composite_type(raw_type string, module_name string, mut collected map[string]bool) {
	typ := reflection_base_type(raw_type)
	key := reflection_type_key(typ, module_name)
	if key.starts_with('[]') || key.starts_with('map[')
		|| (key.starts_with('(') && key.ends_with(')')) {
		collected[key] = true
	}
	if key.starts_with('[]') {
		reflection_collect_composite_type(key[2..], '', mut collected)
	} else if key.starts_with('map[') {
		close := reflection_matching_bracket(key, 3)
		if close > 3 && close + 1 < key.len {
			reflection_collect_composite_type(key[4..close], '', mut collected)
			reflection_collect_composite_type(key[close + 1..], '', mut collected)
		}
	} else if key.starts_with('(') && key.ends_with(')') {
		for part in reflection_split_types(key[1..key.len - 1]) {
			reflection_collect_composite_type(part, '', mut collected)
		}
	}
}

fn (mut g FlatGen) gen_reflection_type(name string, module_name string, kind int, info string, methods string, mut seen_ids map[int]bool) {
	full_idx := reflection_type_id(name, module_name)
	mut indexes := [full_idx]
	short_idx := full_idx & 0xffff
	if short_idx != full_idx {
		indexes << short_idx
	}
	display_name := if name.starts_with('main.') {
		name[5..]
	} else if module_name !in ['', 'builtin'] && name.starts_with('${module_name}.') {
		name[module_name.len + 1..]
	} else {
		name
	}
	for idx in indexes {
		if idx == 0 || seen_ids[idx] {
			continue
		}
		seen_ids[idx] = true
		g.writeln('\t${reflection_c_prefix}add_type_symbol((${reflection_c_prefix}TypeSymbol){.name = _S("${reflection_c_string(display_name)}"), .mod = _S("${reflection_c_string(module_name)}"), .idx = ${idx}, .parent_idx = 0, .language = 0, .kind = ${kind}, .info = ${info}, .methods = ${methods}});')
		g.writeln('\t${reflection_c_prefix}add_type((${reflection_c_prefix}Type){.name = _S("${reflection_c_string(display_name)}"), .idx = ${idx}});')
	}
}

fn (mut g FlatGen) gen_reflection_functions() {
	mut file_ids := g.a.source_files.keys()
	file_ids.sort()
	for file_id in file_ids {
		file := g.a.source_files[file_id] or { continue }
		g.writeln('\t${reflection_c_prefix}add_string(_S("${reflection_c_string(file.name)}"), ${file_id});')
	}
	mut seen := map[string]bool{}
	for node_id, node in g.a.nodes {
		if node.kind != .fn_decl || !node.pos.is_valid() || node.value.len == 0
			|| node.value.contains('.') {
			continue
		}
		file := g.a.source_files[node.pos.id] or { continue }
		module_name := g.tc.file_modules[file.name] or { 'main' }
		key := '${module_name}\n${node.value}\n${node.pos.id}\n${node.pos.offset}'
		if seen[key] {
			continue
		}
		seen[key] = true
		g.writeln('\t${reflection_c_prefix}add_func(${g.reflection_function(node_id, node)});')
	}
}

// gen_reflection_data registers source modules and structs after g_reflection is initialized.
fn (mut g FlatGen) gen_reflection_data() {
	mut modules := map[string]bool{}
	for _, module_name in g.tc.file_modules {
		if module_name.len > 0 {
			modules[module_name] = true
		}
	}
	for _, module_name in g.modules {
		if module_name.len > 0 {
			modules[module_name] = true
		}
	}
	modules['v.reflection'] = true
	mut module_names := modules.keys()
	module_names.sort()
	for module_name in module_names {
		g.writeln('\t${reflection_c_prefix}add_module(_S("${reflection_c_string(module_name)}"));')
	}

	mut seen_ids := map[int]bool{}
	builtin_names := ['void', 'voidptr', 'byteptr', 'charptr', 'i8', 'i16', 'i32', 'int', 'i64',
		'isize', 'u8', 'u16', 'u32', 'u64', 'usize', 'f32', 'f64', 'char', 'bool', 'none', 'string',
		'rune', 'float literal', 'int literal', 'thread']
	for name in builtin_names {
		g.gen_reflection_type(name, 'builtin', reflection_builtin_type_kind(name), reflection_none_info(),
			g.reflection_methods(name), mut seen_ids)
	}

	mut names := g.struct_decl_infos.keys()
	names.sort()
	for name in names {
		info := g.struct_decl_infos[name]
		if reflection_builtin_type_id(info.full_name) != none {
			continue
		}
		key := reflection_type_key(info.full_name, info.module)
		g.gen_reflection_type(key, info.module, 28, g.reflection_struct_info(info),
			g.reflection_methods(key), mut seen_ids)
	}

	mut enum_names := g.tc.enum_names.keys()
	enum_names.sort()
	for name in enum_names {
		module_name := g.reflection_decl_module(name, .enum_decl)
		g.gen_reflection_type(name, module_name, 33, reflection_enum_info(g.tc.enum_fields[name] or {
			[]string{}
		}, name in g.tc.flag_enums), g.reflection_methods(reflection_type_key(name, module_name)),
			mut seen_ids)
	}

	mut alias_names := g.tc.type_aliases.keys()
	alias_names.sort()
	for name in alias_names {
		raw_module_name := g.tc.type_alias_modules[name] or {
			if name.contains('.') { name.all_before_last('.') } else { 'main' }
		}
		module_name := if raw_module_name.len > 0 { raw_module_name } else { 'main' }
		g.gen_reflection_type(name, module_name, 32, reflection_alias_info(g.tc.type_aliases[name],
			module_name), g.reflection_methods(reflection_type_key(name, module_name)), mut seen_ids)
	}

	mut sum_names := g.tc.sum_types.keys()
	sum_names.sort()
	for name in sum_names {
		module_name := g.reflection_decl_module(name, .type_decl)
		g.gen_reflection_type(name, module_name, 31, reflection_sum_type_info(g.tc.sum_types[name] or {
			[]string{}
		}, module_name), g.reflection_methods(reflection_type_key(name, module_name)), mut seen_ids)
	}

	mut interface_names := g.tc.interface_names.keys()
	interface_names.sort()
	for name in interface_names {
		module_name := g.reflection_decl_module(name, .interface_decl)
		methods := g.reflection_interface_methods(name, module_name)
		g.gen_reflection_type(name, module_name, 35, g.reflection_interface_info(name,
			module_name), methods, mut seen_ids)
	}

	mut composites := map[string]bool{}
	for _, info in g.struct_decl_infos {
		for i in 0 .. info.node.children_count {
			field := g.a.child_node(&info.node, i)
			if field.kind == .field_decl {
				reflection_collect_composite_type(field.typ, info.module, mut composites)
			}
		}
	}
	for node in g.a.nodes {
		if node.kind != .fn_decl || !node.pos.is_valid() {
			continue
		}
		file := g.a.source_files[node.pos.id] or { continue }
		module_name := g.tc.file_modules[file.name] or { 'main' }
		reflection_collect_composite_type(node.typ, module_name, mut composites)
		for i in 0 .. node.children_count {
			param := g.a.child_node(&node, i)
			if param.kind == .param {
				reflection_collect_composite_type(param.typ, module_name, mut composites)
			}
		}
	}
	for name, target in g.tc.type_aliases {
		module_name := g.tc.type_alias_modules[name] or { 'main' }
		reflection_collect_composite_type(target, module_name, mut composites)
	}
	for name, variants in g.tc.sum_types {
		module_name := g.reflection_decl_module(name, .type_decl)
		for variant in variants {
			reflection_collect_composite_type(variant, module_name, mut composites)
		}
	}
	mut composite_names := composites.keys()
	composite_names.sort()
	for name in composite_names {
		if name.starts_with('[]') {
			g.gen_reflection_type(name, '', 23, reflection_array_info(name), g.reflection_methods(name),
				mut seen_ids)
		} else if name.starts_with('map[') {
			g.gen_reflection_type(name, '', 25, reflection_map_info(name), g.reflection_methods(name),
				mut seen_ids)
		} else if name.starts_with('(') && name.ends_with(')') {
			g.gen_reflection_type(name, '', 30, reflection_multi_return_info(name),
				g.reflection_methods(name), mut seen_ids)
		}
	}
	g.gen_reflection_functions()
}
