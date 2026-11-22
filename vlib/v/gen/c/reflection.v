module c

import v.flat
import v.types

const reflection_c_prefix = 'reflection__'

const cgen_reflection_float_literal_type = flat.source_type_id('float literal')
const cgen_reflection_int_literal_type = flat.source_type_id('int literal')

// reflection_source_type_key retains normalized reflection source spellings as identities.
fn reflection_source_type_key(raw_type_id u32, module_name string) u32 {
	typ := flat.source_type_syntax(raw_type_id).trimmed()
	shape := flat.source_type_shape(typ)
	syntax := flat.source_type_syntax(typ)
	if kind := reflection_wrapper_composition(shape) {
		return flat.source_type_composition_one(kind, reflection_source_type_key(syntax.raw_base(), module_name))
	}
	if shape.has(.map_type) && syntax.bracket_closed() && syntax.raw_base() != 0 {
		return flat.source_type_composition_two(.map,
			reflection_source_type_key(syntax.bracket_raw_inner(), module_name),
			reflection_source_type_key(syntax.raw_base(), module_name))
	}
	if syntax.tuple_like() && syntax.reflection_tuple_parts().len > 1 {
		return flat.source_type_composition(.multi_return,
			syntax.reflection_tuple_parts().map(reflection_source_type_key(it, module_name)))
	}
	if flat.source_type_query(typ).has(.bracket_suffix) && syntax.bracket_closed()
		&& syntax.bracket_suffix() == 0 && syntax.generic_base() != 0 {
		mut parts := [reflection_source_type_key(syntax.generic_base(), module_name)]
		for arg in flat.source_type_syntax(syntax.bracket_raw_inner()).reflection_parts() {
			parts << reflection_source_type_key(arg, module_name)
		}
		return flat.source_type_composition(.generic, parts)
	}
	if types.builtin_type_index(typ) > 0 || shape.has(.dotted)
		|| module_name in ['', 'builtin'] {
		return typ
	}
	return flat.source_type_composition_two(.qualified, flat.source_type_id(module_name), typ)
}

fn reflection_wrapper_composition(shape flat.SourceTypeShape) ?flat.SourceTypeComposition {
	if shape.has(.option_type) { return .option }
	if shape.has(.result_type) { return .result }
	if shape.has(.pointer) { return .pointer }
	if shape.has(.mutable_type) { return .mutable_parameter }
	if shape.has(.shared_type) { return .shared_type }
	if shape.has(.atomic_type) { return .atomic_type }
	if shape.has(.variadic) { return .variadic_parameter }
	if shape.has(.array_type) { return .array }
	return none
}

fn compare_reflection_source_type_ids(a &u32, b &u32) int {
	return compare_strings(flat.source_type_name(*a), flat.source_type_name(*b))
}

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

fn reflection_split_types(text string) []string {
	return flat.source_type_syntax(flat.source_type_id(text)).reflection_parts().map(flat.source_type_name(it))
}

fn reflection_type_key(raw_type string, module_name string) string {
	return flat.source_type_name(reflection_source_type_key(flat.source_type_id(raw_type), module_name))
}

fn (g &FlatGen) reflection_type_id(type_name_id u32, module_name string) int {
	return g.tc.type_index(reflection_source_type_key(type_name_id, module_name))
}

fn (g &FlatGen) reflection_type_id_bytes(type_name string, module_name string) int {
	return g.reflection_type_id(flat.source_type_id(type_name), module_name)
}

fn reflection_builtin_type_kind(name_id u32) int {
	return match name_id {
		flat.source_type_void { 1 }
		flat.source_type_voidptr { 2 }
		flat.source_type_byteptr { 3 }
		flat.source_type_charptr { 4 }
		flat.source_type_i8 { 5 }
		flat.source_type_i16 { 6 }
		flat.source_type_i32 { 7 }
		flat.source_type_i64 { 8 }
		flat.source_type_int { 9 }
		flat.source_type_isize { 10 }
		flat.source_type_u8 { 11 }
		flat.source_type_u16 { 12 }
		flat.source_type_u32 { 13 }
		flat.source_type_u64 { 14 }
		flat.source_type_uint { 15 }
		flat.source_type_f32 { 16 }
		flat.source_type_f64 { 17 }
		flat.source_type_char { 18 }
		flat.source_type_rune { 19 }
		flat.source_type_bool { 20 }
		flat.source_type_none { 21 }
		flat.source_type_string { 22 }
		cgen_reflection_float_literal_type { 36 }
		cgen_reflection_int_literal_type { 37 }
		cgen_thread_source_type { 39 }
		else { 0 }
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
		is_string := (clean.len >= 2 && clean[0] in [`'`, `\"`] && clean[clean.len - 1] == clean[0])
			|| (clean.len >= 3 && clean[0] == `r` && clean[1] in [`'`, `\"`]
				&& clean[clean.len - 1] == clean[1])
		return ReflectionAttributeInfo{
			name: decode_attribute_string(clean)
			kind: if is_string { 1 } else { 0 }
		}
	}
	name := clean[..colon].trim_space()
	raw_arg := clean[colon + 1..].trim_space()
	is_string := (raw_arg.len >= 2 && raw_arg[0] in [`'`, `\"`]
		&& raw_arg[raw_arg.len - 1] == raw_arg[0])
		|| (raw_arg.len >= 3 && raw_arg[0] == `r` && raw_arg[1] in [`'`, `\"`]
			&& raw_arg[raw_arg.len - 1] == raw_arg[1])
	arg := decode_attribute_string(raw_arg)
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

fn (g &FlatGen) reflection_type_array(values []u32, module_name string) string {
	if values.len == 0 {
		return reflection_empty_array('u64')
	}
	items := values.map(g.reflection_field_type_id(it, module_name).str())
	return 'new_array_from_c_array(${items.len}, ${items.len}, sizeof(u64), (u64[]){${items.join(', ')}})'
}

fn (g &FlatGen) reflection_type_array_bytes(values []string, module_name string) string {
	if values.len == 0 {
		return reflection_empty_array('u64')
	}
	items := values.map(g.reflection_field_type_id_bytes(it, module_name).str())
	return 'new_array_from_c_array(${items.len}, ${items.len}, sizeof(u64), (u64[]){${items.join(', ')}})'
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

fn (g &FlatGen) reflection_field_type_id(raw_type_id u32, module_name string) u64 {
	mut typ := flat.source_type_syntax(raw_type_id).trimmed()
	mut flags := u64(0)
	shape := flat.source_type_shape(typ)
	if shape.has(.option_type) {
		flags |= u64(1) << 32
		typ = flat.source_type_syntax(flat.source_type_syntax(typ).raw_base()).trimmed()
	} else if shape.has(.result_type) {
		flags |= u64(1) << 33
		typ = flat.source_type_syntax(flat.source_type_syntax(typ).raw_base()).trimmed()
	}
	wrapped_shape := flat.source_type_shape(typ)
	if wrapped_shape.has(.shared_type) {
		flags |= u64(1) << 36
		typ = flat.source_type_syntax(flat.source_type_syntax(typ).raw_base()).trimmed()
	} else if wrapped_shape.has(.atomic_type) {
		flags |= u64(1) << 37
		typ = flat.source_type_syntax(flat.source_type_syntax(typ).raw_base()).trimmed()
	}
	syntax := flat.source_type_syntax(typ)
	return u64(g.reflection_type_id(syntax.pointer_base(), module_name)) | flags | (u64(syntax.pointer_depth()) << 16)
}

fn (g &FlatGen) reflection_field_type_id_bytes(raw_type string, module_name string) u64 {
	return g.reflection_field_type_id(flat.source_type_id(raw_type), module_name)
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
		fields << '(reflection__StructField){.name = _S("${reflection_c_string(field.value)}"), .typ = ${g.reflection_field_type_id(field.typ, info.module)}, .attrs = ${reflection_attributes_array(attrs)}, .is_pub = ${flags.contains('p')}, .is_mut = ${flags.contains('m')}}'
	}
	if fields.len == 0 {
		return reflection_empty_array('reflection__StructField')
	}
	return 'new_array_from_c_array(${fields.len}, ${fields.len}, sizeof(reflection__StructField), (reflection__StructField[]){${fields.join(', ')}})'
}

fn (g &FlatGen) reflection_struct_info(info StructDeclInfo) string {
	attrs := g.decl_attrs[info.node_id] or { []string{} }
	value := '(reflection__Struct){.parent_idx = 0, .attrs = ${reflection_attributes_array(attrs)}, .fields = ${g.reflection_struct_fields(info)}}'
	return '(reflection__TypeInfo){.typ = 10, .reflection__Struct = ${value}}'
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
		args << '(reflection__FunctionArg){.name = _S("${reflection_c_string(param.value)}"), .typ = ${g.reflection_field_type_id(param.typ, module_name)}, .is_mut = ${param.is_mut}}'
		param_idx++
	}
	if args.len == 0 {
		return reflection_empty_array('reflection__FunctionArg')
	}
	return 'new_array_from_c_array(${args.len}, ${args.len}, sizeof(reflection__FunctionArg), (reflection__FunctionArg[]){${args.join(', ')}})'
}

@[unsafe]
fn (g &FlatGen) reflection_function(node_id int, node flat.Node) string {
	file := g.a.source_files[node.pos.id] or { unsafe { nil } }
	file_name := if isnil(file) { '' } else { file.name }
	module_name := g.tc.file_modules[file_name] or { 'main' }
	is_method := node.value.contains('.')
	mut receiver_type := u64(0)
	mut is_variadic := false
	mut param_idx := 0
	for i in 0 .. node.children_count {
		param := g.a.child_node(&node, i)
		if param.kind != .param {
			continue
		}
		if is_method && param_idx == 0 {
			receiver_type = g.reflection_field_type_id(param.typ, module_name)
		}
		if flat.source_type_shape(param.typ).has(.variadic) {
			is_variadic = true
		}
		param_idx++
	}
	pos := g.a.source_position(node.pos) or {
		return '(reflection__Function){.mod_name = _S("${reflection_c_string(module_name)}"), .name = _S("${reflection_c_string(node.value.all_after_last('.'))}"), .attrs = ${reflection_attributes_array(g.decl_attrs[node_id] or {
			[]string{}
		})}, .args = ${g.reflection_function_args(node, is_method, module_name)}, .file_idx = ${node.pos.id}, .is_variadic = ${is_variadic}, .return_typ = ${g.reflection_field_type_id(node.typ, module_name)}, .receiver_typ = ${receiver_type}, .is_pub = ${node.op == .arrow}}'
	}
	return '(reflection__Function){.mod_name = _S("${reflection_c_string(module_name)}"), .name = _S("${reflection_c_string(node.value.all_after_last('.'))}"), .attrs = ${reflection_attributes_array(g.decl_attrs[node_id] or {
		[]string{}
	})}, .args = ${g.reflection_function_args(node, is_method, module_name)}, .file_idx = ${node.pos.id}, .line_start = ${pos.line}, .line_end = ${pos.line}, .is_variadic = ${is_variadic}, .return_typ = ${g.reflection_field_type_id(node.typ, module_name)}, .receiver_typ = ${receiver_type}, .is_pub = ${node.op == .arrow}}'
}

fn (g &FlatGen) reflection_method_receiver_key(node flat.Node) u32 {
	if !node.value.contains('.') {
		return 0
	}
	for i in 0 .. node.children_count {
		param := g.a.child_node(&node, i)
		if param.kind != .param {
			continue
		}
		file := g.a.source_files[node.pos.id] or { return 0 }
		module_name := g.tc.file_modules[file.name] or { 'main' }
		return reflection_source_type_key(reflection_base_type_id(param.typ), module_name)
	}
	return 0
}

fn (g &FlatGen) reflection_methods(type_key_id u32) string {
	mut methods := []string{}
	mut seen := map[string]bool{}
	for node_id, node in g.a.nodes {
		if node.kind != .fn_decl || !node.pos.is_valid()
			|| g.reflection_method_receiver_key(node) != type_key_id {
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
	return '(reflection__TypeInfo){.typ = 9, .reflection__None = ${value}}'
}

fn (g &FlatGen) reflection_array_info(type_key_id u32) string {
	mut elem := type_key_id
	mut dims := 0
	for flat.source_type_shape(elem).has(.array_type) {
		dims++
		elem = flat.source_type_syntax(elem).raw_base()
	}
	value := '(reflection__Array){.nr_dims = ${dims}, .elem_type = ${g.reflection_field_type_id(elem, '')}}'
	return '(reflection__TypeInfo){.typ = 2, .reflection__Array = ${value}}'
}

fn (g &FlatGen) reflection_map_info(type_key_id u32) string {
	syntax := flat.source_type_syntax(type_key_id)
	key := if syntax.bracket_closed() { syntax.bracket_raw_inner() } else { u32(0) }
	value_type := if syntax.bracket_closed() { syntax.raw_base() } else { u32(0) }
	value := '(reflection__Map){.key_type = ${g.reflection_field_type_id(key, '')}, .value_type = ${g.reflection_field_type_id(value_type, '')}}'
	return '(reflection__TypeInfo){.typ = 7, .reflection__Map = ${value}}'
}

fn (g &FlatGen) reflection_multi_return_info(type_key_id u32) string {
	values := flat.source_type_syntax(type_key_id).reflection_tuple_parts()
	value := '(reflection__MultiReturn){.types = ${g.reflection_type_array(values, '')}}'
	return '(reflection__TypeInfo){.typ = 8, .reflection__MultiReturn = ${value}}'
}

fn (g &FlatGen) reflection_alias_info(target_id u32, module_name string) string {
	value := '(reflection__Alias){.parent_idx = ${g.reflection_field_type_id(target_id, module_name)}, .language = 0}'
	return '(reflection__TypeInfo){.typ = 1, .reflection__Alias = ${value}}'
}

fn reflection_enum_info(values []string, is_flag bool) string {
	value := '(reflection__Enum){.vals = ${reflection_string_array(values)}, .is_flag = ${is_flag}}'
	return '(reflection__TypeInfo){.typ = 4, .reflection__Enum = ${value}}'
}

fn (g &FlatGen) reflection_sum_type_info(variants []u32, module_name string) string {
	value := '(reflection__SumType){.parent_idx = 0, .variants = ${g.reflection_type_array(variants, module_name)}}'
	return '(reflection__TypeInfo){.typ = 11, .reflection__SumType = ${value}}'
}

fn (g &FlatGen) reflection_interface_methods(name string, module_name string) string {
	methods := g.tc.interface_abstract_methods[flat.source_type_id(name)] or { []string{} }
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
	name_id := flat.source_type_id(name)
	short_name := reflection_short_type_name(name)
	value := '(reflection__Interface){.name = _S("${reflection_c_string(short_name)}"), .methods = ${g.reflection_interface_methods(name, module_name)}, .fields = ${reflection_empty_array('reflection__StructField')}, .is_generic = ${name_id in g.tc.interface_generic_params}}'
	return '(reflection__TypeInfo){.typ = 6, .reflection__Interface = ${value}}'
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
	return flat.source_type_name(reflection_base_type_id(flat.source_type_id(raw_type)))
}

fn reflection_base_type_id(raw_type_id u32) u32 {
	mut typ := flat.source_type_syntax(raw_type_id).trimmed()
	for {
		shape := flat.source_type_shape(typ)
		if shape.has(.option_type) || shape.has(.result_type) || shape.has(.pointer)
			|| shape.has(.mutable_type) || shape.has(.shared_type) || shape.has(.atomic_type)
			|| shape.has(.variadic) {
			typ = flat.source_type_syntax(flat.source_type_syntax(typ).raw_base()).trimmed()
		} else {
			return typ
		}
	}
}

fn reflection_collect_composite_type(raw_type_id u32, module_name string, mut collected map[u32]bool) {
	key := reflection_source_type_key(reflection_base_type_id(raw_type_id), module_name)
	shape := flat.source_type_shape(key)
	syntax := flat.source_type_syntax(key)
	if shape.has(.array_type) || shape.has(.map_type) || syntax.tuple_like() {
		collected[key] = true
	}
	if shape.has(.array_type) {
		reflection_collect_composite_type(syntax.raw_base(), '', mut collected)
	} else if shape.has(.map_type) {
		if syntax.bracket_closed() && syntax.raw_base() != 0 {
			reflection_collect_composite_type(syntax.bracket_raw_inner(), '', mut collected)
			reflection_collect_composite_type(syntax.raw_base(), '', mut collected)
		}
	} else if syntax.tuple_like() {
		for part in syntax.reflection_tuple_parts() {
			reflection_collect_composite_type(part, '', mut collected)
		}
	}
}

fn reflection_collect_composite_type_bytes(raw_type string, module_name string, mut collected map[u32]bool) {
	reflection_collect_composite_type(flat.source_type_id(raw_type), module_name, mut collected)
}

fn (mut g FlatGen) gen_reflection_type(name_id u32, module_name string, kind int, info string, methods string, mut seen_ids map[int]bool) {
	idx := g.reflection_type_id(name_id, module_name)
	if idx == 0 || seen_ids[idx] {
		return
	}
	seen_ids[idx] = true
	mut display_name_id := name_id
	if suffix := flat.source_type_qualified_suffix(name_id, flat.source_type_id('main')) {
		display_name_id = suffix
	} else if module_name !in ['', 'builtin'] {
		if suffix := flat.source_type_qualified_suffix(name_id, flat.source_type_id(module_name)) {
			display_name_id = suffix
		}
	}
	display_name := flat.source_type_name(display_name_id)
	g.writeln('\t${reflection_c_prefix}add_type_symbol((${reflection_c_prefix}TypeSymbol){.name = _S("${reflection_c_string(display_name)}"), .mod = _S("${reflection_c_string(module_name)}"), .idx = ${idx}, .parent_idx = 0, .language = 0, .kind = ${kind}, .info = ${info}, .methods = ${methods}});')
	g.writeln('\t${reflection_c_prefix}add_type((${reflection_c_prefix}Type){.name = _S("${reflection_c_string(display_name)}"), .idx = ${idx}});')
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
	builtin_types := [flat.source_type_void, flat.source_type_voidptr, flat.source_type_byteptr,
		flat.source_type_charptr, flat.source_type_i8, flat.source_type_i16, flat.source_type_i32,
		flat.source_type_int, flat.source_type_i64, flat.source_type_isize, flat.source_type_u8,
		flat.source_type_u16, flat.source_type_u32, flat.source_type_u64, flat.source_type_uint,
		flat.source_type_f32, flat.source_type_f64, flat.source_type_char, flat.source_type_bool,
		flat.source_type_none, flat.source_type_string, flat.source_type_rune,
		cgen_reflection_float_literal_type, cgen_reflection_int_literal_type, cgen_thread_source_type]
	for name_id in builtin_types {
		g.gen_reflection_type(name_id, 'builtin', reflection_builtin_type_kind(name_id), reflection_none_info(),
			g.reflection_methods(name_id), mut seen_ids)
	}

	mut names := g.struct_decl_infos.keys()
	names.sort_with_compare(compare_reflection_source_type_ids)
	for name in names {
		info := g.struct_decl_infos[name]
		if types.builtin_type_index(info.full_name) > 0 {
			continue
		}
		key_id := reflection_source_type_key(info.full_name, info.module)
		g.gen_reflection_type(key_id, info.module, 28, g.reflection_struct_info(info),
			g.reflection_methods(key_id), mut seen_ids)
	}

	mut enum_names := g.tc.enum_names.keys()
	enum_names.sort_with_compare(compare_reflection_source_type_ids)
	for registry_name_id in enum_names {
		name := flat.source_type_name(registry_name_id)
		module_name := g.reflection_decl_module(name, .enum_decl)
		g.gen_reflection_type(registry_name_id, module_name, 33, reflection_enum_info(g.tc.enum_fields[registry_name_id] or {
			[]string{}
		}, registry_name_id in g.tc.flag_enums), g.reflection_methods(reflection_source_type_key(registry_name_id, module_name)),
			mut seen_ids)
	}

	mut alias_names := g.tc.type_aliases.keys()
	alias_names.sort_with_compare(compare_reflection_source_type_ids)
	for registry_name_id in alias_names {
		name := flat.source_type_name(registry_name_id)
		raw_module_name := g.tc.type_alias_modules[registry_name_id] or {
			if name.contains('.') { name.all_before_last('.') } else { 'main' }
		}
		module_name := if raw_module_name.len > 0 { raw_module_name } else { 'main' }
		g.gen_reflection_type(registry_name_id, module_name, 32, g.reflection_alias_info(g.tc.type_aliases.value_id(registry_name_id),
			module_name), g.reflection_methods(reflection_source_type_key(registry_name_id, module_name)), mut seen_ids)
	}

	mut sum_names := g.tc.sum_types.keys()
	sum_names.sort_with_compare(compare_reflection_source_type_ids)
	for registry_name_id in sum_names {
		name := flat.source_type_name(registry_name_id)
		module_name := g.reflection_decl_module(name, .type_decl)
		g.gen_reflection_type(registry_name_id, module_name, 31, g.reflection_sum_type_info(g.tc.sum_types.type_ids(registry_name_id) or { []u32{} }, module_name), g.reflection_methods(reflection_source_type_key(registry_name_id, module_name)), mut seen_ids)
	}

	mut interface_names := g.tc.interface_names.keys()
	interface_names.sort_with_compare(compare_reflection_source_type_ids)
	for registry_name_id in interface_names {
		name := flat.source_type_name(registry_name_id)
		module_name := g.reflection_decl_module(name, .interface_decl)
		methods := g.reflection_interface_methods(name, module_name)
		g.gen_reflection_type(registry_name_id, module_name, 35, g.reflection_interface_info(name,
			module_name), methods, mut seen_ids)
	}

	mut composites := map[u32]bool{}
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
	for registry_name_id, target_id in g.tc.type_aliases.ids {
		module_name := g.tc.type_alias_modules[registry_name_id] or { 'main' }
		reflection_collect_composite_type(target_id, module_name, mut composites)
	}
	for registry_name_id, variants_id in g.tc.sum_types.ids {
		name := flat.source_type_name(registry_name_id)
		module_name := g.reflection_decl_module(name, .type_decl)
		for variant_id in variants_id {
			reflection_collect_composite_type(variant_id, module_name, mut composites)
		}
	}
	mut composite_names := composites.keys()
	composite_names.sort_with_compare(compare_reflection_source_type_ids)
	for name_id in composite_names {
		shape := flat.source_type_shape(name_id)
		if shape.has(.array_type) {
			g.gen_reflection_type(name_id, '', 23, g.reflection_array_info(name_id), g.reflection_methods(name_id),
				mut seen_ids)
		} else if shape.has(.map_type) {
			g.gen_reflection_type(name_id, '', 25, g.reflection_map_info(name_id), g.reflection_methods(name_id),
				mut seen_ids)
		} else if flat.source_type_syntax(name_id).tuple_like() {
			g.gen_reflection_type(name_id, '', 30, g.reflection_multi_return_info(name_id),
				g.reflection_methods(name_id), mut seen_ids)
		}
	}
	g.gen_reflection_functions()
}
