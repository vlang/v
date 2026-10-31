module c

import strings

struct CppInitializerField {
	name  string
	value string
	order int
}

// cpp_runtime_helpers needs no headers: native feature configuration still runs
// before any system include, and the capture packs are ordinary C++11 aggregates.
fn (mut g FlatGen) cpp_runtime_helpers() {
	if g.ccompiler != 'cplusplus' { return }
	g.writeln('template<class T> static inline T&& __v3_internal_symbol_cpp_array(T&& value) { return static_cast<T&&>(value); }')
	g.writeln('template<class... T> struct __v3_internal_symbol_cpp_values;')
	g.writeln('template<> struct __v3_internal_symbol_cpp_values<> {};')
	g.writeln('template<class T, class... Rest> struct __v3_internal_symbol_cpp_values<T, Rest...> { T value; __v3_internal_symbol_cpp_values<Rest...> rest; };')
}

// cpp_order_struct_initializer keeps C++ designators in declaration order while
// evaluating runtime field expressions once, in their original source order.
// Braced capture members evaluate in source order. Their expressions remain in
// the caller, outside the lambda, so temporaries outlive the enclosing consumer.
// The order comes only from checked V declarations, never native headers.
fn (mut g FlatGen) cpp_order_struct_initializer(start int, name string, owner u32) {
	if g.ccompiler != 'cplusplus' || start >= g.sb.len {
		return
	}
	mut order := []string{}
	if g.is_interface_type_name(owner) {
		order = ['_object', '_interface_meta', '_typ', '_object_is_boxed']
		for field in g.interface_cached_fields(owner) {
			order << g.cname(field.name)
		}
	} else if fields := g.struct_fields_for_type(owner) {
		for field in fields {
			order << g.init_field_c_name(owner, field.name)
		}
	}
	if order.len < 2 {
		return
	}
	source := g.sb.after(start)
	marker := '(${name}){'
	marker_start := source.index(marker) or { return }
	open := marker_start + marker.len - 1
	tokens := msvc_tokenize(source, open, source.len)
	mut depth := 0
	mut close := -1
	mut field_start := open + 1
	mut fields := []CppInitializerField{}
	for token in tokens {
		if token.kind != .punct {
			continue
		}
		text := source[token.pos..token.end]
		if text in ['{', '(', '['] {
			depth++
		} else if text in ['}', ')', ']'] {
			depth--
			if depth == 0 {
				field := cpp_initializer_field(source[field_start..token.pos], order) or {
					return
				}
				fields << field
				close = token.pos
				break
			}
		} else if text == ',' && depth == 1 {
			field := cpp_initializer_field(source[field_start..token.pos], order) or {
				return
			}
			fields << field
			field_start = token.end
		}
	}
	if close < 0 || fields.len < 2 {
		return
	}
	mut needs_order := false
	for i in 1 .. fields.len {
		if fields[i].order < fields[i - 1].order {
			needs_order = true
			break
		}
	}
	if !needs_order {
		return
	}
	mut capture_types := []string{}
	mut capture_values := []string{}
	capture_name := g.tmp_name()
	if !g.static_c_initializer {
		mut projection := capture_name
		for i in 0 .. fields.len {
			if cpp_initializer_zero_braces(fields[i].value) { continue }
			if fields[i].value.trim_space().starts_with('{') { return }
			capture_types << if g.is_interface_type_name(owner)
				&& fields[i].name in ['_typ', '_object_is_boxed'] {
				'u32'
			} else {
				'__typeof__(((${name}*)0)->${fields[i].name})'
			}
			capture_values << fields[i].value
			fields[i] = CppInitializerField{
				...fields[i]
				value: '${projection}.value'
			}
			projection += '.rest'
		}
	}
	fields.sort(a.order < b.order)
	capture_type := '__v3_internal_symbol_cpp_values<${capture_types.join(', ')}>'
	mut ordered := strings.new_builder(source.len + 128)
	ordered.write_string(source[..marker_start])
	if capture_values.len > 0 {
		ordered.write_string('([](const ${capture_type}& ${capture_name}) { return ')
	}
	ordered.write_string(source[marker_start..open + 1])
	for i, field in fields {
		if i > 0 { ordered.write_string(', ') }
		ordered.write_string('.${field.name} = ${field.value}')
	}
	ordered.write_string('}')
	if capture_values.len > 0 {
		ordered.write_string('; })(${capture_type}{')
		for value in capture_values {
			ordered.write_string('${value}, {')
		}
		ordered.write_string('}'.repeat(capture_values.len + 1))
		ordered.write_string(')')
	}
	ordered.write_string(source[close + 1..])
	g.sb.cut_to(start)
	g.write(ordered.str())
}

fn cpp_initializer_zero_braces(value string) bool {
	clean := value.replace(' ', '').replace('\t', '').replace('\n', '')
	return clean in ['{}', '{0}', '{E_STRUCT}']
}

fn cpp_initializer_field(source string, order []string) ?CppInitializerField {
	clean := source.trim_space()
	if !clean.starts_with('.') {
		return none
	}
	equal := clean.index('=') or { return none }
	name := clean[1..equal].trim_space()
	index := order.index(name)
	if index < 0 {
		return none
	}
	return CppInitializerField{
		name:  name
		value: clean[equal + 1..].trim_space()
		order: index
	}
}

// cpp_copy_source wraps temporary arrays only at immediate copying operations.
// Forwarding lets GNU C++ decay temporary arrays while their full-expression
// lifetime still covers the copy. Existing array and pointer sources also work;
// this helper is never used to give escaping storage a longer lifetime.
fn (g &FlatGen) cpp_copy_source(source string) string {
	if g.ccompiler == 'cplusplus' {
		return '__v3_internal_symbol_cpp_array(${source})'
	}
	return source
}

// cpp_c_linkage keeps bodyless C bindings linked to their original C symbols.
// Only compiler-produced declarations are wrapped; native headers stay opaque.
fn (g &FlatGen) cpp_c_linkage(declaration string) string {
	if g.ccompiler == 'cplusplus' {
		return 'extern "C" {\n${declaration}\n}'
	}
	return declaration
}

fn (g &FlatGen) cpp_copy_source_prefix() string {
	return if g.ccompiler == 'cplusplus' { '__v3_internal_symbol_cpp_array(' } else { '' }
}

fn (g &FlatGen) cpp_copy_source_suffix() string {
	return if g.ccompiler == 'cplusplus' { ')' } else { '' }
}
