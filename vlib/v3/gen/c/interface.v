module c

import v3.types
import v3.flat

fn (mut g FlatGen) emit_sum_type(name string) {
	variants := g.tc.sum_types[name]
	g.writeln('struct ${c_name(name)} {')
	g.writeln('\tint typ;')
	g.writeln('\tunion {')
	for v in variants {
		ct := g.tc.c_type(g.tc.parse_type(v))
		field := g.sum_field_name(v)
		g.writeln('\t\t${ct}* ${field};')
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
	_ = variant
	_ = sum_name
	return true
}

fn (g &FlatGen) variant_refs_sum_inner(variant string, sum_name string, mut visited map[string]bool) bool {
	normalized_variant := g.normalize_variant_name(variant)
	if normalized_variant == sum_name
		|| normalized_variant.all_after_last('.') == sum_name.all_after_last('.') {
		return true
	}
	if normalized_variant in visited {
		return false
	}
	visited[normalized_variant] = true
	mut lookup := normalized_variant
	if lookup !in g.tc.structs && !lookup.contains('.') && sum_name.contains('.') {
		qualified := '${sum_name.all_before_last('.')}.${lookup}'
		if qualified in g.tc.structs {
			lookup = qualified
		}
	}
	if lookup !in g.tc.structs && !lookup.contains('.') {
		for struct_name, _ in g.tc.structs {
			if struct_name.all_after_last('.') == lookup {
				lookup = struct_name
				break
			}
		}
	}
	if lookup in g.tc.structs {
		for f in g.tc.structs[lookup] {
			if g.type_references_sum(f.typ, sum_name, mut visited) {
				return true
			}
		}
	}
	return false
}

fn (g &FlatGen) normalize_variant_name(name string) string {
	_ = g
	mut res := name
	if res.starts_with('&') {
		res = res[1..]
	}
	if res.starts_with('ptr') && res.len > 3 {
		res = res[3..]
	}
	if res.contains('__') && !res.contains('.') {
		res = res.replace('__', '.')
	}
	return res
}

fn (g &FlatGen) type_references_sum(typ types.Type, sum_name string, mut visited map[string]bool) bool {
	resolved_sum := g.resolve_sum_name(sum_name)
	clean := types.unwrap_pointer(typ)
	if clean is types.Struct && g.resolve_sum_name(clean.name) == resolved_sum {
		return true
	}
	if clean is types.SumType && g.resolve_sum_name(clean.name) == resolved_sum {
		return true
	}
	if clean is types.SumType {
		return true
	}
	if clean is types.Struct {
		if g.variant_refs_sum_inner(clean.name, resolved_sum, mut visited) {
			return true
		}
	}
	if clean is types.Array {
		return g.type_references_sum(clean.elem_type, resolved_sum, mut visited)
	}
	return false
}

fn (g &FlatGen) resolve_sum_name(sum_name string) string {
	if sum_name in g.tc.sum_types {
		return sum_name
	}
	for name, _ in g.tc.sum_types {
		if name.all_after_last('.') == sum_name {
			return name
		}
	}
	return sum_name
}

fn (g &FlatGen) resolve_variant(sum_name string, variant string) string {
	resolved_sum := g.resolve_sum_name(sum_name)
	normalized_variant := g.normalize_variant_name(variant)
	if resolved_sum in g.tc.sum_types {
		for v in g.tc.sum_types[resolved_sum] {
			if v == normalized_variant {
				return normalized_variant
			}
		}
		for v in g.tc.sum_types[resolved_sum] {
			if v.all_after_last('.') == normalized_variant {
				return v
			}
		}
	}
	return normalized_variant
}

fn (g &FlatGen) sum_field_name(variant string) string {
	if variant.starts_with('&') {
		return g.sum_field_name(variant[1..])
	}
	if variant.starts_with('ptr') && variant.len > 3 && variant[3..].contains('.') {
		return g.sum_field_name(variant[3..])
	}
	if variant.starts_with('ptr') && variant.len > 3 && variant[3..].contains('__') {
		return g.sum_field_name(variant[3..].replace('__', '.'))
	}
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

// collect_interface_impls discovers, for every interface, the concrete struct
// types that implement it (structural typing), and assigns each a stable 1-based
// type id. The id is stored in the boxed interface value's `_typ` field and is
// what the generated method-dispatch switch matches on.
fn (mut g FlatGen) collect_interface_impls() {
	mut iface_names := []string{}
	for name, _ in g.interfaces {
		iface_names << name
	}
	iface_names.sort()
	mut struct_names := []string{}
	for name, _ in g.tc.structs {
		struct_names << name
	}
	struct_names.sort()
	for iface in iface_names {
		if c_name(iface) == 'IError' {
			continue
		}
		mut impls := []string{}
		for concrete in struct_names {
			if g.tc.named_type_implements_interface(concrete, iface) {
				impls << concrete
			}
		}
		g.iface_impls[iface] = impls
		for idx, concrete in impls {
			g.iface_type_ids['${iface}::${concrete}'] = idx + 1
		}
	}
}

// iface_type_id returns the 1-based dispatch id assigned to `concrete` for
// interface `iface`, or 0 if `concrete` does not implement `iface`.
fn (g &FlatGen) iface_type_id(iface string, concrete string) int {
	return g.iface_type_ids['${iface}::${concrete}'] or { 0 }
}

fn (g &FlatGen) is_interface_type_name(name string) bool {
	return name in g.interfaces || g.tc.qualify_name(name) in g.interfaces
}

fn (g &FlatGen) has_ierror_interface() bool {
	for name, _ in g.interfaces {
		if c_name(name) == 'IError' {
			return true
		}
	}
	return false
}

// interface_init_typ_id computes the `_typ` dispatch id for a boxed interface
// literal by recovering the concrete type from its `_object` field.
fn (g &FlatGen) interface_init_typ_id(node flat.Node) ?int {
	iface := if node.value in g.interfaces {
		node.value
	} else {
		g.tc.qualify_name(node.value)
	}
	for i in 0 .. node.children_count {
		field := g.a.child_node(&node, i)
		if field.kind == .field_init && field.value == '_object' && field.children_count > 0 {
			obj_type := g.tc.resolve_type(g.a.child(field, 0))
			concrete := types.unwrap_pointer(obj_type)
			id := g.iface_type_id(iface, concrete.name())
			if id != 0 {
				return id
			}
			return none
		}
	}
	return none
}

// interface_method_stubs emits a dispatch function for every abstract interface
// method: it switches on the boxed value's `_typ` and forwards to the concrete
// implementation, passing `_object` as the receiver. Interfaces with no known
// implementers (and the special builtin `IError`) fall back to a panic stub.
fn (mut g FlatGen) interface_method_stubs() {
	for iface_name, methods in g.interfaces {
		cn := c_name(iface_name)
		for method in methods {
			if !g.should_emit_interface_dispatch(iface_name, method) {
				continue
			}
			g.gen_interface_dispatch(iface_name, cn, method)
		}
	}
	if g.interfaces.len > 0 {
		g.writeln('')
	}
}

fn (g &FlatGen) should_emit_interface_dispatch(iface_name string, method string) bool {
	if !g.has_used_fn_filter() {
		return true
	}
	name := '${iface_name}.${method}'
	if g.used_fn_contains(name) || g.used_fn_contains(c_name(name)) {
		return true
	}
	short_name := '${iface_name.all_after_last('.')}.${method}'
	return short_name != name && g.interface_dispatch_short_name_is_unambiguous(short_name, method)
		&& (g.used_fn_contains(short_name) || g.used_fn_contains(c_name(short_name)))
}

fn (g &FlatGen) interface_dispatch_short_name_is_unambiguous(short_name string, method string) bool {
	mut matches := 0
	for iface_name, methods in g.interfaces {
		if method !in methods {
			continue
		}
		if '${iface_name.all_after_last('.')}.${method}' == short_name {
			matches++
			if matches > 1 {
				return false
			}
		}
	}
	return matches == 1
}

fn (mut g FlatGen) gen_interface_dispatch(iface_name string, cn string, method string) {
	sid := g.intern_string('interface method ${cn}.${method} not implemented')
	mname := '${iface_name}.${method}'
	impls := g.iface_impls[iface_name] or { []string{} }
	if cn == 'IError' {
		ret_ct := if method == 'code' { 'int' } else { 'string' }
		g.writeln('${ret_ct} ${cn}__${method}(${cn}* i) {')
		match method {
			'msg' {
				g.writeln('\treturn i->message;')
			}
			'code' {
				g.writeln('\treturn i->code;')
			}
			else {
				g.writeln('\tv_panic(_str_${sid});')
				g.writeln('\treturn (${ret_ct}){0};')
			}
		}

		g.writeln('}')
		return
	}
	// Interface-declared method signatures store named params unreliably (a named
	// param like `node &ast.Node` can be split into two type-only params). The
	// concrete implementer's method is a real fn_decl with a correctly parsed
	// signature, so derive the dispatch parameter types from the first implementer
	// that has the method. The receiver convention is resolved per implementer.
	mut sig_key := ''
	for concrete in impls {
		ck := '${concrete}.${method}'
		if ck in g.tc.fn_param_types {
			sig_key = ck
			break
		}
	}
	ret_type := g.tc.fn_ret_types[mname] or {
		if sig_key.len > 0 {
			g.tc.fn_ret_types[sig_key] or { types.Type(types.void_) }
		} else {
			types.Type(types.void_)
		}
	}
	ret_ct := g.optional_type_name(ret_type)
	mut sig_params := if sig_key.len > 0 {
		g.tc.fn_param_types[sig_key] or { []types.Type{} }
	} else {
		[]types.Type{}
	}
	mut arg_names := []string{}
	g.write('${ret_ct} ${cn}__${method}(${cn}* i')
	for pi := 1; pi < sig_params.len; pi++ {
		pt := sig_params[pi]
		pct := if pt is types.OptionType || pt is types.ResultType {
			g.optional_type_name(pt)
		} else {
			g.tc.c_type(pt)
		}
		an := '_a${pi - 1}'
		arg_names << an
		g.write(', ${pct} ${an}')
	}
	g.writeln(') {')
	if impls.len > 0 {
		g.writeln('\tswitch (i->_typ) {')
		for concrete in impls {
			id := g.iface_type_id(iface_name, concrete)
			concrete_key := '${concrete}.${method}'
			if id == 0 || concrete_key !in g.tc.fn_param_types
				|| !g.interface_dispatch_target_is_emitted(concrete_key) {
				continue
			}
			concrete_params := g.tc.fn_param_types[concrete_key] or { []types.Type{} }
			recv_is_ptr := concrete_params.len > 0 && concrete_params[0] is types.Pointer
			cct := g.tc.c_type(g.tc.parse_type(concrete))
			recv := if recv_is_ptr {
				'(${cct}*)i->_object'
			} else {
				'*(${cct}*)i->_object'
			}
			g.write('\t\tcase ${id}: ')
			mut call := '${c_name(concrete_key)}(${recv}'
			for an in arg_names {
				call += ', ${an}'
			}
			call += ')'
			if ret_ct == 'void' {
				g.writeln('${call}; return;')
			} else {
				g.writeln('return ${call};')
			}
		}
		g.writeln('\t\tdefault: break;')
		g.writeln('\t}')
	}
	g.writeln('\tv_panic(_str_${sid});')
	if ret_ct != 'void' {
		g.writeln('\treturn (${ret_ct}){0};')
	}
	g.writeln('}')
}

fn (g &FlatGen) interface_dispatch_target_is_emitted(concrete_key string) bool {
	if !g.has_used_fn_filter() {
		return true
	}
	return g.used_fn_contains(concrete_key) || g.used_fn_contains(c_name(concrete_key))
		|| g.used_fn_contains(concrete_key.all_after_last('.'))
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
