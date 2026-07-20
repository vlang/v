module c

import strings
import v3.gen.c.naming
import v3.types
import v3.flat

// emit_sum_type emits emit sum type output for c.
fn (mut g FlatGen) emit_sum_type(name string) {
	variants := g.tc.sum_types[name]
	g.writeln('struct ${g.cname(name)} {')
	g.writeln('\tint typ;')
	g.writeln('\tbool _pointer_variant_is_owned;')
	g.writeln('\tunion {')
	for v in variants {
		variant_type := select_receive_unalias_type(g.tc.parse_canonical_type(v))
		ct := if variant_type is types.Pointer {
			g.value_c_type(variant_type.base_type)
		} else {
			g.value_c_type(variant_type)
		}
		field := g.sum_field_name(v)
		g.writeln('\t\t${ct}* ${field};')
	}
	g.writeln('\t};')
	g.writeln('};')
	g.writeln('')
}

// sum_type_contains_struct reports whether sum type contains struct applies in c.
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

// sum_type_index supports sum type index handling for FlatGen.
fn (g &FlatGen) sum_type_index(sum_name string, variant string) int {
	mut resolved_sum := sum_name
	if resolved_sum !in g.tc.sum_types && resolved_sum.contains('.') {
		// Resolve an import-aliased sum name (`tast.Value` for module `sub.tast`)
		// exactly first. Use suffix and bare-name fallbacks only when unique.
		aliased_sum := g.tc.qualify_name(resolved_sum)
		if aliased_sum in g.tc.sum_types {
			resolved_sum = aliased_sum
		} else {
			suffix := '.' + resolved_sum
			mut suffix_match := ''
			mut suffix_ambiguous := false
			for key, _ in g.tc.sum_types {
				if key.ends_with(suffix) {
					if suffix_match.len > 0 && suffix_match != key {
						suffix_ambiguous = true
						break
					}
					suffix_match = key
				}
			}
			if !suffix_ambiguous && suffix_match.len > 0 {
				resolved_sum = suffix_match
			} else if suffix_match.len == 0 {
				short_sum := resolved_sum.all_after_last('.')
				mut short_match := ''
				mut ambiguous := false
				for key, _ in g.tc.sum_types {
					if key == short_sum || key.all_after_last('.') == short_sum {
						if short_match.len > 0 && short_match != key {
							ambiguous = true
							break
						}
						short_match = key
					}
				}
				if !ambiguous && short_match.len > 0 {
					resolved_sum = short_match
				}
			}
		}
	}
	return g.sum_type_index_resolved(resolved_sum, variant)
}

fn (g &FlatGen) sum_type_index_resolved(sum_name string, variant string) int {
	if sum_name in g.tc.sum_types {
		for i, v in g.tc.sum_types[sum_name] {
			if v == variant {
				return i + 1
			}
		}
		resolved_variant := g.tc.qualify_name(variant)
		if resolved_variant != variant {
			for i, v in g.tc.sum_types[sum_name] {
				if v == resolved_variant {
					return i + 1
				}
			}
		}
		for i, v in g.tc.sum_types[sum_name] {
			if v.all_after_last('.') == variant {
				return i + 1
			}
		}
		// Container variants written through an import alias (`map[string]ast.Value`
		// vs the registered `map[string]toml.ast.Value`) match on the
		// module-stripped spelling only when that spelling is unambiguous.
		if variant.contains('.') || variant.contains('[') {
			short_variant := short_module_type_text(variant)
			mut short_match := 0
			for i, v in g.tc.sum_types[sum_name] {
				if short_module_type_text(v) == short_variant {
					if short_match != 0 {
						return 0
					}
					short_match = i + 1
				}
			}
			return short_match
		}
	}
	return 0
}

fn (mut g FlatGen) interface_tmp(prefix string) string {
	name := '_${prefix}_${g.tmp_count}'
	g.tmp_count++
	return name
}

fn (g &FlatGen) interface_str_lit(text string) string {
	return 'v3_c_lit("${c_escape(text)}", ${text.len})'
}

fn (g &FlatGen) interface_str_plus(left string, right string) string {
	return 'string__plus(${left}, ${right})'
}

fn (mut g FlatGen) interface_dispatch_def_string(iface_name string, cn string, method string) string {
	saved_sb := g.sb
	saved_line_start := g.line_start
	g.sb = strings.new_builder(2048)
	g.line_start = true
	g.gen_interface_dispatch_with_fallback(iface_name, cn, method, false)
	body := g.sb.str()
	unsafe { g.sb.free() }
	g.sb = saved_sb
	g.line_start = saved_line_start
	return body
}

fn (g &FlatGen) interface_dispatch_receiver_expr(concrete string, concrete_params []types.Type, wants_ptr bool) string {
	cct := g.interface_concrete_storage_c_type(concrete)
	if concrete_params.len == 0 {
		return if wants_ptr { '(${cct}*)i->_object' } else { '*(${cct}*)i->_object' }
	}
	concrete_type := g.interface_concrete_type(concrete)
	expected_type := concrete_params[0]
	if path := g.embedded_receiver_path_for_expected(concrete_type, expected_type) {
		base := '(${cct}*)i->_object'
		mut access := base
		mut access_is_ptr := true
		for field in path {
			op := if access_is_ptr { '->' } else { '.' }
			access = '(${access})${op}${c_field_name(field.name)}'
			access_is_ptr = field.typ is types.Pointer
		}
		if access_is_ptr == wants_ptr {
			return access
		}
		return if wants_ptr { '&(${access})' } else { '*(${access})' }
	}
	return if wants_ptr { '(${cct}*)i->_object' } else { '*(${cct}*)i->_object' }
}

fn (g &FlatGen) interface_arg_conversion_expr(name string, source_type types.Type, target_type types.Type) ?string {
	if source_type is types.Pointer || target_type is types.Pointer {
		return none
	}
	source_iface := g.interface_receiver_name(source_type)
	target_iface := g.interface_receiver_name(target_type)
	if source_iface.len == 0 || target_iface.len == 0 || source_iface == target_iface {
		return none
	}
	mappings := g.interface_receiver_type_id_mappings(source_iface, target_iface)
	if mappings.len == 0 {
		return none
	}
	target_ct := g.tc.c_type(types.unwrap_pointer(target_type))
	mut expr := '(${target_ct}){._typ = '
	for mapping in mappings {
		expr += '(${name}._typ == ${mapping.source_id} ? ${mapping.target_id} : '
	}
	expr += '0'
	for _ in mappings {
		expr += ')'
	}
	expr += ', ._object = ${name}._object'
	for field in g.tc.interface_fields[target_iface] or { []types.StructField{} } {
		if interface_field_type_contains_self_by_value(field.typ, target_iface) {
			continue
		}
		field_ct := g.tc.c_type(field.typ)
		expr += ', .${g.cname(field.name)} = '
		for mapping in mappings {
			field_expr := g.interface_impl_field_access_expr('${name}._object', mapping.impl,
				field.name)
			expr += '(${name}._typ == ${mapping.source_id} ? ${field_expr} : '
		}
		expr += '(${field_ct}){0}'
		for _ in mappings {
			expr += ')'
		}
	}
	expr += '}'
	return expr
}

fn (g &FlatGen) interface_method_signature_key(iface_name string, method string) ?string {
	key := '${iface_name}.${method}'
	if key in g.tc.fn_ret_types || key in g.tc.fn_param_types {
		return key
	}
	for embed in g.tc.interface_embeds[iface_name] or { []string{} } {
		if found := g.interface_method_signature_key(embed, method) {
			return found
		}
	}
	return none
}

fn (g &FlatGen) interface_dispatch_target_is_emitted(concrete_key string) bool {
	if !g.has_used_fn_filter() {
		return true
	}
	if g.used_interface_dispatch_key(concrete_key) {
		return true
	}
	receiver_name := concrete_key.all_before_last('.')
	if receiver_name.contains('.') {
		return false
	}
	method := concrete_key.all_after_last('.')
	short_key := '${receiver_name.all_after_last('.')}.${method}'
	return short_key != concrete_key
		&& g.interface_dispatch_target_short_name_is_unambiguous(short_key, method)
		&& g.used_interface_dispatch_key(short_key)
}

fn (g &FlatGen) interface_dispatch_target_short_name_is_unambiguous(short_name string, method string) bool {
	mut seen := map[string]bool{}
	mut matches := 0
	for _, impls in g.iface_impls {
		for concrete in impls {
			if seen[concrete] {
				continue
			}
			seen[concrete] = true
			if '${concrete.all_after_last('.')}.${method}' == short_name {
				matches++
				if matches > 1 {
					return false
				}
			}
		}
	}
	return matches == 1
}

// variant_references_sum supports variant references sum handling for FlatGen.
fn (g &FlatGen) variant_references_sum(variant string, sum_name string) bool {
	_ = variant
	_ = sum_name
	return true
}

// variant_refs_sum_inner supports variant refs sum inner handling for FlatGen.
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

// normalize_variant_name transforms normalize variant name data for c.
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

// type_references_sum returns type references sum data for FlatGen.
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

// resolve_sum_name resolves resolve sum name information for c.
fn (g &FlatGen) resolve_sum_name(sum_name string) string {
	if resolved := g.sum_name_lookup[sum_name] {
		return resolved
	}
	return sum_name
}

fn (mut g FlatGen) precompute_sum_name_lookup() {
	g.sum_name_lookup = map[string]string{}
	for name, _ in g.tc.sum_types {
		g.sum_name_lookup[name] = name
		short := name.all_after_last('.')
		if short.len > 0 && short !in g.sum_name_lookup {
			g.sum_name_lookup[short] = name
		}
	}
}

// resolve_variant resolves resolve variant information for c.
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

// sum_field_name supports sum field name handling for FlatGen.
fn (g &FlatGen) sum_field_name(variant string) string {
	if variant.starts_with('&') {
		return g.sum_field_name(variant[1..])
	}
	if variant.starts_with('?') {
		return '_Option_${g.cname(variant[1..])}'
	}
	if variant.starts_with('!') {
		return '_Result_${g.cname(variant[1..])}'
	}
	if variant.starts_with('ptr') && variant.len > 3 && variant[3..].contains('.') {
		return g.sum_field_name(variant[3..])
	}
	if variant.starts_with('ptr') && variant.len > 3 && variant[3..].contains('__') {
		return g.sum_field_name(variant[3..].replace('__', '.'))
	}
	if variant.starts_with('[]') {
		return '_Array_${g.cname(variant[2..])}'
	}
	if variant.starts_with('map[') {
		return '_Map_${g.cname(variant[4..].replace(']', '_'))}'
	}
	if variant.starts_with('fn(') || variant.starts_with('fn (') {
		return '_Fn_${callback_stable_key_hash(sum_fn_variant_key(variant))}'
	}
	if sum_variant_needs_type_name_field(variant) {
		return '_${naming.type_name_part(variant)}'
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
		else { g.cname(variant) }
	}
}

fn sum_variant_needs_type_name_field(variant string) bool {
	return variant.contains('(') || variant.contains(')') || variant.contains(' ')
}

fn sum_fn_variant_key(variant string) string {
	clean := variant.trim_space()
	open := clean.index('(') or { return clean.replace(' ', '') }
	close := clean.last_index(')') or { return clean.replace(' ', '') }
	params := clean[open + 1..close]
	ret := clean[close + 1..].trim_space().replace(' ', '')
	mut parts := []string{}
	for part in sum_fn_split_top_level_commas(params) {
		ptyp := sum_fn_param_type(part)
		if ptyp.len > 0 {
			parts << ptyp
		}
	}
	return 'fn(${parts.join(',')})${ret}'
}

fn sum_fn_split_top_level_commas(params string) []string {
	mut parts := []string{}
	mut depth := 0
	mut start := 0
	for i := 0; i < params.len; i++ {
		ch := params[i]
		if ch == `(` || ch == `[` || ch == `{` {
			depth++
		} else if ch == `)` || ch == `]` || ch == `}` {
			if depth > 0 {
				depth--
			}
		} else if ch == `,` && depth == 0 {
			parts << params[start..i].trim_space()
			start = i + 1
		}
	}
	parts << params[start..].trim_space()
	return parts
}

fn sum_fn_param_type(param string) string {
	clean := param.trim_space()
	if clean.len == 0 {
		return ''
	}
	if clean.starts_with('fn(') || clean.starts_with('fn (') {
		return sum_fn_variant_key(clean)
	}
	space := clean.index(' ') or { return clean }
	first := clean[..space]
	if sum_fn_is_ident(first) && first !in ['fn', 'mut', 'shared'] {
		return clean[space + 1..].trim_space().replace(' ', '')
	}
	if first in ['mut', 'shared'] {
		rest := clean[space + 1..].trim_space()
		second_space := rest.index(' ') or { return clean.replace(' ', '') }
		second := rest[..second_space]
		if sum_fn_is_ident(second) {
			return '${first}${rest[second_space + 1..].trim_space().replace(' ', '')}'
		}
	}
	return clean.replace(' ', '')
}

fn sum_fn_is_ident(s string) bool {
	if s.len == 0 {
		return false
	}
	first := s[0]
	if !((first >= `a` && first <= `z`) || (first >= `A` && first <= `Z`) || first == `_`) {
		return false
	}
	for i := 1; i < s.len; i++ {
		ch := s[i]
		if !((ch >= `a` && ch <= `z`) || (ch >= `A` && ch <= `Z`)
			|| (ch >= `0` && ch <= `9`) || ch == `_`) {
			return false
		}
	}
	return true
}

// register_interface_strings updates register interface strings state for c.
fn (mut g FlatGen) register_interface_strings() {
	for iface_name, methods in g.interfaces {
		cn := g.cname(iface_name)
		for method in methods {
			g.intern_string('interface method ${cn}.${method} not implemented')
		}
	}
}

// collect_interface_impls discovers, for every interface, the concrete struct
// types that implement it (structural typing), and assigns each a stable nonzero
// type id. The id is stored in the boxed interface value's `_typ` field and is
// what the generated method-dispatch switch matches on.
fn (mut g FlatGen) collect_interface_impls() {
	g.ierror_method_emit_names = map[string]bool{}
	mut iface_names := []string{}
	for name, _ in g.interfaces {
		iface_names << name
	}
	iface_names.sort()
	for iface in iface_names {
		mut impls := []string{}
		if g.is_ierror_type_name(iface) {
			impls = g.tc.ierror_impl_names()
		} else {
			// Structs plus type aliases with their own implementing methods; ids
			// must come from tc.interface_impl_names so the transform's `is`
			// checks agree with the dispatch ids assigned here.
			impls = g.tc.interface_impl_names(iface)
		}
		g.iface_impls[iface] = impls
		type_ids := if g.is_ierror_type_name(iface) {
			types.stable_interface_type_ids(impls)
		} else {
			g.tc.interface_type_ids(iface)
		}
		for concrete in impls {
			g.iface_type_ids['${iface}::${concrete}'] = type_ids[concrete]
		}
		if g.is_ierror_type_name(iface) {
			g.collect_ierror_method_emit_names(impls)
		}
	}
}

fn (mut g FlatGen) collect_ierror_method_emit_names(impls []string) {
	for concrete in impls {
		for method in ['msg', 'code'] {
			call := g.ierror_method_call(concrete, method) or { continue }
			g.add_ierror_method_emit_name(call.method_name)
		}
	}
}

fn (mut g FlatGen) add_ierror_method_emit_name(name string) {
	if name.len == 0 {
		return
	}
	g.ierror_method_emit_names[name] = true
	lowered := g.cname(name)
	if lowered != name {
		g.ierror_method_emit_names[lowered] = true
	}
}

// iface_type_id returns the 1-based dispatch id assigned to `concrete` for
// interface `iface`, or 0 if `concrete` does not implement `iface`.
fn (g &FlatGen) iface_type_id(iface string, concrete string) int {
	return g.iface_type_ids['${iface}::${concrete}'] or { 0 }
}

fn (g &FlatGen) iface_type_id_for_pattern(iface string, pattern string) int {
	pattern_key := if pattern.contains('.')
		&& types.is_builtin_type_name(pattern.all_after_last('.')) {
		pattern.all_after_last('.')
	} else {
		pattern
	}
	id := g.iface_type_id(iface, pattern_key)
	if id != 0 {
		return id
	}
	qpattern := g.tc.qualify_name(pattern_key)
	if qpattern != pattern_key {
		qid := g.iface_type_id(iface, qpattern)
		if qid != 0 {
			return qid
		}
	}
	if pattern_key.contains('.') {
		return 0
	}
	mut found := 0
	for concrete in g.iface_impls[iface] or { []string{} } {
		if concrete.all_after_last('.') != pattern_key {
			continue
		}
		cid := g.iface_type_id(iface, concrete)
		if cid == 0 {
			continue
		}
		if found != 0 {
			return 0
		}
		found = cid
	}
	return found
}

fn (g &FlatGen) ierror_interface_name() ?string {
	if 'IError' in g.interfaces {
		return 'IError'
	}
	if 'builtin.IError' in g.interfaces {
		return 'builtin.IError'
	}
	for name, _ in g.interfaces {
		if g.is_ierror_type_name(name) {
			return name
		}
	}
	return none
}

fn (g &FlatGen) is_ierror_type_name(name string) bool {
	_ = g
	return name == 'IError' || name == 'builtin.IError'
}

fn (g &FlatGen) ierror_direct_method_name(concrete string, method string) ?string {
	direct := '${concrete}.${method}'
	if g.ierror_method_signature_matches(direct, concrete, method) {
		return direct
	}
	qconcrete := g.tc.qualify_name(concrete)
	if qconcrete != concrete {
		qdirect := '${qconcrete}.${method}'
		if g.ierror_method_signature_matches(qdirect, qconcrete, method) {
			return qdirect
		}
	}
	return none
}

fn (g &FlatGen) ierror_method_signature_matches(name string, concrete string, method string) bool {
	params := g.tc.fn_param_types[name] or { return false }
	if params.len != 1 {
		return false
	}
	ret := g.tc.fn_ret_types[name] or { return false }
	if !g.ierror_method_return_matches(method, ret) {
		return false
	}
	receiver := g.ierror_clean_type(params[0])
	expected := g.ierror_clean_type(g.tc.parse_canonical_type(concrete))
	return g.type_names_match(receiver, expected)
}

fn (g &FlatGen) ierror_method_return_matches(method string, ret types.Type) bool {
	clean := if ret is types.Alias { ret.base_type } else { ret }
	return match method {
		'msg' { clean is types.String }
		'code' { clean.name() == 'int' }
		else { false }
	}
}

fn (g &FlatGen) ierror_clean_type(typ types.Type) types.Type {
	clean0 := types.unwrap_pointer(typ)
	return if clean0 is types.Alias { clean0.base_type } else { clean0 }
}

struct IErrorMethodCall {
	method_name string
	path        []types.StructField
}

fn (g &FlatGen) ierror_method_call(concrete string, method string) ?IErrorMethodCall {
	if direct := g.ierror_direct_method_name(concrete, method) {
		return IErrorMethodCall{
			method_name: direct
		}
	}
	mut seen := map[string]bool{}
	return g.ierror_promoted_method_call(concrete, method, mut seen)
}

fn (g &FlatGen) ierror_promoted_method_call(concrete string, method string, mut seen map[string]bool) ?IErrorMethodCall {
	if concrete in seen {
		return none
	}
	seen[concrete] = true
	for field in g.struct_embedded_fields(concrete) {
		embedded_name := g.embedded_field_type_name(field)
		if embedded_name.len == 0 {
			continue
		}
		if direct := g.ierror_direct_method_name(embedded_name, method) {
			return IErrorMethodCall{
				method_name: direct
				path:        [field]
			}
		}
		if nested := g.ierror_promoted_method_call(embedded_name, method, mut seen) {
			mut path := [field]
			path << nested.path
			return IErrorMethodCall{
				method_name: nested.method_name
				path:        path
			}
		}
	}
	return none
}

fn (g &FlatGen) ierror_method_receiver_expr(concrete string, path []types.StructField, recv_is_ptr bool) string {
	concrete_ct := g.tc.c_type(g.tc.parse_canonical_type(concrete))
	object := '(${concrete_ct}*)i->_object'
	if path.len == 0 {
		return if recv_is_ptr { object } else { '*${object}' }
	}
	mut access := object
	mut access_is_ptr := true
	for field in path {
		op := if access_is_ptr { '->' } else { '.' }
		access = '(${access})${op}${c_field_name(field.name)}'
		access_is_ptr = field.typ is types.Pointer
	}
	if access_is_ptr == recv_is_ptr {
		return access
	}
	return if recv_is_ptr { '&(${access})' } else { '*(${access})' }
}

fn (g &FlatGen) type_can_box_as_ierror(concrete string) bool {
	return g.tc.named_type_compatible_with_ierror(concrete)
}

fn (g &FlatGen) ierror_concrete_name(t types.Type) ?string {
	clean := g.ierror_payload_concrete_type(t)
	if clean !is types.Struct {
		return none
	}
	iface := g.ierror_interface_name() or { return none }
	name := (clean as types.Struct).name
	scoped_name := g.tc.resolve_ierror_payload_name(name)
	if scoped_name != name && g.iface_type_id(iface, scoped_name) != 0 {
		return scoped_name
	}
	if g.iface_type_id(iface, name) != 0 {
		return name
	}
	qname := g.tc.qualify_name(name)
	if qname != name && g.iface_type_id(iface, qname) != 0 {
		return qname
	}
	return none
}

fn (g &FlatGen) ierror_payload_concrete_type(t types.Type) types.Type {
	mut clean := t
	mut seen := map[string]bool{}
	for {
		clean = types.unwrap_pointer(clean)
		if clean is types.Alias {
			if seen[clean.name] {
				return clean
			}
			seen[clean.name] = true
			clean = clean.base_type
			continue
		}
		return clean
	}
	return clean
}

fn (g &FlatGen) ierror_type_id_for_pattern(pattern string) int {
	iface := g.ierror_interface_name() or { return 0 }
	return g.iface_type_id_for_pattern(iface, pattern)
}

fn (g &FlatGen) should_emit_ierror_method(name string, qname string) bool {
	if name in g.ierror_method_emit_names || qname in g.ierror_method_emit_names {
		return true
	}
	return g.cname(qname) in g.ierror_method_emit_names
}

fn (mut g FlatGen) gen_ierror_from_expr(id flat.NodeId) bool {
	s := g.ierror_from_expr_string(id) or { return false }
	g.write(s)
	return true
}

fn (mut g FlatGen) ierror_none_literal_string() string {
	type_id := g.ierror_type_id_for_pattern('None__')
	empty_sid := g.intern_string('')
	return '(IError){._typ = ${type_id}, ._object = memdup(&(None__){0}, sizeof(None__)), ._object_is_boxed = true, .message = _str_${empty_sid}, .code = 0}'
}

fn (mut g FlatGen) ierror_from_expr_string(id flat.NodeId) ?string {
	node := g.a.nodes[int(id)]
	mut actual := g.usable_expr_type(id)
	if node.kind == .struct_init && node.value.len > 0 {
		// A concrete error returned from a result function can carry the surrounding
		// result type as its node annotation. The literal name still identifies the
		// concrete IError implementation that must be boxed.
		actual = g.tc.parse_type(node.value)
	} else if node.kind == .ident {
		if param_type := g.current_param_type(node.value) {
			actual = param_type
		} else if param_type := g.cur_param_types[node.value] {
			actual = param_type
		}
	}
	return g.ierror_from_expr_string_with_type(id, actual)
}

fn (mut g FlatGen) ierror_from_expr_string_with_type(id flat.NodeId, actual types.Type) ?string {
	node := g.a.nodes[int(id)]
	concrete := g.ierror_concrete_name(actual) or { return none }
	iface := g.ierror_interface_name() or { return none }
	type_id := g.iface_type_id(iface, concrete)
	if type_id == 0 {
		return none
	}
	expr := g.expr_to_string(id)
	concrete_ct := g.tc.c_type(g.tc.parse_canonical_type(concrete))
	pointer_object_is_owned := actual is types.Pointer
		&& g.ierror_pointer_payload_creates_owned_object(node)
	object := if actual is types.Pointer {
		if g.ierror_pointer_payload_needs_heap_copy(node)
			|| g.ierror_pointer_payload_alias_needs_heap_copy(node) {
			'memdup(${expr}, sizeof(${concrete_ct}))'
		} else {
			expr
		}
	} else {
		'memdup((${concrete_ct}[]){${expr}}, sizeof(${concrete_ct}))'
	}
	empty_sid := g.intern_string('')
	boxed := pointer_object_is_owned || object.starts_with('memdup(')
	return '(IError){._typ = ${type_id}, ._object = ${object}, ._object_is_boxed = ${boxed}, .message = _str_${empty_sid}, .code = 0}'
}

// ierror_pointer_payload_creates_owned_object reports pointer expressions whose C
// lowering allocates independent storage. Result/interface destruction must release
// these objects even though ordinary pointer-backed interface values are borrowed.
fn (g &FlatGen) ierror_pointer_payload_creates_owned_object(node flat.Node) bool {
	clean := g.ierror_pointer_payload_unwrapped_node(node)
	if clean.kind != .prefix || clean.op != .amp || clean.children_count == 0 {
		return false
	}
	child := g.ierror_pointer_payload_unwrapped_node(g.a.nodes[int(g.a.child(&clean, 0))])
	return child.kind in [.struct_init, .assoc]
}

fn (g &FlatGen) ierror_pointer_payload_needs_heap_copy(node flat.Node) bool {
	root := g.ierror_pointer_payload_address_root(node, false) or { return false }
	return g.ierror_pointer_payload_root_needs_heap_copy(root)
}

fn (g &FlatGen) ierror_stack_subobject_address_needs_heap_copy(node flat.Node) bool {
	root := g.ierror_pointer_payload_address_root(node, true) or { return false }
	return g.ierror_pointer_payload_root_needs_heap_copy(root)
}

fn (g &FlatGen) ierror_pointer_payload_expr_needs_heap_copy(node flat.Node) bool {
	clean := g.ierror_pointer_payload_unwrapped_node(node)
	if g.ierror_pointer_payload_needs_heap_copy(clean) {
		return true
	}
	if g.ierror_array_get_pointer_alias_needs_copy(clean) {
		return true
	}
	if clean.kind == .ident {
		return g.ierror_pointer_alias_needs_copy(clean.value)
	}
	return false
}

fn (g &FlatGen) ierror_pointer_payload_alias_needs_heap_copy(node flat.Node) bool {
	clean := g.ierror_pointer_payload_unwrapped_node(node)
	return clean.kind == .ident && g.cur_scope_has_local_name(clean.value)
		&& g.ierror_pointer_alias_needs_copy(clean.value)
}

fn (g &FlatGen) ierror_array_get_pointer_alias_needs_copy(node flat.Node) bool {
	base_name := g.ierror_array_get_base_name(node) or { return false }
	return g.ierror_pointer_alias_needs_copy(base_name)
}

fn (g &FlatGen) ierror_array_get_base_name(node flat.Node) ?string {
	mut clean := g.ierror_pointer_payload_unwrapped_node(node)
	if clean.kind == .prefix && clean.op == .mul && clean.children_count > 0 {
		clean = g.ierror_pointer_payload_unwrapped_node(g.a.nodes[int(g.a.child(&clean, 0))])
	}
	if clean.kind != .call || clean.children_count < 2 {
		return none
	}
	target := g.call_target_name(g.a.child(&clean, 0))
	if target !in ['array_get', 'array__get'] {
		return none
	}
	base := g.ierror_pointer_payload_unwrapped_node(g.a.nodes[int(g.a.child(&clean, 1))])
	if base.kind == .ident && base.value.len > 0 {
		return base.value
	}
	return none
}

fn (g &FlatGen) ierror_pointer_alias_name_from_addr(node flat.Node) ?string {
	clean := g.ierror_pointer_payload_unwrapped_node(node)
	if clean.kind == .ident && clean.value.len > 0 {
		return clean.value
	}
	if clean.kind == .prefix && clean.op == .amp && clean.children_count > 0 {
		child := g.ierror_pointer_payload_unwrapped_node(g.a.nodes[int(g.a.child(&clean, 0))])
		if child.kind == .ident && child.value.len > 0 {
			return child.value
		}
	}
	return none
}

fn (g &FlatGen) ierror_pointer_payload_address_root(node flat.Node, require_subobject bool) ?flat.Node {
	clean_node := g.ierror_pointer_payload_unwrapped_node(node)
	if clean_node.kind != .prefix || clean_node.op != .amp || clean_node.children_count == 0 {
		return none
	}
	mut child_id := g.a.child(&clean_node, 0)
	mut saw_subobject := false
	for {
		child := g.ierror_pointer_payload_unwrapped_node(g.a.nodes[int(child_id)])
		if child.kind !in [.selector, .index] || child.children_count == 0 {
			break
		}
		saw_subobject = true
		child_id = g.a.child(&child, 0)
	}
	if require_subobject && !saw_subobject {
		return none
	}
	root := g.ierror_pointer_payload_unwrapped_node(g.a.nodes[int(child_id)])
	if root.kind != .ident {
		return none
	}
	return root
}

fn (g &FlatGen) ierror_pointer_payload_unwrapped_node(node flat.Node) flat.Node {
	mut cur := node
	for cur.kind in [.paren, .expr_stmt, .cast_expr, .as_expr] && cur.children_count > 0 {
		cur = g.a.nodes[int(g.a.child(&cur, 0))]
	}
	return cur
}

fn (g &FlatGen) ierror_pointer_payload_root_needs_heap_copy(root flat.Node) bool {
	if param_type := g.current_param_type(root.value) {
		return param_type !is types.Pointer
	}
	if param_type := g.cur_param_types[root.value] {
		return param_type !is types.Pointer
	}
	if local_type := g.tc.cur_scope.lookup(root.value) {
		return local_type !is types.Pointer
	}
	return false
}

// iface_type_id_for_concrete resolves the dispatch id for a boxed concrete
// type, including alias implementers. The checker normalizes alias-typed
// values to their base type (`p := Puppy{}` annotates `p` as `Dog`), so when
// the direct lookup fails, fall back to the alias's base type, and from a base
// type to the single alias implementer that resolves to it (if unambiguous).
fn (g &FlatGen) iface_type_id_for_concrete(iface string, concrete types.Type) int {
	if concrete is types.Array {
		id := g.iface_type_id(iface, 'array')
		if id != 0 {
			return id
		}
	}
	if concrete is types.Map {
		id := g.iface_type_id(iface, 'map')
		if id != 0 {
			return id
		}
	}
	concrete_name := concrete.name()
	mut id := g.iface_type_id(iface, concrete_name)
	if id != 0 {
		return id
	}
	if concrete_name.starts_with('main.') || concrete_name.starts_with('builtin.') {
		id = g.iface_type_id(iface, concrete_name.all_after_last('.'))
		if id != 0 {
			return id
		}
	}
	if concrete is types.Alias {
		id = g.iface_type_id(iface, concrete.base_type.name())
		if id != 0 {
			return id
		}
	}
	mut alias_id := 0
	mut matches := 0
	for impl in g.iface_impls[iface] or { []string{} } {
		target := g.tc.type_aliases[impl] or { continue }
		if target == concrete_name || g.tc.qualify_name(target) == g.tc.qualify_name(concrete_name) {
			alias_id = g.iface_type_id(iface, impl)
			matches++
		}
	}
	if matches == 1 {
		return alias_id
	}
	return 0
}

fn (mut g FlatGen) gen_interface_value_expr(id flat.NodeId, expected types.Type) bool {
	iface_type := cgen_unalias_type(expected)
	if iface_type !is types.Interface {
		return false
	}
	iface := iface_type as types.Interface
	if g.is_ierror_type_name(iface.name) {
		if s := g.ierror_from_expr_string(id) {
			g.write(s)
			return true
		}
	}
	node := g.a.nodes[int(id)]
	mut actual := g.usable_expr_type(id)
	if node.kind == .ident {
		if param_type := g.current_param_type(node.value) {
			actual = param_type
		}
	}
	actual_clean := if actual is types.Pointer { actual.base_type } else { actual }
	actual_base := cgen_unalias_type(actual_clean)
	if actual_base is types.Interface {
		return false
	}
	concrete_name := actual_base.name()
	if concrete_name.len == 0 {
		return false
	}
	type_id := g.iface_type_id_for_concrete(iface.name, actual_clean)
	ct := g.tc.c_type(iface)
	fields := g.interface_cached_fields(iface.name)
	concrete_ct := g.tc.c_type(actual_base)
	if fields.len > 0 {
		tmp := g.tmp_count
		g.tmp_count++
		if actual is types.Pointer {
			g.write('({ ${concrete_ct}* _iface${tmp} = ')
			g.gen_expr(id)
			g.write('; (${ct}){._typ = ${type_id}, ._object = _iface${tmp}, ._object_is_boxed = false')
			for field in fields {
				field_ct := g.tc.c_type(field.typ)
				field_name := g.cname(field.name)
				g.write(', .${field_name} = _iface${tmp} ? _iface${tmp}->${field_name} : (${field_ct}){0}')
			}
			g.write('}; })')
		} else {
			g.write('({ ${concrete_ct} _iface${tmp} = ')
			g.gen_expr(id)
			g.write('; (${ct}){._typ = ${type_id}, ._object = memdup(&_iface${tmp}, sizeof(${concrete_ct})), ._object_is_boxed = true')
			for field in fields {
				g.write(', .${g.cname(field.name)} = _iface${tmp}.${g.cname(field.name)}')
			}
			g.write('}; })')
		}
		return true
	}
	g.write('(${ct}){._typ = ${type_id}, ._object = ')
	if actual is types.Pointer {
		g.gen_expr(id)
		g.write(', ._object_is_boxed = false')
	} else if node.kind in [.ident, .selector, .index] {
		g.write('memdup(&')
		g.gen_expr(id)
		g.write(', sizeof(${concrete_ct})), ._object_is_boxed = true')
	} else {
		g.write('memdup((${concrete_ct}[]){')
		g.gen_expr(id)
		g.write('}, sizeof(${concrete_ct})), ._object_is_boxed = true')
	}
	g.write('}')
	return true
}

// is_interface_type_name reports whether is interface type name applies in c.
fn (g &FlatGen) is_interface_type_name(name string) bool {
	mut clean := name
	base, _, is_generic := shared_generic_app_parts(clean)
	if is_generic {
		clean = base
	}
	return clean in g.interfaces || g.tc.qualify_name(clean) in g.interfaces
}

// has_ierror_interface reports whether has ierror interface applies in c.
fn (g &FlatGen) has_ierror_interface() bool {
	for name, _ in g.interfaces {
		if g.is_ierror_type_name(name) {
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
			id := g.iface_type_id_for_concrete(iface, concrete)
			if id != 0 {
				return id
			}
			return none
		}
	}
	return none
}

// interface_init_object_is_boxed reports whether an interface literal's `_object`
// field owns a heap copy produced by memdup rather than borrowing a concrete pointer.
fn (g &FlatGen) interface_init_object_is_boxed(node flat.Node) bool {
	for i in 0 .. node.children_count {
		field := g.a.child_node(&node, i)
		if field.kind == .field_init && field.value == '_object' && field.children_count > 0 {
			return g.interface_object_expr_is_boxed(g.a.child(field, 0))
		}
	}
	return false
}

fn (g &FlatGen) interface_object_expr_is_boxed(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= g.a.nodes.len {
		return false
	}
	node := g.a.nodes[int(id)]
	if node.kind == .call && node.children_count > 0 {
		callee := g.a.child_node(&node, 0)
		return callee.kind == .ident && callee.value == 'memdup'
	}
	if node.kind in [.cast_expr, .paren, .expr_stmt] && node.children_count > 0 {
		return g.interface_object_expr_is_boxed(g.a.child(&node, 0))
	}
	return false
}

// interface_method_stubs emits a dispatch function for every abstract interface
// method: it switches on the boxed value's `_typ` and forwards to the concrete
// implementation, passing `_object` as the receiver. Interfaces with no known
// implementers (and the special builtin `IError`) fall back to a panic stub.
fn (mut g FlatGen) interface_method_stubs() {
	mut wrote_prototype := false
	for iface_name, methods in g.interfaces {
		cn := g.cname(iface_name)
		for method in methods {
			if !g.should_emit_interface_dispatch(iface_name, method) {
				continue
			}
			g.writeln('${g.interface_dispatch_signature(iface_name, cn, method)};')
			wrote_prototype = true
		}
	}
	if wrote_prototype {
		g.writeln('')
	}
	for iface_name, methods in g.interfaces {
		cn := g.cname(iface_name)
		for method in methods {
			if !g.should_emit_interface_dispatch(iface_name, method) {
				continue
			}
			if g.cache_split {
				// Dispatch tables depend on the concrete implementations in the
				// current program, so they belong beside main instead of in the
				// source-stable object that owns the interface declaration.
				g.writeln('/* V3CACHE_MODULE main */')
			}
			g.gen_interface_dispatch(iface_name, cn, method)
		}
	}
	if g.interfaces.len > 0 {
		g.writeln('')
	}
}

fn (mut g FlatGen) interface_method_forward_decls() {
	for iface_name, methods in g.interfaces {
		cn := g.cname(iface_name)
		for method in methods {
			if !g.should_emit_interface_dispatch(iface_name, method) {
				continue
			}
			if cn == 'IError' {
				ret_ct := if method == 'code' { 'int' } else { 'string' }
				g.writeln('${ret_ct} ${cn}__${method}(${cn}* i);')
				if g.cache_split {
					g.ierror_dispatch_target_forward_decls(iface_name, method, ret_ct)
				}
				continue
			}
			mname := '${iface_name}.${method}'
			decl_key := g.interface_method_signature_key(iface_name, method) or { mname }
			impls := g.iface_impls[iface_name] or { []string{} }
			mut sig_key := ''
			for concrete in impls {
				candidate := '${concrete}.${method}'
				if candidate in g.tc.fn_param_types {
					sig_key = candidate
					break
				}
			}
			ret_type := g.tc.fn_ret_types[decl_key] or {
				if sig_key.len > 0 {
					g.tc.fn_ret_types[sig_key] or { types.Type(types.void_) }
				} else {
					types.Type(types.void_)
				}
			}
			decl_params := g.tc.fn_param_types[decl_key] or { []types.Type{} }
			concrete_params := if sig_key.len > 0 {
				g.tc.fn_param_types[sig_key] or { []types.Type{} }
			} else {
				[]types.Type{}
			}
			sig_params := if decl_params.len > 0
				&& (concrete_params.len == 0 || decl_params.len == concrete_params.len) {
				decl_params
			} else {
				concrete_params
			}
			g.write('${g.fn_return_type_name(ret_type)} ${cn}__${method}(${cn}* i')
			for pi := 1; pi < sig_params.len; pi++ {
				pt := sig_params[pi]
				pct := if pt is types.OptionType || pt is types.ResultType {
					g.optional_type_name(pt)
				} else {
					g.tc.c_type(pt)
				}
				g.write(', ${pct} _a${pi - 1}')
			}
			g.writeln(');')
		}
	}
	if g.interfaces.len > 0 {
		g.writeln('')
	}
}

fn (mut g FlatGen) ierror_dispatch_target_forward_decls(iface_name string, method string, ret_ct string) {
	mut forwarded := map[string]bool{}
	for concrete in g.iface_impls[iface_name] or { []string{} } {
		call := g.ierror_method_call(concrete, method) or { continue }
		target_c_name := g.cname(call.method_name)
		if forwarded[target_c_name] {
			continue
		}
		params := g.tc.fn_param_types[call.method_name] or { continue }
		if params.len != 1 {
			continue
		}
		forwarded[target_c_name] = true
		g.writeln('${ret_ct} ${target_c_name}(${g.tc.c_type(params[0])} _recv);')
	}
}

fn (g &FlatGen) should_emit_interface_dispatch(iface_name string, method string) bool {
	if g.cache_split {
		return true
	}
	if !g.has_used_fn_filter() {
		return true
	}
	name := '${iface_name}.${method}'
	if g.used_interface_dispatch_key(name) {
		return true
	}
	if decl_key := g.interface_method_signature_key(iface_name, method) {
		if decl_key != name && g.used_interface_dispatch_key(decl_key) {
			return true
		}
		decl_short_name := '${decl_key.all_before_last('.').all_after_last('.')}.${method}'
		if decl_short_name != decl_key && g.interface_dispatch_short_name_allowed(iface_name)
			&& g.used_interface_dispatch_key(decl_short_name) {
			return true
		}
	}
	for alias in g.interface_alias_names(iface_name) {
		alias_name := '${alias}.${method}'
		if g.used_interface_dispatch_key(alias_name) {
			return true
		}
		short_alias_name := '${alias.all_after_last('.')}.${method}'
		if short_alias_name != alias_name && g.interface_dispatch_short_name_allowed(alias)
			&& g.used_interface_dispatch_key(short_alias_name) {
			return true
		}
	}
	short_name := '${iface_name.all_after_last('.')}.${method}'
	return short_name != name && g.interface_dispatch_short_name_allowed(iface_name)
		&& g.used_interface_dispatch_key(short_name)
}

fn (mut g FlatGen) interface_dispatch_signature(iface_name string, cn string, method string) string {
	if cn == 'IError' {
		ret_ct := if method == 'code' { 'int' } else { 'string' }
		return '${ret_ct} ${cn}__${method}(${cn}* i)'
	}
	mname := '${iface_name}.${method}'
	decl_key := g.interface_method_signature_key(iface_name, method) or { mname }
	impls := g.iface_impls[iface_name] or { []string{} }
	mut sig_key := ''
	for concrete in impls {
		if concrete in g.tc.interface_names {
			continue
		}
		ck := '${concrete}.${method}'
		if ck in g.tc.fn_param_types {
			sig_key = ck
			break
		}
	}
	ret_type := g.tc.fn_ret_types[decl_key] or {
		if sig_key.len > 0 {
			g.tc.fn_ret_types[sig_key] or { types.Type(types.void_) }
		} else {
			types.Type(types.void_)
		}
	}
	ret_ct := g.fn_return_type_name(ret_type)
	decl_params := g.tc.fn_param_types[decl_key] or { []types.Type{} }
	concrete_sig_params := if sig_key.len > 0 {
		g.tc.fn_param_types[sig_key] or { []types.Type{} }
	} else {
		[]types.Type{}
	}
	sig_params := if decl_params.len > 0
		&& (concrete_sig_params.len == 0 || decl_params.len == concrete_sig_params.len) {
		decl_params.clone()
	} else {
		concrete_sig_params.clone()
	}
	mut sig := '${ret_ct} ${cn}__${method}(${cn}* i'
	for pi := 1; pi < sig_params.len; pi++ {
		pt := sig_params[pi]
		pct := if pt is types.OptionType || pt is types.ResultType {
			g.optional_type_name(pt)
		} else {
			g.tc.c_type(pt)
		}
		sig += ', ${pct} _a${pi - 1}'
	}
	sig += ')'
	return sig
}

fn (g &FlatGen) interface_alias_names(iface_name string) []string {
	mut aliases := []string{}
	for alias, target in g.tc.type_aliases {
		qtarget := g.tc.qualify_name(target)
		if target == iface_name || qtarget == iface_name {
			aliases << alias
		}
	}
	return aliases
}

fn (g &FlatGen) used_interface_dispatch_key(name string) bool {
	return g.used_fn_contains(name) || g.used_fn_contains(g.cname(name))
}

fn (g &FlatGen) interface_dispatch_short_name_allowed(iface_name string) bool {
	return !iface_name.contains('.')
}

// gen_interface_dispatch emits interface dispatch output for c.
fn (mut g FlatGen) gen_interface_dispatch(iface_name string, cn string, method string) {
	g.gen_interface_dispatch_with_fallback(iface_name, cn, method, true)
}

fn (mut g FlatGen) gen_interface_dispatch_with_fallback(iface_name string, cn string, method string, panic_on_default bool) {
	sid := if panic_on_default {
		g.intern_string('interface method ${cn}.${method} not implemented')
	} else {
		-1
	}
	mname := '${iface_name}.${method}'
	decl_key := g.interface_method_signature_key(iface_name, method) or { mname }
	impls := g.iface_impls[iface_name] or { []string{} }
	if cn == 'IError' {
		ret_ct := if method == 'code' { 'int' } else { 'string' }
		g.writeln('${ret_ct} ${cn}__${method}(${cn}* i) {')
		for concrete in impls {
			id := g.iface_type_id(iface_name, concrete)
			call := g.ierror_method_call(concrete, method) or { continue }
			if id == 0 {
				continue
			}
			params := g.tc.fn_param_types[call.method_name] or { []types.Type{} }
			recv_is_ptr := params.len > 0 && params[0] is types.Pointer
			recv := g.ierror_method_receiver_expr(concrete, call.path, recv_is_ptr)
			g.writeln('\tif (i->_typ == ${id}) return ${g.cname(call.method_name)}(${recv});')
		}
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
	ret_type := g.tc.fn_ret_types[decl_key] or {
		if sig_key.len > 0 {
			g.tc.fn_ret_types[sig_key] or { types.Type(types.void_) }
		} else {
			types.Type(types.void_)
		}
	}
	// Use the ABI return type, not the bare value type: a fixed-array return is its `_v_ret_*`
	// wrapper struct (a C function cannot return an array by value), matching what the concrete
	// implementer's method returns and what the call site unwraps.
	ret_ct := g.fn_return_type_name(ret_type)
	decl_params := g.tc.fn_param_types[decl_key] or { []types.Type{} }
	concrete_sig_params := if sig_key.len > 0 {
		g.tc.fn_param_types[sig_key] or { []types.Type{} }
	} else {
		[]types.Type{}
	}
	mut sig_params := if decl_params.len > 0
		&& (concrete_sig_params.len == 0 || decl_params.len == concrete_sig_params.len) {
		decl_params.clone()
	} else {
		concrete_sig_params.clone()
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
	str_dispatch_is_boxed_only := g.interface_dispatch_can_use_implicit_str(method, ret_ct,
		sig_params)
	if impls.len > 0 {
		g.writeln('\tswitch (i->_typ) {')
		for concrete in impls {
			id := g.iface_type_id(iface_name, concrete)
			if id == 0 {
				continue
			}
			concrete_type_for_dispatch := g.interface_concrete_type(concrete)
			concrete_is_fn_type := g.interface_unaliased_type(concrete_type_for_dispatch) is types.FnType
			if str_dispatch_is_boxed_only && concrete !in g.tc.interface_names
				&& !concrete_is_fn_type
				&& !g.interface_boxed_type_marked_for_dispatch(iface_name, concrete) {
				continue
			}
			if concrete in g.tc.interface_names {
				decl := g.interface_method_signature_key(concrete, method) or { continue }
				if !g.interface_dispatch_target_is_emitted('${concrete}.${method}')
					&& !g.interface_dispatch_target_is_emitted(decl) {
					continue
				}
				concrete_params := g.tc.fn_param_types[decl] or { []types.Type{} }
				recv_is_ptr := concrete_params.len > 0 && concrete_params[0] is types.Pointer
				recv := if recv_is_ptr {
					'(${g.cname(concrete)}*)i->_object'
				} else {
					'*(${g.cname(concrete)}*)i->_object'
				}
				g.write('\t\tcase ${id}: ')
				mut call := '${g.cname(concrete)}__${method}(${recv}'
				for ai, an in arg_names {
					arg_idx := ai + 1
					concrete_param := if arg_idx < concrete_params.len {
						concrete_params[arg_idx]
					} else {
						types.Type(types.void_)
					}
					dispatch_param := if arg_idx < sig_params.len {
						sig_params[arg_idx]
					} else {
						concrete_param
					}
					if concrete_param is types.Pointer && dispatch_param !is types.Pointer {
						call += ', &${an}'
					} else if concrete_param !is types.Pointer && dispatch_param is types.Pointer {
						call += ', *${an}'
					} else {
						call += ', ${an}'
					}
				}
				call += ')'
				if ret_ct == 'void' {
					g.writeln('${call}; return;')
				} else {
					g.writeln('return ${call};')
				}
				continue
			}
			concrete_key := '${concrete}.${method}'
			method_key := g.tc.concrete_method_signature_key(concrete, method) or { concrete_key }
			if method_key !in g.tc.fn_param_types
				|| !g.interface_dispatch_target_is_emitted(method_key) {
				if str_dispatch_is_boxed_only {
					mut str_stack := []string{}
					if str_expr := g.interface_implicit_str_expr(g.interface_concrete_type(concrete),
						g.interface_dispatch_boxed_value_expr(concrete), false, mut str_stack)
					{
						g.writeln('\t\tcase ${id}: return ${str_expr};')
					}
				}
				continue
			}
			concrete_params := g.tc.fn_param_types[method_key] or { []types.Type{} }
			recv_is_ptr := concrete_params.len > 0 && concrete_params[0] is types.Pointer
			recv := g.interface_dispatch_receiver_expr(concrete, concrete_params, recv_is_ptr)
			g.write('\t\tcase ${id}: ')
			mut call := '${g.cname(method_key)}(${recv}'
			for ai, an in arg_names {
				arg_idx := ai + 1
				concrete_param := if arg_idx < concrete_params.len {
					concrete_params[arg_idx]
				} else {
					types.Type(types.void_)
				}
				dispatch_param := if arg_idx < sig_params.len {
					sig_params[arg_idx]
				} else {
					concrete_param
				}
				if concrete_param is types.Pointer && dispatch_param !is types.Pointer {
					call += ', &${an}'
				} else if concrete_param !is types.Pointer && dispatch_param is types.Pointer {
					call += ', *${an}'
				} else if converted := g.interface_arg_conversion_expr(an, dispatch_param,
					concrete_param)
				{
					call += ', ${converted}'
				} else {
					call += ', ${an}'
				}
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
	if panic_on_default {
		g.writeln('\tv_panic(_str_${sid});')
	} else if ret_ct == 'void' {
		g.writeln('\treturn;')
	}
	if ret_ct != 'void' {
		g.writeln('\treturn (${ret_ct}){0};')
	}
	g.writeln('}')
}

fn (mut g FlatGen) interface_boxed_type_marked_for_dispatch(iface_name string, concrete string) bool {
	g.collect_interface_boxed_types_for_dispatch()
	if g.interface_boxed_types['${iface_name}::${concrete}']
		|| g.interface_boxed_types['${iface_name}::${c_name(concrete)}']
		|| g.interface_boxed_types['${iface_name}::${concrete.all_after_last('.')}'] {
		return true
	}
	for candidate in [concrete, g.tc.qualify_name(concrete)] {
		if target := g.tc.type_aliases[candidate] {
			if g.interface_boxed_types['${iface_name}::${target}']
				|| g.interface_boxed_types['${iface_name}::${c_name(target)}']
				|| g.interface_boxed_types['${iface_name}::${target.all_after_last('.')}'] {
				return true
			}
		}
	}
	return false
}

fn (mut g FlatGen) collect_interface_boxed_types_for_dispatch() {
	if g.interface_boxed_types_done {
		return
	}
	g.interface_boxed_types_done = true
	for node in g.a.nodes {
		if node.kind != .struct_init || node.children_count == 0 {
			continue
		}
		iface_name := if node.value in g.interfaces {
			node.value
		} else {
			g.tc.qualify_name(node.value)
		}
		if iface_name !in g.interfaces {
			continue
		}
		for i in 0 .. node.children_count {
			field := g.a.child_node(&node, i)
			if field.kind != .field_init || field.value != '_object' || field.children_count == 0 {
				continue
			}
			obj_type := g.tc.resolve_type(g.a.child(field, 0))
			concrete := types.unwrap_pointer(obj_type)
			concrete_name := concrete.name()
			if concrete_name.len == 0 {
				continue
			}
			g.mark_interface_boxed_type_for_dispatch(iface_name, concrete_name)
		}
	}
}

fn (mut g FlatGen) mark_interface_boxed_type_for_dispatch(iface_name string, concrete_name string) {
	g.interface_boxed_types['${iface_name}::${concrete_name}'] = true
	g.interface_boxed_types['${iface_name}::${c_name(concrete_name)}'] = true
	g.interface_boxed_types['${iface_name}::${concrete_name.all_after_last('.')}'] = true
}

fn (g &FlatGen) interface_dispatch_can_use_implicit_str(method string, ret_ct string, sig_params []types.Type) bool {
	return method == 'str' && ret_ct == 'string' && sig_params.len == 1
}

fn (g &FlatGen) interface_dispatch_boxed_value_expr(concrete string) string {
	ct := g.interface_concrete_storage_c_type(concrete)
	return '*(${ct}*)i->_object'
}

fn (g &FlatGen) interface_concrete_storage_c_type(concrete string) string {
	concrete_type := g.interface_concrete_type(concrete)
	return if concrete_type is types.Unknown {
		g.cname(concrete)
	} else {
		g.tc.c_type(concrete_type)
	}
}

fn (g &FlatGen) interface_concrete_type(concrete string) types.Type {
	if types.is_builtin_type_name(concrete) {
		return g.tc.parse_type(concrete)
	}
	for candidate in [concrete, g.tc.qualify_name(concrete)] {
		if candidate in g.tc.type_aliases {
			return types.Type(types.Alias{
				name:      candidate
				base_type: g.tc.parse_type(g.tc.type_aliases[candidate])
			})
		}
		if candidate in g.tc.structs {
			return types.Type(types.Struct{
				name: candidate
			})
		}
		if candidate in g.tc.interface_names {
			return types.Type(types.Interface{
				name: candidate
			})
		}
		if candidate in g.tc.sum_types {
			return types.Type(types.SumType{
				name: candidate
			})
		}
		if candidate in g.tc.enum_names {
			return types.Type(types.Enum{
				name: candidate
			})
		}
	}
	return g.tc.parse_type(concrete)
}

// short_module_type_text strips module qualifiers from every identifier in a
// type text: `map[string]toml.ast.Value` -> `map[string]Value`.
fn short_module_type_text(text string) string {
	mut out := []u8{cap: text.len}
	mut i := 0
	for i < text.len {
		c := text[i]
		if (c >= `a` && c <= `z`) || (c >= `A` && c <= `Z`) || c == `_` {
			start := i
			for i < text.len {
				c2 := text[i]
				if (c2 >= `a` && c2 <= `z`) || (c2 >= `A` && c2 <= `Z`)
					|| (c2 >= `0` && c2 <= `9`) || c2 == `_` || c2 == `.` {
					i++
				} else {
					break
				}
			}
			token := text[start..i]
			unsafe { out.push_many(token.all_after_last('.').str, token.all_after_last('.').len) }
			continue
		}
		out << c
		i++
	}
	return out.bytestr()
}

fn (mut g FlatGen) interface_implicit_str_expr(typ types.Type, expr string, quote_string bool, mut stack []string) ?string {
	clean := g.interface_unaliased_type(typ)
	if typ is types.Alias {
		if custom := g.interface_custom_str_expr(typ.name, typ, expr) {
			return custom
		}
	}
	match clean {
		types.String {
			if quote_string {
				return g.interface_str_plus(g.interface_str_plus(g.interface_str_lit("'"), expr),
					g.interface_str_lit("'"))
			}
			return expr
		}
		types.Char, types.Rune {
			inner := 'rune__str((u32)(${expr}))'
			return g.interface_str_plus(g.interface_str_plus(g.interface_str_lit('`'), inner),
				g.interface_str_lit('`'))
		}
		types.ISize {
			return 'v3_i64_zpad((i64)(${expr}), 0)'
		}
		types.USize {
			return 'u64__str((u64)(${expr}))'
		}
		types.Primitive {
			name := types.Type(clean).name()
			if clean.props.has(.float) {
				return 'f64__str((double)(${expr}))'
			}
			if name == 'bool' {
				return '((${expr}) ? ${g.interface_str_lit('true')} : ${g.interface_str_lit('false')})'
			}
			if name in ['i8', 'i16', 'i32', 'i64', 'int'] {
				return 'v3_i64_zpad((i64)(${expr}), 0)'
			}
			if name in ['u8', 'byte', 'u16', 'u32', 'u64'] {
				return 'u64__str((u64)(${expr}))'
			}
			return none
		}
		types.Pointer {
			return g.interface_pointer_str_expr(clean.base_type, expr, mut stack)
		}
		types.FnType {
			return g.interface_str_lit(types.Type(clean).name().replace('fn(', 'fn ('))
		}
		types.Array {
			return g.interface_array_str_expr(clean, expr, mut stack)
		}
		types.ArrayFixed {
			return g.interface_fixed_array_str_expr(clean, expr, mut stack)
		}
		types.Map {
			key_kind := map_str_kind(g.tc, clean.key_type)
			value_kind := map_str_kind(g.tc, clean.value_type)
			fixed_len := map_str_fixed_len(clean.value_type)
			return 'v3_map_str(${expr}, ${key_kind}, ${value_kind}, ${fixed_len})'
		}
		types.Enum {
			return '${g.cname(clean.name)}__autostr(${expr})'
		}
		types.Struct {
			if custom := g.interface_custom_str_expr(clean.name, types.Type(clean), expr) {
				return custom
			}
			return g.interface_struct_str_expr(clean.name, expr, mut stack)
		}
		else {
			return none
		}
	}
}

fn (g &FlatGen) interface_unaliased_type(typ types.Type) types.Type {
	mut clean := typ
	for _ in 0 .. 100 {
		if clean is types.Alias {
			clean = clean.base_type
			continue
		}
		break
	}
	return clean
}

fn (mut g FlatGen) interface_custom_str_expr(type_name string, typ types.Type, expr string) ?string {
	method_key := g.tc.concrete_method_signature_key(type_name, 'str') or { return none }
	if method_key !in g.tc.fn_param_types || !g.interface_dispatch_target_is_emitted(method_key) {
		return none
	}
	params := g.tc.fn_param_types[method_key] or { []types.Type{} }
	wants_ptr := params.len > 0 && params[0] is types.Pointer
	arg := if wants_ptr {
		if typ is types.Pointer { expr } else { '&(${expr})' }
	} else {
		if typ is types.Pointer { '*(${expr})' } else { expr }
	}
	return '${g.cname(method_key)}(${arg})'
}

fn (mut g FlatGen) interface_pointer_str_expr(base_type types.Type, expr string, mut stack []string) ?string {
	ptr_type := types.Type(types.Pointer{
		base_type: base_type
	})
	ptr_ct := g.tc.c_type(ptr_type)
	tmp := g.interface_tmp('iface_str_ptr')
	out := g.interface_tmp('iface_str_out')
	mut inner := ''
	if custom := g.interface_custom_str_expr(base_type.name(), ptr_type, tmp) {
		inner = custom
	} else {
		inner = g.interface_implicit_str_expr(base_type, '*${tmp}', false, mut stack) or {
			'ptr_str(${tmp})'
		}
	}
	return '({ ${ptr_ct} ${tmp} = (${ptr_ct})(${expr}); string ${out} = ${g.interface_str_lit('&nil')}; if (${tmp} != 0) { ${out} = ${g.interface_str_plus(g.interface_str_lit('&'),
		inner)}; } ${out}; })'
}

fn (mut g FlatGen) interface_array_str_expr(arr types.Array, expr string, mut stack []string) ?string {
	elem_ct := g.tc.c_type(arr.elem_type)
	tmp := g.interface_tmp('iface_str_arr')
	out := g.interface_tmp('iface_str_out')
	idx := g.interface_tmp('iface_str_i')
	item := '*(${elem_ct}*)((u8*)${tmp}.data + ${idx} * ${tmp}.element_size)'
	item_str := g.interface_implicit_str_expr(arr.elem_type, item, true, mut stack) or {
		g.interface_str_lit('<array value>')
	}
	return '({ Array ${tmp} = ${expr}; string ${out} = ${g.interface_str_lit('[')}; for (int ${idx} = 0; ${idx} < ${tmp}.len; ++${idx}) { if (${idx} > 0) ${out} = ${g.interface_str_plus(out,
		g.interface_str_lit(', '))}; ${out} = ${g.interface_str_plus(out, item_str)}; } ${g.interface_str_plus(out,
		g.interface_str_lit(']'))}; })'
}

fn (mut g FlatGen) interface_fixed_array_str_expr(arr types.ArrayFixed, expr string, mut stack []string) ?string {
	elem_ct := g.tc.c_type(arr.elem_type)
	tmp := g.interface_tmp('iface_str_fixed')
	out := g.interface_tmp('iface_str_out')
	idx := g.interface_tmp('iface_str_i')
	item := '${tmp}[${idx}]'
	item_str := g.interface_implicit_str_expr(arr.elem_type, item, true, mut stack) or {
		g.interface_str_lit('<array value>')
	}
	return '({ ${elem_ct}* ${tmp} = (${elem_ct}*)(${expr}); string ${out} = ${g.interface_str_lit('[')}; for (int ${idx} = 0; ${idx} < ${arr.len}; ++${idx}) { if (${idx} > 0) ${out} = ${g.interface_str_plus(out,
		g.interface_str_lit(', '))}; ${out} = ${g.interface_str_plus(out, item_str)}; } ${g.interface_str_plus(out,
		g.interface_str_lit(']'))}; })'
}

fn (mut g FlatGen) interface_struct_str_expr(struct_name string, expr string, mut stack []string) ?string {
	display_name := struct_name.all_after_last('.')
	empty_struct := '${display_name}{}'
	if struct_name in stack {
		return g.interface_str_lit(empty_struct)
	}
	fields := g.tc.structs[struct_name] or { return none }
	if fields.len == 0 {
		return g.interface_str_lit(empty_struct)
	}
	stack << struct_name
	defer {
		stack.delete_last()
	}
	tmp := g.interface_tmp('iface_str_struct')
	out := g.interface_tmp('iface_str_out')
	ct := g.cname(struct_name)
	mut body := '${ct} ${tmp} = ${expr}; string ${out} = ${g.interface_str_lit('${display_name} {\n')};'
	for field in fields {
		field_expr := '${tmp}.${c_field_name(field.name)}'
		field_clean_type := g.interface_unaliased_type(field.typ)
		mut field_str := if field_clean_type.name() == struct_name {
			g.interface_str_lit(empty_struct)
		} else {
			g.interface_implicit_str_expr(field.typ, field_expr, field_clean_type is types.String, mut
				stack) or { g.interface_str_lit('<field value>') }
		}
		body += ' ${out} = ${g.interface_str_plus(out, g.interface_str_lit('    ${field.name}: '))};'
		body += ' ${out} = ${g.interface_str_plus(out, field_str)};'
		body += ' ${out} = ${g.interface_str_plus(out, g.interface_str_lit('\n'))};'
	}
	body += ' ${g.interface_str_plus(out, g.interface_str_lit('}'))};'
	return '({ ${body} })'
}
